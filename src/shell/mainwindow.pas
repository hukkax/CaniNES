unit MainWindow;

{$MODE DELPHI}
{$I canines.inc}
{$I basement.inc}

interface

uses
	Classes, SysUtils, Types,
	Basement.Util, Basement.Window,
	Basement.Renderer.Overlay,
	Basement.Renderer.CRT, Basement.Renderer.NTSC,
	SDL2, Graphics32,
	InputBindings, ConfigurationManager, MenuHandler, InfoBoxDisplay,
	NES.Types, NES.Config, NES.Console,
	NES.InputManager, NES.Controllers,
	Menubar,
	NES.PPU, NES.APU;

const
	EnabledString: array [Boolean] of String = ('disabled', 'enabled');
	MouseHideThreshold = 3;

type
	TMenuCreationCallback = procedure (Finished: Boolean) of Object;
	TMenuUpLevelCallback  = procedure of Object;
	TShowMenuPageCallback = procedure (Page: TMenuPage) of Object;

	TNESWindow = class(TWindow)
	private
		Controller: array[0..1] of TStandardControllerState;
		IgnoreStartButton: Boolean;
		ControllerUsesMouse:         Boolean;
		MouseHiddenPos:    Types.TPoint;

		procedure RendererChanged;
		procedure RealignOverlays;
		procedure EnableMouse(Enable: Boolean);
		procedure GetCRTRendererConfig;
		procedure HideMenubar;
		procedure UpdateCursor;
	public
		MenuBar:      TMenuBar;
		FontFilePath: String;
		Framecounter: Word;
		Icons:        TBitmap32;

		OnUpdateMenu:    TMenuCreationCallback;
		OnAddSubMenu,
		OnAddMenuItem:   TMenuItemAddCallback;
		OnMenuUpLevel:   TMenuUpLevelCallback;
		OnCheckMenuItem: TMenuItemCheckCallback;
		OnReinitWindow:  TNotifyEvent;
		OnShowMenuPage:  TShowMenuPageCallback;

		procedure ROMLoaded;
		procedure UpdateControllers;
		procedure UpdatePalette;

		procedure InitRendering;
		procedure InitMenubar;
		procedure UpdateMenus;
		function  ReinitWindow: Boolean; override;

		procedure UpdateOSD; inline;
		procedure DrawIcon(Index: Byte; OldIcon: Byte = 255); inline;

		procedure PixelScalingChanged; override;
		procedure ControllerSetupChanged;
		procedure OnSettingChange(Item: TConfigItem); overload;
		procedure OnSettingChange(ID: Integer); overload;
		procedure OnMenuAction(Entry: TMenuEntry);
		procedure ShowMenuPage(const Page: String; AsWindow: Boolean = True);
		procedure Perform(Action: TAction; Pressed: Boolean = True);

		procedure UpdateDisplay; inline;
		procedure RunFrame; // called each emulated frame
		procedure DoFrame;  // called each actual display frame
		procedure ToggleFastForward(Enable: Boolean);
		procedure OnUserEvent(EventCode: Integer); override;

		procedure OnKey(Key: Integer; Shift: TShiftState; Pressed, Repeated: Boolean); override;
		procedure OnMouseEnterLeave(Entered: Boolean); override;
		procedure OnMouseMove(Pos, UnscaledPos: Types.TPoint); override;
		procedure OnMouseWheel(WheelDelta: Types.TPoint); override;
		procedure OnMouseButton(Button: Basement.Window.TMouseButton; Pressed: Boolean); override;
		procedure OnJoyButton(Pressed: Boolean; PadNum: Integer; Button: Byte); override;
		procedure OnWindowResized(NewSize: Types.TPoint); override;
		procedure OnFileDropped(const Filename: String); override;

		constructor Create;
		destructor  Destroy; override;
	end;

var
	Window: TNESWindow;

	Tick: UInt32 = 0;

	Pause: Boolean;

	function  GetAspectRatioWidthMultiplier: Single;
	procedure GetSettings(Settings: PBasementInitSettings);


implementation

uses
	Math, TextOutput, Logging, MainMenu,
	BookmarkManager,
	{$IFDEF MEASURETIMING} Basement.Timing, {$ENDIF}
	Basement.Renderer;

const
	MSG_RENDERFRAME = 10;

var
	GeneratingFrame: Boolean;
	FastForwardFramesRendered: Integer;
	FastForwardTimer: TSDL_TimerID;

	CurrentIcon: Byte = 0;
	IconOverlay: TOverlayRenderer;

	{$IFDEF MEASURETIMING}
	FrameTimer: TTimeMeasurer;
	{$ENDIF}


function GetAspectRatioWidthMultiplier: Single;
var
	ar: TAspectRatio;
begin
	ar := TAspectRatio(Configuration.Display.Window.AspectRatio);

	if ar = arAuto then
	begin
		if (Assigned(Console)) and (Console.Model in [nesPAL, nesDendy]) then
			ar := arPAL
		else
			ar := arNTSC;
	end;

	case ar of
		arNTSC:       Result :=  8 / 7;
		arPAL:        Result := 11 / 8;
		arStandard:   Result :=  4 / 3;
		arWidescreen: Result := 16 / 9;
		else          Result := 1;
	end;
end;

// Create resources as required at startup
//
constructor TNESWindow.Create;
var
	S: String;
begin
	inherited Create;

	if QuitFlag then Exit;

	Window := Self;

	// generate resources if missing
	//
	WriteResource('CHEATS', ConfigPath + 'cheats.ini');

	FontFilePath := IncludeTrailingPathDelimiter(ConfigPath + 'fonts');
	{$IFDEF RENDER_FONT_FREETYPE}
	WriteResource('FONT',    FontFilePath + TTF_FONTFILE);
	WriteResource('FONTLIC', FontFilePath + TTF_FONTLICENSE);
	{$ELSE}
	WriteResource('FONT_0', FontFilePath + 'default_0.png');
	WriteResource('FONT_1', FontFilePath + 'default_1.png');
	WriteResource('FONT_2', FontFilePath + 'default_2.png');
	{$ENDIF}
	FontManager := TOverlayFontManager.Create(FontFilePath);

	Icons := TBitmap32.Create;
	if not Icons.LoadPNGFromResource('ICONS') then
	begin
		Icons.SetSize(ICON_SIZE, ICON_SIZE*4);
		S := FontFilePath + 'icons.png';
		WriteResource('ICONS', S);
		Icons.LoadFromFile(S);
	end;

	Bookmarks := TBookmarkCollection.Create(ConfigPath + 'bookmarks.ini');

	Framecounter := 0;
	Controller[0].Value := 0;
	Controller[1].Value := 0;

	DefaultConfigItemCallback := OnSettingChange;

	{$IFDEF MEASURETIMING}
	FrameTimer.Init;
	{$ENDIF}

	InitRendering;
end;

destructor TNESWindow.Destroy;
begin
	Icons.Free;
	Bookmarks.Free;
	MenuBar.Free;

	inherited Destroy;
end;

procedure TNESWindow.UpdateDisplay;
begin
	{if Configuration.Display.NTSC.Enabled then}
	if Assigned(Console) and Assigned(NES_PPU) then
	begin
		if NTSCRenderer.Enabled then
			NES_PPU.FillNTSCbuffer(NTSCRenderer.FrameBuffer)
		else
			NES_PPU.FillFrameBuffer;
	end;
end;

procedure TNESWindow.RunFrame;
label
	Done;
var
	i: Integer;
begin
	if GeneratingFrame then Exit;
	GeneratingFrame := True;

	if (Console.FastForward) and (not Console.Want_FastForward_Code) and
		(Configuration.Emulator.FastForwardSpeed > 1) then
	begin
		// limit fast forwarding speed if user initiated
		Inc(FastForwardFramesRendered);
		if FastForwardFramesRendered > Configuration.Emulator.FastForwardSpeed then
			goto Done;
	end;

	case EmulationMode of

		PAUSED:
			Pause := True;

		NORMAL: // Normal emulation
			if not (Menu.Visible and Configuration.Emulator.PauseInMenu) then
			begin
				Pause := False;

				{$IFDEF MEASURETIMING}
				FrameTimer.Start;
				{$ENDIF}

				if (Configuration.Emulator.RunAheadFrames = 0) or
					(Console.FastForward) or (Console.Rewind) then
						Console.RunFrame
				else
					Console.RunFrameWithRunAhead;

				{$IFDEF MEASURETIMING}
				//Timing.Start;
				{$ENDIF}

				UpdateDisplay;

				{$IFDEF MEASURETIMING}
				//TimingInfo.BlitBuffer := Timing.Stop;
				{$ENDIF}

				for i := 0 to Console.ControlManager.ControlDevices.Count-1 do
					Console.ControlManager.ControlDevices[i].Visualize;

				{$IFDEF MEASURETIMING}
				TimingInfo.Frame := FrameTimer.Stop;
				{$ENDIF}
			end
			else
				Pause := True;
		end;

Done:
	if (Menu.Visible) and (Configuration.Emulator.PauseInMenu) and (NTSCRenderer.Enabled) then
		UpdateDisplay;

	IconOverlay.Visible := not Menu.Visible;

	if (not Menu.Visible) and (MenuBar <> nil) then
	begin
		MenuRenderer.Visible := Mouse.Visible or MenuBar.Active or MenuBar.Hovering;
		if MenuRenderer.Visible then
			MenuBar.Draw;
	end;

	// Fast forwarding
	//
	if (Console.FastForward) <>
		(Console.Want_FastForward_User or Console.Want_FastForward_Code) then
			Window.ToggleFastForward(not Console.FastForward);

	if Console.FastForward then
	begin
		if (SDL_GetTicks - Tick) >=
			((1000 / Console.GetFPS * Configuration.Emulator.FastForwardInterval) - 1) then
		begin
			FrameBuffer.Dirty := True;
			FlipFrame;
			HandleInput;
			FastForwardFramesRendered := 0;
			Tick := SDL_GetTicks;
		end;
	end;

	GeneratingFrame := False;
end;

procedure TNESWindow.UpdateOSD;
begin
	TextOutput.UpdateOSD;
end;

procedure TNESWindow.DoFrame;
begin
	FrameBuffer.Dirty := True;

	UpdateOSD;

	{$Q-}Inc(Framecounter);

	if Menu.Visible then
		MenuRenderer.Opacity := 1.0;

	if RepeatedPadButtonInfo.Action <> actNone then
	begin
		if RepeatedPadButtonInfo.Action in RepeatedPadButtons then
		begin
			RepeatedPadButtonInfo.Counter -= 1;
			if RepeatedPadButtonInfo.Counter <= 0 then
			begin
				RepeatedPadButtonInfo.Counter := Configuration.Input.UI.PadRepeat.Interval;
				Perform(RepeatedPadButtonInfo.Action, True);
			end;
		end;
	end;

	if (Mouse.Timer > 0) and (Mouse.InWindow) then
	begin
		if (Menubar.Hovering) or (Menu.Visible) then Exit;
		Dec(Mouse.Timer);
		if (Mouse.Timer = 0) or (not Mouse.InWindow) then
			EnableMouse(
				{$IFNDEF USE_LCL}MenuBar.RefreshActiveState or{$ENDIF}
				(ControllerUsesMouse and not Configuration.Input.Zapper.HidePointer));
	end;
end;

procedure TNESWindow.OnMenuAction(Entry: TMenuEntry);
begin
	if Entry <> nil then
		Perform(TAction(Entry.Action));
end;

procedure TNESWindow.ShowMenuPage(const Page: String; AsWindow: Boolean);
begin
	{$IFNDEF USE_LCL}
	MenuRenderer.Opacity := 1.0;
	MenuRenderer.Visible := True;

	Menu.ShowAsWindow := AsWindow;
	Menu.ShowPage(Page);
	{$ELSE}
	Menu.ShowAsWindow := True;
	Menu.ShowPage(Page);
	if Assigned(OnShowMenuPage) then
		OnShowMenuPage(Menu.CurrentPage);
	Menu.Show(False);
	{$ENDIF}
end;

procedure TNESWindow.Perform(Action: TAction; Pressed: Boolean = True);
var
	Ctrl: Byte = 0;
begin
	if Menu.Visible then
	begin
		if Menu.ProcessAction(Action, Pressed) then
			Exit;
	end
	else
		RepeatedPadButtonInfo.Action := actNone;

{	if Action in ContinuousActions then
		ActionActive[Action] := Pressed;}

	case Action of
		actPadUp2, actPadDown2, actPadLeft2, actPadRight2,
		actPadSelect2, actPadStart2,
		actPadA2, actPadB2, actPadTurboA2, actPadTurboB2:
		begin
			Ctrl := 1;
			Action := TAction(Ord(Action) - Ord(actPadUp2));
		end;
	end;

	case Action of

		actPadA:     Controller[Ctrl].A := Pressed;
		actPadB:     Controller[Ctrl].B := Pressed;
		actPadLeft:
		begin
			Controller[Ctrl].Left := Pressed;
			if (Pressed) and (not Configuration.Input.AllowInvalidInput) then
				Controller[Ctrl].Right := False;
		end;
		actPadRight:
		begin
			Controller[Ctrl].Right := Pressed;
			if (Pressed) and (not Configuration.Input.AllowInvalidInput) then
				Controller[Ctrl].Left := False;
		end;
		actPadUp:
		begin
			Controller[Ctrl].Up := Pressed;
			if (Pressed) and (not Configuration.Input.AllowInvalidInput) then
				Controller[Ctrl].Down := False;
		end;
		actPadDown:
		begin
			Controller[Ctrl].Down := Pressed;
			if (Pressed) and (not Configuration.Input.AllowInvalidInput) then
				Controller[Ctrl].Up := False;
		end;
		actPadStart:
		begin
			if not Pressed then
				IgnoreStartButton := False
			else
			if IgnoreStartButton then
				Pressed := False;
			Controller[Ctrl].Start := Pressed;
		end;
		actPadSelect: Controller[Ctrl].Select := Pressed;

		{$IFNDEF USE_LCL}
		actMenubarFocus:
			if Pressed then
			begin
				if (Menubar.Hovering) or (Menu.Visible) then Exit;
				Menubar.Active := not Menubar.Active;
				if Menubar.Active then
				begin
					Menubar.Visible := True;
					Menubar.RootMenu.ActivateItem(Menubar.RootMenu.Items.First);
					Menubar.RootMenu.Sticky := True;
					MenuRenderer.Opacity := 1.0;
				end
				else
				if ControllerUsesMouse then
					HideMenubar;
				UpdateCursor;
			end;
		{$ENDIF}

		actAppExit:
			if Pressed then
			begin
				Log('Quitting...');
				ToggleFastForward(False);
				Close;
			end;

		actToggleFullscreen:
			if Pressed then
				Configuration.ToggleBool(@Configuration.Display.Window.FullScreen);

		actROMLoadPrevious:
			if Pressed then
				Console.LoadROM(Configuration.Application.LastROMFile);

		actROMLoadFromMenu:
			if Pressed then // this was called via the Menubar
			begin
				if (Menubar.ActiveMenu = nil) or (Menubar.ActiveMenu.ActiveItem = nil) then Exit;
				Console.LoadROM(Menubar.ActiveMenu.ActiveItem.Data);
				EmulationMode := NORMAL;
				Menubar.Active := False;
			end;

		actRecordWAV:
			if Pressed then
				Console.ToggleWAVRecording;

		actFavourite:
			if Pressed then
			begin
				Bookmarks.Toggle('');
				InitMenubar;
			end;

		actMoviePlayback:
			if Pressed and Console.MovieManager.Loaded then
			begin
				if Console.MovieManager.IsPlaying then
					Console.MovieManager.Stop
				else
				begin
					Console.Reset(False);
					Console.MovieManager.Play;
				end;
				UpdateMenus;
				Menu.Show(False);
				OSD('');
			end;

		actStateLoad:
			if Pressed then
			begin
				if Console.LoadState then
				begin
					OSD('State loaded.');
					EmulationMode := NORMAL;
				end
				else
					OSD('Failed to load state.');
			end;

		actStateSave:
			if Pressed then
				if Console.SaveState then
					OSD('State saved.');

		// Console
		actConsolePause:
			if Pressed then
				Console.TogglePause;

		actConsoleReset,
		actConsoleRestart:
			if Pressed then
			begin
				Console.Reset(Action = actConsoleReset);
				if Action = actConsoleReset then
					OSD('Console reset.')
				else
				begin
					Menu.Show(False);
					OSD('Console powercycled.');
				end;
				EmulationMode := NORMAL;
			end;

		actRewind:
			if Pressed then
			begin
				Console.RewindManager.Start;
				DrawIcon(ICON_REWIND);
			end
			else
			begin
				Console.RewindManager.Stop;
				DrawIcon(ICON_NONE);
			end;

		actFastForward:
		begin
			if not Console.GetPaused then
			begin
				Console.Want_FastForward_User := Pressed;
				DrawIcon(IfThen(Pressed, ICON_FORWARD, ICON_NONE));
			end
			else
			if Pressed then
			begin
				EmulationMode := NORMAL;
				RunFrame;
				EmulationMode := PAUSED;
			end;
		end;

		actToggleInfoBox:
			if (Pressed) and (not Menu.Visible) then InfoBox.Toggle;

		actMenuShow:
			if Pressed then ShowMenuPage('Main', False);

		actROMBrowser:
			if Pressed then ShowMenuPage('Load ROM');

		actListCheats:
			if Pressed then ShowMenuPage('Cheats');

		actCartInfo:
			if Pressed then ShowMenuPage('Cart Info');

		actBookmarks:
			if Pressed then ShowMenuPage('Favourites');

{ !!!
		actToggleFilterNTSC_MergeFields:
			if Pressed then
			with Configuration.Display do
			begin
				NTSC.MergeFields := not NTSC.MergeFields;
				NES_PPU.ConfigureNTSCFilter(NTSC);
				OSD('NTSC merge fields ' + EnabledString[NTSC.MergeFields]);
			end;
}

		actToggleFilterNTSC:
			if Pressed then
			begin
				Configuration.ToggleBool(@Configuration.Display.NTSC.Enabled);
				MenuBar.ForceRedraw;
				OSD('NTSC filter ' + EnabledString[Configuration.Display.NTSC.Enabled]);
			end;

		actToggleFilterCRT:
			if Pressed then
			begin
				Configuration.ToggleBool(@Configuration.Display.CRT.Enabled);
				MenuBar.ForceRedraw;
				OSD('CRT filter ' + EnabledString[CRTRenderer.Enabled]);
			end;

	end;
end;

//==================================================================================================
// Fast Forwarding
//==================================================================================================

function FastForwardTimerCallback(interval: Uint32; param: Pointer): UInt32; cdecl;
var
	event: TSDL_Event;
begin
	if (Initialized) and (not Locked) then
	begin
		event.type_ := SDL_USEREVENT;
		event.user.code := MSG_RENDERFRAME;
	    SDL_PushEvent(@event);
	end;
	Result := interval;
end;

procedure TNESWindow.OnUserEvent(EventCode: Integer);
begin
	if EventCode = MSG_RENDERFRAME then
		RunFrame;
end;

procedure TNESWindow.ToggleFastForward(Enable: Boolean);
begin
	Console.FastForward := Enable;
	Console.Rewind := False;
	Framecounter := 0;
	FastForwardFramesRendered := 0;
	if Enable then
	begin
		FastForwardTimer := SDL_AddTimer(1, FastForwardTimerCallback, nil);
	end
	else
	begin
		SDL_RemoveTimer(FastForwardTimer);
		OSD('');
	end;
end;

procedure TNESWindow.ROMLoaded;
var
	NewHz: Double;
	NeedReinit: Boolean;
	S: String;
begin
	Controller[0].Value := 0;
	Controller[1].Value := 0;
	IgnoreStartButton := True;
	UpdateControllers;

	Menu.Show(False);

	NewHz := Console.GetFPS;
	NeedReinit := False;

	if TAspectRatio(Configuration.Display.Window.AspectRatio) = arAuto then
	begin
		if GetAspectRatioWidthMultiplier <> Settings.AspectRatioWidthMultiplier then
			NeedReinit := True;
	end;

	if NewHz <> Settings.Framerate then
	begin
		NES.Config.Configuration.Display.CRT.Enabled := CRTRenderer.Enabled;

		if SetFrameRate(NewHz) then
		begin
			if Video.HaveVSync then S := 'VSync' else S := 'No VSync';
			Log('Reinitializing video at %f Hz (%s)', [NewHz, S]);
			NeedReinit := True;
		end;
	end;

	if NeedReinit then
		ReinitWindow
	else
		UpdateMenus;

	S := Format('%s (%s, mapper %d)',
		[ExtractFilename(Console.LoadedFile), NESSystemNames[Console.System], Cartridge.nMapperID]);
	if Cartridge.RomData.Info.IsPatched then
		S := S + ' (patched)';
	OSD(S);

	S := Cartridge.RomData.Info.DatabaseInfo.Title;
	if S.IsEmpty then
		S := ChangeFileExt(ExtractFilename(Console.LoadedFile), '');
	SetTitle(APPNAME + {$IFDEF DEBUG} ' (Debug)' + {$ENDIF} ' - ' + S);
end;

procedure TNESWindow.UpdateMenus;
begin
	UpdateMainMenu;
	Window.InitMenubar;
end;

procedure TNESWindow.UpdateControllers;
begin
	if not Console.MovieManager.IsPlaying then
	begin
		Console.ControlManager.PrimaryController.SetStateFromInput(Controller[0].Value);
		Console.ControlManager.SecondaryController.SetStateFromInput(Controller[1].Value);
	end;
end;

procedure TNESWindow.UpdatePalette;
begin
	// default and NTSC filter palette generators use different scaling,
	// attempt to match them (eyeballed, good enough)
	//
{
	with NES.Config.Configuration.Display do
	begin
		NTSC.Hue := (-Palette.HueShift / 20) + 0.08;
		NTSC.Saturation := Palette.Saturation / 10 - 0.06;
		NTSC.Contrast := Palette.Contrast / 10;
		NTSC.Brightness := Palette.Brightness - 1.0;
		NTSC.Gamma := Palette.Gamma / 10;
	end;
}
	NES_PPU.Palette.Editor.Changed;
	NES_PPU.ConfigureNTSCFilter(Configuration.Display.NTSC);

	if not (NTSCRenderer.Enabled) then
		UpdateDisplay;
end;

procedure TNESWindow.RendererChanged;
begin
	RendererFlipMode := Configuration.Display.Renderer.FlipMode;
	NTSCRenderer.Enabled := Configuration.Display.NTSC.Enabled;

	if NTSCRenderer.Enabled then
	begin
		CRTRenderer.FrameBuffer := NTSCRenderer.FrameBuffer;
		CRTRenderer.Texture := NTSCRenderer.Texture;
	end
	else
	begin
		CRTRenderer.FrameBuffer := Self.FrameBuffer;
		CRTRenderer.Texture := Self.Video.Texture;
		UpdateDisplay;
	end;
end;

procedure TNESWindow.GetCRTRendererConfig;
begin
	with CRTRenderer do
	begin
		Enabled := NES.Config.Configuration.Display.CRT.Enabled;
		with NES.Config.Configuration.Display.CRT do
		begin
			Options.ScanlinesEnabled := ScanlineOpacity >= 0.05;
			Options.MaskEnabled      := MaskOpacity >= 0.05;

			Options.ScanlineOpacity        := ScanlineOpacity;
			Options.MaskOpacity            := MaskOpacity * 0.5;
			Options.NoiseOpacity           := NoiseAmount;
			Options.ScanlineBloom          := ScanlineBloom;
			Options.DotCrawlSpeed          := DotCrawlSpeed;
			Options.HorizontalBlur         := HorizontalBlur;
			Options.ExtraContrast          := ExtraContrast;
			Options.EnlargeMaskAtZoomLevel := EnlargeMaskAtZoomLevel;
		end;
	end;
end;

procedure TNESWindow.OnSettingChange(Item: TConfigItem);
begin
	OnSettingChange(Item.ID);
end;

procedure TNESWindow.OnSettingChange(ID: Integer);
begin
	case ID of

		cfgInitWindow,
		cfgRenderer:
			ReinitWindow;

		cfgRendererPalette:
			UpdatePalette;

		cfgRendererNTSC:
		begin
			NES_PPU.ConfigureNTSCFilter(Configuration.Display.NTSC);
			RendererChanged;
		end;

		cfgRendererCRT,
		cfgRendererCRTReinit:
		begin
			GetCRTRendererConfig;
			if ID = cfgRendererCRTReinit then
				CRTRenderer.Init
			else
				CRTRenderer.OptionsChanged;
		end;

		cfgFullScreen:
		begin
			NES_APU.Stop;
			{$IFNDEF USE_LCL}
			SetFullScreen(Configuration.Display.Window.FullScreen);
			{$ENDIF}
		end;

		cfgGUIAlign:
			RealignOverlays;

		cfgEmulationAPU,
		cfgEmulationAPUVolume,
		cfgEmulationAPUPanning:
			NES_APU.SettingsChanged;

		cfgInputZapper:
			Console.ControllerSetupChanged;

		cfgInputVis:
			Console.ControlManager.PadVisualizationChanged;

	end;
end;

procedure TNESWindow.HideMenubar;
begin
	MenuRenderer.Opacity := 0;
	Menubar.Visible := False;
	Menubar.Active := False;
	UpdateCursor;
end;

//==================================================================================================
// Input handling
//==================================================================================================

procedure TNESWindow.OnKey(Key: Integer; Shift: TShiftState; Pressed, Repeated: Boolean);
var
	Action: TAction;
	Binding: ^TinputBinding;
	bShift, MenuVisible: Boolean;
begin
	MenuVisible := Menu.Visible;

	if MenuVisible then
	begin
		if Menu.ProcessKey(Key, Shift, Pressed, Repeated) then
			Exit;
	end
	{$IFNDEF USE_LCL}
	else
	if Menubar.Active then
	begin
		if Menubar.ProcessKey(Key, Shift, Pressed, Repeated) then
			Exit;
	end
	{$ENDIF};

	if (Key = 0) or (Repeated) then Exit;

	bShift := ssShift in Shift;

	if (not Pressed) and ((Key = SDLK_LSHIFT) or (Key = SDLK_RSHIFT)) then
	begin
		for Action in TAction do
			if Action in ContinuousActions then
			begin
				Binding := @Bindings[Action];
				if (Binding.IsShift) or (Binding.Shift) then
					Perform(Action, False);
			end;
	end
	else
	for Action in TAction do
	begin
		Binding := @Bindings[Action];
		if (Binding.Key = Key) then
			if (Binding.IsShift) or (Binding.Shift = bShift) then
			begin
				Perform(Action, Pressed);
				if MenuVisible then Exit; // don't pass through actions from menu
			end;
	end;

	case Key of

		//SDLK_F:
		//	if Pressed then EmulationMode := STEP_FRAME;

		SDLK_0..SDLK_9:
		begin
			Console.SaveStateManager.SelectSaveSlot(Key - SDLK_0, True);
			//OSD('Selected save slot ' + Console.SaveStateManager.Index.ToString);
		end

	else
		if Key <> SDLK_ESCAPE then
			inherited;
	end;

	UpdateControllers;
end;

procedure TNESWindow.OnJoyButton(Pressed: Boolean; PadNum: Integer; Button: Byte);
var
	Action: TAction;
	Binding: ^TinputBinding;
begin
	if Menu.Visible then
		if Menu.ProcessJoy(Pressed, PadNum, Button) then
			Exit;

	if Pressed then
	begin
		RepeatedPadButtonInfo.Button := Button;
		RepeatedPadButtonInfo.Action := actNone;
		RepeatedPadButtonInfo.Counter := Configuration.Input.UI.PadRepeat.Initial;
	end
	else
	begin
		RepeatedPadButtonInfo.Button := 0;
		RepeatedPadButtonInfo.Action := actNone;
		RepeatedPadButtonInfo.Counter := 0;
	end;

	for Action in TAction do
	begin
		Binding := @Bindings[Action];
		if (Binding.PadNum = PadNum) and (Binding.PadButton = Button) then
		begin
			if Pressed then
				RepeatedPadButtonInfo.Action := Action;
			Perform(Action, Pressed);
		end;
	end;

	//writeln('Joy ', PadNum, '=', Button);

	UpdateControllers;
end;

procedure TNESWindow.EnableMouse(Enable: Boolean);
begin
	if Enable then
	begin
		Mouse.Timer := IfThen(Trunc(Video.SyncRate) > 40, Trunc(Video.SyncRate), 60) * 2; // 2 seconds
		if not Mouse.Visible then
		begin
			Mouse.Visible := True;
			ShowMouse;
		end;
	end
	else
	if Mouse.Visible then
	begin
		Mouse.Visible := False;
		MouseHiddenPos := Mouse.Pos;
		ShowMouse;

		Menubar.Hovering := False;
		{$IFNDEF USE_LCL}
		if not Menu.Visible then
			if (MenuBar.RootMenu.ActiveItem = nil) or
				(not MenuBar.RootMenu.Sticky) then
				begin
					MenuRenderer.Opacity := 0;
					Menubar.Active := False;
					Menubar.RootMenu.Sticky := False;
				end;
		{$ENDIF}
	end;
end;

procedure TNESWindow.OnMouseEnterLeave(Entered: Boolean);
begin
	inherited;
	EnableMouse(Mouse.InWindow);
end;

procedure TNESWindow.OnMouseMove(Pos, UnscaledPos: Types.TPoint);
var
	B: Boolean;
begin
	if not ControllerUsesMouse then
	begin
		B := Mouse.Visible;
		if not B then
		if	(Menubar.Hovering) or (Menubar.Active) or
			(Abs(Pos.X - MouseHiddenPos.X) >= MouseHideThreshold) or
			(Abs(Pos.Y - MouseHiddenPos.Y) >= MouseHideThreshold) then
				B := True;
		if B then
			EnableMouse(True)
		else
			Exit;
	end;

	if Menu.Visible then
		Menu.OnMouseMove(Point(UnscaledPos.X*Scale, UnscaledPos.Y*Scale))
	else
	{$IFNDEF USE_LCL}
	if Menubar.Visible then
	begin
		if (Menubar.Active) or (UnScaledPos.Y < MenuRenderer.Font.GlyphHeight) then
			MenuRenderer.Opacity := 1.0
		else
			MenuRenderer.Opacity := Max(0, 1 - (UnScaledPos.Y / (BasementOptions.FramebufferHeight / 2)));
		Menubar.OnMouseMove(Point(UnscaledPos.X*Scale, UnscaledPos.Y*Scale));
	end
	else
	{$ENDIF}
	begin
		UnscaledPos.X := Trunc(UnscaledPos.X / Settings.AspectRatioWidthMultiplier);
		InputManager.Mouse.Pos := UnscaledPos;
		UpdateControllers;
	end;
end;

procedure TNESWindow.OnMouseButton(Button: Basement.Window.TMouseButton; Pressed: Boolean);
begin
	inherited;

	if Menu.Visible then
		Menu.OnMouseButton(Button, Pressed)
	else
	{$IFNDEF USE_LCL}
	if Menubar.Active then
	begin
		Menubar.OnMouseButton(Button, Pressed);
		if (ControllerUsesMouse) and (not Menubar.Active) then
			HideMenubar;
		OnMouseMove(Mouse.Pos, Mouse.UnscaledPos);
	end
	else
	if (Pressed) and (Button = Basement.Window.mbRight) then
		Perform(actMenubarFocus, Pressed)
	else
	{$ENDIF}
	begin
		InputManager.Mouse.Buttons[Button] := Pressed;
		UpdateControllers;
	end;
end;

procedure TNESWindow.OnMouseWheel(WheelDelta: Types.TPoint);
begin
	if Menu.Visible then
	begin
		Menu.CurrentPage.Scroll(-WheelDelta.Y * 3); // scroll amount per wheel rotation
		Menu.Draw;
	end
	{$IFNDEF USE_LCL}
	else
	if (MenuBar.Hovering) and (MenuBar.ActiveMenu <> nil) then
		MenuBar.ActiveMenu.OnMouseWheel(WheelDelta.Y);
	{$ENDIF}
end;

procedure TNESWindow.OnFileDropped(const Filename: String);
begin
	Console.LoadROM(Filename);
end;

procedure GetSettings(Settings: PBasementInitSettings);
begin
	with Settings^ do
	begin
		X := Configuration.Display.Window.X;
		Y := Configuration.Display.Window.Y;
		Scale := Configuration.Display.Window.Scale;
		AspectRatioWidthMultiplier := GetAspectRatioWidthMultiplier;
		AutoswitchResolution := Configuration.Display.Renderer.AutoswitchResolution;
		ScalingQuality := Configuration.Display.Renderer.ScalingQuality;
		HighPriority := Configuration.Application.HighPriority;
		VSyncMode := Configuration.Display.Renderer.VSyncMode;
		Backend := Configuration.Display.Renderer.Backend;
		FullScreen := Configuration.Display.Window.FullScreen;
		with Configuration.Display.Renderer.Overscan do
			Overscan := Rect(L, U, R, D);
	end;
end;

procedure TNESWindow.UpdateCursor;
begin
	{$IFDEF USE_LCL}
	SetSystemCursor(IfThen(ControllerUsesMouse or Menu.Visible,
		SDL_SYSTEM_CURSOR_ARROW, SDL_SYSTEM_CURSOR_CROSSHAIR));
	{$ELSE}
	SetSystemCursor(IfThen((not ControllerUsesMouse) or (Menubar.Active),
		SDL_SYSTEM_CURSOR_ARROW, SDL_SYSTEM_CURSOR_CROSSHAIR));
	{$ENDIF}
end;

procedure TNESWindow.ControllerSetupChanged;
begin
	if not Assigned(Menu) then Exit;

	ControllerUsesMouse := Console.CurrentControllerType in MouseControllers;
	if ControllerUsesMouse then
		HideMenubar;
	UpdateCursor;

	Mouse.Enabled := True;
	Mouse.Visible := (Console <> nil) and (ControllerUsesMouse) and
		(not Configuration.Input.Zapper.HidePointer);
	ShowMouse;

	Console.ControlManager.PadVisualizationChanged;
end;

procedure TNESWindow.RealignOverlays;
var
	M: Integer;
begin
	M := Configuration.Display.GUI.OSD.Alignments.Margin;

	MenuRenderer.Align(laCenter, 0);
	OSDLayer.Align(Configuration.Display.GUI.OSD.Alignments.Messages, M);
	PadOverlay.Align(Configuration.Display.GUI.OSD.Alignments.Pads, M);
	IconOverlay.Align(Configuration.Display.GUI.OSD.Alignments.Icons, M);
	InfoBox.Realign;
end;

function TNESWindow.ReinitWindow: Boolean;
begin
	MenuRenderer := nil;

	if Assigned(OnReinitWindow) then OnReinitWindow(Self);

	GetSettings(@Settings);
	Tick := 0;

	Result := SetupVideo;
	if Result then
		InitRendering
	else
		Close;
end;

procedure TNESWindow.InitRendering;
var
	W, H: Integer;
begin
	// renderer list should be clear before entering here

	W := Trunc(OverscanRect.Width * Scale * Settings.AspectRatioWidthMultiplier);
	H := OverscanRect.Height * Scale;

	NTSCRenderer := TNTSCRenderer.Create(Self, 602, 240, 'NTSC');

	CRTRenderer := TCRTRenderer.Create(Self, nil, 'CRT');
	GetCRTRendererConfig;
	CRTRenderer.Init;

	RendererChanged;

	FontManager.LoadFont;

	MenuRenderer := TOverlayRenderer.Create(Self, 'Menu', W, H, 0);
	MenuRenderer.SetFont(FontManager.Font);

	// Create overlays
	//
	OSDLayer := TOverlayRenderer.Create(Self, 'OSD',
		W - Configuration.Display.GUI.OSD.Alignments.Margin*2,
		FontManager.Font.GlyphHeight*3+(OSD_PADDING*2), 0);
	OSDLayer.SetFont(FontManager.Font);
	PadOverlay := TOverlayRenderer.Create(Self, 'Pads',
		(PadsSizeX + PadsMargin) * 4 + (PadsMargin*2), PadsSizeY, PadsScale);
	IconOverlay := TOverlayRenderer.Create(Self, 'Icon',
		ICON_SIZE, ICON_SIZE, PadsScale);

	InfoBox.Free;
	InfoBox := TInfoBox.Create;

	RealignOverlays;

	UpdateMenus;
	ControllerSetupChanged;
	Mouse.InWindow := True;
end;

procedure TNESWindow.PixelScalingChanged;
begin
	if Initialized then
		InitRendering;
end;

procedure TNESWindow.DrawIcon(Index: Byte; OldIcon: Byte = 255);
begin
	if CurrentIcon <> Index then
	begin
		if (OldIcon <> 255) and (OldIcon <> CurrentIcon) then Exit;
		CurrentIcon := Index;
		IconOverlay.FrameBuffer.Clear(0);
		if Index > 0 then
		begin
			Dec(Index);
			IconOverlay.FrameBuffer.DrawColorKey(0, 0,
				Bounds(0, Index*ICON_SIZE, ICON_SIZE, ICON_SIZE), Icons.Pixel[0,0], Icons);
			IconOverlay.SetActiveArea(IconOverlay.FrameBuffer.CroppedRect(0));
		end;
	end;
end;

procedure TNESWindow.InitMenubar;
var
	Item: TMenuItem;
	Root: TSubMenu;
	i: Integer;
	S: String;

	procedure MenuUpLevel;
	begin
		if Assigned(OnMenuUpLevel) then OnMenuUpLevel;
	end;

begin
	if Console = nil then Exit;

	if Assigned(OnUpdateMenu) then
		OnUpdateMenu(False);

	MenuBar.Free;
	MenuBar := TMenuBar.Create(MenuRenderer.FrameBuffer, MenuRenderer.Font);

	Root := MenuBar.RootMenu;
	MenuBar.OnAddItem    := OnAddMenuItem;
	MenuBar.OnAddSubMenu := OnAddSubMenu;
	MenuBar.OnCheckItem  := OnCheckMenuItem;

	{File
		Open
		Reload Previous
		Recent Files >
		Favourites >
		-
		Save State
		Load State
		-
		Exit
	}
	Item := Root.AddItem('&File');
	with Item.AddSubMenu(0) do
	begin
		Item := AddItem('&Open'+Dots, actShowPage, 'Load ROM');

		Item := AddItem('&Recent Files');
		with Item.AddSubMenu(0) do
			for i := 0 to MRUcount-1 do
			begin
				S := Configuration.Application.MRU[i];
				if not S.IsEmpty then AddItem(
					ChangeFileExt(ExtractFileName(Configuration.Application.MRU[i]), ''),
						actROMLoadFromMenu, S);
			end;

		MenuUpLevel;
		Item := AddItem('&Favourites');
		with Item.AddSubMenu(0) do
		begin
			if (Console.GotCartridge) and (not Console.LoadedFile.IsEmpty) then
			begin
				if Bookmarks.Contains(Console.LoadedFile) then
					S := '&Remove' else S := '&Add';
				S := S + ' Current';
				AddItem(S, actFavourite);
				if Bookmarks.Items.Count > 0 then
					AddSeparator;
			end;

			if Bookmarks.Items.Count > 0 then
				for S in Bookmarks.Items do
					if not S.IsEmpty then
						AddItem(ChangeFileExt(ExtractFilename(S), ''), actROMLoadFromMenu, S);
		end;

		MenuUpLevel;
		AddSeparator;
		Item := AddItem('&Save State', actStateSave);
		Item := AddItem('&Load State', actStateLoad);
		AddSeparator;
		Item := AddItem('E&xit', actAppExit);
	end;

	{Console
		Pause
		Reset
		Power Cycle
		-
		Play Movie
	}
	Item := Root.AddItem('&Console');
	with Item.AddSubMenu(0) do
	begin
		AddItem('&Pause', actConsolePause);
		AddItem('&Reset', actConsoleReset);
		AddItem('Power &Cycle', actConsoleRestart);

		if Console.MovieManager.Loaded then
		begin
			AddSeparator;
			if Console.MovieManager.IsPlaying then
				AddItem('Stop &Movie', actMoviePlayback)
			else
				AddItem('Play &Movie', actMoviePlayback);
		end;
	end;

	{Options
		Display >
		Audio >
		Input >
		Emulation >
		Application >
	}
	Item := Root.AddItem('&Options');
	with Item.AddSubMenu(0) do
	begin
		AddItem('&Display'+Dots,     actShowPage);
		AddItem('&Sound'+Dots,       actShowPage, 'Audio');
		AddItem('&Input'+Dots,       actShowPage);
		AddItem('&Emulation'+Dots,   actShowPage);
		AddItem('&Application'+Dots, actShowPage);
	end;

	{View
		Cart Info
		-
		[X] CRT Filter
		[X] NTSC Filter
		[X] Fullscreen
	}
	Item := Root.AddItem('&View');
	with Item.AddSubMenu(0) do
	begin
		AddItem('Cartridge &Info'+Dots, actShowPage, 'Cart Info').SetKeyAction(actCartInfo);
		AddSeparator;
		AddCheckItem('&NTSC Filter', @Configuration.Display.NTSC.Enabled).SetKeyAction(actToggleFilterNTSC);
		AddCheckItem('&CRT  Filter', @Configuration.Display.CRT.Enabled).SetKeyAction(actToggleFilterCRT);
		AddSeparator;
		AddCheckItem('&Fullscreen',  @Configuration.Display.Window.FullScreen).SetKeyAction(actToggleFullscreen);
	end;

	{Tools
		Cheat Browser
		Record Audio
		Debug Log
	}
	Item := Root.AddItem('&Tools');
	with Item.AddSubMenu(0) do
	begin
		AddItem('&Cheat Browser'+Dots, actShowPage, 'Cheats').SetKeyAction(actListCheats);
		AddItem('Debug &Log'+Dots, actShowPage);

		AddSeparator;
		if NES_APU.WavRecording then
			AddItem('Stop &Recording Audio', actRecordWAV)
		else
			AddItem('&Record Audio', actRecordWAV);
	end;

	{Help
		About CaniNES
	}
	Item := Root.AddItem('&Help');
	with Item.AddSubMenu(0) do
	begin
		AddItem('&About CaniNES'+Dots, actShowPage, 'About');
	end;

	if Assigned(OnUpdateMenu) then
		OnUpdateMenu(True);
end;

procedure TNESWindow.OnWindowResized(NewSize: Types.TPoint);
var
	i, sx, sy: Integer;
begin
	inherited;

	if NES_APU <> nil then
		NES_APU.Stop;

	sx := NewSize.X div Trunc(OverscanRect.Width * Settings.AspectRatioWidthMultiplier);
	sy := NewSize.Y div OverscanRect.Height;
	Scale := Max(2, Min(sx, sy));

	{$IFNDEF USE_LCL}
	if (SDL_GetWindowFlags(Video.Window) and SDL_WINDOW_MAXIMIZED) = 0 then
	begin
		sx := Scale * Trunc(OverscanRect.Width * Settings.AspectRatioWidthMultiplier);
		sy := Scale * OverscanRect.Height;
		SDL_SetWindowSize(Video.Window, sx, sy);
	end;
	{$ENDIF}

	for i := Renderers.Count-1 downto 1 do
		Renderers.Delete(i);

	InitRendering;
	PixelScalingChanged;
end;


end.

