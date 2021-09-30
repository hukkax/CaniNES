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
	TNESWindow = class(TWindow)
	private
		Controller: array[0..1] of TStandardControllerState;
		IgnoreStartButton: Boolean;
		MouseHiddenPos: Types.TPoint;

		procedure RendererChanged;
		procedure RealignOverlays;
		procedure EnableMouse;
		procedure GetCRTRendererConfig;
	public
		FontFilePath: String;
		Framecounter: Word;
		Icons:        TBitmap32;

		procedure ROMLoaded;
		procedure UpdateControllers;
		procedure UpdatePalette;

		procedure InitRendering;
		procedure InitMenubar;
		procedure ReinitWindow; override;

		procedure UpdateOSD; inline;
		procedure DrawIcon(Index: Byte; OldIcon: Byte = 255); inline;

		procedure PixelScalingChanged; override;
		procedure ControllerSetupChanged;
		procedure OnSettingChange(Item: TConfigItem); overload;
		procedure OnSettingChange(ID: Integer); overload;
		procedure OnMenuAction(Entry: TMenuEntry);
		procedure ShowMenuPage(const Page: String; AsWindow: Boolean = True);
		procedure Perform(Action: TAction; Pressed: Boolean = True);

		procedure RunFrame; // called each emulated frame
		procedure DoFrame;  // called each actual display frame
		procedure ToggleFastForward(Enable: Boolean);
		procedure OnUserEvent(EventCode: Integer); override;

		procedure OnKey(Key: Integer; Shift: TShiftState; Pressed, Repeated: Boolean); override;
		procedure OnMouseMove(Pos, UnscaledPos: Types.TPoint); override;
		procedure OnMouseWheel(WheelDelta: Types.TPoint); override;
		procedure OnMouseButton(Button: Basement.Window.TMouseButton; Pressed: Boolean); override;
		procedure OnJoyButton(Pressed: Boolean; PadNum: Integer; Button: Byte); override;
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
	MenuBar: TMenuBar;

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
		arPAL:        Result := 355 / 256; //11 / 8;
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
	Controller[0] := Default(TStandardControllerState);
	Controller[1] := Default(TStandardControllerState);

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
			if not ((Menu.Visible) and (Configuration.Emulator.PauseInMenu)) then
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

				if Configuration.Display.NTSC.Enabled then
					NES_PPU.FillNTSCbuffer(NTSCRenderer.FrameBuffer)
				else
					NES_PPU.FillFrameBuffer;

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

	Inc(Framecounter);

	if Mouse.Timer > 0 then
	begin
		if (Menubar.Hovering) or (Menu.Visible) then Exit;

		Dec(Mouse.Timer);
		if (Mouse.Timer = 0) or (not Mouse.InWindow) then
		begin
			if not MenuBar.RefreshActiveState then
			begin
				Mouse.Visible := False;
				MouseHiddenPos := Mouse.Pos;
				ShowMouse;
			end
			else
				EnableMouse;
		end;
	end;
end;

procedure TNESWindow.OnMenuAction(Entry: TMenuEntry);
begin
	if Entry <> nil then
		Perform(TAction(Entry.Action));
end;

procedure TNESWindow.ShowMenuPage(const Page: String; AsWindow: Boolean);
begin
	MenuRenderer.Opacity := 1.0;
	MenuRenderer.Visible := True;
	Menu.ShowAsWindow := AsWindow;
	Menu.ShowPage(Page);
end;

procedure TNESWindow.Perform(Action: TAction; Pressed: Boolean = True);
var
	Ctrl: Byte = 0;
begin
	if Menu.Visible then
		if Menu.ProcessAction(Action, Pressed) then
			Exit;

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

		actMenubarFocus:
			if Pressed then
			begin
				Menubar.Active := not Menubar.Active;
				if Menubar.Active then
					MenuRenderer.Opacity := 1.0;
			end;

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
			if Configuration.Application.LastROMFile <> '' then
			begin
				Console.LoadROM(Configuration.Application.LastROMFile);
				EmulationMode := NORMAL;
			end;

		actROMLoadMRU:
			if Pressed then
			begin
				// this was called via the Menubar
				Console.LoadROM(Menubar.ActiveMenu.ActiveItem.Data);
				EmulationMode := NORMAL;
				Menubar.Active := False;
			end;

		actRecordWAV:
			if Pressed then
				Console.ToggleWAVRecording;

		actFavourite:
			if Pressed then
				Bookmarks.Toggle('');

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
				UpdateMainMenu;
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

		actToggleFilterNTSC_MergeFields:
			if Pressed then
			with Configuration.Display do
			begin
				NTSC.MergeFields := not NTSC.MergeFields;
				NES_PPU.ConfigureNTSCFilter(NTSC);
				OSD('NTSC merge fields ' + EnabledString[NTSC.MergeFields]);
			end;

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
	S: String;
begin
	Controller[0].Value := 0;
	UpdateControllers;
	IgnoreStartButton := True;

	Menu.Show(False);

	NewHz := Console.GetFPS;

	if TAspectRatio(Configuration.Display.Window.AspectRatio) = arAuto then
	begin
		if GetAspectRatioWidthMultiplier <> Settings.AspectRatioWidthMultiplier then
			ReinitWindow;
	end;

	if NewHz <> Settings.Framerate then
	begin
		NES.Config.Configuration.Display.CRT.Enabled := CRTRenderer.Enabled;

		if SetFrameRate(NewHz) then
		begin
			if Video.HaveVSync then S := 'VSync' else S := 'No VSync';
			Log('Reinitializing video at %f Hz (%s)', [NewHz, S]);
			ReinitWindow;
		end;
	end;

	EmulationMode := NORMAL;

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
	with NES.Config.Configuration.Display do
	begin
		NTSC.Hue := (-Palette.HueShift / 20) + 0.08;
		NTSC.Saturation := Palette.Saturation / 10 - 0.06;
		NTSC.Contrast := Palette.Contrast / 10;
		NTSC.Brightness := Palette.Brightness - 1.0;
		NTSC.Gamma := Palette.Gamma / 10;
	end;

	NES_PPU.Palette.Editor.Changed;
	NES_PPU.ConfigureNTSCFilter(Configuration.Display.NTSC);
end;

procedure TNESWindow.RendererChanged;
begin
	NTSCRenderer.Enabled := Configuration.Display.NTSC.Enabled;

	if NTSCRenderer.Enabled then
	begin
		CRTRenderer.FrameBuffer := NTSCRenderer.FrameBuffer;
		CRTRenderer.Texture := NTSCRenderer.Texture;
		if (Assigned(Console)) and (Assigned(NES_PPU)) then
			NES_PPU.FillNTSCbuffer(NTSCRenderer.FrameBuffer)
	end
	else
	begin
		CRTRenderer.FrameBuffer := Self.FrameBuffer;
		CRTRenderer.Texture := Self.Video.Texture;
		if (Assigned(Console)) and (Assigned(NES_PPU)) then
			NES_PPU.FillFrameBuffer;
	end;
end;

procedure TNESWindow.GetCRTRendererConfig;
begin
	with CRTRenderer do
	begin
		Enabled := NES.Config.Configuration.Display.CRT.Enabled;
		with NES.Config.Configuration.Display.CRT do
		begin
			Options.MaskEnabled            := MaskEnabled;
			Options.ScanlinesEnabled       := ScanlinesEnabled;
			Options.ScanlineBloom          := ScanlineBloom;
			Options.DotCrawlSpeed          := DotCrawlSpeed;
			Options.HorizontalBlur         := HorizontalBlur;
			Options.ScanlineBrightness     := ScanlineBrightness;
			Options.MaskBrightness         := MaskBrightness;
			Options.BrightAndSharp         := BrightAndSharp;
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
			SetFullScreen(Configuration.Display.Window.FullScreen);
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

//==================================================================================================
// Input handling
//==================================================================================================

procedure TNESWindow.EnableMouse;
begin
	Mouse.Timer := IfThen(Trunc(Video.SyncRate) > 40, Trunc(Video.SyncRate), 60) * 2; // 2 seconds
	if not Mouse.Visible then
	begin
		Mouse.Visible := True;
		ShowMouse;
	end;
end;

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
	else
	if Menubar.Active then
	begin
		if Menubar.ProcessKey(Key, Shift, Pressed, Repeated) then
			Exit;
	end;

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

	for Action in TAction do
	begin
		Binding := @Bindings[Action];
		if (Binding.PadNum = PadNum) and (Binding.PadButton = Button) then
			Perform(Action, Pressed);
	end;

	//writeln('Joy ', PadNum, '=', Button);

	UpdateControllers;
end;

procedure TNESWindow.OnMouseMove(Pos, UnscaledPos: Types.TPoint);
var
	B: Boolean;
begin
	B := Mouse.Visible;
	if not B then
	if	(Menubar.Hovering) or (Menubar.Active) or
		(Abs(Pos.X - MouseHiddenPos.X) >= MouseHideThreshold) or
		(Abs(Pos.Y - MouseHiddenPos.Y) >= MouseHideThreshold) then
			B := True;
	if B then
		EnableMouse
	else
		Exit;

	if Menu.Visible then
		Menu.OnMouseMove(Point(UnscaledPos.X*Scale, UnscaledPos.Y*Scale))
	else
	if Menubar.Visible then
	begin
		if (Menubar.Active) or (UnScaledPos.Y < MenuRenderer.Font.GlyphHeight) then
			MenuRenderer.Opacity := 1.0
		else
			MenuRenderer.Opacity := Max(0, 1 - (UnScaledPos.Y / (BasementOptions.Height / 2)));
		Menubar.OnMouseMove(Point(UnscaledPos.X*Scale, UnscaledPos.Y*Scale));
	end
	else
	begin
		UnscaledPos.X := Trunc(UnscaledPos.X / Settings.AspectRatioWidthMultiplier);
		InputManager.Mouse.Pos := UnscaledPos;
		UpdateControllers;
	end;
end;

procedure TNESWindow.OnMouseButton(Button: Basement.Window.TMouseButton; Pressed: Boolean);
begin
	if Menu.Visible then
		Menu.OnMouseButton(Button, Pressed)
	else
	if Menubar.Active then
		Menubar.OnMouseButton(Button, Pressed)
	else
	{if (Pressed) and (Button = Basement.Window.mbRight) then
		Perform(actMenubarFocus, Pressed)
		//Perform(actMenuShow, Pressed)
	else}
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
	end;
end;

procedure TNESWindow.OnFileDropped(const Filename: String);
begin
	Console.LoadROM(Filename);
end;

procedure GetSettings(Settings: PBasementInitSettings);
begin
	with Settings^ do
	begin
		MaxScale := Max(1, Configuration.Display.Window.MaxScale);
		AspectRatioWidthMultiplier := GetAspectRatioWidthMultiplier;
		AutoswitchResolution := Configuration.Display.Renderer.AutoswitchResolution;
		ScalingQuality := Configuration.Display.Renderer.ScalingQuality;
		HighPriority := Configuration.Application.HighPriority;
		VSyncMode := Configuration.Display.Renderer.VSyncMode;
		Backend := Configuration.Display.Renderer.Backend;
		with Configuration.Display.Renderer.Overscan do
			Overscan := Rect(L, U, R, D);
	end;
end;

procedure TNESWindow.ControllerSetupChanged;
begin
	if not Assigned(Menu) then Exit;

	Mouse.Enabled := True;
	{(Menu.Visible) or (Configuration.Input.Zapper.Enabled) or
		((Console <> nil) and (Console.CurrentControllerType in [gitZapper, gitTwoZappers]));}
	//if Mouse.Enabled then
		Mouse.Visible := {(Menu.Visible) or}
		(	(Console <> nil) and (Console.CurrentControllerType in [gitZapper, gitTwoZappers]) and
			(not Configuration.Input.Zapper.HidePointer) );
	ShowMouse;
	Console.ControlManager.PadVisualizationChanged;
end;

procedure TNESWindow.ReinitWindow;
begin
	MenuRenderer := nil;

	GetSettings(@Settings);

	SetupVideo;
	InitRendering;
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
	CRTRenderer.OptionsChanged;

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

	UpdateMainMenu;
	InitMenubar;
	ControllerSetupChanged;
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
	Root, SubMenu: TSubMenu;
	i: Integer;
begin
	if Console = nil then Exit;

	MenuBar.Free;
	MenuBar := TMenuBar.Create(MenuRenderer.FrameBuffer, MenuRenderer.Font);

	Root := MenuBar.RootMenu;

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
	Item := Root.AddItem('File');
	with Item.AddSubMenu(0) do
	begin
		Item := AddItem('Open...', actShowPage, 'Load ROM');

		Item := AddItem('Recent Files');
		with Item.AddSubMenu(0) do
			for i := 0 to MRUcount-1 do
				AddItem(ChangeFileExt(ExtractFileName(Configuration.Application.MRU[i]), ''),
					actROMLoadMRU).Data := Configuration.Application.MRU[i];
		Item := AddItem('Favourites...', actShowPage, 'Favourites');
		AddSeparator;
		Item := AddItem('Save State', actStateSave);
		Item := AddItem('Load State', actStateLoad);
		AddSeparator;
		Item := AddItem('Exit', actAppExit);
	end;

	{Console
		Pause
		Reset
		Power Cycle
		-
		Play Movie
	}
	Item := Root.AddItem('Console');
	with Item.AddSubMenu(0) do
	begin
		AddItem('Pause', actConsolePause);
		AddItem('Reset', actConsoleReset);
		AddItem('Power Cycle', actConsoleRestart);

		if Console.MovieManager.Loaded then
		begin
			AddSeparator;
			if Console.MovieManager.IsPlaying then
				AddItem('Stop Movie', actMoviePlayback)
			else
				AddItem('Play Movie', actMoviePlayback);
		end;
	end;

	{Options
		Display >
		Audio >
		Input >
		Emulation >
		Application >
	}
	Item := Root.AddItem('Options');
	with Item.AddSubMenu(0) do
	begin
		AddItem('Display',     actShowPage);
		AddItem('Audio',       actShowPage);
		AddItem('Input',       actShowPage);
		AddItem('Emulation',   actShowPage);
		AddItem('Application', actShowPage);
	end;

	{View
		Cart Info
		-
		[X] CRT Filter
		[X] NTSC Filter
		[X] Fullscreen
	}
	Item := Root.AddItem('View');
	with Item.AddSubMenu(0) do
	begin
		AddItem('Cart Info', actShowPage, 'Cart Info');
		AddSeparator;
		AddCheckItem('NTSC Filter', @Configuration.Display.NTSC.Enabled);
		AddCheckItem('CRT  Filter', @Configuration.Display.CRT.Enabled);
		AddCheckItem('Fullscreen',  @Configuration.Display.Window.FullScreen);
	end;

	{Tools
		Cheat Browser
		Record Audio
		Debug Log
	}
	Item := Root.AddItem('Tools');
	with Item.AddSubMenu(0) do
	begin
		AddItem('Cheat Browser', actShowPage, 'Cheats');

		if NES_APU.WavRecording then
			AddItem('Stop Recording Audio', actRecordWAV)
		else
			AddItem('Record Audio', actRecordWAV);

		AddItem('Debug Log', actShowPage);
	end;

	{Help
		About CaniNES
	}
{
	Item := Root.AddItem('Help');
	with Item.AddSubMenu(0) do
	begin
		AddItem('About CaniNES...');
	end;
}
end;


end.

