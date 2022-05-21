unit Canines.Main;

{$MODE DELPHI}{$H+}

interface

uses
	Classes, SysUtils;

	procedure CaniNES_Init;
	procedure CaniNES_ProcessFrame;
	procedure CaniNES_Shutdown;

var
	ProcessingFrame: Boolean;


implementation

uses
	{$I basement-prestart1.inc}
	LazLogger, Logging,
	FileUtil, Basement.Util,
	Basement.Window, MainWindow,
	TextOutput, Basement.Renderer.Overlay,
	Basement.Renderer.NTSC,
	Graphics32, SDL2,
	NES.Types, NES.Config, ConfigurationManager,
	MenuHandler, MainMenu, InfoBoxDisplay, InputBindings,
	NES.Console, NES.Cartridge,
	NES.CPU, NES.PPU, NES.APU;

	function GetApplicationName: String; begin Result := APPNAME; end;

var
	Console: TConsole;
	S, ROMPath: String;


procedure CaniNES_Init;
begin
	{$I basement-startup.inc}

	DebugLn('================================================');
	DebugLn(Format('%s v%s starting.', [APPNAME, APPVERSION]));
	DebugLn('================================================');

	Pause := False;
	OnGetApplicationName := GetApplicationName;

	// Load configuration
	//
	ConfigPath := GetAppConfigDir(False);
	{$IFDEF DEBUG}
	ConfigPath := ConfigPath.Replace('-debug', '');
	{$ENDIF}
	S := ConfigPath + CONFIG_FILENAME;

	//TempPath := IncludeTrailingPathDelimiter(ConfigPath + 'temp');

	ConfigManager := TConfigurationManager.Create;
	Configuration.Init(S);
	{StoredConfiguration.Init(S);
	Configuration := StoredConfiguration;}

	InitDefaultInputBindings;
	LoadBindings;

	with BasementOptions do
	begin
		{$IFDEF CPU64} Caption := APPNAME; {$ELSE} Caption := APPNAME + ' x64'; {$ENDIF}

		FramebufferWidth  := NES_RESOLUTION_X;
		FramebufferHeight := NES_RESOLUTION_Y;
		MinScale := 2; // x1 too small for menu fonts

		GetSettings(@BasementOptions);

		{$IFDEF USE_LCL}
		//UseGamepads := False;
		FullScreen := False;
		Configuration.Display.Window.FullScreen := False;
		{$ENDIF}
	end;

	Window := TNESWindow.Create;

	if QuitFlag then
	begin
		SDL_Quit;
		Exit;
	end;

	if (Configuration.Display.Window.X > 0) and (Configuration.Display.Window.Y > 0) then
		SDL_SetWindowPosition(Window.Video.Window,
			Configuration.Display.Window.X, Configuration.Display.Window.Y);

	ROMPath := IncludeTrailingPathDelimiter(AppPath + 'rom');

	if Configuration.Application.Paths.Palette.IsEmpty then
		Configuration.Application.Paths.Palette := DataPath + 'palettes';
	Configuration.Application.Paths.Palette :=
		IncludeTrailingPathDelimiter(Configuration.Application.Paths.Palette);

	Console := TConsole.Create(AppPath, Window.FrameBuffer);

	if not Configuration.Display.Palette.Filename.IsEmpty then
	begin
		S := GetDataFile(Configuration.Application.Paths.Palette +
			Configuration.Display.Palette.Filename);
		if S <> '' then
			NES_PPU.Palette.LoadFromFile(S);
	end
	else
	with Configuration.Display.Palette do
	begin
		NES_PPU.Palette.Generate(Saturation, HueShift, Contrast, Brightness, Gamma);
		Log('Generated NTSC palette.');
	end;

	Window.UpdatePalette;

	Window.Framebuffer.Clear(Color_Background);
	NTSCRenderer.FrameBuffer.Clear(Color_Background);

	CreateMainMenu;
	Window.InitMenubar;

	if Configuration.Application.RestoreROMOnStartup then
	begin
		Menu.Visible := False;
		Window.Perform(actROMLoadPrevious);
	end
	else
		Menu.Visible := True;
end;

procedure CaniNES_ProcessFrame;
begin
//	if ProcessingFrame then Exit;
	ProcessingFrame := True;

	if Console.GotCartridge then
	begin
		Window.RunFrame;

		if Pause then
		begin
			NES_APU.Stop;
			Window.DrawIcon(ICON_PAUSE);
		end
		else
			Window.DrawIcon(ICON_NONE, ICON_PAUSE);
	end
	else
		Menu.Visible := True;

	if Menu.Visible then
	begin
		InfoBox.Layer.Visible := False;
		Menu.Draw;
	end
	else
	begin
		// Show debug info box at top left corner
		if {(not Pause) and} (Configuration.Application.ShowFrameTime > 0) then
			InfoBox.Update
	   else
			InfoBox.Layer.Visible := False;
	end;

	Window.DoFrame;

	if not Console.FastForward then
	begin
		Window.ProcessFrame(Tick);
		Window.Framecounter := 0;
		Tick := SDL_GetTicks;
	end;

	ProcessingFrame := False;
end;

procedure CaniNES_Shutdown;
begin
	if NES_APU <> nil then
		NES_APU.Stop;

	Configuration.Display.Window.X := Window.Video.Window.x;
	Configuration.Display.Window.Y := Window.Video.Window.y;
	Configuration.Display.Window.Scale := Window.Scale;

	//DeleteDirectory(TempPath, False);

	InfoBox.Free;
	Menu.Free;
	FontManager.Free;
	ConfigManager.Save;
	SaveBindings;
	ConfigManager.Free;
	Console.Free;
	Window.Free;
end;


end.

