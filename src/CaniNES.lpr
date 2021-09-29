program CaniNES;

{$MODE DELPHI}{$H+}
{$I canines.inc}
{$I basement.inc}

uses
	{$I basement-prestart1.inc}
	Classes, SysUtils, Types, Math,
	LazLogger, Logging,
	FileUtil, Basement.Util,
	Basement.Window, MainWindow,
	TextOutput, Basement.Renderer.Overlay,
	Basement.Renderer.NTSC, Basement.Renderer.CRT,
	Graphics32, SDL2,
	NES.Types, NES.Config, ConfigurationManager,
	MenuHandler, MainMenu, InfoBoxDisplay, InputBindings,
	NES.Console, NES.Cartridge, NES.Mapper, NES.ROM,
	NES.MemoryManager, NES.InputManager, NES.Controllers,
	NES.CPU, NES.PPU, NES.APU, NES.APU.Mixer;

var
	Console: TConsole;
	S, ROMPath: String;

//==================================================================================================

	// ======================================================================
	// Initialization
	// ======================================================================

{$R *.res}

begin
	{$I basement-startup.inc}

	DebugLn('================================================');
	DebugLn(Format('%s v%s starting.', [APPNAME, APPVERSION]));
	DebugLn('================================================');

	Pause := False;

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
		{$IFDEF CPU32} Caption := APPNAME; {$ENDIF}
		{$IFDEF CPU64} Caption := APPNAME + ' x64'; {$ENDIF}

		Width  := NES_RESOLUTION_X;
		Height := NES_RESOLUTION_Y;

		X := Configuration.Display.Window.X;
		Y := Configuration.Display.Window.Y;

		GetSettings(@BasementOptions);
	end;

	Window := TNESWindow.Create;

	if (Configuration.Display.Window.X > 0) and (Configuration.Display.Window.Y > 0) then
		SDL_SetWindowPosition(Window.Video.Window,
			Configuration.Display.Window.X, Configuration.Display.Window.Y);

	ROMPath := IncludeTrailingPathDelimiter(AppPath + 'rom');

	Console := TConsole.Create(AppPath, Window.FrameBuffer);

	if Configuration.Display.Palette.Filename <> '' then
	begin
		S := GetDataFile(Configuration.Display.Palette.Filename);
		if S <> '' then
			NES_PPU.Palette.LoadFromFile(S);
	end
	else
	with Configuration.Display.Palette do
	begin
		NES_PPU.Palette.Generate(Saturation, HueShift, Contrast, Brightness, Gamma);
		DebugLn('Generated NTSC palette.');
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

//==================================================================================================

	// ======================================================================
	// Main loop
	// ======================================================================

	while not QuitFlag do
	begin
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
	end;

	// ======================================================================

	NES_APU.Stop;

	//DeleteDirectory(TempPath, False);

	Configuration.Display.Window.X := Window.Video.Window.x;
	Configuration.Display.Window.Y := Window.Video.Window.y;
	Configuration.Display.CRT.Enabled := CRTRenderer.Enabled;

	InfoBox.Free;
	Menu.Free;
	FontManager.Free;
	ConfigManager.Save;
	SaveBindings;
	ConfigManager.Free;
	Console.Free;
	Window.Free;
end.

