unit MainMenu;

{$mode delphi}

interface

uses
	Classes, SysUtils,
	MenuHandler, Basement.Renderer.Overlay;

	procedure CreateMainMenu;
	procedure UpdateMainMenu;

var
	MenuRenderer: TOverlayRenderer;
	MainPage: TMenuPage;


implementation

uses
	MainWindow, InputBindings,
	NES.Types, NES.Config, NES.Console,
	Logging, FileUtil,
	MenuPage.FileBrowser,
	MenuPage.CheatBrowser,
	MenuPage.CartInfo,
	MenuPage.BindingsEditor,
	MenuPage.Palette,
	MenuPage.MRUBrowser,
	MenuPage.BookmarkBrowser,
	MenuPage.DebugLog,
	MenuPage.AboutDialog;

// ============================================================================
// Main menu
// ============================================================================

procedure UpdateMainMenu;
var
	PT: AnsiString;
	i: Integer = -1;
begin
	if not Assigned(Menu) then Exit;

	if Menu.Visible then
	begin
		PT := Menu.CurrentPage.Title;
		i := Menu.GetItemIndex;
	end
	else
		PT := '';

	CreateMainMenu;

	if not PT.IsEmpty then
	begin
		Menu.Show;
		Menu.SwitchPage(PT);
		Menu.UpdateFilelisting;
		if i >= 0 then
			Menu.SetItemIndex(i);
	end;
end;

procedure CreateMainMenu;
var
	Page: TMenuPage;

	procedure AddCommand(Name: String; Cmd: TAction);
	begin
		MainPage.AddCommand(Name, Cmd);
	end;

var
	S: String;
begin
	Menu.Free;
	Menu := TEmulatorMenu.Create;
	Menu.ShowAsWindow := False;

	with Menu do
	begin
		MainPage := AddPage('Main', 'CaniNES Menu');

			MainPage.AddHeader('File');
				MainPage.AddPageEntry('Load ROM');
				AddCommand('Reload Previous ROM', actROMLoadPrevious);
				MainPage.AddPageEntry('Recent Files');
				MainPage.AddPageEntry('Favourites');
				if NES_APU.WavRecording then
					AddCommand('Stop Recording Audio', actRecordWAV) else
					AddCommand('Record Audio', actRecordWAV);

			MainPage.AddHeader('Game');

				MainPage.AddPageEntry('Cart Info');
				MainPage.AddPageEntry('Cheats');
				AddCommand('Load State', actStateLoad);
				AddCommand('Save State', actStateSave);
				if Console.MovieManager.Loaded then
				begin
					if Console.MovieManager.IsPlaying then
						AddCommand('Stop Movie', actMoviePlayback)
					else
						AddCommand('Play Movie', actMoviePlayback);
				end;

			TCartInfoBrowser.Create(
				AddPage('Cart Info', 'Cartridge Information', MainPage));

			TAboutDialog.Create(
				AddPage('About', 'About CaniNES', MainPage));

			TCheatBrowser.Create(
				AddPage('Cheats', 'Cheat Browser', MainPage));

			with AddPage('Debug Log', '', MainPage) do
			begin
				FullScreen := True;
				OnEnter := PageEnter_DebugLog;
			end;

			with AddPage('Recent Files', '', MainPage) do
				OnEnter := PageEnter_MRU;

			with AddPage('Favourites', '', MainPage) do
				OnEnter := PageEnter_Bookmarks;

			MainPage.AddHeader('Console');
				AddCommand('Toggle Fullscreen', actToggleFullscreen);
				AddCommand('Pause', actConsolePause);
				AddCommand('Reset', actConsoleReset);
				AddCommand('Power Cycle', actConsoleRestart);

			MainPage.AddHeader('Options');
				MainPage.AddPageEntry('Application');
				MainPage.AddPageEntry('Input');
				MainPage.AddPageEntry('Display');
				MainPage.AddPageEntry('Audio');
				MainPage.AddPageEntry('Emulation');

		S := 'Application';
		Page := AddPage(S, 'Application Settings', MainPage);
			Page.AddSettings(S);

		S := 'Input';
		Page := AddPage(S, 'Input Settings', MainPage);
			Page.AddPageEntry('Input Bindings');
			Page.AddSetting(S, 'AllowInvalidInput');
			Page.AddSetting(S, 'PadVisual.Count');
			Page.AddSetting(S, 'PadVisual.MoviesOnly');
			Page.AddHeader('Peripherals');
				Page.AddSetting(S, 'FourScore');
				Page.AddHeader('Zapper', 2);
					Page.AddSetting(S, 'Zapper.Enabled');
					Page.AddSetting(S, 'Zapper.HidePointer');
					Page.AddSetting(S, 'Zapper.DetectionRadius');
			Page.AddHeader('User Interface');
				Page.AddSetting(S, 'UI.Repeat.Initial');
				Page.AddSetting(S, 'UI.Repeat.Interval');

		S := 'Display';
		Page := AddPage(S, 'Display Settings', MainPage);
			Page.AddSettings(S);
				Page.AddSetting('Renderer', 'Vsync');
				Page.AddSetting('Renderer', 'AutoswitchResolution');
				Page.AddSetting('Window',   'AspectRatio');
				Page.AddSetting('Renderer', 'ScalingMethod');
				Page.AddSetting('Renderer', 'FlipMode');
				Page.AddSetting('GUI',      'BoxArtUpscale');

				S := 'Overscan';
				Page.AddPageEntry(S);
				with AddPage(S, '', Page) do
					AddSettings(S);

				S := 'On-Screen Display';
				Page.AddPageEntry(S);
					with AddPage(S, '', Page) do
					begin
						AddSettingsItems('GUI', 'OSD.*');
						AddSettingsItems('GUI', 'Align.*');
					end;

				Page.AddHeader('Palette');
				Page.AddSettings('Palette.NTSC');
				S := 'Palette presets';
				TPalettePage.Create(AddPage(S, '', Page));
				Page.AddPageEntry(S);

				Page.AddHeader('Filters');
				S := 'CRT simulation';
				Page.AddPageEntry(S);
					with AddPage(S, '', Page) do
						AddSettings('Filter.CRT');
				S := 'NTSC simulation';
				Page.AddPageEntry(S);
					with AddPage(S, '', Page) do
						AddSettings('Filter.NTSC');

		S := 'Audio';
		Page := AddPage(S, 'Audio Settings', MainPage);
			Page.AddSetting(S, 'Latency');
			Page.AddSetting(S, 'DynamicRate');
			Page.AddSetting('Emulation.APU', 'ReduceDmcPopping');
			Page.AddSetting('Emulation.APU', 'SilenceTriangleHighFreq');
			Page.AddHeader('Channel Mixer');
				Page.AddSetting(S, 'Volume');
				Page.AddSettingsItems('Emulation.APU.Channels', '*Volume');
				S := 'Channel Panning';
				Page.AddPageEntry(S);
					with AddPage(S, '', Page) do
						AddSettingsItems('Emulation.APU.Channels', '*Panning');

		S := 'Emulation';
		Page := AddPage(S, 'Emulation Settings', MainPage);
			Page.AddSettings(S);

			Page.AddHeader('Power-On State');
			Page.AddSettings('Emulation.PowerOnState');

			Page.AddHeader('Hardware');

			S := 'PPU Settings';
			Page.AddPageEntry(S);
				with AddPage(S, '', Page) do
					AddSettings('Emulation.PPU');

			S := 'Famicom Disk System Settings';
			Page.AddPageEntry(S);
				with AddPage(S, '', Page) do
					AddSettings('Emulation.FDS');

		MainPage.AddHeader('CaniNES');
			MainPage.AddPageEntry('Debug Log');
			AddCommand('Exit', actAppExit);

		// Create special pages
		//
		TBindingBrowser.Create;
		TMRUBrowser.Create;
		TFileBrowser.Create(
			AddPage('Load ROM', 'File Browser', MainPage),
				Configuration.Application.Paths.ROM);

		// done
		//
		MainPage.Width := MenuRenderer.Font.GlyphWidth * 30;
		Changed;
	end;
end;


end.

