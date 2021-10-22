unit MenuPage.Palette;

{$mode delphi}

interface

uses
	Classes, SysUtils,
	MenuHandler;

type
	TPalettePage = class(TMenuPageClass)
	public
		procedure CmdOpenPalette(Entry: TMenuEntry);
		procedure CmdUseGeneratedPalette(Entry: TMenuEntry);

		constructor Create(BrowserPage: TMenuPage); overload;
	end;

	procedure PageEnter_Palette(Page: TMenuPage);


implementation

uses
	MainWindow, MainMenu,
	NES.Config, NES.Types, NES.Console,
	FileUtil, StrUtils;

var
	PalettePage: TPalettePage;

// ============================================================================
// Palette setup
// ============================================================================

constructor TPalettePage.Create(BrowserPage: TMenuPage);
begin
	inherited Create;

	PalettePage := Self;

	Page := BrowserPage;
//	Page.FullScreen := True;
	Page.OnEnter := PageEnter_Palette;
end;

procedure TPalettePage.CmdUseGeneratedPalette(Entry: TMenuEntry);
begin
	Configuration.Display.Palette.Filename := '';
	NES_PPU.Palette.PaletteFileUsed := False;
	Window.UpdatePalette;
end;

procedure TPalettePage.CmdOpenPalette(Entry: TMenuEntry);
begin
	if NES_PPU.Palette.LoadFromFile(Entry.Data) then
	begin
		Configuration.Display.Palette.Filename := ExtractFileName(Entry.Data);
		Window.UpdatePalette;
	end;
end;

procedure PageEnter_Palette(Page: TMenuPage);
var
	S: String;
	Files: TStringList;
begin
	Page.Items.Clear;
	Page.AddBackCommand;
	Page.HeaderCaption := 'Palette presets';

	Page.AddEntry('<Default>', 0, Palette[COLOR_MENU_SETTING], PalettePage.CmdUseGeneratedPalette);

	Files := FindAllFiles(Configuration.Application.Paths.Palette, PaletteFileExts, False);
	try
		for S in Files do
			Page.AddFile(S, True, Palette[COLOR_MENU_NORMAL], PalettePage.CmdOpenPalette);
	finally
		Files.Free;
	end;
end;

end.

