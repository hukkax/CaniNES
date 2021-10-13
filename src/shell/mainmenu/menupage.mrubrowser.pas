unit MenuPage.MRUBrowser;

{$mode delphi}

interface

uses
	Classes, SysUtils,
	MenuHandler;

type
	TMRUBrowser = class(TMenuPageClass)
	public
		constructor Create; override;
	end;

	procedure PageEnter_MRU(Page: TMenuPage);


implementation

uses
	MainWindow, NES.Config,
	Logging, FileUtil;

var
	{%H-}MRUBrowser: TMRUBrowser;

// ============================================================================
// MRU Browser
// ============================================================================

constructor TMRUBrowser.Create;
begin
	inherited Create;

	MRUBrowser := Self;
end;

procedure PageEnter_MRU(Page: TMenuPage);
var
	S: String;
	i: Integer;
begin
	Page.Width := Trunc(Menu.Width * 1.1);
	Page.Items.Clear;
	Page.AddBackCommand;

	for i := 0 to MRUcount-1 do
	begin
		S := Configuration.Application.MRU[i];
		if not S.IsEmpty then
			Page.AddFile(S, True, Palette[COLOR_MENU_NORMAL], Page.CmdOpenFile);
	end;

	Menu.UpdateFilelisting;
end;

end.

