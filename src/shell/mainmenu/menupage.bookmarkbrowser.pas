unit MenuPage.BookmarkBrowser;

{$mode delphi}

interface

uses
	Classes, SysUtils,
	MenuHandler;

type
	TBookmarkBrowser = class(TMenuPageClass)
	public
		constructor Create; override;
	end;

	procedure PageEnter_Bookmarks(Page: TMenuPage);


implementation

uses
	BookmarkManager, MainWindow, MainMenu,
	NES.Config, NES.Console, InputBindings,
	StrUtils;

var
	{%H-}BookmarkBrowser: TBookmarkBrowser;

// ============================================================================
// Bookmark Browser
// ============================================================================

constructor TBookmarkBrowser.Create;
begin
	inherited Create;

	BookmarkBrowser := Self;
end;

procedure PageEnter_Bookmarks(Page: TMenuPage);
var
	S: String;
begin
	Page.FullScreen := True;
	Page.Width := MenuRenderer.FrameBuffer.Width;
	//Page.Width := Trunc(Menu.Width * 1.1);
	Page.Items.Clear;
	Page.AddBackCommand;

	if (Console.GotCartridge) and (not Console.LoadedFile.IsEmpty) then
		Page.AddCommand(IfThen(Bookmarks.Contains(Console.LoadedFile),
			'Remove From Favourites', 'Add To Favourites'), actFavourite);

	if Bookmarks.Items.Count < 1 then
		Page.AddText('No ROMs favourited')
	else
	for S in Bookmarks.Items do
		if not S.IsEmpty then
			Page.AddFile(S, True, Palette[COLOR_MENU_NORMAL], Page.CmdOpenFile);

	Menu.UpdateFilelisting;
end;

end.

