unit MenuPage.CheatBrowser;

{$mode delphi}

interface

uses
	Classes, SysUtils,
	MenuHandler;

type
	TCheatBrowser = class(TMenuPageClass)
	private
		procedure   CmdToggleCheat(Entry: TMenuEntry);
	public
		constructor Create(BrowserPage: TMenuPage); overload;
	end;

	procedure PageEnter_Cheats(Page: TMenuPage);


implementation

uses
	MainWindow, MainMenu,
	InputBindings, Basement.Renderer.Overlay,
	NES.Types, NES.Cheats,
	NES.Config, NES.Console,
	Logging, FileUtil,
	Graphics32;

var
	CheatBrowser: TCheatBrowser;

// ============================================================================
// Cheat Browser
// ============================================================================

constructor TCheatBrowser.Create(BrowserPage: TMenuPage);
begin
	inherited Create;

	CheatBrowser := Self;

	Page := BrowserPage;

	Page.FullScreen := True;
	Page.Width := MenuRenderer.FrameBuffer.Width;
	Dec(Page.ItemHeight, 1);

	Page.OnEnter := PageEnter_Cheats;
end;

function GetEntryColor(Code: TCodeInfo): Cardinal;
begin
	if Code.Enabled then
		Result := $FF99FF66
	else
		Result := $FFFFCCCC;
end;

procedure TCheatBrowser.CmdToggleCheat(Entry: TMenuEntry);
var
	i: Integer;
	Code: TCodeInfo;
begin
	//Log(Page.Items.IndexOf(Entry), ':', Entry.Caption);
	i := Page.Items.IndexOf(Entry) - 1;
	if (i < 0) or (i >= Console.CheatManager.AvailableCodes.Count) then Exit;

	Code := Console.CheatManager.AvailableCodes[i];
	Code.Enabled := not Code.Enabled;
	Entry.Checked := Code.Enabled;
	Entry.Color := GetEntryColor(Code);
end;

procedure PageEnter_Cheats(Page: TMenuPage);
var
	Code: TCodeInfo;
begin
	Page.Items.Clear;
	Page.HeaderCaption := 'Cheat Browser';
	Page.AddBackCommand;

	if not Console.GotCartridge then Exit;

	Console.CheatManager.GetAvailableCheats;

	for Code in Console.CheatManager.AvailableCodes do
		Page.AddEntry(Code.Description, 0, GetEntryColor(Code),
			CheatBrowser.CmdToggleCheat).Checkbox := True;

	if Page.Items.Count < 1+1 then
		Page.AddEntry('No cheats found', 0);
end;

end.

