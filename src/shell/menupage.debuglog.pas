unit MenuPage.DebugLog;

{$mode delphi}

interface

uses
	Classes, SysUtils,
	MenuHandler,
	NES.Types;

	procedure PageEnter_DebugLog(Page: TMenuPage);


implementation

uses
	MainWindow, TextOutput,
	NES.Config,
	Logging, FileUtil;

// ============================================================================
// Debug Log
// ============================================================================

procedure PageEnter_DebugLog(Page: TMenuPage);
var
	S: String;
begin
	Page.Items.Clear;
	Page.AddBackCommand;

	// don't use AddText() as then items can't be scrolled
	for S in Logger.Log do
		Page.AddEntry(FixChars(S), 0);
end;

end.

