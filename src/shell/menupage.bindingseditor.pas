unit MenuPage.BindingsEditor;

{$mode delphi}

interface

uses
	Classes, SysUtils,
	Graphics32,
	MainMenu, MenuHandler,
	NES.Types;

type
	TBindingBrowser = class(TMenuPageClass)
	private
		Subpage: TMenuPage;

		procedure   CmdEditBinding(Entry: TMenuEntry);
	public
		constructor Create; override;
	end;

	procedure PageEnter_Bindings(Page: TMenuPage);


implementation

uses
	MainWindow, NES.Config,
	InputBindings, Logging, FileUtil,
	SDL2;

var
	BindingBrowser: TBindingBrowser;

// ============================================================================
// Bindings Editor
// ============================================================================

constructor TBindingBrowser.Create;
begin
	inherited Create;

	BindingBrowser := Self;

	Page := Menu.AddPage('Input Bindings', '', MainPage);
	Page.FullScreen := True;
	Page.Width := MenuRenderer.FrameBuffer.Width;
	Dec(Page.ItemHeight, 1);
	Page.OnEnter := PageEnter_Bindings;
	Menu.BindingsPage := Page;

	Subpage := Menu.AddPage('Bind Action', '', nil);
	Subpage.Items.Clear;
	Subpage.AddHeader('Press key/joypad button now');
	Subpage.AddEntry('Escape to cancel', 0);
end;

procedure TBindingBrowser.CmdEditBinding(Entry: TMenuEntry);
var
	i: Integer;
	Action: TAction;
begin
	i := Page.Items.IndexOf(Entry) - 1;
	if i >= 0 then
	begin
		Action := TAction(i);
//		{$IFDEF DEBUG}Log('Binding action: ', BindingNames[Action]);{$ENDIF}
		Menu.WaitForBinding(@Bindings[Action]);
		Subpage.HeaderCaption := 'Bind action: ' + BindingNames[Action];
		Menu.SwitchPage(Subpage);
		Page.OnEnter := PageEnter_Bindings;
	end;
end;

procedure PageEnter_Bindings(Page: TMenuPage);
var
	Act: TAction;
	Bind: PInputBinding;
	SK, SJ: String;
begin
	Page.OnEnter := nil;

	Page.Items.Clear;
	Page.AddBackCommand;

	for Act in TAction do
	begin
		Bind := @Bindings[Act];

		SK := '';
		if Bind.Key > 0 then
		begin
			if Bind.Shift then
				SK := 'Shift + ';
			SK := SK + SDL_GetKeyName(Bind.Key);
		end;

		if (Bind.PadNum >= 0) and (Bind.PadButton <> 0) then
			SJ := Format('Joy %d Button %d', [Bind.PadNum, Bind.PadButton])
		else
			SJ := '';

		Page.AddEntry(
			Format('%-36s  %-23s  %-20s', [ BindingNames[Act], SK, SJ ]),
			0, $FFFFFFFF, BindingBrowser.CmdEditBinding);
	end;

end;

end.

