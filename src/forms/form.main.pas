unit Form.Main;

{$mode Delphi}

interface

uses
	Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
	Menubar, ConfigurationManager;

type
	TFormMain = class(TForm)
		Menu: TMainMenu;
		miDummy: Menus.TMenuItem;
		procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
		procedure FormShow(Sender: TObject);
		procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
	private
		pb: TPanel;

		PrevWindowState: record
			WasMaximized: Boolean;
			Area: TRect;
		end;

		MenuInfo: record
			LastAddedMenuItem,
			SubmenuOwner: Menus.TMenuItem;
		end;

		procedure CreateContainer(Sender: TObject);

		procedure OnUpdateMenu(Finished: Boolean);
		procedure OnMenuUpLevel;
		procedure OnAddSubMenu (const SubMenu: Menubar.TSubMenu; const Item: Menubar.TMenuItem);
		procedure OnAddMenuItem(const SubMenu: Menubar.TSubMenu; const Item: Menubar.TMenuItem);
		procedure OnCheckMenuItem(const Item: Menubar.TMenuItem; Checked: Boolean);
		procedure MenuItemClickHandler(Sender: TObject);

		procedure OnSettingChange(Item: TConfigItem);

		procedure RenderLoop(Data: PtrInt);
	public
	end;

var
	FormMain: TFormMain;

implementation

{$R *.lfm}

uses
	Windows, LCLType,
	MainWindow,
	Canines.Main,
	Basement.Window,
	InputBindings, MenuHandler,
	NES.Config, NES.Console;

const
	MODE_RUN = 0;
	MODE_INIT_FULLSCREEN = 1;

var
	PrevWndProc: WNDPROC;
	Closing: Boolean = False;
	RunMode: Integer = MODE_RUN;

// ================================================================================================
// Engine
// ================================================================================================

procedure TFormMain.CreateContainer(Sender: TObject);
begin
	if Configuration.Display.Window.FullScreen then
		Window.Settings.WinHandle := nil
	else
	begin
		if pb <> nil then
			pb.Free;
		pb := TPanel.Create(FormMain);
		pb.Parent := Self;
		pb.Align := alClient;
		pb.Cursor := crCross;
		pb.Show;
		ActiveControl := pb;

		BasementOptions.WinHandle := Pointer(pb.Handle);
		if Window <> nil then
			Window.Settings.WinHandle := BasementOptions.WinHandle;
	end;
end;

procedure MenuEnter(Entered: Boolean);
begin
	if Entered then NES_APU.Stop;
end;

function WndCallback(AHWND: HWND; uMsg: UINT; wParam: WParam; lParam: LParam): LRESULT; stdcall;
begin
	case uMsg of
		WM_ENTERSIZEMOVE,
		WM_ENTERMENULOOP: MenuEnter(True);
		WM_EXITSIZEMOVE,
		WM_EXITMENULOOP:  MenuEnter(False);
	end;
	Result := CallWindowProc(PrevWndProc, Ahwnd, uMsg, WParam, LParam);
end;

procedure TFormMain.RenderLoop(Data: PtrInt);
begin
	while not QuitFlag do
	begin
		case RunMode of

			MODE_RUN:
				CaniNES_ProcessFrame;

			MODE_INIT_FULLSCREEN:
			begin
				QuitFlag := not Window.ReinitWindow;
				RunMode := MODE_RUN;
			end;

		end;
	end;

	if not Closing then Close;
end;

// ================================================================================================
// Form events
// ================================================================================================

procedure TFormMain.FormShow(Sender: TObject);
begin
	OnShow := nil;
	PrevWndProc := Windows.WNDPROC(SetWindowLongPtr(Self.Handle, GWL_WNDPROC, PtrInt(@WndCallback)));

	CreateContainer(nil);

	CaniNES_Init;

	if QuitFlag then
	begin
		Close;
		Exit;
	end;

	Window.OnUpdateMenu    := OnUpdateMenu;
	Window.OnAddSubMenu    := OnAddSubMenu;
	Window.OnAddMenuItem   := OnAddMenuItem;
	Window.OnMenuUpLevel   := OnMenuUpLevel;
	Window.OnCheckMenuItem := OnCheckMenuItem;
	Window.OnReinitWindow  := CreateContainer;

	DefaultConfigItemCallback := OnSettingChange;

	ClientWidth  := 256 * 3;
	ClientHeight := 240 * 3;

	Window.InitMenubar;

	PrevWindowState.Area := Bounds(Left, Top, Width, Height);

	Application.QueueAsyncCall(RenderLoop, 0);
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
	Window.Close;
	Closing := True;
end;

procedure TFormMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
	CaniNES_Shutdown;
end;

// ================================================================================================
// Settings
// ================================================================================================

procedure TFormMain.OnSettingChange(Item: TConfigItem);
begin
	Window.OnSettingChange(Item.ID);

	case Item.ID of

		cfgFullScreen:
			RunMode := MODE_INIT_FULLSCREEN;

	end;
end;

// ================================================================================================
// Menubar
// ================================================================================================

procedure TFormMain.OnUpdateMenu(Finished: Boolean);
begin
	if not Finished then
		Menu.Items.Clear;
	MenuInfo.SubmenuOwner := nil;
	MenuInfo.LastAddedMenuItem := nil;
end;

procedure TFormMain.OnMenuUpLevel;
begin
	if MenuInfo.SubmenuOwner <> nil then
		MenuInfo.SubmenuOwner := MenuInfo.SubmenuOwner.Parent;
end;

procedure TFormMain.OnAddSubMenu(const SubMenu: Menubar.TSubMenu; const Item: Menubar.TMenuItem);
begin
	MenuInfo.SubmenuOwner := MenuInfo.LastAddedMenuItem;
end;

procedure TFormMain.OnAddMenuItem(const SubMenu: Menubar.TSubMenu; const Item: Menubar.TMenuItem);
var
	MI: Menus.TMenuItem = nil;
	S: String;
begin
	if Item = nil then Exit;

	S := Item.MnemonicCaption;
	if S.IsEmpty then
		S := Item.Caption;
	S := S.Replace('& ', '&& ', [rfReplaceAll]);

	if SubMenu = Window.Menubar.RootMenu then
	begin
		MI := Menus.TMenuItem.Create(Menu);
		MI.Caption := S;
		Menu.Items.Add(MI);
	end
	else
	if MenuInfo.SubmenuOwner <> nil then
	begin
		MI := Menus.TMenuItem.Create(MenuInfo.SubmenuOwner);
		if Item.IsSeparator then
		begin
			if (MenuInfo.LastAddedMenuItem <> nil) and
				(MenuInfo.LastAddedMenuItem.Caption = '-') then
			begin
				MI.Free;
				Exit;
			end;
			MI.Caption := '-';
		end
		else
		begin
			MI.Caption := S;
			MI.Tag := PtrInt(Item);
			MI.OnClick := MenuItemClickHandler;
			if Item.IsCheckItem then
			begin
				MI.GroupIndex := Menu.Items.Count+1;
				MI.Checked := Item.Checked;
			end;
		end;
		MenuInfo.SubmenuOwner.Add(MI);
	end;

	if MI <> nil then
		MenuInfo.LastAddedMenuItem := MI;
end;

procedure TFormMain.OnCheckMenuItem(const Item: Menubar.TMenuItem; Checked: Boolean);
var
	MI, RI: Menus.TMenuItem;
begin
	for RI in Menu.Items do
		for MI in RI do
			if (MI.Tag <> 0) and (Menubar.TMenuItem(PtrInt(MI.Tag)) = Item) then
			begin
				MI.Checked := Checked;
				Exit;
			end;
end;

procedure TFormMain.MenuItemClickHandler(Sender: TObject);
var
	MI: Menus.TMenuItem;
	Item: Menubar.TMenuItem;
begin
	MI := Menus.TMenuItem(Sender);
	if MI.Tag = 0 then Exit;

	Item := Menubar.TMenuItem(PtrInt(MI.Tag));
	if (Item = nil) or (Item.IsSeparator) then Exit;

	case Item.Action of

		actROMLoadFromMenu:
		begin
			Console.LoadROM(Item.Data);
			EmulationMode := NORMAL;
		end;

		else
			Item.Click;
	end;
end;


end.

