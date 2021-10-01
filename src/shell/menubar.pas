unit Menubar;

{$mode Delphi}

interface

uses
	Types, Classes, SysUtils,
	Generics.Collections,
	Basement.Window, Basement.Font,
	InputBindings,
	Graphics32;

const
	Dots = '...'; //' …';

type
	TSubMenuID = Word;

	TSubMenu = class;

	TMenuMetrics = record
		ROOTSPACING,
		ITEMSPACING,
		SEPHEIGHT,
		PADDING_X,
		PADDING_Y: Integer;
	end;

	TMenuItemState = ( misUndrawn, misDisabled, misNormal, misSelected );

	TMenuItem = class
	private
		Flags: bitpacked record
			IsSeparator,
			IsCheckbox:  Boolean;
		end;

		function    GetChecked: Boolean;
		procedure   SetChecked(Value: Boolean);
	public
		Action:     TAction;
		Caption:    String;
		Data:       String;
		ParentMenu: TSubMenu;
		SubMenu:    TSubMenu;
		DrawnState: TMenuItemState;
		Rect:       TRect;
		ValuePtr:   PBoolean; // for checkbox
		OnScreen:   Boolean;

		function    GetWidth: Word;
		function    GetHeight(FullHeight: Boolean = False): Word;
		function    SubmenuActive: Boolean;
		function    ActivateSubMenu(Show, Sticky: Boolean): Boolean;
		function    AddSubMenu(nID: TSubMenuID): TSubMenu;
		function    GetDrawableCaption: String;
		function    Click: Boolean;

		constructor Create(AParent: TSubMenu; const ACaption: String; AAction: TAction = actNone);

		property    Checked: Boolean read GetChecked write SetChecked;
	end;

	TSubMenu = class
	private
		NeedUpdate: Boolean;
		Drawn:      Boolean;
		Sticky:     Boolean; // keyboard used, disallow some mouse interactions
		Scrollable: Boolean; // more than MaxItemCount items in submenu?
		Metrics:    TMenuMetrics;

		procedure   Draw;
		procedure   SelectionChanged;
	public
		Visible:    Boolean;
		ID:         TSubMenuID;
		Caption:    String;
		ScrollPos:  Word;
		Parent:     TSubMenu;
		ActiveItem: TMenuItem;
		Rect:       TRect;

		Items:      TObjectList<TMenuItem>;
		Buffer:     TBitmap32; // internal framebuffer
		Position:   TPoint;    // pixel coords on destination framebuffer

		function    GetWidth:  Integer;
		function    GetHeight: Integer;

		procedure   SetScrollPos(i: Integer);
		procedure   PreviousItem;
		procedure   NextItem;
		procedure   ActivateItem(Item: TMenuItem);
		procedure   Activate(Show: Boolean);

		procedure   DrawTo(const DestBuffer: TBitmap32);
		procedure   Changed;

		function    AddItem(const ACaption: String; AAction: TAction = actNone;
		            const AData: String = ''): TMenuItem;
		function    AddCheckItem(const ACaption: String; const ValuePtr: PBoolean): TMenuItem;
		function    AddSeparator: TMenuItem;

		function    OnMouseMove(P: TPoint): Boolean;
		function    OnMouseWheel(WheelDelta: Integer): Boolean;

		constructor Create(AParent: TSubMenu; nID: TSubMenuID);
		destructor  Destroy; override;
	end;

	TMenuBar = class
	private
		Items:      TObjectList<TSubMenu>;
		Buffer:     TBitmap32;
		Font:       TRendererFont;
		FActive:    Boolean;
		MousePos:   TPoint;
		MouseMoved: Boolean;

		procedure   SetActive(Value: Boolean);
		function    GetMenuAt(P: TPoint): TSubMenu;
		function    GetX(V: Single): Integer;
		function    GetY(V: Single): Integer;
	public
		ActiveMenu: TSubmenu; // currently active submenu
		RootMenu:   TSubMenu;
		Visible:    Boolean;
		Changed:    Boolean;
		Hovering:   Boolean;
		Metrics:    TMenuMetrics;
		MaxItemCount: Byte;

		function    ProcessKey(Key: Integer; Shift: TShiftState; Pressed, Repeated: Boolean): Boolean;
		procedure   OnMouseButton(Button: Basement.Window.TMouseButton; Pressed: Boolean);
		procedure   OnMouseMove(P: TPoint);

		procedure   Draw;
		procedure   ForceUpdate;
		procedure   ForceRedraw;
		function    RefreshActiveState: Boolean;

		constructor Create(const ABuffer: TBitmap32; const AFont: TRendererFont);
		destructor  Destroy; override;

		property    Active: Boolean read FActive write SetActive;
	end;


implementation

uses
	Math, SDL2,
	MainWindow,
	Graphics32.Extra,
	NES.Types, NES.Config, ConfigurationManager;

const
	SUBMENU_PADDING_X   = 0.5;
	SUBMENU_PADDING_Y   = 0.25;
	SUBMENU_ITEMSPACING = 0.1;
	SUBMENU_ROOTSPACING = 0.5;
	SUBMENU_SEPHEIGHT   = 0.3;
	SUBMENU_MAXWIDTH    = 0.76;

	CheckboxGlyph: array[Boolean] of String = ( '  ', '✓ ' );
	//STR_CHECKBOX_NO, STR_CHECKBOX_YES );

var
	Menubar: TMenuBar;
	FromKeyboard: Boolean;

//==================================================================================================
// TMenuItem
//==================================================================================================

constructor TMenuItem.Create(AParent: TSubMenu; const ACaption: String; AAction: TAction);
begin
	inherited Create;

	if AParent = nil then
		ParentMenu := Menubar.RootMenu
	else
		ParentMenu := AParent;

	Flags.IsSeparator := ACaption.IsEmpty;
	Caption := ACaption;
	Action := AAction;
	SubMenu := nil;
	DrawnState := misUndrawn;

	ParentMenu.Items.Add(Self);
	ParentMenu.Changed;
end;

function TMenuItem.AddSubMenu(nID: TSubMenuID): TSubMenu;
begin
	if SubMenu = nil then
		SubMenu := TSubMenu.Create(ParentMenu, nID);
	SubMenu.Caption := Self.Caption;
	Result := SubMenu;
end;

function TMenuItem.SubmenuActive: Boolean;
begin
	Result := (SubMenu <> nil) and (SubMenu.Visible);
end;

function TMenuItem.ActivateSubMenu(Show, Sticky: Boolean): Boolean;
begin
	Result := (SubMenu <> nil);
	if Result then
	begin
		SubMenu.Activate(Show);
		SubMenu.Sticky := Sticky;
	end;
end;

function TMenuItem.GetWidth: Word;
begin
	Result := Menubar.Font.TextWidth(GetDrawableCaption);
	if ParentMenu = Menubar.RootMenu then
		Inc(Result, Trunc(Menubar.Font.GlyphWidth * 0.4))
	else
	if SubMenu <> nil then
		Inc(Result, Trunc(Menubar.Font.GlyphWidth * 2.5));
end;

function TMenuItem.GetHeight(FullHeight: Boolean = False): Word;
begin
	if (not FullHeight) and (Flags.IsSeparator) then
		Result := MenuBar.Metrics.SEPHEIGHT
	else
		Result := Menubar.Font.GlyphHeight;
end;

function TMenuItem.GetChecked: Boolean;
begin
	Result := (ValuePtr <> nil) and (ValuePtr^ = True);
end;

procedure TMenuItem.SetChecked(Value: Boolean);
begin
	if (ValuePtr <> nil) and (Value <> ValuePtr^) then
	begin
		if Value then
		begin
			Flags.IsCheckbox  := True;
			Flags.IsSeparator := False;
		end;
		ValuePtr^ := Value;
		DrawnState := misUndrawn;
		ParentMenu.Changed;
	end;

	{if Value <> Flags.Checked then
	begin
		if Value then
		begin
			Flags.IsCheckbox  := True;
			Flags.IsSeparator := False;
		end;
		Flags.Checked := Value;
		DrawnState := misUndrawn;
		ParentMenu.Changed;
	end;}
end;

function TMenuItem.GetDrawableCaption: String;
begin
	if Flags.IsCheckbox then
		Result := CheckboxGlyph[Checked] + Caption
	else
	if Flags.IsSeparator then
		Result := ''
	else
		Result := Caption;
end;

function TMenuItem.Click: Boolean;
var
	Act: TAction;
begin
	// keep menu open if no action taken
	Result := (Action <> actNone) or (Flags.IsCheckbox);

	if Result then
	begin
		DrawnState := misUndrawn;
		if ParentMenu <> nil then
			ParentMenu.Drawn := False;

		// FIXME super hacky, get the configuration item from a pboolean
		// and call its callback
		//
		if Flags.IsCheckbox then
			Checked := Configuration.ToggleBool(ValuePtr);

		if Action <> actNone then
		begin
			if Action = actShowPage then
				Window.ShowMenuPage(Data, True)
			else
				Window.Perform(Action);
		end;
	end
	else
		ActivateSubMenu(not SubmenuActive, False); // toggle submenu
end;


//==================================================================================================
// TSubMenu
//==================================================================================================

constructor TSubMenu.Create(AParent: TSubMenu; nID: TSubMenuID);
begin
	inherited Create;

	ID := nID;
	Items := TObjectList<TMenuItem>.Create(True);
	Buffer := TBitmap32.Create;

	Position := Point(0, 0);
	Metrics := Menubar.Metrics;
	Visible := False;

	if AParent = nil then
		Parent := Menubar.RootMenu
	else
		Parent := AParent;

	if not Menubar.Items.Contains(Self) then
		Menubar.Items.Add(Self);
end;

destructor TSubMenu.Destroy;
begin
	Items.Free;
	Buffer.Free;

	inherited Destroy;
end;

function TSubMenu.GetWidth: Integer;
var
	Item: TMenuItem;
begin
	if Self = Menubar.RootMenu then
		Result := Menubar.Buffer.Width
	else
	begin
		Result := 0;
		for Item in Items do
			Result := Max(Result, Item.GetWidth);
		Inc(Result, Menubar.Font.GlyphWidth);
		Result := Min(Trunc(Menubar.Buffer.Width * SUBMENU_MAXWIDTH), Result); // limit submenu width
	end;
end;

function TSubMenu.GetHeight: Integer;
var
	Item: TMenuItem;
	FullHeightSep: Boolean;
	GH, H: Integer;
begin
	GH := Menubar.Font.GlyphHeight;
	if Self = Menubar.RootMenu then
		Result := GH + (Metrics.PADDING_Y * 2)
	else
	begin
		Scrollable := Items.Count > Menubar.MaxItemCount;
		Result := (Metrics.PADDING_Y * 2) - Metrics.ITEMSPACING;
		H := 0;
		for Item in Items do
			Inc(H, Item.GetHeight(Scrollable) + Metrics.ITEMSPACING);
		H := Min(H, Menubar.MaxItemCount * (GH + Metrics.ITEMSPACING));
		Inc(Result, H);
	end;
end;

procedure TSubMenu.Draw;

	function GetItemState(const Item: TMenuItem): TMenuItemState;
	begin
		if (Item = ActiveItem) and (not Item.Flags.IsSeparator) then
			Result := misSelected
		else
			Result := misNormal;
	end;

var
	Item: TMenuItem;
	I, X, Y, W, H: Integer;
	colFg, colBg: TColor32;
	State: TMenuItemState;
begin
	if (not NeedUpdate) or (Items.Count < 1) then
		Exit;

	X := Metrics.PADDING_X;
	Y := Metrics.PADDING_Y-1;

	colFg := Palette[COLOR_MENU_NORMAL];
	colBg := Palette[COLOR_MENU_BACKGROUND];

	// FreeType font rendering is slow and runs in the same thread as emulation so
	// we only want to paint the items that have changed state
	if not Drawn then // force full repaint initially
		for Item in Items do
			Item.DrawnState := misUndrawn;

	if not Drawn then
		Buffer.Clear(colBg);

	if Self = Menubar.RootMenu then
	begin
		H := Buffer.Height;

		for Item in Items do // top menubar, horizontal
		begin
			Item.OnScreen := True;
			W := Item.GetWidth;

			State := GetItemState(Item);
			if State <> Item.DrawnState then
			begin
				// undraw selection box
				if Item.DrawnState = misSelected then
					Buffer.FillRectS(Item.Rect, colBg);

				Item.DrawnState := State;

				Item.Rect := Bounds(X-Metrics.ROOTSPACING, 0, W+(Metrics.ROOTSPACING*2), H-1);

				if (Menubar.Active) and (Item = ActiveItem) then
					Buffer.DrawSelection(Item.Rect, Palette[COLOR_MENU_SELECTION]);

				Menubar.Font.DrawString(Buffer, X, Y, Item.GetDrawableCaption, colFg, Buffer.Width);

				if Item.SubMenu <> nil then
					Item.SubMenu.Position := Point(Max(0, Item.Rect.Left-(Metrics.PADDING_X div 2)), Item.Rect.Bottom);
			end;

			Inc(X, W + (Metrics.ROOTSPACING * 2));
		end;
	end
	else
	begin
		W := Metrics.PADDING_X div 2;

		// dropdown menu, vertical
		for I := ScrollPos to Min(ScrollPos + Menubar.MaxItemCount-1, Items.Count-1) do
		begin
			Item := Items[I];
			if Item = nil then Continue;

			Item.OnScreen := True;

			H := Item.GetHeight(Scrollable) + Metrics.ITEMSPACING;

			State := GetItemState(Item);
			if State <> Item.DrawnState then
			begin
				// undraw selection box
				if Item.DrawnState = misSelected then
					Buffer.FillRectS(Item.Rect, colBg);

				Item.DrawnState := State;

				if Item.Flags.IsSeparator then
				begin
					Item.Rect := Bounds(0, Y, Buffer.Width-1, H);
					Buffer.FillRectS(Bounds(Item.Rect.Left, Y + (H div 2) - 1, Item.Rect.Width, 2),
						Palette[COLOR_MENU_BORDER]);
				end
				else
				begin
					Item.Rect := Types.Rect(X-W, Y, Buffer.Width-W, Y+H);

					if Item = ActiveItem then
						Buffer.DrawSelection(Item.Rect, Palette[COLOR_MENU_SELECTION]);

					Menubar.Font.DrawString(Buffer, X, Y, Item.GetDrawableCaption, colFg, Buffer.Width);

					if Item.SubMenu <> nil then
					begin
						Item.SubMenu.Position := Point(
							Position.X + Buffer.Width - Menubar.Font.GlyphWidth,
							Position.Y + Y - Metrics.PADDING_Y);
						Menubar.Font.DrawString(Buffer,
							Buffer.Width - Metrics.PADDING_X - Menubar.Font.GlyphWidth, Y,
							STR_PAGE_NEXT, colFg, Buffer.Width);
					end;
				end;
			end;

			Inc(Y, H);
		end;

		if not Drawn then
			Buffer.ThickFrameRect(Buffer.BoundsRect, -2, Palette[COLOR_MENU_BORDER]);
	end;

	X := Position.X;
	Y := Position.Y;
	W := Buffer.Width;
	H := Buffer.Height;
	if (X + W) > Menubar.Buffer.Width then
	begin
		X := Menubar.Buffer.Width - W;
		Position.X := X;
	end;
	if (Y + H) > Menubar.Buffer.Height then
	begin
		Position.Y := Menubar.RootMenu.GetHeight;
	end;

	Rect := Bounds(X, Y, W, H-1);

	Drawn := True;
	NeedUpdate := False;
end;

procedure TSubMenu.DrawTo(const DestBuffer: TBitmap32);
begin
	if Visible then
	begin
		Draw;
		DestBuffer.Draw(Position.X, Position.Y, Buffer);
	end;
end;

procedure TSubMenu.Changed;
begin
	Buffer.SetSize(GetWidth, GetHeight);
	Menubar.Changed := True;
	NeedUpdate := True;
end;

function TSubMenu.AddItem(const ACaption: String; AAction: TAction; const AData: String): TMenuItem;
begin
	Result := TMenuItem.Create(Self, ACaption, AAction);
	Result.Data := AData;
	if (AAction = actShowPage) and (AData.IsEmpty) then
		Result.Data := ACaption.Replace(Dots, '');
end;

function TSubMenu.AddSeparator: TMenuItem;
begin
	Result := TMenuItem.Create(Self, '');
end;

function TSubMenu.AddCheckItem(const ACaption: String; const ValuePtr: PBoolean): TMenuItem;
begin
	Result := AddItem(ACaption);
	Result.Flags.IsCheckbox := True;
	Result.ValuePtr := ValuePtr;
end;

procedure TSubMenu.SetScrollPos(i: Integer);
var
	P: Integer;
	Item: TMenuItem;
begin
	P := Max(i, 0);
	if (P + Menubar.MaxItemCount) >= Items.Count then
		P := Max(0, Items.Count - Menubar.MaxItemCount);
	if P <> ScrollPos then
	begin
		for Item in Items do
			if Item <> nil then
			begin
				Item.DrawnState := misUndrawn;
				Item.OnScreen := False;
			end;
		ScrollPos := P;
		NeedUpdate := True;
		Drawn := False;
	end;
end;

procedure TSubMenu.SelectionChanged;
var
	i: Integer;
begin
	if ActiveItem = nil then Exit;

	i := Items.IndexOf(ActiveItem);
	if i < 0 then Exit;

	if i < ScrollPos then
		SetScrollPos(i)
	else
	if i >= (ScrollPos + Menubar.MaxItemCount) then
		SetScrollPos(i - Menubar.MaxItemCount + 1);
end;

procedure TSubMenu.ActivateItem(Item: TMenuItem);
begin
	if Visible then
		Activate(True);
	ActiveItem := Item;

	SelectionChanged;
	NeedUpdate := True;
end;

procedure TSubMenu.NextItem;
var
	i: Integer;
begin
	if Visible then
		Activate(True);
	Sticky := True;

	if Items.Count > 1 then
	repeat
		i := Items.IndexOf(ActiveItem) + 1;
		if i >= Items.Count then i := 0;
		if (i >= 0) and (i < Items.Count) then
			ActiveItem := Items[i]
		else
			ActiveItem := nil;
	until (ActiveItem = nil) or (not ActiveItem.Flags.IsSeparator);

	SelectionChanged;
	NeedUpdate := True;
end;

procedure TSubMenu.PreviousItem;
var
	i: Integer;
begin
	if Visible then
		Activate(True);
	Sticky := True;

	if Items.Count > 1 then
	repeat
		i := Items.IndexOf(ActiveItem) - 1;
		if i < 0 then i := Items.Count-1;
		if (i >= 0) and (i < Items.Count) then
			ActiveItem := Items[i]
		else
			ActiveItem := nil;
	until (ActiveItem = nil) or (not ActiveItem.Flags.IsSeparator);

	SelectionChanged;
	NeedUpdate := True;
end;

procedure TSubMenu.Activate(Show: Boolean);
var
	Item: TMenuItem;
begin
	// close other visible submenus of the same level
	if (Show) and (Parent <> nil) then
		for Item in Parent.Items do
			if Item.SubMenu <> Self then
				Item.ActivateSubMenu(False, Sticky);

	if Show <> Visible then
	begin
		if Show then
		begin
			if FromKeyboard then
				ActiveItem := Items.First
			else
				ActiveItem := nil;

			Menubar.ActiveMenu := Self;
			Menubar.Active := True;
		end
		else
		begin
			for Item in Items do
				Item.ActivateSubMenu(False, Sticky);
			Menubar.ActiveMenu := Parent;
		end;

		if Show then
			SelectionChanged;
	end;

	if Self <> Menubar.RootMenu then
		Visible := Show;

	NeedUpdate := True;
	Menubar.Changed := True;
end;

function TSubMenu.OnMouseMove(P: TPoint): Boolean;
var
	Item: TMenuItem;
	WasOpen: Boolean;
begin
	Result := Menubar.ActiveMenu <> Menubar.RootMenu;
	for Item in Items do
	begin
		if (Item.OnScreen) and (PtInRect(Item.Rect, P)) then
		begin
			if Item <> ActiveItem then
			begin
				WasOpen := False;
				if ActiveItem <> nil then
				begin
					WasOpen := ActiveItem.SubmenuActive;
					if WasOpen then
						ActiveItem.ActivateSubMenu(False, Sticky);
				end;
				Activate(True);
				ActiveItem := Item;
				if (WasOpen) or (Self <> Menubar.RootMenu) then
					Item.ActivateSubMenu(True, False); // open submenu
				Menubar.Changed := True;
				NeedUpdate := True;
			end;
			Result := True;
			Exit;
		end;
	end;
end;

function TSubMenu.OnMouseWheel(WheelDelta: Integer): Boolean;
begin
	Result := Scrollable;
	if Result then
	begin
		SetScrollPos(ScrollPos - (WheelDelta*3));
		Menubar.Changed := True;
	end;
end;


//==================================================================================================
// TMenuBar
//==================================================================================================

constructor TMenuBar.Create(const ABuffer: TBitmap32; const AFont: TRendererFont);
begin
	inherited Create;

	Menubar := Self;
	Visible := True;

	Buffer := ABuffer;
	Font := AFont;

	// get metrics in pixels according to current font size
	with Metrics do
	begin
		ROOTSPACING := GetX(SUBMENU_ROOTSPACING);
		ITEMSPACING := GetY(SUBMENU_ITEMSPACING);
		SEPHEIGHT   := GetY(SUBMENU_SEPHEIGHT);
		PADDING_X   := GetX(SUBMENU_PADDING_X);
		PADDING_Y   := GetY(SUBMENU_PADDING_Y);
	end;

	Items := TObjectList<TSubMenu>.Create(True);

	RootMenu := TSubMenu.Create(nil, 0);
	RootMenu.Visible := True;
	RootMenu.Caption := 'Root';

	ActiveMenu := RootMenu;

	Buffer.Clear(0);

	MaxItemCount := (Buffer.Height - RootMenu.GetHeight - (Metrics.PADDING_Y * 2)) div
		(Font.GlyphHeight + Metrics.ITEMSPACING) - 1;
end;

destructor TMenuBar.Destroy;
begin
	Items.Free;

	inherited Destroy;
end;

function TMenuBar.GetX(V: Single): Integer;
begin
	Result := Round(Font.GlyphWidth * V);
end;

function TMenuBar.GetY(V: Single): Integer;
begin
	Result := Round(Font.GlyphHeight * V);
end;

function TMenuBar.GetMenuAt(P: TPoint): TSubMenu;
var
	Menu: TSubMenu;
	i: Integer;
begin
	Result := nil;
	if Items.Count > 0 then
	for i := Items.Count-1 downto 0 do // iterate starting from the topmost
	begin
		Menu := Items[i];
		if (Menu.Visible) and (PtInRect(Menu.Rect, MousePos)) then
			Exit(Menu);
	end;
end;

procedure TMenuBar.ForceUpdate;
begin
	if ActiveMenu <> nil then
	begin
		ActiveMenu.Drawn := False;
		ActiveMenu.NeedUpdate := True;
	end;
	Changed := True;
end;

procedure TMenuBar.ForceRedraw;
begin
	ForceUpdate;
	Draw;
end;

procedure TMenuBar.Draw;

	procedure Deselect(var Menu: TSubMenu);
	begin
		// if menu has been interacted with the keyboard don't
		// deselect current item when mouse leaves menu
		if not Menu.Sticky then
		begin
			if Menu.ActiveItem <> nil then
			begin
				Menu.ActiveItem.ActivateSubMenu(False, False);
				Menu.ActiveItem := nil;
			end;
			Menu.NeedUpdate := True;
			Changed := True;
		end;
	end;

var
	Menu: TSubMenu;
begin
	if MouseMoved then
	begin
		Menu := GetMenuAt(MousePos);
		Hovering := (Menu <> nil);
		if Hovering then
		begin
			if not Menu.OnMouseMove(Types.Point(MousePos.X-Menu.Position.X, MousePos.Y-Menu.Position.Y)) then
				Deselect(Menu);
		end
		else
		if (ActiveMenu <> nil) and (ActiveMenu.ActiveItem <> nil) then
		begin
			if not ActiveMenu.ActiveItem.SubmenuActive then
				Deselect(ActiveMenu);
		end;
		RefreshActiveState;
		MouseMoved := False;
	end;

	if (Visible) and (Changed) then
	begin
		Buffer.Clear(0);

		if Active then
			for Menu in Items do
				Menu.DrawTo(Buffer)
		else
			RootMenu.DrawTo(Buffer);
		Changed := False;
	end;
end;

procedure TMenuBar.SetActive(Value: Boolean);
var
	Menu: TSubMenu;
begin
	if FActive <> Value then
	begin
		FActive := Value;

		for Menu in Items do
			Menu.Activate(False);
		RootMenu.Visible := True;

		if Value then
		begin
			RootMenu.ActiveItem := RootMenu.Items.First;
			ActiveMenu := RootMenu;
			RootMenu.Changed;
		end
		else
		begin
			if ActiveMenu <> nil then
			begin
				ActiveMenu.Activate(False);
				ActiveMenu := nil;
			end;
			RootMenu.ActiveItem := nil;
		end;

		Draw;
	end;
end;

function TMenuBar.ProcessKey(Key: Integer; Shift: TShiftState; Pressed, Repeated: Boolean): Boolean;
var
	GotItem: Boolean;
	Item: TMenuItem;
begin
	Result := Pressed;
	if (not Result) or (ActiveMenu = nil) then Exit;

	Item := ActiveMenu.ActiveItem;
	GotItem := (Item <> nil);

	FromKeyboard := True;

	if ActiveMenu = RootMenu then
	case Key of
		SDLK_UP,
		SDLK_DOWN:	if GotItem then Item.ActivateSubMenu(True, True); // open submenu
		SDLK_LEFT:	ActiveMenu.PreviousItem;
		SDLK_RIGHT:	ActiveMenu.NextItem;
		else		Result := False;
	end
	else
	case Key of

		SDLK_HOME:	ActiveMenu.ActivateItem(ActiveMenu.Items.First);
		SDLK_END:	ActiveMenu.ActivateItem(ActiveMenu.Items.Last);

		SDLK_UP:	ActiveMenu.PreviousItem;
		SDLK_DOWN:	ActiveMenu.NextItem;

		SDLK_LEFT:
		begin
			ActiveMenu.Activate(False); // close submenu
			if ActiveMenu = RootMenu then
			begin
				RootMenu.PreviousItem;
				RootMenu.ActiveItem.ActivateSubMenu(True, True);
			end;
		end;

		SDLK_RIGHT:
		if GotItem then
		begin
			if not Item.ActivateSubMenu(True, True) then // open submenu
			if ActiveMenu.Parent = RootMenu then
			begin
				ActiveMenu.Activate(False);
				RootMenu.NextItem;
				RootMenu.ActiveItem.ActivateSubMenu(True, True);
			end;
		end;

		SDLK_RETURN:
			if (GotItem) and (Item.Click) then
				Active := False;

		else
			Result := False;
	end;

	FromKeyboard := False;
end;

procedure TMenuBar.OnMouseButton(Button: Basement.Window.TMouseButton; Pressed: Boolean);
var
	Menu: TSubMenu;
	Item: TMenuItem;
begin
	if (Pressed) and (Button = mbLeft) then
	begin
		Menu := GetMenuAt(MousePos);
		if Menu <> nil then
		begin
			Item := Menu.ActiveItem;
			if Item <> nil then
				if Item.Click then
					Active := False;
		end
		else
			Active := False;
		Changed := True;
	end;
end;

procedure TMenuBar.OnMouseMove(P: TPoint);
begin
	// just memorize mouse movement, we don't want to redraw multiple times per frame
	MousePos := P;
	MouseMoved := True;
end;

function TMenuBar.RefreshActiveState: Boolean;
begin
	Result := (Hovering) or ((RootMenu <> nil) and (RootMenu.ActiveItem <> nil));
	Active := Result;
end;


end.

