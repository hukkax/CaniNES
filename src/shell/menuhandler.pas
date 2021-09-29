unit MenuHandler;

{$MODE DELPHI}

interface

uses
	Types, Classes, SysUtils, Generics.Collections,
	ConfigurationManager, InputBindings,
	Basement.Window,
	LazUTF8, Graphics32;

const
	SearchTimeout = 1000;

type
	TMenuEntry = class;

	TMenuEntryKind = ( meText, meHeader, meCommand, mePage, meSetting, meImage );

	TMenuEntryCallback = procedure(Entry: TMenuEntry) of Object;

	TMenuEntry = class
	public
		Kind:       TMenuEntryKind;
		Caption,
		Data:       AnsiString;
		IsFile,
		IsFavourited,
		IsLoaded,
		Checkbox,
		Checked:    Boolean;
		Action:     Integer;
		Height:     Byte;
		Color:      Cardinal;
		ConfigItem: TConfigItem;
		Callback:   TMenuEntryCallback;

		Position:   TPoint; // images only
		Area:       TRect;
		Image:      TBitmap32;

		constructor Create(EntryKind: TMenuEntryKind; const ACaption: AnsiString;
			AData: AnsiString = '');
	end;

	TMenuPage = class;
	OnEnterPageEvent = procedure(Page: TMenuPage);

	TMenuPage = class
	private
		ParentPage: TMenuPage;
		ItemIndex:  Integer;

		function Add(Item: TMenuEntry): TMenuEntry;
	public
		Title, HeaderCaption: AnsiString;
		Width:       Word;
		ItemHeight:  Byte;
		FullScreen:  Boolean;

		TotalHeight: Cardinal;
		RowsVisible,
		LastVisibleItem,
		Offset:      Word;
		Area:        TRect;

		Items: TObjectList<TMenuEntry>;

		OnEnter: OnEnterPageEvent;

		function  AddHeader   (const Title: AnsiString; Level: Byte = 0): TMenuEntry;
		function  AddText(const Title: AnsiString): TMenuEntry;
		function  AddEntry    (const Title: AnsiString; Action: Integer;
			Color: Cardinal = $FFFFFFFF;
			Callback: TMenuEntryCallback = nil): TMenuEntry;
		function  AddFile(const Filename: AnsiString; StripExtension: Boolean;
			Color: Cardinal = $FFFFFFFF; Callback: TMenuEntryCallback = nil): TMenuEntry;
		function  AddPageEntry(const Title: AnsiString): TMenuEntry;
		function  AddSetting  (const Section, Name: AnsiString): TMenuEntry;
		function  AddSettings (const Section: AnsiString): Boolean;
		function  AddSettingsItems(Section, SearchString: AnsiString): Boolean;
		function  AddCommand(const Name: String; Cmd: TAction): TMenuEntry;
		function  AddBackCommand: TMenuEntry;

		procedure CmdOpenFile(Entry: TMenuEntry);

		procedure BrowseItems(Change: Integer; AllowWrap: Boolean = True; Center: Boolean = True);
		procedure ScrollToView;
		procedure Scroll(Delta: Integer);

		constructor Create(const ATitle, AHeaderCaption: AnsiString);
		destructor  Destroy; override;
	end;

	TMenuPageClass = class
	protected
		Page: TMenuPage;
	public
		constructor Create; virtual;
		destructor  Destroy; override;
	end;

	TEmulatorMenu = class
	private
		Pages: TObjectList<TMenuPage>;
		PageClasses: TObjectList<TMenuPageClass>;

		NeedUpdate:    Boolean;
		SkipNextKeyUp: Boolean;
		MousePos:      TPoint;

		SearchInfo: record
			Timestamp: Cardinal;
			Query:     String;
		end;

		Scrollbar: record
			Enabled,
			Dragging:  Boolean;
			Rect,
			ThumbRect: TRect;
		end;

		procedure UpdateScrollbar;

	public
		Visible:      Boolean;
		Recreated:    Boolean;
		ShowAsWindow: Boolean;
		Width:        Word;
		ItemHeight:   Byte;

		WaitingForBinding: PInputBinding;

		BindingsPage: TMenuPage;

		CurrentPage:   TMenuPage;
		CurrentEntry:  TMenuEntry;

		function  AddPage(const Title: AnsiString; HeaderCaption: AnsiString;
			ParentPage: TMenuPage = nil): TMenuPage;

		procedure AddPageClass(PageClass: TMenuPageClass);
		procedure RemovePageClass(PageClass: TMenuPageClass);

		procedure WaitForBinding(B: PInputBinding);

		procedure CmdBackPage(Entry: TMenuEntry);

		procedure Changed;
		procedure Show(Enable: Boolean = True);

		function  Draw(ForceRedraw: Boolean = False): Boolean;

		function  ProcessAction(Action: TAction; Pressed: Boolean): Boolean;
		function  ProcessKey(Key: Integer; Shift: TShiftState; Pressed, Repeated: Boolean): Boolean;
		function  ProcessJoy(Pressed: Boolean; PadNum: Integer; Button: Byte): Boolean;
		function  SearchKey(Key: Char): Boolean;

		procedure OnBindingComplete;
		procedure OnMouseButton(Button: Basement.Window.TMouseButton; Pressed: Boolean);
		procedure OnMouseMove(P: TPoint);

		function  SelectEntry(AltSelect: Boolean = False): Boolean;
		procedure ModifyEntry(Increase: Boolean = True);
		procedure BrowseItems(Change: Integer; AllowWrap: Boolean = True; Center: Boolean = True);
		function  GetItemIndex: Integer;
		function  SetItemIndex(i: Integer; Center: Boolean = True): Boolean;
		function  ItemIndexFromPixelPos(P: TPoint): Integer;

		function  FindPage(PageName: AnsiString): TMenuPage;
		procedure SwitchPage(Page: TMenuPage); overload;
		procedure SwitchPage(PageName: AnsiString); overload;
		procedure ShowPage(PageName: AnsiString);

		procedure UpdateFilelisting;

		constructor Create;
		destructor  Destroy; override;
	end;

var
	Menu: TEmulatorMenu;
	ShiftDown: Boolean;


implementation

uses
	Basement.Util, Math, StrUtils, TextOutput,
	Graphics32.Extra,
	MainWindow, MainMenu, Basement.Renderer.Overlay,
	NES.Config, NES.Types, NES.Console, BookmarkManager,
	SDL2;

// ==========================================================================
// TMenuPageClass
// ==========================================================================

constructor TMenuPageClass.Create;
begin
	inherited Create;

	Menu.AddPageClass(Self);
end;

destructor TMenuPageClass.Destroy;
begin
	Menu.RemovePageClass(Self);

	inherited Destroy;
end;

// ==========================================================================
// TMenuEntry
// ==========================================================================

constructor TMenuEntry.Create(EntryKind: TMenuEntryKind; const ACaption: AnsiString;
	AData: AnsiString = '');
begin
	inherited Create;

	Kind := EntryKind;
	Caption := ACaption;
	Data := AData;

	Checkbox := False;
	Checked := False;

	ConfigItem := nil;
end;

// ==========================================================================
// TMenuPage
// ==========================================================================

constructor TMenuPage.Create(const ATitle, AHeaderCaption: AnsiString);
begin
	inherited Create;

	Title := ATitle;
	if AHeaderCaption = '' then
		HeaderCaption := ATitle
	else
		HeaderCaption := AHeaderCaption;

	Items := TObjectList<TMenuEntry>.Create(True);

	OnEnter := nil;
	ParentPage := nil;
	ItemIndex := 0;
	FullScreen := False;
end;

destructor TMenuPage.Destroy;
begin
	Items.Free;

	inherited Destroy;
end;

procedure TMenuPage.CmdOpenFile(Entry: TMenuEntry);
begin
	Console.LoadROM(Entry.Data);
end;

function TMenuPage.Add(Item: TMenuEntry): TMenuEntry;
begin
	if Item <> nil then Items.Add(Item);
	Result := Item;
	Item.Color := Palette[COLOR_MENU_NORMAL];
	if (ItemIndex < 0) and not (Item.Kind in [meHeader, meText]) then
		ItemIndex := Items.Count-1;
	Result.Height := ItemHeight;
end;

function TMenuPage.AddHeader(const Title: AnsiString; Level: Byte = 0): TMenuEntry;
begin
	if Title <> '' then
		Result := Add(TMenuEntry.Create(meHeader, Title, IntToStr(Level)))
	else
		Exit(nil);

	Inc(Result.Height, 2);
	Result.Color := Palette[COLOR_MENU_HEADING];
end;

function TMenuPage.AddText(const Title: AnsiString): TMenuEntry;
begin
	if Title <> '' then
		Result := Add(TMenuEntry.Create(meText, Title))
	else
		Result := nil;
	Inc(Result.Height, 2);
	Result.Color := Palette[COLOR_MENU_HEADING];
end;

function TMenuPage.AddEntry(const Title: AnsiString; Action: Integer;
	Color: Cardinal; Callback: TMenuEntryCallback): TMenuEntry;
begin
	Result := Add(TMenuEntry.Create(meCommand, Title));
	Result.Action := Action;
	Result.Color := Color;
	if Assigned(Callback) then
		Result.Callback := Callback;
end;

function TMenuPage.AddFile(const Filename: AnsiString; StripExtension: Boolean;
	Color: Cardinal; Callback: TMenuEntryCallback): TMenuEntry;
var
	S: String;
begin
	S := FixChars(ExtractFileName(Filename));
	if StripExtension then
		S := ChangeFileExt(S, '');

	Result := Add(TMenuEntry.Create(meCommand, S));
	Result.Data := Filename;
	Result.Color := Color;
	Result.IsFile := True;
	if Assigned(Callback) then
		Result.Callback := Callback;
end;

function TMenuPage.AddPageEntry(const Title: AnsiString): TMenuEntry;
begin
	Result := Add(TMenuEntry.Create(mePage, Title)); // pagename?
end;

function TMenuPage.AddSetting(const Section, Name: AnsiString): TMenuEntry;
var
	Item: TConfigItem;
begin
	Result := Add(TMenuEntry.Create(meSetting, Name, Section));
	if Result <> nil then
	begin
		Item := ConfigManager.Find(Section, Name);
		if Item <> nil then
		begin
			Result.ConfigItem := Item;
			if not Item.Caption.IsEmpty then
				Result.Caption := Item.Caption;
		end;
	end;
end;

function TMenuPage.AddSettings(const Section: AnsiString): Boolean;
var
	Item: TConfigItem;
begin
	Result := False;

	for Item in ConfigManager.Items do
	begin
		if (Item.Caption.IsEmpty) or (Item.Caption.StartsWith('!')) then Continue;
		if Item.Section = Section then
		begin
			AddSetting(Item.Section, Item.Name);
			Result := True;
		end;
	end;
end;

function TMenuPage.AddSettingsItems(Section, SearchString: AnsiString): Boolean;
var
	Item: TConfigItem;
begin
	Result := False;

	if not (SearchString.Contains('*')) and not (SearchString.Contains('?')) then
		SearchString := SearchString + '*';

	for Item in ConfigManager.Items do
	begin
		if (Item.Caption.IsEmpty) or (Item.Caption.StartsWith('!')) then Continue;
		if (Item.Section = Section) and (IsWild(Item.Name, SearchString, True)) then
		begin
			AddSetting(Item.Section, Item.Name);
			Result := True;
		end;
	end;
end;

function TMenuPage.AddCommand(const Name: String; Cmd: TAction): TMenuEntry;
begin
	Result := AddEntry(Name, Ord(Cmd), Palette[COLOR_MENU_NORMAL], Window.OnMenuAction);
end;

function TMenuPage.AddBackCommand: TMenuEntry;
begin
	Result := nil;
	if (ParentPage <> nil) and (Menu <> nil) then
		if (ParentPage <> MainMenu.MainPage) or (not Menu.ShowAsWindow) then
			Result := AddEntry(STR_PAGE_PREV, 0, Palette[COLOR_MENU_BACKBUTTON], Menu.CmdBackPage);
end;

procedure TMenuPage.BrowseItems(Change: Integer; AllowWrap: Boolean = True; Center: Boolean = True);

	function ValidEntry(const E: TMenuEntry): Boolean;
	begin
		Result := not (E.Kind in [meHeader, meText, meImage]);
	end;

var
	E: TMenuEntry;
	B: Boolean;
begin
	if Items.Count < 1 then Exit;

	B := False;
	for E in Items do
		if ValidEntry(E) then
		begin
			B := True;
			Break;
		end;
	if not B then Exit;

	Inc(ItemIndex, Change);
	if ItemIndex < 0 then
		ItemIndex := IfThen(AllowWrap, Items.Count-1, 0)
	else
	if ItemIndex >= Items.Count then
		ItemIndex := IfThen(AllowWrap, 0, Items.Count-1);

	if not ValidEntry(Items[ItemIndex]) then
	begin
		if AllowWrap then
			BrowseItems(IfThen(Change < 0, -1, +1))
		else
			BrowseItems(IfThen(Change < 0, +1, -1));
	end;

	if Center then
		ScrollToView;
end;

procedure TMenuPage.Scroll(Delta: Integer);
var
	O: Integer;
begin
	if Delta = 0 then Exit;

	if FullScreen then
	begin
		O := Max(0, Offset + Delta);
		Offset := Min(O, Max(0, Items.Count-RowsVisible));
	end
	else
	begin
		Menu.ModifyEntry(Delta < 0);
	end;
	Menu.NeedUpdate := True;
end;

procedure TMenuPage.ScrollToView;
var
	ClientHeight: Integer;
begin
	ClientHeight := MenuRenderer.FrameBuffer.Height - (MenuRenderer.Font.GlyphHeight + 2);
	RowsVisible := Round(ClientHeight / MenuRenderer.Font.GlyphHeight);

	if (not FullScreen) or (ItemIndex < 0) then
	begin
		Offset := 0;
		Exit;
	end;

	if TotalHeight <= ClientHeight then Exit;

	Offset := Max(0, ItemIndex - Integer(RowsVisible div 2));
	if (Offset + RowsVisible) > Items.Count then
		Offset := Items.Count - RowsVisible;

	if ItemIndex < Offset then
		Offset := ItemIndex
	else
	if ItemIndex >= (Offset + RowsVisible) then
		Offset := ItemIndex - RowsVisible + 1;
end;

// ==========================================================================
// TEmulatorMenu
// ==========================================================================

constructor TEmulatorMenu.Create;
begin
	inherited Create;

	Visible := False;
	Width := FontManager.Font.GlyphWidth * 40;
	ItemHeight := FontManager.Font.GlyphHeight;// + 3;

	CurrentPage := nil;

	Pages := TObjectList<TMenuPage>.Create(True);
	PageClasses := TObjectList<TMenuPageClass>.Create(True);

	BindingsPage := nil;
	WaitingForBinding := nil;

	Recreated := True;
	NeedUpdate := True;
end;

destructor TEmulatorMenu.Destroy;
begin
	Pages.Free;
	PageClasses.Free;

	inherited Destroy;
end;

function TEmulatorMenu.AddPage(const Title: AnsiString;
	HeaderCaption: AnsiString;
	ParentPage: TMenuPage = nil): TMenuPage;
begin
	Result := TMenuPage.Create(Title, HeaderCaption);
	if Result <> nil then
	begin
		Result.ParentPage := ParentPage;
		Result.Width := Width;
		Result.ItemHeight := ItemHeight;
		if ParentPage <> nil then
		begin
			Result.AddBackCommand;
			Result.AddHeader(Title);
		end;
		Pages.Add(Result);
	end;
end;

procedure TEmulatorMenu.AddPageClass(PageClass: TMenuPageClass);
begin
	PageClasses.Add(PageClass);
end;

procedure TEmulatorMenu.RemovePageClass(PageClass: TMenuPageClass);
begin
	if PageClasses.Contains(PageClass) then
		PageClasses.Delete(PageClasses.IndexOf(PageClass));
end;

procedure TEmulatorMenu.WaitForBinding(B: PInputBinding);
begin
	WaitingForBinding := B;
	SkipNextKeyUp := True;
end;

procedure TEmulatorMenu.Changed;
begin
	SwitchPage(Pages.First);
	CurrentEntry := CurrentPage.Items.First;
	CurrentPage.ItemIndex := 0;
	BrowseItems(0);
	NeedUpdate := True;
end;

procedure TEmulatorMenu.Show(Enable: Boolean);
begin
	Visible := Enable;
	if Enable then
		NeedUpdate := True;

	Window.ControllerSetupChanged; // show/hide mouse pointer

	MenuRenderer.Opacity := 1;
	MenuRenderer.FrameBuffer.Clear(0);
end;

function TEmulatorMenu.Draw(ForceRedraw: Boolean): Boolean;
var
	Buffer: TBitmap32;
	OL: TOverlayRenderer;

	i, HY, Y, X1, X2, Y1, Y2, W, GW, GH, OSY, IX, IY: Integer;
	R, SR: TRect;
	Item: TMenuEntry;
	C: TColor32;
	S: String;

	{procedure DrawSectionOutline; inline;
	begin
		if (HY > 0) and (not CurrentPage.FullScreen) then
			OL.FrameBuffer.FrameRect(X1-1, HY, X2+2, Y+GH-1, $FFFF0000); //COLOR_HEADER);
	end;}

begin
	Result := Visible;
	if ForceRedraw then NeedUpdate := True;

	if (not Visible) or (not NeedUpdate) or (CurrentPage = nil) then Exit;

	OL := MenuRenderer;
	if OL = nil then Exit;

	Buffer := OL.FrameBuffer;

	W := Buffer.Width;
	GW := OL.Font.GlyphWidth;
	GH := OL.Font.GlyphHeight;

	if not CurrentPage.FullScreen then
	begin
		Buffer.Clear(0);

		Buffer.FillBox(Rect(0, 0, W-1, GH+3), Palette[COLOR_MENU_HEADER], 0, -1);
		OL.Font.DrawStringCentered(Buffer, 2, CurrentPage.HeaderCaption, Palette[COLOR_MENU_HEADING]);

		X1 := (W - CurrentPage.Width) div 2;
		X2 := W - X1 - 1;
		Y1 := (Buffer.Height - (CurrentPage.Items.Count * GH)) div 2;
		Y := Y1;
		OSY := -3;
		R.TopLeft := Point(X1-2, Y-3);
	end
	else
	begin
		Buffer.Clear(Palette[COLOR_MENU_BACKGROUND]);
		CurrentPage.Area := Buffer.BoundsRect;

		Y := Buffer.Height - (CurrentPage.RowsVisible * GH);
		Buffer.FillBox(Rect(0, 0, W-1, Y), Palette[COLOR_MENU_HEADER], 0, -1);
		OL.Font.DrawStringCentered(Buffer, 0, CurrentPage.HeaderCaption, Palette[COLOR_MENU_HEADING]);

		X1 := 0;
		X2 := Buffer.Width;
		Y1 := -1;
		OSY := -3;
		Inc(Y, 2);
	end;

	HY := 0;
	W := X2 - X1;

	CurrentPage.LastVisibleItem := CurrentPage.Items.Count-1;

	for i := CurrentPage.Offset to CurrentPage.Items.Count-1 do
	begin
		Item := CurrentPage.Items[i];
		GH := OL.Font.GlyphHeight; //Item.Height;

		if Item.Kind = meHeader then
		begin
			HY := Y - 2;
			Item.Area := Rect(X1-1, HY, X2, Y+GH-1);
			Buffer.FillBox(Item.Area, Palette[COLOR_MENU_HEADER], 40, -1);
			OL.Font.DrawStringCentered(Buffer, Max(0, Y)-2, Item.Caption, Item.Color);
		end
		else
		if Item.Kind = meImage then
		begin
			if (Item.Image = nil) or (Item.Image.Height <= 1) then Continue;

			IX := Item.Position.X;
			IY := Item.Position.Y;

			if IX = 0 then
				IX := (Buffer.Width - Item.Image.Width) div 2
			else
			if IX < 0 then
				IX := Buffer.Width - Item.Image.Width - 1 - Abs(IX);

			if IY = 0 then
				IY := (Buffer.Height - Item.Image.Height) div 2
			else
			if IY < 0 then
				IY := Buffer.Height - Item.Image.Height - 1 - Abs(IY);

			SR := Bounds(IX, IY, Item.Image.Width, Item.Image.Height);
			Buffer.Draw(SR.Left, SR.Top, Item.Image);

			for HY := 0 to 1 do
			begin
				SR.Inflate(1, 1);
				if Item = CurrentEntry then
					Buffer.FrameRect(SR, Palette[COLOR_MENU_SELECTION])
				else
					Buffer.FrameRect(SR, Palette[COLOR_MENU_SHADOW]);
			end;

			Buffer.DropShadow(SR, Palette[COLOR_MENU_SHADOW], 3);
			//Inc(Y, Item.Image.Height + 4);
			Dec(Y, GH);
		end
		else
		begin
			//{%H-}DrawSectionOutline;

			SR := Rect(X1, Y-2, X2, Y+GH-2);

			Item.Area := SR;

			if Item = CurrentEntry then
				Buffer.DrawSelection(SR, Palette[COLOR_MENU_SELECTION])
			else
			if not CurrentPage.FullScreen then
				Buffer.FillRectS(SR, Palette[COLOR_MENU_BACKGROUND]);

			if (Item.Kind = meSetting) and (Item.ConfigItem <> nil) then
			begin
				OL.DrawString(-X1-2, -(Y+OSY),
					Item.ConfigItem.Caption + ': ', Item.Color, W);

				S := Item.ConfigItem.ValueToString;
				OL.DrawString(-(X2-2 - (Length(S) * GW)), -(Y+OSY), S, Palette[COLOR_MENU_SETTING]);
			end
			else
			begin
				if Item.Checkbox then
				begin
					if Item.Checked then
						S := STR_CHECKBOX_YES
					else
						S := STR_CHECKBOX_NO;
					OL.DrawString(-X1-2, -(Y+OSY), S + Item.Caption, Item.Color, W);
				end
				else
				begin
					C := Item.Color;
					if Item.IsFile then
					begin
						if Item.IsFavourited then
						begin
							if Item <> CurrentEntry then
								Buffer.FillBox(SR, Lighten(Palette[COLOR_FILE_FAVOURITED], -20), 50, 1);
							//OL.DrawString(-(X2-GW-2), -(Y+OSY), '❤', $FFFF4433, X2-X1);
						end;
						if Item.IsLoaded then
							C := Palette[COLOR_FILE_CURRENT];
					end;

					OL.DrawString(-X1-2, -(Y+OSY), Item.Caption, C, X2-X1);
				end;

				if Item.Kind = mePage then
				begin
					S := STR_PAGE_NEXT;
					OL.DrawString(-(X2-4-GW), -(Y+OSY), S, Item.Color, W);
				end;
			end;
		end;

		Inc(Y, GH);
		if Y >= Buffer.Height then
		begin
			CurrentPage.LastVisibleItem := i;
			Break;
		end;
	end;

	if Y1 >= 0 then
	begin
		R := Rect(X1-1, Y1-3, X2+1, Y+{GH}3-4);
		CurrentPage.Area := R;
		Buffer.FrameRect(R, Palette[COLOR_MENU_HEADER]);
		R.Inflate(1, 1);
		Buffer.FrameRect(R, Palette[COLOR_MENU_BACKGROUND]);
		Buffer.DropShadow(R, Palette[COLOR_MENU_SHADOW], 3);
	end;

	// Scrollbar
	//
	if (CurrentPage.FullScreen) and (CurrentPage.Items.Count > CurrentPage.RowsVisible) then
	begin
		Scrollbar.Enabled := True;

		W := (Buffer.Width div GW) - 1;
		Y1 := Buffer.Height - (CurrentPage.RowsVisible * GH);
		Y2 := Buffer.Height;
		X2 := Buffer.Width;
		X1 := W * GW;
		W := X2 - X1;
		HY := Y2 - Y1;
		R := Rect(X1, Y1, X2, Y2);

		Scrollbar.Rect := R;
		Buffer.FillRectS(R, Palette[COLOR_SCROLLBAR_BG]);
		Buffer.VertLineS(X1, Y1, Y2, Palette[COLOR_MENU_HEADER]);

		// thumbsize = scrollbar size * pagesize / scrollbar range

		if CurrentPage.Offset > 0 then
			R.Top := Trunc( (CurrentPage.Offset / CurrentPage.Items.Count) * HY) + Y1
		else
			R.Top := Y1;
		R.Bottom := Round( ((CurrentPage.LastVisibleItem+1) / CurrentPage.Items.Count) * HY) + Y1;
		R.Left  := X1 + 2;
		R.Right := X2 - 1;

		Scrollbar.ThumbRect := R;
		Buffer.FillRectS(R, Palette[COLOR_SCROLLBAR_FG]);
	end
	else
		Scrollbar.Enabled := False;

	NeedUpdate := False;
end;

function TEmulatorMenu.ProcessAction(Action: TAction; Pressed: Boolean): Boolean;
begin
	Result := False;
	if (not Visible) or (not Pressed) then Exit;

	case Action of
		actMenuSelect: SelectEntry;
		actAltSelect:  SelectEntry(True);
		actMenuBack:   CmdBackPage(CurrentEntry);
		actMenuShow:   Show(False);

		actPadUp:      BrowseItems(-1);
		actPadDown:    BrowseItems(+1);
		actPadLeft:    ModifyEntry(False);
		actPadRight:   ModifyEntry(True);
	else
		Exit;
	end;

	NeedUpdate := True;
	Result := True;
end;

procedure TEmulatorMenu.OnBindingComplete;
begin
	WaitingForBinding := nil;
	SkipNextKeyUp := False;
	SwitchPage(BindingsPage);
end;

// for joypad binding configuration
function TEmulatorMenu.ProcessJoy(Pressed: Boolean; PadNum: Integer; Button: Byte): Boolean;
begin
	if (not Visible) or (not Pressed) or (WaitingForBinding = nil) then Exit(False);

	WaitingForBinding.PadNum := PadNum;
	WaitingForBinding.PadButton := Button;

	OnBindingComplete;
	Result := True;
end;

function TEmulatorMenu.SearchKey(Key: Char): Boolean;
var
	ts: Cardinal;
	i, count: Integer;
	E: TMenuEntry;
begin
	ts := SDL_GetTicks;
	if (ts - SearchInfo.Timestamp) >= SearchTimeout then
		SearchInfo.Query := Key
	else
		SearchInfo.Query := SearchInfo.Query + Key;

	count := CurrentPage.Items.Count;
	i := CurrentPage.ItemIndex;

	while count > 0 do
	begin
		Dec(count);
		Inc(i);

		if i >= CurrentPage.Items.Count then
			i := 0;
		E := CurrentPage.Items[i];

		if (E.Kind = meCommand) and
		   (E.Caption.StartsWith(SearchInfo.Query, True)) then
		begin
			SetItemIndex(i);
			count := 0;
		end;
	end;

	SearchInfo.Timestamp := ts;
	Result := True;
end;

function TEmulatorMenu.ProcessKey(Key: Integer; Shift: TShiftState;
	Pressed, Repeated: Boolean): Boolean;
begin
	Result := False;
	if (not Visible) then Exit;

	if WaitingForBinding <> nil then
	begin
		if Pressed then Exit;
		if SkipNextKeyUp then
		begin
			SkipNextKeyUp := False;
			Exit;
		end;

		if Key <> SDLK_ESCAPE then
		begin
			WaitingForBinding.Key := Key;
			WaitingForBinding.Shift := ssShift in Shift;
			FixBindingShift(WaitingForBinding);
		end;
		OnBindingComplete;
		Result := True;
	end
	else
	if (not Pressed) then
		Exit;

	case Key of

		// duplicated here to allow repeated actions via keyboard
		SDLK_UP:       BrowseItems(-1);
		SDLK_DOWN:     BrowseItems(+1);
		SDLK_LEFT:     ModifyEntry(False);
		SDLK_RIGHT:    ModifyEntry(True);

		SDLK_HOME:     SetItemIndex(0);
		SDLK_END:      SetItemIndex(MaxInt);

		SDLK_PAGEUP:   BrowseItems(-(CurrentPage.RowsVisible-1), False);
		SDLK_PAGEDOWN: BrowseItems(+(CurrentPage.RowsVisible-1), False);

	else
		if (Shift <> []) or (Key < 32) or (Key > 255) or (Key in [126, 127, 167]) then Exit;
		if not SearchKey(Chr(Key)) then Exit;
	end;

	NeedUpdate := True;
	Result := True;
end;

function TEmulatorMenu.FindPage(PageName: AnsiString): TMenuPage;
var
	P: TMenuPage;
begin
	for P in Pages do
		if P.Title = PageName then
			Exit(P);
	Result := nil;
end;

procedure TEmulatorMenu.ShowPage(PageName: AnsiString);
var
	P: TMenuPage;
begin
	P := FindPage(PageName);
	if P = nil then Exit;

	if P = CurrentPage then
		Show(not Visible)
	else
	begin
		SwitchPage(PageName);
		Show;
	end;
end;

procedure TEmulatorMenu.SwitchPage(PageName: AnsiString);
var
	P: TMenuPage;
begin
	P := FindPage(PageName);
	if P <> nil then
		SwitchPage(P)
	else
		Log('SwitchPage: no page called "' + PageName + '"');
end;

procedure TEmulatorMenu.SwitchPage(Page: TMenuPage);
var
	H: Cardinal;
	I: TMenuEntry;
begin
	if Page <> nil then
	begin
		CurrentPage := Page;
		if Assigned(Page.OnEnter) then
			Page.OnEnter(Page);

		H := 0;
		for I in Page.Items do
			Inc(H, Max(I.Height, Page.ItemHeight));
		Page.TotalHeight := H;

		Page.BrowseItems(0);
		if Page.Items.Count > 0 then
			CurrentEntry := Page.Items[Min(Page.ItemIndex, Page.Items.Count-1)]
		else
			CurrentEntry := nil;
	end;
	NeedUpdate := True;
end;

function TEmulatorMenu.SelectEntry(AltSelect: Boolean = False): Boolean;
begin
	if CurrentEntry = nil then Exit(False);

	ShiftDown := AltSelect;
	Result := True;

	case CurrentEntry.Kind of

		meCommand:
			if Assigned(CurrentEntry.Callback) then
				CurrentEntry.Callback(CurrentEntry);

		mePage:
			SwitchPage(CurrentEntry.Caption);

		meSetting: ;

	else
		Result := False;
	end;
end;

procedure TEmulatorMenu.ModifyEntry(Increase: Boolean = True);
begin
	if (CurrentEntry <> nil) and (CurrentEntry.ConfigItem <> nil) then
	begin
		if Increase then
			CurrentEntry.ConfigItem.ModifyValue(+CurrentEntry.ConfigItem.Step)
		else
			CurrentEntry.ConfigItem.ModifyValue(-CurrentEntry.ConfigItem.Step);
		if Recreated then
		begin
			Recreated := False;
			Exit;
		end;
		if Assigned(CurrentEntry.Callback) then
			CurrentEntry.Callback(CurrentEntry);

		NeedUpdate := True;
	end;
end;

procedure TEmulatorMenu.BrowseItems(Change: Integer; AllowWrap: Boolean = True; Center: Boolean = True);
begin
	if CurrentPage <> nil then
		CurrentPage.BrowseItems(Change, AllowWrap, Center);

	if (CurrentPage.Items.Count > 0) and (CurrentPage.ItemIndex >= 0) and
		(CurrentPage.ItemIndex < CurrentPage.Items.Count) then
		CurrentEntry := CurrentPage.Items[CurrentPage.ItemIndex]
	else
		CurrentEntry := nil;

	NeedUpdate := True;
end;

function TEmulatorMenu.GetItemIndex: Integer;
begin
	if CurrentPage = nil then
		Result := -1
	else
		Result := CurrentPage.ItemIndex;
end;

function TEmulatorMenu.SetItemIndex(i: Integer; Center: Boolean = True): Boolean;
begin
	Result := False;
	if (CurrentPage = nil) or (i < 0) then Exit;

	i := Max(0, Min(i, CurrentPage.Items.Count-1));

	if i <> CurrentPage.ItemIndex then
	begin
		Result := True;
		CurrentPage.ItemIndex := i;
		BrowseItems(0, True, Center);
	end;
end;

function TEmulatorMenu.ItemIndexFromPixelPos(P: TPoint): Integer;
var
	i: Integer;
	E: TMenuEntry;
begin
	if (CurrentPage <> nil) and (PtInRect(CurrentPage.Area, P)) then
	begin
		for i := CurrentPage.Offset to CurrentPage.LastVisibleItem do
		begin
			E := CurrentPage.Items[i];
			if (P.Y >= E.Area.Top) and (P.Y < E.Area.Bottom) then
				Exit(CurrentPage.Items.IndexOf(E));
		end;
	end;
	Result := -1;
end;

procedure TEmulatorMenu.UpdateScrollbar;
var
	Y, H, T, M: Integer;
begin
	if (CurrentPage = nil) or (not Scrollbar.Enabled) then Exit;

	T := Scrollbar.Rect.Top;
	H := Scrollbar.Rect.Bottom - T - 1;
	Y := Max(MousePos.Y - T, 0);
	M := CurrentPage.Items.Count - CurrentPage.RowsVisible; // Maximum offset value

	if Y > 0 then
		T := Min(Trunc((Y / H) * M), M)
	else
		T := 0;

	if T <> CurrentPage.Offset then
	begin
		CurrentPage.Offset := T;
		NeedUpdate := True;
	end;
end;

procedure TEmulatorMenu.OnMouseButton(Button: Basement.Window.TMouseButton; Pressed: Boolean);
begin
	if not Pressed then
	begin
		if Scrollbar.Dragging then
		begin
			SDL_CaptureMouse(SDL_False);
			Scrollbar.Dragging := False;
		end;
		Exit;
	end;

	if (Scrollbar.Enabled) and (PtInRect(Scrollbar.Rect, MousePos)) then
	begin
		Scrollbar.Dragging := True;
		SDL_CaptureMouse(SDL_True);
		UpdateScrollbar;
	end
	else
	if SelectEntry(Button = Basement.Window.mbMiddle) then
		NeedUpdate := True;
end;

procedure TEmulatorMenu.OnMouseMove(P: TPoint);
var
	i: Integer;
begin
	MousePos := P;

	if (Scrollbar.Enabled) and ((Scrollbar.Dragging) or (PtInRect(Scrollbar.Rect, P))) then
	begin
		if Scrollbar.Dragging then
			UpdateScrollbar;
	end
	else
	begin
		i := ItemIndexFromPixelPos(P);
		if (i >= 0) and (i <> CurrentPage.ItemIndex) and (SetItemIndex(i, False)) then
		begin
			NeedUpdate := True;
			Window.Mouse.Visible := True;
			Window.ShowMouse;
		end;
	end;
end;

procedure TEmulatorMenu.UpdateFilelisting;
var
	i: Integer;
	S: String;
	E: TMenuEntry;
begin
	if CurrentPage <> nil then
	for E in CurrentPage.Items do
	begin
		if not E.IsFile then Continue;

		E.IsFavourited := Bookmarks.Contains(E.Data);
		E.IsLoaded := E.Data = Console.LoadedFile;
	end;
end;

procedure TEmulatorMenu.CmdBackPage(Entry: TMenuEntry);
begin
	if (CurrentPage <> nil) and (CurrentPage.ParentPage <> nil) then
		SwitchPage(CurrentPage.ParentPage);
end;

initialization

	Menu := nil;

end.

