unit Form.DynamicPage;

{$mode Delphi}

interface

uses
	Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, CheckLst,
	GR32, GR32_Image, Graphics32,
	Generics.Collections, MenuHandler;

type
	TControlInfo = class
		MenuEntry:  TMenuEntry;
		InfoLabel,
		ValueLabel: TLabel;
	end;

	TControlInfos = TObjectList<TControlInfo>;

	TFormPage = class(TForm)
		procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
	private
	public
		procedure StartUpdate;
		procedure EndUpdate;

		procedure ControlValueModified(Sender: TObject);
	end;

	procedure ShowPageAsForm(Page: TMenuPage);

var
	FormPage: TFormPage;
	ControlInfos: TControlInfos;

	function AddControlFromEntry(const Item: TMenuEntry; Owner: TWinControl): TControl;


implementation

uses
	Canines.Main, MainWindow,
	ConfigurationManager;

{$R *.lfm}

type
	TRowType = ( rtNormal, rtFull, rtLeft, rtRight );

const
	PaddingX = 8;
	PaddingY = 4;
	LeftColumnWidth = 150;
	ValueLabelWidth = 50;


procedure SetWidth(Ctrl, Owner: TControl);
begin
	Ctrl.Width := Owner.Width - (PaddingX * 2);
	Ctrl.Anchors := [akTop, akLeft, akRight];
end;

procedure FinishControl(Ctrl: TControl; Owner: TWinControl; const Item: TMenuEntry; HorizPos: TRowType);
var
	Lbl: TLabel = nil;
	CI: TControlInfo;
begin
	if HorizPos = rtRight then
	begin
		Lbl := TLabel.Create(Owner);
		FinishControl(Lbl, Owner, Item, rtLeft);
	end;

	Ctrl.Parent := Owner;

	Ctrl.Caption := Item.Caption;

	Ctrl.Top := Owner.Tag + PaddingX;

	if HorizPos = rtRight then
	begin
		Ctrl.Left := PaddingX + LeftColumnWidth;
		SetWidth(Ctrl, Owner);
		Ctrl.Width := Ctrl.Width - LeftColumnWidth;
	end
	else
	begin
		Ctrl.Left := PaddingX;
		if HorizPos = rtFull then
			SetWidth(Ctrl, Owner);
	end;

	if HorizPos <> rtLeft then
	begin
		Owner.Tag := Owner.Tag + Ctrl.Height + PaddingY;

		CI := TControlInfo.Create;
		CI.MenuEntry := Item;
		CI.InfoLabel := Lbl;
		Ctrl.Tag := PtrInt(CI);

		if Lbl <> nil then
		begin
			Lbl.Caption := Lbl.Caption + ':';
			if Ctrl is TComboBox then
				Lbl.Top := Lbl.Top + 4;
		end;

		if Ctrl is TCheckBox then
		begin
			Ctrl.Top := Ctrl.Top - 2;
			Owner.Tag := Owner.Tag - 2;
		end;

		ControlInfos.Add(CI);
	end;
end;

function NeedsCombobox(const Item: TMenuEntry): Boolean;
var
	L: Integer;
	I: TConfigItem;
begin
	I := Item.ConfigItem;
	L := Length(I.ValueNames);
	Result := L > 0;
	if (Result) and (L = 2) then
	begin
		if I.GetValueName(0) = 'No' then Result := False;
	end;
	if Result then
	begin
		if I.Section = 'Emulation' then
			Result := I.Name = 'ConsoleModel';
	end;
end;

procedure TFormPage.ControlValueModified(Sender: TObject);
var
	Ctrl: TControl;
	CtrlInfo: TControlInfo;
	CI: TConfigItem;
	Lbl: TLabel;
begin
	Ctrl := Sender as TControl;
	if Ctrl.Tag = 0 then Exit;

	CtrlInfo := TControlInfo(PtrInt(Ctrl.Tag));
	if CtrlInfo = nil then Exit;
	CI := CtrlInfo.MenuEntry.ConfigItem;

	if Ctrl is TScrollBar then
	begin
		if CI = nil then Exit;
		if (CI is TConfigItemFloat) then
		begin
			TConfigItemFloat(CI).Value^ := (TScrollBar(Ctrl).Position) / 100;
			CI.CallCallback;
		end
		else
			CI.SetValue(TScrollBar(Ctrl).Position);
	end
	else
	if Ctrl is TCheckBox then
	begin
		if CI = nil then Exit;
		CI.SetValue(TCheckBox(Ctrl).Checked.ToInteger);
	end
	else
	if Ctrl is TComboBox then
	begin
		if CI = nil then Exit;
		CI.SetValue(TComboBox(Ctrl).ItemIndex);
	end
	else
	if Ctrl is TButton then
	begin
		if CtrlInfo.MenuEntry.Action <> 0 then
			Window.OnMenuAction(CtrlInfo.MenuEntry)
		else
			Window.ShowMenuPage(CtrlInfo.MenuEntry.Caption);
	end;

	if CI <> nil then
	begin
		Lbl := CtrlInfo.ValueLabel;
		if Lbl <> nil then
			Lbl.Caption := CI.ValueToString;
	end;

	if Assigned(CtrlInfo.MenuEntry.Callback) then
		CtrlInfo.MenuEntry.Callback(CtrlInfo.MenuEntry);

//	CaniNES_ProcessFrame;
end;

function AddControl_Label(const Item: TMenuEntry; Owner: TWinControl): TLabel;
begin
	Result := TLabel.Create(Owner);

	if Item.Kind = meHeader then
	begin
		Owner.Tag := Owner.Tag + 3;
		Result.AutoSize := False;
		Result.Color := clGray;
		Result.Font.Color := clWhite;
		Result.Height := Result.Height + 5;
		Result.Layout := tlCenter;
		FinishControl(Result, Owner, Item, rtFull);
		Result.Caption := ' ' + Result.Caption;
	end
	else
		FinishControl(Result, Owner, Item, rtNormal);
end;

function AddControl_Button(const Item: TMenuEntry; Owner: TWinControl): TButton;
begin
	if Item.Caption = 'OK' then Exit(nil);

	Result := TButton.Create(Owner);
	Result.Height := Result.Height + 2;
	if Item.Kind = mePage then
		FinishControl(Result, Owner, Item, rtFull)
	else
		FinishControl(Result, Owner, Item, rtNormal);
	Result.OnClick := FormPage.ControlValueModified;
end;

function AddControl_ComboBox(const Item: TMenuEntry; Owner: TWinControl): TComboBox;
begin
	if (Item.ConfigItem = nil) then Exit(nil);

	Result := TComboBox.Create(Owner);
	Result.Style := csDropDownList;
	Result.Items.AddStrings(Item.ConfigItem.ValueNames);

	FinishControl(Result, Owner, Item, rtRight);

	if Item.ConfigItem is TConfigItemByte then
		Result.ItemIndex := TConfigItemByte(Item.ConfigItem).GetValue // - Item.ConfigItem.Min;
	else
	if Item.ConfigItem is TConfigItemBoolean then
		Result.ItemIndex := Integer(TConfigItemBoolean(Item.ConfigItem).GetValue);

	Result.OnChange := FormPage.ControlValueModified;
end;

function AddControl_CheckBox(const Item: TMenuEntry; Owner: TWinControl): TCheckBox;
begin
	if (Item.ConfigItem = nil) or (not (Item.ConfigItem is TConfigItemBoolean)) then Exit(nil);
	if NeedsCombobox(Item) then
	begin
		AddControl_ComboBox(Item, Owner);
		Exit(nil);
	end;

	Result := TCheckBox.Create(Owner);
	Result.Checked := TConfigItemBoolean(Item.ConfigItem).GetValue;
	FinishControl(Result, Owner, Item, rtNormal);

	Result.OnChange := FormPage.ControlValueModified;
end;

function AddControl_Image(const Item: TMenuEntry; Owner: TWinControl): TImage32;
var
	ImgD: GR32.TBitmap32;
	ImgS: Graphics32.TBitmap32;
	DP: GR32.PColor32;
	SP: Graphics32.PColor32;
	i: Integer;
begin
	Result := TImage32.Create(Owner);

	ImgD := Result.Bitmap;
	ImgS := Item.Image;

	Result.Width  := ImgS.Width;
	Result.Height := ImgS.Height;
	ImgD.SetSize(ImgS.Width, ImgS.Height);

	DP := ImgD.PixelPtr[0,0];
	SP := ImgS.PixelPtr[0,0];

	for i := 0 to ImgS.Width*ImgS.Height-1 do
	begin
		DP^ := SP^;
		Inc(DP); Inc(SP);
	end;

	FinishControl(Result, Owner, Item, rtNormal);
end;

function AddControl_ScrollBar(const Item: TMenuEntry; Owner: TWinControl): TScrollBar;
var
	Lbl: TLabel;
begin
	if (Item.ConfigItem = nil) then Exit(nil);
	if NeedsCombobox(Item) then
	begin
		AddControl_ComboBox(Item, Owner);
		Exit(nil);
	end;

	Result := TScrollBar.Create(Owner);
	Result.Max := Item.ConfigItem.Max;
	Result.Min := Item.ConfigItem.Min;

	if Item.ConfigItem is TConfigItemByte then
		Result.Position := TConfigItemByte(Item.ConfigItem).GetValue
	else
	if Item.ConfigItem is TConfigItemWord then
		Result.Position := TConfigItemWord(Item.ConfigItem).GetValue
	else
	if Item.ConfigItem is TConfigItemInteger then
		Result.Position := TConfigItemInteger(Item.ConfigItem).GetValue
	else
	if Item.ConfigItem is TConfigItemFloat then
	begin
		Result.Max := Trunc(TConfigItemFloat(Item.ConfigItem).FMax * 100);
		Result.Min := Trunc(TConfigItemFloat(Item.ConfigItem).FMin * 100);
		Result.Position := Trunc(TConfigItemFloat(Item.ConfigItem).GetValue * 100);
	end;

	FinishControl(Result, Owner, Item, rtRight);

	Result.Width := Result.Width - ValueLabelWidth - 4;

	if Result.Tag <> 0 then
	begin
		Lbl := TLabel.Create(Owner);
		Lbl.Parent := Owner;
		Lbl.AutoSize := False;
		Lbl.Top := ControlInfos.Last.InfoLabel.Top;
		Lbl.Left := Result.Left + Result.Width + 6;
		Lbl.Width := Owner.Width - Lbl.Left;
		Lbl.Anchors := [akTop, akRight];
		TControlInfo(PtrInt(Result.Tag)).ValueLabel := Lbl;
		FormPage.ControlValueModified(Result);
	end;

	Result.OnChange := FormPage.ControlValueModified;
end;

function AddControl_Edit(const Item: TMenuEntry; Owner: TWinControl): TEdit;
begin
	if (Item.ConfigItem = nil) or (not (Item.ConfigItem is TConfigItemString)) then Exit(nil);

	Result := TEdit.Create(Owner);
	Result.Text := TConfigItemString(Item.ConfigItem).GetValue;
	FinishControl(Result, Owner, Item, rtRight);
end;

function AddControlFromEntry(const Item: TMenuEntry; Owner: TWinControl): TControl;
begin
	Result := nil;

	case Item.Kind of

		meText,
		meHeader:
			Result := AddControl_Label(Item, Owner);

		meCommand,
		mePage:
			Result := AddControl_Button(Item, Owner);

		meSetting:
			if Item.ConfigItem <> nil then
			begin
				if Item.ConfigItem is TConfigItemBoolean then
					Result := AddControl_CheckBox(Item, Owner)
				else
				if Item.ConfigItem is TConfigItemString then
					Result := AddControl_Edit(Item, Owner)
				else
					Result := AddControl_ScrollBar(Item, Owner);
			end;

		meImage:
			Result := AddControl_Image(Item, Owner);

	end;
end;

procedure ShowPageAsForm(Page: TMenuPage);
var
	Item: TMenuEntry;
begin
	if Page = nil then Exit;

	ControlInfos := TControlInfos.Create(True);

	FreeAndNil(FormPage);
	FormPage := TFormPage.Create(nil);
	FormPage.StartUpdate;
	FormPage.Caption := Page.HeaderCaption;
	FormPage.Tag := -3;

	for Item in Page.Items do
		AddControlFromEntry(Item, FormPage);

	FormPage.ClientHeight := FormPage.Tag + (PaddingX * 2);
	FormPage.EndUpdate;
{
	FormPage.ShowModal;

	ControlInfos.Free;
	FormPage.Free;
}
	FormPage.Show;
end;

{ TFormPage }

procedure TFormPage.StartUpdate;
begin
	BeginFormUpdate;
end;

procedure TFormPage.EndUpdate;
begin
	EndFormUpdate;
end;

procedure TFormPage.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
	ControlInfos.Free;
end;

finalization

	FreeAndNil(FormPage);

end.

