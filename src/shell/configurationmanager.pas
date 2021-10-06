unit ConfigurationManager;

interface

uses
	Classes, Types, Generics.Collections, IniFiles;

const
	CN_YESNO: array[0..1] of AnsiString = ('No', 'Yes');

type
	TConfigItem = class;

	TSettingChangeCallback = procedure(Item: TConfigItem) of Object;

	TConfigItem = class
	public
		Section,
		Name,
		Caption,
		FormatString:	AnsiString;
		ValueNames: 	array of AnsiString;
		Min, Max:		Int64;
		Step,
		LargeStep:		Integer;
		ID:				Integer;
		Callback:		TSettingChangeCallback;

		procedure 		SetInfo(const aCaption: AnsiString; aID : Integer = 0); overload;
		procedure 		SetInfo(const aCaption: AnsiString; aID, aMin, aMax: Integer;
						const aValueNames: array of AnsiString;
						const aCallback: TSettingChangeCallback = nil;
						const aFormatString: AnsiString = '';
						aStep: Integer = 1; aLargeStep: Integer = 5); overload;
		procedure 		SetInfo(const aCaption: AnsiString; aID, aMin, aMax: Integer;
						const aValueNames: TStrings;
						const aCallback: TSettingChangeCallback = nil;
						const aFormatString: AnsiString = '';
						aStep: Integer = 1; aLargeStep: Integer = 5); overload;
		procedure 		SetInfoFromDir(const aCaption: AnsiString; aID: Integer;
						const Dir, Extensions: String;
						const aCallback: TSettingChangeCallback = nil);
		procedure		ModifyValue(Amount: Integer); virtual;
		procedure		SetValue(NewValue: Integer); virtual;
		function		ValueToString: AnsiString; virtual;
		function 		GetValueName(i: Integer): AnsiString;
		function		ListValues(const sl: TStrings): Integer; virtual;
		procedure 		CallCallback;

		procedure		Load(const Ini: TIniFile); virtual;
		procedure		Save(const Ini: TIniFile); virtual;

		constructor		Create(const ItemSection, ItemName: String); overload;
		destructor		Destroy; override;
	end;

	TConfigItemBoolean = class(TConfigItem)
	public
		Value:			PBoolean;
		DefaultValue:	Boolean;
		procedure		ModifyValue(Amount: Integer); override;
		procedure		SetValue(NewValue: Integer); override;
		function		GetValue: Boolean;
		function		ValueToString: AnsiString; override;
		function		ListValues(const sl: TStrings): Integer; override;
		procedure		Load(const Ini: TIniFile); override;
		procedure		Save(const Ini: TIniFile); override;
	end;

	TConfigItemByte = class(TConfigItem)
	public
		Value:			PByte;
		DefaultValue:	Byte;
		procedure		ModifyValue(Amount: Integer); override;
		procedure		SetValue(NewValue: Integer); override;
		function		GetValue: Byte;
		function		ValueToString: AnsiString; override;
		function		ListValues(const sl: TStrings): Integer; override;
		procedure		Load(const Ini: TIniFile); override;
		procedure		Save(const Ini: TIniFile); override;
	end;

	TConfigItemWord = class(TConfigItem)
	public
		Value:			PWord;
		DefaultValue:	Word;
		procedure		ModifyValue(Amount: Integer); override;
		procedure		SetValue(NewValue: Integer); override;
		function		GetValue: Word;
		function		ValueToString: AnsiString; override;
		function		ListValues(const sl: TStrings): Integer; override;
		procedure		Load(const Ini: TIniFile); override;
		procedure		Save(const Ini: TIniFile); override;
	end;

	TConfigItemInteger = class(TConfigItem)
	public
		Value:			PInteger;
		DefaultValue:	Integer;
		procedure		ModifyValue(Amount: Integer); override;
		procedure		SetValue(NewValue: Integer); override;
		function		GetValue: Integer;
		function		ValueToString: AnsiString; override;
		function		ListValues(const sl: TStrings): Integer; override;
		procedure		Load(const Ini: TIniFile); override;
		procedure		Save(const Ini: TIniFile); override;
	end;

	TConfigItemCardinal = class(TConfigItem)
	public
		Value:			PCardinal;
		DefaultValue:	Cardinal;
		procedure		ModifyValue(Amount: Integer); override;
		function		GetValue: Cardinal;
		function		ValueToString: AnsiString; override;
		procedure		Load(const Ini: TIniFile); override;
		procedure		Save(const Ini: TIniFile); override;
	end;

	TConfigItemFloat = class(TConfigItem)
	public
		Value:			PDouble;
		DefaultValue,
		FStep,
		FMin, FMax:		Double;

		procedure 		SetInfo(const aCaption: AnsiString; aID: Integer;
						aMin, aMax, aStep: Double); overload;

		procedure		ModifyValue(Amount: Integer); override;
		function		GetValue: Double;
		function		ValueToString: AnsiString; override;
		procedure		Load(const Ini: TIniFile); override;
		procedure		Save(const Ini: TIniFile); override;
	end;

	TConfigItemString = class(TConfigItem)
	public
		CurrentIndex:	Integer;
		Value:			PString;
		DefaultValue:	String;
		AllowEmpty:		Boolean;
		procedure		ModifyValue(Amount: Integer); override;
		procedure		SetValue(NewValue: Integer); override;
		function		GetValue: String;
		function		ValueToString: AnsiString; override;
		function		ListValues(const sl: TStrings): Integer; override;
		procedure		Load(const Ini: TIniFile); override;
		procedure		Save(const Ini: TIniFile); override;

		constructor		Create(const ItemSection, ItemName: String); overload;
	end;

	TConfigItemList = TObjectList<TConfigItem>;

	TConfigurationManager = class
		Items:		TConfigItemList;
		Filename:	String;

		constructor	Create;
		destructor	Destroy; override;

		function	AddBoolean	(const Section, Name: AnsiString; const Value: PBoolean;
								 const DefaultValue: Boolean = False): TConfigItemBoolean;
		function	AddByte		(const Section, Name: AnsiString; const Value: PByte;
								 const DefaultValue: Byte = 0): TConfigItemByte;
		function	AddWord		(const Section, Name: AnsiString; const Value: PWord;
								 const DefaultValue: Word = 0): TConfigItemWord;
		function	AddInteger	(const Section, Name: AnsiString; const Value: PInteger;
								 const DefaultValue: Integer = 0): TConfigItemInteger;
		function	AddCardinal	(const Section, Name: AnsiString; const Value: PCardinal;
								 const DefaultValue: Cardinal = 0): TConfigItemCardinal;
		function 	AddFloat	(const Section, Name: AnsiString; const Value: PDouble;
								 const DefaultValue: Double = 0): TConfigItemFloat;
		function 	AddString	(const Section, Name: AnsiString; const Value: PString;
								 const DefaultValue: AnsiString = '';
								 AllowEmpty: Boolean = True): TConfigItemString;

		function	Find(const Section, Name: AnsiString): TConfigItem;

		function	OpenINI: TIniFile;
		function	Load: Boolean;
		procedure	Save;
	end;

var
	DefaultConfigItemCallback: TSettingChangeCallback;

implementation

{$R-}

uses
	Math, SysUtils, FileUtils;

{ TConfigurationManager }

constructor TConfigurationManager.Create;
begin
	Filename := '';
	Items := TConfigItemList.Create(True);
end;

destructor TConfigurationManager.Destroy;
begin
	Items.Free;
	inherited Destroy;
end;

function TConfigurationManager.OpenINI: TIniFile;
begin
	Result := TIniFile.Create(Filename);
end;

function TConfigurationManager.Load: Boolean;
var
	Ini: TIniFile;
	Item: TConfigItem;
begin
	if (Filename = '') or (not FileExists(Filename)) then Exit(False);
	if Items.Count <= 0 then Exit(False);	// nothing to load

	Ini := OpenINI;
	if Assigned(Ini) then
	begin
		try
			for Item in Items do
				Item.Load(Ini);
		finally
			Ini.Free;
		end;
		Result := True;
	end
	else
		Result := False;
end;

procedure TConfigurationManager.Save;
var
	Ini: TIniFile;
	Item: TConfigItem;
begin
	if Filename = '' then Exit;
	if Items.Count <= 0 then Exit;	// nothing to save

	Ini := OpenINI;
	if Assigned(Ini) then
	try
		for Item in Items do
			Item.Save(Ini);
	finally
		Ini.Free;
	end;
end;

function TConfigurationManager.AddBoolean(const Section, Name: AnsiString; const Value: PBoolean;
	const DefaultValue: Boolean): TConfigItemBoolean;
begin
	Result := TConfigItemBoolean.Create(Section, Name);
	Result.Value := Value;
	PBoolean(Result.Value)^ := DefaultValue;
	Result.DefaultValue := DefaultValue;
	Items.Add(Result);
end;

function TConfigurationManager.AddByte(const Section, Name: AnsiString; const Value: PByte;
	const DefaultValue: Byte): TConfigItemByte;
begin
	Result := TConfigItemByte.Create(Section, Name);
	Result.Value := Value;
	PByte(Result.Value)^ := DefaultValue;
	Result.DefaultValue := DefaultValue;
	Result.Max := $FF;
	Items.Add(Result);
end;

function TConfigurationManager.AddWord(const Section, Name: AnsiString; const Value: PWord;
	const DefaultValue: Word): TConfigItemWord;
begin
	Result := TConfigItemWord.Create(Section, Name);
	Result.Value := Value;
	PWord(Result.Value)^ := DefaultValue;
	Result.DefaultValue := DefaultValue;
	Result.Max := $FFFF;
	Items.Add(Result);
end;

function TConfigurationManager.AddInteger(const Section, Name: AnsiString; const Value: PInteger;
	const DefaultValue: Integer): TConfigItemInteger;
begin
	Result := TConfigItemInteger.Create(Section, Name);
	Result.Value := Value;
	PInteger(Result.Value)^ := DefaultValue;
	Result.DefaultValue := DefaultValue;
	Items.Add(Result);
end;

function TConfigurationManager.AddCardinal(const Section, Name: AnsiString; const Value: PCardinal;
	const DefaultValue: Cardinal): TConfigItemCardinal;
begin
	Result := TConfigItemCardinal.Create(Section, Name);
	Result.Value := Value;
	Result.Min := 0;
	Result.Max := $FFFFFFFF;
	PCardinal(Result.Value)^ := DefaultValue;
	Result.DefaultValue := DefaultValue;
	Items.Add(Result);
end;

function TConfigurationManager.AddFloat(const Section, Name: AnsiString; const Value: PDouble;
	const DefaultValue: Double): TConfigItemFloat;
begin
	Result := TConfigItemFloat.Create(Section, Name);
	Result.Value := Value;
	PDouble(Result.Value)^ := DefaultValue;
	Result.DefaultValue := DefaultValue;
	Items.Add(Result);
end;

function TConfigurationManager.AddString(const Section, Name: AnsiString; const Value: PString;
	const DefaultValue: AnsiString; AllowEmpty: Boolean): TConfigItemString;
begin
	Result := TConfigItemString.Create(Section, Name);
	Result.Value := Value;
	PString(Result.Value)^ := DefaultValue;
	Result.DefaultValue := DefaultValue;
	Result.AllowEmpty := AllowEmpty;
	Items.Add(Result);
end;

function TConfigurationManager.Find(const Section, Name: AnsiString): TConfigItem;
var
	Item: TConfigItem;
begin
	for Item in Items do
		if (Item.Section = Section) and (Item.Name = Name) then
			Exit(Item);
	Result := nil;
end;

{ TConfigItem }

constructor TConfigItem.Create(const ItemSection, ItemName: String);
begin
	inherited Create;

	Section := ItemSection;
	Name := ItemName;
	Caption := '';
	Min := 0;
	Max := 0;
	Callback := nil;
end;

destructor TConfigItem.Destroy;
begin
	inherited Destroy;
end;

procedure TConfigItem.SetInfo(const aCaption: AnsiString; aID: Integer = 0);
begin
	if (Self is TConfigItemBoolean) then
		SetInfo(aCaption, aID, Min, Max, CN_YESNO)
	else
		SetInfo(aCaption, aID, Min, Max, []);
end;

procedure TConfigItem.SetInfo(const aCaption: AnsiString; aID, aMin, aMax: Integer;
	const aValueNames: array of AnsiString;
	const aCallback: TSettingChangeCallback; const aFormatString: AnsiString;
	aStep: Integer; aLargeStep: Integer);
var
	i, os: Integer;
begin
	ID := aID;
	if aCaption = '' then
		Caption := Name
	else
		Caption := aCaption;
	FormatString := aFormatString;
	SetLength(ValueNames, Length(aValueNames));
	os := Low(aValueNames);
	for i := Low(aValueNames) to High(aValueNames) do
		ValueNames[i-os] := aValueNames[i];
	Min := aMin;
	Max := aMax;
	Step := aStep;
	LargeStep := aLargeStep;
	Callback := aCallback;
end;

procedure TConfigItem.SetInfo(const aCaption: AnsiString; aID, aMin, aMax: Integer;
	const aValueNames: TStrings;
	const aCallback: TSettingChangeCallback; const aFormatString: AnsiString;
	aStep: Integer; aLargeStep: Integer);
var
	i: Integer;
	NameArr: array of AnsiString;
begin
	SetLength(NameArr{%H-}, aValueNames.Count);
	for i := 0 to aValueNames.Count-1 do
		NameArr[i] := aValueNames[i];
	SetInfo(aCaption, aID, aMin, aMax, NameArr,
		aCallback, aFormatString, aStep, aLargeStep);
end;

procedure TConfigItem.SetInfoFromDir(const aCaption: AnsiString; aID: Integer;
	const Dir, Extensions: String;
	const aCallback: TSettingChangeCallback = nil);
var
	Filelist: array of AnsiString;
	Sl: TStringList;
	i: Integer;
begin
	Sl := TStringList.Create;
	FileSearch(Dir, Extensions, Sl);
	Sl.Sort;
	SetLength(Filelist{%H-}, Sl.Count);
	for i := 0 to Sl.Count-1 do
		Filelist[i] := ChangeFileExt(ExtractFilename(Sl[i]), '');

	SetInfo(aCaption, aID, 0, Sl.Count-1, Filelist, aCallback);

	if (Self is TConfigItemString) then
	with (Self as TConfigItemString) do
	begin
		for i := 0 to Sl.Count-1 do
			if FileList[i] = Value^ then
			begin
				CurrentIndex := i;
				Break;
			end;
	end;

	Sl.Free;
end;

procedure TConfigItem.Load(const Ini: TIniFile);
begin
end;

procedure TConfigItem.Save(const Ini: TIniFile);
begin
end;

function TConfigItem.ValueToString: AnsiString;
begin
	Result := '';
end;

function TConfigItem.GetValueName(i: Integer): AnsiString; inline;
begin
	if i < Low(ValueNames)  then i := Low(ValueNames)
	else
	if i > High(ValueNames) then i := High(ValueNames);
	Result := ValueNames[i];
end;

function TConfigItem.ListValues(const sl: TStrings): Integer;
begin
	Result := -1;
end;

procedure TConfigItem.CallCallback;
begin
	if Assigned(Callback) then
		Callback(Self)
	else
	if Assigned(DefaultConfigItemCallback) then
		DefaultConfigItemCallback(Self);
end;

procedure TConfigItem.ModifyValue(Amount: Integer);
begin
	CallCallback;
end;

procedure TConfigItem.SetValue(NewValue: Integer);
begin
	if Assigned(Callback) then
		Callback(Self)
	else
	if Assigned(DefaultConfigItemCallback) then
		DefaultConfigItemCallback(Self);
end;


{ TConfigItemString }

constructor TConfigItemString.Create(const ItemSection, ItemName: String);
begin
	inherited Create(ItemSection, ItemName);
	CurrentIndex := 0;
end;

procedure TConfigItemString.Load(const Ini: TIniFile);
var
	S: String;
	i: Integer;
begin
	S := Ini.ReadString(Section, Name, PString(Value)^);
	if (AllowEmpty) or (S <> '') then
		PString(Value)^ := S;
	for i := 0 to High(ValueNames) do
		if ValueNames[i] = S then
		begin
			CurrentIndex := i;
			Break;
		end;
end;

procedure TConfigItemString.Save(const Ini: TIniFile);
begin
	Ini.WriteString(Section, Name, PString(Value)^);
end;

function TConfigItemString.GetValue: String;
begin
	Result := Value^;
end;

procedure TConfigItemString.ModifyValue(Amount: Integer);
var
	V: Integer;
begin
	V := CurrentIndex + Amount;
	if V < Min then
		V := Min
	else
	if V > Max then
		V := Max;
	Value^ := Self.ValueNames[V];
	CurrentIndex := V;
	inherited;
end;

procedure TConfigItemString.SetValue(NewValue: Integer);
begin
	if (NewValue >= Low(ValueNames)) and (NewValue <= High(ValueNames)) then
	begin
		Value^ := Self.ValueNames[NewValue];
		CurrentIndex := NewValue;
		inherited;
	end;
end;

function TConfigItemString.ValueToString: AnsiString;
begin
	Result := Value^;
end;

function TConfigItemString.ListValues(const sl: TStrings): Integer;
begin
	if not Assigned(sl) then Exit(-1);
	sl.AddStrings(ValueNames);
	Result := CurrentIndex;
end;

{ TConfigItemFloat }

procedure TConfigItemFloat.Load(const Ini: TIniFile);
var
	Val: Double;
begin
	Val := Ini.ReadFloat(Section, Name, PDouble(Value)^);
//	if (Val < Min) or (Val > Max) then Val := DefaultValue;
	PDouble(Value)^ := Val;
end;

procedure TConfigItemFloat.Save(const Ini: TIniFile);
begin
//	Ini.WriteFloat(Section, Name, PDouble(Value)^);
	Ini.WriteString(Section, Name, ValueToString);
end;

function TConfigItemFloat.GetValue: Double;
begin
	Result := Value^;
end;

procedure TConfigItemFloat.SetInfo(const aCaption: AnsiString;
	aID: Integer; aMin, aMax, aStep: Double);
begin
	if aCaption = '' then
		Caption := Name
	else
		Caption := aCaption;
	ID := aID;
	FMin := aMin;
	FMax := aMax;
	FStep := aStep;
	Step := 1;
end;

procedure TConfigItemFloat.ModifyValue(Amount: Integer);
var
	V: Double;
begin
	V := Value^ + (Amount * FStep);
	if V < FMin then
		V := FMin
	else
	if V > FMax then
		V := FMax;
	Value^ := V;

	inherited;
end;

{function TConfigItemFloat.ValueToString(V: Single): AnsiString;
begin
	if (V < Min) or (V > Max) then Exit('');

	if FormatString <> '' then
		Result := Format(FormatString, [V])
	else
		Result := Format('%f', [Max - V]);

	if (Value^ = Min) and (Length(ValueNames) > 0) then
		Result := ValueNames[0]
	else
	if (Value^ = Max) and (High(ValueNames) >= 1) then
		Result := ValueNames[1];
end;}

function TConfigItemFloat.ValueToString: AnsiString;
begin
//	Result := ValueToString(Value^);

	if FormatString <> '' then
		Result := Format(FormatString, [Value^])
	else
		Result := Format('%f', [{Max - }Value^]);

	if (Value^ = Min) and (Length(ValueNames) > 0) then
		Result := ValueNames[0]
	else
	if (Value^ = Max) and (High(ValueNames) >= 1) then
		Result := ValueNames[1];
end;

{ TConfigItemCardinal }

procedure TConfigItemCardinal.Load(const Ini: TIniFile);
var
	S: String;
	Val: Cardinal;
begin
	S := Ini.ReadString(Section, Name, '');
	if S <> '' then
	begin
		Val := Cardinal(StrToInt(S));
		if (Val < Min) or (Val > Max) then
			Val := DefaultValue;
	end
	else
		Val := DefaultValue;
	PCardinal(Value)^ := Val;
end;

procedure TConfigItemCardinal.Save(const Ini: TIniFile);
begin
	Ini.WriteString(Section, Name, '$' + IntToHex(PCardinal(Value)^, 8));
end;

function TConfigItemCardinal.GetValue: Cardinal;
begin
	Result := Value^;
end;

procedure TConfigItemCardinal.ModifyValue(Amount: Integer);
var
	V: Cardinal;
begin
	V := Value^ {%H-}+ Amount;
	if V < Min then
		V := Min
	else
	if V > Max then
		V := Max;
	Value^ := V;
	inherited;
end;

function TConfigItemCardinal.ValueToString: AnsiString;
begin
	if Value = nil then Exit('NIL');

	if FormatString <> '' then
		Result := Format(FormatString, [Value^])
	else
		Result := IntToStr(Value^);

	if (Value^ = Min) and (Length(ValueNames) > 0) then
		Result := ValueNames[0]
	else
	if (Value^ = Max) and (High(ValueNames) >= 1) then
		Result := ValueNames[1];
end;

{ TConfigItemInteger }

procedure TConfigItemInteger.Load(const Ini: TIniFile);
var
	i: Integer;
begin
	i := Ini.ReadInteger(Section, Name, PInteger(Value)^);
	if (i < Min) or (i > Max) then
		i := DefaultValue;
	PInteger(Value)^ := i;
end;

procedure TConfigItemInteger.Save(const Ini: TIniFile);
begin
	Ini.WriteInteger(Section, Name, PInteger(Value)^);
end;

function TConfigItemInteger.GetValue: Integer;
begin
	Result := Value^;
end;

procedure TConfigItemInteger.ModifyValue(Amount: Integer);
var
	V: Integer;
begin
	V := Value^ + Amount;
	if V < Min then
		V := Min
	else
	if V > Max then
		V := Max;
	Value^ := V;
	inherited;
end;

procedure TConfigItemInteger.SetValue(NewValue: Integer);
begin
	if (NewValue >= Min) and (NewValue <= Max) then
	begin
		Value^ := NewValue;
		inherited;
	end;
end;

function TConfigItemInteger.ValueToString: AnsiString;
begin
	if Value = nil then Exit('NIL');

	if FormatString <> '' then
		Result := Format(FormatString, [Value^])
	else
		Result := IntToStr(Value^);

	if (Value^ = Min) and (Length(ValueNames) > 0) then
		Result := ValueNames[0]
	else
	if (Value^ = Max) and (High(ValueNames) >= 1) then
		Result := ValueNames[1];
end;

function TConfigItemInteger.ListValues(const sl: TStrings): Integer;
begin
	if not Assigned(sl) then Exit(-1);
	Result := 0; // todo
end;

{ TConfigItemWord }

procedure TConfigItemWord.Load(const Ini: TIniFile);
var
	i: Integer;
begin
	i := Ini.ReadInteger(Section, Name, PWord(Value)^);
	if (i < Min) or (i > Max) then
		i := DefaultValue;
	PWord(Value)^ := i;
end;

procedure TConfigItemWord.Save(const Ini: TIniFile);
begin
	Ini.WriteInteger(Section, Name, PWord(Value)^);
end;

function TConfigItemWord.GetValue: Word;
begin
	Result := Value^;
end;

procedure TConfigItemWord.ModifyValue(Amount: Integer);
var
	V: Integer;
begin
	V := Value^ + Amount;
	if V < Min then
		V := Min
	else
	if V > Max then
		V := Max;
	Value^ := V;
	inherited;
end;

procedure TConfigItemWord.SetValue(NewValue: Integer);
begin
	if (NewValue >= 0) and (NewValue >= Min) and (NewValue <= Max) then
	begin
		Value^ := NewValue;
		inherited;
	end;
end;

function TConfigItemWord.ValueToString: AnsiString;
begin
	if Value = nil then Exit('NIL');

	if FormatString <> '' then
		Result := Format(FormatString, [Value^])
	else
		Result := IntToStr(Value^);

	if (Value^ = Min) and (Length(ValueNames) > 0) then
		Result := ValueNames[0]
	else
	if (Value^ = Max) and (High(ValueNames) >= 1) then
		Result := ValueNames[1];
end;

function TConfigItemWord.ListValues(const sl: TStrings): Integer;
begin
	if not Assigned(sl) then Exit(-1);
	Result := 0; // todo
end;

{ TConfigItemByte }

procedure TConfigItemByte.Load(const Ini: TIniFile);
var
	i: Integer;
begin
	i := Ini.ReadInteger(Section, Name, PByte(Value)^);
	if (i < Min) or (i > Max) then
		i := DefaultValue;
	if (i >= 0) and (i <= 255) then
		PByte(Value)^ := i;
end;

procedure TConfigItemByte.Save(const Ini: TIniFile);
begin
	Ini.WriteInteger(Section, Name, PByte(Value)^);
end;

function TConfigItemByte.GetValue: Byte;
begin
	Result := Value^;
end;

procedure TConfigItemByte.ModifyValue(Amount: Integer);
var
	V: Integer;
begin
	V := PByte(Value)^ + Amount;
	if V < Min then
		V := Min
	else
	if V > Max then
		V := Max;
	if PByte(Value)^ <> Byte(V) then
	begin
		PByte(Value)^ := Byte(V);
		inherited;
	end;
end;

procedure TConfigItemByte.SetValue(NewValue: Integer);
begin
	if (NewValue >= Min) and (NewValue <= Max) then
	begin
		PByte(Value)^ := Byte(NewValue);
		inherited;
	end;
end;

function TConfigItemByte.ValueToString: AnsiString;
begin
	if Value = nil then Exit('NIL');

	if (Max - Min) <= Length(ValueNames) then
		Result := GetValueName(Value^-Min)
	else
	if (Value^ = Min) and (Length(ValueNames) > 0) then
		Result := GetValueName(Min)
	else
	if (Value^ = Max) and (High(ValueNames) >= 1) then
		Result := GetValueName(1)
	else
	if FormatString <> '' then
		Result := Format(FormatString, [Value^])
	else
		Result := IntToStr(Value^);
end;

function TConfigItemByte.ListValues(const sl: TStrings): Integer;
var
	i: Integer;
begin
	if not Assigned(sl) then Exit(-1);

	for i := Min to Max do
	begin
		if (Max - Min) <= Length(ValueNames) then
			sl.Add(GetValueName(i-Min))
		else
		if (i = Min) and (Length(ValueNames) > 0) then
			sl.Add(GetValueName(Min))
		else
		if (i = Max) and (High(ValueNames) >= 1) then
			sl.Add(GetValueName(1))
		else
		if FormatString <> '' then
			sl.Add(Format(FormatString, [i]))
		else
			sl.Add(IntToStr(i));
	end;
	Result := Value^ - Min;
end;

{ TConfigItemBoolean }

procedure TConfigItemBoolean.Load(const Ini: TIniFile);
begin
	PBoolean(Value)^ := Ini.ReadBool(Section, Name, PBoolean(Value)^);
end;

procedure TConfigItemBoolean.Save(const Ini: TIniFile);
begin
	Ini.WriteBool(Section, Name, PBoolean(Value)^);
end;

function TConfigItemBoolean.GetValue: Boolean;
begin
	Result := Value^;
end;

procedure TConfigItemBoolean.ModifyValue(Amount: Integer);
var
	B: Boolean;
begin
	B := (Amount > 0);
	if B <> Value^ then
	begin
		Value^ := B;
		inherited;
	end;
end;

procedure TConfigItemBoolean.SetValue(NewValue: Integer);
var
	B: Boolean;
begin
	B := (NewValue > 0);
	if B <> Value^ then
	begin
		Value^ := B;
		inherited;
	end;
end;

function TConfigItemBoolean.ValueToString: AnsiString;
begin
	if Value^ = True then
	begin
		if High(ValueNames) >= 1 then
			Result := ValueNames[1]
		else
			Result := 'True';
	end
	else
	begin
		if Length(ValueNames) > 0 then
			Result := ValueNames[0]
		else
			Result := 'False';
	end;
end;

function TConfigItemBoolean.ListValues(const sl: TStrings): Integer;
begin
	if not Assigned(sl) then Exit(-1);

	sl.Add(ValueNames[0]);
	sl.Add(ValueNames[1]);
	case Value^ of
		False: Result := 0;
		True:  Result := 1;
	end;
end;

initialization

	DefaultConfigItemCallback := nil;

end.

