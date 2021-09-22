unit NES.Cheats;

interface

{$MODE DELPHI}

uses
	Classes, Generics.Collections;

type
	TCodeInfo = class
		Enabled:           Boolean;
		Address:           Integer;
		Value:             Byte;
		CompareValue:      Integer;
		IsRelativeAddress: Boolean;
		Description:       String;
	end;

	TCodeList = TObjectList<TCodeInfo>;

	//procedure FixIniFile(Filename: String); // temp

	TCheatManager = class
	private
		CurrentHash:        Cardinal;
		HasCode:            Boolean;
		AbsoluteCheatCodes: TCodeList;
		RelativeCheatCodes: array [Word] of TCodeList;
		Filename:           String;

		function  DecodeValue(code: Cardinal; bitIndexes: array of Cardinal; bitCount: Cardinal): Cardinal;
	public
		AvailableCodes:     TCodeList;

		procedure ApplyCodes(addr: Word; var value: Byte);
		procedure ClearCodes;
		procedure AddCheatCode(Code: TCodeInfo);

		function  AddProActionRockyCode(PARCode: AnsiString): TCodeInfo; overload;
		function  AddProActionRockyCode(PARCode: Cardinal):   TCodeInfo; overload;
		function  AddGameGenieCode(GGCode: String): TCodeInfo;

		function  GetProActionRockyInfo(PARCode: Cardinal): TCodeInfo;
		function  GetGameGenieCodeInfo(GGCode: String): TCodeInfo;

		procedure GetAvailableCheats;

		constructor Create(const AFilename: String);
		destructor  Destroy; override;
	end;



implementation

uses
	Basement.Util, SysUtils,
	NES.Config, NES.Console, NES.Mapper;

{procedure FixIniFile(Filename: String);
var
	sl, so: TStringList;
	S, sect: String;
	X, Y: Integer;
begin
	so := TStringList.Create;

	sl := TStringList.Create;
	sl.LoadFromFile(Filename);
	sect := '';

	for Y := 0 to sl.Count-1 do
	begin
		if Y >= sl.Count then Break;

		if sl[Y].StartsWith('[') then
		begin
			if sl[Y] = sect then
				sl.Delete(Y)
			else
			begin
				sect := sl[Y];

				if so.Count > 0 then
					so.Add('');
				X := sect.IndexOf(':');
				S := (Copy(sect, x+2, Length(sect)));
				S := Copy(S, 1, Length(S)-1);
				so.Add(';' + S);
				so.Add(Copy(sect, 1, X-1) + ']');
			end;
		end
		else
			so.Add(sl[Y]);
	end;
	so.SaveToFile(Filename + '.ini');
	so.Free;
	sl.Free;
end;}

constructor TCheatManager.Create(const AFilename: String);
var
	i: Integer;
begin
	inherited Create;

	HasCode := False;
	Filename := AFilename;

	for i := 0 to High(Word) do
		RelativeCheatCodes[i] := nil;

	AvailableCodes := TCodeList.Create(True);
	AbsoluteCheatCodes := TCodeList.Create(False);

//	AddGameGenieCode('KKKZSPIU'); // SMB3 debug mode
end;

destructor TCheatManager.Destroy;
var
	i: Integer;
begin
	for i := 0 to High(Word) do
		RelativeCheatCodes[i].Free;
	AbsoluteCheatCodes.Free;
	AvailableCodes.Free;

	inherited;
end;

function TCheatManager.DecodeValue(code: Cardinal; bitIndexes: array of Cardinal; bitCount: Cardinal): Cardinal;
var
	i: Integer;
begin
	Result := 0;
	for i := 0 to bitCount-1 do
	begin
		Result := Result shl 1;
		Result := Result or (Cardinal(code shr bitIndexes[i]) and 1);
	end;
end;

procedure TCheatManager.ClearCodes;
var
	CheatRemoved: Boolean = False;
	i: Integer;
begin
	for i := 0 to High(Word) do
	begin
		if RelativeCheatCodes[i] <> nil then
		begin
			FreeAndNil(RelativeCheatCodes[i]);
			CheatRemoved := True;
		end;
	end;

	CheatRemoved := CheatRemoved or (AbsoluteCheatCodes.Count > 0);
	AbsoluteCheatCodes.Clear;
	HasCode := False;
end;

procedure TCheatManager.ApplyCodes(addr: Word; var value: Byte);
var
	List: TCodeList;
	Code: TCodeInfo;
	AbsAddr: Integer;
begin
	if not HasCode then Exit;

	List := RelativeCheatCodes[addr];
	if List <> nil then
	begin
		for Code in List do
		if Code.Enabled then
		begin
			if (Code.CompareValue = -1) or (Code.CompareValue = value) then
			begin
				value := Code.Value;
				Exit;
			end;
		end;
	end
	else
	if AbsoluteCheatCodes.Count > 0 then
	begin
		AbsAddr := Mapper.ToAbsoluteAddress(addr);
		if absAddr >= 0 then
		begin
			for Code in AbsoluteCheatCodes do
			if Code.Enabled then
			begin
				if (Code.Address = AbsAddr) and
					((Code.CompareValue = -1) or (Code.CompareValue = value)) then
				begin
					value := Code.Value;
					Exit;
				end;
			end;
		end;
	end;
end;

procedure TCheatManager.AddCheatCode(Code: TCodeInfo);
var
	List: TCodeList;
begin
	if Code.IsRelativeAddress then
	begin
		if (Code.Address < 0) or (Code.Address > $FFFF) then Exit;

		List := RelativeCheatCodes[Code.Address];
		if List = nil then
		begin
			List := TCodeList.Create(False);
			RelativeCheatCodes[Code.Address] := List;
		end;
		List.Add(Code);
	end
	else
		AbsoluteCheatCodes.Add(Code);

	Code.Enabled := False;
	AvailableCodes.Add(Code);

	HasCode := True;
end;

function TCheatManager.AddProActionRockyCode(PARCode: AnsiString): TCodeInfo;
var
	X: Integer;
begin
	Result := nil;

	X := PARCode.IndexOf(':');
	if X > 0 then
	begin
		Inc(X);
		Result := TCodeInfo.Create;
		Result.IsRelativeAddress := True;
		try
			Result.Address := Copy(PARCode, 1, X-1).ToInteger;
			Result.Value   := Copy(PARCode, X+1, 3).ToInteger;
			Result.CompareValue := -1;

			AddCheatCode(Result);
		except
			FreeAndNil(Result);
		end;
	end;
end;

function TCheatManager.AddProActionRockyCode(PARCode: Cardinal): TCodeInfo;
begin
	Result := GetProActionRockyInfo(PARCode);
	AddCheatCode(Result);
end;

function TCheatManager.AddGameGenieCode(GGCode: String): TCodeInfo;
begin
	Result := GetGameGenieCodeInfo(GGCode);
	AddCheatCode(Result);
end;

function TCheatManager.GetProActionRockyInfo(PARCode: Cardinal): TCodeInfo;
const
	shiftValues: array[0..30] of Cardinal = (
		3, 13, 14, 1, 6, 9, 5, 0, 12, 7, 2, 8, 10, 11, 4, //address
		19, 21, 23, 22, 20, 17, 16, 18, //compare
		29, 31, 24, 26, 25, 30, 27, 28  //value
	);
	xorValue = $5C184B91;
var
	key: Cardinal = $7E5EE93A;
	res: Cardinal = 0;
	i: Integer;
begin
	// Throw away bit 0, not used.
	ParCode := PARCode shr 1;

	for i := 30 downto 0 do
	begin
		if (((key xor PARCode) shr 30) and 1) = 1 then
		begin
			res := res or Cardinal(1 shl shiftValues[i]);
			key := key xor xorValue;
		end;
		PARCode := PARCode shl 1;
		key := key shl 1;
	end;

	Result := TCodeInfo.Create;
	Result.IsRelativeAddress := True;
	Result.Address      := (res and $7FFF) + $8000;
	Result.Value        := (res shr 24) and $FF;
	Result.CompareValue := (res shr 16) and $FF;
end;

function TCheatManager.GetGameGenieCodeInfo(GGCode: String): TCodeInfo;
const
	GGLetters = 'APZLGITYEOXUKSVN';
var
	i, idx: Integer;
	RawCode: Cardinal = 0;
	addressBits:      array[0..14] of Cardinal = ( 14, 13, 12, 19, 22, 21, 20, 7, 10, 9, 8, 15, 18, 17, 16 );
	valueBits:        array[0..7]  of Cardinal = ( 3, 6, 5, 4, 23, 2, 1, 0 );
	compareValueBits: array[0..7]  of Cardinal = ( 27, 30, 29, 28, 23, 26, 25, 24 );
begin
	Result := TCodeInfo.Create;

	for i := 0 to Length(GGCode)-1 do
	begin
		idx := GGLetters.IndexOf(GGCode[i+1]);
		if idx >= 0 then
			RawCode := RawCode or Cardinal(idx shl (i * 4));
	end;

	Result.Address := -1;
	Result.CompareValue := -1;
	Result.IsRelativeAddress := True;

	if Length(GGCode) = 8  then
	begin
		// Bit 5 of the value is stored in a different location for 8-character codes
		valueBits[4] := 31;
		Result.CompareValue := DecodeValue(RawCode, compareValueBits, 8);
	end;

	Result.Address := DecodeValue(RawCode, addressBits, 15) + $8000;
	Result.Value   := DecodeValue(RawCode, valueBits, 8);
end;

procedure TCheatManager.GetAvailableCheats;
var
	i, X: Integer;
	Ini: TStringList;
	Code: TCodeInfo;
	S, CS, DS, Hash: String;
begin
	if (not Console.GotCartridge) or
		(Cartridge.RomData.Info.Hash.PrgCrc32 = CurrentHash) then
			Exit;

	ClearCodes;
	AvailableCodes.Clear;
	AbsoluteCheatCodes.Clear;

	Ini := TStringList.Create;
	try
		Ini.LoadFromFile(Filename);
	except
		Ini.Free;
		Exit;
	end;

	try
		CurrentHash := Cartridge.RomData.Info.Hash.PrgCrc32;
		Hash := CurrentHash.ToHexString;
		Hash := Copy(Hash, 1, MaxInt);
		Hash := '[' + Hash + ']';

		i := Ini.IndexOf(Hash);

		if i >= 0 then
		begin
			Inc(i);

			for X := 0 to High(Word) do
				if RelativeCheatCodes[X] <> nil then
					RelativeCheatCodes[X].Clear;

			while i < Ini.Count do
			begin
				S := Ini[i];
				if S.StartsWith('[') then Break; // done

				X := S.IndexOf('=');

				if X > 1 then
				begin
					CS := Copy(S, 1, X);
					DS := Copy(S, X+2, MaxInt);
					if CS.Contains(':') then
						Code := AddProActionRockyCode(CS)
					else
						Code := AddGameGenieCode(CS);
					if Assigned(Code) then
						Code.Description := DS;
				end;

				Inc(i);
			end;

			if AvailableCodes.Count > 0 then
				Log('%d cheats found', [AvailableCodes.Count]);
		end;
	finally
		Ini.Free;
	end;
end;


end.

