unit NES.ROM.Header;

interface

uses
	SysUtils,
	NES.Types;

type
	TRomHeaderVersion = ( Nes2_0, iNes, OldiNes );

	// iNES Format Header
	TNESHeader = record
		Name: array[0..3] of AnsiChar;
		PrgCount,
		ChrCount,
		Byte6,  // mapper1
		Byte7,  // mapper2
		Byte8,  // prg_ram_size
		Byte9,  // tv_system1
		Byte10, // tv_system2
		Byte11,
		Byte12,
		Byte13,
		Byte14,
		Byte15: Byte;

		function GetMapperID: Word;
		function HasBattery: Boolean;
		function HasTrainer: Boolean;
		function GetRomHeaderVersion: TRomHeaderVersion;
		function GetSizeValue(exponent, multiplier: Cardinal): Cardinal;
		function GetPrgSize: Int32;
		function GetChrSize: Int32;
		function GetWorkRamSize: Int32;
		function GetSaveRamSize: Int32;
		function GetChrRamSize: Int32;
		function GetSaveChrRamSize: Int32;
		function GetSubMapper: Byte;
		function GetMirroringType: TMirroringType;
		function GetGameSystem: TGameSystem;
		function GetNesGameSystem: TGameSystem;

		//function  GameInputType GetInputType();
		//function  VsSystemType GetVsSystemType();
		//function  PpuModel GetVsSystemPpuModel();
	end;
	PNESHeader = ^TNESHeader;

	THashInfo = record
		Crc32,
		PrgCrc32,
		PrgChrCrc32: Cardinal;
		Sha1,
		PrgChrMd5:   String;
	end;

	TDatabaseEntry = record
		Extra:    Cardinal; // unused
		DirIndex: Word;
		Hash:     THashInfo;
		Filename: String;
	end;

	TGameInfo = record
		//Crc:          Cardinal;
		System,
		Board,
		Pcb,
		Chip:         String;
		MapperID:     Word;
		PrgRomSize,
		ChrRomSize,
		ChrRamSize,
		WorkRamSize,
		SaveRamSize:  Cardinal;
		HasBattery:   Boolean;
		Mirroring,
		BusConflicts,
		SubmapperID:  String;
		InitRamValue: Integer;

		Title:        String;
		Artwork:      array [caBoxArt..caTitles] of String; // file paths to cart. artwork

		InputType:    TGameInputType;
		//VsSystemType VsType;
		//PpuModel VsPpuModel;

		function  GetBusConflictType: TBusConflictType;
		function  GetSubMapper: Byte;
		//function GetGameSystem: TGameSystem(const system: String);
	end;
	PGameInfo = ^TGameInfo;


implementation

uses
	Basement.Util, Math;

{ TNESHeader }

function TNESHeader.GetNesGameSystem: TGameSystem;
begin
	Result := TGameSystem.Unknown;

	case GetRomHeaderVersion of

		TRomHeaderVersion.Nes2_0:
			case (Byte12 and $03) of
				0: Result := TGameSystem.NES_NTSC;
				1: Result := TGameSystem.NES_PAL;
				2: Result := TGameSystem.NES_NTSC; // Game works with both NTSC/PAL, pick NTSC by default
				3: Result := TGameSystem.Dendy;
			end;

		TRomHeaderVersion.iNes:
			if Odd(Byte9) then
				Result := TGameSystem.NES_PAL
			else
				Result := TGameSystem.Unknown;

	end;
end;

function TNESHeader.GetGameSystem: TGameSystem;
begin
	Result := GetNesGameSystem;

	case GetRomHeaderVersion of

		TRomHeaderVersion.Nes2_0:
			case (Byte7 and $03) of
				1: Result := TGameSystem.VsSystem;
				2: Result := TGameSystem.Playchoice;
				3: case Byte13 of
					1: Result := TGameSystem.VsSystem;
					2: Result := TGameSystem.Playchoice;
					else
						Log('[iNes] Unsupported console type detected (using NES NTSC instead)');
						Result := TGameSystem.NES_NTSC;
			   end;
			end;

		TRomHeaderVersion.iNes:
			if Odd(Byte7) then
				Result := TGameSystem.VsSystem
			else
			if (Byte7 and $02) <> 0 then
				Result := TGameSystem.Playchoice;
	end;
end;

function TNESHeader.GetMapperID: Word;
begin
	case GetRomHeaderVersion of
		iNes:    Result := (Byte7 and $F0) or (Byte6 shr 4);
		OldiNes: Result := (Byte6 shr 4);
		Nes2_0:  Result := ((Byte8 and $0F) shl 8) or (Byte7 and $F0) or (Byte6 shr 4);
	end;
end;

function TNESHeader.HasBattery: Boolean;
begin
	Result := (Byte6 and $02) <> 0;
end;

function TNESHeader.HasTrainer: Boolean;
begin
	Result := (Byte6 and $04) <> 0;
end;

function TNESHeader.GetRomHeaderVersion: TRomHeaderVersion;
begin
	if (Byte7 and $0C) = $08 then
		Result := Nes2_0
	else
	if (Byte7 and $0C) = $00 then
		Result := iNes
	else
		Result := OldiNes;
end;

function TNESHeader.GetSizeValue(exponent, multiplier: Cardinal): Cardinal;
var
	size: QWord;
begin
	if exponent > 60 then
	begin
		// Restrict max size to avoid overflow in a 64-bit value
		exponent := 60;
		Log('[iNes] Unsupported size value.');
	end;
	multiplier := multiplier * 2 + 1;
	size := multiplier * QWord(1 shl exponent);
	if size >= QWord(1 shl 32) then
		Log('[iNes] Unsupported size value.');
	Result := Cardinal(size);
end;

function TNESHeader.GetPrgSize: Int32;
begin
	if GetRomHeaderVersion = Nes2_0 then
	begin
		if (Byte9 and $0F) = $0F then
			Result := GetSizeValue(PrgCount shr 2, PrgCount and $03)
		else
			Result := (((Byte9 and $0F) shl 8) or PrgCount) * $4000;
	end
	else
	begin
		if PrgCount = 0 then
			Result := 256 * $4000 //0 is a special value and means 256
		else
			Result := PrgCount * $4000;
	end;
end;

function TNESHeader.GetChrSize: Int32;
begin
	if GetRomHeaderVersion = Nes2_0 then
	begin
		if (Byte9 and $F0) = $F0 then
			Result := GetSizeValue(ChrCount shr 2, ChrCount and $03)
		else
			Result := (((Byte9 and $F0) shl 4) or ChrCount) * $2000;
	end
	else
		Result := ChrCount * $2000;
end;

function TNESHeader.GetWorkRamSize: Int32;
var
	value: Byte;
begin
	if GetRomHeaderVersion = Nes2_0 then
	begin
		value := Byte10 and $0F;
		Result := IfThen(value = 0, 0, Trunc(128 * Power(2, value-1)) );
	end
	else
		Result := -1;
end;

function TNESHeader.GetSaveRamSize: Int32;
var
	value: Byte;
begin
	if GetRomHeaderVersion = Nes2_0 then
	begin
		value := (Byte10 and $F0) shr 4;
		Result := IfThen(value = 0, 0, Trunc(128 * Power(2, value-1)) );
	end
	else
		Result := -1;
end;

function TNESHeader.GetChrRamSize: Int32;
var
	value: Byte;
begin
	if GetRomHeaderVersion = Nes2_0 then
	begin
		value := Byte11 and $0F;
		Result := IfThen(value = 0, 0, Trunc(128 * Power(2, value-1)) );
	end
	else
		Result := -1;
end;

function TNESHeader.GetSaveChrRamSize: Int32;
var
	value: Byte;
begin
	if GetRomHeaderVersion = Nes2_0 then
	begin
		value := (Byte11 and $F0) shr 4;
		Result := IfThen(value = 0, 0, Trunc(128 * Power(2, value-1)) );
	end
	else
		Result := -1;
end;

function TNESHeader.GetSubMapper: Byte;
begin
	if GetRomHeaderVersion = Nes2_0 then
		Result := (Byte8 and $F0) shr 4
	else
		Result := 0;
end;

function TNESHeader.GetMirroringType: TMirroringType;
begin
	if (Byte6 and $08) <> 0 then
		Result := MIRROR_FOURSCREENS
	else
	if Odd(Byte6) then
		Result := MIRROR_VERTICAL
	else
		Result := MIRROR_HORIZONTAL;
end;

{ TGameInfo }

function TGameInfo.GetBusConflictType: TBusConflictType;
begin
	Result := buscDefault;
	if not BusConflicts.IsEmpty then
	case LowerCase(BusConflicts[1]) of
		'y': Result := buscYes;
		'n': Result := buscNo;
	end;
end;

function TGameInfo.GetSubMapper: Byte;
begin
	if SubmapperID.IsEmpty then
		Result := 0
	else
		Result := SubmapperID.ToInteger;
end;

end.
