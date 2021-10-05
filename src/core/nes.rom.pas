unit NES.ROM;

interface

{$MODE DELPHI}

uses
	SysUtils,
	NES.Types, NES.ROM.Header;

type
	TRomInfo = record
		RomName,
		Filename:        String;
		//Format:          TRomFormat;

		IsNes20Header,
		IsInDatabase,
		IsHeaderlessRom: Boolean;

		FilePrgOffset:   Cardinal;

		MapperID:        Word;
		SubMapperID:     Byte;

		System: TGameSystem;
		//VsSystemType VsType = VsSystemType::Default;
		//GameInputType InputType = GameInputType::Unspecified;
		//PpuModel VsPpuModel = PpuModel::Ppu2C02;

		HasChrRam,
		HasBattery,
		HasTrainer,
		IsPatched:     Boolean;

		Mirroring:     TMirroringType;
		BusConflicts:  TBusConflictType;

		Hash: THashInfo;
		NesHeader: TNESHeader;
		DatabaseInfo: TGameInfo;

		procedure Init;
	end;

	TRomData = record
		Info: TRomInfo;

		PrgSize,
		ChrSize,
		ChrRamSize,
		SaveChrRamSize,
		SaveRamSize,
		WorkRamSize: Int32;

		RawData,
		PrgRom, ChrRom,
		TrainerData:     TBytes;
		FdsDiskData,
		FdsDiskHeaders:  T2DByteArray;

		BiosMissing, Error: Boolean;

		procedure UpdateRomData;
		function  SetGameInfo(updateRomData: Boolean): Boolean;
	end;

	PRomData = ^TRomData;

implementation

uses
	Basement.Util, TextOutput,
	NES.Database,
	NES.Console;


procedure TROMInfo.Init;
begin
	System := TGameSystem.Unknown;
	//VsSystemType VsType = VsSystemType::Default;
	//GameInputType InputType = GameInputType::Unspecified;
	//PpuModel VsPpuModel = PpuModel::Ppu2C02;

	Mirroring := MIRROR_HORIZONTAL;
	BusConflicts := buscDefault;
end;

procedure TRomData.UpdateRomData;
var
	isValidatedEntry: Boolean;
	DB: TGameInfo;
begin
	DB := Info.DatabaseInfo;

	Info.MapperID := DB.MapperID;
	Info.System := Console.Database.GetGameSystem(DB.System);
	//if(Info.System == GameSystem::VsSystem) {
	//	romData.Info.VsType = info.VsType;
	//	romData.Info.VsPpuModel = info.VsPpuModel;
	//romData.Info.InputType = info.InputType;

	Info.SubMapperID  := DB.GetSubMapper;
	Info.BusConflicts := DB.GetBusConflictType;

	isValidatedEntry := not DB.SubmapperID.IsEmpty;
	if (isValidatedEntry) or (DB.ChrRamSize > 0) then
		ChrRamSize := DB.ChrRamSize;
	if (isValidatedEntry) or (DB.WorkRamSize > 0) then
		WorkRamSize := DB.WorkRamSize;
	if (isValidatedEntry) or (DB.SaveRamSize > 0) then
		SaveRamSize := DB.SaveRamSize;
	if isValidatedEntry then
		Info.HasBattery := DB.HasBattery
	else
		Info.HasBattery := Info.HasBattery or DB.HasBattery;

	if not DB.Mirroring.IsEmpty then
	case LowerCase(DB.Mirroring[1]) of
		'h': Info.Mirroring := MIRROR_HORIZONTAL;
		'v': Info.Mirroring := MIRROR_VERTICAL;
		'4': Info.Mirroring := MIRROR_FOURSCREENS;
//		'0': Info.Mirroring := MIRROR_SCREENAONLY;
//		'1': Info.Mirroring := MIRROR_SCREENBONLY;
	end;
end;

function TRomData.SetGameInfo(updateRomData: Boolean): Boolean;
begin
	if not Console.Database.GetGameInfo(Info.Hash.PrgChrCrc32, Info) then
	begin
		Info.IsInDatabase := False;
		Exit(False);
	end;
	Result := True;
	if updateRomData then
	begin
		LogVerbose('[DB] Database info will be used instead of file header.');
		Info.IsInDatabase := True;
		Self.UpdateRomData;
	end;
end;

end.
