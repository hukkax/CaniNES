unit NES.Cartridge;

{$MODE DELPHI}

interface

uses
	SysUtils, FileStreamEx,
	NES.Types, NES.ROM;

const
	NES_HEADER: AnsiString = 'NES'#$1A;
	FDS_HEADER: AnsiString = 'FDS'#$1A;
	HVC_HEADER: AnsiString = #1'*NINTENDO-HVC*';

type
	TCartridge = class //(TSnapshotable)
	public
		RomData:     TRomData;
		bImageValid: Boolean;
		nMapperID:   Word;
		prgSize,
		chrSize:     Cardinal;
		ROM, VMEM:   TBytes;
		Filename:    String;

		constructor Create(const sFileName: String; var Data, IPSData: TBytes); overload;

		procedure   Init(const Data, IPSData: TBytes);
		procedure   Reset;

		function    ImageValid: Boolean;
	end;

	function GetMapperID(const Filename: String): Integer;


implementation

uses
	Classes, TextOutput, IPSpatcher,
	NES.Config, NES.Console, NES.Database, NES.ROM.Header,
	NES.Mapper, NES.MapperFactory, NES.Mapper_FDS;


function GetMapperID(const Filename: String): Integer;
var
	FS: TFileStreamEx;
	header: PNESHeader;
	RomData: TRomData;
	FmtStr: AnsiString;
begin
	Result := -1;

	FS := TFileStreamEx.Create(Filename, 0);

	try
		if Console.Database.Enabled then
		begin
			RomData.Info := Default(TRomInfo);
			RomData.Info.Init;
			RomData.Info.Hash.Crc32 := StreamGetCRC32(FS);
		end;

		FmtStr := FS.ReadString(True, 15);
		FS.SeekTo(0);

		if FmtStr.StartsWith(NES_HEADER) then
		begin
			FS.Read(RomData.Info.NesHeader, SizeOf(TNesHeader));
			header := @RomData.Info.NesHeader;
			Result := header.GetMapperID;

			if (header.Byte11 > 0) or (header.Byte12 > 0) or
			   (header.Byte13 > 0) or (header.Byte14 > 0) then
				Result := header.Byte6 shr 4;

			if RomData.Info.NesHeader.HasTrainer then
				FS.Skip(512);
			RomData.Info.Hash.PrgChrCrc32 := StreamGetCRC32(FS);

			if (Console.Database.Enabled) and (RomData.SetGameInfo(True)) then
			begin
				Result := RomData.Info.MapperID;
			end;

		end
		else
		if (FmtStr.StartsWith(FDS_HEADER)) or (FmtStr.StartsWith(HVC_HEADER)) then
		begin
			Result := TMapper.FdsMapperID;
		end;

	finally
		FS.Free;
	end;
end;

constructor TCartridge.Create(const sFileName: String; var Data, IPSData: TBytes);
var
	Fn: String;
begin
	inherited Create;

	Filename := sFilename;

	if Length(Data) < 512 then
	begin
		FileToBytes(Filename, Data{%H-});

		if Length(IPSData) < 1 then
		begin
			Fn := ChangeFileExt(FileName, '.ips');
			if FileExists(Fn) then
				FileToBytes(Fn, IPSData{%H-});
		end;
	end;

	Init(Data, IPSData);
end;

procedure TCartridge.Init(const Data, IPSData: TBytes);
var
	FS: TStreamEx;
	FStr: TMemoryStream;
	header: PNESHeader;
	TempMapper: TMapper;
	MapperClass: TMapperClass;
	PatchedData: TBytes;
	SizeChanged: Boolean = False;
	DataSize, UnpatchedSize: Cardinal;
	S: String;
	FmtStr: AnsiString;
const
	SEP = '==============================================================================';
	MSG_CANCELED = 'Loading failed: file length does not match header information';
label
	Finish;
begin
	bImageValid := False;

	RomData.Info := Default(TRomInfo);
	RomData.Info.Init;

	if not Configuration.Application.EnableDatabase then
		Console.Database.Enabled := False;

	FStr := TMemoryStream.Create;

	// use the stream via some helper functions contained in TStreamEx
	FS := TStreamEx.Create(FStr);

	// load file data
	DataSize := Length(Data);

	// apply IPS patch if applicable
	//
	if (Configuration.Application.IPSAutoPatch > 0) and (Length(IPSData) > 0) then
	begin
		UnpatchedSize := DataSize;

		FStr.Write(Data[0], DataSize);
		FStr.Seek(0, soFromBeginning);

		if Configuration.Application.IPSAutoPatch = 2 then
		begin
			FStr.Read(RomData.Info.NesHeader, SizeOf(TNesHeader));
			if RomData.Info.NesHeader.HasTrainer then
				FS.Skip(512);

			RomData.Info.Hash.PrgChrCrc32 := FS.GetCRC32;

			if (Console.Database.Enabled) and (RomData.SetGameInfo(True)) then
				prgSize := RomData.Info.DatabaseInfo.PrgRomSize
			else
				prgSize := RomData.Info.NesHeader.GetPrgSize;
			RomData.Info.Hash.PrgCrc32 := FS.GetCRC32(-1, prgSize);
		end;

		FS.SeekTo(0);

		FStr.Clear;
		FStr.Write(IPSData[0], Length(IPSData));
		FStr.Seek(0, soFromBeginning);

		S := '[IPS] Applying patch...'; // + ExtractFilename(S);
		try
			RomData.Info.IsPatched := TIpsPatcher.PatchBuffer(FStr, Data, PatchedData);
		finally
			FS.SeekTo(0);
			DataSize := Length(PatchedData);
			SizeChanged := (DataSize <> UnpatchedSize);
			FStr.Clear;
			FStr.Write(PatchedData[0], DataSize);
		end;
	end
	else
	begin
		S := '';
		FStr.Write(Data[0], DataSize);
	end;

	// stuff the data in a stream
	FStr.Seek(0, soFromBeginning);

	Log(SEP);
	Log(ExtractFilename(Filename));
	Log(SEP);

	RomData.Info.Hash.Crc32 := FS.GetCRC32;


	// Check file format

	FmtStr := FS.ReadString(True, 15);
	FS.SeekTo(0);

	if FmtStr.StartsWith(NES_HEADER) then
	begin
		LogVerbose('Type: NES ROM');
	end
	else
	if (FmtStr.StartsWith(FDS_HEADER)) or (FmtStr.StartsWith(HVC_HEADER)) then
	begin
		LogVerbose('Type: Famicom Disk System disk image');

		nMapperID := TMapper.FdsMapperID;
		RomData.Info.MapperID := nMapperID;
		prgSize := 0;
		chrSize := 0; // 8KB CHR RAM
		RomData.ChrRamSize := 8 * 1024;
		RomData.WorkRamSize := 32 * 1024;
		RomData.Info.Filename := Filename;

		SetLength(RomData.RawData, FS.Size);
		FStr.Read(RomData.RawData[0], FS.Size);

		goto Finish;
	end
	else
	begin
		LogVerbose('Type: Headerless ROM');
	end;

	if S <> '' then Log(S);

	// Read file header
	FStr.Read(RomData.Info.NesHeader, SizeOf(TNesHeader));
	header := @RomData.Info.NesHeader;

	RomData.Info.Filename := Filename;
	RomData.Info.IsNes20Header := (header.GetRomHeaderVersion = Nes2_0);
	RomData.Info.MapperID := header.GetMapperID;
	RomData.Info.SubMapperID := header.GetSubMapper;
	RomData.Info.Mirroring := header.GetMirroringType;
	RomData.Info.HasBattery := header.HasBattery;
	RomData.Info.HasTrainer := header.HasTrainer;
	RomData.Info.System := header.GetGameSystem;
	//RomData.Info.VsType := header.GetVsSystemType;
	//RomData.Info.VsPpuModel := header.GetVsSystemPpuModel;
	//RomData.Info.InputType := header.GetInputType;
	RomData.ChrRamSize := header.GetChrRamSize;
	RomData.SaveChrRamSize := header.GetSaveChrRamSize;
	RomData.WorkRamSize := header.GetWorkRamSize;
	RomData.SaveRamSize := header.GetSaveRamSize;
	RomData.Error := False;

	(*
	if(_romData.Info.System == GameSystem::Unknown) {
		//Use filename to detect PAL/VS system games
		string name = _romData.Info.Filename;
		std::transform(name.begin(), name.end(), name.begin(), ::tolower);

		if(name.find("(e)") != string::npos || name.find("(australia)") != string::npos || name.find("(europe)") != string::npos ||
			name.find("(germany)") != string::npos || name.find("(spain)") != string::npos) {
			_romData.Info.System = GameSystem::NesPal;
		} else if(name.find("(vs)") != string::npos) {
			_romData.Info.System = GameSystem::VsSystem;
		} else {
			_romData.Info.System = GameSystem::NesNtsc;
		}
	}
	*)
	(*
	if(romData.Info.System == GameSystem::VsSystem) {
		string type = "Vs-UniSystem";
		switch(romData.Info.VsType) {
			case VsSystemType::Default: break;
			case VsSystemType::IceClimberProtection: type = "VS-UniSystem (Ice Climbers)"; break;
			case VsSystemType::RaidOnBungelingBayProtection: type = "VS-DualSystem (Raid on Bungeling Bay)"; break;
			case VsSystemType::RbiBaseballProtection: type = "VS-UniSystem (RBI Baseball)"; break;
			case VsSystemType::SuperXeviousProtection: type = "VS-UniSystem (Super Xevious)"; break;
			case VsSystemType::TkoBoxingProtection: type = "VS-UniSystem (TKO Boxing)"; break;
			case VsSystemType::VsDualSystem: type = "VS-DualSystem"; break;
		}
		Log("[iNes] System: " + type);
	}
	*)

	// If a "trainer" exists we just need to read past it before we get to the good stuff
	//
	if header.HasTrainer then
	begin
		// 512-byte trainer at $7000-$71FF (stored before PRG data)
		if(DataSize >= 512) then
		begin
			SetLength(RomData.TrainerData, 512);
			FStr.Read(RomData.TrainerData[0], 512);
			Dec(DataSize, 512);
		end
		else
		begin
			RomData.Error := True;
			Message(MSG_CANCELED);
			FS.Free;
			Exit;
		end;
	end;

	RomData.Info.Hash.PrgChrMd5 := ''; // FS.GetMd5Sum(prgSize);

	if RomData.Info.Hash.PrgChrCrc32 = 0 then
		RomData.Info.Hash.PrgChrCrc32 := FS.GetCRC32;

	if (Console.Database.Enabled) and (not SizeChanged) and (RomData.SetGameInfo(True)) then
	begin
		prgSize := RomData.Info.DatabaseInfo.PrgRomSize;
		chrSize := RomData.Info.DatabaseInfo.ChrRomSize;
	end
	else
	begin
		//Fallback on header sizes when game is not in DB (or DB is disabled)
		if Console.Database.Enabled then
			Log('[DB] Game not found in database.');

		prgSize := header.GetPrgSize;
		chrSize := header.GetChrSize;

		if (header.Byte11 > 0) or (header.Byte12 > 0) or
		   (header.Byte13 > 0) or (header.Byte14 > 0) then
			nMapperID := header.Byte6 shr 4;
	end;

	if not Configuration.Application.EnableDatabase then
		LogVerbose('[DB] Game database disabled.');

	if prgSize + chrSize > dataSize then
	begin
		Message(MSG_CANCELED);
		RomData.Error := True;
		FS.Free;
		Exit;
	end
	else
	if(prgSize + chrSize < dataSize) then
		LogVerbose('[iNES] Warning: File is larger than excepted (based on the file header).');

	// Determine Mapper ID
	nMapperID := RomData.Info.MapperID;

	// Display file header information
	//
	{$IFDEF VERBOSE}
	if RomData.Info.IsNes20Header then
		S := '[iNES 2.0]'
	else
		S := '[iNES]';
	LogVerbose(S + ' Mapper: %.3d, Submapper: %d, %s mirroring',
		[header^.GetMapperID,
		header^.GetSubMapper,
		MirroringTypeNames[header^.GetMirroringType]]);
	{$ENDIF}

	// Read in ROM/RAM data
	//
	if RomData.Info.Hash.PrgCrc32 = 0 then
		RomData.Info.Hash.PrgCrc32 := FS.GetCRC32(-1, prgSize);

	SetLength(ROM, prgSize);
	FStr.Read(ROM[0], prgSize);

	if chrSize > 0 then
	begin
		SetLength(VMEM, chrSize);
		FStr.Read(VMEM[0], chrSize);
		RomData.ChrRom := VMEM;
	end;

	RomData.PrgRom := ROM; // assign memory

	// Write out ROM info
	//
	Console.Database.GetGameTitle(RomData.Info);
	Console.Database.FindBoxArt(RomData.Info);

	RomData.PrgSize := prgSize;
	RomData.ChrSize := chrSize;

	{$IFDEF DEBUG}
	Log('[CRC32] File:%s PRG:%s PRG+CHR:%s', [
		RomData.Info.Hash.Crc32.ToHexString,
		RomData.Info.Hash.PrgCrc32.ToHexString,
		RomData.Info.Hash.PrgChrCrc32.ToHexString] );

	Log('PRG ROM: %dK (%d)', [prgSize div 1024, prgSize]);
	if chrSize > 0 then
		Log('CHR ROM: %dK (%d)', [chrSize div 1024, chrSize])
	else
		Log('CHR ROM: None');
	if (RomData.ChrRamSize > 0) or (RomData.Info.IsNes20Header) then
		Log('CHR RAM: %dK (%d)', [RomData.ChrRamSize div 1024, RomData.ChrRamSize])
	else
	if chrSize = 0 then
		Log('CHR RAM: 8 KB');

	if RomData.WorkRamSize > 0 then
		Log('WorkRAM: %dK (%d)', [RomData.WorkRamSize div 1024, RomData.WorkRamSize]);
	if RomData.SaveRamSize > 0 then
		Log('SaveRAM: %dK (%d)', [RomData.SaveRamSize div 1024, RomData.SaveRamSize]);
	{$ENDIF}

	S := 'System: ' + NESSystemNames[RomData.Info.System];
	if RomData.Info.DatabaseInfo.Board <> '' then
		S := S + ', Board: ' + RomData.Info.DatabaseInfo.Board;
	Log(S);

	S := '';
	if RomData.Info.HasBattery then S := S + ' [Battery]';
	if RomData.Info.HasTrainer then S := S + ' [Trainer]';

Finish:

	FS.Free;

	// Load appropriate mapper
	//
	TempMapper := nil;

	MapperClass := GetMapperClass(nMapperID, RomData.Info.SubMapperID, @RomData);
	if MapperClass <> nil then
	begin
		TempMapper := MapperClass.Create(Self);

		if MapperClass = TMapper_FDS then
			TMapper_FDS(TempMapper).Loader_LoadRom(RomData);

		if TempMapper.GetSoundChipName <> '' then
			S := S + ' [Audio:' + TempMapper.GetSoundChipName + ']';
		if S <> '' then
			Log('Additional features:' + S);
	end
	else
		Message('Mapper #%d not implemented!', [nMapperID]);

	bImageValid := Assigned(TempMapper);
	if bImageValid then
	begin
		Mapper.Free;
		Mapper := TempMapper;
		Mapper.cart := Self;

		if nMapperID < 60000 then
			Message('Mapper: %.3d, Submapper: %d, %s mirroring',
				[nMapperID, RomData.Info.SubMapperID, MirroringTypeNames[RomData.Info.Mirroring]]);
	end;
end;

function TCartridge.ImageValid: Boolean;
begin
	Result := bImageValid;
end;

procedure TCartridge.Reset;
begin
	// Note: does not reset the ROM contents, but does reset the mapper.
	if Assigned(Mapper) then Mapper.Reset(True);
end;

end.
