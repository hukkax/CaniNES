unit NES.Mapper_FDS;

interface

uses
	Classes, Types, SysUtils,
	NES.Types, NES.Cartridge,
	NES.Mapper, NES.ROM, NES.APU.FDS;

type
	TMapper_FDS = class(TMapper)
	const
		NoDiskInserted = $FF;
		FdsDiskSideCapacity = 65500;
	private
		Audio: TFDSAudio;

		//Write registers
		//
		_irqReloadValue,
		_irqCounter: Word;

		_irqEnabled,
		_irqRepeatEnabled,
		_diskRegEnabled,
		_soundRegEnabled: Boolean;

		_writeDataReg: Byte;

		_motorOn,
		_resetTransfer,
		_readMode,
		_crcControl,
		_diskReady,
		_diskIrqEnabled: Boolean;

		_autoDiskEjectCounter,
		_autoDiskSwitchCounter,
		_restartAutoInsertCounter,
		_lastDiskCheckFrame,
		_successiveChecks: Integer;
		_previousDiskNumber,
		_previousFrame: Cardinal;

		_extConWriteReg: Byte;

		//Read registers
		//
		_badCrc,
		_endOfHead,
		_readWriteEnabled,
		_diskWriteProtected: Boolean;

		_readDataReg: Byte;

		//Internal values
		//
		_diskNumber,
		_diskPosition,
		_delay: Cardinal;
		_crcAccumulator: Word;

		_previousCrcControlFlag,
		_gapEnded,
		_scanningDisk,
		_transferComplete: Boolean;

		_fdsRawData:   TBytes;
		_fdsDiskSides,  _fdsDiskHeaders,
		_orgDiskSides,  _orgDiskHeaders: T2DByteArray;
		_romFilepath: String;

		_disableAutoInsertDisk,
		_gameStarted,
		_needSave: Boolean;

		procedure Loader_AddGaps(var diskSide: TBytes; readBuffer: PByte);
		function  Loader_LoadBios: TBytes;

	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;
		function  GetWorkRamPageSize: Cardinal; override;
		function  GetWorkRamSize: Cardinal; override;
		function  RegisterStartAddress: Word; override;
		function  RegisterEndAddress: Word; override;
		function  AllowRegisterRead: Boolean; override;

		procedure InitMapper; override;
		procedure InitMapper(var RomData: TRomData); override;

		procedure LoadDiskData(ipsData: TBytes);
		function  CreateIpsPatch: TBytes;
		function  GetFdsDiskSideSize(side: Byte): Cardinal;
		function  ReadFdsDisk: Byte;
		procedure WriteFdsDisk(value: Byte);
		procedure ProcessAutoDiskInsert;

		procedure ClockIrq;
		procedure UpdateCrc(value: Byte);

		procedure WriteRegister(addr: Word;  value: Byte); override;
		function  ReadRegister(addr: Word): Byte; override;

	public
		function  ReadRAM(addr: Word): Byte; override;

		procedure SaveBattery; override;
		//function  GetAvailableFeatures: TConsoleFeatures; override;

		function  GetSideCount: Cardinal;
		procedure EjectDisk;
		procedure InsertDisk(diskNumber: Cardinal);
		function  GetCurrentDisk: Cardinal;
		function  IsDiskInserted: Boolean;
		function  IsAutoInsertDiskEnabled: Boolean;

		procedure Reset(SoftReset: Boolean); override;

		procedure ProcessCpuClock; override;

		procedure LoadSnapshot; override;
		procedure SaveSnapshot; override;

		function  Loader_RebuildFdsFile(diskData: T2DByteArray; needHeader: Boolean): TBytes;
		procedure Loader_LoadDiskData(romFile: TBytes; var diskData, diskHeaders: T2DByteArray);
		procedure Loader_LoadRom(var romData: TROMData);

		procedure SwitchDiskSide;
		procedure InsertNextDisk;

		constructor Create(cartridge: TCartridge); override;
		destructor  Destroy; override;
	end;

implementation

uses
	Basement.Util, Math, hkaFileUtils, IPSpatcher,
	NES.Config, NES.Console, NES.CPU;

constructor TMapper_FDS.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	//Write registers
	//
	_irqReloadValue   := 0;
	_irqCounter       := 0;
	_irqEnabled       := False;
	_irqRepeatEnabled := False;
	_diskRegEnabled   := True;
	_soundRegEnabled  := True;
	_writeDataReg     := 0;
	_motorOn          := False;
	_resetTransfer    := False;
	_readMode         := False;
	_crcControl       := False;
	_diskReady        := False;
	_diskIrqEnabled   := False;
	_autoDiskEjectCounter     := -1;
	_autoDiskSwitchCounter    := -1;
	_restartAutoInsertCounter := -1;
	_previousFrame      := 0;
	_lastDiskCheckFrame := 0;
	_successiveChecks   := 0;
	_previousDiskNumber := NoDiskInserted;
	_extConWriteReg     := 0;

	//Read registers
	//
	_badCrc             := False;
	_endOfHead          := False;
	_readWriteEnabled   := False;
	_readDataReg        := 0;
	_diskWriteProtected := False;

	//Internal values
	//
	_diskNumber   := NoDiskInserted;
	_diskPosition := 0;
	_delay        := 0;
	_previousCrcControlFlag := False;
	_gapEnded               := True;
	_scanningDisk           := False;
	_transferComplete       := False;

	_gameStarted := False;
	_needSave    := False;

	Audio := TFDSAudio.Create('FDSAUDIO');
end;

destructor TMapper_FDS.Destroy;
begin
	// Restore emulation speed to normal when closing
	//_settings.ClearFlags(EmulationFlags.ForceMaxSpeed);
	Console.Want_FastForward_Code := False;

	Audio.Free;

	inherited Destroy;
end;

procedure TMapper_FDS.InitMapper;
begin
	// FDS BIOS
	SetCpuMemoryMapping($E000, $FFFF, 0, TPrgMemoryType.PrgRom, Ord(maRead));
	// Work RAM
	SetCpuMemoryMapping($6000, $DFFF, 0, TPrgMemoryType.PrgWorkRam, Ord(maReadWrite));
	// 8k of CHR RAM
	SelectCHRPage(0, 0);
end;

procedure TMapper_FDS.InitMapper(var RomData: TRomData);
var
	ipsData: TBytes;
begin
	_romFilepath    := romData.Info.Filename;
	_fdsDiskSides   := romData.FdsDiskData;
	_fdsDiskHeaders := romData.FdsDiskHeaders;
	_fdsRawData     := romData.RawData;

	Loader_LoadDiskData(_fdsRawData, _orgDiskSides, _orgDiskHeaders);

	// Apply save data (saved as an IPS file), if found
	ipsData := [];
	DoLoadBattery(battIPS, ipsData);
	LoadDiskData(ipsData);

	if Configuration.Emulator.FDS.AutoInsertDisk then
		InsertDisk(0);
end;

function TMapper_FDS.GetPRGPageSize: Word;         begin Result := $2000; end;
function TMapper_FDS.GetCHRPageSize: Word;         begin Result := $2000; end;
function TMapper_FDS.GetWorkRamPageSize: Cardinal; begin Result := $8000; end;
function TMapper_FDS.GetWorkRamSize:     Cardinal; begin Result := $8000; end;
function TMapper_FDS.RegisterStartAddress: Word;   begin Result := $4020; end;
function TMapper_FDS.RegisterEndAddress:   Word;   begin Result := $4092; end;
function TMapper_FDS.AllowRegisterRead: Boolean;   begin Result := True;  end;

procedure TMapper_FDS.LoadDiskData(ipsData: TBytes);
var
	patchedData: TBytes;
begin
	SetLength(_fdsDiskSides,   0);
	SetLength(_fdsDiskHeaders, 0);

	if (Length(ipsData) > 0) and (TIpsPatcher.PatchBuffer(ipsData, _fdsRawData, patchedData)) then
		Loader_LoadDiskData(patchedData, _fdsDiskSides, _fdsDiskHeaders)
	else
		Loader_LoadDiskData(_fdsRawData, _fdsDiskSides, _fdsDiskHeaders);
end;

function TMapper_FDS.CreateIpsPatch: TBytes;
var
	newData: TBytes;
	needHeader: Boolean;
begin
	needHeader := StringInData(@_fdsRawData[0], FDS_HEADER);
	newData := loader_RebuildFdsFile(_fdsDiskSides, needHeader);
	Result := TIpsPatcher.CreatePatch(_fdsRawData, newData);
end;

procedure TMapper_FDS.SaveBattery;
var
	ipsData: TBytes;
begin
	if _needSave then
	begin
		ipsData := CreateIpsPatch;
		DoSaveBattery(battIPS, ipsData, Length(ipsData));
		_needSave := False;
	end;
end;

procedure TMapper_FDS.Reset(SoftReset: Boolean);
begin
	inherited Reset(SoftReset);

	_autoDiskEjectCounter  := -1;
	_autoDiskSwitchCounter := -1;
	_disableAutoInsertDisk := False;
	_gameStarted := False;
end;

function TMapper_FDS.GetFdsDiskSideSize(side: Byte): Cardinal;
begin
	// assert(side < _fdsDiskSides.size);
	Result := Length(_fdsDiskSides[side]);
end;

function TMapper_FDS.ReadFdsDisk: Byte;
begin
	// assert(_diskNumber < _fdsDiskSides.size);
	// assert(_diskPosition < _fdsDiskSides[_diskNumber].size);
	Result := _fdsDiskSides[_diskNumber][_diskPosition];
end;

procedure TMapper_FDS.WriteFdsDisk(value: Byte);
var
	currentValue: Byte;
begin
	//assert(_diskNumber < _fdsDiskSides.size);
	//assert(_diskPosition < _fdsDiskSides[_diskNumber].size);
	currentValue := _fdsDiskSides[_diskNumber][_diskPosition - 2];
	if currentValue <> value then
	begin
		_fdsDiskSides[_diskNumber][_diskPosition - 2] := value;
		_needSave := True;
	end;
end;

procedure TMapper_FDS.ClockIrq;
begin
	if not _irqEnabled then Exit;

	if _irqCounter = 0 then
	begin
		NES_CPU.SetIrqSource(irqExternal);
		_irqCounter := _irqReloadValue;
		if not _irqRepeatEnabled then
			_irqEnabled := False;
	end
	else
		Dec(_irqCounter);
end;

function TMapper_FDS.ReadRAM(addr: Word): Byte;
const
	sDiskSides: array[0..1] of String = ( 'A', 'B' );
var
	i, j, matchCount, matchIndex: Integer;
	match: Boolean;
	bufferAddr: Word;
	buffer: array[0..9] of Byte;
begin
	if (addr = $E18C) and (not _gameStarted) and ((Console.MemoryManager.DebugRead($100) and $C0) <> 0) then
	begin
		// $E18B is the NMI entry point (using $E18C due to dummy reads)
		// When NMI occurs while $100 and $C0 <> 0, it typically means that the game is starting.
//		if not _gameStarted then
//			NES_CPU.DumpRAM('X:\Data\ROM\NES\FDS\cpuram.bin');
		_gameStarted := True;
	end
	else
	if (addr = $E445) and (IsAutoInsertDiskEnabled) then
	begin
		// Game is trying to check if a specific disk/side is inserted
		// Find the matching disk and insert it automatically
		bufferAddr := Console.MemoryManager.DebugReadWord(0);
		for i := 0 to 9 do
		begin
			//Prevent infinite recursion
			if (bufferAddr + i <> $E445) then
				buffer[i] := Console.MemoryManager.DebugRead(bufferAddr + i)
			else
				buffer[i] := 0;
		end;

		matchCount := 0;
		matchIndex := -1;
		for j := 0 to Length(_fdsDiskHeaders)-1 do
		begin
			match := True;
			for i := 0 to 9 do
			begin
				if (buffer[i] <> $FF) and (buffer[i] <> _fdsDiskHeaders[j][i + 14]) then
				begin
					match := False;
					Break;
				end;
			end;

			if match then
			begin
				Inc(matchCount);
				matchIndex := IfThen(matchCount > 1, -1, j);
			end;
		end;

		// More than 1 disk matches, can happen in unlicensed games - disable auto insert logic
		if matchCount > 1 then
			_disableAutoInsertDisk := True;

		if matchIndex >= 0 then
		begin
			// Found a single match, insert it
			_diskNumber := matchIndex;
			if _diskNumber <> _previousDiskNumber then
			begin
				Log(Format('[FDS] Disk automatically inserted: Disk %d Side %s',
					[(_diskNumber div 2) + 1, sDiskSides[_diskNumber and 1]]));
				_previousDiskNumber := _diskNumber;
			end;

			// Make sure we disable fast forward
			if matchIndex > 0 then
				_gameStarted := True;
		end;

		// Prevent disk from being switched again until the disk is actually read
		_autoDiskSwitchCounter    := -1;
		_restartAutoInsertCounter := -1;
	end;

	Result := inherited ReadRAM(addr);
end;

procedure TMapper_FDS.ProcessAutoDiskInsert;
var
	fastForwardEnabled: Boolean;
	currentFrame: Cardinal;
begin
	if not IsAutoInsertDiskEnabled then Exit;

	fastForwardEnabled := Configuration.Emulator.FDS.FastForwardOnLoad;
	currentFrame := NES_PPU.FrameCount;

	if _previousFrame = currentFrame then Exit;
	_previousFrame := currentFrame;

	if _autoDiskEjectCounter > 0 then
	begin
		// After reading a disk, wait until this counter reaches 0 before
		// automatically ejecting the disk the next time $4032 is read
		Dec(_autoDiskEjectCounter);
		Console.Want_FastForward_Code := (fastForwardEnabled) and (_autoDiskEjectCounter <> 0);
	end
	else
	if _autoDiskSwitchCounter > 0 then
	begin
		// After ejecting the disk, wait a bit before we insert a new one
		Dec(_autoDiskSwitchCounter);
		Console.Want_FastForward_Code := (fastForwardEnabled) and (_autoDiskSwitchCounter <> 0);
		if _autoDiskSwitchCounter = 0 then
		begin
			// Insert a disk (real disk/side will be selected when game executes $E445
			Log('[FDS] Auto-inserted dummy disk.');
			InsertDisk(0);
			// Restart process after 200 frames if the game hasn't read the disk yet
			_restartAutoInsertCounter := 200;
		end;
	end
	else
	if _restartAutoInsertCounter > 0 then
	begin
		// After ejecting the disk, wait a bit before we insert a new one
		Dec(_restartAutoInsertCounter);
		Console.Want_FastForward_Code := (fastForwardEnabled) and (_restartAutoInsertCounter <> 0);
		if _restartAutoInsertCounter = 0 then
		begin
			// Wait a bit before ejecting the disk (eject in ~34 frames)
			Log('[FDS] Game failed to load disk, try again.');
			_previousDiskNumber    := NoDiskInserted;
			_autoDiskEjectCounter  := 34;
			_autoDiskSwitchCounter := -1;
		end;
	end;
end;

procedure TMapper_FDS.ProcessCpuClock;
var
	diskData: Byte;
	needIrq: Boolean;
begin
	if Configuration.Emulator.FDS.FastForwardOnLoad then
		Console.Want_FastForward_Code := _scanningDisk or not _gameStarted
	else
		Console.Want_FastForward_Code := False;

	ProcessAutoDiskInsert;

	ClockIrq;
	Audio.Clock;

	if (_diskNumber = NoDiskInserted) or (not _motorOn) then
	begin
		// Disk has been ejected
		_endOfHead    := True;
		_scanningDisk := False;
		Exit;
	end;

	if (_resetTransfer) and (not _scanningDisk) then Exit;

	if _endOfHead then
	begin
		_delay := 50000;
		_endOfHead := False;
		_diskPosition := 0;
		_gapEnded := False;
		Exit;
	end;

	if _delay > 0 then
		Dec(_delay)
	else
	begin
		_scanningDisk := True;
		_autoDiskEjectCounter  := -1;
		_autoDiskSwitchCounter := -1;

		diskData := 0;
		needIrq := _diskIrqEnabled;

		if _readMode then
		begin
			diskData := ReadFdsDisk;

			if not _previousCrcControlFlag then
				UpdateCrc(diskData);

			if not _diskReady then
			begin
				_gapEnded := False;
				_crcAccumulator := 0;
			end
			else
			if (diskData <> 0) and (not _gapEnded) then
			begin
				_gapEnded := True;
				needIrq   := False;
			end;

			if _gapEnded then
			begin
				_transferComplete := True;
				_readDataReg := diskData;
				if needIrq then
					NES_CPU.SetIRQSource(irqFDSDisk);
			end;
		end
		else
		begin
			if not _crcControl then
			begin
				_transferComplete := True;
				diskData := _writeDataReg;
				if needIrq then
					NES_CPU.SetIRQSource(irqFDSDisk);
			end;

			if not _diskReady then
				diskData := $00;

			if not _crcControl then
				UpdateCrc(diskData)
			else
			begin
				if(not _previousCrcControlFlag) then
				begin
					// Finish CRC calculation
					UpdateCrc($00);
					UpdateCrc($00);
				end;
				diskData := _crcAccumulator and $FF;
				_crcAccumulator := _crcAccumulator shr 8;
			end;

			WriteFdsDisk(diskData);
			_gapEnded := False;
		end;

		_previousCrcControlFlag := _crcControl;

		Inc(_diskPosition);
		if _diskPosition >= GetFdsDiskSideSize(_diskNumber) then
		begin
			_motorOn := False;
			// Wait a bit before ejecting the disk (eject in ~77 frames)
			_autoDiskEjectCounter := 77;
		end
		else
			_delay := 150;
	end;
end;

procedure TMapper_FDS.UpdateCrc(value: Byte);
var
	n: Integer;
	carry: Byte;
begin
	for n in [$01, $02, $04, $08, $10, $20, $40, $80] do
	begin
		carry := _crcAccumulator and 1;
		_crcAccumulator := _crcAccumulator shr 1;
		if carry <> 0 then
			_crcAccumulator := _crcAccumulator xor $8408;
		if (value and n) <> 0 then
			_crcAccumulator := _crcAccumulator xor $8000;
	end;
end;

procedure TMapper_FDS.WriteRegister(addr: Word; value: Byte);
begin
	if (not _diskRegEnabled)  and (addr >= $4024) and (addr <= $4026) then Exit;

	case addr of

		$4020:
			_irqReloadValue := (_irqReloadValue and $FF00) or value;

		$4021:
			_irqReloadValue := (_irqReloadValue and $00FF) or (value shl 8);

		$4022:
		begin
			_irqRepeatEnabled := Odd(value);
			_irqEnabled := ((value and $02) = $02) and _diskRegEnabled;

			if _irqEnabled then
				_irqCounter := _irqReloadValue
			else
				NES_CPU.ClearIrqSource(irqExternal);
		end;

		$4023:
		begin
			_diskRegEnabled  := (value and $01) = $01;
			_soundRegEnabled := (value and $02) = $02;

			if not _diskRegEnabled then
			begin
				_irqEnabled := False;
				NES_CPU.ClearIrqSource(irqExternal);
				NES_CPU.ClearIrqSource(irqFdsDisk);
			end;
		end;

		$4024:
		begin
			_writeDataReg := value;
			_transferComplete := False;

			// Unsure about clearing irq here: FCEUX/Nintendulator don't do this, puNES does.
			NES_CPU.ClearIrqSource(irqFdsDisk);
		end;

		$4025:
		begin
			_motorOn       := (value and $01) = $01;
			_resetTransfer := (value and $02) = $02;
			_readMode      := (value and $04) = $04;
			ApplyMirroringType(value and $08,
				MIRROR_HORIZONTAL, MIRROR_VERTICAL);
			_crcControl     := (value and $10) = $10;
			//Bit 6 is not used, always 1
			_diskReady      := (value and $40) = $40;
			_diskIrqEnabled := (value and $80) = $80;

			//Writing to $4025 clears IRQ according to FCEUX, puNES and Nintendulator
			//Fixes issues in some unlicensed games (error $20 at power on)
			NES_CPU.ClearIrqSource(irqFdsDisk);
		end;

		$4026:
			_extConWriteReg := value;

		else
			if addr >= $4040 then
				if _soundRegEnabled then
					Audio.WriteRegister(addr, value);

	end;
end;

function TMapper_FDS.ReadRegister(addr: Word): Byte;
var
	value: Byte;
begin
	value := Console.MemoryManager.GetOpenBus;
	Result := value;

	if (_soundRegEnabled) and (addr >= $4040) then
		Result := Audio.ReadRegister(addr)
	else
	if (_diskRegEnabled) and (addr <= $4033) then
	begin
		case addr of

			$4030: // Disk Status Register 0
			begin
				// These 3 pins are open bus
				value := value and $2C;

				value := value or IfThen(NES_CPU.HasIrqSource(irqExternal), $01, $00);
				value := value or IfThen(_transferComplete, $02, $00);
				value := value or IfThen(_badCrc, $10, $00);
				//value |= _endOfHead ? $40 : $00;
				//value |= _diskRegEnabled ? $80 : $00;

				_transferComplete := False;
				NES_CPU.ClearIrqSource(irqExternal);
				NES_CPU.ClearIrqSource(irqFdsDisk);
				Result := value;
			end;

			$4031: // Read data register
			begin
				_transferComplete := False;
				NES_CPU.ClearIrqSource(irqFdsDisk);
				Result := _readDataReg;
			end;

			$4032: // Disk drive status register
			begin
				// These 5 pins are open bus
				value := value and $F8;
				// Disk not in drive
				value := value or IfThen(IsDiskInserted, $00, $01);
				// Disk not ready
				value := value or IfThen((not IsDiskInserted) or (not _scanningDisk), $02, $00);
				// Disk not writable
				value := value or IfThen(IsDiskInserted, $00, $04);

				if IsAutoInsertDiskEnabled then
				begin
					if (NES_PPU.FrameCount - _lastDiskCheckFrame < 100) then
						Inc(_successiveChecks)
					else
						_successiveChecks := 0;
					_lastDiskCheckFrame := NES_PPU.FrameCount;

					if (_successiveChecks > 20) and (_autoDiskEjectCounter = 0)
						and (_autoDiskSwitchCounter = -1) then
					begin
						// Game tried to check if a disk was inserted or not -
						// this is usually done when the disk needs to be changed
						// Eject the current disk and insert the new one in ~77 frames
						_lastDiskCheckFrame := 0;
						_successiveChecks := 0;
						_autoDiskSwitchCounter := 77;
						_previousDiskNumber := _diskNumber;
						_diskNumber := NoDiskInserted;

						Log('[FDS] Disk automatically ejected.');
					end;
				end;

				Result := value;
			end;

			$4033: // External connector read
				Result := _extConWriteReg; // Always return good battery

		end;
	end;
end;

// TConsoleFeatures FDS.GetAvailableFeatures begin Result := ConsoleFeatures.Fds; end;

function TMapper_FDS.GetSideCount: Cardinal;
begin
	Result := Length(_fdsDiskSides);
end;

procedure TMapper_FDS.EjectDisk;
begin
	_diskNumber := NoDiskInserted;
end;

procedure TMapper_FDS.InsertDisk(diskNumber: Cardinal);
begin
	if _diskNumber = NoDiskInserted then
	begin
		if diskNumber > 0 then
			diskNumber := diskNumber mod GetSideCount;
		_diskNumber := diskNumber;
	end;
end;

function TMapper_FDS.GetCurrentDisk: Cardinal;
begin
	Result := _diskNumber;
end;

function TMapper_FDS.IsDiskInserted: Boolean;
begin
	Result := _diskNumber <> NoDiskInserted;
end;

function TMapper_FDS.IsAutoInsertDiskEnabled: Boolean;
begin
	Result := (not _disableAutoInsertDisk) and
		(Configuration.Emulator.FDS.AutoLoadDisk);
end;

procedure TMapper_FDS.LoadSnapshot;
begin
	inherited LoadSnapshot;

	Audio.LoadSnapshot;
end;

procedure TMapper_FDS.SaveSnapshot;
begin
	inherited SaveSnapshot;

	Audio.SaveSnapshot;
end;

// ==========================================================================
// Loader
// ==========================================================================

procedure TMapper_FDS.Loader_AddGaps(var diskSide: TBytes; readBuffer: PByte);
var
	blockType: Byte;
	j, c, blockLength: Cardinal;
	S: RawByteString;
begin
	S := StringOfChar(#0, 28300 div 8);
	j := 0;
	while j < FdsDiskSideCapacity do
	begin
		blockType := readBuffer[j];
		blockLength := 1;
		case blockType of
			1: blockLength := 56; // Disk header
			2: blockLength := 2;  // File count
			3: blockLength := 16; // File header
			4: blockLength := 1 + readBuffer[j-3] + readBuffer[j-2] * $100;
			else Exit; // End parsing when we encounter an invalid block type
		end;

		if blockType = 0 then
			S := S + #0
		else
		begin
			S := S + #$80;
			for c := 0 to blockLength-1 do
				S := S + Chr(readBuffer[j+c]);
			S := S + #$4D#$62 + // Fake CRC value
				StringofChar(#0, 976 div 8); // Insert 976 bits of gap after a block
		end;

		Inc(j, blockLength);
		diskSide := diskSide + BytesOf(S);
		S := '';
	end;
end;

function TMapper_FDS.Loader_LoadBios: TBytes;
const
	Validation: array[0..3] of Byte = (	$00, $38, $4C, $C6 );
var
	Filename: String;
	FS: TFileStream;
	Files: TStrings;
	Done: Boolean;
	i: Integer;
begin
	Done := False;
	SetLength(Result{%H-}, 0);
	Files := ListFiles(Configuration.Path + '*', False);

	for Filename in Files do
	begin
		try
			FS := TFileStream.Create(Filename, fmOpenRead);
			if FS.Size = 8*1024 then
			try
				SetLength(Result, FS.Size);
				FS.ReadBuffer(Result, FS.Size);
				Done := True;
				for i := 0 to High(Validation) do
					if Result[i] <> Validation[i] then
						Done := False;
			except
				Done := False;
			end;
		finally
			FS.Free;
		end;
		if Done then Break;
	end;

	Files.Free;
	if Done then
		Log('[FDS] Loaded BIOS from %s.', [Filename])
	else
		Log('[FDS] Did not find a valid BIOS file.');
end;

function TMapper_FDS.Loader_RebuildFdsFile(diskData: T2DByteArray; needHeader: Boolean): TBytes;
var
	output: TBytes = [];
	i, len, gapNeeded: Int64;
	p, fileSize, blockLength: Cardinal;
	inGap: Boolean;
	side: Integer;
	diskSide: PByte;
begin
	// SetLength(output, Length(diskData) * FdsDiskSideCapacity + 16);
	if needHeader then
	begin
		SetLength(output, 16);
		ZeroMemory(@output[0], 16);
		CopyMemory(@output[0], @FDS_HEADER[1], Length(FDS_HEADER));
		output[4] := Length(diskData);
	end;

	for side := Low(diskData) to High(diskData) do
	begin
		diskSide := @diskData[side, 0];

		inGap := True;
		i := 0;
		len := System.Length(diskData[side]);
		gapNeeded := FdsDiskSideCapacity;
		fileSize := 0;

		while i < len do
		begin
			if inGap then
			begin
				if diskSide[i] = $80 then
					inGap := False;
				Inc(i);
			end
			else
			begin
				case diskSide[i] of
					1: blockLength := 56; // Disk header
					2: blockLength := 2;  // File count
					3: begin blockLength := 16; // File header
						fileSize := diskSide[i + 13] + diskSide[i + 14] * $100;
					end;
					4: blockLength := 1 + fileSize;
					else
						blockLength := 1;
				end;

				//output := output + Copy(diskSide^, i, blockLength);
				p := Length(output);
				SetLength(output, p + blockLength);
				CopyMemory(@output[p], @diskside[i], blockLength);
				//output.insert(output.end(), &diskSide[i], &diskSide[i] + blockLength);

				Dec(gapNeeded, blockLength);
				Inc(i, blockLength);
				Inc(i, 2); // Skip CRC after block
				inGap := True;
			end;
		end;

		if gapNeeded > 0 then
		begin
			p := Length(output);
			SetLength(output, p + gapNeeded);
			ZeroMemory(@output[p], gapNeeded);
			//output.insert(output.end(), gapNeeded, 0);
		end;
	end;

	Result := output;
end;

procedure TMapper_FDS.Loader_LoadDiskData(romFile: TBytes; var diskData, diskHeaders: T2DByteArray);
var
	i: Integer;
	numberOfSides: Byte = 0;
	fileOffset: Cardinal = 0;
	hasHeader: Boolean;
	fdsDiskImage: TBytes;
begin
	hasHeader := StringInData(@romFile[0], FDS_HEADER);

	if hasHeader then
	begin
		numberOfSides := romFile[4];
		fileOffset := 16;
	end
	else
		numberOfSides := Length(romFile) div 65500;

	// Log('LoadDiskData: ', numberOfSides, ' disk side(s)');

	SetLength(diskData,    numberOfSides);
	SetLength(diskHeaders, numberOfSides);

	for i := 0 to numberOfSides-1 do
	begin
		SetLength(fdsDiskImage{%H-}, 0);

		SetLength(diskHeaders[i], 56);
		Move(romFile[fileOffset+1], diskHeaders[i][0], 56);

		Loader_AddGaps(fdsDiskImage, @romFile[fileOffset]);
		Inc(fileOffset, FdsDiskSideCapacity);

		// Ensure the image is 65500 bytes
		if Length(fdsDiskImage) < FdsDiskSideCapacity then
			SetLength(fdsDiskImage, FdsDiskSideCapacity);

		SetLength(diskData[i], 0);
		diskData[i] := diskData[i] + fdsDiskImage;
	end;
end;

procedure TMapper_FDS.Loader_LoadRom(var romData: TROMData);
begin
//	romData.Info.Hash.PrgCrc32 = CRC32::GetCRC(romFile, Length(romFile));

	//romData.Info.Format := RomFormat::Fds;
	romData.Info.System    := TGameSystem.FDS;
	romData.Info.MapperID  := TMapper.FdsMapperID;
	romData.Info.Mirroring := MIRROR_VERTICAL;

	romData.PrgRom := Loader_LoadBios;

	if Length(romData.PrgRom) <> $2000 then
	begin
		romData.Error := True;
		romData.BiosMissing := True;
		Log('Invalid ROM length!');
	end;
end;

// ==========================================================================
// Disk insertion/swapping
// ==========================================================================

procedure TMapper_FDS.SwitchDiskSide;
begin

end;

procedure TMapper_FDS.InsertNextDisk;
begin

end;


end.
