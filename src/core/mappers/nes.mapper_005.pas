unit NES.Mapper_005;

{$WARN 4035 off : Mixing signed expressions and longwords gives a 64bit result}

// Mapper 005: MMC5 ExROM

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper,
	NES.MemoryHandler, NES.SaveState,
	NES.APU.MMC5;

type
	TMMC5MemoryHandler = class(TIMemoryHandler)
	private
		ppuRegs: array[0..7] of Byte;
	public
		function  GetReg(addr: Word): Byte; inline;
		function  ReadRAM(addr: Word): Byte; override;
		procedure WriteRAM(addr: Word; value: Byte); override;
	end;

	TMapper_005 = class(TMapper)
	const
		ExRamSize = $400;
		NtWorkRamIndex  = 4;
		NtEmptyIndex    = 2;
		NtFillModeIndex = 3;
	private
		MemoryHandler: TMMC5MemoryHandler;
		Audio: TMMC5Audio;

		prgRamProtect1,
		prgRamProtect2: Byte;

		fillModeTile,
		fillModeColor: Byte;

		verticalSplitEnabled,
		verticalSplitRightSide: Boolean;
		verticalSplitDelimiterTile,
		verticalSplitScroll,
		verticalSplitBank: Byte;

		splitInSplitRegion: Boolean;
		splitVerticalScroll,
		splitTile: Cardinal;
		splitTileNumber: Int32;

		multiplierValue1,
		multiplierValue2: Byte;

		nametableMapping,
		extendedRamMode: Byte;

		// Extended attribute mode fields (used when extendedRamMode = 1)
		exAttributeLastNametableFetch: Word;
		exAttrLastFetchCounter: Int8;
		exAttrSelectedChrBank: Byte;

		unusedPreviousScanline: Word;
		unusedSpriteFetch,
		unusedLargeSprites: Boolean;

		prgMode: Byte;
		prgBanks: array[0..4] of Byte;

		// CHR-related fields
		chrMode,
		chrUpperBits: Byte;
		chrBanks: array[0..11] of Word;
		lastChrReg: Word;
		prevChrA: Boolean;

		// IRQ counter related fields
		irqCounterTarget,
		scanlineCounter: Byte;
		irqEnabled,
		irqPending: Boolean;

		needInFrame,
		ppuInFrame: Boolean;
		ntReadCounter,
		ppuIdleCounter: Byte;
		lastPpuReadAddr: Word;

		procedure SwitchPrgBank(reg: Word; value: Byte);
		procedure GetCpuBankInfo(reg: Word; out bankNumber: Byte;
			out memoryType: TPrgMemoryType; out accessType: TMemoryAccessType);
		procedure UpdatePrgBanks;
		procedure SwitchChrBank(reg: Word; value: Byte);
		procedure UpdateChrBanks(forceUpdate: Boolean);
		procedure SetNametableMapping(value: Byte);
		procedure SetExtendedRamMode(mode: Byte);
		procedure SetFillModeTile(tile: Byte);
		procedure SetFillModeColor(color: Byte);
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;
		function  RegisterStartAddress: Word; override;
		function  RegisterEndAddress:   Word; override;
		function  GetSaveRamPageSize: Cardinal; override;
		function  GetWorkRamPageSize: Cardinal; override;
		function  ForceSaveRamSize: Boolean; override;
		function  ForceWorkRamSize: Boolean; override;

		function  GetSaveRamSize: Cardinal; override;
		function  GetWorkRamSize: Cardinal; override;
		function  AllowRegisterRead: Boolean; override;
		procedure InitMapper; override;

		function  ReadRegister(addr: Word): Byte; override;
		procedure WriteRegister(address: Word; value: Byte); override;

		procedure DetectScanlineStart(addr: Word);
	public
	(*	function IsExtendedAttributes: Boolean;
		function GetExAttributeNtPalette(ntAddr: Word): Byte;
		function GetExAttributeAbsoluteTileAddr(ntAddr, chrAddr: Word): Cardinal;
		function GetExAttributeTileData(ntAddr, chrAddr: Word): Byte; *)

		procedure Reset(softReset: Boolean); override;
		procedure ProcessCpuClock; override;
		procedure WriteRAM(addr: Word; value: Byte); override;
		function  MapperReadVRAM(addr: Word; memoryOperationType: TMemoryOperationType): Byte; override;

		procedure LoadSnapshot; override;
		procedure SaveSnapshot; override;

		function  GetSoundChipName: AnsiString; override;

		constructor Create(cartridge: TCartridge); override;
		destructor  Destroy; override;
	end;


implementation

uses
	SysUtils, Math, Basement.Util,
	NES.Config, NES.Console, NES.CPU, NES.PPU;

// ============================================================================
// TMMC5MemoryHandler
// ============================================================================

function TMMC5MemoryHandler.GetReg(addr: Word): Byte;
begin
	Result := ppuRegs[addr and $07];
end;

function TMMC5MemoryHandler.ReadRAM(addr: Word): Byte;
begin
	Result := 0;
end;

procedure TMMC5MemoryHandler.WriteRAM(addr: Word; value: Byte);
begin
	NES_PPU.WriteRAM(addr, value);
	ppuRegs[addr and $07] := value;
end;

// ============================================================================
// TMapper_005
// ============================================================================

constructor TMapper_005.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterArray(Length(prgBanks)*1, @prgBanks[0]);
	RegisterArray(Length(chrBanks)*2, @chrBanks[0]);

	RegisterProperty(8, @prgRamProtect1);
	RegisterProperty(8, @prgRamProtect2);
	RegisterProperty(8, @fillModeTile);
	RegisterProperty(8, @fillModeColor);
	RegisterProperty(1, @verticalSplitEnabled);
	RegisterProperty(1, @verticalSplitRightSide);
	RegisterProperty(8, @verticalSplitDelimiterTile);
	RegisterProperty(8, @verticalSplitScroll);
	RegisterProperty(8, @verticalSplitBank);
	RegisterProperty(8, @multiplierValue1);
	RegisterProperty(8, @multiplierValue2);
	RegisterProperty(8, @nametableMapping);
	RegisterProperty(8, @extendedRamMode);
	RegisterProperty(16,@exAttributeLastNametableFetch);
	RegisterProperty(8, @exAttrLastFetchCounter);
	RegisterProperty(8, @exAttrSelectedChrBank);
	RegisterProperty(8, @prgMode);
	RegisterProperty(8, @chrMode);
	RegisterProperty(8, @chrUpperBits);
	RegisterProperty(16,@lastChrReg);
	RegisterProperty(1, @unusedSpriteFetch);
	RegisterProperty(1, @unusedLargeSprites);
	RegisterProperty(8, @irqCounterTarget);
	RegisterProperty(1, @irqEnabled);
	RegisterProperty(16,@unusedPreviousScanline);
	RegisterProperty(8, @scanlineCounter);
	RegisterProperty(1, @irqPending);
	RegisterProperty(1, @ppuInFrame);
	RegisterProperty(1, @splitInSplitRegion);
	RegisterProperty(32,@splitVerticalScroll);
	RegisterProperty(32,@splitTile);
	RegisterProperty(32,@splitTileNumber);
	RegisterProperty(1, @needInFrame);

	MemoryHandler := TMMC5MemoryHandler.Create('MEM5');

	Audio := TMMC5Audio.Create;
end;

destructor TMapper_005.Destroy;
begin
	MemoryHandler.Free;
	Audio.Free;

	inherited Destroy;
end;

function TMapper_005.GetSoundChipName: AnsiString; begin Result := 'Nintendo MMC5'; end;
function TMapper_005.GetPRGPageSize: Word;         begin Result := $2000; end;
function TMapper_005.GetCHRPageSize: Word;         begin Result := $0400; end;
function TMapper_005.GetSaveRamPageSize: Cardinal; begin Result := $2000; end;
function TMapper_005.GetWorkRamPageSize: Cardinal; begin Result := $2000; end;
function TMapper_005.RegisterStartAddress: Word;   begin Result := $5000; end;
function TMapper_005.RegisterEndAddress: Word;     begin Result := $5206; end;
function TMapper_005.ForceSaveRamSize: Boolean;    begin Result := True;  end;
function TMapper_005.ForceWorkRamSize: Boolean;    begin Result := True;  end;
function TMapper_005.AllowRegisterRead: Boolean;   begin Result := True;  end;

function TMapper_005.GetSaveRamSize: Cardinal;
begin
	if IsNes20 then
		Result := romInfo.NesHeader.GetSaveRamSize
	else
	if romInfo.IsInDatabase then
		Result := romInfo.DatabaseInfo.SaveRamSize
	else
		// Emulate as if a single 64k block of work/save ram existed (+ 1kb of ExRAM)
		Result := IfThen(romInfo.HasBattery, $10000, 0);

	if HasBattery then
		Inc(Result, ExRamSize);
end;

function TMapper_005.GetWorkRamSize: Cardinal;
begin
	if IsNes20 then
		Result := romInfo.NesHeader.GetWorkRamSize
	else
	if romInfo.IsInDatabase then
		Result := romInfo.DatabaseInfo.WorkRamSize
	else
		// Emulate as if a single 64k block of work/save ram existed (+ 1kb of ExRAM)
		Result := IfThen(romInfo.HasBattery, 0, $10000);

	if not HasBattery then
		Inc(Result, ExRamSize);
end;

procedure TMapper_005.GetCpuBankInfo(reg: Word; out bankNumber: Byte;
	out memoryType: TPrgMemoryType; out accessType: TMemoryAccessType);
var
	realWorkRamSize, realSaveRamSize: Integer;
begin
	bankNumber := prgBanks[reg - $5113];
	memoryType := TPrgMemoryType.PrgRom;

	if (reg = $5113) or ( ((bankNumber and $80) = $00) and (reg <> $5117) ) then
	begin
		bankNumber := bankNumber and $07;
		if (prgRamProtect1 = $02) and (prgRamProtect2 = $01) then
			accessType := TMemoryAccessType.maReadWrite
		else
			accessType := TMemoryAccessType.maRead;

		// WRAM/SRAM mirroring logic (only supports existing/known licensed MMC5 boards)
		//            Bank number
		//            0 1 2 3 4 5 6 7
		// --------------------------
		// None     : - - - - - - - -
		// 1x 8kb   : 0 0 0 0 - - - -
		// 2x 8kb   : 0 0 0 0 1 1 1 1
		// 1x 32kb  : 0 1 2 3 - - - -
		realWorkRamSize := workRamSize - IfThen(HasBattery, 0, ExRamSize);
		realSaveRamSize := saveRamSize - IfThen(HasBattery, ExRamSize, 0);

		if (IsNes20) or (romInfo.IsInDatabase) then
		begin
			memoryType := TPrgMemoryType.PrgWorkRam;
			if (HasBattery) and ( (bankNumber <= 3) or (realSaveRamSize > $2000) ) then
				memoryType := TPrgMemoryType.PrgSaveRam;
			// When not 2x 8kb (=16kb), banks 4/5/6/7 select the empty socket and return open bus
			if (bankNumber >= 4) and (realSaveRamSize + realWorkRamSize <> $4000) then
				accessType := TMemoryAccessType.maNoAccess;
		end
		else
		begin
			if HasBattery then
				memoryType := TPrgMemoryType.PrgSaveRam
			else
				memoryType := TPrgMemoryType.PrgWorkRam;
		end;

		if memoryType = TPrgMemoryType.PrgWorkRam then
		begin
			// Properly mirror work ram (by ignoring the extra 1kb ExRAM section)
			bankNumber := bankNumber and ((realWorkRamSize div $2000) - 1);
			if workRamSize = ExRamSize then
				accessType := TMemoryAccessType.maNoAccess;
		end
		else
		if memoryType = TPrgMemoryType.PrgSaveRam then
		begin
			// Properly mirror work ram (by ignoring the extra 1kb ExRAM section)
			bankNumber := bankNumber and ((realSaveRamSize div $2000) - 1);
			if saveRamSize = ExRamSize then
				accessType := TMemoryAccessType.maNoAccess;
		end;
	end
	else
	begin
		accessType := TMemoryAccessType.maRead;
		bankNumber := bankNumber and $7F;
	end;
end;

procedure TMapper_005.SwitchPrgBank(reg: Word; value: Byte);
begin
	prgBanks[reg - $5113] := value;
	UpdatePrgBanks;
end;

procedure TMapper_005.UpdatePrgBanks;
var
	value: Byte;
	memoryType: TPrgMemoryType;
	accessType: TMemoryAccessType;
begin
	GetCpuBankInfo($5113, value, memoryType, accessType);
	SetCpuMemoryMapping($6000, $7FFF, value, memoryType, accessType);

	//PRG Bank 0
	//Mode 0,1,2 - Ignored
	//Mode 3 - Select an 8KB PRG bank at $8000-$9FFF
	if prgMode = 3 then
	begin
		GetCpuBankInfo($5114, value, memoryType, accessType);
		SetCpuMemoryMapping($8000, $9FFF, value, memoryType, accessType);
	end;

	//PRG Bank 1
	//Mode 0 - Ignored
	//Mode 1,2 - Select a 16KB PRG bank at $8000-$BFFF (ignore bottom bit)
	//Mode 3 - Select an 8KB PRG bank at $A000-$BFFF
	GetCpuBankInfo($5115, value, memoryType, accessType);
	case prgMode of
		1,2: SetCpuMemoryMapping($8000, $BFFF, value and $FE, memoryType, accessType);
		3:   SetCpuMemoryMapping($A000, $BFFF, value, memoryType, accessType);
	end;

	//Mode 0,1 - Ignored
	//Mode 2,3 - Select an 8KB PRG bank at $C000-$DFFF
	if InRange(prgMode, 2, 3) then
	begin
		GetCpuBankInfo($5116, value, memoryType, accessType);
		SetCpuMemoryMapping($C000, $DFFF, value, memoryType, accessType);
	end;

	//Mode 0 - Select a 32KB PRG ROM bank at $8000-$FFFF (ignore bottom 2 bits)
	//Mode 1 - Select a 16KB PRG ROM bank at $C000-$FFFF (ignore bottom bit)
	//Mode 2,3 - Select an 8KB PRG ROM bank at $E000-$FFFF
	GetCpuBankInfo($5117, value, memoryType, accessType);
	case prgMode of
		0:   SetCpuMemoryMapping($8000, $FFFF, value and $7C, memoryType, accessType);
		1:   SetCpuMemoryMapping($C000, $FFFF, value and $7E, memoryType, accessType);
		2,3: SetCpuMemoryMapping($E000, $FFFF, value and $7F, memoryType, accessType);
	end;
end;

procedure TMapper_005.SwitchChrBank(reg: Word; value: Byte);
var
	newValue: Word;
begin
	newValue := value or (chrUpperBits shl 8);
	if (lastChrReg <> reg) or (newValue <> chrBanks[reg - $5120]) then
	begin
		chrBanks[reg - $5120] := newValue;
		lastChrReg := reg;
		UpdateChrBanks(True);
	end;
end;

procedure TMapper_005.UpdateChrBanks(forceUpdate: Boolean);
var
	largeSprites, chrA: Boolean;
begin
	largeSprites := (MemoryHandler.GetReg($2000) and $20) <> 0;

	// Using 8x8 sprites resets the last written to bank logic
	if not largeSprites then
		lastChrReg := 0;

	chrA := (not largeSprites) or
		( (splitTileNumber >= 32) and (splitTileNumber < 40) ) or
		( (not ppuInFrame) and (lastChrReg <= $5127));

	if (not forceUpdate) and (chrA = prevChrA) then Exit;

	prevChrA := chrA;

	case chrMode of
	0:		SelectChrPage8x(0, chrBanks[IfThen(chrA, $07, $0B)] shl 3);
	1:	begin
			SelectChrPage4x(0, chrBanks[IfThen(chrA, $03, $0B)] shl 2);
			SelectChrPage4x(1, chrBanks[IfThen(chrA, $07, $0B)] shl 2);
		end;
	2:	begin
			SelectChrPage2x(0, chrBanks[IfThen(chrA, $01, $09)] shl 1);
			SelectChrPage2x(1, chrBanks[IfThen(chrA, $03, $0B)] shl 1);
			SelectChrPage2x(2, chrBanks[IfThen(chrA, $05, $09)] shl 1);
			SelectChrPage2x(3, chrBanks[IfThen(chrA, $07, $0B)] shl 1);
		end;
	3:	begin
			SelectCHRPage(0, chrBanks[IfThen(chrA, $00, $08)]);
			SelectCHRPage(1, chrBanks[IfThen(chrA, $01, $09)]);
			SelectCHRPage(2, chrBanks[IfThen(chrA, $02, $0A)]);
			SelectCHRPage(3, chrBanks[IfThen(chrA, $03, $0B)]);
			SelectCHRPage(4, chrBanks[IfThen(chrA, $04, $08)]);
			SelectCHRPage(5, chrBanks[IfThen(chrA, $05, $09)]);
			SelectCHRPage(6, chrBanks[IfThen(chrA, $06, $0A)]);
			SelectCHRPage(7, chrBanks[IfThen(chrA, $07, $0B)]);
		end;
	end;
end;

procedure TMapper_005.ProcessCpuClock;
begin
	Audio.Clock;

	if ppuIdleCounter > 0 then
	begin
		Dec(ppuIdleCounter);
		if ppuIdleCounter = 0 then
		begin
			// "The "in-frame" flag is cleared when the PPU is no longer rendering.
			// This is detected when 3 CPU cycles pass without a PPU read having occurred
			// (PPU /RD has not been low during the last 3 M2 rises)."
			ppuInFrame := False;
			UpdateChrBanks(True);
		end;
	end;
end;

procedure TMapper_005.SetNametableMapping(value: Byte);
var
	// [0]: "0 - On-board VRAM page 0"
	// [1]:	"1 - On-board VRAM page 1"
	// [2]: "2 - Internal Expansion RAM, only if the Extended RAM mode allows it ($5104 is 00/01); otherwise, the nametable will read as all zeros"
	// [3]: "3 - Fill-mode data"
	nametables: array[0..3] of Byte = (0, 1, 0, NtFillModeIndex);
	i, j: Integer;
	nametableId: Byte;
	source: PByte;
begin
	nametableMapping := value;
	nametables[2] := IfThen(extendedRamMode <= 1, NtWorkRamIndex, NtEmptyIndex);

	for i := 0 to 3 do
	begin
		nametableId := nametables[(value shr (i*2)) and $03];
		if nametableId = NtWorkRamIndex then
		begin
			if HasBattery then
				source := @SaveRam[SaveRamSize - ExRamSize]
			else
				source := @WorkRam[workRamSize - ExRamSize];
			j := $2000 + (i * $400);
			SetPpuMemoryMapping(j, j+$3FF, source, Ord(TMemoryAccessType.maReadWrite));
		end
		else
			SetNametable(i, nametableId);
	end;
end;

procedure TMapper_005.SetExtendedRamMode(mode: Byte);
var
	accessType: TMemoryAccessType;
begin
	extendedRamMode := mode;

	case mode of
		// "Mode 0/1 - Not readable (returns open bus), can only be written while
		// the PPU is rendering (otherwise, 0 is written)"
		// See overridden WriteRam function for implementation
		0,1: accessType := TMemoryAccessType.maWrite;
		  2: accessType := TMemoryAccessType.maReadWrite;
		else accessType := TMemoryAccessType.maRead;
	end;

	if HasBattery then
		SetCpuMemoryMapping($5C00, $5FFF, TPrgMemoryType.PrgSaveRam,
			saveRamSize - ExRamSize, Ord(accessType))
	else
		SetCpuMemoryMapping($5C00, $5FFF, TPrgMemoryType.PrgWorkRam,
			workRamSize - ExRamSize, Ord(accessType));

	SetNametableMapping(nametableMapping);
end;

procedure TMapper_005.SetFillModeTile(tile: Byte);
var
	P: PByte;
begin
	fillModeTile := tile;

	P := GetNametable(NtFillModeIndex);
	if P <> nil then
		FillByte(P^, 32*30, tile) //32 tiles per row, 30 rows
	else
		Log('SetFillModeTile: GetNametable returned NIL!');
end;

procedure TMapper_005.SetFillModeColor(color: Byte);
var
	P: PByte;
	attributeByte: Byte;
begin
	fillModeColor := color;
	attributeByte := (color) or (color shl 2) or (color shl 4) or (color shl 6);

	P := GetNametable(NtFillModeIndex);
	if P <> nil then
	begin
		Inc(P, 32*30);
		FillByte(P^, 64, attributeByte); // Attribute table is 64 bytes
	end
	else
		Log('SetFillModeColor: GetNametable returned NIL!');
end;

procedure TMapper_005.InitMapper;
var
	P: PByte;
begin
	AddRegisterRange($FFFA, $FFFB, TMemoryOperation.moRead);

	// Override the 2000-2007 registers to catch all writes to the PPU registers (but not their mirrors)
	// MemoryHandler.Reset;

	FillByte(chrBanks[0], SizeOf(chrBanks), 0);

	ppuIdleCounter := 0;
	lastPpuReadAddr := 0;
	ntReadCounter := 0;
	prevChrA := False;

	chrMode := 0;
	prgRamProtect1 := 0;
	prgRamProtect2 := 0;
	extendedRamMode := 0;
	nametableMapping := 0;
	fillModeColor := 0;
	fillModeTile := 0;
	verticalSplitScroll := 0;
	verticalSplitBank := 0;
	verticalSplitEnabled := False;
	verticalSplitDelimiterTile := 0;
	verticalSplitRightSide := False;
	multiplierValue1 := 0;
	multiplierValue2 := 0;
	chrUpperBits := 0;
	lastChrReg := 0;

	exAttrLastFetchCounter := 0;
	exAttributeLastNametableFetch := 0;
	exAttrSelectedChrBank := 0;

	irqPending := False;
	irqCounterTarget := 0;
	scanlineCounter := 0;
	irqEnabled := False;
	ppuInFrame := False;
	needInFrame := False;

	splitInSplitRegion := False;
	splitVerticalScroll := 0;
	splitTile := 0;
	splitTileNumber := -1;

	P := GetNametable(NtEmptyIndex);
	if P <> nil then
		FillByte(P^, _NametableSize, 0);

	SetExtendedRamMode(0);

	// "Additionally, Romance of the 3 Kingdoms 2 seems to expect it to be in 8k PRG mode ($5100 = $03)."
	WriteRegister($5100, $03);

	// "Games seem to expect $5117 to be $FF on powerup (last PRG page swapped in)."
	WriteRegister($5117, $FF);

	UpdateChrBanks(True);
end;

procedure TMapper_005.Reset(softReset: Boolean);
begin
	Console.MemoryManager.RegisterWriteHandler(MemoryHandler, $2000, $2007);
end;

function TMapper_005.ReadRegister(addr: Word): Byte;
begin
	case addr of

		$5010, $5015:
			Result := Audio.ReadRegister(addr);

		$5204:
		begin
			Result := IfThen(ppuInFrame, $40, 0) or IfThen(irqPending, $80, 0);
			irqPending := False;
			CPUClearIRQSource(irqExternal);
		end;

		$5205: Result := (multiplierValue1 * multiplierValue2) and $FF;
		$5206: Result := (multiplierValue1 * multiplierValue2) shr 8;

		$FFFA, $FFFB:
		begin
			ppuInFrame := False;
			UpdateChrBanks(True);
			lastPpuReadAddr := 0;
			scanlineCounter := 0;
			irqPending := False;
			CPUClearIRQSource(irqExternal);
			Result := DebugReadRAM(addr);
		end;

		else
			Result := Console.MemoryManager.GetOpenBus;
	end;
end;

procedure TMapper_005.WriteRegister(address: Word; value: Byte);
var
	vand: Byte;
begin
	vand := value and $03;

	case address of

		$5113..$5117: SwitchPrgBank(address, value);
		$5120..$512B: SwitchChrBank(address, value);

		5000..$5007, $5010, $5011, $5015:
			Audio.WriteRegister(address, value);

		$5100: begin prgMode := vand; UpdatePrgBanks; end;
		$5101: begin chrMode := vand; UpdateChrBanks(True); end;
		$5102: begin prgRamProtect1 := vand; UpdatePrgBanks; end;
		$5103: begin prgRamProtect2 := vand; UpdatePrgBanks; end;
		$5104: SetExtendedRamMode(vand);
		$5105: SetNametableMapping(value);
		$5106: SetFillModeTile(value);
		$5107: SetFillModeColor(vand);
		$5130: chrUpperBits := vand;
		$5200:
		begin
			verticalSplitEnabled       := (value and $80) = $80;
			verticalSplitRightSide     := (value and $40) = $40;
			verticalSplitDelimiterTile := (value and $1F);
		end;
		$5201: verticalSplitScroll := value;
		$5202: verticalSplitBank   := value;
		$5203: irqCounterTarget    := value;
		$5204:
		begin
			irqEnabled := (value and $80) = $80;
			if not irqEnabled then
				CPUClearIRQSource(irqExternal)
			else
			if irqPending then
				CPUSetIRQSource(irqExternal);
		end;
		$5205: multiplierValue1 := value;
		$5206: multiplierValue2 := value;
	end;
end;

procedure TMapper_005.WriteRAM(addr: Word; value: Byte);
begin
	if (not ppuInFrame) and (extendedRamMode <= 1) and (InRange(addr, $5C00, $5FFF)) then
	begin
		// Expansion RAM ($5C00-$5FFF, read/write)
		// Mode 0/1 - Not readable (returns open bus), can only be written while
		// the PPU is rendering (otherwise, 0 is written)
		value := 0;
	end;
	inherited WriteRAM(addr, value);
end;

function TMapper_005.MapperReadVRAM(addr: Word; memoryOperationType: TMemoryOperationType): Byte;
var
	isNtFetch: Boolean;
	vertSplitScroll: Word;
	value, tileNumber: Byte;
begin
	isNtFetch := InRange(addr, $2000, $2FFF) and ((addr and $3FF) < $3C0);
	if isNtFetch then
	begin
		// Nametable data, not an attribute fetch
		splitInSplitRegion := False;
		Inc(splitTileNumber);

		if ppuInFrame then
			UpdateChrBanks(False)
		else
		if needInFrame then
		begin
			needInFrame := False;
			ppuInFrame := True;
			UpdateChrBanks(False);
		end;
	end;
	DetectScanlineStart(addr);

	ppuIdleCounter := 3;
	lastPpuReadAddr := addr;

	if (ppuInFrame) and (extendedRamMode <= 1) then
	begin
		if verticalSplitEnabled then
		begin
			vertSplitScroll := (verticalSplitScroll + scanlineCounter) mod 240;
			if addr >= $2000 then
			begin
				if isNtFetch then
				begin
					tileNumber := (splitTileNumber + 2) mod 42;

					if (tileNumber <= 32) and (
						((verticalSplitRightSide) and (tileNumber >= verticalSplitDelimiterTile)) or
						((not verticalSplitRightSide) and (tileNumber < verticalSplitDelimiterTile)) ) then
					begin
						//Split region (for next 3 fetches, attribute + 2x tile data)
						splitInSplitRegion := True;
						splitTile := ((vertSplitScroll and $F8) shl 2) or tileNumber;
						Exit( InternalReadRam($5C00 + splitTile) );
					end
					else
					begin
						//Outside of split region (or sprite data), result can get modified by ex ram mode code below
						splitInSplitRegion := False;
					end;
				end
				else
				if splitInSplitRegion then
				begin
					Exit( InternalReadRam($5FC0 or
						((splitTile and $380) shr 4) or ((splitTile and $1F) shr 2)) );
				end;
			end
			else
			if splitInSplitRegion then
			begin
				// CHR tile fetches for split region
				Exit( ChrRom[(verticalSplitBank mod (GetCHRPageCount div 4)) * $1000 +
					(((addr and (not $07)) or (vertSplitScroll and $07)) and $FFF)] );
			end;
		end;

		if (extendedRamMode = 1) and ( (splitTileNumber < 32) or (splitTileNumber >= 40) ) then
		begin
			//"In Mode 1, nametable fetches are processed normally, and can come from CIRAM nametables, fill mode, or even Expansion RAM, but attribute fetches are replaced by data from Expansion RAM."
			//"Each byte of Expansion RAM is used to enhance the tile at the corresponding address in every nametable"

			//When fetching NT data, we set a flag and then alter the VRAM values read by the PPU on the following 3 cycles (palette, tile low/high byte)
			if isNtFetch then
			begin
				// Nametable fetches
				exAttributeLastNametableFetch := addr and $03FF;
				exAttrLastFetchCounter := 3;
			end
			else
			if exAttrLastFetchCounter > 0 then // Attribute fetches
			begin
				Dec(exAttrLastFetchCounter);
				case exAttrLastFetchCounter of
				2:	begin
						//PPU palette fetch
						//Check work ram (expansion ram) to see which tile/palette to use
						//Use InternalReadRam to bypass the fact that the ram is supposed to be write-only in mode 0/1
						value := InternalReadRam($5C00 + exAttributeLastNametableFetch);

						//"The pattern fetches ignore the standard CHR banking bits, and instead use the top two bits of $5130 and the bottom 6 bits from Expansion RAM to choose a 4KB bank to select the tile from."
						exAttrSelectedChrBank := ((value and $3F) or (chrUpperBits shl 6)) mod (chrRomSize div $1000);

						//Return a byte containing the same palette 4 times - this allows the PPU to select the right palette no matter the shift value
						value := (value and $C0) shr 6;
						Exit( (value) or (value shl 2) or (value shl 4) or (value shl 6) );
					end;

				1, 0: // PPU tile data fetch (high byte and low byte)
					Exit( ChrRom[exAttrSelectedChrBank * $1000 + (addr and $FFF)] );
				end;
			end;
		end;

	end;

	Result := InternalReadVRAM(addr);
end;

procedure TMapper_005.DetectScanlineStart(addr: Word);
begin
	if InRange(addr, $2000, $2FFF) then
	begin
		if lastPpuReadAddr = addr then
			Inc(ntReadCounter) // Count consecutive identical reads
		else
			ntReadCounter := 0;

		if ntReadCounter >= 2 then
		begin
			if (not ppuInFrame) and (not needInFrame) then
			begin
				needInFrame := True;
				scanlineCounter := 0;
			end
			else
			begin
				Inc(scanlineCounter);
				if irqCounterTarget = scanlineCounter then
				begin
					irqPending := True;
					if irqEnabled then
						CPUSetIRQSource(irqExternal);
				end;
			end;
			splitTileNumber := 0;
		end;
	end
	else
		ntReadCounter := 0;
end;

(*
function TMapper_005.IsExtendedAttributes: Boolean;
begin
	Result := (extendedRamMode = 1);
end;

function TMapper_005.GetExAttributeNtPalette(ntAddr: Word): Byte;
begin
	Result := (InternalReadRam($5C00 + (ntAddr and $3FF)) and $C0) shr 6;
end;

function TMapper_005.GetExAttributeAbsoluteTileAddr(ntAddr, chrAddr: Word): Cardinal;
var
	value: Byte;
	chrBank: Word;
begin
	value := InternalReadRam($5C00 + (ntAddr and $3FF));

	// "The pattern fetches ignore the standard CHR banking bits, and instead
	// use the top two bits of $5130 and the bottom 6 bits from Expansion RAM
	// to choose a 4KB bank to select the tile from."
	chrBank := (Word(value and $3F) or Word(chrUpperBits shl 6)) mod (chrRomSize div $1000);

	Result := chrBank * $1000 + (chrAddr and $FFF);
end;

function TMapper_005.GetExAttributeTileData(ntAddr, chrAddr: Word): Byte;
begin
	Result := ChrRom[GetExAttributeAbsoluteTileAddr(ntAddr, chrAddr)];
end;
*)

procedure TMapper_005.LoadSnapshot;
begin
	inherited LoadSnapshot;

	Audio.LoadSnapshot;

	UpdatePrgBanks;
	SetNametableMapping(nametableMapping);
end;

procedure TMapper_005.SaveSnapshot;
begin
	inherited SaveSnapshot;

	Audio.SaveSnapshot;
end;

end.

