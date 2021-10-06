unit NES.Mapper_001;

// Mapper 001: MMC1/SxROM
//
// CPU $6000-$7FFF: 8 KB PRG RAM bank, (optional)
// CPU $8000-$BFFF: 16 KB PRG ROM bank, either switchable or fixed to the first bank
// CPU $C000-$FFFF: 16 KB PRG ROM bank, either fixed to the last bank or switchable
// PPU $0000-$0FFF: 4 KB switchable CHR bank
// PPU $1000-$1FFF: 4 KB switchable CHR bank
//
// Through writes to the MMC1 control register, it is possible for the program to swap the fixed
// and switchable PRG ROM banks or to set up 32 KB PRG bankswitching (like BNROM), but most games
// use the default setup, which is similar to that of UxROM.

interface

uses
	NES.Mapper, NES.ROM, NES.Cartridge;

type
	TMMC1Registers = ( Reg8000, RegA000, RegC000, RegE000 );

	TPrgMode = ( _16k = 16, _32k = 32 );

	TChrMode = ( _4k = 4, _8k = 8 );

	TSlotSelect = ( x8000 = $8000, xC000 = $C000 );

	TMapper_001 = class(TMapper)
	private
		WriteBuffer, ShiftCount: Byte;
		ForceWramOn, WramDisable: Boolean;
		ChrMode: TChrMode;
		PrgMode: TPrgMode;
		SlotSelect: TSlotSelect;
		ChrReg0, ChrReg1, PrgReg: Byte;
		LastWriteCycle: QWord;
		LastChrReg: TMMC1Registers;

		function  HasResetFlag(value: Byte): Boolean;
		function  IsBufferFull(value: Byte): Boolean;
		procedure ResetBuffer;

	protected
		state: record
			Reg8000, RegA000, RegC000, RegE000: Byte;
		end;

		Initializing: Boolean;

		procedure UpdateState; virtual;

		function GetPRGPageSize: Word; override;
		function GetCHRPageSize: Word; override;

		procedure InitMapper; override;
		procedure WriteRegister(addr: Word; value: Byte); override;

	public
		constructor Create(cartridge: TCartridge); override;

		procedure Reset(SoftReset: Boolean); override;

		procedure LoadSnapshot; override;
	end;

	TMapper_155 = class(TMapper_001)
	protected
		procedure UpdateState; override;
	end;

implementation

uses
	Basement.Util, Math, SysUtils,
	NES.Types, NES.Console, NES.CPU;


constructor TMapper_001.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(8, @state.Reg8000);
	RegisterProperty(8, @state.RegA000);
	RegisterProperty(8, @state.RegC000);
	RegisterProperty(8, @state.RegE000);

	RegisterProperty(8,  @WriteBuffer);
	RegisterProperty(8,  @ShiftCount);
	RegisterProperty(64, @lastWriteCycle);
	RegisterProperty(8,  @LastChrReg);
end;

procedure TMapper_001.LoadSnapshot;
begin
	inherited LoadSnapshot;

	UpdateState;
end;

procedure TMapper_001.Reset(SoftReset: Boolean);
begin
	inherited;

	Initializing := True;
	LastWriteCycle := 0;
	ResetBuffer;
end;

function TMapper_001.HasResetFlag(value: Byte): Boolean;
begin
	Result := (value and $80) <> 0;
end;

function TMapper_001.IsBufferFull(value: Byte): Boolean;
begin
	if HasResetFlag(value) then
	begin
		//When 'r' is set:
		//	- 'd' is ignored
		//	- hidden temporary reg is reset (so that the next write is the "first" write)
		//	- bits 2,3 of reg $8000 are set (16k PRG mode, $8000 swappable)
		//	- other bits of $8000 (and other regs) are unchanged
		ResetBuffer;
		state.Reg8000 := state.Reg8000 or $0C;
		UpdateState;
		Result := False;
	end
	else
	begin
		WriteBuffer := WriteBuffer shr 1;
		WriteBuffer := WriteBuffer or ((value shl 4) and $10);
		Inc(ShiftCount);
		Result := (ShiftCount = 5);
	end;
end;

procedure TMapper_001.ResetBuffer;
begin
	ShiftCount  := 0;
	WriteBuffer := 0;
end;

procedure TMapper_001.UpdateState;
var
	extraReg, prgBankSelect: Byte;
	access:  Int8; //TMemoryAccessType;
	memType: TPrgMemoryType;
begin
	case (state.Reg8000 and 3) of
		0: SetMirroringType(TMirroringType.MIRROR_SCREENAONLY);
		1: SetMirroringType(TMirroringType.MIRROR_SCREENBONLY);
		2: SetMirroringType(TMirroringType.MIRROR_VERTICAL);
		3: SetMirroringType(TMirroringType.MIRROR_HORIZONTAL);
	end;

	WramDisable := (state.RegE000 and $10) = $10;

	if (state.Reg8000 and $04) = $04 then
		SlotSelect := x8000	else SlotSelect := xC000;

	if (state.Reg8000 and $08) = $08 then
		PrgMode := _16k else PrgMode := _32k;

	if (state.Reg8000 and $10) = $10 then
		ChrMode := _4k else ChrMode := _8k;

	ChrReg0 := state.RegA000 and $1F;
	ChrReg1 := state.RegC000 and $1F;
	PrgReg  := state.RegE000 and $0F;

	if (LastChrReg = RegC000) and (ChrMode = _4k) then
		extraReg := ChrReg1
	else
		extraReg := ChrReg0;

	// 512kb carts use bit 7 of $A000/$C000 to select page
	// This is used for SUROM (Dragon Warrior 3/4, Dragon Quest 4)
	if prgSize = $80000 then
		prgBankSelect := extraReg and $10
	else
		prgBankSelect := 0;

	access := IfThen((WramDisable) and (not ForceWramOn),
		0 {maNoAccess}, 3 {maReadWrite});
	if HasBattery then
		memType := PrgSaveRam
	else
		memType := PrgWorkRam;

	if (saveRamSize + WorkRamSize) > $4000 then
	begin
		// SXROM, 32kb of save ram
		{$IFDEF DEBUG}
		if Initializing then LogVerbose('MAPPER 1: SXROM, 32kb of save ram');
		{$ENDIF}
		SetCpuMemoryMapping($6000, $7FFF, (extraReg shr 2) and $03, memType, access);
	end
	else
	if (saveRamSize + WorkRamSize) > $2000 then
	begin
		if (saveRamSize = $2000) and (WorkRamSize = $2000) then
		begin
			// SOROM, half of the 16kb ram is battery backed
			{$IFDEF DEBUG}
			if Initializing then LogVerbose('MAPPER 1: SOROM, half of the 16kb ram is battery backed');
			{$ENDIF}
			if ((extraReg shr 3) and $01) <> 0 then
				SetCpuMemoryMapping($6000, $7FFF, 0, PrgWorkRam, access)
			else
				SetCpuMemoryMapping($6000, $7FFF, 0, PrgSaveRam, access);
		end
		else
		begin
			// Unknown, shouldn't happen
			{$IFDEF DEBUG}
			if Initializing then LogVerbose('MAPPER 1: Unknown, shouldn''t happen');
			{$ENDIF}
			SetCpuMemoryMapping($6000, $7FFF, (extraReg shr 2) and $01, memType, access);
		end;
	end
	else
	begin
		{$IFDEF DEBUG}
		if Initializing then LogVerbose('MAPPER 1: Normal (8K RAM)');
		{$ENDIF}
		// Everything else - 8kb of work or save ram
		SetCpuMemoryMapping($6000, $7FFF, 0, memType, access);
	end;

	if (RomInfo.SubMapperID = 5) then
	begin
		// SubMapper 5
		// "001: 5 Fixed PRG  SEROM, SHROM, SH1ROM use a fixed 32k PRG ROM with no banking support.
		{$IFDEF DEBUG}
		if Initializing then LogVerbose('MAPPER 1: SubMapper 5');
		{$ENDIF}
		SelectPrgPage2x(0, 0);
	end
	else
	begin
		if prgMode = _32k then
		begin
			{$IFDEF DEBUG}
			if Initializing then LogVerbose('MAPPER 1: PrgMode = 32K');
			{$ENDIF}
			SelectPrgPage2x(0, (prgReg and $FE) or prgBankSelect);
		end
		else
		begin
			// prgMode = _16k
			if slotSelect = x8000 then
			begin
				{$IFDEF DEBUG}
				if Initializing then LogVerbose('MAPPER 1: PrgMode = 16K:x8000');
				{$ENDIF}
				SelectPRGPage(0, PrgReg or prgBankSelect);
				SelectPRGPage(1, $0F or prgBankSelect);
			end
			else
			if slotSelect = xC000 then
			begin
				{$IFDEF DEBUG}
				if Initializing then LogVerbose('MAPPER 1: PrgMode = 16K:xC000');
				{$ENDIF}
				SelectPRGPage(0, prgBankSelect);
				SelectPRGPage(1, PrgReg or prgBankSelect);
			end;
		end;
	end;

	if chrMode = _8k then
	begin
		{$IFDEF DEBUG}
		if Initializing then LogVerbose('MAPPER 1: ChrMode = 8K');
		{$ENDIF}
		SelectCHRPage(0,  ChrReg0 and $1E);
		SelectCHRPage(1, (ChrReg0 and $1E) + 1);
	end
	else
	begin
		// chrMode = _4k
		{$IFDEF DEBUG}
		if Initializing then LogVerbose('MAPPER 1: ChrMode = 4K');
		{$ENDIF}
		SelectCHRPage(0, ChrReg0);
		SelectCHRPage(1, ChrReg1);
	end;
	Initializing := False;
end;

function TMapper_001.GetPRGPageSize: Word;
begin
	Result := $4000;
end;

function TMapper_001.GetCHRPageSize: Word;
begin
	Result := $1000;
end;

procedure TMapper_001.InitMapper;
begin
	inherited;

	// On powerup: bits 2,3 of $8000 are set (this ensures the $8000 is bank 0,
	// and $C000 is the last bank - needed for SEROM/SHROM/SH1ROM which do no support banking)
	state.Reg8000 := GetPowerOnByte or $0C;
	state.RegA000 := GetPowerOnByte;
	state.RegC000 := GetPowerOnByte;

	// WRAM Disable: enabled by default for MMC1B
	state.RegE000 := IfThen(RomInfo.DatabaseInfo.Board.Contains('MMC1B'), $10, $00);

	//"MMC1A: PRG RAM is always enabled" - Normally these roms should be classified as mapper 155
	ForceWramOn := (RomInfo.DatabaseInfo.Board = 'MMC1A');

	LastChrReg := RegA000;

	UpdateState;
end;

procedure TMapper_001.WriteRegister(addr: Word; value: Byte);
var
	CurrentCycle: QWord;
begin
	CurrentCycle := NES_CPU.GetCycleCount;

	// Ignore write if within 2 cycles of another write (i.e the real write after a dummy write)
	if (CurrentCycle - LastWriteCycle >= 2) and (IsBufferFull(value)) then
	begin
		case TMMC1Registers((addr and $6000) shr 13) of

			Reg8000:
				state.Reg8000 := WriteBuffer;

			RegA000: begin
				LastChrReg := RegA000;
				state.RegA000 := WriteBuffer;
			end;

			RegC000: begin
				LastChrReg := RegC000;
				state.RegC000 := WriteBuffer;
			end;

			RegE000:
				state.RegE000 := WriteBuffer;
		end;

		UpdateState;
		ResetBuffer; //Reset buffer after writing 5 bits
	end;

	LastWriteCycle := CurrentCycle;
end;

{ TMapper_155 }

procedure TMapper_155.UpdateState;
begin
	// WRAM disable bit does not exist in mapper 155
	state.RegE000 := state.RegE000 and $0F;
	inherited UpdateState;
end;

end.

