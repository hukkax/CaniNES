unit NES.Mapper_JyCompany;

// Mapper 090/209/211: JyCompany

interface

uses
	NES.Cartridge, NES.Mapper, NES.Types;

type
	TJyIrqSource = (
		CpuClock = 0,
		PpuA12Rise,
		PpuRead,
		CpuWrite
	);

	TMapper_209 = class(TMapper)
	private
		chrLowRegs,
		chrHighRegs: array [0..7] of Byte;
		chrLatch:    array [0..1] of Byte;
		prgRegs:     array [0..3] of Byte;
		ntLowRegs,
		ntHighRegs:  array [0..3] of Byte;

		enablePrgAt6000,
		chrBlockMode,
		mirrorChr,
		advancedNtControl,
		disableNtRam,
		irqEnabled,
		irqFunkyMode,
		irqSmallPrescaler: Boolean;

		prgMode, prgBlock,
		chrMode, chrBlock,
		mirroringReg,
		ntRamSelectBit,
		irqCountDirection, irqFunkyModeReg,
		irqPrescaler, irqCounter, irqXorReg,
		multiplyValue1, multiplyValue2,
		regRamValue: Byte;

		lastPpuAddr: Word;

		irqSource: Byte;

	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;
		function  AllowRegisterRead: Boolean; override;

		function  ReadRegister(addr: Word): Byte; override;
		procedure WriteRegister(address: Word; value: Byte); override;

		function InvertPrgBits(prgReg: Byte; needInvert: Boolean): Byte;
		function GetChrReg(index: Integer): Word;

		procedure InitMapper; override;
		procedure TickIrqCounter;

		procedure UpdateState;
		procedure UpdatePrgState;
		procedure UpdateChrState;
		procedure UpdateMirroringState;

	public
		procedure ProcessCpuClock; override;
		procedure NotifyVRAMAddressChange(addr: Word); override;
		procedure LoadSnapshot; override;

		function  MapperReadVRAM(addr: Word; operationType: TMemoryOperationType): Byte; override;

		constructor Create(cartridge: TCartridge); override;
	end;

	TMapper_211 = class(TMapper_209)
	end;

	TMapper_090 = class(TMapper_209)
	end;

	TMapper_035 = class(TMapper_209)
	end;

implementation

uses
	SysUtils, Math, Basement.Util,
	NES.Config, NES.Console, NES.CPU;

// ============================================================================
// TMapper_090
// ============================================================================

function TMapper_209.GetPRGPageSize: Word; begin Result := $2000; end;
function TMapper_209.GetCHRPageSize: Word; begin Result := $400;  end;
function TMapper_209.AllowRegisterRead: Boolean; begin Result := True; end;

constructor TMapper_209.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterArray(Length(prgRegs),    @prgRegs[0]);
	RegisterArray(Length(chrLowRegs), @chrLowRegs[0]);
	RegisterArray(Length(chrHighRegs),@chrHighRegs[0]);
	RegisterArray(Length(ntLowRegs),  @ntLowRegs[0]);
	RegisterArray(Length(ntHighRegs), @ntHighRegs[0]);
	RegisterArray(Length(chrLatch),   @chrLatch[0]);

	RegisterProperty(1,	@enablePrgAt6000);
	RegisterProperty(1, @chrBlockMode);
	RegisterProperty(1, @mirrorChr);
	RegisterProperty(1, @advancedNtControl);
	RegisterProperty(1, @disableNtRam);
	RegisterProperty(1, @irqEnabled);
	RegisterProperty(1, @irqFunkyMode);
	RegisterProperty(1, @irqSmallPrescaler);

	RegisterProperty(8, @prgMode);
	RegisterProperty(8, @prgBlock);
	RegisterProperty(8, @chrMode);
	RegisterProperty(8, @chrBlock);
	RegisterProperty(8, @mirroringReg);
	RegisterProperty(8, @ntRamSelectBit);
	RegisterProperty(8, @irqCountDirection);
	RegisterProperty(8, @irqFunkyModeReg);
	RegisterProperty(8, @irqPrescaler);
	RegisterProperty(8, @irqCounter);
	RegisterProperty(8, @irqXorReg);
	RegisterProperty(8, @multiplyValue1);
	RegisterProperty(8, @multiplyValue2);
	RegisterProperty(8, @regRamValue);
	RegisterProperty(8, @irqSource);

	RegisterProperty(16, @lastPpuAddr);

end;

procedure TMapper_209.InitMapper;
begin
	RemoveRegisterRange($8000, $FFFF, TMemoryOperation.moRead);
	AddRegisterRange($5000, $5FFF, TMemoryOperation.moAny);

	chrLatch[0] := 0;
	chrLatch[1] := 4;

	ClearArray(prgRegs);
	ClearArray(chrLowRegs);
	ClearArray(chrHighRegs);

	prgMode := 0;
	enablePrgAt6000 := False;
	prgBlock := 0;

	chrMode := 0;
	chrBlockMode := False;
	chrBlock := 0;
	mirrorChr := False;

	mirroringReg := 0;
	advancedNtControl := False;
	disableNtRam := False;

	ntRamSelectBit := 0;
	ClearArray(ntLowRegs);
	ClearArray(ntHighRegs);

	irqEnabled := False;
	irqSource := Byte(CpuClock);
	lastPpuAddr := 0;
	irqCountDirection := 0;
	irqFunkyMode := False;
	irqFunkyModeReg := 0;
	irqSmallPrescaler := False;
	irqPrescaler := 0;
	irqCounter := 0;
	irqXorReg := 0;

	multiplyValue1 := 0;
	multiplyValue2 := 0;
	regRamValue := 0;

	UpdateState;
end;

procedure TMapper_209.LoadSnapshot;
begin
	inherited;
	UpdateState;
end;

function TMapper_209.ReadRegister(addr: Word): Byte;
begin
	case (addr and $F803) of
		$5000: Result := 0; // Dip switches
		$5800: Result := (multiplyValue1 * multiplyValue2) and $FF;
		$5801: Result := ((multiplyValue1 * multiplyValue2) >> 8) and $FF;
		$5803: Result := regRamValue;
		else   Result := Console.MemoryManager.GetOpenBus;
	end;
end;

procedure TMapper_209.WriteRegister(address: Word; value: Byte);
begin
	if address < $8000 then
	begin
		case (address and $F803) of
			$5800: multiplyValue1 := value;
			$5801: multiplyValue2 := value;
			$5803: regRamValue    := value;
		end;
	end
	else
	case (address and $F007) of
		$8000..$8007: prgRegs    [address and $03] := value and $7F;
		$9000..$9007: chrLowRegs [address and $07] := value;
		$A000..$A007: chrHighRegs[address and $07] := value;
		$B000..$B003: ntLowRegs  [address and $03] := value;
		$B004..$B007: ntHighRegs [address and $03] := value;

		$C000:
			if (value and $01) <> 0 then
				irqEnabled := True
			else
			begin
				irqEnabled := False;
				CPUClearIRQSource(irqExternal);
			end;

		$C001:
		begin
			irqCountDirection := (value >> 6) and $03;
			irqFunkyMode := (value and $08) = $08;
			irqSmallPrescaler := ((value >> 2) and $01) = $01;
			irqSource := value and $03;
		end;

		$C002:
		begin
			irqEnabled := False;
			CPUClearIRQSource(irqExternal);
		end;

		$C003: irqEnabled      := True;
		$C004: irqPrescaler    := value xor irqXorReg;
		$C005: irqCounter      := value xor irqXorReg;
		$C006: irqXorReg       := value;
		$C007: irqFunkyModeReg := value;

		$D000:
		begin
			prgMode := value and $07;
			chrMode := (value >> 3) and $03;
			advancedNtControl := (value and $20) = $20;
			disableNtRam := (value and $40) = $40;
			enablePrgAt6000 := (value and $80) = $80;
		end;

		$D001: mirroringReg   := value and $03;
		$D002: ntRamSelectBit := value and $80;

		$D003:
		begin
			mirrorChr := (value and $80) = $80;
			chrBlockMode := (value and $20) = $00;
			chrBlock := ((value and $18) >> 2) or (value and $01);
			if romInfo.MapperID in [35, 90, 209, 211] then
				prgBlock := value and $06;
		end;
	end;

	UpdateState;
end;

function TMapper_209.InvertPrgBits(prgReg: Byte; needInvert: Boolean): Byte;
begin
	if needInvert then Result :=
		((prgReg and 1) << 6) or ((prgReg and 2) << 4) or ((prgReg and 4) << 2) or (prgReg and 8) or
		((prgReg and $10) >> 2) or ((prgReg and $20) >> 4) or ((prgReg and $40) >> 6)
	else
		Result := prgReg;
end;

function TMapper_209.GetChrReg(index: Integer): Word;
var
	mask, shift: Byte;
begin
	if (chrMode >= 2) and (mirrorChr) and (index in [2,3]) then
		Dec(index, 2);

	if chrBlockMode then
	begin
		case chrMode of
			1: begin mask := $3F; shift := 6; end;
			2: begin mask := $7F; shift := 7; end;
			3: begin mask := $FF; shift := 8; end;
			else     mask := $1F; shift := 5;
		end;
		Result := (chrLowRegs[index] and mask) or (chrBlock << shift);
	end
	else
		Result := chrLowRegs[index] or (chrHighRegs[index] << 8);
end;

procedure TMapper_209.TickIrqCounter;
var
	clockIrqCounter: Boolean = False;
	mask, prescaler: Byte;
begin
	mask := IfThen(irqSmallPrescaler, $07, $FF);
	prescaler := irqPrescaler and mask;

	if irqCountDirection = $01 then
	begin
		{$R-}Inc(prescaler);
		if (prescaler and mask) = 0 then
			clockIrqCounter := True;
	end
	else
	if irqCountDirection = $02 then
	begin
		{$R-}Dec(prescaler);
		if prescaler = 0 then
			clockIrqCounter := True;
	end;

	irqPrescaler := (irqPrescaler and (not mask)) or (prescaler and mask);

	if clockIrqCounter then
	begin
		if irqCountDirection = $01 then
		begin
			{$R-}Inc(irqCounter);
			if (irqCounter = 0) and (irqEnabled) then
				CPUSetIRQSource(irqExternal);
		end
		else
		if irqCountDirection = $02 then
		begin
			{$R-}Dec(irqCounter);
			if (irqCounter = $FF) and (irqEnabled) then
				CPUSetIRQSource(irqExternal);
		end;
	end;
end;

procedure TMapper_209.UpdateState;
begin
	UpdatePrgState;
	UpdateChrState;
	UpdateMirroringState;
end;

procedure TMapper_209.UpdatePrgState;
var
	i: Integer;
	invertBits: Boolean;
	TmpPrgRegs: array[0..3] of Integer;
begin
	invertBits := (prgMode and $03) = $03;
	for i := 0 to 3 do
		TmpPrgRegs[i] := InvertPrgBits(prgRegs[i], invertBits);

	case (prgMode and $03) of
	0:	begin
			SelectPrgPage4x(0, IfThen((prgMode and $04) <> 0, TmpPrgRegs[3], $3C));
			if enablePrgAt6000 then
				SetCpuMemoryMapping($6000, $7FFF, TmpPrgRegs[3] * 4 + 3, TPrgMemoryType.PrgRom);
		end;

	1:	begin
			SelectPrgPage2x(0, TmpPrgRegs[1]  shl  1);
			SelectPrgPage2x(1, IfThen((prgMode and $04) <> 0, TmpPrgRegs[3], $3E));
			if enablePrgAt6000 then
				SetCpuMemoryMapping($6000, $7FFF, TmpPrgRegs[3] * 2 + 1, TPrgMemoryType.PrgRom);
		end;

	2,3:begin
			i := prgBlock << 5;
			SelectPrgPage(0, TmpPrgRegs[0] or i);
			SelectPrgPage(1, TmpPrgRegs[1] or i);
			SelectPrgPage(2, TmpPrgRegs[2] or i);
			SelectPrgPage(3, IfThen((prgMode and $04) <> 0, TmpPrgRegs[3] or i, $3F));
			if enablePrgAt6000 then
				SetCpuMemoryMapping($6000, $7FFF, TmpPrgRegs[3], TPrgMemoryType.PrgRom);
		end;
	end;

	if not enablePrgAt6000 then
		RemoveCpuMemoryMapping($6000, $7FFF);
end;

procedure TMapper_209.UpdateChrState;
var
	chrRegs: array[0..7] of Integer;
	i: Integer;
begin
	for i := 0 to 7 do
		chrRegs[i] := GetChrReg(i);

	case chrMode of

		0:	SelectChrPage8x(0, chrRegs[0] << 3);

		1:	begin
				SelectChrPage4x(0, chrRegs[chrLatch[0]] << 2);
				SelectChrPage4x(1, chrRegs[chrLatch[1]] << 2);
			end;

		2:	begin
				SelectChrPage2x(0, chrRegs[0] << 1);
				SelectChrPage2x(1, chrRegs[2] << 1);
				SelectChrPage2x(2, chrRegs[4] << 1);
				SelectChrPage2x(3, chrRegs[6] << 1);
			end;

		3:	for i := 0 to 7 do
				SelectCHRPage(i, chrRegs[i]);
	end;
end;

procedure TMapper_209.UpdateMirroringState;
var
	i: Integer;
begin
	//"Mapper 211 behaves as though N were always set (1), and mapper 090 behaves as though N were always clear(0)."
	if ((advancedNtControl) or (romInfo.MapperID = 211)) and (romInfo.MapperID <> 90) then
		for i := 0 to 3 do
			SetNametable(i, ntLowRegs[i] and $01)
	else
		GenericChooseMirroringType(mirroringReg);
end;

procedure TMapper_209.ProcessCpuClock;
begin
	if (TJyIrqSource(irqSource) = CpuClock) or
		((TJyIrqSource(irqSource) = CpuWrite) and Console.CPU.IsCPUWrite) then
			TickIrqCounter;
end;

procedure TMapper_209.NotifyVRAMAddressChange(addr: Word);
begin
	if (TJyIrqSource(irqSource) = PpuA12Rise) and
		((addr and $1000) <> 0) and ((lastPpuAddr and $1000) = 0) then
			TickIrqCounter;
	lastPpuAddr := addr;

	if romInfo.MapperID = 209 then
		case (addr and $2FF8) of $0FD8, $0FE8:
			begin
				chrLatch[addr >> 12] := (addr >> 4) and (((addr >> 10) and $04) or $02);
				UpdateChrState;
			end;
		end;
end;

function TMapper_209.MapperReadVRAM(addr: Word; operationType: TMemoryOperationType): Byte;
var
	ntIndex: Byte;
	chrPage: Word;
	chrOffset: Cardinal;
begin
	if (TJyIrqSource(irqSource) = PpuRead) and (operationType = memopPPURenderingRead) then
		TickIrqCounter;

	if addr >= $2000 then
	begin
		// This behavior only affects reads, not writes.
		// Additional info: https://forums.nesdev.com/viewtopic.php?f=3&t=17198
		if ((advancedNtControl) or (romInfo.MapperID = 211)) and (romInfo.MapperID <> 90) then
		begin
			ntIndex := ((addr and $2FFF) - $2000) div $400;
			if (disableNtRam) or ((ntLowRegs[ntIndex] and $80) <> (ntRamSelectBit and $80)) then
			begin
				chrPage := ntLowRegs[ntIndex] or (ntHighRegs[ntIndex] << 8);
				chrOffset := chrPage * $400 + (addr and $3FF);
				Result := IfThen(chrRomSize > chrOffset, chrRom[chrOffset], 0);
				Exit;
			end;
		end;
	end;

	Result := inherited MapperReadVRAM(addr, operationType);
end;


end.

