unit NES.Mapper_VRC2_4;

// Mapper 21/22/23/25/27: VRC 2/4

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper, NES.VRC_IRQ;

type
	TMapper_VRC2_4 = class(TMapper)
	private
		IRQ: TVrcIrq;
		variant: TVRCVariant;

		useHeuristics: Boolean;

		prgReg0, prgReg1,
		prgMode,
		latch: Byte;

		hiCHRRegs, loCHRRegs: array [0..7] of Byte;

		procedure DetectVariant;

	protected
		procedure UpdateState;

		function GetPRGPageSize: Word; override;
		function GetCHRPageSize: Word; override;
		function AllowRegisterRead: Boolean; override;

		procedure InitMapper; override;

		function  ReadRegister(addr: Word): Byte; override;
		procedure WriteRegister(address: Word; value: Byte); override;

	public
		procedure ProcessCpuClock; override;
		function  TranslateAddress(addr: Word): Word;

		constructor Create(cartridge: TCartridge); override;
		destructor  Destroy; override;
	end;

implementation

uses
	SysUtils, Math, Basement.Util,
	NES.Config, NES.Console;

// ============================================================================
// TMapper_VRC2_4
// ============================================================================

function TMapper_VRC2_4.GetPRGPageSize: Word; begin Result := $2000; end;
function TMapper_VRC2_4.GetCHRPageSize: Word; begin Result := $400;  end;
function TMapper_VRC2_4.AllowRegisterRead: Boolean; begin Result := True; end;

constructor TMapper_VRC2_4.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	IRQ := TVrcIrq.Create;

	RegisterProperty(8, @prgReg0);
	RegisterProperty(8, @prgReg1);
	RegisterProperty(8, @prgMode);
	RegisterProperty(8, @latch);

	RegisterArray(Length(loChrRegs), @loChrRegs[0]);
	RegisterArray(Length(hiChrRegs), @hiChrRegs[0]);
end;

destructor TMapper_VRC2_4.Destroy;
begin
	IRQ.Free;

	inherited Destroy;
end;

procedure TMapper_VRC2_4.InitMapper;
var
	i: Integer;
begin
	IRQ.Reset;
	DetectVariant;

	// PRG mode only exists for VRC4+ (so keep it as 0 at all times for VRC2)
	prgMode := IfThen(variant >= VRC4a, GetPowerOnByte and $01, 0);

	prgReg0 := GetPowerOnByte and $1F;
	prgReg1 := GetPowerOnByte and $1F;
	latch := 0;

	for i := 0 to 7 do
	begin
		loCHRRegs[i] := GetPowerOnByte and $0F;
		hiCHRRegs[i] := GetPowerOnByte and $1F;
	end;

	UpdateState;

	RemoveRegisterRange(0, $FFFF, moRead);
	if (not useHeuristics) and (variant <= VRC2c) and
		(workRamSize = 0) and (saveRamSize = 0) then
			AddRegisterRange($6000, $7FFF, moAny);
end;

procedure TMapper_VRC2_4.DetectVariant;
begin
	case romInfo.MapperID of

		22: variant := VRC2a;

		23: //Conflicts: VRC4e
			case romInfo.SubMapperID of
				2:   variant := VRC4e;
				3:   variant := VRC2b;
				else variant := VRC2b;
			end;

		25: //Conflicts: VRC2c, VRC4d
			case romInfo.SubMapperID of
				1:   variant := VRC4b;
				2:   variant := VRC4d;
				3:   variant := VRC2c;
				else variant := VRC4b;
			end;

		27: variant := VRC427; // Untested

	else // Conflicts: VRC4c
		case romInfo.SubMapperID of
			1:   variant := VRC4a;
			2:   variant := VRC4c;
			else variant := VRC4a;
		end;
	end;

	useHeuristics := (romInfo.SubMapperID = 0) and
		(romInfo.MapperID <> 22) and (romInfo.MapperID <> 27);
end;

procedure TMapper_VRC2_4.ProcessCpuClock;
begin
	// Only VRC4 supports IRQs
	if ((useHeuristics) and (romInfo.MapperID <> 22)) or (variant >= VRC4a) then
		irq.ProcessCpuClock;
end;

procedure TMapper_VRC2_4.UpdateState;
var
	i: Integer;
	page: Cardinal;
begin
	for i := 0 to 7 do
	begin
		page := loCHRRegs[i] or (hiCHRRegs[i] << 4);
		// "On VRC2a (mapper 022) only the high 7 bits of the CHR regs are used --
		// the low bit is ignored.  Therefore, you effectively have to right-shift
		// the CHR page by 1 to get the actual page number."
		if variant = VRC2a then
			page := page >> 1;
		SelectCHRPage(i, page);
	end;

	if(prgMode = 0) then
	begin
		SelectPRGPage(0, prgReg0);
		SelectPRGPage(1, prgReg1);
		SelectPRGPage(2, -2);
		SelectPRGPage(3, -1);
	end
	else
	begin
		SelectPRGPage(0, -2);
		SelectPRGPage(1, prgReg1);
		SelectPRGPage(2, prgReg0);
		SelectPRGPage(3, -1);
	end;
end;

function TMapper_VRC2_4.ReadRegister(addr: Word): Byte;
begin
	// Microwire interface ($6000-$6FFF) (VRC2 only)
	Result := latch or (Console.MemoryManager.GetOpenBus and $FE);
end;

procedure TMapper_VRC2_4.WriteRegister(address: Word; value: Byte);
var
	mask, regNumber: Byte;
begin
	if address < $8000 then
	begin
		// Microwire interface ($6000-$6FFF) (VRC2 only)
		latch := value and $01;
		Exit;
	end;

	address := TranslateAddress(address) and $F00F;

	if(address >= $8000) and (address <= $8006) then
		prgReg0 := value and $1F
	else
	if ((variant <= VRC2c) and (address >= $9000) and (address <= $9003)) or
	   ((variant >= VRC4a) and (address >= $9000) and (address <= $9001)) then
	begin
		mask := $03;
		// When we are certain this is a VRC2 game, only use the first bit for mirroring selection
		if (not useHeuristics) and (variant >= VRC2a) and (variant <= VRC2c) then
			mask := $01;
		GenericChooseMirroringType(value and mask);
	end
	else
	if (variant >= VRC4a) and (address >= $9002) and (address <= $9003) then
		prgMode := (value >> 1) and $01
	else
	if (address >= $A000) and (address <= $A006) then
		prgReg1 := value and $1F
	else
	if (address >= $B000) and (address <= $E006) then
	begin
		regNumber := ((((address >> 12) and $07) - 3) << 1) + ((address >> 1) and $01);
		if (address and $01) = $00 then
			loCHRRegs[regNumber] := value and $0F  // The other reg contains the low 4 bits
		else
			hiCHRRegs[regNumber] := value and $1F; // One reg contains the high 5 bits
	end
	else
	if (address = $F000) then
		IRQ.SetReloadValueNibble(value, False)
	else
	if (address = $F001) then
		IRQ.SetReloadValueNibble(value, True)
	else
	if (address = $F002) then
		IRQ.SetControlValue(value)
	else
	if (address = $F003) then
		IRQ.AcknowledgeIrq;

	UpdateState;
end;

function TMapper_VRC2_4.TranslateAddress(addr: Word): Word;
var
	A0: Cardinal = 0;
	A1: Cardinal = 0;
begin
	if useHeuristics  then
	case variant of

		VRC2c, VRC4b, VRC4d: // Mapper 25
		begin
			// ORing both values should make most games work.
			// VRC2c and VRC4b (Both use the same bits)
			A0 := (addr >> 1) and $01;
			A1 := (addr and $01);
			//VRC4d
			A0 := A0 or ((addr >> 3) and $01);
			A1 := A1 or ((addr >> 2) and $01);
		end;

		VRC4a, VRC4c: // Mapper 21
		begin
			//VRC4a
			A0 := (addr >> 1) and $01;
			A1 := (addr >> 2) and $01;
			//VRC4c
			A0 := (A0 or (addr >> 6) and $01);
			A1 := (A1 or (addr >> 7) and $01);
		end;

		VRC2b, VRC4e: // Mapper 23
		begin
			//VRC2b
			A0 := addr and $01;
			A1 := (addr >> 1) and $01;
			//VRC4e
			A0 := A0 or ((addr >> 2) and $01);
			A1 := A1 or ((addr >> 3) and $01);
		end;

		else
			Log('Unsupported ROM');
	end
	else
	case variant of

		VRC2a: // Mapper 22
		begin
			A0 := (addr >> 1) and $01;
			A1 := (addr and $01);
		end;

		VRC427: // Mapper 27
		begin
			A0 := addr and $01;
			A1 := (addr >> 1) and $01;
		end;

		VRC2c, VRC4b: // Mapper 25
		begin
			A0 := (addr >> 1) and $01;
			A1 := (addr and $01);
		end;

		VRC4d: // Mapper 25
		begin
			A0 := (addr >> 3) and $01;
			A1 := (addr >> 2) and $01;
		end;

		VRC4a: // Mapper 21
		begin
			A0 := (addr >> 1) and $01;
			A1 := (addr >> 2) and $01;
		end;

		VRC4c: // Mapper 21
		begin
			A0 := (addr >> 6) and $01;
			A1 := (addr >> 7) and $01;
		end;

		VRC2b: // Mapper 23
		begin
			A0 := addr and $01;
			A1 := (addr >> 1) and $01;
		end;

		VRC4e: // Mapper 23
		begin
			A0 := (addr >> 2) and $01;
			A1 := (addr >> 3) and $01;
		end;

		else
			Log('Unsupported ROM');
	end;

	Result := (addr and $FF00) or (A1 << 1) or A0;
end;

end.

