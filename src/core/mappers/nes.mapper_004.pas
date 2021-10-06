unit NES.Mapper_004;

{$WARN 4035 off : Mixing signed expressions and longwords gives a 64bit result}

// Mapper 004: MMC3A, MMC3B, MMC3C, MMC6
// Mapper 044/045/047/049/052/091/114/187/189: MMC3 variants
//
// MMC3: CPU $6000-$7FFF: 8 KB PRG RAM bank (optional)
// MMC6: CPU $7000-$7FFF: 1 KB PRG RAM, mirrored
// ALL:  CPU $8000-$9FFF (or $C000-$DFFF): 8 KB switchable PRG ROM bank
// ALL:  CPU $A000-$BFFF: 8 KB switchable PRG ROM bank
// ALL:  CPU $C000-$DFFF (or $8000-$9FFF): 8 KB PRG ROM bank, fixed to the second-last bank
// ALL:  CPU $E000-$FFFF: 8 KB PRG ROM bank, fixed to the last bank
// PPU $0000-$07FF (or $1000-$17FF): 2 KB switchable CHR bank
// PPU $0800-$0FFF (or $1800-$1FFF): 2 KB switchable CHR bank
// PPU $1000-$13FF (or $0000-$03FF): 1 KB switchable CHR bank
// PPU $1400-$17FF (or $0400-$07FF): 1 KB switchable CHR bank
// PPU $1800-$1BFF (or $0800-$0BFF): 1 KB switchable CHR bank
// PPU $1C00-$1FFF (or $0C00-$0FFF): 1 KB switchable CHR bank

interface

uses
	NES.A12Watcher, NES.Cartridge,
	NES.Mapper, NES.Types;

type
	TMMC3Registers = (
		Reg8000 = $8000,
		Reg8001 = $8001,
		RegA000 = $A000,
		RegA001 = $A001,
		RegC000 = $C000,
		RegC001 = $C001,
		RegE000 = $E000,
		RegE001 = $E001
	);

	TMapper_004 = class(TMapper)
	private
		wramEnabled,
		wramWriteProtected: Boolean;

		A12Watcher: TA12Watcher;

	protected
		registers: array[0..7] of Byte;

		currentRegister: Byte;
		irqEnabled,
		irqReload:       Boolean;
		irqReloadValue,
		irqCounter:      Byte;
		chrMode,
		prgMode:         Byte;
		isMmc3RevA:      Boolean;

		State: record
			Reg8000,
			RegA000,
			RegA001: Byte;
		end;

		procedure InitMapper; override;
		procedure ResetMmc3;
		procedure WriteRegister(address: Word; value: Byte); override;
		procedure TriggerIrq; virtual;

		function  ForceMmc3RevAIrqs:  Boolean;  virtual;
		function  CanWriteToWorkRam:  Boolean;
		function  GetPRGPageSize:     Word;     override;
		function  GetCHRPageSize:     Word;     override;
		function  GetSaveRamSize:     Cardinal; override;
		function  GetSaveRamPageSize: Cardinal; override;

		procedure UpdateMirroring;  virtual;
		procedure UpdatePrgMapping; virtual;
		procedure UpdateChrMapping; virtual;
		procedure UpdateState; virtual;

	public
		procedure NotifyVRAMAddressChange(addr: Word); override;

		procedure LoadSnapshot; override;
		procedure SaveSnapshot; override;

		constructor Create(cartridge: TCartridge); override;
		destructor  Destroy; override;
	end;

	TMapper_004_MCACC = class(TMapper_004)
	private
		counter: Cardinal;
		prevAddr: Word;
	protected
		procedure WriteRegister(address: Word; value: Byte); override;
	public
		procedure NotifyVRAMAddressChange(addr: Word); override;

		constructor Create(cartridge: TCartridge); override;
	end;

	TMapper_004_ChrRam = class(TMapper_004)
	private
		FirstRamBank,
		LastRamBank,
		ChrRamSize: Word;
	protected
		function  GetChrRamPageSize: Word; override;
		function  GetChrRamSize: Cardinal; override;
		procedure SelectCHRPage(slot, page: Word; memoryType: TChrMemoryType = ChrDefault); override;
	public
		procedure Init(FirstBank, LastBank, ChrSize: Word);

		constructor Create(cartridge: TCartridge); override;
	end;

	TMapper_044 = class(TMapper_004)
	private
		_selectedBlock: Byte;
	protected
		procedure SelectCHRPage(slot, page: Word; memoryType: TChrMemoryType = ChrDefault); override;
		procedure SelectPRGPage(slot: Word; page: Int32; memoryType: TPrgMemoryType = TPrgMemoryType.PrgRom); override;
		procedure WriteRegister(address: Word; value: Byte); override;
	public
		procedure Reset(SoftReset: Boolean); override;

		constructor Create(cartridge: TCartridge); override;
	end;

	TMapper_045 = class(TMapper_004)
	private
		_regIndex: Byte;
		_reg: array[0..3] of Byte;

		procedure CheckReg; inline;
	protected
		function  RegisterStartAddress: Word; override;
		function  RegisterEndAddress: Word; override;
		procedure InitMapper; override;
		procedure SelectCHRPage(slot, page: Word; memoryType: TChrMemoryType = ChrDefault); override;
		procedure SelectPRGPage(slot: Word; page: Int32; memoryType: TPrgMemoryType = TPrgMemoryType.PrgRom); override;
		procedure WriteRegister(address: Word; value: Byte); override;
	public
		procedure Reset(SoftReset: Boolean); override;
		procedure LoadSnapshot; override;
		procedure SaveSnapshot; override;

		constructor Create(cartridge: TCartridge); override;
	end;

	TMapper_047 = class(TMapper_004)
	private
		selectedBlock: Byte;
	protected
		function RegisterStartAddress: Word; override;
		function RegisterEndAddress:   Word; override;

		procedure SelectCHRPage(slot, page: Word; memoryType: TChrMemoryType = ChrDefault); override;
		procedure SelectPRGPage(slot: Word; page: Int32; memoryType: TPrgMemoryType = TPrgMemoryType.PrgRom); override;
		procedure WriteRegister(address: Word; value: Byte); override;
	public
		procedure Reset(SoftReset: Boolean); override;

		constructor Create(cartridge: TCartridge); override;
	end;

	TMapper_049 = class(TMapper_004)
	private
		selectedBlock, prgReg: Byte;
		xprgMode: Boolean;
	protected
		function RegisterStartAddress: Word; override;
		function RegisterEndAddress:   Word; override;

		procedure SelectCHRPage(slot, page: Word; memoryType: TChrMemoryType = ChrDefault); override;
		procedure SelectPRGPage(slot: Word; page: Int32; memoryType: TPrgMemoryType = TPrgMemoryType.PrgRom); override;
		procedure WriteRegister(address: Word; value: Byte); override;
	public
		procedure Reset(SoftReset: Boolean); override;

		constructor Create(cartridge: TCartridge); override;
	end;

	TMapper_052 = class(TMapper_004)
	private
		ExtraReg: Byte;
	protected
		function  RegisterStartAddress: Word; override;
		function  RegisterEndAddress: Word; override;
		procedure SelectCHRPage(slot, page: Word; memoryType: TChrMemoryType = ChrDefault); override;
		procedure SelectPRGPage(slot: Word; page: Int32; memoryType: TPrgMemoryType = TPrgMemoryType.PrgRom); override;
		procedure WriteRegister(address: Word; value: Byte); override;
	public
		constructor Create(cartridge: TCartridge); override;
		procedure Reset(SoftReset: Boolean); override;
	end;

	TMapper_091 = class(TMapper_004)
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;
		function  RegisterStartAddress: Word; override;
		function  RegisterEndAddress: Word; override;

		procedure InitMapper; override;
		procedure UpdateState; override;
		procedure WriteRegister(address: Word; value: Byte); override;
	end;

	TMapper_114 = class(TMapper_004)
	const
		security: array[0..7] of Byte = ( 0,3,1,5,6,7,2,4 );
	private
		exRegs: array[0..1] of Byte;
	protected
		function  RegisterStartAddress: Word; override;

		procedure InitMapper; override;
		procedure UpdatePrgMapping; override;
		procedure WriteRegister(addr: Word; value: Byte); override;
	public
		constructor Create(cartridge: TCartridge); override;
	end;

	TMapper_115 = class(TMapper_004)
	private
		prgReg, chrReg,
		protectionReg: Byte;
	protected
		procedure InitMapper; override;
		procedure UpdateState; override;

		procedure SelectCHRPage(slot, page: Word; memoryType: TChrMemoryType = ChrDefault); override;

		function  AllowRegisterRead: Boolean; override;
		function  ReadRegister(addr: Word): Byte; override;
		procedure WriteRegister(addr: Word; value: Byte); override;
	public
		constructor Create(cartridge: TCartridge); override;
	end;

	TMapper_121 = class(TMapper_004)
	private
		exRegs: array[0..7] of Byte;
	protected
		procedure InitMapper; override;

		procedure SelectCHRPage(slot, page: Word; memoryType: TChrMemoryType = ChrDefault); override;
		procedure SelectPRGPage(slot: Word; page: Int32; memoryType: TPrgMemoryType = TPrgMemoryType.PrgRom); override;

		function  AllowRegisterRead: Boolean; override;
		function  ReadRegister(addr: Word): Byte; override;
		procedure WriteRegister(addr: Word; value: Byte); override;

		procedure UpdateExRegs;
	public
		procedure Reset(SoftReset: Boolean); override;

		constructor Create(cartridge: TCartridge); override;
	end;

	TMapper_187 = class(TMapper_004)
	private
		exRegs: array[0..1] of Byte;
	protected
		procedure InitMapper; override;
		procedure SelectCHRPage(slot, page: Word; memoryType: TChrMemoryType = ChrDefault); override;
		procedure SelectPRGPage(slot: Word; page: Int32; memoryType: TPrgMemoryType = TPrgMemoryType.PrgRom); override;

		function  AllowRegisterRead: Boolean; override;
		function  ReadRegister(addr: Word): Byte; override;
		procedure WriteRegister(addr: Word; value: Byte); override;
	public
		constructor Create(cartridge: TCartridge); override;
	end;

	TMapper_189 = class(TMapper_004)
	private
		prgReg: Byte;
	protected
		function  RegisterStartAddress: Word; override;

		procedure UpdateState; override;
		procedure WriteRegister(addr: Word; value: Byte); override;
	public
		constructor Create(cartridge: TCartridge); override;
	end;

	TMapper_245 = class(TMapper_004)
	protected
		procedure UpdateState; override;
		procedure UpdatePrgMapping; override;
	end;

	TMapper_250 = class(TMapper_004)
	protected
		procedure WriteRegister(addr: Word; value: Byte); override;
	end;

	TMapper_262 = class(TMapper_004)
	private
		exReg, resetSwitch: Byte;
	protected
		procedure InitMapper; override;
		procedure SelectCHRPage(slot, page: Word; memoryType: TChrMemoryType = ChrDefault); override;

		function  GetCHRRamPageSize: Word; override;
		function  GetCHRRamSize: Cardinal; override;
		function  AllowRegisterRead: Boolean; override;

		function  ReadRegister(addr: Word): Byte; override;
		procedure WriteRegister(addr: Word; value: Byte); override;
	public
		constructor Create(cartridge: TCartridge); override;
	end;


implementation

uses
	SysUtils, Math,
	NES.Config, NES.Console, NES.CPU, NES.PPU;

// ============================================================================
// TMapper_115
// ============================================================================

constructor TMapper_115.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(8, @prgReg);
	RegisterProperty(8, @chrReg);
end;

procedure TMapper_115.InitMapper;
begin
	AddRegisterRange($4100, $7FFF, moWrite);
	AddRegisterRange($5000, $5FFF, moRead);
	RemoveRegisterRange($8000, $FFFF, moRead);

	inherited InitMapper;
end;

procedure TMapper_115.UpdateState;
begin
	inherited UpdateState;

	if (prgReg and $80) <> 0 then
	begin
		if (prgReg and $20) <> 0 then
			SelectPrgPage4x(0, ((prgReg and $0F) shr 1) shl 2)
		else
		begin
			SelectPrgPage2x(0, (prgReg and $0F) shl 1);
			SelectPrgPage2x(1, (prgReg and $0F) shl 1);
		end;
	end;
end;

procedure TMapper_115.SelectCHRPage(slot, page: Word; memoryType: TChrMemoryType);
begin
	page := page or (chrReg shl 8);
	inherited SelectCHRPage(slot, page, memoryType);
end;

function TMapper_115.AllowRegisterRead: Boolean;
begin
	Result := True;
end;

function TMapper_115.ReadRegister(addr: Word): Byte;
begin
	Result := protectionReg;
end;

procedure TMapper_115.WriteRegister(addr: Word; value: Byte);
begin
	if addr < $8000 then
	begin
		if addr = $5080 then
			protectionReg := value
		else
		begin
			if Odd(addr) then
				chrReg := value and $01
			else
				prgReg := value;
			UpdateState;
		end;
	end
	else
		inherited WriteRegister(addr, value);
end;


// ============================================================================
// TMapper_004
// ============================================================================

constructor TMapper_004.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(8, @state.Reg8000);
	RegisterProperty(8, @state.RegA000);
	RegisterProperty(8, @state.RegA001);

	RegisterProperty(8,  @currentRegister);
	RegisterProperty(8,  @chrMode);
	RegisterProperty(8,  @prgMode);
	RegisterProperty(32, @irqReloadValue);
	RegisterProperty(32, @irqCounter);
	RegisterProperty(1,  @irqReload);
	RegisterProperty(1,  @irqEnabled);
	RegisterProperty(1,  @wramEnabled);
	RegisterProperty(1,  @wramWriteProtected);

	RegisterArray(Length(registers), @registers[0]);

	A12Watcher := TA12Watcher.Create(10);
end;

destructor TMapper_004.Destroy;
begin
	A12Watcher.Free;

	inherited Destroy;
end;

function TMapper_004.GetPRGPageSize: Word;
begin
	Result := $2000;
end;

function TMapper_004.GetCHRPageSize: Word;
begin
	Result := $0400;
end;

function TMapper_004.GetSaveRamSize: Cardinal;
begin
	Result := IfThen(RomInfo.SubMapperID = 1, $400, $2000);
end;

function TMapper_004.GetSaveRamPageSize: Cardinal;
begin
	Result := IfThen(RomInfo.SubMapperID = 1, $200, $2000);
end;

procedure TMapper_004.InitMapper;
begin
	// Force MMC3A irqs for boards that are known to use the A revision.
	// Some MMC3B boards also have the A behavior, but currently no way to tell them apart.
	isMmc3RevA :=
		(Configuration.Emulator.AltMMC3Behavior) or
		(RomInfo.DatabaseInfo.Chip.Contains('MMC3A'));

	ResetMmc3;
	SetCpuMemoryMapping($6000, $7FFF, 0,
		ChoosePrgMemoryType(HasBattery, PrgSaveRam, PrgWorkRam));
	UpdateState;
	UpdateMirroring;
end;

procedure TMapper_004.TriggerIrq;
begin
	NES_CPU.SetIrqSource(irqExternal);
end;

procedure TMapper_004.ResetMmc3;
begin
	state.Reg8000 := GetPowerOnByte;
	state.RegA000 := GetPowerOnByte;
	state.RegA001 := GetPowerOnByte;

	chrMode := GetPowerOnByte and 1;
	prgMode := GetPowerOnByte and 1;

	currentRegister := GetPowerOnByte;

	registers[0] := GetPowerOnByte(0);
	registers[1] := GetPowerOnByte(2);
	registers[2] := GetPowerOnByte(4);
	registers[3] := GetPowerOnByte(5);
	registers[4] := GetPowerOnByte(6);
	registers[5] := GetPowerOnByte(7);
	registers[6] := GetPowerOnByte(0);
	registers[7] := GetPowerOnByte(1);

	irqCounter     := GetPowerOnByte;
	irqReloadValue := GetPowerOnByte;
	irqReload      := (GetPowerOnByte and 1) <> 0;
	irqEnabled     := (GetPowerOnByte and 1) <> 0;

	wramEnabled        := (GetPowerOnByte and 1) <> 0;
	wramWriteProtected := (GetPowerOnByte and 1) <> 0;
end;

procedure TMapper_004.NotifyVRAMAddressChange(addr: Word);
var
	count: Int32;
begin
	if A12Watcher.UpdateVramAddress(addr, NES_PPU.GetFrameCycle) = scRise then
	begin
		count := irqCounter;

		if (irqCounter = 0) or (irqReload) then
			irqCounter := irqReloadValue
		else
			Dec(irqCounter);

		if ForceMmc3RevAIrqs {or Configuration.Emulator.AltMMC3Behavior} then
		begin
			// MMC3 Revision A behavior
			if (((count > 0) and (irqReloadValue > 0)) or (irqReload))
				and ((irqCounter = 0) and (irqEnabled)) then
					TriggerIrq;
		end
		else
		if (irqCounter = 0) and (irqEnabled) then
			TriggerIrq;

		irqReload := False;
	end;
end;

procedure TMapper_004.LoadSnapshot;
begin
	inherited LoadSnapshot;

	A12Watcher.LoadSnapshot;
end;

procedure TMapper_004.SaveSnapshot;
begin
	inherited SaveSnapshot;

	A12Watcher.SaveSnapshot;
end;

procedure TMapper_004.WriteRegister(address: Word; value: Byte);
begin
	case TMMC3Registers(address and $E001) of

		Reg8000: // Bank select ($8000-$9FFE, even)
		begin
			State.Reg8000 := value;
			UpdateState;
		end;

		Reg8001: // Bank Data ($8001-$9FFF, odd)
		begin
			// Writes to registers 0 and 1 always ignore bit 0
			if currentRegister <= 1 then
				value := value and (not $01);

			registers[currentRegister] := value;
			UpdateState;
		end;

		RegA000:
		begin
			state.RegA000 := value;
			UpdateMirroring;
		end;

		RegA001:
		begin
			State.RegA001 := value;
			UpdateState;
		end;

		RegC000:
			irqReloadValue := value;

		RegC001:
		begin
			irqCounter := 0;
			irqReload  := True;
		end;

		RegE000:
		begin
			irqEnabled := False;
			Console.CPU.ClearIRQSource(irqExternal);
		end;

		RegE001:
			irqEnabled := True;

	end;
end;

function TMapper_004.ForceMmc3RevAIrqs: Boolean;
begin
	Result := isMmc3RevA;
end;

procedure TMapper_004.UpdateMirroring;
begin
	if GetMirroringType <> MIRROR_FOURSCREENS then
	begin
		if Odd(State.RegA000) then
			SetMirroringType(MIRROR_HORIZONTAL)
		else
			SetMirroringType(MIRROR_VERTICAL);
	end;
end;

procedure TMapper_004.UpdateChrMapping;
begin
	if chrMode = 0 then
	begin
		SelectCHRPage(0, registers[0] and $FE);
		SelectCHRPage(1, registers[0] or  $01);
		SelectCHRPage(2, registers[1] and $FE);
		SelectCHRPage(3, registers[1] or  $01);

		SelectCHRPage(4, registers[2]);
		SelectCHRPage(5, registers[3]);
		SelectCHRPage(6, registers[4]);
		SelectCHRPage(7, registers[5]);
	end
	else
	if chrMode = 1 then
	begin
		SelectCHRPage(0, registers[2]);
		SelectCHRPage(1, registers[3]);
		SelectCHRPage(2, registers[4]);
		SelectCHRPage(3, registers[5]);

		SelectCHRPage(4, registers[0] and $FE);
		SelectCHRPage(5, registers[0] or  $01);
		SelectCHRPage(6, registers[1] and $FE);
		SelectCHRPage(7, registers[1] or  $01);
	end;
end;

procedure TMapper_004.UpdatePrgMapping;
begin
	if prgMode = 0 then
	begin
		SelectPRGPage(0, registers[6]);
		SelectPRGPage(1, registers[7]);
		SelectPRGPage(2, -2);
		SelectPRGPage(3, -1);
	end
	else
	if prgMode = 1 then
	begin
		SelectPRGPage(0, -2);
		SelectPRGPage(1, registers[7]);
		SelectPRGPage(2, registers[6]);
		SelectPRGPage(3, -1);
	end;
end;

function TMapper_004.CanWriteToWorkRam: Boolean;
begin
	Result := (wramEnabled = True) and (wramWriteProtected = False);
end;

procedure TMapper_004.UpdateState;
var
	i: Int32;
	access, FirstBankAccess, LastBankAccess: Byte;
	PrgAcc: TPrgMemoryType;
begin
	currentRegister := State.Reg8000 and $07;
	chrMode := (State.Reg8000 and $80) >> 7;
	prgMode := (State.Reg8000 and $40) >> 6;

	if (RomInfo.MapperID = 4) and (RomInfo.SubMapperID = 1) then
	begin
		wramEnabled := (state.Reg8000 and $20) = $20; //MMC6

		if wramEnabled then
		begin
			FirstBankAccess := IfThen((state.RegA001 and $10) = $10, 2{maWrite}, 0) or
				IfThen(state.RegA001 and $20 = $20, 1{maRead}, 0);
			LastBankAccess  := IfThen((state.RegA001 and $40) = $40, 2{maWrite}, 0) or
				IfThen(state.RegA001 and $80 = $80, 1{maRead}, 0);
		end
		else
		begin
			FirstBankAccess := 0; //TMemoryAccessType.NoAccess;
			LastBankAccess  := 0; //TMemoryAccessType.NoAccess;
		end;

		for i := 0 to 3 do
		begin
			SetCpuMemoryMapping($7000 + i * $400, $71FF + i * $400, 0, PrgSaveRam, FirstBankAccess);
			SetCpuMemoryMapping($7200 + i * $400, $73FF + i * $400, 1, PrgSaveRam, LastBankAccess);
		end;
	end
	else
	begin
		wramEnabled        := (State.RegA001 and $80) = $80;
		wramWriteProtected := (State.RegA001 and $40) = $40;
		if RomInfo.SubMapperID = 0 then
		begin
			if wramEnabled then
				access := IfThen( CanWriteToWorkRam, 3{ReadWrite}, 1{Read} )
			else
				access := 0; //NoAccess;
			if HasBattery then
				PrgAcc := PrgSaveRam
			else
				PrgAcc := PrgWorkRam;
			SetCpuMemoryMapping($6000, $7FFF, 0, PrgAcc, access);
		end;
	end;

	UpdatePrgMapping;
	UpdateChrMapping;
end;

// ============================================================================
// TMapper_004_MCACC
// ============================================================================

constructor TMapper_004_MCACC.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(32, @counter);
	RegisterProperty(16, @prevAddr);
end;

procedure TMapper_004_MCACC.WriteRegister(address: Word; value: Byte);
begin
	// "Writing to $C001 resets pulse counter."
	if (address and $E001) = $C001 then
		counter := 0;
	inherited WriteRegister(address, value);
end;

procedure TMapper_004_MCACC.NotifyVRAMAddressChange(addr: Word);
begin
	if ((addr and $1000) = 0) and ((prevAddr and $1000) <> 0) then
	begin
		Inc(counter);
		if counter = 1 then
		begin
			// "Counter clocking happens once per 8 A12 cycles at first cycle"
			if (irqCounter = 0) or (irqReload) then
				irqCounter := irqReloadValue
			else
				Dec(irqCounter);

			if (irqCounter = 0) and (irqEnabled) then
				TriggerIrq;

			irqReload := False;
		end
		else
		if counter = 8 then
			counter := 0;
	end;
	prevAddr := addr;
end;

// ============================================================================
// TMapper_004_ChrRam
// ============================================================================

constructor TMapper_004_ChrRam.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(16, @FirstRamBank);
	RegisterProperty(16, @LastRamBank);
	RegisterProperty(16, @ChrRamSize);

	if Assigned(cartridge) then
	case cartridge.nMapperID of
		074: Init($08, $09, 2);
		119: Init($40, $7F, 8);
		191: Init($80, $FF, 2);
		192: Init($08, $0B, 4);
		194: Init($00, $01, 2);
		195: Init($00, $03, 4);
	end;
end;

procedure TMapper_004_ChrRam.Init(FirstBank, LastBank, ChrSize: Word);
begin
	FirstRamBank := FirstBank;
	LastRamBank := LastBank;
	ChrRamSize := ChrSize;
end;

function TMapper_004_ChrRam.GetChrRamPageSize: Word;
begin
	Result := $400;
end;

function TMapper_004_ChrRam.GetChrRamSize: Cardinal;
begin
	Result := ChrRamSize * $400;
end;

procedure TMapper_004_ChrRam.SelectCHRPage(slot, page: Word; memoryType: TChrMemoryType);
begin
	if (page >= FirstRamBank) and (page <= LastRamBank) then
	begin
		memoryType := TChrMemoryType.ChrRam;
		Dec(page, FirstRamBank);
	end;
	inherited SelectCHRPage(slot, page, memoryType);
end;

// ============================================================================
// TMapper_044
// ============================================================================

procedure TMapper_044.SelectCHRPage(slot, page: Word; memoryType: TChrMemoryType);
begin
	page := page and IfThen(_selectedBlock <= 5, $7F, $FF);
	page := page or (_selectedBlock * $80);
	inherited SelectCHRPage(slot, page, memoryType);
end;

procedure TMapper_044.SelectPRGPage(slot: Word; page: Int32; memoryType: TPrgMemoryType);
begin
	page := page and IfThen(_selectedBlock <= 5, $0F, $1F);
	page := page or (_selectedBlock * $10);
	inherited SelectPRGPage(slot, page, memoryType);
end;

procedure TMapper_044.Reset(SoftReset: Boolean);
begin
	_selectedBlock := 0;
	UpdateState;
end;

constructor TMapper_044.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);
	RegisterProperty(8, @_selectedBlock);
end;

procedure TMapper_044.WriteRegister(address: Word; value: Byte);
begin
	if (address and $E001) = $A001 then
	begin
		_selectedBlock := value and $07;
		if _selectedBlock = 7 then
			_selectedBlock := 6;
	end;
	inherited WriteRegister(address, value);
end;

// ============================================================================
// TMapper_045
// ============================================================================

function TMapper_045.RegisterStartAddress: Word; begin Result := $8000; end;
function TMapper_045.RegisterEndAddress:   Word; begin Result := $FFFF; end;

constructor TMapper_045.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);
	RegisterProperty(8, @_regIndex);
	RegisterArray(Length(_reg), @_reg[0]);
end;

procedure TMapper_045.InitMapper;
begin
	inherited;

	// Needed by Famicom Yarou Vol 1
	// - Game apparently writes to CHR RAM before initializing the registers
	registers[0] := 0;
	registers[1] := 2;
	registers[2] := 4;
	registers[3] := 5;
	registers[4] := 6;
	registers[5] := 7;

	UpdateChrMapping;
	Reset(True); // seems required for some multicarts
end;

procedure TMapper_045.Reset(SoftReset: Boolean);
begin
	AddRegisterRange($6000, $7FFF);
	_regIndex := 0;
	FillByte(_reg[0], SizeOf(_reg), 0);
	_reg[2] := $0F;
	UpdateState;
end;

procedure TMapper_045.LoadSnapshot; begin inherited; CheckReg; end;
procedure TMapper_045.SaveSnapshot; begin inherited; CheckReg; end;

procedure TMapper_045.SelectCHRPage(slot, page: Word; memoryType: TChrMemoryType);
begin
	if not HasChrRam then
	begin
		page := page and ( $FF >> ($0F - (_reg[2] and $0F)) );
		page := page or ( _reg[0] or ((_reg[2] and $F0) << 4) );
	end;
	inherited SelectCHRPage(slot, page, memoryType);
end;

procedure TMapper_045.SelectPRGPage(slot: Word; page: Int32; memoryType: TPrgMemoryType);
begin
	page := page and ($3F xor (_reg[3] and $3F));
	page := page or _reg[1];
	inherited SelectPRGPage(slot, page, memoryType);
end;

procedure TMapper_045.CheckReg;
begin
	if (_reg[3] and $40) <> 0 then
		RemoveRegisterRange($6000, $7FFF);
end;

procedure TMapper_045.WriteRegister(address: Word; value: Byte);
begin
	if address < $8000 then
	begin
		if (_reg[3] and $40) = 0 then
		begin
			_reg[_regIndex] := value;
			_regIndex := (_regIndex + 1) and $03;
		end;

		CheckReg;
		UpdateState;
	end
	else
		inherited WriteRegister(address, value);
end;

// ============================================================================
// TMapper_047
// ============================================================================

function TMapper_047.RegisterStartAddress: Word; begin Result := $6000; end;
function TMapper_047.RegisterEndAddress:   Word; begin Result := $FFFF; end;

constructor TMapper_047.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(8, @selectedBlock);
end;

procedure TMapper_047.SelectCHRPage(slot, page: Word; memoryType: TChrMemoryType);
begin
	page := page and $7F;
	if selectedBlock = 1 then
		page := page or $80;

	inherited SelectCHRPage(slot, page, memoryType);
end;

procedure TMapper_047.SelectPRGPage(slot: Word; page: Int32; memoryType: TPrgMemoryType);
begin
	page := page and $0F;
	if selectedBlock = 1 then
		page := page or $10;

	inherited SelectPRGPage(slot, page, memoryType);
end;

procedure TMapper_047.WriteRegister(address: Word; value: Byte);
begin
	if address < $8000 then
	begin
		if CanWriteToWorkRam then
		begin
			selectedBlock := value and 1;
			UpdateState;
		end;
	end
	else
		inherited WriteRegister(address, value);
end;

procedure TMapper_047.Reset(SoftReset: Boolean);
begin
	selectedBlock := 0;
	UpdateState;
end;

// ============================================================================
// TMapper_049
// ============================================================================

function TMapper_049.RegisterStartAddress: Word; begin Result := $6000; end;
function TMapper_049.RegisterEndAddress:   Word; begin Result := $FFFF; end;

constructor TMapper_049.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(8, @selectedBlock);
	RegisterProperty(8, @prgReg);
	RegisterProperty(1, @xprgMode);
end;

procedure TMapper_049.Reset(SoftReset: Boolean);
begin
	selectedBlock := 0;
	prgReg := 0;
	xprgMode := False;
	UpdateState;
end;

procedure TMapper_049.SelectCHRPage(slot, page: Word; memoryType: TChrMemoryType);
begin
	page := (page and $7F) or ($80 * selectedBlock);

	inherited SelectCHRPage(slot, page, memoryType);
end;

procedure TMapper_049.SelectPRGPage(slot: Word; page: Int32; memoryType: TPrgMemoryType);
begin
	if xprgMode then
		page := (page and $0F) or ($10 * selectedBlock)
	else
		page := prgReg * 4 + slot;

	inherited SelectPRGPage(slot, page, memoryType);
end;

procedure TMapper_049.WriteRegister(address: Word; value: Byte);
begin
	if address < $8000 then
	begin
		if CanWriteToWorkRam then
		begin
			selectedBlock := (value shr 6) and $03;
			prgReg := (value shr 4) and $03;
			xprgMode := value and $01 <> 0;
			UpdateState;
		end;
	end
	else
		inherited WriteRegister(address, value);
end;

// ============================================================================
// TMapper_052
// ============================================================================

constructor TMapper_052.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(8, @ExtraReg);

	ExtraReg := 0;
end;

procedure TMapper_052.Reset(SoftReset: Boolean);
begin
	inherited;
	ExtraReg := 0;
	UpdateState;
end;

function TMapper_052.RegisterStartAddress: Word;
begin
	Result := $6000;
end;

function TMapper_052.RegisterEndAddress: Word;
begin
	Result := $FFFF;
end;

procedure TMapper_052.SelectCHRPage(slot, page: Word; memoryType: TChrMemoryType);
var
	E, W: Word;
begin
	E := ExtraReg;
	W := ((E and $20) >> 3) or ((E and $10) >> 4);

	if (E and $40) <> 0 then
	begin
		page := page and $7F;
		W := W or ((E and $04) >> 1);
	end
	else
		page := page and $FF;

	page := page or (W << 7);

	inherited SelectCHRPage(slot, page, memoryType);
end;

procedure TMapper_052.SelectPRGPage(slot: Word; page: Int32; memoryType: TPrgMemoryType);
begin
	if (ExtraReg and $08) <> 0 then
	begin
		page := page and $0F;
		page := page or (Word(ExtraReg and $07) << 4);
	end
	else
	begin
		page := page and $1F;
		page := page or (Word(ExtraReg and $06) << 4);
	end;

	inherited SelectPRGPage(slot, page, memoryType);
end;

procedure TMapper_052.WriteRegister(address: Word; value: Byte);
begin
	if address < $8000 then
	begin
		if CanWriteToWorkRam then
		begin
			if (ExtraReg and $80) = 0 then
			begin
				ExtraReg := value;
				UpdateState;
			end
			else
				inherited WritePrgRam(address, value);
		end;
	end
	else
		inherited WriteRegister(address, value);
end;

// ============================================================================
// TMapper_091
// ============================================================================

function TMapper_091.GetPRGPageSize: Word; begin Result := $2000; end;
function TMapper_091.GetCHRPageSize: Word; begin Result := $800;  end;
function TMapper_091.RegisterStartAddress: Word; begin Result := $6000; end;
function TMapper_091.RegisterEndAddress:   Word; begin Result := $7FFF; end;

procedure TMapper_091.InitMapper;
begin
	SelectPRGPage(2, -2);
	SelectPRGPage(3, -1);
end;

procedure TMapper_091.UpdateState;
begin
	// Do nothing, we are only using MMC3 code to emulate the IRQs
end;

procedure TMapper_091.WriteRegister(address: Word; value: Byte);
begin
	case (address and $7003) of
		$6000: SelectCHRPage(0, value);
		$6001: SelectCHRPage(1, value);
		$6002: SelectCHRPage(2, value);
		$6003: SelectCHRPage(3, value);
		$7000: SelectPRGPage(0, value and $0F);
		$7001: SelectPRGPage(1, value and $0F);
		$7002: inherited WriteRegister($E000, value);
		$7003: begin
			inherited WriteRegister($C000, $07);
			inherited WriteRegister($C001, value);
			inherited WriteRegister($E001, value);
		end;
	end;
end;

// ============================================================================
// TMapper_114
// ============================================================================

constructor TMapper_114.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterArray(Length(exRegs), @exRegs[0]);
end;

function TMapper_114.RegisterStartAddress: Word;
begin
	Result := $5000;
end;

procedure TMapper_114.InitMapper;
begin
	inherited;

	exRegs[0] := 0;
	exRegs[1] := 0;
	isMmc3RevA := True;
end;

procedure TMapper_114.UpdatePrgMapping;
begin
	if (exRegs[0] and $80) <> 0 then
	begin
		SelectPrgPage2x(0, (exRegs[0] and $0F) << 1);
		SelectPrgPage2x(1, (exRegs[0] and $0F) << 1);
	end
	else
		inherited;
end;

procedure TMapper_114.WriteRegister(addr: Word; value: Byte);
begin
	if addr < $8000 then
	begin
		exRegs[0] := value;
		UpdatePrgMapping;
	end
	else
	case (addr and $E001) of

		$8001:
			inherited WriteRegister($A000, value);

		$A000:
		begin
			inherited WriteRegister($8000, (value and $C0) or security[value and $07]);
			exRegs[1] := 1;
		end;

		$A001:
			irqReloadValue := value;

		$C000:
			if exRegs[1] <> 0 then
			begin
				exRegs[1] := 0;
				inherited WriteRegister($8001, value);
			end;

		$C001:
			irqReload := True;

		$E000:
		begin
			NES_CPU.ClearIrqSource(irqExternal);
			irqEnabled := False;
		end;

		$E001:
			irqEnabled := True;
	end;
end;

// ============================================================================
// TMapper_121
// ============================================================================

constructor TMapper_121.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterArray(Length(exRegs), @exRegs[0]);
end;

procedure TMapper_121.Reset(SoftReset: Boolean);
begin
	FillByte(exRegs[0], SizeOf(exRegs), 0);
	exRegs[3] := $80;
end;

procedure TMapper_121.InitMapper;
begin
	inherited InitMapper;

	AddRegisterRange($5000, $5FFF, moAny);
	RemoveRegisterRange($8000, $FFFF, moRead);
end;

procedure TMapper_121.SelectPRGPage(slot: Word; page: Int32; memoryType: TPrgMemoryType);
var
	orValue: Byte;
begin
	orValue := (exRegs[3] and $80) shr 2;
	if (exRegs[5] and $3F) <> 0 then
	begin
		inherited SelectPRGPage(slot, (page and $1F) or orValue, memoryType);
		inherited SelectPRGPage(1, exRegs[2] or orValue, memoryType);
		inherited SelectPRGPage(2, exRegs[1] or orValue, memoryType);
		inherited SelectPRGPage(3, exRegs[0] or orValue, memoryType);
	end else
		inherited SelectPRGPage(slot, (page and $1F) or orValue, memoryType);
end;

procedure TMapper_121.SelectCHRPage(slot, page: Word; memoryType: TChrMemoryType);
begin
	if prgSize = chrRomSize then // Hack for Super 3-in-1
		inherited SelectCHRPage(slot, page or ((exRegs[3] and $80) shl 1), memoryType)
	else
	begin
		if ((slot < 4) and (chrMode = 0)) or ((slot >= 4) and (chrMode = 1)) then
			page := page or $100;
		inherited SelectCHRPage(slot, page, memoryType);
	end;
end;

function TMapper_121.AllowRegisterRead: Boolean;
begin
	Result := True;
end;

function TMapper_121.ReadRegister(addr: Word): Byte;
begin
	Result := exRegs[4];
end;

procedure TMapper_121.WriteRegister(addr: Word; value: Byte);
const
	lookup: array [0..3] of Byte = ( $83, $83, $42, $00 );
begin
	if addr < $8000 then
	begin
		// $5000-$5FFF
		exRegs[4] := lookup[value and $03];
		if (addr and $5180) = $5180 then
		begin
			// Hack for Super 3-in-1
			exRegs[3] := value;
			UpdateState;
		end;
	end
	else
	if addr < $A000 then
	begin
		// $8000-$9FFF
		if (addr and $03) = $03 then
		begin
			exRegs[5] := value;
			UpdateExRegs;
			inherited WriteRegister($8000, value);
		end
		else
		if (addr and $01) = $01 then
		begin
			exRegs[6] := ((value and $01) shl 5) or ((value and $02) shl 3) or
				((value and $04) shl 1) or ((value and $08) shr 1) or
				((value and $10) shr 3) or ((value and $20) shr 5);
			if exRegs[7] = 0 then
				UpdateExRegs;
			inherited WriteRegister($8001, value);
		end
		else
			inherited WriteRegister($8000, value);
	end
	else
		inherited WriteRegister(addr, value);
end;

procedure TMapper_121.UpdateExRegs;
begin
	case (exRegs[5] and $3F) of
		$20: begin exRegs[7] := 1; exRegs[0] := exRegs[6]; end;
		$29: begin exRegs[7] := 1; exRegs[0] := exRegs[6]; end;
		$26: begin exRegs[7] := 0; exRegs[0] := exRegs[6]; end;
		$2B: begin exRegs[7] := 1; exRegs[0] := exRegs[6]; end;
		$2C: begin exRegs[7] := 1; if exRegs[6] <> 0 then exRegs[0] := exRegs[6]; end;
		$3C,
		$3F: begin exRegs[7] := 1; exRegs[0] := exRegs[6]; end;
		$28: begin exRegs[7] := 0; exRegs[1] := exRegs[6]; end;
		$2A: begin exRegs[7] := 0; exRegs[2] := exRegs[6]; end;
		$2F: ;
		else exRegs[5] := 0;
	end;
end;

// ============================================================================
// TMapper_187
// ============================================================================

constructor TMapper_187.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterArray(Length(exRegs), @exRegs[0]);
end;

procedure TMapper_187.InitMapper;
begin
	inherited;

	exRegs[0] := 0;
	exRegs[1] := 0;

	AddRegisterRange($5000, $5FFF, TMemoryOperation.moAny);
	AddRegisterRange($6000, $6FFF, TMemoryOperation.moWrite);
	RemoveRegisterRange($8000, $FFFF, TMemoryOperation.moRead);
end;

procedure TMapper_187.SelectCHRPage(slot, page: Word; memoryType: TChrMemoryType);
begin
	if ((chrMode <> 0) and (slot >= 4)) or ((chrMode = 0) and (slot < 4)) then
		page := page or $100;
	inherited SelectCHRPage(slot, page, memoryType);
end;

procedure TMapper_187.SelectPRGPage(slot: Word; page: Int32; memoryType: TPrgMemoryType);
begin
	if (exRegs[0] and $80) = 0 then
		inherited SelectPRGPage(slot, page and $3F, memoryType)
	else
	begin
		page := exRegs[0] and $1F;

		if (exRegs[0] and $20) <> 0 then
		begin
			if (exRegs[0] and $40) <> 0 then
			begin
				page := page and $FC;
				inherited SelectPRGPage(0, page + 0);
				inherited SelectPRGPage(1, page + 1);
				inherited SelectPRGPage(2, page + 2);
				inherited SelectPRGPage(3, page + 3);
			end
			else
			begin
				page := (page and $FE) shl 1;
				inherited SelectPRGPage(0, page + 0);
				inherited SelectPRGPage(1, page + 1);
				inherited SelectPRGPage(2, page + 2);
				inherited SelectPRGPage(3, page + 3);
			end;
		end
		else
		begin
			page := page shl 1;
			inherited SelectPRGPage(0, page + 0);
			inherited SelectPRGPage(1, page + 1);
			inherited SelectPRGPage(2, page + 0);
			inherited SelectPRGPage(3, page + 1);
		end;
	end;
end;

function TMapper_187.AllowRegisterRead: Boolean;
begin
	Result := True;
end;

function TMapper_187.ReadRegister(addr: Word): Byte;
const
	security: array [0..3] of Byte = ( $83, $83, $42, $00 );
begin
	Result := security[exRegs[1] and $03];
end;

procedure TMapper_187.WriteRegister(addr: Word; value: Byte);
begin
	if addr < $8000 then
	begin
		if (addr = $5000) or (addr = $6000) then
		begin
			exRegs[0] := value;
			UpdatePrgMapping;
		end;
	end
	else
	begin
		if addr = $8000 then
		begin
			exRegs[1] := 1;
			inherited WriteRegister(addr, value);
		end
		else
		if addr = $8001 then
		begin
			if exRegs[1] = 1 then
				inherited WriteRegister(addr, value);
		end
		else
			inherited WriteRegister(addr, value);
	end;
end;

// ============================================================================
// TMapper_189
// ============================================================================

function TMapper_189.RegisterStartAddress: Word;
begin
	Result := $4120;
end;

constructor TMapper_189.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(8, @prgReg);
	prgReg := 0;
end;

procedure TMapper_189.UpdateState;
var
	prgPage: Byte;
begin
	inherited UpdateState;

	// "$4120-7FFF:  [AAAA BBBB]"
	// " 'A' and 'B' bits of the $4120 reg seem to be effectively OR'd."
	prgPage := (((prgReg) or (prgReg >> 4)) and $07) * 4;
	SelectPRGPage(0, prgPage+0);
	SelectPRGPage(1, prgPage+1);
	SelectPRGPage(2, prgPage+2);
	SelectPRGPage(3, prgPage+3);
end;

procedure TMapper_189.WriteRegister(addr: Word; value: Byte);
begin
	if addr <= $7FFF then
	begin
		prgReg := value;
		UpdateState;
	end
	else
		inherited WriteRegister(addr, value);
end;

// ============================================================================
// TMapper_245
// ============================================================================

procedure TMapper_245.UpdateState;
begin
	inherited;

	if HasChrRam then
	begin
		if chrMode <> 0 then
		begin
			SelectChrPage4x(0, 4);
			SelectChrPage4x(1, 0);
		end
		else
		begin
			SelectChrPage4x(0, 0);
			SelectChrPage4x(1, 4);
		end;
	end;
end;

procedure TMapper_245.UpdatePrgMapping;
var
	orValue: Byte;
	lastPageInBlock: Word;
begin
	orValue := IfThen((registers[0] and $02) <> 0, $40, $00);
	registers[6] := (registers[6] and $3F) or orValue;
	registers[7] := (registers[7] and $3F) or orValue;
	{$R-}lastPageInBlock := IfThen(GetPRGPageCount >= $40, $3F or orValue, -1);
	if prgMode = 0 then
	begin
		SelectPRGPage(0, registers[6]);
		SelectPRGPage(1, registers[7]);
		SelectPRGPage(2, lastPageInBlock - 1);
		SelectPRGPage(3, lastPageInBlock);
	end
	else
	if prgMode = 1 then
	begin
		SelectPRGPage(0, lastPageInBlock - 1);
		SelectPRGPage(1, registers[7]);
		SelectPRGPage(2, registers[6]);
		SelectPRGPage(3, lastPageInBlock);
	end;
end;

// ============================================================================
// TMapper_250
// ============================================================================

procedure TMapper_250.WriteRegister(addr: Word; value: Byte);
begin
	inherited WriteRegister((addr and $E000) or ((addr and $0400) shr 10), addr and $FF);
end;

// ============================================================================
// TMapper_262
// ============================================================================

constructor TMapper_262.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(8, @exReg);
	RegisterProperty(8, @resetSwitch);
end;

function TMapper_262.GetCHRRamSize: Cardinal; begin Result := $2000; end;
function TMapper_262.GetCHRRamPageSize: Word; begin Result := $2000; end;
function TMapper_262.AllowRegisterRead: Boolean; begin Result := True; end;

procedure TMapper_262.InitMapper;
begin
	exReg := 0;
	resetSwitch := 0;

	inherited InitMapper;

	AddRegisterRange($4100, $4100, moAny);
	RemoveRegisterRange($8000, $FFFF, moRead);
end;

procedure TMapper_262.SelectCHRPage(slot, page: Word; memoryType: TChrMemoryType);
begin
	if (exReg and $40) <> 0 then
		inherited SelectCHRPage(0, 0, TChrMemoryType.ChrRam)
	else
	case slot of
		0,1: inherited SelectCHRPage(slot, page or ((exReg and $08) shl 5));
		2,3: inherited SelectCHRPage(slot, page or ((exReg and $04) shl 6));
		4,5: inherited SelectCHRPage(slot, page or ((exReg and $01) shl 8));
		else inherited SelectCHRPage(slot, page or ((exReg and $02) shl 7));
	end;
end;

function TMapper_262.ReadRegister(addr: Word): Byte;
begin
	Result := resetSwitch;
end;

procedure TMapper_262.WriteRegister(addr: Word; value: Byte);
begin
	if addr = $4100 then
	begin
		exReg := value;
		UpdateState;
	end
	else
		inherited;
end;

end.

