unit NES.Mapper_083;

// Mapper 083: Cony/Yoko

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper;

type
	TMapper_083 = class(TMapper)
	private
		regs:   array [0..10] of Byte;
		exRegs: array [0..3]  of Byte;

		mode, bank: Byte;
		irqCounter: Word;
		irqEnabled,
		is2kBank, isNot2kBank: Boolean;

	protected
		procedure UpdateState;

		function  GetDipSwitchCount: Cardinal; override;
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;
		function  AllowRegisterRead: Boolean; override;

		function  ReadRegister(addr: Word): Byte; override;
		procedure WriteRegister(addr: Word;  value: Byte); override;

		procedure InitMapper; override;

	public
		procedure ProcessCpuClock; override;

		constructor Create(cartridge: TCartridge); override;
	end;


implementation

uses
	Basement.Util, NES.Console, NES.CPU;

{ TMapper_083 }

function TMapper_083.GetDipSwitchCount: Cardinal; begin Result := 2;     end;
function TMapper_083.GetPRGPageSize: Word;        begin Result := $2000; end;
function TMapper_083.GetCHRPageSize: Word;        begin Result := $400;  end;
function TMapper_083.AllowRegisterRead: Boolean;  begin Result := True;  end;

constructor TMapper_083.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterArray(Length(regs),   @regs[0]);
	RegisterArray(Length(exRegs), @exRegs[0]);

	RegisterProperty(1, @is2kBank);
	RegisterProperty(1, @isNot2kBank);
	RegisterProperty(8, @mode);
	RegisterProperty(8, @bank);
	RegisterProperty(16,@irqCounter);
	RegisterProperty(1, @irqEnabled);
end;

procedure TMapper_083.InitMapper;
begin
	is2kBank := false;
	isNot2kBank := false;
	mode := 0;
	bank := 0;
	irqCounter := 0;
	irqEnabled := False;

	ClearArray(regs);
	ClearArray(exRegs);

	AddRegisterRange($5000, $5000, TMemoryOperation.moRead);
	AddRegisterRange($5100, $5103, TMemoryOperation.moAny);
	RemoveRegisterRange($8000, $FFFF, TMemoryOperation.moRead);

	UpdateState;
end;

function TMapper_083.ReadRegister(addr: Word): Byte;
begin
	if addr = $5000 then
		Result := (Console.MemoryManager.GetOpenBus and $FC) or GetDipSwitches
	else
		Result := exRegs[addr and $03];
end;

procedure TMapper_083.WriteRegister(addr: Word; value: Byte);
begin
	if addr < $8000 then
		exRegs[addr and $03] := value
	else
	if (addr >= $8300) and (addr <= $8302) then
	begin
		mode := mode and $BF;
		regs[addr - $8300 + 8] := value;
		UpdateState;
	end
	else
	if (addr >= $8310) and (addr <= $8317) then
	begin
		regs[addr - $8310] := value;
		if (addr >= $8312) and (addr <= $8315) then
			isNot2kBank := True;
		UpdateState;
	end
	else
	case addr of

		$8000,
		$B000, $B0FF, $B1FF: // Dragon Ball Z Party [p1] BMC
		begin
			if addr = $8000 then is2kBank := True;
			bank := value;
			mode := mode or $40;
			UpdateState;
		end;

		$8100:
		begin
			mode := value or (mode and $40);
			UpdateState;
		end;

		$8200:
		begin
			irqCounter := (irqCounter and $FF00) or value;
			CPUClearIRQSource(irqExternal);
		end;

		$8201:
		begin
			irqEnabled := (mode and $80) = $80;
			irqCounter := (irqCounter and $FF) or (value << 8);
		end;

	end;
end;

procedure TMapper_083.UpdateState;
var
	i: Integer;
begin
	GenericChooseMirroringType(mode);

	if (is2kBank) and (not isNot2kBank) then
	begin
		SelectChrPage2x(0, regs[0] << 1);
		SelectChrPage2x(1, regs[1] << 1);
		SelectChrPage2x(2, regs[6] << 1);
		SelectChrPage2x(3, regs[7] << 1);
	end
	else
	for i := 0 to 7 do
		SelectCHRPage(i, regs[i] or ((bank and $30) << 4));

	if (mode and $40) <> 0 then
	begin
		SelectPrgPage2x(0, (bank and $3F) << 1);
		SelectPrgPage2x(1, ((bank and $30) or $0F) << 1);
	end
	else
	begin
		SelectPRGPage(0, regs[8]);
		SelectPRGPage(1, regs[9]);
		SelectPRGPage(2, regs[10]);
		SelectPRGPage(3, -1);
	end;
end;

procedure TMapper_083.ProcessCpuClock;
begin
	if not irqEnabled then Exit;

	{$R-}Dec(irqCounter);
	if irqCounter = 0 then
	begin
		irqEnabled := False;
		irqCounter := $FFFF;
		CPUSetIRQSource(irqExternal);
	end;
end;

end.

