unit NES.Mapper_040;

// Mapper 040: NTDEC 2722 (SMB2J)

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper;

type
	TMapper_040 = class(TMapper)
	private
		irqCounter: Word;
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;

		procedure InitMapper; override;
		procedure WriteRegister(addr: Word;  value: Byte); override;
	public
		procedure ProcessCpuClock; override;

		constructor Create(cartridge: TCartridge); override;
	end;


implementation

uses
	NES.CPU;

{ TMapper_040 }

function TMapper_040.GetPRGPageSize: Word;
begin
	Result := $2000;
end;

function TMapper_040.GetCHRPageSize: Word;
begin
	Result := $2000;
end;

constructor TMapper_040.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(16, @irqCounter);
end;

procedure TMapper_040.InitMapper;
begin
	irqCounter := 0;
	SetCpuMemoryMapping($6000, $7FFF, 6, TPrgMemoryType.PrgRom);
	SelectPRGPage(0, 4);
	SelectPRGPage(1, 5);
	SelectPRGPage(3, 7);
	SelectCHRPage(0, 0);
end;

procedure TMapper_040.ProcessCpuClock;
begin
	if irqCounter > 0 then
	begin
		{$R-}Dec(irqCounter);
		if irqCounter = 0 then
			CPUSetIRQSource(irqExternal);
	end;
end;

procedure TMapper_040.WriteRegister(addr: Word; value: Byte);
begin
	case (addr and $E000) of
		$8000:
		begin
			irqCounter := 0;
			CPUClearIRQSource(irqExternal);
		end;
		$A000: irqCounter := 4096;
		$E000: SelectPRGPage(2, value);
	end;
end;

end.
