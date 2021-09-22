unit NES.Mapper_168;

// Mapper 168: Racermate

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper;

type
	TMapper_168 = class(TMapper)
	private
		irqCounter: Word;
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;
		function  GetChrRamSize:  Cardinal; override;
		function  GetSaveRamSize: Cardinal; override;
		function  ForceChrBattery:   Boolean; override;

		procedure WriteRegister(addr: Word;  value: Byte); override;

		procedure InitMapper; override;
	public
		procedure ProcessCpuClock; override;

		constructor Create(cartridge: TCartridge); override;
	end;


implementation

uses NES.CPU;

{ TMapper_168 }

function TMapper_168.GetPRGPageSize: Word; begin Result := $4000; end;
function TMapper_168.GetCHRPageSize: Word; begin Result := $1000; end;
function TMapper_168.GetChrRamSize:  Cardinal; begin Result := $10000; end;
function TMapper_168.GetSaveRamSize: Cardinal; begin Result := 0; end;
function TMapper_168.ForceChrBattery: Boolean; begin Result := True; end;


constructor TMapper_168.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(16, @irqCounter);
end;

procedure TMapper_168.InitMapper;
begin
	irqCounter := 0;

	SelectPRGPage(1, -1);
	SelectCHRPage(0, 0);
end;

procedure TMapper_168.ProcessCpuClock;
begin
	{$R-}Dec(irqCounter);
	if irqCounter = 0 then
	begin
		irqCounter := 1024;
		CPUSetIRQSource(irqExternal);
	end;
end;

procedure TMapper_168.WriteRegister(addr: Word; value: Byte);
begin
	case (addr and $C000) of

		$8000:
		begin
			SelectPRGPage(0, (value shr 6) and $03);
			SelectCHRPage(1, value and $0F);
		end;

		$C000:
		begin
			irqCounter := 1024;
			CPUClearIRQSource(irqExternal);
		end;

	end;
end;

end.
