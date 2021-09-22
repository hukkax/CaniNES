unit NES.Mapper_065;

// Mapper 065: Irem H3001

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper;

type
	TMapper_065 = class(TMapper)
	private
		irqReloadValue,
		irqCounter:  Word;
		irqEnabled:  Boolean;

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

uses NES.CPU;

{ TMapper_065 }

function TMapper_065.GetPRGPageSize: Word;
begin
	Result := $2000;
end;

function TMapper_065.GetCHRPageSize: Word;
begin
	Result := $400;
end;

constructor TMapper_065.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(1, @irqEnabled);
	RegisterProperty(16, @irqCounter);
	RegisterProperty(16, @irqReloadValue);
end;

procedure TMapper_065.InitMapper;
begin
	SelectPRGPage(0, 0);
	SelectPRGPage(1, 1);
	SelectPRGPage(2, $FE);
	SelectPRGPage(3, -1);
end;

procedure TMapper_065.ProcessCpuClock;
begin
	if irqEnabled then
	begin
		{$R-}Dec(irqCounter);
		if irqCounter = 0 then
		begin
			irqEnabled := False;
			CPUSetIRQSource(irqExternal);
		end;
	end;
end;

procedure TMapper_065.WriteRegister(addr: Word; value: Byte);
begin
	case addr of

		$8000: SelectPRGPage(0, value);
		$A000: SelectPRGPage(1, value);
		$C000: SelectPRGPage(2, value);
		$B000..$B007:
			SelectCHRPage(addr-$B000, value);

		$9001: ApplyMirroringType(value and $80, MIRROR_HORIZONTAL, MIRROR_VERTICAL);
		$9003:
		begin
			irqEnabled := (value and $80) = $80;
			CPUClearIRQSource(irqExternal);
		end;

		$9004:
		begin
			irqCounter := irqReloadValue;
			CPUClearIRQSource(irqExternal);
		end;

		$9005: irqReloadValue := (irqReloadValue and $00FF) or (value shl 8);
		$9006: irqReloadValue := (irqReloadValue and $FF00) or value;

	end;
end;

end.
