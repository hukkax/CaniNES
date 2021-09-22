unit NES.Mapper_067;

// Sunsoft 3

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper;

type
	TMapper_067 = class(TMapper)
	private
		irqLatch, irqEnabled: Boolean;
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

uses Math, NES.CPU;

{ TMapper_067 }

function TMapper_067.GetPRGPageSize: Word; begin Result := $4000; end;
function TMapper_067.GetCHRPageSize: Word; begin Result := $800;  end;

constructor TMapper_067.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(1,  @irqLatch);
	RegisterProperty(1,  @irqEnabled);
	RegisterProperty(16, @irqCounter);
end;

procedure TMapper_067.InitMapper;
begin
	SelectPRGPage(1, -1);
end;

procedure TMapper_067.ProcessCpuClock;
begin
	if irqEnabled then
	begin
		{$R-}Dec(irqCounter);
		if irqCounter = $FFFF then
		begin
			irqEnabled := False;
			CPUSetIRQSource(irqExternal);
		end;
	end;
end;

procedure TMapper_067.WriteRegister(addr: Word; value: Byte);
begin
	case (addr and $F800) of

		$8800: SelectCHRPage(0, value);
		$9800: SelectCHRPage(1, value);
		$A800: SelectCHRPage(2, value);
		$B800: SelectCHRPage(3, value);

		$C800:
		begin
			irqCounter := irqCounter
				and (IfThen(irqLatch, $FF00, $00FF))
				or  (IfThen(irqLatch, value, value shl 8));
			irqLatch := not irqLatch;
		end;

		$D800:
		begin
			irqEnabled := (value and $10) = $10;
			irqLatch := False;
			CPUClearIRQSource(irqExternal);
		end;

		$E800: GenericChooseMirroringType(value and $03);
		$F800: SelectPRGPage(0, value);
	end;
end;

end.
