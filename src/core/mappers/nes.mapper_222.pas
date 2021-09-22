unit NES.Mapper_222;

// Mapper 222

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper,
	NES.A12Watcher;

type
	TMapper_222 = class(TMapper)
	private
		irqCounter: Word;
		A12Watcher: TA12Watcher;

	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;

		procedure WriteRegister(addr: Word;  value: Byte); override;

		procedure InitMapper; override;
	public
		procedure NotifyVRAMAddressChange(addr: Word); override;

		constructor Create(cartridge: TCartridge); override;
		destructor  Destroy; override;
	end;


implementation

uses
	NES.Console, NES.CPU;

{ TMapper_222 }

function TMapper_222.GetPRGPageSize: Word;
begin
	Result := $2000;
end;

function TMapper_222.GetCHRPageSize: Word;
begin
	Result := $400;
end;

constructor TMapper_222.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(16, @irqCounter);

	A12Watcher := TA12Watcher.Create(10);
end;

destructor TMapper_222.Destroy;
begin
	inherited Destroy;

	A12Watcher.Free;
end;

procedure TMapper_222.InitMapper;
begin
	irqCounter := 0;
	SelectPrgPage2x(1, -2);
end;

procedure TMapper_222.NotifyVRAMAddressChange(addr: Word);
begin
	if (A12Watcher.UpdateVramAddress(addr, Console.PPU.GetFrameCycle) = scRise)
	and (irqCounter <> 0) then
	begin
		{$R-}Inc(irqCounter);
		if irqCounter >= 240 then
		begin
			CPUSetIRQSource(irqExternal);
			irqCounter := 0;
		end;
	end;
end;

procedure TMapper_222.WriteRegister(addr: Word; value: Byte);
begin
	case (addr and $F003) of
		$8000: SelectPRGPage(0, value);
		$9000: ApplyMirroringType(value and 1, MIRROR_HORIZONTAL, MIRROR_VERTICAL);
		$A000: SelectPRGPage(1, value);
		$B000: SelectCHRPage(0, value);
		$B002: SelectCHRPage(1, value);
		$C000: SelectCHRPage(2, value);
		$C002: SelectCHRPage(3, value);
		$D000: SelectCHRPage(4, value);
		$D002: SelectCHRPage(5, value);
		$E000: SelectCHRPage(6, value);
		$E002: SelectCHRPage(7, value);
		$F000: begin
			irqCounter := value;
			CPUClearIRQSource(irqExternal);
		end;
	end;
end;

end.
