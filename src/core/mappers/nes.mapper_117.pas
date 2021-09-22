unit NES.Mapper_117;

// Mapper 117: Future Media

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper,
	NES.A12Watcher;

type
	TMapper_117 = class(TMapper)
	private
		irqCounter, irqReloadValue: Byte;
		irqEnabled, irqEnabledAlt:  Boolean;
		A12Watcher: TA12Watcher;

	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;

		procedure InitMapper; override;
		procedure WriteRegister(addr: Word;  value: Byte); override;
	public
		procedure NotifyVRAMAddressChange(addr: Word); override;

		constructor Create(cartridge: TCartridge); override;
		destructor  Destroy; override;
	end;


implementation

uses NES.Console, NES.CPU;

{ TMapper_117 }

function TMapper_117.GetPRGPageSize: Word;
begin
	Result := $2000;
end;

function TMapper_117.GetCHRPageSize: Word;
begin
	Result := $400;
end;


constructor TMapper_117.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(8, @irqCounter);
	RegisterProperty(8, @irqReloadValue);
	RegisterProperty(1, @irqEnabled);
	RegisterProperty(1, @irqEnabledAlt);

	A12Watcher := TA12Watcher.Create(10);
end;

destructor TMapper_117.Destroy;
begin
	A12Watcher.Free;

	inherited Destroy;
end;

procedure TMapper_117.InitMapper;
begin
	irqEnabled     := False;
	irqEnabledAlt  := False;
	irqCounter     := 0;
	irqReloadValue := 0;

	SelectPrgPage4x(0, -4);
end;

procedure TMapper_117.NotifyVRAMAddressChange(addr: Word);
begin
	if A12Watcher.UpdateVramAddress(addr, NES_PPU.GetFrameCycle) = TA12StateChange.scRise then
	begin
		if (irqEnabled) and (irqEnabledAlt) and (irqCounter <> 0) then
		begin
			{$R-}Dec(irqCounter);
			if irqCounter = 0 then
			begin
				CPUSetIRQSource(irqExternal);
				irqEnabledAlt := False;
			end;
		end;
	end;
end;

procedure TMapper_117.WriteRegister(addr: Word; value: Byte);
begin
	case addr of

		$8000..$8003:
			SelectPRGPage(addr and $03, value);

		$A000..$A007:
			SelectCHRPage(addr and $07, value);

		$C001: irqReloadValue := value;
		$C002: CPUClearIRQSource(irqExternal);

		$C003:
		begin
			irqCounter := irqReloadValue;
			irqEnabledAlt := True;
		end;

		$D000:
			ApplyMirroringType(value and 1,
				MIRROR_HORIZONTAL, MIRROR_VERTICAL);

		$E000:
		begin
			irqEnabled := (value and $01) = $01;
			CPUClearIRQSource(irqExternal);
		end;

	end;
end;

end.
