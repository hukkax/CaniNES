unit NES.Mapper_048;

// Mapper 048: Taito TC0690

interface

uses
	NES.Types, NES.Cartridge,
	NES.Mapper, NES.Mapper_004;

type
	TMapper_048 = class(TMapper_004)
	private
		irqDelay: Byte;
		isFlintstones: Boolean;
	protected
		procedure TriggerIRQ; override;

		procedure InitMapper; override;
		procedure WriteRegister(addr: Word;  value: Byte); override;
	public
		procedure ProcessCPUClock; override;

		constructor Create(cartridge: TCartridge); override;
	end;


implementation

uses
	Math, NES.CPU;

{ TMapper_048 }

constructor TMapper_048.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(8, @irqDelay);
end;

procedure TMapper_048.InitMapper;
begin
	irqDelay := 0;

	SelectPRGPage(2, -2);
	SelectPRGPage(3, -1);

	// This cart appears to behave differently (maybe not an identical mapper?)
	// IRQ seems to be triggered at a different timing (approx 100 cpu cycles before regular mapper 48 timings)
	isFlintstones := RomInfo.SubMapperID = 255;
end;

procedure TMapper_048.TriggerIRQ;
begin
	// "The IRQ seems to trip a little later than it does on MMC3.  It looks like about a 4 CPU cycle delay from the normal MMC3 IRQ time."
	// A value of 6 removes the shaking from The Jetsons.
	irqDelay := IfThen(isFlintstones, 19, 6);
end;

procedure TMapper_048.ProcessCPUClock;
begin
	if irqDelay > 0 then
	begin
		Dec(irqDelay);
		if irqDelay = 0 then
			CPUSetIRQSource(irqExternal);
	end;
end;

procedure TMapper_048.WriteRegister(addr: Word; value: Byte);
begin
	case (addr and $E003) of

		$8000:
			SelectPRGPage(0, value and $3F);

		$8001:
			SelectPRGPage(1, value and $3F);

		$8002:
		begin
			SelectCHRPage(0, value * 2);
			SelectCHRPage(1, value * 2 + 1);
		end;

		$8003:
		begin
			SelectCHRPage(2, value * 2);
			SelectCHRPage(3, value * 2 + 1);
		end;

		$A000..$A003:
			SelectCHRPage(4 + (addr and $03), value);

		$C000:
		begin
			// Flintstones expects either $C000 or $C001 to clear the irq flag
			CPUClearIRQSource(irqExternal);
			{$R-}irqReloadValue := (value xor $FF) + IfThen(isFlintstones, 0, 1);
		end;

		$C001:
		begin
			// Flintstones expects either $C000 or $C001 to clear the irq flag
			CPUClearIRQSource(irqExternal);
			irqCounter := 0;
			irqReload := True;
		end;

		$C002:
			irqEnabled := True;

		$C003:
		begin
			irqEnabled := False;
			CPUClearIRQSource(irqExternal);
		end;

		$E000:
			ApplyMirroringType(value and $40 = $40, MIRROR_HORIZONTAL, MIRROR_VERTICAL);

	end;
end;


end.
