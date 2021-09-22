unit NES.Mapper_064;

// Mapper 064/158: Tengen RAMBO-1
//
// CPU $8000-$9FFF: 8 KiB switchable PRG ROM bank
// CPU $A000-$BFFF: 8 KiB switchable PRG ROM bank
// CPU $C000-$DFFF: 8 KiB switchable PRG ROM bank
// CPU $E000-$FFFF: 8 KiB PRG ROM bank, fixed to the last bank
// PPU -- Three selectable configurations:
//  1 KiB switchable CHR banks at $0000, $0400, $0800, $0C00, $1000, $1400, $1800, $1C00
//  2 KiB switchable CHR banks at $0000, $0800; 1 KiB switchable CHR banks at $1000, $1400, $1800, $1C00
//  2 KiB switchable CHR banks at $1000, $1800; 1 KiB switchable CHR banks at $0000, $0400, $0800, $0C00

interface

uses
 	NES.Types, NES.Cartridge, NES.Mapper,
	NES.A12Watcher;

type
	TMapper_064 = class(TMapper)
	const
		PpuIrqDelay = 2;
		CpuIrqDelay = 1;
	private
		{procedure ResetMmc3;
		procedure TriggerIrq; inline;
		function  CanWriteToWorkRam: Boolean;
		procedure UpdateMirroring;
		procedure UpdateChrMapping;
		procedure UpdatePrgMapping;}

		procedure ClockIrqCounter(delay: Byte);
		procedure UpdateState;
	protected
		irqEnabled,
		irqCycleMode,
		needReload,
		forceClock: Boolean;
		irqCounter,
		irqReloadValue,
		cpuClockCounter,
		currentRegister,
		needIrqDelay: Byte;

		registers: array[0..15] of Byte;

		a12Watcher: TA12Watcher;

		procedure InitMapper; override;
		procedure WriteRegister(address: Word; value: Byte); override;

		function  GetPRGPageSize:     Word;     override;
		function  GetCHRPageSize:     Word;     override;
	public
		procedure NotifyVRAMAddressChange(addr: Word); override;
		procedure ProcessCpuClock; override;

		procedure LoadSnapshot; override;
		procedure SaveSnapshot; override;

		constructor Create(cartridge: TCartridge); override;
		destructor  Destroy; override;
	end;

	// Tengen 800037
	//
	TMapper_158 = class(TMapper_064)
	protected
		procedure WriteRegister(address: Word; value: Byte); override;
	end;

implementation

uses
	SysUtils, Math,
	NES.Console, NES.CPU, NES.PPU;

{ TMapper_064 }

constructor TMapper_064.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(1, @irqEnabled);
	RegisterProperty(1, @irqCycleMode);
	RegisterProperty(1, @needReload);
	RegisterProperty(1, @forceClock);
	RegisterProperty(8, @irqCounter);
	RegisterProperty(8, @irqReloadValue);
	RegisterProperty(8, @cpuClockCounter);
	RegisterProperty(8, @currentRegister);
	RegisterProperty(8, @needIrqDelay);

	RegisterArray(Length(Registers), @Registers[0]);

	a12Watcher := TA12Watcher.Create(30);
end;

destructor TMapper_064.Destroy;
begin
	a12Watcher.Free;
	inherited Destroy;
end;

function TMapper_064.GetPRGPageSize: Word;
begin
	Result := $2000;
end;

function TMapper_064.GetCHRPageSize: Word;
begin
	Result := $0400;
end;

procedure TMapper_064.InitMapper;
begin
	inherited;

	irqEnabled := False;
	irqCycleMode := False;
	needReload := False;
	irqCounter := 0;
	irqReloadValue := 0;
	cpuClockCounter := 0;

	currentRegister := 0;
	needIrqDelay := 0;
	forceClock := False;

	FillByte(registers, SizeOf(registers), 0);
	SelectPRGPage(3, -1);
end;

procedure TMapper_064.NotifyVRAMAddressChange(addr: Word);
begin
	if not irqCycleMode then
	begin
		if A12Watcher.UpdateVramAddress(addr, NES_PPU.GetFrameCycle) = scRise then
			ClockIrqCounter(PpuIrqDelay);
	end;
end;

procedure TMapper_064.WriteRegister(address: Word; value: Byte);
begin
	case (address and $E001) of

		$8000: // Bank select
			currentRegister := value;

		$8001: // Bank data
		begin
			registers[currentRegister and $0F] := value;
			UpdateState;
		end;

		$A000: // Mirroring
			if Odd(value) then
				SetMirroringType(MIRROR_HORIZONTAL)
			else
				SetMirroringType(MIRROR_VERTICAL);

		$C000: // IRQ latch
			irqReloadValue := value;

		$C001: // IRQ mode select
		begin
			// "To be clear, after the write in the reg $C001, are needed more than four CPU clock cycles
			// before the switch takes place, allowing another clock of irq running the reload." -FHorse
			if (irqCycleMode) and (not Odd(value)) then
				forceClock := True; // Fixes Skull & Crossbones

			irqCycleMode := Odd(value);
			if irqCycleMode then
				cpuClockCounter := 0;
			needReload := True;
		end;

		$E000: // IRQ acknowledge/disable
		begin
			irqEnabled := False;
			NES_CPU.ClearIRQSource(irqExternal);
		end;

		$E001: // IRQ enable
			irqEnabled := True;

	end;
end;

procedure TMapper_064.ProcessCpuClock;
begin
	if needIrqDelay <> 0 then
	begin
		Dec(needIrqDelay);
		if needIrqDelay = 0 then
			NES_CPU.SetIrqSource(irqExternal);
	end;

	if (irqCycleMode) or (forceClock) then
	begin
		cpuClockCounter := (cpuClockCounter + 1) and $03;
		if cpuClockCounter = 0 then
		begin
			ClockIrqCounter(CpuIrqDelay);
			forceClock := False;
		end;
	end;
end;

procedure TMapper_064.LoadSnapshot;
begin
	inherited LoadSnapshot;
end;

procedure TMapper_064.SaveSnapshot;
begin
	inherited SaveSnapshot;
end;

procedure TMapper_064.ClockIrqCounter(delay: Byte);
begin
	if needReload then
	begin
		//Fixes Hard Drivin'
		if irqReloadValue <= 1 then
			irqCounter := (irqReloadValue + 1) and $FF
		else
			irqCounter := (irqReloadValue + 2) and $FF;
		needReload := False;
	end
	else
	if irqCounter = 0 then
		irqCounter := (irqReloadValue + 1) and $FF;

	irqCounter := (irqCounter - 1) and $FF;
	if (irqCounter = 0) and (irqEnabled) then
		needIrqDelay := delay;
end;

procedure TMapper_064.UpdateState;
var
	a12Inversion: Byte;
begin
	if (currentRegister and $40) <> 0 then
	begin
		SelectPRGPage(0, registers[15]);
		SelectPRGPage(1, registers[6]);
		SelectPRGPage(2, registers[7]);
	end
	else
	begin
		SelectPRGPage(0, registers[6]);
		SelectPRGPage(1, registers[7]);
		SelectPRGPage(2, registers[15]);
	end;

	a12Inversion := IfThen( (currentRegister and $80) <> 0, $04, $00);
	SelectCHRPage(0 xor a12Inversion, registers[0]);
	SelectCHRPage(2 xor a12Inversion, registers[1]);
	SelectCHRPage(4 xor a12Inversion, registers[2]);
	SelectCHRPage(5 xor a12Inversion, registers[3]);
	SelectCHRPage(6 xor a12Inversion, registers[4]);
	SelectCHRPage(7 xor a12Inversion, registers[5]);

	if (currentRegister and $20) <> 0 then
	begin
		SelectCHRPage(1 xor a12Inversion, registers[8]);
		SelectCHRPage(3 xor a12Inversion, registers[9]);
	end
	else
	begin
		SelectCHRPage(1 xor a12Inversion, registers[0]+1);
		SelectCHRPage(3 xor a12Inversion, registers[1]+1);
	end;
end;


{ TMapper_158 }

procedure TMapper_158.WriteRegister(address: Word; value: Byte);
var
	nametable: Byte;
begin
	if (address and $E001) = $8001 then
	begin
		nametable := value >> 7;

		if(currentRegister and $80) <> 0 then
		begin
			case (currentRegister and $07) of
				2: SetNametable(0, nametable);
				3: SetNametable(1, nametable);
				4: SetNametable(2, nametable);
				5: SetNametable(3, nametable);
			end;
		end
		else
		case (currentRegister and $07) of
			0:	begin
					SetNametable(0, nametable);
					SetNametable(1, nametable);
				end;

			1:	begin
					SetNametable(2, nametable);
					SetNametable(3, nametable);
				end;
		end;
	end;

	if (address and $E001) <> $A000 then
		inherited WriteRegister(address, value);
end;

end.

