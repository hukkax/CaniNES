unit NES.Mapper_105;

// MMC1_105

interface

uses
	NES.Types, NES.Cartridge,
	NES.Mapper, NES.Mapper_001;

type
	TMapper_105 = class(TMapper_001)
	private
		initState:  Byte;
		irqCounter: Cardinal;
		irqEnabled: Boolean;
	protected
		procedure InitMapper; override;
		procedure UpdateState; override;
	public
		procedure ProcessCpuClock; override;

		constructor Create(cartridge: TCartridge); override;
	end;


implementation

uses NES.CPU;

{ TMapper_105 }

constructor TMapper_105.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(8,  @initState);
	RegisterProperty(32, @irqCounter);
	RegisterProperty(1,  @irqEnabled);
end;

procedure TMapper_105.InitMapper;
begin
	inherited InitMapper;

	initState := 0;
	irqCounter := 0;
	irqEnabled := False;
	state.RegA000 := state.RegA000 or $10; // Set I bit to 1
end;

procedure TMapper_105.ProcessCpuClock;
var
	maxCounter: Cardinal;
begin
	if irqEnabled then
	begin
		Inc(irqCounter);
		maxCounter := $20000000 or (GetDipSwitches shl 25);
		if irqCounter >= maxCounter then
		begin
			CPUSetIrqSource(irqExternal);
			irqEnabled := False;
		end;
	end;
end;

procedure TMapper_105.UpdateState;
var
	prgReg: Byte;
	access: TMemoryAccessType;
begin
	if (initState = 0) and ((state.RegA000 and $10) = $00) then
		initState := 1
	else
	if (initState = 1) and ((state.RegA000 and $10) <> 0) then
		initState := 2;

	if (state.RegA000 and $10) <> 0 then
	begin
		irqEnabled := False;
		irqCounter := 0;
		CPUClearIrqSource(irqExternal);
	end
	else
		irqEnabled := True;

	case (state.Reg8000 and $03) of
		0: SetMirroringType(MIRROR_SCREENAONLY);
		1: SetMirroringType(MIRROR_SCREENBONLY);
		2: SetMirroringType(MIRROR_VERTICAL);
		3: SetMirroringType(MIRROR_HORIZONTAL);
	end;

	access := ChooseMemoryAccessType((state.RegE000 and $10) <> 0, maNoAccess, maReadWrite);
	SetCpuMemoryMapping($6000, $7FFF, 0,
		ChoosePrgMemoryType(HasBattery, PrgSaveRam, PrgWorkRam), access);

	if initState = 2 then
	begin
		if (state.RegA000 and $08) <> 0 then // MMC1 mode
		begin
			prgReg := (state.RegE000 and $07) or $08;
			if (state.Reg8000 and $08) <> 0 then
			begin
				if (state.Reg8000 and $04) <> 0 then
				begin
					SelectPRGPage(0, prgReg);
					SelectPRGPage(1, $0F);
				end
				else
				begin
					SelectPRGPage(0, $08);
					SelectPRGPage(1, prgReg);
				end;
			end
			else
				SelectPrgPage2x(0, prgReg and $FE);
		end
		else
			SelectPrgPage2x(0, state.RegA000 and $06);
	end
	else
		SelectPrgPage2x(0, 0);
end;

end.
