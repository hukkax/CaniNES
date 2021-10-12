unit NES.Mapper_KS202;

// Mapper 056/142: Kaiser KS202

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper;

type
	TMapper_KS202 = class(TMapper)
	private
		irqReloadValue,
		irqCounter:  Word;
		irqEnabled:  Boolean;
		selectedReg: Byte;
		prgRegs: array [0..3] of Byte;

	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;

		procedure InitMapper; override;
		procedure WriteRegister(addr: Word;  value: Byte); override;
	public
		procedure LoadSnapshot; override;
		procedure ProcessCpuClock; override;

		constructor Create(cartridge: TCartridge); override;
	end;


implementation

uses
	Basement.Util, NES.CPU;

{ TMapper_KS202 }

function TMapper_KS202.GetPRGPageSize: Word;
begin
	Result := $2000;
end;

function TMapper_KS202.GetCHRPageSize: Word;
begin
	Result := $400;
end;

constructor TMapper_KS202.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(16, @irqReloadValue);
	RegisterProperty(16, @irqCounter);
	RegisterProperty(1, @irqEnabled);
	RegisterProperty(8, @selectedReg);

	RegisterArray(Length(prgRegs), @prgRegs[0]);
end;

procedure TMapper_KS202.InitMapper;
begin
	irqReloadValue := 0;
	irqCounter := 0;
	irqEnabled := False;
	selectedReg := 0;
	ClearArray(prgRegs);

	SelectPRGPage(3, -1);
end;

procedure TMapper_KS202.LoadSnapshot;
begin
	inherited LoadSnapshot;

	SetCpuMemoryMapping($6000, $7FFF, prgRegs[3],
		TPrgMemoryType.PrgRom, TMemoryAccessType.maReadWrite);
end;

procedure TMapper_KS202.ProcessCpuClock;
begin
	if irqEnabled then
	begin
		{$R-}Inc(irqCounter);
		if irqCounter = $FFFF then
		begin
			irqCounter := irqReloadValue;
			CPUSetIRQSource(irqExternal);
		end;
	end;
end;

procedure TMapper_KS202.WriteRegister(addr: Word; value: Byte);
var
	bank: Byte;
begin
	case (addr and $F000) of

		$8000: irqReloadValue := (irqReloadValue and $FFF0) or  (value and $0F);
		$9000: irqReloadValue := (irqReloadValue and $FF0F) or ((value and $0F) shl 4);
		$A000: irqReloadValue := (irqReloadValue and $F0FF) or ((value and $0F) shl 8);
		$B000: irqReloadValue := (irqReloadValue and $0FFF) or ((value and $0F) shl 12);

		$C000:
		begin
			irqEnabled := (value <> 0);
			if irqEnabled then
				irqCounter := irqReloadValue;
			CPUClearIRQSource(irqExternal);
		end;

		$D000: CPUClearIRQSource(irqExternal);
		$E000: selectedReg := (value and $0F) - 1;

		$F000:
		begin
			if selectedReg < 3 then
				prgRegs[selectedReg] := ((prgRegs[selectedReg]) and $10) or (value and $0F)
			else
			if selectedReg < 4 then
			begin
				// For Kaiser7032 (Mapper 142)
				prgRegs[selectedReg] := value;
				SetCpuMemoryMapping($6000, $7FFF, value,
					TPrgMemoryType.PrgRom, TMemoryAccessType.maReadWrite);
			end;

			case (addr and $FC00) of

				$F000:
				begin
					bank := addr and $03;
					if bank < 3 then
						prgRegs[bank] := (value and $10) or (prgRegs[bank] and $0F)
				end;

				$F800:
					ApplyMirroringType(value and 1,
						MIRROR_VERTICAL, MIRROR_HORIZONTAL);

				$FC00:
					SelectCHRPage(addr and $07, value);
			end;

			SelectPRGPage(0, prgRegs[0]);
			SelectPRGPage(1, prgRegs[1]);
			SelectPRGPage(2, prgRegs[2]);
		end;

	end;
end;

end.
