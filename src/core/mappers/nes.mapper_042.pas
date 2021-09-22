unit NES.Mapper_042;

// Mapper 042: hacked FDS games converted to cartridge form

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper;

type
	TMapper_042 = class(TMapper)
	private
		prgReg: Byte;
		irqCounter: Word;
		irqEnabled: Boolean;

	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;

		procedure WriteRegister(address: Word; value: Byte); override;
		procedure InitMapper; override;
		procedure UpdateState;

	public
		procedure ProcessCpuClock; override;
		procedure LoadSnapshot; override;

		constructor Create(cartridge: TCartridge); override;
	end;


implementation

uses
	SysUtils,
	NES.Config, NES.Console, NES.CPU;

// ============================================================================
// TMapper_042
// ============================================================================

function TMapper_042.GetPRGPageSize: Word; begin Result := $2000; end;
function TMapper_042.GetCHRPageSize: Word; begin Result := $2000;  end;

constructor TMapper_042.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(16, @irqCounter);
	RegisterProperty(1, @irqEnabled);
	RegisterProperty(8, @prgReg);
end;

procedure TMapper_042.InitMapper;
begin
	prgReg := 0;
	irqCounter := 0;
	irqEnabled := False;

	SelectPRGPage(0, -4);
	SelectPRGPage(1, -3);
	SelectPRGPage(2, -2);
	SelectPRGPage(3, -1);
	SelectCHRPage(0,  0);

	UpdateState;
end;

procedure TMapper_042.LoadSnapshot;
begin
	inherited;
	UpdateState;
end;

procedure TMapper_042.WriteRegister(address: Word; value: Byte);
begin
	case (address and $E003) of

		$8000:
			if chrRomSize > 0 then
				SelectCHRPage(0, value and $0F);

		$E000:
		begin
			prgReg := value and $0F;
			UpdateState;
		end;

		$E001:
			ApplyMirroringType(value and $08, MIRROR_HORIZONTAL, MIRROR_VERTICAL);

		$E002:
		begin
			irqEnabled := (value = $02);

			if not irqEnabled then
			begin
				CPUClearIRQSource(irqExternal);
				irqCounter := 0;
			end;
		end;

	end;
end;

procedure TMapper_042.UpdateState;
begin
	SetCpuMemoryMapping($6000, $7FFF, prgReg and $0F, TPrgMemoryType.PrgRom);
end;

procedure TMapper_042.ProcessCpuClock;
begin
	if not irqEnabled then Exit;

	Inc(irqCounter);
	if irqCounter >= $8000 then
		Dec(irqCounter, $8000);

	if irqCounter >= $6000 then
		CPUSetIRQSource(irqExternal)
	else
		CPUClearIRQSource(irqExternal);
end;

end.

