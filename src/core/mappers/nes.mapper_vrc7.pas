unit NES.Mapper_VRC7;

interface

uses
	NES.Cartridge, NES.Mapper, NES.Types,
	NES.VRC_IRQ, NES.APU.VRC7;

type
	TMapper_VRC7 = class(TMapper)
	private
		IRQ: TVrcIrq;
		Audio: TVrc7Audio;

		controlFlags: Byte;
		chrRegisters: array [0..7] of Byte;

		procedure UpdatePrgRamAccess;
		procedure UpdateState;
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;

		procedure InitMapper; override;

		procedure WriteRegister(address: Word; value: Byte); override;
	public
		procedure ProcessCpuClock; override;

		procedure LoadSnapshot; override;
		procedure SaveSnapshot; override;

		function  GetSoundChipName: AnsiString; override;

		constructor Create(cartridge: TCartridge); override;
		destructor  Destroy; override;
	end;


implementation

uses
	SysUtils, Basement.Util,
	NES.Config, NES.Console, NES.CPU;

// ============================================================================
// TMapper_VRC7
// ============================================================================

constructor TMapper_VRC7.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(8, @controlFlags);
	RegisterArray(Length(chrRegisters), @chrRegisters[0]);

	IRQ := TVrcIrq.Create;
	Audio := TVrc7Audio.Create;
end;

destructor TMapper_VRC7.Destroy;
begin
	Audio.Free;
	IRQ.Free;

	inherited Destroy;
end;

function TMapper_VRC7.GetPRGPageSize: Word; begin Result := $2000; end;
function TMapper_VRC7.GetCHRPageSize: Word; begin Result := $0400; end;
function TMapper_VRC7.GetSoundChipName: AnsiString; begin Result := 'Konami VRC7'; end;

procedure TMapper_VRC7.InitMapper;
begin
	controlFlags := 0;
	ZeroMemory(@chrRegisters[0], Length(chrRegisters));

	IRQ.Reset;
	Audio.Reset;

	SelectPRGPage(3, -1);
end;

procedure TMapper_VRC7.UpdatePrgRamAccess;
var
	PMT: TPrgMemoryType;
	MAT: TMemoryAccessType;
begin
	if HasBattery then
		PMT := TPrgMemoryType.PrgSaveRam
	else
		PMT := TPrgMemoryType.PrgWorkRam;

	if (controlFlags and $80) <> 0 then
		MAT := TMemoryAccessType.maReadWrite
	else
		MAT := TMemoryAccessType.maNoAccess;

	SetCpuMemoryMapping($6000, $7FFF, 0, PMT, MAT);
end;

procedure TMapper_VRC7.UpdateState;
begin
	GenericChooseMirroringType(controlFlags);
	UpdatePrgRamAccess;
end;

procedure TMapper_VRC7.WriteRegister(address: Word; value: Byte);
begin
	if ((address and $10) <> 0) and ((address and $F010) <> $9010) then
		address := (address or $08) and (not $10);

	case (address and $F038) of
		$8000: SelectPRGPage(0, value and $3F);
		$8008: SelectPRGPage(1, value and $3F);
		$9000: SelectPRGPage(2, value and $3F);
		$9010,
		$9030: Audio.WriteRegister(address, value);
		$A000: SelectCHRPage(0, value);
		$A008: SelectCHRPage(1, value);
		$B000: SelectCHRPage(2, value);
		$B008: SelectCHRPage(3, value);
		$C000: SelectCHRPage(4, value);
		$C008: SelectCHRPage(5, value);
		$D000: SelectCHRPage(6, value);
		$D008: SelectCHRPage(7, value);
		$E000: begin controlFlags := value; UpdateState; end;
		$E008: IRQ.SetReloadValue(value);
		$F000: IRQ.SetControlValue(value);
		$F008: IRQ.AcknowledgeIrq;
	end;
end;

procedure TMapper_VRC7.ProcessCpuClock;
begin
	IRQ.ProcessCpuClock;
	Audio.Clock;
end;

procedure TMapper_VRC7.LoadSnapshot;
begin
	inherited LoadSnapshot;

	IRQ.LoadSnapshot;
	Audio.LoadSnapshot;

	UpdatePrgRamAccess;
end;

procedure TMapper_VRC7.SaveSnapshot;
begin
	inherited SaveSnapshot;

	IRQ.SaveSnapshot;
	Audio.SaveSnapshot;
end;


end.

