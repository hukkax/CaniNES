unit NES.Mapper_069;

// Mapper 069: Sunsoft FME7

interface

uses
	NES.Cartridge, NES.Mapper,
	NES.APU.Sunsoft5B;

type
	TMapper_069 = class(TMapper)
	private
		Audio: TSunsoft5bAudio;

		command,
		workRamValue: Byte;
		irqEnabled,
		irqCounterEnabled: Boolean;
		irqCounter: Word;
	protected
		function GetPRGPageSize: Word; override;
		function GetCHRPageSize: Word; override;

		function  GetWorkRamSize: Cardinal; override;
		function  GetWorkRamPageSize: Cardinal; override;
		function  GetSaveRamSize: Cardinal; override;
		function  GetSaveRamPageSize: Cardinal; override;

		procedure UpdateWorkRam;
		procedure InitMapper; override;
		procedure WriteRegister(addr: Word; value: Byte); override;
	public
		procedure ProcessCpuClock; override;

		function  GetSoundChipName: AnsiString; override;

		constructor Create(cartridge: TCartridge); override;
		destructor  Destroy; override;
	end;



implementation

uses
	NES.Types, NES.CPU;

{ TMapper_069 }

function TMapper_069.GetPRGPageSize:     Word;     begin Result := $2000; end;
function TMapper_069.GetCHRPageSize:     Word;     begin Result := $400;  end;
function TMapper_069.GetWorkRamSize:     Cardinal; begin Result := $8000; end;
function TMapper_069.GetWorkRamPageSize: Cardinal; begin Result := $2000; end;
function TMapper_069.GetSaveRamSize:     Cardinal; begin Result := $8000; end;
function TMapper_069.GetSaveRamPageSize: Cardinal; begin Result := $2000; end;
function TMapper_069.GetSoundChipName: AnsiString; begin Result := 'Sunsoft 5B'; end;

constructor TMapper_069.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	Audio := TSunsoft5bAudio.Create;
end;

destructor TMapper_069.Destroy;
begin
	Audio.Free;

	inherited Destroy;
end;

procedure TMapper_069.InitMapper;
begin
	command := 0;
	workRamValue := 0;
	irqEnabled := False;
	irqCounterEnabled := False;
	irqCounter := 0;

	SelectPRGPage(3, -1);

	UpdateWorkRam;
end;

procedure TMapper_069.UpdateWorkRam;
var
	accessType: TMemoryAccessType;
	prgType: TPrgMemoryType;
begin
	if (workRamValue and $40) <> 0 then
	begin
		if (workRamValue and $80) <> 0 then
			accessType := maReadWrite
		else
			accessType := maNoAccess;
		if HasBattery then
			PrgType := PrgSaveRam
		else
			PrgType := PrgWorkRam;
		SetCpuMemoryMapping($6000, $7FFF, workRamValue and $3F, PrgType, accessType);
	end
	else
		SetCpuMemoryMapping($6000, $7FFF, workRamValue and $3F, TPrgMemoryType.PrgRom);
end;

procedure TMapper_069.ProcessCpuClock;
begin
	if irqCounterEnabled then
	begin
		{$R-}Dec(irqCounter);
		if (irqEnabled) and (irqCounter = $FFFF) then
			CPUSetIrqSource(irqExternal);
	end;

	Audio.Clock;
end;

procedure TMapper_069.WriteRegister(addr: Word; value: Byte);
begin
	case (addr and $E000) of

		$8000:
			command := value;

		$A000:
			case command of

				0..7:	SelectCHRPage(command, value);

				8:
				begin
					workRamValue := value;
					UpdateWorkRam;
				end;

				9..$B:	SelectPRGPage(command - 9, value and $3F);

				$C:		GenericChooseMirroringType(value);

				$D:
				begin
					irqEnabled        := (value and $01) = $01;
					irqCounterEnabled := (value and $80) = $80;
					CPUClearIrqSource(irqExternal);
				end;

				$E:		irqCounter := (irqCounter and $FF00) or value;
				$F:		irqCounter := (irqCounter and $FF) or (value  shl  8);

			end;

		$C000, $E000:	Audio.WriteRegister(addr, value);

	end;
end;


end.

