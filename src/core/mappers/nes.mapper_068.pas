unit NES.Mapper_068;

// Sunsoft 4

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper;

type
	TMapper_068 = class(TMapper)
	private
		ntRegs: array[0..1] of Byte;

		useChrForNametables,
		prgRamEnabled,
		usingExternalRom: Boolean;
		licensingTimer:   Cardinal;
		externalPage:     Byte;

		procedure UpdateNametables;
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;

		procedure UpdateState;

		procedure InitMapper; override;
		procedure WriteRegister(addr: Word;  value: Byte); override;
	public
		procedure ProcessCpuClock; override;
		procedure WriteRAM(addr: Word; value: Byte); override;

		constructor Create(cartridge: TCartridge); override;
	end;


implementation

uses Math;

{ TMapper_068 }

function TMapper_068.GetPRGPageSize: Word; begin Result := $4000; end;
function TMapper_068.GetCHRPageSize: Word; begin Result := $800;  end;

constructor TMapper_068.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(8, @ntRegs[0]);
	RegisterProperty(8, @ntRegs[1]);
	RegisterProperty(1, @useChrForNametables);
	RegisterProperty(1, @prgRamEnabled);
	RegisterProperty(1, @usingExternalRom);
	RegisterProperty(8, @externalPage);
end;

procedure TMapper_068.InitMapper;
begin
	useChrForNametables := False;
	ntRegs[0] := 0;
	ntRegs[1] := 0;

	licensingTimer := 0;
	usingExternalRom := False;
	prgRamEnabled := False;

	//Bank 0's initial state is undefined, but some roms expect it to be the first page
	SelectPRGPage(0, 0);
	SelectPRGPage(1, 7);

	UpdateState;
end;

procedure TMapper_068.UpdateNametables;
var
	i: Integer;
	reg: Byte;
begin
	if useChrForNametables then
	begin
		for i := 0 to 3 do
		begin
			reg := 0;
			case GetMirroringType of
				MIRROR_VERTICAL:    reg := i and $01;
				MIRROR_HORIZONTAL:  reg := (i and $02) shr 1;
				MIRROR_SCREENAONLY: reg := 0;
				MIRROR_SCREENBONLY: reg := 1;
			end;

			SetPpuMemoryMapping($2000+i*$400, $2000+i*$400+$3FF,
				ChrDefault, ntRegs[reg] * $400,
				Ord(ChooseMemoryAccessType(chrRamSize > 0, maReadWrite, maRead) ));
		end;
	end
	else
		SetMirroringType(GetMirroringType); //Reset to default mirroring
end;

procedure TMapper_068.UpdateState;
{var
	access: TMemoryAccessType;}
begin
	// assigned but unused in Mesen source?
	//access := ChooseMemoryAccessType(prgRamEnabled, maReadWrite, maNoAccess);

	SetCpuMemoryMapping($6000, $7FFF, 0, ChoosePrgMemoryType(HasBattery, prgSaveRam, prgWorkRam));
	if usingExternalRom then
	begin
		if licensingTimer = 0 then
			RemoveCpuMemoryMapping($8000, $BFFF)
		else
			SelectPRGPage(0, externalPage);
	end;
end;

procedure TMapper_068.ProcessCpuClock;
begin
	if licensingTimer > 0 then
	begin
		Dec(licensingTimer);
		if licensingTimer = 0 then
			UpdateState;
	end;
end;

procedure TMapper_068.WriteRAM(addr: Word; value: Byte);
begin
	if InRange(addr, $6000, $7FFF) then
	begin
		licensingTimer := 1024 * 105;
		UpdateState;
	end;
	inherited WriteRAM(addr, value);
end;

procedure TMapper_068.WriteRegister(addr: Word; value: Byte);
var
	externalPrg: Boolean;
begin
	case (addr and $F000) of

		$8000: SelectCHRPage(0, value);
		$9000: SelectCHRPage(1, value);
		$A000: SelectCHRPage(2, value);
		$B000: SelectCHRPage(3, value);

		$C000:
		begin
			ntRegs[0] := value or $80;
			UpdateNametables;
		end;

		$D000:
		begin
			ntRegs[1] := value or $80;
			UpdateNametables;
		end;

		$E000:
		begin
			GenericChooseMirroringType(value);
			useChrForNametables := (value and $10) = $10;
			UpdateNametables;
		end;

		$F000:
		begin
			externalPrg := (value and $08) = 0;
			if(externalPrg) and (GetPRGPageCount > 8) then
			begin
				usingExternalRom := True;
				externalPage := $08 or ((value and $07) mod (GetPRGPageCount - $08));
				SelectPRGPage(0, externalPage);
			end
			else
			begin
				usingExternalRom := False;
				SelectPRGPage(0, value and $07);
			end;
			prgRamEnabled := (value and $10) = $10;
			UpdateState;
		end;

	end; // case
end;

end.
