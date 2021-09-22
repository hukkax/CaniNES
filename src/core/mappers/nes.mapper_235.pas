unit NES.Mapper_235;

// Mapper 235: Bmc235

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper;

type
	TMapper_235 = class(TMapper)
	private
		openBus: Boolean;
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;

		procedure InitMapper; override;
		procedure WriteRegister(addr: Word;  value: Byte); override;
	public
		procedure Reset(SoftReset: Boolean); override;

		constructor Create(cartridge: TCartridge); override;
	end;


implementation

// ============================================================================
// TMapper_235
// ============================================================================

constructor TMapper_235.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(8, @openBus);

	//if (!saving && _openBus) then
	//	RemoveCpuMemoryMapping(0x8000, 0xFFFF); !!! TODO
end;

function TMapper_235.GetPRGPageSize: Word;
begin
	Result := $4000;
end;

function TMapper_235.GetCHRPageSize: Word;
begin
	Result := $2000;
end;

procedure TMapper_235.InitMapper;
begin
	SelectPrgPage2x(0, 0);
	SelectCHRPage(0, 0);
end;

procedure TMapper_235.Reset(SoftReset: Boolean);
begin
	SelectPrgPage2x(0, 0);
	openBus := False;
end;

procedure TMapper_235.WriteRegister(addr: Word; value: Byte);
const
	config: array[0..3, 0..3, 0..1] of Byte = (
		(($00, 0), ($00, 1), ($00, 1), ($00, 1 )),
		(($00, 0), ($00, 1), ($20, 0), ($00, 1 )),
		(($00, 0), ($00, 1), ($20, 0), ($40, 0)),
		(($00, 0), ($20, 0), ($40, 0), ($60, 0)));
var
	mode, bank: Byte;
begin
	if (addr and $0400) <> 0 then
		SetMirroringType(MIRROR_SCREENAONLY)
	else
		ApplyMirroringType(addr and $2000, MIRROR_HORIZONTAL, MIRROR_VERTICAL);

	case GetPRGPageCount of
		64:  mode := 0;
		128: mode := 1;
		256: mode := 2;
		else mode := 3;
	end;;

	bank := (config[mode][addr >> 8 and $03][0]) or (addr and $1F);
	openBus := False;

	if(config[mode][addr >> 8 and $03][1]) <> 0 then
	begin
		openBus := True; //Open bus
		RemoveCpuMemoryMapping($8000, $FFFF);
	end
	else
	if (addr and $800) <> 0 then
	begin
		bank := (bank << 1) or (addr >> 12 and $01);
		SelectPRGPage(0, bank);
		SelectPRGPage(1, bank);
	end
	else
		SelectPrgPage2x(0, bank << 1);
end;

end.

