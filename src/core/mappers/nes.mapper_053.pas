unit NES.Mapper_053;

// Mapper 053: Supervision

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper;

type
	TMapper_053 = class(TMapper)
	const
		EPROM_CRC: Cardinal = $63794E25;
	private
		regs: array[0..1] of Byte;
		epromFirst: Boolean;

		procedure UpdateState;
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;

		function  RegisterStartAddress: Word; override;
		function  RegisterEndAddress: Word; override;

		procedure WriteRegister(addr: Word;  value: Byte); override;

		procedure InitMapper; override;
	public
		constructor Create(cartridge: TCartridge); override;
	end;


implementation

uses
	Math, CRC;

{ TMapper_053 }

function TMapper_053.GetPRGPageSize: Word; begin Result := $2000; end;
function TMapper_053.GetCHRPageSize: Word; begin Result := $2000; end;
function TMapper_053.RegisterStartAddress: Word; begin Result := $6000; end;
function TMapper_053.RegisterEndAddress:   Word; begin Result := $FFFF; end;

constructor TMapper_053.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterArray(Length(regs), @regs[0]);
end;

procedure TMapper_053.InitMapper;
begin
	inherited InitMapper;

	epromFirst := (prgSize >= $8000) and (CRC32(0, @prgRom[0], $8000) = EPROM_CRC);
	regs[0] := 0; regs[1] := 0;

	UpdateState;
end;

procedure TMapper_053.UpdateState;
var
	r: Word;
begin
	r := (regs[0] << 3) and $78;

	SetCpuMemoryMapping($6000, $7FFF,
		(r << 1 or $0F) + IfThen(epromFirst, $04, $00),
		TPrgMemoryType.PrgRom);

	SelectPrgPage2x(0, IfThen( (regs[0] and $10) <> 0,
		(r or (regs[1] and $07)) + IfThen(epromFirst, $02, $00),
		IfThen(epromFirst, $00, $80)) << 1);

	SelectPrgPage2x(1, IfThen( (regs[0] and $10) <> 0,
		(r or ($FF and $07)) + IfThen(epromFirst, $02, $00),
		IfThen(epromFirst, $01, $81)) << 1);

	ApplyMirroringType(regs[0] and $20, MIRROR_HORIZONTAL, MIRROR_VERTICAL);
end;

procedure TMapper_053.WriteRegister(addr: Word; value: Byte);
begin
	if addr < $8000 then
		regs[0] := value
	else
		regs[1] := value;

	UpdateState;
end;

end.
