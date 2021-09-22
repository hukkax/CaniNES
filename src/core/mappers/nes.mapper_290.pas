unit NES.Mapper_290;

// Mapper 290: BmcNtd03

interface

uses
	NES.Types, NES.Mapper;

type
	TMapper_290 = class(TMapper)
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;

		procedure WriteRegister(addr: Word;  value: Byte); override;
	public
		procedure Reset(SoftReset: Boolean); override;
	end;


implementation

{ TMapper_290 }

function TMapper_290.GetPRGPageSize: Word; begin Result := $4000; end;
function TMapper_290.GetCHRPageSize: Word; begin Result := $2000; end;

procedure TMapper_290.Reset(SoftReset: Boolean);
begin
	inherited;
	WriteRegister($8000, 0);
end;

procedure TMapper_290.WriteRegister(addr: Word; value: Byte);
var
	prg, chr: Byte;
begin
	prg := (addr shr 10) and $1E;
	chr := ((addr and $0300) shr 5) or (addr and $07);

	if (addr and $80) <> 0 then
	begin
		prg := prg or ((addr shr 6) and 1);
		SelectPRGPage(0, prg);
		SelectPRGPage(1, prg);
	end
	else
		SelectPrgPage2x(0, prg and $FE);

	SelectCHRPage(0, chr);

	ApplyMirroringType(addr and $400,
		MIRROR_HORIZONTAL, MIRROR_VERTICAL);
end;

end.
