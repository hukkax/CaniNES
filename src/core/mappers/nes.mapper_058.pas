unit NES.Mapper_058;

// Mapper 58: multicart (68-in-1 (Game Star - HKX5268))

interface

uses
	NES.Types, NES.Mapper;

type
	TMapper_058 = class(TMapper)
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;

		procedure InitMapper; override;
		procedure WriteRegister(addr: Word;  value: Byte); override;
	end;


implementation

uses
	Basement.Util;

// ============================================================================
// TMapper_058
// ============================================================================

function TMapper_058.GetPRGPageSize: Word;
begin
	Result := $4000;
end;

function TMapper_058.GetCHRPageSize: Word;
begin
	Result := $2000;
end;

procedure TMapper_058.InitMapper;
begin
	SelectPRGPage(0, 0);
	SelectPRGPage(1, 1);
	SelectCHRPage(0, 0);
end;

procedure TMapper_058.WriteRegister(addr: Word; value: Byte);
var
	prgBank: Byte;
begin
	prgBank := addr and $07;

	if (addr and $40) <> 0 then
	begin
		SelectPRGPage(0, prgBank);
		SelectPRGPage(1, prgBank);
	end
	else
		SelectPrgPage2x(0, prgBank and $06);

	SelectCHRPage(0, (addr >> 3) and $07);
	ApplyMirroringType(addr and $80, MIRROR_HORIZONTAL, MIRROR_VERTICAL);
end;

end.

