unit NES.Mapper_227;

// Mapper 227: multicart (1200-in-1, 600-in-1), Nanjing/Waixing/Yancheng

interface

uses
	NES.Types, NES.Mapper;

type
	TMapper_227 = class(TMapper)
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;

		procedure InitMapper; override;
		procedure WriteRegister(addr: Word; value: Byte); override;
	end;


implementation

uses
	Basement.Util;

// ============================================================================
// TMapper_227
// ============================================================================

function TMapper_227.GetPRGPageSize: Word;
begin
	Result := $4000;
end;

function TMapper_227.GetCHRPageSize: Word;
begin
	Result := $2000;
end;

procedure TMapper_227.InitMapper;
begin
	WriteRegister($8000, 0);
end;

procedure TMapper_227.WriteRegister(addr: Word; value: Byte);
var
	sFlag, lFlag, prgMode: Boolean;
	prgBank: Word;
begin
	prgBank := ((addr >> 2) and $1F) or ((addr and $100) >> 3);
	sFlag   := (addr and $01) = $01;
	lFlag   := ((addr >> 9) and $01) = $01;
	prgMode := ((addr >> 7) and $01) = $01;

	if prgMode then
	begin
		if sFlag then
			SelectPrgPage2x(0, prgBank and $FE)
		else
		begin
			SelectPRGPage(0, prgBank);
			SelectPRGPage(1, prgBank);
		end;
	end
	else
	if sFlag then
	begin
		if lFlag then
		begin
			SelectPRGPage(0, prgBank and $3E);
			SelectPRGPage(1, prgBank  or $07);
		end
		else
		begin
			SelectPRGPage(0, prgBank and $3E);
			SelectPRGPage(1, prgBank and $38);
		end;
	end
	else
	begin
		if lFlag then
		begin
			SelectPRGPage(0, prgBank);
			SelectPRGPage(1, prgBank or $07);
		end
		else
		begin
			SelectPRGPage(0, prgBank);
			SelectPRGPage(1, prgBank and $38);
		end;
	end;

	ApplyMirroringType((addr and 2) = 2, MIRROR_HORIZONTAL, MIRROR_VERTICAL);
end;

end.

