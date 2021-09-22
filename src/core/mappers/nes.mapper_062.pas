unit NES.Mapper_062;

// Mapper 062: multicart (Super 700-in-1)

interface

uses
	NES.Types, NES.Mapper;

type
	TMapper_062 = class(TMapper)
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;

		procedure WriteRegister(addr: Word;  value: Byte); override;

		procedure InitMapper; override;
	public
		procedure Reset(SoftReset: Boolean); override;
	end;


implementation


{ TMapper_062 }

function TMapper_062.GetPRGPageSize: Word; begin Result := $4000; end;
function TMapper_062.GetCHRPageSize: Word; begin Result := $2000; end;

procedure TMapper_062.InitMapper;
begin
	inherited InitMapper;

	SelectPRGPage(0, 0);
	SelectPRGPage(1, 1);
	SelectCHRPage(0, 0);
end;

procedure TMapper_062.Reset(SoftReset: Boolean);
begin
	if SoftReset then
		InitMapper;
end;

procedure TMapper_062.WriteRegister(addr: Word; value: Byte);
var
	prgPage, chrPage: Byte;
begin
	prgPage := ((addr and $3F00) >> 8) or (addr  and $40);
	chrPage := ((addr and $001F) << 2) or (value and $03);

	if (addr and $20) <> 0 then
	begin
		SelectPRGPage(0, prgPage);
		SelectPRGPage(1, prgPage);
	end
	else
	begin
		prgPage := prgPage and $FE;
		SelectPRGPage(0, prgPage);
		SelectPRGPage(1, prgPage + 1);
	end;

	SelectCHRPage(0, chrPage);

	ApplyMirroringType(addr and $80, MIRROR_HORIZONTAL, MIRROR_VERTICAL);
end;

end.

