unit NES.Mapper_061;

// Mapper 061: multicart: 20-in-1

interface

uses
	NES.Types, NES.Mapper;

type
	TMapper_061 = class(TMapper)
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;

		procedure WriteRegister(addr: Word;  value: Byte); override;
		procedure InitMapper; override;
	end;


implementation

{ TMapper_061 }

function TMapper_061.GetPRGPageSize: Word;
begin
	Result := $4000;
end;

function TMapper_061.GetCHRPageSize: Word;
begin
	Result := $2000;
end;

procedure TMapper_061.InitMapper;
begin
	SelectPRGPage(0, 0);
	SelectPRGPage(1, 1);
	SelectCHRPage(0, 0);
end;

procedure TMapper_061.WriteRegister(addr: Word; value: Byte);
var
	prgPage: Byte;
begin
	prgPage := Byte((addr and $0F) << 1) or Byte((addr >> 5) and $01);

	if (addr and $10) <> 0 then
	begin
		SelectPRGPage(0, prgPage);
		SelectPRGPage(1, prgPage);
	end
	else
	begin
		SelectPRGPage(0,  prgPage and $FE);
		SelectPRGPage(1, (prgPage and $FE) + 1);
	end;

	ApplyMirroringType(addr and $80, MIRROR_HORIZONTAL, MIRROR_VERTICAL);
end;

end.

