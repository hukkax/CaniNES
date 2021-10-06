unit NES.Mapper_229;

// Mapper 229: multicart: BMC 31-IN-1

interface

uses
	NES.Types, NES.Mapper;

type
	TMapper_229 = class(TMapper)
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;

		procedure WriteRegister(addr: Word;  value: Byte); override;
		procedure InitMapper; override;
	end;


implementation

{ TMapper_229 }

function TMapper_229.GetPRGPageSize: Word;
begin
	Result := $4000;
end;

function TMapper_229.GetCHRPageSize: Word;
begin
	Result := $2000;
end;

procedure TMapper_229.InitMapper;
begin
	WriteRegister($8000, 0);
end;

procedure TMapper_229.WriteRegister(addr: Word; value: Byte);
begin
	SelectCHRPage(0, addr and $FF);

	if (addr and $1E) = 0 then
		SelectPrgPage2x(0, 0)
	else
	begin
		SelectPRGPage(0, addr and $1F);
		SelectPRGPage(1, addr and $1F);
	end;

	ApplyMirroringType(addr and $20,
		MIRROR_HORIZONTAL, MIRROR_VERTICAL);
end;

end.
