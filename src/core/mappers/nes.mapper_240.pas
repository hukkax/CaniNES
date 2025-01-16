unit NES.Mapper_240;

// Mapper 240

interface

uses
	NES.Types, NES.Mapper;

type
	TMapper_240 = class(TMapper)
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;
		function  RegisterStartAddress: Word; override;
		function  RegisterEndAddress:   Word; override;

		procedure WriteRegister(addr: Word;  value: Byte); override;

		procedure InitMapper; override;
	end;


implementation

{ TMapper_240 }

function TMapper_240.GetPRGPageSize: Word; begin Result := $8000; end;
function TMapper_240.GetCHRPageSize: Word; begin Result := $2000; end;
function TMapper_240.RegisterStartAddress: Word; begin Result := $4020; end;
function TMapper_240.RegisterEndAddress:   Word; begin Result := $5FFF; end;

procedure TMapper_240.InitMapper;
begin
	SelectPrgPage(0, 0);
	SelectChrPage(0, 0);
end;

procedure TMapper_240.WriteRegister(addr: Word; value: Byte);
begin
	SelectPrgPage(0, (value shr 4) and $0F);
	SelectChrPage(0, value and $0F);
end;


end.
