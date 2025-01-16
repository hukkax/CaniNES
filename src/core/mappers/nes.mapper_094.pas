unit NES.Mapper_094;

// Mapper 094: Nintendo

interface

uses
	NES.Types, NES.Mapper;

type
	TMapper_094 = class(TMapper)
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;

		procedure WriteRegister(addr: Word;  value: Byte); override;

		procedure InitMapper; override;
	end;


implementation

{ TMapper_094 }

function TMapper_094.GetPRGPageSize: Word; begin Result := $4000; end;
function TMapper_094.GetCHRPageSize: Word; begin Result := $2000; end;

procedure TMapper_094.InitMapper;
begin
	// First and last PRG page
	SelectPrgPage(0, 0);
	SelectPrgPage(1, -1);

	SelectChrPage(0, 0);
end;

procedure TMapper_094.WriteRegister(addr: Word; value: Byte);
begin
	SelectPRGPage(0, (value shr 2) and $07);
end;


end.
