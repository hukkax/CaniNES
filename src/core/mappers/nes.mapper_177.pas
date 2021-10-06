unit NES.Mapper_177;

// Mapper 177: Hénggé Diànzǐ

interface

uses
	NES.Types, NES.Mapper;

type
	TMapper_177 = class(TMapper)
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;
		function  RegisterStartAddress: Word; override;
		function  RegisterEndAddress:   Word; override;

		procedure InitMapper; override;
		procedure WriteRegister(addr: Word;  value: Byte); override;
	end;


implementation

{ TMapper_177 }

function TMapper_177.GetPRGPageSize: Word; begin Result := $8000; end;
function TMapper_177.GetCHRPageSize: Word; begin Result := $2000; end;
function TMapper_177.RegisterStartAddress: Word; begin Result := $8000; end;
function TMapper_177.RegisterEndAddress:   Word; begin Result := $FFFF; end;

procedure TMapper_177.InitMapper;
begin
	SelectPRGPage(0, 0);
	SelectCHRPage(0, 0);
end;

procedure TMapper_177.WriteRegister(addr: Word; value: Byte);
begin
	SelectPRGPage(0, value);
	ApplyMirroringType(value and $20, MIRROR_HORIZONTAL, MIRROR_VERTICAL);
end;

end.
