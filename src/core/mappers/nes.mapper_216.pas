unit NES.Mapper_216;

// Mapper 216: Various Russian Dendy games

interface

uses
	NES.Mapper;

type
	TMapper_216 = class(TMapper)
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;
		function  AllowRegisterRead: Boolean; override;

		procedure WriteRegister(addr: Word;  value: Byte); override;
		procedure InitMapper; override;
	end;

implementation

uses
	NES.Types;

{ TMapper_216 }

function TMapper_216.GetPRGPageSize: Word; begin Result := $8000; end;
function TMapper_216.GetCHRPageSize: Word; begin Result := $2000; end;
function TMapper_216.AllowRegisterRead: Boolean; begin Result := True;  end;

procedure TMapper_216.InitMapper;
begin
	WriteRegister($8000, 0);
	AddRegisterRange($5000, $5000);
	RemoveRegisterRange($8000, $FFFF, moRead);
end;

procedure TMapper_216.WriteRegister(addr: Word; value: Byte);
begin
	SelectPRGPage(0, addr and $01);
	SelectCHRPage(0, (addr and $0E) shr 1);
end;


end.
