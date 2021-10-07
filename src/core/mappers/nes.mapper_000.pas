unit NES.Mapper_000;

// Mapper 000: NROM
// Mapper 143: Sachen NROM (Dancing Blocks/Magical Mathematics)

interface

uses
	NES.Mapper;

type
	TMapper_000 = class(TMapper)
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;

		procedure InitMapper; override;
	end;

	TMapper_143 = class(TMapper_000)
	protected
		function  RegisterStartAddress: Word; override;
		function  RegisterEndAddress:   Word; override;
		function  AllowRegisterRead: Boolean; override;
		function  ReadRegister(addr: Word): Byte; override;
	end;

implementation

{ TMapper_000 }

function TMapper_000.GetPRGPageSize: Word; begin Result := $4000; end;
function TMapper_000.GetCHRPageSize: Word; begin Result := $2000; end;

procedure TMapper_000.InitMapper;
begin
	SelectPRGPage(0, 0);
	SelectPRGPage(1, 1);
	SelectCHRPage(0, 0);
end;


{ TMapper_143 }

function TMapper_143.RegisterStartAddress: Word; begin Result := $4100; end;
function TMapper_143.RegisterEndAddress:   Word; begin Result := $5FFF; end;
function TMapper_143.AllowRegisterRead: Boolean; begin Result := True;  end;

function TMapper_143.ReadRegister(addr: Word): Byte;
begin
	Result := ((not addr) and $3F) or $40;
end;


end.
