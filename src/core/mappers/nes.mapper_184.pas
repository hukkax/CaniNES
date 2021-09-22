unit NES.Mapper_184;

// Mapper 184: Sunsoft

interface

uses
	NES.Types, NES.Mapper;

type
	TMapper_184 = class(TMapper)
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;
		function  RegisterStartAddress: Word; override;
		function  RegisterEndAddress:   Word; override;

		procedure WriteRegister(addr: Word;  value: Byte); override;

		procedure InitMapper; override;
	end;


implementation

{ TMapper_184 }

function TMapper_184.GetPRGPageSize: Word; begin Result := $8000; end;
function TMapper_184.GetCHRPageSize: Word; begin Result := $1000; end;
function TMapper_184.RegisterStartAddress: Word; begin Result := $6000; end;
function TMapper_184.RegisterEndAddress:   Word; begin Result := $7FFF; end;

procedure TMapper_184.InitMapper;
begin
	SelectPRGPage(0, 0);
end;

procedure TMapper_184.WriteRegister(addr: Word; value: Byte);
begin
	SelectCHRPage(0, value and $07);

	// "The most significant bit of H is always set in hardware."
	SelectCHRPage(1, $80 or ((value shr 4) and $07));
end;

end.
