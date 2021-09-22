unit NES.Mapper_087;

// Mapper 087/101: Jaleco JFxx

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper;

type
	TMapper_087 = class(TMapper)
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;

		function  RegisterStartAddress: Word; override;
		function  RegisterEndAddress: Word; override;

		procedure WriteRegister(addr: Word;  value: Byte); override;

		procedure InitMapper; override;
	end;

	TMapper_101 = class(TMapper_087)
	protected
		procedure WriteRegister(addr: Word;  value: Byte); override;
	end;


implementation

uses
	NES.Console;


{ TMapper_087 }

function TMapper_087.GetPRGPageSize: Word;       begin Result := $8000; end;
function TMapper_087.GetCHRPageSize: Word;       begin Result := $2000; end;
function TMapper_087.RegisterStartAddress: Word; begin Result := $6000; end;
function TMapper_087.RegisterEndAddress: Word;   begin Result := $FFFF; end;

procedure TMapper_087.InitMapper;
begin
	SelectPRGPage(0, 0);
	SelectCHRPage(0, 0);
end;

procedure TMapper_087.WriteRegister(addr: Word; value: Byte);
begin
	SelectCHRPage(0, ((value and 1) shl 1) or ((value and 2) shr 1));
end;


{ TMapper_101 }

procedure TMapper_101.WriteRegister(addr: Word; value: Byte);
begin
	SelectCHRPage(0, value);
end;

end.
