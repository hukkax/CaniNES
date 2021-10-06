unit NES.Mapper_086;

{$WARN 4035 off : Mixing signed expressions and longwords gives a 64bit result}

// Mapper 086: Jaleco JF-13

interface

uses
	NES.Types, NES.Mapper;

type
	TMapper_086 = class(TMapper)
	private
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;

		function  RegisterStartAddress: Word; override;
		function  RegisterEndAddress: Word; override;

		procedure WriteRegister(addr: Word;  value: Byte); override;

		procedure InitMapper; override;
	end;


implementation

{ TMapper_086 }

function TMapper_086.GetPRGPageSize: Word;
begin
	Result := $8000;
end;

function TMapper_086.GetCHRPageSize: Word;
begin
	Result := $2000;
end;

function TMapper_086.RegisterStartAddress: Word;
begin
	Result := $6000;
end;

function TMapper_086.RegisterEndAddress: Word;
begin
	Result := $7FFF;
end;

procedure TMapper_086.InitMapper;
begin
	SelectPRGPage(0, 0);
end;

procedure TMapper_086.WriteRegister(addr: Word; value: Byte);
begin
	case (addr and $7000) of

		$6000:
		begin
			SelectPRGPage(0, (value and $30) >> 4);
			SelectCHRPage(0, (value and $03) or ((value >> 4) and $04));
		end;

		$7000: ; // Audio hardware not implemented
	end;

end;

end.

