unit NES.Mapper_225;

{$WARN 4035 off : Mixing signed expressions and longwords gives a 64bit result}

// Mapper 225: various multicarts
// Mapper 255: 110-in-1 multicart

interface

uses
	NES.Types, NES.Mapper;

type
	TMapper_225 = class(TMapper)
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;
		procedure WriteRegister(addr: Word;  value: Byte); override;
		procedure InitMapper; override;
	end;

	TMapper_255 = class(TMapper_225)
	end;


implementation

uses
	Math;

{ TMapper_225 }

function TMapper_225.GetPRGPageSize: Word;
begin
	Result := $4000;
end;

function TMapper_225.GetCHRPageSize: Word;
begin
	Result := $2000;
end;

procedure TMapper_225.InitMapper;
begin
	WriteRegister($8000, 0);
end;

procedure TMapper_225.WriteRegister(addr: Word; value: Byte);
var
	prgBit, bank: Byte;
begin
	prgBit := IfThen((addr and $1000) <> 0, 0, 1);
	bank := ((addr >> 8) and $40) or ((addr >> 6) and $3F);

	SelectPRGPage(0, bank and not prgBit);
	SelectPRGPage(1, bank or prgBit);
	SelectCHRPage(0, ((addr >> 8) and $40) or (addr and $3F));

	ApplyMirroringType(addr and $2000, MIRROR_HORIZONTAL, MIRROR_VERTICAL);
end;

end.
