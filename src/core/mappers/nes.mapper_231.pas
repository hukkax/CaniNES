unit NES.Mapper_231;

// Mapper 231: multicart (22-in-1 alt.)

interface

uses
	NES.Types, NES.Mapper;

type
	TMapper_231 = class(TMapper)
	private
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;

		procedure InitMapper; override;
		procedure WriteRegister(addr: Word;  value: Byte); override;
	public
		procedure Reset(SoftReset: Boolean); override;
	end;


implementation

{ TMapper_231 }

function TMapper_231.GetPRGPageSize: Word;
begin
	Result := $4000;
end;

function TMapper_231.GetCHRPageSize: Word;
begin
	Result := $2000;
end;

procedure TMapper_231.InitMapper;
begin
	SelectPRGPage(0, 0);
	SelectPRGPage(1, 0);
	SelectCHRPage(0, 0);
end;

procedure TMapper_231.Reset(SoftReset: Boolean);
begin
	SelectPRGPage(0, 0);
	SelectPRGPage(1, 0);
end;

procedure TMapper_231.WriteRegister(addr: Word; value: Byte);
var
	prgBank: Byte;
begin
	prgBank := ((addr >> 5) and $01) or (addr and $1E);
	SelectPRGPage(0, prgBank and $1E);
	SelectPRGPage(1, prgBank);
	ApplyMirroringType(addr and $80, MIRROR_HORIZONTAL, MIRROR_VERTICAL);
end;

end.
