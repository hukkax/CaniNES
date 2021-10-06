unit NES.Mapper_079;

{$WARN 4035 off : Mixing signed expressions and longwords gives a 64bit result}

// Mapper 079: NINA-03/NINA-06
// Mapper 113: Multicart mode

interface

uses
	NES.Mapper;

type
	TMapper_079 = class(TMapper)
	private
		MulticartMode: Boolean;
	protected
		function GetPRGPageSize: Word; override;
		function GetCHRPageSize: Word; override;
		function RegisterStartAddress: Word; override;
		function RegisterEndAddress: Word; override;

		procedure InitMapper; override;
		procedure WriteRegister(addr: Word; value: Byte); override;
	public
	end;

implementation

uses
	NES.Types;

function TMapper_079.GetPRGPageSize: Word;
begin
	Result := $8000;
end;

function TMapper_079.GetCHRPageSize: Word;
begin
	Result := $2000;
end;

function TMapper_079.RegisterStartAddress: Word;
begin
	Result := $4100;
end;

function TMapper_079.RegisterEndAddress: Word;
begin
	Result := $5FFF;
end;

procedure TMapper_079.InitMapper;
begin
	MulticartMode := (RomInfo.MapperID = 113);
	SelectPRGPage(0, 0);
	SelectCHRPage(0, 0);
end;

procedure TMapper_079.WriteRegister(addr: Word; value: Byte);
begin
	if (addr and $E100) = $4100 then
	begin
		if MulticartMode then
		begin
			//Mapper 113
			SelectPRGPage(0, (value >> 3) and $07);
			SelectCHRPage(0, (value and $07) or ((value >> 3) and $08));
			ApplyMirroringType(value and $80,
				MIRROR_VERTICAL, MIRROR_HORIZONTAL);
		end
		else
		begin
			SelectPRGPage(0, (value >> 3) and $01);
			SelectCHRPage(0, value and $07);
		end;
	end;
end;

end.

