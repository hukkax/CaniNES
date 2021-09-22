unit NES.Mapper_190;

// Mapper 190: Magic Kid Googoo

interface

uses
	NES.Types, NES.Mapper;

type
	TMapper_190 = class(TMapper)
	private
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;

		procedure InitMapper; override;
		procedure WriteRegister(addr: Word;  value: Byte); override;
	public
	end;


implementation


function TMapper_190.GetPRGPageSize: Word;
begin
	Result := $4000;
end;

function TMapper_190.GetCHRPageSize: Word;
begin
	Result := $800;
end;

procedure TMapper_190.InitMapper;
begin
	SelectPRGPage(0, 0);
	SelectPRGPage(1, 0);
	SelectChrPage4x(0, 0);
	SetMirroringType(MIRROR_VERTICAL);
end;

procedure TMapper_190.WriteRegister(addr: Word; value: Byte);
begin
	if (addr >= $8000) and (addr <= $9FFF) then
		SelectPRGPage(0, value and $07)
	else
	if(addr >= $C000) and (addr <= $DFFF) then
		SelectPRGPage(0, (value and $07) or $08)
	else
	if (addr and $A000) = $A000 then
		SelectCHRPage(addr and $03, value);
end;

end.
