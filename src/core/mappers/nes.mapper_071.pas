unit NES.Mapper_071;

// Mapper 071: BF909x

interface

uses
	NES.Mapper;

type
	TMapper_071 = class(TMapper)
	private
		bf9097Mode: Boolean;
	protected
		function GetPRGPageSize: Word; override;
		function GetCHRPageSize: Word; override;

		procedure InitMapper; override;
		procedure WriteRegister(addr: Word; value: Byte); override;
	end;

implementation

uses
	NES.Types;

function TMapper_071.GetPRGPageSize: Word;
begin
	Result := $4000;
end;

function TMapper_071.GetCHRPageSize: Word;
begin
	Result := $2000;
end;

procedure TMapper_071.InitMapper;
begin
	inherited;

	bf9097Mode := (RomInfo.SubMapperID = 1);

	//First and last PRG page
	SelectPRGPage(0, 0);
	SelectPRGPage(1, -1);
	SelectCHRPage(0, 0);
end;

procedure TMapper_071.WriteRegister(addr: Word; value: Byte);
begin
	//Firehawk uses $9000 to change mirroring
	if addr = $9000 then
		bf9097Mode := True;

	if (not bf9097Mode) or (addr >= $C000) then
		SelectPRGPage(0, value)
	else
	if addr < $C000 then
		ApplyMirroringType(value and $10, MIRROR_SCREENAONLY, MIRROR_SCREENBONLY);
end;

end.

