unit NES.Mapper_078;

// Mapper 078: Jaleco JF-16

interface

uses
	NES.Types, NES.Mapper;

type
	TMapper_078 = class(TMapper)
	private
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;
		function  HasBusConflicts: Boolean; override;

		procedure InitMapper; override;
		procedure WriteRegister(addr: Word;  value: Byte); override;
	public
	end;


implementation


function TMapper_078.GetPRGPageSize: Word;
begin
	Result := $4000;
end;

function TMapper_078.GetCHRPageSize: Word;
begin
	Result := $2000;
end;

function TMapper_078.HasBusConflicts: Boolean;
begin
	Result := True;
end;

procedure TMapper_078.InitMapper;
begin
	SelectPRGPage(0, GetPowerOnByte);
	SelectPRGPage(1, -1);

	SelectCHRPage(0, GetPowerOnByte);
end;

procedure TMapper_078.WriteRegister(addr: Word; value: Byte);
begin
	SelectPRGPage(0, value and $07);
	SelectCHRPage(0, (value shr 4) and $0F);

	if RomInfo.SubMapperID = 3 then // 078.3: Holy Diver
		ApplyMirroringType(value and $08, MIRROR_VERTICAL, MIRROR_HORIZONTAL)
	else
		ApplyMirroringType(value and $08, MIRROR_SCREENBONLY, MIRROR_SCREENAONLY);
end;

end.
