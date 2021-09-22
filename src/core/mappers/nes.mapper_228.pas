unit NES.Mapper_228;

// Mapper 228: Active Enterprises Action 52 and Cheetahmen II

interface

uses
	NES.Mapper;

type
	TMapper_228 = class(TMapper)
	protected
		function GetPRGPageSize: Word; override;
		function GetCHRPageSize: Word; override;

		procedure InitMapper; override;
		procedure WriteRegister(addr: Word; value: Byte); override;
	public
		procedure Reset(SoftReset: Boolean); override;
	end;

implementation

uses
	NES.Types;

function TMapper_228.GetPRGPageSize: Word;
begin
	Result := $4000;
end;

function TMapper_228.GetCHRPageSize: Word;
begin
	Result := $2000;
end;

procedure TMapper_228.InitMapper;
begin
	SelectCHRPage(0, 0);
	WriteRegister(0, GetPowerOnByte);
end;

procedure TMapper_228.Reset(SoftReset: Boolean);
begin
	inherited;
	WriteRegister($8000, 0);
end;

procedure TMapper_228.WriteRegister(addr: Word; value: Byte);
var
	chipSelect, prgPage: Byte;
begin
	chipSelect := (addr >> 11) and $03;
	if chipSelect = 3 then chipSelect := 2;

	prgPage := ((addr >> 6) and $1F) or (chipSelect << 5);

	if (addr and $20) <> 0 then
	begin
		SelectPRGPage(0, prgPage);
		SelectPRGPage(1, prgPage);
	end
	else
	begin
		SelectPRGPage(0,  prgPage and $FE);
		SelectPRGPage(1, (prgPage and $FE)+1);
	end;

	SelectCHRPage(0, ((addr and $0F) << 2) or (value and $03));

	ApplyMirroringType(addr and $2000, MIRROR_HORIZONTAL, MIRROR_VERTICAL);
end;

end.
