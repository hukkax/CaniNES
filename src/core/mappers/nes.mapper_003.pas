unit NES.Mapper_003;

// Mapper 003/185: CNROM
//
// PPU $0000-$1FFF: 8 KB switchable CHR ROM bank

interface

uses
	NES.Mapper;

type
	TMapper_003 = class(TMapper)
	protected
		function GetPRGPageSize: Word; override;
		function GetCHRPageSize: Word; override;
		function HasBusConflicts: Boolean; override;

		procedure InitMapper; override;
		procedure WriteRegister(addr: Word; value: Byte); override;
	end;

	TMapper_185 = class(TMapper_003)
	protected
		procedure WriteRegister(addr: Word; value: Byte); override;
	end;

implementation


function TMapper_003.GetPRGPageSize: Word;
begin
	Result := $8000;
end;

function TMapper_003.GetCHRPageSize: Word;
begin
	Result := $2000;
end;

function TMapper_003.HasBusConflicts: Boolean;
begin
	Result := ((RomInfo.MapperID = 3) and (RomInfo.SubMapperID = 2)) or (RomInfo.MapperID = 185);
end;

procedure TMapper_003.InitMapper;
begin
	SelectPRGPage(0, 0);
	SelectCHRPage(0, GetPowerOnByte);
end;

procedure TMapper_003.WriteRegister(addr: Word; value: Byte);
begin
	SelectCHRPage(0, value);
end;


{ TMapper_185 }

procedure TMapper_185.WriteRegister(addr: Word; value: Byte);
var
	validAccess: Boolean;
begin
	//Submapper 0: Use heuristics - "if C AND $0F is nonzero, and if C does not equal $13: CHR is enabled"
	//Submapper 4: Enable CHR-ROM if bits 0..1 of the latch hold the value 0, otherwise disable CHR-ROM.
	//Submapper 5: Enable CHR-ROM if bits 0..1 of the latch hold the value 1, otherwise disable CHR-ROM.
	//Submapper 6: Enable CHR-ROM if bits 0..1 of the latch hold the value 2, otherwise disable CHR-ROM.
	//Submapper 7: Enable CHR-ROM if bits 0..1 of the latch hold the value 3, otherwise disable CHR-ROM.

	case RomInfo.SubMapperID of
		0: validAccess := ((value and $0F) > 0) and (value <> $13);
		4: validAccess := (value and $03) = 0;
		5: validAccess := (value and $03) = 1;
		6: validAccess := (value and $03) = 2;
		7: validAccess := (value and $03) = 3;
	else
		validAccess := False;
	end;

	if validAccess then
		SelectCHRPage(0, 0)
	else
		RemovePpuMemoryMapping($0000, $1FFF);
end;


end.

