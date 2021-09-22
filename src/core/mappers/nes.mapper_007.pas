unit NES.Mapper_007;

// Mapper 007: AxROM
//
// Board    PRG ROM     Bus conflicts
// AMROM    128 KB      Yes
// ANROM    128 KB      No
// AN1ROM   64 KB       No
// AOROM    128/256 KB  Depends on +CE wiring

interface

uses
	NES.Mapper;

type
	TMapper_007 = class(TMapper)
	protected
		function GetPRGPageSize: Word; override;
		function GetCHRPageSize: Word; override;
		function HasBusConflicts: Boolean; override;

		procedure InitMapper; override;
		procedure WriteRegister(addr: Word; value: Byte); override;
	end;

implementation

uses
	NES.Types;

function TMapper_007.GetPRGPageSize: Word;
begin
	Result := $8000;
end;

function TMapper_007.GetCHRPageSize: Word;
begin
	Result := $2000;
end;

procedure TMapper_007.InitMapper;
begin
	SelectCHRPage(0, 0);
	WriteRegister(0, GetPowerOnByte);
end;

function TMapper_007.HasBusConflicts: Boolean;
begin
	Result := (RomInfo.SubMapperID = 2);
end;

procedure TMapper_007.WriteRegister(addr: Word; value: Byte);
begin
	SelectPRGPage(0, value and $0F);
	ApplyMirroringType(value and $10, MIRROR_SCREENBONLY, MIRROR_SCREENAONLY);
end;

end.
