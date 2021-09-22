unit NES.Mapper_011;

// Mapper 011: Color Dreams
// Mapper 241: BxROM-like
//
// CPU $8000-$FFFF: 32 KB switchable PRG ROM bank
// PPU $0000-$1FFF:  8 KB switchable CHR ROM bank

interface

uses
	NES.Mapper;

type
	TMapper_011 = class(TMapper)
	protected
		function GetPRGPageSize: Word; override;
		function GetCHRPageSize: Word; override;
		function HasBusConflicts: Boolean; override;

		procedure InitMapper; override;
		procedure WriteRegister(addr: Word; value: Byte); override;
	end;

	TMapper_241 = class(TMapper_011)
	protected
		function HasBusConflicts: Boolean; override;

		procedure WriteRegister(addr: Word; value: Byte); override;
	end;

implementation

// ============================================================================
// TMapper_011
// ============================================================================

function TMapper_011.GetPRGPageSize: Word;
begin
	Result := $8000;
end;

function TMapper_011.GetCHRPageSize: Word;
begin
	Result := $2000;
end;

function TMapper_011.HasBusConflicts: Boolean;
begin
	Result := True;
end;

procedure TMapper_011.InitMapper;
begin
	SelectPRGPage(0, 0);
	SelectCHRPage(0, 0);
end;

procedure TMapper_011.WriteRegister(addr: Word; value: Byte);
begin
	if RomInfo.MapperID = 144 then
		// "This addition means that only the ROM's least significant bit always wins bus conflicts."
		value := value or (ReadRAM(addr) and $01);

	// TODO: Re-add size restriction when adding an option to prevent oversized roms
	SelectPRGPage(0, value and $0F);
	SelectCHRPage(0, (value >> 4) and $0F);
end;

// ============================================================================
// TMapper_241
// ============================================================================

function TMapper_241.HasBusConflicts: Boolean;
begin
	Result := False;
end;

procedure TMapper_241.WriteRegister(addr: Word; value: Byte);
begin
	SelectPRGPage(0, value);
end;

end.

