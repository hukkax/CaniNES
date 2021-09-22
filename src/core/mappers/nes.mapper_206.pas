unit NES.Mapper_206;

// Mapper 206: DxROM/Namcot 118/Tengen MIMIC-1

interface

uses
	NES.Mapper, NES.Mapper_004;

type
	TMapper_206 = class(TMapper_004)
	protected
		procedure WriteRegister(addr: Word; value: Byte); override;
		procedure UpdateMirroring; override;
	end;

	TMapper_076 = class(TMapper_206)
	protected
		function  GetCHRPageSize: Word; override;
		procedure UpdateChrMapping; override;
	end;

	TMapper_088 = class(TMapper_206)
	protected
		procedure UpdateChrMapping; override;
	end;

	TMapper_095 = class(TMapper_206)
	protected
		procedure WriteRegister(addr: Word; value: Byte); override;
	end;

	TMapper_154 = class(TMapper_206)
	protected
		procedure WriteRegister(addr: Word; value: Byte); override;
	end;

implementation

uses
	NES.Types;

// ================================================================================================
// Mapper 206
// ================================================================================================

procedure TMapper_206.WriteRegister(addr: Word; value: Byte);
begin
	// Redirect 0x8000-0xFFFF writes to 0x8000-0x8001, other features do not exist in this version
	addr := addr and $8001;

	if addr = $8000 then
	begin
		// Disable CHR Mode 1 and PRG Mode 1
		// "PRG always has the last two 8KiB banks fixed to the end."
		// "CHR always gives the left pattern table (0000-0FFF) the two 2KiB banks,
		// and the right pattern table (1000-1FFF) the four 1KiB banks."
		value := value and $3F;
	end;

	inherited;
end;

procedure TMapper_206.UpdateMirroring;
begin
	// Do nothing - Namco 108 has hardwired mirroring only
	// "Mirroring is hardwired, one game uses 4-screen mirroring (Gauntlet, DRROM)."
end;

// ================================================================================================
// Mapper 076
// increases CHR to 128KiB by inflating the 1KiB CHR banks to 2KiB and making the originally-2KiB banks inaccessible
// ================================================================================================

function TMapper_076.GetCHRPageSize: Word;
begin
	Result := $0800;
end;

procedure TMapper_076.UpdateChrMapping;
begin
	SelectCHRPage(0, registers[2]);
	SelectCHRPage(1, registers[3]);
	SelectCHRPage(2, registers[4]);
	SelectCHRPage(3, registers[5]);
end;

// ================================================================================================
// Mapper 088
// increases CHR to 128KiB by connecting PPU's A12 line to the CHR ROM's A16 line,
// making tiles in $0000 and $1000 come from disjoint sections of ROM
// ================================================================================================

procedure TMapper_088.UpdateChrMapping;
begin
	registers[0] := registers[0] and $3F;
	registers[1] := registers[1] and $3F;
	registers[2] := registers[2] and $40;
	registers[3] := registers[3] and $40;
	registers[4] := registers[4] and $40;
	registers[5] := registers[5] and $40;

	inherited UpdateChrMapping;
end;

// ================================================================================================
// Mapper 095
// uses the MSB to control mirroring by connecting CHR A15 to CIRAM A10
// ================================================================================================

procedure TMapper_095.WriteRegister(addr: Word; value: Byte);
var
	nameTable1, nameTable2: Byte;
begin
	inherited;

	if Odd(addr) then
	begin
		nameTable1 := (registers[0] >> 5) and 1;
		nameTable2 := (registers[1] >> 5) and 1;
		SetNametables(nameTable1, nameTable1, nameTable2, nameTable2);
	end;
end;

// ================================================================================================
// Mapper 154
// starts with mapper 88, then adds mapper-controlled one-screen mirroring
// ================================================================================================

procedure TMapper_154.WriteRegister(addr: Word; value: Byte);
begin
	ApplyMirroringType((value and $40) = $40,
		MIRROR_SCREENBONLY, MIRROR_SCREENAONLY);

	inherited;
end;


end.

