unit NES.Mapper_268;

{$WARN 4035 off : Mixing signed expressions and longwords gives a 64bit result}

// Mapper 268: MMC3-Coolboy
//
// Mostly taken from FCEUX's code

interface

uses
	NES.Cartridge,
	NES.Mapper_004, NES.Mapper, NES.Types;

type
	TMapper_268 = class(TMapper_004)
	private
		exRegs: array[0..3] of Byte;
	protected
		function  RegisterStartAddress: Word; override;
		function  GetCHRRamSize: Cardinal; override;

		procedure SelectCHRPage(slot, page: Word; memoryType: TChrMemoryType = ChrDefault); override;
		procedure SelectPRGPage(slot: Word; page: Int32; memoryType: TPrgMemoryType = TPrgMemoryType.PrgRom); override;
		procedure WriteRegister(address: Word; value: Byte); override;
	public
		procedure Reset(SoftReset: Boolean); override;

		constructor Create(cartridge: TCartridge); override;
	end;


implementation

uses
	SysUtils, Math, Basement.Util, NES.Config;

// ============================================================================
// TMapper_268
// ============================================================================

constructor TMapper_268.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterArray(Length(exRegs), @exRegs[0]);
end;

function TMapper_268.GetCHRRamSize: Cardinal;
begin
	Result := $40000;
end;

function TMapper_268.RegisterStartAddress: Word;
begin
	Result := $6000;
end;

procedure TMapper_268.Reset(SoftReset: Boolean);
begin
	ClearArray(exRegs);
	inherited Reset(softReset);
	ResetMmc3;
	UpdateState;
end;

procedure TMapper_268.SelectCHRPage(slot, page: Word; memoryType: TChrMemoryType);
var
	addr, mask: Word;
	cbase: Integer;
begin
	addr := slot * $400;
	mask := $FF xor (exRegs[0] and $80);
	cbase := IfThen(chrMode <> 0, $1000, 0);

	if (exRegs[3] and $10) <> 0 then
	begin
		if (exRegs[3] and $40) <> 0 then
		begin
			case (cbase xor addr) of
				$0400, $0C00: page := page and $7F;
			end;
		end;

		inherited SelectCHRPage(slot,
			(page and $80 and mask) or ((((exRegs[0] and $08) shl 4) and (not mask)))
			or ((exRegs[2] and $0F) shl 3) or slot);
	end
	else
	begin
		if (exRegs[3] and $40) <> 0 then
		begin
			case (cbase xor addr) of
				$0000: page := registers[0];
				$0800: page := registers[1];
				$0400,
				$0C00: page := 0;
			end;
		end;

		inherited SelectCHRPage(slot,
			(page and mask) or (((exRegs[0] and $08) shl 4) and (not mask)));
	end;
end;

procedure TMapper_268.SelectPRGPage(slot: Word; page: Int32; memoryType: TPrgMemoryType);
var
	addr: Word;
	mask, base: Cardinal;
	emask: Byte;
begin
	addr := $8000 + slot * $2000;
	mask := (($3F or (exRegs[1] and $40) or ((exRegs[1] and $20) shl 2)) xor ((exRegs[0] and $40) shr 2)) xor ((exRegs[1] and $80) shr 2);
	base := ((exRegs[0] and $07) shr 0) or ((exRegs[1] and $10) shr 1) or ((exRegs[1] and $0C) shl 2) or ((exRegs[0] and $30) shl 2);

	if ((exRegs[3] and $40) <> 0) and (page >= $FE) and (prgMode <> 0) then
	case slot of
		1: if prgMode <> 0 then page := 0;
		2: if prgMode =  0 then page := 0;
		3: page := 0;
	end;

	if (exRegs[3] and $10) = 0 then
		inherited SelectPRGPage(slot, (((base shl 4) and (not mask))) or (page and mask))
	else
	begin
		mask := mask and $F0;
		if (exRegs[1] and $02) <> 0 then
			emask := (exRegs[3] and $0C) or ((addr and $4000) shr 13)
		else
			emask := exRegs[3] and $0E;

		inherited SelectPRGPage(slot, ((base shl 4) and (not mask)) or
			(page and mask) or emask or (slot and $01));
	end;
end;

procedure TMapper_268.WriteRegister(address: Word; value: Byte);
begin
	if address < $8000 then
	begin
		if (State.RegA001 and $80) <> 0 then
			WritePrgRam(address, value);
		if (exRegs[3] and $90) <> $80 then
		begin
			exRegs[address and $03] := value;
			UpdateState;
		end;
	end
	else
		inherited;
end;

end.

