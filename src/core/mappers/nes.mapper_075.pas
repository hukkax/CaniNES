unit NES.Mapper_075;

// Mapper 075: VRC1

interface

uses
	NES.Mapper, NES.Cartridge;

type
	TMapper_075 = class(TMapper)
	private
		chrBanks: array[0..1] of Byte;

		procedure UpdateChrBanks;
	protected
		function GetPRGPageSize: Word; override;
		function GetCHRPageSize: Word; override;

		procedure InitMapper; override;
		procedure WriteRegister(addr: Word; value: Byte); override;
	public
		constructor Create(cartridge: TCartridge); override;
	end;


implementation

uses
	Math, NES.Types;


constructor TMapper_075.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterArray(Length(chrBanks), @chrBanks[0]);
end;

function TMapper_075.GetPRGPageSize: Word;
begin
	Result := $2000;
end;

function TMapper_075.GetCHRPageSize: Word;
begin
	Result := $1000;
end;

procedure TMapper_075.InitMapper;
begin
	SelectPRGPage(3, -1);
end;

procedure TMapper_075.UpdateChrBanks;
begin
	SelectCHRPage(0, chrBanks[0]);
	SelectCHRPage(1, chrBanks[1]);
end;

procedure TMapper_075.WriteRegister(addr: Word; value: Byte);
const
	AllowOversizedPrg = True;
var
	prgMask: Byte;
begin
	// TODO: Create a setting to enable/disable oversized PRG
	prgMask := IfThen(AllowOversizedPrg, $FF, $0F);

	case (addr and $F000) of

		$8000: SelectPRGPage(0, value and prgMask);

		$9000: begin
			// "The mirroring bit is ignored if the cartridge is wired for 4-screen VRAM,
			// as is typical for Vs. System games using the VRC1."
			if GetMirroringType <> MIRROR_FOURSCREENS then
				ApplyMirroringType(value and 1, MIRROR_HORIZONTAL, MIRROR_VERTICAL);
			chrBanks[0] := (chrBanks[0] and $0F) or ((value and $02) << 3);
			chrBanks[1] := (chrBanks[0] and $0F) or ((value and $04) << 2);
			UpdateChrBanks;
		end;

		$A000: SelectPRGPage(1, value and prgMask);
		$C000: SelectPRGPage(2, value and prgMask);

		$E000: begin
			chrBanks[0] := (chrBanks[0] and $10) or (value and $0F);
			UpdateChrBanks;
		end;

		$F000: begin
			chrBanks[1] := (chrBanks[1] and $10) or (value and $0F);
			UpdateChrBanks;
		end;

	end;
end;


end.

