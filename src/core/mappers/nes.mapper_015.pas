unit NES.Mapper_015;

// Mapper 015: K-1029

interface

uses
	NES.Mapper;

type
	TMapper_015 = class(TMapper)
	private
	protected
		function GetPRGPageSize: Word; override;
		function GetCHRPageSize: Word; override;

		procedure InitMapper; override;
		procedure WriteRegister(addr: Word; value: Byte); override;
	public
		procedure Reset(softReset: Boolean); override;
	end;


implementation

uses
	Math, NES.Types;

procedure TMapper_015.Reset(softReset: Boolean);
begin
	WriteRegister($8000, 0);
end;

function TMapper_015.GetPRGPageSize: Word;
begin
	Result := $2000;
end;

function TMapper_015.GetCHRPageSize: Word;
begin
	Result := $2000;
end;

procedure TMapper_015.InitMapper;
begin
	inherited;
	SelectCHRPage(0, 0);
end;

procedure TMapper_015.WriteRegister(addr: Word; value: Byte);
var
	subBank, bank, mode: Byte;
begin
	subBank := value >> 7;
	bank := (value and $7F) << 1;
	mode := addr and $03;

	ApplyMirroringType(value and $40, MIRROR_HORIZONTAL, MIRROR_VERTICAL);

	if (mode = 0) or (mode = 3) then
		SetPpuMemoryMapping(0, $1FFF, 0, ChrDefault, Ord(maRead))
	else
		SetPpuMemoryMapping(0, $1FFF, 0, ChrDefault, Ord(maReadWrite));

	case mode of
		0:
		begin
			SelectPRGPage(0, bank xor subBank);
			SelectPRGPage(1, (bank + 1) xor subBank);
			SelectPRGPage(2, (bank + 2) xor subBank);
			SelectPRGPage(3, (bank + 3) xor subBank);
		end;

		1, 3:
		begin
			bank := bank or subBank;
			SelectPRGPage(0, bank);
			SelectPRGPage(1, bank + 1);
			bank := IfThen(mode = 3, bank, (bank or $0E) or subBank);
			SelectPRGPage(2, bank + 0);
			SelectPRGPage(3, bank + 1);
		end;

		2:
		begin
			bank := bank or subBank;
			SelectPRGPage(0, bank);
			SelectPRGPage(1, bank);
			SelectPRGPage(2, bank);
			SelectPRGPage(3, bank);
		end;
	end;
end;

end.

