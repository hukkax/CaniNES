unit NES.Mapper_057;

// Mapper 57: multicart (GK 47-in-1, 6-in-1 (SuperGK))

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper;

type
	TMapper_057 = class(TMapper)
	private
		registers: array[0..1] of Byte;
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;

		procedure UpdateState;

		procedure InitMapper; override;
		procedure WriteRegister(addr: Word;  value: Byte); override;
	public
		constructor Create(cartridge: TCartridge); override;
	end;


implementation

uses
	Basement.Util;

// ============================================================================
// TMapper_057
// ============================================================================

constructor TMapper_057.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(8, @registers[0]);
	RegisterProperty(8, @registers[1]);
end;

function TMapper_057.GetPRGPageSize: Word;
begin
	Result := $4000;
end;

function TMapper_057.GetCHRPageSize: Word;
begin
	Result := $2000;
end;

procedure TMapper_057.InitMapper;
begin
	registers[0] := 0;
	registers[1] := 0;

	UpdateState;
end;

procedure TMapper_057.UpdateState;
var
	B: Byte;
begin
	ApplyMirroringType(registers[1] and $08, MIRROR_HORIZONTAL, MIRROR_VERTICAL);

	SelectCHRPage(0, ((registers[0] and $40) >> 3) or ((registers[0] or registers[1]) and $07));

	if (registers[1] and $10) <> 0 then
	begin
		B := (registers[1] >> 5) and $06;
		SelectPRGPage(0, B);
		SelectPRGPage(1, B+1);
	end
	else
	begin
		B := (registers[1] >> 5) and $07;
		SelectPRGPage(0, B);
		SelectPRGPage(1, B);
	end;
end;

procedure TMapper_057.WriteRegister(addr: Word; value: Byte);
begin
	case (addr and $8800) of
		$8000: registers[0] := value;
		$8800: registers[1] := value;
	end;

	UpdateState;
end;

end.

