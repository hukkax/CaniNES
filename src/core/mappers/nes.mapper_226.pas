unit NES.Mapper_226;

// Mapper 226: multicart (76-in-1, Super 42-in-1)
// Mapper 233: multicart (Super 42-in-1 alt.)

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper;

type
	TMapper_226 = class(TMapper)
	private
		registers: array[0..1] of Byte;
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;

		function  GetPRGPage: Byte; virtual;
		procedure UpdatePrg;

		procedure InitMapper; override;
		procedure WriteRegister(addr: Word;  value: Byte); override;
	public
		procedure Reset(SoftReset: Boolean); override;

		constructor Create(cartridge: TCartridge); override;
	end;

	TMapper_233 = class(TMapper_226)
	private
		nreset: Byte;
	protected
		function  GetPRGPage: Byte; override;
	public
		procedure Reset(SoftReset: Boolean); override;

		constructor Create(cartridge: TCartridge); override;
	end;

implementation

uses
	Basement.Util;

// ============================================================================
// TMapper_226
// ============================================================================

constructor TMapper_226.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(8, @registers[0]);
	RegisterProperty(8, @registers[1]);
end;

function TMapper_226.GetPRGPageSize: Word;
begin
	Result := $4000;
end;

function TMapper_226.GetCHRPageSize: Word;
begin
	Result := $2000;
end;

function TMapper_226.GetPRGPage: Byte;
begin
	Result :=
		Byte( registers[0] and $1F)       or
		Byte((registers[0] and $80) >> 2) or
		Byte((registers[1] and $01) << 6);
end;

procedure TMapper_226.InitMapper;
begin
	registers[0] := 0;
	registers[1] := 0;

	SelectPRGPage(0, 0);
	SelectPRGPage(1, 1);
	SelectCHRPage(0, 0);
end;

procedure TMapper_226.Reset(SoftReset: Boolean);
begin
	if softReset then
		InitMapper;
end;

procedure TMapper_226.UpdatePrg;
var
	prgPage: Byte;
begin
	prgPage := GetPrgPage;

	if (registers[0] and $20) <> 0 then
	begin
		SelectPRGPage(0, prgPage);
		SelectPRGPage(1, prgPage);
	end
	else
	begin
		prgPage := prgPage and $FE;
		SelectPRGPage(0, prgPage);
		SelectPRGPage(1, prgPage+1);
	end;
end;

procedure TMapper_226.WriteRegister(addr: Word; value: Byte);
begin
	case (addr and $8001) of
		$8000: registers[0] := value;
		$8001: registers[1] := value;
	end;

	UpdatePrg;
	ApplyMirroringType(registers[0] and $40, MIRROR_VERTICAL, MIRROR_HORIZONTAL);
end;

// ============================================================================
// TMapper_233
// ============================================================================

constructor TMapper_233.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(8, @nreset);
end;

function TMapper_233.GetPRGPage: Byte;
begin
	Result := Byte(registers[0] and $1F) or Byte(nreset << 5) or Byte((registers[1] and $01) << 6);
end;

procedure TMapper_233.Reset(SoftReset: Boolean);
begin
	inherited;

	if softReset then
	begin
		nreset := nreset xor $01;
		UpdatePrg;
	end
	else
		nreset := 0;
end;

end.

