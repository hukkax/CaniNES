unit NES.Mapper_156;

// Mapper 156: Daou Infosys

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper;

type
	TMapper_156 = class(TMapper)
	private
		chrLow, chrHigh: array [0..7] of Byte;
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;
		function  RegisterStartAddress: Word; override;
		function  RegisterEndAddress:   Word; override;

		procedure InitMapper; override;
		procedure WriteRegister(addr: Word;  value: Byte); override;

		procedure UpdateChrBanks;
	public
		procedure LoadSnapshot; override;

		constructor Create(cartridge: TCartridge); override;
	end;


implementation

uses
	Basement.Util, Math;

{ TMapper_156 }

function TMapper_156.GetPRGPageSize: Word; begin Result := $4000; end;
function TMapper_156.GetCHRPageSize: Word; begin Result := $0400; end;
function TMapper_156.RegisterStartAddress: Word; begin Result := $C000; end;
function TMapper_156.RegisterEndAddress:   Word; begin Result := $C014; end;

constructor TMapper_156.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterArray(Length(chrLow),  @chrLow[0]);
	RegisterArray(Length(chrHigh), @chrHigh[0]);

	RegisterProperty(8, @chrLow);
	RegisterProperty(8, @chrHigh);
end;

procedure TMapper_156.InitMapper;
begin
	ClearArray(chrLow);
	ClearArray(chrHigh);

	SelectPRGPage(1, -1);
	SetMirroringType(MIRROR_SCREENAONLY);
end;

procedure TMapper_156.LoadSnapshot;
begin
	UpdateChrBanks;
end;

procedure TMapper_156.UpdateChrBanks;
var
	i: Integer;
begin
	for i := 0 to High(chrHigh) do
		SelectCHRPage(i, (chrHigh[i] shl 8) or chrLow[i]);
end;

procedure TMapper_156.WriteRegister(addr: Word; value: Byte);
var
	bank: Byte;
begin
	case addr of

		$C000..$C00F:
		begin
			bank := (addr and $03) + IfThen(addr >= $C008, 4, 0);
			if (addr and $04) <> 0 then
				chrHigh[bank] := value
			else
				chrLow[bank]  := value;
			UpdateChrBanks;
		end;

		$C010:
			SelectPRGPage(0, value);

		$C014:
			ApplyMirroringType(Odd(value), MIRROR_HORIZONTAL, MIRROR_VERTICAL);

	end;
end;

end.
