unit NES.Mapper_152;

// Mapper 070/152: Bandai74161_7432

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper;

type
	TMapper_152 = class(TMapper)
	protected
		enableMirroringControl: Boolean;

		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;

		procedure WriteRegister(addr: Word;  value: Byte); override;
		procedure InitMapper; override;
	public
		constructor Create(cartridge: TCartridge); override;
	end;

	TMapper_070 = class(TMapper_152)
	public
		constructor Create(cartridge: TCartridge); override;
	end;

implementation

{ TMapper_152 }

function TMapper_152.GetPRGPageSize: Word; begin Result := $4000; end;
function TMapper_152.GetCHRPageSize: Word; begin Result := $2000; end;

constructor TMapper_152.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(1, @enableMirroringControl);
	enableMirroringControl := True;
end;

procedure TMapper_152.InitMapper;
begin
	SelectPRGPage(0,  0);
	SelectPRGPage(1, -1);
	SelectCHRPage(0,  0);

	// Hack to make Kamen Rider Club - Gekitotsu Shocker Land work correctly (bad header)
	SetMirroringType(MIRROR_VERTICAL);
end;

procedure TMapper_152.WriteRegister(addr: Word; value: Byte);
var
	mirroringBit: Boolean;
begin
	mirroringBit := (value and $80) = $80;

	// If any game tries to set the bit to true, assume it will use mirroring switches
	// This is a hack to make as many games as possible work without CRC checks
	if mirroringBit then
		enableMirroringControl := True;

	if enableMirroringControl then
		ApplyMirroringType(mirroringBit, MIRROR_SCREENBONLY, MIRROR_SCREENAONLY);

	// Biggest PRG ROM I could find for mapper 70/152 is 128kb, so the 4th bit will never be used on those
	SelectPRGPage(0, (value shr 4) and $07);
	SelectCHRPage(0, value and $0F);
end;

{ TMapper_070 }

constructor TMapper_070.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	// According to NesDev Wiki, Mapper 70 is meant to have mirroring forced (by the board)
	// and Mapper 152 allows the code to specify the mirroring type
	enableMirroringControl := False;
end;

end.
