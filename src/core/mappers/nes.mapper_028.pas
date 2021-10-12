unit NES.Mapper_028;

{$WARN 4035 off : Mixing signed expressions and longwords gives a 64bit result}

// Mapper 028: Action 53

interface

uses
	NES.Cartridge, NES.Mapper;

type
	TMapper_028 = class(TMapper)
	private
		selectedReg,
		mirroringBit: Byte;
		regs: array[0..3] of Byte;
	protected
		function GetPRGPageSize: Word; override;
		function GetCHRPageSize: Word; override;

		procedure UpdateState;

		procedure InitMapper; override;
		procedure WriteRegister(addr: Word; value: Byte); override;
	public
		constructor Create(cartridge: TCartridge); override;
	end;

implementation

uses
	Basement.Util, NES.Types, Math;


function TMapper_028.GetPRGPageSize: Word;
begin
	Result := $4000;
end;

function TMapper_028.GetCHRPageSize: Word;
begin
	Result := $2000;
end;

constructor TMapper_028.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(8, @selectedReg);
	RegisterProperty(8, @mirroringBit);

	RegisterArray(Length(regs), @regs[0]);
end;

procedure TMapper_028.InitMapper;
begin
	selectedReg := 0;
	mirroringBit := 0;

	ClearArray(regs);

	AddRegisterRange($5000, $5FFF, moWrite);

	SelectPRGPage(1, -1);
end;

procedure TMapper_028.UpdateState;
const
	outerAnd: array[0..3] of Word = ( $1FE, $1FC, $1F8, $1F0 );
	innerAnd: array[0..3] of Byte = ( $01, $03, $07, $0F );
var
	mirroring, bank, gameSize, prgSize,
	slotSelect, chrSelect, prgSelect: Byte;
	outerPrgSelect: Word;
begin
	mirroring := regs[2] and $03;
	if (mirroring and $02) = 0 then
		mirroring := mirroringBit;

	case mirroring of
		0: SetMirroringType(MIRROR_SCREENAONLY);
		1: SetMirroringType(MIRROR_SCREENBONLY);
		2: SetMirroringType(MIRROR_VERTICAL);
		3: SetMirroringType(MIRROR_HORIZONTAL);
	end;

	gameSize := (regs[2] and $30) >> 4;
	prgSize  := (regs[2] and $08) >> 3;
	slotSelect := (regs[2] and $04) >> 2;
	chrSelect  := regs[0] and $03;
	prgSelect  := regs[1] and $0F;
	outerPrgSelect := regs[3] << 1;

	SelectCHRPage(0, chrSelect);

	if prgSize > 0 then
	begin
		bank := IfThen(slotSelect <> 0, 0, 1);
		case gameSize of
			0: SelectPRGPage(bank, (outerPrgSelect and $1FE) or (prgSelect and $01));
			1: SelectPRGPage(bank, (outerPrgSelect and $1FC) or (prgSelect and $03));
			2: SelectPRGPage(bank, (outerPrgSelect and $1F8) or (prgSelect and $07));
			3: SelectPRGPage(bank, (outerPrgSelect and $1F0) or (prgSelect and $0F));
		end;
		SelectPRGPage(IfThen(slotSelect <> 0, 1, 0), (outerPrgSelect and $1FE) or slotSelect);
	end
	else
	begin
		prgSelect := prgSelect << 1;
		SelectPRGPage(0, (outerPrgSelect and outerAnd[gameSize]) or (prgSelect and innerAnd[gameSize]));
		SelectPRGPage(1, (outerPrgSelect and outerAnd[gameSize]) or ((prgSelect or $01) and innerAnd[gameSize]));
	end;
end;

procedure TMapper_028.WriteRegister(addr: Word; value: Byte);
begin
	if addr <= $5FFF then
		selectedReg := ((value and $80) >> 6) or (value and 1)
	else
	if addr >= $8000 then
	begin
		if selectedReg <= 1 then
			mirroringBit := (value >> 4) and 1
		else
		if selectedReg = 2 then
			mirroringBit := value and 1;

		regs[selectedReg] := value;
		UpdateState;
	end;
end;

end.

