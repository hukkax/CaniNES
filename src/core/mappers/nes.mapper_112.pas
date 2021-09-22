unit NES.Mapper_112;

// Mapper 112: Asder, NTDEC

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper;

type
	TMapper_112 = class(TMapper)
	protected
		currentReg,
		outerChrBank: Byte;
		registers: array [0..7] of Byte;

		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;
		function  RegisterStartAddress: Word; override;
		function  RegisterEndAddress:   Word; override;

		procedure WriteRegister(addr: Word;  value: Byte); override;

		procedure InitMapper; override;

		procedure UpdateState;
	public
		constructor Create(cartridge: TCartridge); override;
	end;


implementation

{ TMapper_112 }

function TMapper_112.GetPRGPageSize: Word; begin Result := $2000; end;
function TMapper_112.GetCHRPageSize: Word; begin Result := $0400; end;
function TMapper_112.RegisterStartAddress: Word; begin Result := $8000; end;
function TMapper_112.RegisterEndAddress:   Word; begin Result := $FFFF; end;

constructor TMapper_112.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterArray(Length(registers), @registers[0]);
	RegisterProperty(8, @currentReg);
	RegisterProperty(8, @outerChrBank);
end;

procedure TMapper_112.InitMapper;
begin
	currentReg   := 0;
	outerChrBank := 0;

	FillByte(registers[0], SizeOf(registers), 0);

	SetMirroringType(MIRROR_VERTICAL);
	AddRegisterRange($4020, $5FFF, moWrite);

	SelectPRGPage(2, -2);
	SelectPRGPage(3, -1);
	UpdateState;
end;

procedure TMapper_112.UpdateState;
begin
	SelectPRGPage(0, registers[0]);
	SelectPRGPage(1, registers[1]);

	SelectChrPage2x(0, registers[2]);
	SelectChrPage2x(1, registers[3]);

	SelectCHRPage(4, registers[4] or ((outerChrBank and $10) shl 4));
	SelectCHRPage(5, registers[5] or ((outerChrBank and $20) shl 3));
	SelectCHRPage(6, registers[6] or ((outerChrBank and $40) shl 2));
	SelectCHRPage(7, registers[7] or ((outerChrBank and $80) shl 1));
end;

procedure TMapper_112.WriteRegister(addr: Word; value: Byte);
begin
	case (addr and $E001) of
		$8000: currentReg := value and $07;
		$A000: registers[currentReg] := value;
		$C000: outerChrBank := value;
		$E000: ApplyMirroringType(Odd(value), MIRROR_HORIZONTAL, MIRROR_VERTICAL);
	end;
	UpdateState;
end;

end.
