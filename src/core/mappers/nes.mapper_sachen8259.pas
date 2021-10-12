unit NES.Mapper_Sachen8259;

// Mapper 137/138/139/141: Sachen 8259A..8259AD

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper;

type
	TMapper_Sachen8259 = class(TMapper)
	protected
		currentReg, shift: Byte;
		regs:  array [0..7] of Byte;
		chrOr: array [0..2] of Byte;

		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;
		function  RegisterStartAddress: Word; override;
		function  RegisterEndAddress:   Word; override;

		procedure InitMapper; override;
		procedure WriteRegister(addr: Word;  value: Byte); override;

		procedure UpdateState; virtual;
	public
		constructor Create(cartridge: TCartridge); override;
	end;

	TMapper_141 = class(TMapper_Sachen8259) // Sachen8259A
	public
		constructor Create(cartridge: TCartridge); override;
	end;

	TMapper_138 = class(TMapper_Sachen8259) // Sachen8259B
	end;

	TMapper_139 = class(TMapper_Sachen8259) // Sachen8259C
	public
		constructor Create(cartridge: TCartridge); override;
	end;

	TMapper_137 = class(TMapper_Sachen8259) // Sachen8259D
	protected
		function  GetCHRPageSize: Word; override;
		procedure UpdateState; override;
	end;

implementation

uses
	Basement.Util, Math;

{ TMapper_141 / Sachen8259A }

constructor TMapper_141.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	shift := 1;
	chrOr[0] := 1;
	chrOr[1] := 0;
	chrOr[2] := 1;
end;


{ TMapper_139 / Sachen8259C }

constructor TMapper_139.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	shift := 2;
	chrOr[0] := 1;
	chrOr[1] := 2;
	chrOr[2] := 3;
end;


{ TMapper_137 / Sachen8259D }

function TMapper_137.GetCHRPageSize: Word;
begin
	Result := $0400;
end;

procedure TMapper_137.UpdateState;
var
	simpleMode: Boolean;
begin
	simpleMode := (regs[7] and $01) = $01;
	case ((regs[7] shr 1) and $03) of
		0: SetMirroringType(MIRROR_HORIZONTAL);
		1: SetMirroringType(MIRROR_VERTICAL);
		2: SetNametables(0, 1, 1, 1);
		3: SetMirroringType(MIRROR_SCREENAONLY);
	end;

	// "Enable "simple" mode. (mirroring is fixed to H, and banks become weird)"
	if simpleMode then
		SetMirroringType(MIRROR_HORIZONTAL);

	SelectPRGPage(0, regs[5]);

	SelectCHRPage(0, regs[0]);
	SelectCHRPage(1, ((regs[4] and $01) shl 4) or regs[IfThen(simpleMode, 0, 1)]);
	SelectCHRPage(2, ((regs[4] and $02) shl 3) or regs[IfThen(simpleMode, 0, 2)]);
	SelectCHRPage(3, ((regs[4] and $04) shl 2) or ((regs[6] and $01) shl 3)
		or regs[IfThen(simpleMode, 0, 3)]);
	SelectChrPage4x(1, Word(-4));
end;


{ TMapper_Sachen8259 }

function TMapper_Sachen8259.GetPRGPageSize: Word; begin Result := $8000; end;
function TMapper_Sachen8259.GetCHRPageSize: Word; begin Result := $0800; end;

function TMapper_Sachen8259.RegisterStartAddress: Word; begin Result := $4100; end;
function TMapper_Sachen8259.RegisterEndAddress:   Word; begin Result := $7FFF; end;

constructor TMapper_Sachen8259.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterArray(Length(regs), @regs[0]);
	RegisterProperty(8, @currentReg);
end;

procedure TMapper_Sachen8259.InitMapper;
begin
	ClearArray(regs);
	currentReg := 0;
	SelectPRGPage(0, 0);
end;

procedure TMapper_Sachen8259.UpdateState;
var
	chrHigh: Byte;
	simpleMode: Boolean;
begin
	simpleMode := (regs[7] and $01) = $01;
	case ((regs[7] shr 1) and $03) of
		0: SetMirroringType(MIRROR_VERTICAL);
		1: SetMirroringType(MIRROR_HORIZONTAL);
		2: SetNametables(0, 1, 1, 1);
		3: SetMirroringType(MIRROR_SCREENAONLY);
	end;

	SelectPRGPage(0, regs[5]);

	if not HasChrRam then
	begin
		chrHigh := regs[4] shl 3;
		SelectCHRPage(0, (chrHigh or regs[0]) shl shift);
		SelectCHRPage(1, ((chrHigh or (regs[IfThen(simpleMode, 0, 1)])) shl shift) or chrOr[0]);
		SelectCHRPage(2, ((chrHigh or (regs[IfThen(simpleMode, 0, 2)])) shl shift) or chrOr[1]);
		SelectCHRPage(3, ((chrHigh or (regs[IfThen(simpleMode, 0, 3)])) shl shift) or chrOr[2]);
	end;
end;

procedure TMapper_Sachen8259.WriteRegister(addr: Word; value: Byte);
begin
	case (addr and $C101) of
		$4100: begin currentReg := value and $07; end;
		$4101: begin regs[currentReg] := value and $07; UpdateState; end;
	end;
end;

end.

