unit NES.Mapper_150;

// Mapper 150/243: Sachen SA-015

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper;

type
	TMapper_150 = class(TMapper)
	private
		currentRegister: Byte;
		regs: array [0..7] of Byte;
	protected
		function  GetDipSwitchCount: Cardinal; override;
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;
		function  RegisterStartAddress: Word; override;
		function  RegisterEndAddress: Word; override;
		function  AllowRegisterRead: Boolean; override;

		function  ReadRegister(addr: Word): Byte; override;
		procedure WriteRegister(addr: Word;  value: Byte); override;

		procedure InitMapper; override;
		procedure UpdateState;
	public
		constructor Create(cartridge: TCartridge); override;
	end;

	TMapper_243 = class(TMapper_150);


implementation

uses
	Basement.Util, Math, NES.Console;

{ TMapper_150 }

function TMapper_150.GetDipSwitchCount: Cardinal;
begin
	Result := IfThen(romInfo.MapperID = 150, 1, 0);
end;
function TMapper_150.GetPRGPageSize: Word;       begin Result := $8000; end;
function TMapper_150.GetCHRPageSize: Word;       begin Result := $2000; end;
function TMapper_150.RegisterStartAddress: Word; begin Result := $4100; end;
function TMapper_150.RegisterEndAddress: Word;   begin Result := $7FFF; end;
function TMapper_150.AllowRegisterRead: Boolean; begin Result := True;  end;

constructor TMapper_150.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterArray(Length(regs), @regs[0]);
end;

procedure TMapper_150.InitMapper;
begin
	currentRegister := 0;
	ClearArray(regs);
	UpdateState;
end;

procedure TMapper_150.UpdateState;
var
	chrPage: Byte;
begin
	if romInfo.MapperID = 150 then
		chrPage := ((regs[4] and $01) shl 2) or (regs[6] and $03)
	else
		chrPage := (regs[2] and $01) or ((regs[4] and $01) shl 1) or ((regs[6] and $03) shl 2);

	SelectCHRPage(0, chrPage);
	SelectPRGPage(0, regs[5] and $03);

	case ((regs[7] shr 1) and $03) of
		0: SetNametables(0, 0, 0, 1);
		1: SetMirroringType(MIRROR_HORIZONTAL);
		2: SetMirroringType(MIRROR_VERTICAL);
		3: SetMirroringType(MIRROR_SCREENAONLY);
	end;
end;

function TMapper_150.ReadRegister(addr: Word): Byte;
var
	openBus: Byte;
begin
	openBus := Console.MemoryManager.GetOpenBus;
	Result := openBus;

	if (addr and $C101) = $4101 then
	begin
		if (GetDipSwitches and $01) <> 0 then
			//"In the latter setting, the ASIC sees all writes as being OR'd with $04, while on reads, D2 is open bus."
			Result := (openBus and $FC) or (regs[currentRegister] and $03)
		else
			Result := (openBus and $F8) or (regs[currentRegister] and $07);
	end;
end;

procedure TMapper_150.WriteRegister(addr: Word; value: Byte);
begin
	//"In the latter setting, the ASIC sees all writes as being OR'd with $04, while on reads, D2 is open bus."
	if Odd(GetDipSwitches) then
		value := value or $04;

	case (addr and $C101) of
		$4100: currentRegister := value and $07;
		$4101: begin regs[currentRegister] := (value and $07); UpdateState; end;
	end;
end;

end.
