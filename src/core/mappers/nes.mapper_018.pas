unit NES.Mapper_018;

// Mapper 018: Jaleco SS88006

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper;

type
	TMapper_018 = class(TMapper)
	const
		irqMask: array[0..3] of Word = ( $FFFF, $0FFF, $00FF, $000F );
	protected
		irqCounter:     Word;
		irqCounterSize: Byte;
		irqEnabled:     Boolean;
		prgBanks: array [0..2] of Byte;
		chrBanks: array [0..7] of Byte;
		irqReloadValue: array [0..3] of Byte;

		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;

		procedure UpdatePrgBank(bankNumber, value: Byte; updateUpperBits: Boolean);
		procedure UpdateChrBank(bankNumber, value: Byte; updateUpperBits: Boolean);
		procedure ReloadIrqCounter;
		procedure ClockIrqCounter;

		procedure InitMapper; override;
		procedure WriteRegister(addr: Word;  value: Byte); override;
	public
		procedure ProcessCpuClock; override;

		constructor Create(cartridge: TCartridge); override;
	end;


implementation

uses NES.CPU;

{ TMapper_018 }

function TMapper_018.GetPRGPageSize: Word;
begin
	Result := $2000;
end;

function TMapper_018.GetCHRPageSize: Word;
begin
	Result := $0400;
end;

constructor TMapper_018.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(16, @irqCounter);
	RegisterProperty( 8, @irqCounterSize);
	RegisterProperty( 1, @irqEnabled);

	RegisterArray(Length(prgBanks), @prgBanks[0]);
	RegisterArray(Length(chrBanks), @chrBanks[0]);
	RegisterArray(Length(irqReloadValue), @irqReloadValue[0]);
end;

procedure TMapper_018.InitMapper;
begin
	FillByte(prgBanks[0], SizeOf(prgBanks), 0);
	FillByte(chrBanks[0], SizeOf(chrBanks), 0);
	FillByte(irqReloadValue[0], SizeOf(irqReloadValue), 0);

	irqCounter := 0;
	irqCounterSize := 0;
	irqEnabled := False;

	SelectPRGPage(3, -1);
end;

procedure TMapper_018.UpdatePrgBank(bankNumber, value: Byte; updateUpperBits: Boolean);
begin
	if updateUpperBits then
		prgBanks[bankNumber] := (prgBanks[bankNumber] and $0F) or (value shl 4)
	else
		prgBanks[bankNumber] := (prgBanks[bankNumber] and $F0) or value;

	SelectPRGPage(bankNumber, prgBanks[bankNumber]);
end;

procedure TMapper_018.UpdateChrBank(bankNumber, value: Byte; updateUpperBits: Boolean);
begin
	if updateUpperBits then
		chrBanks[bankNumber] := (chrBanks[bankNumber] and $0F) or (value shl 4)
	else
		chrBanks[bankNumber] := (chrBanks[bankNumber] and $F0) or value;

	SelectCHRPage(bankNumber, chrBanks[bankNumber]);
end;

procedure TMapper_018.ProcessCpuClock;
begin
	//Clock irq counter every memory read/write (each cpu cycle either reads or writes memory)
	ClockIrqCounter;
end;

procedure TMapper_018.ReloadIrqCounter;
begin
	irqCounter := irqReloadValue[0] or (irqReloadValue[1] shl 4) or
		(irqReloadValue[2] shl 8) or (irqReloadValue[3] shl 12);
end;

procedure TMapper_018.ClockIrqCounter;
var
	counter: Word;
begin
	if irqEnabled then
	begin
		counter := irqCounter and irqMask[irqCounterSize];
		{$R-}Dec(counter);
		if counter = 0 then
			CPUSetIRQSource(irqExternal);
		irqCounter := (irqCounter and (not irqMask[irqCounterSize])) or
			(counter and irqMask[irqCounterSize]);
	end;
end;

procedure TMapper_018.WriteRegister(addr: Word; value: Byte);
var
	updateUpperBits: Boolean;
begin
	updateUpperBits := (addr and $01) = $01;
	value := value and $0F;

	case (addr and $F003) of
		$8000, $8001: UpdatePrgBank(0, value, updateUpperBits);
		$8002, $8003: UpdatePrgBank(1, value, updateUpperBits);
		$9000, $9001: UpdatePrgBank(2, value, updateUpperBits);
		$A000, $A001: UpdateChrBank(0, value, updateUpperBits);
		$A002, $A003: UpdateChrBank(1, value, updateUpperBits);
		$B000, $B001: UpdateChrBank(2, value, updateUpperBits);
		$B002, $B003: UpdateChrBank(3, value, updateUpperBits);
		$C000, $C001: UpdateChrBank(4, value, updateUpperBits);
		$C002, $C003: UpdateChrBank(5, value, updateUpperBits);
		$D000, $D001: UpdateChrBank(6, value, updateUpperBits);
		$D002, $D003: UpdateChrBank(7, value, updateUpperBits);

		$E000..$E003:
			irqReloadValue[addr and $03] := value;

		$F000:
		begin
			CPUClearIRQSource(irqExternal);
			ReloadIrqCounter;
		end;

		$F001:
		begin
			CPUClearIRQSource(irqExternal);
			irqEnabled := ((value and $01) and $01) = $01;
			if (value and $08) <> 0 then
				irqCounterSize := 3  //4-bit counter
			else if (value and $04) <> 0 then
				irqCounterSize := 2  //8-bit counter
			else if(value and $02) <> 0 then
				irqCounterSize := 1  //12-bit counter
			else
				irqCounterSize := 0; //16-bit counter
		end;

		$F002:
			GenericChooseMirroringType(value and $03);

		$F003:
			; // Expansion audio, not supported yet
	end;
end;

end.
