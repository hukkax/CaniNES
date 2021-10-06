unit NES.Mapper_Namco163;

interface

{$mode objfpc}

uses
	NES.Types, NES.Cartridge, NES.Mapper,
	NES.APU.Namco163;

type
	{$minEnumSize 1}
	TNamcoVariant = ( Namco163, Namco175, Namco340, Unknown );
	{$minEnumSize normal}

	TMapper_Namco163 = class(TMapper)
	private
		Audio: TNamco163Audio;

		variant: TNamcoVariant;

		notNamco340,
		autoDetectVariant: Boolean;

		lowChrNtMode,
		highChrNtMode: Boolean;
		writeProtect: Byte;
		irqCounter: Word;

		procedure SetVariant(newVariant: TNamcoVariant);
		procedure UpdateSaveRamAccess;
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;
		function  GetSaveRamPageSize: Cardinal; override;
		function  AllowRegisterRead: Boolean; override;

		procedure InitMapper; override;

		procedure WriteRegister(addr: Word;  value: Byte); override;
		function  ReadRegister(addr: Word): Byte; override;
	public
		procedure ProcessCpuClock; override;
		procedure WriteRAM(addr: Word; value: Byte); override;

		procedure LoadBattery; override;
		procedure SaveBattery; override;
		procedure LoadSnapshot; override;
		procedure SaveSnapshot; override;

		function  GetSoundChipName: AnsiString; override;

		constructor Create(cartridge: TCartridge); override;
		destructor  Destroy; override;
	end;


implementation

uses
	SysUtils, Math, NES.CPU, Basement.Util;

{ TMapper_Namco163 }

function TMapper_Namco163.GetPRGPageSize:     Word;     begin Result := $2000; end;
function TMapper_Namco163.GetCHRPageSize:     Word;     begin Result := $400;  end;
function TMapper_Namco163.GetSaveRamPageSize: Cardinal; begin Result := $800;  end;
function TMapper_Namco163.AllowRegisterRead:  Boolean;  begin Result := True;  end;
function TMapper_Namco163.GetSoundChipName: AnsiString; begin Result := 'Namco 163'; end;

constructor TMapper_Namco163.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	Audio := TNamco163Audio.Create;

	RegisterProperty(8, @variant);
	RegisterProperty(1, @notNamco340);
	RegisterProperty(1, @autoDetectVariant);
	RegisterProperty(8, @writeProtect);
	RegisterProperty(1, @lowChrNtMode);
	RegisterProperty(1, @highChrNtMode);
	RegisterProperty(16, @irqCounter);
end;

destructor TMapper_Namco163.Destroy;
begin
	Audio.Free;

	inherited Destroy;
end;

procedure TMapper_Namco163.SetVariant(newVariant: TNamcoVariant);
begin
	if autoDetectVariant then
		if (not notNamco340) or (variant <> Namco340) then
			variant := newVariant;
end;

procedure TMapper_Namco163.UpdateSaveRamAccess;
var
	memType: TPrgMemoryType;
	globalWriteEnable: Boolean;
begin
	memType := ChoosePrgMemoryType(HasBattery, PrgSaveRam, PrgWorkRam);

	case variant of

		Namco163:
		begin
			globalWriteEnable := (writeProtect and $40) = $40;
			SetCpuMemoryMapping($6000, $67FF, 0, memType, ChooseMemoryAccessType(
				(globalWriteEnable) and ((writeProtect and 1) = 0), maReadWrite, maRead));
			SetCpuMemoryMapping($6800, $6FFF, 1, memType, ChooseMemoryAccessType(
				(globalWriteEnable) and ((writeProtect and 2) = 0), maReadWrite, maRead));
			SetCpuMemoryMapping($7000, $77FF, 2, memType, ChooseMemoryAccessType(
				(globalWriteEnable) and ((writeProtect and 4) = 0), maReadWrite, maRead));
			SetCpuMemoryMapping($7800, $7FFF, 3, memType, ChooseMemoryAccessType(
				(globalWriteEnable) and ((writeProtect and 8) = 0), maReadWrite, maRead));
		end;

		Namco175:
			SetCpuMemoryMapping($6000, $7FFF, 0, memType, ChooseMemoryAccessType(
				Odd(writeProtect), maReadWrite, maRead));

		else
			SetCpuMemoryMapping($6000, $7FFF, 0, memType, maNoAccess);
	end;
end;

procedure TMapper_Namco163.InitMapper;
begin
	case romInfo.MapperID of

		19:	begin
				variant := Namco163;
				autoDetectVariant := False;

				case romInfo.DatabaseInfo.Board of
					'NAMCOT-163': variant := Namco163;
					'NAMCOT-175': variant := Namco175;
					'NAMCOT-340': variant := Namco340;
					else          autoDetectVariant := True;
				end;
			end;

		210:
			case romInfo.SubMapperID of
				0: begin variant := Unknown;  autoDetectVariant := True;  end;
				1: begin variant := Namco175; autoDetectVariant := False; end;
				2: begin variant := Namco340; autoDetectVariant := False; end;
			end;

	end;

	notNamco340 := False;

	writeProtect := 0;
	lowChrNtMode := False;
	highChrNtMode := False;
	irqCounter := 0;

	AddRegisterRange($4800, $5FFF, moAny);
	RemoveRegisterRange($6000, $FFFF, moRead);

	SelectPRGPage(3, -1);
	UpdateSaveRamAccess;
end;

procedure TMapper_Namco163.LoadSnapshot;
begin
	inherited LoadSnapshot;

	Audio.LoadSnapshot;
	UpdateSaveRamAccess;
end;

procedure TMapper_Namco163.SaveSnapshot;
begin
	inherited SaveSnapshot;

	Audio.SaveSnapshot;
end;

procedure TMapper_Namco163.LoadBattery;
var
	batteryContent: TBytes;
begin
	if not HasBattery then Exit;

	SetLength(batteryContent{%H-}, saveRamSize + Audio.AudioRamSize);
	DoLoadBattery(battNormal, batteryContent, Length(batteryContent));

	CopyMemory(@saveRam[0], @batteryContent[0], saveRamSize);
	CopyMemory(Audio.GetInternalRam, @batteryContent[saveRamSize], Audio.AudioRamSize);
end;

procedure TMapper_Namco163.SaveBattery;
var
	batteryContent: TBytes;
begin
	if not HasBattery then Exit;

	SetLength(batteryContent{%H-}, saveRamSize + Audio.AudioRamSize);

	CopyMemory(@batteryContent[0], @saveRam[0], saveRamSize);
	CopyMemory(@batteryContent[saveRamSize], Audio.GetInternalRam, Audio.AudioRamSize);

	DoSaveBattery(battNormal, batteryContent, Length(batteryContent));
end;

procedure TMapper_Namco163.ProcessCpuClock;
begin
	if ((irqCounter and $8000) <> 0) and ((irqCounter and $7FFF) <> $7FFF) then
	begin
		Inc(irqCounter);
		if (irqCounter and $7FFF) = $7FFF then
			CPUSetIrqSource(irqExternal);
	end;

	if variant = Namco163 then
		Audio.Clock;
end;

procedure TMapper_Namco163.WriteRAM(addr: Word; value: Byte);
begin
	if InRange(addr, $6000, $7FFF) then
	begin
		notNamco340 := True;
		if variant = Namco340 then
			SetVariant(TNamcoVariant.Unknown);
	end;

	inherited WriteRAM(addr, value);
end;

function TMapper_Namco163.ReadRegister(addr: Word): Byte;
begin
	case (addr and $F800) of
		$4800: Result := Audio.ReadRegister(addr);
		$5000: Result := irqCounter and $FF;
		$5800: Result := irqCounter shr 8;
		else   Result := inherited ReadRegister(addr);
	end;
end;

procedure TMapper_Namco163.WriteRegister(addr: Word; value: Byte);
var
	bankNumber: Byte;
begin
	addr := addr and $F800;

	case addr of

		$4800:
		begin
			SetVariant(Namco163);
			Audio.WriteRegister(addr, value);
		end;

		$5000:
		begin
			SetVariant(Namco163);
			irqCounter := (irqCounter and $FF00) or value;
			CPUClearIrqSource(irqExternal);
		end;

		$5800:
		begin
			SetVariant(Namco163);
			irqCounter := (irqCounter and $00FF) or (value shl 8);
			CPUClearIrqSource(irqExternal);
		end;

		$8000, $8800, $9000, $9800:
		begin
			bankNumber := (addr - $8000) shr 11;
			if (not lowChrNtMode) and (value >= $E0) and (variant = Namco163) then
				SelectCHRPage(bankNumber, value and $01, ChrNametableRam)
			else
				SelectCHRPage(bankNumber, value);
		end;

		$A000, $A800, $B000, $B800:
		begin
			bankNumber := ((addr - $A000) shr 11) + 4;
			if (not highChrNtMode) and (value >= $E0) and (variant = Namco163) then
				SelectCHRPage(bankNumber, value and $01, ChrNametableRam)
			else
				SelectCHRPage(bankNumber, value);
		end;

		$C000, $C800, $D000, $D800:
		begin
			if addr >= $C800 then
				SetVariant(Namco163)
			else
			if variant <> Namco163 then
				SetVariant(Namco175);

			if variant = Namco175 then
			begin
				writeProtect := value;
				UpdateSaveRamAccess;
			end
			else
			begin
				bankNumber := ((addr - $C000) shr 11) + 8;
				if value >= $E0 then
					SelectCHRPage(bankNumber, value and $01, ChrNametableRam)
				else
					SelectCHRPage(bankNumber, value);
			end;
		end;

		$E000:
		begin
			if (value and $80) = $80 then
				SetVariant(Namco340)
			else
			if ((value and $40) = $40) and (variant <> Namco163) then
				SetVariant(Namco340);

			SelectPRGPage(0, value and $3F);

			if(variant = Namco340) then
			begin
				case ((value and $C0) shr 6) of
					0: SetMirroringType(MIRROR_SCREENAONLY);
					1: SetMirroringType(MIRROR_VERTICAL);
					2: SetMirroringType(MIRROR_HORIZONTAL);
					3: SetMirroringType(MIRROR_SCREENBONLY);
				end;
			end
			else
			if variant = Namco163 then
				Audio.WriteRegister(addr, value);
		end;

		$E800:
		begin
			SelectPRGPage(1, value and $3F);
			if variant = Namco163 then
			begin
				lowChrNtMode  := (value and $40) = $40;
				highChrNtMode := (value and $80) = $80;
			end;
		end;

		$F000:
			SelectPRGPage(2, value and $3F);

		$F800:
		begin
			SetVariant(Namco163);
			if variant = Namco163 then
			begin
				writeProtect := value;
				UpdateSaveRamAccess;

				Audio.WriteRegister(addr, value);
			end;
		end;

	end;
end;


end.
