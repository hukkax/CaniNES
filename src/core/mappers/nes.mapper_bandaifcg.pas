unit NES.Mapper_BandaiFCG;

// Mapper 016/153/157/159: Bandai FCG

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper,
	NES.BaseEeprom24C0X;

type
	TEeprom24C01 = class(TEeprom24C0X);
	TEeprom24C02 = class(TEeprom24C0X);

	TMapper_BandaiFCG = class(TMapper)
	private
		irqEnabled: Boolean;
		irqCounter,
		irqReload: Word;
		prgPage,
		prgBankSelect: Byte;
		chrRegs: array [0..7] of Byte;

		standardEeprom, extraEeprom: TEeprom24C0X;
		// sharedptr<DatachBarcodeReader> barcodeReader; TODO

	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;

		function  RegisterStartAddress: Word; override;
		function  RegisterEndAddress: Word; override;
		function  AllowRegisterRead: Boolean; override;

		function  ReadRegister(addr: Word): Byte; override;
		procedure WriteRegister(addr: Word;  value: Byte); override;

		procedure InitMapper; override;
	public
		procedure ProcessCpuClock; override;
		procedure SaveBattery; override;

		constructor Create(cartridge: TCartridge); override;
		destructor  Destroy; override;
	end;


implementation

uses
	Basement.Util, NES.Console, NES.CPU;

{ TMapper_BandaiFCG }

function TMapper_BandaiFCG.GetPRGPageSize: Word;       begin Result := $4000; end;
function TMapper_BandaiFCG.GetCHRPageSize: Word;       begin Result := $0400; end;
function TMapper_BandaiFCG.RegisterStartAddress: Word; begin Result := $6000; end;
function TMapper_BandaiFCG.RegisterEndAddress:   Word; begin Result := $FFFF; end;
function TMapper_BandaiFCG.AllowRegisterRead: Boolean; begin Result := True;  end;

constructor TMapper_BandaiFCG.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	standardEeprom := nil;
	extraEeprom    := nil;

	RegisterProperty(1,  @irqEnabled);
	RegisterProperty(16, @irqCounter);
	RegisterProperty(16, @irqReload);
	RegisterProperty(8,  @prgPage);
	RegisterProperty(8,  @prgBankSelect);

	RegisterArray(Length(chrRegs), @chrRegs[0]);
end;

destructor TMapper_BandaiFCG.Destroy;
begin
	standardEeprom.Free;
	extraEeprom.Free;

	inherited Destroy;
end;

procedure TMapper_BandaiFCG.InitMapper;
begin
	ClearArray(chrRegs);

	irqEnabled := False;
	irqCounter := 0;
	irqReload  := 0;
	prgPage    := 0;
	prgBankSelect := 0;

	// Only allow reads from $6000 to $7FFF
	RemoveRegisterRange($8000, $FFFF, TMemoryOperation.moRead);

	case romInfo.MapperID of

		157:  // Datach Joint ROM System boards
		begin
			{ // TODO
 			barcodeReader.reset(new DatachBarcodeReader(console));
			mapperControlDevice := barcodeReader;
			}

			// "It contains an internal 256-byte serial EEPROM (24C02) that is shared among all Datach games."
			// "One game, Battle Rush: Build up Robot Tournament, has an additional external 128-byte serial EEPROM (24C01) on the game cartridge."
			// "The NES 2.0 header's PRG-NVRAM field will only denote whether the game cartridge has an additional 128-byte serial EEPROM"
			if (not IsNes20) or (romInfo.NesHeader.GetSaveRamSize = 128) then
				extraEeprom := TEeprom24C01.Create;

			// All mapper 157 games have an internal 256-byte EEPROM
			standardEeprom := TEeprom24C02.Create;
		end;

		159:  // LZ93D50 with 128 byte serial EEPROM (24C01)
			standardEeprom := TEeprom24C01.Create;

		16:   // "INES Mapper 016 submapper 4: FCG-1/2 ASIC, no serial EEPROM, banked CHR-ROM"
		begin // "INES Mapper 016 submapper 5: LZ93D50 ASIC and no or 256-byte serial EEPROM, banked CHR-ROM"
			// Add a 256 byte serial EEPROM (24C02)̇:
			// Connect a 256-byte EEPROM for iNES roms, and when submapper 5 + 256 bytes of save ram in header
			if (not IsNes20) or ((romInfo.SubMapperID = 5) and
				(romInfo.NesHeader.GetSaveRamSize = 256)) then
					standardEeprom := TEeprom24C02.Create;

			case romInfo.SubMapperID of
				4: RemoveRegisterRange($8000, $FFFF, moWrite);
				5: RemoveRegisterRange($6000, $7FFF, moWrite);
			end;
		end;

	end;

	if romInfo.MapperID <> 16 then
	begin
		// "For iNES Mapper 153 (with SRAM), the writeable ports
		// must only be mirrored across $8000-$FFFF."
		// "Mappers 157 and 159 do not need to support the FCG-1 and -2
		// and so should only mirror the ports across $8000-$FFFF."
		if romInfo.MapperID = 153 then
			// Mapper 153 has regular save ram from $6000-$7FFF,
			// need to remove the register for both read and writes
			RemoveRegisterRange($6000, $7FFF, moAny)
		else
			RemoveRegisterRange($6000, $7FFF, moWrite);
	end;

	SelectPRGPage(1, $0F); // Last bank
end;

procedure TMapper_BandaiFCG.SaveBattery;
begin
	inherited;	// !!! TODO
	Exit;

	if Assigned(standardEeprom) then
		standardEeprom.SaveBattery;

	if Assigned(extraEeprom) then
		extraEeprom.SaveBattery
	else
		// Do not call BaseMapper.SaveBattery when the extra EEPROM exists
		// (prevent unused .sav file from being created)
		inherited SaveBattery;
end;

procedure TMapper_BandaiFCG.ProcessCpuClock;
begin
	// Checking counter before decrementing seems to be the only way to get both
	// Famicom Jump II - Saikyou no 7 Nin (J) and Magical Taruruuto-kun 2 - Mahou Daibouken (J)
	// to work without glitches with the same code.
	if irqEnabled then
	begin
		if irqCounter = 0 then
			CPUSetIRQSource(irqExternal);
		{$R-}Dec(irqCounter);
	end;
end;

function TMapper_BandaiFCG.ReadRegister(addr: Word): Byte;
begin
	Result := 0;

// TODO
//	if barcodeReader then
//		output := output or barcodeReader.GetOutput;

	if Assigned(extraEeprom) and Assigned(standardEeprom) then
		Result := Result or (standardEeprom.Read and extraEeprom.Read) shl 4
	else
	if Assigned(standardEeprom) then
		Result := Result or (standardEeprom.Read shl 4);

	Result := Result or Console.MemoryManager.GetOpenBus($E7);
end;

procedure TMapper_BandaiFCG.WriteRegister(addr: Word; value: Byte);
var
	i: Integer;
	scl, sda: Byte;
begin
	case (addr and $000F) of

		$00..$07:
		begin
			chrRegs[addr and $07] := value;
			if (romInfo.MapperID = 153) or (GetPRGPageCount >= $20) then
			begin
				prgBankSelect := 0;
				for i := 0 to 7 do
					prgBankSelect := prgBankSelect or (chrRegs[i] and $01) shl 4;
				SelectPRGPage(0, prgPage or prgBankSelect);
				SelectPRGPage(1, $0F or prgBankSelect);
			end
			else
			if (not HasChrRam) and (romInfo.MapperID <> 157) then
				SelectCHRPage(addr and $07, value);

			if (Assigned(extraEeprom)) and (romInfo.MapperID = 157) and ((addr and $0F) <= 3) then
				extraEeprom.WriteScl((value shr 3) and $01);
		end;

		$08:
		begin
			prgPage := value and $0F;
			SelectPRGPage(0, prgPage or prgBankSelect);
		end;

		$09:
			GenericChooseMirroringType(value);

		$0A:
		begin
			irqEnabled := (value and $01) = $01;

			// Wiki claims there is no reload value, however this seems to be the only way
			// to make Famicom Jump II - Saikyou no 7 Nin work properly
			if (romInfo.MapperID <> 16) or (not IsNes20) or (romInfo.SubMapperID = 5) then
			// "On the LZ93D50 (Submapper 5), writing to this register
			// also copies the latch to the actual counter."
				irqCounter := irqReload;

			CPUClearIRQSource(irqExternal);
		end;

		$0B:
		begin
			if (romInfo.MapperID <> 16) or (not IsNes20) or (romInfo.SubMapperID <> 4) then
				// "On the LZ93D50 (Submapper 5), these registers instead modify a latch that
				// will only be copied to the actual counter when register $800A is written to."
				irqReload := (irqReload and $FF00) or value
			else
				// "On the FCG-1/2 (Submapper 4), writing to these two registers directly
				// modifies the counter itself; all such games therefore disable counting
				// before changing the counter value."
				irqCounter := (irqCounter and $FF00) or value;
		end;

		$0C:
		begin
			if (romInfo.MapperID <> 16) or (not IsNes20) or (romInfo.SubMapperID <> 4) then
				irqReload  := (irqReload and $FF) or (value shl 8)
			else
				irqCounter := (irqCounter and $FF00) or value;
		end;

		$0D:
		begin
			if (romInfo.MapperID = 153) then
			begin
				SetCpuMemoryMapping($6000, $7FFF, 0,
					ChoosePrgMemoryType(HasBattery, PrgSaveRam, PrgWorkRam),
					ChooseMemoryAccessType((value and $20) <> 0, maReadWrite, maNoAccess));
			end
			else
			begin
				scl := (value and $20) shr 5;
				sda := (value and $40) shr 6;
				if Assigned(standardEeprom) then
					standardEeprom.Write(scl, sda);
				if Assigned(extraEeprom) then
					extraEeprom.WriteSda(sda);
			end;
		end;

	end;
end;

end.
