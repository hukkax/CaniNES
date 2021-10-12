unit NES.PPU;

interface

{$MODE DELPHI}
{$MACRO ON}
{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}

{$I canines.inc}

uses
	Graphics32,
	NES.Types, NES.Config,
	NES.MemoryHandler, NES.Cartridge,
	NES.CPU, NES.Palette, NES.NTSC;

const
	InputPollScanline = 220;

type
	PPURegisters = (
		Control = $00,    Mask,            Status,
		SpriteAddr,       SpriteData,      ScrollOffsets,
		VideoMemoryAddr,  VideoMemoryData,
		SpriteDMA = $4014
	);

	TPPUControlFlags = record
		VerticalWrite: Boolean;
		SpritePatternAddr,
		BackgroundPatternAddr: Word;
		LargeSprites,
		VBlank: Boolean;

		Grayscale,
		BackgroundMask, SpriteMask,
		BackgroundEnabled, SpritesEnabled,
		IntensifyRed, IntensifyGreen, IntensifyBlue: Boolean;
	end;

	TPPUStatusFlags = record
		SpriteOverflow,
		Sprite0Hit,
		VerticalBlank: Boolean;
	end;

	TPPUState = record
		Control, Mask, Status: Byte;
		SpriteRamAddr: Byte;
		VideoRamAddr: Word;
		XScroll: Byte;
		TmpVideoRamAddr: Word;
		WriteToggle: Boolean;
		HighBitShift, LowBitShift: Word;
	end;

	TTileInfo = record
		LowByte, HighByte: Byte;
		PaletteOffset: Byte;
		TileAddr: Word;
	end;

	TSpriteInfo = record
		LowByte, HighByte: Byte;
		PaletteOffset: Byte;
		TileAddr: Word;

		//VerticalMirror,
		HorizontalMirror,
		BackgroundPriority: Boolean;
		SpriteX: Byte;
	end;
	PSpriteInfo = ^TSpriteInfo;

	TPPU = class(TIMemoryHandler)
	const
		ScreenWidth  = 256;
		ScreenHeight = 240;
		PixelCount = ScreenWidth * ScreenHeight - 1;
		OamDecayCycleCount = 3000;
	private
		NtscSetup:  TNtscSetup;
		NtscFilter: TNtscFilter;

		state: TPPUState;

		Control, Mask, Status: Byte;
		SpriteRamAddr: Byte;
		VideoRamAddr: Word;
		XScroll: Byte;
		TmpVideoRamAddr: Word;
		WriteToggle: Boolean;
		HighBitShift, LowBitShift: Word;

		PixPtr: PWord;

		MasterClock: QWord;
		MasterClockDivider,
		MemoryReadBuffer: Byte;

		spriteRAM:  array [Byte] of Byte;
		secondarySpriteRAM: array [0..$1F] of Byte;
		hasSprite:  array [0..256] of Boolean;

		currentOutputBuffer: array[0..PixelCount] of Word;

		nesModel: TNesModel;
		standardVblankEnd,
		standardNmiScanline,
		vblankEnd,
		nmiScanline,
		palSpriteEvalScanline: Word;

		flags: TPPUControlFlags;
		statusFlags: TPPUStatusFlags;

		intensifyColorBits: Word;
		paletteRamMask: Byte;
		lastUpdatedPixel: Int32;

		ppuBusAddress: Word;
		currentTile, nextTile, previousTile: TTileInfo;

		spriteTiles: array [0..63] of TSpriteInfo;
		spriteCount,
		secondaryOAMAddr: Cardinal;
		sprite0Visible: Boolean;
		lastSprite: PSpriteInfo;
		firstVisibleSpriteAddr,
		lastVisibleSpriteAddr: Byte;
		spriteIndex: Cardinal;

		palIndex: Byte;

		openBus: Byte;
		openBusDecayStamp: array [0..7] of Int32;
		ignoreVramRead: Cardinal;

		spriteAddrH, spriteAddrL,
		overflowBugCounter,
		oamCopybuffer: Byte;
		oamCopyDone,
		spriteInRange,
		sprite0Added: Boolean;

		needStateUpdate,
		RenderingEnabled, prevRenderingEnabled,
		preventVblFlag: Boolean;

		updateVramAddr: Word;
		updateVramAddrDelay: Byte;

		minimumDrawBgCycle,
		minimumDrawSpriteCycle,
		minimumDrawSpriteStandardCycle: Cardinal;

		burstphase: Byte;

		oamDecayCycles: array [0..$3F] of QWord;
		corruptOamRow:  array [0..31]  of Boolean;

		function  GetRegisterID(addr: Word): PPURegisters; inline;

		procedure SetControlRegister(value: Byte);
		procedure SetMaskRegister(value: Byte);

		procedure ProcessTmpAddrScrollGlitch(normalAddr, value, mask: Word); inline;

		procedure SetOpenBus(mask, value: Byte);
		function  ApplyOpenBus(mask, value: Byte): Byte; inline;
		procedure ProcessStatusRegOpenBus(var openBusMask, returnValue: Byte); inline;

		procedure UpdateVideoRamAddr; inline;
		procedure IncVerticalScrolling;
		procedure IncHorizontalScrolling;
		function  GetNameTableAddr: Word; inline;
		function  GetAttributeAddr: Word; inline;

		procedure ProcessScanline; inline;
		procedure ProcessSpriteEvaluation; inline;
		procedure BeginVBlank; inline;
		procedure TriggerNmi; inline;

		procedure LoadTileInfo;
		procedure LoadSprite(spriteY, tileIndex, attributes, spriteX: Byte; extraSprite: Boolean);
		procedure LoadSpriteTileInfo; inline;
		procedure LoadExtraSprites;
		procedure ShiftTileRegisters; inline;

		procedure SetOamCorruptionFlags; inline;
		procedure ProcessOamCorruption; inline;

		function  GetPixelColor: Byte; inline;
		procedure DrawPixel; inline;

		procedure SendFrame; inline;

		procedure UpdateStatusFlag; inline;
		procedure UpdateGrayscaleAndIntensifyBits;
		procedure UpdateMinimumDrawCycles; inline;
		procedure UpdateState;
		procedure UpdateApuStatus; inline;

		procedure SetBusAddress(address: Word); inline;
		function  ReadVram(address: Word; kind: TMemoryOperationType = memopPpuRenderingRead): Byte; inline;
		procedure WriteVram(address: Word; value: Byte); inline;
		function  ReadSpriteRam(addr: Byte): Byte; inline;
		procedure WriteSpriteRam(addr, value: Byte); inline;

	public
		Settings:       TPPUConfig;

		paletteRAM:     array [0..$1F] of Byte;

		scanline:       Int32;
		cycle,
		frameCount:     Cardinal;
		frame_complete: Boolean;
		frameOdd:       Boolean;  // are we in an "odd frame" (one cycle shorter)?
		A13pinLowSum:   Byte;

		Palette:        TNESPaletteClass;
		cart:           TCartridge;
		Framebuffer:    TBitmap32;  // graphics output pixel buffer

		procedure Reset;

		procedure UpdatePalette;

		function  PeekRAM(addr: Word): Byte; override;
		function  ReadRAM(addr: Word): Byte; override;
		procedure WriteRAM(addr: Word; value: Byte); override;
		function  ReadPaletteRAM(addr: Word): Byte; inline;
		procedure WritePaletteRAM(addr: Word; value: Byte);

		procedure Exec;
		procedure Run(runTo: QWord); inline;

		function  GetFrameCycle: Cardinal; inline;
		function  GetOverclockRate: Double;
		function  GetCurrentBgColor: Word;
		function  GetPixelBrightness(X, Y: Byte): Cardinal;

		procedure SetNesModel(model: TNesModel);
		procedure ConnectCartridge(const ACartridge: TCartridge);

		procedure GetPatternTable(i, palettenum: Byte; X, Y: Int32);
		//function  GetColourFromPaletteRam(palettenum, pixel: Byte): Cardinal; inline;

		procedure FillFramebuffer;

		procedure ConfigureNTSCFilter(const NTSCConfig: TFilterNTSCConfig);
		procedure FillNTSCbuffer(NTSCbuffer: TBitmap32); inline;

		constructor Create(const Buffer: TBitmap32); overload;
		destructor  Destroy; override;

		procedure GetMemoryRanges(var ranges: TMemoryRanges); override;
		procedure WriteDebug;
		procedure DumpRAM(const Filename: String);

		procedure LoadSnapshot; override;
		procedure SaveSnapshot; override;
	end;

{$IFDEF MEASURETIMING}
var
	PPUTimeTaken: Single = 0;
{$ENDIF}

implementation

uses
	SysUtils, Math, Classes,
	{$IFDEF MEASURETIMING_PPU} Basement.Timing, {$ENDIF}
	Basement.Util, Basement.Window,
	NES.Console, NES.Mapper;

	{$INCLUDE coredefs.inc}

{$IFDEF MEASURETIMING_PPU}
var
	PPUTiming: TTimeMeasurer;
{$ENDIF}

// ================================================================================================
// ================================================================================================
// New PPU (Mesen)
// ================================================================================================
// ================================================================================================

constructor TPPU.Create(const Buffer: TBitmap32);
begin
	inherited Create('PPU');

	RegisterProperty(8,  @paletteRamMask);
	RegisterProperty(16, @intensifyColorBits);

	RegisterProperty(32, @scanline);
	RegisterProperty(32, @cycle);
	RegisterProperty(32, @frameCount);
	RegisterProperty(8,  @memoryReadBuffer);

	RegisterProperty(32, @spriteIndex);
	RegisterProperty(32, @spriteCount);
	RegisterProperty(32, @secondaryOAMAddr);
	RegisterProperty(1,  @sprite0Visible);
	RegisterProperty(8,  @oamCopybuffer);
	RegisterProperty(1,  @spriteInRange);
	RegisterProperty(1,  @sprite0Added);
	RegisterProperty(8,  @spriteAddrH);
	RegisterProperty(8,  @spriteAddrL);
	RegisterProperty(1,  @oamCopyDone);
	RegisterProperty(8,  @nesModel);
	RegisterProperty(1,  @prevRenderingEnabled);
	RegisterProperty(1,  @renderingEnabled);
	RegisterProperty(8,  @openBus);
	RegisterProperty(32, @ignoreVramRead);
	RegisterProperty(8,  @overflowBugCounter);
	RegisterProperty(16, @updateVramAddr);
	RegisterProperty(8,  @updateVramAddrDelay);
	RegisterProperty(1,  @needStateUpdate);
	RegisterProperty(16, @ppuBusAddress);
	RegisterProperty(1,  @preventVblFlag);
	RegisterProperty(64, @masterClock);

	Framebuffer := Buffer;

	// This should (presumably) persist across resets
	ClearArray(corruptOamRow);

	with Settings do
	begin
		PPUModel := Ppu2C02;

		DisablePaletteRead         := False;
		DisableOamAddrBug          := False;
		DisablePpu2004Reads        := False;
		EnablePpu2000ScrollGlitch  := False;
		EnablePpu2006ScrollGlitch  := False;
		EnablePpuOamRowCorruption  := False;
		EnableOamDecay             := False;
		RemoveSpriteLimit          := False;
		AdaptiveSpriteLimit        := False;
		ForceBackgroundFirstColumn := False;
		ForceSpritesFirstColumn    := False;
		ExtraScanlinesBeforeNmi    := 0;
		ExtraScanlinesAfterNmi     := 0;
	end;

	Palette := TNESPaletteClass.Create;
	Palette.Generate(1.0, 0.0, 1.0, 1.0, 1.8);

	SetNesModel(nesNTSC);
	Reset;

	ConfigureNTSCFilter(Configuration.Display.NTSC);
	NtscFilter := TNtscFilter.Create(NtscSetup);

	UpdatePalette;
end;

destructor TPPU.Destroy;
begin
	Palette.Free;
	NtscFilter.Free;

	inherited Destroy;
end;

procedure TPPU.UpdatePalette;
begin
	NtscSetup.base_palette := @Self.Palette.RawColors[0,0];
	NtscFilter.Init(NtscSetup);
end;

procedure TPPU.Reset;
const
	PaletteRamBootValues: array [$00..$1F] of Byte = (
		$09, $01, $00, $01, $00, $02, $02, $0D,
		$08, $10, $08, $24, $00, $00, $04, $2C,
		$09, $01, $34, $03, $00, $04, $00, $14,
		$08, $3A, $00, $02, $00, $20, $2C, $08 );
begin
	state       := default(TPPUState);
	flags       := default(TPPUControlFlags);
	statusFlags := default(TPPUStatusFlags);

	previousTile := default(TTileInfo);
	currentTile  := default(TTileInfo);
	nextTile     := default(TTileInfo);

	masterClock := 0;
	preventVblFlag := False;
	needStateUpdate := False;
	ppuBusAddress := 0;
	intensifyColorBits := 0;
	paletteRamMask := $3F;
	lastUpdatedPixel := -1;
	lastSprite := nil;
	spriteInRange := False;
	sprite0Added := False;
	spriteAddrH := 0;
	spriteAddrL := 0;
	oamCopybuffer := 0;
	oamCopyDone := False;
	RenderingEnabled := False;
	prevRenderingEnabled := False;
	spriteCount := 0;
	secondaryOAMAddr := 0;
	sprite0Visible := False;
	spriteIndex := 0;
	openBus := 0;
	frameCount := 1;
	memoryReadBuffer := 0;
	overflowBugCounter := 0;
	updateVramAddrDelay := 0;
	updateVramAddr := 0;
	ignoreVramRead := 0;

	//First execution will be cycle 0, scanline 0
	scanline := -1;
	cycle := 340;

	ClearArray(oamDecayCycles);
	ClearArray(openBusDecayStamp);

	ClearArray(spriteRAM);
	ClearArray(secondarySpriteRAM);
	CopyMemory(@paletteram[0], @PaletteRamBootValues[0], Length(PaletteRamBootValues)*1);

	ClearArray(hasSprite);
	FillByte(spriteTiles[0], Length(spriteTiles) * SizeOf(spriteTiles[0]), 0);

	UpdateMinimumDrawCycles;
end;

procedure TPPU.SetNesModel(model: TNesModel);
begin
	nesModel := model;

	case nesModel of

		nesNTSC:
		begin
			nmiScanline := 241;
			vblankEnd := 260;
			standardNmiScanline := 241;
			standardVblankEnd := 260;
			masterClockDivider := 4;
		end;

		nesPAL:
		begin
			nmiScanline := 241;
			vblankEnd := 310;
			standardNmiScanline := 241;
			standardVblankEnd := 310;
			masterClockDivider := 5;
		end;

		nesDendy:
		begin
			nmiScanline := 291;
			vblankEnd := 310;
			standardNmiScanline := 291;
			standardVblankEnd := 310;
			masterClockDivider := 5;
		end;

	end;

	Inc(nmiScanline, Settings.ExtraScanlinesBeforeNmi);
	palSpriteEvalScanline := nmiScanline + 24;
	Inc(standardVblankEnd, Settings.ExtraScanlinesBeforeNmi);
	Inc(vblankEnd, Settings.ExtraScanlinesAfterNmi + Settings.ExtraScanlinesBeforeNmi);
end;

procedure TPPU.ConnectCartridge(const ACartridge: TCartridge);
begin
	cart := Cartridge;

	{$IFDEF MEASURETIMING_PPU}
	PPUTiming.Init;
	{$ENDIF}
end;

function TPPU.GetOverclockRate: Double;
var
	regularVblankEnd: Word;
begin
	case nesModel of
		nesNTSC:  regularVblankEnd := 260;
		nesPAL:   regularVblankEnd := 310;
		nesDendy: regularVblankEnd := 310;
		else      regularVblankEnd := 260;
	end;
	Result := (vblankEnd + 2) / (regularVblankEnd + 2);
end;

function TPPU.GetFrameCycle: Cardinal;
begin
	Result := (Cardinal(scanline + 1) * 341) + cycle;
end;

procedure TPPU.GetMemoryRanges(var ranges: TMemoryRanges);
begin
	ranges.AddHandler(moRead,  $2000, $3FFF);
	ranges.AddHandler(moWrite, $2000, $3FFF);
	ranges.AddHandler(moWrite, $4014);
end;

function TPPU.GetRegisterID(addr: Word): PPURegisters;
begin
	if addr = $4014 then
		Result := PPURegisters.SpriteDMA
	else
		Result := PPURegisters(addr and $07);
end;

function TPPU.ReadSpriteRam(addr: Byte): Byte;
var
	elapsedCycles: QWord;
begin
	if not Settings.EnableOamDecay then
		Result := spriteRAM[addr]
	else
	begin
		elapsedCycles := CPUGetCycleCount - oamDecayCycles[addr >> 3];
		if elapsedCycles <= OamDecayCycleCount then
		begin
			oamDecayCycles[addr >> 3] := CPUGetCycleCount;
			Result := spriteRAM[addr];
		end
		else
			Result := $10; //simulate decay
	end;
end;

procedure TPPU.WriteSpriteRam(addr, value: Byte);
begin
	spriteRAM[addr] := value;
	if Settings.EnableOamDecay then
		oamDecayCycles[addr >> 3] := CPUGetCycleCount;
end;

function TPPU.ReadPaletteRAM(addr: Word): Byte;
begin
	addr := addr and $1F;
	if (addr = $10) or (addr = $14) or (addr = $18) or (addr = $1C) then
		addr := addr and (not $10);
	Result := paletteRAM[addr];
end;

procedure TPPU.WritePaletteRAM(addr: Word; value: Byte);
begin
	addr  := addr  and $1F;
	value := value and $3F;
	case addr of
		$00, $10: begin paletteRAM[$00] := value; paletteRAM[$10] := value; end;
		$04, $14: begin paletteRAM[$04] := value; paletteRAM[$14] := value; end;
		$08, $18: begin paletteRAM[$08] := value; paletteRAM[$18] := value; end;
		$0C, $1C: begin paletteRAM[$0C] := value; paletteRAM[$1C] := value; end;
		else paletteRAM[addr] := value;
	end;
end;

{function TPPU.GetColourFromPaletteRam(palettenum, pixel: Byte): Cardinal;
begin
//	Result := Palette.Colors[0, Read($3F00 + (palettenum shl 2) + (pixel and 3)) and $3F];
end;}

procedure TPPU.GetPatternTable(i, palettenum: Byte; X, Y: Int32);
var
	row, col, nTileX, nTileY: Int32;
	nOffset: Word;
	pixel, tile_lsb, tile_msb: Byte;
begin
	// Loop through all 16x16 tiles
	for nTileY := 0 to 15 do
	for nTileX := 0 to 15 do
	begin
		nOffset := nTileY * 256 + nTileX * 16;
		// Now loop through 8 rows of 8 pixels
		for row := 0 to 7 do
		begin
			tile_lsb := Mapper.ReadVRAM(i * $1000 + nOffset + row);
			tile_msb := Mapper.ReadVRAM(i * $1000 + nOffset + row + 8);
			for col := 0 to 7 do
			begin
				pixel := ((tile_msb and 1) << 1) or (tile_lsb and 1);
				tile_lsb := tile_lsb >> 1;
				tile_msb := tile_msb >> 1;

				Framebuffer.SetPixel(
					nTileX * 8 + (7 - col) + X, nTileY * 8 + row + Y,
					//GetColourFromPaletteRam(palette, pixel)
					Palette.Colors[0, paletteram[(Byte(palettenum << 2) or Byte(pixel and 3)) and $3F]]
					);
			end;
		end;
	end;
end;

procedure TPPU.SetBusAddress(address: Word);
begin
	ppuBusAddress := address;
	Mapper.NotifyVRAMAddressChange(address);
end;

procedure TPPU.UpdateVideoRamAddr;
begin
	if (scanline >= 240) or (not RenderingEnabled) then
	begin
		VideoRamAddr := (VideoRamAddr + IfThen(flags.VerticalWrite, 32, 1)) and $7FFF;
		//Trigger memory read when setting the vram address - needed by MMC3 IRQ counter
		//"Should be clocked when A12 changes to 1 via $2007 read/write"
		SetBusAddress(VideoRamAddr and $3FFF);
	end
	else
	begin
		//"During rendering (on the pre-render line and the visible lines 0-239, provided either background or sprite rendering is enabled), "
		//it will update v in an odd way, triggering a coarse X increment and a Y increment simultaneously"
		IncHorizontalScrolling;
		IncVerticalScrolling;
	end;
end;

procedure TPPU.UpdateStatusFlag;
begin
	Status :=
		(Byte(statusFlags.SpriteOverflow) << 5) or
		(Byte(statusFlags.Sprite0Hit)     << 6) or
		(Byte(statusFlags.VerticalBlank)  << 7);

	statusFlags.VerticalBlank := False;
	CPUClearNmiFlag;

	// "Reading one PPU clock before reads it as clear and never sets the flag or generates NMI for that frame."
	if (cycle = 0) and (scanline = nmiScanline) then
		preventVblFlag := True;
end;

procedure TPPU.ProcessTmpAddrScrollGlitch(normalAddr, value, mask: Word);
begin
	TmpVideoRamAddr := normalAddr;
	//Use open bus to set some parts of V (glitch that occurs when writing to $2000/$2005/$2006 on cycle 257)
	if (RenderingEnabled) and (Settings.EnablePpu2000ScrollGlitch) and
		(cycle = 257) and (scanline < 240) then
			VideoRamAddr := (VideoRamAddr and (not mask)) or (value and mask);
end;

procedure TPPU.SetOpenBus(mask, value: Byte);
var
	i: Int32;
	ob: Word;
begin
	// Decay expired bits, set new bits and update stamps on each individual bit
	if mask = $FF then
	begin
		// Shortcut when mask is $FF - all bits are set to the value and stamps updated
		openBus := value;
		for i := 0 to 7 do
			openBusDecayStamp[i] := frameCount;
	end
	else
	begin
		ob := (openBus << 8);
		for i := 0 to 7 do
		begin
			ob := ob >> 1;
			if Odd(mask) then
			begin
				if Odd(value) then
					ob := ob or $80
				else
					ob := ob and $FF7F;
				openBusDecayStamp[i] := frameCount;
			end
			else
			if (frameCount - openBusDecayStamp[i]) > 30 then
				ob := ob and $FF7F;
			value := value >> 1;
			mask  := mask  >> 1;
		end;
		openBus := Byte(ob and $FF);
	end;
end;

function TPPU.ApplyOpenBus(mask, value: Byte): Byte;
begin
	SetOpenBus(not mask, value);
	Result := value or (openBus and mask);
end;

procedure TPPU.ProcessStatusRegOpenBus(var openBusMask, returnValue: Byte);
begin
	case Settings.PPUModel of
		Ppu2C05A: returnValue := returnValue or $1B;
		Ppu2C05B: returnValue := returnValue or $3D;
		Ppu2C05C: returnValue := returnValue or $1C;
		Ppu2C05D: returnValue := returnValue or $1B;
		else      Exit;
	end;
	openBusMask := $00;
end;

function TPPU.ReadVram(address: Word; kind: TMemoryOperationType): Byte;
begin
	SetBusAddress(address);
	Result := Mapper.ReadVRAM(address, kind);
end;

procedure TPPU.WriteVram(address: Word; value: Byte);
begin
	SetBusAddress(address);
	Mapper.WriteVRAM(address, value);
end;

//Used by debugger to get register values without side-effects (heavily edited copy of ReadRAM)
//
function TPPU.PeekRAM(addr: Word): Byte;
var
	openBusMask: Byte = $FF;
begin
	Result := 0;
	case GetRegisterID(addr) of

		PPURegisters.Status:
		begin
			Result :=
				(Byte(statusFlags.SpriteOverflow) << 5) or
				(Byte(statusFlags.Sprite0Hit)     << 6) or
				(Byte(statusFlags.VerticalBlank)  << 7);
			// Clear vertical blank flag
			if (scanline = nmiScanline) and (cycle < 3) then
				Result := Result and $7F;
			openBusMask := $1F;
			ProcessStatusRegOpenBus(openBusMask, Result);
		end;

		PPURegisters.SpriteData: // $2004 OAMDATA
			if not Settings.DisablePpu2004Reads then
			begin
				if (RenderingEnabled) and (scanline <= 239) then
				begin
					if (cycle >= 257) and (cycle <= 320) then
					begin
						addr := (cycle - 257) div 8 * 4 +
							IfThen( ((cycle - 257) and 7) > 3, 3, ((cycle - 257) and 7) );
						Result := secondarySpriteRAM[addr];
					end
					else
						Result := oamCopybuffer;
				end
				else
					Result := spriteRAM[SpriteRamAddr];
				openBusMask := $00;
			end;

		PPURegisters.VideoMemoryData: // $2007 PPUDATA
		begin
			Result := memoryReadBuffer;
			if (not Settings.DisablePaletteRead) and
				((ppuBusAddress and $3FFF) >= $3F00) then
			begin
				Result := ReadPaletteRAM(VideoRamAddr) or (openBus and $C0);
				openBusMask := $C0;
			end
			else
				openBusMask := $00;
		end;

	end;
	Result := Result or (openBus and openBusMask);
end;

function TPPU.ReadRAM(addr: Word): Byte;
var
	openBusMask: Byte = $FF;
begin
	Result := 0;
	case GetRegisterID(addr) of

		PPURegisters.Status:
		begin
			WriteToggle := False;
			UpdateStatusFlag;
			Result := Status;
			openBusMask := $1F;
			ProcessStatusRegOpenBus(openBusMask, Result);
		end;

		PPURegisters.SpriteData: // $2004 OAMDATA
			if not Settings.DisablePpu2004Reads then
			begin
				if (RenderingEnabled) and (scanline <= 239) then
				begin
					// While the screen is being drawn
					if (cycle >= 257) and (cycle <= 320) then
					begin
						// If we're doing sprite rendering, set OAM copy buffer to its proper value
						secondaryOAMAddr := (cycle - 257) div 8 * 4 +
							IfThen( ((cycle - 257) and 7) > 3,  3,  ((cycle - 257) and 7) );
						oamCopybuffer := secondarySpriteRAM[secondaryOAMAddr];
					end;
					// Return the value that PPU is currently using for sprite evaluation/rendering
					Result := oamCopybuffer;
				end
				else
					Result := ReadSpriteRam(SpriteRamAddr);
				openBusMask := $00;
			end;

		PPURegisters.VideoMemoryData: // $2007 PPUDATA
			if ignoreVramRead <> 0 then
			begin
				// 2 reads to $2007 in quick succession (2 consecutive CPU cycles) causes the
				// 2nd read to be ignored (normally depends on PPU/CPU timing, but this is the simplest solution)
				// Return open bus in this case? (which will match the last value read)
				openBusMask := $FF;
			end
			else
			begin
				Result := memoryReadBuffer;
				memoryReadBuffer := ReadVram(ppuBusAddress and $3FFF, memopRead); // fixes vram_access.nes

				if (not Settings.DisablePaletteRead) and
					((ppuBusAddress and $3FFF) >= $3F00) then
				begin
					Result := ReadPaletteRAM(ppuBusAddress) or (openBus and $C0);
					//Console.DebugProcessVramReadOperation(memopRead, ppuBusAddress and $3FFF, Result);
					openBusMask := $C0;
				end
				else
					openBusMask := $00;

				UpdateVideoRamAddr;
				ignoreVramRead := 6;
				needStateUpdate := True;
			end;
	end;
	Result := ApplyOpenBus(openBusMask, Result);
end;

procedure TPPU.WriteRAM(addr: Word; value: Byte);
var
	newAddr: Word;
begin
	if addr <> $4014 then
		SetOpenBus($FF, value);

	case GetRegisterID(addr) of

		PPURegisters.Control: // PPUCTRL
		begin
			if Settings.PPUModel in [Ppu2C05A..Ppu2C05E] then
				SetMaskRegister(value)
			else
				SetControlRegister(value);
		end;

		PPURegisters.Mask: // PPUMASK
		begin
			if Settings.PPUModel in [Ppu2C05A..Ppu2C05E] then
				SetControlRegister(value)
			else
				SetMaskRegister(value);
		end;

		PPURegisters.SpriteAddr: // $2003 OAMADDR
			SpriteRamAddr := value;

		PPURegisters.SpriteData: // $2004 OAMDATA
			if (not RenderingEnabled) or ( (scanline >= 240) and
				((nesModel <> nesPAL) or (scanline < palSpriteEvalScanline)) ) then
			begin
				if (SpriteRamAddr and $03) = $02 then
				begin
					// "The three unimplemented bits of each sprite's byte 2 do not exist in the PPU and
					// always read back as 0 on PPU revisions that allow reading PPU OAM through OAMDATA ($2004)"
					value := value and $E3;
				end;
				WriteSpriteRam(SpriteRamAddr, value);
				SpriteRamAddr := (SpriteRamAddr + 1) and $FF;
			end
			else
			begin
				// "Writes to OAMDATA during rendering (on the pre-render line and the visible lines 0-239,
				// provided either sprite or background rendering is enabled) do not modify values in OAM,
				// but do perform a glitchy increment of OAMADDR, bumping only the high 6 bits"
				SpriteRamAddr := (SpriteRamAddr + 4) and $FF;
			end;

		PPURegisters.ScrollOffsets: // PPUSCROLL
		begin
			if WriteToggle then
			begin
				// First write to scroll register contains Y offset in pixel space
				TmpVideoRamAddr := (TmpVideoRamAddr and Word(not $73E0)) or
					(Word(value and $F8) << 2) or (Word(value and $07) << 12);
			end
			else
			begin
				// First write to scroll register contains X offset in pixel space
				XScroll := value and $07;
				newAddr := (TmpVideoRamAddr and Word(not $001F)) or (value >> 3);
				ProcessTmpAddrScrollGlitch(newAddr, (Console.MemoryManager.GetOpenBus >> 3), $001F);
			end;
			WriteToggle := not WriteToggle;
		end;

		PPURegisters.VideoMemoryAddr: // PPUADDR
		begin
			if WriteToggle then
			begin
				TmpVideoRamAddr := (TmpVideoRamAddr and $FF00) or value;
				// Video RAM update is apparently delayed by 3 PPU cycles (based on Visual NES findings)
				needStateUpdate := True;
				updateVramAddrDelay := 3;
				updateVramAddr := TmpVideoRamAddr;
				//console->DebugSetLastFramePpuScroll(_updateVramAddr, _XScroll, false);
			end
			else
			begin
				newAddr := (TmpVideoRamAddr and $00FF) or (Word(value and $3F) << 8);
				ProcessTmpAddrScrollGlitch(newAddr, Word(Console.MemoryManager.GetOpenBus) << 8, $0C00);
			end;
			WriteToggle := not WriteToggle;
		end;

		PPURegisters.VideoMemoryData: // $2007 PPUDATA
		begin
			if (ppuBusAddress and $3FFF) >= $3F00 then
				WritePaletteRAM(ppuBusAddress, value)
				//console.DebugProcessVramWriteOperation(_ppuBusAddress & 0x3FFF, value);
			else
			if (scanline >= 240) or (not RenderingEnabled) then
			begin
				Mapper.WriteVRAM(ppuBusAddress and $3FFF, value);
			end
			else
			begin
				// During rendering, the value written is ignored, and instead the
				// address' LSB is used (not confirmed, based on Visual NES)
				Mapper.WriteVRAM(ppuBusAddress and $3FFF, ppuBusAddress and $FF);
			end;
			UpdateVideoRamAddr;
		end;

		PPURegisters.SpriteDMA:
			NES_CPU.RunDMATransfer(value);

	end;
end;

procedure TPPU.UpdateMinimumDrawCycles;
begin
	minimumDrawBgCycle := IfThen(flags.BackgroundEnabled,
		IfThen(flags.BackgroundMask or Settings.ForceBackgroundFirstColumn, 0, 8), 300);
	minimumDrawSpriteCycle := IfThen(flags.SpritesEnabled,
		IfThen(flags.SpriteMask or Settings.ForceSpritesFirstColumn, 0, 8), 300);
	minimumDrawSpriteStandardCycle := IfThen(flags.SpritesEnabled,
		IfThen(flags.SpriteMask, 0, 8), 300);
end;

procedure TPPU.SetControlRegister(value: Byte);
var
	nameTable: Byte;
	normalAddr: Word;
begin
	Control := value;

	nameTable  := (Control and $03);
	normalAddr := (TmpVideoRamAddr and Word(not $0C00)) or (Word(nameTable) << 10);

	ProcessTmpAddrScrollGlitch(normalAddr, Word(Console.MemoryManager.GetOpenBus) << 10, $0400);

	flags.VerticalWrite := (Control and $04) = $04;
	flags.SpritePatternAddr     := IfThen((Control and $08) = $08, $1000, $0000);
	flags.BackgroundPatternAddr := IfThen((Control and $10) = $10, $1000, $0000);
	flags.LargeSprites := (Control and $20) = $20;
	flags.VBlank := (Control and $80) = $80;

	// "By toggling NMIoutput ($2000 bit 7) during vertical blank without reading $2002,
	// a program can cause /NMI to be pulled low multiple times, causing multiple NMIs to be generated."
	if not flags.VBlank then
		CPUClearNmiFlag
	else
	if statusFlags.VerticalBlank then
		CPUSetNmiFlag;
end;

procedure TPPU.SetMaskRegister(value: Byte);
var
	pal: Byte;
begin
	Mask := value;
	flags.Grayscale         := value.TestBit(0);// (value and $01) = $01;
	flags.BackgroundMask    := value.TestBit(1);// (value and $02) = $02;
	flags.SpriteMask        := value.TestBit(2);// (value and $04) = $04;
	flags.BackgroundEnabled := value.TestBit(3);// (value and $08) = $08;
	flags.SpritesEnabled    := value.TestBit(4);// (value and $10) = $10;
	flags.IntensifyBlue     := value.TestBit(7);// (value and $80) = $80;

	if (renderingEnabled <> (flags.BackgroundEnabled or flags.SpritesEnabled)) then
		needStateUpdate := True;

	UpdateMinimumDrawCycles;
	UpdateGrayscaleAndIntensifyBits;

	// "Bit 0 controls a greyscale mode, which causes the palette to use only
	// the colors from the grey column: $00, $10, $20, $30. This is implemented
	// as a bitwise AND with $30 on any value read from PPU $3F00-$3FFF"
	paletteRamMask := IfThen(flags.Grayscale, $30, $3F);

	pal := (value and $E0) >> 5; // BGR0 0000 -> 0000 0BGR

	if nesModel = nesNTSC then
	begin
		flags.IntensifyRed   := (value and $20) = $20;
		flags.IntensifyGreen := (value and $40) = $40;
		intensifyColorBits   := (value and $E0) << 1;
		palIndex := pal;
	end
	else
	begin
		// "Note that on the Dendy and PAL NES, the green and red bits swap meaning."
		flags.IntensifyRed   := (value and $40) = $40;
		flags.IntensifyGreen := (value and $20) = $20;
		intensifyColorBits   :=
			IfThen(flags.IntensifyRed,   $40, $00) or
			IfThen(flags.IntensifyGreen, $80, $00) or
			IfThen(flags.IntensifyBlue, $100, $00);
		palIndex := ((pal and 2) >> 1) or // G
		            ((pal and 1) >> 1) or // R
		             (pal and 4);         // B
	end;
end;

// Taken from http://wiki.nesdev.com/w/index.php/TheskinnyonNESscrolling#Wrappingaround
//
procedure TPPU.IncVerticalScrolling;
var
	addr: Word;
	Y: Int32;
begin
	addr := VideoRamAddr;

	if (addr and $7000) <> $7000 then
	begin
		// if fine Y < 7
		Inc(addr, $1000);                // increment fine Y
	end
	else
	begin
		// fine Y := 0
		addr := addr and Word(not $7000);
		y := (addr and $03E0) >> 5;  // let y := coarse Y
		if y = 29 then
		begin
			y := 0;                   // coarse Y := 0
			addr := addr xor $0800;   // switch vertical nametable
		end
		else
		if y = 31 then
			y := 0                    // coarse Y := 0, nametable not switched
		else
			Inc(y);                   // increment coarse Y
		addr := (addr and Word(not $03E0)) or (Word(y << 5));     // put coarse Y back into v
	end;

	VideoRamAddr := addr;
end;

//Taken from http://wiki.nesdev.com/w/index.php/TheskinnyonNESscrolling#Wrappingaround
//
procedure TPPU.IncHorizontalScrolling;
var
	addr: Word;
begin
	// Increase coarse X scrolling value
	addr := VideoRamAddr;

	// When the value is 31, wrap around to 0 and switch nametable
	if (addr and $001F) = 31 then
		addr := addr xor $041F // (addr and Word(not $001F)) xor $0400
	else
		Inc(addr);

	VideoRamAddr := addr;
end;

//Taken from http://wiki.nesdev.com/w/index.php/TheskinnyonNESscrolling#Tileandattributefetching
function TPPU.GetNameTableAddr: Word;
begin
	Result := $2000 or (VideoRamAddr and $0FFF);
end;

//Taken from http://wiki.nesdev.com/w/index.php/TheskinnyonNESscrolling#Tileandattributefetching
function TPPU.GetAttributeAddr: Word;
begin
	Result := $23C0 or
		Word(VideoRamAddr and $0C00) or
		(Word(VideoRamAddr >> 4) and $38) or
		(Word(VideoRamAddr >> 2) and $07);
end;

procedure TPPU.LoadTileInfo;
var
	B: Byte;
begin
	if RenderingEnabled then
	case (cycle and $07) of

		1: begin
			previousTile := currentTile;
			currentTile  := nextTile;

			LowBitShift  := LowBitShift  or nextTile.LowByte;
			HighBitShift := HighBitShift or nextTile.HighByte;

			B := ReadVram(GetNameTableAddr);
			nextTile.TileAddr := (B << 4) or (VideoRamAddr >> 12) or flags.BackgroundPatternAddr;
			//nextTile.OffsetY := VideoRamAddr shr 12;
		end;

		3: begin
			B := Byte((VideoRamAddr >> 4) and $04) or Byte(VideoRamAddr and $02);
			nextTile.PaletteOffset := ((ReadVram(GetAttributeAddr) >> B) and $03) << 2;
		end;

		5: begin
			nextTile.LowByte := ReadVram(nextTile.TileAddr);
			//nextTile.AbsoluteTileAddr := console.GetMapper.ToAbsoluteChrAddress(nextTile.TileAddr);
		end;

		7: nextTile.HighByte := ReadVram(nextTile.TileAddr + 8);

	end;
end;

procedure TPPU.LoadSprite(spriteY, tileIndex, attributes, spriteX: Byte; extraSprite: Boolean);
var
	tileAddr: Word;
	lineOffset: Byte;
	fetchLastSprite: Boolean = True;
	info: PSpriteInfo;
	i: Int32;
begin
	if (attributes and $80) = $80 then
		lineOffset := IfThen(flags.LargeSprites, 15, 7) - (scanline - spriteY)
	else
		lineOffset := scanline - spriteY;

	if flags.LargeSprites then
		tileAddr :=
			IfThen(Odd(tileIndex), $1000, $0000) or
			((tileIndex and (not $01)) << 4) +
			IfThen(lineOffset >= 8, lineOffset + 8, lineOffset)
	else
		tileAddr := ((tileIndex << 4) or flags.SpritePatternAddr) + lineOffset;

	if ((extraSprite) or (spriteIndex < spriteCount)) and (spriteY < 240) then
	begin
		info := @spriteTiles[spriteIndex];
		info^.BackgroundPriority := (attributes and $20) = $20;
		info^.HorizontalMirror   := (attributes and $40) = $40;
		//info^.VerticalMirror     := verticalMirror;
		info^.PaletteOffset      := ((attributes and $03) << 2) or $10;
		if extraSprite then
		begin
			//Use DebugReadVRAM for extra sprites to prevent side-effects.
			info^.LowByte  := Mapper.DebugReadVram(tileAddr);
			info^.HighByte := Mapper.DebugReadVram(tileAddr + 8);
		end
		else
		begin
			fetchLastSprite := False;
			info^.LowByte  := ReadVram(tileAddr);
			info^.HighByte := ReadVram(tileAddr + 8);
		end;
		info^.TileAddr := tileAddr;
		//info^.AbsoluteTileAddr := console.GetMapper.ToAbsoluteChrAddress(tileAddr);
		//info^.OffsetY := lineOffset;
		info^.SpriteX := spriteX;

		//Sprites read on prerender scanline are not shown on scanline 0
		if scanline >= 0 then
			for i := 0 to 7 do
				if (spriteX + i + 1) < 257 then
					hasSprite[spriteX + i + 1] := True;
	end;

	// Fetches to sprite $FF for remaining sprites/hidden - used by MMC3 IRQ counter
	if fetchLastSprite then
	begin
		lineOffset := 0;
		tileIndex := $FF;
		if flags.LargeSprites then
			tileAddr :=
				(IfThen(Odd(tileIndex), $1000, $0000)) or
				((tileIndex and (not $01)) << 4) +
				(IfThen(lineOffset >= 8, lineOffset + 8, lineOffset))
		else
			tileAddr := ((tileIndex << 4) or flags.SpritePatternAddr) + lineOffset;

		ReadVram(tileAddr);
		ReadVram(tileAddr + 8);
	end;

	Inc(spriteIndex);
end;

procedure TPPU.LoadExtraSprites;
var
	i: Int32;
	_loadExtraSprites: Boolean = True;
	lastPosition: Word = $FFFF;
	identicalSpriteCount: Byte = 0;
	maxIdenticalSpriteCount: Byte = 0;
	x, y: Byte;
	position: Word;
begin
	if (Settings.RemoveSpriteLimit) and (spriteCount = 8) then
	begin
		if Settings.AdaptiveSpriteLimit then
		begin
			for i := 0 to 63 do
			begin
				y := spriteRAM[i << 2];
				if (scanline >= y) and (scanline < (y + IfThen(flags.LargeSprites, 16, 8))) then
				begin
					x := spriteRAM[(i << 2) + 3];
					position := (y << 8) or x;
					if lastPosition <> position then
					begin
						if identicalSpriteCount > maxIdenticalSpriteCount then
							maxIdenticalSpriteCount := identicalSpriteCount;
						lastPosition := position;
						identicalSpriteCount := 1;
					end
					else
						Inc(identicalSpriteCount);
				end;
			end;
			_loadExtraSprites := (identicalSpriteCount < 8) and (maxIdenticalSpriteCount < 8);
		end;

		if _loadExtraSprites then
		begin
			i := (lastVisibleSpriteAddr + 4) and $FF;
			while i <> firstVisibleSpriteAddr do
			begin
				y := spriteRAM[i];
				if (scanline >= y) and (scanline < (y + IfThen(flags.LargeSprites, 16, 8))) then
				begin
					LoadSprite(y, spriteRAM[i+1], spriteRAM[i+2], spriteRAM[i+3], True);
					Inc(spriteCount);
				end;
				i := (i + 4) and $FF;
			end;

		end;

	end;
end;

procedure TPPU.LoadSpriteTileInfo;
var
	A: PByte;
begin
	A := @secondarySpriteRAM[spriteIndex * 4];
	LoadSprite(A[0], A[1], A[2], A[3], False);
end;

procedure TPPU.ShiftTileRegisters;
begin
	LowBitShift  := LowBitShift  << 1;
	HighBitShift := HighBitShift << 1;
end;

function TPPU.GetPixelColor: Byte;
var
	offset, backgroundColor, spriteColor, spriteBgColor: Byte;
	i, shift: Int32;
begin
	offset := XScroll;
	backgroundColor := 0;
	spriteBgColor   := 0;

	if cycle > minimumDrawBgCycle then
	begin
		//BackgroundMask = False: Hide background in leftmost 8 pixels of screen
		spriteBgColor := (((LowBitShift << offset) and $8000) >> 15) or
			(((HighBitShift << offset) and $8000) >> 14);
		// TODO if (settings.BackgroundEnabled) then
			backgroundColor := spriteBgColor;
	end;

	if (hasSprite[cycle]) and (cycle > minimumDrawSpriteCycle) then
	begin
		// SpriteMask = True: Hide sprites in leftmost 8 pixels of screen
		for i := 0 to spriteCount-1 do
		begin
			shift := cycle - spriteTiles[i].SpriteX - 1;
			if (shift >= 0) and (shift < 8) then
			begin
				lastSprite := @spriteTiles[i];
				if lastSprite^.HorizontalMirror then
					spriteColor := ((lastSprite^.LowByte >> shift) and $01) or
						((lastSprite^.HighByte >> shift) and $01) << 1
				else
					spriteColor := ((lastSprite^.LowByte << shift) and $80) >> 7 or
						((lastSprite^.HighByte << shift) and $80) >> 6;

				if spriteColor <> 0 then // First sprite without a 00 color, use it.
				begin
					if (i = 0) and (sprite0Visible) and (spriteBgColor <> 0) and
						(cycle <> 256) and (flags.BackgroundEnabled) and
						(not statusFlags.Sprite0Hit) and (cycle > minimumDrawSpriteStandardCycle) then
					begin
						//"The hit condition is basically sprite zero is in range AND the first
						// sprite output unit is outputting a non-zero pixel AND the background
						// drawing unit is outputting a non-zero pixel."
						//"Sprite zero hits do not register at x=255" (cycle 256)
						//"... provided that background and sprite rendering are both enabled"
						//"Should always miss when Y >= 239"
						statusFlags.Sprite0Hit := True;
						//console.DebugProcessEvent(EventType.SpriteZeroHit);
					end;

					//Check sprite priority
					//if (Settings.SpritesEnabled) and
					if (backgroundColor = 0) or (not lastSprite^.BackgroundPriority) then
						Exit(lastSprite^.PaletteOffset + spriteColor)
					else
						Break;
				end;

			end;
		end; // i
	end; // sprite

	Result := IfThen(offset + ((cycle - 1) and $07) < 8,
		previousTile.PaletteOffset, currentTile.PaletteOffset) + backgroundColor;
end;

// This is called 3.7 million times per second - needs to be as fast as possible.
procedure TPPU.DrawPixel;
var
	color: Byte;
begin
	if (RenderingEnabled) or ((VideoRamAddr and $3F00) <> $3F00) then
	begin
		color := GetPixelColor;
		PixPtr^ := paletteRAM[IfThen(color and $03 <> 0, color, 0)];
	end
	else
	begin
		// "If the current VRAM address points in the range $3F00-$3FFF during
		// forced blanking, the color indicated by this palette location will be
		// shown on screen instead of the backdrop color."
		PixPtr^ := paletteRAM[VideoRamAddr and $1F];
	end;

	Inc(PixPtr);
end;

function TPPU.GetCurrentBgColor: Word;
var
	color: Word;
begin
	if (RenderingEnabled) or (VideoRamAddr and $3F00 <> $3F00) then
		color := paletteRAM[0]
	else
		color := paletteRAM[VideoRamAddr and $1F];
	Result := (color and paletteRamMask) or intensifyColorBits;
end;

// Used by Zapper, gives a rough approximation of the brightness level of the specific pixel
function TPPU.GetPixelBrightness(X, Y: Byte): Cardinal;
var
	argbColor: Cardinal;
begin
	argbColor := Palette.Colors[0, currentOutputBuffer[Y << 8 or X] and $3F];
	Result := (argbColor and $FF) + ((argbColor >> 8) and $FF) + ((argbColor >> 16) and $FF);
end;

procedure TPPU.UpdateGrayscaleAndIntensifyBits;
var
	pixelNumber: Int32;
	pout: PWord;
begin
	if (scanline < 0) or (scanline > nmiScanline) then Exit;

	if scanline >= 240 then
		pixelNumber := 61439
	else
	if cycle < 3 then
		pixelNumber := (scanline << 8) - 1
	else
	if cycle <= 258 then
		pixelNumber := (scanline << 8) + Int32(cycle) - 3
	else
		pixelNumber := (scanline << 8) + 255;

	if (paletteRamMask = $3F) and (intensifyColorBits = 0) then
	begin
		// Nothing to do (most common case)
		lastUpdatedPixel := pixelNumber;
	end
	else
	if lastUpdatedPixel < pixelNumber then
	begin
		pout := @currentOutputBuffer[lastUpdatedPixel + 1];
		while lastUpdatedPixel < pixelNumber do
		begin
			pout^ := (pout^ and paletteRamMask) or intensifyColorBits;
			Inc(pout);
			Inc(lastUpdatedPixel);
		end;
	end;
end;

procedure TPPU.ProcessSpriteEvaluation;
begin
	if (RenderingEnabled) or ((nesModel = nesPAL) and (scanline >= palSpriteEvalScanline)) then
	begin
		if cycle < 65 then
		begin
			// Clear secondary OAM at between cycle 1 and 64
			oamCopybuffer := $FF;
			secondarySpriteRAM[(cycle-1) >> 1] := $FF;
		end
		else
		begin
			if cycle = 65 then
			begin
				sprite0Added := False;
				spriteInRange := False;
				secondaryOAMAddr := 0;
				overflowBugCounter := 0;
				oamCopyDone := False;
				spriteAddrH := (SpriteRamAddr >> 2) and $3F;
				spriteAddrL := SpriteRamAddr and $03;
				firstVisibleSpriteAddr := spriteAddrH * 4;
				lastVisibleSpriteAddr := firstVisibleSpriteAddr;
			end
			else
			if cycle = 256 then
			begin
				sprite0Visible := sprite0Added;
				spriteCount := secondaryOAMAddr >> 2;
			end;

			if Odd(cycle) then
			begin
				// Read a byte from the primary OAM on odd cycles
				oamCopybuffer := ReadSpriteRam(SpriteRamAddr);
			end
			else
			begin
				if oamCopyDone then
				begin
					spriteAddrH := (spriteAddrH + 1) and $3F;
					if secondaryOAMAddr >= $20 then
					begin
						// "As seen above, a side effect of the OAM write disable signal
						// is to turn writes to the secondary OAM into reads from it."
						oamCopybuffer := secondarySpriteRAM[secondaryOAMAddr and $1F];
					end;
				end
				else
				begin
					if (not spriteInRange) and (scanline >= oamCopybuffer) and
						(scanline < (oamCopybuffer + IfThen(flags.LargeSprites, 16, 8))) then
							spriteInRange := True;

					if secondaryOAMAddr < $20 then
					begin
						// Copy 1 byte to secondary OAM
						secondarySpriteRAM[secondaryOAMAddr] := oamCopybuffer;

						if spriteInRange then
						begin
							Inc(spriteAddrL);
							Inc(secondaryOAMAddr);
							if spriteAddrH = 0 then
								sprite0Added := True;

							// Note: Using "(secondaryOAMAddr and $03) = 0" instead of
							// "spriteAddrL = 0" is required to replicate a hardware bug
							// noticed in oamflickertestreenable when disabling and
							// re-enabling rendering on a single scanline
							if (secondaryOAMAddr and $03) = 0 then
							begin
								//Done copying all 4 bytes
								spriteInRange := False;
								spriteAddrL := 0;
								lastVisibleSpriteAddr := spriteAddrH * 4;
								spriteAddrH := (spriteAddrH + 1) and $3F;
								if spriteAddrH = 0 then
									oamCopyDone := True;
							end;
						end
						else
						begin
							//Nothing to copy, skip to next sprite
							spriteAddrH := (spriteAddrH + 1) and $3F;
							if spriteAddrH = 0 then
								oamCopyDone := True;
						end;
					end
					else
					begin
						// "As seen above, a side effect of the OAM write disable signal
						// is to turn writes to the secondary OAM into reads from it."
						oamCopybuffer := secondarySpriteRAM[secondaryOAMAddr and $1F];

						// 8 sprites found, check next sprite for overflow + emulate PPU bug
						if spriteInRange then
						begin
							// Sprite is visible, consider this to be an overflow
							statusFlags.SpriteOverflow := True;
							Inc(spriteAddrL);
							if spriteAddrL = 4 then
							begin
								spriteAddrH := (spriteAddrH + 1) and $3F;
								spriteAddrL := 0;
							end;
							if overflowBugCounter = 0 then
								overflowBugCounter := 3
							else
							if overflowBugCounter > 0 then
							begin
								Dec(overflowBugCounter);
								if overflowBugCounter = 0 then
								begin
									// "After it finishes "fetching" this sprite (and setting
									// the overflow flag), it realigns back at the beginning
									// of this line and then continues here on the next sprite"
									oamCopyDone := True;
									spriteAddrL := 0;
								end;
							end;
						end
						else
						begin
							// Sprite isn't on this scanline, trigger sprite evaluation bug -
							// increment both H and L at the same time
							spriteAddrH := (spriteAddrH + 1) and $3F;
							spriteAddrL := (spriteAddrL + 1) and $03;
							if spriteAddrH = 0 then
								oamCopyDone := True;
						end;
					end;
				end;
				SpriteRamAddr := (spriteAddrL and $03) or Byte(spriteAddrH << 2);
			end;
		end;
	end;
end;

procedure TPPU.ProcessScanline;
begin
	if cycle <= 256 then // Only called for cycle 1+
	begin
		LoadTileInfo;

		if (prevRenderingEnabled) and ((cycle and $07) = 0) then
		begin
			IncHorizontalScrolling;
			if cycle = 256 then
				IncVerticalScrolling;
		end;

		if scanline >= 0 then
		begin
			DrawPixel;
			ShiftTileRegisters;
			// "Secondary OAM clear and sprite evaluation do not occur on the pre-render line"
			ProcessSpriteEvaluation;
		end
		else
		if cycle < 9 then
		begin
			// Pre-render scanline logic
			if cycle = 1 then
			begin
				statusFlags.VerticalBlank := False;
				CPUClearNmiFlag;
			end;
			if (RenderingEnabled) and (SpriteRamAddr >= $08) and
				(not Settings.DisableOamAddrBug) then
			begin
				// This should only be done if rendering is enabled (otherwise oamstress test fails immediately)
				// "If OAMADDR is not less than eight when rendering starts, the eight bytes starting at OAMADDR and $F8 are copied to the first eight bytes of OAM"
				WriteSpriteRam(Byte(cycle-1), ReadSpriteRam((SpriteRamAddr and $F8) + cycle - 1));
			end;
		end;
	end
	else
	if (cycle >= 257) and (cycle <= 320) then
	begin
		if cycle = 257 then
		begin
			spriteIndex := 0;
			ClearArray(hasSprite);
			if prevRenderingEnabled then
			begin
				//copy horizontal scrolling value from t
				VideoRamAddr := (VideoRamAddr and Word(not $041F)) or (TmpVideoRamAddr and $041F);
				//console.DebugSetLastFramePpuScroll(VideoRamAddr, XScroll, True);
			end;
		end;
		if RenderingEnabled then
		begin
			//"OAMADDR is set to 0 during each of ticks 257-320 (the sprite tile loading interval) of the pre-render and visible scanlines." (When rendering)
			SpriteRamAddr := 0;

			if ((cycle - 261) and 7) = 0 then
			begin
				//Cycle 260, 268, etc.  This is an approximation (each tile is actually loaded in 8 steps (e.g from 257 to 264))
				LoadSpriteTileInfo;
			end
			else
			if ((cycle - 257) and 7) = 0 then
			begin
				//Garbage NT sprite fetch (257, 265, 273, etc.) - Required for proper MC-ACC IRQs (MMC3 clone)
				ReadVram(GetNameTableAddr);
			end
			else
			if ((cycle - 259) and 7) = 0 then
			begin
				//Garbage AT sprite fetch
				ReadVram(GetAttributeAddr);
			end;

			if (scanline = -1) and (cycle >= 280) and (cycle <= 304) then
			begin
				//copy vertical scrolling value from t
				VideoRamAddr := (VideoRamAddr and (not $7BE0)) or (TmpVideoRamAddr and $7BE0);
			end;
		end;
	end
	else
	if (cycle >= 321) and (cycle <= 336) then
	begin
		if cycle = 321 then
		begin
			if RenderingEnabled then
			begin
				LoadExtraSprites;
				oamCopybuffer := secondarySpriteRAM[0];
			end;
			LoadTileInfo;
			//if scanline = -1 then
			//	console.DebugSetLastFramePpuScroll(VideoRamAddr, XScroll, False);
		end
		else
		if (prevRenderingEnabled) and ((cycle = 328) or (cycle = 336)) then
		begin
			LoadTileInfo;
			LowBitShift  := LowBitShift  << 8;
			HighBitShift := HighBitShift << 8;
			IncHorizontalScrolling;
		end
		else
			LoadTileInfo;
	end
	else
	if (RenderingEnabled) and ((cycle = 337) or (cycle = 339)) then
	begin
		ReadVram(GetNameTableAddr);
		if (scanline = -1) and (cycle = 339) and (Odd(frameCount)) and
			(nesModel = nesNTSC) and (Settings.PPUModel = Ppu2C02) then
		begin
			//This behavior is NTSC-specific - PAL frames are always the same number of cycles
			//"With rendering enabled, each odd PPU frame is one PPU clock shorter than normal" (skip from 339 to 0, going over 340)
			cycle := 340;
		end;
	end;
end;

procedure TPPU.FillFramebuffer;
var
	x, y: Integer;
	Pixel: PColor32;
begin
	PixPtr := @currentOutputBuffer[0]; // NES color indexes with emphasis bits

	for y := 0 to ScreenHeight-1 do
	begin
		Pixel := Framebuffer.PixelPtr[0, y];
		for x := 0 to ScreenWidth-1 do
		begin
			Pixel^ := Palette.Colors[0, PixPtr^];
			Inc(Pixel);
			Inc(PixPtr);
		end;
	end;

{	// show tile palette
	for x := 0 to 3 do
	for y := 0 to 3 do
	begin
		Framebuffer.Pixel[x, y] := Palette.Colors[0, paletteRAM[y*4+x]];
	end;}
	//GetPatternTable(0, 0, 0, 0);
end;

procedure TPPU.ConfigureNTSCFilter(const NTSCConfig: TFilterNTSCConfig);
begin
	if not Assigned(NtscFilter) then
		NtscSetup := nes_ntsc_composite;
	with NtscSetup do
	begin
		hue        := NTSCConfig.Hue;
		saturation := NTSCConfig.Saturation;
		contrast   := NTSCConfig.Contrast;
		brightness := NTSCConfig.Brightness;
		sharpness  := NTSCConfig.Sharpness;

		// Advanced parameters
		gamma      := NTSCConfig.Gamma;
		resolution := NTSCConfig.Resolution;
		artifacts  := NTSCConfig.Artifacts;
		fringing   := NTSCConfig.Fringing;
		bleed      := NTSCConfig.Bleed;

		merge_fields   := NTSCConfig.MergeFields;
		decoder_matrix := nil;
		palette        := nil;
		base_palette   := @Self.Palette.RawColors[0,0];
	end;
	if Assigned(NtscFilter) then
		NtscFilter.Init(NtscSetup);
end;

procedure TPPU.FillNTSCbuffer(NTSCbuffer: TBitmap32);
begin
	if (not Console.Rewind) and (not Console.FastForward) and (not Console.IsRunAheadFrame) then
		burstphase := 1 - burstphase;
	NtscFilter.Blit(
		@currentOutputBuffer[0],   // buf_in
		@NTSCbuffer.Bits[0],       // buf_out
		burstphase,                // frameCount and 1{3}, // burst_phase
		ScreenWidth,               // in_width
		ScreenHeight,              // in_height
		ScreenWidth * 2,           // in_pitch
		NTSCbuffer.Width * 4       // out_pitch
	);
end;

procedure TPPU.SendFrame;
begin
	UpdateGrayscaleAndIntensifyBits;

	frame_complete := True;
	Inc(frameCount);
end;

procedure TPPU.TriggerNmi;
begin
	if flags.VBlank then
		CPUSetNmiFlag;
end;

procedure TPPU.BeginVBlank;
begin
	TriggerNmi;
end;

procedure TPPU.UpdateApuStatus;
begin
	NES_APU.SetApuStatus(True);
	//Disable APU for extra lines before/after NMI
	if (scanline > 240) and ((scanline > standardVblankEnd) or
		((scanline >= standardNmiScanline) and (scanline < nmiScanline))) then
			NES_APU.SetApuStatus(False);
end;

procedure TPPU.SetOamCorruptionFlags;
var
	base, offset: Byte;
begin
	if not Settings.EnablePpuOamRowCorruption then Exit;

	//Note: Still pending more research, but this currently matches a portion of the issues that have been observed
	//When rendering is disabled in some sections of the screen, either:
	// A- During Secondary OAM clear (first ~64 cycles)
	// B- During OAM tile fetching (cycle ~256 to cycle ~320)
	//then OAM memory gets corrupted the next time the PPU starts rendering again (usually at the start of the next frame)
	//This usually causes the first "row" of OAM (the first 8 bytes) to get copied over another, causing some sprites to be missing
	//and causing an extra set of the first 2 sprites to appear on the screen (not possible to see them except via any overflow they may cause)

	if cycle < 64 then
	begin
		//Every 2 dots causes the corruption to shift down 1 OAM row (8 bytes)
		corruptOamRow[cycle >> 1] := True;
	end
	else
	if (cycle >= 256) and (cycle < 320) then
	begin
		//This section is in 8-dot segments.
		//The first 3 dot increment the corrupted row by 1, and then the last 5 dots corrupt the next row for 5 dots.
		base := (cycle - 256) >> 3;
		offset := Min(3, (cycle - 256) and $07);
		corruptOamRow[base * 4 + offset] := True;
	end;
end;

procedure TPPU.ProcessOamCorruption;
var
	i: Integer;
begin
	if Settings.EnablePpuOamRowCorruption then
	begin
		// Copy first OAM row over another row, as needed by corruption flags
		// (can be over itself, which causes no actual harm)
		for i := 0 to 31 do
			if corruptOamRow[i] then
			begin
				CopyMemory(@spriteRAM[i*8], @spriteRAM[0], 8);
				corruptOamRow[i] := False;
			end;
	end;
end;

procedure TPPU.Exec;
begin
	if cycle > 339 then
	begin
		cycle := 0;
		Inc(scanline);

		if scanline > vblankEnd then
		begin
			lastUpdatedPixel := -1;
			scanline := -1;
			// Force prerender scanline sprite fetches to load the dummy $FF tiles
			spriteCount := 0;
			if RenderingEnabled then ProcessOamCorruption;
			UpdateMinimumDrawCycles;
		end;

		UpdateApuStatus;

// TODO !!!
//		if scanline = InputPollScanline then
//			MainWindow.HandleInput;

		//Cycle = 0
		if scanline = -1 then
		begin
			statusFlags.SpriteOverflow := False;
			statusFlags.Sprite0Hit := False;
			//Switch to alternate output buffer (VideoDecoder may still be decoding the last frame buffer)
			//currentOutputBuffer := (currentOutputBuffer = outputBuffers[0]) ? outputBuffers[1] : outputBuffers[0];
		end
		else
		if scanline = 240 then
		begin
			//At the start of vblank, the bus address is set back to VideoRamAddr.
			//According to Visual NES, this occurs on scanline 240, cycle 1, but is done here on cycle for performance reasons
			SetBusAddress(VideoRamAddr);
			SendFrame;
			PixPtr := @currentOutputBuffer[0]; //FrameBuffer.PixelPtr[0, 0];
		end
		else
		if (scanline >= 0) and (scanline < ScreenHeight) then
			PixPtr := @currentOutputBuffer[scanline*ScreenWidth]; //FrameBuffer.PixelPtr[0, scanline];
	end
	else
	begin
		//Cycle > 0
		Inc(cycle);

		//console.DebugProcessPpuCycle;
		if scanline < 240 then
			ProcessScanline
		else
		if (cycle = 1) and (scanline = nmiScanline) then
		begin
			if not preventVblFlag then
			begin
				statusFlags.VerticalBlank := True;
				BeginVBlank;
			end;
			preventVblFlag := False;
		end
		else
		if (nesModel = nesPAL) and (scanline >= palSpriteEvalScanline) then
		begin
			//"On a PAL machine, because of its extended vertical blank, the PPU begins refreshing OAM roughly 21 scanlines after NMI[2], to prevent it
			//from decaying during the longer hiatus of rendering. Additionally, it will continue to refresh during the visible portion of the screen
			//even if rendering is disabled. Because of this, OAM DMA must be done near the beginning of vertical blank on PAL, and everywhere else
			//it is liable to conflict with the refresh. Since the refresh can't be disabled like on the NTSC hardware, OAM decay does not occur at all on the PAL NES."
			if cycle <= 256 then
				ProcessSpriteEvaluation
			else
			if (cycle >= 257) and (cycle < 320) then
				SpriteRamAddr := 0;
		end;
	end;

	if needStateUpdate then UpdateState;

	// pull level high when PPU/VRAM addr bit 13 is low
	if (ppuBusAddress and $2000) <> 0 then
		Inc(A13pinLowSum); // invert relative to 2A03
end;

procedure TPPU.UpdateState;
begin
	needStateUpdate := False;

	// Rendering enabled flag is apparently set with a 1 cycle delay
	// (i.e setting it at cycle 5 will render cycle 6 like cycle 5 and then take the new settings for cycle 7)
	if prevRenderingEnabled <> RenderingEnabled then
	begin
		prevRenderingEnabled := RenderingEnabled;
		if scanline < 240 then
		begin
			if prevRenderingEnabled then
				//Rendering was just enabled, perform oam corruption if any is pending
				ProcessOamCorruption
			else
			begin
				//Rendering was just disabled by a write to $2001, check for oam row corruption glitch
				SetOamCorruptionFlags;

				//When rendering is disabled midscreen, set the vram bus back to the value of 'v'
				SetBusAddress(VideoRamAddr and $3FFF);

				if (cycle >= 65) and (cycle <= 256) then
				begin
					//Disabling rendering during OAM evaluation will trigger a glitch causing the current address to be incremented by 1
					//The increment can be "delayed" by 1 PPU cycle depending on whether or not rendering is disabled on an even/odd cycle
					//e.g, if rendering is disabled on an even cycle, the following PPU cycle will increment the address by 5 (instead of 4)
					//     if rendering is disabled on an odd cycle, the increment will wait until the next odd cycle (at which point it will be incremented by 1)
					//In practice, there is no way to see the difference, so we just increment by 1 at the end of the next cycle after rendering was disabled
					Inc(SpriteRamAddr);

					//Also corrupt H/L to replicate a bug found in oamflickertestreenable when rendering is disabled around scanlines 128-136
					//Reenabling the causes the OAM evaluation to restart misaligned, and ends up generating a single sprite that's offset by 1
					//such that it's Y=tile index, index := attributes, attributes := x, and X := the next sprite's Y value
					spriteAddrH := (SpriteRamAddr >> 2) and $3F;
					spriteAddrL :=  SpriteRamAddr and $03;
				end;
			end;
		end;
	end;

	if RenderingEnabled <> (flags.BackgroundEnabled or flags.SpritesEnabled) then
	begin
		RenderingEnabled := flags.BackgroundEnabled or flags.SpritesEnabled;
		needStateUpdate := True;
	end;

	//console.DebugAddDebugEvent(DebugEventType.BgColorChange);

	if updateVramAddrDelay > 0 then
	begin
		Dec(updateVramAddrDelay);
		if updateVramAddrDelay = 0 then
		begin
			if (Settings.EnablePpu2006ScrollGlitch) and
				(RenderingEnabled) and (scanline < 240) then
			begin
				// When a $2006 address update lands on the Y or X increment,
				// the written value is bugged and is ANDed with the incremented value
				if cycle = 257 then
				begin
					VideoRamAddr := VideoRamAddr and updateVramAddr;
					{sharedptr<Debugger> debugger := console.GetDebugger(False);
					if (debugger and debugger.CheckFlag(DebuggerFlags.BreakOnPpu2006ScrollGlitch)) then
						debugger.BreakImmediately(BreakSource.BreakOnPpu2006ScrollGlitch);}
				end
				else
				if (cycle > 0) and (cycle and 7 = 0) and ((cycle <= 256) or (cycle > 320)) then
				begin
					VideoRamAddr := (updateVramAddr and not $41F) or
						(VideoRamAddr and updateVramAddr and $41F);
					{sharedptr<Debugger> debugger := console.GetDebugger(False);
					if (debugger and debugger.CheckFlag(DebuggerFlags.BreakOnPpu2006ScrollGlitch)) then
						debugger.BreakImmediately(BreakSource.BreakOnPpu2006ScrollGlitch);}
				end
				else
					VideoRamAddr := updateVramAddr;
			end
			else
				VideoRamAddr := updateVramAddr;

			{if not renderingEnabled then
				console.DebugAddDebugEvent(DebugEventType.BgColorChange);}

			//The glitches updates corrupt both V and T, so set the new value of V back into T
			TmpVideoRamAddr := VideoRamAddr;

			if (not RenderingEnabled) or (scanline >= 240) then
			begin
				//Only set the VRAM address on the bus if the PPU is not rendering
				//More info here: https://forums.nesdev.com/viewtopic.php?p=132145#p132145
				//Trigger bus address change when setting the vram address - needed by MMC3 IRQ counter
				//"4) Should be clocked when A12 changes to 1 via $2006 write"
				SetBusAddress(VideoRamAddr and $3FFF);
			end;
		end
		else
			needStateUpdate := True;
	end;

	if ignoreVramRead > 0 then
	begin
		Dec(ignoreVramRead);
		if ignoreVramRead > 0 then
			needStateUpdate := True;
	end;
end;

procedure TPPU.Run(runTo: QWord);
begin
	{$IFDEF MEASURETIMING_PPU}
	PPUTiming.Start;
	{$ENDIF}

	while (MasterClock + MasterClockDivider) <= RunTo do
	begin
		Exec;
		Inc(MasterClock, MasterClockDivider);
	end;

	{$IFDEF MEASURETIMING_PPU}
	PPUTimeTaken := PPUTimeTaken + PPUTiming.Stop;
	{$ENDIF}
end;

procedure TPPU.WriteDebug;
var
	S: String;
	i: Int32;
	B: Byte;
begin
	S := '';
	i := $6004;
	repeat
		B := NES_CPU.MemoryManager_Peek(i);
		if B > 0 then S := S + Chr(B);
		Inc(i);
	until B = 0;
	Log('"' + S + '"');
end;

procedure TPPU.DumpRAM(const Filename: String);
var
	FS: TFileStream;
	i: Integer;
begin
	FS := TFileStream.Create(Filename, fmCreate);

	for i := 0 to $3FFF do
		FS.WriteByte( Mapper.ReadVram(i, memopRead) );

	FS.Free;
end;

procedure TPPU.LoadSnapshot;
var
	i: Integer;
begin
	inherited LoadSnapshot;

	StartSection('PPUFLAGS');

	Stream.Read(state,        SizeOf(state));
	Stream.Read(flags,        SizeOf(flags));
	Stream.Read(statusFlags,  SizeOf(statusFlags));

	Stream.Read(currentTile,  SizeOf(currentTile));
	Stream.Read(nextTile,     SizeOf(nextTile));
	Stream.Read(previousTile, SizeOf(previousTile));

	StartSection('PPUCFG');
	Stream.Read(Settings,     SizeOf(Settings));

	StartSection('PPURAM');
	Stream.Read(paletteRam[0],         SizeOf(paletteRam));
	Stream.Read(spriteram[0],          SizeOf(spriteram));
	Stream.Read(secondarySpriteRam[0], SizeOf(secondarySpriteRam));
	Stream.Read(openBusDecayStamp[0],  SizeOf(openBusDecayStamp));

	// Done loading, reinit things
	//
	Control := state.Control;
	Mask := state.Mask;
	Status := state.Status;
	SpriteRamAddr := state.SpriteRamAddr;
	VideoRamAddr := state.VideoRamAddr;
	XScroll := state.XScroll;
	TmpVideoRamAddr := state.TmpVideoRamAddr;
	WriteToggle := state.WriteToggle;
	HighBitShift := state.HighBitShift;
	LowBitShift := state.LowBitShift;

	SetNesModel(nesModel);
	UpdateMinimumDrawCycles;

	//Set oam decay cycle to the current cycle to ensure it doesn't decay when loading a state
	for i := 0 to $1F do
		oamDecayCycles[i] := CPUGetCycleCount;

	ClearArray(corruptOamRow);

	for i := 0 to 256 do
		hasSprite[i] := True;

	lastUpdatedPixel := -1;

	UpdateApuStatus;
end;

procedure TPPU.SaveSnapshot;
begin
	inherited SaveSnapshot;

	state.Control := Control;
	state.Mask := Mask;
	state.Status := Status;
	state.SpriteRamAddr := SpriteRamAddr;
	state.VideoRamAddr := VideoRamAddr;
	state.XScroll := XScroll;
	state.TmpVideoRamAddr := TmpVideoRamAddr;
	state.WriteToggle := WriteToggle;
	state.HighBitShift := HighBitShift;
	state.LowBitShift := LowBitShift;

	StartSection('PPUFLAGS');
	Stream.WriteBuffer(state,        SizeOf(state));
	Stream.WriteBuffer(flags,        SizeOf(flags));
	Stream.WriteBuffer(statusFlags,  SizeOf(statusFlags));

	Stream.WriteBuffer(currentTile,  SizeOf(currentTile));
	Stream.WriteBuffer(nextTile,     SizeOf(nextTile));
	Stream.WriteBuffer(previousTile, SizeOf(previousTile));

	StartSection('PPUCFG');
	Stream.WriteBuffer(Settings,     SizeOf(Settings));

	StartSection('PPURAM');
	Stream.WriteBuffer(paletteRam[0],         SizeOf(paletteRam));
	Stream.WriteBuffer(spriteram[0],          SizeOf(spriteram));
	Stream.WriteBuffer(secondarySpriteRam[0], SizeOf(secondarySpriteRam));
	Stream.WriteBuffer(openBusDecayStamp[0],  SizeOf(openBusDecayStamp));
end;


end.
