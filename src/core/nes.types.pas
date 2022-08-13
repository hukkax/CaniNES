unit NES.Types;

{$MODE DELPHI}

{$I basement.inc}

interface

uses
	Classes, SysUtils;

const
	APPNAME    = 'CaniNES';
	APPVERSION = '0.9.1';

	NES_RESOLUTION_X = 256;
	NES_RESOLUTION_Y = 240;

	CONFIG_FILENAME = APPNAME + '.ini';

	KB   = 1024;
	KB8  = KB * 8;
	KB16 = KB * 16;

	Color_Background = $FF443333;

	ArchiveFileExts = '*.zip';
	ImageFileExts   = '*.png;*.jpg;*.jpeg';
	MovieFileExts   = '*.mmo;*.fm2;*.fm3'; // .bk2
	PaletteFileExts = '*.pal';
	RomFileExts     = '*.nes;*.fds' + ';' + ArchiveFileExts;
	FileBrowserExts = RomFileExts + ';' + MovieFileExts;

	ICON_SIZE    = 16;
	ICON_NONE    = 0;
	ICON_PAUSE   = 1;
	ICON_FORWARD = 2;
	ICON_REWIND  = 3;

	{$IFDEF RENDER_FONT_FREETYPE}
    TTF_FONTLICENSE = 'license.txt';

	STR_OSD_REWIND      = '◀◀';
	STR_OSD_FASTFORWARD = '▶▶';
	STR_OSD_PAUSED      = '||';
	STR_CHECKBOX_YES    = '☑ ';
	STR_CHECKBOX_NO     = '☐ ';
	STR_PAGE_NEXT       = '▶';
	STR_PAGE_PREV       = '◀ Back';
	STR_PAGE_ACCEPT     = 'OK';
	STR_ARROW_UP        = '▲';
	STR_ARROW_DOWN      = '▼';
	{$ELSE}
	STR_OSD_REWIND      = #22#23;
	STR_OSD_FASTFORWARD = #24#25;
	STR_OSD_PAUSED      = #26#27;
	STR_CHECKBOX_YES    = #20#21;
	STR_CHECKBOX_NO     = #18#19;
	STR_PAGE_NEXT       = #16;
	STR_PAGE_PREV       = #17' Back';
	{$ENDIF}

	channel_Square1      = 0;
	channel_Square2      = 1;
	channel_Triangle     = 2;
	channel_Noise        = 3;
	channel_DMC          = 4;
	channel_FDS          = 5;
	channel_VRC6         = 6;
	channel_VRC7         = 7;
	channel_MMC5         = 8;
	channel_Namco163     = 9;
	channel_Sunsoft5B    = 10;
	channel_InvA13       = 11;
	channel_InvOE1       = 12;
	channel_Last    = channel_InvOE1;
	MaxChannelCount = channel_Last;

	caBoxArt = 0;
	caSnaps  = 1;
	caTitles = 2;
	caLastIndex = caSnaps;

	irAllZeros = 0;
	irAllOnes  = 1;
	irRandom   = 2;

	INITRAM_DEFAULT = -1;
	INITRAM_RANDOM  = -2;

type
//	TByteArray = packed array of Byte;
	T2DByteArray = array of array of Byte;

	// =========
	// Console

	TNESModel   = ( nesAuto, nesNTSC, nesPAL, nesDendy );

	TGameSystem = ( NES_NTSC, NES_PAL, Famicom, Dendy, VsSystem, Playchoice, FDS, Unknown );

	// =========
	// Input

	TGameInputType = (
		gitUnspecified,
		gitStandardControllers,
		gitFourScore, gitFourPlayerAdapter,
		gitVsSystem, gitVsSystemSwapped, gitVsSystemSwapAB, gitVsZapper,
		gitZapper, gitTwoZappers,
		gitBandaiHypershot,
		gitPowerPadSideA, gitPowerPadSideB,
		gitFamilyTrainerSideA, gitFamilyTrainerSideB,
		gitArkanoidControllerNes, gitArkanoidControllerFamicom, gitDoubleArkanoidController,
		gitKonamiHyperShot,
		gitPachinkoController,
		gitExcitingBoxing,
		gitJissenMahjong,
		gitPartyTap,
		gitOekaKidsTablet,
		gitBarcodeBattler,
		gitMiraclePiano,
		gitPokkunMoguraa,
		gitTopRider,
		gitDoubleFisted,
		gitFamicom3dSystem,
		gitDoremikkoKeyboard,
		gitROB,
		gitFamicomDataRecorder,
		gitTurboFile,
		gitBattleBox,
		gitFamilyBasicKeyboard,
		gitPec586Keyboard, gitBit79Keyboard,
		gitSuborKeyboard, gitSuborKeyboardMouse1, gitSuborKeyboardMouse2,
		gitSnesMouse,
		gitGenericMulticart,
		gitSnesControllers,
		gitRacermateBicycle,
		gitUForce
	);

	// =========
	// PPU

	TPPUModel = (
		Ppu2C02,  Ppu2C03,
		Ppu2C04A, Ppu2C04B, Ppu2C04C, Ppu2C04D,
		Ppu2C05A, Ppu2C05B, Ppu2C05C, Ppu2C05D, Ppu2C05E
	);

	TNTSCCrawl = ( crawlNormal, crawlMerge, crawlPAL );

	// =========
	// Cartridge

	TPrgMemoryType = ( PrgRom, PrgSaveRam, PrgWorkRam );
	TChrMemoryType = ( ChrDefault, ChrRom, ChrRam, ChrNametableRam );

	TBusConflictType = ( buscDefault, buscYes, buscNo );

	TMirroringType = (
		MIRROR_HORIZONTAL,
		MIRROR_VERTICAL,
		MIRROR_SCREENAONLY,
		MIRROR_SCREENBONLY,
		MIRROR_FOURSCREENS );

	// ======
	// Memory

	TMemoryOperation = ( moRead = 1, moWrite = 2, moAny = 3 );

	TMemoryOperationType = (
		memopRead,
		memopWrite,
		memopExecOpcode,
		memopExecOperand,
		memopPPURenderingRead,
		memopDummyRead,
		memopDMCRead,
		memopDummyWrite
	);

	TMemoryAccessType = (
		maUnspecified = -1,
		maNoAccess    =  0,
		maRead        =  1,
		maWrite       =  2,
		maReadWrite   =  3
	);

	TBatterySaveType = ( battNormal, battChr, battIPS );

	TAspectRatio = (
		arNoStretching,
		arAuto,
		arNTSC,
		arPAL,
		arStandard,
		arWidescreen
	);

var
	// APU channels to enable by default
	APUChannelUsed: array[0..MaxChannelCount] of Boolean = (
		True, True, True,  True, True,
		False, False, False, False, False, False,
		True, True );

const
	// controller types that use the mouse for emulated input
	MouseControllers: set of TGameInputType = [
		gitZapper, gitTwoZappers,
		gitOekaKidsTablet,
		gitSuborKeyboardMouse1, gitSuborKeyboardMouse2,
		gitSnesMouse
	];

	NESSystemNames: array[TGameSystem] of String = (
		'NES NTSC', 'NES PAL', 'Famicom', 'Dendy',
		'VsSystem', 'Playchoice', 'Famicom Disk System', 'Unknown' );

	ShortNESSystemNames: array[TGameSystem] of String = (
		'NES NTSC',
		'NES PAL',
		'Famicom',
		'Dendy',
		'Vs',
		'Playchoice',
		'FDS',
		'Unknown' );

	NESModelNames: array[TNESModel] of String = ( 'Auto', 'NTSC', 'PAL', 'Dendy' );

	PPUModelNames: array[TPPUModel] of String = (
		'Ppu2C02',  'Ppu2C03',
		'Ppu2C04A', 'Ppu2C04B', 'Ppu2C04C', 'Ppu2C04D',
		'Ppu2C05A', 'Ppu2C05B', 'Ppu2C05C', 'Ppu2C05D', 'Ppu2C05E' );

	NTSCCrawlNames: array[TNTSCCrawl] of String = ( 'Normal', 'Merge fields', 'PAL' );

	APUChannelNames: array[0..MaxChannelCount] of String = (
	'Square 1', 'Square 2', 'Triangle', 'Noise', 'DMC',
	'FDS', 'VRC6', 'VRC7', 'MMC5', 'Namco163', 'Sunsoft5B',
	'Interference /A13', 'Interference /OE1' );

	APUChannelMixMultiplier: array[0..MaxChannelCount] of Word = (
		0, 0, 0, 0, 0, 20, 75, 43, 43, 20, 15, 26, 400 );

	MirroringTypeNames: array[TMirroringType] of String = (
		'Horizontal', 'Vertical', 'Screen A only', 'Screen B only', '4-screen' );

	ShortMirroringTypeNames: array[TMirroringType] of String = ( 'H', 'V', 'A', 'B', '4' );

	BatterySaveExtensions: array[TBatterySaveType] of String = (
		'.sav', '.chr', '.ips' );

	AspectRatioNames: array[TAspectRatio] of String = (
		'No stretching', 'Auto', 'NTSC', 'PAL', 'Standard (4:3)', 'Widescreen (16:9)' );

	RendererFlipNames: array[0..3] of String = ( 'None', 'Horizontal', 'Vertical', 'Both' );

	function ChoosePrgMemoryType(B: Boolean; IfTrue, IfFalse: TPrgMemoryType): TPrgMemoryType; inline;
	function ChooseChrMemoryType(B: Boolean; IfTrue, IfFalse: TChrMemoryType): TChrMemoryType; inline;
	function ChooseMemoryAccessType(B: Boolean; IfTrue, IfFalse: TMemoryAccessType): TMemoryAccessType; inline;
	function ChooseMirroringType(B: Boolean; IfTrue, IfFalse: TMirroringType): TMirroringType; overload; inline;
	function ChooseMirroringType(B: Integer; IfTrue, IfFalse: TMirroringType): TMirroringType; overload; inline;
	function FileExtensionMatches(const Filename, SearchFrom: String): Boolean; inline;


implementation

function FileExtensionMatches(const Filename, SearchFrom: String): Boolean;
begin
	Result := SearchFrom.Contains(LowerCase(ExtractFileExt(Filename)));
end;

function ChoosePrgMemoryType(B: Boolean; IfTrue, IfFalse: TPrgMemoryType): TPrgMemoryType;
begin
	if B then Result := IfTrue else Result := IfFalse;
end;

function ChooseChrMemoryType(B: Boolean; IfTrue, IfFalse: TChrMemoryType): TChrMemoryType;
begin
	if B then Result := IfTrue else Result := IfFalse;
end;

function ChooseMemoryAccessType(B: Boolean; IfTrue, IfFalse: TMemoryAccessType): TMemoryAccessType;
begin
	if B then Result := IfTrue else Result := IfFalse;
end;

function ChooseMirroringType(B: Boolean; IfTrue, IfFalse: TMirroringType): TMirroringType;
begin
	if B then Result := IfTrue else Result := IfFalse;
end;

function ChooseMirroringType(B: Integer; IfTrue, IfFalse: TMirroringType): TMirroringType;
begin
	if B <> 0 then Result := IfTrue else Result := IfFalse;
end;

end.

