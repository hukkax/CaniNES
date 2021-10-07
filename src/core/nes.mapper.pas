unit NES.Mapper;

interface

{$MODE DELPHI}
{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}

uses
	Types, Classes, SysUtils,
	NES.Types, NES.SaveState, NES.MemoryHandler,
	NES.ROM, NES.ROM.Header, NES.Cartridge,
	NES.APU.Interference;

type
	TMapperClass = class of TMapper;

	TMapper = class(TIMemoryHandler)
	const
		FdsMapperID      = 65535;
		NsfMapperID      = 65534;
		StudyBoxMapperID = 65533;

	private
		mirroringType: TMirroringType;

		nametableRam:   TBytes;
		nametableCount: Byte;
		onlyChrRam:     Boolean;

		IsReadRegisterAddr, IsWriteRegisterAddr: array [Word] of Boolean;
		prgMemoryAccess, chrMemoryAccess: array [Byte] of TMemoryAccessType;
		prgPages, chrPages: array [Byte] of PByte;
		prgMemoryOffset, chrMemoryOffset: array [Byte] of Int32;
		prgMemoryType: array [Byte] of TPrgMemoryType;
		chrMemoryType: array [Byte] of TChrMemoryType;

		originalPrgRom, originalChrRom: TBytes;

		batteryFilename: String;

		function InternalGetPrgPageSize: Word;
		function InternalGetSaveRamPageSize: Word;
		function InternalGetWorkRamPageSize: Word;
		function InternalGetChrPageSize: Word;
		function InternalGetChrRamPageSize: Word;
		function ValidateAddressRange(startAddr, endAddr: Word): Boolean;

	protected
		_NametableCount: Cardinal;
		_NametableSize:  Cardinal;

		RomInfo: TRomInfo;

		prgRom, chrRom, chrRam,
		saveRam, workRam: TBytes;

		prgSize, chrRomSize, chrRamSize,
		saveRamSize, workRamSize: Cardinal;

		hasChrBattery: Boolean;
		vramOpenBusValue: Int16;

		function  GetPRGPageSize: Word; virtual;
		function  GetCHRPageSize: Word; virtual;
		function  GetChrRamPageSize: Word; virtual;

		//Save ram is battery backed and saved to disk
		function  GetSaveRamSize: Cardinal; virtual;
		function  GetSaveRamPageSize: Cardinal; virtual;
		function  ForceChrBattery: Boolean; virtual;
		function  ForceSaveRamSize: Boolean; virtual;
		function  ForceWorkRamSize: Boolean; virtual;
		function  GetChrRamSize: Cardinal; virtual;

		//Work ram is NOT saved - aka Expansion ram, etc.
		function  GetWorkRamSize: Cardinal; virtual;
		function  GetWorkRamPageSize: Cardinal; virtual;

		function  RegisterStartAddress: Word; virtual;
		function  RegisterEndAddress: Word; virtual;
		function  AllowRegisterRead: Boolean; virtual;

		function  GetDipSwitchCount: Cardinal; virtual;
		function  HasBusConflicts: Boolean; virtual;

		function  InternalReadRam(addr: Word): Byte;

		procedure WriteRegister(addr: Word;  value: Byte); virtual;
		function  ReadRegister(addr: Word): Byte; virtual;

		procedure SelectPRGPage  (slot: Word; page: Int32; memoryType: TPrgMemoryType = TPrgMemoryType.PrgRom); virtual;
		procedure SelectPrgPage2x(slot: Word; page: Int32; memoryType: TPrgMemoryType = TPrgMemoryType.PrgRom);
		procedure SelectPrgPage4x(slot: Word; page: Int32; memoryType: TPrgMemoryType = TPrgMemoryType.PrgRom);
		procedure SetCpuMemoryMapping(startAddr, endAddr: Word; pageNumber: Int32; memoryType: TPrgMemoryType; accessType: TMemoryAccessType); overload;
		procedure SetCpuMemoryMapping(startAddr, endAddr: Word; pageNumber: Int32; memoryType: TPrgMemoryType; accessType: Int8 = -1); overload;
		procedure SetCpuMemoryMapping(startAddr, endAddr: Word; memoryType: TPrgMemoryType; sourceOffset: Cardinal; accessType: Int8); overload;
		procedure SetCpuMemoryMapping(startAddr, endAddr: Word; sourceMemory: PByte; accessType: Int8 = -1); overload;
		procedure RemoveCpuMemoryMapping(startAddr, endAddr: Word); overload;

		procedure SelectCHRPage  (slot, page: Word; memoryType: TChrMemoryType = ChrDefault); virtual;
		procedure SelectChrPage2x(slot, page: Word; memoryType: TChrMemoryType = ChrDefault); virtual;
		procedure SelectChrPage4x(slot, page: Word; memoryType: TChrMemoryType = ChrDefault); virtual;
		procedure SelectChrPage8x(slot, page: Word; memoryType: TChrMemoryType = ChrDefault); virtual;
		procedure SetPpuMemoryMapping(startAddr, endAddr, pageNumber: Word; memoryType: TChrMemoryType = ChrDefault; accessType: Int8 = -1); overload;
		procedure SetPpuMemoryMapping(startAddr, endAddr: Word; memoryType: TChrMemoryType; sourceOffset: Cardinal; accessType: Int8); overload;
		procedure SetPpuMemoryMapping(startAddr, endAddr: Word; sourceMemory: PByte; accessType: Int8 = -1); overload;
		procedure RemovePpuMemoryMapping(startAddr, endAddr: Word); overload;

		procedure InitMapper; virtual; overload;
		procedure InitMapper(var RomData: TRomData); virtual; overload;

		function  GetPRGPageCount: Word;
		function  GetCHRPageCount: Word;

		function  GetPowerOnByte(defaultValue: Byte = 0): Byte;
		function  GetDipSwitches: Cardinal;

		procedure SetupDefaultWorkRam;
		procedure InitializeChrRam(Size: Int32 = -1);

		procedure AddRegisterRange   (startAddr, endAddr: Word; operation: TMemoryOperation = moAny);
		procedure RemoveRegisterRange(startAddr, endAddr: Word; operation: TMemoryOperation = moAny);

		procedure RestorePrgChrState;

		function  GetNametable(nametableIndex: Byte): PByte;
		procedure SetNametable(index, nametableIndex: Byte);
		procedure SetNametables(Index1, Index2, Index3, Index4: Byte);
		function  InternalReadVRAM(addr: Word): Byte; inline;

		procedure DoLoadBattery(BatteryKind: TBatterySaveType; var data: TBytes; length: Cardinal = 0);
		procedure DoSaveBattery(BatteryKind: TBatterySaveType; var data: TBytes; length: Cardinal);

	public
		// Stored locally as many of the mappers require this information
		cart: TCartridge;

		_HasBusConflicts,
		_AllowRegisterRead: Boolean;

		InvA13Audio,
		InvOE1Audio: TAudioInterference;

		constructor Create(cartridge: TCartridge); virtual; overload;
		destructor  Destroy; override;

		procedure Initialize(var RomData: TRomData);
		procedure Reset(SoftReset: Boolean); virtual;

		//function  GetAvailableFeatures: TConsoleFeatures; virtual;
		function  IsNes20: Boolean;
		procedure SetNesModel(model: TNesModel); virtual;
		procedure ProcessCpuClock; virtual;
		procedure NotifyVRAMAddressChange(addr: Word); virtual;
		procedure GetMemoryRanges(var ranges: TMemoryRanges); override;

		//procedure SetConsole(var console: TConsole);
		//function  GetMapperControlDevice: TBaseControlDevice;
		function  GetRomInfo: TRomInfo;
		function  GetMapperDipSwitchCount: Cardinal;
		procedure ApplySamples(buffer: PInt16; sampleCount: QWord; volume: Single); virtual;

		function  HasBattery: Boolean;
		function  GetBatteryFilename(BatteryKind: TBatterySaveType = battNormal): String;
		procedure LoadBattery; virtual;
		procedure SaveBattery; virtual;

		function  GetSoundChipName: AnsiString; virtual;

		function  GetMirroringType: TMirroringType; inline;
		procedure SetMirroringType(MirrorType: TMirroringType);
		function  GenericChooseMirroringType(B: Byte): TMirroringType;
		function  ApplyMirroringType(B: Boolean; IfTrue, IfFalse: TMirroringType): TMirroringType; overload;
		function  ApplyMirroringType(I: Integer; IfTrue, IfFalse: TMirroringType): TMirroringType; overload;

		function  ReadRAM(addr: Word): Byte; override;
		function  PeekRAM(addr: Word): Byte; override;
		function  DebugReadRAM(addr: Word): Byte;
		procedure WriteRAM(addr: Word; value: Byte); override;
		procedure DebugWriteRAM(addr: Word; value: Byte);
		procedure WritePrgRam(addr: Word; value: Byte);
		function  MapperReadVRAM(addr: Word; operationType: TMemoryOperationType): Byte; virtual;
		function  ReadVRAM(addr: Word; operationType: TMemoryOperationType = memopPpuRenderingRead): Byte; //inline;
		procedure WriteVRAM(addr: Word; value: Byte);
		procedure DebugWriteVRAM(addr: Word; value: Byte; disableSideEffects: Boolean = True);
		function  DebugReadVRAM(addr: Word; disableSideEffects: Boolean = True): Byte;

		procedure CopyChrTile(address: Cardinal; dest: PByte);

		//Debugger Helper Functions

		function  HasChrRam: Boolean;
		function  HasChrRom: Boolean;

		//function  GetState: TCartridgeState;
		function  GetPrgRom:  PByte;
		function  GetWorkRam: PByte;
		function  GetSaveRam: PByte;

{		function  GetMemoryValue(memoryType: TDebugMemoryType, address: Cardinal): Byte;
		procedure SetMemoryValue(memoryType: TDebugMemoryType; address: Cardinal; value: Byte);
		function  GetMemorySize(_type: TDebugMemoryType): Cardinal;

		function  CopyMemory (memoryType: TDebugMemoryType, buffer: PByte): Cardinal;
		procedure WriteMemory(memoryType: TDebugMemoryType, buffer: PByte; length: Int32);

		procedure GetAbsoluteAddressAndType(relativeAddr: Cardinal; var info: TAddressTypeInfo);
		procedure GetPpuAbsoluteAddressAndType(relativeAddr: Cardinal; var info: TPpuAddressTypeInfo);

		function  ToAbsoluteSaveRamAddress(addr: Word): Int32;
		function  ToAbsoluteWorkRamAddress(addr: Word): Int32;
		function  ToAbsoluteChrAddress(addr: Word): Int32;
		function  ToAbsoluteChrRamAddress(addr: Word): Int32;
		function  ToAbsoluteChrRomAddress(addr: Word): Int32;
		function  FromAbsoluteChrAddress(addr: Cardinal): Int32;
		function  FromAbsoluteAddress(addr: Cardinal; _type: TAddressType = TAddressType.PrgRom): Int32;
		function  FromAbsolutePpuAddress(addr: Cardinal; _type: TPpuAddressType): Int32;
}
		function  ToAbsoluteAddress(address: Word): Integer;

		function  IsReadRegister (addr: Word): Boolean; inline;
		function  IsWriteRegister(addr: Word): Boolean; inline;

		procedure GetRomFileData(var _out: TBytes; asIpsFile: Boolean; header: PByte);

		//function  GetPrgChrCopy: TBytes;
		procedure RestorePrgChrBackup(var backupData: TBytes);
		procedure RevertPrgChrChanges;
		function  HasPrgChrChanges: Boolean;
		procedure CopyPrgChrRom(var _mapper: TMapper);

		procedure LoadSnapshot; override;
		procedure SaveSnapshot; override;
	end;

	function GetMapperClass(MapperID, SubMapperID: Word; RomData: PRomData = nil): TMapperClass;

var
	Mapper: TMapper;


implementation

uses
	Basement.Util, Math, NES.Console, NES.Config, TextOutput,

	NES.Mapper_FDS,
	NES.Mapper_VRC2_4,		NES.Mapper_VRC6,		NES.Mapper_VRC7,
	NES.Mapper_KS202,		NES.Mapper_BandaiFCG,	NES.Mapper_Namco163,
	NES.Mapper_Sachen,		NES.Mapper_Sachen8259,	NES.Mapper_TXC,
	NES.Mapper_JyCompany,

	NES.Mapper_000, NES.Mapper_001, NES.Mapper_002, NES.Mapper_003, NES.Mapper_004,
					NES.Mapper_005, NES.Mapper_006, NES.Mapper_007, NES.Mapper_009,
	NES.Mapper_010, NES.Mapper_011, NES.Mapper_013, NES.Mapper_015, NES.Mapper_018,
	NES.Mapper_028,
	NES.Mapper_030, NES.Mapper_032, NES.Mapper_033, NES.Mapper_034,
					NES.Mapper_037, NES.Mapper_038,
	NES.Mapper_040, NES.Mapper_041, NES.Mapper_042, NES.Mapper_043,
					NES.Mapper_046, NES.Mapper_048,
	NES.Mapper_050, NES.Mapper_053, NES.Mapper_057, NES.Mapper_058, NES.Mapper_059,
	NES.Mapper_060, NES.Mapper_061, NES.Mapper_062, NES.Mapper_064, NES.Mapper_065,
					NES.Mapper_066, NES.Mapper_067, NES.Mapper_068, NES.Mapper_069,
	NES.Mapper_071, NES.Mapper_072, NES.Mapper_073, NES.Mapper_075,
					NES.Mapper_078, NES.Mapper_079,
	NES.Mapper_080, NES.Mapper_082, NES.Mapper_083, NES.Mapper_086, NES.Mapper_087,
	NES.Mapper_093,
	NES.Mapper_105,
	NES.Mapper_111, NES.Mapper_112, NES.Mapper_117, NES.Mapper_118,
	NES.Mapper_140,
	NES.Mapper_150, NES.Mapper_152, NES.Mapper_156,
	NES.Mapper_168,
	NES.Mapper_177,
	NES.Mapper_180,	NES.Mapper_184,
	NES.Mapper_190, NES.Mapper_193,
	NES.Mapper_206,
	NES.Mapper_222, NES.Mapper_225,
					NES.Mapper_226, NES.Mapper_227, NES.Mapper_228, NES.Mapper_229,
	NES.Mapper_230, NES.Mapper_231, NES.Mapper_232, NES.Mapper_234, NES.Mapper_235,
	NES.Mapper_246, NES.Mapper_268, NES.Mapper_290,
	NES.Mapper_521;

{$INCLUDE coredefs.inc}

function GetMapperClass(MapperID, SubMapperID: Word; RomData: PRomData): TMapperClass;
begin
	Result := nil;

	case MapperID of
		0:   Result := TMapper_000;			// NROM
		1:   Result := TMapper_001;			// MMC1/SxROM
		2:   Result := TMapper_002;			// UNROM
		3:   Result := TMapper_003;			// CNROM
		4:   if SubMapperID = 3 then
		     Result := TMapper_004_MCACC	// MMC3 MCACC
		     else
		     Result := TMapper_004; 		// MMC3/MMC6
		5:   Result := TMapper_005; 		// MMC5/ExROM
		6:   Result := TMapper_006; 		// FrontFareast
		7:   Result := TMapper_007; 		// AxROM
		8:   Result := TMapper_006; 		// FrontFareast
		9:   Result := TMapper_009; 		// MMC2
		10:  Result := TMapper_010; 		// MMC4
		11:  Result := TMapper_011; 		// Color Dreams
		13:  Result := TMapper_013; 		// CPROM
		15:  Result := TMapper_015; 		// K-1029
		16:  Result := TMapper_BandaiFCG;	// Bandai FCG
		17:  Result := TMapper_006; 		// FrontFareast
		18:  Result := TMapper_018; 		// Jaleco SS88006
		19:  Result := TMapper_Namco163; 	// Namco 163
		21:  Result := TMapper_VRC2_4; 		// VRC4
		22:  Result := TMapper_VRC2_4; 		// VRC2
		23:  Result := TMapper_VRC2_4; 		// VRC2/4
		24:  Result := TMapper_VRC6a;  		// VRC6a
		25:  Result := TMapper_VRC2_4; 		// VRC2/4
		26:  Result := TMapper_VRC6b;  		// VRC6b
		27:  Result := TMapper_VRC2_4; 		// VRC4
		28:  Result := TMapper_028;    		// Action 53
		30:  Result := TMapper_030;    		// UNROM512
		32:  Result := TMapper_032;    		// IREM G-101
		33:  Result := TMapper_033;    		// Taito TC0190
		34:  if RomData <> nil then Result :=
			GetMapper034Class(MapperID, SubMapperID, Length(RomData.ChrRom));
		35:  Result := TMapper_035; 		// JyCompany
		36:  Result := TMapper_036; 		// TXC22000
		37:  Result := TMapper_037; 		// SMB + Tetris + Nintendo World Cup
		38:  Result := TMapper_038; 		// UnlPci556
		39:  Result := TMapper_241; 		// BxROM-like
		40:  Result := TMapper_040; 		// NTDEC 2722
		41:  Result := TMapper_041; 		// Caltron41
		42:  Result := TMapper_042; 		// FDS games hacked to cartridge
		43:  Result := TMapper_043; 		// TONY-I/YS-612 (SMB2J)
		44:  Result := TMapper_044; 		// MMC3 variant
		45:  Result := TMapper_045; 		// MMC3 variant
		46:  Result := TMapper_046; 		// Color Dreams Rumblestation 15-in-1
		47:  Result := TMapper_047; 		// MMC3 variant
		48:  Result := TMapper_048; 		// Taito TC0690
		49:  Result := TMapper_049; 		// MMC3 variant
		50:  Result := TMapper_050; 		// N-32 conversion of SMB2(J)
		52:  Result := TMapper_052; 		// multicart: MMC3_52 (Mario 7-in-1)
		53:  Result := TMapper_053; 		// multicart: Supervision
		56:  Result := TMapper_KS202; 		// Kaiser KS202
		57:  Result := TMapper_057; 		// multicart: GK
		58:  Result := TMapper_058; 		// multicart: 68-in-1
		59:  Result := TMapper_059; 		// multicart: UnlD1038
		60:  Result := TMapper_060; 		// multicart: Reset Based 4-in-1
		61:  Result := TMapper_061; 		// multicart (20-in-1)
		62:  Result := TMapper_062; 		// multicart (Super 700-in-1)
		64:  Result := TMapper_064; 		// Tengen RAMBO-1
		65:  Result := TMapper_065; 		// Irem H3001
		66:  Result := TMapper_066; 		// GxROM
		67:  Result := TMapper_067; 		// Sunsoft 3
		68:  Result := TMapper_068; 		// Sunsoft 4
		69:  Result := TMapper_069; 		// Sunsoft FME7
		70:  Result := TMapper_070; 		// Bandai74161_7432
		71:  Result := TMapper_071; 		// BF909x
		72:  Result := TMapper_072; 		// Jaleco JF-17
		73:  Result := TMapper_VRC3; 		// VRC3
		74:  Result := TMapper_004_ChrRam; 	// MMC3
		75:  Result := TMapper_075; 		// VRC1
		76:  Result := TMapper_076; 		// Namcot 118 variant
		78:  Result := TMapper_078; 		// Jaleco JF-16
		79:  Result := TMapper_079; 		// NINA-03/NINA-06
		80:  Result := TMapper_080; 		// Taito X1005
		82:  Result := TMapper_082; 		// Taito X1017
		83:  Result := TMapper_083; 		// Cony/Yoko
		85:  Result := TMapper_VRC7; 		// VRC7
		86:  Result := TMapper_086; 		// Jaleco JF-13
		87:  Result := TMapper_087;			// Jaleco JF-xx
		88:  Result := TMapper_088; 		// Namcot 118 variant
		90:  Result := TMapper_090; 		// JyCompany
		91:  Result := TMapper_091; 		// MMC3 variant
		92:  Result := TMapper_092; 		// Jaleco JF-19
		93:  Result := TMapper_093; 		// Sunsoft
		95:  Result := TMapper_095; 		// Namcot 118 variant
		101: Result := TMapper_101;			// Jaleco JF-xx
		105: Result := TMapper_105; 		// MMC1_105
		111: Result := TMapper_111; 		// GTROM/Cheapocabra
		112: Result := TMapper_112; 		// Asder, NTDEC
		113: Result := TMapper_079; 		// NINA-03/NINA-06 Multicart mode
		114: Result := TMapper_114; 		// MMC3 clone
		115: Result := TMapper_115; 		// MMC3 clone
		117: Result := TMapper_117; 		// Future Media
		118: Result := TMapper_118; 		// TKSROM and TLSROM
		119: Result := TMapper_004_ChrRam; 	// MMC3 TQROM
		121: Result := TMapper_121;			// MMC3 variant
		132: Result := TMapper_132; 		// TXC Corporation TXC22211A
		133: Result := TMapper_133; 		// Sachen
		136: Result := TMapper_136; 		// Sachen 3011
		137: Result := TMapper_137; 		// Sachen 8259D
		138: Result := TMapper_138; 		// Sachen 8259B
		139: Result := TMapper_139; 		// Sachen 8259C
		140: Result := TMapper_140; 		// Jaleco JF-11/JF-14
		141: Result := TMapper_141; 		// Sachen8259A
		142: Result := TMapper_KS202; 		// Kaiser KS202
		144: Result := TMapper_011; 		// Color Dreams
		147: Result := TMapper_147; 		// Sachen TC-011
		148: Result := TMapper_148; 		// Sachen
		149: Result := TMapper_149; 		// Sachen
		150: Result := TMapper_150; 		// Sachen SA-015
		152: Result := TMapper_152; 		// Bandai74161_7432
		153: Result := TMapper_BandaiFCG; 	// Bandai FCG
		154: Result := TMapper_154; 		// Namcot 118 variant
		155: Result := TMapper_155; 		// MMC1 variant
		156: Result := TMapper_156; 		// Daou Infosys
		157: Result := TMapper_BandaiFCG; 	// Bandai FCG
		158: Result := TMapper_158; 		// Tengen 800037
		159: Result := TMapper_BandaiFCG; 	// Bandai FCG
		168: Result := TMapper_168; 		// Racermate
		172: Result := TMapper_172; 		// TXC Corporation TXC22211B !!! verify init
		173: Result := TMapper_173; 		// TXC Corporation TXC22211C
		177: Result := TMapper_177; 		// Hénggé Diànzǐ
		180: Result := TMapper_180; 		// UnRom_180
		182: Result := TMapper_114; 		// MMC3 clone; TODO verify; Mesen has a separate impl. for this
		184: Result := TMapper_184; 		// Sunsoft
		185: Result := TMapper_185;			// CNROM w/ copy protection
		187: Result := TMapper_187; 		// MMC3 variant
		189: Result := TMapper_189; 		// MMC3 variant
		190: Result := TMapper_190; 		// Magic Kid Googoo
		191: Result := TMapper_004_ChrRam; 	// MMC3 variant
		192: Result := TMapper_004_ChrRam; 	// MMC3 variant
		193: Result := TMapper_193; 		// NTDEC TC-112
		194: Result := TMapper_004_ChrRam; 	// MMC3 variant
		195: Result := TMapper_004_ChrRam; 	// MMC3 variant
		206: Result := TMapper_206; 		// Namcot 118
		207: Result := TMapper_207; 		// Taito X1005
		209: Result := TMapper_209; 		// JyCompany
		210: Result := TMapper_Namco163; 	// Namco 163
		211: Result := TMapper_211; 		// JyCompany
		222: Result := TMapper_222; 		// Dragon Ninja pirate
		225: Result := TMapper_225; 		// multicart: various
		226: Result := TMapper_226; 		// multicart (76-in-1, Super 42-in-1)
		227: Result := TMapper_227; 		// multicart (1200-in-1, 600-in-1), Nanjing/Waixing/Yancheng
		228: Result := TMapper_228; 		// Active Enterprises Action 52/Cheetahmen II
		229: Result := TMapper_229; 		// multicart: BMC 31-IN-1
		230: Result := TMapper_230; 		// multicart: 22-in-1
		231: Result := TMapper_231; 		// multicart: 22-in-1 alt.
		232: Result := TMapper_232; 		// BF9096
		233: Result := TMapper_233; 		// multicart: Super 42-in-1 (alt)
		234: Result := TMapper_234; 		// multicart: Maxi 15
		235: Result := TMapper_235; 		// multicart: Bmc235
		241: Result := TMapper_241; 		// BxROM-like
		243: Result := TMapper_243; 		// Sachen SA-015
		245: Result := TMapper_245; 		// MMC3 variant
		246: Result := TMapper_246; 		// Fong Shen Bang - Zhu Lu Zhi Zhan
		250: Result := TMapper_250; 		// MMC3 variant
		255: Result := TMapper_255; 		// multicart: 110-in-1
		262: Result := TMapper_262;			// MMC3 variant
		268: Result := TMapper_268; 		// MMC3-Coolboy
		290: Result := TMapper_290;			// BmcNtd03 (Asder - 20 in 1)
		521: Result := TMapper_521; 		// DreamTech01

		TMapper.FdsMapperID:
			Result := TMapper_FDS; // Famicom Disk System
	end;
end;


// ============================================================================
// TMapper
// ============================================================================

constructor TMapper.Create(cartridge: TCartridge);
var
	i: Integer;
begin
	inherited Create('MAPR');

	_NametableCount := $10;
	_NametableSize  := $400;

	RegisterProperty(32, @_nametableCount);
	RegisterProperty(8,  @MirroringType);
	RegisterProperty(16, @vramOpenBusValue);
	RegisterProperty(1,  @hasChrBattery);
	RegisterProperty(1,  @onlyChrRam);

	cart := cartridge;

	prgSize    := Length(cart.ROM);
	chrRomSize := Length(cart.VMEM);

	for i := 0 to 255 do
	begin
		prgPages[i] := nil;
		chrPages[i] := nil;
	end;

	NametableCount := 2;
	vramOpenBusValue := -1;
	onlyChrRam := False;
end;

destructor TMapper.Destroy;
begin
	InvA13Audio.Free;
	InvOE1Audio.Free;

	SaveBattery;

	inherited;
end;

function TMapper.IsReadRegister(addr: Word): Boolean;
begin Result := _allowRegisterRead and isReadRegisterAddr[addr]; end;

function TMapper.IsWriteRegister(addr: Word): Boolean;
begin Result := isWriteRegisterAddr[addr]; end;

// private

// Make sure the page size is no bigger than the size of the ROM itself
// Otherwise we will end up reading from unallocated memory

function TMapper.InternalGetPrgPageSize: Word;
begin Result := Min(GetPRGPageSize, prgSize); end;

function TMapper.InternalGetSaveRamPageSize: Word;
begin Result := Min(GetSaveRamPageSize, saveRamSize); end;

function TMapper.InternalGetWorkRamPageSize: Word;
begin Result := Min(GetWorkRamPageSize, workRamSize); end;

function TMapper.InternalGetChrPageSize: Word;
begin Result := Min(GetCHRPageSize, chrRomSize); end;

function TMapper.InternalGetChrRamPageSize: Word;
begin Result := Min(GetChrRamPageSize, chrRamSize); end;

function TMapper.GetPRGPageSize:       Word;     begin Result := 0;     end;
function TMapper.GetCHRPageSize:       Word;     begin Result := 0;     end;
function TMapper.GetChrRamPageSize:    Word;     begin Result := $2000; end;
function TMapper.GetSaveRamSize:       Cardinal; begin Result := IfThen(HasBattery, $2000, 0); end;
function TMapper.GetSaveRamPageSize:   Cardinal; begin Result := $2000; end;
function TMapper.ForceChrBattery:      Boolean;  begin Result := False; end;
function TMapper.ForceSaveRamSize:     Boolean;  begin Result := False; end;
function TMapper.ForceWorkRamSize:     Boolean;  begin Result := False; end;
function TMapper.GetChrRamSize:        Cardinal; begin Result := $0000; end;
function TMapper.GetWorkRamSize:       Cardinal; begin Result := IfThen(HasBattery, 0, $2000); end;
function TMapper.GetWorkRamPageSize:   Cardinal; begin Result := $2000; end;
function TMapper.RegisterStartAddress: Word;     begin Result := $8000; end;
function TMapper.RegisterEndAddress:   Word;     begin Result := $FFFF; end;
function TMapper.AllowRegisterRead:    Boolean;  begin Result := False; end;
function TMapper.GetDipSwitchCount:    Cardinal; begin Result := 0;     end;
function TMapper.HasBusConflicts:      Boolean;  begin Result := False; end;

function TMapper.IsNes20: Boolean;
begin Result := RomInfo.NesHeader.GetRomHeaderVersion = Nes2_0; end;

function TMapper.ValidateAddressRange(startAddr, endAddr: Word): Boolean;
begin
	//Ignore this request in release mode - granularity smaller than 256 bytes is not supported
	if ((startAddr and $FF) <> 0) or ((endAddr and $FF) <> $FF) then
		Result := False
	else
		Result := True;
end;

// protected

procedure TMapper.InitMapper;
begin
end;

procedure TMapper.InitMapper(var RomData: TRomData);
begin
end;

function TMapper.InternalReadRam(addr: Word): Byte;
begin
	Result := IfThen(prgPages[addr >> 8] <> nil, prgPages[addr >> 8][addr and $FF], 0);
end;

function TMapper.ReadRegister(addr: Word): Byte;
begin
	Result := 0;
end;

procedure TMapper.WriteRegister(addr: Word;  value: Byte);
begin
end;

procedure TMapper.SetCpuMemoryMapping(startAddr, endAddr: Word;
	pageNumber: Int32; memoryType: TPrgMemoryType; accessType: TMemoryAccessType);
begin
	SetCpuMemoryMapping(startAddr, endAddr, pageNumber, memoryType, Ord(accessType));
end;

procedure TMapper.SetCpuMemoryMapping(startAddr, endAddr: Word;
	pageNumber: Int32; memoryType: TPrgMemoryType; accessType: Int8 = -1);
var
	pageCount, pageSize, addr: Word;
	defaultAccessType: Byte;

	function WrapPageNumber(page: Int32): Int32; inline;
	begin
		// Can't use modulo for negative number because pageCount is
		// sometimes not a power of 2.  (Fixes some Mapper 191 games)
		if page < 0 then
			Result := page + pageCount
		else
			Result := page mod pageCount;
	end;

begin
	if (not ValidateAddressRange(startAddr, endAddr)) or (startAddr > $FF00)
		or (endAddr <= startAddr) then Exit;

	defaultAccessType := Ord(TMemoryAccessType.maRead);

	case memoryType of

		TPrgMemoryType.PrgRom:
		begin
			pageCount := GetPRGPageCount;
			pageSize  := InternalGetPrgPageSize;
		end;

		TPrgMemoryType.PrgSaveRam:
		begin
			pageSize := InternalGetSaveRamPageSize;
			if pageSize = 0 then
			begin
				{$IFDEF DEBUG}
				DebugMessage('[Mapper] Tried to map undefined save ram.');
				{$ENDIF}
				Exit;
			end;
			pageCount := saveRamSize div pageSize;
			defaultAccessType := defaultAccessType or Ord(TMemoryAccessType.maWrite);
		end;

		TPrgMemoryType.PrgWorkRam:
		begin
			pageSize := InternalGetWorkRamPageSize;
			if pageSize = 0 then
			begin
				{$IFDEF DEBUG}
				DebugMessage('[Mapper] Tried to map undefined work ram.');
				{$ENDIF}
				Exit;
			end;
			pageCount := workRamSize div pageSize;
			defaultAccessType := defaultAccessType or Ord(TMemoryAccessType.maWrite);
		end;

	end;

	if pageCount = 0 then
	begin
		{$IFDEF DEBUG}
		DebugMessage('[Mapper] Tried to map undefined save/work ram.');
		{$ENDIF}
		Exit;
	end;

	pageNumber := {%H-}WrapPageNumber(pageNumber);

	accessType := IfThen(accessType <> -1, accessType, defaultAccessType);

	if Word(endAddr - startAddr) >= pageSize then
	begin
		{$IFDEF DEBUG}
		DebugMessage('[Mapper] Tried to map undefined prg - page size too small for selected range.');
		{$ENDIF}
		// If range is bigger than a single page, keep going until we reach the last page
		addr := startAddr;
		while addr <= (endAddr - pageSize + 1) do
		begin
			SetCpuMemoryMapping(addr, addr+pageSize-1, memoryType, pageNumber*pageSize, accessType);
			Inc(addr, pageSize);
			Inc(pageNumber);
			pageNumber := WrapPageNumber(pageNumber);
		end;
	end
	else
		SetCpuMemoryMapping(startAddr, endAddr, memoryType, pageNumber*pageSize, accessType);
end;

procedure TMapper.SetCpuMemoryMapping(startAddr, endAddr: Word;
	memoryType: TPrgMemoryType; sourceOffset: Cardinal; accessType: Int8);
var
	source: PByte;
	i, firstSlot, slotCount: Int32;
begin
	case memoryType of
		TPrgMemoryType.PrgRom:     source := @prgRom[0];
		TPrgMemoryType.PrgSaveRam: source := @saveRam[0];
		TPrgMemoryType.PrgWorkRam: source := @workRam[0];
	end;

	firstSlot := startAddr >> 8;
	slotCount := (endAddr - startAddr + 1) >> 8;
	for i := 0 to slotCount-1 do
	begin
		prgMemoryOffset[firstSlot+i] := Int32(sourceOffset) + i * $100;
		prgMemoryType  [firstSlot+i] := memoryType;
		prgMemoryAccess[firstSlot+i] := TMemoryAccessType(accessType);
	end;

	SetCpuMemoryMapping(startAddr, endAddr, source+sourceOffset, accessType);
end;

procedure TMapper.SetCpuMemoryMapping(startAddr, endAddr: Word;
	sourceMemory: PByte; accessType: Int8 = -1);
var
	i: Integer;
begin
	if ValidateAddressRange(startAddr, endAddr) then
	begin
		startAddr := startAddr >> 8;
		endAddr   := endAddr   >> 8;
		for i := startAddr to endAddr do
		begin
			prgPages[i] := sourceMemory;
			prgMemoryAccess[i] :=
				ChooseMemoryAccessType(accessType < 0, maRead, TMemoryAccessType(accessType));
			if sourceMemory <> nil then
				Inc(sourceMemory, $100);
		end;
	end;
end;

procedure TMapper.RemoveCpuMemoryMapping(startAddr, endAddr: Word);
var
	i, firstSlot, slotCount: Int32;
begin
	// Unmap this section of memory (causing open bus behavior)
	firstSlot := startAddr shr 8;
	slotCount := (endAddr - startAddr + 1) shr 8;
	for i := 0 to slotCount-1 do
	begin
		prgMemoryOffset[firstSlot + i] := -1;
		prgMemoryType  [firstSlot + i] := TPrgMemoryType.PrgRom;
		prgMemoryAccess[firstSlot + i] := TMemoryAccessType.maNoAccess;
	end;
	SetCpuMemoryMapping(startAddr, endAddr, nil, Ord(TMemoryAccessType.maNoAccess));
end;

procedure TMapper.SetPpuMemoryMapping(startAddr, endAddr, pageNumber: Word;
	memoryType: TChrMemoryType = TChrMemoryType.ChrDefault; accessType: Int8 = -1);
var
	pageCount, pageSize, addr: Word;
	defaultAccessType: Byte;
begin
//Log('SetPpuMemoryMapping: ' + Format('%d, %d, %d, %d', [startaddr, endaddr, pagenumber, accesstype ]) );

	if (not ValidateAddressRange(startAddr, endAddr)) or (startAddr > $3F00) or
		(endAddr > $3FFF) or (endAddr <= startAddr) then
		Exit;

	defaultAccessType := Ord(TMemoryAccessType.maRead);

	case memoryType of

		TChrMemoryType.ChrDefault:
		begin
			pageSize := InternalGetChrPageSize;
			if pageSize = 0 then
			begin
				{$IFDEF DEBUG}
				DebugMessage('[Mapper] Tried to map undefined CHR ROM/RAM.');
				{$ENDIF}
				Exit;
			end;
			pageCount := GetCHRPageCount;
			if onlyChrRam then
				defaultAccessType := defaultAccessType or Ord(TMemoryAccessType.maWrite);
		end;

		TChrMemoryType.ChrRom:
		begin
			pageSize := InternalGetChrPageSize;
			if pageSize = 0 then
			begin
				{$IFDEF DEBUG}
				DebugMessage('[Mapper] Tried to map undefined CHR ROM.');
				{$ENDIF}
				Exit;
			end;
			pageCount := GetCHRPageCount;
		end;

		TChrMemoryType.ChrRam:
		begin
			pageSize := InternalGetChrRamPageSize;
			if pageSize = 0 then
			begin
				{$IFDEF DEBUG}
				DebugMessage('[Mapper] Tried to map undefined CHR RAM.');
				{$ENDIF}
				Exit;
			end;
			pageCount := chrRamSize div pageSize;
			defaultAccessType := defaultAccessType or Ord(TMemoryAccessType.maWrite);
		end;

		TChrMemoryType.ChrNametableRam:
		begin
			pageSize  := _NametableSize;
			pageCount := _NametableCount;
			defaultAccessType := defaultAccessType or Ord(TMemoryAccessType.maWrite);
		end;

	end;

	if pageCount = 0 then
	begin
		{$IFDEF DEBUG}
		DebugMessage('[Mapper] Tried to map undefined CHR RAM. (pagecount=0)');
		{$ENDIF}
		Exit;
	end;

	pageNumber := pageNumber mod pageCount;

	if (endAddr - startAddr) >= pageSize then
	begin
		{$IFDEF DEBUG}
		DebugMessage('[Mapper] Tried to map undefined CHR - page size too small for selected range.');
		{$ENDIF}
		addr := startAddr;
		while addr <= (endAddr - pageSize + 1) do
		begin
			//Log(' --> addr=', addr);
			SetPpuMemoryMapping(addr, addr+pageSize-1, memoryType, pageNumber*pageSize, accessType);
			Inc(addr, pageSize);
			pageNumber := (pageNumber + 1) mod pageCount;
		end;
	end
	else
	begin
		//Log(' ==> startAddr=', startAddr);
		SetPpuMemoryMapping(startAddr, endAddr, memoryType, pageNumber*pageSize,
			IfThen(accessType = -1, defaultAccessType, accessType));
	end;
end;

procedure TMapper.SetPpuMemoryMapping(startAddr, endAddr: Word;
	memoryType: TChrMemoryType; sourceOffset: Cardinal; accessType: Int8);
var
	source: PByte;
	i, firstSlot, slotCount: Int32;
begin
	case memoryType of
		TChrMemoryType.ChrRom:          source := @chrRom[0];
		TChrMemoryType.ChrRam:          source := @chrRam[0];
		TChrMemoryType.ChrNametableRam: source := @nametableRam[0];
	else
		if onlyChrRam then
			source := @chrRam[0]
		else
			source := @chrRom[0];
		memoryType := ChooseChrMemoryType(onlyChrRam,
			TChrMemoryType.ChrRam, TChrMemoryType.ChrRom);
	end;

	firstSlot := startAddr >> 8;
	slotCount := (endAddr - startAddr + 1) >> 8;
	for i := 0 to slotCount-1 do
	begin
		chrMemoryOffset[firstSlot + i] := Int32(sourceOffset) + (i * $100);
		chrMemoryType  [firstSlot + i] := memoryType;
		chrMemoryAccess[firstSlot + i] := TMemoryAccessType(accessType);
	end;

	SetPpuMemoryMapping(startAddr, endAddr, source+sourceOffset, accessType);
end;

procedure TMapper.SetPpuMemoryMapping(startAddr, endAddr: Word;
	sourceMemory: PByte; accessType: Int8 = -1);
var
	i: Int32;
begin
	if ValidateAddressRange(startAddr, endAddr) then
	begin
		startAddr := startAddr >> 8;
		endAddr   := endAddr   >> 8;
		for i := startAddr to endAddr do
		begin
			chrPages[i] := sourceMemory;
			chrMemoryAccess[i] := ChooseMemoryAccessType(accessType <> -1,
				TMemoryAccessType(accessType), maReadWrite);
			if sourceMemory <> nil then
				Inc(sourceMemory, $100);
		end;
	end;
end;

procedure TMapper.RemovePpuMemoryMapping(startAddr, endAddr: Word);
var
	i, firstSlot, slotCount: Int32;
begin
	// Unmap this section of memory (causing open bus behavior)
	firstSlot := startAddr >> 8;
	slotCount := (endAddr - startAddr + 1) >> 8;
	for i := 0 to slotCount-1 do
	begin
		chrMemoryOffset[firstSlot + i] := -1;
		chrMemoryType  [firstSlot + i] := TChrMemoryType.ChrDefault;
		chrMemoryAccess[firstSlot + i] := TMemoryAccessType.maNoAccess;
	end;
	SetPpuMemoryMapping(startAddr, endAddr, nil, Ord(TMemoryAccessType.maNoAccess));
end;

procedure TMapper.SelectPRGPage(slot: Word; page: Int32;
	memoryType: TPrgMemoryType = TPrgMemoryType.PrgRom);
var
	startAddr, endAddr: Word;
begin
	if (prgSize < $8000) and (GetPRGPageSize > prgSize) then
	begin
		// Total PRG size is smaller than available memory range, map the entire PRG to all slots
		// i.e same logic as NROM (mapper 0) when PRG is 16kb. Needed by "Pyramid" (mapper 79)
		// "PrgSizeWarning"
		for slot := 0 to ($8000 div prgSize)-1 do
		begin
			startAddr := $8000 + (slot * prgSize);
			endAddr   := startAddr + prgSize - 1;
			SetCpuMemoryMapping(startAddr, endAddr, 0, memoryType);
		end;
	end
	else
	begin
		startAddr := $8000 + (slot * InternalGetPrgPageSize);
		endAddr := startAddr + InternalGetPrgPageSize - 1;
		SetCpuMemoryMapping(startAddr, endAddr, page, memoryType);
	end;
end;

procedure TMapper.SelectPrgPage2x(slot: Word; page: Int32;
	memoryType: TPrgMemoryType = TPrgMemoryType.PrgRom);
begin
	SelectPRGPage(slot*2,   page,   memoryType);
	SelectPRGPage(slot*2+1, page+1, memoryType);
end;

procedure TMapper.SelectPrgPage4x(slot: Word; page: Int32;
	memoryType: TPrgMemoryType = TPrgMemoryType.PrgRom);
begin
	SelectPrgPage2x(slot*2,   page,   memoryType);
	SelectPrgPage2x(slot*2+1, page+2, memoryType);
end;

procedure TMapper.SelectCHRPage(slot, page: Word;
	memoryType: TChrMemoryType = TChrMemoryType.ChrDefault);
var
	startAddr, endAddr, pageSize: Word;
begin
	if memoryType = TChrMemoryType.ChrNametableRam then
		pageSize := _NametableSize
	else
		pageSize := IfThen(memoryType = TChrMemoryType.ChrRam,
			InternalGetChrRamPageSize, InternalGetChrPageSize);

	startAddr := slot * pageSize;
	endAddr   := startAddr + pageSize - 1;

	SetPpuMemoryMapping(startAddr, endAddr, page, memoryType);
end;

procedure TMapper.SelectChrPage2x(slot, page: Word;
	memoryType: TChrMemoryType = TChrMemoryType.ChrDefault);
begin
	SelectCHRPage(slot*2,   page,   memoryType);
	SelectCHRPage(slot*2+1, page+1, memoryType);
end;

procedure TMapper.SelectChrPage4x(slot, page: Word;
	memoryType: TChrMemoryType = TChrMemoryType.ChrDefault);
begin
	SelectChrPage2x(slot*2,   page,   memoryType);
	SelectChrPage2x(slot*2+1, page+2, memoryType);
end;

procedure TMapper.SelectChrPage8x(slot, page: Word;
	memoryType: TChrMemoryType = TChrMemoryType.ChrDefault);
begin
	SelectChrPage4x(slot,     page,   memoryType);
	SelectChrPage4x(slot*2+1, page+4, memoryType);
end;

function TMapper.GetPRGPageCount: Word;
var
	pageSize: Word;
begin
	pageSize := InternalGetPrgPageSize;
	Result := IfThen(pageSize > 0, prgSize div pageSize, 0);
end;

function TMapper.GetCHRPageCount: Word;
var
	pageSize: Word;
begin
	pageSize := InternalGetChrPageSize;
	Result := IfThen(pageSize > 0, chrRomSize div pageSize, 0);
end;

function TMapper.GetPowerOnByte(defaultValue: Byte = 0): Byte;
begin
	if Configuration.Emulator.PowerOnState.RandomizeForMappers then
		Result := Random($FF)
	else
		Result := defaultValue;
end;

function TMapper.GetDipSwitches: Cardinal;
begin
	// TODO
	Result := 0; // console.GetSettings.GetDipSwitches and ((1 shl GetDipSwitchCount) - 1);
end;

function TMapper.HasBattery: Boolean;
begin
	Result := RomInfo.HasBattery;
end;

procedure TMapper.SetupDefaultWorkRam;
begin
	// Setup a default work/save ram in $6000-$7FFF space
	if (HasBattery) and (saveRamSize > 0) then
		SetCpuMemoryMapping($6000, $7FFF, 0, TPrgMemoryType.PrgSaveRam)
	else
	if workRamSize > 0 then
		SetCpuMemoryMapping($6000, $7FFF, 0, TPrgMemoryType.PrgWorkRam);
end;

procedure TMapper.InitializeChrRam(Size: Int32 = -1);
var
	defaultRamSize: Cardinal;
begin
	defaultRamSize := IfThen(GetChrRamSize > 0, GetChrRamSize, $2000);
	chrRamSize := IfThen(Size >= 0, Size, defaultRamSize);
	if chrRamSize > 0 then
	begin
		SetLength(chrRam, chrRamSize);
		Console.InitializeRam(@chrRam[0], chrRamSize);
	end;
end;

procedure TMapper.AddRegisterRange(startAddr, endAddr: Word;
	operation: TMemoryOperation = moAny);
var
	i: Integer;
begin
	for i := startAddr to endAddr do
	begin
		if operation in [moRead, moAny] then
			isReadRegisterAddr[i]  := True;
		if operation in [moWrite, moAny] then
			isWriteRegisterAddr[i] := True;
	end;
end;

procedure TMapper.RemoveRegisterRange(startAddr, endAddr: Word;
	operation: TMemoryOperation = moAny);
var
	i: Integer;
begin
	for i := startAddr to endAddr do
	begin
		if operation in [moRead, moAny] then
			isReadRegisterAddr[i]  := False;
		if operation in [moWrite, moAny] then
			isWriteRegisterAddr[i] := False;
	end;
end;

procedure TMapper.RestorePrgChrState;
var
	i: Integer;
	startAddr: Word;
begin
	for i := 0 to $FF do
	begin
		startAddr := i shl 8;
		if prgMemoryAccess[i] <> maNoAccess then
			SetCpuMemoryMapping(startAddr, startAddr + $FF,
				prgMemoryType[i], prgMemoryOffset[i], Ord(prgMemoryAccess[i]))
		else
			RemoveCpuMemoryMapping(startAddr, startAddr + $FF);
	end;

	for i := 0 to $3F do
	begin
		startAddr := i shl 8;
		if chrMemoryAccess[i] <> maNoAccess then
			SetPpuMemoryMapping(startAddr, startAddr + $FF,
				chrMemoryType[i], chrMemoryOffset[i], Ord(chrMemoryAccess[i]))
		else
			RemovePpuMemoryMapping(startAddr, startAddr + $FF);
	end;
end;

function TMapper.GetNametable(nametableIndex: Byte): PByte;
begin
	if nametableIndex >= _NametableCount then
	begin
		{$IFDEF DEBUG}
		DebugMessage('[Mapper] Invalid nametable index: ' + nametableIndex.ToString);
		{$ENDIF}
		Result := @nametableRam[0];
	end
	else
	begin
		nametableCount := Max(nametableCount, nametableIndex + 1);
		Result := @nametableRam[nametableIndex * _NametableSize];
	end;
end;

procedure TMapper.SetNametable(index, nametableIndex: Byte);
begin
	if nametableIndex >= _NametableCount then
	begin
		{$IFDEF DEBUG}
		DebugMessage('[Mapper] Invalid nametable index: %d, %d', [index, nametableIndex]);
		{$ENDIF}
		Exit;
	end;

	nametableCount := Max(nametableCount, nametableIndex + 1);

	SetPpuMemoryMapping($2000 + index * $400, $2000 + (index + 1) * $400 - 1,
		nametableIndex, TChrMemoryType.ChrNametableRam);

	// Mirror $2000-$2FFF to $3000-$3FFF, while keeping a distinction between the addresses
	SetPpuMemoryMapping($3000 + index * $400, $3000 + (index + 1) * $400 - 1,
		nametableIndex, TChrMemoryType.ChrNametableRam);
end;

procedure TMapper.SetNametables(Index1, Index2, Index3, Index4: Byte);
begin
	SetNametable(0, Index1);
	SetNametable(1, Index2);
	SetNametable(2, Index3);
	SetNametable(3, Index4);
end;

function TMapper.GetMirroringType: TMirroringType;
begin
	Result := mirroringType;
end;

procedure TMapper.SetMirroringType(MirrorType: TMirroringType);
begin
	mirroringType := MirrorType;
	case MirrorType of
		MIRROR_VERTICAL:    SetNametables(0, 1, 0, 1);
		MIRROR_HORIZONTAL:  SetNametables(0, 0, 1, 1);
		MIRROR_SCREENAONLY: SetNametables(0, 0, 0, 0);
		MIRROR_SCREENBONLY: SetNametables(1, 1, 1, 1);
		MIRROR_FOURSCREENS: SetNametables(0, 1, 2, 3);
	end;
end;

function TMapper.ApplyMirroringType(B: Boolean; IfTrue, IfFalse: TMirroringType): TMirroringType;
begin
	Result := ChooseMirroringType(B, IfTrue, IfFalse);
	SetMirroringType(Result);
end;

function TMapper.ApplyMirroringType(I: Integer; IfTrue, IfFalse: TMirroringType): TMirroringType;
begin
	Result := ChooseMirroringType(I <> 0, IfTrue, IfFalse);
	SetMirroringType(Result);
end;

function TMapper.GenericChooseMirroringType(B: Byte): TMirroringType;
begin
	case B and 3 of
		0: Result := MIRROR_VERTICAL;
		1: Result := MIRROR_HORIZONTAL;
		2: Result := MIRROR_SCREENAONLY;
		3: Result := MIRROR_SCREENBONLY;
	else
		Exit(GetMirroringType);
	end;
	SetMirroringType(Result);
end;

function TMapper.InternalReadVRAM(addr: Word): Byte;
begin
	if Odd(Ord(chrMemoryAccess[addr >> 8])) then // maRead
		Result := chrPages[addr >> 8][addr and $FF]
	else
		// Open bus - "When CHR is disabled, the pattern tables are open bus. Theoretically,
		// this should return the LSB of the address read, but real-world behavior varies."
		Result := IfThen(vramOpenBusValue >= 0, vramOpenBusValue, addr and $FF);
end;

// public

procedure TMapper.Initialize(var RomData: TRomData);
var
	i: Integer;
begin
	romInfo := romData.Info;
	batteryFilename := GetBatteryFilename;

	if (romData.SaveRamSize = -1) or (ForceSaveRamSize) then
		saveRamSize := GetSaveRamSize
	else
		saveRamSize := romData.SaveRamSize;

	if (romData.WorkRamSize = -1) or (ForceWorkRamSize) then
		workRamSize := GetWorkRamSize
	else
		workRamSize := romData.WorkRamSize;

	_AllowRegisterRead := AllowRegisterRead;

	FillByte(IsReadRegisterAddr[0],  SizeOf(IsReadRegisterAddr),  0);
	FillByte(IsWriteRegisterAddr[0], SizeOf(IsWriteRegisterAddr), 0);
	AddRegisterRange(RegisterStartAddress, RegisterEndAddress, moAny);

	prgSize    := Length(romData.PrgRom);
	chrRomSize := Length(romData.ChrRom);
	originalPrgRom := romData.PrgRom;
	originalChrRom := romData.ChrRom;

	//SetLength(prgRom, prgSize);
	prgRom := romData.PrgRom;

	//SetLength(chrRom, chrRomSize);
	//if chrRomSize > 0 then
		chrRom := romData.ChrRom;

	hasChrBattery := (romData.SaveChrRamSize > 0) or (ForceChrBattery);

	case romData.Info.BusConflicts of
		buscDefault: _hasBusConflicts := HasBusConflicts;
		buscYes:     _hasBusConflicts := True;
		buscNo:      _hasBusConflicts := False;
	end;
	{$IFDEF DEBUG}
	if _hasBusConflicts then LogVerbose('[Mapper] Bus conflicts enabled');
	{$ENDIF}

	SetLength(saveRam, saveRamSize);
	SetLength(workRam, workRamSize);

	//Log('WorkRam: ', WorkRamSize);

	if saveRamSize > 0 then
		Console.InitializeRam(@saveRam[0], saveRamSize);
	if workRamSize > 0 then
		Console.InitializeRam(@workRam[0], workRamSize);

	nametableCount := 2;
	i := _NametableSize * _NametableCount;
	SetLength(nametableRam, i);
	Console.InitializeRam(@nametableRam[0], i);

	// Allow us to map a different page every 256 bytes
	for i := 0 to $FF do
	begin
		prgPages[i] := nil;
		prgMemoryOffset[i] := -1;
		prgMemoryType  [i] := TPrgMemoryType.PrgRom;
		prgMemoryAccess[i] := TMemoryAccessType.maNoAccess;

		chrPages[i] := nil;
		chrMemoryOffset[i] := -1;
		chrMemoryType  [i] := TChrMemoryType.ChrDefault;
		chrMemoryAccess[i] := TMemoryAccessType.maNoAccess;
	end;

	if chrRomSize = 0 then
	begin
		// Assume there is CHR RAM if no CHR ROM exists
		onlyChrRam := True;

		InitializeChrRam(romData.ChrRamSize);

		{$IFDEF DEBUG}
		LogVerbose('[Mapper] Assuming CHR RAM: %dK', [ChrRamSize div 1024]);
		{$ENDIF}

		// Map CHR RAM to $0000-$1FFF by default when no CHR ROM exists
		SetPpuMemoryMapping($0000, $1FFF, 0, TChrMemoryType.ChrRam);
		chrRomSize := chrRamSize;
	end
	else
	if romData.ChrRamSize >= 0 then
		InitializeChrRam(romData.ChrRamSize)
	else
	if GetChrRamSize > 0 then
		InitializeChrRam;

	if romData.Info.HasTrainer then
	begin
		if workRamSize >= $2000 then
			CopyMemory(@workRam[$1000], @romData.TrainerData[0], 512)
		else
		if saveRamSize >= $2000 then
			CopyMemory(@saveRam[$1000], @romData.TrainerData[0], 512);
	end;

	SetupDefaultWorkRam;
	SetMirroringType(RomData.Info.Mirroring);

	InitMapper;
	InitMapper(RomData);

	InvA13Audio.Free;
	InvA13Audio := TAudioInterference.Create(channel_InvA13);
	InvOE1Audio.Free;
	InvOE1Audio := TAudioInterference.Create(channel_InvOE1);

	LoadBattery; // Load battery data if present

	RomInfo.HasChrRam := HasChrRam;
end;

procedure TMapper.Reset(SoftReset: Boolean);
begin
// !!!	ApplyPrgCodes(@cart.ROM[0], prgSize);
end;

{function TMapper.GetAvailableFeatures: TConsoleFeatures;
begin
	Result := TConsoleFeatures.None;
end;}

procedure TMapper.SetNesModel(model: TNesModel);
begin
end;

procedure TMapper.ProcessCpuClock;
begin
end;

procedure TMapper.NotifyVRAMAddressChange(addr: Word);
begin
	// This is called when the VRAM addr on the PPU memory bus changes
	// Used by MMC3/MMC5/etc.
end;

procedure TMapper.GetMemoryRanges(var ranges: TMemoryRanges);
begin
	if RomInfo.System = VsSystem then
	begin
		ranges.AddHandler(moRead,  $6000, $FFFF);
		ranges.AddHandler(moWrite, $6000, $FFFF);
	end
	else
	begin
		ranges.AddHandler(moRead,  $4018, $FFFF);
		ranges.AddHandler(moWrite, $4018, $FFFF);
	end;
end;

{procedure TMapper.SetConsole(var console: TConsole);
begin
end;

function TMapper.GetMapperControlDevice: TBaseControlDevice;
begin
end;}

function TMapper.GetRomInfo: TRomInfo;
begin
	Result := RomInfo;
end;

function TMapper.GetMapperDipSwitchCount: Cardinal;
begin
	Result := GetDipSwitchCount;
end;

procedure TMapper.ApplySamples(buffer: PInt16; sampleCount: QWord; volume: Single);
begin
end;

function TMapper.PeekRAM(addr: Word): Byte;
begin
	Result := DebugReadRAM(addr);
end;

function TMapper.ReadRAM(addr: Word): Byte;
begin
	if (_allowRegisterRead) and (IsReadRegisterAddr[addr]) then
		Result := ReadRegister(addr)
	else
	//if (Ord(prgMemoryAccess[addr >> 8])) and (Ord(TMemoryAccessType.maRead)) <> 0 then
	if Odd(Ord(prgMemoryAccess[addr >> 8])) then
		Result := prgPages[addr >> 8][addr and $FF]
	else
		Result := Console.MemoryManager.GetOpenBus;
end;

procedure TMapper.WriteRAM(addr: Word; value: Byte);
var
	prgValue: Byte;
begin
	if IsWriteRegisterAddr[addr] then
	begin
		if _hasBusConflicts then
		begin
			prgValue := prgPages[addr >> 8][addr and $FF];
			//if (value <> prgValue) then
			//	Console.DebugProcessEvent(EventType.BusConflict);
			value := value and prgValue;
		end;
		WriteRegister(addr, value);
	end
	else
		WritePrgRam(addr, value);
end;

function TMapper.DebugReadRAM(addr: Word): Byte;
begin
	if Odd(Ord(prgMemoryAccess[addr >> 8])) then
		Result := prgPages[addr >> 8][addr and $FF]
	else
		Result := addr >> 8; // Fake open bus
end;

procedure TMapper.DebugWriteRAM(addr: Word; value: Byte);
begin
	if isWriteRegisterAddr[addr] then
	begin
		if _hasBusConflicts then
			value := value and prgPages[addr >> 8][addr and $FF];
	end
	else
		WritePrgRam(addr, value);
end;

procedure TMapper.WritePrgRam(addr: Word; value: Byte);
begin
	if (Ord(prgMemoryAccess[addr >> 8]) and Ord(maWrite)) <> 0 then
		prgPages[addr >> 8][addr and $FF] := value;
end;

function TMapper.MapperReadVRAM(addr: Word; operationType: TMemoryOperationType): Byte;
begin
	Result := InternalReadVRAM(addr);
end;

function TMapper.ReadVRAM(addr: Word;
	operationType: TMemoryOperationType = memopPpuRenderingRead): Byte;
begin
	Result := MapperReadVRAM(addr, operationType);
	// Console.DebugProcessVramReadOperation(operationType, addr, value);
end;

procedure TMapper.WriteVRAM(addr: Word; value: Byte);
begin
	//Console.DebugProcessVramWriteOperation(addr, value);
	if (Ord(chrMemoryAccess[addr >> 8]) and Ord(maWrite)) <> 0 then
		chrPages[addr >> 8][addr and $FF] := value;
end;

function TMapper.DebugReadVRAM(addr: Word; disableSideEffects: Boolean = True): Byte;
begin
	addr := addr and $3FFF;
	if not disableSideEffects then
		NotifyVRAMAddressChange(addr);
	Result := InternalReadVRAM(addr);
end;

procedure TMapper.DebugWriteVRAM(addr: Word; value: Byte; disableSideEffects: Boolean = True);
begin
	addr := addr and $3FFF;
	if disableSideEffects then
	begin
		// Always allow writes when side-effects are disabled
		if chrPages[addr shr 8] <> nil then
			chrPages[addr shr 8][addr and $FF] := value;
	end
	else
	begin
		NotifyVRAMAddressChange(addr);
		if (Ord(chrMemoryAccess[addr shr 8]) and Ord(maWrite)) <> 0 then
			chrPages[addr shr 8][addr and $FF] := value;
	end;
end;

procedure TMapper.CopyChrTile(address: Cardinal; dest: PByte);
begin
end;

//Debugger Helper Functions

function TMapper.HasChrRam: Boolean;
begin
	Result := chrRamSize > 0;
end;

function TMapper.HasChrRom: Boolean;
begin
	Result := not onlyChrRam;
end;

{function TMapper.GetState: TCartridgeState;
begin
end;}

function TMapper.GetPrgRom: PByte;
begin
	Result := @prgRom[0];
end;

function TMapper.GetWorkRam: PByte;
begin
	Result := @workRam[0];
end;

function TMapper.GetSaveRam: PByte;
begin
	Result := @saveRam[0];
end;

{
function TMapper.GetMemoryValue(memoryType: TDebugMemoryType, address: Cardinal): Byte;
begin
end;

procedure TMapper.SetMemoryValue(memoryType: TDebugMemoryType; address: Cardinal; value: Byte);
begin
end;

function TMapper.GetMemorySize(_type: TDebugMemoryType): Cardinal;
begin
end;

function TMapper.CopyMemory(memoryType: TDebugMemoryType, buffer: PByte): Cardinal;
begin
end;

procedure TMapper.WriteMemory(memoryType: TDebugMemoryType, buffer: PByte; length: Int32);
begin
end;

procedure TMapper.GetAbsoluteAddressAndType(relativeAddr: Cardinal; var info: TAddressTypeInfo);
begin
end;

procedure TMapper.GetPpuAbsoluteAddressAndType(relativeAddr: Cardinal; var info: TPpuAddressTypeInfo);
begin
end;
}

function TMapper.ToAbsoluteAddress(address: Word): Integer;
//var
//	prgAddr: Cardinal;
//	prgAddr: PByte;
begin
	Result := 0;

//	prgAddr := @prgPages[(addr >> 8) + address];
{
	prgAddr := GetAddressOf
	(prgPages[address shr 8]) + addr;
	if (prgAddr >= @prgRom[0]) and (prgAddr <= @prgRom[prgSize-1]) then
		Result := prgAddr - @prgRom
	else
		Result := -1;
} // !!! TODO for cheats
end;

{
function TMapper.ToAbsoluteSaveRamAddress(addr: Word): Int32;
begin
	Byte *prgRamAddr := prgPages[addr shr 8] + (Byte)addr;
	if (prgRamAddr >= saveRam) and (prgRamAddr < saveRam + saveRamSize) then
		Result := prgRamAddr - saveRam
	else
		Result := -1;
end;

function TMapper.ToAbsoluteWorkRamAddress(addr: Word): Int32;
begin
	Byte *prgRamAddr := prgPages[addr shr 8] + (Byte)addr;
	if (prgRamAddr >= workRam) and (prgRamAddr < workRam + workRamSize) then
		Result := prgRamAddr - _workRam
	else
		Result := -1;
end;

function TMapper.ToAbsoluteChrAddress(addr: Word): Int32;
begin
	Byte *chrAddr := chrPages[addr shr 8] + (Byte)addr;
	if (chrAddr >= chrRom) and (chrAddr < chrRom + chrRomSize) then
		Result := chrAddr - chrRom
	else
	if (chrAddr >= chrRam) and (chrAddr < chrRam + chrRamSize) then
		Result := chrAddr - chrRam
	else
		Result := -1;
end;

function TMapper.ToAbsoluteChrRamAddress(addr: Word): Int32;
begin
	Byte *chrAddr := chrPages[addr shr 8] + (Byte)addr;
	if (chrAddr >= chrRam) and (chrAddr < chrRam + chrRamSize) then
		Result := chrAddr - chrRam
	else
		Result := -1;
end;

function TMapper.ToAbsoluteChrRomAddress(addr: Word): Int32;
begin
	Byte *chrAddr := chrPages[addr shr 8] + (Byte)addr;
	if (chrAddr >= chrRom) and (chrAddr < chrRom + chrRomSize) then
		Result := chrAddr - chrRom
	else
		Result := -1;
end;

function TMapper.FromAbsoluteChrAddress(addr: Cardinal): Int32;
begin
end;

function TMapper.FromAbsoluteAddress(addr: Cardinal; _type: TAddressType = TAddressType.PrgRom): Int32;
begin
end;

function TMapper.FromAbsolutePpuAddress(addr: Cardinal; _type: TPpuAddressType): Int32;
begin
end;
}

procedure TMapper.GetRomFileData(var _out: TBytes; asIpsFile: Boolean; header: PByte);
begin
end;

{function TMapper.GetPrgChrCopy: TBytes;
begin
end;}

procedure TMapper.RestorePrgChrBackup(var backupData: TBytes);
begin
end;

procedure TMapper.RevertPrgChrChanges;
begin
end;

function TMapper.HasPrgChrChanges: Boolean; // TODO
begin
	Result := False;
end;

procedure TMapper.CopyPrgChrRom(var _mapper: TMapper);
begin
end;

// Battery

procedure TMapper.DoLoadBattery(BatteryKind: TBatterySaveType;
	var data: TBytes; length: Cardinal = 0);
var
	Filename: String;
	Stream: TFileStream;
begin
	Filename := GetBatteryFilename(BatteryKind);
	if FileExists(Filename) then
	begin
		Log('Loading battery: ' + Filename);
		Stream := TFileStream.Create(Filename, fmOpenRead);
		if length = 0 then
		begin
			length := Stream.Size;
			SetLength(data, length);
		end;
		try
			Stream.Read(data, length);
		finally
			Stream.Free;
		end;
	end;
end;

procedure TMapper.DoSaveBattery(BatteryKind: TBatterySaveType;
	var data: TBytes; length: Cardinal);
var
	Filename: String;
	Stream: TFileStream;
begin
	Filename := GetBatteryFilename(BatteryKind);
	Log('Saving battery: ' + Filename);
	Stream := TFileStream.Create(Filename, fmOpenWrite or fmCreate);
	try
		Stream.Write(data, length);
	finally
		Stream.Free;
	end;
end;

procedure TMapper.LoadBattery;
begin
	if (HasBattery) and (saveRamSize > 0) then
		DoLoadBattery(battNormal, saveRam, saveRamSize);
	if (hasChrBattery) and (chrRamSize > 0) then
		DoLoadBattery(battChr, chrRam, chrRamSize);
end;

procedure TMapper.SaveBattery;
begin
	if (HasBattery) and (saveRamSize > 0) then
		DoSaveBattery(battNormal, saveRam, saveRamSize);
	if (hasChrBattery) and (chrRamSize > 0) then
		DoSaveBattery(battChr, chrRam, chrRamSize);
end;

function TMapper.GetBatteryFilename(BatteryKind: TBatterySaveType = battNormal): String;
var
	Fn: String;
begin
	Fn := ExtractFileName(RomInfo.Filename);
	Result := Console.SaveStateManager.GetPath +
		ChangeFileExt(Fn, BatterySaveExtensions[BatteryKind]);
	{$IFDEF DEBUG}
//	if HasBattery then Log('Battery File=' + ExtractFileName(Result));
	{$ENDIF}
end;

function TMapper.GetSoundChipName: AnsiString;
begin
	Result := '';
end;

// Savestate

procedure TMapper.LoadSnapshot;
var
	L: Cardinal;
begin
	inherited LoadSnapshot;

	StartSection('MEMOFF');
	Stream.Read(prgMemoryOffset[0], SizeOf(prgMemoryOffset));
	Stream.Read(chrMemoryOffset[0], SizeOf(chrMemoryOffset)); // $40
	Stream.Read(prgMemoryType[0],   SizeOf(prgMemoryType));
	Stream.Read(chrMemoryType[0],   SizeOf(chrMemoryType));   // $40
	Stream.Read(prgMemoryAccess[0], SizeOf(prgMemoryAccess));
	Stream.Read(chrMemoryAccess[0], SizeOf(chrMemoryAccess)); // $40

	StartSection('CHRR');
	chrRamSize := Stream.ReadDWord;
	SetLength(chrRam, chrRamSize);
	Stream.Read(chrRam[0], chrRamSize);

	StartSection('WORK');
	workRamSize := Stream.ReadDWord;
	SetLength(workRam, workRamSize);
	Stream.Read(workRam[0], workRamSize);

	StartSection('SAVE');
	saveRamSize := Stream.ReadDWord;
	SetLength(saveRam, saveRamSize);
	Stream.Read(saveRam[0], saveRamSize);

	StartSection('NTAB');
	L := Stream.ReadDWord; //
	nametableCount := L div _NametableSize;
	SetLength(nametableRam, L);
	Stream.Read(nametableRam[0], L);

//	Stream(_mirroringType, chrRam, workRam, saveRam, nametableRam, prgMemoryOffset, chrMemoryOffset, prgMemoryType, chrMemoryType, prgMemoryAccess, chrMemoryAccess);

	RestorePrgChrState;
end;

procedure TMapper.SaveSnapshot;
begin
	inherited SaveSnapshot;

	StartSection('MEMOFF');
	Stream.WriteBuffer(prgMemoryOffset[0], SizeOf(prgMemoryOffset));
	Stream.WriteBuffer(chrMemoryOffset[0], SizeOf(chrMemoryOffset)); // $40
	Stream.WriteBuffer(prgMemoryType[0],   SizeOf(prgMemoryType));
	Stream.WriteBuffer(chrMemoryType[0],   SizeOf(chrMemoryType));   // $40
	Stream.WriteBuffer(prgMemoryAccess[0], SizeOf(prgMemoryAccess));
	Stream.WriteBuffer(chrMemoryAccess[0], SizeOf(chrMemoryAccess)); // $40

	StartSection('CHRR');
	Stream.WriteDWord(chrRamSize);
	if chrRamSize > 0 then
		Stream.WriteBuffer(chrRam[0], chrRamSize);

	StartSection('WORK');
	Stream.WriteDWord(workRamSize);
	if workRamSize > 0 then
		Stream.WriteBuffer(workRam[0], workRamSize);

	StartSection('SAVE');
	Stream.WriteDWord(saveRamSize);
	if saveRamSize > 0 then
		Stream.WriteBuffer(saveRam[0], saveRamSize);

	StartSection('NTAB');
	Stream.WriteDWord(nametableCount * _NametableSize);
	Stream.WriteBuffer(nametableRam[0], nametableCount * _NametableSize);

	//	IsReadRegisterAddr, IsWriteRegisterAddr: array [Word] of Boolean;
end;


end.

