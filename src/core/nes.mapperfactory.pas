unit NES.MapperFactory;

interface

{$MODE DELPHI}

uses
	Classes, NES.Types, NES.ROM, NES.Mapper;

	function GetMapperClass(MapperID, SubMapperID: Word; RomData: PRomData = nil): TMapperClass;

implementation

uses
	Basement.Util, NES.Config,

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
	NES.Mapper_093, NES.Mapper_094, NES.Mapper_096,
	NES.Mapper_105,
	NES.Mapper_111, NES.Mapper_112, NES.Mapper_117, NES.Mapper_118,
	NES.Mapper_140,
	NES.Mapper_150, NES.Mapper_152, NES.Mapper_156,
	NES.Mapper_168,
	NES.Mapper_177,
	NES.Mapper_180,	NES.Mapper_184,
	NES.Mapper_190, NES.Mapper_193,
	NES.Mapper_206,
	NES.Mapper_216,
	NES.Mapper_222, NES.Mapper_225,
					NES.Mapper_226, NES.Mapper_227, NES.Mapper_228, NES.Mapper_229,
	NES.Mapper_230, NES.Mapper_231, NES.Mapper_232, NES.Mapper_234, NES.Mapper_235,
	NES.Mapper_240, NES.Mapper_244, NES.Mapper_246, NES.Mapper_268, NES.Mapper_290,
	NES.Mapper_521;


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
		94:  Result := TMapper_094; 		// Nintendo
		95:  Result := TMapper_095; 		// Namcot 118 variant
		96:  Result := TMapper_096; 		// Oeka Kids
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
		141: Result := TMapper_141; 		// Sachen 8259A
		142: Result := TMapper_KS202; 		// Kaiser KS202
		143: Result := TMapper_143; 		// Sachen NROM
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
		172: Result := TMapper_172; 		// TXC Corporation TXC22211B
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
		216: Result := TMapper_216; 		// Various Russian Dendy games
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
		240: Result := TMapper_240; 		//
		241: Result := TMapper_241; 		// BxROM-like
		243: Result := TMapper_243; 		// Sachen SA-015
		244: Result := TMapper_244; 		//
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


end.

