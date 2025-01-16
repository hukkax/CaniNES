// This is basically the 6502 emulation from Mesen ported to FreePascal.

unit
	NES.CPU;

{$MODE DELPHI}
{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}

{$I canines.inc}

interface

uses
	SysUtils, Classes,
	NES.Types, NES.SaveState;

const
	psfCarry     = $01;
	psfZero      = $02;
	psfInterrupt = $04;
	psfDecimal   = $08;
	psfBreak     = $10;
	psfReserved  = $20;
	psfOverflow  = $40;
	psfNegative  = $80;

	TFuncMnemonic: array [0..91] of AnsiString = (
		'AND','EOR','ORA',
		'ADD','ADC','SBC',
		'CMP','CPA','CPX','CPY',
		'INC','DEC',
		'ASL','LSR','ROL','ROR','ASLAddr','LSRAddr','ROLAddr','RORAddr',
		'JMP','BIT',
		// Legal Opcodes
		'LDA','LDX','LDY','STA','STX','STY',
		'TAX','TAY','TSX','TXA','TXS','TYA',
		'PHA','PHP','PLA','PLP',
		'INX','INY','DEX','DEY',
		'ASL_Acc','ASL_Mem','LSR_Acc','LSR_Mem','ROL_Acc','ROL_Mem','ROR_Acc','ROR_Mem',
		'JMP_Abs','JMP_Ind','JSR','RTS',
		'BCC','BCS','BEQ','BMI','BNE','BPL','BVC','BVS',
		'CLC','CLD','CLI','CLV','SEC','SED','SEI',
		'BRK','RTI',
		'NOP',
		// Invalid opcodes
		'SLO','SRE','RLA','RRA','SAX','LAX','DCP','ISB','AAC','ASR','ARR','ATX','AXS','SYA','SXA',
		// Unimplemented/incorrect opcodes
		'HLT','UNK','AXA','TAS','LAS'
	);

type
	TFunc = procedure of Object;

	TIRQSource = ( irqExternal = 1, irqFrameCounter = 2, irqDMC = 4, irqFDSDisk = 8 );

	TAddrMode = (                 amNone,
		amAcc,  amImp,   amImm,   amRel,
		amZero, amAbs,   amZeroX, amZeroY,
		amInd,  amIndX,  amIndY,  amIndYW,
		amAbsX, amAbsXW, amAbsY,  amAbsYW,
		amOther
	);

type
	TFuncType = (
		_AND,_EOR,_ORA,
		_ADD,_ADC,_SBC,
		_CMP,_CPA,_CPX,_CPY,
		_INC,_DEC,
		_ASL,_LSR,_ROL,_ROR,_ASLAddr,_LSRAddr,_ROLAddr,_RORAddr,
		_JMP,_BIT,
		// Legal Opcodes
		_LDA,_LDX,_LDY,_STA,_STX,_STY,
		_TAX,_TAY,_TSX,_TXA,_TXS,_TYA,
		_PHA,_PHP,_PLA,_PLP,
		_INX,_INY,_DEX,_DEY,
		_ASL_Acc,_ASL_Mem,_LSR_Acc,_LSR_Mem,_ROL_Acc,_ROL_Mem,_ROR_Acc,_ROR_Mem,
		_JMP_Abs,_JMP_Ind,_JSR,_RTS,
		_BCC,_BCS,_BEQ,_BMI,_BNE,_BPL,_BVC,_BVS,
		_CLC,_CLD,_CLI,_CLV,_SEC,_SED,_SEI,
		_BRK,_RTI,
		_NOP,
		// Invalid opcodes
//		_SLO,_SRE,_RLA,_RRA,_SAX,_LAX,_DCP,_ISB,_AAC,_ASR,_ARR,_ATX,_AXS,_SYA,_SXA,
		_SLO,_SRE,_RLA,_RRA,_SAX,_LAX,_DCP,_ISB,_AAC,_ASR,_ARR,_ATX,_AXS,_SHY,_SHX,
		_SHAA,_SHAZ,
		// Unimplemented/incorrect opcodes
//		_HLT,_UNK,_AXA,_TAS,_LAS
		_HLT,_UNK,_TAS,_LAS
	);

	TCPU = class(TSnapshotable)
	const
		NMIVector   = $FFFA;
		ResetVector = $FFFC;
		IRQVector   = $FFFE;

		ClockRateNTSC  = 1789773;
		ClockRatePAL   = 1662607;
		ClockRateDendy = 1773448;

		_opTable: array [Byte] of TFuncType = (
		//  0     1     2     3     4     5     6         7     8     9     A        B     C        D     E         F
			_BRK, _ORA, _HLT, _SLO, _NOP, _ORA, _ASL_Mem, _SLO, _PHP, _ORA, _ASL_Acc,_AAC, _NOP,    _ORA, _ASL_Mem, _SLO, //0
			_BPL, _ORA, _HLT, _SLO, _NOP, _ORA, _ASL_Mem, _SLO, _CLC, _ORA, _NOP,    _SLO, _NOP,    _ORA, _ASL_Mem, _SLO, //1
			_JSR, _AND, _HLT, _RLA, _BIT, _AND, _ROL_Mem, _RLA, _PLP, _AND, _ROL_Acc,_AAC, _BIT,    _AND, _ROL_Mem, _RLA, //2
			_BMI, _AND, _HLT, _RLA, _NOP, _AND, _ROL_Mem, _RLA, _SEC, _AND, _NOP,    _RLA, _NOP,    _AND, _ROL_Mem, _RLA, //3
			_RTI, _EOR, _HLT, _SRE, _NOP, _EOR, _LSR_Mem, _SRE, _PHA, _EOR, _LSR_Acc,_ASR, _JMP_Abs,_EOR, _LSR_Mem, _SRE, //4
			_BVC, _EOR, _HLT, _SRE, _NOP, _EOR, _LSR_Mem, _SRE, _CLI, _EOR, _NOP,    _SRE, _NOP,    _EOR, _LSR_Mem, _SRE, //5
			_RTS, _ADC, _HLT, _RRA, _NOP, _ADC, _ROR_Mem, _RRA, _PLA, _ADC, _ROR_Acc,_ARR, _JMP_Ind,_ADC, _ROR_Mem, _RRA, //6
			_BVS, _ADC, _HLT, _RRA, _NOP, _ADC, _ROR_Mem, _RRA, _SEI, _ADC, _NOP,    _RRA, _NOP,    _ADC, _ROR_Mem, _RRA, //7
			_NOP, _STA, _NOP, _SAX, _STY, _STA, _STX,     _SAX, _DEY, _NOP, _TXA,    _UNK, _STY,    _STA, _STX,	    _SAX, //8
			_BCC, _STA, _HLT, _SHAZ,_STY, _STA, _STX,     _SAX, _TYA, _STA, _TXS,    _TAS, _SHY,    _STA, _SHX,	    _SHAA,//9
			_LDY, _LDA, _LDX, _LAX, _LDY, _LDA, _LDX,     _LAX, _TAY, _LDA, _TAX,    _ATX, _LDY,    _LDA, _LDX,	    _LAX, //A
			_BCS, _LDA, _HLT, _LAX, _LDY, _LDA, _LDX,     _LAX, _CLV, _LDA, _TSX,    _LAS, _LDY,    _LDA, _LDX,	    _LAX, //B
			_CPY, _CPA, _NOP, _DCP, _CPY, _CPA, _DEC,     _DCP, _INY, _CPA, _DEX,    _AXS, _CPY,    _CPA, _DEC,	    _DCP, //C
			_BNE, _CPA, _HLT, _DCP, _NOP, _CPA, _DEC,     _DCP, _CLD, _CPA, _NOP,    _DCP, _NOP,    _CPA, _DEC,	    _DCP, //D
			_CPX, _SBC, _NOP, _ISB, _CPX, _SBC, _INC,     _ISB, _INX, _SBC, _NOP,    _SBC, _CPX,    _SBC, _INC,	    _ISB, //E
			_BEQ, _SBC, _HLT, _ISB, _NOP, _SBC, _INC,     _ISB, _SED, _SBC, _NOP,    _ISB, _NOP,    _SBC, _INC,	    _ISB  //F
		);

		AddrMode: array [Byte] of TAddrMode = (
		//  0      1       2       3        4       5       6       7       8      9       A      B       C       D       E       F
			amImp, amIndX, amNone, amIndX,  amZero, amZero, amZero, amZero, amImp, amImm,  amAcc, amImm,  amAbs,  amAbs,  amAbs,  amAbs,   // 0
			amRel, amIndY, amNone, amIndYW, amZeroX,amZeroX,amZeroX,amZeroX,amImp, amAbsY, amImp, amAbsYW,amAbsX, amAbsX, amAbsXW,amAbsXW, // 1
			amAbs, amIndX, amNone, amIndX,  amZero, amZero, amZero, amZero, amImp, amImm,  amAcc, amImm,  amAbs,  amAbs,  amAbs,  amAbs,   // 2
			amRel, amIndY, amNone, amIndYW, amZeroX,amZeroX,amZeroX,amZeroX,amImp, amAbsY, amImp, amAbsYW,amAbsX, amAbsX, amAbsXW,amAbsXW, // 3
			amImp, amIndX, amNone, amIndX,  amZero, amZero, amZero, amZero, amImp, amImm,  amAcc, amImm,  amAbs,  amAbs,  amAbs,  amAbs,   // 4
			amRel, amIndY, amNone, amIndYW, amZeroX,amZeroX,amZeroX,amZeroX,amImp, amAbsY, amImp, amAbsYW,amAbsX, amAbsX, amAbsXW,amAbsXW, // 5
			amImp, amIndX, amNone, amIndX,  amZero, amZero, amZero, amZero, amImp, amImm,  amAcc, amImm,  amInd,  amAbs,  amAbs,  amAbs,   // 6
			amRel, amIndY, amNone, amIndYW, amZeroX,amZeroX,amZeroX,amZeroX,amImp, amAbsY, amImp, amAbsYW,amAbsX, amAbsX, amAbsXW,amAbsXW, // 7
			amImm, amIndX, amImm,  amIndX,  amZero, amZero, amZero, amZero, amImp, amImm,  amImp, amImm,  amAbs,  amAbs,  amAbs,  amAbs,   // 8
			amRel, amIndYW,amNone, amOther, amZeroX,amZeroX,amZeroY,amZeroY,amImp, amAbsYW,amImp, amOther,amOther,amAbsXW,amOther,amOther, // 9
			amImm, amIndX, amImm,  amIndX,  amZero, amZero, amZero, amZero, amImp, amImm,  amImp, amImm,  amAbs,  amAbs,  amAbs,  amAbs,   // A
			amRel, amIndY, amNone, amIndY,  amZeroX,amZeroX,amZeroY,amZeroY,amImp, amAbsY, amImp, amAbsY, amAbsX, amAbsX, amAbsY, amAbsY,  // B
			amImm, amIndX, amImm,  amIndX,  amZero, amZero, amZero, amZero, amImp, amImm,  amImp, amImm,  amAbs,  amAbs,  amAbs,  amAbs,   // C
			amRel, amIndY, amNone, amIndYW, amZeroX,amZeroX,amZeroX,amZeroX,amImp, amAbsY, amImp, amAbsYW,amAbsX, amAbsX, amAbsXW,amAbsXW, // D
			amImm, amIndX, amImm,  amIndX,  amZero, amZero, amZero, amZero, amImp, amImm,  amImp, amImm,  amAbs,  amAbs,  amAbs,  amAbs,   // E
			amRel, amIndY, amNone, amIndYW, amZeroX,amZeroX,amZeroX,amZeroX,amImp, amAbsY, amImp, amAbsYW,amAbsX, amAbsX, amAbsXW,amAbsXW  // F
		);

	private
		OpTable:           array [Byte] of TFunc;
		Operand:           Word;
		InstAddrMode:      TAddrMode;

		spriteDmaOffset:   Byte;
		SpriteDmaTransfer: Boolean;

		dmcDmaRunning:  Boolean;
		abortDmcDma:    Boolean;
		needDummyRead:  Boolean;
		cpuWrite:       Boolean;
		WriteAddr:      Word;

		ppuOffset,
		startClockCount,
		endClockCount:  Byte;

		NeedHalt,
		NeedNMI,
		PrevRunIRQ,
		PrevRunNMI,
		prevNeedNMI,
		prevNmiFlag,
		RunIrq:         Boolean;

	public
		A, X, Y: Byte;
		SP, PS:  Byte;
		PC:      Word;

		CycleCount,
		masterClock: QWord;

		IRQFlag: Byte;
		NMIFlag: Boolean;
		DisableOCNextFrame: Boolean;

		Mnemonics: array[Byte] of AnsiString;

		function  GetA: Byte;         inline;
		procedure SetA(value: Byte);  inline;
		function  GetX: Byte;         inline;
		procedure SetX(value: Byte);  inline;
		function  GetY: Byte;         inline;
		procedure SetY(value: Byte);  inline;
		function  GetSP: Byte;        inline;
		procedure SetSP(value: Byte); inline;
		function  GetPS: Byte;        inline;
		procedure SetPS(value: Byte); inline;
		function  GetPC: Word;        inline;
		procedure SetPC(value: Word); inline;

		function GetIndAddr:   Word; inline;
		function GetImmediate: Byte; inline;
		function GetZeroAddr:  Byte; inline;
		function GetZeroXAddr: Byte; inline;
		function GetZeroYAddr: Byte; inline;
		function GetAbsAddr:   Word; inline;
		function GetAbsXAddr(wantDummyRead: Boolean = True): Word; inline;
		function GetAbsYAddr(wantDummyRead: Boolean = True): Word; inline;
		function GetInd:       Word; inline;
		function GetIndXAddr:  Word; inline;
		function GetIndYAddr(wantDummyRead: Boolean = True): Word; inline;

		// ========================================================================
		// Operations
		// ========================================================================

		procedure OpAND; inline;
		procedure OpEOR; inline;
		procedure OpORA; inline;
		procedure OpADD(value: Byte);
		procedure OpADC; inline;
		procedure OpSBC; inline;
		procedure OpCMP(reg, value: Byte);
		procedure OpCPA; inline;
		procedure OpCPX; inline;
		procedure OpCPY; inline;
		procedure OpINC;
		procedure OpDEC;
		function  OpASL(value: Byte): Byte;
		function  OpLSR(value: Byte): Byte;
		function  OpROL(value: Byte): Byte;
		function  OpROR(value: Byte): Byte;
		procedure OpASLAddr;
		procedure OpLSRAddr;
		procedure OpROLAddr;
		procedure OpRORAddr;
		procedure OpJMP(addr: Word);
		procedure OpBIT;
		procedure BranchRelative(branch: Boolean);

		// ========================================================================
		// Legal Opcodes
		// ========================================================================

		procedure OpLDA; inline; procedure OpLDX; inline; procedure OpLDY; inline;
		procedure OpSTA; inline; procedure OpSTX; inline; procedure OpSTY; inline;

		procedure OpTAX; inline; procedure OpTAY; inline;
		procedure OpTSX; inline; procedure OpTXA; inline;
		procedure OpTXS; inline; procedure OpTYA; inline;

		procedure OpPHA; inline; procedure OpPHP; inline;
		procedure OpPLA; inline; procedure OpPLP; inline;

		procedure OpINX; inline; procedure OpINY; inline;
		procedure OpDEX; inline; procedure OpDEY; inline;

		procedure OpASL_Acc; inline; procedure OpASL_Mem; inline;
		procedure OpLSR_Acc; inline; procedure OpLSR_Mem; inline;

		procedure OpROL_Acc; inline; procedure OpROL_Mem; inline;
		procedure OpROR_Acc; inline; procedure OpROR_Mem; inline;

		procedure OpJMP_Abs; inline; procedure OpJMP_Ind; inline;
		procedure OpJSR;     inline; procedure OpRTS; inline;

		procedure OpBCC; inline; procedure OpBCS; inline;
		procedure OpBEQ; inline; procedure OpBMI; inline;
		procedure OpBNE; inline; procedure OpBPL; inline;
		procedure OpBVC; inline; procedure OpBVS; inline;

		procedure OpCLC; inline; procedure OpCLD; inline;
		procedure OpCLI; inline; procedure OpCLV; inline;
		procedure OpSEC; inline; procedure OpSED; inline;
		procedure OpSEI; inline; procedure OpBRK;
		procedure OpRTI;         procedure OpNOP; inline;

		// Invalid opcodes
		//
		procedure OpSLO; procedure OpSRE; procedure OpRLA; procedure OpRRA; procedure OpSAX;
		procedure OpLAX; procedure OpDCP; procedure OpISB; procedure OpAAC; procedure OpASR;
		procedure OpARR; procedure OpATX; procedure OpAXS; { procedure OpSYA; procedure OpSXA; }
		procedure OpSHX; procedure OpSHY; procedure OpSHAA; procedure OpSHAZ;

		// Unimplemented/incorrect opcodes
		//
		procedure OpHLT; procedure OpUNK; {procedure OpAXA} procedure OpTAS; procedure OpLAS;

		procedure SyaSxaAxa(baseAddr: Word; indexReg, valueReg: Byte);

		// --------------------------------------------------------------------------------------------

		function  GetClockRate(model: TNESModel): Cardinal;

		procedure ClearFlags(flags: Byte);
		procedure SetFlags(flags: Byte);
		function  CheckFlag(flag: Byte): Boolean;
		procedure SetZeroNegativeFlags(value: Byte);
		function  CheckPageCrossedS(valA: Word; valB: Int8): Boolean; inline;
		function  CheckPageCrossedU(valA: Word; valB: Byte): Boolean; inline;

		function  GetCycleCount: QWord; inline;
		procedure SetNMIFlag;   inline;
		procedure ClearNMIFlag; inline;
		procedure SetIRQSource(source: TIRQSource);          inline;
		function  HasIRQSource(source: TIRQSource): Boolean; inline;
		procedure ClearIRQSource(source: TIRQSource);        inline;
		procedure SetNextFrameOverclockStatus(disabled: Boolean); inline;
		function  IsCPUWrite: Boolean; inline;

		//procedure IncCycleCount;
		function  FetchOperand: Word; inline;
		function  GetOperandValue: Byte; inline;

		//procedure ProcessCPUClock;
		procedure StartCpuCycle(forRead: Boolean); inline;
		procedure EndCpuCycle(forRead: Boolean); inline;
		function  ProcessDMARead(addr: Word; var prevReadAddress: Word; enableInternalRegReads, isNesBehavior: Boolean): Byte;
		procedure ProcessPendingDMA(readAddress: Word);

		procedure DummyRead; inline;
		function  ReadByte: Byte; inline;
		function  ReadWord: Word; inline;

		procedure MemoryWrite(addr: Word; value: Byte; operationType: TMemoryOperationType = memopWrite); inline;
		function  MemoryRead(addr: Word; operationType: TMemoryOperationType = memopRead): Byte; inline;
		function  MemoryReadWord(addr: Word; operationType: TMemoryOperationType = memopRead): Word; inline;
		procedure SetRegister(var reg: Byte; value: Byte); inline;

		procedure Push(value: Byte); inline;
		procedure PushWord(value: Word); inline;
		function  Pop: Byte; inline;
		function  PopWord: Word; inline;

		procedure Reset(softReset: Boolean; model: TNesModel);

		procedure IRQ; inline;

		procedure RunDMATransfer(offsetValue: Byte);
		procedure StartDMCTransfer;
		procedure StopDMCTransfer;

		procedure Exec; inline;
		procedure ExecFrame;

		procedure DumpRAM(const Filename: String);

		function  MemoryManager_Peek(address: Word): Byte; inline;

		constructor Create;
		destructor  Destroy; override;
	end;


	function  NTH_BIT(const aValue: Cardinal; const Bit: Byte): Byte; inline;
	function  Flipbyte(b: Byte): Byte;

	function  CPUGetCycleCount: QWord;
	procedure CPUSetNMIFlag;
	procedure CPUClearNMIFlag;
	function  CPUHasIRQSource(source: TIRQSource): Boolean;
	procedure CPUSetIRQSource(source: TIRQSource);
	procedure CPUClearIRQSource(source: TIRQSource);
	procedure CPUSetNextFrameOverclockStatus(disabled: Boolean);

//==================================================================================================

implementation

uses
	Basement.Util, Math, TextOutput,
	NES.Config, NES.MemoryManager, NES.Console, NES.PPU;

	{$INCLUDE coredefs.inc}

{$IFDEF DEBUGGER}
const
	Mnemonic: array[Byte] of AnsiString = (
		'BRK','ORA','???','SLO','NOP','ORA','ASL','SLO','PHP','ORA','ASL','???','NOP','ORA','ASL','SLO',
		'BPL','ORA','???','SLO','NOP','ORA','ASL','SLO','CLC','ORA','NOP','SLO','NOP','ORA','ASL','SLO',
		'JSR','AND','???','RLA','BIT','AND','ROL','RLA','PLP','AND','ROL','???','BIT','AND','ROL','RLA',
		'BMI','AND','???','RLA','NOP','AND','ROL','RLA','SEC','AND','NOP','RLA','NOP','AND','ROL','RLA',
		'RTI','EOR','???','SRE','NOP','EOR','LSR','SRE','PHA','EOR','LSR','???','JMP','EOR','LSR','SRE',
		'BVC','EOR','???','SRE','NOP','EOR','LSR','SRE','CLI','EOR','NOP','SRE','NOP','EOR','LSR','SRE',
		'RTS','ADC','???','RRA','NOP','ADC','ROR','RRA','PLA','ADC','ROR','???','JMP','ADC','ROR','RRA',
		'BVS','ADC','???','RRA','NOP','ADC','ROR','RRA','SEI','ADC','NOP','RRA','NOP','ADC','ROR','RRA',
		'NOP','STA','???','SAX','STY','STA','STX','SAX','DEY','BIT','TXA','???','STY','STA','STX','SAX',
		'BCC','STA','???','???','STY','STA','STX','SAX','TYA','STA','TXS','???','???','STA','???','???',
		'LDY','LDA','LDX','LAX','LDY','LDA','LDX','LAX','TAY','LDA','TAX','???','LDY','LDA','LDX','LAX',
		'BCS','LDA','???','LAX','LDY','LDA','LDX','LAX','CLV','LDA','TSX','???','LDY','LDA','LDX','LAX',
		'CPY','CMP','???','DCP','CPY','CMP','DEC','DCP','INY','CMP','DEX','???','CPY','CMP','DEC','DCP',
		'BNE','CMP','???','DCP','NOP','CMP','DEC','DCP','CLD','CMP','NOP','DCP','NOP','CMP','DEC','DCP',
		'CPX','SBC','???','ISB','CPX','SBC','INC','ISB','INX','SBC','NOP','SBC','CPX','SBC','INC','ISB',
		'BEQ','SBC','???','ISB','NOP','SBC','INC','ISB','SED','SBC','NOP','ISB','NOP','SBC','INC','ISB'
	);
{$ENDIF}

// ================================================================================================
// Utility
// ================================================================================================

function NTH_BIT(const aValue: Cardinal; const Bit: Byte): Byte;
begin
	Result := (aValue shr Bit) and 1;
end;

function Flipbyte(b: Byte): Byte;
begin
	b := Byte((b and $F0) shr 4) or (b and $0F) shl 4;
	b := Byte((b and $CC) shr 2) or (b and $33) shl 2;
	b := Byte((b and $AA) shr 1) or (b and $55) shl 1;
	Result := b;
end;

// ================================================================================================
// TCPU
// ================================================================================================

constructor TCPU.Create;
var
	i: Int32;
begin
	inherited Create('CPU');

	// TODO: include extraScanlinesBeforeNmi, extraScanlinesAfterNmi in snapshot

	RegisterProperty(16, @PC);
	RegisterProperty(8,  @SP);
	RegisterProperty(8,  @PS);
	RegisterProperty(8,  @A);
	RegisterProperty(8,  @X);
	RegisterProperty(8,  @Y);
	RegisterProperty(64, @CycleCount);
	RegisterProperty(1,  @NMIFlag);
	RegisterProperty(8,  @IRQFlag);
	RegisterProperty(1,  @dmcDmaRunning);
	RegisterProperty(1,  @abortDmcDma);
	RegisterProperty(1,  @SpriteDmaTransfer);
	RegisterProperty(1,  @needDummyRead);
	RegisterProperty(1,  @needHalt);
	RegisterProperty(8,  @startClockCount);
	RegisterProperty(8,  @endClockCount);
	RegisterProperty(8,  @ppuOffset);
	RegisterProperty(64, @masterClock);
	RegisterProperty(1,  @prevNeedNmi);
	RegisterProperty(1,  @prevNmiFlag);
	RegisterProperty(1,  @needNmi);

	{RegisterProperty(16, @Operand);
	RegisterProperty(SizeOf(TAddrMode)*8, @InstAddrMode);
	RegisterProperty(8,  @spriteDmaOffset);
	RegisterProperty(1,  @cpuWrite);
	RegisterProperty(16, @WriteAddr);
	RegisterProperty(1,  @PrevRunIRQ);
	RegisterProperty(1,  @PrevRunNMI);
	RegisterProperty(1,  @RunIrq);}

	InstAddrMode := amNone;
	CycleCount := 0;
	Operand := 0;
	SpriteDmaTransfer := False;
	SpriteDmaOffset := 0;
	NeedHalt := False;
	ppuOffset := 0;
	startClockCount := 6;
	endClockCount := 6;
	MasterClock := 0;
	dmcDmaRunning := False;
	abortDmcDma := False;
	cpuWrite := False;
	//irqMask := 0;
	WriteAddr := 0;
	PrevRunNMI := False;
	PrevRunIRQ := False;
	RunIrq := False;

	for i := 0 to 255 do
	begin
		case _opTable[i] of

		_AND: OpTable[i] := OpAND;
		_EOR: OpTable[i] := OpEOR;
		_ORA: OpTable[i] := OpORA;

		//_ADD: OpTable[i] := OpADD;
		_ADC: OpTable[i] := OpADC;
		_SBC: OpTable[i] := OpSBC;

		//_CMP: OpTable[i] := OpCMP;
		_CPA: OpTable[i] := OpCPA;
		_CPX: OpTable[i] := OpCPX;
		_CPY: OpTable[i] := OpCPY;

		_INC: OpTable[i] := OpINC;
		_DEC: OpTable[i] := OpDEC;

		//_ASL: OpTable[i] := OpASL;
		//_LSR: OpTable[i] := OpLSR;
		//_ROL: OpTable[i] := OpROL;
		//_ROR: OpTable[i] := OpROR;

		_ASLAddr: OpTable[i] := OpASLAddr;
		_LSRAddr: OpTable[i] := OpLSRAddr;
		_ROLAddr: OpTable[i] := OpROLAddr;
		_RORAddr: OpTable[i] := OpRORAddr;

		//_JMP: OpTable[i] := OpJMP;
		_BIT: OpTable[i] := OpBIT;

		// Legal Opcodes

		_LDA: OpTable[i] := OpLDA;
		_LDX: OpTable[i] := OpLDX;
		_LDY: OpTable[i] := OpLDY;
		_STA: OpTable[i] := OpSTA;
		_STX: OpTable[i] := OpSTX;
		_STY: OpTable[i] := OpSTY;

		_TAX: OpTable[i] := OpTAX;
		_TAY: OpTable[i] := OpTAY;
		_TSX: OpTable[i] := OpTSX;
		_TXA: OpTable[i] := OpTXA;
		_TXS: OpTable[i] := OpTXS;
		_TYA: OpTable[i] := OpTYA;

		_PHA: OpTable[i] := OpPHA;
		_PHP: OpTable[i] := OpPHP;
		_PLA: OpTable[i] := OpPLA;
		_PLP: OpTable[i] := OpPLP;

		_INX: OpTable[i] := OpINX;
		_INY: OpTable[i] := OpINY;
		_DEX: OpTable[i] := OpDEX;
		_DEY: OpTable[i] := OpDEY;

		_ASL_Acc: OpTable[i] := OpASL_Acc;
		_ASL_Mem: OpTable[i] := OpASL_Mem;
		_LSR_Acc: OpTable[i] := OpLSR_Acc;
		_LSR_Mem: OpTable[i] := OpLSR_Mem;
		_ROL_Acc: OpTable[i] := OpROL_Acc;
		_ROL_Mem: OpTable[i] := OpROL_Mem;
		_ROR_Acc: OpTable[i] := OpROR_Acc;
		_ROR_Mem: OpTable[i] := OpROR_Mem;

		_JMP_Abs: OpTable[i] := OpJMP_Abs;
		_JMP_Ind: OpTable[i] := OpJMP_Ind;
		_JSR:     OpTable[i] := OpJSR;
		_RTS:     OpTable[i] := OpRTS;

		_BCC: OpTable[i] := OpBCC;
		_BCS: OpTable[i] := OpBCS;
		_BEQ: OpTable[i] := OpBEQ;
		_BMI: OpTable[i] := OpBMI;
		_BNE: OpTable[i] := OpBNE;
		_BPL: OpTable[i] := OpBPL;
		_BVC: OpTable[i] := OpBVC;
		_BVS: OpTable[i] := OpBVS;

		_CLC: OpTable[i] := OpCLC;
		_CLD: OpTable[i] := OpCLD;
		_CLI: OpTable[i] := OpCLI;
		_CLV: OpTable[i] := OpCLV;
		_SEC: OpTable[i] := OpSEC;
		_SED: OpTable[i] := OpSED;
		_SEI: OpTable[i] := OpSEI;

		_BRK: OpTable[i] := OpBRK;
		_RTI: OpTable[i] := OpRTI;
		_NOP: OpTable[i] := OpNOP;

		// Invalid opcodes

		_SLO: OpTable[i] := OpSLO;
		_SRE: OpTable[i] := OpSRE;
		_RLA: OpTable[i] := OpRLA;
		_RRA: OpTable[i] := OpRRA;
		_SAX: OpTable[i] := OpSAX;
		_LAX: OpTable[i] := OpLAX;
		_DCP: OpTable[i] := OpDCP;
		_ISB: OpTable[i] := OpISB;
		_AAC: OpTable[i] := OpAAC;
		_ASR: OpTable[i] := OpASR;
		_ARR: OpTable[i] := OpARR;
		_ATX: OpTable[i] := OpATX;
		_AXS: OpTable[i] := OpAXS;
		//_SYA: OpTable[i] := OpSYA;
		//_SXA: OpTable[i] := OpSXA;

		_SHX: OpTable[i] := OpSHX;
		_SHY: OpTable[i] := OpSHY;
		_SHAA:OpTable[i] := OpSHAA;
		_SHAZ:OpTable[i] := OpSHAZ;

		// Unimplemented/incorrect opcodes

		_HLT: OpTable[i] := OpHLT;
		_UNK: OpTable[i] := OpUNK;
//		_AXA: OpTable[i] := OpAXA;
		_TAS: OpTable[i] := OpTAS;
		_LAS: OpTable[i] := OpLAS;

		{$IFDEF DEBUG}
		else Log('What: %d', [Byte(_Optable[i])]);
		{$ENDIF}

		end;
	end;
end;

destructor TCPU.Destroy;
begin
	inherited;
end;

//==================================================================================================

procedure TCPU.ClearFlags(flags: Byte);         begin PS := PS and (not flags); end;
procedure TCPU.SetFlags  (flags: Byte);         begin PS := PS or flags;        end;
function  TCPU.CheckFlag (flag: Byte): Boolean; begin Result := (PS and flag) = flag; end;

procedure TCPU.StartCpuCycle(forRead: Boolean);
begin
	{$R-}Inc(masterClock, IfThen(forRead, startClockCount-1, startClockCount+1));

	{$R-}Inc(CycleCount);

	NES_PPU.Run(masterClock - ppuOffset);

	Console.ProcessCPUClock;
end;

procedure TCPU.EndCpuCycle(forRead: Boolean);
begin
	Inc(masterClock, IfThen(forRead, endClockCount+1, endClockCount-1));

	NES_PPU.Run(masterClock - ppuOffset);

	// "The internal signal goes high during φ1 of the cycle that follows the one
	// where the edge is detected, and stays high until the NMI has been handled. "
	prevNeedNmi := NeedNmi;

	// "This edge detector polls the status of the NMI line during φ2 of each CPU cycle (i.e., during the
	// second half of each cycle) and raises an internal signal if the input goes from being high during
	// one cycle to being low during the next"
	if (not prevNmiFlag) and (NMIFlag) then
		NeedNmi := True;

	prevNmiFlag := NMIFlag;

	// "it's really the status of the interrupt lines at the end of the second-to-last cycle that matters."
	// Keep the irq lines values from the previous cycle.  The before-to-last cycle's values will be used
	prevRunIrq := runIrq;
	RunIrq := (IRQFlag {and irqMask} > 0) and (not CheckFlag(psfInterrupt));
end;

procedure TCPU.MemoryWrite(addr: Word; value: Byte; operationType: TMemoryOperationType);
begin
	cpuWrite := True;
	StartCpuCycle(False);
	MemoryManager.Write(addr, value, operationType);
	EndCpuCycle(False);
	cpuWrite := False;
end;

function TCPU.MemoryRead(addr: Word; operationType: TMemoryOperationType): Byte;
begin
	ProcessPendingDMA(addr);
	StartCpuCycle(True);
	Result := MemoryManager.Read(addr, operationType);
	EndCpuCycle(True);
end;

function TCPU.MemoryReadWord(addr: Word; operationType: TMemoryOperationType): Word;
var
	lo, hi: Byte;
begin
	lo := MemoryRead(addr, operationType);
	hi := MemoryRead(addr + 1, operationType);
	Result := Word(hi << 8) or lo;
end;

procedure TCPU.SetRegister(var reg: Byte; value: Byte);
begin
	ClearFlags(psfZero or psfNegative);
	SetZeroNegativeFlags(value);
	reg := value;
end;

function TCPU.GetOperandValue: Byte;
begin
	if InstAddrMode >= amZero then
		Result := MemoryRead(Operand)
	else
		Result := Operand;
end;

//==================================================================================================

function  TCPU.GetA: Byte;         begin Result := A;           end;
procedure TCPU.SetA(value: Byte);  begin SetRegister(A, value); end;

function  TCPU.GetX: Byte;         begin Result := X;           end;
procedure TCPU.SetX(value: Byte);  begin SetRegister(X, value); end;

function  TCPU.GetY: Byte;         begin Result := Y;           end;
procedure TCPU.SetY(value: Byte);  begin SetRegister(Y, value); end;

function  TCPU.GetSP: Byte;        begin Result := SP; end;
procedure TCPU.SetSP(value: Byte); begin SP := value;  end;

function  TCPU.GetPS: Byte;        begin Result := PS;        end;
procedure TCPU.SetPS(value: Byte); begin PS := value and $CF; end;

function  TCPU.GetPC: Word;        begin Result := PC; end;
procedure TCPU.SetPC(value: Word); begin PC := value;  end;

procedure TCPU.DummyRead;        begin MemoryRead(PC, memopDummyRead); end;
function  TCPU.ReadByte:  Byte;  begin Result := MemoryRead(PC, memopExecOperand);     {$R-}Inc(PC);    end;
function  TCPU.ReadWord:  Word;  begin Result := MemoryReadWord(PC, memopExecOperand); {$R-}Inc(PC, 2); end;

procedure TCPU.Push(value: Byte);
begin
	MemoryWrite(Word(SP) + $100, value);
	Dec(SP);
end;

procedure TCPU.PushWord(value: Word);
begin
	MemoryWrite(Word(SP) + $100, value >> 8);
	Dec(SP);
	MemoryWrite(Word(SP) + $100, value and $FF);
	Dec(SP);
end;

function TCPU.Pop: Byte;
begin
	{$R-}Inc(SP);
	Result := MemoryRead(Word($100) + SP, memopRead);
end;

function TCPU.PopWord: Word;
var
	lo, hi: Byte;
begin
	lo := Pop;
	hi := Pop;
	Result := Word(hi << 8) or lo;
end;

function TCPU.CheckPageCrossedS(valA: Word; valB: Int8): Boolean;
begin
	Result := ((valA + valB) and $FF00) <> (valA and $FF00);
end;

function TCPU.CheckPageCrossedU(valA: Word; valB: Byte): Boolean;
begin
	Result := ((valA + valB) and $FF00) <> (valA and $FF00);
end;

function TCPU.GetClockRate(model: TNESModel): Cardinal;
begin
	case model of
		nesPAL:   Result := ClockRatePAL;
		nesDendy: Result := ClockRateDendy;
		else      Result := ClockRateNTSC;
	end;
end;

function  TCPU.GetCycleCount: QWord;                      begin Result := CycleCount; end;
procedure TCPU.SetNMIFlag;                                begin NMIFlag := True;  end;
procedure TCPU.ClearNMIFlag;                              begin NMIFlag := False; end;
function  TCPU.HasIRQSource(source: TIRQSource): Boolean; begin Result := (IRQFlag and Byte(source)) <> 0; end;
procedure TCPU.SetIRQSource(source: TIRQSource);          begin IRQFlag := IRQFlag or  Byte(source);       end;
procedure TCPU.ClearIRQSource(source: TIRQSource);        begin IRQFlag := IRQFlag and (not Byte(source)); end;
function  TCPU.IsCPUWrite: Boolean;                       begin Result := cpuWrite; end;
procedure TCPU.SetNextFrameOverclockStatus(disabled: Boolean); begin DisableOCNextFrame := disabled; end;

// hack for inlining
//
function  CPUGetCycleCount: QWord;                      begin Result := NES_CPU.CycleCount; end;
procedure CPUSetNMIFlag;                                begin NES_CPU.NMIFlag := True;  end;
procedure CPUClearNMIFlag;                              begin NES_CPU.NMIFlag := False; end;
function  CPUHasIRQSource(source: TIRQSource): Boolean; begin Result := (NES_CPU.IRQFlag and Byte(source)) <> 0; end;
procedure CPUSetIRQSource(source: TIRQSource);          begin NES_CPU.IRQFlag := NES_CPU.IRQFlag or  Byte(source);       end;
procedure CPUClearIRQSource(source: TIRQSource);        begin NES_CPU.IRQFlag := NES_CPU.IRQFlag and (not Byte(source)); end;
procedure CPUSetNextFrameOverclockStatus(disabled: Boolean); begin NES_CPU.DisableOCNextFrame := disabled; end;

// =================================================================================================

procedure TCPU.Reset(softReset: Boolean; model: TNesModel);
var
	i: Int32;
	ppuDivider, cpuDivider, cpuOffset: Byte;
begin
	if softReset then
		LogVerbose('NES Soft Reset')
	else
		LogVerbose('NES Hard Reset');

	NMIFlag := False;
	IRQFlag := 0;

	SpriteDmaTransfer := False;
	SpriteDmaOffset := 0;
	NeedHalt := False;
	dmcDmaRunning := False;
	cpuWrite := False;

	// Use MemoryManager.Read directly to prevent clocking the PPU/APU when setting PC at reset
	PC := MemoryManager.Read(ResetVector) or MemoryManager.Read(ResetVector+1) << 8;

	LogVerbose('[CPU] ResetVector: ' + IntToHex(ResetVector, 4) + '   PC: ' + IntToHex(PC, 4));

	if softReset then
	begin
		SetFlags(psfInterrupt);
		Dec(SP, 3);
	end
	else
	begin
		//irqMask := $FF;
		A := 0;
		SP := $FD;
		X := 0;
		Y := 0;
		PS := psfInterrupt;
		runIrq := False;
	end;

	case model of
		nesAuto, nesNTSC:
		begin
			ppuDivider := 4;
			cpuDivider := 12;
			startClockCount := 6;
			endClockCount   := 6;
		end;
		nesPAL:
		begin
			ppuDivider := 5;
			cpuDivider := 16;
			startClockCount := 8;
			endClockCount   := 8;
		end;
		nesDendy:
		begin
			ppuDivider := 5;
			cpuDivider := 15;
			startClockCount := 7;
			endClockCount   := 8;
		end;
	end;

	CycleCount := QWord(-1);

	if Configuration.Emulator.PowerOnState.RandomizeAlignment then
	begin
		ppuOffset := Random(ppuDivider);
		cpuOffset := Random(cpuDivider);
	end
	else
	begin
		cpuOffset := 0;
		ppuOffset := 1;
	end;

	MasterClock := Word(cpuDivider + cpuOffset);

	// The CPU takes some cycles before starting its execution after a reset/power up
	for i := 1 to 8 do
	begin
		StartCpuCycle(True);
		EndCpuCycle(True);
	end;
end;

function TCPU.MemoryManager_Peek(address: Word): Byte;
begin
	Result := MemoryManager.DebugRead(address);
end;

procedure TCPU.RunDMATransfer(offsetValue: Byte);
begin
	SpriteDmaTransfer := True;
	SpriteDmaOffset := offsetValue;
	NeedHalt := True;
end;

procedure TCPU.StartDMCTransfer;
begin
	// DMC DMA adds 4 cycles normally, 2 if it lands on the $4014 write or during OAM DMA
	// 3 cycles if it lands on the last write cycle of any instruction
	dmcDmaRunning := True;
	needDummyRead := True;
	NeedHalt := True;
end;

procedure TCPU.StopDMCTransfer;
begin
	if dmcDmaRunning then
	begin
		if NeedHalt then
		begin
			// If interrupted before the halt cycle starts, cancel DMA completely
			// This can happen when a write prevents the DMA from starting after being queued
			dmcDmaRunning := False;
			needDummyRead := False;
			NeedHalt := False;
		end
		else
		begin
			// Abort DMA if possible (this only appears to be possible if done within the first cycle of DMA)
			abortDmcDma := True;
		end;
	end;
end;

procedure TCPU.IRQ;
begin
	DummyRead;  // fetch opcode (and discard it - $00 (BRK) is forced into the opcode register instead)
	DummyRead;  // read next instruction byte (actually the same as above, since PC increment is suppressed. Also discarded.)
	PushWord(PC);

	if NeedNMI then
	begin
		NeedNMI := False;
		Push(PS or psfReserved);
		SetFlags(psfInterrupt);
		SetPC(MemoryReadWord(NMIVector));
	end
	else
	begin
		Push(PS or psfReserved);
		SetFlags(psfInterrupt);
		SetPC(MemoryReadWord(IRQVector));
	end;
end;

procedure TCPU.OpBRK;
var
	flags: Byte;
begin
	PushWord(PC + 1);
	flags := PS or psfBreak or psfReserved;

	if NeedNMI then
	begin
		NeedNMI := False;
		Push(flags);
		SetFlags(psfInterrupt);
		SetPC(MemoryReadWord(NMIVector));
	end
	else
	begin
		Push(flags);
		SetFlags(psfInterrupt);
		SetPC(MemoryReadWord(IRQVector));
	end;

	// Ensure we don't start an NMI right after running a BRK instruction
	// (first instruction in IRQ handler must run first - needed for nmi_and_brk test)
	prevNeedNMI := False;
end;

procedure TCPU.OpRTI;
begin
	DummyRead;
	SetPS(Pop);
	SetPC(PopWord);
end;

procedure TCPU.SetZeroNegativeFlags(value: Byte);
begin
	if value = 0 then
		SetFlags(psfZero)
	else
	if (value and $80) <> 0 then
		SetFlags(psfNegative);
end;

function TCPU.ProcessDMARead(addr: Word; var prevReadAddress: Word; enableInternalRegReads, isNesBehavior: Boolean): Byte;
var
	internalAddr:  Word;
	isSameAddress: Boolean;
	val, obMask, externalValue: Byte;
begin
	// This is to reproduce a CPU bug that can occur during DMA which can cause the 2A03 to read from
	// its internal registers (4015, 4016, 4017) at the same time as the DMA unit reads a byte from
	// the bus. This bug occurs if the CPU is halted while it's reading a value in the $4000-$401F range.
	//
	// This has a number of side effects:
	// -It can cause a read of $4015 to occur without the program's knowledge, which would clear the frame counter's IRQ flag
	// -It can cause additional bit deletions while reading the input (e.g more than the DMC glitch usually causes)
	// -It can also *prevent* bit deletions from occurring at all in another scenario
	// -It can replace/corrupt the byte that the DMA is reading, causing DMC to play the wrong sample

	if not enableInternalRegReads then
	begin
		if (addr >= $4000) and (addr <= $401F) then
			// Nothing will respond on $4000-$401F on the external bus - return open bus value
			Result := MemoryManager.GetOpenBus
		else
			Result := MemoryManager.Read(addr, memopDMARead);
		prevReadAddress := addr;
		Exit;
	end
	else
	begin
		// This glitch causes the CPU to read from the internal APU/Input registers
		// regardless of the address the DMA unit is trying to read
		internalAddr  := $4000 or (addr and $1F);
		isSameAddress := internalAddr = addr;

		case internalAddr of

			$4015:
			begin
				Result := MemoryManager.Read(internalAddr, memopDMARead);
				// Also trigger a read from the actual address the CPU was supposed to read from (external bus)
				if not isSameAddress then
					MemoryManager.Read(addr, memopDMARead);
			end;

			$4016, $4017:
			begin
				// Reading from the same input register twice in a row, skip the read entirely to avoid
				// triggering a bit loss from the read, since the controller won't react to this read
				// Return the same value as the last read, instead
				// On PAL, the behavior is unknown - for now, don't cause any bit deletions
				if (Console.Model = nesPAL) or (isNesBehavior and (prevReadAddress = internalAddr)) then
					Result := MemoryManager.GetOpenBus
				else
					Result := MemoryManager.Read(internalAddr, memopDMARead);

				if not isSameAddress then
				begin
					// The DMA unit is reading from a different address, read from it too (external bus)
					obMask := Console.ControlManager.GetOpenBusMask(internalAddr - $4016);
					externalValue := MemoryManager.Read(addr, memopDMARead);

					// Merge values, keep the external value for all open bus pins on the 4016/4017 port
					// AND all other bits together (bus conflict)
					val := externalValue and obMask;
					obMask := not obMask;
					Result := val or ((Result and obMask) and (externalValue and obMask));
				end;
			end;

			else
				Result := MemoryManager.Read(addr, memopDMARead);
		end;

		prevReadAddress := internalAddr;
	end;
end;

procedure TCPU.ProcessPendingDMA(readAddress: Word);

	procedure ProcessCycle; inline;
	begin
		// Sprite DMA cycles count as halt/dummy cycles for the DMC DMA when both run at the same time
		if abortDmcDma then
		begin
			dmcDmaRunning := False;
			abortDmcDma := False;
			NeedDummyRead := False;
			NeedHalt := False;
		end
		else
		if NeedHalt then
			NeedHalt := False
		else
		if NeedDummyRead then
			NeedDummyRead := False;

		StartCpuCycle(True);
	end;

var
	spriteReadAddr, readValue: Byte;
	spriteDmaCounter, prevReadAddress, dmcAddress: Word;
	isNtscInputBehavior, isNesBehavior, enableInternalRegReads,
	skipFirstInputClock, skipDummyReads, getCycle: Boolean;
begin
	if not NeedHalt then Exit;

	prevReadAddress := readAddress;
	enableInternalRegReads := (readAddress and $FFE0) = $4000;
	skipFirstInputClock := False;

	if (enableInternalRegReads) and (dmcDmaRunning) and ((readAddress = $4016) or (readAddress = $4017)) then
	begin
		dmcAddress := NES_APU.GetDmcReadAddress;
		if (dmcAddress and $1F) = (readAddress and $1F) then
		begin
			// DMC will cause a read on the same address as the CPU was reading from
			// This will hide the reads from the controllers because /OE will be active the whole time
			skipFirstInputClock := True;
		end;
	end;

	// On PAL, the dummy/idle reads done by the DMA don't appear to be done on the
	// address that the CPU was about to read. This prevents the 2+x reads on registers issues.
	// The exact specifics of where the CPU reads instead aren't known yet - so just disable read side-effects entirely on PAL
	isNtscInputBehavior := Console.Model <> nesPAL;

	// On Famicom, each dummy/idle read to 4016/4017 is interpreted as a read of the joypad registers
	// On NES (or AV Famicom), only the first dummy/idle read causes side effects (e.g only a single bit is lost)
	isNesBehavior := True; // !!! console->GetNesConfig.ConsoleType != NesConsoleType::Hvc001;
	skipDummyReads := (not isNtscInputBehavior) or
		(isNesBehavior and ((readAddress = $4016) or (readAddress = $4017)));
	NeedHalt := False;

	// "If this cycle is a read, hijack the read, discard the value, and prevent all other
	// actions that occur on this cycle (PC not incremented, etc)"
	StartCpuCycle(True);
	if (abortDmcDma) and (isNesBehavior) and ((readAddress = $4016) or (readAddress = $4017)) then
	begin
		// Skip halt cycle dummy read on 4016/4017
		// The DMA was aborted, and the CPU will read 4016/4017 next
		// If 4016/4017 is read here, the controllers will see 2 separate reads
		// even though they would only see a single read on hardware (except the original Famicom)
	end
	else
	if (isNtscInputBehavior) and (not skipFirstInputClock) then
		MemoryManager.Read(readAddress, memopDMARead);
	EndCpuCycle(True);

	if abortDmcDma then
	begin
		dmcDmaRunning := False;
		abortDmcDma := False;
		if not spriteDmaTransfer then
		begin
			// If DMC DMA was cancelled and OAM DMA isn't about to start,
			// stop processing DMA entirely. Otherwise, OAM DMA needs to run,
			// so the DMA process has to continue.
			needDummyRead := False;
			Exit;
		end;
	end;

	spriteDmaCounter := 0;
	spriteReadAddr := 0;
	readValue := 0;

	while (dmcDmaRunning) or (spriteDmaTransfer) do
	begin
		getCycle := (CycleCount and 1) = 0;
		if getCycle then
		begin
			if (dmcDmaRunning) and (not NeedHalt) and (not needDummyRead) then
			begin
				//DMC DMA is ready to read a byte (both halt and dummy read cycles were performed before this)
				ProcessCycle;
				//readValue := MemoryManager.Read(NES_APU.GetDmcReadAddress, memopDMCRead);
				readValue := ProcessDMARead(NES_APU.GetDmcReadAddress,
					prevReadAddress, enableInternalRegReads, isNesBehavior);
				EndCpuCycle(True);
				dmcDmaRunning := False;
				abortDmcDma := False;
				NES_APU.SetDmcReadBuffer(readValue);
			end
			else
			if spriteDmaTransfer then
			begin
				//DMC DMA is not running, or not ready, run sprite DMA
				ProcessCycle;
				//readValue := MemoryManager.Read(Word(spriteDmaOffset) * $100 + spriteReadAddr);
				readValue := ProcessDMARead(spriteDmaOffset * $100 + spriteReadAddr,
					prevReadAddress, enableInternalRegReads, isNesBehavior);
				EndCpuCycle(True);
				Inc(spriteReadAddr);
				Inc(spriteDmaCounter);
			end
			else
			begin
				// DMC DMA is running, but not ready (need halt/dummy read) and sprite DMA
				// isn't running, perform a dummy read
				ProcessCycle;
				if not skipDummyReads then
					MemoryManager.Read(readAddress, memopDummyRead);
				EndCpuCycle(True);
			end;
		end
		else
		begin
			if (spriteDmaTransfer) and ((spriteDmaCounter and 1) = 1) then
			begin
				// Sprite DMA write cycle (only do this if a sprite dma read was performed last cycle)
				ProcessCycle;
				MemoryManager.Write($2004, readValue, memopDMAWrite);
				EndCpuCycle(True);
				Inc(spriteDmaCounter);
				if spriteDmaCounter = $200 then
					spriteDmaTransfer := False;
			end
			else
			begin
				// Align to read cycle before starting sprite DMA (or align to perform DMC read)
				ProcessCycle;
				if not skipDummyReads then
					MemoryManager.Read(readAddress, memopDummyRead);
				EndCpuCycle(True);
			end;
		end;

	end;
end;

// =================================================================================================

function TCPU.GetIndAddr: Word;
begin
	Result := ReadWord;
end;

function TCPU.GetImmediate: Byte;
begin
	Result := ReadByte;
end;

function TCPU.GetZeroAddr: Byte;
begin
	Result := ReadByte;
end;

function TCPU.GetZeroXAddr: Byte;
var
	value: Byte;
begin
	value := ReadByte;
	MemoryRead(value, memopDummyRead); // Dummy read
	Result := value + X;
end;

function TCPU.GetZeroYAddr: Byte;
var
	value: Byte;
begin
	value := ReadByte;
	MemoryRead(value, memopDummyRead); //Dummy read
	Result := value + Y;
end;

function TCPU.GetAbsAddr: Word;
begin
	Result := ReadWord;
end;

function TCPU.GetAbsXAddr(wantDummyRead: Boolean = True): Word;
var
	baseAddr: Word;
	pageCrossed: Boolean;
begin
	baseAddr := ReadWord;
	pageCrossed := CheckPageCrossedU(baseAddr, X);

	// Dummy read done by the processor (only when page is crossed for READ instructions)
	if (pageCrossed) or (wantDummyRead) then
		MemoryRead(baseAddr + X - IfThen(pageCrossed, $100, 0), memopDummyRead);

	Result := baseAddr + X;
end;

function TCPU.GetAbsYAddr(wantDummyRead: Boolean = True): Word;
var
	baseAddr: Word;
	pageCrossed: Boolean;
begin
	baseAddr := ReadWord;
	pageCrossed := CheckPageCrossedU(baseAddr, Y);

	// Dummy read done by the processor (only when page is crossed for READ instructions)
	if (pageCrossed) or (wantDummyRead) then
		MemoryRead(baseAddr + Y - IfThen(pageCrossed, $100, 0), memopDummyRead);

	Result := baseAddr + Y;
end;

function TCPU.GetInd: Word;
var
	addr: Word;
	lo, hi: Byte;
begin
	addr := Operand;
	if (addr and $FF) = $FF then
	begin
		lo := MemoryRead(addr);
		hi := MemoryRead(addr - $FF);
		Result :=  Word(hi << 8) or lo;
	end
	else
		Result := MemoryReadWord(addr);
end;

function TCPU.GetIndXAddr: Word;
var
	zero: Byte;
begin
	zero := ReadByte;
	MemoryRead(zero, memopDummyRead); //Dummy read
	Inc(zero, X);

	if zero = $FF then
		Result := Word(MemoryRead($FF)) or (Word(MemoryRead($00)) << 8)
	else
		Result := MemoryReadWord(zero);
end;

function TCPU.GetIndYAddr(wantDummyRead: Boolean = True): Word;
var
	zero: Byte;
	addr: Word;
	pageCrossed: Boolean;
begin
	zero := ReadByte;

	if zero = $FF then
		addr := Word(MemoryRead($FF)) or (Word(MemoryRead($00)) << 8)
	else
		addr := MemoryReadWord(zero);

	pageCrossed := CheckPageCrossedU(addr, Y);
	// Dummy read done by the processor (only when page is crossed for READ instructions)
	if (pageCrossed) or (wantDummyRead) then
		MemoryRead(addr + Y - IfThen(pageCrossed, $100, 0), memopDummyRead);

	Result := addr + Y;
end;

// ========================================================================
// Operations
// ========================================================================

procedure TCPU.OpAND;
begin
	SetA(A and GetOperandValue);
end;

procedure TCPU.OpEOR;
begin
	SetA(A xor GetOperandValue);
end;

procedure TCPU.OpORA;
begin
	SetA(A or GetOperandValue);
end;

procedure TCPU.OpADD(value: Byte);
var
	res: Word;
begin
	res := Word(A) + Word(value) + Word(IfThen(CheckFlag(psfCarry), psfCarry, $00));

	ClearFlags(psfCarry or psfNegative or psfOverflow or psfZero);
	SetZeroNegativeFlags(res and $FF);

	if (not(A xor value) and (A xor res) and $80) <> 0 then
		SetFlags(psfOverflow);

	if res > $FF then
		SetFlags(psfCarry);

	SetA(res and $FF);
end;

procedure TCPU.OpADC;
begin
	OpADD(GetOperandValue);
end;

procedure TCPU.OpSBC;
begin
	OpADD(GetOperandValue xor $FF);
end;

procedure TCPU.OpCMP(reg, value: Byte);
var
	res: Int16;
begin
	ClearFlags(psfCarry or psfNegative or psfZero);
	res := reg - value;
	if (reg >= value)      then SetFlags(psfCarry);
	if (reg = value)       then SetFlags(psfZero);
	if (res and $80) = $80 then SetFlags(psfNegative);
end;

procedure TCPU.OpCPA;
begin
	OpCMP(A, GetOperandValue);
end;

procedure TCPU.OpCPX;
begin
	OpCMP(X, GetOperandValue);
end;

procedure TCPU.OpCPY;
begin
	OpCMP(Y, GetOperandValue);
end;

procedure TCPU.OpINC;
var
	addr: Word;
	value: Byte;
begin
	addr := Operand;
	ClearFlags(psfNegative or psfZero);
	value := MemoryRead(addr);
	MemoryWrite(addr, value, memopDummyWrite); //Dummy write
	Inc(value);
	SetZeroNegativeFlags(value);
	MemoryWrite(addr, value);
end;

procedure TCPU.OpDEC;
var
	addr: Word;
	value: Byte;
begin
	addr := Operand;
	ClearFlags(psfNegative or psfZero);
	value := MemoryRead(addr);
	MemoryWrite(addr, value, memopDummyWrite); //Dummy write
	Dec(value);
	SetZeroNegativeFlags(value);
	MemoryWrite(addr, value);
end;

function TCPU.OpASL(value: Byte): Byte;
begin
	ClearFlags(psfCarry or psfNegative or psfZero);
	if (value and $80) <> 0 then
		SetFlags(psfCarry);
	Result := value << 1;
	SetZeroNegativeFlags(Result);
end;

function TCPU.OpLSR(value: Byte): Byte;
begin
	ClearFlags(psfCarry or psfNegative or psfZero);
	if Odd(value) then
		SetFlags(psfCarry);
	Result := value >> 1;
	SetZeroNegativeFlags(Result);
end;

function TCPU.OpROL(value: Byte): Byte;
var
	carryFlag: Boolean;
begin
	carryFlag := CheckFlag(psfCarry);
	ClearFlags(psfCarry or psfNegative or psfZero);
	if (value and $80) <> 0 then
		SetFlags(psfCarry);
	Result := (value << 1) or Byte(IfThen(carryFlag, $01, $00));
	SetZeroNegativeFlags(Result);
end;

function TCPU.OpROR(value: Byte): Byte;
var
	carryFlag: Boolean;
begin
	carryFlag := CheckFlag(psfCarry);
	ClearFlags(psfCarry or psfNegative or psfZero);
	if Odd(value) then
		SetFlags(psfCarry);
	Result := (value >> 1) or Byte(IfThen(carryFlag, $80, $00));
	SetZeroNegativeFlags(Result);
end;

procedure TCPU.OpASLAddr;
var
	addr: Word;
	value: Byte;
begin
	addr := Operand;
	value := MemoryRead(addr);
	MemoryWrite(addr, value, memopDummyWrite); //Dummy write
	MemoryWrite(addr, OpASL(value));
end;

procedure TCPU.OpLSRAddr;
var
	addr: Word;
	value: Byte;
begin
	addr := Operand;
	value := MemoryRead(addr);
	MemoryWrite(addr, value, memopDummyWrite); //Dummy write
	MemoryWrite(addr, OpLSR(value));
end;

procedure TCPU.OpROLAddr;
var
	addr: Word;
	value: Byte;
begin
	addr := Operand;
	value := MemoryRead(addr);
	MemoryWrite(addr, value, memopDummyWrite); //Dummy write
	MemoryWrite(addr, OpROL(value));
end;

procedure TCPU.OpRORAddr;
var
	addr: Word;
	value: Byte;
begin
	addr := Operand;
	value := MemoryRead(addr);
	MemoryWrite(addr, value, memopDummyWrite); //Dummy write
	MemoryWrite(addr, OpROR(value));
end;

procedure TCPU.OpJMP(addr: Word);
begin
	SetPC(addr);
end;

procedure TCPU.OpBIT;
var
	value: Byte;
begin
	value := GetOperandValue;
	ClearFlags(psfZero or psfOverflow or psfNegative);
	if (A and value)   = 0 then SetFlags(psfZero);
	if (value and $40) > 0 then SetFlags(psfOverflow);
	if (value and $80) > 0 then SetFlags(psfNegative);
end;

procedure TCPU.BranchRelative(branch: Boolean);
var
	offset: Int8;
begin
	offset := Int8(Operand);
	if branch then
	begin
		// a taken non-page-crossing branch ignores IRQ/NMI during its last clock,
		// so that next instruction executes before the IRQ. Fixes "branch_delays_irq" test
		if (RunIrq) and (not PrevRunIRQ) then
			RunIrq := False;
		DummyRead;
		if CheckPageCrossedS(PC, offset) then
			DummyRead;
		SetPC(PC + offset);
	end;
end;

// =================================================================================================
// Legal Opcodes
// =================================================================================================

procedure TCPU.OpLDA; begin SetA(GetOperandValue); end;
procedure TCPU.OpLDX; begin SetX(GetOperandValue); end;
procedure TCPU.OpLDY; begin SetY(GetOperandValue); end;

procedure TCPU.OpSTA; begin MemoryWrite(Operand, A); end;
procedure TCPU.OpSTX; begin MemoryWrite(Operand, X); end;
procedure TCPU.OpSTY; begin MemoryWrite(Operand, Y); end;

procedure TCPU.OpTAX; begin SetX(A);  end;
procedure TCPU.OpTAY; begin SetY(A);  end;
procedure TCPU.OpTSX; begin SetX(SP); end;
procedure TCPU.OpTXA; begin SetA(X);  end;
procedure TCPU.OpTXS; begin SetSP(X); end;
procedure TCPU.OpTYA; begin SetA(Y);  end;

procedure TCPU.OpPHA; begin Push(A); end;
procedure TCPU.OpPHP; begin Push(PS or psfBreak or psfReserved); end;
procedure TCPU.OpPLA; begin DummyRead; SetA(Pop);  end;
procedure TCPU.OpPLP; begin DummyRead; SetPS(Pop); end;

procedure TCPU.OpINX; begin SetX(X + 1); end;
procedure TCPU.OpINY; begin SetY(Y + 1); end;
procedure TCPU.OpDEX; begin SetX(X - 1); end;
procedure TCPU.OpDEY; begin SetY(Y - 1); end;

procedure TCPU.OpASL_Acc; begin SetA(OpASL(A)); end;
procedure TCPU.OpASL_Mem; begin OpASLAddr;      end;
procedure TCPU.OpLSR_Acc; begin SetA(OpLSR(A)); end;
procedure TCPU.OpLSR_Mem; begin OpLSRAddr;      end;
procedure TCPU.OpROL_Acc; begin SetA(OpROL(A)); end;
procedure TCPU.OpROL_Mem; begin OpROLAddr;      end;
procedure TCPU.OpROR_Acc; begin SetA(OpROR(A)); end;
procedure TCPU.OpROR_Mem; begin OpRORAddr;      end;

procedure TCPU.OpJMP_Abs; begin SetPC(Operand); end;
procedure TCPU.OpJMP_Ind; begin SetPC(GetInd);  end;

procedure TCPU.OpJSR;
var
	addr: Word;
begin
	addr := Operand;
	DummyRead;
	PushWord(PC - 1);
	SetPC(addr);
end;

procedure TCPU.OpRTS;
var
	addr: Word;
begin
	addr := PopWord;
	DummyRead;
	DummyRead;
	SetPC(addr + 1);
end;

procedure TCPU.OpBCC; begin BranchRelative(not CheckFlag(psfCarry)); end;
procedure TCPU.OpBCS; begin BranchRelative(CheckFlag(psfCarry)); end;
procedure TCPU.OpBEQ; begin BranchRelative(CheckFlag(psfZero)); end;
procedure TCPU.OpBMI; begin BranchRelative(CheckFlag(psfNegative)); end;
procedure TCPU.OpBNE; begin BranchRelative(not CheckFlag(psfZero)); end;
procedure TCPU.OpBPL; begin BranchRelative(not CheckFlag(psfNegative)); end;
procedure TCPU.OpBVC; begin BranchRelative(not CheckFlag(psfOverflow)); end;
procedure TCPU.OpBVS; begin BranchRelative(CheckFlag(psfOverflow)); end;

procedure TCPU.OpCLC; begin ClearFlags(psfCarry); end;
procedure TCPU.OpCLD; begin ClearFlags(psfDecimal); end;
procedure TCPU.OpCLI; begin ClearFlags(psfInterrupt); end;
procedure TCPU.OpCLV; begin ClearFlags(psfOverflow); end;
procedure TCPU.OpSEC; begin SetFlags(psfCarry); end;
procedure TCPU.OpSED; begin SetFlags(psfDecimal); end;
procedure TCPU.OpSEI; begin SetFlags(psfInterrupt); end;
procedure TCPU.OpNOP; begin GetOperandValue; end;

//
// Illegal Opcodes
//

procedure TCPU.OpSLO;
var
	value, shiftedValue: Byte;
begin
	//ASL and ORA
	value := GetOperandValue;
	MemoryWrite(Operand, value, memopDummyWrite); //Dummy write
	shiftedValue := OpASL(value);
	SetA(A or shiftedValue);
	MemoryWrite(Operand, shiftedValue);
end;

procedure TCPU.OpSRE;
var
	value, shiftedValue: Byte;
begin
	//ROL and AND
	value := GetOperandValue;
	MemoryWrite(Operand, value, memopDummyWrite); //Dummy write
	shiftedValue := OpLSR(value);
	SetA(A xor shiftedValue);
	MemoryWrite(Operand, shiftedValue);
end;

procedure TCPU.OpRLA;
var
	value, shiftedValue: Byte;
begin
	//LSR and EOR
	value := GetOperandValue;
	MemoryWrite(Operand, value, memopDummyWrite); //Dummy write
	shiftedValue := OpROL(value);
	SetA(A and shiftedValue);
	MemoryWrite(Operand, shiftedValue);
end;

procedure TCPU.OpRRA;
var
	value, shiftedValue: Byte;
begin
	//ROR and ADC
	value := GetOperandValue;
	MemoryWrite(Operand, value, memopDummyWrite); //Dummy write
	shiftedValue := OpROR(value);
	OpADD(shiftedValue);
	MemoryWrite(Operand, shiftedValue);
end;

procedure TCPU.OpSAX;
begin
	//STA and STX
	MemoryWrite(Operand, A and X);
end;

procedure TCPU.OpLAX;
var
	value: Byte;
begin
	//LDA and LDX
	value := GetOperandValue;
	SetX(value);
	SetA(value);
end;

procedure TCPU.OpDCP;
var
	value: Byte;
begin
	//DEC and CMP
	value := GetOperandValue;
	MemoryWrite(Operand, value, memopDummyWrite); //Dummy write
	Dec(value);
	OpCMP(A, value);
	MemoryWrite(Operand, value);
end;

procedure TCPU.OpISB;
var
	value: Byte;
begin
	//INC and SBC
	value := GetOperandValue;
	MemoryWrite(Operand, value, memopDummyWrite); //Dummy write
	Inc(value);
	OpADD(value xor $FF);
	MemoryWrite(Operand, value);
end;

procedure TCPU.OpAAC;
begin
	SetA(A and GetOperandValue);
	ClearFlags(psfCarry);
	if CheckFlag(psfNegative) then
		SetFlags(psfCarry);
end;

procedure TCPU.OpASR;
begin
	ClearFlags(psfCarry);
	SetA(A and GetOperandValue);
	if (A and 1) = 1 then
		SetFlags(psfCarry);
	SetA(A  shr  1);
end;

procedure TCPU.OpARR;
begin
	SetA(((A and GetOperandValue) shr 1) or Byte(IfThen(CheckFlag(psfCarry), $80, $00)));
	ClearFlags(psfCarry or psfOverflow);
	if (A and $40) = $40 then
		SetFlags(psfCarry);
	if Byte(IfThen(CheckFlag(psfCarry), $01, $00)) xor Byte((A shr 5) and 1) = 1 then
		SetFlags(psfOverflow);
end;

procedure TCPU.OpATX;
var
	value: Byte;
begin
	//LDA and TAX
	value := GetOperandValue;
	SetA(value); //LDA
	SetX(A); //TAX
	SetA(A); //Update flags based on A
end;

procedure TCPU.OpAXS;
var
	value, opValue: Byte;
begin
	//CMP and DEX
	opValue := GetOperandValue;
	value := (A and X) - opValue;

	ClearFlags(psfCarry);
	if (A and X) >= opValue then
		SetFlags(psfCarry);

	SetX(value);
end;

(*
procedure TCPU.OpSYA;
var
	value, addrHi, addrLo: Byte;
begin
	addrHi := Operand shr 8;
	addrLo := Operand and $FF;
	value := Y and (addrHi + 1);
	//From here: http://forums.nesdev.com/viewtopic.php?f=3Opt=3831Opstart=30
	//Unsure if this is accurate or not
	// "the target address for e.g. SYA becomes ((y and (addr_high + 1)) shl 8) or addr_low
	// instead of the normal ((addr_high + 1) shl 8) or addr_low"
	MemoryWrite(((Y and (addrHi + 1)) shl 8) or addrLo, value);
end;

procedure TCPU.OpSXA;
var
	value, addrHi, addrLo: Byte;
begin
	addrHi := Operand shr 8;
	addrLo := Operand and $FF;
	value := X and (addrHi + 1);
	MemoryWrite(((X and (addrHi + 1)) shl 8) or addrLo, value);
end;
*)

// See thread/test rom: https://forums.nesdev.org/viewtopic.php?p=297765
//
procedure TCPU.SyaSxaAxa(baseAddr: Word; indexReg, valueReg: Byte);
var
	pageCrossed, hadDMA: Boolean;
	cyc: QWord;
	operand: Word;
	addrHi, addrLo: Byte;
begin
	pageCrossed := CheckPageCrossedU(baseAddr, indexReg);

	cyc := CycleCount; // dummy read
	MemoryRead(baseAddr + indexReg - IfThen(pageCrossed, $100, 0), memopDummyRead);

	// if dummy read took more than 1 cycle, it was interrupted by a DMA
	hadDma := (CycleCount - cyc) > 1;
	operand := baseAddr + indexReg;
	addrHi := operand shr 8;
	addrLo  := operand and $FF;
	// when a page is crossed, the address written to is ANDed with the register
	if pageCrossed then
		addrHi := addrHi and valueReg;

	// when a DMA interrupts the instruction right before the dummy read cycle,
	// the value written is not ANDed with the MSB of the address
	MemoryWrite((addrHi shl 8) or addrLo,
		IfThen(hadDMA, valueReg, valueReg and ((baseAddr shr 8) + 1)));
end;

procedure TCPU.OpSHY;
begin
	SyaSxaAxa(ReadWord, X, Y);
end;

procedure TCPU.OpSHX;
begin
	SyaSxaAxa(ReadWord, Y, X);
end;

procedure TCPU.OpSHAA;
begin
	SyaSxaAxa(ReadWord, Y, X and A);
end;

procedure TCPU.OpSHAZ;
var
	baseAddr: Word;
	zero, lo, hi: Byte;
begin
	zero := ReadByte;
	if zero = 0 then
	begin
		lo := MemoryRead($FF);
		hi := MemoryRead($00);
		baseAddr := (hi shl 8) or lo;
	end
	else
		baseAddr := MemoryReadWord(zero);

	SyaSxaAxa(baseAddr, Y, X and A);
end;

//
//Unimplemented/Incorrect Unofficial OP codes
//

procedure TCPU.OpHLT;
begin
	// normally freezes the cpu, we can probably assume nothing will ever call this
	GetOperandValue;
end;

procedure TCPU.OpUNK;
begin
	// make sure we take the right amount of cycles (not reliable for operations that write to memory, etc.)
	GetOperandValue;
end;

(*procedure TCPU.OpAXA;
var
	addr: Word;
begin
	addr := Operand;
	// "This opcode stores the result of A AND X AND the high Byte of the target address of the Operand +1 in memory."
	// This may not be the actual behavior, but the read/write operations are needed for proper cycle counting
	MemoryWrite(Operand, ((addr shr 8) + 1) and A and X);
end;*)

procedure TCPU.OpTAS;
(*var
	addr: Word;*)
begin
	(*
	//"AND X register with accumulator and store result in stack
	//pointer, then AND stack pointer with the high Byte of the
	//target address of the argument + 1. Store result in memory."
	addr := Operand;
	SetSP(X and A);
	MemoryWrite(addr, SP and ((addr shr 8) + 1));
	*)
		//Same as "SHA abs, y", but also sets SP = A & X
	opSHAA;
	SetSP(X and A);
end;

procedure TCPU.OpLAS;
var
	value: Byte;
begin
	//"AND memory with stack pointer, transfer result to accumulator, X register and stack pointer."
	value := GetOperandValue;
	SetA(value and SP);
	SetX(A);
	SetSP(A);
end;

{$IFDEF DEBUGGER}

procedure TCPU.Disassemble(nStart, nStop: Word);
var
	addr: Cardinal;
	opcode, value, lo, hi: Byte;
	relad: Int32;
	line_addr: Word;
	sInst: AnsiString;
begin
	for addr := nStart to nStop do
		mapLines[addr] := '';

	addr := nStart;
	value := $00; lo := $00; hi := $00;
	line_addr := 0;

	// Starting at the specified address we read an instruction
	// byte, which in turn yields information from the lookup table
	// as to how many additional bytes we need to read and what the
	// addressing mode is. I need this info to assemble human readable
	// syntax, which is different depending upon the addressing mode

	// As the instruction is decoded, a std::string is assembled
	// with the readable output
	while addr <= nStop do
	begin
		line_addr := addr;

		opcode := MemoryManager_Peek(addr);//, true);

		// Prefix line with instruction address
		sInst := '$' + IntToHex(addr, 4) + ': ' + IntToHex(opcode, 2) + ': ';

		// Read instruction, and get its readable name
		Inc(addr);
		sInst := sInst + mnemonic[opcode] + ' ';

		// Get operands from desired locations, and form the instruction based
		// upon its addressing mode. These routines mimic the actual fetch routine
		// of the 6502 in order to get accurate data as part of the instruction.

		case AddrMode[opcode] of

			amImp: ;
				//sInst := sInst + '{IMP}';

			amImm:
			begin
				value := MemoryManager_Peek(addr); Inc(addr);
				sInst := sInst + '#$' + IntToHex(value, 2);// + ' {IMM}';
			end;

			amZero:
			begin
				lo := MemoryManager_Peek(addr); Inc(addr);
				hi := $00;
				sInst := sInst + '$' + IntToHex(lo, 2);// + ' {ZP0}';
			end;

			amZeroX:
			begin
				lo := MemoryManager_Peek(addr); Inc(addr);
				hi := $00;
				sInst := sInst + '$' + IntToHex(lo, 2) + ', X';// {ZPX}';
			end;

			amZeroY:
			begin
				lo := MemoryManager_Peek(addr); Inc(addr);
				hi := $00;
				sInst := sInst + '$' + IntToHex(lo, 2) + ', Y';// {ZPY}';
			end;

			amRel:
			begin
				value := MemoryManager_Peek(addr); Inc(addr);
				relad := value;
				if relad >= 128 then
				begin
					relad := -(256-value);
					sInst := sInst + IntToStr(relad);
				end
				else
					sInst := sInst + '+' + IntToStr(relad);
				sInst := sInst + ' [$' + IntToHex(addr + relad, 4) + ']';// {REL}';
			end;

			amAbs:
			begin
				lo := MemoryManager_Peek(addr); Inc(addr);
				hi := MemoryManager_Peek(addr); Inc(addr);
				sInst := sInst + '$' + IntToHex(Word(hi shl 8) or lo, 4);// + ' {ABS}';
			end;

			amAbsX:
			begin
				lo := MemoryManager_Peek(addr); Inc(addr);
				hi := MemoryManager_Peek(addr); Inc(addr);
				sInst := sInst + '$' + IntToHex(Word(hi shl 8) or lo, 4) + ', X';// {ABX}';
			end;

			amAbsY:
			begin
				lo := MemoryManager_Peek(addr); Inc(addr);
				hi := MemoryManager_Peek(addr); Inc(addr);
				sInst := sInst + '$' + IntToHex(Word(hi shl 8) or lo, 4) + ', Y';// {ABY}';
			end;

			amInd:
			begin
				lo := MemoryManager_Peek(addr); Inc(addr);
				hi := MemoryManager_Peek(addr); Inc(addr);
				sInst := sInst + '($' + IntToHex(Word(hi shl 8) or lo, 4) + ')';// {IND}';
			end;

			amIndX:
			begin
				lo := MemoryManager_Peek(addr); Inc(addr);
				hi := $00;
				sInst := sInst + '($' + IntToHex(lo, 2) + '), X';// {IZX}';
			end;

			amIndY:
			begin
				lo := MemoryManager_Peek(addr); Inc(addr);
				hi := $00;
				sInst := sInst + '($' + IntToHex(lo, 2) + '), Y';// {IZY}';
			end;

			else ;

		end;

		if not (AddrMode[opcode] in [amImp, amImm]) then
			sInst := sInst + ' = $' + IntToHex(MemoryManager_Peek(addr), 2);

		// Add the formed string to a std::map, using the instruction's
		// address as the key. This makes it convenient to look for later
		// as the instructions are variable in length, so a straight up
		// incremental index is not sufficient.
		mapLines[line_addr] := sInst;
	end;
end;
{$ENDIF}

function TCPU.FetchOperand: Word;
begin
	case InstAddrMode of
		amAcc,
		amImp:   begin DummyRead; Result := 0; end;
		amImm,
		amRel:   Result := GetImmediate;
		amZero:  Result := GetZeroAddr;
		amZeroX: Result := GetZeroXAddr;
		amZeroY: Result := GetZeroYAddr;
		amInd:   Result := ReadWord; //GetIndAddr;
		amIndX:  Result := GetIndXAddr;
		amIndY:  Result := GetIndYAddr(False);
		amIndYW: Result := GetIndYAddr(True);
		amAbs:   Result := ReadWord; //GetAbsAddr;
		amAbsX:  Result := GetAbsXAddr(False);
		amAbsXW: Result := GetAbsXAddr(True);
		amAbsY:  Result := GetAbsYAddr(False);
		amAbsYW: Result := GetAbsYAddr(True);
		else     Result := 0;
		(*else
		begin
			Result := 0;
			Log('Invalid opcode - CPU crashed. PC=', IntToHex(PC, 4));
			Console.BreakIfDebugging;
			if Console.IsNSF then // Don't stop emulation on CPU crash when playing NSFs, reset cpu instead
				Console.Reset(True)
			else
			if (not Console.GetDebugger(False)) and (not Console.EmulationFlags.DeveloperMode)) then
				//Throw an error and stop emulation core (if debugger is not enabled)
				throw std.runtime_error("Invalid OP code - CPU crashed");
		end;*)
	end;
end;

procedure TCPU.Exec;
var
	opcode: Byte;
begin
	opcode := MemoryRead(PC, memopExecOpcode);
	{$R-}Inc(PC);
	InstAddrMode := AddrMode[opcode];
	Operand      := FetchOperand;
	OpTable[opcode]; // Execute
	if PrevRunIRQ or PrevNeedNMI then IRQ;
end;

procedure TCPU.ExecFrame;
begin
	{$IFDEF MEASURETIMING}
	PPUTimeTaken := 0;
	{$ENDIF}

	repeat
		Exec;
	until NES_PPU.frame_complete;

	SetNextFrameOverclockStatus(False);
end;

procedure TCPU.DumpRAM(const Filename: String);
var
	FS: TFileStream;
	i: Integer;
begin
	FS := TFileStream.Create(Filename, fmCreate);

	for i := 0 to $FFFF do
		FS.WriteByte(MemoryManager_Peek(i));

	FS.Free;
end;

end.

