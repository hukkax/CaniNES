// **********************************************************
// * 65032 CPU emulator core                                *
// * Pascal code by pik33 (pik33@o2.pl)                     *
// * based on Fake6502 C code                               *
// **********************************************************
// * Original Fake6502 LICENSE:                             *
// * (c)2011 Mike Chambers (miker00lz@gmail.com)            *
// * This source code is released into the                  *
// * public domain, but if you use it please do give        *
// * credit. I put a lot of effort into writing this!       *
// **********************************************************
// Modified by hukka for use in pascSID on 2018-10-12

unit fpcSID.CPU;

{$mode objfpc}{$H+}
{$MACRO ON}
{$R-}
{$optimization fastmath}

interface

uses
	Classes, SysUtils;

const
	SP_INIT = $FD;

var
	rtsDone:        Boolean = False; // returned from all subroutines recently? (for pascSID)
	calldepth:      Integer = 0;     // depth of current subroutine
	clockticks6502: Integer = 0;
	clockgoal6502:  Integer = 0;

	storadd, addr,
	PC, ea, Value:       Word;
	A, X, Y, SP, Status: Byte;

	memory: array[0..65535] of Byte;

	function  Read6502 (address: Word): Byte; inline;
	procedure Write6502(address: Word; Value: Byte); inline;
	procedure Reset6502(mempos: Integer = -1);
	procedure NMI6502;
	procedure IRQ6502;
	procedure JSR6502(aa, addr: Word);
	procedure Exec6502(tickcount: Integer);
	procedure Fast6502(tickcount: Integer);
	function  Step6502: Byte;
	procedure ClearDepth;

implementation

	procedure imp;  forward;
	procedure indx; forward;
	procedure zp;   forward;
	procedure imm;  forward;
	procedure acc;  forward;
	procedure abso; forward;
	procedure rel;  forward;
	procedure indy; forward;
	procedure absy; forward;
	procedure absx; forward;
	procedure ind;  forward;
	procedure zpx;  forward;
	procedure zpy;  forward;

	procedure iax; forward;
	procedure brk; forward;
	procedure ora; forward;
	procedure nop; forward;
	procedure ana; forward;
	procedure slo; forward;
	procedure asl; forward;
	procedure bpl; forward;
	procedure php; forward;
	procedure clc; forward;
	procedure jsr; forward;
	procedure rla; forward;
	procedure bit; forward;
	procedure rol; forward;
	procedure bmi; forward;
	procedure plp; forward;
	procedure sec; forward;
	procedure rti; forward;
	procedure eor; forward;
	procedure sre; forward;
	procedure lsr; forward;
	procedure pha; forward;
	procedure bvc; forward;
	procedure cli; forward;
	procedure jmp; forward;
	procedure rts; forward;
	procedure adc; forward;
	procedure bvs; forward;
	procedure rra; forward;
	procedure ror; forward;
	procedure pla; forward;
	procedure sei; forward;
	procedure sta; forward;
	procedure sax; forward;
	procedure stx; forward;
	procedure sty; forward;
	procedure dey; forward;
	procedure txa; forward;
	procedure bcc; forward;
	procedure tya; forward;
	procedure txs; forward;
	procedure ldy; forward;
	procedure lda; forward;
	procedure ldx; forward;
	procedure lax; forward;
	procedure tay; forward;
	procedure tax; forward;
	procedure bcs; forward;
	procedure clv; forward;
	procedure tsx; forward;
	procedure cpy; forward;
	procedure cmp; forward;
	procedure dcp; forward;
	procedure dea; forward;
	procedure iny; forward;
	procedure dex; forward;
	procedure bne; forward;
	procedure cld; forward;
	procedure cpx; forward;
	procedure isb; forward;
	procedure inx; forward;
	procedure beq; forward;
	procedure sbc; forward;
	procedure ina; forward;
	procedure sed; forward;
	procedure anc; forward;
	procedure alr; forward;
	procedure arr; forward;
	procedure xaa; forward;
	procedure ahx; forward;
	procedure shx; forward;
	procedure tas; forward;
	procedure shy; forward;
	procedure las; forward;
	procedure axs; forward;
	procedure atx; forward;
	procedure dop; forward;
	procedure top; forward;
	procedure bra; forward;
	procedure phx; forward;
	procedure phy; forward;

type
	Taddr   = procedure;
	TOpcode = procedure;

var
	addrtable: array[Byte] of TAddr = (
	//  |   0   |  1   |  2   |  3   |  4   |  5   |  6   |  7   |  8   |  9   |  A   |  B   |  C   |  D   |  E   |  F    |
	{ 0 }  @imp, @indx,  @imp, @indx,   @zp,   @zp,   @zp,   @zp,  @imp,  @imm,  @acc,  @imm, @abso, @abso, @abso, @abso, { 0 }
	{ 1 }  @rel, @indy,  @imp, @indy,  @zpx,  @zpx,  @zpx,  @zpx,  @imp, @absy,  @imp, @absy, @absx, @absx, @absx, @absx, { 1 }
	{ 2 } @abso, @indx,  @imp, @indx,   @zp,   @zp,   @zp,   @zp,  @imp,  @imm,  @acc,  @imm, @abso, @abso, @abso, @abso, { 2 }
	{ 3 }  @rel, @indy,  @imp, @indy,  @zpx,  @zpx,  @zpx,  @zpx,  @imp, @absy,  @imp, @absy, @absx, @absx, @absx, @absx, { 3 }
	{ 4 }  @imp, @indx,  @imp, @indx,   @zp,   @zp,   @zp,   @zp,  @imp,  @imm,  @acc,  @imm, @abso, @abso, @abso, @abso, { 4 }
	{ 5 }  @rel, @indy,  @imp, @indy,  @zpx,  @zpx,  @zpx,  @zpx,  @imp, @absy,  @imp, @absy, @absx, @absx, @absx, @absx, { 5 }
	{ 6 }  @imp, @indx,  @imp, @indx,   @zp,   @zp,   @zp,   @zp,  @imp,  @imm,  @acc,  @imm,  @ind, @abso, @abso, @abso, { 6 }
	{ 7 }  @rel, @indy,  @imp, @indy,  @zpx,  @zpx,  @zpx,  @zpx,  @imp, @absy,  @imp, @absy,  @iax, @absx, @absx, @absx, { 7 }
	{ 8 }  @imm, @indx,  @imm, @indx,   @zp,   @zp,   @zp,   @zp,  @imp,  @imm,  @imp,  @imm, @abso, @abso, @abso, @abso, { 8 }
	{ 9 }  @rel, @indy,  @imp, @indy,  @zpx,  @zpx,  @zpy,  @zpy,  @imp, @absy,  @imp, @absy, @absx, @absx, @absy, @absy, { 9 }
	{ A }  @imm, @indx,  @imm, @indx,   @zp,   @zp,   @zp,   @zp,  @imp,  @imm,  @imp,  @imm, @abso, @abso, @abso, @abso, { A }
	{ B }  @rel, @indy,  @imp, @indy,  @zpx,  @zpx,  @zpy,  @zpy,  @imp, @absy,  @imp, @absy, @absx, @absx, @absy, @absy, { B }
	{ C }  @imm, @indx,  @imm, @indx,   @zp,   @zp,   @zp,   @zp,  @imp,  @imm,  @imp,  @imm, @abso, @abso, @abso, @abso, { C }
	{ D }  @rel, @indy,  @imp, @indy,  @zpx,  @zpx,  @zpx,  @zpx,  @imp, @absy,  @imp, @absy, @absx, @absx, @absx, @absx, { D }
	{ E }  @imm, @indx,  @imm, @indx,   @zp,   @zp,   @zp,   @zp,  @imp,  @imm,  @imp,  @imm, @abso, @abso, @abso, @abso, { E }
	{ F }  @rel, @indy,  @imp, @indy,  @zpx,  @zpx,  @zpx,  @zpx,  @imp, @absy,  @imp, @absy, @absx, @absx, @absx, @absx  { F }
	);

	optable: array[Byte] of TOpcode = (
	//  |    0  |   1  |   2  |   3  |   4  |   5  |   6  |   7  |   8  |   9  |   A  |   B  |   C  |   D  |   E  |   F   |
	{ 0 }  @brk,  @ora,  @nop,  @slo,  @dop,  @ora,  @asl,  @slo,  @php,  @ora,  @asl,  @anc,  @top,  @ora,  @asl,  @slo, { 0 }
	{ 1 }  @bpl,  @ora,  @nop,  @slo,  @dop,  @ora,  @asl,  @slo,  @clc,  @ora,  @nop,  @slo,  @top,  @ora,  @asl,  @slo, { 1 }
	{ 2 }  @jsr,  @ana,  @nop,  @rla,  @bit,  @ana,  @rol,  @rla,  @plp,  @ana,  @rol,  @anc,  @bit,  @ana,  @rol,  @rla, { 2 }
	{ 3 }  @bmi,  @ana,  @nop,  @rla,  @dop,  @ana,  @rol,  @rla,  @sec,  @ana,  @nop,  @rla,  @top,  @ana,  @rol,  @rla, { 3 }
	{ 4 }  @rti,  @eor,  @nop,  @sre,  @dop,  @eor,  @lsr,  @sre,  @pha,  @eor,  @lsr,  @alr,  @jmp,  @eor,  @lsr,  @sre, { 4 }
	{ 5 }  @bvc,  @eor,  @nop,  @sre,  @dop,  @eor,  @lsr,  @sre,  @cli,  @eor,  @nop,  @sre,  @top,  @eor,  @lsr,  @sre, { 5 }
	{ 6 }  @rts,  @adc,  @nop,  @rra,  @dop,  @adc,  @ror,  @rra,  @pla,  @adc,  @ror,  @arr,  @jmp,  @adc,  @ror,  @rra, { 6 }
	{ 7 }  @bvs,  @adc,  @nop,  @rra,  @dop,  @adc,  @ror,  @rra,  @sei,  @adc,  @nop,  @rra,  @top,  @adc,  @ror,  @rra, { 7 }
	{ 8 }  @dop,  @sta,  @dop,  @sax,  @sty,  @sta,  @stx,  @sax,  @dey,  @dop,  @txa,  @xaa,  @sty,  @sta,  @stx,  @sax, { 8 }
	{ 9 }  @bcc,  @sta,  @nop,  @ahx,  @sty,  @sta,  @stx,  @sax,  @tya,  @sta,  @txs,  @tas,  @shy,  @sta,  @shx,  @ahx, { 9 }
	{ A }  @ldy,  @lda,  @ldx,  @lax,  @ldy,  @lda,  @ldx,  @lax,  @tay,  @lda,  @tax,  @atx,  @ldy,  @lda,  @ldx,  @lax, { A }
	{ B }  @bcs,  @lda,  @nop,  @lax,  @ldy,  @lda,  @ldx,  @lax,  @clv,  @lda,  @tsx,  @las,  @ldy,  @lda,  @ldx,  @lax, { B }
	{ C }  @cpy,  @cmp,  @dop,  @dcp,  @cpy,  @cmp,  @dea,  @dcp,  @iny,  @cmp,  @dex,  @axs,  @cpy,  @cmp,  @dea,  @dcp, { C }
	{ D }  @bne,  @cmp,  @nop,  @dcp,  @dop,  @cmp,  @dea,  @dcp,  @cld,  @cmp,  @nop,  @dcp,  @top,  @cmp,  @dea,  @dcp, { D }
	{ E }  @cpx,  @sbc,  @dop,  @isb,  @cpx,  @sbc,  @ina,  @isb,  @inx,  @sbc,  @nop,  @sbc,  @cpx,  @sbc,  @ina,  @isb, { E }
	{ F }  @beq,  @sbc,  @nop,  @isb,  @dop,  @sbc,  @ina,  @isb,  @sed,  @sbc,  @nop,  @isb,  @top,  @sbc,  @ina,  @isb  { F }
	);

	ticktable: array[Byte] of Byte = (
	{   |  0  |  1  |  2  |  3  |  4  |  5  |  6  |  7  |  8  |  9  |  A  |  B  |  C  |  D  |  E  |  F   |   }
	{ 0 }  7,    6,    7,    8,    5,    3,    5,    5,    3,    2,    2,    2,    6,    4,    6,    6,  { 0 }
	{ 1 }  2,    5,    5,    8,    5,    4,    6,    6,    2,    4,    2,    7,    6,    4,    7,    7,  { 1 }
	{ 2 }  6,    6,    7,    8,    3,    3,    5,    5,    4,    2,    2,    2,    4,    4,    6,    6,  { 2 }
	{ 3 }  2,    5,    5,    8,    4,    4,    6,    6,    2,    4,    2,    7,    4,    4,    7,    7,  { 3 }
	{ 4 }  6,    6,    7,    8,    7,    3,    5,    5,    3,    2,    2,    2,    3,    4,    6,    6,  { 4 }
	{ 5 }  2,    5,    5,    8,    4,    4,    6,    6,    2,    4,    3,    7,    4,    4,    7,    7,  { 5 }
	{ 6 }  6,    6,    7,    8,    3,    3,    5,    5,    4,    2,    2,    2,    5,    4,    6,    6,  { 6 }
	{ 7 }  2,    5,    5,    8,    4,    4,    6,    6,    2,    4,    4,    7,    6,    4,    7,    7,  { 7 }
	{ 8 }  3,    6,    7,    6,    3,    3,    3,    3,    2,    2,    2,    2,    4,    4,    4,    4,  { 8 }
	{ 9 }  2,    6,    5,    6,    4,    4,    4,    4,    2,    5,    2,    5,    4,    5,    5,    5,  { 9 }
	{ A }  2,    6,    2,    6,    3,    3,    3,    3,    2,    2,    2,    2,    4,    4,    4,    4,  { A }
	{ B }  2,    5,    5,    5,    4,    4,    4,    4,    2,    4,    2,    4,    4,    4,    4,    4,  { B }
	{ C }  2,    6,    6,    8,    3,    3,    5,    5,    2,    2,    2,    2,    4,    4,    6,    6,  { C }
	{ D }  2,    5,    5,    8,    4,    4,    6,    6,    2,    4,    3,    7,    4,    4,    7,    7,  { D }
	{ E }  2,    6,    6,    8,    3,    3,    5,    5,    2,    2,    2,    2,    4,    4,    6,    6,  { E }
	{ F }  2,    5,    5,    8,    4,    4,    6,    6,    2,    4,    4,    7,    4,    4,    7,    7   { F }
	);

{$define FLAG_CARRY:=$01}
{$define FLAG_ZERO:=$02}
{$define FLAG_INTERRUPT:=$04}
{$define FLAG_DECIMAL:=$08}
{$define FLAG_BREAK:=$10}
{$define FLAG_CONSTANT:=$20}
{$define FLAG_OVERFLOW:=$40}
{$define FLAG_SIGN:=$80}
{$define BASE_STACK:=$100}

//flag modifier macros

{$define setcarry:= status :=status or FLAG_CARRY}
{$define clearcarry:= status := status and not(FLAG_CARRY)}
{$define setzero:= status :=status or FLAG_ZERO}
{$define clearzero:= status:= status and not(FLAG_ZERO)}
{$define setinterrupt:= status :=status or FLAG_INTERRUPT}
{$define clearinterrupt:= status:= status and not(FLAG_INTERRUPT)}
{$define setdecimal:= status :=status or FLAG_DECIMAL}
{$define cleardecimal:= status := status and not(FLAG_DECIMAL)}
{$define setoverflow:= status :=status or FLAG_OVERFLOW}
{$define clearoverflow:= status := status and not(FLAG_OVERFLOW)}
{$define setsign:= status :=status or FLAG_SIGN}
{$define clearsign:= status := status and not(FLAG_SIGN)}

//helper variables

var
	oldpc, reladdr, aresult: Word;
	opcode, penaltyop, penaltyaddr: Byte;


function Read6502(address: Word): Byte;
begin
	Result := memory[address];
	addr := address;
end;

procedure Write6502(address: Word; Value: Byte);
begin
	memory[address] := Value;
	storadd := address;
end;

//a few general functions used by various other functions

procedure Push32(pushval: Cardinal);
begin
	Write6502(BASE_STACK + sp, (pushval shr 24) and $FF);
	Write6502(BASE_STACK + ((sp - 1) and $FF), (pushval shr 16) and $FF);
	Write6502(BASE_STACK + ((sp - 2) and $FF), (pushval shr 8) and $FF);
	Write6502(BASE_STACK + ((sp - 3) and $FF), pushval and $FF);
	sp -= 4;
end;

procedure Push16(pushval: Word);
begin
	Write6502(BASE_STACK + sp, (pushval shr 8) and $FF);
	Write6502(BASE_STACK + ((sp - 1) and $FF), pushval and $FF);
	sp -= 2;
end;

procedure Push8(pushval: Byte);
begin
	Write6502(BASE_STACK + sp, pushval);
	Dec(sp);
end;

function Pull32: Cardinal;
var
	temp32: cardinal;
begin
	temp32 := Read6502(BASE_STACK + ((sp + 4) and $FF));
	temp32 := (temp32 shl 8) + Read6502(BASE_STACK + ((sp + 3) and $FF));
	temp32 := (temp32 shl 8) + Read6502(BASE_STACK + ((sp + 2) and $FF));
	temp32 := (temp32 shl 8) + Read6502(BASE_STACK + ((sp + 1) and $FF));
	Result := temp32;
	sp += 4;
end;

function Pull16: Word;
var
	temp16: word;
begin
	temp16 := Read6502(BASE_STACK + ((sp + 2) and $FF));
	temp16 := (temp16 shl 8) + Read6502(BASE_STACK + ((sp + 1) and $FF));
	Result := temp16;
	sp += 2;
end;

function Pull8: Byte;
begin
	Inc(sp);
	Result := (Read6502(BASE_STACK + sp));
end;

function GetValue: Byte;
var
	ea2: Integer;
begin
	ea2 := ea;
	if (addrtable[opcode] = @acc) then
		Result := a
	else
		Result := Read6502(ea2);
end;

function GetValue16: Word;
var
	ea2: Integer;
begin
	ea2 := ea;
	Result := Word(Read6502(ea2)) or (Word(Read6502(ea2 + 1)) shl 8);
end;

function GetValue32: Cardinal;
var
	ea2: Integer;
begin
	ea2 := ea;
	Result := Cardinal(Read6502(ea2)) or (Cardinal(Read6502(ea2 + 1)) shl 8) or
		(Cardinal(Read6502(ea2 + 2)) shl 16) or (Cardinal(Read6502(ea2 + 3)) shl 24);
end;

procedure PutValue(saveval: Word);
var
	ea2: Integer;
begin
	ea2 := ea;
	if (addrtable[opcode] = @acc) then
		a := Byte(saveval and $00FF)
	else
		Write6502(ea2, (saveval and $00FF));
end;

procedure PutValue32(saveval: Cardinal);
var
	ea2: Integer;
begin
	ea2 := ea;
	Write6502(ea2,      (saveval         and $000000FF));
	Write6502(ea2 + 1, ((saveval shl  8) and $000000FF));
	Write6502(ea2 + 2, ((saveval shl 16) and $000000FF));
	Write6502(ea2 + 3, ((saveval shl 24) and $000000FF));
end;

procedure ClearDepth;
begin
	calldepth := 0;
	rtsDone := False;
end;

procedure Reset6502(mempos: Integer = -1);
begin
	if mempos >= 0 then
		PC := mempos
	else
		pc := Word(Read6502($FFFC)) or (Word(Read6502($FFFD) shl 8));
	SP := SP_INIT;
	a := 0;
	x := 0;
	y := 0;
	ClearDepth;
	//ds:=@dsa;
	//cs:=@csa;
	//:=0; ds^:=0; csi:=0; dsi:=0;
	status := status or FLAG_CONSTANT;
	clockgoal6502 := 0;
	clockticks6502 := 0;
end;

procedure NMI6502;
begin
	Push16(pc);
	Push8(status);
	status := status or FLAG_INTERRUPT;
	pc := Word(Read6502($FFFA)) or (Word(Read6502($FFFB)) shl 8);
	//cs:=@csi;
	//ds:=@dsi;
end;

procedure IRQ6502;
begin
	Push16(pc);
	Push8(status);
	status := status or FLAG_INTERRUPT;
	pc := Word(Read6502($FFFE)) or (Word(Read6502($FFFF)) shl 8);
	//cs:=@csi;
	//ds:=@dsi;
end;

procedure Exec6502(tickcount: Integer);
begin
	clockgoal6502 += tickcount;
	while (clockticks6502 < clockgoal6502) do
	begin
		opcode := Read6502(pc);
		pc += 1;
		//  status := status or FLAG_CONSTANT;
		penaltyop := 0;
		penaltyaddr := 0;
		addrtable[opcode];
		optable[opcode];
		clockticks6502 += ticktable[opcode];
		if (penaltyop <> 0) and (penaltyaddr <> 0) then
			clockticks6502 += 1;
	end;
end;

procedure Fast6502(tickcount: Integer);
begin
	clockgoal6502 += tickcount;
	while (clockticks6502 < clockgoal6502) do
	begin
		opcode := Read6502(pc);
		pc += 1;
		addrtable[opcode];
		optable[opcode];
		clockticks6502 += 1;
	end;
end;

function Step6502: Byte;
begin
	opcode := Read6502(pc);
	Result := opcode;
	Inc(pc);
	status := status or FLAG_CONSTANT;
	penaltyop := 0;
	penaltyaddr := 0;

	addrtable[opcode];
	optable[opcode];

	clockticks6502 := ticktable[opcode];
	if (penaltyop <> 0) and (penaltyaddr <> 0) then
		Inc(clockticks6502);
	clockgoal6502 := clockticks6502;
end;

procedure JSR6502(aa, addr: Word);
begin
	pc := addr;
	sp := $FD;
	if aa < 256 then
	begin
		a := aa;
		x := 0;
		y := 0;
		status := 0;
	end;
	ClearDepth;
	repeat
		opcode := Read6502(pc);
		Inc(pc);
		addrtable[opcode];
		optable[opcode];

	until rtsDone; //calldepth < 0;
end;

//addressing mode functions, calculates effective addresses

procedure imp; //implied
begin
	ea := $FFFF; // -1;
end;

procedure acc;  //accumulator
begin
	ea := $FFFF; // -1;
end;

procedure imm;  //immediate
begin
	ea := pc;
	Inc(pc);
end;

procedure zp;  //zero-page
begin
	ea := word(Read6502(pc));
	Inc(pc);
end;

procedure zpx; //zero-page,X
begin
	ea := ((Read6502(pc) + x) and $FF); //zero-page wraparound
	Inc(pc);
end;

procedure zpy; //zero-page,Y
begin
	ea := ((Read6502(pc) + y) and $FF); //zero-page wraparound
	Inc(pc);
end;

procedure rel; //relative for branch ops (8-bit immediate value, sign-extended)
begin
	reladdr := word(Read6502(pc));
	Inc(pc);
	if (reladdr and $80) <> 0 then
		reladdr := reladdr or $FF00;
	ea := reladdr;
end;

procedure abso; //absolute
begin
	ea := word(Read6502(pc)) or (word(Read6502(pc + 1)) shl 8);
	pc += 2;
end;

procedure absx;  //absolute,X
var
	startpage: word;
begin
	ea := word(Read6502(pc)) or (word(Read6502(pc + 1)) shl 8);
	startpage := ea and $FF00;
	ea += x;
	if (startpage <> (ea and $FF00)) then
		penaltyaddr := 1;   //one cycle penalty for page-crossing on some opcodes
	pc += 2;
end;

procedure absy;  //absolute,Y
var
	startpage: word;
begin
	ea := word(Read6502(pc)) or (word(Read6502(pc + 1)) shl 8);
	startpage := ea and $FF00;
	ea += y;
	if (startpage <> (ea and $FF00)) then
		penaltyaddr := 1; //one cycle penalty for page-crossing on some opcodes
	pc += 2;
end;

procedure ind;  //indirect
var
	eahelp, eahelp2: word;
begin
	eahelp := word(Read6502(pc)) or (word(Read6502(pc + 1)) shl 8);
	eahelp2 := (eahelp and $FF00) or ((eahelp + 1) and $00FF);
	//replicate 6502 page-boundary wraparound bug
	ea := word(Read6502(eahelp)) or (word(Read6502(eahelp2)) shl 8);
	pc += 2;
end;

procedure indx;  // (indirect,zp,X)
var
	eahelp: word;
begin
	eahelp := (word(Read6502(pc) + x) and $FF); //zero-page wraparound for table pointer
	Inc(pc);
	ea := word(Read6502(eahelp and $00FF)) or (word(Read6502((eahelp + 1) and $00FF)) shl 8);
end;

procedure iax;  // (indirect,X)
var
	eahelp: word;
begin
	eahelp := word(Read6502(pc)) + (word(Read6502(pc + 1) shl 8) + y);
	//zero-page wraparound for table pointer
	pc += 2;
	ea := word(Read6502(eahelp and $00FF)) or (word(Read6502((eahelp + 1) and $00FF)) shl 8);
end;

procedure indy; // (indirect),zp,Y
var
	eahelp, eahelp2, startpage: word;
begin
	eahelp := word(Read6502(pc));
	Inc(pc);
	eahelp2 := (eahelp and $FF00) or ((eahelp + 1) and $00FF); //zero-page wraparound
	ea := word(Read6502(eahelp)) or (word(Read6502(eahelp2)) shl 8);
	startpage := ea and $FF00;
	ea += y;
	if (startpage <> (ea and $FF00)) then
		penaltyaddr := 1; //one cycle penalty for page-crossing on some opcodes
end;

// addr modes end

//instruction handler functions

procedure adc;
begin
	penaltyop := 1;
	Value := GetValue;
	aresult := word(Value) + a + (status and FLAG_CARRY);
	if (aresult and $FF00) <> 0 then
		setcarry
	else
		clearcarry;
	if (aresult and $00FF) <> 0 then
		clearzero
	else
		setzero;
	if ((aresult xor a) and (aresult xor Value) and $0080) <> 0 then
		setoverflow
	else
		clearoverflow;
	if (aresult and $0080) <> 0 then
		setsign
	else
		clearsign;
	if (status and FLAG_DECIMAL) <> 0 then
	begin
		Inc(clockticks6502);
		clearcarry;
		if ((a and $0F) > $09) then
			a += $06;
		if ((a and $F0) > $90) then
		begin
			a += $60;
			setcarry;
		end;
	end;
	a := byte(aresult and $00FF);
end;

procedure ana;
begin
	penaltyop := 1;
	Value := GetValue;
	aresult := a and Value;
	if (aresult and $00FF) <> 0 then
		clearzero
	else
		setzero;
	if (aresult and $0080) <> 0 then
		setsign
	else
		clearsign;
	a := byte(aresult and $00FF);
end;

procedure asl;
begin
	Value := GetValue;
	aresult := Value shl 1;
	if (aresult and $FF00) <> 0 then
		setcarry
	else
		clearcarry;
	if (aresult and $00FF) <> 0 then
		clearzero
	else
		setzero;
	if (aresult and $0080) <> 0 then
		setsign
	else
		clearsign;
	PutValue(aresult);
end;

procedure bcc;
begin
	if ((status and FLAG_CARRY) = 0) then
	begin
		oldpc := pc;
		pc += reladdr;
		if ((oldpc and $FF00) <> (pc and $FF00)) then
			clockticks6502 += 2 //check if jump crossed a page boundary
		else
			clockticks6502 += 1;
	end;
end;

procedure bcs;
begin
	if ((status and FLAG_CARRY) = FLAG_CARRY) then
	begin
		oldpc := pc;
		pc += reladdr;
		if ((oldpc and $FF00) <> (pc and $FF00)) then
			clockticks6502 += 2 //check if jump crossed a page boundary
		else
			clockticks6502 += 1;
	end;
end;

procedure beq;
begin
	if ((status and FLAG_ZERO) = FLAG_ZERO) then
	begin
		oldpc := pc;
		pc += reladdr;
		if ((oldpc and $FF00) <> (pc and $FF00)) then
			clockticks6502 += 2 //check if jump crossed a page boundary
		else
			clockticks6502 += 1;
	end;
end;

procedure bit;
begin
	Value := GetValue;
	aresult := a and Value;
	if (aresult and $00FF) <> 0 then
		clearzero
	else
		setzero;
	status := (status and $3F) or (Value and $C0);
end;

procedure bmi;
begin
	if ((status and FLAG_SIGN) = FLAG_SIGN) then
	begin
		oldpc := pc;
		pc += reladdr;
		if ((oldpc and $FF00) <> (pc and $FF00)) then
			clockticks6502 += 2 //check if jump crossed a page boundary
		else
			clockticks6502 += 1;
	end;
end;

procedure bne;
begin
	if ((status and FLAG_ZERO) = 0) then
	begin
		oldpc := pc;
		pc += reladdr;
		if ((oldpc and $FF00) <> (pc and $FF00)) then
			clockticks6502 += 2 //check if jump crossed a page boundary
		else
			clockticks6502 += 1;
	end;
end;

procedure bpl;
begin
	if ((status and FLAG_SIGN) = 0) then
	begin
		oldpc := pc;
		pc += reladdr;
		if ((oldpc and $FF00) <> (pc and $FF00)) then
			clockticks6502 += 2 //check if jump crossed a page boundary
		else
			clockticks6502 += 1;
	end;
end;

procedure bra;
begin
	oldpc := pc;
	pc += reladdr;
	if ((oldpc and $FF00) <> (pc and $FF00)) then
		clockticks6502 += 2 //check if jump crossed a page boundary
	else
		clockticks6502 += 1;
end;

procedure brk;
begin
	pc += 1;
	Push16(pc); //push next instruction address onto stack
	Push8(status or FLAG_BREAK); //push CPU status to stack
	setinterrupt; //set interrupt flag
	pc := word(Read6502($FFFE)) or (word(Read6502($FFFF)) shl 8);
end;

procedure bvc;
begin
	if ((status and FLAG_OVERFLOW) = 0) then
	begin
		oldpc := pc;
		pc += reladdr;
		if ((oldpc and $FF00) <> (pc and $FF00)) then
			clockticks6502 += 2 //check if jump crossed a page boundary
		else
			clockticks6502 += 1;
	end;
end;

procedure bvs;
begin
	if ((status and FLAG_OVERFLOW) = FLAG_OVERFLOW) then
	begin
		oldpc := pc;
		pc += reladdr;
		if ((oldpc and $FF00) <> (pc and $FF00)) then
			clockticks6502 += 2 //check if jump crossed a page boundary
		else
			clockticks6502 += 1;
	end;
end;

procedure clc;
begin
	clearcarry;
end;

procedure cld;
begin
	cleardecimal;
end;

procedure cli;
begin
	clearinterrupt;
end;

procedure clv;
begin
	clearoverflow;
end;

procedure cmp;
begin
	penaltyop := 1;
	Value := byte(GetValue);
	aresult := word(a) - Value;
	if (a >= (Value and $00FF)) then
		setcarry
	else
		clearcarry;
	if (a = (byte(Value and $00FF))) then
		setzero
	else
		clearzero;
	if (aresult and $0080) <> 0 then
		setsign
	else
		clearsign;
end;

procedure cpx;
begin
	Value := GetValue;
	aresult := word(x) - Value;
	if (x >= (Value and $00FF)) then
		setcarry
	else
		clearcarry;
	if (x = (byte(Value and $00FF))) then
		setzero
	else
		clearzero;
	if (aresult and $0080) <> 0 then
		setsign
	else
		clearsign;
end;

procedure cpy;
begin
	Value := GetValue;
	aresult := word(y) - Value;
	if (y >= (Value and $00FF)) then
		setcarry
	else
		clearcarry;
	if (y = (Value and $00FF)) then
		setzero
	else
		clearzero;
	if (aresult and $0080) <> 0 then
		setsign
	else
		clearsign;
end;

procedure dea;
begin
	Value := GetValue;
	aresult := Value - 1;
	if (aresult and $0080) <> 0 then
		setsign
	else
		clearsign;
	if (aresult and $00FF) <> 0 then
		clearzero
	else
		setzero;
	PutValue(aresult);
end;

procedure dex;
begin
	x -= 1;
	if (x and $0080) <> 0 then
		setsign
	else
		clearsign;
	if (x and $00FF) <> 0 then
		clearzero
	else
		setzero;
end;

procedure dey;
begin
	y -= 1;
	if (y and $0080) <> 0 then
		setsign
	else
		clearsign;
	if (y and $00FF) <> 0 then
		clearzero
	else
		setzero;
end;

procedure eor;
begin
	penaltyop := 1;
	Value := GetValue;
	aresult := a xor Value;
	if (aresult and $0080) <> 0 then
		setsign
	else
		clearsign;
	if (aresult and $00FF) <> 0 then
		clearzero
	else
		setzero;
	a := byte(aresult and $00FF);
end;

procedure ina;
begin
	Value := GetValue;
	aresult := Value + 1;
	if (aresult and $0080) <> 0 then
		setsign
	else
		clearsign;
	if (aresult and $00FF) <> 0 then
		clearzero
	else
		setzero;
	PutValue(aresult);
end;

procedure inx;
begin
	x += 1;
	if (x and $0080) <> 0 then
		setsign
	else
		clearsign;
	if (x and $00FF) <> 0 then
		clearzero
	else
		setzero;
end;

procedure iny;
begin
	y += 1;
	if (y and $0080) <> 0 then
		setsign
	else
		clearsign;
	if (y and $00FF) <> 0 then
		clearzero
	else
		setzero;
end;

procedure jmp;
begin
	pc := ea;
end;

procedure jsr;
begin
	Inc(calldepth);
	Push16(pc - 1);
	pc := ea;
end;

procedure lda;
begin
	penaltyop := 1;
	Value := GetValue;
	a := (Value and $00FF);
	if (a and $0080) <> 0 then
		setsign
	else
		clearsign;
	if (a and $00FF) <> 0 then
		clearzero
	else
		setzero;
end;

procedure ldx;
begin
	penaltyop := 1;
	Value := GetValue;
	x := (Value and $00FF);
	if (x and $0080) <> 0 then
		setsign
	else
		clearsign;
	if (x and $00FF) <> 0 then
		clearzero
	else
		setzero;
end;

procedure ldy;
begin
	penaltyop := 1;
	Value := GetValue;
	y := (Value and $00FF);
	if (y and $0080) <> 0 then
		setsign
	else
		clearsign;
	if (y and $00FF) <> 0 then
		clearzero
	else
		setzero;
end;

procedure lsr;
begin
	Value := GetValue and $FF;
	aresult := Value shr 1;
	if (Value and 1) = 1 then
		setcarry
	else
		clearcarry;
	if (aresult and $0080) <> 0 then
		setsign
	else
		clearsign;
	if (aresult and $00FF) <> 0 then
		clearzero
	else
		setzero;
	PutValue(aresult);
end;

procedure nop;
begin
end;

procedure ora;
begin
	penaltyop := 1;
	Value := GetValue;
	aresult := a or Value;
	if (aresult and $0080) <> 0 then
		setsign
	else
		clearsign;
	if (aresult and $00FF) <> 0 then
		clearzero
	else
		setzero;
	a := byte(aresult and $00FF);
end;

procedure pha;
begin
	Push8(a);
end;

procedure phx;
begin
	Push8(x);
end;

procedure phy;
begin
	Push8(y);
end;

procedure php;
begin
	Push8(status or FLAG_BREAK);
end;

procedure pla;
begin
	a := Pull8;
	if (a and $0080) <> 0 then
		setsign
	else
		clearsign;
	if (a and $00FF) <> 0 then
		clearzero
	else
		setzero;
end;

procedure plp;
begin
	status := Pull8 or FLAG_CONSTANT;
end;

procedure rol;
begin
	Value := GetValue;
	aresult := (Value shl 1) or (status and FLAG_CARRY);
	if (aresult and $FF00) <> 0 then
		setcarry
	else
		clearcarry;
	if (aresult and $00FF) <> 0 then
		clearzero
	else
		setzero;
	if (aresult and $0080) <> 0 then
		setsign
	else
		clearsign;
	PutValue(aresult);
end;

procedure ror;
begin
	Value := GetValue;
	aresult := (Value shr 1) or ((status and FLAG_CARRY) shl 7);
	if (Value and 1) = 1 then
		setcarry
	else
		clearcarry;
	if (aresult and $00FF) <> 0 then
		clearzero
	else
		setzero;
	if (aresult and $0080) <> 0 then
		setsign
	else
		clearsign;
	PutValue(aresult);
end;

procedure rti;
begin
	status := Pull8;
	Value := Pull16;
	pc := Value;
//	Dec(calldepth);
//	if calldepth < 0 then
		rtsDone := True;
end;

procedure rts;
begin
	Value := Pull16;
	pc := Value + 1;
	Dec(calldepth);
	//if calldepth < 0 then
	if SP = $FF then
		rtsDone := True;
end;

procedure sbc;
begin
	penaltyop := 1;
	Value := GetValue xor $00FF;
	aresult := word(a) + Value + (status and FLAG_CARRY);
	if (aresult and $FF00) <> 0 then
		setcarry
	else
		clearcarry;
	if (aresult and $00FF) <> 0 then
		clearzero
	else
		setzero;
	if ((aresult xor a) and (aresult xor Value) and $0080) <> 0 then
		setoverflow
	else
		clearoverflow;
	if (aresult and $0080) <> 0 then
		setsign
	else
		clearsign;

	if (status and FLAG_DECIMAL) <> 0 then
	begin
		Inc(clockticks6502);
		clearcarry;
		if ((a and $0F) > $09) then
			a += $06;
		if ((a and $F0) > $90) then
		begin
			a += $60;
			setcarry;
		end;
	end;

	a := byte(aresult and $00FF);
end;

procedure sec;
begin
	setcarry;
end;

procedure sed;
begin
	setdecimal;
end;

procedure sei;
begin
	setinterrupt;
end;

procedure sta;
begin
	PutValue(a);
end;

procedure stx;
begin
	PutValue(x);
end;

procedure sty;
begin
	PutValue(y);
end;

procedure tax;
begin
	x := a;
	if (x and $0080) <> 0 then
		setsign
	else
		clearsign;
	if (x and $00FF) <> 0 then
		clearzero
	else
		setzero;
end;

procedure tay;
begin
	y := a;
	if (y and $0080) <> 0 then
		setsign
	else
		clearsign;
	if (y and $00FF) <> 0 then
		clearzero
	else
		setzero;
end;

procedure tsx;
begin
	x := sp;
	if (x and $0080) <> 0 then
		setsign
	else
		clearsign;
	if (x and $00FF) <> 0 then
		clearzero
	else
		setzero;
end;

procedure txa;
begin
	a := x;
	if (a and $0080) <> 0 then
		setsign
	else
		clearsign;
	if (a and $00FF) <> 0 then
		clearzero
	else
		setzero;
end;

procedure txs;
begin
	sp := x;
end;

procedure tya;
begin
	a := y;
	if (a and $0080) <> 0 then
		setsign
	else
		clearsign;
	if (a and $00FF) <> 0 then
		clearzero
	else
		setzero;
end;

//undocumented instructions

procedure lax;
begin
	Value := GetValue;
	a := (Value and $00FF);
	x := a;
	if (a and $0080) <> 0 then
		setsign
	else
		clearsign;
	if (a and $00FF) <> 0 then
		clearzero
	else
		setzero;
end;


procedure sax;
begin
	PutValue(a and x);
	if (a and x and $0080) <> 0 then
		setsign
	else
		clearsign;
	if (a and x and $00FF) <> 0 then
		clearzero
	else
		setzero;
end;

procedure dcp;
begin
	Value := GetValue;
	Value := (Value - 1) and 255;
	PutValue(Value);
	cmp;
end;

procedure isb;
begin
	Value := (GetValue + 1) and 255;
	PutValue(Value);
	Value := Value xor $00FF;
	aresult := word(a) + Value + (status and FLAG_CARRY);
	if (aresult and $FF00) <> 0 then
		setcarry
	else
		clearcarry;
	if (aresult and $00FF) <> 0 then
		clearzero
	else
		setzero;
	if ((aresult xor a) and (aresult xor Value) and $0080) <> 0 then
		setoverflow
	else
		clearoverflow;
	if (aresult and $0080) <> 0 then
		setsign
	else
		clearsign;

	if (status and FLAG_DECIMAL) <> 0 then
	begin
		Inc(clockticks6502);
		clearcarry;
		if ((a and $0F) > $09) then
			a += $06;
		if ((a and $F0) > $90) then
		begin
			a += $60;
			setcarry;
		end;
	end;

	a := byte(aresult and $00FF);
end;

procedure slo;
begin
	Value := GetValue;
	aresult := Value shl 1;
	if (aresult and $FF00) <> 0 then
		setcarry
	else
		clearcarry;
	if (aresult and $00FF) <> 0 then
		clearzero
	else
		setzero;
	if (aresult and $0080) <> 0 then
		setsign
	else
		clearsign;
	PutValue(aresult);
	Value := aresult;
	aresult := a or Value;
	if (aresult and $0080) <> 0 then
		setsign
	else
		clearsign;
	if (aresult and $00FF) <> 0 then
		clearzero
	else
		setzero;
	a := byte(aresult and $00FF);
end;

procedure rla;
begin
	Value := GetValue;
	aresult := (Value shl 1) or (status and FLAG_CARRY);
	if (aresult and $FF00) <> 0 then
		setcarry
	else
		clearcarry;
	if (aresult and $00FF) <> 0 then
		clearzero
	else
		setzero;
	if (aresult and $0080) <> 0 then
		setsign
	else
		clearsign;
	PutValue(aresult);
	aresult := a and aresult;
	if (aresult and $00FF) <> 0 then
		clearzero
	else
		setzero;
	if (aresult and $0080) <> 0 then
		setsign
	else
		clearsign;
	a := byte(aresult and $00FF);
	if (penaltyop <> 0) and (penaltyaddr <> 0) then
		Dec(clockticks6502);
end;

procedure sre;
begin
	Value := GetValue and $FF;
	aresult := Value shr 1;
	if (Value and 1) = 1 then
		setcarry
	else
		clearcarry;
	if (aresult and $0080) <> 0 then
		setsign
	else
		clearsign;
	if (aresult and $00FF) <> 0 then
		clearzero
	else
		setzero;
	PutValue(aresult);
	aresult := a xor aresult;
	if (aresult and $0080) <> 0 then
		setsign
	else
		clearsign;
	if (aresult and $00FF) <> 0 then
		clearzero
	else
		setzero;
	a := byte(aresult and $00FF);
end;

procedure rra;
begin
	Value := GetValue;
	aresult := (Value shr 1) or ((status and FLAG_CARRY) shl 7);
	if (Value and 1) = 1 then
		setcarry
	else
		clearcarry;
	if (aresult and $00FF) <> 0 then
		clearzero
	else
		setzero;
	if (aresult and $0080) <> 0 then
		setsign
	else
		clearsign;
	PutValue(aresult);

	Value := aresult;
	aresult := word(Value) + a + (status and FLAG_CARRY);
	if (aresult and $FF00) <> 0 then
		setcarry
	else
		clearcarry;
	if (aresult and $00FF) <> 0 then
		clearzero
	else
		setzero;
	if ((aresult xor a) and (aresult xor Value) and $0080) <> 0 then
		setoverflow
	else
		clearoverflow;
	if (aresult and $0080) <> 0 then
		setsign
	else
		clearsign;
	if (status and FLAG_DECIMAL) <> 0 then
	begin
		Inc(clockticks6502);
		clearcarry;
		if ((a and $0F) > $09) then
			a += $06;
		if ((a and $F0) > $90) then
		begin
			a += $60;
			setcarry;
		end;
	end;
	a := byte(aresult and $00FF);
end;

procedure anc;
begin
	ana;
	if (a and $80) > 0 then
		setcarry
	else
		clearcarry;
end;

procedure alr;
begin
	Value := GetValue;
	aresult := (a and Value) shr 1;
	if (aresult and $0080) <> 0 then
		setsign
	else
		clearsign;
	if (aresult and $00FF) <> 0 then
		clearzero
	else
		setzero;
	a := aresult;
end;

procedure arr;
begin
	Value := GetValue;
	aresult := a and Value;
	aresult := (aresult shr 1) or ((status and FLAG_CARRY) shl 7);
	if (aresult and 96) = 96 then
	begin
		setcarry;
		clearoverflow;
	end
	else if (aresult and 96) = 0 then
	begin
		clearcarry;
		clearoverflow;
	end
	else if (aresult and 96) = 32 then
	begin
		clearcarry;
		setoverflow;
	end
	else
	begin
		setcarry;
		setoverflow;
	end;
	a := aresult;
end;

procedure xaa;
begin
	a := x;
	Value := GetValue;
	aresult := a and Value;
	if (aresult and $00FF) <> 0 then
		clearzero
	else
		setzero;
	if (aresult and $0080) <> 0 then
		setsign
	else
		clearsign;
	a := byte(aresult and $00FF);
end;

procedure ahx;
begin
	aresult := a and x and 7;
	PutValue(aresult);
end;

procedure tas;
begin
	sp := a and x and (ea shr 8) + 1;
	PutValue(sp);
end;

procedure shy;
begin
	Value := (((ea) shr 8) and y) + 1;
	PutValue(Value);
end;

procedure shx;
begin
	Value := (((ea) shr 8) and x) + 1;
	PutValue(Value);
end;

procedure las;
begin
	Value := GetValue and sp;
	sp := Value;
	x := Value;
	a := Value;
end;

procedure axs;
begin
	Value := GetValue;
	x := word(a and x) - Value;
	if (x >= (Value and $00FF)) then
		setcarry
	else
		clearcarry;
	if (x = (byte(Value and $00FF))) then
		setzero
	else
		clearzero;
	if (x and $0080) <> 0 then
		setsign
	else
		clearsign;
end;

procedure atx;
begin
	Value := GetValue;
	a := (a and Value and $00FF);
	x := a;
	if (a and $0080) <> 0 then
		setsign
	else
		clearsign;
	if (a and $00FF) <> 0 then
		clearzero
	else
		setzero;
end;

procedure dop;
begin
	// double nop
end;

procedure top;
begin
	// double nop
end;

// end of opcodes

end.
