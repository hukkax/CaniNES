unit NES.APU.Channel;

interface

uses
	NES.Types, NES.MemoryHandler;

{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}

type
	TPeriodTable = array [0..15] of Word;

	TBaseApuChannel = class(TIMemoryHandler)
	protected
		PreviousCycle: Cardinal;
		Timer, Period: Word;
		nesModel:      TNesModel;
	private
		function DoRun(targetCycle: Cardinal): Boolean;
	public
		Output:  ShortInt;
		Channel: Byte;

		constructor Create(const RegisterName: AnsiString); override;

		procedure SetNesModel(model: TNESModel); virtual;
		function  GetNesModel: TNESModel;
		procedure Reset(softReset: Boolean); virtual;
		procedure Run(targetCycle: Cardinal);
		procedure AddOutput(value: ShortInt);
		procedure EndFrame;
		procedure Clock; virtual; abstract;

		procedure LoadSnapshot; override;
	end;

// =================================================================================================

	TApuLengthCounter = class(TBaseApuChannel)
	const
		LEN_COUNTER_TABLE: array[0..31] of Byte = (
			10, 254, 20,  2, 40,  4, 80,  6, 160,  8, 60, 10, 14, 12, 26, 14,
			12,  16, 24, 18, 48, 20, 96, 22, 192, 24, 72, 26, 16, 28, 32, 30 );
	private
		NewHaltValue: Boolean;
	protected
		Enabled:           Boolean;
		LengthCounterHalt: Boolean;
		LengthCounter,
		LengthCounterReloadValue,
		LengthCounterPreviousValue: Byte;

		procedure InitializeLengthCounter(haltFlag: Boolean);
		procedure LoadLengthCounter(value: Byte);
	public
		constructor Create(const RegisterName: AnsiString); override;

		procedure Reset(softReset: Boolean); override;
		procedure ReloadCounter;
		procedure TickLengthCounter;
		procedure SetEnabled(Enable: Boolean);
		function  GetStatus: Boolean; virtual;
		procedure Debug; virtual;
	end;

// =================================================================================================

	TApuEnvelope = class(TApuLengthCounter)
	private
		ConstantVolume:  Boolean;
		Volume:          Byte;
		EnvelopeCounter: Byte;
		Start:           Boolean;
		Divider:         ShortInt;
		Counter:         Byte;
	protected
		procedure InitializeEnvelope(regValue: Byte);
		procedure ResetEnvelope;
		function  GetVolume: Byte;
	public
		constructor Create(const RegisterName: AnsiString); override;

		procedure Reset(softReset: Boolean); override;
		procedure TickEnvelope;
	end;

// =================================================================================================

	TSquareChannel = class(TApuEnvelope)
	const
		DUTY_CYCLE_TABLE: array [0..3, 0..7] of Byte = (
			( 0, 0, 0, 0, 0, 0, 0, 1 ),
			( 0, 0, 0, 0, 0, 0, 1, 1 ),
			( 0, 0, 0, 0, 1, 1, 1, 1 ),
			( 1, 1, 1, 1, 1, 1, 0, 0 ) );
	protected
		procedure InitializeSweep(regValue: Byte); virtual;
		procedure SetPeriod(newPeriod: Word);
		procedure UpdateTargetPeriod;
	public
		IsChannel1,
		IsMMC5Square:      Boolean;
		Duty, DutyPos:     Byte;
		SweepEnabled,
		SweepNegate,
		ReloadSweep:       Boolean;
		SweepPeriod,
		SweepShift,
		SweepDivider:      Byte;
		SweepTargetPeriod: Cardinal;
		RealPeriod:        Word;

		constructor Create(asChannel1: Boolean); virtual; overload;

		procedure GetMemoryRanges(var ranges: TMemoryRanges); override;

		procedure UpdateOutput;
		procedure TickSweep;
		procedure Reset(softReset: Boolean); override;
		procedure WriteRAM(addr: Word; value: Byte); override;
		procedure Clock; override;
		procedure Debug; override;
	end;

// =================================================================================================

	TTriangleChannel = class(TApuEnvelope)
	const
		TRIANGLE_TABLE: array [0..31] of Byte = (
			15, 14, 13, 12, 11, 10, 09, 08, 07, 06, 05, 04, 03, 02, 01, 00,
			00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, 12, 13, 14, 15 );
	private
		LinearReloadFlag,
		LinearControlFlag:  Boolean;
		LinearCounter,
		LinearCounterReload,
		SequencePosition:   Byte;
	public
		constructor Create; overload;

		procedure GetMemoryRanges(var ranges: TMemoryRanges); override;

		procedure TickLinearCounter;
		procedure Reset(softReset: Boolean); override;
		procedure WriteRAM(addr: Word; value: Byte); override;
		procedure Clock; override;
		procedure Debug; override;
	end;

// =================================================================================================

	TNoiseChannel = class(TApuEnvelope)
	const
		PeriodTableNtsc: TPeriodTable = (
			4, 8, 16, 32, 64, 96, 128, 160, 202, 254, 380, 508, 762, 1016, 2034, 4068 );
		PeriodTablePal:  TPeriodTable = (
			4, 8, 14, 30, 60, 88, 118, 148, 188, 236, 354, 472, 708,  944, 1890, 3778 );
	private
		PeriodLookupTable: TPeriodTable;

		ShiftRegister: Word;
		ModeFlag:      Boolean;
	public
		constructor Create; overload;

		procedure GetMemoryRanges(var ranges: TMemoryRanges); override;
		procedure SetNesModel(model: TNESModel); override;

		procedure Reset(softReset: Boolean); override;
		procedure WriteRAM(addr: Word; value: Byte); override;
		procedure Clock; override;
		procedure Debug; override;
	end;

// =================================================================================================

	TDMCChannel = class(TApuEnvelope)
	const
		PeriodTableNtsc: TPeriodTable = (
			428, 380, 340, 320, 286, 254, 226, 214, 190, 160, 142, 128, 106, 84, 72, 54 );
		PeriodTablePal:  TPeriodTable = (
			398, 354, 316, 298, 276, 236, 210, 198, 176, 148, 132, 118,  98, 78, 66, 50 );
	private
		PeriodLookupTable: TPeriodTable;

		CurrentAddr,
		BytesRemaining,
		SampleAddr,
		SampleLength: Word;

		IRQEnabled,
		LoopFlag,
		SilenceFlag,
		BufferEmpty: Boolean;

		NeedInit,
		LastValue4011,
		OutputLevel,
		ReadBuffer,
		ShiftRegister,
		BitsRemaining: Byte;

		procedure InitSample;
	public
		NeedToRun: Boolean;
		DisableDelay,
		TransferStartDelay: Byte;

		constructor Create; overload;

		procedure GetMemoryRanges(var ranges: TMemoryRanges); override;
		procedure SetNesModel(model: TNESModel); override;

		procedure Reset(softReset: Boolean); override;
		procedure WriteRAM(addr: Word; value: Byte); override;
		procedure Clock; override;
		procedure ProcessClock;

		function  GetStatus: Boolean; override;
		function  IRQPending(cyclesToRun: Cardinal): Boolean;
		procedure SetEnabled(Enable: Boolean);
		function  NeedsToRun: Boolean;
		procedure StartDMCTransfer;

		function  GetDmcReadAddress: Word;
		procedure SetDmcReadBuffer(value: Byte);
		procedure Debug; override;
	end;

// =================================================================================================


implementation

uses
	SysUtils, NES.Config,
	NES.Console, NES.CPU, NES.APU, NES.APU.Mixer;


function BoolSt(const S: AnsiString; B: Boolean): String; inline;
begin
	Result := S[Byte(B)+1];
end;

// =================================================================================================
//
//     TBaseApuChannel
//
// =================================================================================================

constructor TBaseApuChannel.Create(const RegisterName: AnsiString);
begin
	inherited Create(RegisterName);

	RegisterProperty(8,  @Output);
	RegisterProperty(16, @Timer);
	RegisterProperty(16, @Period);
	RegisterProperty(SizeOf(TNesModel)*8, @nesModel);
end;

procedure TBaseApuChannel.SetNesModel(model: TNESModel);
begin
	nesModel := model;
end;

function TBaseApuChannel.GetNesModel: TNESModel;
begin
	if nesModel = nesDendy then
		Result := nesNTSC // Dendy APU works with NTSC timings
	else
		Result := nesModel;
end;

procedure TBaseApuChannel.Reset(softReset: Boolean);
begin
	Timer := 0;
	Period := 0;
	PreviousCycle := 0;
	Output := 0;
end;

function TBaseApuChannel.DoRun(targetCycle: Cardinal): Boolean;
var
	cyclesToRun: Int32;
begin
	cyclesToRun := targetCycle - PreviousCycle;
	if cyclesToRun > Timer then
	begin
		Inc(PreviousCycle, Timer + 1);
		Timer := Period;
		Exit(True);
	end;

	Dec(Timer, cyclesToRun);
	PreviousCycle := targetCycle;
	Result := False;
end;

procedure TBaseApuChannel.Run(targetCycle: Cardinal);
begin
	while DoRun(targetCycle) do
		Clock;
end;

procedure TBaseApuChannel.AddOutput(value: ShortInt);
begin
	if value <> Output then
	begin
		Mixer.AddDelta(Channel, PreviousCycle, value - Output);
		Output := value;
	end;
end;

procedure TBaseApuChannel.EndFrame;
begin
	PreviousCycle := 0;
end;

procedure TBaseApuChannel.LoadSnapshot;
begin
	inherited LoadSnapshot;

	EndFrame;
end;

// =================================================================================================
//
//     TApuLengthCounter
//
// =================================================================================================

constructor TApuLengthCounter.Create(const RegisterName: AnsiString);
begin
	inherited Create(RegisterName);

	SetNesModel(nesNTSC);

	RegisterProperty(1, @Enabled);
	RegisterProperty(1, @LengthCounterHalt);
	RegisterProperty(1, @NewHaltValue);
	RegisterProperty(8, @LengthCounter);
	RegisterProperty(8, @LengthCounterPreviousValue);
	RegisterProperty(8, @LengthCounterReloadValue);
end;

procedure TApuLengthCounter.Reset(softReset: Boolean);
begin
	inherited Reset(softReset);

	Enabled := False;

	// At reset, length counters should be enabled, triangle unaffected
	if (not softReset) or (Channel <> channel_Triangle) then
	begin
		LengthCounterHalt := False;
		LengthCounter := 0;
		NewHaltValue := False;
		LengthCounterReloadValue := 0;
		LengthCounterPreviousValue := 0;
	end;
end;

procedure TApuLengthCounter.InitializeLengthCounter(haltFlag: Boolean);
begin
	NES_APU.SetNeedToRun;
	NewHaltValue := haltFlag;
end;

procedure TApuLengthCounter.LoadLengthCounter(value: Byte);
begin
	if Enabled then
	begin
		LengthCounterReloadValue := LEN_COUNTER_TABLE[value];
		LengthCounterPreviousValue := LengthCounter;
		NES_APU.SetNeedToRun;
	end;
end;

procedure TApuLengthCounter.ReloadCounter;
begin
	if LengthCounterReloadValue > 0 then
	begin
		if (LengthCounter = LengthCounterPreviousValue) then
			LengthCounter := LengthCounterReloadValue;
		LengthCounterReloadValue := 0;
	end;

	LengthCounterHalt := NewHaltValue;
end;

procedure TApuLengthCounter.TickLengthCounter;
begin
	if (LengthCounter > 0) and (not LengthCounterHalt) then
		Dec(LengthCounter);
end;

procedure TApuLengthCounter.SetEnabled(Enable: Boolean);
begin
	if not Enable then
		LengthCounter := 0;
	Enabled := Enable;
end;

function TApuLengthCounter.GetStatus: Boolean;
begin
	Result := LengthCounter > 0;
end;

procedure TApuLengthCounter.Debug;
begin
end;

// =================================================================================================
//
//     TApuEnvelope
//
// =================================================================================================

constructor TApuEnvelope.Create(const RegisterName: AnsiString);
begin
	inherited Create(RegisterName);

	RegisterProperty(1, @ConstantVolume);
	RegisterProperty(8, @Volume);
	RegisterProperty(8, @EnvelopeCounter);
	RegisterProperty(1, @Start);
	RegisterProperty(8, @Divider);
	RegisterProperty(8, @Counter);
end;

procedure TApuEnvelope.InitializeEnvelope(regValue: Byte);
begin
	ConstantVolume := (regValue and $10) = $10;
	Volume := regValue and $F;
end;

procedure TApuEnvelope.ResetEnvelope;
begin
	Start := True;
end;

function TApuEnvelope.GetVolume: Byte;
begin
	if LengthCounter > 0 then
	begin
		if ConstantVolume then
			Result := Volume
		else
			Result := Counter;
	end
	else
		Result := 0;
end;

procedure TApuEnvelope.Reset(softReset: Boolean);
begin
	inherited Reset(softReset);

	ConstantVolume := False;
	Volume := 0;
	EnvelopeCounter := 0;
	Start := False;
	Divider := 0;
	Counter := 0;
end;

procedure TApuEnvelope.TickEnvelope;
begin
	if not Start then
	begin
		Dec(Divider);
		if Divider < 0 then
		begin
			Divider := Volume;
			if Counter > 0 then
				Dec(Counter)
			else
			if LengthCounterHalt then
				Counter := 15;
		end;
	end
	else
	begin
		Start := False;
		Counter := 15;
		Divider := Volume;
	end;
end;

// =================================================================================================
//
//     TSquareChannel
//
// =================================================================================================

constructor TSquareChannel.Create(asChannel1: Boolean);
begin
	inherited Create('APU_SQCH');

	RegisterProperty(16, @RealPeriod);
	RegisterProperty(8,  @Duty);
	RegisterProperty(8,  @DutyPos);
	RegisterProperty(1,  @SweepEnabled);
	RegisterProperty(8,  @SweepPeriod);
	RegisterProperty(1,  @SweepNegate);
	RegisterProperty(8,  @SweepShift);
	RegisterProperty(1,  @ReloadSweep);
	RegisterProperty(8,  @SweepDivider);
	RegisterProperty(32, @SweepTargetPeriod);

	IsChannel1 := asChannel1;

	if asChannel1 then
		Channel := channel_Square1
	else
		Channel := channel_Square2;
end;

procedure TSquareChannel.UpdateOutput;
begin
	// A period of t < 8, either set explicitly or via a sweep period
	// update, silences the corresponding pulse channel.
	if (RealPeriod < 8) or ((not SweepNegate) and (SweepTargetPeriod > $7FF)) then
		AddOutput(0)
	else
		AddOutput(DUTY_CYCLE_TABLE[Duty, DutyPos] * GetVolume);
end;

procedure TSquareChannel.Clock;
begin
	DutyPos := (DutyPos - 1) and 7;
	UpdateOutput;
end;

procedure TSquareChannel.Reset(softReset: Boolean);
begin
	inherited Reset(softReset);

	Duty := 0;
	DutyPos := 0;

	RealPeriod := 0;

	SweepEnabled := False;
	SweepPeriod := 0;
	SweepNegate := False;
	SweepShift := 0;
	ReloadSweep := False;
	SweepDivider := 0;
	SweepTargetPeriod := 0;

	UpdateTargetPeriod;
end;

procedure TSquareChannel.WriteRAM(addr: Word; value: Byte);
begin
	NES_APU.Run;

	case (addr and 3) of

		0: //4000 & 4004
		begin
			InitializeLengthCounter((value and $20) = $20);
			InitializeEnvelope(value);

			Duty := (value and $C0) shr 6;
			//if Configuration.SwapDutyCycles then
			//	Duty := ((Duty and $02) shr 1) or ((Duty and 1) shl 1);
		end;

		1: //4001 & 4005
			InitializeSweep(value);

		2: //4002 & 4006
			SetPeriod((RealPeriod and $0700) or value);

		3: //4003 & 4007
		begin
			LoadLengthCounter(value shr 3);

			SetPeriod((RealPeriod and $FF) or ((value and $07) shl 8));

			//The sequencer is restarted at the first value of the current sequence.
			DutyPos := 0;

			//The envelope is also restarted.
			ResetEnvelope;
		end;

	end;

	if not IsMMC5Square then
		UpdateOutput;
end;

// Sweep

procedure TSquareChannel.TickSweep;
begin
	Dec(SweepDivider);
	if SweepDivider = 0 then
	begin
		if (SweepShift > 0) and (SweepEnabled) and (RealPeriod >= 8) and (SweepTargetPeriod <= $7FF) then
			SetPeriod(SweepTargetPeriod);
		SweepDivider := SweepPeriod;
	end;

	if(ReloadSweep) then
	begin
		SweepDivider := SweepPeriod;
		ReloadSweep  := False;
	end;
end;

procedure TSquareChannel.InitializeSweep(regValue: Byte);
begin
	SweepEnabled := (regValue and $80) <> 0;
	SweepNegate  := (regValue and $08) <> 0;

	// The divider's period is set to P + 1
	SweepPeriod := ((regValue and $70) shr 4) + 1;
	SweepShift  := (regValue  and $07);

	UpdateTargetPeriod;

	//Side effects: Sets the reload flag
	ReloadSweep := True;
end;

// Period

procedure TSquareChannel.UpdateTargetPeriod;
var
	shiftResult: Word;
begin
	shiftResult := RealPeriod shr SweepShift;

	if SweepNegate then
	begin
		SweepTargetPeriod := RealPeriod - shiftResult;
		if IsChannel1 then
			// As a result, a negative sweep on pulse channel 1 will subtract the shifted period value minus 1
			Dec(SweepTargetPeriod);
	end
	else
		SweepTargetPeriod := RealPeriod + shiftResult;
end;

procedure TSquareChannel.GetMemoryRanges(var ranges: TMemoryRanges);
begin
	if IsChannel1 then
		ranges.AddHandler(moWrite, $4000, $4003)
	else
		ranges.AddHandler(moWrite, $4004, $4007);
end;

procedure TSquareChannel.SetPeriod(newPeriod: Word);
begin
	RealPeriod := newPeriod;
	Period := (RealPeriod * 2) + 1;
	UpdateTargetPeriod;
end;

procedure TSquareChannel.Debug;
begin
end;

// =================================================================================================
//
//     TTriangleChannel
//
// =================================================================================================

constructor TTriangleChannel.Create;
begin
	inherited Create('APU_TRCH');

	RegisterProperty(8, @LinearCounter);
	RegisterProperty(8, @LinearCounterReload);
	RegisterProperty(1, @LinearReloadFlag);
	RegisterProperty(1, @LinearControlFlag);
	RegisterProperty(8, @SequencePosition);

	Channel := channel_Triangle;
end;

procedure TTriangleChannel.GetMemoryRanges(var ranges: TMemoryRanges);
begin
	ranges.AddHandler(moWrite, $4008, $400B);
end;

procedure TTriangleChannel.Reset(softReset: Boolean);
begin
	inherited Reset(softReset);

	LinearCounter := 0;
	LinearCounterReload := 0;
	LinearReloadFlag := False;
	LinearControlFlag := False;
	SequencePosition := 0;
end;

procedure TTriangleChannel.Clock;
begin
	// The sequencer is clocked by the timer as long as both
	// the linear counter and the length counter are nonzero.
	if (LengthCounter > 0) and (LinearCounter > 0) then
	begin
		SequencePosition := (SequencePosition + 1) and $1F;

		if (Period >= 2) or (not NES_APU.Settings.SilenceTriangleHighFreq) then
		begin
			// Disabling the triangle channel when period is < 2 removes "pops" in
			// the audio that are caused by the ultrasonic frequencies.
			AddOutput(TRIANGLE_TABLE[SequencePosition]);
		end;
	end;
end;

procedure TTriangleChannel.WriteRAM(addr: Word; value: Byte);
begin
	NES_APU.Run;

	case (addr and 3) of

		0: // $4008
		begin
			LinearControlFlag   := (value and $80) = $80;
			LinearCounterReload :=  value and $7F;
			InitializeLengthCounter(LinearControlFlag);
		end;

		2: // $400A
		begin
			Period := (Period and $FF00) or value;
		end;

		3: // $400B
		begin
			LoadLengthCounter(value shr 3);
			Period := (Period and $FF) or ((value and 7) shl 8);
			// Side effect: sets the linear counter reload flag
			LinearReloadFlag := True;
		end;

	end;
end;

procedure TTriangleChannel.TickLinearCounter;
begin
	if LinearReloadFlag then
		LinearCounter := LinearCounterReload
	else
	if LinearCounter > 0 then
		Dec(LinearCounter);

	if not LinearControlFlag then
		LinearReloadFlag := False;
end;

procedure TTriangleChannel.Debug;
begin
end;

// =================================================================================================
//
//     TNoiseChannel
//
// =================================================================================================

constructor TNoiseChannel.Create;
begin
	inherited Create('APU_NSCH');

	RegisterProperty(16, @ShiftRegister);
	RegisterProperty(1,  @ModeFlag);

	Channel := channel_Noise;
end;

procedure TNoiseChannel.GetMemoryRanges(var ranges: TMemoryRanges);
begin
	ranges.AddHandler(moWrite, $400C, $400F);
end;

procedure TNoiseChannel.Clock;
var
	feedback: Word;
begin
	// Feedback is calculated as the exclusive-OR of bit 0 and one other bit:
	// bit 6 if Mode flag is set, otherwise bit 1.
	if ModeFlag then
		feedback := (ShiftRegister and 1) xor Word((ShiftRegister shr 6) and 1)
	else
		feedback := (ShiftRegister and 1) xor Word((ShiftRegister shr 1) and 1);

	ShiftRegister := ShiftRegister shr 1;
	ShiftRegister := ShiftRegister or (feedback shl 14);

	// The mixer receives the current envelope volume except when
	// Bit 0 of the shift register is set, or the length counter is zero
	if (ShiftRegister and 1) <> 0 then
		AddOutput(0)
	else
		AddOutput(GetVolume);
end;

procedure TNoiseChannel.Reset(softReset: Boolean);
begin
	inherited Reset(softReset);

	Period := PeriodLookupTable[0] - 1;
	ShiftRegister := 1;
	ModeFlag := False;
end;

procedure TNoiseChannel.SetNesModel(model: TNESModel);
begin
	inherited SetNesModel(model);

	if GetNesModel = nesNTSC then
		PeriodLookupTable := PeriodTableNtsc
	else
		PeriodLookupTable := PeriodTablePal;
end;

procedure TNoiseChannel.WriteRAM(addr: Word; value: Byte);
begin
	NES_APU.Run;

	case (addr and 3) of

		0: // $400C
		begin
			InitializeLengthCounter((value and $20) <> 0);
			InitializeEnvelope(value);
		end;

		2: // $400E
		begin
			Period := PeriodLookupTable[value and $F] - 1;
			ModeFlag := (value and $80) <> 0;
		end;

		3: // $400F
		begin
			LoadLengthCounter(value shr 3);
			ResetEnvelope;  //The envelope is also restarted.
		end;
	end;
end;

procedure TNoiseChannel.Debug;
const
	i = 4;
begin

ChDebug[i,00] := 'Noise ' + BoolSt(' X', Enabled) + BoolSt('mM', ModeFlag);

ChDebug[i,01] := Format('Period %4d', [ Period-1 ]);
ChDebug[i,02] := Format('Timer  %4d', [ Timer ]);
ChDebug[i,03] := Format('Freq   %4d', [ 0 ]);
ChDebug[i,04] := Format('Shift  %4x', [ ShiftRegister ]);

{ChDebug[i,05] := Format('Env:  %s %s %s', [
	BoolSt('rR', self.TickEnvelope),
	BoolSt('lL', self._l),
	BoolSt('eE', Envelope.Enabled)
	]);}
{ChDebug[i,06] := Format('Counter %3d', [Envelope.Counter]);
ChDebug[i,07] := Format('Divider %3d', [Envelope.Divider]);}
ChDebug[i,08] := Format('Volume   %2d', [Output]);

{ChDebug[i,09] := Format('LengthCtr %s', [ BoolSt('hH', LengthCounter.Halt)]);
ChDebug[i,10] := Format('Counter %3d', [LengthCounter.Value]);
ChDebug[i,11] := Format('Reload  %3d', [LengthCounter.ReloadValue]);
}
end;

// =================================================================================================
//
//     TDMCChannel
//
// =================================================================================================

constructor TDMCChannel.Create;
begin
	inherited Create('APU_DMCH');

	RegisterProperty(16, @SampleAddr);
	RegisterProperty(16, @SampleLength);
	RegisterProperty(8,  @OutputLevel);
	RegisterProperty(1,  @IRQEnabled);
	RegisterProperty(1,  @LoopFlag);
	RegisterProperty(16, @CurrentAddr);
	RegisterProperty(16, @BytesRemaining);
	RegisterProperty(8,  @ReadBuffer);
	RegisterProperty(1,  @BufferEmpty);
	RegisterProperty(8,  @ShiftRegister);
	RegisterProperty(8,  @BitsRemaining);
	RegisterProperty(1,  @SilenceFlag);
	RegisterProperty(1,  @NeedToRun);
	RegisterProperty(8,  @TransferStartDelay);
	RegisterProperty(8,  @DisableDelay);

	Channel := channel_DMC;

	BufferEmpty := True;
	SilenceFlag := True;
end;

procedure TDMCChannel.GetMemoryRanges(var ranges: TMemoryRanges);
begin
	ranges.AddHandler(moWrite, $4010, $4013);
end;

procedure TDMCChannel.SetNesModel(model: TNESModel);
begin
	inherited SetNesModel(model);

	if GetNesModel in [nesNTSC, nesDendy] then
		PeriodLookupTable := PeriodTableNtsc
	else
		PeriodLookupTable := PeriodTablePal;
end;

procedure TDMCChannel.Reset(softReset: Boolean);
begin
	inherited Reset(softReset);

	if not softReset then
	begin
		// At power on, the sample address is set to $C000 and sample length is set to 1
		// Resetting does not reset their value
		SampleAddr   := $C000;
		SampleLength := 1;
	end;

	OutputLevel    := 0;
	IRQEnabled     := False;
	LoopFlag       := False;

	CurrentAddr    := 0;
	BytesRemaining := 0;
	ReadBuffer     := 0;
	BufferEmpty    := True;

	ShiftRegister  := 0;
	BitsRemaining  := 8;
	SilenceFlag    := True;
	NeedToRun      := False;
	BufferEmpty    := True;

	NeedInit       := 0;
	LastValue4011  := 0;

	DisableDelay       := 0;
	TransferStartDelay := 0;

	// Not sure if this is accurate, but it seems to make things better rather than worse (for dpcmletterbox)
	// On the real thing, I think the power-on value is 428 (or the equivalent at least -
	// it uses a linear feedback shift register), though only the even/oddness should matter for this test.
	Period := PeriodLookupTable[0] - 1;

	// Make sure the DMC doesn't tick on the first cycle - this is part of what keeps
	// Sprite/DMC DMA tests working while fixing dmc_pitch.
	Timer := Period;
end;

function TDMCChannel.GetStatus: Boolean;
begin
	Result := BytesRemaining > 0;
end;

function TDMCChannel.IRQPending(cyclesToRun: Cardinal): Boolean;
var
	cyclesToEmptyBuffer: Cardinal;
begin
	Result := False;
	if (IRQEnabled) and (BytesRemaining > 0) then
	begin
		cyclesToEmptyBuffer := (BitsRemaining + (BytesRemaining-1) * 8) * Period;
		if cyclesToRun >= cyclesToEmptyBuffer then
			Result := True;
	end;
end;

procedure TDMCChannel.SetEnabled(Enable: Boolean);
begin
	if not Enable then
	begin
		if DisableDelay = 0 then
		begin
			// Disabling takes effect with a 1 apu cycle delay
			// If a DMA starts during this time, it gets cancelled
			// but this will still cause the CPU to be halted for 1 cycle
			if (NES_CPU.GetCycleCount and 1) = 0 then
				DisableDelay := 2
			else
				DisableDelay := 3;
		end;
		NeedToRun := True;
	end
	else
	if BytesRemaining = 0 then
	begin
		InitSample;
		// Delay a number of cycles based on odd/even cycles
		// Allows behavior to match dmc_dma_start_test
		if (NES_CPU.GetCycleCount and 1) = 0 then
			TransferStartDelay := 2
		else
			TransferStartDelay := 3;
		NeedToRun := True;
	end;
end;

procedure TDMCChannel.InitSample;
begin
	CurrentAddr    := SampleAddr;
	BytesRemaining := SampleLength;
	NeedToRun      := NeedToRun or (BytesRemaining > 0);
end;

function TDMCChannel.NeedsToRun: Boolean;
begin
	if NeedToRun then ProcessClock;
	Result := NeedToRun;
end;

procedure TDMCChannel.StartDMCTransfer;
begin
	if (BufferEmpty) and (BytesRemaining > 0) then
		NES_CPU.StartDMCTransfer;
end;

procedure TDMCChannel.WriteRAM(addr: Word; value: Byte);
var
	newValue, previousLevel: Byte;
begin
	NES_APU.Run;

	case (addr and 3) of

		0: // $4010
		begin
			IRQEnabled := (value and $80) = $80;
			LoopFlag   := (value and $40) = $40;

			// "The rate determines for how many CPU cycles happen between changes
			// in the output level during automatic delta-encoded sample playback."
			// Because BaseApuChannel does not decrement when setting Timer, we need
			// to actually set the value to 1 less than the lookup table
			Period := PeriodLookupTable[value and $0F] - 1;

			if not IRQEnabled then
				CPUClearIRQSource(irqDMC);
		end;

		1: // $4011
		begin
			newValue := value and $7F;
			previousLevel := OutputLevel;
			OutputLevel := newValue;

			// Reduce popping sounds for $4011 writes
			if (NES_APU.Settings.ReduceDmcPopping) and (Abs(OutputLevel - previousLevel) > 50) then
				Dec(OutputLevel, (OutputLevel - previousLevel) div 2);

			// $4011 applies new output right away, not on the timer's reload.
			// This fixes bad DMC sound when playing through $4011.
			AddOutput(OutputLevel);

			if (newValue > 0) and (LastValue4011 <> value) then
				CPUSetNextFrameOverclockStatus(True);

			LastValue4011 := newValue;
		end;

		2: // $4012
		begin
			SampleAddr := $C000 + (value shl 6);
			if value > 0 then
				CPUSetNextFrameOverclockStatus(False);
		end;

		3: // $4013
		begin
			SampleLength := (value shl 4) or 1;
			if value > 0 then
				CPUSetNextFrameOverclockStatus(False);
		end;

	end;
end;

procedure TDMCChannel.Clock;
begin
	if not SilenceFlag then
	begin
		if (ShiftRegister and 1) <> 0 then
		begin
			if OutputLevel <= 125 then
				Inc(OutputLevel, 2);
		end
		else
		if OutputLevel >= 2 then
			Dec(OutputLevel, 2);
		ShiftRegister := ShiftRegister shr 1;
	end;

	Dec(BitsRemaining);

	if BitsRemaining = 0 then
	begin
		BitsRemaining := 8;
		if BufferEmpty then
			SilenceFlag := True
		else
		begin
			SilenceFlag   := False;
			ShiftRegister := ReadBuffer;
			BufferEmpty   := True;
			NeedToRun     := True;
			StartDmcTransfer;
		end;
	end;

	AddOutput(OutputLevel);
end;

procedure TDMCChannel.ProcessClock;
begin
	if DisableDelay > 0 then
	begin
		Dec(DisableDelay);
		if DisableDelay = 0 then
		begin
			BytesRemaining := 0;
			// Abort any on-going transfer that hasn't fully started
			NES_CPU.StopDMCTransfer;
		end;
	end;

	if TransferStartDelay > 0 then
	begin
		Dec(TransferStartDelay);
		if TransferStartDelay = 0 then
			StartDmcTransfer;
	end;

	NeedToRun := (DisableDelay > 0) or (TransferStartDelay > 0) or (BytesRemaining > 0);
end;

procedure TDMCChannel.Debug;
begin
end;

function TDMCChannel.GetDmcReadAddress: Word;
begin
	Result := CurrentAddr;
end;

procedure TDMCChannel.SetDmcReadBuffer(value: Byte);
begin
	if BytesRemaining > 0 then
	begin
		ReadBuffer := value;
		BufferEmpty := False;

		// "The address is incremented; if it exceeds $FFFF, it is wrapped around to $8000."
		Inc(CurrentAddr);
		if CurrentAddr = 0 then
			CurrentAddr := $8000;

		Dec(BytesRemaining);
		if BytesRemaining = 0 then
		begin
			if LoopFlag then // Looped sample should never set IRQ flag
				InitSample
			else
			if IRQEnabled then
				CPUSetIrqSource(irqDMC);
		end;
	end;

	if (LoopFlag = False) and (SampleLength = 1) then
	begin
		// When DMA ends around the time the bit counter resets, a CPU glitch sometimes causes another DMA to be requested immediately.
		if (BitsRemaining = 8) and (Timer = Period) then //and (console->GetNesConfig().EnableDmcSampleDuplicationGlitch)
		begin
			// When the DMA ends on the same cycle as the bit counter resets
			// This glitch exists on all H CPUs and some G CPUs (those from around 1990 and later)
			// In this case, a full DMA is performed on the same address, and the same sample byte
			// is played twice in a row by the DMC
			ShiftRegister := ReadBuffer;
			SilenceFlag := False;
			BufferEmpty := True;
			InitSample;
			StartDmcTransfer;
		end
		else
		if (BitsRemaining = 1) and (Timer < 2) then
		begin
			// When the DMA ends on the APU cycle before the bit counter resets
			// If it this happens right before the bit counter resets,
			// a DMA is triggered and aborted 1 cycle later (causing one halted CPU cycle)
			ShiftRegister := ReadBuffer;
			BufferEmpty := False;
			InitSample;
			DisableDelay := 3;
		end;
	end;
end;

end.
