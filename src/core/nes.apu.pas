unit NES.APU;

interface

uses
	NES.Types, NES.Config,
	NES.MemoryHandler,
	NES.AudioManager,
	NES.APU.Channel;

{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}

type
	TFrameType = ( None = 0, QuarterFrame = 1, HalfFrame = 2 );

	TStepCycles = array [0..1, 0..5] of Int32;

type
	TApuFrameCounter = class(TIMemoryHandler)
	const
		FrameType: array [0..1, 0..5] of TFrameType = (
			( QuarterFrame, HalfFrame, QuarterFrame, None, HalfFrame, None ),
			( QuarterFrame, HalfFrame, QuarterFrame, None, HalfFrame, None ) );

		StepCyclesNTSC: TStepCycles = (
			( 7457, 14913, 22371, 29828, 29829, 29830 ),
			( 7457, 14913, 22371, 29829, 37281, 37282 ) );
		StepCyclesPAL: TStepCycles = (
			( 8313, 16627, 24939, 33252, 33253, 33254 ),
			( 8313, 16627, 24939, 33253, 41565, 41566 ) );
	private
		PreviousCycle,
		CurrentStep,
		StepMode:   Cardinal;    // 0: 4-step mode, 1: 5-step mode
		InhibitIRQ: Boolean;
		NewValue:   Int16;
		BlockFrameCounterTick: Byte;
		WriteDelayCounter: ShortInt;
		nesModel:   TNesModel;
		StepCycles: TStepCycles;
	public
		procedure SetNesModel(model: TNESModel);
		procedure Reset(softReset: Boolean);
		function  Run(var cyclesToRun: Cardinal): Cardinal;
		function  NeedToRun(cyclesToRun: Cardinal): Boolean;
		procedure GetMemoryRanges(var ranges: TMemoryRanges); override;
		function  ReadRAM(addr: Word): Byte; override;
		procedure WriteRAM(addr: Word; value: Byte); override;
	end;

	// ======================================================================

	{ TAPU }

	TAPU = class(TPlayRoutine)
	private
		nesModel: TNesModel;

		Enabled,
		IsNeedToRun:     Boolean;
		PreviousCycle,
		CurrentCycle:    Cardinal;
		CyclesNeeded:    Single;
		FrameCounter:    TApuFrameCounter;

		SquareChannel:   array[0..1] of TSquareChannel;
		TriangleChannel: TTriangleChannel;
		NoiseChannel:    TNoiseChannel;
		DMCChannel:      TDMCChannel;

		function  NeedToRun(currentCycle: Cardinal): Boolean; inline;
		procedure FrameCounterTick(_type: TFrameType);
	public
		BufferSize: Cardinal;
		Settings:   TAPUConfig;

		constructor Create; overload;
		destructor  Destroy; override;

		procedure GetMemoryRanges(var ranges: TMemoryRanges); override;

		procedure Reset(softReset: Boolean);
		procedure SetNesModel(model: TNESModel; ForceInit: Boolean = False);
		procedure SettingsChanged;

		function  ReadRAM(addr: Word): Byte; override;
		function  PeekRAM(addr: Word): Byte; override;
		procedure WriteRAM(addr: Word; value: Byte); override;

		procedure Exec; inline;
		procedure ProcessCpuClock;
		procedure Run(additionalCycles: Int32 = 0);
		procedure EndFrame;

		procedure AddExpansionAudioDelta(channel: Byte; delta: Int16); inline;
		function  GetStatus: Byte;
		procedure SetApuStatus(_enabled: Boolean);
		function  IsApuEnabled: Boolean;
		procedure SetNeedToRun;

		function  GetDmcReadAddress: Word;
		procedure SetDmcReadBuffer(value: Byte);

		//procedure ClearQueuedAudio; inline;

		procedure Debug;

		procedure LoadSnapshot; override;
		procedure SaveSnapshot; override;
	end;


var
	ChDebug: array[1..4, 0..11] of AnsiString;


implementation

uses
	Math, SysUtils, Basement.Util,
	NES.Console, NES.CPU, NES.APU.Mixer;

	{$INCLUDE coredefs.inc}

// =================================================================================================
//
//     TApuFrameCounter
//
// =================================================================================================

procedure TApuFrameCounter.Reset(softReset: Boolean);
begin
	PreviousCycle := 0;
	CurrentStep := 0;

	// "After reset: APU mode in $4017 was unchanged", so we need to
	// keep whatever value StepMode has for soft resets
	if not softReset then
		StepMode := 0;

	// "After reset or power-up, APU acts as if $4017 were written with $00 from 9 to 12 clocks
	// before first instruction begins."  This is emulated in the CPU::Reset function
	// Reset acts as if $00 was written to $4017
	NewValue := IfThen(StepMode <> 0, $80, $00);

	WriteDelayCounter := 3;
	InhibitIRQ := False;

	BlockFrameCounterTick := 0;
end;

procedure TApuFrameCounter.SetNesModel(model: TNESModel);
begin
	if nesModel = model then Exit;

	nesModel := model;
	if model = nesPAL then
		StepCycles := stepCyclesPal
	else
	if model <> nesAuto then
		StepCycles := stepCyclesNtsc;
end;

function TApuFrameCounter.Run(var cyclesToRun: Cardinal): Cardinal;
var
	cyclesRan: Cardinal;
	ftype: TFrameType;
begin
	if (PreviousCycle + cyclesToRun) >= StepCycles[StepMode, CurrentStep] then
	begin
		if (not InhibitIRQ) and (StepMode = 0) and (CurrentStep >= 3) then
		begin
			//Set IRQ on the last 3 cycles for 4-step mode
			CPUSetIRQSource(irqFrameCounter);
		end;

		ftype := FrameType[StepMode, CurrentStep];
		if (ftype <> None) and (BlockFrameCounterTick = 0) then
		begin
			NES_APU.FrameCounterTick(ftype);
			// Do not allow writes to 4017 to clock the frame counter for the next cycle
			// (i.e this odd cycle + the following even cycle)
			BlockFrameCounterTick := 2;
		end;

		if StepCycles[StepMode, CurrentStep] < PreviousCycle then
			//This can happen when switching from PAL to NTSC, which can cause a freeze (endless loop in APU)
			cyclesRan := 0
		else
			cyclesRan := StepCycles[StepMode, CurrentStep] - PreviousCycle;

		Dec(cyclesToRun, cyclesRan);

		Inc(CurrentStep);
		if CurrentStep = 6 then
		begin
			CurrentStep := 0;
			PreviousCycle := 0;
		end
		else
			Inc(PreviousCycle, cyclesRan);
	end
	else
	begin
		cyclesRan := cyclesToRun;
		cyclesToRun := 0;
		Inc(PreviousCycle, cyclesRan);
	end;

	if NewValue >= 0 then
	begin
		Dec(WriteDelayCounter);
		if WriteDelayCounter = 0 then
		begin
			//Apply new value after the appropriate number of cycles has elapsed
			StepMode := IfThen((NewValue and $80) = $80, 1, 0);

			WriteDelayCounter := -1;
			CurrentStep := 0;
			PreviousCycle := 0;
			NewValue := -1;

			if (StepMode <> 0) and (BlockFrameCounterTick = 0) then
			begin
				// "Writing to $4017 with bit 7 set will immediately generate a clock for both the
				// quarter frame and the half frame units, regardless of what the sequencer is doing."
				NES_APU.FrameCounterTick(HalfFrame);
				BlockFrameCounterTick := 2;
			end;
		end;
	end;

	if BlockFrameCounterTick > 0 then
		Dec(BlockFrameCounterTick);

	Result := cyclesRan;
end;

function TApuFrameCounter.NeedToRun(cyclesToRun: Cardinal): Boolean;
begin
	// Run APU when:
	// - A new value is pending
	// - The "blockFrameCounterTick" process is running
	// - We're at the before-last or last tick of the current step
	Result := (NewValue >= 0) or (BlockFrameCounterTick > 0) or
		(PreviousCycle + cyclesToRun >= StepCycles[StepMode, CurrentStep] - 1);
end;

procedure TApuFrameCounter.GetMemoryRanges(var ranges: TMemoryRanges);
begin
	ranges.AddHandler(moWrite, $4017);
end;

function TApuFrameCounter.ReadRAM(addr: Word): Byte;
begin
	Result := 0;
end;

procedure TApuFrameCounter.WriteRAM(addr: Word; value: Byte);
begin
	NES_APU.Run;

	NewValue := value;

	// Reset sequence after $4017 is written to
	if Odd(NES_CPU.CycleCount) then
		// If the write occurs between APU cycles, the effects occur 4 CPU cycles after the write cycle.
		WriteDelayCounter := 4
	else
		// If the write occurs during an APU cycle, the effects occur 3 CPU cycles after the $4017 write cycle
		WriteDelayCounter := 3;

	InhibitIRQ := (value and $40) = $40;
	if InhibitIRQ then
		CPUClearIRQSource(irqFrameCounter);
end;

// =================================================================================================
//
//     TApu
//
// =================================================================================================

constructor TAPU.Create;
begin
	inherited Create;

	NES_APU := Self;

	BufferSize := Configuration.Audio.Latency;

	if not Init(Configuration.Audio.Latency, True, Configuration.Audio.DeviceName) then
		Log('Audio init failed!');

	Mixer := TSoundMixer.Create(Self);

	nesModel := nesNTSC;
	SetNesModel(nesModel);

	Enabled := True;
	IsNeedToRun  := False;
	CyclesNeeded := 0;

	SquareChannel[0] := TSquareChannel.Create(True);
	SquareChannel[1] := TSquareChannel.Create(False);
	TriangleChannel  := TTriangleChannel.Create;
	NoiseChannel     := TNoiseChannel.Create;
	DMCChannel       := TDMCChannel.Create;
	FrameCounter     := TApuFrameCounter.Create('APUF');

	with Console.MemoryManager do
	begin
		RegisterIODevice(SquareChannel[0]);
		RegisterIODevice(SquareChannel[1]);
		RegisterIODevice(FrameCounter);
		RegisterIODevice(TriangleChannel);
		RegisterIODevice(NoiseChannel);
		RegisterIODevice(DMCChannel);
	end;

	Reset(False);
end;

destructor TAPU.Destroy;
begin
	Mixer.Free;

	SquareChannel[0].Free;
	SquareChannel[1].Free;
	TriangleChannel.Free;
	NoiseChannel.Free;
	DMCChannel.Free;
	FrameCounter.Free;

	inherited Destroy;
end;

procedure TAPU.Reset(softReset: Boolean);
begin
//	Stop;

	CurrentCycle := 0;
	PreviousCycle := 0;
	Enabled := True;

	SquareChannel[0].Reset(softReset);
	SquareChannel[1].Reset(softReset);
	TriangleChannel.Reset(softReset);
	NoiseChannel.Reset(softReset);
	DMCChannel.Reset(softReset);
	FrameCounter.Reset(softReset);

//	if softReset then
		Mixer.Reset;
end;

procedure TAPU.SettingsChanged;
begin
	Mixer.SettingsChanged;
end;

procedure TAPU.SetNesModel(model: TNESModel; ForceInit: Boolean);
begin
	if (nesModel <> model) or (ForceInit) then
	begin
		//Finish the current apu frame before switching model
		Run;

		nesModel := model;

		SquareChannel[0].SetNesModel(model);
		SquareChannel[1].SetNesModel(model);
		TriangleChannel.SetNesModel(model);
		NoiseChannel.SetNesModel(model);
		DMCChannel.SetNesModel(model);

		FrameCounter.SetNesModel(model);

		Mixer.SetNesModel(model);
	end;
end;

procedure TAPU.GetMemoryRanges(var ranges: TMemoryRanges);
begin
	ranges.AddHandler(moRead,  $4015);
	ranges.AddHandler(moWrite, $4015);
end;

function TAPU.GetStatus: Byte;
begin
	Result := 0;
	Result := Result or IfThen(SquareChannel[0].GetStatus, $01, 0); // 1
	Result := Result or IfThen(SquareChannel[1].GetStatus, $02, 0); // 2
	Result := Result or IfThen(TriangleChannel.GetStatus,  $04, 0); // T
	Result := Result or IfThen(NoiseChannel.GetStatus,     $08, 0); // N
	Result := Result or IfThen(DMCChannel.GetStatus,       $10, 0); // D
	Result := Result or IfThen(CPUHasIRQSource(irqFrameCounter), $40, 0); // F
	Result := Result or IfThen(CPUHasIRQSource(irqDMC),          $80, 0); // I
end;

function TAPU.ReadRAM(addr: Word): Byte; // $4015 read
begin
	Run;
	Result := GetStatus;
	//Reading $4015 clears the Frame Counter interrupt flag.
	CPUClearIRQSource(irqFrameCounter);
end;

function TAPU.PeekRAM(addr: Word): Byte;
begin
	Run;
	Result := GetStatus;
end;

procedure TAPU.WriteRAM(addr: Word; value: Byte); // $4015
begin
	Run;

	// Writing to $4015 clears the DMC interrupt flag.
	// This needs to be done before setting the enabled flag for the DMC (because doing so can trigger an IRQ)
	CPUClearIRQSource(irqDMC);

	SquareChannel[0].SetEnabled((value and $01) = $01);
	SquareChannel[1].SetEnabled((value and $02) = $02);
	TriangleChannel.SetEnabled ((value and $04) = $04);
	NoiseChannel.SetEnabled    ((value and $08) = $08);
	DMCChannel.SetEnabled      ((value and $10) = $10);
end;

function TAPU.GetDmcReadAddress: Word;
begin
	Result := DMCChannel.GetDmcReadAddress;
end;

procedure TAPU.SetDmcReadBuffer(value: Byte);
begin
	DMCChannel.SetDmcReadBuffer(value);
end;

function TAPU.NeedToRun(currentCycle: Cardinal): Boolean;
var
	cyclesToRun: Cardinal;
begin
	if (IsNeedToRun) or (DMCChannel.NeedsToRun) then
	begin
		// Need to run whenever we alter the length counters
		// Need to run every cycle when DMC is running to get accurate emulation
		// (CPU stalling, interaction with sprite DMA, etc.)
		IsNeedToRun := False;
		Result := True;
	end
	else
	begin
		cyclesToRun := currentCycle - PreviousCycle;
		Result := (FrameCounter.NeedToRun(cyclesToRun)) or
			(DMCChannel.IrqPending(cyclesToRun));
	end;
end;

procedure TAPU.Exec;
begin
	Inc(CurrentCycle);
	if (CurrentCycle = Mixer_CycleLength - 1) then
		EndFrame
	else
	if NeedToRun(CurrentCycle) then
		Run;
end;

procedure TAPU.ProcessCpuClock;
begin
	if Enabled then
		Exec;
end;

procedure TAPU.FrameCounterTick(_type: TFrameType);
begin
	// Quarter & half frame clock envelope & linear counter
	SquareChannel[0].TickEnvelope;
	SquareChannel[1].TickEnvelope;
	TriangleChannel.TickLinearCounter;
	NoiseChannel.TickEnvelope;

	if _type = HalfFrame then
	begin
		// Half frames clock length counter & sweep
		SquareChannel[0].TickLengthCounter;
		SquareChannel[1].TickLengthCounter;
		TriangleChannel.TickLengthCounter;
		NoiseChannel.TickLengthCounter;

		SquareChannel[0].TickSweep;
		SquareChannel[1].TickSweep;
	end;
end;

procedure TAPU.Run(additionalCycles: Int32 = 0);
var
	cyclesToRun: Cardinal;
begin
	// Update framecounter and all channels
	// This is called:
	// - At the end of a frame
	// - Before APU registers are read/written to
	// - When a DMC or FrameCounter interrupt needs to be fired
	cyclesToRun := Max(0, CurrentCycle - PreviousCycle + additionalCycles);

	while cyclesToRun > 0 do
	begin
		Inc(PreviousCycle, FrameCounter.Run(cyclesToRun));

		// Reload counters set by writes to 4003/4008/400B/400F after running the
		// frame counter to allow the length counter to be clocked first
		// This fixes the test "len_reload_timing" (tests 4 & 5)
		SquareChannel[0].ReloadCounter;
		SquareChannel[1].ReloadCounter;
		TriangleChannel.ReloadCounter;
		NoiseChannel.ReloadCounter;

		SquareChannel[0].Run(PreviousCycle);
		SquareChannel[1].Run(PreviousCycle);
		TriangleChannel.Run(PreviousCycle);
		NoiseChannel.Run(PreviousCycle);
		DMCChannel.Run(PreviousCycle);
	end;
end;

procedure TAPU.EndFrame;
begin
	Run;

	SquareChannel[0].EndFrame;
	SquareChannel[1].EndFrame;
	TriangleChannel.EndFrame;
	NoiseChannel.EndFrame;
	DMCChannel.EndFrame;

	Mixer.PlayAudioBuffer(CurrentCycle);
	ProcessEndOfFrame;

	CurrentCycle  := 0;
	PreviousCycle := 0;
end;

procedure TAPU.AddExpansionAudioDelta(channel: Byte; delta: Int16);
begin
	Mixer.AddDelta(channel, CurrentCycle, delta);
end;

function TAPU.IsApuEnabled: Boolean;
begin
	// Adding extra lines before/after NMI temporarely turns off the APU
	// This appears to result in less side-effects than spreading out the APU's
	// load over the entire PPU frame, like what was done before.
	// This is most likely due to the timing of the Frame Counter & DMC IRQs.
	Result := Enabled;
end;

procedure TAPU.SetApuStatus(_enabled: Boolean);
begin
	Enabled := _enabled;
end;

procedure TAPU.SetNeedToRun;
begin
	IsNeedToRun := True;
end;

{procedure TAPU.ClearQueuedAudio;
begin
	Mixer.ClearQueuedAudio;
end;}

procedure TAPU.Debug;
begin
	SquareChannel[0].Debug;
	SquareChannel[1].Debug;
	TriangleChannel.Debug;
	NoiseChannel.Debug;
	DMCChannel.Debug;
end;

procedure TAPU.LoadSnapshot;
begin
	inherited LoadSnapshot;

	SquareChannel[0].LoadSnapshot;
	SquareChannel[1].LoadSnapshot;
	TriangleChannel.LoadSnapshot;
	NoiseChannel.LoadSnapshot;
	DMCChannel.LoadSnapshot;
	FrameCounter.LoadSnapshot;
	Mixer.LoadSnapshot;

	PreviousCycle := 0;
	CurrentCycle  := 0;
end;

procedure TAPU.SaveSnapshot;
begin
	inherited SaveSnapshot;

	SquareChannel[0].SaveSnapshot;
	SquareChannel[1].SaveSnapshot;
	TriangleChannel.SaveSnapshot;
	NoiseChannel.SaveSnapshot;
	DMCChannel.SaveSnapshot;
	FrameCounter.SaveSnapshot;
	Mixer.SaveSnapshot;
end;

end.
