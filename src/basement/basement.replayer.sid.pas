unit Basement.Replayer.SID;

// ================================================================================================
// fpcSID - a SID player for FreePascal based on cSID-light
// ================================================================================================
// cSID by Hermit (Mihaly Horvath), 2017 - http://hermit.sidrip.com
// fpcSID by hukka (Joel Toivonen) 2018-10-19 - http://hukka.ncn.fi
// License: WTF - Do what you want with this code, but please give appropriate credit.
// ================================================================================================
// fpcSID changes from the original cSID-light:
//
// * The original cSID used its own 6502 emulation code, which I've replaced with a slightly
//   modified version of "65032 CPU emulator core" by pik33 (pik33@o2.pl), which is based on
//   the Fake6502 code by Mike Chambers (miker00lz@gmail.com).
// * Added channel mute and panning settings
// * Stereo output instead of mono
// * Optional tweaks to the ADSR generation to improve playback of GoatTracker v1.x tunes
// * Some fixes to SID file loader (code always assumed that the data started with a load address)
//
// Usage example:  var SID: TSIDPlayer.Create;  SID.Load('foo.sid', 1, 8580);  [...]  SID.Free;
// ================================================================================================

// Enable workarounds for some playroutines that would play wrong without.
// Disable if you don't need it to gain some speed.
//
{$DEFINE WORKAROUNDS}

// Use the Basement framework for audio playback? (You will need to substitute your own otherwise)
//
{$DEFINE USE_BASEMENT}

// ================================================================================================

{$MODE DELPHI}
{$OPTIMIZATION FASTMATH}
{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}

interface

uses
	{$IFDEF USE_BASEMENT}
	Basement.Audio,
	{$ENDIF}
	Classes, SysUtils;

type
	TWaveformArray = array[0..4095] of Word;

	TSIDPlayer = class {$IFDEF USE_BASEMENT} (TPlayRoutine) {$ENDIF}
	private

		procedure	FixEnvironment(addr: Word); inline;
		function	CombinedWaveform(num, channel: Byte; var wfarray: TWaveformArray;
					index: Integer; differ6581: Boolean; freqh: Byte): Cardinal;
		procedure	CreateCombinedWaveform(var wfarray: TWaveformArray;
					bitmul, bitstrength, threshold: Single);

		procedure	InitSID;
		procedure	InitCPU(mempos: Word);

		procedure	SID(num: Byte; baseaddr: Word); // SID chip emulation

	public

		SIDamount, subtune, subtune_amount: Integer;
		SIDmodel: array[0..2] of Boolean;
		InitAddress, PlayAddress: Word;
		VBLTiming, IRQMode: Boolean;

		Info: record
			Title, Author, Info: String;
		end;

		Settings: record

			// Channel mute flags for SIDs 1-3. Muted channels are still emulated.
			ChannelEnabled: array[0..8] of Boolean;

			// Channel pan values for SIDs 1-3; 0=center, -1.0=full left, +1.0=full right
			Panning:        array[0..8] of Single;

			AutoPlay:       Boolean;

			WorkArounds: record
				// Modifies how the ADSR generation is handled. Defaults to True, but
				// set to False if you have "skipping" in e.g. GoatTracker v1.x tunes.
				ADSRDelayBug: Boolean;

				// Set all of the below to False for a slight speedup if your tune has no issues!

				// Fix Whittaker player (if GATE-bit triggered too fast, 0 for some cycles then 1)
				GateFix:      Boolean;
				// Dynamic CIA-setting (Galway/Rubicon workaround)
				DynamicCIA:   Boolean;
				// CJ in the USA workaround (writing above $D420, except SID2/SID3)
				BoundsFix:    Boolean;
			end;

		end;

		function	LoadFromFile(const Filename: String): Boolean; override;
		function	Load(const Filename: String;
						TuneIndex: Byte = 0;   // Subtune, 0 for default
						SIDChip:   Word = 0    // 6581/8580 or 0 for default specified in the file
						): Boolean;
		procedure 	Reset;

		procedure	Render(Buffer: System.PUInt8; Len: System.DWord; Data: Pointer); override;

		constructor Create;
		destructor  Destroy; override;
	end;


implementation

uses
	fpcSID.CPU,
	Math;

type
	TArrayOfSmallInt = array [0..65535] of SmallInt;

const
	SampleRate = 44100;
	StereoSeparation = 20; // default stereo separation in percent, 0..100

	SID_6581 = False;
	SID_8580 = True;
	SIDModelNames: array[Boolean] of String = ('6581', '8580');

	SID1 = $D400; // base address of the SID chip
	C64_PAL_CPUCLK = 985248.0;
	PAL_FRAMERATE = 50.06;

	// cca 1.5 MOhm Rshunt across VCR FET drain and source (causing 220Hz bottom cutoff with
	// 470pF integrator capacitors in old C64)
	VCR_SHUNT_6581 = 1500; // kOhm
	// (on cutoff numeric range 0..2048) for the VCR cutoff freq control FET below which it doesn't conduct
	VCR_FET_TRESHOLD = 192; // Vth
	//filter capacitor value for 6581
	CAP_6581 = 0.470; //nF
	//bigger value = darker filter control (=cutoff freq increases less with the same cutoff value)
	FILTER_DARKNESS_6581 = 22.0;
	//bigger value = more resistance-modulation (filter distortion) applied to 6581 cutoff-control
	FILTER_DISTORTION_6581 = 0.0016;

	FILTSW: array[0..8] of Byte = ( 1, 2, 4,  1, 2, 4,  1, 2, 4);
	CLOCK_RATIO_DEFAULT = C64_PAL_CPUCLK / SampleRate;

	GATE_BITMASK         = $01;
	SYNC_BITMASK         = $02;
	RING_BITMASK         = $04;
	TEST_BITMASK         = $08;
	TRI_BITMASK          = $10;
	SAW_BITMASK          = $20;
	PULSE_BITMASK        = $40;
	NOISE_BITMASK        = $80;
	HOLDZERO_BITMASK     = $10;
	DECAYSUSTAIN_BITMASK = $40;
	ATTACK_BITMASK       = $80;
	LOWPASS_BITMASK      = $10;
	BANDPASS_BITMASK     = $20;
	HIGHPASS_BITMASK     = $40;
	OFF3_BITMASK         = $80;

	BANKSELECT = $01;

var
	BT: array[Boolean] of Byte;

	//raw output divided by this after multiplied by main volume,
	// this also compensates for filter-resonance emphasis to avoid distortion
	OUTPUT_SCALEDOWN: Integer;
	clock_ratio: Single = CLOCK_RATIO_DEFAULT;

	//SID-emulation variables

	ADSRstate, expcnt, prevSR, sourceMSBrise: array[0..8] of Byte;
	envcnt: array[0..8] of SmallInt;
	prevwfout, prevwavdata, noise_LFSR: array[0..8] of Cardinal;
	sourceMSB: array[0..2] of Cardinal;
	phaseaccu, prevaccu: array[0..8] of Cardinal;//Integer;
	prevlowpass, prevbandpass: array[0..2] of Integer;
	ratecnt: array[0..8] of Single;
	cutoff_ratio_8580, cutoff_steepness_6581, cap_6581_reciprocal: Single;

	//player-related variables
	timermode: array[0..$20-1] of Byte;
	SID_address: array[0..2] of Word = (SID1, 0, 0);
	framecnt, frame_sampleperiod: Integer;

	//CPU (and CIA/VIC-IRQ) emulation constants and variables
	playaddf, pPC: Word;
	CPUtime: Single = 0.0;
	finished: Boolean = False;
	dynCIA: Boolean = False;
	RSID: Boolean = False;

	outsample: array[0..1] of Single; // Renderer


{$IFDEF DEBUG}
procedure Debug(const Text: String; Vars: array of Const); inline;
begin
	Log(Format(Text, Vars));
end;
{$ENDIF}

procedure TSIDPlayer.Reset;
var
	timeout: Integer;
begin
	InitCPU(InitAddress);
	InitSID;

	A := subtune;
	memory[$DC05] := 0;

	ClearDepth;
	for timeout := 100000 downto 0 do
	begin
		Step6502;
		if rtsDone then Break;
	end;

	if (timermode[subtune] <> 0) or (memory[$DC05] <> 0) then // CIA timing
	begin
		if memory[$DC05] = 0 then // C64 startup default
		begin
			memory[$DC04] := $24;
			memory[$DC05] := $40;
		end;
		frame_sampleperiod := Trunc((memory[$DC04] + memory[$DC05] * 256) / clock_ratio);
		VBLTiming := False;
	end
	else
	begin
		frame_sampleperiod := Trunc(samplerate / PAL_FRAMERATE); // VBL timing
		VBLTiming := True;
	end;

	{$IFDEF DEBUG}
	Debug('Frame-sampleperiod: %d samples (%.1fX speed)',
		[Trunc(frame_sampleperiod), samplerate / PAL_FRAMERATE / frame_sampleperiod]);
	{$ENDIF}

	if playaddf = 0 then
	begin
		if (memory[1] and 3) < 2 then
			PlayAddress := memory[$FFFE] + memory[$FFFF] * 256
		else
			PlayAddress := memory[$314] + memory[$315] * 256;
		IRQMode := True;
		{$IFDEF DEBUG}
		Debug('IRQ playaddress: $%4.4X', [PlayAddress]);
		{$ENDIF}
	end
	else
	begin
		//player under KERNAL (Crystal Kingdom Dizzy)
		PlayAddress := playaddf;
		{$IFDEF DEBUG}
		Debug('KERNAL playaddress: $%4.4X', [PlayAddress]);
		{$ENDIF}
		IRQMode := False;
	end;
	InitCPU(PlayAddress);

	framecnt := 1;
	CPUtime := 0;
	finished := False;
end;

procedure TSIDPlayer.FixEnvironment(addr: Word); inline;
begin
	if (RSID) or (addr  < $A000) then memory[BANKSELECT] := $37  // I/O, Kernal-ROM, Basic-ROM
	else      if (addr  < $D000) then memory[BANKSELECT] := $36  // I/O, Kernal-ROM
	else      if (addr >= $E000) then memory[BANKSELECT] := $35  // I/O only
	else                              memory[BANKSELECT] := $34; // RAM only
end;

procedure TSIDPlayer.InitCPU(mempos: Word);
begin
	FixEnvironment(mempos);
	Reset6502(mempos);
end;

//registers: 0:freql1  1:freqh1  2:pwml1  3:pwmh1  4:ctrl1  5:ad1   6:sr1  7:freql2  8:freqh2  9:pwml2 10:pwmh2 11:ctrl2 12:ad2  13:sr 14:freql3 15:freqh3 16:pwml3 17:pwmh3 18:ctrl3 19:ad3  20:sr3
//           21:cutoffl 22:cutoffh 23:flsw_reso 24:vol_ftype 25:potX 26:potY 27:OSC3 28:ENV3
procedure TSIDPlayer.InitSID;
var
	i: Integer;
begin
	for i := $D400 to $D7FF do memory[i] := 0;
	for i := $DE00 to $DFFF do memory[i] := 0;
	for i := 0 to 8 do
	begin
		ADSRstate[i] := HOLDZERO_BITMASK;
		ratecnt[i] := 0;
		envcnt[i] := 0;
		expcnt[i] := 0;
	end;
	memory[$D418] := $F; // max volume
end;

// Fills Buffer with Len bytes of audio data. Buffer is assumed to be in signed 16-bit format.
//
procedure TSIDPlayer.Render(Buffer: System.PUInt8; Len: System.DWord; Data: Pointer);

	{$IFNDEF USE_BASEMENT}
	function Clamp(val: Single): Integer; inline;
	begin
		Result := Trunc(val);
		if Result >  32767 then Result :=  32767
		else
		if Result < -32768 then Result := -32768;
	end;
	{$ENDIF}

var
	i: Integer;
	outputL, outputR: Single;
	stream: ^TArrayOfSmallInt absolute Buffer;
begin
	for i := 0 to len div 4 - 1 do
	begin
		Dec(framecnt);
		if framecnt <= 0 then
		begin
			framecnt := frame_sampleperiod;
			finished := False;
			PC := PlayAddress;
			SP := SP_INIT;
			ClearDepth;
			FixEnvironment(PC);
		end;

		if not finished then
		begin
			while CPUtime <= clock_ratio do
			begin
				pPC := PC;
				Step6502;

				//RTS,RTI and IRQ player ROM return handling
				if (rtsDone) or
					( ((memory[BANKSELECT] and 3) > 1) and (pPC < $E000) and ((PC = $EA31) or (PC = $EA81)) ) then
				begin
					finished := True;
					rtsDone := False;
					Break;
				end
				else
					CPUtime += clockticks6502;

				{$IFDEF WORKAROUNDS}

				// Whittaker player (if GATE-bit triggered too fast, 0 for some cycles then 1)
				if (Settings.WorkArounds.GateFix) and ( (storadd >= $D404) and (storadd <= $D412) ) then
				begin
					if (storadd = $D404) and ((memory[$D404] and GATE_BITMASK) = 0) then
						ADSRstate[0] := ADSRstate[0] and $3E
					else
					if (storadd = $D40B) and ((memory[$D40B] and GATE_BITMASK) = 0) then
						ADSRstate[1] := ADSRstate[1] and $3E
					else
					if (storadd = $D412) and ((memory[$D412] and GATE_BITMASK) = 0) then
						ADSRstate[2] := ADSRstate[2] and $3E;
				end
				else
				// dynamic CIA-setting (Galway/Rubicon workaround)
				if (Settings.WorkArounds.DynamicCIA) and
					( ((storadd = $DC05) or (storadd = $DC04)) and
					  ((memory[BANKSELECT] and 3) <> 0) and (timermode[subtune] <> 0) ) then
				begin
					frame_sampleperiod := Trunc((memory[$DC04] + memory[$DC05] * 256) / clock_ratio);
					if not dynCIA then
					begin
						dynCIA := True;
						{$IFDEF DEBUG}
						Debug('( Dynamic CIA settings. New frame-sampleperiod: %d samples  (%.1fX speed) )',
						[frame_sampleperiod, samplerate/PAL_FRAMERATE / frame_sampleperiod]);
						{$ENDIF}
					end;
				end
				else
				// CJ in the USA workaround (writing above $D420, except SID2/SID3)
				// write to $D400..D41F if not in SID2/SID3 address-space
				if (Settings.WorkArounds.BoundsFix) and (
				   (storadd >= $D420) and (storadd < $D800) and ((memory[BANKSELECT] and 3) <> 0) ) then
				begin
					if (not	((SID_address[1] <= storadd) and (storadd < SID_address[1]+$1F))  and
						not ((SID_address[2] <= storadd) and (storadd < SID_address[2]+$1F))) then
						memory[storadd and $D41F] := memory[storadd];
				end;

				{$ENDIF}
			end;

			CPUtime -= clock_ratio;
		end;

		SID(0, SID1);
		outputL := Clamp(outsample[0]);
		outputR := Clamp(outsample[1]);

		if SIDamount > 1 then
		begin
			SID(1, SID_address[1]);
			outputL += Clamp(outsample[0]);
			outputR += Clamp(outsample[1]);

			if SIDamount = 3 then
			begin
				SID(2, SID_address[2]);
				outputL += Clamp(outsample[0]);
				outputR += Clamp(outsample[1]);
			end;
		end;

		stream[i*2]   := Clamp(outputL);
		stream[i*2+1] := Clamp(outputR);
	end;
end;

const
	PERIOD0 = CLOCK_RATIO_DEFAULT;
	STEP0 = 3;
	ADSR_exptable: array [0..255] of Byte = (
		1, 30, 30, 30, 30, 30, 30, 16, 16, 16, 16, 16, 16, 16, 16, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 4, 4, 4, 4, 4, //pos0:1  pos6:30  pos14:16  pos26:8
		4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, //pos54:4 //pos93:2
		1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
		1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
		1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1);

var
	TriSaw_8580, PulseSaw_8580, PulseTriSaw_8580: TWaveformArray; // unsigned int
	ADSRperiods: array [0..15] of Single = (
		PERIOD0, 32, 63, 95, 149, 220, 267, 313, 392, 977, 1954, 3126, 3907, 11720, 19532, 31251 );
	ADSRstep: array [0..15] of Byte = ( STEP0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 );

	channel, ctrl, SR, prevgate, wf, test: Byte;
	sReg, vReg: PByte;
	accuadd, MSB, pw, wfout: Cardinal;
	tmp, step, lim, filtout: Integer;
	nonfilt, filtin: array[0..2] of Integer;
	period, steep, ftmp, rDS_VCR_FET: Single;
	cutoff, resonance: array[0..2] of Single;


// the SID emulation itself ('num' is the number of SID to iterate (0..2)
procedure TSIDPlayer.SID(num: Byte; baseaddr: Word);
var
	ch: Integer;
begin
	//vReg := @memory[baseaddr + (channel * 7)];
	vReg := @memory[baseaddr];
	sReg := @memory[baseaddr];

	for ch := 0 to 2 do
	begin
		nonfilt[ch]:= 0;
		filtin[ch] := 0;
	end;

	for channel := num * 3 to (num + 1) * 3 - 1 do
	begin
		ctrl := vReg[4];

	// ============================================================================================
	// ADSR envelope generator
	// ============================================================================================

		SR := vReg[6];
		tmp := 0;
		prevgate := ADSRstate[channel] and GATE_BITMASK;

		// gatebit-change?
		if prevgate <> (ctrl and GATE_BITMASK) then
		begin
			// falling edge
			if (prevgate <> 0) then
				ADSRstate[channel] := ADSRstate[channel] and
					($FF - (GATE_BITMASK or ATTACK_BITMASK or DECAYSUSTAIN_BITMASK))
			else
			begin
				// rising edge, also sets hold_zero_bit=0
				ADSRstate[channel] := (GATE_BITMASK or ATTACK_BITMASK or DECAYSUSTAIN_BITMASK);
				// assume SR->GATE write order: workaround to have crisp soundstarts by triggering delay-bug
				// this is for the possible missed CTRL(GATE) vs SR register write order situations
				// (1MHz CPU is cca 20 times faster than samplerate)
				if (Settings.WorkArounds.ADSRDelayBug) and ((SR and $F) > (prevSR[channel] and $F)) then
					tmp := 1;
			end;
		end;

		// assume SR->GATE write order: workaround to have crisp soundstarts by triggering delay-bug
		prevSR[channel] := SR;

		ratecnt[channel] += clock_ratio;
		if (ratecnt[channel] >= $8000) then
		begin
			if Settings.WorkArounds.ADSRDelayBug then
				ratecnt[channel] -= $8000 // can wrap around (ADSR delay-bug: short 1st frame)
			else
				ratecnt[channel] := 0;
		end;

		//set ADSR period that should be checked against rate-counter (depending on ADSR state Attack/DecaySustain/Release)
		if (ADSRstate[channel] and ATTACK_BITMASK) <> 0 then
			step := vReg[5] shr 4
		else
		if (ADSRstate[channel] and DECAYSUSTAIN_BITMASK) <> 0 then
			step := vReg[5] and $F
		else
			step := SR and $F;

		period := ADSRperiods[step];
		step := ADSRstep[step];

		// ratecounter shot (matches rateperiod) (in genuine SID ratecounter is LFSR)
		if (tmp = 0) and (ratecnt[channel] >= period) and (ratecnt[channel] < (period + clock_ratio)) then
		begin
			// compensation for timing instead of simply setting 0 on rate-counter overload
			ratecnt[channel] -= period;

			// if ((ADSRstate[channel] & ATTACK_BITMASK) || ++expcnt[channel] == ADSR_exptable[envcnt[channel]])
			if (ADSRstate[channel] and ATTACK_BITMASK) = 0 then Inc(expcnt[channel]);

			if ((ADSRstate[channel] and ATTACK_BITMASK) <> 0) or (expcnt[channel] = ADSR_exptable[envcnt[channel]]) then
			begin
				if (ADSRstate[channel] and HOLDZERO_BITMASK) = 0 then
				begin
					if (ADSRstate[channel] and ATTACK_BITMASK) <> 0 then
					begin
						envcnt[channel] += step;
						if (envcnt[channel] >= $FF) then
						begin
							envcnt[channel] := $FF;
							ADSRstate[channel] := ADSRstate[channel] and ($FF - ATTACK_BITMASK);
						end;
					end
					else
					if ((ADSRstate[channel] and DECAYSUSTAIN_BITMASK) = 0) or
						(envcnt[channel] > ((SR and $F0) + (SR shr 4))) then
					begin
						envcnt[channel] -= step;
						if (envcnt[channel] <= 0) and ((envcnt[channel] + step) <> 0) then
						begin
							envcnt[channel] := 0;
							ADSRstate[channel] := ADSRstate[channel] or HOLDZERO_BITMASK;
						end;
					end;
				end;
				expcnt[channel] := 0;
			end;
		end;
		envcnt[channel] := envcnt[channel] and $FF;

	// ============================================================================================
	// Waveform generation code (phase accumulator and waveform-selector)
	// ============================================================================================

		test := ctrl and TEST_BITMASK;
		wf := ctrl and $F0;
		accuadd := Trunc( (vReg[0] + vReg[1] * 256) * clock_ratio);

		//if test || ((ctrl & SYNC_BITMASK) && sourceMSBrise[num])
		if (test <> 0) or (((ctrl and SYNC_BITMASK) <> 0) and (sourceMSBrise[num] <> 0)) then
			phaseaccu[channel] := 0
		else
		begin
			phaseaccu[channel] += accuadd;
			if (phaseaccu[channel] > $FFFFFF) then
				phaseaccu[channel] -= $1000000;
		end;

		phaseaccu[channel] := phaseaccu[channel] and $FFFFFF;
		MSB := phaseaccu[channel] and $800000;
		sourceMSBrise[num] := BT[MSB > (prevaccu[channel] and $800000)];

		// ========================================================================================
		// Noise
		// ========================================================================================
		if (wf and NOISE_BITMASK) <> 0 then
		begin
			tmp := noise_LFSR[channel];

			// Clock LFSR all time if clockrate exceeds observable at given samplerate
			if (accuadd >= $100000) or ((phaseaccu[channel] and $100000) <> (prevaccu[channel] and $100000)) then
			begin
				step := (tmp and $400000) xor ((tmp and $20000) shl 5);
				// tmp = ((tmp << 1) + (step ? 1 : test)) & 0x7FFFFF;
				if step <> 0 then
					tmp := ((tmp shl 1) + 1) and $7FFFFF
				else
					tmp := ((tmp shl 1) + test) and $7FFFFF;
				noise_LFSR[channel] := tmp;
			end;

			// We simply zero output when other waveform is mixed with noise.
			// On real SID LFSR continuously gets filled by zero and locks up.
			// ($C1 waveform with pw<8 can keep it for a while...)
			if (wf and $70) <> 0 then
				wfout := 0
			else
				wfout := // wfout = (wf & 0x70) ? 0 :
					((tmp and $100000) shr 5) + ((tmp and $40000) shr 4) +
					((tmp and $004000) shr 1) + ((tmp and $00800) shl 1) +
					((tmp and $000200) shl 2) + ((tmp and $00020) shl 5) +
					((tmp and $000004) shl 7) + ((tmp and $00001) shl 8);
		end
		else
		// ========================================================================================
		// Simple pulse wave
		// ========================================================================================
		if (wf and PULSE_BITMASK) <> 0 then
		begin
			pw := (vReg[2] + (vReg[3] and $F) * 256) * 16;
			tmp := Integer(accuadd) shr 9; // ???
			if (0 < pw) and (pw < tmp) then pw := tmp;
			tmp := tmp xor $FFFF;
			if (pw > tmp) then pw := tmp;
			tmp := phaseaccu[channel] shr 8;

			// Simple pulse, most often used waveform, make it sound as clean as possible without oversampling
			if (wf = PULSE_BITMASK) then
			begin
				//step := (accuadd>=255) ? 65535/(accuadd/256.0) : $FFFF;
				if accuadd >= 255 then
					step := Trunc(65535 / (accuadd / 256.0))
				else
					step := $FFFF;

				if test <> 0 then
					wfout := $FFFF
				else
				if (tmp < pw) then
				begin
					// rising edge
					lim := ($FFFF - pw) * step;
					if lim > $FFFF then lim := $FFFF;
					tmp := lim - (pw - tmp) * step;
					if tmp < 0 then
						wfout := 0
					else
						wfout := tmp;
				end
				else
				begin
					// falling edge
					lim := pw * step;
					if (lim > $FFFF) then lim := $FFFF;
					tmp := ($FFFF - tmp) * step - lim;
					if tmp >= 0 then
						wfout := $FFFF
					else
						wfout := tmp;
				end;
			end
			else
			begin
				// Combined pulse
				// ==============

				// (this would be enough for a simple but aliased-at-high-pitches pulse)
				// wfout := (tmp >= pw || test) ? $FFFF:0;
				if (tmp >= pw) or (test <> 0) then
					wfout := $FFFF
				else
					wfout := 0;

				// Pulse+triangle
				if (wf and TRI_BITMASK) <> 0 then
				begin
					// pulse+saw+triangle (waveform nearly identical to tri+saw)
					if (wf and SAW_BITMASK) <> 0 then
					begin
						if wfout <> 0 then
							wfout := CombinedWaveform(num, channel, PulseTriSaw_8580, tmp shr 4, True, vReg[1]);
					end
					else
					begin
						if (ctrl and RING_BITMASK) <> 0 then
							tmp := phaseaccu[channel] xor sourceMSB[num]
						else
							tmp := phaseaccu[channel];

						if wfout <> 0 then
						begin
							if (tmp and $800000) <> 0 then
								wfout := CombinedWaveform(num, channel, PulseSaw_8580,
									(tmp xor $FFFFFF0) shr 11, False, vReg[1])
							else
								wfout := CombinedWaveform(num, channel, PulseSaw_8580,
									(tmp xor 0) shr 11, False, vReg[1]);
						end;
					end
				end
				else
				// pulse+saw
				if (wf and SAW_BITMASK) <> 0 then
					if wfout <> 0 then
						wfout := CombinedWaveform(num, channel, PulseSaw_8580, tmp shr 4, True, vReg[1]);
			end
		end
		else
		// ========================================================================================
		// Saw wave
		// ========================================================================================
		if (wf and SAW_BITMASK) <> 0 then
		begin
			wfout := phaseaccu[channel] shr 8;
			//saw (this row would be enough for simple but aliased-at-high-pitch saw)
			//The anti-aliasing (cleaning) of high-pitched sawtooth wave works by the same principle as mentioned above for the pulse,
			//but the sawtooth has even harsher edge/transition, and as the falling edge gets longer, tha rising edge should became shorter,
			//and to keep the amplitude, it should be multiplied a little bit (with reciprocal of rising-edge steepness).
			//The waveform at the output essentially becomes an asymmetric triangle, more-and-more approaching symmetric shape towards high frequencies.
			//(If you check a recording from the real SID, you can see a similar shape, the high-pitch sawtooth waves are triangle-like...)
			//But for deep sounds the sawtooth is really close to a sawtooth, as there is no aliasing there, but deep sounds should be sharp...
			if (wf and TRI_BITMASK) <> 0 then
				wfout := CombinedWaveform(num, channel, TriSaw_8580, wfout shr 4, True, vReg[1]) //saw+triangle
			else
			begin
				// simple cleaned (bandlimited) saw
				steep := (accuadd / 65536.0) / 288.0;
				wfout += Trunc(wfout * steep);
				if(wfout > $FFFF) then
					wfout := $FFFF - Trunc((wfout - $10000) / steep);
			end;
		end
		else
		// ========================================================================================
		// Triangle wave
		// ========================================================================================
		// (this waveform has no harsh edges, so it doesn't suffer from strong aliasing at high pitches)
		if (wf and TRI_BITMASK) <> 0 then
		begin
			if (ctrl and RING_BITMASK) <> 0 then
				tmp := phaseaccu[channel] xor sourceMSB[num]
			else
				tmp := phaseaccu[channel];

			if (tmp and $800000) <> 0 then
				wfout := (tmp xor $FFFFFF) shr 7
			else
				wfout := tmp shr 7;
		end;

 // ===============================================================================================

		wfout := wfout and $FFFF;
		if (wf <> 0) then
			prevwfout[channel] := wfout
		else
			wfout := prevwfout[channel];

		// Emulate waveform 00 floating wave-DAC (on real SID waveform00 decays after
		// 15s..50s depending on temperature?) (So the decay is not an exact value. Anyway,
		// we just simply keep the value to avoid clicks and support SounDemon digi later...)
		prevaccu[channel] := phaseaccu[channel];
		sourceMSB[num] := MSB;

		Inc(vReg, 7);
		if not Settings.ChannelEnabled[channel] then Continue;

		// Route the channel signal to either the filter or the unfiltered master output
		// depending on filter-switch SID-registers
		filtout := Trunc((Integer(wfout) - $8000) * envcnt[channel] / 256);

		if (sReg[$17] and FILTSW[channel]) <> 0 then
		begin
			filtin[0] += filtout;
			filtin[1] += Trunc( filtout * (1 - Settings.Panning[channel]) );
			filtin[2] += Trunc( filtout * (1 + Settings.Panning[channel]) );
		end
		else
		if ((FILTSW[channel] <> 4)) or ((sReg[$18] and OFF3_BITMASK) = 0) then
		begin
			nonfilt[0] += filtout;
			nonfilt[1] += Trunc( filtout * (1 - Settings.Panning[channel]) );
			nonfilt[2] += Trunc( filtout * (1 + Settings.Panning[channel]) );
		end;

	end; // channel

	// update readable SID1-registers (some SID tunes might use 3rd channel ENV3/OSC3 value as control)
	if (num = 0) and ((memory[BANKSELECT] and 3) <> 0) then
	begin
		// OSC3, ENV3 (some players rely on it)
		sReg[$1B] := wfout shr 8;
		sReg[$1C] := envcnt[3];
	end;

	// FILTER: two integrator loop bi-quadratic filter, workings learned from resid code,
	// but I kind of simplified the equations. The phases of lowpass and highpass outputs are
	// inverted compared to the input, but bandpass IS in phase with the input signal.
	// The 8580 cutoff frequency control-curve is ideal (binary-weighted resistor-ladder VCRs),
	// while the 6581 has a treshold, and below that it outputs a constant ~200Hz cutoff frequency.
	// (6581 uses MOSFETs as VCRs to control cutoff causing nonlinearity and some 'distortion'
	// due to resistance-modulation. There's a cca. 1.53Mohm resistor in parallel with the MOSFET
	// in 6581 which doesn't let the frequency go below 200..220Hz even if the MOSFET doesn't
	// conduct at all. 470pF capacitors are small, so 6581 can't go below this cutoff-frequency
	/// with 1.5MOhm.)
	cutoff[num] := sReg[$16] * 8 + (sReg[$15] and 7);
	if SIDmodel[num] = SID_8580 then
	begin
		cutoff[num] := (1 - Exp((cutoff[num] + 2) * cutoff_ratio_8580)); //linear curve by resistor-ladder VCR
		resonance[num] := (Power(2, ((4 - (sReg[$17] shr 4)) / 8.0)));
	end
	else
	begin //6581
		// MOSFET-VCR control-voltage-modulation (resistance-modulation aka 6581 filter distortion) emulation
		cutoff[num] += Round(filtin[0] * FILTER_DISTORTION_6581); // !!!

		if cutoff[num] <= VCR_FET_TRESHOLD then
			// below Vth treshold Vgs control-voltage FET presents an open circuit
			rDS_VCR_FET := 100000000.0
		else
			// rDS ~ (-Vth*rDSon) / (Vgs-Vth)
			// above Vth FET drain-source resistance is proportional to reciprocal of cutoff-control voltage
			rDS_VCR_FET := cutoff_steepness_6581 / (cutoff[num] - VCR_FET_TRESHOLD);

		cutoff[num] := (1 - Exp(cap_6581_reciprocal / (VCR_SHUNT_6581 * rDS_VCR_FET /
			(VCR_SHUNT_6581 + rDS_VCR_FET)) / samplerate)); //curve with 1.5MOhm VCR parallel Rshunt emulation

		if (sReg[$17] > $5F) then
			resonance[num] := 8.0 / (sReg[$17] shr 4)
		else
			resonance[num] := 1.41;
	end;

	for ch := 1 to 2 do
	begin
		filtout := 0;
		ftmp := filtin[ch] + prevbandpass[num] * resonance[num] + prevlowpass[num];
		if (sReg[$18] and HIGHPASS_BITMASK) <> 0 then filtout -= Trunc(ftmp);

		ftmp := prevbandpass[num] - ftmp * cutoff[num];
		prevbandpass[num] := Trunc(ftmp);
		if (sReg[$18] and BANDPASS_BITMASK) <> 0 then filtout -= Trunc(ftmp);

		ftmp := prevlowpass[num] + ftmp * cutoff[num];
		prevlowpass[num] := Trunc(ftmp);
		if (sReg[$18] and LOWPASS_BITMASK)  <> 0 then filtout += Trunc(ftmp);

		outsample[ch-1] := (nonfilt[ch] + filtout) * (sReg[$18] and $F) / OUTPUT_SCALEDOWN;
	end;

	//output stage for one SID
	//when it comes to $D418 volume-register digi playback, I made an AC/DC separation for $D418 value
	//in SwinSID at low (20Hz or so) cutoff-frequency, and sent the AC (highpass) value to a 4th
	//'digi' channel mixed to the master output, and set ONLY the DC (lowpass) value to the volume-control.
	//This solved 2 issues: Thanks to the lowpass filtering of the volume-control, SID tunes where
	//digi is played together with normal SID channels, won't sound distorted anymore, and the
	//volume-clicks disappear when setting SID-volume.

	//Result := (nonfilt[0] + filtout) * (sReg[$18] and $F) / OUTPUT_SCALEDOWN;
end;


function TSIDPlayer.CombinedWaveform(num, channel: Byte; var wfarray: TWaveformArray;
	index: Integer; differ6581: Boolean; freqh: Byte): Cardinal;
var
	addf: Single;
begin
	addf := 0.6 + 0.4 / freqh;
	if (differ6581) and (SIDmodel[num] = SID_6581) then
		index := index and $7FF
	else
		index := index and $FFF;
	prevwavdata[channel] := Trunc(wfarray[index] * addf + prevwavdata[channel] * (1.0 - addf));
	Result := prevwavdata[channel];
end;


procedure TSIDPlayer.CreateCombinedWaveform(var wfarray: TWaveformArray;
	bitmul, bitstrength, threshold: Single);
var
	i, j, k: Integer;
	bitlevel: Single;
begin
	for i := 0 to 4095 do
	begin
		wfarray[i] := 0;
		for j := 0 to 11 do
		begin
			bitlevel := 0;
			for k := 0 to 11 do
				bitlevel += ( bitmul / Power(bitstrength, Abs(k-j)) ) * (((i shr k) and 1) - 0.5);
			if bitlevel >= threshold then
				wfarray[i] += Trunc(Power(2, j));
		end;
		wfarray[i] *= 12;
	end;
end;


constructor TSIDPlayer.Create;
var
	i: Integer;
begin
	inherited;

	BT[False] := 0; BT[True] := 1;
	for i := 0 to 2 do
		SIDmodel[i] := SID_8580;
	SIDamount := 1;

	cutoff_ratio_8580     := -2 * 3.14 * (12500 / 2048) / samplerate; // approx. 30Hz..12kHz according to datasheet, but only for 6.8nF value, 22nF makes 9Hz...3.7kHz? wrong
	cap_6581_reciprocal   := -1000000 / CAP_6581; // lighten CPU-load in sample-callback
	cutoff_steepness_6581 := FILTER_DARKNESS_6581 * (2048.0 - VCR_FET_TRESHOLD); //pre-scale for 0...2048 cutoff-value range //lighten CPU-load in sample-callback

	CreateCombinedWaveform(TriSaw_8580,      0.8, 2.4, 0.64);
	CreateCombinedWaveform(PulseSaw_8580,    1.4, 1.9, 0.68);
	CreateCombinedWaveform(PulseTriSaw_8580, 0.8, 2.5, 0.64);

	frame_sampleperiod := Trunc(samplerate / PAL_FRAMERATE);
	clock_ratio := C64_PAL_CPUCLK / samplerate;
	{$IFDEF DEBUG}
	Debug('Clock ratio: %f', [clock_ratio]);
	{$ENDIF}
	if clock_ratio > 9 then
	begin
		ADSRperiods[0] := clock_ratio;
		ADSRstep[0] := Ceil(clock_ratio / 9.0);
	end
	else
	begin
		ADSRperiods[0] := 9.0;
		ADSRstep[0] := 1;
	end;

	with Settings.WorkArounds do
	begin
		ADSRDelayBug := True;
		GateFix      := True;
		DynamicCIA   := True;
		BoundsFix    := True;
	end;

	Settings.AutoPlay := True;

	{$IFDEF USE_BASEMENT}
	Audio.RegisterPlayer(TPlayRoutine(Self), 'SID', '*.sid;sid.*');
	{$ENDIF}
end;

destructor TSIDPlayer.Destroy;
begin
	{$IFDEF USE_BASEMENT}
	Audio.UnregisterPlayer(TPlayRoutine(Self));
	{$ENDIF}
	inherited;
end;

function TSIDPlayer.LoadFromFile(const Filename: String): Boolean;
begin
	Result := Load(Filename);
end;

function TSIDPlayer.Load(const Filename: String;
	TuneIndex: Byte = 0; SIDChip: Word = 0): Boolean;
var
	filedata: array of Byte;

	function ReadString(offs, maxlen: Word): String;
	var
		i, strend: Integer;
	begin
		Result := '';
		for i := 0 to maxlen-1 do
		begin
			strend := filedata[offs + i];
			if strend = 0 then
				Break
			else
				Result := Result + Chr(strend);
		end;
	end;

var
	preferred_SID_model: array[0..2] of Boolean = (SID_8580, SID_8580, SID_8580);
	i, datalen, offs, loadaddr: Cardinal;
	Stream: TFileStream;
	version: Byte;
	magic: String;
	sep: Single;

begin
	Result := False;
	if not FileExists(Filename) then Exit;

	Stream := TFileStream.Create(Filename, fmOpenRead);
	try
		datalen := Stream.Size;
		SetLength(filedata, datalen);
		Stream.Seek(0, soFromBeginning);
		Stream.Read(filedata[0], datalen);
	finally
		Stream.Free;
	end;

	magic := ReadString(0, 4);
	version := filedata[5];

	if magic = 'RSID' then
		RSID := True
	else
	if magic = 'PSID' then
		RSID := False
	else
		Exit;

	{$IFDEF DEBUG}
	Debug('File: %s (%d bytes, %s version %d)', [ExtractFilename(filename), datalen, magic, version]);
	{$ENDIF}

	subtune := TuneIndex;

	for i := 0 to 8 do
	begin
		ADSRstate[i] := HOLDZERO_BITMASK;
		envcnt[i] := 0; ratecnt[i] := 0;
		phaseaccu[i] := 0; prevaccu[i] := 0;
		expcnt[i] := 0; prevSR[i] := 0;
		prevwfout[i] := 0;
		noise_LFSR[i] := $7FFFFF;
		Settings.ChannelEnabled[i] := True;
	end;
	for i := 0 to 2 do
	begin
		sourceMSBrise[i] := 0; sourceMSB[i] := 0;
		prevlowpass[i] := 0; prevbandpass[i] := 0;
	end;

	offs := filedata[7];
	if (filedata[8] + filedata[9]) <> 0 then
	begin
		// data doesn't begin with a load address
		loadaddr := filedata[8] * 256 + filedata[9];
		{$IFDEF DEBUG}
		Debug('Offset: $%4.4X, Loadaddress: $%4.4X', [offs, loadaddr]);
		{$ENDIF}
	end
	else
	begin
		// data begins with the load address
		loadaddr := filedata[offs] + filedata[offs + 1] * 256;
		Inc(offs, 2);
		{$IFDEF DEBUG}
		Debug('Offset: $%4.4X, Loadaddress: $%4.4X, Data starts with load address', [offs, loadaddr]);
		{$ENDIF}
	end;

	for i := 0 to 31 do
	begin
		if (filedata[$12 + (i shr 3)] and Trunc(Power(2, 7 - i mod 8)) <> 0) then
		    timermode[31-i] := 1
		else
		    timermode[31-i] := 0;
	end;

	for i := 0 to Length(memory)-1 do
		memory[i] := 0;

	for i := offs to datalen-1 do
	begin
		if (loadaddr + i - offs < Length(memory)) then
			memory[loadaddr + i - offs] := filedata[i];
	end;

	Info.Title  := ReadString($16, 32);
	Info.Author := ReadString($36, 32);
	Info.Info   := ReadString($56, 32);

	if (filedata[$A] + filedata[$B]) <> 0 then
		InitAddress := filedata[$A] * 256 + filedata[$B]
	else
		InitAddress := loadaddr;

	PlayAddress := filedata[$C] * 256 + filedata[$D];
	playaddf := PlayAddress;

	subtune_amount := filedata[$F];
	if subtune > subtune_amount then subtune := 0;

	if subtune = 0 then
		subtune := filedata[$11] - 1
	else
		Dec(subtune); // 1-based to 0-based

	if version >= 2 then
	begin
		preferred_SID_model[0] := ((filedata[$77] and $30) >= $20);
		preferred_SID_model[1] := ((filedata[$77] and $C0) >= $80);
		preferred_SID_model[2] := ((filedata[$76] and 3) >= 3);

		if (filedata[$7A] >= $42) and ((filedata[$7A] < $80) or (filedata[$7A] >= $E0)) then
			SID_address[1] := $D000 + filedata[$7A] * 16
		else
			SID_address[1] := 0;

		if (filedata[$7B] >= $42) and ((filedata[$7B] < $80) or (filedata[$7B] >= $E0)) then
			SID_address[2] := $D000 + filedata[$7B] * 16
		else
			SID_address[2] := 0;

		SIDamount := 1 + BT[SID_address[1] > 0] + BT[SID_address[2] > 0];
	end
	else
	begin
		// PSIDv1 file
		SIDamount := 1;
		memory[$02A6] := 1; // PAL
	end;

	for i := 0 to SIDamount-1 do
	case SIDChip of
		0:    SIDmodel[i] := preferred_SID_model[i];
		6581: SIDmodel[i] := False;
		8580: SIDmodel[i] := True;
	end;

	OUTPUT_SCALEDOWN := 3 * 16 + 26;
	if SIDamount = 2 then
		OUTPUT_SCALEDOWN := Trunc(OUTPUT_SCALEDOWN / 0.6)
	else
	if SIDamount >= 3 then
		OUTPUT_SCALEDOWN := Trunc(OUTPUT_SCALEDOWN / 0.4);

	Reset;

	sep := StereoSeparation / 100;
	for i := 0 to 2 do
	begin
		Settings.Panning[(i*3)+0] := 0.0;
		Settings.Panning[(i*3)+1] := -sep;
		Settings.Panning[(i*3)+2] := +sep;
	end;

	framecnt := 0;
	pPC := 0; addr := 0; storadd := 0;

	{$IFDEF DEBUG}
	Debug('Title:  %s', [Info.title]);
	Debug('Author: %s', [Info.author]);
	Debug('Info:   %s', [Info.info]);
	Debug('Init address: $%4.4X, Play address: $%4.4X', [InitAddress, PlayAddress]);
	Debug('Subtunes:%d, Preferred SID model: %s', [subtune_amount, SIDModelNames[preferred_SID_model[0]]]);
	if SIDamount >= 2 then
		Debug('Multi-SID tune, using %d SID chips', [SIDamount]);
	{$ENDIF}

	{$IFDEF USE_BASEMENT}
	// pass the Render callback routine to the external audio handler in Basement.Audio
	// so we don't need to depend on any specific audio library here.

	Result := Audio.PreparePlayer(Self);
	if (Result) and (Settings.AutoPlay) then
		Audio.Play;
	{$ELSE}
	Result := True;
	{$ENDIF}
end;


end.

