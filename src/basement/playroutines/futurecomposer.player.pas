(*
 * FC14PLAY v1.0 - 3rd of March 2016 - http://16-bits.org
 * ======================================================
 *
 * Very accurate C port of Future Composer 1.4's replayer,
 * by Olav "8bitbubsy" Sørensen, using the original asm source codes + disassembly.
 * Works perfectly with v1.0..v1.3 modules as well.
 *
 * The BLEP and filter routines were coded by aciddose.
 *
 * Delphi port by hukka, Feb/Mar 2016
 * FreePascal port by hukka, Nov 2017
 *)

unit FutureComposer.Player;

interface
{$R-}

uses
	{$I basement-uses_audio.inc}
	SysUtils,
	Basement.Audio,
	FutureComposer.Util,
	FutureComposer.Paula,
	ProTracker.Filters;



const
	SEQ_SIZE           = 13;
	PAT_MAX_SIZE       = 64;
	VOL_TAB_SIZE       = 64;
	FREQ_TAB_SIZE      = 64;
	NUM_SAMPLES        = 10;
	NUM_WAVEFORMS      = 80;
	NUM_WAVEFORMS_SMOD = 47;
	//TRACKTAB_ENTRY_LENGTH   = $000D; // 3*4+1

	SMOD_SONGTAB_OFFSET     = $0064; // 100
	FC14_SMPHEADERS_OFFSET  = $0028; // 40
	FC14_WAVEHEADERS_OFFSET = $0064; // 100
	FC14_SONGTAB_OFFSET     = $00B4; // 180

	PATTERN_LENGTH      = $0040; // 32*2
	PAT_END_MARKER      = $49;
	SEQ_END             = $E1;

	SNDMOD_LOOP         = $E0;
	SNDMOD_END          = SEQ_END;
	SNDMOD_SETWAVE      = $E2;
	SNDMOD_CHANGEWAVE   = $E4;
	SNDMOD_NEWVIB       = $E3;
	SNDMOD_SUSTAIN      = $E8;
	SNDMOD_NEWSEQ       = $E7;
	SNDMOD_SETPACKWAVE  = $E9;
	SNDMOD_PITCHBEND    = $EA;

	ENVELOPE_LOOP       = $E0;
	ENVELOPE_END        = SEQ_END;
	ENVELOPE_SUSTAIN    = $E8;
	ENVELOPE_SLIDE      = $EA;

var
	FCOptions: record
		StereoSeparation: Byte;
		FilterHighPass:   Boolean;  // 5.2Hz high-pass filter present in all Amigas
		FilterLowPass:    Boolean;  // 4.4kHz low-pass filter in all Amigas except A1200
		NormFactor:       Single;   // Sound amplification factor
	end;

type
	TSampleInfo = class
	public
		Length: 	Word; 		// 0: Length in words
		LoopStart:	Word;		// 2: Loop start offset in bytes
		LoopLength:	Word; 		// 4: Loop length in words (1 = no loop)
		IsSSMP: 	Boolean;	//    Data starts with 'SSMP'
		Data: array of ShortInt;
	end;

	TFCChannel = class
	public
		Paula: TPaulaVoice;
		Enabled, vibratoUp, portaDelay, pitchBendDelay, volSlideDelay: Boolean;
		pitchBendValue, pitchBendCounter, note, noteTranspose,
		soundTranspose, volume, periodTranspose: ShortInt;
		portaParam, freqSusCounter, volSusCounter,
		vibratoSpeed, vibratoDepth, vibratoCounter, vibratoDelay,
		volSlideSpeed, volSlideCounter, volDelayCounter, volDelayLength: Byte;
		portaValue: Int16;
		LoopLength, freqTabPos, volTabPos, patPos: Word;
		seqPos: Cardinal;
		freqTabPtr, volTabPtr, patPtr: PByteArray;
		seqStartPtr, LoopStart: PArrayOfShortInt;
	end;

	TFC14Module = class
	private
	public
		fc14: Boolean;

		SEQdata: PByteArray; // sequence data
		PATdata: PByteArray; // pattern data
		FRQdata: PByteArray; // frequency sequence
		VOLdata: PByteArray; // volume sequence

		ID: String;         // 0:  ID mark (SMOD/FC14)
		lnSeq: Word;        // 4:  Length of sequences in bytes (Song length by divide it with 13)
		osPatterns: Word;   // 8:  Offset to patterns
		lnPatterns: Word;   // 12: Length of patterns in bytes
		osFreqSeq: Word;    // 16: Offset to frequency sequence
		lnFreqSeq: Word;    // 20: Length of frequency sequence in bytes
		osVolSeq: Word;     // 24: Offset to volume sequence
		lnVolSeq: Word;     // 28: Length of volume sequence in bytes
		osSamples: Word;    // 32: Offset to sample data
		osWaveTabs: Word;   // 36: Offset to wave tables
		numSequences: Cardinal;

		// 40: Sample info (10 samples, each use 6 bytes)
		Samples: array [0..NUM_SAMPLES + NUM_WAVEFORMS] of TSampleInfo;

		// 100: Wavetables lengths in word (80 bytes)
		// 180: Sequences (13 bytes each)
		// Sequences: array of TSequence;

		Data: RawByteString;

		destructor 	Destroy; override;

		function 	LoadFromFile(const Filename: String): Boolean;
	end;

	TFC14Player = class(TPlayRoutine)
	private
		FilterHi, FilterLo: TLossyIntegrator;
		Blep, BlepVol: array [1..4] of TBlep;

		InitDone:  Boolean;
		spdtemp:   Byte;
		spdtemp2:  Byte;
		respcnt:   Byte;
		repspd:    Byte;
		onoff:     Boolean;
		samplesPerFrame: Cardinal;
		MixBuffer:       array of SmallInt;

		procedure mixSampleBlock(streamOut: Pointer; numSamples: Cardinal);
		procedure play_music;
		procedure doFreqModulation(var ch: TFCChannel);
		procedure doVolModulation(var ch: TFCChannel);
		procedure do_VOLbend(var ch: TFCChannel);
		procedure new_note(var ch: TFCChannel);
		procedure effects(var ch: TFCChannel);

	public
		Channel: array [1..4] of TFCChannel;
		Module: TFC14Module;

		constructor	Create;
		destructor 	Destroy; override;

		function 	Init(outputFreq: Word): Boolean;
		procedure 	Close;
		procedure 	Pause(Pause: Boolean = True);
		procedure 	Play;
		procedure 	SetStereoSeparation(percentage: Byte);

		function	LoadFromFile(const Filename: String): Boolean; override;
		procedure 	Render(Buffer: System.PUInt8; Len: System.DWord; Data: Pointer); override;
	end;


{$IFDEF DEBUG}
procedure Debug(const S: string); overload;
procedure Debug(const S: string; const Args: array of const ); overload;
{$ENDIF}


implementation

uses
	Classes, hkaFileUtils;

var
	Mixing: Boolean;
	samplesLeft: Cardinal;

// ==========================================================================
// Audio API
// ==========================================================================

// Fills Buffer with Len bytes of audio data. Buffer is assumed to be in signed 16-bit format.
//
procedure TFC14Player.Render(Buffer: System.PUInt8; Len: System.DWord; Data: Pointer);
var
	outStream: ^TArrayOfSmallInt absolute Buffer;
	pos, sampleBlock, samplesTodo: Integer;
begin
	FillChar(Buffer^, Len, 0);
	if (Module = nil) or (Mixing) then Exit;

	Mixing := True;
	pos := 0;
	sampleBlock := Len div 4;

	while sampleBlock > 0 do
	begin
		if (sampleBlock < samplesLeft) then
			samplesTodo := sampleBlock
		else
			samplesTodo := samplesLeft;

		if samplesTodo > 0 then
		begin
			MixSampleBlock(@outStream[pos], samplesTodo);
			Inc(pos, samplesTodo * 2);

			Dec(sampleBlock, samplesTodo);
			Dec(samplesLeft, samplesTodo);
		end
		else
		begin
			play_music;
			samplesLeft := samplesPerFrame;
		end;
	end;

	Mixing := False;
end;

{$IFDEF DEBUG}
procedure Debug(const S: string); overload;
begin
	Log(S);
end;

procedure Debug(const S: string; const Args: array of const ); overload;
begin
	Log(Format(S, Args));
end;
{$ENDIF}

// ==========================================================================
// TFC14Module
// ==========================================================================

function TFC14Player.LoadFromFile(const Filename: String): Boolean;
begin
	Result := Module.LoadFromFile(Filename);
	if Result then Init(44100);
end;

function TFC14Module.LoadFromFile(const Filename: string): Boolean;
//var
//	Data: RawByteString;

	procedure ReadBytes(const Source; var Dest; Count: Cardinal);
	begin
		try
			Move(Source, Dest, Count);
		except
		end;
	end;

	(*procedure ReadData(offset: Integer; var Bytes: TByteArray; const DbgStr: String = '');
	var
		p, l: Integer;
	begin
		p := offset + 1;
		offset := GetVal32R(Data, p); // offset to data
		l := GetVal32R(Data, p); // offset + 4: Length of data in bytes
		// get the data
		SetLength(Bytes, l);
		ReadBytes(Data[offset+1], Bytes[0], l);

		{$IFDEF DEBUG}
		if DbgStr <> '' then
			Debug(DbgStr + ': 0x%s : %d bytes', [IntToHex(offset, 8), l]);
		{$ENDIF}
	end;*)

var
	i, L, sd, p, j: Integer;
begin
	{$IFDEF DEBUG}
	Debug('Load: %s', [Filename]);
	{$ENDIF}

	Result := False;
	Data := FileToString(Filename);
	if (Length(Data) < 256) then
		Exit;
	p := 1;

	// "SMOD" and "FC14"
	//
	ID := UpperCase(Copy(Data, 1, 4));

	if (ID <> 'SMOD') and (ID <> 'FC14') then
		Exit
	else
		fc14 := (ID = 'FC14');

	{$IFDEF DEBUG}
	Debug('Format: %s', [ID]);
	{$ENDIF}

	// init samples
	//
	for i := 0 to High(Samples) do
	begin
		Samples[i] := TSampleInfo.Create;
		with Samples[i] do
		begin
			SetLength(Data, 2);
			Data[0] := 0;
			Data[1] := 0;
			Length     := 0;
			LoopStart  := 0;
			LoopLength := 1;
		end;
	end;

	// setup pointers
	//
	// 100/180: Sequences (13 bytes each)
	p := 4 + 1;
	sd := GetVal32R(Data, p); // seqdata size
	lnSeq := sd;
//	SetLength(SEQdata, sd);
	if fc14 then
		p := FC14_SONGTAB_OFFSET + 1
	else
		p := SMOD_SONGTAB_OFFSET + 1;

//	ReadBytes(Data[p], SEQdata[0], sd);
	SEQdata := @Data[p];

	{$IFDEF DEBUG}
	Debug('SEQ: 0x%s : %d bytes', [IntToHex(p, 8), lnSeq]);
	{$ENDIF}

	// 8:  Offset to patterns
	// 12: Length of patterns in bytes
//	ReadData(8, PATdata, 'PAT');
	p := 8 + 1;
	l := GetVal32R(Data, p);
	PATdata := @Data[l+1]; // offset to data


	// 16: Offset to frequency sequence
	// 20: Length of frequency sequence in bytes
//	ReadData(16, FRQdata, 'FRQ');
	p := 16 + 1;
	l := GetVal32R(Data, p);
	FRQdata := @Data[l+1]; // offset to data

	// 24: Offset to volume sequence
	// 28: Length of volume sequence in bytes
//	ReadData(24, VOLdata, 'VOL');
	p := 24 + 1;
	l := GetVal32R(Data, p);
	VOLdata := @Data[l+1]; // offset to data

	// Max. 10 samples (0..9) or 10 sample-packs of 10 samples each.
	// Maximum sample length = 50000.
	// Previously: 32KB.
	// Total: 100000 (gee, old times).
	//
	// Sample length in words, repeat offset in bytes (but even),
	// Repeat length (in words, min=1). But in editor: all in bytes!
	//
	// One-shot sample (*recommended* method):
	//   repeat offset := length*2 (to zero word at end of sample),
	//   replength = 1

	// offset of sample data
	p := 32 + 1;
	sd := GetVal32R(Data, p) + 1;
	// sample infos
	p := FC14_SMPHEADERS_OFFSET + 1;

	for i := 0 to NUM_SAMPLES - 1 do
	begin
		Samples[i].Length     := GetVal16R(Data, p);
		Samples[i].LoopStart  := GetVal16R(Data, p);
		Samples[i].LoopLength := GetVal16R(Data, p);

		L := Samples[i].Length * 2;
		SetLength(Samples[i].Data, L);

		if Samples[i].LoopStart >= L then
			Samples[i].LoopStart := 0;

		// copy sample data
		if L > 0 then
		begin
			ReadBytes(Data[sd], Samples[i].Data[0], L);

			Samples[i].IsSSMP :=
					(Samples[i].Data[0] = Ord('S'))
				and (Samples[i].Data[1] = Ord('S'))
				and (Samples[i].Data[2] = Ord('M'))
				and	(Samples[i].Data[3] = Ord('P'));

			{$IFDEF DEBUG}
			Debug('');
			if Samples[i].IsSSMP then
				Debug('[SSMP %d]', [i])
			else
				Debug('[Sample %d]', [i]);
			Debug('  Length: %d',  [L]);
			Debug('  Repeat: %d',  [Samples[i].LoopStart]);
			Debug('  RepLen: %d',  [Samples[i].LoopLength]);
			Debug('  Offset: %d',  [sd-1]);
			{$ENDIF}

			// fix endless beep on non-looping samples (FC14 doesn't do this)
			//
			if Samples[i].LoopLength <= 1 then
			begin
				Samples[i].LoopLength := 1;
				if (Samples[i].Length >= 1) and (not Samples[i].IsSSMP) then
				begin
					Samples[i].Data[0] := 0;
					Samples[i].Data[1] := 0;
				end;
			end;
		end
		else
			Samples[i].IsSSMP := False;

		Inc(sd, L+2);
	end;

	// Load waveforms.
	// 80 waveforms ($0a-$59), max $100 bytes length each.
	//
	if fc14 then
	begin
		// offset of wave tables
		p := 36 + 1;
		sd := GetVal32R(Data, p) + 1;

		p := FC14_WAVEHEADERS_OFFSET + 1;

		for i := NUM_SAMPLES to NUM_SAMPLES + NUM_WAVEFORMS - 1 do
		begin
			Samples[i].Length := GetVal8(Data, p);
			Samples[i].LoopStart := 0;
			Samples[i].LoopLength := Samples[i].Length;

			// copy sample data
			L := Samples[i].Length * 2;
			if L > 0 then
			begin
				SetLength(Samples[i].Data, L);
				ReadBytes(Data[sd], Samples[i].Data[0], L);
				Inc(sd, L);
			end;
		end;
	end
	else
	begin
        // Old FC has built-in waveforms.
		p := 0;
		for i := 0 to NUM_WAVEFORMS-1 do
		with Samples[NUM_SAMPLES+i] do
		begin
			if i < NUM_WAVEFORMS_SMOD then
			begin
				Length := waveformLengths[i];
				LoopStart  := 0;
				LoopLength := Length;
				L := Length * 2;
				// copy sample data
				if L > 0 then
				begin
					SetLength(Data, L);
					ReadBytes(waveformDatas[p], Data[0], L);
				end;
				Inc(p, L);
			end;
			{else
			begin
				Length := 2;
				LoopStart  := 0;
				LoopLength := Length;
				SetLength(Data, Length * 2 + 1);
				for j := 0 to Length * 2 - 1 do
					Data[j] := 0;
			end;}
		end;
	end;

	// get number of sequences and make it a multiple of 13 (SEQ_SIZE)
	p := 4+1;
	numSequences := GetVal32R(Data, p) div SEQ_SIZE * SEQ_SIZE;

	Result := True;

	{$IFDEF DEBUG}
	Debug('Loaded.');
	{$ENDIF}
end;


// ==========================================================================
// TFC14Player
// ==========================================================================

constructor TFC14Player.Create;
var
	i: Integer;
begin
	inherited Create;

	Module := TFC14Module.Create;

	for i := 1 to 4 do
		Channel[i] := TFCChannel.Create;

	Audio.RegisterPlayer(Self, 'Future Composer', '*.fc;*.fc13;*.fc14;*.fc3;*.fc4;fc.*');
	Audio.PreparePlayer(Self);
	Audio.Init;
end;

destructor TFC14Player.Destroy;
var
	i: Integer;
begin
	Close;
	for i := 1 to 4 do
		Channel[i].Free;

	if Assigned(Module) then
		FreeAndNil(Module);

	inherited Destroy;
end;

function TFC14Player.Init(outputFreq: Word): Boolean;
var
	i: Integer;
begin
	Result := False;
	if not Assigned(Module) then Exit;

	samplesPerFrame := Round((outputFreq / 50.0) + 0.5);
	SetLength(MixBuffer, samplesPerFrame * 2 + 2);
	{$IFDEF DEBUG}
	Debug('Samples per Frame: %d', [samplesPerFrame]);
	{$ENDIF}

	// Initialize channels
	//
	for i := 1 to 4 do
	with Channel[i] do
	begin
		if Paula = nil then
			Paula := TPaulaVoice.Create(outputFreq)
		else
			Paula.Reset;

		Enabled        := True;
		volume         := 0;
		note           := 0;
		LoopStart      := nil;
		LoopLength     := 0;
		volTabPtr      := @silentTable[0];
		freqTabPtr     := @silentTable[0];
		seqPos         := SEQ_SIZE;
		seqStartPtr    := @Module.SEQdata[3 * (i - 1)];
		patPtr         := @Module.PATdata[seqStartPtr[0] * PAT_MAX_SIZE];
		noteTranspose  := seqStartPtr[1];
		soundTranspose := seqStartPtr[2];
		patPos         := 0;
		freqTabPos     := 0;
		freqSusCounter := 0;
		volTabPos      := 0;
		volDelayCounter:= 1;
		volDelayLength := 1;
		VolSusCounter  := 0;
		volSlideSpeed  := 0;
		volSlideCounter:= 0;
		volSlideDelay  := False;
		vibratoCounter := 0;
		vibratoDepth   := 0;
		vibratoDelay   := 0;
		vibratoSpeed   := 0;
		vibratoUp      := False;
		periodTranspose:= 0;
		portaDelay     := False;
		portaValue     := 0;
		portaParam     := 0;
		pitchBendValue := 0;
		pitchBendCounter := 0;
		pitchBendDelay := True;
	end;

	repspd := Module.SEQdata[12];
	if repspd = 0 then
		repspd := 3;
	respcnt  := repspd;
	spdtemp  := 0;
	spdtemp2 := 0;

	SetStereoSeparation(FCOptions.StereoSeparation);

	// Amiga 500 RC low-pass filter (R = 360 ohm, C = 0.1uF)
	// hz = 1 / (2pi * R * C)    = ~4421.Hz
	calcCoeffLossyIntegrator(outputFreq, 4421.0, @FilterLo);

	// Amiga 500 RC high-pass filter (R = 1390 ohm, C = 22uF)
	// hz = 1 / (2pi * R * C)    = ~5.2Hz
	calcCoeffLossyIntegrator(outputFreq, 5.2, @FilterHi);

	{$IFDEF BASS}
	Stream := BASS_StreamCreate(outputFreq, 2, 0, AudioCallback, @Module);

	if Stream = 0 then
	begin
		Log('Error creating audio stream!');
		Exit;
	end;
	if not BASS_ChannelPlay(Stream, True) then
	begin
	    Log('Error starting audio stream playback!');
		Exit;
	end;
	{$ELSE}
	//Audio.Init(AudioCallback_SDL2, outputFreq);
	// TODO error handling
	{$ENDIF}

	InitDone := True;
	Result := True;
end;

destructor TFC14Module.Destroy;
var
	i: Integer;
begin
	for i := 0 to High(Samples) do
		if Assigned(Samples[i]) then
			Samples[i].Free;
	inherited Destroy;
end;

procedure TFC14Player.Close;
begin
	onoff := False;

	{$IFDEF BASS}
	if Stream <> 0 then
		BASS_StreamFree(Stream);
	{$ENDIF}
end;

procedure TFC14Player.Pause(Pause: Boolean);
begin
	onoff := Pause;
end;

procedure TFC14Player.Play;

	procedure ZeroBlep(var blep: TBlep);
	var
		j: Integer;
	begin
		blep.index := 0;
		blep.SamplesLeft := 0;
		blep.LastValue := 0.0;
		for j := 0 to High(blep.Buffer) do
			blep.Buffer[j] := 0.0;
	end;

var
	i: Integer;
begin
	if not InitDone then Exit;

	onoff := not onoff;

	if onoff then
	begin
		for i := 1 to 4 do
		begin
			ZeroBlep(Blep[i]);
			ZeroBlep(BlepVol[i]);
		end;

		clearLossyIntegrator(@FilterLo);
		clearLossyIntegrator(@FilterHi);

		{$IFDEF BASS}
		BASS_ChannelPlay(Stream, False);
		{$ENDIF}
	end
	{$IFDEF BASS}
	else
	BASS_ChannelStop(Stream);
	{$ENDIF}
end;


procedure TFC14Player.SetStereoSeparation(percentage: Byte);

	// these are used to create an equal powered panning
	function SinApx(x: Single): Single;
	begin
		x := x * (2.0 - x);
		Result := x * 1.09742972 + x * x * 0.31678383;
	end;

	function CosApx(x: Single): Single;
	begin
		x := (1.0 - x) * (1.0 + x);
		Result := x * 1.09742972 + x * x * 0.31678383;
	end;

var
	scaledPanPos: Byte;
	p: Single;
begin
	{$IFDEF DEBUG}
	Debug('Player.SetStereoSeparation', []);
	{$ENDIF}

	if percentage > 100 then
		percentage := 100;

	scaledPanPos := Trunc((percentage * 128) / 100);

	p := (128 - scaledPanPos) * (1.0 / 256.0);
	Channel[1].Paula.PanL := CosApx(p);
	Channel[1].Paula.PanR := SinApx(p);
	Channel[4].Paula.PanL := Channel[1].Paula.PanL;
	Channel[4].Paula.PanR := Channel[1].Paula.PanR;

	p := (128 + scaledPanPos) * (1.0 / 256.0);
	Channel[2].Paula.PanL := CosApx(p);
	Channel[2].Paula.PanR := SinApx(p);
	Channel[3].Paula.PanL := Channel[2].Paula.PanL;
	Channel[3].Paula.PanR := Channel[2].Paula.PanR;

	FCOptions.StereoSeparation := percentage;
end;

procedure TFC14Player.new_note(var ch: TFCChannel);
var
	tmpSeqPtr, tmpPatPtr, ptr8u_1: PByteArray;
	note, info: Byte;
	O: Integer;
begin
	tmpPatPtr := @ch.patPtr[ch.patPos];

	if (Module.fc14 and ((tmpPatPtr[0] and $7F) = PAT_END_MARKER)) or (ch.patPos = PAT_MAX_SIZE) then
	begin
		ch.patPos := 0;

		if ch.seqPos >= Module.numSequences then
			ch.seqPos := 0;

		tmpSeqPtr := @ch.seqStartPtr[ch.seqPos];
		ch.patPtr := @Module.PATdata[tmpSeqPtr[0] * PAT_MAX_SIZE];

		ch.noteTranspose  := ch.seqStartPtr[ch.seqPos+1]; //tmpSeqPtr[1];
		ch.soundTranspose := ch.seqStartPtr[ch.seqPos+2]; //tmpSeqPtr[2];

		Inc(spdtemp);
		if spdtemp >= 4 then
		begin
			// we've read all channels now, let's increase the pos used for RS
			spdtemp := 0;
			Inc(spdtemp2);
			if spdtemp2 >= (Module.numSequences div SEQ_SIZE) then // numSequences is a multiple of SEQ_SIZE
				spdtemp2 := 0; // wrap sequence position
		end;

		// read current RS (replay speed. only update if non-zero)
		if Module.SEQdata[(spdtemp2 * 13) + 12] <> 0 then
		begin
			repspd  := Module.SEQdata[(spdtemp2 * 13) + 12];
			respcnt := repspd;
		end;

		Inc(ch.seqPos, SEQ_SIZE);
		tmpPatPtr := ch.patPtr; // set new temp pattern pointer ???????
	end;

	note := tmpPatPtr[0];
	info := tmpPatPtr[1];

	if note = 0 then
	begin
		info := info and $C0;
		if info <> 0 then
		begin
			ch.portaParam := 0;
			if (info and (1 shl 7)) <> 0 then
				ch.portaParam := tmpPatPtr[3];
		end;
	end
	else
	begin
		ch.portaValue := 0;
		ch.portaParam := 0;
		if (info and (1 shl 7)) <> 0 then
			ch.portaParam := tmpPatPtr[3];
	end;

	note := note and $7F;
	if note <> 0 then
	begin
		ch.note := note;

		{O := (((tmpPatPtr[1] and $3F) + ch.soundTranspose) and $3F) * VOL_TAB_SIZE;
		O := O mod Length(Module.VOLdata);
		if (O < 0) or (O >= Length(Module.VOLdata)) then
			O := 0;
		ptr8u_1 := @Module.VOLdata[O];}

		ptr8u_1 := @Module.VOLdata[(((tmpPatPtr[1] and $3F) + ch.soundTranspose) and $3F) * VOL_TAB_SIZE];
		ch.volDelayLength  := ptr8u_1[0];
		ch.volDelayCounter := ch.volDelayLength;
		ch.freqTabPtr      := @Module.FRQdata[(ptr8u_1[1] and $3F) * FREQ_TAB_SIZE];
		ch.freqTabPos      := 0;
		ch.freqSusCounter  := 0;
		ch.vibratoSpeed    := ptr8u_1[2];
		ch.vibratoDepth    := ptr8u_1[3];
		ch.vibratoDelay    := ptr8u_1[4];
		ch.vibratoCounter  := ch.vibratoDepth;
		ch.vibratoUp       := True; // default initial state on new note
		ch.volTabPtr       := @ptr8u_1[5];
		ch.volTabPos       := 0;
		ch.volSusCounter   := 0;

		ch.Paula.StopDMA; // yes, this is important
	end;

	Inc(ch.patPos, 2);
end;

procedure TFC14Player.doFreqModulation(var ch: TFCChannel);
var
	tabPtr: PByteArray;
	doTranspose: Boolean;
	Sample: TSampleInfo;
	cp: Cardinal;
	cpos: Word;
label
	testsustain,
	testeffects;
begin

testsustain:
	if ch.freqSusCounter > 0 then
		Dec(ch.freqSusCounter)
	else
	begin
		tabPtr := @ch.freqTabPtr[ch.freqTabPos];

testeffects:
		if tabPtr[0] <> SNDMOD_END then
		begin
			doTranspose := True;

			if tabPtr[0] = SNDMOD_LOOP then // freq pos jump
			begin
				//Debug('FX: E0 freq pos jump');
				ch.freqTabPos := tabPtr[1] and $3F;
				tabPtr := @ch.freqTabPtr[ch.freqTabPos];
			end;

			case tabPtr[0] of

				SNDMOD_SETWAVE: // set waveform
				begin
					if tabPtr[1] < (NUM_SAMPLES + NUM_WAVEFORMS) then
					begin
						Sample := Module.Samples[tabPtr[1]];

						ch.LoopStart  := @Sample.Data[Sample.LoopStart];
						ch.LoopLength := Sample.LoopLength;

						ch.Paula.SetData(@Sample.Data[0]);
						ch.Paula.SetLength(Sample.Length);
						ch.Paula.StartDMA;

						ch.volTabPos := 0;
						ch.volDelayCounter := 1;
						Inc(ch.freqTabPos, 2);
					end;
				end;

				SNDMOD_CHANGEWAVE: // update waveform
				begin
					if tabPtr[1] < (NUM_SAMPLES + NUM_WAVEFORMS) then
					begin
						//Debug('FX: E4 update waveform');
						Sample := Module.Samples[tabPtr[1]];

						ch.LoopStart  := @Sample.Data[Sample.LoopStart];
						ch.LoopLength := Sample.LoopLength;

						ch.Paula.SetData(@Sample.Data[0]);
						ch.Paula.SetLength(Sample.Length);
					end;
					Inc(ch.freqTabPos, 2);
				end;

				SNDMOD_SETPACKWAVE: // set packed waveform
				begin
					if tabPtr[1] < (NUM_SAMPLES + NUM_WAVEFORMS) then
					begin
						// Debug('FX: E9 set packed waveform');
						Sample := Module.Samples[tabPtr[1]];

						if Sample.IsSSMP then
						begin
							// cast a part of sample data to uint32
							cpos := 4 + (tabPtr[2] * 16);
							cp := GetPtr32(@Sample.Data[cpos]);

							ch.LoopStart  := @Sample.Data[(4 + 320 + cp) + GetPtr16(@Sample.Data[cpos+6])];
							ch.LoopLength := GetPtr16(@Sample.Data[cpos+8]);

							// fix endless beep on non-looping samples (FC14 doesn't do this)
							if ch.LoopLength <= 1 then
							begin
								ch.loopLength := 1;
								Sample.Data[4 + 320 + cp + 0] := 0;
								Sample.Data[4 + 320 + cp + 1] := 0;
							end;

							ch.Paula.SetData(@Sample.Data[4 + 320 + cp]);
							ch.Paula.SetLength(GetPtr16(@Sample.Data[cpos+4]));
							ch.Paula.StartDMA;

							ch.volTabPos := 0;
							ch.volDelayCounter := 1;
						end;
					end;
					Inc(ch.freqTabPos, 3);
				end;

				SNDMOD_NEWSEQ: // new freq pos
				begin
					tabPtr := @Module.FRQdata[(tabPtr[1] and $3F) * FREQ_TAB_SIZE];
					ch.freqTabPtr := tabPtr;
					ch.freqTabPos := 0;
					goto testeffects;
				end;

				ENVELOPE_SUSTAIN: // set freq sustain (FC1.4 only)
				begin
					// Debug('FX: E8 set freq sustain');
					ch.freqSusCounter := tabPtr[1];
					Inc(ch.freqTabPos, 2);
					goto testsustain;
				end;

				SNDMOD_NEWVIB: // set vibrato
				begin
					// Debug('FX: E3 set vibrato');
					ch.vibratoSpeed := tabPtr[1];
					ch.vibratoDepth := tabPtr[2];
					Inc(ch.freqTabPos, 3);
					doTranspose := False; // don't do period transpose here
				end;

				ENVELOPE_SLIDE: // set pitch bend
				begin
					//Debug('FX: EA pitch bend  [1]=%d  [2]=%d', [Integer(tabPtr[1]), Integer(tabPtr[2])]);
					ch.pitchBendValue   := ShortInt(tabPtr[1]);
					ch.pitchBendCounter := ShortInt(tabPtr[2]);
					Inc(ch.freqTabPos, 3);
				end;

			end;

			if doTranspose then
			begin
				ch.periodTranspose := ShortInt(ch.freqTabPtr[ch.freqTabPos]);
				Inc(ch.freqTabPos);
			end;
		end;
	end;
end;

procedure TFC14Player.do_VOLbend(var ch: TFCChannel);
begin
	ch.volSlideDelay := not ch.volSlideDelay;
	if ch.volSlideDelay then
	begin
		Dec(ch.volSlideCounter);
		Inc(ch.volume, ch.volSlideSpeed);
		if ch.volume < 0 then
		begin
			ch.volSlideCounter := 0;
			ch.volume := 0;
		end;
	end;
end;

procedure TFC14Player.doVolModulation(var ch: TFCChannel);
var
	tabPtr: PByteArray;
label
	VOLUfx,
	volu_cmd;
begin
VOLUfx:

	if ch.volSusCounter > 0 then
		Dec(ch.volSusCounter)
	else
	begin
		if ch.volSlideCounter > 0 then
			do_VOLbend(ch)
		else
		begin
			if ch.volDelayCounter > 0 then
				Dec(ch.volDelayCounter);
			if ch.volDelayCounter = 0 then
			begin
				ch.volDelayCounter := ch.volDelayLength;
volu_cmd:
				tabPtr := @ch.volTabPtr[ch.volTabPos];

				if tabPtr^[0] <> ENVELOPE_END then
				begin

					if tabPtr[0] = ENVELOPE_SUSTAIN then // set vol sustain
					begin
						ch.volSusCounter := tabPtr[1];
						Inc(ch.volTabPos, 2);
						goto VOLUfx;
					end
					else
					if tabPtr[0] = ENVELOPE_SLIDE then // set vol slide
					begin
						ch.volSlideSpeed   := tabPtr[1];
						ch.volSlideCounter := tabPtr[2];
						Inc(ch.volTabPos, 3);
						do_VOLbend(ch);
					end
					else
					if tabPtr[0] = ENVELOPE_LOOP then // set vol pos
					begin
						if ((tabPtr[1] and $3F) - 5) < 0 then
							ch.volTabPos := 0
						else
							ch.volTabPos := (tabPtr[1] and $3F) - 5;
						goto volu_cmd;
					end
					else
					begin
						ch.volume := ShortInt(tabPtr[0]);
						Inc(ch.volTabPos);
					end;

				end;
			end;
		end;
	end;
end;

procedure TFC14Player.effects(var ch: TFCChannel);
var
	tmpNote: ShortInt;
	tmpVibPeriod, tmpPeriod: SmallInt;
	tmpVibNote: Word;
begin
	doFreqModulation(ch);
	doVolModulation(ch);

	// get period from note and transposes...
	tmpNote := ch.periodTranspose;
	if tmpNote >= 0 then
	begin
		Inc(tmpNote, ch.note);
		Inc(tmpNote, ch.noteTranspose);
	end;
	tmpNote := tmpNote and $7F;
	tmpPeriod := periods[Byte(tmpNote)];

	// apply vibrato to period
	if ch.vibratoDelay > 0 then
		Dec(ch.vibratoDelay)
	else
	begin
		tmpVibPeriod := ch.vibratoCounter;
		if not ch.vibratoUp then
		begin
			Dec(tmpVibPeriod, ch.vibratoSpeed);
			if tmpVibPeriod < 0 then
			begin
				tmpVibPeriod := 0;
				ch.vibratoUp := True;
			end;
		end
		else
		begin
			Inc(tmpVibPeriod, ch.vibratoSpeed);
			if tmpVibPeriod > (ch.vibratoDepth * 2) then
			begin
				tmpVibPeriod := ch.vibratoDepth * 2;
				ch.vibratoUp := False;
			end;
		end;
		ch.vibratoCounter := tmpVibPeriod and $00FF;

		Dec(tmpVibPeriod, ch.vibratoDepth);

		tmpVibNote := tmpNote * 2;
		while tmpVibNote < (12 * 8) do
		begin
			tmpVibPeriod := tmpVibPeriod * 2;
			Inc(tmpVibNote, (12 * 2));
		end;

		Inc(tmpPeriod, tmpVibPeriod);
	end;

	// update portamento value (twice as slow on FC1.4)
	ch.portaDelay := not ch.portaDelay;
	if (not Module.fc14) or (ch.portaDelay) then
	if ch.portaParam > 0 then
	begin
		if ch.portaParam > $1F then
			Inc(ch.portaValue, (ch.portaParam and $1F))
		else
			Dec(ch.portaValue, ch.portaParam);
	end;

	// apply pitch bend to portamento value
	ch.pitchBendDelay := not ch.pitchBendDelay;
	if ch.pitchBendDelay then
	begin
		if ch.pitchBendCounter > 0 then
		begin
			Dec(ch.pitchBendCounter);
			Dec(ch.portaValue, ch.pitchBendValue);
		end;
	end;

	Inc(tmpPeriod, ch.portaValue);

	if Module.fc14 then
		ch.Paula.SetPeriod(CLAMP(tmpPeriod, $0071, $0D60))
	else
		ch.Paula.SetPeriod(CLAMP(tmpPeriod, $0071, $06B0));
	ch.Paula.SetVolume(CLAMP(ch.volume, 0, 64));
end;

procedure TFC14Player.play_music;
var
	i: Integer;
begin
	Dec(respcnt);
	if respcnt = 0 then
	begin
		respcnt := repspd;
		for i := 1 to 4 do
			new_note(Channel[i]);
	end;

	for i := 1 to 4 do
	begin
		effects(Channel[i]);
		with Channel[i] do
		begin
			Paula.SetData(LoopStart);
			Paula.SetLength(LoopLength);
		end;
	end;
end;

procedure TFC14Player.mixSampleBlock(streamOut: Pointer; numSamples: Cardinal);
var
	i: Integer;
	j: Word;
	sndOut: ^TArrayOfSmallInt absolute streamOut;
	Amp, tempSample, tempVolume: Single;
	outSample: array [0..1] of Single;
	masterBufferL, masterBufferR: array of Single;
	v: TPaulaVoice;
	bSmp, bVol: PBlep;
const
	SCALER = 1.0 / 128.0;
begin
	Amp := -32767.0 / FCOptions.NormFactor; // negative because signal phase is flipped on Amiga

	SetLength(masterBufferL, numSamples + 2);
	SetLength(masterBufferR, numSamples + 2);

	for i := 1 to 4 do
	begin
		v := Channel[i].Paula;

		if (Channel[i].Enabled) and (v.Active) and (v.Length >= 2) and (v.Data <> nil) then
		begin
			bSmp := @blep[i];
			bVol := @blepVol[i];

			for j := 0 to numSamples-1 do
			begin
				tempSample := v.Data[v.Position] * SCALER;
				tempVolume := v.Volume;

				// Blep reduces some unwanted aliasing (closer to real Amiga)
				if tempSample <> bSmp.LastValue then
				begin
					if ((v.LastDelta > 0.0) and (v.LastDelta > v.LastPhase)) then
						BlepAdd(bSmp, v.LastPhase / v.LastDelta, bSmp.lastValue - tempSample);
					bSmp.lastValue := tempSample;
				end;
				if tempVolume <> bVol.LastValue then
				begin
					BlepAdd(bVol, 0.0, bVol.LastValue - tempVolume);
					bVol.LastValue := tempVolume;
				end;
				if bSmp.samplesLeft > 0 then
					tempSample := tempSample + BlepRun(bSmp);
				if bVol.samplesLeft > 0 then
					tempVolume := tempVolume + BlepRun(bVol);

				tempSample := tempSample * tempVolume;
				masterBufferL[j] := masterBufferL[j] + (tempSample * v.PanL);
				masterBufferR[j] := masterBufferR[j] + (tempSample * v.PanR);

				v.Phase := v.Phase + v.Delta;
				if v.Phase >= 1.0 then
				begin
					v.Phase := v.Phase - 1.0;
					v.LastPhase := v.Phase;
					v.LastDelta := v.Delta;

					Inc(v.Position);
					if v.Position >= v.Length then
					begin
						// re-fetch Paula register values now
						v.Position := 0;
						v.Length := v.NewLength;
						v.Data := v.NewData;
					end;
				end;
			end;
		end;
	end;

	for j := 0 to numSamples-1 do
	begin
		outSample[0] := masterBufferL[j];
		outSample[1] := masterBufferR[j];

		if FCOptions.FilterLowPass then
			LossyIntegrator(@FilterLo, @outSample[0], @outSample[0]);
		{if FCOptions.LEDStatus then
			LossyIntegratorLED(@FilterLEDC, @FilterLED, @outSample[0], @outSample[0]);}
		if FCOptions.FilterHighPass then
			LossyIntegratorHighPass(@FilterHi, @outSample[0], @outSample[0]);

		outSample[0] := outSample[0] * Amp;
		outSample[1] := outSample[1] * Amp;

		if (outSample[0] < -32768.0) then
			outSample[0] := -32768.0
		else
		if (outSample[0] >  32767.0) then
			outSample[0] :=  32767.0;
		if (outSample[1] < -32768.0) then
			outSample[1] := -32768.0
		else
		if (outSample[1] >  32767.0) then
			outSample[1] :=  32767.0;

		sndOut[j*2]   := Trunc(outSample[0]);
		sndOut[j*2+1] := Trunc(outSample[1]);
	end;
end;

initialization

	FCOptions.NormFactor := 4.20;
	FCOptions.StereoSeparation := 12;
	FCOptions.FilterLowPass  := True;
	FCOptions.FilterHighPass := True;

end.

