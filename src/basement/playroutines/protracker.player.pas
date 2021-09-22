(*
 * Based on PT2PLAY v1.3b by Olav "8bitbubsy" Sørensen - http://16-bits.org
 * BLEP (Band-Limited Step) and filter routines by aciddose
 * FreePascal 3.x port for use with Basement framework by hukka, 2016-2018
 *)

unit ProTracker.Player;

interface

uses
	{$I basement-uses_audio.inc}
	SysUtils, Classes,
	FileStreamEx,
	Generics.Collections,
	Basement.Util,
	Basement.Audio,
	ProTracker.Util,
	ProTracker.Messaging,
	ProTracker.Sample,
	ProTracker.Filters;


const
	// callback events
	AUDIOEVENT_PLAY    = 1;
	AUDIOEVENT_SAMPLE  = 2;
	AUDIOEVENT_ROW     = 3;
	AUDIOEVENT_ORDER   = 4;
	AUDIOEVENT_FILTER  = 5;

    OFFSET_SONGTITLE	= 0;
	OFFSET_SAMPLEINFO	= 20;
	OFFSET_ORDERLIST	= 952;
	OFFSET_ID			= 1080;
	OFFSET_PATTERNS		= 1084;

	MODFILESIZE_MIN		=    2108;
	MODFILESIZE_MAX		= 4195326;

	MAX_PATTERNS		= 100;
	AMOUNT_CHANNELS		= 4;

	PLAY_STOPPED		= 0;
	PLAY_SONG			= 1;
	PLAY_PATTERN		= 2;

	PAULA_PAL_CLK 		= 3546895;
	CIA_PAL_CLK   		= 709379;

	// messages
	MSG_TIMERTICK	= 10;
	MSG_VUMETER		= 11;
	MSG_ROWCHANGE	= 20;
	MSG_ORDERCHANGE	= 21;

var
	PTOptions: record
		Frequency: 				Word;		// 11025/22050/44100/48000
		Buffer: 				Integer;	// audio buffer in milliseconds, 0 = detect minimum
		Amplification: 			Single;		// default: 3.25
		StereoSeparation: 		Byte;		// 0..100%, 0=mono
		FilterLowPass: 			Boolean;
		FilterHighPass: 		Boolean;
		FilterLed: 				Boolean;
		CIAmode: 				Boolean;	// tempo mode, False=VBlank, True=CIA
		EditorInvertLoop: 		Boolean;	// play EFx command like in PT editor?
		EnableKarplusStrong:	Boolean;	// enable E8x Karplus-Strong effect?
	end;

type
	TModuleEvent   		= procedure (EventType, Data1, Data2: Byte) of Object;
	TSpeedChangeEvent	= procedure (Speed, Tempo: Byte) of Object;
	TProgressEvent 		= procedure (Progress: Cardinal) of Object;
	TProgressInitEvent 	= procedure (Max: Cardinal; const Title: String) of Object;
	TModifiedEvent 		= procedure (B: Boolean = True; Force: Boolean = False) of Object;

	TModuleFormat = (
		FORMAT_MK,     // ProTracker 1.x
		FORMAT_MK2,    // ProTracker 2.x (if tune has >64 patterns)
		FORMAT_FLT4,   // StarTrekker
		FORMAT_4CHN,   // FastTracker II (only 4 channel MODs)
		FORMAT_STK,    // The Ultimate SoundTracker (15 samples)
		FORMAT_NT,     // NoiseTracker 1.0
		FORMAT_FEST,   // NoiseTracker (special one)
		FORMAT_UNKNOWN
	);

	TNote = packed record
		Sample:			Byte;	// 8 bits
		Command:		Byte;	// 4 bits
		Parameter:		Byte;	// 8 bits
		Pitch:			Byte;	// index to NoteText[]
		//Period:		Word;	// 12 bits (Period)
	end;
	PNote = ^TNote;

	TSongPosition = record
		Pattern,
		Row,
		Order:	Byte;
	end;

	// ============================================================================================
	// Paula emulation
	// ============================================================================================

	TPaulaVoice = class
	public
		Enabled,
		HasSound: Boolean;

		SRC_DAT: PArrayOfShortInt;
		SRC_LEN: uint32;
		SRC_VOL: Single;

		DMA_DAT: PArrayOfShortInt;
		DMA_LEN: uint32;
		DMA_POS: uint32;

		f_OutputFreq: Single;
		PANL, PANR:   Single;

		DELTA, LASTDELTA: Single;
		FRAC, LASTFRAC:   Single;

		Volume: Single;

		Sample,
		QueuedSample: SmallInt;
		QueuedOffset: Cardinal;
		PlayPos:      Integer;

		constructor Create(OutputFreq: Word);

		procedure Kill;
		procedure TurnOffDMA; inline;
		procedure RestartDMA;
		procedure SetData(NewSample: Byte; NewOffset: Integer); inline;
		procedure SetDataPtr(const src: PArrayOfShortInt); inline;
		procedure SetLength(len: Cardinal); inline;
		procedure SetPeriod(period: Word); inline;
		procedure SetVolume(vol: Word);
	end;

	// ============================================================================================
	// ProTracker Module
	// ============================================================================================

	TOrderList = record
	private
		FItems: 	packed array [0..127] of Byte;
		function	GetItem(Index: Byte): Byte;
		procedure	SetItem(Index: Byte; const Value: Byte);
	public
		function 	GetHighestUsed: Byte;
		procedure	Insert(Index: Byte; const Value: Byte);
		procedure	Delete(Index: Byte);
		property 	Items[Index: Byte]: Byte read GetItem write SetItem; default;
	end;

	TPTChannel = class
	private
		FEnabled: Boolean;
	public
		Paula: 			TPaulaVoice;
		Note:			PNote;
		n_start,
		n_wavestart,
		n_loopstart:	Integer;
		n_index,
		n_volume,
		n_toneportdirec,
		n_vibratopos,
		n_tremolopos,
		n_pattpos,
		n_loopcount:	ShortInt;
		n_wavecontrol,
		n_glissfunk,
		n_sampleoffset,
		n_toneportspeed,
		n_vibratocmd,
		n_tremolocmd,
		n_finetune,
		n_funkoffset:	Byte;
		n_sample,
		n_period,
		n_note,
		n_wantedperiod:	SmallInt;
		n_length,
		n_replen,
		n_repend: 		Cardinal;

		procedure 	Reset;
		procedure	SetEnabled(E: Boolean);

		constructor Create(i: Byte);
		destructor  Destroy; override;
	property
		Enabled:	Boolean read FEnabled write SetEnabled;
	end;

	TPTModule = class(TPlayRoutine)
	private
		StereoSeparation:	Byte;
		FilterHi, FilterLo:	TLossyIntegrator;
		FilterLEDC: 		TLedFilterCoeff;
		FilterLED: 			TLedFilter;
		LEDStatus: 			Boolean;
		Blep, BlepVol: 		array [0..AMOUNT_CHANNELS-1] of TBlep;
		SetBPMFlag:			Byte;

		DisableMixer: 		Boolean;
		samplesPerFrame: 	Cardinal;
		MixBuffer: 			array of SmallInt;

		procedure 	ClearRowVisitTable;
		procedure 	FindDefaultTempo(GotSpeed, GotTempo: Boolean);

		procedure	MixSampleBlock(streamOut: Pointer; numSamples: Cardinal);
		procedure 	CalculatePans(percentage: Byte);

		procedure	UpdateFunk(var ch: TPTChannel);
		procedure	SetGlissControl(var ch: TPTChannel);
		procedure 	SetVibratoControl(var ch: TPTChannel);
		procedure 	SetFineTune(var ch: TPTChannel);
		procedure 	JumpLoop(var ch: TPTChannel);
		procedure 	SetTremoloControl(var ch: TPTChannel);
		procedure 	KarplusStrong(var ch: TPTChannel);
		procedure 	DoRetrg(var ch: TPTChannel);
		procedure 	RetrigNote(var ch: TPTChannel);
		procedure 	VolumeSlide(var ch: TPTChannel);
		procedure 	VolumeFineUp(var ch: TPTChannel);
		procedure 	VolumeFineDown(var ch: TPTChannel);
		procedure 	NoteCut(var ch: TPTChannel);
		procedure 	NoteDelay(var ch: TPTChannel);
		procedure 	PatternDelay(var ch: TPTChannel);
		procedure 	FunkIt(var ch: TPTChannel);
		procedure 	PositionJump(var ch: TPTChannel);
		procedure 	VolumeChange(var ch: TPTChannel);
		procedure 	PatternBreak(var ch: TPTChannel);
		procedure 	Arpeggio(var ch: TPTChannel);
		procedure 	PortaUp(var ch: TPTChannel);
		procedure 	PortaDown(var ch: TPTChannel);
		procedure 	FilterOnOff(var ch: TPTChannel);
		procedure 	FinePortaUp(var ch: TPTChannel);
		procedure 	FinePortaDown(var ch: TPTChannel);
		procedure 	SetTonePorta(var ch: TPTChannel);
		procedure 	TonePortNoChange(var ch: TPTChannel);
		procedure 	TonePortamento(var ch: TPTChannel);
		procedure 	VibratoNoChange(var ch: TPTChannel);
		procedure 	Vibrato(var ch: TPTChannel);
		procedure 	TonePlusVolSlide(var ch: TPTChannel);
		procedure 	VibratoPlusVolSlide(var ch: TPTChannel);
		procedure 	Tremolo(var ch: TPTChannel);
		procedure 	SampleOffset(var ch: TPTChannel);
		procedure 	E_Commands(var ch: TPTChannel);
		procedure 	CheckMoreEffects(var ch: TPTChannel);
		procedure 	CheckEffects(var ch: TPTChannel);
		procedure 	SetPeriod(var ch: TPTChannel);
		procedure 	SetReplayerBPM(bpm: Byte);

		procedure 	SetOutputFreq(Hz: Cardinal);

	public
		RenderInfo: record
			SamplesRendered: 	Int64;
			LastRow:			Byte;
			RowsRendered:		Byte;
			OrderChanges: 		Word;
			TimesLooped,
			LoopsWanted: 		Byte;
			Canceled,
			HasBeenPlayed: 		Boolean;
			RowVisitTable: 		array [0..MAX_PATTERNS-1, 0..63] of Boolean;
		end;

		{$IFDEF BASS}
		Stream: 		HSTREAM;
		{$ENDIF}
		Notes: 			array [0..MAX_PATTERNS, 0..AMOUNT_CHANNELS-1, 0..63] of TNote; // pattern - track - row
		Channel:		array [0..AMOUNT_CHANNELS-1] of TPTChannel;
		Samples: 		TObjectList<TSample>; // array [0..30]  of TSample;
		OrderList:		TOrderList;
		PlayMode:		Byte;
		Warnings:		Boolean;
		ClippedSamples: Integer;
		SampleChanged:  packed array[0..31] of Boolean;

		DefaultSpeed,
		DefaultTempo:	Byte;

		Info: record
			Format:			TModuleFormat;
			ID:				packed array [0..3]  of AnsiChar;
			Title:			packed array [0..19] of AnsiChar;
			RestartPos: 	Byte;
			OrderCount,
			PatternCount,
			Speed,
			BPM: 			Word;
			Filesize:		Cardinal;
			Filename:		String;
		end;

		FilterHighPass: 	Boolean; 	// 5.2Hz high-pass filter present in all Amigas
		FilterLowPass: 		Boolean; 	// 4.4kHz low-pass filter in all Amigas except A1200
		PreventClipping: 	Boolean; 	// Clamps the audio output to prevent clipping
		NormFactor: 		Single; 	// Sound amplification factor

		Modified,
		PosJumpAssert,
		PBreakFlag: 		Boolean;

		VBlankMode: 		Boolean;	// False = CIA, True = VBlank
		PBreakPosition: 	ShortInt;
		PattDelTime,
		PattDelTime2: 		ShortInt;

		LowMask,
		Counter,
		CurrentSpeed,
		CurrentBPM:			Byte;
		soundBufferSize: 	Integer;

		PlayPos:			TSongPosition;

		OnFilter,
		OnSamplePlay,
		OnPlayModeChange,
		OnRowChange,
		OnOrderChange:		TModuleEvent;
		OnSpeedChange:		TSpeedChangeEvent;

		OnProgressInit:		TProgressInitEvent;
		OnProgress:			TProgressEvent;
		OnModified: 		TModifiedEvent;

		PlayStarted:		TDateTime;

		RenderMode:			(RENDER_NONE, RENDER_LENGTH, RENDER_SAMPLE, RENDER_FILE);

		procedure 	RepostChanges;
		procedure 	SetModified(B: Boolean = True; Force: Boolean = False);

		function 	LoadFromStream(Stream: TStream): Boolean;
		function 	LoadFromFile(const Filename: String{; Force: Boolean = False}): Boolean; override;
		function	InternalLoad(var ModFile: TStreamEx): Boolean;

		function 	SaveToFile(const Filename: String): Boolean;
		procedure 	Render(Buffer: System.PUInt8; Len: System.DWord; Data: Pointer); override;

		function 	IsPatternEmpty(i: Byte): Boolean;
		function 	CountUsedPatterns: Byte;
		procedure 	IndexSamples;

		procedure 	SetTitle(const S: AnsiString);
		procedure 	SetSpeed(NewSpeed: Byte; DoChange: Boolean = True);
		procedure 	SetTempo(bpm: Word);

		procedure 	Reset;
		procedure 	InitPlay(pattern: Byte);
		procedure 	PlayPattern(pattern: Byte; row: Byte = 0);
		procedure 	Play(order: Byte = 0; row: Byte = 0);
		procedure 	Stop;
		procedure 	Pause(Pause: Boolean = True);
		procedure 	Close;

		procedure 	PlayNote(Note: PNote; Chan: Byte; Vol: Byte = 255);
		procedure 	PlaySample(_note, _sample, _channel: Byte;
					_volume: ShortInt = -1; _start: Integer = 0; _length: Integer = 0);
		procedure 	PlayVoice(var ch: TPTChannel);

		procedure 	NextPosition(FromEditor: Boolean = False);
		procedure 	IntMusic;

		procedure 	ApplyAudioSettings;

		constructor	Create;
		destructor	Destroy; override;
	end;

var
	EmptyNote: 		TNote;

implementation

uses
	{$IFDEF WINDOWS}Windows,{$ENDIF}
	Math;

var
	Module: TPTModule;
	Mixing: Boolean;
	samplesLeft: Cardinal;
	outputFreq: Cardinal;
	EmptySample: TSample;


procedure Log(const S: AnsiString);
begin
	{$IFDEF DEBUG}
	Log(S);
	{$ENDIF}
end;

// ==========================================================================
// Paula
// ==========================================================================

constructor TPaulaVoice.Create(OutputFreq: Word);
begin
	inherited Create;

	f_OutputFreq := OutputFreq;

	SRC_DAT := nil;
	DMA_DAT := nil;

	Sample := -1;
	QueuedSample := Sample;
end;

procedure TPaulaVoice.TurnOffDMA;
begin
	Volume := 0;
end;

procedure TPaulaVoice.RestartDMA;
begin
	FRAC := 0.0;
	DMA_POS := 0;
	DMA_DAT := SRC_DAT;
	DMA_LEN := Max(SRC_LEN, 2);
	PlayPos := -1;
	Sample := QueuedSample;

	if (Enabled) and (Sample >= 0) and (Sample < Module.Samples.Count) then
	begin
		with Module.Samples[Sample] do
		begin
			PlayPos := QueuedOffset;
			Age := 6;//Trunc(6 * SRC_VOL) + 2;
		end;
	end;

	QueuedOffset := 0;
	Volume := SRC_VOL;
end;

procedure TPaulaVoice.Kill;
begin
	DMA_DAT := nil;
	SRC_DAT := nil;
	SRC_LEN := 0;
	DMA_LEN := 0;
	DMA_POS := 0;
	SRC_VOL := 0;
	DELTA   := 0;
	FRAC    := 0;
	LASTDELTA := 0;
	LASTFRAC  := 0;
	Volume  := 0;
	PlayPos := -1;
	Sample := -1;
	QueuedSample := -1;
	HasSound := False;
end;

procedure TPaulaVoice.SetPeriod(period: Word);
begin
	// This is what really happens on Paula on a real Amiga
	// on normal video modes. Tested and confirmed by 8bitbubsy!
	if period > 0 then
	begin
		DELTA := (PAULA_PAL_CLK / Max(period, 113)) / f_outputFreq;
		HasSound := True;
	end
	else
	begin
		DELTA := 0.0;
		HasSound := False;
	end;
	if LASTDELTA = 0.0 then
		LASTDELTA := DELTA;
end;

procedure TPaulaVoice.SetVolume(vol: Word);
var
	SV: Single;
begin
	if (vol and (1 shl 6)) <> 0 then
		vol := $0040
	else
		vol := vol and $003F;

	SV := SRC_VOL;
	SRC_VOL := vol * (1.0 / 64.0);
	if SV <> SRC_VOL then
		Volume := SRC_VOL;
end;

procedure TPaulaVoice.SetLength(len: Cardinal);
begin
	SRC_LEN := len * 2;
end;

procedure TPaulaVoice.SetData(NewSample: Byte; NewOffset: Integer);
begin
	if NewOffset >= 0 then
	begin
		SRC_DAT := @Module.Samples[NewSample].Data[NewOffset];
		QueuedSample := NewSample;
		QueuedOffset := NewOffset;
	end
	else
		SetDataPtr(nil);
end;

procedure TPaulaVoice.SetDataPtr(const src: PArrayOfShortInt);
begin
	if src <> nil then
		SRC_DAT := src
	else
	begin
		SRC_DAT := @EmptySample.Data[0];
		QueuedSample := -1;
	end;
end;

// ==========================================================================
// Orderlist
// ==========================================================================

function TOrderList.GetItem(Index: Byte): Byte;
begin
	if Index <= 127 then
		Result := FItems[Index]
	else
		Result := 0;
end;

procedure TOrderList.SetItem(Index: Byte; const Value: Byte);
begin
	if Index <= 127 then
		FItems[Index] := Value;
end;

function TOrderList.GetHighestUsed: Byte;
var
	i: Integer;
begin
	Result := 0; // last pattern to save
	for i := 0 to Module.Info.OrderCount-1 do
		Result := Max(Result, GetItem(i));
end;

procedure TOrderList.Insert(Index: Byte; const Value: Byte);
var
	i: Integer;
begin
	if Index < 127 then
	begin
		for i := 127 downto Index+1 do
			FItems[i] := FItems[i-1];
		FItems[Index] := Value;
	end;
end;

procedure TOrderList.Delete(Index: Byte);
var
	i: Integer;
begin
	if Index < 127 then
	begin
		for i := Index to 127-1 do
			FItems[i] := FItems[i+1];
	end;
	FItems[127] := 0;
end;

// ==========================================================================
// Audio API
// ==========================================================================

// Fills Buffer with Len bytes of audio data. Buffer is assumed to be in signed 16-bit format.
//
procedure TPTModule.Render(Buffer: System.PUInt8; Len: System.DWord; Data: Pointer);
var
	outStream: ^TArrayOfSmallInt absolute Buffer;
	pos, sampleBlock, samplesTodo: Integer;
begin
	FillChar(Buffer^, Len, 0);
	if Mixing then Exit;

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
			if (not DisableMixer) and (PlayMode <> PLAY_STOPPED) then
				IntMusic;
			samplesLeft := samplesPerFrame;
		end;
	end;

	Mixing := False;
end;

// 16-bit integer mixer
//
{$IFDEF BASS}
function AudioCallback(Handle: HSTREAM; Buffer: Pointer; Len: DWord; User: Pointer)
: DWord; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
{$ELSE}
procedure AudioCallback_SDL2(Data: Pointer; Buffer: PUInt8; Len: Integer); cdecl;
{$ENDIF}
var
	outStream: ^TArrayOfSmallInt absolute Buffer;
	pos, sampleBlock, samplesTodo: Integer;
begin
	{$IFDEF BASS}
	Result := Len;
	{$ENDIF}
	FillChar(Buffer^, Len, 0);
	if (Module = nil) or (Module.RenderMode <> RENDER_NONE) then Exit;

	Mixing := True;

	if Module.DisableMixer then
	begin
		Mixing := False;
		Exit;
	end;

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
			Module.MixSampleBlock(@outStream[pos], samplesTodo);
			Inc(pos, samplesTodo * 2);

			Dec(sampleBlock, samplesTodo);
			Dec(samplesLeft, samplesTodo);
		end
		else
		begin
			if (not Module.DisableMixer) and (Module.PlayMode <> PLAY_STOPPED) then
				Module.IntMusic;
			samplesLeft := Module.samplesPerFrame;
		end;
	end;

	Mixing := False;
end;

// ==========================================================================
// Utility
// ==========================================================================

function GetString(var Buf: array of AnsiChar): AnsiString;
var
	i: Integer;
begin
	Result := '';
	for i := 0 to High(Buf) do
	begin
		if Buf[i] = #0 then Exit;
		Result := Result + Buf[i];
	end;
end;

// ==========================================================================
// TChannel
// ==========================================================================

constructor TPTChannel.Create(i: Byte);
begin
	inherited Create;

	n_index := i;
	Paula := TPaulaVoice.Create(outputFreq);
	SetEnabled(True);
	Reset;
end;

procedure TPTChannel.Reset;
begin
	Note := nil;

	n_start := -1;
	n_wavestart := -1;
	n_loopstart := -1;
	n_volume := 0;
	n_toneportdirec := 0;
	n_vibratopos := 0;
	n_tremolopos := 0;
	n_pattpos := 0;
	n_loopcount := 0;
	n_wavecontrol := 0;
	n_glissfunk := 0;
	n_sampleoffset := 0;
	n_toneportspeed := 0;
	n_vibratocmd := 0;
	n_tremolocmd := 0;
	n_finetune := 0;
	n_funkoffset := 0;
	n_sample := 0;
	n_period := 0;
	n_note := 0;
	n_wantedperiod := 0;
	n_length := 0;
	n_replen := 0;
	n_repend := 0;

	Paula.Sample := 31;
	Paula.QueuedSample := 31;
	Paula.PlayPos := -1;
	Paula.f_OutputFreq := outputFreq;
end;

procedure TPTChannel.SetEnabled(E: Boolean);
begin
	FEnabled := E;
	Paula.Enabled := E;
end;

destructor TPTChannel.Destroy;
begin
	Paula.Free;
	inherited Destroy;
end;

// ==========================================================================
// TPTModule
// ==========================================================================

function TPTModule.CountUsedPatterns: Byte;
var
	i: Integer;
begin
	Result := 0;
	for i := MAX_PATTERNS-1 downto 0 do
	begin
		if not IsPatternEmpty(i) then
		begin
			Result := i;
			Break;
		end;
	end;
	Info.PatternCount := Result;
end;

function TPTModule.IsPatternEmpty(i: Byte): Boolean;
var
	x, y: Integer;
	Note: PNote;
begin
	Result := True;
	if i >= MAX_PATTERNS then Exit;
	for x := 0 to AMOUNT_CHANNELS-1 do
		for y := 0 to 63 do
		begin
			Note := @Notes[i, x, y];
			if (Note.Sample > 0) or (Note.Command > 0)   or
			   (Note.Pitch > 0) or (Note.Parameter > 0) then
					Exit(False);
		end;
end;

procedure TPTModule.SetTitle(const S: AnsiString);
var
	i: Integer;
begin
	Info.Title := '';
	if Length(S) > 0 then
		for i := 0 to Min(19, Length(S)-1) do
			Info.Title[i] := S[i+1];
end;

procedure TPTModule.ClearRowVisitTable;
var
	i, j: Integer;
begin
	for i := 0 to MAX_PATTERNS-1 do
		for j := 0 to 63 do
			RenderInfo.RowVisitTable[i,j] := False;
end;

function CheckModType(const buf: AnsiString): TModuleFormat;
begin
	if (buf = 'M.K.') then
	begin
		// ProTracker v1.x, handled as ProTracker v2.x
		Result := FORMAT_MK;
	end
	else
	if (buf = 'M!K!') then
	begin
		// ProTracker v2.x (if >64 patterns)
		Result := FORMAT_MK2;
	end
	else
	if (buf = 'FLT4') then
	begin
		// StarTrekker (4ch), handled as ProTracker v2.x
		Result := FORMAT_FLT4;
	end
	else
	if (buf = '4CHN') then
	begin
		// FastTracker II (4ch), handled as ProTracker v2.x
		Result := FORMAT_4CHN;
	end
	else
	if (buf = 'N.T.') then
	begin
		// NoiseTracker 1.0, handled as ProTracker v2.x
		Result := FORMAT_MK;
	end
	else
	if (buf = 'FEST') or (buf = 'M&K!') then
	begin
		// Special NoiseTracker format (used in music disks?)
		Result := FORMAT_FEST;
	end
	else
	begin
		// may be The Ultimate SoundTracker, 15 samples
		Result := FORMAT_UNKNOWN;
	end;
end;

function TPTModule.SaveToFile(const Filename: String): Boolean;
var
	i, j, k, l: Integer;
	B: Word;
	c: Cardinal;
	Stream: TFileStreamEx;
begin
	c := 0;
	for i := 0 to 30 do // get length of longest sample
		c := Max(c, Samples[i].Length * 2);

	if c > $1FFFF then // crop samples >127K
	begin
		for i := 0 to 30 do
			Samples[i].Length := Min(Samples[i].Length, $FFFF);
		Log('Samples longer than 127K have been cropped!');
	end;

	if c > $FFFF then
	begin
		Log('Module contains samples longer than 64K.');
		Log('This may cause problems with some trackers/players.');
	end;

	Stream := TFileStreamEx.Create(Filename, fmCreate);
	Warnings := False;

	// write song title
	//
	for i := High(Info.Title) downto Low(Info.Title) do
		if Info.Title[i] = ' ' then
			Info.Title[i] := #0
		else
			Break;

	for i := Low(Info.Title) to High(Info.Title) do
		Stream.Write8(Info.Title[i]);

	// write sample infos
	//
	for i := 0 to 30 do
	begin
		for j := 21 downto 0 do
			if Samples[i].Name[j] = ' ' then
				Samples[i].Name[j] := #0
			else
				Break;

		for j := 0 to 21 do
			Stream.Write8(Samples[i].Name[j]);

		Stream.Write8(Byte(Samples[i].Length * 2 shr 9));
		Stream.Write8(Byte(Samples[i].Length * 2 shr 1));
		Stream.Write8(Samples[i].Finetune and $0F);
		Stream.Write8(Min(Samples[i].Volume, 64));

		c := Max(Samples[i].LoopLength * 2, 2);
		j := Samples[i].LoopStart * 2;
		if c = 2 then j := 0;

		Stream.Write8(j shr 9); // tempLoopStart
		Stream.Write8(j shr 1);
		Stream.Write8(c shr 9); // tempLoopLength
		Stream.Write8(c shr 1);
	end;

	Stream.Write8(Info.OrderCount and $FF);
	Stream.Write8($7F); // ProTracker puts 0x7F at this place (restart pos)

	// write orderlist
	//
	for i := 0 to 127 do
		Stream.Write8(OrderList[i] and $FF);

	// write ID
	//
	if CountUsedPatterns < 64 then
		Stream.WriteString('M.K.')
	else
		Stream.WriteString('M!K!'); // >64 patterns

	// write pattern data
	//
	j := OrderList.GetHighestUsed;
	//Log('Writing %d patterns...', [j+1]);

	for i := 0 to j do
	begin
		for l := 0 to 63 do
		begin
			for k := 0 to AMOUNT_CHANNELS-1 do
			with Notes[i,k,l] do
			begin
				if Pitch = 0 then
					B := 0
				else
					B := PeriodTable[Pitch-1];
				Stream.Write8(((B shr 8) and $0F) or (Sample and $10));
				Stream.Write8(B and $FF);
				Stream.Write8((Sample shl 4) or (Command and $0F));
				Stream.Write8(Parameter);
			end;
		end;
	end;

{	if j < Info.PatternCount then
		Log('Discarded %d unused patterns!', [Info.PatternCount - j]);}

	// write sample data
	//
	for i := 0 to 30 do
	begin
		if (Samples[i].Length >= 2) and (Samples[i].LoopLength <= 2) then
		begin
			// Amiga ProTracker beep fix: zero first word of sample data
			Stream.Write16(0);
			k := 2;
		end
		else
			k := 0;
		for j := k to Samples[i].Length*2-1 do
			Stream.Write8(Samples[i].Data[j]);
	end;

	// write file to disk
	//
	Stream.Free;
	Info.Filename := Filename;

	Log('Module saved: ' + Filename + '.');

	Warnings := False;
	Result := True;
end;

function ppdecrunch(src, dst, offsetLens: PByte; srcLen, dstLen: uint32; skipBits: Byte): Boolean;
var
	bitsLeft, bitCnt: Byte;
	bufSrc,	dstEnd, bout: PByte;
	x, todo, offBits, offset,
	written, bitBuffer: uint32;

	procedure PP_READ_BITS(nbits: Byte; var nvar: Cardinal);
	begin
		bitCnt := nbits;
		while (bitsLeft < bitCnt) do
		begin
			if (bufSrc < src) then Exit;
			Dec(bufSrc);
			bitBuffer := bitbuffer or (bufSrc^ shl bitsLeft);
			Inc(bitsLeft, 8);
		end;

		nvar := 0;
		Dec(bitsLeft, bitCnt);

		while (bitCnt > 0) do
		begin
			nvar := (nvar shl 1) or (bitBuffer and 1);
			bitBuffer := bitBuffer shr 1;
			Dec(bitCnt);
		end;
	end;

begin
	Result := False;
	if (src = nil) or (dst = nil) or (offsetLens = nil) then Exit;

	bitsLeft  := 0;
	bitBuffer := 0;
	written   := 0;
	bufSrc    := src + srcLen;
	bout      := dst + dstLen;
	dstEnd    := bout;

	PP_READ_BITS(skipBits, x);
	while (written < dstLen) do
	begin
		PP_READ_BITS(1, x);
		if (x = 0) then
		begin
			todo := 1;

			repeat
				PP_READ_BITS(2, x);
				Inc(todo, x);
			until (x <> 3);

			while (todo > 0) do
			begin
				Dec(todo);
				PP_READ_BITS(8, x);
				if (bout <= dst) then
					Exit(False);
				Dec(bout);
				bout^ := Byte(x);
				Inc(written);
			end;

			if (written = dstLen) then
				Break;
		end;

		PP_READ_BITS(2, x);

		offBits := offsetLens[x];
		todo    := x + 2;

		if (x = 3) then
		begin
			PP_READ_BITS(1, x);
			if (x = 0) then offBits := 7;

			PP_READ_BITS(offBits, offset);
			repeat
				PP_READ_BITS(3, x);
				Inc(todo, x);
			until (x <> 7);
		end
		else
			PP_READ_BITS(offBits, offset);

		if ((bout + offset) >= dstEnd) then
			Exit(False);

		while (todo > 0) do
		begin
			Dec(todo);
			x := bout[offset];
			if (bout <= dst) then
				Exit(False);
			Dec(bout);
			bout^ := Byte(x);
			Inc(written);
		end;
	end;
	Result := True;
end;

function TPTModule.LoadFromFile(const Filename: String{; Force: Boolean = False}): Boolean;
var
	FileStream: TFileStream;
begin
	Log('Loading module: ' + Filename);
	try
		FileStream := TFileStream.Create(Filename, fmOpenRead, fmShareDenyNone);
		Result := LoadFromStream(FileStream);
		if Result then
			Info.Filename := Filename;
	except
		Result := False;
	end;
	FileStream.Free;
end;

function TPTModule.LoadFromStream(Stream: TStream): Boolean;
var
	StreamEx: TStreamEx;
begin
	StreamEx := TStreamEx.Create(Stream);
	Result := InternalLoad(StreamEx);
	StreamEx.Free;
end;

function TPTModule.InternalLoad(var ModFile: TStreamEx): Boolean;
const
	TEXT_INVALIDMOD = 'Invalid .MOD file: ';
var
	os, i, j, patt, row, ch, loopOverflow: Integer;
	s: TSample;
	Note: PNote;
	mightBeSTK, lateVerSTKFlag: Boolean;
	bytes: array [0..3] of Byte;
	sFile, WarnText: AnsiString;
	Origin: Cardinal;

	// powerpacker decrunch
	ppPackLen, ppUnpackLen: uint32;
	ppCrunchData: array [0..3] of Byte;
	ppBuffer, modBuffer: array of Byte;
	TempFilename: AnsiString;

	procedure ExitError(const Msg: AnsiString; const Args: array of const);
	begin
		if TempFilename <> '' then
			DeleteFile(PChar(TempFilename));
		if ModFile <> nil then
			ModFile.Free;
//		Log('Load failed: ' + Msg, Args);
		Log('Load failed: ' + Msg);
	end;

label
	Done;
begin
	Result := False;
	Warnings := False;
	lateVerSTKFlag := False;

	// Read file data
	//

	Origin := 0;
	Reset;
	//ModFile := TFileStreamEx.Create(Filename, fmOpenRead, fmShareDenyNone);

	Info.BPM := 0;

	// Verify file size
	//
	Info.Filesize := ModFile.Size;
	Info.Filename := '';
	TempFilename := '';

	// ======================================================================
	// Determine module type
	// ======================================================================
	//
	ModFile.SeekTo(Origin);
	sFile := ModFile.ReadString(False, 4);

	if sFile = 'PX20' then
	begin
		ExitError('Encrypted PowerPacker module!', []);
		Exit;
	end
	else
	if sFile = 'PP20' then
	begin
(*
		// decrunch PowerPacker module
		//
		Log('File is packed with PowerPacker.');

		ppPackLen := Info.Filesize;
		if (ppPackLen and 3) <> 0 then
		begin
			ExitError('Unknown PowerPacker error!', []);
			Exit;
		end;

		ModFile.SeekTo(Origin + ppPackLen - 4);
		ModFile.Stream.Read(ppCrunchData[0], 4);

		ppUnpackLen := (ppCrunchData[0] shl 16) or (ppCrunchData[1] shl 8) or ppCrunchData[2];

		// smallest and biggest possible .MOD
		if (ppUnpackLen < 2108) or (ppUnpackLen > 4195326) then
		begin
			ExitError('Not a valid module (incorrect unpacked file size)', []);
			Exit;
		end;

		SetLength(modBuffer, ppUnpackLen+1);
		ModFile.SeekTo(Origin);

		i := ModFile.Size;
		SetLength(ppBuffer, i+1);
		ModFile.Stream.Read(ppBuffer[0], i);

		ppdecrunch(@ppBuffer[8], @modBuffer[0],
			@ppBuffer[4], ppPackLen-12, ppUnpackLen, ppCrunchData[3]);

		ModFile.Free;
		// create a temp. file for the unpacked mod, lame
		TempFilename := ConfigPath + 'temp.pp.mod';
		ModFile := TFileStreamEx.Create(TempFilename, fmCreate or fmOpenReadWrite);
		ModFile.Write(modBuffer[0], ppUnpackLen);

		Info.Filesize := ppUnpackLen;
*)
	end;

	// get normal mod ID
	//
	ModFile.SeekTo(Origin + OFFSET_ID);
	ModFile.Stream.Read(Info.ID[0], 4);
	Info.Format := CheckModType(Info.ID);
	mightBeSTK := (Info.Format = FORMAT_UNKNOWN);

	// ======================================================================
	// Import Impulse Tracker module
	// ======================================================================
	// (this will break if a ST module's title begins with "IMPM"!)
	//
	if (Info.Format = FORMAT_UNKNOWN) then
	begin
		ExitError('Unknown format.', []);
		Exit;
	end
	else
	if ((Info.Filesize < MODFILESIZE_MIN) or (Info.Filesize > MODFILESIZE_MAX)) then
	begin
		ExitError('Invalid filesize.', []);
		Exit;
	end;

	// Read song title
	//
	ModFile.SeekTo(Origin + OFFSET_SONGTITLE);
	ModFile.Stream.Read(Info.Title[0], 20);

	//ModFile.SeekTo(Origin + OFFSET_SAMPLEINFO);

	// ======================================================================
	// Read sample headers
	// ======================================================================
	//
	for i := 0 to 30 do
	begin
		s := Samples[i];

		if (mightBeSTK) and (i > 14) then
		begin
			s.LoopLength := 1;
			Continue;
		end;

		WarnText := '';

		// index 23 of s.text is already zeroed
		//ModFile.ReadBytes(PByte(@s.Name[0]), 22);
		for j := 0 to 21 do
			s.Name[j] := AnsiChar(Max(32, ModFile.Read8));

		s.Length := ModFile.Read16R * 2;
		lateVerSTKFlag := (s.Length > 9999); // Only used if mightBeSTK is set

		if Info.Format = FORMAT_FEST then
			// One more bit of precision, + inverted
			s.Finetune := ((0 - ModFile.Read8 and $1F) div 2)
		else
			s.Finetune := ModFile.Read8 and $0F;

		s.Volume := Min(64, ModFile.Read8);

		s.LoopStart  := ModFile.Read16R; // repeat
		if (not mightBeSTK) then
			s.LoopStart := s.LoopStart * 2;
		s.LoopLength := Max(ModFile.Read16R * 2, 2); // replen

	//Log('Sam %.2d  Len=%8x  S=%7x  L=%7x',	[i+1, s.Length, s.LoopStart, s.LoopLength]);

		// fix for poorly converted STK.PTMOD modules.
		if (not mightBeSTK) and (s.LoopLength > 2) and (s.LoopStart + s.LoopLength > s.Length) then
		begin
			WarnText := Format('Sample %d has illegal loop.', [i+1]);
			if ((s.LoopStart div 2 + s.LoopLength) <= s.Length) then
			begin
				s.LoopStart := s.LoopStart div 2;
				WarnText := WarnText + ' Loop start adjusted.';
			end;
		end;

		if mightBeSTK then
		begin
			if s.LoopLength > 2 then
			begin
				s.Length       := Max(s.Length - s.LoopStart, 0);
				s.tmpLoopStart := s.LoopStart;
				s.LoopStart    := 0;
			end;
			s.Finetune := 0; // No finetune in STK/UST
		end;

		// some modules are broken like this, adjust sample length if possible
		if (s.LoopLength > 2) and (s.LoopStart + s.LoopLength > s.Length) then
		begin
			WarnText := Format('Sample %d has illegal loop.', [i+1]);
			loopOverflow := s.LoopStart + s.LoopLength - s.Length;
			if (s.Length > 0) and ((s.Length + loopOverflow) <= 131070) then
			begin
				Inc(s.Length, loopOverflow);
				WarnText := WarnText + Format(' Increased sample length by %d bytes.', [loopOverflow*2]);
			end;
		end;

		if (s.Length > 0) and (WarnText <> '') then
		begin
			Log(WarnText);
			Warnings := True;
		end;
	end;

	// STK 2.5 had loopStart in words, not bytes. Convert if late version STK.
	//
	if mightBeSTK and lateVerSTKFlag then
	begin
		//Log('Converting sample loops from STK.');
		for i := 0 to 15 do
		begin
			s := Samples[i];
			if s.LoopStart > 2 then
			begin
				s.Length := s.Length - s.tmpLoopStart;
				s.tmpLoopStart := s.tmpLoopStart * 2;
			end;
		end;
	end;

	if mightBeSTK then log('mightBeSTK');
	if lateVerSTKFlag then log('lateVerSTKFlag');

	for i := 0 to 30 do
	begin
		s := Samples[i];
		s.Length := s.Length div 2;
		s.LoopStart := s.LoopStart div 2;
		s.LoopLength := s.LoopLength div 2;
		if s.LoopStart + s.LoopLength > s.Length then
		begin
			if s.Length > 0 then
			begin
				//Log(TEXT_ERROR + 'Sample %d has illegal loop. Loop deactivated.', [i+1]);
				Warnings := True;
			end;
			s.LoopStart := 0;
			s.LoopLength := 1;
		end;
	end;

	// ======================================================================
	// Read orderlist and find amount of patterns in file
	// ======================================================================
	//
	//ModFile.SeekTo(Origin + 950);
	Info.OrderCount := Integer(ModFile.Read8);

	if Info.OrderCount > 127 then // fixes beatwave.mod (129 orders) and other weird MODs
	begin
		if Info.OrderCount > 129 then
		begin
			ExitError(TEXT_INVALIDMOD + 'Orderlist too long! (%d)', [Info.OrderCount]);
			Exit;
		end
		else
			Info.OrderCount := 127;
	end
	else
	if Info.OrderCount = 0 then
	begin
		ExitError(TEXT_INVALIDMOD + 'Invalid ID or Zero-length orderlist!', []);
		Exit;
	end;

	Info.RestartPos := ModFile.Read8;

	if (mightBeSTK) and ((Info.RestartPos = 0) or (Info.RestartPos > 220)) then
	begin
		ExitError(TEXT_INVALIDMOD + 'Invalid restart pos. (%d)', [Info.RestartPos]);
		Exit;
	end;

	// If we're still here at this point and the mightBeSTK flag is set,
	// then it's definitely a proper The Ultimate SoundTracker (STK) module.
	//
	if mightBeSTK then
	begin
		Info.Format := FORMAT_STK;
		if Info.RestartPos = 120 then
			Info.RestartPos := 125
		else
		begin
			if Info.RestartPos > 239 then
				Info.RestartPos := 239;
			// max BPM: 14536 (there was no clamping originally, sick!)
			Info.BPM := Round(1773447 / ((240 - Info.RestartPos) * 122));
			SetTempo(Info.BPM);
		end;
	end;

	Info.PatternCount := 0;
	for i := 0 to 127 do
	begin
		OrderList[i] := ModFile.Read8;
		if OrderList[i] > Info.PatternCount then
			Info.PatternCount := OrderList[i];
	end;

	if Info.PatternCount > MAX_PATTERNS then
	begin
		ExitError(TEXT_INVALIDMOD + 'Too many patterns! (%d)', [Info.PatternCount]);
		Exit;
	end;

	if Info.Format <> FORMAT_STK then	// The Ultimate SoundTracker MODs don't have this tag
		ModFile.Skip(4); 				// We already read/tested the tag earlier, skip it

	// ======================================================================
	// Read pattern data
	// ======================================================================
	//
	Warnings := False;

	for patt := 0 to Info.PatternCount do
	begin
		for row := 0 to 63 do
		begin
			for ch := 0 to AMOUNT_CHANNELS-1 do
			begin
				note := @Notes[patt, ch, row];
				ModFile.Stream.Read(bytes[0], 4);

				note.Pitch     := PeriodToNote(((bytes[0] and $0F) shl 8) or bytes[1]);
				// Don't (!) clamp, the player checks for invalid samples
				note.Sample    := (bytes[0] and $F0) or (bytes[2] shr 4);
				note.Command   := bytes[2] and $0F;
				note.Parameter := bytes[3];
				if note.Command = $C then
					note.Parameter := Min(note.Parameter, 64);
				if Note.Pitch >= 37 then
					Warnings := True;

				if Info.Format = FORMAT_FEST then
				begin
					// Any Dxx = D00 in FEST modules
					if note.Command = $0D then
						note.Parameter := $00;
				end
				else
				if mightBeSTK then
				begin
					// Convert STK effects to PT effects
					if not lateVerSTKFlag then
					begin
						if note.Command = $01 then
						begin
							// Arpeggio
							note.Command := $00;
						end
						else
						if note.Command = $02 then
						begin
							// Pitch slide
							if (note.Parameter and $F0) <> 0 then
							begin
								note.Command := $02;
								note.Parameter := note.Parameter shr 4;
							end
							else
							if (note.Parameter and $0F) <> 0 then
							begin
								note.Command := $01;
							end;
						end;
					end;

					// Volume slide/pattern break
					if note.Command = $0D then
					begin
						if note.Parameter = 0 then
							note.Command := $0D
						else
							note.Command := $0A;
					end;
				end
				else
				if Info.Format = FORMAT_4CHN then // 4CHN != PT MOD
				begin
					// Remove FastTracker II 8xx/E8x panning commands if present
					if (note.Command = $08) or
					   ((note.Command = $0E) and ((note.Parameter shr 4) = $08)) or
					// Remove F00, FastTracker II didn't use F00 as STOP in .MOD
					   ((note.Command = $0F) and (note.Parameter = $00)) then
					begin
						note.Command   := 0;
						note.Parameter := 0;
					end;
				end;

			end; // channel
		end; // row
	end; // pattern

	if Warnings then
		Log('Module contains notes above B-3!');

	// ======================================================================
	// Read sampledata
	// ======================================================================
	//
	//	ModFile.SeekTo(Origin + OFFSET_PATTERNS + (Info.PatternCount * 1024));
	if Info.Format = FORMAT_STK then
		os := 15
	else
		os := 31;

	for i := 0 to os-1 do
	begin
		s := Samples[i];

		if (mightBeSTK) and (s.LoopLength > 2) then
		begin
			ModFile.Skip(s.tmpLoopStart * 2);
			j := (s.Length - (s.LoopStart)) * 2;
			//Debug('Read STK sample %d, offset %d, %d bytes', [i+1, ModFile.Position, j]);
			s.LoadData(ModFile, j);
		end
		else
		if s.Length > 0 then
		begin
			//Log('Read sample %d, offset %d, %d bytes', [i+1, ModFile.Position, s.Length * 2]);
			s.LoadData(ModFile, s.Length * 2);
		end;

		//TODO FIXME
		// fix illegal loop length (f.ex. from "Fasttracker II" .MODs)
		if (s.LoopLength <= 2) then
		begin
			s.LoopLength := 1;
			// if no loop, zero first two samples of data to prevent "beep"
			if (s.Length >= 2) then
			begin
				s.Data[0] := 0;
				s.Data[1] := 0;
			end;
		end;

	end;

// ======================================================================
Done:
// ======================================================================

	Modified := False;

	if TempFilename <> '' then
		DeleteFile(PChar(TempFilename));

	CalculatePans(StereoSeparation);
	IndexSamples;

	if Info.BPM > 0 then
		DefaultTempo := Info.BPM;
	FindDefaultTempo(False, Info.BPM > 0);

	CurrentSpeed := DefaultSpeed;
	if Info.BPM = 0 then
	begin
		Info.BPM := DefaultTempo;
		SetTempo(Info.BPM);
	end;

	Result := True;
	Log('Done.');
end;

procedure TPTModule.FindDefaultTempo(GotSpeed, GotTempo: Boolean);
var
	patt, ch, row: Integer;
	Note: PNote;
begin
	patt := OrderList[0];
	for ch := 0 to AMOUNT_CHANNELS-1 do
	for row := 0 to 63 do
	begin
		Note := @Notes[patt, ch, row];
		if Note.Command = $F then
		begin
			if Note.Parameter >= 32 then
			begin
				if not GotTempo then
					DefaultTempo := Note.Parameter;
				GotTempo := True;
			end
			else
			if Note.Parameter > 0 then
			begin
				if not GotSpeed then
					DefaultSpeed := Note.Parameter;
				GotSpeed := True;
			end;
			if (GotSpeed) and (GotTempo) then Exit;
		end;
	end;
end;

procedure TPTModule.RepostChanges;
begin
	if RenderMode = RENDER_NONE then
	begin
		PostMessagePtr(MSG_ORDERCHANGE, @PlayPos);
		PostMessagePtr(MSG_ROWCHANGE, @PlayPos);
	end;
end;

procedure TPTModule.SetModified(B: Boolean = True; Force: Boolean = False);
begin
	if Assigned(OnModified) then
		OnModified(B, Force);
	Modified := B;
end;

constructor TPTModule.Create;
var
	i: Integer;
begin
	inherited Create;

	Module := Self;

	OnFilter := nil;
	OnSpeedChange := nil;
	OnPlayModeChange := nil;

	with EmptyNote do
	begin
		Sample := 0;
		Pitch := 0;
		Command := 0;
		Parameter := 0;
	end;

	EmptySample := TSample.Create;
	EmptySample.Resize(16);

	Samples := TObjectList<TSample>.Create(True);

	Warnings := False;
	Modified := False;

	PreventClipping := True;
	RenderMode := RENDER_NONE;

	DefaultSpeed := 6;
	DefaultTempo := 125;

	Info.Filename := '';
	Info.OrderCount := 1;
	Info.PatternCount := 0;
	Info.Speed := DefaultSpeed;
	Info.BPM := DefaultTempo;
	CurrentSpeed := 6;
	SetBPMFlag := 0;

	DisableMixer := True;
	PlayMode := PLAY_STOPPED;

	for i := 0 to 31 do
		Samples.Add(TSample.Create);
	IndexSamples;

	outputFreq := PTOptions.Frequency;

	for i := 0 to AMOUNT_CHANNELS-1 do
		Channel[i] := TPTChannel.Create(i);

	SetOutputFreq(outputFreq);
	CalculatePans(StereoSeparation);

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

	Audio.RegisterPlayer(Self, 'ProTracker', '*.mod;mod.*');
	Audio.PreparePlayer(Self);
	Audio.Init;

	DisableMixer := False;

	RegisterMessages(MSG_TIMERTICK, 4);
end;

procedure TPTModule.SetOutputFreq(Hz: Cardinal);
var
	i: Integer;
begin
	if Hz > 0 then
	begin
		outputFreq := Hz;
		i := Round(2 * ((Hz * 125) / 50 / 32));
		if i mod 2 > 0 then Inc(i);
		{j := samplesPerFrame * 2 + 2;
		if j > i then
		begin
			Log('j!');
			i := j;
		end;}
		SetLength(MixBuffer, i);
//		Log('Mix buffer set to %d samples.', [i]);
	end;

	SetTempo(Info.BPM);

	for i := 0 to AMOUNT_CHANNELS-1 do
		Channel[i].Reset;

	Reset;

	// Amiga 500 RC low-pass filter (R = 360 ohm, C = 0.1uF)
	// hz = 1 / (2pi * R * C)    = ~4421.Hz
	CalcCoeffLossyIntegrator(outputFreq, 4421.0, @FilterLo);

	// Amiga 500 RC high-pass filter (R = 1390 ohm, C = 22uF)
	// hz = 1 / (2pi * R * C)    = ~5.2Hz
	CalcCoeffLossyIntegrator(outputFreq, 5.2, @FilterHi);

	// Amiga 500 Sallen-Key "LED" filter (R1 := 10k ohm, R2 := 10k ohm, C1 := 6800pf, C2 := 3900pf)
	// hz := 1 / (2pi * root(R1 * R2 * C1 * C2))    := ~3090.5Hz
	CalcCoeffLED(outputFreq, 3090.5, @FilterLEDC);
end;

destructor TPTModule.Destroy;
var
	i: Integer;
begin
	Close;

	{$IFDEF BASS}
	if Stream <> 0 then
	begin
		BASS_ChannelStop(Stream);
		BASS_StreamFree(Stream);
	end;
	{$ENDIF}

	for i := 0 to AMOUNT_CHANNELS-1 do
		Channel[i].Free;

	{for i := 0 to High(Samples) do
		if Assigned(Samples[i]) then
			Samples[i].Free;}
	Samples.Free;
	EmptySample.Free;

	inherited Destroy;
end;

procedure TPTModule.Close;
begin
	DisableMixer := True;
	while Mixing do;

	PlayMode := PLAY_STOPPED;
end;

procedure TPTModule.IndexSamples;
var
	i: Integer;
begin
	for i := 0 to Samples.Count-1 do
		Samples[i].Index := i + 1;
end;

procedure TPTModule.CalculatePans(percentage: Byte);

	// these are used to create an equal powered panning
	function sinApx(x: Single): Single;
	begin
		x := x * (2.0 - x);
		Result := x * 1.09742972 + x * x * 0.31678383;
	end;

	function cosApx(x: Single): Single;
	begin
		x := (1.0 - x) * (1.0 + x);
		Result := x * 1.09742972 + x * x * 0.31678383;
	end;

var
	scaledPanPos: Byte;
	p: Single;
begin
	if percentage > 100 then
		percentage := 100;

	scaledPanPos := Trunc((percentage * 128) / 100);

	p := (128 - scaledPanPos) * (1.0 / 256.0);
	Channel[0].Paula.PANL := cosApx(p);
	Channel[0].Paula.PANR := sinApx(p);
	Channel[3].Paula.PANL := cosApx(p);
	Channel[3].Paula.PANR := sinApx(p);

	p := (128 + scaledPanPos) * (1.0 / 256.0);
	Channel[1].Paula.PANL := cosApx(p);
	Channel[1].Paula.PANR := sinApx(p);
	Channel[2].Paula.PANL := cosApx(p);
	Channel[2].Paula.PANR := sinApx(p);

	StereoSeparation := percentage;
end;

{$IFDEF DEBUG}
{$R-}
{$ENDIF}

// ==========================================================================
// Effects
// ==========================================================================

procedure TPTModule.UpdateFunk(var ch: TPTChannel);
var
	funkspeed: ShortInt;
begin
	funkspeed := ch.n_glissfunk shr 4;
	if funkspeed > 0 then
	begin
		Inc(ch.n_funkoffset, FunkTable[funkspeed]);
		if (ch.n_funkoffset >= 128) then
		begin
			ch.n_funkoffset := 0;
			if (ch.n_loopstart >= 0) and (ch.n_wavestart >= 0) then
			begin
				if not ch.Paula.HasSound then Exit;
				Inc(ch.n_wavestart);
				if (ch.n_wavestart >= (ch.n_loopstart + (ch.n_replen * 2))) then
					ch.n_wavestart := ch.n_loopstart;
				Samples[ch.n_sample].Data[ch.n_wavestart] :=
					-1 - Samples[ch.n_sample].Data[ch.n_wavestart];
				SampleChanged[ch.n_sample] := True;
			end;
		end;
	end;
end;

procedure TPTModule.SetGlissControl(var ch: TPTChannel);
begin
	ch.n_glissfunk := (ch.n_glissfunk and $F0) or (ch.Note.Parameter and $000F);
end;

procedure TPTModule.SetVibratoControl(var ch: TPTChannel);
begin
	ch.n_wavecontrol := (ch.n_wavecontrol and $F0) or (ch.Note.Parameter and $0F);
end;

procedure TPTModule.SetFineTune(var ch: TPTChannel);
begin
	ch.n_finetune := ch.Note.Parameter and $0F;
end;

procedure TPTModule.JumpLoop(var ch: TPTChannel);
var
	i: Integer;
begin
	if Counter <> 0 then Exit;

	if (ch.Note.Parameter and $0F) = 0 then
	begin
		ch.n_pattpos := PlayPos.Row;
		Exit;
	end;

	if (ch.n_loopcount = 0) then
	begin
		ch.n_loopcount := ch.Note.Parameter and $0F;
	end
	else
	begin
		Dec(ch.n_loopcount);
		if (ch.n_loopcount = 0) then Exit;
	end;

	PBreakPosition  := ch.n_pattpos;
	PBreakFlag := True;

	if RenderMode <> RENDER_NONE then
		for i := PBreakPosition to PlayPos.Row do
			RenderInfo.RowVisitTable[PlayPos.Order, i] := False;
end;

procedure TPTModule.SetTremoloControl(var ch: TPTChannel);
begin
	ch.n_wavecontrol := ((ch.Note.Parameter and $0F) shl 4) or (ch.n_wavecontrol and $0F);
end;

procedure TPTModule.KarplusStrong(var ch: TPTChannel);
var
	len: Word;
	i, p: Integer;
	Sam: TSample;
begin
	if not ch.Paula.HasSound then Exit;
	if not PTOptions.EnableKarplusStrong then Exit;

	Sam := Samples[ch.n_sample];
    len := ((ch.n_replen * 2) and $FFFF) - 1;
	p := ch.n_loopstart;

	if p >= 0 then
	begin
		for i := 0 to len-1 do
		begin
			Sam.Data[p] := SarSmallint(ShortInt(Sam.Data[p]) + ShortInt(Sam.Data[p+1]));
			Inc(p);
		end;

		Sam.Data[p] := SarSmallint(ShortInt(Sam.Data[ch.n_loopstart]) + ShortInt(Sam.Data[p]));
		SampleChanged[ch.n_sample] := True;
	end;
end;

procedure TPTModule.DoRetrg(var ch: TPTChannel);
begin
	with ch do
	begin
		if not Paula.HasSound then Exit;

		Paula.SetData(n_sample, n_start); // n_start is increased on 9xx
		Paula.SetLength(n_length);
		Paula.SetPeriod(n_period);
		Paula.RestartDMA;

		// these take effect after the current DMA cycle is done
		Paula.SetData(n_sample, n_loopstart);
		Paula.SetLength(n_replen);
	end;
end;

procedure TPTModule.RetrigNote(var ch: TPTChannel);
begin
	if (ch.Note.Parameter and $0F) <> 0 then
	begin
		if (Counter = 0) then
			if (ch.n_note and $0FFF) <> 0 then
				Exit;

		if (Counter mod (ch.Note.Parameter and $0F)) = 0 then
			DoRetrg(ch);
	end;
end;

procedure TPTModule.VolumeSlide(var ch: TPTChannel);
var
	cmd: Byte;
begin
	cmd := ch.Note.Parameter;
	if (cmd and $F0) = 0 then
	begin
		Dec(ch.n_volume, (cmd and $0F));
		if ch.n_volume < 0 then ch.n_volume := 0;
	end
	else
	begin
		Inc(ch.n_volume, (cmd shr 4));
		if ch.n_volume > 64 then ch.n_volume := 64;
	end;
end;

procedure TPTModule.VolumeFineUp(var ch: TPTChannel);
begin
	if Counter = 0 then
	begin
		Inc(ch.n_volume, (ch.Note.Parameter and $0F));
		if ch.n_volume > 64 then ch.n_volume := 64;
	end;
end;

procedure TPTModule.VolumeFineDown(var ch: TPTChannel);
begin
	if Counter = 0 then
	begin
		Dec(ch.n_volume, (ch.Note.Parameter and $0F));
		if ch.n_volume < 0 then ch.n_volume := 0;
	end;
end;

procedure TPTModule.NoteCut(var ch: TPTChannel);
begin
	if (Counter = (ch.Note.Parameter and $0F)) then
		ch.n_volume := 0;
end;

procedure TPTModule.NoteDelay(var ch: TPTChannel);
begin
	if (Counter = (ch.Note.Parameter and $0F)) then
	begin
		if (ch.n_note and $0FFF) <> 0 then
			DoRetrg(ch);
	end;
end;

procedure TPTModule.PatternDelay(var ch: TPTChannel);
begin
	if (Counter = 0) then
	begin
		if (PattDelTime2 = 0) then
			PattDelTime := (ch.Note.Parameter and $0F) + 1;
	end;
end;

procedure TPTModule.FunkIt(var ch: TPTChannel);
begin
	if Counter = 0 then
	begin
		ch.n_glissfunk := ((ch.Note.Parameter and $0F) shl 4) or (ch.n_glissfunk and $0F);
		if (ch.n_glissfunk and $F0) <> 0 then
			UpdateFunk(ch);
	end;
end;

procedure TPTModule.PositionJump(var ch: TPTChannel);
var
	NewOrder: ShortInt;
begin
	NewOrder :=	ch.Note.Parameter and $7F - 1;
	if NewOrder = -1 then
		NewOrder := Info.OrderCount - 1;
	PlayPos.Order := NewOrder;
	PBreakPosition := 0;
	PosJumpAssert  := True;

	if RenderMode = RENDER_NONE then
	begin
		PostMessagePtr(MSG_ORDERCHANGE, @PlayPos);
		if Assigned(OnOrderChange) then
			OnOrderChange(AUDIOEVENT_ORDER, PlayPos.Order, PlayPos.Row);
	end;
end;

procedure TPTModule.VolumeChange(var ch: TPTChannel);
begin
	ch.n_volume := Min(64, ch.Note.Parameter);
end;

procedure TPTModule.PatternBreak(var ch: TPTChannel);
begin
	PBreakPosition := (((ch.Note.Parameter and $F0) shr 4) * 10) + (ch.Note.Parameter and $0F);
	if (PBreakPosition > 63) then
		PBreakPosition := 0;
	// TODO: PlayPos change?
	PosJumpAssert := True;
end;

procedure TPTModule.SetReplayerBPM(bpm: Byte);
var
	ciaVal: Word;
	bufsize: Cardinal;
	f_hz, f_smp: Single;
begin
	if outputFreq < 0.1 then Exit;
	if bpm < 32 then bpm := 32;
	SetBPMFlag := 0;

	ciaVal := Trunc(1773447 / bpm); // yes, truncate here
	f_hz  := CIA_PAL_CLK / ciaVal;
	f_smp := outputFreq / f_hz;

	samplesPerFrame := Trunc(f_smp + 0.5);

	bufsize := samplesPerFrame * 2 + 2;
	if Length(MixBuffer) < bufsize then
		SetLength(MixBuffer, bufsize);

	if RenderMode = RENDER_NONE then
		if Assigned(OnSpeedChange) then
			OnSpeedChange(CurrentSpeed, bpm);
end;

procedure TPTModule.SetSpeed(NewSpeed: Byte; DoChange: Boolean = True);
begin
	if NewSpeed = 0 then Exit;

	Counter := 0;

	if (VBlankMode) or (NewSpeed < 32) then
	begin
		CurrentSpeed := NewSpeed;

		if NewSpeed = 0 then
			RenderInfo.HasBeenPlayed := True;
		if (DoChange) and (RenderMode = RENDER_NONE) and (Assigned(OnSpeedChange)) then
			OnSpeedChange(NewSpeed, CurrentBPM);
	end
	else
	begin
		// CIA doesn't refresh its registers until the next interrupt, so change it later
		SetBPMFlag := NewSpeed;

		if (DoChange) and (RenderMode = RENDER_NONE) and (Assigned(OnSpeedChange)) then
			OnSpeedChange(CurrentSpeed, NewSpeed);
	end;
end;

procedure TPTModule.Arpeggio(var ch: TPTChannel);
var
	i: Integer;
	dat: Byte;
	arpPointer: PArrayOfSmallInt;
begin
	if not ch.Paula.HasSound then Exit;

	dat := Counter mod 3;

	case dat of
		0: begin ch.Paula.SetPeriod(ch.n_period); Exit; end;
		1: dat := (ch.Note.Parameter and $F0) shr 4;
		2: dat := ch.Note.Parameter and $0F;
	end;

	arpPointer := @PeriodTable[37 * ch.n_finetune];
	for i := 0 to 36 do
	begin
		if (ch.n_period >= arpPointer[i]) then
		begin
			ch.Paula.SetPeriod(arpPointer[i + dat]);
			Break;
		end;
	end;
end;

procedure TPTModule.PortaUp(var ch: TPTChannel);
begin
	if not ch.Paula.HasSound then Exit;

	Dec(ch.n_period, ((ch.Note.Parameter) and LowMask));
	LowMask := $FF;

	if ((ch.n_period and $0FFF) < 113) then
		ch.n_period := (ch.n_period and $F000) or 113;

	ch.Paula.SetPeriod(ch.n_period and $0FFF);
end;

procedure TPTModule.PortaDown(var ch: TPTChannel);
begin
	if not ch.Paula.HasSound then Exit;

	Inc(ch.n_period, ((ch.Note.Parameter) and LowMask));
	LowMask := $FF;

	if ((ch.n_period and $0FFF) > 856) then
		ch.n_period := (ch.n_period and $F000) or 856;

	ch.Paula.SetPeriod(ch.n_period and $0FFF);
end;

procedure TPTModule.FilterOnOff(var ch: TPTChannel);
begin
	LEDStatus := (ch.Note.Parameter and $01) = 0; // !(ch.n_cmd and $0001);
	if Assigned(OnFilter) then
		OnFilter(AUDIOEVENT_FILTER, Word(LEDStatus), 0);
end;

procedure TPTModule.FinePortaUp(var ch: TPTChannel);
begin
	if Counter = 0 then
	begin
		LowMask := $0F;
		PortaUp(ch);
	end;
end;

procedure TPTModule.FinePortaDown(var ch: TPTChannel);
begin
	if Counter = 0 then
	begin
		LowMask := $0F;
		PortaDown(ch);
	end;
end;

procedure TPTModule.SetTonePorta(var ch: TPTChannel);
var
	i: Integer;
	note: Word;
	portaPointer: PArrayOfSmallInt;
begin
	if not ch.Paula.HasSound then Exit;

	note := ch.n_note and $0FFF;
	portaPointer := @PeriodTable[37 * ch.n_finetune];

	i := 0;
	while True do
	begin
		// portaPointer[36] := 0, so i=36 is safe
		if (note >= portaPointer[i]) then
			Break;
		Inc(i);
		if (i >= 37) then
		begin
			i := 35;
			Break;
		end;
	end;

	if ((ch.n_finetune and 8) <> 0) and (i <> 0) then
		Dec(i);

	ch.n_wantedperiod := portaPointer[i];
	ch.n_toneportdirec := 0;

	if (ch.n_period = ch.n_wantedperiod) then
		ch.n_wantedperiod := 0
	else
	if (ch.n_period > ch.n_wantedperiod) then
		ch.n_toneportdirec := 1;
end;

procedure TPTModule.TonePortNoChange(var ch: TPTChannel);
var
	i: Integer;
	portaPointer: PArrayOfSmallInt;
begin
	if not ch.Paula.HasSound then Exit;

	if ch.n_wantedperiod <> 0 then
	begin
		if ch.n_toneportdirec <> 0 then
		begin
			Dec(ch.n_period, ch.n_toneportspeed);
			if ch.n_period <= ch.n_wantedperiod then
			begin
				ch.n_period := ch.n_wantedperiod;
				ch.n_wantedperiod := 0;
			end;
		end
		else
		begin
			Inc(ch.n_period, ch.n_toneportspeed);
			if ch.n_period >= ch.n_wantedperiod then
			begin
				ch.n_period := ch.n_wantedperiod;
				ch.n_wantedperiod := 0;
			end;
		end;

		if (ch.n_glissfunk and $0F) = 0 then
			ch.Paula.SetPeriod(ch.n_period)
		else
		begin
			portaPointer := @PeriodTable[37 * ch.n_finetune];
			i := 0;
			while True do
			begin
				// portaPointer[36] = 0, so i=36 is safe
				if ch.n_period >= portaPointer[i] then
					Break;
				Inc(i);
				if i >= 37 then
				begin
					i := 35;
					Break;
				end;
			end;
			ch.Paula.SetPeriod(portaPointer[i]);
		end;
	end;
end;

procedure TPTModule.TonePortamento(var ch: TPTChannel);
begin
	if (ch.Note.Parameter) <> 0 then
		ch.n_toneportspeed := ch.Note.Parameter;

	TonePortNoChange(ch);
end;

procedure TPTModule.VibratoNoChange(var ch: TPTChannel);
var
	vibratoTemp: Byte;
	vibratoData: Word;
begin
	if not ch.Paula.HasSound then Exit;

	vibratoTemp := (ch.n_vibratopos div 4) and 31;
	vibratoData := ch.n_wavecontrol and 3;

	if vibratoData = 0 then
		vibratoData := VibratoTable[vibratoTemp]
	else
	begin
		if vibratoData = 1 then
		begin
			if ch.n_vibratopos < 0 then
				vibratoData := 255 - (vibratoTemp * 8)
			else
				vibratoData := vibratoTemp * 8;
		end
		else
			vibratoData := 255;
	end;

	vibratoData := (vibratoData * (ch.n_vibratocmd and $0F)) div 128;

	if (ch.n_vibratopos < 0) then
		vibratoData := ch.n_period - vibratoData
	else
		vibratoData := ch.n_period + vibratoData;

	ch.Paula.SetPeriod(vibratoData);

	Inc(ch.n_vibratopos, ((ch.n_vibratocmd shr 4) * 4));
end;

procedure TPTModule.Vibrato(var ch: TPTChannel);
begin
	if ch.Note.Parameter <> 0 then
	begin
		if (ch.Note.Parameter and $0F) <> 0 then
			ch.n_vibratocmd := (ch.n_vibratocmd and $F0) or (ch.Note.Parameter and $0F);

		if (ch.Note.Parameter and $F0) <> 0 then
			ch.n_vibratocmd := (ch.Note.Parameter and $F0) or (ch.n_vibratocmd and $0F);
	end;

	VibratoNoChange(ch);
end;

procedure TPTModule.TonePlusVolSlide(var ch: TPTChannel);
begin
	TonePortNoChange(ch);
	VolumeSlide(ch);
end;

procedure TPTModule.VibratoPlusVolSlide(var ch: TPTChannel);
begin
	VibratoNoChange(ch);
	VolumeSlide(ch);
end;

procedure TPTModule.Tremolo(var ch: TPTChannel);
var
	tremoloTemp: ShortInt;
	tremoloData: SmallInt;
begin
	if (ch.Note.Parameter) <> 0 then
	begin
		if (ch.Note.Parameter and $0F) <> 0 then
			ch.n_tremolocmd := (ch.n_tremolocmd and $F0) or (ch.Note.Parameter and $0F);
		if (ch.Note.Parameter and $F0) <> 0 then
			ch.n_tremolocmd := (ch.Note.Parameter and $F0) or (ch.n_tremolocmd and $0F);
	end;

	tremoloTemp := (ch.n_tremolopos div 4) and 31;
	tremoloData := (ch.n_wavecontrol shr 4) and 3;

	if (tremoloData = 0) then
		tremoloData := VibratoTable[tremoloTemp]
	else
	if (tremoloData = 1) then
	begin
		if (ch.n_vibratopos < 0) then // PT bug, should've been n_tremolopos
			tremoloData := 255 - (tremoloTemp * 8)
		else
			tremoloData := tremoloTemp * 8;
	end
	else
		tremoloData := 255;

	tremoloData := (tremoloData * (ch.n_tremolocmd and $0F)) div 64;

	if (ch.n_tremolopos < 0) then
	begin
		tremoloData := ch.n_volume - tremoloData;
		if (tremoloData < 0) then tremoloData := 0;
	end
	else
	begin
		tremoloData := ch.n_volume + tremoloData;
		if (tremoloData > 64) then tremoloData := 64;
	end;

	ch.Paula.SetVolume(tremoloData);

	Inc(ch.n_tremolopos, (ch.n_tremolocmd shr 4) * 4);
end;

procedure TPTModule.SampleOffset(var ch: TPTChannel);
var
	newOffset: Word;
begin
	if (ch.Note.Parameter) <> 0 then
		ch.n_sampleoffset := ch.Note.Parameter;

	newOffset := ch.n_sampleoffset * 128;
	if (ch.n_length <= 32767) and (newOffset < ch.n_length) then
	begin
		Dec(ch.n_length, newOffset);
		Inc(ch.n_start, (newOffset * 2));
	end
	else
		ch.n_length := 1; // this must NOT be set to 0! 1 is the correct value
end;

procedure TPTModule.E_Commands(var ch: TPTChannel);
begin
	case (ch.Note.Parameter shr 4) of
		$00: FilterOnOff(ch);
		$01: FinePortaUp(ch);
		$02: FinePortaDown(ch);
		$03: SetGlissControl(ch);
		$04: SetVibratoControl(ch);
		$05: SetFineTune(ch);
		$06: JumpLoop(ch);
		$07: SetTremoloControl(ch);
		$08: KarplusStrong(ch);
		$09: RetrigNote(ch);
		$0A: VolumeFineUp(ch);
		$0B: VolumeFineDown(ch);
		$0C: NoteCut(ch);
		$0D: NoteDelay(ch);
		$0E: PatternDelay(ch);
		$0F: FunkIt(ch);
	end;
end;

procedure TPTModule.CheckMoreEffects(var ch: TPTChannel);
begin
	if not PTOptions.EditorInvertLoop then
		UpdateFunk(ch);

	case ch.Note.Command of
		$09: SampleOffset(ch);
		$0B: PositionJump(ch);
		$0D: PatternBreak(ch);
		$0E: E_Commands(ch);
		$0F: SetSpeed(ch.Note.Parameter);
		$0C: VolumeChange(ch);
	else
		ch.Paula.SetPeriod(ch.n_period);
	end;
end;

procedure TPTModule.CheckEffects(var ch: TPTChannel);
begin
	UpdateFunk(ch);

	if (ch.Note.Command <> 0) or (ch.Note.Parameter <> 0) then
	begin
		case (ch.Note.Command and $0F) of
			$00: Arpeggio(ch);
			$01: PortaUp(ch);
			$02: PortaDown(ch);
			$03: TonePortamento(ch);
			$04: Vibrato(ch);
			$05: TonePlusVolSlide(ch);
			$06: VibratoPlusVolSlide(ch);
			$0E: E_Commands(ch);
			$07: begin
					ch.Paula.SetPeriod(ch.n_period);
					Tremolo(ch);
					Exit; // don't call Paula.SetVolume with tremolo
				 end;
			$0A: begin
					ch.Paula.SetPeriod(ch.n_period);
					VolumeSlide(ch);
				 end;
		else
			ch.Paula.SetPeriod(ch.n_period);
		end;
	end;

	ch.Paula.SetVolume(ch.n_volume);
end;

procedure TPTModule.SetPeriod(var ch: TPTChannel);
var
	n, i: Integer;
begin
	n := -1;
	for i := 0 to 36 do // PeriodTable[36] = 0, so i=36 is safe
		if ch.n_note >= PeriodTable[i] then
		begin
			n := i;
			Break;
		end;

	ch.n_period := PeriodTable[37 * ch.n_finetune + n];

	//if ((ch.n_cmd and $0FF0) <> $0ED0) then // no note delay
	if (ch.Note.Command <> $E) or ((ch.Note.Parameter and $F0) <> $D0) then
	begin
		if (ch.n_wavecontrol and $04) = 0 then ch.n_vibratopos := 0;
		if (ch.n_wavecontrol and $40) = 0 then ch.n_tremolopos := 0;

		// pt2play <1.3
		{if ch.n_length = 0 then
		begin
			ch.n_loopstart := -1;
			ch.n_length := 1; // this must NOT be set to 0! 1 is the correct value
			ch.n_replen := 1;
		end;}

		with ch.Paula do
		begin
			SetLength(ch.n_length);
			SetData(ch.n_sample, ch.n_start);

			// pt2play 1.3+
			if ch.n_start < 0 then
			begin
				ch.n_loopstart := -1;
				SetLength(1);
				ch.n_replen := 1;
			end;

			SetPeriod(ch.n_period);
			RestartDMA;
		end;
	end;

	CheckMoreEffects(ch);
end;

{$IFDEF DEBUG}
{$R+}
{$ENDIF}

// ==========================================================================
// Playback
// ==========================================================================

procedure TPTModule.PlayVoice(var ch: TPTChannel);
var
	sample: Byte;
	srepeat: Word;
	Sam: TSample;
begin
	if (ch.n_note = 0) and (ch.Note.Command = 0) and (ch.Note.Parameter = 0) then
		ch.Paula.SetPeriod(Word(ch.n_period));

	ch.Note := @Notes[PlayPos.Pattern, ch.n_index, PlayPos.Row];

	if ch.Note.Pitch in [1..36] then
	begin
		ch.n_note := PeriodTable[ch.Note.Pitch-1] and $0FFF;
		if Assigned(OnSamplePlay) then
			OnSamplePlay(AUDIOEVENT_SAMPLE, ch.Note.Sample, ch.Note.Pitch);
	end
	else
	begin
		ch.n_note := 0;
		if ch.Note.Pitch > 36 then
			SetPeriod(ch); // kill audio on invalid note
	end;

	// SAFETY BUG FIX: don't handle samples >31
	sample := ch.Note.Sample;
	if (sample >= 1) and (sample <= 31) then
	begin
		Dec(sample);
		Sam := Samples[sample];

		ch.n_sample   := sample;
		ch.n_start    := 0; // Data[0]
		ch.n_finetune := Sam.Finetune;
		ch.n_volume   := Sam.Volume;
		ch.n_length   := Sam.Length and $FFFF; // limit to 127K
		ch.n_replen   := Sam.LoopLength;
		srepeat       := Sam.LoopStart;

		ch.Paula.Sample := sample;
		if ch.Paula.Enabled then
			Sam.Age := 3;

		if srepeat > 0 then
		begin
			ch.n_loopstart := ch.n_start + (srepeat * 2);
			ch.n_wavestart := ch.n_loopstart;
			ch.n_length    := srepeat + ch.n_replen;
		end
		else
		begin
			ch.n_loopstart := ch.n_start;
			ch.n_wavestart := ch.n_start;
		end;

		if ch.n_length = 0 then
		begin
			ch.Paula.QueuedSample := -1;
			ch.n_loopstart := -1;
			ch.n_wavestart := -1;
		end;
	end;

	if ch.n_note > 0 then
	begin
		case ch.Note.Command of

			$3, $5:
			begin
				SetTonePorta(ch);
				CheckMoreEffects(ch);
			end;

			$9:
			begin
				CheckMoreEffects(ch);
				SetPeriod(ch);
			end;

			$E:
			begin
				if (ch.Note.Parameter and $F0) = $50 then
					SetFineTune(ch);
				SetPeriod(ch);
			end;
		else
			SetPeriod(ch);
		end;
	end
	else
		CheckMoreEffects(ch);
end;

procedure TPTModule.NextPosition(FromEditor: Boolean = False);
var
	NewOrder: Byte;
begin
	PlayPos.Row    := PBreakPosition;
	PBreakPosition := 0;
	PosJumpAssert  := False;

	if PlayMode = PLAY_SONG then
	begin
		NewOrder := (PlayPos.Order + 1) and $7F;
		if (NewOrder >= Info.OrderCount) then
		begin
			NewOrder := 0;
			RenderInfo.HasBeenPlayed := True;
		end;

		if FromEditor then
			Play(NewOrder, 0)
		else
		begin
			PlayPos.Order := NewOrder;
			PlayPos.Pattern := OrderList[NewOrder];
		end;

		if RenderMode = RENDER_NONE then
		begin
			PostMessagePtr(MSG_ORDERCHANGE, @PlayPos);
			if Assigned(OnOrderChange) then
				OnOrderChange(AUDIOEVENT_ORDER, PlayPos.Order, PlayPos.Row);
		end;
	end;
end;

// called (SongBPM/2.5) times a second (duration: 1000/(SongBPM/2.5) milliseconds)
//
procedure TPTModule.IntMusic;
var
	i: Integer;
begin
	if (RenderMode <> RENDER_NONE) and (RenderMode <> RENDER_SAMPLE) then
	begin
		RenderInfo.HasBeenPlayed := False;
		if Counter = 0 then
			RenderInfo.RowVisitTable[PlayPos.Order, PlayPos.Row] := True;
	end;

	// PT quirk: CIA refreshes its timer values on the next interrupt, so do the real tempo change here
	if SetBPMFlag > 0 then
	begin
		CurrentBPM := SetBPMFlag;
		SetReplayerBPM(CurrentBPM);
	end;

	Inc(Counter);

	if Counter < CurrentSpeed then
	begin
		CheckEffects(Channel[0]);
		CheckEffects(Channel[1]);
		CheckEffects(Channel[2]);
		CheckEffects(Channel[3]);
		if PosJumpAssert then
			NextPosition;
		Exit;
	end;

	Counter := 0;

	if PattDelTime2 = 0 then
	begin
		for i := 0 to AMOUNT_CHANNELS-1 do
		begin
			PlayVoice(Channel[i]);
			Channel[i].Paula.SetVolume(Channel[i].n_volume);
			// these take effect after the current DMA cycle is done
			Channel[i].Paula.SetData(Channel[i].n_sample, Channel[i].n_loopstart);
			Channel[i].Paula.SetLength(Channel[i].n_replen);
		end;
	end
	else
	begin
		CheckEffects(Channel[0]);
		CheckEffects(Channel[1]);
		CheckEffects(Channel[2]);
		CheckEffects(Channel[3]);
	end;

	Inc(PlayPos.Row);

	if PattDelTime <> 0 then
	begin
		PattDelTime2 := PattDelTime;
		PattDelTime  := 0;
	end;

	if PattDelTime2 <> 0 then
	begin
		Dec(PattDelTime2);
		if PattDelTime2 <> 0 then
			Dec(PlayPos.Row);
	end;

	if PBreakFlag then
	begin
		if RenderMode = RENDER_SAMPLE then
		begin
			Stop;
			Exit;
		end;
		PlayPos.Row := PBreakPosition;
		PBreakPosition := 0;
		PBreakFlag := False;
	end;

	if (PosJumpAssert) or (PlayPos.Row > 63) then
		NextPosition
	else
	if RenderMode = RENDER_NONE then
	begin
        PostMessagePtr(MSG_ROWCHANGE, @PlayPos);
		if Assigned(OnRowChange) then
			OnRowChange(AUDIOEVENT_ROW, PlayPos.Order, PlayPos.Row);
	end
	else
	if PattDelTime2 = 0 then
	begin
		if RenderMode = RENDER_SAMPLE then
		begin
			Inc(RenderInfo.RowsRendered);
			if RenderInfo.RowsRendered > RenderInfo.LastRow then
			begin
				Stop;
				Exit;
			end;
		end
		else
		if RenderInfo.RowVisitTable[PlayPos.Order, PlayPos.Row] then
		begin
			// rendermode <> none
			Inc(RenderInfo.TimesLooped);
			//Log('STOP (Loop=%d) Ord=%d Patt=%d Row=%d', [RenderInfo.TimesLooped, PlayPos.Order, PlayPos.Pattern, PlayPos.Row]);
			if (RenderMode = RENDER_LENGTH) and (RenderInfo.TimesLooped > RenderInfo.LoopsWanted) then
				Stop
			else
				ClearRowVisitTable;
		end;
	end;
end;

procedure TPTModule.SetTempo(bpm: Word);
begin
	if bpm > 0 then
	begin
		CurrentBPM := bpm;
		SetReplayerBPM(CurrentBPM);
	end;
end;

procedure TPTModule.ApplyAudioSettings;
begin
	NormFactor       := Max(PTOptions.Amplification, 0.01);
	FilterHighPass   := PTOptions.FilterHighPass;
	FilterLowPass    := PTOptions.FilterLowPass;

	if StereoSeparation <> PTOptions.StereoSeparation then
	begin
		StereoSeparation := PTOptions.StereoSeparation;
		CalculatePans(StereoSeparation);
	end;
end;

procedure TPTModule.Reset;
var
	i: Integer;
begin
	ClearLossyIntegrator(@FilterLo);
	ClearLossyIntegrator(@FilterHi);
	ClearLEDFilter(@FilterLED);

	ApplyAudioSettings;

	SetBPMFlag     := 0;
	PattDelTime    := 0;
	PattDelTime2   := 0;
	PBreakPosition := 0;
	PosJumpAssert  := False;
	PBreakFlag     := False;
	LowMask        := $FF;

	VBlankMode := PTOptions.CIAmode;

	LEDStatus := False;
	Counter := CurrentSpeed;

	if RenderMode <> RENDER_NONE then
		ClearRowVisitTable;

	for i := 0 to 30 do
		Samples[i].Age := -1;

	for i := 0 to AMOUNT_CHANNELS-1 do
	begin
		Channel[i].Reset;
		ZeroBlep(@Blep[i]);
		ZeroBlep(@BlepVol[i]);
	end;
end;

procedure TPTModule.InitPlay(pattern: Byte);
var
	i: Integer;
begin
	Reset;

	for i := 0 to AMOUNT_CHANNELS-1 do
	begin
		Channel[i].Paula.Kill;
		Channel[i].Note := @Notes[pattern, i, 0];
	end;

	RenderInfo.SamplesRendered := 0;
	RenderInfo.RowsRendered := 0;
	RenderInfo.OrderChanges := 0;
	RenderInfo.TimesLooped := 0;
	RenderInfo.HasBeenPlayed := False;
	RenderInfo.Canceled := False;

	if RenderMode = RENDER_LENGTH then Exit;

	DisableMixer := False;
	PlayStarted := Now;

	if Assigned(OnPlayModeChange) then
		OnPlayModeChange(AUDIOEVENT_PLAY, PlayMode, PlayMode);
end;

procedure TPTModule.PlayPattern(pattern: Byte; row: Byte = 0);
begin
	Stop;

	PlayPos.Pattern := pattern;
	PlayPos.Row     := row;

	if RenderMode = RENDER_NONE then
	begin
		PostMessagePtr(MSG_ROWCHANGE, @PlayPos);
		if Assigned(OnRowChange) then
			OnRowChange(AUDIOEVENT_ROW, PlayPos.Order, PlayPos.Row);
	end;

	PlayMode := PLAY_PATTERN;
	InitPlay(pattern);
end;

procedure TPTModule.Play(order: Byte = 0; row: Byte = 0);
begin
	Stop;

	PlayPos.Pattern := OrderList[order];
	PlayPos.Row := row;
	PlayPos.Order := order;

	PlayMode := PLAY_SONG;
	InitPlay(OrderList[0]);

	if RenderMode = RENDER_NONE then
	begin
		PostMessagePtr(MSG_ROWCHANGE, @PlayPos);
		if Assigned(OnRowChange) then
			OnRowChange(AUDIOEVENT_ROW, PlayPos.Order, PlayPos.Row);
	end;
end;

procedure TPTModule.Stop;
var
	i: Integer;
begin
	DisableMixer := True;
	RenderInfo.HasBeenPlayed := True;

	if RenderMode = RENDER_NONE then
		while Mixing do;

	PlayMode := PLAY_STOPPED;

	if RenderMode = RENDER_NONE then
		if Assigned(OnPlayModeChange) then
			OnPlayModeChange(AUDIOEVENT_PLAY, PlayMode, PlayMode);

	for i := 0 to AMOUNT_CHANNELS-1 do
	begin
		Channel[i].Paula.Kill;
		Channel[i].Note := @Notes[OrderList[0], i, 0];
	end;

{	if PTOptions.ResetTempo then
	begin
		SetSpeed(DefaultSpeed, False);
		SetTempo(DefaultTempo);
	end;}

	Counter := 0;
	DisableMixer := False;
end;

procedure TPTModule.Pause(Pause: Boolean);
begin
	DisableMixer := Pause;
	if Pause then
		while Mixing do;
end;

procedure TPTModule.PlayNote(Note: PNote; Chan: Byte; Vol: Byte = 255);
var
	sample: TSample;
	srepeat: Word;
	ch: TPTChannel;
begin
	if (Chan >= AMOUNT_CHANNELS) then Exit;
	if not (Note.Sample in [1..31]) then Exit;

	ch := Channel[Chan];
	if not ch.Enabled then Exit;

	sample := Samples[Note.Sample-1];
	if (sample.Length = 0) or (Note.Pitch = 0) then
	begin
		ch.Paula.Kill;
		Exit;
	end;

	DisableMixer := True;

	ch.Note := Note;
	ch.n_note := PeriodTable[Note.Pitch-1 mod 37] and $0FFF;

	ch.n_sample   := Note.Sample-1;
	ch.n_start    := 0; // Data[0]
	ch.n_period   := PeriodTable[(37 * sample.Finetune) + Note.Pitch - 1];
	ch.n_finetune := sample.Finetune;
	if Vol > 64 then
		ch.n_volume := sample.Volume
	else
		ch.n_volume := Min(Vol, 64);
	ch.n_length   := sample.Length;
	ch.n_replen   := sample.LoopLength;
	srepeat       := sample.LoopStart;

	if (srepeat > 0) then
	begin
		ch.n_loopstart := ch.n_start + (srepeat * 2);
		ch.n_wavestart := ch.n_loopstart;
		ch.n_length    := srepeat + ch.n_replen;
	end
	else
	begin
		ch.n_loopstart := ch.n_start;
		ch.n_wavestart := ch.n_start;
	end;

	CheckMoreEffects(ch); // parse volume fx

	with ch do
	begin
		Paula.PlayPos := -1;

		Paula.SetData(n_sample, n_start);
		Paula.SetLength(n_length);
		Paula.SetPeriod(n_period);
		Paula.SetVolume(n_volume);
		Paula.RestartDMA;

		// these take effect after the current DMA cycle is done
		Paula.SetData(n_sample, n_loopstart);
		Paula.SetLength(n_replen);
	end;

	DisableMixer := False;
end;

procedure TPTModule.PlaySample(_note, _sample, _channel: Byte; _volume: ShortInt = -1;
	_start: Integer = 0; _length: Integer = 0);
var
	S: TSample;
	srepeat: Word;
begin
	if (_channel >= AMOUNT_CHANNELS) then Exit;
	if not (_sample in [1..32]) then Exit;

	Dec(_sample);
	S := Samples[_sample];

	with Channel[_channel] do
	begin
		if not Enabled then Exit;

		DisableMixer := True;

		n_period   := PeriodTable[(37 * S.Finetune) + _note - 1];
		n_sample   := _sample;
		n_start    := _start;
		n_finetune := S.Finetune;
		if _volume >= 0 then
			n_volume := _volume
		else
			n_volume := S.Volume;
		if _length = 0 then
			n_length := S.Length
		else
			n_length := _length;

		n_replen := S.LoopLength;
		srepeat  := S.LoopStart;

		Paula.PlayPos := -1;

		if n_length = 0 then
		begin
			Paula.Kill;
			DisableMixer := False;
			Exit;
		end;

		// just play a section of sample, unlooped
		if _length > 0 then
		begin
			//srepeat := 0;
			n_loopstart := 0;
			n_wavestart := n_start;
			n_replen := 1;
		end
		else
		if srepeat > 0 then
		begin
			n_loopstart := n_start + (srepeat * 2);
			n_wavestart := n_loopstart;
			n_length    := srepeat + n_replen;
		end
		else
		begin
			n_loopstart := n_start;
			n_wavestart := n_start;
		end;

		Paula.SetData(_sample, n_start);
		Paula.SetLength(n_length);
		Paula.SetPeriod(n_period);
		Paula.SetVolume(n_volume);
		Paula.RestartDMA;

		// these take effect after the current DMA cycle is done
		Paula.SetData(_sample, n_loopstart);
		Paula.SetLength(n_replen);
	end;

	DisableMixer := False;
end;

{$R-}
procedure TPTModule.MixSampleBlock(streamOut: Pointer; numSamples: Cardinal);
var
	i: Integer;
	j: Word;
	sndOut: ^TArrayOfSmallInt absolute streamOut;
	Amp, tempSample, tempVolume: Single;
	outSample: array [0..1] of Single;
	masterBufferL, masterBufferR: array of Single;
	v: TPaulaVoice;
	bSmp, bVol: PBlep;
	Sam: TSample;
const
	SCALER = 1.0 / 128.0;
begin
	Amp := -32767.0 / NormFactor; // negative because signal phase is flipped on Amiga
	ClippedSamples := 0;

	SetLength(masterBufferL, numSamples + 2);
	SetLength(masterBufferR, numSamples + 2);

	for i := 0 to AMOUNT_CHANNELS-1 do
	begin
		bSmp := @Blep[i];
		bVol := @BlepVol[i];
		v := Channel[i].Paula;

		if v.DELTA > 0.0 then
		begin
			for j := 0 to numSamples-1 do
			begin
				if (v.DMA_DAT = nil) or (not v.HasSound) then
				begin
					tempSample := 0.0;
					tempVolume := 0.0;
				end
				else
				begin
					tempSample := v.DMA_DAT[v.DMA_POS] * SCALER;
					tempVolume := v.SRC_VOL;
				end;

				// Blep reduces some unwanted aliasing (closer to real Amiga)
				if (tempSample <> bSmp.lastValue) then
				begin
					if ((v.LASTDELTA > 0.0) and (v.LASTDELTA > v.LASTFRAC)) then
						BlepAdd(bSmp, v.LASTFRAC / v.LASTDELTA, bSmp.lastValue - tempSample);
					bSmp.lastValue := tempSample;
				end;

				if (tempVolume <> bVol.lastValue) then
				begin
					BlepAdd(bVol, 0.0, bVol.lastValue - tempVolume);
					bVol.lastValue := tempVolume;
				end;

				if bSmp.samplesLeft > 0 then
					tempSample := tempSample + BlepRun(bSmp);
				if bVol.samplesLeft > 0 then
					tempVolume := tempVolume + BlepRun(bVol);

				tempSample := tempSample * tempVolume;

				if not Channel[i].Enabled then
					tempSample := 0;

				masterBufferL[j] := masterBufferL[j] + (tempSample * v.PANL);
				masterBufferR[j] := masterBufferR[j] + (tempSample * v.PANR);

				v.FRAC := v.FRAC + v.DELTA;
				if v.FRAC >= 1.0 then
				begin
					v.FRAC := v.FRAC - 1.0;
					v.LASTFRAC := v.FRAC;
					v.LASTDELTA := v.DELTA;

					if v.Sample < 0 then
						Sam := EmptySample
					else
						Sam := Samples[v.Sample];

					Inc(v.DMA_POS);
					if v.DMA_POS >= v.DMA_LEN then
					begin
						// re-fetch Paula register values now
						v.DMA_POS := 0;
						v.DMA_LEN := v.SRC_LEN;
						v.DMA_DAT := @v.SRC_DAT[0];
						v.Sample  := v.QueuedSample;

						if Sam.LoopLength <= 2 then
							v.PlayPos := -1
						else
							v.PlayPos := v.QueuedOffset;
					end
					else
					if v.PlayPos >= 0 then
						Inc(v.PlayPos);
				end;
			end;
		end;
	end;

	for j := 0 to numSamples-1 do
	begin
		outSample[0] := masterBufferL[j];
		outSample[1] := masterBufferR[j];

		if FilterLowPass then
			LossyIntegrator(@FilterLo, @outSample[0], @outSample[0]);

		if LEDStatus then
			LossyIntegratorLED(@FilterLEDC, @FilterLED, @outSample[0], @outSample[0]);

		if FilterHighPass then
			LossyIntegratorHighPass(@FilterHi, @outSample[0], @outSample[0]);

		outSample[0] := outSample[0] * Amp;
		outSample[1] := outSample[1] * Amp;

		sndOut[j*2]   := CLAMP2(Trunc(outSample[0]), -32768, 32767, ClippedSamples);
		sndOut[j*2+1] := CLAMP2(Trunc(outSample[1]), -32768, 32767, ClippedSamples);
	end;
end;

initialization

	PTOptions.Frequency := 44100;
	PTOptions.Amplification := 5.0;
	PTOptions.StereoSeparation := 10;
	PTOptions.FilterHighPass := True;
	PTOptions.FilterLowPass  := True;
	PTOptions.FilterLed := False;
	PTOptions.CIAmode := False;
	PTOptions.EditorInvertLoop := True;

end.




