unit NES.AudioManager;

{$MODE DELPHI}
{$INCLUDE coredefs.inc}

interface

uses
	Classes, SysUtils,
	SDL2, Basement.Util,
	fpwavwriter, fpwavformat,
	NES.Types, NES.MemoryHandler;

const
	ARA_maxGapMS = 100; // milliseconds
	ARA_maxGap   = 3;
	ARA_maxSubAdjustment = 3600*10;


type
	TAudioRenderer = procedure (Buffer: PUInt8; Len: DWord; Data: Pointer) of Object;

	TPlayRoutine = class(TIMemoryHandler)
	private
		Initialized:     Boolean;

		BufferSize:      Cardinal;
		SampleRate:      Cardinal;
		BytesPerSample:  Byte;

		PreviousLatency: Word;
		WritePosition,
		ReadPosition:    Cardinal;

		Buffer:          TBytes;

		procedure	ReadFromBuffer(output: PByte; len: Cardinal);
		procedure	WriteToBuffer (input:  PByte; len: Cardinal; Backwards: Boolean);
	protected
		Playing:      Boolean;

		CursorGapIndex:  Word;
		CursorGapFilled: Boolean;
		CursorGaps: array[0..59] of Cardinal;

		WavWriter: TWavWriter;
		WavStream: TMemoryStream;
	public
		AudioDevice: TSDL_AudioDeviceID;

		AudioLatency: Word;
		AverageLatency: Single;
		IsStereo: Boolean;
		WavRecording: Boolean;
		BufferUnderrunEventCount: Cardinal;

		constructor	Create; overload;
		destructor 	Destroy; override;

		function 	Init(WantedLatency: Word = 0; Stereo: Boolean = True; Device: String = ''): Boolean;

		procedure	PlayBuffer(SoundBuffer: PInt16; Len: Cardinal; Backwards: Boolean);

		procedure	Play;
		procedure	Pause;
		procedure	Stop;

		procedure	ResetStats;
		procedure	ProcessLatency(ReadPos, WritePos: Cardinal);
		procedure	ProcessEndOfFrame;
		procedure	UpdateSoundSettings;

		function	StartRecording(const Filename: String): Boolean;
		procedure	StopRecording(Flush: Boolean = True);
		procedure	FlushRecording;
	end;

implementation

uses
	Math, Types,
	NES.Config,	NES.Console;

var
	Audio: TPlayRoutine;


procedure AudioCallback(UserData: Pointer; Buffer: PByte; Len: Integer); cdecl;
begin
	FillByte(Buffer^, Len, 0);

	Audio.ReadFromBuffer(Buffer, Len);
end;


constructor TPlayRoutine.Create;
var
	i: Integer;
begin
	inherited Create('APU');

	SampleRate := Configuration.Audio.SampleRate;
	BufferSize := $10000;

	Initialized := False;
	Audio := Self;

	PreviousLatency := 0;
	WritePosition := 0;
	ReadPosition  := 0;

	for i := 0 to High(CursorGaps) do
		CursorGaps[i] := 0;
end;

destructor TPlayRoutine.Destroy;
begin
	StopRecording;
	Stop;
	SDL_CloseAudioDevice(AudioDevice);

	inherited;
end;

function TPlayRoutine.Init(WantedLatency: Word = 0; Stereo: Boolean = True;
	Device: String = ''): Boolean;
var
	RequestedByteLatency: Cardinal;
	desiredSpec, obtainedSpec: TSDL_AudioSpec;
begin
	if Initialized then Exit(True);

	SDL_Init(SDL_INIT_AUDIO);

	BytesPerSample := 2 * IfThen(Stereo, 2, 1);
	IsStereo := Stereo;

	PreviousLatency := WantedLatency;
	AudioLatency := WantedLatency;

	RequestedByteLatency := Trunc((SampleRate * WantedLatency) / 1000 * BytesPerSample);
	BufferSize := Trunc(Ceil(RequestedByteLatency * 2 / $10000) * $10000);
	SetLength(Buffer, BufferSize);
	ZeroMemory(@Buffer[0], BufferSize);

	ZeroMemory(@desiredspec, SizeOf(desiredspec));
	desiredSpec.freq     := SampleRate;
	desiredSpec.format   := AUDIO_S16;
	desiredSpec.channels := IfThen(Stereo, 2, 1);
	desiredSpec.samples  := 1024;
	desiredSpec.callback := @AudioCallback;
	desiredSpec.userdata := @Self;

	try
		if Device = '' then
			AudioDevice := SDL_OpenAudioDevice(nil, 0, @desiredSpec, @obtainedSpec, 0)
		else
			AudioDevice := SDL_OpenAudioDevice(PAnsiChar(Device), 0, @desiredSpec, @obtainedSpec, 0);

		Result := (AudioDevice >= 2);
	except
		Log('[Audio] Device init failure!');
		Result := False;
	end;

	ResetStats;

	Initialized := Result;
	Stop;
end;

procedure TPlayRoutine.ResetStats;
begin
	CursorGapIndex := 0;
	CursorGapFilled := False;
	BufferUnderrunEventCount := 0;
	AverageLatency := 0;
end;

procedure TPlayRoutine.ReadFromBuffer(output: PByte; len: Cardinal);
var
	RemainingBytes: Integer;
	{P1, P2: PInt32;
	Tmp: Int32;}
begin
	if (ReadPosition + len) < BufferSize then
	begin
		CopyMemory(output, @Buffer[ReadPosition], len);
		Inc(ReadPosition, len);
	end
	else
	begin
		RemainingBytes := (BufferSize - ReadPosition);
		CopyMemory(output, @Buffer[ReadPosition], RemainingBytes);
		CopyMemory(output + RemainingBytes, @Buffer[0], len - RemainingBytes);
		ReadPosition := len - RemainingBytes;
	end;

	if (ReadPosition >= WritePosition) and ((ReadPosition - WritePosition) < (BufferSize div 2)) then
		Inc(BufferUnderrunEventCount);

{	if (Console <> nil) and (Console.Rewind) then
	begin
		p1 := @output[0];
		p2 := p1 + (len div 4) - 1;
		while p1 < p2 do
		begin
			Tmp := p1^;
			p1^ := p2^;
			p2^ := Tmp;
			Inc(p1);
			Dec(p2);
		end;
	end;}
end;

procedure TPlayRoutine.WriteToBuffer(input: PByte; len: Cardinal; Backwards: Boolean);
var
	RemainingBytes: Integer;
begin
	if (WritePosition + len) < BufferSize then
	begin
		if not Backwards then
			CopyMemory(@Buffer[WritePosition], input, len)
		else
		begin
			for RemainingBytes := (len div 2)-1 downto 0 do
			begin
				Buffer[WritePosition+(RemainingBytes*2)]   := input^;
				Inc(input);
				Buffer[WritePosition+(RemainingBytes*2)+1] := input^;
				Inc(input);
			end;
		end;

		Inc(WritePosition, len);
	end
	else
	begin
		RemainingBytes := BufferSize - WritePosition;
		CopyMemory(@Buffer[WritePosition], input, RemainingBytes);
		CopyMemory(@Buffer[0], input + RemainingBytes, len - RemainingBytes);
		WritePosition := len - RemainingBytes;
	end;

	if (WavRecording) and (WavWriter <> nil) and (not Console.IsRunAheadFrame) then
	begin
		WavStream.Write(input^, len);
		if WavStream.Size >= 1024*4*100 then
			FlushRecording;
	end;
end;

procedure TPlayRoutine.PlayBuffer(SoundBuffer: PInt16; Len: Cardinal; Backwards: Boolean);
var
	ByteLatency, PlayWriteByteLatency: Integer;
begin
	UpdateSoundSettings;

	WriteToBuffer(PByte(SoundBuffer), Len * BytesPerSample, Backwards);

	ByteLatency := Round((SampleRate * PreviousLatency) / 1000 * BytesPerSample);
	PlayWriteByteLatency := WritePosition - ReadPosition;
	if PlayWriteByteLatency < 0 then
		PlayWriteByteLatency := BufferSize - ReadPosition + WritePosition;

	// Start playing
	if (not Playing) and (PlayWriteByteLatency > ByteLatency) then
		Play;
end;

procedure TPlayRoutine.Play;
begin
	if not Initialized then Init;
	if not Playing then
	begin
		SDL_PauseAudioDevice(AudioDevice, 0);
		Playing := True;
	end;
end;

procedure TPlayRoutine.Pause;
begin
	if (Initialized) and (Playing) then
	begin
		SDL_PauseAudioDevice(AudioDevice, 1);
		Playing := False;
	end;
end;

procedure TPlayRoutine.Stop;
begin
	Pause;

	ReadPosition  := 0;
	WritePosition := 0;

	ResetStats;
end;

procedure TPlayRoutine.ProcessLatency(ReadPos, WritePos: Cardinal);
var
	i: Integer;
	CursorGap: Cardinal;
	GapSum: Cardinal;
begin
	// Record latency between read & write cursors once per frame
	if WritePos < ReadPos then
		CursorGap := WritePos - ReadPos + BufferSize
	else
		CursorGap := WritePos - ReadPos;

	CursorGaps[CursorGapIndex] := CursorGap;
	Inc(CursorGapIndex);
	if CursorGapIndex > High(CursorGaps) then
	begin
		CursorGapIndex := 0;
		CursorGapFilled := True;
	end;

	if CursorGapFilled then
	begin
		// Once we have 60+ frames worth of data to work with, adjust playback frequency by +/- 0.5%
		// To speed up or slow down playback in order to reach our latency goal.
		GapSum := 0;
		for i := 0 to High(CursorGaps) do
			Inc(GapSum, CursorGaps[i]);
		AverageLatency := ((GapSum / 60) / BytesPerSample) / SampleRate * 1000;
	end;
end;

procedure TPlayRoutine.ProcessEndOfFrame;
var
	EmulationSpeed: Word;
begin
	ProcessLatency(ReadPosition, WritePosition);
	EmulationSpeed := Console.GetEmulationSpeed;

	// Latency is way off (over 50ms gap), stop audio & start again
	if (AverageLatency > 0) and (EmulationSpeed <= 100) and
		(Abs(AverageLatency - AudioLatency) >= ARA_maxGapMS) then
		begin
			Stop;
			Inc(BufferUnderrunEventCount);
		end;
end;

procedure TPlayRoutine.UpdateSoundSettings;
begin
	// TODO
end;

procedure TPlayRoutine.FlushRecording;
begin
	if (WavWriter <> nil) and (WavStream <> nil) then
	begin
		WavWriter.WriteBuf(WavStream.Memory^, WavStream.Size);
		WavStream.Clear;
	end;
end;

function TPlayRoutine.StartRecording(const Filename: String): Boolean;
begin
	if (WavWriter <> nil) then Exit(False);

	WavStream := TMemoryStream.Create;
	WavWriter := TWavWriter.Create;

	WavWriter.fmt.Format := AUDIO_FORMAT_PCM;
	WavWriter.fmt.BitsPerSample := 16;
	WavWriter.fmt.Channels := 2;
	WavWriter.fmt.SampleRate := Samplerate;
	WavWriter.fmt.ByteRate := Samplerate * 4;
	WavWriter.fmt.BlockAlign := 2;

	WavRecording := WavWriter.StoreToFile(Filename);

	if not WavRecording then
		StopRecording(False);

	Result := WavRecording;
end;

procedure TPlayRoutine.StopRecording(Flush: Boolean = True);
begin
	WavRecording := False;
	if Flush then
		FlushRecording;
	WavWriter.Free;
	WavStream.Free;
	WavWriter := nil;
	WavStream := nil;
end;


end.

