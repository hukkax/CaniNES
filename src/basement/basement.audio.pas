unit Basement.Audio;

{$mode delphi}
{$I basement.inc}

{.$DEFINE BASS}

{.$DEFINE SDL_DEPRECATED} // use deprecated audio init calls?

// Include code for rendering the audio output to a WAV file?
{.$DEFINE RENDERWAV}

interface

uses
	Classes, SysUtils, FGL,
	{$I basement-uses_audio.inc}
	{$IFDEF RENDERWAV}
	fpwavwriter, fpwavformat,
	{$ENDIF}
	Basement.Util,
	NES.MemoryHandler;

{$IFDEF BASS}
const
    MINIMUM_AUDIOBUFFER_LENGTH = 45; // in milliseconds
{$ENDIF}

type
	TAudioRenderer = procedure (Buffer: PUInt8; Len: DWord; Data: Pointer) of Object;

	TPlayRoutine = class(TIMemoryHandler)
	public
//		function	LoadFromFile(const Filename: String): Boolean; virtual; abstract;
//		procedure	Render(Buffer: System.PUInt8; Len: System.DWord; Data: Pointer); virtual; abstract;
	end;

	TAudioHandler = class
	private
		Initialized: Boolean;
		RegisteredPlayers: TFPGObjectList<TPlayRoutine>;
	public
		Playing:     Boolean;
		Player:	     TPlayRoutine;
		{$IFNDEF BASS}
		AudioDevice: TSDL_AudioDeviceID;
		{$ENDIF}

		constructor	Create;
		destructor 	Destroy; override;

		function 	Init(BufferSize: DWord = 0; Device: String = ''): Boolean;

		function 	PreparePlayer(aPlayer: TPlayRoutine): Boolean;
		procedure	RegisterPlayer(aPlayer: TPlayRoutine; const PlayerName, ExtensionMask: String);
		procedure	UnregisterPlayer(aPlayer: TPlayRoutine);

		procedure	Play;
		procedure	Stop;

		{$IFDEF RENDERWAV}
		function 	RenderToWAV(const Filename: String): Boolean;
		procedure 	StopRendering;
		{$ENDIF}
	end;


	function Clamp(val: Single): Integer; inline;


var
	Audio: TAudioHandler;

implementation

{$IFDEF BASS}
uses
	Math;
{$ENDIF}

{$IFDEF RENDERWAV}
var
	Wav: TWavWriter;
{$ENDIF}


function Clamp(val: Single): Integer; inline;
begin
	Result := Trunc(val);
	if Result >  32767 then Result :=  32767
	else
	if Result < -32768 then Result := -32768;
end;


{$IFDEF BASS}
function AudioCallback(Handle: HSTREAM; Buffer: Pointer; Len: DWord; Data: Pointer)
: DWord; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
{$ELSE}
procedure AudioCallback(Data: Pointer; Buffer: PUInt8; Len: Integer); cdecl;
{$ENDIF}
begin
	{$IFDEF BASS}
	Result := Len;
	{$ENDIF}
	FillChar(Buffer^, Len, 0);

	if (Audio.Playing) and (Assigned(Audio.Player)) then
	begin
//		Audio.Player.Render(Buffer, Len, Data); // !!! RE-ENABLE, not abstract
		{$IFDEF RENDERWAV}
		if Wav <> nil then Wav.WriteBuf(Buffer^, len);
		{$ENDIF}
	end;
end;

function TAudioHandler.PreparePlayer(aPlayer: TPlayRoutine): Boolean;
begin
	{$IFDEF DEBUG}
	Log('PreparePlayer: ' + aPlayer.ClassName);
	{$ENDIF}
	Player := aPlayer;
	Result := True;
end;

procedure TAudioHandler.RegisterPlayer(aPlayer: TPlayRoutine; const PlayerName, ExtensionMask: String);
begin
	{$IFDEF DEBUG}
	Log('RegisterPlayer: ' + aPlayer.ClassName);
	{$ENDIF}

	if RegisteredPlayers.IndexOf(aPlayer) < 0 then
	begin
		RegisteredPlayers.Add(aPlayer);
		{$IFDEF DEBUG}
		Log(' = Added');
		{$ENDIF}
	end
	{$IFDEF DEBUG}
	else
		Log(' = already registered');
	{$ENDIF}
end;

procedure TAudioHandler.UnregisterPlayer(aPlayer: TPlayRoutine);
begin
	{$IFDEF DEBUG}
	Log('UnregisterPlayer: ' + aPlayer.ClassName);
	{$ENDIF}

	if RegisteredPlayers.IndexOf(aPlayer) >= 0 then
	begin
		RegisteredPlayers.Delete(RegisteredPlayers.IndexOf(aPlayer));
		{$IFDEF DEBUG}
		Log(' = Removed');
		{$ENDIF}
	end;
	if Player = aPlayer then
		Player := nil;
end;

function TAudioHandler.Init(BufferSize: DWord = 0; Device: String = ''): Boolean;
{$IFNDEF BASS}
var
	desiredSpec, obtainedSpec: TSDL_AudioSpec;
begin
	{$IFDEF DEBUG}
	Log('Audio.Init');
	{$ENDIF}

//	PlayRoutine := Callback;
	if Initialized then Exit(True);

	SDL_Init(SDL_INIT_AUDIO);

	Stop;

	if BufferSize < 128 then BufferSize := 1024 * 1;

	ZeroMemory(@desiredspec, SizeOf(desiredspec));
	desiredSpec.freq     := 44100;
	desiredSpec.format   := AUDIO_S16;
	desiredSpec.channels := 2;
	desiredSpec.samples  := BufferSize;
	desiredSpec.callback := nil; //@AudioCallback;
	desiredSpec.userdata := nil;

	try
		{$IFDEF SDL_DEPRECATED}
		Result := (SDL_OpenAudio(@desiredSpec, @obtainedSpec) = 0);
		{$ELSE}
		if Device = '' then
			AudioDevice := SDL_OpenAudioDevice(nil, 0, @desiredSpec, @obtainedSpec, 0)
		else
			AudioDevice := SDL_OpenAudioDevice(PAnsiChar(Device), 0, @desiredSpec, @obtainedSpec, 0);
		Result := (AudioDevice >= 2);
		{$IFDEF DEBUG} Log('AudioDevice=',AudioDevice); {$ENDIF}
		{$ENDIF}
	except
		{$IFDEF DEBUG} Log('Init failure!'); {$ENDIF}
		Result := False;
	end;
	Initialized := Result;
end;
{$ENDIF}
{$IFDEF BASS}
var
	bassdevice: BASS_DEVICEINFO;

	function GetAudioDevice(Devname: AnsiString): Integer;
	var
		i: Integer;
	begin
		Result := -1;
		if (DevName = '') or (DevName = 'Default') then Exit;
		for i := 1 to 99 do
			if (BASS_GetDeviceInfo(i, bassdevice)) and (bassdevice.name = Devname) then
				Exit(i);
	end;

var
	info: BASS_INFO;
	flags: DWord;
	dev, Minbuf: Integer;
	WindowHandle: Cardinal = 0;
begin
	Result := False;
	if Initialized then Exit;
{ !!!
	if Device <> '-' then
		Options.Audio.Device := Device;
}
	{$IFDEF BASS_DYNAMIC}
	// load the BASS library dynamically at runtime
		{$IFDEF WINDOWS}
		if not Load_BASSDLL('bass.dll') then
		{$ENDIF}
		{$IFDEF LINUX}
	    if not Load_BASSDLL(ExtractFilePath(ParamStr(0)) + 'libbass.so') then
		{$ENDIF}
		{$IFDEF DARWIN}
	    if not Load_BASSDLL(ExtractFilePath(ParamStr(0)) + 'libbass.dylib') then
		{$ENDIF}
	    begin
			Log('Could not init BASS library!');
			HALT;
		end;
	{$ENDIF}

	flags := {BASS_DEVICE_STEREO or }
		BASS_DEVICE_MONO or BASS_DEVICE_LATENCY or BASS_DEVICE_DMIX;
	BASS_SetConfig(BASS_CONFIG_DEV_DEFAULT, 1);
	{$IFDEF WINDOWS}
	BASS_SetConfig(BASS_CONFIG_VISTA_TRUEPOS, 0); // Speeds up initialization
	{$ENDIF}

	dev := GetAudioDevice({Options.Audio.Device}Device);
	if BASS_Init(dev, 44100, flags, WindowHandle, nil) then
//		Log('Initialized audio device ' + IntToStr(dev) + ' (' + Options.Audio.Device + ')')
	else
	begin
		if not BASS_Init(-1, {Frequency,}44100, flags, WindowHandle, nil) then
		begin
			Log('Error initializing audio device!');
			Exit(False);
		end;
	end;

	BASS_GetDeviceInfo(BASS_GetDevice, bassdevice);
	BASS_GetInfo(info);

	// Use the recommended minimum buffer length with 1ms margin:
	// Get update period, add the 'minbuf' plus 1ms margin
	BASS_SetConfig(BASS_CONFIG_UPDATEPERIOD, 10);

	// Minimum recommended buffer size
	Minbuf := Max(info.minbuf + 10 + 1, MINIMUM_AUDIOBUFFER_LENGTH);

	if BufferSize = 0 then //Options.Audio.Buffer = 0 then
		// Default buffer size = 'minbuf' + update period + 1ms extra margin
		BASS_SetConfig(BASS_CONFIG_BUFFER, Minbuf)
	else
		// User set buffer
		BASS_SetConfig(BASS_CONFIG_BUFFER, BufferSize);//Options.Audio.Buffer);

	Result := True;
	Initialized := Result;
end;
{$ENDIF}

procedure TAudioHandler.Play;
begin
	if not Initialized then Init;

	Playing := True;
	{$IFDEF SDL_DEPRECATED}
	SDL_PauseAudio(0);
	{$ELSE}
		{$IFNDEF BASS}
		SDL_PauseAudioDevice(AudioDevice, 0);
		{$ENDIF}
	{$ENDIF}

	{$IFDEF DEBUG}
	Log('Audio.Play');
	{$ENDIF}
end;

procedure TAudioHandler.Stop;
begin
	if not Initialized then Exit;

	Playing := False;
	{$IFDEF SDL_DEPRECATED}
	SDL_PauseAudio(1);
	{$ELSE}
		{$IFNDEF BASS}
//		SDL_PauseAudioDevice(AudioDevice, 1);
		{$ENDIF}
	{$ENDIF}

	{$IFDEF DEBUG}
	Log('Audio.Stop');
	{$ENDIF}
end;

{$IFDEF RENDERWAV}
function TAudioHandler.RenderToWAV(const Filename: String): Boolean;
begin
	StopRendering;
	Wav := TWavWriter.Create;

	Wav.fmt.Format := AUDIO_FORMAT_PCM;
	Wav.fmt.BitsPerSample := 16;
	Wav.fmt.Channels := 2;
	Wav.fmt.SampleRate := 44100;
	Wav.fmt.ByteRate := 44100 * 4;
	Wav.fmt.BlockAlign := 2;

	Result := Wav.StoreToFile(Filename);
	if not Result then
	begin
		//Debug('Error creating WAV file!', []);
		Wav.Free;
	end;
end;

procedure TAudioHandler.StopRendering;
begin
	if Wav <> nil then Wav.Free;
end;
{$ENDIF}

constructor TAudioHandler.Create;
begin
	inherited;

	Initialized := False;

	RegisteredPlayers := TFPGObjectList<TPlayRoutine>.Create(False);

	Player := nil;
	Playing := False;
end;

destructor TAudioHandler.Destroy;
begin
	Stop;

	RegisteredPlayers.Free;

	{$IFDEF BASS}
	BASS_Free;
		{$IFDEF BASS_DYNAMIC}
		Unload_BASSDLL;
		{$ENDIF}
	{$ELSE}
	SDL_CloseAudioDevice(AudioDevice);
	{$ENDIF}

	inherited;
end;

initialization

	Audio := TAudioHandler.Create;

finalization

	Audio.Free;

end.

