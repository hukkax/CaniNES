unit NES.APU.Namco163;

{$WARN 4035 off : Mixing signed expressions and longwords gives a 64bit result}

interface

uses
	NES.Types, NES.SaveState,
	NES.APU, NES.APU.ExpansionAudio;

type
	TNamco163Audio = class(TBaseExpansionAudio)
	const
		AudioRamSize = $80;

		SoundReg_FrequencyLow = 0;
		SoundReg_PhaseLow = 1;
		SoundReg_FrequencyMid = 2;
		SoundReg_PhaseMid = 3;
		SoundReg_FrequencyHigh = 4;
		SoundReg_WaveLength = 4;
		SoundReg_PhaseHigh = 5;
		SoundReg_WaveAddress = 6;
		SoundReg_Volume = 7;
	private
		internalRam: array[0..AudioRamSize-1] of Byte;
		channelOutput: array[0..7] of Int16;

		ramPosition: Byte;
		autoIncrement: Boolean;
		updateCounter: Byte;
		currentChannel: Int8;
		lastOutput: Int16;
		disableSound: Boolean;

		function  GetBaseAddr(channel: Integer): Byte; inline;
		function  GetFrequency(channel: Integer): Cardinal;
		function  GetPhase(channel: Integer): Cardinal;
		procedure SetPhase(channel: Integer; phase: Cardinal);
		function  GetWaveAddress(channel: Integer): Byte;
		function  GetWaveLength(channel: Integer): Byte;
		function  GetVolume(channel: Integer): Byte;
		function  GetNumberOfChannels: Byte;
		procedure UpdateChannel(channel: Integer);
		procedure UpdateOutputLevel;
	protected
		procedure ClockAudio; override;
	public
		function GetInternalRam: PByte;

		function  ReadRegister(addr: Word): Byte;
		procedure WriteRegister(addr: Word; value: Byte);

		constructor Create; overload;
	end;


implementation

uses
	SysUtils, Basement.Util,
	NES.Config, NES.Console, NES.CPU;

// ============================================================================
// TNamco163Audio
// ============================================================================

constructor TNamco163Audio.Create;
begin
	inherited Create('163AUD');

	RegisterProperty(8,  @ramPosition);
	RegisterProperty(1,  @autoIncrement);
	RegisterProperty(8,  @updateCounter);
	RegisterProperty(8,  @currentChannel);
	RegisterProperty(16, @lastOutput);
	RegisterProperty(1,  @disableSound);

	RegisterArray(Length(internalRam),     @internalRam[0]);
	RegisterArray(Length(channelOutput)*2, @channelOutput[0]);

	currentChannel := 7;
end;

function TNamco163Audio.GetBaseAddr(channel: Integer): Byte; inline;
begin
	Result := $40 + channel * $08;
end;

function TNamco163Audio.GetFrequency(channel: Integer): Cardinal;
var
	baseAddr: Byte;
begin
	baseAddr := GetBaseAddr(channel);
	Result := (
		(internalRam[baseAddr + SoundReg_FrequencyHigh] and $03) shl 16) or
		(internalRam[baseAddr + SoundReg_FrequencyMid]  shl 8) or
		(internalRam[baseAddr + SoundReg_FrequencyLow]);
end;

function TNamco163Audio.GetPhase(channel: Integer): Cardinal;
var
	baseAddr: Byte;
begin
	baseAddr := GetBaseAddr(channel);
	Result :=
		(internalRam[baseAddr + SoundReg_PhaseHigh] shl 16) or
		(internalRam[baseAddr + SoundReg_PhaseMid]  shl 8)  or
		(internalRam[baseAddr + SoundReg_PhaseLow]);
end;

procedure TNamco163Audio.SetPhase(channel: Integer; phase: Cardinal);
var
	baseAddr: Byte;
begin
	baseAddr := GetBaseAddr(channel);
	internalRam[baseAddr + SoundReg_PhaseHigh] := (phase shr 16) and $FF;
	internalRam[baseAddr + SoundReg_PhaseMid]  := (phase shr 8) and $FF;
	internalRam[baseAddr + SoundReg_PhaseLow]  :=  phase and $FF;
end;

function TNamco163Audio.GetWaveAddress(channel: Integer): Byte;
begin
	Result := internalRam[GetBaseAddr(channel) + SoundReg_WaveAddress];
end;

function TNamco163Audio.GetWaveLength(channel: Integer): Byte;
begin
	{$R-}Result := 256 - (internalRam[GetBaseAddr(channel) + SoundReg_WaveLength] and $FC);
end;

function TNamco163Audio.GetVolume(channel: Integer): Byte;
begin
	Result := internalRam[GetBaseAddr(channel) + SoundReg_Volume] and $0F;
end;

function TNamco163Audio.GetNumberOfChannels: Byte;
begin
	Result := (internalRam[$7F] shr 4) and $07;
end;

procedure TNamco163Audio.UpdateChannel(channel: Integer);
var
	phase, freq: Cardinal;
	length, offset, volume: Byte;
	sample: Int8;
	samplePosition: Byte;
begin
	phase  := GetPhase(channel);
	freq   := GetFrequency(channel);
	length := GetWaveLength(channel);
	offset := GetWaveAddress(channel);
	volume := GetVolume(channel);

	if length = 0 then
		phase := 0
	else
		phase := (phase + freq) mod (length shl 16);
	samplePosition := ((phase  shr  16) + offset) and $FF;

	if Odd(samplePosition) then
		sample := internalRam[samplePosition div 2] shr 4
	else
		sample := internalRam[samplePosition div 2] and $0F;

	channelOutput[channel] := (sample - 8) * volume;
	UpdateOutputLevel;
	SetPhase(channel, phase);
end;

procedure TNamco163Audio.UpdateOutputLevel;
var
	summedOutput: Int16 = 0;
	i: Integer;
begin
	for i := 7 downto (7 - GetNumberOfChannels) do
		Inc(summedOutput, channelOutput[i]);

	summedOutput := summedOutput div (GetNumberOfChannels + 1);
	NES_APU.AddExpansionAudioDelta(channel_Namco163, summedOutput - lastOutput);
	lastOutput := summedOutput;
end;

procedure TNamco163Audio.ClockAudio;
begin
	if disableSound then Exit;

	Inc(updateCounter);
	if updateCounter >= 15 then
	begin
		UpdateChannel(currentChannel);

		updateCounter := 0;
		Dec(currentChannel);
		if(currentChannel < 7 - GetNumberOfChannels) then
			currentChannel := 7;
	end;
end;

function TNamco163Audio.GetInternalRam: PByte;
begin
	Result := @internalRam[0];
end;

function TNamco163Audio.ReadRegister(addr: Word): Byte;
begin
	if (addr and $F800) = $4800 then
	begin
		Result := internalRam[ramPosition];
		if autoIncrement then
			ramPosition := (ramPosition + 1) and $7F;
	end
	else
		Result := 0;
end;

procedure TNamco163Audio.WriteRegister(addr: Word; value: Byte);
begin
	case (addr and $F800) of

		$4800:
		begin
			internalRam[ramPosition] := value;
			if autoIncrement then
				ramPosition := (ramPosition + 1) and $7F;
		end;

		$E000:
			disableSound := (value and $40) = $40;

		$F800:
		begin
			ramPosition := value and $7F;
			autoIncrement := (value and $80) = $80;
		end;

	end;
end;


end.

