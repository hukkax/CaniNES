unit NES.APU.Mixer;

{$mode delphi}

interface

uses
	Classes, SysUtils, Generics.Collections,
	NES.Types, NES.SaveState, NES.APU,
	Blip;

const
	Mixer_CycleLength   = 10000;
	BitsPerSample: Byte = 16;
	MaxSampleRate = 96000;

type
	TSoundMixer = class(TSnapshotable)
	private
		APU: TAPU;
		NesModel: TNesModel;

		MaxSamplesPerFrame: Cardinal;
		masterVolume: Single;
		fadeRatio: Single;
		muteFrameCount: Cardinal;

		previousOutputLeft, previousOutputRight: Int16;

		underTarget: Int32;

		blipBufLeft, blipBufRight: TBlip;

		channelOutput: array[0..MaxChannelCount] of array [0..Mixer_CycleLength] of Int16;
		currentOutput: array[0..MaxChannelCount] of Int16;

		outputBuffer: array of Int16;

		volumes, panning: array[0..MaxChannelCount] of Single;

		//NesModel model;
		sampleRate, clockRate: Cardinal;

		hasPanning: Boolean;
		LoadingSaveState: Boolean;

		previousTargetRate: Single;

		timestamps: TList<Cardinal>;

		function  GetMuteFrameCount: Cardinal;
		function  GetRateAdjustment: Double;
		function  GetTargetRateAdjustment: Double;

		procedure ResetMuteFrameCount;
		procedure SetFadeRatio(NewFadeRatio: Single);
		procedure UpdateRates(ForceUpdate: Boolean);
		procedure UpdateTargetSampleRate;

	public
		rateAdjustment: Double;

		constructor Create(APUInstance: TAPU); overload;
		destructor  Destroy; override;

		procedure LoadSnapshot; override;

		procedure SettingsChanged;
		procedure SetNesModel(model: TNESModel);
		procedure Reset;
		procedure PlayAudioBuffer(time: Cardinal);

		function  GetChannelOutput(channel: Byte; forRightChannel: Boolean): Single; inline;
		function  GetOutputVolume(forRightChannel: Boolean): Int16;

		procedure AddDelta(channel: Byte; time: Cardinal; delta: ShortInt);
		procedure EndFrame(time: Cardinal);
	end;


var
	Mixer: TSoundMixer;


implementation

uses
	Math, NES.Config,
	NES.AudioManager, NES.Console;


constructor TSoundMixer.Create(APUInstance: TAPU);
begin
	inherited Create('SMIX');

	APU := APUInstance;
	Mixer := Self;

	RegisterProperty(32, @clockRate);
	RegisterProperty(32, @sampleRate);
	RegisterProperty(8,  @NesModel);

	RegisterProperty(16, @previousOutputLeft);
	RegisterProperty(16, @previousOutputRight);

	RegisterArray(Length(currentOutput)*2, @currentOutput[0]);


	// * fadeRatio;// {settings->GetMasterVolume()} 0.8 * fadeRatio;
//	SettingsChanged;

	sampleRate := Configuration.Audio.SampleRate;

	timestamps := TList<Cardinal>.Create;

	// x4 to allow CPU overclocking up to 10x, x2 for panning stereo
	MaxSamplesPerFrame := Round(MaxSampleRate / 60 * 4 * 2);
	SetLength(outputBuffer, MaxSamplesPerFrame*2);
	blipBufLeft  := TBlip.Create(MaxSamplesPerFrame);
	blipBufRight := TBlip.Create(MaxSamplesPerFrame);

	LoadingSaveState := False;
	NesModel := nesNTSC;
	Reset;
end;

destructor TSoundMixer.Destroy;
begin
	blipBufLeft.Free;
	blipBufRight.Free;
	timestamps.Free;
	Mixer := nil;

	inherited Destroy;
end;

procedure TSoundMixer.SettingsChanged;
begin
	UpdateRates(True);
end;

procedure TSoundMixer.SetNesModel(model: TNESModel);
begin
	if NesModel <> model then
	begin
		NesModel := model;
		UpdateRates(True);
	end;
end;

procedure TSoundMixer.Reset;
var
	i: Int32;
begin
	fadeRatio := 1.0;
	muteFrameCount := 0;
	underTarget := 0;

	if not LoadingSaveState then
	begin
		previousOutputLeft  := 0;
		previousOutputRight := 0;
		FillByte(currentOutput[0], Length(currentOutput)*2, 0);
	end;

	blipBufLeft.Clear;
	blipBufRight.Clear;
	timestamps.Clear;

	for i := 0 to MaxChannelCount do
	begin
		volumes[i] := 1;
		panning[i] := 1;
		FillByte(channelOutput[i], Length(channelOutput[i])*2, 0);
	end;

	UpdateRates(True);

	previousTargetRate := sampleRate;
end;

procedure TSoundMixer.SetFadeRatio(NewFadeRatio: Single);
begin
	fadeRatio := NewFadeRatio;
end;

function TSoundMixer.GetMuteFrameCount: Cardinal;
begin
	Result := muteFrameCount;
end;

procedure TSoundMixer.ResetMuteFrameCount;
begin
	muteFrameCount := 0;
end;

procedure TSoundMixer.PlayAudioBuffer(time: Cardinal);
var
	i, sampleCount: Cardinal;
begin
	UpdateTargetSampleRate;
	EndFrame(time);

	sampleCount := blipBufLeft.ReadSamples(@outputBuffer[0], MaxSamplesPerFrame, True);
	if sampleCount < 1 then Exit;

	if hasPanning then
		blipBufRight.ReadSamples(@outputBuffer[1], MaxSamplesPerFrame, True)
	else
		// Copy left channel to right channel (optimization - when no panning is used)
		for i := 0 to sampleCount-1 do
			outputBuffer[i*2 + 1] := outputBuffer[i*2];

	if not Console.IsRunAheadFrame then
		APU.PlayBuffer(@outputBuffer[0], sampleCount);
end;

procedure TSoundMixer.UpdateRates(ForceUpdate: Boolean);
var
	newRate: Cardinal;
	targetRate: Double;
	havePanning: Boolean;
	i: Int32;
begin
	newRate := NES_CPU.GetClockRate(NesModel);

	masterVolume := Configuration.Audio.MixingVolume;

	// Adjust sample rate when running at 60.0 fps instead of 60.1
	if Configuration.Emulator.IntegerFpsMode then
	begin
		if NesModel = nesPAL then
			newRate := Trunc(newRate * 50.0 / 50.00697796826829)
		else
			newRate := Trunc(newRate * 60.0 / 60.0988118623484);
	end;

	targetRate := sampleRate * GetTargetRateAdjustment;

	if (ForceUpdate) or (clockRate <> newRate) then
	begin
		clockRate := newRate;
		blipBufLeft.SetRates (clockRate, targetRate);
		blipBufRight.SetRates(clockRate, targetRate);
	end;

	{if ForceUpdate then
	begin
		if NesModel = nesPAL then
			newRate := Trunc(MaxSampleRate / 50 * 4 * 2)
		else
			newRate := Trunc(MaxSampleRate / 60 * 4 * 2);

		if newRate <> MaxSamplesPerFrame then
		begin
			MaxSamplesPerFrame := newRate;

			SetLength(outputBuffer, MaxSamplesPerFrame*2);

			blipBufLeft.Free;
			blipBufRight.Free;
			blipBufLeft  := TBlip.Create(MaxSamplesPerFrame);
			blipBufRight := TBlip.Create(MaxSamplesPerFrame);
		end;
	end;}

	havePanning := False;
	for i := 0 to MaxChannelCount do
	begin
		volumes[i] := Configuration.Emulator.APU.Channels[i].Volume;
		panning[i] := Configuration.Emulator.APU.Channels[i].Panning + 1.0;
		APUChannelUsed[i] := volumes[i] >= 0.1;
		if panning[i] <> 1.0 then
		begin
			if not hasPanning then
			begin
				blipBufLeft.Clear;
				blipBufRight.Clear;
			end;
			havePanning := True;
		end;
	end;
	hasPanning := havePanning;
end;

function TSoundMixer.GetChannelOutput(channel: Byte; forRightChannel: Boolean): Single;
begin
	if forRightChannel then
		Result := currentOutput[channel] * volumes[channel] * panning[channel]
	else
		Result := currentOutput[channel] * volumes[channel] * (2.0 - panning[channel]);
end;

function TSoundMixer.GetOutputVolume(forRightChannel: Boolean): Int16;
var
	squareOutput, tndOutput: Single;
	squareVolume, tndVolume: Word;
	i: Integer;
begin
	squareOutput :=
		GetChannelOutput(channel_Square1, forRightChannel) +
		GetChannelOutput(channel_Square2, forRightChannel);
	tndOutput :=
		3 * GetChannelOutput(channel_Triangle, forRightChannel) +
		2 * GetChannelOutput(channel_Noise,    forRightChannel) +
		    GetChannelOutput(channel_DMC,      forRightChannel);

	if squareOutput = 0.0 then
		squareVolume := 0
	else
		squareVolume := {$R-}(Trunc(477600 / (8128.0 / squareOutput + 100)));

	if tndOutput = 0.0 then
		tndVolume := 0
	else
		tndVolume := {$R-}(Trunc((818350 / (24329.0 / tndOutput + 100))));

	Result := Int16(squareVolume + tndVolume);

	// mix in any extra channels that are being used
	for i := channel_FDS to channel_Last do
		if APUChannelUsed[i] then
			Inc(Result, Int16(Trunc(
				GetChannelOutput(i, forRightChannel) * APUChannelMixMultiplier[i]
			)));
end;

function TSoundMixer.GetRateAdjustment: Double;
begin
	Result := rateAdjustment;
end;

function TSoundMixer.GetTargetRateAdjustment: Double;
var
	spd: Integer;
	latencyGap, adjustment, subAdjustment: Double;
begin
	spd := Console.GetEmulationSpeed;

	if Configuration.Audio.DynamicRate then
	begin
		if (APU.AverageLatency > 0) and (spd = 100) then
		begin
			// Try to stay within +/- 3ms of requested latency

			latencyGap := APU.AverageLatency - APU.AudioLatency;
			adjustment := Min(0.0025, (Ceil((Abs(latencyGap) - ARA_maxGap) * 8)) * 0.00003125);

			if (latencyGap < 0) and (underTarget < ARA_maxSubAdjustment) then
				Inc(underTarget)
			else
			if (latencyGap > 0) and (underTarget > -ARA_maxSubAdjustment) then
				Dec(underTarget);

			// For every ~1 second spent under/over target latency, further adjust rate
			// (GetTargetRate is called approx. 3x per frame)
			// This should slowly get us closer to the actual output rate of the sound card
			subAdjustment := 0.00003125 * underTarget / 180;

			if adjustment > 0 then
			begin
				if latencyGap > ARA_maxGap then
					rateAdjustment := 1 - adjustment + subAdjustment
				else
				if(latencyGap < -ARA_maxGap) then
					rateAdjustment := 1 + adjustment + subAdjustment;
			end
			else
			if Abs(latencyGap) < 1 then
			begin
				//Restore normal rate once we get within +/- 1ms
				rateAdjustment := 1 + subAdjustment;
			end;
		end
		else
		begin
			underTarget := 0;
			rateAdjustment := 1; //100 / spd;
		end;
	end
	else
	begin
		underTarget := 0;
		rateAdjustment := 1; //100 / spd;
	end;

	Result := rateAdjustment;
end;

procedure TSoundMixer.UpdateTargetSampleRate;
var
	targetRate: Single;
begin
	targetRate := sampleRate * GetTargetRateAdjustment;
	if targetRate <> previousTargetRate then
	begin
		blipBufLeft.SetRates (clockRate, targetRate);
		blipBufRight.SetRates(clockRate, targetRate);
		previousTargetRate := targetRate;
	end;
end;

procedure TSoundMixer.AddDelta(channel: Byte; time: Cardinal; delta: ShortInt);
begin
	if (delta <> 0) and (APUChannelUsed[channel]) then
	begin
		timestamps.Add(time);
		{$R-}Inc(channelOutput[channel, time], delta);
	end;
end;

procedure TSoundMixer.EndFrame(time: Cardinal);
var
	muteFrame: Boolean;
	prevstamp, stamp: Cardinal;
	currentOut: Int16;
	i, j: Int32;
begin
	muteFrame := True;

	if timestamps.Count > 0 then
	begin
		timestamps.Sort;
		prevstamp := 0;

		for i := 0 to timestamps.Count-1 do
		begin
			stamp := timestamps[i];
			if (stamp = prevstamp) and (prevstamp <> 0) then Continue;

			for j := 0 to MaxChannelCount do
			begin
				if not APUChannelUsed[j] then Continue;
				// Assume any change in output means sound is playing, disregarding volume options
				if channelOutput[j, stamp] <> 0 then
					muteFrame := False;
				Inc(currentOutput[j], channelOutput[j, stamp]);
			end;

			currentOut := GetOutputVolume(False);

			blipBufLeft.AddDeltaFast(stamp, Trunc((currentOut - previousOutputLeft) * masterVolume));
			previousOutputLeft := currentOut;

			if hasPanning then
			begin
				currentOut := GetOutputVolume(True);
				blipBufRight.AddDeltaFast(stamp, Trunc((currentOut - previousOutputRight) * masterVolume));
				previousOutputRight := currentOut;
			end;

			prevstamp := stamp;
		end;
	end;

	blipBufLeft.EndFrame(time);
	if hasPanning then
		blipBufRight.EndFrame(time);

	if muteFrame then
		Inc(muteFrameCount)
	else
		muteFrameCount := 0;

	// Reset everything
	timestamps.Clear;

	for i := 0 to MaxChannelCount do
		if APUChannelUsed[i] then
			FillByte(channelOutput[i], Length(channelOutput[i]) * 2, 0);
end;

procedure TSoundMixer.LoadSnapshot;
begin
	inherited LoadSnapshot;

	LoadingSaveState := True;
	Reset;
	LoadingSaveState := False;
end;

end.

