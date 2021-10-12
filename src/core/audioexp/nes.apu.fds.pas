unit NES.APU.FDS;

{$mode delphi}

interface

uses
	Classes, SysUtils,
	NES.SaveState,
	NES.APU.ExpansionAudio;

type
	TBaseFDSChannel = class(TSnapshotable)
	protected
		speed,
		gain: Byte;
		envelopeOff,
		volumeIncrease: Boolean;
		frequency: Word;
		timer: Cardinal;
		masterSpeed: Byte;
	public
		procedure SetMasterEnvelopeSpeed(NewMasterSpeed: Byte); inline;
		procedure WriteReg(addr: Word; value: Byte); virtual;
		function  TickEnvelope: Boolean;
		function  GetGain: Byte; inline;
		function  GetFrequency: Word; inline;
		procedure ResetTimer; inline;

		constructor Create(const RegisterName: AnsiString); override;
	end;

	// ==========================================================================

	TModChannel = class(TBaseFDSChannel)
	const
		ModReset = $FF;
		modLut: array[0..7] of Integer = ( 0, 1, 2, 4, ModReset, -4, -2, -1 );
	private
		counter: Int8;
		modulationDisabled: Boolean;

		modTable: array[0..63] of Byte;
		modTablePosition: Byte;
		overflowCounter: Word;
		output: Integer;
	public
		procedure WriteReg(addr: Word; value: Byte); override;
		procedure WriteModTable(value: Byte);
		procedure UpdateCounter(value: Int8);
		function  IsEnabled: Boolean; inline;
		function  TickModulator: Boolean;
		procedure UpdateOutput(volumePitch: Word);
		function  GetOutput: Integer; inline;

		constructor Create(const RegisterName: AnsiString); override;
	end;

	// ==========================================================================

	TFDSAudio = class(TBaseExpansionAudio)
	const
		WaveVolumeTable: array[0..3] of Byte = ( 36, 24, 17, 14 );
	private
		// Register values
		waveTable: array[0..63] of Byte;
		waveWriteEnabled: Boolean;

		volume: TBaseFDSChannel;
		modChannel: TModChannel;

		disableEnvelopes,
		haltWaveform: Boolean;

		masterVolume: Byte;

		// Internal values
		waveOverflowCounter: Word;
		wavePitch: Integer;
		wavePosition: Byte;

		lastOutput: Byte;
	protected
		procedure ClockAudio; override;
		procedure UpdateOutput;
	public
		function  ReadRegister(addr: Word): Byte;
		procedure WriteRegister(addr: Word; value: Byte);

		procedure LoadSnapshot; override;
		procedure SaveSnapshot; override;

		constructor Create(const RegisterName: AnsiString); override;
		destructor  Destroy; override;
	end;


implementation

uses
	Math,
	NES.Types, NES.Console;


// ==========================================================================
// TBaseFDSChannel
// ==========================================================================

constructor TBaseFDSChannel.Create(const RegisterName: AnsiString);
begin
	inherited Create(RegisterName);

	RegisterProperty(08, @speed);
	RegisterProperty(08, @gain);
	RegisterProperty(01, @envelopeOff);
	RegisterProperty(01, @volumeIncrease);
	RegisterProperty(16, @frequency);
	RegisterProperty(32, @timer);
	RegisterProperty(08, @masterSpeed);

	masterSpeed := $FF;
end;

function TBaseFDSChannel.GetGain: Byte;
begin
	Result := gain;
end;

function TBaseFDSChannel.GetFrequency: Word;
begin
	Result := frequency;
end;

procedure TBaseFDSChannel.SetMasterEnvelopeSpeed(NewMasterSpeed: Byte);
begin
	masterSpeed := NewMasterSpeed;
end;

procedure TBaseFDSChannel.ResetTimer;
begin
	timer := 8 * (speed + 1) * (masterSpeed {+ 1});
end;

procedure TBaseFDSChannel.WriteReg(addr: Word; value: Byte);
begin
	case (addr and $03) of
		0: begin
			speed := value and $3F;
			volumeIncrease := (value and $40) = $40;
			envelopeOff    := (value and $80) = $80;
			ResetTimer;
			if envelopeOff then
				gain := speed; // Envelope is off, gain = speed
		end;
		2:	frequency := (frequency and $0F00) or value;
		3:	frequency := (frequency and $00FF) or ((value and $0F) shl 8);
	end;
end;

function TBaseFDSChannel.TickEnvelope: Boolean;
begin
	Result := False;

	if (not envelopeOff) and (masterSpeed > 0) then
	begin
		{$R-}Dec(timer);
		if timer = 0 then
		begin
			ResetTimer;

			if (volumeIncrease) and (gain < 32) then
				Inc(gain)
			else
			if (not volumeIncrease) and (gain > 0) then
				Dec(gain);

			Result := True;
		end;
	end;
end;


// ==========================================================================
// TModChannel
// ==========================================================================

constructor TModChannel.Create(const RegisterName: AnsiString);
begin
	inherited Create(RegisterName);

	RegisterProperty(08, @counter);
	RegisterProperty(01, @modulationDisabled);
	RegisterProperty(08, @modTablePosition);
	RegisterProperty(16, @overflowCounter);
	RegisterProperty(32, @output);

	RegisterArray(Length(modTable), @modTable[0]);
end;

procedure TModChannel.WriteReg(addr: Word; value: Byte);
begin
	case addr of

		$4084, $4086:
			inherited WriteReg(addr, value);

		$4085:
			UpdateCounter(value and $7F);

		$4087:
		begin
			inherited WriteReg(addr, value);

			modulationDisabled := (value and $80) = $80;
			if modulationDisabled then
				overflowCounter := 0;
		end;
	end;
end;

procedure TModChannel.WriteModTable(value: Byte);
begin
	// "This register has no effect unless the mod unit is disabled via the high bit of $4087."
	if modulationDisabled then
	begin
		modTable[modTablePosition     and $3F] := value and $07;
		modTable[(modTablePosition+1) and $3F] := value and $07;
		modTablePosition := (modTablePosition + 2) and $3F;
	end;
end;

procedure TModChannel.UpdateCounter(value: Int8);
var
	ctr: Integer;
begin
	ctr := value;
	if ctr >= 64 then
		Dec(ctr, 128)
	else
	if ctr < -64 then
		Inc(ctr, 128);
	counter := ctr;
end;

function TModChannel.IsEnabled: Boolean;
begin
	Result := (not modulationDisabled) and (frequency > 0);
end;

function TModChannel.TickModulator: Boolean;
var
	offset: Integer;
begin
	Result := False;
	if IsEnabled then
	begin
		{$R-}Inc(overflowCounter, frequency);
		if overflowCounter < frequency then // Overflowed, tick the modulator
		begin
			offset := modLut[modTable[modTablePosition]];
			UpdateCounter(IfThen(offset = ModReset, 0, counter + offset));
			modTablePosition := (modTablePosition + 1) and $3F;
			Result := True;
		end;
	end;
end;

// Code from NesDev Wiki
procedure TModChannel.UpdateOutput(volumePitch: Word);
var
	temp, remainder: Int32;
begin
	// pitch   = $4082/4083 (12-bit unsigned pitch value)
	// counter = $4085 (7-bit signed mod counter)
	// gain    = $4084 (6-bit unsigned mod gain)

	// 1. multiply counter by gain, lose lowest 4 bits of result but "round" in a strange way
	temp := counter * gain;
	remainder := temp and $F;
	temp := SarLongInt(temp, 4); // arithmetic shifts!
	if (remainder > 0) and ((temp and $80) = 0) then
		Inc(temp, IfThen(counter < 0, -1, +2));

	// 2. wrap if a certain range is exceeded
	if temp >= 192 then
		Dec(temp, 256)
	else
	if temp < -64 then
		Inc(temp, 256);

	// 3. multiply result by pitch, then round to nearest while dropping 6 bits
	{$Q-}temp := volumePitch * temp;
	remainder := temp and $3F;
	temp := SarLongInt(temp, 6); //temp shr 6;
	if remainder >= 32 then
		Inc(temp);

	// final mod result is in temp
	output := temp;
end;

function TModChannel.GetOutput: Integer;
begin
	Result := IfThen(IsEnabled, output, 0);
end;


// ==========================================================================
// TFDSAudio
// ==========================================================================

constructor TFDSAudio.Create(const RegisterName: AnsiString);
begin
	inherited Create(RegisterName);

	RegisterProperty(01, @waveWriteEnabled);
	RegisterProperty(01, @disableEnvelopes);
	RegisterProperty(01, @haltWaveform);
	RegisterProperty(08, @masterVolume);
	RegisterProperty(32, @waveOverflowCounter);
	RegisterProperty(32, @wavePitch);
	RegisterProperty(08, @wavePosition);
	RegisterProperty(08, @lastOutput);

	RegisterArray(Length(waveTable), @waveTable[0]);

	volume := TBaseFDSChannel.Create('FDSVOL');
	modChannel := TModChannel.Create('FDSMOD');
end;

destructor TFDSAudio.Destroy;
begin
	volume.Free;
	modChannel.Free;

	inherited Destroy;
end;

procedure TFDSAudio.ClockAudio;
var
	frequency: Integer;
begin
	frequency := volume.GetFrequency;
	if (not haltWaveform) and (not disableEnvelopes) then
	begin
		volume.TickEnvelope;
		if modChannel.TickEnvelope then
			modChannel.UpdateOutput(frequency);
	end;

	// Modulator was ticked, update wave pitch
	if modChannel.TickModulator then
		modChannel.UpdateOutput(frequency);

	if haltWaveform then
	begin
		wavePosition := 0;
		UpdateOutput;
	end
	else
	begin
		UpdateOutput;

		if (frequency + modChannel.GetOutput > 0) and (not waveWriteEnabled) then
		begin
			{$R-}Inc(waveOverflowCounter, frequency + modChannel.GetOutput);
			if waveOverflowCounter < (frequency + modChannel.GetOutput) then
				wavePosition := (wavePosition + 1) and $3F;
		end;
	end;
end;

procedure TFDSAudio.UpdateOutput;
var
	level: Cardinal;
	outputLevel: Byte;
begin
	level := Min(volume.GetGain, 32) * WaveVolumeTable[masterVolume];
	outputLevel := Trunc((waveTable[wavePosition] * level) / 1152);

	if lastOutput <> outputLevel then
	begin
		NES_APU.AddExpansionAudioDelta(channel_FDS, outputLevel - lastOutput);
		lastOutput := outputLevel;
	end;
end;

function TFDSAudio.ReadRegister(addr: Word): Byte;
var
	value: Byte;
begin
	value := Console.MemoryManager.GetOpenBus;
	if addr <= $407F then
	begin
		value := value and $C0;
		value := value or waveTable[addr and $3F];
	end
	else
	if addr = $4090 then
	begin
		value := value and $C0;
		value := value or volume.GetGain;
	end
	else
	if addr = $4092 then
	begin
		value := value and $C0;
		value := value or modChannel.GetGain;
	end;
	Result := value;
end;

procedure TFDSAudio.WriteRegister(addr: Word; value: Byte);
begin
	if addr <= $407F then
	begin
		if waveWriteEnabled then
			waveTable[addr and $3F] := value and $3F;
	end
	else
	case addr of

		$4080, $4082:
			volume.WriteReg(addr, value);

		$4083:
		begin
			disableEnvelopes := (value and $40) = $40;
			haltWaveform     := (value and $80) = $80;
			if disableEnvelopes then
			begin
				volume.ResetTimer;
				modChannel.ResetTimer;
			end;
			volume.WriteReg(addr, value);
		end;

		$4084..$4087:
			modChannel.WriteReg(addr, value);

		$4088:
			modChannel.WriteModTable(value);

		$4089:
		begin
			masterVolume := value and $03;
			waveWriteEnabled := (value and $80) = $80;
		end;

		$408A:
		begin
			volume.SetMasterEnvelopeSpeed(value);
			modChannel.SetMasterEnvelopeSpeed(value);
		end;

	end;
end;

procedure TFDSAudio.LoadSnapshot;
begin
	inherited LoadSnapshot;

	volume.LoadSnapshot;
	modChannel.LoadSnapshot;
end;

procedure TFDSAudio.SaveSnapshot;
begin
	inherited SaveSnapshot;

	volume.SaveSnapshot;
	modChannel.SaveSnapshot;
end;


end.

