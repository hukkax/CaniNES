unit NES.APU.VRC6;

interface

uses
	Classes, SysUtils,
	NES.SaveState,
	NES.APU, NES.APU.ExpansionAudio;

type
	TVrc6ChannelType = ( chPulse1 = 0, chPulse2, chSaw );

	TVrc6Channel = class(TSnapshotable)
	protected
		Frequency:  Word;
		Enabled:    Boolean;

		timer: Int32;
		step,
		frequencyShift: Byte;
	public
		IsSaw: Boolean;

		Pulse: record
			volume,
			dutyCycle:  Byte;
			ignoreDuty: Boolean;
		end;
		Saw: record
			accumulatorRate,
			accumulator: Byte;
		end;

		procedure SetFrequencyShift(shift: Byte);
		function  GetVolume: Byte;
		procedure WriteReg(addr: Word; value: Byte);
		procedure Clock;

		constructor Create(Kind: TVrc6ChannelType); overload;
	end;

	TVrc6Audio = class(TBaseExpansionAudio)
	private
		Channels: array[TVrc6ChannelType] of TVrc6Channel;

		HaltAudio:  Boolean;
		LastOutput: Int32;
	protected
		procedure ClockAudio; override;
	public
		procedure Reset;
		procedure WriteRegister(addr: Word; value: Byte);

		procedure LoadSnapshot; override;
		procedure SaveSnapshot; override;

		constructor Create; overload;
		destructor  Destroy; override;
	end;


implementation

uses
	Math, Basement.Util,
	NES.Types, NES.Console;

// ============================================================================
// TVrc6Channel
// ============================================================================

constructor TVrc6Channel.Create(Kind: TVrc6ChannelType);
var
	RegisterName: AnsiString;
begin
	case Kind of
		chPulse1: RegisterName := 'CH6.P1';
		chPulse2: RegisterName := 'CH6.P2';
		chSaw:    RegisterName := 'CH6.S1';
	end;

	inherited Create(RegisterName);

	RegisterProperty(8, @Pulse.volume);
	RegisterProperty(8, @Pulse.dutyCycle);
	RegisterProperty(1, @Pulse.ignoreDuty);

	RegisterProperty(8, @Saw.accumulatorRate);
	RegisterProperty(8, @Saw.accumulator);

	RegisterProperty(16, @Frequency);
	RegisterProperty(1,  @Enabled);
	RegisterProperty(32, @timer);
	RegisterProperty(8,  @step);
	RegisterProperty(8,  @frequencyShift);

	IsSaw := (Kind = chSaw);

	Saw.accumulatorRate := 0;
	Saw.accumulator := 0;

	Pulse.volume := 0;
	Pulse.dutyCycle := 0;
	Pulse.ignoreDuty := False;

	Frequency := 1;
	Enabled := False;
	timer := 1;
	step := 0;
	frequencyShift := 0;
end;

procedure TVrc6Channel.SetFrequencyShift(shift: Byte);
begin
	frequencyShift := shift;
end;

function TVrc6Channel.GetVolume: Byte;
begin
	if Enabled then
	begin
		if IsSaw then
			Result := Saw.accumulator shr 3
		else
		begin
			if Pulse.ignoreDuty then
				Result := Pulse.volume
			else
				Result := IfThen(step <= Pulse.dutyCycle, Pulse.volume, 0);
		end;
	end
	else
		Result := 0;
end;

procedure TVrc6Channel.WriteReg(addr: Word; value: Byte);
begin
	case (addr and $03) of

	0:	begin
			Saw.accumulatorRate := value and $3F;
			Pulse.volume     :=  value and $0F;
			Pulse.dutyCycle  := (value and $70) shr 4;
			Pulse.ignoreDuty := (value and $80) = $80;
		end;

	1:	frequency := (frequency and $0F00) or value;

	2:	begin
			frequency := (frequency and $FF) or ((value and $0F) shl 8);
			Enabled := (value and $80) = $80;
			if not Enabled then
			begin
				Saw.accumulator := 0;
				step := 0;
			end;
		end;

	end;
end;

procedure TVrc6Channel.Clock;
begin
	if not Enabled then Exit;

	Dec(timer);
	if timer = 0 then
	begin
		timer := (frequency shr frequencyShift) + 1;
		Inc(step);

		if IsSaw then
		begin
			step := step mod 14;

			if step = 0 then
				Saw.accumulator := 0
			else
			if not Odd(step) then
				Inc(Saw.accumulator, Saw.accumulatorRate);
		end
		else
			step := step and $0F;
	end;
end;

// ============================================================================
// TVrc6Audio
// ============================================================================

constructor TVrc6Audio.Create;
var
	Ch: TVrc6ChannelType;
begin
	inherited Create('VRCAUD');

	RegisterProperty(32, @LastOutput);
	RegisterProperty(1,  @HaltAudio);

	for Ch in TVrc6ChannelType do
		Channels[Ch] := TVrc6Channel.Create(Ch);

	Reset;
end;

destructor TVrc6Audio.Destroy;
var
	Ch: TVrc6ChannelType;
begin
	for Ch in TVrc6ChannelType do
		Channels[Ch].Free;

	inherited Destroy;
end;

procedure TVrc6Audio.ClockAudio;
var
	Ch: TVrc6ChannelType;
	OutputLevel: Integer;
begin
	OutputLevel := 0;

	for Ch in TVrc6ChannelType do
		with Channels[Ch] do
		begin
			if not HaltAudio then
				Clock;
			Inc(OutputLevel, GetVolume);
		end;

	NES_APU.AddExpansionAudioDelta(channel_VRC6, OutputLevel - LastOutput);
	LastOutput := OutputLevel;
end;

procedure TVrc6Audio.Reset;
begin
	LastOutput := 0;
	HaltAudio := False;
end;

procedure TVrc6Audio.WriteRegister(addr: Word; value: Byte);
var
	frequencyShift: Byte;
	Ch: TVrc6ChannelType;
begin
	case addr of
		$9000..$9002,
		$A000..$A002,
		$B000..$B002:
		begin
			Ch := TVrc6ChannelType(addr div $1000 - 9);
			Channels[Ch].WriteReg(addr, value);
		end;

		$9003:
		begin
			HaltAudio := Odd(value);
			frequencyShift := IfThen( (value and 4) = 4, 8, IfThen( (value and 2) = 2, 4, 0) );
			for Ch in TVrc6ChannelType do
				Channels[Ch].SetFrequencyShift(frequencyShift);
		end;
	end;
end;

procedure TVrc6Audio.LoadSnapshot;
var
	Ch: TVrc6ChannelType;
begin
	inherited LoadSnapshot;

	for Ch in TVrc6ChannelType do
		Channels[Ch].LoadSnapshot;
end;

procedure TVrc6Audio.SaveSnapshot;
var
	Ch: TVrc6ChannelType;
begin
	inherited SaveSnapshot;

	for Ch in TVrc6ChannelType do
		Channels[Ch].SaveSnapshot;
end;


end.

