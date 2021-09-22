unit NES.APU.Sunsoft5B;

// original: SSGAudio from Mesen
// TODO: implement the new version

interface

uses
	NES.Types, NES.SaveState,
	NES.APU, NES.APU.ExpansionAudio;


type
	TSunsoft5bAudio = class(TBaseExpansionAudio)
	private
		currentRegister: Byte;
		lastOutput:      Int16;
		processTick:     Boolean;

		volumeLut: array[0..15] of Byte;
		registers: array[0..15] of Byte;
		timer:     array[0..2]  of Int16;
		toneStep:  array[0..2]  of Byte;

		function  GetPeriod(channel: Integer): Word;
		function  GetEnvelopePeriod: Word;
		function  GetNoisePeriod: Byte;
		function  GetVolume(channel: Integer): Byte;
		function  IsEnvelopeEnabled(channel: Integer): Boolean;
		function  IsToneEnabled(channel: Integer): Boolean;
		function  IsNoiseEnabled(channel: Integer): Boolean;

		procedure UpdateChannel(channel: Integer);
		procedure UpdateOutputLevel;

	protected
		procedure ClockAudio; override;

	public
		procedure WriteRegister(addr: Word; value: Byte);

		constructor Create; overload;
	end;


implementation

uses
	SysUtils, Basement.Util,
	NES.Config, NES.Console, NES.CPU;

// ============================================================================
// TSunsoft5bAudio
// ============================================================================

constructor TSunsoft5bAudio.Create;
var
	i: Integer;
	output: Single = 1.0;
begin
	inherited Create('SS5AUD');

	RegisterProperty(8,  @currentRegister);
	RegisterProperty(16, @lastOutput);
	RegisterProperty(1,  @processTick);

	RegisterArray(Length(volumeLut), @volumeLut[0]);
	RegisterArray(Length(registers), @registers[0]);
	RegisterArray(Length(timer)*2,   @timer[0]);
	RegisterArray(Length(toneStep),  @toneStep[0]);

	volumeLut[0] := 0;
	for i := 0 to High(volumeLut) do
	begin
		//+1.5 dB 2x for every 1 step in volume
		output *= 1.1885022274370184377301224648922;
		output *= 1.1885022274370184377301224648922;
		volumeLut[i] := Trunc(output);
	end;
end;

function TSunsoft5bAudio.GetPeriod(channel: Integer): Word;
begin
	Result := registers[channel * 2] or (registers[channel * 2 + 1] shl 8);
end;

function TSunsoft5bAudio.GetEnvelopePeriod: Word;
begin
	Result := registers[$0B] or (registers[$0C] shl 8);
end;

function TSunsoft5bAudio.GetNoisePeriod: Byte;
begin
	Result := registers[6];
end;

function TSunsoft5bAudio.GetVolume(channel: Integer): Byte;
begin
	Result := volumeLut[registers[8 + channel] and $0F];
end;

function TSunsoft5bAudio.IsEnvelopeEnabled(channel: Integer): Boolean;
begin
	Result := (registers[8 + channel] and $10) = $10;
end;

function TSunsoft5bAudio.IsToneEnabled(channel: Integer): Boolean;
begin
	Result := ((registers[7] shr channel) and $01) = $00;
end;

function TSunsoft5bAudio.IsNoiseEnabled(channel: Integer): Boolean;
begin
	Result := ((registers[7] shr (channel + 3)) and $01) = $00;
end;

procedure TSunsoft5bAudio.UpdateChannel(channel: Integer);
begin
	Dec(timer[channel]);
	if timer[channel] <= 0 then
	begin
		{$R-}timer[channel] := GetPeriod(channel);
		toneStep[channel] := (toneStep[channel] + 1) and $0F;
	end;
end;

procedure TSunsoft5bAudio.UpdateOutputLevel;
var
	i: Integer;
	summedOutput: Int16 = 0;
begin
	for i := 0 to 2 do
		if (IsToneEnabled(i)) and (toneStep[i] < $08) then
			Inc(summedOutput, GetVolume(i));

	NES_APU.AddExpansionAudioDelta(channel_Sunsoft5B, summedOutput - lastOutput);
	lastOutput := summedOutput;
end;

procedure TSunsoft5bAudio.ClockAudio;
var
	i: Integer;
begin
	if processTick then
	begin
		for i := 0 to 2 do
			UpdateChannel(i);
		UpdateOutputLevel;
	end;
	processTick := not processTick;
end;

procedure TSunsoft5bAudio.WriteRegister(addr: Word; value: Byte);
begin
	case (addr and $E000) of
		$C000: currentRegister := value and $0F;
		$E000: registers[currentRegister] := value;
	end;
end;


end.

