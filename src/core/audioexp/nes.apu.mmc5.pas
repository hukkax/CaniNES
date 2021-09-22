unit NES.APU.MMC5;

interface

uses
	NES.Types, NES.SaveState,
	NES.APU, NES.APU.ExpansionAudio, NES.APU.Channel;

type
	TMMC5Square = class(TSquareChannel)
	private
		currentOutput: Int8;
	protected
		procedure InitializeSweep(regValue: Byte); override;
	public
		function  GetOutput: Int8;
		procedure RunChannel;

		constructor Create(asChannel1: Boolean); override;
	end;

	TMMC5Audio = class(TBaseExpansionAudio)
	private
		Square1,
		Square2:  TMMC5Square;

		audioCounter,
		lastOutput: Int16;

		pcmReadMode,
		pcmIrqEnabled: Boolean;
		pcmOutput: Byte;
	protected
		procedure ClockAudio; override;
	public
		procedure LoadSnapshot; override;
		procedure SaveSnapshot; override;

		function  ReadRegister(addr: Word): Byte;
		procedure WriteRegister(addr: Word; value: Byte);

		constructor Create; overload;
		destructor  Destroy; override;
	end;


implementation

uses
	SysUtils, Basement.Util,
	NES.Config, NES.Console, NES.CPU;

// ============================================================================
// TMMC5Square
// ============================================================================

constructor TMMC5Square.Create(asChannel1: Boolean);
begin
	inherited Create(asChannel1);

	currentOutput := 0;
	isMmc5Square := True;
	Reset(False);
end;

procedure TMMC5Square.InitializeSweep(regValue: Byte);
begin
	// "$5001 has no effect. The MMC5 pulse channels will not sweep, as they have no sweep unit."
end;

function TMMC5Square.GetOutput: Int8;
begin
	Result := currentOutput;
end;

procedure TMMC5Square.RunChannel;
begin
	if timer = 0 then
	begin
		dutyPos := (dutyPos - 1) and 7;
		// "Frequency values less than 8 do not silence the MMC5
		// pulse channels; they can output ultrasonic frequencies."
		currentOutput := DUTY_CYCLE_TABLE[duty][dutyPos] * GetVolume;
		timer := period;
	end
	else
		Dec(timer);
end;

// ============================================================================
// TMMC5Audio
// ============================================================================

constructor TMMC5Audio.Create;
begin
	inherited Create('MMCAUD');

	RegisterProperty(16, @audioCounter);
	RegisterProperty(16, @lastOutput);
	RegisterProperty(1,  @pcmReadMode);
	RegisterProperty(1,  @pcmIrqEnabled);
	RegisterProperty(8,  @pcmOutput);

	Square1 := TMMC5Square.Create(False);
	Square2 := TMMC5Square.Create(False);
end;

destructor TMMC5Audio.Destroy;
begin
	Square1.Free;
	Square2.Free;

	inherited Destroy;
end;

procedure TMMC5Audio.ClockAudio;
var
	summedOutput: Int16;
begin
	Dec(audioCounter);

	Square1.RunChannel;
	Square2.RunChannel;

	if audioCounter <= 0 then
	begin
		// ~240hz envelope/length counter
		audioCounter := Trunc(NES_CPU.GetClockRate(Console.Model) / 240);
		Square1.TickLengthCounter;
		Square1.TickEnvelope;
		Square2.TickLengthCounter;
		Square2.TickEnvelope;
	end;

	// "The sound output of the square channels are equivalent in volume to the corresponding APU channels"
	// "The polarity of all MMC5 channels is reversed compared to the APU."
	summedOutput := -(Square1.GetOutput + Square2.GetOutput + pcmOutput);
	if summedOutput <> lastOutput  then
	begin
		NES_APU.AddExpansionAudioDelta(channel_MMC5, summedOutput - lastOutput);
		lastOutput := summedOutput;
	end;

	Square1.ReloadCounter;
	Square2.ReloadCounter;
end;

function TMMC5Audio.ReadRegister(addr: Word): Byte;
begin
	case addr of

		$5010:
			Result := 0; // TODO: PCM IRQ

		$5015:
		begin
			Result := 0;
			if Square1.GetStatus then
				Result := Result or $01;
			if Square2.GetStatus then
				Result := Result or $02;
		end;

		else
			Result := Console.MemoryManager.GetOpenBus;
	end;
end;

procedure TMMC5Audio.WriteRegister(addr: Word; value: Byte);
begin
	case addr of

		$5000..$5003:
			Square1.WriteRAM(addr, value);

		$5004..$5007:
			Square2.WriteRAM(addr, value);

		$5010: // TODO: Read mode & PCM IRQs are not implemented
		begin
			pcmReadMode   := (value and $01) = $01;
			pcmIrqEnabled := (value and $80) = $80;
		end;

		$5011:
			if (not pcmReadMode) and (value <> 0) then
				pcmOutput := value;

		$5015:
		begin
			Square1.SetEnabled((value and 1) = 1);
			Square2.SetEnabled((value and 2) = 2);
		end;

	end;
end;

procedure TMMC5Audio.LoadSnapshot;
begin
	inherited LoadSnapshot;

	Square1.LoadSnapshot;
	Square2.LoadSnapshot;
end;

procedure TMMC5Audio.SaveSnapshot;
begin
	inherited SaveSnapshot;

	Square1.SaveSnapshot;
	Square2.SaveSnapshot;
end;


end.

