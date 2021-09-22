unit NES.APU.VRC7;

interface

uses
	Classes, SysUtils,
	NES.SaveState,
	NES.APU, NES.APU.ExpansionAudio,
	NES.APU.OPLL;

const
	Section_Buffer = 'VRC7BUFF';
	Section_Chip   = 'VRC7OPLL';

type
	TInputEntry = record
		addr,
		data,
		cycle: Byte;
		wrote: Boolean;
	end;

	TVrc7Audio = class(TBaseExpansionAudio)
	const
		CycleCount = 12;
		INPUTBUFFERSIZE = CycleCount;
	private
		Chip: TOPLL;

		LastOutput, CurrentOutput: Int16;
		Clock: Double;
		InputBuffer: array[0..INPUTBUFFERSIZE] of TInputEntry;

		procedure WriteToChip(a, d: Byte);

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
// TVrc7Audio
// ============================================================================

constructor TVrc7Audio.Create;
var
	i: Integer;
begin
	inherited Create('VRC7');

	Chip := TOPLL.Create;

	RegisterProperty(16, @LastOutput);
	RegisterProperty(16, @CurrentOutput);
	RegisterProperty(64, @Clock);

	for i := 0 to INPUTBUFFERSIZE do
		InputBuffer[i] := default(TInputEntry);

//	Stream(lastOutput, currentOutput, inputBuffer, chip, clock);
end;

destructor TVrc7Audio.Destroy;
begin
	Chip.Free;

	inherited Destroy;
end;

procedure TVrc7Audio.WriteToChip(a, d: Byte);
var
	c: Byte;
begin
	c := Floor(Clock) mod CycleCount;
	with InputBuffer[c] do
	begin
		addr  := a;
		data  := d;
		cycle := c;
		wrote := True;
	end;
end;

procedure TVrc7Audio.ClockAudio;
var
	cycle: Integer;
begin
	Clock := Clock + ((3579545 / 4) / NES_CPU.GetClockRate(Console.Model));

	while Clock >= CycleCount do
	begin
		CurrentOutput := 0;

		for cycle := 0 to CycleCount-1 do
		begin
			Clock := Clock - 1;

			Inc(CurrentOutput, Chip.Clock());

			with InputBuffer[cycle] do
				if wrote then
				begin
					wrote := False;
					Chip.Write(addr, data);
				end;
		end;

		NES_APU.AddExpansionAudioDelta(channel_VRC7, CurrentOutput - LastOutput);
		LastOutput := CurrentOutput;
	end;
end;

procedure TVrc7Audio.Reset;
begin
	LastOutput := 0;
end;

procedure TVrc7Audio.WriteRegister(addr: Word; value: Byte);
begin
	if (addr = $9010) or (addr = $9030) then
		WriteToChip(BoolVal[(addr and $F030) = $9030], value);
end;

procedure TVrc7Audio.LoadSnapshot;
var
	i: Integer;
begin
	inherited LoadSnapshot;

	StartSection(Section_Buffer);

	for i := 0 to INPUTBUFFERSIZE do
	begin
		with InputBuffer[i] do
		begin
			addr  := Stream.ReadByte;
			data  := Stream.ReadByte;
			cycle := Stream.ReadByte;
			wrote := (Stream.ReadByte <> 0);
		end;
	end;

	Chip.LoadSnapshot;
end;

procedure TVrc7Audio.SaveSnapshot;
var
	i: Integer;
begin
	inherited SaveSnapshot;

	StartSection(Section_Buffer);

	for i := 0 to INPUTBUFFERSIZE do
	begin
		with InputBuffer[i] do
		begin
			Stream.WriteByte(addr);
			Stream.WriteByte(data);
			Stream.WriteByte(cycle);
			Stream.WriteByte(IfThen(wrote, 1, 0));
		end;
	end;

	Chip.SaveSnapshot;
end;


end.

