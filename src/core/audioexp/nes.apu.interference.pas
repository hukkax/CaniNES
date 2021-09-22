unit NES.APU.Interference;

{$mode Delphi}

interface

uses
	Classes, SysUtils,
	NES.APU.ExpansionAudio;

type
	TAudioInterference = class(TBaseExpansionAudio)
	private
		ChannelType,
		CurrentOutput,
		LastOutput: Byte;
	protected
		procedure ClockAudio; override;
	public
		Interference: Byte;

		constructor Create(AChannelType: Byte); overload;
	end;


implementation

uses
	NES.Console;

{ TAudioInterference }

constructor TAudioInterference.Create(AChannelType: Byte);
begin
	inherited Create;

	ChannelType := AChannelType;
end;

procedure TAudioInterference.ClockAudio;
begin
	CurrentOutput := Interference;

	if CurrentOutput <> LastOutput then
	begin
		Console.APU.AddExpansionAudioDelta(ChannelType, CurrentOutput - LastOutput);
		LastOutput := CurrentOutput;
	end;
end;

end.

