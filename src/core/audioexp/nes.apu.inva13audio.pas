unit NES.APU.InvA13Audio;

{$mode Delphi}

interface

uses
	Classes, SysUtils,
	NES.Console, NES.APU.ExpansionAudio;

type
	TInvA13Audio = class(TBaseExpansionAudio)
	private
		currentOutput,
		lastOutput:    Byte;

		procedure UpdateOutputLevel;
	protected
		procedure ClockAudio; override;
	public
		//constructor Create; override;
	end;

implementation

{ TInvA13Audio }

procedure TInvA13Audio.UpdateOutputLevel;
begin
	if currentOutput <> lastOutput then
	begin
		Console.APU.AddExpansionAudioDelta(AudioChannel::InvOE1, currentOutput - lastOutput);
		lastOutput := currentOutput;
	end;
end;

procedure TInvA13Audio.ClockAudio;
begin
	currentOutput := _console->_InvOE1;
	UpdateOutputLevel;
end;

end.

