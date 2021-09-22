unit NES.APU.InvOE1Audio;

{$mode Delphi}

interface

uses
	Classes, SysUtils,
	NES.Console, NES.APU.ExpansionAudio;

type
	TInvOE1Audio = class(TBaseExpansionAudio)
	private
		currentOutput,
		lastOutput: Byte;

		procedure UpdateOutputLevel;
	protected
		procedure ClockAudio; override;
	public
		//constructor Create; override;
	end;

implementation

{ TInvOE1Audio }

procedure TInvOE1Audio.UpdateOutputLevel;
begin
	if currentOutput <> lastOutput then
	begin
		Console.APU.AddExpansionAudioDelta(AudioChannel::InvOE1, currentOutput - lastOutput);
		lastOutput := currentOutput;
	end;
end;

procedure TInvOE1Audio.ClockAudio;
begin
	currentOutput := _console->_InvOE1;
	UpdateOutputLevel;
end;

end.

