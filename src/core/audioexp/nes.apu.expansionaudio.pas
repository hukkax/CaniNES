unit NES.APU.ExpansionAudio;

{$mode delphi}

interface

uses
	Classes, SysUtils,
	NES.SaveState;

type
	TBaseExpansionAudio = class(TSnapshotable)
	protected
		procedure ClockAudio; virtual;
	public
		procedure Clock;
	end;

implementation

uses
	NES.Console;

{ TFDSAudio }

procedure TBaseExpansionAudio.ClockAudio;
begin
end;

procedure TBaseExpansionAudio.Clock;
begin
	if NES_APU.IsApuEnabled then
		ClockAudio;
end;

end.

