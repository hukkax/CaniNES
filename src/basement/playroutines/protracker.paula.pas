unit ProTracker.Paula;

interface

uses
	ProTracker.Util,
	ProTracker.Sample;

var
	EmptySample: TSample;


implementation

uses
	ProTracker.Player;


initialization

	EmptySample := TSample.Create;
	EmptySample.Resize(16);

finalization

	EmptySample.Free;

end.
