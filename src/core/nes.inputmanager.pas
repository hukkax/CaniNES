unit NES.InputManager;

{$mode delphi}

interface

uses
	Classes, Types,
	Basement.Window;

var
	InputManager: record

		Mouse: record
			Pos: TPoint;
			Buttons: array[TMouseButton] of Boolean;
		end;

	end;

implementation

end.

