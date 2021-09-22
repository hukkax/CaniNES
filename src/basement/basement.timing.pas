// Measures the elapsed time in milliseconds between 
// calling Start and Stop. by hukka - hukka.ncn.fi

unit Basement.Timing;

interface

uses
	SDL2;

type
	TTimeMeasurer = record
	private
		Initialized: Boolean;
		Overhead: Int64;
		Freq: Double;
	public
		Ctr1, Ctr2: UInt64;

		MillisecondsFloat: Double;
		//MillisecondsFiltered: Double;

		procedure Init;
		procedure Start;
		function  Stop: Double;
	end;

var
	Timing: TTimeMeasurer;


implementation


procedure TTimeMeasurer.Init;
begin
	Freq := SDL_GetPerformanceFrequency / 1000;

	Ctr1 := SDL_GetPerformanceCounter;
	Ctr2 := SDL_GetPerformanceCounter;
	Overhead := 0;//Ctr2 - Ctr1; // determine API overhead

	Initialized := True;
end;

procedure TTimeMeasurer.Start;
begin
	if not Initialized then Init;

	Ctr1 := SDL_GetPerformanceCounter;
end;

function TTimeMeasurer.Stop: Double;
//const FilterSpeed : Single = 0.05;
begin
	Ctr2 := SDL_GetPerformanceCounter;
	MillisecondsFloat := (Ctr2 - Ctr1 - Overhead) / Freq;
//	MillisecondsFiltered := MillisecondsFiltered * (1-FilterSpeed) + MillisecondsFloat * FilterSpeed;
	Result := MillisecondsFloat;
end;

initialization

	Timing := Default(TTimeMeasurer);
	Timing.Initialized := False;

end.
