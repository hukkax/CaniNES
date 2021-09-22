// Measures the elapsed time in milliseconds between 
// calling Start and Stop. by hukka - hukka.ncn.fi

unit TimeMeasurer;

interface

uses
	Windows;

type
	TTimeMeasurer = record
	public
		Ctr1, Ctr2, Freq, Overhead: Int64;

		Milliseconds: Integer;
		MillisecondsFloat: Single;
		MillisecondsFiltered: Single;

		procedure Init;
		procedure Start;
		procedure Stop;
	end;


implementation


procedure TTimeMeasurer.Init;
begin
	QueryPerformanceFrequency(Freq);
	QueryPerformanceCounter(Ctr1);
	QueryPerformanceCounter(Ctr2);
	Overhead := Ctr2 - Ctr1; // determine API overhead
end;

procedure TTimeMeasurer.Start;
begin
	QueryPerformanceCounter(Ctr1);
end;

procedure TTimeMeasurer.Stop;
const
	FilterSpeed : Single = 0.05;
begin
	QueryPerformanceCounter(Ctr2);
	MillisecondsFloat := (((Ctr2 - Ctr1) - Overhead) / Freq) * 1000;
	Milliseconds := Round(MillisecondsFloat);
	MillisecondsFiltered := MillisecondsFiltered * (1-FilterSpeed) + MillisecondsFloat * FilterSpeed;
end;

end.
