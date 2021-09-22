unit basement.easing;

{$mode delphi}

interface

uses
	Classes, SysUtils, Basement.Sprites;

const
	NegCosPi = 1.61803398874989; { 2 / -Cos(Pi * 1.2) }

type
	Float = Single;

	TEasing = function(Percent: Float): Float;

	TEasingDefaults = record
	public
		{ The default easing function with no interpolation }
		class function Linear(Percent: Float): Float; static;
		{ Slow, fast, then slow }
		class function Easy(Percent: Float): Float; static;
		{ Real slow, fast, then real slow }
		class function EasySlow(Percent: Float): Float; static;
		{ Wind up slow, fast, then overshoot and wind down slow }
		class function Extend(Percent: Float): Float; static;
		{ Slow then fast }
		class function Drop(Percent: Float): Float; static;
		{ Real slow then fast }
		class function DropSlow(Percent: Float): Float; static;
		{ Real slow then fast }
		class function Snap(Percent: Float): Float; static;
		{ Slow, fast, then bounce a few times }
		class function Bounce(Percent: Float): Float; static;
		{ Slow, fast, then bounce a few more times }
		class function Bouncy(Percent: Float): Float; static;
		{ Fast, then rebound slowly down   }
		class function Rubber(Percent: Float): Float; static;
		{ Fast, then rebound fast }
		class function Spring(Percent: Float): Float; static;
		{ Fast, then rebound realy fast }
		class function Boing(Percent: Float): Float; static;
	end;

	TEasingSprite = class(TTexture)
	private
		FX, FY, FScale,			// X, Y, Scale factor
		DestScale: Single;		// Destination X, Y, Scale for animation
		DestX, DestY: Integer;
		Delta,
		Percent:	Single;

		function	GetX: Integer;
		function	GetY: Integer;
		procedure	SetX(AValue: Integer);
		procedure	SetY(AValue: Integer);
	public
		property	X: Integer read GetX write SetX;
		property	Y: Integer read GetY write SetY;
		property	Scale: Single read FScale write FScale;

		procedure	MoveTo(NewX, NewY: Integer; NewScale: Single; Steps: Integer);
		procedure	Update;
	end;

	function Interpolate(Easing: TEasing; Percent, Start, Finish: Float): Float;

var
    EasingDefaults: TEasingDefaults;


implementation


function Power(const Base, Exponent: Float): Float;
begin
	if Exponent = 0 then
		Result := 1
	else if (Base = 0) and (Exponent > 0) then
		Result := 0
	else
		Result := Exp(Exponent * Ln(Base));
end;


class function TEasingDefaults.Linear(Percent: Float): Float;
begin
	Result := Percent;
end;

class function TEasingDefaults.Easy(Percent: Float): Float;
begin
	Result := Percent * Percent * (3 - 2 * Percent);
end;

class function TEasingDefaults.EasySlow(Percent: Float): Float;
begin
	Percent := Easy(Percent);
	Result := Percent * Percent * (3 - 2 * Percent);
end;

class function TEasingDefaults.Extend(Percent: Float): Float;
begin
	Percent := (Percent * 1.4) - 0.2;
	Result := 0.5 - Cos(Pi * Percent) / NegCosPi;
end;

class function TEasingDefaults.Drop(Percent: Float): Float;
begin
	Result := Percent * Percent;
end;

class function TEasingDefaults.DropSlow(Percent: Float): Float;
begin
	Result := Percent * Percent * Percent * Percent * Percent;
end;

class function TEasingDefaults.Snap(Percent: Float): Float;
begin
	Percent := Percent * Percent;
	Percent := (Percent * 1.4) - 0.2;
	Result := 0.5 - Cos(Pi * Percent) / NegCosPi;
end;

class function TEasingDefaults.Bounce(Percent: Float): Float;
begin
	if Percent > 0.9 then
	begin
		Result := Percent - 0.95;
		Result := 1 + Result * Result * 20 - (0.05 * 0.05 * 20);
	end
	else if Percent > 0.75 then
	begin
		Result := Percent - 0.825;
		Result := 1 + Result * Result * 16 - (0.075 * 0.075 * 16);
	end
	else if Percent > 0.5 then
	begin
		Result := Percent - 0.625;
		Result := 1 + Result * Result * 12 - (0.125 * 0.125 * 12);
	end
	else
	begin
		Percent := Percent * 2;
		Result := Percent * Percent;
	end;
end;

class function TEasingDefaults.Bouncy(Percent: Float): Float;
var
	Scale, Start, Step: Float;
begin
	Result := 1;
	Scale := 5;
	Start := 0.5;
	Step := 0.2;
	if Percent < Start then
	begin
		Result := Percent / Start;
		Result :=  Result * Result;
	end
	else
	while Step > 0.01 do
		if Percent < Start + Step then
		begin
			Step := Step / 2;
			Result := (Percent - (Start + Step)) * Scale;
			Result :=  Result * Result;
			Result := Result + 1 - Power(Step * Scale, 2);
			Break;
		end
		else
		begin
			Start := Start + Step;
			Step := Step * 0.6;
		end;
end;

class function TEasingDefaults.Rubber(Percent: Float): Float;
begin
	if Percent > 0.9 then
	begin
		Result := Percent - 0.95;
		Result := 1 - Result * Result * 20 + (0.05 * 0.05 * 20);
	end
	else if Percent > 0.75 then
	begin
		Result := Percent - 0.825;
		Result := 1 + Result * Result * 18 - (0.075 * 0.075 * 18);
	end
	else if Percent > 0.5 then
	begin
		Result := Percent - 0.625;
		Result := 1 - Result * Result * 14 + (0.125 * 0.125 * 14);
	end
	else
	begin
		Percent := Percent * 2;
		Result := Percent * Percent;
	end;
end;

class function TEasingDefaults.Spring(Percent: Float): Float;
begin
	Percent := Percent * Percent;
	Result := Sin(PI * Percent * Percent * 10 - PI / 2) / 4;
	Result := Result * (1 - Percent) + 1;
	if Percent < 0.3 then
		Result := Result * Easy(Percent / 0.3);
end;

class function TEasingDefaults.Boing(Percent: Float): Float;
begin
	Percent := Power(Percent, 1.5);
	Result := Sin(PI * Power(Percent, 2) * 20 - PI / 2) / 4;
	Result := Result * (1 - Percent) + 1;
	if Percent < 0.2 then
		Result := Result * Easy(Percent / 0.2);
end;


function Interpolate(Easing: TEasing; Percent, Start, Finish: Float): Float;
begin
	if Percent < 0 then
		Result := Start
	else
	if Percent > 1 then
		Result := Finish
	else
	begin
		Percent := Easing(Percent);
		Result := Start * (1 - Percent) + Finish * Percent;
	end;
end;


{ ================================================================================================}
{ TEasingSprite }
{ ================================================================================================}

function TEasingSprite.GetX: Integer;
begin
	Result := Trunc(FX);
end;

function TEasingSprite.GetY: Integer;
begin
	Result := Trunc(FY);
end;

procedure TEasingSprite.SetX(AValue: Integer);
begin
	FX := AValue;
	SetDestRect(Bounds(
		Trunc(FX - (Width  * FScale / 2)), Trunc(FY - (Height * FScale / 2)),
		Trunc(Width  * FScale), Trunc(Height * FScale)) );
end;

procedure TEasingSprite.SetY(AValue: Integer);
begin
	FY := AValue;
	SetDestRect(Bounds(
		Trunc(FX - (Width  * FScale / 2)), Trunc(FY - (Height * FScale / 2)),
		Trunc(Width  * FScale), Trunc(Height * FScale)) );
end;

procedure TEasingSprite.MoveTo(NewX, NewY: Integer; NewScale: Single; Steps: Integer);
begin
	DestX := NewX;
	DestY := NewY;
	DestScale := NewScale;

	Delta := 1.0 / Steps; // percent = 0..1
	Percent := 0;
end;

procedure TEasingSprite.Update;
var
	XX, YY, ZZ: Single;
	EaseFunc: TEasing;
begin
	if Percent < 1.0 then
	begin
		Percent += Delta;

		EaseFunc := EasingDefaults.Easy;

		XX := Interpolate(EaseFunc, Percent, FX, DestX);
		YY := Interpolate(EaseFunc, Percent, FY, DestY);
		ZZ := Interpolate(EaseFunc, Percent, FScale, DestScale);

		SetDestRect(Bounds(
			Trunc(XX - (Width  * ZZ / 2)),
			Trunc(YY - (Height * ZZ / 2)),
			Trunc(Width  * ZZ),
			Trunc(Height * ZZ) ));

		if Percent >= 1.0 then
		begin
			FX := XX;
			FY := YY;
			FScale := ZZ;
		end;
	end;

	Blit;
end;



end.

