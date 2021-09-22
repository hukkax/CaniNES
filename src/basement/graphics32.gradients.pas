unit Graphics32.Gradients;

{$MODE DELPHI}
{$INLINE ON}

interface

uses
	Classes, SysUtils, Graphics32;

type
	TMixColorFunc = function(Color1, Color2: TColor32; Mix: Single): TColor32;

	TFloatColor32 = record
		R, G, B: Single;
		procedure FromColor32(Color: TColor32); inline;
		function  ToColor32: TColor32; inline;
		function  ColorSum: Single; inline;
		procedure sRGBInverseCompanding; inline;
		procedure sRGBCompanding; inline;
	end;

	function  LinearInterpolation(brightness1, brightness2, mix: Single): Single; inline;
	function  From_sRGB(f: Single): Single; inline;
	function  To_sRGB(x: Single): Single; inline;

	function  GetMixColorFunc(Perceptual: Boolean): TMixColorFunc; inline;

	function  MixColor(Color1, Color2: TColor32; Mix: Single): TColor32; inline;
	function  MixColorPerceptual(Color1, Color2: TColor32; Mix: Single): TColor32; inline;

	procedure FillGradientPalette(var Palette: TPalette32);

	procedure GradientH(Buffer: TBitmap32; R: TRect; Color1, Color2: TColor32;
	          Perceptual: Boolean = True);
	procedure GradientV(Buffer: TBitmap32; R: TRect; Color1, Color2: TColor32;
	          Perceptual: Boolean = True);
	procedure GradientD(Buffer: TBitmap32; R: TRect; Pos1, Pos2: TPoint;
	          Color1, Color2: TColor32; Perceptual: Boolean = True);
	procedure GradientR(Buffer: TBitmap32; R: TRect; CenterOffset: TPoint;
	          Color1, Color2: TColor32; Perceptual: Boolean = True);


implementation

uses
	Math, Graphics32.LowLevel;

// ================================================================================================
// Utility
// ================================================================================================

function LinearInterpolation(brightness1, brightness2, mix: Single): Single;
begin
	Result := (brightness1 * (1.0 - mix)) + (brightness2 * mix);
end;

// Returns a linear value in the range 0..1 for sRGB input in 0..255
function From_sRGB(f: Single): Single;
begin
	if f <= 0.04045 then
		Result := f / 12.92
	else
		Result := Power(((f + 0.055) / 1.055), 2.4);
end;

// Returns a sRGB value in the range 0..1 for linear input in 0..1
function To_sRGB(x: Single): Single;
begin
	if x <= 0.0031308 then
		Result := x * 12.92
	else
		Result := (1.055 * (Power(x, 1 / 2.4)) ) - 0.055;
end;

function ClampToByte(F: Single): Integer;
begin
	Result := Trunc(F);
	if F < 0 then
		F := 0
	else
	if F > 255 then
		F := 255;
end;

function GetCol(c: Single): Byte; inline;
const
	mul = 255.9999;
begin
	Result := ClampToByte(mul * c);
end;

function FloatRGBToColor32(R, G, B: Single): TColor32; inline;
begin
	Result := GetCol(R) shl 16 or GetCol(G) shl 8 or GetCol(B);
end;

function GetMixColorFunc(Perceptual: Boolean): TMixColorFunc;
begin
	if Perceptual then
		Result := MixColorPerceptual
	else
		Result := MixColor;
end;

{ Implements "Mark's method" color mixing algorithm

 https://stackoverflow.com/questions/22607043/color-gradient-algorithm

   Input:
      color1: Color, (rgb)   The first color to mix
      color2: Color, (rgb)   The second color to mix
      mix:    Number, (0..1) The mix ratio. 0 ==> pure Color1, 1 ==> pure Color2
   Output:
      color:  Color, (rgb)   The mixed color
}
function MixColor(Color1, Color2: TColor32; Mix: Single): TColor32;
var
	Res, FC1, FC2: TFloatColor32;
begin
	if Mix < 0.0 then
		Mix := 0.0
	else
	if Mix >= 1.0 then
		Mix := 1.0;

	//Convert each color component from 0..255 to 0..1
	FC1.FromColor32(Color1);
	FC2.FromColor32(Color2);

	//Linearly interpolate rgb values using mix (0..1)
	Res.R := LinearInterpolation(FC1.R, FC2.R, Mix);
	Res.G := LinearInterpolation(FC1.G, FC2.G, Mix);
	Res.B := LinearInterpolation(FC1.B, FC2.B, Mix);

	//Convert color components from 0..1 to 0..255
	Result := Res.ToColor32;
end;

function MixColorPerceptual(Color1, Color2: TColor32; Mix: Single): TColor32;
const
	Gamma = 0.43;
var
	Col1: TColor32Entry absolute Color1;
	Col2: TColor32Entry absolute Color2;
	//Res, FC1, FC2: TFloatColor32;
	R,G,B, R1,G1,B1, R2,G2,B2,
	factor, brightness1, brightness2: Single;
begin
	if Mix < 0.0 then
		Mix := 0.0
	else
	if Mix >= 1.0 then
		Mix := 1.0;

	// Convert each color component from 0..255 to 0..1
	// Apply inverse sRGB companding to convert each channel into linear light

	//FC1.FromColor32(Color1);
	//FC1.sRGBInverseCompanding;
	R1 := From_sRGB(Col1.R / 255);
	G1 := From_sRGB(Col1.G / 255);
	B1 := From_sRGB(Col1.B / 255);

	//FC2.FromColor32(Color2);
	//FC2.sRGBInverseCompanding;
	R2 := From_sRGB(Col2.R / 255);
	G2 := From_sRGB(Col2.G / 255);
	B2 := From_sRGB(Col2.B / 255);

	//Linearly interpolate rgb values using mix (0..1)
	R := LinearInterpolation(R1, R2, Mix);
	G := LinearInterpolation(G1, G2, Mix);
	B := LinearInterpolation(B1, B2, Mix);

	//Compute a measure of brightness of the two colors using empirically determined gamma
	brightness1 := Power(R1 + G1 + B1, Gamma);
	brightness2 := Power(R2 + G2 + B2, Gamma);

	//Interpolate a new brightness value, and convert back to linear light
	brightness1 := LinearInterpolation(brightness1, brightness2, Mix);

	//Apply adjustment factor to each rgb value based
	factor := R + G + B;
	if factor > 0.0 then
	begin
		factor := Power(brightness1, 1 / Gamma) / factor;
		R *= factor;
		G *= factor;
		B *= factor;
	end;

	//Apply sRGB companding to convert from linear to perceptual light
	//Res.sRGBCompanding;
	R := To_sRGB(R);
	G := To_sRGB(G);
	B := To_sRGB(B);

	//Convert color components from 0..1 to 0..255
	Result := FloatRGBToColor32(R,G,B);
end;

procedure FillGradientPalette(var Palette: TPalette32);
var
	P, I, X: Integer;
	C1, C2: TColor32;
begin
	P := -1;
	C1 := Palette[0];
	for I := 0 to 255 do
	begin
		if Palette[I] > 0 then
		begin
			if P < 0 then
			begin
				P := I;
				C1 := Palette[I];
			end
			else
			begin
				C2 := Palette[I];
				for X := P to I do
					Palette[X] := MixColorPerceptual(C1, C2, (X-P)/(I-P)  );
				P := I;
				C1 := C2;
			end;
		end;
	end;
end;

// ================================================================================================
// TFloatColor32
// ================================================================================================

procedure TFloatColor32.FromColor32(Color: TColor32);
var
	Col: TColor32Entry absolute Color;
begin
	R := Col.R / 255;
	G := Col.G / 255;
	B := Col.B / 255;
end;

procedure TFloatColor32.sRGBInverseCompanding;
begin
	R := From_sRGB(R);
	G := From_sRGB(G);
	B := From_sRGB(B);
end;

procedure TFloatColor32.sRGBCompanding;
begin
	R := To_sRGB(R);
	G := To_sRGB(G);
	B := To_sRGB(B);
end;

function TFloatColor32.ToColor32: TColor32;
begin
	Result := GetCol(R) shl 16 or GetCol(G) shl 8 or GetCol(B);
end;

function TFloatColor32.ColorSum: Single;
begin
    Result := R + G + B;
end;

// ================================================================================================
// Gradient drawing
// ================================================================================================

// Horizontal gradient
procedure GradientH(Buffer: TBitmap32; R: TRect; Color1, Color2: TColor32;
	Perceptual: Boolean = True);
var
	X, W: Integer;
	MixColorFunc: TMixColorFunc;
begin
	MixColorFunc := GetMixColorFunc(Perceptual);

	R.Top    := Max(R.Top, 0);
	R.Bottom := Min(R.Bottom, Buffer.Height-1);
	W := R.Width;

	for X := R.Left to R.Right do
		if (X >= 0) and (X < Buffer.Width) then
			Buffer.VertLineS(X, R.Top, R.Bottom, MixColorFunc(Color1, Color2, X / W));
end;

// Vertical gradient
procedure GradientV(Buffer: TBitmap32; R: TRect; Color1, Color2: TColor32;
	Perceptual: Boolean = True);
var
	Y, H: Integer;
	MixColorFunc: TMixColorFunc;
begin
	MixColorFunc := GetMixColorFunc(Perceptual);

	R.Left  := Max(R.Left, 0);
	R.Right := Min(R.Right, Buffer.Width-1);
	H := R.Height;

	for Y := R.Top to R.Bottom do
		if (Y >= 0) and (Y < Buffer.Height) then
			Buffer.HorzLineS(R.Left, Y, R.Right, MixColorFunc(Color1, Color2, Y / H));
end;

// Diagonal gradient
procedure GradientD(Buffer: TBitmap32; R: TRect; Pos1, Pos2: TPoint;
	Color1, Color2: TColor32; Perceptual: Boolean = True);
var
	X, Y, W, H: Integer;
	z, x1, y1, x2, y2: Single;
	MixColorFunc: TMixColorFunc;
begin
	MixColorFunc := GetMixColorFunc(Perceptual);

	W := R.Width;
	H := R.Height;

	x1 := Pos1.X - R.Left;
	y1 := Pos1.Y - R.Top;
	x2 := Pos2.X - R.Left - x1;
	y2 := Pos2.Y - R.Top  - y1;
	z := (x2 * x2) + (y2 * y2);

	for Y := 0 to H-1 do
	for X := 0 to W-1 do
		Buffer.PixelS[X + R.Left, Y + R.Top] :=
			MixColorFunc(Color1, Color2, ((x - x1) * x2 + (y - y1) * y2) / z);

	Buffer.PixelS[Pos1.X, Pos1.Y] := clAqua32;
	Buffer.PixelS[Pos2.X, Pos2.Y] := clYellow32;
end;

// Radial gradient (unoptimized!)
procedure GradientR(Buffer: TBitmap32; R: TRect; CenterOffset: TPoint;
	Color1, Color2: TColor32; Perceptual: Boolean = True);
var
	x, y, ym, d: Single;
	cx, cy, xx, yy: Integer;
	MixColorFunc: TMixColorFunc;
	DrawnTop, DrawnLeft: Boolean;
	QR: TRect;
	SP, DP: PColor32;
begin
	MixColorFunc := GetMixColorFunc(Perceptual);

	// centerpoint
	cx := CenterOffset.X - R.Left;
	cy := CenterOffset.Y - R.Top;

	// figure out the largest quadrant
	DrawnLeft := (cx >= (R.Width  div 2));
	DrawnTop  := (cy >= (R.Height div 2));

	// max distance from center
	d := Sqrt(cx*cx + cy*cy);

	cx := CenterOffset.X;
	cy := CenterOffset.Y;

	QR := R;

	if DrawnLeft then
		QR.Right := CenterOffset.X
	else
		QR.Left := CenterOffset.X;

	if DrawnTop then
		QR.Bottom := CenterOffset.Y
	else
		QR.Top := CenterOffset.Y;

	// draw only the largest quadrant, rest will be mirrored
	for yy := QR.Top to QR.Bottom-1 do
	begin
		y := yy - cy + 0.5;
		ym := y * y;
		for xx := QR.Left to QR.Right-1 do
		begin
			// coordinates relative to center, shifted to pixel centers
			x := xx - cx + 0.5;
			Buffer.Pixel[xx,yy] := MixColorFunc(Color2, Color1, Sqrt(x*x + ym) / d);
		end;
	end;

	// mirror X
	if DrawnLeft then
	begin
		// mirror left half to right half
		for yy := QR.Top to QR.Bottom-1 do
		begin
			SP := Buffer.PixelPtr[QR.Right-1, yy];
			DP := Buffer.PixelPtr[QR.Right,   yy];
			for xx := QR.Right to R.Right-1 do
			begin
				DP^ := SP^;
				Dec(SP);
				Inc(DP);
			end;
		end;
	end
	else
	begin
		// mirror right half to left half
		for yy := QR.Top to QR.Bottom-1 do
		begin
			SP := Buffer.PixelPtr[QR.Left,   yy];
			DP := Buffer.PixelPtr[QR.Left-1, yy];
			for xx := R.Left to QR.Left-1 do
			begin
				DP^ := SP^;
				Inc(SP);
				Dec(DP);
			end;
		end;
	end;

	cx := R.Left;
	// mirror Y
	if DrawnTop then
	begin
		// mirror top half to bottom half
		cy := QR.Bottom-1; // source scanline
		for yy := QR.Bottom to R.Bottom-1 do
		begin
			MoveLongword(Buffer.PixelPtr[cx,cy]^, Buffer.PixelPtr[cx,yy]^, R.Width);
			Dec(cy);
		end;
	end
	else
	begin
		// mirror bottom half to top half
		cy := QR.Top; // source scanline
		for yy := QR.Top-1 downto R.Top do
		begin
			MoveLongword(Buffer.PixelPtr[cx,cy]^, Buffer.PixelPtr[cx,yy]^, R.Width);
			Inc(cy);
		end;
	end;

	Buffer.PixelS[CenterOffset.X, CenterOffset.Y] := clYellow32;
end;


end.

