unit Graphics32;

// Minimal version of Graphics32 for Basement framework by hukka
// Adapted from Gr32.pas and Gr32_LowLevel.pas; original license follows
// Resampling code adapted from BGRABitmap (BGRAResample.pas; LGPL-3.0 with linking exception)

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1 or LGPL 2.1 with linking exception
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * Alternatively, the contents of this file may be used under the terms of the
 * Free Pascal modified version of the GNU Lesser General Public License
 * Version 2.1 (the "FPC modified LGPL License"), in which case the provisions
 * of this license are applicable instead of those above.
 * Please see the file LICENSE.txt for additional information concerning this
 * license.
 *
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2009
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Michael Hansen <dyster_tid@hotmail.com>
 *   Andre Beckedorf <Andre@metaException.de>
 *   Mattias Andersson <mattias@centaurix.com>
 *   J. Tulach <tulach at position.cz>
 *   Jouni Airaksinen <markvera at spacesynth.net>
 *   Timothy Weber <teejaydub at users.sourceforge.net>
 *
 * ***** END LICENSE BLOCK ***** *)

{$MODE DELPHI}
{$INLINE ON}
{$POINTERMATH ON}

{$IFDEF RASPI}
	{$DEFINE PUREPASCAL}
{$ELSE}
	{$ASMMODE INTEL}
{$ENDIF}

{$DEFINE SUPPORT_PNG}
{$DEFINE SUPPORT_JPG}
{.$DEFINE SUPPORT_IFF}
{.$DEFINE SUPPORT_PCX}

interface

uses
	Classes, SysUtils, Types,
	Graphics32.LowLevel;

type
	TColor32 = UInt32;
	PColor32 = ^TColor32;

	TColor32Array = array of TColor32;
	PColor32Array = ^TColor32Array;

	TPalette32 = array[Byte] of TColor32;

	TColor32Component = (ccRed, ccGreen, ccBlue, ccAlpha);
	TColor32Components = set of TColor32Component;

	TColor32Entry = packed record
	case Integer of
		0: (B, G, R, A: Byte);
		1: (ARGB: TColor32);
		2: (Planes: array[0..3] of Byte);
		3: (Components: array[TColor32Component] of Byte);
	end;
	PColor32Entry = ^TColor32Entry;

	TResampleFilter = (
		rfBox,			// Equivalent of simple stretch with high quality and pixel-centered coordinates
		rfLinear,		// Linear interpolation giving slow transition between pixels
		rfHalfCosine,	// Mix of rfLinear and rfCosine giving medium speed stransition between pixels
		rfCosine,		// Cosine-like interpolation giving fast transition between pixels
		rfBicubic,		// Simple bi-cubic filter (blurry)
		rfMitchell,		// Mitchell filter, good for downsizing interpolation
		rfSpline,		// Spline filter, good for upsizing interpolation, however slightly blurry
		rfLanczos2,		// Lanczos with radius 2, blur is corrected
		rfLanczos3,		// Lanczos with radius 3, high contrast
		rfLanczos4,		// Lanczos with radius 4, high contrast
		rfBestQuality	// Best quality using rfMitchell or rfSpline
	);

	TBitmap32 = class(TPersistent)
	private
		Raster:		TPoint;

		function 	GetPixelPtr(X, Y: Integer): PColor32; inline;
		function 	GetScanline(Y: Integer): PColor32; inline;
	public
		Width,
		Height: 	Word;
		BoundsRect,
		ClipRect:	TRect;
		Bits:		array of TColor32;
		Dirty:		Boolean;

		procedure	AssignTo(Dest: TPersistent); override;
		procedure	Assign(Source: TPersistent); override;

		function 	LoadPNGFromResource(const Name: String): Boolean;
		function	LoadFromStream(Stream: TStream): Boolean; virtual;
		function	LoadFromFile(const Filename: String): Boolean; virtual;
		procedure 	SaveToFile(const Filename: String);

		function	SaneCoords(X1, Y1, X2, Y2: Integer): Boolean; inline;
		function	ValidCoords(X, Y: Integer): Boolean; inline;
		procedure	ValidateRect(var R: TRect); inline;
		function	ValidateCoords(var X1, Y1, X2, Y2: Integer): Boolean; inline;
		function 	ValidateX(var X: Integer): Boolean; inline;
		function 	ValidateY(var Y: Integer): Boolean; inline;
		procedure	SetSize(W, H: Cardinal); virtual;
		procedure 	MoveTo(X, Y: Integer); inline;
		procedure 	ResetClipRect;
		procedure	Changed; inline;
		procedure 	ResetAlpha(Alpha: Byte = 255);

		procedure 	Draw(DstX, DstY: Integer; Src: TBitmap32); overload;
		procedure 	Draw(DstX, DstY: Integer; const SrcRect: TRect; Src: TBitmap32); overload;
		procedure 	DrawColorKey(DstX, DstY: Integer; TransColor: TColor32; Src: TBitmap32); overload;
		procedure 	DrawColorKey(DstX, DstY: Integer;
					const SrcRect: TRect; TransColor: TColor32; Src: TBitmap32); overload;
		procedure 	FillRect(R: TRect; Value: TColor32); overload;
		procedure 	FillRect(X1, Y1, X2, Y2: Integer; Value: TColor32); overload;
		procedure 	FillRectS(R: TRect; Value: TColor32); overload;
		procedure 	FillRectS(X1, Y1, X2, Y2: Integer; Value: TColor32); overload;
		procedure 	FillRectT(X1, Y1, X2, Y2: Integer; Value: TColor32);
		procedure 	FillRectTS(R: TRect; Value: TColor32); overload;
		procedure 	FillRectTS(X1, Y1, X2, Y2: Integer; Value: TColor32); overload;
		procedure 	FrameRect(R: TRect; Value: TColor32); overload;
		procedure 	FrameRect(X1, Y1, X2, Y2: Integer; Value: TColor32); overload;
		procedure 	ThickFrameRect(R: TRect; Thickness: Integer; Value: TColor32);
		procedure 	Clear(FillColor: TColor32);
		procedure 	HorzLine(X1, Y, X2: Integer; Value: TColor32);
		procedure 	HorzLineS(X1, Y, X2: Integer; Value: TColor32);
		procedure 	HorzLineT(X1, Y, X2: Integer; Value: TColor32);
		procedure 	HorzLineTS(X1, Y, X2: Integer; Value: TColor32);
		procedure 	VertLine(X, Y1, Y2: Integer; Value: TColor32);
		procedure 	VertLineS(X, Y1, Y2: Integer; Value: TColor32);
		procedure 	VertLineT(X, Y1, Y2: Integer; Value: TColor32);
		procedure 	VertLineTS(X, Y1, Y2: Integer; Value: TColor32);
		procedure 	Line(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean);
		procedure 	LineS(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean);
		procedure 	LineTo(X, Y: Integer; PenColor: TColor32);
		procedure 	LineToS(X, Y: Integer; PenColor: TColor32);

		procedure	GradientRectT(R: TRect; Color1, Color2: TColor32; Horizontal: Boolean);
		procedure	GradientRect(R: TRect; Color1, Color2: TColor32; Horizontal: Boolean);
		function	FloodFill(X, Y: Integer; FillColor: TColor32): Cardinal;
		function	ReplaceColor(ColorSource, ColorDest: TColor32): Cardinal;

		procedure	SetPixel(X, Y: Integer; Value: TColor32); inline;
		procedure	SetPixelS(X, Y: Integer; Value: TColor32); inline;
		function	GetPixel(X, Y: Integer): TColor32; inline;
		function	GetPixelS(X, Y: Integer): TColor32; inline;

		property	Pixel[X, Y: Integer]: TColor32 read GetPixel write SetPixel; default;
		property	PixelS[X, Y: Integer]: TColor32 read GetPixelS write SetPixelS;
	    property 	PixelPtr[X, Y: Integer]: PColor32 read GetPixelPtr;
	    property 	Scanline[Y: Integer]: PColor32 read GetScanline;

		constructor Create; overload;
		constructor Create(W, H: Integer); overload;

		function  	GetResized(NewWidth, NewHeight: Cardinal; Filter: TResampleFilter): TBitmap32;
		procedure 	Resize(NewWidth, NewHeight: Cardinal; Filter: TResampleFilter);
		procedure	ScaleToFit(NewWidth, NewHeight: Cardinal; AllowUpscale: Boolean);

		procedure	Crop(R: TRect);

		function	Clone: TBitmap32;
		function	CroppedCopy(R: TRect): TBitmap32;
		function	DoubledCopy: TBitmap32;
		function	CroppedRect(BgColor: TColor32; Padding: Integer = 0): TRect;
	end;


	TWideKernelFilter = class
		function Interpolation(t: single): single; virtual; abstract;
		function ShouldCheckRange: boolean; virtual; abstract;
		function KernelWidth: single; virtual; abstract;
	end;

	TMitchellKernel = class(TWideKernelFilter)
		function Interpolation(t: single): single; override;
		function ShouldCheckRange: boolean; override;
		function KernelWidth: single; override;
	end;

	TSplineKernel = class(TWideKernelFilter)
	public
		Coeff: single;
		constructor Create; overload;
		constructor Create(ACoeff: single); overload;
		function Interpolation(t: single): single; override;
		function ShouldCheckRange: boolean; override;
		function KernelWidth: single; override;
	end;

	TCubicKernel = class(TWideKernelFilter)
		function Pow3(x: single): single; inline;
		function Interpolation(t: single): single; override;
		function ShouldCheckRange: boolean; override;
		function KernelWidth: single; override;
	end;

	TLanczosKernel = class(TWideKernelFilter)
	private
		FNumberOfLobes: integer;
		FFactor: ValReal;
		procedure SetNumberOfLobes(AValue: integer);
	public
		constructor Create(ANumberOfLobes: integer);
		function Interpolation(t: single): single; override;
		function ShouldCheckRange: boolean; override;
		function KernelWidth: single; override;
		property NumberOfLobes : integer read FNumberOfLobes write SetNumberOfLobes;
	end;

	{$I graphics32.colors.inc}

	function  LoadImage(const Filename: String): TBitmap32;
	function  Bitmap32FromData(Width, Height: Word; Data: PCardinal): TBitmap32;

	function  Color32(R, G, B: Byte): TColor32; overload; inline;
	function  Color32(R, G, B, A: Byte): TColor32; overload; inline;
	{$IFDEF SUPPORT_JPG}{
	function  Color32(C: TColor): TColor32; overload; inline;}
	{$ENDIF}
	function  SetAlpha(Color: TColor32; A: Byte): TColor32; inline;
	procedure Color32ToRGB(Color: TColor32; var R, G, B: Byte);
	procedure Color32ToRGBA(Color: TColor32; var R, G, B, A: Byte);
	function  Gray32(Intensity: Byte; Alpha: Byte = $FF): TColor32; inline;
	function  RedComponent(Color: TColor32): Byte; inline;
	function  GreenComponent(Color: TColor32): Byte; inline;
	function  BlueComponent(Color: TColor32): Byte; inline;
	function  AlphaComponent(Color: TColor32): Integer; inline;
	function  Intensity(Color: TColor32): Integer; inline;
	function  InvertColor(Color: TColor32): TColor32; inline;
	function  BlendColors(Col1, Col2: TColor32): TColor32; inline;
	function  MergeColors(Col1, Col2: TColor32): TColor32; inline;
	function  HSLtoRGB(H, S, L: Single): TColor32; overload;
	function  HSLtoRGB(H, S, L: Integer; A: Integer = $FF): TColor32; overload;
	procedure RGBtoHSL(RGB: TColor32; out H, S, L : Single); overload;
	procedure RGBtoHSL(RGB: TColor32; out H, S, L: Byte); overload;
	function  Lighten(C: TColor32; Amount: Integer): TColor32; inline;
	function  Brightness(C: TColor32; Amount: Single): TColor32; inline;
	function  BrightnessOpt(C: TColor32; Amount: Single): TColor32; inline;


{-------------------------------- Fine resample ------------------------------------}

type
	TPointRec = record
		Pos:    Integer;
		Weight: Single;
	end;

	TCluster = array of TPointRec;
	TMappingTable = array of TCluster;

	function BuildMappingTable(DstLo, DstHi, ClipLo, ClipHi, SrcLo, SrcHi: Integer;
		KernelSmaller, KernelLarger: TWideKernelFilter): TMappingTable;

	function FineResample(bmp: TBitmap32; NewWidth, NewHeight: Integer;
		ResampleFilter: TResampleFilter): TBitmap32;

	function WideKernelResample(bmp: TBitmap32; NewWidth, NewHeight: Integer;
		ResampleFilterSmaller, ResampleFilterLarger: TWideKernelFilter): TBitmap32;


implementation

uses
	{$IFDEF SUPPORT_PNG}Graphics32.PNG, GR32_PortableNetworkGraphic,{$ENDIF}
	{$IFDEF SUPPORT_JPG}Graphics32.JPG,{$ENDIF}
	{$IFDEF SUPPORT_PCX}Graphics32.PCX,{$ENDIF}
	{$IFDEF SUPPORT_IFF}Graphics32.IFF,{$ENDIF}
	Math, hkaFileUtils;

const
	{$ifdef UNICODE}
	RT_RCDATA = PWideChar(10);
	{$else}
	RT_RCDATA = PAnsiChar(10);
	{$endif}

{ ============================================================================}
{ Utility }
{ ============================================================================}

function LoadImage(const Filename: String): TBitmap32;
begin
	if not FileExists(Filename) then Exit(nil);
	Result := TBitmap32.Create;
	if not Result.LoadFromFile(Filename) then FreeAndNil(Result);
end;

function Bitmap32FromData(Width, Height: Word; Data: PCardinal): TBitmap32;
begin
	Result := TBitmap32.Create(Width, Height);
	Move(Data^, Result.Bits[0], Width * 4 * Height - 1);
end;

{ ============================================================================}
{ Color }
{ ============================================================================}

function Color32(R, G, B: Byte): TColor32;
begin
	Result := $FF000000 or (R shl 16) or (G shl 8) or B;
end;

function Color32(R, G, B, A: Byte): TColor32;
{$IFDEF CPU32____}
asm
	MOV		AH, A
	SHL		EAX, 16
	MOV		AH, DL
	MOV		AL, CL
end;
{$ELSE}
begin
	Result := (A shl 24) or (R shl 16) or (G shl 8) or B;
end;
{$ENDIF}

{$IFDEF SUPPORT_JPG}
{function Color32(C: TColor): TColor32;
begin
	Result := $FF000000 or (C and $0000FF shl 16) or (C and $00FF00) or (C and $FF0000 shr 16);
end;}
{$ENDIF}

function SetAlpha(Color: TColor32; A: Byte): TColor32;
begin
	Result := (A shl 24) or (Color and $FFFFFF);
end;

procedure Color32ToRGB(Color: TColor32; var R, G, B: Byte);
begin
  R := (Color and $00FF0000) shr 16;
  G := (Color and $0000FF00) shr 8;
  B :=  Color and $000000FF;
end;

procedure Color32ToRGBA(Color: TColor32; var R, G, B, A: Byte);
begin
  A :=  Color shr 24;
  R := (Color and $00FF0000) shr 16;
  G := (Color and $0000FF00) shr 8;
  B :=  Color and $000000FF;
end;

function Gray32(Intensity: Byte; Alpha: Byte = $FF): TColor32;
begin
	Result := Alpha shl 24 + Intensity shl 16 +
		Intensity shl 8 + Intensity;
end;

function RedComponent(Color: TColor32): Byte;
begin
	Result := (Color and $FF0000) shr 16;
end;
function GreenComponent(Color: TColor32): Byte;
begin
	Result := (Color and $00FF00) shr 8;
end;
function BlueComponent(Color: TColor32): Byte;
begin
	Result := Color and $0000FF;
end;

function AlphaComponent(Color: TColor32): Integer;
begin
	Result := Color shr 24;
end;

function Intensity(Color: TColor32): Integer;
begin
	// (R * 61 + G * 174 + B * 21) / 256
	Result := (
		(Color and $00FF0000) shr 16 * 61 +
		(Color and $0000FF00) shr 8 * 174 +
		(Color and $000000FF) * 21
	) shr 8;
end;

function InvertColor(Color: TColor32): TColor32;
begin
	TColor32Entry(Result).R := $FF - TColor32Entry(Color).R;
	TColor32Entry(Result).G := $FF - TColor32Entry(Color).G;
	TColor32Entry(Result).B := $FF - TColor32Entry(Color).B;
	TColor32Entry(Result).A :=       TColor32Entry(Color).A;
end;

function BlendColors(Col1, Col2: TColor32): TColor32;
var
	sA, dA: Single;
	cS: TColor32Entry absolute Col1;
	cD: TColor32Entry absolute Col2;
begin
	// Get alpha channels as 0..1
	sA := cS.A / 255;
	dA := cD.A / 255;

	// Calculate the source alpha channel (with blending value)
	//sA := sA * Constrain(opacity, 0.0, 1.0);

	// Blend RGB colors
	Result := Color32(
		Trunc(cS.R * sA + cD.R * dA - sA * dA * cD.R),
		Trunc(cS.G * sA + cD.G * dA - sA * dA * cD.G),
		Trunc(cS.B * sA + cD.B * dA - sA * dA * cD.B),
		Trunc((dA + sA - dA * sA) * 255) // alpha
	);
end;

// same as BlendMem_ASM, but slower
function MergeColors(Col1, Col2: TColor32): TColor32;
var
	Color1, Color2: TColor32Entry;
	A, R, G, B: Single;
begin
	Color1.ARGB := Col1;
	Color2.ARGB := Col2;
    R := (Color1.R * (255 - Color2.A) {%H-}+ Color2.R * Color2.A) / 255;
    G := (Color1.G * (255 - Color2.A) {%H-}+ Color2.G * Color2.A) / 255;
    B := (Color1.B * (255 - Color2.A) {%H-}+ Color2.B * Color2.A) / 255;
    A := 255 - ((255 - Color1.A) * (255 - Color2.A) / 255);
	Result := Color32(Trunc(R), Trunc(G), Trunc(B), Trunc(A));
end;

{ Color space conversions }

function HSLtoRGB(H, S, L: Single): TColor32;
const
	OneOverThree = 1 / 3;
var
	M1, M2: Single;

	function HueToColor(Hue: Single): Byte;
	var
		V: Double;
	begin
		Hue := Hue - Floor(Hue);
		if 6 * Hue < 1 then
			V := M1 + (M2 - M1) * Hue * 6
		else if 2 * Hue < 1 then
			V := M2
		else if 3 * Hue < 2 then
			V := M1 + (M2 - M1) * (2 * OneOverThree - Hue) * 6
		else V := M1;
		Result := Round($FF * V);
	end;

begin
	if S = 0 then
		Exit(Gray32(Round($FF * L)));

	if L <= 0.5 then
		M2 := L * (1 + S)
	else
		M2 := L + S - L * S;
	M1 := 2 * L - M2;
	Result := Color32(
		HueToColor(H + OneOverThree),
		HueToColor(H),
		HueToColor(H - OneOverThree));
end;

function HSLtoRGB(H, S, L, A: Integer): TColor32;
var
	V, M, M1, M2, VSF: Integer;
begin
	if L <= $7F then
		V := L * (256 + S) shr 8
	else
		V := L + S - Integer(Div255(L * S));
	if V <= 0 then
		Result := $FF000000
	else
	begin
		M := L * 2 - V;
		H := H * 6;
		VSF := (V - M) * (H and $FF) shr 8;
		M1 := M + VSF;
		M2 := V - VSF;
		case H shr 8 of
			0: Result := Color32(V, M1, M, A);
			1: Result := Color32(M2, V, M, A);
			2: Result := Color32(M, V, M1, A);
			3: Result := Color32(M, M2, V, A);
			4: Result := Color32(M1, M, V, A);
			5: Result := Color32(V, M, M2, A);
		else
			Result := 0;
		end;
	end;
end;

procedure RGBtoHSL(RGB: TColor32; out H, S, L : Single);
const
	R6 = 1 / 6; // reciprocal mul. opt.
	COne255th = 1 / $FF;
var
	R, G, B, D, Cmax, Cmin: Single;
begin
	R := RedComponent(RGB) * COne255th;
	G := GreenComponent(RGB) * COne255th;
	B := BlueComponent(RGB) * COne255th;
	Cmax := Max(R, Max(G, B));
	Cmin := Min(R, Min(G, B));
	L := (Cmax + Cmin) * 0.5;

	if Cmax = Cmin then
	begin
		H := 0; S := 0;
	end
	else
	begin
		D := Cmax - Cmin;
		if L < 0.5 then
			S := D / (Cmax + Cmin)
		else
			S := D / (2 - Cmax - Cmin);

		if R = Cmax then
			H := (G - B) / D
		else
		if G = Cmax then
			H := 2 + (B - R) / D
		else
			H := 4 + (R - G) / D;

		H := H * R6;
		if H < 0 then H := H + 1;
	end;
end;

procedure RGBtoHSL(RGB: TColor32; out H, S, L: Byte);
var
	R, G, B, D, Cmax, Cmin, HL: Integer;
begin
	R := (RGB shr 16) and $ff;
	G := (RGB shr 8) and $ff;
	B := RGB and $ff;

	Cmax := Max(R, G, B);
	Cmin := Min(R, G, B);
	L := (Cmax + Cmin) shr 1;

	if Cmax = Cmin then
	begin
		H := 0; S := 0;
	end
	else
	begin
		D := (Cmax - Cmin) * $FF;
		if L <= $7F then
			S := D div (Cmax + Cmin)
		else
			S := D div ($FF * 2 - Cmax - Cmin);

		D := D * 6;
		if R = Cmax then
			HL := (G - B) * $FF * $FF div D
		else if G = Cmax then
			HL := $FF * 2 div 6 + (B - R) * $FF * $FF div D
		else
			HL := $FF * 4 div 6 + (R - G) * $FF * $FF div D;

		if HL < 0 then HL := HL + $FF * 2;
		H := HL;
	end;
end;

function Lighten(C: TColor32; Amount: Integer): TColor32;
var
	r, g, b: Integer;
	CX: TColor32Entry absolute C;
begin
	r := CX.R + Amount;
	g := CX.G + Amount;
	b := CX.B + Amount;
	if r > 255 then r := 255 else if r < 0 then r := 0;
	if g > 255 then g := 255 else if g < 0 then g := 0;
	if b > 255 then b := 255 else if b < 0 then b := 0;
	Result := Color32(r, g, b, CX.a);
end;

function Brightness(C: TColor32; Amount: Single): TColor32;
var
	CX: TColor32Entry absolute C;
	r, g, b: Integer;
begin
	r := Trunc(CX.R * Amount);
	g := Trunc(CX.G * Amount);
	b := Trunc(CX.B * Amount);
	if r > 255 then r := 255 else if r < 0 then r := 0;
	if g > 255 then g := 255 else if g < 0 then g := 0;
	if b > 255 then b := 255 else if b < 0 then b := 0;
	Result := Color32(r, g, b, CX.a);
end;

// more optimized; Amount must be 0..1!
function BrightnessOpt(C: TColor32; Amount: Single): TColor32;
var
	CX: TColor32Entry absolute C;
begin
	Result := (C and $FF000000) or
		(Trunc(CX.R * Amount) shl 16) or
		(Trunc(CX.G * Amount) shl 8)  or
		(Trunc(CX.B * Amount));
end;

{ ============================================================================}
{ TBitmap32 }
{ ============================================================================}

constructor TBitmap32.Create;
begin
	Create(0, 0);
end;

constructor TBitmap32.Create(W, H: Integer);
begin
	SetSize(W, H);
	Dirty := False;
end;

procedure TBitmap32.Changed; inline;
begin
	Dirty := True;
end;

function TBitmap32.GetPixelPtr(X, Y: Integer): PColor32; inline;
begin
	Result := @Bits[X + Y * Width];
end;

function TBitmap32.GetScanline(Y: Integer): PColor32; inline;
begin
	Result := @Bits[Y * Width];
end;

procedure TBitmap32.AssignTo(Dest: TPersistent);
var
	D: TBitmap32;
begin
	if Dest is TBitmap32 then
	begin
		D := TBitmap32(Dest);
		D.SetSize(Width, Height);
		D.Draw(0, 0, Self);
	end
	else
		inherited;
end;

procedure TBitmap32.Assign(Source: TPersistent);
var
	S: TBitmap32;
begin
	if Source is TBitmap32 then
	begin
		S := TBitmap32(Source);
		SetSize(S.Width, S.Height);
		Draw(0, 0, S);
	end
	else
		inherited;
end;

function TBitmap32.GetPixel(X, Y: Integer): TColor32; inline;
begin
	Result := Bits[X + Y * Width];
end;

procedure TBitmap32.SetPixel(X, Y: Integer; Value: TColor32); inline;
begin
	Bits[X + Y * Width] := Value;
end;

function TBitmap32.ValidCoords(X, Y: Integer): Boolean;
begin
	Result := (X >= ClipRect.Left) and (X < ClipRect.Right) and
		(Y >= ClipRect.Top) and (Y < ClipRect.Bottom);
end;

function TBitmap32.GetPixelS(X, Y: Integer): TColor32;
begin
	if ValidCoords(X, Y) then
		Result := Bits[X + Y * Width]
	else
		Result := 0;//OuterColor;
end;

procedure TBitmap32.SetPixelS(X, Y: Integer; Value: TColor32);
begin
	if ValidCoords(X, Y) then
	begin
		Bits[X + Y * Width] := Value;
		Changed;
	end;
end;

function TBitmap32.Clone: TBitmap32;
begin
	Result := TBitmap32.Create(Width, Height);
	Result.Draw(0, 0, Self);
end;

function TBitmap32.SaneCoords(X1, Y1, X2, Y2: Integer): Boolean;
begin
	Result :=
		(X2 > X1) and (Y2 > Y1) and
		(X2 <= ClipRect.Right)  and (Y2 <= ClipRect.Bottom) and
		(X1 >= ClipRect.Left)   and (Y1 >= ClipRect.Top);
end;

function TBitmap32.ValidateX(var X: Integer): Boolean;
begin
	Result := False;
	if X < ClipRect.Left   then X := ClipRect.Left
	else
	if X >= ClipRect.Right then X := ClipRect.Right-1
	else
		Result := True;
end;

function TBitmap32.ValidateY(var Y: Integer): Boolean;
begin
	Result := False;
	if Y < ClipRect.Top     then Y := ClipRect.Top
	else
	if Y >= ClipRect.Bottom then Y := ClipRect.Bottom-1
	else
		Result := True;
end;

procedure TBitmap32.ValidateRect(var R: TRect);
begin
	if R.Left   < ClipRect.Left   then R.Left   := ClipRect.Left;
	if R.Top    < ClipRect.Top    then R.Top    := ClipRect.Top;
	if R.Right  > ClipRect.Right  then R.Right  := ClipRect.Right;
	if R.Bottom > ClipRect.Bottom then R.Bottom := ClipRect.Bottom;
end;

function TBitmap32.ValidateCoords(var X1, Y1, X2, Y2: Integer): Boolean;
begin
	if X1 < ClipRect.Left   then X1 := ClipRect.Left;
	if Y1 < ClipRect.Top    then Y1 := ClipRect.Top;
	if X2 > ClipRect.Right  then X2 := ClipRect.Right;
	if Y2 > ClipRect.Bottom then Y2 := ClipRect.Bottom;
	Result := (X1 < X2) and (Y1 < Y2);
end;

procedure TBitmap32.SetSize(W, H: Cardinal);
begin
	if (W = Width) and (H = Height) then Exit;
	Width  := W;
	Height := H;
	SetLength(Bits, W * H);
	ResetClipRect;
	Changed;
end;

procedure TBitmap32.Crop(R: TRect);
var
	Tmp: TBitmap32;
begin
	if R = BoundsRect then Exit;

	Tmp := TBitmap32.Create(R.Width, R.Height);
	Tmp.Draw(0, 0, R, Self);
	SetSize(Tmp.Width, Tmp.Height);
	Draw(0, 0, Tmp);
	Tmp.Free;
	ResetClipRect;
	Changed;
end;

function TBitmap32.CroppedCopy(R: TRect): TBitmap32;
begin
	Result := TBitmap32.Create(R.Width, R.Height);
	Result.Draw(0, 0, R, Self);
end;

function TBitmap32.CroppedRect(BgColor: TColor32; Padding: Integer = 0): TRect;

	function FindNonEmptyLineY(YY, Dir: Integer): Integer;
	var
		X, Y: Integer;
		Pix: PColor32;
	begin
		Y := YY;
		while (Y >= 0) and (Y < Height) do
		begin
			Pix := Scanline[Y];

			for X := 0 to Width-1 do
			begin
				if Pix^ <> BgColor then
					Exit(Y);
				Inc(Pix);
			end;

			Inc(Y, Dir);
		end;
		Result := Y;
	end;

	function FindNonEmptyLineX(XX, Dir: Integer): Integer;
	var
		X, Y: Integer;
	begin
		X := XX;
		while (X >= 0) and (X < Width) do
		begin
			for Y := 0 to Height-1 do
				if Pixel[X,Y] <> BgColor then
					Exit(X);
			Inc(X, Dir);
		end;
		Result := X;
	end;

begin
	Result.Top    := Max(FindNonEmptyLineY(0, 1) - Padding, 0);
	Result.Bottom := Min(FindNonEmptyLineY(Height-1, -1) + Padding, Height-1)+1;
	Result.Left   := Max(FindNonEmptyLineX(0, 1) - Padding, 0);
	Result.Right  := Min(FindNonEmptyLineX(Width-1, -1) + Padding, Width-1)+1;
end;

// Returns a pixel-doubled copy of the bitmap
function TBitmap32.DoubledCopy: TBitmap32;
var
	X, Y: Integer;
	SP, DP1, DP2: PCardinal;
begin
	Result := TBitmap32.Create(Width*2, Height*2);
	for Y := 0 to Height-1 do
	begin
		SP := GetPixelPtr(0, Y);
		DP1 := Result.GetPixelPtr(0, Y*2+0);
		DP2 := Result.GetPixelPtr(0, Y*2+1);
		for X := 0 to Width-1 do
		begin
			DP1^ := SP^; Inc(DP1); DP1^ := SP^; Inc(DP1);
			DP2^ := SP^; Inc(DP2); DP2^ := SP^; Inc(DP2);
			Inc(SP);
		end;
	end;
end;

procedure TBitmap32.ResetAlpha(Alpha: Byte = 255);
var
	X, Y: Integer;
begin
	for Y := 0 to Height-1 do
	for X := 0 to Width-1 do
		TColor32Entry(PixelPtr[X,Y]^).A := Alpha;
	Changed;
end;

function TBitmap32.LoadPNGFromResource(const Name: String): Boolean;
var
	ResStream: TResourceStream;
	bufPNG: TPortableNetworkGraphic32;
begin
	Result := False;
	ResStream := TResourceStream.Create(HInstance, Name, RT_RCDATA);

	bufPNG := TPortableNetworkGraphic32.Create;
	try
		bufPNG.LoadFromStream(ResStream);
		bufPNG.AssignTo(Self);
        SetSize(bufPNG.Width, bufPNG.Height);
		Result := True;
	finally
		bufPNG.Free;
		Changed;
	end;

	ResStream.Free;
end;

// PNG only for now !!!
//
function TBitmap32.LoadFromStream(Stream: TStream): Boolean;
const
	Hdr_PNG = $89504E47;
	Hdr_JPG = $FFD80000;
var
	HdrBytes: Cardinal;
	bufPNG: TPortableNetworkGraphic32;
	{$IFDEF SUPPORT_JPG}
	tmp: TBitmap32;
	{$ENDIF}
begin
	Result := False;

	Stream.Seek(0, soFromBeginning);
	HdrBytes := (Stream.ReadByte shl 24) or (Stream.ReadByte shl 16) or
		(Stream.ReadByte shl 8) or Stream.ReadByte;
	Stream.Seek(0, soFromBeginning);

	if HdrBytes = Hdr_PNG then
	begin
		bufPNG := TPortableNetworkGraphic32.Create;
		try
			bufPNG.LoadFromStream(Stream);
			bufPNG.AssignTo(Self);
	        SetSize(bufPNG.Width, bufPNG.Height);
			Result := True;
		finally
			bufPNG.Free;
		end;
	end
	{$IFDEF SUPPORT_JPG}
	else
	if (HdrBytes and $FFFF0000) = Hdr_JPG then
	try
		tmp := JPEGFromStream(Stream);
		if tmp <> nil then
		begin
			Self.Assign(tmp);
			Result := True;
		end;
	finally
		tmp.Free;
	end;
	{$ENDIF}

	if Result then Changed;
end;

function TBitmap32.LoadFromFile(const Filename: String): Boolean;
var
	Ext: String;
	{$IFDEF SUPPORT_PNG}bufPNG: TPortableNetworkGraphic32;{$ENDIF}
	{$IFDEF SUPPORT_JPG}tmp:    TBitmap32;{$ENDIF}
	{$IFDEF SUPPORT_PCX}bufPCX: TPCXImage;{$ENDIF}
	{$IFDEF SUPPORT_IFF}bufIFF: TIFFImage;{$ENDIF}
begin
	Result := False;
	Ext := UpperCase(ExtractFileExt(Filename));

	{$IFDEF SUPPORT_PNG}
	if Ext = '.PNG' then
	begin
		//LoadBitmap32FromPNG(Self, Filename);
		bufPNG := TPortableNetworkGraphic32.Create;
		try
			bufPNG.LoadFromFile(Filename);
			bufPNG.AssignTo(Self);
            SetSize(bufPNG.Width, bufPNG.Height);
			Result := True;
		finally
			bufPNG.Free;
			Changed;
		end;
		Exit;
	end;
	{$ENDIF}

	{$IFDEF SUPPORT_JPG}
	if Pos(Ext, '.JPG .JPEG') > 0 then
	begin
		try
			tmp := JPEGFromFile(Filename);
			if tmp <> nil then
			begin
				Self.Assign(tmp);
				Result := True;
			end;
		finally
			tmp.Free;
		end;
		Exit;
	end;
	{$ENDIF}

	{$IFDEF SUPPORT_PCX}
	if Ext = '.PCX' then
	begin
		bufPCX := TPCXImage.Create;
		try
			if bufPCX.LoadFromFile(Filename) then
			begin
				SetSize(bufPCX.Width, bufPCX.Height);
				bufPCX.DrawTo(Self);
				Result := True;
			end;
		finally
			bufPCX.Free;
			Changed;
		end;
		Exit;
	end;
	{$ENDIF}

	{$IFDEF SUPPORT_IFF}
	if Pos(Ext, '.IFF .ILBM .HAM') > 0 then
	begin
		bufIFF := TIFFImage.Create;
		try
			if bufIFF.LoadFromFile(Filename) then
			begin
				bufIFF.DrawTo(Self);
				ResetAlpha;
				Result := True;
			end;
		finally
			bufIFF.Free;
		end;
	end;
	{$ENDIF}
end;

// !!! TODO
procedure TBitmap32.SaveToFile(const Filename: String);
var
	S: RawByteString;
	X, Y: Integer;
	C: TColor32;
begin
	S := '';
	for Y := 0 to Height-1 do
	begin
		for X := 0 to Width-1 do
		begin
			C := Pixel[X,Y];
			S := S + Chr(RedComponent(C));
			S := S + Chr(GreenComponent(C));
			S := S + Chr(BlueComponent(C));
		end;
	end;

	StringToFile(Filename, S);
end;

procedure TBitmap32.Clear(FillColor: TColor32);
begin
	if (Width < 1) or (Height < 1) then Exit;
	FillLongword(Bits[0], Width * Height, FillColor);
	Changed;
end;

procedure TBitmap32.FillRectS(X1, Y1, X2, Y2: Integer; Value: TColor32);
begin
	if (X2 > X1) and (Y2 > Y1) and (X1 < ClipRect.Right) and (Y1 < ClipRect.Bottom) then
		if ValidateCoords(X1, Y1, X2, Y2) then
			FillRect(X1, Y1, X2, Y2, Value);
end;

procedure TBitmap32.FillRectS(R: TRect; Value: TColor32);
begin
	FillRectS(R.Left, R.Top, R.Right, R.Bottom, Value);
end;

procedure TBitmap32.FillRect(X1, Y1, X2, Y2: Integer; Value: TColor32);
var
	Y: Integer;
begin
	if (Width < 1) or (Height < 1) then Exit;
	for Y := Y1 to Y2-1 do
		FillLongWord(Bits[y * Width + x1], X2 - X1, Value);
	Changed;
end;

procedure TBitmap32.FillRect(R: TRect; Value: TColor32);
begin
	FillRectS(R.Left, R.Top, R.Right, R.Bottom, Value);
end;

procedure TBitmap32.FillRectT(X1, Y1, X2, Y2: Integer; Value: TColor32);
var
	Y: Integer;
begin
	if (Width < 1) or (Height < 1) then Exit;
	for Y := Y1 to Y2-1 do
		HorzLineT(X1, Y, X2, Value);
	Changed;
end;

procedure TBitmap32.FillRectTS(X1, Y1, X2, Y2: Integer; Value: TColor32);
begin
	if (X2 > X1) and (Y2 > Y1) and (X1 < ClipRect.Right) and (Y1 < ClipRect.Bottom) then
		if ValidateCoords(X1, Y1, X2, Y2) then
		begin
			if AlphaComponent(Value) < 255 then
				FillRectT(X1, Y1, X2, Y2, Value)
			else
				FillRect(X1, Y1, X2, Y2, Value);
		end;
end;

procedure TBitmap32.FillRectTS(R: TRect; Value: TColor32);
begin
	FillRectTS(R.Left, R.Top, R.Right, R.Bottom, Value);
end;

procedure TBitmap32.ThickFrameRect(R: TRect; Thickness: Integer; Value: TColor32);
var
	i: Integer;
begin
	if Thickness <> 0 then
	for i := 1 to Abs(Thickness) do
	begin
		FrameRect(R, Value);
		if Thickness > 0 then
			InflateRect(R, 1, 1)
		else
			InflateRect(R, -1, -1);
	end;
end;

procedure TBitmap32.FrameRect(X1, Y1, X2, Y2: Integer; Value: TColor32);
begin
	if SaneCoords(X1, Y1, X2, Y2) then
	begin
		Dec(Y2); Dec(X2);
		HorzLine(X1, Y1, X2, Value);
		if Y2 > Y1 then
			HorzLine(X1, Y2, X2, Value);
		if Y2 > Y1 + 1 then
		begin
			VertLine(X1, Y1+1, Y2-1, Value);
			if X2 > X1 then
				VertLine(X2, Y1+1, Y2-1, Value);
		end;
		Changed;
	end;
end;

procedure TBitmap32.FrameRect(R: TRect; Value: TColor32);
begin
	FrameRect(R.Left, R.Top, R.Right, R.Bottom, Value);
end;

procedure TBitmap32.HorzLine(X1, Y, X2: Integer; Value: TColor32);
begin
	if ValidateY(Y) then
	begin
		ValidateX(X1); ValidateX(X2);
		FillLongWord(Bits[X1 + Y * Width], X2 - X1 + 1, Value);
		Changed;
	end;
end;

procedure TBitmap32.HorzLineS(X1, Y, X2: Integer; Value: TColor32);
begin
	if (Y >= ClipRect.Top) and (Y < ClipRect.Bottom) and
		TestClip(X1, X2, ClipRect.Left, ClipRect.Right) then
			HorzLine(X1, Y, X2, Value);
end;

procedure TBitmap32.HorzLineT(X1, Y, X2: Integer; Value: TColor32);
var
	X: Integer;
	P: PColor32;
begin
	P := PixelPtr[X1, Y];
	for X := X1 to X2 do
	begin
		P^ := BlendColors(Value, P^);
		Inc(P);
	end;
	Changed;
end;

procedure TBitmap32.HorzLineTS(X1, Y, X2: Integer; Value: TColor32);
begin
	if ValidateY(Y) then
	begin
		ValidateX(X1); ValidateX(X2);
		if AlphaComponent(Value) < 255 then
			HorzLineT(X1, Y, X2, Value)
		else
			HorzLine(X1, Y, X2, Value);
	end;
end;

procedure TBitmap32.VertLine(X, Y1, Y2: Integer; Value: TColor32);
var
	I, NH, NL: Integer;
	P: PColor32;
begin
	if ValidateX(X) then
	begin
		ValidateY(Y1); ValidateY(Y2);
		if Y2 < Y1 then Exit;
		P := PixelPtr[X, Y1];
		I := Y2 - Y1 + 1;
		NH := I shr 2;
		NL := I and $03;
		for I := 0 to NH - 1 do
		begin
			P^ := Value; Inc(P, Width);
			P^ := Value; Inc(P, Width);
			P^ := Value; Inc(P, Width);
			P^ := Value; Inc(P, Width);
		end;
		for I := 0 to NL - 1 do
		begin
			P^ := Value; Inc(P, Width);
		end;
		Changed;
	end;
end;

procedure TBitmap32.VertLineS(X, Y1, Y2: Integer; Value: TColor32);
begin
	if (X >= ClipRect.Left) and (X < ClipRect.Right) and
		TestClip(Y1, Y2, ClipRect.Top, ClipRect.Bottom) then
			VertLine(X, Y1, Y2, Value);
end;

procedure TBitmap32.VertLineT(X, Y1, Y2: Integer; Value: TColor32);
var
	Y: Integer;
	P: PColor32;
begin
	P := PixelPtr[X, Y1];
	for Y := Y1 to Y2 do
	begin
		P^ := BlendColors(Value, P^);
		Inc(P, Width);
	end;
	Changed;
end;

procedure TBitmap32.VertLineTS(X, Y1, Y2: Integer; Value: TColor32);
begin
	if ValidateX(X) then
	begin
		ValidateY(Y1); ValidateY(Y2);
		if AlphaComponent(Value) < 255 then
			VertLineT(X, Y1, Y2, Value)
		else
			VertLine(X, Y1, Y2, Value);
	end;
end;

procedure TBitmap32.Line(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean);
var
	Dy, Dx, Sy, Sx, I, Delta: Integer;
	P: PColor32;
begin
	ValidateCoords(X1, Y1, X2, Y2);
	Dx := X2 - X1;
	Dy := Y2 - Y1;
	if Dx > 0 then
		Sx := 1
	else
	if Dx < 0 then
    begin
      Dx := -Dx;
      Sx := -1;
    end
    else // Dx = 0
    begin
      if Dy > 0 then VertLine(X1, Y1, Y2 - 1, Value)
      else if Dy < 0 then VertLine(X1, Y2 + 1, Y1, Value);
      if L then Pixel[X2, Y2] := Value;
      Exit;
    end;

    if Dy > 0 then Sy := 1
    else if Dy < 0 then
    begin
      Dy := -Dy;
      Sy := -1;
    end
    else // Dy = 0
    begin
      if X2 > X1 then HorzLine(X1, Y1, X2 - 1, Value)
      else HorzLine(X2 + 1, Y1, X1, Value);
      if L then Pixel[X2, Y2] := Value;
      Exit;
    end;

    P := PixelPtr[X1, Y1];
    Sy := Sy * Width;

    if Dx > Dy then
    begin
      Delta := Dx shr 1;
      for I := 0 to Dx - 1 do
      begin
        P^ := Value;
        Inc(P, Sx);
        Inc(Delta, Dy);
        if Delta >= Dx then
        begin
          Inc(P, Sy);
          Dec(Delta, Dx);
        end;
      end;
    end
    else // Dx < Dy
    begin
      Delta := Dy shr 1;
      for I := 0 to Dy - 1 do
      begin
        P^ := Value;
        Inc(P, Sy);
        Inc(Delta, Dx);
        if Delta >= Dy then
        begin
          Inc(P, Sx);
          Dec(Delta, Dy);
        end;
      end;
    end;
    if L then P^ := Value;

	Changed;
end;

procedure TBitmap32.LineS(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean);
var
	Dx2, Dy2,Cx1, Cx2, Cy1, Cy2, PI, Sx, Sy, Dx, Dy, xd, yd, rem, term, e: Integer;
	OC: Int64;
	Swapped, CheckAux: Boolean;
	P: PColor32;
begin
    Dx := X2 - X1; Dy := Y2 - Y1;

    // check for trivial cases...
    if Dx = 0 then // vertical line?
    begin
      if Dy > 0 then VertLineS(X1, Y1, Y2 - 1, Value)
      else if Dy < 0 then VertLineS(X1, Y2 + 1, Y1, Value);
      if L then PixelS[X2, Y2] := Value;
      Exit;
    end
    else if Dy = 0 then // horizontal line?
    begin
      if Dx > 0 then HorzLineS(X1, Y1, X2 - 1, Value)
      else if Dx < 0 then HorzLineS(X2 + 1, Y1, X1, Value);
      if L then PixelS[X2, Y2] := Value;
      Exit;
    end;

    Cx1 := ClipRect.Left; Cx2 := ClipRect.Right - 1;
    Cy1 := ClipRect.Top;  Cy2 := ClipRect.Bottom - 1;

    if Dx > 0 then
    begin
      if (X1 > Cx2) or (X2 < Cx1) then Exit; // segment not visible
      Sx := 1;
    end
    else
    begin
      if (X2 > Cx2) or (X1 < Cx1) then Exit; // segment not visible
      Sx := -1;
      X1 := -X1;   X2 := -X2;   Dx := -Dx;
      Cx1 := -Cx1; Cx2 := -Cx2;
      Swap(Cx1, Cx2);
    end;

    if Dy > 0 then
    begin
      if (Y1 > Cy2) or (Y2 < Cy1) then Exit; // segment not visible
      Sy := 1;
    end
    else
    begin
      if (Y2 > Cy2) or (Y1 < Cy1) then Exit; // segment not visible
      Sy := -1;
      Y1 := -Y1;   Y2 := -Y2;   Dy := -Dy;
      Cy1 := -Cy1; Cy2 := -Cy2;
      Swap(Cy1, Cy2);
    end;

    if Dx < Dy then
    begin
      Swapped := True;
      Swap(X1, Y1); Swap(X2, Y2); Swap(Dx, Dy);
      Swap(Cx1, Cy1); Swap(Cx2, Cy2); Swap(Sx, Sy);
    end
    else
      Swapped := False;

    // Bresenham's set up:
    Dx2 := Dx shl 1; Dy2 := Dy shl 1;
    xd := X1; yd := Y1; e := Dy2 - Dx; term := X2;
    CheckAux := True;

    // clipping rect horizontal entry
    if Y1 < Cy1 then
    begin
      OC := Int64(Dx2) * (Cy1 - Y1) - Dx;
      Inc(xd, OC div Dy2);
      rem := OC mod Dy2;
      if xd > Cx2 then Exit;
      if xd >= Cx1 then
      begin
        yd := Cy1;
        Dec(e, rem + Dx);
        if rem > 0 then
        begin
          Inc(xd);
          Inc(e, Dy2);
        end;
        CheckAux := False; // to avoid ugly goto we set this to omit the next check
      end;
    end;

    // clipping rect vertical entry
    if CheckAux and (X1 < Cx1) then
    begin
      OC := Int64(Dy2) * (Cx1 - X1);
      Inc(yd, OC div Dx2);
      rem := OC mod Dx2;
      if (yd > Cy2) or (yd = Cy2) and (rem >= Dx) then Exit;
      xd := Cx1;
      Inc(e, rem);
      if (rem >= Dx) then
      begin
        Inc(yd);
        Dec(e, Dx2);
      end;
    end;

    // set auxiliary var to indicate that term is not clipped, since
    // term still has the unclipped value assigned at setup.
    CheckAux := False;

    // is the segment exiting the clipping rect?
    if Y2 > Cy2 then
    begin
      OC := Int64(Dx2) * (Cy2 - Y1) + Dx;
      term := X1 + OC div Dy2;
      rem := OC mod Dy2;
      if rem = 0 then Dec(term);
      CheckAux := True; // set auxiliary var to indicate that term is clipped
    end;

    if term > Cx2 then
    begin
      term := Cx2;
      CheckAux := True; // set auxiliary var to indicate that term is clipped
    end;

    Inc(term);

    if Sy = -1 then
      yd := -yd;

    if Sx = -1 then
    begin
      xd := -xd;
      term := -term;
    end;

    Dec(Dx2, Dy2);

    if Swapped then
    begin
      PI := Sx * Width;
      P := @Bits[yd + xd * Width];
    end
    else
    begin
      PI := Sx;
      Sy := Sy * Width;
      P := @Bits[xd + yd * Width];
    end;

    // do we need to skip the last pixel of the line and is term not clipped?
    if not(L or CheckAux) then
    begin
      if xd < term then
        Dec(term)
      else
        Inc(term);
    end;

    while xd <> term do
    begin
      Inc(xd, Sx);

      P^ := Value;
      Inc(P, PI);
      if e >= 0 then
      begin
        Inc(P, Sy);
        Dec(e, Dx2);
      end
      else
        Inc(e, Dy2);
    end;

	Changed;
end;

procedure TBitmap32.MoveTo(X, Y: Integer);
begin
	Raster.X := X;
	Raster.Y := Y;
end;

procedure TBitmap32.LineTo(X, Y: Integer; PenColor: TColor32);
begin
	Line(Raster.X, Raster.Y, X, Y, PenColor, False);
	MoveTo(X, Y);
end;

procedure TBitmap32.LineToS(X, Y: Integer; PenColor: TColor32);
begin
	LineS(Raster.X, Raster.Y, X, Y, PenColor, False);
	MoveTo(X, Y);
end;

procedure TBitmap32.ResetClipRect;
begin
	ClipRect   := Types.Rect(0, 0, Width, Height);
	BoundsRect := Types.Rect(0, 0, Width, Height);
end;

procedure TBitmap32.Draw(DstX, DstY: Integer; Src: TBitmap32);
var
	SrcP, DstP: PColor32;
	W, Y, Y1, Y2: Integer;
begin
	if (Src = nil) or (DstX >= Width) or (DstY >= Height) then Exit;

	ValidateX(DstX);

	W  := Min(Src.Width, Self.Width);
	if W <= 0 then Exit;
	if DstX+W > BoundsRect.Right then
		W := BoundsRect.Right - DstX;

	Y1 := Max(DstY, 0);
	Y2 := Min(Self.Height, Y1+Src.Height);

	SrcP := Src.PixelPtr[0,0];
	DstP := Self.PixelPtr[DstX, Y1];

	try
		for Y := Y1 to Y2-1 do
		begin
			MoveLongWord(SrcP^, DstP^, W);
			Inc(SrcP, Src.Width);
			Inc(DstP, Width);
		end;
	finally
		{$IFNDEF PUREPASCAL}
		asm
			EMMS;	// not super sure if this is necessary
		end;
		{$ENDIF}
		Changed;
	end;
end;

procedure TBitmap32.Draw(DstX, DstY: Integer; const SrcRect: TRect; Src: TBitmap32);
var
	SrcP, DstP: PColor32;
	sr: TRect;
	W, Y, Y1, Y2: Integer;
begin
	if Src = nil then Exit;

	ValidateX(DstX);

	sr.Left   := Max(SrcRect.Left, 0);
	if sr.Left >= Src.Width then Exit;

	sr.Right  := Min(SrcRect.Right, Src.Width);
	if sr.Right < sr.Left then Exit;

	sr.Top    := Max(SrcRect.Top, 0);
	if sr.Top >= Src.Height then Exit;

	sr.Bottom := Min(SrcRect.Bottom, Src.Height);
	if sr.Bottom < sr.Top then Exit;

	W := Min(Src.Width, sr.Right - sr.Left);
	W := Min(W, Self.Width);
	if W <= 0 then Exit;
	if DstX + W > BoundsRect.Right then
		W := BoundsRect.Right - DstX;

	Y1 := Max(DstY, 0);
	if Y1 >= Self.Height then Exit;
	Y2 := Min(Src.Height, sr.Bottom - sr.Top) + Y1;
	if Y2 >= Self.Height then
		Y2 := Self.Height - 0;
	if (Y2 < Y1) then Exit;

	SrcP := Src.PixelPtr[sr.Left, sr.Top];
	DstP := Self.PixelPtr[DstX, Y1];

	try
		for Y := Y1 to Y2-1 do
		begin
			MoveLongWord(SrcP^, DstP^, W);
			Inc(SrcP, Src.Width);
			Inc(DstP, Width);
		end;
	finally
		{$IFNDEF PUREPASCAL}
		asm
			EMMS;	// not super sure if this is necessary
		end;
		{$ENDIF}
		Changed;
	end;
end;

procedure TBitmap32.DrawColorKey(DstX, DstY: Integer;
	TransColor: TColor32; Src: TBitmap32);
var
	SP, DP, SrcP, DstP: PColor32;
	X, W, Y, Y1, Y2: Integer;
begin
	if Src = nil then Exit;

	ValidateX(DstX);

	W  := Min(Src.Width, Self.Width);
	if W <= 0 then Exit;
	if DstX + W > BoundsRect.Right then
		W := BoundsRect.Right - DstX;

	Y1 := Max(DstY, 0);
	if Y1 >= Self.Height then Exit;
	Y2 := Min(Self.Height, Y1+Src.Height) - 1;
	if (Y2 < Y1) then Exit;

	SrcP := Src.PixelPtr[0,0];
	DstP := Self.PixelPtr[DstX, Y1];

	for Y := Y1 to Y2-1 do
	begin
		SP := SrcP;
		DP := DstP;
		for X := 0 to W-1 do
		begin
			if TransColor <> SP^ then DP^ := SP^;
			Inc(SP); Inc(DP);
		end;
		Inc(SrcP, Src.Width);
		Inc(DstP, Width);
	end;
	Changed;
end;

procedure TBitmap32.DrawColorKey(DstX, DstY: Integer;
	const SrcRect: TRect; TransColor: TColor32; Src: TBitmap32);
var
	SP, DP, SrcP, DstP: PColor32;
	X, W, Y, Y1, Y2: Integer;
begin
	if Src = nil then Exit;

	if (DstX < 0) or (DstY < 0) or (DstX >= Width) or (DstY >= Height) then Exit; // !!! temp

	ValidateX(DstX);

	W := Min(Src.Width, SrcRect.Right - SrcRect.Left);
	W := Min(W, Self.Width);
	if W <= 0 then Exit;
	if DstX+W > BoundsRect.Right then
		W := BoundsRect.Right - DstX;

	Y1 := Max(DstY, 0);
	Y  := Min(Src.Height, SrcRect.Bottom - SrcRect.Top);
	Y2 := Min(Self.Height, Y1+Y);

	SrcP := Src.PixelPtr[SrcRect.Left, SrcRect.Top];
	DstP := Self.PixelPtr[DstX, Y1];

	for Y := Y1 to Y2-1 do
	begin
		SP := SrcP;
		DP := DstP;
		for X := 0 to W-1 do
		begin
			if TransColor <> SP^ then DP^ := SP^;
			Inc(SP); Inc(DP);
		end;
		Inc(SrcP, Src.Width);
		Inc(DstP, Width);
	end;
	Changed;
end;

function TBitmap32.ReplaceColor(ColorSource, ColorDest: TColor32): Cardinal;
var
	P: PColor32;
	X, Y: Integer;
begin
	P := PixelPtr[0,0];
	Result := 0;
	for Y := 0 to Height-1 do
	for X := 0 to Width-1 do
	begin
		if P^ = ColorSource then
		begin
			P^ := ColorDest;
			Inc(Result);
		end;
		Inc(P);
	end;
	Changed;
end;

function TBitmap32.FloodFill(X, Y: Integer; FillColor: TColor32): Cardinal;
var
	ReplaceColor: TColor32;
	Stack: array of TPoint;

	procedure PutInStack(X, Y: Integer);
	begin
		SetLength(Stack, Length(Stack)+1);
		Stack[Length(Stack)-1] := Types.Point(X, Y);
	end;

	procedure GetFromStack(var X, Y: Integer);
	begin
		X := Stack[Length(Stack)-1].X;
		Y := Stack[Length(Stack)-1].Y;
		SetLength(Stack, Length(Stack)-1);
	end;

begin
	Result := 0;
	if (not ValidateX(X)) or (not ValidateY(Y)) then Exit;

	ReplaceColor := PixelS[X, Y];
	if ReplaceColor = FillColor then Exit;

	PutInStack(X, Y);
	while Length({%H-}Stack) > 0 do
	begin
		GetFromStack(X, Y);
		while (X > 0) and (Pixel[X-1, Y] = ReplaceColor) do
			Dec(X);
		while (X < Width) and (Pixel[X, Y] = ReplaceColor) do
		begin
			if Y > 0 then
				if Pixel[X, Y-1] = ReplaceColor then
					PutInStack(X, Y-1);
			if Y+1 < Height then
				if Pixel[X, Y+1] = ReplaceColor then
					PutInStack(X, Y+1);
			if Pixel[X,Y] <> FillColor then
			begin
				Pixel[X, Y] := FillColor;
				Inc(Result);
			end;
			Inc(X);
		end;
	end;
	Changed;
end;


function TBitmap32.GetResized(NewWidth, NewHeight: Cardinal; Filter: TResampleFilter): TBitmap32;
begin
	Result := FineResample(Self, NewWidth, NewHeight, Filter);
end;

procedure TBitmap32.Resize(NewWidth, NewHeight: Cardinal; Filter: TResampleFilter);
var
	tmp: TBitmap32;
begin
	tmp := GetResized(NewWidth, NewHeight, Filter);
	if Assigned(tmp) then
	begin
		SetSize(NewWidth, NewHeight);
		Clear(0);
		Draw(0, 0, tmp);
		tmp.Free;
	end;
end;

procedure TBitmap32.ScaleToFit(NewWidth, NewHeight: Cardinal; AllowUpscale: Boolean);
var
	P: TPoint;
begin
	P := Scale(Width, Height, NewWidth, NewHeight, AllowUpscale);
	if (P.X <> Width) or (P.Y <> Height) then
		Resize(P.X, P.Y, rfCosine);
end;

procedure TBitmap32.GradientRect(R: TRect; Color1, Color2: TColor32; Horizontal: Boolean);
var
	Col1: TColor32Entry absolute Color1;
	Col2: TColor32Entry absolute Color2;
	C: TColor32Entry;
	X, Y, W, H, cR, cG, cB,
	rdiv, gdiv, bdiv: Integer;
	Z: Single;
begin
	if Color1 = Color2 then
	begin
		FillRectS(R, Color1);
		Exit;
	end
	else
	if (AlphaComponent(Color1) < 255) or (AlphaComponent(Color2) < 255) then
	begin
		GradientRectT(R, Color1, Color2, Horizontal);
		Exit;
	end;

	ValidateRect(R);

	cR := Col1.R;
	cG := Col1.G;
	cB := Col1.B;

	rdiv := cR - Col2.R;
	gdiv := cG - Col2.G;
	bdiv := cB - Col2.B;

	C.A := 255;

	if Horizontal then
	begin
		W := R.Width-1;
		for X := R.Left to R.Right-1 do
		begin
			Z := (X - R.Left) / W;
			C.R := cR - Round(Z * bdiv);
			C.G := cG - Round(Z * gdiv);
			C.B := cB - Round(Z * rdiv);
			VertLine(X, R.Top, R.Bottom-1, C.ARGB);
		end;
	end
	else
	begin
		H := R.Height-1;
		for Y := R.Top to R.Bottom-1 do
		begin
			Z := (Y - R.Top) / H;
			C.R := cR - Round(Z * bdiv);
			C.G := cG - Round(Z * gdiv);
			C.B := cB - Round(Z * rdiv);
			HorzLine(R.Left, Y, R.Right-1, C.ARGB);
		end;
	end;
end;

procedure TBitmap32.GradientRectT(R: TRect; Color1, Color2: TColor32; Horizontal: Boolean);
var
	Col1: TColor32Entry absolute Color1;
	Col2: TColor32Entry absolute Color2;
	C: TColor32Entry;
	X, Y, W, H, cR, cG, cB, cA,
	rdiv, gdiv, bdiv, adiv: Integer;
	Z: Single;
begin
	if Color1 = Color2 then
	begin
		FillRectTS(R, Color1);
		Exit;
	end
	else
	if (AlphaComponent(Color1) = 255) and (AlphaComponent(Color2) = 255) then
	begin
		GradientRect(R, Color1, Color2, Horizontal);
		Exit;
	end;

	ValidateRect(R);

	cR := Col1.R;
	cG := Col1.G;
	cB := Col1.B;
	cA := Col1.A;

	rdiv := cR - Col2.R;
	gdiv := cG - Col2.G;
	bdiv := cB - Col2.B;
	adiv := cA - Col2.A;

	if Horizontal then
	begin
		W := R.Width-1;
		for X := R.Left to R.Right-1 do
		begin
			Z := (X - R.Left) / W;
			C.R := cR - Round(Z * bdiv);
			C.G := cG - Round(Z * gdiv);
			C.B := cB - Round(Z * rdiv);
			C.A := cA - Round(Z * adiv);
			VertLineT(X, R.Top, R.Bottom-1, C.ARGB);
		end;
	end
	else
	begin
		H := R.Height-1;
		for Y := R.Top to R.Bottom-1 do
		begin
			Z := (Y - R.Top) / H;
			C.R := cR - Round(Z * bdiv);
			C.G := cG - Round(Z * gdiv);
			C.B := cB - Round(Z * rdiv);
			C.A := cA - Round(Z * adiv);
			HorzLineT(R.Left, Y, R.Right-1, C.ARGB);
		end;
	end;
end;


// ================================================================================================
// Resampling
// ================================================================================================

{$warnings off}

function BuildMappingTable(DstLo, DstHi, ClipLo, ClipHi, SrcLo, SrcHi: Integer;
	KernelSmaller, KernelLarger: TWideKernelFilter): TMappingTable;
const
	FullEdge = False;
var
	SrcW, DstW, ClipW, Left, Right, I, J, K: Integer;
	FilterWidth, Scale, OldScale, Center, Weight: Single;
begin
	SrcW := SrcHi - SrcLo;
	DstW := DstHi - DstLo;
	ClipW := ClipHi - ClipLo;

	if SrcW = 0 then
		Exit(nil)
	else
	if SrcW = 1 then
	begin
		SetLength(Result, ClipW);
		for I := 0 to ClipW - 1 do
		begin
			SetLength(Result[I], 1);
			Result[I][0].Pos := 0;
			Result[I][0].Weight := 1;
		end;
		Exit;
	end;

	SetLength(Result, ClipW);
	if ClipW = 0 then Exit;

	if FullEdge then
		Scale := DstW / SrcW
	else
		Scale := (DstW - 1) / (SrcW - 1);

	K := 0;

	if Scale = 0 then
	begin
		SetLength(Result[0], 1);
		Result[0][0].Pos := (SrcLo + SrcHi) div 2;
		Result[0][0].Weight := 1;
	end
	else if Scale < 1 then
	begin
		FilterWidth := KernelSmaller.KernelWidth;
		OldScale := Scale;
		Scale := 1 / Scale;
		FilterWidth := FilterWidth * Scale;
		for I := 0 to ClipW - 1 do
		begin
			if FullEdge then
				Center := SrcLo - 0.5 + (I - DstLo + ClipLo + 0.5) * Scale
			else
				Center := SrcLo + (I - DstLo + ClipLo) * Scale;
			Left := Floor(Center - FilterWidth);
			Right := Ceil(Center + FilterWidth);
			for J := Left to Right do
			begin
				Weight := KernelSmaller.Interpolation((Center - J) * OldScale) * OldScale;
				if Weight <> 0 then
				begin
					K := Length(Result[I]);
					SetLength(Result[I], K + 1);
					Result[I][K].Pos := Constrain(J, SrcLo, SrcHi - 1);
					Result[I][K].Weight := Weight;
				end;
			end;
			if Length(Result[I]) = 0 then
			begin
				SetLength(Result[I], 1);
				Result[I][0].Pos := Floor(Center);
				Result[I][0].Weight := 1;
			end;
		end;
	end
	else // scale > 1
	begin
		FilterWidth := KernelLarger.KernelWidth;
		Scale := 1 / Scale;
		for I := 0 to ClipW - 1 do
		begin
			if FullEdge then
				Center := SrcLo - 0.5 + (I - DstLo + ClipLo + 0.5) * Scale
			else
				Center := SrcLo + (I - DstLo + ClipLo) * Scale;
			Left := Floor(Center - FilterWidth);
			Right := Ceil(Center + FilterWidth);
			for J := Left to Right do
			begin
				Weight := KernelLarger.Interpolation(Center - j);
				if Weight <> 0 then
				begin
					K := Length(Result[I]);
					SetLength(Result[I], k + 1);
					Result[I][K].Pos := Constrain(j, SrcLo, SrcHi - 1);
					Result[I][K].Weight := Weight;
				end;
			end;
		end;
	end;
end;

{$warnings on}

function FineInterpolation(t: Single; ResampleFilter: TResampleFilter): Single;
begin
	if ResampleFilter <= rfLinear then
	begin
		if ResampleFilter = rfBox then
			Result := Round(t)
		else
			Result := t;
	end
	else
	begin
		if t <= 0.5 then
			result := t * t * 2
		else
		result := 1 - (1 - t) * (1 - t) * 2;
		if ResampleFilter <> rfCosine then
			result := (result + t) * 0.5;
	end;
end;

function WideKernelResample(bmp: TBitmap32; NewWidth, NewHeight: Integer;
	ResampleFilterSmaller, ResampleFilterLarger: TWideKernelFilter): TBitmap32;
type
	TSum = record
		sumR, sumG, sumB, sumA: Single;
	end;

var
	mapX, mapY: TMappingTable;
	xb, yb, xc, yc, MapXLoPos, MapXHiPos: Integer;
	clusterX, clusterY: TCluster;
	verticalSum: array of TSum;
	sum: TSum;
	w, wa: Single;
	c: TColor32Entry;
	pdest: PColor32;
begin
	Result := TBitmap32.Create(NewWidth, NewHeight);
	if (NewWidth = 0) or (NewHeight = 0) then Exit;

	mapX := BuildMappingTable(0, NewWidth,  0, NewWidth,  0,
		bmp.Width,  ResampleFilterSmaller, ResampleFilterLarger);
	mapY := BuildMappingTable(0, NewHeight, 0, NewHeight, 0,
		bmp.Height, ResampleFilterSmaller, ResampleFilterLarger);

	MapXLoPos := MapX[0][0].Pos;
	MapXHiPos := MapX[NewWidth-1][High(MapX[NewWidth-1])].Pos;

	SetLength(verticalSum, MapXHiPos - MapXLoPos + 1);

	for yb := 0 to NewHeight-1 do
	begin
		clusterY := mapY[yb];

		for xb := MapXLoPos to MapXHiPos do
		begin
			FillChar(verticalSum[xb - MapXLoPos], SizeOf(verticalSum[xb - MapXLoPos]), 0);
			for yc := 0 to High(clusterY) do
				with verticalSum[xb - MapXLoPos] do
				begin
					c.ARGB := bmp.PixelPtr[xb, clusterY[yc].Pos]^;
					w  := clusterY[yc].Weight;
					wa := w * c.A;
					IncF(sumA, wa);
					IncF(sumR, c.R * wa);
					IncF(sumG, c.G * wa);
					IncF(sumB, c.B * wa);
				end;
		end;

		pdest := Result.Scanline[yb];

		for xb := 0 to NewWidth-1 do
		begin
			clusterX := mapX[xb];
			//{$H-}FillChar(sum, SizeOf(sum), 0);
			sum := Default(Tsum);
			for xc := 0 to High(clusterX) do
			begin
				w := clusterX[xc].Weight;
				with verticalSum[ClusterX[xc].Pos - MapXLoPos] do
				begin
					IncF(sum.sumA, sumA * w);
					IncF(sum.sumR, sumR * w);
					IncF(sum.sumG, sumG * w);
					IncF(sum.sumB, sumB * w);
				end;
			end;

			if sum.sumA < 0.5 then
				pdest^ := 0 //BGRAPixelTransparent
			else
			begin
				c.R := Constrain(Round(sum.sumR / sum.sumA), 0, 255);
				c.G := Constrain(Round(sum.sumG / sum.sumA), 0, 255);
				c.B := Constrain(Round(sum.sumB / sum.sumA), 0, 255);
				if sum.sumA > 255 then
					c.A := 255
				else
					c.A := Round(sum.sumA);
				pdest^ := c.ARGB;
			end;
			inc(pdest);
		end;
	end;
end;

function FineResampleLarger(bmp: TBitmap32; newWidth, newHeight: Integer;
	ResampleFilter: TResampleFilter): TBitmap32;
type
	TInterpolationEntry = record
		isrc1, isrc2, factCorr: Integer;
	end;
var
	yb, xb: Integer;
	pdest, psrc1, psrc2: PColor32;
	xsrc, ysrc, xfactor, yfactor: Double;
	xTab,yTab: array of TInterpolationEntry;
	xInfo,yInfo: TInterpolationEntry;
	cUpLeft, cUpRight, cLowLeft, cLowRight: TColor32Entry;
	factHoriz, factVert: Single;
	fUpLeft, fUpRight, fLowLeft, fLowRight: Integer;
	faUpLeft, faUpRight, faLowLeft, faLowRight: Integer;
	rSum, gSum, bSum, aSum: Integer;
	//temp: TBitmap32;
begin
	Result := nil;

	if (newWidth < bmp.Width) or (newHeight < bmp.Height) or
	   (newWidth = 0) or (newHeight = 0) then Exit;

	//bmp.LoadFromBitmapIfNeeded;

	if (bmp.Width = 1) and (bmp.Height = 1) then
	begin
		Result := TBitmap32.Create(NewWidth, NewHeight);
		Result.Clear(bmp.GetPixel(0, 0));
		Exit;
	end
	else
	if bmp.Width = 1 then
	begin
		{temp := TBitmap32.Create(2, bmp.Height);
		temp.PutImage(0, 0, bmp, dmSet);
		temp.PutImage(1, 0, bmp, dmSet);
		Result := FineResampleLarger(temp, 2, newHeight, ResampleFilter);
		temp.Free;
		temp := Result;
		Result := SimpleStretch(temp, newWidth,temp.Height);
		temp.Free;}
		Exit;
	end
	else
	if bmp.Height = 1 then
	begin
		{temp := bmp.NewBitmap(bmp.Width, 2);
		temp.PutImage(0, 0, bmp, dmSet);
		temp.PutImage(0, 1, bmp, dmSet);
		Result := FineResampleLarger(temp, newWidth, 2, ResampleFilter);
		temp.Free;
		temp := Result;
		Result := SimpleStretch(temp, temp.Width,newHeight);
		temp.Free;}
		Exit;
	end;

	Result := TBitmap32.Create(NewWidth, NewHeight);
	yfactor := (bmp.Height - 1) / (newHeight - 1);
	xfactor := (bmp.Width - 1)  / (newWidth - 1);

	SetLength(yTab, newHeight);
	for yb := 0 to newHeight - 1 do
	begin
		ysrc     := yb * yfactor;
		factVert := frac(ysrc);
		yTab[yb].isrc1   := floor(ysrc);
		yTab[yb].isrc2 := min(bmp.Height-1, ceil(ysrc));
		yTab[yb].factCorr := Round(FineInterpolation(factVert, ResampleFilter) * 256);
	end;

	SetLength(xTab, newWidth);
	for xb := 0 to newWidth - 1 do
	begin
		xsrc     := xb * xfactor;
		factHoriz := frac(xsrc);
		xTab[xb].isrc1   := floor(xsrc);
		xTab[xb].isrc2 := min(bmp.Width-1,ceil(xsrc));
		xTab[xb].factCorr := Round(FineInterpolation(factHoriz, ResampleFilter) * 256);
	end;

	for yb := 0 to newHeight - 1 do
	begin
		pdest    := Result.Scanline[yb];
		yInfo    := yTab[yb];
		psrc1    := bmp.scanline[yInfo.isrc1];
		psrc2    := bmp.scanline[yInfo.isrc2];
		for xb := 0 to newWidth - 1 do
		begin
			xInfo  := xTab[xb];

			cUpLeft.ARGB   := (psrc1 + xInfo.isrc1)^;
			cUpRight.ARGB  := (psrc1 + xInfo.isrc2)^;
			cLowLeft.ARGB  := (psrc2 + xInfo.isrc1)^;
			cLowRight.ARGB := (psrc2 + xInfo.isrc2)^;

			fLowRight := (xInfo.factCorr * yInfo.factCorr + 128) shr 8;
			fLowLeft := yInfo.factCorr - fLowRight;
			fUpRight := xInfo.factCorr - fLowRight;
			fUpLeft := (256 - xInfo.factCorr) - fLowLeft;

			faUpLeft   := fUpLeft   * cUpLeft.A;
			faUpRight  := fUpRight  * cUpRight.A;
			faLowLeft  := fLowLeft  * cLowLeft.A;
			faLowRight := fLowRight * cLowRight.A;

			rSum := cUpLeft.R * faUpLeft + cUpRight.R * faUpRight +
				cLowLeft.R * faLowLeft + cLowRight.R * faLowRight;
			gSum := cUpLeft.G * faUpLeft + cUpRight.G * faUpRight +
				cLowLeft.G * faLowLeft + cLowRight.G * faLowRight;
			bSum := cUpLeft.B * faUpLeft + cUpRight.B * faUpRight +
				cLowLeft.B * faLowLeft + cLowRight.B * faLowRight;
			aSum := cUpLeft.A * fUpLeft + cUpRight.A * fUpRight +
				cLowLeft.A * fLowLeft + cLowRight.A * fLowRight;

			if aSum = 0 then
				pdest^ := 0 //BGRAPixelTransparent
			else
				pdest^ := Color32(
					(rSum + aSum shr 1) div aSum,
					(gSum + aSum shr 1) div aSum,
					(bSum + aSum shr 1) div aSum,
					(aSum + 128) shr 8);
			Inc(pdest);
		end;
	end;
end;

function FineResampleSmaller(bmp: TBitmap32; newWidth, newHeight: Integer): TBitmap32;
var
	yb, xb, yb2, xb2: integer;
	pdest, psrc: PColor32;
	lineDelta, delta: Integer;
	xsrc1, ysrc1, xsrc2, ysrc2, xfactor, yfactor: Double;
	ixsrc1, ixsrc2, iysrc1, iysrc2, ixsrc1p1, ixsrc2m1, iysrc1p1, iysrc2m1: Integer;
	cBorder, cFull, cUpLeft, cUpRight, cLowLeft, cLowRight: TColor32Entry;
	factHoriz1, factHoriz2, factVert1, factVert2, Sum, fUpLeft, fUpRight,
	fLowLeft, fLowRight, faUpLeft, faUpRight, faLowLeft, faLowRight: Single;
	rSum, gSum, bSum, aSum: Double;
begin
	Result := nil;
	if (newWidth > bmp.Width) or (newHeight > bmp.Height) then Exit;

	Result := TBitmap32.Create(NewWidth, NewHeight);
	if (newWidth = 0) or (newHeight = 0) or (bmp.Width = 0) or (bmp.Height = 0) then Exit;

	//bmp.LoadFromBitmapIfNeeded;

	lineDelta := bmp.Width;

	yfactor := bmp.Height / newHeight;
	xfactor := bmp.Width / newWidth;
	for yb := 0 to newHeight - 1 do
	begin
		pdest  := Result.Scanline[yb];
		ysrc1  := yb * yfactor;
		ysrc2  := (yb + 1) * yfactor;
		iysrc1 := trunc(ysrc1);
		if (int(ysrc2) = int(ysrc1)) or (ysrc2 = iysrc1 + 1) then
		begin
			iysrc2    := iysrc1;
			factVert1 := 1;
			factVert2 := 0;
		end
		else
		begin
			iysrc2    := trunc(ysrc2);
			factVert1 := 1 - frac(ysrc1);
			factVert2 := frac(ysrc2);
		end;
		if iysrc2 >= bmp.Height then Break;
		for xb := 0 to newWidth - 1 do
		begin
			xsrc1  := xb * xfactor;
			xsrc2  := (xb + 1) * xfactor;
			ixsrc1 := trunc(xsrc1);
			if (int(xsrc2) = int(xsrc1)) or (xsrc2 = ixsrc1 + 1) then
			begin
				ixsrc2     := ixsrc1;
				factHoriz1 := 1;
				factHoriz2 := 0;
			end
			else
			begin
				ixsrc2     := trunc(xsrc2);
				factHoriz1 := 1 - frac(xsrc1);
				factHoriz2 := frac(xsrc2);
			end;

			cUpLeft.ARGB   := bmp.GetPixel(ixsrc1, iysrc1);
			cUpRight.ARGB  := bmp.GetPixel(ixsrc2, iysrc1);
			cLowLeft.ARGB  := bmp.GetPixel(ixsrc1, iysrc2);
			cLowRight.ARGB := bmp.GetPixel(ixsrc2, iysrc2);

			fUpLeft   := factHoriz1 * factVert1;
			fUpRight  := factHoriz2 * factVert1;
			fLowLeft  := factHoriz1 * factVert2;
			fLowRight := factHoriz2 * factVert2;

			faUpLeft   := fUpLeft   * cUpLeft.A;
			faUpRight  := fUpRight  * cUpRight.A;
			faLowLeft  := fLowLeft  * cLowLeft.A;
			faLowRight := fLowRight * cLowRight.A;

			Sum  := fUpLeft + fUpRight + fLowLeft + fLowRight;
			rSum := cUpLeft.R * faUpLeft + cUpRight.R * faUpRight +
				cLowLeft.R * faLowLeft + cLowRight.R * faLowRight;
			gSum := cUpLeft.G * faUpLeft + cUpRight.G * faUpRight +
				cLowLeft.G * faLowLeft + cLowRight.G * faLowRight;
			bSum := cUpLeft.B * faUpLeft + cUpRight.B * faUpRight +
				cLowLeft.B * faLowLeft + cLowRight.B * faLowRight;
			aSum := cUpLeft.A * fUpLeft + cUpRight.A * fUpRight +
				cLowLeft.A * fLowLeft + cLowRight.A * fLowRight;

			ixsrc1p1 := ixsrc1 + 1;
			ixsrc2m1 := ixsrc2 - 1;
			iysrc1p1 := iysrc1 + 1;
			iysrc2m1 := iysrc2 - 1;

			if ixsrc2m1 >= ixsrc1p1 then
			begin
				psrc := bmp.scanline[iysrc1] + ixsrc1p1;
				for xb2 := ixsrc1p1 to ixsrc2m1 do
				begin
					cBorder.ARGB := psrc^;
					Inc(psrc);
					IncF(rSum, cBorder.R * cBorder.A * factVert1);
					IncF(gSum, cBorder.G * cBorder.A * factVert1);
					IncF(bSum, cBorder.B * cBorder.A * factVert1);
					IncF(aSum, cBorder.A * factVert1);
					IncF(Sum, factVert1);
				end;

				if (factVert2 <> 0) and (iysrc2 < bmp.Height) then
				begin
					psrc := bmp.scanline[iysrc2] + ixsrc1p1;
					for xb2 := ixsrc1p1 to ixsrc2m1 do
					begin
						cBorder.ARGB := psrc^;
						Inc(psrc);
						IncF(rSum, cBorder.R * cBorder.A * factVert2);
						IncF(gSum, cBorder.G * cBorder.A * factVert2);
						IncF(bSum, cBorder.B * cBorder.A * factVert2);
						IncF(aSum, cBorder.A * factVert2);
						IncF(Sum, factVert2);
					end;
				end;
			end;

			if iysrc2m1 >= iysrc1p1 then
			begin
				psrc := bmp.scanline[iysrc1p1] + ixsrc1;
				for yb2 := iysrc1p1 to iysrc2m1 do
				begin
					cBorder.ARGB := psrc^;
					Inc(psrc, lineDelta);
					IncF(rSum, cBorder.R * cBorder.A * factHoriz1);
					IncF(gSum, cBorder.G * cBorder.A * factHoriz1);
					IncF(bSum, cBorder.B * cBorder.A * factHoriz1);
					IncF(aSum, cBorder.A * factHoriz1);
					IncF(Sum, factHoriz1);
				end;

				if (factHoriz2 <> 0) and (ixsrc2 < bmp.Width) then
				begin
					psrc := bmp.scanline[iysrc1p1] + ixsrc2;
					for yb2 := iysrc1p1 to iysrc2m1 do
					begin
						cBorder.ARGB := psrc^;
						Inc(psrc, lineDelta);
						IncF(rSum, cBorder.R * cBorder.A * factHoriz2);
						IncF(gSum, cBorder.G * cBorder.A * factHoriz2);
						IncF(bSum, cBorder.B * cBorder.A * factHoriz2);
						IncF(aSum, cBorder.A * factHoriz2);
						IncF(Sum, factHoriz2);
					end;
				end;
			end;

			if (ixsrc2m1 >= ixsrc1p1) and (iysrc2m1 >= iysrc1p1) then
			begin
				delta := lineDelta - (ixsrc2m1 - ixsrc1p1 + 1);
				psrc  := bmp.scanline[iysrc1p1] + ixsrc1p1;
				for yb2 := iysrc1p1 to iysrc2m1 do
				begin
					for xb2 := ixsrc1p1 to ixsrc2m1 do
					begin
						cFull.ARGB := psrc^;
						IncF(rSum, cFull.R * cFull.A);
						IncF(gSum, cFull.G * cFull.A);
						IncF(bSum, cFull.B * cFull.A);
						IncF(aSum, cFull.A);
						IncF(Sum, 1);
						Inc(psrc);
					end;
					Inc(psrc, delta);
				end;
			end;

			if aSum = 0 then
				pdest^ := 0 // BGRAPixelTransparent
			else
				pdest^ := Color32(Round(rSum / aSum), Round(gSum / aSum),
					Round(bSum / aSum), Round(aSum / Sum));
			Inc(pdest);
		end;
	end;
end;

function FineResample(bmp: TBitmap32; NewWidth, NewHeight: Integer;
	ResampleFilter: TResampleFilter): TBitmap32;
var
	temp, newtemp: TBitmap32;
	tempFilter1, tempFilter2: TWideKernelFilter;
begin
	if (NewWidth = bmp.Width) and (NewHeight = bmp.Height) then
	begin
		Result := bmp.Clone;
		Exit;
	end;

	case ResampleFilter of

		rfBicubic:
		begin
			tempFilter1 := TCubicKernel.Create;
			result := WideKernelResample(bmp,NewWidth,NewHeight,tempFilter1,tempFilter1);
			tempFilter1.Free;
		end;

		rfMitchell:
		begin
			tempFilter1 := TMitchellKernel.Create;
			result := WideKernelResample(bmp,NewWidth,NewHeight,tempFilter1,tempFilter1);
			tempFilter1.Free;
		end;

		rfSpline:
		begin
			tempFilter1 := TSplineKernel.Create;
			result := WideKernelResample(bmp,NewWidth,NewHeight,tempFilter1,tempFilter1);
			tempFilter1.Free;
		end;

		rfLanczos2, rfLanczos3, rfLanczos4:
		begin
			tempFilter1 := TLanczosKernel.Create(ord(ResampleFilter)-ord(rfLanczos2)+2);
			result := WideKernelResample(bmp,NewWidth,NewHeight,tempFilter1,tempFilter1);
			tempFilter1.Free;
		end;

		rfBestQuality:
		begin
			tempFilter1 := TSplineKernel.Create;
			tempFilter2 := TMitchellKernel.Create;
			result := WideKernelResample(bmp,NewWidth,NewHeight,tempFilter2,tempFilter1);
			tempFilter1.Free;
			tempFilter2.Free;
		end;

	else

		if (NewWidth >= bmp.Width) and (NewHeight >= bmp.Height) then
			Result := FineResampleLarger(bmp, NewWidth, NewHeight, ResampleFilter)
		else
		if (NewWidth <= bmp.Width) and (NewHeight <= bmp.Height) then
			Result := FineResampleSmaller(bmp, NewWidth, NewHeight)
		else
		begin
			temp := bmp;

			if NewWidth < bmp.Width then
			begin
				newtemp := FineResampleSmaller(temp, NewWidth, temp.Height);
				if (temp <> bmp) then
					temp.Free;
				temp := newtemp;
			end;

			if NewHeight < bmp.Height then
			begin
				newtemp := FineResampleSmaller(temp, temp.Width, NewHeight);
				if (temp <> bmp) then
					temp.Free;
				temp := newtemp;
			end;

			if NewWidth > bmp.Width then
			begin
				newtemp := FineResampleLarger(temp, NewWidth, temp.Height, ResampleFilter);
				if (temp <> bmp) then
					temp.Free;
				temp := newtemp;
			end;

			if NewHeight > bmp.Height then
			begin
				newtemp := FineResampleLarger(temp, temp.Width, NewHeight, ResampleFilter);
				if (temp <> bmp) then
					temp.Free;
				temp := newtemp;
			end;

			if temp <> bmp then
				Result := temp
			else
				Result := bmp.Clone;
		end;

	end; // case
end;

// ------------------------------------------------------------------------------------------------
// TCubicKernel

function TCubicKernel.Pow3(x: Single): Single;
begin
	if x <= 0.0 then
		Result := 0.0
	else
		Result := x * x * x;
end;

function TCubicKernel.ShouldCheckRange: boolean;
begin
	Result := False;
end;

function TCubicKernel.KernelWidth: single;
begin
	Result := 2;
end;

function TCubicKernel.Interpolation(t: Single): Single;
const
	Globalfactor = 1/6;
begin
	if t > 2 then
		Result := 0
	else
		Result := Globalfactor * (Pow3(t+2)-4 * Pow3(t+1)+6 * Pow3(t)-4 * Pow3(t-1));
end;

// ------------------------------------------------------------------------------------------------
// TMitchellKernel

function TMitchellKernel.Interpolation(t: Single): Single;
const
	OneEighteenth = 1 / 18;
var
	tt, ttt: Single;
begin
	t   := Abs(t);
	tt  := Sqr(t);
	ttt := tt * t;
	if t < 1 then
		Result := (21 * ttt - 36 * tt + 16 ) * OneEighteenth
	else
	if t < 2 then
		Result := (- 7 * ttt + 36 * tt - 60 * t + 32) * OneEighteenth
	else
		Result := 0;
end;

function TMitchellKernel.ShouldCheckRange: Boolean;
begin
	Result := True;
end;

function TMitchellKernel.KernelWidth: single;
begin
	Result := 2;
end;

// ------------------------------------------------------------------------------------------------
// TSplineKernel

constructor TSplineKernel.Create;
begin
	coeff := 0.5;
end;

constructor TSplineKernel.Create(ACoeff: Single);
begin
	Coeff := ACoeff;
end;

function TSplineKernel.ShouldCheckRange: Boolean;
begin
	Result := True;
end;

function TSplineKernel.KernelWidth: single;
begin
	Result := 2;
end;

function TSplineKernel.Interpolation(t: Single): Single;
var
	tt, ttt: Single;
begin
	t   := Abs(t);
	tt  := Sqr(t);
	ttt := tt * t;
	if t < 1 then
		Result := (2 - Coeff) * ttt - (3 - Coeff) * tt + 1
	else
	if t < 2 then
		Result := -Coeff * (ttt - 5 * tt + 8 * t - 4)
	else
		Result := 0;
end;

// ------------------------------------------------------------------------------------------------
// TLanczosKernel by stab

constructor TLanczosKernel.Create(ANumberOfLobes: Integer);
begin
	NumberOfLobes := ANumberOfLobes;
end;

function TLanczosKernel.ShouldCheckRange: Boolean;
begin
	Result := True;
end;

function TLanczosKernel.KernelWidth: Single;
begin
	Result := FNumberOfLobes;
end;

procedure TLanczosKernel.SetNumberOfLobes(AValue: Integer);
begin
	if AValue < 1 then AValue := 1;
	if FNumberOfLobes <> AValue then
	begin
		FNumberOfLobes := AValue;
		if AValue = 1 then FFactor := 1.5 else FFactor := AValue;
	end;
end;

function TLanczosKernel.Interpolation(t: Single): Single;
var
	Pi_t: ValReal;
begin
	if t = 0 then
		Result := 1
	else
	if t < FNumberOfLobes then
	begin
		Pi_t := Pi * t;
		Result := FFactor * Sin(Pi_t) * Sin(Pi_t / FNumberOfLobes) / (Pi_t * Pi_t);
	end
	else
		Result := 0;
end;

end.

