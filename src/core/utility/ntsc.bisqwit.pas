// FreePascal port of Bisqwit's NTSC routines

unit NTSC.Bisqwit;

{$MODE DELPHI}
{$RANGECHECKS OFF}

{.$DEFINE EMULATE_NTSC}

interface

uses
	Classes, SysUtils, Math;

	function MakeRGBcolor(pixel: Word;
		saturation: Single = 1.0;
		hue_tweak:  Single = 0.0;
		contrast:   Single = 1.0;
		brightness: Single = 1.0;
		gamma:      Single = 1.8): Cardinal;

	{$IFDEF EMULATE_NTSC}
	function  YIQtoRGB(y, i, q: Single): Cardinal; inline;
	function  NTSCsignal(pixel, phase: Word): Single;
	procedure RenderNTSCpixel(x, y, pixel, PPU_cycle_counter: Word);
	procedure RenderNTSCScanline(phase: Single);
	procedure NTSC_FlushScanline(py: Word);
	procedure NTSC_DecodeLine(Width: Word; Phase0: Integer;
	          var Signal: array of ShortInt; var Target: array of Cardinal);
	procedure NTSC_PutPixel(px, py: Word; pixel: Byte);

var
	// Eight signal levels for each pixel, normalized to 0..1 range.
	NTSC_SignalLevels:   array[0..WIDTH_SCANLINE*8] of Single;
	NTSC_ScanlinePixels: array[0..WIDTH_SCANLINE*{XScale}8]   of Cardinal;
	SinTab, CosTab:      array[0..11] of Single;
	yiqmap: array[0..256-1, 0..240-1] of array[0..11] of Single;
	Signals: array[0..WIDTH_SCANLINE*8] of ShortInt;
	{$ENDIF}


implementation

{$IFDEF EMULATE_NTSC}

const
	Xscale = 4;
	Yscale = 3;
	Gamma = 1.8;
	WIDTH_SCANLINE = 256; // up to 2048

{$ENDIF}


// from Bisqwit, https://forums.nesdev.com/viewtopic.php?p=85060
//
function MakeRGBcolor;
const
	// Voltage levels, relative to synch voltage
	black       = 0.518;
	white       = 1.962;
	attenuation = 0.746;
	levels: array[0..7] of Single = (
		0.350, 0.518, 0.962, 1.550,   // Signal low
		1.094, 1.506, 1.962, 1.962 ); // Signal high

	function Wave(phase, color: Integer): Boolean; inline;
	begin
		Result := ((color + 8 + phase) mod 12) < 6;
	end;

	function GammaFix(f: Single): Single; inline;
	begin
		if f <= 0.0 then
			Result := 0
		else
			Result := Power(f, 2.2 / gamma);
	end;

	function Clamp(v: Integer): Integer; inline;
	begin
		if v > 255 then Result := 255 else
		if v < 0   then Result := 0   else
			Result := v;
	end;

var
	p, color, level: Integer;
	spot, v, y, i, q: Single;
	lo_and_hi: array[0..1] of Single;
begin
	// The input value is a NES color index (with de-emphasis bits).
	// We need RGB values. Convert the index into RGB.
	// http://wiki.nesdev.com/w/index.php/NTSC_video

	// Decode the color index
	color := pixel and $F;
	level := IfThen(color < $E, (pixel shr 4) and 3, 1);

	lo_and_hi[0] := levels[level + 4 * IfThen(color = $0, 1, 0)];
	lo_and_hi[1] := levels[level + 4 * IfThen(color < $D, 1, 0)];

	// Calculate the luma and chroma by emulating the relevant circuits:
	y := 0; i := 0; q := 0;

	for p := 0 to 11 do // 12 clock cycles per pixel
	begin
		// NES NTSC modulator (square wave between two voltage levels):
		spot := lo_and_hi[Byte(Wave(p, color))];

		// De-emphasis bits attenuate a part of the signal:
		if (
		    ( ((pixel and  $40) <> 0) and (Wave(p,12)) )
		or  ( ((pixel and  $80) <> 0) and (Wave(p, 4)) )
		or  ( ((pixel and $100) <> 0) and (Wave(p, 8)) )
		) then spot *= attenuation;

		// Normalize:
		v := (spot - black) / (white - black);

		// Ideal TV NTSC demodulator:
		// Apply contrast/brightness
		v := (v - 0.5) * contrast + 0.5;
		v *= brightness / 12;

		y += v;
		i += v * Cos((PI / 6) * (p + hue_tweak));
		q += v * Sin((PI / 6) * (p + hue_tweak));
	end;

	i *= saturation;
	q *= saturation;

	// Convert YIQ into RGB according to FCC-sanctioned conversion matrix.
	Result := $FF000000 or
	  ($10000 * Clamp(Trunc(255 * GammaFix(y +  0.946882*i +  0.623557*q))))
	+ ($00100 * Clamp(Trunc(255 * GammaFix(y + -0.274788*i + -0.635691*q))))
	+ ($00001 * Clamp(Trunc(255 * GammaFix(y + -1.108545*i +  1.709007*q))));
end;


{$IFDEF EMULATE_NTSC}

procedure InitSinCosTabs;
var
	i: Integer;
begin
	for i := 0 to 11 do
	begin
		SinTab[i] := Cos(Pi * ((i + 9) mod 12) / 6) * 1.5;
		CosTab[i] := Cos(Pi * (i) / 6) * 1.5;
	end;

	//cos[12] = { c(0),c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9),c(10),c(11) };
	//sin[12] = { c(9),c(10),c(11),c(0),c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8) };
end;



// pixel = Pixel color (9-bit) given as input. Bitmask format: "eeellcccc".
// phase = Signal phase (0..11). It is a variable that increases by 8 each pixel.
function NTSCsignal(pixel, phase: Word): Single;

	function InColorPhase(color: Integer): Boolean; inline;
	begin
		Result := ((color + phase) mod 12) < 6;
	end;

const
    // Voltage levels, relative to synch voltage
	attenuation = 0.746;

	levels: array[0..7] of Single = (
		0.350, 0.518, 0.962, 1.550,   // Signal low
		1.094, 1.506, 1.962, 1.962 ); // Signal high

var
	color, level, emphasis: Integer;
	lo, hi: Single;
begin
	// Decode the NES color.
	color := (pixel and $0F);      // 0..15 "cccc"
	level := (pixel shr 4) and 3;  // 0..3  "ll"
	emphasis := (pixel shr 6);     // 0..7  "eee"
	if color > 13 then level := 1; // For colors 14..15, level 1 is forced.

	// The square wave for this color alternates between these two voltages:
	lo := levels[level + 0];
	hi := levels[level + 4];
	if color = 0 then
		lo := hi // For color 0, only high level is emitted
	else
	if color > 12 then
		hi := lo; // For colors 13..15, only low level is emitted

	// Generate the square wave
	Result := IfThen(InColorPhase(color), hi, lo);

    // When de-emphasis bits are set, some parts of the signal are attenuated:
	if( ((emphasis and 1 <> 0) and InColorPhase(0))
	or  ((emphasis and 2 <> 0) and InColorPhase(4))
	or  ((emphasis and 4 <> 0) and InColorPhase(8)) ) then
		Result := Result * attenuation;
end;


procedure RenderNTSCpixel(x, y, pixel, PPU_cycle_counter: Word);
const
	black = 0.518;
	white = 1.962;
	bwdiv = white - black;
var
	p, phase: Integer;
	signal: Single;
begin
	phase := (PPU_cycle_counter * 8);
	for p := 0 to 7 do // Each pixel produces distinct 8 samples of NTSC signal.
	begin
		signal := NTSCsignal(pixel, phase + p); // Calculated as above
		// Optionally apply some lowpass-filtering to the signal here.
		signal := (signal - black) / bwdiv;
		NTSC_SignalLevels[x*8 + p] := signal;
		signals[x*8+p] := floor((signal-0.518)*1000/12)-15;
//		yiqmap[x,y][p] := (signal - black) / bwdiv / 12;
	end;
end;

function YIQtoRGB(y, i, q: Single): Cardinal;

	function GammaFix(f: Single): Single; inline;
	const
		gamma = 2.0; // Assumed display gamma
	begin
		if f <= 0.0 then
			Result := 0
		else
			Result := Power(f, 2.2 / gamma);
	end;

	function Clamp(v: Integer): Integer; inline;
	begin
		if v > 255 then Result := 255 else
		//if v < 0 then Result := 0 else
		Result := v;
	end;

begin

	Result :=
	  ($10000 * Clamp(Trunc(255.95 * GammaFix(y +  0.946882*i +  0.623557*q))))
	+ ($00100 * Clamp(Trunc(255.95 * GammaFix(y + -0.274788*i + -0.635691*q))))
	+ ($00001 * Clamp(Trunc(255.95 * GammaFix(y + -1.108545*i +  1.709007*q))));
{
	Result :=
	   (Clamp(Trunc(255.95 * GammaFix(y +  0.946882*i +  0.623557*q))) shl 16)
	or (Clamp(Trunc(255.95 * GammaFix(y + -0.274788*i + -0.635691*q))) shl 8)
	or (Clamp(Trunc(255.95 * GammaFix(y + -1.108545*i +  1.709007*q))) );
}
end;

// Phase: This should the value that was PPU_cycle_counter * 8 + 3.9
// at the BEGINNING of this scanline. It should be modulo 12.
// It can additionally include a floating-point hue offset.
//
procedure RenderNTSCScanline(phase: Single);
const
	Pi2 = Pi*2;
var
	x, p, center, b, e: Integer;
	a, y, i, q, level: Single;
begin
	center := 0;

	for x := 0 to WIDTH_SCANLINE-1 do
	begin
        // Determine the region of scanline signal to sample. Take 12 samples.
		b := center - 6; if b < 0       then b := 0;
		e := center + 6; if e > (256*8) then e := 256*8;
		y := 0; i := 0; q := 0; // Calculate the color in YIQ.

		for p := b to e-1 do // Collect and accumulate samples
		begin
			level := NTSC_SignalLevels[p] / 12;
			a := (Pi * (phase + p) / 6);
			y += level;
			i += level * Cos(a);
			q += level * Sin(a);
		end;

		NTSC_ScanlinePixels[x] := YIQtoRGB(y, i, q); // Send the YIQ color for rendering.
		center += 8;
	end;
end;





procedure NTSC_PutPixel(px, py: Word; pixel: Byte);
const
	// Voltage levels, relative to synch voltage
	black = 0.518;
	white = 1.962;
	attenuation = 0.746;
	bwdiv = white - black;
	levels: array[0..7] of Single = (
		0.350, 0.518, 0.962, 1.550,   // Signal low
		1.094, 1.506, 1.962, 1.962 ); // Signal high

	function Wave(phase, color: Integer): Boolean; inline;
	begin
		Result := ((color + 8 + phase) mod 12) < 6;
	end;

var
	p, color, level, emphasis: Integer;
	spot, lo, hi: Single;
begin
	// Decode the NES color.
	color := (pixel and $0F);      // 0..15 "cccc"
	level := (pixel shr 4) and 3;  // 0..3  "ll"
	if color > 13 then level := 1; // For colors 14..15, level 1 is forced.
	//emphasis := (pixel shr 6);     // 0..7  "eee"

	// The square wave for this color alternates between these two voltages:
	lo := levels[level + 0];
	hi := levels[level + 4];
	if color = 0 then
		lo := hi // For color 0, only high level is emitted
	else
	if color > 12 then
		hi := lo; // For colors 13..15, only low level is emitted


{
	for p := 0 to 11 do // 12 clock cycles per pixel
	begin
		// NES NTSC modulator (square wave between two voltage levels)
		spot := levels[level + 4*(color <= 12 * Wave(p,color))];
		// De-emphasis bits attenuate a part of the signal

		if (((pixel and  $40) and Wave(p,12))
		or  ((pixel and  $80) and Wave(p, 4))
		or  ((pixel and $100) and Wave(p, 8))) then spot *= attenuation;
		// Normalize:
		yiqmap[px,py][p] := (spot - black) / (bwdiv) / 12;

{
		// Generate the square wave
		if InColorPhase(color) then
			Result := hi
		else
			Result := lo;

	    // When de-emphasis bits are set, some parts of the signal are attenuated:
		if( ((emphasis and 1 <> 0) and InColorPhase(0))
		or  ((emphasis and 2 <> 0) and InColorPhase(4))
		or  ((emphasis and 4 <> 0) and InColorPhase(8)) ) then
			Result := Result * attenuation;

	end;
}
end;


procedure NTSC_FlushScanline(py: Word);
var
	px, x, p, r, o: Integer;
	v, level07, level15: Single;
	cache: array[0..256*12-1] of Single;
	yiq: array[0..2] of Single;
begin
	level07 := 0; level15 := 0;
	o := 0;

	for px := 0 to 255 do
	begin
		for p := 0 to 11 do
		begin
			level07 := level07 *  0.3 + 0.7 * (yiqmap[px,py][p] - 0.5);
			level15 := level15 * -0.5 + 1.5 * (yiqmap[px,py][p] - 0.5);
			cache[o] := 0.5 + (level07 * 0.7 + level15 * 0.3);
			Inc(o);
		end;
	end;

	for px := 0 to 255 do
	begin
		for r := 0 to Xscale-1 do
		begin
			yiq[0] := 0; yiq[1] := 0; yiq[2] := 0;

			x := Trunc(px*12 + ((r+1-Xscale)*12 / Xscale));

			for p := 0 to 11 do
			begin
				if (x >= 0) and (x < 256*12) then
				begin
					v := cache[x];
					// Simulate ideal TV NTSC decoder:
					yiq[0] += v;
					yiq[1] += v * CosTab[x mod 12] * 1.5;
					yiq[2] += v * SinTab[x mod 12] * 1.5;
				end;
				Inc(x);
			end;

			// Convert YIQ into RGB according to FCC sanctioned matrix.
			//for p := 0 to Yscale-1 do
			//	pix[ (py*Yscale+p) * (256*Xscale) + px*Xscale + r] = rgb;

			NTSC_ScanlinePixels[px*Xscale+r] := YIQtoRGB(yiq[0], yiq[1], yiq[2]);

			//NTSC_ScanlinePixels[x] := YIQtoRGB(y, i, q); // Send the YIQ color for rendering.

		end;
	end;
end;




(**
 * NTSC_DecodeLine(Width, Signal, Target, Phase0)
 *
 * Convert NES NTSC graphics signal into RGB using integer arithmetics only.
 *
 * Width: Number of NTSC signal samples.
 *        For a 256 pixels wide screen, this would be 256*8. 283*8 if you include borders.
 *
 * Signal: An array of Width samples.
 *         The following sample values are recognized:
 *          -29 = Luma 0 low   32 = Luma 0 high (-38 and  6 when attenuated)
 *          -15 = Luma 1 low   66 = Luma 1 high (-28 and 31 when attenuated)
 *           22 = Luma 2 low  105 = Luma 2 high ( -1 and 58 when attenuated)
 *           71 = Luma 3 low  105 = Luma 3 high ( 34 and 58 when attenuated)
 *         In this scale, sync signal would be -59 and colorburst would be -40 and 19,
 *         but these are not interpreted specially in this function.
 *         The value is calculated from the relative voltage with:
 *                   floor((voltage-0.518)*1000/12)-15
 *
 * Target: Pointer to a storage for Width RGB32 samples (00rrggbb).
 *         Note that the function will produce a RGB32 value for _every_ half-clock-cycle.
 *         This means 2264 RGB samples if you render 283 pixels per scanline (incl. borders).
 *         The caller can pick and choose those columns they want from the signal
 *         to render the picture at their desired resolution.
 *
 * Phase0: An integer in range 0-11 that describes the phase offset into colors on this scanline.
 *         Would be generated from the PPU clock cycle counter at the start of the scanline.
 *         In essence it conveys in one integer the same information that real NTSC signal
 *         would convey in the colorburst period in the beginning of each scanline.
 *)

procedure NTSC_DecodeLine(Width: Word; Phase0: Integer;
	var Signal: array of ShortInt; var Target: array of Cardinal);
const
	(* Ywidth, Iwidth and Qwidth are the filter widths for Y,I,Q respectively.
	 * All widths at 12 produce the best signal quality.
	 * 12,24,24 would be the closest values matching the NTSC spec.
	 * But off-spec values 12,22,26 are used here, to bring forth mild
	 * "chroma dots", an artifacting common with badly tuned TVs.
	 * Larger values = more horizontal blurring.
	 *)
	Ywidth = 12;
	Iwidth = 23;
	Qwidth = 23;

	Contrast   = 167941;
	Saturation = 144044;

	yr = Contrast/Ywidth;
	ir = Contrast* 1.994681e-6*Saturation/Iwidth;
	qr = Contrast* 9.915742e-7*Saturation/Qwidth;

	yg = Contrast/Ywidth;
	ig = Contrast* 9.151351e-8*Saturation/Iwidth;
	qg = Contrast*-6.334805e-7*Saturation/Qwidth;

	yb = Contrast/Ywidth;
	ib = Contrast*-1.012984e-6*Saturation/Iwidth;
	qb = Contrast* 1.667217e-6*Saturation/Qwidth;

    // To finetune hue, you would have to recalculate sinetable[].
    // Coarse changes can be made with Phase0.
	sinetable: array[0..26] of ShortInt = ( 0,4,7,8,7,4, 0,-4,-7,-8,-7,-4,
                                            0,4,7,8,7,4, 0,-4,-7,-8,-7,-4,
                                            0,4,7 ); // 8*sin(x*2pi/12)

	function Read(pos: Integer): ShortInt; inline;
	begin
		if pos >= 0 then
			Result := Signal[pos]
		else
			Result := 0;
	end;

	function _Cos(pos: Integer): ShortInt; inline;
	begin
		Result := sinetable[((pos+36) mod 12) + Phase0];
	end;

	function _Sin(pos: Integer): ShortInt; inline;
	begin
		Result := sinetable[((pos+36) mod 12) + Phase0 + 3];
	end;

var
	ysum, isum, qsum,
	s, r, g, b: Integer;
begin
    ysum := 0; isum := 0; qsum := 0;

	for s := 0 to Width-1 do
	begin
		ysum += Read(s)           - Read(s - Ywidth);
		isum += Read(s) * _Cos(s) - Read(s - Iwidth) * _Cos(s - Iwidth);
		qsum += Read(s) * _Sin(s) - Read(s - Qwidth) * _Sin(s - Qwidth);

		r := Min(255, Max(0, Trunc((ysum*yr + isum*ir + qsum*qr) / 65536 )));
		g := Min(255, Max(0, Trunc((ysum*yg + isum*ig + qsum*qg) / 65536 )));
		b := Min(255, Max(0, Trunc((ysum*yb + isum*ib + qsum*qb) / 65536 )));

		Target[s] := $FF000000 or ( (r shl 16) or (g shl 8) or b );
	end;
end;


initialization

	InitSinCosTabs;

{$ENDIF}

end.

