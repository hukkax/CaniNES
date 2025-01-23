unit NES.NTSC;

(* nes_ntsc 0.2.2. http://www.slack.net/~ant/ */

 FreePascal port by Joel Toivonen (hukka) 2021, http://hukka.ncn.fi
 with copious amounts of help from muzzy

 2021-01-06: Initial release

 Copyright (C) 2006-2007 Shay Green. This module is free software; you
 can redistribute it and/or modify it under the terms of the GNU Lesser
 General Public License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version. This
 module is distributed in the hope that it will be useful, but WITHOUT ANY
 WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 details. You should have received a copy of the GNU Lesser General Public
 License along with this module; if not, write to the Free Software Foundation,
 Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)

{$MODE DELPHI}
{$MACRO ON}
{$POINTERMATH ON}

// Enable emphasis support and use a 512 color palette instead of the base 64 color palette
{$DEFINE NES_NTSC_EMPHASIS}

{.$DEFINE DISABLE_CORRECTION}

{.$DEFINE DEBUG_VERIFY_TABLE}

{.$DEFINE DISABLE_RESCALE}


interface

uses
	Classes, SysUtils;

type
	// Type of input pixel values
	{$IFDEF NES_NTSC_EMPHASIS}
	TInputPixel = Word; // nes_ntsc_in_t
	{$ELSE}
	TInputPixel = Byte; // nes_ntsc_in_t
	{$ENDIF}
	PInputPixel = ^TInputPixel;

	TOutputPixel = Cardinal; // nes_ntsc_out_t
	POutputPixel = ^TOutputPixel;

	TRGB = Cardinal;
	PRGB = ^TRGB;
	PRGBArray = array of TRGB;

	Float = Single;
	PFloat = PSingle;

	TPixelInfoKernelArray = array[0..3] of Float;

	TPixelInfo = record // pixel_info_t
		offset: Integer;
		negate: Float;
		kernel: TPixelInfoKernelArray;
	end;
	PPixelInfo = ^TPixelInfo;

const
	{$IFDEF NES_NTSC_EMPHASIS}
	nes_ntsc_palette_size = 64 * 8;
	{$ELSE}
	nes_ntsc_palette_size = 64;
	{$ENDIF}

	nes_ntsc_in_chunk  = 3;   // number of input pixels read per chunk
	nes_ntsc_out_chunk = 7;   // number of output pixels generated per chunk
	nes_ntsc_black     = 15;  // palette index for black

	nes_ntsc_rgb_builder = $200802;   // (1shl21) | (1shl11) | (1shl1) => $200000|$800|2
	nes_ntsc_clamp_mask  = $300C03;   // nes_ntsc_rgb_builder * 3 / 2
	nes_ntsc_clamp_add   = $20280A02; // nes_ntsc_rgb_builder * $101

	nes_ntsc_min_in_width       = 256;
	nes_ntsc_640_in_width       = 271;
	nes_ntsc_640_overscan_left  = 8;
	nes_ntsc_full_in_width      = 283;
	nes_ntsc_full_overscan_left = 16;

	nes_ntsc_entry_size         = 128;

	burst_count     = 3; // burst phase cycles through 0, 1, and 2
	burst_size      = nes_ntsc_entry_size div burst_count;

	alignment_count = 3;

	rescale_in      = 8;
	{$IFDEF DISABLE_RESCALE}
	rescale_out     = 1;
	{$ELSE}
	rescale_out     = 7;
	{$ENDIF}

	kernel_half     = 16;
	kernel_size     = kernel_half * 2 + 1;

	artifacts_mid   = 1.0;
	artifacts_max   = artifacts_mid * 1.5;
	fringing_mid    = 1.0;
	fringing_max    = fringing_mid * 2;
	gamma_size      = 1;
	std_decoder_hue = -15;
	ext_decoder_hue = std_decoder_hue + 15;
	LUMA_CUTOFF     = 0.20;

	rgb_bits        = 8;
	rgb_unit        = 1 shl rgb_bits;
	rgb_offset      = rgb_unit * 2 + 0.5;
	rgb_bias        : Cardinal = rgb_unit * 2 * nes_ntsc_rgb_builder;
	rgb_kernel_size = burst_size div alignment_count;


type
	TNtscInit = record
		contrast,
		brightness,
		artifacts,
		fringing:  Float;
		to_rgb:    array [0..burst_count * 6] of Float;
		to_float:  array [0..gamma_size]      of Float;
		kernel:    array [0..rescale_out * kernel_size * 2] of Float;
	end;

	// Image parameters, ranging from -1.0 to 1.0. Actual internal values shown
	// in parenthesis and should remain fairly stable in future versions.
	TNtscSetup = record
		// Basic parameters
		hue,                     // -1 = -180 degrees     +1 = +180 degrees
		saturation,              // -1 = grayscale (0.0)  +1 = oversaturated colors (2.0)
		contrast,                // -1 = dark (0.5)       +1 = light (1.5)
		brightness,              // -1 = dark (0.5)       +1 = light (1.5)
		sharpness:      Double;   // edge contrast enhancement/blurring

		// Advanced parameters
		gamma,                   // -1 = dark (1.5)       +1 = light (0.5)
		resolution,              // image resolution
		artifacts,               // artifacts caused by color changes
		fringing,                // color artifacts caused by brightness changes
		bleed:          Double;  // color bleed (color resolution reduction)
		merge_fields:   Boolean; // merge even and odd fields together to reduce flicker?
		decoder_matrix: PFloat;  // optional RGB decoder matrix, 6 elements

		// You can replace the standard NES color generation with an RGB palette. The
		// first replaces all color generation, while the second replaces only the core
		// 64-color generation and does standard color emphasis calculations on it.
		palette,                 // optional 512-entry RGB palette in, 3 bytes per color
		base_palette:   PByte;   // optional  64-entry RGB palette in, 3 bytes per color
	end;

	TNesPalette = array [0..nes_ntsc_palette_size] of TRGB;

	TNtscFilter = class
	private
		table:   array [0..nes_ntsc_palette_size * nes_ntsc_entry_size] of TRGB;
		palette: TNesPalette;
		impl:    TNtscInit;

		procedure DoInit;
		procedure InitFilters;
		procedure Init_nes_ntsc_pixels;
		procedure GenerateKernel(y, i, q: Float; outpixel: PRGB);
		procedure MergeKernelFields(io: PRGB);
		procedure CorrectErrors(color: TRGB; outpixel: PRGB);
	public
		setup: TNtscSetup;

		constructor Create(NtscSetup: TNtscSetup); overload; // nes_ntsc_init
		procedure   Init(NtscSetup: TNtscSetup);

		function  GetPalette: TNesPalette; // returns the generated palette as table of ARGB values

		// Filters one or more rows of pixels. Input pixels are 6/9-bit palette indices.
		// In_row_width is the number of pixels to get to the next input row.
		// Out_pitch is the number of *bytes* to get to the next output row.
		procedure Blit(input: PWord; output: PByte;
			burst_phase, in_width, in_height,
			in_row_width, out_pitch: Integer);
	end;


const
	// Video format presets

	// desaturated + artifacts
	nes_ntsc_monochrome: TNTSCSetup = (
		hue: 0; saturation: -1; contrast: 0; brightness: 0; sharpness: 0.2;
		gamma: 0; resolution: 0.2; artifacts: -0.2; fringing: -0.2; bleed: -1;
		merge_fields: True; decoder_matrix:nil; palette:nil; base_palette:nil);

	// color bleeding + artifacts
	nes_ntsc_composite: TNTSCSetup = (
		hue: 0; saturation: 0; contrast: 0; brightness: 0; sharpness: 0;
		gamma: 0; resolution: 0; artifacts: 0; fringing: 0; bleed: 0;
		merge_fields: True; decoder_matrix:nil; palette:nil; base_palette:nil);

	// color bleeding only
	nes_ntsc_svideo: TNTSCSetup = (
		hue: 0; saturation: 0; contrast: 0; brightness: 0; sharpness: 0.2;
		gamma: 0; resolution: 0.2; artifacts: -1; fringing: -1; bleed: 0;
		merge_fields: True; decoder_matrix:nil; palette:nil; base_palette:nil);

	// crisp image
	nes_ntsc_rgb: TNTSCSetup = (
		hue: 0; saturation: 0; contrast: 0; brightness: 0; sharpness: 1.0;
		gamma: 0; resolution: 1.0; artifacts: -1; fringing: -1; bleed: -1;
		merge_fields: True; decoder_matrix:nil; palette:nil; base_palette:nil);


implementation

uses
	{$IFDEF DEBUG_VERIFY_TABLE}
	NES.NTSC.DLL,
	{$ENDIF}
	Math;

const
	default_decoder: array[0..5] of Float = ( 0.956, 0.621, -0.272, -0.647, -1.105, 1.702 );

var
	nes_ntsc_pixels: array [0..alignment_count-1] of TPixelInfo;

// ================================================================================================
// TNtscFilter
// ================================================================================================

function TNtscFilter.GetPalette: TNesPalette;
begin
	Result := palette;
end;

// Generate pixel at all burst phases and column alignments
procedure TNtscFilter.GenerateKernel(y, i, q: Float; outpixel: PRGB);
var
	op, k, n,
	alignment_remain, burst_remain: Integer;
	ic0, qc1, ic2, qc3, yc0, yc1, yc2, yc3,
	_y, _i, _q,
	t, ii, qq, yy, factor: Float;
	r, g, b: Cardinal;
	pixel: PPixelInfo;
begin
	// generate for each scanline burst phase
	y := y - rgb_offset;
	op := 0;

	// Encode yiq into *two* composite signals (to allow control over artifacting).
	// Convolve these with kernels which: filter respective components, apply
	// sharpening, and rescale horizontally. Convert resulting yiq to rgb and pack
	// into integer. Based on algorithm by NewRisingSun.
	for burst_remain := 1 to burst_count do
	begin
		for alignment_remain := 0 to alignment_count-1 do
		begin
			pixel := @nes_ntsc_pixels[alignment_remain];

			// negate is -1 when composite starts at odd multiple of 2
			yy  := y * impl.fringing * pixel.negate;
			ic0 := (i + yy) * pixel.kernel[0];
			qc1 := (q + yy) * pixel.kernel[1];
			ic2 := (i - yy) * pixel.kernel[2];
			qc3 := (q - yy) * pixel.kernel[3];

			factor := impl.artifacts * pixel.negate;
			ii  := i * factor;
			yc0 := (y + ii) * pixel.kernel[0];
			yc2 := (y - ii) * pixel.kernel[2];

			qq  := q * factor;
			yc1 := (y + qq) * pixel.kernel[1];
			yc3 := (y - qq) * pixel.kernel[3];

			k := pixel.offset;

			for n := 1 to rgb_kernel_size do
			begin
				_i := impl.kernel[k+0]*ic0 + impl.kernel[k+2]*ic2;
				_q := impl.kernel[k+1]*qc1 + impl.kernel[k+3]*qc3;
				_y := impl.kernel[k+kernel_size+0]*yc0 + impl.kernel[k+kernel_size+1]*yc1 +
				      impl.kernel[k+kernel_size+2]*yc2 + impl.kernel[k+kernel_size+3]*yc3 +
				      rgb_offset;

				{$IFDEF DISABLE_RESCALE}
				Dec(k);
				{$ELSE}
				if k < (kernel_size * 2 * (rescale_out - 1)) then
					Inc(k, kernel_size * 2 - 1)
				else
					Dec(k, kernel_size * 2 * (rescale_out - 1) + 2);
				{$ENDIF}

				{$R-} r := Trunc( (_y + impl.to_rgb[op+0] * _i + impl.to_rgb[op+1] * _q) );
				{$R-} g := Trunc( (_y + impl.to_rgb[op+2] * _i + impl.to_rgb[op+3] * _q) );
				{$R-} b := Trunc( (_y + impl.to_rgb[op+4] * _i + impl.to_rgb[op+5] * _q) );

				{$R-} outpixel^ := ((r shl 21) or (g shl 11) or (b shl 1)) - rgb_bias;

				Inc(outpixel);
			end;
		end;

		if burst_count <= 1 then Break;

		Inc(op, 6);

		t := i * -0.5 - q * -0.866025;
		q := i * -0.866025 + q * -0.5;
		i := t;
	end;
end;

procedure TNtscFilter.MergeKernelFields(io: PRGB);
var
	n: Integer;
	p0, p1, p2: TRGB;
begin
	for n := 1 to burst_size do
	begin
		{$Q-} p0 := io[burst_size * 0] + rgb_bias;
		{$Q-} p1 := io[burst_size * 1] + rgb_bias;
		{$Q-} p2 := io[burst_size * 2] + rgb_bias;

		// merge colors without losing precision
		{$Q-} io[burst_size * 0] :=
			((p0 + p1 - ((p0 xor p1) and nes_ntsc_rgb_builder)) shr 1) - rgb_bias;
		{$Q-} io[burst_size * 1] :=
			((p1 + p2 - ((p1 xor p2) and nes_ntsc_rgb_builder)) shr 1) - rgb_bias;
		{$Q-} io[burst_size * 2] :=
			((p2 + p0 - ((p2 xor p0) and nes_ntsc_rgb_builder)) shr 1) - rgb_bias;

		Inc(io);
	end;
end;

procedure TNtscFilter.CorrectErrors(color: TRGB; outpixel: PRGB);
var
	n, i: Integer;
	error, fourth: TRGB;
begin
	for n := 1 to burst_count do
	begin
		for i := 0 to rgb_kernel_size div 2-1 do
		begin
			{$IFNDEF DISABLE_CORRECTION}
				{$R-}error := color -
						outpixel[i  ] - outpixel[(i+12) mod 14 +14] - outpixel[(i+10) mod 14 +28] -
						outpixel[i+7] - outpixel[(i+5)         +14] - outpixel[(i+3)         +28];

				{$Q-}fourth := (error + 2 * nes_ntsc_rgb_builder) shr 2;
				{$Q-}fourth := fourth and ((rgb_bias shr 1) - nes_ntsc_rgb_builder);
				{$R-}Dec(fourth, rgb_bias shr 2);

				{$Q-}Inc(outpixel[i+3+28], fourth);
				{$Q-}Inc(outpixel[i+5+14], fourth);
				{$Q-}Inc(outpixel[i+7],    fourth);
				{$Q-}Inc(outpixel[i], error - fourth * 3);
			{$ELSE}
				{$Q-}Inc(outpixel[i], rgb_bias);
			{$ENDIF}
		end;
		Inc(outpixel, alignment_count * rgb_kernel_size);
	end;
end;

// 3 input pixels -> 8 composite samples
//
procedure TNtscFilter.Init_nes_ntsc_pixels;

	function PIXEL_OFFSET(ntsc, scaled: Integer): Integer;
	begin
		Result := (kernel_size div 2) + ntsc + (IfThen(scaled <> 0, 1, 0)) + (rescale_out - scaled)
			mod rescale_out + (kernel_size * 2 * scaled);
	end;

const
	arr_ntsc:   array[0..alignment_count-1] of Integer = ( -4, -2, 0 );
	arr_scaled: array[0..alignment_count-1] of Integer = ( -9, -7, -5 );
	arr_kernel: array[0..alignment_count-1] of TPixelInfoKernelArray = (
	  ( 1, 1, 0.6667, 0 ),
	  ( 0.3333, 1, 1, 0.3333 ),
	  ( 0, 0.6667, 1, 1 )
	);
var
	i: Integer;
begin
	for i := 0 to alignment_count-1 do
	with nes_ntsc_pixels[i] do
	begin
		if rescale_in > 1 then
			{%H-}offset := PIXEL_OFFSET( (arr_ntsc[i] - arr_scaled[i] div rescale_out * rescale_in),
				((arr_scaled[i] + rescale_out * 10) mod rescale_out) )
		else
			{%H-}offset := kernel_size div 2 + arr_ntsc[i] - arr_scaled[i];
		negate := 1.0 - ((arr_ntsc[i] + 100) mod 2);
		kernel := arr_kernel[i];
	end;
end;

procedure TNtscFilter.InitFilters;
const
	cutoff_factor = -0.03125;
	maxh = 32;
var
	i, x, kernelpos: Integer;
	rolloff, pow_a_n, to_angle, angle,
	fx, blackman,
	cur, m, weight, remain,
	sum, num, den,
	rolloff_cos_a, cutoff: Float;
	fout: PFloat;
	{$IFDEF DISABLE_RESCALE}
	kernels: PFloat;
	{$ELSE}
	kernels: array [0..kernel_size*2] of Float;
	{$ENDIF}
begin
	{$IFDEF DISABLE_RESCALE}
	kernels := @impl.kernel[0];
	{$ENDIF}
	kernelpos := kernel_size * 3 div 2 - kernel_half;

	// generate luma (y) filter using sinc kernel

	// sinc with rolloff (dsf)
	rolloff := 1 + setup.sharpness * 0.032;
	pow_a_n := Power(rolloff, maxh);

	// quadratic mapping to reduce negative (blurring) range
	to_angle := setup.resolution + 1;
	to_angle := Pi / maxh * LUMA_CUTOFF * (to_angle * to_angle + 1);

	kernels[kernel_size * 3 div 2] := maxh; // default center value

	for i := 0 to kernel_half * 2 do
	begin
		x := i - kernel_half;
		angle := x * to_angle;
		// instability occurs at center point with rolloff very close to 1.0
		if (x <> 0) or (pow_a_n > 1.056) or (pow_a_n < 0.981) then
		begin
			rolloff_cos_a := rolloff * Cos(angle);
			num := 1 - rolloff_cos_a -
					pow_a_n * Cos(maxh * angle) +
					pow_a_n * rolloff * Cos((maxh - 1) * angle);
			den := 1 - rolloff_cos_a - rolloff_cos_a + rolloff * rolloff;
			kernels[kernelpos + i] := (num / den) - 0.5;
		end;
	end;

	// apply blackman window and find sum
	sum := 0;
	for i := 0 to kernel_half * 2 do
	begin
		fx := Pi * 2 / (kernel_half * 2) * i;
		blackman := 0.42 - 0.5 * Cos(fx) + 0.08 * Cos(fx * 2);
		x := kernelpos + i;
		kernels[x] := kernels[x] * blackman;
		sum := sum + kernels[x];
	end;

	// normalize kernel
	sum := 1.0 / sum;
	for i := 0 to kernel_half * 2 do
		kernels[kernelpos+i] := kernels[kernelpos+i] * sum;

	// generate chroma (iq) filter using gaussian kernel

	cutoff := setup.bleed;
	if cutoff < 0 then
	begin
		// keep extreme value accessible only near upper end of scale (1.0)
		cutoff := cutoff * cutoff;
		cutoff := cutoff * cutoff;
		cutoff := cutoff * cutoff;
		cutoff := cutoff * (-30.0 / 0.65);
	end;
	cutoff := cutoff_factor - 0.65 * cutoff_factor * cutoff;

	for i := -kernel_half to kernel_half do
		kernels[kernel_size div 2 + i] := Exp(i * i * cutoff);

	// normalize even and odd phases separately
	for i := 0 to 1 do
	begin
		sum := 0;

		x := i;
		repeat
			sum := sum + kernels[x];
			Inc(x, 2);
		until x > kernel_size;

		sum := 1.0 / sum;

		x := i;
		repeat
			kernels[x] := kernels[x] * sum;
			Inc(x, 2);
		until x > kernel_size;
	end;

	{$IFDEF DEBUG_VERIFY_TABLE}
	Log('[luma]');
		for i := kernel_size to kernel_size*2-1 do
			Log(format('%2d. %.6f', [i, kernels[i]]));
	Log('[chroma]');
		for i := 0 to kernel_size-1 do
			Log(format('%2d. %.6f', [i, kernels[i]]));
	{$ENDIF}

	// generate linear rescale kernels
	{$IF rescale_out > 1}
	weight := 1.0;
	fout := @impl.kernel[0];

	for x := 1 to rescale_out do
	begin
		remain := 0;
		weight -= 1.0 / rescale_in;
		for i := 0 to kernel_size * 2 - 1 do
		begin
			cur := kernels[i];
			m := cur * weight;
			fout^ := m + remain;
			Inc(fout);
			remain := cur - m;
		end;
	end;
	{$ENDIF}
end;

constructor TNtscFilter.Create(NtscSetup: TNtscSetup);
begin
	inherited Create;
	Init(NtscSetup);
end;

procedure TNtscFilter.Init(NtscSetup: TNtscSetup);
const
	tints: array [0..7] of Byte = ( 0, 6, 10, 8, 2, 4, 0, 0 );

	lo_levels: array [0..3] of Float = ( -0.12, 0.00, 0.31, 0.72 );
	hi_levels: array [0..3] of Float = (  0.40, 0.68, 1.00, 1.00 );

	phases: array [$00..$12] of Float = ( // phases[i] := Cos(i * PI / 6)
		-1.0, -0.866025, -0.5, 0.0,  0.5,  0.866025,
		 1.0,  0.866025,  0.5, 0.0, -0.5, -0.866025,
		-1.0, -0.866025, -0.5, 0.0,  0.5,  0.866025,
		1.0 );

	to_float = 1.0 / 256;

	atten_mul = 0.79399;
	atten_sub = 0.0782838;
var
	merge_fields: Boolean;
	level, entry, color, tint, tint_color: Integer;
	gamma_factor, gamma,
	sat, y, i, q, lo, hi,
	r, g, b: Float;
	rgb, clamped, sub, clamp: TRGB;
	kernel: PRGB;
	_out: PByte;
	tblen: Cardinal;

	{$IFDEF DEBUG_VERIFY_TABLE}
	table2: array [0..nes_ntsc_palette_size * nes_ntsc_entry_size] of TRGB;
	FS: TFileStream;
	kernel2: PRGB;
	_in: PByte;
	{$ENDIF}

	procedure GetRGB(inptr: PByte);
	begin
		r := to_float * inptr[0];
		g := to_float * inptr[1];
		b := to_float * inptr[2];
	end;

	procedure GetYIQ;
	begin
		y := (r * 0.299) + (g * 0.587) + (b * 0.114);
		i := (r * 0.596) - (g * 0.275) - (b * 0.321);
		q := (r * 0.212) - (g * 0.523) + (b * 0.311);
	end;

begin
	tblen := nes_ntsc_palette_size * nes_ntsc_entry_size * SizeOf(TRGB);
	FillByte(table[0], tblen, 0);

	setup := NtscSetup;
	impl := Default(TNtscInit);

	DoInit;

	// setup fast gamma
	gamma := setup.gamma * -0.5;
	if (setup.base_palette = nil) or (setup.palette = nil) then
		gamma := gamma + 0.1333;
	gamma_factor := Power(Abs(gamma), 0.73);
	if gamma < 0 then
		gamma_factor := -gamma_factor;

	merge_fields := setup.merge_fields;
	if (setup.artifacts <= -1) and (setup.fringing <= -1) then
		merge_fields := True;

	// Base 64-color generation
	for entry := 0 to nes_ntsc_palette_size - 1 do
	begin
		level := (entry shr 4) and $03;
		lo := lo_levels[level];
		hi := hi_levels[level];

		color := entry and $0F;
		if color = $00 then lo := hi;
		if color = $0D then hi := lo;
		if color > $0D then begin hi := 0; lo := 0; end;

		// Convert raw waveform to YIQ
		sat := (hi - lo) * 0.5;
		i := phases[color+0] * sat;
		q := phases[color+3] * sat;
		y := (hi + lo) * 0.5;

		// Optionally use base palette instead
		if setup.base_palette <> nil then
		begin
			GetRGB(@setup.base_palette[(entry and $3F) * 3]);
			GetYIQ;
		end;

		// Apply color emphasis
		{$IFDEF NES_NTSC_EMPHASIS}
		tint := (entry shr 6) and 7;
		if (tint <> 0) and (color <= $0D) then
		begin
			if tint = 7 then
				y := y * (atten_mul * 1.13) - (atten_sub * 1.13)
			else
			begin
				tint_color := tints[tint];
				sat := hi * (0.5 - atten_mul * 0.5) + atten_sub * 0.5;
				y := y - (sat * 0.5);
				if (tint >= 3) and (tint <> 4) then
				begin
					sat := sat * 0.6;
					y := y - sat;
				end;
				i := i + (phases[tint_color+0] * sat);
				q := q + (phases[tint_color+3] * sat);
			end;
		end;
		{$ENDIF}

		// Optionally use palette instead
		if setup.palette <> nil then
		begin
			GetRGB(@setup.palette[entry * 3]);
			GetYIQ;
		end;

		// Apply brightness, contrast, and gamma
		y := y * (setup.contrast * 0.5 + 1);
		// adjustment reduces error when using input palette
		y := y + (setup.brightness * 0.5 - 0.5 / 256);

		r := y + default_decoder[0] * i + default_decoder[1] * q;
		g := y + default_decoder[2] * i + default_decoder[3] * q;
		b := y + default_decoder[4] * i + default_decoder[5] * q;

		// fast approximation of n = Power(n, gamma)
		r := (r * gamma_factor - gamma_factor) * r + r;
		g := (g * gamma_factor - gamma_factor) * g + g;
		b := (b * gamma_factor - gamma_factor) * b + b;

		GetYIQ;

		i := i * rgb_unit;
		q := q * rgb_unit;
		y := y * rgb_unit + rgb_offset;

		// Generate kernel
		//
		r := y + impl.to_rgb[0] * i + impl.to_rgb[1] * q;
		g := y + impl.to_rgb[2] * i + impl.to_rgb[3] * q;
		b := y + impl.to_rgb[4] * i + impl.to_rgb[5] * q;
		// blue tends to overflow, so clamp it
		rgb := (Trunc(r) shl 21) or (Trunc(g) shl 11) or (Min(Trunc(b), $3E0) shl 1);

		//if setup.palette_out <> nil then
		begin
			_out := @palette[entry];
			clamped := rgb;
			begin
				sub := (clamped shr 9) and nes_ntsc_clamp_mask;
				clamp := nes_ntsc_clamp_add - sub;
				clamped := clamped or clamp;
				Dec(clamp, sub);
				clamped := clamped and clamp;
			end;
			_out[0] := $FF; // alpha
			_out[1] := clamped shr 21;
			_out[2] := clamped shr 11;
			_out[3] := clamped shr 1;
		end;

		kernel := @table[entry * nes_ntsc_entry_size];
		GenerateKernel(y, i, q, kernel);
		if merge_fields then
			MergeKernelFields(kernel);
		CorrectErrors(rgb, kernel);
	end;

	{$IFDEF DEBUG_VERIFY_TABLE}
	FS := TFileStream.Create('!ntsc_table_hukka.dat', fmCreate);
	FS.WriteBuffer(table[0], tblen);
	FS.Free;

	_in := UrsiNES_NTSC_raw_blargg_ntsc;
	_out := @table2[0];
	Move(_in^, _out^, tblen);

	FS := TFileStream.Create('!ntsc_table_blargg.dat', fmCreate);
	FS.WriteBuffer(table2[0], tblen);
	FS.Free;

	for entry := 0 to 15 do
	begin
		kernel  := @table [entry * rgb_kernel_size];
		kernel2 := @table2[entry * rgb_kernel_size];
		if kernel^ <> kernel2^ then
		begin
			Log('entry=', entry, ': ',
				kernel^.ToHexString(8), ' <> ',
				kernel2^.ToHexString(8) );
		end;
	end;
	{$ENDIF}
end;

procedure TNtscFilter.DoInit;
var
	decoder, pin, pout: PFloat;
	to_float, gamma,
	hue, sat, s, c, i, q, t: Float;
	n, nn: Integer;
begin
	impl.brightness := setup.brightness * (0.5 * rgb_unit) + rgb_offset;
	impl.contrast   := setup.contrast   * (0.5 * rgb_unit) + rgb_unit;

	impl.artifacts  := setup.artifacts;
	if impl.artifacts > 0 then
		impl.artifacts := impl.artifacts * (artifacts_max - artifacts_mid);
	impl.artifacts := impl.artifacts * artifacts_mid + artifacts_mid;

	impl.fringing := setup.fringing;
	if impl.fringing > 0 then
		impl.fringing := impl.fringing * (fringing_max - fringing_mid);
	impl.fringing := impl.fringing * fringing_mid + fringing_mid;

	Init_nes_ntsc_pixels;
	InitFilters;

	// generate gamma table
	if gamma_size > 1 then
	begin
		to_float := 1.0 / (gamma_size - 1);
		gamma := 1.1333 - setup.gamma * 0.5;
		// match common PC's 2.2 gamma to TV's 2.65 gamma
		for n := 0 to gamma_size-1 do
			impl.to_float[n] := Power(n * to_float, gamma) * impl.contrast + impl.brightness;
	end;

	// setup decoder matrices
	hue := setup.hue * PI + PI / 180 * ext_decoder_hue;
	sat := setup.saturation + 1;
	if setup.decoder_matrix = nil then
	begin
		decoder := @default_decoder[0];
		if (setup.base_palette = nil) or (setup.palette = nil) then
			hue := hue + (PI / 180 * (std_decoder_hue - ext_decoder_hue));
	end
	else
		decoder := setup.decoder_matrix;

	s := Sin(hue) * sat;
	c := Cos(hue) * sat;
	pout := @impl.to_rgb[0];

	for n := 1 to burst_count do
	begin
		pin := decoder;
		for nn := 0 to 2 do
		begin
			i := pin^; Inc(pin);
			q := pin^; Inc(pin);
			pout^ := i * c - q * s; Inc(pout);
			pout^ := i * s + q * c; Inc(pout);
		end;
		if burst_count <= 1 then Break;

		// +120 degrees
		t := s * -0.5 - c * 0.866025;
		c := s * 0.866025 + c * -0.5;
		s := t;
	end;
end;

procedure TNtscFilter.Blit(input: PWord; output: PByte;
	burst_phase, in_width, in_height, in_row_width, out_pitch: Integer);
const
	EntrySize = nes_ntsc_entry_size * SizeOf(TRGB);
var
	y, n, chunk_count: Integer;
	ktable: PByte;
	kernel0, kernelx0,
	kernel1, kernelx1,
	kernel2, kernelx2,
	line_out: PRGB;
	line_in:  PWord;

	procedure Process(i: Byte); inline;
	var
		raw, sub, clamp: TRGB;
	begin
		raw := kernel0[i] + kernel1[(i+12) mod 7 +14] + kernel2[(i+10) mod 7 +28] +
			   kernelx0[(i+7) mod 14] + kernelx1[(i+5) mod 7 +21] + kernelx2[(i+3) mod 7 +35];
		sub := (raw shr 9) and nes_ntsc_clamp_mask;
		clamp := nes_ntsc_clamp_add - sub;
		raw := raw or clamp;
		Dec(clamp, sub);
		raw := raw and clamp;
		line_out[i] := $FF000000 or (raw shr 5 and $FF0000) or
			(raw shr 3 and $00FF00) or (raw shr 1 and $0000FF);
	end;

begin
	chunk_count := (in_width - 1) div nes_ntsc_in_chunk;

	for y := 1 to in_height do
	begin
		ktable := @table[burst_phase * burst_size];

		line_in  := @input[0];
		line_out := @output[0];

		kernel0 := @ktable[nes_ntsc_black * EntrySize];
		kernel1 := @ktable[nes_ntsc_black * EntrySize];
		kernel2 := @ktable[line_in[0] * EntrySize];

		kernelx1 := kernel0;
		kernelx2 := kernel0;

		Inc(line_in);

		for n := 1 to chunk_count do
		begin
			kernelx0 := kernel0;
			kernel0 := @ktable[line_in[0] * EntrySize];
			{%H-}Process(0);
			Process(1);

			kernelx1 := kernel1;
			kernel1 := @ktable[line_in[1] * EntrySize];
			Process(2);
			Process(3);

			kernelx2 := kernel2;
			kernel2 := @ktable[line_in[2] * EntrySize];
			Process(4);
			Process(5);
			Process(6);

			Inc(line_in,  nes_ntsc_in_chunk);
			Inc(line_out, nes_ntsc_out_chunk);
		end;

		// finish final pixels

		kernelx0 := kernel0;
		kernel0 := @ktable[nes_ntsc_black * EntrySize];
		Process(0);
		Process(1);

		kernelx1 := kernel1;
		kernel1 := @ktable[nes_ntsc_black * EntrySize];
		Process(2);
		Process(3);

		kernelx2 := kernel2;
		kernel2 := @ktable[nes_ntsc_black * EntrySize];
		Process(4);
		Process(5);
		Process(6);

		burst_phase := (burst_phase + 1) mod burst_count;

		Inc(PByte(input), in_row_width);
		Inc(output, out_pitch);
	end;

end;


end.

