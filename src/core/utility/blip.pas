unit Blip;

{$MODE DELPHI}
{$WARN 4035 off : Mixing signed expressions and longwords gives a 64bit result}

interface

uses
	Classes, SysUtils;

type
	buf_t   = Int32;
	fixed_t = Cardinal;

	pbuf_t = ^buf_t;
	(*
	// Equivalent to ULONG_MAX >= 0xFFFFFFFF00000000.
	//Avoids constants that don't fit in 32 bits.

	#if ULONG_MAX/0xFFFFFFFF > 0xFFFFFFFF
		typedef unsigned long fixed_t;
		enum { pre_shift = 32 };
	#elif defined(ULLONG_MAX)
		typedef unsigned long long fixed_t;
		enum { pre_shift = 32 };
	#else
		typedef unsigned fixed_t;
		enum { pre_shift = 0 };
	#endif
	*)

const
	max_sample = +32767;
	min_sample = -32768;

	// Maximum clock_rate/sample_rate ratio. For a given sample_rate,
	// clock_rate must not be greater than sample_rate*blip_max_ratio.
	blip_max_ratio = 1 shl 20;

	// Maximum number of samples that can be generated from one time frame.
	blip_max_frame = 4000;

	pre_shift = 0;
	time_bits = pre_shift + 20;
	time_unit: fixed_t = 1 shl time_bits;

	bass_shift  = 9;     // affects high-pass filter breakpoint frequency
	end_frame_extra = 2; // allows deltas slightly after frame length

	half_width  = 8;
	buf_extra   = half_width*2 + end_frame_extra;
	phase_bits  = 5;
	phase_count = 1 shl phase_bits;
	delta_bits  = 15;
	delta_unit  = 1 shl delta_bits;
	frac_bits = time_bits - pre_shift;

	// Sinc_Generator( 0.9, 0.55, 4.5 )
	bl_step: array[0..phase_count] of array [0..half_width-1] of Int16 =
		(
		(   43, -115,  350, -488, 1136, -914, 5861,21022),
		(   44, -118,  348, -473, 1076, -799, 5274,21001),
		(   45, -121,  344, -454, 1011, -677, 4706,20936),
		(   46, -122,  336, -431,  942, -549, 4156,20829),
		(   47, -123,  327, -404,  868, -418, 3629,20679),
		(   47, -122,  316, -375,  792, -285, 3124,20488),
		(   47, -120,  303, -344,  714, -151, 2644,20256),
		(   46, -117,  289, -310,  634,  -17, 2188,19985),
		(   46, -114,  273, -275,  553,  117, 1758,19675),
		(   44, -108,  255, -237,  471,  247, 1356,19327),
		(   43, -103,  237, -199,  390,  373,  981,18944),
		(   42,  -98,  218, -160,  310,  495,  633,18527),
		(   40,  -91,  198, -121,  231,  611,  314,18078),
		(   38,  -84,  178,  -81,  153,  722,   22,17599),
		(   36,  -76,  157,  -43,   80,  824, -241,17092),
		(   34,  -68,  135,   -3,    8,  919, -476,16558),
		(   32,  -61,  115,   34,  -60, 1006, -683,16001),
		(   29,  -52,   94,   70, -123, 1083, -862,15422),
		(   27,  -44,   73,  106, -184, 1152,-1015,14824),
		(   25,  -36,   53,  139, -239, 1211,-1142,14210),
		(   22,  -27,   34,  170, -290, 1261,-1244,13582),
		(   20,  -20,   16,  199, -335, 1301,-1322,12942),
		(   18,  -12,   -3,  226, -375, 1331,-1376,12293),
		(   15,   -4,  -19,  250, -410, 1351,-1408,11638),
		(   13,    3,  -35,  272, -439, 1361,-1419,10979),
		(   11,    9,  -49,  292, -464, 1362,-1410,10319),
		(    9,   16,  -63,  309, -483, 1354,-1383, 9660),
		(    7,   22,  -75,  322, -496, 1337,-1339, 9005),
		(    6,   26,  -85,  333, -504, 1312,-1280, 8355),
		(    4,   31,  -94,  341, -507, 1278,-1205, 7713),
		(    3,   35, -102,  347, -506, 1238,-1119, 7082),
		(    1,   40, -110,  350, -499, 1190,-1021, 6464),
		(    0,   43, -115,  350, -488, 1136, -914, 5861)
		);

type
	// Sample buffer that resamples to output rate and accumulates samples until they're read out
	//
	TBlip = class
	private
	public
		factor,
		offset,
		avail:      fixed_t;
		size,
		integrator: Int32;
		samples:    array of buf_t;

		constructor Create(ASize: Cardinal);
		destructor  Destroy; override;

		procedure Clear;
		procedure CheckAssumptions;

		procedure SetRates(clock_rate, sample_rate: Single);
		function  ClocksNeeded(samples: Int32): Int32;
		procedure EndFrame(t: Cardinal);
		function  SamplesAvail: Int32;
		procedure RemoveSamples(count: Int32);
		function  ReadSamples(buffer: PInt16; count: Int32; stereo: Boolean): Int32;
		procedure AddDelta(time: Cardinal; delta: Int32);
		procedure AddDeltaFast(time: Cardinal; delta: Int32);
	end;


implementation


function ARITH_SHIFT(n: LongInt; shift: Byte): LongInt; inline;
begin
	Result := SarLongint(n, shift);
end;

procedure CLAMP(var n: Int32); inline;
begin
	if n > max_sample then n := max_sample
	else
	if n < min_sample then n := min_sample;
end;


constructor TBlip.Create(ASize: Cardinal);
begin
	inherited Create;

	factor := Trunc(time_unit / blip_max_ratio);
	size := ASize;
	Clear;
	CheckAssumptions;
end;

destructor TBlip.Destroy;
begin
	inherited Destroy;
end;

procedure TBlip.CheckAssumptions;
var
	n: Int32;
begin
	Assert(ARITH_SHIFT(-3, 1) = -2); // right shift must preserve sign

	n := max_sample * 2;
	CLAMP(n);
	Assert(n = max_sample);

	n := min_sample * 2;
	CLAMP(n);
	Assert(n = min_sample);

	Assert(blip_max_ratio <= time_unit);
//	Assert(blip_max_frame <= fixed_t(-1 shr time_bits));
end;

procedure TBlip.Clear;
begin
	// We could set offset to 0, factor/2, or factor-1. 0 is suitable if
	// factor is rounded up. factor-1 is suitable if factor is rounded down.
	// Since we don't know rounding direction, factor/2 accommodates either,
	// with the slight loss of showing an error in half the time. Since for
	// a 64-bit factor this is years, the halving isn't a problem. */
	offset     := factor div 2;
	avail      := 0;
	integrator := 0;
	SetLength(samples, size + buf_extra);
	FillByte(samples[0], (size + buf_extra) * SizeOf(buf_t), 0);
end;

procedure TBlip.SetRates(clock_rate, sample_rate: Single);
var
	sfactor: Single;
begin
	sfactor := time_unit * sample_rate / clock_rate;
	factor := Trunc(sfactor);

	// Fails if clock_rate exceeds maximum, relative to sample_rate
	Assert((0 <= sfactor - factor) and (sfactor - factor < 1));

	// Equivalent to factor := Ceil(sfactor)
	if (factor < Trunc(sfactor)) then Inc(factor);

	// At this point, factor is most likely rounded up, but could still
	// have been rounded down in the floating-point calculation.
end;

function TBlip.ClocksNeeded(samples: Int32): Int32;
var
	needed: fixed_t;
begin
	// Fails if buffer can't hold that many more samples
	Assert( (samples >= 0) and (avail + samples <= size) );

	needed := samples * time_unit;
	if needed < offset then
		Result := 0
	else
		Result := Trunc((needed - offset + factor - 1) / factor);
end;

procedure TBlip.EndFrame(t: Cardinal);
var
	off: fixed_t;
begin
	off := t * factor + offset;
	Inc(avail, off shr time_bits);
	offset := off and (time_unit - 1);

	// Fails if buffer size was exceeded
	Assert(avail <= size);
end;

function TBlip.SamplesAvail: Int32;
begin
	Result := avail;
end;

procedure TBlip.RemoveSamples(count: Int32);
var
	remain: Int32;
begin
	remain := avail + buf_extra - count;
	Dec(avail, count);

	//memmove( &buf[0], &buf [count], remain * sizeof buf[0] );
	Move(samples[count], samples[0], remain * SizeOf(buf_t));

	//memset( &buf[remain], 0, count * sizeof buf[0] );
	FillChar(samples[remain], count * SizeOf(buf_t), 0);
end;

//#define SAMPLES(buf)   ((buf_t * ) ((buf) + 1))

function TBlip.ReadSamples(buffer: PInt16; count: Int32; stereo: Boolean): Int32;
var
	sum, step: Int32;
	WP, RP: Cardinal;
	s: Int32;
begin
	if count > avail then count := avail;

	if count > 0 then
	begin
		WP := 0; // write pos
		RP := 0; // read pos
		if stereo then
			step := 2
		else
			step := 1;

		sum := integrator;

		while RP < count do
		begin
			// Eliminate fraction
			// Arithmetic (sign-preserving) right shift
			s := SarLongint(sum, delta_bits);

			Inc(sum, samples[RP]);
			Inc(RP);

			CLAMP(s);

			buffer[WP] := s;
			Inc(WP, step);

			// High-pass filter
			Dec(sum, s shl (delta_bits - bass_shift));
		end;

		integrator := sum;
		RemoveSamples(count);
	end;

	Result := count;
end;

// Shifting by pre_shift allows calculation using unsigned int rather than
// possibly-wider fixed_t. On 32-bit platforms, this is likely more efficient.
// And by having pre_shift 32, a 32-bit platform can easily do the shift by
// simply ignoring the low half.
//
procedure TBlip.AddDelta(time: Cardinal; delta: Int32);
var
	fixed: Int32; // unsigned
	rev, fin: PInt16;
	outf: PInt32; //pbuf_t;
	phase, phase_shift,
	interp, delta2: Int32;
begin
	//fixed := Cardinal(ARITH_SHIFT(time * factor + offset, pre_shift));
	fixed := (time * factor + offset);

	outf := @samples[avail + (fixed shr frac_bits)];

	phase_shift := frac_bits - phase_bits;
	phase := SarLongint(fixed, phase_shift) and (phase_count - 1);
	fin := @bl_step[phase];
	rev := @bl_step[phase_count - phase];

	interp := Int32(fixed shr phase_shift - delta_bits) and (delta_unit - 1);
	delta2 := SarLongint(delta * interp, delta_bits);
	Dec(delta, delta2);

	// Fails if buffer size was exceeded
	//Assert( outf <= samples[size + end_frame_extra] );

	Inc(outf[0], fin[0]*delta + fin[half_width+0]*delta2);
	Inc(outf[1], fin[1]*delta + fin[half_width+1]*delta2);
	Inc(outf[2], fin[2]*delta + fin[half_width+2]*delta2);
	Inc(outf[3], fin[3]*delta + fin[half_width+3]*delta2);
	Inc(outf[4], fin[4]*delta + fin[half_width+4]*delta2);
	Inc(outf[5], fin[5]*delta + fin[half_width+5]*delta2);
	Inc(outf[6], fin[6]*delta + fin[half_width+6]*delta2);
	Inc(outf[7], fin[7]*delta + fin[half_width+7]*delta2);

	fin := rev;
	Inc(outf[ 8], fin[7]*delta + fin[7-half_width]*delta2);
	Inc(outf[ 9], fin[6]*delta + fin[6-half_width]*delta2);
	Inc(outf[10], fin[5]*delta + fin[5-half_width]*delta2);
	Inc(outf[11], fin[4]*delta + fin[4-half_width]*delta2);
	Inc(outf[12], fin[3]*delta + fin[3-half_width]*delta2);
	Inc(outf[13], fin[2]*delta + fin[2-half_width]*delta2);
	Inc(outf[14], fin[1]*delta + fin[1-half_width]*delta2);
	Inc(outf[15], fin[0]*delta + fin[0-half_width]*delta2);
end;

procedure TBlip.AddDeltaFast(time: Cardinal; delta: Int32);
var
	fixed: Cardinal; // unsigned
	outf: PInt32; //pbuf_t;
	interp, delta2: Int32;
begin
	fixed := Cardinal(time * factor + offset);
	outf := @samples[avail + (fixed shr frac_bits)];

	interp := SarLongint(fixed, frac_bits - delta_bits) and (delta_unit - 1);
	delta2 := delta * interp;

	Inc(outf[7], delta * delta_unit - delta2);
	Inc(outf[8], delta2);
end;


end.

