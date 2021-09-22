unit FutureComposer.Filters;

interface

uses
	FutureComposer.Util, Math;

const
	M_PI_F   = 3.1415927;
	M_2PI_F  = 6.2831855;

	BLEP_ZC  = 8;
	BLEP_OS  = 5;
	BLEP_SP  = 5;
	BLEP_NS  = BLEP_ZC * BLEP_OS div BLEP_SP;
	BLEP_RNS = 7; // RNS = (2^ > NS) - 1


type
	TBlep = record
		index: Integer;
		SamplesLeft: Integer;
		Buffer: array [0..BLEP_RNS+1] of Single;
		LastValue: Single;
	end;
	PBlep = ^TBlep;

	TLossyIntegrator = record
		Buffer, coeff: array [0..2] of Single;
	end;


	function  LERP(const x, y, z: Single): Single; inline;

	procedure calcCoeffLossyIntegrator(sr, hz: Single; var filter: TLossyIntegrator);
	procedure clearLossyIntegrator(var filter: TLossyIntegrator);
	procedure lossyIntegrator(var filter: TLossyIntegrator; _in, _out: PArrayOfSingle);
	procedure lossyIntegratorHighPass(var filter: TLossyIntegrator; _in, _out: PArrayOfSingle);

	procedure blepAdd(var b: TBlep; offset, amplitude: Single);
	function  blepRun(var b: TBlep): Single;


implementation


const
	blepData: array [0..47] of Single = (
		0.999541342258453,		0.999348163604736,
		0.999368846416473,		0.999342143535614,
		0.998741447925568,		0.996602177619934,
		0.991206049919128,		0.979688584804535,
		0.957750201225281,		0.919731378555298,
		0.859310805797577,		0.770949482917786,
		0.651933908462524,		0.504528284072876,
		0.337462306022644,		0.165925666689873,
		0.0095284366980195,		-0.111686512827873,
		-0.182304441928864,		-0.196162655949593,
		-0.158864945173264,		-0.0870067626237869,
		-0.00398899614810944,	0.0664445236325264,
		0.106864832341671,		0.1107332482934,
		0.0833132192492485,		0.0386873260140419,
		-0.00589261529967189,	-0.0361355841159821,
		-0.0450748540461063,	-0.0342595390975475,
		-0.0117176882922649,	0.0121078062802553,
		0.0286061242222786,		0.0337694510817528,
		0.0286726150661707,		0.0179044492542744,
		0.00698232231661677,	0,0,0,0,0,0,0,0,0
	);


function LERP(const x, y, z: Single): Single;
begin
	Result := ((x) + ((y) - (x)) * (z));
end;


// ==========================================================================
// Filters
// ==========================================================================

procedure calcCoeffLossyIntegrator(sr, hz: Single; var filter: TLossyIntegrator);
begin
	filter.coeff[0] := Tan(M_PI_F * hz / sr);
	filter.coeff[1] := 1.0 / (1.0 + filter.coeff[0]);
end;

procedure clearLossyIntegrator(var filter: TLossyIntegrator);
begin
	filter.buffer[0] := 0.0;
	filter.buffer[1] := 0.0;
end;

procedure lossyIntegrator(var filter: TLossyIntegrator; _in, _out: PArrayOfSingle);
var
	output: Single;
begin
	// left channel low-pass
	output := (filter.coeff[0] * _in[0] + filter.buffer[0]) * filter.coeff[1];
	filter.buffer[0] := filter.coeff[0] * (_in[0] - output) + output + 1e-10;
	_out[0] := output;

	// right channel low-pass
	output := (filter.coeff[0] * _in[1] + filter.buffer[1]) * filter.coeff[1];
	filter.buffer[1] := filter.coeff[0] * (_in[1] - output) + output + 1e-10;
	_out[1] := output;
end;

procedure lossyIntegratorHighPass(var filter: TLossyIntegrator; _in, _out: PArrayOfSingle);
var
	low: array[0..1] of Single;
begin
	lossyIntegrator(filter, _in, @low);
	_out[0] := _in[0] - low[0];
	_out[1] := _in[1] - low[1];
end;


// ==========================================================================
// BLEP
// ==========================================================================

procedure blepAdd(var b: TBlep; offset, amplitude: Single);
var
	n, i, blepSrc: Integer;
	f: Single;
begin
	i := Trunc(offset * BLEP_SP);
	blepSrc := i + BLEP_OS;
	f := (offset * BLEP_SP) - i;

	i := b.Index;
	n := BLEP_NS;

	while n > 0 do
	begin
		b.Buffer[i] := b.Buffer[i] +
			(amplitude * LERP(blepData[blepSrc], blepData[blepSrc+1], f));

		Inc(blepSrc, BLEP_SP);
		Inc(i);
		i := i and BLEP_RNS;	// i = 0..7
		Dec(n);
	end;

	b.SamplesLeft := BLEP_NS;
end;

function blepRun(var b: TBlep): Single;
begin
	Result := b.Buffer[b.Index];
	b.Buffer[b.Index] := 0.0;
	Inc(b.Index);
	b.Index := b.Index and BLEP_RNS;
	Dec(b.SamplesLeft);
end;


end.
