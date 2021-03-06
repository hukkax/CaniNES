unit ProTracker.Util;

interface

uses
	Types,
	Basement.Util;

const
	// ========================================================================
	// Replayer

	FunkTable: packed array [0..16-1] of Byte = (
		$00, $05, $06, $07, $08, $0A, $0B, $0D,
		$10, $13, $16, $1A, $20, $2B, $40, $80
	);

	VibratoTable: packed array [0..32-1] of Byte = (
		$00, $18, $31, $4A, $61, $78, $8D, $A1,
		$B4, $C5, $D4, $E0, $EB, $F4, $FA, $FD,
		$FF, $FD, $FA, $F4, $EB, $E0, $D4, $C5,
		$B4, $A1, $8D, $78, $61, $4A, $31, $18
	);

	FineTunes: packed array [$0..$F] of SmallInt = (
		0,+1,+2,+3,+4,+5,+6,+7,-8,-7,-6,-5,-4,-3,-2,-1
	);

	PeriodTable: packed array [0..606-1] of SmallInt = (
		856,808,762,720,678,640,604,570,538,508,480,453,
		428,404,381,360,339,320,302,285,269,254,240,226,
		214,202,190,180,170,160,151,143,135,127,120,113,0,
		850,802,757,715,674,637,601,567,535,505,477,450,
		425,401,379,357,337,318,300,284,268,253,239,225,
		213,201,189,179,169,159,150,142,134,126,119,113,0,
		844,796,752,709,670,632,597,563,532,502,474,447,
		422,398,376,355,335,316,298,282,266,251,237,224,
		211,199,188,177,167,158,149,141,133,125,118,112,0,
		838,791,746,704,665,628,592,559,528,498,470,444,
		419,395,373,352,332,314,296,280,264,249,235,222,
		209,198,187,176,166,157,148,140,132,125,118,111,0,
		832,785,741,699,660,623,588,555,524,495,467,441,
		416,392,370,350,330,312,294,278,262,247,233,220,
		208,196,185,175,165,156,147,139,131,124,117,110,0,
		826,779,736,694,655,619,584,551,520,491,463,437,
		413,390,368,347,328,309,292,276,260,245,232,219,
		206,195,184,174,164,155,146,138,130,123,116,109,0,
		820,774,730,689,651,614,580,547,516,487,460,434,
		410,387,365,345,325,307,290,274,258,244,230,217,
		205,193,183,172,163,154,145,137,129,122,115,109,0,
		814,768,725,684,646,610,575,543,513,484,457,431,
		407,384,363,342,323,305,288,272,256,242,228,216,
		204,192,181,171,161,152,144,136,128,121,114,108,0,
		907,856,808,762,720,678,640,604,570,538,508,480,
		453,428,404,381,360,339,320,302,285,269,254,240,
		226,214,202,190,180,170,160,151,143,135,127,120,0,
		900,850,802,757,715,675,636,601,567,535,505,477,
		450,425,401,379,357,337,318,300,284,268,253,238,
		225,212,200,189,179,169,159,150,142,134,126,119,0,
		894,844,796,752,709,670,632,597,563,532,502,474,
		447,422,398,376,355,335,316,298,282,266,251,237,
		223,211,199,188,177,167,158,149,141,133,125,118,0,
		887,838,791,746,704,665,628,592,559,528,498,470,
		444,419,395,373,352,332,314,296,280,264,249,235,
		222,209,198,187,176,166,157,148,140,132,125,118,0,
		881,832,785,741,699,660,623,588,555,524,494,467,
		441,416,392,370,350,330,312,294,278,262,247,233,
		220,208,196,185,175,165,156,147,139,131,123,117,0,
		875,826,779,736,694,655,619,584,551,520,491,463,
		437,413,390,368,347,328,309,292,276,260,245,232,
		219,206,195,184,174,164,155,146,138,130,123,116,0,
		868,820,774,730,689,651,614,580,547,516,487,460,
		434,410,387,365,345,325,307,290,274,258,244,230,
		217,205,193,183,172,163,154,145,137,129,122,115,0,
		862,814,768,725,684,646,610,575,543,513,484,457,
		431,407,384,363,342,323,305,288,272,256,242,228,
		216,203,192,181,171,161,152,144,136,128,121,114,0,
		// PT BUGFIX: overflowing arpeggio on -1 finetuned samples, add extra zeroes.
		0,0,0,0,0,0,0,0,0,0,0,0,0,0
	);

	NoteHz: array [0..107] of Single = ( // used when generating waveforms
	  16.35,   17.32,   18.35,   19.45,   20.60,   21.83,   23.12,   24.50,   25.96,   27.50,   29.14,   30.87,
	  32.70,   34.65,   36.71,   38.89,   41.20,   43.65,   46.25,   49.00,   51.91,   55.00,   58.27,   61.74,
	  65.41,   69.30,   73.42,   77.78,   82.41,   87.31,   92.50,   98.00,  103.83,  110.00,  116.54,  123.47,
	 130.81,  138.59,  146.83,  155.56,  164.81,  174.61,  185.00,  196.00,  207.65,  220.00,  233.08,  246.94,
	 261.63,  277.18,  293.66,  311.13,  329.63,  349.23,  369.99,  392.00,  415.30,  440.00,  466.16,  493.88,
	 523.25,  554.37,  587.33,  622.25,  659.25,  698.46,  739.99,  783.99,  830.61,  880.00,  932.33,  987.77,
	1046.50, 1108.73, 1174.66, 1244.51, 1318.51, 1396.91, 1479.98, 1567.98, 1661.22, 1760.00, 1864.66, 1975.53,
	2093.00, 2217.46, 2349.32, 2489.02, 2637.02, 2793.83, 2959.96, 3135.96, 3322.44, 3520.00, 3729.31, 3951.07,
	4186.01, 4434.92, 4698.63, 4978.03, 5274.04, 5587.65, 5919.91, 6271.93, 6644.88, 7040.00, 7458.62, 7902.13
	);

	NoteText: array [0..85] of AnsiString = ( '...',
		'C-1','C#1','D-1','D#1','E-1','F-1','F#1','G-1','G#1','A-1','A#1','B-1',
		'C-2','C#2','D-2','D#2','E-2','F-2','F#2','G-2','G#2','A-2','A#2','B-2',
		'C-3','C#3','D-3','D#3','E-3','F-3','F#3','G-3','G#3','A-3','A#3','B-3',
		// invalid notes
		'C-4','C#4','D-4','D#4','E-4','F-4','F#4','G-4','G#4','A-4','A#4','B-4',
		'C-5','C#5','D-5','D#5','E-5','F-5','F#5','G-5','G#5','A-5','A#5','B-5',
		'C-6','C#6','D-6','D#6','E-6','F-6','F#6','G-6','G#6','A-6','A#6','B-6',
		'C-7','C#7','D-7','D#7','E-7','F-7','F#7','G-7','G#7','A-7','A#7','B-7',
		'???'
	);

	// ========================================================================
	// Utility

	function  GetPeriodTableOffset(period: Word): Integer;
	function  PeriodToNote(period: Word): Integer;
	function  PeriodToHz(period: Word): Cardinal; inline;

	function  LinearToDecibel(linear: Single): Single; inline;
	function  DecibelToLinear(dB: Single): Single; inline;

	procedure ZeroMemory(Destination: Pointer; Length: DWord); inline;
    procedure CopyMemory(Destination, Source:pointer; Length:DWord); inline;

    function  CLAMP(const x, low, high: Integer): Integer; inline;
	function  CLAMP2(x, low, high: Integer; var clipcount: Integer): Integer; inline;
	function  RoundUp(X: Real): Integer;
	function  Swap16(const x: Word): Word;
	function  GetPtr16(const S: PArrayOfByte): Word;
	function  GetPtr32(const S: PArrayOfByte): Cardinal;


implementation

uses
	SysUtils, Math,
	ProTracker.Player;


procedure ZeroMemory(Destination: Pointer; Length: DWord);
begin
	FillChar(Destination^, Length, 0);
end;

procedure CopyMemory(Destination, Source:pointer; Length:DWord);
begin
	Move(Source^, Destination^, Length);
end;

// ==========================================================================
// ProTracker
// ==========================================================================

// returns index to PeriodTable[]
//
function GetPeriodTableOffset(period: Word): Integer;
var
	i: Integer;
begin
	Result := -1;
	if period = 0 then Exit;
	for i := Low(PeriodTable) to High(PeriodTable) do
		if period = PeriodTable[i] then Exit(i);
end;

// returns index to NoteText[]
//
function PeriodToNote(period: Word): Integer;
var
	i: Integer;
begin
	if period = 0 then Exit(0);
	for i := 1 to High(NoteText) do
		if period = PeriodTable[i-1] then Exit(i);
	Result := High(NoteText); //37;
end;

function PeriodToHz(period: Word): Cardinal;
begin
	Result := Round(7093789.2 / (period * 2));
end;

// ==========================================================================
// Math
// ==========================================================================

function LinearToDecibel(linear: Single): Single;
begin
	if linear <> 0 then
		Result := 20.0 * Log10(linear)
	else
		Result := -144.0;
end;

function DecibelToLinear(dB: Single): Single;
begin
	Result := Power(10.0, dB / 20.0);
end;

// Returns True if clamped
function CLAMP2(x, low, high: Integer; var clipcount: Integer): Integer;
begin
	if x > high then
	begin
		Result := high;
		Inc(clipcount);
	end
	else
	if x < low then
	begin
		Result := low;
		Inc(clipcount);
	end
	else
		Result := x;
end;

function CLAMP(const x, low, high: Integer): Integer;
begin
	if x > high then
		Result := high
	else
	if x < low then
		Result := low
	else
		Result := x;
end;

function RoundUp(X: Real): Integer;
var
	RUAdder: Integer;
begin
	if Frac(X) > 0 then RUAdder := 1 else RUAdder := 0;
	Result := Trunc(X) + RUAdder;
end;

function Swap16(const x: Word): Word;
begin
	Result := (x shl 8) or (x shr 8);
end;

function GetPtr16(const S: PArrayOfByte): Word;
begin
	Result := (S[1] shl 8) + S[0];
end;

function GetPtr32(const S: PArrayOfByte): Cardinal;
begin
	Result := (S[0] shl 24) + (S[1] shl 16) + (S[2] shl 8) + S[3];
end;

end.
