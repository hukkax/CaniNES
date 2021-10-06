unit Basement.Util;

interface

uses
	Types;

{$IF Defined(DEBUG) or Defined(UNIX)}
	{$DEFINE HAVE_CONSOLE}
{$ELSE}
	{$UNDEF  HAVE_CONSOLE}
{$IFEND}

const
	// Initial window placement on screen: OS Default/Centered/Custom placement
	//
	WINDOWPOS_DEFAULT  = -1;
	WINDOWPOS_CENTERED = -2;

	// Vertical sync: Auto/Forced on/Disabled
	//
	VSYNC_AUTO	= 0;
	VSYNC_FORCE	= 1;
	VSYNC_OFF	= 2;

	// Mouse pointer: Native/Software sprite/Hidden
	//
	CURSOR_SYSTEM = 0;
	CURSOR_CUSTOM = 1;
	CURSOR_NONE   = 2;

type
	// hacks to allow pointer maths on data
	TArrayOfByte 		= array [0..65535] of Byte;
	TArrayOfShortInt 	= array [0..65535] of ShortInt;
	TArrayOfSmallInt 	= array [0..65535] of SmallInt;
	TArrayOfCardinal	= array [0..65535] of Cardinal;
	TArrayOfSingle 		= array [0..65535] of Single;
	TArrayOfString		= array of String;
	TArrayOfAnsiString	= array of AnsiString;
	TFloatArray			= array of Single;

	PArrayOfByte		= ^TArrayOfByte;
	PArrayOfShortInt	= ^TArrayOfShortInt;
	PArrayOfSmallInt 	= ^TArrayOfSmallInt;
	PArrayOfSingle 		= ^TArrayOfSingle;
	PArrayOfCardinal	= ^TArrayOfCardinal;

	TMinMax = record
		Min, Max: Integer;
	end;

var
	AppPath, DataPath, ConfigPath: String;
	Locked: Boolean;

	// ========================================================================
	// Utility

    procedure Log(const S: AnsiString); overload;
	procedure Log(const S: AnsiString; const Args: array of const); overload;
	procedure LogDebug(const Msg: AnsiString);
	procedure LogIfDebug(const Msg: AnsiString);

	procedure LogInfo(const Msg: AnsiString); inline;
	procedure LogWarning(const Msg: AnsiString); inline;
	procedure LogError(const Msg: AnsiString); inline;
	procedure LogFatal(const Msg: AnsiString); inline;

	function  GetDataFile(Filename: String): String;
	function  ValidFilename(const Filename: String): Boolean; inline;

	function  SplitWords(const S, Separator: String; out S1, S2: String): Boolean;
	function  SplitString(const aString, aSeparator: String; aMax: Integer = 0): TArrayOfString;

	function  StringInData(Data: PByte; const Text: AnsiString): Boolean; inline;
	function  StringFromData({%H-}Data: PByte; Len: Word): AnsiString;

	procedure ZeroMemory(Destination: Pointer; Length: DWord); inline;
    procedure CopyMemory(Destination, Source: Pointer; Length:DWord); inline;

    function  CLAMP(const x, low, high: Integer): Integer; inline;
	function  CLAMP2(x, low, high: Integer; var clipcount: Integer): Integer; inline;
	function  RoundUp(X: Real): Integer; inline;
	function  Swap16(const x: Word): Word; inline;
	function  GetPtr16(const S: PArrayOfByte): Word; inline;
	function  GetPtr32(const S: PArrayOfByte): Cardinal; inline;

var
	OnLog: procedure (const Msg: AnsiString) of Object;


implementation

uses
	LazLoggerBase,
	{$IFDEF WINDOWS}Windows,{$ENDIF}
	SysUtils;

var
	HaveConsole: Boolean;

function HasTerminal: Boolean;
{$IFDEF WINDOWS}
var
	Stdout: THandle;
begin
	Stdout := GetStdHandle(Std_Output_Handle);
	Win32Check(Stdout <> Invalid_Handle_Value);
	Result := Stdout <> 0;
end;
{$ELSE}
begin Result := True; end;
{$ENDIF}

// ==========================================================================
// Logging
// ==========================================================================

procedure Log(const S: AnsiString); overload;
begin
	if Assigned(OnLog) then
		OnLog(S)
	else
	if HaveConsole then
		WriteLn(S);
end;

procedure Log(const S: AnsiString; const Args: array of const); overload;
begin
	Log(Format(S, Args));
end;

procedure LogDebug(const Msg: AnsiString);
begin
	if HaveConsole then
		Log(Msg);
end;

procedure LogIfDebug(const Msg: AnsiString);
begin
	{$IFDEF DEBUG}LogDebug(Msg);{$ENDIF}
end;

procedure LogInfo(const Msg: AnsiString);
begin
	LogDebug(Msg);
end;

procedure LogWarning(const Msg: AnsiString);
begin
	LogDebug('Warning: ' + Msg);
end;

procedure LogError(const Msg: AnsiString);
begin
	LogDebug('ERROR: ' + Msg);
end;

procedure LogFatal(const Msg: AnsiString);
begin
	LogDebug('FATAL: ' + Msg);
end;

// ==========================================================================
// Lowlevel
// ==========================================================================

procedure ZeroMemory(Destination: Pointer; Length: DWord);
begin
	FillByte(Destination^, Length, 0);
end;

procedure CopyMemory(Destination, Source: Pointer; Length:DWord);
begin
	Move(Source^, Destination^, Length);
end;

function StringInData(Data: PByte; const Text: AnsiString): Boolean;
begin
	Result := CompareMem(Data, @Text[1], Length(Text));
end;

function StringFromData(Data: PByte; Len: Word): AnsiString;
begin
	SetLength(Result, Len);
	if Len > 0 then
		Move(Data, Result[1], Len);
end;

// ==========================================================================
// Utility
// ==========================================================================

function GetDataFile(Filename: String): String;
begin
	if FileExists(Filename) then
		Exit(Filename)
	else
	if Pos(':', Filename) > 0 then
		Filename := ExtractFileName(Filename);

	Result := DataPath + Filename;

	if (not FileExists(Result)) and (ConfigPath <> DataPath) then
		Result := ConfigPath + Filename;

	if not FileExists(Result) then
	begin
		Result := '';
//		LogDebug('GetDataFile: File not found: "' + Filename + '"');
	end;
end;

function ValidFilename(const Filename: String): Boolean;
begin
	Result := (Filename <> '') and (FileExists(Filename));
end;

function SplitWords(const S, Separator: String; out S1, S2: String): Boolean;
var
	X: Integer;
begin
	X := S.IndexOf(Separator);
	if X < 0 then Exit(False);

	S1 := Copy(S, 1, X);
	S2 := Copy(S, X+1+Length(Separator), MaxInt);

	Result := True;
end;

function SplitString(const aString, aSeparator: String; aMax: Integer = 0): TArrayOfString;
var
	i, strt, cnt, sepLen: Integer;

	procedure AddString(aEnd: Integer = -1);
	var
		endPos: Integer;
	begin
		if (aEnd = -1) then
			endPos := i
		else
			endPos := aEnd + 1;
		if (strt < endPos) then
			Result[cnt] := Copy(aString, strt, endPos - strt)
		else
			Result[cnt] := '';
		Inc(cnt);
	end;

begin
	if (aString = '') or (aMax < 0) then
	begin
		SetLength(Result, 0);
		Exit;
	end;
	if (aSeparator = '') then
	begin
		SetLength(Result, 1);
		Result[0] := aString;
		Exit;
	end;
	sepLen := Length(aSeparator);
	SetLength(Result, (Length(aString) div sepLen) + 1);
	i     := 1;
	strt  := i;
	cnt   := 0;
	while (i <= (Length(aString)- sepLen + 1)) do
	begin
		if (aString[i] = aSeparator[1]) then
		if (Copy(aString, i, sepLen) = aSeparator) then
		begin
			AddString;
			if (cnt = aMax) then
			begin
				SetLength(Result, cnt);
				Exit;
			end;
			Inc(i, sepLen - 1);
			strt := i + 1;
		end;
		Inc(i);
	end;
	AddString(Length(aString));
	SetLength(Result, cnt);
end;

// ==========================================================================
// Math
// ==========================================================================

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
	Result := (S^[1] shl 8) + S^[0];
end;

function GetPtr32(const S: PArrayOfByte): Cardinal;
begin
	Result := (S^[0] shl 24) + (S^[1] shl 16) + (S^[2] shl 8) + S^[3];
end;


initialization

	HaveConsole := HasTerminal;

end.
