unit FileStreamEx;

// Simple class to simplify file access, by hukka 2017-03-31
// methods ending in R read/write big-endian values

{$mode delphi}

interface

uses
	Classes, SysUtils;

const
	RLE_ESCAPE = $FF; // escape byte telling a run follows
	RLE_MAXRUN = $FE; // maximum length of one run
	RLE_MINRUN = $03; // minimum run length to encode

type
	{ TByteDataReader }

	TByteDataReader = class
	private
		Pos,
		Size:		Int64;
	public
		Data:		array of Byte;

		function	LoadFromFile(const Filename: String): Int64;
		function	LoadFromStream(const Stream: TStream): Int64;

		function	Read8:		Byte; inline;
		function	Read16:		Word; inline;
		function	Read16R:	Word; inline;
		function	Read24:		Cardinal; inline;
		function	Read24R:	Cardinal; inline;
		function	Read32:		Cardinal; inline;
		function	Read32R:	Cardinal; inline;

		procedure	Skip(Len: Int64); inline;
		procedure	SeekTo(Offset: Int64); inline;

		function	RLEDecode(NumBytes: Integer = -1): TMemoryStream;

		property 	Position: Int64 read Pos;
	end;

	{ TFileStreamEx }

	TFileStreamEx = class(TFileStream)
	public
		Bytes:		TByteDataReader;

		function	ReadBool:	Boolean; inline;
		function	Read8:		Byte; inline;
		function	Read16:		Word; inline;
		function	Read16R:	Word; inline;
		function	Read24:		Cardinal; inline;
		function	Read24R:	Cardinal; inline;
		function	Read32:		Cardinal; inline;
		function	Read32R:	Cardinal; inline;
		function	Read64:		QWord; inline;
		function	Read64R:	QWord; inline;
		function	ReadString(StopAtZero: Boolean;
					MaxLen: Word = 0): AnsiString; overload;
		function 	ReadData: Int64;

		procedure   WriteBool(Val: Boolean);
		procedure	Write8  (Val: Byte); overload; inline;
		procedure	Write8  (Val: Char); overload; inline;
		procedure	Write16 (Val: Word); inline;
		procedure	Write16R(Val: Word); inline;
		procedure	Write32 (Val: Cardinal); inline;
		procedure	Write32R(Val: Cardinal); inline;
		procedure	Write64 (Val: QWord); inline;
		procedure	Write64R(Val: QWord); inline;
		procedure	WriteString(const Val: AnsiString; Terminate: Boolean = False);
		procedure	WriteData;

		procedure	Skip(Len: Int64); inline;
		procedure	SeekTo(Offset: Int64); inline;

		destructor  Destroy; override;
	end;


	{ TStreamEx }

	TStreamEx = class
	public
		Stream:		TStream;
		//Bytes:		TByteDataReader;

		function	Read8:		Byte; inline;
		function	Read16:		Word; inline;
		function	Read16R:	Word; inline;
		function	Read32:		Cardinal; inline;
		function	Read32R:	Cardinal; inline;
		function	Read64:		QWord; inline;
		function	Read64R:	QWord; inline;
		function	ReadString(StopAtZero: Boolean;
					MaxLen: Word = 0): AnsiString; overload;
		//function 	ReadData: Int64;
		function	GetCRC32(StartPos: Int64 = -1; Len: Int64 = -1): Cardinal;

		function	Pos:  QWord;
		function	Size: QWord;

		procedure	Skip(Len: Int64); inline;
		procedure	SeekTo(Offset: Int64); inline;

		constructor Create(const AStream: TStream);
		destructor  Destroy; override;
	end;


	function  FileToString(const FileName: String): AnsiString;
	function  FileToBytes(const FileName: String; var Bytes: TBytes): Int64;
	function  StreamReadString(Stream: TStream;
		StopAtZero: Boolean = True; MaxLen: Word = 0): AnsiString; inline;
	procedure StreamWriteString(Stream: TStream;
		const Val: AnsiString; Terminate: Boolean = True); inline;

	function FileGetCRC32(const Filename: String; StartPos: Int64 = -1; Len: Int64 = -1): Cardinal;
	function StreamGetCRC32(Stream: TStream; StartPos: Int64 = -1; Len: Int64 = -1): Cardinal;


implementation

uses
	Math, CRC;

function FileGetCRC32(const Filename: String; StartPos: Int64 = -1; Len: Int64 = -1): Cardinal;
var
	Stream: TFileStream;
begin
	Stream := TFileStream.Create(FileName, fmOpenRead);
	Result := StreamGetCRC32(Stream, StartPos, Len);
	Stream.Free;
end;

function StreamGetCRC32(Stream: TStream; StartPos: Int64 = -1; Len: Int64 = -1): Cardinal;
var
	OrigPos: Int64;
	Buf: array of Byte;
begin
	Result := CRC32(0, nil, 0);
	OrigPos := Stream.Position;
	if Len < 0 then Len := Stream.Size;
	if StartPos < 0 then
		StartPos := OrigPos
	else
		Stream.Seek(StartPos, soBeginning);
	Len := Min(Len, Stream.Size-StartPos);
	if Len > 0 then
	begin
		SetLength(Buf, Len);
		Stream.Read(Buf[0], Len);
		Result := CRC32(Result, @Buf[0], Len);
		Stream.Seek(OrigPos, soBeginning);
	end;
end;

function StreamReadString(Stream: TStream;
	StopAtZero: Boolean; MaxLen: Word): AnsiString;
var
	x: Integer;
	B: Byte;
begin
	Result := '';
	if MaxLen = 0 then MaxLen := High(Word);
	x := 0;
	repeat
		B := Stream.ReadByte;
		if (StopAtZero) and (B = 0) then
			Break;
		Result := Result + AnsiChar(B);
		Inc(x);
	until x >= MaxLen;
end;

procedure StreamWriteString(Stream: TStream; const Val: AnsiString; Terminate: Boolean);
begin
	Stream.WriteBuffer(Pointer(Val)^, Length(Val));
	if Terminate then
		Stream.WriteByte(0);
end;


function FileToString(const FileName: String): AnsiString;
var
	Stream: TFileStream;
begin
	Stream := TFileStream.Create(FileName, fmOpenRead);
	try
		SetLength(Result, Stream.Size);
		Stream.ReadBuffer(Pointer(Result)^, Length(Result));
	finally
		Stream.Free;
	end;
end;

function FileToBytes(const FileName: String; var Bytes: TBytes): Int64;
var
	Stream: TFileStream;
begin
	if not FileExists(Filename) then Exit(-1);

	Stream := TFileStream.Create(FileName, fmOpenRead);
	try
		try
			SetLength(Bytes, Stream.Size);
			Stream.ReadBuffer(Bytes[0], Stream.Size);
			Result := Stream.Size;
		except
			Result := -1;
		end;
	finally
		Stream.Free;
	end;
end;

{ TStreamEx }

constructor TStreamEx.Create(const AStream: TStream);
begin
	inherited Create;
	Stream := AStream;
	//Bytes := nil;
end;

destructor TStreamEx.Destroy;
begin
	Stream.Free;
	inherited Destroy;
end;

function TStreamEx.Read8: Byte;
begin
	Result := Stream.ReadByte;
end;

function TStreamEx.Read16: Word;
begin
	Result := LEtoN(Stream.ReadWord);
end;

function TStreamEx.Read16R: Word;
begin
	Result := BEtoN(Stream.ReadWord);
end;

function TStreamEx.Read32: Cardinal;
begin
	Result := LEtoN(Stream.ReadDWord);
end;

function TStreamEx.Read32R: Cardinal;
begin
	Result := BEtoN(Stream.ReadDWord);
end;

function TStreamEx.Read64: QWord;
begin
	Result := LEtoN(Stream.ReadQWord);
end;

function TStreamEx.Read64R: QWord;
begin
	Result := BEtoN(Stream.ReadQWord);
end;

function TStreamEx.ReadString(StopAtZero: Boolean; MaxLen: Word): AnsiString;
begin
	Result := StreamReadString(Stream, StopAtZero, MaxLen);
end;

{function TStreamEx.ReadData: Int64;
begin
	if Assigned(Bytes) then Bytes.Free;
	Bytes := TByteDataReader.Create;
	Bytes.LoadFromStream(Stream);
end;}

function TStreamEx.GetCRC32(StartPos: Int64 = -1; Len: Int64 = -1): Cardinal;
begin
	Result := StreamGetCRC32(Stream, StartPos, Len);
end;

function TStreamEx.Pos: QWord;
begin
	Result := Stream.Position;
end;

function TStreamEx.Size: QWord;
begin
	Result := Stream.Size;
end;

procedure TStreamEx.Skip(Len: Int64);
begin
	Stream.Seek(Len, soCurrent);
end;

procedure TStreamEx.SeekTo(Offset: Int64);
begin
	Stream.Seek(Offset, soBeginning);
end;

{ TByteDataReader }

function TByteDataReader.LoadFromFile(const Filename: String): Int64;
var
	Stream: TFileStream;
begin
	Stream := TFileStream.Create(FileName, fmOpenRead);
	Result := LoadFromStream(Stream);
	Stream.Free;
end;

function TByteDataReader.LoadFromStream(const Stream: TStream): Int64;
var
	P: Int64;
begin
	Size := Stream.Size;
	P := Stream.Position;
	SetLength(Data, Size);
	Stream.Seek(0, soFromBeginning);
	Stream.Read(Data[0], Size);
	Stream.Seek(P, soFromBeginning);
	Pos := 0;
	Result := Size;
end;

function TByteDataReader.Read8: Byte;
begin
	if Pos > Size then
		Result := 0
	else
		Result := Data[Pos];
	Skip(1);
end;

function TByteDataReader.Read16: Word;
begin
	if (Pos+1) > Size then
		Result := 0
	else
		Result := Data[Pos+1] shl 8 + Data[Pos];
	Skip(2);
end;

function TByteDataReader.Read16R: Word;
begin
	if (Pos+1) > Size then
		Result := 0
	else
		Result := Data[Pos] shl 8 + Data[Pos+1];
	Skip(2);
end;

function TByteDataReader.Read24: Cardinal;
begin
	if (Pos+2) > Size then
		Result := 0
	else
		Result := Data[Pos+2] shl 16 + Data[Pos+1] shl 8 + Data[Pos];
	Skip(3);
end;

function TByteDataReader.Read24R: Cardinal;
begin
	if (Pos+2) > Size then
		Result := 0
	else
		Result := Data[Pos] shl 16 + Data[Pos+1] shl 8 + Data[Pos+2];
	Skip(3);
end;

function TByteDataReader.Read32: Cardinal;
begin
	if (Pos+3) > Size then
		Result := 0
	else
		Result := Data[Pos+3] shl 24 + Data[Pos+2] shl 16 + Data[Pos+1] shl 8 + Data[Pos];
	Skip(4);
end;

function TByteDataReader.Read32R: Cardinal;
begin
	if (Pos+3) > Size then
		Result := 0
	else
		Result := Data[Pos] shl 24 + Data[Pos+1] shl 16 + Data[Pos+2] shl 8 + Data[Pos+3];
	Skip(4);
end;

procedure TByteDataReader.Skip(Len: Int64);
begin
	Pos := Min(Size, Pos + Len);
end;

procedure TByteDataReader.SeekTo(Offset: Int64);
begin
	Pos := Min(Size, Offset);
end;

function TByteDataReader.RLEDecode(NumBytes: Integer = -1): TMemoryStream;
var
	b: Byte;
	len: Integer;
	TargetSize: Cardinal;
begin
	if NumBytes < 0 then
		NumBytes := Size - Pos;
	if Numbytes < 6 then Exit(nil);

	Result := TMemoryStream.Create;

	TargetSize := Read32;

	while Result.Size < TargetSize do
	begin
		b := Read8;
		Dec(NumBytes);
		if b = RLE_ESCAPE then
		begin
			// decode run
			len := Read8; // how many bytes to fill
			if len = RLE_ESCAPE then
				Result.WriteByte(RLE_ESCAPE) // just write byte $FF
			else
			begin
				b := Read8; // byte to fill with
				while len > 0 do
				begin
					Result.WriteByte(b);
					Dec(len);
				end;
			end;
		end
		else
			Result.WriteByte(b);
	end;
	Result.Seek(0, soFromBeginning);
end;

{ TFileStreamEx }

// ============================================================================
// Read
// ============================================================================

function TFileStreamEx.ReadBool: Boolean;
begin
	Result := ReadByte <> 0;
end;

function TFileStreamEx.Read8: Byte;
begin
	if Position >= Size then
		Result := 0
	else
		Result := ReadByte;
end;

function TFileStreamEx.Read16: Word;
begin
	Result := LEtoN(ReadWord);
end;

function TFileStreamEx.Read16R: Word;
begin
	Result := BEtoN(ReadWord);
end;

function TFileStreamEx.Read24: Cardinal;
begin
	Result := LEtoN(ReadDWord) and $FFFFFF;
	Seek(-1, soFromCurrent);
end;

function TFileStreamEx.Read24R: Cardinal;
begin
	Result := BEtoN(ReadDWord) and $FFFFFF;
	Seek(-1, soFromCurrent);
end;

function TFileStreamEx.Read32: Cardinal;
begin
	Result := LEtoN(ReadDWord);
end;

function TFileStreamEx.Read32R: Cardinal;
begin
	Result := BEtoN(ReadDWord);
end;

function TFileStreamEx.Read64: QWord;
begin
	Result := LEtoN(ReadQWord);
end;

function TFileStreamEx.Read64R: QWord;
begin
	Result := BEtoN(ReadQWord);
end;

function TFileStreamEx.ReadString(StopAtZero: Boolean; MaxLen: Word = 0): AnsiString;
begin
	Result := StreamReadString(Self, StopAtZero, MaxLen);
end;

function TFileStreamEx.ReadData: Int64;
{var
	P: Int64;}
begin
	if Assigned(Bytes) then Bytes.Free;
	Bytes := TByteDataReader.Create;
	Bytes.LoadFromStream(Self);
	Result := Size;
	{P := Position;
	SetLength(Data, Result+1);
	SeekTo(0);
	Read(Data[0], Result);
	SeekTo(P);}
end;

// ============================================================================
// Write
// ============================================================================

procedure TFileStreamEx.WriteBool(Val: Boolean);
begin
	WriteByte(Byte(Val));
end;

procedure TFileStreamEx.Write8(Val: Byte);
begin
	WriteByte(Val);
end;

procedure TFileStreamEx.Write8(Val: Char);
begin
	WriteByte(Ord(Val));
end;

procedure TFileStreamEx.Write16(Val: Word);
begin
	WriteWord(NtoLE(Val));
end;

procedure TFileStreamEx.Write16R(Val: Word);
begin
	WriteWord(NtoBE(Val));
end;

procedure TFileStreamEx.Write32(Val: Cardinal);
begin
	WriteDWord(NtoLE(Val));
end;

procedure TFileStreamEx.Write32R(Val: Cardinal);
begin
	WriteDWord(NtoBE(Val));
end;

procedure TFileStreamEx.Write64(Val: QWord);
begin
	WriteQWord(NtoLE(Val));
end;

procedure TFileStreamEx.Write64R(Val: QWord);
begin
	WriteQWord(NtoBE(Val));
end;

procedure TFileStreamEx.WriteString(const Val: AnsiString; Terminate: Boolean = False);
begin
	StreamWriteString(Self, Val, Terminate);
end;

procedure TFileStreamEx.WriteData;
begin
	if System.Length(Bytes.Data) > 0 then
		Write(Bytes.Data[0], System.Length(Bytes.Data));
end;

procedure TFileStreamEx.Skip(Len: Int64);
begin
	Seek(Len, soCurrent);
end;

procedure TFileStreamEx.SeekTo(Offset: Int64);
begin
	Seek(Offset, soBeginning);
end;

destructor TFileStreamEx.Destroy;
begin
	if Assigned(Bytes) then
		Bytes.Free;
	inherited;
end;

end.

