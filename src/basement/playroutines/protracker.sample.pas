unit ProTracker.Sample;

interface

uses
	FileStreamEx;

const
	MAX_IMPORTED_SAMPLESIZE = 6; // load max. 6 megabytes of 8-bit mono sample data

	// Bit width (8 bits for simplicity)
	ST_BIT_MASK	= $000000FF;
	ST_8		= 8;		// 8-bit sample data
	ST_16		= 16;		// 16-bit sample data

	// Channels (4 bits)
	ST_CHN_MASK	= $00000F00;
	ST_MONO		= 0 shl 8;	// mono sample
	ST_STEREO 	= 1 shl 8;	// stereo sample

	// Endianness (4 bits)
	ST_END_MASK = $0000F000;
	ST_LE 		= 0 shl 12;	// little-endian byte order
	ST_BE 		= 1 shl 12;	// big-endian byte order

	// Encoding (8 bits)
	ST_ENC_MASK = $00FF0000;
	ST_UNSIGNED	= 0 shl 16;	// PCM, signed
	ST_SIGNED	= 1 shl 16;	// PCM, unsigned
	ST_DELTA	= 2 shl 16;	// PCM, delta encoded
	ST_IT214 	= 3 shl 16;	// Impulse Tracker 2.14 compressed
	ST_IT215 	= 4 shl 16;	// Impulse Tracker 2.15 compressed

	// Default: 8-bit signed PCM data
	RS_PCM8S	= (ST_SIGNED or ST_8 or ST_MONO);
	// 8-bit unsigned PCM data
	RS_PCM8U	= (ST_UNSIGNED or ST_8 or ST_MONO);
	// 8-bit ADPCM data with linear table
	RS_PCM8D	= (ST_DELTA or ST_8 or ST_MONO);
	// 16-bit ADPCM data with linear table
	RS_PCM16D	= (ST_DELTA or ST_16 or ST_MONO);
	// 16-bit signed PCM data
	RS_PCM16S	= (ST_SIGNED or ST_16 or ST_MONO);
	// 16-bit signed mono PCM motorola byte order
	RS_PCM16M	= (ST_SIGNED or ST_16 or ST_MONO or ST_BE);
	// 16-bit unsigned PCM data
	RS_PCM16U	= (ST_UNSIGNED or ST_16 or ST_MONO);
	// 16-bit signed stereo big endian
	RS_STPCM16M	= (ST_SIGNED or ST_16 or ST_STEREO or ST_BE);
	// 8-bit stereo samples
	RS_STPCM8S	= (ST_SIGNED   or ST_8 or ST_STEREO);
	RS_STPCM8U	= (ST_UNSIGNED or ST_8 or ST_STEREO);
	RS_STPCM8D	= (ST_DELTA    or ST_8 or ST_STEREO);
	// 16-bit stereo samples
	RS_STPCM16S	= (ST_SIGNED   or ST_16 or ST_STEREO);
	RS_STPCM16U	= (ST_UNSIGNED or ST_16 or ST_STEREO);
	RS_STPCM16D	= (ST_DELTA    or ST_16 or ST_STEREO);
	// IT compressed samples
	RS_IT2148	= (ST_IT214 or ST_8  or ST_MONO);
	RS_IT21416	= (ST_IT214 or ST_16 or ST_MONO);
	RS_IT2158	= (ST_IT215 or ST_8  or ST_MONO);
	RS_IT21516	= (ST_IT215 or ST_16 or ST_MONO);
	RS_IT2148S	= (ST_IT214 or ST_8  or ST_STEREO);
	RS_IT21416S	= (ST_IT214 or ST_16 or ST_STEREO);
	RS_IT2158S	= (ST_IT215 or ST_8  or ST_STEREO);
	RS_IT21516S	= (ST_IT215 or ST_16 or ST_STEREO);

type
	TSampleFormat = ( SamFmtRAW, SamFmtIFF, SamFmtFromExt );

	TSample = class
	public
		Data:			packed array of Byte;
		Name:			packed array [0..21] of AnsiChar;
		Finetune:		ShortInt;
		Volume:			Byte;
		Length:			Cardinal;		// data length in words
		LoopStart,
		LoopLength,
		TempLoopStart,
		TempLoopLength,
		tmpLoopStart:	Cardinal;		// positions in words
		Age: 			ShortInt;
		FileOffset:		Cardinal;
		Index:			Byte; 			// 1-based!

		function 		IsEmpty: Boolean; inline;
		function 		IsLooped: Boolean; inline;
		function		IsDuplicateOf(const Other: TSample): Boolean;

		constructor 	Create;
		procedure 		Validate;
		procedure		ValidateCoords(out X1, X2: Integer);
		function 		ByteLength: Cardinal; inline;
		procedure		Assign(const Source: TSample);

		procedure 		LoadData(var ModFile: TStreamEx;
						NumSamples: Cardinal; Flags: Cardinal = RS_PCM8S);
		function		LoadFromFile(const Filename: String): Boolean;

		function 		GetName: AnsiString;
		procedure		SetName(const S: AnsiString);
		function  		SetLoopStart(WordPos: Integer): Boolean;
		function  		SetLoopEnd(WordPos: Integer): Boolean;
		procedure 		Resize(NewSize: Cardinal); // size in bytes
		procedure		ZeroFirstWord;
		procedure 		Clear;

	end;

	function IsEmptySample(const Sam: TSample): Boolean; inline;

var
	LastSampleFormat: packed record
		Length:		Cardinal;
		isStereo,
		is16Bit: 	Boolean;
	end;

implementation

uses
	Math, Classes, SysUtils,
	ProTracker.Player;


function IsEmptySample(const Sam: TSample): Boolean;
begin
	if Sam = nil then
		Exit(True)
	else
		Result := (Sam.Length < 2);
end;

// ==========================================================================
// TSample
// ==========================================================================

constructor TSample.Create;
begin
	Clear;
end;

function TSample.ByteLength: Cardinal;
begin
	Result := System.Length(Data);
end;

function TSample.IsEmpty: Boolean;
begin
	Result := High(Data) <= 1;
end;

function TSample.IsLooped: Boolean;
begin
	Result := ((LoopLength + LoopStart) > 1);
end;

function TSample.IsDuplicateOf(const Other: TSample): Boolean;
var
	i: Integer;
begin
	Result := False;
	if (Other.Length <> Length) or (Other.Finetune <> Finetune) or (Other.Volume <> Volume) or
		(Other.LoopStart <> LoopStart) or (Other.LoopLength <> LoopLength) then Exit;
	for i := 0 to High(Data) do
		if Other.Data[i] <> Data[i] then Exit;
	Result := True;
end;

procedure TSample.ZeroFirstWord;
begin
	if (not IsLooped) and (not IsEmpty) then
	begin
		Data[0] := 0;
		Data[1] := 0;
	end;
end;

procedure TSample.ValidateCoords(out X1, X2: Integer);
var
	L: Integer;
begin
	L := High(Data);
	if ({%H-}X2 < {%H-}X1) or (X2 > L) then
		X2 := L;
	if (X1 < 0) or (X1 > L) then
		X1 := 0;
end;

function TSample.SetLoopStart(WordPos: Integer): Boolean;
begin
	WordPos := Max(WordPos, 0);
	WordPos := Min(WordPos, Length);
	if WordPos < (LoopStart + LoopLength) then
	begin
		LoopLength := (LoopStart + LoopLength) - WordPos;
		LoopStart  := WordPos;
		Result := True;
	end
	else
		Result := False;
end;

function TSample.SetLoopEnd(WordPos: Integer): Boolean;
begin
	WordPos := Max(WordPos, 0);
	WordPos := Min(WordPos, Length);
	if (WordPos >= 2) and (WordPos > LoopStart) then
	begin
		LoopLength := WordPos - LoopStart;
		Result := True;
	end
	else
		Result := False;
end;

procedure TSample.Resize(NewSize: Cardinal); // size in bytes
begin
	// TODO: clear new data on sample size increase
	if (NewSize mod 2) <> 0 then Inc(NewSize);
	SetLength(Data, NewSize);
	Length := NewSize div 2;
end;

function TSample.GetName: AnsiString;
var
	x: Integer;
begin
	Result := '';
	for x := 0 to 21 do
		if (Ord(Name[x]) >= 32) then
			Result := Result + Name[x]
		else
			Result := Result + ' ';
	Result := TrimRight(Result);
end;

procedure TSample.SetName(const S: AnsiString);
var
	x: Integer;
begin
	for x := 0 to 21 do
		Name[x] := #0;
	if S <> '' then
		for x := 1 to Min(System.Length(S), 21+1) do
			Name[x-1] := S[x];
end;

procedure TSample.Assign(const Source: TSample);
var
	X: Integer;
begin
	if (Source = nil) or (Source = Self) then Exit;

	Length := Source.Length;
	SetLength(Data, Source.ByteLength);
	for X := 0 to Source.ByteLength-1 do
		Data[X] := Source.Data[X];

	LoopStart  := Source.LoopStart;
	LoopLength := Source.LoopLength;

	for X := 0 to High(Name) do
		Name[X] := Source.Name[X];

	Finetune := Source.Finetune;
	Volume := Source.Volume;
	tmpLoopStart := Source.tmpLoopStart;
	Age := 0;
	FileOffset := Source.FileOffset;
end;

procedure TSample.Clear;
begin
	SetName('');
	SetLength(Data, 2);
	Length     := 0;
	LoopStart  := 0;
	LoopLength := 1;
	TempLoopStart  := LoopStart;
	TempLoopLength := LoopLength;
	Volume     := 64;
	Finetune   := 0;
	Age        := -1;
	ZeroFirstWord;
end;

procedure TSample.Validate;
var
	L: Word;
begin
	if ByteLength < (Length * 2) then
		SetLength(Data, Length * 2 + 1);

	L := Length and $FFFF;
	if LoopStart > L then 					// fix this stuff
		LoopStart := 0;
	if (LoopStart + LoopLength) > L then
		LoopLength := L - LoopStart;
	if LoopLength < 1 then
		LoopLength := 1;

	ZeroFirstWord;
end;

function TSample.LoadFromFile(const Filename: String): Boolean;
var
	FileAcc: TFileStreamEx;
	ID, sName: AnsiString;
	i: Integer;
	Len: Cardinal;
begin
	Result := False;

	if not FileExists(Filename) then
	begin
		//Log('File not found: %s', [Filename]);
		Exit;
	end;

	Self.Volume := 64;
	Self.Finetune := 0;
	Self.LoopStart  := 0;
	Self.LoopLength := 1;
	Self.SetName(ExtractFileName(Filename));

	FileAcc := TFileStreamEx.Create(Filename, fmOpenRead, fmShareDenyNone);

	ID := FileAcc.ReadString(False, 4);

	if ID = 'FORM' then	// IFF 8SVX
	begin
		// Should be the size of the file minus 4+4 ( 'FORM'+size )
		Len := FileAcc.Read32R;
		ID := FileAcc.ReadString(False, 4);
		if ID <> '8SVX' then Exit;
		i := 0;

		while (ID <> 'BODY') and (i < 30) do
		begin
			ID := FileAcc.ReadString(False, 4);
			Inc(i); // iterations
			Len := FileAcc.Read32R;

			if ID = 'VHDR' then
			begin
				// # samples in the high octave 1-shot part
				Self.LoopStart  := FileAcc.Read32R div 2;
				// # samples in the high octave repeat part
				Self.LoopLength := FileAcc.Read32R div 2;
				if Self.LoopLength < 1 then
					Self.LoopLength := 1;
				FileAcc.Read32;				// # samples/cycle in high octave, else 0
				FileAcc.Read16;				// samples per second
				FileAcc.Read8;				// # octaves of waveforms
				if FileAcc.Read8 <> 0 then	// # data compression technique used
					Exit;
				Self.Volume := Trunc((FileAcc.Read32R / 1024) + 0.5);	// playback volume
			end
			else
			if ID = 'BODY' then
			begin
				// 8-bit sample data
				Resize(Len);
				FileAcc.Read(Data[0], Len-1);

				if 	(Self.LoopStart > Self.Length) or
					((Self.LoopStart + Self.LoopLength) > Self.Length) then
				begin
					Self.LoopStart  := 0;
					Self.LoopLength := 1;
				end;
			end
			else
			if ID = 'NAME' then
			begin
				sName := FileAcc.ReadString(False, Len);
				Self.SetName(sName);
				if Len mod 2 = 1 then FileAcc.Read8;
			end
			else
			begin
				if (Len and 1) <> 0 then Inc(Len);	// padding
				// skip the remaining bytes of this chunk
				if (Len <> 0) then FileAcc.Skip(Len);
			end;
		end;
	end
	else
	begin				// read file as raw 8-bit mono sample data
		Len := FileAcc.Size;
		Resize(Len);
		FileAcc.Read(Data[0], Len-1);
	end;

	ZeroFirstWord;
	FileAcc.Free;

	LastSampleFormat.Length := Length;
	Result := True;
end;

procedure TSample.LoadData(var ModFile: TStreamEx;
	NumSamples: Cardinal; Flags: Cardinal);
var
	i: Integer;
	Data16: array of SmallInt;
begin
	SetLength(Data, NumSamples);
	case Flags of

		// 8-bit signed PCM data
		RS_PCM8S:
			ModFile.Stream.Read(Data[0], NumSamples);

		// 16-bit signed PCM data
		RS_PCM16S:
		begin
			SetLength(Data16, NumSamples);
			ModFile.Stream.Read(Data16[0], NumSamples);
			for i := 0 to NumSamples-1 do
				Data[i] := Word(Data16[i]) div 256;
		end;
	end;
end;


end.
