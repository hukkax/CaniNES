unit IPSpatcher;

{$mode delphi}

interface

uses
	Classes, SysUtils;

type
	TIpsPatcher = class
	public
		class function PatchBuffer(ipsFilepath: String; const input: TBytes; out output: TBytes): Boolean; overload; static;
		class function PatchBuffer(const ipsData, input: TBytes; out output: TBytes): Boolean; overload; static;
		class function PatchBuffer(ipsFile: TStream; const input: TBytes; out output: TBytes): Boolean; overload; static;

		class function CreatePatch(originalData, newData: TBytes): TBytes; static;
	end;

	TIpsRecord = class
	public
		Address: Cardinal;
		Length: Word;
		Replacement: TBytes;

		// For RLE records (when length = 0)
		RepeatCount: Word;
		Value: Byte;

		function  ReadRecord(ipsFile: TStream): Boolean;
		procedure WriteRecord(var output: TBytes);
	end;


implementation

uses
	Basement.Util, Generics.Collections;

const
	S_EOF  : AnsiString = 'EOF';
	S_PATCH: AnsiString = 'PATCH';

{ TIpsRecord }

function TIpsRecord.ReadRecord(ipsFile: TStream): Boolean;
var
	buffer: array[0..2] of Byte;

	procedure DoReadBuffer(L: Cardinal); inline;
	begin
		ipsFile.ReadBuffer(buffer[0], L);
	end;

begin
	Address := 0;

	DoReadBuffer(3);
	if StringInData(@buffer[0], S_EOF) then
		Exit(False);

	Address := (buffer[2]) or (buffer[1] shl 8) or (buffer[0] shl 16);

	DoReadBuffer(2);
	Length := (buffer[1]) or (buffer[0] shl 8);

	if Length = 0 then // RLE record
	begin
		DoReadBuffer(3);
		RepeatCount := (buffer[1]) or (buffer[0] shl 8);
		Value := buffer[2];
	end
	else
	begin
		SetLength(Replacement, Length);
		try
			ipsFile.ReadBuffer(Replacement[0], Length);
		except
		end;
	end;

	Result := True;
end;

procedure TIpsRecord.WriteRecord(var output: TBytes);
begin
	output := output + [
		(Address shr 16) and $FF, (Address shr 8) and $FF, Address and $FF,
		(Length shr 8) and $FF, Length and $FF
	];

	if Length = 0 then
		output := output + [
			(RepeatCount shr 8) and $FF,
			RepeatCount and $FF,
			Value
		]
	else
		output := output + Replacement;
end;

{ TIpsPatcher }

class function TIpsPatcher.PatchBuffer(ipsFilepath: String; const input: TBytes; out output: TBytes): Boolean;
var
	ipsFile: TFileStream;
begin
	Result := False;
	try
		ipsFile := TFileStream.Create(ipsFilepath, fmOpenRead);
		Result := PatchBuffer(ipsFile, input, output);
	finally
		ipsFile.Free;
	end;
end;

class function TIpsPatcher.PatchBuffer(const ipsData, input: TBytes; out output: TBytes): Boolean;
var
	ss: TMemoryStream;
begin
	try
		ss := TMemoryStream.Create;
		ss.WriteBuffer(ipsData[0], Length(ipsData));
		ss.Seek(0, soFromBeginning);
		Result := PatchBuffer(ss, input, output);
	finally
		ss.Free;
	end;
end;

class function TIpsPatcher.PatchBuffer(ipsFile: TStream; const input: TBytes; out output: TBytes): Boolean;
var
	maxOutputSize: Int64;
	truncateOffset: Integer = -1;
	header: array[0..4] of AnsiChar;
	buffer: array[0..2] of Byte;
	rec: TIpsRecord;
	records: TObjectList<TIpsRecord>;
begin
	ipsFile.ReadBuffer({%H-}header[0], Length(header));
	if header <> S_PATCH then
		Exit(False); // Invalid ips file

	maxOutputSize := Length(input);
	records := TObjectList<TIpsRecord>.Create(True);

	while ipsFile.Position < ipsFile.Size do
	begin
		rec := TIpsRecord.Create;
		if rec.ReadRecord(ipsFile) then
		begin
			if Int64(rec.Address + rec.Length + rec.RepeatCount) > maxOutputSize then
				maxOutputSize := Int64(rec.Address + rec.Length + rec.RepeatCount);
			records.Add(rec);
		end
		else
		begin
			rec.Free;
			// EOF, try to read truncate offset record if it exists
			try
				ipsFile.ReadBuffer(buffer[0], Length(buffer));
			except
			end;
			if ipsFile.Position < ipsFile.Size then
				truncateOffset := (buffer[2]) or (buffer[1] shl 8) or (buffer[0] shl 16);
			Break;
		end;
	end;

	output := Copy(input);
	SetLength(output, maxOutputSize);

	for rec in records do
	begin
		if rec.Length = 0 then
			FillByte(output[rec.Address], rec.RepeatCount, rec.Value)
		else
			CopyMemory(@output[rec.Address], @rec.Replacement[0], Length(rec.Replacement));
			//std.copy(rec.Replacement.begin, rec.Replacement.end, output.begin+rec.Address);
	end;

	if (truncateOffset <> -1) and (Length(output) > truncateOffset) then
		SetLength(output, truncateOffset);

	records.Free;
	Result := True;
end;

class function TIpsPatcher.CreatePatch(originalData, newData: TBytes): TBytes;
var
	i: Cardinal = 0;
	len: Cardinal;
	rleByte, rleCount: Byte;
	createRleRecord: Boolean;
	patchRecord: TIpsRecord;
begin
	len := Length(originalData);
	//Assert(len = Length(newData));

	Result := BytesOf(S_PATCH);

	while i < len do
	begin
		while (i < len) and (originalData[i] = newData[i]) do
			Inc(i);

		if i < len then
		begin
			patchRecord := TIpsRecord.Create;

			rleByte := newData[i];
			rleCount := 0;
			createRleRecord := False;

			patchRecord.Address := i;
			patchRecord.Length := 0;

			while (i < len) and (patchRecord.Length < 65535) and (originalData[i] <> newData[i]) do
			begin
				if newData[i] = rleByte then
					Inc(rleCount)
				else
				if createRleRecord then
					Break
				else
				begin
					rleByte := newData[i];
					rleCount := 1;
				end;

				Inc(patchRecord.Length);
				Inc(i);

				if (rleCount > 13) or ((patchRecord.Length = rleCount) and (rleCount > 3)) then
				begin
					// Making a RLE entry would probably save space, so write the current entry and create a RLE entry after it
					if patchRecord.Length = rleCount then
						//Same character since the start of this entry, make the RLE entry now
						createRleRecord := True
					else
					begin
						Dec(patchRecord.Length, rleCount);
						Dec(i, rleCount);
						Break;
					end;
				end;
			end;

			if createRleRecord then
			begin
				patchRecord.Length := 0;
				patchRecord.RepeatCount := rleCount;
				patchRecord.Value := rleByte;
			end
			else
			begin
				SetLength(patchRecord.Replacement, patchRecord.Length);
				CopyMemory(@patchRecord.Replacement[0],
					@newData[patchRecord.Address], patchRecord.Length);
			end;

			patchRecord.WriteRecord(Result);
			patchRecord.Free;
		end;
	end;

	Result := Result + BytesOf(S_EOF);
end;

end.

