unit Basement.MJPEG;

(* An extremely naïve implementation of an MJPEG loader. Not really intended for any
 * serious use. Uses the native JPEG loader so might need prebuffering or use of a
 * more optimized decoder such as libjpeg-turbo for realtime use on slower systems.
 *
 * MJPEGs are basically a bunch of JPEG images concatenated into a file, optionally
 * within a container such as AVI. This should load both raw MJPEGs and ones within
 * a container, provided that each frame (JPEG) can be decoded separately without
 * relying on any data from the previous frame(s).
 * However, any metadata in the container is ignored and there is no notion of
 * framerate.
 *
 * -hukka Dec 2018
 *)

{$mode objfpc}
//{$mode delphi}

interface

uses
	Classes, SysUtils, Generics.Collections, FPImage, FPReadJPEG, Graphics32;

type
	TFPCompactImgBGRA8BitValue = packed record
		b, g, r, a: Byte;
	end;
	PFPCompactImgBGRA8BitValue = ^TFPCompactImgBGRA8BitValue;

	TFPCompactImgBGRA8Bit = class(TFPCompactImgBase)
	private
		FDesc: TFPCompactImgDesc;
	protected
		FData:      PFPCompactImgBGRA8BitValue;
		procedure   SetInternalColor(X, Y: Integer; const Value: TFPColor); override;
	public
		property    Data: PFPCompactImgBGRA8BitValue read FData;
		procedure   SetSize(AWidth, AHeight: Integer); override;
		constructor Create(AWidth, AHeight: Integer); override;
		destructor  Destroy; override;
	end;

	TImageItem = TFPCompactImgBGRA8Bit;

	TMJPEG = class
	private
		CurrentFrame:   Cardinal;
		Stream:         TFileStream;
		JPG:            TImageItem;
		FrameOffsets:   specialize TList<Cardinal>;
		InternalBuffer: TBitmap32;
	public
		function  LoadFromFile(const Filename: String): Boolean;

		procedure Advance(Bitmap: TBitmap32);
		procedure RenderFrame(FrameNum: Cardinal; Bitmap: TBitmap32);

		constructor Create; overload;
		destructor  Destroy; override;
	end;

implementation

{ TFPCompactImgRGBA8Bit }

procedure TFPCompactImgBGRA8Bit.SetInternalColor(X, Y: Integer; const Value: TFPColor);
var
	v: TFPCompactImgBGRA8BitValue;
begin
	v.a := 255; //Value.alpha shr 8;
	v.r := Value.red   shr 8;
	v.g := Value.green shr 8;
	v.b := Value.blue  shr 8;
	FData[x + y * Width] := v;
end;

constructor TFPCompactImgBGRA8Bit.Create(AWidth, AHeight: Integer);
begin
	FDesc := GetFPCompactImgDesc(False, 8, True);
	inherited Create(AWidth, AHeight);
end;

destructor TFPCompactImgBGRA8Bit.Destroy;
begin
	ReAllocMem(FData, 0);
	inherited Destroy;
end;

procedure TFPCompactImgBGRA8Bit.SetSize(AWidth, AHeight: integer);
begin
	if (AWidth = Width) and (AHeight = Height) then Exit;
	ReAllocMem(FData, SizeOf(TFPCompactImgBGRA8BitValue) * AWidth * AHeight);
	inherited SetSize(AWidth, AHeight);
end;

{ TMJPEG }

function TMJPEG.LoadFromFile(const Filename: String): Boolean;
var
	i, FileSize, PosStart: Integer;
	Data: array of Byte;
begin
	Log('TMJPEG.LoadFromFile: ', Filename);

	Result := False;

	if JPG <> nil then
	begin
		FreeAndNil(JPG);
		Stream.Free;
	end;

	try
		Stream := TFileStream.Create(Filename, fmOpenRead);
	except
		Stream.Free;
		Exit;
	end;

	FrameOffsets.Clear;

	FileSize := Stream.Size;
	SetLength(Data, FileSize+1);
	Stream.Read(Data[0], FileSize);

	PosStart := -1;
	i := 0;

	while i < FileSize do
	begin
		if Data[i] = $FF then
		begin
			if (Data[i+1] = $D8) and (Data[i+2] = $FF) then
			begin
				PosStart := i;
				Inc(i, 2);
			end
			else
			if (PosStart >= 0) and (Data[i+1] = $D9) then
			begin
				FrameOffsets.Add(PosStart);
				PosStart := -1;
				Inc(i);
			end;
		end;
		Inc(i);
	end;

	Stream.Seek(0, soFromBeginning);

	JPG := TImageItem.Create(0, 0);

	CurrentFrame := 0;
	Result := True;

	Log('File processed.');
end;

procedure TMJPEG.Advance(Bitmap: TBitmap32);
begin
	RenderFrame(CurrentFrame, Bitmap);
	Inc(CurrentFrame);
	if CurrentFrame >= FrameOffsets.Count then
		CurrentFrame := 0;
end;

procedure TMJPEG.RenderFrame(FrameNum: Cardinal; Bitmap: TBitmap32);
begin
	if FrameNum >= FrameOffsets.Count then FrameNum := 0;
	if (JPG <> nil) and (Stream <> nil) then
	begin
//	Log('Frame ', FrameNum, ' at ', FrameOffsets[FrameNum]);
			Stream.Seek(FrameOffsets[FrameNum], soFromBeginning);

//	Log('Load jpg');
		try
			JPG.LoadFromStream(Stream);
		except
			on E:Exception do
			begin
				Log('Exception: ', E.Message);
				Exit;
			end;
		end;

//	Log('setsize ', jpg.width, ', ', jpg.height);
		InternalBuffer.SetSize(JPG.Width, JPG.Height);

//	Log('assign');
		Move(JPG.Data^, InternalBuffer.Bits[0], JPG.Width * JPG.Height * 4);
		Bitmap.Draw(0, 0, InternalBuffer);
	end;
end;

constructor TMJPEG.Create;
begin
	inherited;

	JPG := nil;
	Stream := nil;
	FrameOffsets := specialize TList<Cardinal>.Create;
	InternalBuffer := TBitmap32.Create;
	CurrentFrame := 0;
end;

destructor TMJPEG.Destroy;
begin
	InternalBuffer.Free;
	JPG.Free;
	FrameOffsets.Free;
	Stream.Free;

	inherited;
end;

end.

