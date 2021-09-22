unit Graphics32.JPG;

{$mode objfpc}

interface

uses
	SysUtils, Classes,
	FPImage, FPReadJPEG,
	Graphics32;

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

	TJPGImageItem = TFPCompactImgBGRA8Bit;

	function JPEGFromFile(const Filename: String): TBitmap32;
	function JPEGFromStream(Stream: TStream): TBitmap32;


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


function JPEGFromFile(const Filename: String): TBitmap32;
var
	JPG: TJPGImageItem;
begin
	JPG := TJPGImageItem.Create(0, 0);
	Result := nil;
	try
		try
			if JPG.LoadFromFile(Filename) then
			begin
				Result := TBitmap32.Create(JPG.Width, JPG.Height);
				Move(JPG.Data^, Result.Bits[0], JPG.Width * JPG.Height * 4);
			end;
		except
		end;
	finally
		JPG.Free;
	end;
end;

function JPEGFromStream(Stream: TStream): TBitmap32;
var
	JPG: TJPGImageItem;
begin
	JPG := TJPGImageItem.Create(0, 0);
	Result := nil;
	try
		try
			JPG.LoadFromStream(Stream);
			if JPG.Width > 0 then
			begin
				Result := TBitmap32.Create(JPG.Width, JPG.Height);
				Move(JPG.Data^, Result.Bits[0], JPG.Width * JPG.Height * 4);
			end;
		except
		end;
	finally
		JPG.Free;
	end;
end;

end.

