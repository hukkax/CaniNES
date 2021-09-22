unit basement.fonts;

{$mode delphi}
{$I basement.inc}

interface

uses
	Classes, SysUtils,
	Basement.Window, Basement.Sprites,
	Megabot.Graphics,
	SDL2, Graphics32;

type
	TBitmapFont = class(TTexture)
	private
	public
		GlyphSize:	TPoint;
		Kerning:	ShortInt;

		constructor Create; overload;

		procedure	ApplyEffect(Effect: TBitmapEffect; Color: TColor32);

		procedure	LoadFromFile(const Filename: String); override;
		procedure	Print(X, Y: Integer; const Text: AnsiString);
	end;

implementation

{ TBitmapFont }

constructor TBitmapFont.Create;
begin
	inherited Create(GameRenderer, 0, 0);

	Kerning := 0;
end;

procedure TBitmapFont.LoadFromFile(const Filename: String);
begin
	inherited LoadFromFile(Filename);

	UseAlphaChannel := True;
	ReplaceColor(PixelS[0,0], 0);

	GlyphSize := Point(Width div 16, Height div 16);
	FDestRect.w := GlyphSize.X;
	FDestRect.h := GlyphSize.Y;
end;

procedure TBitmapFont.ApplyEffect(Effect: TBitmapEffect; Color: TColor32);
var
	Res, GlyphBuf, Tmp: TBitmap32;
	X, Y, SX, SY: Integer;
begin
	case Effect of
		fxDropShadow:	begin X := 1; Y := 1; end;
		fxOutline:		begin X := 2; Y := 2; end;
		else			begin X := 0; Y := 0; end;
	end;

	SX := GlyphSize.X + X;
	SY := GlyphSize.Y + Y;
	Kerning -= X;

	Res := TBitmap32.Create;
	Res.SetSize(Width, Height);
	Res.Draw(0, 0, Self);

	Tmp := TBitmap32.Create;
	Tmp.SetSize(GlyphSize.X, GlyphSize.Y);

	SetSize(SX * 16, SY * 16);

	for Y := 0 to 15 do
	for X := 0 to 15 do
	begin
		Tmp.Clear(0);
		Tmp.Draw(0, 0, Bounds(X * GlyphSize.X, Y * GlyphSize.Y, GlyphSize.X, GlyphSize.Y), Res);

		GlyphBuf := BitmapEffect(Tmp, Effect, Color);
		Draw(X * SX, Y * SY, GlyphBuf);
		GlyphBuf.Free;
	end;

	GlyphSize   := Point(SX, SY);
	FDestRect.w := SX;
	FDestRect.h := SY;

	Tmp.Free;
	Res.Free;

	CreateTexture;
end;

procedure TBitmapFont.Print(X, Y: Integer; const Text: AnsiString);
var
	i, G: Integer;
	SR: TSDL_Rect;
begin
	if Dirty then
	begin
		SDL_UpdateTexture(Texture, nil, @Bits[0], Width * 4);
		Dirty := False;
	end;

	SR.w := GlyphSize.X;
	SR.h := GlyphSize.Y;

	FDestRect.x := X;
	FDestRect.y := Y;

	for i := 1 to Length(Text) do
	begin
		G := Ord(Text[i]);
		SR.x := G mod 16 * GlyphSize.X;
		SR.y := G div 16 * GlyphSize.Y;
		FDestRect.x += GlyphSize.X + Kerning;
		SDL_RenderCopy(Renderer, Texture, @SR, @FDestRect);
	end;
end;

end.

