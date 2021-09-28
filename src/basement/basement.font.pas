unit Basement.Font;

{$MODE DELPHI}

{$I basement.inc}

interface

uses
	Classes, SysUtils,
	{$IFDEF RENDER_FONT_FREETYPE}
	Graphics32.FreeType, LazUTF8,
	{$ENDIF}
	Graphics32;

type
	{$IFNDEF RENDER_FONT_FREETYPE}
	TFreeTypeAlignment = (ftaLeft,ftaCenter,ftaRight,ftaJustify,ftaTop,ftaVerticalCenter,ftaBaseline,ftaBottom);
	TFreeTypeAlignments = set of TFreeTypeAlignment;
	{$ENDIF}

	TRendererFont = class
	protected
		Filename:    String;
	public
		Loaded:      Boolean;

		GlyphWidth,
		GlyphHeight: Byte;

		function  LoadFromFile(const Filename: String): Boolean; virtual; abstract;
		function  TextWidth(const Text: String): Word; virtual; abstract;

		procedure DrawGlyph(Buffer: TBitmap32; G: Char; X, Y: Word; Col: TColor32); virtual; abstract;
		procedure DrawString(Buffer: TBitmap32; X, Y: Integer; const S: String; Col: TColor32; MaxWidth: Integer = -1); virtual; abstract;
		procedure DrawStringCentered(Buffer: TBitmap32; Y: Word; const S: String; Col: TColor32); virtual; abstract;
		procedure DrawTextRect(Buffer: TBitmap32; const S: String; R: TRect; Col: TColor32; Align: TFreeTypeAlignments); virtual; abstract;

		constructor Create(const Filename: String); virtual;
		destructor  Destroy; override;
	end;

	{$IFDEF RENDER_FONT_BITMAP}
	TRendererFontBitmap = class(TRendererFont)
	public
		MultiLine: Boolean;
		Font:      TBitmap32;

		function  LoadFromFile(const Filename: String): Boolean; override;
		function  TextWidth(const Text: String): Word; override;

		procedure DrawGlyph(Buffer: TBitmap32; G: Char; X, Y: Word; Col: TColor32); override;
		procedure DrawString(Buffer: TBitmap32; X, Y: Integer; const S: String; Col: TColor32; MaxWidth: Integer = -1); override;
		procedure DrawStringCentered(Buffer: TBitmap32; Y: Word; const S: String; Col: TColor32); override;

		constructor Create(const Filename: String); override;
		destructor  Destroy; override;
	end;
	{$ENDIF}

	{$IFDEF RENDER_FONT_FREETYPE}
	TRendererFontFreeType = class(TRendererFont)
	public
		Font: TFreeTypeFont;

		FontOffsetY,
		FontKerning: Integer;

		function  LoadFromFile(const Filename: String): Boolean; override;
		function  TextWidth(const Text: String): Word; override;

		procedure DrawGlyph(Buffer: TBitmap32; G: Char; X, Y: Word; Col: TColor32); override;
		procedure DrawString(Buffer: TBitmap32; X, Y: Integer; const S: String; Col: TColor32; MaxWidth: Integer = -1); override;
		procedure DrawStringCentered(Buffer: TBitmap32; Y: Word; const S: String; Col: TColor32); override;
		procedure DrawTextRect(Buffer: TBitmap32; const S: String; R: TRect; Col: TColor32; Align: TFreeTypeAlignments); override;

		constructor Create(const Filename: String); override;
		destructor  Destroy; override;
	end;
	{$ENDIF}


implementation

uses
	Basement.Util;

// ================================================================================================
// TRendererFont
// ================================================================================================

constructor TRendererFont.Create(const Filename: String);
begin
	inherited Create;

	GlyphWidth  := 2;
	GlyphHeight := 2;
end;

destructor TRendererFont.Destroy;
begin
	inherited Destroy;
end;

{$IFDEF RENDER_FONT_BITMAP}

// ================================================================================================
// TRendererFontBitmap
// ================================================================================================

constructor TRendererFontBitmap.Create(const Filename: String);
begin
	inherited Create(Filename);
end;

destructor TRendererFontBitmap.Destroy;
begin
	inherited Destroy;
end;

function TRendererFontBitmap.LoadFromFile(const Filename: String): Boolean;
var
	i: Integer;
	P: PColor32;
begin
	Font.Free;
	Font := TBitmap32.Create(256*GlyphWidth, GlyphHeight);

	Result := FileExists(Filename);

	if Result then
	begin
		Font.LoadFromFile(Filename);
		Font.ReplaceColor(Font.Pixel[0,0], 0);

		P := Font.PixelPtr[0, 0];
		for i := 0 to Font.Height*Font.Width-1 do
		begin
			if P^ <> 0 then
				P^ := Gray32(P^ and $FF, P^ and $FF);
			Inc(P);
		end;

		if Font.Width >= (256 * 2) then
		begin
			GlyphWidth  := Font.Width div 256;
			GlyphHeight := Font.Height;
			MultiLine := False;
		end
		else
		begin
			GlyphWidth  := Font.Width  div 16;
			GlyphHeight := Font.Height div 16;
			MultiLine := True;
		end;
	end;

	Loaded := Result;
end;

procedure TRendererFontBitmap.DrawGlyph(Buffer: TBitmap32; G: Char; X, Y: Word; Col: TColor32);
var
	px, py, ox, oy, dy: Integer;
	DP, FP: PColor32;
	A: Byte;
	// CA: Single; // support alpha component of Col
begin
	if (not Loaded) or ((X + GlyphWidth) > Buffer.Width) then Exit;

	if MultiLine then
	begin
		ox := Ord(G) mod 16 * GlyphWidth;
		oy := Ord(G) div 16 * GlyphHeight;
	end
	else
	begin
		ox := Ord(G) * GlyphWidth;
		oy := 0;
	end;

	{CA := AlphaComponent(Col);
	if CA < 1 then Exit;
	CA := CA / 256;}

	dy := Min(Y + GlyphHeight - 1, Buffer.Height - 1);

	for py := Y to dy do
	begin
		DP := Buffer.PixelPtr[X, py];
		FP := Font.PixelPtr[ox, oy+py-Y];
		for px := 0 to GlyphWidth-1 do
		begin
			A := FP^ and $FF; // AlphaComponent

			// TODO: BlendMem_ASM broken on Linux?
			if A > 0 then
			begin
				{$IFDEF WINDOWS}
				BlendMem_ASM(SetAlpha(Col, A), DP^); // faster, ignore alpha of Col
				//BlendMem_ASM(SetAlpha(Col, Trunc(A*CA)), DP^);
				{$ELSE}
				DP^ := MergeColors(Col, A);
				{$ENDIF}
			end;
			Inc(DP);
			Inc(FP);
		end;
	end;
end;

procedure TRendererFontBitmap.DrawString(Buffer: TBitmap32; X, Y: Integer;
	const S: String; Col: TColor32; MaxWidth: Integer);
var
	i: Integer;
begin
	if (not Loaded) or (S.IsEmpty) then Exit;

	X := IfThen(X < 0, Abs(X), X {* GlyphWidth + 2});
	Y := IfThen(Y < 0, Abs(Y), Y {* GlyphHeight});
	if MaxWidth <= 0 then
		MaxWidth := Buffer.Width
	else
		Inc(MaxWidth, X);

	for i := 1 to Length(S) do
	begin
		if (X + GlyphWidth) > MaxWidth then Break;
		if S[i] <> ' ' then
			DrawGlyph(Buffer, S[i], X, Y, Col);
		Inc(X, GlyphWidth);
	end;
end;

procedure TRendererFontBitmap.DrawStringCentered(Buffer: TBitmap32; Y: Word; const S: String; Col: TColor32);
var
	X: Integer;
begin
	X := ( Buffer.Width - (Length(S) * GlyphWidth) ) div 2;
	DrawString(Buffer, -X, -Y, S, Col);
end;

{$ENDIF}


{$IFDEF RENDER_FONT_FREETYPE}

// ================================================================================================
// TRendererFontFreeType
// ================================================================================================

constructor TRendererFontFreeType.Create(const Filename: String);
begin
	inherited Create(Filename);

	LoadFromFile(Filename);
end;

destructor TRendererFontFreeType.Destroy;
begin
	Font.Free;

	inherited Destroy;
end;

function TRendererFontFreeType.LoadFromFile(const Filename: String): Boolean;
begin
	Result := FileExists(Filename);
	Loaded := Result;
	if not Result then Exit;

	Font.Free;
	Font := TFreeTypeFont.Create;
	Font.Name := FontCollection.AddFile(Filename).Family.FamilyName;
	Font.Hinted := True;
	Font.Quality := grqHighQuality;
	Font.ClearType := False; // rendering not implemented
	Font.KerningEnabled := False; // used as monospace here
	Font.DPI := 72;

	GlyphWidth  := Trunc(Font.TextWidth('X'));
	GlyphHeight := Trunc(Font.TextHeight('X'));
end;

procedure TRendererFontFreeType.DrawGlyph(Buffer: TBitmap32; G: Char; X, Y: Word; Col: TColor32);
begin
	DrawString(Buffer, X, Y, G, Col);
end;

procedure TRendererFontFreeType.DrawTextRect(Buffer: TBitmap32; const S: String;
	R: TRect; Col: TColor32; Align: TFreeTypeAlignments);
begin
	Font.DrawTextRect(Buffer, S, R.Left, R.Top, R.Right, R.Bottom, Col, Align);
end;

procedure TRendererFontFreeType.DrawString(Buffer: TBitmap32; X, Y: Integer;
	const S: String; Col: TColor32; MaxWidth: Integer);
var
	i: Integer;
	p: PChar;
	unicode: Cardinal;
	CPLen: integer;
begin
	if (not Loaded) or (S.IsEmpty) or (Buffer = nil) then Exit;
	if MaxWidth <= 0 then
		MaxWidth := Buffer.Width;

	i := GlyphHeight + FontOffsetY;
	Inc(Y, i);
	if Y >= Buffer.Height then
		Exit;

	if FontKerning = 0 then
	begin
		i := UTF8Length(S) * GlyphWidth;
		if i <= MaxWidth then
			Font.DrawText(Buffer, S, X, Y, Col)
		else
			Font.DrawText(Buffer, UTF8Copy(S, 1, MaxWidth div GlyphWidth), X, Y, Col)
	end
	else
	begin
		Inc(MaxWidth, X);

		// we have no control over kerning using DrawText so we do it the hard way
		p := PChar(S);
		repeat
			if (X + GlyphWidth) > MaxWidth then Break;

			unicode := UTF8CodepointToUnicode(p, CPLen);
			if unicode > 0 then
				Font.DrawGlyph(Buffer, Font.CharIndex[unicode], X, Y, Col);

			Inc(p, CPLen);
			Inc(X, GlyphWidth);

		until (CPLen = 0) or (unicode = 0);
	end;
end;

procedure TRendererFontFreeType.DrawStringCentered(Buffer: TBitmap32; Y: Word; const S: String; Col: TColor32);
begin
	if Buffer <> nil then
		Font.DrawTextRect(Buffer, S, 0, Y, Buffer.Width-1, Y+GlyphHeight, Col, [ftaCenter, ftaVerticalCenter]);
end;

function TRendererFontFreeType.TextWidth(const Text: String): Word;
begin
	Result := Length(Text) * GlyphWidth; //!!! Round(Font.TextWidth(Text));
end;

{$ENDIF}

end.

