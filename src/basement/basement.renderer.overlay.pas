unit Basement.Renderer.Overlay;

{$MODE DELPHI}

{$I basement.inc}

{.$DEFINE OVERLAY_DEBUG}

interface

uses
	Classes, SysUtils,
	SDL2, Graphics32,
	Basement.Window,
	Basement.Renderer,
	Basement.Font,
	Basement.Util;

const
	{$IFDEF RENDER_FONT_FREETYPE}
	MaxNativeScale = 9; // window sizes larger than this get pixel doubled
	{$ELSE}
	MaxNativeScale = 3; // larger value would require having larger bitmap fonts
	{$ENDIF}

	{$IFDEF RENDER_FONT_FREETYPE}
	TTF_FONTFILE    = 'officecodepro.ttf';
	{$ENDIF}

type
	TLayerAlignment = (
		laTopLeft = 0, laTopCenter,    laTopRight,
		laCenterLeft,  laCenter,       laCenterRight,
		laBottomLeft,  laBottomCenter, laBottomRight
	);

const
	LayerAlignmentNames: array[TLayerAlignment] of AnsiString = (
		'TopLeft',    'TopCenter',    'TopRight',
		'CenterLeft', 'Center',       'CenterRight',
		'BottomLeft', 'BottomCenter', 'BottomRight'
	);

	MARGIN_NONE = -1337;

type
	TOverlayFontManager = class
	private
		FontDir: String;

		{$IFDEF RENDER_FONT_FREETYPE}
		TTF: TRendererFontFreeType;
		{$ENDIF}
	public
		Font: TRendererFont;

		constructor Create(const FontPath: String);
		destructor  Destroy; override;

		procedure  LoadFont;
	end;

	TOverlayRenderer = class(TRenderer)
	private
		Width, Height: Word;
		FOpacity:      Single;
		OriginalSize:  TPoint;
		RenderRect,
		ActiveRect,
		DestRect:      TSDL_Rect;
		FTop, FLeft:   Integer;
		FAlignment:    TLayerAlignment;
		FActiveArea:   TRect;
		FMarginX,
		FMarginY:      Integer;

		procedure	SetOpacity(AOpacity: Single);
		procedure	SetX(NewX: Integer);
		procedure	SetY(NewY: Integer);
		procedure	SetAlignment(AAlignment: TLayerAlignment);
		procedure	SetActiveArea(Value: TRect);
	public
		Scale:           Byte;
		ScaleWithWindow: Boolean;
		Font:            TRendererFont;
		GlyphsPerScreen: TPoint;

		procedure 	Init; override;
		procedure	Resize(W, H: Word); override;
		procedure	ScaleChanged(NewScale: Byte); override;
		procedure   Render; override;

		procedure	DrawRect(X, Y, W, H: Word; Col: TColor32; Padding: Byte = 0);
		procedure	DrawString(X, Y: Integer; const S: String; Col: TColor32; MaxWidth: Integer = -1);
		procedure	SetFont(const AFont: TRendererFont);

		procedure	Align(AAlignment: Byte; MarginX: Integer = MARGIN_NONE; MarginY: Integer = MARGIN_NONE); overload;
		procedure	Align(AAlignment: TLayerAlignment; MarginX: Integer = MARGIN_NONE; MarginY: Integer = MARGIN_NONE); overload;
		procedure	SetActiveArea(R: TRect; Realign: Boolean = True);

		property	Opacity: Single read FOpacity write SetOpacity;
		property	Left: Integer read FLeft write SetX;
		property	Top:  Integer read FTop  write SetY;
		property	ActiveArea: TRect read FActiveArea write SetActiveArea;
		property	Alignment: TLayerAlignment read FAlignment write SetAlignment;

		constructor Create(Window: TWindow; const Title: String; AWidth, AHeight: Word; AScale: Integer); overload;
		destructor  Destroy; override;
	end;

var
	FontManager: TOverlayFontManager;


implementation

uses
	Math,
	{$IFDEF RENDER_FONT_FREETYPE} LazUTF8, {$ENDIF}
	Graphics32.LowLevel;

// ================================================================================================
// TOverlayFontManager
// ================================================================================================

constructor TOverlayFontManager.Create(const FontPath: String);
begin
	inherited Create;

	FontDir := FontPath;

	{$IFDEF RENDER_FONT_FREETYPE}
	TTF := TRendererFontFreeType.Create(FontPath + TTF_FONTFILE);
	Font := TTF;
	{$ELSE}
	Font := TRendererFontBitmap.Create(FontPath);
	{$ENDIF}
end;

destructor TOverlayFontManager.Destroy;
begin
	Font.Free;

	inherited Destroy;
end;

procedure TOverlayFontManager.LoadFont;
	{$IFDEF RENDER_FONT_FREETYPE}
	var
		i: Integer;

	procedure SetFontData(fntOffset, fntKerning: Integer);
	var
		tmp, unicode: Integer;
	begin
		TTF.FontOffsetY := fntOffset;
		TTF.FontKerning := fntKerning;
		TTF.GlyphHeight := i * 8;
		TTF.Font.SizeInPixels := Font.GlyphHeight - 1;
		unicode := UTF8CodepointToUnicode(PChar('X'), tmp);
		Font.GlyphWidth := Trunc(TTF.Font.CharWidthFromGlyph(unicode)) + TTF.FontKerning;
        if TTF.FontKerning <> 0 then Dec(TTF.GlyphWidth);
	end;
	{$ENDIF}

	{$IFDEF RENDER_FONT_BITMAP}
	var
		FontFile: String;
	{$ENDIF}
begin
	{$IFDEF RENDER_FONT_FREETYPE}

	i := Max(1, BasementMainWindow.Scale);
	if i > MaxNativeScale then
		if Odd(i) then i := 3 else i := 2;
	// set font size and kerning according to window scale
	case i of
		1: SetFontData(-1,   0);
		2: SetFontData(-3,   0);
		3: SetFontData(-4,   0);
		4: SetFontData(-6,  -1);
		5: SetFontData(-7,  -1);
		6: SetFontData(-9,  -1);
		7: SetFontData(-10, -1);
		8: SetFontData(-12, -1);
		9: SetFontData(-13, -1);
	end;

	{$ELSE}

	if BasementMainWindow.Scale <= 1 then
		FontFile := 'default_0.png'
	else
	if Odd(BasementMainWindow.Scale) then
		FontFile := 'default_2.png'
	else
		FontFile := 'default_1.png';

	FontFile := FontDir + FontFile;

	if Font.LoadFromFile(FontFile) then
		Log('Font loaded: ' + FontFile)
	else
		Log('Font file not found: ' + FontFile);

	{$ENDIF}
end;

// ================================================================================================
// TOverlayRenderer
// ================================================================================================

constructor TOverlayRenderer.Create(Window: TWindow; const Title: String; AWidth, AHeight: Word; AScale: Integer);
begin
	inherited Create(Window, nil, Title);

	Scale := Abs(AScale);
	ScaleWithWindow := (AScale > 0);


	if (not ScaleWithWindow) and (Window.Scale > MaxNativeScale) then
	begin
		AWidth  := (AWidth  div Window.Scale) * 2;
		AHeight := (AHeight div Window.Scale) * 2;
	end;

	OriginalSize := Point(AWidth, AHeight);
	Width  := AWidth  div Max(1, Scale);
	Height := AHeight div Max(1, Scale);

	//Log('TOverlayRenderer.Create(%s): %d*%d', [Title,Width, Height]);

	FOpacity := 1.0;
	Texture := nil;
	FAlignment := laTopLeft;

	FrameBuffer := TBitmap32.Create(Width, Height);
	Resize(Width, Height);
end;

destructor TOverlayRenderer.Destroy;
begin
	FrameBuffer.Free;
	SDL_DestroyTexture(Texture);

	inherited;
end;

procedure TOverlayRenderer.Init;
begin
	inherited;

	SDL_SetTextureBlendMode(Texture, SDL_BLENDMODE_BLEND);
end;

procedure TOverlayRenderer.Resize(W, H: Word);
begin
	inherited Resize(W, H);

	OriginalSize := Point(W, H);
	ScaleChanged(0);
end;

procedure TOverlayRenderer.ScaleChanged(NewScale: Byte);
var
	Z, W, H: Integer;
begin
	SDL_RenderGetViewport(Renderer, @RenderRect);

	if not ScaleWithWindow then
	begin
		Z := IfThen(NewScale > 0, NewScale, Window.Scale);
		Z := Min(Z, Window.Scale);
		if Z > MaxNativeScale then
			Z := Z div 2
		else
			Z := Max(1, Scale);
	end
	else
		Z := Scale * Window.Scale;

	if Scale = 0 then
	begin
		W := OriginalSize.X;
		H := OriginalSize.Y;
	end
	else
	begin
		W := {RenderRect.w}OriginalSize.X * Window.Scale div Z;
		H := {RenderRect.h}OriginalSize.Y * Window.Scale div Z;
	end;

	FrameBuffer.SetSize(W, H);
	FrameBuffer.Clear(0);
	SetActiveArea(FrameBuffer.BoundsRect, False);

	DestRect.x := FLeft;
	DestRect.y := FTop;
	DestRect.w := W;
	DestRect.h := H;

	if Texture <> nil then
		SDL_DestroyTexture(Texture);
	Texture := SDL_CreateTexture(Renderer, UInt32(SDL_PIXELFORMAT_ARGB8888),
		SInt32(SDL_TEXTUREACCESS_STREAMING), W, H);

	SetOpacity(FOpacity);
	SetFont(nil);

	inherited;
end;

procedure TOverlayRenderer.Render;
var
	X, Y: Single;
begin
	if not Visible then Exit;

	// Setup SDL renderer for 1:1 pixel mapping
	if not ScaleWithWindow then
	begin
		SDL_RenderGetScale(Renderer, @X, @Y);
		SDL_RenderSetScale(Renderer, Max(1, Scale), Max(1, Scale));
	end;

	if FrameBuffer.Dirty then
	begin
		{$IFDEF OVERLAY_DEBUG}
		// show dirty rectangles
		Framebuffer.FrameRect(FActiveArea, $FF000000+Random($FFFFFF));
		{$ENDIF}

		SDL_UpdateTexture(Texture, nil, @FrameBuffer.Bits[0], FrameBuffer.Width*4);
		FrameBuffer.Dirty := False;
	end;

	SDL_RenderCopy(Renderer, Texture, @ActiveRect, @DestRect);

	if not ScaleWithWindow then
		SDL_RenderSetScale(Renderer, X, Y);
end;

// ================================================================================================
// Font

procedure TOverlayRenderer.SetFont(const AFont: TRendererFont);
begin
	if Assigned(AFont) then
		Font := AFont;

	if Assigned(Font) then
	begin
		GlyphsPerScreen.X := FrameBuffer.Width  div Font.GlyphWidth;
		GlyphsPerScreen.Y := FrameBuffer.Height div Font.GlyphHeight;
	end;
end;

procedure TOverlayRenderer.DrawString(X, Y: Integer; const S: String; Col: TColor32; MaxWidth: Integer = -1);
begin
	if Assigned(Font) then
	begin
		X := IfThen(X < 0, Abs(X), X * Font.GlyphWidth + 2);
		Y := IfThen(Y < 0, Abs(Y), Y * Font.GlyphHeight);
		Font.DrawString(FrameBuffer, X, Y, S, Col, MaxWidth);
	end;
end;

procedure TOverlayRenderer.DrawRect(X, Y, W, H: Word; Col: TColor32; Padding: Byte);
begin
	with Font do
		FrameBuffer.FillRectS(Bounds(
			X*GlyphWidth-Padding, Y*GlyphHeight-1-Padding,
			W*GlyphWidth+3+(Padding*2), H*GlyphHeight+1+(Padding*2)), Col);
end;

// ================================================================================================
// Setter methods

procedure TOverlayRenderer.SetOpacity(AOpacity: Single);
begin
	FOpacity := AOpacity;
	SDL_SetTextureAlphaMod(Texture, Min(255, Trunc(FOpacity * 255)));
end;

procedure TOverlayRenderer.SetX(NewX: Integer);
begin
	FLeft := NewX;
	ScaleChanged(0);
end;

procedure TOverlayRenderer.SetY(NewY: Integer);
begin
	FTop := NewY;
	ScaleChanged(0);
end;

procedure TOverlayRenderer.SetActiveArea(R: TRect; Realign: Boolean);
begin
	{$IFDEF OVERLAY_DEBUG}
	Log(Self.Name + '.SetActiveArea: ' + Format('%d,%d,%d,%d', [R.Left, R.Top, R.Width, R.Height]));
	{$ENDIF}

	FActiveArea := R;

	ActiveRect.x := R.Left;
	ActiveRect.y := R.Top;
	ActiveRect.w := R.Width;
	ActiveRect.h := R.Height;

	if Realign then
		Align(FAlignment, FMarginX, FMarginY);
end;

procedure TOverlayRenderer.SetAlignment(AAlignment: TLayerAlignment);
begin
	Align(AAlignment);
end;

procedure TOverlayRenderer.SetActiveArea(Value: TRect);
begin
	if FActiveArea = Value then Exit;
	FActiveArea := Value;
end;

procedure TOverlayRenderer.Align(AAlignment: Byte; MarginX, MarginY: Integer);
begin
	Align(TLayerAlignment(AAlignment), MarginX, MarginY);
end;

procedure TOverlayRenderer.Align(AAlignment: TLayerAlignment; MarginX, MarginY: Integer);
var
	Sc, X, Y, W, H, BW, BH: Integer;
begin
	FAlignment := AAlignment;

	SDL_RenderGetViewport(Renderer, @RenderRect);

	if MarginX = Margin_NONE then MarginX := FMarginX;
	if MarginY = Margin_NONE then MarginY := MarginX;

	Sc := IfThen(ScaleWithWindow, 1, Window.Scale);
	W := RenderRect.W * Sc;
	H := RenderRect.H * Sc;

	BW := FActiveArea.Width;
	BH := FActiveArea.Height;

	FMarginX := MarginX;
	FMarginY := MarginY;
	MarginX := MarginX * Sc;
	MarginY := MarginY * Sc;

	// horizontal alignment
	case AAlignment of
		laTopLeft, laCenterLeft, laBottomLeft:
			X := MarginX;
		laTopCenter, laCenter, laBottomCenter:
			X := (W div 2) - (BW div 2);
		laTopRight, laCenterRight, laBottomRight:
			X := W - BW - MarginX;
	end;

	// vertical alignment
	case AAlignment of
		laTopLeft, laTopCenter, laTopRight:
			Y := MarginY;
		laCenterLeft, laCenter, laCenterRight:
			Y := (H div 2) - (BH div 2) + MarginY;
		laBottomLeft, laBottomCenter, laBottomRight:
			Y := H - BH - MarginY;
	end;

	FLeft := X;
	FTop  := Y;

	DestRect.x := X;
	DestRect.y := Y;
	DestRect.w := BW;
	DestRect.h := BH;
end;


end.

