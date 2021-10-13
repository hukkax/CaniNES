unit Basement.Renderer.CRT;

{$mode delphi}

interface

uses
	Classes, SysUtils,
	Basement.Renderer, Basement.Window, Basement.Util,
	SDL2, Graphics32;

const
	HORIZBLURMOD = 30;

type
	TCRTRenderer = class(TRenderer)
	private
		MaskCounter:    Single;         // Counter for the dot crawl texture animation
		Overlay1:       PSDL_Texture;   // The overlay texture for Scanlines
		Overlay2:       PSDL_Texture;   // The overlay texture for CRT Pixel Mask
		Overlay3:       PSDL_Texture;   // The overlay texture for CRT Noise
		ScanlineBitmap: TBitmap32;      // Source bitmaps used for generating the
		MaskBitmap:     TBitmap32;      // overlay textures as pixel scaling changes
		NoiseBitmap:    TBitmap32;      //
	public
		Enabled: Boolean;

		Options: record

			// Enable or disable the overlay textures.
			MaskEnabled: Boolean;
			ScanlinesEnabled: Boolean;

			// Doubles the pixel size of the CRT mask overlay starting
			// from this zoom factor
			EnlargeMaskAtZoomLevel: Byte;

			// The brightness of the overlay images, applied while
			// (re)generating the overlay texture.
			ScanlineBrightness: Double;
			MaskBrightness: Double;

			// Simply changes the color modulation values of the
			// Mask overlay blit (thus MaskEnabled must be True).
			// Lower values increase blurriness but darken the image.
			// Higher values sharpen and brighten the image.
			BrightAndSharp: Double; // 0..1

			// This is a simple semitransparent, vertically offset blit of
			// the framebuffer onto itself. Increases brightness slightly.
			ScanlineBloom: Double;

			// Amount of horizontal "blurring". It's not a real blur but
			// just blits additional semitransparent copies of the framebuffer
			// on top of itself. As such, larger values increase brightness.
			HorizontalBlur: Byte; // 0..3

			// Values >0 enable an animated "dot crawl" noise effect.
			// The subtleness depends on MaskBrightness and other values.
			DotCrawlSpeed: Double;

			// Noise overlay opacity
			NoiseOpacity: Byte;

			// Applies a bit of extra contrast to the final image.
			// Requires a recent SDL2 version with SDL_BLENDMODE_MUL.
			ExtraContrast: Byte;
		end;

		procedure 	LoadOverlays(const ScanlinesFile, MaskFile: String);

		procedure 	Init; override;
		procedure	ScaleChanged(NewScale: Byte); override;
		procedure 	OptionsChanged;
		procedure   Render; override;

		constructor Create(AWindow: TWindow; ATexture: PSDL_Texture = nil; const Title: String = ''); override;
		destructor  Destroy; override;
	end;

const
	// The default image used for MaskBitmap
	DefaultMaskData: array[0..3*6-1] of Cardinal = (
		$FFFFCDCD, $FFBDFBD9, $FFDDD5FF,
		$FFFFCDCD, $FFBDFBD9, $FFDDD5FF,
		$FFDDD5FF, $FFFFCDCD, $FFBDFBD9,
		$FFDDD5FF, $FFFFCDCD, $FFBDFBD9,
		$FFBDFBD9, $FFDDD5FF, $FFFFCDCD,
		$FFBDFBD9, $FFDDD5FF, $FFFFCDCD
	 );

	// The default image used for ScanlineBitmap
	DefaultScanlineData: array[0..20*2-1] of Cardinal =
	(
		$FFFFEEFF, $FFE5FFF6,	// 2x2 pixel bitmap
		$FF92609B, $FF609268,	// for 2X zoom

		$FF65346E, $FF34653C,	// 2x3 pixel bitmap
		$FFFFEEFF, $FFE5FFF6,	// for 3X zoom
		$FFBE95BE, $FF8CBE9D,

		$FF92609B, $FF609268,	// 2x4 pixel bitmap
		$FFFFEEFF, $FFE5FFF6,	// for 4X zoom
		$FFFFD6FF, $FFCDFFDE,
		$FF65346E, $FF34653C,

		$FFBF8DC8, $FF8DBF95,	// 2x5 pixel bitmap
		$FFF1D2F1, $FFC9F1DA,	// for 5X zoom
		$FFFFEBFF, $FFE2FFF3,
		$FFE0B7E0, $FFAEE0BF,
		$FF92609B, $FF609268,

		$FF92609B, $FF609268,	// 2x6 pixel bitmap
		$FFFFD6FF, $FFCDFFDE,	// for 6X zoom and further
		$FFFFEEFF, $FFE5FFF6,
		$FFFFD6FF, $FFCDFFDE,
		$FF92609B, $FF609268,
		$FF65346E, $FF34653C
	);

var
	CRTRenderer: TCRTRenderer;


implementation

uses Math;

constructor TCRTRenderer.Create(AWindow: TWindow; ATexture: PSDL_Texture; const Title: String);
begin
	inherited Create(AWindow, ATexture, Title);

	CRTRenderer := Self;

	// Default settings
	with Options do
	begin
		MaskEnabled := True;
		ScanlinesEnabled := True;
		EnlargeMaskAtZoomLevel := 3;
		ScanlineBloom := 0.0;
		DotCrawlSpeed := 0.0;
		HorizontalBlur := 2;
		ScanlineBrightness := 1.15;
		MaskBrightness := 1.18;
		BrightAndSharp := 0.85; // 0=blurriest and darkest, 1.0=sharpest and brightest
		ExtraContrast := 0;
		NoiseOpacity := 0;
	end;

	Init;
end;

destructor TCRTRenderer.Destroy;
begin
	if Overlay1 <> nil then
		SDL_DestroyTexture(Overlay1);
	if Overlay2 <> nil then
		SDL_DestroyTexture(Overlay2);
	if Overlay3 <> nil then
		SDL_DestroyTexture(Overlay3);

	ScanlineBitmap.Free;
	MaskBitmap.Free;
	NoiseBitmap.Free;
	CRTRenderer := nil;

	inherited;
end;

procedure TCRTRenderer.LoadOverlays(const ScanlinesFile, MaskFile: String);
var
	Tmp: TBitmap32;
begin
	if ScanlinesFile <> '' then
	begin
		Tmp := LoadImage(ScanlinesFile);
		if Tmp <> nil then
		begin
			ScanlineBitmap.Free;
			ScanlineBitmap := Tmp;
		end;
	end;

	if MaskFile <> '' then
	begin
		Tmp := LoadImage(MaskFile);
		if Tmp <> nil then
		begin
			MaskBitmap.Free;
			MaskBitmap := Tmp;
		end;
	end;

	Init;
end;

// Reinitialize overlay bitmaps
procedure TCRTRenderer.Init;

	// The vertical pixel offsets of various subimages in the scanlines bitmap
	function GetScanlinePixY(Scale: Word): Word; inline;
	begin
		Result := (Scale+1) * (Scale-2) div 2;
	end;

const
	NoiseSize = 3;
var
	DR: TSDL_Rect;
	Y, Scale: Integer;
	Tmp: TBitmap32;
	P: PColor32;
begin
	inherited;

	SDL_RenderGetViewport(Renderer, @DR);
	Scale := Window.Scale;
	DR.w *= Scale;
	DR.h *= Scale;

	with Options do
	begin
		if (ScanlinesEnabled) and (Scale > 1) then
		begin
			// Get the offset to the relevant scanlines bitmap
			Y := GetScanlinePixY(Scale);
			ScanlineBitmap.Free;
			ScanlineBitmap := Bitmap32FromData(2, 20, @DefaultScanlineData[0]);
			if (ScanlineBitmap <> nil) and (Y < ScanlineBitmap.Height) then
			begin
				Tmp := Bitmap32FromData(2, Scale, ScanlineBitmap.PixelPtr[0,Y]);
				Overlay1 := CreateTiledTexture(Renderer, Tmp, DR.w, DR.h, ScanlineBrightness);
				Tmp.Free;
			end;
		end;

		if MaskEnabled then
		begin
			MaskBitmap.Free;
			MaskBitmap := Bitmap32FromData(3, 6, @DefaultMaskData[0]);

			if Scale < EnlargeMaskAtZoomLevel then
				Tmp := MaskBitmap.Clone
			else
				Tmp := MaskBitmap.DoubledCopy;

			if DotCrawlSpeed >= 0.01 then
				Inc(DR.h, MaskBitmap.Height);

			Overlay2 := CreateTiledTexture(Renderer, Tmp, DR.w, DR.h, MaskBrightness);
			Tmp.Free;
		end;

		//if NoiseOpacity > 0 then
		begin
			DR.w := Window.Settings.FramebufferWidth;
			DR.h := Window.Settings.FramebufferHeight;

			NoiseBitmap.Free;
			NoiseBitmap := TBitmap32.Create(DR.w*NoiseSize, DR.h*NoiseSize);

			for Y := 0 to NoiseBitmap.Height-1 do
			begin
				P := NoiseBitmap.PixelPtr[0, Y];
				for Scale := 0 to NoiseBitmap.Width-1 do
				begin
					P^ := Gray32(Random(256));
					Inc(P);
				end;
			end;

			Overlay3 := SDL_CreateTexture(Renderer, UInt32(SDL_PIXELFORMAT_ARGB8888),
				SInt32(SDL_TEXTUREACCESS_STATIC), NoiseBitmap.Width, NoiseBitmap.Height);

			SDL_UpdateTexture(Overlay3, nil, @NoiseBitmap.Bits[0], NoiseBitmap.Width*4);
			SDL_SetTextureBlendMode(Overlay3, SDL_BLENDMODE_BLEND);
			SDL_SetTextureColorMod(Overlay3, 128, 128, 128);
		end;
	end;

	OptionsChanged;
end;

procedure TCRTRenderer.ScaleChanged(NewScale: Byte);
begin
	FreeAndNil(ScanlineBitmap);
	FreeAndNil(MaskBitmap);
	Init;
end;

procedure TCRTRenderer.OptionsChanged;
var
	i: Integer;
begin
	if Overlay2 <> nil then
	begin
		i := Min(Trunc(Options.BrightAndSharp * 255), 255);
		SDL_SetTextureColorMod(Overlay2, i,i,i);
	end;
end;

procedure TCRTRenderer.Render;
var
	V, Scale: Integer;
	X, Y: Single;
	DR: TSDL_Rect;
begin
	if not Enabled then Exit;

	Texture := RenderedTexture;

	{$IFNDEF DISABLE_RENDERCLEAR}
	SDL_RenderClear(Renderer);
	{$ENDIF}

	SDL_RenderCopy(Renderer, Texture, @SrcRect, nil);

	// Setup SDL renderer for 1:1 pixel mapping
	SDL_RenderGetScale(Renderer, @X, @Y);
	SDL_RenderSetScale(Renderer, 1.0, 1.0);
	Scale := Window.Scale;

	// Render scanlines
	if (Scale > 1) and (Options.ScanlinesEnabled) then
		SDL_RenderCopy(Renderer, Overlay1, nil, nil);

	// Fake CRT mask
	if Options.MaskEnabled then
	begin
		if Options.DotCrawlSpeed >= 0.01 then // Animate it
		begin
			SDL_RenderGetViewport(Renderer, @DR);
			DR.x := 0;
			DR.y := Trunc(MaskCounter) - MaskBitmap.Height;
			Inc(DR.h, MaskBitmap.Height);
			MaskCounter += Options.DotCrawlSpeed;
			if MaskCounter > MaskBitmap.Height then MaskCounter := 0;
			SDL_RenderCopy(Renderer, Overlay2, nil, @DR);
		end
		else
			SDL_RenderCopy(Renderer, Overlay2, nil, nil);
	end;

	SDL_SetTextureBlendMode(Texture, SDL_BLENDMODE_BLEND);

	// "Blurring"
	if Scale > 1 then
	begin
		SDL_RenderGetViewport(Renderer, @DR);

		// Crappy but cheap "horizontal blurring"
		V := Options.HorizontalBlur;
		if V > 0 then
		begin
			DR.y := 0;
			DR.x := -2;
			SDL_SetTextureAlphaMod(Texture, HORIZBLURMOD*2);
			SDL_RenderCopy(Renderer, Texture, @SrcRect, @DR);
			if V >= 2 then
			begin
				DR.x := V - 1;
				SDL_SetTextureAlphaMod(Texture, HORIZBLURMOD*2);
				SDL_RenderCopy(Renderer, Texture, @SrcRect, @DR);
				if V >= 3 then
				begin
					DR.x := V + 1;
					SDL_SetTextureAlphaMod(Texture, HORIZBLURMOD div 2);
					SDL_RenderCopy(Renderer, Texture, @SrcRect, @DR);
				end;
			end;
		end;

	end;

	// Also a simple vertical blur for larger zoom levels
	if (Scale >= 3) and (Options.ScanlineBloom >= 0.01) then
	begin
		DR.x := 0;
		DR.y := Trunc(Scale / 2 * Options.ScanlineBloom);
		SDL_SetTextureAlphaMod(Texture, Trunc(Min(255, 22 * Options.ScanlineBloom)));
		SDL_RenderCopy(Renderer, Texture, @SrcRect, @DR);
	end;

	if Options.NoiseOpacity > 0 then
	begin
		DR.w := Window.Settings.FrameBufferWidth;
		DR.h := Window.Settings.FrameBufferHeight;
		DR.x := Random(NoiseBitmap.Width  - DR.w);
		DR.y := Random(NoiseBitmap.Height - DR.h);
		SDL_SetTextureAlphaMod(Overlay3, Options.NoiseOpacity);
		SDL_RenderCopy(Renderer, Overlay3, @DR, nil);
	end;

	if Options.ExtraContrast > 0 then
	begin
		SDL_SetTextureBlendMode(Texture, SDL_BLENDMODE_ADD);
		SDL_SetTextureAlphaMod(Texture, Options.ExtraContrast);
		SDL_RenderCopy(Renderer, Texture, @SrcRect, nil);
	end;

	// Reset SDL renderer setup
	SDL_SetTextureBlendMode(Texture, SDL_BLENDMODE_NONE);
	SDL_RenderSetScale(Renderer, X, Y);
end;


end.

