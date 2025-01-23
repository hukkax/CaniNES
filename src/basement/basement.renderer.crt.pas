unit Basement.Renderer.CRT;

{$mode delphi}

interface

uses
	Classes, SysUtils,
	Basement.Renderer, Basement.Window, Basement.Util,
	SDL2, Graphics32;

const
	HORIZBLURMOD = 120;

type
	TCRTRenderer = class(TRenderer)
	private
		MaskCounter:    Single;         // Counter for the dot crawl texture animation
		Overlay1:       PSDL_Texture;   // The overlay texture for Scanlines
		Overlay2:       PSDL_Texture;   // The overlay texture for CRT Pixel Mask
		Overlay3:       PSDL_Texture;   // The overlay texture for CRT Noise
		TempTarget:     PSDL_Texture;   // Temporary texture for blurring

		ScanlineBitmap: TBitmap32;      // Source bitmaps used for generating the
		MaskBitmap:     TBitmap32;      // overlay textures as pixel scaling changes
		NoiseBitmap:    TBitmap32;      //
	public
		Enabled: Boolean;

		Options: record

			// Enable or disable the overlay textures.
			MaskEnabled:      Boolean;
			ScanlinesEnabled: Boolean;

			// Doubles the pixel size of the CRT mask overlay starting
			// from this zoom factor
			EnlargeMaskAtZoomLevel: Byte;

			// The brightness of the overlay images, applied while
			// (re)generating the overlay texture.
			ScanlineOpacity: Double;
			MaskOpacity:     Double;

			// This is a simple semitransparent, vertically offset blit of
			// the framebuffer onto itself. Increases brightness slightly.
			ScanlineBloom: Double;

			// Amount of horizontal "blurring". It's not a real blur but
			// just blits additional semitransparent copies of the framebuffer
			// on top of itself. As such, larger values increase brightness.
			HorizontalBlur: Byte;

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
	COL_MASK_R = $FFFF0000;
	COL_MASK_G = $FF00FF00;
	COL_MASK_B = $FF0000FF;

	// The default image used for MaskBitmap
	DefaultMaskData: array[0..3*6-1] of Cardinal = (
		COL_MASK_R, COL_MASK_G, COL_MASK_B,
		COL_MASK_R, COL_MASK_G, COL_MASK_B,
		COL_MASK_B, COL_MASK_R, COL_MASK_G,
		COL_MASK_B, COL_MASK_R, COL_MASK_G,
		COL_MASK_G, COL_MASK_B, COL_MASK_R,
		COL_MASK_G, COL_MASK_B, COL_MASK_R
	 );

	// The default image used for ScanlineBitmap
	DefaultScanlineData: array[0..20-1] of Cardinal =
	(
		// 1x2 pixel bitmap for 2X zoom
		$00000000, $FF000000,
		// 1x3 pixel bitmap for 3X zoom
		$66000000, $00000000, $FF000000,
		// 1x4 pixel bitmap for 4X zoom
		$55000000, $00000000, $CC000000, $FF000000,
		// 1x5 pixel bitmap for 5X zoom
		$55000000, $00000000, $88000000, $CC000000, $FF000000,
		// 1x6 pixel bitmap for 6X zoom and further
		$44000000, $00000000, $66000000, $AA000000, $DD000000, $FF000000
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
		HorizontalBlur := 0;
		ScanlineOpacity := 0.5;
		MaskOpacity := 0.1;
		ExtraContrast := 0;
		NoiseOpacity := 0;
	end;

	//Init;
end;

destructor TCRTRenderer.Destroy;
begin
	if Overlay1 <> nil then
		SDL_DestroyTexture(Overlay1);
	if Overlay2 <> nil then
		SDL_DestroyTexture(Overlay2);
	if Overlay3 <> nil then
		SDL_DestroyTexture(Overlay3);
	if TempTarget <> nil then
		SDL_DestroyTexture(TempTarget);

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
	Y, Scale, i: Integer;
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
			ScanlineBitmap := Bitmap32FromData(1, 20, @DefaultScanlineData[0]);
			if (ScanlineBitmap <> nil) and (Y < ScanlineBitmap.Height) then
			begin
				Tmp := Bitmap32FromData(1, Scale, ScanlineBitmap.PixelPtr[0,Y]);
				Overlay1 := CreateTiledTexture(Renderer,
					Tmp, DR.w, DR.h, SDL_BLENDMODE_BLEND, ScanlineOpacity);
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

			Overlay2 := CreateTiledTexture(Renderer,
				Tmp, DR.w, DR.h, SDL_BLENDMODE_MUL, MaskOpacity);
			Tmp.Free;
		end;

		//if NoiseOpacity > 0 then
		begin
			DR.w := Window.Settings.FramebufferWidth;
			DR.h := Window.Settings.FramebufferHeight;

			NoiseBitmap.Free;
			NoiseBitmap := TBitmap32.Create(DR.w * NoiseSize, DR.h * NoiseSize);

			for Y := 0 to NoiseBitmap.Height-1 do
			begin
				P := NoiseBitmap.PixelPtr[0, Y];
				for i := 0 to NoiseBitmap.Width-1 do
				begin
					P^ := Gray32(Random(256));
					Inc(P);
				end;
			end;

			Overlay3 := SDL_CreateTexture(Renderer,
				SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_STATIC,
				NoiseBitmap.Width, NoiseBitmap.Height);
			SDL_UpdateTexture(Overlay3, nil, @NoiseBitmap.Bits[0], NoiseBitmap.Width*4);
			SDL_SetTextureBlendMode(Overlay3, SDL_BLENDMODE_BLEND);
			SDL_SetTextureColorMod(Overlay3, 128, 128, 128);
		end;

		TempTarget := SDL_CreateTexture(Renderer,
			SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_TARGET,
			Window.Settings.FramebufferWidth * Scale, Window.Settings.FrameBufferHeight * Scale);
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
begin
	SetBlending(Overlay1, SDL_BLENDMODE_BLEND, Options.ScanlineOpacity);
	SetBlending(Overlay2, SDL_BLENDMODE_MUL, Options.MaskOpacity);
	SDL_SetTextureAlphaMod(Overlay3, Options.NoiseOpacity);
	SetBlending(TempTarget, SDL_BLENDMODE_BLEND, 1.0);
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

	SDL_RenderCopyEx(Renderer, Texture, @SrcRect, nil, 0, nil, RendererFlipMode);

	// Setup SDL renderer for 1:1 pixel mapping
	SDL_RenderGetScale(Renderer, @X, @Y);
	SDL_RenderSetScale(Renderer, 1.0, 1.0);
	Scale := Window.Scale;

	SDL_SetTextureBlendMode(Texture, SDL_BLENDMODE_BLEND);

	// "Blurring"
	if Scale > 1 then
	begin
		// Crappy but cheap "horizontal blurring"
		V := Options.HorizontalBlur;
		if V > 0 then
		begin
			// render original image to temp texture
			SDL_SetRenderTarget(Renderer, TempTarget);
			SDL_RenderSetScale(Renderer, 1.0, 1.0);
			SDL_RenderGetViewport(Renderer, @DR);
			SDL_SetTextureAlphaMod(Texture, 255);
			//SDL_RenderClear(Renderer);
			//SDL_RenderCopy(Renderer, Texture, nil, nil);
			SDL_RenderCopyEx(Renderer, Texture, @SrcRect, nil, 0, nil, RendererFlipMode);

			SDL_SetTextureAlphaMod(Texture, V * 2);
			DR.x := +1;
			SDL_RenderCopyEx(Renderer, Texture, @SrcRect, @DR, 0, nil, RendererFlipMode);
			DR.x := -1;
			SDL_RenderCopyEx(Renderer, Texture, @SrcRect, @DR, 0, nil, RendererFlipMode);

			SDL_SetTextureAlphaMod(Texture, V);
			DR.x := -2;
			SDL_RenderCopyEx(Renderer, Texture, @SrcRect, @DR, 0, nil, RendererFlipMode);
			DR.x := +2;
			SDL_RenderCopyEx(Renderer, Texture, @SrcRect, @DR, 0, nil, RendererFlipMode);

			SDL_SetTextureAlphaMod(Texture, V div 2);
			DR.x := -3;
			SDL_RenderCopyEx(Renderer, Texture, @SrcRect, @DR, 0, nil, RendererFlipMode);
			DR.x := +3;
			SDL_RenderCopyEx(Renderer, Texture, @SrcRect, @DR, 0, nil, RendererFlipMode);

			// temp texture back to main
			SDL_SetRenderTarget(Renderer, nil);
			SDL_RenderCopy(Renderer, TempTarget, nil, nil);
		end;

		// Render scanlines
		if Options.ScanlinesEnabled then
		begin
			SDL_RenderCopy(Renderer, Overlay1, nil, nil);
		end;
	end;

	// Fake CRT mask
	if Options.MaskEnabled then
	begin
		if Options.DotCrawlSpeed >= 0.01 then // Animate it
		begin
			SDL_RenderGetViewport(Renderer, @DR);
			DR.x := 0;
			DR.y := (Trunc(MaskCounter) - MaskBitmap.Height) * 2;
			Inc(DR.h, {MaskBitmap.Height}16);
			MaskCounter += Options.DotCrawlSpeed;
			if MaskCounter > MaskBitmap.Height then MaskCounter := 0;
			SDL_RenderCopy(Renderer, Overlay2, nil, @DR);
		end
		else
			SDL_RenderCopy(Renderer, Overlay2, nil, nil);
	end;

	// Snow
	if Options.NoiseOpacity > 0 then
	begin
		DR.w := Window.Settings.FrameBufferWidth;
		DR.h := Window.Settings.FrameBufferHeight;
		DR.x := Random(NoiseBitmap.Width  - DR.w);
		DR.y := Random(NoiseBitmap.Height - DR.h);
		SDL_RenderCopy(Renderer, Overlay3, @DR, nil);
	end;

	// Extra contrast
	if Options.ExtraContrast > 0 then
	begin
		SDL_SetTextureBlendMode(Texture, SDL_BLENDMODE_ADD);
		SDL_SetTextureAlphaMod(Texture, Options.ExtraContrast);
		SDL_RenderCopyEx(Renderer, Texture,
			@SrcRect, nil, 0, nil, RendererFlipMode);
	end;

	// Reset SDL renderer setup
	SDL_SetTextureBlendMode(Texture, SDL_BLENDMODE_NONE);
	SDL_RenderSetScale(Renderer, X, Y);
end;


end.

