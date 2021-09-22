unit Basement.Renderer.CRT;

{$mode delphi}

interface

uses
	Classes, SysUtils,
	Basement.Renderer, Basement.Window, Basement.Util,
	SDL2, Graphics32;

type
	TCRTRenderer = class(TRenderer)
	private
		MaskCounter:Single;
		TempTexture,
		Overlay1,
		Overlay2:	PSDL_Texture;

		ScanlineBitmap,
		MaskBitmap:	TBitmap32;

	public
		Options: record

			MaskEnabled: Boolean;
			ScanlinesEnabled: Boolean;
			ScanlineBrightness: Single;
			MaskBrightness: Single;
			ScanlineBloom: Single;
			HorizontalBlur: Byte;
			DotCrawlSpeed: Single;
			BlurBrightness: Single;
			FilePath: String;

		end;

		procedure 	Init; override;
		procedure	ScaleChanged(NewScale: Byte); override;
		procedure   Render; override;

		constructor Create(Window: TWindow); override;
		destructor  Destroy; override;
	end;


implementation

uses Math;

constructor TCRTRenderer.Create(Window: TWindow);
begin
	inherited;

	TempTexture := nil;

	with Options do
	begin
		FilePath := DataPath + 'crt/';

		MaskEnabled := True;
		ScanlinesEnabled := True;
		ScanlineBloom := 2.0;
		DotCrawlSpeed := 0.0;
		HorizontalBlur := 3;
		BlurBrightness := 1.2;
		ScanlineBrightness := 1.0;//1.25;
		MaskBrightness := 1.0;
	end;

	Init;
end;

destructor TCRTRenderer.Destroy;
begin
	if Overlay1 <> nil then
		SDL_DestroyTexture(Overlay1);
	if Overlay2 <> nil then
		SDL_DestroyTexture(Overlay2);

	ScanlineBitmap.Free;
	MaskBitmap.Free;

	inherited;
end;

// Reinitialize overlay bitmaps
procedure TCRTRenderer.Init;
var
	DR: TSDL_Rect;
	Scale: Integer;
	S: String;
begin
	inherited;

	ScanlineBitmap.Free;
	MaskBitmap.Free;

	SDL_GetRendererOutputSize(Renderer, @DR.w, @DR.h);
	if TempTexture <> nil then
		SDL_DestroyTexture(TempTexture);
	TempTexture := SDL_CreateTexture(Renderer,
		UInt32(SDL_PIXELFORMAT_ARGB8888), SDL_TEXTUREACCESS_TARGET, DR.w, DR.h);

	SDL_RenderGetViewport(Renderer, @DR);
	Scale := Window.Scale;
	DR.w := DR.w * Scale;
	DR.h := DR.h * Scale;

	with Options do
	begin
		if (ScanlinesEnabled) and (Scale > 1) then
		begin
			ScanlineBitmap := TBitmap32.Create;
			S := FilePath + Format('scanlines-%d.png', [Scale]);
			if not FileExists(S) then S := FilePath + 'scanlines-0.png';
			ScanlineBitmap.LoadFromFile(S);
			Overlay1 := CreateTiledTexture(Renderer, ScanlineBitmap, DR.w, DR.h, ScanlineBrightness);
		end;
		if MaskEnabled then
		begin
			MaskBitmap := TBitmap32.Create;
{			if Scale > 2 then
				MaskBitmap.LoadFromFile(FilePath + 'mask.png')
			else}
				MaskBitmap.LoadFromFile(FilePath + 'mask-1x.png');
{maskbitmap.setsize(16,16);
maskbitmap.Clear(clWhite32);
maskbitmap.HorzLine(0, 0, 15, clLime32);
maskbitmap.vertLine(0, 0, 15, clAqua32);}
			if DotCrawlSpeed >= 0.01 then
				DR.h := DR.h + MaskBitmap.Height;
			Overlay2 := CreateTiledTexture(Renderer, MaskBitmap, DR.w, DR.h, MaskBrightness);
		end;
	end;
end;

procedure TCRTRenderer.ScaleChanged(NewScale: Byte);
begin
	inherited;
	Init;
end;

procedure TCRTRenderer.Render;
var
	V, Scale: Integer;
	X, Y, B: Single;
	DR: TSDL_Rect;
begin
	if (TempTexture = nil) or (FrameBuffer = nil) then Exit;

	// Setup our texture as the render target and copy the framebuffer to it
	SDL_SetRenderTarget(Renderer, TempTexture);
	SDL_RenderCopy(Renderer, Texture, nil, nil);

	// Setup SDL for 1:1 pixel mapping
	SDL_RenderGetScale(Renderer, @X, @Y);
	SDL_RenderSetScale(Renderer, 1.0, 1.0);
	Scale := Window.Scale;

	// Render scanlines
	if (Scale > 1) and (Options.ScanlinesEnabled) then
		SDL_RenderCopy(Renderer, Overlay1, nil, nil);

	// "Blurring"
	if Scale > 1 then
	begin
		SDL_RenderGetViewport(Renderer, @DR);
		SDL_SetTextureBlendMode(TempTexture, SDL_BLENDMODE_ADD);

		// Crappy but cheap "horizontal blurring"
		V := Options.HorizontalBlur;
		if V > 0 then
		begin
			B := Options.BlurBrightness;
			DR.y := 0;
			DR.x := -3;
			SDL_SetTextureAlphaMod(TempTexture, Min(Trunc(50 * B), 255));
			SDL_RenderCopy(Renderer, TempTexture, nil, @DR);
			if V >= 2 then
			begin
				DR.x := V - 1;
				SDL_SetTextureAlphaMod(TempTexture, Min(Trunc(70 * B), 255));
				SDL_RenderCopy(Renderer, TempTexture, nil, @DR);
				if V >= 3 then
				begin
					DR.x := V + 1;
					SDL_SetTextureAlphaMod(TempTexture, Min(Trunc(55 * B), 255));
					SDL_RenderCopy(Renderer, TempTexture, nil, @DR);
				end;
			end;
		end;

		// Also a simple vertical blur for larger zoom levels
		B := Options.ScanlineBloom;
		if (Scale >= 3) and (B >= 0.1) then
		begin
			DR.x := 0;
			DR.y := Scale div 2;
			SDL_SetTextureAlphaMod(TempTexture, Min(Trunc(70 * B), 255));
			SDL_RenderCopy(Renderer, TempTexture, nil, @DR);
			DR.y := -DR.y;
			SDL_RenderCopy(Renderer, TempTexture, nil, @DR);
		end;
	end;

	// Fake CRT mask
{
	if Options.MaskEnabled then
	begin
		if Options.DotCrawlSpeed >= 0.01 then // Animate it
		begin
			SDL_GetRendererOutputSize(Renderer, @DR.W, @DR.H);
			DR.x := 0;
			DR.y := Trunc(MaskCounter);
			MaskCounter += Options.DotCrawlSpeed;
			if MaskCounter >= MaskBitmap.Height then MaskCounter := 0;
			SDL_RenderCopy(Renderer, Overlay2, nil, @DR);
		end
		else
			SDL_RenderCopy(Renderer, Overlay2, nil, nil);
	end;
}
	SDL_SetRenderTarget(Renderer, nil);

	// Reset SDL renderer setup
	SDL_SetTextureBlendMode(TempTexture, SDL_BLENDMODE_NONE);
	SDL_RenderSetScale(Renderer, X, Y);

	SDL_RenderCopy(Renderer, TempTexture, nil, nil);
end;


end.

