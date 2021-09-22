unit Basement.Renderer;

{$MODE DELPHI}

{$I basement.inc}

interface

uses
	Classes, SysUtils, FGL,
	SDL2, Graphics32,
	Basement.Window, Basement.Font;

type
	TRenderer = class
	class var
		Renderer:        PSDL_Renderer;
		RenderedTexture: PSDL_Texture;
		SrcRect:         TSDL_Rect;
	protected
		Window: 	 TWindow;
	public
		FrameBuffer: TBitmap32;
		Font:        TRendererFont;
		Texture:     PSDL_Texture;
		Visible:     Boolean;
		Name:        String;

		procedure	Render; virtual;
		procedure	Init; virtual;
		procedure	ScaleChanged(NewScale: Byte); virtual;
		procedure	Resize(W, H: Word); virtual;

		constructor	Create(AWindow: TWindow; ATexture: PSDL_Texture = nil; const Title: String = ''); virtual;
		destructor	Destroy; override;
	end;

	function AddRenderer(Renderer: TRenderer): Boolean;
	function RemoveRenderer(Renderer: TRenderer): Boolean;
	function CreateTiledTexture(Renderer: PSDL_Renderer; const Tile: TBitmap32;
	         Width, Height: Word; Brightness: Single = 1.0): PSDL_Texture;

var
	Renderers: TFPGObjectList<TRenderer>;


implementation

uses
	Basement.Util;

// ================================================================================================
// Utility
// ================================================================================================

function AddRenderer(Renderer: TRenderer): Boolean;
begin
	if Renderers.IndexOf(Renderer) >= 0 then
		Result := False
	else
	begin
		Renderers.Add(Renderer);
		Result := True;
//		Log('AddRenderer: ' + Renderer.ClassName);
	end;
end;

function RemoveRenderer(Renderer: TRenderer): Boolean;
begin
	if Renderers.IndexOf(Renderer) >= 0 then
	begin
		Result := False;
		Log('RemoveRenderer: Fail');
	end
	else
	begin
		Renderers.Remove(Renderer);
		Result := True;
	end;
end;

function CreateTiledTexture(Renderer: PSDL_Renderer; const Tile: TBitmap32;
	Width, Height: Word; Brightness: Single = 1.0): PSDL_Texture;
var
	X, Y: Integer;
	Tmp: TBitmap32;
begin
	Result := SDL_CreateTexture(Renderer, UInt32(SDL_PIXELFORMAT_ARGB8888),
		SInt32(SDL_TEXTUREACCESS_STATIC), Width, Height);

	Tmp := TBitmap32.Create;
	Tmp.SetSize(Width, Height);

	Y := 0;
	while Y < Height do
	begin
		X := 0;
		while X < Width do
		begin
			Tmp.Draw(X, Y, Tile);
			Inc(X, Tile.Width);
		end;
		Inc(Y, Tile.Height);
	end;

	if Brightness <> 1.0 then
	begin
		for Y := 0 to Height-1 do
		for X := 0 to Width-1 do
			Tmp.Pixel[X,Y] := Graphics32.Brightness(Tmp.Pixel[X,Y], Brightness);
	end;

	SDL_UpdateTexture(Result, nil, @Tmp.Bits[0], Width*4);
	SDL_SetTextureBlendMode(Result, SDL_BLENDMODE_MOD);
	SDL_SetTextureColorMod(Result, 255, 255, 255);

	Tmp.Free;
end;

// ================================================================================================
// TRenderer
// ================================================================================================

constructor TRenderer.Create(AWindow: TWindow; ATexture: PSDL_Texture; const Title: String);
var
	i: Integer;
begin
	inherited Create;

    Renderer := nil;

	if Title <> '' then
		for i := Renderers.Count-1 downto 0 do
			if Renderers[i].Name = Title then
				Renderers.Delete(i);

	Name := Title;
	Window := AWindow;
	FrameBuffer := Window.FrameBuffer;
	Renderer := Window.Video.Renderer;
	if ATexture = nil then
		Texture := Window.Video.Texture
    else
	    Texture := ATexture;
	Visible := True;

	AddRenderer(Self);
end;

// don't free directly, use RemoveRenderer or Renderers.Delete/Clear
destructor TRenderer.Destroy;
begin
	inherited;
end;

procedure TRenderer.Init;
begin
	//
end;

procedure TRenderer.ScaleChanged(NewScale: Byte);
begin
	Init;
end;

procedure TRenderer.Resize(W, H: Word);
begin
	//
end;

procedure TRenderer.Render;
begin
	if not Visible then Exit;

	SrcRect.x := Window.OverscanRect.Left;
	SrcRect.y := Window.OverscanRect.Top;
	SrcRect.w := Window.OverscanRect.Width;
	SrcRect.h := Window.OverscanRect.Height;

	{$IFNDEF DISABLE_RENDERCLEAR}
	SDL_RenderClear(Renderer);
	{$ENDIF}

	if FrameBuffer.Dirty then
	begin
		SDL_UpdateTexture(Texture, nil, @FrameBuffer.Bits[0], FrameBuffer.Width*4);
		FrameBuffer.Dirty := False;
	end;

	SDL_RenderCopy(Renderer, Texture, @SrcRect, nil);

	RenderedTexture := Texture;
end;

// ================================================================================================
// Init
// ================================================================================================

initialization

	Renderers := TFPGObjectList<TRenderer>.Create(True);

finalization

	Renderers.Free;

end.

