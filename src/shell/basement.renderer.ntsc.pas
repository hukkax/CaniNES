unit Basement.Renderer.NTSC;

{$mode delphi}

interface

uses
	Classes, SysUtils,
	Basement.Renderer, Basement.Window, Basement.Util,
	SDL2, Graphics32;

type
	TNTSCRenderer = class(TRenderer)
	public
		Enabled: Boolean;

		procedure   Render; override;

		constructor Create(Window: TWindow; Width, Height: Word; const Title: String = ''); overload;
		destructor  Destroy; override;
	end;

var
	NTSCRenderer: TNTSCRenderer;

implementation

constructor TNTSCRenderer.Create(Window: TWindow; Width, Height: Word; const Title: String);
begin
	inherited Create(Window, nil, Title);

	Enabled := True;
	NTSCRenderer := Self;

	FrameBuffer := TBitmap32.Create(Width, Height);
	FrameBuffer.Clear(0);
	Texture := SDL_CreateTexture(Renderer, UInt32(SDL_PIXELFORMAT_ARGB8888),
		SInt32(SDL_TEXTUREACCESS_STREAMING), Width, Height);

	Init;
end;

destructor TNTSCRenderer.Destroy;
begin
	FrameBuffer.Free;
	SDL_DestroyTexture(Texture);
	NTSCRenderer := nil;

	inherited;
end;

procedure TNTSCRenderer.Render;
begin
	if Enabled then
	begin
		SrcRect.x := (Window.OverscanRect.Left + 1) * 3;
		SrcRect.y := Window.OverscanRect.Top;
		SrcRect.w := FrameBuffer.Width -  SrcRect.x - 1;
		SrcRect.h := Window.OverscanRect.Height;

		SDL_UpdateTexture(Texture, nil, @FrameBuffer.Bits[0], FrameBuffer.Width*4);
		SDL_RenderCopy(Renderer, Texture, @SrcRect, nil);

		RenderedTexture := Texture;
	end;
end;


end.

