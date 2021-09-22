unit Basement.Sprites;

{$mode delphi}
{$I basement.inc}

interface

uses
	Classes, SysUtils, Generics.Collections,
	Basement.Window,
	SDL2, Graphics32;

type
	{ TTexture }

	TTexture = class(TBitmap32)
	protected
		FUseAlpha:	Boolean;
		FAlpha:		Byte;
		FSrcRect,
		FDestRect:	TSDL_Rect;
		Renderer:	PSDL_Renderer;

		procedure 	CreateTexture;
		procedure 	SetUseAlphaChannel(AUseAlpha: Boolean);
		procedure 	SetAlpha(AAlpha: Byte);
	public
		Texture:	PSDL_Texture;
		DestRect:	PSDL_Rect;

        procedure	SetSize(W, H: Cardinal); override;

		procedure	Blit(const SourceOffset: TPoint); overload;
		procedure	Blit; overload;
		procedure 	BlitOffset(const Offset: TPoint);
		procedure 	BlitTile(const SourcePos, Offset: TPoint; TileSize: Byte);

		procedure 	SetSrcRect(R: TRect);
		procedure 	SetDestRect(R: TRect); inline;
		procedure	SetOffset(P: TPoint); inline;
		procedure 	SetPosition(X, Y: Integer); inline;
		procedure 	ClearDestRect;

		function	Clone(Flip: Boolean = False): TTexture;

		property	UseAlphaChannel: Boolean read FUseAlpha write SetUseAlphaChannel;
		property	Alpha: Byte read FAlpha write SetAlpha;

		function 	LoadFromResource(const Name: String): Boolean;
		function	LoadFromFile(const Filename: String): Boolean; override;

		constructor	Create(const ARenderer: PSDL_Renderer; W, H: Word;
					PaddingX: Integer = 0; PaddingY: Integer = -1);
		destructor	Destroy; override;
	end;

	TSpriteFrame = record
		HotSpot:	TPoint;
		Rect:		TSDL_Rect;
	end;

	{ TSprite }

	TSprite = class(TTexture)
	private
	public
		X, Y:		Integer;
		FrameCount:	Word;
		FrameSize:	TPoint;
		Frames:		array of TSpriteFrame;

		constructor Create; overload;

		function	LoadFromFile(const Filename: String;
					FrameSizeX: Word = 0; FrameSizeY: Word = 0): Boolean; overload;
		procedure	InitFrames(FrameSizeX, FrameSizeY: Word);

		procedure	Blit(Frame: Word); overload;
		procedure	Blit(Rect: TRect); overload;
	end;

	{ TSpriteEngine }

	TSpriteList = TObjectList<TSprite>;

var
	GameRenderer: PSDL_Renderer;
	Sprites: TSpriteList;


implementation

{ ================================================================================================}
{ TTexture }
{ ================================================================================================}

constructor TTexture.Create(const ARenderer: PSDL_Renderer; W, H: Word;
	PaddingX: Integer = 0; PaddingY: Integer = -1);
begin
	if PaddingY < 0 then PaddingY := PaddingX;

	inherited Create(PaddingX*2 + W, PaddingY*2 + H);

	Renderer := ARenderer;
	FUseAlpha := False;

	CreateTexture;

	Alpha := 255;
	Clear(0);

	ClearDestRect;
end;

procedure TTexture.CreateTexture;
begin
	with FSrcRect do
	begin
		X := 0;
		Y := 0;
		W := Self.Width;
		H := Self.Height;
	end;

	if Assigned(Texture) then
		SDL_DestroyTexture(Texture);
	Texture := SDL_CreateTexture(Renderer,
		UInt32(SDL_PIXELFORMAT_ARGB8888),
		SInt32(SDL_TEXTUREACCESS_STREAMING), Width, Height);

	SetUseAlphaChannel(FUseAlpha);
end;

function TTexture.LoadFromFile(const Filename: String): Boolean;
begin
	Result := inherited LoadFromFile(Filename);
	CreateTexture;
end;

function TTexture.LoadFromResource(const Name: String): Boolean;
begin
	Result := inherited LoadPNGFromResource(Name);
	CreateTexture;
end;

destructor TTexture.Destroy;
begin
	if Texture <> nil then
		SDL_DestroyTexture(Texture);

	inherited Destroy;
end;

procedure TTexture.SetUseAlphaChannel(AUseAlpha: Boolean);
begin
	FUseAlpha := AUseAlpha;
	if FUseAlpha then
		SDL_SetTextureBlendMode(Texture, SDL_BLENDMODE_BLEND)
	else
		SDL_SetTextureBlendMode(Texture, SDL_BLENDMODE_NONE);
end;

procedure TTexture.SetAlpha(AAlpha: Byte);
begin
	FAlpha := AAlpha;
	SDL_SetTextureAlphaMod(Texture, FAlpha);
end;

procedure TTexture.Blit(const SourceOffset: TPoint);
begin
	if Dirty then
	begin
		SDL_UpdateTexture(Texture, nil,
			PixelPtr[SourceOffset.X, SourceOffset.Y],
			Width * 4);
		Dirty := False;
	end;

	SDL_RenderCopy(Renderer, Texture, @FSrcRect, DestRect);
end;

procedure TTexture.SetSize(W, H: Cardinal);
begin
	inherited SetSize(W, H);
    FSrcRect.W := W;
    FSrcRect.H := H;
end;

procedure TTexture.BlitOffset(const Offset: TPoint);
begin
	if Dirty then
	begin
		SDL_UpdateTexture(Texture, nil, @Bits[0], Width * 4);
		Dirty := False;
	end;
	FSrcRect.x := Offset.X;
	FSrcRect.y := Offset.Y;
	SDL_RenderCopy(Renderer, Texture, @FSrcRect, DestRect);
end;

procedure TTexture.BlitTile(const SourcePos, Offset: TPoint; TileSize: Byte);
begin
	if Dirty then
	begin
		SDL_UpdateTexture(Texture, nil, @Bits[0], Width * 4);
		Dirty := False;
	end;
	FSrcRect.x := SourcePos.X * TileSize;
	FSrcRect.y := SourcePos.Y * TileSize;
	FSrcRect.w := TileSize;
	FSrcRect.h := TileSize;
	FDestRect.x := Offset.X;
	FDestRect.y := Offset.Y;
	FDestRect.w := TileSize;
	FDestRect.h := TileSize;
	SDL_RenderCopy(Renderer, Texture, @FSrcRect, @FDestRect);
end;

procedure TTexture.SetPosition(X, Y: Integer); inline;
begin
	SetDestRect(Bounds(X, Y, Width, Height));
end;

procedure TTexture.SetDestRect(R: TRect); inline;
begin
	with FDestRect do
	begin
		x := R.Left;
		y := R.Top;
		w := R.Width;
		h := R.Height;
	end;
	DestRect := @FDestRect;
end;

procedure TTexture.ClearDestRect;
begin
	DestRect := nil;
end;

function TTexture.Clone(Flip: Boolean = False): TTexture;
begin
	Result := TTexture.Create(Renderer, Width, Height);
	Result.Draw(0, 0, Self);
	if Flip then
	begin
		// !!! TODO in graphics32
	end;
end;

procedure TTexture.SetOffset(P: TPoint); inline;
begin
	FSrcRect.x := P.X;
	FSrcRect.y := P.Y;
end;

procedure TTexture.SetSrcRect(R: TRect);
begin
	with FSrcRect do
	begin
		x := R.Left;
		y := R.Top;
		if R.Width > 0 then
			w := R.Width;
		if R.Height > 0 then
			h := R.Height;
	end;
end;

procedure TTexture.Blit; inline;
begin
	Blit(Point(0, 0));
end;

{ ================================================================================================}
{ TSprite }
{ ================================================================================================}

constructor TSprite.Create;
begin
	inherited Create(GameRenderer, 0, 0);
end;

function TSprite.LoadFromFile(const Filename: String;
	FrameSizeX: Word = 0; FrameSizeY: Word = 0): Boolean;
begin
	//Log('Load: ' + Filename);
	Result := inherited LoadFromFile(Filename);

	if FrameSizeX < 1 then FrameSizeX := Width;
	if FrameSizeY < 1 then FrameSizeY := Height;
	InitFrames(FrameSizeX, FrameSizeY);
end;

procedure TSprite.InitFrames(FrameSizeX, FrameSizeY: Word);
var
	i, px, py: Integer;
begin
	if FrameSizeX < 1 then FrameSizeX := Width;
	if FrameSizeY < 1 then FrameSizeY := Height;

	FrameCount := (Width div FrameSizeX) * (Height div FrameSizeY);
	SetLength(Frames, FrameCount);
	FrameSize.X := FrameSizeX;
	FrameSize.Y := FrameSizeY;

	if FrameCount <= 1 then
	begin
    	SetLength(Frames, 1);
		with Frames[0].Rect do
		begin
			x := 0;
			y := 0;
			w := Width;
			h := Height;
		end;
	end
	else
	begin
		i := 0;
		for py := 0 to (Height div FrameSizeY)-1 do
		for px := 0 to (Width  div FrameSizeX)-1 do
		begin
			with Frames[i].Rect do
			begin
				x := px * FrameSizeX;
				y := py * FrameSizeY;
				w := FrameSizeX;
				h := FrameSizeY;
			end;
			Inc(i);
		end;
	end;

	FDestRect.w := FrameSizeX;
	FDestRect.h := FrameSizeY;

	SDL_DestroyTexture(Texture);
	Texture := SDL_CreateTexture(Renderer, UInt32(SDL_PIXELFORMAT_ARGB8888),
		SInt32(SDL_TEXTUREACCESS_STREAMING), Width, Height);
	UseAlphaChannel := True;
	Alpha := 255;

	Dirty := True;
end;

procedure TSprite.Blit(Frame: Word);
begin
	if Dirty then
	begin
		SDL_UpdateTexture(Texture, nil, @Bits[0], Width * 4);
		Dirty := False;
	end;
	FDestRect.x := X;
	FDestRect.y := Y;
	SDL_RenderCopy(Renderer, Texture, @Frames[Frame].Rect, @FDestRect);
end;

procedure TSprite.Blit(Rect: TRect);
var
	R: TSDL_Rect;
begin
	if Dirty then
	begin
		SDL_UpdateTexture(Texture, nil, @Bits[0], Width * 4);
		Dirty := False;
	end;
	FDestRect.x := X;
	FDestRect.y := Y;

	with R do
	begin
		x := Rect.Left;
		y := Rect.Top;
		w := Rect.Width;
		h := Rect.Height;
	end;

	SDL_RenderCopy(Renderer, Texture, @R, @FDestRect);
end;


end.

