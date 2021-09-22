unit MenuPage.CartInfo;

{$mode delphi}

interface

uses
	Classes, Types, SysUtils,
	Graphics32,
	Basement.FileCollection,
	NES.Types, MenuHandler;

type
	TCartInfoBrowser = class(TMenuPageClass)
	private
		Images: array [caBoxArt..caLastIndex] of TBitmap32;
	public
		constructor Create(BrowserPage: TMenuPage); overload;
		destructor  Destroy; override;
	end;

	procedure PageEnter_CartInfo(Page: TMenuPage);


implementation

uses
	MainWindow, MainMenu,
	InputBindings, Basement.Renderer.Overlay,
	NES.Config, NES.Console, NES.Cartridge, NES.ROM,
	Logging, Math, FileUtil;

var
	CartInfo: TCartInfoBrowser;

// ============================================================================
// TCartInfoBrowser
// ============================================================================

constructor TCartInfoBrowser.Create(BrowserPage: TMenuPage);
var
	i: Integer;
begin
	inherited Create;

	CartInfo := Self;
	Page := BrowserPage;

	Page.FullScreen := True;
	Page.Width := MenuRenderer.FrameBuffer.Width;
	Dec(Page.ItemHeight, 1);

	for i := caBoxArt to caLastIndex do
		Images[i] := TBitmap32.Create;

	Page.OnEnter := PageEnter_CartInfo;
end;

destructor TCartInfoBrowser.Destroy;
var
	i: Integer;
begin
	for i := caBoxArt to caLastIndex do
		Images[i].Free;

	inherited Destroy;
end;

procedure PageEnter_CartInfo(Page: TMenuPage);
var
	LinesAdded: Integer;

	procedure Add(const Desc, Text: String);
	begin
		if not Text.IsEmpty then
		begin
			Page.AddText(Desc.PadRight(10) + ' ' + Text);
			Inc(LinesAdded);
		end;
	end;

var
	RomData: PRomData;
	W, H, Padding, i, sc: Integer;
	R: TRect;
	ImgPos: array[caBoxArt..caLastIndex] of TRect;
	img: TBitmap32;
	S: String;
	Str: TMemoryStream;
begin
	Str := nil;

	LinesAdded := 2; // header + Back command
	Page.Items.Clear;
	Page.HeaderCaption := 'Cartridge Information';
	Page.AddBackCommand;

	if not Console.GotCartridge then
	begin
		Page.AddEntry('No cartridge loaded', 0);
		Exit;
	end;

	RomData := @Cartridge.RomData;

	W := MenuRenderer.FrameBuffer.Width;
	H := MenuRenderer.FrameBuffer.Height;
	R := Bounds(0, 0, W, H);
	Padding := Min(14, W div 30);


	Add('Filename:',  ExtractFilename(RomData.Info.Filename));
	Add('Title:',     RomData.Info.DatabaseInfo.Title);
	Add('System:',    NESSystemNames[RomData.Info.System]);
	Add('Board:',     RomData.Info.DatabaseInfo.Board);
	Add('Mapper:',    RomData.Info.MapperID.ToString + ' Sub: ' + RomData.Info.SubMapperID.ToString);
	Add('Mirroring:', MirroringTypeNames[RomData.Info.Mirroring]);

	Add('PRG ROM:', Format('%dK (%d)', [RomData.prgSize div 1024, RomData.prgSize]));
	if RomData.chrSize > 0 then
		Add('CHR ROM:', Format('%dK (%d)', [RomData.chrSize div 1024, RomData.chrSize]))
	else
		Add('CHR ROM:', 'None');
	if (RomData.ChrRamSize > 0) or (RomData.Info.IsNes20Header) then
		Add('CHR RAM:', Format('%dK (%d)', [RomData.ChrRamSize div 1024, RomData.ChrRamSize]))
	else
	if RomData.chrSize = 0 then
		Add('CHR RAM:', '8 KB');

	Add('ROM CRC:', RomData.Info.Hash.Crc32.ToHexString);
	Add('PRG CRC:',  RomData.Info.Hash.PrgCrc32.ToHexString);
	Add('PRG+CHR:',  RomData.Info.Hash.PrgChrCrc32.ToHexString);


	for i := caBoxArt to caLastIndex do
	with CartInfo do
	begin
		if not Assigned(Images[i]) then
			Images[i] := TBitmap32.Create;
		img := Images[i];

		case i of // set up image layouts
			caBoxArt:
				R := Rect(
					W div 2,
					MenuRenderer.Font.GlyphHeight * 4,
					W - Padding,
					H - Padding);

			caSnaps:
				R := Rect(
					Padding,
					MenuRenderer.Font.GlyphHeight * LinesAdded,
					Max(Padding, R.Left - Padding), // don't overlap box art horizontally
					H - Padding);

			caTitles: ; // TODO
		end;

		S := RomData.Info.DatabaseInfo.Artwork[i];
		Str := Console.Database.FileList[i].GetFileStream(S);

		if Str <> nil then
		begin
			img.LoadFromStream(Str);
			FreeAndNil(Str);

			// downscale?
			if (img.Height > R.Height) or (img.Width > R.Width) then
				img.ScaleToFit(R.Width, R.Height, False)
			else
			begin
				// upscale?
				if Configuration.Display.GUI.BoxArtUpscale then
				begin
					img.ScaleToFit(R.Width, R.Height, True);
				end
				else
				begin
					sc := Min(R.Height div img.Height, R.Width div img.Width);
					if sc > 1 then
						img.Resize(img.Width*sc, img.Height*sc, rfBox);
				end;
			end;
		end
		else
		begin
			img.SetSize(1, 1);
			img.Clear(0);
		end;

		ImgPos[i] := Bounds(
			Trunc(R.CenterPoint.X) - (img.Width  div 2),
			Trunc(R.CenterPoint.Y) - (img.Height div 2),
			img.Width, img.Height );
	end;

	// add box art image
	with Page.AddEntry('', 0) do
	begin
		Kind  := meImage;
		Image := CartInfo.Images[caBoxArt];

		// adjust location
		//
		if ImgPos[caBoxArt].Bottom < ImgPos[caSnaps].Bottom then
			ImgPos[caBoxArt].Offset(0, ImgPos[caSnaps].Bottom - ImgPos[caBoxArt].Bottom);
		Position := ImgPos[caBoxArt].TopLeft;
	end;

	// add gameplay screenshot
	with Page.AddEntry('', 0) do
	begin
		Kind  := meImage;
		Image := CartInfo.Images[caSnaps];
		Position := ImgPos[caSnaps].TopLeft;
	end;
end;


end.

