unit NES.Palette;

interface

{$MODE DELPHI}
{$RANGECHECKS OFF}
{$MACRO ON}

type
	TPalettePropertyKind = ( ppR, ppG, ppB, ppSaturation, ppHue, ppContrast, ppBrightness, ppGamma );

	TPaletteProperty = record
		Data:    PDouble;
		Caption: String;
	end;

	TNesPaletteClass = class;

	TPaletteEditor = record
		Visible:    Boolean;
		EditedProperty: TPalettePropertyKind;
		PropertyInfo: array[TPalettePropertyKind] of TPaletteProperty;
		ColorIndex: Byte;
		Palette:    TNesPaletteClass;

		procedure Redraw;
		procedure ToggleEnabled;

		procedure PrevProperty;
		procedure NextProperty;
		procedure ModifyValue(Dir: Integer);

		procedure PrevColor;
		procedure NextColor;
		procedure ModifyColor(RGB, Dir: Integer);

		procedure Changed;
	end;

	TNESColors = array[0..7] of array[0..$3F] of Cardinal; // RGB palettes with emphasis

	TNesPaletteClass = class
	private
		procedure SetRawColors;
	public
		Editor: TPaletteEditor;
		Colors: TNESColors;
		RawColors: array[0..63] of array[0..2] of Byte;
		PaletteFileUsed: Boolean;

		constructor Create; overload;

		function  LoadFromFile(const Filename: String): Boolean;
		procedure Fill;
		procedure Generate(saturation, hue_tweak, contrast, brightness, gamma: Double);
	end;


implementation

uses
	SysUtils, Math, Classes,
	Basement.Util, FileStreamEx,
	MainMenu, Graphics32, NTSC.Bisqwit,
	NES.Config, NES.Console, NES.PPU;

// ================================================================================================
// Palette I/O / Generation
// ================================================================================================

constructor TNesPaletteClass.Create;
const
	PropertyNames: array[TPalettePropertyKind] of String = (
		'R','G','B', 'Saturation', 'Hue', 'Contrast', 'Brightness', 'Gamma' );
var
	P: TPalettePropertyKind;
begin
	inherited Create;
	Editor.Palette := Self;

	for P in TPalettePropertyKind do
		Editor.PropertyInfo[P].Caption := PropertyNames[P];

	Editor.PropertyInfo[ppR].Data := nil;
	Editor.PropertyInfo[ppG].Data := nil;
	Editor.PropertyInfo[ppB].Data := nil;
	Editor.PropertyInfo[ppSaturation].Data := @Configuration.Display.Palette.Saturation;
	Editor.PropertyInfo[ppHue].Data        := @Configuration.Display.Palette.HueShift;
	Editor.PropertyInfo[ppContrast].Data   := @Configuration.Display.Palette.Contrast;
	Editor.PropertyInfo[ppBrightness].Data := @Configuration.Display.Palette.Brightness;
	Editor.PropertyInfo[ppGamma].Data      := @Configuration.Display.Palette.Gamma;
end;

// Loads a 64/512 entry RGB palette from file
//
function TNesPaletteClass.LoadFromFile(const Filename: String): Boolean;
var
	Pal: TBytes;
	p, i, x: Integer;
begin
	Result := False;
	i := FileToBytes(Filename, Pal{%H-});
	if i < (64*3) then Exit;

	if i >= (512*3) then
	begin
		x := 0;
		for p := 0 to 7 do
		for i := 0 to 63 do
		begin
			Colors[p,i] := $FF000000 or
				(Pal[x] shl 16) or (Pal[x+1] shl 8) or (Pal[x+2]);
			Inc(x, 3);
		end;
		Log('Loaded full NES palette from %s.', [Filename]);
	end
	else
	begin
		for i := 0 to 63 do
			Colors[0,i] := $FF000000 or
				(Pal[i*3] shl 16) or (Pal[i*3+1] shl 8) or (Pal[i*3+2]);
		Fill;
		Log('Loaded NES palette from %s.', [Filename]);
	end;

	PaletteFileUsed := True;
	Result := True;
end;

// Generates an NTSC palette programmatically
//
procedure TNesPaletteClass.Generate(saturation, hue_tweak, contrast, brightness, gamma: Double);
var
	p, i: Integer;
begin
	if PaletteFileUsed then Exit;

	for p := 0 to 7 do    // emphasis bits
	for i := 0 to 63 do   // palette index
	begin
		Colors[p, i] := MakeRGBcolor(p shl 6 or i, // color with emphasis bits
			saturation, hue_tweak, contrast, brightness, gamma);
	end;
	SetRawColors;
	Fill;
end;

procedure TNesPaletteClass.SetRawColors;
var
	i: Integer;
	C: Cardinal;
begin
	for i := 0 to 63 do   // palette index
	begin
		C := Colors[0, i];
		RawColors[i, 0] := (C >> 16) and $FF;
		RawColors[i, 1] := (C >> 8)  and $FF;
		RawColors[i, 2] := C and $FF;
	end;
end;

// Create the emphasised versions of the 64-color base palette
//
procedure TNesPaletteClass.Fill;
const
	Amount_Add = 0; // values just eyeballed, not based on any measurements
	Amount_Sub = 22;

	function Emph(B: Boolean; C: Integer): Byte;
	begin
		if B then
			Result := Min(C + Amount_Add, 255)
		else
			Result := Max(C - Amount_Sub, 0);
	end;

var
	p, i: Integer;
	r, g, b: Integer;
	C: Cardinal;
begin
	SetRawColors;

	for p := 1 to 7 do
	begin
		for i := 0 to 63 do
		begin
			C := Colors[0,i];

			if not (i in [$00..$0D, $10..$1D, $20..$2D, $30..$3D]) then
			begin
				// $0F black is not affected by color emphasis
				Colors[p,i] := C;
			end
			else
			if p = 7 then
			begin
				//Setting all three emphasis bits will darken colors
				r := Emph(False, RedComponent(C));
				g := Emph(False, GreenComponent(C));
				b := Emph(False, BlueComponent(C));
				Colors[p,i] := Color32(r, g, b);
			end
			else
			begin
				r := Emph(p and 1 <> 0, RedComponent(C));
				g := Emph(p and 2 <> 0, GreenComponent(C));
				b := Emph(p and 4 <> 0, BlueComponent(C));
				Colors[p,i] := Color32(r, g, b);
			end;
		end;
	end;

	if NES_PPU <> nil then
		NES_PPU.UpdatePalette;
end;

// ================================================================================================
// Palette Editor
// ================================================================================================

procedure TPaletteEditor.Redraw;
var
	X, Y, C: Integer;
	R: TRect;
	Val: Double;
begin
	with MenuRenderer do
	begin
		FrameBuffer.Clear(0);

		if not Visible then Exit;

		DrawString(1, 1, PropertyInfo[EditedProperty].Caption, clWhite32);

		if PropertyInfo[EditedProperty].Data = nil then // RGB value
			DrawString(1, 3, ColorIndex.ToHexString(2) + ':' +
				Palette.Colors[0, ColorIndex].ToHexString(6), clWhite32)
		else
		begin
			Val := Double(PropertyInfo[EditedProperty].Data^);
			DrawString(1, 3, Val.ToString(ffFixed, 3, 3), clWhite32);
		end;

		for Y := 0 to 3 do
		for X := 0 to 15 do
		begin
			R := Bounds(X*8+128, Y*8, 8, 8);
			C := Y*16+X;
			Framebuffer.FillRectS(R, Palette.Colors[0, C]);
			if C = ColorIndex then
				FrameBuffer.FrameRect(R, $FF99FF00);
		end;

	end;
end;

procedure TPaletteEditor.ToggleEnabled;
var
	Stream: TFileStreamEx;
	C: Cardinal;
	i: Integer;
begin
	Visible := not Visible;

	// save palette
	if not Visible then
	begin
		Stream := TFileStreamEx.Create(AppPath + 'canines.pal', fmCreate);

		for i := 0 to 63 do
		begin
			C := Palette.Colors[0, i];
			Stream.Write8((C and $00FF0000) shr 16);
			Stream.Write8((C and $0000FF00) shr 8);
			Stream.Write8((C and $000000FF));
		end;

		Stream.Free;
	end;

	Redraw;
end;

procedure TPaletteEditor.PrevProperty;
begin
	if EditedProperty = Low(TPalettePropertyKind) then
		EditedProperty := High(TPalettePropertyKind)
	else
		Dec(EditedProperty);
	Redraw;
end;

procedure TPaletteEditor.NextProperty;
begin
	if EditedProperty = High(TPalettePropertyKind) then
		EditedProperty := Low(TPalettePropertyKind)
	else
		Inc(EditedProperty);
	Redraw;
end;

procedure TPaletteEditor.PrevColor;
begin
	if ColorIndex > 0 then
		Dec(ColorIndex)
	else
		ColorIndex := 63;
	Redraw;
end;

procedure TPaletteEditor.NextColor;
begin
	if ColorIndex < 63 then
		Inc(ColorIndex)
	else
		ColorIndex := 0;
	Redraw;
end;

procedure TPaletteEditor.ModifyValue(Dir: Integer);
const
	Step = 0.05;
var
	V: Double;
begin
	case EditedProperty of
		ppR: ModifyColor(0, Dir);
		ppG: ModifyColor(1, Dir);
		ppB: ModifyColor(2, Dir);
		else
			if Dir > 0 then V := Step else V := -Step;
			PropertyInfo[EditedProperty].Data^ += V;
			Changed;
	end;
end;

procedure TPaletteEditor.Changed;
begin
	Palette.Generate(
		Configuration.Display.Palette.Saturation,
		Configuration.Display.Palette.HueShift,
		Configuration.Display.Palette.Contrast,
		Configuration.Display.Palette.Brightness,
		Configuration.Display.Palette.Gamma);

	if NES_PPU <> nil then
		NES_PPU.UpdatePalette;

	Redraw;
end;

procedure TPaletteEditor.ModifyColor(RGB, Dir: Integer);
var
	C: Cardinal;
	R, G, B: Byte;
begin
	C := Palette.Colors[0, ColorIndex];
	R := (C and $00FF0000) shr 16;
	G := (C and $0000FF00) shr 8;
	B := (C and $000000FF);
	case RGB of
		0: R := (R + Dir) mod 255;
		1: G := (G + Dir) mod 255;
		2: B := (B + Dir) mod 255;
	end;
	Palette.Colors[0, ColorIndex] := $FF000000 or (R shl 16) or (G shl 8) or (B);
	Palette.Fill;
	Redraw;
end;


end.
