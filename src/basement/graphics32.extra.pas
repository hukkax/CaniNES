unit Graphics32.Extra;

{$mode Delphi}

interface

uses
	Classes, SysUtils,
	Graphics32;

type
	TBitmap32Ext = class helper for TBitmap32
		procedure DropShadow(const R: TRect; Color: TColor32; ShadowDistance: Byte = 3);
		procedure Bevel(R: TRect; Contrast: Byte);

		procedure FillBox(const R: TRect; Color: TColor32; Contrast: Byte; Gradient: Integer);
		procedure DrawSelection(const R: TRect; Color: TColor32); inline;
	end;


implementation

procedure TBitmap32Ext.DropShadow(const R: TRect; Color: TColor32; ShadowDistance: Byte = 3);
begin
	FillRectS(R.Left + ShadowDistance, R.Bottom,
		R.Right + ShadowDistance, R.Bottom + ShadowDistance, Color);
	FillRectS(R.Right, R.Top + ShadowDistance,
		R.Right + ShadowDistance, R.Bottom + ShadowDistance, Color);
end;

procedure TBitmap32Ext.Bevel(R: TRect; Contrast: Byte);
var
	Col: TColor32;
begin
	if Contrast = 0 then Exit;

	ValidateRect(R);

	Col := SetAlpha(clWhite32, Contrast); // light edges
	HorzLineTS(R.Left, R.Top, R.Right-1,  Col);
	VertLineTS(R.Left, R.Top, R.Bottom-1, Col);

	Col := SetAlpha(clBlack32, Contrast); // dark edges
	HorzLineTS(R.Left,    R.Bottom-1, R.Right-1,  Col);
	VertLineTS(R.Right-1, R.Top,      R.Bottom-1, Col);
end;

procedure TBitmap32Ext.FillBox(const R: TRect; Color: TColor32; Contrast: Byte; Gradient: Integer);
begin
	if Gradient < 0 then
		Gradient := -20;
	GradientRect(R, Lighten(Color, -Gradient), Lighten(Color, +Gradient), False);
	Bevel(R, Contrast);
end;

procedure TBitmap32Ext.DrawSelection(const R: TRect; Color: TColor32);
begin
	FillBox(R, Color, 36, 17);
end;

end.

