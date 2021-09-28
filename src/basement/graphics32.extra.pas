unit Graphics32.Extra;

{$mode Delphi}

interface

uses
	Classes, SysUtils,
	Graphics32;

type
	TBitmap32Ext = class helper for TBitmap32
		procedure DropShadow(const R: TRect; Color: Cardinal; ShadowDistance: Byte = 3);
		procedure FillBox(SR: TRect; Color: Cardinal; Contrast: Integer; Gradient: Integer);
		procedure DrawSelection(const R: TRect; Color: Cardinal); inline;
	end;


implementation

procedure TBitmap32Ext.DropShadow(const R: TRect; Color: Cardinal; ShadowDistance: Byte = 3);
begin
	FillRectS(R.Left + ShadowDistance, R.Bottom,
		R.Right + ShadowDistance, R.Bottom + ShadowDistance, Color);
	FillRectS(R.Right, R.Top + ShadowDistance,
		R.Right + ShadowDistance, R.Bottom + ShadowDistance, Color);
end;

procedure TBitmap32Ext.FillBox(SR: TRect; Color: Cardinal; Contrast: Integer; Gradient: Integer);
var
	Col: Cardinal;
	A, Y: Integer;
begin
	FillRectS(SR, Color);

	if Gradient <> 0 then
	begin
		A := 0;
		for Y := SR.Top to SR.Bottom-1 do
		begin
			Col := Lighten(Color, A);
			HorzLineS(SR.Left, Y, SR.Right, Col);
			Inc(A, Gradient);
		end;
	end;

	if Contrast <> 0 then
	begin
		Col := Lighten(Color, +Contrast);
		LineS(SR.Left, SR.Top, SR.Right, SR.Top,    Col, False);
		LineS(SR.Left, SR.Top, SR.Left,  SR.Bottom, Col, False);
		Col := Lighten(Color, -Contrast);
		SR.Bottom := SR.Bottom - 1;
		LineS(SR.Left,  SR.Bottom, SR.Right, SR.Bottom, Col, False);
		LineS(SR.Right, SR.Top,    SR.Right, SR.Bottom, Col, True);
	end;
end;

procedure TBitmap32Ext.DrawSelection(const R: TRect; Color: Cardinal);
begin
	FillBox(R, Lighten(Color, -20), 50, 2);
end;

end.

