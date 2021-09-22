unit TextOutput;

{$MODE DELPHI}

{$I basement.inc}
{$I canines.inc}

interface

uses
	Classes, SysUtils,
	Graphics32,
	Basement.Renderer.Overlay;

const
	OSD_PADDING = 6;

	function FixChars(const S: String): AnsiString; inline;

	procedure Log(const S: AnsiString); overload;
	procedure Log(const S: AnsiString; const Args: array of const); overload;

	procedure LogVerbose(const S: AnsiString); overload;
	procedure LogVerbose(const S: AnsiString; const Args: array of const); overload;

	procedure Message(S: AnsiString); overload;
	procedure Message(S: AnsiString; const Args: array of const); overload;

	procedure DebugMessage(S: AnsiString); overload;
	procedure DebugMessage(S: AnsiString; const Args: array of const); overload;

	procedure OSD(const Bitmap: TBitmap32); overload;
	procedure OSD(Text: String); overload;
	procedure UpdateOSD;

var
	OSDLayer: TOverlayRenderer;


implementation

uses
	Math,
	{$IFDEF RENDER_FONT_FREETYPE}
	{LazUTF8,} Graphics32.FreeType,
	{$ENDIF}
	Basement.Util, Basement.Font,
	MenuHandler, MainWindow,
	NES.Config;

var
	OSDTimeout: Cardinal;
	PreviousDebugMessage: AnsiString;


function FixChars(const S: String): AnsiString;
begin
	Result := S;
	{$IFNDEF RENDER_FONT_FREETYPE}
	SetCodePage(RawByteString(Result), cp_utf8, False);
	SetCodePage(RawByteString(Result), 28605, True);
	{$ENDIF}
end;

// ========================================================================
// Logging
// ========================================================================

procedure Log(const S: AnsiString); overload;
begin
	Basement.Util.Log(S);
end;

procedure Log(const S: AnsiString; const Args: array of const); overload;
begin
	Basement.Util.Log(Format(S, Args));
end;

procedure LogVerbose(const S: AnsiString); overload;
begin
	{$IFDEF VERBOSE}
	Basement.Util.Log(S);
	{$ENDIF}
end;

procedure LogVerbose(const S: AnsiString; const Args: array of const); overload;
begin
	{$IFDEF VERBOSE}
	Basement.Util.Log(Format(S, Args));
	{$ENDIF}
end;

procedure Message(S: AnsiString); overload;
begin
	OSD(S);
	Basement.Util.Log(S);
end;

procedure Message(S: AnsiString; const Args: array of const); overload;
begin
	Message(Format(S, Args));
end;

procedure DebugMessage(S: AnsiString); overload;
begin
	if S <> PreviousDebugMessage then
	begin
		PreviousDebugMessage := S;
		LogVerbose(S);
		OSD(S);
	end;
end;

procedure DebugMessage(S: AnsiString; const Args: array of const); overload;
begin
	Message(Format(S, Args));
end;

// ========================================================================
// OSD
// ========================================================================

procedure ShowOSD;
begin
	OSDLayer.Align(OSDLayer.Alignment);
	OSDLayer.Opacity := Configuration.Display.GUI.OSD.Opacity;
	OSDTimeout := Trunc(Window.Video.DesiredFramerate * Configuration.Display.GUI.OSD.Timeout);
end;

procedure OSD(const Bitmap: TBitmap32);
begin
	if (OSDLayer = nil) or (OSDLayer.Font = nil) then Exit;

	OSDLayer.FrameBuffer.Clear(0);

	if Bitmap <> nil then
	begin
		if (Bitmap.Width > OSDLayer.FrameBuffer.Width) or
		   (Bitmap.Height > OSDLayer.FrameBuffer.Height) then
				OSDLayer.Resize(Bitmap.Width, Bitmap.Height);

		OSDLayer.FrameBuffer.Draw(0, 0, Bitmap);
		OSDLayer.SetActiveArea(Bitmap.BoundsRect);
	end;

	ShowOSD;
end;

procedure OSD(Text: String);
var
	W, H: Integer;
	R: TRect;
	Alignment: TFreeTypeAlignments;
begin
	if (OSDLayer = nil) or (OSDLayer.Font = nil) then Exit;

	ShowOSD;

	if Text.IsEmpty then Exit;

	W := OSDLayer.FrameBuffer.Width;
	H := OSDLayer.FrameBuffer.Height;

	{$IFDEF RENDER_FONT_FREETYPE}
	//TW := UTF8Length(Text);

	with TRendererFontFreeType(OSDLayer.Font) do
	begin
		//TW := Trunc(Font.TextWidth(Text));

		OSDLayer.FrameBuffer.Clear(Palette[COLOR_MESSAGE_BG]);

		Alignment := [ftaTop];
		if OSDLayer.Alignment in [laTopCenter, laBottomCenter, laCenter] then
			Include(Alignment, ftaCenter)
		else
		if OSDLayer.Alignment in [laTopRight, laBottomRight, laCenterRight] then
			Include(Alignment, ftaRight);

		Font.DrawTextRect(OSDLayer.FrameBuffer, Text,
			OSD_PADDING, OSD_PADDING, W-OSD_PADDING, H+FontOffsetY+0,
			Palette[COLOR_MESSAGE_FG], Alignment);

		R := OSDLayer.FrameBuffer.CroppedRect(Palette[COLOR_MESSAGE_BG], OSD_PADDING);

		OSDLayer.SetActiveArea(R);
	end;

	{$ELSE}

	Text := FixChars(Text);
	TW := Length(Text);
	OSDLayer.DrawRect(0, 1, TW, 1, Palette[COLOR_MESSAGE_BG], 4);
	OSDLayer.DrawString(0, 1, Text, Palette[COLOR_MESSAGE_FG]);

	{$ENDIF}
end;

procedure UpdateOSD;
var
	O: Double;
begin
	if OSDTimeout > 0 then
	begin
		O := Configuration.Display.GUI.OSD.Opacity;

		if OSDTimeout < 100 then
			OSDLayer.Opacity := IfThen(Menu.Visible, O, Min(O, OSDTimeout / 100));

		Dec(OSDTimeout);
		if OSDTimeout <= 0 then
		begin
			OSDLayer.FrameBuffer.Clear(0);
			if Menu.Visible then Menu.Draw(True); // force repaint
			OSDLayer.Opacity := O;
		end;
	end;
end;

initialization

	OSDLayer := nil;

end.

