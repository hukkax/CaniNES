unit InfoBoxDisplay;

{$MODE DELPHI}

{$I canines.inc}

interface

uses
	Classes, SysUtils, Basement.Renderer.Overlay;

const
	INFOBOX_WIDTH  = 16;
	INFOBOX_HEIGHT = 8;

type
	TInfoBox = class
	const
		Padding = 4;
	private
		DisplaySize: Byte;
	public
		Layer: TOverlayRenderer;

		procedure Realign;
		procedure Update;
		procedure Toggle;

		constructor Create;
	end;

var
	InfoBox: TInfoBox;


implementation

uses
	NES.Types, NES.Config, NES.Console, NES.Mapper, NES.APU.Mixer,
	Graphics32, {$IFNDEF MEASURETIMING} SDL2, {$ENDIF}
	Basement.Window, MainWindow;

{ TInfoBox }

constructor TInfoBox.Create;
begin
	inherited Create;

	Layer := TOverlayRenderer.Create(Window, 'DebugInfo',
		INFOBOX_WIDTH  * FontManager.Font.GlyphWidth + 6,
		INFOBOX_HEIGHT * FontManager.Font.GlyphHeight,
		0);
	Layer.Font := FontManager.Font;

	Realign;
end;

procedure TInfoBox.Realign;
var
	R: TRect;
	H: Integer;
begin
	Layer.Align(Configuration.Display.GUI.OSD.Alignments.InfoBox,
		Configuration.Display.GUI.OSD.Alignments.Margin);

	R := Layer.FrameBuffer.BoundsRect;
	H := FontManager.Font.GlyphHeight;

	if Configuration.Application.ShowFrameTime = 1 then
		R.Bottom := H * 3;

	Dec(H, Padding);
	R.Top := R.Top + H;
	R.Bottom := R.Bottom - H;
	R.Left := FontManager.Font.GlyphWidth - Padding;

	Layer.SetActiveArea(R);
end;

procedure TInfoBox.Toggle;
begin
	with Configuration.Application do
	begin
		if ShowFrameTime < 2 then
			Inc(ShowFrameTime)
		else
			ShowFrameTime := 0;

		Layer.Visible := ShowFrameTime > 0;
		Layer.FrameBuffer.Clear(0);
	end;
end;

procedure TInfoBox.Update;
var
	i: Integer;
begin
	Layer.Visible := True;

	i := Configuration.Application.ShowFrameTime;
	if i <> DisplaySize then
	begin
		DisplaySize := i;
		Realign;
	end;

	with Layer do
	begin
		Dec(i); // range to 0-1

		DrawRect(1, 1, INFOBOX_WIDTH, 1 + (i*5), $88000000, Padding); // darken the background

		// time it took to render previous frame
		if Console.FastForward then
			DrawString(1, 1, '>>', clWhite32)
		else
		if Console.Rewind then
			DrawString(1, 1, '<<', clWhite32)
		else
		begin
			{$IFDEF MEASURETIMING}
			if TimingInfo.Frame < 10.0 then
				DrawString(1, 1, Format('%.1f', [TimingInfo.Frame]), clWhite32)
			else
				DrawString(1, 1, IntToStr(Trunc(TimingInfo.Frame)), clRed32);
			{$ELSE}
			DrawString(1, 1, IntToStr(SDL_GetTicks - Tick), clWhite32);
			{$ENDIF}
		end;

		// current mirroring type
		DrawString(INFOBOX_WIDTH-1, 1, ShortMirroringTypeNames[Mapper.GetMirroringType], clWhite32);

		// name of the system being emulated
		DrawString( 5, 1, ShortNESSystemNames[Cartridge.RomData.Info.System], clWhite32);

		if i > 0 then
		begin
			{$IFDEF MEASURETIMING}
			DrawString(1, 2, Format('CPU:%.1f', [TimingInfo.CPU]), clWhite32);
			{$IFDEF MEASURETIMING_PPU}
			DrawString(9, 2, Format('PPU:%.1f', [TimingInfo.PPU]), clWhite32);
			{$ENDIF}
			{$ENDIF}

			DrawString(1, 3, 'Latency: ' + Trunc(NES_APU.AverageLatency).ToString + ' ms', clWhite32);
			DrawString(1, 4, 'Underruns: ' + NES_APU.BufferUnderrunEventCount.ToString, clWhite32);
			DrawString(1, 5, 'Rate: ' +
				Trunc(Configuration.Audio.SampleRate * Mixer.RateAdjustment).ToString + ' Hz', clWhite32);
			DrawString(1, 6, 'Mapper: ' + Cartridge.nMapperID.ToString + '.' +
				Cartridge.RomData.Info.SubMapperID.ToString, clWhite32);

		end;
	end;

	Layer.Visible := Configuration.Application.ShowFrameTime > 0;
end;


end.

