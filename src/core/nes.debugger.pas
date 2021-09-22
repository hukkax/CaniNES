unit NES.Debugger;

{$mode delphi}
{$I canines.inc}

interface

uses
	Classes, SysUtils, Types, Math,
	Graphics32, SDL2,
	NES.Types, NES.Config,
	NES.Console, NES.Cartridge, NES.Mapper, NES.ROM,
	NES.MemoryManager,
	NES.CPU, NES.PPU, NES.APU,
	NES.APU.Mixer;

var
	DebugView: ( ShowDisasm, ShowAPU );
	pattpal: Integer = 0;


	function  UpdateDebugger: Boolean;
	procedure DrawDebugger;


implementation

uses
	MainWindow,
	Basement.Renderer.Overlay;

var
	a, x, y: Integer;


function UpdateDebugger: Boolean;
begin
	Result := False;

	case EmulationMode of

		PAUSED:
		begin
			Window.Framebuffer.FillRectS(256, 0,
				Window.FrameBuffer.Width-1, Window.FrameBuffer.Height-1, $FF112233);
			OverlayRenderer.DrawString(1, 53, 'PAUSED');
			Pause := True;
			Result := True;
		end;

		STEP_INSTRUCTION: // I: Emulate code step-by-step
		begin
			Console.Step;
			NES_CPU.Disassemble(NES_CPU.PC, Min($FFFF,NES_CPU.PC+32));
			Pause := True;
			Result := True;
		end;

		// Shift-I: Run until entered or left IRQ
		STEP_IRQ:
		begin {
			a := NES.CPU.status and FLAG_I;
			repeat
				Console.Clock;
			until (NES_CPU.status and FLAG_I) <> a; }
			Pause := True;
			Result := True;
		end;

		// Ctrl-I: Run until stack pointer increases
		STEP_SUBROUTINE:
		begin {
			a := NES_CPU.stkp;
			if a < $FF then
			begin
				x := 0;
				repeat
					Console.Clock;
					Inc(x);
				until (x > 29780*100) or (NES_CPU.stkp > a);
			end; }
			Pause := True;
			Result := True;
		end;

		// S: Run until PPU scanline changed
		STEP_SCANLINE:
		begin
			a := NES_PPU.scanline;
			repeat
				Console.Clock;
			until NES_PPU.scanline <> a;
			Pause := True;
			Result := True;
		end;

		// Shift-S: Run until entering VBlank
		STEP_VBL:
		begin
			repeat
				Console.Clock;
			until NES_PPU.scanline = 241;
			Pause := True;
			Result := True;
		end;

	end;
end;

procedure DrawDebugger;
var
	SF: AnsiString; // flags
	//oc: Byte;
	S: String;
begin
	OverlayRenderer.FrameBuffer.Clear(0);

	NES_PPU.GetPatternTable(0, pattpal, 256, 0);
	NES_PPU.GetPatternTable(1, pattpal, 256+128, 0);

	{for y := 0 to 15 do
	for x := 0 to 15 do
		OverlayRenderer.DrawString(
			IntToHex(NES_CPU.Peek((y*16+x) + $0000, True), 2),
			x*2, y, 32);}

	if DebugView = ShowDisasm then
	begin
		x := 37-2;
		y := 45-1;
		a := NES_CPU.PS; //status;
		SF := '        ';

		with OverlayRenderer do
		begin
			DrawString(x, y+0, 'PC '+ IntToHex(NES_CPU.PC, 4));
			DrawString(x, y+1, 'A: '+ IntToHex(NES_CPU.A, 2));
			DrawString(x, y+2, 'X: '+ IntToHex(NES_CPU.X, 2));
			DrawString(x, y+3, 'Y: '+ IntToHex(NES_CPU.Y, 2));
			DrawString(x, y+4, 'SP '+ IntToHex(NES_CPU.SP, 2));
			DrawString(x, y+5, 'ST '+ IntToHex(a, 2));
			DrawString(x+6, y+5, 'CZIDBUVN');
			//DrawString(NES.CPU.sInterrupt, x, y+6, 33);
		end;

		if (a and FLAG_C) <> 0 then SF[1] := 'C'; // Carry Bit
		if (a and FLAG_Z) <> 0 then SF[2] := 'Z'; // Zero
		if (a and FLAG_I) <> 0 then SF[3] := 'I'; // Disable Interrupts
		if (a and FLAG_D) <> 0 then SF[4] := 'D'; // Decimal Mode (unused)
		if (a and FLAG_B) <> 0 then SF[5] := 'B'; // Break
		if (a and FLAG_U) <> 0 then SF[6] := 'U'; // Unused
		if (a and FLAG_V) <> 0 then SF[7] := 'V'; // Overflow
		if (a and FLAG_N) <> 0 then SF[8] := 'N'; // Negative

		OverlayRenderer.DrawString(x+6, y+5, SF);

		//NES.PPU.DrawString('Cycles=' + IntToStr(NES.CPU.cpu_time), x+6, y+6, 32);

		// PPU status
		if Pause then
		with OverlayRenderer do
		begin
			DrawString(x, y+7, 'Scanline: ' + IntToStr(NES_PPU.scanline));
			DrawString(x, y+8, 'PPUSTATUS:' + IntToHex(NES_PPU.PeekRAM(0), 2));
		end;

		y := 23;
		a := NES_CPU.PC;
		a := a - (a mod 8);
		for x := Max(0, a-$12) to a+$12 do
		begin
			if NES_CPU.PC = x then
			begin
				if mapLines[x] = '' then
					NES_CPU.Disassemble(NES_CPU.PC, Min(NES_CPU.PC+32, $FFFF));
				OverlayRenderer.DrawString(1, y+8, mapLines[x]);
				Inc(y);
			end
			else
			if (x <= High(mapLines)) and (mapLines[x] <> '') then
			begin
				OverlayRenderer.DrawString(1, y+8, mapLines[x], clRed32);
				Inc(y);
			end;
		end;

		// Draw OAM Contents (first 26 out of 64) ======================================
		{
		for a := 0 to 25 do
			NES.PPU.DrawString(Format('%2x: (%3d, %3d) ID:%2x AT:%2x', [a,
				NES.PPU.pOAM[a * 4 + 3],  NES.PPU.pOAM[a * 4 + 0],
				NES.PPU.pOAM[a * 4 + 1],  NES.PPU.pOAM[a * 4 + 2]]), 35, 17 + a, 3);}
	end
	else
	if DebugView = ShowAPU then
	with NES_PPU do
	begin
		X := 1;
		Y := 31;
		a := 34;

	{	DrawString('Square1      Square2      Triangle     Noise     M  DMC      LI', X, Y+00, 33);

		DrawString('Period XXXX  Period XXXX  Period XXXX  Period XXXX  Period XXXX', X, Y+02, a);
		DrawString('Timer  XXXX  Timer  XXXX  Timer  XXXX  Timer  XXXX  Timer  XXXX', X, Y+03, a);
		DrawString('Freq   XXXX  Freq   XXXX  Freq   XXXX  Freq   XXXX  Freq   XXXX', X, Y+04, a);
		DrawString('Duty X DP X  Duty X DP X  SeqPos  XXX  Shift  XXXX  Addr   XXXX', X, Y+05, a);

		DrawString('Env:  S L C  Env:  S L C  1  Sweep  2  Env:  S L C  Frame Ctr:', X, Y+07, a);
		DrawString('Counter XXX  Counter XXX  Ena N Ena N  Counter XXX  [X] 5-Step', X, Y+08, a);
		DrawString('Divider XXX  Divider XXX  P:XXX P:XXX  Divider XXX  [X] IRQ',    X, Y+09, a);
		DrawString('Volume  XXX  Volume  XXX  S:XXX S:XXX  Volume  XXX  Step: XXXX', X, Y+10, a);

		DrawString('LengthCtr H  LengthCtr H  LengthCtr H  LengthCtr H', X, Y+12, a);
		DrawString('Counter XXX  Counter XXX  Counter XXX  Counter XXX', X, Y+13, a);
		DrawString('Reload  XXX  Reload  XXX  Reload  XXX  Reload  XXX', X, Y+14, a);}
{
		NES_APU.Debug;

		Y := 31;
		for a := 0 to 11 do
		begin
			if a in [1, 5, 9] then Inc(y);
			OverlayRenderer.DrawString(ChDebug[1,a], X, Y+a, 34);
		end;

		Inc(X, 12);
		Y := 31;
		for a := 0 to 11 do
		begin
			if a in [1, 5, 9] then Inc(y);
			OverlayRenderer.DrawString(ChDebug[2,a], X, Y+a, 34);
		end;

		Inc(X, 12);
		Y := 31;
		for a := 0 to 11 do
		begin
			if a in [1, 5, 9] then Inc(y);
			OverlayRenderer.DrawString(ChDebug[4,a], X, Y+a, 34);
		end;
}
	end;

	{
	// show palette ram
	a := 16;
	for y := 0 to 3 do
	for x := 0 to 3 do
	begin
		Window.FrameBuffer.FillRect(x*a, y*a+24, x*a+(a-1), y*a+(a-1)+24,
		NES_PPU.Palette.Colors[0,NES_PPU.Paletteram[y*4+x]]);
		//NES_PPU.tblPalette[y*4+x+0] := Random($3F);
	end;
	for y := 0 to 3 do
	for x := 0 to 3 do
		Window.FrameBuffer.FillRect(x*a+(a*5), y*a+24, x*a+(a-1)+(a*5), y*a+(a-1)+24,
		NES_PPU.Palette.Colors[0,NES_PPU.Paletteram[y*4+x+16]]);
	}

	a := $6000;  //<-prg ram   $2BC0; <-vram
	for y := 0 to 13 do
	begin
		S := '';
		for x := 0 to 15 do
		begin
			S := S + IntToHex(MemoryManager.DebugRead(a), 2);
			//S := S + IntToHex(Mapper.ReadVRAM(a), 2);
			Inc(a);
		end;
		OverlayRenderer.DrawString(32, Y+16, S);
	end;

end;


end.

