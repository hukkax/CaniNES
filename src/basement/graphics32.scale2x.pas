(*
32-bit only scale2x for SDL; by James Hofmann 2007
based on Pete Shinners' SDL scale2x.
Doubles the size of a surface with a smoothing algorithm.
This code is public domain.
*)

{$inline on}
{$mode objfpc}

unit Graphics32.Scale2X;

interface

uses
	SDL2;


	procedure Scale2x(const src: PSDL_Surface; var dst: PSDL_Surface);


implementation


function max(const a, b: longint): longint; inline;
begin
	// note: if we were to use unsigned numbers the border checks would
	// cause a crash because there would be a negative overflow
	if a > b then Result := a else Result := b;
end;

function min(const a, b: longint): longint; inline;
begin
	if a < b then Result := a else Result := b;
end;

(*
this requires a destination surface already setup to be twice as
large as the source. oh, and formats must match too. this will just
blindly assume you didn't flounder.
*)

procedure Scale2x(const src: PSDL_Surface; var dst: PSDL_Surface);
var
	srclinewidth, dstlinewidth,
	width, height,
	looph, loopw,
	E0, E1, E2, E3, B, D, E, F, H: longint;
	srcpix, dstpix: ^longint;
begin
	srclinewidth := (src^.pitch) div SizeOf(longword);
	dstlinewidth := (dst^.pitch) div SizeOf(longword);
	width  := src^.w;
	height := src^.h;

	srcpix := src^.pixels;
	dstpix := dst^.pixels;

	for looph := 0 to height-1 do
	begin
		for loopw := 0 to width-1 do
		begin
			{ from this grid we get the values BDEFH:
			ABC
			DEF
			GHI

			and map them into a E0,E1,E2,E3:
			E0 E1
			E2 E3

			we must also account for the borders(using the pixels nearest to border)
			}

			// increment each pointer to the requested pixel

			B := (srcpix + srclinewidth * Max(0, looph-1) + loopw)^;
			D := (srcpix + srclinewidth * looph + Max(0, loopw-1))^;
			E := (srcpix + srclinewidth * looph + loopw)^;
			F := (srcpix + srclinewidth * looph + Min(width-1, loopw+1))^;
			H := (srcpix + srclinewidth * Min(height-1, looph+1) + loopw)^;

			// determine smoothing result

			if ((D = B) and (B <> F) and (D <> H)) then e0 := D else e0 := E;
			if ((B = F) and (B <> D) and (F <> H)) then e1 := F else e1 := E;
			if ((D = H) and (D <> B) and (H <> F)) then e2 := D else e2 := E;
			if ((H = F) and (D <> H) and (B <> F)) then e3 := F else e3 := E;

			// blit the doubled pixel

			(dstpix +  (looph*2    * dstlinewidth +  loopw*2))^    := E0;
			(dstpix +  (looph*2    * dstlinewidth + (loopw*2+1)))^ := E1;
			(dstpix + ((looph*2+1) * dstlinewidth +  loopw*2))^    := E2;
			(dstpix + ((looph*2+1) * dstlinewidth + (loopw*2+1)))^ := E3;
		end;
	end;
end;

end.

