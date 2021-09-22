(* FreePascal wrapper for Blargg's nes_ntsc DLL
   DLL and interface provided by muzzy
 *)

unit NES.NTSC.DLL;

interface

uses
	Classes, SysUtils;

{$MACRO ON}
{$DEFINE DLLCall:=cdecl; external nesntsc_dll}

const
	nesntsc_dll =
	{$IFDEF MSWINDOWS}'UrsiNES_NTSC.dll';  {$ENDIF}
	{$IFDEF LINUX}    'ursines_ntsc.so';   {$ENDIF}
	{$IFDEF MACOS}    'ursines_ntsc.dylib';{$ENDIF}

type
	// Image parameters, ranging from -1.0 to 1.0.
	TBlarggFloatParam = (
	    // Basic parameters
		BLARGG_Hue        = 0, // -1 = -180 degrees   +1 = +180 degrees
		BLARGG_Saturation = 1, // -1 = grayscale      +1 = oversaturated colors
		BLARGG_Contrast   = 2, // -1 = dark           +1 = light
		BLARGG_Brightness = 3, // -1 = dark           +1 = light
		BLARGG_Sharpness  = 4, // edge contrast enhancement/blurring

		// Advanced parameters
		BLARGG_Gamma      = 5, // -1 = dark           +1 = light
		BLARGG_Resolution = 6, // image resolution
		BLARGG_Artifacts  = 7, // artifacts caused by color changes
		BLARGG_Fringing   = 8, // color artifacts caused by brightness changes
		BLARGG_Bleed      = 9  // color bleed (color resolution reduction)
	);

	TBlarggPreset = (
	    BLARGG_Composite  = 0, // color bleeding + artifacts
	    BLARGG_SVideo     = 1, // color bleeding only
	    BLARGG_RGB        = 2, // crisp image
	    BLARGG_Monochrome = 3  // desaturated + artifacts
	);


	// Settings
	//
	procedure UrsiNES_NTSC_blargg_load_preset(preset: TBlarggPreset); DLLCall;
	procedure UrsiNES_NTSC_blargg_setf(param: TBlarggFloatParam; value: Single); DLLCall;
	procedure UrsiNES_NTSC_blargg_set_merge(merge: Boolean); DLLCall;
	procedure UrsiNES_NTSC_blargg_set_base_palette(base_palette: Pointer); DLLCall; // array of 64*3 bytes

	// Init
	//
	function  UrsiNES_NTSC_get_output_width(input_width: Integer): Integer; DLLCall;

	// Blit
	//
	procedure UrsiNES_NTSC_blit(buf_in: Pointer;
		in_width, in_height, in_pitch, burst_phase: Integer;
		buf_out: Pointer; out_pitch: Integer); DLLCall;

	// Debug
	//
	// gets the pointer to the raw table[] for debugging purposes
	function UrsiNES_NTSC_raw_blargg_ntsc: Pointer; DLLCall;


implementation

end.
