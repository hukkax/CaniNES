unit NES.Config;

{$MODE DELPHI}

interface

uses
	Classes, SysUtils,
	NES.Types,
	ConfigurationManager;

const
	MRUcount = 20;

type
	TGUIColor = (
		COLOR_MESSAGE_FG,			COLOR_MESSAGE_BG,

		COLOR_MENU_BACKGROUND,		COLOR_MENU_SHADOW,
		COLOR_MENU_SELECTION,		COLOR_MENU_HEADER,
		COLOR_MENU_NORMAL,			COLOR_MENU_SETTING,
		COLOR_MENU_HEADING,			COLOR_MENU_BACKBUTTON,

		COLOR_SCROLLBAR_FG,			COLOR_SCROLLBAR_BG,

		COLOR_FILE_DRIVES,			COLOR_FILE_DRIVE,
		COLOR_FILE_PARENT,			COLOR_FILE_DIRECTORY,
		COLOR_FILE_EXTRA,			COLOR_FILE_ROM,
		COLOR_FILE_CURRENT,			COLOR_FILE_FAVOURITED,
		COLOR_FILE_MOVIE,			COLOR_FILE_ARCHIVE,
		COLOR_FILE_BAD,

		COLOR_SAVESLOT_TEXT,		COLOR_SAVESLOT_MESSAGE,
		COLOR_SAVESLOT_BORDER,		COLOR_SAVESLOT_SELECTED,
		COLOR_SAVESLOT_EXISTS,		COLOR_SAVESLOT_EMPTY,

		COLOR_PAD_BACKGROUND,		COLOR_PAD_BORDER,
		COLOR_PAD_INACTIVE,			COLOR_PAD_ACTIVE
	);

var
	Palette: array [TGUIColor] of Cardinal;

type
	// -----------------------------------------------

	TApplicationConfig = record
		HighPriority:     Boolean; // Set high priority on Windows?
		EnableDatabase:   Boolean; // Enable built-in game database?
		RestoreROMOnStartup: Boolean; // Restore last loaded ROM on startup?
		RestoreROMState:  Boolean; // Restore last ROM state on load?
		IPSAutoPatch:     0..2;    // Auto-apply IPS patches?
		ShowFrameTime:    0..2;
		DateTimeFormat:   String;
		DefaultROMPath:   String;
		RecordingPath:    String;
		ImagePath_Boxart,
		ImagePath_Snaps,
		ImagePath_Titles: String;
		LastROMFile:      String;  // Path to most recently opened ROM
		MRU:              array[0..MRUcount-1] of String;
	end;

	// -----------------------------------------------

	TWindowConfig = record
		X, Y:        Integer;
		AspectRatio: Byte;
		MaxScale:    Byte;
	end;

	TUIConfig = record
		Scale:         Byte;
		BoxArtUpscale: Boolean; // False = only pixel double boxart, True = free scaling
		FontName:      String;
		OSD: record
			Opacity,
			PadsOpacity: Double;  // 0..1
			Timeout:     Byte;    // Show OSD for this many seconds before fading
			Alignments: record
				Margin:  Byte;
				InfoBox,
				Pads,
				Messages,
				Icons:   Byte;
			end;
		end;
	end;

	TFilterCRTConfig = record
		Enabled:                Boolean;
		MaskEnabled:            Boolean;
		ScanlinesEnabled:       Boolean;
		ScanlineBloom:          Double;
		DotCrawlSpeed:          Double;
		HorizontalBlur:         Byte;
		ScanlineBrightness:     Double;
		MaskBrightness:         Double;
		BrightAndSharp:         Double;
		ExtraContrast:          Byte;
		EnlargeMaskAtZoomLevel: Byte;
	end;

	TFilterNTSCConfig = record
		Enabled:     Boolean;
		Hue,
		Saturation,
		Contrast,
		Brightness,
		Sharpness,
		Gamma,
		Resolution,
		Artifacts,
		Fringing,
		Bleed:        Double;
		MergeFields:  Boolean;
	end;

	TNTSCPaletteConfig = record
		Saturation,
		HueShift,
		Contrast,
		Brightness,
		Gamma:       Double;
		Filename:    String;
	end;

	TRendererConfig = record
		VSyncMode:            Byte;
		AutoswitchResolution: Boolean;
		ScalingQuality:       Boolean; // nearest/linear
		Backend:              String;
		Overscan: record
			L, R, U, D: Byte;
		end;
	end;

	TPPUConfig = record
		PPUModel:                   TPPUModel;
		DisablePaletteRead:         Boolean;
		DisableOamAddrBug:          Boolean;
		DisablePpu2004Reads:        Boolean;
		EnablePpu2000ScrollGlitch:  Boolean;
		EnablePpu2006ScrollGlitch:  Boolean;
		EnablePpuOamRowCorruption:  Boolean;
		EnableOamDecay:             Boolean;
		RemoveSpriteLimit:          Boolean;
		AdaptiveSpriteLimit:        Boolean;
		ForceBackgroundFirstColumn: Boolean;
		ForceSpritesFirstColumn:    Boolean;
		ExtraScanlinesBeforeNmi:    Word;
		ExtraScanlinesAfterNmi:     Word;
	end;

	TDisplayConfig = record
		Window:      TWindowConfig;
		GUI:         TUIConfig;
		Renderer:    TRendererConfig;
		Palette:     TNTSCPaletteConfig;
		CRT:         TFilterCRTConfig;
		NTSC:        TFilterNTSCConfig;
	end;

	// -----------------------------------------------

	TAPUChannelMixerConfig = record
		Volume:  Double;
		Panning: Double;
	end;

	TAPUConfig = record
		SilenceTriangleHighFreq: Boolean; // Reduce popping on triangle channel
		ReduceDmcPopping:        Boolean; // Reduce popping on DMC channel
		Channels: array[0..MaxChannelCount] of TAPUChannelMixerConfig;
	end;

	TAudioConfig = record
		SampleRate:    Word;
		Latency:       Word;
		MixingVolume:  Double;
		DynamicRate:   Boolean;
		DeviceName:    String;
	end;

	// -----------------------------------------------

	TEmulatorConfig = record
		NESModel: TNESModel;

		PPU:      TPPUConfig;
		APU:      TAPUConfig;

		IntegerFpsMode:         Boolean; // Round down FPS for improved sync?
		PauseInMenu:            Boolean; // Emulation paused when displaying menu?
		AltMMC3Behavior:        Boolean; // Use alternative MMC3 IRQ behavior?
		RunAheadFrames:         Byte;
		RewindBufferSize:       Word;    // Number of snapshots used for rewind
		RewindSnapshotInterval: Byte;    // Snapshot rewind state every Nth frame
		FastForwardInterval:    Byte;    // Update display every Nth frame when ffwd'ing
		FastForwardSpeed:       Byte;    // Fast forward speed multiplier, 1=unlimited

		PowerOnState: record
			RandomizeForMappers: Boolean; // Randomize power-on state for mappers?
			RandomizeAlignment:  Boolean; // Randomize power-on/reset CPU/PPU alignment?
			DefaultRAMState:     Byte;    // Default power-on state for RAM
		end;

		FDS: record
			AutoInsertDisk:    Boolean;
			AutoLoadDisk:      Boolean;
			FastForwardOnLoad: Boolean;
		end;
	end;

	// -----------------------------------------------

	TInputConfig = record
		AllowInvalidInput: Boolean;
		FourScore:         Boolean;
		PadVisual: record
			ControllerCount: Byte;
			MoviesOnly:      Boolean;
		end;
		Zapper: record
			Enabled:         Boolean;
			HidePointer:     Boolean;
			DetectionRadius: Byte;
		end;
	end;

	// -----------------------------------------------
	// Main configuration
	// -----------------------------------------------

	TCaniNESConfig = record
	private
		procedure SetPalette(ID: TGUIColor; Value: Cardinal);
	public
		Application: TApplicationConfig;
		Input:       TInputConfig;
		Display:     TDisplayConfig;
		Audio:       TAudioConfig;
		Emulator:    TEmulatorConfig;

		Filename:   String;
		Path:       String;

		procedure Init(const FilePath: String);
	end;


	procedure WriteResource(const ResourceName, Filename: String);


var
	//StoredConfiguration,
	Configuration: TCaniNESConfig;
	ConfigManager: TConfigurationManager;

implementation

uses
	{$IFDEF WINDOWS} Windows, {$ENDIF} // for RT_RCDATA
	Basement.Util,
	Basement.Renderer.Overlay;

procedure WriteResource(const ResourceName, Filename: String);
var
	Stream: TResourceStream;
	FileStream: TFileStream;
begin
	if FileExists(Filename) then Exit;

	ForceDirectories(ExtractFilePath(Filename));

	Stream := TResourceStream.Create(HInstance, ResourceName, RT_RCDATA);
	FileStream := TFileStream.Create(Filename, fmCreate);
	try
		FileStream.CopyFrom(Stream, Stream.Size);
	finally
		FileStream.Free;
		Stream.Free;
	end;
end;

{ TCaniNESConfig }

procedure TCaniNESConfig.SetPalette(ID: TGUIColor; Value: Cardinal);
var
	S: String;
begin
	WriteStr(S, ID);
	S := LowerCase(S.Replace('COLOR_', '')).Replace('_', '.');
	Palette[ID] := Value;
	ConfigManager.AddCardinal('Palette.GUI', S, @Palette[ID], Value);
end;

procedure TCaniNESConfig.Init(const FilePath: String);
var
	i, L, H: Integer;
	Cfg: TConfigurationManager;
	Sect: String;
begin
	Path := IncludeTrailingPathDelimiter(ExtractFilePath(FilePath));
	Filename := FilePath;
	ConfigManager.Filename := FilePath;

	Cfg := ConfigManager;

	DefaultFormatSettings.DecimalSeparator := '.';

	// -------------------------------------------------------

	SetPalette(COLOR_MESSAGE_FG,		$FFFFFFFF);
	SetPalette(COLOR_MESSAGE_BG,		$88000000);

	SetPalette(COLOR_MENU_BACKGROUND,	$E9111C20);	// background
	SetPalette(COLOR_MENU_SHADOW,		$FF111111);	// shadow
	SetPalette(COLOR_MENU_SELECTION,	$FF883322);	// selection background
	SetPalette(COLOR_MENU_HEADER,		$EE225577);	// header background
	SetPalette(COLOR_MENU_NORMAL,		$FFFFFFFF);	// normal text
	SetPalette(COLOR_MENU_SETTING,		$FFFFE0AA);	// setting text
	SetPalette(COLOR_MENU_HEADING,		$FFDDCCAA);	// header text
	SetPalette(COLOR_MENU_BACKBUTTON,	$FF33EEFF);	// Go back button

	SetPalette(COLOR_FILE_DRIVES,		$FFFF99CC);
	SetPalette(COLOR_FILE_DRIVE,		$FFFFCC99);
	SetPalette(COLOR_FILE_PARENT,		$FFFFCC99);
	SetPalette(COLOR_FILE_DIRECTORY,	$FFCCDDFF);
	SetPalette(COLOR_FILE_EXTRA,		$FFDDFFCC);
	SetPalette(COLOR_FILE_ROM,			$FFFFFFFF);
	SetPalette(COLOR_FILE_CURRENT,		$FFAAEE88);
	SetPalette(COLOR_FILE_FAVOURITED,	$FF003366);
	SetPalette(COLOR_FILE_MOVIE,		$FFAACCFF);
	SetPalette(COLOR_FILE_ARCHIVE,		$FFFFDDAA);
	SetPalette(COLOR_FILE_BAD,			$FFFF9999);

	SetPalette(COLOR_SCROLLBAR_FG,		$FFFFCC99);
	SetPalette(COLOR_SCROLLBAR_BG,		$E9111C20);

	SetPalette(COLOR_SAVESLOT_TEXT,		$FFFFFFFF);
	SetPalette(COLOR_SAVESLOT_MESSAGE,	$FFFFFFFF);
	SetPalette(COLOR_SAVESLOT_BORDER,	$FF000000);
	SetPalette(COLOR_SAVESLOT_SELECTED,	$FFFFFFFF);
	SetPalette(COLOR_SAVESLOT_EXISTS,	$FF449922);
	SetPalette(COLOR_SAVESLOT_EMPTY,	$FF555555);

	SetPalette(COLOR_PAD_INACTIVE,		$FF000000);
	SetPalette(COLOR_PAD_ACTIVE,		$FFFFFFFF);
	SetPalette(COLOR_PAD_BACKGROUND,	$FF888888);
	SetPalette(COLOR_PAD_BORDER,		$FF333333);

	// -------------------------------------------------------

	Sect := 'Application';

	Cfg.AddString(Sect, 'DefaultROMPath', @Application.DefaultROMPath);

	Cfg.AddString(Sect, 'LastROMFile', @Application.LastROMFile);

	Cfg.AddString(Sect, 'RecordingPath', @Application.RecordingPath);

	Cfg.AddString(Sect, 'DateTimeFormat', @Application.DateTimeFormat,
		'ddddd hh":"nn":"ss', False);

	Cfg.AddString(Sect, 'ImagePath.Boxart', @Application.ImagePath_Boxart);
	Cfg.AddString(Sect, 'ImagePath.Snaps',  @Application.ImagePath_Snaps);
	Cfg.AddString(Sect, 'ImagePath.Titles', @Application.ImagePath_Titles);

	Cfg.AddBoolean(Sect, 'HighPriority', @Application.HighPriority, False)
	{$IFDEF WINDOWS}.SetInfo('Application task priority', 0, 1, ['Normal', 'High']){$ENDIF};

	Cfg.AddBoolean(Sect, 'RestoreROMOnStartup', @Application.RestoreROMOnStartup, False)
	.SetInfo('Restore last ROM on startup', 0, 1, ['No', 'Yes']);

	Cfg.AddBoolean(Sect, 'RestoreROMState', @Application.RestoreROMState, False)
	.SetInfo('Restore ROM state on load', 0, 1, ['No', 'Yes']);

	Cfg.AddBoolean(Sect, 'EnableDatabase', @Application.EnableDatabase, True)
	.SetInfo('Built-in game database', 0, 1, ['Disabled', 'Enabled']);

	Cfg.AddByte(Sect, 'AutoPatching', @Application.IPSAutoPatch, 2)
	.SetInfo('Auto-apply IPS patches', 0, 2, ['No', 'Yes', 'Yes, use old CRC']);

	Cfg.AddByte(Sect, 'ShowFrameTime', @Application.ShowFrameTime, 0)
	.SetInfo('Info display', 0, 2, ['Hidden', 'Small', 'Full']);

	// -------------------------------------------------------

	Sect := 'MRU';

	for i := 0 to MRUcount-1 do
		Cfg.AddString(Sect, i.ToString, @Application.MRU[i]);

	// -------------------------------------------------------

	Sect := 'Window';

	Cfg.AddInteger(Sect, 'X', @Display.Window.X, WINDOWPOS_DEFAULT);
	Cfg.AddInteger(Sect, 'Y', @Display.Window.Y, WINDOWPOS_DEFAULT);
	Cfg.AddByte(Sect, 'AspectRatio', @Display.Window.AspectRatio, 0)
	.SetInfo('Aspect ratio', Ord(arNoStretching), Ord(arWidescreen), AspectRatioNames );
	Cfg.AddByte(Sect, 'MaxScale', @Display.Window.MaxScale, 5)
	.SetInfo('Window size', 2, 99, [], nil, '%dx');

	// -------------------------------------------------------

	Sect := 'GUI';
	Cfg.AddByte(Sect, 'Scale', @Display.GUI.Scale, 1);
	//.SetInfo('GUI scale factor', 0, 9, ['Default']);

	Cfg.AddString(Sect, 'Font', @Display.GUI.FontName, 'default', False);

	Cfg.AddBoolean(Sect, 'BoxArtUpscale', @Display.GUI.BoxArtUpscale, False)
	.SetInfo('Box art upscaling', 0, 1, ['Pixel double', 'Full']);

	L := Ord(Low(TLayerAlignment));
	H := Ord(High(TLayerAlignment));

	Cfg.AddByte(Sect, 'OSD.Timeout', @Display.GUI.OSD.Timeout, 5)
	.SetInfo('Messages Timeout', 1, 30, [], nil, '%d seconds');

	Cfg.AddFloat(Sect, 'OSD.Opacity', @Display.GUI.OSD.Opacity, 1.0)
	.SetInfo('Messages Opacity', 0.1, 1.0, 0.05);

	Cfg.AddFloat(Sect, 'OSD.PadsOpacity', @Display.GUI.OSD.PadsOpacity, 0.6)
	.SetInfo('Pads Opacity', 0.1, 1.0, 0.05);

	Cfg.AddByte(Sect, 'Align.Margin', @Display.GUI.OSD.Alignments.Margin, 4)
	.SetInfo('Edge margin', 0, 50, []);

	Cfg.AddByte(Sect, 'Align.Messages', @Display.GUI.OSD.Alignments.Messages, Ord(laBottomLeft))
	.SetInfo('Messages', L, H, LayerAlignmentNames);

	Cfg.AddByte(Sect, 'Align.Icons', @Display.GUI.OSD.Alignments.Icons, Ord(laBottomRight))
	.SetInfo('Icons', L, H, LayerAlignmentNames);

	Cfg.AddByte(Sect, 'Align.Pads', @Display.GUI.OSD.Alignments.Pads, Ord(laTopRight))
	.SetInfo('Pads', L, H, LayerAlignmentNames);

	Cfg.AddByte(Sect, 'Align.InfoBox', @Display.GUI.OSD.Alignments.InfoBox, Ord(laTopLeft))
	.SetInfo('InfoBox', L, H, LayerAlignmentNames);

	// -------------------------------------------------------

	Sect := 'Input';

	Cfg.AddBoolean(Sect, 'AllowInvalidInput', @Input.AllowInvalidInput, False)
	.SetInfo('Allow invalid input');

	Cfg.AddBoolean(Sect, 'FourScore', @Input.FourScore, False)
	.SetInfo('Enable FourScore');

	Cfg.AddByte(Sect, 'PadVisual.Count', @Input.PadVisual.ControllerCount, 0)
	.SetInfo('Show pad inputs', 0, 4, ['Disabled', '1 pad', '2 pads', '3 pads', '4 pads']);

	Cfg.AddBoolean(Sect, 'PadVisual.MoviesOnly', @Input.PadVisual.MoviesOnly, False)
	.SetInfo('When to display', 0, 1, ['Always', 'During movies only']);

	Cfg.AddBoolean(Sect, 'Zapper.Enabled',      @Input.Zapper.Enabled, False)
	.SetInfo('Force enable Zapper');
	Cfg.AddBoolean(Sect, 'Zapper.HidePointer',  @Input.Zapper.HidePointer, False)
	.SetInfo('Hide mouse cursor when using Zapper');
	Cfg.AddByte(Sect, 'Zapper.DetectionRadius', @Input.Zapper.DetectionRadius, 0)
	.SetInfo('Zapper detection radius');

	// -------------------------------------------------------

	Sect := 'Audio';

	Cfg.AddString(Sect, 'DeviceName', @Audio.DeviceName, '');

	{Cfg.AddInteger(Sect, 'BufferSize', @Audio.BufferSize, 2048)
	.SetInfo('Audio buffer size');}

	Cfg.AddWord(Sect, 'SampleRate', @Audio.SampleRate, 44100)
	.SetInfo('Audio sampling rate');

	Cfg.AddWord(Sect, 'Latency', @Audio.Latency, 60)
	.SetInfo('Audio latency');

	Cfg.AddFloat(Sect, 'Volume', @Audio.MixingVolume, 6.6)
	.SetInfo('Master volume', 0.0, 9.9, 0.1);

	Cfg.AddBoolean(Sect, 'DynamicRate', @Audio.DynamicRate, True)
	.SetInfo('Dynamic sampling rate');

	// -------------------------------------------------------

	Sect := 'Renderer';

	Cfg.AddByte(Sect, 'Vsync', @Display.Renderer.VSyncMode, VSYNC_AUTO)
	.SetInfo('Vertical sync', VSYNC_AUTO, VSYNC_OFF, ['Auto', 'Force On', 'Off']);
	Cfg.AddBoolean(Sect, 'ScalingMethod', @Display.Renderer.ScalingQuality, False)
	.SetInfo('Scaling quality', 0, 1, ['Nearest', 'Linear']);
	Cfg.AddBoolean(Sect, 'AutoswitchResolution', @Display.Renderer.AutoswitchResolution, False)
	.SetInfo('Autoswitch fullscreen 50/60 Hz', 0, 1, ['No', 'Yes']);

	Cfg.AddString(Sect, 'Backend', @Display.Renderer.Backend, '');
	//.SetInfo('Renderer backend');

	// -------------------------------------------------------

	Sect := 'Overscan';
	Cfg.AddByte(Sect, 'Top',    @Display.Renderer.Overscan.U, 0).SetInfo('', 0, 99, []);
	Cfg.AddByte(Sect, 'Bottom', @Display.Renderer.Overscan.D, 0).SetInfo('', 0, 99, []);
	Cfg.AddByte(Sect, 'Left',   @Display.Renderer.Overscan.L, 0).SetInfo('', 0, 99, []);
	Cfg.AddByte(Sect, 'Right',  @Display.Renderer.Overscan.R, 0).SetInfo('', 0, 99, []);

	// -------------------------------------------------------

	Sect := 'Filter.CRT';

	Cfg.AddBoolean(Sect, 'Enabled',            @Display.CRT.Enabled,            False)
	.SetInfo('Enable filter');

	Cfg.AddBoolean(Sect, 'ScanlinesEnabled',   @Display.CRT.ScanlinesEnabled,   True)
	.SetInfo('Enable scanlines');
	Cfg.AddFloat(Sect,   'ScanlineBrightness', @Display.CRT.ScanlineBrightness, 1.3)
	.SetInfo('Scanline brightness', 0.0, 2.0, 0.1);
	Cfg.AddFloat(Sect,   'ScanlineBloom',      @Display.CRT.ScanlineBloom,      1.0)
	.SetInfo('Scanline bloom', 0.0, 2.0, 0.1);
	Cfg.AddBoolean(Sect, 'MaskEnabled',        @Display.CRT.MaskEnabled,        True)
	.SetInfo('Enable mask');
	Cfg.AddFloat(Sect,   'MaskBrightness',     @Display.CRT.MaskBrightness,     1.5)
	.SetInfo('Mask brightness', 0.0, 5.0, 0.1);
	Cfg.AddByte(Sect, 'EnlargeMaskAtZoomLevel',@Display.CRT.EnlargeMaskAtZoomLevel, 2)
	.SetInfo('Enlarge mask at zoom level');
	Cfg.AddFloat(Sect,   'DotCrawlSpeed',      @Display.CRT.DotCrawlSpeed,      0.5)
	.SetInfo('Dot crawl speed', 0.0, 5.0, 0.1);
	Cfg.AddFloat(Sect,   'BrightAndSharp',     @Display.CRT.BrightAndSharp,     1.0)
	.SetInfo('Brightness/sharpness modifier', 0.0, 1.0, 0.1);
	Cfg.AddByte(Sect,   'ExtraContrast',       @Display.CRT.ExtraContrast,      60)
	.SetInfo('Extra contrast');
	Cfg.AddByte(Sect,    'HorizontalBlur',     @Display.CRT.HorizontalBlur,     1)
	.SetInfo('Horizontal blurring');

	// -------------------------------------------------------

	Sect := 'Filter.NTSC';

	Cfg.AddBoolean(Sect,'Enabled',     @Display.NTSC.Enabled, False)
	.SetInfo('Enable filter');

{
	Cfg.AddFloat(Sect,  'Hue',         @Display.NTSC.Hue,         0).SetInfo('', -1, 2, 0.02);
	Cfg.AddFloat(Sect,  'Saturation',  @Display.NTSC.Saturation,  0).SetInfo('', -1, 2, 0.02);
	Cfg.AddFloat(Sect,  'Contrast',    @Display.NTSC.Contrast,    0).SetInfo('', -1, 2, 0.02);
	Cfg.AddFloat(Sect,  'Brightness',  @Display.NTSC.Brightness,  0).SetInfo('', -1, 2, 0.02);
	Cfg.AddFloat(Sect,  'Gamma',       @Display.NTSC.Gamma,       0).SetInfo('', -1, 2, 0.02);
}

	Cfg.AddFloat(Sect,  'Sharpness',   @Display.NTSC.Sharpness,   0)
	.SetInfo('', 0.0, 2.0, 0.1);
	Cfg.AddFloat(Sect,  'Resolution',  @Display.NTSC.Resolution,  0)
	.SetInfo('', 0.0, 2.0, 0.1);
	Cfg.AddFloat(Sect,  'Artifacts',   @Display.NTSC.Artifacts,   0)
	.SetInfo('', 0.0, 2.0, 0.1);
	Cfg.AddFloat(Sect,  'Fringing',    @Display.NTSC.Fringing,    0)
	.SetInfo('Color fringing', 0.0, 2.0, 0.1);
	Cfg.AddFloat(Sect,  'Bleed',       @Display.NTSC.Bleed,       0)
	.SetInfo('Color bleed', 0.0, 2.0, 0.1);
	Cfg.AddBoolean(Sect,'MergeFields', @Display.NTSC.MergeFields, False)
	.SetInfo('Merge fields');

	// -------------------------------------------------------

	Sect := 'Palette.NTSC';

	Cfg.AddString(Sect, 'PaletteFile', @Display.Palette.Filename,   '');
	Cfg.AddFloat(Sect,  'Brightness',  @Display.Palette.Brightness, 0.94).SetInfo('', 0.0, 3.0, 0.02);
	Cfg.AddFloat(Sect,  'Contrast',    @Display.Palette.Contrast,   0.92).SetInfo('', 0.0, 3.0, 0.02);
	Cfg.AddFloat(Sect,  'Gamma',       @Display.Palette.Gamma,      1.80).SetInfo('', 0.0, 3.0, 0.02);
	Cfg.AddFloat(Sect,  'Saturation',  @Display.Palette.Saturation, 1.26).SetInfo('', 0.0, 3.0, 0.02);
	Cfg.AddFloat(Sect,  'HueShift',    @Display.Palette.HueShift,  -0.10).SetInfo('', -10.0, +10.0, 0.02);

	// -------------------------------------------------------

	Sect := 'Emulation';

	Cfg.AddByte(Sect, 'ConsoleModel', @Emulator.NESModel, Ord(nesAuto))
	.SetInfo('NES model', Ord(Low(TNESModel)), Ord(High(TNESModel)), NESModelNames);

	Cfg.AddBoolean(Sect, 'AltMMC3Behavior', @Emulator.AltMMC3Behavior, False)
	.SetInfo('Alternative MMC3 behavior');

	Cfg.AddBoolean(Sect, 'IntegerFpsMode', @Emulator.IntegerFpsMode, True)
	.SetInfo('Integer FPS mode');

	Cfg.AddBoolean(Sect, 'PauseInMenu', @Emulator.PauseInMenu, True)
	.SetInfo('Pause when in menu');


	Cfg.AddByte(Sect, 'RunAhead', @Emulator.RunAheadFrames, 0)
	.SetInfo('Runahead frames', 0, 10, [], nil);

	Cfg.AddWord(Sect, 'RewindBuffer', @Emulator.RewindBufferSize, 0)
	.SetInfo('Rewind buffer length', 0, 600, ['Disabled', ''], nil, '%d seconds', 5, 10);

	Cfg.AddByte(Sect, 'RewindSnapshotInterval', @Emulator.RewindSnapshotInterval, 2)
	.SetInfo('Rewind speed', 1, 100, [], nil, '%d00%%');

	Cfg.AddByte(Sect, 'FastForwardSpeed', @Emulator.FastForwardSpeed, 1)
	.SetInfo('Fast forward speed', 1, 10,
		['Unlimited', '200%', '300%', '400%', '500%', '600%', '700%', '800%', '900%', '1000%'], nil);

	Cfg.AddByte(Sect, 'FastForwardInterval', @Emulator.FastForwardInterval, 1)
	.SetInfo('Fast forward frame update interval', 1, 50, [], nil);

	// -------------------------------------------------------

	Sect := 'Emulation.PowerOnState';

	Cfg.AddByte(Sect, 'DefaultRAMState', @Emulator.PowerOnState.DefaultRAMState, 0)
	.SetInfo('Initialize RAM with', 0, 2, ['Zeroes', 'Ones', 'Random'], nil);

	Cfg.AddBoolean(Sect, 'RandomizeForMappers', @Emulator.PowerOnState.RandomizeForMappers, False)
	.SetInfo('Randomize for mappers');

	Cfg.AddBoolean(Sect, 'RandomizeAlignment', @Emulator.PowerOnState.RandomizeAlignment, False)
	.SetInfo('Randomize CPU/PPU alignment');

	// -------------------------------------------------------

	Sect := 'Emulation.FDS';

	Cfg.AddBoolean(Sect, 'AutoInsertDisk', @Emulator.FDS.AutoInsertDisk, True)
	.SetInfo('Automatically insert disk 1 side A');

	Cfg.AddBoolean(Sect, 'AutoLoadDisk', @Emulator.FDS.AutoLoadDisk, False)
	.SetInfo('Automatically switch disks');

	Cfg.AddBoolean(Sect, 'FastForwardOnLoad', @Emulator.FDS.FastForwardOnLoad, False)
	.SetInfo('Automatically fast forward on load');

	// -------------------------------------------------------

	Sect := 'Emulation.PPU';

	with Emulator.PPU do
	begin
		Cfg.AddByte   (Sect, 'Model',                      @PPUModel, 0)
		.SetInfo('PPU model', Ord(Low(TPPUModel)), Ord(High(TPPUModel)), PPUModelNames);

		Cfg.AddBoolean(Sect, 'RemoveSpriteLimit',          @RemoveSpriteLimit, False)
		.SetInfo('Remove sprite limit');
		Cfg.AddBoolean(Sect, 'AdaptiveSpriteLimit',        @AdaptiveSpriteLimit, False)
		.SetInfo('Adaptive sprite limit');

		Cfg.AddBoolean(Sect, 'ForceBackgroundFirstColumn', @ForceBackgroundFirstColumn, False)
		.SetInfo('Force bg tiles in first column');
		Cfg.AddBoolean(Sect, 'ForceSpritesFirstColumn',    @ForceSpritesFirstColumn, False)
		.SetInfo('Force sprites in first column');

		Cfg.AddBoolean(Sect, 'DisablePaletteRead',         @DisablePaletteRead, False)
		.SetInfo('Disable palette reads');
		Cfg.AddBoolean(Sect, 'DisableOamAddrBug',          @DisableOamAddrBug, False)
		.SetInfo('Disable OAM addressing bug');
		Cfg.AddBoolean(Sect, 'DisablePpu2004Reads',        @DisablePpu2004Reads, False)
		.SetInfo('Disable PPU 2004 reads');
		Cfg.AddBoolean(Sect, 'EnablePpu2000ScrollGlitch',  @EnablePpu2000ScrollGlitch, False)
		.SetInfo('Emulate PPU2000 scroll glitch');
		Cfg.AddBoolean(Sect, 'EnablePpu2006ScrollGlitch',  @EnablePpu2006ScrollGlitch, False)
		.SetInfo('Emulate PPU2006 scroll glitch');
		Cfg.AddBoolean(Sect, 'EnablePpuOamRowCorruption',  @EnablePpuOamRowCorruption, False)
		.SetInfo('Emulate PPU OAM row corruption');
		Cfg.AddBoolean(Sect, 'EnableOamDecay',             @EnableOamDecay, False)
		.SetInfo('Emulate OAM decay');

		Cfg.AddWord(Sect, 'ExtraScanlinesBeforeNmi',    @ExtraScanlinesBeforeNmi, 0)
		.SetInfo('Extra scanlines before NMI', 0, 2000, [], nil, '', 10, 20);
		Cfg.AddWord(Sect, 'ExtraScanlinesAfterNmi',     @ExtraScanlinesAfterNmi,  0)
		.SetInfo('Extra scanlines after NMI', 0, 2000, [], nil, '', 10, 20);
	end;

	// -------------------------------------------------------

	Sect := 'Emulation.APU';

	Cfg.AddBoolean(Sect, 'ReduceDmcPopping', @Emulator.APU.ReduceDmcPopping, True)
	.SetInfo('Reduce popping on DMC channel');

	Cfg.AddBoolean(Sect, 'SilenceTriangleHighFreq', @Emulator.APU.SilenceTriangleHighFreq, True)
	.SetInfo('Reduce popping on triangle channel');

	// -------------------------------------------------------

	Sect := 'Emulation.APU.Channels';

	for i := 0 to MaxChannelCount do
	begin
		Cfg.AddFloat(Sect, IntToStr(i) + '.Volume',
			@Emulator.APU.Channels[i].Volume, 1.0)
			.SetInfo(APUChannelNames[i] + ' volume', 0.0, 2.0, 0.05);
	end;

	for i := 0 to MaxChannelCount do
	begin
		Cfg.AddFloat(Sect, IntToStr(i) + '.Panning',
			@Emulator.APU.Channels[i].Panning, 0.0)
			.SetInfo(APUChannelNames[i] + ' panning', -1.0, +1.0, 0.01);
	end;

	// -------------------------------------------------------

	// disable audio interference by default
	Emulator.APU.Channels[channel_InvA13].Volume := 0;
	Emulator.APU.Channels[channel_InvOE1].Volume := 0;

	if Cfg.Load then
		Log('Configuration loaded from ' + Filename + '.')
	else
		Log('Failed to load configuration from ' + Filename + '!')
end;


end.

