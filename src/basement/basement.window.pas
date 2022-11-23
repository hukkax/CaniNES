unit Basement.Window;

{$I basement.inc}

interface

uses
	Classes, SysUtils, Types,
	SDL2, Graphics32, Basement.Util;

type
	TMouseButton = ( mbNone, mbLeft, mbRight, mbMiddle, mbX1, mbX2 );

	TVideoInfo = record
		Renderer:		PSDL_Renderer;
		Window:			PSDL_Window;
		Texture:		PSDL_Texture;
		IsFullScreen: 	Boolean;
		IsMaximized:	Boolean;
		IsCustomRes:	Boolean;
		HaveVSync:		Boolean;
		DesiredFramerate,
		SyncRate:		Double;
		NextFrameTime:	UInt64;

		NewSDL: 		Boolean;
		RendererName,
		LibraryVersion:	AnsiString;
	end;

	TBasementInitSettings = packed record
		X, Y:           Integer;
		//Width, Height,
		FrameBufferWidth,
		FrameBufferHeight: Word;
		Scale:          Byte;
		MinScale:       Byte;
		VSyncMode:      Byte;
		Framerate:      Double;
		AspectRatioWidthMultiplier:	Single;
		WinHandle:      Pointer;
		VSyncLimits:    TMinMax;
		FullScreen:     Boolean;
		Maximized:      Boolean;
		ScalingQuality: Boolean; // nearest/linear
		HighPriority:   Boolean;
		CloseOnAltF4:   Boolean;
		UseGamepads:    Boolean;
		AutoswitchResolution: Boolean;
		Overscan:       Types.TRect;
		Backend:        String;
		Caption:        AnsiString;
	end;
	PBasementInitSettings = ^TBasementInitSettings;

	TMouseInfo = record
		Enabled:     Boolean;
		Visible:     Boolean;
		ButtonState: array [TMouseButton] of Boolean;
		Timer:       Cardinal;
		Pos,
		UnscaledPos,
		OldPos,
		Scaling:     Types.TPoint;
		InWindow:    Boolean;
		CurrentCursor: record
			Kind:   TSDL_SystemCursor;
			Cursor: PSDL_Cursor;
		end;
	end;

	TCursorChangeEvent = procedure (Kind: TSDL_SystemCursor) of Object;

	TWindow = class
	const
		TimerInterval = 10;
	private
		LastTitle: AnsiString;
		Logo: TBitmap32;
	protected
		{$IFDEF LIMIT_KEYBOARD_EVENTS}
		PrevKeyTimeStamp: Uint32;
		{$ENDIF}
		PerfFreq: UInt64;
		SkippedFrames: Byte;

		GamePads:	TFPList;

		WindowSize: Types.TPoint;

		CustomRes50,
		CustomRes60: TSDL_DisplayMode;

		function 	GetMaxScaling(MaxScale: Byte = 0): Byte;
		function 	SetupVideo: Boolean;
		function	ReinitWindow: Boolean; virtual;
	public
		Settings:		TBasementInitSettings;
		Video:			TVideoInfo;
		Mouse:			TMouseInfo;
		Scale:			Byte;
		Visible: 		Boolean;
		FrameBuffer:	TBitmap32;
		OverscanRect:	Types.TRect;

		OnSetCursor: TCursorChangeEvent;

		constructor Create;
		destructor 	Destroy; override;
		procedure	Initialize;
		procedure	Uninitialize;
		procedure	Close;

		procedure	SetFullScreen(B: Boolean);
		function 	SetFrameRate(Rate: Double): Boolean;
		function 	ProcessFrame(Tick: UInt32 = 0): UInt32; virtual;
		procedure	FlipFrame; virtual;
		procedure	SyncFramerate;
		procedure	SetTitle(const Title: AnsiString);
		procedure	ShowMouse; virtual;
		procedure	SetSystemCursor(Kind: TSDL_SystemCursor);
		procedure 	PixelScalingChanged; virtual;

		procedure	OnKey(Key: Integer; Shift: TShiftState; Pressed, Repeated: Boolean); virtual;

		procedure 	HandleInput;
		procedure	OnMouseMove(Pos, UnscaledPos: Types.TPoint); virtual;
		procedure	OnMouseEnterLeave(Entered: Boolean); virtual;
		procedure	OnMouseWheel(WheelDelta: Types.TPoint); virtual;
		procedure	OnMouseButton(Button: TMouseButton; Pressed: Boolean); virtual;
		procedure	OnJoyButton(Pressed: Boolean; PadNum: Integer; Button: Byte); virtual;
		procedure	OnWindowResized(NewSize: Types.TPoint); virtual;
		procedure	OnUserEvent(EventCode: Integer); virtual;
		procedure	OnFileDropped(const Filename: String); virtual;
	end;


	function	GetModifierKey(keymod: TSDL_KeyMod; var Shift: TShiftState;
				keymodconst: Integer; shiftconst: TShiftStateEnum): Boolean; inline;
	function 	GetShiftState: TShiftState;

const
	ScalingQualityString: array[Boolean] of AnsiString = ( 'nearest', 'linear' );

var
	BasementMainWindow: TWindow;

	QuitFlag:		Boolean;
	Initialized:	Boolean;
	BasementOptions: TBasementInitSettings;


implementation

uses
	{$IFDEF WINDOWS}Windows,{$ENDIF}
	Basement.Renderer,
    BuildInfo, Math;

// ================================================================================================
// Utility
// ================================================================================================

procedure ClearMessageQueue;
var
	InputEvent: TSDL_Event;
begin
	SDL_Delay(50);
	while SDL_PollEvent(@InputEvent) <> 0 do;
end;

function GetModifierKey(keymod: TSDL_KeyMod; var Shift: TShiftState;
	keymodconst: Integer; shiftconst: TShiftStateEnum): Boolean;
begin
	Result := (keymod and keymodconst) <> 0;
	if Result then
		Include(Shift, shiftconst);
end;

function GetShiftState: TShiftState;
var
	M: TSDL_Keymod;
begin
	Result := [];
	M := SDL_GetModState;
	GetModifierKey(M, Result, KMOD_SHIFT,	ssShift);	// Shift
	GetModifierKey(M, Result, KMOD_CTRL,	ssCtrl);	// Ctrl
	GetModifierKey(M, Result, KMOD_ALT,		ssAlt);		// Alt
	GetModifierKey(M, Result, KMOD_MODE,	ssAltGr);	// AltGr
	GetModifierKey(M, Result, KMOD_GUI,		ssMeta);	// Windows
	//GetModifierKey(M, Result, KMOD_NUM,	ssNum);		// Num Lock
	GetModifierKey(M, Result, KMOD_CAPS,	ssCaps);	// Caps Lock
end;

procedure TWindow.PixelScalingChanged;
var
	R: TRenderer;
begin
	//ClearMessageQueue;
	//SetFullScreen(Video.IsFullScreen);
	for R in Renderers do
		R.ScaleChanged(Scale);
end;

function TWindow.GetMaxScaling(MaxScale: Byte = 0): Byte;
var
	w, h, di: Integer;
	R: TSDL_Rect;
begin
	if Video.Window <> nil then
		di := SDL_GetWindowDisplayIndex(Video.Window)
	else
		di := 0;

	if MaxScale = 0 then MaxScale := High(Byte); //Max(Settings.MaxScale, 1);

	{$IFNDEF DISABLE_SDL2_2_0_5}
	if Video.NewSDL then
		SDL_GetDisplayUsableBounds(di, @R)
	else
	{$ENDIF}
		SDL_GetDisplayBounds(di, @R);

	repeat
		w := OverscanRect.Width  * MaxScale;
		h := OverscanRect.Height * MaxScale;

		if (w <= R.w) and (h <= R.h) then Break;
		Dec(MaxScale);
	until MaxScale <= 1;

	Result := Max(MaxScale, 1);
end;

// ================================================================================================
// Event handlers
// ================================================================================================

{$PUSH}
	// don't complain about unused parameters in dummy event handlers
	{$WARN 5024 OFF : Parameter "$1" not used}

	procedure TWindow.OnUserEvent(EventCode: Integer);
	begin
	//
	end;

	procedure TWindow.OnFileDropped(const Filename: String);
	begin
	//
	end;

	procedure TWindow.OnWindowResized(NewSize: Types.TPoint);
	begin
	//
	end;

	// ============================================================================================
	// Mouse Input

	procedure TWindow.OnMouseEnterLeave(Entered: Boolean);
	begin
		if Video.IsFullScreen then
			Mouse.InWindow := True
		else
			Mouse.InWindow := Entered;
	end;

	procedure TWindow.OnMouseMove(Pos, UnscaledPos: Types.TPoint);
	begin
		//SetTitle(format('Pos=(%d,%d) (%d,%d)', [Pos.X,Pos.Y, UnscaledPos.X,UnscaledPos.Y]));
	end;

	procedure TWindow.OnMouseWheel(WheelDelta: Types.TPoint);
	begin
		//SetTitle(format('Wheel delta=(%d,%d)', [WheelDelta.X,WheelDelta.Y]));
	end;

	procedure TWindow.OnMouseButton(Button: TMouseButton; Pressed: Boolean);
	begin
		Mouse.ButtonState[Button] := Pressed;
		//SetTitle(format('Button %d %d', [Ord(Button), ButtonDown.ToInteger]));
	end;

	procedure TWindow.OnJoyButton(Pressed: Boolean; PadNum: Integer; Button: Byte);
	begin
	//
	end;

	// ============================================================================================
	// Keyboard Input

	procedure TWindow.OnKey(Key: Integer; Shift: TShiftState; Pressed, Repeated: Boolean);
	begin
		//SetTitle(Format('Key %d: %d, %d', [Key, Pressed.ToInteger, Repeated.ToInteger]));


		if (Pressed) and (not Repeated) then
		case Key of

			{$IFNDEF USE_LCL}
			// Exit program
			SDLK_ESCAPE:
				if Settings.WinHandle = nil then
					Close;
			{$ENDIF}

			// Toggle fullscreen with Alt-Enter
			SDLK_RETURN:
				if ssAlt in Shift then
					SetFullScreen(not Video.IsFullScreen);

		end;
	end;

// re-enable warnings
{$POP}

// ================================================================================================
// Input Handler
// ================================================================================================

procedure TWindow.HandleInput;
var
	InputEvent: TSDL_Event;
	Key: TSDL_KeyCode;
	km: TSDL_KeyMod;
	Btn: TMouseButton;
	Shift: TShiftState;

begin
	if Locked then Exit;

	while SDL_PollEvent(@InputEvent) <> 0 do
	case {%H-}InputEvent.type_ of

		SDL_KEYDOWN, SDL_KEYUP:
		begin
			{$IFDEF LIMIT_KEYBOARD_EVENTS}
			if (InputEvent.key.timestamp - PrevKeyTimeStamp) > 4 then
			{$ENDIF}
			begin
				Key := InputEvent.key.keysym.sym;
				case Key of
					SDLK_UNKNOWN: ;
					{SDLK_LSHIFT, SDLK_RSHIFT, SDLK_LCTRL, SDLK_RCTRL, SDLK_LALT, SDLK_RALT, SDLK_LGUI, SDLK_RGUI}
				else
					Shift := [];
					if InputEvent.key.keysym._mod <> KMOD_NONE then
					begin
						km := InputEvent.key.keysym._mod;
						GetModifierKey(km, Shift, KMOD_SHIFT, ssShift); // Shift
						GetModifierKey(km, Shift, KMOD_CTRL,  ssCtrl);  // Ctrl
						GetModifierKey(km, Shift, KMOD_ALT,   ssAlt);   // Alt
						GetModifierKey(km, Shift, KMOD_GUI,   ssMeta);  // Windows
						GetModifierKey(km, Shift, KMOD_CAPS,  ssCaps);  // Caps Lock
						GetModifierKey(km, Shift, KMOD_MODE,  ssAltGr); // AltGr
					end;
					OnKey(Integer(Key), Shift,
						(InputEvent.type_ = SDL_KEYDOWN), // Pressed?
						(InputEvent.key._repeat <> 0));   // Repeated?
				end;
			end;
			{$IFDEF LIMIT_KEYBOARD_EVENTS}
			PrevKeyTimeStamp := InputEvent.key.timestamp;
			{$ENDIF}
		end;

		{SDL_TEXTINPUT:
			if InputEvent.text.text[0] <> #0 then
				OnTextInput(InputEvent.text.text);}

		SDL_MOUSEBUTTONDOWN,
		SDL_MOUSEBUTTONUP:
			if Mouse.Enabled then
			begin
				case InputEvent.button.button of
					SDL_BUTTON_LEFT:   Btn := mbLeft;
					SDL_BUTTON_MIDDLE: Btn := mbMiddle;
					SDL_BUTTON_RIGHT:  Btn := mbRight;
					SDL_BUTTON_X1:     Btn := mbX1;
					SDL_BUTTON_X2:     Btn := mbX2;
				else
					Btn := mbNone;
				end;
				OnMouseButton(Btn, (InputEvent.type_ = SDL_MOUSEBUTTONDOWN));
			end;

		SDL_MOUSEMOTION:
			if Mouse.Enabled then
			begin
				Mouse.UnscaledPos := Types.Point(InputEvent.motion.x, InputEvent.motion.y);
				Mouse.Pos := Types.Point(
					Trunc(Mouse.UnscaledPos.X / Mouse.Scaling.X),
					Mouse.UnscaledPos.Y div Mouse.Scaling.Y);
				if not PointsEqual(Mouse.UnscaledPos, Mouse.OldPos) then
				begin
					Mouse.OldPos := Mouse.UnscaledPos;
					OnMouseMove(Mouse.Pos, Mouse.UnscaledPos);
				end;
			end;

		SDL_MOUSEWHEEL:
			if Mouse.Enabled then
				OnMouseWheel(Types.Point(InputEvent.wheel.x, InputEvent.wheel.y));

		SDL_CONTROLLERBUTTONDOWN,
		SDL_CONTROLLERBUTTONUP:
			OnJoyButton(InputEvent.type_ = SDL_CONTROLLERBUTTONDOWN,
				InputEvent.cbutton.which, InputEvent.cbutton.button);

		//SDL_CONTROLLERAXISMOTION:
			//OnJoyMotion(InputEvent.caxis.which, InputEvent.caxis.value);


		SDL_WINDOWEVENT:
			case InputEvent.window.event of
		        SDL_WINDOWEVENT_ENTER:      OnMouseEnterLeave(True);
		        SDL_WINDOWEVENT_LEAVE:      OnMouseEnterLeave(False);

				SDL_WINDOWEVENT_SHOWN,
				SDL_WINDOWEVENT_EXPOSED:    Visible := True;

				SDL_WINDOWEVENT_MINIMIZED,
				SDL_WINDOWEVENT_HIDDEN:     Visible := False;

				SDL_WINDOWEVENT_MAXIMIZED:
				if Settings.WinHandle = nil then
				begin
					Video.IsMaximized := True;
					Visible := True;
				end;

				SDL_WINDOWEVENT_RESTORED:
				if Settings.WinHandle = nil then
				begin
					Video.IsMaximized := False;
					Visible := True;
				end;

				SDL_WINDOWEVENT_RESIZED:
					OnWindowResized(Types.Point(
						InputEvent.window.data1, InputEvent.window.data2));
			end;


		SDL_DROPFILE:
			OnFileDropped(InputEvent.drop._file);

		SDL_USEREVENT:
			OnUserEvent(InputEvent.user.code);

		SDL_QUITEV:
			Close;

	end;
end;

// ================================================================================================
// Video
// ================================================================================================

function TWindow.SetupVideo: Boolean;

	function SetHint(const Hint: AnsiString; Val: Boolean): Boolean;
	var
		bs: AnsiString;
	begin
		if Val then bs := '1' else bs := '0';
		Result := SDL_SetHint(PAnsiChar(Hint), PAnsiChar(bs));
		{if not Result then
			LogIfDebug('Failed to set SDL hint "' + Hint + '"!');}
	end;

	function GetWindowPosValue(Flag: Integer): Integer;
	begin
		case Flag of
			WINDOWPOS_DEFAULT:  Result := SDL_WINDOWPOS_UNDEFINED;
			WINDOWPOS_CENTERED: Result := SDL_WINDOWPOS_CENTERED;
		else
			Result := Flag;
		end;
	end;

	procedure FatalError(S: String);
	begin
		S := S + ': ' + SDL_GetError;
		LogFatal(S);
		SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR, 'Init Failure', PChar(S), Video.Window);
	end;

var
	dm: TSDL_DisplayMode;
	windowFlags: TSDL_WindowFlags;
	rendererFlags: TSDL_RendererFlags;
	screenW, screenH, sx, sy, fbx, fby: Word;
	Icon: PSDL_Surface;
	Fn: String;
	rinfo: TSDL_RendererInfo;
	sdlVersion: TSDL_Version;
	Initflags, i: UInt32;
	pad: TSDL_GameController;
	P: Types.TPoint;
begin
    Result := False;
	Locked := True;
	Visible := True;

	if Initialized then
		Renderers.Clear;

	screenW := Settings.FrameBufferWidth;  //Max(Settings.Width,  Settings.FrameBufferWidth);
	screenH := Settings.FrameBufferHeight; //Max(Settings.Height, Settings.FrameBufferHeight);
	fbx := screenW;
	fby := screenH;

	OverscanRect := Types.Rect(Settings.Overscan.Left, Settings.Overscan.Top,
		screenW - Settings.Overscan.Right, screenH - Settings.Overscan.Bottom);

	screenW := OverscanRect.Width;
	screenH := OverscanRect.Height;

	if not Assigned(FrameBuffer) then
		FrameBuffer := TBitmap32.Create;
	FrameBuffer.SetSize(fbx, fby);

	if not Initialized then
	begin
		SDL_GetVersion(@sdlVersion);
		Video.NewSDL := sdlVersion.patch >= 5; // we want SDL 2.0.5 or newer
		Video.LibraryVersion := Format('%d.%d.%d',
			[sdlVersion.major, sdlVersion.minor, sdlVersion.patch]);
		//LogIfDebug('Loaded SDL ' + Video.LibraryVersion);
	end;

	Video.DesiredFramerate := Settings.Framerate;

	windowFlags := UInt32(SDL_WINDOW_SHOWN) or UInt32(SDL_WINDOW_RESIZABLE);
	rendererFlags := UInt32(SDL_RENDERER_ACCELERATED or SDL_RENDERER_TARGETTEXTURE);

	if Settings.HighPriority then
	begin
		{$IFDEF WINDOWS}
		SetPriorityClass(GetCurrentProcess, HIGH_PRIORITY_CLASS);
		{$ENDIF}
		SDL_SetThreadPriority(SDL_THREAD_PRIORITY_HIGH);
	end;

	{$IFDEF UNIX}
//	SetHint('SDL_VIDEO_X11_XRANDR',   False);
//	SetHint('SDL_VIDEO_X11_XVIDMODE', True);
	{$ENDIF}

	SetHint(SDL_HINT_TIMER_RESOLUTION, True);
	SetHint(SDL_HINT_VIDEO_HIGHDPI_DISABLED, True);
	SetHint(SDL_HINT_WINDOWS_NO_CLOSE_ON_ALT_F4, not Settings.CloseOnAltF4);
	SetHint(SDL_HINT_RENDER_SCALE_QUALITY, Settings.ScalingQuality);
	if Video.NewSDL then
		SetHint(SDL_HINT_WINDOWS_DISABLE_THREAD_NAMING, True);

	if not Initialized then
	begin
		Log('[SetupVideo] First init');

		Initflags := SDL_INIT_VIDEO or SDL_INIT_TIMER;
		if Settings.UseGamepads then
			Initflags := Initflags or SDL_INIT_GAMECONTROLLER;

		if SDL_Init(Initflags) <> 0 then
		begin
			FatalError('Error initializing SDL');
			Exit;
		end;

		SDL_StopTextInput;

		SetHint(SDL_HINT_RENDER_BATCHING, True);
		if Settings.Backend <> '' then
		begin
			Log('Setting rendering backend to: ' + Settings.Backend);
			SDL_SetHint(SDL_HINT_RENDER_DRIVER, PChar(Settings.Backend));
		end;
	end
	else
	begin
		Log('[SetupVideo] Reinitializing');

		SDL_DestroyRenderer(Video.Renderer);
		SDL_DestroyTexture(Video.Texture);
		SDL_DestroyWindow(Video.Window);
		Video.Renderer := nil;
		Video.Texture := nil;
		Video.Window := nil;
	end;

	Video.SyncRate := 0;

	if Settings.VSyncMode <> VSYNC_OFF then
	begin
		if SDL_GetDesktopDisplayMode(0, @dm) = 0 then
		begin
			Video.SyncRate := dm.refresh_rate;
			//Log('[Video] Refresh rate (Monitor/Wanted): %d/%d', [Video.SyncRate, Settings.Framerate]);
		end
		else
			Log('GetDesktopDisplayMode failed: ' + SDL_GetError);

		// enable vsync, optionally only if the screen refresh rate is within accepted limits
		if (Settings.VSyncMode = VSYNC_FORCE) or (
			(Video.SyncRate >= Settings.VSyncLimits.Min) and (Video.SyncRate <= Settings.VSyncLimits.Max)
			) then
		begin
			rendererFlags := rendererFlags or UInt32(SDL_RENDERER_PRESENTVSYNC);
			//if (not Initialized) or (not Video.HaveVSync) then
			//	Log('[Video] VSync');
			Video.HaveVSync := True;
		end
		else
		begin
			//if (not Initialized) or (Video.HaveVSync) then
			//	Log('[Video] No VSync');
		    Video.HaveVSync := False;
		end;
	end;

	// make sure not to exceed display bounds
	i := GetMaxScaling(Scale);
	if (Scale = 0) or (Scale > i) then
		Scale := i;
	sx := Trunc(screenW * Scale * Settings.AspectRatioWidthMultiplier);
	sy := screenH * Scale;

	if Settings.WinHandle <> nil then
	begin
		Log('Create window: embedded');
		Video.Window := SDL_CreateWindowFrom(Settings.WinHandle);
	end
	else
	begin
		Log('Create window: standalone');
		Video.Window := SDL_CreateWindow(PAnsiChar(Settings.Caption),
			GetWindowPosValue(Settings.X), GetWindowPosValue(Settings.Y),
			sx, sy, windowFlags);
	end;

	WindowSize := Types.Point(sx, sy);
	if Video.Window = nil then
	begin
		FatalError('Error setting up window');
		Exit;
	end;

	Video.Renderer := SDL_CreateRenderer(Video.Window, -1, rendererFlags);
	if (Video.Renderer = nil) and (Video.HaveVSync) then
	begin
		// try again without vsync flag
        Video.HaveVSync := False;
		rendererFlags := rendererFlags and not UInt32(SDL_RENDERER_PRESENTVSYNC);
		Video.Renderer := SDL_CreateRenderer(Video.Window, -1, rendererFlags);
	end;

	if Video.Renderer = nil then
	begin
		FatalError('Error creating renderer');
		Exit;
	end;

    SDL_SetRenderDrawBlendMode(Video.Renderer, SDL_BLENDMODE_NONE);
	SDL_SetRenderDrawColor(Video.Renderer, 0, 0, 0, SDL_ALPHA_OPAQUE);

	SDL_GetRendererInfo(Video.Renderer, @rinfo);
	Video.RendererName := rinfo.name;
	//if not Initialized then
	//	Log('[Video] Renderer: ' + rinfo.name);

	if SDL_RenderSetLogicalSize(Video.Renderer, Trunc(screenW * Settings.AspectRatioWidthMultiplier), screenH) <> 0 then
	begin
		FatalError('Error setting renderer size');
		Exit;
	end;
	{$IFNDEF DISABLE_SDL2_2_0_5}
    if Video.NewSDL then
	begin
		if not Settings.ScalingQuality then
			SDL_RenderSetIntegerScale(Video.Renderer, SDL_TRUE)
		else
			SDL_RenderSetIntegerScale(Video.Renderer, SDL_FALSE);
	end;
	{$ENDIF}

	Settings.MinScale := Max(1, Settings.MinScale);
	SDL_SetWindowMinimumSize(Video.Window,
		Trunc(screenW * Settings.MinScale * Settings.AspectRatioWidthMultiplier),
		screenH * Settings.MinScale);

	Video.Texture := SDL_CreateTexture(Video.Renderer,
		UInt32(SDL_PIXELFORMAT_ARGB8888), SInt32(SDL_TEXTUREACCESS_STREAMING), fbx, fby);
	if Video.Texture = nil then
	begin
		FatalError('Error initializing streaming texture');
		Exit;
	end
	else
		SDL_SetTextureBlendMode(Video.Texture, SDL_BLENDMODE_NONE);

	TRenderer.Create(Self, nil, 'Main');

	if not Initialized then
	begin
		Fn := GetDataFile('icon.bmp');
		if Fn <> '' then
		begin
			Icon := SDL_LoadBMP(PAnsiChar(Fn));
			SDL_SetWindowIcon(Video.Window, Icon);
			SDL_FreeSurface(Icon);
		end;

		PixelScalingChanged;
	end;

	if Settings.UseGamepads then
	if SDL_NumJoysticks > 0 then
	begin
		for i := 0 to SDL_NumJoysticks-1 do
			if SDL_IsGameController(i) = SDL_TRUE then
			begin
				pad := SDL_GameControllerOpen(i);
				if SDL_GameControllerGetAttached(pad) = SDL_TRUE then
					GamePads.add(pad);
			end;
		SDL_GameControllerEventState(SDL_ENABLE);
	end;

	if Initialized then
		SDL_SetWindowPosition(Video.Window, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED);

	SetTitle(LastTitle);

	PerfFreq := SDL_GetPerformanceFrequency;
	Video.NextFrameTime := Trunc(SDL_GetPerformanceCounter + ((PerfFreq / Video.DesiredFramerate) + 0.5));

	Result := True;
	Locked := False;

	//if not Initialized then
	begin
		FrameBuffer.Clear($FF000000);

		if (Assigned(Logo)) and (Logo.Width > 1) then
		begin
			P := FrameBuffer.BoundsRect.CenterPoint;
			FrameBuffer.Draw(P.X - (Logo.Width div 2), P.Y - (Logo.Height div 2), Logo);
		end;

		FlipFrame;
	end;

	if not Initialized then
	if SDL_GetDesktopDisplayMode(0, @dm) = 0 then
	begin
		sx := dm.w;
		sy := dm.h;

		CustomRes50.refresh_rate := 0;
		for i := 0 to SDL_GetNumDisplayModes(0)-1 do
		begin
			SDL_GetDisplayMode(0, i, @dm);
			if (dm.refresh_rate in [48..52]) and (dm.w <= sx) and (dm.h <= sy) then
			begin
				CustomRes50 := dm;
				LogDebug(Format('50Hz custom mode: %d*%d @ %dHz', [dm.w, dm.h, dm.refresh_rate]));
				Break;
			end;
		end;

		CustomRes60.refresh_rate := 0;
		for i := 0 to SDL_GetNumDisplayModes(0)-1 do
		begin
			SDL_GetDisplayMode(0, i, @dm);
			if (dm.refresh_rate in [58..62]) and (dm.w <= sx) and (dm.h <= sy) then
			begin
				CustomRes60 := dm;
				LogDebug(Format('60Hz custom mode: %d*%d @ %dHz', [dm.w, dm.h, dm.refresh_rate]));
				Break;
			end;
		end;
	end;

	if Settings.FullScreen then
		SetFullScreen(True);

	if Initialized then
		SetSystemCursor(Mouse.CurrentCursor.Kind);
end;

procedure TWindow.ShowMouse;
begin
	if (Mouse.Enabled) and (Mouse.Visible) then
		SDL_ShowCursor(SDL_ENABLE)
	else
		SDL_ShowCursor(SDL_DISABLE);
end;

procedure TWindow.SetSystemCursor(Kind: TSDL_SystemCursor);
var
	Cursor: PSDL_Cursor;
begin
	{$IFNDEF USE_LCL}
	Cursor := SDL_CreateSystemCursor(Kind);
	if Cursor <> nil then
	begin
		if Mouse.CurrentCursor.Cursor <> nil then
			SDL_FreeCursor(Mouse.CurrentCursor.Cursor);
		Mouse.CurrentCursor.Kind   := Kind;
		Mouse.CurrentCursor.Cursor := Cursor;
		SDL_SetCursor(Cursor);
	end;
	{$ENDIF}
	if Assigned(OnSetCursor) then OnSetCursor(Kind);
end;

procedure TWindow.SetFullScreen(B: Boolean);
var
	w, h: Integer;
	X, Y: Single;
	DispMode: TSDL_DisplayMode;
	{$IFDEF DISABLE_FULLSCREEN}
	R: TSDL_Rect;
    {$ENDIF}
const
	SDL_WINDOW_WINDOWED = 0;
begin
	Visible := True;

	// SDL2 seems to have huge problems returning to windowed from a different
	// fullscreen resolution so let's just reinit everything instead
	//
	if (not B) and (Video.IsCustomRes) and (Video.IsFullScreen) then
	begin
	    Video.IsFullScreen := False;
		Video.IsCustomRes := False;
		ReinitWindow;
		Exit;
	end;

	Locked := True;
    Video.IsFullScreen := B;
	Video.IsCustomRes := False;

	if B then
	begin
		if Settings.WinHandle = nil then
		begin
			{$IFNDEF DISABLE_FULLSCREEN}
			DispMode.refresh_rate := 0; // don't switch

			if Settings.AutoswitchResolution then
			begin
				case Trunc(Settings.Framerate) of
					48..52: DispMode := CustomRes50;
					58..62: DispMode := CustomRes60;
				end;
			end;

			if DispMode.refresh_rate < 10 then
				SDL_SetWindowFullscreen(Video.Window, SDL_WINDOW_FULLSCREEN_DESKTOP)
			else
			begin
				Log('Resolution change: %d*%d @ %dHz', [DispMode.w, DispMode.h, DispMode.refresh_rate]);
				SDL_SetWindowDisplayMode(Video.Window, @DispMode);
				SDL_SetWindowFullscreen(Video.Window, SDL_WINDOW_FULLSCREEN);
				Video.IsCustomRes := True;
				Mouse.InWindow := True;
			end;
	    	{$ELSE}
			// Borderless windowed fullscreen
			SDL_GetDisplayUsableBounds(SDL_GetWindowDisplayIndex(Video.Window), @R);
	        SDL_SetWindowBordered(Video.Window, SDL_FALSE);
			SDL_SetWindowSize(Video.Window, R.w, R.h);
			SDL_SetWindowPosition(Video.Window, R.x, R.y);
	        {$ENDIF}
		end;
	end
	else
	begin
		//SDL_GetDesktopDisplayMode(0, @DispMode);
		//SDL_SetWindowDisplayMode(Video.Window, @DispMode);

		h := GetMaxScaling(Scale);
		w := Trunc(OverscanRect.Width * h * Settings.AspectRatioWidthMultiplier);
		h := OverscanRect.Height * h;

		if Settings.WinHandle = nil then
		begin
			SDL_SetWindowFullscreen(Video.Window, SDL_WINDOW_WINDOWED);
			SDL_SetWindowGrab(Video.Window, SDL_FALSE);
			SDL_SetWindowBordered(Video.Window, SDL_TRUE);
			SDL_SetWindowSize(Video.Window, w, h);
			SDL_SetWindowPosition(Video.Window, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED);
		end;
	end;

	SDL_RenderGetScale(Video.Renderer, @X, @Y);
	w := Max(Trunc(X), 1); h := Max(Trunc(Y), 1);
	w := Min(w, h);

	if w <> Scale then
	begin
		Scale := w;
		PixelScalingChanged;
	end;

	Mouse.Scaling := Types.Point(Trunc(w * Settings.AspectRatioWidthMultiplier), w);
	ShowMouse;

	{$IFNDEF DISABLE_SDL2_2_0_5}
	if Video.NewSDL then
		SDL_SetWindowInputFocus(Video.Window);
	{$ENDIF}

    ClearMessageQueue;
    Locked := False;
end;

procedure TWindow.SyncFramerate;
var
	delayMs, timeNow_64bit: UInt64;
begin
	if (Visible) and (Video.HaveVSync or Locked) or (PerfFreq = 0) then Exit;
	timeNow_64bit := SDL_GetPerformanceCounter;
	if Video.NextFrameTime > timeNow_64bit then
	begin
		delayMs := Trunc((Video.NextFrameTime - timeNow_64bit) * (1000.0 / PerfFreq) + 0.5);
		SDL_Delay(delayMs);
	end;
	Inc(Video.NextFrameTime, Trunc(PerfFreq / Video.DesiredFramerate + 0.5));
end;

procedure TWindow.FlipFrame;
var
	i: Integer;
begin
	if Locked then Exit;

	for i := 0 to Renderers.Count-1 do
	    if Renderers[i] <> nil then
			Renderers[i].Render;

	SDL_RenderPresent(Video.Renderer);
end;

function TWindow.ProcessFrame(Tick: UInt32 = 0): UInt32;
begin
	if not Locked then
	begin
		if Tick = 0 then Tick := SDL_GetTicks;
		Result := SDL_GetTicks - Tick;

		if (Result < 16) or (SkippedFrames >= 5) then // 5
		begin
			SyncFramerate;
			FlipFrame;
			SkippedFrames := 0;
		end
		else
			Inc(SkippedFrames);

	end
	else
		Result := 0;

	HandleInput;
end;

// ================================================================================================
// TWindow
// ================================================================================================

constructor TWindow.Create;
var
	S: String;
{var
	Dir: String;
	i: Integer;
	Warnings: Boolean = False;}
begin
	inherited Create;

	Initialized := False;

	BasementMainWindow := Self;
	Settings := BasementOptions;

	Mouse.CurrentCursor.Cursor := nil;
	Mouse.CurrentCursor.Kind := 0;

	Scale := Settings.Scale;
	LastTitle := '';

	GamePads := TFPList.Create;
	Initialize;

	if not QuitFlag then
	begin
		Logo := TBitmap32.Create(1, 1);
		Logo.LoadPNGFromResource('LOGO');

		{if Assigned(SDL.Log) then
			SDL.Log.SDL_LogSetOutputFunction(SDLLogFunc, nil)
		else
			Log('Couldn''t set up SDL logging!');}

		if not Video.NewSDL then
			LogWarning('Using an older version of SDL. (< 2.0.5)');

		if Video.SyncRate = 0 then
			S := 'unknown'
		else
			S := Video.SyncRate.ToString;
		S := Format('Video: SDL %s, %s renderer at %s Hz', [Video.LibraryVersion, Video.RendererName, S]);
		if Video.HaveVSync then	S := S + ' VSync';
		LogInfo(S);

		{$IFDEF LIMIT_KEYBOARD_EVENTS}
		PrevKeyTimeStamp := 0;
		{$ENDIF}

		SetFullScreen(Settings.FullScreen);
		//SDL.Timer.SDL_AddTimer(TimerInterval, TimerTickCallback, nil);
	end;

	Initialized := True;
end;

procedure TWindow.Initialize;
begin
	QuitFlag := False;
	Locked := True;
	SkippedFrames := 0;

	// Init application directories
	//
	{ConfigPath := GetAppConfigDir(False);
	if ConfigPath = '' then
		ConfigPath := DataPath;
	ConfigPath := IncludeTrailingPathDelimiter(ConfigPath);
	ForceDirectories(ConfigPath);}

	// Init SDL
	//
//	LogInfo('Setting up video...');

	if not SetupVideo then
	begin
		LogFatal('Could not initialize video!');
		QuitFlag := True;
		Exit;
	end;
end;

procedure TWindow.Uninitialize;
begin
	if Initialized then
	begin
		Initialized := False;

		FreeAndNil(FrameBuffer);

		if Video.Renderer <> nil then
			SDL_DestroyRenderer(Video.Renderer);
		if Video.Texture <> nil then
			SDL_DestroyTexture(Video.Texture);
		if Video.Window <> nil then
			SDL_DestroyWindow(Video.Window);

		SDL_Quit;
	end;
end;

destructor TWindow.Destroy;
begin
	if Mouse.CurrentCursor.Cursor <> nil then
		SDL_FreeCursor(Mouse.CurrentCursor.Cursor);

	GamePads.Free;
	Logo.Free;

	Uninitialize;

	inherited Destroy;
end;

procedure TWindow.Close;
begin
	QuitFlag := True;
end;

procedure TWindow.SetTitle(const Title: AnsiString);
begin
	LastTitle := Title;
	if Initialized then
		SDL_SetWindowTitle(Video.Window, PAnsiChar(Title));
end;

function TWindow.SetFrameRate(Rate: Double): Boolean;
begin
	Result := False;
	if Rate <> Video.DesiredFramerate then
	begin
		Video.DesiredFramerate := Rate;
		Settings.Framerate := Rate;

		Settings.VSyncLimits.Min := Trunc(Rate - 4);
		Settings.VSyncLimits.Max := Trunc(Rate + 4);

		if Settings.VSyncMode <> VSYNC_OFF then
			Result := SetupVideo;
	end;
end;

function TWindow.ReinitWindow: Boolean;
begin
	Result := SetupVideo;
end;


initialization

	AppPath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
	DataPath := IncludeTrailingPathDelimiter(AppPath + 'data');
	DefaultFormatSettings.DecimalSeparator := '.';

	with BasementOptions do
	begin
		WinHandle := nil;
		FramebufferWidth  := 320;
		FramebufferHeight := 200;
		Scale := 0;
		MinScale := 1;

		X := WINDOWPOS_CENTERED;
		Y := WINDOWPOS_CENTERED;
		FullScreen := False;
		Maximized := False;
		AspectRatioWidthMultiplier := 1;
		Overscan := Types.Rect(0, 0, 0, 0);

		VSyncMode := VSYNC_AUTO; //VSYNC_OFF;
		Framerate := 60;
		VSyncLimits.Min := 49;
		VSyncLimits.Max := 61;
		AutoswitchResolution := False;

		UseGamepads := True;

		Caption := ApplicationName;
		CloseOnAltF4 := True;
		HighPriority := False;
		ScalingQuality := False; // nearest filtering
	end;

end.

