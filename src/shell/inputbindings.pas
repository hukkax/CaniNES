unit InputBindings;

{$MODE DELPHI}

interface

uses
	Classes, SysUtils;

type
	TAction =
	(
		// Menu
		actMenuShow, actMenuBack, actMenuSelect, actAltSelect,

		// Controller 1
		actPadUp, actPadDown, actPadLeft, actPadRight,
		actPadSelect, actPadStart,
		actPadA, actPadB,
		actPadTurboA, actPadTurboB,
		// Controller 2
		actPadUp2, actPadDown2, actPadLeft2, actPadRight2,
		actPadSelect2, actPadStart2,
		actPadA2, actPadB2,
		actPadTurboA2, actPadTurboB2,
		// Zapper
		actZapperTrigger,

		// Console
		actFastForward, actRewind, actMoviePlayback,
		actConsolePause, actConsoleReset, actConsoleRestart,

		// Savestates
		actStateLoad, actStateSave,
		actStateSelectPrev, actStateSelectNext,
		actStateSelect0, actStateSelect1, actStateSelect2, actStateSelect3, actStateSelect4,
		actStateSelect5, actStateSelect6, actStateSelect7, actStateSelect8, actStateSelect9,

		// Application
		actAppExit,
		actFavourite, actBookmarks,
		actListCheats, actROMBrowser, actCartInfo,
		actROMLoadPrevious, actRecordWAV,
		actToggleInfoBox, actToggleFullscreen,
		actToggleFilterCRT, actToggleFilterNTSC, actToggleFilterNTSC_MergeFields
	);

	TInputBinding = packed record
		Key:       Integer;
		Shift:     Boolean;
		IsShift:   Boolean;
		PadNum:    Int16;
		PadButton: Byte;
	end;
	PInputBinding = ^TInputBinding;


const
	ContinuousActions: set of TAction = [
		// Controller 1
		actPadUp, actPadDown, actPadLeft, actPadRight,
		actPadSelect, actPadStart,
		actPadA, actPadB,
		actPadTurboA, actPadTurboB,
		// Controller 2
		actPadUp2, actPadDown2, actPadLeft2, actPadRight2,
		actPadSelect2, actPadStart2,
		actPadA2, actPadB2,
		actPadTurboA2, actPadTurboB2,
		// Console
		actFastForward, actRewind
	];


var
//	ActionActive: array[TAction] of Boolean;
	Bindings:     array[TAction] of TInputBinding;
	BindingNames: array[TAction] of AnsiString;

	function  IsBindingShift(Binding: TInputBinding): Boolean;
	procedure FixBindingShift(Binding: PInputBinding);
	procedure SetInputBinding(Action: TAction; Key: Integer; Shift: Boolean = False);
	procedure JoyInputBinding(Action: TAction; PadNum, Button: Integer);
	procedure InitDefaultInputBindings;
	procedure LoadBindings;
	procedure SaveBindings;


implementation

uses
	SDL2, IniFiles, Math, ConfigurationManager, NES.Config;

const
	SECT_KEYBOARD = 'Bindings.Keyboard';
	SECT_JOYPAD   = 'Bindings.JoyPad';


function IsBindingShift(Binding: TInputBinding): Boolean;
begin
	Result := (Binding.Key = SDLK_LSHIFT) or (Binding.Key = SDLK_RSHIFT);
end;

procedure FixBindingShift(Binding: PInputBinding);
begin
	Binding.IsShift := IsBindingShift(Binding^);
	if Binding.IsShift then
		Binding.Shift := False;
end;

procedure SetInputBinding(Action: TAction; Key: Integer; Shift: Boolean = False);
var
	S: AnsiString;
begin
	Bindings[Action].Key := Key;
	Bindings[Action].Shift := Shift;
	Bindings[Action].PadNum := -1;
	Bindings[Action].PadButton := 0;

	WriteStr(S, Action);
	BindingNames[Action] := Copy(S, 4, 99);
end;

procedure JoyInputBinding(Action: TAction; PadNum, Button: Integer);
begin
	Bindings[Action].PadNum := PadNum;
	Bindings[Action].PadButton := Button;
end;

procedure InitDefaultInputBindings;
begin
	// Controller 1
	SetInputBinding(actPadUp, SDLK_UP);
	SetInputBinding(actPadDown, SDLK_DOWN);
	SetInputBinding(actPadLeft, SDLK_LEFT);
	SetInputBinding(actPadRight, SDLK_RIGHT);
	SetInputBinding(actPadSelect, SDLK_RSHIFT);
	SetInputBinding(actPadStart, SDLK_RETURN);
	SetInputBinding(actPadA, SDLK_X);
	SetInputBinding(actPadB, SDLK_Z);
	SetInputBinding(actPadTurboA, SDLK_X, True);
	SetInputBinding(actPadTurboB, SDLK_Z, True);
	// Controller 2
	SetInputBinding(actPadUp2, SDLK_KP_8);
	SetInputBinding(actPadDown2, SDLK_KP_2);
	SetInputBinding(actPadLeft2, SDLK_KP_4);
	SetInputBinding(actPadRight2, SDLK_KP_6);
	SetInputBinding(actPadSelect2, SDLK_KP_PERIOD);
	SetInputBinding(actPadStart2, SDLK_KP_ENTER);
	SetInputBinding(actPadA2, SDLK_V);
	SetInputBinding(actPadB2, SDLK_C);
	SetInputBinding(actPadTurboA2, 0, True);
	SetInputBinding(actPadTurboB2, 0, True);
	// Zapper
	SetInputBinding(actZapperTrigger, 0);

	// Menu
	SetInputBinding(actMenuShow, SDLK_ESCAPE);
	SetInputBinding(actMenuBack, SDLK_BACKSPACE);
	SetInputBinding(actMenuSelect, SDLK_RETURN);
	SetInputBinding(actAltSelect, SDLK_RETURN, True);

	SetInputBinding(actFavourite, SDLK_F,  True);
	SetInputBinding(actBookmarks, SDLK_F2, True);

	// Savestates
	SetInputBinding(actStateLoad,    SDLK_F7);
	SetInputBinding(actStateSave,    SDLK_F5);
	SetInputBinding(actStateSelectPrev, 0);
	SetInputBinding(actStateSelectNext, 0);
	SetInputBinding(actStateSelect0, SDLK_0);
	SetInputBinding(actStateSelect1, SDLK_1);
	SetInputBinding(actStateSelect2, SDLK_2);
	SetInputBinding(actStateSelect3, SDLK_3);
	SetInputBinding(actStateSelect4, SDLK_4);
	SetInputBinding(actStateSelect5, SDLK_5);
	SetInputBinding(actStateSelect6, SDLK_6);
	SetInputBinding(actStateSelect7, SDLK_7);
	SetInputBinding(actStateSelect8, SDLK_8);
	SetInputBinding(actStateSelect9, SDLK_9);

	// Console
	SetInputBinding(actMoviePlayback,  SDLK_M, True);
	SetInputBinding(actFastForward,    SDLK_TAB);
	SetInputBinding(actRewind,         SDLK_BACKSPACE);
	SetInputBinding(actConsolePause,   SDLK_SPACE);
	SetInputBinding(actConsoleReset,   SDLK_R);
	SetInputBinding(actConsoleRestart, SDLK_R, True);

	// Application
	SetInputBinding(actAppExit, 0);
	SetInputBinding(actListCheats, SDLK_F2);
	SetInputBinding(actCartInfo, SDLK_I);
	SetInputBinding(actROMBrowser, SDLK_F9);
	SetInputBinding(actROMLoadPrevious, SDLK_L);
	SetInputBinding(actRecordWAV, SDLK_W, True);
	SetInputBinding(actToggleInfoBox, SDLK_F1);
	SetInputBinding(actToggleFullscreen, SDLK_F11);
	SetInputBinding(actToggleFilterNTSC, SDLK_F3);
	SetInputBinding(actToggleFilterCRT, SDLK_F4);
	SetInputBinding(actToggleFilterNTSC_MergeFields, SDLK_F2);

	// Controller 1
	JoyInputBinding(actPadUp, 0, 11);
	JoyInputBinding(actPadDown, 0, 12);
	JoyInputBinding(actPadLeft, 0, 13);
	JoyInputBinding(actPadRight, 0, 14);
	JoyInputBinding(actPadSelect, 0, 4);
	JoyInputBinding(actPadStart, 0, 6);
	JoyInputBinding(actPadA, 0, 0);
	JoyInputBinding(actPadB, 0, 2);
	//JoyInputBinding(actPadTurboA,
	//JoyInputBinding(actPadTurboB,

	JoyInputBinding(actMenuSelect, 0, 0);
	JoyInputBinding(actMenuShow, 0, 9);
	JoyInputBinding(actMenuBack, 0, 10);
end;

procedure LoadBindings;
var
	Ini: TIniFile;
	Action: TAction;
	Binding: ^TInputBinding;
	S, R: AnsiString;
	X: Integer;
begin
	Ini := ConfigManager.OpenINI;
	if Assigned(Ini) then
	try
		for Action in TAction do
		begin
			Binding := @Bindings[Action];

			S := Ini.ReadString(SECT_KEYBOARD, BindingNames[Action], '');
			if S <> '' then
			begin
				X := Length(S);
				Binding.Shift := False;
				if (X > 1) and (S[1] = '!') then
				begin
					Binding.Shift := True;
					S := Copy(S, 2, X);
				end;
				Binding.Key := IfThen(S <> '', SDL_GetKeyFromName(PAnsiChar(S)), 0);
				FixBindingShift(Binding);
			end;

			S := Ini.ReadString(SECT_JOYPAD, BindingNames[Action], '');
			if S <> '' then
			begin
				X := S.IndexOf(':') + 1;
				if X > 1 then
				begin
					R := Copy(S, X+1, Length(S));
					S := Copy(S, 1, X-1);
					Binding.PadNum := S.ToInteger;
					Binding.PadButton := R.ToInteger;
				end;
			end;

		end;
	finally
		Ini.Free;
	end;
end;

procedure SaveBindings;
var
	Ini: TIniFile;
	Action: TAction;
	Binding: TInputBinding;
	S: AnsiString;
begin
	Ini := ConfigManager.OpenINI;
	if Assigned(Ini) then
	try
		Ini.EraseSection(SECT_JOYPAD);

		for Action in TAction do
		begin
			Binding := Bindings[Action];
			S := '';

			if Binding.Key <> 0 then
			begin
				S := SDL_GetKeyName(Binding.Key);
				if Binding.Shift then S := '!' + S;
			end;
			Ini.WriteString(SECT_KEYBOARD, BindingNames[Action], S);

			if Binding.PadNum >= 0 then
			begin
				S := Format('%d:%d', [Binding.PadNum, Binding.PadButton]);
				Ini.WriteString(SECT_JOYPAD, BindingNames[Action], S);
			end;
		end;

	finally
		Ini.Free;
	end;
end;

end.

