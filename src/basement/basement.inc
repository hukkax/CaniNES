// enable C-style operators
{$COperators on}

// remove annoying "Case statement does not handle all possible cases" warning
{$if fpc_fullversion >= 030300} {$warn 6060 off} {$endif}

{$IFDEF UNIX}
	{$DEFINE DISABLE_FULLSCREEN}		// disable broken SDL fullscreen mode
	{.$DEFINE LIMIT_KEYBOARD_EVENTS}	// fix duplicate keyboard events on Linux with FCITX
	{.$DEFINE DISABLE_RENDERCLEAR}		// don't call SDL_RenderClear (crashes on my VM)
{$ENDIF}

{$IFDEF WINDOWS}
{$ENDIF}

{$DEFINE RENDER_FONT_FREETYPE}		// enable FreeType font renderer
{.$DEFINE RENDER_FONT_BITMAP}		// enable faster but uglier bitmap fonts

