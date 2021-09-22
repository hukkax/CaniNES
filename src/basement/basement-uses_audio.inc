	{$IFDEF BASS}
		{$IFDEF BASS_DYNAMIC}
		lazdynamic_bass,
		{$ELSE}
		BASS,
		{$ENDIF}
	{$ELSE}
	SDL2,
	{$ENDIF}
