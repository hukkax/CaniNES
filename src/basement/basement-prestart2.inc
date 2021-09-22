	{$IFDEF UNIX}
	{$IFDEF BASS}
	// on Unix we need to initialize the threading system before
	// using custom callbacks with BASS or we crash!
	type
		TDummyThread = class(TThread)
			procedure Execute; override;
		end;

		procedure TDummyThread.Execute;
		begin
		end;
	{$ENDIF}
	{$ENDIF}

