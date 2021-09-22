	{$IFDEF UNIX}
	{$IFDEF BASS}
	with TDummyThread.Create(False) do
	begin
		WaitFor;
		Free;
	end;
	{$ENDIF}
	{$ENDIF}

	{$IF declared(UseHeapTrace)}
	GlobalSkipIfNoLeaks := True;
	SetHeapTraceOutput('trace.log');
	{$ENDIF}
