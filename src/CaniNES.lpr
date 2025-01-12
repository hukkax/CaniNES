program CaniNES;

{$MODE DELPHI}{$H+}
{$R *.res}
{$I canines.inc}
{$I basement.inc}

uses
	{$I basement-prestart1.inc}
	Canines.Main,
	Basement.Window;

begin
	{$I basement-startup.inc}

	{$IF DECLARED(UseHeapTrace)}
	if UseHeapTrace then
		WriteLn('HeapTrace enabled and active')
	else
		WriteLn('HeapTrace enabled but inactive');
	{$ENDIF}

	CaniNES_Init;

	while not QuitFlag do
	begin
		CaniNES_ProcessFrame;
	end;

	CaniNES_Shutdown;
end.

