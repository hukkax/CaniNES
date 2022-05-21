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

	CaniNES_Init;

	while not QuitFlag do
	begin
		CaniNES_ProcessFrame;
	end;

	CaniNES_Shutdown;
end.

