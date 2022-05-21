program CaniNES_GUI;

{$MODE DELPHI}{$H+}

{$I canines.inc}
{$I basement.inc}

uses
	{$I basement-prestart1.inc}

	{$IFDEF UNIX}cthreads,{$ENDIF}
	{$IFDEF HASAMIGA}athreads,{$ENDIF}
	Interfaces, Forms,
	NES.Types,
	Form.Main;

{$R *.res}

begin
	{$I basement-startup.inc}

	RequireDerivedFormResource := True;
	Application.Title := APPNAME;
	Application.Scaled := True;
	Application.Initialize;
	Application.CreateForm(TFormMain, FormMain);
	Application.Run;
end.

