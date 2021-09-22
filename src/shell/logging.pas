unit Logging;

{$mode delphi}

interface

uses
	Classes, SysUtils;

type
	TLogger = class
	public
		Log: TStringList;

		procedure OnLogMessage(const Msg: AnsiString);

		constructor Create;
		destructor  Destroy; override;
	end;

var
	Logger: TLogger;


implementation

uses
	LazLoggerBase,
	Basement.Util;

constructor TLogger.Create;
begin
	inherited;

	Log := TStringList.Create;
	OnLog := OnLogMessage;
end;

destructor TLogger.Destroy;
begin
	OnLog := nil;
	Log.Free;

	inherited;
end;

procedure TLogger.OnLogMessage(const Msg: AnsiString);
begin
	Log.Add(Msg);
	DebugLn(Msg);
end;


initialization

	Logger := TLogger.Create;

finalization

	Logger.Free;

end.

