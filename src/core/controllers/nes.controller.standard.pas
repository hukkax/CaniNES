unit NES.Controller.Standard;

{$MODE DELPHI}

interface

uses
	Classes, SysUtils,
	NES.Controllers;

type
	TStandardController = class(TBaseControlDevice)
	private
		stateBuffer: Cardinal;
	protected
		procedure RefreshStateBuffer; override;
		procedure InternalSetStateFromInput(B: Byte); override;
	public
		ControllerState: TStandardControllerState;
		Visual: TPadVisual;

		constructor Create(PortNumber: Byte); overload;

		procedure Visualize; override;

		function  ReadRAM(address: Word): Byte; override;
		procedure WriteRAM(address: Word; value: Byte); override;
		procedure Reset; override;
	end;

implementation

uses
	NES.Types, NES.Config, NES.Console;

// ================================================================================================
// TStandardController
// ================================================================================================

constructor TStandardController.Create(PortNumber: Byte);
begin
	inherited Create(PortNumber);

	RegisterProperty(32, @stateBuffer);
	RegisterProperty(8,  @ControllerState.value);

	Visual.Init(PortNumber);
end;

procedure TStandardController.RefreshStateBuffer;
begin
	if (Configuration.Input.FourScore) and (Console.System <> Famicom) then
	begin
		if port >= 2 then
			stateBuffer := ControllerState.value << 8
		else
			//Add some 0 bit padding to allow P3/P4 controller bits + signature bits
			stateBuffer := $FF000000 or ControllerState.value;
	end
	else
		stateBuffer := $FFFFFF00 or ControllerState.value;
end;

procedure TStandardController.InternalSetStateFromInput(B: Byte);
begin
	ControllerState.value := B;
	Visual.ControllerState.value := B;
end;

function TStandardController.ReadRAM(address: Word): Byte;
begin
	if ((address = $4016) and (not Odd(port))) or ((address = $4017) and (Odd(port))) then
	begin
		StrobeProcessRead;

		Result := stateBuffer and 1;
		if (port >= 2) and (Console.System = Famicom) then
			Result := Result << 1; // Famicom outputs P3 & P4 on bit 1
		stateBuffer := stateBuffer >> 1;
		// "All subsequent reads will return D=1 on an authentic controller
		// but may return D=0 on third party controllers."
		stateBuffer := stateBuffer or $80000000;
	end
	else
		Result := 0;
end;

procedure TStandardController.WriteRAM(address: Word; value: Byte);
begin
	StrobeProcessWrite(value);
end;

procedure TStandardController.Reset;
begin
	stateBuffer := 0;
	InternalSetStateFromInput(0);
end;

procedure TStandardController.Visualize;
begin
	Visual.Draw;
end;

end.

