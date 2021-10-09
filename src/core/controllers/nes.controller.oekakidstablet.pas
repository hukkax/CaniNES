unit NES.Controller.OekaKidsTablet;

{$MODE DELPHI}

interface

uses
	Classes, SysUtils,
	NES.Controllers;

type
	TTabletButtons = ( Click, Touch );

	TOekaKidsTablet = class(TBaseControlDevice)
	protected
		strobe, shift: Boolean;
		stateBuffer:   Cardinal;
		Buttons:       TTabletButtons;

		function  HasCoordinates: Boolean; override;
		procedure InternalSetStateFromInput(B: Byte); override;
	public
		function  ReadRAM(address: Word): Byte; override;
		procedure WriteRAM(address: Word; value: Byte); override;
	end;

implementation

uses
	Math, Basement.Window,
	NES.Config, NES.Console, NES.InputManager;

// ================================================================================================
// TOekaKidsTablet
// ================================================================================================

function TOekaKidsTablet.HasCoordinates: Boolean;
begin
	Result := True;
end;

procedure TOekaKidsTablet.InternalSetStateFromInput(B: Byte);
var
	Pos: TPoint;
	Pressed: Boolean;
begin
	//if Configuration.Input.
	begin
		Pos := InputManager.Mouse.Pos;
		Pressed := InputManager.Mouse.Buttons[mbLeft];

		SetPressedState(Ord(Click), Pressed);
		SetPressedState(Ord(Touch), (Pos.Y >= 48) or Pressed);
		SetCoordinates(Pos);
	end;
end;

function TOekaKidsTablet.ReadRAM(address: Word): Byte;
begin
	if address = $4017 then
	begin
		if strobe then
		begin
			if shift then
				Result := IfThen((stateBuffer and $40000) <> 0, $00, $08)
			else
				Result := $04;
		end
		else
			Result := $00;
	end
	else
		Result := 0;
end;

procedure TOekaKidsTablet.WriteRAM(address: Word; value: Byte);
var
	_shift: Boolean;
	pos: TPoint;
	xPosition, yPosition: Byte;
begin
	strobe := Odd(value);
	_shift := Odd(value shr 1);

	if strobe then
	begin
		if (not shift and _shift) then
			stateBuffer := stateBuffer shl 1;
		shift := _shift;
	end
	else
	begin
		pos := GetCoordinates;
		xPosition := Trunc(Max(0, pos.X + 8)  / 256.0 * 240);
		yPosition := Trunc(Max(0, pos.Y - 14) / 240.0 * 256);
		stateBuffer := (xPosition shl 10) or (yPosition shl 2) or
			(IfThen(IsPressed(Ord(Touch)), $02, $00)) or
			(IfThen(IsPressed(Ord(Click)), $01, $00));
	end;
end;

end.

