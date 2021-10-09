unit NES.Controller.Zapper;

{$MODE DELPHI}

interface

uses
	Classes, SysUtils,
	NES.Controllers;

type
	TZapperController = class(TBaseControlDevice)
	protected
		FirePressed: Boolean;

		function  IsLightFound: Boolean;
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
// TZapperController
// ================================================================================================

function TZapperController.HasCoordinates: Boolean;
begin
	Result := True;
end;

procedure TZapperController.InternalSetStateFromInput(B: Byte);
begin
	FirePressed := InputManager.Mouse.Buttons[mbLeft];

	if InputManager.Mouse.Buttons[mbRight] then
		SetCoordinates(Point(-1, -1))
	else
		SetCoordinates(InputManager.Mouse.Pos);
end;

function TZapperController.ReadRAM(address: Word): Byte;
begin
	if (IsExpansionDevice and (address = $4017)) or IsCurrentPort(address) then
		Result := IfThen(IsLightFound, 0, 8) or IfThen(FirePressed, $10, $00)
	else
		Result := 0;
end;

procedure TZapperController.WriteRAM(address: Word; value: Byte);
begin
	// nothing
end;

function TZapperController.IsLightFound: Boolean;
var
	pos: TPoint;
	scanline, cycle, radius,
	xOffset, yOffset, xPos, yPos: Integer;
begin
	Result := False;

	pos := InputManager.Mouse.Pos;
	if (pos.X < 0) or (pos.Y < 0) then Exit;

	scanline := NES_PPU.scanline;
	cycle := NES_PPU.cycle;
	radius := Configuration.Input.Zapper.DetectionRadius;

	for yOffset := -radius to radius do
	begin
		yPos := pos.Y + yOffset;
		if (yPos >= 0) and (yPos < NES_PPU.ScreenHeight) then
		begin
			for xOffset := -radius to radius do
			begin
				xPos := pos.X + xOffset;
				// Light cannot be detected if the Y/X position is further ahead
				// than the PPU, or if the PPU drew a dark color
				if (xPos >= 0) and (xPos < NES_PPU.ScreenWidth) then
					if (scanline >= yPos) and (scanline - yPos <= 20) and
						( (scanline <> yPos) or (cycle > xPos) ) and
						(NES_PPU.GetPixelBrightness(xPos, yPos) >= 85) then
							Exit(True);
			end;
		end;
	end;
end;

end.

