unit NES.Controllers;

{$MODE DELPHI}

interface

uses
	Classes, Types, SysUtils, Generics.Collections,
	NES.Types, NES.MemoryHandler, NES.SaveState,
	Graphics32, Basement.Renderer.Overlay;

const
	PadsScale   = 1;
	PadsMargin  = 2;
	PadsSizeX   = 33+3;
	PadsSizeY   = 10+3;

type
	TStandardControllerState = bitpacked record
	case Integer of
		0: ( value: Byte );
		1: ( A, B, Select, Start, Up, Down, Left, Right: Boolean );
	end;

	TPadVisual = record
		Index:   Byte;
		Area:    TRect;

		Buffer:  TBitmap32;

		ControlRects:    array[0..8] of TRect;
		ControllerState: TStandardControllerState;

		procedure Init(PadIndex: Byte);
		procedure Draw;
	end;

	TBaseControlDevice = class(TSnapshotable)
	protected
		function  IsCurrentPort(addr: Word): Boolean;
		function  IsExpansionDevice: Boolean;

		function  HasCoordinates: Boolean; virtual;
		procedure SetCoordinates(pos: TPoint);
		function  GetCoordinates: TPoint;

		procedure StrobeProcessRead;
		procedure StrobeProcessWrite(value: Byte);

		{procedure SetPressedState(bit: Byte; keyCode: Cardinal);
		procedure SetPressedState(bit: Byte; enabled: Boolean);
		procedure ClearState;}
		procedure RefreshStateBuffer; virtual;

		procedure InternalSetStateFromInput(B: Byte); virtual;
	private
		strobe: Boolean;
		port:   Byte;
		State:  array[0..7] of Byte;
	public
		procedure SetStateFromInput(B: Byte);

		function  ReadRAM(address: Word): Byte; virtual;
		procedure WriteRAM(address: Word; value: Byte); virtual;
		procedure Reset; virtual;

		procedure Visualize; virtual;

		constructor Create(PortNumber: Byte); overload;
		destructor  Destroy; override;
	end;


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


	TControlManager = class(TIMemoryHandler)
	private
		function  GetOpenBusMask(port: Byte): Byte;
	public
		ControlDevices: TObjectList<TBaseControlDevice>;

		PrimaryController,
		SecondaryController: TBaseControlDevice;

		OE1address: Word;
		OE1strobed: ByteBool;

		procedure GetMemoryRanges(var ranges: TMemoryRanges); override;
		function  ReadRAM(address: Word): Byte; override;
		procedure WriteRAM(address: Word; value: Byte); override;
		procedure Reset;
		procedure GetInvOE1; // inline;

		procedure PadVisualizationChanged;

		constructor Create(const RegisterName: AnsiString; InputType: TGameInputType); overload;
		destructor  Destroy; override;
	end;

var
	PadOverlay: TOverlayRenderer;


implementation

uses
	Math,
	NES.Config, NES.Console, NES.Mapper,
	NES.InputManager, Basement.Window;

var
	CtrlColor: array[Boolean] of Cardinal;

// ================================================================================================
// TPadVisual
// ================================================================================================
// 0..7 = A, B, Select, Start, Up, Down, Left, Right

{
R	7,4		3,3
L	1,4		3,3
D	4,7		3,3
U	4,1		3,3
Sta	18,5	4,2
Sel	13,5	4,2
B	25,6	3,3
A	30,6	3,3
empty 4,4	3,3
box 33,10
}

procedure TPadVisual.Init(PadIndex: Byte); // 0..3
var
	X, Y, i: Integer;
begin
	Index := PadIndex;
	Buffer := PadOverlay.Framebuffer;

	CtrlColor[False] := Palette[COLOR_PAD_INACTIVE];
	CtrlColor[True]  := Palette[COLOR_PAD_ACTIVE];

	Y := 0;
	X := Buffer.Width - PadsSizeX - ((PadsSizeX + PadsMargin - 1) * index);

	Area := Bounds(X, Y, PadsSizeX, PadsSizeY);

	ControlRects[0] := Bounds(30,06, 3,3); // A
	ControlRects[1] := Bounds(25,06, 3,3); // B
	ControlRects[2] := Bounds(13,05, 4,2); // Select
	ControlRects[3] := Bounds(18,05, 4,2); // Start
	ControlRects[4] := Bounds(04,01, 3,3); // Up
	ControlRects[5] := Bounds(04,07, 3,3); // Down
	ControlRects[6] := Bounds(01,04, 3,3); // Left
	ControlRects[7] := Bounds(07,04, 3,3); // Right
	ControlRects[8] := Bounds(04,04, 3,3); // ---

	for i := 0 to High(ControlRects) do
		ControlRects[i].Offset(X+1, Y+1);
end;

procedure TPadVisual.Draw;
var
	i: Integer;
begin
	if (PadOverlay.Visible = False) or
		(Index >= Configuration.Input.PadVisual.ControllerCount) then
			Exit;

	Buffer := PadOverlay.Framebuffer;
	Buffer.FillRect (Area, Palette[COLOR_PAD_BACKGROUND]);
	Buffer.FrameRect(Area, Palette[COLOR_PAD_BORDER]);
	Buffer.FillRect(ControlRects[8], CtrlColor[False]);
	for i := 0 to 7 do
		Buffer.FillRect(ControlRects[i], CtrlColor[ControllerState.value.TestBit(i)]);
end;

// ================================================================================================
// TControlManager
// ================================================================================================

constructor TControlManager.Create(const RegisterName: AnsiString; InputType: TGameInputType);
begin
	inherited Create(RegisterName);

	ControlDevices := TObjectList<TBaseControlDevice>.Create(True);

	case InputType of

		gitZapper, gitTwoZappers:
		begin
			if InputType = gitTwoZappers then
				PrimaryController := TZapperController.Create(0)
			else
				PrimaryController := TStandardController.Create(0);
			SecondaryController := TZapperController.Create(1);
		end;

	else
		PrimaryController   := TStandardController.Create(0);
		SecondaryController := TStandardController.Create(1);
	end;

	ControlDevices.Add(PrimaryController);
	ControlDevices.Add(SecondaryController);

	case InputType of
		gitFourScore, gitFourPlayerAdapter:
		begin
			ControlDevices.Add(TStandardController.Create(2));
			ControlDevices.Add(TStandardController.Create(3));
		end;
	end;
end;

destructor TControlManager.Destroy;
begin
	ControlDevices.Free;

	inherited Destroy;
end;

procedure TControlManager.PadVisualizationChanged;
begin
	if not Assigned(PadOverlay) then Exit;

	PadOverlay.FrameBuffer.Clear(0);
	PadOverlay.Opacity := Configuration.Display.GUI.OSD.PadsOpacity;
	PadOverlay.Visible := (Configuration.Input.PadVisual.ControllerCount > 0);
	if (PadOverlay.Visible) and (Configuration.Input.PadVisual.MoviesOnly) then
		PadOverlay.Visible := Console.MovieManager.IsPlaying;
end;

function TControlManager.GetOpenBusMask(port: Byte): Byte;
begin
	// "In the NES and Famicom, the top three (or five) bits are not driven,
	// and so retain the bits of the previous byte on the bus.
	// Usually this is the most significant byte of the address of the controller port - 0x40.
	// Paperboy relies on this behavior and requires that reads from the controller
	// ports return exactly $40 or $41 as appropriate."

	Result := $E0;

	if Console.System = Famicom then
	begin
		if port = 0 then Result := $F8;
	end
	else
	begin
		//if(_console->GetSettings()->CheckFlag(EmulationFlags::UseNes101Hvc101Behavior))
			//if port = 0 then Result := $E4;
	end;
end;

procedure TControlManager.GetMemoryRanges(var ranges: TMemoryRanges);
begin
	ranges.AddHandler(moRead,  $4016,  $4017);
	ranges.AddHandler(moWrite, $4016);
end;

function TControlManager.ReadRAM(address: Word): Byte;
var
	Device: TBaseControlDevice;
begin
	OE1address := address;
	Result := Console.MemoryManager.GetOpenBus(GetOpenBusMask(address - $4016));
	for Device in ControlDevices do
	begin
		Result := Result or Device.ReadRAM(address);
		OE1strobed := True;
	end;
end;

procedure TControlManager.WriteRAM(address: Word; value: Byte);
var
	Device: TBaseControlDevice;
begin
	for Device in ControlDevices do
		Device.WriteRAM(address, value);
end;

procedure TControlManager.Reset;
begin
	PrimaryController.Reset;
	SecondaryController.Reset;
end;

procedure TControlManager.GetInvOE1;
begin
	// pull low for only one clock
	if OE1address = $4016 then
		Mapper.InvOE1Audio.Interference := Byte(OE1strobed); // invert relative to 2A03
	OE1strobed := False;
end;

// ================================================================================================
// TBaseControlDevice
// ================================================================================================

constructor TBaseControlDevice.Create(PortNumber: Byte);
begin
	inherited Create('CTRL');

	port := PortNumber;
	strobe := False;

	RegisterProperty(8, @port);
	RegisterProperty(1, @strobe);
end;

destructor TBaseControlDevice.Destroy;
begin
	inherited Destroy;
end;

function TBaseControlDevice.IsCurrentPort(addr: Word): Boolean;
begin
	Result := (port = (addr - $4016));
end;

function TBaseControlDevice.IsExpansionDevice: Boolean;
begin
	Result := False; // port == BaseControlDevice::ExpDevicePort;
end;

function TBaseControlDevice.HasCoordinates: Boolean;
begin
	Result := False;
end;

procedure TBaseControlDevice.SetCoordinates(pos: TPoint);
begin
	State[0] := pos.X and $FF;
	State[1] := (pos.X >> 8) and $FF;
	State[2] := pos.Y and $FF;
	State[3] := (pos.Y >> 8) and $FF;
end;

function TBaseControlDevice.GetCoordinates: TPoint;
begin
	Result.X := State[0] or (State[1] << 8);
	Result.Y := State[2] or (State[3] << 8);
end;

procedure TBaseControlDevice.StrobeProcessRead;
begin
	if strobe then RefreshStateBuffer;
end;

procedure TBaseControlDevice.StrobeProcessWrite(value: Byte);
var
	prevStrobe: Boolean;
begin
	prevStrobe := strobe;
	strobe := Odd(value);

	if (prevStrobe) and (not strobe) then
		RefreshStateBuffer;
end;

(*procedure TBaseControlDevice.SetPressedState(bit: Byte; keyCode: Cardinal);
begin
end;

procedure TBaseControlDevice.SetPressedState(bit: Byte; enabled: Boolean);
begin
end;

procedure TBaseControlDevice.ClearState;
begin
//	state := ControlDeviceState;
end;*)

procedure TBaseControlDevice.RefreshStateBuffer;
begin
end;

procedure TBaseControlDevice.InternalSetStateFromInput(B: Byte);
begin
end;

procedure TBaseControlDevice.SetStateFromInput(B: Byte);
begin
	//ClearState;
	InternalSetStateFromInput(B);
end;

function TBaseControlDevice.ReadRAM(address: Word): Byte;
begin
	Result := 0;
end;

procedure TBaseControlDevice.WriteRAM(address: Word; value: Byte);
begin
	StrobeProcessWrite(value);
end;

procedure TBaseControlDevice.Reset;
begin
//
end;

procedure TBaseControlDevice.Visualize;
begin
	// nothing
end;

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

