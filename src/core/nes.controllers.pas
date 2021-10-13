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
		strobe: Boolean;
		port:   Byte;
		State:  array[0..7] of Byte;

		function  IsCurrentPort(addr: Word): Boolean;
		function  IsExpansionDevice: Boolean;

		function  HasCoordinates: Boolean; virtual;
		procedure SetCoordinates(pos: TPoint);
		function  GetCoordinates: TPoint;

		procedure StrobeProcessRead;
		procedure StrobeProcessWrite(value: Byte);

		procedure EnsureCapacity(minBitCount: Integer);
		function  GetByteIndex(bit: Byte): Cardinal;

		procedure SetBit(bit: Byte);
		procedure ClearBit(bit: Byte);
		procedure InvertBit(bit: Byte);

		function  IsPressed(bit: Byte): Boolean;
		procedure SetPressedState(bit: Byte; keyCode: Cardinal);
		procedure SetPressedState(bit: Byte; enabled: Boolean);
		procedure RefreshStateBuffer; virtual;
		procedure ClearState;
		procedure InternalSetStateFromInput(B: Byte); virtual;
	public
		procedure SetStateFromInput(B: Byte);

		function  ReadRAM(address: Word): Byte; virtual;
		procedure WriteRAM(address: Word; value: Byte); virtual;
		procedure Reset; virtual;

		procedure Visualize; virtual;

		constructor Create(PortNumber: Byte); overload;
		destructor  Destroy; override;
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
	Basement.Window, Math,
	NES.Config, NES.Console, NES.Mapper,
	NES.Controller.Standard,
	NES.Controller.Zapper,
	NES.Controller.OekaKidsTablet;

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

	SecondaryController := nil;

	case InputType of

		gitZapper, gitTwoZappers:
		begin
			if InputType = gitTwoZappers then
				PrimaryController := TZapperController.Create(0)
			else
				PrimaryController := TStandardController.Create(0);
			SecondaryController := TZapperController.Create(1);
		end;

		gitOekaKidsTablet:
		begin
			PrimaryController   := TOekaKidsTablet.Create(0);
			SecondaryController := TStandardController.Create(1);
		end

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

procedure TBaseControlDevice.ClearState;
begin
	//State := ControlDeviceState;
	FillByte(State[0], Length(State)*1, 0);
end;

procedure TBaseControlDevice.RefreshStateBuffer;
begin
end;

procedure TBaseControlDevice.InternalSetStateFromInput(B: Byte);
begin
end;

procedure TBaseControlDevice.SetStateFromInput(B: Byte);
begin
	ClearState;
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

procedure TBaseControlDevice.SetPressedState(bit: Byte; enabled: Boolean);
begin
	if Enabled then SetBit(bit);
end;

procedure TBaseControlDevice.SetPressedState(bit: Byte; keyCode: Cardinal);
begin
//	if(IsKeyboard() && keyCode < 0x200 && !_console->GetSettings()->IsKeyboardMode()) {
//		//Prevent keyboard device input when keyboard mode is off
//		return;

//	if(_console->GetSettings()->InputEnabled() && (!_console->GetSettings()->IsKeyboardMode() || keyCode >= 0x200 || IsKeyboard()) && KeyManager::IsKeyPressed(keyCode)) {
//		SetBit(bit);
end;

procedure TBaseControlDevice.EnsureCapacity(minBitCount: Integer);
begin
//	uint32_t minByteCount = minBitCount / 8 + 1 + (HasCoordinates() ? 32 : 0);
//	int32_t gap = minByteCount - (int32_t)_state.State.size();

//	if gap > 0 then
//		State.insert(_state.State.end(), gap, 0);
end;

function TBaseControlDevice.GetByteIndex(bit: Byte): Cardinal;
begin
	Result := (bit div 8) + IfThen(HasCoordinates, 4, 0);
end;

procedure TBaseControlDevice.SetBit(bit: Byte);
var
	n, BitMask: Byte;
begin
	EnsureCapacity(bit);
	BitMask := 1 shl (bit mod 8);
	n := GetByteIndex(bit);
	State[n] := State[n] or BitMask;
end;

procedure TBaseControlDevice.ClearBit(bit: Byte);
var
	n, BitMask: Byte;
begin
	EnsureCapacity(bit);
	BitMask := 1 shl (bit mod 8);
	n := GetByteIndex(bit);
	State[n] := State[n] and (not BitMask);
end;

procedure TBaseControlDevice.InvertBit(bit: Byte);
begin
	if IsPressed(bit) then
		ClearBit(bit)
	else
		SetBit(bit);
end;

function TBaseControlDevice.IsPressed(bit: Byte): Boolean;
var
	BitMask: Byte;
begin
	EnsureCapacity(bit);
	BitMask := 1 shl (bit mod 8);
	Result := (State[GetByteIndex(bit)] and BitMask) <> 0;
end;


end.

