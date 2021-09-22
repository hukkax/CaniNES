unit NES.MemoryHandler;

interface

{$MODE DELPHI}
{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}

uses
	Classes, SysUtils, Generics.Collections, FileStreamEx,
	NES.Types, NES.SaveState;

type
	TRamAddressList = TList<Word>;

	TMemoryRanges = class
	private
		ramReadAddresses,
		ramWriteAddresses: TRamAddressList;
		allowOverride: Boolean;
	public
		constructor Create;
		destructor  Destroy; override;

		function  GetRAMReadAddresses:  TRamAddressList;
		function  GetRAMWriteAddresses: TRamAddressList;
		function  GetAllowOverride: Boolean;
		procedure SetAllowOverride;
		procedure AddHandler(operation: TMemoryOperation; _start: Word; _end: Word = 0);
	end;

	TIMemoryHandler = class(TSnapshotable)
	public
		Debugged: Boolean;

		constructor Create(const RegisterName: AnsiString); override;

		function  PeekRAM (addr: Word): Byte; virtual;
		function  ReadRAM (addr: Word): Byte; virtual;
		procedure WriteRAM(addr: Word; value: Byte); virtual;

		procedure GetMemoryRanges(var ranges: TMemoryRanges); virtual;
		function  GetOpenBus: Byte; virtual;

		procedure WriteDebug(Stream: TFileStreamEx);
	end;

	TInternalRamHandler = class(TIMemoryHandler)
	const
		Mask = $7FF;
	private
		internalRam: TBytes;
	public
		function  PeekRAM(addr: Word): Byte; override;
		function  ReadRAM(addr: Word): Byte; override;
		procedure WriteRAM(addr: Word; value: Byte); override;

		procedure GetMemoryRanges(var ranges: TMemoryRanges); override;
		procedure SetInternalRam(var Ram: TBytes);
	end;

	TOpenBusHandler = class(TIMemoryHandler)
	private
		lastReadValue: Byte;
	public
		constructor Create(const RegisterName: AnsiString); override;

		function  PeekRAM(addr: Word): Byte; override;
		function  ReadRAM(addr: Word): Byte; override;
		procedure WriteRAM(addr: Word; value: Byte); override;

		procedure GetMemoryRanges(var ranges: TMemoryRanges); override;
		function  GetOpenBus: Byte; override;
		procedure SetOpenBus(value: Byte); inline;
	end;

	TMemoryHandlers = array of TIMemoryHandler;


implementation

// ============================================================================
// TMemoryRanges
// ============================================================================

constructor TMemoryRanges.Create;
begin
	inherited;
	ramReadAddresses  := TRamAddressList.Create;
	ramWriteAddresses := TRamAddressList.Create;
end;

destructor TMemoryRanges.Destroy;
begin
	ramReadAddresses.Free;
	ramWriteAddresses.Free;
	inherited;
end;

function TMemoryRanges.GetRAMReadAddresses: TRamAddressList;
begin
	Result := ramReadAddresses; // @
end;

function TMemoryRanges.GetRAMWriteAddresses: TRamAddressList;
begin
	Result := ramWriteAddresses; // @
end;

function TMemoryRanges.GetAllowOverride: Boolean;
begin
	Result := allowOverride;
end;

procedure TMemoryRanges.SetAllowOverride;
begin
	allowOverride := True;
end;

procedure TMemoryRanges.AddHandler(operation: TMemoryOperation; _start: Word; _end: Word = 0);
var
	i: Word;
begin
	if _end = 0 then
		_end := _start;

	if (operation = moRead) or (operation = moAny) then
		for i := _start to _end do
			ramReadAddresses.Add(i);
	
	if (operation = moWrite) or (operation = moAny) then
		for i := _start to _end do
			ramWriteAddresses.Add(i);
end;

// ============================================================================
// TIMemoryHandler
// ============================================================================

constructor TIMemoryHandler.Create(const RegisterName: AnsiString);
begin
	inherited Create(RegisterName);
end;

function TIMemoryHandler.PeekRAM(addr: Word): Byte;
begin
	Result := 0;
end;

function TIMemoryHandler.ReadRAM(addr: Word): Byte;
begin
	Result := 0;
end;

procedure TIMemoryHandler.WriteRAM(addr: Word; value: Byte);
begin
end;

procedure TIMemoryHandler.GetMemoryRanges(var ranges: TMemoryRanges);
begin
end;

function TIMemoryHandler.GetOpenBus: Byte;
begin
	Result := 0;
end;

procedure TIMemoryHandler.WriteDebug(Stream: TFileStreamEx);
var
	ranges: TMemoryRanges;
begin
	if Debugged then Exit;

	Stream.WriteString(Self.ClassName, True);

//	ranges := TMemoryRanges.Create;
//	Self.GetMemoryRanges(ranges);
//	ranges.Free;

	Debugged := True;
end;


// ============================================================================
// TInternalRamHandler
// ============================================================================

procedure TInternalRamHandler.SetInternalRam(var Ram: TBytes);
begin
	internalRam := Ram;
end;

procedure TInternalRamHandler.GetMemoryRanges(var ranges: TMemoryRanges);
begin
	ranges.SetAllowOverride;
	ranges.AddHandler(moAny, 0, $1FFF);
end;

function TInternalRamHandler.PeekRAM(addr: Word): Byte;
begin
	Result := internalRam[addr and Mask];
end;

function TInternalRamHandler.ReadRAM(addr: Word): Byte;
begin
	Result := internalRam[addr and Mask];
end;

procedure TInternalRamHandler.WriteRAM(addr: Word; value: Byte);
begin
	internalRam[addr and Mask] := value;
end;

// ============================================================================
// TOpenBusHandler
// ============================================================================

constructor TOpenBusHandler.Create(const RegisterName: AnsiString);
begin
	inherited Create(RegisterName);
	lastReadValue := 0;
end;

function TOpenBusHandler.PeekRAM(addr: Word): Byte;
begin
	Result := addr >> 8; // Fake open bus for debugger
end;

function TOpenBusHandler.ReadRAM(addr: Word): Byte;
begin
	Result := lastReadValue;
end;

procedure TOpenBusHandler.WriteRAM(addr: Word; value: Byte);
begin
end;

procedure TOpenBusHandler.GetMemoryRanges(var ranges: TMemoryRanges);
begin
end;

function TOpenBusHandler.GetOpenBus: Byte;
begin
	Result := lastReadValue;
end;

procedure TOpenBusHandler.SetOpenBus(value: Byte);
begin
	lastReadValue := value;
end;

end.
