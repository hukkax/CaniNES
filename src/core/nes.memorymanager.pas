unit NES.MemoryManager;

interface

{$MODE DELPHI}
{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}

uses
	Types, Classes, SysUtils,
	NES.Types, NES.SaveState,
	NES.MemoryHandler, NES.Mapper;

type
	TMemoryManager = class(TSnapshotable)
	const
		InternalRAMSize = $800;
		RAMSize  = $10000;
	private
		mapper: TMapper;

		InternalRAM: TBytes; //array [0..InternalRAMSize-1] of Byte;

		OpenBusHandler: TOpenBusHandler;
		InternalRamHandler: TInternalRamHandler;

		ramReadHandlers, ramWriteHandlers: TMemoryHandlers; //IMemoryHandler** 

		procedure InitializeMemoryHandlers(var memoryHandlers: TMemoryHandlers;
			const handler: TIMemoryHandler; addresses: TRamAddressList; allowOverride: Boolean);
	public
		constructor Create; //(const AConsole: TConsole);
		destructor  Destroy; override;

		procedure SetMapper(const AMapper: TMapper);

		procedure Reset(softReset: Boolean);
		procedure RegisterIODevice(const handler: TIMemoryHandler);
		procedure RegisterWriteHandler(const handler: TIMemoryHandler; _start, _end: Cardinal);
		procedure UnregisterIODevice(const handler: TIMemoryHandler);

		function  DebugRead(addr: Word; disableSideEffects: Boolean = True): Byte;
		function  DebugReadWord(addr: Word): Word;
		procedure DebugWrite(addr: Word; value:Byte; disableSideEffects: Boolean = True);

		function  GetInternalRAM: PByte;

		function  Read(addr: Word; operationType: TMemoryOperationType = memopRead): Byte;
		procedure Write(addr: Word; value: Byte; operationType: TMemoryOperationType);

		//function  ToAbsolutePrgAddress(ramAddr: Word): Cardinal;
		function  GetOpenBus(mask: Byte = $FF): Byte;
		function  GetInternalOpenBus(mask: Byte = $FF): Byte;

		procedure LoadSnapshot; override;
		procedure SaveSnapshot; override;
	end;

var
	MemoryManager: TMemoryManager;

implementation

uses
	Basement.Util,
	NES.Console;


constructor TMemoryManager.Create; //(const AConsole: TConsole);
var
	i: Int32;
begin
	inherited Create;

	//console := AConsole;
	MemoryManager := Self;

	SetLength(InternalRAM, InternalRAMSize);

	internalRamHandler := TInternalRamHandler.Create('RAMH');
	internalRamHandler.SetInternalRam(internalRAM);
	openBusHandler := TOpenBusHandler.Create('OBUS');

	SetLength(ramReadHandlers,  RAMSize);
	SetLength(ramWriteHandlers, RAMSize);
	for i := 0 to RAMSize-1 do
	begin
		ramReadHandlers[i]  := openBusHandler;
		ramWriteHandlers[i] := openBusHandler;
	end;

	RegisterIODevice(internalRamHandler);
end;

destructor TMemoryManager.Destroy;
begin
	internalRamHandler.Free;
	openBusHandler.Free;
	inherited;
end;

procedure TMemoryManager.SetMapper(const AMapper: TMapper);
begin
	mapper := AMapper;
end;

procedure TMemoryManager.Reset(softReset: Boolean);
begin
	if not softReset then
		Console.InitializeRam(@InternalRAM[0], InternalRAMSize);
	if Mapper <> nil then
		Mapper.Reset(softReset);
end;

procedure TMemoryManager.InitializeMemoryHandlers(var memoryHandlers: TMemoryHandlers;
	const handler: TIMemoryHandler; addresses: TRamAddressList; allowOverride: Boolean);
var
	address: Word;
begin	
	for address in addresses do
	begin		
		//if(!allowOverride && memoryHandlers[address] != &_openBusHandler && memoryHandlers[address] != handler) 
		//	throw std::runtime_error("Not supported");
		memoryHandlers[address] := handler;
	end;
end;

procedure TMemoryManager.RegisterIODevice(const handler: TIMemoryHandler);
var
	ranges: TMemoryRanges;
begin
	if handler is TIMemoryHandler then
	begin
		ranges := TMemoryRanges.Create;
		handler.GetMemoryRanges(ranges);
		InitializeMemoryHandlers(ramReadHandlers,  handler, ranges.GetRAMReadAddresses,  ranges.GetAllowOverride);
		InitializeMemoryHandlers(ramWriteHandlers, handler, ranges.GetRAMWriteAddresses, ranges.GetAllowOverride);
		ranges.Free;
	end
	else
		Log('Attempt to RegisterIODevice an invalid handler: ' + handler.ClassName);
end;

procedure TMemoryManager.RegisterWriteHandler(const handler: TIMemoryHandler; _start, _end: Cardinal);
var
	i: Integer;
begin
	// Log('MemoryManager.RegisterWriteHandler: ' + handler.ClassName);
	for i := _start to _end do
		ramWriteHandlers[i] := handler;
end;

procedure TMemoryManager.UnregisterIODevice(const handler: TIMemoryHandler);
var
	ranges: TMemoryRanges;
	address: Word;
begin	
	ranges := TMemoryRanges.Create;

	handler.GetMemoryRanges(ranges);
	for address in ranges.GetRAMReadAddresses do
		ramReadHandlers[address] := @openBusHandler;
	for address in ranges.GetRAMWriteAddresses do
		ramWriteHandlers[address] := @openBusHandler;

	ranges.Free;
end;

function TMemoryManager.GetInternalRAM: PByte;
begin
	Result := @InternalRAM[0];
end;

function TMemoryManager.DebugRead(addr: Word; disableSideEffects: Boolean = True): Byte;
var
	handler: TIMemoryHandler;
begin	
	if addr <= $1FFF then
		Result := ramReadHandlers[addr].ReadRAM(addr)
	else
	begin
		handler := ramReadHandlers[addr];
		if handler <> nil then
		begin			
			if disableSideEffects then
				Result := handler.PeekRAM(addr)
			else 
				Result := handler.ReadRAM(addr);
		end
		else 
			Result := addr >> 8; // Fake open bus
	end;
	Console.CheatManager.ApplyCodes(addr, Result);
end;

function TMemoryManager.DebugReadWord(addr: Word): Word;
begin
	Result := DebugRead(addr) or Word(DebugRead(addr + 1) << 8);
end;

procedure TMemoryManager.DebugWrite(addr: Word; value:Byte; disableSideEffects: Boolean = True);
var
	handler: TIMemoryHandler;
begin
	if addr <= $1FFF then
		ramWriteHandlers[addr].WriteRAM(addr, value)
	else 
	begin		
		handler := ramReadHandlers[addr];
		if handler <> nil then
		begin
			if disableSideEffects then
			begin
				// Only allow writes to prg/chr ram/rom (e.g not ppu, apu, mapper registers, etc.)
				if handler = mapper then
					TMapper(handler).DebugWriteRAM(addr, value);
			end
			else 
				handler.WriteRAM(addr, value);
		end;
	end;
end;

function TMemoryManager.Read(addr: Word; operationType: TMemoryOperationType = memopRead): Byte;
begin	
	Result := ramReadHandlers[addr].ReadRAM(addr);
	Console.CheatManager.ApplyCodes(addr, Result);
	//console.DebugProcessRamOperation(operationType, addr, Result);
	openBusHandler.SetOpenBus(Result, addr = $4015);
end;

procedure TMemoryManager.Write(addr: Word; value: Byte; operationType: TMemoryOperationType);
begin	
//	if console.DebugProcessRamOperation(operationType, addr, value) then
//	if (emu.ProcessMemoryWrite(addr, value, operationType)) then
	ramWriteHandlers[addr].WriteRAM(addr, value);
	openBusHandler.SetOpenBus(value, False);
end;

{function TMemoryManager.ToAbsolutePrgAddress(ramAddr: Word): Cardinal;
begin	
	Result := mapper.ToAbsoluteAddress(ramAddr);
end;}

(*
procedure TMemoryManager.StreamState(Saving: Boolean);
var
	i: Integer;
//internalRamHandler
begin
	if Saving then
	begin
		Stream.WriteString('MEMM', True);

		Stream.Write32(Length(InternalRAM));
		Stream.WriteBuffer(InternalRAM[0], Length(InternalRAM));

{
	for i := 0 to Length(ramReadHandlers)-1 do
		ramReadHandlers[i].Debugged := False;
	for i := 0 to Length(ramWriteHandlers)-1 do
		ramWriteHandlers[i].Debugged := False;

	Stream.WriteString('READHANDLERS');
	for i := 0 to Length(ramReadHandlers)-1 do
		ramReadHandlers[i].WriteDebug(Stream);

	Stream.WriteString('WRITEHANDLERS');
	for i := 0 to Length(ramWriteHandlers)-1 do
		ramWriteHandlers[i].WriteDebug(Stream);
}

	end
	else
	begin
		Log(Stream.ReadString(True));

		SetLength(InternalRAM, Stream.Read32);
		Stream.ReadBuffer(InternalRAM[0], Length(InternalRAM));
	end;
end;
*)

function TMemoryManager.GetOpenBus(mask: Byte = $FF): Byte;
begin
	Result := openBusHandler.GetOpenBus and mask;
end;

function TMemoryManager.GetInternalOpenBus(mask: Byte = $FF): Byte;
begin
	Result := openBusHandler.GetInternalOpenBus and mask;
end;

procedure TMemoryManager.LoadSnapshot;
begin
	inherited LoadSnapshot;

	StartSection('IRAM');
	Stream.Read(InternalRAM[0], InternalRAMSize);
end;

procedure TMemoryManager.SaveSnapshot;
begin
	inherited SaveSnapshot;

	StartSection('IRAM');
	Stream.WriteBuffer(InternalRAM[0], InternalRAMSize);
end;

end.

