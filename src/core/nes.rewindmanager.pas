unit NES.RewindManager;

{$mode delphi}

interface

uses
	Classes, SysUtils,
	NES.SaveState;

type
	TRewindManager = class
	private
		Stream:        TMemoryStream;

		Initialized,
		BufferFilled:    Boolean;
		FramesLeft,
		FrameCounter:    Int64;
		CallCounter:     Integer;
		FramesAllocated: Cardinal;

		States: array of TBytes;
	public
		Snapshoting: Boolean;

		procedure   Initialize;
		procedure   Start;
		procedure   Stop;
		procedure   Update;

		constructor Create;
		destructor  Destroy; override;
	end;

implementation

uses
	Basement.Util, Math,
	NES.Config, NES.Console;

{ TRewindManager }

constructor TRewindManager.Create;
begin
	inherited Create;

	Stream := TMemoryStream.Create;

	Initialized := False;
	Snapshoting := False;
end;

destructor TRewindManager.Destroy;
begin
	Stream.Free;

	inherited Destroy;
end;

procedure TRewindManager.Initialize;
begin
	Console.Rewind := False;
	BufferFilled := False;
	CallCounter := 0;
	FrameCounter := 0;
	FramesLeft := 0;
	FramesAllocated := Configuration.Emulator.RewindBufferSize * 60
		div Configuration.Emulator.RewindSnapshotInterval;

	// create a snapshot to determine how much memory we need for each snapshot
	Stream.Clear;
	Console.SaveStateManager.SaveState(Stream);
	SetLength(States, FramesAllocated+1);

	Initialized := True;
	Stop;
end;

procedure TRewindManager.Start;
begin
	if (Initialized) and (not Console.Rewind) then
	begin
		Console.Rewind := True;
	end;
end;

procedure TRewindManager.Stop;
begin
	if (Initialized) and (Console.Rewind) then
	begin
		Console.Rewind := False;
		CallCounter := 0;
	end;
end;

procedure TRewindManager.Update;
var
	L: Int64;
begin
	if (not Initialized) or (Configuration.Emulator.RewindBufferSize < 1) or
		(NES_PPU.frameCount < 4) then Exit;

	Stream.Clear;

	if Console.Rewind then
	begin
		Dec(FrameCounter);
		Dec(FramesLeft);

		if (FramesLeft < 0) then
		begin
			Stop;
			FramesLeft := 0;
			if FrameCounter < 0 then
				FrameCounter := 0;
			Exit;
		end
		else
		if FrameCounter < 0 then
		begin
			if BufferFilled then
				FrameCounter := FramesAllocated
			else
			begin
				FrameCounter := 0;
				Stop;
				Exit;
			end;
		end;

		Snapshoting := True;

		L := Length(States[FrameCounter]);
		if L > 0 then
		begin
			Stream.WriteBuffer(States[FrameCounter][0], L);
			Stream.Position := 0;
			Console.SaveStateManager.LoadState(Stream);
		end;
	end
	else
	begin
		Dec(CallCounter);
		if CallCounter > 0 then Exit;
		CallCounter := Configuration.Emulator.RewindSnapshotInterval;

		Snapshoting := True;

		if not Console.SaveStateManager.SaveState(Stream) then
		begin
			Log('Snapshot failed!');
			Snapshoting := False;
			Exit;
		end;

		L := Stream.Position;
		Stream.Position := 0;

		if L <= 0 then
			Log('Snapshot empty!')
		else
		begin
			SetLength(States[FrameCounter], L);
			Stream.Read(States[FrameCounter][0], L);
			FramesLeft := Min(FramesLeft+1, FramesAllocated);

			Inc(FrameCounter);
			if FrameCounter > FramesAllocated then
			begin
				FrameCounter := 0;
				BufferFilled := True;
			end;
		end;
	end;

	Snapshoting := False;
end;

end.

