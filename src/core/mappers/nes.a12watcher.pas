unit NES.A12Watcher;

interface

uses
	NES.SaveState;

type
	TA12StateChange = ( scNone = 0, scRise = 1, scFall = 2 );

	TA12Watcher = class(TSnapshotable)
	private
		lastCycle,
		cyclesDown: Cardinal;
	public
		minDelay: Byte;

		function UpdateVramAddress(addr: Word; frameCycle: Cardinal): TA12StateChange;

		constructor Create(MinimumDelay: Byte); overload;
	end;

implementation

uses
	SysUtils, NES.CPU;


constructor TA12Watcher.Create(MinimumDelay: Byte);
begin
	inherited Create('A12W');

	RegisterProperty(32, @lastCycle);
	RegisterProperty(32, @cyclesDown);
	RegisterProperty(8,  @minDelay);

	minDelay := MinimumDelay;
	lastCycle  := 0;
	cyclesDown := 0;
end;

function TA12Watcher.UpdateVramAddress(addr: Word; frameCycle: Cardinal): TA12StateChange;
begin
	Result := scNone;

	if cyclesDown > 0 then
	begin
		if lastCycle > frameCycle then // We changed frames
			{$R-}Inc(cyclesDown, (89342 - lastCycle) + frameCycle)
		else
			{$R-}Inc(cyclesDown, frameCycle - lastCycle);
	end;

	if (addr and $1000) = 0 then
	begin
		if cyclesDown = 0 then
		begin
			cyclesDown := 1;
			Result := scFall;
		end;
	end
	else
	begin
		if cyclesDown > minDelay then
			Result := scRise;
		cyclesDown := 0;
	end;
	lastCycle := frameCycle;
end;

end.

