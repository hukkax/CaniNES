unit FutureComposer.Paula;

interface

uses
	FutureComposer.Util;

type
{
	volatile bool active;

	const int8_t *data, *newData;
	int32_t length, newLength, pos;
	float fVolume, fDelta, fPhase;
	float fLastDelta, fLastPhase;
}

	TPaulaVoice = class
	private
		f_OutputFreq: Single;
	public
		NewData: PArrayOfShortInt;
		NewLength: Cardinal;
		Volume: Single;

		Data: PArrayOfShortInt;
		Length: Cardinal;
		Position: Cardinal;

		Active:  Boolean;

		PanL, PanR: Single;
		Delta, LastDelta: Single;
		Phase, LastPhase: Single;

		constructor Create(const OutputFreq: Word);

		procedure Reset;
		procedure StopDMA;
		procedure StartDMA;
		procedure SetData(const src: PArrayOfShortInt);
		procedure SetLength(const len: Word);
		procedure SetPeriod(period: Word);
		procedure SetVolume(vol: Word);
	end;


implementation


constructor TPaulaVoice.Create(const OutputFreq: Word);
begin
	inherited Create;
	f_OutputFreq := OutputFreq;
	Reset;
end;

procedure TPaulaVoice.Reset;
begin
	Active := False;
	Data := nil;
	NewData := nil;
	Length := 0;
	NewLength := 0;
	Position := 0;
	Volume := 0;
	Delta := 0;
	Phase := 0;
	LastDelta := 0;
	PanL := 0;
	PanR := 0;
end;

procedure TPaulaVoice.StopDMA;
begin
	Active := False;
end;

procedure TPaulaVoice.StartDMA;
begin
	Phase := 0.0;
	Position := 0;
	Data := NewData;
	Length := NewLength;
	Active := True;
end;

procedure TPaulaVoice.SetPeriod(period: Word);
begin
	// This is what really happens on Paula on a real Amiga
	// on normal video modes. Tested and confirmed!
	if period = 0 then
		Delta := 0.0
	else
	begin
		if period < 113 then
			period := 113;
		Delta := (3546895.0 / period) / f_outputFreq;
	end;
	if LastDelta = 0.0 then
		LastDelta := Delta;
end;

procedure TPaulaVoice.SetVolume(vol: Word);
begin
	{if (vol and (1 shl 6)) <> 0 then
		vol := $0040
	else
		vol := vol and $003F;}
	vol := vol and 127;
	if vol > 64 then vol := 64;
	Volume := vol * (1.0 / 64.0);
end;

procedure TPaulaVoice.SetLength(const len: Word);
begin
	NewLength := len * 2;
end;

procedure TPaulaVoice.SetData(const src: PArrayOfShortInt);
begin
	NewData := src;
	if src = nil then
		Active := False;
end;


end.

