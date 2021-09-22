unit NES.VRC_IRQ;

interface

uses
	Classes, SysUtils,
	NES.SaveState;

type
	TVRCVariant = (
		VRC2a,  // Mapper 22
		VRC2b,  // 23
		VRC2c,  // 25
		VRC4a,  // 21
		VRC4b,  // 25
		VRC4c,  // 21
		VRC4d,  // 25
		VRC4e,  // 23
		VRC427, // 27
		VRC6a,  // 24
		VRC6b   // 26
	);

	TVrcIrq = class(TSnapshotable)
	private
		irqReloadValue,
		irqCounter: Byte;
		irqPrescalerCounter: Int16;

		irqEnabled,
		irqEnabledAfterAck,
		irqCycleMode: Boolean;
	public
		procedure Reset;
		procedure ProcessCpuClock;
		procedure SetReloadValue(value: Byte);
		procedure SetReloadValueNibble(value: Byte; highBits: Boolean);
		procedure SetControlValue(value: Byte);
		procedure AcknowledgeIrq;

		constructor Create; overload;
	end;

implementation

uses
	NES.CPU;

{ TVrcIrq }

constructor TVrcIrq.Create;
begin
	inherited Create('VRCIRQ');

	RegisterProperty(8,  @irqReloadValue);
	RegisterProperty(8,  @irqCounter);
	RegisterProperty(16, @irqPrescalerCounter);
	RegisterProperty(1,  @irqEnabled);
	RegisterProperty(1,  @irqEnabledAfterAck);
	RegisterProperty(1,  @irqCycleMode);
end;

procedure TVrcIrq.Reset;
begin
	irqPrescalerCounter := 0;
	irqReloadValue := 0;
	irqCounter := 0;
	irqEnabled := False;
	irqEnabledAfterAck := False;
	irqCycleMode := False;
end;

procedure TVrcIrq.ProcessCpuClock;
begin
	if not irqEnabled then Exit;

	Dec(irqPrescalerCounter, 3);

	if (irqCycleMode) or ((irqPrescalerCounter <= 0) and (not irqCycleMode)) then
	begin
		if irqCounter = $FF then
		begin
			irqCounter := irqReloadValue;
			CPUSetIrqSource(irqExternal);
		end
		else
			Inc(irqCounter);

		Inc(irqPrescalerCounter, 341);
	end;
end;

procedure TVrcIrq.SetReloadValue(value: Byte);
begin
	irqReloadValue := value;
end;

procedure TVrcIrq.SetReloadValueNibble(value: Byte; highBits: Boolean);
begin
	if highBits then
		irqReloadValue := (irqReloadValue and $0F) or ((value and $0F) shl 4)
	else
		irqReloadValue := (irqReloadValue and $F0) or  (value and $0F);
end;

procedure TVrcIrq.SetControlValue(value: Byte);
begin
	irqEnabledAfterAck := (value and 1) = 1;
	irqEnabled         := (value and 2) = 2;
	irqCycleMode       := (value and 4) = 4;

	if irqEnabled then
	begin
		irqCounter := irqReloadValue;
		irqPrescalerCounter := 341;
	end;

	CPUClearIrqSource(irqExternal);
end;

procedure TVrcIrq.AcknowledgeIrq;
begin
	irqEnabled := irqEnabledAfterAck;
	CPUClearIrqSource(irqExternal);
end;


end.

