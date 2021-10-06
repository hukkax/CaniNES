unit NES.Mapper_073;

// Mapper 073: VRC3

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper;

type
	TMapper_VRC3 = class(TMapper)
	private
		irqEnableOnAck,
		irqEnabled,
		smallCounter: Boolean;
		irqReload,
		irqCounter:   Word;
	protected
		function GetPRGPageSize: Word; override;
		function GetCHRPageSize: Word; override;

		procedure InitMapper; override;

		procedure WriteRegister(address: Word; value: Byte); override;

	public
		procedure ProcessCpuClock; override;

		constructor Create(cartridge: TCartridge); override;
	end;

implementation

uses
	SysUtils, Basement.Util,
	NES.Config, NES.CPU;

// ============================================================================
// TMapper_VRC3
// ============================================================================

function TMapper_VRC3.GetPRGPageSize: Word;
begin
	Result := $4000;
end;

function TMapper_VRC3.GetCHRPageSize: Word;
begin
	Result := $2000;
end;

constructor TMapper_VRC3.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(1, @irqEnableOnAck);
	RegisterProperty(1, @smallCounter);
	RegisterProperty(1, @irqEnabled);

	RegisterProperty(16, @irqCounter);
	RegisterProperty(16, @irqReload);
end;

procedure TMapper_VRC3.InitMapper;
begin
	SelectPRGPage(1, -1);
	SelectCHRPage(0,  0);
end;

procedure TMapper_VRC3.ProcessCpuClock;
var
	sCounter: Byte;
begin
	if not irqEnabled then Exit;

	if smallCounter then
	begin
		sCounter := irqCounter and $FF;
		{$R-}Inc(sCounter);
		if not smallCounter then
		begin
			sCounter := irqReload and $FF;
			CPUSetIRQSource(irqExternal);
		end;
		irqCounter := (irqCounter and $FF00) or sCounter;
	end
	else
	begin
		{$R-}Inc(irqCounter);
		if irqCounter = 0 then
		begin
			irqCounter := irqReload;
			CPUSetIRQSource(irqExternal);
		end;
	end;
end;

procedure TMapper_VRC3.WriteRegister(address: Word; value: Byte);
begin
	case (address and $F000) of
		$8000: irqReload := (irqReload and $FFF0) or (value and $0F);
		$9000: irqReload := (irqReload and $FF0F) or ((value and $0F) shl 4);
		$A000: irqReload := (irqReload and $F0FF) or ((value and $0F) shl 8);
		$B000: irqReload := (irqReload and $0FFF) or ((value and $0F) shl 12);
		$C000:
		begin
			irqEnabled := (value and $02) = $02;
			if irqEnabled then
				irqCounter := irqReload;
			smallCounter   := (value and $04) = $04;
			irqEnableOnAck := (value and $01) = $01;
			CPUClearIrqSource(irqExternal);
		end;
		$D000:
		begin
			CPUClearIrqSource(irqExternal);
			irqEnabled := irqEnableOnAck;
		end;
		$F000: SelectPRGPage(0, value and $07);
	end;
end;


end.

