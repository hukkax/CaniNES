unit NES.Mapper_050;

// Mapper 050: N-32 conversion of SMB2(J)

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper;

type
	TMapper_050 = class(TMapper)
	protected
		irqCounter: Word;
		irqEnabled: Boolean;

		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;
		function  RegisterStartAddress: Word; override;
		function  RegisterEndAddress: Word; override;

		procedure InitMapper; override;
		procedure WriteRegister(addr: Word;  value: Byte); override;
	public
		procedure ProcessCpuClock; override;

		constructor Create(cartridge: TCartridge); override;
	end;


implementation

uses
	NES.CPU;

{ TMapper_050 }

function TMapper_050.GetPRGPageSize: Word;
begin
	Result := $2000;
end;

function TMapper_050.GetCHRPageSize: Word;
begin
	Result := $2000;
end;

function TMapper_050.RegisterStartAddress: Word;
begin
	Result := $4020;
end;

function TMapper_050.RegisterEndAddress: Word;
begin
	Result := $5FFF;
end;

constructor TMapper_050.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(16, @irqCounter);
	RegisterProperty(1 , @irqEnabled);
end;

procedure TMapper_050.InitMapper;
begin
	irqCounter := 0;
	irqEnabled := False;
	SetCpuMemoryMapping($6000, $7FFF, $0F, TPrgMemoryType.PrgRom);
	SelectPRGPage(0, $08);
	SelectPRGPage(1, $09);
	SelectPRGPage(3, $0B);
	SelectCHRPage(0, $00);
end;

procedure TMapper_050.ProcessCpuClock;
begin
	if irqEnabled then
	begin
		{$R-}Inc(irqCounter);
		if irqCounter = $1000 then
		begin
			CPUSetIRQSource(irqExternal);
			irqEnabled := False;
		end;
	end;
end;

procedure TMapper_050.WriteRegister(addr: Word; value: Byte);
begin
	case (addr and $4120) of

		$4020:
			SelectPRGPage(2, (value and $08) or
				((value and $01) shl 2) or ((value and $06) shr 1));

		$4120:
			if (value and $01) <> 0 then
				irqEnabled := True
			else
			begin
				CPUClearIRQSource(irqExternal);
				irqCounter := 0;
				irqEnabled := False;
			end;
	end;
end;

end.
