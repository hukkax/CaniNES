unit NES.Mapper_006;

// Mapper 006: FrontFareast

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper;

type
	TMapper_006 = class(TMapper)
	private
		irqCounter: Word;
		irqEnabled,
		ffeAltMode: Boolean;
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;
		function  GetChrRamSize: Cardinal; override;

		function  RegisterStartAddress: Word; override;
		function  RegisterEndAddress: Word; override;

		procedure WriteRegister(addr: Word;  value: Byte); override;

		procedure InitMapper; override;
	public
		procedure ProcessCpuClock; override;

		constructor Create(cartridge: TCartridge); override;
	end;


implementation

uses
	NES.CPU;

{ TMapper_006 }

constructor TMapper_006.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(16, @irqCounter);
	RegisterProperty(1,  @irqEnabled);
	RegisterProperty(1,  @ffeAltMode);
end;

function TMapper_006.GetPRGPageSize: Word;
begin
	Result := $2000;
end;

function TMapper_006.GetCHRPageSize: Word;
begin
	Result := $400;
end;

function TMapper_006.GetChrRamSize: Cardinal;
begin
	Result := $8000;
end;

function TMapper_006.RegisterStartAddress: Word;
begin
	Result := $42FE;
end;

function TMapper_006.RegisterEndAddress: Word;
begin
	Result := $4517;
end;

procedure TMapper_006.InitMapper;
begin
	inherited;

	irqCounter := 0;
	irqEnabled := False;
	ffeAltMode := True;

	case romInfo.MapperID of

		6:	begin
				AddRegisterRange($8000, $FFFF, moWrite);
				SelectPrgPage2x(0, 0);
				SelectPrgPage2x(1, 14);
			end;

		8:	begin
				AddRegisterRange($8000, $FFFF, moWrite);
				SelectPrgPage4x(0, 0);
			end;

		17:		SelectPrgPage4x(0, -4);

	end;
end;

procedure TMapper_006.ProcessCpuClock;
begin
	if irqEnabled then
	begin
		Inc(irqCounter);
		if irqCounter = 0 then
		begin
			CPUSetIRQSource(irqExternal);
			irqEnabled := False;
		end;
	end;
end;

procedure TMapper_006.WriteRegister(addr: Word; value: Byte);
begin
//	inherited WriteRegister(addr, value);

	case addr of

			$42FE:
			begin
				ffeAltMode := (value and $80) = $00;
				ApplyMirroringType(((value >> 4) and $01) = 0,
					MIRROR_SCREENAONLY, MIRROR_SCREENBONLY);
			end;

			$42FF:
				ApplyMirroringType(((value >> 4) and $01) = 0,
					MIRROR_VERTICAL, MIRROR_HORIZONTAL);

			$4501:
			begin
				irqEnabled := False;
				CPUClearIrqSource(irqExternal);
			end;

			$4502:
			begin
				irqCounter := (irqCounter and $FF00) or value;
				CPUClearIrqSource(irqExternal);
			end;

			$4503:
			begin
				irqCounter := (irqCounter and $00FF) or (value << 8);
				irqEnabled := True;
				CPUClearIrqSource(irqExternal);
			end

			else

				if romInfo.MapperID = 6 then
				begin
					if addr >= $8000 then
					begin
						if (HasChrRam) or (ffeAltMode) then
						begin
							SelectPrgPage2x(0, (value and $FC) >> 1);
							value := value and $03;
						end;
						SelectChrPage8x(0, value << 3);
					end;
				end
				else
				if romInfo.MapperID = 8 then
				begin
					if addr >= $8000 then
					begin
						SelectPrgPage2x(0, (value and $F8) >> 2);
						SelectChrPage8x(0, (value and $07) << 3);
					end;
				end
				else
				case addr of
					$4504..$4507:	SelectPRGPage(addr - $4504, value);
					$4510..$4517:	SelectCHRPage(addr - $4510, value);
				end;

		end;
end;

end.
