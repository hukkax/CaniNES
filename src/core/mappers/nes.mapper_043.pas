unit NES.Mapper_043;

// Mapper 043: TONY-I/YS-612 (SMB2J)

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper,
	NES.Mapper_050;

type
	TMapper_043 = class(TMapper_050)
	private
		swap: Boolean;
		reg:  Byte;
	protected
		function  RegisterEndAddress: Word; override;

		procedure UpdateState;
		procedure InitMapper; override;
		procedure WriteRegister(addr: Word;  value: Byte); override;
	public
		constructor Create(cartridge: TCartridge); override;
	end;


implementation

uses
	Math, NES.CPU;

{ TMapper_043 }

function TMapper_043.RegisterEndAddress: Word;
begin
	Result := $FFFF;
end;

procedure TMapper_043.UpdateState;
begin
	SetCpuMemoryMapping($6000, $7FFF, IfThen(swap, 0, 2), TPrgMemoryType.PrgRom);
	SelectPRGPage(2, reg);
	SelectPRGPage(3, IfThen(swap, 8, 9));
end;

constructor TMapper_043.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(8 , @reg);
	RegisterProperty(1 , @swap);
end;

procedure TMapper_043.InitMapper;
begin
	irqCounter := 0;
	irqEnabled := False;
	swap := False;
	reg := 0;
	UpdateState;
	SetCpuMemoryMapping($5000, $5FFF, 8, TPrgMemoryType.PrgRom);
	SelectPRGPage(0, 1);
	SelectPRGPage(1, 0);
	SelectCHRPage(0, 0);
end;


procedure TMapper_043.WriteRegister(addr: Word; value: Byte);
const
	lut: array [0..7] of Byte = ( 4, 3, 5, 3, 6, 3, 7, 3 );
begin
	case (addr and $F1FF) of
		$4022: begin reg  := lut[value and $07];   UpdateState; end;
		$4120: begin swap := (value and $01) <> 0; UpdateState; end;
		$8122, $4122: begin
			irqEnabled := (value and $01) = $01;
			CPUClearIRQSource(irqExternal);
			irqCounter := 0;
		end;
	end;
end;

end.
