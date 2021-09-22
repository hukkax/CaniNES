unit NES.Mapper_037;

// Mapper 037: Super Mario Bros. + Tetris + Nintendo World Cup

interface

uses
	NES.Types, NES.Cartridge,
	NES.Mapper, NES.Mapper_004;

type
	TMapper_037 = class(TMapper_004)
	private
		selectedBlock: Byte;
	protected
		function  RegisterStartAddress: Word; override;
		function  RegisterEndAddress:   Word; override;

		procedure SelectCHRPage(slot, page: Word; memoryType: TChrMemoryType = ChrDefault); override;
		procedure SelectPRGPage(slot: Word; page: Int32; memoryType: TPrgMemoryType = TPrgMemoryType.PrgRom); override;

		procedure WriteRegister(addr: Word;  value: Byte); override;
	public
		procedure Reset(SoftReset: Boolean); override;

		constructor Create(cartridge: TCartridge); override;
	end;


implementation


{ TMapper_037 }

function TMapper_037.RegisterStartAddress: Word; begin Result := $6000; end;
function TMapper_037.RegisterEndAddress:   Word; begin Result := $FFFF; end;

constructor TMapper_037.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(8, @selectedBlock);
end;

procedure TMapper_037.Reset(SoftReset: Boolean);
begin
	selectedBlock := 0;
	UpdateState;
end;

procedure TMapper_037.SelectCHRPage(slot, page: Word; memoryType: TChrMemoryType);
begin
	if selectedBlock >= 4 then
		page := page or $80;

	inherited;
end;

procedure TMapper_037.SelectPRGPage(slot: Word; page: Int32; memoryType: TPrgMemoryType);
begin
	case selectedBlock of
		0..2: page := (page and $07);
		3:    page := (page and $07) or $08;
		7:    page := (page and $07) or $20;
		else  page := (page and $0F) or $10;
	end;

	inherited;
end;

procedure TMapper_037.WriteRegister(addr: Word; value: Byte);
begin
	if addr < $8000 then
	begin
		if CanWriteToWorkRam then
		begin
			selectedBlock := value and $07;
			UpdateState;
		end;
	end
	else
		inherited;
end;

end.

