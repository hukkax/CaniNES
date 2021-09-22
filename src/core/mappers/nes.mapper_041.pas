unit NES.Mapper_041;

// Caltron41

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper;

type
	TMapper_041 = class(TMapper)
	private
		prgBank,
		chrBank: Byte;
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;
		function  RegisterStartAddress: Word; override;
		function  RegisterEndAddress: Word; override;

		procedure WriteRegister(addr: Word;  value: Byte); override;
		procedure InitMapper; override;
	public
		procedure Reset(SoftReset: Boolean); override;

		constructor Create(cartridge: TCartridge); override;
	end;


implementation


{ TMapper_041 }

function TMapper_041.GetPRGPageSize: Word;
begin
	Result := $8000;
end;

function TMapper_041.GetCHRPageSize: Word;
begin
	Result := $2000;
end;

function TMapper_041.RegisterStartAddress: Word;
begin
	Result := $8000;
end;

function TMapper_041.RegisterEndAddress: Word;
begin
	Result := $FFFF;
end;

constructor TMapper_041.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(8, @prgBank);
	RegisterProperty(8, @chrBank);
end;

procedure TMapper_041.InitMapper;
begin
	AddRegisterRange($6000, $67FF, moWrite);
end;

procedure TMapper_041.Reset(SoftReset: Boolean);
begin
	inherited Reset(SoftReset);

	prgBank := 0;
	chrBank := 0;

	WriteRegister($6000, 0);
	WriteRegister($8000, 0);
end;

procedure TMapper_041.WriteRegister(addr: Word; value: Byte);
begin
	if addr <= $67FF then
	begin
		prgBank := addr and $07;
		chrBank := (chrBank and $03) or ((addr shr 1) and $0C);
		SelectPRGPage(0, prgBank);
		SelectCHRPage(0, chrBank);
		ApplyMirroringType(addr and $20, MIRROR_HORIZONTAL, MIRROR_VERTICAL);
	end
	else
	begin
		// "Note that the Inner CHR Bank Select only can be written while the PRG ROM bank is 4, 5, 6, or 7"
		if prgBank >= 4 then
		begin
			chrBank := (chrBank and $0C) or (value and $03);
			SelectCHRPage(0, chrBank);
		end;
	end;
end;

end.
