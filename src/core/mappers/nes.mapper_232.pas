unit NES.Mapper_232;

// BF9096

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper;

type
	TMapper_232 = class(TMapper)
	private
		prgBlock,
		prgPage: Byte;
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;

		procedure WriteRegister(addr: Word;  value: Byte); override;
		procedure InitMapper; override;
	public
		constructor Create(cartridge: TCartridge); override;
	end;


implementation


{ TMapper_232 }

function TMapper_232.GetPRGPageSize: Word;
begin
	Result := $4000;
end;

function TMapper_232.GetCHRPageSize: Word;
begin
	Result := $2000;
end;

constructor TMapper_232.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(8, @prgPage);
	RegisterProperty(8, @prgBlock);
end;

procedure TMapper_232.InitMapper;
begin
	prgPage := 0;
	prgBlock := 0;

	SelectPRGPage(0, 0);
	SelectPRGPage(1, 3);
	SelectCHRPage(0, 0);
end;

procedure TMapper_232.WriteRegister(addr: Word; value: Byte);
begin
	if addr >= $C000 then
		prgPage := value and 3
	else
	if addr < $C000 then
	begin

		if romInfo.SubMapperID = 1 then
		begin
			// "232: 1 Aladdin Deck Enhancer"
			// "Aladdin Deck Enhancer variation.Swap the bits of the outer bank number."
			// But this seems to match the Pegasus 4-in-1 behavior?  Wiki wrong?
			prgBlock := ((value shr 4) and 1) or ((value shr 2) and 2);
		end
		else
			prgBlock := (value shr 3) and 3;
	end;

	SelectPRGPage(0, (prgBlock shl 2) or prgPage);
	SelectPRGPage(1, (prgBlock shl 2) or 3);
end;

end.
