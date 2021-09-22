unit NES.Mapper_059;

// Mapper 059: UnlD1038

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper;

type
	TMapper_059 = class(TMapper)
	private
		returnDipSwitch: Boolean;
	protected
		function  GetDipSwitchCount: Cardinal; override;

		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;

		function  ReadRegister(addr: Word): Byte; override;
		procedure WriteRegister(addr: Word;  value: Byte); override;

		procedure InitMapper; override;
	public
		constructor Create(cartridge: TCartridge); override;
	end;


implementation


{ TMapper_059 }

function TMapper_059.GetDipSwitchCount: Cardinal;
begin
	Result := 2;
end;

function TMapper_059.GetPRGPageSize: Word;
begin
	Result := $4000;
end;

function TMapper_059.GetCHRPageSize: Word;
begin
	Result := $2000;
end;

constructor TMapper_059.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(1, @returnDipSwitch);
end;

procedure TMapper_059.InitMapper;
begin
	inherited InitMapper;

	returnDipSwitch := False;
	WriteRegister($8000, 0);
end;

function TMapper_059.ReadRegister(addr: Word): Byte;
begin
	if returnDipSwitch then
		Result := GetDipSwitches
	else
		Result := InternalReadRam(addr);
end;

procedure TMapper_059.WriteRegister(addr: Word; value: Byte);
begin
	if (addr and $80) <> 0 then
	begin
		SelectPRGPage(0, (addr and $70) >> 4);
		SelectPRGPage(1, (addr and $70) >> 4);
	end
	else
		SelectPrgPage2x(0, (addr and $60) >> 4);

	SelectCHRPage(0, addr and $07);
	ApplyMirroringType(addr and $08, MIRROR_HORIZONTAL, MIRROR_VERTICAL);
	returnDipSwitch := ((addr and $100) = $100);
end;

end.
