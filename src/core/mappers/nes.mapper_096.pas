unit NES.Mapper_096;

// Mapper 096: Oeka Kids

interface

uses
	NES.Mapper, NES.Cartridge;

type
	TMapper_096 = class(TMapper)
	private
		outerChrBank,
		innerChrBank: Byte;
		lastAddress:  Word;
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;
		function  HasBusConflicts: Boolean; override;

		procedure UpdateChrBanks;

		procedure WriteRegister(addr: Word;  value: Byte); override;
		procedure InitMapper; override;
	public
		procedure NotifyVRAMAddressChange(addr: Word); override;

		constructor Create(cartridge: TCartridge); override;
	end;

implementation

uses
	NES.Types;

{ TMapper_096 }

function TMapper_096.GetPRGPageSize: Word; begin Result := $8000; end;
function TMapper_096.GetCHRPageSize: Word; begin Result := $1000; end;
function TMapper_096.HasBusConflicts: Boolean; begin Result := True; end;

constructor TMapper_096.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(8, @outerChrBank);
	RegisterProperty(8, @innerChrBank);
	RegisterProperty(16,@lastAddress);
end;

procedure TMapper_096.InitMapper;
begin
	outerChrBank := 0;
	innerChrBank := 0;
	lastAddress  := 0;

	SelectPRGPage(0, 0);
end;

procedure TMapper_096.NotifyVRAMAddressChange(addr: Word);
begin
	if ((lastAddress and $3000) <> $2000) and ((addr and $3000) = $2000) then
	begin
		innerChrBank := (addr shr 8) and $03;
		UpdateChrBanks;
	end;
	lastAddress := addr;
end;

procedure TMapper_096.UpdateChrBanks;
begin
	SelectCHRPage(0, outerChrBank or innerChrBank);
	SelectCHRPage(1, outerChrBank or $03);
end;

procedure TMapper_096.WriteRegister(addr: Word; value: Byte);
begin
	SelectPRGPage(0, value and $03);
	outerChrBank := value and $04;
	UpdateChrBanks;
end;

end.
