unit NES.Mapper_036;

// Mapper 036: TXC22000

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper,
	NES.TXCChip;

type
	TMapper_036 = class(TMapper)
	protected
		TXC: TTXCChip;

		chrBank: Byte;

		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;

		function  RegisterStartAddress: Word; override;
		function  RegisterEndAddress: Word; override;
		function  AllowRegisterRead: Boolean; override;

		function  ReadRegister(addr: Word): Byte; override;
		procedure WriteRegister(addr: Word;  value: Byte); override;

		procedure InitMapper; override;
		procedure UpdateState; virtual;
	public
		constructor Create(cartridge: TCartridge); override;
		destructor  Destroy; override;
	end;


implementation

uses
	NES.Console;

{ TMapper_036 }

function TMapper_036.GetPRGPageSize: Word;       begin Result := $8000; end;
function TMapper_036.GetCHRPageSize: Word;       begin Result := $2000; end;
function TMapper_036.RegisterStartAddress: Word; begin Result := $8000; end;
function TMapper_036.RegisterEndAddress: Word;   begin Result := $FFFF; end;
function TMapper_036.AllowRegisterRead: Boolean; begin Result := True;  end;

constructor TMapper_036.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(8, @chrBank);

	TXC := TTXCChip.Create(False);
end;

destructor TMapper_036.Destroy;
begin
	TXC.Free;

	inherited Destroy;
end;

procedure TMapper_036.InitMapper;
begin
	AddRegisterRange($4100, $5FFF, moAny);
	RemoveRegisterRange($8000, $FFFF, moRead);
	chrBank := 0;
	SelectPRGPage(0, 0);
	SelectCHRPage(0, 0);
end;

function TMapper_036.ReadRegister(addr: Word): Byte;
var
	openBus: Byte;
begin
	openBus := Console.MemoryManager.GetOpenBus;
	if (addr and $103) = $100 then
		Result := (openBus and $CF) or ((txc.Read shl 4) and $30)
	else
		Result := openBus;
	UpdateState;
end;

procedure TMapper_036.WriteRegister(addr: Word; value: Byte);
begin
	if (addr and $F200) = $4200 then
		chrBank := value;
	txc.Write(addr, (value shr 4) and $03);
	UpdateState;
end;

procedure TMapper_036.UpdateState;
begin
	SelectPRGPage(0, txc.GetOutput and $03);
	SelectCHRPage(0, chrBank);
end;


end.
