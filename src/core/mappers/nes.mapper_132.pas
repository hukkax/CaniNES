unit NES.Mapper_132;

// Mapper 132: TXC22211A
// Mapper 172: TXC22211B
// Mapper 173: TXC22211C

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper,
	NES.TXCChip;

type
	TMapper_132 = class(TMapper)
	protected
		TXC: TTXCChip;

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

	TMapper_172 = class(TMapper_132)
	protected
		function  ReadRegister(addr: Word): Byte; override;
		procedure WriteRegister(addr: Word;  value: Byte); override;
		procedure UpdateState; override;
	end;

	TMapper_173 = class(TMapper_132)
	protected
		procedure UpdateState; override;
	end;

implementation

uses
	Math,
	NES.Console;

{ TMapper_132 }

function TMapper_132.GetPRGPageSize: Word;       begin Result := $8000; end;
function TMapper_132.GetCHRPageSize: Word;       begin Result := $2000; end;
function TMapper_132.RegisterStartAddress: Word; begin Result := $8000; end;
function TMapper_132.RegisterEndAddress: Word;   begin Result := $FFFF; end;
function TMapper_132.AllowRegisterRead: Boolean; begin Result := True;  end;

constructor TMapper_132.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	TXC := TTXCChip.Create(False);
end;

destructor TMapper_132.Destroy;
begin
	TXC.Free;

	inherited Destroy;
end;

procedure TMapper_132.InitMapper;
begin
	AddRegisterRange($4020, $5FFF, moAny);
	RemoveRegisterRange($8000, $FFFF, moRead);
	SelectPRGPage(0, 0);
	SelectCHRPage(0, 0);
end;

function TMapper_132.ReadRegister(addr: Word): Byte;
var
	openBus: Byte;
begin
	openBus := Console.MemoryManager.GetOpenBus;
	if (addr and $103) = $100 then
		Result := (openBus and $F0) or (txc.Read and $0F)
	else
		Result := openBus;
	UpdateState;
end;

procedure TMapper_132.WriteRegister(addr: Word; value: Byte);
begin
	txc.Write(addr, value and $0F);
	UpdateState;
end;

procedure TMapper_132.UpdateState;
begin
	SelectPRGPage(0, (txc.GetOutput shr 2) and $01);
	SelectCHRPage(0, txc.GetOutput and $03);
end;


{ TMapper_172 }

function ConvertValue(v: Byte): Byte; inline;
begin
	Result :=
		((v and $01) shl 5) or ((v and $02) shl 3) or ((v and $04) shl 1) or
		((v and $08) shr 1) or ((v and $10) shr 3) or ((v and $20) shr 5);
end;

function TMapper_172.ReadRegister(addr: Word): Byte;
var
	openBus: Byte;
begin
	openBus := Console.MemoryManager.GetOpenBus;
	if (addr and $103) = $100 then
		Result := (openBus and $C0) or ConvertValue(txc.Read)
	else
		Result := openBus;
	UpdateState;
end;

procedure TMapper_172.WriteRegister(addr: Word; value: Byte);
begin
	txc.Write(addr, ConvertValue(value));
	if addr >= $8000 then
		UpdateState;
end;

procedure TMapper_172.UpdateState;
begin
	SelectCHRPage(0, txc.GetOutput);
	ApplyMirroringType(txc.GetInvertFlag, MIRROR_VERTICAL, MIRROR_HORIZONTAL);
end;


{ TMapper_173 }

procedure TMapper_173.UpdateState;
begin
	SelectPRGPage(0, 0);
	if chrRomSize > $2000 then
		SelectCHRPage(0, (txc.GetOutput and $01) or (IfThen(txc.GetY, $02, 0)) or ((txc.GetOutput and $02) shl 1))
	else
	begin
		if txc.GetY then
			SelectCHRPage(0, 0)
		else
			RemovePpuMemoryMapping(0, $1FFF);
	end;
end;

end.
