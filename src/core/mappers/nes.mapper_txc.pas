unit NES.Mapper_TXC;

// Mapper 132: TXC22211A
// Mapper 136: Sachen 3011
// Mapper 147: Chinese Kungfu: 少林武者 (TC-011)
// Mapper 172: TXC22211B
// Mapper 173: TXC22211C
// Mapper 036: TXC22000

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper,
	NES.TXCChip;

type
	TMapper_TXC = class(TMapper)
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

	TMapper_132 = TMapper_TXC;

	TMapper_136 = class(TMapper_TXC)
	protected
		function  ReadRegister(addr: Word): Byte; override;
		procedure WriteRegister(addr: Word;  value: Byte); override;
		procedure UpdateState; override;
	end;

	TMapper_147 = class(TMapper_TXC)
	protected
		function  ReadRegister(addr: Word): Byte; override;
		procedure WriteRegister(addr: Word;  value: Byte); override;
		procedure UpdateState; override;
	end;

	TMapper_172 = class(TMapper_TXC)
	protected
		function  ReadRegister(addr: Word): Byte; override;
		procedure WriteRegister(addr: Word;  value: Byte); override;
		procedure UpdateState; override;
	end;

	TMapper_173 = class(TMapper_TXC)
	protected
		procedure UpdateState; override;
	end;

	TMapper_036 = class(TMapper_TXC)
	protected
		chrBank: Byte;

		function  ReadRegister(addr: Word): Byte; override;
		procedure WriteRegister(addr: Word;  value: Byte); override;

		procedure InitMapper; override;
		procedure UpdateState; override;
	public
		constructor Create(cartridge: TCartridge); override;
	end;


implementation

uses
	Math,
	NES.Console;

{ TMapper_132 }

function TMapper_TXC.GetPRGPageSize: Word;       begin Result := $8000; end;
function TMapper_TXC.GetCHRPageSize: Word;       begin Result := $2000; end;
function TMapper_TXC.RegisterStartAddress: Word; begin Result := $8000; end;
function TMapper_TXC.RegisterEndAddress: Word;   begin Result := $FFFF; end;
function TMapper_TXC.AllowRegisterRead: Boolean; begin Result := True;  end;

constructor TMapper_TXC.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	TXC := TTXCChip.Create((Self is TMapper_147) or (Self is TMapper_172));
end;

destructor TMapper_TXC.Destroy;
begin
	TXC.Free;

	inherited Destroy;
end;

procedure TMapper_TXC.InitMapper;
begin
	AddRegisterRange($4020, $5FFF, moAny);
	RemoveRegisterRange($8000, $FFFF, moRead);
	SelectPRGPage(0, 0);
	SelectCHRPage(0, 0);
end;

function TMapper_TXC.ReadRegister(addr: Word): Byte;
var
	openBus: Byte;
begin
	openBus := Console.MemoryManager.GetOpenBus;
	if (addr and $103) = $100 then
		Result := (openBus and $F0) or (TXC.Read and $0F)
	else
		Result := openBus;
	UpdateState;
end;

procedure TMapper_TXC.WriteRegister(addr: Word; value: Byte);
begin
	TXC.Write(addr, value and $0F);
	UpdateState;
end;

procedure TMapper_TXC.UpdateState;
begin
	SelectPRGPage(0, (TXC.GetOutput shr 2) and $01);
	SelectCHRPage(0, TXC.GetOutput and $03);
end;


{ TMapper_136 }

procedure TMapper_136.UpdateState;
begin
	SelectCHRPage(0, TXC.GetOutput);
end;

function TMapper_136.ReadRegister(addr: Word): Byte;
var
	openBus: Byte;
begin
	openBus := Console.MemoryManager.GetOpenBus;
	if (addr and $103) = $100 then
		Result := (openBus and $C0) or (TXC.Read and $3F)
	else
		Result := openBus;
	UpdateState;
end;

procedure TMapper_136.WriteRegister(addr: Word; value: Byte);
begin
	TXC.Write(addr, value and $3F);
	UpdateState;
end;


{ TMapper_147 }

procedure TMapper_147.UpdateState;
var
	o: Byte;
begin
	o := TXC.GetOutput;
	SelectPRGPage(0, ((o and $20) shr 4) or (o and $01));
	SelectCHRPage(0, (o and $1E) shr 1);
end;

function TMapper_147.ReadRegister(addr: Word): Byte;
var
	v: Byte;
begin
	if (addr and $103) = $100 then
	begin
		v := TXC.Read;
		Result := ((v and $3F) shl 2) or ((v and $C0) shr 6);
	end
	else
		Result := Console.MemoryManager.GetOpenBus;
	UpdateState;
end;

procedure TMapper_147.WriteRegister(addr: Word; value: Byte);
begin
	TXC.Write(addr, ((value and $FC) shr 2) or ((value and $03) shl 6));
	if addr >= $8000 then
		UpdateState;
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
		Result := (openBus and $C0) or ConvertValue(TXC.Read)
	else
		Result := openBus;
	UpdateState;
end;

procedure TMapper_172.WriteRegister(addr: Word; value: Byte);
begin
	TXC.Write(addr, ConvertValue(value));
	if addr >= $8000 then
		UpdateState;
end;

procedure TMapper_172.UpdateState;
begin
	SelectCHRPage(0, TXC.GetOutput);
	ApplyMirroringType(TXC.GetInvertFlag, MIRROR_VERTICAL, MIRROR_HORIZONTAL);
end;


{ TMapper_173 }

procedure TMapper_173.UpdateState;
begin
	SelectPRGPage(0, 0);
	if chrRomSize > $2000 then
		SelectCHRPage(0, (TXC.GetOutput and $01) or
			(IfThen(TXC.GetY, $02, 0)) or ((TXC.GetOutput and $02) shl 1))
	else
	begin
		if TXC.GetY then
			SelectCHRPage(0, 0)
		else
			RemovePpuMemoryMapping(0, $1FFF);
	end;
end;


{ TMapper_036 }

constructor TMapper_036.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(8, @chrBank);
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
		Result := (openBus and $CF) or ((TXC.Read shl 4) and $30)
	else
		Result := openBus;
	UpdateState;
end;

procedure TMapper_036.WriteRegister(addr: Word; value: Byte);
begin
	if (addr and $F200) = $4200 then
		chrBank := value;
	TXC.Write(addr, (value shr 4) and $03);
	UpdateState;
end;

procedure TMapper_036.UpdateState;
begin
	SelectPRGPage(0, TXC.GetOutput and $03);
	SelectCHRPage(0, chrBank);
end;


end.
