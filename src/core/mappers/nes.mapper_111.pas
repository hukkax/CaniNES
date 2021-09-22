unit NES.Mapper_111;

// Mapper 111: GTROM/Cheapocabra

interface

uses
	NES.Types, NES.Mapper,
	NES.Flash.SST39SF040;

type
	TMapper_111 = class(TMapper_SST39SF040)
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;
		function  GetWorkRamSize: Cardinal; override;
		function  GetSaveRamSize: Cardinal; override;
		function  GetChrRamSize:  Cardinal; override;

		function  RegisterStartAddress: Word; override;
		function  RegisterEndAddress:   Word; override;

		function  AllowRegisterRead: Boolean; override;

		procedure WriteRegister(addr: Word;  value: Byte); override;

		procedure InitMapper; override;
	public
	end;


implementation

{ TMapper_111 }

function TMapper_111.GetPRGPageSize: Word;       begin Result := $8000; end;
function TMapper_111.GetCHRPageSize: Word;       begin Result := $2000; end;
function TMapper_111.GetWorkRamSize: Cardinal;   begin Result := 0;     end;
function TMapper_111.GetSaveRamSize: Cardinal;   begin Result := 0;     end;
function TMapper_111.GetChrRamSize:  Cardinal;   begin Result := $4000; end;
function TMapper_111.RegisterStartAddress: Word; begin Result := $5000; end;
function TMapper_111.RegisterEndAddress:   Word; begin Result := $5FFF; end;
function TMapper_111.AllowRegisterRead: Boolean; begin Result := True; end;

procedure TMapper_111.InitMapper;
begin
	FlashInit(@prgRom[0], prgSize);

	AddRegisterRange($7000, $7FFF, moWrite);
	AddRegisterRange($8000, $FFFF, moAny);
	RemoveRegisterRange($5000, $5FFF, moRead);

	WriteRegister($5000, GetPowerOnByte);

	orgPrgRom := prgRom;
	ApplySaveData;
end;

procedure TMapper_111.WriteRegister(addr: Word; value: Byte);
var
	i: Integer;
begin
	if addr < $8000 then
	begin
		prgByte := value and $0F;
		SelectPRGPage(0, prgByte);

		SelectCHRPage(0, (value shr 4) and 1);
		for i := 0 to 7 do
		begin
			if (value and $20) <> 0 then
				SetNametable(i, 8+i)
			else
				SetNametable(i, i);
		end;
	end
	else
		FlashWrite(Word(prgByte shl 15) or (addr and $7FFF), value);
end;


end.

