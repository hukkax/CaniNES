unit NES.Mapper_034;

// Mapper 034: BNROM/NINA-001

interface

uses
	NES.Mapper;

type
	TMapper_034_BNROM = class(TMapper)
	protected
		function GetPRGPageSize: Word; override;
		function GetCHRPageSize: Word; override;

		procedure InitMapper; override;
		procedure WriteRegister(addr: Word; value: Byte); override;
	end;

	TMapper_034_Nina001 = class(TMapper)
	protected
		function GetPRGPageSize: Word; override;
		function GetCHRPageSize: Word; override;

		function RegisterStartAddress: Word; override;
		function RegisterEndAddress: Word; override;

		procedure InitMapper; override;
		procedure WriteRegister(addr: Word; value: Byte); override;
	end;


	function GetMapper034Class(MapperID, SubMapperID: Word; ChrRomSize: Cardinal): TMapperClass;


implementation

uses
	NES.Types;


function GetMapper034Class(MapperID, SubMapperID: Word; ChrRomSize: Cardinal): TMapperClass;
var
	B: Boolean;
begin
	Result := nil;
	if MapperID <> 34 then Exit;

	case SubMapperID of
		0: B := ChrRomSize = 0; // BnROM uses CHR RAM (so no CHR rom in the .NES file)
		1: B := False;
		2: B := True;
	else
		Exit;
	end;
	if B then
		Result := TMapper_034_BNROM // BNROM
	else
		Result := TMapper_034_Nina001; // NINA-001
end;

{ TMapper_034_BNROM }

function TMapper_034_BNROM.GetPRGPageSize: Word;
begin
	Result := $8000;
end;

function TMapper_034_BNROM.GetCHRPageSize: Word;
begin
	Result := $2000;
end;

procedure TMapper_034_BNROM.InitMapper;
begin
	SelectPRGPage(0, GetPowerOnByte);
	SelectCHRPage(0, 0);
end;

procedure TMapper_034_BNROM.WriteRegister(addr: Word; value: Byte);
begin
	// "While the original BNROM board connects only 2 bits, it is recommended that emulators
	// implement this as an 8-bit register allowing selection of up to 8 MB PRG ROM if present."
	SelectPRGPage(0, value);
end;


{ TMapper_034_Nina001 }

function TMapper_034_Nina001.GetPRGPageSize: Word;
begin
	Result := $8000;
end;

function TMapper_034_Nina001.GetCHRPageSize: Word;
begin
	Result := $1000;
end;

function TMapper_034_Nina001.RegisterStartAddress: Word;
begin
	Result := $7FFD;
end;

function TMapper_034_Nina001.RegisterEndAddress: Word;
begin
	Result := $7FFF;
end;

procedure TMapper_034_Nina001.InitMapper;
begin
	SelectPRGPage(0, 0);
end;

procedure TMapper_034_Nina001.WriteRegister(addr: Word; value: Byte);
begin
	case addr of
		$7FFD: SelectPRGPage(0, value and $01);
		$7FFE: SelectCHRPage(0, value and $0F);
		$7FFF: SelectCHRPage(1, value and $0F);
	end;
	WritePrgRam(addr, value);
end;


end.

