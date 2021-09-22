unit NES.Mapper_093;

// Mapper 093: Sunsoft

interface

uses
	NES.Types, NES.Mapper;

type
	TMapper_093 = class(TMapper)
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;

		procedure WriteRegister(addr: Word;  value: Byte); override;

		procedure InitMapper; override;
	end;


implementation

{ TMapper_093 }

function TMapper_093.GetPRGPageSize: Word; begin Result := $4000; end;
function TMapper_093.GetCHRPageSize: Word; begin Result := $2000; end;

procedure TMapper_093.InitMapper;
begin
	SelectPRGPage(1, -1);
end;

procedure TMapper_093.WriteRegister(addr: Word; value: Byte);
begin
	SelectPRGPage(0, (value shr 4) and $07);
	if Odd(value) then
		SelectCHRPage(0, 0)
	else
		RemovePpuMemoryMapping($0000, $1FFF);
end;


end.
