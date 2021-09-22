unit NES.Mapper_180;

// Mapper 180: UnRom_180

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper;

type
	TMapper_180 = class(TMapper)
	private
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;

		procedure InitMapper; override;
		procedure WriteRegister(addr: Word;  value: Byte); override;
	public
	end;


implementation


function TMapper_180.GetPRGPageSize: Word;
begin
	Result := $4000;
end;

function TMapper_180.GetCHRPageSize: Word;
begin
	Result := $2000;
end;

procedure TMapper_180.InitMapper;
begin
	SelectPRGPage(0, 0);
	SelectPRGPage(1, GetPowerOnByte and $07);
	SelectCHRPage(0, 0);
end;

procedure TMapper_180.WriteRegister(addr: Word; value: Byte);
begin
	SelectPRGPage(1, value and $07);
end;

end.
