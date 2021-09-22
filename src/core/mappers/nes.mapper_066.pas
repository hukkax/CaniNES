unit NES.Mapper_066;

// Mapper 066: GxROM

interface

uses
	NES.Mapper;

type
	TMapper_066 = class(TMapper)
	private
	protected
		function GetPRGPageSize: Word; override;
		function GetCHRPageSize: Word; override;

		procedure InitMapper; override;
		procedure WriteRegister(addr: Word; value: Byte); override;
	public
	end;

implementation

function TMapper_066.GetPRGPageSize: Word;
begin
	Result := $8000;
end;

function TMapper_066.GetCHRPageSize: Word;
begin
	Result := $2000;
end;

procedure TMapper_066.InitMapper;
begin
	SelectPRGPage(0, GetPowerOnByte and $03);
	SelectCHRPage(0, GetPowerOnByte and $03);
end;

procedure TMapper_066.WriteRegister(addr: Word; value: Byte);
begin
	SelectPRGPage(0, (value >> 4) and $03);
	SelectCHRPage(0, value and $03);
end;

end.

