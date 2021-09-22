unit NES.Mapper_038;

// Mapper 038: UnlPci556

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper;

type
	TMapper_038 = class(TMapper)
	private
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;
		function  RegisterStartAddress: Word; override;
		function  RegisterEndAddress:   Word; override;

		procedure InitMapper; override;
		procedure WriteRegister(addr: Word;  value: Byte); override;
	public
	end;


implementation


function TMapper_038.GetPRGPageSize: Word;
begin
	Result := $8000;
end;

function TMapper_038.GetCHRPageSize: Word;
begin
	Result := $2000;
end;

function TMapper_038.RegisterStartAddress: Word;
begin
	Result := $7000;
end;

function TMapper_038.RegisterEndAddress: Word;
begin
	Result := $7FFF;
end;

procedure TMapper_038.InitMapper;
begin
	SelectPRGPage(0, 0);
	SelectCHRPage(0, 0);
end;

procedure TMapper_038.WriteRegister(addr: Word; value: Byte);
begin
	SelectPRGPage(0, value and 3);
	SelectCHRPage(0, (value shr 2) and 3);
end;

end.
