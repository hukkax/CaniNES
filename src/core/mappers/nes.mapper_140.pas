unit NES.Mapper_140;

// Mapper 140: Jaleco JF-11/JF-14

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper;

type
	TMapper_140 = class(TMapper)
	private
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;

		function  RegisterStartAddress: Word; override;
		function  RegisterEndAddress:   Word; override;

		procedure WriteRegister(addr: Word;  value: Byte); override;

		procedure InitMapper; override;
	public
	end;


implementation

{ TMapper_140 }

function TMapper_140.GetPRGPageSize: Word;
begin
	Result := $8000;
end;

function TMapper_140.GetCHRPageSize: Word;
begin
	Result := $2000;
end;

function TMapper_140.RegisterStartAddress: Word;
begin
	Result := $6000;
end;

function TMapper_140.RegisterEndAddress: Word;
begin
	Result := $7FFF;
end;

procedure TMapper_140.InitMapper;
begin
	SelectPRGPage(0, 0);
	SelectCHRPage(0, 0);
end;

procedure TMapper_140.WriteRegister(addr: Word; value: Byte);
begin
	SelectPRGPage(0, (value shr 4) and $03);
	SelectCHRPage(0, value and $0F);
end;

end.
