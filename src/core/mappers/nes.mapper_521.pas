unit NES.Mapper_521;

// Mapper 521: DreamTech01 (Korean Igo)

interface

uses
	NES.Types, NES.Mapper;

type
	TMapper_521 = class(TMapper)
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

{ TMapper_521 }

function TMapper_521.GetPRGPageSize: Word;
begin
	Result := $4000;
end;

function TMapper_521.GetCHRPageSize: Word;
begin
	Result := $2000;
end;

function TMapper_521.RegisterStartAddress: Word;
begin
	Result := $5020;
end;

function TMapper_521.RegisterEndAddress: Word;
begin
	Result := $5020;
end;

procedure TMapper_521.InitMapper;
begin
	SelectPRGPage(0, 0);
	SelectPRGPage(1, 8);
	SelectCHRPage(0, 0);
end;

procedure TMapper_521.WriteRegister(addr: Word; value: Byte);
begin
	SelectPRGPage(0, value and $07);
end;

end.
