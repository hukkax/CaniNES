unit NES.Mapper_193;

// Mapper 193: NTDEC TC-112

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper;

type
	TMapper_193 = class(TMapper)
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

{ TMapper_193 }

function TMapper_193.GetPRGPageSize: Word;
begin
	Result := $2000;
end;

function TMapper_193.GetCHRPageSize: Word;
begin
	Result := $0800;
end;

function TMapper_193.RegisterStartAddress: Word;
begin
	Result := $6000;
end;

function TMapper_193.RegisterEndAddress: Word;
begin
	Result := $7FFF;
end;

procedure TMapper_193.InitMapper;
begin
	SelectPRGPage(1, -3);
	SelectPRGPage(2, -2);
	SelectPRGPage(3, -1);
end;

procedure TMapper_193.WriteRegister(addr: Word; value: Byte);
begin
	case (addr and $03) of
		0:	begin
			SelectCHRPage(0,  value shr 1);
			SelectCHRPage(1, (value shr 1) + 1);
			end;
		1:	SelectCHRPage(2,  value shr 1);
		2:	SelectCHRPage(3,  value shr 1);
		3:	SelectPRGPage(0,  value);
	end;
end;

end.
