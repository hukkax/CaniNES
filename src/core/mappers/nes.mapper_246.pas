unit NES.Mapper_246;

// Mapper 246: Fong Shen Bang - Zhu Lu Zhi Zhan

interface

uses
	NES.Types, NES.Mapper;

type
	TMapper_246 = class(TMapper)
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;
		function  RegisterStartAddress: Word; override;
		function  RegisterEndAddress: Word; override;

		procedure InitMapper; override;
		procedure WriteRegister(addr: Word;  value: Byte); override;
	public
		procedure Reset(SoftReset: Boolean); override;
	end;


implementation

{ TMapper_246 }

function TMapper_246.GetPRGPageSize: Word;
begin
	Result := $2000;
end;

function TMapper_246.GetCHRPageSize: Word;
begin
	Result := $800;
end;

function TMapper_246.RegisterStartAddress: Word;
begin
	Result := $6000;
end;

function TMapper_246.RegisterEndAddress: Word;
begin
	Result := $67FF;
end;

procedure TMapper_246.InitMapper;
begin
	SelectPRGPage(3, $FF);
end;

procedure TMapper_246.Reset(SoftReset: Boolean);
begin
	InitMapper;
end;

procedure TMapper_246.WriteRegister(addr: Word; value: Byte);
begin
	if (addr and 7) <= 3 then
		SelectPRGPage(addr and 3, value)
	else
		SelectCHRPage(addr and 3, value);
end;

end.
