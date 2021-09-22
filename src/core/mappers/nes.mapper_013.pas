unit NES.Mapper_013;

// CPROM (Videomation)

interface

uses
	NES.Types, NES.Mapper;

type
	TMapper_013 = class(TMapper)
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;
		function  GetCHRRamSize: Cardinal; override;

		procedure WriteRegister(addr: Word;  value: Byte); override;
		procedure InitMapper; override;
	end;

implementation

{ TMapper_013 }

function TMapper_013.GetPRGPageSize: Word;
begin
	Result := $8000;
end;

function TMapper_013.GetCHRPageSize: Word;
begin
	Result := $1000;
end;

function TMapper_013.GetCHRRamSize: Cardinal;
begin
	Result := $4000;
end;

procedure TMapper_013.InitMapper;
begin
	SelectPRGPage(0, 0);
	SelectCHRPage(0, 0);
	SetMirroringType(TMirroringType.MIRROR_VERTICAL);
end;

procedure TMapper_013.WriteRegister(addr: Word; value: Byte);
begin
	if addr >= $8000 then
		SelectCHRPage(1, value and 3);
end;

end.
