unit NES.Mapper_000;

// Mapper 000: NROM

interface

uses
	NES.Mapper;

type
	TMapper_000 = class(TMapper)
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;

		procedure InitMapper; override;
	end;

implementation

procedure TMapper_000.InitMapper;
begin
	SelectPRGPage(0, 0);
	SelectPRGPage(1, 1);
	SelectCHRPage(0, 0);
end;

function TMapper_000.GetPRGPageSize: Word;
begin
	Result := $4000;
end;

function TMapper_000.GetCHRPageSize: Word;
begin
	Result := $2000;
end;

end.
