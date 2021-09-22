unit NES.Mapper_002;

// Mapper 002: UNROM

interface

uses
	NES.Mapper;

type
	TMapper_002 = class(TMapper)
	protected
		function GetPRGPageSize: Word; override;
		function GetCHRPageSize: Word; override;
		function HasBusConflicts: Boolean; override;

		procedure InitMapper; override;
		procedure WriteRegister(addr: Word; value: Byte); override;
	end;

implementation

function TMapper_002.GetPRGPageSize: Word;
begin
	Result := $4000;
end;

function TMapper_002.GetCHRPageSize: Word;
begin
	Result := $2000;
end;

function TMapper_002.HasBusConflicts: Boolean;
begin
	Result := RomInfo.SubMapperID = 2;
end;

procedure TMapper_002.InitMapper;
begin
	inherited;
	//First and last PRG page
	SelectPRGPage(0,  0);
	SelectPRGPage(1, -1);
	SelectCHRPage(0,  0);
end;

procedure TMapper_002.WriteRegister(addr: Word; value: Byte);
begin
	SelectPRGPage(0, value);
end;

end.

