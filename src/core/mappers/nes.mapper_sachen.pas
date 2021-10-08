unit NES.Mapper_Sachen;

// Mapper : Sachen

interface

uses
	NES.Types, NES.Mapper;

type
	TMapper_Sachen = class(TMapper)
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;

		procedure InitMapper; override;
	end;

	TMapper_133 = class(TMapper_Sachen)
	protected
		function  RegisterStartAddress: Word; override;
		function  RegisterEndAddress: Word; override;
		procedure InitMapper; override;
		procedure WriteRegister(address: Word; value: Byte); override;
	end;

	TMapper_148 = class(TMapper_Sachen)
	protected
		function  HasBusConflicts: Boolean; override;
		procedure WriteRegister(address: Word; value: Byte); override;
	end;

	TMapper_149 = class(TMapper_Sachen)
	protected
		procedure WriteRegister(address: Word; value: Byte); override;
	end;


implementation

uses
	SysUtils,
	NES.Config;

{ TMapper_Sachen }

function TMapper_Sachen.GetPRGPageSize: Word;
begin
	Result := $8000;
end;

function TMapper_Sachen.GetCHRPageSize: Word;
begin
	Result := $2000;
end;

procedure TMapper_Sachen.InitMapper;
begin
	SelectPRGPage(0, 0);
end;

{ TMapper_133 }

function TMapper_133.RegisterStartAddress: Word; begin Result := $4100; end;
function TMapper_133.RegisterEndAddress: Word;   begin Result := $FFFF; end;

procedure TMapper_133.InitMapper;
begin
	inherited;
	SelectCHRPage(0, 0);
end;

procedure TMapper_133.WriteRegister(address: Word; value: Byte);
begin
	if (address and $6100) = $4100 then
	begin
		SelectPRGPage(0, (value >> 2) and 1);
		SelectCHRPage(0, value and 3);
	end;
end;

{ TMapper_148 }

function TMapper_148.HasBusConflicts: Boolean;
begin
	Result := True;
end;

procedure TMapper_148.WriteRegister(address: Word; value: Byte);
begin
	SelectPRGPage(0, (value >> 3) and 1);
	SelectCHRPage(0, value and 7);
end;

{ TMapper_149 }

procedure TMapper_149.WriteRegister(address: Word; value: Byte);
begin
	SelectCHRPage(0, (value >> 7) and 1);
end;

end.

