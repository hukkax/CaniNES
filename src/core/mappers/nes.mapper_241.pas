unit NES.Mapper_241;

// Mapper 241: BxROM-like

interface

uses
	NES.Mapper, NES.Mapper_011;

type
	TMapper_241 = class(TMapper_011)
	protected
		function HasBusConflicts: Boolean; override;
		procedure WriteRegister(addr: Word; value: Byte); override;
	end;

implementation

function TMapper_241.HasBusConflicts: Boolean;
begin
	Result := False;
end;

procedure TMapper_241.WriteRegister(addr: Word; value: Byte);
begin
	SelectPRGPage(0, value);
end;

end.

