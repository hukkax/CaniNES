unit NES.Mapper_072;

// Mapper 072/092: Jaleco JF-17/19

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper;

type
	TMapper_072 = class(TMapper)
	protected
		jf19Mode,
		prgFlag,
		chrFlag: Boolean;

		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;
		function  HasBusConflicts: Boolean; override;

		procedure WriteRegister(addr: Word;  value: Byte); override;

		procedure InitMapper; override;

	public
		constructor Create(cartridge: TCartridge); override;
	end;

	TMapper_092 = class(TMapper_072)
	public
		constructor Create(cartridge: TCartridge); override;
	end;

implementation

{ TMapper_072 }

function TMapper_072.GetPRGPageSize: Word;
begin
	Result := $4000;
end;

function TMapper_072.GetCHRPageSize: Word;
begin
	Result := $2000;
end;

function TMapper_072.HasBusConflicts: Boolean;
begin
	Result := True;
end;

constructor TMapper_072.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(1, @jf19Mode);
	RegisterProperty(1, @prgFlag);
	RegisterProperty(1, @chrFlag);
end;

procedure TMapper_072.InitMapper;
begin
	SelectPRGPage(0,  0);
	SelectPRGPage(1, -1);
	SelectCHRPage(0,  0);
end;

procedure TMapper_072.WriteRegister(addr: Word; value: Byte);
begin
	if (not prgFlag) and ((value and $80) <> 0)  then
	begin
		if jf19Mode then
			SelectPRGPage(1, value and $0F)
		else
			SelectPRGPage(0, value and $07);
	end;

	if (not chrFlag) and ((value and $40) <> 0)  then
		SelectCHRPage(0, value and $0F);

	prgFlag := (value and $80) = $80;
	chrFlag := (value and $40) = $40;
end;

{ TMapper_092 }

constructor TMapper_092.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	jf19Mode := True;
end;

end.

