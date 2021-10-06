unit NES.Mapper_046;

{$WARN 4035 off : Mixing signed expressions and longwords gives a 64bit result}

// Mapper 046: Color Dreams Rumblestation 15-in-1

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper;

type
	TMapper_046 = class(TMapper)
	private
		regs: array [0..1] of Byte;
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;
		function  RegisterStartAddress: Word; override;
		function  RegisterEndAddress: Word; override;

		procedure UpdateState;
		procedure InitMapper; override;
		procedure WriteRegister(addr: Word;  value: Byte); override;
	public
		procedure Reset(SoftReset: Boolean); override;

		constructor Create(cartridge: TCartridge); override;
	end;


implementation

{ TMapper_046 }

function TMapper_046.GetPRGPageSize: Word;
begin
	Result := $8000;
end;

function TMapper_046.GetCHRPageSize: Word;
begin
	Result := $2000;
end;

function TMapper_046.RegisterStartAddress: Word;
begin
	Result := $6000;
end;

function TMapper_046.RegisterEndAddress: Word;
begin
	Result := $FFFF;
end;

constructor TMapper_046.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterArray(Length(regs), @regs[0]);
end;

procedure TMapper_046.InitMapper;
begin
	WriteRegister($6000, 0);
	WriteRegister($8000, 0);
end;

procedure TMapper_046.Reset(SoftReset: Boolean);
begin
	InitMapper;
end;

procedure TMapper_046.UpdateState;
begin
	SelectPRGPage(0, ((regs[0] and $0F) << 1) or  (regs[1] and $01));
	SelectCHRPage(0, ((regs[0] and $F0) >> 1) or ((regs[1] and $70) >> 4));
end;

procedure TMapper_046.WriteRegister(addr: Word; value: Byte);
begin
	if addr < $8000 then
		regs[0] := value
	else
		regs[1] := value;

	UpdateState;
end;

end.
