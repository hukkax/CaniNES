unit NES.Mapper_230;

// Mapper 230: multicart (22-in-1)

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper;

type
	TMapper_230 = class(TMapper)
	private
		ContraMode: Boolean;
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;

		procedure InitMapper; override;
		procedure WriteRegister(addr: Word; value: Byte); override;
	public
		procedure Reset(SoftReset: Boolean); override;

		constructor Create(cartridge: TCartridge); override;
	end;


implementation

uses
	Basement.Util;

// ============================================================================
// TMapper_230
// ============================================================================

function TMapper_230.GetPRGPageSize: Word;
begin
	Result := $4000;
end;

function TMapper_230.GetCHRPageSize: Word;
begin
	Result := $2000;
end;

constructor TMapper_230.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(1, @ContraMode);
end;

procedure TMapper_230.InitMapper;
begin
	SelectCHRPage(0, 0);
	Reset(True);
end;

procedure TMapper_230.Reset(SoftReset: Boolean);
begin
	if softReset then
	begin
		ContraMode := not ContraMode;

		if ContraMode then
		begin
			SelectPRGPage(0, 0);
			SelectPRGPage(1, 7);
			SetMirroringType(MIRROR_VERTICAL);
		end
		else
		begin
			SelectPRGPage(0, 8);
			SelectPRGPage(1, 9);
			SetMirroringType(MIRROR_HORIZONTAL);
		end;
	end;
end;

procedure TMapper_230.WriteRegister(addr: Word; value: Byte);
begin
	if ContraMode then
		SelectPRGPage(0, value and $07)
	else
	begin
		if (value and $20) <> 0 then
		begin
			SelectPRGPage(0, (value and $1F) + 8);
			SelectPRGPage(1, (value and $1F) + 8);
		end
		else
		begin
			SelectPRGPage(0, (value and $1E) + 8);
			SelectPRGPage(1, (value and $1E) + 9);
		end;

		ApplyMirroringType(value and $40, MIRROR_VERTICAL, MIRROR_HORIZONTAL);
	end;
end;

end.

