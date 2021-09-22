unit NES.Mapper_234;

// Mapper 234: Maxi 15

interface

uses
	NES.Types, NES.Mapper;

type
	TMapper_234 = class(TMapper)
	private
		regs: array[0..1] of Byte;

		procedure UpdateState;
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;

		function  RegisterStartAddress: Word; override;
		function  RegisterEndAddress: Word; override;
		function  AllowRegisterRead: Boolean; override;
		function  HasBusConflicts: Boolean; override;

		function  ReadRegister(addr: Word): Byte; override;
		procedure WriteRegister(addr: Word;  value: Byte); override;

		procedure InitMapper; override;
	end;


implementation


{ TMapper_234 }

function TMapper_234.GetPRGPageSize: Word;       begin Result := $8000; end;
function TMapper_234.GetCHRPageSize: Word;       begin Result := $2000; end;
function TMapper_234.RegisterStartAddress: Word; begin Result := $FF80; end;
function TMapper_234.RegisterEndAddress:   Word; begin Result := $FF9F; end;
function TMapper_234.AllowRegisterRead: Boolean; begin Result := True;  end;
function TMapper_234.HasBusConflicts:   Boolean; begin Result := True;  end;

procedure TMapper_234.InitMapper;
begin
	AddRegisterRange($FFE8, $FFF8, moAny);
	regs[0] := 0; regs[1] := 0;
	UpdateState;
end;

procedure TMapper_234.UpdateState;
begin
	if (regs[0] and $40) <> 0 then
	begin
		// NINA-03 mode
		SelectPRGPage(0, (regs[0] and $0E) or (regs[1] and $01));
		SelectCHRPage(0, ((regs[0] << 2) and $38) or ((regs[1] >> 4) and $07));
	end
	else
	begin
		//CNROM mode
		SelectPRGPage(0, regs[0] and $0F);
		SelectCHRPage(0, ((regs[0] << 2) and $3C) or ((regs[1] >> 4) and $03));
	end;

	ApplyMirroringType(regs[0] and $80, MIRROR_HORIZONTAL, MIRROR_VERTICAL);
end;

function TMapper_234.ReadRegister(addr: Word): Byte;
begin
	Result := InternalReadRam(addr);
	if addr <= $FF9F then
	begin
		if (regs[0] and $3F) = 0 then
		begin
			regs[0] := Result;
			UpdateState;
		end;
	end
	else
	begin
		regs[1] := Result and $71;
		UpdateState;
	end;
end;

procedure TMapper_234.WriteRegister(addr: Word; value: Byte);
begin
	if addr <= $FF9F then
	begin
		if (regs[0] and $3F) = 0 then
		begin
			regs[0] := value;
			UpdateState;
		end;
	end
	else
	begin
		regs[1] := value and $71;
		UpdateState;
	end;
end;

end.
