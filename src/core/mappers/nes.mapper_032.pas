unit NES.Mapper_032;

// Mapper 032: IREM G-101

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper;

type
	TMapper_032 = class(TMapper)
	protected
		prgMode: Byte;
		prgRegs: array [0..1] of Byte;

		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;

		procedure InitMapper; override;
		procedure UpdatePrgMode;
		procedure WriteRegister(addr: Word;  value: Byte); override;
	public
		constructor Create(cartridge: TCartridge); override;
	end;


implementation

uses Math;

{ TMapper_032 }

function TMapper_032.GetPRGPageSize: Word;
begin
	Result := $2000;
end;

function TMapper_032.GetCHRPageSize: Word;
begin
	Result := $400;
end;

constructor TMapper_032.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(8, @prgMode);
	RegisterArray(Length(prgRegs), @prgRegs[0]);
end;

procedure TMapper_032.InitMapper;
begin
	prgMode := 0;
	prgRegs[0] := 0;
	prgRegs[1] := 0;

	SelectPRGPage(2, -2);
	SelectPRGPage(3, -1);

	// 032: 1 Major League
	// CIRAM A10 is tied high (fixed one-screen mirroring) and
	// PRG banking style is fixed as 8+8+16F
	if romInfo.SubMapperID = 1 then
		SetMirroringType(MIRROR_SCREENAONLY);
end;

procedure TMapper_032.UpdatePrgMode;
begin
	if prgMode = 0 then
	begin
		SelectPRGPage(0, prgRegs[0]);
		SelectPRGPage(1, prgRegs[1]);
		SelectPRGPage(2, -2);
		SelectPRGPage(3, -1);
	end
	else
	begin
		SelectPRGPage(0, -2);
		SelectPRGPage(1, prgRegs[1]);
		SelectPRGPage(2, prgRegs[0]);
		SelectPRGPage(3, -1);
	end;
end;

procedure TMapper_032.WriteRegister(addr: Word; value: Byte);
begin
	case (addr and $F000) of

		$8000:
		begin
			prgRegs[0] := value and $1F;
			SelectPRGPage(IfThen(prgMode = 0, 0, 2), prgRegs[0]);
		end;

		$9000:
		begin
			prgMode := (value and $02) shr 1;
			if romInfo.SubMapperID = 1 then
				prgMode := 0;
			UpdatePrgMode;
			ApplyMirroringType(value and $01, MIRROR_HORIZONTAL, MIRROR_VERTICAL);
		end;

		$A000:
		begin
			prgRegs[1] := value and $1F;
			SelectPRGPage(1, prgRegs[1]);
		end;

		$B000:
			SelectCHRPage(addr and $07, value);
	end;
end;

end.
