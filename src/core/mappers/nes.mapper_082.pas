unit NES.Mapper_082;

// Mapper 082: Taito X1017

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper;

type
	TMapper_082 = class(TMapper)
	private
		chrMode: Byte;
		chrRegs: array [0..5] of Byte;
		ramPermission: array [0..2] of Byte;

	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;
		function  GetSaveRamSize: Cardinal; override;
		function  GetSaveRamPageSize: Cardinal; override;
		function  RegisterStartAddress: Word; override;
		function  RegisterEndAddress:   Word; override;

		procedure WriteRegister(addr: Word;  value: Byte); override;

		procedure InitMapper; override;

		procedure UpdateRamAccess;
		procedure UpdateChrBanking;
	public
		procedure LoadSnapshot; override;

		constructor Create(cartridge: TCartridge); override;
	end;


implementation

{ TMapper_082 }

constructor TMapper_082.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(8, @chrMode);

	RegisterArray(Length(ramPermission), @ramPermission[0]);
	RegisterArray(Length(chrRegs), @chrRegs[0]);
end;

procedure TMapper_082.InitMapper;
begin
	chrMode := 0;
	FillByte(ramPermission[0], SizeOf(ramPermission), 0);
	FillByte(chrRegs[0], SizeOf(chrRegs), 0);

	SelectPRGPage(3, -1);
	UpdateRamAccess;
end;

function TMapper_082.GetPRGPageSize: Word; begin Result := $2000; end;
function TMapper_082.GetCHRPageSize: Word; begin Result := $0400; end;
function TMapper_082.GetSaveRamSize:     Cardinal; begin Result := $1400; end;
function TMapper_082.GetSaveRamPageSize: Cardinal; begin Result := $0400; end;
function TMapper_082.RegisterStartAddress: Word;   begin Result := $7EF0; end;
function TMapper_082.RegisterEndAddress:   Word;   begin Result := $7EFF; end;

procedure TMapper_082.UpdateRamAccess;
begin
	SetCpuMemoryMapping($6000, $63FF, 0, PrgSaveRam,
		ChooseMemoryAccessType(ramPermission[0] = $CA, maReadWrite, maNoAccess));
	SetCpuMemoryMapping($6400, $67FF, 1, PrgSaveRam,
		ChooseMemoryAccessType(ramPermission[0] = $CA, maReadWrite, maNoAccess));
	SetCpuMemoryMapping($6800, $6BFF, 2, PrgSaveRam,
		ChooseMemoryAccessType(ramPermission[1] = $69, maReadWrite, maNoAccess));
	SetCpuMemoryMapping($6C00, $6FFF, 3, PrgSaveRam,
		ChooseMemoryAccessType(ramPermission[1] = $69, maReadWrite, maNoAccess));
	SetCpuMemoryMapping($7000, $73FF, 4, PrgSaveRam,
		ChooseMemoryAccessType(ramPermission[2] = $84, maReadWrite, maNoAccess));
end;

procedure TMapper_082.UpdateChrBanking;
begin
	if chrMode = 0 then
	begin
		//Regs 0 and 1 ignore the LSB
		SelectChrPage2x(0, chrRegs[0] and $FE);
		SelectChrPage2x(1, chrRegs[1] and $FE);

		SelectCHRPage(4, chrRegs[2]);
		SelectCHRPage(5, chrRegs[3]);
		SelectCHRPage(6, chrRegs[4]);
		SelectCHRPage(7, chrRegs[5]);
	end
	else
	begin
		SelectCHRPage(0, chrRegs[2]);
		SelectCHRPage(1, chrRegs[3]);
		SelectCHRPage(2, chrRegs[4]);
		SelectCHRPage(3, chrRegs[5]);

		//Regs 0 and 1 ignore the LSB
		SelectChrPage2x(2, chrRegs[0] and $FE);
		SelectChrPage2x(3, chrRegs[1] and $FE);
	end;
end;

procedure TMapper_082.WriteRegister(addr: Word; value: Byte);
begin
	case addr of

		$7EF0..$7EF5:
		begin
			chrRegs[addr and $F] := value;
			UpdateChrBanking;
		end;

		$7EF6:
		begin
			ApplyMirroringType(Odd(value), MIRROR_VERTICAL, MIRROR_HORIZONTAL);
			chrMode := (value and $02) shr 1;
			UpdateChrBanking;
		end;

		$7EF7..$7EF9:
		begin
			ramPermission[(addr and $F) - 7] := value;
			UpdateRamAccess;
		end;

		$7EFA: SelectPRGPage(0, value shr 2);
		$7EFB: SelectPRGPage(1, value shr 2);
		$7EFC: SelectPRGPage(2, value shr 2);
	end;
end;

procedure TMapper_082.LoadSnapshot;
begin
	inherited;
	UpdateRamAccess;
end;


end.
