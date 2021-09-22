unit NES.Mapper_080;

// Mapper 080/207: Taito X1005

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper;

type
	TMapper_TaitoX1005 = class(TMapper)
	protected
		alternateMirroring: Boolean;
		ramPermission: Byte;

		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;
		function  GetWorkRamSize: Cardinal; override;
		function  GetSaveRamSize: Cardinal; override;
		function  GetWorkRamPageSize: Cardinal; override;
		function  GetSaveRamPageSize: Cardinal; override;
		function  RegisterStartAddress: Word; override;
		function  RegisterEndAddress:   Word; override;

		procedure WriteRegister(addr: Word;  value: Byte); override;

		procedure InitMapper; override;

		procedure UpdateRamAccess;
	public
		procedure LoadSnapshot; override;

		constructor Create(cartridge: TCartridge); override;
	end;

	TMapper_080 = TMapper_TaitoX1005;

	TMapper_207 = class(TMapper_TaitoX1005)
	public
		constructor Create(cartridge: TCartridge); override;
	end;


implementation

{ TMapper_TaitoX1005 }

constructor TMapper_TaitoX1005.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(8, @ramPermission);
end;

procedure TMapper_TaitoX1005.InitMapper;
begin
	ramPermission := 0;

	SelectPRGPage(3, -1);

	UpdateRamAccess;
end;

function TMapper_TaitoX1005.GetPRGPageSize: Word; begin Result := $2000; end;
function TMapper_TaitoX1005.GetCHRPageSize: Word; begin Result := $0400; end;
function TMapper_TaitoX1005.GetSaveRamSize:     Cardinal; begin Result := $100; end;
function TMapper_TaitoX1005.GetSaveRamPageSize: Cardinal; begin Result := $100; end;
function TMapper_TaitoX1005.GetWorkRamSize:     Cardinal; begin Result := $100; end;
function TMapper_TaitoX1005.GetWorkRamPageSize: Cardinal; begin Result := $100; end;
function TMapper_TaitoX1005.RegisterStartAddress: Word;   begin Result := $7EF0; end;
function TMapper_TaitoX1005.RegisterEndAddress:   Word;   begin Result := $7EFF; end;

procedure TMapper_TaitoX1005.UpdateRamAccess;
begin
	SetCpuMemoryMapping($7F00, $7FFF, 0,
		ChoosePrgMemoryType(HasBattery, PrgSaveRam, PrgWorkRam),
		ChooseMemoryAccessType(ramPermission = $A3, maReadWrite, maNoAccess));
end;

procedure TMapper_TaitoX1005.WriteRegister(addr: Word; value: Byte);
begin
	case addr of

		$7EF0:
		begin
			SelectCHRPage(0, value);
			SelectCHRPage(1, value+1);
			if alternateMirroring then
			begin
				SetNametable(0, value shr 7);
				SetNametable(1, value shr 7);
			end;
		end;

		$7EF1:
		begin
			SelectCHRPage(2, value);
			SelectCHRPage(3, value+1);
			if alternateMirroring then
			begin
				SetNametable(2, value shr 7);
				SetNametable(3, value shr 7);
			end;
		end;

		$7EF2: SelectCHRPage(4, value);
		$7EF3: SelectCHRPage(5, value);
		$7EF4: SelectCHRPage(6, value);
		$7EF5: SelectCHRPage(7, value);

		$7EF6, $7EF7:
		begin
			if not alternateMirroring then
				ApplyMirroringType(Odd(value),
					MIRROR_VERTICAL, MIRROR_HORIZONTAL);
		end;

		$7EF8, $7EF9:
		begin
			ramPermission := value;
			UpdateRamAccess;
		end;

		$7EFA, $7EFB: SelectPRGPage(0, value);
		$7EFC, $7EFD: SelectPRGPage(1, value);
		$7EFE, $7EFF: SelectPRGPage(2, value);
	end;
end;

procedure TMapper_TaitoX1005.LoadSnapshot;
begin
	inherited;
	UpdateRamAccess;
end;

{ TMapper_207 }

constructor TMapper_207.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);
	alternateMirroring := True;
end;

end.
