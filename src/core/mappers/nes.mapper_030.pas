unit NES.Mapper_030;

// Mapper 030: UNROM512

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper,
	NES.Flash.SST39SF040;

type
	TMapper_030 = class(TMapper_SST39SF040)
	private
		enableMirroringBit: Boolean;
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;
		function  GetWorkRamSize: Cardinal; override;
		function  GetSaveRamSize: Cardinal; override;
		function  GetChrRamSize:  Cardinal; override;

		function  RegisterStartAddress: Word; override;
		function  RegisterEndAddress:   Word; override;

		function  HasBusConflicts:   Boolean; override;
		function  AllowRegisterRead: Boolean; override;

		procedure WriteRegister(addr: Word;  value: Byte); override;

		procedure InitMapper; override;
	public
	end;


implementation

{ TMapper_030 }

function TMapper_030.GetPRGPageSize: Word;       begin Result := $4000; end;
function TMapper_030.GetCHRPageSize: Word;       begin Result := $2000; end;
function TMapper_030.GetWorkRamSize: Cardinal;   begin Result := 0;     end;
function TMapper_030.GetSaveRamSize: Cardinal;   begin Result := 0;     end;
function TMapper_030.GetChrRamSize:  Cardinal;   begin Result := $8000; end;
function TMapper_030.RegisterStartAddress: Word; begin Result := $8000; end;
function TMapper_030.RegisterEndAddress:   Word; begin Result := $FFFF; end;
function TMapper_030.HasBusConflicts:   Boolean; begin Result := not HasBattery; end;
function TMapper_030.AllowRegisterRead: Boolean; begin Result := HasBattery;     end;

(*
if(saving) then
begin
	vector<uint8_t> prgRom := vector<uint8_t>(_prgRom, _prgRom + _prgSize);
	vector<uint8_t> ipsData := IpsPatcher.CreatePatch(_orgPrgRom, prgRom);
	VectorInfo<uint8_t> data begin &ipsData end;;
	Stream(data);
end
else
begin
	vector<uint8_t> ipsData;
	VectorInfo<uint8_t> data begin &ipsData end;;
	Stream(data);

	vector<uint8_t> patchedPrgRom;
	if(IpsPatcher.PatchBuffer(ipsData, _orgPrgRom, patchedPrgRom)) then begin
		memcpy(_prgRom, patchedPrgRom.data, _prgSize);
	end;
end;
*)

procedure TMapper_030.InitMapper;
begin
	FlashInit(@prgRom[0], prgSize);

	SelectPRGPage(0,  0);
	SelectPRGPage(1, -1);

	enableMirroringBit := False;

	if GetMirroringType in [MIRROR_SCREENAONLY, MIRROR_SCREENBONLY] then
	begin
		SetMirroringType(MIRROR_SCREENAONLY);
		enableMirroringBit := True;
	end
	else
	case (RomInfo.NesHeader.Byte6 and $09) of
		0:       SetMirroringType(MIRROR_HORIZONTAL);
		1:       SetMirroringType(MIRROR_VERTICAL);
		8: begin SetMirroringType(MIRROR_SCREENAONLY); enableMirroringBit := True; end;
		9:       SetMirroringType(MIRROR_FOURSCREENS);
	end;

	if (GetMirroringType = MIRROR_FOURSCREENS) and (HasChrRam) and (chrRamSize >= $8000) then
	begin
		// InfiniteNesLives four-screen mirroring variation, last 8kb of CHR RAM is
		// always mapped to $2000-$3FFF ($3EFF due to palette)
		// This "breaks" the "UNROM512_4screen_test" test ROM - was the ROM actually
		// tested on this board? Seems to contradict hardware specs
		SetPpuMemoryMapping($2000, $3FFF, TChrMemoryType.ChrRam, $6000, Ord(maReadWrite));
	end;

	if HasBattery then
	begin
		AddRegisterRange($8000, $FFFF, moRead);
		orgPrgRom := prgRom;
		ApplySaveData;
	end;
end;

procedure TMapper_030.WriteRegister(addr: Word; value: Byte);
begin
	if (not HasBattery) or (addr >= $C000) then
	begin
		SelectPRGPage(0, value and $1F);
		prgByte := value and $1F;

		SelectCHRPage(0, (value shr 5) and $03);

		if enableMirroringBit then
			ApplyMirroringType(value and $80, MIRROR_SCREENBONLY, MIRROR_SCREENAONLY);
	end
	else
		FlashWrite((addr and $3FFF) or (prgByte shl 14), value);
end;


end.

