unit NES.Mapper_VRC6;

interface

uses
	NES.Cartridge, NES.Mapper, NES.Types,
	NES.VRC_IRQ, NES.APU.VRC6;

type
	TMapper_VRC6 = class(TMapper)
	private
		IRQ: TVrcIrq;
		Audio: TVrc6Audio;

		model: TVRCVariant;
		bankingMode: Byte;
		chrRegisters: array [0..7] of Byte;

		procedure UpdatePrgRamAccess;
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;

		procedure InitMapper; override;

		procedure SetPpuMapping(bank, page: Byte);
		procedure UpdatePpuBanking;
		procedure WriteRegister(address: Word; value: Byte); override;
	public
		procedure ProcessCpuClock; override;

		procedure LoadSnapshot; override;
		procedure SaveSnapshot; override;

		function  GetSoundChipName: AnsiString; override;

		constructor Create(cartridge: TCartridge); override;
		destructor  Destroy; override;
	end;

	TMapper_VRC6a = class(TMapper_VRC6)
	end;

	TMapper_VRC6b = class(TMapper_VRC6)
	public
		constructor Create(cartridge: TCartridge); override;
	end;

implementation

uses
	SysUtils, Math, Basement.Util,
	NES.Config;

{ TMapper_VRC6b }

constructor TMapper_VRC6b.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	model := VRC6b;
end;

// ============================================================================
// TMapper_VRC6
// ============================================================================

constructor TMapper_VRC6.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(8, @bankingMode);
	RegisterArray(Length(chrRegisters), @chrRegisters[0]);

	IRQ := TVrcIrq.Create;
	Audio := TVrc6Audio.Create;

	model := VRC6a;
end;

destructor TMapper_VRC6.Destroy;
begin
	Audio.Free;
	IRQ.Free;

	inherited Destroy;
end;

function TMapper_VRC6.GetPRGPageSize: Word; begin Result := $2000; end;
function TMapper_VRC6.GetCHRPageSize: Word; begin Result := $0400; end;
function TMapper_VRC6.GetSoundChipName: AnsiString; begin Result := 'Konami VRC6'; end;

procedure TMapper_VRC6.InitMapper;
begin
	IRQ.Reset;
	Audio.Reset;

	ZeroMemory(@chrRegisters[0], Length(chrRegisters));
	SelectPRGPage(3, -1);
end;

procedure TMapper_VRC6.UpdatePrgRamAccess;
var
	PMT: TPrgMemoryType;
	MAT: TMemoryAccessType;
begin
	if HasBattery then
		PMT := TPrgMemoryType.PrgSaveRam
	else
		PMT := TPrgMemoryType.PrgWorkRam;

	if (bankingMode and $80) <> 0 then
		MAT := TMemoryAccessType.maReadWrite
	else
		MAT := TMemoryAccessType.maNoAccess;

	SetCpuMemoryMapping($6000, $7FFF, 0, PMT, MAT);
end;

procedure TMapper_VRC6.SetPpuMapping(bank, page: Byte);
begin
	SetPpuMemoryMapping($2000 + bank * $400, $23FF + bank * $400, page);
	SetPpuMemoryMapping($3000 + bank * $400, $33FF + bank * $400, page);
end;

procedure TMapper_VRC6.UpdatePpuBanking;
var
	mask, orMask: Byte;
begin
	mask   := IfThen((bankingMode and $20) <> 0, $FE, $FF);
	orMask := IfThen((bankingMode and $20) <> 0, 1, 0);

	case (bankingMode and $03) of
		0:
		begin
			SelectCHRPage(0, chrRegisters[0]);
			SelectCHRPage(1, chrRegisters[1]);
			SelectCHRPage(2, chrRegisters[2]);
			SelectCHRPage(3, chrRegisters[3]);
			SelectCHRPage(4, chrRegisters[4]);
			SelectCHRPage(5, chrRegisters[5]);
			SelectCHRPage(6, chrRegisters[6]);
			SelectCHRPage(7, chrRegisters[7]);
		end;
		1:
		begin
			SelectCHRPage(0,  chrRegisters[0] and mask);
			SelectCHRPage(1, (chrRegisters[0] and mask) or orMask);
			SelectCHRPage(2,  chrRegisters[1] and mask);
			SelectCHRPage(3, (chrRegisters[1] and mask) or orMask);
			SelectCHRPage(4,  chrRegisters[2] and mask);
			SelectCHRPage(5, (chrRegisters[2] and mask) or orMask);
			SelectCHRPage(6,  chrRegisters[3] and mask);
			SelectCHRPage(7, (chrRegisters[3] and mask) or orMask);
		end;
		2, 3:
		begin
			SelectCHRPage(0,  chrRegisters[0]);
			SelectCHRPage(1,  chrRegisters[1]);
			SelectCHRPage(2,  chrRegisters[2]);
			SelectCHRPage(3,  chrRegisters[3]);
			SelectCHRPage(4,  chrRegisters[4] and mask);
			SelectCHRPage(5, (chrRegisters[4] and mask) or orMask);
			SelectCHRPage(6,  chrRegisters[5] and mask);
			SelectCHRPage(7, (chrRegisters[5] and mask) or orMask);
		end;
	end;

	if (bankingMode and $10) <> 0 then
	begin
		// CHR ROM nametables
		case (bankingMode and $2F) of

			$20, $27:
			begin
				SetPpuMapping(0,  chrRegisters[6] and $FE);
				SetPpuMapping(1, (chrRegisters[6] and $FE) or 1);
				SetPpuMapping(2,  chrRegisters[7] and $FE);
				SetPpuMapping(3, (chrRegisters[7] and $FE) or 1);
			end;

			$23, $24:
			begin
				SetPpuMapping(0, (chrRegisters[6] and $FE));
				SetPpuMapping(1, (chrRegisters[7] and $FE));
				SetPpuMapping(2, (chrRegisters[6] and $FE) or 1);
				SetPpuMapping(3, (chrRegisters[7] and $FE) or 1);
			end;

			$28, $2F:
			begin
				SetPpuMapping(0, chrRegisters[6] and $FE);
				SetPpuMapping(1, chrRegisters[6] and $FE);
				SetPpuMapping(2, chrRegisters[7] and $FE);
				SetPpuMapping(3, chrRegisters[7] and $FE);
			end;

			$2B, $2C:
			begin
				SetPpuMapping(0, (chrRegisters[6] and $FE) or 1);
				SetPpuMapping(1, (chrRegisters[7] and $FE) or 1);
				SetPpuMapping(2, (chrRegisters[6] and $FE) or 1);
				SetPpuMapping(3, (chrRegisters[7] and $FE) or 1);
			end;

			else
				case (bankingMode and $07) of
					0, 6, 7:
					begin
						SetPpuMapping(0, chrRegisters[6]);
						SetPpuMapping(1, chrRegisters[6]);
						SetPpuMapping(2, chrRegisters[7]);
						SetPpuMapping(3, chrRegisters[7]);
					end;
					1, 5:
					begin
						SetPpuMapping(0, chrRegisters[4]);
						SetPpuMapping(1, chrRegisters[5]);
						SetPpuMapping(2, chrRegisters[6]);
						SetPpuMapping(3, chrRegisters[7]);
					end;
					2, 3, 4:
					begin
						SetPpuMapping(0, chrRegisters[6]);
						SetPpuMapping(1, chrRegisters[7]);
						SetPpuMapping(2, chrRegisters[6]);
						SetPpuMapping(3, chrRegisters[7]);
					end;
				end;
				// (bankingMode and $07)
			end;
			// (bankingMode and $2F)
	end
	else
	begin
		// Regular nametables (CIRAM)
		case (bankingMode and $2F) of
			$20, $27: SetMirroringType(TMirroringType.MIRROR_VERTICAL);
			$23, $24: SetMirroringType(TMirroringType.MIRROR_HORIZONTAL);
			$28, $2F: SetMirroringType(TMirroringType.MIRROR_SCREENAONLY);
			$2B, $2C: SetMirroringType(TMirroringType.MIRROR_SCREENBONLY);
			else
				case (bankingMode and $07) of
					0,6,7:
					begin
						SetNametable(0, chrRegisters[6] and $01);
						SetNametable(1, chrRegisters[6] and $01);
						SetNametable(2, chrRegisters[7] and $01);
						SetNametable(3, chrRegisters[7] and $01);
					end;
					1,5:
					begin
						SetNametable(0, chrRegisters[4] and $01);
						SetNametable(1, chrRegisters[5] and $01);
						SetNametable(2, chrRegisters[6] and $01);
						SetNametable(3, chrRegisters[7] and $01);
					end;
					2,3,4:
					begin
						SetNametable(0, chrRegisters[6] and $01);
						SetNametable(1, chrRegisters[7] and $01);
						SetNametable(2, chrRegisters[6] and $01);
						SetNametable(3, chrRegisters[7] and $01);
					end;
				end;
		end;
	end;

	UpdatePrgRamAccess;
end;

procedure TMapper_VRC6.WriteRegister(address: Word; value: Byte);
begin
	if model = VRC6b then
		address := (address and $FFFC) or
			((address and $01) shl 1)  or
			((address and $02) shr 1);

	case (address and $F003) of

		$8000..$8003:	SelectPrgPage2x(0, (value and $0F) shl 1);

		$9000..$9003,
		$A000..$A002,
		$B000..$B002:	Audio.WriteRegister(address, value);

		$B003:			begin
						bankingMode := value;
						UpdatePpuBanking;
						end;

		$C000..$C003:	SelectPRGPage(2, value and $1F);

		$D000..$D003:	begin
						chrRegisters[address and $03] := value;
						UpdatePpuBanking;
						end;

		$E000..$E003:	begin
						chrRegisters[4 + (address and $03)] := value;
						UpdatePpuBanking;
						end;

		$F000:			IRQ.SetReloadValue(value);
		$F001:			IRQ.SetControlValue(value);
		$F002:			IRQ.AcknowledgeIrq;
	end;

end;

procedure TMapper_VRC6.ProcessCpuClock;
begin
	IRQ.ProcessCpuClock;
	Audio.Clock;
end;

procedure TMapper_VRC6.LoadSnapshot;
begin
	inherited LoadSnapshot;

	IRQ.LoadSnapshot;
	Audio.LoadSnapshot;

	UpdatePrgRamAccess;
	UpdatePpuBanking;
end;

procedure TMapper_VRC6.SaveSnapshot;
begin
	inherited SaveSnapshot;

	IRQ.SaveSnapshot;
	Audio.SaveSnapshot;
end;


end.

