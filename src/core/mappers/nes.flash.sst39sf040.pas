unit NES.Flash.SST39SF040;

// SST39SF040 chip emulation - used by mappers 30 (UNROM512) and 111 (GTROM)

interface

uses
	Types, Classes, SysUtils,
	NES.Types, NES.Cartridge, NES.Mapper, NES.SaveState;

type
	TChipMode = ( WaitingForCommand, Write, Erase );

	TMapper_SST39SF040 = class(TMapper)
	private
		mode: TChipMode;
		cycle: Byte;
		softwareId: Boolean;

		data: PByte;
		size: Cardinal;
	protected
		prgByte: Byte;

		orgPrgRom: TBytes;

		function  ReadRegister(addr: Word): Byte; override;

		procedure FlashInit(dataptr: PByte; datasize: Cardinal);
		function  FlashRead(addr: Cardinal): Int16;
		procedure FlashWrite(addr: Cardinal; value: Byte);
		procedure FlashReset;

		procedure ApplySaveData;
	public
		procedure SaveBattery; override;

		constructor Create(cartridge: TCartridge); override;
	end;


implementation

uses TextOutput;

// ============================================================================
// TMapper_SST39SF040
// ============================================================================

constructor TMapper_SST39SF040.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(8, @prgByte);
	RegisterProperty(8, @mode);
	RegisterProperty(8, @cycle);
	RegisterProperty(1, @softwareId);
end;

procedure TMapper_SST39SF040.FlashInit(dataptr: PByte; datasize: Cardinal);
begin
	FlashReset;

	data := dataptr;
	size := datasize;
end;

procedure TMapper_SST39SF040.ApplySaveData;
begin
	Log('Flash.ApplySaveData');
	{
	// Apply save data (saved as an IPS file), if found
	vector<uint8_t> ipsData := _console.GetBatteryManager.LoadBattery(".ips");
	if(not ipsData.empty) then begin
		vector<uint8_t> patchedPrgRom;
		if(IpsPatcher.PatchBuffer(ipsData, _orgPrgRom, patchedPrgRom)) then
			memcpy(_prgRom, patchedPrgRom.data, _prgSize);
	end;
	}
end;

procedure TMapper_SST39SF040.SaveBattery;
//var
//	ipsData: TBytes;
begin
	if HasBattery then
	begin
		Log('Flash.SaveBattery');
	{
		vector<uint8_t> prgRom := vector<uint8_t>(_prgRom, _prgRom + _prgSize);
		vector<uint8_t> ipsData := IpsPatcher.CreatePatch(_orgPrgRom, prgRom);
		ipsData := CreateIpsPatch;
		if Length(ipsData) > 8 then
			DoSaveBattery(battIPS, ipsData, Length(ipsData));
	}
	end;
end;

function TMapper_SST39SF040.ReadRegister(addr: Word): Byte;
var
	value: Int16;
begin
	value := FlashRead(addr);
	if value >= 0 then
		Result := value and $FF
	else
		Result := InternalReadRam(addr);
end;

// ============================================================================

procedure TMapper_SST39SF040.FlashReset;
begin
	mode := TChipMode.WaitingForCommand;
	cycle := 0;
end;

function TMapper_SST39SF040.FlashRead(addr: Cardinal): Int16;
begin
exit(-1);
	if softwareId then
	case (addr and $1FF) of
		$00: Result := $BF;
		$01: Result := $B7;
		else Result := $FF;
	end
	else
		Result := -1;
end;

procedure TMapper_SST39SF040.FlashWrite(addr: Cardinal; value: Byte);
var
	cmd: Word;
	offset: Cardinal;
begin
	cmd := addr and $7FFF;

	case mode of

		TChipMode.WaitingForCommand:
		begin
			if cycle = 0 then
			begin
				if (cmd = $5555) and (value = $AA) then
				begin
					// 1st write, $5555 := $AA
					Inc(cycle);
				end
				else
				if value = $F0 then
				begin
					// Software ID exit
					FlashReset;
					softwareId := False;
				end;
			end
			else
			if (cycle = 1) and (cmd = $2AAA) and (value = $55) then
			begin
				// 2nd write, $2AAA := $55
				Inc(cycle);
			end
			else
			if (cycle = 2) and (cmd = $5555) then
			begin
				// 3rd write, determines command type
				Inc(cycle);
				case value of
					$80: begin mode := TChipMode.Erase; end;
					$90: begin FlashReset; softwareId := True; end;
					$A0: begin mode := TChipMode.Write; end;
					$F0: begin FlashReset; softwareId := False; end;
				end;
			end
			else
				cycle := 0;
		end;

		TChipMode.Write:
		begin
			// Write a single byte
			if addr < size then
				data[addr] := data[addr] and value;
			FlashReset;
		end;

		TChipMode.Erase:
		begin
			if cycle = 3 then
			begin
				// 4th write for erase command, $5555 := $AA
				if (cmd = $5555) and (value = $AA) then
					Inc(cycle)
				else
					FlashReset;
			end
			else
			if cycle = 4 then
			begin
				// 5th write for erase command, $2AAA := $55
				if (cmd = $2AAA) and (value = $55) then
					Inc(cycle)
				else
					FlashReset;
			end
			else
			if cycle = 5 then
			begin
				if (cmd = $5555) and (value = $10) then
				begin
					// Chip erase
					FillByte(data[0], size, $FF);
				end
				else
				if value = $30 then
				begin
					// Sector erase
					offset := addr and $7F000;
					if (offset + $1000) <= size then
						FillByte(data[offset], $1000, $FF);
				end;
				FlashReset;
			end;
		end;

	end;
end;

end.

