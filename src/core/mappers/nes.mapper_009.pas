unit NES.Mapper_009;

// Mapper 009: MMC2, PNROM, PEEOROM

interface

uses
	NES.Types, NES.Mapper, NES.Cartridge;

type
	TMMC2Registers = (
		RegA000 = $A,
		RegB000 = $B,
		RegC000 = $C,
		RegD000 = $D,
		RegE000 = $E,
		RegF000 = $F
	);

	TMapper_009 = class(TMapper)
	protected
		leftLatch,   rightLatch:   Byte;
		leftChrPage, rightChrPage: array [0..1] of Byte;
		NeedChrUpdate: Boolean;

		procedure InitMapper; override;
		procedure WriteRegister(address: Word; value: Byte); override;

		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;
	public
		procedure NotifyVRAMAddressChange(addr: Word); override;

		constructor Create(cartridge: TCartridge); override;
	end;

implementation

uses
	SysUtils,
	NES.Config, NES.Console, NES.CPU;

{ TMapper_009 }

constructor TMapper_009.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(8, @leftLatch);
	RegisterProperty(8, @rightLatch);
	RegisterProperty(8, @leftChrPage[0]);
	RegisterProperty(8, @leftChrPage[1]);
	RegisterProperty(8, @rightChrPage[0]);
	RegisterProperty(8, @rightChrPage[1]);
	RegisterProperty(1, @NeedChrUpdate);
end;

function TMapper_009.GetPRGPageSize: Word;
begin
	Result := $2000;
end;

function TMapper_009.GetCHRPageSize: Word;
begin
	Result := $1000;
end;

procedure TMapper_009.InitMapper;
begin
	leftLatch  := 1;
	rightLatch := 1;
	leftChrPage[0]  := GetPowerOnByte and $1F;
	leftChrPage[1]  := GetPowerOnByte and $1F;
	rightChrPage[0] := GetPowerOnByte and $1F;
	rightChrPage[1] := GetPowerOnByte and $1F;
	NeedChrUpdate := False;

	SelectPRGPage(1, -3);
	SelectPRGPage(2, -2);
	SelectPRGPage(3, -1);
end;

procedure TMapper_009.NotifyVRAMAddressChange(addr: Word);
begin
	if NeedChrUpdate then
	begin
		SelectCHRPage(0, leftChrPage[leftLatch]);
		SelectCHRPage(1, rightChrPage[rightLatch]);
		NeedChrUpdate := False;
	end;
	case addr of
		$0FD8:        leftLatch  := 0;
		$0FE8:        leftLatch  := 1;
		$1FD8..$1FDF: rightLatch := 0;
		$1FE8..$1FEF: rightLatch := 1;
	    else Exit;
	end;
	NeedChrUpdate := True;
end;

procedure TMapper_009.WriteRegister(address: Word; value: Byte);
begin
	case TMMC2Registers(address >> 12) of

		RegA000:
			SelectPRGPage(0, value and $0F);

		RegB000:
		begin
			leftChrPage[0] := value and $1F;
			SelectCHRPage(0, leftChrPage[leftLatch]);
		end;

		RegC000:
		begin
			leftChrPage[1] := value and $1F;
			SelectCHRPage(0, leftChrPage[leftLatch]);
		end;

		RegD000:
		begin
			rightChrPage[0] := value and $1F;
			SelectCHRPage(1, rightChrPage[rightLatch]);
		end;

		RegE000:
		begin
			rightChrPage[1] := value and $1F;
			SelectCHRPage(1, rightChrPage[rightLatch]);
		end;

		RegF000:
			ApplyMirroringType(Odd(value), MIRROR_HORIZONTAL, MIRROR_VERTICAL);

	end;
end;


end.

