unit NES.Mapper_010;

// Mapper 010: MMC4

interface

uses
	NES.Mapper, NES.Mapper_009;

type
	TMapper_010 = class(TMapper_009)
	protected
		procedure InitMapper; override;

		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;
	public
		procedure NotifyVRAMAddressChange(addr: Word); override;
	end;

implementation

uses
	SysUtils,
	NES.Config, NES.Console, NES.CPU;

{ TMapper_010 }

function TMapper_010.GetPRGPageSize: Word;
begin
	Result := $4000;
end;

function TMapper_010.GetCHRPageSize: Word;
begin
	Result := $1000;
end;

procedure TMapper_010.InitMapper;
begin
	leftLatch  := 1;
	rightLatch := 1;
	leftChrPage[0]  := GetPowerOnByte and $1F;
	leftChrPage[1]  := GetPowerOnByte and $1F;
	rightChrPage[0] := GetPowerOnByte and $1F;
	rightChrPage[1] := GetPowerOnByte and $1F;
	NeedChrUpdate := False;

	SelectPRGPage(1, -1);
end;

procedure TMapper_010.NotifyVRAMAddressChange(addr: Word);
begin
	if needChrUpdate then
	begin
		SelectCHRPage(0, leftChrPage[leftLatch]);
		SelectCHRPage(1, rightChrPage[rightLatch]);
		needChrUpdate := False;
	end;

	case addr of

		$0FD8..$0FDF:
		begin
			leftLatch := 0;
			needChrUpdate := True;
		end;

		$0FE8..$0FEF:
		begin
			leftLatch := 1;
			needChrUpdate := True;
		end;

		$1FD8..$1FDF:
		begin
			rightLatch := 0;
			needChrUpdate := True;
		end;

		$1FE8..$1FEF:
		begin
			rightLatch := 1;
			needChrUpdate := True;
		end;

	end;
end;


end.

