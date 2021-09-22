unit NES.Mapper_060;

// Mapper 060: multicart: Reset Based 4-in-1

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper;

type
	TMapper_060 = class(TMapper)
	private
		resetCounter: Byte;
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;

		procedure InitMapper; override;
	public
		procedure Reset(SoftReset: Boolean); override;

		constructor Create(cartridge: TCartridge); override;
	end;


implementation

{ TMapper_060 }

function TMapper_060.GetPRGPageSize: Word;
begin
	Result := $4000;
end;

function TMapper_060.GetCHRPageSize: Word;
begin
	Result := $2000;
end;

constructor TMapper_060.Create(cartridge: TCartridge);
begin
	inherited Create(cartridge);

	RegisterProperty(8, @resetCounter);
end;

procedure TMapper_060.InitMapper;
begin
	resetCounter := 0;
	SelectPRGPage(0, 0);
	SelectPRGPage(1, 0);
	SelectCHRPage(0, 0);
end;

procedure TMapper_060.Reset(SoftReset: Boolean);
begin
	if SoftReset then
	begin
		resetCounter := (resetCounter + 1) mod 4;
		SelectPRGPage(0, resetCounter);
		SelectPRGPage(1, resetCounter);
		SelectCHRPage(0, resetCounter);
	end;
end;

end.

