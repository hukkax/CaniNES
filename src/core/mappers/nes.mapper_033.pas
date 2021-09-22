unit NES.Mapper_033;

// Mapper 033: Taito TC0190

interface

uses
	NES.Types, NES.Mapper;

type
	TMapper_033 = class(TMapper)
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;

		procedure InitMapper; override;
		procedure WriteRegister(addr: Word;  value: Byte); override;
	end;


implementation

{ TMapper_033 }

function TMapper_033.GetPRGPageSize: Word; begin Result := $2000; end;
function TMapper_033.GetCHRPageSize: Word; begin Result := $0400; end;

procedure TMapper_033.InitMapper;
begin
	SelectPRGPage(2, -2);
	SelectPRGPage(3, -1);
end;

procedure TMapper_033.WriteRegister(addr: Word; value: Byte);
begin
	case (addr and $A003) of

		$8000:
		begin
			SelectPRGPage(0, value and $3F);
			ApplyMirroringType(value and $40 = $40, MIRROR_HORIZONTAL, MIRROR_VERTICAL);
		end;

		$8001:
			SelectPRGPage(1, value and $3F);

		$8002:
		begin
			SelectCHRPage(1, value * 2 + 1);
			SelectCHRPage(0, value * 2);
		end;

		$8003:
		begin
			SelectCHRPage(2, value * 2);
			SelectCHRPage(3, value * 2 + 1);
		end;

		$A000..$A003:
			SelectCHRPage(4 + (addr and $03), value);

	end;
end;

end.
