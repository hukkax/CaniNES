unit NES.Mapper_244;

// Mapper 244

interface

uses
	NES.Types, NES.Mapper;

const
	lutPrg: array [0..3, 0..3] of Byte = (
		( 0, 1, 2, 3 ),
		( 3, 2, 1, 0 ),
		( 0, 2, 1, 3 ),
		( 3, 1, 2, 0 )
	);

	lutChr: array [0..7, 0..7] of Byte = (
		( 0, 1, 2, 3, 4, 5, 6, 7 ),
		( 0, 2, 1, 3, 4, 6, 5, 7 ),
		( 0, 1, 4, 5, 2, 3, 6, 7 ),
		( 0, 4, 1, 5, 2, 6, 3, 7 ),
		( 0, 4, 2, 6, 1, 5, 3, 7 ),
		( 0, 2, 4, 6, 1, 3, 5, 7 ),
		( 7, 6, 5, 4, 3, 2, 1, 0 ),
		( 7, 6, 5, 4, 3, 2, 1, 0 )
	);

type
	TMapper_244 = class(TMapper)
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;

		procedure WriteRegister(addr: Word;  value: Byte); override;

		procedure InitMapper; override;
	end;


implementation

{ TMapper_244 }

function TMapper_244.GetPRGPageSize: Word; begin Result := $8000; end;
function TMapper_244.GetCHRPageSize: Word; begin Result := $2000; end;

procedure TMapper_244.InitMapper;
begin
	SelectPrgPage(0, 0);
	SelectChrPage(0, 0);
end;

procedure TMapper_244.WriteRegister(addr: Word; value: Byte);
begin
	if (value and 8) <> 0 then
		SelectChrPage(0, lutChr[(value shr 4) and 7][value and 7])
	else
		SelectPrgPage(0, lutPrg[(value shr 4) and 3][value and 3]);
end;


end.
