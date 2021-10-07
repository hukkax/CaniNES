unit NES.TXCChip;

{$mode Delphi}

interface

uses
	Classes, SysUtils,
	NES.Types, NES.SaveState;

type
	TTXCChip = class(TSnapshotable)
	private
		mask,
		accumulator,
		inverter,
		staging,
		output: Byte;

		isJv001,
		increase,
		yFlag,
		invert: Boolean;

	public
		function  GetInvertFlag: Boolean;
		function  GetY: Boolean;
		function  GetOutput: Byte;
		function  Read: Byte;
		procedure Write(addr: Word; value: Byte);

		constructor Create(Is_Jv001: Boolean);
	end;

implementation

uses Math;

{ TTXCChip }

constructor TTXCChip.Create(Is_Jv001: Boolean);
begin
	inherited Create('TXC');

	accumulator := 0;
	inverter := 0;
	staging := 0;
	output := 0;

	increase := False;
	yFlag := False;

	isJv001 := is_Jv001;
	mask   := IfThen(is_Jv001, $0F, $07);
	invert := is_Jv001;

	RegisterProperty(8, @accumulator);
	RegisterProperty(1, @invert);
	RegisterProperty(8, @inverter);
	RegisterProperty(8, @staging);
	RegisterProperty(8, @output);
	RegisterProperty(1, @increase);
	RegisterProperty(1, @yFlag);
end;

function TTXCChip.GetInvertFlag: Boolean;
begin
	Result := invert;
end;

function TTXCChip.GetY: Boolean;
begin
	Result := yFlag;
end;

function TTXCChip.GetOutput: Byte;
begin
	Result := output;
end;

function TTXCChip.Read: Byte;
begin
	Result := (accumulator and mask) or ((inverter xor (IfThen(invert, $FF, 0))) and (not mask));
	yFlag := (not invert) or ((Result and $10) <> 0);
end;

procedure TTXCChip.Write(addr: Word; value: Byte);
begin
	if(addr < $8000) then
	begin
		case (addr and $E103) of

			$4100:
				if increase then
					Inc(accumulator)
				else
					accumulator := ((accumulator and (not mask)) or
						(staging and mask)) xor (IfThen(invert, $FF, 0));

			$4101:
				invert := Odd(value);

			$4102:
			begin
				staging  := value and mask;
				inverter := value and (not mask);
			end;

			$4103:
				increase := Odd(value);

		end;
	end
	else
	begin
		if isJv001 then
			output := (accumulator and $0F) or (inverter and $F0)
		else
			output := (accumulator and $0F) or ((inverter and $08) shl 1);
	end;

	yFlag := (not invert) or ((value and $10) <> 0);
end;

end.

