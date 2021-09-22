unit NES.Mapper_118;

// TKSROM and TLSROM

interface

uses
	NES.Types, NES.Mapper, NES.Mapper_004;

type
	TMapper_118 = class(TMapper_004)
	protected
		procedure UpdateMirroring; override;
		procedure WriteRegister(addr: Word;  value: Byte); override;
	end;


implementation

{ TMapper_118 }

procedure TMapper_118.UpdateMirroring;
begin
	// This is disabled, 8001 writes are used to setup mirroring instead
end;

procedure TMapper_118.WriteRegister(addr: Word; value: Byte);
var
	nametable: Byte;
begin
	if (addr and $E001) = $8001 then
	begin
		nametable := value shr 7;

		if ChrMode = 0 then
		begin
			case CurrentRegister of
				0:
				begin
					SetNametable(0, nametable);
					SetNametable(1, nametable);
				end;
				1:
				begin
					SetNametable(2, nametable);
					SetNametable(3, nametable);
				end;
			end;
		end
		else
		begin
			case CurrentRegister of
				2: SetNametable(0, nametable);
				3: SetNametable(1, nametable);
				4: SetNametable(2, nametable);
				5: SetNametable(3, nametable);
			end;
		end;
	end;

	inherited WriteRegister(addr, value);
end;

end.
