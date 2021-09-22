unit NES.BaseEeprom24C0X;

{$mode Delphi}

interface

uses
	Classes, SysUtils,
	NES.Types, NES.Console, NES.SaveState;

type
	TEepromMode = (
		ermIdle = 0,
		ermAddress,
		ermRead,
		ermWrite,
		ermSendAck,
		ermWaitAck,
		ermChipAddress
	);

	TEeprom24C0X = class(TSnapshotable)
	protected
		Mode, NextMode,
		chipAddress, address,
		data, counter, output,
		prevScl, prevSda: Byte;

		romData: array[Byte] of Byte;

	public
		function  Read: Byte; virtual;

		procedure Write(scl, sda: Byte); virtual;
		procedure WriteScl(scl: Byte);
		procedure WriteSda(sda: Byte);

		procedure SaveBattery; virtual;

		constructor Create;
	end;

implementation

{ TEeprom24C0X }

constructor TEeprom24C0X.Create;
begin
	inherited Create('EEPROM');

	RegisterProperty(8, @Mode);
	RegisterProperty(8, @NextMode);
	RegisterProperty(8, @chipAddress);
	RegisterProperty(8, @address);
	RegisterProperty(8, @data);
	RegisterProperty(8, @counter);
	RegisterProperty(8, @output);
	RegisterProperty(8, @prevScl);
	RegisterProperty(8, @prevSda);

	RegisterArray(Length(romData), @romData[0]);
end;

function TEeprom24C0X.Read: Byte;
begin
	Result := output;
end;

procedure TEeprom24C0X.Write(scl, sda: Byte);
begin
	//
end;

procedure TEeprom24C0X.WriteScl(scl: Byte);
begin
	Write(scl, prevSda);
end;

procedure TEeprom24C0X.WriteSda(sda: Byte);
begin
	Write(prevScl, sda);
end;

procedure TEeprom24C0X.SaveBattery;
begin
	//
end;


end.

