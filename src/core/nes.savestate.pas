unit NES.SaveState;

{$MODE DELPHI}

{$I basement.inc}

interface

uses
	Classes, SysUtils,
	Generics.Collections,
	FileStreamEx;

const
	BoolByte: array[Boolean] of Byte = (0, 1);

	SAVESLOT_TEMP = 255;

type
	TSnapshotProperty = class
		DataSize: Cardinal;
		IsArray:  Boolean;
		Data:     Pointer;
	end;

	TStateFileInfo = record
		Exists:   Boolean;
		Filename: String;
		DateTime: TDateTime;
	end;

	TSnapshotable = class
	class var
		Saving: Boolean;
		Stream: TStream;
	private
		RegisteredName: AnsiString;
		RegisteredProperties: TObjectList<TSnapshotProperty>;
	protected
		procedure RegisterProperty(Size: Integer; Data: Pointer);
		procedure RegisterArray(Size: Word; Data: Pointer);
	public
		constructor Create(const RegisterName: AnsiString); virtual; overload;
		destructor  Destroy; override;

		function  AssertSection(const Section: AnsiString): Boolean;
		function  StartSection(const Section: AnsiString): Boolean;

		procedure LoadSnapshot; virtual;
		procedure SaveSnapshot; virtual;
	end;

	TSaveStateManager = class
	const
		FileFormatVersion = 13;
		MaxIndex = 9;
	private
		Stream: TStream;
		Path: String;
	public
		Index: Byte;
		StateFileInfo: array[0..MaxIndex] of TStateFileInfo;

		procedure SetPath(const DirectoryPath: String);
		function  GetPath: String;
		function  GetStateFileName(SlotIndex: Byte): String;
		procedure UpdateStateAvailability;

		procedure DisplaySaveSlots;

		procedure SelectSaveSlot(SlotIndex: Byte; ShowOSD: Boolean = True);
		procedure MoveToNextSlot(ShowOSD: Boolean = True);
		procedure MoveToPreviousSlot(ShowOSD: Boolean = True);

		function  LoadState(const Filename: String):  Boolean; overload;
		function  LoadState(SlotIndex: Integer = -1): Boolean; overload;
		function  LoadState(AStream: TStream): Boolean; overload;

		function  SaveState(const Filename: String):  Boolean; overload;
		function  SaveState(SlotIndex: Integer = -1): Boolean; overload;
		function  SaveState(AStream: TStream): Boolean; overload;
	end;


implementation

uses
	Basement.Util, Math,
	Graphics32, TextOutput,	{$IFDEF RENDER_FONT_FREETYPE}Graphics32.FreeType,{$ENDIF}
	NES.Config, NES.Types, NES.Console, NES.Cartridge;

// ========================================================================
// TSnapshotable
// ========================================================================

constructor TSnapshotable.Create(const RegisterName: AnsiString);
begin
	inherited Create;

	RegisteredName := RegisterName;
	RegisteredProperties := TObjectList<TSnapshotProperty>.Create;
end;

destructor TSnapshotable.Destroy;
begin
	RegisteredProperties.Free;

	inherited Destroy;
end;

function TSnapshotable.AssertSection(const Section: AnsiString): Boolean;
var
	P: Int64;
	RS: String;
begin
	P := Stream.Position;
	RS := StreamReadString(Stream);
	Result := RS = Section;
	if not Result then
		Log(Format('LoadState: AssertSection failed at 0x%s: Expected "%s", got "%s"',
			[P.ToHexString(4), Section, RS]));
end;

function TSnapshotable.StartSection(const Section: AnsiString): Boolean;
begin
	if Saving then
	begin
		// Log('StartSection "' + Section + '" at 0x' + Stream.Position.ToHexString(4));
		StreamWriteString(Stream, Section);
		Result := True;
	end
	else
		Result := AssertSection(Section);
end;

// Example: RegisterProperty(8, @someByte)
//
procedure TSnapshotable.RegisterProperty(Size: Integer; Data: Pointer);
var
	P: TSnapshotProperty;
begin
	P := TSnapshotProperty.Create;
	P.Data := Data;
	if Size = 1 then Size := 8; // Boolean
	if Size < 0 then
		P.DataSize := Abs(Size)
	else
		P.DataSize := Size div 8; // bitcount->bytecount
	P.IsArray := (Size < 0);
	RegisteredProperties.Add(P);
end;

// Example: RegisterArray(Length(someArray) * SizeOf(someArray[0]), @someArray[0]);
//
procedure TSnapshotable.RegisterArray(Size: Word; Data: Pointer);
begin
	RegisterProperty(-Size, Data);
end;

procedure TSnapshotable.LoadSnapshot;
var
	P: TSnapshotProperty;
begin
	if RegisteredName <> '' then
		if not StartSection(RegisteredName) then
			Exit;

	if Assigned(RegisteredProperties) then
		for P in RegisteredProperties do
			Stream.Read(P.Data^, P.DataSize);
end;

procedure TSnapshotable.SaveSnapshot;
var
	P: TSnapshotProperty;
begin
	if RegisteredName <> '' then
		StartSection(RegisteredName);

	if Assigned(RegisteredProperties) then
		for P in RegisteredProperties do
			Stream.WriteBuffer(P.Data^, P.DataSize);
end;

// ========================================================================
// TSaveStateManager
// ========================================================================

function TSaveStateManager.GetPath: String;
begin
	Result := IncludeTrailingPathDelimiter(Path);
end;

procedure TSaveStateManager.SetPath(const DirectoryPath: String);
begin
	Path := IncludeTrailingPathDelimiter(DirectoryPath);
	ForceDirectories(Path);
end;

function TSaveStateManager.GetStateFileName(SlotIndex: Byte): String;
var
	S: String;
begin
	if SlotIndex = SAVESLOT_TEMP then
		S := 'X'
	else
		S := SlotIndex.ToString;
	Result := Path + Cartridge.RomData.Info.Hash.Crc32.ToHexString(8) +
		'_' + S + '.state';
end;

procedure TSaveStateManager.SelectSaveSlot(SlotIndex: Byte; ShowOSD: Boolean = True);
begin
	if SlotIndex <= MaxIndex then
	begin
		Index := SlotIndex;
		if ShowOSD then DisplaySaveSlots;
	end;
end;

procedure TSaveStateManager.MoveToNextSlot(ShowOSD: Boolean = True);
begin
	Index := Min(Index+1, MaxIndex);
	if ShowOSD then DisplaySaveSlots;
end;

procedure TSaveStateManager.MoveToPreviousSlot(ShowOSD: Boolean = True);
begin
	Index := Max(Index-1, 0);
	if ShowOSD then DisplaySaveSlots;
end;

procedure TSaveStateManager.UpdateStateAvailability;
var
	i: Integer;
	Fn: String;
begin
	for i := 0 to MaxIndex do
	begin
		Fn := GetStateFileName(i);
		StateFileInfo[i].Filename := Fn;
		StateFileInfo[i].Exists := FileExists(Fn);
		if StateFileInfo[i].Exists then
			FileAge(Fn, StateFileInfo[i].DateTime);
	end;
end;

function TSaveStateManager.LoadState(const Filename: String): Boolean;
begin
	if not FileExists(Filename) then Exit(False);

	{$IFDEF DEBUG}
	Log('LoadState: ' + Filename);
	{$ENDIF}

	Stream := TFileStream.Create(Filename, fmOpenRead);
	try
		TSnapshotable.Stream := Stream;
		TSnapshotable.Saving := False;

		if Stream.ReadDWord <> FileFormatVersion then
			Log('Warning: Savestate version mismatch!');

		Console.LoadSnapshot;
	finally
		Stream.Free;
		Result := True;
	end;
end;

function TSaveStateManager.LoadState(SlotIndex: Integer): Boolean;
var
	Fn: String;
begin
	SlotIndex := IfThen(SlotIndex < 0, Index, SlotIndex);
	Fn := GetStateFileName(SlotIndex);
	Result := LoadState(Fn);
	StateFileInfo[SlotIndex].Filename := Fn;
	StateFileInfo[SlotIndex].Exists := Result;
	if Result then
		FileAge(Fn, StateFileInfo[SlotIndex].DateTime);
end;

function TSaveStateManager.LoadState(AStream: TStream): Boolean;
begin
	if AStream = nil then Exit(False);

	TSnapshotable.Stream := AStream;
	TSnapshotable.Saving := False;

	Console.LoadSnapshot;
	Result := True;
end;

function TSaveStateManager.SaveState(const Filename: String): Boolean;
begin
	//Log('SaveStateManager.SaveState: ', Filename);

	Stream := TFileStream.Create(Filename, fmCreate);
	try
		TSnapshotable.Stream := Stream;
		TSnapshotable.Saving := True;

		Stream.WriteDWord(FileFormatVersion);
		Console.SaveSnapshot;

	finally
		Stream.Free;
		TSnapshotable.Stream := nil;
		TSnapshotable.Saving := False;
		Result := True;
	end;
end;

function TSaveStateManager.SaveState(SlotIndex: Integer): Boolean;
var
	Fn: String;
begin
	SlotIndex := IfThen(SlotIndex<0, Index, SlotIndex);
	Fn := GetStateFileName(SlotIndex);
	Result := SaveState(Fn);
	StateFileInfo[SlotIndex].Filename := Fn;
	StateFileInfo[SlotIndex].Exists := Result;
	StateFileInfo[SlotIndex].DateTime := Now;
end;

function TSaveStateManager.SaveState(AStream: TStream): Boolean;
begin
	if AStream = nil then Exit(False);

	TSnapshotable.Stream := AStream;
	TSnapshotable.Saving := True;

	Console.SaveSnapshot;
	Result := True;
end;

procedure TSaveStateManager.DisplaySaveSlots;
var
	i, W, H, Y: Integer;
	R: TRect;
	buf: TBitmap32;
	S: String;
begin
	Y := OSDLayer.Font.GlyphHeight + 6;

	W := Trunc(OSDLayer.Font.GlyphWidth  * 3.3);
	H := Trunc(OSDLayer.Font.GlyphHeight * 1.5);

	buf := TBitmap32.Create(W*10+2, H+Y+1);

	buf.Clear(Palette[COLOR_MESSAGE_BG]);

	for i := 0 to MaxIndex do
	begin
		R := Bounds(i*W+1, Y+1, W, H-1);

		buf.FillRect(R, IfThen(StateFileInfo[i].Exists, Palette[COLOR_SAVESLOT_EXISTS], Palette[COLOR_SAVESLOT_EMPTY]));
		buf.ThickFrameRect(R, -2, IfThen(i=Index, Palette[COLOR_SAVESLOT_SELECTED], Palette[COLOR_SAVESLOT_BORDER]));

		R.Offset(2, -1);
		OSDLayer.Font.DrawTextRect(buf, i.ToString, R, Palette[COLOR_SAVESLOT_TEXT], [ftaCenter, ftaVerticalCenter]);
	end;

	if StateFileInfo[Index].Exists then
		S := FormatDateTime(Configuration.Application.DateTimeFormat, StateFileInfo[Index].DateTime)
	else
		S := 'Unused';

	OSDLayer.Font.DrawTextRect(buf, Format('%d: %s', [Index, S]),
		Bounds(0, 0, buf.Width, Y-1), Palette[COLOR_SAVESLOT_MESSAGE], [ftaCenter, ftaVerticalCenter]);

	OSD(buf);

	buf.Free;
end;

end.

