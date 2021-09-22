unit NES.Database;

{$MODE DELPHI}

interface

uses
	Classes, SysUtils,
	Generics.Collections,
	Basement.FileCollection,
	NES.Types, NES.ROM, NES.ROM.Header;

const
	SystemNames: array[TGameSystem] of String = (
		'NesNtsc',  'NesPal',     'Famicom',  'Dendy',
		'VsSystem', 'Playchoice', 'FDS',      'Unknown'
	);

	Replacements: array [0..24, 0..1] of String = (
		(' (U)', ''),
		(' (E)', ''),
		(' (J)', ''),
		(' (W)', ''),
		(' [!]', ''),

		(' (U)', ' (USA)'),
		(' (E)', ' (Europe)'),
		(' (J)', ' (Japan)'),
		(' (W)', ' (World)'),

		(' (USA)',    ''),
		(' (Europe)', ''),
		(' (Japan)',  ''),
		(' (World)',  ''),
		(' (Unl)',    ''),
		(' (Beta)',   ''),
		(' (PRG0)',   ''),
		(' (PRG1)',   ''),

		( '_',    ' ' ),
		( '&',    '_' ),

		( ' II', ' 2' ),
		( ' III',' 3' ),
		( ' IV', ' 4' ),
		( ' V',  ' 5' ),
		( ' VI', ' 6' ),

		( 'Bros.', 'Bros' )
	);

type
	TGameDatabase = class
	private
		DB, Titles: TStringList;
		ImageExts:  TStringArray;

		function  GetGameInfoString(romCrc: Cardinal): String;
	public
		Enabled, Initialized: Boolean;

		FileList: array [caBoxArt..caLastIndex] of TFileCollector;

		constructor Create(Path, fnFilename, fnTitles: String);
		destructor  Destroy; override;

		function  GetStringByCRC(list: TStrings; romCrc: Cardinal): String;

		procedure FindBoxArt(var info: TRomInfo);

		function  LoadFromFile(const fnFilename, fnTitles: String): Boolean;
		function  GetGameInfo  (RomCrc: Cardinal; var RomInfo: TRomInfo): Boolean;
		function  GetGameTitle (var RomInfo: TRomInfo): String;
		function  GetiNesHeader(RomCrc: Cardinal; var nesHeader: TNESHeader): Boolean;
		function  GetDbRomSize (RomCrc: Cardinal; var prgSize, chrSize: Cardinal): Boolean;
		function  GetGameSystem(const System: String): TGameSystem;
	end;

implementation

uses
	Math, hkaFileUtils, Basement.Util, TextOutput,
	NES.Config, NES.Console;


// roms.ini
// uses hkaFileUtils, FileStreamEx
{
function ListRoms(const Path: String);
var
	B: Boolean;
	sl1, sl2: TStrings;
	ss,si,cc: Integer;
	Crc32: Cardinal;
	Fn: String;
begin
	Sect := 'X:\Data\ROM\NES\';

	sl1 := ListFiles(Sect + '*.nes', True);

	for i := 0 to sl1.count-1 do
	begin
		sl1[i] := sl1[i].Replace(Sect, '').Replace('\', '/');

		Fn := Sect + sl1[i];

		if not FileExists(Fn) then
		begin
			writeln('Not found: "' + Fn + '"');
			Continue;
		end;

		Crc32 := FileGetCRC32(Fn, 16);

		if Crc32 <> 0 then
			sl1[i] := sl1[i] + '=' + Crc32.ToHexString(8)
		else
			sl1[i] := ';' + sl1[i];

		if i mod 20 = 0 then
			writeln( IntToStr(Trunc((i / (sl1.Count-1)) * 100)) + '%' );
	end;

	TStringList(sl1).Sort;


	sl1.Insert(0, '[Roms]');
	sl1.Insert(0, '');
	sl1.Insert(0, 'path=' + Sect.Replace('\', '/'));
	sl1.Insert(0, '[Root]');

	sl1.SaveToFile(Path + 'roms.ini');
	sl1.Free;
end;
}

{ TGameDatabase }

constructor TGameDatabase.Create(Path, fnFilename, fnTitles: String);
var
	i: Integer;
	Dir, S: String;
	sl: TStrings;
begin
	inherited Create;

	DB := TStringList.Create;
	Titles := TStringList.Create;

	Enabled := LoadFromFile(Path + fnFilename, Path + fnTitles);

	LogVerbose('[DB] Indexing box art...');

	ImageExts := ImageFileExts.Replace('*', '').Split(';');

	for i := caBoxArt to caLastIndex do
	begin
		case i of
			caBoxArt: Dir := Configuration.Application.ImagePath_Boxart;
			caSnaps:  Dir := Configuration.Application.ImagePath_Snaps;
			caTitles: Dir := Configuration.Application.ImagePath_Titles;
		end;
		if Dir = '' then Continue;

		FileList[i] := TFileCollector.Create;
		FileList[i].FileMask := ImageFileExts;
		FileList[i].AddSource(Dir, False);
	end;
end;

destructor TGameDatabase.Destroy;
var
	i: Integer;
begin
	DB.Free;
	Titles.Free;

	for i := caBoxArt to caLastIndex do
		FileList[i].Free;

	inherited Destroy;
end;

// plenty of room for improvement here, all this hassle could be avoided if boxart image sets
// used hash-based filenames instead...
//
procedure TGameDatabase.FindBoxArt(var info: TRomInfo);
var
	Paths: array [caBoxArt..caTitles] of String;
	Item: TFileCollectionItem;

	function TryFindMatchingFile(Kind: Integer; const Filename: String): String;
	var
		S, TS, Fn, Ext: String;
		sl: TStringList;
		x, x2: Integer;
	begin
		Result := '';

		sl := TStringList.Create;
		sl.Sorted := True;
		sl.Duplicates := dupIgnore;

		S := LowerCase(Filename);
		sl.Add(S);
		TS := S;

		repeat
			x  := TS.LastIndexOf('(');
			x2 := TS.LastIndexOf('[');
			x := Max(x, x2);

			if x > 0 then
			begin
				TS := Copy(TS, 1, x-1);
				sl.Add(TS);
			end
			else
				TS := '';
		until TS = '';

		for S in sl do
		begin
			if S.IsEmpty then Continue;

			for Ext in ImageExts do
			begin
				if FileList[Kind].FindFile(S + Ext, Item) then
				begin
					sl.Free;
					Exit(S + Ext);
				end;
			end;
		end;

		sl.Free;
		//log('TryFindMatchingFile(%s) = %s', [Filename, Result]);
	end;

var
	S, SrcFn, Fn, Fn2: String;
	i, ci: Integer;
	slo, sl: TStringList;
begin
	Paths[caBoxArt] := IncludeTrailingPathDelimiter(Configuration.Application.ImagePath_Boxart);
	Paths[caSnaps]  := IncludeTrailingPathDelimiter(Configuration.Application.ImagePath_Snaps);
	Paths[caTitles] := IncludeTrailingPathDelimiter(Configuration.Application.ImagePath_Titles);

	for ci := caBoxArt to caLastIndex do
		info.DatabaseInfo.Artwork[ci] := '';

	slo := TStringList.Create;
	slo.Sorted := True;
	slo.Duplicates := dupIgnore;

	// we want to try basing the search first on the rom's full title, if found in DB.
	// otherwise use the rom filename for matching.
	//
	slo.Add(info.DatabaseInfo.Title);
	slo.Add(ExtractFilename(info.Filename));
	// todo could add crc32 hash to list

	sl := TStringList.Create;
	sl.Sorted := True;
	sl.Duplicates := dupIgnore;

	for SrcFn in slo do
	begin
		Fn := SrcFn;
		if UpperCase(ExtractFileExt(Fn)) = '.NES' then
			Fn := ChangeFileExt(Fn, '');

		// perform the heuristics to look for the appropriate matching filename separately
		// for each piece of artwork, since filenames may differ between sets
		//
		for ci := caBoxArt to caLastIndex do
		begin
			if not info.DatabaseInfo.Artwork[ci].IsEmpty then Continue;

			sl.Clear;
			Fn2 := TryFindMatchingFile(ci, Fn);

			if Fn2.IsEmpty then
			begin
				for i := Low(Replacements) to High(Replacements) do
				begin
					if Fn.Contains(Replacements[i, 0]) then
						sl.Add(Fn.Replace(Replacements[i, 0], Replacements[i, 1]))
					else
					if Fn.Contains(Replacements[i, 1]) then
						sl.Add(Fn.Replace(Replacements[i, 1], Replacements[i, 0]));
				end;

				for S in sl do
				begin
					Fn2 := TryFindMatchingFile(ci, S);
					if not Fn2.IsEmpty then Break; // found match
				end;
			end;

			if not Fn2.IsEmpty then
			begin
				info.DatabaseInfo.Artwork[ci] := Fn2;
				LogVerbose('[DB] Found box art: %s', [ExtractFilename(Fn2)]);
			end;
		end;
	end;

	sl.Free;
	slo.Free;
end;

function TGameDatabase.LoadFromFile(const fnFilename, fnTitles: String): Boolean;
begin
	Result := False;

	WriteResource('DATABASE', fnFilename);
	WriteResource('TITLES',   fnTitles);

	if FileExists(fnFilename) then
	begin
		DB.LoadFromFile(fnFilename);
		Result := DB.Count > 0;
		Log('Loaded cart info database from %s. (%d entries)', [fnFilename, DB.Count]);
	end
	else
		Log('Failed to load cart info database from %s.', [fnFilename]);

	if FileExists(fnTitles) then
	begin
		Titles.LoadFromFile(fnTitles);
		Log('Loaded cart title database from %s. (%d entries)', [fnTitles, Titles.Count]);
	end
	else
		Log('Failed to load cart title database from %s.', [fnTitles]);
end;

function TGameDatabase.GetGameSystem(const System: String): TGameSystem;
var
	GS: TGameSystem;
begin
	for GS in TGameSystem do
		if SystemNames[GS] = System then
			Exit(GS);
	Result := TGameSystem.NES_NTSC;
end;

function TGameDatabase.GetStringByCRC(list: TStrings; romCrc: Cardinal): String;
var
	S, CRC: String;
begin
	CRC := romCRC.ToHexString(8);
	for S in list do
		if S.StartsWith(CRC) then
			Exit(S);
	Result := '';
end;

function TGameDatabase.GetGameInfoString(romCrc: Cardinal): String;
begin
	Result := GetStringByCRC(DB, romCrc);
end;

function TGameDatabase.GetGameTitle(var RomInfo: TRomInfo): String;
var
	S: String;
begin
	Result := '';

	S := GetStringByCRC(Titles, RomInfo.Hash.PrgChrCrc32);
	if S.IsEmpty then
		S := GetStringByCRC(Titles, RomInfo.Hash.PrgCrc32);

	if not S.IsEmpty then
	begin
		S := Copy(S, S.IndexOf('=')+2, MaxInt);
		if not S.IsEmpty then
		begin
			Result := S;
			RomInfo.DatabaseInfo.Title := S;
			Log('[DB] Title: ' + S);
		end;
	end;
end;

function TGameDatabase.GetGameInfo(RomCrc: Cardinal; var RomInfo: TRomInfo): Boolean;

	function GetInt(const S: String): Integer;
	begin
		if not TryStrToInt(S, Result) then Result := 0;
	end;

var
	S: String;
	values: TStringArray;
	info: PGameInfo;
begin
	info := @RomInfo.DatabaseInfo;

	Result := False;

	S := GetGameInfoString(romCrc);
	if S = '' then Exit;

	Log('[DB] Found: ' + S);

	values := S.Split([',']);
	if Length(values) < 16 then
	begin;
		Log('[DB] Parsing failed.');
		Exit;
	end;

	//Info.CRC := romCrc;  // values[00]
	try
		Info.System      := values[01];
		Info.Board       := values[02];
		Info.Pcb         := values[03];
		Info.Chip        := values[04];
		Info.MapperID    := GetInt(values[05]);
		Info.PrgRomSize  := GetInt(values[06]) * 1024;
		Info.ChrRomSize  := GetInt(values[07]) * 1024;
		Info.ChrRamSize  := GetInt(values[08]) * 1024;
		Info.WorkRamSize := GetInt(values[09]) * 1024;
		Info.SaveRamSize := GetInt(values[10]) * 1024;
		Info.HasBattery  := values[11].ToBoolean;
		Info.Mirroring   := values[12];
		Info.InputType   := TGameInputType(GetInt(values[13]));
		Info.BusConflicts:= values[14];
		Info.SubmapperID := values[15];
		//Info.VsType := (VsSystemType)ToInt<uint32_t>(values[16]);
		//Info.VsPpuModel := (PpuModel)ToInt<uint32_t>(values[17]);
		//if Info.MapperID = 65000 then
		//	Info.MapperID := UnifLoader.GetMapperID(Info.Board);

		//FindBoxArt(RomInfo);
	except
		Exit(False);
	end;

	Result := True;
end;

function TGameDatabase.GetiNesHeader(romCrc: Cardinal; var nesHeader: TNESHeader): Boolean;
begin
	Result := False;
end;

function TGameDatabase.GetDbRomSize(romCrc: Cardinal; var prgSize, chrSize: Cardinal): Boolean;
var
	info: TRomInfo;
begin
	Result := GetGameInfo(romCrc, info{%H-});
	if Result then
	begin
		prgSize := info.DatabaseInfo.PrgRomSize;
		chrSize := info.DatabaseInfo.ChrRomSize;
	end;
end;

end.

