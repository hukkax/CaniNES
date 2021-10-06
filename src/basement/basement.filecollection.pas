unit Basement.FileCollection;

{ Transparent access to files from within various sources
  (currently supported: normal directories and Zip archives)

  Public Domain, hukka 2021-09-21
}

{$mode Delphi}

{$IFDEF DEBUG}
{.$DEFINE FILECOLL_LOGGING}
{$ENDIF}

interface

uses
	Classes, SysUtils,
	Generics.Collections,
	Basement.UnZip;

type
	TFileList = class;

	TFileCollectionItem = record
		IsArchive:  Boolean;
		SourcePath: String;
		List: TFileList;
	end;

	TFileListItem = class
		RelativePath: String; // empty for files in the root; using / as dir separator
	end;

	TFileList = class
	public
		IsArchive,
		Recursive,
		CaseSensitive: Boolean;
		SourcePath,
		FileMask:      String;

		ZipFile:       TUnZipperEx;
		Files:         TStringList;

		// Rescan files from the source
		procedure Update;
		// Returns a file item if it exists within the list
		function  FindFile(const Filename, Filepath: String; var Item: TFileCollectionItem): Boolean;

		constructor Create(const Path, ExtensionMask: String; CaseSensitivity, IncludeSubDirs: Boolean); overload;
		destructor  Destroy; override;
	end;

	TFileCollection = TObjectList<TFileList>;

	TFileCollector = class
	public
		CaseSensitive: Boolean;
		FileMask: String;
		Sources:  TFileCollection;

		// Rescan files from all sources
		procedure Update;

		// Add a source directory or archive file; use semicolon to separate multiple pathes
		procedure AddSource(const Path: String; IncludeSubDirs: Boolean);
		// Remove a source directory or archive
		function  RemoveSource(const Path: String): Boolean;

		// Returns True if a file was found within any source
		function  FindFile(const Filename: String; var Item: TFileCollectionItem): Boolean;

		// Returns the contents of a file (if found) as a memory stream
		function  GetFileStream(const Filename: String): TMemoryStream;
		// Returns the contents of a file (if found) as an array of bytes
		function  GetFileBytes(const Filename: String): TBytes;

		constructor Create;
		destructor  Destroy; override;
	end;


implementation

uses
	{$IFDEF FILECOLL_LOGGING}TextOutput,{$ENDIF}
	hkaFileUtils, StrUtils;


function SameString(const S1, S2: String; IgnoreCase: Boolean): Boolean; inline;
begin
	if Length(S1) <> Length(S2) then
		Result := False
	else
		Result := S1.Compare(S1, S2, IgnoreCase) = 0;
end;

// =================================================================================================
// TFileList
// =================================================================================================

constructor TFileList.Create(const Path, ExtensionMask: String; CaseSensitivity, IncludeSubDirs: Boolean);
begin
	inherited Create;

	SourcePath := IncludeTrailingPathDelimiter(Path);
	FileMask := ExtensionMask;
	CaseSensitive := CaseSensitivity;
	Recursive := IncludeSubDirs;

	{$IFDEF FILECOLL_LOGGING}
	Log('TFileList.Create: %s, %s', [Path, FileMask]);
	{$ENDIF}

	Files := TStringList.Create;
	Files.CaseSensitive := CaseSensitive;
	Files.OwnsObjects := True;

	IsArchive := IsZipFile(Path);
	if IsArchive then
		ZipFile := TUnZipperEx.Create(Path);

	Update;

	{$IFDEF FILECOLL_LOGGING}
	Log('* %d files.', [Files.Count]);
	{$ENDIF}
end;

destructor TFileList.Destroy;
begin
	Files.Free;
	ZipFile.Free;

	inherited Destroy;
end;

// Rescan files from the source
//
procedure TFileList.Update;
const
	AllMask = '*.*';
var
	List: TStringList;
	Item: TFileListItem;
	Fn, S, Mask: String;
	Masks: TStringArray;
begin
	Files.Clear;

	Masks := FileMask.Split(';');

	if IsArchive then
	begin
		{$IFDEF FILECOLL_LOGGING}
		Log('TFileList.Update: ZipFile.ListFiles("", "%s")', [FileMask]);
		{$ENDIF}
		List := ZipFile.ListFiles('', AllMask);
	end
	else
	begin
		{$IFDEF FILECOLL_LOGGING}
		Log('TFileList.Update: ListFiles("%s"+"%s")', [SourcePath, FileMask]);
		{$ENDIF}
		List := ListFiles(SourcePath + AllMask, Recursive);
	end;

	if List = nil then
	begin
		{$IFDEF FILECOLL_LOGGING}
		Log('TFileList.Update: List is nil!');
		{$ENDIF}
		Exit;
	end;

	for S in List do
	begin
		if (IsArchive) and (not Recursive) then
			If S.Contains('/') or S.Contains('\') then
				Continue;

		for Mask in Masks do
		if IsWild(S, Mask, True) then
		begin
			Item := TFileListItem.Create;

			Fn := ExtractFilePath(S);
			Fn := Fn.Replace(SourcePath, '');
			Item.RelativePath := Fn.Replace('\', '/');

			Fn := ExtractFileName(S);
			Files.AddObject(Fn, Item);

			{$IFDEF FILECOLL_LOGGING}
			Log('AddFile: %s // %s', [Item.RelativePath, Fn]);
			{$ENDIF}
			Break;
		end;
	end;

	List.Free;
end;

// Returns a file item if it exists within the list
//
function TFileList.FindFile(const Filename, Filepath: String; var Item: TFileCollectionItem): Boolean;
var
	i: Integer;
	FileItem: TFileListItem;
begin
	for i := 0 to Files.Count-1 do
	begin
		if SameString(Files[i], Filename, not CaseSensitive) then
		begin
			FileItem := TFileListItem(Files.Objects[i]);
			if FileItem = nil then Continue;

			// if a path was given, the file's path must match
			if (not Filepath.IsEmpty) and (not SameString(FileItem.RelativePath, Filepath, True)) then
				Continue;

			// found the requested file
			Item.SourcePath := SourcePath + FileItem.RelativePath + Files[i];
			Item.IsArchive := IsArchive;
			Item.List := Self;
			{$IFDEF FILECOLL_LOGGING}
			Log('FindFile: found Name=%s Path=%s Full=%s', [Filename, Filepath, Item.SourcePath]);
			{$ENDIF}
			Exit(True);
		end;
	end;

	Result := False;
end;

// =================================================================================================
// TFileCollector
// =================================================================================================

constructor TFileCollector.Create;
begin
	inherited Create;

	CaseSensitive := False;
	FileMask := '*.*';

	Sources := TFileCollection.Create(True);
end;

destructor TFileCollector.Destroy;
begin
	Sources.Free;

	inherited Destroy;
end;

// Rescan files from all sources
//
procedure TFileCollector.Update;
var
	List: TFileList;
begin
	for List in Sources do
		List.Update;
end;

// Add a source directory or archive file
//
procedure TFileCollector.AddSource(const Path: String; IncludeSubDirs: Boolean);
var
	List: TFileList;
	Arr: TStringArray;
	S: String;
begin
	{$IFDEF FILECOLL_LOGGING}
	Log('AddSource: %s', [Path]);
	{$ENDIF}

	Arr := Path.Split(';');
	for S in Arr do
	begin
		RemoveSource(S);
		List := TFileList.Create(S, FileMask, CaseSensitive, IncludeSubDirs);
		Sources.Add(List);
	end;
end;

// Remove a source directory or archive
//
function TFileCollector.RemoveSource(const Path: String): Boolean;
var
	i: Integer;
begin
	Result := False;
	for i := Sources.Count-1 downto 0 do
	begin
		if SameString(Sources[i].SourcePath, Path, True) then
		begin
			Sources.Delete(i);
			Result := True;
		end;
	end;
end;

// Returns True if a file was found within any source
//
function TFileCollector.FindFile(const Filename: String; var Item: TFileCollectionItem): Boolean;
var
	List: TFileList;
	Name, Path: String;
begin
	if not Assigned(Sources) then Exit(False);

	Name := Filename.Replace('\', '/');
	Path := '';

	if Name.Contains('/') then
	begin
		Path := ExtractFilePath(Name);
		Name := ExtractFilename(Filename);
	end;

	for List in Sources do
		if List.FindFile(Name, Path, Item) then
			Exit(True);
	Result := False;
end;

// Returns the contents of a file (if found) as a memory stream
//
function TFileCollector.GetFileStream(const Filename: String): TMemoryStream;
var
	Item: TFileCollectionItem;
	Data: TBytes;
begin
	Result := nil;
	if Filename.IsEmpty then Exit;

	{$IFDEF FILECOLL_LOGGING}
	Log('GetFileStream: %s', [Filename]);
	{$ENDIF}

	Item := Default(TFileCollectionItem);

	if FindFile(Filename, Item) then
	begin
		Result := TMemoryStream.Create;

		if not Item.IsArchive then
			Result.LoadFromFile(Item.SourcePath)
		else
		begin
			Data := Item.List.ZipFile.ExtractFile(ExtractFilename(Item.SourcePath));
			Result.Write(Data[0], Length(Data));
		end;

		Result.Seek(0, soFromBeginning);
	end
	{$IFDEF FILECOLL_LOGGING}
	else
		Log('Not found!');
	{$ENDIF}
end;

// Returns the contents of a file (if found) as an array of bytes
//
function TFileCollector.GetFileBytes(const Filename: String): TBytes;
var
	Str: TMemoryStream;
begin
	Result := nil;
	if Filename.IsEmpty then Exit;

	Str := GetFileStream(Filename);
	if Str <> nil then
	begin
		SetLength(Result, Str.Size);
		Str.Read(Result[0], Str.Size);
		Str.Free;
	end;
end;

end.

