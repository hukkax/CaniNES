unit Basement.UnZip;

{$mode Delphi}

interface

uses
	Classes, SysUtils,
	Zipper;

type
	TZipParts = record
		IsZip: Boolean;
		Filename,
		ZipPath,
		ZipFilename: String;

		function GetParts(FullPath: String): Boolean;
	end;

	TUnZipperEx = class(TUnZipper)
	private
		TempStrings: TStringList;
		TempBytes:   TBytes;

		procedure DoCreateOutZipStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
		procedure DoDoneOutZipStream_Strings(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
		procedure DoDoneOutZipStream_Bytes(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
	public
		function ListDirectories(const SearchPath: String = ''): TStringList;
		function ListFiles(const SearchPath: String; const SearchMask: String): TStringList;

		function ExtractTextFile(const AFilename: String): TStringList;
		function ExtractFile(const AFilename: String): TBytes;

		constructor Create(const AFilename: String); overload;
	end;


	function IsZipFile(const Filename: String): Boolean;


implementation

uses
	StrUtils;


function IsZipFile(const Filename: String): Boolean;
begin
	Result := (LowerCase(ExtractFileExt(Filename)) = '.zip') and
		(FileExists(Filename)) and (not DirectoryExists(Filename));
end;

// Input: "C:\foo.zip\subdir\file.txt"
//
function TZipParts.GetParts(FullPath: String): Boolean;
var
	X: Integer;
begin
	// Filename = C:\foo.zip
	// ZipPath = subdir\file.txt
	// ZipFilename = foo.zip

	IsZip := False;
	Filename := '';
	ZipFilename := '';
	ZipPath := '';

	X := Pos('.zip', LowerCase(FullPath));
	if X > 1 then
	begin
		IsZip := True;
		Inc(X, 3);

		ZipPath := Copy(FullPath, X+2, maxInt); // somedir\2\file.txt
		Filename := ExcludeTrailingPathDelimiter(Copy(FullPath, 1, X)); // C:\foo.zip
		ZipFilename := ExtractFilename(Filename);
	end;

	Result := IsZip;
end;

procedure TUnZipperEx.DoCreateOutZipStream(Sender: TObject;
	var AStream: TStream; AItem: TFullZipFileEntry);
begin
	AStream := TMemoryStream.Create;
end;

procedure TUnZipperEx.DoDoneOutZipStream_Strings(Sender: TObject;
	var AStream: TStream; AItem: TFullZipFileEntry);
begin
	AStream.Position := 0;
	if Assigned(TempStrings) then
		TempStrings.LoadFromStream(AStream);
	AStream.Free;
end;

procedure TUnZipperEx.DoDoneOutZipStream_Bytes(Sender: TObject;
	var AStream: TStream; AItem: TFullZipFileEntry);
begin
	AStream.Position := 0;
	SetLength(TempBytes, AStream.Size);
	AStream.Read(TempBytes, AStream.Size);
	AStream.Free;
end;


function TUnZipperEx.ListFiles(const SearchPath: String; const SearchMask: String): TStringList;
var
	i: Integer;
	E: TZipFileEntry;
	SP: String = '';
	WantPath: Boolean;
begin
	Result := TStringList.Create;

	WantPath := not SearchPath.IsEmpty;
	if WantPath then
	begin
		SP := IncludeTrailingPathDelimiter(SearchPath);
		SP := SP.Replace('\','/');
	end;

	for i := 0 to Entries.Count-1 do
	begin
		E := Entries.Entries[i];
		if (not E.IsDirectory) and (IsWild(E.ArchiveFileName, SearchMask, True)) then
		begin
			if (not WantPath) or (E.ArchiveFileName.StartsWith(SP, True)) then
				Result.Add(E.ArchiveFileName);
		end;
	end;
end;

function TUnZipperEx.ListDirectories(const SearchPath: String): TStringList;
var
	i: Integer;
	E: TZipFileEntry;
	SP: String = '';
	WantPath: Boolean;
begin
	Result := TStringList.Create;

	WantPath := not SearchPath.IsEmpty;
	if WantPath then
	begin
		SP := IncludeTrailingPathDelimiter(SearchPath);
		SP := SP.Replace('\','/');
	end;

	for i := 0 to Entries.Count-1 do
	begin
		E := Entries.Entries[i];
		if E.IsDirectory then
		begin
			if (not WantPath) or (E.ArchiveFileName.StartsWith(SP, True)) then
				Result.Add(E.ArchiveFileName);
		end;
	end;
end;


function TUnZipperEx.ExtractTextFile(const AFilename: String): TStringList;
begin
	TempStrings := TStringList.Create;

	OnCreateStream := DoCreateOutZipStream;
	OnDoneStream   := DoDoneOutZipStream_Strings;
	UnZipFile(AFilename);

	Result := TStringList.Create;
	Result.Assign(TempStrings);
	TempStrings.Free;
end;

function TUnZipperEx.ExtractFile(const AFilename: String): TBytes;
begin
	SetLength(TempBytes, 0);
	OnCreateStream := DoCreateOutZipStream;
	OnDoneStream   := DoDoneOutZipStream_Bytes;
	UnZipFile(AFilename);
	Result := TempBytes;
end;

constructor TUnZipperEx.Create(const AFilename: String);
begin
	inherited Create;

	Filename := AFilename;
	Examine;
end;


end.

