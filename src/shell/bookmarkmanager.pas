unit BookmarkManager;

{$mode Delphi}

interface

uses
	Classes, SysUtils;

type
	TBookmarkCollection = class
	private
		FFilename: String;
	public
		Items: TStringList;

		function  Load: Boolean;
		function  Save: Boolean;

		function  Add(const Filename: String): Boolean;
		function  Remove(const Filename: String): Boolean;
		function  Toggle(Filename: String): Boolean;

		function  Contains(Filename: String): Boolean;

		constructor Create(const Filename: String);
		destructor  Destroy; override;
	end;

var
	Bookmarks: TBookmarkCollection;

implementation

uses
	NES.Console, MainMenu, MenuHandler;

{ TBookmarkCollection }

constructor TBookmarkCollection.Create(const Filename: String);
begin
	inherited Create;

	Items := TStringList.Create;
	Items.Sorted := True;
	Items.CaseSensitive := False;
	Items.Duplicates := dupIgnore;

	FFilename := Filename;
	Load;
end;

destructor TBookmarkCollection.Destroy;
begin
	Save;
	Items.Free;

	inherited Destroy;
end;

function TBookmarkCollection.Load: Boolean;
begin
	Result := FileExists(FFilename);
	if Result then
		Items.LoadFromFile(FFilename);
end;

function TBookmarkCollection.Save: Boolean;
begin
	Items.SaveToFile(FFilename);
	Result := True;
end;

function TBookmarkCollection.Contains(Filename: String): Boolean;
begin
	Result := Items.IndexOf(Filename) >= 0;
end;

function TBookmarkCollection.Add(const Filename: String): Boolean;
begin
	Result := not Contains(Filename);
	if Result then
		Items.Add(Filename);
end;

function TBookmarkCollection.Remove(const Filename: String): Boolean;
var
	i: Integer;
begin
	i := Items.IndexOf(Filename);
	Result := i >= 0;
	if Result then
		Items.Delete(i);
end;

function TBookmarkCollection.Toggle(Filename: String): Boolean;
var
	i: Integer;
begin
	if Filename.IsEmpty then
	begin
		Filename := Console.LoadedFile;
		if Menu.Visible then
		begin
			if (Menu.CurrentEntry <> nil) and (Menu.CurrentEntry.IsFile) then
				Filename := Menu.CurrentEntry.Data;
		end;
	end;

	i := Items.IndexOf(Filename);
	Result := i >= 0;
	if Result then
		Items.Delete(i)
	else
		Items.Add(Filename);

	if Menu.Visible then
		UpdateMainMenu;
end;

end.

