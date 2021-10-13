unit MenuPage.FileBrowser;

{$mode delphi}

interface

uses
	Classes, SysUtils,
	Graphics32,
	MenuHandler, NES.Types;

type
	TFileBrowser = class(TMenuPageClass)
	private
		Directory: String;

		procedure CmdOpenFile(Entry: TMenuEntry);
		procedure CmdBrowseToParent(Entry: TMenuEntry);
		procedure CmdBrowseToDirectory(Entry: TMenuEntry);
		procedure CmdSetRomPath(Entry: TMenuEntry);
		procedure CmdSetBoxArtPath(Entry: TMenuEntry);
		procedure CmdSetSnapsPath(Entry: TMenuEntry);
		procedure CmdSetTitlesPath(Entry: TMenuEntry);

		{$IFDEF WINDOWS}
		procedure CmdListDrives(Entry: TMenuEntry);
		procedure CmdBrowseToDrive(Entry: TMenuEntry);
		{$ENDIF}
	public
		procedure BrowseTo(const Dir: String; ShowMappers: Boolean = False; AllowLoadRom: Boolean = False);

		constructor Create(BrowserPage: TMenuPage; const Dir: String); overload;
	end;


implementation

uses
	MainWindow, MainMenu, InputBindings, Basement.Renderer.Overlay,
	NES.Config, NES.Console, NES.Cartridge, NES.Mapper, NES.MapperFactory,
	{$IFDEF WINDOWS}Windows,{$ENDIF}
	Logging, Math, FileUtil,
	Basement.UnZip,
	SDL2;

var
	MappersShown: Boolean;
	DirlistCache, FilelistCache: TStringList;


// ============================================================================
// File Browser
// ============================================================================

constructor TFileBrowser.Create(BrowserPage: TMenuPage; const Dir: String);
var
	S: String;
	i: Integer;
begin
	inherited Create;

	Page := BrowserPage;

	Page.FullScreen := True;
	Page.Width := MenuRenderer.FrameBuffer.Width;
	Dec(Page.ItemHeight, 1);

	S := ExtractFilePath(Configuration.Application.LastROMFile);
	if S.IsEmpty then S := Dir;

	BrowseTo(S, ShiftDown);

	S := Configuration.Application.LastROMFile;
	if not S.IsEmpty then
	begin
		S := ExtractFileName(S);
		for i := 0 to Page.Items.Count-1 do
			if Page.Items[i].Caption = S then
			begin
				Menu.SetItemIndex(i);
				Break;
			end;
	end;
end;

procedure TFileBrowser.CmdSetRomPath(Entry: TMenuEntry);
begin
	Configuration.Application.DefaultROMPath := Directory;
	BrowseTo(Directory);
end;

procedure TFileBrowser.CmdSetBoxArtPath(Entry: TMenuEntry);
begin
	Configuration.Application.ImagePath_Boxart := Directory;
	BrowseTo(Directory);
end;

procedure TFileBrowser.CmdSetSnapsPath(Entry: TMenuEntry);
begin
	Configuration.Application.ImagePath_Snaps := Directory;
	BrowseTo(Directory);
end;

procedure TFileBrowser.CmdSetTitlesPath(Entry: TMenuEntry);
begin
	Configuration.Application.ImagePath_Titles := Directory;
	BrowseTo(Directory);
end;

procedure TFileBrowser.CmdOpenFile(Entry: TMenuEntry);
var
	Filename: String;
begin
	Filename := Entry.Data;

	if IsZipFile(Filename) then
		BrowseTo(Filename, False, True)
	else
	begin
		if MappersShown then
			Filename := Copy(Filename, 5, MaxInt);
		Console.LoadROM(Filename);
	end;
end;

{$IFDEF WINDOWS}

procedure TFileBrowser.CmdListDrives(Entry: TMenuEntry);
var
	Drive: Char;
	DriveLetter: String;
	OldMode: Word;
	MaxFileNameLength, VolFlags, {%H-}SerNum: DWord;
	Buf: array [0..MAX_PATH] of Char;
begin
	Page.Items.Clear;
	Page.HeaderCaption := 'Drives';
	Page.AddBackCommand;

	OldMode := SetErrorMode(SEM_FAILCRITICALERRORS);
	try
		for Drive := 'A' to 'Z' do
		begin
			DriveLetter := Drive + ':\';
			if not (GetDriveType(PChar(DriveLetter)) in [DRIVE_UNKNOWN, DRIVE_NO_ROOT_DIR]) then
			begin
				if GetVolumeInformation(PChar(DriveLetter), @Buf, SizeOf(Buf),
					nil, MaxFileNameLength{%H-}, VolFlags{%H-}, nil, 0) then
						DriveLetter := DriveLetter + ' ' + Buf;
				Page.AddEntry(DriveLetter, 0, Palette[COLOR_FILE_DRIVE], CmdBrowseToDrive);
			end;
		end;
	finally
		SetErrorMode(OldMode);
	end;

	Menu.SwitchPage(Page);
	Menu.SetItemIndex(0);
end;

procedure TFileBrowser.CmdBrowseToDrive(Entry: TMenuEntry);
var
	X: Integer;
	S: String;
begin
	S := Entry.Caption;
	X := Pos(' ', S);
	if X > 2 then
		S := Copy(S, 1, X-1);
	BrowseTo(S, ShiftDown);
end;

{$ENDIF}

procedure TFileBrowser.CmdBrowseToParent(Entry: TMenuEntry);
var
	X: Integer;
begin
	X := Length(Directory);
	while X > 2 do
	begin
		Dec(X);
		if Directory[X] in ['\', '/'] then
		begin
			Directory := Copy(Directory, 1, X);
			BrowseTo(Directory, ShiftDown);
			Exit;
		end;
	end;
end;

procedure TFileBrowser.CmdBrowseToDirectory(Entry: TMenuEntry);
var
	S: String;
begin
	S := Entry.Caption;
	S := S.Replace('<', '');
	S := S.Replace('>', '');
	BrowseTo(Directory + S, ShiftDown);
end;

procedure TFileBrowser.BrowseTo(const Dir: String; ShowMappers: Boolean; AllowLoadRom: Boolean);
type
	TFileKind = ( ftBad, ftRom, ftMovie, ftArchive );
var
	Dirs, Files: TStringList;
	DS, S: String;
	Y, ID, RomsListed: Integer;
	MapperClass: TMapperClass;
	ME: TMenuEntry;
	Rescan, PathIsZip: Boolean;
	ZipFile: TUnZipperEx;
	Kind: TFileKind;
	C: TColor32;
begin
	Files := nil;
	Dirs  := nil;

	S := '.zip' + PathDelim;
	Y := Pos(S, LowerCase(Dir));
	PathIsZip := (Y > 1) or IsZipFile(Dir);

	if PathIsZip then
	begin
		if Y > 1 then
			Directory := Copy(Dir, 1, Y+3)
		else
			Directory := Dir;

		ZipFile := TUnZipperEx.Create(Directory);
	end
	else
	begin
		if DirectoryExists(Dir) then
			Directory := Dir
		else
		begin
			Directory := Configuration.Application.DefaultROMPath;
			if not DirectoryExists(Directory) then
				Directory := GetCurrentDir;
		end;

		Directory := IncludeTrailingPathDelimiter(Directory);
	end;

	Rescan := True;

	if DirlistCache.Count > 0 then
	begin
		if DirlistCache[0] = Directory then
		begin
			Rescan := False;

			Files := TStringList.Create;
			Dirs  := TStringList.Create;

			Files.AddStrings(FilelistCache);

			for ID := 1 to DirlistCache.Count-1 do
				Dirs.Add(DirlistCache[ID]);
		end;
	end;

	Page.Items.Clear;
	Page.AddBackCommand;
	Page.Width := MenuRenderer.FrameBuffer.Width;
	//Page.AddHeader(Directory);
	Page.HeaderCaption := Directory;

	{$IFDEF WINDOWS}
	Page.AddEntry('[Drives]', 0, Palette[COLOR_FILE_DRIVES], CmdListDrives);
	{$ENDIF}

	if Directory.CountChar(PathDelim) > 1 then
		Page.AddEntry('[Parent directory]', 0, Palette[COLOR_FILE_PARENT], CmdBrowseToParent);

	if (not PathIsZip) and (Directory <> Configuration.Application.DefaultROMPath) then
		Page.AddEntry('[Set as default ROM path]', 0, Palette[COLOR_FILE_EXTRA], CmdSetRomPath);

	if Rescan then
	begin
		FreeAndNil(Dirs);

		if not PathIsZip then
			Dirs := FindAllDirectories(Directory, False);
		{else
			Dirs := ZipFile.ListDirectories;}

		DirlistCache.Clear;
		DirlistCache.Add(Directory);
		if Assigned(Dirs) then
			DirlistCache.AddStrings(Dirs);
	end;

	if Assigned(Dirs) then
	begin
		Dirs.Sort;
		for S in Dirs do
		begin
			if not PathIsZip then
				DS := ExtractFileName(S);
			{else
				DS := ExcludeTrailingPathDelimiter(S);}
			{$IFNDEF WINDOWS}
			if DS.StartsWith('.') then Continue;
			{$ENDIF}
			Page.AddEntry(Format('<%s>', [DS]), 0, Palette[COLOR_FILE_DIRECTORY], CmdBrowseToDirectory);
		end;
		Dirs.Free;
	end;

	// list ROM files
	//
	if Rescan then
	begin
		Files.Free;

		if not PathIsZip then
			Files := FindAllFiles(Directory, FileBrowserExts, False)
		else
		begin
			Files := ZipFile.ListFiles('', '*.nes');
			for Y := 0 to Files.Count-1 do
				Files[Y] := IncludeTrailingPathDelimiter(Directory) + Files[Y];
		end;

		FilelistCache.Clear;
		FilelistCache.AddStrings(Files);
	end;

	try
		Files.Sort;

		if ShowMappers then
		begin
			for Y := 0 to Files.Count-1 do
			begin
				S := Files[Y];
				if FileExtensionMatches(S, MovieFileExts) then Continue;

				ID := GetMapperID(S);
				if ID < 0 then
					DS := '???'
				else
					DS := Format('%.3d', [ID]);

				Files[Y] := DS + ' ' + S;
			end;

			Files.Sort;
			if not PathIsZip then
				Files.SaveToFile(Directory + '!romlist.txt');
		end;

		RomsListed := 0;
		DS := '';

		for S in Files do
		begin
			if FileExtensionMatches(S, ArchiveFileExts) then
			begin
				if PathIsZip then Continue; // no support for archives within archives
				Kind := ftArchive;
				C := Palette[COLOR_FILE_ARCHIVE];
			end
			else
			if FileExtensionMatches(S, MovieFileExts) then
			begin
				Kind := ftMovie;
				C := Palette[COLOR_FILE_MOVIE];
			end
			else
			begin
				Kind := ftRom;
				C := Palette[COLOR_FILE_ROM];
				if PathIsZip then
				begin
					Inc(RomsListed);
					DS := S;
				end;
			end;

			if ShowMappers then
			begin
				if Kind <> ftMovie then
				begin
					if TryStrToInt(Copy(S, 1, 3), ID) then
						MapperClass := GetMapperClass(ID, 0)
					else
						MapperClass := nil;
				end;

				ME := Page.AddFile(S, False,
					IfThen(Kind <> ftRom, C,
						IfThen(MapperClass <> nil, C, Palette[COLOR_FILE_BAD])),
					CmdOpenFile);
			end
			else
				ME := Page.AddFile(S, False, C, CmdOpenFile);

			// special formatting for files inside archives
			if (PathIsZip) and (ME <> nil) then
				ME.Caption := Copy(ME.Data, Length(Directory)+2, MaxInt);
		end;

	finally

		// if we find some png images allow the option to set the boxart path
		if Files.Count = 0 then
		begin
			Files.Free;
			Files := FindAllFiles(Directory, ImageFileExts, False);
			if Files.Count > 10 then
			begin
				if Directory <> Configuration.Application.ImagePath_Boxart then
					Page.AddEntry('[Set as BoxArt path]', 0, Palette[COLOR_FILE_EXTRA], CmdSetBoxArtPath);

				if caLastIndex >= caSnaps then
				if Directory <> Configuration.Application.ImagePath_Snaps then
					Page.AddEntry('[Set as Screenshots path]', 0, Palette[COLOR_FILE_EXTRA], CmdSetSnapsPath);

				if caLastIndex >= caTitles then
				if Directory <> Configuration.Application.ImagePath_Titles then
					Page.AddEntry('[Set as Titlescreens path]', 0, Palette[COLOR_FILE_EXTRA], CmdSetTitlesPath);
			end;
		end;

		Files.Free;
	end;

	MappersShown := ShowMappers;

	Menu.SwitchPage(Page);
	Menu.UpdateFilelisting;
	Menu.SetItemIndex(0);

	if PathIsZip then
	begin
		ZipFile.Free;

		// hack: if the archive only contained one .nes file, automatically open it
		if (AllowLoadRom) and (RomsListed = 1) then
			Console.LoadROM(DS);
	end;
end;

initialization

	FilelistCache := TStringList.Create;
	DirlistCache := TStringList.Create;

finalization

	FilelistCache.Free;
	DirlistCache.Free;

end.

