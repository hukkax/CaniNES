unit NES.MovieManager;

{$mode Delphi}

interface

uses
	Classes, SysUtils,
	Zipper, NES.Controllers;

type
	TMovieInfo = record
		ROMfile:    String; // need to load a ROM matching this filename
		FrameCount: Cardinal;
		IsPAL:      Boolean;
		Ports:      array[0..2] of Byte;
	end;

	TMovieFrameAction = bitpacked record
	case Byte of
	   0: (	Value: Byte );
	   1: (	Subtitle, // bit 4
		    DiskSide, // bit 3
		    DiskSet,  // bit 2
		    Power,    // bit 1
		    Reset     // bit 0
			: Boolean );
	end;

	TMovieFrame = packed record
		Action:     TMovieFrameAction;
		Controller: array[0..3] of TStandardControllerState;
	end;
	PMovieFrame = ^TMovieFrame;

	TMovieManager = class
	private
		sl: TStringList;
		procedure DoCreateOutZipStream(Sender: TObject; var AStream: TStream;
			AItem: TFullZipFileEntry);
		procedure DoDoneOutZipStream(Sender: TObject; var AStream: TStream;
			AItem: TFullZipFileEntry);
		function ExtractFileFromZip(Zip: TUnZipper; const FileName: String): Boolean;
	protected
		Frames:    array of TMovieFrame;
		Subtitles: TStringList;
		//OriginalPowerOnState: TRamPowerOnState;

		function  ParseBizhawk: Boolean;
		function  ParseFCEU:    Boolean;
		function  ParseMesen:   Boolean;
	public
		Loaded,
		IsPlaying: Boolean;

		CurrentFrame,
		CurrentSubtitle: Cardinal;
		Filename:  String;

		CurrentFrameData: PMovieFrame;
		MovieInfo: TMovieInfo;

		function  LoadFromFile(const AFilename: String): Boolean;

		function  Update: Boolean;
		procedure Play;
		procedure Stop;
		procedure Reset;

		constructor Create;
		destructor  Destroy; override;
	end;

implementation

uses
	TextOutput, Basement.Util,
	NES.Console;


constructor TMovieManager.Create;
begin
	inherited Create;

	sl := TStringList.Create;
	Subtitles := TStringList.Create;
end;

destructor TMovieManager.Destroy;
begin
	Subtitles.Free;
	sl.Free;

	inherited Destroy;
end;

procedure TMovieManager.Reset;
begin
	CurrentFrame := 0;
	CurrentSubtitle := 0;
	IsPlaying := False;
	if Loaded then
		Log('[Movie] Playback reset.');
end;

procedure TMovieManager.Play;
begin
	if (IsPlaying) or (not Loaded) then Exit;

	IsPlaying := True;
	Message('Movie playback started.');
end;

procedure TMovieManager.Stop;
begin
	if not IsPlaying then Exit;

	IsPlaying := False;
	Message('Movie playback stopped.');
end;

function TMovieManager.Update: Boolean;
begin
	if not IsPlaying then Exit(False);

	CurrentFrame := NES_PPU.frameCount-1;
	if CurrentFrame >= MovieInfo.FrameCount then
	begin
		Stop;
		Exit(True);
	end;

	CurrentFrameData := @Frames[CurrentFrame];

	Console.ControlManager.PrimaryController.SetStateFromInput(CurrentFrameData.Controller[0].value);
	Console.ControlManager.SecondaryController.SetStateFromInput(CurrentFrameData.Controller[1].value);

	if CurrentFrameData.Action.Subtitle then
	begin
		if (Subtitles.Count > 0) and (CurrentSubtitle < Subtitles.Count) then
		begin
			OSD(Subtitles[CurrentSubtitle]);
			Inc(CurrentSubtitle);
		end;
	end;

	Result := True;
end;

function TMovieManager.LoadFromFile(const AFilename: String): Boolean;
var
	Ext: String;
begin
	Result := False;
	if not FileExists(AFilename) then
	begin
		Log('[Movie] File not found: ' + AFilename);
		Exit;
	end
	else
		Log('[Movie] Loading file: ' + AFilename);

	Ext := UpperCase(ExtractFileExt(AFilename));
	Filename := AFilename;

	sl.Clear;
	SetLength(Frames, 0);

	if Ext = '.MMO' then
	begin
		Log('[Movie] Parsing Mesen movie...');
		Result := ParseMesen;
	end
	else
	{if Ext = '.BK2' then
	begin
		Log('[Movie] Parsing Bizhawk movie...');
		Result := ParseBizhawk;
	end
	else}
    if '.FM2 .FM3'.Contains(Ext) then
	begin
		Log('[Movie] Parsing FCEU movie...');
		Result := ParseFCEU;
	end
	else
	begin
		Filename := '';
		Log('[Movie] Unsupported format!');
	end;

	if Result then
	begin
		Log('[Movie] Parsed %d frames.', [MovieInfo.FrameCount]);
		Reset;
	end;
	{else
		Log('[Movie] Parsing failed.');}

	Loaded := Result;
	sl.Clear;
end;


// ==========================================================================
// Bizhawk movie (bk2)
// ==========================================================================

function TMovieManager.ParseBizhawk: Boolean;
begin
	Result := False;
end;


// ==========================================================================
// FCEU movie (fm2/fm3)
// ==========================================================================

function TMovieManager.ParseFCEU: Boolean;
var
	X, Y, F, CP, IC: Integer;
	S, C, W:   String;
	IsBinary:  Boolean;
	MaxFrames: Cardinal;
	slFrames:  TStringList;
begin
	Result := False;

	IsBinary := False;
	MaxFrames := High(Cardinal);
	MovieInfo.FrameCount := 0;
	MovieInfo.ROMfile := '';
	MovieInfo.IsPAL := False;
	Subtitles.Clear;

	try
		sl.LoadFromFile(Filename);
	except
		Exit;
	end;

	// Parse header
	//
	for Y := 0 to sl.Count-1 do
	begin
		S := sl[Y];
		if S.Length < 1 then Continue;

		if S.StartsWith('|') then
			Break
		else
		begin
			if not SplitWords(S, ' ', C, W) then Continue;

			C := LowerCase(C);

			{
			[ ] version (required)
			[ ] emuVersion (required)
			[ ] rerecordCount (optional)
TODO		[X] palFlag (bool) (optional)
			[ ] NewPPU (bool) (optional)
TODO		[ ] FDS (bool) (optional)
			[ ] fourscore (bool)
TODO		[ ] port0
TODO		[ ] port1
			[ ] port2 (required)
TODO		[X] binary (bool) (optional)
			[X] length (required in FM3)

			[X] romFilename (required)
TODO		[ ] romChecksum (required)
TODO		[ ] comment (optional)
			[X] subtitle (optional)
TODO		[ ] guid (required)
			}
			if C = 'binary' then
				IsBinary := '1 true'.Contains(LowerCase(W))
			else
			if C = 'palflag' then
				MovieInfo.IsPAL := '1 true'.Contains(LowerCase(W))
			else
			if C = 'romfilename' then
				MovieInfo.ROMfile := W
			else
			if C = 'length' then
				MaxFrames := W.ToInteger
			else
			if C = 'subtitle' then
				Subtitles.Add(W);
		end;
	end;

	// Parse frames
	//
	if IsBinary then
	begin
		Log('[Movie] Binary format not implemented!');
	end
	else
	begin
		slFrames := TStringList.Create;

		for Y := 0 to sl.Count-1 do // count them first
		begin
			S := sl[Y];
			if (S.StartsWith('|')) and (S.Length >= 11) then
			begin
				slFrames.Add(S);
				Inc(MovieInfo.FrameCount);
				if MovieInfo.FrameCount >= MaxFrames then Break;
			end;
		end;

		// insert 4 empty frames at the start
		F := 4;
		Inc(MovieInfo.FrameCount, F);

		SetLength(Frames, MovieInfo.FrameCount);

		for Y := 0 to F-1 do
			Frames[Y] := Default(TMovieFrame);

		for Y := 0 to slFrames.Count-1 do
		begin
			S := slFrames[Y];

			IC := StrToInt(S[2]);
			Frames[F].Action.Value := IC;

			S := Copy(S.Replace('|', ''), 2, MaxInt);
			S := Trim(S).Replace(' ', '.');

			// "|0|....T...|||"          -> "....T..."
			// "|0|RLDUT..A|R..UTSBA||"  -> "RLDUT..AR..UTSBA"

			for CP := 0 to 2 do
			begin
				IC := 0;
				if S.Length >= 8 then // read controller bits
				begin
					for X := 1 to 8 do
					begin
						IC := IC shl 1;
						if S[X] <> '.' then
							IC := IC or 1;
					end;
					S := S.Remove(0, 8);
				end;
				Frames[F].Controller[CP].value := IC;
			end;

			Inc(F);
		end;

		slFrames.Free;
	end;

	for Y := 0 to Subtitles.Count-1 do
	begin
		if not SplitWords(Subtitles[Y], ' ', C, W) then Continue;
		Subtitles[Y] := W;
		IC := C.ToInteger;
		if (IC >= 0) and (IC <= MovieInfo.FrameCount) then
			Frames[IC].Action.Subtitle := True;
	end;

	Result := (MovieInfo.FrameCount > 0);
end;


// ==========================================================================
// Mesen movie (mmo)
// ==========================================================================

procedure TMovieManager.DoCreateOutZipStream(Sender: TObject;
	var AStream: TStream; AItem: TFullZipFileEntry);
begin
	AStream := TMemoryStream.Create;
end;

procedure TMovieManager.DoDoneOutZipStream(Sender: TObject;
	var AStream: TStream; AItem: TFullZipFileEntry);
begin
	AStream.Position := 0;
	sl.LoadFromStream(Astream);
	AStream.Free;
end;

function TMovieManager.ExtractFileFromZip(Zip: TUnZipper; const FileName: String): Boolean;
var
	s: TStringList;
begin
	Result := False;
	s := TStringList.Create;
	s.Add(FileName);
	try
		Zip.UnZipFiles(s);
		Result := s.Count > 0;
	finally
		s.Free;
	end;
end;


function TMovieManager.ParseMesen: Boolean;
var
	ZipFile: TUnZipper;

	function LoadFile(const Fn: String): Boolean;
    begin
		try
			sl.Clear;
			//sl.LoadFromFile(Path + Fn);
			Result := ExtractFileFromZip(ZipFile, Fn);
		except
	    	Result := False;
		end;
    end;

var
	X, Y, F, CP, IC: Integer;
	S, C, W: String;
	MaxFrames: Cardinal;
	slFrames:  TStringList;
begin
	Result := False;

	MaxFrames := High(Cardinal);
	MovieInfo.FrameCount := 0;
	MovieInfo.ROMfile := '';
	MovieInfo.IsPAL := False;
	Subtitles.Clear;

	ZipFile := TUnZipper.Create;
	ZipFile.FileName := Filename;
	ZipFile.OnCreateStream := DoCreateOutZipStream;
	ZipFile.OnDoneStream   := DoDoneOutZipStream;

    if not LoadFile('GameSettings.txt') then
	begin
		Log('[Movie] Could not open game settings!');
		Exit;
	end;

	for Y := 0 to sl.Count-1 do
	begin
		S := sl[Y];
		if S.Length < 1 then Continue;

		if not SplitWords(S, ' ', C, W) then Continue;

		C := LowerCase(C);

		{
		[ ] str		MesenVersion
		[ ] int		MovieFormatVersion
		[X] str		GameFile
		[ ] str		SHA1
		[X] str		Region
		[ ] str		ConsoleType
		[ ] str		Controller1
		[ ] str		Controller2
		[ ] int		CpuClockRate
		[ ] int		ExtraScanlinesBeforeNmi
		[ ] int		ExtraScanlinesAfterNmi
		[ ] int		InputPollScanline
		[ ] bool	DisablePpu2004Reads
		[ ] bool	DisablePaletteRead
		[ ] bool	DisableOamAddrBug
		[ ] bool	UseNes101Hvc101Behavior
		[ ] bool	EnableOamDecay
		[ ] bool	DisablePpuReset
		[ ] int		ZapperDetectionRadius
		[ ] int		RamPowerOnState
		}

		if C = 'region' then
			MovieInfo.IsPAL := (LowerCase(W) = 'pal')
		else
		if C = 'gamefile' then
			MovieInfo.ROMfile := W;
	end;

    // -------------------------------------------------------------

    //LoadFile('MovieInfo.txt');
    // TODO

    // -------------------------------------------------------------

    if LoadFile('Input.txt') then
	begin
		slFrames := TStringList.Create;

		for Y := 0 to sl.Count-1 do // count them first
		begin
			S := sl[Y];
			if (S.StartsWith('|')) and (S.Length >= 11) then
			begin
				slFrames.Add(S);
				Inc(MovieInfo.FrameCount);
				if MovieInfo.FrameCount >= MaxFrames then Break;
			end;
		end;

		F := 0;
		SetLength(Frames, MovieInfo.FrameCount);

		for Y := 0 to slFrames.Count-1 do
		begin
			S := slFrames[Y];

	//		IC := StrToInt(Copy(S, 2, 2));
			IC := 0; // !!! TODO
			Frames[F].Action.Value := IC;

			S := Copy(S.Replace('|', ''), 3, MaxInt);
			S := Trim(S).Replace(' ', '.');

			X := 1;
			for CP := 0 to 2 do
			begin
				IC := 0;
				if (X + 7) <= S.Length then // read controller bits
				begin
					with Frames[F].Controller[CP] do // UDLRTSBA
					begin
						Up     := S[X+0] <> '.';
						Down   := S[X+1] <> '.';
						Left   := S[X+2] <> '.';
						Right  := S[X+3] <> '.';
						Start  := S[X+4] <> '.';
						Select := S[X+5] <> '.';
						B      := S[X+6] <> '.';
						A      := S[X+7] <> '.';
					end;
					Inc(X, 8);
				end;
			end;

			Inc(F);
		end;

		slFrames.Free;
	end;

    // -------------------------------------------------------------
	// seemingly not supported by Mesen but I thought I'd add this just in case
	//
	if LoadFile('Subtitles.txt') then
	begin
		Subtitles.Clear;

		for Y := 0 to sl.Count-1 do
		begin
			if not SplitWords(sl[Y], ' ', C, W) then Continue;
			IC := C.ToInteger;
			if (IC >= 0) and (IC <= MovieInfo.FrameCount) then
			begin
				Frames[IC].Action.Subtitle := True;
				Subtitles.Add(W);
			end;
		end;
	end;

	ZipFile.Free;

	Result := (MovieInfo.FrameCount > 0);
end;



end.

