unit NES.Console;

interface

{$MODE DELPHI}
{$RANGECHECKS OFF}
{$WARN 4110 off : Range check error while evaluating constants ($1 must be between $2 and $3)}

{$I canines.inc}

uses
	Classes, Graphics32, FileStreamEx,
	NES.Types, NES.SaveState, NES.RewindManager, NES.MovieManager,
	NES.Cartridge, NES.Controllers, NES.MemoryManager,
	NES.CPU, NES.PPU, NES.APU,
	NES.Database, NES.Cheats;

var
	EmulationMode: (
		PAUSED,     NORMAL,
		STEP_FRAME, STEP_VBL,         STEP_SCANLINE,
		STEP_IRQ,   STEP_INSTRUCTION, STEP_SUBROUTINE ) = NORMAL;

const
	EmulationSpeed   = 100;

	SAVESTATEFILE    = 'savestate.dat';
	GAMEDATABASEFILE = 'database.txt';
	GAMETITLESFILE   = 'titles.txt';

    // CPU status register flags
	//
	FLAG_C = 1;   // Carry Bit
	FLAG_Z = 2;   // Zero
	FLAG_I = 4;   // Disable Interrupts
	FLAG_D = 8;   // Decimal Mode (unused)
	FLAG_B = 16;  // Break
	FLAG_U = 32;  // Unused
	FLAG_V = 64;  // Overflow
	FLAG_N = 128; // Negative

	// CPU addressing modes
	//
	ADDM_IMP = 0;
	ADDM_IMM = 1;
	ADDM_ZP0 = 2;
	ADDM_ZPX = 3;
	ADDM_ZPY = 4;
	ADDM_REL = 5;
	ADDM_ABS = 6;
	ADDM_ABX = 7;
	ADDM_ABY = 8;
	ADDM_IND = 9;
	ADDM_IZX = 10;
	ADDM_IZY = 11;

type
	TConsole = class(TSnapshotable)
	const
		// only update audio interference every N clocks to save on CPU usage
		InterferenceAudioInterval = 3;
	private
		InterferenceAudioCtr: Byte;

		RomDirs: TStringList;
	public
		// === Devices on Main Bus ===

		CPU: TCPU;  // The 6502 derived processor
		PPU: TPPU; // The 2C02 Picture Processing Unit
		APU: TAPU;  // The Audio Processing Unit

		Model:  TNESModel;
		System: TGameSystem;

		Database: TGameDatabase;
		MemoryManager: TMemoryManager;
		ControlManager: TControlManager;
		SaveStateManager: TSaveStateManager;
		RewindManager: TRewindManager;
		CheatManager: TCheatManager;
		MovieManager: TMovieManager;

		Controller: array[0..3] of TStandardController;
		CurrentControllerType: TGameInputType;

		RunAheadState: TMemoryStream;

		MRU: TStringList;

		Rewind,
		FastForward, Want_FastForward_User, Want_FastForward_Code,
		GotCartridge,
		IsRunAheadFrame: Boolean;

		LoadedFile: String;

		Interference: array[channel_InvA13..channel_InvOE1] of Byte;

		constructor Create(const AppPath: String; const Framebuffer: TBitmap32);
		destructor  Destroy; override;

		function  FindROMFile(Filename: String): String;

		function  LoadROM(Filename: String): Boolean;
		procedure InsertCartridge(const ACartridge: TCartridge);
		procedure Reset(SoftReset: Boolean; InitMapper: Boolean = True);

		function  GetFPS: Double; inline;
		function  GetFrameDelay: Double; inline;
		function  GetEmulationSpeed(IgnoreTurbo: Boolean = False): Cardinal;

		procedure Clock;
		procedure Run(cycle_count: Cardinal);
		procedure RunFrame;
		procedure RunFrameWithRunAhead;
		procedure Step;
		procedure ProcessCpuClock; inline;
		procedure ProcessInterferenceAudio; inline;

		procedure InitializeRam(data: Pointer; length: Cardinal);
		procedure WaitForFrameEnd;

		procedure SetPaused(B: Boolean);
		function  GetPaused: Boolean;
		function  TogglePause: Boolean;

		function  ToggleWAVRecording: Boolean;
		function  StartWAVRecording: Boolean;
		procedure StopWAVRecording;

		procedure ControllerSetupChanged;

		function  LoadState(TempState: Boolean = False): Boolean;
		function  SaveState(TempState: Boolean = False): Boolean;

		procedure LoadSnapshot; override;
		procedure SaveSnapshot; override;
	end;


var
	TimingInfo: record
		CPU,
		PPU,
		APU,
		BlitBuffer,
		BlitRender,
		Frame
		: Single;
	end;

	Console: TConsole;
	Cartridge: TCartridge;
	NES_CPU: TCPU;
	NES_PPU: TPPU;
	NES_APU: TAPU;


implementation

uses
//	md5, sha1, base64,
	Math, SysUtils, FileUtil, hkaFileUtils,
	Basement.Util, Basement.UnZip, TextOutput,
	{$IFDEF MEASURETIMING} Basement.Timing, {$ENDIF}
	NES.Config, NES.Mapper, NES.AudioManager,
	MainWindow;


function TConsole.GetFPS: Double;
begin
	if model = nesNTSC then
		Result := IfThen(Configuration.Emulator.IntegerFpsMode, 60.0, 60.098812)
	else
		Result := IfThen(Configuration.Emulator.IntegerFpsMode, 50.0, 50.006978);
end;

// TODO use this
//
function TConsole.GetFrameDelay: Double;
var
	EmulationSpeed: Cardinal;
begin
	EmulationSpeed := GetEmulationSpeed;

	if EmulationSpeed = 0 then
		Exit(0)
	else
	case Model of // 60.1fps (NTSC), 50.01fps (PAL/Dendy)
	nesPAL, nesDendy:
		Result := IfThen(Configuration.Emulator.IntegerFpsMode, 20, 19.99720920217466);
	else
		Result := IfThen(Configuration.Emulator.IntegerFpsMode, 16.6666666666666666667, 16.63926405550947);
	end;

	Result := Result / EmulationSpeed / 100;
end;

function TConsole.GetEmulationSpeed(IgnoreTurbo: Boolean = False): Cardinal;
begin
	Result := EmulationSpeed;
	if not IgnoreTurbo then
	begin
		if FastForward then
			Result := Configuration.Emulator.FastForwardSpeed * 100
		else
		if Rewind then
			Result := Configuration.Emulator.RewindSnapshotInterval * 100;
	end;
end;

constructor TConsole.Create(const AppPath: String; const Framebuffer: TBitmap32);
begin
	inherited Create('CANINES0');

	Console := Self;

	MemoryManager := TMemoryManager.Create;
	Database := TGameDatabase.Create(Configuration.Path,
		GAMEDATABASEFILE, GAMETITLESFILE);

	Model := nesNTSC;
	GotCartridge := False;
	FastForward  := False;
	Rewind       := False;
	Cartridge := nil;

	SaveStateManager := TSaveStateManager.Create;
	SaveStateManager.SetPath(IncludeTrailingPathDelimiter(Configuration.Path + 'saves'));

	RunAheadState := TMemoryStream.Create;

	RewindManager := TRewindManager.Create;
	CheatManager := TCheatManager.Create(Configuration.Path + 'cheats.ini');
	MovieManager := TMovieManager.Create;

	MRU := TStringList.Create;

	CPU := TCPU.Create;
	NES_CPU := CPU;

	PPU := TPPU.Create(Framebuffer);
	NES_PPU := PPU;
	PPU.SetNesModel(Model);

	{$IFDEF AUDIO}
	APU := TAPU.Create;
	NES_APU := APU;
	{$ELSE}
	NES_APU := nil;
	{$ENDIF}

	ControllerSetupChanged;

	RomDirs := TStringList.Create;
end;

destructor TConsole.Destroy;
begin
	if Assigned(Mapper) then
		Mapper.SaveBattery;

	MRU.Free;
	RomDirs.Free;
	PPU.Free;
	APU.Free;
	CPU.Free;
	MovieManager.Free;
	MemoryManager.Free;
	Database.Free;
	ControlManager.Free;
	Cartridge.Free;
	RewindManager.Free;
	CheatManager.Free;
	RunAheadState.Free;
	SaveStateManager.Free;

	inherited Destroy;
end;

function TConsole.FindROMFile(Filename: String): String;

	function DoSearch(const Fn: String): String;
	var
		S: String;
	begin
		for S in RomDirs do
		begin
			//if LowerCase(ExtractFileName(S)) = Fn then
			if FileExists(S + Fn) then
				Exit(S);
		end;
		Result := '';
	end;

var
	i: Integer;
	Dir, Fn: String;
	tmp, tmp2: TStringList;
begin
	if ExtractFileExt(Filename) = '' then
		Filename := Filename + '.nes';

	Log('FindROMFile: ' + Filename);

	Filename := {LowerCase}(ExtractFilename(Filename));

{	if RomDirs.Count < 1 then
		FindAllFiles(RomDirs, Configuration.Application.DefaultROMPath,
			RomFileExts, True);
}

	tmp  := TStringList.Create;

	if RomDirs.Count < 1 then
	begin
		//FindAllFiles(RomDirs, Configuration.Application.DefaultROMPath, RomFileExts, True);
		FindAllDirectories(RomDirs, Configuration.Application.DefaultROMPath, False);

		tmp2 := TStringList.Create;

		for i := RomDirs.Count-1 downto 0 do
		begin
			if RomDirs[i].StartsWith('.') then
				RomDirs.Delete(i)
			else
			begin
				RomDirs[i] := IncludeTrailingPathDelimiter(RomDirs[i]);

				FindAllDirectories(tmp2, RomDirs[i], True);
				tmp.AddStrings(tmp2);
				tmp2.Clear;
			end;
		end;

		RomDirs.Insert(0, Configuration.Application.DefaultROMPath);
		RomDirs.AddStrings(tmp);
		for i := 0 to RomDirs.Count-1 do
			RomDirs[i] := IncludeTrailingPathDelimiter(RomDirs[i]);

		tmp2.Free;
		tmp.Clear;
	end;

	tmp.Sorted := True;
	tmp.Duplicates := dupIgnore;
	tmp.Add(Filename);

	for i := Low(Replacements) to High(Replacements) do
	begin
		if Filename.Contains(Replacements[i, 0]) then
			tmp.Add(Filename.Replace(Replacements[i, 0], Replacements[i, 1]))
		else
		if Filename.Contains(Replacements[i, 1]) then
			tmp.Add(Filename.Replace(Replacements[i, 1], Replacements[i, 0]));
	end;

	// hacky hack
	for Fn in tmp do
	begin
		tmp.Add(Fn.Replace(' [!]', ''));
		tmp.Add(Fn.Replace(' (PRG0)', ''));
		tmp.Add(Fn.Replace(' (PRG1)', ''));
	end;

	for Fn in tmp do
	begin
		for Dir in RomDirs do
		begin
			if FileExists(Dir + Fn) then
			begin
				Result := Dir + Fn;
				tmp.Free;
				Exit;
			end;
		end;
	end;

	tmp.Free;
	Result := '';
end;

function TConsole.LoadROM(Filename: String): Boolean;
var
	TempCartridge: TCartridge;
	MovieGotROM: Boolean;
	ZP: TZipParts;
	ZipFile: TUnZipperEx;
	FileData, IPSData: TBytes;
begin
	LogVerbose('Console.LoadROM: ' + Filename);

	ZP.GetParts(Filename);

	// C:\foo\Rom.zip\subdir\Foo.nes
	//
	if ZP.IsZip then
	begin
		// Filename = C:\foo\Rom.zip
		// ZipFilename = Rom.zip
		// ZipPath = subdir\Foo.nes
		LoadedFile := Filename;
		ZP.ZipFilename := ExtractFileName(ZP.Filename);
		{if FileExists(TempPath + ZP.ZipFilename) then
			Filename := TempPath + ZP.ZipFilename // already extracted earlier
		else}
		begin
			Filename := ZP.Filename;
			ZipFile := TUnZipperEx.Create(Filename);
			try
				//ZipFile.OutputPath := TempPath;
				//ForceDirectories(TempPath);
				Log('Extracting %s from %s...', [ExtractFilename(ZP.ZipPath), ZP.Filename]);

				//ZipFile.UnZipFile(ZP.ZipPath);
				FileData := ZipFile.ExtractFile(ZP.ZipPath);
				IPSData  := ZipFile.ExtractFile(ChangeFileExt(ZP.ZipPath, '.ips'));

				//Filename := TempPath + ZP.ZipPath;
				Filename := ExtractFilename(ZP.ZipPath);
			finally
				ZipFile.Free;
			end;
		end;
	end
	else
	if not FileExists(Filename) then
	begin
		Message('File not found: ' + Filename);
		Exit(False);
	end;

	MovieGotROM := False;

	// is this a movie file?
	if FileExtensionMatches(Filename, MovieFileExts) then
	begin
		if MovieManager.LoadFromFile(Filename) then
		begin
			Filename := FindROMFile(MovieManager.MovieInfo.ROMfile);
			MovieGotROM := not Filename.IsEmpty;
			Window.UpdateMenus;
			if not MovieGotROM then
			begin
				Message('Could not find associated ROM: ' +
					ExtractFilename(MovieManager.MovieInfo.ROMfile) + '!');
				Exit;
			end;
		end;
	end;

	if (GotCartridge) and (Configuration.Application.RestoreROMState) then
		SaveState(True);

//	Log('Load ROM: ' + Filename);

	// Ensure we save any battery file before loading a new game
	if Assigned(Mapper) then
		Mapper.SaveBattery;

	ControlManager.Reset;

	Randomize;

	TempCartridge := TCartridge.Create(Filename, FileData, IPSData);

	Result := TempCartridge.ImageValid;

	// Insert into NES
	if Result then
	begin
		if not ZP.IsZip then
			LoadedFile := Filename;

		if not MovieGotROM then
			Configuration.Application.LastROMFile := LoadedFile;

		InsertCartridge(TempCartridge);

		Window.ROMLoaded;

		if Configuration.Application.RestoreROMState then
			LoadState(True);

		if (MovieManager.Loaded) and (MovieGotROM) then
			MovieManager.Play;

		ControlManager.PadVisualizationChanged;
	end
	else
	begin
		Message('ROM invalid or unsupported!');
		TempCartridge.Free;
	end;

	LogVerbose('Console.LoadROM done');
end;

procedure TConsole.InsertCartridge(const ACartridge: TCartridge);
var
	i: Integer;
//ss,s: String;
begin
	if not ACartridge.ImageValid then Exit;

	APU.Stop;

	// Connects cartridge to both Main Bus and CPU Bus
	Cartridge.Free;
	Cartridge := ACartridge;
	GotCartridge := True;

	RewindManager.Stop;

{
log('[SHA1] '+SHA1Print(SHA1File(LoadedFile)));
log('[MD5]  '+MD5Print(MD5File(LoadedFile)));

ss := '';
s := DecodeStringBase64('cefvdUpxbO1bMGoeW26TwQ==');
for i := 1 to Length(s) do
	ss := ss + IntToHex(Ord(s[i]),2);
log('[MD5] '+ss);
}


	// MRU
	MRU.Clear; // transfer from settings
	for i := 0 to MRUcount-1 do
	begin
		if Configuration.Application.MRU[i] <> '' then
			MRU.Add(Configuration.Application.MRU[i]);
		Configuration.Application.MRU[i] := '';
	end;
	i := MRU.IndexOf(LoadedFile);
	if i >= 0 then
		MRU.Move(i, 0)
	else
		MRU.Insert(0, LoadedFile);
	while MRU.Count > MRUcount do
		MRU.Delete(MRU.Count - 1);
	for i := 0 to MRU.Count-1 do // transfer to settings
		Configuration.Application.MRU[i] := MRU[i];

	SaveStateManager.UpdateStateAvailability;

	// set model
	if Configuration.Emulator.NESModel = nesAuto then
	begin
		System := Cartridge.RomData.Info.System;
		case System of
			TGameSystem.NES_PAL: Model := nesPAL;
			TGameSystem.Dendy:   Model := nesDendy;
			else                 Model := nesNTSC;
		end
	end
	else
	begin
		Model := Configuration.Emulator.NESModel;
		System := TGameSystem.Unknown; // fixme
	end;

	Mapper.SetNesModel(Model);

	APU.Settings := Configuration.Emulator.APU;
	APU.SetNesModel(Model, True);

	PPU.Settings := Configuration.Emulator.PPU;
	{$IFDEF USE_NTSC_FILTER}
	PPU.ConfigureNTSCFilter(Configuration.Display.NTSC);
	{$ENDIF}
	PPU.SetNesModel(Model);
	PPU.ConnectCartridge(Cartridge);

	if Cartridge.RomData.Info.DatabaseInfo.InputType <> CurrentControllerType then
	begin
		CurrentControllerType := Cartridge.RomData.Info.DatabaseInfo.InputType;
		ControllerSetupChanged;
	end;

	MemoryManager.SetMapper(Mapper);
	MemoryManager.RegisterIODevice(PPU);
	MemoryManager.RegisterIODevice(APU);
	MemoryManager.RegisterIODevice(Mapper);
	MemoryManager.RegisterIODevice(ControlManager);

	Mapper.Initialize(Cartridge.RomData);

	Reset(False, False);
	RewindManager.Initialize;

	Window.UpdateMenus;
end;

procedure TConsole.ControllerSetupChanged;
var
	CT: TGameInputType;
begin
	if Configuration.Input.Zapper.Enabled then
		CT := gitZapper
	else
		CT := CurrentControllerType;

	if Assigned(ControlManager) then
		ControlManager.Free;
	ControlManager := TControlManager.Create('CTRL', CT);

	Window.ControllerSetupChanged;
end;

procedure TConsole.Reset(SoftReset: Boolean; InitMapper: Boolean = True);
begin
	if not GotCartridge then Exit;

	{$IFDEF DEBUG}
	//Log('Console.Reset: ' + SoftReset.ToString);
	{$ENDIF}

	MovieManager.Reset;
	MemoryManager.Reset(SoftReset);
	PPU.Reset;
	{$IFDEF AUDIO}
	APU.Reset(SoftReset);
	{$ENDIF}

	ControlManager.Reset;
	if (InitMapper) and (not SoftReset) and Assigned(Mapper) then
		Mapper.Initialize(Cartridge.RomData);

	CPU.Reset(SoftReset, Model);
end;

procedure TConsole.InitializeRam(data: Pointer; length: Cardinal);
var
	i, Value: Integer;
begin
	if (data <> nil) and (length > 0) then
	begin
		case Configuration.Emulator.PowerOnState.DefaultRAMState of
			irRandom:   Value := -1;
			irAllOnes:  Value :=  1;
			else        Value :=  0;
		end;

		// override initialization value from database
		if (Configuration.Application.EnableDatabase) and
			(Configuration.Emulator.PowerOnState.RAMStateOverride) then
		begin
			i := Cartridge.RomData.Info.DatabaseInfo.InitRamValue;
			case i of
				INITRAM_DEFAULT: ;
				INITRAM_RANDOM:  Value := -1;
				else             Value :=  i;
			end;
		end;

		if Value < 0 then
		begin
			for i := 0 to length-1 do
			begin
				PByte(data)^ := Random($FF);
				Inc(PByte(data));
			end;
		end
		else
			FillByte(data^, length, Value);
	end;
end;

procedure TConsole.Clock;
begin
	CPU.Exec;
end;

procedure TConsole.Step;
begin
	CPU.Exec;
end;

procedure TConsole.Run(cycle_count: Cardinal);
begin
//	Inc(cycle_count, cpu_time);
//	while (cpu_time < cycle_count) do Clock;
end;

procedure TConsole.RunFrame;
begin
	// Reset frame completion flag
	PPU.frame_complete := False;

	if (not IsRunAheadFrame) and (MovieManager.IsPlaying) then
		MovieManager.Update;

	WaitForFrameEnd;
end;

procedure TConsole.RunFrameWithRunAhead;
var
	RunAheadFrames: Integer;
begin
	RunAheadFrames := Configuration.Emulator.RunAheadFrames;

	// Run a single frame and save the state (no audio/video)
	IsRunAheadFrame := True;
	RunFrame;
	RunAheadState.Clear;
	SaveStateManager.SaveState(RunAheadState);

	while RunAheadFrames > 1 do
	begin
		// Run extra frames if the requested run ahead frame count is higher than 1
		Dec(RunAheadFrames);
		RunFrame;
	end;

	// Run one frame normally (with audio/video output)
	IsRunAheadFrame := False;
	RunFrame;

	IsRunAheadFrame := True;
	RunAheadState.Position := 0;
	SaveStateManager.LoadState(RunAheadState);
	IsRunAheadFrame := False;
end;

procedure TConsole.ProcessInterferenceAudio;
begin
	if APUChannelUsed[channel_InvA13] then
	begin
		Mapper.InvA13Audio.Interference := PPU.A13pinLowSum;
		PPU.A13pinLowSum := 0;
	end;

	if APUChannelUsed[channel_InvOE1] then
	begin
		ControlManager.GetInvOE1;
	end;
end;

procedure TConsole.ProcessCpuClock;
begin
	if Mapper = nil then Exit;

	InterferenceAudioCtr := (InterferenceAudioCtr + 1) mod InterferenceAudioInterval;
	if InterferenceAudioCtr = 0 then
		ProcessInterferenceAudio;

	Mapper.ProcessCpuClock;

	if InterferenceAudioCtr = 0 then
	begin
		if APUChannelUsed[channel_InvA13] then Mapper.InvA13Audio.Clock;
		if APUChannelUsed[channel_InvOE1] then Mapper.InvOE1Audio.Clock;
	end;

	{$IFDEF AUDIO}
	APU.ProcessCpuClock;
	{$ENDIF}
end;

procedure TConsole.WaitForFrameEnd;
begin
	if not GotCartridge then Exit;

	{$IFDEF MEASURETIMING}
	Timing.Start;
	CPU.ExecFrame;
	TimingInfo.CPU := Timing.Stop - PPUTimeTaken;
	TimingInfo.PPU := PPUTimeTaken;
	{$ELSE}
	CPU.ExecFrame;
	{$ENDIF}

	{$IFDEF AUDIO}
	APU.EndFrame;
	{$ENDIF}

	if not IsRunAheadFrame then
		RewindManager.Update;
end;

(*
void Console::UpdateNesModel(bool sendNotification)
{
	bool configChanged = false;
	if(_settings->NeedControllerUpdate()) {
		_controlManager->UpdateControlDevices();
		configChanged = true;
	}

	NesModel model = _settings->GetNesModel();
	if(model == NesModel::Auto) {
		switch(_mapper->GetRomInfo().System) {
			case GameSystem::NesPal: model = NesModel::PAL; break;
			case GameSystem::Dendy: model = NesModel::Dendy; break;
			default: model = NesModel::NTSC; break;
		}
	}
	if(_model != model) {
		_model = model;
		configChanged = true;

		if(sendNotification) {
			MessageManager::DisplayMessage("Region", model == NesModel::PAL ? "PAL" : (model == NesModel::Dendy ? "Dendy" : "NTSC"));
		}
	}

	_cpu->SetMasterClockDivider(model);
	_mapper->SetNesModel(model);
	_ppu->SetNesModel(model);
	_apu->SetNesModel(model);

	if(configChanged && sendNotification) {
		_notificationManager->SendNotification(ConsoleNotificationType::ConfigChanged);
	}
}
*)

procedure TConsole.LoadSnapshot;
begin
	inherited LoadSnapshot;

	if (not RewindManager.Snapshoting) and (not IsRunAheadFrame) then
	begin
		Rewind := False;

		//WaitForFrameEnd;
		//APU.EndFrame;

		if Cartridge.RomData.Info.Hash.Crc32 <> Stream.ReadDWord then
			Log('CRC32 mismatch!');

		StreamReadString(Stream);  // ROM file path
	end;

	CPU.LoadSnapshot;
	PPU.LoadSnapshot;
	MemoryManager.LoadSnapshot;
	APU.LoadSnapshot;
	ControlManager.LoadSnapshot;
	Mapper.LoadSnapshot;

//	UpdateNESModel(False);

	if (not RewindManager.Snapshoting) and (not IsRunAheadFrame) then
		RewindManager.Initialize;
end;

procedure TConsole.SaveSnapshot;
begin
	inherited SaveSnapshot;

	if (not RewindManager.Snapshoting) and (not IsRunAheadFrame) then
	begin
		// APU.EndFrame;
		Stream.WriteDWord(Cartridge.RomData.Info.Hash.Crc32);
		StreamWriteString(Stream, Cartridge.Filename);
	end;

	CPU.SaveSnapshot;
	PPU.SaveSnapshot;
	MemoryManager.SaveSnapshot;
	APU.SaveSnapshot;
	ControlManager.SaveSnapshot;
	Mapper.SaveSnapshot;
end;

function TConsole.LoadState(TempState: Boolean = False): Boolean;
begin
	if not Console.GotCartridge then Exit(False);

	if TempState then
		Result := SaveStateManager.LoadState(SAVESLOT_TEMP)
	else
		Result := SaveStateManager.LoadState;
end;

function TConsole.SaveState(TempState: Boolean = False): Boolean;
begin
	if not Console.GotCartridge then Exit(False);

	WaitForFrameEnd;

	if TempState then
		Result := SaveStateManager.SaveState(SAVESLOT_TEMP)
	else
		Result := SaveStateManager.SaveState;
end;

procedure TConsole.SetPaused(B: Boolean);
begin
	APU.Stop;
	OSD('');

	if not B then
		EmulationMode := NORMAL
	else
		EmulationMode := PAUSED;
end;

function TConsole.GetPaused: Boolean;
begin
	Result := (EmulationMode = PAUSED);
end;

function TConsole.TogglePause: Boolean;
begin
	SetPaused(EmulationMode <> PAUSED);
	Result := GetPaused;
end;

function TConsole.ToggleWAVRecording: Boolean;
begin
	if APU.WavRecording then
		StopWAVRecording
	else
		StartWAVRecording;

	Result := APU.WAVRecording;
end;

function TConsole.StartWAVRecording: Boolean;
const
	Ext = '.wav';
var
	Fn, Dir: String;
begin
	if APU.WavRecording then Exit(False);

	if (not GotCartridge) or (LoadedFile.IsEmpty) then
		Fn := APPNAME + Ext
	else
		Fn := ChangeFileExt(ExtractFilename(LoadedFile), Ext);

	Dir := Configuration.Application.RecordingPath;
	if (Dir.IsEmpty) or (not DirectoryExists(Dir)) then
		Dir := ConfigPath;

	Fn := UniqueFilename(IncludeTrailingPathDelimiter(Dir) + Fn);

	Result := APU.StartRecording(Fn);

	if Result then
	begin
		Message('Recording audio to ' + Fn + '.');
		Window.UpdateMenus;
	end;
end;

procedure TConsole.StopWAVRecording;
begin
	if APU.WavRecording then
	begin
		APU.StopRecording;
		Message('Audio recording stopped.');
		Window.UpdateMenus;
	end;
end;


end.
