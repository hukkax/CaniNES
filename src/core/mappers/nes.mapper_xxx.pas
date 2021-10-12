unit NES.Mapper_XXX;

// Mapper XXX:

interface

uses
	NES.Types, NES.Cartridge, NES.Mapper;

type
	TMapper_0XX = class(TMapper)
	private
	protected
		function  GetPRGPageSize: Word; override;
		function  GetCHRPageSize: Word; override;
		function  GetWorkRamSize: Cardinal; override;
		function  GetSaveRamSize: Cardinal; override;
		function  GetChrRamSize:  Cardinal; override;

		function  GetWorkRamPageSize: Cardinal; override;
		function  GetSaveRamPageSize: Cardinal; override;
		function  GetChrRamPageSize:  Word; override;
		function  GetDipSwitchCount:  Cardinal; override;

		function  ForceSaveRamSize:  Boolean; override;
		function  ForceWorkRamSize:  Boolean; override;
		function  ForceChrBattery:   Boolean; override;
		function  HasBusConflicts:   Boolean; override;
		function  AllowRegisterRead: Boolean; override;

		function  RegisterStartAddress: Word; override;
		function  RegisterEndAddress:   Word; override;

		function  ReadRegister(addr: Word): Byte; override;
		procedure WriteRegister(addr: Word;  value: Byte); override;

		procedure InitMapper; override;
		procedure InitMapper(var RomData: TRomData); override;
	public
		procedure Reset(SoftReset: Boolean); override;

		procedure ProcessCpuClock; override;
		procedure NotifyVRAMAddressChange(addr: Word); override;
		procedure WriteRAM(addr: Word; value: Byte); override;

		procedure LoadSnapshot; override;
		procedure SaveSnapshot; override;

		procedure LoadBattery; override;
		procedure SaveBattery; override;

		constructor Create(cartridge: TCartridge); override;
		destructor  Destroy; override;
	end;


implementation

uses
	Basement.Util;

// RegisterArray(Length(someArray), @someArray[0]);

end.
