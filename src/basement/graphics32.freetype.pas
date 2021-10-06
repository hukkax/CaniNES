{
 *****************************************************************************
  Original file: EasyLazFreeType.pas (part of LazUtils).

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit Graphics32.FreeType;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, Laz_AVL_Tree,
	LazUTF8, LazFreeType, TTRASTER, TTTypes, TTObjs, Types,
	Graphics32, Graphics32.LowLevel;

type
	TGlyphRenderQuality = (grqMonochrome, grqLowQuality, grqHighQuality);

	ArrayOfSingle= array of single;

	TCharPosition= record
		x, width, yTop, yBase, yBottom: single;
	end;

	ArrayOfCharPosition = array of TCharPosition;

	TFreeTypeAlignment = (ftaLeft,ftaCenter,ftaRight,ftaJustify,ftaTop,ftaVerticalCenter,ftaBaseline,ftaBottom);
	TFreeTypeAlignments = set of TFreeTypeAlignment;

	TFreeTypeInformation = (ftiCopyrightNotice, ftiFamily, ftiStyle, ftiIdentifier, ftiFullName,
		ftiVersionString, ftiPostscriptName, ftiTrademark, ftiManufacturer, ftiDesigner,
		ftiVendorURL, ftiDesignerURL, ftiLicenseDescription, ftiLicenseInfoURL);

	TFreeTypeStyle = (ftsBold, ftsItalic);
	TFreeTypeStyles = set of TFreeTypeStyle;
	TFreeTypeWordBreakHandler = procedure(var ABefore, AAfter: string) of object;

const
	FreeTypeInformationStr : array[TFreeTypeInformation] of String =
		('Copyright notice', 'Family', 'Style', 'Identifier', 'Full name',
		'Version string', 'Postscript name', 'Trademark', 'Manufacturer', 'Designer',
		'Vendor URL', 'Designer URL', 'License description', 'License info URL');

type
  TFreeTypeGlyph = class;
  TFreeTypeFont = class;
  TFreeTypeKerning = record
    Kerning, Minimum: TPointF;
    Found: boolean;
  end;

  EFreeType = class(Exception);

  TFontCollectionItemDestroyProc = procedure() of object;
  TFontCollectionItemDestroyListener = record
    TargetObject: TObject;
    NotifyProc: TFontCollectionItemDestroyProc;
  end;

  function FontCollectionItemDestroyListener(ATargetObject: TObject; ANotifyProc: TFontCollectionItemDestroyProc): TFontCollectionItemDestroyListener;

type
  ArrayOfFontCollectionItemDestroyListener = array of TFontCollectionItemDestroyListener;
  TCustomFamilyCollectionItem = class;

  { TCustomFontCollectionItem }

  TCustomFontCollectionItem = class
  protected
    FFamily: TCustomFamilyCollectionItem;
    function GetBold: boolean; virtual; abstract;
    function GetInformation(AIndex: TFreeTypeInformation): string; virtual; abstract;
    function GetItalic: boolean; virtual; abstract;
    function GetStyleCount: integer; virtual; abstract;
    function GetStyles: string; virtual; abstract;
    function GetFilename: string; virtual; abstract;
    function GetVersionNumber: string; virtual; abstract;
    function GetStyle(AIndex: integer): string; virtual; abstract;
    procedure NotifyDestroy; virtual; abstract;
  public
    function HasStyle(AStyle: string): boolean; virtual; abstract;
    function CreateFont: TFreeTypeFont; virtual; abstract;
    function QueryFace(AListener: TFontCollectionItemDestroyListener): TT_Face; virtual; abstract;
    procedure ReleaseFace(AListener: TFontCollectionItemDestroyListener); virtual; abstract;

    property Styles: string read GetStyles;
    property Italic: boolean read GetItalic;
    property Bold: boolean read GetBold;
    property Filename: string read GetFilename;
    property Information[AIndex: TFreeTypeInformation]: string read GetInformation;
    property VersionNumber: string read GetVersionNumber;
    property Style[AIndex: integer]: string read GetStyle;
    property StyleCount: integer read GetStyleCount;
    property Family: TCustomFamilyCollectionItem read FFamily write FFamily;
  end;

  IFreeTypeFontEnumerator = interface
    function MoveNext: boolean;
    function GetCurrent: TCustomFontCollectionItem;
    property Current: TCustomFontCollectionItem read GetCurrent;
  end;

  { TCustomFamilyCollectionItem }

  TCustomFamilyCollectionItem = class
  protected
    function GetFontByIndex(AIndex: integer): TCustomFontCollectionItem; virtual; abstract;
    function GetStyle(AIndex: integer): string; virtual; abstract;
    function GetStyles: string; virtual; abstract;
    function GetFamilyName: string; virtual; abstract;
    function GetFontCount: integer; virtual; abstract;
    function GetStyleCount: integer; virtual; abstract;
  public
    function GetFont(const AStyles: array of string; NeedAllStyles: boolean = false; NoMoreStyle: boolean = false): TCustomFontCollectionItem; virtual; abstract; overload;
    function GetFont(AStyle: string; NeedAllStyles: boolean = false; NoMoreStyle: boolean = false): TCustomFontCollectionItem; virtual; abstract; overload;
    function GetFontIndex(const AStyles: array of string; NeedAllStyles: boolean = false; NoMoreStyle: boolean = false): integer; virtual; abstract; overload;
    function GetFontIndex(AStyle: string; NeedAllStyles: boolean = false; NoMoreStyle: boolean = false): integer; virtual; abstract; overload;
    function HasStyle(AName: string): boolean; virtual; abstract;
    property FamilyName: string read GetFamilyName;
    property Font[AIndex: integer]: TCustomFontCollectionItem read GetFontByIndex;
    property FontCount: integer read GetFontCount;
    property Style[AIndex: integer]: string read GetStyle;
    property StyleCount: integer read GetStyleCount;
    property Styles: string read GetStyles;
  end;

  IFreeTypeFamilyEnumerator = interface
    function MoveNext: boolean;
    function GetCurrent: TCustomFamilyCollectionItem;
    property Current: TCustomFamilyCollectionItem read GetCurrent;
  end;

  { TCustomFreeTypeFontCollection }

  TCustomFreeTypeFontCollection = class
  protected
    function GetFont(AFileName: string): TCustomFontCollectionItem; virtual; abstract;
    function GetFamily(AName: string): TCustomFamilyCollectionItem; virtual; abstract;
    function GetFamilyCount: integer; virtual; abstract;
    function GetFontCount: integer; virtual; abstract;
  public
    constructor Create; virtual; abstract;
    procedure Clear; virtual; abstract;
    procedure BeginUpdate; virtual; abstract;
    procedure AddFolder(AFolder: string; AIncludeSubdirs: Boolean = false); virtual; abstract;
    procedure RemoveFolder(AFolder: string); virtual; abstract;
    function AddFile(AFilename: string): TCustomFontCollectionItem; virtual; abstract;
    function RemoveFile(AFilename: string): boolean; virtual; abstract;
    function AddStream(AStream: TStream; AOwned: boolean): boolean; virtual; abstract;
    procedure EndUpdate; virtual; abstract;
    function FontFileEnumerator: IFreeTypeFontEnumerator; virtual; abstract;
    function FamilyEnumerator: IFreeTypeFamilyEnumerator; virtual; abstract;
    property FontFileCount: integer read GetFontCount;
    property FontFile[AFileName: string]: TCustomFontCollectionItem read GetFont;
    property FamilyCount: integer read GetFamilyCount;
    property Family[AName: string]: TCustomFamilyCollectionItem read GetFamily;
  end;

{***************************** Rendering classes *********************************}

  TOnRenderTextHandler = procedure(AText: string; x,y: single) of object;

  { TFreeTypeRenderableFont }

  TFreeTypeRenderableFont = class
  protected
	FDestination: TBitmap32;
	FColor: TColor32;
	FWordBreakHandler: TFreeTypeWordBreakHandler;
	FOnRenderText: TOnRenderTextHandler;
	function GetClearType: boolean; virtual; abstract;
	procedure SetClearType(const AValue: boolean); virtual; abstract;
	function GetLineFullHeight: single; virtual; abstract;
	function GetAscent: single; virtual; abstract;
	function GetDescent: single; virtual; abstract;
	function GetLineSpacing: single; virtual; abstract;
	procedure DefaultWordBreakHandler(var ABefore, AAfter: string);
	function GetHinted: boolean; virtual; abstract;
	procedure SetHinted(const AValue: boolean); virtual; abstract;
	procedure RenderDirectly(x, y, tx: integer; data: pointer);
  public
	UnderlineDecoration,StrikeOutDecoration: boolean;
	Orientation: integer;
	function TextWidth(AText: string): single; virtual; abstract;
	function TextHeight(AText: string): single; virtual; abstract;
	function CharWidthFromUnicode(AUnicode: integer): single; virtual; abstract;
	procedure SplitText(var AText: string; AMaxWidth: single; out ARemains: string);
	procedure GetTextSize(AText: string; out w,h: single); virtual;
	procedure RenderText(AText: string; x,y: single; ARect: TRect; OnRender : TDirectRenderingFunction); virtual; abstract;
	property ClearType: boolean read GetClearType write SetClearType;
	property Ascent: single read GetAscent;
	property Descent: single read GetDescent;
	property LineSpacing: single read GetLineSpacing;
	property LineFullHeight: single read GetLineFullHeight;
	property Hinted: boolean read GetHinted write SetHinted;
	property OnWordBreak: TFreeTypeWordBreakHandler read FWordBreakHandler write FWordBreakHandler;
	property OnRenderText: TOnRenderTextHandler read FOnRenderText write FOnRenderText;

	procedure ClippedDrawPixel(x,y: integer; const c: TColor32);
	procedure DrawPixel(x,y: integer; const c: TColor32); inline;
	procedure DrawText(Buffer: TBitmap32; const AText: String; X, Y: Single; AColor: TColor32; AOpacity: Byte); virtual; overload;
	procedure DrawText(Buffer: TBitmap32; const AText: String; X, Y: Single; AColor: TColor32; AOpacity: Byte; AAlign: TFreeTypeAlignments); virtual; overload;
	procedure DrawText(Buffer: TBitmap32; const AText: String; X, Y: Single; AColor: TColor32); virtual; overload;
	procedure DrawText(Buffer: TBitmap32; AText: String; X, Y: Single; AColor: TColor32; AAlign: TFreeTypeAlignments); virtual; overload;
	procedure DrawTextWordBreak(Buffer: TBitmap32; AText: String; X, Y, AMaxWidth: Single; AColor: TColor32; AAlign: TFreeTypeAlignments);
	procedure DrawTextRect(Buffer: TBitmap32; const AText: String; X1, Y1, X2, Y2: Single; AColor: TColor32; AAlign: TFreeTypeAlignments);
	procedure DrawGlyph(Buffer: TBitmap32; AGlyph: Integer; X, Y: Single; AColor: TColor32); virtual; overload;
	procedure DrawGlyph(Buffer: TBitmap32; AGlyph: Integer; X, Y: Single; AColor: TColor32; AAlign: TFreeTypeAlignments); virtual; overload;
  end;

{********************************* Font implementation **********************************}

  { TFreeTypeFont }

  TFreeTypeFont = class(TFreeTypeRenderableFont)
  private
    FName: String;
    FFaceChanged: boolean;
    FDPI: integer;
    FStream: TStream;
    FOwnedStream: boolean;
    FPointSize: single;
    FHinted: boolean;
    FKerningEnabled, FKerningFallbackEnabled: boolean;
    FStyleStr: string;
    FWidthFactor: single;
    FClearType: boolean;
    FNamesArray: array of string;
    FCollection: TCustomFreeTypeFontCollection;
    function FindGlyphNode(Index: Integer): TAvlTreeNode;
    function GetCharIndex(AUnicodeChar: integer): integer;
    function GetDPI: integer;
    function GetFamily: string;
    function GetFreeTypeStyles: TFreeTypeStyles;
    function GetGlyph(Index: integer): TFreeTypeGlyph;
    function GetGlyphCount: integer;
    function GetInformation(AIndex: TFreeTypeInformation): string;
    function GetGlyphKerning(AGlyphLeft, AGlyphRight: integer): TFreeTypeKerning;
    function GetCharKerning(AUnicodeCharLeft, AUnicodeCharRight: integer): TFreeTypeKerning;
    function GetPixelSize: single;
    function GetVersionNumber: string;
    procedure SetDPI(const AValue: integer);
    procedure SetFreeTypeStyles(AValue: TFreeTypeStyles);
    procedure SetLineFullHeight(AValue: single);
    procedure SetStyleAsString(AValue: string);
    procedure LoadFace;
    procedure SetName(const AValue: String);
    procedure DiscardFace;
    procedure DiscardInstance;
    procedure DiscardStream;
    procedure SetPixelSize(const AValue: single);
    procedure SetPointSize(AValue: single);
    function LoadGlyphInto(_glyph      : TT_Glyph;
                            glyph_index : Word): boolean;
    procedure SetWidthFactor(const AValue: single);
    procedure UpdateInstance;
    procedure UpdateSizeInPoints;
    procedure UpdateMetrics;
    procedure UpdateCharmap;
    procedure RenderTextDecoration(AText: string; x,y: single; ARect: TRect; OnRender : TDirectRenderingFunction);
    procedure FillRect(ARect: TRect; OnRender : TDirectRenderingFunction);
  protected
    FFace: TT_Face;
    FFaceItem: TCustomFontCollectionItem;
    FFaceLoaded: boolean;
    FInstance: TT_Instance;
    FInstanceCreated : boolean;
    FGlyphTable: TAvlTree;
    FCharMap: TT_CharMap;
    FCharmapOk, FCharmapSymbol: boolean;
    FAscentValue, FDescentValue, FLineGapValue, FLargeLineGapValue, FCapHeight: single;
    FUnitsPerEM: TT_UShort;
    procedure FaceChanged;
    function GetClearType: boolean; override;
    procedure SetClearType(const AValue: boolean); override;
    function GetLineFullHeight: single; override;
    function GetAscent: single; override;
    function GetDescent: single; override;
    function GetLineSpacing: single; override;
    function GetCapHeight: single;
    procedure SetHinted(const AValue: boolean); override;
    function GetHinted: boolean; override;
    procedure OnDestroyFontItem;
    procedure FetchNames;
    function GetCollection: TCustomFreeTypeFontCollection;
    function CheckFace: boolean;
  public
    Quality : TGlyphRenderQuality;
    SmallLinePadding: boolean;
    constructor Create;
    destructor Destroy; override;
    procedure AccessFromStream(AStream: TStream; AStreamOwner: boolean);
    procedure RenderText(AText: string; x,y: single; ARect: TRect; OnRender : TDirectRenderingFunction); override;
    procedure RenderGlyph(AGlyph: Integer; x,y: single; ARect: TRect; OnRender : TDirectRenderingFunction);
    procedure SetNameAndStyle(AName: string; AStyle: string); overload;
    procedure SetNameAndStyle(AName: string; AStyle: TFreeTypeStyles); overload;
    function TextWidth(AText: string): single; override;
    function TextHeight(AText: string): single; override;
    function CharWidthFromUnicode(AUnicodeChar: integer): single; override;
    function CharWidthFromGlyph(AGlyph: integer): single;
    function CharsWidth(AText: string): ArrayOfSingle;
    function CharsPosition(AText: string): ArrayOfCharPosition; overload;
    function CharsPosition(AText: string; AAlign: TFreeTypeAlignments): ArrayOfCharPosition; overload;
    function CheckInstance: boolean;
    property Name: String read FName write SetName;
    property DPI: integer read GetDPI write SetDPI;
    property SizeInPoints: single read FPointSize write SetPointSize;
    property SizeInPixels: single read GetPixelSize write SetPixelSize;
    property CapHeight: single read GetCapHeight;
    property Glyph[Index: integer]: TFreeTypeGlyph read GetGlyph;
    property GlyphCount: integer read GetGlyphCount;
    property CharKerning[AUnicodeCharLeft, AUnicodeCharRight: integer]: TFreeTypeKerning read GetCharKerning;
    property GlyphKerning[AGlyphLeft, AGlyphRight: integer]: TFreeTypeKerning read GetGlyphKerning;
    property CharIndex[AUnicodeChar: integer]: integer read GetCharIndex;
    property Hinted: boolean read FHinted write SetHinted;
    { Kerning brings closer certain letters that fit together }
    property KerningEnabled: boolean read FKerningEnabled write FKerningEnabled;
    { When enabled, if the kerning is not found between two letters, alternate codes are tried }
    property KerningFallbackEnabled: boolean read FKerningFallbackEnabled write FKerningFallbackEnabled;
    property WidthFactor: single read FWidthFactor write SetWidthFactor;
    property LineFullHeight: single read GetLineFullHeight write SetLineFullHeight;
    property Information[AIndex: TFreeTypeInformation]: string read GetInformation;
    property VersionNumber: string read GetVersionNumber;
    property Family: string read GetFamily;
    property Collection: TCustomFreeTypeFontCollection read GetCollection write FCollection;
    property StyleAsString: string read FStyleStr write SetStyleAsString;
    property Style: TFreeTypeStyles read GetFreeTypeStyles write SetFreeTypeStyles;
  end;

  { TFreeTypeGlyph }

  TFreeTypeGlyph = class
  private
    FLoaded: boolean;
    FGlyphData: TT_Glyph;
    FIndex: integer;
    FOrientation: Integer;
    function GetAdvance: single;
    function GetBounds: TRect;
    function GetBoundsWithOffset(x, y: single): TRect;
    {%H-}constructor create;
  public
    constructor Create(AFont: TFreeTypeFont; AIndex: integer);
    function RenderDirectly(x,y: single; Rect: TRect; OnRender : TDirectRenderingFunction; quality : TGlyphRenderQuality; ClearType: boolean = false): boolean;
    function RenderDirectly(ARasterizer: TFreeTypeRasterizer; x,y: single; Rect: TRect; OnRender : TDirectRenderingFunction; quality : TGlyphRenderQuality; ClearType: boolean = false): boolean;
    function Clone(AOrientation:Integer): TFreeTypeGlyph;
    destructor Destroy; override;
    property Loaded: boolean read FLoaded;
    property Data: TT_Glyph read FGlyphData;
    property Index: integer read FIndex;
    property Bounds: TRect read GetBounds;
    property BoundsWithOffset[x,y: single]: TRect read GetBoundsWithOffset;
    property Advance: single read GetAdvance;
  end;

  { TFreeTypeRasterMap }

  TFreeTypeRasterMap = class
  protected
    map: TT_Raster_Map;
    FRasterizer: TFreeTypeRasterizer;
    function GetHeight: integer; virtual;
    function GetWidth: integer; virtual;
    function GetScanLine(y: integer): pointer;
    procedure Init(AWidth,AHeight: integer); virtual; abstract;
  public
    constructor Create(AWidth,AHeight: integer); virtual;
    constructor Create(ARasterizer: TFreeTypeRasterizer; AWidth,AHeight: integer); virtual;
    procedure Clear;
    procedure Fill;
    function RenderGlyph(glyph : TFreeTypeGlyph; x,y: single) : boolean; virtual; abstract;
    procedure ScanMoveTo(x,y: integer); virtual; abstract;
    destructor Destroy; override;

    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
    property ScanLine[y: integer]: pointer read GetScanLine;
  end;

  { TFreeTypeMonochromeMap }

  TFreeTypeMonochromeMap = class(TFreeTypeRasterMap)
  private
    ScanPtrStart,ScanPtrCur: pbyte;
    ScanBit: byte;
    ScanX: integer;
    function GetPixelsInHorizlineNoBoundsChecking(x,y,x2: integer) : integer; inline;
  protected
    procedure Init(AWidth,AHeight: integer); override;
  public
    function RenderGlyph(glyph : TFreeTypeGlyph; x,y: single) : boolean; override;
    procedure ScanMoveTo(x,y: integer); override;
    function ScanNextPixel: boolean;
    function GetPixel(x,y: integer): boolean;
    procedure SetPixel(x,y: integer; value: boolean);
    function GetPixelsInRect(x,y,x2,y2: integer): integer;
    function GetPixelsInHorizline(x,y,x2: integer): integer;
    procedure TogglePixel(x,y: integer);
  end;

  { TFreeTypeGrayscaleMap }

  TFreeTypeGrayscaleMap = class(TFreeTypeRasterMap)
  private
    ScanPtrStart: pbyte;
    ScanX: integer;
  protected
    procedure Init(AWidth, AHeight: integer); override;
  public
    RenderQuality: TGlyphRenderQuality;
    function RenderGlyph(glyph : TFreeTypeGlyph; x,y: single) : boolean; override;
    procedure ScanMoveTo(x,y: integer); override;
    function ScanNextPixel: byte;
    function GetPixel(x,y: integer): byte;
    procedure SetPixel(x,y: integer; value: byte);
    procedure XorPixel(x,y: integer; value: byte);
  end;

	ArrayOfString = array of string;

const
	FreeTypeMinPointSize = 1;

var
	FontCollection: TCustomFreeTypeFontCollection;


function StylesToArray(AStyles: string): ArrayOfString;


implementation

uses
	Math,
	Graphics32.Freetype.FontCollection;

const
	TT_PLATFORM_APPLE_UNICODE = 0;
	//TT_PLATFORM_MACINTOSH     = 1;
	TT_PLATFORM_ISO           = 2; // deprecated
	TT_PLATFORM_MICROSOFT     = 3;
	//TT_PLATFORM_CUSTOM        = 4;
	//TT_PLATFORM_ADOBE         = 7; // artificial

function FontCollectionItemDestroyListener(ATargetObject: TObject;
	ANotifyProc: TFontCollectionItemDestroyProc): TFontCollectionItemDestroyListener;
begin
	Result.TargetObject := ATargetObject;
	Result.NotifyProc := ANotifyProc;
end;

function StylesToArray(AStyles: string): ArrayOfString;
var
  StartIndex, EndIndex: integer;
  Count: integer;

  procedure AddStyle(AName: string);
  begin
    if (AName = 'Normal') or (AName = 'Regular') or (AName = 'Roman') or (AName = 'Plain') or (AName = 'Book') then exit;
    if Count = length(result) then
      setlength(result, length(result)+4);
    result[Count] := AName;
    inc(Count);
  end;

begin
  Count := 0;
  result := nil;
  StartIndex := 1;
  while StartIndex <= length(AStyles) do
  begin
    while (StartIndex < length(AStyles)) and (AStyles[StartIndex] = ' ') do inc(StartIndex);
    if AStyles[StartIndex] <> ' ' then
    begin
      EndIndex := StartIndex;
      while (EndIndex < length(AStyles)) and (AStyles[EndIndex+1] <> ' ') do inc(EndIndex);
      AddStyle(copy(AStyles, StartIndex, EndIndex-StartIndex+1));
      StartIndex := EndIndex+1;
    end;
  end;
  setlength(result,Count);
end;

var
  BitCountTable: packed array[0..255] of byte;
  RegularGray5: TT_Gray_Palette;
  FreeTypeInitialized,FreeTypeCannotInitialize : boolean;

procedure EnsureFreeTypeInitialized;
begin
  if not FreeTypeInitialized and not FreeTypeCannotInitialize then
  begin
    FreeTypeInitialized := (TT_Init_FreeType = TT_Err_Ok);
    FreeTypeCannotInitialize := not FreeTypeInitialized;
  end;
  if FreeTypeCannotInitialize then
    raise EFreeType.Create('FreeType cannot be initialized');
end;

function GlyphTableOnCompare(Item1, Item2: Pointer): Integer;
var
   G1: TFreeTypeGlyph absolute Item1;
   G2: TFreeTypeGlyph absolute Item2;
begin
  if G1.Index > G2.Index then
    Result := 1
  else if G1.Index < G2.Index then
    Result := -1
  else
    Result := 0;
end;

{ TFreeTypeRenderableFont }

procedure TFreeTypeRenderableFont.DefaultWordBreakHandler(var ABefore,
  AAfter: string);
var p: integer;
begin
  if (AAfter <> '') and (ABefore <> '') and (AAfter[1]<> ' ') and (ABefore[length(ABefore)] <> ' ') then
  begin
    p := length(ABefore);
    while (p > 1) and (ABefore[p-1] <> ' ') do dec(p);
    if p > 1 then //can put the word after
    begin
      AAfter := copy(ABefore,p,length(ABefore)-p+1)+AAfter;
      ABefore := copy(ABefore,1,p-1);
    end else
    begin //cannot put the word after, so before

    end;
  end;
  while (ABefore <> '') and (ABefore[length(ABefore)] =' ') do delete(ABefore,length(ABefore),1);
  while (AAfter <> '') and (AAfter[1] =' ') do delete(AAfter,1,1);
end;

procedure TFreeTypeRenderableFont.SplitText(var AText: string;
  AMaxWidth: single; out ARemains: string);
var
  pstr: pchar;
  left,charlen: integer;
  totalWidth: single;
  firstChar: boolean;
  glyphWidth: single;
  glyphCode: cardinal;

  procedure WordBreak(ADropCount: Integer = 0);
  begin
    ARemains:= copy(AText, length(AText) - left + 1 + ADropCount, left);
    SetLength(AText, length(AText) - left);
  end;

begin
  ARemains := '';
  if AText = '' then
    exit;
  totalWidth := 0;
  pstr := @AText[1];
  left := length(AText);
  firstChar := true;
  while left > 0 do
  begin
    if pstr[0] in [#13, #10] then
    begin
      if (left > 1)  and ([pstr[0], pstr[1]] = [#13, #10]) then
        WordBreak(2)
      else
        WordBreak(1);
      exit;
    end;

    charlen := UTF8CodepointSize(pstr);
    glyphCode := UTF8CodepointToUnicode(pstr, charlen);
    inc(pstr,charlen);

    glyphWidth := CharWidthFromUnicode(glyphCode);
    if glyphWidth <> 0 then
    begin
      totalWidth += glyphWidth;
      if (totalWidth > AMaxWidth) and not firstChar then
      begin
        WordBreak;
        if Assigned(FWordBreakHandler) then
          FWordBreakHandler(AText,ARemains)
        else
          DefaultWordBreakHandler(AText,ARemains);
        exit;
      end;
    end;

    dec(left,charlen);
    firstChar := false;
  end;
  ARemains := ''; //no split
end;

procedure TFreeTypeRenderableFont.GetTextSize(AText: string; out w, h: single);
begin
  w := TextWidth(AText);
  h := TextHeight(AText);
end;

{ TFreeTypeGlyph }

function TFreeTypeGlyph.GetBounds: TRect;
begin
  result := GetBoundsWithOffset(0, 0);
end;

function TFreeTypeGlyph.GetAdvance: single;
var
  metrics: TT_Glyph_Metrics;
begin
  if TT_Get_Glyph_Metrics(FGlyphData, metrics) = TT_Err_Ok then
    result := metrics.advance/64
  else
    result := 0;
end;

function TFreeTypeGlyph.GetBoundsWithOffset(x, y: single): TRect;
var
  metrics: TT_Glyph_Metrics;
  outline: TT_Outline;
  bbox: TT_BBox;
  error: TT_Error;
begin

  if FOrientation<>0 then
  begin
    error := TT_Get_Glyph_Outline(FGlyphData, outline{%H-});
    if error=TT_Err_Ok then
      error := TT_Get_Outline_BBox(outline, bbox{%H-});
  end else
  begin
    error := TT_Get_Glyph_Metrics(FGlyphData, metrics);
    if error=TT_Err_Ok then
      bbox := metrics.bbox;
  end;

  if error=TT_Err_Ok then
    with bbox do
      result := rect(IncludeFullGrainMin(xMin+round(x*64),64) div 64,
                     IncludeFullGrainMin(-yMax+round(y*64),64) div 64,
                    (IncludeFullGrainMax(xMax+round(x*64),64)+1) div 64,
                    (IncludeFullGrainMax(-yMin+round(y*64),64)+1) div 64)
  else
    result := TRect.Empty;
end;

constructor TFreeTypeGlyph.Create(AFont: TFreeTypeFont; AIndex: integer);
begin
  if not AFont.CheckFace or (TT_New_Glyph(AFont.FFace, FGlyphData) <> TT_Err_Ok) then
    raise EFreeType.Create('Cannot create empty glyph');
  FLoaded := AFont.LoadGlyphInto(FGlyphData, AIndex);
  FIndex := AIndex;
end;

constructor TFreeTypeGlyph.create;
begin
end;

function TFreeTypeGlyph.Clone(AOrientation: Integer): TFreeTypeGlyph;
begin
  if not FLoaded then
    raise EFreeType.Create('Cannot create a clone of an empty glyph');

  result := TFreeTypeGlyph.create;
  result.FLoaded := FLoaded;
  result.FIndex := FIndex;
  result.FOrientation := AOrientation;

  TT_Copy_Glyph(FGlyphData, result.FGlyphData);
end;

function TFreeTypeGlyph.RenderDirectly(x, y: single; Rect: TRect;
  OnRender: TDirectRenderingFunction; quality : TGlyphRenderQuality; ClearType: boolean): boolean;
begin
  result := RenderDirectly(TTGetDefaultRasterizer, x,y, Rect, OnRender, quality, ClearType);
end;

function TFreeTypeGlyph.RenderDirectly(ARasterizer: TFreeTypeRasterizer; x,
  y: single; Rect: TRect; OnRender: TDirectRenderingFunction;
  quality: TGlyphRenderQuality; ClearType: boolean): boolean;
var mono: TFreeTypeMonochromeMap;
    tx,xb,yb: integer;
    pdest: pbyte;
    buf: pointer;
    glyphBounds: TRect;
begin
  if ClearType then
  begin
    Rect.Left *= 3;
    Rect.Right *= 3;
    x *= 3;
  end;

  glyphBounds := BoundsWithOffset[x,y];

  if ClearType then
  begin
    InflateRect(glyphBounds,1,0);
    glyphBounds.Left := IncludeFullGrainMin( glyphBounds.Left, 3);
    glyphBounds.Right := IncludeFullGrainMax( glyphBounds.Right-1, 3) + 1;
  end;
  if not IntersectRect(Rect,Rect,glyphBounds) then exit(False);

  case quality of
    grqMonochrome:
      begin
        tx := rect.right-rect.left;
        mono := TFreeTypeMonochromeMap.Create(ARasterizer,tx,rect.bottom-rect.top);
        result := mono.RenderGlyph(self,x-rect.left,y-rect.top);
        if result then
        begin
          getmem(buf, tx);
          for yb := mono.Height-1 downto 0 do
          begin
            mono.ScanMoveTo(0,yb);
            pdest := pbyte(buf);
            for xb := tx-1 downto 0 do
            begin
              if mono.ScanNextPixel then
                pdest^ := $ff
              else
                pdest^ := 0;
              inc(pdest);
            end;
            OnRender(rect.Left,rect.top+yb,tx,buf);
          end;
          freemem(buf);
        end;
        mono.Free;
      end;
    grqLowQuality:
      begin
        ARasterizer.Set_Raster_Palette(RegularGray5);
        result := TT_Render_Directly_Glyph_Gray(FGlyphData,
                    round((x-rect.left)*64), round((rect.bottom-y)*64),
                    rect.left, rect.top, rect.right-rect.left, rect.bottom-rect.top,
                    OnRender, ARasterizer) = TT_Err_Ok;
      end;
    grqHighQuality:
      result := TT_Render_Directly_Glyph_HQ(FGlyphData,
                  round((x-rect.left)*64), round((rect.bottom-y)*64),
                  rect.left, rect.top, rect.right-rect.left, rect.bottom-rect.top,
                  OnRender, ARasterizer) = TT_Err_Ok;
  end;
end;

destructor TFreeTypeGlyph.Destroy;
begin
  if FreeTypeInitialized then
    TT_Done_Glyph(FGlyphData);
  inherited Destroy;
end;

{ TFreeTypeFont }

procedure TFreeTypeFont.LoadFace;
var errorNum: TT_Error;
    familyItem: TCustomFamilyCollectionItem;
    fontItem: TCustomFontCollectionItem;
begin
  DiscardFace;
  if FStream <> nil then
  begin
    errorNum := TT_Open_Face(FStream,False,FFace);
    if errorNum <> TT_Err_Ok then
      raise EFreeType.Create('Cannot open font (TT_Error ' + intToStr(errorNum)+') <Stream>');
  end else
  begin
    if Pos(PathDelim, FName) <> 0 then
    begin
      errorNum := TT_Open_Face(FName,FFace);
      if errorNum <> TT_Err_Ok then
        raise EFreeType.Create('Cannot open font (TT_Error ' + intToStr(errorNum)+') "'+FName+'"');
    end else
    begin
      familyItem := Collection.Family[FName];
      if familyItem = nil then
        raise EFreeType.Create('Font family not found ("'+FName+'")');
      fontItem := familyItem.GetFont(FStyleStr);
      if fontItem = nil then
        raise EFreeType.Create('Font style not found ("'+FStyleStr+'")');
      FFace := fontItem.QueryFace(FontCollectionItemDestroyListener(self,@OnDestroyFontItem));
      FFaceItem := fontItem;
    end;
  end;

  FFaceLoaded:= true;
  UpdateInstance;
end;

procedure TFreeTypeFont.SetName(const AValue: String);
begin
  DiscardStream;
  if FName=AValue then exit;
  FName := AValue;
  FaceChanged;
end;

{$push}
{$hints off}
function TFreeTypeFont.GetDPI: integer;
var metrics: TT_Instance_Metrics;
begin
  if not CheckInstance then
  begin
    result := FDPI;
  end
  else
  begin
    if TT_Get_Instance_Metrics(FInstance,metrics) = TT_Err_Ok then
      result := metrics.y_resolution
    else
      result := FDPI;
  end;
end;
{$pop}

function TFreeTypeFont.GetFamily: string;
begin
  result := Information[ftiFamily];
end;

function TFreeTypeFont.GetFreeTypeStyles: TFreeTypeStyles;
var a: array of string;
    i: integer;
begin
  result := [];
  a := StylesToArray(StyleAsString);
  for i := 0 to high(a) do
    if a[i] = 'Bold' then result += [ftsBold] else
    if (a[i] = 'Italic') or (a[i] = 'Oblique') then result += [ftsItalic];
end;

function TFreeTypeFont.FindGlyphNode(Index: Integer): TAvlTreeNode;
var DataValue: integer;
begin
  Result:=FGlyphTable.Root;
  while (Result<>nil) do begin
    DataValue := TFreeTypeGlyph(Result.Data).Index;
    if Index=DataValue then exit;
    if Index<DataValue then begin
      Result:=Result.Left
    end else begin
      Result:=Result.Right
    end;
  end;
end;

function TFreeTypeFont.GetAscent: single;
begin
  CheckInstance;
  result := FAscentValue*SizeInPixels;
end;

function TFreeTypeFont.GetCapHeight: single;
begin
  CheckInstance;
  result := FCapHeight*SizeInPixels;
end;

function TFreeTypeFont.GetClearType: boolean;
begin
  Result:= FClearType;
end;

function TFreeTypeFont.GetCharIndex(AUnicodeChar: integer): integer;
begin
  if not CheckInstance then
  begin
    result := 0;
    exit;
  end;
  if FCharmapOk then
  begin
    if FCharmapSymbol then
      result := TT_Char_Index(FCharMap, AUnicodeChar or $F000)
    else
      result := TT_Char_Index(FCharMap, AUnicodeChar);
  end
  else
    result := 0;
end;

function TFreeTypeFont.GetDescent: single;
begin
  CheckInstance;
  result := FDescentValue*SizeInPixels;
end;

function TFreeTypeFont.GetGlyph(Index: integer): TFreeTypeGlyph;
var
  node: TAvlTreeNode;
  lGlyph: TFreeTypeGlyph;
begin
  if not CheckInstance then
  begin
    result := nil;
    exit;
  end;
  node := FindGlyphNode(Index);
  if node = nil then
  begin
    lGlyph := TFreeTypeGlyph.Create(self, Index);;
    FGlyphTable.Add(lGlyph);
  end else
    lGlyph := TFreeTypeGlyph(node.Data);
  result := lGlyph;
end;

{$push}
{$hints off}
function TFreeTypeFont.GetGlyphCount: integer;
var prop : TT_Face_Properties;
begin
  if not CheckFace then
    result := 0
  else
  begin
    if TT_Get_Face_Properties(FFace, prop) <> TT_Err_Ok then
      result := 0
    else
      result := prop.num_glyphs;
  end;
end;

function TFreeTypeFont.GetInformation(AIndex: TFreeTypeInformation): string;
begin
  if FNamesArray = nil then FetchNames;
  if (ord(AIndex) < 0) or (ord(AIndex) > high(FNamesArray)) then
    result := ''
  else
    result := FNamesArray[ord(AIndex)];
end;
{$pop}

function TFreeTypeFont.GetGlyphKerning(AGlyphLeft, AGlyphRight: integer): TFreeTypeKerning;
var
  kerningInfo: TT_KerningInfo;
  factor: single;
begin
  kerningInfo := TT_Get_KerningInfo(FFace, AGlyphLeft, AGlyphRight);
  factor := SizeInPixels/FUnitsPerEM;
  result.Kerning.x := kerningInfo.kerning_x*factor;
  result.Kerning.y := kerningInfo.kerning_y*factor;
  result.Minimum.x := kerningInfo.minimum_x*factor;
  result.Minimum.y := kerningInfo.minimum_y*factor;
  result.Found := kerningInfo.found;
end;

function TFreeTypeFont.GetLineFullHeight: single;
begin
  CheckInstance;
  result := (FAscentValue + FDescentValue)*SizeInPixels + GetLineSpacing;
end;

function TFreeTypeFont.GetLineSpacing: single;
begin
  CheckInstance;
  if not SmallLinePadding then
    result := FLargeLineGapValue*SizeInPixels
  else
    result := FLineGapValue*SizeInPixels;
end;

procedure TFreeTypeFont.OnDestroyFontItem;
begin
  DiscardFace;
  FaceChanged;
end;

function TFreeTypeFont.GetPixelSize: single;
begin
  result := SizeInPoints * DPI / 72;
end;

function TFreeTypeFont.GetVersionNumber: string;
var VersionStr: string;
    idxStart,idxEnd: integer;
begin
  VersionStr := Information[ftiVersionString];
  idxStart := 1;
  while (idxStart < length(VersionStr)) and not (VersionStr[idxStart] in['0'..'9']) do
    inc(idxStart);
  idxEnd := idxStart;
  while (idxEnd+1 <= length(VersionStr)) and (VersionStr[idxEnd+1] in['0'..'9']) do inc(idxEnd);
  if (idxEnd+1 <= length(VersionStr)) and (VersionStr[idxEnd+1] = '.') then inc(idxEnd);
  while (idxEnd+1 <= length(VersionStr)) and (VersionStr[idxEnd+1] in['0'..'9']) do inc(idxEnd);
  result := copy(VersionStr,idxStart,idxEnd-idxStart+1);
end;

procedure TFreeTypeFont.SetClearType(const AValue: boolean);
begin
  if FClearType=AValue then exit;
  FClearType:=AValue;
  UpdateSizeInPoints;
end;

procedure TFreeTypeFont.SetDPI(const AValue: integer);
begin
  if FDPI = AValue then exit;
  FDPI := AValue;
  if FInstanceCreated then
  begin
    TT_Set_Instance_Resolutions(FInstance, AValue,AValue);
    UpdateSizeInPoints;
  end;
end;

procedure TFreeTypeFont.SetFreeTypeStyles(AValue: TFreeTypeStyles);
var str: string;
begin
  str := '';
  if ftsBold in AValue then str += 'Bold ';
  if ftsItalic in AValue then str += 'Italic ';
  StyleAsString := trim(str);
end;

procedure TFreeTypeFont.SetHinted(const AValue: boolean);
begin
  if FHinted=AValue then exit;
  FHinted:=AValue;
  FGlyphTable.FreeAndClear;
end;

function TFreeTypeFont.GetHinted: boolean;
begin
  result := FHinted;
end;

procedure TFreeTypeFont.SetLineFullHeight(AValue: single);
var Ratio: single;
begin
  CheckInstance;
  Ratio := FAscentValue + FDescentValue;
  if not SmallLinePadding then
    Ratio += FLargeLineGapValue
  else
    Ratio += FLineGapValue;
  if Ratio <> 0 then
    SizeInPixels := AValue / Ratio
  else
    SizeInPixels := AValue;
end;

procedure TFreeTypeFont.SetStyleAsString(AValue: string);
begin
  AValue := Trim(AValue);
  if FStyleStr=AValue then Exit;
  FStyleStr:=AValue;
  FaceChanged;
end;

procedure TFreeTypeFont.DiscardFace;
begin
  if FFaceLoaded then
  begin
    DiscardInstance;
    if FFaceItem <> nil then
    begin
      FFaceItem.ReleaseFace(FontCollectionItemDestroyListener(self,@OnDestroyFontItem));
      FFaceItem := nil;
    end
    else
      TT_Close_Face(FFace);
    FFaceLoaded := false;
    FNamesArray := nil;
  end;
  FCharmapOk := false;
end;

procedure TFreeTypeFont.DiscardInstance;
begin
  if FInstanceCreated then
  begin
    if FreeTypeInitialized then
      TT_Done_Instance(FInstance);
    FInstanceCreated := false;
    FGlyphTable.FreeAndClear;
  end;
end;

procedure TFreeTypeFont.DiscardStream;
begin
  if FStream <> nil then
  begin
    DiscardFace;
    if FOwnedStream then FStream.Free;
    FStream := nil;
    FOwnedStream:= false;
  end;
end;

procedure TFreeTypeFont.SetPixelSize(const AValue: single);
begin
  SizeInPoints := AValue*72/DPI;
end;

procedure TFreeTypeFont.SetPointSize(AValue: single);
begin
  if AValue < FreeTypeMinPointSize then AValue := FreeTypeMinPointSize;
  if FPointSize=AValue then exit;
  FPointSize:=AValue;
  UpdateSizeInPoints;
end;

function TFreeTypeFont.LoadGlyphInto(_glyph: TT_Glyph; glyph_index: Word): boolean;
var flags: integer;
begin
  if not CheckInstance then
    raise EFreeType.Create('No font instance');
  flags := TT_Load_Scale_Glyph;
  if FHinted then flags := flags or TT_Load_Hint_Glyph;
  result := (TT_Load_Glyph(FInstance, _glyph, glyph_index, flags) = TT_Err_Ok);
end;

procedure TFreeTypeFont.SetWidthFactor(const AValue: single);
begin
  if FWidthFactor=AValue then exit;
  FWidthFactor:=AValue;
  FGlyphTable.FreeAndClear;
  UpdateSizeInPoints;
end;

procedure TFreeTypeFont.UpdateInstance;
var
  errorNum: TT_Error;
begin
  DiscardInstance;

  errorNum := TT_New_Instance(FFace, FInstance);
  if errorNum = TT_Err_Ok then
  begin
    FInstanceCreated := true;
    TT_Set_Instance_Resolutions(FInstance, FDPI,FDPI);
    UpdateSizeInPoints;
    UpdateMetrics;
    UpdateCharmap;
  end else
    raise EFreeType.Create('Cannot create font instance (TT_Error ' + intToStr(errorNum)+')');
end;

procedure TFreeTypeFont.UpdateSizeInPoints;
var charsizex: integer;
begin
  if FInstanceCreated then
  begin
    if not FClearType then
      charsizex := round(FPointSize*64*FWidthFactor)
    else
      charsizex := round(FPointSize*64*FWidthFactor*3);

    if TT_Set_Instance_CharSizes(FInstance,charsizex,round(FPointSize*64)) <> TT_Err_Ok then
      raise EFreeType.Create('Unable to set point size');
    FGlyphTable.FreeAndClear;
  end;
end;

procedure TFreeTypeFont.UpdateMetrics;
var prop: TT_Face_Properties;
begin
  if CheckFace then
  begin
    TT_Get_Face_Properties(FFace,prop);
    FAscentValue := prop.horizontal^.ascender;
    FDescentValue := prop.horizontal^.descender;
    FLineGapValue:= prop.horizontal^.line_gap;
    FLargeLineGapValue:= FLineGapValue;
    FUnitsPerEM := prop.header^.units_per_EM;

    if (FAscentValue = 0) and (FDescentValue = 0) then
    begin
      if prop.os2^.version <> $ffff then
      begin
        if (prop.os2^.usWinAscent <> 0) or (prop.os2^.usWinDescent <> 0) then
        begin
          FAscentValue := prop.os2^.usWinAscent;
          FDescentValue := -prop.os2^.usWinDescent;
        end else
        begin
          FAscentValue := prop.os2^.sTypoAscender;
          FDescentValue := prop.os2^.sTypoDescender;
        end;
      end;
    end;

    if prop.os2^.version <> $ffff then
      if prop.os2^.sTypoLineGap > FLargeLineGapValue then
        FLargeLineGapValue := prop.os2^.sTypoLineGap;

    if prop.os2^.version >= 2 then
      FCapHeight:=prop.os2^.sCapHeight
    else
      FCapHeight:=FAscentValue;

    FAscentValue /= FUnitsPerEM;
    FDescentValue /= -FUnitsPerEM;
    FLineGapValue /= FUnitsPerEM;
    FLargeLineGapValue /= FUnitsPerEM;
    FCapHeight /= FUnitsPerEM;

    if FLargeLineGapValue = 0 then
      FLargeLineGapValue := (FAscentValue+FDescentValue)*0.1;

  end else
  begin
    FAscentValue := -0.5;
    FDescentValue := 0.5;
    FLineGapValue := 0;
    FLargeLineGapValue:= 0;
    FUnitsPerEM   := 1;
  end;
end;

procedure TFreeTypeFont.UpdateCharmap;
var i,n: integer;
    lPlatform,encoding: integer;
begin
  if FCharmapOk then exit;
  if not FFaceLoaded then
  begin
    FCharmapOk := false;
    exit;
  end;

  n := TT_Get_CharMap_Count(FFace);
  lPlatform := -1;
  encoding := 0;
  FCharmapSymbol := false;

  //MS Unicode
  for i := 0 to n-1 do
  begin
    if TT_Get_CharMap_ID(FFace, i, lPlatform, encoding) = TT_Err_Ok then
    begin
      if (lPlatform = TT_PLATFORM_MICROSOFT) and (encoding = 1) then
      begin
        if TT_Get_CharMap(FFace, i, FCharMap) = TT_Err_Ok then
        begin
          FCharmapOk := true;
          exit;
        end
      end else
      if (lPlatform = TT_PLATFORM_MICROSOFT) and (encoding = 0) then
      begin
        if TT_Get_CharMap(FFace, i, FCharMap) = TT_Err_Ok then
        begin
          FCharmapOk := true;
          FCharmapSymbol:= true;
          exit;
        end;
      end;
    end;
  end;

  //Apple Unicode
  for i := 0 to n-1 do
  begin
    if TT_Get_CharMap_ID(FFace, i, lPlatform, encoding) = TT_Err_Ok then
    begin
      if (lPlatform = TT_PLATFORM_APPLE_UNICODE) then
        if TT_Get_CharMap(FFace, i, FCharMap) = TT_Err_Ok then
        begin
          FCharmapOk := true;
          exit;
        end;
    end;
  end;

  //ISO Unicode
  for i := 0 to n-1 do
  begin
    if TT_Get_CharMap_ID(FFace, i, lPlatform, encoding) = TT_Err_Ok then
    begin
      if (lPlatform = TT_PLATFORM_ISO) and (encoding = 1) then
        if TT_Get_CharMap(FFace, i, FCharMap) = TT_Err_Ok then
        begin
          FCharmapOk := true;
          exit;
        end;
    end;
  end;

  FCharmapOk := false;
end;

procedure TFreeTypeFont.RenderTextDecoration(AText: string; x, y: single;
  ARect: TRect; OnRender: TDirectRenderingFunction);
  procedure HorizLine(AYCoeff, AHeightCoeff: single);
  var
    ly, height: single;
    clippedRect,unclippedRect: TRect;
  begin
    ly := y + self.Ascent * AYCoeff;
    height := Max(self.Ascent * AHeightCoeff, 1);
    unclippedRect := Types.Rect(round(x),round(ly),
      round(x+self.TextWidth(AText)),round(ly+height));
    clippedRect := rect(0,0,0,0);
    if IntersectRect(clippedRect,unclippedRect,ARect) then
      FillRect(clippedRect,OnRender);
  end;
begin
  if UnderlineDecoration then
    HorizLine(+1.5*0.08, 0.08);
  if StrikeoutDecoration then
    HorizLine(-0.3, 0.06);
end;

procedure TFreeTypeFont.FillRect(ARect: TRect; OnRender: TDirectRenderingFunction);
var
  yb,temp,tx: integer;
  data: pbyte;
begin
  if ARect.Top > ARect.Bottom then
  begin
    temp := ARect.Top;
    ARect.Top := ARect.Bottom;
    ARect.Bottom := temp;
  end;
  if ARect.Left > ARect.Right then
  begin
    temp := ARect.Left;
    ARect.Left := ARect.Right;
    ARect.Right:= temp;
  end;
  if ClearType then
  begin
    ARect.Left *= 3;
    ARect.Right *= 3;
  end;
  tx := ARect.Right-ARect.Left;
  if tx > 0 then
  begin
    getmem(data,tx);
    try
      fillchar(data^, tx, 255);
      for yb := ARect.Top to ARect.Bottom-1 do
        OnRender(ARect.Left,yb,tx,data);
    finally
      freemem(data);
    end;
  end;
end;

procedure TFreeTypeFont.FaceChanged;
begin
  if not FFaceChanged then
  begin
    FFaceChanged := true;
    DiscardFace;
  end;
end;

constructor TFreeTypeFont.Create;
begin
  EnsureFreeTypeInitialized;
  FFaceLoaded := false;
  FFaceItem := nil;
  FInstanceCreated := false;
  FCharmapOk := false;
  FPointSize := 10;
  FDPI := 96;
  FGlyphTable := TAvlTree.Create;
  FGlyphTable.OnCompare := @GlyphTableOnCompare;
  FHinted := true;
  FKerningEnabled:= true;
  FKerningFallbackEnabled:= true;
  FWidthFactor := 1;
  FClearType := false;
  FStyleStr:= 'Regular';
  SmallLinePadding:= true;
  Quality := grqHighQuality;
  FFaceChanged := true;
end;

destructor TFreeTypeFont.Destroy;
begin
  DiscardInstance;
  DiscardFace;
  DiscardStream;
  FGlyphTable.Free;
  inherited Destroy;
end;

procedure TFreeTypeFont.AccessFromStream(AStream: TStream; AStreamOwner: boolean);
begin
  DiscardStream;
  FStream := AStream;
  FOwnedStream:= AStreamOwner;
  FaceChanged;
end;

procedure TFreeTypeFont.RenderText(AText: string; x, y: single; ARect: TRect;
  OnRender: TDirectRenderingFunction);
var
  pstr: pchar;
  left,charcode,charlen: integer;
  idx: integer;
  g: TFreeTypeGlyph;
  prevCharcode, glyphIndex: integer;
  txmatrix: TT_Matrix;
  angle: single;
  outline: ^TT_Outline;
  vector: TT_Vector;
  corrX, corrY: single;
begin
  if not CheckInstance then exit;
  if AText = '' then exit;
  idx := pos(LineEnding,AText);

  if Orientation<>0 then
  begin
    angle := Orientation * PI / 1800;
    txmatrix.xx :=  Round( cos( angle ) * $10000 );
    txmatrix.xy := -Round( sin( angle ) * $10000 );
    txmatrix.yx :=  Round( sin( angle ) * $10000 );
    txmatrix.yy :=  Round( cos( angle ) * $10000 );
  end;

  while idx <> 0 do
  begin
    RenderText(copy(AText,1,idx-1),x,y,ARect,OnRender);
    delete(AText,1,idx+length(LineEnding)-1);
    if Orientation<>0 then
    begin
      vector.x := 0;
      vector.y := -round(LineFullHeight * 64);
      TT_Transform_Vector(vector.x, vector.y, txmatrix);
      x += vector.x / 64;
      y -= vector.y / 64;
    end else
      y += LineFullHeight;
    idx := pos(LineEnding,AText);
  end;
  If Assigned(FOnRenderText) then
    FOnRenderText(AText,x,y);

  // TODO: Rotation at arbitraty angles requires antialiased drawing
  RenderTextDecoration(AText,x,y,ARect,OnRender);
  pstr := @AText[1];
  left := length(AText);
  prevCharcode := -1;
  while left > 0 do
  begin
    charcode := UTF8CodepointToUnicode(pstr, charlen);
    dec(left,charlen);
    glyphIndex := CharIndex[charcode];
    g := Glyph[glyphIndex];
    if Orientation<>0 then
      g := g.Clone(Orientation);

    if g <> nil then
    with g do
    begin
      corrX := Advance;

      if KerningEnabled and (prevCharcode <> -1) then
        corrX += round(GetCharKerning(prevCharcode, charcode).Kerning.x);

      vector.x := round(corrX * 64);
      vector.y := 0;

      if Orientation<>0 then begin
        outLine := @PGlyph(Data.z)^.outline;
        TT_Transform_Outline(outline^, txmatrix);
        TT_Transform_Vector(vector.x, vector.y, txmatrix);
      end;

      corrX := vector.x / 64;
      corry := vector.y / 64;

      if Hinted then
        RenderDirectly(x,round(y),ARect,OnRender,quality,FClearType)
      else
        RenderDirectly(x,y,ARect,OnRender,quality,FClearType);

      if FClearType then
        x += corrX/3
      else
        x += corrX;

      y -= corrY;

      prevCharcode := charcode;
      if Orientation<>0 then
        g.Free;
    end;
    inc(pstr,charlen);
  end;
end;

procedure TFreeTypeFont.RenderGlyph(AGlyph: Integer; x, y: single;
  ARect: TRect; OnRender: TDirectRenderingFunction);
var
  g: TFreeTypeGlyph;
begin
  if not CheckInstance then exit;
  g := Glyph[AGlyph];
  if g <> nil then
  with g do
  begin
    if Hinted then
     RenderDirectly(x,round(y),ARect,OnRender,quality,FClearType)
    else
     RenderDirectly(x,y,ARect,OnRender,quality,FClearType);
  end;
end;

procedure TFreeTypeFont.SetNameAndStyle(AName: string; AStyle: string);
begin
  AStyle := Trim(AStyle);
  if (AName = FName) and (AStyle = FStyleStr) then exit;
  FName := AName;
  FStyleStr := AStyle;
  FaceChanged;
end;

procedure TFreeTypeFont.SetNameAndStyle(AName: string; AStyle: TFreeTypeStyles);
var styleStr: string;
begin
  styleStr := '';
  if ftsBold in AStyle then styleStr += 'Bold ';
  if ftsItalic in AStyle then styleStr += 'Italic ';
  SetNameAndStyle(AName, Trim(styleStr));
end;

function TFreeTypeFont.TextWidth(AText: string): single;
var
  pstr: pchar;
  left,charcode,charlen: integer;
  maxWidth,w: single;
  idx: integer;
  g: TFreeTypeGlyph;
  prevCharcode, glyphIndex: integer;
begin
  result := 0;
  if not CheckInstance then exit;
  if AText = '' then exit;

  maxWidth := 0;
  idx := pos(LineEnding,AText);
  while idx <> 0 do
  begin
    w := TextWidth(copy(AText,1,idx-1));
    if w > maxWidth then maxWidth:= w;
    delete(AText,1,idx+length(LineEnding)-1);
    idx := pos(LineEnding,AText);
  end;
  if AText = '' then
  begin
    result := maxWidth;
    exit;
  end;

  pstr := @AText[1];
  left := length(AText);
  prevCharcode := -1;
  while left > 0 do
  begin
    charcode := UTF8CodepointToUnicode(pstr, charlen);
    inc(pstr,charlen);
    dec(left,charlen);
    glyphIndex := CharIndex[charcode];
    g := Glyph[glyphIndex];
    if g <> nil then
    with g do
    begin
      if KerningEnabled and (prevCharcode <> -1) then
        result += GetCharKerning(prevCharcode, charcode).Kerning.x;
      if FClearType then
        result += Advance/3
      else
        result += Advance;
      prevCharcode := charcode;
    end;
  end;
  if maxWidth > result then
    result := maxWidth;
end;

function TFreeTypeFont.TextHeight(AText: string): single;
var idx: integer;
    nb: integer;
begin
  if AText= '' then result := 0
   else
  begin
    result := LineFullHeight;
    nb := 1;
    idx := pos(LineEnding,AText);
    while idx <> 0 do
    begin
      nb += 1;
      delete(AText,1,idx+length(LineEnding)-1);
      idx := pos(LineEnding,AText);
    end;
    result *= nb;
  end;
end;

function TFreeTypeFont.CharWidthFromUnicode(AUnicodeChar: integer): single;
var g: TFreeTypeGlyph;
begin
  g := Glyph[CharIndex[AUnicodeChar]];
  if g = nil then result := 0
  else
    result := g.Advance;
  if FClearType then result /= 3;
end;

function TFreeTypeFont.CharWidthFromGlyph(AGlyph: integer): single;
var g: TFreeTypeGlyph;
begin
  g := Glyph[AGlyph];
  if g = nil then result := 0
  else
    result := g.Advance;
  if FClearType then result /= 3;
end;

function TFreeTypeFont.CharsWidth(AText: string): ArrayOfSingle;
var
  pstr: pchar;
  left,charcode,charlen: integer;
  resultIndex: integer;
  w: single;
  prevCharcode,glyphIndex: integer;
  g: TFreeTypeGlyph;
begin
  if AText = '' then
  begin
    setlength(result{%H-}, 0);
    exit;
  end;
  pstr := @AText[1];
  left := length(AText);
  setlength(result, UTF8Length(AText));
  resultIndex := 0;
  prevCharcode := -1;
  while left > 0 do
  begin
    charcode := UTF8CodepointToUnicode(pstr, charlen);
    inc(pstr,charlen);
    dec(left,charlen);

    glyphIndex := CharIndex[charcode];
    g := Glyph[glyphIndex];
    if g <> nil then
    with g do
    begin
      if FClearType then
        w := Advance/3
      else
        w := Advance;
      if KerningEnabled and (prevCharcode <> -1) and (resultIndex > 0) then
        result[resultIndex-1] += GetCharKerning(prevCharcode, charcode).Kerning.x;
      prevCharcode := charcode;
    end else
      w := 0;

    if resultIndex >= length(result) then
      setlength(result, resultIndex+1);
    result[resultIndex] := w;
    inc(resultIndex);
  end;
  setlength(result, resultIndex);
end;

function TFreeTypeFont.CharsPosition(AText: string): ArrayOfCharPosition;
begin
  result := CharsPosition(AText, []);
end;

function TFreeTypeFont.CharsPosition(AText: string; AAlign: TFreeTypeAlignments): ArrayOfCharPosition;
var
  resultIndex,resultLineStart: integer;
  curX: single;

  procedure ApplyHorizAlign;
  var delta: single;
      i: integer;
  begin
    if ftaLeft in AAlign then exit;
    if ftaCenter in AAlign then
      delta := -curX/2
    else if ftaRight in AAlign then
      delta := -curX
    else
      exit;

    for i := resultLineStart to resultIndex-1 do
      result[i].x += delta;
  end;

var
  pstr: pchar;
  left,charcode,charlen: integer;
  i : integer;
  w,h,y,yTopRel,yBottomRel: single;
  Found: boolean;
  StrLineEnding: string; // a string version of LineEnding, don't remove or else wont compile in UNIXes
  g: TFreeTypeGlyph;
  prevCharcode, glyphIndex: integer;
begin
  result := nil;
  if not CheckInstance then exit;
  if AText = '' then exit;
  StrLineEnding := LineEnding;
  pstr := @AText[1];
  left := length(AText);
  setlength(result, UTF8Length(AText)+1);
  resultIndex := 0;
  resultLineStart := 0;
  if ftaLeft in AAlign then AAlign -= [ftaLeft, ftaCenter, ftaRight];
  if ftaBaseline in AAlign then AAlign -= [ftaTop, ftaBaseline, ftaBottom, ftaVerticalCenter];
  curX := 0;
  y := 0;
  if ftaTop in AAlign then
  begin
    y += Ascent;
    AAlign -= [ftaTop, ftaBottom, ftaVerticalCenter];
  end;
  yTopRel := -Ascent;
  yBottomRel := Descent;
  h := LineFullHeight;
  prevCharcode := -1;
  while left > 0 do
  begin
    if (left > length(StrLineEnding)) and (pstr^ = StrLineEnding[1]) then
    begin
      Found := true;
      for i := 2 to length(StrLineEnding) do
        if (pstr+(i-1))^ <> StrLineEnding[i] then
        begin
          Found := false;
          break;
        end;
      if Found then
      begin
        for i := 1 to length(StrLineEnding) do
        begin
          with result[resultIndex] do
          begin
            x := curX;
            width := 0;
            yTop := y+yTopRel;
            yBase := y;
            yBottom := y+yBottomRel;
          end;
          inc(resultIndex);
          inc(pstr);
          dec(left);
        end;
        ApplyHorizAlign;
        y += h;
        curX := 0;
        resultLineStart := resultIndex;
        prevCharcode := -1;
        if left <= 0 then break;
      end;
    end;
    charcode := UTF8CodepointToUnicode(pstr, charlen);
    inc(pstr,charlen);
    dec(left,charlen);
    glyphIndex := CharIndex[charcode];
    g := Glyph[glyphIndex];
    if g <> nil then
    with g do
    begin
      if FClearType then
        w := Advance/3
      else
        w := Advance;
      if KerningEnabled and (prevCharcode <> -1) then
        curX += GetCharKerning(prevCharcode, charcode).Kerning.x;
      prevCharcode := charcode
    end else
      w := 0;
    if resultIndex >= length(result) then
      setlength(result, resultIndex+1);
    with result[resultIndex] do
    begin
      x := curX;
      width := w;
      yTop := y+yTopRel;
      yBase := y;
      yBottom := y+yBottomRel;
    end;
    inc(resultIndex);
    curX += w;
  end;
  if resultIndex >= length(result) then
    setlength(result, resultIndex+1);
  with result[resultIndex] do
  begin
    x := curX;
    width := 0;
    yTop := y+yTopRel;
    yBase := y;
    yBottom := y+yBottomRel;
  end;
  inc(resultIndex);
  setlength(result, resultIndex);
  ApplyHorizAlign;

  if ftaBottom in AAlign then
  begin
    y += LineFullHeight-Ascent;
    for i := 0 to high(result) do
    with result[i] do
    begin
      yTop -= y;
      yBase -= y;
      yBottom -= y;
    end;
  end else
  if ftaVerticalCenter in AAlign then
  begin
    y += LineFullHeight/2-Ascent;
    for i := 0 to high(result) do
    with result[i] do
    begin
      yTop -= y;
      yBase -= y;
      yBottom -= y;
    end;
  end;
end;

procedure TFreeTypeFont.FetchNames;
const
  maxNameIndex = 22;
var i,j: integer;
  nrPlatformID,nrEncodingID,nrLanguageID,nrNameID,len: integer;
  value,value2: string;

begin
  // setlength(FNamesArray, maxNameIndex+1);
  // wp: Move this into the "if" to avoid ignoring font files after reading defective one.
  if CheckFace then
  begin
    setlength(FNamesArray, maxNameIndex+1);
    for i := 0 to TT_Get_Name_Count(FFace)-1 do
    begin
      if TT_Get_Name_ID(FFace, i, nrPlatformID, nrEncodingID,
                        nrLanguageID, nrNameID) <> TT_Err_Ok then continue;

      if (nrNameID < 0) or (nrNameID > maxNameIndex) then continue;

        { check for Microsoft, Unicode, English }
      if ((nrPlatformID=TT_PLATFORM_MICROSOFT) and (nrEncodingID in[0,1]) and
         ((nrLanguageID=$0409) or (nrLanguageID=$0809) or
          (nrLanguageID=$0c09) or (nrLanguageID=$1009) or
          (nrLanguageID=$1409) or (nrLanguageID=$1809))) or
        { or for Unicode, English }
        ((nrPlatformID=TT_PLATFORM_APPLE_UNICODE) and
         (nrLanguageID=0)) then
      begin
        value := TT_Get_Name_String(FFace, i);
        for j := 1 to length(value) div 2 do
          pword(@value[j*2-1])^ := BEtoN(pword(@value[j*2-1])^);
        setlength(value2{%H-}, 3*(length(value) div 2) + 1); //maximum is 3-byte chars and NULL char at the end
        len := system.UnicodeToUtf8(@value2[1],length(value2),PUnicodeChar( @value[1] ),length(value) div 2);
        if len > 0 then
        begin
          setlength(value2, len-1 );
          value := value2;
        end;
        FNamesArray[nrNameID] := value;
      end;
    end;
  end;
end;

function TFreeTypeFont.GetCollection: TCustomFreeTypeFontCollection;
begin
  if FCollection = nil then
    result := FontCollection
  else
    result := FCollection;
end;

function TFreeTypeFont.CheckFace: boolean;
begin
  if FFaceChanged then
  begin
    FFaceChanged:= false;
    LoadFace;
  end;
  result := FFaceLoaded;
end;

function TFreeTypeFont.GetCharKerning(AUnicodeCharLeft, AUnicodeCharRight: integer): TFreeTypeKerning;
const
  UpperCaseKerningLeft = 'FPTVWY';
  UpperCaseKerningRight = 'TVWY';
  LowerCaseKerningLeftA = 'bcehmnops';
  LowerCaseKerningRightA = 'cdegoqs';
  LowerCaseKerningLeftU = 'gkqrvwxyz';
  LowerCaseKerningRightU = 'mnprvwxyz';
  LowerCaseKerningLeftACircumflex = 'ĉêôŝ';
  LowerCaseKerningRightACircumflex = 'ĉêĝôŝ';
  LowerCaseKerningLeftUCircumflex = 'ĝŵŷẑ';
  LowerCaseKerningRightUCircumflex = 'ŵŷẑ';
  LowerCaseKerningLeftADiaresis = 'ëö';
  LowerCaseKerningRightADiaresis = 'ëö';
  LowerCaseKerningLeftUDiaresis = 'ẅẍÿ';
  LowerCaseKerningRightUDiaresis = 'ẅẍÿ';
  LowerCaseKerningLeftAAcute = 'ćéḿńóṕś';
  LowerCaseKerningRightAAcute = 'ćéǵóś';
  LowerCaseKerningLeftUAcute = 'ǵŕẃýź';
  LowerCaseKerningRightUAcute = 'ḿńṕŕẃýź';
  LowerCaseKerningLeftAGrave = 'èǹò';
  LowerCaseKerningRightAGrave = 'èò';
  LowerCaseKerningLeftUGrave = 'ẁỳ';
  LowerCaseKerningRightUGrave = 'ǹẁỳ';
type
  TKerningFallbackInfo = record
    u: integer;      //composed charcode
    fb: integer;     //fallback code
  end;
const
  KerningFallbackInfo : array[0..195] of TKerningFallbackInfo = (
  (u:$C0; fb:$41), (u:$C1; fb:$41), (u:$C2; fb:$41), (u:$C3; fb:$41), (u:$C4; fb:$41),
  (u:$C5; fb:$41), (u:$C7; fb:$43), (u:$C8; fb:$45), (u:$C9; fb:$45), (u:$CA; fb:$45),
  (u:$CB; fb:$45), (u:$CC; fb:$49), (u:$CD; fb:$49), (u:$CE; fb:$49), (u:$CF; fb:$49),
  (u:$D1; fb:$4E), (u:$D2; fb:$4F), (u:$D3; fb:$4F), (u:$D4; fb:$4F), (u:$D5; fb:$4F),
  (u:$D6; fb:$4F), (u:$D9; fb:$55), (u:$DA; fb:$55), (u:$DB; fb:$55), (u:$DC; fb:$55),
  (u:$DD; fb:$59), (u:$100; fb:$41), (u:$102; fb:$41), (u:$104; fb:$41),
  (u:$106; fb:$43), (u:$108; fb:$43), (u:$10A; fb:$43), (u:$10C; fb:$43),
  (u:$10E; fb:$44), (u:$112; fb:$45), (u:$114; fb:$45), (u:$116; fb:$45),
  (u:$118; fb:$45), (u:$11A; fb:$45), (u:$11C; fb:$47), (u:$11E; fb:$47),
  (u:$120; fb:$47), (u:$122; fb:$47), (u:$124; fb:$48), (u:$128; fb:$49),
  (u:$12A; fb:$49), (u:$12C; fb:$49), (u:$12E; fb:$49), (u:$130; fb:$49),
  (u:$134; fb:$4A), (u:$136; fb:$4B), (u:$139; fb:$4C), (u:$13B; fb:$4C),
  (u:$13D; fb:$4C), (u:$143; fb:$4E), (u:$145; fb:$4E), (u:$147; fb:$4E),
  (u:$14C; fb:$4F), (u:$14E; fb:$4F), (u:$150; fb:$4F), (u:$154; fb:$52),
  (u:$156; fb:$52), (u:$158; fb:$52), (u:$15A; fb:$53), (u:$15C; fb:$53),
  (u:$15E; fb:$53), (u:$160; fb:$53), (u:$162; fb:$54), (u:$164; fb:$54),
  (u:$168; fb:$55), (u:$16A; fb:$55), (u:$16C; fb:$55), (u:$16E; fb:$55),
  (u:$170; fb:$55), (u:$172; fb:$55), (u:$174; fb:$57), (u:$176; fb:$59),
  (u:$178; fb:$59), (u:$179; fb:$5A), (u:$17B; fb:$5A), (u:$17D; fb:$5A),
  (u:$1CD; fb:$41), (u:$1CF; fb:$49), (u:$1D1; fb:$4F), (u:$1D3; fb:$55),
  (u:$1E2; fb:$C6), (u:$1E6; fb:$47), (u:$1E8; fb:$4B), (u:$1EA; fb:$4F),
  (u:$1F4; fb:$47), (u:$1F8; fb:$4E), (u:$1FC; fb:$C6), (u:$200; fb:$41),
  (u:$202; fb:$41), (u:$204; fb:$45), (u:$206; fb:$45), (u:$208; fb:$49),
  (u:$20A; fb:$49), (u:$20C; fb:$4F), (u:$20E; fb:$4F), (u:$210; fb:$52),
  (u:$212; fb:$52), (u:$214; fb:$55), (u:$216; fb:$55), (u:$218; fb:$53),
  (u:$21A; fb:$54), (u:$21E; fb:$48), (u:$226; fb:$41), (u:$228; fb:$45),
  (u:$22E; fb:$4F), (u:$232; fb:$59), (u:$38F; fb:$3A9), (u:$403; fb:$413),
  (u:$476; fb:$474), (u:$4EA; fb:$4E8), (u:$1E00; fb:$41), (u:$1E02; fb:$42),
  (u:$1E04; fb:$42), (u:$1E06; fb:$42), (u:$1E08; fb:$C7), (u:$1E0A; fb:$44),
  (u:$1E0C; fb:$44), (u:$1E0E; fb:$44), (u:$1E10; fb:$44), (u:$1E12; fb:$44),
  (u:$1E18; fb:$45), (u:$1E1A; fb:$45), (u:$1E1E; fb:$46), (u:$1E20; fb:$47),
  (u:$1E22; fb:$48), (u:$1E24; fb:$48), (u:$1E26; fb:$48), (u:$1E28; fb:$48),
  (u:$1E2A; fb:$48), (u:$1E2C; fb:$49), (u:$1E30; fb:$4B), (u:$1E32; fb:$4B),
  (u:$1E34; fb:$4B), (u:$1E36; fb:$4C), (u:$1E3A; fb:$4C), (u:$1E3C; fb:$4C),
  (u:$1E3E; fb:$4D), (u:$1E40; fb:$4D), (u:$1E42; fb:$4D), (u:$1E44; fb:$4E),
  (u:$1E46; fb:$4E), (u:$1E48; fb:$4E), (u:$1E4A; fb:$4E), (u:$1E54; fb:$50),
  (u:$1E56; fb:$50), (u:$1E58; fb:$52), (u:$1E5A; fb:$52), (u:$1E5E; fb:$52),
  (u:$1E60; fb:$53), (u:$1E62; fb:$53), (u:$1E6A; fb:$54), (u:$1E6C; fb:$54),
  (u:$1E6E; fb:$54), (u:$1E70; fb:$54), (u:$1E72; fb:$55), (u:$1E74; fb:$55),
  (u:$1E76; fb:$55), (u:$1E7C; fb:$56), (u:$1E7E; fb:$56), (u:$1E80; fb:$57),
  (u:$1E82; fb:$57), (u:$1E84; fb:$57), (u:$1E86; fb:$57), (u:$1E88; fb:$57),
  (u:$1E8A; fb:$58), (u:$1E8C; fb:$58), (u:$1E8E; fb:$59), (u:$1E90; fb:$5A),
  (u:$1E92; fb:$5A), (u:$1E94; fb:$5A), (u:$1EA0; fb:$41), (u:$1EA2; fb:$41),
  (u:$1EB8; fb:$45), (u:$1EBA; fb:$45), (u:$1EBC; fb:$45), (u:$1EC8; fb:$49),
  (u:$1ECA; fb:$49), (u:$1ECC; fb:$4F), (u:$1ECE; fb:$4F), (u:$1EE4; fb:$55),
  (u:$1EE6; fb:$55), (u:$1EF2; fb:$59), (u:$1EF4; fb:$59), (u:$1EF6; fb:$59),
  (u:$1EF8; fb:$59), (u:$1F68; fb:$3A9), (u:$1F69; fb:$3A9), (u:$1FFA; fb:$3A9),
  (u:$1FFC; fb:$3A9), (u:$2126; fb:$3A9), (u:$212A; fb:$4B));

  function FindFallback(var ACode: integer): boolean;
  var
    minIdx, maxIdx, midIdx: Integer;
  begin
    minIdx := low(KerningFallbackInfo);
    maxIdx := high(KerningFallbackInfo);
    while minIdx < maxIdx do
    begin
      midIdx := (minIdx+maxIdx) shr 1;
      if ACode > KerningFallbackInfo[midIdx].u then
        minIdx := midIdx+1
      else
        maxIdx := midIdx;
    end;
    if KerningFallbackInfo[minIdx].u = ACode then
    begin
      ACode := KerningFallbackInfo[minIdx].fb;
      if ACode = $C7 {C WITH CEDILLA} then ACode := ord('C');
      result := true;
    end
    else result := false;
  end;
var
  glyphLeft, glyphRight: integer;
  isFallback: Boolean;
  leftUTF8, rightUTF8: String;
begin
  glyphLeft := CharIndex[AUnicodeCharLeft];
  glyphRight := CharIndex[AUnicodeCharRight];
  result := GetGlyphKerning(glyphLeft, glyphRight);
  if not result.Found and KerningFallbackEnabled then
  begin
    //try to find glyphs without accents
    isFallback := false;
    if FindFallback(AUnicodeCharLeft) then
    begin
      glyphLeft := CharIndex[AUnicodeCharLeft];
      isFallback := true;
    end;
    if FindFallback(AUnicodeCharRight) then
    begin
      glyphRight := CharIndex[AUnicodeCharRight];
      isFallback := true;
    end;
    if isFallback then
    begin
      result := GetGlyphKerning(glyphLeft, glyphRight);
      if result.Found then exit;
    end;

    //try to find equivalence for kernings that were not forseen by the font (ex: AE, Vs)
    if AUnicodeCharRight = $C6 {AE} then
    begin
      AUnicodeCharRight := ord('A');
      glyphRight := CharIndex[AUnicodeCharRight];
      result := GetGlyphKerning(glyphLeft, glyphRight);
      if result.Found then exit;
    end else
    if AUnicodeCharRight = $152 {OE} then
    begin
      AUnicodeCharRight := ord('O');
      glyphRight := CharIndex[AUnicodeCharRight];
      result := GetGlyphKerning(glyphLeft, glyphRight);
      if result.Found then exit;
    end;

    if (AUnicodeCharLeft < 128) and (AUnicodeCharRight < 128) then
    begin
      if (pos(chr(AUnicodeCharLeft), UpperCaseKerningLeft) <> 0) and
         (pos(chr(AUnicodeCharRight), LowerCaseKerningRightA) <> 0) then
         exit(GetCharKerning(AUnicodeCharLeft, ord('a')));

      if (pos(chr(AUnicodeCharLeft), LowerCaseKerningLeftA) <> 0) and
         (pos(chr(AUnicodeCharRight), UpperCaseKerningRight) <> 0) then
         exit(GetCharKerning(ord('a'), AUnicodeCharRight));

      if (pos(chr(AUnicodeCharLeft), UpperCaseKerningLeft) <> 0) and
         (pos(chr(AUnicodeCharRight), LowerCaseKerningRightU) <> 0) then
         exit(GetCharKerning(AUnicodeCharLeft, ord('u')));

      if (pos(chr(AUnicodeCharLeft), LowerCaseKerningLeftU) <> 0) and
         (pos(chr(AUnicodeCharRight), UpperCaseKerningRight) <> 0) then
         exit(GetCharKerning(ord('u'), AUnicodeCharRight));
    end else
    begin
      leftUTF8 := UnicodeToUTF8(AUnicodeCharLeft);
      rightUTF8 := UnicodeToUTF8(AUnicodeCharRight);

      if (pos(leftUTF8, UpperCaseKerningLeft) <> 0) and
         (pos(rightUTF8, LowerCaseKerningRightACircumflex) <> 0) then
         exit(GetCharKerning(AUnicodeCharLeft, $E2));

      if (pos(leftUTF8, LowerCaseKerningLeftACircumflex) <> 0) and
         (pos(rightUTF8, UpperCaseKerningRight) <> 0) then
         exit(GetCharKerning($E2, AUnicodeCharRight));

      if (pos(leftUTF8, UpperCaseKerningLeft) <> 0) and
         (pos(rightUTF8, LowerCaseKerningRightUCircumflex) <> 0) then
         exit(GetCharKerning(AUnicodeCharLeft, $FB));

      if (pos(leftUTF8, LowerCaseKerningLeftUCircumflex) <> 0) and
         (pos(rightUTF8, UpperCaseKerningRight) <> 0) then
         exit(GetCharKerning($FB, AUnicodeCharRight));


      if (pos(leftUTF8, UpperCaseKerningLeft) <> 0) and
         (pos(rightUTF8, LowerCaseKerningRightADiaresis) <> 0) then
         exit(GetCharKerning(AUnicodeCharLeft, $E4));

      if (pos(leftUTF8, LowerCaseKerningLeftADiaresis) <> 0) and
         (pos(rightUTF8, UpperCaseKerningRight) <> 0) then
         exit(GetCharKerning($E4, AUnicodeCharRight));

      if (pos(leftUTF8, UpperCaseKerningLeft) <> 0) and
         (pos(rightUTF8, LowerCaseKerningRightUDiaresis) <> 0) then
         exit(GetCharKerning(AUnicodeCharLeft, $FC));

      if (pos(leftUTF8, LowerCaseKerningLeftUDiaresis) <> 0) and
         (pos(rightUTF8, UpperCaseKerningRight) <> 0) then
         exit(GetCharKerning($FC, AUnicodeCharRight));


      if (pos(leftUTF8, UpperCaseKerningLeft) <> 0) and
         (pos(rightUTF8, LowerCaseKerningRightAAcute) <> 0) then
         exit(GetCharKerning(AUnicodeCharLeft, $E1));

      if (pos(leftUTF8, LowerCaseKerningLeftAAcute) <> 0) and
         (pos(rightUTF8, UpperCaseKerningRight) <> 0) then
         exit(GetCharKerning($E1, AUnicodeCharRight));

      if (pos(leftUTF8, UpperCaseKerningLeft) <> 0) and
         (pos(rightUTF8, LowerCaseKerningRightUAcute) <> 0) then
         exit(GetCharKerning(AUnicodeCharLeft, $FA));

      if (pos(leftUTF8, LowerCaseKerningLeftUAcute) <> 0) and
         (pos(rightUTF8, UpperCaseKerningRight) <> 0) then
         exit(GetCharKerning($FA, AUnicodeCharRight));


      if (pos(leftUTF8, UpperCaseKerningLeft) <> 0) and
         (pos(rightUTF8, LowerCaseKerningRightAGrave) <> 0) then
         exit(GetCharKerning(AUnicodeCharLeft, $E0));

      if (pos(leftUTF8, LowerCaseKerningLeftAGrave) <> 0) and
         (pos(rightUTF8, UpperCaseKerningRight) <> 0) then
         exit(GetCharKerning($E0, AUnicodeCharRight));

      if (pos(leftUTF8, UpperCaseKerningLeft) <> 0) and
         (pos(rightUTF8, LowerCaseKerningRightUGrave) <> 0) then
         exit(GetCharKerning(AUnicodeCharLeft, $F9));

      if (pos(leftUTF8, LowerCaseKerningLeftUGrave) <> 0) and
         (pos(rightUTF8, UpperCaseKerningRight) <> 0) then
         exit(GetCharKerning($F9, AUnicodeCharRight));
    end;
  end;
end;

function TFreeTypeFont.CheckInstance: boolean;
begin
  result := CheckFace and FInstanceCreated;
end;

{ TFreeTypeGrayscaleMap }

procedure TFreeTypeGrayscaleMap.Init(AWidth, AHeight: integer);
begin
  map.Width := AWidth;
  map.Rows := AHeight;
  map.Cols:= (AWidth+3) and not 3;
  map.flow:= TT_Flow_Down;
  map.Size:= map.Rows*map.Cols;
  getmem(map.Buffer,map.Size);
  Clear;
  RenderQuality := grqHighQuality;
end;

function TFreeTypeGrayscaleMap.RenderGlyph(glyph: TFreeTypeGlyph; x, y: single): boolean;
var mono: TFreeTypeMonochromeMap;
    psrc,pdest: pbyte;
    xb,yb,tx: integer;
    curBit: byte;
begin
  case RenderQuality of
    grqMonochrome:
      begin
        tx := Width;
        mono := TFreeTypeMonochromeMap.Create(FRasterizer, tx,Height);
        result := mono.RenderGlyph(glyph,x,y);
        if result then
        begin
          for yb := mono.Height-1 downto 0 do
          begin
            psrc := mono.ScanLine[yb];
            pdest := self.ScanLine[yb];
            curBit := $80;
            for xb := tx-1 downto 0 do
            begin
              if psrc^ and curBit <> 0 then
                pdest^ := $ff;
              curBit := curBit shr 1;
              if curBit = 0 then
              begin
                curBit := $80;
                inc(psrc);
              end;
              inc(pdest);
            end;
          end;
        end;
        mono.Free;
      end;
    grqLowQuality:
      begin
        FRasterizer.Set_Raster_Palette(RegularGray5);
        result := TT_Get_Glyph_Pixmap(glyph.data, map, round(x*64), round((height-y)*64), FRasterizer) = TT_Err_Ok;
      end;
    grqHighQuality:
      begin
        result := TT_Get_Glyph_Pixmap_HQ(glyph.data, map, round(x*64), round((height-y)*64), FRasterizer) = TT_Err_Ok;
      end;
  end;
end;

procedure TFreeTypeGrayscaleMap.ScanMoveTo(x, y: integer);
begin
  ScanPtrStart := pbyte(ScanLine[y]);
  ScanX := x mod Width;
  if ScanX < 0 then inc(ScanX,Width);
end;

function TFreeTypeGrayscaleMap.ScanNextPixel: byte;
begin
  if ScanPtrStart = nil then
    result := 0
  else
  begin
    result := (ScanPtrStart+ScanX)^;
    inc(ScanX);
    if ScanX = map.Width then ScanX := 0;
  end;
end;

function TFreeTypeGrayscaleMap.GetPixel(x, y: integer): byte;
begin
  if (x < 0) or (x>= width) or (y <0) or (y >= height) then
    result := 0
  else
    result := (pbyte(map.Buffer) + y*map.Cols + x)^;
end;

procedure TFreeTypeGrayscaleMap.SetPixel(x, y: integer; value: byte);
begin
  if (x < 0) or (x>= width) or (y <0) or (y >= height) then
    exit
  else
    (pbyte(map.Buffer) + y*map.Cols + x)^ := value;
end;

procedure TFreeTypeGrayscaleMap.XorPixel(x, y: integer; value: byte);
var p : pbyte;
begin
  if (x < 0) or (x>= width) or (y <0) or (y >= height) then
    exit
  else
  begin
    p := (pbyte(map.Buffer) + y*map.Cols + x);
    p^ := p^ xor value;
  end;
end;

{ TFreeTypeRasterMap }

function TFreeTypeRasterMap.GetHeight: integer;
begin
  result := map.Rows;
end;

function TFreeTypeRasterMap.GetWidth: integer;
begin
  result := map.Width;
end;

function TFreeTypeRasterMap.GetScanLine(y: integer): pointer;
begin
  if (y <0) or (y >= height) then
    result := nil
  else
    Result:= pointer(pbyte(map.Buffer) + y*map.Cols);
end;

constructor TFreeTypeRasterMap.Create(AWidth, AHeight: integer);
begin
  FRasterizer := TTGetDefaultRasterizer;
  Init(AWidth,AHeight);
end;

constructor TFreeTypeRasterMap.Create(ARasterizer: TFreeTypeRasterizer; AWidth,
  AHeight: integer);
begin
  FRasterizer := ARasterizer;
  Init(AWidth,AHeight);
end;

procedure TFreeTypeRasterMap.Clear;
begin
  fillchar(map.Buffer^, map.Size, 0);
end;

procedure TFreeTypeRasterMap.Fill;
begin
  fillchar(map.Buffer^, map.Size, $ff);
end;

destructor TFreeTypeRasterMap.Destroy;
begin
  freemem(map.Buffer);
  inherited Destroy;
end;

{ TFreeTypeMonochromeMap }

function TFreeTypeMonochromeMap.RenderGlyph(glyph: TFreeTypeGlyph; x,y: single): boolean;
begin
  result := TT_Get_Glyph_Bitmap(glyph.data, map, round(x*64), round((height-y)*64), FRasterizer) = TT_Err_Ok;
end;

procedure TFreeTypeMonochromeMap.ScanMoveTo(x, y: integer);
begin
  ScanPtrStart := pbyte(ScanLine[y]);
  ScanX := x mod Width;
  if ScanX < 0 then inc(ScanX,Width);

  if ScanPtrStart <> nil then
  begin
    ScanPtrCur := ScanPtrStart + (ScanX shr 3);
    ScanBit := $80 shr (ScanX and 7);
  end else
  begin
    ScanPtrCur := nil;
    ScanBit := 0;
  end;
end;

function TFreeTypeMonochromeMap.ScanNextPixel: boolean;
begin
  if ScanPtrCur = nil then
    result := false
  else
  begin
    result := (pbyte(ScanPtrCur)^ and ScanBit) <> 0;
    inc(ScanX);
    if ScanX = map.Width then
    begin
      ScanX := 0;
      ScanBit := $80;
      ScanPtrCur := ScanPtrStart;
    end else
    begin
      ScanBit := ScanBit shr 1;
      if ScanBit = 0 then
      begin
        ScanBit := $80;
        inc(ScanPtrCur);
      end;
    end;
  end;
end;

function TFreeTypeMonochromeMap.GetPixel(x, y: integer): boolean;
begin
  if (x < 0) or (x>= width) or (y <0) or (y >= height) then
    result := false
  else
    result := (pbyte(map.Buffer) + y*map.Cols + (x shr 3))^ and ($80 shr (x and 7)) <> 0;
end;

procedure TFreeTypeMonochromeMap.SetPixel(x, y: integer; value: boolean);
var p: pbyte;
begin
  if (x < 0) or (x>= width) or (y <0) or (y >= height) then
    exit
  else
  begin
    p := pbyte(map.Buffer) + y*map.Cols + (x shr 3);
    if not value then
      p^ := p^ and not ($80 shr (x and 7))
    else
      p^ := p^ or ($80 shr (x and 7));
  end;
end;

function TFreeTypeMonochromeMap.GetPixelsInRect(x, y, x2, y2: integer): integer;
var yb: integer;
begin
  result := 0;

  if x < 0 then x := 0;
  if x2 > width then x2 := width;
  if x2 <= x then exit;

  if y < 0 then y := 0;
  if y2 > height then y2 := height;
  for yb := y to y2-1 do
    result += GetPixelsInHorizlineNoBoundsChecking(x,yb,x2-1);
end;

function TFreeTypeMonochromeMap.GetPixelsInHorizline(x, y, x2: integer): integer;
begin
  if x < 0 then x := 0;
  if x2 >= width then x2 := width-1;
  if x2 <= x then
  begin
    result := 0;
    exit;
  end;
  if (y < 0) or (y >= height) then
  begin
    result := 0;
    exit;
  end;

  result := GetPixelsInHorizlineNoBoundsChecking(x,y,x2);
end;

function TFreeTypeMonochromeMap.GetPixelsInHorizlineNoBoundsChecking(x, y, x2: integer
  ): integer;
var p: pbyte;
    ix,ix2: integer;
begin
  result := 0;
  ix := x shr 3;
  ix2 := x2 shr 3;
  p := pbyte(map.Buffer) + y*map.Cols + ix;
  if ix2 > ix then
  begin
    result += BitCountTable[ p^ and ($ff shr (x and 7)) ];
    inc(p^);
    inc(ix);
    while (ix2 > ix) do
    begin
      result += BitCountTable[p^];
      inc(ix);
      inc(p^);
    end;
    result += BitCountTable[ p^ and ($ff shl (x2 and 7 xor 7)) ];
  end else
    result += BitCountTable[ p^ and ($ff shr (x and 7)) and ($ff shl (x2 and 7 xor 7))];
end;

procedure TFreeTypeMonochromeMap.Init(AWidth, AHeight: integer);
begin
  map.Width := AWidth;
  map.Rows := AHeight;
  map.Cols:= (AWidth+7) shr 3;
  map.flow:= TT_Flow_Down;
  map.Size:= map.Rows*map.Cols;
  getmem(map.Buffer,map.Size);
  Clear;
end;

procedure TFreeTypeMonochromeMap.TogglePixel(x, y: integer);
var p: pbyte;
begin
  if (x < 0) or (x>= width) or (y <0) or (y >= height) then
    exit
  else
  begin
    p := pbyte(map.Buffer) + y*map.Cols + (x shr 3);
    p^ := p^ xor ($80 shr (x and 7));
  end;
end;

procedure InitTables;
var i: integer;
begin
  for i := 0 to 255 do
  begin
    BitCountTable[i] := (i and 1) + (i shr 1 and 1) + (i shr 2 and 1) + (i shr 3 and 1) +
       (i shr 4 and 1) + (i shr 5 and 1) + (i shr 6 and 1) + (i shr 7 and 1);
  end;

  RegularGray5[0] := 0;
  RegularGray5[1] := $60;
  RegularGray5[2] := $a0;
  RegularGray5[3] := $d0;
  RegularGray5[4] := $ff;
end;

{ from TFreeTypeDrawer }

procedure TFreeTypeRenderableFont.DrawPixel(X, Y: integer; const c: TColor32);
begin
	BlendMem_ASM(c, FDestination.PixelPtr[X,Y]^);
end;

procedure TFreeTypeRenderableFont.ClippedDrawPixel(X, Y: integer; const c: TColor32);
begin
	if (X >= 0) and (Y >= 0) and (X < FDestination.Width) and (Y < FDestination.Height) then
		DrawPixel(X, Y, c);
end;

procedure TFreeTypeRenderableFont.RenderDirectly(X, Y, tx: Integer; data: Pointer);
var
	a: Byte;
	//fontalpha: Single;
	fontcolor: TColor32;
	psrc: PByte;
	pdst: PColor32;
begin
	if (FDestination = nil) or
		(Y < 0) or (Y >= FDestination.Height) or
		(X < 0) or (X >  FDestination.Width-tx) then Exit;

	//fontalpha := FColor shr 24 / 255;      // font color alpha
	fontcolor := FColor and $FFFFFF; // font color rgb

	psrc := PByte(data);               // alpha values
	pdst := FDestination.PixelPtr[X,Y]; // background bitmap

	while tx > 0 do
	begin
		a := psrc^; // glyph pixel alpha
		if a > 0 then
			pdst^ := BlendColors(a shl 24 or fontcolor, pdst^);
		Inc(psrc); Inc(pdst);
		Dec(tx);
	end;
end;

procedure TFreeTypeRenderableFont.DrawText(Buffer: TBitmap32; const AText: String;
	X, Y: Single; AColor: TColor32);
begin
	FDestination := Buffer;
	FColor := AColor;
	RenderText(AText, X, Y, FDestination.BoundsRect, @RenderDirectly);
end;

procedure TFreeTypeRenderableFont.DrawText(Buffer: TBitmap32; const AText: String;
	X, Y: Single; AColor: TColor32; AOpacity: Byte);
var
	col: TColor32Entry;
begin
	col.ARGB := AColor;
	col.A := col.A * AOpacity div 255;
	DrawText(Buffer, AText, X, Y, col.ARGB, []);
end;

procedure TFreeTypeRenderableFont.DrawText(Buffer: TBitmap32; const AText: String;
	X, Y: Single; AColor: TColor32; AOpacity: Byte; AAlign: TFreeTypeAlignments);
var
	col: TColor32Entry;
begin
	col.ARGB := AColor;
	col.A := col.A * AOpacity div 255;
	DrawText(Buffer, AText, X, Y, col.ARGB, AAlign);
end;

procedure TFreeTypeRenderableFont.DrawText(Buffer: TBitmap32; AText: String;
	X, Y: Single; AColor: TColor32; AAlign: TFreeTypeAlignments);
var
	idx: Integer;
	delta: Single;
begin
	if not (ftaBaseline in AAlign) then
	begin
		if ftaTop in AAlign then
			y += Ascent
		else
		if ftaBottom in AAlign then
			y += Ascent - TextHeight(AText)
		else
		if ftaVerticalCenter in AAlign then
			y += Ascent - TextHeight(AText) * 0.5;
	end;
	AAlign -= [ftaTop,ftaBaseline,ftaBottom,ftaVerticalCenter];

	idx := pos(LineEnding, AText);
	while idx <> 0 do
	begin
		DrawText(Buffer, Copy(AText, 1, idx-1), X, Y, AColor, AAlign);
		Delete(AText, 1, idx+length(LineEnding)-1);
		idx := pos(LineEnding, AText);
		y += LineFullHeight;
	end;

	if not (ftaLeft in AAlign) then
	begin
		delta := 0;
		if ftaCenter in AAlign then
			delta := -TextWidth(AText) / 2
		else
		if ftaRight in AAlign then
			delta := -TextWidth(AText);
		if Hinted then delta := round(delta);
		x += delta;
	end;

	DrawText(Buffer, AText, X, Y, AColor);
end;

procedure TFreeTypeRenderableFont.DrawTextWordBreak(Buffer: TBitmap32; AText: String;
	X, Y, AMaxWidth: Single; AColor: TColor32; AAlign: TFreeTypeAlignments);
var
	i: integer;
	stepX, stepY, lineShift: single;
	lineAlignment: TFreeTypeAlignments;
	ARemains: string;
	lines: TStringList;
begin
	if (AText = '') or (AMaxWidth <= 0) then Exit;

	stepX := 0;
	stepY := LineFullHeight;

	AAlign -= [ftaBaseline]; //ignored
	if AAlign * [ftaTop,ftaVerticalCenter,ftaBottom] = [] then AAlign += [ftaTop]; //top by default
	lineAlignment := AAlign * [ftaLeft,ftaCenter,ftaRight] + [ftaVerticalCenter];

	if ftaTop in AAlign then
	begin
		lineShift := 0.5;
		X += stepX*lineShift;
		Y += stepY*lineShift;
		repeat
			SplitText(AText, AMaxWidth, ARemains);
			DrawText(Buffer, AText, X, Y, AColor, lineAlignment);
			AText := ARemains;
			X += stepX;
			Y += stepY;
		until ARemains = '';
	end
	else
	begin
		lines := TStringList.Create;
		repeat
			SplitText(AText, AMaxWidth, ARemains);
			lines.Add(AText);
			AText := ARemains;
		until ARemains = '';

		if ftaVerticalCenter in AAlign then
			lineShift := lines.Count / 2 - 0.5
		else
		if ftaBottom in AAlign then
			lineShift := lines.Count - 0.5
		else
			lineShift := -0.5;

		X -= stepX * lineShift;
		Y -= stepY * lineShift;
		for i := 0 to lines.Count-1 do
		begin
			DrawText(Buffer, lines[i], X, Y, AColor, lineAlignment);
			X += stepX;
			Y += stepY;
		end;
		lines.Free;
	end;
end;

procedure TFreeTypeRenderableFont.DrawTextRect(Buffer: TBitmap32; const AText: String;
	X1, Y1, X2, Y2: Single; AColor: TColor32; AAlign: TFreeTypeAlignments);
var
	X, Y: single;
begin
	if X2 <= X1 then Exit;

	if ftaVerticalCenter in AAlign then
		Y := (Y1 + Y2) / 2
	else
	if ftaBottom in AAlign then
		Y := Y2
	else
		Y := Y1;

	if ftaCenter in AAlign then
		X := (X1 + X2) / 2
	else
	if ftaRight in AAlign then
		X := X2
	else
		X := X1;

	DrawTextWordBreak(Buffer, AText, X, Y, X2-X1, AColor, AAlign);
end;

procedure TFreeTypeRenderableFont.DrawGlyph(Buffer: TBitmap32; AGlyph: Integer;
	X, Y: Single; AColor: TColor32);
begin
	if Self is TFreeTypeFont then
	begin
		FColor := AColor;
		FDestination := Buffer;
		TFreeTypeFont(Self).RenderGlyph(AGlyph, X, Y, FDestination.BoundsRect, @RenderDirectly);
	end;
end;

procedure TFreeTypeRenderableFont.DrawGlyph(Buffer: TBitmap32; AGlyph: Integer;
	X, Y: Single; AColor: TColor32; AAlign: TFreeTypeAlignments);
var
	F: TFreeTypeFont;
begin
	if not (Self is TFreeTypeFont) then Exit;

	F := TFreeTypeFont(Self);

	if ftaTop in AAlign then
		y += Ascent
	else
	if ftaVerticalCenter in AALign then
		y += Ascent - LineFullHeight * 0.5
	else
	if ftaBottom in AAlign then
		y += Ascent - LineFullHeight;

	if ftaCenter in AAlign then
		x -= F.CharWidthFromGlyph(AGlyph) * 0.5
	else
	if ftaRight in AAlign then
		x -= F.CharWidthFromGlyph(AGlyph);

	DrawGlyph(Buffer, AGlyph, X, Y, AColor);
end;


initialization

  FreeTypeInitialized := false;
  FreeTypeCannotInitialize := false;
  InitTables;

finalization

  if FreeTypeInitialized then
  begin
    TT_Done_FreeType;
    FreeTypeInitialized := false;
  end;

end.

