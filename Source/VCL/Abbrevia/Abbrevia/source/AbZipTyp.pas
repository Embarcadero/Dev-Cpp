(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Abbrevia
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Craig Peterson <capeterson@users.sourceforge.net>
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* ABBREVIA: AbZipTyp.pas                                *}
{*********************************************************}
{* ABBREVIA: PKZip types                                 *}
{* Based on information from Appnote.txt, shipped with   *}
{* PKWare's PKZip for Windows 2.5                        *}
{*********************************************************}

unit AbZipTyp;

{$I AbDefine.inc}

interface

uses
  SysUtils, Classes, AbArcTyp, AbUtils, AbSpanSt;

const
  { note  #$50 = 'P', #$4B = 'K'}
  Ab_ZipVersion = 63;
  Ab_ZipLocalFileHeaderSignature            : Integer = $04034B50;
  Ab_ZipDataDescriptorSignature             : Integer = $08074B50;
  Ab_ZipCentralDirectoryFileHeaderSignature : Integer = $02014B50;
  Ab_Zip64EndCentralDirectorySignature      : Integer = $06064B50;
  Ab_Zip64EndCentralDirectoryLocatorSignature:Integer = $07064B50;
  Ab_ZipEndCentralDirectorySignature        : Integer = $06054B50;
  Ab_ZipSpannedSetSignature                 : Integer = $08074B50;
  Ab_ZipPossiblySpannedSignature            : Integer = $30304B50;
  Ab_GeneralZipSignature                    : Word    = $4B50;

  Ab_ArchiveExtraDataRecord                 : Integer = $08064B50;
  Ab_DigitalSignature                       : Integer = $05054B50;

  Ab_WindowsExeSignature                    : Word    = $5A4D;
  Ab_LinuxExeSignature                      : Integer = $464C457F;

  AbDefZipSpanningThreshold = 0;
  AbDefPasswordRetries      = 3;
  AbFileIsEncryptedFlag     = $0001;
  AbHasDataDescriptorFlag   = $0008;
  AbLanguageEncodingFlag    = $0800;

  Ab_Zip64SubfieldID                        : Word    = $0001;
  Ab_InfoZipUnicodePathSubfieldID           : Word    = $7075;
  Ab_XceedUnicodePathSubfieldID             : Word    = $554E;
  Ab_XceedUnicodePathSignature              : UInt32= $5843554E;

type
  PAbByteArray4K = ^TAbByteArray4K;
  TAbByteArray4K = array[1..4096] of Byte;
  PAbByteArray8K = ^TAbByteArray8K;
  TAbByteArray8K = array[0..8192] of Byte;
  PAbIntArray8K  = ^TAbIntArray8K;
  TAbIntArray8K  = array[0..8192] of SmallInt;

  PAbWordArray   = ^TAbWordArray;
  TAbWordArray   = array[0..65535 div SizeOf(Word)-1] of Word;
  PAbByteArray   = ^TAbByteArray;
  TAbByteArray   = array[0..65535-1] of Byte;
  PAbSmallIntArray = ^TAbSmallIntArray;
  TAbSmallIntArray = array[0..65535 div SizeOf(SmallInt)-1] of SmallInt;

  PAbIntegerArray = ^TAbIntegerArray;
  TAbIntegerArray = array[0..65535 div sizeof(integer)-1] of integer;

  TAbZip64EndOfCentralDirectoryRecord = packed record
    Signature               : Integer;
    RecordSize              : Int64;
    VersionMadeBy           : Word;
    VersionNeededToExtract  : Word;
    DiskNumber              : UInt32;
    StartDiskNumber         : UInt32;
    EntriesOnDisk           : Int64;
    TotalEntries            : Int64;
    DirectorySize           : Int64;
    DirectoryOffset         : Int64;
  end;

  TAbZip64EndOfCentralDirectoryLocator = packed record
    Signature               : Integer;
    StartDiskNumber         : Integer;
    RelativeOffset          : Int64;
    TotalDisks              : Integer;
  end;

  TAbZipEndOfCentralDirectoryRecord = packed record
    Signature               : Integer;
    DiskNumber              : Word;
    StartDiskNumber         : Word;
    EntriesOnDisk           : Word;
    TotalEntries            : Word;
    DirectorySize           : UInt32;
    DirectoryOffset         : UInt32;
    CommentLength           : Word;
  end;

  TAbFollower =                      {used to expand reduced files}
    packed record
      Size : Byte;                {size of follower set}
      FSet : array[0..31] of Byte; {follower set}
    end;
  PAbFollowerSets = ^TAbFollowerSets;
  TAbFollowerSets = array[0..255] of TAbFollower;


  PAbSfEntry = ^TAbSfEntry;
  TAbSfEntry =                       {entry in a Shannon-Fano tree}
    packed record
      case Byte of
        0 : (Code : Word; Value, BitLength : Byte);
        1 : (L : Integer);
    end;
  PAbSfTree = ^TAbSfTree;
  TAbSfTree =
    packed record                        {a Shannon-Fano tree}
      Entries : SmallInt;
      MaxLength : SmallInt;
      Entry : array[0..256] of TAbSfEntry;
    end;

  PInfoZipUnicodePathRec = ^TInfoZipUnicodePathRec;
  TInfoZipUnicodePathRec = packed record
    Version: Byte;
    NameCRC32: Integer;
    UnicodeName: array[0..0] of Byte;
  end;

  PXceedUnicodePathRec = ^TXceedUnicodePathRec;
  TXceedUnicodePathRec = packed record
    Signature: UInt32;
    Length: Integer;
    UnicodeName: array[0..0] of WideChar;
  end;

  PZip64LocalHeaderRec = ^TZip64LocalHeaderRec;
  TZip64LocalHeaderRec = packed record
    UncompressedSize: Int64;
    CompressedSize: Int64;
  end;

type
  TAbZipCompressionMethod =
    (cmStored, cmShrunk, cmReduced1, cmReduced2, cmReduced3,
     cmReduced4, cmImploded, cmTokenized, cmDeflated,
     cmEnhancedDeflated, cmDCLImploded, cmBzip2 = 12, cmLZMA = 14,
     cmIBMTerse = 18, cmLZ77, cmJPEG = 96, cmWavPack = 97, cmPPMd);

  TAbZipSupportedMethod =
    (smStored, smDeflated, smBestMethod);

  {ExternalFileAttributes compatibility;  aliases are Info-ZIP/PKZIP overlaps}
  TAbZipHostOS =
    (hosDOS, hosAmiga, hosVAX, hosUnix, hosVMCMS, hosAtari,
     hosOS2, hosMacintosh, hosZSystem, hosCPM, hosNTFS, hosTOPS20 = hosNTFS,
     hosMVS, hosWinNT = hosMVS, hosVSE, hosQDOS = hosVSE, hosRISC,
     hosVFAT, hosAltMVS, hosBeOS, hosTandem, hosOS400, hosTHEOS = hosOS400,
     hosDarwin, hosAtheOS = 30);

  {for method 6 - imploding}
  TAbZipDictionarySize =
    (dsInvalid, ds4K, ds8K);

  {for method 8 - deflating}
  TAbZipDeflationOption =
    (doInvalid, doNormal, doMaximum, doFast, doSuperFast );

type
  TAbNeedPasswordEvent = procedure(Sender : TObject; var NewPassword : string) of object;

const
  AbDefCompressionMethodToUse = smBestMethod;
  AbDefDeflationOption = doNormal;


type
  TAbZipDataDescriptor = class( TObject )
  protected {private}
    FCRC32            : Integer;
    FCompressedSize   : Int64;
    FUncompressedSize : Int64;
  public {methods}
    procedure SaveToStream( Stream : TStream );
  public {properties}
    property CRC32 : Integer
      read FCRC32 write FCRC32;
    property CompressedSize : Int64
      read FCompressedSize write FCompressedSize;
    property UncompressedSize : Int64
      read FUncompressedSize write FUncompressedSize;
  end;

type
{ TAbZipFileHeader interface =============================================== }
  {ancestor class for ZipLocalFileHeader and DirectoryFileHeader}
  TAbZipFileHeader = class(TObject)
  protected {private}
    FValidSignature : Integer;
    FSignature : Integer;
    FVersionNeededToExtract : Word;
    FGeneralPurposeBitFlag : Word;
    FCompressionMethod : Word;
    FLastModFileTime : Word;
    FLastModFileDate : Word;
    FCRC32 : Integer;
    FCompressedSize : UInt32;
    FUncompressedSize : UInt32;
    FFileName : string;
    FExtraField : TAbExtraField;
  protected {methods}
    function GetCompressionMethod : TAbZipCompressionMethod;
    function GetCompressionRatio : Double;
    function GetDataDescriptor : Boolean;
    function GetDeflationOption : TAbZipDeflationOption;
    function GetDictionarySize : TAbZipDictionarySize;
    function GetEncrypted : Boolean;
    function GetIsUTF8 : Boolean;
    function GetShannonFanoTreeCount : Byte;
    function GetValid : Boolean;
    procedure SetCompressionMethod( Value : TAbZipCompressionMethod );
    procedure SetIsUTF8( Value : Boolean );
  public {methods}
    constructor Create;
    destructor Destroy; override;
  public {properties}
    property Signature : Integer
      read FSignature write FSignature;
    property VersionNeededToExtract : Word
      read FVersionNeededToExtract write FVersionNeededToExtract;
    property GeneralPurposeBitFlag : Word
      read FGeneralPurposeBitFlag write FGeneralPurposeBitFlag;
    property CompressionMethod : TAbZipCompressionMethod
      read GetCompressionMethod write SetCompressionMethod;
    property LastModFileTime : Word
      read FLastModFileTime write FLastModFileTime;
    property LastModFileDate : Word
      read FLastModFileDate write FLastModFileDate;
    property CRC32 : Integer
      read FCRC32 write FCRC32;
    property CompressedSize : UInt32
      read FCompressedSize write FCompressedSize;
    property UncompressedSize : UInt32
      read FUncompressedSize write FUncompressedSize;
    property FileName : string read FFileName write FFileName;
    property ExtraField : TAbExtraField
      read FExtraField;

    property CompressionRatio : Double
      read GetCompressionRatio;
    property DeflationOption : TAbZipDeflationOption
      read GetDeflationOption;
    property DictionarySize : TAbZipDictionarySize
      read GetDictionarySize;
    property HasDataDescriptor : Boolean
      read GetDataDescriptor;
    property IsValid : Boolean
      read GetValid;
    property IsEncrypted : Boolean
      read GetEncrypted;
    property IsUTF8 : Boolean
      read GetIsUTF8 write SetIsUTF8;
    property ShannonFanoTreeCount : Byte
      read GetShannonFanoTreeCount;
  end;

{ TAbZipLocalFileHeader interface ========================================== }
  TAbZipLocalFileHeader = class( TAbZipFileHeader )
  public {methods}
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromStream( Stream : TStream );
    procedure SaveToStream( Stream : TStream );
  end;

{ TAbZipDirectoryFileHeader interface ====================================== }
  TAbZipDirectoryFileHeader = class( TAbZipFileHeader )
  protected {private}
    FRawFileName            : TBytes;
    FVersionMadeBy          : Word;
    FDiskNumberStart        : Word;
    FInternalFileAttributes : Word;
    FExternalFileAttributes : UInt32;
    FRelativeOffset         : UInt32;
    FFileComment            : string;
  public {methods}
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromStream( Stream : TStream );
    procedure SaveToStream( Stream : TStream );
  public {properties}
    property VersionMadeBy : Word
      read FVersionMadeBy write FVersionMadeBy;
    property DiskNumberStart : Word
      read FDiskNumberStart write FDiskNumberStart;
    property InternalFileAttributes : Word
      read FInternalFileAttributes write FInternalFileAttributes;
    property ExternalFileAttributes : UInt32
      read FExternalFileAttributes write FExternalFileAttributes;
    property RelativeOffset : UInt32
      read FRelativeOffset write FRelativeOffset;
    property FileComment : string
      read FFileComment write FFileComment;
  end;

{ TAbZipDirectoryFileFooter interface ====================================== }
  TAbZipDirectoryFileFooter = class( TObject )
  protected {private}
    FDiskNumber             : UInt32;
    FStartDiskNumber        : UInt32;
    FEntriesOnDisk          : Int64;
    FTotalEntries           : Int64;
    FDirectorySize          : Int64;
    FDirectoryOffset        : Int64;
    FZipfileComment         : string;
    function GetIsZip64: Boolean;
  public {methods}
    procedure LoadFromStream( Stream : TStream );
    procedure LoadZip64FromStream( Stream : TStream );
    procedure SaveToStream( Stream : TStream; aZip64TailOffset : Int64 = -1 );
  public {properties}
    property DiskNumber : UInt32
      read FDiskNumber write FDiskNumber;
    property EntriesOnDisk : Int64
      read FEntriesOnDisk write FEntriesOnDisk;
    property TotalEntries : Int64
      read FTotalEntries write FTotalEntries;
    property DirectorySize : Int64
      read FDirectorySize write FDirectorySize;
    property DirectoryOffset : Int64
      read FDirectoryOffset write FDirectoryOffset;
    property StartDiskNumber : UInt32
      read FStartDiskNumber write FStartDiskNumber;
    property ZipfileComment : string
      read FZipfileComment write FZipfileComment;
    property IsZip64: Boolean
      read GetIsZip64;
  end;

{ TAbZipItem interface ===================================================== }
  TAbZipItem = class( TAbArchiveItem )
  protected {private}
    FItemInfo : TAbZipDirectoryFileHeader;
    FDiskNumberStart : UInt32;
    FLFHExtraField : TAbExtraField;
    FRelativeOffset : Int64;

  protected {methods}
    function GetCompressionMethod : TAbZipCompressionMethod;
    function GetCompressionRatio : Double;
    function GetDeflationOption : TAbZipDeflationOption;
    function GetDictionarySize : TAbZipDictionarySize;
    function GetExtraField : TAbExtraField;
    function GetFileComment : string;
    function GetGeneralPurposeBitFlag : Word;
    function GetHostOS: TAbZipHostOS;
    function GetInternalFileAttributes : Word;
    function GetRawFileName : string;
    function GetShannonFanoTreeCount : Byte;
    function GetVersionMadeBy : Word;
    function GetVersionNeededToExtract : Word;
    procedure SaveCDHToStream( Stream : TStream );
    procedure SaveDDToStream( Stream : TStream );
    procedure SaveLFHToStream( Stream : TStream );
    procedure SetCompressionMethod( Value : TAbZipCompressionMethod );
    procedure SetDiskNumberStart( Value : UInt32 );
    procedure SetFileComment(const Value : string);
    procedure SetGeneralPurposeBitFlag( Value : Word );
    procedure SetHostOS( Value : TAbZipHostOS );
    procedure SetInternalFileAttributes( Value : Word );
    procedure SetRelativeOffset( Value : Int64 );
    procedure SetVersionMadeBy( Value : Word );
    procedure SetVersionNeededToExtract( Value : Word );
    procedure UpdateVersionNeededToExtract;
    procedure UpdateZip64ExtraHeader;

  protected {redefined property methods}
    function  GetCRC32 : Integer; override;
    function  GetExternalFileAttributes : UInt32; override;
    function  GetIsDirectory: Boolean; override;
    function  GetIsEncrypted : Boolean; override;
    function  GetLastModFileDate : Word; override;
    function  GetLastModFileTime : Word; override;
    function  GetNativeFileAttributes : Integer; override;
    procedure SetCompressedSize( const Value : Int64 ); override;
    procedure SetCRC32( const Value : Integer ); override;
    procedure SetExternalFileAttributes( Value : UInt32 ); override;
    procedure SetFileName(const Value : string ); override;
    procedure SetLastModFileDate(const Value : Word ); override;
    procedure SetLastModFileTime(const Value : Word ); override;
    procedure SetUncompressedSize( const Value : Int64 ); override;

  public {methods}
    constructor Create;
    destructor  Destroy; override;
    procedure LoadFromStream( Stream : TStream );

  public {properties}
    property CompressionMethod : TAbZipCompressionMethod
      read GetCompressionMethod
      write SetCompressionMethod;
    property CompressionRatio : Double
      read GetCompressionRatio;
    property DeflationOption : TAbZipDeflationOption
      read GetDeflationOption;
    property DictionarySize : TAbZipDictionarySize
      read GetDictionarySize;
    property DiskNumberStart : UInt32
      read FDiskNumberStart
      write SetDiskNumberStart;
    property ExtraField : TAbExtraField
      read GetExtraField;
    property FileComment : string
      read GetFileComment
      write SetFileComment;
    property HostOS: TAbZipHostOS
      read GetHostOS
      write SetHostOS;
    property InternalFileAttributes : Word
      read GetInternalFileAttributes
      write SetInternalFileAttributes;
    property GeneralPurposeBitFlag : Word
      read GetGeneralPurposeBitFlag
      write SetGeneralPurposeBitFlag;
    property LFHExtraField : TAbExtraField
      read FLFHExtraField;
    property RawFileName : string read GetRawFileName;
    property RelativeOffset : Int64
      read FRelativeOffset
      write SetRelativeOffset;
    property ShannonFanoTreeCount : Byte
      read GetShannonFanoTreeCount;
    property VersionMadeBy : Word
      read GetVersionMadeBy
      write SetVersionMadeBy;
    property VersionNeededToExtract : Word
      read GetVersionNeededToExtract
      write SetVersionNeededToExtract;
  end;

{ TAbZipArchive interface ================================================== }
  TAbZipArchive = class( TAbArchive )
  protected {private}
    FCompressionMethodToUse : TAbZipSupportedMethod;
    FDeflationOption        : TAbZipDeflationOption;
    FInfo                   : TAbZipDirectoryFileFooter;
    FIsExecutable           : Boolean;
    FPassword               : string;
    FPasswordRetries        : Byte;
    FStubSize               : UInt32;

    FExtractHelper          : TAbArchiveItemExtractEvent;
    FExtractToStreamHelper  : TAbArchiveItemExtractToStreamEvent;
    FTestHelper             : TAbArchiveItemTestEvent;
    FInsertHelper           : TAbArchiveItemInsertEvent;
    FInsertFromStreamHelper : TAbArchiveItemInsertFromStreamEvent;
    FOnNeedPassword         : TAbNeedPasswordEvent;
    FOnRequestLastDisk      : TAbRequestDiskEvent;
    FOnRequestNthDisk       : TAbRequestNthDiskEvent;
    FOnRequestBlankDisk     : TAbRequestDiskEvent;

  protected {methods}
    procedure DoExtractHelper(Index : Integer; const NewName : string);
    procedure DoExtractToStreamHelper(Index : Integer; aStream : TStream);
    procedure DoTestHelper(Index : Integer);
    procedure DoInsertHelper(Index : Integer; OutStream : TStream);
    procedure DoInsertFromStreamHelper(Index : Integer; OutStream : TStream);
    function GetItem( Index : Integer ) : TAbZipItem;
    function GetZipfileComment : string;
    procedure PutItem( Index : Integer; Value : TAbZipItem );
    procedure DoRequestDisk(const AMessage: string; var Abort : Boolean);
    procedure DoRequestLastDisk( var Abort : Boolean );
      virtual;
    procedure DoRequestNthDisk(Sender: TObject; DiskNumber : Byte; var Abort : Boolean );
      virtual;
    procedure DoRequestBlankDisk(Sender: TObject; var Abort : Boolean );
      virtual;
    procedure ExtractItemAt(Index : Integer; const UseName : string);
      override;
    procedure ExtractItemToStreamAt(Index : Integer; aStream : TStream);
      override;
    procedure TestItemAt(Index : Integer);
      override;
    function FixName(const Value : string ) : string;
      override;
    function GetSupportsEmptyFolders: Boolean;
      override;
    procedure LoadArchive;
      override;
    procedure SaveArchive;
      override;
    procedure SetZipfileComment(const Value: string);

  protected {properties}
    property IsExecutable : Boolean
      read FIsExecutable write FIsExecutable;

  public {protected}
    procedure DoRequestImage(Sender: TObject; ImageNumber: Integer;
      var ImageName: string; var Abort: Boolean);

  public {methods}
    constructor CreateFromStream( aStream : TStream; const ArchiveName : string );
      override;
    destructor Destroy;
      override;
    function CreateItem(const FileName : string): TAbArchiveItem;
      override;

  public {properties}
    property CompressionMethodToUse : TAbZipSupportedMethod
      read FCompressionMethodToUse
      write FCompressionMethodToUse;
    property DeflationOption : TAbZipDeflationOption
      read FDeflationOption
      write FDeflationOption;
    property ExtractHelper : TAbArchiveItemExtractEvent
      read FExtractHelper
      write FExtractHelper;
    property ExtractToStreamHelper : TAbArchiveItemExtractToStreamEvent
      read FExtractToStreamHelper
      write FExtractToStreamHelper;
    property TestHelper : TAbArchiveItemTestEvent
      read FTestHelper
      write FTestHelper;
    property InsertHelper : TAbArchiveItemInsertEvent
      read FInsertHelper
      write FInsertHelper;
    property InsertFromStreamHelper : TAbArchiveItemInsertFromStreamEvent
      read FInsertFromStreamHelper
      write FInsertFromStreamHelper;
    property Password : string read FPassword write FPassword;
    property PasswordRetries : Byte
      read FPasswordRetries
      write FPasswordRetries
      default AbDefPasswordRetries;
    property StubSize : UInt32
      read FStubSize;
    property ZipfileComment : string
      read GetZipfileComment
      write SetZipfileComment;

    property Items[Index : Integer] : TAbZipItem
      read GetItem
      write PutItem; default;

  public {events}
    property OnNeedPassword : TAbNeedPasswordEvent
      read FOnNeedPassword write FOnNeedPassword;
    property OnRequestLastDisk : TAbRequestDiskEvent
      read FOnRequestLastDisk write FOnRequestLastDisk;
    property OnRequestNthDisk : TAbRequestNthDiskEvent
      read FOnRequestNthDisk write FOnRequestNthDisk;
    property OnRequestBlankDisk : TAbRequestDiskEvent
      read FOnRequestBlankDisk write FOnRequestBlankDisk;
  end;

{============================================================================}
procedure MakeSelfExtracting( StubStream, ZipStream,
  SelfExtractingStream : TStream );
    {-takes an executable stub, and a .zip format stream, and creates
     a SelfExtracting stream.  The stub should create a TAbZipArchive
     passing itself as the file, using a read-only open mode.  It should
     then perform operations as needed - like ExtractFiles( '*.*' ).
     This routine updates the RelativeOffset of each item in the archive}

function FindCentralDirectoryTail(aStream : TStream) : Int64;

function VerifyZip(Strm : TStream) : TAbArchiveType;

function VerifySelfExtracting(Strm : TStream) : TAbArchiveType;

function ZipCompressionMethodToString(aMethod: TAbZipCompressionMethod): string;

implementation

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Math,
  Character,
  IOUtils,
  AbCharset,
  AbResString,
  AbExcept,
  AbVMStrm;

function VerifyZip(Strm : TStream) : TAbArchiveType;
{ determine if stream appears to be in PkZip format }
var
  Footer       : TAbZipEndOfCentralDirectoryRecord;
  Sig          : Integer;
  TailPosition : int64;
  StartPos     : int64;
begin
  StartPos := Strm.Position;
  Result := atUnknown;
  try
    Strm.Position := 0;
    Strm.Read(Sig, SizeOf(Sig));
    if (Sig = Ab_ZipSpannedSetSignature) then
      Result := atSpannedZip
    else begin
      { attempt to find Central Directory Tail }
      TailPosition := FindCentralDirectoryTail( Strm );
      if TailPosition <> -1 then begin
        { check Central Directory Signature }
        Strm.ReadBuffer(Footer, SizeOf(Footer));
        if Footer.Signature = Ab_ZipEndCentralDirectorySignature then
          if Footer.DiskNumber = 0 then
            Result := atZip
          else
            Result := atSpannedZip;
      end;
    end;
  except
    on EReadError do
      Result := atUnknown;
  end;
  Strm.Position := StartPos;
end;

function VerifySelfExtracting(Strm : TStream) : TAbArchiveType;
{ determine if stream appears to be an executable with appended PkZip data }
var
  FileSignature : Integer;
  StartPos      : Int64;
  IsWinExe, IsLinuxExe : Boolean;
begin
  StartPos := Strm.Position;
  { verify presence of executable stub }
  {check file type of stub stream}
  Strm.Position := 0;
  Strm.Read( FileSignature, sizeof( FileSignature ) );

  Result := atSelfExtZip;

  { detect executable type }
  IsLinuxExe := FileSignature = Ab_LinuxExeSignature;
  IsWinExe := LongRec(FileSignature).Lo = Ab_WindowsExeSignature;
  if not (IsWinExe or IsLinuxExe) then
    Result := atUnknown;

  { Check for central directory tail }
  if VerifyZip(Strm) <> atZip then
    Result := atUnknown;

  Strm.Position := StartPos;
end;
{============================================================================}
function ZipCompressionMethodToString(aMethod: TAbZipCompressionMethod): string;
begin
  case aMethod of
    cmStored:
      Result := AbZipStored;
    cmShrunk:
      Result := AbZipShrunk;
    cmReduced1..cmReduced4:
      Result := AbZipReduced;
    cmImploded:
      Result := AbZipImploded;
    cmTokenized:
      Result := AbZipTokenized;
    cmDeflated:
      Result := AbZipDeflated;
    cmEnhancedDeflated:
      Result := AbZipDeflate64;
    cmDCLImploded:
      Result := AbZipDCLImploded;
    cmBzip2:
      Result := AbZipBzip2;
    cmLZMA:
      Result := AbZipLZMA;
    cmIBMTerse:
      Result := AbZipIBMTerse;
    cmLZ77:
      Result := AbZipLZ77;
    cmJPEG:
      Result := AbZipJPEG;
    cmWavPack:
      Result := AbZipWavPack;
    cmPPMd:
      Result := AbZipPPMd;
    else
      Result := Format(AbZipUnknown, [Ord(aMethod)]);
  end;
end;
{============================================================================}
function FindCentralDirectoryTail(aStream : TStream) : Int64;
{ search end of aStream looking for ZIP Central Directory structure
  returns position in stream if found (otherwise returns -1),
  leaves stream positioned at start of structure or at original
  position if not found }
const
  StartBufSize = 512;
  MaxBufSize = 64 * 1024;
var
  StartPos  : Int64;
  TailRec   : TAbZipEndOfCentralDirectoryRecord;
  Buffer    : PByte;
  Offset    : Int64;
  TestPos   : PByte;
  Done      : boolean;
  BytesRead : Int64;
  BufSize   : Int64;
  CommentLen: integer;
begin
  {save the starting position}
  StartPos := aStream.Seek(0, soCurrent);

  {start off with the majority case: no zip file comment, so the
   central directory tail is the last thing in the stream and it's a
   fixed size and doesn't indicate a zip file comment}
  Result := aStream.Seek(-sizeof(TailRec), soEnd);
  if (Result >= 0) then begin
    aStream.ReadBuffer(TailRec, sizeof(TailRec));
    if (TailRec.Signature = Ab_ZipEndCentralDirectorySignature) and
       (TailRec.CommentLength = 0) then begin
      aStream.Seek(Result, soBeginning);
      Exit;
    end;
  end;

  {the zip stream seems to have a comment, or it has null padding
   bytes from some flaky program, or it's not even a zip formatted
   stream; we need to search for the tail signature}

  {get a buffer}
  BufSize := StartBufSize;
  GetMem(Buffer, BufSize);
  try

    {start out searching backwards}
    Offset := -BufSize;

    {while there is still data to search ...}
    Done := false;
    while not Done do begin

      {seek to the search position}
      Result := aStream.Seek(Offset, soEnd);
      if (Result <= 0) then begin
        Result := aStream.Seek(0, soBeginning);
        Done := true;
      end;

      {read a buffer full}
      BytesRead := aStream.Read(Buffer^, BufSize);

      if BytesRead < sizeOf(TailRec) then begin
        Result := -1;
        Exit;
      end;

      {search backwards through the buffer looking for the signature}
      TestPos := Buffer + BytesRead - sizeof(TailRec);
      while (TestPos <> Buffer) and
            (PInteger(TestPos)^ <> Ab_ZipEndCentralDirectorySignature) do
        dec(TestPos);

      {if we found the signature...}
      if (PInteger(TestPos)^ = Ab_ZipEndCentralDirectorySignature) then begin

        {get the tail record at this position}
        Move(TestPos^, TailRec, sizeof(TailRec));

        {if it's as valid a tail as we can check here...}
        CommentLen := -Offset - (TestPos - Buffer + sizeof(TailRec));
        if (TailRec.CommentLength <= CommentLen) then begin

          {calculate its position and exit}
          Result := Result + (TestPos - Buffer);
          aStream.Seek(Result, soBeginning);
          Exit;
        end;
      end;

      {otherwise move back one step, doubling the buffer}
      if (BufSize < MaxBufSize) then begin
        FreeMem(Buffer);
        BufSize := BufSize * 2;
        if BufSize > MaxBufSize then
          BufSize := MaxBufSize;
        GetMem(Buffer, BufSize);
      end;
      dec(Offset, BufSize - SizeOf(TailRec));
    end;

    {if we reach this point, the CD tail is not present}
    Result := -1;
    aStream.Seek(StartPos, soBeginning);
  finally
    FreeMem(Buffer);
  end;
end;
{============================================================================}
procedure MakeSelfExtracting( StubStream, ZipStream,
                              SelfExtractingStream : TStream );
  {-takes an executable stub, and a .zip format stream, and creates
   a SelfExtracting stream.  The stub should create a TAbZipArchive
   passing itself as the file, using a read-only open mode.  It should
   then perform operations as needed - like ExtractFiles( '*.*' ).
   This routine updates the RelativeOffset of each item in the archive}
var
  DirectoryStart : Int64;
  FileSignature : Integer;
  StubSize : UInt32;
  TailPosition : Int64;
  ZDFF : TAbZipDirectoryFileFooter;
  ZipItem : TAbZipItem;
  IsWinExe, IsLinuxExe : Boolean;
begin
  {check file type of stub stream}
  StubStream.Position := 0;
  StubStream.Read(FileSignature, SizeOf(FileSignature));

  {detect executable type }
  IsLinuxExe := FileSignature = Ab_LinuxExeSignature;
  IsWinExe := LongRec(FileSignature).Lo = Ab_WindowsExeSignature;

  if not (IsWinExe or IsLinuxExe) then
    raise EAbZipInvalidStub.Create;

  StubStream.Position := 0;
  StubSize := StubStream.Size;

  ZipStream.Position := 0;
  ZipStream.Read( FileSignature, sizeof( FileSignature ) );
  if LongRec(FileSignature).Lo <> Ab_GeneralZipSignature then
    raise EAbZipInvalid.Create;
  ZipStream.Position := 0;

  {copy the stub into the selfex stream}
  SelfExtractingStream.Position := 0;
  SelfExtractingStream.CopyFrom( StubStream, 0 );

  TailPosition := FindCentralDirectoryTail( ZipStream );
  if TailPosition = -1 then
    raise EAbZipInvalid.Create;
  {load the ZipDirectoryFileFooter}
  ZDFF := TAbZipDirectoryFileFooter.Create;
  try
    ZDFF.LoadFromStream( ZipStream );
    DirectoryStart := ZDFF.DirectoryOffset;
  finally
    ZDFF.Free;
  end;
  {copy everything up to the CDH into the SelfExtractingStream}
  ZipStream.Position := 0;
  SelfExtractingStream.CopyFrom( ZipStream, DirectoryStart );
  ZipStream.Position := DirectoryStart;
  repeat
    ZipItem := TAbZipItem.Create;
    try
      ZipItem.LoadFromStream( ZipStream );
      ZipItem.RelativeOffset := ZipItem.RelativeOffset + StubSize;
      {save the modified entry into the Self Extracting Stream}
      ZipItem.SaveCDHToStream( SelfExtractingStream );
    finally
      ZipItem.Free;
    end;
  until ZipStream.Position = TailPosition;

  {save the CDH Footer.}
  ZDFF := TAbZipDirectoryFileFooter.Create;
  try
    ZDFF.LoadFromStream( ZipStream );
    ZDFF.DirectoryOffset := ZDFF.DirectoryOffset + StubSize;
    ZDFF.SaveToStream( SelfExtractingStream );
  finally
    ZDFF.Free;
  end;
end;
{============================================================================}
{ TAbZipDataDescriptor implementation ====================================== }
procedure TAbZipDataDescriptor.SaveToStream( Stream : TStream );
begin
  Stream.Write( Ab_ZipDataDescriptorSignature, sizeof( Ab_ZipDataDescriptorSignature ) );
  Stream.Write( FCRC32, sizeof( FCRC32 ) );
  if (FCompressedSize >= $FFFFFFFF) or (FUncompressedSize >= $FFFFFFFF) then begin
    Stream.Write( FCompressedSize, sizeof( FCompressedSize ) );
    Stream.Write( FUncompressedSize, sizeof( FUncompressedSize ) );
  end
  else begin
    Stream.Write( FCompressedSize, sizeof( UInt32 ) );
    Stream.Write( FUncompressedSize, sizeof( UInt32 ) );
  end;
end;
{ -------------------------------------------------------------------------- }

{ TAbZipFileHeader implementation ========================================== }
constructor TAbZipFileHeader.Create;
begin
  inherited Create;
  FExtraField := TAbExtraField.Create;
  FValidSignature := $0;
end;
{ -------------------------------------------------------------------------- }
destructor TAbZipFileHeader.Destroy;
begin
  FreeAndNil(FExtraField);
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
function TAbZipFileHeader.GetCompressionMethod : TAbZipCompressionMethod;
begin
  Result := TAbZipCompressionMethod( FCompressionMethod );
end;
{ -------------------------------------------------------------------------- }
function TAbZipFileHeader.GetDataDescriptor : Boolean;
begin
  Result := ( CompressionMethod = cmDeflated ) and
            ( ( FGeneralPurposeBitFlag and AbHasDataDescriptorFlag ) <> 0 );
end;
{ -------------------------------------------------------------------------- }
function TAbZipFileHeader.GetCompressionRatio : Double;
var
  CompSize : Int64;
begin
  {adjust for encrypted headers - ensures we never get negative compression
  ratios for stored, encrypted files - no guarantees about negative
  compression ratios in other cases}
  if isEncrypted then
    CompSize := CompressedSize - 12
  else
    CompSize := CompressedSize;
  if UncompressedSize > 0 then
    Result := 100.0 * ( 1 - ( ( 1.0 * CompSize ) / UncompressedSize ) )
  else
    Result := 0.0;
end;
{ -------------------------------------------------------------------------- }
function TAbZipFileHeader.GetDeflationOption : TAbZipDeflationOption;
begin
  if CompressionMethod = cmDeflated then
    if ( ( FGeneralPurposeBitFlag and $02 ) <> 0 ) then
      if ( ( FGeneralPurposeBitFlag and $04 ) <> 0 ) then
        Result := doSuperFast
      else
        Result := doMaximum
    else
      if ( ( FGeneralPurposeBitFlag and $04 ) <> 0 ) then
        Result := doFast
      else
        Result := doNormal
  else
    Result := doInvalid;
end;
{ -------------------------------------------------------------------------- }
function TAbZipFileHeader.GetDictionarySize : TAbZipDictionarySize;
begin
  if CompressionMethod = cmImploded  then
    if ( ( FGeneralPurposeBitFlag and $02 ) <> 0 ) then
      Result := ds8K
    else
      Result := ds4K
  else
    Result := dsInvalid;
end;
{ -------------------------------------------------------------------------- }
function TAbZipFileHeader.GetEncrypted : Boolean;
begin
  {bit 0 of the GeneralPurposeBitFlag}
  Result := ( ( FGeneralPurposeBitFlag and AbFileIsEncryptedFlag ) <> 0 );
end;
{ -------------------------------------------------------------------------- }
function TAbZipFileHeader.GetIsUTF8 : Boolean;
begin
  Result := ( ( GeneralPurposeBitFlag and AbLanguageEncodingFlag ) <> 0 );
end;
{ -------------------------------------------------------------------------- }
function TAbZipFileHeader.GetShannonFanoTreeCount : Byte;
begin
  if CompressionMethod = cmImploded then
    if ( ( FGeneralPurposeBitFlag and $04 ) <> 0 ) then
      Result := 3
    else
      Result := 2
  else
    Result := 0;
end;
{ -------------------------------------------------------------------------- }
function TAbZipFileHeader.GetValid : Boolean;
begin
  Result := ( FValidSignature = FSignature );
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipFileHeader.SetCompressionMethod( Value :
                                               TAbZipCompressionMethod );
begin
  FCompressionMethod := Ord( Value );
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipFileHeader.SetIsUTF8( Value : Boolean );
begin
  if Value then
    GeneralPurposeBitFlag := GeneralPurposeBitFlag or AbLanguageEncodingFlag
  else
    GeneralPurposeBitFlag := GeneralPurposeBitFlag and not AbLanguageEncodingFlag;
end;
{ -------------------------------------------------------------------------- }

{ TAbZipLocalFileHeader implementation ===================================== }
constructor TAbZipLocalFileHeader.Create;
begin
  inherited Create;
  FValidSignature := Ab_ZipLocalFileHeaderSignature;
end;
{ -------------------------------------------------------------------------- }
destructor TAbZipLocalFileHeader.Destroy;
begin
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipLocalFileHeader.LoadFromStream( Stream : TStream );
var
  ExtraFieldLength, FileNameLength : Word;
  pBytes: TBytes;
begin
  Stream.Read( FSignature, sizeof( FSignature ) );
  Stream.Read( FVersionNeededToExtract, sizeof( FVersionNeededToExtract ) );
  Stream.Read( FGeneralPurposeBitFlag, sizeof( FGeneralPurposeBitFlag ) );
  Stream.Read( FCompressionMethod, sizeof( FCompressionMethod ) );
  Stream.Read( FLastModFileTime, sizeof( FLastModFileTime ) );
  Stream.Read( FLastModFileDate, sizeof( FLastModFileDate ) );
  Stream.Read( FCRC32, sizeof( FCRC32 ) );
  Stream.Read( FCompressedSize, sizeof( FCompressedSize ) );
  Stream.Read( FUncompressedSize, sizeof( FUncompressedSize ) );
  Stream.Read( FileNameLength, sizeof( FileNameLength ) );
  Stream.Read( ExtraFieldLength, sizeof( ExtraFieldLength ) );

  if FileNameLength > 0 then
  begin
    SetLength(pBytes, FileNameLength );
    Stream.Read(pBytes, Length(pBytes));
    case AbDetectCharSet(pBytes) of
      csASCII: FFileName := TEncoding.ASCII.GetString(pBytes);
      csANSI: FFileName := TEncoding.ANSI.GetString(pBytes);
      csUTF8: FFileName := TEncoding.UTF8.GetString(pBytes);
    end;
  end
  else
    FFileName := '';

  FExtraField.LoadFromStream( Stream, ExtraFieldLength );
  if not IsValid then
    raise EAbZipInvalid.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipLocalFileHeader.SaveToStream( Stream : TStream );
var
  ExtraFieldLength, FileNameLength: Word;
  pBytes: TBytes;
begin
  if IsUTF8 then
    pBytes := TEncoding.UTF8.GetBytes(FFileName)
  else
    pBytes := TEncoding.ANSI.GetBytes(FFileName);
  {write the valid signature from the constant}
  Stream.Write( FValidSignature, sizeof( FValidSignature ) );
  Stream.Write( FVersionNeededToExtract, sizeof( FVersionNeededToExtract ) );
  Stream.Write( FGeneralPurposeBitFlag, sizeof( FGeneralPurposeBitFlag ) );
  Stream.Write( FCompressionMethod, sizeof( FCompressionMethod ) );
  Stream.Write( FLastModFileTime, sizeof( FLastModFileTime ) );
  Stream.Write( FLastModFileDate, sizeof( FLastModFileDate ) );
  Stream.Write( FCRC32, sizeof( FCRC32 ) );
  Stream.Write( FCompressedSize, sizeof( FCompressedSize ) );
  Stream.Write( FUncompressedSize, sizeof( FUncompressedSize ) );
  FileNameLength := Word( Length( pBytes ) );
  Stream.Write( FileNameLength, sizeof( FileNameLength ) );
  ExtraFieldLength := Length(FExtraField.Buffer);
  Stream.Write( ExtraFieldLength, sizeof( ExtraFieldLength ) );
  if FileNameLength > 0 then
  begin
    Stream.Write(pBytes, Length(pBytes));
  end;
  if ExtraFieldLength > 0 then
    Stream.Write(FExtraField.Buffer[0], ExtraFieldLength);
end;
{ -------------------------------------------------------------------------- }

{ TAbZipDirectoryFileHeader implementation ================================= }
constructor TAbZipDirectoryFileHeader.Create;
begin
  inherited Create;
  FValidSignature := Ab_ZipCentralDirectoryFileHeaderSignature;
end;
{ -------------------------------------------------------------------------- }
destructor TAbZipDirectoryFileHeader.Destroy;
begin
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipDirectoryFileHeader.LoadFromStream( Stream : TStream );
var
  ExtraFieldLength, FileCommentLength, FileNameLength : Word;
  pBytes: TBytes;
begin
  with Stream do begin
    Read( FSignature, sizeof( FSignature ) );
    Read( FVersionMadeBy, sizeof( FVersionMadeBy ) );
    Read( FVersionNeededToExtract, sizeof( FVersionNeededToExtract ) );
    Read( FGeneralPurposeBitFlag, sizeof( FGeneralPurposeBitFlag ) );
    Read( FCompressionMethod, sizeof( FCompressionMethod ) );
    Read( FLastModFileTime, sizeof( FLastModFileTime ) );
    Read( FLastModFileDate, sizeof( FLastModFileDate ) );
    Read( FCRC32, sizeof( FCRC32 ) );
    Read( FCompressedSize, sizeof( FCompressedSize ) );
    Read( FUncompressedSize, sizeof( FUncompressedSize ) );
    Read( FileNameLength, sizeof( FileNameLength ) );
    Read( ExtraFieldLength, sizeof( ExtraFieldLength ) );
    Read( FileCommentLength, sizeof( FileCommentLength ) );
    Read( FDiskNumberStart, sizeof( FDiskNumberStart ) );
    Read( FInternalFileAttributes, sizeof( FInternalFileAttributes ) );
    Read( FExternalFileAttributes, sizeof( FExternalFileAttributes ) );
    Read( FRelativeOffset, sizeof( FRelativeOffset ) );

    if FileNameLength > 0 then
    begin
      SetLength(FRawFileName, FileNameLength);
      Read(FRawFileName, Length(FRawFileName));
      case AbDetectCharSet(FRawFileName) of
        csASCII: FFileName := TEncoding.ASCII.GetString(FRawFileName);
        csANSI: FFileName := TEncoding.ANSI.GetString(FRawFileName);
        csUTF8: FFileName := TEncoding.UTF8.GetString(FRawFileName);
      end;
    end
    else
    begin
      FFileName := '';
      FRawFileName := nil;
    end;

    FExtraField.LoadFromStream( Stream, ExtraFieldLength );

    if FileCommentLength > 0 then
    begin
      SetLength(pBytes, FileCommentLength);
      Read(pBytes, Length(pBytes));
      FFileComment := TEncoding.ANSI.GetString(pBytes);
    end
    else
      FFileComment := '';
  end;
  if not IsValid then
    raise EAbZipInvalid.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipDirectoryFileHeader.SaveToStream( Stream : TStream );
var
  ExtraFieldLength, FileCommentLength, FileNameLength : Word;
  pBytes: TBytes;
begin
  if IsUTF8 then
    pBytes := TEncoding.UTF8.GetBytes(FFileName)
  else
    pBytes := TEncoding.ANSI.GetBytes(FFileName);
  {write the valid signature from the constant}
  Stream.Write( FValidSignature, sizeof( FValidSignature ) );
  Stream.Write( FVersionMadeBy, sizeof( FVersionMadeBy ) );
  Stream.Write( FVersionNeededToExtract, sizeof( FVersionNeededToExtract ) );
  Stream.Write( FGeneralPurposeBitFlag, sizeof( FGeneralPurposeBitFlag ) );
  Stream.Write( FCompressionMethod, sizeof( FCompressionMethod ) );
  Stream.Write( FLastModFileTime, sizeof( FLastModFileTime ) );
  Stream.Write( FLastModFileDate, sizeof( FLastModFileDate ) );
  Stream.Write( FCRC32, sizeof( FCRC32 ) );
  Stream.Write( FCompressedSize, sizeof( FCompressedSize ) );
  Stream.Write( FUncompressedSize, sizeof( FUncompressedSize ) );
  FileNameLength := Word( Length( pBytes ) );
  Stream.Write( FileNameLength, sizeof( FileNameLength ) );
  ExtraFieldLength := Length(FExtraField.Buffer);
  Stream.Write( ExtraFieldLength, sizeof( ExtraFieldLength ) );
  FileCommentLength := Word( Length( FFileComment ) );
  Stream.Write( FileCommentLength, sizeof( FileCommentLength ) );
  Stream.Write( FDiskNumberStart, sizeof( FDiskNumberStart ) );
  Stream.Write( FInternalFileAttributes, sizeof( FInternalFileAttributes ) );
  Stream.Write( FExternalFileAttributes, sizeof( FExternalFileAttributes ) );
  Stream.Write( FRelativeOffset, sizeof( FRelativeOffset ) );
  if FileNameLength > 0 then
  begin
    Stream.Write(pBytes, Length(pBytes));
  end;
  if ExtraFieldLength > 0 then
    Stream.Write( FExtraField.Buffer[0], ExtraFieldLength );
  if FileCommentLength > 0 then
  begin
    pBytes := TEncoding.ANSI.GetBytes(FFileComment);
    Stream.Write(pBytes, Length(pBytes));
  end;
end;
{ -------------------------------------------------------------------------- }

{ TAbZipDirectoryFileFooter implementation ================================= }
function TAbZipDirectoryFileFooter.GetIsZip64: Boolean;
begin
  Result := (DiskNumber >= $FFFF) or
            (StartDiskNumber >= $FFFF) or
            (EntriesOnDisk >= $FFFF) or
            (TotalEntries >= $FFFF) or
            (DirectorySize >= $FFFFFFFF) or
            (DirectoryOffset >= $FFFFFFFF);
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipDirectoryFileFooter.LoadFromStream( Stream : TStream );
var
  Footer: TAbZipEndOfCentralDirectoryRecord;
  pBytes: TBytes;
begin
  Stream.ReadBuffer( Footer, SizeOf(Footer) );
  if Footer.Signature <> Ab_ZipEndCentralDirectorySignature then
    raise EAbZipInvalid.Create;
  FDiskNumber := Footer.DiskNumber;
  FStartDiskNumber := Footer.StartDiskNumber;
  FEntriesOnDisk := Footer.EntriesOnDisk;
  FTotalEntries := Footer.TotalEntries;
  FDirectorySize := Footer.DirectorySize;
  FDirectoryOffset := Footer.DirectoryOffset;

  if Footer.CommentLength > 0 then
  begin
    SetLength(pBytes, Footer.CommentLength);
    Stream.ReadBuffer(pBytes, Length(pBytes));
    FZipfileComment := TEncoding.ANSI.GetString(pBytes);
  end
  else
    FZipfileComment := '';
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipDirectoryFileFooter.LoadZip64FromStream( Stream : TStream );
  {load the ZIP64 end of central directory record.
   LoadFromStream() must be called first to load the standard record}
var
  Footer: TAbZip64EndOfCentralDirectoryRecord;
begin
  Stream.ReadBuffer( Footer, SizeOf(Footer) );
  if Footer.Signature <> Ab_Zip64EndCentralDirectorySignature then
    raise EAbZipInvalid.Create;
  if FDiskNumber = $FFFF then
    FDiskNumber := Footer.DiskNumber;
  if FStartDiskNumber = $FFFF then
    FStartDiskNumber := Footer.StartDiskNumber;
  if FEntriesOnDisk = $FFFF then
    FEntriesOnDisk := Footer.EntriesOnDisk;
  if FTotalEntries = $FFFF then
    FTotalEntries := Footer.TotalEntries;
  if FDirectorySize = $FFFFFFFF then
    FDirectorySize := Footer.DirectorySize;
  if FDirectoryOffset = $FFFFFFFF then
    FDirectoryOffset := Footer.DirectoryOffset;
  {RecordSize, VersionMadeBy, and VersionNeededToExtract are currently ignored}
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipDirectoryFileFooter.SaveToStream( Stream : TStream;
  aZip64TailOffset: Int64 = -1);
  {write end of central directory record, along with Zip64 records if necessary.
   aZip64TailOffset is the value to use for the Zip64 locator's directory
   offset, and is only necessary when writing to an intermediate stream}
var
  Footer: TAbZipEndOfCentralDirectoryRecord;
  Zip64Footer: TAbZip64EndOfCentralDirectoryRecord;
  Zip64Locator: TAbZip64EndOfCentralDirectoryLocator;
  pBytes: TBytes;
begin
  if IsZip64 then begin
    {setup Zip64 end of central directory record}
    Zip64Footer.Signature := Ab_Zip64EndCentralDirectorySignature;
    Zip64Footer.RecordSize := SizeOf(Zip64Footer) -
      SizeOf(Zip64Footer.Signature) - SizeOf(Zip64Footer.RecordSize);
    Zip64Footer.VersionMadeBy := 45;
    Zip64Footer.VersionNeededToExtract := 45;
    Zip64Footer.DiskNumber := DiskNumber;
    Zip64Footer.StartDiskNumber := StartDiskNumber;
    Zip64Footer.EntriesOnDisk := EntriesOnDisk;
    Zip64Footer.TotalEntries := TotalEntries;
    Zip64Footer.DirectorySize := DirectorySize;
    Zip64Footer.DirectoryOffset := DirectoryOffset;
    {setup Zip64 end of central directory locator}
    Zip64Locator.Signature := Ab_Zip64EndCentralDirectoryLocatorSignature;
    Zip64Locator.StartDiskNumber := DiskNumber;
    if aZip64TailOffset = -1 then
      Zip64Locator.RelativeOffset := Stream.Position
    else
      Zip64Locator.RelativeOffset := aZip64TailOffset;
    Zip64Locator.TotalDisks := DiskNumber + 1;
    {write Zip64 records}
    Stream.WriteBuffer(Zip64Footer, SizeOf(Zip64Footer));
    Stream.WriteBuffer(Zip64Locator, SizeOf(Zip64Locator));
  end;
  Footer.Signature := Ab_ZipEndCentralDirectorySignature;
  Footer.DiskNumber := Min(FDiskNumber, $FFFF);
  Footer.StartDiskNumber := Min(FStartDiskNumber, $FFFF);
  Footer.EntriesOnDisk := Min(FEntriesOnDisk, $FFFF);
  Footer.TotalEntries := Min(FTotalEntries, $FFFF);
  Footer.DirectorySize := Min(FDirectorySize, $FFFFFFFF);
  Footer.DirectoryOffset := Min(FDirectoryOffset, $FFFFFFFF);
  pBytes := TEncoding.ANSI.GetBytes(FZipfileComment);
  Footer.CommentLength := Length(pBytes);
  Stream.WriteBuffer( Footer, SizeOf(Footer) );
  if pBytes <> nil then
    Stream.Write(pBytes, Length(pBytes));
end;
{ -------------------------------------------------------------------------- }

{ TAbZipItem implementation ================================================ }
constructor TAbZipItem.Create;
begin
  inherited Create;
  FItemInfo := TAbZipDirectoryFileHeader.Create;
  FLFHExtraField := TAbExtraField.Create;
end;
{ -------------------------------------------------------------------------- }
destructor TAbZipItem.Destroy;
begin
  FLFHExtraField.Free;
  FItemInfo.Free;
  FItemInfo := nil;
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetCompressionMethod : TAbZipCompressionMethod;
begin
  Result := FItemInfo.CompressionMethod;
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetCompressionRatio : Double;
begin
  Result := FItemInfo.CompressionRatio;
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetCRC32 : Integer;
begin
  Result := FItemInfo.CRC32;
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetDeflationOption : TAbZipDeflationOption;
begin
  Result := FItemInfo.DeflationOption;
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetDictionarySize : TAbZipDictionarySize;
begin
  Result := FItemInfo.DictionarySize;
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetGeneralPurposeBitFlag : Word;
begin
  Result := FItemInfo.GeneralPurposeBitFlag;
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetHostOS: TAbZipHostOS;
begin
  Result := TAbZipHostOS(Hi(VersionMadeBy));
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetExternalFileAttributes : UInt32;
begin
  Result := FItemInfo.ExternalFileAttributes;
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetExtraField : TAbExtraField;
begin
  Result := FItemInfo.ExtraField;
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetFileComment : string;
begin
  Result := FItemInfo.FileComment;
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetInternalFileAttributes : Word;
begin
  Result := FItemInfo.InternalFileAttributes;
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetIsDirectory: Boolean;
begin
  Result := ((ExternalFileAttributes and faDirectory) <> 0) or
    ((FileName <> '') and Filename[Length(FFilename)].IsInArray(['\','/']));
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetIsEncrypted : Boolean;
begin
  Result := FItemInfo.IsEncrypted;
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetLastModFileDate : Word;
begin
  Result := FItemInfo.LastModFileDate;
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetLastModFileTime : Word;
begin
  Result := FItemInfo.LastModFileTime;
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetNativeFileAttributes : Integer;
begin
{$IFDEF MSWINDOWS}
  if (HostOS = hosUnix) or (ExternalFileAttributes > $1FFFF) then
    Result := AbUnix2DosFileAttributes(ExternalFileAttributes shr 16)
  else
    Result := Byte(ExternalFileAttributes);
{$ENDIF}
{$IFDEF POSIX}
  if HostOS in [hosDOS, hosNTFS, hosWinNT] then
    Result := AbDOS2UnixFileAttributes(ExternalFileAttributes)
  else
    Result := ExternalFileAttributes shr 16;
{$ENDIF}
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetRawFileName : string;
begin
  Result := FItemInfo.FileName;
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetShannonFanoTreeCount : Byte;
begin
  Result := FItemInfo.ShannonFanoTreeCount;
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetVersionMadeBy : Word;
begin
  Result := FItemInfo.VersionMadeBy;
end;
{ -------------------------------------------------------------------------- }
function TAbZipItem.GetVersionNeededToExtract : Word;
begin
  Result := FItemInfo.VersionNeededToExtract;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.LoadFromStream( Stream : TStream );
var
  FieldSize: Word;
  FieldStream: TStream;
  InfoZipField: PInfoZipUnicodePathRec;
  UnicodeName: UnicodeString;
  UTF8Name: string;
  XceedField: PXceedUnicodePathRec;
  pBuffer: TBytes;
begin
  FItemInfo.LoadFromStream( Stream );

  { decode filename (ANSI/OEM/UTF-8) }
  if FItemInfo.IsUTF8 or (AbDetectCharSet(FItemInfo.FRawFileName) = csUTF8) then
    FFileName := TEncoding.UTF8.GetString(FItemInfo.FRawFileName)
  else if FItemInfo.ExtraField.Get(Ab_InfoZipUnicodePathSubfieldID, Pointer(InfoZipField), FieldSize) and
     (FieldSize > SizeOf(TInfoZipUnicodePathRec)) and
     (InfoZipField.Version = 1) and
     (InfoZipField.NameCRC32 = AbCRC32Of(TEncoding.ANSI.GetBytes(FItemInfo.FileName))) then
  begin
    SetLength(pBuffer, FieldSize - SizeOf(TInfoZipUnicodePathRec) + 1);
    Move(InfoZipField.UnicodeName, pBuffer[0], Length(pBuffer));
    UTF8Name := TEncoding.UTF8.GetString(pBuffer);
    FFileName := UTF8Name;
  end
  else if FItemInfo.ExtraField.Get(Ab_XceedUnicodePathSubfieldID, Pointer(XceedField), FieldSize) and
     (FieldSize > SizeOf(TXceedUnicodePathRec)) and
     (XceedField.Signature = Ab_XceedUnicodePathSignature) and
     (XceedField.Length * SizeOf(WideChar) = FieldSize - SizeOf(TXceedUnicodePathRec) + SizeOf(WideChar)) then begin
    SetString(UnicodeName, XceedField.UnicodeName, XceedField.Length);
    FFileName := string(UnicodeName);
  end
  {$IFDEF MSWINDOWS}
  else if (GetACP <> GetOEMCP) and ((HostOS = hosDOS) or AbIsOEM(TEncoding.ANSI.GetBytes(FItemInfo.FileName))) then begin
    SetLength(FFileName, Length(FItemInfo.FileName));
    pBuffer := TEncoding.ANSI.GetBytes(FItemInfo.FileName);
    if pBuffer <> nil then
      OemToCharBuff(PAnsiChar(@(pBuffer[0])), PChar(FFileName), Length(FFileName));
  end
  {$ENDIF}
  else
    FFileName := FItemInfo.FileName;

  { read ZIP64 extended header }
  FUncompressedSize := FItemInfo.UncompressedSize;
  FCompressedSize := FItemInfo.CompressedSize;
  FRelativeOffset := FItemInfo.RelativeOffset;
  FDiskNumberStart := FItemInfo.DiskNumberStart;
  if FItemInfo.ExtraField.GetStream(Ab_Zip64SubfieldID, FieldStream) then
    try
      if FItemInfo.UncompressedSize = $FFFFFFFF then
        FieldStream.ReadBuffer(FUncompressedSize, SizeOf(Int64));
      if FItemInfo.CompressedSize = $FFFFFFFF then
        FieldStream.ReadBuffer(FCompressedSize, SizeOf(Int64));
      if FItemInfo.RelativeOffset = $FFFFFFFF then
        FieldStream.ReadBuffer(FRelativeOffset, SizeOf(Int64));
      if FItemInfo.DiskNumberStart = $FFFF then
        FieldStream.ReadBuffer(FDiskNumberStart, SizeOf(UInt32));
    finally
      FieldStream.Free;
    end;

  LastModFileTime := FItemInfo.LastModFileTime;
  LastModFileDate := FItemInfo.LastModFileDate;
  FDiskFileName := FileName;
  AbUnfixName( FDiskFileName );
  Action := aaNone;
  Tagged := False;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SaveLFHToStream( Stream : TStream );
var
  LFH : TAbZipLocalFileHeader;
  Zip64Field: TZip64LocalHeaderRec;
begin
  LFH := TAbZipLocalFileHeader.Create;
  try
    LFH.VersionNeededToExtract := VersionNeededToExtract;
    LFH.GeneralPurposeBitFlag := GeneralPurposeBitFlag;
    LFH.CompressionMethod := CompressionMethod;
    LFH.LastModFileTime := LastModFileTime;
    LFH.LastModFileDate := LastModFileDate;
    LFH.CRC32 := CRC32;
    LFH.FileName := RawFileName;
    LFH.ExtraField.Assign(LFHExtraField);
    LFH.ExtraField.CloneFrom(ExtraField, Ab_InfoZipUnicodePathSubfieldID);
    LFH.ExtraField.CloneFrom(ExtraField, Ab_XceedUnicodePathSubfieldID);
    { setup sizes;  unlike the central directory header, the ZIP64 local header
      needs to store both compressed and uncompressed sizes if either needs it }
    if (CompressedSize >= $FFFFFFFF) or (UncompressedSize >= $FFFFFFFF) then begin
      LFH.UncompressedSize := $FFFFFFFF;
      LFH.CompressedSize := $FFFFFFFF;
      Zip64Field.UncompressedSize := UncompressedSize;
      Zip64Field.CompressedSize := CompressedSize;
      LFH.ExtraField.Put(Ab_Zip64SubfieldID, Zip64Field, SizeOf(Zip64Field));
    end
    else begin
      LFH.UncompressedSize := UncompressedSize;
      LFH.CompressedSize := CompressedSize;
      LFH.ExtraField.Delete(Ab_Zip64SubfieldID);
    end;
    LFH.SaveToStream( Stream );
  finally
    LFH.Free;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SaveCDHToStream( Stream : TStream );
  {-Save a ZipCentralDirectorHeader entry to Stream}
begin
  FItemInfo.SaveToStream( Stream );
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SaveDDToStream( Stream : TStream );
var
  DD : TAbZipDataDescriptor;
begin
  DD := TAbZipDataDescriptor.Create;
  try
    DD.CRC32 := CRC32;
    DD.CompressedSize := CompressedSize;
    DD.UncompressedSize := UncompressedSize;
    DD.SaveToStream( Stream );
  finally
    DD.Free;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetCompressedSize( const Value : Int64 );
begin
  FCompressedSize := Value;
  FItemInfo.CompressedSize := Min(Value, $FFFFFFFF);
  UpdateZip64ExtraHeader;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetCompressionMethod( Value : TAbZipCompressionMethod );
begin
  FItemInfo.CompressionMethod := Value;
  UpdateVersionNeededToExtract;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetCRC32( const Value : Integer );
begin
  FItemInfo.CRC32 := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetDiskNumberStart( Value : UInt32 );
begin
  FDiskNumberStart := Value;
  FItemInfo.DiskNumberStart := Min(Value, $FFFF);
  UpdateZip64ExtraHeader;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetExternalFileAttributes( Value : UInt32 );
begin
  FItemInfo.ExternalFileAttributes := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetFileComment(const Value : string );
begin
  FItemInfo.FileComment := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetFileName(const Value : string );
var
  FieldSize : Word;
  I : Integer;
  InfoZipField : PInfoZipUnicodePathRec;
  UseExtraField: Boolean;
  pBytes: TBytes;
  {$IFDEF MSWINDOWS}
  AnsiName: AnsiString;
  {$ENDIF}
begin
  inherited SetFileName(Value);

  {$IFDEF MSWINDOWS}
  FItemInfo.IsUTF8 := False;
  HostOS := hosDOS;
  if AbTryEncode(Value, CP_OEMCP, False, AnsiName) then
    {no-op}
  else if (GetACP <> GetOEMCP) and AbTryEncode(Value, CP_ACP, False, AnsiName) then
    HostOS := hosWinNT
  else if AbTryEncode(Value, CP_OEMCP, True, AnsiName) then
    {no-op}
  else if (GetACP <> GetOEMCP) and AbTryEncode(Value, CP_ACP, True, AnsiName) then
    HostOS := hosWinNT
  else
    FItemInfo.IsUTF8 := True;
  {$ENDIF}
  {$IFDEF POSIX}
  FItemInfo.IsUTF8 := AbSysCharSetIsUTF8;
  {$ENDIF}
  FItemInfo.FileName := Value;

  UseExtraField := False;
  if not FItemInfo.IsUTF8 then
    for i := 1 to Length(Value) do
    begin
      if Ord(Value[i]) > 127 then
      begin
        UseExtraField := True;
        Break;
      end;
    end;

  if UseExtraField then
  begin
    pBytes := TEncoding.UTF8.GetBytes(Value);
    FieldSize := SizeOf(TInfoZipUnicodePathRec) + Length(pBytes) - 1;
    GetMem(InfoZipField, FieldSize);
    try
      InfoZipField.Version := 1;
      InfoZipField.NameCRC32 := AbCRC32Of(TEncoding.ANSI.GetBytes(FItemInfo.FileName));
      Move(pBytes[0], InfoZipField.UnicodeName, Length(pBytes));
      FItemInfo.ExtraField.Put(Ab_InfoZipUnicodePathSubfieldID, InfoZipField^, FieldSize);
    finally
      FreeMem(InfoZipField);
    end;
  end
  else
    FItemInfo.ExtraField.Delete(Ab_InfoZipUnicodePathSubfieldID);
  FItemInfo.ExtraField.Delete(Ab_XceedUnicodePathSubfieldID);
end;
{$IFDEF OPTIMIZATIONS_ON}{$O+}{$ENDIF}
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetGeneralPurposeBitFlag( Value : Word );
begin
  FItemInfo.GeneralPurposeBitFlag := Value;
  UpdateVersionNeededToExtract;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetHostOS( Value : TAbZipHostOS );
begin
  FItemInfo.VersionMadeBy := Low(FItemInfo.VersionMadeBy) or
    Word(Ord(Value)) shl 8;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetInternalFileAttributes( Value : Word );
begin
  FItemInfo.InternalFileAttributes := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetLastModFileDate( const Value : Word );
begin
  FItemInfo.LastModFileDate := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetLastModFileTime( const Value : Word );
begin
  FItemInfo.LastModFileTime := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetRelativeOffset( Value : Int64 );
begin
  FRelativeOffset := Value;
  FItemInfo.RelativeOffset := Min(Value, $FFFFFFFF);
  UpdateZip64ExtraHeader;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetUncompressedSize( const Value : Int64 );
begin
  FUncompressedSize := Value;
  FItemInfo.UncompressedSize:= Min(Value, $FFFFFFFF);
  UpdateZip64ExtraHeader;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetVersionMadeBy( Value : Word );
begin
  FItemInfo.VersionMadeBy := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.SetVersionNeededToExtract( Value : Word );
begin
  FItemInfo.VersionNeededToExtract := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.UpdateVersionNeededToExtract;
  {calculates VersionNeededToExtract and VersionMadeBy based on used features}
begin
  {According to AppNote.txt zipx compression methods should set the Version
   Needed To Extract to the AppNote version the method was introduced in (e.g.,
   6.3 for PPMd).  Most utilities just set it to 2.0 and rely on the extractor
   detecting unsupported compression methods, since it's easier to add support
   for decompression methods without implementing the entire newer spec. }
  if ExtraField.Has(Ab_Zip64SubfieldID) then
    VersionNeededToExtract := 45
  else if IsDirectory or IsEncrypted or not (CompressionMethod in [cmStored..cmImploded]) then
    VersionNeededToExtract := 20
  else
    VersionNeededToExtract := 10;
  VersionMadeBy := (VersionMadeBy and $FF00) + Max(20, VersionNeededToExtract);
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipItem.UpdateZip64ExtraHeader;
var
  Changed: Boolean;
  FieldStream: TMemoryStream;
begin
  FieldStream := TMemoryStream.Create;
  try
    if UncompressedSize >= $FFFFFFFF then
      FieldStream.WriteBuffer(FUncompressedSize, SizeOf(Int64));
    if CompressedSize >= $FFFFFFFF then
      FieldStream.WriteBuffer(FCompressedSize, SizeOf(Int64));
    if RelativeOffset >= $FFFFFFFF then
      FieldStream.WriteBuffer(FRelativeOffset, SizeOf(Int64));
    if DiskNumberStart >= $FFFF then
      FieldStream.WriteBuffer(FDiskNumberStart, SizeOf(UInt32));
    Changed := (FieldStream.Size > 0) <> ExtraField.Has(Ab_Zip64SubfieldID);
    if FieldStream.Size > 0 then
      ExtraField.Put(Ab_Zip64SubfieldID, FieldStream.Memory^, FieldStream.Size)
    else
      ExtraField.Delete(Ab_Zip64SubfieldID);
    if Changed then
      UpdateVersionNeededToExtract;
  finally
    FieldStream.Free;
  end;
end;
{ -------------------------------------------------------------------------- }


{ TAbZipArchive implementation ============================================= }
constructor TAbZipArchive.CreateFromStream( aStream : TStream;
                                      const ArchiveName : string );
begin
  inherited CreateFromStream( aStream, ArchiveName );
  FCompressionMethodToUse := smBestMethod;
  FInfo := TAbZipDirectoryFileFooter.Create;
  StoreOptions := StoreOptions + [soStripDrive];
  FDeflationOption := doNormal;
  FPasswordRetries := AbDefPasswordRetries;
  FTempDir := '';
  SpanningThreshold := AbDefZipSpanningThreshold;
end;
{ -------------------------------------------------------------------------- }
destructor TAbZipArchive.Destroy;
begin
  FInfo.Free;
  FInfo := nil;
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
function TAbZipArchive.CreateItem( const FileName : string ): TAbArchiveItem;
var
  FileSpec : string;
begin
  FileSpec := FileName;
  Result := TAbZipItem.Create;
  with TAbZipItem( Result ) do begin
    CompressionMethod := cmDeflated;
    GeneralPurposeBitFlag := 0;
    CompressedSize := 0;
    CRC32 := 0;
    DiskFileName := ExpandFileName(FileSpec);
    FileName := FixName(FileSpec);
    RelativeOffset := 0;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.DoExtractHelper(Index : Integer; const NewName : string);
begin
  if Assigned(FExtractHelper) then
    FExtractHelper(Self, ItemList[Index], NewName)
  else
    raise EAbZipNoExtraction.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.DoExtractToStreamHelper(Index : Integer;
                                                aStream : TStream);
begin
  if Assigned(FExtractToStreamHelper) then
    FExtractToStreamHelper(Self, ItemList[Index], aStream)
  else
    raise EAbZipNoExtraction.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.DoTestHelper(Index : Integer);
begin
  if Assigned(FTestHelper) then
    FTestHelper(Self, ItemList[Index])
  else
    raise EAbZipNoExtraction.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.DoInsertHelper(Index : Integer; OutStream : TStream);
begin
  if Assigned(FInsertHelper) then
    FInsertHelper(Self, ItemList[Index], OutStream)
  else
    raise EAbZipNoInsertion.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.DoInsertFromStreamHelper(Index : Integer;
  OutStream : TStream);
begin
  if Assigned(FInsertFromStreamHelper) then
    FInsertFromStreamHelper(Self, ItemList[Index], OutStream, InStream)
  else
    raise EAbZipNoInsertion.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.DoRequestDisk(const AMessage: string; var Abort : Boolean);
begin
{$IFDEF MSWINDOWS}
  Abort := Windows.MessageBox( 0, PChar(AMessage), PChar(AbDiskRequestS),
    MB_TASKMODAL or MB_OKCANCEL ) = IDCANCEL;
{$ELSE}
  Abort := True;
{$ENDIF}
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.DoRequestLastDisk( var Abort : Boolean );
begin
  Abort := False;
  if Assigned( FOnRequestLastDisk ) then
    FOnRequestLastDisk( Self, Abort )
  else
    DoRequestDisk( AbLastDiskRequestS, Abort );
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.DoRequestNthDisk( Sender: TObject;
                                          DiskNumber : Byte;
                                          var Abort : Boolean );
begin
  Abort := False;
  if Assigned( FOnRequestNthDisk ) then
    FOnRequestNthDisk( Self, DiskNumber, Abort )
  else
    DoRequestDisk( Format(AbDiskNumRequestS, [DiskNumber]), Abort );
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.DoRequestBlankDisk(Sender: TObject; var Abort : Boolean );
begin
  Abort := False;
  FSpanned := True;

  if Assigned( FOnRequestBlankDisk ) then
    FOnRequestBlankDisk( Self, Abort )
  else
    DoRequestDisk( AbBlankDiskS, Abort );
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.DoRequestImage(Sender: TObject; ImageNumber : Integer;
  var ImageName : string ; var Abort : Boolean);
begin
  if Assigned(FOnRequestImage) then
    FOnRequestImage(Self, ImageNumber, ImageName, Abort);
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.ExtractItemAt(Index : Integer; const UseName : string);
begin
  DoExtractHelper(Index, UseName);
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.ExtractItemToStreamAt(Index : Integer;
                                              aStream : TStream);
begin
  DoExtractToStreamHelper(Index, aStream);
end;
{ -------------------------------------------------------------------------- }
function TAbZipArchive.FixName(const Value : string ) : string;
  {-changes backslashes to forward slashes}
var
  i : SmallInt;
  lValue : string;
begin
  lValue := Value;
  {$IFDEF MSWINDOWS}
  if DOSMode then begin
    {Add the base directory to the filename before converting }
    {the file spec to the short filespec format. }
    if BaseDirectory <> '' then begin
      {Does the filename contain a drive or a leading backslash? }
      if not ((Pos(':', lValue) = 2) or (Pos(AbPathDelim, lValue) = 1)) then
        {If not, add the BaseDirectory to the filename.}
        lValue := TPath.Combine(BaseDirectory, lValue);
    end;
    lValue := AbGetShortFileSpec( lValue );
  end;
  {$ENDIF MSWINDOWS}

  {Zip files Always strip the drive path}
  StoreOptions := StoreOptions + [soStripDrive];

  {strip drive stuff}
  if soStripDrive in StoreOptions then
    AbStripDrive( lValue );

  {check for a leading backslash}
  if (Length(lValue) > 1) and (lValue.Chars[0] = AbPathDelim) then
    lValue.Remove(0, 1);

  if soStripPath in StoreOptions then begin
    lValue := ExtractFileName( lValue );
  end;

  if soRemoveDots in StoreOptions then
    AbStripDots( lValue );

  for i := 1 to Length( lValue ) do
    if lValue[i] = '\' then
      lValue[i] := '/';
  Result := lValue;
end;
{ -------------------------------------------------------------------------- }
function TAbZipArchive.GetItem( Index : Integer ) : TAbZipItem;
begin
  Result := TAbZipItem(FItemList.Items[Index]);
end;
{ -------------------------------------------------------------------------- }
function TAbZipArchive.GetSupportsEmptyFolders: Boolean;
begin
  Result := True;
end;
{ -------------------------------------------------------------------------- }
function TAbZipArchive.GetZipfileComment : string;
begin
  Result := FInfo.ZipfileComment;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.LoadArchive;
var
  Abort : Boolean;
  TailPosition : int64;
  Item : TAbZipItem;
  Progress : Byte;
  FileSignature : Integer;
  Zip64Locator : TAbZip64EndOfCentralDirectoryLocator;
begin
  Abort := False;
  if FStream.Size = 0 then
    Exit;

  {Get signature info}
  FStream.Position := 0;
  FStream.Read( FileSignature, sizeof( FileSignature ) );

  {Get Executable Type;  allow non-native stubs}
  IsExecutable :=
    (LongRec(FileSignature).Lo = Ab_WindowsExeSignature) or
    (FileSignature = Ab_LinuxExeSignature);

  { try to locate central directory tail }
  TailPosition := FindCentralDirectoryTail( FStream );
  if (TailPosition = -1) and (FileSignature = Ab_ZipSpannedSetSignature) and
     FOwnsStream and AbDriveIsRemovable(ArchiveName) then begin
    while TailPosition = -1 do begin
      FreeAndNil(FStream);
      DoRequestLastDisk(Abort);
      if Abort then begin
        FStatus := asInvalid; //TODO: Status updates are extremely inconsistent
        raise EAbUserAbort.Create;
      end;
      FStream := TFileStream.Create( ArchiveName, Mode );
      TailPosition := FindCentralDirectoryTail( FStream );
    end;
  end;

  if TailPosition = -1 then begin
    FStatus := asInvalid;
    raise EAbZipInvalid.Create;
  end;

  { load the ZipDirectoryFileFooter }
  FInfo.LoadFromStream(FStream);

  { find Zip64 end of central directory locator; it will usually occur
    immediately before the standard end of central directory record.
    the actual Zip64 end of central directory may be on another disk }
  if FInfo.IsZip64 then begin
    Dec(TailPosition, SizeOf(Zip64Locator));
    repeat
      if TailPosition < 0 then
        raise EAbZipInvalid.Create;
      FStream.Position := TailPosition;
      FStream.ReadBuffer(Zip64Locator, SizeOf(Zip64Locator));
      Dec(TailPosition);
    until Zip64Locator.Signature = Ab_Zip64EndCentralDirectoryLocatorSignature;
    { update current image number }
    FInfo.DiskNumber := Zip64Locator.TotalDisks - 1;
  end;

  { setup spanning support and move to the start of the central directory }
  FSpanned := FInfo.DiskNumber > 0;

  if FSpanned then begin
    if FOwnsStream then begin
      FStream := TAbSpanReadStream.Create( ArchiveName, FInfo.DiskNumber, FStream );
      TAbSpanReadStream(FStream).OnRequestImage := DoRequestImage;
      TAbSpanReadStream(FStream).OnRequestNthDisk := DoRequestNthDisk;
      if FInfo.IsZip64 then begin
        TAbSpanReadStream(FStream).SeekImage(Zip64Locator.StartDiskNumber,
          Zip64Locator.RelativeOffset);
        FInfo.LoadZip64FromStream(FStream);
      end;
      TAbSpanReadStream(FStream).SeekImage(FInfo.StartDiskNumber, FInfo.DirectoryOffset);
    end
    else
      raise EAbZipBadSpanStream.Create;
  end
  else begin
    if FInfo.IsZip64 then begin
      FStream.Position := Zip64Locator.RelativeOffset;
      FInfo.LoadZip64FromStream(FStream);
    end;
    FStream.Position := FInfo.DirectoryOffset;
  end;

  { build Items list from central directory records }
  FStubSize := High(UInt32);
  while Count < FInfo.TotalEntries do begin
    { create new Item }
    Item := TAbZipItem.Create;
    try
      Item.LoadFromStream(FStream);
      Item.Action := aaNone;
      FItemList.Add(Item);
    except
      Item.Free;
      raise;
    end;

    if IsExecutable and (Item.DiskNumberStart = 0) and
       (Item.RelativeOffset < FStubSize) then
      FStubSize := Item.RelativeOffset;

    Progress := (Count * 100) div FInfo.TotalEntries;
    DoArchiveProgress( Progress, Abort );
    if Abort then begin
      FStatus := asInvalid;
      raise EAbUserAbort.Create;
    end;
  end;

  DoArchiveProgress(100, Abort);
  FIsDirty := False;
end;

{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.PutItem( Index : Integer; Value : TAbZipItem );
begin
  FItemList.Items[Index] := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.SaveArchive;
  {builds a new archive and copies it to FStream}
var
  Abort              : Boolean;
  MemStream          : TMemoryStream;
  HasDataDescriptor  : Boolean;
  i                  : UInt32;
  LFH                : TAbZipLocalFileHeader;
  NewStream          : TStream;
  WorkingStream      : TAbVirtualMemoryStream;
  CurrItem           : TAbZipItem;
  Progress           : Byte;
begin
  if Count = 0 then
    Exit;

  {shouldn't be trying to overwrite an existing spanned archive}
  if Spanned then begin
    for i := 0 to Pred(Count) do
      if ItemList[i].Action <> aaFailed then
        ItemList[i].Action := aaNone;
    FIsDirty := False;
    raise EAbZipSpanOverwrite.Create;
  end;

  {init new zip archive stream
   can span only new archives, if SpanningThreshold > 0 or removable drive
   spanning writes to original location, rather than writing to a temp stream first}
  if FOwnsStream and (FStream.Size = 0) and not IsExecutable and
     ((SpanningThreshold > 0) or  AbDriveIsRemovable(ArchiveName)) then begin
    NewStream := TAbSpanWriteStream.Create(ArchiveName, FStream, SpanningThreshold);
    FStream := nil;
    TAbSpanWriteStream(NewStream).OnRequestBlankDisk := DoRequestBlankDisk;
    TAbSpanWriteStream(NewStream).OnRequestImage := DoRequestImage;
  end
  else begin
    NewStream := TAbVirtualMemoryStream.Create;
    TAbVirtualMemoryStream(NewStream).SwapFileDirectory := FTempDir;
  end;

  try {NewStream}
    {copy the executable stub over to the output}
    if IsExecutable then
      NewStream.CopyFrom( FStream, StubSize )
    {assume spanned for spanning stream}
    else if NewStream is TAbSpanWriteStream then
      NewStream.Write(Ab_ZipSpannedSetSignature,
        SizeOf(Ab_ZipSpannedSetSignature));

    {build new zip archive from existing archive}
    for i := 0 to pred( Count ) do begin
      CurrItem := (ItemList[i] as TAbZipItem);
      FCurrentItem := ItemList[i];

      case CurrItem.Action of
        aaNone, aaMove: begin
          {just copy the file to new stream}
          Assert(not (NewStream is TAbSpanWriteStream));
          FStream.Position := CurrItem.RelativeOffset;
          CurrItem.DiskNumberStart := 0;
          CurrItem.RelativeOffset := NewStream.Position;
          {toss old local file header}
          LFH := TAbZipLocalFileHeader.Create;
          try {LFH}
            LFH.LoadFromStream( FStream );
            if CurrItem.LFHExtraField.Count = 0 then
              CurrItem.LFHExtraField.Assign(LFH.ExtraField);
          finally {LFH}
            LFH.Free;
          end; {LFH}
          {write out new local file header and append compressed data}

          CurrItem.SaveLFHToStream( NewStream );
          if (CurrItem.CompressedSize > 0) then
            NewStream.CopyFrom(FStream, CurrItem.CompressedSize);
        end;

        aaDelete: begin
          {doing nothing omits file from new stream}
        end;

        aaAdd, aaFreshen, aaReplace, aaStreamAdd: begin
          {compress the file and add it to new stream}
          try
            WorkingStream := TAbVirtualMemoryStream.Create;
            try {WorkingStream}
              WorkingStream.SwapFileDirectory := FTempDir;
              {compress the file}
              if (CurrItem.Action = aaStreamAdd) then
                DoInsertFromStreamHelper(i, WorkingStream)
              else
                DoInsertHelper(i, WorkingStream);
              {write local header}
              if NewStream is TAbSpanWriteStream then begin
                MemStream := TMemoryStream.Create;
                try
                  CurrItem.SaveLFHToStream(MemStream);
                  TAbSpanWriteStream(NewStream).WriteUnspanned(
                    MemStream.Memory^, MemStream.Size);
                  {calculate positions after the write in case it triggered a new span}
                  CurrItem.DiskNumberStart := TAbSpanWriteStream(NewStream).CurrentImage;
                  CurrItem.RelativeOffset := NewStream.Position - MemStream.Size;
                finally
                  MemStream.Free;
                end;
              end
              else begin
                CurrItem.DiskNumberStart := 0;
                CurrItem.RelativeOffset := NewStream.Position;
                CurrItem.SaveLFHToStream(NewStream);
              end;
              {copy compressed data}
              NewStream.CopyFrom(WorkingStream, 0);
              if CurrItem.IsEncrypted then
                CurrItem.SaveDDToStream(NewStream);
            finally
              WorkingStream.Free;
            end;
          except
            on E : Exception do
            begin
              { Exception was caused by a User Abort and Item Failure should not be called
                Question:  Do we want an New Event when this occurs or should the
                exception just be re-raised [783614] }
              if (E is EAbUserAbort) then
                raise;
              CurrItem.Action := aaDelete;
              DoProcessItemFailure(CurrItem, ptAdd, ecFileOpenError, 0);
            end;
          end;
        end;
      end; { case }

      { TODO: Check HasDataDescriptior behavior;  seems like it's getting
              written twice for encrypted files }
      {Now add the data descriptor record to new stream}
      HasDataDescriptor := (CurrItem.CompressionMethod = cmDeflated) and
        ((CurrItem.GeneralPurposeBitFlag and AbHasDataDescriptorFlag) <> 0);
      if (CurrItem.Action <> aaDelete) and HasDataDescriptor then
        CurrItem.SaveDDToStream(NewStream);
      Progress := AbPercentage(9 * succ( i ), 10 * Count);
      DoArchiveSaveProgress(Progress, Abort);
      DoArchiveProgress(Progress, Abort);
      if Abort then
        raise EAbUserAbort.Create;
    end;

    {write the central directory}
    if NewStream is TAbSpanWriteStream then
      FInfo.DiskNumber := TAbSpanWriteStream(NewStream).CurrentImage
    else
      FInfo.DiskNumber := 0;
    FInfo.StartDiskNumber := FInfo.DiskNumber;
    FInfo.DirectoryOffset := NewStream.Position;
    FInfo.DirectorySize := 0;
    FInfo.EntriesOnDisk := 0;
    FInfo.TotalEntries := 0;
    MemStream := TMemoryStream.Create;
    try
      {write central directory entries}
      for i := 0 to Count - 1 do begin
        if not (FItemList[i].Action in [aaDelete, aaFailed]) then begin
          (FItemList[i] as TAbZipItem).SaveCDHToStream(MemStream);
          if NewStream is TAbSpanWriteStream then begin
            TAbSpanWriteStream(NewStream).WriteUnspanned(MemStream.Memory^, MemStream.Size);
            {update tail info on span change}
            if FInfo.DiskNumber <> TAbSpanWriteStream(NewStream).CurrentImage then begin
              FInfo.DiskNumber := TAbSpanWriteStream(NewStream).CurrentImage;
              FInfo.EntriesOnDisk := 0;
              if FInfo.TotalEntries = 0 then begin
                FInfo.StartDiskNumber := FInfo.DiskNumber;
                FInfo.DirectoryOffset := NewStream.Position - MemStream.Size;
              end;
            end;
          end
          else
            NewStream.WriteBuffer(MemStream.Memory^, MemStream.Size);
          FInfo.DirectorySize := FInfo.DirectorySize + MemStream.Size;
          FInfo.EntriesOnDisk := FInfo.EntriesOnDisk + 1;
          FInfo.TotalEntries := FInfo.TotalEntries + 1;
          MemStream.Clear;
        end;
      end;
      {append the central directory footer}
      FInfo.SaveToStream(MemStream, NewStream.Position);
      if NewStream is TAbSpanWriteStream then begin
        {update the footer if writing it would trigger a new span}
        if not TAbSpanWriteStream(NewStream).WriteUnspanned(MemStream.Memory^,
                                                            MemStream.Size) then begin
          FInfo.DiskNumber := TAbSpanWriteStream(NewStream).CurrentImage;
          FInfo.EntriesOnDisk := 0;
          FInfo.SaveToStream(NewStream);
        end;
      end
      else
        NewStream.WriteBuffer(MemStream.Memory^, MemStream.Size);
    finally {MemStream}
      MemStream.Free;
    end; {MemStream}

    FSpanned := (FInfo.DiskNumber > 0);

    {update output stream}
    if NewStream is TAbSpanWriteStream then begin
      {zip has already been written to target location}
      FStream := TAbSpanWriteStream(NewStream).ReleaseStream;
      if Spanned then begin
        {switch to read stream}
        FStream := TAbSpanReadStream.Create(ArchiveName, FInfo.DiskNumber, FStream);
        TAbSpanReadStream(FStream).OnRequestImage := DoRequestImage;
        TAbSpanReadStream(FStream).OnRequestNthDisk := DoRequestNthDisk;
      end
      else begin
        {replace spanned signature}
        FStream.Position := 0;
        FStream.Write(Ab_ZipPossiblySpannedSignature,
          SizeOf(Ab_ZipPossiblySpannedSignature));
      end;
    end
    else begin
      {copy new stream to FStream (non-spanned only)}
      NewStream.Position := 0;
      if (FStream is TMemoryStream) then
        TMemoryStream(FStream).LoadFromStream(NewStream)
      else begin
        if FOwnsStream then begin
          {need new stream to write}
          FreeAndNil(FStream);
          FStream := TFileStream.Create(FArchiveName,
            fmOpenReadWrite or fmShareDenyWrite);
        end;
        FStream.Size := 0;
        FStream.Position := 0;
        FStream.CopyFrom(NewStream, 0)
      end;
    end;

    {update Items list}
    for i := pred( Count ) downto 0 do begin
      if FItemList[i].Action = aaDelete then
        FItemList.Delete( i )
      else if FItemList[i].Action <> aaFailed then
        FItemList[i].Action := aaNone;
    end;

    DoArchiveSaveProgress( 100, Abort );
    DoArchiveProgress( 100, Abort );
  finally {NewStream}
    NewStream.Free;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.SetZipfileComment(const Value: string);
begin
  FInfo.FZipfileComment := Value;
  FIsDirty := True;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipArchive.TestItemAt(Index : Integer);
begin
  DoTestHelper(Index);
end;

end.
