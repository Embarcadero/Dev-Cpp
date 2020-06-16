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
 *   Craig Peterson <capeterson@users.sourceforge.net>
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* ABBREVIA: AbCabTyp.pas                                *}
{*********************************************************}
{* ABBREVIA: Cabinet Archive                             *}
{* Based on info from the FCI/FDI Library Description,   *}
{* included in the Microsoft Cabinet SDK                 *}
{*********************************************************}

unit AbCabTyp;

{$I AbDefine.inc}

interface

{$IFDEF MSWINDOWS}

uses
  Windows, Classes, AbFciFdi, AbArcTyp, AbUtils;

type
  TAbCabItem = class(TAbArchiveItem)
  protected {private}
    FPartialFile : Boolean;
    FRawFileName : AnsiString;
  public
    property PartialFile : Boolean
      read  FPartialFile
      write FPartialFile;
    property RawFileName : AnsiString
      read FRawFileName
      write FRawFileName;
  end;

type
  TAbCabCompressionType = (ctNone, ctMSZIP, ctLZX);
  TAbCabinetMode = (cmRead, cmWrite);
  TAbCabStatus = (csFile, csFolder, csCabinet);


const
  faExtractAndExecute  = $040;
  faUTF8Name = $080;
  AbDefCabSpanningThreshold  = 0;
  AbDefFolderThreshold = 0;
  AbDefCompressionType = ctMSZIP;
  AbDefReserveHeaderSize = 0;
  AbDefReserveFolderSize = 0;
  AbDefReserveDataSize = 0;
  AbDefLZXWindowSize = 18;

  CompressionTypeMap : array[TAbCabCompressionType] of Word = (0, 1, 4611);

type
  TAbCabArchive = class(TAbArchive)
  protected {private}
    {internal variables}
    FCabName         : AnsiString;
    FCabPath         : AnsiString;
    FFCICabInfo      : FCICabInfo;
    FFCIContext      : HFCI;
    FFDIContext      : HFDI;
    FFDICabInfo      : FDICabInfo;
    FErrors          : CabErrorRecord;
    FItemInProgress  : TAbCabItem;
    FItemStream      : TStream;
    FIIPName         : string;
    FItemProgress    : DWord;
    FNextCabinet     : string;
    FNextDisk        : string;
    FTempFileID      : Integer;

    {property variables}
    FCurrentCab         : Word;
    FCabSize           : Integer;
    FCompressionType   : TAbCabCompressionType;
    FFileCount         : Word;
    FFolderThreshold   : UInt32;
    FFolderCount       : Word;
    FHasPrev           : Boolean;
    FHasNext           : Boolean;
    FSetID             : Word;

    {internal methods}
    procedure CloseCabFile;
    procedure CreateCabFile;
    function  CreateItem( const FileSpec : string ): TAbArchiveItem;
      override;
    procedure DoCabItemProgress(BytesCompressed : DWord;
      var Abort : Boolean);
    procedure DoGetNextCabinet(CabIndex : Integer; var CabName : string;
                               var Abort : Boolean);
    procedure ExtractItemAt(Index : Integer; const NewName : string);
      override;
    procedure ExtractItemToStreamAt(Index : Integer; OutStream : TStream);
      override;
    function  GetItem(ItemIndex : Integer) : TAbCabItem;
    procedure LoadArchive;
      override;
    procedure OpenCabFile;
    procedure PutItem( Index : Integer; Value : TAbCabItem );
    procedure SaveArchive;
      override;
    procedure SetFolderThreshold(Value : UInt32);
    procedure SetSetID(Value : Word);
    procedure SetSpanningThreshold(Value : Int64);
      override;
    procedure TestItemAt(Index : Integer);
      override;

  public {methods}
    constructor Create(const FileName : string; Mode : Word);
      override;
    constructor CreateFromStream(aStream : TStream; const aArchiveName : string);
      override;
    destructor Destroy;
      override;
    procedure Add(AItem: TAbArchiveItem); override;
    procedure NewCabinet;
    procedure NewFolder;

  public {properties}
    property CurrentCab : Word
      read  FCurrentCab;
    property CabSize : Integer
      read  FCabSize;
    property CompressionType : TAbCabCompressionType
      read  FCompressionType
      write FCompressionType;
    property FolderThreshold : UInt32
      read  FFolderThreshold
      write SetFolderThreshold;
    property FolderCount : Word
      read  FFolderCount;
    property HasPrev : Boolean
      read  FHasPrev;
    property HasNext : Boolean
      read  FHasNext;
    property Items[Index : Integer] : TAbCabItem
      read  GetItem
      write PutItem; default;
    property ItemProgress : DWord
      read  FItemProgress
      write FItemProgress;
    property SetID : Word
      read  FSetID
      write SetSetID;
  end;

function VerifyCab(const Fn : string) : TAbArchiveType; overload;
function VerifyCab(Strm : TStream) : TAbArchiveType; overload;

{$ENDIF}

implementation

{$IFDEF MSWINDOWS}

uses
  SysUtils, AnsiStrings, AbCharset, AbConst, AbExcept;

{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}

type
  PWord    = ^Word;
  PInteger = ^Integer;

{ == FDI/FCI Callback Functions - cdecl calling convention ================= }
function FXI_GetMem(uBytes : Integer) : Pointer;
  cdecl;
  {allocate memory}
begin
  Result := nil;
  if (uBytes > 0) then
    GetMem(Result, uBytes);
end;
{ -------------------------------------------------------------------------- }
procedure FXI_FreeMem(lpBuffer : Pointer);
  cdecl;
  {free memory}
begin
  FreeMem(lpBuffer);
end;


{ == FCI Callback Functions - cdecl calling convention ===================== }
function FCI_FileOpen(lpPathName: PAnsiChar; Flag, Mode: Integer;
  PError: PInteger; Archive: TAbCabArchive) : PtrInt;
  cdecl;
  {open a file}
begin
  Result := _lcreat(lpPathName, 0);
  if (Result = -1) then
    raise EAbFCIFileOpenError.Create;
end;
{ -------------------------------------------------------------------------- }
function FCI_FileRead(hFile: PtrInt; lpBuffer: Pointer;
  uBytes: UINT; PError: PInteger; Archive: TAbCabArchive) : UINT;
  cdecl;
  {read from a file}
begin
  Result := _lread(hFile, lpBuffer, uBytes);
  if (Result = UINT(-1)) then
    raise EAbFCIFileReadError.Create;
end;
{ -------------------------------------------------------------------------- }
function FCI_FileWrite(hFile: PtrInt; lpBuffer: Pointer;
  uBytes: UINT; PError: PInteger; Archive: TAbCabArchive) : UINT;
  cdecl;
  {write to a file}
begin
  Result := _lwrite(hFile, lpBuffer, uBytes);
  if (Result = UINT(-1)) then
    raise EAbFCIFileWriteError.Create;
end;
{ -------------------------------------------------------------------------- }
function FCI_FileClose(hFile: PtrInt; PError: PInteger;
  Archive: TAbCabArchive) : Integer;
  cdecl;
  {close a file}
begin
  Result := _lclose(hFile);
  if (Result = -1) then
    raise EAbFCIFileCloseError.Create;
end;
{ -------------------------------------------------------------------------- }
function FCI_FileSeek(hFile: PtrInt; Offset: Integer;
  Origin: Integer; PError: PInteger; Archive: TAbCabArchive) : Integer;
  cdecl;
  {reposition file pointer}
begin
  Result := _llseek(hFile, Offset, Origin);
  if (Result = -1) then
    raise EAbFCIFileSeekError.Create;
end;
{ -------------------------------------------------------------------------- }
function FCI_FileDelete(lpFilename: PAnsiChar;  PError: PInteger;
  Archive: TAbCabArchive) : Boolean;
  cdecl;
  {delete a file}
begin
  Result := SysUtils.DeleteFile(string(lpFilename));
  if not Result then
    raise EAbFCIFileDeleteError.Create;
end;
{ -------------------------------------------------------------------------- }
function FCI_GetNextCab(lpCCab: PFCICabInfo; PrevCab: Integer;
  Archive: TAbCabArchive) : Boolean;
  cdecl;
  {get next cabinet filename}
var
  CabName : string;
  Abort : Boolean;
begin
  Abort := False;
  with lpCCab^ do begin
    CabName := string(szCab);
    {obtain next cabinet.  Make index zero-based}
    Archive.DoGetNextCabinet(Pred(iCab), CabName, Abort);
    if not Abort then
      AnsiStrings.StrPLCopy(szCab, AnsiString(CabName), Length(szCab));
  end;
  Result := not Abort;
end;
{ -------------------------------------------------------------------------- }
function FCI_FileDest(PCCab: PFCICabInfo; PFilename: PAnsiChar; cbFile: Integer;
  Continuation: Boolean; Archive: TAbCabArchive) : Integer;
  cdecl;
  {currently not used}
begin
  Result := 0;
end;
{ -------------------------------------------------------------------------- }
function FCI_GetOpenInfo(lpPathname: Pointer; PDate, PTime, PAttribs : PWord;
  PError: PInteger; Archive: TAbCabArchive) : PtrInt;
  cdecl;
  {open a file and return date/attributes}
var
  AttrEx: TAbAttrExRec;
  I, DT: Integer;
  RawName: RawByteString;
begin
  Result := FileOpen(string(lpPathname), fmOpenRead or fmShareDenyNone);
  if (Result = -1) then
    raise EAbFCIFileOpenError.Create;
  if not AbFileGetAttrEx(string(lpPathname), AttrEx) then
    raise EAbFileNotFound.Create;
  PAttribs^ := AttrEx.Attr;
  DT := DateTimeToFileDate(AttrEx.Time);
  PDate^ := DT shr 16;
  PTime^ := DT and $0FFFF;
  Archive.ItemProgress := 0;
  Archive.FItemInProgress.UncompressedSize := AttrEx.Size;
  RawName := Archive.FItemInProgress.RawFileName;
  for I := 1 to Length(RawName) do
    if Ord(RawName[I]) > 127 then
      PAttribs^ := PAttribs^ or faUTF8Name;
end;
{ -------------------------------------------------------------------------- }
function FCI_Status(Status: Word; cb1, cb2: DWord;
                    Archive: TAbCabArchive) : Integer; cdecl;
  {keep archive informed}
var
  Abort : Boolean;
begin
  Result := 0;
  if (Status = Word(csCabinet)) then begin
    Archive.DoSave;
    Archive.FCabSize := cb2;
    Result := cb2;
  end else if (Status = Word(csFolder)) then
    Archive.FCabSize := Archive.FCabSize + Integer(cb2)
  else if (Status = Word(csFile)) then begin
    Archive.DoCabItemProgress(cb2, Abort);
    Result := Integer(Abort);
  end;
end;
{ -------------------------------------------------------------------------- }
function FCI_GetTempFile(lpTempName: PAnsiChar; TempNameSize: Integer;
                         Archive: TAbCabArchive) : PtrInt; cdecl;
  {obtain temporary filename}
var
  TempPath : array[0..255] of AnsiChar;
begin
  Archive.FTempFileID := Archive.FTempFileID + 1;
  if (Archive.TempDirectory <> '') then
    AnsiStrings.StrPLCopy(TempPath, AnsiString(Archive.TempDirectory), Length(TempPath))
  else
    GetTempPathA(255, TempPath);
  GetTempFileNameA(TempPath, 'VMS', Archive.FTempFileID, lpTempName);
  Result := 1;
end;

{ == FDI Callback Functions - cdecl calling convention ===================== }
function FDI_FileOpen(lpPathName: PAnsiChar; Flag, Mode: Integer) : PtrInt;
  cdecl;
  {open a file}
begin
  try
    Result := PtrInt(TFileStream.Create(string(lpPathName), fmOpenRead or fmShareDenyWrite));
  except on EFOpenError do
    Result := -1;
  end;
end;
{ -------------------------------------------------------------------------- }
function FDI_FileRead(hFile: PtrInt; lpBuffer: Pointer; uBytes: UINT) : UINT;
  cdecl;
  {read from a file}
begin
  Result := TStream(hFile).Read(lpBuffer^, uBytes);
end;
{ -------------------------------------------------------------------------- }
function FDI_FileWrite(hFile: PtrInt; lpBuffer: Pointer; uBytes: UINT) : UINT;
  cdecl;
  {write to a file}
begin
  Result := TStream(hFile).Write(lpBuffer^, uBytes);
end;
{ -------------------------------------------------------------------------- }
function FDI_FileClose(hFile : PtrInt) : Integer;
  cdecl;
  {close a file}
begin
  try
    TStream(hFile).Free;
    Result := 0;
  except
    Result := -1;
  end;
end;
{ -------------------------------------------------------------------------- }
function FDI_FileSeek(hFile : PtrInt; Offset : Integer; Origin : Integer) : Integer;
  cdecl;
  {reposition file pointer}
begin
  Result := TStream(hFile).Seek(Offset, Origin);
end;
{ -------------------------------------------------------------------------- }
function FDI_EnumerateFiles(fdint : FDINOTIFICATIONTYPE;
  pfdin : PFDINotification) : PtrInt;
  cdecl;
  {Enumerate the files and build the archive file list}
var
  Item : TAbCabItem;
  Archive : TAbCabArchive;
begin
  Result := 0;
  Archive := pfdin^.pv;
  with Archive do case fdint of
    FDINT_Cabinet_Info :
      begin
        FSetID := pfdin^.setID;
        FCurrentCab := pfdin^.iCabinet;
        FNextCabinet := string(pfdin^.psz1);
        FNextDisk    := string(pfdin^.psz2);
        Result := 0;
      end;
    FDINT_Copy_File, FDINT_Partial_File :
      begin
        Item := TAbCabItem.Create;
        with Item do begin
          RawFileName := AnsiString(pfdin^.psz1);
          if (pfdin^.attribs and faUTF8Name) = faUTF8Name then
            Filename := UTF8ToString(RawFileName)
          else
            Filename := string(RawFileName);
          UnCompressedSize := pfdin^.cb;
          LastModFileDate := pfdin^.date;
          LastModFileTime := pfdin^.time;
          ExternalFileAttributes := pfdin^.attribs;
          IsEncrypted := False;  {encryption not implemented at this time}
          PartialFile := (fdint = FDINT_Partial_File);
        end;
        FItemList.Add(Item);
        Result := 0;
      end;
  end;
end;
{ -------------------------------------------------------------------------- }
function FDI_ExtractFiles(fdint : FDINOTIFICATIONTYPE;
  pfdin : PFDINotification) : PtrInt;
  cdecl;
  {extract file from cabinet}
var
  Archive : TAbCabArchive;
begin
  Result := 0;
  Archive := pfdin^.pv;
  case fdint of
    FDINT_Copy_File :
      begin
        if (AnsiString(pfdin^.psz1) = Archive.FItemInProgress.RawFileName) then
          if Archive.FIIPName <> '' then
            Result := Integer(TFileStream.Create(Archive.FIIPName, fmCreate))
          else
            Result := Integer(Archive.FItemStream)
        else
          Result := 0;
      end;
    FDINT_Next_Cabinet :
      begin
        if pfdin^.fdie = FDIError_None then
          Result := 0
        else
          Result := -1;
      end;
    FDINT_Close_File_Info :
      begin
        if Archive.FIIPName <> '' then begin
          FileSetDate(TFileStream(pfdin^.hf).Handle,
            Integer(pfdin^.date) shl 16 + pfdin^.time);
          TFileStream(pfdin^.hf).Free;
          FileSetAttr(Archive.FIIPName, pfdin^.attribs);
        end;
        Result := 1;
      end;
  end;
end;


{ == TAbCabArchive ========================================================= }
function VerifyCab(const Fn : string) : TAbArchiveType;
var
  Stream : TFileStream;
begin
  Stream := TFileStream.Create(FN, fmOpenRead or fmShareDenyNone);
  try
    Result := VerifyCab(Stream);
  finally
    Stream.Free;
  end;
end;
{ -------------------------------------------------------------------------- }
function VerifyCab(Strm : TStream) : TAbArchiveType; overload;
var
  Context : HFDI;
  Info : FDICabInfo;
  Errors : CabErrorRecord;
  StartPos : int64;
begin
  Result := atUnknown;
  Context := FDICreate(@FXI_GetMem, @FXI_FreeMem, @FDI_FileOpen,
    @FDI_FileRead, @FDI_FileWrite, @FDI_FileClose, @FDI_FileSeek, cpuDefault,
    @Errors);
  if Context = nil then
    Exit;
  try
    StartPos := Strm.Position;
    if FDIIsCabinet(Context, Integer(Strm), @Info) then
      Result := atCab;
    Strm.Position := StartPos;
  finally
    FDIDestroy(Context);
  end;
end;


{ == TAbCabArchive ========================================================= }
constructor TAbCabArchive.Create(const FileName : string; Mode : Word );
begin
  {Mode is used to identify which interface to use: }
  {  fmOpenWrite - FCI, fmOpenRead - FDI}
  inherited CreateInit;
  if (Mode and fmCreate) = fmCreate then FMode := fmOpenWrite
  else FMode := Mode and fmOpenWrite;
  FArchiveName := FileName;
  FCabName := AnsiString(ExtractFileName(FileName));
  FCabPath := AnsiString(ExtractFilePath(FileName));
  SpanningThreshold := AbDefCabSpanningThreshold;
  FFolderThreshold := AbDefFolderThreshold;
  FItemInProgress := nil;
  FItemProgress := 0;
end;
{ -------------------------------------------------------------------------- }
constructor TAbCabArchive.CreateFromStream(aStream : TStream;
  const aArchiveName : string);
begin
  raise EAbCabException.Create('TAbCabArchive does not support CreateFromStream');
end;
{ -------------------------------------------------------------------------- }
destructor TAbCabArchive.Destroy;
begin
  CloseCabFile;
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCabArchive.Add(AItem: TAbArchiveItem);
  {add a file to the cabinet}
var
  lConfirm: Boolean;
  lItemAdded: Boolean;
  lItem: TAbCabItem;
begin
  lItemAdded := False;
  try
    CheckValid;
    if FMode <> fmOpenWrite then
    begin
      DoProcessItemFailure(AItem, ptAdd, ecCabError, 0);
      Exit;
    end;
    if FItemList.IsActiveDupe(AItem.FileName) then
    begin
      DoProcessItemFailure(AItem, ptAdd, ecAbbrevia, AbDuplicateName);
      Exit;
    end;
    DoConfirmProcessItem(AItem, ptAdd, lConfirm);
    if not lConfirm then
      Exit;
    lItem := TAbCabItem(AItem);
    FItemInProgress := lItem;
    lItem.Action := aaAdd;

    lItem.RawFileName := UTF8Encode(lItem.FileName);
    if not FCIAddFile(FFCIContext, Pointer(lItem.DiskFileName),
       PAnsiChar(lItem.RawFileName), False, @FCI_GetNextCab, @FCI_Status,
       @FCI_GetOpenInfo, CompressionTypeMap[FCompressionType]) then
      raise EAbFCIAddFileError.Create;
    FItemList.Add(lItem);
    lItemAdded := True;

    FIsDirty := True;
  finally
    if not lItemAdded then
      AItem.Free;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCabArchive.CloseCabFile;
  {Make sure the Cabinet DLL is shut down}
var
  Abort : Boolean;
begin
  if (FFDIContext <> nil) then begin
    FDIDestroy(FFDIContext);
    FFDIContext := nil;
  end;
  if (FFCIContext <> nil) then begin
    FCIFlushCabinet(FFCIContext, False, @FCI_GetNextCab, @FCI_Status);
    FCIDestroy(FFCIContext);
    FFCIContext := nil;
  end;
  DoArchiveProgress(0, Abort);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCabArchive.CreateCabFile;
  {create a new cabinet}
begin
    {set cabinet parameters}
  with FFCICabInfo do begin
    if (SpanningThreshold > 0) then
      cb := SpanningThreshold
    else
      cb := AbDefCabSpanningThreshold;
    if (FolderThreshold > 0) then
      cbFolderThresh := FolderThreshold
    else
      cbFolderThresh  := AbDefFolderThreshold;
    cbReserveCFHeader := AbDefReserveHeaderSize;
    cbReserveCFFolder := AbDefReserveFolderSize;
    cbReserveCFData   := AbDefReserveDataSize;
    iCab              := 1;
    iDisk             := 0;
    fFailOnIncompressible := 0;
    setID             := SetID;
    AnsiStrings.StrPCopy(szDisk, '');
    AnsiStrings.StrPLCopy(szCab, FCabName, Length(szCab));
    AnsiStrings.StrPLCopy(szCabPath, FCabPath, Length(szCabPath));
  end;

    {obtain an FCI context}
  FFCIContext := FCICreate(@FErrors, @FCI_FileDest, @FXI_GetMem, @FXI_FreeMem,
    @FCI_FileOpen, @FCI_FileRead, @FCI_FileWrite, @FCI_FileClose, @FCI_FileSeek,
    @FCI_FileDelete, @FCI_GetTempFile, @FFCICabInfo, Self);
  if (FFCIContext = nil) then
    if FErrors.ErrorPresent then begin
      CloseCabFile;
      raise EAbFCICreateError.Create;
    end;
end;
{ -------------------------------------------------------------------------- }
function TAbCabArchive.CreateItem( const FileSpec : string ): TAbArchiveItem;
  {create a new item for the file list}
begin
  Result := TAbCabItem.Create;
  with TAbCabItem(Result) do begin
    CompressedSize := 0;
    DiskFileName := ExpandFileName(FileSpec);
    FileName := FixName(FileSpec);
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCabArchive.DoCabItemProgress(BytesCompressed : DWord;
  var Abort : Boolean);
  {fire OnCabItemProgress event}
var
  Progress : Byte;
begin
  Abort := False;
  if Assigned(FOnArchiveItemProgress) then begin
    Inc(FItemProgress, BytesCompressed);
    Progress := AbPercentage(FItemProgress,
      FItemInProgress.UnCompressedSize);
    FOnArchiveItemProgress(Self, FItemInProgress, Progress, Abort);
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCabArchive.DoGetNextCabinet(CabIndex : Integer;
  var CabName : string; var Abort : Boolean);
  {fire OnRequestImage event}
begin
  Abort := False;
  if Assigned(FOnRequestImage) then
    FOnRequestImage(Self, CabIndex, CabName, Abort)
  else
    AbIncFilename(CabName, CabIndex);
end;
{----------------------------------------------------------------------------}
procedure TAbCabArchive.ExtractItemAt(Index : Integer; const NewName : string);
  {extract a file from the cabinet}
begin
  FItemInProgress := GetItem(Index);
  FIIPName := NewName;
  try
    if not FDICopy(FFDIContext, PAnsiChar(FCabName), PAnsiChar(FCabPath), 0,
                   @FDI_ExtractFiles, nil, Self) then
      DoProcessItemFailure(FItemInProgress, ptExtract, ecCabError, FErrors.ErrorCode);
  finally
    FIIPName := '';
  end;
end;
{----------------------------------------------------------------------------}
procedure TAbCabArchive.ExtractItemToStreamAt(Index : Integer; OutStream : TStream);
begin
  FItemInProgress := GetItem(Index);
  FItemStream := OutStream;
  try
    if not FDICopy(FFDIContext, PAnsiChar(FCabName), PAnsiChar(FCabPath), 0,
                   @FDI_ExtractFiles, nil, Self) then
      DoProcessItemFailure(FItemInProgress, ptExtract, ecCabError, FErrors.ErrorCode);
  finally
    FItemStream := nil;
  end;
end;
{----------------------------------------------------------------------------}
function TAbCabArchive.GetItem(ItemIndex : Integer) : TAbCabItem;
  {fetch an item from the file list}
begin
  Result := TAbCabItem(FItemList.Items[ItemIndex]);
end;
{----------------------------------------------------------------------------}
procedure TAbCabArchive.LoadArchive;
  {Open existing cabinet or create a new one}
begin
  if (FMode = fmOpenRead) then begin
    FFDIContext := FDICreate(@FXI_GetMem, @FXI_FreeMem, @FDI_FileOpen,
    @FDI_FileRead, @FDI_FileWrite, @FDI_FileClose, @FDI_FileSeek,
    cpuDefault, @FErrors);
    if (FFDIContext = nil) then
      raise EAbFDICreateError.Create;
    OpenCabFile;
  end else
    CreateCabFile;
end;
{----------------------------------------------------------------------------}
procedure TAbCabArchive.NewCabinet;
  {flush current cabinet and start a new one}
begin
  if not FCIFlushCabinet(FFCIContext, True, @FCI_GetNextCab, @FCI_Status) then
    raise EAbFCIFlushCabinetError.Create;
end;
{----------------------------------------------------------------------------}
procedure TAbCabArchive.NewFolder;
  {flush current folder and start a new one}
begin
  if not FCIFlushFolder(FFCIContext, @FCI_GetNextCab, @FCI_Status) then
    raise EAbFCIFlushFolderError.Create;
end;
{----------------------------------------------------------------------------}
procedure TAbCabArchive.OpenCabFile;
  {Open an existing cabinet}
var
  Abort : Boolean;
  Stream : TFileStream;
begin
    {verify that the archive can be opened and is a cabinet}
  Stream := TFileStream.Create(FArchiveName, fmOpenRead or fmShareDenyNone);
  try
    if not FDIIsCabinet(FFDIContext, PtrInt(Stream), @FFDICabInfo) then begin
      CloseCabFile;
      raise EAbInvalidCabFile.Create;
    end;
  finally
    Stream.Free;
  end;

    {store information about the cabinet}
  FCabSize := FFDICabInfo.cbCabinet;
  FFolderCount := FFDICabInfo.cFolders;
  FFileCount := FFDICabInfo.cFiles;
  FCurrentCab := FFDICabInfo.iCabinet;
  FHasPrev := FFDICabInfo.hasPrev;
  FHasNext := FFDICabInfo.hasNext;

    {Enumerate the files and build the file list}
  if not FDICopy(FFDIContext, PAnsiChar(FCabName), PAnsiChar(FCabPath), 0,
    @FDI_EnumerateFiles, nil, Self) then begin
    CloseCabFile;
    raise EAbFDICopyError.Create;
  end;
  DoArchiveProgress(100, Abort);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCabArchive.PutItem( Index : Integer; Value : TAbCabItem );
  {replace an existing item in the file list}
begin
  FItemList.Items[Index] := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCabArchive.SaveArchive;
begin
  { No-op;  file is flushed in destructor }
end;
{ -------------------------------------------------------------------------- }
procedure TAbCabArchive.SetFolderThreshold(Value : UInt32);
  {set maximum compression boundary}
begin
  if (Value > 0) then
    FFolderThreshold := Value
  else
    FFolderThreshold := AbDefFolderThreshold;
  FFCICabInfo.cbFolderThresh := FFolderThreshold;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCabArchive.SetSetID(Value : Word);
  {set cabinet SetID}
begin
  FSetID := Value;
  FFCICabInfo.SetID := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCabArchive.SetSpanningThreshold(Value : Int64);
  {set maximum cabinet size}
begin
  if (Value > 0) then
    FSpanningThreshold := Value
  else
    FSpanningThreshold := AbDefCabSpanningThreshold;
  FFCICabInfo.cb := FSpanningThreshold;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCabArchive.TestItemAt(Index : Integer);
begin
  {not implemented for cabinet archives}
end;

{$ENDIF}

end.
