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
 *
 * ***** END LICENSE BLOCK ***** *)

unit _ZipKit;

interface

uses
  ComObj, Classes, Windows, Abbrevia_TLB, ActiveX, axctrls, AbZipKit, AbArcTyp,
  AbUtils, _ZipItem, _GZipItem, _TarItem, AbZipTyp, AbTarTyp, AbGzTyp,
  AbConst, AbBrowse;

type
  {$IFNDEF VER130}{$WARN SYMBOL_PLATFORM OFF}{$ENDIF}
  TZipKit = class(TAutoObject, IConnectionPointContainer, IEnumVariant, IZipKit)
  private
    {private declarations}
    FConnectionPoints : TConnectionPoints;
    FEvents           : IZipKitEvents;
    FOwner            : TAbZipKit;
    FEnumPos          : Integer;

    {Events for FOwner}
    procedure _OnArchiveItemProgress(Sender : TObject; Item : TAbArchiveItem; Progress : Byte; var Abort : Boolean);
    procedure _OnArchiveProgress(Sender : TObject; Progress : Byte; var Abort : Boolean);
    procedure _OnChange(Sender : TObject);
    procedure _OnConfirmOverwrite(var Name : string; var confirm : Boolean);
    procedure _OnConfirmProcessItem(Sender : TObject; Item : TAbArchiveItem; ProcessType : TAbProcessType; var Confirm : Boolean);
    procedure _OnConfirmSave(Sender : TObject; var Confirm : Boolean);
    procedure _OnLoad(Sender : TObject);
    procedure _OnNeedPassword(Sender : TObject; var NewPassword : AnsiString);
    procedure _OnProcessItemFailure(Sender : TObject; Item : TAbArchiveItem; ProcessType : TAbProcessType; ErrorClass : TAbErrorClass; ErrorCode : Integer);
    procedure _OnRequestBlankDisk(Sender : TObject; var Abort : Boolean);
    procedure _OnRequestImage(Sender : TObject; ImageNumber : Integer; var ImageName : string; var Abort : Boolean);
    procedure _OnRequestLastDisk(Sender : TObject; var Abort : Boolean);
    procedure _OnRequestNthDisk(Sender : TObject; DiskNumber : Byte; var Abort : Boolean);
    procedure _OnSave(Sender : TObject);

  public
    procedure Initialize; override;
    destructor Destroy; override;

  protected

    {IConnectionPointContainer}
    property ConnectionPoints: TConnectionPoints read FConnectionPoints
      implements IConnectionPointContainer;
    procedure EventSinkChanged(const EventSink: IUnknown); override;


    {IEnumVariant}
    function Next(celt: LongWord; var rgvar : OleVariant; out pceltFetched: LongWord): HResult; stdcall;
    function Skip(celt: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out Enum: IEnumVariant): HResult; stdcall;


    {IZipKit}
    procedure Add(const FileMask: WideString; const ExclusionMask: WideString; SearchAttr: Integer); safecall;
    procedure AddFromStream(const FileName: WideString; Stream: OleVariant); safecall;
    function  Get_AutoSave: WordBool; safecall;
    procedure Set_AutoSave(Value: WordBool); safecall;
    function  Get_BaseDirectory: WideString; safecall;
    procedure Set_BaseDirectory(const Value: WideString); safecall;
    procedure ClearTags; safecall;
    function  Get_CompressionMethodToUse: TZipSupportMethod; safecall;
    procedure Set_CompressionMethodToUse(Value: TZipSupportMethod); safecall;
    function  Get_Count: Integer; safecall;
    function  Get_DeflateOption: TZipDeflateOption; safecall;
    procedure Set_DeflateOption(Value: TZipDeflateOption); safecall;
    procedure Delete(const FileMask: WideString; const ExclusionMask: WideString); safecall;
    procedure DeleteAt(Index: Integer); safecall;
    procedure DeleteTaggedItems; safecall;
    function  Get_DOSMode: WordBool; safecall;
    procedure Set_DOSMode(Value: WordBool); safecall;
    procedure Extract(const FileMask: WideString; const ExclusionMask: WideString); safecall;
    procedure ExtractAt(Index: Integer; const NewName: WideString); safecall;
    function  Get_ExtractOptions: TZipExtractOptions; safecall;
    procedure Set_ExtractOptions(Value: TZipExtractOptions); safecall;
    procedure ExtractTaggedItems; safecall;
    function  Get_FileName: WideString; safecall;
    procedure Set_FileName(const Value: WideString); safecall;
    function  Find(const FileName: WideString): Integer; safecall;
    procedure Freshen(const FileMask: WideString; const ExclusionMask: WideString); safecall;
    procedure FreshenTaggedItems; safecall;
    function  Get_Item(Index: Integer): IDispatch; safecall;
    function  Get_LogFile: WideString; safecall;
    procedure Set_LogFile(const Value: WideString); safecall;
    function  Get_Logging: WordBool; safecall;
    procedure Set_Logging(Value: WordBool); safecall;
    function  Get_Password: WideString; safecall;
    procedure Set_Password(const Value: WideString); safecall;
    function  Get_PasswordRetries: Byte; safecall;
    procedure Set_PasswordRetries(Value: Byte); safecall;
    procedure Replace(const FileMask: WideString); safecall;
    procedure Save; safecall;
    function  Get_Spanned: WordBool; safecall;
    function  Get_SpanningThreshold: Integer; safecall;
    procedure Set_SpanningThreshold(Value: Integer); safecall;
    function  Get_Status: TArchiveStatus; safecall;
    function  Get_StoreOptions: TStoreOptions; safecall;
    procedure Set_StoreOptions(Value: TStoreOptions); safecall;
    procedure TagItems(const FileMask: WideString); safecall;
    function  Get_TempDirectory: WideString; safecall;
    procedure Set_TempDirectory(const Value: WideString); safecall;
    procedure TestTaggedItems; safecall;
    procedure UntagItems(const FileMask: WideString); safecall;
    function  Get_ZipFileComment: WideString; safecall;
    procedure Set_ZipFileComment(const Value: WideString); safecall;
    function  License(const Key: WideString): WordBool; safecall;
    function  Get__NewEnum: IUnknown; safecall;
    function ExtractToStream(const FileName: WideString): OleVariant; safecall;
    function Get_CompressionType: TArchiveType; safecall;
    procedure Set_CompressionType(Value: TArchiveType); safecall;

    function Get_TarAutoHandle: WordBool; safecall;
    procedure Set_TarAutoHandle(Value: WordBool); safecall;
  end;


implementation


uses
  ComServ;

{------------------------------------------------------------------------------}
{IConnectionPointContainer}
{------------------------------------------------------------------------------}
procedure TZipKit.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as IZipKitEvents;
end;
{------------------------------------------------------------------------------}
{IEnumVariant}
{------------------------------------------------------------------------------}
function TZipKit.Next(celt: LongWord; var rgvar : OleVariant; out pceltFetched: LongWord): HResult; stdcall;
var
  V : OleVariant;
  I : Integer;
begin
  Result := S_FALSE;
  try
    if @pceltFetched <> nil then
      pceltFetched := 0;
    for I := 0 to celt - 1 do begin
      if FEnumPos >= FOwner.Count then begin
        FEnumPos := 0;
        Exit;
      end;
      V := Get_Item(FEnumPos);
      PVariantArgList(@rgvar)[I] := TVariantArg(V);

      { Prevent COM garbage collection }
      TVarData(V).VType := varEmpty;
      TVarData(V).VInteger := 0;

      Inc(FEnumPos);
      if @pceltFetched <> nil then
        Inc(pceltFetched);
    end;
  except
  end;
  if (@pceltFetched = nil) or (pceltFetched = celt) then
   Result := S_OK;
end;
{------------------------------------------------------------------------------}
function TZipKit.Skip(celt: LongWord): HResult;
begin
  Inc(FEnumPos, celt);
  Result := S_OK;
end;
{------------------------------------------------------------------------------}
function TZipKit.Reset: HResult;
begin
  FEnumPos := 0;
  Result := S_OK;
end;
{------------------------------------------------------------------------------}
function TZipKit.Clone(out Enum: IEnumVariant): HResult;
begin
  Enum := nil;
  Result := S_OK;
  try
    Enum := Self.Create;
    TZipKit(Enum).FOwner := FOwner;
  except
    Result := E_OUTOFMEMORY;
  end;
end;
{------------------------------------------------------------------------------}
{IZipKit}
{------------------------------------------------------------------------------}
procedure TZipKit.Add(const FileMask: WideString; const ExclusionMask: WideString; SearchAttr: Integer);
begin
  FOwner.AddFilesEx(FileMask, ExclusionMask, SearchAttr);
end;
{------------------------------------------------------------------------------}
procedure TZipKit.AddFromStream(const FileName: WideString; Stream: OleVariant);
var
  InStream : TMemoryStream;
  Info     : array of Byte;
begin
  Info := nil;
  InStream := TMemoryStream.Create;
  try
    Info := Stream;
    InStream.Write(Info[0], Length(Info));
    InStream.Position := 0;
    FOwner.AddFromStream(FileName, InStream);
  finally
    InStream.Free;
  end;
end;
{------------------------------------------------------------------------------}
function  TZipKit.Get_AutoSave: WordBool;
begin
  Result := FOwner.AutoSave;
end;
{------------------------------------------------------------------------------}
procedure TZipKit.Set_AutoSave(Value: WordBool);
begin
  FOwner.AutoSave := Value;
end;
{------------------------------------------------------------------------------}
function  TZipKit.Get_BaseDirectory: WideString;
begin
  Result := FOwner.BaseDirectory;
end;
{------------------------------------------------------------------------------}
procedure TZipKit.Set_BaseDirectory(const Value: WideString);
begin
  FOwner.BaseDirectory := Value;
end;
{------------------------------------------------------------------------------}
procedure TZipKit.ClearTags;
begin
  FOwner.ClearTags;
end;
{------------------------------------------------------------------------------}
function  TZipKit.Get_CompressionMethodToUse: TZipSupportMethod;
begin
  Result := TZipCompressionMethod(FOwner.CompressionMethodToUse);
end;
{------------------------------------------------------------------------------}
procedure TZipKit.Set_CompressionMethodToUse(Value: TZipSupportMethod);
begin
  FOwner.CompressionMethodToUse := TAbZipSupportedMethod(Value);
end;
{------------------------------------------------------------------------------}
function  TZipKit.Get_Count: Integer;
begin
  Result := FOwner.Count;
end;
{------------------------------------------------------------------------------}
function  TZipKit.Get_DeflateOption: TZipDeflateOption;
begin
  Result := TZipDeflateOption(FOwner.DeflationOption);
end;
{------------------------------------------------------------------------------}
procedure TZipKit.Set_DeflateOption(Value: TZipDeflateOption);
begin
  FOwner.DeflationOption := TAbZipDeflationOption(Value);
end;
{------------------------------------------------------------------------------}
procedure TZipKit.Delete(const FileMask: WideString; const ExclusionMask: WideString);
begin
  FOwner.DeleteFilesEx(FileMask, ExclusionMask);
end;
{------------------------------------------------------------------------------}
procedure TZipKit.DeleteAt(Index: Integer);
begin
  FOwner.DeleteAt(Index);
end;
{------------------------------------------------------------------------------}
procedure TZipKit.DeleteTaggedItems;
begin
  FOwner.DeleteTaggedItems;
end;
{------------------------------------------------------------------------------}
function  TZipKit.Get_DOSMode: WordBool;
begin
  Result := FOwner.DOSMode;
end;
{------------------------------------------------------------------------------}
procedure TZipKit.Set_DOSMode(Value: WordBool);
begin
  FOwner.DOSMode := Value;
end;
{------------------------------------------------------------------------------}
procedure TZipKit.Extract(const FileMask: WideString; const ExclusionMask: WideString);
begin
  FOwner.ExtractFilesEx(FileMask, ExclusionMask);
end;
{------------------------------------------------------------------------------}
procedure TZipKit.ExtractAt(Index: Integer; const NewName: WideString);
begin
  FOwner.ExtractAt(Index, NewName);
end;
{------------------------------------------------------------------------------}
function  TZipKit.Get_ExtractOptions: TZipExtractOptions;
begin
  Result := 0;
  if TAbExtractOption(eoCreateDirs) in FOwner.ExtractOptions then
    Result := Result + TZipExtractOptions(eoCreateDirs);
  if TAbExtractOption(eoRestorePath) in FOwner.ExtractOptions then
    Result := Result + TZipExtractOptions(eoRestorePath);
end;
{------------------------------------------------------------------------------}
procedure TZipKit.Set_ExtractOptions(Value: TZipExtractOptions);
var
  TempOptions : TAbExtractOptions;
begin
  TempOptions := [];
  if (Value or Abbrevia_TLB.eoCreateDirs) = Value then
    Include(TempOptions, AbArcTyp.eoCreateDirs);
  if (Value or Abbrevia_TLB.eoRestorePath) = Value then
    Include(TempOptions, AbArcTyp.eoRestorePath);
  FOwner.ExtractOptions := TempOptions
end;
{------------------------------------------------------------------------------}
procedure TZipKit.ExtractTaggedItems;
begin
  FOwner.ExtractTaggedItems;
end;
{------------------------------------------------------------------------------}
function  TZipKit.Get_FileName: WideString;
begin
  Result := FOwner.FileName;
end;
{------------------------------------------------------------------------------}
procedure TZipKit.Set_FileName(const Value: WideString);
begin
  FOwner.FileName := Value;
end;
{------------------------------------------------------------------------------}
function  TZipKit.Find(const FileName: WideString): Integer;
begin
  Result := FOwner.FindFile(FileName);
end;
{------------------------------------------------------------------------------}
procedure TZipKit.Freshen(const FileMask: WideString; const ExclusionMask: WideString);
begin
  FOwner.FreshenFilesEx(FileMask, ExclusionMask);
end;
{------------------------------------------------------------------------------}
procedure TZipKit.FreshenTaggedItems;
begin
  FOwner.FreshenTaggedItems;
end;
{------------------------------------------------------------------------------}
function  TZipKit.Get_Item(Index: Integer): IDispatch;
begin
  Result := TZipItem.Create(FOwner.Items[Index], FOwner);
end;
{------------------------------------------------------------------------------}
function  TZipKit.Get_LogFile: WideString;
begin
  Result := FOwner.LogFile;
end;
{------------------------------------------------------------------------------}
procedure TZipKit.Set_LogFile(const Value: WideString);
begin
  FOwner.LogFile := Value;
end;
{------------------------------------------------------------------------------}
function  TZipKit.Get_Logging: WordBool;
begin
  Result := FOwner.Logging;
end;
{------------------------------------------------------------------------------}
procedure TZipKit.Set_Logging(Value: WordBool);
begin
  FOwner.Logging := Value;
end;
{------------------------------------------------------------------------------}
function  TZipKit.Get_Password: WideString;
begin
  Result := WideString(FOwner.Password);
end;
{------------------------------------------------------------------------------}
procedure TZipKit.Set_Password(const Value: WideString);
begin
  FOwner.Password := AnsiString(Value);
end;
{------------------------------------------------------------------------------}
function  TZipKit.Get_PasswordRetries: Byte;
begin
  Result := FOwner.PasswordRetries;
end;
{------------------------------------------------------------------------------}
procedure TZipKit.Set_PasswordRetries(Value: Byte);
begin
  FOwner.PasswordRetries := Value;
end;
{------------------------------------------------------------------------------}
procedure TZipKit.Replace(const FileMask: WideString);
begin
  FOwner.Replace(FOwner.Items[FOwner.FindFile(FileMask)]);
end;
{------------------------------------------------------------------------------}
procedure TZipKit.Save;
begin
  FOwner.Save;
end;
{------------------------------------------------------------------------------}
function  TZipKit.Get_Spanned: WordBool;
begin
  Result := FOwner.Spanned;
end;
{------------------------------------------------------------------------------}
function  TZipKit.Get_SpanningThreshold: Integer;
begin
  Result := FOwner.SpanningThreshold;
end;
{------------------------------------------------------------------------------}
procedure TZipKit.Set_SpanningThreshold(Value: Integer);
begin
  FOwner.SpanningThreshold := Value;
end;
{------------------------------------------------------------------------------}
function  TZipKit.Get_Status: TArchiveStatus;
begin
  Result := TArchiveStatus(FOwner.Status);
end;
{------------------------------------------------------------------------------}
function  TZipKit.Get_StoreOptions: TStoreOptions;
begin
  Result := 0;
  if TAbStoreOption(soStripDrive) in FOwner.StoreOptions then
    Result := Result + TStoreOptions(soStripDrive);
  if TAbStoreOption(soStripPath) in FOwner.StoreOptions then
    Result := Result + TStoreOptions(soStripPath);
  if TAbStoreOption(soRemoveDots) in FOwner.StoreOptions then
    Result := Result + TStoreOptions(soRemoveDots);
  if TAbStoreOption(soRecurse) in FOwner.StoreOptions then
    Result := Result + TStoreOptions(soRecurse);
  if TAbStoreOption(soFreshen) in FOwner.StoreOptions then
    Result := Result + TStoreOptions(soFreshen);
  if TAbStoreOption(soReplace) in FOwner.StoreOptions then
    Result := Result + TStoreOptions(soReplace);
end;
{------------------------------------------------------------------------------}
procedure TZipKit.Set_StoreOptions(Value: TStoreOptions);
var
  TempOptions : TAbStoreOptions;
begin
  TempOptions := [];
  if (Value or Abbrevia_TLB.soStripDrive) = Value then
    Include(TempOptions, AbArcTyp.soStripDrive);
  if (Value or Abbrevia_TLB.soStripPath) = Value then
    Include(TempOptions, AbArcTyp.soStripPath);
  if (Value or Abbrevia_TLB.soRemoveDots) = Value then
    Include(TempOptions, AbArcTyp.soRemoveDots);
  if (Value or Abbrevia_TLB.soRecurse) = Value then
    Include(TempOptions, AbArcTyp.soRecurse);
  if (Value or Abbrevia_TLB.soFreshen) = Value then
    Include(TempOptions, AbArcTyp.soFreshen);
  if (Value or Abbrevia_TLB.soReplace) = Value then
    Include(TempOptions, AbArcTyp.soReplace);
  FOwner.StoreOptions := TempOptions
end;
{------------------------------------------------------------------------------}
procedure TZipKit.TagItems(const FileMask: WideString);
begin
  FOwner.TagItems(FileMask);
end;
{------------------------------------------------------------------------------}
function  TZipKit.Get_TempDirectory: WideString;
begin
  Result := FOwner.TempDirectory;
end;
{------------------------------------------------------------------------------}
procedure TZipKit.Set_TempDirectory(const Value: WideString);
begin
  FOwner.TempDirectory := Value;
end;
{------------------------------------------------------------------------------}
procedure TZipKit.TestTaggedItems;
begin
  FOwner.TestTaggedItems;
end;
{------------------------------------------------------------------------------}
procedure TZipKit.UntagItems(const FileMask: WideString);
begin
  FOwner.UnTagItems(FileMask);
end;
{------------------------------------------------------------------------------}
function  TZipKit.Get_ZipFileComment: WideString;
begin
  Result := WideString(FOwner.ZipFileComment);
end;
{------------------------------------------------------------------------------}
procedure TZipKit.Set_ZipFileComment(const Value: WideString);
begin
  FOwner.ZipfileComment := AnsiString(Value);
end;
{------------------------------------------------------------------------------}
function  TZipKit.License(const Key: WideString): WordBool;
begin
  Result := True;
end;
{------------------------------------------------------------------------------}
function  TZipKit.Get__NewEnum: IUnknown;
begin
  Result := Self;
end;
{------------------------------------------------------------------------------}
function TZipKit.ExtractToStream(const FileName: WideString): OleVariant;
var
  Stream : TMemoryStream;
  Info : array of Byte;
begin
  Stream := TMemoryStream.Create;
  try
    FOwner.ExtractToStream(FileName, Stream);
    Stream.Position := 0;
    SetLength(Info, Stream.Size);
    Stream.Read(Info[0], Stream.Size);
    Result := Info;
  finally
    Stream.Free;
  end;
end;
{------------------------------------------------------------------------------}
function TZipKit.Get_CompressionType: TArchiveType;
begin
  Result := TArchiveType((FOwner as TAbBaseBrowser).ArchiveType);
end;
{------------------------------------------------------------------------------}
procedure TZipKit.Set_CompressionType(Value: TArchiveType);
begin
  (FOwner as TAbBaseBrowser).ArchiveType := TAbArchiveType(ord(Value));
end;
{------------------------------------------------------------------------------}
function TZipKit.Get_TarAutoHandle: WordBool;
begin
  Result := FOwner.TarAutoHandle;
end;
{------------------------------------------------------------------------------}
procedure TZipKit.Set_TarAutoHandle(Value: WordBool);
begin
  FOwner.TarAutoHandle := Value;
end;
{------------------------------------------------------------------------------}
{TZipKit Events}
{------------------------------------------------------------------------------}
procedure TZipKit._OnArchiveItemProgress(Sender : TObject; Item : TAbArchiveItem;
                   Progress : Byte; var Abort : Boolean);
var
  FAbort : WordBool;
begin
  FAbort := Abort;
  if Assigned(FEvents) then begin
    if ((FOwner as TAbBaseBrowser).ArchiveType = atZip) then
      FEvents.OnArchiveItemProgress(TZipItem.Create(TAbZipItem(Item), FOwner),
              Progress, FAbort)
    else if ((FOwner as TAbBaseBrowser).ArchiveType = atTar) then
      FEvents.OnArchiveItemProgress(TTarItem.Create(TAbTarItem(Item), FOwner),
              Progress, FAbort)
    else if ((FOwner as TAbBaseBrowser).ArchiveType = atGZip) then
      FEvents.OnArchiveItemProgress(TGZipItem.Create(TAbGZipItem(Item), FOwner),
              Progress, FAbort);
  end;
  Abort := FAbort;
end;
{------------------------------------------------------------------------------}
procedure TZipKit._OnArchiveProgress(Sender : TObject; Progress : Byte;
                   var Abort : Boolean);
var
  FAbort : WordBool;
begin
  FAbort := Abort;
  if Assigned(FEvents) then
    FEvents.OnArchiveProgress(Progress, FAbort);
  Abort := FAbort;
end;
{------------------------------------------------------------------------------}
procedure TZipKit._OnChange(Sender : TObject);
begin
  if Assigned(FEvents) then
    FEvents.OnChange;
end;
{------------------------------------------------------------------------------}
procedure TZipKit._OnConfirmOverwrite(var Name : string; var confirm : Boolean);
var
  FConfirm : WordBool;
  FName    : WideString;
begin
  FConfirm := Confirm;
  FName    := Name;
  if Assigned(FEvents) then
    FEvents.OnConfirmOverwrite(FName, FConfirm);
  Name    := FName;
  Confirm := FConfirm;
end;
{------------------------------------------------------------------------------}
procedure TZipKit._OnConfirmProcessItem(Sender : TObject; Item : TAbArchiveItem;
                   ProcessType : TAbProcessType; var Confirm : Boolean);
var
  FConfirm : WordBool;
begin
  FConfirm := Confirm;
  if Assigned(FEvents) then begin
    if ((FOwner as TAbBaseBrowser).ArchiveType = atZip) then
      FEvents.OnConfirmProcessItem(TZipItem.Create(TAbZipItem(Item), FOwner),
              TProcessType(ProcessType), FConfirm)
    else if ((FOwner as TAbBaseBrowser).ArchiveType = atTar) then
      FEvents.OnConfirmProcessItem(TTarItem.Create(TAbTarItem(Item), FOwner),
              TProcessType(ProcessType), FConfirm)
    else if ((FOwner as TAbBaseBrowser).ArchiveType = atGZip) then
      FEvents.OnConfirmProcessItem(TGZipItem.Create(TAbGZipItem(Item), FOwner),
              TProcessType(ProcessType), FConfirm);
  end;
  Confirm := FConfirm
end;
{------------------------------------------------------------------------------}
procedure TZipKit._OnConfirmSave(Sender : TObject; var Confirm : Boolean);
var
  FConfirm : WordBool;
begin
  FConfirm := Confirm;
  if Assigned(FEvents) then
    FEvents.OnConfirmSave(FConfirm);
  Confirm := FConfirm;
end;
{------------------------------------------------------------------------------}
procedure TZipKit._OnLoad(Sender : TObject);
begin
  if Assigned(FEvents) then
    FEvents.OnLoad;
end;
{------------------------------------------------------------------------------}
procedure TZipKit._OnNeedPassword(Sender : TObject; var NewPassword : AnsiString);
var
  FNewPassword : WideString;
begin
  FNewPassword := WideString(NewPassword);
  if Assigned(FEvents) then
    FEvents.OnNeedPassword(FNewPassword);
  NewPassword := AnsiString(FNewPassword);
end;
{------------------------------------------------------------------------------}
procedure TZipKit._OnProcessItemFailure(Sender : TObject; Item : TAbArchiveItem;
                   ProcessType : TAbProcessType; ErrorClass : TAbErrorClass;
                   ErrorCode : Integer);
begin
  if Assigned(FEvents) then begin
    if ((FOwner as TAbBaseBrowser).ArchiveType = atZip) then
      FEvents.OnProcessItemFailure(TZipItem.Create(TAbZipItem(Item), FOwner),
              TProcessType(ProcessType), TErrorClass(ErrorClass),
              TErrorCode(ErrorCode), AbStrRes(ErrorCode))
    else if ((FOwner as TAbBaseBrowser).ArchiveType = atTar) then
      FEvents.OnProcessItemFailure(TTarItem.Create(TAbTarItem(Item), FOwner),
              TProcessType(ProcessType), TErrorClass(ErrorClass),
              TErrorCode(ErrorCode),AbStrRes(ErrorCode))
    else if ((FOwner as TAbBaseBrowser).ArchiveType = atGZip) then
      FEvents.OnProcessItemFailure(TGZipItem.Create(TAbGZipItem(Item), FOwner),
              TProcessType(ProcessType),  TErrorClass(ErrorClass),
              TErrorCode(ErrorCode),AbStrRes(ErrorCode));
  end;
end;
{------------------------------------------------------------------------------}
procedure TZipKit._OnRequestBlankDisk(Sender : TObject; var Abort : Boolean);
var
  FAbort : WordBool;
begin
  FAbort := Abort;
  if Assigned(FEvents) then
    FEvents.OnRequestBlankDisk(FAbort);
  Abort := FAbort;
end;
{------------------------------------------------------------------------------}
procedure TZipKit._OnRequestImage(Sender : TObject; ImageNumber : Integer; var ImageName : string; var Abort : Boolean);
var
  FImageName : WideString;
  FAbort     : WordBool;
begin
  FImageName := ImageName;
  FAbort := Abort;
  if Assigned(FEvents) then
    FEvents.OnRequestImage(ImageNumber, FImageName, FAbort);
  Abort := FAbort;
  ImageName := FImageName;
end;
{------------------------------------------------------------------------------}
procedure TZipKit._OnRequestLastDisk(Sender : TObject; var Abort : Boolean);
var
  FAbort : WordBool;
begin
  FAbort := Abort;
  if Assigned(FEvents) then
    FEvents.OnRequestLastDisk(FAbort);
  Abort := FAbort;
end;
{------------------------------------------------------------------------------}
procedure TZipKit._OnRequestNthDisk(Sender : TObject; DiskNumber : Byte; var Abort : Boolean);
var
  FAbort : WordBool;
begin
  FAbort := Abort;
  if Assigned(FEvents) then
    FEvents.OnRequestNthDisk(DiskNumber, FAbort);
  Abort := FAbort;
end;
{------------------------------------------------------------------------------}
procedure TZipKit._OnSave(Sender : TObject);
begin
  if Assigned(FEvents) then
    FEvents.OnSave;
end;
{------------------------------------------------------------------------------}
procedure TZipKit.Initialize;
begin
  inherited Initialize;
  FConnectionPoints := TConnectionPoints.Create(Self);
  if AutoFactory.EventTypeInfo <> nil then
    FConnectionPoints.CreateConnectionPoint(AutoFactory.EventIID,
      ckSingle, EventConnect);
  FOwner := AbZipKit.TAbZipKit.Create(nil);
  FOwner.OnArchiveItemProgress := _OnArchiveItemProgress;
  FOwner.OnArchiveProgress     := _OnArchiveProgress;
  FOwner.OnChange              := _OnChange;
  FOwner.OnConfirmOverwrite    := _OnConfirmOverwrite;
  FOwner.OnConfirmProcessItem  := _OnConfirmProcessItem;
  FOwner.OnConfirmSave         := _OnConfirmSave;
  FOwner.OnLoad                := _OnLoad;
  FOwner.OnNeedPassword        := _OnNeedPassword;
  FOwner.OnProcessItemFailure  := _OnProcessItemFailure;
  FOwner.OnRequestBlankDisk    := _OnRequestBlankDisk;
  FOwner.OnRequestImage        := _OnRequestImage;
  FOwner.OnRequestLastDisk     := _OnRequestLastDisk;
  FOwner.OnRequestNthDisk      := _OnRequestNthDisk;
  FOwner.OnSave                := _OnSave;
  FEnumPos := 0;
end;
{------------------------------------------------------------------------------}
destructor TZipKit.Destroy;
begin
  FOwner.Free;
  inherited Destroy;
end;
{------------------------------------------------------------------------------}

initialization
  TAutoObjectFactory.Create(ComServer, TZipKit, Class_ZipKit, ciMultiInstance, tmBoth);


end.
