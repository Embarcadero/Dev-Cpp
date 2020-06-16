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

{*********************************************************}
{* ABBREVIA: AbZipper.pas                                *}
{*********************************************************}
{* ABBREVIA: Non-visual Component with Zip support       *}
{*********************************************************}

unit AbZipper;

{$I AbDefine.inc}

interface

uses
  Classes,
  AbBrowse, AbZBrows, AbArcTyp, AbZipTyp;

type
  TAbCustomZipper = class(TAbCustomZipBrowser)
  protected {private}
    FAutoSave               : Boolean;
    FCompressionMethodToUse : TAbZipSupportedMethod;
    FDeflationOption        : TAbZipDeflationOption;
    FDOSMode : Boolean;
    FOnConfirmSave          : TAbArchiveConfirmEvent;
    FOnSave                 : TAbArchiveEvent;
    FOnArchiveSaveProgress  : TAbArchiveProgressEvent;
    FArchiveSaveProgressMeter : IAbProgressMeter;

    FStoreOptions           : TAbStoreOptions;

  protected {methods}
    procedure DoConfirmSave(Sender : TObject; var Confirm : Boolean);
      virtual;
    procedure DoSave(Sender : TObject);
      virtual;
    procedure DoArchiveSaveProgress(Sender : TObject; Progress : Byte;
                                    var Abort : Boolean);

    procedure InitArchive;
      override;
    procedure SetAutoSave(Value : Boolean);
    procedure SetCompressionMethodToUse(Value : TAbZipSupportedMethod);
    procedure SetDeflationOption(Value : TAbZipDeflationOption);
    procedure SetDOSMode( Value : Boolean );
    procedure SetFileName(const aFileName : string);
      override;
    procedure SetStoreOptions( Value : TAbStoreOptions );
    procedure SetArchiveSaveProgressMeter(const Value: IAbProgressMeter);
    procedure SetZipfileComment(const Value : string); override;
    procedure ZipProc(Sender : TObject; Item : TAbArchiveItem;
                      OutStream : TStream);
    procedure ZipFromStreamProc(Sender : TObject; Item : TAbArchiveItem;
                                OutStream, InStream : TStream );
    procedure Notification(Component: TComponent;
      Operation: TOperation); override;
    procedure ResetMeters; override;

  protected {properties}
    property AutoSave : Boolean
      read  FAutoSave
      write SetAutoSave;
    property CompressionMethodToUse : TAbZipSupportedMethod
      read  FCompressionMethodToUse
      write SetCompressionMethodToUse
      default AbDefCompressionMethodToUse;
    property DeflationOption : TAbZipDeflationOption
      read  FDeflationOption
      write SetDeflationOption
      default AbDefDeflationOption;
    property DOSMode : Boolean
      read  FDOSMode
      write SetDOSMode;
    property StoreOptions : TAbStoreOptions
      read  FStoreOptions
      write SetStoreOptions
      default AbDefStoreOptions;
    property ArchiveSaveProgressMeter : IAbProgressMeter
      read  FArchiveSaveProgressMeter
      write SetArchiveSaveProgressMeter;


  protected {events}
    property OnConfirmSave : TAbArchiveConfirmEvent
      read  FOnConfirmSave
      write FOnConfirmSave;
    property OnSave : TAbArchiveEvent
      read  FOnSave
      write FOnSave;
    property OnArchiveSaveProgress : TAbArchiveProgressEvent
      read FOnArchiveSaveProgress
      write FOnArchiveSaveProgress;

  public {methods}
    constructor Create(AOwner : TComponent);
      override;
    destructor Destroy;
      override;
    procedure AddFiles(const FileMask : string; SearchAttr : Integer);
    procedure AddFilesEx(const FileMask, ExclusionMask : string; SearchAttr : Integer);
    procedure AddFromStream(const NewName : string; FromStream : TStream);
    procedure DeleteAt(Index : Integer);
    procedure DeleteFiles(const FileMask : string);
    procedure DeleteFilesEx(const FileMask, ExclusionMask : string);
    procedure DeleteTaggedItems;
    procedure FreshenFiles(const FileMask : string);
    procedure FreshenFilesEx(const FileMask, ExclusionMask : string);
    procedure FreshenTaggedItems;
    procedure Move(aItem : TAbArchiveItem; const NewStoredPath : string);
    procedure Save;
    procedure Replace(aItem : TAbArchiveItem);
  end;

type
  TAbZipper = class(TAbCustomZipper)
  published
    property ArchiveProgressMeter;
    property ArchiveSaveProgressMeter;
    property ItemProgressMeter;
    property AutoSave;
    property BaseDirectory;
    property CompressionMethodToUse;
    property DeflationOption;
    property DOSMode;
    property SpanningThreshold;
    property LogFile;
    property Logging;
    property OnArchiveProgress;
    property OnArchiveSaveProgress;
    property OnArchiveItemProgress;
    property OnChange;
    property OnConfirmProcessItem;
    property OnConfirmSave;
    property OnLoad;
    property OnProcessItemFailure;
    property OnRequestBlankDisk;
    property OnRequestImage;
    property OnRequestLastDisk;
    property OnRequestNthDisk;
    property OnSave;
    property Password;
    property StoreOptions;
    property TempDirectory;
    property Version;
    property FileName; {must be after OnLoad}
  end;

implementation

uses
  SysUtils, AbUtils, AbTarTyp, AbGzTyp, AbBzip2Typ, AbExcept, AbZipPrc;

{ -------------------------------------------------------------------------- }
constructor TAbCustomZipper.Create( AOwner : TComponent );
begin
  inherited Create( AOwner );
  CompressionMethodToUse := AbDefCompressionMethodToUse;
  DeflationOption := AbDefDeflationOption;
  StoreOptions := AbDefStoreOptions;
end;
{ -------------------------------------------------------------------------- }
destructor TAbCustomZipper.Destroy;
begin
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.AddFiles(const FileMask : string; SearchAttr : Integer);
  {Add files to the archive where the disk filespec matches}
begin
  if (FArchive <> nil) then
    FArchive.AddFiles(FileMask, SearchAttr)
  else
    raise EAbNoArchive.Create;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.AddFilesEx(const FileMask, ExclusionMask : string;
  SearchAttr : Integer);
  {Add files that match Filemask except those matching ExclusionMask}
begin
  if (FArchive <> nil) then
    FArchive.AddFilesEx(FileMask, ExclusionMask, SearchAttr)
  else
    raise EAbNoArchive.Create;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.AddFromStream(const NewName : string;
                                        FromStream : TStream);
  {Add stream directly to archive}
begin
  if (FArchive <> nil) then begin
    FromStream.Position := 0;
    FArchive.AddFromStream(NewName, FromStream);
  end else
    raise EAbNoArchive.Create;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.DeleteFiles(const FileMask : string);
  {delete all files from the archive that match the file mask}
begin
  if (FArchive <> nil) then
    FArchive.DeleteFiles( FileMask )
  else
    raise EAbNoArchive.Create;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.DeleteAt(Index : Integer);
  {delete item at Index}
begin
  if (FArchive <> nil) then
    FArchive.DeleteAt( Index )
  else
    raise EAbNoArchive.Create;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.DeleteFilesEx(const FileMask, ExclusionMask : string);
  {Delete files that match Filemask except those matching ExclusionMask}
begin
  if (FArchive <> nil) then
    FArchive.DeleteFilesEx(FileMask, ExclusionMask)
  else
    raise EAbNoArchive.Create;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.DeleteTaggedItems;
  {delete all tagged items from the archive}
begin
  if (FArchive <> nil) then
    FArchive.DeleteTaggedItems
  else
    raise EAbNoArchive.Create;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.DoConfirmSave(Sender : TObject; var Confirm : Boolean);
begin
  Confirm := True;
  if Assigned(FOnConfirmSave) then
    FOnConfirmSave(Self, Confirm);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.DoSave(Sender : TObject);
begin
  if Assigned(FOnSave) then
    FOnSave(Self);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.FreshenFiles(const FileMask : string);
  {freshen all items that match the file mask}
begin
  if (FArchive <> nil) then
    FArchive.FreshenFiles( FileMask )
  else
    raise EAbNoArchive.Create;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.FreshenFilesEx(const FileMask, ExclusionMask : string);
  {freshen all items matching FileMask except those matching ExclusionMask}
begin
  if (FArchive <> nil) then
    FArchive.FreshenFilesEx( FileMask, ExclusionMask )
  else
    raise EAbNoArchive.Create;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.FreshenTaggedItems;
  {freshen all tagged items}
begin
  if (FArchive <> nil) then
    FArchive.FreshenTaggedItems
  else
    raise EAbNoArchive.Create;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.InitArchive;
begin
  inherited InitArchive;
  if FArchive <> nil then begin
    {properties}
    FArchive.AutoSave                                := FAutoSave;
    FArchive.DOSMode                                 := FDOSMode;
    FArchive.StoreOptions                            := FStoreOptions;
    {events}
    FArchive.OnArchiveSaveProgress                   := DoArchiveSaveProgress;
    FArchive.OnConfirmSave                           := DoConfirmSave;
    FArchive.OnSave                                  := DoSave;
  end;
  if (FArchive is TAbZipArchive) then begin
    {properties}
    TAbZipArchive(FArchive).CompressionMethodToUse := FCompressionMethodToUse;
    TAbZipArchive(FArchive).DeflationOption        := FDeflationOption;
    {events}
    TAbZipArchive(FArchive).OnRequestBlankDisk     := OnRequestBlankDisk;
    TAbZipArchive(FArchive).InsertHelper           := ZipProc;
    TAbZipArchive(FArchive).InsertFromStreamHelper := ZipFromStreamProc;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.Move(aItem : TAbArchiveItem; const NewStoredPath : string);
  {renames the item}
begin
  if (FArchive <> nil) then
    FArchive.Move(aItem, NewStoredPath)
  else
    raise EAbNoArchive.Create;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.Replace(aItem : TAbArchiveItem);
  {replace the item}
begin
  if (FArchive <> nil) then
    FArchive.Replace( aItem )
  else
    raise EAbNoArchive.Create;
  DoChange;                                                            
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.Save;
begin
  if (FArchive <> nil) then begin
    FArchive.Save;
    DoChange;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.SetAutoSave(Value : Boolean);
begin
  FAutoSave := Value;
  if (FArchive <> nil) then
    FArchive.AutoSave := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.SetCompressionMethodToUse(
  Value : TAbZipSupportedMethod);
begin
  FCompressionMethodToUse := Value;
  if (FArchive is TAbZipArchive) then
    TAbZipArchive(FArchive).CompressionMethodToUse := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.SetDeflationOption(Value : TAbZipDeflationOption);
begin
  FDeflationOption := Value;
  if (FArchive is TAbZipArchive) then
    TAbZipArchive(FArchive).DeflationOption := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.SetDOSMode(Value : Boolean);
begin
  FDOSMode := Value;
  if (FArchive <> nil) then
    FArchive.DOSMode := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.SetFileName(const aFileName : string);
var
  ArcType : TAbArchiveType;
begin
  FFileName := aFileName;
  if (csDesigning in ComponentState) then
    Exit;

  if Assigned(FArchive) then
  begin
    FArchive.Save;
    FreeAndNil(FArchive);
  end;

  ArcType := ArchiveType;

  if (FileName <> '') then
    if FileExists(FileName) then begin { open it }

    if not ForceType then
      ArcType := AbDetermineArcType(FileName, atUnknown);

      case ArcType of
        atZip, atSpannedZip, atSelfExtZip : begin
         FArchive := TAbZipArchive.Create(FileName, fmOpenRead or fmShareDenyNone);
         InitArchive;
        end;

        atTar : begin
          FArchive := TAbTarArchive.Create(FileName, fmOpenReadWrite or fmShareDenyNone);
          inherited InitArchive;
        end;

        atGZip : begin
          FArchive := TAbGzipArchive.Create(FileName, fmOpenReadWrite or fmShareDenyNone);
          TAbGzipArchive(FArchive).TarAutoHandle := FTarAutoHandle;
          TAbGzipArchive(FArchive).IsGzippedTar := False;
          inherited InitArchive;
        end;

        atGZippedTar : begin
          FArchive := TAbGzipArchive.Create(FileName, fmOpenReadWrite or fmShareDenyNone);
          TAbGzipArchive(FArchive).TarAutoHandle := FTarAutoHandle;
          TAbGzipArchive(FArchive).IsGzippedTar := True;
          inherited InitArchive;
        end;

{$IFNDEF NoBZip2}
        atBzip2 : begin
          FArchive := TAbBzip2Archive.Create(FileName, fmOpenReadWrite or fmShareDenyNone);
          TAbBzip2Archive(FArchive).TarAutoHandle := FTarAutoHandle;
          TAbBzip2Archive(FArchive).IsBzippedTar := False;
          inherited InitArchive;
        end;

        atBzippedTar : begin
          FArchive := TAbBzip2Archive.Create(FileName, fmOpenReadWrite or fmShareDenyNone);
          TAbBzip2Archive(FArchive).TarAutoHandle := FTarAutoHandle;
          TAbBzip2Archive(FArchive).IsBzippedTar := True;
          inherited InitArchive;
        end;
{$ENDIF}

        else
          raise EAbUnhandledType.Create;
      end {case};
      FArchive.Load;
      FArchiveType := ArcType;

    end else begin  { file doesn't exist, so create a new one }
      if not ForceType then
        ArcType := AbDetermineArcType(FileName, atUnknown);

      case ArcType of
        atZip : begin                                                    
          FArchive := TAbZipArchive.Create(FileName, fmCreate);
          InitArchive;
        end;

        atTar : begin
          FArchive := TAbTarArchive.Create(FileName, fmCreate or fmShareDenyNone);
          inherited InitArchive;
        end;

        atGZip : begin
          FArchive := TAbGzipArchive.Create(FileName, fmCreate or fmShareDenyNone);
          TAbGzipArchive(FArchive).TarAutoHandle := FTarAutoHandle;
          TAbGzipArchive(FArchive).IsGzippedTar := False;
          inherited InitArchive;
        end;

        atGZippedTar : begin
          FArchive := TAbGzipArchive.Create(FileName, fmCreate or fmShareDenyNone);
          TAbGzipArchive(FArchive).TarAutoHandle := FTarAutoHandle;
          TAbGzipArchive(FArchive).IsGzippedTar := True;
          inherited InitArchive;
        end;

{$IFNDEF NoBZip2}
        atBzip2 : begin
          FArchive := TAbBzip2Archive.Create(FileName, fmCreate or fmShareDenyNone);
          TAbBzip2Archive(FArchive).TarAutoHandle := FTarAutoHandle;
          TAbBzip2Archive(FArchive).IsBzippedTar := False;
          inherited InitArchive;
        end;

        atBzippedTar : begin
          FArchive := TAbBzip2Archive.Create(FileName, fmCreate or fmShareDenyNone);
          TAbBzip2Archive(FArchive).TarAutoHandle := FTarAutoHandle;
          TAbBzip2Archive(FArchive).IsBzippedTar := True;
          inherited InitArchive;
        end;
{$ENDIF}

        else
          raise EAbUnhandledType.Create;
      end {case};

      FArchiveType := ArcType;
    end;
  DoChange;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.SetStoreOptions(Value : TAbStoreOptions);
begin
  FStoreOptions := Value;
  if (FArchive <> nil) then
    FArchive.StoreOptions := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.SetArchiveSaveProgressMeter(const Value: IAbProgressMeter);
begin
  ReferenceInterface(FArchiveSaveProgressMeter, opRemove);
  FArchiveSaveProgressMeter := Value;
  ReferenceInterface(FArchiveSaveProgressMeter, opInsert);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.SetZipfileComment(const Value : string);
begin
  if (FArchive is TAbZipArchive) then
    TAbZipArchive(FArchive).ZipfileComment := Value
  else
    raise EAbNoArchive.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.ZipProc(Sender : TObject; Item : TAbArchiveItem;
  OutStream : TStream);
begin
  AbZip(TAbZipArchive(Sender), TAbZipItem(Item), OutStream);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.ZipFromStreamProc(Sender : TObject; Item : TAbArchiveItem;
  OutStream, InStream : TStream);
begin
  if Assigned(InStream) then
    AbZipFromStream(TAbZipArchive(Sender), TAbZipItem(Item),
                    OutStream, InStream)
  else
    raise EAbZipNoInsertion.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.DoArchiveSaveProgress(Sender : TObject;
                                                Progress : Byte;
                                                var Abort : Boolean);
begin
  Abort := False;
  if Assigned(FArchiveSaveProgressMeter) then
    FArchiveSaveProgressMeter.DoProgress(Progress);
  if Assigned(FOnArchiveSaveProgress) then
    FOnArchiveSaveProgress(Self, Progress, Abort);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.Notification(Component: TComponent;
                                       Operation: TOperation);
begin
  inherited Notification(Component, Operation);
  if (Operation = opRemove) then
    if Assigned(ArchiveSaveProgressMeter) and Component.IsImplementorOf(ArchiveSaveProgressMeter) then
      ArchiveSaveProgressMeter := nil
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipper.ResetMeters;
begin
  inherited ResetMeters;
  if Assigned(FArchiveSaveProgressMeter) then
    FArchiveSaveProgressMeter.Reset;
end;
{ -------------------------------------------------------------------------- }

end.

