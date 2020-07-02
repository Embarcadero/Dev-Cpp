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
{* ABBREVIA: AbZipKit.pas                                *}
{*********************************************************}
{* ABBREVIA: TABZipKit component                         *}
{*********************************************************}

unit AbZipKit;

{$I AbDefine.inc}

interface

uses
  Classes, AbZipper, AbArcTyp, AbZipTyp;

type
  TAbCustomZipKit = class(TAbCustomZipper)
  protected {private}
    FExtractOptions : TAbExtractOptions;
    FOnConfirmOverwrite : TAbConfirmOverwriteEvent;
    FOnNeedPassword : TAbNeedPasswordEvent;
    FPasswordRetries : Byte;

  protected {methods}
    procedure DoConfirmOverwrite(var Name : string; var Confirm : Boolean);
      virtual;
    procedure DoNeedPassword(Sender : TObject; var NewPassword : string); virtual;
    procedure InitArchive;
      override;
    procedure SetExtractOptions(Value : TAbExtractOptions);
    procedure SetPasswordRetries(Value : Byte);
    procedure UnzipProc(Sender : TObject; Item : TAbArchiveItem;
                        const NewName : string );
    procedure UnzipToStreamProc(Sender : TObject; Item : TAbArchiveItem;
                                OutStream : TStream);
    procedure TestItemProc(Sender : TObject; Item : TAbArchiveItem);

  protected {properties}
    property ExtractOptions : TAbExtractOptions
      read  FExtractOptions
      write SetExtractOptions
      default AbDefExtractOptions;
    property PasswordRetries : Byte
      read  FPasswordRetries
      write SetPasswordRetries
      default AbDefPasswordRetries;

  protected {events}
    property OnConfirmOverwrite : TAbConfirmOverwriteEvent
      read  FOnConfirmOverwrite
      write FOnConfirmOverwrite;
    property OnNeedPassword : TAbNeedPasswordEvent
      read  FOnNeedPassword
      write FOnNeedPassword;

  public {methods}
    constructor Create(AOwner : TComponent);
      override;
    destructor Destroy;
      override;
    procedure ExtractAt(Index : Integer; const NewName : string);
    procedure ExtractFiles(const FileMask : string);
      {extract all files from the archive that match the mask}
    procedure ExtractFilesEx(const FileMask, ExclusionMask : string);
      {extract files matching FileMask except those matching ExclusionMask}
    procedure ExtractTaggedItems;
      {extract all tagged items from the archive}
    procedure ExtractToStream(const aFileName : string; ToStream : TStream);
      {extract the specified item to TStream descendant}
    procedure TestTaggedItems;
      {test all tagged items in the archive}

  public {property}
    property Spanned;
  end;

  TAbZipKit = class(TAbCustomZipKit)
  published
    property ArchiveProgressMeter;
    property ArchiveSaveProgressMeter;
    property AutoSave;
    property BaseDirectory;
    property CompressionMethodToUse;
    property DeflationOption;
    {$IFDEF MSWINDOWS}
    property DOSMode;
    {$ENDIF}
    property ExtractOptions;
    property SpanningThreshold;
    property ItemProgressMeter;
    property LogFile;
    property Logging;
    property OnArchiveProgress;
    property OnArchiveSaveProgress;
    property OnArchiveItemProgress;
    property OnChange;
    property OnConfirmOverwrite;
    property OnConfirmProcessItem;
    property OnConfirmSave;
    property OnLoad;
    property OnNeedPassword;
    property OnProcessItemFailure;
    property OnRequestBlankDisk;
    property OnRequestImage;
    property OnRequestLastDisk;
    property OnRequestNthDisk;
    property OnSave;
    property Password;
    property PasswordRetries;
    property StoreOptions;
    property TempDirectory;
    property Version;
    property FileName; {must be after OnLoad}
  end;


implementation

uses
  AbExcept,
  AbUnzPrc,
  AbZBrows;

{ -------------------------------------------------------------------------- }
constructor TAbCustomZipKit.Create( AOwner : TComponent );
begin
  inherited Create( AOwner );
  PasswordRetries := AbDefPasswordRetries;
end;
{ -------------------------------------------------------------------------- }
destructor TAbCustomZipKit.Destroy;
begin
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipKit.DoConfirmOverwrite( var Name : string;
                                        var Confirm : Boolean );
begin
  Confirm := True;
  if Assigned( FOnConfirmOverwrite ) then
    FOnConfirmOverwrite( Name, Confirm );
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipKit.DoNeedPassword( Sender : TObject; var NewPassword : string );
begin
  if Assigned( FOnNeedPassword ) then begin
    FOnNeedPassword( Self, NewPassword );
    FPassword := NewPassword;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipKit.ExtractAt(Index : Integer; const NewName : string);
  {extract a file from the archive that match the index}
begin
  if (FArchive <> nil) then
    FArchive.ExtractAt( Index, NewName )
  else
    raise EAbNoArchive.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipKit.ExtractFiles(const FileMask : string);
  {extract all files from the archive that match the mask}
begin
  if (FArchive <> nil) then
    FArchive.ExtractFiles( FileMask )
  else
    raise EAbNoArchive.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipKit.ExtractFilesEx(const FileMask, ExclusionMask : string);
  {extract files matching FileMask except those matching ExclusionMask}
begin
  if (FArchive <> nil) then
    FArchive.ExtractFilesEx( FileMask, ExclusionMask )
  else
    raise EAbNoArchive.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipKit.ExtractTaggedItems;
  {extract all tagged items from the archive}
begin
  if (FArchive <> nil) then
    FArchive.ExtractTaggedItems
  else
    raise EAbNoArchive.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipKit.ExtractToStream(const aFileName : string;
                                          ToStream : TStream);
begin
  if (FArchive <> nil) then
    FArchive.ExtractToStream(aFileName, ToStream)
  else
    raise EAbNoArchive.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipKit.InitArchive;                                 
begin
  inherited InitArchive;
  if (FArchive <> nil) then begin
    FArchive.ExtractOptions                       := FExtractOptions;
    FArchive.OnConfirmOverwrite                   := DoConfirmOverwrite;
  end;
  if FArchive is TAbZipArchive then begin
    {properties}
    TAbZipArchive(FArchive).PasswordRetries       := FPasswordRetries;
    {events}
    TAbZipArchive(FArchive).OnNeedPassword        := DoNeedPassword;
    TAbZipArchive(FArchive).ExtractHelper         := UnzipProc;
    TAbZipArchive(FArchive).ExtractToStreamHelper := UnzipToStreamProc;
    TAbZipArchive(FArchive).TestHelper            := TestItemProc;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipKit.SetExtractOptions( Value : TAbExtractOptions );
begin
  FExtractOptions := Value;
  if (FArchive <> nil) then
    FArchive.ExtractOptions := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipKit.SetPasswordRetries( Value : Byte );
begin
  FPasswordRetries := Value;
  if (FArchive <> nil) then
    (FArchive as TAbZipArchive).PasswordRetries := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipKit.TestTaggedItems;
  {test all tagged items in the archive}
begin
  if (FArchive <> nil) then
    FArchive.TestTaggedItems
  else
    raise EAbNoArchive.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipKit.UnzipProc( Sender : TObject; Item : TAbArchiveItem;
  const NewName : string );
begin
  AbUnzip( TAbZipArchive(Sender), TAbZipItem(Item), NewName);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipKit.UnzipToStreamProc(Sender : TObject; Item : TAbArchiveItem;
  OutStream : TStream);
begin
  AbUnzipToStream(TAbZipArchive(Sender), TAbZipItem(Item), OutStream);
end;
{ -------------------------------------------------------------------------- }
procedure TAbCustomZipKit.TestItemProc(Sender : TObject; Item : TAbArchiveItem);
begin
  AbTestZipItem(TAbZipArchive(Sender), TAbZipItem(Item));
end;
{ -------------------------------------------------------------------------- }

end.

