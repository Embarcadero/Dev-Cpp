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
 * Robert Love
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

unit AbZipKitTests;

{$I AbDefine.inc}

interface

uses
  AbTestFrameWork, AbZipKit;

type
  TAbZipKitTests = class(TAbCompTestCase)
  private
    Component : TAbZipKit;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestDefaultStreaming;
    procedure TestComponentLinks;
    procedure TestAddThenExtract;
    procedure TestTaggedFiles;
    procedure FreshenTest;
    procedure FreshenBaseDir;
    procedure TestComment;
    procedure TestRenameCollision;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Classes, SysUtils,
  TestFrameWork,
  AbArcTyp, AbZipTyp;

{ TAbZipKitTests }

procedure TAbZipKitTests.FreshenBaseDir;
  // Test Freshen without setting the Base Directory
  // SF.NET Tracker [ 892830 ] DiskFileName is not set correctly
var
  TestFile : string;
  SL : TStringList;
  MS : TMemoryStream;
begin
  TestFile := TestTempDir + 'freshenBaseDir.zip';

  Component.StoreOptions := Component.StoreOptions + [soRecurse,soFreshen];
  Component.FileName := TestFile;
  Component.DeflationOption := doMaximum;

  // Create Files to add

  // Create 3 Text Files to add to archive.
  SL := TStringList.Create;
  try
    SL.Add('Test File');

    SL.SaveToFile(TestTempDir + 'Freshen1base.fsh');
    SL.SaveToFile(TestTempDir + 'Freshen2base.fsh');
    SL.SaveToFile(TestTempDir + 'Freshen3base.fsh');
    Component.AddFiles(TestTempDir + 'Freshen1base.fsh', 0);
    Component.AddFiles(TestTempDir + 'Freshen2base.fsh', 0);
    Component.AddFiles(TestTempDir + 'Freshen3base.fsh', 0);

    Component.CloseArchive;

    // Modify the 2nd File
    SL.Add('Modification');
    SL.SaveToFile(TestTempDir + 'Freshen2base.fsh');
  finally
    SL.Free;
  end;

  // Freshen the archive
  Component.FileName := TestFile;
  Component.DeflationOption := doMaximum;
  Component.StoreOptions := Component.StoreOptions + [soRecurse, soFreshen];
  Component.AddFiles(TestTempDir + 'Freshen1base.fsh', 0);
  Component.AddFiles(TestTempDir + 'Freshen2base.fsh', 0);
  Component.AddFiles(TestTempDir + 'Freshen3base.fsh', 0);
  Component.Save;

  // Make sure modified file and archive value matches
  MS := TMemoryStream.create;
  try
    Component.ExtractToStream(Component.Items[1].FileName, MS);
    CheckFileMatchesStream(TestTempDir + 'Freshen2base.fsh', MS,
      'Freshened file on disk did not match archived value');
  finally
    MS.Free;
  end;
end;

procedure TAbZipKitTests.FreshenTest;
  // [887909] soFreshen isn't working
var
  SL : TStringList;
  MS : TMemoryStream;
begin
  // Create 3 Text Files to add to archive.
  SL := TStringList.Create;
  try
    SL.Add('Test File');

    SL.SaveToFile(TestTempDir + 'Freshen1.fsh');
    SL.SaveToFile(TestTempDir + 'Freshen2.fsh');
    SL.SaveToFile(TestTempDir + 'Freshen3.fsh');

    if FileExists(TestTempDir + 'Freshen.zip') then
      DeleteFile(TestTempDir + 'Freshen.zip');

    Component.FileName := TestTempDir + 'Freshen.zip';
    Component.BaseDirectory := TestTempDir;
    Component.DeflationOption := doMaximum;
    Component.StoreOptions := Component.StoreOptions + [soRecurse, soFreshen];
    Component.AddFiles('*.fsh',0);
    Component.Save;
    Component.CloseArchive;

    // Modify the 2nd File
    SL.Add('Modification');
    SL.SaveToFile(TestTempDir + 'Freshen2.fsh');
  finally
    SL.Free;
  end;

  // Freshen the archive
  Component.FileName := TestTempDir + 'Freshen.zip';
  Component.BaseDirectory := TestTempDir;
  Component.DeflationOption := doMaximum;
  Component.StoreOptions := Component.StoreOptions + [soRecurse, soFreshen];
  Component.AddFiles('*.fsh',0);
  Component.Save;

  // Make sure modified file and archive value matches
  MS := TMemoryStream.create;
  try
    Component.ExtractToStream('Freshen2.fsh',MS);
    CheckFileMatchesStream(TestTempDir + 'Freshen2.fsh', MS,
      'Freshened file on disk did not match archived value');
  finally
    MS.Free;
  end;
end;

procedure TAbZipKitTests.SetUp;
begin
  inherited;
  Component := TAbZipKit.Create(nil);
end;

procedure TAbZipKitTests.TearDown;
begin
  Component.Free;
  inherited;
end;

procedure TAbZipKitTests.TestAddThenExtract;
var
  MS : TMemoryStream;
  I : Integer;
begin
  //TODO: This is broken.  The BaseDirectory is wrong, so it never actually adds anything
  // [ 785769 ] SF.NET Tracker ID is the Bug this is testing for.

  // This test is designed to add to an archive
  // Then extract from it without having to close/reopen archive.

  Component.FileName := TestTempDir + 'ZKitTest.zip';
  Component.BaseDirectory := GetWindowsDir;
  Component.AddFiles('*.ZIP', faAnyFile);
  Component.Save;
  for I := 0 to Component.Count - 1 do begin
    MS := TMemoryStream.Create;
    try
      // Compare uncompressed files to original files
      Component.ExtractToStream(Component.Items[I].FileName, MS);
      CheckFileMatchesStream(TestFileDir + ExtractFileName(Component.Items[I].FileName),
        MS, 'File ' + Component.Items[I].FileName + ' did not match original');
    finally
      MS.Free;
    end;
  end;
  Check(True); //TODO: Replace this with a proper test
end;

procedure TAbZipKitTests.TestComponentLinks;
begin
  TestComponentLink(Component, 'ArchiveProgressMeter', TAbTestMeter);
  TestComponentLink(Component, 'ArchiveSaveProgressMeter', TAbTestMeter);
  TestComponentLink(Component, 'ItemProgressMeter', TAbTestMeter);
end;

procedure TAbZipKitTests.TestDefaultStreaming;
begin
  inherited TestDefaultStreaming(Component);
end;

procedure TAbZipKitTests.TestTaggedFiles;
  // [ 806077 ] TestTaggedItems fails if called after modifying an archive
begin
  Component.FileName := TestTempDir + 'test.zip';
  Component.AutoSave := True;
  Component.BaseDirectory := TestFileDir;
  Component.ClearTags;
  Component.AddFiles('*.*',0);
  Component.TagItems('*.*');
  Check(Component.Count > 0);
  // TestTaggedItems should not raise an error.
  Component.TestTaggedItems;
  Component.CloseArchive;
end;

procedure TAbZipKitTests.TestComment;
  // Verify that Delphi 2009 handles zip file comments correctly (fixed in rev 158)
const
  SComment = 'Test comment';
var
  Zip: TAbZipKit;
begin
  Zip := TAbZipKit.Create(nil);
  try
    Zip.FileName := TestTempDir + 'comment.zip';
    Zip.ZipFileComment := SComment;
    Zip.BaseDirectory := MPLDir;
    Zip.AddFiles('MPL-1_1.txt', 0);
    Zip.Save;
  finally
    Zip.Free;
  end;
  Zip := TAbZipKit.Create(nil);
  try
    Zip.FileName := TestTempDir + 'comment.zip';
    CheckEquals(SComment, Zip.ZipFileComment);
  finally
    Zip.Free
  end;
end;

procedure TAbZipKitTests.TestRenameCollision;
var
  Zip: TAbZipKit;
  FN: string;
begin
  Zip := TAbZipKit.Create(nil);
  try
    Zip.FileName := CanterburyDir + 'Deflate.zip';
    FN := UpperCase(Zip[0].FileName);
    CheckNotEquals(Zip[0].FileName, FN, 'Initial filenames match');
    Zip.Move(Zip[0], FN);
    CheckEquals(Zip[0].FileName, FN, 'Changing only case failed');
    Zip.Move(Zip[0], Zip[1].FileName);
    CheckEquals(Zip[0].FileName, FN, 'Collision succeeded');
  finally
    Zip.Free;
  end;
end;

initialization

  TestFramework.RegisterTest('Abbrevia.Component Level Test Suite',
    TAbZipKitTests.Suite);
 
end.

