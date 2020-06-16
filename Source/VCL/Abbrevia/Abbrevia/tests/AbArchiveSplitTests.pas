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

unit AbArchiveSplitTests;

{$I AbDefine.inc}

interface

uses
  AbTestFramework;

type

  { TAbArchiveSplitTests }

  TAbArchiveSplitTests = class(TAbTestCase)
  private
    procedure AbortOnImageRequestEVENT (Sender : TObject; ImageNumber : Integer;
                                  var ImageName : string; var Abort : Boolean);
    procedure TestExtract(const aFileName, aSourceDir: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure DecompressSplitArchive;
    procedure CreateSplitArchive;
    procedure AbortOnImageRequest;
  end;

implementation

uses
  SysUtils, TestFrameWork, AbExcept, AbUnzper, AbZipTyp, AbZipper;

{ TAbArchiveSplitTests }

procedure TAbArchiveSplitTests.AbortOnImageRequest;
var
  Zip : TAbZipper;
begin
  // [ 783614 ] Item #1
  // 3.05 Beta Code produced EAbBadStream, it should be EAbUserAbort
  ExpectedException := EAbUserAbort;

  Zip := TAbZipper.create(nil);
  try
    Zip.BaseDirectory := CanterburySourceDir;
    Zip.CompressionMethodToUse := smStored;
    Zip.FileName := TestTempDir + 'SplitTest.zip';
    Zip.SpanningThreshold := 50000;
    Zip.OnRequestImage := AbortOnImageRequestEVENT;
    Zip.AddFiles('*', faAnyFile);
    Zip.Save;
  finally
    Zip.Free;
  end;
end;

procedure TAbArchiveSplitTests.AbortOnImageRequestEVENT(Sender: TObject;
  ImageNumber: Integer; var ImageName: string; var Abort: Boolean);
begin
  if ImageNumber = 2 then
    Abort := True;
end;

procedure TAbArchiveSplitTests.TestExtract(const aFileName, aSourceDir: string);
var
  TargetDir: string;
  UnZip: TAbUnZipper;
begin
  TargetDir := TestTempDir + 'test';
  CreateDir(TargetDir);
  UnZip := TAbUnZipper.Create(nil);
  try
    UnZip.BaseDirectory := TargetDir;
    UnZip.FileName := aFileName;
    UnZip.ExtractFiles('*');
  finally
    UnZip.Free;
  end;
  CheckDirMatch(aSourceDir, TargetDir);
end;

procedure TAbArchiveSplitTests.CreateSplitArchive;
var
  Zip : TAbZipper;
begin
  Zip := TAbZipper.Create(nil);
  try
    Zip.BaseDirectory := CanterburySourceDir;
    Zip.FileName := TestTempDir + 'Test.zip';
    Zip.SpanningThreshold := 50000;
    Zip.AddFiles('*', faAnyFile);
    Zip.Save;
  finally
    Zip.Free;
  end;
  TestExtract(TestTempDir + 'Test.zip', CanterburySourceDir);
end;

procedure TAbArchiveSplitTests.DecompressSplitArchive;
begin
  TestExtract(CanterburyDir + 'Split' + PathDelim + 'Split.zip',
    CanterburySourceDir);
end;

procedure TAbArchiveSplitTests.SetUp;
begin
  inherited;
end;

procedure TAbArchiveSplitTests.TearDown;
begin
  inherited;
end;

initialization

  TestFramework.RegisterTest('Abbrevia.File Splitting Tests',
    TAbArchiveSplitTests.Suite);
 
end.

 
