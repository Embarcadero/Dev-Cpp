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
unit AbZipperTests;

{$I AbDefine.inc}

interface

uses
  AbTestFrameWork, AbZipper;

type
  TAbZipperTests = class(TAbCompTestCase)
  private
    Component : TAbZipper;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestDefaultStreaming;
    procedure TestComponentLinks;
    procedure BasicZipFile;
    procedure CreateAndTestBasicZipFile;
    procedure CreateAndTestBasicZipFile2;
    procedure CreateAndTestBasicZipFile3;
    procedure CreateAndTestBasicZipFile4;
    procedure BasicGZipTarFile;
    procedure BasicGZipTarFile2;
    procedure TestBasicForceTypeZip;
    procedure TestBasicForceTypeGZipTar;
    procedure CreatePasswordProtectedAddedByStream;
    procedure GZipInputStreamClearTest;
    procedure CreateSimplePWZip;
    procedure CreateMultiple;
    procedure TestLocale1;
    procedure TestLocale2;
    procedure TestLocale3;
    procedure TestLocale4;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Classes, SysUtils,
  TestFrameWork,
  AbArcTyp, AbUtils, AbUnZper, AbZipTyp;

{ TAbZipperTests }

procedure TAbZipperTests.BasicGZipTarFile;
var
  TestFileName : string;
begin
  // This test only insure that the Gzip Tar file is created without raising errors
  // it is not designed to test the data in the resulting zip file.
  TestFileName := TestTempDir + 'basic.tgz';
  Component.FileName := TestFileName;
  Component.BaseDirectory := TestFileDir;
  Component.AddFiles('*.*',faAnyFile);
  Component.Save;
  CheckFileExists(TestFileName);
end;

procedure TAbZipperTests.BasicGZipTarFile2;
var
  TestFileName : string;
begin
  TestFileName := TestTempDir + 'basic.tar.gz';
  Component.FileName := TestFileName;
  Component.BaseDirectory := TestFileDir;
  Component.AddFiles('*.*',faAnyFile);
  Component.Save;
  CheckFileExists(TestFileName);
end;

procedure TAbZipperTests.BasicZipFile;
var
  TestFileName : string;
begin
  // This test only insure that the zip file is created without raising errors
  // it is not designed to test the data in the resulting zip file.
  TestFileName := TestTempDir + 'basic.zip';
  Component.FileName := TestFileName;
  Component.BaseDirectory := TestFileDir;
  Component.AddFiles('*.*',faAnyFile);
  Component.Save;
  CheckFileExists(TestFileName);
end;

procedure TAbZipperTests.CreateAndTestBasicZipFile;
var
  ExtractDir, TestFileName : string;
  AbUnZip : TAbUnZipper;
begin
  // Test with Setting BaseDirectory and not specifying AutoSave
  TestFileName := TestTempDir + 'basic.zip';
  if FileExists(TestFileName) then
    DeleteFile(TestFileName);
  Component.FileName := TestFileName;
  Component.BaseDirectory := TestFileDir;
  Component.AddFiles('*.*',faAnyFile);
  Component.Save;
  Component.FileName := '';
  CheckFileExists(TestFileName);

  AbUnZip := TAbUnZipper.Create(nil);
  try
    AbUnZip.FileName := TestFileName;
    // Clean out old Directory and create a new one.
    Extractdir := TestTempDir + 'extracttest';
    if DirectoryExists(ExtractDir) then
      DelTree(ExtractDir);
    CreateDir(ExtractDir);
    // Extract Files.
    AbUnZip.BaseDirectory := ExtractDir;
    AbUnZip.ExtractFiles('*.*');
    // Compare Extracted Files
    CheckDirMatch(TestFileDir,ExtractDir);
  finally
    AbUnZip.Free;
  end;
  DeleteFile(TestFileName);
end;

procedure TAbZipperTests.CreateAndTestBasicZipFile2;
var
  TestFileName : String;
  ExtractDir : String;
  AbUnZip : TAbUnZipper;
begin
  // Test AutoSave and not setting BaseDirectory specify full path AddFiles
  TestFileName := TestTempDir + 'basic.zip';
  if FileExists(TestFileName) then
    DeleteFile(TestFileName);
  Component.AutoSave := True;
  Component.FileName := TestFileName;
  Component.AddFiles(TestFileDir + '*.*',faAnyFile);
  Component.FileName := '';
  CheckFileExists(TestFileName);

  AbUnZip := TAbUnZipper.Create(nil);
  try
    AbUnZip.FileName := TestFileName;
    // Clean out old Directory and create a new one.
    Extractdir := TestTempDir + 'extracttest';
    if DirectoryExists(ExtractDir) then
      DelTree(ExtractDir);
    CreateDir(ExtractDir);
    // Extract Files.
    AbUnZip.BaseDirectory := ExtractDir;
    AbUnZip.ExtractFiles('*.*');
    // Compare Extracted Files
	CheckDirMatch(TestFileDir,ExtractDir);
  finally
    AbUnZip.Free;
  end;
  DeleteFile(TestFileName);
end;

procedure TAbZipperTests.CreateAndTestBasicZipFile3;
var
  ExtractDir, TestFileName : string;
  AbUnZip : TAbUnZipper;
begin
  // Test AutoSave setting Base Directory.
  TestFileName := TestTempDir + 'basic.zip';
  Component.AutoSave := True;
  Component.BaseDirectory := TestFileDir;
  Component.FileName := TestFileName;
  Component.AddFiles('*.*',faAnyFile);
  Component.FileName := '';
  CheckFileExists(TestFileName);

  AbUnZip := TAbUnZipper.Create(nil);
  try
    AbUnZip.FileName := TestFileName;
    // Clean out old Directory and create a new one.
    Extractdir := TestTempDir + 'extracttest';
    if DirectoryExists(ExtractDir) then
      DelTree(ExtractDir);
    CreateDir(ExtractDir);
    // Extract Files.
    AbUnZip.BaseDirectory := ExtractDir;
    AbUnZip.ExtractFiles('*.*');
    // Compare Extracted Files
	CheckDirMatch(TestFileDir,ExtractDir);
  finally
    AbUnZip.Free;
  end;
end;

procedure TAbZipperTests.CreateAndTestBasicZipFile4;
var
  ExtractDir, TestFileName : String;
  AbUnZip : TAbUnZipper;
begin
  // Test AutoSave setting BaseDirectory and specifing full path to AddFiles
  TestFileName := TestTempDir + 'basic.zip';
  Component.AutoSave := True;
  Component.BaseDirectory := TestTempDir;
  Component.FileName := TestFileName;
  Component.AddFiles(TestFileDir + '*.*',faAnyFile);
  Component.FileName := '';
  CheckFileExists(TestFileName);

  AbUnZip := TAbUnZipper.Create(nil);
  try
    AbUnZip.FileName := TestFileName;
    // Clean out old Directory and create a new one.
    Extractdir := TestTempDir + 'extracttest';
    if DirectoryExists(ExtractDir) then
      DelTree(ExtractDir);
    CreateDir(ExtractDir);
    // Extract Files.
    AbUnZip.BaseDirectory := ExtractDir;
    AbUnZip.ExtractFiles('*.*');
    // Compare Extracted Files
    CheckDirMatch(TestFileDir,ExtractDir);
  finally
    AbUnZip.Free;
  end;
end;

procedure TAbZipperTests.SetUp;
begin
  inherited;
  Component := TAbZipper.Create(nil);
end;

procedure TAbZipperTests.TearDown;
begin
  Component.Free;
  inherited;
end;

procedure TAbZipperTests.TestBasicForceTypeGZipTar;
var
  TestFileName : string;
begin
  TestFileName := TestTempDir + 'basicGzipTar';
  Component.ArchiveType := atGzippedTar;
  Component.ForceType := True;
  Component.FileName := TestFileName;
  Component.BaseDirectory := TestFileDir;
  Component.AddFiles('*.*',faAnyFile);
  Component.Save;
  CheckFileExists(TestFileName);
end;

procedure TAbZipperTests.TestBasicForceTypeZip;
var
  TestFileName : string;
begin
  TestFileName := TestTempDir + 'basic';
  Component.ArchiveType := atZip;
  Component.ForceType := True;
  Component.FileName := TestFileName;
  Component.BaseDirectory := TestFileDir;
  Component.AddFiles('*.*',faAnyFile);
  Component.Save;
  CheckFileExists(TestFileName);
end;

procedure TAbZipperTests.TestComponentLinks;
begin
  TestComponentLink(Component, 'ArchiveProgressMeter', TAbTestMeter);
  TestComponentLink(Component, 'ArchiveSaveProgressMeter', TAbTestMeter);
  TestComponentLink(Component, 'ItemProgressMeter', TAbTestMeter);
end;

procedure TAbZipperTests.TestDefaultStreaming;
begin
  inherited TestDefaultStreaming(Component);
end;

procedure TAbZipperTests.CreatePasswordProtectedAddedByStream;
var
  oFileStream : TFileStream;
  sZipFile : string;
begin
  // Remove the path from the zip
  Component.StoreOptions := [soStripPath];

  sZipFile := TestTempDir +  'PWStream.zip';

  // Create the directory if it doesn't exist
  {$IFDEF DELPHI5} // ForceDirectories not part of Delphi 4 so assume created by hand
  Check(ForceDirectories(TestTempDir),'Unable to create Test Temp directory.');
  {$ELSE}
  Check(DirectoryExists(TestTempDir),'Test Temp Directory needs to be created.');
  {$ENDIF}

  // File we need to zip
  Component.FileName := sZipFile;

  // Password protect the source file
  //  Component.Password := 'password';

  oFileStream := TFileStream.Create(MPLDir + 'MPL-1_1.txt', fmOpenRead or fmShareDenyNone);
  try
    // Add file to the zip file
   Component.AddFromStream('file.ext', oFileStream);
   Component.Save;
  finally
    oFileStream.Free;
  end;
  CheckFileExists(sZipFile);
end;

procedure TAbZipperTests.GZipInputStreamClearTest;
  // [ 820489 ] CloseArchive does not close input stream
var
  fs,fs2 : TFileStream;
  extractFilename, filename : string;
  unzip : TAbUnZipper;
begin
  fs := TFileStream.Create(MPLDir + 'MPL-1_1.txt',fmOpenRead);
  try
    filename := TestTempDir + 'clearinputstr.gz';
    if FileExists(filename) then
      DeleteFile(fileName);
    Component.ForceType := true;
    Component.ArchiveType := atGzip;
    Component.FileName := FileName;
    Fs.Position := 0;
    Component.AddFromStream('', fs);
    Component.Save;
    Component.CloseArchive;
    if FileExists(filename) then
      DeleteFile(fileName);
    Component.ForceType := true;
    Component.ArchiveType := atGzip;
    Component.FileName := FileName;
    Fs.Position := 0;
    Component.AddFromStream('', fs);
    Component.Save;
    Component.CloseArchive;
    //Check Archive for match
    unzip := TAbUnZipper.Create(nil);
    try
      extractFilename := TestTempDir + 'extractmpl.txt';
      unzip.FileName := filename;
      unzip.ExtractAt(0,extractFileName);
    finally
      unzip.free;
    end;
    fs2 := TFileStream.Create(extractFilename, fmOpenRead);
    try
      CheckStreamMatch(fs, fs2, 'Extracted file does not match original');
    finally
      fs2.free;
    end;
  finally
    fs.free;
  end;
end;


procedure TAbZipperTests.CreateSimplePWZip;
var
  TestFile : string;
begin
  // This is to address a problem where archives where not created at all
  // when a password is used.   It is not designed to test if the archive
  // is extractable.   Another test should be written for that.
  TestFile := TestTempDir + 'simplepw.zip';
  if FileExists(TestFile) then
    DeleteFile(TestFile);
  Component.Password := 'simple';
  Component.StoreOptions := [];
  Component.BaseDirectory := MPLDir;
  Component.FileName := TestFile;
  Component.AddFiles('MPL-1_1.txt',0);
  Component.Save;
  Component.CloseArchive;

  //Current Actual Size 9151 (Could change if we change default compresion so not testing for it)
  Check(AbFileGetSize(TestFile) > 8000, TestFile + ' too small check if created correctly');
end;

procedure TAbZipperTests.CreateMultiple;
var
  I : Integer;
  SL : TStringList;
begin
  SL := TStringList.Create;
  SL.Add('Test File Test File Test File Test File');
  SL.Add('Test File Test File Test File Test File');
  SL.Add('Test File Test File Test File Test File');
  SL.Add('Test File Test File Test File Test File');
  SL.Add('Test File Test File Test File Test File');
  SL.Add('Test File Test File Test File Test File');
  SL.Add('Test File Test File Test File Test File');
  for I := 0 to 30 do begin
    SL.SaveToFile(TestTempDir + 'multi' + IntToStr(I) + '.txt');
    Component.DeflationOption := doMaximum;
    Component.BaseDirectory := TestTempDir;
    Component.FileName := TestTempDir + 'multi' + IntToStr(I) + '.zip';
    Component.AddFiles(TestTempDir + 'multi' + IntToStr(I) + '.txt',0);
    Component.Save;
    Component.CloseArchive;
  end;
  Check(True); //TODO: Replace this with a proper test
end;

procedure TAbZipperTests.TestLocale1;
var
  ltestdir, ltestfile : string;
begin
  // This test verifies use Ability to use Charactes such as ãëíõú
  // In the Archive Directory Name

  // 236 changes into a ? on my machine in the delphi editor
  // so I thought it would be a good character to test with

  ltestdir := TestTempDir  + chr(236) + 'ãëíõú';
  ForceDirectories(ltestDir);
  ltestFile := lTestdir + PathDelim + 'locale1.zip';

  if FileExists(lTestFile) then
    DeleteFile(lTestFile);

  Component.FileName := lTestFile;
  Component.BaseDirectory := MPLDir;
  Component.AddFiles('MPL-1_1.txt',0);
  Component.Save;

  CheckFileExists(lTestFile);
end;

procedure TAbZipperTests.TestLocale2;
var
  ltestdir, ltestfile : string;
begin
  // This test verifies use Ability to use Charactes such as ãëíõú
  // In the Archive File Name
  ltestdir := TestTempDir;
  ForceDirectories(ltestDir);

  // 236 changes into a ? on my machine in the delphi editor
  // so I thought it would be a good character to test with
  ltestFile := lTestdir   + chr(236)+ 'ãëíõú.zip';

  if FileExists(lTestFile) then
    DeleteFile(lTestFile);

  Component.FileName := lTestFile;
  Component.BaseDirectory := MPLDir;
  Component.AddFiles('MPL-1_1.txt',0);
  Component.Save;

  CheckFileExists(lTestFile);
end;

procedure TAbZipperTests.TestLocale3;
var
  lBaseDir, ltestdir, ltestfile : string;
begin
  // This test verifies use Ability to use Charactes such as ãëíõú
  // In the BaseDirectory

  ltestdir := TestTempDir;
  ForceDirectories(ltestDir);
  ltestFile := lTestdir + 'locale3.zip';
  if FileExists(lTestFile) then
    DeleteFile(lTestFile);

  // 236 changes into a ? on my machine in the delphi editor
  // so I thought it would be a good character to test with
  lBaseDir := TestTempDir + chr(236) + 'ãëíõú' + PathDelim;
  ForceDirectories(lBaseDir);
  CreateDummyFile(lBaseDir + 'test1.lc3', 4000);
  CreateDummyFile(lBaseDir + 'test2.lc3', 6000);
  CreateDummyFile(lBaseDir + 'test3.lc3', 1000);

  Component.FileName := lTestFile;
  Component.BaseDirectory := lBaseDir;
  Component.AddFiles('*.lc3',0);
  Component.Save;

  CheckFileExists(lTestFile);
end;

procedure TAbZipperTests.TestLocale4;
var
  ltestdir : string;
  ltestfile : string;
  lBaseDir : string;
begin
// This test verifies use Ability to use Charactes such as ãëíõú
// In the Zip Archive Files (Base directory also has character in it)

  ltestdir := TestTempDir;
  ForceDirectories(ltestDir);
  ltestFile := lTestdir + 'locale4.zip';
  if FileExists(lTestFile) then
     DeleteFile(lTestFile);

  //236 changes into a ? on my machine in the delphi editor
  // so I thought it would be a good character to test with
  lBaseDir := TestTempDir + chr(236) + 'ãëíõú' + PathDelim;
  ForceDirectories(lBaseDir);
  CreateDummyFile(lBaseDir + 'testãëíõú1.lc4',4000);
  CreateDummyFile(lBaseDir + 'testãëíõú2.lc4',6000);
  CreateDummyFile(lBaseDir + 'testãëíõú3.lc4',1000);

  Component.FileName := lTestFile;
  Component.BaseDirectory := lBaseDir;
  Component.AddFiles('*.lc4',0);
  Component.Save;

  CheckFileExists(lTestFile);
end;


initialization

  TestFramework.RegisterTest('Abbrevia.Component Level Test Suite',
    TAbZipperTests.Suite);

end.

