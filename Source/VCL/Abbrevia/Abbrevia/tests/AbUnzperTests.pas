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

unit AbUnzperTests;

{$I AbDefine.inc}

interface

uses
  AbTestFrameWork, AbUnzper;

type

  TAbUnZipperTests = class(TAbCompTestCase)
  private
    Component : TAbUnZipper;
    FPasswordTry : Integer;
    procedure NeedPassword3Tries(Sender : TObject; var NewPassword : string);

  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure TestUserAbortProgress(Sender : TObject; Progress : Byte; var Abort : Boolean);
  published
    procedure TestDefaultStreaming;
    procedure TestComponentLinks;
    procedure TestBasicUnzip;
    procedure TestBasicUnGzip;
    procedure TestBasicUnGzipTar;
    procedure TestUserAbort;
    procedure TestGZipDecompress;
    procedure TestZeroByteZipFile;
    procedure TestIncompleteZipFile;
    procedure TestArchiveNotFound;
    procedure TestZipCopiedFromFloppy;
    procedure DecompressSimplePW;
    procedure DecompressPasswordTries;
    procedure CheckBadPWOverwrite;
    {$IFNDEF DARWIN} // TODO: OS X uses Unicode normalization form D;  the tests need to be adapted
    procedure TestLocale1;
    procedure TestLocale2;
    procedure TestLocale3;
    procedure TestLocale4;
    {$ENDIF}
    procedure TestFindCentralDirTail;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  SysUtils, Classes, TestFrameWork,
  AbArcTyp, AbDfBase, AbExcept, AbUtils, AbZipTyp;

{ TAbUnZipperTests }

procedure TAbUnZipperTests.CheckBadPWOverwrite;
// [ 698162 ] Failed unzipping of password protected files clobbers original
var
  Fs : TFileStream;
  TestFile : String;
  Buffer, Buffer1 : array[0..20] of Char;
begin
  TestFile := TestTempDir + 'MPL-1_1.txt';
  // Create Dummy file to test with.
  FillChar(Buffer, SizeOf(Buffer),#0);
  Buffer := 'THIS IS A TEST';
  Fs := TFileStream.Create(TestFile, fmCreate);
  try
    Fs.Write(Buffer, SizeOf(Buffer)); // Write something to file
  finally
    Fs.Free;
  end;
  // Try to extract with wrong password
  Component.FileName := TestFileDir + 'simplepw.zip';
  Component.Password := 'wrong password';
  Component.BaseDirectory := TestTempDir;
  try
    Component.ExtractFiles('*.*'); // MPL-1_1.TXT
  except
    on EAbInflatePasswordError do
      // nothing Expected
  end;

  CheckFileExists(TestFile);

  // Test to make sure file matches dummy original
  Fs := TFileStream.Create(TestFile,fmOpenRead);
  try
    FS.Read(Buffer1,SizeOf(Buffer1));
    CompareMem(@Buffer[0],@Buffer[1],SizeOf(Buffer));
  finally
    Fs.free;
  end;
end;

procedure TAbUnZipperTests.DecompressSimplePW;
var
  MS : TMemoryStream;
begin
  MS := TMemoryStream.Create;
  try
    Component.FileName := TestFileDir + 'simplepw.zip';
    Component.Password := 'simple';
    Component.ExtractOptions := [];
    Component.ExtractToStream(Component.Items[0].FileName, MS);
    CheckFileMatchesStream(MPLDir + 'MPL-1_1.txt', MS,
      'simplepw.zip MPL-1_1.txt does not match original');
  finally
    MS.free;
  end;
end;

procedure TAbUnZipperTests.NeedPassword3Tries(Sender : TObject; var NewPassword : string);
begin
  Inc(FPasswordTry);
  case FPasswordTry of
    1: NewPassword := 'invalid1';
    2: NewPassword := 'invalid2';
    3: NewPassword := 'simple';
    else Fail('NeedPassword called too many times');
  end;
end;

procedure TAbUnZipperTests.DecompressPasswordTries;
var
  MS: TMemoryStream;
begin
  FPasswordTry := 0;
  MS := TMemoryStream.Create;
  try
    Component.FileName := TestFileDir + 'simplepw.zip';
    Component.PasswordRetries := 3;
    Component.OnNeedPassword := NeedPassword3Tries;
    Component.ExtractToStream(Component.Items[0].FileName, MS);
    CheckEquals(3, FPasswordTry, 'NeedPassword not called enough times');
    CheckFileMatchesStream(MPLDir + 'MPL-1_1.txt', MS, '');
  finally
    MS.Free;
  end;
end;

procedure TAbUnZipperTests.SetUp;
begin
  inherited;
  Component := TAbUnzipper.Create(nil);
end;

procedure TAbUnZipperTests.TearDown;
begin
  Component.Free;
  inherited;
end;


procedure TAbUnZipperTests.TestArchiveNotFound;
begin
 ExpectedException := EAbFileNotFound;
 // Try to set the filename to the open byte file.
 Component.FileName := TestFileDir + 'nonexist.zip';
end;

procedure TAbUnZipperTests.TestBasicUnGzip;
begin
  Component.BaseDirectory := TestTempDir;
  Component.FileName := MPLDir + 'MPL.gz';
  Component.ExtractFiles('*.*');
  CheckFilesMatch(TestTempDir + 'MPL-1_1.txt', MPLDir + 'MPL-1_1.txt');
end;

procedure TAbUnZipperTests.TestBasicUnGzipTar;
begin
  Component.BaseDirectory := TestTempDir;
  Component.TarAutoHandle := True;
  Component.FileName := MPLDir + 'MPL.tgz';
  Component.ExtractFiles('*.*');
  CheckFilesMatch(TestTempDir + 'MPL-1_1.txt', MPLDir + 'MPL-1_1.txt');
end;

procedure TAbUnZipperTests.TestBasicUnzip;
begin
  Component.BaseDirectory := TestTempDir;
  Component.FileName := MPLDir + 'MPL.zip';
  Component.ExtractFiles('*.*');
  CheckFilesMatch(TestTempDir + 'MPL-1_1.txt', MPLDir + 'MPL-1_1.txt');
end;

procedure TAbUnZipperTests.TestComponentLinks;
begin
  TestComponentLink(Component, 'ArchiveProgressMeter', TAbTestMeter);
  TestComponentLink(Component, 'ItemProgressMeter', TAbTestMeter);
end;

procedure TAbUnZipperTests.TestDefaultStreaming;
begin
  inherited TestDefaultStreaming(Component);
end;

procedure TAbUnZipperTests.TestGZipDecompress;
begin
  Check(True); //TODO: Fix this bug
  Exit;
  // [ 822243 ] GZip decompress problem
  Component.FileName := TestFileDir + 'download[1].datalandsoftware.com-Aug-2003.gz';
  Component.BaseDirectory:= TestTempDir;
  Component.ExtractAt(0, TestTempDir + 'dataland.txt');
  CheckFilesMatch(TestFileDir + 'dataland.txt', TestTempDir + 'dataland.txt');
end;

procedure TAbUnZipperTests.TestIncompleteZipFile;
var
  FS : TFileStream;
begin
  // Create a file that only contains the initial signature and not the 
  // central directory tail structure, which should not be recognized.
  ExpectedException := EAbUnhandledType;
  FS := TFileStream.Create(TestTempDir + 'dummy.zip', fmCreate);
  try
    FS.Write(Ab_ZipLocalFileHeaderSignature, SizeOf(Ab_ZipLocalFileHeaderSignature));
  finally
    FS.Free;
  end;
  // Try to set the filename to the open byte file.
  Component.FileName := TestTempDir + 'dummy.zip';
end;

{$IFNDEF DARWIN}
procedure TAbUnZipperTests.TestLocale1;
var
 ltestdir,
 ltestzip,
 ltestfile : string;
begin
// This test verifies use Ability to use Charactes such as ãëíõú
// In the Archive Directory Name

  // Create New Directory
  //236 changes into a ? on my machine in the delphi editor
  // so I thought it would be a good character to test with
  ltestdir := TestTempDir  + chr(236) + 'ãëíõú' + PathDelim;
  ForceDirectories(ltestDir);

  // copy fresh MPL.ZIP to locale1.zip in the new directory
  ltestFile := lTestdir + 'locale1.zip';
  if FileExists(lTestFile) then
     DeleteFile(lTestFile);
  ltestzip := MPLDir + 'MPL.zip';
  AbCopyFile(ltestzip,ltestFile,false);


  Component.FileName := lTestFile;
  Component.BaseDirectory := TestTempDir;
  // Delete File to be extract if it exists
  if FileExists(TestTempDir + 'MPL-1_1.txt') then
     DeleteFile(TestTempDir + 'MPL-1_1.txt');
  Component.ExtractFiles('*.*');
  Component.CloseArchive;

  CheckFileExists(TestTempDir + 'MPL-1_1.txt');

end;

procedure TAbUnZipperTests.TestLocale2;
var
 ltestdir,
 ltestfile : string;
begin
// This test verifies use Ability to use Charactes such as ãëíõú
// In the Base Directory Name

  // Create New Directory
  //236 changes into a ? on my machine in the delphi editor
  // so I thought it would be a good character to test with
  ltestdir := TestTempDir  + chr(236) + 'ãëíõú' + PathDelim;
  ForceDirectories(ltestDir);

  ltestFile := MPLDir + 'MPL.zip';

  Component.FileName := lTestFile;
  Component.BaseDirectory := lTestDir;
  // Delete File to be extract if it exists
  if FileExists(lTestDir + 'MPL-1_1.txt') then
     DeleteFile(lTestDir + 'MPL-1_1.txt');
  Component.ExtractFiles('*.*');
  Component.CloseArchive;

  CheckFileExists(lTestDir + 'MPL-1_1.txt');
end;

procedure TAbUnZipperTests.TestLocale3;
var
 ltestzip,
 ltestfile : string;
begin
// This test verifies use Ability to use Charactes such as ãëíõú
// In the Archive File Name

  // copy fresh MPL.ZIP to localeãëíõú3.zip in the temp directory
  ltestFile := TestTempDir + 'localeãëíõú3.zip';
  if FileExists(lTestFile) then
     DeleteFile(lTestFile);
  ltestzip := MPLDir + 'MPL.zip';
  AbCopyFile(ltestzip,ltestFile,false);


  Component.FileName := lTestFile;
  Component.BaseDirectory := TestTempDir;
  // Delete File to be extract if it exists
  if FileExists(TestTempDir + 'MPL-1_1.txt') then
    DeleteFile(TestTempDir + 'MPL-1_1.txt');
  Component.ExtractFiles('*.*');
  Component.CloseArchive;

  CheckFileExists(TestTempDir + 'MPL-1_1.txt');

end;

procedure TAbUnZipperTests.TestLocale4;
begin
// This test verifies use Ability to use Charactes such as ãëíõú
// In the Files contained in the Archive.

  Component.FileName := TestFileDir + 'LocaleTests.zip';
  Component.BaseDirectory := TestTempDir;

  // Delete Files in Temp Directory if they exist
  if FileExists(TestTempDir + 'testãëíõú1.lc4') then
     DeleteFile(TestTempDir + 'testãëíõú1.lc4');
  if FileExists(TestTempDir + 'testãëíõú2.lc4') then
     DeleteFile(TestTempDir + 'testãëíõú2.lc4');
  if FileExists(TestTempDir + 'testãëíõú3.lc4') then
     DeleteFile(TestTempDir + 'testãëíõú3.lc4');

  Component.ExtractFiles('*.lc4');
  Component.CloseArchive;

  CheckFileExists(TestTempDir + 'testãëíõú1.lc4');
  CheckFileExists(TestTempDir + 'testãëíõú2.lc4');
  CheckFileExists(TestTempDir + 'testãëíõú3.lc4');

end;
{$ENDIF}

procedure TAbUnZipperTests.TestUserAbort;
var
  UnZipper : TAbUnZipper;
begin
  // This test needs to create and free the component as the problem is in the FREE.
  // Inspired by, but not testing for the same thing as SF.Net Tracker ID [785269]

  // Expecting this exception don't fail if it occurs.
  ExpectedException := EAbUserAbort;

  UnZipper := TAbUnZipper.Create(Nil);
  try
    UnZipper.BaseDirectory := TestTempDir;
    UnZipper.OnArchiveProgress:= TestUserAbortProgress;
    UnZipper.FileName:= MPLDir + 'MPL.zip';
    UnZipper.ExtractFiles('*.*');
  finally
    UnZipper.Free;
  end;
end;

procedure TAbUnZipperTests.TestUserAbortProgress(Sender: TObject;
  Progress: Byte; var Abort: Boolean);
begin
  Abort := (Progress > 25);
end;

procedure TAbUnZipperTests.TestZeroByteZipFile;
var
  FS : TFileStream;
begin
  ExpectedException := EAbBadStream;
  // Delete File if it exists
  if FileExists(TestTempDir + 'zerobyte.zip') then
    DeleteFile(TestTempDir + 'zerobyte.zip');
  // Create Zero Byte File
  FS := TFileStream.Create(TestTempDir + 'zerobyte.zip',fmCreate);
  FS.Free;
  // Try to set the filename to the open byte file.
  Component.FileName := TestTempDir + 'zerobyte.zip';
end;

procedure TAbUnZipperTests.TestZipCopiedFromFloppy;
begin
  //[ 858945 ] copy of File saved to floppy cannot be opened
  // Error of File Not Found appears if it fails.
  Component.FileName := TestFileDir + 'CpFromFloppy.zip';
  Component.BaseDirectory := TestTempDir;
  Component.ExtractFiles('*.*');
  Check(True); //TODO: Replace this with a proper test
end;

procedure TAbUnZipperTests.TestFindCentralDirTail;
{ Test finding the central directory tail when it spans a search block }
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(TestFileDir + 'FindCDTailBug.zip', fmOpenRead);
  try
    Check(VerifyZip(FS) = atZip);
  finally
    FS.Free;
  end;
end;

initialization

  TestFramework.RegisterTest('Abbrevia.Component Level Test Suite',
    TAbUnZipperTests.Suite);

end.

