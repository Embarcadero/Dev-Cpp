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
 * Craig Peterson <capeterson@users.sourceforge.net>
 *
 * ***** END LICENSE BLOCK ***** *)

unit AbFloppySpanTests;

{$I AbDefine.inc}

interface

// When run on a system without a floppy drive these tests require the
// ImDisk virtual disk driver, available from http://www.ltr-data.se/opencode.html#ImDisk
// This is really the preferred method to test, since it doesn't require any
// user interaction.  ImDisk runs on WinNT4-Win7 on both 32-bit and 64-bit releases.
//
// TestCompress/TestDecompress are written to need 3 320KB disks.  If using a real
// floppy drive use "format /Q /F:320 /FS:FAT a:" to format them to the right size.

uses
// Note: The Floppy Span tests are designed to be platform specific
  Windows,
  AbArcTyp, AbUtils,
  AbTestFramework;

type
  TAbRequestedDisk = (rdBlank, rdDisk1, rdDisk2, rdDisk3, rdLast);

  TAbFloppySpanTests = class(TabTestCase)
  private
    FDiskInserted: Boolean;
    FErrorMode: UINT;
    FNextDiskIndex: Integer;
    FExpectedRequest: TAbRequestedDisk;

    procedure CheckCopyFile(const aSrc, aDes: string);
    procedure EjectDisk;
    procedure InsertDisk(aDiskNumber: Integer; aReadOnly: Boolean = False);
    procedure InsertBlankDisk(aReadOnly: Boolean = False);
    function GetDiskFilename(aIndex: Integer): string;

    procedure DecompressSpan;

    // Events for TestDecompress/TestCompress
    procedure CheckRequest(aActual: TAbRequestedDisk);
    procedure DoRequestBlankDisk(Sender : TObject; var Abort : Boolean);
    procedure DoRequestLastDisk(Sender : TObject; var Abort : Boolean);
    procedure DoRequestNthDisk(Sender : TObject; DiskNumber : Byte;
      var Abort : Boolean);

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestDecompress;
    procedure TestCompress;
    procedure TestWriteProtectedMedia;
  end;

implementation

uses
  Classes, Controls, Dialogs, SysUtils,
  TestFrameWork,
  AbUnzper, AbZipper;

var
  ADriveType: Integer = -1;

{ -------------------------------------------------------------------------- }
function HasFloppy: Boolean;
begin
  if ADriveType = -1 then
    ADriveType := GetDriveType('a:');
  Result := (ADriveType <> DRIVE_NO_ROOT_DIR)
end;
{ -------------------------------------------------------------------------- }
procedure WinExecAndWait(CmdLine: string);
var
  pi : TProcessInformation;
  si : TStartupInfo;
  ExitCode: DWORD;
begin
  UniqueString(CmdLine);

  FillChar(si, SizeOf(si), 0);
  si.cb := SizeOf(TStartupInfo);
  si.wShowWindow := SW_HIDE;
  si.dwFlags := STARTF_USESHOWWINDOW;

  if CreateProcess(nil, PChar(CmdLine), nil, nil, False, CREATE_NEW_CONSOLE,
                   nil, nil, si, pi) then
    try
      WaitForSingleObject(pi.hProcess, INFINITE);
      if not GetExitCodeProcess(pi.hProcess, ExitCode) then
        raise Exception.Create('GetExitCodeProcess failed: ' + SysErrorMessage(GetLastError));
      if ExitCode <> 0 then
        raise Exception.CreateFmt('Process exited with a non-zero exit code (%d)', [ExitCode]);
    finally
      CloseHandle(pi.hProcess);
      CloseHandle(pi.hThread);
    end
  else
    raise Exception.Create('CreateProcess failed: ' + SysErrorMessage(GetLastError));
end;

{============================================================================}
{ TAbFloppySpanTests implementation ======================================== }
procedure TAbFloppySpanTests.SetUp;
begin
  inherited;
  FNextDiskIndex := 1;
  // Disable error dialogs for read-only media
	FErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS or SEM_NOOPENFILEERRORBOX);
end;
{ -------------------------------------------------------------------------- }
procedure TAbFloppySpanTests.TearDown;
begin
  if FDiskInserted and not HasFloppy then EjectDisk;
	SetErrorMode(FErrorMode);
  inherited;
end;
{ -------------------------------------------------------------------------- }
procedure TAbFloppySpanTests.CheckCopyFile(const aSrc, aDes: string);
begin
  Check(AbCopyFile(PChar(aSrc), PChar(aDes), True),
    Format('Copying %s to %s failed', [ExtractFileName(aSrc), ExtractFileName(aDes)]));
end;
{ -------------------------------------------------------------------------- }
procedure TAbFloppySpanTests.EjectDisk;
begin
  Assert(FDiskInserted, 'EjectDisk called on empty drive');
  WinExecAndWait('imdisk -D -m a:');
  FDiskInserted := False;
end;
{ -------------------------------------------------------------------------- }
procedure TAbFloppySpanTests.InsertDisk(aDiskNumber: Integer; aReadOnly: Boolean = False);
var Cmd: string;
begin
  if HasFloppy then
    ShowMessage(Format('Insert disk #%d into drive A:', [aDiskNumber]))
  else begin
    if FDiskInserted then EjectDisk;
    Cmd := Format('imdisk -a -f "%s" -m a: -o rem,fd', [GetDiskFilename(aDiskNumber)]);
    if aReadOnly then
      Cmd := Cmd + ',ro';
    WinExecAndWait(Cmd);
  end;
  FDiskInserted := True;
end;
{ -------------------------------------------------------------------------- }
procedure TAbFloppySpanTests.InsertBlankDisk(aReadOnly: Boolean = False);
begin
  if HasFloppy then
    if aReadOnly then
      ShowMessage('Insert a blank write protected disk into drive A:')
    else
      ShowMessage(Format('Insert blank disk #%d into drive A:', [FNextDiskIndex]))
  else begin
    CheckCopyFile(TestFileDir + 'Floppy.bin', GetDiskFilename(FNextDiskIndex));
    InsertDisk(FNextDiskIndex, aReadOnly);
  end;
  Inc(FNextDiskIndex);
end;
{ -------------------------------------------------------------------------- }
function TAbFloppySpanTests.GetDiskFilename(aIndex: Integer): string;
begin
  Result := Format(TestTempDir + 'Floppy#%d.bin', [aIndex]);
end;
{ -------------------------------------------------------------------------- }
procedure TAbFloppySpanTests.CheckRequest(aActual: TAbRequestedDisk);
const
  States: array[TAbRequestedDisk] of string =
    ('RequestBlankDisk', 'RequestNthDisk(1)', 'RequestNthDisk(2)',
     'RequestNthDisk(3)', 'RequestLastDisk');
begin
  Check(FExpectedRequest = aActual,
    Format('Requested disk mismatch.  Expected %s, actual %s',
    [States[FExpectedRequest], States[aActual]]));
end;
{ -------------------------------------------------------------------------- }
procedure TAbFloppySpanTests.DoRequestBlankDisk(Sender : TObject;
  var Abort : Boolean);
begin
  CheckRequest(rdBlank);
  InsertBlankDisk;
end;
{ -------------------------------------------------------------------------- }
procedure TAbFloppySpanTests.DoRequestLastDisk(Sender: TObject;
  var Abort: Boolean);
begin
  CheckRequest(rdLast);
  InsertDisk(3);
  FExpectedRequest := rdDisk1;
end;
{ -------------------------------------------------------------------------- }
procedure TAbFloppySpanTests.DoRequestNthDisk(Sender: TObject;
  DiskNumber: Byte; var Abort: Boolean);
begin
  CheckRequest(TAbRequestedDisk(Ord(rdBlank) + DiskNumber));
  InsertDisk(DiskNumber);
  Inc(FExpectedRequest);
end;
{ -------------------------------------------------------------------------- }
procedure TAbFloppySpanTests.DecompressSpan;
var
  Zip : TAbUnZipper;
  OutputDir : string;
begin
  OutputDir := TestTempDir + 'output';
  CreateDir(OutputDir);
  // Extract spanned zip
  Zip := TAbUnZipper.Create(nil);
  try
    Zip.OnRequestLastDisk := DoRequestLastDisk;
    Zip.OnRequestNthDisk := DoRequestNthDisk;
    Zip.BaseDirectory := OutputDir;
    Zip.FileName := 'a:\Spanned.zip';
    Zip.ExtractFiles('*');
  finally
    Zip.Free;
  end;
  CheckRequest(rdLast);
  CheckDirMatch(CanterburySourceDir, OutputDir);
end;
{ -------------------------------------------------------------------------- }
procedure TAbFloppySpanTests.TestDecompress;
begin
  // Initialize disk images
  InsertBlankDisk;
  AbWriteVolumeLabel('PKBACK#001', 'a');
  CheckCopyFile(CanterburyDir + 'Split' + PathDelim + 'Split.z01', 'a:\Spanned.zip');
  InsertBlankDisk;
  AbWriteVolumeLabel('PKBACK#002', 'a');
  CheckCopyFile(CanterburyDir + 'Split' + PathDelim + 'Split.z02', 'a:\Spanned.zip');
  InsertBlankDisk;
  AbWriteVolumeLabel('PKBACK#003', 'a');
  CheckCopyFile(CanterburyDir + 'Split' + PathDelim + 'Split.zip', 'a:\Spanned.zip');
  // Set initial state
  InsertDisk(1);
  FExpectedRequest := rdLast;

  DecompressSpan;
end;
{ -------------------------------------------------------------------------- }
procedure TAbFloppySpanTests.TestCompress;
var
  Zip: TAbZipper;
begin
  // Compress Canterbury corpus
  FExpectedRequest := rdBlank;
  InsertBlankDisk;
  Zip := TAbZipper.Create(nil);
  try
    Zip.OnRequestBlankDisk := DoRequestBlankDisk;
    Zip.BaseDirectory := CanterburySourceDir;
    Zip.FileName := 'a:\Spanned.zip';
    Zip.AddFiles('*', faAnyFile);
    Zip.Save;
  finally
    Zip.Free;
  end;
  // Decompress it
  FExpectedRequest := rdDisk1;
  DecompressSpan;
end;
{ -------------------------------------------------------------------------- }
procedure TAbFloppySpanTests.TestWriteProtectedMedia;
var
  Zip : TAbZipper;
begin
  ExpectedException := EFCreateError;
  InsertBlankDisk(True);

  Zip := TAbZipper.Create(nil);
  try
    Zip.BaseDirectory := MPLDir;
    Zip.FileName := 'a:\Spanned.zip';
    Zip.AddFiles('MPL-1_1.txt',faAnyFile);
    Zip.Save;
  finally
    Zip.Free;
  end;
end;
{ -------------------------------------------------------------------------- }

initialization

//The floppy tests only work if the image tool is installed
//so that I disabled them.
//  TestFramework.RegisterTest('Abbrevia.Floppy Spanning Suite', TAbFloppySpanTests.Suite);

end.

