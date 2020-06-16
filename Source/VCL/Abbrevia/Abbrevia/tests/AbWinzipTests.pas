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
 * Craig Peterson
 *
 * ***** END LICENSE BLOCK ***** *)

unit AbWinzipTests;

{$I AbDefine.inc}

interface

uses
  SyncObjs, TestFramework, AbTestFramework;

type
  TAbWinzipTests = class(TAbTestCase)
  private
    FSpawnComplete  : TSimpleEvent;
    procedure SpawnErrorEvent(Sender : TObject; Error : Word);
    procedure SpawnCompletedEvent(Sender : TObject);
    procedure SpawnTimeOutEvent(Sender : TObject);
    procedure ExecuteAndWait(const ExeName, Param: string; TimeOut: Integer = 0); // Exception if failure
  published
    procedure WinzipExtractTest;
  end;

implementation

uses
  Classes,
  Controls,
  Windows,
  Forms,
  SysUtils,
  Dialogs,
  StSpawn;

{ Hard Coded as I could not find install location in the Registry to extract and
  make dynamic if this proves to be a problem, we will have to have test
  configuration file that specifies the winzip command line utility path. }
const
  UnWinZip = 'C:\Program Files\WinZip\wzunzip.exe';

procedure TAbWinzipTests.ExecuteAndWait(const ExeName, Param: string; TimeOut: Integer);
var
  Spawn : TStSpawnApplication;
  WR    : TWaitResult;
begin
   // Make sure Application trying to execute is found
   CheckFileExists(ExeName);
   Spawn := TStSpawnApplication.Create(nil);
   try
     Spawn.FileName := ExeName;
     Spawn.RunParameters := Param;
     Spawn.NotifyWhenDone := True;
     Spawn.TimeOut := TimeOut;
     Spawn.OnSpawnError := SpawnErrorEvent;
     Spawn.OnCompleted := SpawnCompletedEvent;
     Spawn.OnTimeOut := SpawnTimeOutEvent;
     Spawn.Execute;
     WR := FSpawnComplete.WaitFor(1000);
     While WR <> wrSignaled do
       begin
          Application.ProcessMessages;
          Check(NOT (WR = wrAbandoned), 'Event has been Abandoonded');
          Check(NOT (WR = wrError),'Event has Errored out');
          WR := FSpawnComplete.WaitFor(1000);
       end;
   finally
      Spawn.Free;
   end;
end;

procedure TAbWinzipTests.SpawnCompletedEvent(Sender: TObject);
begin
  FSpawnComplete.SetEvent;
end;

procedure TAbWinzipTests.SpawnErrorEvent(Sender: TObject; Error: Word);
begin
 FSpawnComplete.SetEvent;
 Fail('Error: ' + IntToSTr(Error) + ' occured launching WinZip');
end;

procedure TAbWinzipTests.SpawnTimeOutEvent(Sender: TObject);
begin
 FSpawnComplete.SetEvent;
 Fail('Timeout occured launching WinZip');
end;

procedure TAbWinzipTests.WinzipExtractTest;
var
  ExtractTo : string;
  FileList : TStringList;
  I : integer;
begin
  // This test will use the Winzip command line utility to determine if the
  // file created in CreateBasicSpan can be extracted.
  // This is a good comptability routine.

  if MessageDlg('This test requires the TESTSPAN.ZIP created in the '+#13+#10+'"CreateBasicSpan" test. '+ #13#10 + 'It also requires WinZIP Command Line Utility. '+#13#10#13#10 +'Please Insert LAST Disk of span set.'+#13+#10+''+#13+#10+'Pressing Cancel will terminate this test.', mtInformation, [mbOK,mbCancel], 0) = mrCancel then
    Fail('Test Aborted');

  ExtractTo := TestTempDir + 'WZSpan\';
  CreateDir(ExtractTo);
  ExecuteAndWait(UnWinZip, 'A:\SPANTEST.ZIP ' + ExtractTo);

  // Files have now been extracted Time to test.
  FileList := FilesInDirectory(ExtractTo);
  try
    // Make sure files where extracted
    Check(Filelist.Count > 0, 'Unable to find any extract files');
    for I := 0 to FileList.Count -1 do
      CheckFilesMatch(GetWindowsDir + FileList[I], ExtractTo + FileList[I], FileList[I] + ' did not match master file');
  finally
    FileList.free;
  end;
end;

end.