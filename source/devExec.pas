{
    This file is part of Dev-C++
    Copyright (c) 2004 Bloodshed Software

    Dev-C++ is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Dev-C++ is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Dev-C++; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit devExec;

interface

uses 
{$IFDEF WIN32}
  Windows, Classes;
{$ENDIF}
{$IFDEF LINUX}
  Classes;
{$ENDIF}

type
  TExecThread = class(TThread)
  private
    fFile: string;
    fPath: string;
    fParams: string;
    fTimeOut: Cardinal;
    fProcess: Cardinal;
    fVisible: boolean;
    procedure ExecAndWait;
  public
    procedure Execute; override;
  published
    property FileName: string read fFile write fFile;
    property Path: string read fPath write fPath;
    property Params: string read fParams write fParams;
    property TimeOut: Cardinal read fTimeOut write fTimeOut;
    property Visible: boolean read fVisible write fVisible;
    property Process: Cardinal read fProcess;
  end;

  TdevExecutor = class(TPersistent)
  private
    fExec: TExecThread;
    fIsRunning: boolean;
    fOnTermEvent: TNotifyEvent;
    procedure TerminateEvent(Sender: TObject);
  public
    class function devExecutor: TdevExecutor;
    procedure Reset;
    procedure ExecuteAndWatch(sFileName, sParams, sPath: string; bVisible: boolean; iTimeOut: Cardinal; OnTermEvent: TNotifyEvent);
  published
    property Running: boolean read fIsRunning;
  end;


function devExecutor: TdevExecutor;

implementation

{ TExecThread }

procedure TExecThread.Execute;
begin
  inherited;
  ExecAndWait;
end;

procedure TExecThread.ExecAndWait;
// Author    : Francis Parlant.
// Update    : Bill Rinko-Gay
// Adaptation: Yiannis Mandravellos
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
  with StartupInfo do begin
    cb := SizeOf(TStartupInfo);
    dwFlags := STARTF_USESHOWWINDOW or STARTF_FORCEONFEEDBACK;
    if fVisible then
      wShowWindow := SW_SHOW
    else
      wShowWindow := SW_HIDE;
  end;
  if CreateProcess(nil, PChar(fFile + ' ' + fParams), nil, nil, False,
    NORMAL_PRIORITY_CLASS, nil, PChar(fPath),
    StartupInfo, ProcessInfo) then begin
    fProcess := ProcessInfo.hProcess;
    WaitForSingleObject(ProcessInfo.hProcess, fTimeOut);
  end;
  CloseHandle(ProcessInfo.hProcess);
  CloseHandle(ProcessInfo.hThread);
end;

var
  fDevExecutor: TdevExecutor;

function devExecutor: TdevExecutor;
begin
  if not Assigned(fDevExecutor) then begin
    try
      fDevExecutor := TdevExecutor.Create;
    finally
    end;
  end;
  Result := fDevExecutor;
end;

{ TdevExecutor }

class function TdevExecutor.devExecutor: TdevExecutor;
begin
  Result := devExec.devExecutor;
end;

procedure TdevExecutor.ExecuteAndWatch(sFileName, sParams, sPath: string;
  bVisible: boolean; iTimeOut: Cardinal; OnTermEvent: TNotifyEvent);
begin
  fIsRunning := True;
  fOnTermEvent := OnTermEvent;
  fExec := TExecThread.Create(True);
  with fExec do begin
    FileName := sFileName;
    Params := sParams;
    Path := sPath;
    TimeOut := iTimeOut;
    Visible := bVisible;
    OnTerminate := TerminateEvent;
    FreeOnTerminate := True;
    Resume;
  end;
end;

procedure TdevExecutor.Reset;
begin
  if Assigned(fExec) then
    TerminateProcess(fExec.Process, 0);
  fIsRunning := False;
end;

procedure TdevExecutor.TerminateEvent(Sender: TObject);
begin
  fIsRunning := False;
  if Assigned(fOnTermEvent) then
    fOnTermEvent(Self);
end;

end.

