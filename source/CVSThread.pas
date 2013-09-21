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

unit CVSThread;

interface

uses
{$IFDEF WIN32}
  Classes, Windows, SysUtils, StrUtils, utils;
{$ENDIF}
{$IFDEF LINUX}
  Classes, SysUtils, StrUtils, utils;
{$ENDIF}

type
  TLineOutputEvent = procedure(Sender: TObject; const Line: string) of object;
  TNeedPasswordEvent = procedure(var Passwd: string) of object;

  TCVSThread = class(TThread)
  private
    CurrentLine: string;
    fPasswd: string;
    FLineOutput: TLineOutputEvent;
    fCheckAbort: TCheckAbortFunc;
    fNeedPassword: TNeedPasswordEvent;
    procedure TypePassword(Pass: string);
  protected
    procedure Execute; override;
    procedure CallLineOutputEvent;
    procedure LineOutput(Line: string);
    procedure CallNeedPassword;
    function RunCVSCommand(WindowTitle, Cmd, WorkingDir: string): string;
  public
    Command: string;
    Directory: string;
    Output: string;
    property OnLineOutput: TLineOutputEvent read FLineOutput write FLineOutput;
    property OnCheckAbort: TCheckAbortFunc read FCheckAbort write FCheckAbort;
    property OnNeedPassword: TNeedPasswordEvent read fNeedPassword write fNeedPassword;
  end;

implementation

procedure TCVSThread.CallLineOutputEvent;
begin
  FLineOutput(Self, CurrentLine);
end;

procedure TCVSThread.CallNeedPassword;
begin
  fPasswd := '';
  if Assigned(fNeedPassword) then
    fNeedPassword(fPasswd);
end;

procedure TCVSThread.LineOutput(Line: string);
begin
  CurrentLine := Line;
  if Assigned(FLineOutput) then
    Synchronize(CallLineOutputEvent);
end;

procedure TCVSThread.Execute;
begin
  Output := RunCVSCommand('Dev-C++ CVS process', Command, Directory);
end;

function TCVSThread.RunCVSCommand(WindowTitle, Cmd, WorkingDir: string): string;
var
  tsi: TStartupInfo;
  tpi: TProcessInformation;
  nRead: DWORD;
//  hWin: HWND;
  aBuf: array[0..4095] of char;
  sa: TSecurityAttributes;
  hOutputReadTmp, hOutputRead, hOutputWrite, hErrorWrite: THandle;
//  hInputWriteTmp, hInputRead, hInputWrite: THandle;
  FOutput: string;
  CurrentLine: string;
  bAbort: boolean;
  idx: integer;
begin
  FOutput := '';
  CurrentLine := '';
  sa.nLength := SizeOf(TSecurityAttributes);
  sa.lpSecurityDescriptor := nil;
  sa.bInheritHandle := True;

  CreatePipe(hOutputReadTmp, hOutputWrite, @sa, 0);
  DuplicateHandle(GetCurrentProcess(), hOutputWrite, GetCurrentProcess(),
    @hErrorWrite, 0, false, DUPLICATE_SAME_ACCESS);

// CL: removed all input redirection, causes too much troubles
 // CreatePipe(hInputRead, hInputWriteTmp, @sa, 0);

  // Create new output read handle and the input write handle. Set
  // the inheritance properties to FALSE. Otherwise, the child inherits
  // the these handles; resulting in non-closeable handles to the pipes
  // being created.
  DuplicateHandle(GetCurrentProcess(), hOutputReadTmp, GetCurrentProcess(),
    @hOutputRead, 0, false, DUPLICATE_SAME_ACCESS);
  //DuplicateHandle(GetCurrentProcess(), hInputWriteTmp, GetCurrentProcess(),
  //  @hInputWrite, 0, false, DUPLICATE_SAME_ACCESS);
  CloseHandle(hOutputReadTmp);
  //CloseHandle(hInputWriteTmp);

  FillChar(tsi, SizeOf(TStartupInfo), 0);
  tsi.cb := SizeOf(TStartupInfo);
  tsi.lpTitle := PChar(WindowTitle);
  tsi.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
  //tsi.hStdInput := hInputRead;
  tsi.hStdOutput := hOutputWrite;
  tsi.hStdError := hOutputWrite;
  tsi.wShowWindow := SW_SHOW;//SW_HIDE;

  if not CreateProcess(nil, PChar(Cmd), @sa, @sa, true, CREATE_NEW_CONSOLE, nil, PChar(WorkingDir),
    tsi, tpi) then begin
    LineOutput('unable to run program file: ' + SysErrorMessage(GetLastError));
    exit;
  end;
  CloseHandle(hOutputWrite);
 // CloseHandle(hInputRead);
  CloseHandle(hErrorWrite);

  repeat
    if Assigned(fCheckAbort) then
      fCheckAbort(bAbort);
    if bAbort then
      Break;

    FillChar(aBuf, sizeof(aBuf), 0);
    if (not ReadFile(hOutputRead, aBuf, sizeof(aBuf), nRead, nil)) or (nRead = 0) then
    begin
      if GetLastError = ERROR_BROKEN_PIPE then
        Break
      else
        LineOutput('Pipe read error, could not execute command');
    end;
    aBuf[nRead] := #0;
    FOutput := FOutput + PChar(@aBuf[0]);

    CurrentLine := CurrentLine + PChar(@aBuf[0]);
    repeat
      idx := Pos(#10, CurrentLine);
      if idx > 0 then begin
        LineOutput(Copy(CurrentLine, 1, idx - 1));
        Delete(CurrentLine, 1, idx);
      end;
    until idx = 0;

  {  if AnsiEndsText('password:', Trim(FOutput)) then begin
      LineOutput(CurrentLine);
      CurrentLine := '';
      Synchronize(CallNeedPassword);
      hWin := FindWindow(nil, PChar(WindowTitle));
      if hWin > 0 then begin
        ShowWindow(hWin, SW_SHOW);
        SetForegroundWindow(hWin);
        TypePassword(fPasswd);
        ShowWindow(hWin, SW_HIDE);
      end
      else begin
        LineOutput('Failed to supply password...');
        TerminateProcess(tpi.hProcess, 65534);
        Break;
      end;
    end;}
//    else begin
//      CurrentLine := CurrentLine + PChar(@aBuf[0]);
//      if CurrentLine[Length(CurrentLine)] = #10 then
//      begin
//        Delete(CurrentLine, Length(CurrentLine), 1);
//        LineOutput(CurrentLine);
//        CurrentLine := '';
//      end;
//    end;
  until False;
  if bAbort then
    TerminateProcess(tpi.hProcess, 65535);
  GetExitCodeProcess(tpi.hProcess, nRead);
  Result := FOutput + ' ' + IntToStr(nRead);
end;

procedure TCVSThread.TypePassword(Pass: string);
var
  Key: Byte;
  I: integer;
begin
  for I := 1 to Length(Pass) do begin
    Key := VkKeyScan(Pass[I]);
    keybd_event(LoByte(Key), 0, 0, 0);
    keybd_event(LoByte(Key), 0, KEYEVENTF_KEYUP, 0);
  end;
{$IFDEF WIN32}
  keybd_event(VK_RETURN, 0, 0, 0);
  keybd_event(VK_RETURN, 0, KEYEVENTF_KEYUP, 0);
{$ENDIF}
{$IFDEF LINUX}
  keybd_event(XK_RETURN, 0, 0, 0);
  keybd_event(XK_RETURN, 0, KEYEVENTF_KEYUP, 0);
{$ENDIF}
end;

end.

