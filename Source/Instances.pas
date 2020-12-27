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

unit Instances;

interface

uses
  Windows, Messages, Psapi, SysUtils, StrUtils, Classes;

function GetSentStructData(Message: TMessage): String;
procedure SendToPreviousInstance(Instance: THandle; const Data: String);
function GetPreviousInstanceCallback(Handle: THandle; Param: Integer): boolean; stdcall;
function GetPreviousInstance: THandle;
const
  SENDDATAID = 12345; // message used to inform Dev-C++ of an opening instance

implementation

uses
  main, VCL.Dialogs, VCL.Forms;

var
  PreviousInstance: THandle; // return value for GetPreviousInstanceCallback

function GetSentStructData(Message: TMessage): String;
var
  DataStruct: PCopyDataStruct;
begin
  DataStruct := PCopyDataStruct(Message.LParam);
  if Assigned(DataStruct) and (DataStruct^.dwData = SENDDATAID) then
    Result := PChar(DataStruct^.lpData)
  else
    Result := '';
end;

procedure SendToPreviousInstance(Instance: THandle; const Data: String);
var
  DataStruct: TCopyDataStruct;
  Buffer: PChar;
begin
  // Convert string to char array
  Buffer := StrAlloc(Length(Data) + 1);
  try
    StrPCopy(Buffer, Data);

    // Configure send struct
    DataStruct.dwData := SENDDATAID;
    DataStruct.cbData := SizeOf(Char) * (Length(Data) + 1);
    DataStruct.lpData := Buffer;

    // Send the send struct
    SendMessage(Instance,
      WM_COPYDATA,
      Application.Handle,
      Integer(@DataStruct));
  finally
    StrDispose(Buffer);
  end;
end;

function GetPreviousInstanceCallback(Handle: THandle; Param: Integer): boolean; stdcall;
var
  Buffer: array[0..511] of char;
  WindowModuleName, WindowClassName, CompareFileName: String;
  WindowModule, WindowProcess: THandle;
  ProcessID: Cardinal;
begin
  Result := True;

  // Get its class name (from WNDCLASSEX)
  if GetClassName(Handle, Buffer, SizeOf(Buffer)) = 0 then
    Exit;
  WindowClassName := Buffer;

  // Class names match
  if WindowClassName = TMainForm.ClassName then begin
    WindowModule := GetWindowLong(Handle, GWL_HINSTANCE);
    if WindowModule = 0 then
      Exit;

    // Get the ID of the process that created this window
    GetWindowThreadProcessId(Handle, @ProcessID);
    if ProcessID = 0 then
      Exit;

    // Get the process associated with the ID
    WindowProcess := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, ProcessID);
    if WindowProcess = 0 then
      Exit;

    // Get its module filename
    if GetModuleFileNameEx(WindowProcess, WindowModule, Buffer, SizeOf(Buffer)) = 0 then
      Exit;
    WindowModuleName := Buffer;
    CloseHandle(WindowProcess); // not needed anymore

    // from the "same" application?
    CompareFileName := PString(Param)^;
    if SameFileName(WindowModuleName, CompareFileName) then begin
      PreviousInstance := Handle;

      // Stop EnumWindows loop
      Result := False;
    end;
  end;
end;

function GetPreviousInstance: THandle;
var
  UniqueMutex: THandle;
  ThisModuleFileName: String;
  Buffer: array[0..511] of char;
begin
  Result := 0;

  // Check if a previous has already claimed this mutex name
  // This mutex is closed automatically when the application terminates
  UniqueMutex := CreateMutex(nil, True, 'DevCppSingleInstanceMutex');
  if (UniqueMutex <> 0) and (GetLastError = ERROR_ALREADY_EXISTS) then begin
    //ShowMessage('ERROR_ALREADY_EXISTS');
    // Store our own module filename
    if GetModuleFileName(GetModuleHandle(nil), Buffer, SizeOf(Buffer)) = 0 then
      Exit;
    ThisModuleFileName := Buffer;

    // If that's the case, walk all top level windows and find the previous instance
    // At this point, the program that created the mutex might not have created its MainForm yet
    // Temporary fix: try to find the MainForm handle every 100ms

    while True do begin
      if not EnumWindows(@GetPreviousInstanceCallback, Integer(PString(@ThisModuleFileName))) then begin
        Result := PreviousInstance;
        Exit;
      end;
      Sleep(100);
    end;
  end;
end;

end.

