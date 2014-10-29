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

unit devMonitorThread;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, devMonitorTypes, Math, SyncObjs;
{$ENDIF}
{$IFDEF LINUX}
SysUtils, Variants, Classes, QGraphics, QControls, QForms,
QDialogs, QStdCtrls, devMonitorTypes, SyncObjs;
{$ENDIF}

const
  APPMSG_NOTIFYFILECHANGED = WM_USER + 2048;

type
  TdevMonitorThread = class(TThread)
  private
    fOwner: TComponent;
    fFileProperties: TList;
    fFileNames: TStringList;
    fMonitors: array[0..MAXIMUM_WAIT_OBJECTS - 1] of THandle;
    fMonitorCount: integer;
    fChangeType: TdevMonitorChangeType;
    fFilename: AnsiString;
    fShouldQuit: TEvent;
    procedure Notify;
    procedure CreateMonitors;
    procedure DestroyMonitors;
  public
    constructor Create(AOwner: TComponent; FileNames: TStringList);
    destructor Destroy; override;
    procedure Execute; override;
  end;

implementation

uses devFileMonitor;

{ TdevMonitorThread }

constructor TdevMonitorThread.Create(AOwner: TComponent; FileNames: TStringList);
begin
  inherited Create(False); // start immediately
  fOwner := AOwner;
  fFileNames := TStringList.Create;
  fFileNames.Assign(FileNames);
  fFileProperties := TList.Create;
  fMonitorCount := 0;
  fShouldQuit := TEvent.Create(nil, false, false, '');
  CreateMonitors;
end;

destructor TdevMonitorThread.Destroy;
begin
  // Ask the thread to stop (we're in another thread now)
  fShouldQuit.SetEvent;
  Terminate; // set Terminated flag

  // Wait for it
  WaitFor;

  // Clear stuff
  DestroyMonitors;
  fFileProperties.Free;
  fFileNames.Free;
  fShouldQuit.Free;
  inherited;
end;

procedure TdevMonitorThread.CreateMonitors;
var
  I: integer;
  Item: PdevMonitorFile;
  SearchRec: TSearchRec;
  ChangeID: Cardinal;
begin
  // Create structs of filename/timestamp
  for I := 0 to Min(MAXIMUM_WAIT_OBJECTS - 2, fFileNames.Count - 1) do begin

    // Does the file exist?
    if FindFirst(fFileNames[I], faAnyFile, SearchRec) = 0 then begin

      // Can we create a change notifier for it?
      ChangeID := FindFirstChangeNotification(
        PAnsiChar(ExtractFilePath(fFileNames[I])),
        False,
        FILE_NOTIFY_CHANGE_LAST_WRITE or FILE_NOTIFY_CHANGE_FILE_NAME // change contents or change filename
        );
      if ChangeID <> INVALID_HANDLE_VALUE then begin

        // Add to separate HANDLE array to pass to WaitForMultipleObjects
        fMonitors[fMonitorCount] := ChangeID;
        Inc(fMonitorCount);

        // Add to files list
        Item := new(PdevMonitorFile);
        Item^.FileName := fFileNames[i];
        Item^.TimeStamp := SearchRec.Time;
        fFileProperties.Add(Item);

        // Free OS memory
        FindClose(SearchRec);
      end;
    end;
  end;

  // Add a quit signal
  fMonitors[fMonitorCount] := fShouldQuit.Handle;
  Inc(fMonitorCount);
end;

procedure TdevMonitorThread.DestroyMonitors;
var
  I: integer;
begin
  // Stop monitoring files we have found
  for I := 0 to fFileProperties.Count - 1 do
    Dispose(PdevMonitorFile(fFileProperties[i]));
  fFileProperties.Clear;

  // Stop monitoring files
  for I := 0 to fMonitorCount - 2 do // Do not destroy fShouldQuit.Handle...
    FindCloseChangeNotification(THandle(fMonitors[I])); // TODO: EXCEPTION :(
  fMonitorCount := 0;
end;

procedure TdevMonitorThread.Execute;
var
  FileNameIndex: integer;
  WaitResult: Cardinal;
  WaitObjectIndex: integer;
  SearchRec: TSearchRec;
  FileStruct: PdevMonitorFile;
begin
  // Wait for file changes and external commands
  while not Terminated and not Suspended do begin
    WaitResult := WaitForMultipleObjects(fMonitorCount, @fMonitors, False, INFINITE);
    if WaitResult = WAIT_FAILED then // The function has failed.
      Break;
    if WaitResult = WAIT_TIMEOUT then // The time-out interval elapsed...
      Break;
    if (WaitResult >= WAIT_ABANDONED_0) and (WaitResult <= WAIT_ABANDONED_0 + Cardinal(fMonitorCount) - 1) then
      Break;
    if WaitResult = Cardinal(fMonitorCount-1) then // fShouldQuit
      Break;

    // Check timestamp of signaled file...
    WaitObjectIndex := WaitResult - WAIT_OBJECT_0;
    FileStruct := PdevMonitorFile(fFileProperties[WaitObjectIndex]);
    if FindFirst(FileStruct^.FileName, faAnyFile, SearchRec) = 0 then begin

      // Timstamp has changed. File has changed.
      if FileStruct^.TimeStamp <> SearchRec.Time then begin
        FileStruct^.TimeStamp := SearchRec.Time;
        fChangeType := mctChanged;
        fFilename := FileStruct^.FileName;
        Notify;
      end;
      FindClose(SearchRec);

      // Keep monitoring
      FindNextChangeNotification(THandle(fMonitors[WaitObjectIndex]));

      // File has been deleted. Rebuild array :(
    end else begin
      fChangeType := mctDeleted;
      fFilename := FileStruct^.FileName;
      Notify;

      // Remove this one from the monitoring list
      FileNameIndex := fFileNames.IndexOf(FileStruct^.FileName);
      if FileNameIndex <> -1 then
        fFileNames.Delete(FileNameIndex);

      // Rebuild
      DestroyMonitors;
      CreateMonitors;
    end;
  end;
end;

procedure TdevMonitorThread.Notify;
var
  P: PAnsiChar;
begin
  P := StrNew(PAnsiChar(fFilename));
  PostMessage(TdevFileMonitor(fOwner).Handle, APPMSG_NOTIFYFILECHANGED, integer(fChangeType), LPARAM(P));
end;

end.

