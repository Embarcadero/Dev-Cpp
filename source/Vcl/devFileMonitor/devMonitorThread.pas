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

const APPMSG_NOTIFYFILECHANGED = WM_USER + 2048;

type
  TdevMonitorThread = class(TThread)
  private
    fOwner: TComponent;
    fFiles: TList;
    fMonitors: array[0..MAXIMUM_WAIT_OBJECTS] of THandle;
    fMonitorCount: integer;
    fChangeType: TdevMonitorChangeType;
    fFilename: AnsiString;
    fShouldQuit: TEvent;
    procedure Notify;
    procedure CreateMonitors(FileNames : TStringList);
    procedure DestroyMonitors;
  public
    constructor Create(AOwner: TComponent; FileNames: TStringList);
    destructor Destroy; override;
    procedure Execute; override;
    procedure TellToQuit;
  end;

implementation

uses devFileMonitor;

{ TdevMonitorThread }

constructor TdevMonitorThread.Create(AOwner: TComponent; FileNames: TStringList);
begin
	inherited Create(True);
	FreeOnTerminate := True;
	fOwner := AOwner;
	fFiles := TList.Create;
	fShouldQuit := TEvent.Create(nil, false, false, '');
	CreateMonitors(FileNames);
end;

destructor TdevMonitorThread.Destroy;
begin
	fShouldQuit.Free;
	DestroyMonitors;
	inherited;
end;

procedure TdevMonitorThread.TellToQuit;
begin
	fShouldQuit.SetEvent;
end;

procedure TdevMonitorThread.CreateMonitors(FileNames : TStringList);
var
	I : integer;
	Item : PdevMonitorFile;
	SearchRec : TSearchRec;
begin
	// Create structs of filename/timestamp
	for I := 0 to FileNames.Count - 1 do begin
		if FindFirst(FileNames[I], faAnyFile, SearchRec) = 0 then begin
			Item := new(PdevMonitorFile);
			Item^.FileName := FileNames[i];
			Item^.TimeStamp := SearchRec.Time;
			fFiles.Add(Item);
			FindClose(SearchRec);
		end;
	end;

	// Add monitors for each found item
	fMonitorCount := 0;
	for I := 0 to Min(MAXIMUM_WAIT_OBJECTS,fFiles.Count) - 1 do begin
		fMonitors[fMonitorCount] := FindFirstChangeNotification(
			PAnsiChar(ExtractFilePath(PdevMonitorFile(fFiles[I])^.FileName)),
			False,
			FILE_NOTIFY_CHANGE_LAST_WRITE or FILE_NOTIFY_CHANGE_FILE_NAME
		);
		Inc(fMonitorCount);
	end;

	// And a quit signal
	fMonitors[fMonitorCount] := fShouldQuit.Handle;
	Inc(fMonitorCount);
end;

procedure TdevMonitorThread.DestroyMonitors;
var
	I: integer;
begin
	// Stop monitoring...
	for I := 0 to fFiles.Count - 1 do
		FindCloseChangeNotification(fMonitors[I]);
	fMonitorCount := 0;

	// And free structs
	for I := 0 to fFiles.Count - 1 do
		Dispose(PdevMonitorFile(fFiles[i]));
	fFiles.Free;
end;

procedure TdevMonitorThread.Execute;
var
	WaitResult : Cardinal;
	WaitObjectIndex : integer;
	SearchRec: TSearchRec;
	FileStruct: PdevMonitorFile;
begin
	// Wait for file changes and external commands
	while not Terminated and not Suspended do begin
		WaitResult := WaitForMultipleObjects(fMonitorCount, @fMonitors, False, INFINITE);
		if WaitResult = WAIT_FAILED then
			Break;
		if WaitResult = WAIT_TIMEOUT then
			Break;
		if (WaitResult >= WAIT_ABANDONED_0) and (WaitResult <= WAIT_ABANDONED_0 + Cardinal(fMonitorCount) - 1) then
			break;

		// At this point, only valid results are left
		WaitObjectIndex := WaitResult - WAIT_OBJECT_0;
		if (WaitObjectIndex >= 0) and (WaitObjectIndex < fFiles.Count) then begin // filter ShouldQuit event

			// Check timestamp of signaled file...
			FileStruct := PdevMonitorFile(fFiles[WaitObjectIndex]);
			if FindFirst(FileStruct^.FileName, faAnyFile, SearchRec) = 0 then begin

				// Timstamp has changed. File has changed.
				if FileStruct^.TimeStamp <> SearchRec.Time then begin
					FileStruct^.TimeStamp := SearchRec.Time;
					fChangeType := mctChanged;
					fFilename := FileStruct^.FileName;
					Notify;
				end;
				FindClose(SearchRec);

			// File has been deleted
			end else begin
				fChangeType := mctDeleted;
				fFilename := FileStruct^.FileName;
				Notify;
				fFiles.Delete(WaitObjectIndex); // do NOT notify anymore
			end;

			// Keep monitoring
			FindNextChangeNotification(fMonitors[WaitObjectIndex]);
		end else // quit event
			break;
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

