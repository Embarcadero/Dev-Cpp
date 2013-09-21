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

unit WebThread;

interface

uses
{$IFDEF WIN32}
  Windows, Classes, SysUtils, WinInet;
{$ENDIF}
{$IFDEF LINUX}
  Classes, SysUtils;
{$ENDIF}

// this thread retrieves a remote document and saves it in a temporary file.
type
  PUpdateRec = ^TUpdateRec;
  TUpdateRec = packed record
    Name: string;
    Description: string;
    RemoteFilename: string;
    LocalFilename: string;
    Group: string;
    InstallPath: string;
    Version: string;
    LocalVersion: string;
    Size: cardinal;
    Date: string;
    Selected: boolean;
    TempFilename: string;
    Execute : boolean;
  end;

  TWUMessageReason = (wumrConnectSuccess,
    wumrConnectError,
    wumrRetrieveStart,
    wumrRetrieveProgress,
    wumrRetrieveSuccess,
    wumrRetrieveError,
    wumrDisconnect,
    wumrUnknownError);

  TWUNotifyEvent = procedure(Sender: TObject; MsgCode: TWUMessageReason; Msg: string; TransferSize, CurrentSize: cardinal) of object;

  TWebThread = class(TThread)
  private
    { Private declarations }
    fInternetHandle: HINTERNET;
    fFileHandle: HINTERNET;
    fFilesList: TList;
    fRemoteBase: string;
    fMsg: string;
    fTotalSize: cardinal;
    fCurrentSize: cardinal;
    fCurrentIndex: integer;
    fMsgCode: TWUMessageReason;
    fOnMessage: TWUNotifyEvent;
    procedure SetMessage(MsgCode: TWUMessageReason; Msg: string);
    procedure AlertMainThread;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;
    property CurrentIndex: integer read fCurrentIndex;
    property Files: TList read fFilesList write fFilesList;
    property RemoteBase: string read fRemoteBase write fRemoteBase;
    property OnMessage: TWUNotifyEvent read fOnMessage write fOnMessage;
    property LastMessage: TWUMessageReason read fMsgCode;
  end;

implementation

{ TWebThread }

procedure TWebThread.AlertMainThread;
begin
  if Assigned(fOnMessage) then
    fOnMessage(Self, fMsgCode, fMsg, fTotalSize, fCurrentSize);
end;

constructor TWebThread.Create(CreateSuspended: boolean);
begin
  inherited;

  fFilesList := TList.Create;
end;

destructor TWebThread.Destroy;
begin
  fFilesList.Clear;
  fFilesList.Free;

  inherited;
end;

procedure TWebThread.Execute;
var
  Buffer: array[0..4095] of Char;
  CharsRead: cardinal;
  FileSize: integer;
  DownFile: string;
  TempPath, TempFile: array[0..MAX_PATH] of Char;
  hFile: cardinal;
  I: integer;
begin
  try
  // Initialize the DLLs /////////////////////////////////////////////////////
    fInternetHandle := InternetOpen('WebUpdater by mandrav©',
      INTERNET_OPEN_TYPE_PRECONFIG, // load from registry (proxy etc)
      nil, // proxy name
      nil,
      0);
    if fInternetHandle = nil then begin
      SetMessage(wumrConnectError, 'Could not initialize network');
      Synchronize(AlertMainThread);
      Exit;
    end
    else begin
      SetMessage(wumrConnectSuccess, 'Connected');
      Synchronize(AlertMainThread);
    end;

    for I := 0 to fFilesList.Count - 1 do begin
      with PUpdateRec(fFilesList[I])^ do
      begin
        if Pos('http://', LowerCase(RemoteFilename)) = 1 then
          DownFile := RemoteFilename
        else
          DownFile := fRemoteBase + RemoteFilename;
      end;
      fTotalSize := 0;
      fCurrentSize := 0;
      fCurrentIndex := I;
  // Initiate transfer /////////////////////////////////////////////////////
      fFileHandle := InternetOpenUrl(fInternetHandle,
        PChar(DownFile),
        nil,
        0,
        {INTERNET_FLAG_PRAGMA_NOCACHE or } INTERNET_FLAG_RELOAD,
        0);
      if fFileHandle = nil then begin
        SetMessage(wumrRetrieveError, 'Could not start transferring remote file');
        Synchronize(AlertMainThread);
        Continue;
      end
      else begin
        SetMessage(wumrRetrieveStart, 'Start transferring');
        Synchronize(AlertMainThread);
      end;

      FileSize := InternetSetFilePointer(fFileHandle, 0, nil, FILE_END, 0);
      if FileSize = -1 then begin
        SetMessage(wumrRetrieveError, Format('File %s does not exist', [DownFile]));
        Synchronize(AlertMainThread);
        InternetCloseHandle(fFileHandle);
        Continue;
      end
      else begin
        fTotalSize := FileSize;
        SetMessage(wumrRetrieveStart, Format('Downloading %s', [PUpdateRec(fFilesList[I])^.Name]));
        InternetSetFilePointer(fFileHandle, 0, nil, FILE_BEGIN, 0);
      end;
      Synchronize(AlertMainThread);

  // Get a temporary filename and create a file ////////////////////////////
      GetTempPath(SizeOf(TempPath), TempPath);
      GetTempFileName(TempPath, '~wu', 0, TempFile);

  // Transfer the file /////////////////////////////////////////////////////
      hFile := FileCreate(TempFile);
      FillChar(Buffer, SizeOf(Buffer), 0);
      repeat
        InternetReadFile(fFileHandle,
          @Buffer,
          SizeOf(Buffer),
          CharsRead);
        FileWrite(hFile, Buffer, CharsRead);
        fCurrentSize := fCurrentSize + CharsRead;
        SetMessage(wumrRetrieveProgress, '');
        Synchronize(AlertMainThread);
      until CharsRead = 0;
      FileClose(hFile);

      if GetLastError <> 0 then begin
        SetMessage(wumrRetrieveError, 'Error retrieving remote file: ' + SysErrorMessage(GetLastError));
        DeleteFile(TempFile);
        Synchronize(AlertMainThread);
        InternetCloseHandle(fFileHandle);
        Continue;
      end
      else begin
        PUpdateRec(fFilesList[I])^.TempFilename:=TempFile;
        SetMessage(wumrRetrieveSuccess, 'Transfer completed');
        Synchronize(AlertMainThread);
      end;
      InternetCloseHandle(fFileHandle);
    end;

  // Close the handles /////////////////////////////////////////////////////
    InternetCloseHandle(fInternetHandle);
    SetMessage(wumrDisconnect, 'Disconnected');
    Synchronize(AlertMainThread);
  except
    SetMessage(wumrUnknownError, SysErrorMessage(GetLastError));
    Synchronize(AlertMainThread);
  end;
end;

procedure TWebThread.SetMessage(MsgCode: TWUMessageReason; Msg: string);
begin
  fMsg := Msg;
  fMsgCode := MsgCode;
end;

end.

