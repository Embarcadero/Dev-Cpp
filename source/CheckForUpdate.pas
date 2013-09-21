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

unit CheckForUpdate;

interface

uses 
{$IFDEF WIN32}
  Classes, Windows, Wininet, Forms, Messages, SysUtils,
  Dialogs;
{$ENDIF}
{$IFDEF LINUX}
  Classes, QForms, SysUtils,
  QDialogs;
{$ENDIF}

type
  TCheckForUpdate = class(TObject)
  private
    NetHandle: HINTERNET;
    UrlHandle: HINTERNET;
    Buffer: array[0..1024] of char;
    BytesRead: cardinal;

  public
    UpdateFile, VersionName, Needed_package, Description : string;

    constructor Create;
    function  Connect: boolean;
    procedure Disconnect;
    procedure Download;
    procedure Check;
  end;

const URL = 'http://www.bloodshed.net/dev/update.txt';
const LocalFileName = 'update.txt';

procedure CheckUpdate(aOwner: TComponent);

implementation

uses 
  CheckFrm, Version;

procedure CheckUpdate(aOwner: TComponent);
begin
  CheckForm:= TCheckForm.Create(aOwner);
  try
   CheckForm.ShowModal;
  finally
   CheckForm.Free;
  end;
end;

constructor TCheckForUpdate.Create;
begin
  inherited Create;
  Application.ProcessMessages;
end;

function TCheckForUpdate.Connect;
begin
  if Assigned(NetHandle) then
     Disconnect;

  UpdateFile := '';

  NetHandle := InternetOpen('Dev-C++ Check for Update', INTERNET_OPEN_TYPE_PRECONFIG,
                            nil, nil, 0);

  if Assigned(NetHandle) then begin
     UrlHandle := InternetOpenUrl(NetHandle, PChar(Url), nil, 0,
                                  INTERNET_FLAG_RELOAD, 0);
     Result := True;
  end
  else begin
     raise Exception.Create('Dev-C++ was not able to download update file. Please see http://www.bloodshed.net/dev/');
     Result := False;
  end;
end;

procedure TCheckForUpdate.Disconnect;
begin
  InternetCloseHandle(UrlHandle);
  UrlHandle := nil;
end;

procedure TCheckForUpdate.Download;
var F : TextFile;
begin
  if Assigned(UrlHandle) then
     { UrlHandle valid? Proceed with download }
  begin
     FillChar(Buffer, SizeOf(Buffer), 0);
     repeat
        UpdateFile := UpdateFile + Buffer;
        FillChar(Buffer, SizeOf(Buffer), 0);
        InternetReadFile(UrlHandle, @Buffer, SizeOf(Buffer), BytesRead);
     until BytesRead = 0;
     Disconnect;

     AssignFile(F,LocalFileName);
     Rewrite(F);
     Write(F, UpdateFile);
     CloseFile(F);
  end
  else begin
     { UrlHandle is not valid. Raise an exception. }
     raise Exception.CreateFmt('Cannot open URL %s, please see http://www.bloodshed.net/dev/', [Url]);
     Disconnect;
  end;
end;

procedure TCheckForUpdate.Check;
var F : TextFile;
    c     : char;
    tmp : string;
    i   : integer;
begin
  if not FileExists(LocalFileName) then begin
     MessageDlg('Dev-C++ was not able to download the update file. Please see http://www.bloodshed.net/dev/', mtError, [mbOK],0);
     Exit;
  end;

  AssignFile(F, LocalFileName);
  Reset(F);

  repeat
    Read(F,c)
  until c = '$';

  repeat
    Read(F,c);
    VersionName := versionname + c;
  until c = '$';
  while Pos('$',VersionName)<>0 do Delete(VersionName, Pos('$', VersionName), 1);

  repeat
    Read(F,c);
    Needed_package := Needed_package + c;
  until c = '$';
  while Pos('$',Needed_package)<>0 do Delete(Needed_package, Pos('$', Needed_package), 1);

  repeat
    Read(F,c);
    Description := Description + c;
  until c = '$';
  while Pos('$',Description)<>0 do Delete(Description, Pos('$', Description), 1);

  i := 0;

  repeat
    repeat
     Read(F,c);
     tmp := tmp + c;
    until (c = '$') or eof(F);
    while Pos('$',tmp)<>0 do
          Delete(tmp, Pos('$', tmp), 1);
    while Pos(#$A,tmp)<>0 do
          Delete(tmp, Pos(#$A, tmp), 3);
    CheckForm.SiteList.Items.Strings[i] := tmp;
    tmp := '';
    inc(i);
  until eof(F); // end of file

  CheckForm.SiteList.Items.Delete(CheckForm.SiteList.Items.Count-1);
  CheckForm.Memo.Lines.Clear;
  CheckForm.Release.Caption := VersionName;
  CheckForm.Need_Version.Caption := Needed_package;
  if VersionName = DEVCPP_VERSION then
    MessageDlg('You are currently using the newest release, there is no need to download it.', mtInformation, [mbOK], 0);
  CheckForm.Memo.Lines.Add(description);
  CheckForm.Memo.SelStart := 0;

  CloseFile(F);
  DeleteFile(LocalFileName);
end;

end.
