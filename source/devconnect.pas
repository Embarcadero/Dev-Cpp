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

unit devconnect;

interface

uses
{$IFDEF WIN32}
  Classes, Windows, StdCtrls, Forms, CheckForUpdate;
{$ENDIF}
{$IFDEF LINUX}
  Classes, QStdCtrls, QForms, CheckForUpdate;
{$ENDIF}

type
  TDevConnect = class(TThread)

  public
    Check : TCheckForUpdate;
    L     : TLabel;

  protected
    msg   : string;

    procedure Execute; override;
    procedure Sync;

  end;

implementation

{ TDevConnect }

procedure TDevConnect.Sync;
begin
  L.Caption := msg;
  Application.ProcessMessages;
end;

procedure TDevConnect.Execute;
begin
  with Check do begin
    msg := 'Connecting...';
    Synchronize(sync);
    Connect;
    msg := 'Downloading update file...';
    Synchronize(sync);
    Download;
    Synchronize(sync);
    msg := 'Disconnecting...';
    Synchronize(sync);
    Disconnect;
    msg := 'Checking update file...';
    Synchronize(sync);
    Check;
    msg := 'Done';
    Synchronize(sync);
  end;
end;

end.
