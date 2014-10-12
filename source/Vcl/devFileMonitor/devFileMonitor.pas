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

unit devFileMonitor;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Classes, Forms, Controls,
  devMonitorThread, devMonitorTypes;
{$ENDIF}
{$IFDEF LINUX}
SysUtils, Classes, QForms, QControls,
devMonitorThread, devMonitorTypes;
{$ENDIF}

type
  TdevFileMonitor = class(TWinControl)
  private
    fMonitor: TdevMonitorThread;
    fFiles: TStringList;
    fNotifyChange: TdevMonitorChange;
    fUpdateCount: integer;
    procedure SubClassWndProc(var Message: TMessage);
    procedure Refresh;
    procedure Activate;
    procedure Deactivate;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Monitor(const FileName: AnsiString);
    procedure UnMonitor(const FileName: AnsiString);
    procedure BeginUpdate;
    procedure EndUpdate;
  published
    property OnNotifyChange: TdevMonitorChange read fNotifyChange write fNotifyChange;
  end;

procedure Register;

implementation

{ TdevFileMonitor }

procedure Register;
begin
  RegisterComponents('Dev-C++', [TdevFileMonitor]);
end;

constructor TdevFileMonitor.Create(AOwner: TComponent);
begin
  inherited;
  fFiles := TStringList.Create;
  fMonitor := nil;
  fUpdateCount := 0;
  WindowProc := SubClassWndProc;
end;

destructor TdevFileMonitor.Destroy;
begin
  Deactivate;
  fFiles.Free;
  inherited;
end;

procedure TdevFileMonitor.SubClassWndProc(var Message: TMessage);
begin
  if Message.Msg = APPMSG_NOTIFYFILECHANGED then begin
    if Assigned(fNotifyChange) then begin
      fNotifyChange(Self, TDevMonitorChangeType(Message.WParam), PAnsiChar(Message.LParam));
      StrDispose(PAnsiChar(Message.LParam));
    end;
  end else
    WndProc(Message);
end;

procedure TdevFileMonitor.Activate;
begin
  if not Assigned(fMonitor) then begin
    fMonitor := TdevMonitorThread.Create(Self, fFiles); // starts immediately
  end;
end;

procedure TdevFileMonitor.Deactivate;
begin
  if Assigned(fMonitor) then begin // we can spawn a new fMonitor instance directly after calling this
    fMonitor.Free;
    fMonitor := nil;
  end;
end;

procedure TdevFileMonitor.BeginUpdate;
begin
  Inc(fUpdateCount);
  if fUpdateCount <> 0 then
    Deactivate;
end;

procedure TdevFileMonitor.EndUpdate;
begin
  Dec(fUpdateCount);
  if fUpdateCount = 0 then
    Activate;
end;

procedure TdevFileMonitor.Monitor(const FileName: AnsiString);
begin
  fFiles.Add(FileName);
  Refresh;
end;

procedure TdevFileMonitor.UnMonitor(const FileName: AnsiString);
var
  I: integer;
begin
  I := fFiles.IndexOf(FileName);
  if I <> -1 then begin
    fFiles.Delete(I);
    Refresh;
  end;
end;

procedure TdevFileMonitor.Refresh;
begin
  if not Assigned(fMonitor) then
    Activate
  else begin
    Deactivate;
    Activate;
  end;
end;

end.

