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
    { Private declarations }
    fMonitor: TdevMonitorThread;
    fFiles: TStrings;
    fNotifyChange: TdevMonitorChange;
    function GetActive: boolean;
    procedure SetActive(Value: boolean);
    procedure SetFiles(Value: TStrings);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Activate;
    procedure Deactivate;
    procedure Refresh(ActivateIfNot: boolean);
    procedure SubClassWndProc(var Message: TMessage);
  published
    { Published declarations }
    property Active: boolean read GetActive write SetActive;
    property Files: TStrings read fFiles write SetFiles;
    property OnNotifyChange: TdevMonitorChange read fNotifyChange write fNotifyChange;
  end;

implementation

{ TdevFileMonitor }

procedure TdevFileMonitor.SubClassWndProc(var Message: TMessage);
begin
  if Message.Msg = APPMSG_NOTIFYFILECHANGED then
  begin
    if Assigned(fNotifyChange) then
    begin
      fNotifyChange(Self, TDevMonitorChangeType(Message.WParam),
        PAnsiChar(Message.LParam));
      StrDispose(PAnsiChar(Message.LParam));
    end;
  end
  else
    WndProc(Message);
end;

procedure TdevFileMonitor.Activate;
begin
  if not Active then begin
    fMonitor := TdevMonitorThread.Create(Self, fFiles);
    fMonitor.Resume;
  end;
end;

constructor TdevFileMonitor.Create(AOwner: TComponent);
begin
  inherited;
  fFiles := TStringList.Create;
  fMonitor := nil;
  WindowProc := SubClassWndProc;
end;

procedure TdevFileMonitor.Deactivate;
begin
  if Assigned(fMonitor) then begin
    fMonitor.TellToQuit;
    fMonitor.WaitFor;
    fMonitor.Free;
    fMonitor:=nil;
  end;
end;

destructor TdevFileMonitor.Destroy;
begin
  Deactivate;
  fFiles.Free;
  inherited;
end;

function TdevFileMonitor.GetActive: boolean;
begin
  Result := Assigned(fMonitor);
end;

procedure TdevFileMonitor.Refresh(ActivateIfNot: boolean);
begin
  if not Active then
    Activate
  else
    fMonitor.ReloadList(fFiles);
end;

procedure TdevFileMonitor.SetActive(Value: boolean);
begin
  if Value and not Active then
    Activate
  else if not Value and Active then
    Deactivate;
end;

procedure TdevFileMonitor.SetFiles(Value: TStrings);
begin
  fFiles.Assign(Value);
end;

end.

