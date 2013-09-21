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

unit ProcessListFrm;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, MultiLangSupport;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Variants, Classes, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls, QButtons, QExtCtrls, MultiLangSupport;
{$ENDIF}

type
  TProcessListForm = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    Panel1: TPanel;
    ProcessCombo: TComboBox;
    MainLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure LoadText;
    { Private declarations }
  public
    ProcessList : TList;
    { Public declarations }
  end;

var
  ProcessListForm: TProcessListForm;

implementation

uses 
  tlhelp32;

{$R *.dfm}

procedure TProcessListForm.FormCreate(Sender: TObject);
var
  t  : THandle;
  pe : TProcessEntry32;
  HasProcess: boolean;
begin
  LoadText;
  ProcessList := TList.Create;
  t := CreateToolhelp32Snapshot(TH32CS_SNAPALL, 0);
  try
    pe.dwSize:= SizeOf(pe);
    HasProcess := Process32First(t, pe);
    while HasProcess do begin
      ProcessCombo.Items.Add(pe.szExeFile);
      ProcessList.Add(pointer(pe.th32ProcessId));
      HasProcess := Process32Next(t, pe);
    end;
  finally
    CloseHandle(t);
  end;
end;

procedure TProcessListForm.FormDestroy(Sender: TObject);
begin
  ProcessList.Free;
end;

procedure TProcessListForm.LoadText;
begin
  Caption := Lang[ID_ITEM_ATTACHPROCESS];
  MainLabel.Caption := Lang[ID_MSG_ATTACH];
  MainLabel.Width := 360;
  OKBtn.Caption := Lang[ID_BTN_OK];
  CancelBtn.Caption := Lang[ID_BTN_CANCEL];
end;

end.
