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

unit CheckFrm;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Classes, Controls, Menus,
  Forms, Dialogs, StdCtrls, Extctrls, ComCtrls, Buttons;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Classes, QControls, QMenus,
  QForms, QDialogs, QStdCtrls, QExtctrls, QComCtrls, QButtons;
{$ENDIF}

type

  TCheckForm = class(TForm)
    grpTask: TGroupBox;
    grpResults: TGroupBox;
    lblRelVer: TLabel;
    lblNeed: TLabel;
    L: TLabel;
    lblDesc: TLabel;
    Memo: TMemo;
    Release: TLabel;
    Need_version: TLabel;
    lblSites: TLabel;
    SiteList: TListBox;
    btnOk: TBitBtn;
    procedure btnOkClick(Sender: TObject);
    procedure SiteListDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    FirstStart: Boolean;
    procedure LoadText;
  end;

var
 CheckForm: TCheckForm;

implementation

{$R *.dfm}

uses
{$IFDEF WIN32}
  Registry, ShellApi, CheckForUpdate, devconnect, version, MultiLangSupport;
{$ENDIF}
{$IFDEF LINUX}
  CheckForUpdate, devconnect, version, MultiLangSupport;
{$ENDIF}

procedure TCheckForm.btnOkClick(Sender: TObject);
begin
  Close;
end;

// ** should add updated list to downloaded update file
procedure TCheckForm.SiteListDblClick(Sender: TObject);
var i : integer;
begin
  for i := 0 to SiteList.Items.Count-1 do
      if SiteList.Selected[i] then
         ShellExecute(GetDesktopWindow, 'open',
                      pChar(SiteList.Items[i]),
                      nil, nil, SW_SHOWNORMAL);
end;

procedure TCheckForm.FormCreate(Sender: TObject);
begin
  FirstStart := True;
  LoadText;
end;

procedure TCheckForm.FormActivate(Sender: TObject);
var
  Check: TCheckForUpdate;
  Connect : TDevConnect;
begin
  if not FirstStart then Exit;
  FirstStart := False;
  Check := TCheckForUpdate.Create;
  Connect := TDevConnect.Create(true);
  Connect.Check := Check;
  Connect.L := L;
  Connect.FreeOnTerminate := True;
  Connect.Resume;
end;

procedure TCheckForm.LoadText;
begin
  Caption:=            Lang[ID_UCF];
  grpTask.Caption:=    '  '+Lang[ID_UCF_GRP_TASK]+'  ';
  grpResults.Caption:= '  '+Lang[ID_UCF_GRP_RESULTS]+'  ';
  lblRelVer.Caption:=  Lang[ID_UCF_RELEASE];
  lblNeed.Caption:=    Lang[ID_UCF_NEED];
  lblDesc.Caption:=    Lang[ID_UCF_DESC];
  lblSites.Caption:=   Lang[ID_UCF_SITES];

  btnOk.Caption:=      Lang[ID_BTN_OK];
end;

end.
