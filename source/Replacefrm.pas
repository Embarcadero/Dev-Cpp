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

unit Replacefrm;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  SynEdit, StdCtrls, SynEditTypes, XPMenu;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Classes, QGraphics, QControls, QForms,
  QSynEdit, QStdCtrls, QSynEditTypes;
{$ENDIF}

type
  TfrmReplace = class(TForm)
    cboFindText: TComboBox;
    grpOptions: TGroupBox;
    lblFind: TLabel;
    btnReplace: TButton;
    btnCancel: TButton;
    cbMatchCase: TCheckBox;
    cbWholeWord: TCheckBox;
    grpDirection: TGroupBox;
    rbForward: TRadioButton;
    rbBackward: TRadioButton;
    grpScope: TGroupBox;
    rbGlobal: TRadioButton;
    rbSelectedOnly: TRadioButton;
    grpOrigin: TGroupBox;
    rbFromCursor: TRadioButton;
    rbEntireScope: TRadioButton;
    lblReplace: TLabel;
    cboReplaceText: TComboBox;
    cbPrompt: TCheckBox;
    btnReplaceAll: TButton;
    XPMenu: TXPMenu;
    procedure btnReplaceClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnCancelClick(Sender: TObject);
  private
    fSearchOptions: TSynSearchOptions;
    fClose: boolean;
    procedure LoadText;
  public
    property SearchOptions: TSynSearchoptions read fSearchOptions write fSearchOptions;
  end;

var
 frmReplace: TfrmReplace;

implementation

{$R *.dfm}

uses 
{$IFDEF WIN32}
  Dialogs, MultiLangSupport, devcfg;
{$ENDIF}
{$IFDEF LINUX}
  QDialogs, MultiLangSupport, devcfg;
{$ENDIF}

procedure TfrmReplace.btnReplaceClick(Sender: TObject);
begin
  if cboFindText.Text = '' then
   begin
     {MessageBox(Application.MainForm.Handle, PChar(Lang[ID_ERR_SEARCHCANNOTBEEMPTY]),
      PChar(Lang[ID_WARN]), MB_OK or MB_ICONWARNING);
     fClose:= False;}
   end
  else
   begin
     fSearchOptions:= [];
     if cboFindText.Items.Indexof(cboFindText.Text) = -1 then
      cboFindText.Items.Add(cboFindText.Text);

     if cboReplaceText.Items.IndexOf(cboReplaceText.Text) = -1 then
      cboReplaceText.Items.Add(cboReplaceText.Text);

     if modalResult = mrok then
      fSearchOptions:= [ssoReplace];

     if ModalResult = mrAll then
      fSearchOptions:= [ssoReplaceAll];

     if cbPrompt.Checked then
      include(fSearchoptions, ssoPrompt);

     if cbMatchCase.checked then
      include(fSearchOptions, ssoMatchCase);

     if cbWholeWord.Checked then
      include(fSearchOptions, ssoWholeWord);

     if rbBackward.checked then
      include(fSearchOptions, ssoBackwards);

     if rbSelectedOnly.Checked then
      include(fSearchOptions, ssoSelectedOnly);

     if rbEntireScope.Checked then
      include(fSearchOptions, ssoEntireScope);
     fClose:= True;
   end;
end;

procedure TfrmReplace.FormShow(Sender: TObject);
begin
  ActiveControl:= cboFindText;
  LoadText;
end;

procedure TfrmReplace.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if fClose then
   action:= caHide
  else
   begin
     Action:= caNone;
     ActiveControl:= cboFindText;
   end;
end;

procedure TfrmReplace.btnCancelClick(Sender: TObject);
begin
  fClose:= true;
  Close;
end;

procedure TfrmReplace.LoadText;
var
 x: Integer;
begin
  if devData.XPTheme then
    XPMenu.Active := true
  else
    XPMenu.Active := false;
  Caption:=                  Lang[ID_RPLC];
  lblFind.Caption:=          Lang[ID_RPLC_FINDTEXT];
  lblReplace.Caption:=       Lang[ID_RPLC_REPLACETEXT];
  grpOptions.Caption:=       '  '+Lang[ID_RPLC_GRP_OPTIONS] +'  ';
  cbMatchCase.Caption:=      Lang[ID_RPLC_CASE];
  cbWholeWord.Caption:=      Lang[ID_RPLC_WHOLEWORD];
  cbPrompt.Caption:=         Lang[ID_RPLC_PROMPT];
  grpDirection.Caption:=     Lang[ID_RPLC_GRP_DIRECTION];
  rbForward.Caption:=        Lang[ID_RPLC_FORWARD];
  rbBackward.Caption:=       Lang[ID_RPLC_BACKWARD];
  grpScope.Caption:=         '  '+Lang[ID_RPLC_GRP_SCOPE]+'  ';
  rbGlobal.Caption:=         Lang[ID_RPLC_GLOBAL];
  rbSelectedOnly.Caption:=   Lang[ID_RPLC_SELONLY];
  grpOrigin.Caption:=        Lang[ID_RPLC_GRP_ORIGIN];
  rbFromCursor.Caption:=     Lang[ID_RPLC_CURSOR];
  rbEntireScope.Caption:=    Lang[ID_RPLC_ENTIRE];

  btnReplace.Caption:=       Lang[ID_BTN_OK];
  btnCancel.Caption:=        Lang[ID_BTN_CANCEL];
  btnReplaceAll.Caption:=    Lang[ID_BTN_REPLACEALL];

  x:= Self.Canvas.TextWidth(btnReplace.Caption) +5;
  if x> btnReplace.Width then
   begin
     btnReplace.Width:= x;
     btnReplaceAll.Left:= btnReplace.Left +btnReplace.Width +6;
   end;

  x:= Self.Canvas.TextWidth(btnReplaceAll.Caption) +5;
  if x> btnReplaceAll.Width then
   btnReplaceAll.Width:= x;

  x:= Self.Canvas.TextWidth(btnCancel.Caption) +5;
  if x> btnCancel.Width then
   begin
     btnCancel.Left:= btnCancel.Left -((x -btnCancel.Width) +10);
     btnCancel.Width:= x;
   end;
end;

end.
