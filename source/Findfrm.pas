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

unit Findfrm;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  SynEdit, StdCtrls, devTabs, SynEditTypes, XPMenu;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Classes, QGraphics, QControls, QForms,
  QSynEdit, QStdCtrls, devTabs, QSynEditTypes;
{$ENDIF}

type
  TfrmFind = class(TForm)
    btnFind: TButton;
    btnCancel: TButton;
    FindTabs: TdevTabs;
    lblFind: TLabel;
    cboFindText: TComboBox;
    grpOptions: TGroupBox;
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
    grpWhere: TGroupBox;
    rbProjectFiles: TRadioButton;
    rbOpenFIles: TRadioButton;
    XPMenu: TXPMenu;
    procedure btnFindClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnCancelClick(Sender: TObject);
    procedure FindTabsChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    fSearchOptions: TSynSearchOptions;
    fClose: boolean;
    fFindAll: boolean;
    procedure LoadText;
   public
    procedure SetFind;
    property SearchOptions: TSynSearchOptions read fSearchOptions;
    property FindAll: boolean read fFindAll write fFindAll;
  end;

var
 frmFind: TfrmFind;

implementation

uses 
{$IFDEF WIN32}
  Main, Dialogs, MultiLangSupport, devcfg;
{$ENDIF}
{$IFDEF LINUX}
  Xlib, Main, QDialogs, MultiLangSupport, devcfg;
{$ENDIF}

{$R *.dfm}

procedure TfrmFind.btnFindClick(Sender: TObject);
begin
  if cboFindText.Text = '' then
   begin
     {MessageBox(Application.MainForm.Handle, PChar(Lang[ID_ERR_SEARCHCANNOTBEEMPTY]),
       PChar(Lang[ID_WARN]), MB_OK or MB_ICONWARNING);
     fClose:= True;}
   end
  else
   begin
     if cboFindText.Items.IndexOf(cboFindText.Text) = -1 then
      cboFindText.Items.Add(cboFindText.Text);

     fSearchOptions:= [];

     if cbMatchCase.checked then
      include(fSearchOptions, ssoMatchCase);

     if cbWholeWord.Checked then
      include(fSearchOptions, ssoWholeWord);

     if not fFindAll then
      begin
        if rbBackward.checked then
         include(fSearchOptions, ssoBackwards);
        if rbSelectedOnly.Checked then
         include(fSearchOptions, ssoSelectedOnly);
        if rbEntireScope.Checked then
         include(fSearchOptions, ssoEntireScope);
      end
     else
      begin
        MainForm.FindOutput.Items.Clear;
        include(fSearchOptions, ssoEntireScope);
        include(fSearchOptions, ssoReplaceAll);
        include(fSearchOptions, ssoPrompt);
      end;
     fClose:= True;
   end;
end;

procedure TfrmFind.FormShow(Sender: TObject);
begin
  LoadText;
  ActiveControl:= cboFindText;
  FindTabs.Tabs.Clear;
  if fFindAll then
   FindTabs.Tabs.Append(lang[ID_FIND_FINDALLTAB])
  else
   begin
     FindTabs.Tabs.Append(Lang[ID_FIND_FINDTAB]);
     FindTabs.Tabs.Append(Lang[ID_FIND_FINDALLTAB]);
   end;
  FindTabs.TabIndex:= 0;
end;

procedure TfrmFind.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if fClose then
   Action:= caHide
  else
   begin
     Action:= caNone;
     ActiveControl:= cboFindText;
   end;
end;

procedure TfrmFind.btnCancelClick(Sender: TObject);
begin
  fClose:= true;
  Close;
end;

procedure TfrmFind.FindTabsChange(Sender: TObject);
begin
  if FindTabs.Tabs.Count> 1 then
   fFindAll:= FindTabs.TabIndex = 1
  else
   fFindAll:= TRUE;
  SetFind;
end;

procedure TfrmFind.SetFind;
begin
  grpWhere.Visible:= fFindAll;
  grpDirection.Visible:= not fFindAll;
  grpScope.Visible:= not fFindAll;
  grpOrigin.Visible:= not fFindAll;
end;

procedure TfrmFind.LoadText;
var
 x: Integer;
begin
  if devData.XPTheme then
    XPMenu.Active := true
  else
    XPMenu.Active := false;
  Caption:=                 Lang[ID_FIND];

  //tabs
  FindTabs.Tabs.Clear;
  FindTabs.Tabs.Append(Lang[ID_FIND_FINDTAB]);
  FindTabs.Tabs.Append(Lang[ID_FIND_FINDALLTAB]);

  //controls
  lblFind.Caption:=        Lang[ID_FIND_TEXT];
  grpOptions.Caption:=     '  '+Lang[ID_FIND_GRP_OPTIONS] +'  ';
  cbMatchCase.Caption:=    Lang[ID_FIND_CASE];
  cbWholeWord.Caption:=    Lang[ID_FIND_WWORD];

  grpWhere.Caption:=       Lang[ID_FIND_GRP_WHERE];
  rbProjectFiles.Caption:= Lang[ID_FIND_PRJFILES];
  rbOpenFIles.Caption:=    Lang[ID_FIND_OPENFILES];

  grpScope.Caption:=       '  ' +Lang[ID_FIND_GRP_SCOPE] +'  ';
  rbGlobal.Caption:=       Lang[ID_FIND_GLOBAL];
  rbSelectedOnly.Caption:= Lang[ID_FIND_SELONLY];

  grpOrigin.Caption:=      '  ' +Lang[ID_FIND_GRP_ORIGIN] +'  ';
  rbFromCursor.Caption:=   Lang[ID_FIND_CURSOR];
  rbEntireScope.Caption:=  Lang[ID_FIND_ENTIRE];

  grpDirection.Caption:=   '  ' +Lang[ID_FIND_GRP_DIRECTION] +'  ';
  rbForward.Caption:=      Lang[ID_FIND_FORE];
  rbBackward.Caption:=     Lang[ID_FIND_BACK];

  //buttons
  btnFind.Caption:=        Lang[ID_BTN_FIND];
  btnCancel.Caption:=      Lang[ID_BTN_CANCEL];

  x:= Self.Canvas.TextWidth(btnFind.Caption) +5;
  if x> btnFind.Width then
   btnFind.Width:= x;

  x:= Self.Canvas.TextWidth(btnCancel.Caption);
  if x> btnCancel.Width then
   btnCancel.Width:= x;
end;

procedure TfrmFind.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
{$IFDEF WIN32}
  if (Key=VK_TAB) and (Shift=[ssCtrl]) then
{$ENDIF}
{$IFDEF LINUX}
  if (Key=XK_TAB) and (Shift=[ssCtrl]) then
{$ENDIF}
    // switch tabs
    if FindTabs.Tabs.Count> 1 then begin
      if FindTabs.TabIndex=0 then
        FindTabs.TabIndex:=1
      else
        FindTabs.TabIndex:=0;
    end;
end;

end.
