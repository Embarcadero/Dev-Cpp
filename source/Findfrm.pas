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

unit FindFrm;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  SynEdit, StdCtrls, SynEditTypes, ComCtrls;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Classes, QGraphics, QControls, QForms,
  QSynEdit, QStdCtrls, QSynEditTypes;
{$ENDIF}

type
  TfrmFind = class(TForm)
    btnFind: TButton;
    btnCancel: TButton;
    FindTabs: TTabControl;
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
    procedure btnFindClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnCancelClick(Sender: TObject);
    procedure FindTabsChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    fSearchOptions: TSynSearchOptions;
    fClose: boolean;
    fFindInFiles: boolean;
    procedure LoadText;
   public
    property SearchOptions: TSynSearchOptions read fSearchOptions;
    property FindInFiles: boolean read fFindInFiles write fFindInFiles;
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
  if cboFindText.Text <> '' then begin
     if cboFindText.Items.IndexOf(cboFindText.Text) = -1 then
      cboFindText.Items.Add(cboFindText.Text);

     fSearchOptions:= [];

     if cbMatchCase.checked then
      include(fSearchOptions, ssoMatchCase);

     if cbWholeWord.Checked then
      include(fSearchOptions, ssoWholeWord);

     if not fFindInFiles then
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
	fFindInFiles := FindTabs.TabIndex = 1;

	grpWhere.Visible:= fFindInFiles;
	rbProjectFiles.Enabled := Assigned(MainForm.fProject);
	if not Assigned(MainForm.fProject) then
		rbOpenFiles.Checked := true; // only apply when branch is taken!
	grpDirection.Visible:= not fFindInFiles;
	grpScope.Visible:= not fFindInFiles;
	grpOrigin.Visible:= not fFindInFiles;
end;

procedure TfrmFind.LoadText;
begin
	// Set interface font
	Font.Name := devData.InterfaceFont;
	Font.Size := devData.InterfaceFontSize;

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
end;

procedure TfrmFind.FormKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
begin
{$IFDEF WIN32}
	if (Key=VK_TAB) and (Shift=[ssCtrl]) then
{$ENDIF}
{$IFDEF LINUX}
	if (Key=XK_TAB) and (Shift=[ssCtrl]) then
{$ENDIF}
		// eliminated a branch! :D
		FindTabs.TabIndex := (FindTabs.TabIndex+1) mod 2;
end;

procedure TfrmFind.FormCreate(Sender: TObject);
begin
	LoadText;
	ActiveControl := cboFindText;
end;

procedure TfrmFind.FormShow(Sender: TObject);
begin
	if fFindInFiles then
		FindTabs.TabIndex:= 1
	else
		FindTabs.TabIndex:= 0;
	FindTabsChange(nil);
end;

end.
