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

{$WARN UNIT_PLATFORM OFF}
unit ToolEditFrm;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, XPMenu, Macros;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Classes, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls, QExtCtrls, QButtons, Macros;
{$ENDIF}

type
  TToolEditForm = class(TForm)
    lblTitle: TLabel;
    edTitle: TEdit;
    lblProg: TLabel;
    edProgram: TEdit;
    OpenDialog: TOpenDialog;
    lblWorkDir: TLabel;
    edWorkDir: TEdit;
    lblParam: TLabel;
    edParams: TEdit;
    btnCancel: TBitBtn;
    btnOk: TBitBtn;
    Panel1: TPanel;
    lblMacros: TLabel;
    lstMacro: TListBox;
    btnInsert: TBitBtn;
    btnHelp: TBitBtn;
    Bevel1: TBevel;
    lblDesc: TLabel;
    Bevel2: TBevel;
    XPMenu: TXPMenu;
    ParamText: TEdit;
    btnProg: TSpeedButton;
    btnWorkDir: TSpeedButton;
    procedure btnCancelClick(Sender: TObject);
    procedure HelpClick(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure lstMacroClick(Sender: TObject);
    procedure btnProgClick(Sender: TObject);
    procedure btnWorkDirClick(Sender: TObject);
    procedure EditEnter(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure edProgramChange(Sender: TObject);
    procedure edParamsChange(Sender: TObject);
   private
    fMacroTarget: TEdit;
    procedure LoadText;
  end;

implementation

uses 
{$IFDEF WIN32}
  FileCtrl, MultiLangSupport, devcfg, utils;
{$ENDIF}
{$IFDEF LINUX}
  MultiLangSupport, devcfg, utils;
{$ENDIF}

{$R *.dfm}

procedure TToolEditForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TToolEditForm.HelpClick(Sender: TObject);
begin
{  if Application.HelpFile <> '' then
   Application.HelpJump('ToolEditForm')
}

  Application.MessageBox(
    'You can use macros when calling a tool, how it can acts depending on what your doing'+#10#13+
    'in Dev-C++. For example, if you are willing to add a tool to Dev-C++ that can compress'+#10#13+
    'executable files, you may need to know the filename of your project''s executable that'+#10#13+
    'when calling the tool it automatically compress the current project''s executable.'+#10#13+
    'You can use many different parameters macros for your tool, for more information on'+#10#13+
    'what they can do see the Macro lists on the previous dialog.'
    ,'Quick help on macros',
{$IFDEF WIN32}
    MB_ICONINFORMATION);
{$ENDIF}
{$IFDEF LINUX}
    [smbOK], smsInformation);
{$ENDIF}
end;

procedure TToolEditForm.btnInsertClick(Sender: TObject);
begin
  if lstMacro.itemindex > -1 then
    fMacroTarget.SelText:= lstMacro.Items[lstMacro.itemindex];
end;

procedure TToolEditForm.lstMacroClick(Sender: TObject);
begin
  lblDesc.Caption:= Lang[lstMacro.ItemIndex +ID_ET_MACROS];
end;

procedure TToolEditForm.btnProgClick(Sender: TObject);
begin
  if OpenDialog.Execute then
   begin
     edProgram.Text := OpenDialog.FileName;
     edWorkDir.Text := ExtractFilePath(OpenDialog.FileName);
   end;
end;

procedure TToolEditForm.btnWorkDirClick(Sender: TObject);
var
{$IFDEF WIN32}
  new: string;
{$ENDIF}
{$IFDEF LINUX}
  new: WideString;
{$ENDIF}
begin
  if (Trim(edWorkDir.Text)<>'') and DirectoryExists(Trim(edWorkDir.Text)) then
    new:=edWorkDir.Text
  else
    new:=ExtractFilePath(edProgram.Text);
  if SelectDirectory('Select Working Dir', '', new) then
   edWorkDir.text:= New;
end;

procedure TToolEditForm.EditEnter(Sender: TObject);
begin
  fMacroTarget:= Sender as TEdit;
end;

procedure TToolEditForm.FormCreate(Sender: TObject);
begin
  fMacroTarget:= edParams;
  LoadText;
end;

procedure TToolEditForm.LoadText;
begin
  if devData.XPTheme then
    XPMenu.Active := true
  else
    XPMenu.Active := false;
  Caption:=              Lang[ID_TE];
  lblTitle.Caption:=     Lang[ID_TE_TITLE];
  lblProg.Caption:=      Lang[ID_TE_PROG];
  lblWorkDir.Caption:=   Lang[ID_TE_WORK];
  lblParam.Caption:=     Lang[ID_TE_PARAM];
  lblMacros.Caption:=     Lang[ID_TE_AVAIL];

  btnInsert.Caption:=    Lang[ID_TE_INSERT];

  btnOk.Caption:=        Lang[ID_BTN_OK];
  btnCancel.Caption:=    Lang[ID_BTN_CANCEL];
  btnHelp.Caption:=      Lang[ID_BTN_HELP];
end;

procedure TToolEditForm.edProgramChange(Sender: TObject);
begin
  ParamText.Text := ParseMacros(edProgram.Text + ' ' + edParams.Text);
end;

procedure TToolEditForm.edParamsChange(Sender: TObject);
begin
  ParamText.Text := ParseMacros(edProgram.Text + ' ' + edParams.Text);
end;

end.
