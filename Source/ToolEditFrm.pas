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

unit ToolEditFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons;

type
  TToolEditForm = class(TForm)
    lblTitle: TLabel;
    edTitle: TEdit;
    lblProg: TLabel;
    edProgram: TEdit;
    lblWorkDir: TLabel;
    edWorkDir: TEdit;
    lblParam: TLabel;
    edParams: TEdit;
    btnCancel: TBitBtn;
    btnOk: TBitBtn;
    lblMacros: TLabel;
    lstMacro: TListBox;
    btnInsert: TBitBtn;
    btnHelp: TBitBtn;
    lblDesc: TMemo;
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
  FileCtrl, MultiLangSupport, devcfg, utils, main, Macros;

{$R *.dfm}

procedure TToolEditForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TToolEditForm.HelpClick(Sender: TObject);
begin
  Application.MessageBox(
    'You can use macros when calling a tool, how it can acts depending on what your doing' + #10#13 +
    'in Embarcadero Dev-C++. For example, if you are willing to add a tool to Embarcadero Dev-C++' + #10#13 +
    'that can compress executable files, you may need to know the filename of your project''s executable' + #10#13 +
    'that when calling the tool it automatically compress the current project''s executable.' + #10#13 +
    'You can use many different parameters macros for your tool, for more information on' + #10#13 +
    'what they can do see the Macro lists on the previous dialog.'
    , 'Quick help on macros',
    MB_ICONINFORMATION);
end;

procedure TToolEditForm.btnInsertClick(Sender: TObject);
begin
  if lstMacro.itemindex > -1 then
    fMacroTarget.SelText := lstMacro.Items[lstMacro.itemindex];
end;

procedure TToolEditForm.lstMacroClick(Sender: TObject);
begin
  lblDesc.Text := Lang[lstMacro.ItemIndex + ID_ET_MACROS];
end;

procedure TToolEditForm.btnProgClick(Sender: TObject);
begin
  with TOpenDialog.Create(self) do try
    Filter := 'Applications (*.exe;*.bat;*.com;)|*.exe;*.bat;*.com|All files (*.*)|*.*';

    if Assigned(MainForm.Project) then
      InitialDir := MainForm.Project.Directory;

    if Execute then begin
      edProgram.Text := FileName;
      edWorkDir.Text := ExtractFilePath(FileName);
    end;
  finally
    Free;
  end;
end;

procedure TToolEditForm.btnWorkDirClick(Sender: TObject);
var
  new: String;
begin
  if (Trim(edWorkDir.Text) <> '') and SysUtils.DirectoryExists(Trim(edWorkDir.Text)) then
    new := edWorkDir.Text
  else
    new := ExtractFilePath(edProgram.Text);
  if NewSelectDirectory('Select Working Dir', '', new) then
    edWorkDir.text := New;
end;

procedure TToolEditForm.EditEnter(Sender: TObject);
begin
  fMacroTarget := Sender as TEdit;
end;

procedure TToolEditForm.FormCreate(Sender: TObject);
begin
  fMacroTarget := edParams;
  LoadText;
end;

procedure TToolEditForm.LoadText;
begin
  // Set interface font
  Font.Name := devData.InterfaceFont;
  Font.Size := devData.InterfaceFontSize;

  Caption := Lang[ID_TE];
  lblTitle.Caption := Lang[ID_TE_TITLE];
  lblProg.Caption := Lang[ID_TE_PROG];
  lblWorkDir.Caption := Lang[ID_TE_WORK];
  lblParam.Caption := Lang[ID_TE_PARAM];
  lblMacros.Caption := Lang[ID_TE_AVAIL];

  btnInsert.Caption := Lang[ID_TE_INSERT];

  btnOk.Caption := Lang[ID_BTN_OK];
  btnCancel.Caption := Lang[ID_BTN_CANCEL];
  btnHelp.Caption := Lang[ID_BTN_HELP];
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

