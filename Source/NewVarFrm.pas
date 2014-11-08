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

unit NewVarFrm;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;
{$ENDIF}
{$IFDEF LINUX}
SysUtils, Variants, Classes, QGraphics, QControls, QForms,
QDialogs, QStdCtrls, QExtCtrls;
{$ENDIF}

type
  TNewVarForm = class(TForm)
    lblType: TLabel;
    lblName: TLabel;
    rgScope: TRadioGroup;
    txtType: TEdit;
    txtName: TEdit;
    chkReadFunc: TCheckBox;
    chkWriteFunc: TCheckBox;
    Label3: TLabel;
    Label4: TLabel;
    txtReadFunc: TEdit;
    txtWriteFunc: TEdit;
    btnCreate: TButton;
    btnCancel: TButton;
    lblImplementIn: TLabel;
    cmbClass: TComboBox;
    chkInlineR: TCheckBox;
    chkInlineW: TCheckBox;
    grpReadFunc: TGroupBox;
    grpWriteFunc: TGroupBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure txtTypeChange(Sender: TObject);
    procedure chkReadFuncClick(Sender: TObject);
    procedure chkWriteFuncClick(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
    procedure txtWriteFuncKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure txtReadFuncKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    procedure LoadText;
  public
    { Public declarations }
  end;

implementation

uses editor, main, CppParser, MultiLangSupport, devcfg;

{$R *.dfm}

procedure TNewVarForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TNewVarForm.FormShow(Sender: TObject);
var
  sl: TStrings;
begin
  LoadText;

  txtTypeChange(nil);

  sl := TStringList.Create;
  try
    MainForm.CppParser.GetClassesList(sl);
    cmbClass.Items.Assign(sl);
  finally
    sl.Free;
  end;

  chkReadFuncClick(nil);
  chkWriteFuncClick(nil);

  cmbClass.ItemIndex := cmbClass.Items.IndexOf(PStatement(MainForm.ClassBrowser.Selected.Data)^._ScopeCmd);

  txtType.SetFocus;
end;

procedure TNewVarForm.txtTypeChange(Sender: TObject);
begin
  btnCreate.Enabled := (txtType.Text <> '') and (txtName.Text <> '') and (cmbClass.ItemIndex <> -1);

  // If the user didn't fiddle with the get/set controls, update preview
  if txtReadFunc.Tag = 0 then
    txtReadFunc.Text := 'Get' + txtName.Text;
  if txtWriteFunc.Tag = 0 then
    txtWriteFunc.Text := 'Set' + txtName.Text;

  // The user must provide a suitable name for both
  if chkReadFunc.Checked then
    btnCreate.Enabled := btnCreate.Enabled and (txtReadFunc.Text <> '');
  if chkWriteFunc.Checked then
    btnCreate.Enabled := btnCreate.Enabled and (txtWriteFunc.Text <> '');
end;

procedure TNewVarForm.chkReadFuncClick(Sender: TObject);
begin
  chkInlineR.Enabled := chkReadFunc.Checked;
  txtReadFunc.Enabled := chkReadFunc.Checked;
end;

procedure TNewVarForm.chkWriteFuncClick(Sender: TObject);
begin
  chkInlineW.Enabled := chkWriteFunc.Checked;
  txtWriteFunc.Enabled := chkWriteFunc.Checked;
end;

procedure TNewVarForm.btnCreateClick(Sender: TObject);
var
  fName: AnsiString;
  CppFname: AnsiString;
  Line: integer;
  GetSetLine: integer;
  e: TEditor;
  AddScopeStr: boolean;
  GetSetAddScopeStr: boolean;
  VarScope: TStatementClassScope;
  St: PStatement;
  S: AnsiString;
begin

  // Check below shouldn't be needed
  st := PStatement(cmbClass.Items.Objects[cmbClass.Items.IndexOf(cmbClass.Text)]);
  if not Assigned(st) then begin
    MessageDlg(Lang[ID_NEWVAR_MSG_NOCLASS], mtError, [mbOk], 0);
    Exit;
  end;

  // We need a CPP file if we want to define getters or setters it over there
  MainForm.CppParser.GetSourcePair(MainForm.CppParser.GetDeclarationFileName(st), CppFname, fName);
  if (not chkInlineR.Checked or not chkInlineW.Checked) and not FileExists(CppFname) then begin
    MessageDlg(Lang[ID_NEWVAR_MSG_NOIMPL], mtError, [mbOk], 0);
    Exit;
  end;

  // Open header file
  e := MainForm.EditorList.GetEditorFromFileName(fName);
  if not Assigned(e) then
    Exit;

  // Ask CppParser for insertion line suggestion of variable
  case rgScope.ItemIndex of
    0: VarScope := scsPrivate;
    1: VarScope := scsProtected;
    2: VarScope := scsPublic;
  else
    VarScope := scsNone; // shut up compiler
  end;
  Line := MainForm.CppParser.SuggestMemberInsertionLine(st^._ID, VarScope, AddScopeStr);
  if Line = -1 then begin
    MessageDlg(Lang[ID_NEWVAR_MSG_NOLINE], mtError, [mbOk], 0);
    Exit;
  end;

  // Ask CppParser for insertion line suggestion of getter/setter
  if chkReadFunc.Checked or chkWriteFunc.Checked then begin
    GetSetLine := MainForm.CppParser.SuggestMemberInsertionLine(st^._ID, scsPublic, GetSetAddScopeStr);
    if Line = -1 then begin
      MessageDlg(Lang[ID_NEWVAR_MSG_NOLINE], mtError, [mbOk], 0);
      Exit;
    end;
  end else
    GetSetLine := 0; // shut up compiler warning

  e.Text.BeginUpdate;
  try
    e.Text.Lines.Insert(Line, #9#9 + txtType.Text + ' ' + txtName.Text + ';');

    // insert, if needed, the scope string
    if AddScopeStr then
      case VarScope of
        scsPrivate: e.Text.Lines.Insert(Line, #9'private:');
        scsProtected: e.Text.Lines.Insert(Line, #9'protected:');
        scsPublic: e.Text.Lines.Insert(Line, #9'public:');
      end;

    // if needed, insert a getter in the class body
    if chkReadFunc.Checked then begin
      S := #9#9 + txtType.Text + ' ' + txtReadFunc.Text + '()';
      if chkInlineR.Checked then begin
        e.Text.Lines.Insert(GetSetLine, #9#9'}');
        e.Text.Lines.Insert(GetSetLine, #9#9#9'return ' + txtName.Text + ';');
        e.Text.Lines.Insert(GetSetLine, #9#9'{');
      end else
        S := S + '; // returns the value of ' + txtName.Text;
      e.Text.Lines.Insert(GetSetLine, S);
      if GetSetAddScopeStr then
        e.Text.Lines.Insert(GetSetLine, #9'public:');
    end;

    // if needed, insert a setter in the class body
    if chkWriteFunc.Checked then begin
      S := #9#9'void ' + txtWriteFunc.Text + '(' + txtType.Text + ' x)';
      if chkInlineW.Checked then begin
        e.Text.Lines.Insert(GetSetLine, #9#9'}');
        e.Text.Lines.Insert(GetSetLine, #9#9#9 + txtName.Text + ' = x;');
        e.Text.Lines.Insert(GetSetLine, #9#9'{');
      end else
        S := S + '; // sets the value of ' + txtName.Text;
      e.Text.Lines.Insert(GetSetLine, S);
      if GetSetAddScopeStr then
        e.Text.Lines.Insert(GetSetLine, #9'public:');
    end;

    // Mark modified and we're done
    e.SetCaretPos(Line + 1, 1);
    e.Text.Modified := True;
  finally
    e.Text.EndUpdate;
  end;

  // Only mess with the CPP file if we need to
  if not ((chkInlineR.Enabled and not chkInlineR.Checked) or (chkInlineW.Enabled and not chkInlineW.Checked)) then begin
    e.Activate; // activate it if we're not modifying the source file
    Exit;
  end;

  e := MainForm.EditorList.GetEditorFromFileName(CppFname);
  if not Assigned(e) then
    Exit;

  e.Text.BeginUpdate;
  try
    // if needed, insert a getter in the source file
    if (txtReadFunc.Text <> '') and not chkInlineR.Checked then begin
      e.Text.Lines.Add('');
      e.Text.Lines.Add('// returns the value of ' + txtName.Text);
      e.Text.Lines.Add(txtType.Text + ' ' + cmbClass.Text + '::' + txtReadFunc.Text + '()');
      e.Text.Lines.Add('{');
      e.Text.Lines.Add(#9'return ' + txtName.Text + ';');
      e.Text.Lines.Add('}');
    end;

    // if needed, insert a setter in the source file
    if (txtReadFunc.Text <> '') and not chkInlineW.Checked then begin
      e.Text.Lines.Add('');
      e.Text.Lines.Add('// sets the value of ' + txtName.Text);
      e.Text.Lines.Add('void ' + cmbClass.Text + '::' + txtWriteFunc.Text + '(' + txtType.Text + ' x)');
      e.Text.Lines.Add('{');
      e.Text.Lines.Add(#9 + txtName.Text + ' = x;');
      e.Text.Lines.Add('}');
    end;

    e.SetCaretPos(e.Text.Lines.Count - 1, 1);
    e.Activate;
    e.Text.Modified := True;
  finally
    e.Text.Lines.EndUpdate;
  end;
end;

procedure TNewVarForm.LoadText;
begin

  // Set interface font
  Font.Name := devData.InterfaceFont;
  Font.Size := devData.InterfaceFontSize;

  Caption := Lang[ID_POP_NEWVAR];
  lblType.Caption := Lang[ID_NEWVAR_VARTYPE];
  lblName.Caption := Lang[ID_NEWVAR_VARNAME];
  lblImplementIn.Caption := Lang[ID_NEWVAR_IMPLIN];
  rgScope.Caption := Lang[ID_NEWVAR_SCOPE];
  chkReadFunc.Caption := Lang[ID_NEWVAR_CREATEREADFUNC];
  Label3.Caption := Lang[ID_NEWVAR_FUNCNAME];
  chkWriteFunc.Caption := Lang[ID_NEWVAR_CREATEWRITEFUNC];
  Label4.Caption := Lang[ID_NEWVAR_FUNCNAME];
  btnCreate.Caption := Lang[ID_NEWVAR_BTN_CREATE];
  btnCancel.Caption := Lang[ID_NEWVAR_BTN_CANCEL];
end;

procedure TNewVarForm.txtReadFuncKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if txtReadFunc.Text <> '' then
    txtReadFunc.Tag := 1 // mark user-modified
  else
    txtReadFunc.Tag := 0; // reset
end;

procedure TNewVarForm.txtWriteFuncKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if txtWriteFunc.Text <> '' then
    txtWriteFunc.Tag := 1 // mark user-modified
  else
    txtWriteFunc.Tag := 0; // reset
end;

end.

