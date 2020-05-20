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

unit NewFunctionFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, CppParser, CBUtils;

type
  TNewFunctionForm = class(TForm)
    lblReturnType: TLabel;
    lblName: TLabel;
    rgScope: TRadioGroup;
    txtType: TEdit;
    txtName: TEdit;
    lblArguments: TLabel;
    txtArguments: TEdit;
    btnCreate: TButton;
    btnCancel: TButton;
    lblImplementIn: TLabel;
    cmbClass: TComboBox;
    chkToDo: TCheckBox;
    grpAttr: TGroupBox;
    chkStatic: TCheckBox;
    chkVirtual: TCheckBox;
    chkPure: TCheckBox;
    chkInline: TCheckBox;
    procedure txtTypeChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
    procedure chkStaticClick(Sender: TObject);
  private
    { Private declarations }
    procedure LoadText;
  public
    { Public declarations }
  end;

implementation

uses
  editor, main, MultiLangSupport, devcfg;

{$R *.dfm}

procedure TNewFunctionForm.txtTypeChange(Sender: TObject);
begin
  btnCreate.Enabled := (txtType.Text <> '') and (txtName.Text <> '') and (cmbClass.ItemIndex <> -1);
end;

procedure TNewFunctionForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TNewFunctionForm.FormShow(Sender: TObject);
var
  sl: TStringList;
begin
  LoadText;

  sl := TStringList.Create;
  try
    MainForm.CppParser.GetClassesList(sl);
    cmbClass.Items.Assign(sl);
  finally
    sl.Free;
  end;

  cmbClass.ItemIndex := cmbClass.Items.IndexOf(PStatement(MainForm.ClassBrowser.Selected.Data)^._Command);

  txtType.SetFocus;
end;

procedure TNewFunctionForm.btnCreateClick(Sender: TObject);
var
  fName: String;
  CppFname: String;
  Line: integer;
  e: TEditor;
  AddScopeStr: boolean;
  S: String;
  VarScope: TStatementClassScope;
  st: PStatement;
begin

  // Check below shouldn't be needed
  st := PStatement(cmbClass.Items.Objects[cmbClass.Items.IndexOf(cmbClass.Text)]);
  if not Assigned(St) then begin
    MessageDlg(Lang[ID_NEWVAR_MSG_NOCLASS], mtError, [mbOk], 0);
    ModalResult := mrNone;
    Exit;
  end;

  // We need a CPP file if we want to define it over there
  MainForm.CppParser.GetSourcePair(st^._DefinitionFileName, CppFname, fName);
  if not chkInline.Checked and not FileExists(CppFname) then begin
    MessageDlg(Lang[ID_NEWVAR_MSG_NOIMPL], mtError, [mbOk], 0);
    Exit;
  end;

  // Open header file
  e := MainForm.EditorList.GetEditorFromFileName(fName);
  if not Assigned(e) then
    Exit;

  // Ask CppParser for insertion line suggestion
  case rgScope.ItemIndex of
    0: VarScope := scsPrivate;
    1: VarScope := scsProtected;
    2: VarScope := scsPublic;
  else
    VarScope := scsNone; // shut up compiler
  end;
  Line := MainForm.CppParser.SuggestMemberInsertionLine(st, VarScope, AddScopeStr);
  if Line = -1 then begin
    MessageDlg(Lang[ID_NEWVAR_MSG_NOLINE], mtError, [mbOk], 0);
    Exit;
  end;

  // insert the actual function in the header
  S := #9#9;
  if chkVirtual.Checked then
    S := S + 'virtual '
  else if chkStatic.Checked then
    S := S + 'static ';

  S := S + txtType.Text + ' ' + txtName.Text + '(' + txtArguments.Text + ')';
  if chkPure.Checked then
    S := S + ' = 0';

  // Implement it in the class in the header file if needed
  e.Text.BeginUpdate;
  try
    e.Text.UncollapseAroundLine(Line); // uncollapse folds around this line
    if chkInline.Checked then begin
      e.Text.Lines.Insert(Line, #9#9'}');
      if chkToDo.Checked then
        e.Text.Lines.Insert(Line, #9#9#9'/* TODO (#1#): Define ' + cmbClass.Text + '::' + txtName.Text + '(' +
          txtArguments.Text + ') */');
      e.Text.Lines.Insert(Line, #9#9'{');
    end else
      S := S + ';';
    e.Text.Lines.Insert(Line, S);

    // insert, if needed, the scope string
    if AddScopeStr then
      case VarScope of
        scsPrivate: e.Text.Lines.Insert(Line, #9'private:');
        scsProtected: e.Text.Lines.Insert(Line, #9'protected:');
        scsPublic: e.Text.Lines.Insert(Line, #9'public:');
      end;

    // Mark modified and we're done editing the header file

    e.Text.Modified := True;
  finally
    e.Text.EndUpdate;
  end;

  // Continue work on the source file if needed
  if chkInline.Checked or chkPure.Checked then begin
    e.SetCaretPosAndActivate(Line + 1, 1); // activate it if we're not modifying the source file
    Exit;
  end;

  e := MainForm.EditorList.GetEditorFromFileName(CppFname);
  if not Assigned(e) then
    Exit;

  // Implement it in the class if needed
  e.Text.BeginUpdate;
  try
    // insert the definition
    if Trim(e.Text.Lines[e.Text.Lines.Count - 1]) <> '' then
      e.Text.Lines.Add('');
    e.Text.Lines.Add(txtType.Text + ' ' + cmbClass.Text + '::' + txtName.Text + '(' + txtArguments.Text + ')');
    e.Text.Lines.Add('{');
    if chkToDo.Checked then
      e.Text.Lines.Add(#9'/* TODO (#1#): Define ' + cmbClass.Text + '::' + txtName.Text + '() */');
    e.Text.Lines.Add('}');

    // Set caret and leave
    e.SetCaretPosAndActivate(e.Text.Lines.Count - 1, 1);
    e.Text.Modified := True;
  finally
    e.Text.EndUpdate;
  end;
end;

procedure TNewFunctionForm.LoadText;
begin
  // Set interface font
  Font.Name := devData.InterfaceFont;
  Font.Size := devData.InterfaceFontSize;

  Caption := Lang[ID_POP_NEWMEMBER];
  lblReturnType.Caption := Lang[ID_NEWVAR_VARTYPE];
  lblName.Caption := Lang[ID_NEWMEMB_MEMBNAME];
  lblArguments.Caption := Lang[ID_NEWMEMB_ARGS];
  lblImplementIn.Caption := Lang[ID_NEWVAR_IMPLIN];
  rgScope.Caption := Lang[ID_NEWVAR_SCOPE];
  grpAttr.Caption := Lang[ID_NEWMEMB_ATTRS];
  chkToDo.Caption := Lang[ID_ADDTODO_MENUITEM];
  btnCreate.Caption := Lang[ID_NEWVAR_BTN_CREATE];
  btnCancel.Caption := Lang[ID_NEWVAR_BTN_CANCEL];
end;

procedure TNewFunctionForm.chkStaticClick(Sender: TObject);
begin
  // disable event
  chkStatic.OnClick := nil;
  chkVirtual.OnClick := nil;
  chkPure.OnClick := nil;
  chkInline.OnClick := nil;

  if Sender = chkStatic then begin
    chkInline.Enabled := True;
    if chkStatic.Checked then begin
      chkVirtual.Checked := False;
      chkPure.Checked := False;
    end;
  end else if Sender = chkVirtual then begin
    chkInline.Enabled := True;
    if not chkVirtual.Checked then
      chkPure.Checked := False
    else
      chkStatic.Checked := False;
  end else if Sender = chkPure then begin
    chkInline.Enabled := not chkPure.Checked;
    chkInline.Checked := False;
    chkStatic.Checked := False;
    chkVirtual.Checked := True;
  end;

  // re-enable event
  chkStatic.OnClick := chkStaticClick;
  chkVirtual.OnClick := chkStaticClick;
  chkPure.OnClick := chkStaticClick;
  chkInline.OnClick := chkStaticClick;
end;

end.

