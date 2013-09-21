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

unit NewClassFm;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, XPMenu;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Variants, Classes, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls, QButtons;
{$ENDIF}

type
  TNewClassForm = class(TForm)
    Label1: TLabel;
    txtName: TEdit;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    cmbClass: TComboBox;
    Label4: TLabel;
    txtCppFile: TEdit;
    Label5: TLabel;
    txtHFile: TEdit;
    btnBrowseCpp: TSpeedButton;
    btnBrowseH: TSpeedButton;
    chkAddToProject: TCheckBox;
    btnCreate: TButton;
    btnCancel: TButton;
    cmbScope: TComboBox;
    SaveDialog1: TSaveDialog;
    chkInherit: TCheckBox;
    Label6: TLabel;
    txtIncFile: TEdit;
    GroupBox2: TGroupBox;
    Label7: TLabel;
    Label8: TLabel;
    memDescr: TMemo;
    cmbComment: TComboBox;
    Label9: TLabel;
    txtArgs: TEdit;
    XPMenu: TXPMenu;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure chkInheritClick(Sender: TObject);
    procedure txtNameChange(Sender: TObject);
    procedure txtCppFileChange(Sender: TObject);
    procedure btnBrowseCppClick(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
    procedure cmbClassChange(Sender: TObject);
    procedure memDescrChange(Sender: TObject);
    procedure txtNameKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    procedure LoadText;
  public
    { Public declarations }
  end;

var
  NewClassForm: TNewClassForm;

implementation

uses 
  main, CppParser, MultiLangSupport, version, editor, devcfg;

{$R *.dfm}

procedure TNewClassForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TNewClassForm.FormShow(Sender: TObject);
var
  sl: TStrings;
begin
  LoadText;
  txtName.Text := '';
  txtArgs.Text := '';

  cmbScope.ItemIndex := 2;
  sl := TStringList.Create;
  try
    MainForm.CppParser1.GetClassesList(sl);
    cmbClass.Items.Assign(sl);
  finally
    sl.Free;
  end;
  if Assigned(MainForm.ClassBrowser1.Selected) and (PStatement(MainForm.ClassBrowser1.Selected.Data)^._Kind = skClass) then begin
    cmbClass.ItemIndex := cmbClass.Items.IndexOf(PStatement(MainForm.ClassBrowser1.Selected.Data)^._Command);
    chkInherit.Checked := True;
  end
  else begin
    cmbClass.Text := '';
    chkInherit.Checked := False;
  end;

  txtIncFile.Text := '';
  txtCppFile.Text := '';
  txtHFile.Text := '';
  chkAddToProject.Checked := True;

  memDescr.Lines.Clear;
  cmbComment.ItemIndex := 1;

  chkInheritClick(nil);
  txtNameChange(nil);
  cmbClassChange(nil);
  txtName.SetFocus;
end;

procedure TNewClassForm.chkInheritClick(Sender: TObject);
begin
  cmbScope.Enabled := chkInherit.Checked;
  cmbClass.Enabled := chkInherit.Checked;
end;

procedure TNewClassForm.txtNameChange(Sender: TObject);
begin
  if txtName.Text <> '' then begin
    txtCppFile.Text := MainForm.fProject.Directory + LowerCase(txtName.Text) + '.cpp';
    txtHFile.Text := MainForm.fProject.Directory + LowerCase(txtName.Text) + '.h';
  end
  else begin
    txtCppFile.Text := '';
    txtHFile.Text := '';
  end;
  btnCreate.Enabled := (txtName.Text <> '') and
    (txtCppFile.Text <> '') and
    (txtHFile.Text <> '');
end;

procedure TNewClassForm.txtCppFileChange(Sender: TObject);
begin
  btnCreate.Enabled := (txtName.Text <> '') and
    (txtCppFile.Text <> '') and
    (txtHFile.Text <> '');
end;

procedure TNewClassForm.btnBrowseCppClick(Sender: TObject);
begin
  if Sender = btnBrowseCpp then begin
    SaveDialog1.FileName := ExtractFileName(txtCppFile.Text);
    SaveDialog1.InitialDir := ExtractFilePath(txtCppFile.Text);
    SaveDialog1.Filter := FLT_CPPS;
    SaveDialog1.DefaultExt := 'cpp';
  end
  else begin
    SaveDialog1.FileName := ExtractFileName(txtHFile.Text);
    SaveDialog1.InitialDir := ExtractFilePath(txtHFile.Text);
    SaveDialog1.Filter := FLT_HEADS;
    SaveDialog1.DefaultExt := 'h';
  end;
  if SaveDialog1.Execute then begin
    if Sender = btnBrowseCpp then
      txtCppFile.Text := SaveDialog1.FileName
    else
      txtHFile.Text := SaveDialog1.FileName;
  end;
end;

procedure TNewClassForm.btnCreateClick(Sender: TObject);
var
  idx: integer;
  I: integer;
  e: TEditor;
  S: string;
  hfName: string;
  hFile: integer;
  st: PStatement;
begin
  // HEADER FILE IMPLEMENTATION
  if chkAddToProject.Checked then begin
    idx := MainForm.fProject.NewUnit(False, txtHFile.Text);
    e := MainForm.fProject.OpenUnit(idx);
    if idx = -1 then begin
      MessageDlg('Cannot add header file to project...', mtError, [mbOk], 0);
      Exit;
    end;
  end
  else begin
    hFile := FileCreate(txtHFile.Text);
    if hFile > 0 then begin
      FileClose(hFile);
      e := MainForm.GetEditorFromFileName(txtHFile.Text);
    end
    else begin
      MessageDlg('Cannot create header file...', mtError, [mbOk], 0);
      Exit;
    end;
  end;

  if not Assigned(e) then begin
    MessageDlg('Cannot open header file in editor...', mtError, [mbOk], 0);
    Exit;
  end;

  hfName := UpperCase(ExtractFileName(txtHFile.Text));
  hfName := StringReplace(hfName, '.', '_', [rfReplaceAll]);
  hfName := StringReplace(hfName, ' ', '_', [rfReplaceAll]);

  e.Text.Lines.Append('// Class automatically generated by Dev-C++ New Class wizard');
  e.Text.Lines.Append('');
  e.Text.Lines.Append('#ifndef ' + hfName);
  e.Text.Lines.Append('#define ' + hfName);
  e.Text.Lines.Append('');
  if chkInherit.Checked and (txtIncFile.Text <> '') then begin
    st := nil;
    for idx := 0 to MainForm.CppParser1.Statements.Count - 1 do begin
      st := MainForm.CppParser1.Statements[idx];
      if (st^._Kind = skClass) and (st^._ScopelessCmd = cmbClass.Text) and (MainForm.fProject.Units.Indexof(MainForm.CppParser1.GetDeclarationFileName(st)) <> -1) then
        Break;
      st:=nil;
    end;
    if Assigned(st) then
      e.Text.Lines.Append('#include "' + txtIncFile.Text + '" // inheriting class''s header file')
    else
      e.Text.Lines.Append('#include <' + txtIncFile.Text + '> // inheriting class''s header file');
    e.Text.Lines.Append('');
  end;
  S := 'class ' + txtName.Text;
  if chkInherit.Checked and (cmbClass.Text <> '') then
    S := S + ' : ' + cmbScope.Text + ' ' + cmbClass.Text;

  // insert the comment
  if Trim(memDescr.Text) = '' then
    memDescr.Text := 'No description';
  if cmbComment.ItemIndex = 0 then // /** ... */
    e.Text.Lines.Append('/**')
  else if cmbComment.ItemIndex = 1 then // /* ... */
    e.Text.Lines.Append('/*');
  for I := 0 to memDescr.Lines.Count - 1 do
    if cmbComment.ItemIndex in [0, 1] then // /** ... */ or /* ... */
      e.Text.Lines.Append(' * ' + memDescr.Lines[I])
    else
      e.Text.Lines.Append('// ' + memDescr.Lines[I]);
  if cmbComment.ItemIndex in [0, 1] then // /** ... */ or /* ... */
    e.Text.Lines.Append(' */');

  e.Text.Lines.Append(S);
  e.Text.Lines.Append('{');
  e.Text.Lines.Append(#9'public:');
  e.Text.Lines.Append(#9#9'// class constructor');
  e.Text.Lines.Append(#9#9 + txtName.Text + '(' + txtArgs.Text + ');');
  e.Text.Lines.Append(#9#9'// class destructor');
  e.Text.Lines.Append(#9#9'~' + txtName.Text + '();');
  e.Text.Lines.Append('};');
  e.Text.Lines.Append('');
  e.Text.Lines.Append('#endif // ' + hfName);
  e.Text.Lines.Append('');

  e.Modified := True;

  // CPP FILE IMPLEMENTATION
  if chkAddToProject.Checked then begin
    idx := MainForm.fProject.NewUnit(False, txtCppFile.Text);
    e := MainForm.fProject.OpenUnit(idx);
    if idx = -1 then begin
      MessageDlg('Cannot add implementation file to project...', mtError, [mbOk], 0);
      Exit;
    end;
  end
  else begin
    hFile := FileCreate(txtCppFile.Text);
    if hFile > 0 then begin
      FileClose(hFile);
      e := MainForm.GetEditorFromFileName(txtCppFile.Text);
    end
    else begin
      MessageDlg('Cannot create implementation file...', mtError, [mbOk], 0);
      Exit;
    end;
  end;

  if not Assigned(e) then begin
    MessageDlg('Cannot open implementation file in editor...', mtError, [mbOk], 0);
    Exit;
  end;

  e.Text.Lines.Append('// Class automatically generated by Dev-C++ New Class wizard');
  e.Text.Lines.Append('');
  e.Text.Lines.Append('#include "' + ExtractFileName(txtHFile.Text) + '" // class''s header file');
  e.Text.Lines.Append('');
  e.Text.Lines.Append('// class constructor');
  e.Text.Lines.Append(txtName.Text + '::' + txtName.Text + '(' + txtArgs.Text + ')');
  e.Text.Lines.Append('{');
  e.Text.Lines.Append(#9'// insert your code here');
  e.Text.Lines.Append('}');
  e.Text.Lines.Append('');
  e.Text.Lines.Append('// class destructor');
  e.Text.Lines.Append(txtName.Text + '::~' + txtName.Text + '()');
  e.Text.Lines.Append('{');
  e.Text.Lines.Append(#9'// insert your code here');
  e.Text.Lines.Append('}');
  e.Text.Lines.Append('');

  e.Modified := True;
end;

procedure TNewClassForm.cmbClassChange(Sender: TObject);
var
  st: PStatement;
begin
  if cmbClass.Items.IndexOf(cmbClass.Text) <> -1 then begin
    st := PStatement(cmbClass.Items.Objects[cmbClass.Items.IndexOf(cmbClass.Text)]);
    if Assigned(st) then
      txtIncFile.Text := ExtractFileName(MainForm.CppParser1.GetDeclarationFileName(st))
    else
      txtIncFile.Text := LowerCase(cmbClass.Text) + '.h';
  end
  else begin
    if cmbClass.Text <> '' then
      txtIncFile.Text := LowerCase(cmbClass.Text) + '.h'
    else
      txtIncFile.Text := '';
  end;
end;

procedure TNewClassForm.memDescrChange(Sender: TObject);
begin
{ ???
  if memDescr.Lines.Count > 1 then
    cmbComment.ItemIndex := 0
  else
    cmbComment.ItemIndex := 1; }
end;

procedure TNewClassForm.txtNameKeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key in ['_', 'a'..'z', 'A'..'Z', '0'..'9', #1..#31]) then begin
    Key := #0;
    Abort;
  end;
end;

procedure TNewClassForm.LoadText;
begin
  if devData.XPTheme then
    XPMenu.Active := true
  else
    XPMenu.Active := false;
  Caption := Lang[ID_POP_NEWCLASS];
  Label1.Caption := Lang[ID_NEWCLASS_NAME];
  Label9.Caption := Lang[ID_NEWMEMB_ARGS];
  chkInherit.Caption := Lang[ID_NEWCLASS_INHERIT];
  GroupBox1.Caption := Lang[ID_NEWCLASS_INHERITANCE];
  Label2.Caption := Lang[ID_NEWVAR_SCOPE] + ':';
  Label3.Caption := Lang[ID_NEWCLASS_INHERIT_FROM];
  Label6.Caption := Lang[ID_NEWCLASS_INHERIT_HDR];
  Label4.Caption := Lang[ID_NEWCLASS_CPPFILE];
  Label5.Caption := Lang[ID_NEWCLASS_HFILE];
  chkAddToProject.Caption := Lang[ID_NEWCLASS_ADDTOPROJ];
  GroupBox2.Caption := Lang[ID_NEWVAR_COMMENTS];
  Label7.Caption := Lang[ID_NEWVAR_COMMENTSDESCR];
  Label8.Caption := Lang[ID_NEWVAR_COMMENTSSTYLE];
  btnCreate.Caption := Lang[ID_NEWVAR_BTN_CREATE];
  btnCancel.Caption := Lang[ID_NEWVAR_BTN_CANCEL];
end;

end.

