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

unit NewMemberFm;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, CppParser, XPMenu;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Variants, Classes, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls, QExtCtrls, CppParser;
{$ENDIF}

type
  TNewMemberForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    rgScope: TRadioGroup;
    cmbType: TComboBox;
    txtName: TEdit;
    Label3: TLabel;
    txtArguments: TEdit;
    btnCreate: TButton;
    btnCancel: TButton;
    Label4: TLabel;
    cmbClass: TComboBox;
    grpComment: TGroupBox;
    Label5: TLabel;
    Label7: TLabel;
    memDescr: TMemo;
    cmbComment: TComboBox;
    chkToDo: TCheckBox;
    XPMenu: TXPMenu;
    grpAttr: TGroupBox;
    chkStatic: TCheckBox;
    chkVirtual: TCheckBox;
    chkPure: TCheckBox;
    chkInline: TCheckBox;
    procedure cmbTypeChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
    procedure memDescrChange(Sender: TObject);
    procedure chkStaticClick(Sender: TObject);
  private
    { Private declarations }
    procedure LoadText;
  public
    { Public declarations }
  end;

var
  NewMemberForm: TNewMemberForm;

implementation

uses 
  editor, main, MultiLangSupport, devcfg;

{$R *.dfm}

procedure TNewMemberForm.cmbTypeChange(Sender: TObject);
begin
  btnCreate.Enabled := cmbType.Text <> '';
  btnCreate.Enabled := btnCreate.Enabled and (txtName.Text <> '');
end;

procedure TNewMemberForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TNewMemberForm.FormShow(Sender: TObject);
var
  sl: TStrings;
begin
  LoadText;
  cmbClass.Text := '';
  cmbType.Text := '';
  txtName.Text := '';
  txtArguments.Text := '';
  chkToDo.Checked := True;
  memDescr.Lines.Clear;
  rgScope.ItemIndex := 2;

  sl := TStringList.Create;
  try
    MainForm.CppParser1.GetClassesList(sl);
    cmbClass.Items.Assign(sl);
  finally
    sl.Free;
  end;
  cmbClass.ItemIndex := cmbClass.Items.IndexOf(PStatement(MainForm.ClassBrowser1.Selected.Data)^._Command);

  cmbType.SetFocus;
end;

procedure TNewMemberForm.btnCreateClick(Sender: TObject);
var
  pID: integer;
  fName: string;
  CppFname: string;
  I: integer;
  Line: integer;
  e: TEditor;
  AddScopeStr: boolean;
  S: string;
  VarName: string;
  VarType: string;
  VarArguments: string;
  VarScope: TStatementClassScope;
  St: PStatement;
  ClsName: string;
  wa: boolean;
begin
  if cmbClass.ItemIndex = -1 then begin
    MessageDlg(Lang[ID_NEWVAR_MSG_NOCLASS], mtError, [mbOk], 0);
    ModalResult := mrNone;
    Exit;
  end;

  if cmbClass.Items.IndexOf(cmbClass.Text) > -1 then
    St := PStatement(cmbClass.Items.Objects[cmbClass.Items.IndexOf(cmbClass.Text)])
  else
    St := nil;

  if not Assigned(St) then begin
    MessageDlg(Lang[ID_NEWVAR_MSG_NOCLASS], mtError, [mbOk], 0);
    ModalResult := mrNone;
    Exit;
  end;

  with MainForm do begin
    pID := St^._ID;

    VarName := txtName.Text;
    VarType := cmbType.Text;
    case rgScope.ItemIndex of
      0: VarScope := scsPrivate;
      1: VarScope := scsProtected;
      2: VarScope := scsPublic;
      3: VarScope := scsPublished;
    else
      VarScope := scsPublic;
    end;
    VarArguments := txtArguments.Text;
    if Trim(memDescr.Text) = '' then
      memDescr.Text := 'No description';

    CppParser1.GetSourcePair(CppParser1.GetDeclarationFileName(St), CppFname, fName);

    if not chkInline.Checked and not FileExists(CppFname) then begin
      MessageDlg(Lang[ID_NEWVAR_MSG_NOIMPL], mtError, [mbOk], 0);
      Exit;
    end;

    // if the file is modified, ask to save
    e := GetEditorFromFileName(fName);

    if not Assigned(e) then
      Exit;

    if e.Modified then
      case MessageDlg(format(Lang[ID_MSG_ASKSAVECLOSE], [fName]), mtConfirmation, [mbYes, mbCancel], 0) of
        mrYes: if FileExists(fName) then begin
            wa := MainForm.devFileMonitor1.Active;
            MainForm.devFileMonitor1.Deactivate;
            if devEditor.AppendNewline then
              with e.Text do
                if Lines.Count > 0 then
                  if Lines[Lines.Count -1] <> '' then
                    Lines.Add('');
            e.Text.Lines.SaveToFile(fName);
            if wa then
              MainForm.devFileMonitor1.Activate;
          end
          else
            Exit;
        mrCancel: Exit;
      end;

    // Ask CppParser for insertion line suggestion ;)
    Line := CppParser1.SuggestMemberInsertionLine(pID, VarScope, AddScopeStr);

    if Line = -1 then begin
    // CppParser could not suggest a line for insertion :(
      MessageDlg(Lang[ID_NEWVAR_MSG_NOLINE], mtError, [mbOk], 0);
      Exit;
    end;

    if not Assigned(e) then
      Exit;

    // insert the actual function
    S := #9#9;
    if chkVirtual.Checked then
      S := S + 'virtual '
    else if chkStatic.Checked then
      S := S + 'static ';
    S := S + VarType + ' ' + VarName + '(' + VarArguments + ')';
    if chkPure.Checked then
      S := S + ' = 0L';
    if chkInline.Checked then begin
      e.Text.Lines.Insert(Line, #9#9'}');
      if chkToDo.Checked then
        e.Text.Lines.Insert(Line, #9#9#9'/* TODO (#1#): Implement inline function ' + cmbClass.Text + '::' + VarName + '(' + VarArguments + ') */')
      else
        e.Text.Lines.Insert(Line, #9#9#9'// insert your code here');
      e.Text.Lines.Insert(Line, #9#9'{');
    end
    else
      S := S + ';';

    e.Text.Lines.Insert(Line, S);

    // insert the comment
    if cmbComment.ItemIndex in [0, 1] then // /** ... */ or /* ... */
      e.Text.Lines.Insert(Line, #9#9' */');
    for I := memDescr.Lines.Count - 1 downto 0 do
      if cmbComment.ItemIndex in [0, 1] then // /** ... */ or /* ... */
        e.Text.Lines.Insert(Line, #9#9' * ' + memDescr.Lines[I])
      else
        e.Text.Lines.Insert(Line, #9#9'// ' + memDescr.Lines[I]);
    if cmbComment.ItemIndex = 0 then // /** ... */
      e.Text.Lines.Insert(Line, #9#9'/**')
    else if cmbComment.ItemIndex = 1 then // /* ... */
      e.Text.Lines.Insert(Line, #9#9'/*');

    // insert, if needed, the scope string
    if AddScopeStr then
      case VarScope of
        scsPrivate: e.Text.Lines.Insert(Line, #9'private:');
        scsProtected: e.Text.Lines.Insert(Line, #9'protected:');
        scsPublic: e.Text.Lines.Insert(Line, #9'public:');
        scsPublished: e.Text.Lines.Insert(Line, #9'published:');
      end;

    e.GotoLineNr(Line + 1);
    e.Modified := True;

    if chkInline.Checked or chkPure.Checked then
      Exit;

    // set the parent class's name
    ClsName := cmbClass.Text;

    e := GetEditorFromFileName(CppFname);
    if not Assigned(e) then
      Exit;

    // insert the implementation
    if Trim(e.Text.Lines[e.Text.Lines.Count - 1]) <> '' then
      e.Text.Lines.Append('');

    // insert the comment
    if cmbComment.ItemIndex = 0 then // /* ... */
      e.Text.Lines.Append('/*');
    for I := 0 to memDescr.Lines.Count - 1 do
      if cmbComment.ItemIndex = 0 then // /* ... */
        e.Text.Lines.Append(' * ' + memDescr.Lines[I])
      else
        e.Text.Lines.Append('// ' + memDescr.Lines[I]);
    if cmbComment.ItemIndex = 0 then // /* ... */
      e.Text.Lines.Append(' */');

    e.Text.Lines.Append(VarType + ' ' + ClsName + '::' + VarName + '(' + VarArguments + ')');
    e.Text.Lines.Append('{');
    if chkToDo.Checked then
      e.Text.Lines.Append(#9'/* TODO (#1#): Implement ' + ClsName + '::' + VarName + '() */')
    else
      e.Text.Lines.Append(#9'// insert your code here');
    e.Text.Lines.Append('}');
    e.Text.Lines.Append('');
    e.GotoLineNr(e.Text.Lines.Count - 1);
    e.Modified := True;
  end;
  Exit;
end;

procedure TNewMemberForm.memDescrChange(Sender: TObject);
begin
  if memDescr.Lines.Count > 1 then
    cmbComment.ItemIndex := 0
  else
    cmbComment.ItemIndex := 2;
end;

procedure TNewMemberForm.LoadText;
begin
  if devData.XPTheme then
    XPMenu.Active := true
  else
    XPMenu.Active := false;
  Caption := Lang[ID_POP_NEWMEMBER];
  Label1.Caption := Lang[ID_NEWVAR_VARTYPE];
  Label2.Caption := Lang[ID_NEWMEMB_MEMBNAME];
  Label3.Caption := Lang[ID_NEWMEMB_ARGS];
  Label4.Caption := Lang[ID_NEWVAR_IMPLIN];
  rgScope.Caption := Lang[ID_NEWVAR_SCOPE];
  grpComment.Caption := Lang[ID_NEWVAR_COMMENTS];
  grpAttr.Caption := Lang[ID_NEWMEMB_ATTRS];
  Label5.Caption := Lang[ID_NEWVAR_COMMENTSDESCR];
  Label7.Caption := Lang[ID_NEWVAR_COMMENTSSTYLE];
  chkToDo.Caption := Lang[ID_ADDTODO_MENUITEM];
  btnCreate.Caption := Lang[ID_NEWVAR_BTN_CREATE];
  btnCancel.Caption := Lang[ID_NEWVAR_BTN_CANCEL];
end;

procedure TNewMemberForm.chkStaticClick(Sender: TObject);
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
  end
  else if Sender = chkVirtual then begin
    chkInline.Enabled := True;
    if not chkVirtual.Checked then
      chkPure.Checked := False
    else
      chkStatic.Checked := False;
  end
  else if Sender = chkPure then begin
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

