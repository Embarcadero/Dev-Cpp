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

unit NewVarFm;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, XPMenu;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Variants, Classes, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls, QExtCtrls;
{$ENDIF}

type
  TNewVarForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    rgScope: TRadioGroup;
    cmbType: TComboBox;
    txtName: TEdit;
    chkReadFunc: TCheckBox;
    chkWriteFunc: TCheckBox;
    Label3: TLabel;
    Label4: TLabel;
    txtReadFunc: TEdit;
    txtWriteFunc: TEdit;
    btnCreate: TButton;
    btnCancel: TButton;
    Label6: TLabel;
    cmbClass: TComboBox;
    GroupBox1: TGroupBox;
    Label5: TLabel;
    memDescr: TMemo;
    Label7: TLabel;
    cmbComment: TComboBox;
    XPMenu: TXPMenu;
    chkInlineR: TCheckBox;
    chkInlineW: TCheckBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure cmbTypeChange(Sender: TObject);
    procedure chkReadFuncClick(Sender: TObject);
    procedure chkWriteFuncClick(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
    procedure memDescrChange(Sender: TObject);
  private
    { Private declarations }
    procedure LoadText;
  public
    { Public declarations }
  end;

var
  NewVarForm: TNewVarForm;

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
  cmbType.Text := '';
  txtName.Text := '';
  rgScope.ItemIndex := 0;
  chkReadFunc.Checked := False;
  chkWriteFunc.Checked := False;
  txtReadFunc.Text := '';
  txtWriteFunc.Text := '';
  memDescr.Lines.Clear;
  cmbComment.ItemIndex := 2;

  cmbTypeChange(nil);

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

procedure TNewVarForm.cmbTypeChange(Sender: TObject);
begin
  btnCreate.Enabled := cmbType.Text <> '';
  btnCreate.Enabled := btnCreate.Enabled and (txtName.Text <> '');
  if chkReadFunc.Checked then
    btnCreate.Enabled := btnCreate.Enabled and (txtReadFunc.Text <> '');
  if chkWriteFunc.Checked then
    btnCreate.Enabled := btnCreate.Enabled and (txtWriteFunc.Text <> '');
end;

procedure TNewVarForm.chkReadFuncClick(Sender: TObject);
begin
  if chkReadFunc.Checked then begin
    if btnCreate.Enabled then // type and name are ok
      txtReadFunc.Text := 'Get' + txtName.Text
    else
      chkReadFunc.Checked := False;
  end;
  chkInlineR.Enabled := chkReadFunc.Checked;
  if not chkInlineR.Enabled then
    chkInlineR.Checked := False;
end;

procedure TNewVarForm.chkWriteFuncClick(Sender: TObject);
begin
  if chkWriteFunc.Checked then begin
    if btnCreate.Enabled then // type and name are ok
      txtWriteFunc.Text := 'Set' + txtName.Text
    else
      chkWriteFunc.Checked := False;
  end;
  chkInlineW.Enabled := chkWriteFunc.Checked;
  if not chkInlineW.Enabled then
    chkInlineW.Checked := False;
end;

procedure TNewVarForm.btnCreateClick(Sender: TObject);
var
  pID: integer;
  fName: string;
  CppFname: string;
  I: integer;
  Line: integer;
  PublicLine: integer;
  e: TEditor;
  AddScopeStr: boolean;
  fAddScopeStr: boolean;
  VarName: string;
  VarType: string;
  VarRead: boolean;
  VarReadFunc: string;
  VarWrite: boolean;
  VarWriteFunc: string;
  VarScope: TStatementClassScope;
  St: PStatement;
  ClsName: string;
  wa: boolean;
  S: string;
begin
  if cmbClass.ItemIndex = -1 then begin
    MessageDlg(Lang[ID_NEWVAR_MSG_NOCLASS], mtError, [mbOk], 0);
    Exit;
  end;

  if cmbClass.Items.IndexOf(cmbClass.Text) > -1 then
    St := PStatement(cmbClass.Items.Objects[cmbClass.Items.IndexOf(cmbClass.Text)])
  else
    St := nil;

  if not Assigned(St) then begin
    MessageDlg(Lang[ID_NEWVAR_MSG_NOCLASS], mtError, [mbOk], 0);
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
    VarRead := chkReadFunc.Checked;
    VarReadFunc := txtReadFunc.Text;
    VarWrite := chkWriteFunc.Checked;
    VarWriteFunc := txtWriteFunc.Text;
    if Trim(memDescr.Text) = '' then
      memDescr.Text := 'No description';

    CppParser1.GetSourcePair(CppParser1.GetDeclarationFileName(St), CppFname, fName);

    if not FileExists(CppFname) then begin
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
    if VarScope <> scsPublic then
      PublicLine := CppParser1.SuggestMemberInsertionLine(pID, scsPublic, fAddScopeStr)
    else begin
      fAddScopeStr := AddScopeStr;
      PublicLine := Line;
    end;

    if Line = -1 then begin
    // CppParser could not suggest a line for insertion :(
      MessageDlg(Lang[ID_NEWVAR_MSG_NOLINE], mtError, [mbOk], 0);
      Exit;
    end;

    if not Assigned(e) then
      Exit;

    // insert the actual var
    e.Text.Lines.Insert(Line, #9#9 + VarType + ' ' + VarName + ';');

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

    e.GotoLineNr(Line + memDescr.Lines.Count);
    e.Modified := True;

    // if needed, insert a new member function for READ access to the new var
    if VarRead then begin
      S := #9#9 + VarType + ' ' + VarReadFunc + '()';
      if chkInlineR.Checked then begin
        e.Text.Lines.Insert(PublicLine, #9#9'}');
        e.Text.Lines.Insert(PublicLine, #9#9#9'return ' + VarName + ';');
        e.Text.Lines.Insert(PublicLine, #9#9'{');
      end
      else
        S := S + '; // returns the value of ' + VarName;
      e.Text.Lines.Insert(PublicLine, S);
      if fAddScopeStr then
        e.Text.Lines.Insert(PublicLine, #9'public:');
    end;

    // if needed, insert a new member function for WRITE access to the new var
    if VarWrite then begin
      S := #9#9'void ' + VarWriteFunc + '(' + VarType + ' x)';
      if chkInlineW.Checked then begin
        e.Text.Lines.Insert(PublicLine, #9#9'}');
        e.Text.Lines.Insert(PublicLine, #9#9#9 + VarName + ' = x;');
        e.Text.Lines.Insert(PublicLine, #9#9'{');
      end
      else
        S := S + '; // sets the value of ' + VarName;
      e.Text.Lines.Insert(PublicLine, S);
      if fAddScopeStr then
        e.Text.Lines.Insert(PublicLine, #9'public:');
    end;

    // set the parent class's name
    ClsName := cmbClass.Text;

    if ((not VarRead) or (VarRead and chkInlineR.Checked)) and
      ((not VarWrite) or (VarWrite and chkInlineW.Checked)) then
      Exit;

    e := GetEditorFromFileName(CppFname);
    if not Assigned(e) then
      Exit;

    // if needed, insert a new member function for READ access to the new var
    if VarRead and not chkInlineR.Checked then begin
      e.Text.Lines.Append('');
      e.Text.Lines.Append('// returns the value of ' + VarName);
      e.Text.Lines.Append(VarType + ' ' + ClsName + '::' + VarReadFunc + '()');
      e.Text.Lines.Append('{');
      e.Text.Lines.Append(#9'return ' + VarName + ';');
      e.Text.Lines.Append('}');
      e.Text.Lines.Append('');
      e.GotoLineNr(e.Text.Lines.Count - 1);
      e.Modified := True;
    end;

    // if needed, insert a new member function for WRITE access to the new var
    if VarWrite and not chkInlineW.Checked then begin
      e.Text.Lines.Append('');
      e.Text.Lines.Append('// sets the value of ' + VarName);
      e.Text.Lines.Append('void ' + ClsName + '::' + VarWriteFunc + '(' + VarType + ' x)');
      e.Text.Lines.Append('{');
      e.Text.Lines.Append(#9 + VarName + ' = x;');
      e.Text.Lines.Append('}');
      e.Text.Lines.Append('');
      e.GotoLineNr(e.Text.Lines.Count - 1);
      e.Modified := True;
    end;
  end;
  Exit;
end;

procedure TNewVarForm.memDescrChange(Sender: TObject);
begin
  if memDescr.Lines.Count > 1 then
    cmbComment.ItemIndex := 0
  else
    cmbComment.ItemIndex := 2;
end;

procedure TNewVarForm.LoadText;
begin
  if devData.XPTheme then
    XPMenu.Active := true
  else
    XPMenu.Active := false;
  Caption := Lang[ID_POP_NEWVAR];
  Label1.Caption := Lang[ID_NEWVAR_VARTYPE];
  Label2.Caption := Lang[ID_NEWVAR_VARNAME];
  Label6.Caption := Lang[ID_NEWVAR_IMPLIN];
  rgScope.Caption := Lang[ID_NEWVAR_SCOPE];
  chkReadFunc.Caption := Lang[ID_NEWVAR_CREATEREADFUNC];
  Label3.Caption := Lang[ID_NEWVAR_FUNCNAME];
  chkWriteFunc.Caption := Lang[ID_NEWVAR_CREATEWRITEFUNC];
  Label4.Caption := Lang[ID_NEWVAR_FUNCNAME];
  GroupBox1.Caption := Lang[ID_NEWVAR_COMMENTS];
  Label5.Caption := Lang[ID_NEWVAR_COMMENTSDESCR];
  Label7.Caption := Lang[ID_NEWVAR_COMMENTSSTYLE];
  btnCreate.Caption := Lang[ID_NEWVAR_BTN_CREATE];
  btnCancel.Caption := Lang[ID_NEWVAR_BTN_CANCEL];
end;

end.

