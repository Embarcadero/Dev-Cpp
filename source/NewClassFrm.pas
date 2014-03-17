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

unit NewClassFrm;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Variants, Classes, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls, QButtons;
{$ENDIF}

type
  TNewClassForm = class(TForm)
    lblClassName: TLabel;
    txtName: TEdit;
    grpInheritance: TGroupBox;
    lblAccess: TLabel;
    lblInherit: TLabel;
    cmbClass: TComboBox;
    lblCppFile: TLabel;
    txtCppFile: TEdit;
    lblHFile: TLabel;
    txtHFile: TEdit;
    btnBrowseCpp: TSpeedButton;
    btnBrowseH: TSpeedButton;
    chkAddToProject: TCheckBox;
    btnCreate: TButton;
    btnCancel: TButton;
    cmbScope: TComboBox;
    chkInherit: TCheckBox;
    lblHeaderFile: TLabel;
    txtIncFile: TEdit;
    lblArguments: TLabel;
    txtArgs: TEdit;
    chkConstruct: TCheckBox;
    chkDestruct: TCheckBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure chkInheritClick(Sender: TObject);
    procedure txtNameChange(Sender: TObject);
    procedure txtCppFileChange(Sender: TObject);
    procedure btnBrowseCppClick(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
    procedure cmbClassChange(Sender: TObject);
    procedure txtNameKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    procedure LoadText;
  public
    { Public declarations }
  end;

implementation

uses 
  main, CppParser, MultiLangSupport, version, editor, devcfg;

{$R *.dfm}

procedure TNewClassForm.FormClose(Sender: TObject;var Action: TCloseAction);
begin
	Action := caFree;
end;

procedure TNewClassForm.FormShow(Sender: TObject);
var
	sl: TStrings;
begin
	LoadText;

	sl := TStringList.Create;
	try
		MainForm.CppParser.GetClassesList(sl);
		cmbClass.Items.Assign(sl);
	finally
		sl.Free;
	end;

	// public is used most of the time?
	cmbScope.ItemIndex := cmbScope.Items.IndexOf('public');

	// Check if the statement the user selected is a class...
	if Assigned(MainForm.ClassBrowser.Selected) and
	   Assigned(MainForm.ClassBrowser.Selected.Data) and
	   (PStatement(MainForm.ClassBrowser.Selected.Data)^._Kind = skClass) then begin

		// If we are spawned from the class browser, set inheritcance to selected class
		cmbClass.ItemIndex := cmbClass.Items.IndexOf(PStatement(MainForm.ClassBrowser.Selected.Data)^._ScopeCmd);
		if cmbClass.ItemIndex <> -1 then
			chkInherit.Checked := True;
	end else begin
		cmbClass.Text := '';
		chkInherit.Checked := False;
	end;

	txtName.SetFocus;
end;

procedure TNewClassForm.chkInheritClick(Sender: TObject);
begin
	cmbScope.Enabled := chkInherit.Checked;
	cmbClass.Enabled := chkInherit.Checked;
	txtIncFile.Enabled := chkInherit.Checked;
end;

procedure TNewClassForm.txtNameChange(Sender: TObject);
begin
	if txtName.Text <> '' then begin
		txtCppFile.Text := MainForm.Project.Directory + LowerCase(txtName.Text) + '.cpp';
		txtHFile.Text := MainForm.Project.Directory + LowerCase(txtName.Text) + '.h';

		// Make sure one can actually see what is going on
		txtCppFile.SelStart := Length(txtCppFile.Text)-1;
		txtHFile.SelStart := Length(txtHFile.Text)-1;
	end else begin
		txtCppFile.Text := '';
		txtHFile.Text := '';
	end;
	btnCreate.Enabled := (txtName.Text <> '') and (txtCppFile.Text <> '') and (txtHFile.Text <> '');
end;

procedure TNewClassForm.txtCppFileChange(Sender: TObject);
begin
  btnCreate.Enabled := (txtName.Text <> '') and
    (txtCppFile.Text <> '') and
    (txtHFile.Text <> '');
end;

procedure TNewClassForm.btnBrowseCppClick(Sender: TObject);
begin
	with TOpenDialog.Create(self) do try
		if Sender = btnBrowseCpp then begin
			FileName := ExtractFileName(txtCppFile.Text);
			InitialDir := ExtractFilePath(txtCppFile.Text);
			Filter := FLT_CPPS;
			DefaultExt := 'cpp';
		end else begin
			FileName := ExtractFileName(txtHFile.Text);
			InitialDir := ExtractFilePath(txtHFile.Text);
			Filter := FLT_HEADS;
			DefaultExt := 'h';
		end;
		if Execute then begin
			if Sender = btnBrowseCpp then
				txtCppFile.Text := FileName
			else
				txtHFile.Text := FileName;
		end;
	finally
		Free;
	end;
end;

procedure TNewClassForm.btnCreateClick(Sender: TObject);
var
  idx: integer;
  e,headere: TEditor;
  hfName: AnsiString;
  hFile: integer;
  st: PStatement;
begin
	// HEADER FILE IMPLEMENTATION
	if chkAddToProject.Checked then begin
		idx := MainForm.Project.NewUnit(False, txtHFile.Text);
		e := MainForm.Project.OpenUnit(idx);
		if idx = -1 then begin
			MessageDlg('Cannot add header file to project...', mtError, [mbOk], 0);
			Exit;
		end;
	end else begin
		hFile := FileCreate(txtHFile.Text);
		if hFile > 0 then begin
			FileClose(hFile);
			e := MainForm.GetEditorFromFileName(txtHFile.Text);
		end else begin
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

	e.Text.Lines.BeginUpdate;

	// Add header guard
	e.Text.Lines.Add('#ifndef ' + hfName);
	e.Text.Lines.Add('#define ' + hfName);
	e.Text.Lines.Add('');

	// Add inherited piece ": public foo"
	if chkInherit.Checked and (txtIncFile.Text <> '') then begin

		// Find class we inherit from
		st := nil;
		for idx := 0 to MainForm.CppParser.Statements.Count - 1 do begin
			st := MainForm.CppParser.Statements[idx];
			if (st^._Kind = skClass) and (st^._ScopelessCmd = cmbClass.Text) and (MainForm.Project.Units.Indexof(MainForm.CppParser.GetDeclarationFileName(st)) <> -1) then
				Break;
			st:=nil;
		end;

		if Assigned(st) then
			e.Text.Lines.Add('#include "' + txtIncFile.Text + '"')
		else
			e.Text.Lines.Add('#include <' + txtIncFile.Text + '>');
		e.Text.Lines.Add('');
	end;

	// Include inheritance or not, assume it's valid code
	if chkInherit.Checked and (cmbClass.Text <> '') then
		e.Text.Lines.Add('class ' + txtName.Text + ' : ' + cmbScope.Text + ' ' + cmbClass.Text)
	else
		e.Text.Lines.Add('class ' + txtName.Text);

	e.Text.Lines.Add('{');
	e.Text.Lines.Add(#9'public:');

	// Create one optional constructor with args
	if chkConstruct.Checked then
		e.Text.Lines.Add(#9#9 + txtName.Text + '(' + txtArgs.Text + ');');

	// Create one optional destructor
	if chkDestruct.Checked then
		e.Text.Lines.Add(#9#9'~' + txtName.Text + '();');

	e.Text.Lines.Add(#9'protected:');
	e.Text.Lines.Add('};');
	e.Text.Lines.Add('');
	e.Text.Lines.Add('#endif');
	e.Text.Lines.EndUpdate;

	e.Text.Modified := True;
	headere := e; // keep pointer so we can activate

	// CPP FILE IMPLEMENTATION
	if chkAddToProject.Checked then begin
		idx := MainForm.Project.NewUnit(False, txtCppFile.Text);
		e := MainForm.Project.OpenUnit(idx);
		if idx = -1 then begin
			MessageDlg('Cannot add implementation file to project...', mtError, [mbOk], 0);
			Exit;
		end;
	end else begin
		hFile := FileCreate(txtCppFile.Text);
		if hFile > 0 then begin
			FileClose(hFile);
			e := MainForm.GetEditorFromFileName(txtCppFile.Text);
		end else begin
			MessageDlg('Cannot create implementation file...', mtError, [mbOk], 0);
			Exit;
		end;
	end;

	if not Assigned(e) then begin
		MessageDlg('Cannot open implementation file in editor...', mtError, [mbOk], 0);
		Exit;
	end;

	e.Text.Lines.BeginUpdate;
	e.Text.Lines.Add('#include "' + ExtractFileName(txtHFile.Text) + '"');

	if chkConstruct.Checked then begin
		e.Text.Lines.Add('');
		e.Text.Lines.Add(txtName.Text + '::' + txtName.Text + '(' + txtArgs.Text + ')');
		e.Text.Lines.Add('{');
		e.Text.Lines.Add('}');
	end;

	if chkDestruct.Checked then begin
		e.Text.Lines.Add('');
		e.Text.Lines.Add(txtName.Text + '::~' + txtName.Text + '()');
		e.Text.Lines.Add('{');
		e.Text.Lines.Add('}');
	end;

	e.Text.Lines.EndUpdate;
	e.Text.Modified := True;

	// Open CPP file by default
	headere.Activate;
end;

procedure TNewClassForm.cmbClassChange(Sender: TObject);
var
	st: PStatement;
begin
	if cmbClass.Items.IndexOf(cmbClass.Text) <> -1 then begin
		st := PStatement(cmbClass.Items.Objects[cmbClass.Items.IndexOf(cmbClass.Text)]);
		if Assigned(st) then
			txtIncFile.Text := ExtractFileName(MainForm.CppParser.GetDeclarationFileName(st))
		else
			txtIncFile.Text := LowerCase(cmbClass.Text) + '.h';
	end else begin
		if cmbClass.Text <> '' then
			txtIncFile.Text := LowerCase(cmbClass.Text) + '.h'
		else
			txtIncFile.Text := '';
	end;
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
	// Set interface font
	Font.Name := devData.InterfaceFont;
	Font.Size := devData.InterfaceFontSize;

	Caption := Lang[ID_POP_NEWCLASS];
	lblClassName.Caption := Lang[ID_NEWCLASS_NAME];
	lblArguments.Caption := Lang[ID_NEWMEMB_ARGS];
	chkConstruct.Caption := Lang[ID_NEWCLASS_CONSTRUCTOR];
	chkDestruct.Caption := Lang[ID_NEWCLASS_DESTRUCTOR];

	chkInherit.Caption := Lang[ID_NEWCLASS_INHERIT];
	grpInheritance.Caption := Lang[ID_NEWCLASS_INHERITANCE];
	lblAccess.Caption := Lang[ID_NEWVAR_SCOPE] + ':';
	lblInherit.Caption := Lang[ID_NEWCLASS_INHERIT_FROM];
	lblHeaderFile.Caption := Lang[ID_NEWCLASS_INHERIT_HDR];

	lblCppFile.Caption := Lang[ID_NEWCLASS_CPPFILE];
	lblHFile.Caption := Lang[ID_NEWCLASS_HFILE];
	chkAddToProject.Caption := Lang[ID_NEWCLASS_ADDTOPROJ];
	btnCreate.Caption := Lang[ID_NEWVAR_BTN_CREATE];
	btnCancel.Caption := Lang[ID_NEWVAR_BTN_CANCEL];
end;

end.

