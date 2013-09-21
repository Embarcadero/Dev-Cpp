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
  Windows, Messages, editor, SysUtils, Classes, Graphics, Controls, Forms,
  SynEdit, StdCtrls, SynEditTypes, SynEditSearch, Clipbrd, ComCtrls, Menus;
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
    cbPrompt: TCheckBox;
    cboReplaceText: TComboBox;
    lblReplace: TLabel;
    FindPopup: TPopupMenu;
    FindCopy: TMenuItem;
    FindPaste: TMenuItem;
    FindCut: TMenuItem;
    procedure btnFindClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnCancelClick(Sender: TObject);
    procedure FindTabsChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FindCutClick(Sender: TObject);
    procedure FindCopyClick(Sender: TObject);
    procedure FindPasteClick(Sender: TObject);
  private
    fSearchOptions : TSynSearchOptions;
    fCurFile : AnsiString;
    fTabIndex : integer;
    fEditor : TEditor;
    fSearchEngine : TSynEditSearch;
    fTempSynEdit : TSynEdit;
    procedure LoadText;
    procedure CustomOnReplace(Sender: TObject; const aSearch,aReplace: AnsiString; Line, Column: integer; var Action: TSynReplaceAction);
    procedure FindReplaceFirst(editor : TSynEdit);
    procedure FindReplaceFiles(editor : TSynEdit;isreplace : boolean);
  public
    property TabIndex : integer read fTabIndex write fTabIndex;
  end;

var
 frmFind: TfrmFind = nil;

implementation

uses
{$IFDEF WIN32}
  Main, Dialogs, MultiLangSupport, devcfg, utils, SynEditMiscClasses;
{$ENDIF}
{$IFDEF LINUX}
  Xlib, Main, QDialogs, MultiLangSupport, devcfg;
{$ENDIF}

{$R *.dfm}

procedure TfrmFind.FindReplaceFirst(editor : TSynEdit);
var
	enginebackup : TSynEditSearchCustom;
begin
	// Make sure we do not find the same word again and again when using 'from cursor'
	if editor.SelAvail then
		editor.CaretX := editor.BlockEnd.Char;

	enginebackup := editor.SearchEngine;
	editor.SearchEngine := fSearchEngine;
	if(editor.SearchReplace(cboFindText.Text,'',fSearchOptions) = 0) then
		MessageBox(Application.Handle,PChar(format(Lang[ID_MSG_TEXTNOTFOUND], [cboFindText.Text])),PChar('Info'),MB_ICONINFORMATION);
	editor.SearchEngine := enginebackup;
end;

procedure TfrmFind.FindReplaceFiles(editor : TSynEdit;isreplace : boolean);
var
	caretbackup : TBufferCoord;
	onreplacebackup : TReplaceTextEvent;
	enginebackup : TSynEditSearchCustom;
begin
	caretbackup := editor.CaretXY;
	onreplacebackup := editor.OnReplaceText;
	enginebackup := editor.SearchEngine;

	// Don't skip when replacing!
	if not isreplace then
		editor.OnReplaceText := CustomOnReplace;

	editor.SearchEngine := fSearchEngine;
	editor.SearchReplace(cboFindText.Text,cboReplaceText.Text,fSearchOptions);
	editor.SearchEngine := enginebackup;

	editor.CaretXY := caretbackup;
	editor.OnReplaceText := onreplacebackup;
end;

procedure TfrmFind.btnFindClick(Sender: TObject);
var
	isfind,isfindfiles,isreplace,isreplacefiles : boolean;
	I : integer;
begin
	if cboFindText.Text = '' then Exit;

	// Assemble search options
	isfind := (FindTabs.TabIndex = 0);
	isfindfiles := (FindTabs.TabIndex = 1);
	isreplace := (FindTabs.TabIndex = 2);
	isreplacefiles := (FindTabs.TabIndex = 3);

	if cboFindText.Items.IndexOf(cboFindText.Text) = -1 then
		cboFindText.AddItem(cboFindText.Text,nil);

	if (isreplace or isreplacefiles) and (cboReplaceText.Text <> '') then
		if cboReplaceText.Items.IndexOf(cboReplaceText.Text) = -1 then
			cboReplaceText.AddItem(cboReplaceText.Text,nil);

	fSearchOptions := [];

	if cbMatchCase.Checked then
		Include(fSearchOptions,ssoMatchCase);
	if cbWholeWord.Checked then
		Include(fSearchOptions,ssoWholeWord);
	if cbPrompt.Checked or isfindfiles then // do a fake prompted replace when using find in files
		Include(fSearchOptions,ssoPrompt);

	if grpDirection.Visible then begin
		if rbBackward.Checked then
			Include(fSearchOptions,ssoBackwards);
	end;

	if rbEntireScope.Checked or isfindfiles or isreplacefiles then
		Include(fSearchOptions,ssoEntireScope);

	if grpScope.Visible then begin
		if rbSelectedOnly.Checked then
			Include(fSearchOptions,ssoSelectedOnly);
	end;

	if not isfind then begin

		// do a fake prompted replace when using find in files
		Include(fSearchOptions,ssoReplace);

		// Don't stop asking for replaces under any circumstances
		Include(fSearchOptions,ssoReplaceAll);

	end;

	if isfind or isreplace then begin
		fEditor := MainForm.GetEditor;

		FindReplaceFirst(fEditor.Text);
	end;

	// Do the actual searching
	if isfindfiles or isreplacefiles then begin

		if isfindfiles then
			MainForm.FindOutput.Clear;

		// loop through pagecontrol
		if rbOpenFiles.Checked then begin

			// loop through editors, add results to message control
			for I := 0 to MainForm.PageControl.PageCount - 1 do begin
				fEditor := MainForm.GetEditor(i);
				fCurFile := fEditor.FileName;

				if isreplacefiles then
					fEditor.Activate;

				FindReplaceFiles(fEditor.Text,isreplacefiles);
			end;

		// loop through project
		end else begin
			for I := 0 to MainForm.fProject.Units.Count - 1 do begin
				fEditor := MainForm.fProject.Units[i].Editor;
				fCurFile := MainForm.fProject.Units[i].FileName;

				// file is already open, use memory
				if Assigned(fEditor) then begin

					if isreplacefiles then
						fEditor.Activate;

					FindReplaceFiles(fEditor.Text,isreplacefiles);

				// not open? load from disk
				end else begin
					fTempSynEdit.Lines.LoadFromFile(fCurFile);

					FindReplaceFiles(fTempSynEdit,isreplacefiles);
				end;
			end;
		end;

		if isfindfiles then begin
			MainForm.MessageControl.ActivePageIndex := 4; // Find Tab
			MainForm.OpenCloseMessageSheet(TRUE);
		end;
	end;
end;

procedure TfrmFind.CustomOnReplace(Sender: TObject;const aSearch, aReplace: AnsiString; Line, Column: integer;var Action: TSynReplaceAction);
var
	p : TBufferCoord;
	q : TDisplayCoord;
begin
	p.Char := Column;
	p.Line := Line;
	q := TCustomSynEdit(Sender).BufferToDisplayPos(p);

	// Convert to display coords
	MainForm.AddFindOutputItem(IntToStr(Line),IntToStr(Column),fCurFile,TCustomSynEdit(Sender).Lines[Line-1],aSearch);
	action := raSkip;
end;

procedure TfrmFind.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	// update devData
	devData.CaseSensitive := cbMatchCase.Checked;
	devData.Wholewords := cbWholeWord.Checked;
	devData.PromptReplace := cbPrompt.Checked;

	devData.ScopeIsSelected := rbSelectedOnly.Checked;
	devData.DirBackward := rbBackward.Checked;
	devData.WhereOpenFiles := rbOpenFiles.Checked;
	devData.OriginEntireScope := rbEntireScope.Checked;

	fSearchEngine.Free;
	fTempSynEdit.Free;
	Action := caFree;
	frmFind := nil;
end;

procedure TfrmFind.btnCancelClick(Sender: TObject);
begin
	Close;
end;

procedure TfrmFind.FindTabsChange(Sender: TObject);
var
	isfind,isfindfiles,isreplace,isreplacefiles : boolean;
begin
	isfind := (FindTabs.TabIndex = 0);
	isfindfiles := (FindTabs.TabIndex = 1);
	isreplace := (FindTabs.TabIndex = 2);
	isreplacefiles := (FindTabs.TabIndex = 3);

	lblReplace.Visible := isreplace or isreplacefiles;
	cboReplaceText.Visible := isreplace or isreplacefiles;

	grpOrigin.Visible := isfind or isreplace;
	grpScope.Visible := isfind or isreplace;
	grpWhere.Visible := isfindfiles or isreplacefiles;
	grpDirection.Visible := not isfindfiles;
	// grpOption is always visible

	// Disable project search option when none is open
	rbProjectFiles.Enabled := Assigned(MainForm.fProject) and not isreplacefiles;
	if not Assigned(MainForm.fProject) or isreplacefiles then
		rbOpenFiles.Checked := true; // only apply when branch is taken!

	// Disable prompt when doing finds
	cbPrompt.Enabled := isreplace or isreplacefiles;

	if not isreplace and not isreplacefiles then begin
		Caption := Lang[ID_FIND_FINDTAB];
		btnFind.Caption := Lang[ID_BTN_FIND]
	end else begin
		Caption := Lang[ID_FIND_REPLACE];
		btnFind.Caption := Lang[ID_BTN_REPLACE];
	end;
end;

procedure TfrmFind.LoadText;
begin
	// Set interface font
	Font.Name := devData.InterfaceFont;
	Font.Size := devData.InterfaceFontSize;

  Caption:=                 Lang[ID_FIND];

  //tabs
  FindTabs.Tabs.Clear;
  FindTabs.Tabs.Add(Lang[ID_FIND_FINDTAB]);
  FindTabs.Tabs.Add(Lang[ID_FIND_FINDALLTAB]);
  FindTabs.Tabs.Add(Lang[ID_FIND_REPLACE]);
  FindTabs.Tabs.Add(Lang[ID_FIND_REPLACEFILES]);

  //controls
  lblFind.Caption:=        Lang[ID_FIND_TEXT];
  lblReplace.Caption:=     Lang[ID_FIND_REPLACEWITH];
  grpOptions.Caption:=     '  '+Lang[ID_FIND_GRP_OPTIONS] +'  ';
  cbMatchCase.Caption:=    Lang[ID_FIND_CASE];
  cbWholeWord.Caption:=    Lang[ID_FIND_WWORD];
  cbPrompt.Caption:=       Lang[ID_FIND_PROMPTREPLACE];

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

	FindCut.Caption := Lang[ID_ITEM_CUT];
	FindCopy.Caption := Lang[ID_ITEM_COPY];
	FindPaste.Caption := Lang[ID_ITEM_PASTE];
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
		FindTabs.TabIndex := (FindTabs.TabIndex+1) mod 4;
end;

procedure TfrmFind.FormCreate(Sender: TObject);
begin
	LoadText;
	ActiveControl := cboFindText;

	fSearchEngine := TSynEditSearch.Create(Self);

	// Create a temporary editor for closed file searching
	fTempSynEdit := TSynEdit.Create(Self);

	fTempSynEdit.WantTabs := devEditor.UseTabs;
	fTempSynEdit.TabWidth := devEditor.TabSize;
end;

procedure TfrmFind.FormShow(Sender: TObject);
begin
	FindTabs.TabIndex := fTabIndex;

	// read devData
	cbMatchCase.Checked := devData.CaseSensitive;
	cbWholeWord.Checked := devData.Wholewords;
	cbPrompt.Checked := devData.PromptReplace;

	rbSelectedOnly.Checked := devData.ScopeIsSelected;
	rbBackward.Checked := devData.DirBackward;
	rbOpenFiles.Checked := devData.WhereOpenFiles;
	rbEntireScope.Checked := devData.OriginEntireScope;

	FindTabsChange(nil);
end;

procedure TfrmFind.FindCutClick(Sender: TObject);
begin
	if cboFindText.Focused then begin
		Clipboard.AsText := cboFindText.SelText;
		cboFindText.SelText := '';
	end else if cboReplaceText.Focused then begin
		Clipboard.AsText := cboReplaceText.SelText;
		cboReplaceText.SelText := '';
	end;
end;

procedure TfrmFind.FindCopyClick(Sender: TObject);
begin
	if cboFindText.Focused then
		Clipboard.AsText := cboFindText.SelText
	else if cboReplaceText.Focused then
		Clipboard.AsText := cboReplaceText.SelText;
end;

procedure TfrmFind.FindPasteClick(Sender: TObject);
begin
	if cboFindText.Focused then
		cboFindText.SelText := Clipboard.AsText
	else if cboReplaceText.Focused then
		cboReplaceText.SelText := Clipboard.AsText;
end;

end.
