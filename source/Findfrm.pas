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
    rbOpenFiles: TRadioButton;
    cbPrompt: TCheckBox;
    cboReplaceText: TComboBox;
    lblReplace: TLabel;
    FindPopup: TPopupMenu;
    FindCopy: TMenuItem;
    FindPaste: TMenuItem;
    FindCut: TMenuItem;
    N1: TMenuItem;
    FindSelAll: TMenuItem;
    rbCurFile: TRadioButton;
    procedure btnFindClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnCancelClick(Sender: TObject);
    procedure FindTabsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FindCutClick(Sender: TObject);
    procedure FindCopyClick(Sender: TObject);
    procedure FindPasteClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FindSelAllClick(Sender: TObject);
  private
    fSearchOptions : TSynSearchOptions;
    fCurFile : AnsiString;
    fTabIndex : integer;
    fSearchEngine : TSynEditSearch;
    fTempSynEdit : TSynEdit;
    procedure LoadText;
    procedure CustomOnReplace(Sender: TObject; const aSearch,aReplace: AnsiString; Line, Column: integer; var Action: TSynReplaceAction);
    function FindReplaceFirst(editor : TSynEdit) : integer;
    function FindReplaceFiles(editor : TSynEdit;isreplace : boolean) : integer;
  public
    property TabIndex : integer read fTabIndex write fTabIndex;
  end;

var
 frmFind: TfrmFind = nil;

implementation

uses
{$IFDEF WIN32}
  Main, Dialogs, MultiLangSupport, devcfg, utils, SynEditMiscClasses, Math;
{$ENDIF}
{$IFDEF LINUX}
  Xlib, Main, QDialogs, MultiLangSupport, devcfg;
{$ENDIF}

{$R *.dfm}

function TfrmFind.FindReplaceFirst(editor : TSynEdit) : integer;
var
	enginebackup : TSynEditSearchCustom;
begin
	// Make sure we do not find the same word again and again when using 'from cursor'
	if (ssoBackwards in fSearchOptions) then begin
		if editor.SelAvail then
			editor.CaretX := editor.BlockBegin.Char;
	end else begin
		if editor.SelAvail then
			editor.CaretX := editor.BlockEnd.Char;
	end;

	// Swap search engire for ours
	enginebackup := editor.SearchEngine;
	editor.SearchEngine := fSearchEngine;

	result := editor.SearchReplace(cboFindText.Text,cboReplaceText.Text,fSearchOptions);

	// Apply search engine backup
	editor.SearchEngine := enginebackup;
end;

function TfrmFind.FindReplaceFiles(editor : TSynEdit;isreplace : boolean) : integer;
var
	caretbackup,blockbeginbackup,blockendbackup : TBufferCoord;
	onreplacebackup : TReplaceTextEvent;
	enginebackup : TSynEditSearchCustom;
	toplinebackup : integer;
begin

	// Back some data up
	caretbackup := editor.CaretXY;
	blockbeginbackup := editor.BlockBegin;
	blockendbackup := editor.BlockEnd;
	toplinebackup := editor.TopLine;
	onreplacebackup := editor.OnReplaceText;
	enginebackup := editor.SearchEngine;

	// When using find in files, report each find using OnReplaceText
	if not isreplace then
		editor.OnReplaceText := CustomOnReplace;

	// And do the final search
	editor.SearchEngine := fSearchEngine;
	result := editor.SearchReplace(cboFindText.Text,cboReplaceText.Text,fSearchOptions);
	editor.SearchEngine := enginebackup;

	editor.CaretXY := caretbackup;
	editor.BlockBegin := blockbeginbackup;
	editor.BlockEnd := blockendbackup;
	editor.TopLine := toplinebackup;
	editor.OnReplaceText := onreplacebackup;
end;

procedure TfrmFind.btnFindClick(Sender: TObject);
var
	isfind,isfindfiles,isreplace,isreplacefiles : boolean;
	I,findcount : integer;
	e : TEditor;
begin

	findcount := 0;

	// Assemble search options
	isfind := (FindTabs.TabIndex = 0);
	isfindfiles := (FindTabs.TabIndex = 1);
	isreplace := (FindTabs.TabIndex = 2);
	isreplacefiles := (FindTabs.TabIndex = 3);

	// Add input to history
	if (cboFindText.Text <> '') then begin
		if cboFindText.Items.IndexOf(cboFindText.Text) = -1 then
			cboFindText.AddItem(cboFindText.Text,nil);
	end else begin
		MessageBox(Application.Handle, PAnsiChar(Lang[ID_ERR_SEARCHCANNOTBEEMPTY]),PAnsiChar(Lang[ID_INFO]), MB_ICONINFORMATION);
		Exit;
	end;

	if (cboReplaceText.Text <> '') and (isreplace or isreplacefiles) then
		if cboReplaceText.Items.IndexOf(cboReplaceText.Text) = -1 then
			cboReplaceText.AddItem(cboReplaceText.Text,nil);

	fSearchOptions := [];

	// Apply options
	if cbMatchCase.Checked then
		Include(fSearchOptions,ssoMatchCase);
	if cbWholeWord.Checked then
		Include(fSearchOptions,ssoWholeWord);
	if cbPrompt.Checked or isfindfiles then // do a fake prompted replace when using find in files
		Include(fSearchOptions,ssoPrompt);

	// Apply scope, when visible
	if grpScope.Visible then begin
		if rbSelectedOnly.Checked then
			Include(fSearchOptions,ssoSelectedOnly);
	end;

	// Apply direction, when visible
	if grpDirection.Visible then begin
		if rbBackward.Checked then
			Include(fSearchOptions,ssoBackwards);
	end;

	// Apply origin, when visible
	if grpOrigin.Visible then begin
		if rbEntireScope.Checked then
			Include(fSearchOptions,ssoEntireScope);
	end;

	// Use entire scope for file finding/replacing
	if isfindfiles or isreplacefiles then
		Include(fSearchOptions,ssoEntireScope);

	if isfindfiles or isreplace or isreplacefiles then
		Include(fSearchOptions,ssoReplace); // do a fake prompted replace when using find in files

	if isfindfiles or isreplacefiles then
		Include(fSearchOptions,ssoReplaceAll);

	MainForm.FindOutput.Items.BeginUpdate;

	// Do the actual searching
	if isfind or isreplace then begin
		e := MainForm.GetEditor;

		Inc(findcount,FindReplaceFirst(e.Text));

		if findcount = 0 then
			MessageBox(Application.Handle,PAnsiChar(Format(Lang[ID_MSG_TEXTNOTFOUND],[cboFindText.Text])),PAnsiChar(Lang[ID_INFO]), MB_ICONINFORMATION);
	end;

	// Do the actual searching
	if isfindfiles or isreplacefiles then begin

		if isfindfiles then
			MainForm.FindOutput.Clear;

		// loop through pagecontrol
		if rbOpenFiles.Checked then begin

			// loop through editors, add results to message control
			for I := 0 to MainForm.PageControl.PageCount - 1 do begin
				e := MainForm.GetEditor(i);
				fCurFile := e.FileName;

				// Bring an editor up front if we use prompting
				if isreplacefiles and (ssoPrompt in fSearchOptions) then
					e.Activate;

				Inc(findcount,FindReplaceFiles(e.Text,isreplacefiles));
			end;

		// loop through project
		end else if rbProjectFiles.Checked then begin
			for I := 0 to MainForm.fProject.Units.Count - 1 do begin
				e := MainForm.fProject.Units[i].Editor;
				fCurFile := MainForm.fProject.Units[i].FileName;

				// file is already open, use memory
				if Assigned(e) then begin

					if isreplacefiles and (ssoPrompt in fSearchOptions) then
						e.Activate;

					Inc(findcount,FindReplaceFiles(e.Text,isreplacefiles));

				// not open? load from disk
				end else begin

					if isreplacefiles then begin

						// we have to open an editor...
						if ssoPrompt in fSearchOptions then begin
							e := MainForm.GetEditorFromFileName(fCurFile);
							if Assigned(e) then begin
								e.Activate;
								Inc(findcount,FindReplaceFiles(e.Text,isreplacefiles));

								// Save and close
								MainForm.SaveFile(e);
								MainForm.fProject.CloseUnit(MainForm.fProject.Units.Indexof(e));
							end;
						end else begin

							// Stealth replace
							fTempSynEdit.Lines.LoadFromFile(fCurFile);
							Inc(findcount,FindReplaceFiles(fTempSynEdit,isreplacefiles));
							fTempSynEdit.Lines.SaveToFile(fCurFile);
						end;
					end else begin

						// Only finding...
						fTempSynEdit.Lines.LoadFromFile(fCurFile);
						Inc(findcount,FindReplaceFiles(fTempSynEdit,isreplacefiles));
					end;
				end;
			end;
		end else if rbCurFile.Checked then begin
			e := MainForm.GetEditor;

			if Assigned(e) then begin

				fCurFile := e.FileName;

				Inc(findcount,FindReplaceFiles(e.Text,isreplacefiles));
			end;
		end;
	end;

	MainForm.FindOutput.Items.EndUpdate;

	if isfindfiles then begin
		MainForm.MessageControl.ActivePageIndex := 4; // Find Tab
		MainForm.AddFindOutputItem('','','',Format(Lang[ID_SEARCHCOUNT],[findcount]),'');
		MainForm.OpenCloseMessageSheet(TRUE);
	end else
		MainForm.OpenCloseMessageSheet(FALSE);
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
	devData.OriginEntireScope := rbEntireScope.Checked;

	if rbProjectFiles.Checked then
		devData.SearchWhere := 0
	else if rbOpenFiles.Checked then
		devData.SearchWhere := 1
	else if rbCurFile.Checked then
		devData.SearchWhere := 2;

	// Save some memory
	fTempSynEdit.ClearAll;

	Action := caHide;
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
	rbProjectFiles.Enabled := Assigned(MainForm.fProject);
	if not Assigned(MainForm.fProject) then
		rbOpenFiles.Checked := true;

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
  rbCurFile.Caption:=      Lang[ID_FIND_CURRENTFILE];

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
	FindSelAll.Caption := Lang[ID_ITEM_SELECTALL];
end;

procedure TfrmFind.FormCreate(Sender: TObject);
begin
	LoadText;

	fSearchEngine := TSynEditSearch.Create(Self);

	// Create a temporary editor for closed file searching
	fTempSynEdit := TSynEdit.Create(Self);

	fTempSynEdit.WantTabs := devEditor.UseTabs;
	fTempSynEdit.TabWidth := devEditor.TabSize;
end;

procedure TfrmFind.FormShow(Sender: TObject);
begin
	FindTabs.TabIndex := fTabIndex;

	// apply previous settings
	cbMatchCase.Checked := devData.CaseSensitive;
	cbWholeWord.Checked := devData.Wholewords;
	cbPrompt.Checked := devData.PromptReplace;

	rbSelectedOnly.Checked := devData.ScopeIsSelected;
	rbBackward.Checked := devData.DirBackward;
	rbEntireScope.Checked := devData.OriginEntireScope;

	case devData.SearchWhere of
		0 : rbProjectFiles.Checked := true;
		1 : rbOpenFIles.Checked := true;
		2 : rbCurFile.Checked := true;
	end;

	ActiveControl := cboFindText;

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

procedure TfrmFind.FindSelAllClick(Sender: TObject);
begin
	if cboFindText.Focused then
		cboFindText.SelectAll
	else if cboReplaceText.Focused then
		cboReplaceText.SelectAll;
end;

procedure TfrmFind.FormDestroy(Sender: TObject);
begin
	fSearchEngine.Free;
	fTempSynEdit.Free;
end;

end.
