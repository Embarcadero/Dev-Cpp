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
  TFindAction = (faFind, faFindFiles, faReplace, faReplaceFiles);
  TFindForm = class(TForm)
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
    procedure FindAllAction(Sender: TObject; const aSearch,aReplace: AnsiString; Line, Column: integer; var Action: TSynReplaceAction);
    function Execute(editor : TSynEdit;action : TFindAction) : integer;
  public
    property TabIndex : integer read fTabIndex write fTabIndex;
  end;

var
  FindForm: TFindForm = nil;

implementation

uses
{$IFDEF WIN32}
  Main, Dialogs, MultiLangSupport, devcfg, utils, SynEditMiscClasses, Math;
{$ENDIF}
{$IFDEF LINUX}
  Xlib, Main, QDialogs, MultiLangSupport, devcfg;
{$ENDIF}

{$R *.dfm}

function TFindForm.Execute(editor : TSynEdit;action : TFindAction) : integer;
var
	caretbackup,blockbeginbackup,blockendbackup : TBufferCoord;
	onreplacebackup : TReplaceTextEvent;
	enginebackup : TSynEditSearchCustom;
	toplinebackup : integer;
begin

	// Don't affect global actions
	if action in [faFind,faReplace] then begin

		// Only set the cursor to the end of the selection when not using the selection and when using from cursor
		if not (ssoEntireScope in fSearchOptions) and not (ssoSelectedOnly in fSearchOptions) then begin

			// Make sure we do not find the same word again and again when using From Cursor
			if (ssoBackwards in fSearchOptions) then begin
				if editor.SelAvail then
					editor.CaretX := editor.BlockBegin.Char;
			end else begin
				if editor.SelAvail then
					editor.CaretX := editor.BlockEnd.Char;
			end;
		end;
	end;

	// Back some data up
	caretbackup := editor.CaretXY;
	blockbeginbackup := editor.BlockBegin;
	blockendbackup := editor.BlockEnd;
	toplinebackup := editor.TopLine;
	onreplacebackup := editor.OnReplaceText;
	enginebackup := editor.SearchEngine;

	// When using find in files, report each find using OnReplaceText
	if action = faFindFiles then
		editor.OnReplaceText := FindAllAction;

	// Swap search engire for ours
	editor.SearchEngine := fSearchEngine;
	result := editor.SearchReplace(cboFindText.Text,cboReplaceText.Text,fSearchOptions);

	// Don't touch editors which we are only scanning
	if action in [faFindFiles] then begin

		// Put backup back into place
		editor.CaretXY := caretbackup;
		editor.BlockBegin := blockbeginbackup;
		editor.BlockEnd := blockendbackup;
		editor.TopLine := toplinebackup;
	end;

	editor.OnReplaceText := onreplacebackup;
	editor.SearchEngine := enginebackup;
end;

procedure TFindForm.btnFindClick(Sender: TObject);
var
	I,findcount : integer;
	e : TEditor;
	actiontype : TFindAction;
begin

	findcount := 0;

	case FindTabs.TabIndex of
		0: actiontype := faFind;
		1: actiontype := faFindFiles;
		2: actiontype := faReplace;
		3: actiontype := faReplaceFiles;
		else
			Exit;
	end;

	// Add input to history
	if (cboFindText.Text <> '') then begin
		if cboFindText.Items.IndexOf(cboFindText.Text) = -1 then
			cboFindText.AddItem(cboFindText.Text,nil);
	end else begin
		MessageBox(
			Self.Handle,
			PAnsiChar(Lang[ID_ERR_SEARCHCANNOTBEEMPTY]),
			PAnsiChar(Lang[ID_INFO]),
			MB_ICONINFORMATION or MB_TOPMOST);
		Exit;
	end;

	// Add nonempty strings to history
	if (cboReplaceText.Text <> '') and (actiontype in [faReplace,faReplaceFiles]) then
		if cboReplaceText.Items.IndexOf(cboReplaceText.Text) = -1 then
			cboReplaceText.AddItem(cboReplaceText.Text,nil);

	fSearchOptions := [];

	// Apply options
	if cbMatchCase.Checked then
		Include(fSearchOptions,ssoMatchCase);
	if cbWholeWord.Checked then
		Include(fSearchOptions,ssoWholeWord);
	if cbPrompt.Checked or (actiontype = faFindFiles) then // do a fake prompted replace when using find in files
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
	if actiontype in [faFindFiles,faReplaceFiles] then
		Include(fSearchOptions,ssoEntireScope);

	// do a fake prompted replace when using find in files
	if actiontype in [faFindFiles,faReplace,faReplaceFiles] then
		Include(fSearchOptions,ssoReplace);

	if actiontype in [faFindFiles,faReplaceFiles] then
		Include(fSearchOptions,ssoReplaceAll);

	MainForm.FindOutput.Items.BeginUpdate;

	// Find the first one, then quit
	if actiontype in [faFind,faReplace] then begin
		e := MainForm.GetEditor;

		Inc(findcount,Execute(e.Text,actiontype));

	// Or find everything
	end else begin

		// Enumerate results in message view when finding in files
		if actiontype = faFindFiles then
			MainForm.FindOutput.Clear;

		// loop through pagecontrol
		if rbOpenFiles.Checked then begin

			// loop through editors, add results to message control
			for I := 0 to MainForm.PageControl.PageCount - 1 do begin
				e := MainForm.GetEditor(i);
				fCurFile := e.FileName;

				// Bring an editor up front if we use prompting
				if (actiontype = faReplaceFiles) and (ssoPrompt in fSearchOptions) then
					e.Activate;

				Inc(findcount,Execute(e.Text,actiontype));
			end;

		// loop through project
		end else if rbProjectFiles.Checked then begin
			for I := 0 to MainForm.fProject.Units.Count - 1 do begin
				e := MainForm.fProject.Units[i].Editor;
				fCurFile := MainForm.fProject.Units[i].FileName;

				// file is already open, use memory
				if Assigned(e) then begin

					// Bring an editor up front if we use prompting
					if (actiontype = faReplaceFiles) and (ssoPrompt in fSearchOptions) then
						e.Activate;

					Inc(findcount,Execute(e.Text,actiontype));

				// not open? load from disk
				end else begin

					if (actiontype = faReplaceFiles) then begin

						// we have to open an editor...
						if ssoPrompt in fSearchOptions then begin
							e := MainForm.GetEditorFromFileName(fCurFile);
							if Assigned(e) then begin
								e.Activate;

								Inc(findcount,Execute(e.Text,actiontype));

								// Save and close
								e.Save;
								MainForm.fProject.CloseUnit(MainForm.fProject.Units.Indexof(e));
							end;
						end else begin

							// Stealth replace
							fTempSynEdit.Lines.LoadFromFile(fCurFile);
							Inc(findcount,Execute(fTempSynEdit,actiontype));
							fTempSynEdit.Lines.SaveToFile(fCurFile);
						end;
					end else begin

						// Only finding...
						fTempSynEdit.Lines.LoadFromFile(fCurFile);
						Inc(findcount,Execute(fTempSynEdit,actiontype));
					end;
				end;
			end;
		end else if rbCurFile.Checked then begin
			e := MainForm.GetEditor;

			if Assigned(e) then begin

				fCurFile := e.FileName;

				Inc(findcount,Execute(e.Text,actiontype));
			end;
		end;
	end;

	MainForm.FindOutput.Items.EndUpdate;

	if actiontype = faFindFiles then begin
		MainForm.MessageControl.ActivePageIndex := 4; // Find Tab
		MainForm.FindSheet.Caption := Lang[ID_SHEET_FIND] + ' (' + IntToStr(findcount) + ')';
		//MainForm.AddFindOutputItem('','','',Format(Lang[ID_SEARCHCOUNT],[findcount]),'');
		MainForm.OpenCloseMessageSheet(TRUE);
	end else begin
		if findcount = 0 then
			MessageBox(
				Self.Handle,
				PAnsiChar(Format(Lang[ID_MSG_TEXTNOTFOUND],[cboFindText.Text])),
				PAnsiChar(Lang[ID_INFO]),
				MB_ICONINFORMATION or MB_TOPMOST);
	end;
end;

procedure TFindForm.FindAllAction(Sender: TObject;const aSearch, aReplace: AnsiString; Line, Column: integer;var Action: TSynReplaceAction);
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

procedure TFindForm.FormClose(Sender: TObject; var Action: TCloseAction);
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

procedure TFindForm.btnCancelClick(Sender: TObject);
begin
	Close;
end;

procedure TFindForm.FindTabsChange(Sender: TObject);
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
	grpDirection.Visible := isfind or isreplace;
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

procedure TFindForm.LoadText;
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

procedure TFindForm.FormCreate(Sender: TObject);
begin
	LoadText;

	fSearchEngine := TSynEditSearch.Create(Self);

	// Create a temporary editor for closed file searching
	fTempSynEdit := TSynEdit.Create(Self);

	fTempSynEdit.WantTabs := devEditor.UseTabs;
	fTempSynEdit.TabWidth := devEditor.TabSize;
end;

procedure TFindForm.FormShow(Sender: TObject);
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

procedure TFindForm.FindCutClick(Sender: TObject);
begin
	if cboFindText.Focused then begin
		Clipboard.AsText := cboFindText.SelText;
		cboFindText.SelText := '';
	end else if cboReplaceText.Focused then begin
		Clipboard.AsText := cboReplaceText.SelText;
		cboReplaceText.SelText := '';
	end;
end;

procedure TFindForm.FindCopyClick(Sender: TObject);
begin
	if cboFindText.Focused then
		Clipboard.AsText := cboFindText.SelText
	else if cboReplaceText.Focused then
		Clipboard.AsText := cboReplaceText.SelText;
end;

procedure TFindForm.FindPasteClick(Sender: TObject);
begin
	if cboFindText.Focused then
		cboFindText.SelText := Clipboard.AsText
	else if cboReplaceText.Focused then
		cboReplaceText.SelText := Clipboard.AsText;
end;

procedure TFindForm.FindSelAllClick(Sender: TObject);
begin
	if cboFindText.Focused then
		cboFindText.SelectAll
	else if cboReplaceText.Focused then
		cboReplaceText.SelectAll;
end;

procedure TFindForm.FormDestroy(Sender: TObject);
begin
	fSearchEngine.Free;
	fTempSynEdit.Free;
end;

end.
