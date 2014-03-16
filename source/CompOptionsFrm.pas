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

{$WARN UNIT_PLATFORM OFF}
unit CompOptionsFrm;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, StdCtrls, Inifiles, ExtCtrls, ComCtrls, Spin, Math,
  CompOptionsFrame, CompOptionsList;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Classes, QGraphics, QControls, QForms, QDialogs,
  QButtons, QStdCtrls, Inifiles, QExtCtrls, QComCtrls,
  CompOptionsFrame;
{$ENDIF}

type
  TCompOptForm = class(TForm)
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    MainPages: TPageControl;
    tabDirectories: TTabSheet;
    tabCompiler: TTabSheet;
    tabCodeGen: TTabSheet;
    DirTabs: TTabControl;
    btnUp: TSpeedButton;
    btnDown: TSpeedButton;
    lstDirs: TListBox;
    edEntry: TEdit;
    btnDelInval: TButton;
    btnDelete: TButton;
    btnAdd: TButton;
    btnReplace: TButton;
    tabPrograms: TTabSheet;
    lblProgramsText: TLabel;
    lblgcc: TLabel;
    GccEdit: TEdit;
    GppEdit: TEdit;
    lblgpp: TLabel;
    lblmake: TLabel;
    MakeEdit: TEdit;
    lblgdb: TLabel;
    GdbEdit: TEdit;
    lblwindres: TLabel;
    WindresEdit: TEdit;
    lbldllwrap: TLabel;
    DllwrapEdit: TEdit;
    lblgprof: TLabel;
    GprofEdit: TEdit;
    cbCompAdd: TCheckBox;
    Commands: TMemo;
    cbLinkerAdd: TCheckBox;
    Linker: TMemo;
    OptionsTip: TLabel;
    btnBrowse: TSpeedButton;
    btnBrowse2: TSpeedButton;
    btnBrowse3: TSpeedButton;
    btnBrowse4: TSpeedButton;
    btnBrowse5: TSpeedButton;
    btnBrowse6: TSpeedButton;
    btnBrowse7: TSpeedButton;
    btnBrowse8: TSpeedButton;
    CompOptionsFrame1: TCompOptionsFrame;
    grpCompSet: TGroupBox;
    btnAddBlankCompilerSet: TSpeedButton;
    btnDelCompilerSet: TSpeedButton;
    btnRenameCompilerSet: TSpeedButton;
    btnFindCompilers: TSpeedButton;
    cmbCompilerSetComp: TComboBox;
    tabMakefile: TTabSheet;
    grpMakefileGen: TGroupBox;
    lblDelay: TLabel;
    lblDelayMsg: TLabel;
    cbFastDep: TCheckBox;
    seCompDelay: TSpinEdit;
    btnAddFilledCompilerSet: TSpeedButton;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure DirTabsChange(Sender: TObject);
    procedure lstDirsClick(Sender: TObject);
    procedure lstDirsDblClick(Sender: TObject);
    procedure edEntryChange(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
    procedure UpDownClick(Sender: TObject);
    procedure edEntryKeyUp(Sender: TObject; var Key: Word;Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure btnBrws1Click(Sender: TObject);
    procedure cmbCompilerSetCompChange(Sender: TObject);
    procedure btnAddBlankCompilerSetClick(Sender: TObject);
    procedure btnDelCompilerSetClick(Sender: TObject);
    procedure btnRenameCompilerSetClick(Sender: TObject);
    procedure OptionsLinkClick(Sender: TObject);
    procedure cbCompAddClick(Sender: TObject);
    procedure cbLinkerAddClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnFindCompilersClick(Sender: TObject);
    procedure cmbCompilerSetCompEnter(Sender: TObject);
    procedure InterfaceChange(Sender: TObject);
    procedure InterfaceSave(Sender: TObject);
    procedure btnAddFilledCompilerSetClick(Sender: TObject);
  private
    fOldIndex: integer;
    fBinDirCopy: TStringList;
    fLibDirCopy: TStringList;
    fCDirCopy: TStringList;
    fCppDirCopy: TStringList;
    procedure LoadSet(Index : integer);
    procedure SaveSet(Index : integer);
    procedure UpdateButtons;
    procedure LoadText;
  end;

implementation

uses 
{$IFDEF WIN32}
  ShellAPI, Main, FileCtrl, version, devcfg, utils, MultiLangSupport, datamod;
{$ENDIF}
{$IFDEF LINUX}
  Xlib, Main, version, devcfg, utils, MultiLangSupport, datamod;
{$ENDIF}

{$R *.dfm}

procedure TCompOptForm.btnCancelClick(Sender: TObject);
begin
	Close;
end;

procedure TCompOptForm.btnOkClick(Sender: TObject);
var
	I : integer;
begin
	// Update names and current selection
	for I := 0 to cmbCompilerSetComp.Items.Count -1 do
		devCompilerSets[i].Name := cmbCompilerSetComp.Items[i];
	devCompilerSets.CurrentIndex := cmbCompilerSetComp.ItemIndex;

	// Save the current set to disk
	SaveSet(cmbCompilerSetComp.ItemIndex);

	// Save list of sets only
	devCompilerSets.SaveSetList;
end;

procedure TCompOptForm.LoadSet(Index : integer);
begin
	// Load the new set from disk or clear
	devCompilerSets.CurrentIndex := index;
	if index = -1 then begin // erase all
		fBinDirCopy.Clear;
		fCDirCopy.Clear;
		fCppDirCopy.Clear;
		fLibDirCopy.Clear;
		Commands.Lines.Text := '';
		Linker.Lines.Text := '';
		cbCompAdd.Checked := false;
		cbLinkerAdd.Checked := false;
		seCompDelay.Value := 0;
		cbFastDep.Checked := false;
		GccEdit.Text := '';
		GppEdit.Text := '';
		GdbEdit.Text := '';
		MakeEdit.Text := '';
		WindresEdit.Text := '';
		DllwrapEdit.Text := '';
		GprofEdit.Text := '';

		// TODO: disable controls?
		// Tried that. Enabled = False looks weird on page controls though
	end else begin

		// Apply the new set to the UI
		with devCompilerSets[index] do begin
			fBinDirCopy.Assign(BinDir); // use copies
			fCDirCopy.Assign(CDir);
			fCppDirCopy.Assign(CppDir);
			fLibDirCopy.Assign(LibDir);
			Commands.Lines.Text := CompOpts;
			Linker.Lines.Text := LinkOpts;
			cbCompAdd.Checked := AddtoComp;
			cbLinkerAdd.Checked := AddtoLink;
			seCompDelay.Value := Delay;
			cbFastDep.Checked := FastDep;
			GccEdit.Text := gccName;
			GppEdit.Text := gppName;
			GdbEdit.Text := gdbName;
			MakeEdit.Text := makeName;
			WindresEdit.Text := windresName;
			DllwrapEdit.Text := dllwrapName;
			GprofEdit.Text := gprofName;
		end;
	end;

	// fill tab controls
	CompOptionsFrame1.FillOptions;
	DirTabsChange(Self);

	// Mark unmodified
	cmbCompilerSetComp.Tag := 0;
end;

procedure TCompOptForm.SaveSet(Index: integer);
begin
	if (index >= devCompilerSets.Count) or (index < 0) then
		Exit;

	// Save the set to disk (can't be undone by Cancel...)
	with devCompilerSets[index] do begin

		// General
		CompOpts := Commands.Lines.Text;
		LinkOpts := Linker.Lines.Text;
		AddtoLink := cbLinkerAdd.Checked;
		AddtoComp := cbCompAdd.Checked;

		// Settings (flags) are saved by the components

		// Directories
		BinDir.Assign(fBinDirCopy);
		LibDir.Assign(fLibDirCopy);
		CDir.Assign(fCDirCopy);
		CppDir.Assign(fCppDirCopy);

		// Programs
		gccName := GccEdit.Text;
		gppName := gppEdit.Text;
		makeName := MakeEdit.Text;
		gdbName := GdbEdit.Text;
		windresName := WindresEdit.Text;
		dllwrapName := DllwrapEdit.Text;
		gprofName := GprofEdit.Text;

		// Makefile
		Delay := seCompDelay.Value;
		FastDep := cbFastDep.Checked;
	end;

	// Save...
	devCompilerSets.SaveSet(index);

	// Mark unmodified
	cmbCompilerSetComp.Tag := 0;
end;

procedure TCompOptForm.btnHelpClick(Sender: TObject);
begin
	OpenHelpFile('Interface\Dialog Windows\Compiler Options\index.htm'); // TODO: open iframe instead of page
end;

procedure TCompOptForm.DirTabsChange(Sender: TObject);
begin
	case DirTabs.TabIndex of
		0: lstDirs.Items.Assign(fBinDirCopy);
		1: lstDirs.Items.Assign(fLibDirCopy);
		2: lstDirs.Items.Assign(fCDirCopy);
		3: lstDirs.Items.Assign(fCppDirCopy);
	end;
	edEntry.Clear;
	UpdateButtons;
end;

procedure TCompOptForm.lstDirsClick(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TCompOptForm.lstDirsDblClick(Sender: TObject);
begin
	if lstDirs.ItemIndex <> -1 then
		edEntry.Text:= lstDirs.Items[lstDirs.ItemIndex];
end;

procedure TCompOptForm.edEntryChange(Sender: TObject);
begin
	UpdateButtons;
end;

procedure TCompOptForm.btnBrowseClick(Sender: TObject);
var
{$IFDEF WIN32}
  NewItem: AnsiString;
{$ENDIF}
{$IFDEF LINUX}
  NewItem: WideString;
{$ENDIF}
begin
  if (Trim(edEntry.Text)<>'') and DirectoryExists(Trim(edEntry.Text)) then
    newitem:=edEntry.Text
  else
    newitem:=devDirs.Default;
  if SelectDirectory('', '', NewItem) then
   edEntry.Text:= NewItem;
end;

procedure TCompOptForm.ButtonClick(Sender: TObject);
var
 idx: integer;
begin
  case TComponent(Sender).Tag of
   1: lstDirs.Items[lstDirs.ItemIndex]:= TrimRight(edEntry.Text);
   2: lstDirs.Items.Add(TrimRight(edEntry.Text));
   3: lstDirs.DeleteSelected;
   4:
    for idx:= pred(lstDirs.Items.Count) downto 0 do
     if not DirectoryExists(lstDirs.Items[idx]) then
      lstDirs.Items.Delete(idx);
  end; { case }
  edEntry.Clear;

	case DirTabs.TabIndex of
		0: fBinDirCopy.Assign(lstDirs.Items);
		1: fLibDirCopy.Assign(lstDirs.Items);
		2: fCDirCopy.Assign(lstDIrs.Items);
		3: fCppDirCopy.Assign(lstDirs.Items);
	end;
	edEntry.SetFocus;
end;

procedure TCompOptForm.UpDownClick(Sender: TObject);
var
 idx: integer;
begin
  idx:= lstDirs.ItemIndex;
  if Sender = btnUp then
   begin
     lstDirs.Items.Exchange(lstDirs.ItemIndex, lstDirs.ItemIndex -1);
     lstDirs.ItemIndex:= idx -1;
   end
  else
   begin
     if Sender = btnDown then
      begin
        lstDirs.Items.Exchange(lstDirs.ItemIndex, lstDirs.ItemIndex +1);
        lstDirs.ItemIndex:= idx +1;
      end;
   end;

	case DirTabs.TabIndex of
		0: fBinDirCopy.Assign(lstDirs.Items);
		1: fLibDirCopy.Assign(lstDirs.Items);
		2: fCDirCopy.Assign(lstDIrs.Items);
		3: fCppDirCopy.Assign(lstDirs.Items);
	end;
	UpdateButtons;
end;

procedure TCompOptForm.UpdateButtons;
begin
  btnAdd.Enabled:= edEntry.Text <> '';
  if lstDirs.ItemIndex>= 0 then
   begin
     btnDelete.Enabled:= TRUE;
     btnReplace.Enabled:= btnAdd.Enabled;
     btnUp.Enabled:= lstDirs.ItemIndex> 0;
     btnDown.Enabled:= lstDirs.ItemIndex < (lstDirs.Items.Count -1);
   end
  else
   begin
     btnDelete.Enabled:= FALSE;
     btnReplace.Enabled:= FALSE;
     btnUp.Enabled:= FALSE;
     btnDown.Enabled:= FALSE;
   end;
  btnDelInval.Enabled:= lstDirs.Items.Count> 0;
end;

procedure TCompOptForm.edEntryKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
{$IFDEF WIN32}
  if key = vk_return then ButtonClick(btnAdd);
{$ENDIF}
{$IFDEF LINUX}
  if key = XK_RETURN then ButtonClick(btnAdd);
{$ENDIF}
end;

procedure TCompOptForm.FormCreate(Sender: TObject);
var
	I : integer;
begin
	LoadText;

	// Create local copies we can modify
	fBinDirCopy := TStringList.Create;
	fLibDirCopy := TStringList.Create;
	fCDirCopy := TStringList.Create;
	fCppDirCopy := TStringList.Create;

	// fill compiler lists
	for I := 0 to devCompilerSets.Count-1 do
		cmbCompilerSetComp.Items.Add(devCompilerSets[i].Name);

	fOldIndex := -1;
	if devCompilerSets.CurrentIndex < cmbCompilerSetComp.Items.Count then
		cmbCompilerSetComp.ItemIndex := devCompilerSets.CurrentIndex
	else if cmbCompilerSetComp.Items.Count > 0 then
		cmbCompilerSetComp.ItemIndex := 0;
	cmbCompilerSetCompChange(nil);

	cbCompAddClick(cbCompAdd);
	cbLinkerAddClick(cbLinkerAdd);
end;

procedure TCompOptForm.LoadText;
begin
	// Set interface font
	Font.Name := devData.InterfaceFont;
	Font.Size := devData.InterfaceFontSize;

	Caption:=                            Lang[ID_COPT];

	// Tabs
	tabCompiler.Caption:=                Lang[ID_COPT_COMPTAB];
	tabCodeGen.Caption:=                 Lang[ID_COPT_CODEGENTAB];
	tabDirectories.Caption:=             Lang[ID_COPT_DIRTAB];
	tabPrograms.Caption:=                Lang[ID_COPT_PROGRAMSTAB];
	tabMakefile.Caption:=                Lang[ID_COPT_MAKEFILETAB];

	// Directories, subtabs
	DirTabs.Tabs.Clear;
	DirTabs.Tabs.Append(Lang[ID_COPT_BIN]);
	DirTabs.Tabs.Append(Lang[ID_COPT_LIB]);
	DirTabs.Tabs.Append(Lang[ID_COPT_INCC]);
	DirTabs.Tabs.Append(Lang[ID_COPT_INCCPP]);

	// Buttons for all tabs
	btnReplace.Caption:=                 Lang[ID_BTN_REPLACE];
	btnAdd.Caption:=                     Lang[ID_BTN_ADD];
	btnDelete.Caption:=                  Lang[ID_BTN_DELETE];
	btnDelInval.Caption:=                Lang[ID_BTN_DELINVAL];
	btnOk.Caption:=                      Lang[ID_BTN_OK];
	btnCancel.Caption:=                  Lang[ID_BTN_CANCEL];
	btnHelp.Caption:=                    Lang[ID_BTN_HELP];

	// Checkboxes and delay
	cbCompAdd.Caption:=                  Lang[ID_COPT_ADDCOMP];
	cbLinkerAdd.Caption:=                Lang[ID_COPT_LINKADD];
	lblDelay.Caption:=                   Lang[ID_COPT_DELAY];
	lblDelayMsg.Caption:=                Lang[ID_COPT_DELAYMSG];

	// Makefile generation
	grpMakefileGen.Caption:=             ' '+Lang[ID_COPT_MAKEFILEGEN]+' ';
	cbFastDep.Caption:=                  Lang[ID_COPT_FASTDEP];

	OptionsTip.Caption:=                 Lang[ID_COPT_COMPILERTIP];

	// Programs (you may want to...)
	lblProgramsText.Caption:=            Lang[ID_COPT_PROGRAMS];

	// Text above compiler select
	grpCompSet.Caption:=                 ' '+Lang[ID_COPT_COMPSETS]+' ';

	// Set some hints too
	BtnFindCompilers.Hint:=              Lang[ID_COPT_FINDCOMPHINT];
	btnAddBlankCompilerSet.Hint:=        Lang[ID_COPT_ADDCOMPHINT];
	btnAddFilledCompilerSet.Hint:=       Lang[ID_COPT_ADDFOLDERCOMPHINT];
	btnDelCompilerSet.Hint:=             Lang[ID_COPT_DELCOMPHINT];
	btnRenameCompilerSet.Hint:=          Lang[ID_COPT_RENAMECOMPHINT];
end;

procedure TCompOptForm.btnBrws1Click(Sender: TObject);
var
	Obj: TEdit;
begin
	Obj:=nil;
	case TSpeedButton(Sender).Tag of
		2:  Obj:=GccEdit; //gcc
		3:  Obj:=GppEdit; //gpp
		4:  Obj:=MakeEdit; //make
		5:  Obj:=GdbEdit; //gdb
		6:  Obj:=WindresEdit; //windres
		7:  Obj:=DllwrapEdit; //dllwrap
		8:  Obj:=GprofEdit; //gprof
	end;

	if not Assigned(Obj) then
		Exit;

	with TOpenDialog.Create(Self) do try
		Filter:=FLT_ALLFILES;

		// Start in the bin folder
		if fBinDirCopy.Count > 0 then
			InitialDir := fBinDirCopy[0]
		else if devCompilerSets[cmbCompilerSetComp.ItemIndex].BinDir.Count > 0 then
			InitialDir := devCompilerSets[cmbCompilerSetComp.ItemIndex].BinDir[0];

		FileName := IncludeTrailingPathDelimiter(InitialDir) + Obj.Text;
		if Execute then
			Obj.Text := ExtractFileName(FileName);
	finally
		Free;
	end;
end;

procedure TCompOptForm.cmbCompilerSetCompChange(Sender: TObject);
begin
	// Don't allow browsing
	MainPages.Enabled := cmbCompilerSetComp.Items.Count > 0;
	btnDelCompilerSet.Enabled := cmbCompilerSetComp.Items.Count > 0;
	btnRenameCompilerSet.Enabled := cmbCompilerSetComp.Items.Count > 0;
	if cmbCompilerSetComp.Items.Count = 0 then
		MainPages.ActivePageIndex := 0;

	if fOldIndex = cmbCompilerSetComp.ItemIndex then Exit; // why the hell is this event firing anyway?

	// Save old if it still exists
	if (fOldIndex >= 0) and (fOldIndex < devCompilerSets.Count) then
		if (cmbCompilerSetComp.Tag = 1) and (MessageDlg(Format(Lang[ID_MSG_ASKSAVECLOSE],[cmbCompilerSetComp.Items[fOldIndex]]) , mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
			SaveSet(fOldIndex);

	fOldIndex := cmbCompilerSetComp.ItemIndex;
	CompOptionsFrame1.fCurrentIndex := fOldIndex;

	// Load the new set, apply new set to UI
	LoadSet(fOldIndex);
end;

procedure TCompOptForm.btnAddBlankCompilerSetClick(Sender: TObject);
var
	S: AnsiString;
begin
	S := 'New compiler';
	if not InputQuery(Lang[ID_COPT_NEWCOMPSET], Lang[ID_COPT_PROMPTNEWCOMPSET], S) or (S='') then
		Exit;

	// Add empty compiler set
	devCompilerSets.AddSet;

	// Add name to list
	cmbCompilerSetComp.ItemIndex := cmbCompilerSetComp.Items.Add(S);
	cmbCompilerSetCompChange(nil);

	// Mark modified
	InterfaceChange(nil);
end;

procedure TCompOptForm.btnAddFilledCompilerSetClick(Sender: TObject);
var
	S: AnsiString;
begin
	if not SelectDirectory('','',S) then
		Exit;

	// Add empty compiler set
	devCompilerSets.AddSet(S);

	// Add name to list
	cmbCompilerSetComp.ItemIndex := cmbCompilerSetComp.Items.Add(devCompilerSets[devCompilerSets.Count-1].Name);
	cmbCompilerSetCompChange(nil);

	// Mark modified
	InterfaceChange(nil);
end;

procedure TCompOptForm.btnDelCompilerSetClick(Sender: TObject);
var
	deleteindex : integer;
begin
	if cmbCompilerSetComp.ItemIndex <> -1 then begin

		// Politely ask first
		if MessageDlg(Lang[ID_COPT_DELETECOMPSET], mtConfirmation, [mbYes, mbNo], 0)=mrNo then
			Exit;

		// Remove from list
		deleteindex := cmbCompilerSetComp.ItemIndex;
		devCompilerSets.DeleteSet(deleteindex);
		cmbCompilerSetComp.Items.Delete(deleteindex);

		// load previous (if it exists)
		if cmbCompilerSetComp.Items.Count > 0 then
			cmbCompilerSetComp.ItemIndex := max(0,deleteindex - 1)
		else
			cmbCompilerSetComp.ItemIndex := -1;
		cmbCompilerSetCompChange(nil);
		cmbCompilerSetComp.Repaint;
	end;
end;

procedure TCompOptForm.btnRenameCompilerSetClick(Sender: TObject);
var
	S: AnsiString;
	selindex : integer;
begin
	if cmbCompilerSetComp.ItemIndex <> -1 then begin
		S:=cmbCompilerSetComp.Text;
		if not InputQuery(Lang[ID_COPT_RENAMECOMPSET], Lang[ID_COPT_PROMPTRENAMECOMPSET], S) or (S='') or (S=cmbCompilerSetComp.Text) then
			Exit;

		// Selection index is set to -1 when renaming...
		selindex := cmbCompilerSetComp.ItemIndex;
		cmbCompilerSetComp.Items[cmbCompilerSetComp.ItemIndex] := S;
		cmbCompilerSetComp.ItemIndex := selindex;
	end;
end;

procedure TCompOptForm.OptionsLinkClick(Sender: TObject);
begin
	ShellExecute(GetDesktopWindow(), 'open', PAnsiChar(TLabel(Sender).Caption), nil, nil, SW_SHOWNORMAL);
end;

procedure TCompOptForm.cbCompAddClick(Sender: TObject);
begin
	Commands.Enabled := TCheckBox(Sender).Checked;
	InterfaceChange(nil);
end;

procedure TCompOptForm.cbLinkerAddClick(Sender: TObject);
begin
	Linker.Enabled := TCheckBox(Sender).Checked;
	InterfaceChange(nil);
end;

procedure TCompOptForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	// Fix bug in VCL (http://qc.embarcadero.com/wc/qcmain.aspx?d=5265)
	CompOptionsFrame1.vle.Strings.Clear;

	// Destroy local copies we can modify
	fBinDirCopy.Free;
	fLibDirCopy.Free;
	fCDirCopy.Free;
	fCppDirCopy.Free;

	Action := caFree;
end;

procedure TCompOptForm.btnFindCompilersClick(Sender: TObject);
var
	dlgresult,I : integer;
begin
	dlgresult := MessageDlg(
		Format(Lang[ID_COPT_ADDCOMPILERS],[devDirs.Exec + 'MinGW32',devDirs.Exec + 'MinGW64']),mtConfirmation,[mbYes,mbNo,mbCancel],0);

	if dlgresult = mrCancel then Exit;

	// Dialog asked to clear the current compiler list...
	if dlgresult = mrYes then
		devCompilerSets.ClearSets;

	// Find a bunch of compilers
	devCompilerSets.FindSets;

	// Add names to selection box
	cmbCompilerSetComp.Clear;
	for I := 0 to devCompilerSets.Count - 1 do
		cmbCompilerSetComp.Items.Add(devCompilerSets[i].Name);

	// Set a default selection
	fOldIndex := -1;
	if cmbCompilerSetComp.Items.Count > 0 then
		cmbCompilerSetComp.ItemIndex := 0
	else
		cmbCompilerSetComp.ItemIndex := -1;

	// Change event does not happen when changing ItemIndex...
	cmbCompilerSetCompChange(nil);

	// Save them to disk immediately
	devCompilerSets.SaveSets; // save the list too
end;

procedure TCompOptForm.cmbCompilerSetCompEnter(Sender: TObject);
begin
	fOldIndex := cmbCompilerSetComp.ItemIndex;
end;

// Unite into one function?
procedure TCompOptForm.InterfaceChange(Sender: TObject);
begin
	cmbCompilerSetComp.Tag := 1;
end;

procedure TCompOptForm.InterfaceSave(Sender: TObject);
begin
	cmbCompilerSetComp.Tag := 0;
end;

end.