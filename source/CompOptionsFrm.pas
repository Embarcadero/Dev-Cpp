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
  Buttons, StdCtrls, Inifiles, ExtCtrls, ComCtrls, Spin,
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
    lblDelay: TLabel;
    seCompDelay: TSpinEdit;
    lblDelayMsg: TLabel;
    grpMakefileGen: TGroupBox;
    cbFastDep: TCheckBox;
    cbLinkerAdd: TCheckBox;
    Linker: TMemo;
    grpCompSet: TGroupBox;
    OptionsTip: TLabel;
    cmbCompilerSetComp: TComboBox;
    btnAddCompilerSet: TSpeedButton;
    btnDelCompilerSet: TSpeedButton;
    btnRenameCompilerSet: TSpeedButton;
    btnBrowse: TSpeedButton;
    btnBrowse2: TSpeedButton;
    btnBrowse3: TSpeedButton;
    btnBrowse4: TSpeedButton;
    btnBrowse5: TSpeedButton;
    btnBrowse6: TSpeedButton;
    btnBrowse7: TSpeedButton;
    btnBrowse8: TSpeedButton;
    CompOptionsFrame1: TCompOptionsFrame;
    btnFindCompilers: TSpeedButton;
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
    procedure btnAddCompilerSetClick(Sender: TObject);
    procedure btnDelCompilerSetClick(Sender: TObject);
    procedure btnRenameCompilerSetClick(Sender: TObject);
    procedure GccEditChange(Sender: TObject);
    procedure GppEditChange(Sender: TObject);
    procedure MakeEditChange(Sender: TObject);
    procedure GdbEditChange(Sender: TObject);
    procedure WindresEditChange(Sender: TObject);
    procedure DllwrapEditChange(Sender: TObject);
    procedure GprofEditChange(Sender: TObject);
    procedure OptionsLinkClick(Sender: TObject);
    procedure cbCompAddClick(Sender: TObject);
    procedure cbLinkerAddClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnFindCompilersClick(Sender: TObject);
  private
    fBins: AnsiString;
    fLibs: AnsiString;
    fC: AnsiString;
    fCpp: AnsiString;
    CurrentSet : Integer;
    procedure SetOptions(first : boolean);
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
begin
	// Set general options for the current compiler...
	devCompiler.AddtoComp := cbCompAdd.Checked;
	devCompiler.AddtoLink := cbLinkerAdd.Checked;

	devCompiler.CompOpts := Commands.Lines.Text;
	devCompiler.LinkOpts := Linker.Lines.Text;

	devCompiler.Delay := seCompDelay.Value;
	devCompiler.FastDep := cbFastDep.Checked;

	// directories are saved to devCompiler by the UI components

	// compiler/linker flags are saved to devCompiler by the UI components

	// program names are saved to devCompiler by the UI components

	// write compiler list
	devCompiler.Sets.Assign(cmbCompilerSetComp.Items);
	devCompiler.WriteSets;

	// rewrite current compiler options
	devCompiler.SaveSet(CurrentSet);

	// Set Path with New Bins
	SetPath(fBins);
end;

procedure TCompOptForm.SetOptions(first : boolean);
begin
	// fill compiler lists
	if first then begin
		cmbCompilerSetComp.Items.Assign(devCompiler.Sets);

		if devCompiler.CurrentIndex < cmbCompilerSetComp.Items.Count then
			cmbCompilerSetComp.ItemIndex := devCompiler.CurrentIndex
		else if cmbCompilerSetComp.Items.Count > 0 then
			cmbCompilerSetComp.ItemIndex := 0;
	end;

	// Load the new set
	CurrentSet := cmbCompilerSetComp.ItemIndex;
	if CurrentSet <> -1 then
		devCompiler.LoadSet(cmbCompilerSetComp.ItemIndex)
	else
		devCompiler.ClearSet;

		// Apply the new set to the UI
	with devCompiler do begin
		fBins := BinDir;
		fC := CDir;
		fCpp := CppDir;
		fLibs := LibDir;
		Commands.Lines.Text := CompOpts;
		Linker.Lines.Text := LinkOpts;
		cbCompAdd.Checked := AddtoComp;
		cbLinkerAdd.Checked := AddtoLink;
		Commands.Lines.Text := CompOpts;
		Linker.Lines.Text := LinkOpts;
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

	// fill tab controls
	CompOptionsFrame1.FillOptions(nil);
	DirTabsChange(Self);
end;

procedure TCompOptForm.btnHelpClick(Sender: TObject);
begin
	OpenHelpFile;
end;

procedure TCompOptForm.DirTabsChange(Sender: TObject);
begin
  case DirTabs.TabIndex of
   0: StrtoList(fBins, TStrings(lstDirs.Items));
   1: StrtoList(fLibs, TStrings(lstDirs.Items));
   2: StrtoList(fC,    TStrings(lstDirs.Items));
   3: StrtoList(fCpp,  TStrings(lstDirs.Items));
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
   0: fBins:= ListtoStr(lstDirs.Items);
   1: fLibs:= ListtoStr(lstDirs.Items);
   2: fC:=    ListtoStr(lstDIrs.Items);
   3: fCpp:=  ListtoStr(lstDirs.Items);
  end;
  edEntry.SetFocus;

  devCompiler.BinDir:=fBins;
  devCompiler.CDir:=fC;
  devCompiler.CppDir:=fCpp;
  devCompiler.LibDir:=fLibs;
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
   0: fBins:= ListtoStr(lstDirs.Items);
   1: fLibs:= ListtoStr(lstDirs.Items);
   2: fC:=    ListtoStr(lstDIrs.Items);
   3: fCpp:=  ListtoStr(lstDirs.Items);
  end;

  devCompiler.BinDir:=fBins;
  devCompiler.CDir:=fC;
  devCompiler.CppDir:=fCpp;
  devCompiler.LibDir:=fLibs;

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
begin
	LoadText;
	SetOptions(true);
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
	btnAddCompilerSet.Hint:=             Lang[ID_COPT_ADDCOMPHINT];
	btnDelCompilerSet.Hint:=             Lang[ID_COPT_DELCOMPHINT];
	btnRenameCompilerSet.Hint:=          Lang[ID_COPT_RENAMECOMPHINT];
end;

procedure TCompOptForm.cmbCompilerSetCompChange(Sender: TObject);
begin
	// Save the old set
	devCompiler.CompOpts := Commands.Lines.Text;
	devCompiler.LinkOpts := Linker.Lines.Text;

	devCompiler.AddtoLink := cbLinkerAdd.Checked;
	devCompiler.AddtoComp := cbCompAdd.Checked;

	devCompiler.FastDep := cbFastDep.Checked;
	devCompiler.Delay := seCompDelay.Value;

	// other settings are saved to devCompiler by the UI components!

	devCompiler.SaveSet(CurrentSet);

	// Load the new set, apply new set to UI
	SetOptions(false);
end;

procedure TCompOptForm.btnBrws1Click(Sender: TObject);
var
  sl: TStringList;
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

  dmMain.OpenDialog.Filter:=FLT_ALLFILES;
  sl:=TStringList.Create;
  try
    sl.Delimiter:=';';
    sl.DelimitedText:=devCompiler.BinDir;
    if sl.Count>0 then
      dmMain.OpenDialog.InitialDir:=sl[0];
  finally
    sl.Free;
  end;

  dmMain.OpenDialog.FileName:=IncludeTrailingPathDelimiter(dmMain.OpenDialog.InitialDir)+Obj.Text;
  if dmMain.OpenDialog.Execute then begin
    Obj.Text:=ExtractFileName(dmMain.OpenDialog.FileName);
    devCompiler.gccName:=GccEdit.Text;
    devCompiler.gppName:=GppEdit.Text;
    devCompiler.gdbName:=GdbEdit.Text;
    devCompiler.makeName:=MakeEdit.Text;
    devCompiler.windresName:=WindresEdit.Text;
    devCompiler.dllwrapName:=DllwrapEdit.Text;
    devCompiler.gprofName:=GprofEdit.Text;
  end;
end;

procedure TCompOptForm.btnAddCompilerSetClick(Sender: TObject);
var
  S: AnsiString;
begin
  S:='New compiler';
  if not InputQuery(Lang[ID_COPT_NEWCOMPSET], Lang[ID_COPT_PROMPTNEWCOMPSET], S) or (S='') then
    Exit;

  cmbCompilerSetComp.ItemIndex:=cmbCompilerSetComp.Items.Add(S);
  cmbCompilerSetCompChange(nil);
  devCompiler.Sets.Add(S);
end;

procedure TCompOptForm.btnDelCompilerSetClick(Sender: TObject);
begin
	if cmbCompilerSetComp.ItemIndex <> -1 then begin
		if MessageDlg(Lang[ID_COPT_DELETECOMPSET], mtConfirmation, [mbYes, mbNo], 0)=mrNo then
			Exit;

		devCompiler.Sets.Delete(cmbCompilerSetComp.ItemIndex);
		cmbCompilerSetComp.Items.Delete(cmbCompilerSetComp.ItemIndex);
		cmbCompilerSetComp.ItemIndex:=0;
		cmbCompilerSetCompChange(nil);
	end;
end;

procedure TCompOptForm.btnRenameCompilerSetClick(Sender: TObject);
var
	S: AnsiString;
begin
	if cmbCompilerSetComp.ItemIndex <> -1 then begin
		S:=cmbCompilerSetComp.Text;
		if not InputQuery(Lang[ID_COPT_RENAMECOMPSET], Lang[ID_COPT_PROMPTRENAMECOMPSET], S) or (S='') or (S=cmbCompilerSetComp.Text) then
			Exit;

		cmbCompilerSetComp.Items[cmbCompilerSetComp.ItemIndex]:=S;
		cmbCompilerSetComp.ItemIndex:=cmbCompilerSetComp.Items.IndexOf(S);
	end;
end;

procedure TCompOptForm.GccEditChange(Sender: TObject);
begin
  devCompiler.gccName := GccEdit.Text;
end;

procedure TCompOptForm.GppEditChange(Sender: TObject);
begin
  devCompiler.gppName := GppEdit.Text;
end;

procedure TCompOptForm.MakeEditChange(Sender: TObject);
begin
  devCompiler.makeName := MakeEdit.Text;
end;

procedure TCompOptForm.GdbEditChange(Sender: TObject);
begin
  devCompiler.gdbName := GdbEdit.Text;
end;

procedure TCompOptForm.WindresEditChange(Sender: TObject);
begin
  devCompiler.windresName := WindresEdit.Text;
end;

procedure TCompOptForm.DllwrapEditChange(Sender: TObject);
begin
  devCompiler.dllwrapName := DllwrapEdit.Text;
end;

procedure TCompOptForm.GprofEditChange(Sender: TObject);
begin
  devCompiler.gprofName := GprofEdit.Text;
end;

procedure TCompOptForm.OptionsLinkClick(Sender: TObject);
begin
	ShellExecute(GetDesktopWindow(), 'open', PAnsiChar(TLabel(Sender).Caption), nil, nil, SW_SHOWNORMAL);
end;

procedure TCompOptForm.cbCompAddClick(Sender: TObject);
begin
	Commands.Enabled := TCheckBox(Sender).Checked;
end;

procedure TCompOptForm.cbLinkerAddClick(Sender: TObject);
begin
	Linker.Enabled := TCheckBox(Sender).Checked;
end;

procedure TCompOptForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	// Fix bug in VCL (http://qc.embarcadero.com/wc/qcmain.aspx?d=5265)
	CompOptionsFrame1.vle.Strings.Clear;

	Action := caFree;
end;

procedure TCompOptForm.btnFindCompilersClick(Sender: TObject);
var
	compilername : AnsiString;
	dlgresult : integer;
begin
	dlgresult := MessageDlg(
		Format(Lang[ID_COPT_ADDCOMPILERS],[devDirs.Exec + 'MinGW32',devDirs.Exec + 'MinGW64']),mtConfirmation,[mbYes,mbNo,mbCancel],0);

	if dlgresult = mrCancel then Exit;

	cmbCompilerSetComp.Items.BeginUpdate;

	// Dialog asked to clear the current compiler list...
	if dlgresult = mrYes then
		devCompiler.Sets.Clear;

	// copy from InitializeOptions

	// Assume 64bit compilers are put in the MinGW64 folder
	if DirectoryExists(devDirs.Exec + 'MinGW64\') then begin

		compilername := GetInfoOfCompiler(devDirs.Exec + 'MinGW64\bin\');
		if compilername = '' then
			compilername := 'MinGW64';

		devCompiler.Sets.Add(compilername + ' 64-bit');
		devCompiler.SettoDefaults(compilername + ' 64-bit','MinGW64');
		devCompiler.SaveSet(devCompiler.Sets.Count-1);

		devCompiler.Sets.Add(compilername + ' 32-bit');
		devCompiler.SettoDefaults(compilername + ' 32-bit','MinGW64');
		devCompiler.SaveSet(devCompiler.Sets.Count-1);
	end;
	if DirectoryExists(devDirs.Exec + 'MinGW32\') then begin

		compilername := GetInfoOfCompiler(devDirs.Exec + 'MinGW32\bin\');
		if compilername = '' then
			compilername := 'MinGW32';

		devCompiler.Sets.Add(compilername + ' 32-bit');
		devCompiler.SettoDefaults(compilername + ' 32-bit','MinGW32');
		devCompiler.SaveSet(devCompiler.Sets.Count-1);
	end;

	// Write the compiler list
	devCompiler.WriteSets;

	cmbCompilerSetComp.Items.Assign(devCompiler.Sets);
	cmbCompilerSetComp.ItemIndex := 0;
	devCompiler.CurrentIndex := 0;

	cmbCompilerSetComp.Items.EndUpdate;

	cmbCompilerSetCompChange(nil);
end;

end.
