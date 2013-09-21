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
unit EnviroFrm;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, ExtCtrls, ExtDlgs, Buttons,
  CheckLst, Grids, ValEdit, ComCtrls;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Variants, Classes, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls, QComCtrls, QExtCtrls, QButtons,
  QCheckLst, QGrids;
{$ENDIF}

type
  TEnviroForm = class(TForm)
    dlgPic: TOpenPictureDialog;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    PagesMain: TPageControl;
    tabGeneral: TTabSheet;
    lblMRU: TLabel;
    lblMsgTabs: TLabel;
    lblLang: TLabel;
    lblTheme: TLabel;
    cbBackups: TCheckBox;
    cbMinOnRun: TCheckBox;
    cbDefCpp: TCheckBox;
    cbShowBars: TCheckBox;
    cbMultiLineTab: TCheckBox;
    rgbAutoOpen: TRadioGroup;
    cbdblFiles: TCheckBox;
    gbDebugger: TGroupBox;
    cbWatchHint: TCheckBox;
    cbNoSplashScreen: TCheckBox;
    gbProgress: TGroupBox;
    cbShowProgress: TCheckBox;
    cbAutoCloseProgress: TCheckBox;
    seMRUMax: TSpinEdit;
    cboTabsTop: TComboBox;
    cboLang: TComboBox;
    cboTheme: TComboBox;
    tabPaths: TTabSheet;
    lblUserDir: TLabel;
    lblTemplatesDir: TLabel;
    lblSplash: TLabel;
    lblIcoLib: TLabel;
    lblLangPath: TLabel;
    btnDefBrws: TSpeedButton;
    btnOutputbrws: TSpeedButton;
    btnBrwIcon: TSpeedButton;
    btnBrwLang: TSpeedButton;
    btnBrwSplash: TSpeedButton;
    edUserDir: TEdit;
    edTemplatesDir: TEdit;
    edSplash: TEdit;
    edIcoLib: TEdit;
    edLang: TEdit;
    gbAltConfig: TGroupBox;
    btnAltConfig: TSpeedButton;
    chkAltConfig: TCheckBox;
    edAltConfig: TEdit;
    tabExternal: TTabSheet;
    lblExternal: TLabel;
    btnExtAdd: TSpeedButton;
    btnExtDel: TSpeedButton;
    vleExternal: TValueListEditor;
    tabAssocs: TTabSheet;
    lblAssocFileTypes: TLabel;
    lblAssocDesc: TLabel;
    lstAssocFileTypes: TCheckListBox;
    tabCVS: TTabSheet;
    lblCVSExec: TLabel;
    lblCVSCompression: TLabel;
    btnCVSExecBrws: TSpeedButton;
    edCVSExec: TEdit;
    spnCVSCompression: TSpinEdit;
    chkCVSUseSSH: TCheckBox;
    UIfontlabel: TLabel;
    cbUIfont: TComboBox;
    cvsdownloadlabel: TLabel;
    cbUIfontsize: TComboBox;
    cbPauseConsole: TCheckBox;
    procedure BrowseClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure vleExternalEditButtonClick(Sender: TObject);
    procedure vleExternalValidate(Sender: TObject; ACol, ARow: Integer;const KeyName, KeyValue: string);
    procedure btnExtAddClick(Sender: TObject);
    procedure btnExtDelClick(Sender: TObject);
    procedure chkAltConfigClick(Sender: TObject);
    procedure cvsdownloadlabelClick(Sender: TObject);
    procedure cbUIfontDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure cbUIfontsizeDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure cbUIfontsizeChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    procedure LoadText;
  end;

implementation

uses
{$IFDEF WIN32}
  ShellAPI, Filectrl, devcfg, MultiLangSupport, version, datamod, utils, FileAssocs, ImageTheme, main;
{$ENDIF}
{$IFDEF LINUX}
  Xlib, devcfg, MultiLangSupport, version, datamod, utils, FileAssocs, ImageTheme;
{$ENDIF}

{$R *.dfm}

procedure TEnviroForm.BrowseClick(Sender: TObject);
var
 s: AnsiString;
begin
  case TComponent(Sender).Tag of
   1: // default dir browse
    begin
      s:= edUserDir.Text;
      if SelectDirectory(Lang[ID_ENV_SELUSERDIR], '', s) then
       edUserDir.Text:= IncludeTrailingPathDelimiter(s);
    end;

   2: // output dir browse
    begin
      s:= ExpandFileto(edTemplatesDir.Text, devDirs.Exec);
      if SelectDirectory(Lang[ID_ENV_SELTEMPLATESDIR], '', s) then
       edTemplatesDir.Text:= IncludeTrailingPathDelimiter(s);
    end;

    // why was it commented-out???
   3: // icon library browse
    begin
      s:= ExpandFileto(edIcoLib.Text, devDirs.Exec);
      if SelectDirectory(Lang[ID_ENV_SELICOLIB], '', s) then
       edIcoLib.Text:= IncludeTrailingPathDelimiter(s);
    end;

   4: // splash screen browse
    begin
      dlgPic.InitialDir:= ExtractFilePath(edSplash.Text);
      if dlgPic.Execute then
       edSplash.Text:= dlgPic.FileName;
    end;

   5: // Language Dir
    begin
      s:= ExpandFileto(edLang.Text, devDirs.Exec);
      if SelectDirectory(Lang[ID_ENV_SELLANGDIR], '', s) then
       edLang.Text:= IncludeTrailingPathDelimiter(ExtractRelativePath(devDirs.Exec, s));
    end;

   6: // CVS Executable Filename
    begin
      dmMain.OpenDialog.Filter:=FLT_ALLFILES;
      dmMain.OpenDialog.FileName:=edCVSExec.Text;
      if dmMain.OpenDialog.Execute then
        edCVSExec.Text:=dmMain.OpenDialog.FileName;
    end;

   7: // Alternate Configuration File
    begin
      dmMain.OpenDialog.Filter:=FLT_ALLFILES;
      dmMain.OpenDialog.FileName:=edAltConfig.Text;
      if dmMain.OpenDialog.Execute then
        edAltConfig.Text:=dmMain.OpenDialog.FileName;
    end;
  end;
end;

procedure TEnviroForm.FormShow(Sender: TObject);
var
	idx,sel: integer;
begin
	with devData do begin
		rgbAutoOpen.ItemIndex:= AutoOpen;
		cbDefCpp.Checked:= defCpp;
		cbShowBars.Checked:= ShowBars;
		cbMultiLineTab.Checked:= MultiLineTab;
		cbBackups.Checked:= BackUps;
		cbMinOnRun.Checked:= MinOnRun;
		cbdblFiles.Checked:= DblFiles;
		cbNoSplashScreen.Checked:= NoSplashScreen;
		cbPauseConsole.Checked:=ConsolePause;
		seMRUMax.Value:= MRUMax;

		// List the languages
		cboLang.Items.BeginUpdate;
		cboLang.Clear;
		for idx := 0 to Lang.Langs.Count - 1 do begin
			sel := cboLang.Items.Add(Lang.Langs.ValueFromIndex[idx]);
			if SameText(Lang.CurrentLanguage,cboLang.Items[sel]) then
				cboLang.ItemIndex := idx;
		end;
		cboLang.Items.EndUpdate;

		// List the themes
		cboTheme.Items.Clear;
		devImageThemes.GetThemeTitles(cboTheme.Items);
		cboTheme.ItemIndex := devImageThemes.IndexOf(devImageThemes.CurrentTheme.Title);

		cbShowProgress.Checked := ShowProgress;
		cbAutoCloseProgress.Checked := AutoCloseProgress;

		cbWatchHint.Checked := WatchHint;

		cboTabsTop.ItemIndex:= msgTabs;

		chkAltConfig.Checked:= UseAltConfigFile;
		edAltConfig.Text:= AltConfigFile;
		chkAltConfigClick(nil);

		edSplash.Text:= Splash;
		edIcoLib.Text:= ExtractRelativePath(devDirs.Exec, devDirs.Icons);
		edUserDir.Text:= devDirs.Default;
		edTemplatesDir.Text:= ExtractRelativePath(devDirs.Exec, devDirs.Templates);
		edLang.Text:= ExtractRelativePath(devDirs.Exec, devDirs.Lang);

		vleExternal.Strings.Assign(devExternalPrograms.Programs);
		for idx:=0 to vleExternal.Strings.Count-1 do
			vleExternal.ItemProps[idx].EditStyle:=esEllipsis;

		lstAssocFileTypes.Clear;
		for idx:=0 to AssociationsCount-1 do begin
			lstAssocFileTypes.Items.Add(Format('%s  (*.%s)', [Associations[idx, 1], Associations[idx, 0]]));
			lstAssocFileTypes.Checked[lstAssocFileTypes.Items.Count-1]:=IsAssociated(idx);
		end;

		edCVSExec.Text:= devCVSHandler.Executable;
		spnCVSCompression.Value:= devCVSHandler.Compression;
		chkCVSUseSSH.Checked:= devCVSHandler.UseSSH;

		// Add all fonts and select the current one
		cbUIfont.Items.Assign(Screen.Fonts);
		for idx:=0 to pred(cbUIfont.Items.Count) do
			if cbUIfont.Items.Strings[idx] = InterfaceFont then begin
				cbUIfont.ItemIndex := idx;
				break;
			end;

		// Do the same for the size selection
		for idx:=0 to pred(cbUIfontsize.Items.Count) do
			if strtoint(cbUIfontsize.Items.Strings[idx]) = InterfaceFontSize then begin
				cbUIfontsize.ItemIndex := idx;
				break;
			end;
	end;
end;

procedure TEnviroForm.btnOkClick(Sender: TObject);
var
	idx: integer;
	s : AnsiString;
begin
	if chkAltConfig.Enabled then begin
		if UseAltConfigFile<>chkAltConfig.Checked then
			MessageDlg(Lang[ID_ENV_CONFIGCHANGED], mtInformation, [mbOk], 0);
		UseAltConfigFile:= chkAltConfig.Checked and (edAltConfig.Text<>'');
		AltConfigFile:= edAltConfig.Text;
		UpdateAltConfigFile;
	end;

	with devData do begin
		DefCpp:= cbDefCpp.Checked;
		ShowBars:= cbShowBars.Checked;
		MultiLineTab:=  cbMultiLineTab.Checked;
		BackUps:=  cbBackups.Checked;
		MinOnRun:= cbMinOnRun.Checked;
		DblFiles:= cbdblFiles.Checked;
		ConsolePause:= cbPauseConsole.Checked;
		MRUMax:= seMRUMax.Value;
		if not MultiLineTab then begin
			if cboTabsTop.ItemIndex in [2,3] then begin
				MessageBox(application.handle,PAnsiChar('Multiline tabs must be enabled when using vertical tabs.'+#13#10#13#10+'Reverting to Top Tabs...'),PAnsiChar('Error'),MB_OK);
				cboTabsTop.ItemIndex := 0;
			end;
		end;
		MsgTabs:= cboTabsTop.ItemIndex;
		AutoOpen:= rgbAutoOpen.ItemIndex;
		Splash:= edSplash.Text;

		s := Lang.FileFromDescription(cboLang.Text);
		LangChange:= s <> Language;
		Language:= s;
		ThemeChange := cboTheme.Text <> devData.Theme;
		Theme := cboTheme.Text;
		NoSplashScreen := cbNoSplashScreen.Checked;
		ShowProgress := cbShowProgress.Checked;
		AutoCloseProgress := cbAutoCloseProgress.Checked;
		WatchHint := cbWatchHint.Checked;
		InterfaceFont := cbUIFont.Text;
		InterfaceFontSize := strtoint(cbUIfontsize.Text);

		MainForm.Font.Name := devData.InterfaceFont;
		MainForm.Font.Size := devData.InterfaceFontSize;
	end;

	devDirs.Icons:= IncludeTrailingPathDelimiter(ExpandFileto(edIcoLib.Text, devDirs.Exec));
	devDirs.Templates:= IncludeTrailingPathDelimiter(ExpandFileto(edTemplatesDir.Text, devDirs.Exec));
	devDirs.Default:= edUserDir.Text;

	if edLang.Text <> ExtractRelativePath(devDirs.Exec, devDirs.Lang) then begin
		devDirs.Lang:= IncludeTrailingPathDelimiter(ExpandFileto(edLang.Text, devDirs.Exec));
		Lang.CheckLanguageFiles;
	end;

	with dmMain.OpenDialog do begin
		OptionsEx:= [];
		Options:= Options - [ofOldStyleDialog, ofNoLongNames];
	end;

	dmMain.SaveDialog.OptionsEx:= dmMain.OpenDialog.OptionsEx;
	dmMain.SaveDialog.Options:= dmMain.OpenDialog.Options;

	devExternalPrograms.Programs.Assign(vleExternal.Strings);

	try
		for idx:=0 to AssociationsCount - 1 do
			if lstAssocFileTypes.Checked[idx] then
				Associate(idx)
			else
				Unassociate(idx);
	except
		MessageBox(application.handle,PAnsiChar(Lang[ID_ENV_UACERROR]),PAnsiChar(Lang[ID_ERROR]),MB_OK);
	end;

	devCVSHandler.Executable:= edCVSExec.Text;
	devCVSHandler.Compression:= spnCVSCompression.Value;
	devCVSHandler.UseSSH:= chkCVSUseSSH.Checked;
end;

procedure TEnviroForm.LoadText;
begin
	// Set interface font
	Font.Name := devData.InterfaceFont;
	Font.Size := devData.InterfaceFontSize;

  Caption:=                  Lang[ID_ENV];

  //Tabs
  tabGeneral.Caption:=           Lang[ID_ENV_GENTAB];
  tabPaths.Caption:=             Lang[ID_ENV_PATHTAB];
  tabAssocs.Caption:=            Lang[ID_ENV_FASSTAB];
  tabCVS.Caption:=               Lang[ID_ENV_CVSTAB];
  tabExternal.Caption:=          Lang[ID_ENV_EXTERNALS];

  //Buttons
  btnOk.Caption:=                Lang[ID_BTN_OK];
  btnCancel.Caption:=            Lang[ID_BTN_CANCEL];
  btnHelp.Caption:=              Lang[ID_BTN_HELP];

  //Controls
  cbDefCpp.Caption:=             Lang[ID_ENV_DEFCPP];
  cbShowBars.Caption:=           Lang[ID_ENV_SHOWBARS];
  cbMultiLineTab.Caption:=       Lang[ID_ENV_MULTILINETABS];
  cbBackups.Caption:=            Lang[ID_ENV_BACKUPS];
  cbMinOnRun.Caption:=           Lang[ID_ENV_MINONRUN];
  cbdblFiles.Caption:=           Lang[ID_ENV_DBLFILES];
  cbNoSplashScreen.Caption:=     Lang[ID_ENV_NOSPLASH];
  cbPauseConsole.Caption:=       Lang[ID_ENV_PAUSECONSOLE];

  gbProgress.Caption:=           ' '+Lang[ID_ENV_COMPPROGRESSWINDOW]+' ';
  cbShowProgress.Caption:=       Lang[ID_ENV_SHOWPROGRESS];
  cbAutoCloseProgress.Caption:=  Lang[ID_ENV_AUTOCLOSEPROGRESS];

  cbWatchHint.Caption :=         Lang[ID_ENV_WATCHHINT];
  gbDebugger.Caption :=          ' '+Lang[ID_ENV_DEBUGGER]+' ';

  rgbAutoOpen.Caption:=          ' '+Lang[ID_ENV_AUTOOPEN]+' ';
  rgbAutoOpen.Items[0]:=         Lang[ID_ENV_AUTOALL];
  rgbAutoOpen.Items[1]:=         Lang[ID_ENV_AUTOFIRST];
  rgbAutoOpen.Items[2]:=         Lang[ID_ENV_AUTOREMEMBER];
  rgbAutoOpen.Items[3]:=         Lang[ID_ENV_AUTONONE];

  gbAltConfig.Caption:=          ' '+Lang[ID_ENV_GBALTCONFIG]+' ';
  lblLang.Caption:=              Lang[ID_ENV_LANGUAGE];
  lblTheme.Caption:=             Lang[ID_ENV_THEME];
  lblmsgTabs.Caption:=           Lang[ID_ENV_MSGTABS];
  lblMRU.Caption:=               Lang[ID_ENV_MRU];

  lblUserDir.Caption:=           Lang[ID_ENV_USERDIR];
  lblTemplatesDir.Caption:=      Lang[ID_ENV_TEMPLATESDIR];
  lblIcoLib.Caption:=            Lang[ID_ENV_ICOLIB];
  lblSplash.Caption:=            Lang[ID_ENV_SPLASH];
  lblLangPath.Caption:=          Lang[ID_ENV_SELLANGDIR];

  // externals tab
  lblExternal.Caption:=          Lang[ID_ENV_EXTERNPROGASSOCS];
  vleExternal.TitleCaptions.Clear;
  vleExternal.TitleCaptions.Add(Lang[ID_ENV_EXTERNEXT]);
  vleExternal.TitleCaptions.Add(Lang[ID_ENV_EXTERNPROG]);

  btnExtAdd.Caption:=            Lang[ID_BTN_ADD];
  btnExtDel.Caption:=            Lang[ID_BTN_DELETE];

  // associations tab
  lblAssocFileTypes.Caption:=    Lang[ID_ENV_FASSTYPES];
  lblAssocDesc.Caption:=         Lang[ID_ENV_FASSDESC];

  // CVS support tab
  lblCVSExec.Caption:=       Lang[ID_ENV_CVSEXE];
  lblCVSCompression.Caption:=Lang[ID_ENV_CVSCOMPR];
  chkCVSUseSSH.Caption:=     Lang[ID_ENV_CVSUSESSH];
  uifontlabel.Caption:=      Lang[ID_ENV_UIFONT];
end;

procedure TEnviroForm.btnHelpClick(Sender: TObject);
begin
	OpenHelpFile;
end;

procedure TEnviroForm.FormCreate(Sender: TObject);
begin
	LoadText;
end;

procedure TEnviroForm.vleExternalEditButtonClick(Sender: TObject);
begin
  if Trim(vleExternal.Cells[0, vleExternal.Row])='' then begin
    MessageDlg('Add an extension first!', mtError, [mbOk], 0);
    Exit;
  end;

  with dmMain.OpenDialog do begin
    Filter:=FLT_ALLFILES;
    if Execute then
      vleExternal.Cells[1, vleExternal.Row]:=Filename;
  end;
end;

procedure TEnviroForm.vleExternalValidate(Sender: TObject; ACol,
  ARow: Integer; const KeyName, KeyValue: string);
var
  idx: integer;
begin
  if vleExternal.FindRow(KeyName, idx) and (idx<>ARow) then begin
    MessageDlg('Extension exists...', mtError, [mbOk], 0);
    vleExternal.Col:=0;
    vleExternal.Row:=ARow;
    Abort;
  end;
  vleExternal.ItemProps[ARow-1].EditStyle:=esEllipsis;
end;

procedure TEnviroForm.btnExtAddClick(Sender: TObject);
begin
  vleExternal.InsertRow('', '', True);
  vleExternal.Row:=vleExternal.RowCount-1;
  vleExternal.Col:=0;
  vleExternal.SetFocus;
end;

procedure TEnviroForm.btnExtDelClick(Sender: TObject);
begin
  if (vleExternal.Row = 1) and (vleExternal.RowCount = 2) and (vleExternal.Cells[0, 1] = '') then
    exit;
  if (vleExternal.RowCount > 1) and (vleExternal.Row > 0) then
    vleExternal.DeleteRow(vleExternal.Row);
end;

procedure TEnviroForm.chkAltConfigClick(Sender: TObject);
begin
  chkAltConfig.Enabled:=ConfigMode <> CFG_PARAM;
  edAltConfig.Enabled:= chkAltConfig.Enabled and chkAltConfig.Checked;
  btnAltConfig.Enabled:= chkAltConfig.Enabled and chkAltConfig.Checked;
end;

procedure TEnviroForm.cvsdownloadlabelClick(Sender: TObject);
begin
	ShellExecute(GetDesktopWindow(), 'open', PAnsiChar(TLabel(Sender).Caption), nil, nil, SW_SHOWNORMAL);
end;

procedure TEnviroForm.cbUIfontDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
	with TComboBox(Control) do begin
		Canvas.Font.Name := Items.Strings[Index];
		Canvas.Font.Size := strtoint(cbUIfontsize.Text);
		Canvas.FillRect(Rect);
		Canvas.TextOut(Rect.Left, Rect.Top, Canvas.Font.Name);
	end;
end;

procedure TEnviroForm.cbUIfontsizeDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
	with TComboBox(Control) do begin
		Canvas.Font.Name := cbUIfont.Text;
		Canvas.Font.Size := strtoint(Items.Strings[Index]);
		Canvas.FillRect(Rect);
		Canvas.TextOut(Rect.Left, Rect.Top, Items.Strings[Index]);
	end;
end;

procedure TEnviroForm.cbUIfontsizeChange(Sender: TObject);
begin
	cbUIfont.Repaint;
end;

procedure TEnviroForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	Action := caFree;
end;

end.
