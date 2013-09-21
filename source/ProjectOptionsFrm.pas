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

{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}

unit ProjectOptionsFrm;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtDlgs, StdCtrls, ExtCtrls, Buttons, ComCtrls, main, project,
  devTabs, prjtypes, XPMenu, Spin, Grids, ValEdit, CompilerOptionsFrame;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QExtCtrls, QButtons, QComCtrls, main, project,
  devTabs, prjtypes, QGrids, CompilerOptionsFrame, Types;
{$ENDIF}

type
  TfrmProjectOptions = class(TForm)
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    dlgOpen: TOpenDialog;
    dlgPic: TOpenPictureDialog;
    PageControl: TdevPages;
    tabGeneral: TdevPage;
    tabFilesDir: TdevPage;
    lblPrjName: TLabel;
    grpIcon: TGroupBox;
    btnIconBrwse: TBitBtn;
    btnIconLib: TBitBtn;
    grpType: TGroupBox;
    lstType: TListBox;
    edProjectName: TEdit;
    btnUp: TSpeedButton;
    btnDown: TSpeedButton;
    SubTabs: TdevTabs;
    lstList: TListBox;
    btnAdd: TButton;
    btnDelete: TButton;
    btnReplace: TButton;
    edEntry: TEdit;
    btnDelInval: TButton;
    btnRemoveIcon: TBitBtn;
    Panel1: TPanel;
    Icon: TImage;
    tabOutputDir: TdevPage;
    grpOutDirectories: TGroupBox;
    lblExeOutput: TLabel;
    lblObjOutput: TLabel;
    edExeOutput: TEdit;
    edObjOutput: TEdit;
    tabMakefile: TdevPage;
    Panel2: TPanel;
    IncMakeLabel: TLabel;
    MakeIncludes: TListBox;
    btnMakUp: TSpeedButton;
    btnMakDown: TSpeedButton;
    edMakeInclude: TEdit;
    btnMakReplace: TButton;
    btnMakAdd: TButton;
    btnMakDelete: TButton;
    btnMakDelInval: TButton;
    dlgMakeInclude: TOpenDialog;
    InfoMakeBtn: TSpeedButton;
    edOverridenOutput: TEdit;
    chkOverrideOutput: TCheckBox;
    XPMenu: TXPMenu;
    tabFiles: TdevPage;
    lvFiles: TTreeView;
    lblProjectFiles: TLabel;
    grpUnitOptions: TGroupBox;
    chkCompile: TCheckBox;
    chkCompileCpp: TCheckBox;
    tabVersion: TdevPage;
    chkVersionInfo: TCheckBox;
    grpVersion: TGroupBox;
    lblVerMajor: TLabel;
    lblVerMinor: TLabel;
    lblVerRel: TLabel;
    lblVerBuild: TLabel;
    lblVerAdditional: TLabel;
    lblVerLang: TLabel;
    spnMajor: TSpinEdit;
    spnMinor: TSpinEdit;
    spnRelease: TSpinEdit;
    spnBuild: TSpinEdit;
    vleVersion: TValueListEditor;
    cmbLangID: TComboBox;
    tabCompiler: TdevPage;
    chkSupportXP: TCheckBox;
    OpenLibDialog: TOpenDialog;
    chkOverrideBuildCmd: TCheckBox;
    txtOverrideBuildCmd: TMemo;
    lblFname: TLabel;
    lblPrjFname: TLabel;
    lblUnits: TLabel;
    lblPrjUnits: TLabel;
    lblPrjOutputFname: TLabel;
    lblPrjOutput: TLabel;
    chkLink: TCheckBox;
    lblPriority: TLabel;
    spnPriority: TSpinEdit;
    chkAutoIncBuild: TCheckBox;
    tabCompOpts: TdevPage;
    CompOptionsFrame1: TCompOptionsFrame;
    cmbCompiler: TComboBox;
    lblCompilerSet: TLabel;
    lblCompileInfo: TLabel;
    lblAdditions: TLabel;
    lblCompiler: TLabel;
    edCompiler: TMemo;
    lblCppCompiler: TLabel;
    edCppCompiler: TMemo;
    lblLinker: TLabel;
    edLinker: TMemo;
    AddLibBtn: TBitBtn;
    cbUseCustomMakefile: TCheckBox;
    edCustomMakefile: TEdit;
    dlgCustomMake: TOpenDialog;
    btnBrowse: TSpeedButton;
    btnExeOutDir: TSpeedButton;
    btnObjOutDir: TSpeedButton;
    btnCustomMakeBrowse: TSpeedButton;
    btnMakeBrowse: TSpeedButton;
    procedure ListClick(Sender: TObject);
    procedure EditChange(SEnder: TObject);
    procedure ButtonClick(Sender: TObject);
    procedure UpDownClick(Sender: TObject);
    procedure BrowseClick(Sender: TObject);
    procedure SubTabsChange(Sender: TObject);
    procedure SubTabsChanging(Sender: TObject; NewIndex: Integer;
      var AllowChange: Boolean);
    procedure FormShow(Sender: TObject);
    procedure btnIconLibClick(Sender: TObject);
    procedure btnIconBrwseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnRemoveIconClick(Sender: TObject);
    procedure BrowseExecutableOutDirClick(Sender: TObject);
    procedure BrowseObjDirClick(Sender: TObject);
    procedure btnMakeBrowseClick(Sender: TObject);
    procedure btnMakClick(Sender: TObject);
    procedure MakButtonClick(Sender: TObject);
    procedure edMakeIncludeChange(Sender: TObject);
    procedure MakeIncludesClick(Sender: TObject);
    procedure InfoMakeBtnClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure chkOverrideOutputClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure chkCompileClick(Sender: TObject);
    procedure chkVersionInfoClick(Sender: TObject);
    procedure lvFilesChange(Sender: TObject; Node: TTreeNode);
    procedure cmbCompilerChange(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure lstTypeClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure AddLibBtnClick(Sender: TObject);
    procedure txtOverrideBuildCmdChange(Sender: TObject);
    procedure spnPriorityChange(Sender: TObject);
    procedure btnCustomMakeBrowseClick(Sender: TObject);
    procedure cbUseCustomMakefileClick(Sender: TObject);
    procedure MakeIncludesDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
  private
    fOptions: TProjOptions;
    fIcon: string;
    fProject: TProject;
    procedure SetOptions(Value: TProjOptions);
    function GetOptions: TProjOptions;
    procedure UpdateButtons;
    procedure UpdateMakButtons;
    procedure LoadText;
    procedure InitVersionInfo;
    function DefaultBuildCommand(idx: integer): string;
    procedure SaveDirSettings;
  public
    property Options: TProjOptions read GetOptions write SetOptions;
    property Project: TProject read fProject write fProject;
  end;

implementation

uses
{$IFDEF WIN32}
  FileCtrl, devcfg, IconFrm, utils, MultiLangSupport, version;
{$ENDIF}
{$IFDEF LINUX}
  devcfg, IconFrm, utils, MultiLangSupport, version;
{$ENDIF}

{$R *.dfm}

procedure TfrmProjectOptions.UpdateButtons;
begin
  btnAdd.Enabled:= edEntry.Text <> '';
  if lstList.ItemIndex>= 0 then
   begin
     btnDelete.Enabled:= TRUE;
     btnReplace.Enabled:= btnAdd.Enabled;
     btnUp.Enabled:= lstList.ItemIndex> 0;
     btnDown.Enabled:= lstList.ItemIndex < (lstList.Items.Count -1);
   end
  else
   begin
     btnDelete.Enabled:= FALSE;
     btnReplace.Enabled:= FALSE;
     btnUp.Enabled:= FALSE;
     btnDown.Enabled:= FALSE;
   end;
  btnDelInval.Enabled:= lstList.Items.Count> 0;
end;

procedure TfrmProjectOptions.UpdateMakButtons;
begin
  btnMakAdd.Enabled:= edMakeInclude.Text <> '';
  if MakeIncludes.ItemIndex>= 0 then
   begin
     btnMakDelete.Enabled:= TRUE;
     btnMakReplace.Enabled:= btnMakAdd.Enabled;
     btnMakUp.Enabled:= MakeIncludes.ItemIndex> 0;
     btnMakDown.Enabled:= MakeIncludes.ItemIndex < (MakeIncludes.Items.Count -1);
   end
  else
   begin
     btnMakDelete.Enabled:= FALSE;
     btnMakReplace.Enabled:= FALSE;
     btnMakUp.Enabled:= FALSE;
     btnMakDown.Enabled:= FALSE;
   end;
  btnMakDelInval.Enabled:= MakeIncludes.Items.Count> 0;
end;

procedure TfrmProjectOptions.BrowseClick(Sender: TObject);
var
{$IFDEF WIN32}
  NewItem: string;
{$ENDIF}
{$IFDEF LINUX}
  NewItem: WideString;
{$ENDIF}
begin
  if (Trim(edEntry.Text)<>'') and DirectoryExists(Trim(edEntry.Text)) then
    newitem:=edEntry.Text
  else
    newitem:=devDirs.Default;
  case SubTabs.TabIndex of
   0: // Lib tab
    begin
      if SelectDirectory('Library Directory', '', newitem) then
       edEntry.Text:=  NewItem;
    end;
   1: // Include tab
    begin
      if SelectDirectory('Include Directory', '', newitem) then
       edEntry.Text:=  NewItem;
    end;
   2: // Resource dir Tab
    begin
      if SelectDirectory('Resource Directory', '', newitem) then
       edEntry.Text:=  NewItem;
    end;
  end;
  edEntry.SetFocus;
end;

procedure TfrmProjectOptions.ButtonClick(Sender: TObject);
var
 idx: integer;
begin
  case (Sender as TComponent).Tag of
   1: begin
        lstList.Items[lstList.ItemIndex] := TrimRight(edEntry.Text);
        case SubTabs.TabIndex of
          0 : fOptions.Libs[lstList.ItemIndex] := TrimRight(edEntry.Text);
          1 : fOptions.Includes[lstList.ItemIndex] := TrimRight(edEntry.Text);
          2 : fOptions.ResourceIncludes[lstList.ItemIndex] := TrimRight(edEntry.Text);
        end;
      end;
   2: begin
        lstList.Items.Add(TrimRight(edEntry.Text));
        case SubTabs.TabIndex of
          0 : fOptions.Libs.Add(TrimRight(edEntry.Text));
          1 : fOptions.Includes.Add(TrimRight(edEntry.Text));
          2 : fOptions.ResourceIncludes.Add(TrimRight(edEntry.Text));
        end;
      end;
   3: begin
        case SubTabs.TabIndex of
          0 : fOptions.Libs.Delete(lstList.ItemIndex);
          1 : fOptions.Includes.Delete(lstList.ItemIndex);
          2 : fOptions.ResourceIncludes.Delete(lstList.ItemIndex);
        end;
        lstList.DeleteSelected;
       end;
   4:
    begin
      if lstList.Items.Count> 0 then
       for idx:= pred(lstList.Items.Count) downto 0 do
        case SubTabs.TabIndex of
         0: if not DirectoryExists(lstList.Items[idx]) then begin
                fOptions.Libs.Delete(idx);
                lstList.Items.Delete(idx);
            end;
         1: if not DirectoryExists(lstList.Items[idx]) then begin
                fOptions.Includes.Delete(idx);
                lstList.Items.Delete(idx);
            end;
         2: if not DirectoryExists(lstList.Items[idx]) then begin
                fOptions.ResourceIncludes.Delete(idx);
                lstList.Items.Delete(idx);
            end;
        end;
    end;
  end;
  edEntry.Clear;
  UpdateButtons;
end;

procedure TfrmProjectOptions.EditChange(SEnder: TObject);
begin
  UpdateButtons;
end;

procedure TfrmProjectOptions.ListClick(Sender: TObject);
begin
  UpdateButtons;
  edEntry.Text:= lstList.Items[lstList.Itemindex];
end;

procedure TfrmProjectOptions.UpDownClick(Sender: TObject);
var
 idx: integer;
begin
  idx:= lstList.ItemIndex;
  if Sender = btnUp then
   begin
     lstList.Items.Exchange(lstList.ItemIndex, lstList.ItemIndex -1);
     lstList.ItemIndex:= idx -1;
   end
  else
   if Sender = btnDown then
    begin
      lstList.Items.Exchange(lstList.ItemIndex, lstList.ItemIndex +1);
      lstList.ItemIndex:= idx +1;
    end;
  UpdateButtons;
end;

function TfrmProjectOptions.GetOptions: TProjOptions;
var
  I: integer;
begin
  with fOptions do
   begin
     Icon:= fIcon;
     { I had to remove the delimiter text thing, since it causes the edit box to add
       unwanted quotes around the string. We could remove them but that could conflict with user's own quotes,
       for example when using filenames containing spaces }
     {
     *mandrav*:

      Colin, when I did it this way, I wanted to have one option per-line so that
      it is easy to see and spot errors etc.
      Of course, you 've found a bug but I don't think that this is a good solution...

      I will try something different now: I will create my own delimited string (delimited by "_@@_").
      Now it has nothing to do with quotes or spaces. If the user enters quotes, that's
      fine. If he doesn't but he should (like a filename with spaces), then it's
      his/her problem. Even if he did it at command line he still would have compile
      errors after all...
      All I have to do when I want the actual string back, is call StringReplace() et voila :)
     }

     // mandrav: start
     cmdlines.Linker:= edLinker.Lines.Text;
     cmdLines.Compiler:='';
     for I:=0 to edCompiler.Lines.Count-1 do
      cmdLines.Compiler := cmdLines.Compiler + edCompiler.Lines[I] + '_@@_';
     cmdLines.CppCompiler:='';
     for I:=0 to edCppCompiler.Lines.Count-1 do
      cmdLines.CppCompiler := cmdLines.CppCompiler + edCppCompiler.Lines[I] + '_@@_';
     cmdLines.Linker:='';
     for I:=0 to edLinker.Lines.Count-1 do
      cmdLines.Linker := cmdLines.Linker + edLinker.Lines[I] + '_@@_';
     // mandrav: end

     typ:= lstType.ItemIndex;

     ExeOutput := edExeOutput.Text;
     ObjectOutput := edObjOutput.Text;
     OverrideOutput := chkOverrideOutput.Checked;
     OverridenOutput := edOverridenOutput.Text;
     if cbUseCustomMakefile.Checked and FileExists(edCustomMakefile.Text) then
       fProject.UseCustomMakefile := true
     else
       fProject.UseCustomMakefile := false;
     fProject.CustomMakefile:=edCustomMakefile.Text;
     MakeIncludes.Clear;
     MakeIncludes.AddStrings(Self.MakeIncludes.Items);

     fOptions.SupportXPThemes:=chkSupportXP.Checked;
     fOptions.CompilerSet:=cmbCompiler.ItemIndex;

     fOptions.IncludeVersionInfo:=chkVersionInfo.Checked;

     fOptions.VersionInfo.Major:=spnMajor.Value;
     fOptions.VersionInfo.Minor:=spnMinor.Value;
     fOptions.VersionInfo.Release:=spnRelease.Value;
     fOptions.VersionInfo.Build:=spnBuild.Value;
     fOptions.VersionInfo.AutoIncBuildNr:=chkAutoIncBuild.Checked;

     fOptions.VersionInfo.FileDescription:=   vleVersion.Cells[1, 0];
     fOptions.VersionInfo.FileVersion:=       vleVersion.Cells[1, 1];
     fOptions.VersionInfo.ProductName:=       vleVersion.Cells[1, 2];
     fOptions.VersionInfo.ProductVersion:=    vleVersion.Cells[1, 3];
     fOptions.VersionInfo.OriginalFilename:=  vleVersion.Cells[1, 4];
     fOptions.VersionInfo.InternalName:=      vleVersion.Cells[1, 5];
     fOptions.VersionInfo.CompanyName:=       vleVersion.Cells[1, 6];
     fOptions.VersionInfo.LegalCopyright:=    vleVersion.Cells[1, 7];
     fOptions.VersionInfo.LegalTrademarks:=   vleVersion.Cells[1, 8];

     if cmbLangID.ItemIndex>-1 then
     begin
      //fOptions.VersionInfo.LanguageID:=Languages.LocaleID[cmbLangID.ItemIndex];
      
      for I := 0 to Languages.Count-1 do
        if SameText(Languages.Name[I], cmbLangID.Text) then
        begin
          FOptions.VersionInfo.LanguageID := Languages.LocaleID[I];
          Break;
        end;
     end;
     
   end;
  result:= fOptions;
end;

procedure TfrmProjectOptions.SetOptions(Value: TProjOptions);
var
  idx, cntSrc, cntHdr, cntRes: integer;
begin
  fOptions:= Value;
  fIcon:= GetRealPath(fOptions.Icon, fProject.Directory);
  if (fIcon <> '') and (FileExists(fIcon)) then
   try
    Icon.Picture.LoadFromFile(fIcon);
   except
    fIcon:= '';
   end;

  // General Tab
  lstType.ItemIndex:= fOptions.typ;
  edCompiler.Lines.Text:= StringReplace(fOptions.cmdlines.Compiler, '_@@_', #13#10, [rfReplaceAll]);
  edCppCompiler.Lines.Text:= StringReplace(fOptions.cmdlines.CppCompiler, '_@@_', #13#10, [rfReplaceAll]);
  edLinker.Lines.Text:= StringReplace(fOptions.cmdlines.Linker, '_@@_', #13#10, [rfReplaceAll]);
  edProjectName.Text:= fProject.Name;
  lblPrjFname.Caption:=fProject.FileName;
  lblPrjOutputFname.Caption:=fProject.Executable;
  cntSrc:=0;
  cntHdr:=0;
  cntRes:=0;
  for idx:=0 to fProject.Units.Count-1 do
    case GetFileTyp(fProject.Units[idx].FileName)of
      utSrc: Inc(cntSrc);
      utHead: Inc(cntHdr);
      utRes: Inc(cntRes);
    end;
  lblPrjUnits.Caption:=Format(Lang[ID_POPT_UNITSFORMAT], [fProject.Units.Count, cntSrc, cntHdr, cntRes]);
  chkSupportXP.Checked:=fOptions.SupportXPThemes;

  // Output tab
  edExeOutput.Text := fOptions.ExeOutput;
  edObjOutput.Text := fOptions.ObjectOutput;
  chkOverrideOutput.Checked := fOptions.OverrideOutput;
  if fOptions.OverridenOutput<>'' then
    edOverridenOutput.Text := ExtractFilename(fOptions.OverridenOutput)
  else
    edOverridenOutput.Text := ExtractFilename(fProject.Executable);
  edOverridenOutput.Enabled:=fOptions.OverrideOutput;

  // Makefile tab
  cbUseCustomMakefile.Checked:=fProject.UseCustomMakefile;
  edCustomMakefile.Text:=fProject.CustomMakefile;
  cbUseCustomMakefileClick(nil);
  MakeIncludes.Items.AddStrings(fOptions.MakeIncludes);

  // Files tab
  cmbCompiler.Items.Assign(devCompilerSet.Sets);
  cmbCompiler.ItemIndex:=fOptions.CompilerSet;

  // Version tab
  InitVersionInfo;
end;

procedure TfrmProjectOptions.SaveDirSettings;
var
 sl: TStrings;
begin
  sl := nil;
  case SubTabs.TabIndex of
   0: sl:= fOptions.Libs;
   1: sl:= fOptions.Includes;
   2: sl:= fOptions.ResourceIncludes;
  end;
  if assigned(sl) then
   begin
     sl.Clear;
     sl.AddStrings(lstList.Items);
   end;
end;

procedure TfrmProjectOptions.SubTabsChanging(Sender: TObject;
  NewIndex: Integer; var AllowChange: Boolean);
begin
  SaveDirSettings;
end;

procedure TfrmProjectOptions.SubTabsChange(Sender: TObject);
begin
  case SubTabs.TabIndex of
   0: lstList.Items:= fOptions.Libs;
   1: lstList.Items:= fOptions.Includes;
   2: lstList.Items:= fOptions.ResourceIncludes;
  end;
  UpdateButtons;
end;

procedure TfrmProjectOptions.FormShow(Sender: TObject);
begin
  PageControl.ActivePageIndex:= 0;
  //CompPages.ActivePageIndex:= 0;
  SubTabs.TabIndex:= 0;
  lvFiles.Images:=MainForm.ProjectView.Images;
  lvFiles.Items.Assign(MainForm.ProjectView.Items);
  lvFiles.Items[0].Expand(False);
  chkCompile.Enabled:=False;
  chkLink.Enabled:=False;
  spnPriority.Enabled:=False;
  chkCompileCpp.Enabled:=False;
  chkOverrideBuildCmd.Enabled:=False;
  txtOverrideBuildCmd.Enabled:=False;
  txtOverrideBuildCmd.Text:='';
  chkSupportXP.Enabled:=fOptions.typ=dptGUI;
  devCompiler.OptionStr:=fOptions.CompilerOptions;
  CompOptionsFrame1.FillOptions(fProject);
  SubTabsChange(Self);
  UpdateMakButtons();
end;

procedure TfrmProjectOptions.btnIconLibClick(Sender: TObject);
begin
  with TIconForm.Create(Self) do
   try
    if ShowModal = mrOk then
     if Selected <> '' then
      fIcon:= Selected;
   finally
    Free;
   end;
  if fIcon <> '' then
  begin
   Icon.Picture.LoadFromFile(fIcon);
   btnRemoveIcon.Enabled := Length(fIcon) > 0;
  end;
end;

procedure TfrmProjectOptions.btnIconBrwseClick(Sender: TObject);
begin
  if dlgPic.Execute then
   begin
     if FileExists(dlgPic.FileName) then begin
       fIcon:= dlgPic.FileName;
       Icon.Picture.LoadFromFile(fIcon);
       btnRemoveIcon.Enabled := Length(fIcon) > 0;
     end
     else
       MessageDlg(format(Lang[ID_MSG_COULDNOTOPENFILE], [dlgPic.FileName]), mtError, [mbOK], 0);
   end;
end;

procedure TfrmProjectOptions.FormCreate(Sender: TObject);
begin
  cmbLangID.Sorted := True;
  LoadText;
end;

procedure TfrmProjectOptions.LoadText;
begin
  if devData.XPTheme then
    XPMenu.Active := true
  else
    XPMenu.Active := false;
  Caption:= Lang[ID_POPT];
  //tabs
  tabGeneral.Caption:=   Lang[ID_POPT_GENTAB];
  tabFilesDir.Caption:=  Lang[ID_POPT_DIRTAB];
  tabCompiler.Caption:=  Lang[ID_SHEET_COMP];
  tabOutputDir.Caption:= Lang[ID_POPT_OUTTAB];
  tabMakefile.Caption := Lang[ID_POPT_MAKTAB];

  //controls (general tab)
  lblPrjName.Caption:=    Lang[ID_POPT_PRJNAME];
  lblFname.Caption:=      Lang[ID_PROPS_FILENAME]+':';
  lblPrjOutput.Caption:=  Lang[ID_POPT_OUTPUTFILENAME]+':';
  lblUnits.Caption:=      Lang[ID_POPT_FILESTAB]+':';
  grpIcon.Caption:=       '  '+Lang[ID_POPT_GRP_ICON] +'  ';
  btnIconLib.Caption:=    Lang[ID_POPT_ICOLIB];
  grpType.Caption:=       '  '+Lang[ID_POPT_GRP_TYPE]+'  ';
  lstType.Clear;
  lstType.Items.Append(Lang[ID_POPT_TYPE1]);
  lstType.Items.Append(Lang[ID_POPT_TYPE2]);
  lstType.Items.Append(Lang[ID_POPT_TYPE3]);
  lstType.Items.Append(Lang[ID_POPT_TYPE4]);
  chkSupportXP.Caption:=      Lang[ID_POPT_SUPPORTXP];

  // compiler tab
  //tabCompSet.Caption:=  Lang[ID_POPT_COMPTAB];
  tabCompOpts.Caption:=  Lang[ID_PARAM_CAPTION];
  lblAdditions.Caption:=  '  '+Lang[ID_POPT_ADDITIONAL]+'  ';
  lblCompiler.Caption:=   Lang[ID_POPT_COMP];
  lblCppCompiler.Caption:=Lang[ID_COPT_GRP_CPP];
  lblLinker.Caption:=     Lang[ID_COPT_LINKERTAB];
  AddLibBtn.Caption:=      Lang[ID_POPT_ADDLIBRARY];

  //dir tab
  SubTabs.Tabs.Clear;
  SubTabs.Tabs.Append(Lang[ID_POPT_LIBDIRS]);
  SubTabs.Tabs.Append(Lang[ID_POPT_INCDIRS]);
  SubTabs.Tabs.Append(Lang[ID_POPT_RESDIRS]);

  //output tab
  grpOutDirectories.Caption:=Lang[ID_POPT_OUTDIRGRP];
  lblExeOutput.Caption:=     Lang[ID_POPT_EXEOUT];
  lblObjOutput.Caption:=     Lang[ID_POPT_OBJOUT];
  chkOverrideOutput.Caption:=Lang[ID_POPT_OVERRIDEOUT];

  //dialogs
  dlgPic.Title:=        Lang[ID_POPT_OPENICO];
  dlgOpen.Title:=       Lang[ID_POPT_OPENOBJ];

  //buttons
  btnReplace.Caption:=    Lang[ID_BTN_REPLACE];
  btnAdd.Caption:=        Lang[ID_BTN_ADD];
  btnDelete.Caption:=     Lang[ID_BTN_DELETE];
  btnDelInval.Caption:=   Lang[ID_BTN_DELINVAL];
  btnOk.Caption:=         Lang[ID_BTN_OK];
  btnCancel.Caption:=     Lang[ID_BTN_CANCEL];
  btnHelp.Caption:=       Lang[ID_BTN_HELP];
  btnIconBrwse.Caption:=  Lang[ID_BTN_BROWSE];
  btnRemoveIcon.Caption:= Lang[ID_BTN_REMOVEICON];

  cbUseCustomMakefile.Caption := Lang[ID_POPT_USECUSTOMMAKEFILE];
  InfoMakeBtn.Caption :=         Lang[ID_POPT_INFOCUSTOMMAKEFILE];
  IncMakeLabel.Caption :=        Lang[ID_POPT_INCFILEMAKEFILE];

  btnMakReplace.Caption:=    Lang[ID_BTN_REPLACE];
  btnMakAdd.Caption:=        Lang[ID_BTN_ADD];
  btnMakDelete.Caption:=     Lang[ID_BTN_DELETE];
  btnMakDelInval.Caption:=   Lang[ID_BTN_DELINVAL];

  // files tab
  tabFiles.Caption:=          Lang[ID_POPT_FILESTAB];
  lblProjectFiles.Caption:=   Lang[ID_POPT_PROJFILES];
  lblCompilerSet.Caption:=    Lang[ID_POPT_COMP];
  lblCompileInfo.Caption:=    Lang[ID_POPT_COMPINFO];
  grpUnitOptions.Caption:=    Lang[ID_POPT_UNITOPTS];
  lblPriority.Caption:=       Lang[ID_POPT_BUILDPRIORITY];
  chkCompile.Caption:=        Lang[ID_POPT_COMPUNIT];
  chkCompileCpp.Caption:=     Lang[ID_POPT_UNITUSEGPP];
  chkOverrideBuildCmd.Caption:=Lang[ID_POPT_OVERRIDEBUILDCMD];
  chkLink.Caption:=           Lang[ID_POPT_LINKUNIT];

  // version info tab
  tabVersion.Caption:=        Lang[ID_POPT_VERTAB];
  chkVersionInfo.Caption:=    Lang[ID_POPT_INCLUDEVERSION];
  grpVersion.Caption:=        Lang[ID_POPT_VDETAILS];
  lblVerMajor.Caption:=       Lang[ID_POPT_VMAJOR];
  lblVerMinor.Caption:=       Lang[ID_POPT_VMINOR];
  lblVerRel.Caption:=         Lang[ID_POPT_VRELEASE];
  lblVerBuild.Caption:=       Lang[ID_POPT_VBUILD];
  lblVerLang.Caption:=        Lang[ID_POPT_VLANG];
  lblVerAdditional.Caption:=  Lang[ID_POPT_VADDITIONAL];
  chkAutoIncBuild.Caption:=   Lang[ID_POPT_VAUTOINCBUILDNR];
end;

procedure TfrmProjectOptions.btnRemoveIconClick(Sender: TObject);
begin
  btnRemoveIcon.Enabled := False;
  fIcon := '';
  Icon.Picture.Graphic := nil;
end;

procedure TfrmProjectOptions.BrowseExecutableOutDirClick(Sender: TObject);
var
{$IFDEF WIN32}
  Dir: String;
{$ENDIF}
{$IFDEF LINUX}
  Dir: WideString;
{$ENDIF}
begin
  if fProject.Options.ExeOutput<>'' then
    Dir:=ExpandFileto(fProject.Options.ExeOutput, fProject.Directory)
  else
    Dir:=fProject.Directory;
  SelectDirectory('Select Directory', '', Dir);
  edExeOutput.Text := ExtractRelativePath(fProject.Directory, Dir);
end;

procedure TfrmProjectOptions.BrowseObjDirClick(Sender: TObject);
var
{$IFDEF WIN32}
  Dir: String;
{$ENDIF}
{$IFDEF LINUX}
  Dir: WideString;
{$ENDIF}
begin
  if fProject.Options.ObjectOutput<>'' then
    Dir:=ExpandFileto(fProject.Options.ObjectOutput, fProject.Directory)
  else
    Dir:=fProject.Directory;
  SelectDirectory('Select Directory', '', Dir);
  edObjOutput.Text := ExtractRelativePath(fProject.Directory, Dir);
end;

procedure TfrmProjectOptions.btnMakeBrowseClick(Sender: TObject);
begin
  if dlgMakeInclude.Execute then
      edMakeInclude.Text := ExtractRelativePath(fProject.FileName,
        dlgMakeInclude.FileName);
  edMakeInclude.SetFocus;
end;

procedure TfrmProjectOptions.btnMakClick(Sender: TObject);
var
  i: Integer;
begin
  i := MakeIncludes.ItemIndex;
  if i < 0 then
    exit;
  if Sender = btnMakUp then
   begin
     MakeIncludes.Items.Exchange(MakeIncludes.ItemIndex, MakeIncludes.ItemIndex -1);
     MakeIncludes.ItemIndex:= i -1;
   end
  else
   if Sender = btnMakDown then
    begin
      MakeIncludes.Items.Exchange(MakeIncludes.ItemIndex, MakeIncludes.ItemIndex +1);
      MakeIncludes.ItemIndex:= i+1;
    end;

  UpdateMakButtons;
end;

procedure TfrmProjectOptions.MakButtonClick(Sender: TObject);
var
  i: Integer;
begin
  case (Sender as TComponent).Tag of
   1: begin
        MakeIncludes.Items[MakeIncludes.ItemIndex] := (edMakeInclude.Text);
      end;
   2: begin
        MakeIncludes.Items.Add(edMakeInclude.Text);
      end;
   3: begin
        MakeIncludes.DeleteSelected;
       end;
   4: begin
          i := 0;
          while i < MakeIncludes.Items.Count do begin
             if not FileExists(MakeIncludes.Items[i]) then
             begin
                    MakeIncludes.Items.Delete(i);
                    i := -1;
             end;
             i := i + 1;
          end;
      end;
  end;
  edMakeInclude.Clear;
  UpdateMakButtons;
end;

procedure TfrmProjectOptions.edMakeIncludeChange(Sender: TObject);
begin
  UpdateMakButtons;
end;

procedure TfrmProjectOptions.MakeIncludesClick(Sender: TObject);
begin
  UpdateMakButtons;
  edMakeInclude.Text := MakeIncludes.Items[MakeIncludes.Itemindex];
end;

procedure TfrmProjectOptions.InfoMakeBtnClick(Sender: TObject);
begin
    Application.MessageBox(
      'Dev-C++''s Makefile has two important targets:' + #13#10 +
      '- all (which builds the executable)' + #13#10 +
      '- clean (which cleans up object files)' + #13#10 + #13#10 +
      '''all'' depends on 2 targets: all-before and all-after. All-before' + #13#10 +
      'gets called before the compilation process, and all-after gets' + #13#10 +
      'called after the compilation process.' + #13#10 +
      '''clean'' depends on the target clean-custom, which gets called' + #13#10 +
      'before the cleaning process.' + #13#10 + #13#10 +
      'You can change the Makefile''s behavior by defining the targets' + #13#10 +
      'that ''all'' and ''clean'' depend on.',
{$IFDEF WIN32}
      'Information', MB_ICONINFORMATION);
{$ENDIF}
{$IFDEF LINUX}
      'Information', [smbOK], smsInformation);
{$ENDIF}
end;

procedure TfrmProjectOptions.btnHelpClick(Sender: TObject);
begin
  Application.HelpFile := IncludeTrailingBackslash(devDirs.Help) + DEV_MAINHELP_FILE;
  Application.HelpJump('ID_MANAGEPROJECT');
end;

procedure TfrmProjectOptions.chkOverrideOutputClick(Sender: TObject);
begin
  edOverridenOutput.Enabled:=chkOverrideOutput.Checked;
end;

procedure TfrmProjectOptions.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  // check for disallowed characters in filename
  if (Pos('/', edOverridenOutput.Text)>0) or
     (Pos('\', edOverridenOutput.Text)>0) or
     (Pos(':', edOverridenOutput.Text)>0) or
     (Pos('*', edOverridenOutput.Text)>0) or
     (Pos('?', edOverridenOutput.Text)>0) or
     (Pos('"', edOverridenOutput.Text)>0) or
     (Pos('<', edOverridenOutput.Text)>0) or
     (Pos('>', edOverridenOutput.Text)>0) or
     (Pos('|', edOverridenOutput.Text)>0) then begin
     MessageDlg('The output filename you have defined, contains at least one '+
       'of the following illegal characters:'#10#10+
       '\ / : * ? " > < |'#10#10+
       'Please correct this...', mtError, [mbOk], 0);
     CanClose:=False;
  end;
  if CanClose then begin
    fOptions.CompilerOptions:=devCompiler.OptionStr;
    devCompilerSet.LoadSet(devCompiler.CompilerSet);
    devCompilerSet.AssignToCompiler;
  end;
end;

procedure TfrmProjectOptions.lvFilesChange(Sender: TObject;
  Node: TTreeNode);
var
  idx: integer;
begin
  if not Assigned(Node) then begin
    chkCompile.Enabled:=False;
    chkCompileCpp.Enabled:=False;
    chkLink.Enabled:=False;
    chkOverrideBuildCmd.Enabled:=False;
    txtOverrideBuildCmd.Enabled:=False;
    spnPriority.Enabled:=False;
    Exit;
  end;

  // disable events
  chkCompile.OnClick:=nil;
  chkCompileCpp.OnClick:=nil;
  chkLink.OnClick:=nil;
  chkOverrideBuildCmd.OnClick:=nil;
  txtOverrideBuildCmd.OnChange:=nil;
  spnPriority.OnChange:=nil;

  idx:=Integer(Node.Data);
  if (Node.Level>0) and (idx<>-1) then begin // unit
    if fProject.Units[idx].OverrideBuildCmd then
      txtOverrideBuildCmd.Text:=StringReplace(fProject.Units[idx].BuildCmd, '<CRTAB>', #13#10, [rfReplaceAll])
    else
      txtOverrideBuildCmd.Text := DefaultBuildCommand(idx);
    chkOverrideBuildCmd.Checked:=fProject.Units[idx].OverrideBuildCmd;

    chkCompile.Enabled:=GetFileTyp(fProject.Units[idx].FileName) <> utHead;
    chkCompile.Checked:=fProject.Units[idx].Compile;
    chkCompileCpp.Enabled:=chkCompile.Checked and (GetFileTyp(fProject.Units[idx].FileName) in [utSrc]);
    chkCompileCpp.Checked:=fProject.Units[idx].CompileCpp;
    chkLink.Enabled:=chkCompile.Enabled and (GetFileTyp(fProject.Units[idx].FileName) <> utRes);
    chkLink.Checked:=fProject.Units[idx].Link;
    spnPriority.Enabled:=chkCompile.Checked and chkCompile.Enabled;
    spnPriority.Value:=fProject.Units[idx].Priority;
    chkOverrideBuildCmd.Enabled:=chkCompile.Checked and (lvFiles.SelectionCount=1) and not (GetFileTyp(fProject.Units[idx].FileName) in [utHead, utRes]);
    txtOverrideBuildCmd.Enabled:=chkOverrideBuildCmd.Enabled and chkOverrideBuildCmd.Checked;
  end
  else begin
    chkCompile.Enabled:=False;
    chkCompileCpp.Enabled:=False;
    chkLink.Enabled:=False;
    chkOverrideBuildCmd.Enabled:=False;
    txtOverrideBuildCmd.Enabled:=False;
    spnPriority.Enabled:=False;
  end;

  // enable events
  chkCompile.OnClick:=chkCompileClick;
  chkCompileCpp.OnClick:=chkCompileClick;
  chkLink.OnClick:=chkCompileClick;
  chkOverrideBuildCmd.OnClick:=chkCompileClick;
  txtOverrideBuildCmd.OnChange:=txtOverrideBuildCmdChange;
  spnPriority.OnChange:=spnPriorityChange;
end;

procedure TfrmProjectOptions.chkCompileClick(Sender: TObject);
  procedure DoNode(Node: TTreeNode);
  var
    I: integer;
    idx: integer;
  begin
    for I:=0 to Node.Count-1 do begin
      idx:=Integer(Node[I].Data);
      if idx<>-1 then begin // unit
        fProject.Units[idx].Compile:=chkCompile.Checked;
        fProject.Units[idx].CompileCpp:=chkCompileCpp.Checked;
      end
      else if Node[I].HasChildren then
        DoNode(Node[I]);
    end;
  end;
var
  I: integer;
  idx: integer;
begin
  for I:=0 to lvFiles.SelectionCount-1 do begin
    idx:=Integer(lvFiles.Selections[I].Data);
    if idx<>-1 then begin // unit
      fProject.Units[idx].Compile:=chkCompile.Checked;
      fProject.Units[idx].CompileCpp:=chkCompileCpp.Checked;
      fProject.Units[idx].Link:=chkLink.Checked;
      if lvFiles.SelectionCount=1 then begin
        fProject.Units[idx].OverrideBuildCmd:=chkOverrideBuildCmd.Checked;

        txtOverrideBuildCmd.OnChange:=nil;
        txtOverrideBuildCmd.Text:=StringReplace(txtOverrideBuildCmd.Text, '<CRTAB>', #13#10, [rfReplaceAll]);

        spnPriority.Enabled:=chkCompile.Checked;
        chkOverrideBuildCmd.Enabled:=chkCompile.Checked and (GetFileTyp(fProject.Units[idx].FileName) <> utRes);
        if chkCompile.Checked and (GetFileTyp(fProject.Units[idx].FileName)=utOther) then begin
          // non-standard source files, *must* override the build command
          chkCompileCpp.Enabled:=False;
          txtOverrideBuildCmd.Enabled:=True;
          chkOverrideBuildCmd.Checked:=True;
          if txtOverrideBuildCmd.Text='' then
            txtOverrideBuildCmd.Text:='<override this command>';
        end
        else begin
          chkCompileCpp.Enabled:=chkCompile.Checked and (GetFileTyp(fProject.Units[idx].FileName) <> utRes);
          if chkCompileCpp.Checked then begin
            txtOverrideBuildCmd.Text:=StringReplace(txtOverrideBuildCmd.Text, '$(CC)', '$(CPP)', [rfReplaceAll]);
            txtOverrideBuildCmd.Text:=StringReplace(txtOverrideBuildCmd.Text, '$(CFLAGS)', '$(CXXFLAGS)', [rfReplaceAll]);
          end
          else begin
            txtOverrideBuildCmd.Text:=StringReplace(txtOverrideBuildCmd.Text, '$(CPP)', '$(CC)', [rfReplaceAll]);
            txtOverrideBuildCmd.Text:=StringReplace(txtOverrideBuildCmd.Text, '$(CXXFLAGS)', '$(CFLAGS)', [rfReplaceAll]);
          end;
          txtOverrideBuildCmd.Enabled:=chkOverrideBuildCmd.Enabled and chkOverrideBuildCmd.Checked;
        end;
        fProject.Units[idx].BuildCmd:=txtOverrideBuildCmd.Text;
        txtOverrideBuildCmd.OnChange:=txtOverrideBuildCmdChange;
      end;
    end
    else if lvFiles.Selections[I].HasChildren then
      DoNode(lvFiles.Selections[I]);
  end;
end;

procedure TfrmProjectOptions.InitVersionInfo;
var
  I: integer;
  S: string;
begin
  chkVersionInfo.Checked:=fOptions.IncludeVersionInfo;
  chkVersionInfoClick(nil);

  spnMajor.Value:=fOptions.VersionInfo.Major;
  spnMinor.Value:=fOptions.VersionInfo.Minor;
  spnRelease.Value:=fOptions.VersionInfo.Release;
  spnBuild.Value:=fOptions.VersionInfo.Build;
  chkAutoIncBuild.Checked:=fOptions.VersionInfo.AutoIncBuildNr;

  vleVersion.Strings.Clear;
  vleVersion.InsertRow('File Description',  fOptions.VersionInfo.FileDescription,   True);
  vleVersion.InsertRow('File Version',      fOptions.VersionInfo.FileVersion,       True);
  vleVersion.InsertRow('Product Name',      fOptions.VersionInfo.ProductName,       True);
  vleVersion.InsertRow('Product Version',   fOptions.VersionInfo.ProductVersion,    True);
  vleVersion.InsertRow('Original Filename', fOptions.VersionInfo.OriginalFilename,  True);
  vleVersion.InsertRow('Internal Name',     fOptions.VersionInfo.InternalName,      True);
  vleVersion.InsertRow('Company Name',      fOptions.VersionInfo.CompanyName,       True);
  vleVersion.InsertRow('Legal Copyright',   fOptions.VersionInfo.LegalCopyright,    True);
  vleVersion.InsertRow('Legal Trademarks',  fOptions.VersionInfo.LegalTrademarks,   True);

  cmbLangID.Items.Clear;
  for I:=0 to Languages.Count-1 do
    cmbLangID.Items.Add(Languages.Name[I]);
  if Languages.Count > fOptions.VersionInfo.LanguageID then begin
    S := Languages.NameFromLocaleID[fOptions.VersionInfo.LanguageID];
    cmbLangID.ItemIndex := cmbLangID.Items.IndexOf(S);
  end;
end;

procedure TfrmProjectOptions.chkVersionInfoClick(Sender: TObject);
begin
  spnMajor.Enabled:=chkVersionInfo.Checked;
  spnMinor.Enabled:=chkVersionInfo.Checked;
  spnRelease.Enabled:=chkVersionInfo.Checked;
  spnBuild.Enabled:=chkVersionInfo.Checked;
  cmbLangID.Enabled:=chkVersionInfo.Checked;
  vleVersion.Enabled:=chkVersionInfo.Checked;
  chkAutoIncBuild.Enabled:=chkVersionInfo.Checked;
end;

procedure TfrmProjectOptions.cmbCompilerChange(Sender: TObject);
begin
  devCompilerSet.LoadSet(cmbCompiler.ItemIndex);
  devCompilerSet.AssignToCompiler;
  CompOptionsFrame1.FillOptions(fProject);
end;

procedure TfrmProjectOptions.btnCancelClick(Sender: TObject);
begin
  devCompilerSet.LoadSet(fOptions.CompilerSet);
  devCompilerSet.AssignToCompiler;
end;

procedure TfrmProjectOptions.lstTypeClick(Sender: TObject);
begin
  chkSupportXP.Enabled:=lstType.ItemIndex=0;
end;

procedure TfrmProjectOptions.btnOkClick(Sender: TObject);
begin
  SaveDirSettings;
  fOptions.CompilerOptions:=devCompiler.OptionStr;
end;

procedure TfrmProjectOptions.AddLibBtnClick(Sender: TObject);
var
  s: string;
  i: integer;
begin
  if OpenLibDialog.Execute then begin
    for i := 0 to OpenLibDialog.Files.Count - 1 do begin
      S:=ExtractRelativePath(fProject.Directory, OpenLibDialog.Files[i]);
      S:=GenMakePath(S);
      edLinker.Lines.Add(S);
    end;
  end;
end;

function TfrmProjectOptions.DefaultBuildCommand(idx: integer): string;
var
  tfile, ofile: string;
begin
  Result:='';
  if GetFileTyp(fProject.Units[idx].FileName)<>utSrc then
    Exit;

  tfile:= ExtractFileName(fProject.Units[idx].FileName);
  if fProject.Options.ObjectOutput<>'' then
  begin
    ofile := IncludeTrailingPathDelimiter(fProject.Options.ObjectOutput)+ExtractFileName(tfile);
    ofile := GenMakePath(ExtractRelativePath(fProject.FileName, ChangeFileExt(ofile, OBJ_EXT)));
  end
  else
    ofile := GenMakePath(ChangeFileExt(tfile, OBJ_EXT));
  if fProject.Units[idx].CompileCpp then
    Result := '$(CPP) -c '+GenMakePath(tfile)+' -o '+ ofile+' $(CXXFLAGS)'
  else
    Result := '$(CC) -c '+GenMakePath(tfile)+' -o '+ ofile+' $(CFLAGS)';
end;

procedure TfrmProjectOptions.txtOverrideBuildCmdChange(Sender: TObject);
var
  idx: integer;
begin
  if not Assigned(lvFiles.Selected) or not txtOverrideBuildCmd.Enabled then
    Exit;
  idx:=Integer(lvFiles.Selected.Data);
  if (lvFiles.Selected.Level>0) and (idx<>-1) then // unit
    fProject.Units[idx].BuildCmd:=StringReplace(txtOverrideBuildCmd.Text, #13#10, '<CRTAB>', [rfReplaceAll]);
end;

procedure TfrmProjectOptions.spnPriorityChange(Sender: TObject);
var
  I, idx: integer;
begin
  if not Assigned(lvFiles.Selected) or not spnPriority.Enabled then
    Exit;
  for I:=0 to lvFiles.SelectionCount-1 do begin
    idx:=Integer(lvFiles.Selections[I].Data);
    if (lvFiles.Selections[I].Level>0) and (idx<>-1) then // unit
      fProject.Units[idx].Priority:=spnPriority.Value;
  end;
end;

procedure TfrmProjectOptions.btnCustomMakeBrowseClick(Sender: TObject);
begin
  if dlgCustomMake.Execute then
      edCustomMakefile.Text := ExtractRelativePath(fProject.FileName, dlgCustomMake.FileName);
  edCustomMakefile.SetFocus;
end;

procedure TfrmProjectOptions.cbUseCustomMakefileClick(Sender: TObject);
  procedure ColorDisabled(W: TWinControl);
  begin
    if not W.Enabled then
      W.Brush.Color:=clBtnFace
    else
      W.Brush.Color:=clWindow;
    W.Repaint;
  end;
begin
  edCustomMakefile.Enabled:=cbUseCustomMakefile.Checked;
  btnCustomMakeBrowse.Enabled:=cbUseCustomMakefile.Checked;
  InfoMakeBtn.Enabled:=not cbUseCustomMakefile.Checked;
  MakeIncludes.Enabled:=not cbUseCustomMakefile.Checked;
  edMakeInclude.Enabled:=not cbUseCustomMakefile.Checked;
  btnMakUp.Enabled:=not cbUseCustomMakefile.Checked;
  btnMakDown.Enabled:=not cbUseCustomMakefile.Checked;
  btnMakeBrowse.Enabled:=not cbUseCustomMakefile.Checked;
  btnMakReplace.Enabled:=not cbUseCustomMakefile.Checked;
  btnMakAdd.Enabled:=not cbUseCustomMakefile.Checked;
  btnMakDelete.Enabled:=not cbUseCustomMakefile.Checked;
  btnMakDelInval.Enabled:=not cbUseCustomMakefile.Checked;

  // I want the disabled controls to be *shown* as disabled...
  ColorDisabled(edCustomMakefile);
  ColorDisabled(edMakeInclude);
  ColorDisabled(MakeIncludes);

  UpdateMakButtons();
end;

procedure TfrmProjectOptions.MakeIncludesDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  btnMakUp.Enabled := MakeIncludes.Items.Count > 0;
  btnMakDown.Enabled := MakeIncludes.Items.Count > 0;
end;

end.
