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
  Buttons, StdCtrls, Inifiles, ExtCtrls, ComCtrls, devTabs, Spin, XPMenu,
  CompilerOptionsFrame;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Classes, QGraphics, QControls, QForms, QDialogs,
  QButtons, QStdCtrls, Inifiles, QExtCtrls, QComCtrls, devTabs,
  CompilerOptionsFrame;
{$ENDIF}

type
  TCompForm = class(TForm)
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    btnDefault: TBitBtn;
    btnHelp: TBitBtn;
    MainPages: TdevPages;
    tabDirectories: TdevPage;
    tabCompiler: TdevPage;
    tabCodeGen: TdevPage;
    DirTabs: TdevTabs;
    btnUp: TSpeedButton;
    btnDown: TSpeedButton;
    lstDirs: TListBox;
    edEntry: TEdit;
    btnDelInval: TButton;
    btnDelete: TButton;
    btnAdd: TButton;
    btnReplace: TButton;
    tabPrograms: TdevPage;
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
    XPMenu: TXPMenu;
    cbCompAdd: TCheckBox;
    Commands: TMemo;
    lblDelay: TLabel;
    seCompDelay: TSpinEdit;
    lblDelayMsg: TLabel;
    grpMakefileGen: TGroupBox;
    cbFastDep: TCheckBox;
    cbLinkerAdd: TCheckBox;
    Linker: TMemo;
    CompOptionsFrame1: TCompOptionsFrame;
    grpCompSet: TGroupBox;
    cmbCompilerSetComp: TComboBox;
    btnAddCompilerSet: TSpeedButton;
    btnDelCompilerSet: TSpeedButton;
    btnRenameCompilerSet: TSpeedButton;
    btnBrowse: TSpeedButton;
    btnBrws2: TSpeedButton;
    btnBrowse3: TSpeedButton;
    btnBrowse4: TSpeedButton;
    btnBrowse5: TSpeedButton;
    btnBrowse6: TSpeedButton;
    btnBrowse7: TSpeedButton;
    btnBrowse8: TSpeedButton;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btnDefaultClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure DirTabsChange(Sender: TObject);
    procedure DirTabsChanging(Sender: TObject; NewIndex: Integer;
      var AllowChange: Boolean);
    procedure lstDirsClick(Sender: TObject);
    procedure lstDirsDblClick(Sender: TObject);
    procedure edEntryChange(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
    procedure UpDownClick(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure edEntryKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
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
   private
    fBins: string;
    fLibs: string;
    fC: string;
    fCpp: string;
    procedure SetOptions;
    procedure UpdateButtons;
    procedure LoadText;
  end;

var
  NumOpt : integer;
  currentSet: integer;

implementation

uses 
{$IFDEF WIN32}
  Main, FileCtrl, version, devcfg, utils, MultiLangSupport, datamod;
{$ENDIF}
{$IFDEF LINUX}
  Xlib, Main, version, devcfg, utils, MultiLangSupport, datamod;
{$ENDIF}

{$R *.dfm}

const
 Help_Topics: array[0..3] of string = (
  'CompOpt_Directories',
  'CompOpt_Compiler',
  'CompOpt_CodeGen',
  'CompOpt_Linker');

procedure TCompForm.btnCancelClick(Sender: TObject);
begin
     devCompiler.CompilerSet:=cmbCompilerSetComp.ItemIndex;
     Close;
end;

procedure TCompForm.btnOkClick(Sender: TObject);
begin
  if cbCompAdd.Checked and (Commands.Text = '') then
   begin
     MessageDlg('You have selected the "Add commands" option but no commands have been specified.', MtError,[MbOK],0);
     cbCompAdd.Checked := False;
   end;

  if (fBins = '') then
   begin
     MessageDlg('You have not indicated the location of your binaries (compiler).'#13' Please do so now.', mtWarning, [mbOK], 0);
     ModalResult := mrNone;
     exit;
   end;

   //RNC
   devCompilerSet.CmdOpts:= Commands.Lines.Text;
   devCompilerSet.AddtoLink:= cbLinkerAdd.Checked;
   devCompilerSet.AddtoComp:= cbCompAdd.Checked;
   devCompilerSet.LinkOpts:= Linker.Lines.Text;
   devCompilerSet.SaveSettings;
   devCompilerSet.OptionsStr:=devCompiler.OptionStr;
   devCompilerSet.SaveSet(currentSet);
   devCompilerSet.SaveSettings;

  with devCompiler do
   begin
     Delay:= seCompDelay.Value;
     FastDep:= cbFastDep.Checked;




     CompilerSet:=cmbCompilerSetComp.ItemIndex;
     devCompilerSet.Sets.Assign(cmbCompilerSetComp.Items);

     gccName:=devCompilerSet.gccName;
     gppName:=devCompilerSet.gppName;
     makeName:=devCompilerSet.makeName;
     gdbName:=devCompilerSet.gdbName;
     windresName:=devCompilerSet.windresName;
     dllwrapName:=devCompilerSet.dllwrapName;
     gprofName:=devCompilerSet.gprofName;
   end;



  with devDirs do
   begin
     Bins:= fBins;
     C:= fC;
     Cpp:= fCpp;
     Lib:= fLibs;
   end;
  // Set Path with New Bins
  SetPath(fBins);

  devDirs.SaveSettings;
  devCompiler.SaveSettings;
end;

procedure TCompForm.FormActivate(Sender: TObject);
begin
  SetOptions;
  DirTabsChange(Self);
end;

procedure TCompForm.SetOptions;
begin
  with devCompiler do
   begin
     seCompDelay.Value:= Delay;
     cbFastDep.Checked:= FastDep;
//RNC
     //Commands.Lines.Text:= CmdOpts;
     Commands.Lines.Text:= devCompilerSet.CmdOpts;
     cbCompAdd.Checked:= devCompilerSet.AddtoComp;
    // cbCompAdd.Checked:= AddToComp;

     //Linker.Lines.Text:= LinkOpts;
     Linker.Lines.Text:= devCompilerSet.LinkOpts;
     cbLinkerAdd.Checked:= devCompilerSet.AddtoLink;
    // cbLinkerAdd.Checked:= AddtoLink;


     cmbCompilerSetComp.Items.Clear;
     cmbCompilerSetComp.Items.Assign(devCompilerSet.Sets);

     if CompilerSet < cmbCompilerSetComp.Items.Count then
       cmbCompilerSetComp.ItemIndex:=CompilerSet
     else if cmbCompilerSetComp.Items.Count>0 then
      cmbCompilerSetComp.ItemIndex:=0;
     currentSet:=cmbCompilerSetComp.ItemIndex;
     devCompilerSet.LoadSet(CompilerSet);
     cmbCompilerSetCompChange(nil);
   end;
end;

procedure TCompForm.btnDefaultClick(Sender: TObject);
begin
  devCompiler.SettoDefaults;
  SetOptions;
end;

procedure TCompForm.btnHelpClick(Sender: TObject);
begin
  HelpFile:= devDirs.Help +DEV_MAINHELP_FILE;
  // ** temporary removal ** Application.HelpJump(HelpKeyword);
  Application.HelpJump('ID_COMPILEROPTIONS');
end;

procedure TCompForm.DirTabsChange(Sender: TObject);
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

procedure TCompForm.DirTabsChanging(Sender: TObject; NewIndex: Integer;
  var AllowChange: Boolean);
begin
  UpdateButtons;
end;

procedure TCompForm.lstDirsClick(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TCompForm.lstDirsDblClick(Sender: TObject);
begin
  edEntry.Text:= lstDirs.Items[lstDirs.ItemIndex];
end;

procedure TCompForm.edEntryChange(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TCompForm.btnBrowseClick(Sender: TObject);
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
  if SelectDirectory('', '', NewItem) then
   edEntry.Text:= NewItem;
end;

procedure TCompForm.ButtonClick(Sender: TObject);
var
 idx: integer;
begin
  case (Sender as TComponent).Tag of
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

  devCompilerSet.BinDir:=fBins;
  devCompilerSet.CDir:=fC;
  devCompilerSet.CppDir:=fCpp;
  devCompilerSet.LibDir:=fLibs;
end;

procedure TCompForm.UpDownClick(Sender: TObject);
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

  devCompilerSet.BinDir:=fBins;
  devCompilerSet.CDir:=fC;
  devCompilerSet.CppDir:=fCpp;
  devCompilerSet.LibDir:=fLibs;

  UpdateButtons;
end;

procedure TCompForm.UpdateButtons;
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

procedure TCompForm.PageControlChange(Sender: TObject);
begin
  HelpKeyword:= Help_Topics[MainPages.ActivePageIndex];
end;

procedure TCompForm.edEntryKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
{$IFDEF WIN32}
  if key = vk_return then ButtonClick(btnAdd);
{$ENDIF}
{$IFDEF LINUX}
  if key = XK_RETURN then ButtonClick(btnAdd);
{$ENDIF}
end;

procedure TCompForm.FormCreate(Sender: TObject);
begin
  LoadText;
  CompOptionsFrame1.FillOptions(nil);
  MainPages.ActivePageIndex:= 0;
  DirTabs.TabIndex:= 0;
end;

procedure TCompForm.LoadText;
begin
  if devData.XPTheme then
    XPMenu.Active := true
  else
    XPMenu.Active := false;
  Caption:=                            Lang[ID_COPT];
  //tabs
  tabCompiler.Caption:=                Lang[ID_COPT_COMPTAB];
  tabDirectories.Caption:=             Lang[ID_COPT_DIRTAB];
  tabCodeGen.Caption:=                 Lang[ID_COPT_CODEGENTAB];
  tabPrograms.Caption:=                Lang[ID_COPT_PROGRAMSTAB];

  // subtabs
  DirTabs.Tabs.Clear;
  DirTabs.Tabs.Append(Lang[ID_COPT_BIN]);
  DirTabs.Tabs.Append(Lang[ID_COPT_LIB]);
  DirTabs.Tabs.Append(Lang[ID_COPT_INCC]);
  DirTabs.Tabs.Append(Lang[ID_COPT_INCCPP]);

  //buttons
  btnReplace.Caption:=                 Lang[ID_BTN_REPLACE];
  btnAdd.Caption:=                     Lang[ID_BTN_ADD];
  btnDelete.Caption:=                  Lang[ID_BTN_DELETE];
  btnDelInval.Caption:=                Lang[ID_BTN_DELINVAL];
  btnOk.Caption:=                      Lang[ID_BTN_OK];
  btnCancel.Caption:=                  Lang[ID_BTN_CANCEL];
  btnHelp.Caption:=                    Lang[ID_BTN_HELP];
  btnDefault.Caption:=                 Lang[ID_BTN_DEFAULT];

  //controls (compiler tab)
  cbCompAdd.Caption:=                  Lang[ID_COPT_ADDCOMP];
  lblDelay.Caption:=                   Lang[ID_COPT_DELAY];
  lblDelayMsg.Caption:=                Lang[ID_COPT_DELAYMSG];
  cbLinkerAdd.Caption:=                Lang[ID_COPT_LINKADD];

  //controls (code gen tab)
  grpMakefileGen.Caption:=             '  ' +Lang[ID_COPT_MAKEFILEGEN] +'  ';
  cbFastDep.Caption:=                  Lang[ID_COPT_FASTDEP];

  // conrols (Programs tab)
  lblProgramsText.Caption:=            Lang[ID_COPT_PROGRAMS];

  grpCompSet.Caption:=                Lang[ID_COPT_COMPSETS];
end;

procedure TCompForm.cmbCompilerSetCompChange(Sender: TObject);
begin
  devCompilerSet.OptionsStr:=devCompiler.OptionStr;
  devCompilerSet.CmdOpts:=Commands.Lines.Text;
  devCompilerSet.LinkOpts:=Linker.Lines.Text;

  devCompilerSet.AddtoLink:=cbLinkerAdd.Checked;
  devCompilerSet.AddtoComp:=cbCompAdd.Checked;

  devCompilerSet.SaveSet(currentSet);
  devCompilerSet.LoadSet(cmbCompilerSetComp.ItemIndex);
  currentSet:=cmbCompilerSetComp.ItemIndex;


  with devCompilerSet do begin
    fBins:=BinDir;
    fC:=CDir;
    fCpp:=CppDir;
    fLibs:=LibDir;
    Commands.Lines.Text:= CmdOpts;
    Linker.Lines.Text:= LinkOpts;
    cbCompAdd.Checked:=AddtoComp;
    cbLinkerAdd.Checked:=AddtoLink;

  end;
  DirTabsChange(DirTabs);

  with devCompilerSet do begin
     GccEdit.Text := gccName;
     GppEdit.Text := gppName;
     GdbEdit.Text := gdbName;
     MakeEdit.Text := makeName;
     WindresEdit.Text := windresName;
     DllwrapEdit.Text := dllwrapName;
     GprofEdit.Text := gprofName;

     devCompiler.OptionStr:=OptionsStr;
     CompOptionsFrame1.FillOptions(nil);
  end;
end;

procedure TCompForm.btnBrws1Click(Sender: TObject);
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
    sl.DelimitedText:=devCompilerSet.BinDir;
    if sl.Count>0 then
      dmMain.OpenDialog.InitialDir:=sl[0];
  finally
    sl.Free;
  end;

  dmMain.OpenDialog.FileName:=IncludeTrailingPathDelimiter(dmMain.OpenDialog.InitialDir)+Obj.Text;
  if dmMain.OpenDialog.Execute then begin
    Obj.Text:=ExtractFileName(dmMain.OpenDialog.FileName);
    devCompilerSet.gccName:=GccEdit.Text;
    devCompilerSet.gppName:=GppEdit.Text;
    devCompilerSet.gdbName:=GdbEdit.Text;
    devCompilerSet.makeName:=MakeEdit.Text;
    devCompilerSet.windresName:=WindresEdit.Text;
    devCompilerSet.dllwrapName:=DllwrapEdit.Text;
    devCompilerSet.gprofName:=GprofEdit.Text;
  end;
end;

procedure TCompForm.btnAddCompilerSetClick(Sender: TObject);
var
  S: string;
begin
  S:='New compiler';
  if not InputQuery(Lang[ID_COPT_NEWCOMPSET], Lang[ID_COPT_PROMPTNEWCOMPSET], S) or (S='') then
    Exit;

  cmbCompilerSetComp.ItemIndex:=cmbCompilerSetComp.Items.Add(S);
  cmbCompilerSetCompChange(nil);
  devCompilerSet.Sets.Add(S);
end;

procedure TCompForm.btnDelCompilerSetClick(Sender: TObject);
begin
  if cmbCompilerSetComp.Items.Count=1 then begin
    MessageDlg(Lang[ID_COPT_CANTDELETECOMPSET], mtError, [mbOk], 0);
    Exit;
  end;

  if MessageDlg(Lang[ID_COPT_DELETECOMPSET], mtConfirmation, [mbYes, mbNo], 0)=mrNo then
    Exit;

  devCompilerSet.Sets.Delete(cmbCompilerSetComp.ItemIndex);
  cmbCompilerSetComp.Items.Delete(cmbCompilerSetComp.ItemIndex);
  cmbCompilerSetComp.ItemIndex:=0;
  cmbCompilerSetCompChange(nil);
end;

procedure TCompForm.btnRenameCompilerSetClick(Sender: TObject);
var
  S: string;
begin
  S:=cmbCompilerSetComp.Text;
  if not InputQuery(Lang[ID_COPT_RENAMECOMPSET], Lang[ID_COPT_PROMPTRENAMECOMPSET], S) or (S='') or (S=cmbCompilerSetComp.Text) then
    Exit;

  cmbCompilerSetComp.Items[cmbCompilerSetComp.ItemIndex]:=S;
  cmbCompilerSetComp.ItemIndex:=cmbCompilerSetComp.Items.IndexOf(S);
end;

procedure TCompForm.GccEditChange(Sender: TObject);
begin
  devCompilerSet.gccName := GccEdit.Text;
end;

procedure TCompForm.GppEditChange(Sender: TObject);
begin
  devCompilerSet.gppName := GppEdit.Text;
end;

procedure TCompForm.MakeEditChange(Sender: TObject);
begin
  devCompilerSet.makeName := MakeEdit.Text;
end;

procedure TCompForm.GdbEditChange(Sender: TObject);
begin
  devCompilerSet.gdbName := GdbEdit.Text;
end;

procedure TCompForm.WindresEditChange(Sender: TObject);
begin
  devCompilerSet.windresName := WindresEdit.Text;
end;

procedure TCompForm.DllwrapEditChange(Sender: TObject);
begin
  devCompilerSet.dllwrapName := DllwrapEdit.Text;
end;

procedure TCompForm.GprofEditChange(Sender: TObject);
begin
  devCompilerSet.gprofName := GprofEdit.Text;
end;

end.
