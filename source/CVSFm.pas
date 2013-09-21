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
unit CVSFm;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, devTabs, StdCtrls, Spin, devRun, ComCtrls, StrUtils, FileCtrl,
  Grids, ValEdit, CVSThread, XPMenu, Menus, CheckLst, DateUtils;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Variants, Classes, QGraphics, QControls, QForms,
  QDialogs, devTabs, QStdCtrls, devRun, QComCtrls, StrUtils, 
  QGrids, CVSThread, QMenus, QCheckLst, DateUtils, Types;
{$ENDIF}

type
  TCVSAction = (caImport, caCheckout, caCommit, caUpdate, caDiff, caLog, caAdd, caRemove, caLogin, caLogout);

  TCVSForm = class(TForm)
    devPages1: TdevPages;
    tabImport: TdevPage;
    tabRepos: TdevPage;
    btnOK: TButton;
    btnCancel: TButton;
    tabGlobal: TdevPage;
    lblCompression: TLabel;
    spnCompression: TSpinEdit;
    tabCheckout: TdevPage;
    tabCommit: TdevPage;
    tabUpdate: TdevPage;
    tabDiff: TdevPage;
    tabLog: TdevPage;
    chkUpdRecurse: TCheckBox;
    chkUpdResetSticky: TCheckBox;
    chkUpdCreateDirs: TCheckBox;
    chkUpdCleanCopy: TCheckBox;
    tabOutput: TdevPage;
    chkUseSSH: TCheckBox;
    memOutput: TRichEdit;
    chkDiffRecurse: TCheckBox;
    chkLogRecurse: TCheckBox;
    lblCommitMsg: TLabel;
    memCommitMsg: TMemo;
    lblCVSImportDir: TLabel;
    txtCVSImportDir: TEdit;
    btnCVSImportBrws: TButton;
    chkLogDefBranch: TCheckBox;
    chkLogRCS: TCheckBox;
    chkLogNoTag: TCheckBox;
    chkDiffUnified: TCheckBox;
    grpRepDetails: TGroupBox;
    lblMethod: TLabel;
    lblUser: TLabel;
    lblServer: TLabel;
    lblDir: TLabel;
    lblRepos: TLabel;
    txtUser: TEdit;
    txtServer: TEdit;
    txtDir: TEdit;
    cmbMethod: TComboBox;
    lblRep: TLabel;
    cmbRepos: TComboBox;
    lblCOModule: TLabel;
    txtCOmodule: TEdit;
    lblCODir: TLabel;
    txtCOdir: TEdit;
    btnCOBrws: TButton;
    chkCOrecurse: TCheckBox;
    txtCOModuleAs: TEdit;
    chkCOModuleAs: TCheckBox;
    vle: TValueListEditor;
    lblImpAction: TLabel;
    lblImpVendor: TLabel;
    lblImpRelease: TLabel;
    lblImpMsg: TLabel;
    txtImpVendor: TEdit;
    txtImpRelease: TEdit;
    memImpMsg: TMemo;
    lblImpModule: TLabel;
    txtImpModule: TEdit;
    grpUpdRevisions: TGroupBox;
    chkBeforeDate: TCheckBox;
    chkRevision: TCheckBox;
    chkMostRecent: TCheckBox;
    cmbBeforeDate: TComboBox;
    cmbRevision: TComboBox;
    chkUpdPrune: TCheckBox;
    grpLogFilter: TGroupBox;
    chkLogFbyRev: TCheckBox;
    chkLogFbyDate: TCheckBox;
    chkLogFbyUser: TCheckBox;
    cmbLogFbyRev: TComboBox;
    cmbLogFbyDate: TComboBox;
    cmbLogFbyUser: TComboBox;
    grpDiff: TGroupBox;
    txtDiffRev1: TEdit;
    lblDiffRev1: TLabel;
    txtDiffRev2: TEdit;
    lblDiffRev2: TLabel;
    chkDiffDate1: TCheckBox;
    chkDiffDate2: TCheckBox;
    rgbDiff: TRadioButton;
    rgbDiff1: TRadioButton;
    rgbDiff2: TRadioButton;
    tabAdd: TdevPage;
    tabRemove: TdevPage;
    lblAddMsg: TLabel;
    memAddMsg: TMemo;
    chkRemove: TCheckBox;
    XPMenu: TXPMenu;
    tabFiles: TdevPage;
    lblFiles: TLabel;
    lstFiles: TCheckListBox;
    txtPort: TEdit;
    lblPort: TLabel;
    cmbCOBeforeDate: TComboBox;
    chkCOBeforeDate: TCheckBox;
    chkCORevision: TCheckBox;
    cmbCORevision: TComboBox;
    chkCOMostRecent: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cmbMethodChange(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnCVSImportBrwsClick(Sender: TObject);
    procedure cmbReposChange(Sender: TObject);
    procedure btnCOBrwsClick(Sender: TObject);
    procedure chkCOModuleAsClick(Sender: TObject);
    procedure vleGetPickList(Sender: TObject; const KeyName: string;
      Values: TStrings);
    procedure txtImpModuleChange(Sender: TObject);
    procedure txtCOmoduleChange(Sender: TObject);
    procedure chkBeforeDateClick(Sender: TObject);
    procedure chkRevisionClick(Sender: TObject);
    procedure chkLogFbyRevClick(Sender: TObject);
    procedure chkLogFbyDateClick(Sender: TObject);
    procedure chkLogFbyUserClick(Sender: TObject);
    procedure rgbDiff1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lstFilesDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure chkCORevisionClick(Sender: TObject);
    procedure chkCOBeforeDateClick(Sender: TObject);
  private
    { Private declarations }
    fCVSAction: TCVSAction;
    fFiles: TStrings;
    fAllFiles: TStrings;
    fRunner: TCVSThread;
    fTerminateRunner: boolean;
    procedure LoadText;
    function CVSActionStr(CVSAct: TCVSAction): string;
    function CVSActionTabStr(CVSAct: TCVSAction): string;
    procedure UpdateVLE(Path: string);
    procedure SetupDefaults;
    procedure BreakUpRepository;
    procedure UpdateRepositoryDisplay;
    procedure CVSLineOutput(Sender: TObject; const Line: string);
    procedure CVSCheckAbort(var AbortThread: boolean);
    procedure CVSTerminated(Sender: TObject);
    procedure CVSNeedPassword(var Passwd: string);
    procedure FindModifiedFiles;
  public
    { Public declarations }
    property CVSAction: TCVSAction read fCVSAction write fCVSAction;
    property Files: TStrings read fFiles write fFiles;
    property AllFiles: TStrings read fAllFiles write fAllFiles;
  end;

var
  CVSForm: TCVSForm;

implementation

uses 
  devcfg, utils, MultiLangSupport, CVSPasswdFm;

{$R *.dfm}

procedure TCVSForm.SetupDefaults;
begin
  cmbBeforeDate.Items.Add(FormatDateTime('yyyy-mm-dd hh:nn', Now));
  cmbBeforeDate.Items.Add(FormatDateTime('=yyyy-mm-dd hh:nn', Now));
  cmbBeforeDate.Items.Add(FormatDateTime('=dd-mm-yyyy hh:nn', Now));
  cmbBeforeDate.Items.Add(FormatDateTime('<=yyyy-mm-dd', Now));
  cmbBeforeDate.Items.Add(FormatDateTime('dd mmm yyyy', Now));
  cmbBeforeDate.Items.Add(FormatDateTime('dd mmm', Now));
  cmbBeforeDate.Items.Add(FormatDateTime('yyyy-mm', Now));
  cmbBeforeDate.Enabled := False;
  cmbRevision.Enabled := False;
  cmbBeforeDate.Text := '';
  cmbRevision.Text := '';
  chkMostRecent.Enabled := False;
  txtCOModuleAs.Text := '';
  txtCOmodule.Text := '';
  if fFiles.Count > 0 then
    txtCOdir.Text := ExtractFilePath(fFiles[0])
  else
    txtCOdir.Text := '';
  cmbCOBeforeDate.Text := '';
  cmbCORevision.Text := '';
  chkCOMostRecent.Enabled := False;
  txtImpVendor.Text := 'avendor';
  txtImpRelease.Text := 'arelease';
  txtImpModule.Text := '';
  memImpMsg.Lines.Clear;

  cmbLogFbyRev.Text := '';
  cmbLogFbyDate.Text := '';
  cmbLogFbyUser.Text := '';
  cmbLogFbyDate.Items.Assign(cmbBeforeDate.Items);

  txtDiffRev1.Text := '';
  txtDiffRev2.Text := '';
end;

procedure TCVSForm.BreakUpRepository;
var
  idx: integer;
  S: string;
begin
  // take the repository (e.g. ":pserver:user@some.host.com:/remote/dir")
  // and break it up to fill in the respective edit boxes
  cmbMethod.ItemIndex := 1;
  txtUser.Text := '';
  txtServer.Text := '';
  txtPort.Text := '';
  txtPort.Text := '';
  txtDir.Text := '';

  repeat
    S := cmbRepos.Text;
    if (S = '') or (S[1] <> ':') then
      Break;
    Delete(S, 1, 1); // remove first ':'

    idx := Pos(':', S);
    if idx = 0 then
      Break;

    // set method
    cmbMethod.ItemIndex := cmbMethod.Items.IndexOf(Copy(S, 1, idx - 1));
    if cmbMethod.ItemIndex = -1 then
      Break;
    Delete(S, 1, idx); // remove second ':'

    if cmbMethod.ItemIndex > 0 then begin
      // set user
      idx := Pos('@', S);
      if idx = 0 then
        Break;
      txtUser.Text := Copy(S, 1, idx - 1);
      Delete(S, 1, idx); // remove '@'

      // set server
      idx := Pos(':', S);
      if idx = 0 then
        Break;
      txtServer.Text := Copy(S, 1, idx - 1);
      Delete(S, 1, idx);
    end;

    //set port number
    idx := Pos('/', S);
    if idx > 0 then
      if StrToIntDef(Copy(S, 1, idx -1), -1) <> -1 then
      begin
        txtPort.Text := Copy(S, 1, idx -1);
        Delete(S, 1, idx -1);
      end;

    // set dir
    txtDir.Text := S;

  until True;

  UpdateRepositoryDisplay;
end;

procedure TCVSForm.FormShow(Sender: TObject);
var
  I, idx: integer;
begin
  Screen.Cursor := crHourglass;
  LoadText;
  Caption := Format('CVS - %s', [CVSActionStr(fCVSAction)]);

  lstFiles.Items.Assign(fAllFiles);

  // if whole dir
  if ((fFiles.Count = 1) and DirectoryExists(fFiles[0])) or (fFiles.Count = 0) then
    tabFiles.TabVisible := False

  else if fFiles.Count > 0 then begin
    tabFiles.TabVisible := True;
    for I := 0 to fFiles.Count - 1 do begin
      idx := lstFiles.Items.IndexOf(fFiles[I]);
      if idx > -1 then
        lstFiles.Checked[idx] := True;
    end;
  end;

  FindModifiedFiles;

  cmbRepos.Items.Clear;
  for I := 0 to devCVSHandler.Repositories.Count - 1 do
    cmbRepos.Items.Add(devCVSHandler.Repositories.Values[IntToStr(I)]);
  cmbRepos.Text := '';
  if fCVSAction in [caImport, caCheckOut, caCommit, caAdd, caLogin, caLogout] then begin
    tabRepos.TabVisible := True;
    if cmbRepos.Items.Count > 0 then
      cmbRepos.ItemIndex := 0;
    BreakUpRepository;
    if fFiles.Count > 0 then
      txtCVSImportDir.Text := ExtractFilePath(fFiles[0])
    else
      txtCVSImportDir.Text := '';
    if (fCVSAction = caImport) and (txtCVSImportDir.Text <> '') and (txtCVSImportDir.Text <>  devDirs.Default) then
      UpdateVLE(txtCVSImportDir.Text);
  end
  else
    tabRepos.TabVisible := False;

  SetupDefaults;

  spnCompression.Value := devCVSHandler.Compression;

  tabImport.Caption := CVSActionTabStr(caImport);
  tabCheckout.Caption := CVSActionTabStr(caCheckout);
  tabCommit.Caption := CVSActionTabStr(caCommit);
  tabUpdate.Caption := CVSActionTabStr(caUpdate);
  tabDiff.Caption := CVSActionTabStr(caDiff);
  tabLog.Caption := CVSActionTabStr(caLog);
  tabAdd.Caption := CVSActionTabStr(caAdd);
  tabRemove.Caption := CVSActionTabStr(caRemove);

  tabImport.TabVisible := fCVSAction = caImport;
  tabCheckout.TabVisible := fCVSAction = caCheckout;
  tabCommit.TabVisible := fCVSAction = caCommit;
  tabUpdate.TabVisible := fCVSAction = caUpdate;
  tabDiff.TabVisible := fCVSAction = caDiff;
  tabLog.TabVisible := fCVSAction = caLog;
  tabAdd.TabVisible := fCVSAction = caAdd;
  tabRemove.TabVisible := fCVSAction = caRemove;

  memOutput.Lines.Clear;
  if fCVSAction in [caLogin, caLogout] then
    devPages1.ActivePage := tabRepos
  else
    devPages1.ActivePageIndex := Ord(fCVSAction);
  Screen.Cursor := crDefault;
end;

procedure TCVSForm.UpdateRepositoryDisplay;
begin
  if (cmbMethod.ItemIndex > 0) and ((txtUser.Text = '') or (txtServer.Text = '')) then begin
    btnOK.Enabled := False;
    lblRepos.Caption := 'Invalid repository...';
  end
  else if cmbMethod.ItemIndex = 0 then begin
    btnOK.Enabled := True;
    lblRepos.Caption := Format(':%s:%s%s', [
      cmbMethod.Text,
        txtPort.Text,
        txtDir.Text
        ]);
  end
  else begin
    btnOK.Enabled := True;
    lblRepos.Caption := Format(':%s:%s@%s:%s%s', [
      cmbMethod.Text,
        txtUser.Text,
        txtServer.Text,
        txtPort.Text,
        txtDir.Text
        ]);
  end;
end;

procedure TCVSForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

function TCVSForm.CVSActionStr(CVSAct: TCVSAction): string;
begin
  case CVSAct of
    caImport: Result := Lang[ID_CVS_IMPORT];
    caCheckout: Result := Lang[ID_CVS_CHECKOUT];
    caCommit: Result := Lang[ID_CVS_COMMIT];
    caUpdate: Result := Lang[ID_CVS_UPDATE];
    caDiff: Result := Lang[ID_CVS_DIFF];
    caLog: Result := Lang[ID_CVS_LOG];
    caAdd: Result := Lang[ID_CVS_ADD];
    caRemove: Result := Lang[ID_CVS_REMOVE];
  else
    Result := '';
  end;
end;

function TCVSForm.CVSActionTabStr(CVSAct: TCVSAction): string;
begin
  case CVSAct of
    caImport: Result := Lang[ID_CVS_IMPORTTAB];
    caCheckout: Result := Lang[ID_CVS_CHECKOUTTAB];
    caCommit: Result := Lang[ID_CVS_COMMITTAB];
    caUpdate: Result := Lang[ID_CVS_UPDATETAB];
    caDiff: Result := Lang[ID_CVS_DIFFTAB];
    caLog: Result := Lang[ID_CVS_LOGTAB];
    caAdd: Result := Lang[ID_CVS_ADDTAB];
    caRemove: Result := Lang[ID_CVS_REMOVETAB];
  else
    Result := '';
  end;
end;

procedure TCVSForm.cmbMethodChange(Sender: TObject);
begin
  txtUser.Enabled := cmbMethod.ItemIndex > 0;
  txtServer.Enabled := cmbMethod.ItemIndex > 0;
  UpdateRepositoryDisplay;
end;

procedure TCVSForm.btnOKClick(Sender: TObject);
var
  Cmd, sLog, Dir: string;
  I: integer;
  PrettyCmd: string;
begin
  if Assigned(fRunner) then begin
    MessageDlg('Already running a command!', mtWarning, [mbOk], 0);
    Exit;
  end;

  //rebuild the fFiles TStrings based on the lstFiles checked items
  if tabFiles.TabVisible then begin
    fFiles.Clear;
    for I := 0 to lstFiles.Items.Count - 1 do
      if lstFiles.Checked[I] then
        fFiles.Add(lstFiles.Items[I]);
  end;

  if tabRepos.TabVisible then begin
    I := cmbRepos.Items.IndexOf(lblRepos.Caption);
    if I = -1 then
      cmbRepos.Items.Insert(0, lblRepos.Caption)
    else
      cmbRepos.Items.Move(I, 0);
    while cmbRepos.Items.Count > 10 do
      cmbRepos.Items.Delete(cmbRepos.Items.Count - 1);

    devCVSHandler.Repositories.Clear;
    for I := 0 to cmbRepos.Items.Count - 1 do
      devCVSHandler.Repositories.Add(Format('%d=%s', [I, cmbRepos.Items[I]]));
  end;
  devCVSHandler.Compression := spnCompression.Value;

  if chkUseSSH.Checked then
    SetEnvironmentVariable('CVS_RSH', 'ssh.exe')
  else
    SetEnvironmentVariable('CVS_RSH', 'rsh.exe');

  memOutput.Lines.Clear;

  if fFiles.Count > 0 then
    Dir := ExcludeTrailingPathDelimiter(ExtractFilePath(fFiles[0]))
  else
    Dir := GetCurrentDir;

  case fCVSAction of

    caImport: begin
        Cmd := Format('%s -z%d -d %s import', [devCVSHandler.Executable, devCVSHandler.Compression, lblRepos.Caption]);

        Cmd := Format('%s -I ! -I CVS -I .#*', [Cmd]);
        for I := 1 to vle.Strings.Count do
          if vle.Cells[1, I] = 'Ignore' then
            Cmd := Format('%s -I *%s', [Cmd, vle.Cells[0, I]])
          else if vle.Cells[1, I] = 'Binary' then
            Cmd := Format('%s -W "*%s -k''b''"', [Cmd, vle.Cells[0, I]]);

        Dir := txtCVSImportDir.Text;

        sLog := memImpMsg.Text;
        if Trim(sLog) = '' then
          sLog := '* No message *'
        else begin
          sLog := StringReplace(memImpMsg.Text, #13#10, #10, [rfReplaceAll]);
          sLog := StringReplace(sLog, '"', '\"', [rfReplaceAll]);
        end;
        Cmd := Format('%s -m "%s" %s %s %s', [Cmd, sLog, txtImpModule.Text, txtImpVendor.Text, txtImpRelease.Text]);
      end;

    caCheckout: begin
        Cmd := Format('%s -z%d -d %s checkout', [devCVSHandler.Executable, devCVSHandler.Compression, lblRepos.Caption]);

        if not chkCORecurse.Checked then
          Cmd := Format('%s -l', [Cmd]);

        if chkCOModuleAs.Checked then
          Cmd := Format('%s -d %s', [Cmd, txtCOModuleAs.Text]);

        if chkCOBeforeDate.Checked and (Trim(cmbCOBeforeDate.Text) <> '') then
          Cmd := Format('%s -D "%s"', [Cmd, cmbCOBeforeDate.Text]);

        if chkCORevision.Checked and (Trim(cmbCORevision.Text) <> '') then
          Cmd := Format('%s -r %s', [Cmd, cmbCORevision.Text]);

        if chkCOMostRecent.Enabled and
          ((chkCOBeforeDate.Checked and (Trim(cmbCOBeforeDate.Text) <> '')) or
          (chkCORevision.Checked and (Trim(cmbCORevision.Text) <> ''))) then
          Cmd := Format('%s -f', [Cmd]);

        Dir := txtCOdir.Text;

        Cmd := Format('%s %s', [Cmd, txtCOmodule.Text]);
      end;

    caUpdate: begin
        Cmd := Format('%s -z%d update', [devCVSHandler.Executable, devCVSHandler.Compression]);
        if not chkUpdRecurse.Checked then
          Cmd := Format('%s -l', [Cmd]);

        if chkUpdResetSticky.Checked then
          Cmd := Format('%s -A', [Cmd]);

        if chkUpdCreateDirs.Checked then
          Cmd := Format('%s -d', [Cmd]);

        if chkUpdPrune.Checked then
          Cmd := Format('%s -P', [Cmd]);

        if chkUpdCleanCopy.Checked then
          Cmd := Format('%s -C', [Cmd]);

        if chkBeforeDate.Checked and (cmbBeforeDate.Text <> '') then
          Cmd := Format('%s -D "%s"', [Cmd, cmbBeforeDate.Text]);

        if chkRevision.Checked and (cmbRevision.Text <> '') then
          Cmd := Format('%s -r %s', [Cmd, cmbRevision.Text]);

        if chkMostRecent.Enabled and
          ((chkBeforeDate.Checked and (cmbBeforeDate.Text <> '')) or
          (chkRevision.Checked and (cmbRevision.Text <> ''))) then
          Cmd := Format('%s -f', [Cmd]);

        for I := 0 to fFiles.Count - 1 do
          Cmd := Format('%s %s', [Cmd, ExtractRelativePath(Dir, ExtractFileName(fFiles[I]))]);
      end;

    caDiff: begin
        Cmd := Format('%s -z%d diff', [devCVSHandler.Executable, devCVSHandler.Compression]);
        if not chkDiffRecurse.Checked then
          Cmd := Format('%s -l', [Cmd]);
        if chkDiffUnified.Checked then
          Cmd := Format('%s -u', [Cmd]);
        if (rgbDiff1.Checked or rgbDiff2.Checked) and (txtDiffRev1.Text <> '') then begin
          if chkDiffDate1.Checked then
            Cmd := Format('%s -D "%s"', [Cmd, txtDiffRev1.Text])
          else
            Cmd := Format('%s -r%s', [Cmd, txtDiffRev1.Text]);
        end;
        if rgbDiff2.Checked and (txtDiffRev2.Text <> '') then begin
          if chkDiffDate2.Checked then
            Cmd := Format('%s -D "%s"', [Cmd, txtDiffRev2.Text])
          else
            Cmd := Format('%s -r%s', [Cmd, txtDiffRev2.Text]);
        end;
        for I := 0 to fFiles.Count - 1 do
          Cmd := Format('%s %s', [Cmd, ExtractRelativePath(Dir, ExtractFileName(fFiles[I]))]);
      end;

    caLog: begin
        Cmd := Format('%s -z%d log', [devCVSHandler.Executable, devCVSHandler.Compression]);
        if not chkLogRecurse.Checked then
          Cmd := Format('%s -l', [Cmd]);
        if chkLogDefBranch.Checked then
          Cmd := Format('%s -r', [Cmd]);
        if chkLogRCS.Checked then
          Cmd := Format('%s -R', [Cmd]);
        if chkLogNoTag.Checked then
          Cmd := Format('%s -N', [Cmd]);
        if chkLogFbyRev.Checked and (cmbLogFbyRev.Text <> '') then
          Cmd := Format('%s -r%s', [Cmd, cmbLogFbyRev.Text]);
        if chkLogFbyDate.Checked and (cmbLogFbyDate.Text <> '') then
          Cmd := Format('%s -d "%s"', [Cmd, cmbLogFbyDate.Text]);
        if chkLogFbyUser.Checked and (cmbLogFbyUser.Text <> '') then
          Cmd := Format('%s -w%s', [Cmd, cmbLogFbyUser.Text]);
        for I := 0 to fFiles.Count - 1 do
          Cmd := Format('%s %s', [Cmd, ExtractRelativePath(Dir, ExtractFileName(fFiles[I]))]);
      end;

    caAdd: begin
        Cmd := Format('%s -d %s -z%d add', [devCVSHandler.Executable, lblRepos.Caption, devCVSHandler.Compression]);
        if memAddMsg.Text <> '' then
          Cmd := Format('%s -m"%s"', [Cmd, memAddMsg.Text]);
        for I := 0 to fFiles.Count - 1 do
          Cmd := Format('%s %s', [Cmd, ExtractRelativePath(Dir, ExtractFileName(fFiles[I]))]);
      end;

    caRemove: begin
        Cmd := Format('%s -z%d remove', [devCVSHandler.Executable, devCVSHandler.Compression]);
        if chkRemove.Checked then
          Cmd := Format('%s -f', [Cmd]);
        for I := 0 to fFiles.Count - 1 do
          Cmd := Format('%s %s', [Cmd, ExtractRelativePath(Dir, ExtractFileName(fFiles[I]))]);
      end;

    caCommit: begin
        Cmd := Format('%s -d %s -z%d commit', [devCVSHandler.Executable, lblRepos.Caption, devCVSHandler.Compression]);
        sLog := memCommitMsg.Text;
        if Trim(sLog) = '' then
          sLog := '* No message *'
        else begin
          sLog := StringReplace(memCommitMsg.Text, #13#10, #10, [rfReplaceAll]);
          sLog := StringReplace(sLog, '"', '\"', [rfReplaceAll]);
        end;
        Cmd := Format('%s -m"%s"', [Cmd, sLog]);
        for I := 0 to fFiles.Count - 1 do
          Cmd := Format('%s %s', [Cmd, ExtractRelativePath(Dir, ExtractFileName(fFiles[I]))]);
      end;

    caLogin: Cmd := Format('%s -z%d -d %s login', [devCVSHandler.Executable, devCVSHandler.Compression, lblRepos.Caption]);

    caLogout: Cmd := Format('%s -z%d -d %s logout', [devCVSHandler.Executable, devCVSHandler.Compression, lblRepos.Caption]);
  end;

  if Cmd <> '' then begin
    PrettyCmd := StringReplace(Cmd, devCVSHandler.Executable, 'cvs', []);
    if Length(sLog) > 32 then
      PrettyCmd := StringReplace(PrettyCmd, sLog, Copy(sLog, 1, 32) + '...', []);
    PrettyCmd := StringReplace(PrettyCmd, #10, ' ', [rfReplaceAll]);
    CVSLineOutput(nil, Format('>> Running "%s" (in "%s")', [PrettyCmd, Dir]));
    btnOK.Enabled := False;
    fRunner := TCVSThread.Create(true);
    fRunner.Command := Cmd;
    fRunner.Directory := Dir;
    fRunner.OnTerminate := CVSTerminated;
    fRunner.OnLineOutput := CVSLineOutput;
    fRunner.OnCheckAbort := CVSCheckAbort;
    fRunner.OnNeedPassword := CVSNeedPassword;
    fRunner.FreeOnTerminate := True;
    fRunner.Resume;
    btnCancel.Caption := Lang[ID_BTN_CLOSE];
    devPages1.ActivePage := tabOutput;
    memOutput.SetFocus;
    Screen.Cursor := crHourglass;
    fTerminateRunner := False;
  end;
end;

procedure TCVSForm.CVSTerminated(Sender: TObject);
var
  RetValue: integer;
  S: string;
  I: integer;
begin
  S := fRunner.Output;
  I := LastDelimiter(' ', S);
  if I > 0 then
    Delete(S, 1, I);
  RetValue := StrToIntDef(Trim(S), -1);
  CVSLineOutput(nil, Format('>> Command complete (exit code: %d)', [RetValue]));
  Screen.Cursor := crDefault;
  fRunner := nil;
  btnCancel.Caption := Lang[ID_BTN_CLOSE];
  btnOK.Enabled := RetValue <> 0;
end;

procedure TCVSForm.CVSCheckAbort(var AbortThread: boolean);
begin
  AbortThread := fTerminateRunner;
end;

procedure TCVSForm.CVSNeedPassword(var Passwd: string);
begin
  with TCVSPasswdForm.Create(nil) do begin
    ShowModal;
    Passwd := txtPass.Text;
    Free;
  end;
end;

procedure TCVSForm.btnCancelClick(Sender: TObject);
begin
  if Assigned(fRunner) then begin
    if fTerminateRunner then begin
      if MessageDlg('Do you want to force-terminate process?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        if TerminateThread(fRunner.Handle, Cardinal(-1)) then
          CVSTerminated(nil);
    end
    else
      fTerminateRunner := True;
  end
  else
    Close;
end;

procedure TCVSForm.CVSLineOutput(Sender: TObject; const Line: string);
type
  THighlightType = (htNone, htDevCpp, htServer, htUpdated, htModified, htConflict, htUnknown, htDiffIn, htDiffOut, htIgnored, htNew, htPatched);
const
  HighlightColors: array[Low(THighlightType)..High(THighlightType)] of TColor =
  (clBlack, clBlack, clBlack, clGreen, clRed, clMaroon, clGray, clBlue, clRed, clGray, clFuchsia, clGreen);
  HighlightStyles: array[Low(THighlightType)..High(THighlightType)] of TFontStyles =
  ([], [fsBold], [fsBold], [], [fsBold], [fsBold], [], [], [], [], [fsBold], [fsBold]);
var
  HiType: THighlightType;
  sl: TStringList;
  I: integer;
begin
  sl := TStringList.Create;
  try
    sl.Text := Line;

    for I := 0 to sl.Count - 1 do begin
      if AnsiStartsStr('>> ', sl[I]) or
        AnsiStartsStr('@@', sl[I]) then
        HiType := htDevCPP
      else if AnsiStartsStr('? ', sl[I]) then
        HiType := htUnknown
      else if AnsiStartsStr('U ', sl[I]) then
        HiType := htUpdated
      else if AnsiStartsStr('M ', sl[I]) then
        HiType := htModified
      else if AnsiStartsStr('C ', sl[I]) then
        HiType := htConflict
      else if AnsiStartsStr('I ', sl[I]) then
        HiType := htIgnored
      else if AnsiStartsStr('N ', sl[I]) then
        HiType := htNew
      else if AnsiStartsStr('P ', sl[I]) then
        HiType := htPatched
      else if AnsiStartsStr('> ', sl[I]) or
        AnsiStartsStr('+', sl[I]) then
        HiType := htDiffIn
      else if AnsiStartsStr('< ', sl[I]) or
        AnsiStartsStr('-', sl[I]) then
        HiType := htDiffOut
      else if AnsiStartsStr('cvs server: ', sl[I]) then
        HiType := htServer
      else
        HiType := htNone;

      memOutput.SelAttributes.Color := HighlightColors[HiType];
      memOutput.SelAttributes.Style := HighlightStyles[HiType];
      memOutput.Lines.Add(sl[I]);
    end;
  finally
    sl.Free;
  end;
end;

procedure TCVSForm.btnCVSImportBrwsClick(Sender: TObject);
var
  s: string;
begin
  s := txtCVSImportDir.Text;
  if SelectDirectory(Lang[ID_ENV_SELUSERDIR], '', s) then
    txtCVSImportDir.Text := IncludeTrailingPathDelimiter(s);

  vle.Strings.Clear;
  UpdateVLE(txtCVSImportDir.Text);
end;

procedure TCVSForm.cmbReposChange(Sender: TObject);
begin
  BreakUpRepository;
end;

procedure TCVSForm.btnCOBrwsClick(Sender: TObject);
var
  s: string;
begin
  s := txtCOdir.Text;
  if SelectDirectory(Lang[ID_ENV_SELUSERDIR], '', s) then
    txtCOdir.Text := IncludeTrailingPathDelimiter(s);
end;

procedure TCVSForm.chkCOModuleAsClick(Sender: TObject);
begin
  txtCOModuleAs.Enabled := chkCOModuleAs.Checked;
  if txtCOModuleAs.Enabled then
    txtCOModuleAs.Text := txtCOmodule.Text;
end;

procedure TCVSForm.UpdateVLE(Path: string);
  function IsTextOrBinary(Filename: string): integer; // 0:text, 1:binary, 2:ignore
  var
    hFile: integer;
    Buf: array[1..1024] of Char;
  begin
    Result := 2;
    hFile := FileOpen(Filename, fmOpenRead);
    if hFile = -1 then
      Exit;
    while FileRead(hFile, Buf, SizeOf(Buf)) > 0 do begin
      Result := 0;
      if Pos(#01, Buf) > 0 then begin
        Result := 1;
        Break;
      end;
    end;
    FileClose(hFile);
  end;
var
  R: integer;
  SR: TSearchRec;
begin
  if FindFirst(Path + '*.*', faAnyFile, SR) = 0 then
    repeat
      if (SR.Name = '.') or (SR.Name = '..') then
        Continue;
      if (SR.Attr and faDirectory) = faDirectory then
        UpdateVLE(Path + SR.Name + '\')
      else begin
        if (not AnsiStartsStr('.#', SR.Name)) and (ExtractFileExt(SR.Name) <> '') then
          if not vle.FindRow(ExtractFileExt(SR.Name), R) then begin
            case IsTextOrBinary(Path + SR.Name) of
              0: vle.InsertRow(ExtractFileExt(SR.Name), 'Text', True);
              1: vle.InsertRow(ExtractFileExt(SR.Name), 'Binary', True);
              2: vle.InsertRow(ExtractFileExt(SR.Name), 'Ignore', True);
            end;
            vle.ItemProps[ExtractFileExt(SR.Name)].ReadOnly := True;
          end;
      end;
    until FindNext(SR) <> 0;
end;

procedure TCVSForm.vleGetPickList(Sender: TObject; const KeyName: string;
  Values: TStrings);
begin
  Values.Clear;
  Values.Add('Text');
  Values.Add('Binary');
  Values.Add('Ignore');
end;

procedure TCVSForm.txtImpModuleChange(Sender: TObject);
begin
  if fCVSAction = caImport then
    btnOK.Enabled := (txtImpModule.Text <> '') and
      (txtImpVendor.Text <> '') and
      (txtImpRelease.Text <> '');
end;

procedure TCVSForm.txtCOmoduleChange(Sender: TObject);
begin
  if fCVSAction = caCheckout then
    btnOK.Enabled := (txtCOmodule.Text <> '') and
      (txtCOdir.Text <> '') and
      (lblRepos.Caption <> 'Invalid repository...');
end;

procedure TCVSForm.chkBeforeDateClick(Sender: TObject);
begin
  cmbBeforeDate.Enabled := chkBeforeDate.Checked;
  chkMostRecent.Enabled := chkBeforeDate.Checked or chkRevision.Checked;
end;

procedure TCVSForm.chkRevisionClick(Sender: TObject);
begin
  cmbRevision.Enabled := chkRevision.Checked;
  chkMostRecent.Enabled := chkBeforeDate.Checked or chkRevision.Checked;
end;

procedure TCVSForm.chkLogFbyRevClick(Sender: TObject);
begin
  cmbLogFbyRev.Enabled := chkLogFbyRev.Checked;
end;

procedure TCVSForm.chkLogFbyDateClick(Sender: TObject);
begin
  cmbLogFbyDate.Enabled := chkLogFbyDate.Checked;
end;

procedure TCVSForm.chkLogFbyUserClick(Sender: TObject);
begin
  cmbLogFbyUser.Enabled := chkLogFbyUser.Checked;
end;

procedure TCVSForm.rgbDiff1Click(Sender: TObject);
begin
  txtDiffRev1.Enabled := rgbDiff1.Checked or rgbDiff2.Checked;
  txtDiffRev2.Enabled := rgbDiff2.Checked;
  chkDiffDate1.Enabled := txtDiffRev1.Enabled;
  chkDiffDate2.Enabled := txtDiffRev2.Enabled;
end;

procedure TCVSForm.LoadText;
begin
  if devData.XPTheme then
    XPMenu.Active := true
  else
    XPMenu.Active := false;
  tabImport.Caption := Lang[ID_CVS_IMPORTTAB];
  tabRepos.Caption := Lang[ID_CVS_REPOSITORYTAB];
  tabGlobal.Caption := Lang[ID_CVS_GLOBALTAB];
  tabCheckout.Caption := Lang[ID_CVS_CHECKOUTTAB];
  tabCommit.Caption := Lang[ID_CVS_COMMITTAB];
  tabUpdate.Caption := Lang[ID_CVS_UPDATETAB];
  tabDiff.Caption := Lang[ID_CVS_DIFFTAB];
  tabLog.Caption := Lang[ID_CVS_LOGTAB];
  tabOutput.Caption := Lang[ID_CVS_OUTPUTTAB];
  tabFiles.Caption := Lang[ID_NEWTPL_PAGEFILES];

  lblCVSImportDir.Caption := Lang[ID_CVS_IMPDIR];
  lblImpAction.Caption := Lang[ID_CVS_IMPACTION];
  lblImpVendor.Caption := Lang[ID_CVS_IMPVENDOR];
  lblImpRelease.Caption := Lang[ID_CVS_IMPRELEASE];
  lblImpMsg.Caption := Lang[ID_CVS_LOGMSG];
  lblImpModule.Caption := Lang[ID_CVS_MODULE];
  vle.TitleCaptions[0] := Lang[ID_CVS_IMPEXT];
  vle.TitleCaptions[1] := Lang[ID_CVS_IMPACTION];

  lblCOModule.Caption := Lang[ID_CVS_MODULE];
  chkCOModuleAs.Caption := Lang[ID_CVS_COAS];
  lblCODir.Caption := Lang[ID_CVS_CODIR];
  chkCOrecurse.Caption := Lang[ID_CVS_RECURSE];
  chkCOBeforeDate.Caption := Lang[ID_CVS_BEFOREDATE];
  chkCORevision.Caption := Lang[ID_CVS_REVISION];
  chkCOMostRecent.Caption := Lang[ID_CVS_GETMOSTRECENT];

  lblCommitMsg.Caption := Lang[ID_CVS_LOGMSG];

  chkUpdRecurse.Caption := Lang[ID_CVS_RECURSE];
  chkUpdResetSticky.Caption := Lang[ID_CVS_RESETSTICKY];
  chkUpdCreateDirs.Caption := Lang[ID_CVS_CREATEDIRS];
  chkUpdPrune.Caption := Lang[ID_CVS_PRUNEEMPTY];
  chkUpdCleanCopy.Caption := Lang[ID_CVS_GETCLEAN];
  chkBeforeDate.Caption := Lang[ID_CVS_BEFOREDATE];
  chkRevision.Caption := Lang[ID_CVS_REVISION];
  chkMostRecent.Caption := Lang[ID_CVS_GETMOSTRECENT];
  grpUpdRevisions.Caption := Lang[ID_CVS_OTHERREVISIONS];

  chkDiffRecurse.Caption := Lang[ID_CVS_RECURSE];
  chkDiffUnified.Caption := Lang[ID_CVS_DIFFUNIFIED];
  grpDiff.Caption := Lang[ID_CVS_OTHERREVISIONS];
  rgbDiff.Caption := Lang[ID_CVS_DIFF1];
  rgbDiff1.Caption := Lang[ID_CVS_DIFF2];
  rgbDiff2.Caption := Lang[ID_CVS_DIFF3];
  lblDiffRev1.Caption := Lang[ID_CVS_REVISION] + ' 1';
  lblDiffRev2.Caption := Lang[ID_CVS_REVISION] + ' 2';
  chkDiffDate1.Caption := Lang[ID_CVS_ISDATE];
  chkDiffDate2.Caption := Lang[ID_CVS_ISDATE];

  chkLogRecurse.Caption := Lang[ID_CVS_RECURSE];
  chkLogDefBranch.Caption := Lang[ID_CVS_LOGDEFBRANCH];
  chkLogRCS.Caption := Lang[ID_CVS_LOGRCS];
  chkLogNoTag.Caption := Lang[ID_CVS_LOGNOTAG];
  grpLogFilter.Caption := Lang[ID_CVS_LOGFILTER];
  chkLogFbyRev.Caption := Lang[ID_CVS_LOGBYREV];
  chkLogFbyDate.Caption := Lang[ID_CVS_LOGBYDATE];
  chkLogFbyUser.Caption := Lang[ID_CVS_LOGBYUSER];

  lblAddMsg.Caption := Lang[ID_CVS_LOGMSG];

  chkRemove.Caption := Lang[ID_CVS_REMOVEFILE];

  lblRep.Caption := Lang[ID_CVS_REPOSITORY];
  grpRepDetails.Caption := Lang[ID_CVS_REPDETAILS];
  lblMethod.Caption := Lang[ID_CVS_REPMETHOD];
  lblUser.Caption := Lang[ID_CVS_REPUSER];
  lblServer.Caption := Lang[ID_CVS_REPSERVER];
  lblDir.Caption := Lang[ID_CVS_REPDIR];

  lblCompression.Caption := Lang[ID_ENV_CVSCOMPR];
  chkUseSSH.Caption := Lang[ID_ENV_CVSUSESSH];

  lblFiles.Caption := Lang[ID_NEWTPL_PAGEFILES] + ':';

  btnOK.Caption := Lang[ID_BTN_OK];
  btnCancel.Caption := Lang[ID_BTN_CLOSE];
end;

procedure TCVSForm.FormCreate(Sender: TObject);
begin
  fFiles := TStringList.Create;
  fAllFiles := TStringList.Create;
end;

procedure TCVSForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fAllFiles);
  FreeAndNil(fFiles);
end;

procedure TCVSForm.FindModifiedFiles;
  function GetFileTimeStr(Filename: string): string;
  var
    d: TDateTime;
    hFile: THandle;
    st: SYSTEMTIME;
    tz: TTimeZoneInformation;
    ft: TFileTime;
    daystr, monthstr: array[0..16] of char;
  begin
    hFile := FileOpen(Filename, fmOpenRead);
    try
      GetFileTime(hFile, nil, nil, @ft);
      FileTimeToSystemTime(ft, st);
      d := SystemTimeToDateTime(st);
      GetTimeZoneInformation(tz);
    finally
      FileClose(hFile);
    end;

    GetLocaleInfo($0409, LOCALE_SABBREVDAYNAME1 + DayOfWeek(d) - 2, daystr, sizeof(daystr));
    GetLocaleInfo($0409, LOCALE_SABBREVMONTHNAME1 + MonthOfTheYear(d) - 1, monthstr, sizeof(monthstr));

    Result := FormatDateTime('dd hh:nn:ss yyyy', d);
    Result := Format('%s %s %s', [daystr, monthstr, Result]);
  end;
var
  I, idx: integer;
  sl: TStringList;
  fname, dstr, S, prefix: string;
  modif: integer;
begin
  { Not implemented yet }
  Exit;

  if not FileExists('CVS\Entries') then
    Exit;
  if fFiles.Count > 0 then
    prefix := IncludeTrailingPathDelimiter(ExtractFilePath(fFiles[0]));
  sl := TStringList.Create;
  try
    sl.LoadFromFile('CVS\Entries');
    for I := 0 to sl.Count - 1 do begin
      S := sl[I];

      idx := Pos('/', S); // find /
      if idx = 0 then Continue;
      Delete(S, 1, idx);
      // the filename is here
      idx := Pos('/', S); // find /
      if idx = 0 then Continue;
      fname := Copy(S, 1, idx - 1); //fname
      Delete(S, 1, idx);
      idx := Pos('/', S); // find /
      if idx = 0 then Continue;
      Delete(S, 1, idx);
      // the date is here
      idx := Pos('/', S); // find /
      if idx = 0 then Continue;
      dstr := Copy(S, 1, idx - 1); //dstr

      idx := lstFiles.Items.IndexOf(prefix + fname);
      if idx > 0 then begin
        if GetFileTimeStr(prefix + fname) <> dstr then
          modif := 1
        else
          modif := 0;
        lstFiles.Items.Objects[idx] := Pointer(modif);
      end
      else
    end;
  finally
    sl.Free;
  end;
end;

procedure TCVSForm.lstFilesDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  with lstFiles.Canvas do begin
    if lstFiles.Items.Objects[Index] = Pointer(1) then //modified
      Font.Color := clRed;
    FillRect(Rect);
    TextOut(Rect.Left, Rect.Top, lstFiles.Items[Index]);
  end;
end;

procedure TCVSForm.chkCORevisionClick(Sender: TObject);
begin
  cmbCORevision.Enabled := chkCORevision.Checked;
  chkCOMostRecent.Enabled := chkCOBeforeDate.Checked or chkCORevision.Checked;
end;

procedure TCVSForm.chkCOBeforeDateClick(Sender: TObject);
begin
  cmbCOBeforeDate.Enabled := chkCOBeforeDate.Checked;
  chkCOMostRecent.Enabled := chkCOBeforeDate.Checked or chkCORevision.Checked;
end;

end.

