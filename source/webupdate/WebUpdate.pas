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

unit WebUpdate;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, WebThread, CheckLst, ExtCtrls, IniFiles,
  ShellApi, TypInfo, Buttons, XPMenu;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Variants, Classes, QGraphics, QControls, QForms,
  QDialogs, QComCtrls, QStdCtrls, WebThread, QCheckLst, QExtCtrls, IniFiles,
  TypInfo, QButtons;
{$ENDIF}

type
  TWebUpdateForm = class(TForm)
    Label1: TLabel;
    lv: TListView;
    Label2: TLabel;
    lblTotalSize: TLabel;
    memDescr: TMemo;
    Label3: TLabel;
    Label4: TLabel;
    cmbGroups: TComboBox;
    ProgressBar1: TProgressBar;
    Label5: TLabel;
    lblStatus: TLabel;
    Shape1: TShape;
    Bevel1: TBevel;
    Image1: TImage;
    Label6: TLabel;
    cmbMirrors: TComboBox;
    Label7: TLabel;
    Bevel2: TBevel;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    btnCheck: TBitBtn;
    btnClose: TBitBtn;
    XPMenu: TXPMenu;
    procedure FormCreate(Sender: TObject);
    procedure lvClick(Sender: TObject);
    procedure lvChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure cmbGroupsChange(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cmbMirrorsChange(Sender: TObject);
    procedure lvColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure lvKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
    wThread: TWebThread;
    fUpdateList: TList;
    fMirrorList: TStrings;
    fErrorsList: TStrings;
    fSelfUpdate: string;
    ColumnToSort: integer;
    SortAscending: boolean;
    FormInitialized: Boolean;
    procedure GetUpdateInfo(Sender: TObject);
    procedure DownloadFiles(Sender: TObject);
    function CheckFile(Filename: string): boolean;
    procedure ClearList;
    procedure FillList(Filter: string);
    procedure CalcTotalSize;
    procedure UpdateSelf;
    procedure GetMirrorList;
    function ReplaceMacros(Source: string): string;
    procedure WriteInstalled(index : integer; InPackMan: Boolean);
    function GetSelected(index: integer): boolean;
    procedure EnableForm;
    procedure DisableForm;
    procedure CheckInstalledExes;
  public
    { Public declarations }
    BasePath: string;
    procedure wCheckTerminate(Sender: TObject);
    procedure wDownloadTerminate(Sender: TObject);
    procedure wThreadMessage(Sender: TObject; MsgCode: TWUMessageReason; Msg: string; TransferSize, CurrentSize: cardinal);
    procedure CreateParams(var params: TCreateParams); override;
  end;

var
  WebUpdateForm: TWebUpdateForm;

implementation

uses 
{$IFDEF WIN32}
  utils, version, devcfg, PackmanExitCodesU, main;
{$ENDIF}
{$IFDEF LINUX}
  Xlib, utils, version, devcfg, PackmanExitCodesU, main;  
{$ENDIF}

{$R *.dfm}

const
  MAGIC_HEADER = '{WebUpdate_config_file}';
  CONF_FILE = 'webupdate.conf';
//  DEFAULT_MIRROR = 'Andreas Aberg''s server=http://vUpdate.servebeer.com/';  // 'bloodshed.net (Bloodshed server)=http://haiku.bloodshed.net/dev/webupdate/';
  DEFAULT_MIRROR = 'PlanetMirror.com=http://public.planetmirror.com/pub/devcpp/';
  DEFAULT_MIRROR_2 = 'devpaks.org Community Devpaks=http://devpaks.sourceforge.net/';

var
  BASE_URL: string;
  AllGroupsText: string = '<All groups>';

procedure TWebUpdateForm.wThreadMessage(Sender: TObject; MsgCode: TWUMessageReason; Msg: string; TransferSize, CurrentSize: cardinal);
begin
  case MsgCode of
    wumrRetrieveProgress: begin
        ProgressBar1.Max := TransferSize;
        ProgressBar1.Position := CurrentSize;
      end;
    wumrConnectError,
      wumrRetrieveError,
      wumrUnknownError: fErrorsList.Add(Msg);
  else
    ProgressBar1.Position := 0;
    lblStatus.Caption := Msg;
  end;
end;

procedure TWebUpdateForm.wCheckTerminate(Sender: TObject);
var
  tmp: string;
begin
  if fErrorsList.Count > 0 then
{$IFDEF WIN32}
    MessageBox(Self.Handle, 'Could not connect to remote site. The remote site might be down, or your connection ' +
      'to the Internet might not be working...'#13#10#13#10'Please try another mirror site...', '', MB_ICONERROR or MB_OK)
{$ENDIF}
{$IFDEF LINUX}
    MessageDlg('Could not connect to remote site. The remote site might be down, or your connection ' +
      'to the Internet might not be working...'#13#10#13#10'Please try another mirror site...', mtError, [mbOk], 0)
{$ENDIF}
  else begin
    if (wThread.LastMessage = wumrDisconnect) and
      FileExists(PUpdateRec(wThread.Files[0])^.TempFilename) then begin
      tmp := PUpdateRec(wThread.Files[0])^.TempFilename;
      if CheckFile(PUpdateRec(wThread.Files[0])^.TempFilename) then begin
        btnCheck.OnClick := DownloadFiles;
        btnCheck.Caption := '&Download selected';
        memDescr.Lines.Clear;
        memDescr.Lines.Add('Step 1: Select the updates you want to download.');
        memDescr.Lines.Add('Step 2: Press "Download selected" to download the selected updates from the server.');
      end;
      DeleteFile(tmp);
    end;
  end;
  EnableForm;
  wThread := nil;
end;

procedure TWebUpdateForm.wDownloadTerminate(Sender: TObject);
var
  I: integer;
  Dest, PackFileName: string;
  PackProcess: THandle;
  PackExitCode: Cardinal;
  PackProcessInfo: TProcessInformation;
  PackStartupInfo: TStartupInfo;
begin
  if fErrorsList.Count = 0 then
    MessageDlg('The updates you selected have been downloaded. Now they will ' +
      'be installed.',
      mtInformation, [mbOk], 0)
  else if fErrorsList.Count > 0 then begin
    Dest := '';
    for I := 0 to fErrorsList.Count - 1 do
      Dest := Dest + fErrorsList[I] + #13#10;
    MessageDlg('The following files were not downloaded due to errors...'#13#10#13#10 + Dest, mtError, [mbOk], 0);
    if fErrorsList.Count < wThread.Files.Count then
      if MessageDlg('Some updates were retrieved though. Do you want to install them?',
        mtConfirmation, [mbYes, mbNo], 0) = mrNo then begin
        wThread := nil;
        EnableForm;
        Exit;
      end;
  end;
  for I := 0 to wThread.Files.Count - 1 do begin
    if FileExists(PUpdateRec(wThread.Files[I])^.TempFilename) then begin
      if PUpdateRec(wThread.Files[I])^.Execute then begin
        PackFileName := ExtractFilePath(Application.ExeName) + PACKAGES_DIR + ExtractFileName(PUpdateRec(wThread.Files[I])^.LocalFilename);
        ForceDirectories(ExtractFilePath(Application.ExeName) + PACKAGES_DIR); // Create <devcpp>\Packages
        DeleteFile(PackFileName);
        //Win9x does not support MoveFileEx
        if MoveFile(PChar(PUpdateRec(wThread.Files[I])^.TempFilename), PChar(PackFileName)) = BOOL(False) then
        begin
          MessageDlg('Could not move file: ' + PUpdateRec(wThread.Files[I])^.TempFilename +
          ' to ' + PackFileName + #13#10 + SysErrorMessage(GetLastError), mtWarning, [mbOK], 0);
        end
        else if FileExists(ExtractFilePath(Application.ExeName) + PACKMAN_PROGRAM) then begin
          //start Packman.exe
          FillChar(PackStartupInfo, SizeOf(PackStartupInfo), #0);
          with PackStartupInfo do begin
            cb := SizeOf(PackStartupInfo);
            lpReserved := Nil;
            dwFlags := STARTF_USESHOWWINDOW;
            wShowWindow := SW_SHOW;
          end;
          if CreateProcess(nil, PChar(ExtractFilePath(Application.ExeName) + PACKMAN_PROGRAM
            + ' /auto "' + PackFileName + '"'), nil, nil, False, NORMAL_PRIORITY_CLASS,
            nil, PChar(ExtractFilePath(Application.ExeName)),
            PackStartupInfo, PackProcessInfo) then begin
              PackProcess := PackProcessInfo.hProcess;
              while WaitforSingleObject(PackProcess, 250) <> WAIT_OBJECT_0 do
                Application.ProcessMessages;
              GetExitCodeProcess(PackProcess, PackExitCode);
              CloseHandle(PackProcessInfo.hProcess);
              CloseHandle(PackProcessInfo.hThread);
              if PackExitCode = Cardinal(PACKMAN_EXITCODE_NO_ERROR) then
                WriteInstalled(I, True)
              else
                MessageDlg('PackMan.exe failed to install the package or the installation was cancelled!', mtError, [mbOK], 0);
          end
          else
            MessageDlg('Couldn''t start PackMan.exe !', mtError, [mbOK], 0);
        end
        else
          MessageDlg('Could not find PackMan.exe in Dev-C++ folder !', mtError, [mbOK], 0);
      end
      else begin
        Dest := {BasePath + } ReplaceMacros(PUpdateRec(wThread.Files[I])^.InstallPath + '\' + PUpdateRec(wThread.Files[I])^.LocalFilename);
        ForceDirectories(ExtractFilePath(Dest));
        if CopyFile(PChar(PUpdateRec(wThread.Files[I])^.TempFilename), PChar(Dest), False) then begin
          DeleteFile(PUpdateRec(wThread.Files[I])^.TempFilename);
          WriteInstalled(I, False);
        end
        else begin
          if LowerCase(Dest) = LowerCase(Application.ExeName) then begin
            fSelfUpdate := PUpdateRec(wThread.Files[I])^.TempFilename;
            WriteInstalled(I, False);
          end
          else
            MessageDlg('Could not install file: ' + Dest, mtError, [mbOK], 0);
        end;
        if LowerCase(PUpdateRec(wThread.Files[I])^.LocalFilename) = LowerCase(DEV_WEBMIRRORS_FILE) then
          GetMirrorList;
      end;
    end;
  end;
 // MessageDlg('The downloaded updates were installed. The WebUpdate window will now close.', mtInformation, [mbOk], 0);
  UpdateSelf;
  for i := 0 to fUpdateList.Count - 1 do
    if Assigned(fUpdateList[i]) then
      PUpdateRec(fUpdateList[i])^.Selected := GetSelected(i);
  CalcTotalSize;
  EnableForm;
  wThread := nil;
end;

procedure TWebUpdateForm.WriteInstalled(index : integer; InPackMan: Boolean);
//InPackMan=false meaning write in devcpp.cfg
//otherwise just clean from the list because Packman is handling its devpak list
var i : integer;
begin
  if Not InPackMan then
    if FileExists(devDirs.Config + 'devcpp.cfg') then
      with TIniFile.Create(devDirs.Config + 'devcpp.cfg') do
        try
          WriteString(WEBUPDATE_SECTION, PUpdateRec(wThread.Files[index])^.Name, PUpdateRec(wThread.Files[index])^.Version);
        finally
          Free;
        end;

  //clear from the list        
  for i := 0 to lv.Items.Count - 1 do
    if lv.Items[i].Data = PUpdateRec(wThread.Files[index]) then
    begin
      lv.Items.Delete(i);
      break;
    end;
end;

function TWebUpdateForm.CheckFile(Filename: string): boolean;
var
  sl: TStringList;
  I: integer;
  Ini, LocalIni: TIniFile;
  PackmanPacks: TStrings;
  P: PUpdateRec;

  procedure AddGroups(text: String);
  //group text maybe one group or maybe groups separated by comma
  var
    tempSL: TStrings;
    j: Integer;
  begin
    if Pos(',', text) > 0 then
    begin
      tempSL := TStringList.Create;
      tempSL.Text := StringReplace(text, ',', #13#10, [rfReplaceAll]);
      for j := 0 to tempSL.Count-1 do
      begin
        if cmbGroups.Items.IndexOf(tempSL[j]) = -1 then
          cmbGroups.Items.Add(tempSL[j]);
      end;
      tempSL.Free;
    end
    else
    begin
      if cmbGroups.Items.IndexOf(text) = -1 then
        cmbGroups.Items.Add(text);
    end;
  end;

  procedure ReadPackmanPackages(List: TStrings);
  //this procedure parses the packman entry files and returns
  //a list of installed packages in format
  //'AppName=AppVersion'
  var
    tempFiles: TStringList;
    tempIni: TIniFile;
    packName, packVersion: String;
    i: Integer;
  begin
    if List = nil then Exit;

    tempFiles := TStringList.Create;
    FilesFromWildcard(devDirs.Exec + 'Packages',
      '*.entry', tempFiles, False, False, False);

    for i := 0 to tempFiles.Count -1 do
    begin
      packName := '';
      packVersion := '';
      try
        tempIni := TIniFile.Create(tempFiles[i]);
        try
          packName := tempIni.ReadString('Setup', 'AppName', '');
          packVersion := tempIni.ReadString('Setup', 'AppVersion', '');
          if (packName <> '') and (packVersion <> '') then
            List.Add(packName + '=' + packVersion);
        finally
          tempIni.Free;
        end;
      except
      end;
    end;

    tempFiles.Free;
  end;

  function GetPackmanPackVersion(List: TStrings; AppName: String): String;
  //returns AppVersion string, based on given AppName and provided
  //list of Packages as returned by ReadPackmanPackages
  var
    i: Integer;
  begin
    Result := '';
    if List = nil then Exit;

    for i := 0 to List.Count -1 do
    begin
      if Pos(LowerCase(AppName + '='), LowerCase(List[i])) = 1 then
      begin
        Result := Copy(List[i], Length(AppName + '=') + 1,
          Length(List[i]) - Length(AppName + '='));
        Break;
      end;
    end;
  end;

begin
  Result := False;

  ClearList;

  if (not FileExists(Filename)) { or (not FileExists(devDirs.Config + 'devcpp.cfg')) } then
    Exit;

  Ini := TIniFile.Create(Filename);

  LocalIni := TIniFile.Create(devDirs.Config + 'devcpp.cfg');

  PackmanPacks := TStringList.Create;
  ReadPackmanPackages(PackmanPacks);

  sl := TStringList.Create;
  try
    sl.LoadFromFile(Filename);
    Result := (sl.Count > 0) and (sl[0] = MAGIC_HEADER);
    if not Result then
      Exit;
    Ini.ReadSections(sl);
    cmbGroups.Items.Clear;
    cmbGroups.Items.Add(AllGroupsText);
    cmbGroups.ItemIndex := 0;
    for I := 0 to sl.Count - 1 do begin
      P := New(PUpdateRec);
      P^.Name := Ini.ReadString(sl[I], 'Name', '');
      if P^.Name = '' then begin
        P^.Name := sl[I];
      end;      P^.Description := Ini.ReadString(sl[I], 'Description', '');
      P^.RemoteFilename := Ini.ReadString(sl[I], 'RemoteFilename', '');
      P^.LocalFilename := Ini.ReadString(sl[I], 'LocalFilename', '');
      P^.Group := Ini.ReadString(sl[I], 'Group', '');
      P^.InstallPath := Ini.ReadString(sl[I], 'InstallPath', '');
      P^.Version := Ini.ReadString(sl[I], 'Version', '<unknown>');
      P^.LocalVersion := GetPackmanPackVersion(PackmanPacks, P^.Name);
      if P^.LocalVersion = '' then
        P^.LocalVersion := LocalIni.ReadString(WEBUPDATE_SECTION, P^.Name, '');
      P^.Size := Ini.ReadInteger(sl[I], 'Size', 0);
      P^.Date := Ini.ReadString(sl[I], 'Date', '');
      P^.Selected := False;
      P^.Execute := Ini.ReadBool(sl[I], 'Execute', false);
      // verify if the user already has the last update
      if (P^.LocalVersion = P^.Version) then
      begin
        Dispose(P);
        continue;
      end;
      fUpdateList.Add(P);
      AddGroups(P^.Group);
    end;
  finally
    sl.Free;
    Ini.Free;
    LocalIni.Free;
    PackmanPacks.Free;
  end;
  FillList('');
end;

procedure TWebUpdateForm.ClearList;
begin
  while fUpdateList.Count > 0 do begin
    if Assigned(fUpdateList[0]) then
      Dispose(PUpdateRec(fUpdateList[0]));
    fUpdateList.Delete(0);
  end;
end;

procedure TWebUpdateForm.FillList(Filter: string);
var
  I: integer;

  function isInGroup(group, filter: String): Boolean;
  //group may be actual list of groups separated by commas
  var
    j: Integer;
    tempSL: TStrings;
  begin
    if Pos(',', group) > 0 then
    begin
      Result := False;
      tempSL := TStringList.Create;
      tempSL.Text := StringReplace(group, ',', #13#10, [rfReplaceAll]);
      for j := 0 to tempSL.Count -1 do begin
        if tempSL[j] = filter then
        begin
          Result := True;
          Break;
        end;
      end;
      tempSL.Free;
    end
    else
    begin
      Result := (filter = group);
    end;
  end;

begin
  lv.Items.Clear;
  for I := 0 to fUpdateList.Count - 1 do
    if (Filter = '') or (isInGroup(PUpdateRec(fUpdateList[I])^.Group, Filter)) then
      with lv.Items.Add do begin
        Data := fUpdateList[I];
        Caption := PUpdateRec(fUpdateList[I])^.Name;
        Checked := PUpdateRec(fUpdateList[I])^.Selected;
        SubItems.Add(PUpdateRec(fUpdateList[I])^.Version);
        SubItems.Add(PUpdateRec(fUpdateList[I])^.LocalVersion);
        if PUpdateRec(fUpdateList[I])^.Size > 1024 then
          SubItems.Add(Format('%d KB', [PUpdateRec(fUpdateList[I])^.Size div 1024]))
        else if PUpdateRec(fUpdateList[I])^.Size > 0 then
          SubItems.Add(Format('%d Bytes', [PUpdateRec(fUpdateList[I])^.Size]))
        else
          SubItems.Add('Unknown');
        SubItems.Add(PUpdateRec(fUpdateList[I])^.Date);
      end;
end;

procedure TWebUpdateForm.FormCreate(Sender: TObject);
begin
  FormInitialized := False;
end;

procedure TWebUpdateForm.lvClick(Sender: TObject);
var
  I: integer;
begin
  for I := 0 to lv.Items.Count - 1 do
    if Assigned(lv.Items[I].Data) then
      PUpdateRec(lv.Items[I].Data)^.Selected := lv.Items[I].Checked;
  CalcTotalSize;
end;

procedure TWebUpdateForm.lvChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  if Assigned(lv.Selected) and Assigned(lv.Selected.Data) then
    memDescr.Lines.Text := StringReplace(PUpdateRec(lv.Selected.Data)^.Description, '<CR>', #13, [rfReplaceAll])
  else
    memDescr.Lines.Clear;
end;

procedure TWebUpdateForm.cmbGroupsChange(Sender: TObject);
begin
  if cmbGroups.Text = AllGroupsText then
    FillList('')
  else
    FillList(cmbGroups.Text);
end;

procedure TWebUpdateForm.CalcTotalSize;
var
  I: integer;
  Total: cardinal;
  dTotal: double;
  Count: integer;
begin
  Total := 0;
  Count := 0;
  for I := 0 to fUpdateList.Count - 1 do
    if PUpdateRec(fUpdateList[I])^.Selected then begin
      Total := Total + PUpdateRec(fUpdateList[I])^.Size;
      Inc(Count);
    end;

  dTotal := Total / 1.0;
  lblTotalSize.Caption := Format('%d files total, %d KB (%0.0n Bytes)', [Count, Total div 1024, dTotal]);
end;

procedure TWebUpdateForm.DownloadFiles(Sender: TObject);
var
  I, Count: integer;
begin
  if Assigned(wThread) then begin
    MessageDlg('Already downloading!', mtInformation, [mbCancel], 0);
    Exit;
  end;
  Count := 0;
  for I := 0 to fUpdateList.Count - 1 do
    if PUpdateRec(fUpdateList[I])^.Selected then
      Inc(Count);
  if Count = 0 then begin
    MessageDlg('You have not selected anything!', mtError, [mbOk], 0);
    Exit;
  end;
  DisableForm;
  //lv.Enabled := False;

  fErrorsList.Clear;
  wThread := TWebThread.Create(True);
  wThread.OnTerminate := wDownloadTerminate;
  wThread.OnMessage := wThreadMessage;
  wThread.FreeOnTerminate := True;
  wThread.RemoteBase := BASE_URL;
  for I := 0 to fUpdateList.Count - 1 do
    if PUpdateRec(fUpdateList[I])^.Selected then
      wThread.Files.Add(fUpdateList[I]);
  wThread.Resume;
end;

procedure TWebUpdateForm.GetUpdateInfo(Sender: TObject);
var
  P: PUpdateRec;
begin
  if Assigned(wThread) then begin
    MessageDlg('Already checking!', mtInformation, [mbCancel], 0);
    Exit;
  end;

  fUpdateList.Clear;

  P := New(PUpdateRec);
  P^.Name := 'info on available updates';
  P^.RemoteFilename := CONF_FILE;
  fUpdateList.Add(P);

  fErrorsList.Clear;
  wThread := TWebThread.Create(True);
  wThread.OnTerminate := wCheckTerminate;
  wThread.OnMessage := wThreadMessage;
  wThread.FreeOnTerminate := True;
  wThread.RemoteBase := BASE_URL;
  wThread.Files.Add(P);
  wThread.Resume;
end;

procedure TWebUpdateForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TWebUpdateForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  cmbMirrors.Clear;
  cmbGroups.Clear;
  lv.Items.Clear;
  memDescr.Clear;

  ClearList;
  FreeAndNil(fUpdateList);
  FreeAndNil(fMirrorList);
  FreeAndNil(fErrorsList);

  FormInitialized := False;
end;

procedure TWebUpdateForm.UpdateSelf;
var
  SU: TResourceStream;
//  TempPath: array[0..MAX_PATH + 1] of Char;
  TempFile: string;
  ErrMsg: string;
begin
  if fSelfUpdate = '' then
    Exit;
  try
    CopyFile(PChar(Application.ExeName), PChar(ChangeFileExt(Application.ExeName, '.exe.BACKUP')), False);
  except
    MessageDlg('Could not make a backup of devcpp.exe, please verify from your system administrator that you have enough permissions to update Dev-C++.', mtWarning, [mbOK], 0);
    Exit;
  end;
  CopyFile(PChar(fSelfUpdate), PChar(ChangeFileExt(Application.ExeName, '.Update')), False);
  DeleteFile(fSelfUpdate);
  fSelfUpdate := ChangeFileExt(Application.ExeName, '.Update');
  ErrMsg := 'Update failed...'#10 +
    'Please update ''' + ExtractFileName(Application.ExeName) + ''' manually:'#10#10 +
    'Copy ''' + fSelfUpdate + ''' to ''' + Application.ExeName + '''';
  SU := TResourceStream.Create(HInstance, 'SELFUPDATER', 'EXE');
  try
  //  GetTempPath(MAX_PATH + 1, TempPath); // was causing a BSOD under some systems...
    TempFile := 'updater.exe';
    SU.SaveToFile(TempFile);
    Application.ProcessMessages;
    ShellExecute(0,
      'open',
      PChar(TempFile),
      PChar(fSelfUpdate + ' ' + Application.ExeName + ' "Self Updating" "Self-updated succesfully!" "' + ErrMsg + '"'),
      nil,
      SW_HIDE);
  finally
    SU.Free;
  end;
end;

procedure TWebUpdateForm.GetMirrorList;
var
  ini: TIniFile;
  I: integer;
begin
  ini := TIniFile.Create(devDirs.Config + DEV_WEBMIRRORS_FILE);
  try
    ini.ReadSectionValues('WebUpdate mirrors', fMirrorList);
  finally
    ini.Free;
  end;

  // add default mirror if list is empty
  if fMirrorList.Count = 0 then begin
    fMirrorList.Add(DEFAULT_MIRROR);
    fMirrorList.Add(DEFAULT_MIRROR_2);
  end;

  cmbMirrors.Items.Clear;

  // make sure that all URLs end with a slash
  for I := 0 to fMirrorList.Count - 1 do begin
    if (fMirrorList[I] <> '') and (fMirrorList[I][Length(fMirrorList[I])] <> '/') then
      fMirrorList[I] := fMirrorList[I] + '/';
    cmbMirrors.Items.Add(fMirrorList.Names[I]);
  end;

  if cmbMirrors.Items.Count > 0 then
    cmbMirrors.ItemIndex := 0;
  BASE_URL := fMirrorList.Values[cmbMirrors.Text];
end;

procedure TWebUpdateForm.cmbMirrorsChange(Sender: TObject);
var
  lastSelMirror: Integer;
  lastMirrorList: String;
  tmpAction: TCloseAction;
begin
  with cmbMirrors do
  begin
    lastSelMirror := ItemIndex;
    lastMirrorList := Items.Text;

    FormClose(Self, tmpAction);
    FormShow(Self);

    Items.Text := lastMirrorList;
    ItemIndex := lastSelMirror;
  end;

  BASE_URL := fMirrorList.Values[cmbMirrors.Text];
end;

function TWebUpdateForm.ReplaceMacros(Source: string): string;
var
  idx1, idx2: integer;
  PropName: string;
  PropValue: string;
begin
  Result := Source;
  repeat
    idx1 := Pos('{', Result) + 1;
    idx2 := Pos('}', Result) - 1;
    if (idx1 > 0) and (idx2 > idx1) then begin
      PropName := Copy(Result, idx1, idx2 - idx1 + 1);
      if IsPublishedProp(TdevDirs(devDirs), PropName) then
        PropValue := GetStrProp(TdevDirs(devDirs), PropName)
      else if PropName = 'OriginalPath' then
        PropValue := ExtractFilePath(Application.ExeName)
      else
        Break;
      Result := StringReplace(Result, '{' + PropName + '}', PropValue, [rfReplaceAll]);
    end
    else
      Break;
  until False;
  Result := StringReplace(Result, '\\', '\', [rfReplaceAll]);
end;

procedure TWebUpdateForm.EnableForm;
begin
  cmbMirrors.Enabled := true;
  cmbGroups.Enabled := true;
  lv.Enabled := true;
  btnCheck.Enabled := true;
end;

procedure TWebUpdateForm.DisableForm;
begin
  cmbMirrors.Enabled := false;
  cmbGroups.Enabled := false;
  lv.Enabled := false;
  btnCheck.Enabled := false;
end;

procedure TWebUpdateForm.CreateParams(var params: TCreateParams);
begin
  inherited;
  params.ExStyle := params.ExStyle or WS_EX_APPWINDOW;
  params.WndParent := GetDesktopWindow;
end;

function TWebUpdateForm.GetSelected(index: integer): boolean;
var
  i: integer;
begin
  Result := False;
  with lv.Items do
    for i := 0 to Count - 1 do
      if Item[i].Data = PUpdateRec(fUpdateList[index]) then begin
        Result := Item[i].Checked;
        break;
      end;
end;

procedure TWebUpdateForm.lvColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  if ColumnToSort = Column.Index then
    SortAscending := Not SortAscending
  else begin
    ColumnToSort := Column.Index;
    SortAscending := True;
  end;
  (Sender as TCustomListView).AlphaSort;
end;

procedure TWebUpdateForm.lvCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
var
  i: Integer;
  text1, text2, tempText: String;

  function getSizeInBytes(TheItem: TListItem): String;
  var
    i: Integer;
  begin
    if Assigned(TheItem.Data) then
    begin
      Result := IntToStr(PUpdateRec(TheItem.Data)^.Size);
      //add leading zeros
      i := 9 - Length(Result);
      Result := StringOfChar('0', i) + Result;
    end
    else
      Result := 'Unknown';
  end;
begin
  if ColumnToSort = 0 then
  begin
    text1 := Item1.Caption;
    text2 := Item2.Caption;
  end
  else if ColumnToSort = 3 then
  begin
    //special arrangements for File size column - size must be in bytes
    text1 := getSizeInBytes(Item1);
    text2 := getSizeInBytes(Item2);
  end
  else
  begin
   i := ColumnToSort - 1;
   text1 := Item1.SubItems[i];
   text2 := Item2.SubItems[i];
  end;
  if Not SortAscending then
  begin
    tempText := text2;
    text2 := text1;
    text1 := tempText;
  end;

  Compare := CompareText(text1, text2);
end;

procedure TWebUpdateForm.lvKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
{$IFDEF WIN32}
  if Key = VK_SPACE then
{$ENDIF}
{$IFDEF LINUX}
  if Key = XK_SPACE then
{$ENDIF}
    lvClick(Sender);
end;

procedure TWebUpdateForm.FormShow(Sender: TObject);
begin
  if Not FormInitialized then
  begin

    CheckInstalledExes;

    fUpdateList := TList.Create;
    fMirrorList := TStringList.Create;
    fErrorsList := TStringList.Create;
    ProgressBar1.Position := 0;
    lblStatus.Caption := 'Disconnected';
    CalcTotalSize;
    btnCheck.Caption := 'Check for &updates';
    btnCheck.OnClick := GetUpdateInfo;
    fSelfUpdate := '';
    cmbGroups.Items.Add(AllGroupsText);
    cmbGroups.ItemIndex := 0;
    GetMirrorList;
    memDescr.Lines.Add('Press "Check for updates" to request a list of available updates on the server.');

    FormInitialized := True;
  end;
end;

procedure TWebUpdateForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if Assigned(wThread) then begin
    MessageDlg('Please wait until download is complete.', mtInformation, [mbCancel], 0);
    CanClose := False;
  end;
end;

procedure TWebUpdateForm.CheckInstalledExes;
var
  devcppversion,
  packmanversion,
  devcppversion2,
  packmanversion2: String;
begin
//this procedure checks versions of installed components
//that are not handled by packman:
//namely devcpp.exe and packman.exe
//and writes those version to devcpp.cfg if are newer that in devcpp.cfg

  if Not FileExists(devDirs.Config + 'devcpp.cfg') then
    Exit;

  with TIniFile.Create(devDirs.Config + 'devcpp.cfg') do
    try
      devcppversion2 := ReadString(WEBUPDATE_SECTION, 'Dev-C++ Update', '');
      packmanversion2 := ReadString(WEBUPDATE_SECTION, 'PackMan', '');
    finally
      Free;
    end;

  devcppversion := GetVersionString(ParamStr(0));
  if (devcppversion <> '') and (devcppversion <> devcppversion2) then
    with TIniFile.Create(devDirs.Config + 'devcpp.cfg') do
      try
        WriteString(WEBUPDATE_SECTION, 'Dev-C++ Update', devcppversion);
      finally
        Free;
      end;

  packmanversion := GetVersionString(IncludeTrailingPathDelimiter(devDirs.Exec) + PACKMAN_PROGRAM);
  if (packmanversion <> '') and (packmanversion <> packmanversion2) then
    with TIniFile.Create(devDirs.Config + 'devcpp.cfg') do
      try
        WriteString(WEBUPDATE_SECTION, 'PackMan', packmanversion);
      finally
        Free;
      end;
end;

end.


