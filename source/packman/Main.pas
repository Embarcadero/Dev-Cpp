unit Main;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, ComCtrls, ExtCtrls, ToolWin, Buttons, devTabs,
  ShellAPI, ImgList, IniFiles;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Variants, Classes, QGraphics, QControls, QForms,
  QDialogs, QMenus, QStdCtrls, QComCtrls, QExtCtrls, ToolWin, QButtons, devTabs,
  QImgList, IniFiles;
{$ENDIF}

type
  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    Package1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    Exit1: TMenuItem;
    InstallPackage1: TMenuItem;
    N1: TMenuItem;
    ToolBar1: TToolBar;
    DetailsPanel: TPanel;
    Splitter1: TSplitter;
    Packages: TListView;
    StatusBar1: TStatusBar;
    Panel2: TPanel;
    Label1: TLabel;
    SpeedButton1: TSpeedButton;
    devPages1: TdevPages;
    devPage1: TdevPage;
    devPage2: TdevPage;
    FileList: TRichEdit;
    Label2: TLabel;
    PackageName: TEdit;
    PackageVersion: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    PackageDescription: TMemo;
    Label5: TLabel;
    PackageURL: TEdit;
    BitBtn1: TBitBtn;
    InstallBtn: TToolButton;
    ToolbarImages: TImageList;
    ExitBtn: TToolButton;
    ToolButton1: TToolButton;
    AboutBtn: TToolButton;
    ToolButton2: TToolButton;
    View1: TMenuItem;
    Details1: TMenuItem;
    RemoveBtn: TToolButton;
    N2: TMenuItem;
    ListImages: TImageList;
    OpenDialog1: TOpenDialog;
    PopupMenu1: TPopupMenu;
    Copy1: TMenuItem;
    MenuImages: TImageList;
    Verify1: TMenuItem;
    Remove1: TMenuItem;
    VerifyBtn: TToolButton;
    N3: TMenuItem;
    ToolbarMenu1: TMenuItem;
    N4: TMenuItem;
    ReloadDatabase1: TMenuItem;
    Help2: TMenuItem;
    N5: TMenuItem;
    HelpButton: TToolButton;
    procedure Label5Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure View1Click(Sender: TObject);
    procedure Details1Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure InstallPackage1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PackagesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure Copy1Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure ToolbarMenu1Click(Sender: TObject);
    procedure Remove1Click(Sender: TObject);
    procedure ReloadDatabase1Click(Sender: TObject);
    procedure Verify1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure Help2Click(Sender: TObject);
  private
    procedure AppHint(Sender: TObject);
    procedure Reload;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  InstallWizards, PackmanUtils, RemoveForms, VerifyForms, AboutForms,
  PackmanExitCodesU;

{$R *.dfm}

procedure TMainForm.Label5Click(Sender: TObject);
begin
  if Length(PackageURL.Text) > 0 then
      ShellExecute(GetDesktopWindow, nil, PChar(PackageURL.Text), nil, nil, 0);
end;

procedure TMainForm.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.View1Click(Sender: TObject);
begin
  Details1.Checked := DetailsPanel.Visible;
end;

procedure TMainForm.Details1Click(Sender: TObject);
begin
  DetailsPanel.Visible := Details1.Checked;
end;

procedure TMainForm.SpeedButton1Click(Sender: TObject);
begin
  DetailsPanel.Hide;
end;

procedure TMainForm.InstallPackage1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
      InstallWizard := TInstallWizard.Create(Self);
      with InstallWizard do
      try
         Self.Hide;
         Application.ProcessMessages;
         if SetFileName(OpenDialog1.FileName) then
             ShowModal
         else if not DontShowError then
             Application.MessageBox('An error has occured.' + #13#10 +
               'The selected installation file may be damaged or invalid.',
               'Error', MB_ICONHAND);
      finally
         Self.Show;
         Free;
      end;
  end;
  Reload;
end;

procedure TMainForm.AppHint(Sender: TObject);
begin
  if Length(Application.Hint) > 0 then
      StatusBar1.Panels.Items[0].Text := Application.Hint
  else
      StatusBar1.Panels.Items[0].Text := 'Ready.';
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  FileName: String;
  OptionAuto: Boolean;
  OptionUninstall: Boolean;
  OptionQuiet: Boolean;
  OptionReqVersion: String;
  i: Integer;
begin
  Randomize;
  Application.OnHint := AppHint;

// parse command line
// packman [/auto] [/quiet] [[file] | [/uninstall file [/version number]]]
//   /auto - close packman after finishing
//   /quiet - don't require the user to press any buttons
//   file - filename of the devpak
//   /uninstall file - filename of the *.entry of the devpak to uninstall
//     /version number - check the version number of the *.entry to uninstall (AppVersion)

  OptionAuto := False;
  OptionUninstall := False;
  OptionQuiet := False;
  FileName := '';
  OptionReqVersion := '';

  i := 1;
  while i <= ParamCount do
  begin
    if CompareText(ParamStr(i), '/auto') = 0 then
      OptionAuto := True
    else if CompareText(ParamStr(i), '/uninstall') = 0 then
      OptionUninstall := True
    else if CompareText(ParamStr(i), '/quiet') = 0 then
      OptionQuiet := True
    else if (CompareText(ParamStr(i), '/version') = 0)
      and (i + 1 <= ParamCount) then
    begin
      OptionReqVersion := ParamStr(i + 1);
      i := i + 1;
    end
    else
      FileName := ParamStr(i);
    i := i + 1;
  end;
// parsing done

  if Not OptionUninstall and FileExists(FileName) then
  begin
    InstallWizard := TInstallWizard.Create(Self);
    with InstallWizard do
    try
      Application.ProcessMessages;
      Quiet := OptionQuiet;
      if SetFileName(FileName) then
      begin
        if Quiet then
          Close
        else
          ShowModal;
        if OptionAuto then
          Halt(PMExitCode);
      end
      else
      begin
        if (not DontShowError) and (not OptionAuto) then
          Application.MessageBox('An error has occured.' + #13#10 +
            'The selected installation file may be damaged or invalid.',
            'Error', MB_ICONHAND);

        if OptionAuto then
          Halt(PMExitCode);
      end;
    finally
      Free;
    end;
  end
  else if OptionUninstall and FileExists(FileName) then
  begin
    RemoveForm := TRemoveForm.Create(Self);
    with RemoveForm do
    try
      Entry := FileName;
      ReqVersion := OptionReqVersion;
      if OptionQuiet then
        CloseWhenDone := True;
      ShowModal;
    finally
      Free;
      if OptionAuto then
        Halt(PACKMAN_EXITCODE_NO_ERROR);
    end;
  end
  else if OptionAuto then
    Halt(PACKMAN_EXITCODE_FILE_NOT_FOUND);

  Reload;
end;

procedure TMainForm.PackagesSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  i: Integer;
  FileName: PChar;
  Ini: TIniFile;
  IniFile: TStringList;
begin
  for i := 0 to devPage1.ControlCount - 1 do
      devPage1.Controls[i].Enabled := Selected;
  for i := 0 to devPage2.ControlCount - 1 do
      devPage2.Controls[i].Enabled := Selected;
  RemoveBtn.Enabled := Selected;
  Remove1.Enabled := Selected;
  VerifyBtn.Enabled := Selected;
  Verify1.Enabled := Selected;
  if not Selected then Exit;

  FileName := Item.Data;
  if not FileExists(FileName) then
  begin
      Reload;
      Exit;
  end;

  Ini := TIniFile.Create(FileName);
  PackageName.Text := Ini.ReadString('Setup', 'AppName', 'Package');
  PackageVersion.Text := Ini.ReadString('Setup', 'AppVersion', '1.0');
  PackageDescription.Text := Ini.ReadString('Setup', 'Description', '');
  PackageURL.Text := Ini.ReadString('Setup', 'Url', '');
  Label5.Visible := Length(PackageURL.Text) > 0;
  PackageURL.Visible := Length(PackageURL.Text) > 0;

  IniFile := TStringList.Create;
  IniFile.LoadFromFile(FileName);
  for i := 0 to IniFile.Count - 1 do
  begin
      if CompareText(IniFile.Strings[0], '[Files]') = 0 then
      begin
          IniFile.Delete(0);
          Break;
      end else
          IniFile.Delete(0);
  end;
  FileList.Lines.Text := IniFile.Text;
  FileList.SelStart := 0;
  IniFile.Free;

  Ini.Free;
end;

procedure TMainForm.Reload;
var
  Files: TStringList;
  Ini: TIniFile;
  Item: TListItem;
  i: Integer;
  FileName: PChar;
begin
  Packages.Items.BeginUpdate;
  while Packages.Items.Count > 0 do
  begin
      FreeMem(Packages.Items[0].Data);
      Packages.Items[0].Delete;
  end;

  Files := TStringList.Create;
  FilesFromWildcard(ExtractFilePath(ParamStr(0)) + 'Packages', '*.entry',
    Files, False, False);

  for i := 0 to Files.Count - 1 do
  begin
      Ini := TIniFile.Create(Files.Strings[i]);
      Item := Packages.Items.Add;
      Item.Caption := Ini.ReadString('Setup', 'AppName', 'Package');
      Item.ImageIndex := 0;

      FileName := GetMemory(Length(Files.Strings[i]) + 1);
      FileName := StrPLCopy(FileName, Files.Strings[i],
        Length(Files.Strings[i]));
      FileName[Length(Files.Strings[i])] := #0;
      Item.Data := FileName;
      Ini.Free;
  end;
  Files.Free;
  Packages.Items.EndUpdate;
end;

procedure TMainForm.Copy1Click(Sender: TObject);
begin
  FileList.CopyToClipboard;
end;

procedure TMainForm.PopupMenu1Popup(Sender: TObject);
begin
  Copy1.Enabled := FileList.SelLength > 0;
end;

procedure TMainForm.ToolbarMenu1Click(Sender: TObject);
begin
  ToolbarMenu1.Checked := not ToolbarMenu1.Checked;
  Toolbar1.Visible := ToolbarMenu1.Checked;
end;

procedure TMainForm.Remove1Click(Sender: TObject);
begin
  if Application.MessageBox('Are you sure you want to remove this package ' +
    'and all associated files?', 'Remove confirmation',
    MB_ICONQUESTION + MB_YESNO) <> 6 then
      Exit;

  RemoveForm := TRemoveForm.Create(Self);
  with RemoveForm do
  try
     Entry := PChar(Packages.Selected.Data);
     ShowModal;
  finally
     Free;
  end;
  Reload;
end;

procedure TMainForm.ReloadDatabase1Click(Sender: TObject);
begin
  Reload;
end;

procedure TMainForm.Verify1Click(Sender: TObject);
begin
  VerifyForm := TVerifyForm.Create(Self);
  with VerifyForm do
  try
     Entry := PChar(Packages.Selected.Data);
     ShowModal;
  finally
     Free;
  end;
end;

procedure TMainForm.About1Click(Sender: TObject);
begin
  AboutForm := TAboutForm.Create(Self);
  with AboutForm do
  try
     ShowModal;
  finally
     Free;
  end;
end;

procedure TMainForm.Help2Click(Sender: TObject);
begin
  Application.MessageBox(
    'Package Manager is a tool to install and manage add-on ' + #13#10 +
    'libraries for Dev-C++.' + #13#10#13#10 +
    'To install a library, click on the Install button and select a' + #13#10 +
    '.DevPak file or .DevPackage file, and follow the instructions.' + #13#10 +
    'To uninstall an installed library, select it and click on the' + #13#10 +
    'Remove button.',
    'Help', MB_ICONINFORMATION);
end;

end.
