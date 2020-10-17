unit Main;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, ComCtrls, ExtCtrls, ToolWin, Buttons,
  ShellAPI, ImgList, IniFiles, System.ImageList, Vcl.BaseImageCollection,
  Vcl.ImageCollection, Vcl.VirtualImageList, SVGIconImageList,
  SVGIconImageListBase, SVGIconVirtualImageList, SVGIconImageCollection, SVGColor,
  Vcl.Styles.Hooks,
  Vcl.Styles.Utils.Menus, //Style Popup and Shell Menus (class #32768)
  Vcl.Styles.Utils.Forms, //Style dialogs box (class #32770)
  Vcl.Styles.Utils.StdCtrls, //Style buttons, static, and so on
  Vcl.Styles.Utils.ComCtrls, //Style SysTreeView32, SysListView32
  Vcl.Styles.Utils.ScreenTips, //Style the tooltips_class32 class
  Vcl.Styles.Utils.SysControls,
  Vcl.Styles.Utils.SysStyleHook
  ;
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
    Statusbar1: TStatusbar;
    Panel2: TPanel;
    Label1: TLabel;
    SpeedButton1: TSpeedButton;
    devPages1: TPageControl;
    devPage1: TTabSheet;
    devPage2: TTabSheet;
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
    StylesMenu: TMenuItem;
    VirtualImageList: TVirtualImageList;
    ImageCollection: TImageCollection;
    SVGImageCollection: TSVGIconImageCollection;
    SVGVirtualImageList: TSVGIconVirtualImageList;
    SVGImageList: TSVGIconImageList;
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
    procedure OnStyleClick(Sender: TObject);
  private
    FStyleName: String;
    procedure AddStylesToMenu;
    procedure AppHint(Sender: TObject);
    procedure Reload;
    procedure SetStyle(SelStyleName: string);
    procedure ReadDevCStyle(styleparam: string);
    procedure SetColorIconTheme;
  public
    { Public declarations }
  end;
  const
    cDelphiStyle: array[0..3] of string = ('Windows', 'Windows10','Windows10 SlateGray','Windows10 Blue Whale');
    cSVGColor: array[0..3] of string = ('None', 'clGrayText','clMoneyGreen','clSkyBlue'); //

var
  MainForm: TMainForm;

implementation

uses
  InstallWizards, PackmanUtils, RemoveForms, VerifyForms, AboutForms,
  PackmanExitCodesU, Vcl.Themes; //, Config;

{$R *.dfm}

procedure TMainForm.Label5Click(Sender: TObject);
begin
  if Length(PackageURL.Text) > 0 then
      ShellExecuteA(GetDesktopWindow, nil, PAnsiChar(PackageURL.Text), nil, nil, 0);
end;

procedure TMainForm.OnStyleClick(Sender: TObject);
  var SelStyleName : string;
begin
  SelStyleName :=  TMenuItem(Sender).Caption.Replace('&', '');
  SetStyle(SelStyleName);
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
      Statusbar1.Panels.Items[0].Text := Application.Hint
  else
      Statusbar1.Panels.Items[0].Text := 'Ready.';
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  FileName: String;
  OptionAuto: Boolean;
  OptionUninstall: Boolean;
  OptionQuiet: Boolean;
  OptionStyle: Boolean;
  OptionReqVersion: String;
  i: Integer;
  //StyleName: string;
  StyleParam:string;
begin
  AddStylesToMenu;

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
  OptionStyle := False;
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
    else if CompareText(ParamStr(i), '/style') = 0 then
      OptionStyle := True
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
  else if OptionStyle then
  begin
    StyleParam := FileName;
  end
  else if OptionAuto then
    Halt(PACKMAN_EXITCODE_FILE_NOT_FOUND);

  Reload;

  ReadDevCStyle(StyleParam);
  //if FStyleName = '' then
    //FStyleName := ConfigPackman.GetStyle;
  //SetStyle(StyleName);
  //MessageDlg(FStyleName, mtInformation, [mbOK], 0);
end;

procedure TMainForm.PackagesSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  i: Integer;
  FileName: PAnsiChar;
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
  FileName: PAnsiChar;
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
     Entry := PAnsiChar(Packages.Selected.Data);
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
     Entry := PAnsiChar(Packages.Selected.Data);
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

procedure TMainForm.AddStylesToMenu;
var
  Style: string;
  Item : TMenuItem;
begin
  for Style in TStyleManager.StyleNames do begin
    Item := TMenuItem.Create(MainMenu1);
    Item.Caption := Style;
    Item.OnClick := OnStyleClick;
    if TStyleManager.ActiveStyle.Name = Style then
      Item.Checked := TRUE;
    StylesMenu.Add(Item);
  end;

end;

procedure TMainForm.Help2Click(Sender: TObject);
begin
  Application.MessageBox(
    'Package Manager is a tool to install and manage add-on ' + #13#10 +
    'libraries for Embarcadero Dev-C++.' + #13#10#13#10 +
    'To install a library, click on the Install button and select a' + #13#10 +
    '.DevPak file or .DevPackage file, and follow the instructions.' + #13#10 +
    'To uninstall an installed library, select it and click on the' + #13#10 +
    'Remove button.',
    'Help', MB_ICONINFORMATION);
end;

procedure TMainForm.SetStyle(SelStyleName: string);
begin
  if TStyleManager.TrySetStyle(SelStyleName) then begin
    for var i := 0 to StylesMenu.Count - 1 do
      StylesMenu.Items[i].Checked := StylesMenu.Items[i].Caption.Replace('&', '').Equals(SelStyleName);

    //ConfigPackman.SetStyle(SelStyleName);
  end;
end;

procedure TMainForm.ReadDevCStyle(styleparam: string);
var
  Style: Integer;
  StyleName: string;
  IconColor:string;
begin
    if styleparam = '' then
      Style := 0
    else
    begin
      try
        Style := StrToInt(styleparam);
      except
      end;
    end;
    case Style of
      0: begin
        StyleName := 'Windows';
        SetColorIconTheme;
        end;
      1: begin
        StyleName := 'Windows10';
        IconColor := 'clGrayText';
      end;
      2: begin
        StyleName := 'Windows10 SlateGray';
        IconColor := 'clMoneyGreen';
      end;
      3: begin
        StyleName := 'Windows10 Blue Whale';
        IconColor := 'clSkyBlue';
      end;
      else begin
        StyleName := 'Windows10';
        IconColor := 'clGrayText';
      end;
    end;

  TStyleManager.TrySetStyle(StyleName);
  if Style > 0 then
  begin
    SVGVirtualImageList.FixedColor := StringToColor(cSVGColor[Style]);
    SVGImageList.FixedColor := StringToColor(cSVGColor[Style]);
  end;
end;

procedure TMainForm.SetColorIconTheme;
begin
        ToolBar1.Images := VirtualImageList;
        Packages.LargeImages := ListImages;
        InstallBtn.ImageIndex := 0;
        VerifyBtn.ImageIndex := 10;
        RemoveBtn.ImageIndex := 1;
        HelpButton.ImageIndex := 4;
        AboutBtn.ImageIndex := 2;
        ExitBtn.ImageIndex := 8;
        MainMenu1.images := VirtualImageList;
        InstallPackage1.ImageIndex :=  0;
        Verify1.ImageIndex := 10;
        Remove1.ImageIndex := 1;
        ReloadDatabase1.ImageIndex := 11;
        Exit1.ImageIndex := 8;
        Help2.ImageIndex := 4;
        About1.ImageIndex := 9;
end;

end.
