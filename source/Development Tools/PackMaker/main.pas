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

unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, Buttons, ComCtrls, ImgList, libTar, BZip2, IniFiles,
  ExtDlgs, ToolWin, ExtCtrls, StrUtils;

const SETUP_SECTION  = 'Setup';
const FILES_SECTION  = 'Files';
const ICONS_SECTION  = 'Icons';
const RECURSE        = ';recursive';
const DEVPAK_VERSION = '2';

type
  TFileItem = class(TObject)
    public
      Source : string;
      Dest   : string;
      IsDir  : boolean;
      Node   : TListItem;
  end;

  TIconItem = class(TObject)
    public
      Name   : string;
      Target : string;
      Icon   : string;
      Node   : TListItem;
  end;

  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    FileItem: TMenuItem;
    NewItem: TMenuItem;
    OpenItem: TMenuItem;
    N1: TMenuItem;
    SaveItem: TMenuItem;
    SaveAsItem: TMenuItem;
    N2: TMenuItem;
    ExitItem: TMenuItem;
    BuildItem: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    ImageList: TImageList;
    IconImageList: TImageList;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    OpenPictureDialog: TOpenPictureDialog;
    MainImageList: TImageList;
    Panel1: TPanel;
    gbSetupInfo: TGroupBox;
    lblName: TLabel;
    lblNameVersion: TLabel;
    lblVersion: TLabel;
    edName: TEdit;
    edNameVersion: TEdit;
    edVersion: TEdit;
    gbStartMenuIcons: TGroupBox;
    AddItemBtn: TSpeedButton;
    EditItemBtn: TSpeedButton;
    RemoveIconBtn: TSpeedButton;
    IconView: TListView;
    gbFilesToInstall: TGroupBox;
    AddDirBtn: TSpeedButton;
    AddFileBtn: TSpeedButton;
    RemoveBtn: TSpeedButton;
    FileView: TListView;
    gbOptionalInfo: TGroupBox;
    lblWebsite: TLabel;
    lblReadMe: TLabel;
    lblLicense: TLabel;
    lblPicture: TLabel;
    ReadMeBtn: TSpeedButton;
    LicenseBtn: TSpeedButton;
    PictureBtn: TSpeedButton;
    lblDescription: TLabel;
    lblDepend: TLabel;
    edURL: TEdit;
    edReadMe: TEdit;
    edLicense: TEdit;
    edPicture: TEdit;
    edDescription: TEdit;
    chbReboot: TCheckBox;
    edDepend: TEdit;
    ToolBar: TToolBar;
    NewBtn: TToolButton;
    OpenBtn: TToolButton;
    ToolButton3: TToolButton;
    SaveBtn: TToolButton;
    SaveAsBtn: TToolButton;
    ToolButton6: TToolButton;
    BuildBtn: TToolButton;
    ToolButton8: TToolButton;
    AboutBtn: TToolButton;
    Label1: TLabel;
    edStartMenu: TEdit;
    N3: TMenuItem;
    procedure ExitItemClick(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure ReadMeBtnClick(Sender: TObject);
    procedure LicenseBtnClick(Sender: TObject);
    procedure PictureBtnClick(Sender: TObject);
    procedure NewItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SaveItemClick(Sender: TObject);
    procedure SaveAsItemClick(Sender: TObject);
    procedure BuildItemClick(Sender: TObject);
    procedure OpenItemClick(Sender: TObject);
    procedure AddItemBtnClick(Sender: TObject);
    procedure EditItemBtnClick(Sender: TObject);
    procedure RemoveIconBtnClick(Sender: TObject);
    procedure RemoveBtnClick(Sender: TObject);
    procedure AddDirBtnClick(Sender: TObject);
    procedure AddFileBtnClick(Sender: TObject);
  private
    { Private declarations }
    function GetSelectedIcon : TIconItem;
    function GetSelectedFile : TFileItem;
    procedure GetDirFiles(s : string; var sl : TStringList);
  public
    FileName : string;
    IniFile  : TIniFile;
    FileList : TList;
    IconList : TList;

    procedure WriteDevPackFile;
    procedure ReadDevPackFile;
    procedure OpenDevPackFile(devpakfile: String);
    function CreateNewFile: Boolean;
    procedure Clear;
    procedure BuildPackage;
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  menufrm, filefrm, buildfrm, actionfrm;

{$R *.dfm}

procedure FilesFromWildcard(Directory, Mask: String;
  var Files : TStringList; Subdirs, ShowDirs, Multitasking: Boolean);
var
  SearchRec: TSearchRec;
  Attr, Error: Integer;
begin
  if (Directory[Length(Directory)] <> '\') then
     Directory := Directory + '\';

  { First, find the required file... }
  Attr := faAnyFile;
  if ShowDirs = False then
     Attr := Attr - faDirectory;
  Error := FindFirst(Directory + Mask, Attr, SearchRec);
  if (Error = 0) then
  begin
     while (Error = 0) do
     begin
     { Found one! }
        if (SearchRec.Name <> '..') and  // remove uncessary dir refs
           (SearchRec.Name <> '.') then begin
          if (SearchRec.Attr and faDirectory) > 0 then
            Files.Add(Directory + SearchRec.Name + '\')
          else
            Files.Add(Directory + SearchRec.Name);
        end;
        Error := FindNext(SearchRec);
        if Multitasking then
           Application.ProcessMessages;
     end;
     FindClose(SearchRec);
  end;

  { Then walk through all subdirectories. }
  if Subdirs then
  begin
     Error := FindFirst(Directory + '*.*', faAnyFile, SearchRec);
     if (Error = 0) then
     begin
        while (Error = 0) do
        begin
           { Found one! }
           if (SearchRec.Name[1] <> '.') and (SearchRec.Attr and
             faDirectory <> 0) then
              { We do this recursively! }
              FilesFromWildcard(Directory + SearchRec.Name, Mask, Files,
                Subdirs, ShowDirs, Multitasking);
           Error := FindNext(SearchRec);
        end;
     FindClose(SearchRec);
     end;
  end;
end;

procedure TMainForm.ExitItemClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMainForm.About1Click(Sender: TObject);
begin
  MessageDlg('Dev-C++ Package Maker' + #13#10 + #13#10 +
             'Author : Colin Laplace and Hongli Lai' + #13#10 +
             'Tar creation code : Stefan Heymann' + #13#10 +
             'Bzip2 compression code : Julian Seward and Edison Mera Menéndez' + #13#10 + #13#10 +
             'Copyright Bloodshed Software' + #13#10 +
             'Under the GNU General Public License', mtInformation, [mbOK], 0);
end;

procedure TMainForm.ReadMeBtnClick(Sender: TObject);
begin
  with OpenDialog do begin
    Filter := 'Text files (*.txt)|*.txt';
    if Execute then
      edReadMe.Text := ExtractRelativePath(self.FileName, FileName);
  end;
end;

procedure TMainForm.LicenseBtnClick(Sender: TObject);
begin
  with OpenDialog do begin
    Filter := 'Text files (*.txt)|*.txt';
    if Execute then
      edLicense.Text := ExtractRelativePath(self.FileName, FileName);
  end;
end;

procedure TMainForm.PictureBtnClick(Sender: TObject);
begin
  with OpenPictureDialog do begin
    Filter := 'Bitmaps (*.bmp)|*.bmp';
    if Execute then
      edPicture.Text := ExtractRelativePath(self.FileName, FileName);
  end;
end;

procedure TMainForm.NewItemClick(Sender: TObject);
begin
  Clear;
  CreateNewFile;
end;

procedure TMainForm.Clear;
begin
  FileName := '';
  if Assigned(IniFile) then
    IniFile.Free;
  edName.Text := '';
  edNameVersion.Text := '';
  edVersion.Text := '';
  edStartMenu.Text := '';

  edDescription.Text := '';
  edReadMe.Text := '';
  edLicense.Text := '';
  edPicture.Text := '';
  edDepend.Text := '';
  chbReboot.Checked := false;

  IconView.Items.Clear;
  FileView.Items.Clear;
  FileList.Clear;
  IconList.Clear;
end;

function TMainForm.CreateNewFile: Boolean;
begin
  Result := False;
  while not SaveDialog.Execute do
      if Application.MessageBox(
        'You must create the file before starting because filenames you ' +
        'will use later will be relative to that file.', 'Error',
        MB_RETRYCANCEL + MB_ICONEXCLAMATION) = 2 then
          Exit;

  FileName := SaveDialog.FileName;
  OpenDevPackFile(FileName);
  Result := True;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FileName := '';
  IniFile := nil;
  FileList := TList.Create;
  IconList := TList.Create;

  //if parameter given - try to open it as a file
  if ParamCount > 0 then
    if FileExists(ParamStr(1)) then
    begin
      SetCurrentDir(ExtractFileDir(ParamStr(1)));
      OpenDevPackFile(ParamStr(1));
      Exit;
    end;

  //else display new/open dialog
  ActionForm := TActionForm.Create(Self);
  with ActionForm do
  try
      case Start of
      -1: Application.Terminate; // cancel
      0 : if not CreateNewFile then Application.Terminate; // new
      1 : begin // open
              OpenItemClick(Sender);
              if FileName = '' then
                  Application.Terminate;
          end;
      end;
  finally
      Free;
  end;
end;

procedure TMainForm.SaveItemClick(Sender: TObject);
begin
  if FileName = '' then
    SaveAsItemClick(sender)
  else
    WriteDevPackFile;
end;

procedure TMainForm.SaveAsItemClick(Sender: TObject);
begin
  if SaveDialog.Execute then begin
    FileName := SaveDialog.FileName;
    if Assigned(IniFile) then
      IniFile.Free;
    IniFile := TIniFile.Create(FileName);
    SaveItemClick(sender);
  end;
end;

procedure TMainForm.GetDirFiles(s : string; var sl : TStringList);
begin
  FilesFromWildCard(s, '*.*', sl, true, true, true);
end;

procedure TMainForm.BuildPackage;
const BufferSize = 1024 * 32;
var tw      : TTarWriter;
    i, j, k : integer;
    sl      : TStringList;
    tarstr  : string;
    bz2     : TBZCompressionStream;
    bzfile  : TFileStream;
    tarfile : TFileStream;
    Buffer  : array [0..BufferSize] of char;
begin
  // Make TAR
  BuildForm.GroupBox.Caption := 'Building tar archive...';
  BuildForm.ProgressBar.StepIt;
  Application.ProcessMessages;

  tarstr := ChangeFileExt(FileName, '.tar');
  tw := TTarWriter.Create(tarstr);
  tw.AddFile(ExtractFileName(FileName));
  for i := 0 to FileList.Count - 1 do begin
    if TFileItem(FileList[i]).IsDir then begin
      sl := TStringList.Create;
      GetDirFiles(TFileItem(FileList[i]).Source, sl);
      tw.AddDir(TFileItem(FileList[i]).Source, Date);
      for j := 0 to sl.Count - 1 do begin
        if DirectoryExists(sl[j]) then
          tw.AddDir(sl[j], Date)
        else if (ExtractFileName(sl[j]) <> ExtractFileName(FileName)) and
                (ExtractFileName(sl[j]) <> ExtractFileName(tarstr)) then
          tw.AddFile(sl[j]);
      end;
      sl.Free;
    end
    else
      tw.AddFile(TFileItem(FileList[i]).Source);
  end;
  if edReadMe.Text <> '' then begin
    if FileExists(edReadMe.Text) then
      tw.AddFile(edReadMe.Text)
    else
      MessageDlg('Your readme file doesn''t exist! You will need to correct it and build again the package', mtWarning, [mbOK], 0);
  end;
  if edLicense.Text <> '' then begin
    if FileExists(edLicense.Text) then
      tw.AddFile(edLicense.Text)
    else
      MessageDlg('Your license file doesn''t exist! You will need to correct it and build again the package', mtWarning, [mbOK], 0);
  end;
  tw.Finalize;
  tw.Free;

  // Make Bzip2
  BuildForm.GroupBox.Caption := 'Building Bzip2 archive...';
  BuildForm.ProgressBar.StepBy(4);
  Application.ProcessMessages;
  bzfile := TFileStream.Create(ChangeFileExt(FileName, '.DevPak'), fmCreate);
  tarfile := TFileStream.Create(tarstr, fmOpenRead);
  bz2 := TBZCompressionStream.Create(bs5, bzfile);
  k := tarfile.Read(Buffer, BufferSize);
  while k > 0 do begin
    bz2.Write(Buffer, k);
    k := tarfile.Read(Buffer, BufferSize);
  end;
  BuildForm.ProgressBar.StepBy(5);
  Application.ProcessMessages;
  bz2.Destroy;
  bzfile.Destroy;
  tarfile.Destroy;
  if FileExists(tarstr) then
    DeleteFile(tarstr);
  MessageDlg('Your Dev-C++ Package has been successfully created to ' +
             ChangeFileExt(FileName, '.DevPak') +'. It is now ready for testing and distribution :)',
             mtInformation, [mbOK], 0);
end;

procedure TMainForm.BuildItemClick(Sender: TObject);
begin
  BuildForm := TBuildForm.Create(self);
  try
    BuildForm.Show;
    SaveItemClick(sender);
    if FileExists(ChangeFileExt(FileName, '.DevPak')) then
      DeleteFile(ChangeFileExt(FileName, '.DevPak'));
    BuildPackage;
  finally
    BuildForm.Free;
  end;
end;

procedure TMainForm.ReadDevPackFile;
var files, icons : TStringList;
    i  : integer;
    fi : TFileItem;
    ic : TIconItem;
begin
  with IniFile do begin
    edName.Text := ReadString(SETUP_SECTION, 'AppName', '');
    edNameVersion.Text := ReadString(SETUP_SECTION, 'AppVerName', '');
    edVersion.Text := ReadString(SETUP_SECTION, 'AppVersion', '');
    edStartMenu.Text := ReadString(SETUP_SECTION, 'MenuName', '');

    edDescription.Text := ReadString(SETUP_SECTION, 'Description', '');
    edURL.Text := ReadString(SETUP_SECTION, 'Url', '');
    edReadMe.Text := ReadString(SETUP_SECTION, 'Readme', '');
    edLicense.Text := ReadString(SETUP_SECTION, 'License', '');
    edPicture.Text := ReadString(SETUP_SECTION, 'Picture', '');
    edDepend.Text := ReadString(SETUP_SECTION, 'Dependencies', '');
    chbReboot.Checked := ReadBool(SETUP_SECTION, 'Reboot', False);

    files := TStringList.Create;
    icons := TStringList.Create;
    ReadSectionValues(FILES_SECTION, files);
    ReadSectionValues(ICONS_SECTION, icons);

    for i := 0 to files.Count - 1 do begin
      fi := TFileItem.Create;
      fi.Source := files.Names[i];
      fi.Dest := files.Values[files.Names[i]];
      fi.Node := FileView.Items.Add;
      fi.Node.Caption := fi.Source;
      if //(pos(';recursive', files.Values[files.Names[i]]) <> 0) or
         DirectoryExists(files.Names[i]) then begin
        fi.Node.ImageIndex := 1;
        fi.IsDir := true;
      end
      else begin
        fi.Node.ImageIndex := 0;
        fi.IsDir := false;
      end;
      FileList.Add(pointer(fi));
    end;

    for i := 0 to icons.Count - 1 do begin
      ic := TIconItem.Create;
      ic.Name := icons.Names[i];
      ic.Node := IconView.Items.Add;
      ic.Node.Caption := ic.Name;
      ic.Node.ImageIndex := 0;
      if pos(',', icons.Values[icons.Names[i]]) <> 0 then begin
        ic.Target := LeftStr(icons.Values[icons.Names[i]],
                             pos(',', icons.Values[icons.Names[i]]) - 1);
        ic.Icon := RightStr(icons.Values[icons.Names[i]],
                            length(icons.Values[icons.Names[i]]) - pos(',', icons.Values[icons.Names[i]]));
      end
      else begin
        ic.Target := icons.Values[icons.Names[i]];
        ic.Icon := '';
      end;
      IconList.Add(pointer(ic));
    end;
    files.Free;
    icons.Free;
  end;
end;

procedure TMainForm.WriteDevPackFile;
var i : integer;
begin
  with IniFile do begin
    WriteString(SETUP_SECTION, 'Version', DEVPAK_VERSION);
    WriteString(SETUP_SECTION, 'AppName', edName.Text);
    WriteString(SETUP_SECTION, 'AppVerName', edNameVersion.Text);
    WriteString(SETUP_SECTION, 'AppVersion', edVersion.Text);
    WriteString(SETUP_SECTION, 'MenuName', edStartMenu.Text);

    WriteString(SETUP_SECTION, 'Description', edDescription.Text);
    WriteString(SETUP_SECTION, 'Url', edURL.Text);
    WriteString(SETUP_SECTION, 'Readme', edReadMe.Text);
    WriteString(SETUP_SECTION, 'License', edLicense.Text);
    WriteString(SETUP_SECTION, 'Picture', edPicture.Text);
    WriteString(SETUP_SECTION, 'Dependencies', edDepend.Text);
    WriteBool(SETUP_SECTION, 'Reboot', chbReboot.Checked);

    for i := 0 to FileList.Count - 1 do begin
      if TFileItem(FileList[i]).IsDir then
        WriteString(FILES_SECTION, TFileItem(FileList[i]).Source, IncludeTrailingBackslash(TFileItem(FileList[i]).Dest))
      else
        WriteString(FILES_SECTION, TFileItem(FileList[i]).Source, TFileItem(FileList[i]).Dest);
    end;

    for i := 0 to IconList.Count - 1 do begin
      if TIconItem(IconList[i]).Icon <> '' then
        WriteString(ICONS_SECTION, TIconItem(IconList[i]).Name, TIconItem(IconList[i]).Target +
                    ',' + TIconItem(IconList[i]).Icon)
      else
        WriteString(ICONS_SECTION, TIconItem(IconList[i]).Name, TIconItem(IconList[i]).Target);
    end;

    UpdateFile;
  end;
end;

procedure TMainForm.OpenItemClick(Sender: TObject);
begin
  if OpenDialog.Execute then begin
    Clear;
    FileName := OpenDialog.FileName;
    OpenDevPackFile(FileName);
  end;
end;

procedure TMainForm.AddItemBtnClick(Sender: TObject);
var ic : TIconItem;
    m  : TMenuForm;
begin
  m := TMenuForm.Create(self);
  try
    if m.ShowModal = mrOk then begin
     ic := TIconItem.Create;
     ic.Name := m.edName.Text;
     ic.Target := m.edTarget.Text;
     ic.Icon := m.edIcon.Text;
     ic.Node := IconView.Items.Add;
     ic.Node.Caption := ic.Name;
     ic.Node.ImageIndex := 0;
     IconList.Add(pointer(ic));
    end;
  finally
    m.Free;
  end;
end;

procedure TMainForm.EditItemBtnClick(Sender: TObject);
var ic : TIconItem;
    m  : TMenuForm;
begin
  if IconView.SelCount <= 0 then
    exit;
  m := TMenuForm.Create(self);
  try
    ic := GetSelectedIcon;
    m.edName.Text := ic.Name;
    m.edIcon.Text := ic.Icon;
    m.edTarget.Text := ic.Target;
    if m.ShowModal = mrOk then begin
     ic.Name := m.edName.Text;
     ic.Target := m.edTarget.Text;
     ic.Icon := m.edIcon.Text;
     ic.Node.Caption := ic.Name;
    end;
  finally
    m.Free;
  end;
end;

function TMainForm.GetSelectedIcon : TIconItem;
var i : integer;
begin
  result := nil;
  for i := 0 to IconList.Count - 1 do begin
    if TIconItem(IconList[i]).Node = IconView.Selected then begin
      result := TIconItem(IconList[i]);
      break;
    end;
  end;
end;

function TMainForm.GetSelectedFile : TFileItem;
var i : integer;
begin
  result := nil;
  for i := 0 to FileList.Count - 1 do begin
    if TFileItem(FileList[i]).Node = FileView.Selected then begin
      result := TFileItem(FileList[i]);
      break;
    end;
  end;
end;

procedure TMainForm.RemoveIconBtnClick(Sender: TObject);
var ic : TIconItem;
begin
  if IconView.SelCount > 0 then begin
    ic := GetSelectedIcon;
    IniFile.DeleteKey(ICONS_SECTION, ic.Name);
    IconView.Items.Delete(IconView.Selected.Index);
    IconList.Remove(pointer(ic));
    ic.Free;
  end;
end;

procedure TMainForm.RemoveBtnClick(Sender: TObject);
var fi : TFileItem;
begin
  if FileView.SelCount > 0 then begin
    fi := GetSelectedFile;
    IniFile.DeleteKey(FILES_SECTION, fi.Source);
    FileView.Items.Delete(FileView.Selected.Index);
    FileList.Remove(pointer(fi));
    fi.Free;
  end;
end;

procedure TMainForm.AddDirBtnClick(Sender: TObject);
var fi : TFileItem;
    f  : TFileForm;
begin
  f := TFileForm.Create(self);
  try
    f.SetMode(true);
    if f.ShowModal = mrOk then begin
     fi := TFileItem.Create;
     fi.Source := f.edSource.Text;
     fi.Dest := f.edDest.Text;
     fi.Node := FileView.Items.Add;
     fi.Node.Caption := fi.Source;
     fi.Node.ImageIndex := 1;
     fi.IsDir := true;
     FileList.Add(pointer(fi));
    end;
  finally
    f.Free;
  end;
end;

procedure TMainForm.AddFileBtnClick(Sender: TObject);
var fi : TFileItem;
    f  : TFileForm;
begin
  f := TFileForm.Create(self);
  try
    f.SetMode(false);
    if f.ShowModal = mrOk then begin
     fi := TFileItem.Create;
     fi.Source := f.edSource.Text;
     fi.Dest := f.edDest.Text;
     fi.Node := FileView.Items.Add;
     fi.Node.Caption := fi.Source;
     fi.Node.ImageIndex := 0;
     fi.IsDir := false;
     FileList.Add(pointer(fi));
    end;
  finally
    f.Free;
  end;
end;

procedure TMainForm.OpenDevPackFile(devpakfile: String);
begin
  IniFile := TIniFile.Create(devpakfile);
  if FileExists(devpakfile) then
      ReadDevPackFile;
end;

end.
