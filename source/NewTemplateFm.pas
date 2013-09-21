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

unit NewTemplateFm;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst, devTabs, ExtCtrls, Buttons, ComCtrls,
  project, ImgList, ExtDlgs, IniFiles, XPMenu;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Variants, Classes, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls, QCheckLst, devTabs, QExtCtrls, QButtons, QComCtrls,
  project, QImgList, IniFiles, Types;
{$ENDIF}

type
  TNewTemplateForm = class(TForm)
    devPages1: TdevPages;
    pgTemplate: TdevPage;
    pgFiles: TdevPage;
    lblName: TLabel;
    lblDescr: TLabel;
    lblCateg: TLabel;
    lblProjName: TLabel;
    txtDescr: TEdit;
    cmbCateg: TComboBox;
    txtProjName: TEdit;
    lblFiles: TLabel;
    lstFiles: TCheckListBox;
    pgExtras: TdevPage;
    lblCompiler: TLabel;
    memCompiler: TMemo;
    lblLinker: TLabel;
    memLinker: TMemo;
    btnCreate: TButton;
    btnCancel: TButton;
    dlgPic: TOpenPictureDialog;
    lblIcons: TGroupBox;
    lstIcons: TListBox;
    btnLib: TBitBtn;
    btnBrowse: TBitBtn;
    btnRemove: TBitBtn;
    cmbName: TComboBox;
    XPMenu: TXPMenu;
    memCppCompiler: TMemo;
    lblCppCompiler: TLabel;
    cbInclude: TCheckBox;
    cbLibrary: TCheckBox;
    cbRessource: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure btnLibClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure lstIconsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure lstIconsClick(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
    procedure cmbNameChange(Sender: TObject);
  private
    { Private declarations }
    Icons: array[0..1] of TIcon;
    IconFiles: array[0..1] of TFileName;
    sIcon: string;
    sProjIcon: string;
    procedure LoadText;
    procedure ReadCategories;
    procedure FillUnits;
    procedure FillExtras;
    procedure FillIconsList;
    procedure CreateTemplate;
  public
    { Public declarations }
    TempProject: TProject;
  end;

var
  NewTemplateForm: TNewTemplateForm;

implementation

uses utils, IconFrm, devcfg, version, Templates, MultiLangSupport;

{$R *.dfm}

procedure TNewTemplateForm.FormShow(Sender: TObject);
begin
  LoadText;

  cmbName.Text := 'Custom project 1';
  txtDescr.Text := 'This is a custom project.';
  txtProjName.Text := 'Custom project';

  ReadCategories;
  FillUnits;
  FillExtras;
  FillIconsList;

  btnLib.Enabled := False;
  btnBrowse.Enabled := False;
  btnRemove.Enabled := False;

  devPages1.ActivePageIndex := 0;

  cmbNameChange(nil);
end;

procedure TNewTemplateForm.btnLibClick(Sender: TObject);
var
  IconFile: string;
begin
  IconFile := '';
  with TIconForm.Create(Self) do try
    if ShowModal = mrOk then
      if Selected <> '' then
        IconFile := Selected;
  finally
    Free;
  end;

  if IconFile <> '' then begin
    if Icons[lstIcons.ItemIndex] = nil then
      Icons[lstIcons.ItemIndex] := TIcon.Create;
    Icons[lstIcons.ItemIndex].LoadFromFile(IconFile);
    IconFiles[lstIcons.ItemIndex] := IconFile;
    lstIcons.Repaint;
  end;
end;

procedure TNewTemplateForm.btnRemoveClick(Sender: TObject);
begin
  FreeAndNil(Icons[lstIcons.ItemIndex]);
  IconFiles[lstIcons.ItemIndex] := '';
  lstIcons.Repaint;
end;

procedure TNewTemplateForm.btnBrowseClick(Sender: TObject);
var
  IconFile: string;
begin
  if dlgPic.Execute then begin
    IconFile := dlgPic.FileName;
    if IconFile <> '' then begin
      if Icons[lstIcons.ItemIndex] = nil then
        Icons[lstIcons.ItemIndex] := TIcon.Create;
      Icons[lstIcons.ItemIndex].LoadFromFile(IconFile);
      IconFiles[lstIcons.ItemIndex] := IconFile;
      lstIcons.Repaint;
    end;
  end;
end;

procedure TNewTemplateForm.lstIconsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  XOffset: Integer;
  YOffset: Integer;
begin
  with (Control as TListBox).Canvas do begin
    FillRect(Rect);
    XOffset := 2;
    YOffset := ((Control as TListBox).ItemHeight - Abs(Font.Height)) div 2;
    if Icons[Index] <> nil then begin
      DrawIcon(Handle, Rect.Left + XOffset, Rect.Top + XOffset, Icons[Index].Handle);
      XOffset := Icons[Index].Width + 6;
    end
    else begin
      Rectangle(Rect.Left + XOffset, Rect.Top + XOffset, Rect.Left + 32 + XOffset, Rect.Top + 32 + XOffset);
      TextOut(Rect.Left + XOffset + 1, Rect.Top + YOffset, 'Empty');
      XOffset := 32 + 6;
    end;
    TextOut(Rect.Left + XOffset, Rect.Top + YOffset, (Control as TListBox).Items[Index])
  end;
end;

procedure TNewTemplateForm.lstIconsClick(Sender: TObject);
begin
  btnLib.Enabled := lstIcons.ItemIndex <> -1;
  btnBrowse.Enabled := lstIcons.ItemIndex <> -1;
  btnRemove.Enabled := lstIcons.ItemIndex <> -1;
end;

procedure TNewTemplateForm.ReadCategories;
  procedure AddCategory(FileName: TFileName);
  var
    Temp: TTemplate;
  begin
    if not FileExists(FileName) then
      Exit;
    Temp := TTemplate.Create;
    Temp.ReadTemplateFile(FileName);
    if cmbCateg.Items.IndexOf(Temp.Catagory) = -1 then
      cmbCateg.Items.Add(Temp.Catagory);
    cmbName.Items.Add(Temp.Name);
  end;
var
  i: Integer;
  Templates: TStringList;
  sDir: string;
begin
  sDir:=devDirs.Templates;
  if not CheckChangeDir(sDir) then
    Exit;
  cmbCateg.Clear;
  Templates := TStringList.Create;
  try
    FilesFromWildCard(devDirs.Templates, '*' + TEMPLATE_EXT,
      Templates, FALSE, FALSE, TRUE);
    if Templates.Count > 0 then begin
      for i := 0 to Templates.Count - 1 do
        AddCategory(Templates[i]);
    end;
  finally
    Templates.Free;
  end;
end;

procedure TNewTemplateForm.FillUnits;
var
  I: integer;
begin
  lstFiles.Clear;
  for I := 0 to TempProject.Units.Count - 1 do
    lstFiles.Items.Add(ExtractFileName(TempProject.Units.Items[I].FileName));
  for I := 0 to lstFiles.Items.Count - 1 do
    lstFiles.Checked[I] := True;
  if lstFiles.Items.Count > 0 then
    lstFiles.ItemIndex := 0;
end;

procedure TNewTemplateForm.FillExtras;
begin
  memCompiler.Clear;
  memLinker.Clear;
  if TempProject.Options.cmdLines.Compiler <> '' then
    memCompiler.Lines.Add(StringReplace(TempProject.Options.cmdLines.Compiler, '_@@_', #13#10, [rfReplaceAll]));
  if TempProject.Options.cmdLines.CppCompiler <> '' then
    memCppCompiler.Lines.Add(StringReplace(TempProject.Options.cmdLines.CppCompiler, '_@@_', #13#10, [rfReplaceAll]));
  if TempProject.Options.cmdLines.Linker <> '' then
    memLinker.Lines.Add(StringReplace(TempProject.Options.cmdLines.Linker, '_@@_', #13#10, [rfReplaceAll]));
end;

procedure TNewTemplateForm.FillIconsList;
begin
  lstIcons.Items.Clear;
  if TempProject.Options.Icon <> '' then begin
    IconFiles[0] := ExpandFileto(TempProject.Options.Icon, TempProject.Directory);
    Icons[0] := TIcon.Create;
    Icons[0].LoadFromFile(ExpandFileto(TempProject.Options.Icon, TempProject.Directory));
    IconFiles[1] := ExpandFileto(TempProject.Options.Icon, TempProject.Directory);
    Icons[1] := TIcon.Create;
    Icons[1].LoadFromFile(ExpandFileto(TempProject.Options.Icon, TempProject.Directory));
  end
  else begin
    IconFiles[0] := '';
    IconFiles[1] := '';
  end;

  lstIcons.Items.Add(sIcon);
  lstIcons.Items.Add(sProjIcon);
end;

procedure TNewTemplateForm.cmbNameChange(Sender: TObject);
begin
  btnCreate.Enabled := (cmbName.Text <> '') and (cmbCateg.Text <> '');
end;

procedure TNewTemplateForm.btnCreateClick(Sender: TObject);
begin
  CreateTemplate;
  Close;
end;

procedure TNewTemplateForm.CreateTemplate;
var
  tmpIni: TIniFile;
  I, C: integer;
  S, filename: string;
begin
  filename := devDirs.Templates + cmbName.Text + '.template';
  if FileExists(filename) then begin
    if MessageDlg(Lang[ID_MSG_FILEEXISTS],
                  mtWarning, [mbYes, mbNo], 0) = mrYes then
      DeleteFile(filename)
    else begin
      exit;
    end;
  end;
  tmpIni := TIniFile.Create(filename);
  with tmpIni do try
    WriteInteger('Template', 'ver', 1);
    WriteString('Template', 'Name', cmbName.Text);
    if IconFiles[0] <> '' then begin
      CopyFile(PChar(IconFiles[0]), PChar(devDirs.Templates + cmbName.Text + '.ico'), False);
      WriteString('Template', 'Icon', cmbName.Text + '.ico');
    end;
    WriteString('Template', 'Description', txtDescr.Text);
    WriteString('Template', 'Catagory', cmbCateg.Text); // 'catagory' is not a typo...

    C := 0;
    for I := 0 to lstFiles.Items.Count - 1 do
      if lstFiles.Checked[I] then begin
        WriteString('Unit' + IntToStr(C), 'CppName', lstFiles.Items[I]);
        S := StringReplace(cmbName.Text + '_' + lstFiles.Items[I] + '.txt', ' ', '_', [rfReplaceAll]);
        WriteString('Unit' + IntToStr(C), 'Cpp', S);
        CopyFile(PChar(TempProject.Units[I].FileName), PChar(devDirs.Templates + S), False);
        Inc(C);
      end;

    WriteInteger('Project', 'UnitCount', C);
    WriteInteger('Project', 'Type', Integer(TempProject.Options.typ));
    WriteBool('Project', 'IsCpp', TempProject.Options.useGPP);
    WriteString('Project', 'Compiler', StringReplace(memCompiler.Text, #13#10, '_@@_', [rfReplaceAll]));
    WriteString('Project', 'CppCompiler', StringReplace(memCppCompiler.Text, #13#10, '_@@_', [rfReplaceAll]));
    WriteString('Project', 'Linker', StringReplace(memLinker.Text, #13#10, '_@@_', [rfReplaceAll]));
    WriteString('Project', 'CompilerSettings', TempProject.Options.CompilerOptions);
    WriteInteger('Project', 'CompilerSet', TempProject.Options.CompilerSet);
    WriteBool('Project', 'IncludeVersionInfo', TempProject.Options.IncludeVersionInfo);
    WriteBool('Project', 'SupportXPThemes', TempProject.Options.SupportXPThemes);

    if cbInclude.Checked then
      WriteString('Project', 'Includes', TempProject.Options.Includes.DelimitedText);
    if cbLibrary.Checked then
      WriteString('Project', 'Libs', TempProject.Options.Libs.DelimitedText);
    if cbRessource.Checked then
      WriteString('Project', 'ResourceIncludes', TempProject.Options.ResourceIncludes.DelimitedText);

    if txtProjName.Text = '' then
      WriteString('Project', 'Name', cmbName.Text)
    else
      WriteString('Project', 'Name', txtProjName.Text);
    if IconFiles[1] <> '' then begin
      CopyFile(PChar(IconFiles[1]), PChar(devDirs.Templates + cmbName.Text + '.project.ico'), False);
      WriteString('Project', 'ProjectIcon', cmbName.Text + '.project.ico');
    end;
    MessageDlg('The new template has been created!'#10#10 +
      'You can find it as "' + cmbName.Text + '" under the "' + cmbCateg.Text + '" tab in the "New project" dialog.',
      mtInformation, [mbOk], 0);
  finally
    tmpIni.Free;
  end;
end;

procedure TNewTemplateForm.LoadText;
begin
  if devData.XPTheme then
    XPMenu.Active := true
  else
    XPMenu.Active := false;
  with Lang do begin
    lblName.Caption := Strings[ID_NEWTPL_NAME];
    lblDescr.Caption := Strings[ID_NEWTPL_DESCRIPTION];
    lblCateg.Caption := Strings[ID_NEWTPL_CATEGORY];
    lblProjName.Caption := Strings[ID_NEWTPL_PROJECTNAME];
    lblFiles.Caption := Strings[ID_NEWTPL_FILES];
    lblCompiler.Caption := Strings[ID_POPT_COMP];
    lblCppCompiler.Caption := Strings[ID_COPT_GRP_CPP];
    lblLinker.Caption := Strings[ID_COPT_LINKERTAB];
    lblIcons.Caption := Strings[ID_NEWTPL_ICONS];
    pgTemplate.Caption := Strings[ID_NEWTPL_PAGETEMPLATE];
    pgFiles.Caption := Strings[ID_NEWTPL_PAGEFILES];
    pgExtras.Caption := Strings[ID_NEWTPL_PAGEEXTRAS];
    sIcon := Strings[ID_NEWTPL_TEMPLATEICON];
    sProjIcon := Strings[ID_NEWTPL_PROJECTICON];
    btnCreate.Caption := Strings[ID_NEWTPL_CREATE];
    btnCancel.Caption := Strings[ID_BTN_CANCEL];
    btnLib.Caption := Strings[ID_POPT_ICOLIB];
    btnBrowse.Caption := Strings[ID_BTN_BROWSE];
    btnRemove.Caption := Strings[ID_BTN_REMOVEICON];
    Caption := Strings[ID_NEWTPL_CAPTION];
    cbInclude.Caption := Strings[ID_NEWTPL_INCDIR];
    cbLibrary.Caption := Strings[ID_NEWTPL_LIBDIR];
    cbRessource.Caption := Strings[ID_NEWTPL_RESDIR];
  end;
end;

end.

