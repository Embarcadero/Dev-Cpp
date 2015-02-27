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

unit NewProjectFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ImgList, Buttons, ComCtrls, Templates, Inifiles;

type
  TNewProjectForm = class(TForm)
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    rbC: TRadioButton;
    rbCpp: TRadioButton;
    cbDefault: TCheckBox;
    lblPrjName: TLabel;
    edProjectName: TEdit;
    TabsMain: TTabControl;
    ProjView: TListView;
    TemplateLabel: TLabel;
    btnHelp: TBitBtn;
    ImageList: TImageList;
    procedure ProjViewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure FormCreate(Sender: TObject);
    procedure LoadText;
    procedure FormDestroy(Sender: TObject);
    procedure TabsMainChange(Sender: TObject);
    procedure ProjViewDblClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure edProjectNameChange(Sender: TObject);
  private
    procedure AddTemplate(const FileName: AnsiString);
    procedure ReadTemplateDir;
  private
    fTemplates: TList;
    procedure UpdateView;
  public
    function GetTemplate: TTemplate;
  end;

implementation

uses
  MultiLangSupport, utils, DataFrm, FileCtrl, devcfg, version,
  project, ProjectTypes;

{$R *.dfm}

procedure TNewProjectForm.FormCreate(Sender: TObject);
begin
  fTemplates := TList.Create;
  LoadText;
  ReadTemplateDir;
end;

procedure TNewProjectForm.FormDestroy(Sender: TObject);
var
  I: integer;
begin
  for I := 0 to fTemplates.Count - 1 do
    TTemplate(fTemplates[i]).Free;
  fTemplates.Free;
end;

procedure TNewProjectForm.AddTemplate(const FileName: AnsiString);
var
  Template: TTemplate;
begin
  if not FileExists(FileName) then
    Exit;

  Template := TTemplate.Create;
  Template.ReadTemplateFile(FileName);
  fTemplates.Add(Template);
end;

procedure TNewProjectForm.ReadTemplateDir;
var
  i: Integer;
  TemplateFileNames: TStringList;
begin
  TemplateFileNames := TStringList.Create;
  try
    FilesFromWildCard(devDirs.Templates, '*' + TEMPLATE_EXT, TemplateFileNames, FALSE, FALSE, TRUE);
    if TemplateFileNames.Count > 0 then begin
      for i := 0 to TemplateFileNames.Count - 1 do
        AddTemplate(TemplateFileNames[i]);
      UpdateView;
    end;
  finally
    TemplateFileNames.Free;
  end;
end;

function TNewProjectForm.GetTemplate: TTemplate;
begin
  if Assigned(ProjView.Selected) then begin
    result := TTemplate(fTemplates[integer(ProjView.Selected.Data)]);
    result.Options.useGPP := rbCpp.Checked;
    result.Name := edProjectName.Text;
  end else
    result := nil;
end;

procedure TNewProjectForm.ProjViewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
var
  TemplateItem: TTemplate;
begin
  if Assigned(ProjView.Selected) then begin
    TemplateItem := TTemplate(fTemplates[integer(ProjView.Selected.Data)]);
    if not Assigned(TemplateItem) then
      Exit;

    TemplateLabel.Caption := TemplateItem.Description;
    if TemplateItem.Options.useGPP then begin
      rbC.Enabled := False;
      rbCpp.Checked := True;
    end else
      rbC.Enabled := True;
  end else
    TemplateLabel.Caption := '';

  btnOk.Enabled := Assigned(ProjView.Selected) and (edProjectName.Text <> '');
end;

procedure TNewProjectForm.LoadText;
begin
  // Set interface font
  Font.Name := devData.InterfaceFont;
  Font.Size := devData.InterfaceFontSize;

  TemplateLabel.Font.Name := devData.InterfaceFont;
  TemplateLabel.Font.Size := devData.InterfaceFontSize;

  Caption := Lang[ID_NP];
  lblPrjName.Caption := Lang[ID_NP_PRJNAME];
  rbC.Caption := Lang[ID_NP_DEFAULTC];
  rbCpp.Caption := Lang[ID_NP_DEFAULTCPP];
  cbDefault.Caption := Lang[ID_NP_MAKEDEFAULT];

  btnOk.Caption := Lang[ID_BTN_OK];
  btnCancel.Caption := Lang[ID_BTN_CANCEL];
  btnHelp.Caption := Lang[ID_BTN_HELP];

  edProjectName.Text := format(Lang[ID_NEWPROJECT], [dmMain.GetNewFileNumber]);
end;

procedure TNewProjectForm.UpdateView;
  function HasPage(const value: AnsiString): boolean;
  var
    I: integer;
  begin
    for I := 0 to TabsMain.Tabs.Count - 1 do
      if CompareText(TabsMain.Tabs[I], Value) = 0 then begin
        Result := True;
        Exit;
      end;
    Result := False;
  end;
var
  I: integer;
  TemplateItem: TTemplate;
  ListItem: TListItem;
  IconItem: TIcon;
  IconFileName: AnsiString;
begin
  // Keep each icon in a separate image list
  ImageList.Clear;

  ProjView.Items.BeginUpdate;
  try
    ProjView.Items.Clear;

    // Walk all items
    for I := 0 to pred(fTemplates.Count) do begin
      TemplateItem := TTemplate(fTemplates[I]);

      // Add a page for each unique category
      if not HasPage(TemplateItem.Category) then
        TabsMain.Tabs.Append(TemplateItem.Category);

      // Select a category
      if TemplateItem.Category = '' then
        TemplateItem.Category := Lang[ID_NP_PRJSHEET];

      // Only add if we're viewing this category
      if SameText(TemplateItem.Category, TabsMain.Tabs[TabsMain.TabIndex]) then begin
        ListItem := ProjView.Items.Add;
        ListItem.Caption := TemplateItem.Name;
        ListItem.Data := pointer(I);
        IconFileName := ValidateFile(TemplateItem.Icon, '', true);
        if IconFileName <> '' then begin

          // Add icon to central dump and tell ListItem to use it
          IconItem := TIcon.Create;
          try
            IconItem.LoadFromFile(IconFileName); // ValidateFile prepends path
            ListItem.ImageIndex := ImageList.AddIcon(IconItem);
            if ListItem.ImageIndex = -1 then
              ListItem.ImageIndex := 0;
          finally
            IconItem.Free;
          end;
        end else
          ListItem.ImageIndex := 0; // don't use an icon
      end;
    end;
  finally
    ProjView.Items.EndUpdate;
  end;
end;

procedure TNewProjectForm.TabsMainChange(Sender: TObject);
begin
  UpdateView;
end;

procedure TNewProjectForm.ProjViewDblClick(Sender: TObject);
begin
  if Assigned(ProjView.Selected) then
    ModalResult := mrOk;
end;

procedure TNewProjectForm.btnHelpClick(Sender: TObject);
begin
  OpenHelpFile('index.htm');
end;

procedure TNewProjectForm.edProjectNameChange(Sender: TObject);
begin
  btnOk.Enabled := Assigned(ProjView.Selected) and (edProjectName.Text <> '');
end;

end.

