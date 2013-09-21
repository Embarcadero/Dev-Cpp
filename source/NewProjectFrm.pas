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
unit NewProjectFrm;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ImgList, Buttons, ComCtrls, Templates, Inifiles,
  devTabs, XPMenu;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QExtCtrls, QImgList, QButtons, QComCtrls, Templates, Inifiles,
  devTabs;
{$ENDIF}

type
  TNewProjectForm = class(TForm)
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    ImageList1: TImageList;
    Panel1: TPanel;
    rbC: TRadioButton;
    rbCpp: TRadioButton;
    cbDefault: TCheckBox;
    ImageList: TImageList;
    lblPrjName: TLabel;
    edProjectName: TEdit;
    TabsMain: TdevTabs;
    ProjView: TListView;
    pnlDesc: TPanel;
    lblDesc: TLabel;
    TemplateLabel: TLabel;
    Bevel1: TBevel;
    Label2: TLabel;
    btnHelp: TBitBtn;
    XPMenu: TXPMenu;
    procedure ProjViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure FormCreate(Sender: TObject);
    procedure LoadText;
    procedure FormDestroy(Sender: TObject);
    procedure TabsMainChange(Sender: TObject);
    procedure ProjViewDblClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
   private
    procedure AddTemplate(FileName: string);
    procedure ReadTemplateIndex;
   private
    fTemplates: TList;
    procedure UpdateView;
   public
    function GetTemplate: TTemplate;
  end;

implementation

uses
{$IFDEF WIN32}
  MultiLangSupport, utils, datamod, FileCtrl, devcfg, version,
  project, prjtypes;
{$ENDIF}
{$IFDEF LINUX}
  MultiLangSupport, utils, datamod, devcfg, version,
  project, prjtypes;
{$ENDIF}

{$R *.dfm}

procedure TNewProjectForm.FormCreate(Sender: TObject);
begin
  fTemplates:= TList.Create;
//  edProjectFile.Text:= devDirs.Default +edProjectName.Text;
  LoadText;
  ReadTemplateIndex;
  edProjectName.Text:= format(Lang[ID_NEWPROJECT], [dmMain.GetNumber]);
end;

procedure TNewProjectForm.FormDestroy(Sender: TObject);
begin
  fTemplates.Free;
end;

procedure TNewProjectForm.AddTemplate(FileName: string);
var
 Template: TTemplate;
begin
  if not FileExists(FileName) then exit;
  Template:= TTemplate.Create;
  Template.ReadTemplateFile(FileName);
  fTemplates.Add(Template);
end;

procedure TNewProjectForm.ReadTemplateIndex;
var
  i: Integer;
  LTemplates: TStringList;
  sDir: string;
begin
  sDir:=devDirs.Templates;
  if not CheckChangeDir(sDir) then begin
    MessageDlg('Could not change to the Templates directory ('+devDirs.Templates+')...', mtError, [mbOk], 0);
    Exit;
  end;
  LTemplates:= TStringList.Create;
  try
   FilesFromWildCard(devDirs.Templates, '*' +TEMPLATE_EXT,
     LTemplates, FALSE, FALSE, TRUE);
   if LTemplates.Count> 0 then
    begin
      for i:= 0 to pred(LTemplates.Count) do
       AddTemplate(LTemplates[i]);
      UpdateView;
    end;
  finally
   LTemplates.Free;
  end;
end;

function TNewProjectForm.GetTemplate: TTemplate;
var
 Opts: TProjOptions;
begin
  InitOptionsRec(Opts);
  if assigned(ProjView.Selected) then
   begin
     result:= TTemplate(fTemplates[integer(ProjView.Selected.Data)]);
     Opts:= result.OptionsRec;
   end
  else
   begin
     result:= TTemplate.Create;
     result.Version:= -1;
   end;
  result.ProjectName:= edProjectName.Text;
  Opts.useGPP:= rbCpp.Checked;
  result.OptionsRec:= Opts;
end;

procedure TNewProjectForm.ProjViewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var
 LTemplate: TTemplate;
begin
  if not assigned(ProjView.Selected) then
   begin
     TemplateLabel.Caption:= '';
//     edProjectName.Text:= '';
     btnOk.Enabled := False;
   end
  else
   begin
     btnOk.Enabled := True;
     LTemplate:= TTemplate(fTemplates[integer(ProjView.Selected.Data)]);
     if not assigned(LTemplate) then exit;
     TemplateLabel.Caption:= LTemplate.Description;

     if LTemplate.OptionsRec.useGPP then
     begin
         rbC.Enabled := False;
         rbCpp.Checked := True;
     end else
         rbC.Enabled := True;

//     edProjectName.Text:= LTemplate.ProjectName;
   end;
end;

procedure TNewProjectForm.LoadText;
begin
  if devData.XPTheme then
    XPMenu.Active := true
  else
    XPMenu.Active := false;
  Caption :=              Lang[ID_NP];
  lblDesc.Caption :=      Lang[ID_NP_DESC];
  lblPrjName.Caption:=    Lang[ID_NP_PRJNAME];
  rbC.Caption :=          Lang[ID_NP_DEFAULTC];
  rbCpp.Caption :=        Lang[ID_NP_DEFAULTCPP];
  cbDefault.Caption :=    Lang[ID_NP_MAKEDEFAULT];
  Label2.Caption :=       '  '+Lang[ID_NP_PRJOPTIONS]+'  ';

  btnOk.Caption :=        Lang[ID_BTN_OK];
  btnCancel.Caption :=    Lang[ID_BTN_CANCEL];
  btnHelp.Caption:=       Lang[ID_BTN_HELP];
end;

procedure TNewProjectForm.UpdateView;
 function HasPage(const value: string): boolean;
  var
   idx: integer;
  begin
    result:= TRUE;
    for idx:= 0 to pred(TabsMain.Tabs.Count) do
     if AnsiCompareText(TabsMain.Tabs[idx], Value) = 0 then exit;
    result:= FALSE;
  end;
var
 idx: integer;
 LTemplate: TTemplate;
 Item: TListItem;
 LIcon: TIcon;
 fName: string;
begin
  for idx:= 0 to pred(fTemplates.Count) do
   begin
     LTemplate:= TTemplate(fTemplates[idx]);
     if not HasPage(LTemplate.Catagory) then
      TabsMain.Tabs.Append(LTemplate.Catagory);
   end;

  // create current page
  if TabsMain.TabIndex> 0 then
   ProjView.LargeImages:= ImageList1
  else
   ProjView.LargeImages:= ImageList;

  ProjView.Items.Clear;
  for idx:= pred(ImageList1.Count) downto 1 do
   ImageList1.Delete(idx);

  for idx:= 0 to pred(fTemplates.Count) do
   begin
     LTemplate:= TTemplate(fTemplates[idx]);
     if LTemplate.Catagory = '' then LTemplate.Catagory:= Lang[ID_NP_PRJSHEET];
     if AnsiCompareText(LTemplate.Catagory, TabsMain.Tabs[TabsMain.TabIndex]) = 0 then
      begin
        Item:= ProjView.Items.Add;
        Item.Caption:= LTemplate.Name;
        Item.Data:= pointer(idx);
//        ShowMessage(LTemplate.Name + ': ' + LTemplate.OptionsRec.icon);
        fName:= ValidateFile(LTemplate.OptionsRec.Icon, ExtractFilePath(LTemplate.FileName));
        if fName <> '' then
         begin
           LIcon:= TIcon.Create;
           try
            LIcon.LoadFromFile(ExpandFileto(fName, ExtractFilePath(LTemplate.FileName)));
            Item.ImageIndex:= ImageList1.AddIcon(LIcon);
            if Item.ImageIndex = -1 then
             Item.ImageIndex:= 0;
           finally
            LIcon.Free;
           end;
         end
        else
         Item.ImageIndex:= LTemplate.IconIndex;
      end;
   end;
end;

procedure TNewProjectForm.TabsMainChange(Sender: TObject);
begin
  UpdateView;
end;

procedure TNewProjectForm.ProjViewDblClick(Sender: TObject);
begin
  ModalResult:= mrOk;
end;

procedure TNewProjectForm.btnCancelClick(Sender: TObject);
begin
  Dec(dmMain.fProjectCount);
end;

procedure TNewProjectForm.btnHelpClick(Sender: TObject);
begin
  HelpFile:= devDirs.Help +DEV_MAINHELP_FILE;
  Application.HelpJump('ID_CREATEPROJECT');
end;

end.
