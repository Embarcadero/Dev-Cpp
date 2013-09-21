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

unit helpfrm;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Buttons, StdCtrls, Grids, ValEdit, IniFiles, ExtCtrls,
  XPMenu;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Variants, Classes, QGraphics, QControls, QForms,
  QDialogs, QComCtrls, QButtons, QStdCtrls, QGrids, IniFiles, ExtCtrls
  ;
{$ENDIF}

type
  TfrmHelpEdit = class(TForm)
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    grpOptions: TGroupBox;
    cboIcon: TComboBoxEx;
    lblIcon: TLabel;
    grpMenu: TRadioGroup;
    cbPop: TCheckBox;
    lvFiles: TListView;
    grpActions: TGroupBox;
    btnNew: TSpeedButton;
    btnRename: TSpeedButton;
    btnBrowse: TSpeedButton;
    btnDelete: TSpeedButton;
    XPMenu: TXPMenu;
    cbSearchWord: TCheckBox;
    cbAffectF1: TCheckBox;
    procedure cboIconSelect(Sender: TObject);
    procedure grpMenuClick(Sender: TObject);
    procedure cbPopClick(Sender: TObject);
    procedure lvFilesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure Browse;
    procedure lvFilesEditing(Sender: TObject; Item: TListItem;
      var AllowEdit: Boolean);
    procedure lvFilesEdited(Sender: TObject; Item: TListItem;
      var S: String);
    procedure FormCreate(Sender: TObject);
    procedure lvFilesChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure btnNewClick(Sender: TObject);
    procedure btnRenameClick(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cbSearchWordClick(Sender: TObject);
    procedure cbAffectF1Click(Sender: TObject);
   private
    fINI: TINIFile;
    fEditing: boolean;
    procedure ReadHelpINI;
    procedure WriteHelpINI;
    procedure LoadText;
   public
    function Execute: boolean;
  end;


implementation

uses datamod, devcfg, utils, version, MultiLangSupport, DevThemes, ImageTheme;

{$R *.dfm}

type
 TEntry = class
  Menu: byte;
  Icon: smallint;
  searchword: boolean;
  affectF1: boolean;
  pop: boolean;
 end;


{ TfrmHelpEdit }

function TfrmHelpEdit.Execute: boolean;
begin
  ReadHelpINI;
  cboIcon.Images:= devImageThemes.CurrentTheme.HelpImages;//devTheme.Help;
  if lvFiles.Items.Count>0 then
    lvFiles.Selected:=lvFiles.Items[0];
  lvFilesChange(Self, nil, ctState);
  result:= ShowModal = mrOk;
  if result then WriteHelpINI;
end;

procedure TfrmHelpEdit.ReadHelpINI;
var
 hfile: string;
 hfiles: TStringList;
 idx: integer;
 Item: TListItem;
 Entry: TEntry;
begin
  hFile:= ValidateFile(DEV_HELP_INI, devDirs.Help, TRUE);
  if hFile = '' then exit;
  fini:= TINIFile.Create(hFile);
  with fini do
   begin
     hFiles:= TStringList.Create;
     try
      ReadSections(hFiles);
      for idx:= 0 to pred(hFiles.Count) do
       begin
         hFile:= ReadString(hFiles[idx], 'Path', '');
         if hFile = '' then continue;
         if AnsiPos(HTTP, hFile) = 0 then
          hFile:= ValidateFile(hFile, devDirs.Help);

         if (hFile <> '') then
          begin
            Item:= lvFiles.Items.Add;
            Item.Caption:= hFiles[idx];
            Item.SubItems.Add(hFile);
            Entry:= TEntry.Create;
            Entry.Menu:= ReadInteger(hFiles[idx], 'Menu', 0);
            Entry.Icon:= ReadInteger(hFiles[idx], 'Icon', 0);
            Entry.Pop:= ReadBool(hFiles[idx], 'Pop', FALSE);
            Entry.SearchWord:= ReadBool(hFiles[idx], 'SearchWord', FALSE);
            Entry.AffectF1:= ReadBool(hFiles[idx], 'AffectF1', FALSE);
            Item.Data:= Entry;
          end;
       end;
     finally
      hFiles.Free;
     end;
   end;
end;

procedure TfrmHelpEdit.WriteHelpINI;
var
 section,
 hFile: string;
 tmp: TStringList;
 idx: integer;
 Entry: TEntry;
begin
  if not DirectoryExists(devDirs.Help) then
    CreateDir(devDirs.Help);
  hFile:= ValidateFile(DEV_HELP_INI, devDirs.Help, TRUE);
  if not assigned(fINI) then
   if hFile <> '' then
    fINI:= TINIFile.Create(hFile)
   else
    fINI:= TINIFile.Create(devDirs.Help +DEV_HELP_INI)
  else
   fINI:= TINIFile.Create(devDirs.Help +DEV_HELP_INI);

  if (not assigned(fIni)) then begin
    MessageDlg('Coulnd''t create configuration file', mtError, [mbOk], 0);
    exit;
  end;

  tmp:= TStringList.Create;
  try
   fINI.ReadSections(tmp);
   for idx:= 0 to tmp.Count - 1 do
    fINI.EraseSection(tmp[idx]);
  finally
   tmp.Free;
  end;

  with fini do
   for idx:= 0 to lvFiles.Items.Count - 1 do
    begin
      section:= lvfiles.Items[idx].Caption;
      WriteString(section, 'Path',
        ExtractRelativePath(devDirs.Help, lvFiles.Items[idx].SubItems[0]));
      Entry:= TEntry(lvFiles.Items[idx].Data);
      WriteInteger(section, 'Menu', Entry.Menu);
      WriteInteger(section, 'Icon', Entry.Icon);
      WriteBool(section, 'SearchWord', Entry.SearchWord);
      WriteBool(section, 'AffectF1', Entry.AffectF1);
      WriteBool(section, 'Pop', Entry.Pop);
    end;
end;

procedure TfrmHelpEdit.cboIconSelect(Sender: TObject);
begin
  if assigned(lvFiles.Selected) then
   TEntry(lvFiles.Selected.Data).Icon:= cboIcon.ItemIndex -1;
end;

procedure TfrmHelpEdit.grpMenuClick(Sender: TObject);
begin
  if assigned(lvFiles.Selected) then
   TEntry(lvFiles.Selected.Data).Menu:= grpMenu.ItemIndex +1;
end;

procedure TfrmHelpEdit.cbPopClick(Sender: TObject);
begin
  if assigned(lvFiles.Selected) then
   TEntry(lvFiles.Selected.Data).pop:= cbPop.Checked;
end;

procedure TfrmHelpEdit.lvFilesSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if assigned(Item) then
   with Item do
    begin
      cboIcon.ItemIndex:= TEntry(Data).Icon +1;
      grpMenu.ItemIndex:= TEntry(Data).Menu -1;
      cbPop.Checked:= TEntry(Data).pop;
      cbSearchWord.Checked := TEntry(Data).searchword;
      cbAffectF1.Checked := TEntry(Data).affectF1;
    end;
end;

procedure TfrmHelpEdit.Browse;
var
 Item: TListItem;
 value: string;
 data: TEntry;
begin
  with dmMain.OpenDialog do
   begin
     Title:= Lang[ID_NV_HELPFILE];
     Filter:= flt_HELPS;
     InitialDir:=ExtractFilePath(GetRealPath(value, devDirs.Help));
     if Execute then
      begin
        if assigned(lvFiles.Selected) then
          Item:= lvFiles.Selected
        else begin
          Item:= lvFiles.Items.Add;
          Item.SubItems.Add('');
          Data:= TEntry.Create;
          Data.Menu:= 0;
          Data.Icon:= 0;
          Data.pop:= FALSE;
          Data.searchword := false;
          Data.affectF1 := false;
          Item.Data:= Data;
          Item.Selected := true;
        end;

        if Item.SubItems.Count<>0 then
          Item.SubItems.Add('');

        value:= FileName;
        value:= ExtractRelativePath(devDirs.Help, value);
        if Item.Caption='' then
          Item.Caption:=ExtractFilename(value);
        Item.SubItems[0]:= value;
      end;
   end;
end;

procedure TfrmHelpEdit.lvFilesEditing(Sender: TObject; Item: TListItem;
  var AllowEdit: Boolean);
begin
  fEditing:= TRUE;
end;

procedure TfrmHelpEdit.lvFilesEdited(Sender: TObject; Item: TListItem;
  var S: String);
begin
  fEditing:= FALSE;
end;

procedure TfrmHelpEdit.FormCreate(Sender: TObject);
begin
  LoadText;
end;

procedure TfrmHelpEdit.LoadText;
begin
  if devData.XPTheme then
    XPMenu.Active := true
  else
    XPMenu.Active := false;
  Caption:=                    Lang[ID_HE];
  lvFiles.Columns[0].Caption:= Lang[ID_HE_COL1];
  lvFiles.Columns[1].Caption:= Lang[ID_HE_COL2];
  grpOptions.Caption:=         '  '+Lang[ID_HE_GRP_OPTIONS]+'  ';
  lblIcon.Caption:=            Lang[ID_HE_ICON];
  grpMenu.Caption:=            Lang[ID_HE_GRP_MENU];
  grpMenu.Items[0]:=           Lang[ID_HE_SEC1];
  grpMenu.Items[1]:=           Lang[ID_HE_SEC2];
  cbPop.Caption:=              Lang[ID_HE_ONPOP];
  cbSearchWord.Caption:=        Lang[ID_HE_SEARCHWORD];
  cbAffectF1.Caption:=         Lang[ID_HE_AFFECTF1];
  btnNew.Caption:=             Lang[ID_BTN_ADD];
  btnRename.Caption:=          Lang[ID_BTN_RENAME];
  btnBrowse.Caption:=          Lang[ID_BTN_BROWSE];
  btnDelete.Caption:=          Lang[ID_BTN_DELETE];
  grpActions.Caption:=         '  '+Lang[ID_HE_GRP_ACTIONS]+'  ';

  btnOk.Caption:=              Lang[ID_BTN_OK];
  btnCancel.Caption:=          Lang[ID_BTN_CANCEL];
  btnHelp.Caption:=            Lang[ID_BTN_HELP];
end;

procedure TfrmHelpEdit.lvFilesChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  btnRename.Enabled:=Assigned(lvFiles.Selected);
  btnBrowse.Enabled:=Assigned(lvFiles.Selected);
  btnDelete.Enabled:=Assigned(lvFiles.Selected);
end;

procedure TfrmHelpEdit.btnNewClick(Sender: TObject);
begin
  lvFiles.Selected:=nil;
  Browse;
end;

procedure TfrmHelpEdit.btnRenameClick(Sender: TObject);
begin
  if assigned(lvFiles.Selected) then
    lvFiles.Selected.EditCaption;
end;

procedure TfrmHelpEdit.btnBrowseClick(Sender: TObject);
begin
  Browse;
end;

procedure TfrmHelpEdit.btnDeleteClick(Sender: TObject);
begin
  if (assigned(lvFiles.Selected)) then
    lvFiles.Selected.Delete;
end;

procedure TfrmHelpEdit.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if lvFiles.IsEditing then begin
    lvFiles.Selected.CancelEdit;
    Action:=caNone;
  end;
end;

procedure TfrmHelpEdit.cbSearchWordClick(Sender: TObject);
begin
  if assigned(lvFiles.Selected) then
    TEntry(lvFiles.Selected.Data).SearchWord:= cbSearchWord.Checked;
end;

procedure TfrmHelpEdit.cbAffectF1Click(Sender: TObject);
begin
  if assigned(lvFiles.Selected) then
    TEntry(lvFiles.Selected.Data).AffectF1:= cbAffectF1.Checked;
end;

end.
