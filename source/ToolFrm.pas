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

unit ToolFrm;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Classes, Menus, Graphics, Controls, Forms,
  StdCtrls, Buttons, ShellAPI, XPMenu;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Classes, QMenus, QGraphics, QControls, QForms,
  QStdCtrls, QButtons;
{$ENDIF}

type
{ Tool List }

  PToolItem = ^TToolItem;
  TToolItem = record
   Title: string;
   Exec: string;
   WorkDir: string;
   Params: string;
   IcoNumGnome: integer;
   IcoNumBlue: integer;
   IcoNumNewLook: integer;
   HasIcon: boolean;
  end;

  TToolList = class(TObject)
   private
    fList: TList;
    function GetItem(index: integer): PToolItem;
    procedure SetItem(index: integer; Value: PToolItem);
    function GetCount: integer;
    procedure Packit;
   public
    constructor Create;
    destructor Destroy; override;
    function AddItem(Value: PToolItem): integer;
    procedure MoveItem(CurIndex, NewIndex: integer);
    procedure RemoveItem(index: integer);
    procedure LoadTools;
    procedure SaveTools;
    function ParseString(const S: string): string;

    property Items[index: integer]: PToolItem read GetItem write SetItem; default;
    property Count: integer read GetCount;
  end;

{ Tool Edit Form }
  TToolController = class;

  TToolForm = class(TForm)
    grpCurrent: TGroupBox;
    btnClose: TBitBtn;
    ListBox: TListBox;
    btnUp: TSpeedButton;
    btnDown: TSpeedButton;
    XPMenu: TXPMenu;
    grpActions: TGroupBox;
    btnAdd: TSpeedButton;
    btnDelete: TSpeedButton;
    btnEdit: TSpeedButton;
    procedure btnEditClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure ListBoxClick(Sender: TObject);
    procedure PosbtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
   private
    fController: TToolController;
    procedure UpdateList;
    procedure UpdateButtons;
    procedure LoadText;
   public
    property Controller: TToolController read fController write fController;
  end;

  TToolController = class(TObject)
   private
    fToolList: TToolList;
    fMenu: TMenuItem;
    fOnClick: TNotifyEvent;
    fOffset: integer;
   public
    constructor Create;
    destructor Destroy; override;
    procedure BuildMenu;
    procedure Edit;
    property Menu: TMenuItem read fMenu write fMenu;
    property Offset: integer read fOffset write fOffset;
    property ToolClick: TNotifyEvent read fOnClick write fOnClick;
    property ToolList: TToolList read fToolList write fToolList;
  end;

implementation

uses ToolEditFrm, inifiles, devcfg, utils, MultiLangSupport, datamod,
  version;

{$R *.dfm}


 { TToolList }

constructor TToolList.Create;
begin
  inherited Create;
  fList:= TList.Create;
end;

destructor TToolList.Destroy;
begin
  fList.Free;
  inherited Destroy;
end;

procedure TToolList.Packit;
begin
  fList.Pack;
  fList.Capacity:= fList.Count;
end;

function TToolList.GetItem(index: integer): PToolItem;
begin
  result:= fList[index];
end;

procedure TToolList.SetItem(index: integer; Value: PToolItem);
begin
  fList[index]:= Value;
end;

function TToolList.GetCount: integer;
begin
  Packit;
  result:= fList.Count;
end;

function TToolList.AddItem(Value: PToolItem): integer;
begin
  result:= fList.Add(Value);
end;

procedure TToolList.MoveItem(CurIndex, NewIndex: integer);
begin
  fList.Move(CurIndex, NewIndex);
end;

procedure TToolList.RemoveItem(index: integer);
begin
  fList.Delete(index);
  Packit;
end;

procedure TToolList.LoadTools;
var
 Count,
 idx: integer;
 Item: PToolItem;
 Value,
 section: string;
begin
  if not FileExists(devDirs.Config + 'devcpp.cfg') then exit;
  with TINIFile.Create(devDirs.Config + 'devcpp.cfg') do
   try
    Count:= Readinteger('Tools', 'Count', 0);
    if Count <= 0 then exit;
    for idx:= 0 to pred(Count) do
     begin
       new(Item);
       Value:= '';
       section:= 'Tool'+inttostr(idx);
       Item^.Title:= ReadString(section, 'Title', '');
       Value:= ReadString(section, 'Program', '');
       value:= ParseString(value);
       Item^.Exec:= Value;
       Value:= ReadString(section, 'WorkDir', '');
       value:= ParseString(value);
       Item^.WorkDir:=  Value;
       Item^.Params:= ReadString(section, 'Params', '');
       Item^.IcoNumGnome:=-1;
       Item^.IcoNumBlue:=-1;
       Item^.IcoNumNewLook:=-1;
       Item^.HasIcon:=False;
       AddItem(Item);
     end;
   finally
    Free;
   end;
end;

procedure TToolList.SaveTools;
var
 tmp: TStringList;
 Count,
 idx: integer;
 Value,
 section: string;
 item: PToolItem;
begin
  with TINIFile.Create(devDirs.Config +'devcpp.cfg') do
   try
    // remove extra sections if items removed
    Count:= ReadInteger('Tools', 'Count', 0);
    if Count> fList.Count then
     begin
       tmp:= TStringList.Create;
       try
        ReadSections(tmp);
        for idx:= fList.Count to Count do
         EraseSection('Tool'+inttostr(idx));
       finally
        tmp.Free;
       end;
     end;

    for idx:= 0 to pred(fList.Count) do
     begin
       section:= 'Tool'+inttostr(idx);
       Item:= fList[idx];
       WriteString(section, 'Title', Item.Title);
       Value:= Item.Exec;
       Value:= ParseString(value);
       WriteString(section, 'Program', Value);

       Value:= Item.WorkDir;
       Value:= ParseString(Value);
       WriteString(section, 'WorkDir', Value);
       if (Item.Params <> '') and (Item.Params[1] = '"') and (Item.Params[length(Item.Params)] = '"') then // fix the case of param surrounded by quotes
         WriteString(section, 'Params', '"'+Item.Params+'"')
       else
         WriteString(section, 'Params', Item.Params);
     end;
    Writeinteger('Tools', 'Count', fList.Count);
   finally
    free;
   end;
end;

function TToolList.ParseString(const s: string): string;
begin
  result:= StringReplace(s, devDirs.Exec, '<EXECPATH>', [rfReplaceAll]);
  result:= StringReplace(Result, devDirs.Default, '<DEFAULT>', [rfReplaceAll]);
end;


 { TToolController }

constructor TToolController.Create;
begin
  inherited;
  fMenu:= nil;
  fOffset:= -1;
  fOnClick:= nil;
  fToolList:= TToolList.Create;
  fToolList.LoadTools;
end;

destructor TToolController.Destroy;
begin
  if assigned(fMenu) then fMenu.Clear;
  fToolList.SaveTools;
  fToolList.Free;
  inherited;
end;

{ ** enable/disable if not executable }
procedure TToolController.BuildMenu;
var
 idx: integer;
 Item: TMenuItem;
 Icon: TIcon;
 P: PChar;
 w: word;
 s: string;
begin
  if Assigned(fMenu) then
   idx:= fMenu.Count -1  //Clear Tools
  else
   idx:= -1;

  if idx> fOffset then
   repeat
    fMenu.Delete(idx);
    dec(idx);
   until idx = fOffset;

  if not Assigned(fMenu) then
    Exit;

  if fToolList.Count> 0 then
  //Rebuild menu
  for idx:= 0 to pred(fToolList.Count) do
   begin
     Item:= TMenuItem.Create(fMenu);
     Item.Caption:= fToolList.Items[idx].Title;
     Item.OnClick:= fOnClick;
     Item.Tag:= idx;
     if not fToolList.Items[idx].HasIcon then begin
       Icon:=TIcon.Create;
       try
         S:=StringReplace(fToolList.Items[idx].Exec, '<DEFAULT>', devDirs.Default, [rfReplaceAll]);
         S:=StringReplace(S, '<EXECPATH>', devDirs.Exec, [rfReplaceAll]);
         if FileExists(S) then begin
           P:=PChar(S);
           w:=0;
           Icon.Handle:=ExtractAssociatedIcon(hInstance, P, w);
           if Icon.Handle>0 then begin
             fToolList.Items[idx].IcoNumNewLook:=dmMain.MenuImages_NewLook.AddIcon(Icon);
             fToolList.Items[idx].IcoNumBlue:=dmMain.MenuImages_Blue.AddIcon(Icon);
             fToolList.Items[idx].IcoNumGnome:=dmMain.MenuImages_Gnome.AddIcon(Icon);
             fToolList.Items[idx].HasIcon:=True;
           end;
         end;
       finally
         Icon.Free;
       end;
     end;
     if devData.Theme=DEV_GNOME_THEME then
       Item.ImageIndex:=fToolList.Items[idx].IcoNumGnome
     else if devData.Theme=DEV_BLUE_THEME then
       Item.ImageIndex:=fToolList.Items[idx].IcoNumBlue
     else
       Item.ImageIndex:=fToolList.Items[idx].IcoNumNewLook;
     fMenu.Add(Item);
   end;
  fMenu.Visible:= fMenu.Count> 0;
end;

procedure TToolController.Edit;
begin
  with TToolForm.Create(nil) do
   try
     Controller:= Self;
     ShowModal;
     fToolList.SaveTools;
     BuildMenu;
   finally
    Free;
   end;
end;

  { TToolForm }

procedure TToolForm.btnEditClick(Sender: TObject);
var
 Item: PToolItem;
begin
  with TToolEditForm.Create(nil) do
   try
    Item:= fController.ToolList[ListBox.ItemIndex];
    edTitle.Text:= Item.Title;
    edProgram.Text:=fController.ToolList.ParseString(Item.Exec);
    edWorkDir.Text:= fController.ToolList.ParseString(Item.WorkDir);
    edParams.Text:= Item.Params;

     if ShowModal = mrOK then
      begin
        Item.Title:= edTitle.Text;
        Item.Exec:= fController.ToolList.ParseString(edProgram.Text);
        Item.WorkDir:= fController.ToolList.ParseString(edWorkDir.text);
        Item.Params:= edParams.Text;
        fController.ToolList[ListBox.ItemIndex]:= Item;
        UpdateList;
      end;
   finally
    Free;
    UpdateButtons;
   end;
end;

procedure TToolForm.btnAddClick(Sender: TObject);
var
 NewItem: PToolItem;
begin
  with TToolEditForm.Create(Self) do
   try
    new(NewItem);
    edTitle.Text := '';
    edProgram.Text := '';
    edWorkDir.Text := '';
    edParams.Text := '';
    if ShowModal = mrOK then
     begin
       NewItem.Title:= edTitle.Text;
       NewItem.Exec:= edProgram.Text;
       NewItem.WorkDir:= edWorkDir.Text;
       NewItem.Params:= edParams.Text;

       fController.ToolList.AddItem(NewItem);
       UpdateList;
     end;
   finally
    Free;
    UpdateButtons;
   end;
end;

procedure TToolForm.btnDeleteClick(Sender: TObject);
begin
  fController.ToolList.RemoveItem(Listbox.ItemIndex);
  ListBox.Items.Delete(ListBox.ItemIndex);
  UpdateButtons;
end;

procedure TToolForm.ListBoxClick(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TToolForm.PosbtnClick(Sender: TObject);
begin
  case (Sender as TSpeedButton).Tag of
   1: //move up
    begin
      fController.ToolList.MoveItem(ListBox.ItemIndex, ListBox.ItemIndex -1);
      ListBox.Items.Exchange(ListBox.ItemIndex, Listbox.ItemIndex -1);
    end;
   2: //move down
    begin
      fController.ToolList.MoveItem(ListBox.ItemIndex, ListBox.ItemIndex +1);
      ListBox.Items.Exchange(ListBox.ItemIndex, Listbox.ItemIndex +1);
    end;
  end;
  ListBox.SetFocus;
  UpdateButtons;
end;

procedure TToolForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  action:= caFree;
end;

procedure TToolForm.FormShow(Sender: TObject);
begin
  UpdateList;
end;

procedure TToolForm.UpdateList;
var
 idx: integer;
begin
  ListBox.Clear;
  for idx:= 0 to pred(fController.ToolList.Count) do
   ListBox.Items.Append(fController.ToolList[idx].Title);
  UpdateButtons;
end;

procedure TToolForm.UpdateButtons;
begin
  btnDown.Enabled:= (ListBox.ItemIndex <pred(Listbox.Count)) and
                    (ListBox.ItemIndex> -1);
  btnUp.Enabled:= ListBox.ItemIndex> 0;
  btnEdit.Enabled:= Listbox.ItemIndex> -1;
  btnDelete.Enabled:= ListBox.ItemIndex> -1;
end;

procedure TToolForm.LoadText;
begin
  if devData.XPTheme then
    XPMenu.Active := true
  else
    XPMenu.Active := false;
  Caption:=               Lang[ID_TF];
  grpCurrent.Caption:=    '  '+Lang[ID_TF_LABEL] +'  ';

  btnAdd.Caption:=        Lang[ID_BTN_ADD];
  btnDelete.Caption:=     Lang[ID_BTN_DELETE];
  btnEdit.Caption:=       Lang[ID_BTN_EDIT];
  btnClose.Caption:=      Lang[ID_BTN_CLOSE];
  grpActions.Caption:=    '  '+Lang[ID_HE_GRP_ACTIONS]+'  ';
end;

procedure TToolForm.FormCreate(Sender: TObject);
begin
  LoadText;
end;

end.
