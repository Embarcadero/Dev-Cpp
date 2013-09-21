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
  StdCtrls, Buttons, ShellAPI;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Classes, QMenus, QGraphics, QControls, QForms,
  QStdCtrls, QButtons;
{$ENDIF}

type
{ Tool List }

  PToolItem = ^TToolItem;
  TToolItem = record
   Title: AnsiString;
   Exec: AnsiString;
   WorkDir: AnsiString;
   Params: AnsiString;
   IcoNumGnome: integer;
   IcoNumBlue: integer;
   IcoNumNewLook: integer;
   HasIcon: boolean;
  end;

  TToolList = class
   private
    fList: TList;
    function GetItem(index: integer): PToolItem;
    procedure SetItem(index: integer; Value: PToolItem);
    function GetCount: integer;
   public
    constructor Create;
    destructor Destroy; override;
    function AddItem(Value: PToolItem): integer;
    procedure MoveItem(CurIndex, NewIndex: integer);
    procedure RemoveItem(index: integer);
    procedure LoadTools;
    procedure SaveTools;
    function ParseString(const S: AnsiString): AnsiString;

    property Items[index: integer]: PToolItem read GetItem write SetItem; default;
    property Count: integer read GetCount;
  end;

  TToolController = class
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

  TToolForm = class(TForm)
    grpCurrent: TGroupBox;
    ListBox: TListBox;
    btnUp: TSpeedButton;
    btnDown: TSpeedButton;
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

implementation

uses
  ToolEditFrm, inifiles, devcfg, utils, MultiLangSupport, datamod,
  version, main;

{$R *.dfm}

 { TToolList }

constructor TToolList.Create;
begin
	inherited;
	fList:= TList.Create;
end;

destructor TToolList.Destroy;
var
	I : integer;
begin
	for I := 0 to fList.Count - 1 do
		Dispose(PToolItem(fList[I]));
	fList.Free;
	inherited;
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
end;

procedure TToolList.LoadTools;
var
 Count,
 idx: integer;
 Item: PToolItem;
 Value,
 section: AnsiString;
begin
	if not FileExists(devDirs.Config + DEV_TOOLS_FILE) then exit;
	with TINIFile.Create(devDirs.Config + DEV_TOOLS_FILE) do
		try
			Count:= Readinteger('Tools', 'Count', 0);
			for idx:= 0 to pred(Count) do begin
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
 section: AnsiString;
 item: PToolItem;
begin
	if fList.Count = 0 then Exit; // don't bother saving empty files
	with TINIFile.Create(devDirs.Config + DEV_TOOLS_FILE) do
		try
			// remove extra sections if items removed
			Count := ReadInteger('Tools', 'Count', 0);
			if Count > fList.Count then begin
				tmp := TStringList.Create;
				try
					ReadSections(tmp);
					for idx := fList.Count to Count do
						eraseSection('Tool' + inttostr(idx));
				finally
					tmp.Free;
				end;
			end;

			for idx:= 0 to pred(fList.Count) do begin

				section := 'Tool' + inttostr(idx);
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
			Free;
		end;
end;

function TToolList.ParseString(const s: AnsiString): AnsiString;
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
	I: integer;
	Item: TMenuItem;
	Icon: TIcon;
	s: AnsiString;
	w: word;
begin
	if Assigned(fMenu) then
		I:= fMenu.Count - 1 // Clear Tools menu
	else
		I:= -1;

	if I > fOffset then
		repeat
			fMenu.Delete(I);
			Dec(I);
		until I = fOffset;

	if not Assigned(fMenu) then
		Exit;

	// Rebuild menu
	if fToolList.Count > 0 then begin

		// Create a separator
		Item:= TMenuItem.Create(fMenu);
		Item.Caption:= '-';
		fMenu.Add(Item);

		// Pass all menu items
		for I:=0 to pred(fToolList.Count) do begin
			Item:= TMenuItem.Create(fMenu);
			Item.Caption:= fToolList.Items[I].Title;
			Item.OnClick:= fOnClick;
			Item.Tag:= I;

			// If it doesn't have an icon already
			if not fToolList.Items[I].HasIcon then begin
				Icon:=TIcon.Create;
				try

					S:=StringReplace(fToolList.Items[I].Exec, '<DEFAULT>', devDirs.Default, [rfReplaceAll]);
					S:=StringReplace(S, '<EXECPATH>', devDirs.Exec, [rfReplaceAll]);

					if FileExists(S) then begin

						// Er moet een variabele meegegeven worden
						w:=0;
						Icon.Handle:=ExtractAssociatedIcon(hInstance, PAnsiChar(S), w);

						// Add the icon to the image lists if it exists
						if Icon.Handle <> 0 then begin

							// Add it to every theme
							fToolList.Items[I].IcoNumNewLook:=dmMain.MenuImages_NewLook.AddIcon(Icon);
							fToolList.Items[I].IcoNumBlue:=dmMain.MenuImages_Blue.AddIcon(Icon);
							fToolList.Items[I].IcoNumGnome:=dmMain.MenuImages_Gnome.AddIcon(Icon);
							fToolList.Items[I].HasIcon:=True;

							// Setting the image index to 'not -1' should do the trick
							if devData.Theme=DEV_GNOME_THEME then
								Item.ImageIndex:=fToolList.Items[I].IcoNumGnome
							else if devData.Theme=DEV_BLUE_THEME then
								Item.ImageIndex:=fToolList.Items[I].IcoNumBlue
							else
								Item.ImageIndex:=fToolList.Items[I].IcoNumNewLook;
						end;
					end;
				finally
					Icon.Free;
				end;
			end;
			fMenu.Add(Item);
		end;
	end;

	fMenu.Visible:= fMenu.Count > 0;
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
	// Set interface font
	Font.Name := devData.InterfaceFont;
	Font.Size := devData.InterfaceFontSize;

  Caption:=               Lang[ID_TF];
  grpCurrent.Caption:=    '  '+Lang[ID_TF_LABEL] +'  ';

  btnAdd.Caption:=        Lang[ID_BTN_ADD];
  btnDelete.Caption:=     Lang[ID_BTN_DELETE];
  btnEdit.Caption:=       Lang[ID_BTN_EDIT];
  grpActions.Caption:=    '  '+Lang[ID_HE_GRP_ACTIONS]+'  ';
end;

procedure TToolForm.FormCreate(Sender: TObject);
begin
  LoadText;
end;

end.
