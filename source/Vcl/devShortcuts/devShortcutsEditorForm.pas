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

unit devShortcutsEditorForm;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls, Menus, devShortcuts;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Variants, Classes, QGraphics, QControls, QForms,
  QDialogs, QExtCtrls, QComCtrls, QStdCtrls, QMenus;
{$ENDIF}

type
  TfrmShortcutsEditor = class(TForm)
    lvShortcuts: TListView;
    btnOk: TButton;
    btnCancel: TButton;
    btnDefault: TButton;
    procedure lvShortcutsKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
    procedure lvShortcutsCustomDrawItem(Sender: TCustomListView;Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure lvShortcutsExit(Sender: TObject);
    procedure btnDefaultClick(Sender: TObject);
  private
    function GetShortCut(Index: integer): TShortCut;
  public
    procedure AddShortcut(Item : PMenuShortcut;const MenuName : AnsiString);
    procedure Clear;
    function Count: integer;
    property ShortCuts[Index: integer]: TShortCut read GetShortCut;
    procedure LoadText(const WindowCaption,Column1,Column2,OK,Cancel,Default : AnsiString);
  end;

var
  frmShortcutsEditor: TfrmShortcutsEditor;

implementation

uses
	StrUtils;

{$R *.dfm}

procedure TfrmShortcutsEditor.LoadText(const WindowCaption,Column1,Column2,OK,Cancel,Default : AnsiString);
begin
	Caption := WindowCaption;
	lvShortcuts.Columns[0].Caption := Column1;
	lvShortcuts.Columns[1].Caption := Column2;
	btnOk.Caption := OK;
	btnCancel.Caption := Cancel;
	btnDefault.Caption := Default;
end;

procedure TfrmShortcutsEditor.AddShortcut(Item : PMenuShortcut;const MenuName : AnsiString);
var
	MenuItem: TMenuItem;
begin
	MenuItem := item^.MenuItem;

	// ???
	//if (M.Action<>nil) and (LeftStr(M.Action.Name,6)='dynact') then
	//	Exit;

	with lvShortcuts.Items.Add do begin
		Caption := StripHotkey(MenuName + ' >> ' + MenuItem.Caption);
		SubItems.Add(ShortCutToText(MenuItem.ShortCut));
		Data := Item;
	end;
end;

procedure TfrmShortcutsEditor.Clear;
begin
	lvShortcuts.Clear;
end;

function TfrmShortcutsEditor.Count: integer;
begin
	Result := lvShortcuts.Items.Count;
end;

function TfrmShortcutsEditor.GetShortCut(Index: integer): TShortCut;
begin
	Result := TextToShortCut(lvShortcuts.Items[Index].SubItems[0]);
end;

procedure TfrmShortcutsEditor.lvShortcutsKeyDown(Sender: TObject;var Key: Word; Shift: TShiftState);
var
  I: integer;
  sct: AnsiString;
begin
	// Require a selection
	if lvShortcuts.Selected = nil then
		Exit;

	// clear shortcut if ONLY Escape or Del is pressed
	if (Key in [VK_ESCAPE,VK_DELETE]) and (Shift = []) then begin
		lvShortcuts.Selected.SubItems[0] := '';
		Exit;
	end;

	// Don't accept alt shiftstate (messes up menu interaction)
	if (Shift = [ssAlt]) then
		Exit;

	// Require a key combination when using 'normal' keys
	if (Shift = []) and ((Key < VK_F1) or (Key > VK_F24)) then
		Exit; // ... but allow F keys

	// Don't accept shift state keys as main keys
	if (Key in [VK_SHIFT,VK_CONTROL,VK_MENU]) then // VK_MENU is the alt key
		Exit;

	sct := ShortCutToText(ShortCut(Key, Shift));
	lvShortcuts.Selected.SubItems[0] := sct;

	// search other entries using this shortcut, and clear them
	for I:=0 to lvShortcuts.Items.Count-1 do
		if lvShortcuts.Items[I]<>lvShortcuts.Selected then
			if lvShortcuts.Items[I].SubItems[0] = sct then
				lvShortcuts.Items[I].SubItems[0] := '';

	// don't let the keystroke propagate...
	Key := 0;
end;

procedure TfrmShortcutsEditor.lvShortcutsCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;var DefaultDraw: Boolean);
begin
	with Sender.Canvas do begin
		if not (cdsSelected in State) then begin
			if Item.Index mod 2 = 0 then
				Brush.Color := clWhite
			else
				Brush.Color := $E0E0E0;
		end;
	end;
end;

procedure TfrmShortcutsEditor.lvShortcutsExit(Sender: TObject);
var
	key : word;
	shift : TShiftState;
	State : TKeyboardState;
begin
	// Don't let Tab and Alt combinations escape
	if (GetKeyState(VK_TAB) < 0) then
		key := VK_TAB
	//else if (GetKeyState(VK_MENU) < 0) then
	//	key := VK_MENU
	else
		Exit;

	// Get shift state
	GetKeyboardState(State);
	if ((State[vk_Control] and 128) <> 0) then
		shift := shift + [ssCtrl];
	if ((State[vk_Menu] and 128) <> 0) then
		shift := shift + [ssAlt];
	if ((State[vk_Shift] and 128) <> 0) then
		shift := shift + [ssShift];

	lvShortcutsKeyDown(Self,key,Shift);

	lvShortcuts.SetFocus; // retain focus!
end;

procedure TfrmShortcutsEditor.btnDefaultClick(Sender: TObject);
var
	I : integer;
begin
	lvShortcuts.Items.BeginUpdate;
	for I := 0 to lvShortcuts.Items.Count -1  do
		lvShortcuts.Items[I].SubItems[0] := ShortCutToText(PMenuShortcut(lvShortcuts.Items[I].Data)^.Default);
	lvShortcuts.Items.EndUpdate;
end;

end.

