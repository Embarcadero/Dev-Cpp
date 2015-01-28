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
    btnResetAll: TButton;
    lblTip: TLabel;
    btnResetCurrent: TButton;
    procedure lvShortcutsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lvShortcutsCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var
      DefaultDraw: Boolean);
    procedure lvShortcutsExit(Sender: TObject);
    procedure btnResetAllClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnResetCurrentClick(Sender: TObject);
  private
    fReplaceHint: AnsiString;
    fResetAllConfirm: AnsiString;
    fResetCurrentConfirm: AnsiString;
    function GetShortCut(Index: integer): PShortCutItem;
  public
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure AddShortcut(Item: PShortcutItem);
    procedure Clear;
    function Count: integer;
    property ShortCuts[Index: integer]: PShortCutItem read GetShortCut;
    procedure LoadText(const WindowCaption, Column1, Column2, Tip, OK, Cancel, ResetAll, ResetCurrent, ReplaceHint,
      ResetAllConfirm, ResetCurrentConfirm: AnsiString);
  end;

var
  frmShortcutsEditor: TfrmShortcutsEditor;

implementation

uses
  StrUtils;

{$R *.dfm}

procedure TfrmShortcutsEditor.LoadText(const WindowCaption, Column1, Column2, Tip, OK, Cancel, ResetAll, ResetCurrent,
  ReplaceHint, ResetAllConfirm, ResetCurrentConfirm: AnsiString);
begin
  Caption := WindowCaption;
  lvShortcuts.Columns[0].Caption := Column1;
  lvShortcuts.Columns[1].Caption := Column2;
  lblTip.Caption := Tip;
  btnOk.Caption := OK;
  btnCancel.Caption := Cancel;
  btnResetAll.Caption := ResetAll;
  btnResetCurrent.Caption := ResetCurrent;
  fReplaceHint := ReplaceHint;
  fResetAllConfirm := ResetAllConfirm;
  fResetCurrentConfirm := ResetCurrentConfirm;
end;

procedure TfrmShortcutsEditor.BeginUpdate;
begin
  lvShortcuts.Items.BeginUpdate;
end;

procedure TfrmShortcutsEditor.EndUpdate;
begin
  lvShortcuts.Items.EndUpdate;
end;

procedure TfrmShortcutsEditor.AddShortcut(Item: PShortcutItem);
begin
  with lvShortcuts.Items.Add do begin
    Caption := Item^.ListEntry;
    if Assigned(item^.MenuItem) then
      SubItems.Add(ShortCutToText(Item^.MenuItem.ShortCut))
    else if Assigned(item^.Action) then
      SubItems.Add(ShortCutToText(Item^.Action.ShortCut))
    else
      SubItems.Add('');
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

function TfrmShortcutsEditor.GetShortCut(Index: integer): PShortCutItem;
begin
  Result := PShortCutItem(lvShortcuts.Items[Index].Data);
end;

procedure TfrmShortcutsEditor.lvShortcutsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  I, oldindex: integer;
  IntShortCut: TShortCut;

  // Handy macro. Please use
  procedure SetShortCut(index: integer; ShortCut: integer);
  begin
    lvShortcuts.Items[index].SubItems[0] := ShortCutToText(ShortCut);
    PShortCutItem(lvShortcuts.Items[index].Data)^.Temporary := IntShortCut;
  end;
begin
  // Require a selection
  if lvShortcuts.Selected = nil then
    Exit;

  // clear shortcut if ONLY Escape or Del is pressed
  if (Key in [VK_ESCAPE, VK_DELETE]) and (Shift = []) then begin
    SetShortCut(lvShortcuts.ItemIndex, 0);
    Exit;
  end;

  // Don't accept alt shiftstate (messes up menu interaction)
  if (Shift = [ssAlt]) then
    Exit;

  // Require a key combination when using 'normal' keys
  if (Shift = []) and ((Key < VK_F1) or (Key > VK_F24)) then
    Exit; // ... but allow F keys

  // Don't accept shift state keys as main keys
  if (Key in [VK_SHIFT, VK_CONTROL, VK_MENU]) then // VK_MENU is the alt key
    Exit;

  IntShortCut := ShortCut(Key, Shift);

  oldindex := -1;
  for I := 0 to lvShortcuts.Items.Count - 1 do

    // Don't scan popups, they can contain duplicate shortcuts (they're picked based on focus)
    if (lvShortcuts.Items[I] <> lvShortcuts.Selected) and (Pos('Popup', lvShortcuts.Items[I].Caption) = 0) then
      if PShortCutItem(lvShortcuts.Items[I].Data)^.Temporary = IntShortCut then begin
        oldindex := i;
        break;
      end;

  // Can be written in a more compact form, but I prefer readability
  if oldindex <> -1 then begin // already in use
    if MessageDlg(Format(fReplaceHint, [lvShortcuts.Items[oldindex].Caption,
      lvShortcuts.Items[lvShortcuts.ItemIndex].Caption]), mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
      SetShortCut(oldindex, 0); // remove old
      SetShortCut(lvShortcuts.ItemIndex, IntShortCut); // set new
    end;
  end else
    SetShortCut(lvShortcuts.ItemIndex, IntShortCut); // set new

  // don't let the keystroke propagate...
  Key := 0;
end;

procedure TfrmShortcutsEditor.lvShortcutsCustomDrawItem(Sender: TCustomListView; Item: TListItem; State:
  TCustomDrawState; var DefaultDraw: Boolean);
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
  key: word;
  shift: TShiftState;
  State: TKeyboardState;
begin
  // Don't let Tab and Alt combinations escape
  if (GetKeyState(VK_TAB) < 0) then
    key := VK_TAB
      //  else if (GetKeyState(VK_MENU) < 0) then
//    key := VK_MENU
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

  lvShortcutsKeyDown(Self, key, Shift);

  lvShortcuts.SetFocus; // retain focus!
end;

procedure TfrmShortcutsEditor.btnResetAllClick(Sender: TObject);
var
  I: integer;
  Item: PShortCutItem;
begin
  if MessageDlg(fResetAllConfirm, mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
    lvShortcuts.Items.BeginUpdate;
    try
      for I := 0 to lvShortcuts.Items.Count - 1 do begin
        Item := PShortCutItem(lvShortcuts.Items[I].Data);
        Item^.Temporary := Item^.Default;
        lvShortcuts.Items[I].SubItems[0] := ShortCutToText(Item^.Temporary);
      end;
    finally
      lvShortcuts.Items.EndUpdate;
    end;
  end;
end;

procedure TfrmShortcutsEditor.FormCreate(Sender: TObject);
begin
  lvShortcuts.DoubleBuffered := true; // performance hit, but it's worth it
end;

procedure TfrmShortcutsEditor.btnResetCurrentClick(Sender: TObject);
var
  Item: PShortCutItem;
begin
  if lvShortcuts.ItemIndex = -1 then
    Exit;

  // Replace selection
  if MessageDlg(Format(fResetCurrentConfirm, [lvShortcuts.Items[lvShortcuts.ItemIndex].Caption]), mtConfirmation,
    [mbYes, mbNo], 0) = mrYes then begin
    Item := PShortCutItem(lvShortcuts.Items[lvShortcuts.ItemIndex].Data);
    Item^.Temporary := Item^.Default;
    lvShortcuts.Items[lvShortcuts.ItemIndex].SubItems[0] := ShortCutToText(Item^.Temporary);
  end;
end;

end.

