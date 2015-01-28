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

unit devShortcuts;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Classes, Menus, Controls, IniFiles, Graphics, ActnList;
{$ENDIF}
{$IFDEF LINUX}
SysUtils, Classes, QMenus, QControls, IniFiles, QGraphics, QActnList;
{$ENDIF}

type
  TShortCutItem = record
    Default: TShortCut;
    Temporary: TShortCut; // we can't use UI strings to store temp shortcuts (not one to one), so use this instead
    IniEntry: AnsiString; // name in ini file, use untranslated MenuItem.Caption
    ListEntry: AnsiString; // name in editor form
    MenuItem: TMenuItem; // apply Current to this
    Action: TAction; // OR this
  end;
  PShortCutItem = ^TShortCutItem;

  TdevShortcuts = class(TComponent)
  private
    fOwner: TComponent;
    fFilename: TFileName;
    fShortcuts: TList;
    function GetItemDescription(Item: TMenuItem): AnsiString;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Load(List: TActionList);
    procedure Edit(const WindowCaption, Column1, Column2, Tip, OK, Cancel, ResetAll, ResetCurrent, ReplaceHint, ResetAllConfirm, ResetCurrentConfirm, Button: AnsiString);
    procedure Save;
  published
    property Filename: TFilename read fFilename write fFilename;
  end;

procedure Register;

implementation

{$IFDEF WIN32}
uses
  devShortcutsEditorForm, Forms;
{$ENDIF}
{$IFDEF LINUX}
uses
  devShortcutsEditorForm, QForms;
{$ENDIF}

procedure Register;
begin
  RegisterComponents('Dev-C++', [TdevShortcuts]);
end;

{ TdevShortcuts }

constructor TdevShortcuts.Create(AOwner: TComponent);
begin
  inherited;
  fOwner := AOwner;
  fFileName := 'shortcuts.ini'; // DEV_SHORTCUTS_FILE
  fShortcuts := TList.Create;
end;

destructor TdevShortcuts.Destroy;
var
  I: integer;
begin
  for I := 0 to fShortcuts.Count - 1 do
    Dispose(PShortcutItem(fShortcuts[i]));
  fShortcuts.Free;
  inherited;
end;

function TdevShortcuts.GetItemDescription(Item: TMenuItem): AnsiString;
var
  ParentItem: TMenuItem;
begin
  Result := StripHotkey(Item.Caption);
  ParentItem := Item.Parent;
  while ParentItem <> nil do begin
    if ParentItem.Caption <> '' then // menu roots node have no caption
      Result := StripHotkey(ParentItem.Caption) + ' > ' + Result
    else begin
      Result := ParentItem.GetParentMenu.Name + ' > ' + Result;
      Exit;
    end;
    ParentItem := ParentItem.Parent;
  end;
end;

procedure TdevShortcuts.Load(List: TActionList);
var
  I, intvalue: integer;
  Fini: TIniFile;
  value: AnsiString;
  ShortCut: TShortCut;
  item: PShortcutItem;
  MenuItem: TMenuItem;
  Actions: TStringList;
  Action: TAction;
begin
  if fOwner = nil then
    Exit;

  // Create a list of all menu items that, UNTRANSLATED!
  Actions := TStringList.Create;
  try

    // Add all main menu items...
    for I := 0 to fOwner.ComponentCount - 1 do begin
      if fOwner.Components[I] is TMenuItem then begin
        MenuItem := TMenuItem(fOwner.Components[I]);
        if (not MenuItem.IsLine) and (MenuItem.Count = 0) then begin // don't process foldouts and separators

          // Don't add if the main menu counterpart or their action has been added
          if Assigned(MenuItem.Action) and (Actions.IndexOf(MenuItem.Action.Name) <> -1) then
            Continue;

          item := new(PShortcutItem);
          item^.Default := MenuItem.ShortCut;
          item^.Temporary := MenuItem.ShortCut;
          item^.IniEntry := GetItemDescription(MenuItem);
          item^.ListEntry := ''; // to be filled by form (translated)
          item^.MenuItem := MenuItem;
          item^.Action := TAction(MenuItem.Action);
          fShortcuts.Add(item);
        end;

        // Store action of main menu item, and don't create duplicates for popup menu items with the same action
        if Assigned(MenuItem.Action) then
          Actions.Add(MenuItem.Action.Name);
      end;
    end;

    // Lastly, add actions without a menu item
    for I := 0 to List.ActionCount - 1 do begin
      Action := TAction(List.Actions[i]);
      if Actions.IndexOf(Action.Name) = -1 then begin // forgot this action...

        item := new(PShortcutItem);
        item^.Default := Action.ShortCut;
        item^.Temporary := Action.ShortCut;
        item^.IniEntry := Action.Caption;
        item^.ListEntry := ''; // to be filled by form (translated)
        item^.MenuItem := nil;
        item^.Action := Action;
        fShortcuts.Add(item);
      end;
    end;
  finally
    Actions.Free;
  end;

  if (fFileName = '') or not FileExists(fFileName) then
    Exit;

  // For each menu item in our list, read the shortcut from disk
  Fini := TIniFile.Create(fFileName);
  try
    for I := 0 to fShortcuts.Count - 1 do begin
      item := PShortCutItem(fShortcuts[i]);

      // Read shortcut, assume ini file is untranslated
      value := Fini.ReadString('Shortcuts', item^.IniEntry, '');
      if (value <> '') then begin // only apply when found in file

        // New format: unsigned int value
        intvalue := StrToIntDef(value, High(ShortCut) + 1);
        if intvalue = High(ShortCut) + 1 then // old format...
          shortcut := TextToShortCut(value)
        else // new format
          shortcut := intvalue;

        // Apply to Menu
        if Assigned(item^.MenuItem) then
          item^.MenuItem.ShortCut := shortcut;

        // Apply to action
        if Assigned(item^.Action) then
          item^.Action.ShortCut := shortcut;

        // Store shortcut in int format
        item^.Temporary := shortcut;
      end;
    end;
  finally
    Fini.Free;
  end;
end;

procedure TdevShortcuts.Edit(const WindowCaption, Column1, Column2, Tip, OK, Cancel, ResetAll, ResetCurrent, ReplaceHint, ResetAllConfirm, ResetCurrentConfirm, Button: AnsiString);
var
  I: integer;
  item: PShortcutItem;
begin
  frmShortcutsEditor := TfrmShortcutsEditor.Create(Self);
  with frmShortcutsEditor do try
    // translate on the fly, can't use devMultilanguage here...
    LoadText(WindowCaption, Column1, Column2, Tip, OK, Cancel, ResetAll, ResetCurrent, ReplaceHint, ResetAllConfirm,
      ResetCurrentConfirm);

    // Use the preloaded list, do not walk the Components list again
    BeginUpdate;
    try
      for I := 0 to fShortcuts.Count - 1 do begin
        item := PShortcutItem(fShortcuts[i]);
        if Assigned(item^.MenuItem) then
          item^.ListEntry := GetItemDescription(item^.MenuItem)
        else if Assigned(item^.Action) then
          item^.ListEntry := '(' + Button + ') > ' + StripHotkey(item^.Action.Caption)
        else
          item^.ListEntry := '';
        AddShortcut(item); // display translated caption
      end;
    finally
      EndUpdate; // repaint once
    end;

    if ShowModal = mrOK then
      Save;
  finally
    frmShortcutsEditor.Free;
  end;
end;

procedure TdevShortcuts.Save;
var
  I: integer;
  Fini: TIniFile;
  entry, value: AnsiString;
  item: PShortcutItem;
begin
  if fFileName = '' then
    Exit;

  Fini := TIniFile.Create(fFileName);
  try
    with frmShortcutsEditor do begin
      for I := 0 to Count - 1 do begin
        item := PShortCutItem(fShortcuts[i]);

        // Apply to Menu
        if Assigned(item^.MenuItem) then
          item^.MenuItem.ShortCut := ShortCuts[I]^.Temporary; // ShortCuts is the UI list

        // Apply to action
        if Assigned(item^.Action) then
          item^.Action.ShortCut := ShortCuts[I]^.Temporary;

        // Save untranslated
        entry := item^.IniEntry;
        value := IntToStr(ShortCuts[I]^.Temporary); // save in new format
        Fini.WriteString('Shortcuts', entry, value);
      end;
    end;
  finally
    Fini.Free;
  end;
end;

end.

