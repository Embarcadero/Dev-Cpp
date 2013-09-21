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
  TMenuShortcut = record
    Default: TShortCut;
    IniEntry: AnsiString; // save untranslated
    MenuItem: TMenuItem; // pointer, caption can change, so use above
  end;
  PMenuShortcut = ^TMenuShortcut;

  TdevShortcuts = class(TComponent)
  private
    fOwner: TComponent;
    fFilename: TFileName;
    fShortcuts: TList;
    function GetTopmostItemAncestor(Item: TMenuItem): AnsiString;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Load;
    procedure Edit;
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
	I : integer;
begin
	for I := 0 to fShortcuts.Count - 1 do
		Dispose(PMenuShortCut(fShortcuts[i]));
	fShortcuts.Free;
	inherited;
end;

function TdevShortcuts.GetTopmostItemAncestor(Item: TMenuItem): AnsiString;
var
  CurMenu: TMenu;
begin
  Result:=Item.GetParentMenu.Name;
  CurMenu:=Item.GetParentMenu;
  if CurMenu is TMainMenu then
    while Item<>nil do
    begin
      if Item.Caption<>'' then Result:=Item.Caption;
      Item:=Item.Parent;
    end
end;

procedure TdevShortcuts.Load;
var
	I: integer;
	Fini: TIniFile;
	value: AnsiString;
	ShortCut: TShortCut;
	item: PMenuShortcut;
	MenuItem: TMenuItem;
	Actions: TStringList;
begin
	if fOwner = nil then
		Exit;

	// Create a list of all menu items that, UNTRANSLATED!
	Actions := TStringList.Create;
	try

		// Add all main menu items...
		for I := 0 to fOwner.ComponentCount - 1 do begin
			if (fOwner.Components[I] is TMenuItem) then begin
				MenuItem := TMenuItem(fOwner.Components[I]);
				if (not MenuItem.IsLine) and (MenuItem.Count = 0) then begin // don't process foldouts and separators

					// Don't add if the main menu counterpart or their action has been added
					if Assigned(MenuItem.Action) and (Actions.IndexOf(MenuItem.Action.Name) <> -1) then
						continue;

					item := new(PMenuShortcut);
					item^.Default := MenuItem.ShortCut;
					item^.IniEntry := StripHotkey(GetTopmostItemAncestor(MenuItem)) + ':' + StripHotkey(MenuItem.Caption);
					item^.MenuItem := MenuItem;
					fShortcuts.Add(item);

					// Store action of main menu item, and don't create duplicates for popup menu items with the same action
					if Assigned(MenuItem.Action) then
						if (fOwner.Components[I] is TMenuItem) and (MenuItem.GetParentMenu is TMainMenu) then
							Actions.Add(MenuItem.Action.Name);
				end;
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
			item := PMenuShortcut(fShortcuts[i]);
			MenuItem := item^.MenuItem;

			// Read shortcut, assume ini file is untranslated
			value := Fini.ReadString('Shortcuts', item^.IniEntry, '');
			if value <> 'none' then
				shortcut := TextToShortCut(value)
			else
				shortcut := 0;

			// Assign it
			MenuItem.ShortCut := shortcut;
			if Assigned(MenuItem.Action) then
				TAction(MenuItem.Action).ShortCut := shortcut;
		end;
	finally
		Fini.Free;
	end;
end;

procedure TdevShortcuts.Edit;
var
	I : integer;
	item: PMenuShortcut;
	MenuItem: TMenuItem;
begin
	frmShortcutsEditor := TfrmShortcutsEditor.Create(Self);
	with frmShortcutsEditor do try
		Clear;

		// Use the preloaded list, do not walk the Components list again
		for I := 0 to fShortcuts.Count - 1 do begin
			item := PMenuShortcut(fShortcuts[i]);
			MenuItem := item^.MenuItem;
			AddShortcut(item,GetTopmostItemAncestor(MenuItem)); // display translated caption
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
	item: PMenuShortcut;
	MenuItem: TMenuItem;
begin
	if fFileName = '' then
		Exit;

	Fini := TIniFile.Create(fFileName);
	try
		with frmShortcutsEditor do begin
			for I := 0 to Count - 1 do begin
				item := PMenuShortcut(fShortcuts[i]);
				MenuItem := item^.MenuItem;

				// Apply to UI
				MenuItem.ShortCut := ShortCuts[I]; // ShortCuts is the UI list
				if Assigned(MenuItem.Action) then
					TAction(MenuItem.Action).ShortCut := ShortCuts[I];

				// Save untranslated
				entry := item^.IniEntry; // save untranslated
				value := ShortCutToText(MenuItem.ShortCut);
				Fini.WriteString('Shortcuts', entry, value);
			end;
		end;
	finally
		Fini.Free;
	end;
end;

end.

