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
  TMenuAndShortcut = record
    Caption: AnsiString;
    Shortcut: TShortCut;
  end;
  PMenuAndShortcut = ^TMenuAndShortcut;

  TdevShortcuts = class(TComponent)
  private
    { Private declarations }
    fOwner: TComponent;
    fAltColor: TColor;
    fFilename: TFileName;
    procedure ReadShortcuts;
    procedure Save;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Load;
    procedure Edit;
  published
    { Published declarations }
    property Filename: TFilename read fFilename write fFilename;
    property AlternateColor: TColor read fAltColor write fAltColor;
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
  fAltColor := $E0E0E0;
end;

destructor TdevShortcuts.Destroy;
begin
  inherited;
end;

procedure TdevShortcuts.Edit;
begin
  frmShortcutsEditor := TfrmShortcutsEditor.Create(Self);
  with frmShortcutsEditor do
  try
    AltColor := fAltColor;
    Clear;
    ReadShortcuts;
    if ShowModal = mrOK then
      Save;
  finally
    frmShortcutsEditor.Free;
  end;
end;

function GetTopmostItemAncestor(Item: TMenuItem): AnsiString;
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
	item: TMenuItem;
	entry, value: AnsiString;
	shortcut: TShortCut;
begin
	if fOwner = nil then
		Exit;
	if (fFileName = '') or not FileExists(fFileName) then
		Exit;

	// Read shortcuts for all menu items
	Fini := TIniFile.Create(fFileName);
	try
		for I := 0 to fOwner.ComponentCount - 1 do begin
			if (fOwner.Components[I] is TMenuItem) then begin
				item := TMenuItem(fOwner.Components[I]);
				if (not item.IsLine) and (item.Count = 0) then begin // don't process foldouts and separators

					// Read entry from disk
					entry := StripHotkey(GetTopmostItemAncestor(item)) + ':' + StripHotkey(item.Caption);
					value := Fini.ReadString('Shortcuts', entry, ''); // shortcut in text format
					if value <> 'none' then
						shortcut := TextToShortCut(value)
					else
						shortcut := 0;

					// Assign it
					item.ShortCut := shortcut;
					if Assigned(item.Action) then
						TAction(item.Action).ShortCut := shortcut;
				end;
			end;
		end;
	finally
		Fini.Free;
	end;
end;

procedure TdevShortcuts.ReadShortcuts;
var
  I: integer;
  Actions: THashedStringList;
  MenuItem: TMenuItem;
begin
  if fOwner = nil then
    Exit;
  Actions:=THashedStringList.Create;
  for I := 0 to fOwner.ComponentCount - 1 do
    if fOwner.Components[I] is TMenuItem and
      (TMenuItem(fOwner.Components[I]).GetParentMenu is TMainMenu) then
      begin
        MenuItem:=TMenuItem(fOwner.Components[I]);
        if not MenuItem.IsLine then
          if MenuItem.Count = 0 then
          begin
            if Assigned(MenuItem.Action) and (MenuItem.Action.Name<>'') and
               (Actions.IndexOf(MenuItem.Action.Name)=-1) then
                Actions.Add(MenuItem.Action.Name);
            frmShortcutsEditor.AddShortcut(TMenuItem(fOwner.Components[I]),
              GetTopmostItemAncestor(TMenuItem(fOwner.Components[I])));
          end;
      end;
  for I := 0 to fOwner.ComponentCount - 1 do
    if fOwner.Components[I] is TMenuItem and
      (TMenuItem(fOwner.Components[I]).GetParentMenu is TPopupMenu)
      then
      begin
        MenuItem:=TMenuItem(fOwner.Components[I]);
        if not MenuItem.IsLine and (MenuItem.Count=0) and
           ((not Assigned(MenuItem.Action)) or (MenuItem.Action.Name='') or
                (Actions.IndexOf(MenuItem.Action.Name)=-1))
        then
        begin
          frmShortcutsEditor.AddShortcut(TMenuItem(fOwner.Components[I]),
            GetTopmostItemAncestor(TMenuItem(fOwner.Components[I])));
        end;
      end;
end;

procedure TdevShortcuts.Save;
var
  I: integer;
  Fini: TIniFile;
  Smenu: AnsiString;
  Scut: AnsiString;
begin
	if fFileName = '' then
		Exit;
	Fini := TIniFile.Create(fFileName);
	with frmShortcutsEditor do begin
		for I := 0 to Count - 1 do begin
			Items[I].ShortCut := ShortCuts[I];
			if Assigned(Items[I].Action) then
				TAction(Items[I].Action).ShortCut := ShortCuts[I];
			Smenu := StripHotkey(GetTopmostItemAncestor(Items[I]))+':'+StripHotkey(Items[I].Caption);
			Scut := ShortCutToText(ShortCuts[I]);
			if Scut = '' then
				Scut := 'none';
			Fini.WriteString('Shortcuts', Smenu, Scut);
		end;
	end;
end;

end.

