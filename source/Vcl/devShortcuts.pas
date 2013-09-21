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
  TmlStrings = class(TPersistent)
  private
    fCaption: string;
    fTitle: string;
    fTip: string;
    fHeaderEntry: string;
    fHeaderShortcut: string;
    fOK: string;
    fCancel: string;
  protected
  public
  published
    property Caption: string read fCaption write fCaption;
    property Title: string read fTitle write fTitle;
    property Tip: string read fTip write fTip;
    property HeaderEntry: string read fHeaderEntry write fHeaderEntry;
    property HeaderShortcut: string read fHeaderShortcut write fHeaderShortcut;
    property OK: string read fOK write fOK;
    property Cancel: string read fCancel write fCancel;
  end;

  TdevShortcuts = class(TComponent)
  private
    { Private declarations }
    fOwner: TComponent;
    fAltColor: TColor;
    fFilename: TFileName;
    fMLStrings: TmlStrings;
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
    property MultiLangStrings: TmlStrings read fMLStrings write fMLStrings;
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
  RegisterComponents('dev-c++', [TdevShortcuts]);
end;

{ TdevShortcuts }

constructor TdevShortcuts.Create(AOwner: TComponent);
begin
  inherited;
  fOwner := AOwner;
  fFileName := 'shortcuts.cfg';
  fAltColor := $E0E0E0;
  fMLStrings := TMLStrings.Create;
  with fMLStrings do begin
    Caption := 'Configure Shortcuts';
    Title := ' Click on an item and press the shortcut you desire!';
    Tip := 'Tip: press "Escape" to clear a shortcut...';
    HeaderEntry := 'Menu entry';
    HeaderShortcut := 'Shortcut assigned';
    OK := 'OK';
    Cancel := 'Cancel';
  end;
end;

destructor TdevShortcuts.Destroy;
begin
  fMLStrings.Free;
  inherited;
end;

procedure TdevShortcuts.Edit;
begin
  frmShortcutsEditor := TfrmShortcutsEditor.Create(Self);
  with frmShortcutsEditor do
  try
    AltColor := fAltColor;
    Caption := fMLStrings.Caption;
    lblTitle.Caption := fMLStrings.Title;
    lblTip.Caption := fMLStrings.Tip;
    lvShortcuts.Column[0].Caption := fMLStrings.HeaderEntry;
    lvShortcuts.Column[1].Caption := fMLStrings.HeaderShortcut;
    btnOk.Caption := fMLStrings.OK;
    btnCancel.Caption := fMLStrings.Cancel;
    Clear;
    ReadShortcuts;
    if ShowModal = mrOK then
      Save;
  finally
    frmShortcutsEditor.Free;
  end;
end;

function GetTopmostItemAncestor(Item: TMenuItem): string;
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
type TMenuAndShortcut = record
      Caption: string;
      Shortcut: TShortCut;
end;
PMenuAndShortcut = ^TMenuAndShortcut;
var
  I, x: integer;
  Fini: TIniFile;
  Entries: TList;
  ms: PMenuAndShortcut;
  sct: TShortCut;
  SmenuOld, SmenuNew, Entry: string;
  Found: boolean;
begin
  if fOwner = nil then
    Exit;
  if (fFileName = '') or not FileExists(fFileName) then
    Exit;
  Entries:=TList.Create;
  Fini := TIniFile.Create(fFileName);
  try
    for I := 0 to fOwner.ComponentCount - 1 do
      if (fOwner.Components[I] is TMenuItem) then
        if not TMenuItem(fOwner.Components[I]).IsLine then
          if (TMenuItem(fOwner.Components[I]).Count = 0) then begin
            SmenuOld := StripHotkey(TMenuItem(fOwner.Components[I]).Caption);
            SmenuNew := StripHotkey(GetTopmostItemAncestor(TMenuItem(fOwner.Components[I])))+':'+SmenuOld;
            Entry :=Fini.ReadString('Shortcuts', SmenuNew, '');

            Found:=False;
            for x:=0 to Entries.Count-1 do begin
              ms:=Entries[x];
              if ms^.Caption=SmenuOld then begin
                TMenuItem(fOwner.Components[I]).ShortCut:=ms^.Shortcut;
                if Assigned(TMenuItem(fOwner.Components[I]).Action) then
                  TAction(TMenuItem(fOwner.Components[I]).Action).ShortCut:=ms^.Shortcut;
                found:=True;
                Break;
              end;
            end;
            if Found then
              Continue;

            if Entry='' then
              Entry :=Fini.ReadString('Shortcuts', SmenuOld, ShortCutToText(
                TMenuItem(fOwner.Components[I]).ShortCut));
            if Entry <> 'none' then
              sct := TextToShortCut(Entry)
            else
              sct:=0;
            TMenuItem(fOwner.Components[I]).ShortCut := sct;
            if Assigned(TMenuItem(fOwner.Components[I]).Action) then
              TAction(TMenuItem(fOwner.Components[I]).Action).ShortCut:=sct;

            ms:=New(PMenuAndShortcut);
            ms^.Caption:=SmenuOld;
            ms^.Shortcut:=sct;
            Entries.Add(ms);
          end;
  finally
    Fini.Free;
    while Entries.Count>0 do begin
      Dispose(Entries[0]);
      Entries.Delete(0);
    end;
    Entries.Free;
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
  Smenu: string;
  Scut: string;
begin
  if fFileName = '' then
    Exit;
  Fini := TIniFile.Create(fFileName);
  try
    for I := 0 to frmShortcutsEditor.Count - 1 do begin
      frmShortcutsEditor.Items[I].ShortCut := frmShortcutsEditor.ShortCuts[I];
      if Assigned(frmShortcutsEditor.Items[I].Action) then
        TAction(frmShortcutsEditor.Items[I].Action).ShortCut := frmShortcutsEditor.ShortCuts[I];
      Smenu := StripHotkey(GetTopmostItemAncestor(frmShortcutsEditor.Items[I]))+':'+
        StripHotkey(frmShortcutsEditor.Items[I].Caption);
      Scut := ShortCutToText(frmShortcutsEditor.ShortCuts[I]);
      if Scut = '' then
        Scut := 'none';
      Fini.WriteString('Shortcuts', Smenu, Scut);
    end;
  finally
    Fini.Free;
  end;
end;

end.

