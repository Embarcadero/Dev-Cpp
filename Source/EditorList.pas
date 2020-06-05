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

unit EditorList;

interface

uses
  Windows, SysUtils, Dialogs, StdCtrls, Controls, ComCtrls, Forms, Editor, ExtCtrls,
  devrun, version, project, utils, ProjectTypes, Classes, Graphics, Math, Messages;

type
  TLayoutShowType = (lstNone, lstLeft, lstRight, lstBoth);
  TEditorList = class
  private
    fLayout: TLayoutShowType;
    fLeftPageControl: TPageControl;
    fRightPageControl: TPageControl;
    fSplitter: TSplitter;
    fPanel: TPanel; // ui component that is the base layer for all page controls
    fUpdateCount: integer;
    function GetForEachEditor(index: integer): TEditor;
    function GetPageCount: integer;
    function GetFocusedPageControl: TPageControl;
    function GetNewEditorPageControl: TPageControl;
    procedure ShowLayout(Layout: TLayoutShowType);
  public
    function NewEditor(const Filename: String; InProject, NewFile: boolean; PageControl: TPageControl = nil):
      TEditor;
    function FileIsOpen(const FileName: String; ProjectOnly: boolean = FALSE): TEditor;
    function GetEditor(PageIndex: integer = -1; PageControl: TPageControl = nil): TEditor;
    function GetEditorFromFileName(const FileName: String): TEditor;
    function GetEditorFromTag(tag: integer): TEditor;
    function GetPreviousEditor(Editor: TEditor): TEditor;
    procedure ForceCloseEditor(Editor: TEditor); // close, no questions asked
    function CloseEditor(Editor: TEditor): Boolean;
    function CloseAll: boolean;
    function CloseAllButThis: boolean;
    function SwapEditor(Editor: TEditor): boolean;
    procedure OnPanelResize(Sender: TObject);
    procedure SelectNextPage;
    procedure SelectPrevPage;
    procedure BeginUpdate; // stops redraws
    procedure EndUpdate; // ends redraws
    procedure UpdateLayout; // reconfigures layout
    procedure GetVisibleEditors(var Left: TEditor; var Right: TEditor);
    procedure SetPreferences(TabPosition: TTabPosition; MultiLine: boolean);
    property LeftPageControl: TPageControl read fLeftPageControl write fLeftPageControl;
    property RightPageControl: TPageControl read fRightPageControl write fRightPageControl;
    property Splitter: TSplitter read fSplitter write fSplitter;
    property Panel: TPanel read fPanel write fPanel;
    property PageCount: integer read GetPageCount;
    property Editors[Index: integer]: TEditor read GetForEachEditor; default;
    property FocusedPageControl: TPageControl read GetFocusedPageControl;
    property Layout: TLayoutShowType read fLayout;
  end;

implementation

uses
  System.UItypes, main, MultiLangSupport, DataFrm;

function TEditorList.GetPageCount: integer;
begin
  Result := fLeftPageControl.PageCount + fRightPageControl.PageCount;
end;

procedure TEditorList.BeginUpdate;
begin
  Inc(fUpdateCount);
  if fUpdateCount = 1 then
    SendMessage(fPanel.Handle, WM_SETREDRAW, 0, 0); // stop drawing, it's slow
end;

procedure TEditorList.EndUpdate;
begin
  Dec(fUpdateCount);
  if fUpdateCount = 0 then begin
    SendMessage(fPanel.Handle, WM_SETREDRAW, 1, 0); // allow drawing again
    RedrawWindow(fPanel.Handle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN); // draw once
  end;
end;

function TEditorList.GetFocusedPageControl: TPageControl;
var
  ActivePage: TTabSheet;
begin
  case fLayout of
    lstLeft: begin
        Result := fLeftPageControl
      end;
    lstRight: begin
        Result := fRightPageControl
      end;
    lstBoth: begin
        // Check if right is focused, otherwise assume left one is focused
        ActivePage := fRightPageControl.ActivePage;
        if TEditor(ActivePage.Tag).Text.Focused then
          Result := fRightPageControl
        else
          Result := fLeftPageControl; // no focus -> left one
      end;
    lstNone: begin
        Result := nil;
      end;
  else
    Result := nil;
  end;
end;

function TEditorList.GetForEachEditor(index: integer): TEditor;
begin
  // Is it within range of the first one?
  if (index >= 0) and (index < fLeftPageControl.PageCount) then begin
    result := TEditor(fLeftPageControl.Pages[index].Tag);
    Exit;
  end;

  // Nope? Check second one
  Dec(index, fLeftPageControl.PageCount);
  if (index >= 0) and (index < fRightPageControl.PageCount) then begin
    result := TEditor(fRightPageControl.Pages[index].Tag);
    Exit;
  end;

  Result := nil;
end;

function TEditorList.GetNewEditorPageControl;
begin
  case fLayout of
    lstNone: begin
        Result := fLeftPageControl; // first editor should be shown in the leftmost control
      end;
    lstLeft: begin
        Result := fLeftPageControl;
      end;
    lstRight: begin
        Result := fRightPageControl;
      end;
    lstBoth: begin
        Result := GetFocusedPageControl; // depends on the current keyboard focus
      end;
  else
    Result := nil;
  end;
end;

function TEditorList.NewEditor(const Filename: String; InProject, NewFile: boolean; PageControl: TPageControl =
  nil):
  TEditor;
var
  ParentPageControl: TPageControl;
begin
  BeginUpdate;
  try
    if PageControl = nil then
      ParentPageControl := GetNewEditorPageControl
    else
      ParentPageControl := PageControl;

    Result := TEditor.Create(FileName, InProject, NewFile, ParentPageControl);

    // Force layout update when creating, destroying or moving editors
    UpdateLayout;
  finally
    EndUpdate; // redraw once
  end;
end;

function TEditorList.FileIsOpen(const FileName: String; ProjectOnly: boolean = FALSE): TEditor;
var
  e: TEditor;
  I: integer;
begin
  // Check first page control
  for I := 0 to fLeftPageControl.PageCount - 1 do begin
    e := GetEditor(I, fLeftPageControl);
    if SameFileName(e.FileName, FileName) then

      // Accept the file if it's in a project OR if it doesn't have to be in a project
      if (not ProjectOnly) or e.InProject then begin
        Result := e;
        Exit;
      end;
  end;

  // Same for the right page control
  for I := 0 to fRightPageControl.PageCount - 1 do begin
    e := GetEditor(I, fRightPageControl);
    if SameFileName(e.FileName, FileName) then

      // Accept the file if it's in a project OR if it doesn't have to be in a project
      if (not ProjectOnly) or e.InProject then begin
        Result := e;
        Exit;
      end;
  end;

  Result := nil;
end;

function TEditorList.GetEditor(PageIndex: integer = -1; PageControl: TPageControl = nil): TEditor;
var
  SelectedPageControl: TPageControl;
  TabSheet: TTabSheet;
begin
  Result := nil;

  // Select page control
  if PageControl = nil then
    SelectedPageControl := GetFocusedPageControl
  else
    SelectedPageControl := PageControl;
  if not Assigned(SelectedPageControl) then
    Exit;

  // Select tab in selected pagecontrol
  case PageIndex of
    -1: TabSheet := SelectedPageControl.ActivePage;
  else
    TabSheet := SelectedPageControl.Pages[PageIndex];
  end;
  if not Assigned(TabSheet) then
    Exit;

  Result := TEditor(TabSheet.Tag);
end;

function TEditorList.GetPreviousEditor(Editor: TEditor): TEditor;
var
  I: integer;
  EditorPageControl: TPageControl;
  PrevNaturalPage: TTabSheet;
  e: TEditor;
begin
  result := nil;
  if not Assigned(Editor) then
    Exit;

  // Determine what to view next
  EditorPageControl := Editor.PageControl;
  with EditorPageControl do begin

    // If we are closing the active tab, open the tab that was opened when this tab was created
    if ActivePage = Editor.TabSheet then begin // this is the current page...

      // Find the first tab in the history list that is still open
      for I := Editor.PreviousEditors.Count - 1 downto 0 do begin
        e := GetEditorFromTag(Integer(Editor.PreviousEditors[i]));
        if Assigned(e) then begin
          Result := e;
          Exit;
        end;
      end;

      // All history items are gone or this was the first tab to open which has no history
      // Select the editor that would appear naturally when closing this one
      PrevNaturalPage := FindNextPage(Editor.TabSheet, False, True);
      if Assigned(PrevNaturalPage) and (PrevNaturalPage <> Editor.TabSheet) then begin
        Result := GetEditor(PrevNaturalPage.TabIndex, EditorPageControl);
        Exit;
      end;

      // All editors in the current page control are gone. Try the other page control
      if fLayout = lstBoth then begin
        if EditorPageControl = LeftPageControl then begin
          Result := GetEditor(-1, RightPageControl);
          Exit;
        end else begin
          Result := GetEditor(-1, LeftPageControl);
          Exit;
        end;
      end;

      // If we are not closing the active tab, don't change focus
    end else begin
      Result := GetEditor(-1, EditorPageControl);
    end;
  end;
end;

procedure TEditorList.ForceCloseEditor(Editor: TEditor);
begin
  BeginUpdate;
  try
    FreeAndNil(Editor);

    // Force layout update when creating, destroying or moving editors
    UpdateLayout;
  finally
    EndUpdate; // redraw once
  end;
end;

function TEditorList.CloseEditor(Editor: TEditor): Boolean;
var
  projindex: integer;
  PrevEditor: TEditor;
begin
  Result := False;
  if not Assigned(Editor) then
    Exit;

  // Ask user if he wants to save
  if Editor.Text.Modified and not Editor.Text.Text.IsEmpty then begin
    case MessageDlg(Format(Lang[ID_MSG_ASKSAVECLOSE], [Editor.FileName]), mtConfirmation, mbYesNoCancel, 0) of
      mrYes:
        if not Editor.Save then
          Exit;
      mrCancel:
        Exit; // stop closing
    end;
  end;

  // Select editor to open when this one closes
  PrevEditor := GetPreviousEditor(Editor);
  if PrevEditor = Editor then
    PrevEditor := nil;

  BeginUpdate;
  try
    // We're allowed to close...
    Result := True;
    if Editor.InProject and Assigned(MainForm.Project) then begin
      projindex := MainForm.Project.Units.IndexOf(Editor);
      if projindex <> -1 then
        MainForm.Project.CloseUnit(projindex); // calls ForceCloseEditor
    end else begin
      dmMain.AddtoHistory(Editor.FileName);
      FreeAndNil(Editor);

      // Force layout update when creating, destroying or moving editors
      UpdateLayout;
    end;

    // Show new editor after forcing a layout update
    if Assigned(PrevEditor) then
      PrevEditor.Activate;
  finally
    EndUpdate; // redraw once
  end;
end;

function TEditorList.CloseAll: boolean;
begin
  Result := False;

  // Redraw once after the whole ordeal
  BeginUpdate;
  try
    // Keep closing the first one to prevent redrawing
    while fLeftPageControl.PageCount > 0 do
      if not CloseEditor(GetEditor(0, fLeftPageControl)) then
        Exit;

    // Same for the right page control
    while fRightPageControl.PageCount > 0 do
      if not CloseEditor(GetEditor(0, fRightPageControl)) then
        Exit;
  finally
    EndUpdate;
  end;

  Result := True;
end;

function TEditorList.CloseAllButThis: boolean;
var
  I: integer;
  ActiveEditor, Editor: TEditor;
begin
  Result := False;

  // Redraw once after the whole ordeal
  BeginUpdate;
  try
    // Keep closing the first one to prevent redrawing
    ActiveEditor := GetEditor(-1, fLeftPageControl);
    for I := fLeftPageControl.PageCount - 1 downto 0 do begin
      Editor := GetEditor(I, fLeftPageControl);
      if Assigned(Editor) and (Editor <> ActiveEditor) then
        if not CloseEditor(Editor) then
          Exit;
    end;

    // Keep closing the first one to prevent redrawing
    ActiveEditor := GetEditor(-1, fRightPageControl);
    for I := fRightPageControl.PageCount - 1 downto 0 do begin
      Editor := GetEditor(I, fRightPageControl);
      if Assigned(Editor) and (Editor <> ActiveEditor) then
        if not CloseEditor(Editor) then
          Exit;
    end;
  finally
    EndUpdate;
  end;

  Result := True;
end;

function TEditorList.GetEditorFromFileName(const FileName: String): TEditor;
var
  FullFileName: String;
  I: integer;
  e: TEditor;
begin
  Result := nil;

  // ExpandFileName reduces all the "\..\" in the path
  FullFileName := ExpandFileName(FileName);

  // First, check wether the file is already open
  for I := 0 to fLeftPageControl.PageCount - 1 do begin
    e := GetEditor(I, fLeftPageControl);
    if Assigned(e) then begin
      if SameFileName(e.FileName, FullFileName) then begin
        Result := e;
        Exit;
      end;
    end;
  end;

  // Same for the right page control
  for I := 0 to fRightPageControl.PageCount - 1 do begin
    e := GetEditor(I, fRightPageControl);
    if Assigned(e) then begin
      if SameFileName(e.FileName, FullFileName) then begin
        Result := e;
        Exit;
      end;
    end;
  end;

  // Then check the project...
  if Assigned(MainForm.Project) then begin
    I := MainForm.Project.GetUnitFromString(FullFileName);
    if I <> -1 then begin
      result := MainForm.Project.OpenUnit(I);
      Exit;
    end;
  end;

  // Else, just open from disk
  if FileExists(FullFileName) then
    Result := NewEditor(FullFileName, False, False);
end;

function TEditorList.GetEditorFromTag(tag: integer): TEditor;
var
  I: integer;
begin
  result := nil;

  // First, check wether the file is already open
  for I := 0 to fLeftPageControl.PageCount - 1 do begin
    if fLeftPageControl.Pages[i].Tag = tag then begin
      result := TEditor(fLeftPageControl.Pages[i].Tag);
      break;
    end;
  end;

  // Same for the right page control
  for I := 0 to fRightPageControl.PageCount - 1 do begin
    if fRightPageControl.Pages[i].Tag = tag then begin
      result := TEditor(fRightPageControl.Pages[i].Tag);
      break;
    end;
  end;
end;

function TEditorList.SwapEditor(Editor: TEditor): boolean;
var
  FromPageControl: TPageControl;
  FromPageControlPrevTab: TTabSheet;
begin
  Result := True;

  // Redraw once after the whole ordeal
  BeginUpdate;
  try
    // Remember old index
    FromPageControl := Editor.PageControl;
    FromPageControlPrevTab := Editor.PageControl.FindNextPage(Editor.TabSheet, False, True);

    // Determine how to swap
    if Editor.PageControl = fLeftPageControl then
      Editor.PageControl := fRightPageControl
    else
      Editor.PageControl := fLeftPageControl;

    // Switch to previous editor in the other one
    FromPageControl.ActivePage := FromPageControlPrevTab;

    // Force layout update when creating, destroying or moving editors
    UpdateLayout;

    // Move editor focus too
    Editor.Activate;
  finally
    EndUpdate;
  end;
end;

procedure TEditorList.UpdateLayout;
begin
  if (fLeftPageControl.PageCount = 0) and (fRightPageControl.PageCount = 0) then
    ShowLayout(lstNone)
  else if (fLeftPageControl.PageCount > 0) and (fRightPageControl.PageCount = 0) then
    ShowLayout(lstLeft)
  else if (fLeftPageControl.PageCount = 0) and (fRightPageControl.PageCount > 0) then
    ShowLayout(lstRight)
  else if (fLeftPageControl.PageCount > 0) and (fRightPageControl.PageCount > 0) then
    ShowLayout(lstBoth);
end;

procedure TEditorList.ShowLayout(Layout: TLayoutShowType);
begin
  if Layout = fLayout then
    Exit;
  fLayout := Layout;

  // Apply widths if layout does not change
  case fLayout of
    lstLeft: begin
        fLeftPageControl.Align := alClient;
        fLeftPageControl.Visible := True;

        // Hide other components
        fRightPageControl.Visible := False;
        fRightPageControl.Width := 0;
        fSplitter.Visible := False;
        fSplitter.Width := 0;
      end;
    lstRight: begin
        fRightPageControl.Align := alClient;
        fRightPageControl.Visible := True;

        // Hide other components
        fLeftPageControl.Visible := False;
        fLeftPageControl.Width := 0;
        fSplitter.Visible := False;
        fSplitter.Width := 0;
      end;
    lstBoth: begin
        // Align to the left, assign 50% area
        fLeftPageControl.Align := alClient;
        fLeftPageControl.Visible := True;
        fLeftPageControl.Width := (fPanel.Width - 3) div 2;
        fLeftPageControl.Left := 0;

        // Put splitter in between
        fSplitter.Align := alRight;
        fSplitter.Visible := True;
        fSplitter.Width := 3;
        fSplitter.Left := fLeftPageControl.Width;

        // Align other one to the right
        fRightPageControl.Align := alRight;
        fRightPageControl.Visible := True;
        fRightPageControl.Width := (fPanel.Width - 3) div 2;
        fRightPageControl.Left := fLeftPageControl.Width + 3;
      end;
    lstNone: begin
        fLeftPageControl.Visible := False;
        fLeftPageControl.Width := 0;
        fRightPageControl.Visible := False;
        fRightPageControl.Width := 0;
        fSplitter.Visible := False;
        fSplitter.Width := 0;

        // Normally, change events are triggered by editor focus, but since there's no one left, fake it
        // Notify of change AFTER we change the official layout
        fLeftPageControl.OnChange(fLeftPageControl);
      end;
  end;
end;

procedure TEditorList.SelectNextPage;
var
  PageControl: TPageControl;
begin
  PageControl := GetFocusedPageControl;
  if Assigned(PageControl) then
    PageControl.SelectNextPage(True);
end;

procedure TEditorList.SelectPrevPage;
var
  PageControl: TPageControl;
begin
  PageControl := GetFocusedPageControl;
  if Assigned(PageControl) then
    PageControl.SelectNextPage(False);
end;

procedure TEditorList.GetVisibleEditors(var Left: TEditor; var Right: TEditor);
begin
  case fLayout of
    lstNone: begin
        Left := nil;
        Right := nil;
      end;
    lstLeft: begin
        Left := GetEditor(-1, fLeftPageControl);
        Right := nil;
      end;
    lstRight: begin
        Left := nil;
        Right := GetEditor(-1, fRightPageControl);
      end;
    lstBoth: begin
        Left := GetEditor(-1, fLeftPageControl);
        Right := GetEditor(-1, fLeftPageControl);
      end;
  end;
end;

procedure TEditorList.SetPreferences(TabPosition: TTabPosition; MultiLine: boolean);
begin
  LeftPageControl.TabPosition := TabPosition;
  LeftPageControl.MultiLine := MultiLine;
  RightPageControl.TabPosition := TabPosition;
  RightPageControl.MultiLine := MultiLine;
end;

procedure TEditorList.OnPanelResize(Sender: TObject);
//var
//  LeftPageWidthPercent : integer;
begin
  if fLayout = lstBoth then begin
    // Force 50% layout
    // TODO: better option would be to remember pagecontrol width percentages of parent panel
    fLayout := lstNone;
    ShowLayout(lstBoth);
  end;
end;

end.

