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

unit Tests;

interface

uses
  Windows, Classes, Sysutils, Dateutils, Forms, ShellAPI, Dialogs, NewProjectFrm, Project,
  Menus, Registry, Controls, ComCtrls, Math, ActnList, CompOptionsFrm, SynEditKeyCmds, SynEditTypes;

type
  TTestClass = class
  private
    procedure ShowUpdate(Delay: Integer);
  public
    constructor Create;
    function TestEditor: Boolean;
    function TestEditorList: Boolean;
    function TestActions: Boolean;
    function TestCompilerOptions: Boolean;
    function TestAll: Boolean;
  end;

implementation

uses
  Main, EditorList, Editor, Version;

procedure TTestClass.ShowUpdate(Delay: Integer);
begin
  Application.ProcessMessages;
  Sleep(Delay);
end;

function TTestClass.TestEditorList: Boolean;
var
  EditorCount, CloseEditorCount: integer;
  e: TEditor;

  procedure OpenEditors(Count: Integer; PageControl: TPageControl);
  var
    I, StartCount: integer;
  begin
    if Assigned(PageControl) then
      StartCount := PageControl.PageCount
    else
      StartCount := 0;
    for I := 1 to Count do begin
      MainForm.EditorList.NewEditor('', False, True, PageControl);
      if Assigned(PageControl) then
        // make sure property PageCount is correct
        Assert(PageControl.PageCount = StartCount + I);
      ShowUpdate(0);
    end;
  end;
  procedure CloseEditors(PageControl: TPageControl);
  var
    I: integer;
    e: TEditor;
  begin
    for I := PageControl.PageCount - 1 downto 0 do begin
      e := MainForm.EditorList.GetEditor(i, PageControl);
      // if this fails the deleted editor will be acivated after
      // closing
      Assert(e <> MainForm.EditorList.GetPreviousEditor(e));
      MainForm.EditorList.CloseEditor(e);
      // make sure property PageCount is correct
      Assert(PageControl.PageCount = I);
      ShowUpdate(0);
    end;
  end;
  procedure CloseAllEditors;
  begin
    CloseEditors(MainForm.EditorList.LeftPageControl);
    CloseEditors(MainForm.EditorList.RightPageControl);
  end;
  procedure SwapEditors(PageControl: TPageControl);
  begin
    while PageControl.PageCount > 0 do begin
      MainForm.EditorList.SwapEditor(MainForm.EditorList.GetEditor(-1, PageControl));
      ShowUpdate(0);
    end;
  end;
  procedure ActivateEditors(PageControl: TPageControl);
  var
    I: integer;
    e: TEditor;
  begin
    for I := 0 to PageControl.PageCount - 1 do begin
      e := MainForm.EditorList.GetEditor(i, PageControl);
      e.Activate;
      // Make sure property FocusedPageControl is correct
      Assert(MainForm.EditorList.FocusedPageControl = e.PageControl);
      ShowUpdate(0);
    end;
  end;
  procedure ZapEditors(GoForward: Boolean);
  var
    I: integer;
    FocusedPageControl: TPageControl;
  begin
    FocusedPageControl := MainForm.EditorList.FocusedPageControl;
    for I := 0 to MainForm.EditorList.FocusedPageControl.PageCount - 1 do begin
      if GoForward then
        MainForm.EditorList.SelectNextPage
      else
        MainForm.EditorList.SelectPrevPage;
      // Make sure PageControl focus does not change
      Assert(FocusedPageControl = MainForm.EditorList.FocusedPageControl);
      ShowUpdate(0);
    end;
  end;
  procedure CloseEditorsRandom;
  var
    I: Integer;
    e: TEditor;
  begin
    while MainForm.EditorList.PageCount > 0 do begin
      I := RandomRange(0, MainForm.EditorList.PageCount - 1);
      e := MainForm.EditorList.Editors[I];
      if RandomRange(1, 5) = 1 then // test closing active editors too in 1/5 on cases
        e.Activate;
      // if this fails the deleted editor will be acivated after closing
      Assert(e <> MainForm.EditorList.GetPreviousEditor(e));
      MainForm.EditorList.CloseEditor(e);
      ShowUpdate(0);
    end;
  end;
begin
  EditorCount := 10;
  CloseEditorCount := 50;
  try
    MainForm.SetStatusbarMessage('Open editors in the default page control (left)');
    OpenEditors(EditorCount, nil);
    Assert(MainForm.EditorList.PageCount = 1 * EditorCount);
    Assert(MainForm.EditorList.Layout = lstLeft);
    Assert(MainForm.EditorList.FocusedPageControl = MainForm.EditorList.LeftPageControl);

    MainForm.SetStatusbarMessage('Open explicitly in the left page control');
    OpenEditors(EditorCount, MainForm.EditorList.LeftPageControl);
    Assert(MainForm.EditorList.PageCount = 2 * EditorCount);
    Assert(MainForm.EditorList.Layout = lstLeft);
    Assert(MainForm.EditorList.FocusedPageControl = MainForm.EditorList.LeftPageControl);

    MainForm.SetStatusbarMessage('Open explicitly in the right page control');
    OpenEditors(EditorCount, MainForm.EditorList.RightPageControl);
    Assert(MainForm.EditorList.PageCount = 3 * EditorCount);
    Assert(MainForm.EditorList.Layout = lstBoth);
    Assert(MainForm.EditorList.FocusedPageControl = MainForm.EditorList.RightPageControl);

    MainForm.SetStatusbarMessage('Close left editors');
    CloseEditors(MainForm.EditorList.LeftPageControl);
    Assert(MainForm.EditorList.PageCount = 1 * EditorCount);
    Assert(MainForm.EditorList.Layout = lstRight);
    Assert(MainForm.EditorList.FocusedPageControl = MainForm.EditorList.RightPageControl);

    MainForm.SetStatusbarMessage('Close right editors');
    CloseEditors(MainForm.EditorList.RightPageControl);
    Assert(MainForm.EditorList.PageCount = 0);
    Assert(MainForm.EditorList.Layout = lstNone);
    Assert(MainForm.EditorList.FocusedPageControl = nil);

    MainForm.SetStatusbarMessage('Open lots of editors');
    OpenEditors(5 * EditorCount, nil);
    Assert(MainForm.EditorList.PageCount = 5 * EditorCount);
    Assert(MainForm.EditorList.Layout = lstLeft);
    Assert(MainForm.EditorList.FocusedPageControl = MainForm.EditorList.LeftPageControl);

    MainForm.SetStatusbarMessage('Close all');
    CloseEditors(MainForm.EditorList.LeftPageControl);
    Assert(MainForm.EditorList.PageCount = 0);
    Assert(MainForm.EditorList.Layout = lstNone);
    Assert(MainForm.EditorList.FocusedPageControl = nil);

    MainForm.SetStatusbarMessage('Editor activating');
    OpenEditors(EditorCount, MainForm.EditorList.LeftPageControl);
    Assert(MainForm.EditorList.PageCount = 1 * EditorCount);
    Assert(MainForm.EditorList.Layout = lstLeft);
    Assert(MainForm.EditorList.FocusedPageControl = MainForm.EditorList.LeftPageControl);
    ActivateEditors(MainForm.EditorList.LeftPageControl);
    CloseAllEditors;
    Assert(MainForm.EditorList.PageCount = 0);
    Assert(MainForm.EditorList.Layout = lstNone);
    Assert(MainForm.EditorList.FocusedPageControl = nil);

    MainForm.SetStatusbarMessage('Editor swapping');
    OpenEditors(EditorCount, MainForm.EditorList.LeftPageControl);
    Assert(MainForm.EditorList.Layout = lstLeft);
    Assert(MainForm.EditorList.PageCount = 1 * EditorCount);
    SwapEditors(MainForm.EditorList.LeftPageControl);
    Assert(MainForm.EditorList.Layout = lstRight);
    Assert(MainForm.EditorList.PageCount = 1 * EditorCount);
    SwapEditors(MainForm.EditorList.RightPageControl);
    Assert(MainForm.EditorList.Layout = lstLeft);
    Assert(MainForm.EditorList.PageCount = 1 * EditorCount);
    CloseEditors(MainForm.EditorList.LeftPageControl);
    CloseEditors(MainForm.EditorList.RightPageControl);
    Assert(MainForm.EditorList.Layout = lstNone);
    Assert(MainForm.EditorList.PageCount = 0);

    MainForm.SetStatusbarMessage('Editor zapping');
    OpenEditors(EditorCount, MainForm.EditorList.LeftPageControl);
    OpenEditors(EditorCount, MainForm.EditorList.RightPageControl);
    Assert(MainForm.EditorList.Layout = lstBoth);
    ZapEditors(True); // zap right page control
    ZapEditors(False); // idem
    e := MainForm.EditorList.GetEditor(-1, MainForm.EditorList.LeftPageControl);
    e.Activate; // should work
    ZapEditors(True); // zap left page control
    ZapEditors(False); // idem
    CloseAllEditors;

    MainForm.SetStatusbarMessage('Close random editors in the left page control');
    OpenEditors(CloseEditorCount, MainForm.EditorList.LeftPageControl);
    Assert(MainForm.EditorList.Layout = lstLeft);
    Assert(MainForm.EditorList.PageCount = CloseEditorCount);
    CloseEditorsRandom;
    Assert(MainForm.EditorList.Layout = lstNone);
    Assert(MainForm.EditorList.PageCount = 0);

    MainForm.SetStatusbarMessage('Close random editors in the right page control');
    OpenEditors(CloseEditorCount, MainForm.EditorList.RightPageControl);
    Assert(MainForm.EditorList.Layout = lstRight);
    Assert(MainForm.EditorList.PageCount = CloseEditorCount);
    CloseEditorsRandom;
    Assert(MainForm.EditorList.Layout = lstNone);
    Assert(MainForm.EditorList.PageCount = 0);

    MainForm.SetStatusbarMessage('Close random editors in both page controls');
    OpenEditors(CloseEditorCount, MainForm.EditorList.LeftPageControl);
    OpenEditors(CloseEditorCount, MainForm.EditorList.RightPageControl);
    Assert(MainForm.EditorList.Layout = lstBoth);
    Assert(MainForm.EditorList.PageCount = 2 * CloseEditorCount);
    CloseEditorsRandom;
    Assert(MainForm.EditorList.Layout = lstNone);
    Assert(MainForm.EditorList.PageCount = 0);

    Result := True;
  except
    Result := False;
    //raise Exception.Create('TTestClass.TestEditorList');
  end;
end;

function TTestClass.TestActions: Boolean;
var
  I: integer;
  Action: TCustomAction;
begin
  try
    // Super annoying
    for I := 0 to MainForm.ActionList.ActionCount - 1 do begin
      Action := TCustomAction(MainForm.ActionList.Actions[i]);
      if Action.Enabled and (Action.Name <> 'actRunTests') and (Action.Name <> 'actExit') then
        Action.Execute;
    end;
    Result := True;
  except
    Result := False;
  end;
end;

function TTestClass.TestCompilerOptions;
var
  I, SetCount: integer;
begin
  SetCount := 1;
  try
    MainForm.SetStatusbarMessage('Open compiler options');
    with TCompOptForm.Create(nil) do try // copy from actCompOptions
      Show;

      MainForm.SetStatusbarMessage('Delete all compiler sets');
      while cmbCompilerSetComp.Items.Count > 0 do
        btnDelCompilerSet.Click;

      MainForm.SetStatusbarMessage('Add automagically');
      btnFindCompilers.Click;

      MainForm.SetStatusbarMessage('Rename all compiler sets');
      for I := 0 to cmbCompilerSetComp.Items.Count - 1 do begin
        cmbCompilerSetComp.ItemIndex := I;
        btnRenameCompilerSet.Click;
      end;

      MainForm.SetStatusbarMessage('Add blank compiler set');
      for I := 1 to SetCount do
        btnAddBlankCompilerSet.Click;

      MainForm.SetStatusbarMessage('Add filled compiler set');
      for I := 1 to SetCount do
        btnAddFilledCompilerSet.Click;

      MainForm.SetStatusbarMessage('Set current compiler set');
      cmbCompilerSetComp.ItemIndex := 0;

      MainForm.SetStatusbarMessage('Save compiler options');
      btnOk.Click;
      //  MainForm.CheckForDLLProfiling; TODO: private
      MainForm.UpdateCompilerList;
    finally;
      Free;
    end;
    Result := True;
  except
    Result := False;
  end;
end;

function TTestClass.TestEditor: Boolean;
var
  I, FoldCount, LineCount, LineLength, CommentCount, DupeCount, IndentCount: Integer;
  e: TEditor;

  procedure TypeText(const Text: String);
  var
    I: Integer;
  begin
    for I := 1 to Length(Text) do
      e.Text.CommandProcessor(ecChar, Text[i], nil);
    ShowUpdate(0);
  end;
begin
  FoldCount := 20;
  LineCount := 20;
  LineLength := 10;
  CommentCount := 20;
  DupeCount := 20;
  IndentCount := 3;
  try
    MainForm.SetStatusbarMessage('Create new file');
    MainForm.actNewSource.Execute;
    e := MainForm.EditorList.GetEditor;
    //  e := MainForm.EditorList.NewEditor('main.cpp', False, True, nil);
    e.Activate;

    MainForm.SetStatusbarMessage('Add foldable code');
    for I := 1 to FoldCount do begin
      TypeText('{'); // + #13#10;
      e.Text.CommandProcessor(ecLineBreak, #0, nil);
    end;
    for I := 1 to FoldCount do begin
      TypeText('}'); // + #13#10;
      e.Text.CommandProcessor(ecLineBreak, #0, nil);
    end;
    Assert(e.Text.Lines.Count = 2 * FoldCount + 1);

    MainForm.SetStatusbarMessage('Move folds down');
    e.Text.CaretXY := BufferCoord(1, 1);
    for I := 1 to LineCount do begin
      e.Text.CommandProcessor(ecLineBreak, #0, nil);
      ShowUpdate(0);
    end;
    Assert(e.Text.Lines.Count = 2 * FoldCount + 1 + LineCount);

    MainForm.SetStatusbarMessage('Move folds up');
    e.Text.CaretXY := BufferCoord(1, 1);
    for I := 1 to LineCount do begin
      e.Text.CommandProcessor(ecDeleteLine, #0, nil);
      ShowUpdate(0);
    end;
    Assert(e.Text.Lines.Count = 2 * FoldCount + 1);

    MainForm.SetStatusbarMessage('Test fold collapsing and uncollapsing');
    e.Text.CollapseAll;
    ShowUpdate(50);
    e.Text.UncollapseAll;
    ShowUpdate(50);

    MainForm.SetStatusbarMessage('Undo all previous actions to end up with empty editor');
    while e.Text.UndoList.CanUndo do begin
      e.Text.Undo;
      ShowUpdate(0);
    end;
    Assert(e.Text.Text.IsEmpty);

    MainForm.SetStatusbarMessage('Type wall of text');
    for I := Ord('a') to Ord('z') do begin
      TypeText(StringOfChar(Chr(I), LineLength));
      e.Text.CommandProcessor(ecLineBreak, #0, nil);
    end;
    Assert(e.Text.Lines.Count = 26 + 1);

    MainForm.SetStatusbarMessage('Move lines down');
    for I := 0 to e.Text.Lines.Count - 1 do begin
      e.Text.CaretXY := BufferCoord(1, 1);
      for var J := 0 to e.Text.Lines.Count - 3 - I do
        e.Text.CommandProcessor(TSynEditEx.ecMoveSelDown, #0, nil);
      ShowUpdate(0);
    end;

    MainForm.SetStatusbarMessage('Move lines up');
    for I := 0 to e.Text.Lines.Count - 1 do begin
      e.Text.CaretXY := BufferCoord(1, e.Text.Lines.Count);
      for var J := 0 to e.Text.Lines.Count - 1 - I do
        e.Text.CommandProcessor(TSynEditEx.ecMoveSelUp, #0, nil);
      ShowUpdate(0);
    end;
{
    MainForm.SetStatusbarMessage('Comment');
    e.Text.SelectAll;
    for I := 1 to CommentCount do begin
      e.Text.CommandProcessor(ecComment, #0, nil);
      ShowUpdate(20);
    end;
    Assert(e.Text.Lines.Count = 26 + 1);

    MainForm.SetStatusbarMessage('Uncomment');
    e.Text.SelectAll;
    for I := 1 to CommentCount do begin
      e.Text.CommandProcessor(ecUncomment, #0, nil);
      ShowUpdate(20);
    end;
    Assert(e.Text.Lines.Count = 26 + 1);

    MainForm.SetStatusbarMessage('Toggle comment');
    e.Text.SelectAll;
    for I := 1 to CommentCount do begin
      e.Text.CommandProcessor(ecToggleComment, #0, nil);
      ShowUpdate(0);
    end;
    Assert(e.Text.Lines.Count = 26 + 1);   }

    MainForm.SetStatusbarMessage('Undo all previous actions to end up with empty editor');
    while e.Text.UndoList.CanUndo do begin
      e.Text.Undo;
      ShowUpdate(0);
    end;
    // TODO: Lift. Is e.Text.IsEmpty same as e.Text.Lines.Text.IsEmpty
    Assert(e.Text.Lines.Text.IsEmpty);

    MainForm.SetStatusbarMessage('Type line of text');
    for I := Ord('a') to Ord('z') do begin
      TypeText(Chr(I));
    end;
    Assert(e.Text.Lines.Count = 1);

    MainForm.SetStatusbarMessage('Duplicate lines');
    for I := 1 to DupeCount do begin
      e.Text.CommandProcessor(TSynEditEx.ecDuplicateLine, #0, nil);
      ShowUpdate(50);
    end;
    Assert(e.Text.Lines.Count = 1 + DupeCount);

    MainForm.SetStatusbarMessage('Delete lines');
    for I := 1 to DupeCount do begin
      e.Text.CommandProcessor(ecDeleteLine, #0, nil);
      ShowUpdate(20);
    end;
    Assert(e.Text.Lines.Count = 1);

    MainForm.SetStatusbarMessage('Undo all previous actions to end up with empty editor');
    while e.Text.UndoList.CanUndo do begin
      e.Text.Undo;
      ShowUpdate(0);
    end;

    Assert(e.Text.Text.IsEmpty);

    MainForm.SetStatusbarMessage('Type wall of text');
    for I := Ord('a') to Ord('z') do begin
      TypeText(StringOfChar(Chr(I), LineLength));
      e.Text.CommandProcessor(ecLineBreak, #0, nil);
    end;
    Assert(e.Text.Lines.Count = 26 + 1);

    MainForm.SetStatusbarMessage('Indent');
    e.Text.SelectAll;
    for I := 1 to IndentCount do begin
      e.Text.CommandProcessor(ecBlockIndent, #0, nil);
      ShowUpdate(20);
    end;
    Assert(e.Text.Lines.Count = 26 + 1);

    MainForm.SetStatusbarMessage('Unindent');
    e.Text.SelectAll;
    for I := 1 to IndentCount do begin
      e.Text.CommandProcessor(ecBlockUnindent, #0, nil);
      ShowUpdate(20);
    end;
    Assert(e.Text.Lines.Count = 26 + 1);

    MainForm.SetStatusbarMessage('Undo all previous actions to end up with empty editor');
    while e.Text.UndoList.CanUndo do begin
      e.Text.Undo;
      ShowUpdate(0);
    end;
    Assert(e.Text.Text.IsEmpty);

    MainForm.SetStatusbarMessage('Type wall of text');
    for I := Ord('a') to Ord('a') + 9 do begin
      TypeText(StringOfChar(Chr(I), LineLength));
      e.Text.CommandProcessor(ecLineBreak, #0, nil);
    end;
    Assert(e.Text.Lines.Count = 11);

    MainForm.SetStatusbarMessage('Enable bookmarks');
    for I := 1 to 9 do begin
      e.Text.CaretXY := BufferCoord(1, I);
      MainForm.ToggleBookmarksItem.Items[i - 1].Click;
      Assert(MainForm.ToggleBookmarksItem.Items[i - 1].Checked);
      ShowUpdate(20);
    end;
    Assert(e.Text.Lines.Count = 11);

    MainForm.SetStatusbarMessage('Goto bookmarks');
    for I := 9 downto 1 do begin
      MainForm.GotoBookmarksItem.Items[i - 1].Click;
      ShowUpdate(20);
    end;
    Assert(e.Text.Lines.Count = 11);

    MainForm.SetStatusbarMessage('Disable bookmarks');
    for I := 1 to 9 do begin
      MainForm.ToggleBookmarksItem.Items[i - 1].Click;
      Assert(not MainForm.ToggleBookmarksItem.Items[i - 1].Checked);
      ShowUpdate(20);
    end;
    Assert(e.Text.Lines.Count = 11);

    MainForm.SetStatusbarMessage('Undo all previous actions to end up with empty editor');
    while e.Text.UndoList.CanUndo do begin
      e.Text.Undo;
      ShowUpdate(0);
    end;

    Assert(e.Text.Text.IsEmpty);

    MainForm.SetStatusbarMessage('Close editor without saving');
    MainForm.EditorList.CloseEditor(e);

    Result := True;
  except
    Result := False;
  end;
end;

function TTestClass.TestAll: Boolean;
begin
  // TODO: further automate other tests
  Result := {TestEditorList and }TestEditor;
end;

constructor TTestClass.Create;
begin
end;

end.

