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
  Windows, Classes, Sysutils, Dateutils, Forms, ShellAPI, Dialogs,
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
  EditorCount, EditorIndex: integer;
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
    end;
    ShowUpdate(100);
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
    end;
    ShowUpdate(100);
  end;
  procedure CloseAllEditors;
  begin
    CloseEditors(MainForm.EditorList.LeftPageControl);
    CloseEditors(MainForm.EditorList.RightPageControl);
  end;
  procedure SwapEditors(PageControl: TPageControl);
  begin
    while PageControl.PageCount > 0 do
      MainForm.EditorList.SwapEditor(MainForm.EditorList.GetEditor(-1, PageControl));
    ShowUpdate(100);
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
    end;
    ShowUpdate(100);
  end;
  procedure ZapEditors(GoForward: Boolean);
  var
    I: integer;
    FocusedPageControl : TPageControl;
  begin
    FocusedPageControl := MainForm.EditorList.FocusedPageControl;
    for I := 0 to MainForm.EditorList.FocusedPageControl.PageCount - 1 do begin
      if GoForward then
        MainForm.EditorList.SelectNextPage
      else
        MainForm.EditorList.SelectPrevPage;
      // Make sure PageControl focus does not change
      Assert(FocusedPageControl = MainForm.EditorList.FocusedPageControl);
    end;
    ShowUpdate(100);
  end;
  procedure CloseEditorsRandom(PageControl: TPageControl);
  var
    e: TEditor;
  begin
    while PageControl.PageCount > 0 do begin
      EditorIndex := RandomRange(0, PageControl.PageCount - 1);
      e := MainForm.EditorList.GetEditor(EditorIndex, PageControl);
      // if this fails the deleted editor will be acivated after
      // closing
      Assert(e <> MainForm.EditorList.GetPreviousEditor(e));
      MainForm.EditorList.CloseEditor(e);
    end;
    ShowUpdate(100);
  end;
begin
  EditorCount := 5;
  try
    // Open editors in the default page control (left)
    OpenEditors(EditorCount, nil);
    Assert(MainForm.EditorList.PageCount = 1 * EditorCount);
    Assert(MainForm.EditorList.Layout = lstLeft);
    Assert(MainForm.EditorList.FocusedPageControl = MainForm.EditorList.LeftPageControl);

    // Open explicitly in the left page control
    OpenEditors(EditorCount, MainForm.EditorList.LeftPageControl);
    Assert(MainForm.EditorList.PageCount = 2 * EditorCount);
    Assert(MainForm.EditorList.Layout = lstLeft);
    Assert(MainForm.EditorList.FocusedPageControl = MainForm.EditorList.LeftPageControl);

    // Open explicitly in the right page control
    OpenEditors(EditorCount, MainForm.EditorList.RightPageControl);
    Assert(MainForm.EditorList.PageCount = 3 * EditorCount);
    Assert(MainForm.EditorList.Layout = lstBoth);
    Assert(MainForm.EditorList.FocusedPageControl = MainForm.EditorList.RightPageControl);

    // Close left editors
    CloseEditors(MainForm.EditorList.LeftPageControl);
    Assert(MainForm.EditorList.PageCount = 1 * EditorCount);
    Assert(MainForm.EditorList.Layout = lstRight);
    Assert(MainForm.EditorList.FocusedPageControl = MainForm.EditorList.RightPageControl);

    // Close right editors
    CloseEditors(MainForm.EditorList.RightPageControl);
    Assert(MainForm.EditorList.PageCount = 0);
    Assert(MainForm.EditorList.Layout = lstNone);
    Assert(MainForm.EditorList.FocusedPageControl = nil);

    // Open lots of editors
    OpenEditors(5 * EditorCount, nil);
    Assert(MainForm.EditorList.PageCount = 5 * EditorCount);
    Assert(MainForm.EditorList.Layout = lstLeft);
    Assert(MainForm.EditorList.FocusedPageControl = MainForm.EditorList.LeftPageControl);

    // Close all
    CloseEditors(MainForm.EditorList.LeftPageControl);
    Assert(MainForm.EditorList.PageCount = 0);
    Assert(MainForm.EditorList.Layout = lstNone);
    Assert(MainForm.EditorList.FocusedPageControl = nil);

    // Test editor activating
    OpenEditors(EditorCount, MainForm.EditorList.LeftPageControl);
    Assert(MainForm.EditorList.PageCount = 1 * EditorCount);
    Assert(MainForm.EditorList.Layout = lstLeft);
    Assert(MainForm.EditorList.FocusedPageControl = MainForm.EditorList.LeftPageControl);
    ActivateEditors(MainForm.EditorList.LeftPageControl);
    CloseAllEditors;
    Assert(MainForm.EditorList.PageCount = 0);
    Assert(MainForm.EditorList.Layout = lstNone);
    Assert(MainForm.EditorList.FocusedPageControl = nil);

    // Test editor swapping
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

    // Test editor zapping
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

    // Close random editors
    OpenEditors(EditorCount, MainForm.EditorList.LeftPageControl);
    OpenEditors(EditorCount, MainForm.EditorList.RightPageControl);
    CloseEditorsRandom(MainForm.EditorList.LeftPageControl);
    CloseEditorsRandom(MainForm.EditorList.RightPageControl);

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
    with TCompOptForm.Create(nil) do try
      Show;
      // Delete all
      while cmbCompilerSetComp.Items.Count > 0 do
        btnDelCompilerSet.Click;

      // Readd auto
      btnFindCompilers.Click;

      // Rename all
      for I := 0 to cmbCompilerSetComp.Items.Count - 1 do begin
        cmbCompilerSetComp.ItemIndex := I;
        btnRenameCompilerSet.Click;
      end;

      // Add some
      for I := 1 to SetCount do
        btnAddBlankCompilerSet.Click;
      for I := 1 to SetCount do
        btnAddFilledCompilerSet.Click;

      // Save
      btnOk.Click;
    finally
      Free;
    end;
    Result := True;
  except
    Result := False;
  end;
end;

function TTestClass.TestEditor: Boolean;
var
  I, FoldCount, LineCount: Integer;
  e: TEditor;
  procedure TypeText(const Text: AnsiString);
  var
    I: Integer;
  begin
    for I := 1 to Length(Text) do
      e.Text.CommandProcessor(ecChar, Text[i], nil);
    ShowUpdate(20);
  end;
  procedure CollapseUncollapse;
  begin
    e.Text.CollapseAll;
    ShowUpdate(100);
    e.Text.UncollapseAll;
    ShowUpdate(100);
  end;
begin
  FoldCount := 20;
  LineCount := 10;
  try
    // Create new file
    e := MainForm.EditorList.NewEditor('main.cpp', False, True, nil);
    e.Activate;

    // Add code
    for I := 1 to FoldCount do begin
      TypeText('{'); // + #13#10;
      e.Text.CommandProcessor(ecLineBreak, #0, nil);
    end;
    for I := 1 to FoldCount do begin
      TypeText('}'); // + #13#10;
      e.Text.CommandProcessor(ecLineBreak, #0, nil);
    end;

    // Move folds
    e.Text.CaretXY := BufferCoord(1, 1);
    for I := 1 to LineCount do begin
      e.Text.CommandProcessor(ecLineBreak, #0, nil);
      ShowUpdate(50);
    end;
    e.Text.CaretXY := BufferCoord(1, 1);
    for I := 1 to LineCount do begin
      e.Text.CommandProcessor(ecDeleteLine, #0, nil);
      ShowUpdate(50);
    end;

    // Test folds
    CollapseUncollapse;

    // Close
    e.Text.Text := '';
    MainForm.EditorList.CloseEditor(e);

    Result := True;
  except
    Result := False;
  end;
end;

function TTestClass.TestAll: Boolean;
begin
  Result := TestEditorList { and TestActions and TestCompilerOptions and  TestEditor};
end;

constructor TTestClass.Create;
begin
end;

end.

