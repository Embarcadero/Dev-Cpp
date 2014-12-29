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

unit ProfileAnalysisFrm;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Spin, CBUtils;
{$ENDIF}
{$IFDEF LINUX}
SysUtils, Variants, Classes, QGraphics, QControls, QForms,
QDialogs, QStdCtrls, QComCtrls, QExtCtrls;
{$ENDIF}

type
  TProfileAnalysisForm = class(TForm)
    MainPanel: TPanel;
    ProfilePageControl: TPageControl;
    tabFlat: TTabSheet;
    Splitter2: TSplitter;
    memFlat: TMemo;
    lvFlat: TListView;
    tabGraph: TTabSheet;
    Splitter1: TSplitter;
    memGraph: TMemo;
    lvGraph: TListView;
    tabOpts: TTabSheet;
    FuncHiding: TGroupBox;
    chkHideNotCalled: TCheckBox;
    chkSuppressStatic: TCheckBox;
    PrevText: TLabel;
    spnMinCount: TSpinEdit;
    PostText: TLabel;
    btnApply: TButton;
    CustomCommands: TGroupBox;
    chkCustom: TCheckBox;
    editCustom: TEdit;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lvGraphCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw:
      Boolean);
    procedure lvFlatCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw:
      Boolean);
    procedure lvFlatMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure lvFlatClick(Sender: TObject);
    procedure ProfilePageControlChange(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure chkCustomClick(Sender: TObject);
    procedure commandUpdate(Sender: TObject);
    procedure lvFlatAdvancedCustomDraw(Sender: TCustomListView;
      const ARect: TRect; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure lvGraphAdvancedCustomDraw(Sender: TCustomListView;
      const ARect: TRect; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
  private
    flatloaded: boolean;
    graphloaded: boolean;
    procedure LoadText;
    procedure DoFlat;
    procedure DoGraph;
  end;

var
  ProfileAnalysisForm: TProfileAnalysisForm = nil;

implementation

uses
{$IFDEF WIN32}
  devcfg, version, utils, main, ShellAPI, StrUtils, MultiLangSupport, CppParser,
  editor;
{$ENDIF}
{$IFDEF LINUX}
devcfg, version, utils, main, StrUtils, MultiLangSupport, CppParser,
editor, Types;
{$ENDIF}

{$R *.dfm}

procedure TProfileAnalysisForm.FormShow(Sender: TObject);
begin
  LoadText;

  // Spawn DoFlat
  DoFlat;
end;

procedure TProfileAnalysisForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  ProfileAnalysisForm := nil;
end;

procedure TProfileAnalysisForm.DoFlat;
var
  Params: AnsiString;
  Dir: AnsiString;
  I, J: integer;
  Line, Phrase: AnsiString;
  spacepos: integer;
  buffer: TStringList;
  addeditem: TListItem;
begin

  // Set up GPROF execution...
  if not chkCustom.Checked then begin

    Params := ' ' + GPROF_CMD_GENFLAT;

    // Checkbox options
    if not chkHideNotCalled.checked then
      Params := Params + ' -z';
    if chkSuppressStatic.checked then
      Params := Params + ' -a';
    Params := Params + ' -m ' + spnMinCount.Text;

    if Assigned(MainForm.Project) then begin
      Dir := ExtractFilePath(MainForm.Project.Executable);
      Params := Params + ' "' + ExtractFileName(MainForm.Project.Executable) + '"';
    end else begin
      Dir := ExtractFilePath(MainForm.EditorList.GetEditor.FileName);
      Params := Params + ' "' + ExtractFileName(ChangeFileExt(MainForm.EditorList.GetEditor.FileName, EXE_EXT)) + '"';
    end;
  end else begin
    Params := editCustom.Text + ' ' + GPROF_CMD_GENFLAT;
    if Assigned(MainForm.Project) then
      Dir := ExtractFilePath(MainForm.Project.Executable)
    else
      Dir := ExtractFilePath(MainForm.EditorList.GetEditor.FileName);
  end;

  // Run a flat output
  buffer := TStringList.Create;
  buffer.Text := RunAndGetOutput(devCompilerSets.CurrentSet.gprofName + Params, Dir, nil, nil, False);

  i := 0;

  // Find and skip headers
  while (i < buffer.Count) and not ContainsStr(buffer[i], 'Ts/call') do
    Inc(i);

  Inc(i);

  // Process regular output
  lvFlat.Items.BeginUpdate;
  while (i < buffer.Count) and not SameStr(buffer[i], '') do begin

    addeditem := lvFlat.Items.Add;
    addeditem.Caption := Copy(buffer[i], 55, Length(buffer[i]) - 54); // function name

    // Store info of function in Data pointer
    if Pos('(', addeditem.Caption) > 0 then
      Phrase := Copy(addeditem.Caption, 1, Pos('(', addeditem.Caption) - 1)
    else
      Phrase := addeditem.Caption;
    addeditem.Data := MainForm.CppParser.FindStatementOf(Phrase, nil);

    // Dive remaining part based on spaces
    Line := TrimLeft(buffer[i]);
    for J := 1 to 6 do begin

      // Copy until next space
      Line := TrimLeft(Line);
      spacepos := Pos(' ', Line);
      addeditem.SubItems.Add(Copy(Line, 1, spacepos));
      Delete(Line, 1, spacepos);
    end;

    Inc(i);
  end;
  lvFlat.Items.EndUpdate;

  Inc(i);

  // Print output help
  memFlat.Lines.BeginUpdate;
  while (i < buffer.Count) do begin
    memFlat.Lines.Add(buffer[i]);
    Inc(i);
  end;
  memFlat.Lines.EndUpdate;

  buffer.Free;

  flatloaded := true;
end;

procedure TProfileAnalysisForm.DoGraph;
var
  Params: AnsiString;
  Dir: AnsiString;
  I, J, startcol: integer;
  Line, Phrase: AnsiString;
  spacepos: integer;
  buffer: TStringList;
  addeditem: TListItem;
begin

  // Set up GPROF execution...
  if not chkCustom.Checked then begin

    Params := ' ' + GPROF_CMD_GENMAP;

    // Checkbox options
    if not chkHideNotCalled.checked then
      Params := Params + ' -z';
    if chkSuppressStatic.checked then
      Params := Params + ' -a';
    Params := Params + ' -m ' + spnMinCount.Text;

    if Assigned(MainForm.Project) then begin
      Dir := ExtractFilePath(MainForm.Project.Executable);
      Params := Params + ' "' + ExtractFileName(MainForm.Project.Executable) + '"';
    end else begin
      Dir := ExtractFilePath(MainForm.EditorList.GetEditor.FileName);
      Params := Params + ' "' + ExtractFileName(ChangeFileExt(MainForm.EditorList.GetEditor.FileName, EXE_EXT)) + '"';
    end;
  end else begin
    Params := editCustom.Text + ' -q';
    if Assigned(MainForm.Project) then
      Dir := ExtractFilePath(MainForm.Project.Executable)
    else
      Dir := ExtractFilePath(MainForm.EditorList.GetEditor.FileName);
  end;

  // Run a graph output
  buffer := TStringList.Create;
  buffer.Text := RunAndGetOutput(devCompilerSets.CurrentSet.gprofName + Params, Dir, nil, nil, False);

  i := 0;

  // Find and skip headers
  while (i < buffer.Count) and not ContainsStr(buffer[i], '% time') do
    Inc(i);

  Inc(i);

  // Process regular output
  lvGraph.Items.BeginUpdate;
  while (i < buffer.Count) and not SameStr(buffer[i], '') do begin

    addeditem := lvGraph.Items.Add;

    if StartsStr('---', buffer[i]) then begin
      addeditem.Caption := '';
    end else begin

      // Divide remaining part based on spaces
      if StartsStr('[', buffer[i]) then
        addeditem.Caption := Copy(buffer[i], 46, Length(buffer[i]) - 45)
      else
        addeditem.Caption := Copy(buffer[i], 50, Length(buffer[i]) - 49);

      // Store info of function in Data pointer
      if Pos('(', addeditem.Caption) > 0 then
        Phrase := Copy(addeditem.Caption, 1, Pos('(', addeditem.Caption) - 1)
      else
        Phrase := addeditem.Caption;
      addeditem.Data := MainForm.CppParser.FindStatementOf(Phrase, nil);

      // Divide remaining part based on spaces
      Line := TrimLeft(buffer[i]);

      // Add empty columns
      if not StartsStr('[', line) then begin
        addeditem.SubItems.Add('');
        addeditem.SubItems.Add('');
        startcol := 3;
      end else
        startcol := 1;

      for J := startcol to 5 do begin

        // Copy until next space
        Line := TrimLeft(Line);
        spacepos := Pos(' ', Line);
        addeditem.SubItems.Add(Copy(Line, 1, spacepos));
        Delete(Line, 1, spacepos);
      end;
    end;
    Inc(i);
  end;
  lvGraph.Items.EndUpdate;

  Inc(i);

  // Print output help
  memGraph.Lines.BeginUpdate;
  while (i < buffer.Count) do begin
    memGraph.Lines.Add(buffer[i]);
    Inc(i);
  end;
  memGraph.Lines.EndUpdate;

  buffer.Free;

  graphloaded := true;
end;

procedure TProfileAnalysisForm.lvFlatCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
  if not (cdsSelected in State) then begin
    if Assigned(Item.Data) then
      Sender.Canvas.Font.Color := clBlue
    else
      Sender.Canvas.Font.Color := clWindowText;
  end;
end;

procedure TProfileAnalysisForm.lvGraphCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
  if not (cdsSelected in State) then begin
    if not StartsStr('---', Item.Caption) then begin
      if Assigned(Item.Data) then
        Sender.Canvas.Font.Color := clBlue
      else
        Sender.Canvas.Font.Color := clWindowText;
    end;
  end;
end;

procedure TProfileAnalysisForm.LoadText;
begin
  // Set interface font
  Font.Name := devData.InterfaceFont;
  Font.Size := devData.InterfaceFontSize;

  Caption := Lang[ID_PROF_CAPTION];
  tabFlat.Caption := Lang[ID_PROF_TABFLAT];
  tabGraph.Caption := Lang[ID_PROF_TABGRAPH];
  tabOpts.Caption := Lang[ID_PROF_TABOPTS];

  FuncHiding.Caption := Lang[ID_PROF_FUNCHIDING];
  chkHideNotCalled.Caption := Lang[ID_PROF_HIDENOTLONG];
  chkSuppressStatic.Caption := Lang[ID_PROF_SUPPRESSTATIC];
  PrevText.Caption := Lang[ID_PROF_PREVTEXT];
  PostText.Caption := Lang[ID_PROF_POSTTEXT];
  btnApply.Caption := Lang[ID_PROF_APPLY];
  CustomCommands.Caption := Lang[ID_PROF_CUSTOMCOMMANDS];
  chkCustom.Caption := Lang[ID_PROF_CUSTOMUSE];
end;

procedure TProfileAnalysisForm.lvFlatMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  It: TListItem;
begin
  with TListView(Sender) do begin
    It := GetItemAt(X, Y);
    if Assigned(It) and Assigned(It.Data) then
      Cursor := crHandPoint
    else
      Cursor := crDefault;
  end;
end;

procedure TProfileAnalysisForm.lvFlatClick(Sender: TObject);
var
  MouseItem: TListItem;
  MousePos: TPoint;
  e: TEditor;
  Statement: PStatement;
  ListView : TListView;
begin
  ListView := TListView(Sender);
  MousePos := ListView.ScreenToClient(Mouse.CursorPos);
  MouseItem := ListView.GetItemAt(MousePos.X, MousePos.Y);
  if Assigned(MouseItem) and Assigned(MouseItem.Data) then begin
    Statement := PStatement(MouseItem.Data);
    e := MainForm.EditorList.GetEditorFromFileName(Statement^._DefinitionFileName);
    if Assigned(e) then begin
      e.SetCaretPos(Statement^._DefinitionLine, 1);
      e.Activate;
    end;
  end;
end;

procedure TProfileAnalysisForm.ProfilePageControlChange(Sender: TObject);
begin
  case ProfilePageControl.ActivePageIndex of
    0: begin
        if not flatloaded then begin
          DoFlat;
          lvFlat.SetFocus;
        end;
      end;
    1: begin
        if not graphloaded then begin
          DoGraph;
          lvGraph.SetFocus;
        end;
      end;
    2: begin
        commandUpdate(nil);
      end;
  end;
end;

procedure TProfileAnalysisForm.btnApplyClick(Sender: TObject);
begin
  flatloaded := false;
  graphloaded := false;

  // Respawn DoFlat
  ProfilePageControl.ActivePage := tabFlat;
end;

procedure TProfileAnalysisForm.chkCustomClick(Sender: TObject);
begin
  chkHideNotCalled.Enabled := not chkCustom.Checked;
  chkSuppressStatic.Enabled := not chkCustom.Checked;
  spnMinCount.Enabled := not chkCustom.Checked;

  editCustom.Enabled := chkCustom.Checked;
end;

procedure TProfileAnalysisForm.CommandUpdate(Sender: TObject);
var
  assembly: AnsiString;
begin
  if not chkCustom.Checked then begin
    assembly := devCompilerSets.CurrentSet.gprofName;
    if Assigned(MainForm.Project) then
      assembly := assembly + ' "' + ExtractFileName(MainForm.Project.Executable) + '"'
    else
      assembly := assembly + ' "' + ExtractFileName(ChangeFileExt(MainForm.EditorList.GetEditor.FileName, EXE_EXT)) +
        '"';
    if not chkHideNotCalled.Checked then
      assembly := assembly + ' -z';
    if chkSuppressStatic.Checked then
      assembly := assembly + ' -a';
    assembly := assembly + ' -m ' + spnMinCount.Text;
    editCustom.Text := assembly;
  end;
end;

procedure TProfileAnalysisForm.lvFlatAdvancedCustomDraw(
  Sender: TCustomListView; const ARect: TRect; Stage: TCustomDrawStage;
  var DefaultDraw: Boolean);
begin
  SendMessage(lvFlat.Handle, WM_CHANGEUISTATE, MAKEWPARAM(UIS_SET, UISF_HIDEFOCUS), 0);
end;

procedure TProfileAnalysisForm.lvGraphAdvancedCustomDraw(
  Sender: TCustomListView; const ARect: TRect; Stage: TCustomDrawStage;
  var DefaultDraw: Boolean);
begin
  SendMessage(lvFlat.Handle, WM_CHANGEUISTATE, MAKEWPARAM(UIS_SET, UISF_HIDEFOCUS), 0);
end;

end.

