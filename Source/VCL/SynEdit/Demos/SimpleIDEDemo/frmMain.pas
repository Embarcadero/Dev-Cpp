{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: frmMain.pas, released 2000-11-11.

The Original Code is part of the SimpleIDEDemo project, written by
Michael Hieke for the SynEdit component suite.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: frmMain.pas,v 1.7 2004/04/24 17:04:54 markonjezic Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit frmMain;

{$I SynEdit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActnList, ImgList, ComCtrls, ToolWin, SynEdit, SynEditHighlighter,
  SynHighlighterPas, uSimpleIDEDebugger, Menus, SynEditTypes, System.Actions;

type
  TSimpleIDEMainForm = class(TForm)
    ActionClearAllBreakpoints: TAction;
    ActionDebugGotoCursor: TAction;
    ActionDebugPause: TAction;
    ActionDebugRun: TAction;
    ActionDebugStep: TAction;
    ActionDebugStop: TAction;
    ActionListMain: TActionList;
    ActionToggleBreakpoint: TAction;
    ImageListActions: TImageList;
    ImageListGutterGlyphs: TImageList;
    MainMenu: TMainMenu;
    MenuItemDebug: TMenuItem;
    miClearBreakpoints: TMenuItem;
    miDebugGotoCursor: TMenuItem;
    miDebugPause: TMenuItem;
    miDebugRun: TMenuItem;
    miDebugStep: TMenuItem;
    miDebugStop: TMenuItem;
    miToggleBreakpoint: TMenuItem;
    N1: TMenuItem;
    Statusbar: TStatusBar;
    SynEditor: TSynEdit;
    SynPasSyn: TSynPasSyn;
    ToolBarDebug: TToolBar;
    ToolButtonClearAllBreakpoints: TToolButton;
    ToolButtonGotoCursor: TToolButton;
    ToolButtonPause: TToolButton;
    ToolButtonRun: TToolButton;
    ToolButtonStep: TToolButton;
    ToolButtonStop: TToolButton;
    ToolButtonToggleBreakpoint: TToolButton;
    ToolButtonSeparator: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SynEditorSpecialLineColors(Sender: TObject; Line: Integer;
      var Special: Boolean; var FG, BG: TColor);
    procedure ActionDebugRunExecute(Sender: TObject);
    procedure ActionDebugRunUpdate(Sender: TObject);
    procedure ActionDebugStepExecute(Sender: TObject);
    procedure ActionDebugStepUpdate(Sender: TObject);
    procedure ActionDebugGotoCursorExecute(Sender: TObject);
    procedure ActionDebugGotoCursorUpdate(Sender: TObject);
    procedure ActionDebugPauseExecute(Sender: TObject);
    procedure ActionDebugPauseUpdate(Sender: TObject);
    procedure ActionDebugStopExecute(Sender: TObject);
    procedure ActionDebugStopUpdate(Sender: TObject);
    procedure ActionToggleBreakpointExecute(Sender: TObject);
    procedure ActionToggleBreakpointUpdate(Sender: TObject);
    procedure ActionClearAllBreakpointsExecute(Sender: TObject);
    procedure ActionClearAllBreakpointsUpdate(Sender: TObject);
    procedure SynEditorGutterClick(Sender: TObject; Button: TMouseButton;
      X, Y, Line: Integer; Mark: TSynEditMark);
  private
    FCurrentLine: integer;
    FDebugger: TSampleDebugger;
    procedure DebuggerBreakpointChange(Sender: TObject; ALine: integer);
    procedure DebuggerCurrentLineChange(Sender: TObject);
    procedure DebuggerStateChange(Sender: TObject; OldState,
      NewState: TDebuggerState);
    procedure DebuggerYield(Sender: TObject);
    procedure PaintGutterGlyphs(ACanvas: TCanvas; AClip: TRect;
      FirstLine, LastLine: integer);
    procedure SetCurrentLine(ALine: integer);
  end;

var
  SimpleIDEMainForm: TSimpleIDEMainForm;

implementation

{$R *.DFM}

{ TGutterMarkDrawPlugin }

type
  TDebugSupportPlugin = class(TSynEditPlugin)
  protected
    fForm: TSimpleIDEMainForm;
    procedure AfterPaint(ACanvas: TCanvas; const AClip: TRect;
      FirstLine, LastLine: integer); override;
    procedure LinesInserted(FirstLine, Count: integer); override;
    procedure LinesDeleted(FirstLine, Count: integer); override;
  public
    constructor Create(AForm: TSimpleIDEMainForm);
  end;

constructor TDebugSupportPlugin.Create(AForm: TSimpleIDEMainForm);
begin
  inherited Create(AForm.SynEditor);
  fForm := AForm;
end;

procedure TDebugSupportPlugin.AfterPaint(ACanvas: TCanvas; const AClip: TRect;
  FirstLine, LastLine: integer);
begin
  fForm.PaintGutterGlyphs(ACanvas, AClip, FirstLine, LastLine);
end;

procedure TDebugSupportPlugin.LinesInserted(FirstLine, Count: integer);
begin
// Note: You will need this event if you want to track the changes to
//       breakpoints in "Real World" apps, where the editor is not read-only
end;

procedure TDebugSupportPlugin.LinesDeleted(FirstLine, Count: integer);
begin
// Note: You will need this event if you want to track the changes to
//       breakpoints in "Real World" apps, where the editor is not read-only
end;

{ TSimpleIDEMainForm }

procedure TSimpleIDEMainForm.FormCreate(Sender: TObject);
var
  Settings: TStringList;
begin
  FCurrentLine := -1;
  FDebugger := TSampleDebugger.Create;
  with FDebugger do begin
    OnBreakpointChange := DebuggerBreakpointChange;
    OnCurrentLineChange := DebuggerCurrentLineChange;
    OnStateChange := DebuggerStateChange;
    OnYield := DebuggerYield;
  end;

  TDebugSupportPlugin.Create(Self);

  Settings := TStringList.Create;
  try
    SynPasSyn.EnumUserSettings(Settings);
    if Settings.Count > 0 then
      SynPasSyn.UseUserSettings(Settings.Count - 1);
  finally
    Settings.Free;
  end;
  SynEditor.Text := SampleSource;
end;

procedure TSimpleIDEMainForm.FormDestroy(Sender: TObject);
begin
  FDebugger.Free;
end;

procedure TSimpleIDEMainForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if FDebugger.IsRunning then begin
    FDebugger.Stop;
    CanClose := FALSE;
  end;
end;

procedure TSimpleIDEMainForm.SynEditorSpecialLineColors(Sender: TObject;
  Line: Integer; var Special: Boolean; var FG, BG: TColor);
var
  LI: TDebuggerLineInfos;
begin
  if FDebugger <> nil then begin
    LI := FDebugger.GetLineInfos(Line);
    if dlCurrentLine in LI then begin
      Special := TRUE;
      FG := clWhite;
      BG := clBlue;
    end else if dlBreakpointLine in LI then begin
      Special := TRUE;
      FG := clWhite;
      if dlExecutableLine in LI then
        BG := clRed
      else
        BG := clGray;
    end;
  end;
end;

procedure TSimpleIDEMainForm.DebuggerBreakpointChange(Sender: TObject;
  ALine: integer);
begin
  if (ALine >= 1) and (ALine <= SynEditor.Lines.Count) then
  begin
    SynEditor.InvalidateGutterLine(ALine);
    SynEditor.InvalidateLine(ALine);
  end
  else
    SynEditor.Invalidate;
end;

procedure TSimpleIDEMainForm.DebuggerCurrentLineChange(Sender: TObject);
begin
  if (FDebugger <> nil) and not FDebugger.IsRunning then
    SetCurrentLine(FDebugger.CurrentLine)
  else
    SetCurrentLine(-1);
end;

procedure TSimpleIDEMainForm.DebuggerStateChange(Sender: TObject; OldState,
  NewState: TDebuggerState);
var
  s: string;
begin
  case NewState of
    dsRunning: s := 'Program is running';
    dsPaused: s := 'Program is paused';
  else
    s := 'Ready';
  end;
  Statusbar.SimpleText := ' ' + s;
end;

procedure TSimpleIDEMainForm.DebuggerYield(Sender: TObject);
begin
  UpdateActions;
  Application.ProcessMessages;
end;

procedure TSimpleIDEMainForm.PaintGutterGlyphs(ACanvas: TCanvas; AClip: TRect;
  FirstLine, LastLine: integer);
var
  LH, X, Y: integer;
  LI: TDebuggerLineInfos;
  ImgIndex: integer;
begin
  if FDebugger <> nil then
  begin
    FirstLine := SynEditor.RowToLine(FirstLine);
    LastLine := SynEditor.RowToLine(LastLine);
    X := 14;
    LH := SynEditor.LineHeight;
    while FirstLine <= LastLine do
    begin
      Y := (LH - ImageListGutterGlyphs.Height) div 2
           + LH * (SynEditor.LineToRow(FirstLine) - SynEditor.TopLine);
      LI := FDebugger.GetLineInfos(FirstLine);
      if dlCurrentLine in LI then begin
        if dlBreakpointLine in LI then
          ImgIndex := 2
        else
          ImgIndex := 1;
      end else if dlExecutableLine in LI then begin
        if dlBreakpointLine in LI then
          ImgIndex := 3
        else
          ImgIndex := 0;
      end else begin
        if dlBreakpointLine in LI then
          ImgIndex := 4
        else
          ImgIndex := -1;
      end;
      if ImgIndex >= 0 then
        ImageListGutterGlyphs.Draw(ACanvas, X, Y, ImgIndex);
      Inc(FirstLine);
    end;
  end;
end;

procedure TSimpleIDEMainForm.SetCurrentLine(ALine: integer);
begin
  if FCurrentLine <> ALine then
  begin
    SynEditor.InvalidateGutterLine(FCurrentLine);
    SynEditor.InvalidateLine(FCurrentLine);
    FCurrentLine := ALine;
    if (FCurrentLine > 0) and (SynEditor.CaretY <> FCurrentLine) then
      SynEditor.CaretXY := BufferCoord(1, FCurrentLine);
    SynEditor.InvalidateGutterLine(FCurrentLine);
    SynEditor.InvalidateLine(FCurrentLine);
  end;
end;

procedure TSimpleIDEMainForm.ActionDebugRunExecute(Sender: TObject);
begin
  FDebugger.Run;
end;

procedure TSimpleIDEMainForm.ActionDebugRunUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (FDebugger <> nil) and FDebugger.CanRun;
end;

procedure TSimpleIDEMainForm.ActionDebugStepExecute(Sender: TObject);
begin
  FDebugger.Step;
end;

procedure TSimpleIDEMainForm.ActionDebugStepUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (FDebugger <> nil) and FDebugger.CanStep;
end;

procedure TSimpleIDEMainForm.ActionDebugGotoCursorExecute(Sender: TObject);
begin
  FDebugger.GotoCursor(SynEditor.CaretY);
end;

procedure TSimpleIDEMainForm.ActionDebugGotoCursorUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (FDebugger <> nil)
    and FDebugger.CanGotoCursor(SynEditor.CaretY);
end;

procedure TSimpleIDEMainForm.ActionDebugPauseExecute(Sender: TObject);
begin
  FDebugger.Pause;
end;

procedure TSimpleIDEMainForm.ActionDebugPauseUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (FDebugger <> nil) and FDebugger.CanPause;
end;

procedure TSimpleIDEMainForm.ActionDebugStopExecute(Sender: TObject);
begin
  FDebugger.Stop;
end;

procedure TSimpleIDEMainForm.ActionDebugStopUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (FDebugger <> nil) and FDebugger.CanStop;
end;

procedure TSimpleIDEMainForm.ActionToggleBreakpointExecute(Sender: TObject);
begin
  FDebugger.ToggleBreakpoint(SynEditor.CaretY);
end;

procedure TSimpleIDEMainForm.ActionToggleBreakpointUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := FDebugger <> nil;
end;

procedure TSimpleIDEMainForm.ActionClearAllBreakpointsExecute(
  Sender: TObject);
begin
  FDebugger.ClearAllBreakpoints;
end;

procedure TSimpleIDEMainForm.ActionClearAllBreakpointsUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (FDebugger <> nil)
    and FDebugger.HasBreakpoints;
end;

procedure TSimpleIDEMainForm.SynEditorGutterClick(Sender: TObject;
  Button: TMouseButton; X, Y, Line: Integer; Mark: TSynEditMark);
begin
  if FDebugger <> nil then
    FDebugger.ToggleBreakpoint(SynEditor.RowToLine(Line));
end;

end.

  
