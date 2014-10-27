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

unit Editor;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, CodeCompletion, CppParser, SynExportTeX,
  SynEditExport, SynExportRTF,
  Menus, ImgList, ComCtrls, StdCtrls, ExtCtrls, SynEdit, SynEditKeyCmds, version, SynEditCodeFolding, SynExportHTML,
  SynEditTextBuffer, Math, StrUtils, SynEditTypes, SynEditHighlighter, DateUtils, CodeToolTip;
{$ENDIF}
{$IFDEF LINUX}
SysUtils, Classes, Graphics, QControls, QForms, QDialogs, CodeCompletion, CppParser,
QMenus, QImgList, QComCtrls, QStdCtrls, QExtCtrls, QSynEdit, QSynEditKeyCmds, version,
QSynCompletionProposal, StrUtils, QSynEditTypes, QSynEditHighlighter, DevCodeToolTip, QSynAutoIndent, Types;
{$ENDIF}

type
  TEditor = class;
  TDebugGutter = class(TSynEditPlugin)
  protected
    e: TEditor;
    procedure AfterPaint(ACanvas: TCanvas; const AClip: TRect; FirstLine, LastLine: integer); override;
    procedure LinesInserted(FirstLine, Count: integer); override;
    procedure LinesDeleted(FirstLine, Count: integer); override;
  public
    constructor Create(editor: TEditor);
  end;

  // Define what we want to see of the word at any position
  TWordPurpose = (
    wpCompletion, // walk backwards over words, array, functions, parents, no forward movement
    wpEvaluation, // walk backwards over words, array, functions, parents, forwards over words, array
    wpInformation // walk backwards over words, array, functions, parents, forwards over words
    );

  // Define why we are allowed to change the cursor to a handpoint
  THandPointReason = (
    hprPreprocessor, // cursor hovers above preprocessor line
    hprIdentifier, // cursor hovers above identifier
    hprSelection, // cursor hovers above selection
    hprNone // mouseover not allowed
    );

  TEditor = class(TObject)
  private
    fInProject: boolean;
    fFileName: AnsiString;
    fNew: boolean;
    fText: TSynEdit;
    fTabSheet: TTabSheet;
    fErrorLine: integer;
    fActiveLine: integer;
    fDebugGutter: TDebugGutter;
    fCurrentWord: AnsiString;
    fCurrentEvalWord: AnsiString;
    fIgnoreCaretChange: boolean;
    fPreviousEditors: TList;
    fDblClickTime: Cardinal;
    fDblClickMousePos: TBufferCoord;
    fCompletionTimer: TTimer;
    fCompletionBox: TCodeCompletion;
    fFunctionTipTimer: TTimer;
    fFunctionTip: TCodeToolTip;
    WaitForParenths: integer; // These closing chars have already been typed, ignore manual closings
    WaitForArray: integer; // ... 2 means true, 1 means 'handled by KeyDown', 0 means false
    WaitForCurly: integer; // etc
    WaitForSingleQuote: integer;
    WaitForDoubleQuote: integer;
    procedure EditorKeyPress(Sender: TObject; var Key: Char);
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditorKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditorDblClick(Sender: TObject);
    procedure EditorClick(Sender: TObject);
    procedure EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure EditorReplaceText(Sender: TObject; const aSearch, aReplace: AnsiString; Line, Column: integer; var Action:
      TSynReplaceAction);
    procedure EditorDropFiles(Sender: TObject; x, y: integer; aFiles: TStrings);
    procedure EditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure EditorExit(Sender: TObject);
    procedure EditorGutterClick(Sender: TObject; Button: TMouseButton; x, y, Line: integer; mark: TSynEditMark);
    procedure EditorSpecialLineColors(Sender: TObject; Line: integer; var Special: boolean; var FG, BG: TColor);
    procedure EditorPaintTransient(Sender: TObject; Canvas: TCanvas; TransientType: TTransientType);
    procedure EditorEnter(Sender: TObject);
    procedure CompletionKeyPress(Sender: TObject; var Key: Char);
    procedure CompletionInsert(const append: AnsiString);
    procedure CompletionTimer(Sender: TObject);
    function FunctionTipAllowed: boolean;
    procedure FunctionTipTimer(Sender: TObject);
    function HandpointAllowed(var mousepos: TBufferCoord): THandPointReason; // returns for what reason it is allowed
    procedure SetFileName(const value: AnsiString);
    procedure OnMouseOverEvalReady(const evalvalue: AnsiString);
    function HasBreakPoint(Line: integer): integer;
    procedure DebugAfterPaint(ACanvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer);
    function GetPageControl: TPageControl;
    procedure SetPageControl(Value: TPageControl);
  public
    constructor Create(const Filename: AnsiString; InProject, NewFile: boolean; ParentPageControl: TPageControl);
    destructor Destroy; override;
    function Save: boolean;
    function SaveAs: boolean;
    procedure Activate;
    procedure GotoLine;
    procedure SetCaretPos(Line, Col: integer; SetTopLine: boolean = true); // takes folds into account
    procedure ExportToHTML;
    procedure ExportToRTF;
    procedure ExportToTEX;
    procedure InsertString(Value: AnsiString; MoveCursor: boolean);
    procedure SetErrorFocus(Col, Line: integer);
    procedure GotoActiveBreakpoint;
    procedure SetActiveBreakpointFocus(Line: integer);
    procedure RemoveBreakpointFocus;
    procedure UpdateCaption(const NewCaption: AnsiString);
    procedure InsertDefaultText;
    procedure ToggleBreakPoint(Line: integer);
    function GetWordAtPosition(P: TBufferCoord; Purpose: TWordPurpose): AnsiString;
    procedure IndentSelection;
    procedure UnindentSelection;
    procedure InitCompletion;
    procedure ShowCompletion;
    procedure DestroyCompletion;
    property PreviousEditors: TList read fPreviousEditors;
    property FileName: AnsiString read fFileName write SetFileName;
    property InProject: boolean read fInProject write fInProject;
    property New: boolean read fNew write fNew;
    property Text: TSynEdit read fText write fText;
    property TabSheet: TTabSheet read fTabSheet write fTabSheet;
    property FunctionTip: TCodeToolTip read fFunctionTip;
    property CompletionBox: TCodeCompletion read fCompletionBox;
    property PageControl: TPageControl read GetPageControl write SetPageControl;
  end;

implementation

uses
{$IFDEF WIN32}
  main, project, MultiLangSupport, devcfg, utils,
  DataFrm, GotoLineFrm, Macros, debugreader, IncrementalFrm, CodeCompletionForm, SynEditMiscClasses;
{$ENDIF}
{$IFDEF LINUX}
Xlib, main, project, MultiLangSupport, devcfg, Search_Center, utils,
datamod, GotoLineFrm, Macros;
{$ENDIF}

{ TDebugGutter }

constructor TDebugGutter.Create(editor: TEditor);
begin
  inherited Create(editor.Text);
  e := editor;
end;

procedure TDebugGutter.AfterPaint(ACanvas: TCanvas; const AClip: TRect; FirstLine, LastLine: integer);
begin
  e.DebugAfterPaint(ACanvas, AClip, FirstLine, LastLine);
end;

procedure TDebugGutter.LinesInserted(FirstLine, Count: integer);
var
  I, line: integer;
  bp: PBreakPoint;

  procedure LinesInsertedList(Items: TListItems);
  var
    I: integer;
  begin
    for I := 0 to Items.Count - 1 do begin
      if SameFileName(e.fFileName, Items[i].SubItems[1]) then begin
        line := StrToIntDef(Items[i].Caption, -1);
        if (line >= FirstLine) then
          Items[i].Caption := IntToStr(line + Count);
      end;
    end;
  end;
begin
  for I := 0 to MainForm.Debugger.BreakPointList.Count - 1 do begin
    bp := PBreakPoint(MainForm.Debugger.BreakPointList.Items[I]);
    if (integer(bp^.editor) = integer(e)) and (bp^.line >= FirstLine) then
      Inc(bp^.line, Count);
  end;

  LinesInsertedList(MainForm.CompilerOutput.Items);
  LinesInsertedList(MainForm.ResourceOutput.Items);
  LinesInsertedList(MainForm.FindOutput.Items);
end;

procedure TDebugGutter.LinesDeleted(FirstLine, Count: integer);
var
  I, line: integer;
  bp: PBreakPoint;

  procedure LinesDeletedList(Items: TListItems);
  var
    I: integer;
  begin
    for I := Items.Count - 1 downto 0 do begin
      if SameFileName(e.fFileName, Items[i].SubItems[1]) then begin
        line := StrToIntDef(Items[i].Caption, -1);
        if (line >= FirstLine) then begin
          if (line >= FirstLine + Count) then
            Items[i].Caption := IntToStr(line - Count)
          else
            Items.Delete(I);
        end;
      end;
    end;
  end;

begin
  for I := MainForm.Debugger.BreakPointList.Count - 1 downto 0 do begin
    bp := PBreakPoint(MainForm.Debugger.BreakPointList.Items[I]);
    if (integer(bp^.editor) = integer(e)) and (bp^.line >= FirstLine) then begin
      if (bp^.line >= FirstLine + Count) then
        Dec(bp^.line, Count)
      else
        e.ToggleBreakPoint(bp^.line); // remove breakpoints INSIDE deleted selection
    end;
  end;

  // really delete items?
  LinesDeletedList(MainForm.CompilerOutput.Items);
  LinesDeletedList(MainForm.ResourceOutput.Items);
  LinesDeletedList(MainForm.FindOutput.Items);
end;

{ TEditor }

constructor TEditor.Create(const Filename: AnsiString; InProject, NewFile: boolean; ParentPageControl: TPageControl);
var
  s: AnsiString;
  I: integer;
  e: TEditor;
begin
  // Set generic options
  fErrorLine := -1;
  fActiveLine := -1;
  fInProject := InProject;
  if FileName <> '' then
    fFileName := Filename
  else
    fFileName := Lang[ID_UNTITLED] + IntToStr(dmMain.GetNewFileNumber);

  // Remember previous tabs
  fPreviousEditors := TList.Create;
  if Assigned(ParentPageControl.ActivePage) then begin
    e := TEditor(ParentPageControl.ActivePage.Tag); // copy list of previous editor
    for I := 0 to e.PreviousEditors.Count - 1 do
      fPreviousEditors.Add(e.PreviousEditors[i]);
    fPreviousEditors.Add(Pointer(e)); // make current editor history too
  end;

  // Create a new tab
  fTabSheet := TTabSheet.Create(ParentPageControl);
  fTabSheet.Caption := ExtractFileName(fFilename); // UntitlexX or main.cpp
  fTabSheet.PageControl := ParentPageControl;
  fTabSheet.Tag := integer(Self); // Define an index for each tab

  // Create an editor and set static options
  fText := TSynEdit.Create(fTabSheet);

  // Load the file using Lines
  if not NewFile and FileExists(FileName) then begin
    fText.Lines.LoadFromFile(FileName);
    fNew := False;

    // Save main.cpp as main.123456789.cpp
    if devData.Backups then begin
      s := '.' + IntToStr(DateTimeToUnix(Now)) + ExtractFileExt(FileName);
      fText.Lines.SaveToFile(ChangeFileExt(FileName, s));
    end;
  end else
    fNew := True;

  fText.Parent := fTabSheet;
  fText.Visible := True;
  fText.Align := alClient;
  fText.PopupMenu := MainForm.EditorPopup;
  fText.WantTabs := True;
  fText.ShowHint := True;
  fText.OnStatusChange := EditorStatusChange;
  fText.OnReplaceText := EditorReplaceText;
  fText.OnDropFiles := EditorDropFiles;
  fText.OnDblClick := EditorDblClick;
  fText.OnClick := EditorClick;
  fText.OnMouseUp := EditorMouseUp;
  fText.OnMouseMove := EditorMouseMove;
  fText.OnGutterClick := EditorGutterClick;
  fText.OnSpecialLineColors := EditorSpecialLineColors;
  fText.OnEnter := EditorEnter;
  fText.OnExit := EditorExit;
  fText.OnPaintTransient := EditorPaintTransient;
  fText.MaxScrollWidth := 4096; // bug-fix #600748
  fText.MaxUndo := 4096;
  fText.BorderStyle := bsNone;

  fText.Gutter.LeftOffset := 4;
  fText.Gutter.RightOffset := 21;
  fText.Gutter.BorderStyle := gbsNone;

  // Set the variable options
  devEditor.AssignEditor(fText, fFileName);

  // Create a gutter
  fDebugGutter := TDebugGutter.Create(self);

  // Function parameter tips
  fFunctionTip := TCodeToolTip.Create(Application);
  fFunctionTip.Editor := fText;
  fFunctionTip.Parser := MainForm.CppParser;

  // Initialize code completion stuff
  InitCompletion;

  // Setup a monitor which keeps track of outside-of-editor changes
  MainForm.FileMonitor.Monitor(fFileName);

  // Set status bar for the first time
  EditorStatusChange(Self, [scInsertMode]);
end;

destructor TEditor.Destroy;
begin
  MainForm.EditorList.BeginUpdate;
  try
    // Deactivate the file change monitor
    MainForm.FileMonitor.UnMonitor(fFileName);

    // Delete breakpoints in this editor
    MainForm.Debugger.DeleteBreakPointsOf(self);

    // Destroy any completion stuff
    DestroyCompletion;

    // Free everything
    fFunctionTip.Free;
    fText.Free;
    fTabSheet.Free;
    fPreviousEditors.Free;
  finally
    MainForm.EditorList.EndUpdate;
  end;
  inherited;
end;

function TEditor.GetPageControl: TPageControl;
begin
  if Assigned(fTabSheet) then
    Result := fTabSheet.PageControl
  else
    Result := nil;
end;

procedure TEditor.SetPageControl(Value: TPageControl);
begin
  if Assigned(fTabSheet) then
    fTabSheet.PageControl := Value;
end;

procedure TEditor.OnMouseOverEvalReady(const evalvalue: AnsiString);
begin
  fText.Hint := fCurrentEvalWord + ' = ' + evalvalue;
  MainForm.Debugger.OnEvalReady := nil;
end;

procedure TEditor.Activate;
begin
  // Don't waste time refocusing
  if fText.Focused then
    Exit;

  // Allow the user to start typing right away
  fTabSheet.PageControl.ActivePage := fTabSheet;
  fTabSheet.PageControl.OnChange(fTabSheet.PageControl); // event is not fired when changing ActivePage
end;

procedure TEditor.EditorGutterClick(Sender: TObject; Button: TMouseButton; x, y, Line: integer; mark: TSynEditMark);
begin
  ToggleBreakPoint(Line);
end;

procedure TEditor.ToggleBreakpoint(Line: integer);
var
  thisbreakpoint, DisplayLine: integer;
begin
  thisbreakpoint := HasBreakPoint(Line);

  if thisbreakpoint <> -1 then
    MainForm.Debugger.RemoveBreakPoint(Line, self)
  else
    MainForm.Debugger.AddBreakPoint(Line, self);

  // Convert buffer to display position
  DisplayLine := fText.UncollapsedLineToLine(Line);
  fText.InvalidateGutterLine(DisplayLine);
  fText.InvalidateLine(DisplayLine);
end;

function TEditor.HasBreakPoint(Line: integer): integer;
var
  I: integer;
begin
  result := -1;
  for I := 0 to MainForm.Debugger.BreakPointList.Count - 1 do
    if integer(PBreakPoint(MainForm.Debugger.BreakPointList.Items[I])^.editor) = integer(self) then
      if PBreakPoint(MainForm.Debugger.BreakPointList.Items[I])^.line = Line then begin
        Result := I;
        break;
      end;
end;

procedure TEditor.EditorSpecialLineColors(Sender: TObject; Line: Integer; var Special: Boolean; var FG, BG: TColor);
var
  pt: TPoint;
  RealLine: integer;
begin
  RealLine := fText.LineToUncollapsedLine(Line);
  if (RealLine = fActiveLine) then begin
    StrtoPoint(pt, devEditor.Syntax.Values[cABP]);
    BG := pt.X;
    FG := pt.Y;
    Special := TRUE;
  end else if (HasBreakpoint(RealLine) <> -1) then begin
    StrtoPoint(pt, devEditor.Syntax.Values[cBP]);
    BG := pt.X;
    FG := pt.Y;
    Special := TRUE;
  end else if RealLine = fErrorLine then begin
    StrtoPoint(pt, devEditor.Syntax.Values[cErr]);
    BG := pt.X;
    FG := pt.Y;
    Special := TRUE;
  end;
end;

procedure TEditor.DebugAfterPaint(ACanvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer);
var
  X, Y, I, RealLine: integer;
begin
  X := (fText.Gutter.RealGutterWidth(fText.CharWidth) - fText.Gutter.RightOffset) div 2 - 3;
  Y := (fText.LineHeight - dmMain.GutterImages.Height) div 2 + fText.LineHeight * (FirstLine - fText.TopLine);

  for I := FirstLine to LastLine do begin
    RealLine := fText.LineToUncollapsedLine(I);
    if fActiveLine = RealLine then // prefer active line over breakpoints
      dmMain.GutterImages.Draw(ACanvas, X, Y, 1)
    else if HasBreakpoint(RealLine) <> -1 then
      dmMain.GutterImages.Draw(ACanvas, X, Y, 0)
    else if fErrorLine = RealLine then
      dmMain.GutterImages.Draw(ACanvas, X, Y, 2);

    Inc(Y, fText.LineHeight);
  end;
end;

procedure TEditor.EditorDropFiles(Sender: TObject; x, y: integer; aFiles: TStrings);
var
  sl: TStringList;
  I: integer;
  e: TEditor;
begin
  // Insert into current editor
  if devEditor.InsDropFiles then begin
    fText.CaretXY := fText.DisplayToBufferPos(fText.PixelsToRowColumn(x, y));

    sl := TStringList.Create;
    try
      for I := 0 to pred(aFiles.Count) do begin
        sl.LoadFromFile(aFiles[I]);
        fText.SelText := sl.Text;
      end;
    finally
      sl.Free;
    end;
  end else

    // Create new tab/project
    for I := 0 to aFiles.Count - 1 do begin
      e := MainForm.EditorList.FileIsOpen(aFiles[I]);
      if Assigned(e) then
        e.Activate
      else begin // if not open yet
        if GetFileTyp(aFiles[I]) = utPrj then
          MainForm.OpenProject(aFiles[I])
        else
          MainForm.OpenFile(aFiles[I]);
      end;
    end;
end;

procedure TEditor.EditorReplaceText(Sender: TObject; const aSearch, aReplace: AnsiString; Line, Column: integer; var
  Action: TSynReplaceAction);
var
  pt: TPoint;
begin
  pt := fText.ClienttoScreen(fText.RowColumnToPixels(DisplayCoord(Column, Line + 1)));
  MessageBeep(MB_ICONQUESTION);
  case MessageDlgPos(format(Lang[ID_MSG_SEARCHREPLACEPROMPT], [aSearch]), mtConfirmation, [mbYes, mbNo, mbCancel,
    mbAll], 0, pt.x, pt.y + fText.LineHeight) of
    mrYes: Action := raReplace;
    mrNo: Action := raSkip;
    mrCancel: Action := raCancel;
    mrAll: Action := raReplaceAll;
  end;
end;

// Handle WM_KILLFOCUS instead of all the special cases to hide the code tooltip

procedure TEditor.EditorExit(Sender: TObject);
begin
  fFunctionTip.ReleaseHandle;
end;

procedure TEditor.EditorEnter(Sender: TObject);
var
  I, x, y: integer;
begin
  // Set title bar to current file
  MainForm.UpdateAppTitle;

  // Set classbrowser to current file
  MainForm.ClassBrowser.CurrentFile := fFileName;

  // Set compiler selector to current file
  MainForm.UpdateCompilerList;

  // Update status bar
  MainForm.SetStatusbarLineCol;

  // Update bookmark menu
  for i := 1 to 9 do
    if fText.GetBookMark(i, x, y) then begin
      MainForm.TogglebookmarksPopItem.Items[i - 1].Checked := true;
      MainForm.TogglebookmarksItem.Items[i - 1].Checked := true;
    end else begin
      MainForm.TogglebookmarksPopItem.Items[i - 1].Checked := false;
      MainForm.TogglebookmarksItem.Items[i - 1].Checked := false;
    end;

  // Update focus of incremental search
  if Assigned(IncrementalForm) and IncrementalForm.Showing then
    IncrementalForm.Editor := fText;
end;

procedure TEditor.EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
begin
  // scModified is only fired when the modified state changes
  if scModified in Changes then begin
    if fText.Modified then begin
      MainForm.SetStatusbarMessage(Lang[ID_MODIFIED]);
      UpdateCaption('[*] ' + ExtractFileName(fFileName));
    end else begin
      MainForm.SetStatusbarMessage('');
      UpdateCaption(ExtractFileName(fFileName));
    end;
  end;

  // scSelection includes anything caret related
  if scSelection in Changes then begin
    MainForm.SetStatusbarLineCol;

    // Decrement character completion counter
    WaitForParenths := max(0, WaitForParenths - 1);
    WaitForArray := max(0, WaitForArray - 1);
    WaitForCurly := max(0, WaitForCurly - 1);
    WaitForSingleQuote := max(0, WaitForSingleQuote - 1);
    WaitForDoubleQuote := max(0, WaitForDoubleQuote - 1);

    // Update the function tip
    fFunctionTip.ForceHide := false;
    if Assigned(fFunctionTipTimer) then begin
      if fFunctionTip.Activated and FunctionTipAllowed then
        fFunctionTip.Show
      else begin // Reset the timer
        fFunctionTipTimer.Enabled := false;
        fFunctionTipTimer.Enabled := true;
      end;
    end;

    // Remove error line colors
    if not fIgnoreCaretChange then begin
      if (fErrorLine <> -1) then begin
        fText.InvalidateLine(fErrorLine);
        fText.InvalidateGutterLine(fErrorLine);
        fErrorLine := -1;
      end;
    end else
      fIgnoreCaretChange := false;
  end;

  if scInsertMode in Changes then begin
    with MainForm.Statusbar do begin
      // Set readonly / insert / overwrite
      if fText.ReadOnly then
        Panels[1].Text := Lang[ID_READONLY]
      else if fText.InsertMode then
        Panels[1].Text := Lang[ID_INSERT]
      else
        Panels[1].Text := Lang[ID_OVERWRITE];
    end;
  end;
end;

function TEditor.FunctionTipAllowed: boolean;
begin
  Result :=
    not fText.IsScrolling and // don't update when scrolling
  fText.Focused and // don't update other editors
  not fText.SelAvail and // don't update when a selection is available
  devEditor.ShowFunctionTip and // only update when option is enabled
  Assigned(fText.Highlighter) and // don't update in plaintext files
  not fFunctionTip.ForceHide; // don't update when user force hides it using ESC
end;

procedure TEditor.FunctionTipTimer(Sender: TObject);
begin
  if FunctionTipAllowed then
    fFunctionTip.Show;
end;

procedure TEditor.ExportToHTML;
var
  SynExporterHTML: TSynExporterHTML;
  SaveFileName: AnsiString;
begin
  SynExporterHTML := TSynExporterHTML.Create(nil);
  try
    with TSaveDialog.Create(Application) do try

      Filter := SynExporterHTML.DefaultFilter;
      Title := Lang[ID_NV_EXPORT];
      DefaultExt := HTML_EXT;
      FileName := ChangeFileExt(fFileName, HTML_EXT);
      Options := Options + [ofOverwritePrompt];

      if Execute then
        SaveFileName := FileName
      else
        Exit; // automatically gotos finally
    finally
      Free;
    end;

    SynExporterHTML.Title := ExtractFileName(SaveFileName);
    SynExporterHTML.CreateHTMLFragment := False;
    SynExporterHTML.ExportAsText := True;
    SynExporterHTML.UseBackground := True;
    SynExporterHTML.Font := fText.Font;
    SynExporterHTML.Highlighter := fText.Highlighter;

    SynExporterHTML.ExportAll(fText.UnCollapsedLines);
    SynExporterHTML.SaveToFile(SaveFileName);
  finally
    SynExporterHTML.Free;
  end;
end;

procedure TEditor.ExportToRTF;
var
  SynExporterRTF: TSynExporterRTF;
  SaveFileName: AnsiString;
begin
  SynExporterRTF := TSynExporterRTF.Create(nil);
  try
    with TSaveDialog.Create(Application) do try

      Filter := SynExporterRTF.DefaultFilter;
      Title := Lang[ID_NV_EXPORT];
      DefaultExt := RTF_EXT;
      FileName := ChangeFileExt(fFileName, RTF_EXT);
      Options := Options + [ofOverwritePrompt];

      if Execute then
        SaveFileName := FileName
      else
        Exit;
    finally
      Free;
    end;

    SynExporterRTF.Title := ExtractFileName(SaveFileName);
    SynExporterRTF.ExportAsText := True;
    SynExporterRTF.UseBackground := True;
    SynExporterRTF.Font := fText.Font;
    SynExporterRTF.Highlighter := fText.Highlighter;

    SynExporterRTF.ExportAll(fText.UnCollapsedLines);
    SynExporterRTF.SaveToFile(SaveFileName);
  finally
    SynExporterRTF.Free;
  end;
end;

procedure TEditor.ExportToTEX;
var
  SynExporterTEX: TSynExporterTEX;
  SaveFileName: AnsiString;
begin
  SynExporterTEX := TSynExporterTEX.Create(nil);
  try
    with TSaveDialog.Create(Application) do try
      Filter := SynExporterTEX.DefaultFilter;
      Title := Lang[ID_NV_EXPORT];
      DefaultExt := TEX_EXT;
      FileName := ChangeFileExt(fFileName, TEX_EXT);
      Options := Options + [ofOverwritePrompt];

      if Execute then
        SaveFileName := FileName
      else
        Exit;
    finally
      Free;
    end;

    SynExporterTex.Title := ExtractFileName(SaveFileName);
    SynExporterTex.ExportAsText := True;
    SynExporterTex.UseBackground := True;
    SynExporterTex.Font := fText.Font;
    SynExporterTex.Highlighter := fText.Highlighter;

    SynExporterTex.ExportAll(fText.UnCollapsedLines);
    SynExporterTex.SaveToFile(SaveFileName);
  finally
    SynExporterTEX.Free;
  end;
end;

procedure TEditor.GotoLine;
begin
  with TGotoLineForm.Create(nil) do try
    if ShowModal = mrOK then
      SetCaretPos(Line.Value, 1);
  finally
    Free;
  end;
end;

procedure TEditor.InsertString(Value: AnsiString; MoveCursor: boolean);
var
  NewCursorPos: TBufferCoord;
  Char, Line, I: integer;
  P: PAnsiChar;
begin
  // prevent lots of repaints
  fText.BeginUpdate;

  NewCursorPos := fText.CaretXY;
  if MoveCursor then begin
    P := PAnsiChar(value);
    Char := fText.CaretX;
    Line := fText.CaretY;
    I := 0;
    while P[I] <> #0 do begin

      // Assume DOS newlines
      if (P[I] = #13) and (P[I + 1] = #10) then begin
        Inc(I, 2);
        Inc(Line);
        Char := 1;
      end else if (P[I] = '*') and (P[I + 1] = '|') and (P[I + 2] = '*') then begin
        NewCursorPos.Char := Char;
        NewCursorPos.Line := Line;
        Delete(value, I + 1, 3);
        break;
      end else begin
        Inc(Char);
        Inc(I);
      end;
    end;
  end;
  fText.SelText := value;

  // Update the cursor
  fText.CaretXY := NewCursorPos;
  fText.EnsureCursorPosVisible; // not needed?

  // prevent lots of repaints
  fText.EndUpdate;
end;

procedure TEditor.SetErrorFocus(Col, Line: integer);
begin
  fIgnoreCaretChange := true;

  // Disable previous error focus
  if (fErrorLine <> -1) then begin
    fText.InvalidateGutterLine(fErrorLine);
    fText.InvalidateLine(fErrorLine);
  end;

  fErrorLine := Line;

  // Set new error focus
  SetCaretPos(fErrorLine, col, true);

  // Redraw new error line
  fText.InvalidateGutterLine(fErrorLine);
  fText.InvalidateLine(fErrorLine);
end;

procedure TEditor.GotoActiveBreakpoint;
begin
  SetCaretPos(fActiveLine, 1, true);
end;

procedure TEditor.SetActiveBreakpointFocus(Line: integer);
begin
  if Line <> fActiveLine then begin

    // Disable previous active focus
    if fActiveLine <> -1 then begin
      fText.InvalidateGutterLine(fActiveLine);
      fText.InvalidateLine(fActiveLine);
    end;

    // Put the caret at the active breakpoint
    fActiveLine := Line;
    SetCaretPos(fActiveLine, 1, true);

    // Invalidate new active line
    fText.InvalidateGutterLine(fActiveLine);
    fText.InvalidateLine(fActiveLine);
  end;
end;

procedure TEditor.RemoveBreakpointFocus;
begin
  if fActiveLine <> -1 then begin
    fText.InvalidateLine(fActiveLine);
    fText.InvalidateGutterLine(fActiveLine);
    fActiveLine := -1;
  end;
end;

procedure TEditor.UpdateCaption(const NewCaption: AnsiString);
begin
  if Assigned(fTabSheet) then begin
    if NewCaption <> fTabSheet.Caption then begin
      fTabSheet.Caption := NewCaption;
    end;
  end;
end;

procedure TEditor.SetFileName(const value: AnsiString);
begin
  if value <> fFileName then begin
    fFileName := value;
    UpdateCaption(ExtractFileName(fFileName));
  end;
end;

procedure TEditor.InsertDefaultText;
var
  tmp: TStrings;
begin
  if devEditor.DefaultCode and FileExists(devDirs.Config + DEV_DEFAULTCODE_FILE) then begin
    tmp := TStringList.Create;
    try
      tmp.LoadFromFile(devDirs.Config + DEV_DEFAULTCODE_FILE);
      InsertString(ParseMacros(tmp.Text), false);
    finally
      tmp.Free;
    end;
  end;
end;

procedure TEditor.SetCaretPos(line, col: integer; settopline: boolean = true);
var
  fold: TSynEditFoldRange;
  collapsedline: integer;
begin
  // Open up the closed folds around the focused line until we can see the line we're looking for
  repeat
    fold := fText.CollapsedFoldAroundLine(line);
    if Assigned(fold) then
      fText.Uncollapse(fold);
  until not Assigned(fold);

  collapsedline := fText.UncollapsedLineToLine(line);

  fText.CaretXY := BufferCoord(col, collapsedline);

  if settopline then
    fText.TopLine := collapsedline - 3; // leave some space above to line, is easier on the eyes

  // Changing editor focus is expensive (class browser listens to it). Don't do it if it's not needed
  if not fText.Focused then
    Activate;
end;

procedure TEditor.CompletionKeyPress(Sender: TObject; var Key: Char);
begin
  // We received a key from the completion box...
  if fCompletionBox.Enabled then begin
    if (Key in fText.IdentChars) then begin // Continue filtering
      fText.SelText := Key;
      fCompletionBox.Search(GetWordAtPosition(fText.CaretXY, wpCompletion), fFileName);
    end else if Key = Char(VK_BACK) then begin
      fText.ExecuteCommand(ecDeleteLastChar, #0, nil); // Simulate backspace in editor
      fCompletionBox.Search(GetWordAtPosition(fText.CaretXY, wpCompletion), fFileName);
    end else if Key = Char(VK_ESCAPE) then begin
      fCompletionBox.Hide;
    end else if (Key in [Char(VK_RETURN), '(']) then begin // Ending chars, don't insert
      CompletionInsert('');
      fCompletionBox.Hide;
    end else begin
      CompletionInsert(Key);
      fCompletionBox.Hide;
    end;
  end;
end;

procedure TEditor.EditorKeyPress(Sender: TObject; var Key: Char);
var
  allowcompletion: boolean;
  indent: integer;
  attr: TSynHighlighterAttributes;
  s1, s2: AnsiString;
  Ptr: PAnsiChar;
  HighlightPos: TBufferCoord;
begin

  // Doing this here instead of in EditorKeyDown to be able to delete some key messages
  if devEditor.CompleteSymbols and not fText.SelAvail then begin

    // Don't complete symbols inside strings or comments
    allowcompletion := true;

    // Empty line? Check if we're inside an uncompleted comment block, check last nonblank char
    HighlightPos := BufferCoord(fText.CaretX - 1, fText.CaretY);
    while (HighlightPos.Line > 0) and (Length(fText.Lines[HighlightPos.Line - 1]) = 0) do
      Dec(HighlightPos.Line);
    HighlightPos.Char := Length(fText.Lines[HighlightPos.Line - 1]);

    if fText.GetHighlighterAttriAtRowCol(HighlightPos, s1, attr) then
      if (attr = fText.Highlighter.StringAttribute) or (attr = fText.Highlighter.CommentAttribute) or SameStr(attr.Name,
        'Character') then
        allowcompletion := false;

    if allowcompletion then begin

      // Check if we typed anything completable...
      if (Key = '(') and devEditor.ParentheseComplete then begin
        InsertString(')', false);
        WaitForParenths := 2;

        // immediately activate function hint
        if FunctionTipAllowed then
          fFunctionTip.Activated := true;
      end else if (Key = ')') and (WaitForParenths > 0) then begin
        fText.CaretXY := BufferCoord(fText.CaretX + 1, fText.CaretY);
        WaitForParenths := 0;
        Key := #0;
      end else if (Key = '[') and devEditor.ArrayComplete then begin
        InsertString(']', false);
        WaitForArray := 2;
      end else if (Key = ']') and (WaitForArray > 0) then begin
        fText.CaretXY := BufferCoord(fText.CaretX + 1, fText.CaretY);
        WaitForArray := 0;
        Key := #0;
      end else if (Key = '*') and devEditor.CommentComplete then begin
        if (fText.CaretX > 1) and (fText.LineText[fText.CaretX - 1] = '/') then
          InsertString('*/', false);
      end else if (Key = '{') and devEditor.BraceComplete then begin

        s1 := Copy(fText.LineText, 1, fText.CaretX - 1);
        s2 := Trim(s1);

        if EndsStr(')', s2) or
          EndsStr('else', s2) or
          EndsStr('try', s2) or
          EndsStr('catch', s2) or
          EndsStr('default', s2) or
          EndsStr('do', s2) or
          StartsStr('default', s2) or
          SameStr('', s2) then begin

          // Copy indentation
          indent := 0;
          Ptr := PAnsiChar(s1);
          while Ptr^ in [#1..#32] do begin
            Inc(indent);
            Inc(Ptr);
          end;

          // { + enter + indent + }
          InsertString('{' + #13#10 + Copy(s1, 1, indent) + '}', false);
          fText.CaretXY := BufferCoord(fText.CaretX + 1, fText.CaretY);
          Key := #0;
        end else if StartsStr('struct', s2) or
          StartsStr('union', s2) or
          StartsStr('class', s2) or
          StartsStr('enum', s2) or
          StartsStr('typedef', s2) then begin

          // Copy indentation
          indent := 0;
          Ptr := PAnsiChar(s1);
          while Ptr^ in [#1..#32] do begin
            Inc(indent);
            Inc(Ptr);
          end;

          // { + enter + indent + };
          InsertString('{' + #13#10 + Copy(s1, 1, indent) + '};', false);
          fText.CaretXY := BufferCoord(fText.CaretX + 1, fText.CaretY);
          Key := #0;
        end else if StartsStr('case', s2) then begin

          // Copy indentation
          indent := 0;
          Ptr := PAnsiChar(s1);
          while Ptr^ in [#1..#32] do begin
            Inc(indent);
            Inc(Ptr);
          end;

          // { + enter + indent + tab + break; + enter + }
          InsertString('{' + #13#10 + Copy(s1, 1, indent) + #9 + 'break;' + #13#10 + Copy(s1, 1, indent) + '}', false);
          fText.CaretXY := BufferCoord(fText.CaretX + 1, fText.CaretY);
          Key := #0;
        end else begin
          WaitForCurly := 2;
          InsertString('}', false);
        end;
      end else if (Key = '}') and (WaitForCurly > 0) then begin
        fText.CaretXY := BufferCoord(fText.CaretX + 1, fText.CaretY);
        WaitForCurly := 2;
        Key := #0;
      end else if (Key = '<') and devEditor.IncludeComplete then begin
        if StartsStr('#include', fText.LineText) then
          InsertString('>', false);
      end else if (Key = '"') and devEditor.IncludeComplete then begin
        InsertString('"', false);
        WaitForDoubleQuote := 2;
      end else if (Key = '''') and devEditor.SingleQuoteComplete then begin
        InsertString('''', false);
        WaitForSingleQuote := 2;
      end else begin
        WaitForParenths := 0;
        WaitForArray := 0;
        WaitForCurly := 0;
        WaitForSingleQuote := 0;
        WaitForDoubleQuote := 0;
      end;
    end else begin
      WaitForParenths := 0;
      WaitForArray := 0;
      WaitForCurly := 0;
      WaitForSingleQuote := 0;
      WaitForDoubleQuote := 0;
    end;
  end;

  // Is code completion enabled?
  if fCompletionBox.Enabled then begin

    // Use a timer to show the completion window when we just typed a few parent-member linking chars
    case Key of
      '.': fCompletionTimer.Enabled := True;
      '>': if (fText.CaretX > 1) and (Length(fText.LineText) > 1) and (fText.LineText[fText.CaretX - 1] = '-') then
          fCompletionTimer.Enabled := True;
      ':': if (fText.CaretX > 1) and (Length(fText.LineText) > 1) and (fText.LineText[fText.CaretX - 1] = ':') then
          fCompletionTimer.Enabled := True;
    else
      fCompletionTimer.Enabled := False;
    end;
  end;
end;

procedure TEditor.EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  p: TBufferCoord;
  DeletedChar, NextChar: Char;
  S: AnsiString;
  Reason: THandPointReason;

  procedure UndoSymbolCompletion;
  begin
    if devEditor.DeleteSymbolPairs then begin
      if (devEditor.ArrayComplete and (DeletedChar = '[') and (NextChar = ']')) or
        (devEditor.ParentheseComplete and (DeletedChar = '(') and (NextChar = ')')) or
        (devEditor.BraceComplete and (DeletedChar = '{') and (NextChar = '}')) or
        (devEditor.SingleQuoteComplete and (DeletedChar = '''') and (NextChar = '''')) or
        (devEditor.DoubleQuoteComplete and (DeletedChar = '"') and (NextChar = '"')) then begin
        fText.LineText := Copy(S, 1, fText.CaretX - 1) + Copy(S, fText.CaretX + 1, MaxInt);
      end;
    end;
  end;
begin
  case (Key) of
    VK_CONTROL: begin
        Reason := HandpointAllowed(p);
        if Reason <> hprNone then
          fText.Cursor := crHandPoint
        else
          fText.Cursor := crIBeam;
      end;
    VK_ESCAPE: begin // Update function tip
        if ttoHideOnEsc in fFunctionTip.Options then begin
          fFunctionTip.ReleaseHandle;
          fFunctionTip.ForceHide := true;
        end;
      end;
    VK_DELETE: begin // remove completed character
        if not fText.SelAvail then begin
          S := fText.LineText;
          if fText.CaretX < Length(S) then begin
            DeletedChar := S[fText.CaretX];
            NextChar := S[fText.CaretX + 1];
            UndoSymbolCompletion;
          end;
        end;
      end;
    VK_BACK: begin
        if not fText.SelAvail then begin
          S := fText.LineText;
          if (fText.CaretX > 1) and (fText.CaretX <= Length(S)) then begin
            DeletedChar := S[fText.CaretX - 1];
            NextChar := S[fText.CaretX];
            UndoSymbolCompletion;
          end;
        end;
      end;
  end;
end;

procedure TEditor.EditorKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_CONTROL) then
    fText.Cursor := crIBeam;
end;

procedure TEditor.CompletionTimer(Sender: TObject);
begin
  ShowCompletion;
end;

procedure TEditor.ShowCompletion;
var
  P: TPoint;
  M: TMemoryStream;
  s: AnsiString;
  attr: TSynHighlighterAttributes;
begin
  fCompletionTimer.Enabled := False;

  // Position it at the top of the next line
  P := fText.RowColumnToPixels(fText.DisplayXY);
  Inc(P.Y, fText.LineHeight + 2);
  fCompletionBox.Position := fText.ClientToScreen(P);

  // Don't bother scanning inside strings or comments or defines
  if (fText.GetHighlighterAttriAtRowCol(BufferCoord(fText.CaretX - 1, fText.CaretY), s, attr)) then
    if (attr = fText.Highlighter.StringAttribute) or (attr = fText.Highlighter.CommentAttribute) or (attr.Name =
      'Preprocessor') then
      Exit;

  M := TMemoryStream.Create;
  try
    fText.UnCollapsedLines.SaveToStream(M);

    // Reparse whole file (not function bodies) if it has been modified
    // use stream, don't read from disk (not saved yet)
    //if fText.Modified then
    //	MainForm.CppParser.ReParseFile(fFileName,InProject,False,True,M);

    // Scan the current function body
    fCompletionBox.CurrentStatement := MainForm.CppParser.FindAndScanBlockAt(fFileName, fText.CaretY, M);
  finally
    M.Free;
  end;

  // Redirect key presses to completion box if applicable
  fCompletionBox.OnKeyPress := CompletionKeyPress;

  // Filter the whole statement list
  fCompletionBox.Search(GetWordAtPosition(fText.CaretXY, wpCompletion), fFileName);
end;

procedure TEditor.DestroyCompletion;
begin
  if Assigned(fCompletionTimer) then
    FreeAndNil(fCompletionTimer);
  if Assigned(fFunctionTipTimer) then
    FreeAndNil(fFunctionTipTimer);
end;

procedure TEditor.InitCompletion;
begin
  fCompletionBox := MainForm.CodeCompletion;
  fCompletionBox.Enabled := devCodeCompletion.Enabled;

  // This way symbols and tabs are also completed without code completion
  fText.OnKeyPress := EditorKeyPress;
  fText.OnKeyDown := EditorKeyDown;
  fText.OnKeyUp := EditorKeyUp;

  if devEditor.ShowFunctionTip then begin
    if not Assigned(fFunctionTipTimer) then
      fFunctionTipTimer := TTimer.Create(nil);
    fFunctionTipTimer.Enabled := True;
    fFunctionTipTimer.OnTimer := FunctionTipTimer;
    fFunctionTipTimer.Interval := 2 * GetCaretBlinkTime; // fancy
  end else begin
    fFunctionTip.ReleaseHandle; // hide
    FreeAndNil(fFunctionTipTimer); // remove timer, because we only have a limited number avaiable
  end;

  // The other stuff is fully completion dependant
  if fCompletionBox.Enabled then begin
    fCompletionBox.Width := devCodeCompletion.Width;
    fCompletionBox.Height := devCodeCompletion.Height;

    if not Assigned(fCompletionTimer) then
      fCompletionTimer := TTimer.Create(nil);
    fCompletionTimer.Enabled := False;
    fCompletionTimer.OnTimer := CompletionTimer;
    fCompletionTimer.Interval := devCodeCompletion.Delay;
  end else begin
    FreeAndNil(fCompletionTimer);
  end;
end;

function TEditor.GetWordAtPosition(P: TBufferCoord; Purpose: TWordPurpose): AnsiString;
var
  WordBegin, WordEnd, ParamBegin, ParamEnd, len: integer;
  s: AnsiString;
begin
  result := '';
  if (p.Line >= 1) and (p.Line <= fText.Lines.Count) then begin
    s := fText.Lines[p.Line - 1];
    len := Length(s);

    WordBegin := p.Char - 1;
    WordEnd := p.Char - 1;

    // Copy forward until end of word
    if Purpose in [wpEvaluation, wpInformation] then begin
      while (WordEnd + 1 <= len) do begin
        if (Purpose = wpEvaluation) and (s[WordEnd + 1] = '[') then begin
          if not FindComplement(s, '[', ']', WordEnd, 1) then
            break;
        end else if (s[WordEnd + 1] in fText.IdentChars) then
          Inc(WordEnd)
        else
          break;
      end;
    end;

    // Copy backward until end of word
    if Purpose in [wpCompletion, wpEvaluation, wpInformation] then begin
      while (WordBegin > 0) and (WordBegin <= len) do begin
        if (s[WordBegin] = ']') then begin
          if not FindComplement(s, ']', '[', WordBegin, -1) then
            break
          else
            Dec(WordBegin); // step over [
        end else if (s[WordBegin] in fText.IdentChars) then begin
          Dec(WordBegin);
        end else if s[WordBegin] in ['.', ':', '~'] then begin // allow destructor signs
          Dec(WordBegin);
        end else if (WordBegin > 1) and (s[WordBegin - 1] = '-') and (s[WordBegin] = '>') then begin
          Dec(WordBegin, 2);
        end else if (WordBegin > 1) and (s[WordBegin] = ')') then begin
          if not FindComplement(s, ')', '(', WordBegin, -1) then
            break
          else
            Dec(WordBegin); // step over (
        end else
          break;
      end;
    end;
  end;

  // Get end result
  Result := Copy(S, WordBegin + 1, WordEnd - WordBegin);

  // Strip function parameters
  while true do begin
    ParamBegin := Pos('(', Result);
    if ParamBegin > 0 then begin
      ParamEnd := ParamBegin;
      if FindComplement(Result, '(', ')', ParamEnd, 1) then begin
        Delete(Result, ParamBegin, ParamEnd - ParamBegin + 1);
      end else
        break;
    end else
      break;
  end;

  // Strip array stuff
  if not (Purpose = wpEvaluation) then
    while true do begin
      ParamBegin := Pos('[', Result);
      if ParamBegin > 0 then begin
        ParamEnd := ParamBegin;
        if FindComplement(Result, '[', ']', ParamEnd, 1) then begin
          Delete(Result, ParamBegin, ParamEnd - ParamBegin + 1);
        end else
          break;
      end else
        break;
    end;
end;

procedure TEditor.CompletionInsert(const append: AnsiString);
var
  Statement: PStatement;
  FuncAddOn: AnsiString;
begin
  Statement := fCompletionBox.SelectedStatement;
  if not Assigned(Statement) then
    Exit;

  // if we are inserting a function,
  if Statement^._Kind in [skFunction, skConstructor, skDestructor] then begin

    // If the argument list is already there, don't add another ()
    if (Length(fText.LineText) = 0) or (fText.LineText[fText.WordEnd.Char] <> '(') then begin
      FuncAddOn := '()';
    end else
      FuncAddOn := '';
  end else
    FuncAddOn := '';

  // delete the part of the word that's already been typed ...
  fText.SelStart := fText.RowColToCharIndex(fText.WordStart);
  fText.SelEnd := fText.RowColToCharIndex(fText.WordEnd);

  // ... by replacing the selection
  fText.SelText := Statement^._ScopelessCmd + FuncAddOn + append;

  // Move caret inside the ()'s, only when the user has something to do there...
  if (FuncAddOn <> '') and (Statement^._Args <> '()') and (Statement^._Args <> '(void)') then begin

    fText.CaretX := fText.CaretX - Length(FuncAddOn) - Length(append) + 1;

    // immediately activate function hint
    if devEditor.ShowFunctionTip and Assigned(fText.Highlighter) then begin
      fText.SetFocus;
      fFunctionTip.Show;
    end;
  end;
end;

procedure TEditor.EditorDblClick(Sender: TObject);
begin
  fDblClickTime := GetTickCount;
  fText.GetPositionOfMouse(fDblClickMousePos);
end;

procedure TEditor.EditorClick(Sender: TObject);
var
  fTripleClickTime: Cardinal;
  fTripleClickMousePos: TBufferCoord;
  fNewState: TSynStateFlags;
begin
  fTripleClickTime := GetTickCount;
  fText.GetPositionOfMouse(fTripleClickMousePos);
  if (fTripleClickTime > fDblClickTime) and
    (fTripleClickTime - GetDoubleClickTime < fDblClickTime) and
    (fTripleClickMousePos.Char = fDblClickMousePos.Char) and
    (fTripleClickMousePos.Line = fDblClickMousePos.Line) then begin

    // Don't let the editor change the caret
    fNewState := fText.StateFlags;
    Exclude(fNewState, sfWaitForDragging);
    fText.StateFlags := fNewState;

    // Select the current line
    if fText.CaretY < fText.Lines.Count then begin
      fText.BlockBegin := BufferCoord(1, fText.CaretY);
      fText.BlockEnd := BufferCoord(1, fText.CaretY + 1);
    end else begin
      fText.BlockBegin := BufferCoord(1, fText.CaretY);
      fText.BlockEnd := BufferCoord(Length(fText.Lines[fText.CaretY - 1]) + 1, fText.CaretY);
    end;
  end;
end;

procedure TEditor.EditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  s: AnsiString;
  p: TBufferCoord;
  st: PStatement;
  M: TMemoryStream;
  Reason: THandPointReason;
  IsIncludeLine: boolean;

  procedure ShowFileHint;
  var
    FileName: AnsiString;
  begin
    FileName := MainForm.CppParser.GetHeaderFileName(fFileName, s);
    if (FileName <> '') and FileExists(FileName) then
      fText.Hint := FileName + ' - Ctrl+Click for more info'
    else
      fText.Hint := '';
  end;

  procedure ShowDebugHint;
  begin
    // Add to list
    if devData.WatchHint then
      MainForm.Debugger.AddWatchVar(s);

    // Evaluate s
    fCurrentEvalWord := s; // remember name when debugger finishes
    MainForm.Debugger.OnEvalReady := OnMouseOverEvalReady;
    MainForm.Debugger.SendCommand('print', s);
  end;

  procedure ShowParserHint;
  begin
    // This piece of code changes the parser database, possibly making hints and code completion invalid...
    M := TMemoryStream.Create;
    try
      fText.UnCollapsedLines.SaveToStream(M);
      st := MainForm.CppParser.FindStatementOf(fFileName, s, p.Line, M);
    finally
      M.Free;
    end;

    if Assigned(st) then begin
      fText.Hint := MainForm.CppParser.PrettyPrintStatement(st) + ' - ' + ExtractFileName(st^._FileName) + ' (' +
        IntToStr(st^._Line) + ') - Ctrl+Click for more info';
      fText.Hint := StringReplace(fText.Hint, '|', #5, [rfReplaceAll]);
      // vertical bar is used to split up short and long hint versions...
    end;
  end;

  procedure CancelHint;
  begin
    MainForm.Debugger.OnEvalReady := nil;

    // disable editor hint
    Application.CancelHint;
    fCurrentWord := '';
    fText.Hint := '';

    // disable page control hint
    MainForm.CurrentPageHint := '';
    fTabSheet.PageControl.Hint := '';
  end;
begin

  // Leverage Ctrl-Clickability to determine if we can show any information
  Reason := HandpointAllowed(p);

  // Get subject
  IsIncludeLine := False;
  case Reason of
    // When hovering above a preprocessor line, determine if we want to show an include or a identifier hint
    hprPreprocessor: begin
        s := fText.Lines[p.Line - 1];
        IsIncludeLine := MainForm.CppParser.IsIncludeLine(s); // show filename hint
        if not IsIncludeLine then
          s := fText.GetWordAtRowCol(p);
      end;
    hprIdentifier: begin
        if MainForm.Debugger.Executing then
          s := GetWordAtPosition(p, wpEvaluation) // debugging
        else if devEditor.ParserHints and not fCompletionBox.Visible then
          s := GetWordAtPosition(p, wpInformation) // information during coding
        else
          s := '';
      end;
    hprSelection: begin
        s := fText.SelText; // when a selection is available, always only use that
      end;
    hprNone: begin
        fText.Cursor := crIBeam; // nope
        CancelHint;
      end;
  end;

  // Don't rescan the same stuff over and over again (that's slow)
  if s = fCurrentWord then
    Exit; // do NOT remove hint when subject stays the same

  // Remove hint
  CancelHint;
  fCurrentWord := s;

  // We are allowed to change the cursor
  if (ssCtrl in Shift) then
    fText.Cursor := crHandPoint
  else
    fText.Cursor := crIBeam;

  // Determine what to do with subject
  case Reason of
    hprPreprocessor: begin
        if IsIncludeLine then
          ShowFileHint
        else if devEditor.ParserHints and not fCompletionBox.Visible then
          ShowParserHint;
      end;
    hprIdentifier, hprSelection: begin
        if MainForm.Debugger.Executing then
          ShowDebugHint
        else if devEditor.ParserHints and not fCompletionBox.Visible then
          ShowParserHint;
      end;
  end;
end;

procedure TEditor.IndentSelection;
begin
  if FText.BlockBegin.Line <> FText.BlockEnd.Line then
    fText.ExecuteCommand(ecBlockIndent, #0, nil)
  else
    fText.ExecuteCommand(ecTab, #0, nil);
end;

procedure TEditor.UnindentSelection;
begin
  if fText.BlockBegin.Line <> fText.BlockEnd.Line then
    fText.ExecuteCommand(ecBlockUnIndent, #0, nil)
  else
    fText.ExecuteCommand(ecShiftTab, #0, nil);
end;

procedure TEditor.EditorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  p: TDisplayCoord;
  line, FileName: AnsiString;
  e: TEditor;
begin
  // if ctrl+clicked
  if (ssCtrl in Shift) and (Button = mbLeft) and not fText.SelAvail then begin

    p := fText.PixelsToRowColumn(X, Y);
    if P.Row <= fText.Lines.Count then begin

      // reset the cursor
      fText.Cursor := crIBeam;

      // Try to open the header
      line := Trim(fText.Lines[p.Row - 1]);
      if MainForm.CppParser.IsIncludeLine(Line) then begin
        FileName := MainForm.CppParser.GetHeaderFileName(fFileName, line);
        e := MainForm.EditorList.GetEditorFromFileName(FileName);
        if Assigned(e) then
          e.SetCaretPos(1, 1);
      end else
        MainForm.actGotoImplDeclEditorExecute(self);
    end;
  end;
end;

procedure TEditor.EditorPaintTransient(Sender: TObject; Canvas: TCanvas; TransientType: TTransientType);
const
  AllChars = ['{', '[', '(', '}', ']', ')'];
  OpenChars = ['{', '[', '('];
  CloseChars = ['}', ']', ')'];
var
  P: TBufferCoord;
  S: AnsiString;
  Pix: TPoint;
  textrect: TRect;
  Attri: TSynHighlighterAttributes;
  len: integer;

  procedure SetColors;
  var
    index: integer;
  begin
    // Draw using highlighting colors
    if TransientType = ttAfter then begin
      Canvas.Font.Color := fText.Highlighter.WhitespaceAttribute.Background; // swap colors
      Canvas.Brush.Color := Attri.Foreground;

      // Draw using normal colors
    end else begin
      index := fText.RowColToCharIndex(P, false); // TODO: only compute inside first if?
      if fText.SelAvail and (index > fText.SelStart) and (index < fText.SelEnd) then
        begin // highlighted is inside selection
        Canvas.Brush.Color := fText.SelectedColor.Background;
        Canvas.Font.Color := fText.SelectedColor.Foreground;
      end else if devEditor.HighCurrLine and (P.Line = fText.CaretY) then
        begin // matching char is inside highlighted line
        Canvas.Brush.Color := devEditor.HighColor;
        Canvas.Font.Color := Attri.Foreground;
      end else begin
        Canvas.Brush.Color := Attri.Background;
        Canvas.Font.Color := Attri.Foreground;
      end;
    end;
    if Canvas.Font.Color = clNone then
      Canvas.Font.Color := fText.Font.Color;
    if Canvas.Brush.Color = clNone then
      Canvas.Brush.Color := fText.Highlighter.WhitespaceAttribute.Background;
  end;
  procedure SetRect;
  begin
    Pix := fText.RowColumnToPixels(fText.BufferToDisplayPos(p));
    textrect.Left := Pix.X;
    textrect.Top := Pix.Y;
    textrect.Bottom := textrect.Top + fText.LineHeight;
    textrect.Right := textrect.Left + Canvas.TextWidth(S);
  end;
begin
  if (not Assigned(fText.Highlighter)) or (not devEditor.Match) then
    Exit;

  // Is there a bracket char before us?
  len := Length(fText.LineText);
  if (fText.CaretX - 1 > 0) and (fText.CaretX - 1 <= len) and (fText.LineText[fText.CaretX - 1] in AllChars) then
    P := BufferCoord(fText.CaretX - 1, fText.CaretY)

    // Or after us?
  else if (fText.CaretX > 0) and (fText.CaretX <= len) and (fText.LineText[fText.CaretX] in AllChars) then
    P := BufferCoord(fText.CaretX, fText.CaretY);

  // Is the OpenChar before/after us highlighted as a symbol (not a comment or something)?
  if fText.GetHighlighterAttriAtRowCol(P, S, Attri) and (Attri = fText.Highlighter.SymbolAttribute) then begin

    Canvas.Brush.Style := bsSolid;
    Canvas.Font.Assign(fText.Font);
    Canvas.Font.Style := Attri.Style;

    // Draw here using this color
    SetRect;
    SetColors;

    // Redraw bracket
    ExtTextOut(Canvas.Handle, Pix.X, Pix.Y, ETO_OPAQUE, @textrect, PChar(S), Length(S), nil);

    // Draw corresponding bracket too
    P := fText.GetMatchingBracketEx(P);
    if (P.Char > 0) and (P.Line > 0) then begin
      S := fText.Lines[P.Line - 1][P.Char];

      // Draw here using this color again
      SetRect;
      SetColors;

      // Mimic SynEdit drawing
      ExtTextOut(Canvas.Handle, Pix.X, Pix.Y, ETO_OPAQUE, @textrect, PChar(S), Length(S), nil);
    end;
  end;
  Canvas.Brush.Style := bsSolid;
end;

function TEditor.HandpointAllowed(var mousepos: TBufferCoord): THandPointReason;
var
  s: AnsiString;
  HLAttr: TSynHighlighterAttributes;
begin
  Result := hprNone;

  // Only allow in the text area...
  if fText.GetPositionOfMouse(mousepos) then begin

    // Only allow hand points in highlighted areas
    if fText.GetHighlighterAttriAtRowCol(mousepos, s, HLAttr) then begin

      // Only allow Identifiers, Preprocessor directives, and selection
      if Assigned(HLAttr) then begin
        if fText.SelAvail then begin
          if fText.IsPointInSelection(MousePos) then
            Result := hprSelection; // allow selection
        end else if SameStr(HLAttr.Name, 'Identifier') then
          Result := hprIdentifier // allow identifiers if no selection is present
        else if SameStr(HLAttr.Name, 'Preprocessor') then
          Result := hprPreprocessor; // and preprocessor line if no selection is present
      end;
    end;
  end;
end;

function TEditor.Save: boolean;
begin
  Result := True;

  // We will be changing files. Stop monitoring
  MainForm.FileMonitor.BeginUpdate;
  try

    // Is this file read-only?
    if FileExists(fFileName) and (FileGetAttr(fFileName) and faReadOnly <> 0) then begin

      // Yes, ask the user if he wants us to fix that
      if MessageDlg(Format(Lang[ID_MSG_FILEISREADONLY], [fFileName]), mtConfirmation, [mbYes, mbNo], 0) = mrNo then
        Exit;

      // Yes, remove read-only attribute
      if FileSetAttr(fFileName, FileGetAttr(fFileName) - faReadOnly) <> 0 then begin
        MessageDlg(Format(Lang[ID_MSG_FILEREADONLYERROR], [fFileName]), mtError, [mbOk], 0);
        Exit;
      end;
    end;

    // Filename already present? Save without dialog
    if (not fNew) and fText.Modified then begin

      // Save contents directly
      try
        fText.UnCollapsedLines.SaveToFile(fFileName);
        fText.Modified := false;
      except
        MessageDlg(Format(Lang[ID_ERR_SAVEFILE], [fFileName]), mtError, [mbOk], 0);
        Result := False;
      end;

      MainForm.CppParser.ReParseFile(fFileName, InProject);
    end else if fNew then
      Result := SaveAs; // we need a file name, use dialog

  finally
    MainForm.FileMonitor.EndUpdate;
  end;
end;

function TEditor.SaveAs: boolean;
var
  UnitIndex: integer;
  SaveFileName: AnsiString;
begin
  Result := True;
  with TSaveDialog.Create(Application) do try
    Title := Lang[ID_NV_SAVEAS];
    Filter := BuildFilter([FLT_CS, FLT_CPPS, FLT_HEADS, FLT_RES]);
    Options := Options + [ofOverwritePrompt];

    // select appropriate filter
    if GetFileTyp(fFileName) in [utcHead, utcppHead] then begin
      FilterIndex := 4; // .h
      DefaultExt := 'h';
    end else begin
      if Assigned(MainForm.Project) and fInProject then begin
        if MainForm.Project.Options.useGPP then begin
          FilterIndex := 3; // .cpp
          DefaultExt := 'cpp';
        end else begin
          FilterIndex := 2; // .c
          DefaultExt := 'c';
        end;
      end else begin
        FilterIndex := 3; // .cpp
        DefaultExt := 'cpp';
      end;
    end;

    // Set save box options
    FileName := fFileName;
    if (fFileName <> '') then
      InitialDir := ExtractFilePath(fFileName)
    else if Assigned(MainForm.Project) then
      InitialDir := MainForm.Project.Directory;

    // Open the save box
    if Execute then
      SaveFileName := FileName // prevent collision between TEditor.FileName and Dialog.FileName
    else begin
      Result := False;
      Exit;
    end;
  finally
    Free;
  end;

  // Remove *old* file from statement list
  MainForm.CppParser.InvalidateFile(FileName);

  // Try to save to disk
  try
    fText.UnCollapsedLines.SaveToFile(SaveFileName);
    fText.Modified := False;
    fNew := False;
  except
    MessageDlg(Lang[ID_ERR_SAVEFILE] + '"' + SaveFileName + '"', mtError, [mbOk], 0);
    Result := False;
  end;

  // Update project information
  if Assigned(MainForm.Project) and Self.InProject then begin
    UnitIndex := MainForm.Project.Units.IndexOf(FileName); // index of *old* filename
    if UnitIndex <> -1 then
      MainForm.Project.SaveUnitAs(UnitIndex, SaveFileName); // save as new filename
  end else
    fTabSheet.Caption := ExtractFileName(SaveFileName);

  // Update window captions
  MainForm.UpdateAppTitle;

  // Update class browser, redraw once
  MainForm.ClassBrowser.BeginUpdate;
  try
    MainForm.CppParser.ReParseFile(SaveFileName, InProject);
    MainForm.ClassBrowser.CurrentFile := SaveFileName;
  finally
    MainForm.ClassBrowser.EndUpdate;
  end;

  // Set new file name
  FileName := SaveFileName;
end;

end.

