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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, CodeCompletion, CppParser, SynExportTeX,
  SynEditExport, SynExportRTF, Menus, ImgList, ComCtrls, StdCtrls, ExtCtrls, SynEdit, SynEditKeyCmds, version,
  SynEditCodeFolding, SynExportHTML, SynEditTextBuffer, Math, StrUtils, SynEditTypes, SynEditHighlighter, DateUtils,
  CodeToolTip, CBUtils, System.UITypes, System.Contnrs, SynEditPrint;

type
  TCloseTabSheet = class(TTabSheet)
  private
  protected
  public
    FCloseButtonRect: TRect;
    FOnClose: TNotifyEvent;
    procedure DoClose; virtual;
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
  end;

  TCustomSynEditHelper = class helper for TCustomSynEdit
  private
    function GetStateFlags: TSynStateFlags;
    procedure SetStateFlags(AStateFlags: TSynStateFlags);
    function GetPlugins: TObjectList;
    function GetStatusChanges: TSynStatusChanges;
    procedure SetStatusChanges(const Value: TSynStatusChanges);
    function GetInternalfBlockBegin: TBufferCoord;
    procedure SetInternalfBlockBegin(const Value: TBufferCoord);
    function GetInternalfBlockEnd: TBufferCoord;
    procedure SetInternalfBlockEnd(const Value: TBufferCoord);
  public
    property StateFlags: TSynStateFlags read GetStateFlags write SetStateFlags;
    function InternalLeftSpacesEx(const Line: string; WantTabs: Boolean; CalcAlways : Boolean = False): Integer;
    property Plugins: TObjectList read GetPlugins;
    property StatusChanges: TSynStatusChanges read GetStatusChanges write SetStatusChanges;
    property InternalfBlockBegin: TBufferCoord read GetInternalfBlockBegin write SetInternalfBlockBegin;
    property InternalfBlockEnd: TBufferCoord read GetInternalfBlockEnd write SetInternalfBlockEnd;
  end;

  TSynEditEx = class(TSynEdit)
  public const
    ecMoveSelDown = ecUserFirst - 1;
    ecMoveSelUp = ecUserFirst - 2;
    ecDuplicateLine = ecUserFirst - 3;
    ecComment = ecUserFirst - 4;
    ecUncomment = ecUserFirst - 5;
    ecToggleComment = ecUserFirst - 6;
    ecCommentInline = ecUserFirst - 7;
  private
    FeoAddIndent: Boolean;
    procedure InternalDoLinesDeleted(FirstLine, Count: integer);
    procedure InternalDoLinesInserted(FirstLine, Count: integer);
    procedure DoComment;
    procedure DoUnComment;

  //code folding
    // Utility functions
    function HlGetLineRange(Lines: TStrings; Line : Integer) : Pointer;
    function HlGetHighlighterAttriAtRowCol(const Lines : TStrings;
      const Line: Integer; const Char: Integer): TSynHighlighterAttributes;
    function HlGetHighlighterAttriAtRowColEx(const Lines : TStrings;
      const Line, Char: Integer;  var Token: string;
      var TokenType, Start: Integer; var Attri: TSynHighlighterAttributes): boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetCaretXYCentered(ForceToMiddle: Boolean; const Value: TBufferCoord);
    procedure ExecuteCommand(Command: TSynEditorCommand; AChar: WideChar; Data: pointer); override;
    procedure CommandProcessor(Command: TSynEditorCommand; AChar: Char; Data: Pointer); override;
    property eoAddIndent: Boolean read FeoAddIndent write FeoAddIndent;

    procedure DoScanForFoldRanges(Sender: TObject; FoldRanges: TSynFoldRanges; LinesToScan: TStrings; FromLine : Integer; ToLine : Integer);
  end;

  TSynEditPrintHelper = class helper for TSynEditPrint
  public
    procedure PrintEx;
  end;


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

  // Define what we want to skip
  TSymbolCompleteState = (
    scsSkipped, // skipped over completed symbol
    scsInserted, // opening symbol has been inserted
    scsFinished // keystroke that triggered insertion has completed
    );

  // Define how to handle international characters, when the Save or SaveAs called
  TSymbolCorrectionEncoding = (
    sceUserDefined, // to ask the user, if found international symbol, else remains of the original encoding
    sceNever,       // never check Encoding, save as is
    sceAuto,        // automatically correct without dialog, if found, else remains of the original encoding
    sceFixedUTF8    // always set UTF8, without checking
    );

  TEditor = class(TObject)
  private
    fInProject: boolean;
    fFileName: String;
    fNew: boolean;
    fText: TSynEditEx;
    fTabSheet: TTabSheet;
    fErrorLine: integer;
    fActiveLine: integer;
    fDebugGutter: TDebugGutter;
    fCurrentWord: String;
    fCurrentEvalWord: String;
    fIgnoreCaretChange: boolean;
    fPreviousEditors: TList;
    fDblClickTime: Cardinal;
    fDblClickMousePos: TBufferCoord;
    fCompletionTimer: TTimer;
    fCompletionBox: TCodeCompletion;
    fCompletionInitialPosition: TBufferCoord;
    fFunctionTipTimer: TTimer;
    fFunctionTip: TCodeToolTip;
    fParenthCompleteState: TSymbolCompleteState;
    fArrayCompleteState: TSymbolCompleteState;
    fBraceCompleteState: TSymbolCompleteState;
    fEncodingCorrection: TSymbolCorrectionEncoding;
    //fSingleQuoteCompleteState: TSymbolCompleteState;
    //fDoubleQuoteCompleteState: TSymbolCompleteState;
    procedure EditorKeyPress(Sender: TObject; var Key: Char);
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditorKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditorDblClick(Sender: TObject);
    procedure EditorClick(Sender: TObject);
    procedure EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure EditorReplaceText(Sender: TObject; const aSearch, aReplace: String; Line, Column: integer; var Action:
      TSynReplaceAction);
    procedure EditorDropFiles(Sender: TObject; x, y: integer; aFiles: TStrings);
    procedure EditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure EditorExit(Sender: TObject);
    procedure EditorGutterClick(Sender: TObject; Button: TMouseButton; x, y, Line: integer; mark: TSynEditMark);
    procedure EditorSpecialLineColors(Sender: TObject; Line: integer; var Special: boolean; var FG, BG: TColor);
    procedure EditorPaintTransient(Sender: TObject; Canvas: TCanvas; TransientType: TTransientType);
    procedure EditorEnter(Sender: TObject);
    procedure CompletionKeyPress(Sender: TObject; var Key: Char);
    procedure CompletionInsert(const append: String);
    procedure CompletionTimer(Sender: TObject);
    function FunctionTipAllowed: boolean;
    procedure FunctionTipTimer(Sender: TObject);
    procedure HandleSymbolCompletion(var Key: Char);
    procedure HandleCodeCompletion(var Key: Char);
    function HandpointAllowed(var MousePos: TBufferCoord; ShiftState: TShiftState): THandPointReason;
    procedure SetFileName(const value: String);
    procedure OnMouseOverEvalReady(const evalvalue: String);
    function HasBreakPoint(Line: integer): integer;
    procedure DebugAfterPaint(ACanvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer);
    function GetPageControl: TPageControl;
    procedure SetPageControl(Value: TPageControl);
    procedure UpdateEncoding(const FileName: string);
  public
    constructor Create(const Filename: String; InProject, NewFile: boolean; ParentPageControl: TPageControl);
    destructor Destroy; override;
    function Save: boolean;
    function SaveAs: boolean;
    procedure Activate;
    procedure GotoLine;
    procedure SetCaretPosAndActivate(Line, Col: integer); // needs to activate in order to place cursor properly
    procedure ExportToHTML;
    procedure ExportToRTF;
    procedure ExportToTEX;
    procedure InsertString(Value: String; MoveCursor: boolean);
    procedure SetErrorFocus(Col, Line: integer);
    procedure GotoActiveBreakpoint;
    procedure SetActiveBreakpointFocus(Line: integer);
    procedure RemoveBreakpointFocus;
    procedure UpdateCaption(const NewCaption: String);
    procedure InsertDefaultText;
    procedure ToggleBreakPoint(Line: integer);
    function GetWordAtPosition(P: TBufferCoord; Purpose: TWordPurpose): String;
    procedure IndentSelection;
    procedure UnindentSelection;
    procedure InitCompletion;
    procedure ShowCompletion;
    procedure DestroyCompletion;
    property PreviousEditors: TList read fPreviousEditors;
    property FileName: String read fFileName write SetFileName;
    property InProject: boolean read fInProject write fInProject;
    property New: boolean read fNew write fNew;
    property Text: TSynEditEx read fText write fText;
    property TabSheet: TTabSheet read fTabSheet write fTabSheet;
    property FunctionTip: TCodeToolTip read fFunctionTip;
    property CompletionBox: TCodeCompletion read fCompletionBox;
    property PageControl: TPageControl read GetPageControl write SetPageControl;
    property EncodingCorrection: TSymbolCorrectionEncoding read fEncodingCorrection write fEncodingCorrection;
  end;

implementation

uses
  main, project, MultiLangSupport, devcfg, utils,
  DataFrm, GotoLineFrm, Macros, debugreader, IncrementalFrm,
  CodeCompletionForm, SynEditMiscClasses, CharUtils, Vcl.Printers, SynEditPrintTypes;

{ TCloseTabSheet }
constructor TCloseTabSheet.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FCloseButtonRect:=Rect(125, 0, 150, 50);
end;

destructor TCloseTabSheet.Destroy;
begin
  inherited Destroy;
end;

procedure TCloseTabSheet.DoClose;
begin
  if Assigned(FOnClose) then FOnClose(Self);
  Free;
end;

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

type
  TSynEditPluginInternal = class(TSynEditPlugin)
  end;

constructor TEditor.Create(const Filename: String; InProject, NewFile: boolean; ParentPageControl: TPageControl);
var
  s: String;
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
  fTabSheet := TCloseTabSheet.Create(ParentPageControl);
  fTabSheet.Caption := ExtractFileName(fFilename); // UntitlexX or main.cpp
  fTabSheet.PageControl := ParentPageControl;
  fTabSheet.Tag := integer(Self); // Define an index for each tab
  fTabSheet.TabVisible := True;
  //fTabSheet.OnClose := MainForm.CloseTabProc;

  // Create an editor and set static options
  fText := TSynEditEx.Create(fTabSheet);

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
  begin
    // Initialize Lines
    var EmptyStream := TStringStream.Create('');
    fText.Lines.LoadFromStream(EmptyStream);
    EmptyStream.Free;

    fNew := True;
  end;

  // Set constant options
  fText.Parent := fTabSheet;
  fText.Visible := True;
  fText.Align := alClient;
  fText.PopupMenu := MainForm.EditorPopup;
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
  fText.OnKeyPress := EditorKeyPress;
  fText.OnKeyDown := EditorKeyDown;
  fText.OnKeyUp := EditorKeyUp;
  fText.OnPaintTransient := EditorPaintTransient;

  fText.StyleName := '';
  fText.ParentFont := False;

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
  // Deactivate the file change monitor
  MainForm.FileMonitor.UnMonitor(fFileName);

  // Delete breakpoints in this editor
  MainForm.Debugger.DeleteBreakPointsOf(self);

  // Destroy code completion stuff
  DestroyCompletion;

  // Free everything
  FreeAndNil(fFunctionTip);
  FreeAndNil(fText);
  FreeAndNil(fTabSheet);
  FreeAndNil(fPreviousEditors);

  // Move into TObject.Destroy...
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

procedure TEditor.OnMouseOverEvalReady(const evalvalue: String);
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
  thisbreakpoint: integer;
begin
  thisbreakpoint := HasBreakPoint(Line);

  if thisbreakpoint <> -1 then
    MainForm.Debugger.RemoveBreakPoint(Line, self)
  else
    MainForm.Debugger.AddBreakPoint(Line, self);

  // Convert buffer to display position
  fText.InvalidateGutterLine(Line);
  fText.InvalidateLine(Line);
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
begin
  if (Line = fActiveLine) then begin
    StrtoPoint(pt, devEditor.Syntax.Values[cABP]);
    BG := pt.X;
    FG := pt.Y;
    Special := TRUE;
  end else if (HasBreakpoint(Line) <> -1) then begin
    StrtoPoint(pt, devEditor.Syntax.Values[cBP]);
    BG := pt.X;
    FG := pt.Y;
    Special := TRUE;
  end else if Line = fErrorLine then begin
    StrtoPoint(pt, devEditor.Syntax.Values[cErr]);
    BG := pt.X;
    FG := pt.Y;
    Special := TRUE;
  end;
end;

procedure TEditor.DebugAfterPaint(ACanvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer);
var
  X, Y, I, Line: integer;
begin
  // Get point where to draw marks
  X := (fText.Gutter.RealGutterWidth(fText.CharWidth) - fText.Gutter.RightOffset) div 2 - 3;
  Y := (fText.LineHeight - dmMain.GutterImages.Height) div 2 + fText.LineHeight * (FirstLine - fText.TopLine);

  // The provided lines are actually rows...
  for I := FirstLine to LastLine do begin
    Line := fText.RowToLine(I);
    if fActiveLine = Line then // prefer active line over breakpoints
      dmMain.GutterImages.Draw(ACanvas, X, Y, 1)
    else if HasBreakpoint(Line) <> -1 then
      dmMain.GutterImages.Draw(ACanvas, X, Y, 0)
    else if fErrorLine = Line then
      dmMain.GutterImages.Draw(ACanvas, X, Y, 2);

    Inc(Y, fText.LineHeight);
  end;
end;

procedure TEditor.EditorDropFiles(Sender: TObject; x, y: integer; aFiles: TStrings);
var
  sl: TStringList;
  I: integer;
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
    // Open list of provided files
  end else begin
    MainForm.OpenFileList(TStringList(aFiles));
  end;
end;

procedure TEditor.EditorReplaceText(Sender: TObject; const aSearch, aReplace: String; Line, Column: integer; var
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

// Handling this here instead of in PageControlChange because when switching between two active editors side by side
// PageControlChange will not happen, but the editor will be entered and this will be executed

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
      UpdateCaption('[*] ' + ExtractFileName(fFileName));
    end else begin
      UpdateCaption(ExtractFileName(fFileName));
    end;
  end;

  // scSelection includes anything caret related
  if scSelection in Changes then begin
    MainForm.SetStatusbarLineCol;

    // Finish symbol completion
    if fParenthCompleteState = scsInserted then
      fParenthCompleteState := scsFinished;
    if fArrayCompleteState = scsInserted then
      fArrayCompleteState := scsFinished;
    if fBraceCompleteState = scsInserted then
      fBraceCompleteState := scsFinished;

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
  SaveFileName: String;
begin
  SynExporterHTML := TSynExporterHTML.Create(nil);
  try
    with TSaveDialog.Create(nil) do try
      Filter := SynExporterHTML.DefaultFilter;
      Title := Lang[ID_NV_EXPORT];
      DefaultExt := HTML_EXT;
      Options := Options + [ofOverwritePrompt];
      FileName := ChangeFileExt(ExtractFileName(fFileName), HTML_EXT);
      InitialDir := ExtractFilePath(fFileName);


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

    SynExporterHTML.ExportAll(fText.Lines);
    SynExporterHTML.SaveToFile(SaveFileName);
  finally
    SynExporterHTML.Free;
  end;
end;

procedure TEditor.ExportToRTF;
var
  SynExporterRTF: TSynExporterRTF;
  SaveFileName: String;
begin
  SynExporterRTF := TSynExporterRTF.Create(nil);
  try
    with TSaveDialog.Create(nil) do try
      Filter := SynExporterRTF.DefaultFilter;
      Title := Lang[ID_NV_EXPORT];
      DefaultExt := RTF_EXT;
      FileName := ChangeFileExt(ExtractFileName(fFileName), RTF_EXT);
      InitialDir := ExtractFilePath(fFileName);
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

    SynExporterRTF.ExportAll(fText.Lines);
    SynExporterRTF.SaveToFile(SaveFileName);
  finally
    SynExporterRTF.Free;
  end;
end;

procedure TEditor.ExportToTEX;
var
  SynExporterTEX: TSynExporterTEX;
  SaveFileName: String;
begin
  SynExporterTEX := TSynExporterTEX.Create(nil);
  try
    with TSaveDialog.Create(nil) do try
      Filter := SynExporterTEX.DefaultFilter;
      Title := Lang[ID_NV_EXPORT];
      DefaultExt := TEX_EXT;
      FileName := ChangeFileExt(ExtractFileName(fFileName), TEX_EXT);
      InitialDir := ExtractFilePath(fFileName);
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

    SynExporterTex.ExportAll(fText.Lines);
    SynExporterTex.SaveToFile(SaveFileName);
  finally
    SynExporterTEX.Free;
  end;
end;

procedure TEditor.GotoLine;
begin
  with TGotoLineForm.Create(nil) do try
    if ShowModal = mrOK then
      SetCaretPosAndActivate(Line.Value, 1);
  finally
    Free;
  end;
end;

procedure TEditor.InsertString(Value: String; MoveCursor: boolean);
var
  NewCursorPos: TBufferCoord;
  Char, Line, I: integer;
  P: PChar;
begin
  // prevent lots of repaints
  fText.BeginUpdate;
  try
    NewCursorPos := fText.CaretXY;
    if MoveCursor then begin
      P := PChar(value);
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

    // prevent lots of repaints
  finally
    fText.EndUpdate;
  end;
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
  SetCaretPosAndActivate(fErrorLine, col);

  // Redraw new error line
  fText.InvalidateGutterLine(fErrorLine);
  fText.InvalidateLine(fErrorLine);
end;

procedure TEditor.GotoActiveBreakpoint;
begin
  if fActiveLine <> -1 then
    SetCaretPosAndActivate(fActiveLine, 1);
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
    SetCaretPosAndActivate(fActiveLine, 1);

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

procedure TEditor.UpdateCaption(const NewCaption: String);
begin
  if Assigned(fTabSheet) then begin
    if NewCaption <> fTabSheet.Caption then begin
      fTabSheet.Caption := NewCaption;
    end;
  end;
end;

type
  TSynEditStringListEx = class(TSynEditStringList);

// similar functions "IsWideCharMappableToAnsi, IsUnicodeStringMappableToAnsi)
// are placed in the module SynUnicode.pas. But those functions, without WC_NO_BEST_FIT_CHARS flag,
// return incorrect results sometimes
function _IsWideCharMappableToAnsi(const WC: WideChar): Boolean;
var
  UsedDefaultChar: BOOL;
begin
  WideCharToMultiByte(DefaultSystemCodePage, WC_NO_BEST_FIT_CHARS, PWideChar(@WC), 1, nil, 0, nil,
    @UsedDefaultChar);
  Result := not UsedDefaultChar;
end;

function _IsUnicodeStringMappableToAnsi(const WS: UnicodeString): Boolean;
var
  UsedDefaultChar: BOOL;
begin
  WideCharToMultiByte(DefaultSystemCodePage, WC_NO_BEST_FIT_CHARS, PWideChar(WS), Length(WS), nil, 0,
    nil, @UsedDefaultChar);
  Result := not UsedDefaultChar;
end;


procedure TEditor.UpdateEncoding(const FileName: string);
var
  lines: TSynEditStringListEx;
  i: Integer;
begin
  lines := TSynEditStringListEx(fText.Lines);

  if (fEncodingCorrection =  sceNever)
      or not fText.Modified
      or (lines.Encoding.CodePage = CP_UTF8) then exit;

  if fEncodingCorrection = sceFixedUTF8 then
    begin
      lines.SetEncoding(TEncoding.UTF8);
      exit;
    end;

  for I := 0 to Lines.Count-1 do
    if _IsUnicodeStringMappableToAnsi(lines[i]) then
      begin
        if (fEncodingCorrection = sceAuto)
            or (MessageDlg(Format(Lang[ID_MSG_FILESAVEENCODING], [FileName]),
                                mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
          lines.SetEncoding(TEncoding.UTF8);
        break;
      end;
end;

procedure TEditor.SetFileName(const value: String);
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

function TCustomSynEditHelper.GetInternalfBlockBegin: TBufferCoord;
begin
  with Self do Result := fBlockBegin;
end;

function TCustomSynEditHelper.GetInternalfBlockEnd: TBufferCoord;
begin
  with Self do Result := fBlockEnd;
end;

function TCustomSynEditHelper.GetPlugins: TObjectList;
begin
  with Self do Result := fPlugins;
end;

function TCustomSynEditHelper.GetStateFlags: TSynStateFlags;
begin
  with Self do Result := fStateFlags;
end;

function TCustomSynEditHelper.GetStatusChanges: TSynStatusChanges;
begin
  with Self do Result := fStatusChanges;
end;

procedure TCustomSynEditHelper.SetInternalfBlockBegin(const Value: TBufferCoord);
begin
  with Self do fBlockBegin := Value;
end;

procedure TCustomSynEditHelper.SetInternalfBlockEnd(const Value: TBufferCoord);
begin
  with Self do fBlockEnd := Value;
end;

procedure TCustomSynEditHelper.SetStateFlags(AStateFlags: TSynStateFlags);
begin
  with Self do fStateFlags := AStateFlags;
end;

procedure TCustomSynEditHelper.SetStatusChanges(const Value: TSynStatusChanges);
begin
  with Self do fStatusChanges := Value;
end;

function TCustomSynEditHelper.InternalLeftSpacesEx(const Line: string; WantTabs: Boolean; CalcAlways : Boolean = False): Integer;
begin
  with Self do Result := LeftSpacesEx(Line, WantTabs, CalcAlways);  // Access strict protected property
end;

procedure TEditor.SetCaretPosAndActivate(Line, Col: integer);
begin
  // Open up the closed folds around the focused line until we can see the line we're looking for
  fText.UncollapseAroundLine(Line);

  // fText needs to be focused for TopLine and LinesInWindow to be correct
  if not fText.Focused then
    Self.Activate;

  // Position the caret, call EnsureCursorPosVisibleEx after setting block
  fText.SetCaretXYCentered(True,BufferCoord(Col, Line));
end;

procedure TEditor.CompletionKeyPress(Sender: TObject; var Key: Char);
begin
  // We received a key from the completion box...
  if fCompletionBox.Enabled then begin
    if fText.IsIdentChar(Key) then begin // Continue filtering
      fText.SelText := Key;
      fCompletionBox.Search(GetWordAtPosition(fText.CaretXY, wpCompletion), fFileName);
    end else if Key = Char(VK_BACK) then begin
      fText.ExecuteCommand(ecDeleteLastChar, #0, nil); // Simulate backspace in editor
      fCompletionBox.Search(GetWordAtPosition(fText.CaretXY, wpCompletion), fFileName);
    end else if Key = Char(VK_ESCAPE) then begin
      fCompletionBox.Hide;
    end else if CharInSet(Key, [Char(VK_RETURN), '(']) then begin // Ending chars, don't insert
      CompletionInsert('');
      fCompletionBox.Hide;
    end else begin
      CompletionInsert(Key);
      fCompletionBox.Hide;
    end;
  end;
end;

procedure TEditor.HandleSymbolCompletion(var Key: Char);
var
  Attr: TSynHighlighterAttributes;
  Token: String;
  HighlightPos: TBufferCoord;

  procedure HandleParentheseCompletion;
  begin
    InsertString(')', false);
    if FunctionTipAllowed then
      fFunctionTip.Activated := true;
    fParenthCompleteState := scsInserted;
  end;

  procedure HandleParentheseSkip;
  begin
    if fParenthCompleteState = scsFinished then begin
      fText.CaretXY := BufferCoord(fText.CaretX + 1, fText.CaretY); // skip over
      fParenthCompleteState := scsSkipped;
      Key := #0; // remove key press
    end;
  end;

  procedure HandleArrayCompletion;
  begin
    InsertString(']', false);
    fArrayCompleteState := scsInserted;
  end;

  procedure HandleArraySkip;
  begin
    if fArrayCompleteState = scsFinished then begin
      fText.CaretXY := BufferCoord(fText.CaretX + 1, fText.CaretY); // skip over
      fArrayCompleteState := scsSkipped;
      Key := #0; // remove key press
    end;
  end;

  procedure HandleMultilineCommentCompletion;
  begin
    if (fText.CaretX > 1) and (fText.LineText[fText.CaretX - 1] = '/') then
      InsertString('*/', false);
  end;

  procedure HandleBraceCompletion;
  var
    LineBeforeCaret, Indent, MoreIndent: String;
    IndentCount, TabCount: integer;
  begin
    // Determine what word is before us
    LineBeforeCaret := Trim(Copy(fText.LineText, 1, fText.CaretX - 1));

    // Determine current indent
    IndentCount := fText.InternalLeftSpacesEx(fText.LineText, True);

    // Get indentation string
    if eoTabsToSpaces in fText.Options then
      Indent := System.StringOfChar(#32, IndentCount)
    else begin
      // Use tabs as much as possible, add remaining indent using spaces
      TabCount := IndentCount div fText.TabWidth;
      Indent := System.StringOfChar(#9, TabCount) + System.StringOfChar(#32, IndentCount - fText.TabWidth * TabCount);
    end;

    // For case, do the following:
    //{ + enter + indent + tab + break; + enter + }
    if StartsStr('case', LineBeforeCaret) or
      StartsStr('default', LineBeforeCaret) then begin

      // Get extra indentation string
      if eoTabsToSpaces in fText.Options then
        MoreIndent := System.StringOfChar(#32, IndentCount + fText.TabWidth)
      else
        MoreIndent := System.StringOfChar(#9, (IndentCount + fText.TabWidth) div fText.TabWidth);

      InsertString('{' + #13#10 + MoreIndent + 'break;' + #13#10 + Indent + '}', false);
      fText.CaretXY := BufferCoord(fText.CaretX + 1, fText.CaretY);
      Key := #0; // cancel original key insertion

      // For other valid block starts, do the following:
      // { + enter + indent + }
    end else if EndsStr(')', LineBeforeCaret) or
      SameStr('', LineBeforeCaret) or
      EndsStr('else', LineBeforeCaret) or
      EndsStr('try', LineBeforeCaret) or
      EndsStr('catch', LineBeforeCaret) or
      StartsStr('namespace', LineBeforeCaret) or
      EndsStr('do', LineBeforeCaret) then begin

      InsertString('{' + #13#10 + Indent + '}', false);
      fText.CaretXY := BufferCoord(fText.CaretX + 1, fText.CaretY);
      Key := #0; // cancel original key insertion

      // For structs and derivatives, do the following:
      // { + enter + indent + }; <-- SAME EXCEPT FOR THE SEMICOLON
    end else if
      StartsStr('struct', LineBeforeCaret) or
      StartsStr('union', LineBeforeCaret) or
      StartsStr('class', LineBeforeCaret) or
      StartsStr('enum', LineBeforeCaret) or
      StartsStr('typedef ', LineBeforeCaret) then begin

      InsertString('{' + #13#10 + Indent + '};', false);
      fText.CaretXY := BufferCoord(fText.CaretX + 1, fText.CaretY);
      Key := #0; // cancel original key insertion
    end else begin
      InsertString('}', false);
      fBraceCompleteState := scsInserted;
    end;
  end;

  procedure HandleBraceSkip;
  begin
    if fBraceCompleteState = scsFinished then begin
      fText.CaretXY := BufferCoord(fText.CaretX + 1, fText.CaretY); // skip over
      fBraceCompleteState := scsSkipped;
      Key := #0; // remove key press
    end;
  end;

  procedure HandleLocalIncludeCompletion;
  begin
    if StartsStr('#include', fText.LineText) then begin
      InsertString('"', false);
      fText.CaretXY := BufferCoord(fText.CaretX + 1, fText.CaretY); // skip over
      Key := #0;
    end;
  end;

  procedure HandleGlobalIncludeCompletion;
  begin
    if StartsStr('#include', fText.LineText) then
      InsertString('>', false);
  end;

  procedure HandleSingleQuoteCompletion;
  begin
    InsertString('''', false);
    //fSingleQuoteCompleteState := scsTyped;
  end;

  procedure HandleDoubleQuoteCompletion;
  begin
    InsertString('"', false);
    //fDoubleQuoteCompleteState := scsTyped;
  end;

begin
  if not devEditor.CompleteSymbols or fText.SelAvail then
    Exit;

  // Find the end of the first nonblank line above us
  HighlightPos := BufferCoord(fText.CaretX - 1, fText.CaretY);
  while (HighlightPos.Line > 0) and (Length(fText.Lines[HighlightPos.Line - 1]) = 0) do
    Dec(HighlightPos.Line);
  HighlightPos.Char := Length(fText.Lines[HighlightPos.Line - 1]);

  // Check if that line is highlighted as string or character or comment
  if fText.GetHighlighterAttriAtRowCol(HighlightPos, Token, Attr) then
    if (Attr = fText.Highlighter.StringAttribute) or (Attr = fText.Highlighter.CommentAttribute) or SameStr(Attr.Name,
      'Character') then
      Exit;

  case Key of
    '(': begin
        if devEditor.ParentheseComplete then
          HandleParentheseCompletion;
      end;
    ')': begin
        if devEditor.ParentheseComplete then
          HandleParentheseSkip;
      end;
    '[': begin
        if devEditor.ArrayComplete then
          HandleArrayCompletion;
      end;
    ']': begin
        if devEditor.ParentheseComplete then
          HandleArraySkip;
      end;
    '*': begin
        if devEditor.CommentComplete then
          HandleMultilineCommentCompletion;
      end;
    '{': begin
        if devEditor.BraceComplete then
          HandleBraceCompletion;
      end;
    '}': begin
        if devEditor.BraceComplete then
          HandleBraceSkip;
      end;
    '<': begin
        if devEditor.IncludeComplete then // include <>
          HandleGlobalIncludeCompletion;
      end;
    '''': begin
        if devEditor.SingleQuoteComplete then // characters
          HandleSingleQuoteCompletion;
      end;
    '"': begin
        if devEditor.IncludeComplete then // include ""
          HandleLocalIncludeCompletion;
        if devEditor.DoubleQuoteComplete then // strings
          HandleDoubleQuoteCompletion;
      end;
  else begin
      fParenthCompleteState := scsSkipped;
      fArrayCompleteState := scsSkipped;
      fBraceCompleteState := scsSkipped;
      //fSingleQuoteCompleteState := scsSkipped;
      //fDoubleQuoteCompleteState := scsSkipped;
    end;
  end;
end;

procedure TEditor.HandleCodeCompletion(var Key: Char);
begin
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

    // Stop code completion timer if the cursor moves
    fCompletionInitialPosition := BufferCoord(fText.CaretX + 1, fText.CaretY);
  end;
end;

procedure TEditor.EditorKeyPress(Sender: TObject; var Key: Char);
begin
  // Don't offer completion functions for plain text files
  if not Assigned(fText.Highlighter) then
    Exit;

  // Doing this here instead of in EditorKeyDown to be able to delete some key messages
  HandleSymbolCompletion(Key);

  // Spawn code completion window if we are allowed to
  HandleCodeCompletion(Key);
end;

procedure TEditor.EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  p: TBufferCoord;
  DeletedChar, NextChar: Char;
  S: String;
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
  // Don't offer completion functions for plain text files
  if not Assigned(fText.Highlighter) then
    Exit;

  // See if we can undo what has been inserted by HandleSymbolCompletion
  case (Key) of
    VK_CONTROL: begin
        Reason := HandpointAllowed(p, Shift);
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
    VK_BACK: begin // remove completed character
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
  // Don't show the completion box if the cursor has moved during the timer rundown
  if (fText.CaretX <> fCompletionInitialPosition.Char) or
    (fText.CaretY <> fCompletionInitialPosition.Line) then
    Exit;

  ShowCompletion;
end;

procedure TEditor.InitCompletion;
begin
  fCompletionBox := MainForm.CodeCompletion;
  fCompletionBox.Enabled := devCodeCompletion.Enabled;

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

procedure TEditor.ShowCompletion;
var
  P: TPoint;
  M: TMemoryStream;
  s: String;
  attr: TSynHighlighterAttributes;
begin
  fCompletionTimer.Enabled := False;

  // Position it at the top of the next line
  P := fText.RowColumnToPixels(fText.DisplayXY);
  Inc(P.Y, fText.LineHeight + 2);
  fCompletionBox.Position := fText.ClientToScreen(P);

  // Only scan when cursor is placed after a symbol, inside a word, or inside whitespace
  if (fText.GetHighlighterAttriAtRowCol(BufferCoord(fText.CaretX - 1, fText.CaretY), s, attr)) then
    if (attr <> fText.Highlighter.SymbolAttribute) and
      (attr <> fText.Highlighter.WhitespaceAttribute) and
      (attr <> fText.Highlighter.IdentifierAttribute) then
      Exit;

  M := TMemoryStream.Create;
  try
    fText.Lines.SaveToStream(M);

    // Reparse whole file (not function bodies) if it has been modified
    // use stream, don't read from disk (not saved yet)
    if fText.Modified then begin
      MainForm.CppParser.ParseFile(fFileName, InProject, False, True, M);
    end;

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
  FreeAndNil(fCompletionTimer);
  FreeAndNil(fFunctionTipTimer);
end;

function TEditor.GetWordAtPosition(P: TBufferCoord; Purpose: TWordPurpose): String;
var
  WordBegin, WordEnd, ParamBegin, ParamEnd, len: integer;
  s: String;
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
        end else if fText.IsIdentChar(s[WordEnd + 1]) then
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
        end else if fText.IsIdentChar(s[WordBegin]) then begin
          Dec(WordBegin);
        end else if CharInSet(s[WordBegin], ['.', ':', '~']) then begin // allow destructor signs
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

procedure TEditor.CompletionInsert(const append: String);
var
  Statement: PStatement;
  FuncAddOn: String;
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
  fText.SelText := Statement^._Command + FuncAddOn + append;

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
begin
  fTripleClickTime := GetTickCount;
  fText.GetPositionOfMouse(fTripleClickMousePos);
  if (fTripleClickTime > fDblClickTime) and
    (fTripleClickTime - GetDoubleClickTime < fDblClickTime) and
    (fTripleClickMousePos.Char = fDblClickMousePos.Char) and
    (fTripleClickMousePos.Line = fDblClickMousePos.Line) then begin

    // Don't let the editor change the caret
    fText.StateFlags := fText.StateFlags - [sfWaitForDragging];

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
  s: String;
  p: TBufferCoord;
  st: PStatement;
  M: TMemoryStream;
  Reason: THandPointReason;
  IsIncludeLine: boolean;

  procedure ShowFileHint;
  var
    FileName: String;
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
      fText.Lines.SaveToStream(M);
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
  Reason := HandpointAllowed(p, Shift);

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
  line, FileName: String;
  e: TEditor;
begin
  // if ctrl+clicked
  if (ssCtrl in Shift) and (Button = mbLeft) and not fText.SelAvail then begin

    p := fText.PixelsToRowColumn(X, Y);
    if P.Row <= fText.Lines.Count then begin

      // reset the cursor
      fText.Cursor := crIBeam;

      // Try to open the header
      line := fText.Lines[p.Row - 1];
      if MainForm.CppParser.IsIncludeLine(Line) then begin
        FileName := MainForm.CppParser.GetHeaderFileName(fFileName, line);
        e := MainForm.EditorList.GetEditorFromFileName(FileName);
        if Assigned(e) then begin
          e.SetCaretPosAndActivate(1, 1);
        end;
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
  HighlightCharPos: TBufferCoord;
  ComplementCharPos: TBufferCoord;
  Pix: TPoint;
  S: String;
  Attri: TSynHighlighterAttributes;
  LineLength: integer;
  OutIndex: Integer;

  procedure SetColors(Point: TBufferCoord);
  begin
    // Draw using highlighting colors
    if TransientType = ttAfter then begin
      Canvas.Font.Color := fText.Highlighter.WhitespaceAttribute.Background; // swap colors
      Canvas.Brush.Color := Attri.Foreground;

      // Draw using normal colors
    end else begin
      if devEditor.HighCurrLine and (Point.Line = fText.CaretY) then begin // matching char is inside highlighted line
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

begin
  // Don't bother wasting time when we don't have to
  if not Assigned(fText.Highlighter) then // no highlighted file is viewed
    Exit;
  if not devEditor.Match then // user has disabled match painting
    Exit;
  if fText.SelAvail then // not clear cut what to paint
    Exit;
  //Exit; // greatly reduces flicker
  HighlightCharPos.Line := -1;

  // Is there a bracket char before us?
  LineLength := Length(fText.LineText);
  if (fText.CaretX - 1 > 0) and (fText.CaretX - 1 <= LineLength) and CharInSet(fText.LineText[fText.CaretX - 1], AllChars) then
    HighlightCharPos := BufferCoord(fText.CaretX - 1, fText.CaretY)

    // Or after us?
  else if (fText.CaretX > 0) and (fText.CaretX <= LineLength) and CharInSet(fText.LineText[fText.CaretX], AllChars) then
    HighlightCharPos := BufferCoord(fText.CaretX, fText.CaretY);

  // Character not found. Exit.
  if HighlightCharPos.Line = -1 then
    Exit;

  // Is the OpenChar before/after us highlighted as a symbol (not a comment or something)?
  if not (fText.GetHighlighterAttriAtRowCol(HighlightCharPos, S, Attri) and (Attri = fText.Highlighter.SymbolAttribute))
    then
    Exit;

  // Find the corresponding bracket
  ComplementCharPos := fText.GetMatchingBracketEx(HighlightCharPos);
  if (ComplementCharPos.Char = 0) and (ComplementCharPos.Line = 0) then
    Exit;

  // At this point we have found both characters. Check if both are visible
  if fText.AllFoldRanges.FoldHidesLine(HighlightCharPos.Line, OutIndex) or
    fText.AllFoldRanges.FoldHidesLine(ComplementCharPos.Line, OutIndex) then
    Exit;

  // Both are visible. Draw them
  // First, draw bracket where caret is placed next to the caret
  Canvas.Brush.Style := bsSolid;
  Canvas.Font.Assign(fText.Font);
  Canvas.Font.Style := Attri.Style;

  // Draw the character the caret is at here using this color
  SetColors(HighlightCharPos);
  Pix := fText.RowColumnToPixels(fText.BufferToDisplayPos(HighlightCharPos));

  if Pix.X > fText.Gutter.Width then begin // only draw if inside viewable area
    S := fText.Lines[HighlightCharPos.Line - 1][HighlightCharPos.Char];
    Canvas.TextOut(Pix.X, Pix.Y, S);
  end;

  // Then draw complement
  SetColors(ComplementCharPos);
  Pix := fText.RowColumnToPixels(fText.BufferToDisplayPos(ComplementCharPos));

  if Pix.X > fText.Gutter.Width then begin // only draw if inside viewable area
    S := fText.Lines[ComplementCharPos.Line - 1][ComplementCharPos.Char];
    Canvas.TextOut(Pix.X, Pix.Y, S);
  end;

  // Reset brush
  Canvas.Brush.Style := bsSolid;
end;

function TEditor.HandpointAllowed(var MousePos: TBufferCoord; ShiftState: TShiftState): THandPointReason;
var
  s: String;
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
          // do not allow when dragging selection
          if fText.IsPointInSelection(MousePos) and not (ssLeft in ShiftState) then
            Result := hprSelection; // allow selection
        end else if HLAttr.Name = 'Identifier' then
          Result := hprIdentifier // allow identifiers if no selection is present
        else if HLAttr.Name = 'Preprocessor' then
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
        UpdateEncoding(fFileName);
        fText.Lines.SaveToFile(fFileName);
        fText.Modified := false;
      except
        MessageDlg(Format(Lang[ID_ERR_SAVEFILE], [fFileName]), mtError, [mbOk], 0);
        Result := False;
      end;

      MainForm.CppParser.ParseFile(fFileName, InProject);
    end else if fNew then
      Result := SaveAs; // we need a file name, use dialog

  finally
    MainForm.FileMonitor.EndUpdate;
  end;
end;

function TEditor.SaveAs: boolean;
var
  UnitIndex: integer;
  SaveFileName: String;
begin
  Result := True;
  with TSaveDialog.Create(nil) do try
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
    FileName := ExtractFileName(fFileName);
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
    UpdateEncoding(fFileName);
    fText.Lines.SaveToFile(SaveFileName);
    fText.Modified := False;
    fNew := False;
  except
    MessageDlg(Format(Lang[ID_ERR_SAVEFILE], [SaveFileName]), mtError, [mbOk], 0);
    Result := False;
  end;

  // Update highlighter
  devEditor.AssignEditor(fText, SaveFileName);

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
    MainForm.CppParser.ParseFile(SaveFileName, InProject);
    MainForm.ClassBrowser.CurrentFile := SaveFileName;
  finally
    MainForm.ClassBrowser.EndUpdate;
  end;

  // Set new file name
  FileName := SaveFileName;
end;

{ TSynEditEx }

procedure TSynEditEx.CommandProcessor(Command: TSynEditorCommand; AChar: Char; Data: Pointer);
begin
  if FeoAddIndent and (Command = ecLineBreak) and (not ReadOnly)  then
  begin
    var Temp := LineText;
    var SpaceCount2 := 0;
    var BackCounter := CaretY;
    if eoAutoIndent in Options then begin
      repeat
        Dec(BackCounter);
        Temp := Lines[BackCounter];
        SpaceCount2 := InternalLeftSpacesEx(Temp, False);
      until (BackCounter = 0) or (Temp <> '');
    end;

    inherited;

    var Attr: TSynHighlighterAttributes;
    if GetHighlighterAttriAtRowCol(BufferCoord(Length(Temp), CaretY - 1), Temp, Attr) then begin // only add indent to source files
      if Attr <> Highlighter.CommentAttribute then begin // and outside of comments
        if CharInSet(Temp[Length(Temp)], ['{', ':']) then begin // add more indent for these too
          if not (eoTabsToSpaces in Options) then begin
            Lines[CaretY - 1] := Lines[CaretY - 1] + #9;
            Inc(SpaceCount2, 1);
          end else begin
            Lines[CaretY - 1] := Lines[CaretY - 1] + StringOfChar(' ', TabWidth);
            Inc(SpaceCount2, TabWidth); // update caret counter
          end;
        end;
      end;
    end;
    InternalCaretXY := BufferCoord(SpaceCount2 + 1, CaretY);
    Exit;
  end;

  if (Command = ecChar) and FeoAddIndent and CharInSet(AChar, ['}']) then
            // Remove TabWidth of indent of the current line when typing a }
    if TrimLeft(Lines[CaretY - 1]) = '' then begin
    // and the first nonblank char is this new }
      var i := CaretX - 1;
      var SpaceCount1 := 0; // buffer character count
      var SpaceCount2 := 0; // display character count
      while (i > 0) and (i <= Length(Lines[CaretY - 1])) and (SpaceCount2 < TabWidth) do begin
        if Lines[CaretY - 1][i] = #9 then begin
          Inc(SpaceCount1);
          Inc(SpaceCount2, TabWidth);
        end else if Lines[CaretY - 1][i] = #32 then begin
          Inc(SpaceCount1);
          Inc(SpaceCount2);
        end;
        Dec(i);
      end;
      Lines[CaretY - 1] := Copy(Lines[CaretY - 1], 1, i);
      InternalCaretXY := BufferCoord(CaretX - SpaceCount1, CaretY);
    end;

  inherited;
end;

constructor TSynEditEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FeoAddIndent := False;
end;

procedure TSynEditEx.DoComment;
var
  OrigBlockBegin, OrigBlockEnd, OrigCaret: TBufferCoord;
  EndLine, I: integer;
begin
  if not ReadOnly then begin
    DoOnPaintTransient(ttBefore);
    UndoList.BeginBlock;
    try
      OrigBlockBegin := BlockBegin;
      OrigBlockEnd := BlockEnd;
      OrigCaret := CaretXY;

      // Ignore the last line the cursor is placed on
      if OrigBlockEnd.Char = 1 then
        EndLine := max(OrigBlockBegin.Line - 1, OrigBlockEnd.Line - 2)
      else
        EndLine := OrigBlockEnd.Line - 1;

      for I := OrigBlockBegin.Line - 1 to EndLine do begin
        Lines[i] := '//' + Lines[i];
        UndoList.AddChange(crInsert,
          BufferCoord(1, i + 1),
          BufferCoord(3, i + 1),
          '', smNormal);
      end;

      // When grouping similar commands, process one comment action per undo/redo
      UndoList.AddChange(crNothing, BufferCoord(0, 0), BufferCoord(0, 0), '', smNormal);
    finally
      UndoList.EndBlock;
    end;

    // Move begin of selection
    if OrigBlockBegin.Char > 1 then
      Inc(OrigBlockBegin.Char, 2);

    // Move end of selection
    if OrigBlockEnd.Char > 1 then
      Inc(OrigBlockEnd.Char, 2);

    // Move caret
    if OrigCaret.Char > 1 then
      Inc(OrigCaret.Char, 2);

    SetCaretAndSelection(OrigCaret, OrigBlockBegin, OrigBlockEnd);
  end;
end;

procedure TSynEditEx.DoUnComment;
var
  OrigBlockBegin, OrigBlockEnd, OrigCaret: TBufferCoord;
  EndLine, I, J: integer;
  S: string;
begin
  if not ReadOnly then begin
    DoOnPaintTransient(ttBefore);
    UndoList.BeginBlock;
    try
      OrigBlockBegin := BlockBegin;
      OrigBlockEnd := BlockEnd;
      OrigCaret := CaretXY;

      // Ignore the last line the cursor is placed on
      if OrigBlockEnd.Char = 1 then
        EndLine := max(OrigBlockBegin.Line - 1, OrigBlockEnd.Line - 2)
      else
        EndLine := OrigBlockEnd.Line - 1;

      for I := OrigBlockBegin.Line - 1 to EndLine do begin
        S := Lines[i];
        // Find // after blanks only
        J := 1;
        while (J + 1 <= length(S)) and CharInSet(S[j], [#0..#32]) do
          Inc(J);
        if (j + 1 <= length(S)) and (S[j] = '/') and (S[j + 1] = '/') then begin
          Delete(S, J, 2);
          Lines[i] := S;

          UndoList.AddChange(crDelete,
            BufferCoord(J, i + 1),
            BufferCoord(J + 2, i + 1),
            '//', smNormal);

          // Move begin of selection
          if (I = OrigBlockBegin.Line - 1) and (OrigBlockBegin.Char > 1) then
            Dec(OrigBlockBegin.Char, 2);

          // Move end of selection
          if (I = OrigBlockEnd.Line - 1) and (OrigBlockEnd.Char > 1) then
            Dec(OrigBlockEnd.Char, 2);

          // Move caret
          if (I = OrigCaret.Line - 1) and (OrigCaret.Char > 1) then
            Dec(OrigCaret.Char, 2);
        end;
      end;

      // When grouping similar commands, process one uncomment action per undo/redo
      UndoList.AddChange(crNothing, BufferCoord(0, 0), BufferCoord(0, 0), '', smNormal);

      CaretXY := OrigCaret;
      BlockBegin := OrigBlockBegin;
      BlockEnd := OrigBlockEnd;
    finally
      UndoList.EndBlock;
    end;
  end;
end;

procedure TSynEditEx.ExecuteCommand(Command: TSynEditorCommand; AChar: WideChar; Data: pointer);
begin
  case Command of
    ecMoveSelDown:
      if not ReadOnly and (Lines.Count > 0) and (BlockEnd.Line < Lines.Count) then begin
        DoOnPaintTransient(ttBefore);

        // Backup caret and selection
        var OrigBlockBegin := BlockBegin;
        var OrigBlockEnd := BlockEnd;

        // Delete line below selection
        var s := Lines[OrigBlockEnd.Line]; // after end, 0 based
        Lines.Delete(OrigBlockEnd.Line); // after end, 0 based
        InternalDoLinesDeleted(OrigBlockEnd.Line, 1); // before start, 1 based

        // Insert line above selection
        Lines.Insert(OrigBlockBegin.Line - 1, S);
        InternalDoLinesInserted(OrigBlockBegin.Line, 1);

        // Restore caret and selection
        SetCaretAndSelection(
          BufferCoord(CaretX, CaretY + 1),
          BufferCoord(1, OrigBlockBegin.Line + 1),
          BufferCoord(Length(Lines[OrigBlockEnd.Line]) + 1, OrigBlockEnd.Line + 1));

        // Retrieve start of line we moved down
        var MoveDelim := BufferCoord(1, OrigBlockBegin.Line);

        // Support undo, implement as drag and drop
        UndoList.BeginBlock;
        try
          UndoList.AddChange(crSelection,
            OrigBlockBegin,
            OrigBlockEnd,
            '',
            smNormal);
          UndoList.AddChange(crDragDropInsert,
            MoveDelim, // put at start of line me moved down
            BlockEnd, // modified
            SelText + #13#10 + S,
            smNormal);
        finally
          UndoList.EndBlock;
        end;

        DoOnPaintTransient(ttAfter);
      end;

    ecMoveSelUp:
      if not ReadOnly and (Lines.Count > 0) and (BlockBegin.Line > 1) then begin
        DoOnPaintTransient(ttBefore);

        // Backup caret and selection
        var OrigBlockBegin := BlockBegin;
        var OrigBlockEnd := BlockEnd;

        // Delete line above selection
        var s := Lines[OrigBlockBegin.Line - 2]; // before start, 0 based
        Lines.Delete(OrigBlockBegin.Line - 2); // before start, 0 based
        InternalDoLinesDeleted(OrigBlockBegin.Line - 1, 1); // before start, 1 based

        // Insert line below selection
        Lines.Insert(OrigBlockEnd.Line - 1, S);
        InternalDoLinesInserted(OrigBlockEnd.Line, 1);

        // Restore caret and selection
        SetCaretAndSelection(
          BufferCoord(CaretX, CaretY - 1),
          BufferCoord(1, OrigBlockBegin.Line - 1), // put start of selection at start of top line
          BufferCoord(Length(Lines[OrigBlockEnd.Line - 2]) + 1, OrigBlockEnd.Line - 1));
        // put end of selection at end of top line

        // Retrieve end of line we moved up
        var MoveDelim := BufferCoord(Length(Lines[OrigBlockEnd.Line - 1]) + 1, OrigBlockEnd.Line);

        // Support undo, implement as drag and drop
        UndoList.BeginBlock;
        try
          // backup original selection
          UndoList.AddChange(crSelection, OrigBlockBegin, OrigBlockEnd, '', smNormal);
          UndoList.AddChange(crDragDropInsert,
            BlockBegin, // modified
            MoveDelim, // put at end of line me moved up
            S + #13#10 + SelText,
            smNormal);
        finally
          UndoList.EndBlock;
        end;

        DoOnPaintTransient(ttAfter);
      end;

    ecDuplicateLine:
      if not ReadOnly and (Lines.Count > 0) then begin
        DoOnPaintTransient(ttBefore);
        Lines.Insert(CaretY, Lines[CaretY - 1]);
        InternalDoLinesInserted(CaretY + 1, 1);
        UndoList.AddChange(crLineBreak, CaretXY, CaretXY, '', smNormal);
        InternalCaretXY := BufferCoord(1, CaretY); // like seen in the Delphi editor
        DoOnPaintTransient(ttAfter);
      end;
    ecComment: DoComment;
    ecUncomment: DoUnComment;
    ecToggleComment:
      if not ReadOnly then begin
        var OrigBlockBegin := BlockBegin;
        var OrigBlockEnd := BlockEnd;

        // Ignore the last line the cursor is placed on
        var BeginIndex := BlockBegin.Line - 1;
        var EndIndex: Integer;
        if BlockEnd.Char = 1 then
          EndIndex := max(0, BlockEnd.Line - 2)
        else
          EndIndex := BlockEnd.Line - 1;

        // if everything is commented, then uncomment
        for var I := BeginIndex to EndIndex do begin
          if Pos('//', TrimLeft(Lines[i])) <> 1 then begin // not fully commented
            DoComment; // comment everything
            Exit;
          end;
        end;
        DoUncomment;
      end;

      ecCommentInline: // toggle inline comment
        if not ReadOnly and SelAvail then begin
          var Temp := SelText;

          // Check if the selection starts with /* after blanks
          var StartPos := -1;
          var I := 1;
          while I <= Length(Temp) do begin
            if CharInSet(Temp[I], [#9, #32]) then
              Inc(I)
            else if ((I + 1) <= Length(Temp)) and (Temp[i] = '/') and (Temp[i + 1] = '*') then begin
              StartPos := I;
              break;
            end else
              break;
          end;

          // Check if the selection ends with /* after blanks
          var EndPos := -1;
          if StartPos <> -1 then begin
            I := Length(Temp);
            while I > 0 do begin
              if CharInSet(Temp[I], [#9, #32]) then
                Dec(I)
              else if ((I - 1) > 0) and (Temp[i] = '/') and (Temp[i - 1] = '*') then begin
                EndPos := I;
                break;
              end else
                break;
            end;
          end;

          // Keep selection
          var OrigBlockBegin := BlockBegin;
          var OrigBlockEnd := BlockEnd;

          // Toggle based on current comment status
          if (StartPos <> -1) and (EndPos <> -1) then begin
            SelText := Copy(SelText, StartPos + 2, EndPos - StartPos - 3);
            BlockBegin := OrigBlockBegin;
            BlockEnd := BufferCoord(OrigBlockEnd.Char - 4, OrigBlockEnd.Line);
          end else begin
            SelText := '/*' + SelText + '*/';
            BlockBegin := BufferCoord(OrigBlockBegin.Char, OrigBlockBegin.Line);
            BlockEnd := BufferCoord(OrigBlockEnd.Char + 4, OrigBlockEnd.Line);
          end;
        end;
  else
    inherited;
  end;
end;

procedure TSynEditEx.InternalDoLinesDeleted(FirstLine, Count: integer);
var
  i: Integer;
begin
  // gutter marks
  for i := 0 to Marks.Count - 1 do
    if Marks[i].Line >= FirstLine + Count then
      Marks[i].Line := Marks[i].Line - Count
    else if Marks[i].Line > FirstLine then
      Marks[i].Line := FirstLine;

  // plugins
  if Plugins <> nil then
    for i := 0 to Plugins.Count - 1 do
      TSynEditPluginInternal(Plugins[i]).LinesDeleted(FirstLine, Count);
end;

procedure TSynEditEx.InternalDoLinesInserted(FirstLine, Count: integer);
var
  i: Integer;
begin
  // gutter marks
  for i := 0 to Marks.Count - 1 do
    if Marks[i].Line >= FirstLine then
      Marks[i].Line := Marks[i].Line + Count;

  // plugins
  if Plugins <> nil then
    for i := 0 to Plugins.Count - 1 do
      TSynEditPluginInternal(Plugins[i]).LinesInserted(FirstLine, Count);
end;

procedure TSynEditEx.SetCaretXYCentered(ForceToMiddle: Boolean; const Value: TBufferCoord);
begin
  IncPaintLock;
  try
    StatusChanges := StatusChanges + [scSelection];
    if ForceToMiddle then
      SetCaretXYEx(False, Value) // do not call EnsureCursorPosVisible here
    else
      SetCaretXYEx(True, Value);
    if SelAvail then
      InvalidateSelection;

    var NewBufferCoord: TBufferCoord;
    NewBufferCoord.Char := CaretX;
    NewBufferCoord.Line := CaretY;

    InternalfBlockBegin := NewBufferCoord;
    InternalfBlockEnd := NewBufferCoord;

    if ForceToMiddle then
      EnsureCursorPosVisibleEx(True); // but here after block has been set
  finally
    DecPaintLock;
  end;
end;

function TSynEditEx.HlGetHighlighterAttriAtRowCol(const Lines: TStrings; const Line, Char: Integer): TSynHighlighterAttributes;
var
  Token: string;
  TokenType, Start: Integer;
begin
  HlGetHighlighterAttriAtRowColEx(Lines, Line, Char, Token, TokenType, Start, Result);
end;

function TSynEditEx.HlGetHighlighterAttriAtRowColEx(const Lines: TStrings; const Line, Char: Integer; var Token: string; var TokenType, Start: Integer; var Attri: TSynHighlighterAttributes): boolean;
var
  LineText: string;
begin
  if not Assigned(Highlighter) then Exit(False);

  if  (Line >= 0) and (Line < Lines.Count) then
  begin
    LineText := Lines[Line];
    if Line = 0 then
      Highlighter.ResetRange
    else
      Highlighter.SetRange(TSynEditStringList(Lines).Ranges[Line - 1]);
    Highlighter.SetLine(LineText, Line);
    if (Char > 0) and (Char <= Length(LineText)) then
      while not Highlighter.GetEol do
      begin
        Start := Highlighter.GetTokenPos + 1;
        Token := Highlighter.GetToken;
        if (Char >= Start) and (Char < Start + Length(Token)) then
        begin
          Attri := Highlighter.GetTokenAttribute;
          TokenType := Highlighter.GetTokenKind;
          Result := True;
          exit;
        end;
        Highlighter.Next;
      end;
  end;
  Token := '';
  Attri := nil;
  Result := False;
end;

function TSynEditEx.HlGetLineRange(Lines: TStrings; Line: Integer): Pointer;
begin
  if (Line >= 0) and (Line < Lines.Count) then
    Result := TSynEditStringList(Lines).Ranges[Line]
  else
    Result := nil;
end;

procedure TSynEditEx.DoScanForFoldRanges(Sender: TObject; FoldRanges: TSynFoldRanges; LinesToScan: TStrings; FromLine, ToLine: Integer);
var
  CurLine: String;
  Line: Integer;

  function LineHasChar(Line: Integer; character: char;
  StartCol : Integer): boolean; // faster than Pos!
  var
    i: Integer;
  begin
    result := false;
    for I := StartCol to Length(CurLine) do begin
      if CurLine[i] = character then begin
        // Char must have proper highlighting (ignore stuff inside comments...)
        if HlGetHighlighterAttriAtRowCol(LinesToScan, Line, I) <> Highlighter.CommentAttribute then begin
          result := true;
          break;
        end;
      end;
    end;
  end;

  function FindBraces(Line: Integer) : Boolean;
  Var
    Col : Integer;
  begin
    Result := False;

    for Col := 1 to Length(CurLine) do
    begin
      // We've found a starting character
      if CurLine[col] = '{' then
      begin
        // Char must have proper highlighting (ignore stuff inside comments...)
        if HlGetHighlighterAttriAtRowCol(LinesToScan, Line, Col) <> Highlighter.CommentAttribute then
        begin
          // And ignore lines with both opening and closing chars in them
          if not LineHasChar(Line, '}', col + 1) then begin
            FoldRanges.StartFoldRange(Line + 1, 1);
            Result := True;
          end;
          // Skip until a newline
          break;
        end;
      end else if CurLine[col] = '}' then
      begin
        if HlGetHighlighterAttriAtRowCol(LinesToScan, Line, Col) <> Highlighter.CommentAttribute then
        begin
          // And ignore lines with both opening and closing chars in them
          if not LineHasChar(Line, '{', col + 1) then begin
            FoldRanges.StopFoldRange(Line + 1, 1);
            Result := True;
          end;
          // Skip until a newline
          break;
        end;
      end;
    end; // for Col
  end;

  function FoldRegion(Line: Integer): Boolean;
  Var
    S : string;
  begin
    Result := False;
    S := TrimLeft(CurLine);
    if Uppercase(Copy(S, 1, 9)) = '//#REGION' then
    begin
      FoldRanges.StartFoldRange(Line + 1, FoldRegionType);
      Result := True;
    end
    else if Uppercase(Copy(S, 1, 12)) = '//#ENDREGION' then
    begin
      FoldRanges.StopFoldRange(Line + 1, FoldRegionType);
      Result := True;
    end;
  end;

begin
   if not Assigned(Highlighter) then Exit;

  for Line := FromLine to ToLine do
  begin
    // Deal first with Multiline comments (Fold Type 2)
    if Integer(HlGetLineRange(LinesToScan, Line)) = 1 then
    begin
      if Integer(HlGetLineRange(LinesToScan, Line - 1)) <> 1 then
        FoldRanges.StartFoldRange(Line + 1, 2)
      else
        FoldRanges.NoFoldInfo(Line + 1);
      Continue;
    end
    else if Integer(HlGetLineRange(LinesToScan, Line - 1)) = 1 then
    begin
      FoldRanges.StopFoldRange(Line + 1, 2);
      Continue;
    end;

    CurLine := LinesToScan[Line];

    // Skip empty lines
    if CurLine = '' then begin
      FoldRanges.NoFoldInfo(Line + 1);
      Continue;
    end;

    // Find Fold regions
    if FoldRegion(Line) then
      Continue;

    // Find an braces on this line  (Fold Type 1)
    if not FindBraces(Line) then
      FoldRanges.NoFoldInfo(Line + 1);
  end; // while Line
end;


{ TSynEditPrintHelper }

procedure TSynEditPrintHelper.PrintEx;
var
  i, ii: Integer;
begin
  with Self do
  begin
    if fSelectedOnly and not fSelAvail then
      exit;

    FPrinting := True;
    FAbort := False;
    // The next part sets the document title that is used by the printer queue.
    if FDocTitle <> '' then
      Printer.Title := FDocTitle
    else
      Printer.Title := FTitle;
    Printer.BeginDoc;
    if not Printer.Printing then Exit;
    try

      PrintStatus(psBegin, 1, FAbort);
      UpdatePages(Printer.Canvas);

      var EndPage := FPageCount;
      for ii:=1 to Copies do
      begin
        i := 1;
        while (i <= EndPage) and (not FAbort) do begin
          PrintPage(i);
          if ((i < EndPage) or (ii<Copies)) and not FAbort then
            Printer.NewPage;
          i := i + 1;
        end;
      end;
      if not FAbort then
        PrintStatus(psEnd, EndPage, FAbort);
    finally
      Printer.EndDoc;
      FPrinting := False;
    end;
  end;
end;

end.

