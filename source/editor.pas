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

unit editor;

interface

uses 
{$IFDEF WIN32}
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, CodeCompletion, CppParser,
  Menus, ImgList, ComCtrls, StdCtrls, ExtCtrls, SynEdit, SynEditKeyCmds, version, SynEditCodeFolding,
  SynCompletionProposal, SynEditTextBuffer, Math, StrUtils, SynEditTypes, SynEditHighlighter, CodeToolTip, SynAutoIndent;
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
    fEditor : TEditor;
    procedure AfterPaint(ACanvas: TCanvas; const AClip: TRect;FirstLine, LastLine: integer); override;
    procedure LinesInserted(FirstLine, Count: integer); override;
    procedure LinesDeleted(FirstLine, Count: integer); override;
  public
    constructor Create(ed : TEditor);
  end;

  // RNC no longer uses an Editor to toggle
  TBreakpointToggleEvent = procedure (index: integer; BreakExists: boolean) of object;

  TEditor = class
   private
    fInProject: boolean;
    fFileName: AnsiString;
    fNew: boolean;
    fRes: boolean;
    fText: TSynEdit;
    fTabSheet: TTabSheet;
    fErrorLine: integer;
    fActiveLine: integer;
    fErrSetting: boolean;
    fDebugGutter: TDebugGutter;
    fOnBreakPointToggle: TBreakpointToggleEvent;
    fCurrentWord: AnsiString;
    fCompletionEatSpace: boolean;
    fCompletionTimer: TTimer;
    fCompletionTimerKey: Char;
    fCompletionBox: TCodeCompletion;
    fRunToCursorLine: integer;
    fFunctionTipTimer : TTimer;
    fFunctionTip: TCodeToolTip;
    fMouseOverTimer : TTimer;
    fAllowMouseOver : boolean;
    fLastPos : TBufferCoord;
    fAutoIndent: TSynAutoIndent;
    HasCompletedParentheses : integer; // Set to 2 on completion during KeyPress, to 1 immediately after by KeyDown, and to 0 upon next key
    HasCompletedArray : integer; // ...
    HasCompletedCurly : integer; // ...

    procedure EditorKeyPress(Sender: TObject; var Key: Char);
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditorKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure EditorSpecialLineColors(Sender: TObject; Line: integer;var Special: boolean; var FG, BG: TColor);
    procedure EditorGutterClick(Sender: TObject; Button: TMouseButton;x, y, Line: integer; mark: TSynEditMark);
    procedure EditorReplaceText(Sender: TObject;const aSearch, aReplace: AnsiString; Line, Column: integer;var Action: TSynReplaceAction);
    procedure EditorDropFiles(Sender: TObject; x, y: integer;aFiles: TStrings);
    procedure EditorDblClick(Sender: TObject);
    procedure EditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure EditorExit(Sender : TObject);

    procedure MouseOverTimer(Sender : TObject);

    procedure SetEditorText(Key: Char);
    function CurrentPhrase: AnsiString;
    procedure CompletionTimer(Sender: TObject);

    procedure TurnOffBreakpoint(line: integer);
    procedure TurnOnBreakpoint(line: integer);

    function FunctionTipAllowed : boolean;
    procedure FunctionTipTimer(Sender : TObject);

    procedure SetFileName(const value: AnsiString);
    procedure DrawGutterImages(ACanvas: TCanvas; AClip: TRect;FirstLine, LastLine: integer);
    procedure EditorPaintTransient(Sender: TObject; Canvas: TCanvas; TransientType: TTransientType);
   public
    procedure Init(InProject : boolean;const Caption, Filename : AnsiString;DoOpen : boolean;IsRes: boolean = FALSE);
    destructor Destroy; override;

    procedure SetBreakPointsOnOpen;
    function HasBreakPoint(line_number: integer): integer;
    procedure InsertBreakpoint(line: integer);
    procedure RemoveBreakpoint(line: integer);

    procedure Activate;
    function ToggleBreakPoint(Line: integer): boolean;
    procedure RunToCursor(Line : integer);
    procedure GotoLine;
    procedure GotoLineNr(Nr: integer);
    function Search(isReplace: boolean): boolean;
    procedure SearchAgain;
    procedure Exportto(filetype: integer);
    procedure InsertString(Value: AnsiString;MoveCursor: boolean);
    procedure SetErrorFocus(Col, Line: integer);
    procedure SetActiveBreakpointFocus(Line: integer);
    procedure RemoveBreakpointFocus;
    procedure UpdateCaption(const NewCaption: AnsiString);
    procedure InsertDefaultText;
    procedure PaintMatchingBrackets(TransientType: TTransientType);

    procedure CommentSelection;
    procedure UncommentSelection;
    procedure IndentSelection;
    procedure UnindentSelection;

    procedure UpdateParser; // Must be called after recreating the parser
    procedure InitCompletion;
    procedure DestroyCompletion;

    property OnBreakpointToggle: TBreakpointToggleEvent read fOnBreakpointToggle write fOnBreakpointToggle;
    property FileName: AnsiString read fFileName write SetFileName;
    property InProject: boolean read fInProject write fInProject;
    property New: boolean read fNew write fNew;
    property IsRes: boolean read fRes write fRes;
    property Text: TSynEdit read fText write fText;
    property TabSheet: TTabSheet read fTabSheet write fTabSheet;
    property FunctionTip: TCodeToolTip read fFunctionTip;
  end;

implementation

uses
{$IFDEF WIN32}
  main, project, MultiLangSupport, devcfg, Search_Center, utils,
  datamod, GotoLineFrm, Macros, SynEditMiscClasses;
{$ENDIF}
{$IFDEF LINUX}
  Xlib, main, project, MultiLangSupport, devcfg, Search_Center, utils,
  datamod, GotoLineFrm, Macros;
{$ENDIF}

{ TDebugGutter }

constructor TDebugGutter.Create(ed : TEditor);
begin
  inherited Create(ed.Text);
  fEditor := ed;
end;

procedure TDebugGutter.AfterPaint(ACanvas: TCanvas; const AClip: TRect;FirstLine, LastLine: integer);
begin
	fEditor.DrawGutterImages(ACanvas, AClip, FirstLine, LastLine);
end;

procedure TDebugGutter.LinesInserted(FirstLine, Count: integer);
begin
// if this method is not defined -> Abstract error
end;

procedure TDebugGutter.LinesDeleted(FirstLine, Count: integer);
begin
// if this method is not defined -> Abstract error
end;

{ TEditor }

procedure TEditor.Init(InProject : boolean;const Caption, Filename : AnsiString;DoOpen : boolean;IsRes: boolean = FALSE);
var
	s: AnsiString;
	pt: TPoint;
begin
	// Set generic options
	fErrorLine:= -1;
	fActiveLine:= -1;
	fRunToCursorLine := -1;
	fRes:= IsRes;
	fInProject := InProject;
	if Filename = '' then
		fFileName := Caption
	else
		fFileName := Filename;

	// Create a new tab
	fTabSheet := TTabSheet.Create(MainForm.PageControl);
	fTabSheet.Caption := Caption;
	fTabSheet.PageControl := MainForm.PageControl;
	fTabSheet.Tag := integer(self); // Define an index for each tab

	// Set breakpoint events
	fOnBreakpointToggle := MainForm.OnBreakpointToggle;

	// Create an editor and set static options
	fText := TSynEdit.Create(fTabSheet);

	// Load the file using Lines
	if (DoOpen) then begin
		fText.Lines.LoadFromFile(FileName);
		fNew := False;
		if devData.Backups then begin
			s:= ExtractfileExt(FileName);
			Insert('~', s, Pos('.', s) + 1);
			Delete(s, Length(s) -1, 1);
			fText.Lines.SaveToFile(ChangeFileExt(FileName, s));
		end;
	end else
		fNew := True;

	fText.Parent := fTabSheet;
	fText.Align := alClient;
	fText.Visible := True;
	fText.PopupMenu := MainForm.EditorPopupMenu;
	fText.WantTabs := True;
	fText.ShowHint := True;
	fText.OnStatusChange:= EditorStatusChange;
	fText.OnSpecialLineColors:= EditorSpecialLineColors;
	fText.OnGutterClick:= EditorGutterClick;
	fText.OnReplaceText:= EditorReplaceText;
	fText.OnDropFiles:= EditorDropFiles;
	fText.OnDblClick:= EditorDblClick;
	fText.OnMouseUp := EditorMouseUp;
	fText.OnMouseMove := EditorMouseMove;
	fText.OnExit := EditorExit;
	fText.OnPaintTransient := EditorPaintTransient;
	fText.MaxScrollWidth:=4096; // bug-fix #600748
	fText.MaxUndo:=4096;
	fText.BorderStyle:=bsNone;

	fText.Gutter.LeftOffset := 4;
	fText.Gutter.RightOffset := 21;
	fText.Gutter.BorderStyle := gbsNone;

	// Set the variable options
	devEditor.AssignEditor(fText);

	// Select a highlighter
	if not fNew then
		fText.Highlighter:= dmMain.GetHighlighter(fFileName)
	else if fRes then
		fText.Highlighter:= dmMain.Res
	else
		fText.Highlighter:= dmMain.cpp;

	// ReScan depends on the highlighter, so scan here
	fText.ReScan;

	// Set the text color
	StrtoPoint(pt, devEditor.Syntax.Values[cSel]);
	fText.SelectedColor.Background:= pt.X;
	fText.SelectedColor.Foreground:= pt.Y;

	// Folding bar color
	StrtoPoint(pt, devEditor.Syntax.Values[cFld]);
	fText.CodeFolding.FolderBarLinesColor := pt.y;

	// Create a gutter
	fDebugGutter := TDebugGutter.Create(self);

	// Initialize code completion stuff
	InitCompletion;

	// Function parameter tips
	fFunctionTip := TCodeToolTip.Create(Application);
	fFunctionTip.Editor := FText;
	fFunctionTip.Parser := MainForm.CppParser;

	// Auto indent synedit plugin
	FAutoIndent := TSynAutoIndent.Create(Application);
	FAutoIndent.Editor := FText;
	FAutoIndent.IndentChars := '{:';
	FAutoIndent.UnIndentChars := '}';

	// Setup a monitor which keeps track of outside-of-editor changes
	MainForm.devFileMonitor.Files.Add(fFileName);
	MainForm.devFileMonitor.Refresh(True);

	// RNC set any breakpoints that should be set in this file
	SetBreakPointsOnOpen;

	// Set status bar for the first time
	with MainForm.Statusbar do begin
		// Set readonly / insert / overwrite
		if fText.ReadOnly then
			Panels[1].Text:= Lang[ID_READONLY]
		else if fText.InsertMode then
			Panels[1].Text:= Lang[ID_INSERT]
		else
			Panels[1].Text:= Lang[ID_OVERWRITE];
	end;
end;

destructor TEditor.Destroy;
var
	idx: integer;
	lastActPage: Integer;
begin
	// Deactivate the file change monitor
	idx:=MainForm.devFileMonitor.Files.IndexOf(fFileName);
	if idx<>-1 then begin
		MainForm.devFileMonitor.Files.Delete(idx);
		MainForm.devFileMonitor.Refresh(False);
	end;

	// Destroy any completion stuff
	DestroyCompletion;

	// Free everything
	fDebugGutter.Free;
	fFunctionTip.Free;
	fAutoIndent.Free;
	fText.Free;

	// Activates previous tab instead of first one when closing
	with fTabSheet.PageControl do begin
		lastActPage := ActivePageIndex;
		FreeAndNil(fTabSheet);
		if lastActPage >= PageCount then begin
			Dec(lastActPage);
			if (lastActPage > 0) and (lastActPage < PageCount) then
				ActivePageIndex := lastActPage;
		end;
	end;

	inherited;
end;

procedure TEditor.Activate;
begin
	if Assigned(fTabSheet) then begin
		fTabSheet.PageControl.Show;
		fTabSheet.PageControl.ActivePage := fTabSheet;

		if fText.Visible then
			fText.SetFocus;
		if MainForm.ClassBrowser1.Enabled then
			MainForm.PageControlChange(MainForm.PageControl); // this makes sure that the classbrowser is consistent
	end;
end;

// RNC 07-21-04 These functions are used to turn off/on a breakpoint
// without calling RemoveBreakpoint or AddBreakpoint in fDebugger.
// This is helpful when trying to automitically remove a breakpoint
// when a user tries to add one while the debugger is running
procedure TEditor.InsertBreakpoint(line: integer);
begin
	if(line > 0) and (line <= fText.UnCollapsedLines.Count) then begin
		fText.InvalidateLine(line);
		fText.InvalidateGutterLine(line);
	end;
end;

procedure TEditor.RemoveBreakpoint(line: integer);
begin
	if(line > 0) and (line <= fText.UnCollapsedLines.Count) then begin
		fText.InvalidateLine(line);
		fText.InvalidateGutterLine(line);
	end;
end;

// RNC -- 07-02-2004
// I added methods to turn a breakpoint on or off.  Sometimes run to cursor
// got confused and by toggling a breakpoint was turning something on that should
// have been turned off.  By using these functions to explicitly turn on or turn
// off a breakpoint, this cannot happen
procedure TEditor.TurnOnBreakpoint(line: integer);
var
index:integer;
begin
	index := 0;
  if(line > 0) and (line <= fText.UnCollapsedLines.Count) then
    begin
      fText.InvalidateLine(line);
      // RNC new synedit stuff
      fText.InvalidateGutterLine(line);
      MainForm.AddBreakPointToList(line, self, fFilename);
      index := BreakPointList.Count -1;
  end;
  if Assigned(fOnBreakpointToggle) then
    fonBreakpointToggle(index, true);
end;

procedure TEditor.TurnOffBreakpoint(line: integer);
var
  index: integer;
begin
  if(line > 0) and (line <= fText.UnCollapsedLines.Count) then
    begin
      fText.InvalidateLine(line);
      // RNC new synedit stuff
      fText.InvalidateGutterLine(line);
      index := HasBreakPoint(line);
      if index <> -1 then begin
        index:=MainForm.RemoveBreakPointFromList(line, self);
        if Assigned(fOnBreakpointToggle) then
          fonBreakpointToggle(index, false);
      end;
  end;
end;

//RNC function to set breakpoints in a file when it is opened
procedure TEditor.SetBreakPointsOnOpen;
var
  i: integer;
begin
  for i:=0 to BreakPointList.Count -1 do
  begin
      if PBreakPointEntry(BreakPointList.Items[i])^.file_name = self.TabSheet.Caption then begin
          InsertBreakpoint(PBreakPointEntry(BreakPointList.Items[i])^.line);
          PBreakPointEntry(BreakPointList.Items[i])^.editor := self;
      end;
  end;
end;

function TEditor.ToggleBreakpoint(Line: integer): boolean;
var
	idx: integer;
begin
	idx := 0; // Was declared but not defined, could've contained random data
	result:= FALSE;
	if (line > 0) and (line <= fText.UnCollapsedLines.Count) then begin
		fText.InvalidateGutterLine(line);
		fText.InvalidateLine(line);
		idx:= HasBreakPoint(Line);

		//RNC moved the check to see if the debugger is running to here
		if idx <> -1 then begin
			if (MainForm.fDebugger.Executing and MainForm.fDebugger.IsBroken) or not MainForm.fDebugger.Executing then begin
				idx := MainForm.RemoveBreakPointFromList(line, self); // RNC
			end else if (MainForm.fDebugger.Executing and not MainForm.fDebugger.IsBroken) then begin
				MessageDlg('Cannot remove a breakpoint while the debugger is executing.', mtError, [mbOK], 0);
			end;
		end else begin
			if (MainForm.fDebugger.Executing and MainForm.fDebugger.IsBroken) or not MainForm.fDebugger.Executing then begin
				MainForm.AddBreakPointToList(line, self, self.TabSheet.Caption); // RNC
				idx := BreakPointList.Count -1;
				result:= TRUE;
			end else if (MainForm.fDebugger.Executing and not MainForm.fDebugger.IsBroken) then begin
				MessageDlg('Cannot add a breakpoint while the debugger is executing.', mtError, [mbOK], 0);
			end;
		end;
	end else
		fText.Invalidate;
	if Assigned(fOnBreakpointToggle) then
		fOnBreakpointToggle(idx, Result);
end;

//Rnc change to use the new list of breakpoints
function TEditor.HasBreakPoint(line_number: integer): integer;
begin
	for result:= 0 to BreakPointList.Count-1 do begin
		if PBreakPointEntry(BreakPointList.Items[result])^.editor = self then begin
			if PBreakPointEntry(BreakPointList.Items[result])^.line = line_number then
 				exit;
		end;
	end;
	result:= -1;
end;

procedure TEditor.EditorSpecialLineColors(Sender: TObject; Line: Integer;
    var Special: Boolean; var FG, BG: TColor);
var
 pt: TPoint;
begin
  if (Line = fActiveLine) then begin
   StrtoPoint(pt, devEditor.Syntax.Values[cABP]);
   BG:= pt.X;
   FG:= pt.Y;
   Special:= TRUE;
  end
  else if (HasBreakpoint(line) <> -1) then
   begin
     StrtoPoint(pt, devEditor.Syntax.Values[cBP]);
     BG := pt.x;
     FG := pt.y;
     Special := TRUE;
   end
  else
   if Line = fErrorLine then
    begin
      StrtoPoint(pt, devEditor.Syntax.Values[cErr]);
      bg:= pt.x;
      fg:= pt.y;
      Special:= TRUE;
    end;
end;

// ** undo after insert removes all text above insert point;
// ** seems to be a synedit bug!!
procedure TEditor.EditorDropFiles(Sender: TObject; x, y: integer;aFiles: TStrings);
var
	sl: TStringList;
	I, J: integer;
begin
	// Insert into current editor
	if devEditor.InsDropFiles then begin
		fText.CaretXY:= fText.DisplayToBufferPos(fText.PixelsToRowColumn(x, y));

		sl:= TStringList.Create;
		try
			for I := 0 to pred(aFiles.Count) do begin
				sl.LoadFromFile(aFiles[I]);
				fText.SelText:= sl.Text;
			end;
		finally
			sl.Free;
		end;
	end else

		// Create new tab/project
		for I := 0 to pred(aFiles.Count) do begin
			J := MainForm.FileIsOpen(aFiles[I]);
			if J = -1 then begin // if not open yet
				if GetFileTyp(aFiles[I]) = utPrj then
					MainForm.OpenProject(aFiles[I])
				else
					MainForm.OpenFile(aFiles[I]);
			end else begin// if already open
				TEditor(MainForm.PageControl.Pages[J].Tag).Activate;
			end;
		end;
end;

procedure TEditor.EditorDblClick(Sender: TObject);
begin
	if devEditor.DblClkLine then begin
		fText.BlockBegin:= BufferCoord(1, fText.CaretY);
		fText.BlockEnd:= BufferCoord(1, fText.CaretY +1);
	end;
end;

procedure TEditor.EditorGutterClick(Sender: TObject; Button: TMouseButton;
  x, y, Line: integer; mark: TSynEditMark);
begin
  ToggleBreakPoint(Line);
end;

procedure TEditor.EditorReplaceText(Sender: TObject; const aSearch,aReplace: AnsiString; Line, Column: integer; var Action: TSynReplaceAction);
var
	pt	: TPoint;
begin
  if SearchCenter.SingleFile then
   begin
     if aSearch = aReplace then
      begin
        fText.CaretXY:= BufferCoord(Column, Line +1);
        action:= raSkip;
      end
     else
      begin
        pt:= fText.ClienttoScreen(fText.RowColumnToPixels(DisplayCoord(Column, Line +1)));

        MessageBeep(MB_ICONQUESTION);
        case MessageDlgPos(format(Lang[ID_MSG_SEARCHREPLACEPROMPT], [aSearch]),
               mtConfirmation, [mbYes, mbNo, mbCancel, mbAll], 0, pt.x, pt.y) of
         mrYes: action:= raReplace;
         mrNo: action:= raSkip;
         mrCancel: action:= raCancel;
         mrAll: action:= raReplaceAll;
        end;
      end;
   end;
end;

// Handle WM_KILLFOCUS instead of all the special cases to hide the code tooltip
procedure TEditor.EditorExit(Sender : TObject);
begin
	if Assigned(fFunctionTip) then
		fFunctionTip.ReleaseHandle;
end;

procedure TEditor.EditorStatusChange(Sender: TObject;Changes: TSynStatusChanges);
begin
	if scModified in Changes then begin // scModified is only fired when the modified state changes
		if Text.Modified then begin
			MainForm.SetStatusbarMessage(Lang[ID_MODIFIED]);
			UpdateCaption('[*] '+ExtractfileName(fFileName));
			if fText.UnCollapsedLines.Count = 0 then
				fText.ReScan;
		end else begin
			UpdateCaption(ExtractfileName(fFileName));
			MainForm.SetStatusbarMessage('');
		end;
	end;

	// Don't allow mouseover tips when scrolling
	if scTopLine in Changes then begin
		fAllowMouseOver := false;
	end;

	// Slow, but needed...
	if scSelection in Changes then begin
		MainForm.SetStatusbarLineCol;
		fAllowMouseOver := false;
	end;

	if Changes * [scCaretX, scCaretY] <> [] then begin

		// Prevent doing this stuff twice (on CaretX and CaretY change)
		if (fText.CaretX = FLastPos.Char) and (fText.CaretY = FLastPos.Line) then
			Exit;

		FLastPos.Char := fText.CaretX;
		FLastPos.Line := fText.CaretY;

		if not fErrSetting and (fErrorLine <> -1) then begin
			fText.InvalidateLine(fErrorLine);
			fText.InvalidateGutterLine(fErrorLine);
			fErrorLine:= -1;
			fText.InvalidateLine(fErrorLine);
			fText.InvalidateGutterLine(fErrorLine);
		end;

		if Assigned(fFunctionTipTimer) then begin
			if fFunctionTip.Activated and FunctionTipAllowed then
				fFunctionTip.Show
			else begin // Reset the timer
				fFunctionTipTimer.Enabled := false;
				fFunctionTipTimer.Enabled := true;
			end;
		end;
	end;

	if scInsertMode in Changes then begin
		with MainForm.Statusbar do begin
			// Set readonly / insert / overwrite
			if fText.ReadOnly then
				Panels[1].Text:= Lang[ID_READONLY]
			else if fText.InsertMode then
				Panels[1].Text:= Lang[ID_INSERT]
			else
				Panels[1].Text:= Lang[ID_OVERWRITE];
		end;
	end;
end;

function TEditor.FunctionTipAllowed : boolean;
begin
	Result := not fText.IsScrolling and fText.Focused and not fText.SelAvail and devEditor.ShowFunctionTip and Assigned(fText.Highlighter);
end;

procedure TEditor.FunctionTipTimer(Sender : TObject);
begin
	if FunctionTipAllowed then
		fFunctionTip.Show;
end;

procedure TEditor.Exportto(filetype: integer);
begin
	if filetype = 0 then begin
		with dmMain.SaveDialog do begin
			Filter:= dmMain.SynExporterHTML.DefaultFilter;
			Title:= Lang[ID_NV_EXPORT];
			DefaultExt := HTML_EXT;
			if Execute then begin
				dmMain.ExportToHtml(fText.UnCollapsedLines, dmMain.SaveDialog.FileName);
				fText.BlockEnd:= fText.BlockBegin;
			end;
		end;
	end else if filetype = 1 then begin
		with dmMain.SaveDialog do begin
			Filter:= dmMain.SynExporterRTF.DefaultFilter;
			Title:= Lang[ID_NV_EXPORT];
			DefaultExt := RTF_EXT;
			if Execute then begin
				dmMain.ExportToRtf(fText.UnCollapsedLines, dmMain.SaveDialog.FileName);
				fText.BlockEnd:= fText.BlockBegin;
			end;
		end;
	end else begin
		with dmMain.SaveDialog do begin
			Filter:= dmMain.SynExporterTex.DefaultFilter;
			Title:= Lang[ID_NV_EXPORT];
			DefaultExt := TEX_EXT;
			if Execute then begin
				dmMain.ExportToTex(fText.UnCollapsedLines, dmMain.SaveDialog.FileName);
				fText.BlockEnd:= fText.BlockBegin;
			end;
		end;
	end;
end;

{** Modified by Peter **}
procedure TEditor.GotoLine;
var
	GotoForm: TGotoLineForm;
begin
	GotoForm := TGotoLineForm.Create(nil);
	try
		if GotoForm.ShowModal = mrOK then
			fText.CaretXY:= BufferCoord(1, Max(GotoForm.Line.Value,fText.Lines.Count));

		Activate;
	finally
		GotoForm.Free;
	end;
end;

procedure TEditor.InsertString(Value: AnsiString;MoveCursor: boolean);
var
	NewCursorPos: TBufferCoord;
	Char,Line,I : integer;
	P : PAnsiChar;
begin
	NewCursorPos := fText.CaretXY;
	if MoveCursor then begin
		P := PAnsiChar(value);
		Char := fText.CaretX;
		Line := fText.CaretY;
		I := 0;
		while P[I] <> #0 do begin

			// Assume DOS newlines
			if(P[I] = #13) and (P[I+1] = #10) then begin
				Inc(I,2);
				Inc(Line);
				Char := 1;
			end else if (P[I] = '*') and (P[I+1] = '|') and (P[I+2] = '*') then begin
				NewCursorPos.Char := Char;
				NewCursorPos.Line := Line;
				Delete(value,I+1,3);
				break;
			end else begin
				Inc(Char);
				Inc(I);
			end;
		end;
	end;
	fText.SelText:= value;

	// Update the cursor
	fText.CaretXY:= NewCursorPos;
	fText.EnsureCursorPosVisible;
end;

function TEditor.Search(isReplace: boolean): boolean;
var
	s: AnsiString;
begin
	if devEditor.FindText then
		if (fText.SelText = '') then
			s:= fText.WordAtCursor
		else
			s:= fText.SelText;

	with SearchCenter do begin
		FindText:= s;
		Replace:= IsReplace;
		ReplaceText:= '';
		SingleFile:=  TRUE;
		Editor:= Self;
		Result:=ExecuteSearch and not SingleFile; // if changed to "find in all files", open find results
	end;
	Activate;
end;

procedure TEditor.SearchAgain;
var
	Options : TSynSearchOptions;
	return : integer;
begin
	SearchCenter.Editor := Self;
	SearchCenter.AssignSearchEngine;

	if not SearchCenter.SingleFile then
		exit;
	if SearchCenter.FindText = '' then begin
		Search(false);
		exit;
	end;
	Options:= SearchCenter.Options;
	Exclude(Options, ssoEntireScope);

	return:= fText.SearchReplace( SearchCenter.FindText,SearchCenter.ReplaceText,Options);
	if return <> 0 then
		Activate
	else
		MessageDlg(format(Lang[ID_MSG_TEXTNOTFOUND], [SearchCenter.FindText]),mtInformation, [mbOk], 0);
end;

procedure TEditor.SetErrorFocus(Col, Line: integer);
begin
	fErrSetting:= TRUE;
	if fErrorLine <> Line then begin
		if fErrorLine <> -1 then
			fText.InvalidateLine(fErrorLine);

		fText.InvalidateGutterLine(fErrorLine);
		fErrorLine := Line;
		fText.InvalidateLine(fErrorLine);
		fText.InvalidateGutterLine(fErrorLine);
	end;
	fText.CaretXY := BufferCoord(Col, Line);
	fText.EnsureCursorPosVisible;
	fErrSetting := FALSE;
end;

procedure TEditor.SetActiveBreakpointFocus(Line: integer);
begin
  if (fActiveLine <> Line) and (fActiveLine <> -1) then
   fText.InvalidateLine(fActiveLine);
   fText.InvalidateGutterLine(fActiveLine);
  fActiveLine:= Line;
  fText.InvalidateLine(fActiveLine);
   fText.InvalidateGutterLine(fActiveLine);
  fText.CaretY:= Line;
  fText.EnsureCursorPosVisible;
  // RNC -- 07.02.2004 This will clear the run to cursor value when a breakpoint is hit
  {if Line = fRunToCursorLine then begin}
  if fRunToCursorLine <> -1 then begin
    TurnOffBreakpoint(fRunToCursorLine);
    fRunToCursorLine := -1;
  end;
end;

procedure TEditor.RemoveBreakpointFocus;
begin
  if fActiveLine <> -1 then
   fText.InvalidateLine(fActiveLine);
   fText.InvalidateGutterLine(fActiveLine);
  fActiveLine:= -1;
end;

procedure TEditor.UpdateCaption(const NewCaption: AnsiString);
begin
  if assigned(fTabSheet) then
   fTabSheet.Caption:= NewCaption;
end;

procedure TEditor.SetFileName(const value: AnsiString);
begin
  if value <> fFileName then
   begin
     ffileName:= value;
     UpdateCaption(ExtractfileName(fFileName));
   end;
end;

procedure TEditor.DrawGutterImages(ACanvas: TCanvas; AClip: TRect;FirstLine, LastLine: integer);
var
  LH, X, Y: integer;
  ImgIndex: integer;
begin
  X := 14;
  LH := fText.LineHeight;
  Y := (LH - dmMain.GutterImages.Height) div 2
       + LH * (FirstLine - fText.TopLine);

  while FirstLine <= LastLine do begin
    if HasBreakpoint(FirstLine) <> -1 then
      ImgIndex := 0
    else if fActiveLine = FirstLine then
      ImgIndex := 1
    else if fErrorLine = FirstLine then
      ImgIndex := 2
    else
      ImgIndex := -1;
    if ImgIndex >= 0 then
      dmMain.GutterImages.Draw(ACanvas, X, Y, ImgIndex);
    Inc(FirstLine);
    Inc(Y, LH);
  end;
end;

procedure TEditor.InsertDefaultText;
var
	tmp: TStrings;
begin
	if devEditor.DefaultCode and FileExists(devDirs.Config + DEV_DEFAULTCODE_FILE) then begin
		tmp:= TStringList.Create;
		try
			tmp.LoadFromFile(devDirs.Config + DEV_DEFAULTCODE_FILE);
			InsertString(ParseMacros(tmp.Text), FALSE);
		finally
			tmp.Free;
		end;
	end;
end;

procedure TEditor.GotoLineNr(Nr: integer);
begin
	fText.CaretXY:= BufferCoord(1, Nr);
	fText.TopLine:=Nr;
	Activate;
end;

procedure TEditor.EditorKeyPress(Sender: TObject; var Key: Char);
var
	P: TPoint;
	allowcompletion : boolean;
	cursorpos,indent : integer;
	attr : TSynHighlighterAttributes;
	s : AnsiString;
	Ptr : PAnsiChar;
begin

	// Doing this here instead of in EditorKeyDown to be able to delete some key messages
	if devEditor.CompleteSymbols and not (Sender is TForm) and not fText.SelAvail then begin

		// Allerhande voorwaarden
		allowcompletion := true;
		if(fText.GetHighlighterAttriAtRowCol(BufferCoord(fText.CaretX-1,fText.CaretY), s, attr)) then
			if (attr = fText.Highlighter.StringAttribute) or (attr = fText.Highlighter.CommentAttribute) then
				allowcompletion := false;

		if allowcompletion then begin

			// Check if we typed anything completable...
			if (Key = '(') and devEditor.ParentheseComplete then begin
				InsertString(')',false);
				HasCompletedParentheses := 2;

				// immediately activate function hint
				if FunctionTipAllowed then
					fFunctionTip.Activated := true;
			end else if (Key = ')') and (HasCompletedParentheses > 0) then begin
				fText.CaretXY := BufferCoord(fText.CaretX + 1,fText.CaretY);
				HasCompletedParentheses := 0;
				Key:=#0;
			end else if (Key = '[') and devEditor.ArrayComplete then begin
				InsertString(']',false);
				HasCompletedArray := 2;
			end else if (Key = ']') and (HasCompletedArray > 0) then begin
				fText.CaretXY := BufferCoord(fText.CaretX + 1,fText.CaretY);
				HasCompletedArray := 0;
				Key:=#0;
			end else if (Key = '*') and devEditor.CommentComplete then begin
				if (fText.CaretX > 1) and (fText.LineText[fText.CaretX-1] = '/') then
					InsertString('*/',false);
			end else if (Key = '{') and devEditor.BraceComplete then begin

				Ptr := PAnsiChar(fText.Lines.Text);
				cursorpos := fText.RowColToCharIndex(fText.CaretXY);

				// If there's any text before the cursor...
				if not (Ptr[cursorpos-1] in [#13,#10]) and not (cursorpos = 0) then begin

					// See what the last nonblank character before the cursor is
					repeat
						Dec(cursorpos);
					until (cursorpos = 0) or not (Ptr[cursorpos] in [#9,#32]);

					// Complete curly braces of if blocks or function etc.
					if (Ptr[cursorpos] in [')']) or (StrLComp(@Ptr[cursorpos-3],'else',4) = 0) then begin

						// Check indentation
						indent:=0;
						repeat
							Inc(indent);
						until not (fText.LineText[indent] in [#9,#32]);

						// { + enter + Indent + }
						InsertString('{' + #13#10 + Copy(fText.LineText,1,indent-1) + '}',false);
						fText.CaretXY := BufferCoord(fText.CaretX + 1,fText.CaretY);
						Key:=#0;
					end else if StartsStr('struct',TrimLeft(fText.LineText)) or
								StartsStr('union', TrimLeft(fText.LineText)) or
								StartsStr('class', TrimLeft(fText.LineText)) or
								StartsStr('enum',  TrimLeft(fText.LineText)) then begin

						// Check indentation
						indent:=0;
						repeat
							Inc(indent);
						until not (fText.LineText[indent] in [#9,#32]);

						// { + enter + indent + };
						InsertString('{' + #13#10 + Copy(fText.LineText,1,indent-1) + '};',false);
						fText.CaretXY := BufferCoord(fText.CaretX + 1,fText.CaretY);
						Key:=#0;
					end else if StartsStr('case',TrimLeft(fText.LineText)) then begin

						// Check indentation
						indent:=0;
						repeat
							Inc(indent);
						until not (fText.LineText[indent] in [#9,#32]);

						// { + enter + indent + tab + break; + enter + }
						InsertString('{' + #13#10 + Copy(fText.LineText,1,indent-1) + #9 + 'break;' + #13#10 + Copy(fText.LineText,1,indent-1) + '}',false);
						fText.CaretXY := BufferCoord(fText.CaretX + 1,fText.CaretY);
						Key:=#0;
					end else begin
						HasCompletedCurly := 2;
						InsertString('}',false);
					end;
				end else
					InsertString(#13#10 + '}',false);
			end else if (Key = '}') and (HasCompletedCurly > 0) then begin
				fText.CaretXY := BufferCoord(fText.CaretX + 1,fText.CaretY);
				HasCompletedCurly := 0;
				Key:=#0;
			end else if (Key = '<') and devEditor.IncludeComplete then begin
				if StartsStr('#include',fText.LineText) then
					InsertString('>',false);
			end else if (Key = '"') and devEditor.IncludeComplete then begin
				if StartsStr('#include',fText.LineText) then
					InsertString('"',false);
			end;
		end;
	end;

	if fCompletionBox.Enabled then begin

		// If we haven't yet showed the completion window...
		if not (Sender is TForm) then begin
			fCompletionTimer.Enabled:=False;
			fCompletionTimerKey:=Key;
			case Key of
				'.': fCompletionTimer.Enabled:=True;
				'>': if (fText.CaretX > 1) and (fText.LineText[fText.CaretX-1]='-') then fCompletionTimer.Enabled:=True;
				':': if (fText.CaretX > 1) and (fText.LineText[fText.CaretX-1]=':') then fCompletionTimer.Enabled:=True;
				' ': if fCompletionEatSpace then Key:=#0; // eat space if it was ctrl+space (code-completion)
			end;
			P := fText.RowColumnToPixels(fText.DisplayXY);
			P.Y := P.Y + 16;

			P := fText.ClientToScreen(P);
			fCompletionBox.Position:=P;

		// The completion form is already shown
		end else begin
			case Key of
				Char(VK_BACK): if fText.SelStart > 0 then begin
					fText.SelStart := fText.SelStart - 1;
					fText.SelEnd := fText.SelStart+1;
					fText.SelText := '';
					fCompletionBox.Search(nil, CurrentPhrase, fFileName);
				end;
				Char(VK_RETURN): begin
					SetEditorText(Key);
					fCompletionBox.Hide;
				end;
				';', '(', ' ': begin
					SetEditorText(Key);
					fCompletionBox.Hide;
				end;
				'.', '>', ':': begin
					SetEditorText(Key);
					fCompletionBox.Search(nil, CurrentPhrase, fFileName);
				end;
				else begin
					fText.SelText := Key;
					fCompletionBox.Search(nil, CurrentPhrase, fFileName);
				end;
			end;
		end;
		fCompletionEatSpace:=False;
	end;
end;

procedure TEditor.EditorKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
var
	M: TMemoryStream;
begin
	if devEditor.CompleteSymbols then begin
		if not (ssShift in Shift) then begin
			Dec(HasCompletedParentheses);
			Dec(HasCompletedArray);
			Dec(HasCompletedCurly);
		end;
	end;

	// Show the completion box. This needs to be done after the key is inserted!
	if fCompletionBox.Enabled then begin
		fCompletionBox.OnKeyPress:=EditorKeyPress;
		if ssCtrl in Shift then
			if Key=VK_SPACE then begin
				M:=TMemoryStream.Create;
				try
					fText.Lines.SaveToStream(M);
					fCompletionBox.CurrentClass:=MainForm.CppParser.FindAndScanBlockAt(fFileName, fText.CaretY, M);
				finally
					M.Free;
					fCompletionBox.Search(nil, CurrentPhrase, fFileName);
				end;
				fCompletionEatSpace:=True; // this is a hack. without this after ctrl+space, the space appears in the editor :(
			end;
	end;
end;

procedure TEditor.EditorKeyUp(Sender: TObject; var Key: Word;Shift: TShiftState);
begin
	fText.Cursor:=crIBeam;
end;

procedure TEditor.CompletionTimer(Sender: TObject);
var
	M: TMemoryStream;
	curr,s: AnsiString;
	attr: TSynHighlighterAttributes;
begin
	fCompletionTimer.Enabled:=False;
	curr:=CurrentPhrase;

	if(fText.GetHighlighterAttriAtRowCol(BufferCoord(fText.CaretX-1, fText.CaretY), s, attr)) then begin
		if (attr = fText.Highlighter.StringAttribute) or
		   (attr = fText.Highlighter.CommentAttribute) then begin

			fCompletionTimerKey:=#0;
			Exit;
		end;
	end;

	M:=TMemoryStream.Create;
	try
		fText.UnCollapsedLines.SaveToStream(M);
		fCompletionBox.CurrentClass:=MainForm.CppParser.FindAndScanBlockAt(fFileName, fText.CaretY, M);
	finally
		M.Free;
	end;

	case fCompletionTimerKey of
	'.':
		fCompletionBox.Search(nil, curr, fFileName);
	'>':
		if fText.CaretX > 1 then
			if fText.LineText[fText.CaretX-2]='-' then // it makes a '->'
				fCompletionBox.Search(nil, curr, fFileName);
	':':
		if fText.CaretX > 1 then
			if fText.LineText[fText.CaretX-2]=':' then // it makes a '::'
				fCompletionBox.Search(nil, curr, fFileName);
	end;
	fCompletionTimerKey:=#0;
end;

procedure TEditor.DestroyCompletion;
begin
	if Assigned(fCompletionTimer) then
		FreeAndNil(fCompletionTimer);
	if Assigned(fFunctionTipTimer) then
		FreeAndNil(fFunctionTipTimer);
	if Assigned(fMouseOverTimer) then
		FreeAndNil(fMouseOverTimer);
end;

procedure TEditor.InitCompletion;
begin
	fCompletionBox:=MainForm.CodeCompletion;
	fCompletionBox.Enabled:=devClassBrowsing.Enabled and devCodeCompletion.Enabled;

	// This way symbols and tabs are also completed without code completion
	fText.OnKeyPress := EditorKeyPress;
	fText.OnKeyDown := EditorKeyDown;
	fText.OnKeyUp := EditorKeyUp;

	if devEditor.ShowFunctionTip then begin
		if not Assigned(fFunctionTipTimer) then
			fFunctionTipTimer:=TTimer.Create(nil);
		fFunctionTipTimer.Enabled:=True;
		fFunctionTipTimer.OnTimer:=FunctionTipTimer;
		fFunctionTipTimer.Interval:=2*GetCaretBlinkTime; // fancy
	end;

	if devEditor.ParserHints or devData.WatchHint then begin
		if not Assigned(fMouseOverTimer) then
			fMouseOverTimer:=TTimer.Create(nil);
		fMouseOverTimer.Enabled:=True;
		fMouseOverTimer.OnTimer:=MouseOverTimer;
		fMouseOverTimer.Interval:=500;
	end;

	// The other stuff is fully completion dependant
	if fCompletionBox.Enabled then begin
		fCompletionBox.OnKeyPress:=EditorKeyPress;
		fCompletionBox.Width:=devCodeCompletion.Width;
		fCompletionBox.Height:=devCodeCompletion.Height;

		if not Assigned(fCompletionTimer) then
			fCompletionTimer:=TTimer.Create(nil);
		fCompletionTimer.Enabled:=False;
		fCompletionTimer.OnTimer:=CompletionTimer;
		fCompletionTimer.Interval:=devCodeCompletion.Delay;
	end;

	fCompletionEatSpace:=False;
end;

procedure TEditor.MouseOverTimer(Sender : TObject);
begin
	// If we waited long enough AFTER scrolling, allow mouseovers again
	if not fText.SelAvail and Assigned(fText.Highlighter) then
		fAllowMouseOver := true;
end;

function TEditor.CurrentPhrase: AnsiString;
var
	wordend,wordstart,arrayend,arraystart : integer;
	combiningcharfound : boolean;
	text : PChar;
begin
	text := PChar(fText.Lines.Text);
	wordend := fText.RowColToCharIndex(fText.CaretXY);
	wordstart := wordend;
	arraystart := 0;
	arrayend := 0;
	combiningcharfound := false;

	// Walk back until we find the combining character
	repeat
		if (StrLComp(@text[wordstart],'::',2) = 0) or (StrLComp(@text[wordstart],'->',2) = 0) then begin
			Dec(wordstart,2);
			combiningcharfound := true;
			break;
		end else if (text[wordstart] = '.')  then begin
			Dec(wordstart,1);
			combiningcharfound := true;
			break;
		end;
		Dec(wordstart);
	until (wordstart = 0) or (not (text[wordstart] in fText.IdentChars) and not (text[wordstart] in [':','-','>','.']));

	// Then, only allow fText.IdentChars, and skip array bits
	if combiningcharfound then begin
		repeat
			if text[wordstart] = ']' then begin
				arrayend := wordstart;
				repeat
					Dec(wordstart);
				until (wordstart = 0) or (text[wordstart] = '[');
				arraystart := wordstart;
			end;
			Dec(wordstart);
		until (wordstart = 0) or (not (text[wordstart] in fText.IdentChars));
	end;

	Result := Copy(text,wordstart+2,wordend-wordstart-1);
	if arrayend <> arraystart then
		Delete(Result,arraystart-wordstart,arrayend-arraystart+1);
end;

procedure TEditor.SetEditorText(Key: Char);
var
	Statement: PStatement;
	FuncAddOn: AnsiString;
begin
	Statement:=fCompletionBox.SelectedStatement;
	if fCompletionBox.SelectedIsFunction then begin
		if Key=';' then
			FuncAddOn := '();'
		else if Key='.' then
			FuncAddOn := '().'
		else if Key='>' then
			FuncAddOn := '()->'
		else
			FuncAddOn := '()';
	end else begin
{$IFDEF WIN32}
		if Key=Char(VK_RETURN) then
{$ENDIF}
{$IFDEF LINUX}
		if Key=Char(XK_RETURN) then
{$ENDIF}
			FuncAddOn := ''
		else if Key='>' then
			FuncAddOn := '->'
		else if Key=':' then
			FuncAddOn := '::'
		else
			FuncAddOn := Key;
	end;

	if Assigned(Statement) then begin
		// delete selections made
		fText.SelText:='';

		// delete the part of the word that's already been typed
		fText.SelStart := fText.RowColToCharIndex(fText.WordStart);
		fText.SelEnd := fText.RowColToCharIndex(fText.WordEnd);

		// replae the selection
		fText.SelText:=Statement^._Command + FuncAddOn;

		// if we added "()" move caret inside parenthesis
		// only if Key<>'.' and Key<>'>'
		// and function takes arguments...
		if (not (Key in ['.', '>'])) and (FuncAddOn<>'') and ( (Length(Statement^._Args)>2) or (Statement^._Args='()') ) then begin
			fText.CaretX:=fText.CaretX-Length(FuncAddOn)+1;
			if FunctionTipAllowed then
				fFunctionTip.Show;
		end;
	end;
end;

//////// CODE-COMPLETION - mandrav - END ///////

procedure TEditor.EditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
	s,localfind : string;
	p : TBufferCoord;
	attr : TSynHighlighterAttributes;
	st: PStatement;
begin
	// If the mouse van be found INSIDE the window
	if fText.GetPositionOfMouse(p) and fAllowMouseOver then begin

		// Remove leftover filename hint which is shown if fText.Hint is ''
		TabSheet.PageControl.Hint := '';

		// Check if we're inside a comment or string by looking at text color. If we are, skip without showing a tooltip
		if fText.GetHighlighterAttriAtRowCol(p, s, attr) then
			if not (attr = fText.Highlighter.IdentifierAttribute) then begin
				Application.CancelHint;
				fText.Hint := '';
				Exit;
			end;

		s := fText.GetWordAtRowCol(p);

		if(s <> '')then begin

			// Handle Ctrl+Click too
			if ssCtrl in Shift then
				fText.Cursor:=crHandPoint
			else
				fText.Cursor:=crIBeam;

			if(s <> fCurrentWord) then begin

				fCurrentWord := s;

				if devEditor.ParserHints and not MainForm.fDebugger.Executing then begin
					st:=MainForm.FindStatement(s,fText.WordStartEx(p),localfind,p);
					if localfind <> '' then begin
						Application.CancelHint;
						if (p.Char <> 12345) and (p.Line <> 12345) then
							fText.Hint:=localfind + ' - ' + ExtractFileName(fFileName) + ' (' + inttostr(p.Line) + ') - Ctrl+Click to follow'
						else
							fText.Hint:=localfind;
					end else if Assigned(st) then begin
						Application.CancelHint;
						fText.Hint:=Trim(st^._FullText) + ' - ' + ExtractFileName(st^._FileName) + ' (' + inttostr(st^._Line) + ') - Ctrl+Click to follow';
					end else begin
						Application.CancelHint;
						fCurrentWord := s;
						fText.Hint := '';
					end;
				end else if devData.WatchHint and MainForm.fDebugger.Executing then begin
					MainForm.fDebugger.SendCommand(GDB_DISPLAY, s);

					Application.CancelHint;
					fText.Hint:=MainForm.fDebugger.WatchVar + ' = ' + MainForm.fDebugger.WatchValue;
				end;
			end;
		end else begin

			// Handle Ctrl+Click too
			fText.Cursor:=crIBeam;

			fCurrentWord := s;
			Application.CancelHint;
			fText.Hint := '';
		end;
	end;
end;

procedure TEditor.CommentSelection;
var
	oldbbegin,oldbend : TBufferCoord;
	localcopy : string;
	CurPos,len : integer;
begin
	oldbbegin := fText.BlockBegin;
	oldbend := fText.BlockEnd;

	// Prevent repaints while we're busy
	fText.BeginUpdate;

	if fText.BlockBegin.Line <> fText.BlockEnd.Line then begin

		// Comment the first one
		localcopy := '//' + fText.SelText;
		CurPos := 1;
		len := Length(localcopy);
		while CurPos <= len do begin

			// find any enter sequence...
			if(localcopy[CurPos] in [#13,#10]) then begin
				repeat
					Inc(CurPos);
				until (CurPos = len) or not (localcopy[CurPos] in [#13,#10]);

				// and comment only when this enter isn't trailing
				if CurPos < len then begin
					Inc(len,2);
					Insert('//',localcopy,CurPos); // 1 based
				end;
			end;
			Inc(CurPos);
		end;
		fText.SelText := localcopy;
	end else begin
		fText.BeginUndoBlock;

		fText.LineText := '//' + fText.LineText;

		fText.UndoList.AddChange(crInsert,
			BufferCoord(1,fText.CaretY),
			BufferCoord(3,fText.CaretY),
			'',smNormal);

		fText.EndUndoBlock;
	end;

	// Prevent repaints while we're busy
	fText.EndUpdate;

	fText.BlockBegin := oldbbegin;
	if oldbend.Char = 1 then // move due to comment chars
		fText.BlockEnd := oldbend
	else
		fText.BlockEnd := BufferCoord(oldbend.Char + 2,oldbend.line);

	fText.UpdateCaret;
	fText.Modified := true;
end;

procedure TEditor.IndentSelection;
begin
	if FText.BlockBegin.Line <> FText.BlockEnd.Line then
		fText.ExecuteCommand(ecBlockIndent, #0, nil)
	else
		fText.ExecuteCommand(ecTab,#0, nil);
end;

procedure TEditor.UnindentSelection;
begin
	if fText.BlockBegin.Line <> fText.BlockEnd.Line then
		fText.ExecuteCommand(ecBlockUnIndent,#0, nil)
	else
		fText.ExecuteCommand(ecShiftTab,#0, nil);
end;

procedure TEditor.UncommentSelection;
var
	oldbbegin,oldbend : TBufferCoord;
	localcopy : string;
	CurPos : integer;
begin
	oldbbegin := fText.BlockBegin;
	oldbend := fText.BlockEnd;

	// Prevent repaints while we're busy
	fText.BeginUpdate;

	if fText.BlockBegin.Line <> fText.BlockEnd.Line then begin
		localcopy := fText.SelText;
		CurPos := 1;

		// Delete the first one
		if StrLComp(@localcopy[1],'//',2) = 0 then
			Delete(localcopy,CurPos,2);

		while(localcopy[CurPos] <> #0) do begin

			// find any enter sequence...
			if(localcopy[CurPos] in [#13,#10]) then begin
				repeat
					Inc(CurPos);
				until not (localcopy[CurPos] in [#0..#32]);

				if StrLComp(@localcopy[CurPos],'//',2) = 0 then
					Delete(localcopy,CurPos,2);
			end;
			Inc(CurPos);
		end;
		fText.SelText := localcopy;
	end else begin
		fText.BeginUndoBlock;

		localcopy := fText.LineText;
		CurPos := 1;
		while(localcopy[CurPos] <> #0) do begin

			// find any enter sequence...
			if StrLComp(@localcopy[CurPos],'//',2) = 0 then begin
				Delete(localcopy,CurPos,2);
				break;
			end;
			if not (localcopy[CurPos] in [#0..#32]) then
				break;
			Inc(CurPos);
		end;

		fText.LineText := localcopy;

		fText.UndoList.AddChange(crDelete,
			BufferCoord(CurPos,fText.CaretY),
			BufferCoord(CurPos,fText.CaretY),
			'//',smNormal);

		fText.EndUndoBlock;
	end;

	// Prevent repaints while we're busy
	fText.EndUpdate;

	fText.BlockBegin := oldbbegin;
	if oldbend.Char < 3 then // move due to comment chars
		fText.BlockEnd := oldbend
	else
		fText.BlockEnd := BufferCoord(oldbend.Char - 2,oldbend.line);

	fText.UpdateCaret;
	fText.Modified := true;
end;

procedure TEditor.EditorMouseUp(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
var
	p: TDisplayCoord;
	line,fname: AnsiString;
	walker,start : integer;
	e : TEditor;
begin
	p := fText.PixelsToRowColumn(X,Y);

	// if ctrl+clicked
	if (ssCtrl in Shift) and (Button = mbLeft) and (p.Row <= fText.Lines.Count) and not fText.SelAvail then begin

		// reset the cursor
		fText.Cursor:=crIBeam;

		line:=fText.Lines[p.Row-1];
		if StartsStr('#include',line) then begin

			// We've clicked an #include...
			walker := 0;
			repeat
				Inc(walker);
			until line[walker] in ['<','"'];
			start := walker + 1;

			repeat
				Inc(walker);
			until line[walker] in ['>','"'];

			fname := MainForm.CppParser.GetFullFileName(Copy(line, start, walker-start));
			if fname = ExtractFileName(fname) then // no path info, so prepend path of active file
				fname:=ExtractFilePath(fFileName)+fname;

			// refer to the editor of the filename (will open if needed and made active)
			e:=MainForm.GetEditorFromFileName(fname);
			if Assigned(e) then
				e.GotoLineNr(1);
		end;

		MainForm.actGotoImplDeclEditorExecute(self);
	end;
end;

procedure TEditor.RunToCursor(Line : integer);
begin
	if fRunToCursorLine <> -1 then
		TurnOffBreakpoint(fRunToCursorLine);
	fRunToCursorLine := Line;
	TurnOnBreakpoint(fRunToCursorLine);
end;

procedure TEditor.PaintMatchingBrackets(TransientType: TTransientType);
const
	OpenChars:array[0..2] of Char=('{','[','(');
	CloseChars:array[0..2] of Char=('}',']',')');
var
	P: TBufferCoord;
	Pix: TPoint;
	S: AnsiString;
	I: Integer;
	Attri: TSynHighlighterAttributes;
begin
	P := fText.CaretXY;
	if fText.GetHighlighterAttriAtRowCol(P, S, Attri) and (fText.Highlighter.SymbolAttribute = Attri) then begin
		for i := 0 to 2 do begin
			if (S = OpenChars[i]) or (S = CloseChars[i]) then begin
				Pix := fText.RowColumnToPixels(fText.BufferToDisplayPos(p));
				fText.Canvas.Brush.Style := bsSolid;
				fText.Canvas.Font.Assign(fText.Font);
				Text.Canvas.Font.Style := Attri.Style;

				if (TransientType = ttAfter) then begin
					fText.Canvas.Font.Color:= fText.Highlighter.WhitespaceAttribute.Background;
					fText.Canvas.Brush.Color := Attri.Foreground;
				end else begin
					fText.Canvas.Font.Color:= Attri.Foreground;
					if not devEditor.HighCurrLine then
						fText.Canvas.Brush.Color:= fText.Highlighter.WhitespaceAttribute.Background
					else
						fText.Canvas.Brush.Color:= devEditor.HighColor;
				end;

				fText.Canvas.TextOut(Pix.X, Pix.Y, S);
				P := fText.GetMatchingBracketEx(P);

				if not(TransientType = ttAfter) and not (P.Line = fText.CaretY) then
					fText.Canvas.Brush.Color:= fText.Highlighter.WhitespaceAttribute.Background;

				if (P.Char > 0) and (P.Line > 0) then begin
					Pix := fText.RowColumnToPixels(fText.BufferToDisplayPos(p));

					if S = OpenChars[i] then
						fText.Canvas.TextOut(Pix.X, Pix.Y, CloseChars[i])
					else
						fText.Canvas.TextOut(Pix.X, Pix.Y, OpenChars[i]);
				end;
			end;
		end;
		fText.Canvas.Brush.Style := bsSolid;
	end;
end;

procedure TEditor.EditorPaintTransient(Sender: TObject; Canvas: TCanvas;TransientType: TTransientType);
begin
	if Assigned(fText.Highlighter) and devEditor.Match and not fText.SelAvail then
		PaintMatchingBrackets(TransientType);
end;

// Editor needs to be told when class browser has been recreated otherwise AV !
procedure TEditor.UpdateParser;
begin
	fFunctionTip.Parser := MainForm.CppParser;
end;

end.

