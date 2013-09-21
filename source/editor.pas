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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, CodeCompletion, CppParser,
  Menus, ImgList, ComCtrls, StdCtrls, ExtCtrls, SynEdit, SynEditKeyCmds, version, SynEditCodeFolding,
  SynCompletionProposal, SynEditTextBuffer, Math, StrUtils, SynEditTypes, SynEditHighlighter, CodeToolTip;
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
    e : TEditor;
    procedure AfterPaint(ACanvas: TCanvas; const AClip: TRect;FirstLine, LastLine: integer); override;
    procedure LinesInserted(FirstLine, Count: integer); override;
    procedure LinesDeleted(FirstLine, Count: integer); override;
  public
    constructor Create(editor : TEditor);
  end;

  TEditor = class(TObject)
  private
    fInProject: boolean;
    fFileName: AnsiString;
    fNew: boolean;
    fRes: boolean;
    fText: TSynEdit;
    fTabSheet: TTabSheet;
    fErrorLine: integer;
    fActiveLine: integer;
    fDebugGutter: TDebugGutter;
    fCurrentWord: AnsiString;
    fCurrentEvalWord : AnsiString;
    fIgnoreCaretChange : boolean;

    fCompletionTimer: TTimer;
    fCompletionBox: TCodeCompletion;
    fFunctionTipTimer : TTimer;
    fFunctionTip: TCodeToolTip;
    fMouseOverTimer : TTimer;
    fAllowMouseOver : boolean;
  
    HasCompletedParentheses : integer; // Set to 2 on completion during KeyPress, to 1 immediately after by KeyDown, and to 0 upon next key
    HasCompletedArray : integer; // ...
    HasCompletedCurly : integer; // ...

    procedure EditorKeyPress(Sender: TObject; var Key: Char);
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditorKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure EditorReplaceText(Sender: TObject;const aSearch, aReplace: AnsiString; Line, Column: integer;var Action: TSynReplaceAction);
    procedure EditorDropFiles(Sender: TObject; x, y: integer;aFiles: TStrings);
    procedure EditorDblClick(Sender: TObject);
    procedure EditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure EditorExit(Sender : TObject);
    procedure EditorGutterClick(Sender: TObject; Button: TMouseButton;x, y, Line: integer; mark: TSynEditMark);
    procedure EditorSpecialLineColors(Sender: TObject; Line: integer;var Special: boolean; var FG, BG: TColor);

    procedure CompletionKeyPress(Sender: TObject; var Key: Char);

    procedure MouseOverTimer(Sender : TObject);

    procedure SetEditorText(Key: Char);
    function EvaluationPhrase(p : TBufferCoord): AnsiString;
    procedure CompletionTimer(Sender: TObject);

    function FunctionTipAllowed : boolean;
    procedure FunctionTipTimer(Sender : TObject);

    procedure SetFileName(const value: AnsiString);
    procedure EditorPaintTransient(Sender: TObject; Canvas: TCanvas; TransientType: TTransientType);
  public
    procedure Init(InProject : boolean;const Caption, Filename : AnsiString;DoOpen : boolean;IsRes: boolean = FALSE);
    destructor Destroy; override;

    // Recoded breakpoint stuff
    function HasBreakPoint(Line : integer): integer;
    procedure ToggleBreakPoint(Line : integer);
    procedure DebugAfterPaint(ACanvas: TCanvas; AClip: TRect;FirstLine, LastLine: integer);

    // Debugger callback
    procedure OnMouseOverEvalReady(const evalvalue : AnsiString);

    procedure Activate;
    procedure GotoLine;
    procedure GotoLineNr(Nr: integer);
    procedure Exportto(filetype: integer);
    procedure InsertString(Value: AnsiString;MoveCursor: boolean);
    procedure SetErrorFocus(Col, Line: integer);
    procedure SetActiveBreakpointFocus(Line: integer);
    procedure RemoveBreakpointFocus;
    procedure UpdateCaption(const NewCaption: AnsiString);
    procedure InsertDefaultText;
    procedure PaintMatchingBrackets(TransientType: TTransientType);

    function CompletionPhrase(p : TBufferCoord): AnsiString;

    procedure CommentSelection;
    procedure UncommentSelection;
    procedure IndentSelection;
    procedure UnindentSelection;

    procedure InitCompletion;
    procedure DestroyCompletion;

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
  main, project, MultiLangSupport, devcfg, utils,
  datamod, GotoLineFrm, Macros, debugreader, CodeCompletionForm, SynEditMiscClasses;
{$ENDIF}
{$IFDEF LINUX}
  Xlib, main, project, MultiLangSupport, devcfg, Search_Center, utils,
  datamod, GotoLineFrm, Macros;
{$ENDIF}

{ TDebugGutter }

constructor TDebugGutter.Create(editor : TEditor);
begin
	inherited Create(editor.Text);
	e := editor;
end;

procedure TDebugGutter.AfterPaint(ACanvas: TCanvas; const AClip: TRect;FirstLine, LastLine: integer);
begin
	e.DebugAfterPaint(ACanvas,AClip,FirstLine,LastLine);
end;

procedure TDebugGutter.LinesInserted(FirstLine, Count: integer);
var
	I : integer;
	bp : PBreakPoint;
begin
	for I := 0 to MainForm.fDebugger.BreakPointList.Count - 1 do begin
		bp := PBreakPoint(MainForm.fDebugger.BreakPointList.Items[I]);
		if (integer(bp^.editor) = integer(e)) and (bp^.line >= FirstLine) then
			Inc(bp^.line,Count);
	end;
end;

procedure TDebugGutter.LinesDeleted(FirstLine, Count: integer);
var
	I : integer;
	bp : PBreakPoint;
begin
	for I := MainForm.fDebugger.BreakPointList.Count - 1 downto 0 do begin
		bp := PBreakPoint(MainForm.fDebugger.BreakPointList.Items[I]);
		if (integer(bp^.editor) = integer(e)) and (bp^.line >= FirstLine) then
			if (bp^.line >= FirstLine + Count) then
				Dec(bp^.line,Count)
			else
				e.ToggleBreakPoint(bp^.line); // remove breakpoints INSIDE deleted selection
	end;
end;

{ TEditor }

procedure TEditor.Init(InProject : boolean;const Caption, Filename : AnsiString;DoOpen : boolean;IsRes: boolean = FALSE);
var
	s: AnsiString;
begin
	// Set generic options
	fErrorLine:= -1;
	fActiveLine:= -1;
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

	// Create an editor and set static options
	fText := TSynEdit.Create(fTabSheet);

	// Load the file using Lines
	if DoOpen then begin
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

	fText.UseCodeFolding := true;
	fText.Parent := fTabSheet;
	fText.Visible := True;
	fText.Align := alClient;
	fText.PopupMenu := MainForm.EditorPopupMenu;
	fText.WantTabs := True;
	fText.ShowHint := True;
	fText.OnStatusChange:= EditorStatusChange;
	fText.OnReplaceText:= EditorReplaceText;
	fText.OnDropFiles:= EditorDropFiles;
	fText.OnDblClick:= EditorDblClick;
	fText.OnMouseUp := EditorMouseUp;
	fText.OnMouseMove := EditorMouseMove;
	fText.OnGutterClick := EditorGutterClick;
	fText.OnSpecialLineColors := EditorSpecialLineColors;
	fText.OnExit := EditorExit;
	fText.OnPaintTransient := EditorPaintTransient;
	fText.MaxScrollWidth:=4096; // bug-fix #600748
	fText.MaxUndo:=4096;
	fText.BorderStyle:=bsNone;

	fText.Gutter.LeftOffset := 4;
	fText.Gutter.RightOffset := 21;
	fText.Gutter.BorderStyle := gbsNone;

	// Set the variable options
	devEditor.AssignEditor(self.fText,self.fFileName);

	// Create a gutter
	fDebugGutter := TDebugGutter.Create(self);

	// Initialize code completion stuff
	InitCompletion;

	// Function parameter tips
	fFunctionTip := TCodeToolTip.Create(Application);
	fFunctionTip.Editor := FText;
	fFunctionTip.Parser := MainForm.CppParser;

	// Setup a monitor which keeps track of outside-of-editor changes
	MainForm.devFileMonitor.Files.Add(fFileName);
	MainForm.devFileMonitor.Refresh(True);

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
	I,curactive: integer;
begin
	// Deactivate the file change monitor
	I := MainForm.devFileMonitor.Files.IndexOf(fFileName);
	if I <> -1 then begin
		MainForm.devFileMonitor.Files.Delete(I);
		MainForm.devFileMonitor.Refresh(False);
	end;

	// Destroy any completion stuff
	DestroyCompletion;

	// Free everything
	fFunctionTip.Free;
	fText.Free;

	// Delete breakpoints in this editor
	MainForm.fDebugger.DeleteBreakPointsOf(self);

	// Open up the previous tab, not the first one...
	with fTabSheet.PageControl do begin
		curactive := MainForm.PageControl.ActivePageIndex;
		fTabSheet.Free; // resets activepageindex to 0...
		ActivePageIndex := max(0,curactive-1);
	end;

	inherited;
end;

procedure TEditor.OnMouseOverEvalReady(const evalvalue : AnsiString);
begin
	fText.Hint := fCurrentEvalWord + ' = ' + evalvalue;
	MainForm.fDebugger.OnEvalReady := nil;
end;

procedure TEditor.Activate;
begin
	fTabSheet.PageControl.Show;
	fTabSheet.PageControl.ActivePage := fTabSheet;

	if fText.Visible then
		fText.SetFocus;
	if MainForm.ClassBrowser.Enabled then
		MainForm.PageControlChange(MainForm.PageControl); // this makes sure that the classbrowser is consistent
end;

procedure TEditor.EditorGutterClick(Sender: TObject; Button: TMouseButton;x, y, Line: integer; mark: TSynEditMark);
begin
	// Toggle breakpoint at line
	ToggleBreakPoint(Line);
end;

procedure TEditor.ToggleBreakpoint(Line: integer);
var
	thisbreakpoint : integer;
begin
	thisbreakpoint := HasBreakPoint(Line);

	if thisbreakpoint <> -1 then
		MainForm.fDebugger.RemoveBreakPoint(Line,self)
	else
		MainForm.fDebugger.AddBreakPoint(Line,self);

	fText.InvalidateGutterLine(line);
	fText.InvalidateLine(line);
end;

function TEditor.HasBreakPoint(Line : integer): integer;
var
	I : integer;
begin
	result:= -1;
	for I := 0 to MainForm.fDebugger.BreakPointList.Count - 1 do
		if integer(PBreakPoint(MainForm.fDebugger.BreakPointList.Items[I])^.editor) = integer(self) then
			if PBreakPoint(MainForm.fDebugger.BreakPointList.Items[I])^.line = Line then begin
				Result := I;
 				break;
			end;
end;

procedure TEditor.EditorSpecialLineColors(Sender: TObject; Line: Integer;var Special: Boolean; var FG, BG: TColor);
var
	pt: TPoint;
begin
	if (Line = fActiveLine) then begin
		StrtoPoint(pt, devEditor.Syntax.Values[cABP]);
		BG:= pt.X;
		FG:= pt.Y;
		Special:= TRUE;
	end else if (HasBreakpoint(Line) <> -1) then begin
		StrtoPoint(pt, devEditor.Syntax.Values[cBP]);
		BG:= pt.X;
		FG:= pt.Y;
		Special := TRUE;
	end else if Line = fErrorLine then begin
		StrtoPoint(pt, devEditor.Syntax.Values[cErr]);
		BG:= pt.X;
		FG:= pt.Y;
		Special:= TRUE;
	end;
end;

procedure TEditor.DebugAfterPaint(ACanvas: TCanvas; AClip: TRect;FirstLine, LastLine: integer);
var
	X, Y, I: integer;
begin
	X := (fText.Gutter.RealGutterWidth(fText.CharWidth) - fText.Gutter.RightOffset) div 2 - 3;
	Y := (fText.LineHeight - dmMain.GutterImages.Height) div 2 + fText.LineHeight * (FirstLine - fText.TopLine);

	for I := FirstLine to LastLine do begin
		if fActiveLine = I then // prefer active line over breakpoints
			dmMain.GutterImages.Draw(ACanvas, X, Y, 1)
		else if HasBreakpoint(I) <> -1 then
			dmMain.GutterImages.Draw(ACanvas, X, Y, 0)
		else if fErrorLine = I then
			dmMain.GutterImages.Draw(ACanvas, X, Y, 2);

		Inc(Y, fText.LineHeight);
	end;
end;

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
		for I := 0 to aFiles.Count - 1 do begin
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
		if fText.CaretY < fText.Lines.Count then begin
			fText.BlockBegin := BufferCoord(1, fText.CaretY);
			fText.BlockEnd := BufferCoord(1, fText.CaretY + 1);
		end else begin
			fText.BlockBegin := BufferCoord(1, fText.CaretY);
			fText.BlockEnd := BufferCoord(Length(fText.Lines[fText.CaretY-1])+1, fText.CaretY);
		end;
	end;
end;

procedure TEditor.EditorReplaceText(Sender: TObject; const aSearch,aReplace: AnsiString; Line, Column: integer; var Action: TSynReplaceAction);
var
	pt : TPoint;
begin
	pt:= fText.ClienttoScreen(fText.RowColumnToPixels(DisplayCoord(Column, Line +1)));
	MessageBeep(MB_ICONQUESTION);
	case MessageDlgPos(format(Lang[ID_MSG_SEARCHREPLACEPROMPT], [aSearch]), mtConfirmation, [mbYes, mbNo, mbCancel, mbAll], 0, pt.x, pt.y+fText.LineHeight) of
		mrYes: Action := raReplace;
		mrNo: Action := raSkip;
		mrCancel: Action := raCancel;
		mrAll: Action := raReplaceAll;
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
			UpdateCaption('[*] ' + ExtractfileName(fFileName));
		end else begin
			MainForm.SetStatusbarMessage('');
			UpdateCaption(ExtractfileName(fFileName));
		end;
	end;

	// Don't allow mouseover tips when scrolling
	if scTopLine in Changes then begin
		fAllowMouseOver := false;
	end;

	// scSelection includes anything caret related
	if scSelection in Changes then begin
		MainForm.SetStatusbarLineCol;

		// Reset mouseover timer if caret changes
		fAllowMouseOver := false;

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
				fErrorLine:= -1;
			end;
		end else
			fIgnoreCaretChange := false;
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

procedure TEditor.GotoLine;
var
	GotoForm: TGotoLineForm;
begin
	GotoForm := TGotoLineForm.Create(nil);
	try
		if GotoForm.ShowModal = mrOK then
			fText.CaretXY := BufferCoord(1, Min(GotoForm.Line.Value,fText.Lines.Count));

		Activate;
	finally
		GotoForm.Close;
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
	fText.CaretXY := NewCursorPos;
	fText.EnsureCursorPosVisible; // not needed?
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
	fText.CaretXY := BufferCoord(Col, fErrorLine);
	fText.EnsureCursorPosVisible;

	// Redraw new error line
	fText.InvalidateGutterLine(fErrorLine);
	fText.InvalidateLine(fErrorLine);
end;

procedure TEditor.SetActiveBreakpointFocus(Line: integer);
begin
	if Line <> fActiveLine then begin

		// Disable previous active focus
		if fActiveLine <> -1 then begin
			fText.InvalidateGutterLine(fActiveLine);
			fText.InvalidateLine(fActiveLine);
		end;

		fActiveLine := Line;

		fText.CaretXY := BufferCoord(1, fActiveLine);
		fText.EnsureCursorPosVisible;

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
		fActiveLine:= -1;
	end;
end;

procedure TEditor.UpdateCaption(const NewCaption: AnsiString);
begin
	if assigned(fTabSheet) then
		fTabSheet.Caption:= NewCaption;
	MainForm.UpdateAppTitle;
end;

procedure TEditor.SetFileName(const value: AnsiString);
begin
  if value <> fFileName then
   begin
     ffileName:= value;
     UpdateCaption(ExtractfileName(fFileName));
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

procedure TEditor.CompletionKeyPress(Sender: TObject; var Key: Char);
begin
	// We received a key from the completion box...
	if fCompletionBox.Enabled then begin

		// The completion form is already shown
		case Key of
			Char(VK_BACK): begin
				if fText.SelStart > 0 then begin
					fText.SelStart := fText.SelStart - 1;
					fText.SelEnd := fText.SelStart + 1;
					fText.SelText := '';
					fCompletionBox.Search(nil, CompletionPhrase(fText.CaretXY), fFileName);
				end;
			end;

			// We have finished making a selection
			Char(VK_RETURN),';','(',#9,#32: begin
				SetEditorText(Key);
				fCompletionBox.Hide;
			end;

			// Abort, don't insert anything
			Char(VK_ESCAPE): begin
				fCompletionBox.Hide;
			end;

			// Continue filtering
			else begin
				fText.SelText := Key;
				fCompletionBox.Search(nil, CompletionPhrase(fText.CaretXY), fFileName);
			end;
		end;
	end;
end;

procedure TEditor.EditorKeyPress(Sender: TObject; var Key: Char);
var
	P: TPoint;
	allowcompletion : boolean;
	indent : integer;
	attr : TSynHighlighterAttributes;
	s1,s2 : AnsiString;
	Ptr : PAnsiChar;
begin

	// Doing this here instead of in EditorKeyDown to be able to delete some key messages
	if devEditor.CompleteSymbols and not fText.SelAvail then begin

		// Don't complete symbols inside strings or comments
		allowcompletion := true;
		if fText.GetHighlighterAttriAtRowCol(BufferCoord(fText.CaretX-1,fText.CaretY), s1, attr) then
			if (attr = fText.Highlighter.StringAttribute) or (attr = fText.Highlighter.CommentAttribute) or SameStr(attr.Name,'Character') then
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

				s1 := Copy(fText.LineText,1,fText.CaretX-1);
				s2 := Trim(s1);

				if EndsStr(')',s2) or
					EndsStr('else',s2) or
					EndsStr('try',s2) or
					EndsStr('catch',s2) or
					EndsStr('default',s2) or
					EndsStr('do',s2) or
					StartsStr('default',s2) or
					SameStr('',s2) then begin

					// Copy indentation
					indent := 0;
					Ptr := PAnsiChar(s1);
					while Ptr^ in [#1..#32] do begin
						Inc(indent);
						Inc(Ptr);
					end;

					// { + enter + indent + }
					InsertString('{' + #13#10 + Copy(s1,1,indent) + '}',false);
					fText.CaretXY := BufferCoord(fText.CaretX + 1,fText.CaretY);
					Key:=#0;
				end else if StartsStr('struct',  s2) or
							StartsStr('union',   s2) or
							StartsStr('class',   s2) or
							StartsStr('enum',    s2) or
							StartsStr('typedef', s2) then begin

					// Copy indentation
					indent := 0;
					Ptr := PAnsiChar(s1);
					while Ptr^ in [#1..#32] do begin
						Inc(indent);
						Inc(Ptr);
					end;

					// { + enter + indent + };
					InsertString('{' + #13#10 + Copy(s1,1,indent) + '};',false);
					fText.CaretXY := BufferCoord(fText.CaretX + 1,fText.CaretY);
					Key:=#0;
				end else if StartsStr('case', s2) then begin

					// Copy indentation
					indent := 0;
					Ptr := PAnsiChar(s1);
					while Ptr^ in [#1..#32] do begin
						Inc(indent);
						Inc(Ptr);
					end;

					// { + enter + indent + tab + break; + enter + }
					InsertString('{' + #13#10 + Copy(s1,1,indent) + #9 + 'break;' + #13#10 + Copy(s1,1,indent) + '}',false);
					fText.CaretXY := BufferCoord(fText.CaretX + 1,fText.CaretY);
					Key:=#0;
				end else begin
					HasCompletedCurly := 2;
					InsertString('}',false);
				end;
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

		// Use a timer to show the completion window when we just typed a few parent-member linking chars
		case Key of
			'.': fCompletionTimer.Enabled:=True;
			'>': if (fText.CaretX > 1) and (Length(fText.LineText) > 1) and (fText.LineText[fText.CaretX-1]='-') then fCompletionTimer.Enabled:=True;
			':': if (fText.CaretX > 1) and (Length(fText.LineText) > 1) and (fText.LineText[fText.CaretX-1]=':') then fCompletionTimer.Enabled:=True;
			' ': begin

				// If Ctrl is down, immediately show completionbox when space is hit
				if CtrlDown then begin

					// Delete space keypress
					Key := #0;

					P := fText.RowColumnToPixels(fText.DisplayXY);
					Inc(P.Y,16);
					fCompletionBox.Position := fText.ClientToScreen(P);

					// Force show
					fCompletionTimer.OnTimer(nil);
				end;
			end;
		end;

		if fCompletionTimer.Enabled then begin
			P := fText.RowColumnToPixels(fText.DisplayXY);
			Inc(P.Y,16);
			fCompletionBox.Position := fText.ClientToScreen(P);
		end;
	end;
end;

procedure TEditor.EditorKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
var
	P : TBufferCoord;
	s : AnsiString;
	attr : TSynHighlighterAttributes;
begin
	// Allow the user to overwrite ending symbols
	if devEditor.CompleteSymbols then begin
		if not (ssShift in Shift) then begin
			Dec(HasCompletedParentheses);
			Dec(HasCompletedArray);
			Dec(HasCompletedCurly);
		end;
	end;

	// Disable completion when user has continued typing
	if fCompletionBox.Enabled then
		fCompletionTimer.Enabled := False;

	// Update the cursor if it is hovering above a keyword and ctrl is pressed
	if (ssCtrl in Shift) and fText.GetPositionOfMouse(p) and fAllowMouseOver then begin

		// Only show info about variables or includes
		if fText.GetHighlighterAttriAtRowCol(p, s, attr) then
			if not (attr = fText.Highlighter.IdentifierAttribute) and not (SameStr(attr.Name,'Preprocessor')) then
				Exit;

		if(fText.GetWordAtRowCol(p) <> '') then begin

			// Handle Ctrl+Click too
			if ssCtrl in Shift then
				fText.Cursor:=crHandPoint
			else
				fText.Cursor:=crIBeam;
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
	s: AnsiString;
	attr: TSynHighlighterAttributes;
begin
	fCompletionTimer.Enabled:=False;

	if(fText.GetHighlighterAttriAtRowCol(BufferCoord(fText.CaretX-1, fText.CaretY), s, attr)) then
		if (attr = fText.Highlighter.StringAttribute) or (attr = fText.Highlighter.CommentAttribute) then
			Exit;

	fCompletionBox.OnKeyPress := CompletionKeyPress;

	M:=TMemoryStream.Create;
	try

		// Scan the file and corresponding header, ignore function bodies
		//if fText.Modified then
		//	MainForm.CppParser.ReParseFile(fFileName,InProject);

		// Scan the current function
		fText.UnCollapsedLines.SaveToStream(M);
		fCompletionBox.CurrentClass:=MainForm.CppParser.FindAndScanBlockAt(fFileName, fText.CaretY, M);
	finally
		M.Free;
	end;

	fCompletionBox.Search(nil, CompletionPhrase(fText.CaretXY), fFileName);
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
	fCompletionBox := MainForm.CodeCompletion;
	fCompletionBox.Enabled := devClassBrowsing.Enabled and devCodeCompletion.Enabled;

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
	end else if Assigned(fFunctionTip) then
		fFunctionTip.ReleaseHandle;

	if devEditor.ParserHints or devData.WatchHint then begin
		if not Assigned(fMouseOverTimer) then
			fMouseOverTimer:=TTimer.Create(nil);
		fMouseOverTimer.Enabled:=True;
		fMouseOverTimer.OnTimer:=MouseOverTimer;
		fMouseOverTimer.Interval:=500;
	end;

	// The other stuff is fully completion dependant
	if fCompletionBox.Enabled then begin
		fCompletionBox.Width:=devCodeCompletion.Width;
		fCompletionBox.Height:=devCodeCompletion.Height;

		if not Assigned(fCompletionTimer) then
			fCompletionTimer:=TTimer.Create(nil);
		fCompletionTimer.Enabled:=False;
		fCompletionTimer.OnTimer:=CompletionTimer;
		fCompletionTimer.Interval:=devCodeCompletion.Delay;
	end;
end;

procedure TEditor.MouseOverTimer(Sender : TObject);
begin
	// If we waited long enough AFTER scrolling, allow mouseovers again
	if not fText.SelAvail and Assigned(fText.Highlighter) then
		fAllowMouseOver := true;
end;

function TEditor.EvaluationPhrase(p : TBufferCoord): AnsiString;
var
	phrasebegin,phraseend,len,braceindent : integer;
	s : AnsiString;
begin
	result := '';

	if (p.Line >= 1) and (p.Line <= fText.Lines.Count) then begin
		s := fText.Lines[p.Line-1];
		len := Length(s);
		if len = 0 then Exit;

		phrasebegin := p.Char;
		phraseend := p.Char;

		// Copy forward until end of identifier
		while((phraseend <= len) and (s[phraseend] in fText.IdentChars)) do
			Inc(phraseend);

		braceindent := 0;

		// Copy backward until some chars
		while(phrasebegin > 1) do begin

			if s[phrasebegin-1] = '[' then begin
				if braceindent = 0 then break;
				Dec(braceindent);
			end else if s[phrasebegin-1] = ']' then begin
				Inc(braceindent);
			end;

			if (s[phrasebegin-1] in fText.IdentChars) or (s[phrasebegin-1] in ['.','&',':','[',']']) then
				Dec(phrasebegin)
			else if (phrasebegin > 2) and (s[phrasebegin-2] = '-') and (s[phrasebegin-1] = '>') then
				Dec(phrasebegin)
			else if (s[phrasebegin-1] = '-') and (s[phrasebegin] = '>') then
				Dec(phrasebegin)
			else
				break;
		end;

		if phraseend > phrasebegin then
			result := Copy(s,phrasebegin,phraseend-phrasebegin);
	end;
end;

function TEditor.CompletionPhrase(P : TBufferCoord): AnsiString;
var
	braceindent,stripptr,len,curpos,arraybegin,arrayend : integer;
begin
	Result := EvaluationPhrase(p);

	len := Length(result);
	curpos := 1;
	braceindent := 0;
	arraybegin := 1;
	arrayend := 1;

	// Strip array bits
	while (curpos <= len) do begin

		if result[curpos] = '[' then begin
			if braceindent = 0 then
				arraybegin := curpos;
			Inc(braceindent);
		end else if result[curpos] = ']' then begin
			Dec(braceindent);
			if braceindent = 0 then begin
				arrayend := curpos;
				break;
			end;
		end;

		Inc(curpos);
	end;

	if arrayend <> arraybegin then
		Delete(Result,arraybegin,arrayend-arraybegin+1);

	// Strip &'s too
	stripptr := 1;
	while (stripptr <= len) and (result[stripptr] = '&') do
		Inc(stripptr);

	if stripptr > 1 then
		Delete(Result,1,stripptr-1);
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

		// replace the selection
		fText.SelText:=Statement^._ScopelessCmd + FuncAddOn;

		// if we added "()" move caret inside parenthesis
		// only if Key<>'.' and Key<>'>'
		// and function takes arguments...
		if (not (Key in ['.', '>'])) and (FuncAddOn<>'') and ( (Length(Statement^._Args)>2) or (Statement^._Args='()') ) then begin
			fText.CaretX:=fText.CaretX-Length(FuncAddOn)+1;

			// immediately activate function hint
			if devEditor.ShowFunctionTip and Assigned(fText.Highlighter) then begin
				fText.SetFocus;
				fFunctionTip.Show;
			end;
		end;
	end;
end;

procedure TEditor.EditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
	s : AnsiString;
	p : TBufferCoord;
	attr : TSynHighlighterAttributes;
	st: PStatement;
	M : TMemoryStream;

	procedure CancelHint;
	begin
		MainForm.fDebugger.OnEvalReady := nil;
		fCurrentWord := '';
		fText.Cursor := crIBeam;
		Application.CancelHint;
		fText.Hint := '';
	end;
begin

	TabSheet.PageControl.Hint := '';

	// If the mouse can be found INSIDE the window
	if fText.GetPositionOfMouse(p) and fAllowMouseOver then begin

		// Only show info about variables and preprocessor stuff
		if fText.GetHighlighterAttriAtRowCol(p, s, attr) then
			if not (attr = fText.Highlighter.IdentifierAttribute) and not SameStr(attr.Name,'Preprocessor') then begin
				CancelHint;
				Exit;
			end;

		s := fText.GetWordAtRowCol(p);

		if Length(s) = 0 then begin
			CancelHint;
			Exit;
		end;

		// Don't rescan the same stuff over and over again (that's slow)
		if (s <> fCurrentWord) then begin

			// Different word? Cancel old now incorrect hint
			CancelHint;
			fCurrentWord := s;

			// Cursor hovering above an identifier? Allow handpoint cursor
			if (ssCtrl in Shift) then
				fText.Cursor:=crHandPoint
			else
				fText.Cursor:=crIBeam;

			// When debugging, evaluate stuff under cursor, and optionally add to watch list
			if MainForm.fDebugger.Executing then begin

				// Add to list
				if devData.WatchHint then
					MainForm.fDebugger.AddWatchVar(s);

				// Evaluate s
				fCurrentEvalWord := EvaluationPhrase(p);
				MainForm.fDebugger.OnEvalReady := OnMouseOverEvalReady;
				MainForm.fDebugger.SendCommand('print',fCurrentEvalWord);

			// Otherwise, parse code and show information about variable
			end else if devEditor.ParserHints and fText.Focused then begin

				//if not fCompletionBox.Parser then begin
				M := TMemoryStream.Create;
				try
					fText.UnCollapsedLines.SaveToStream(M);
					st := MainForm.CppParser.FindStatementOf(fFileName,CompletionPhrase(p),p.Line,M);
				finally
					M.Free;
				end;

				if Assigned(st) then
					fText.Hint := Trim(st^._FullText) + ' - ' + ExtractFileName(st^._FileName) + ' (' + IntToStr(st^._Line) + ') - Ctrl+Click to follow'
				else
					// couldn't find anything? disable hint
					fText.Hint := '';
			end;
		end;
	end;
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

procedure TEditor.CommentSelection;
var
	oldbbegin,oldbend,oldcaret : TBufferCoord;
	localcopy : string;
	CurPos,len : integer;
begin
	oldbbegin := fText.BlockBegin;
	oldbend := fText.BlockEnd;
	oldcaret := fText.CaretXY;

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

	// Move begin of selection
	if oldbbegin.Char > 1 then
		Inc(oldbbegin.Char,2);

	// Move end of selection
	if oldbend.Char > 1 then
		Inc(oldbend.Char,2);

	// Move caret
	if oldcaret.Char > 1 then
		Inc(oldcaret.Char,2);

	fText.CaretXY := oldcaret;
	fText.BlockBegin := oldbbegin;
	fText.BlockEnd := oldbend;

	// Prevent repaints while we're busy
	fText.EndUpdate;

	fText.UpdateCaret;
	fText.Modified := true;
end;

procedure TEditor.UncommentSelection;
var
	oldbbegin,oldbend,oldcaret : TBufferCoord;
	localcopy : AnsiString;
	CurPos,len : integer;
begin
	oldbbegin := fText.BlockBegin;
	oldbend := fText.BlockEnd;
	oldcaret := fText.CaretXY;

	// Prevent repaints while we're busy
	fText.BeginUpdate;

	if fText.BlockBegin.Line <> fText.BlockEnd.Line then begin
		localcopy := fText.SelText;
		CurPos := 1;
		len := Length(localcopy);

		// Delete the first one
		if StrLComp(@localcopy[1],'//',2) = 0 then
			Delete(localcopy,CurPos,2);

		while(CurPos < len) do begin

			// find any enter sequence...
			if(localcopy[CurPos] in [#13,#10]) then begin
				repeat
					Inc(CurPos);
				until (CurPos = len-1) or not (localcopy[CurPos] in [#0..#32]);

				if (CurPos+1 < len) and (StrLComp(@localcopy[CurPos],'//',2) = 0) then
					Delete(localcopy,CurPos,2);
			end;
			Inc(CurPos);
		end;
		fText.SelText := localcopy;
	end else begin

		localcopy := fText.LineText;
		CurPos := 1;
		len := Length(localcopy);

		// Skip spaces
		while((CurPos < len) and (localcopy[CurPos] in [#0..#32])) do
			Inc(CurPos);

		// First nonblank is comment? Remove
		if (CurPos+1 < len) and (StrLComp(@localcopy[CurPos],'//',2) = 0) then begin
			fText.BeginUndoBlock;

			Delete(localcopy,CurPos,2);

			// Move begin of selection
			if oldbbegin.Char = 2 then
				Dec(oldbbegin.Char,1)
			else if oldbbegin.Char > 2 then
				Dec(oldbbegin.Char,2);

			// Move end of selection
			if oldbend.Char = 2 then
				Dec(oldbend.Char,1)
			else if oldbend.Char > 2 then
				Dec(oldbend.Char,2);

			// Move caret
			if oldcaret.Char = 2 then
				Dec(oldcaret.Char,1)
			else if oldcaret.Char > 2 then
				Dec(oldcaret.Char,2);

			fText.LineText := localcopy;

			fText.UndoList.AddChange(crDelete,
				BufferCoord(CurPos,fText.CaretY),
				BufferCoord(CurPos,fText.CaretY),
				'//',smNormal);

			fText.EndUndoBlock;
		end;
	end;

	// Prevent repaints while we're busy
	fText.EndUpdate;

	fText.CaretXY := oldcaret;
	fText.BlockBegin := oldbbegin;
	fText.BlockEnd := oldbend;

	fText.UpdateCaret;
	fText.Modified := true;
end;

procedure TEditor.EditorMouseUp(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
var
	p: TDisplayCoord;
	line,fname,headername: AnsiString;
	walker,start : integer;
	e : TEditor;
begin
	// if ctrl+clicked
	if (ssCtrl in Shift) and (Button = mbLeft) and not fText.SelAvail then begin

		p := fText.PixelsToRowColumn(X,Y);
		if P.Row <= fText.Lines.Count then begin

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

				headername := Copy(line, start, walker-start);

				// assume std:: C++ header
				if (headername[1] = 'c') and (Pos('.h',headername) = 0) then begin
					Delete(headername,1,1); // remove 'c'
					headername := headername + '.h';
				end;

				fname := MainForm.CppParser.GetFullFileName(headername);
				if fname = ExtractFileName(fname) then // no path info, so prepend path of active file
					fname:=ExtractFilePath(fFileName)+fname;

				// refer to the editor of the filename (will open if needed and made active)
				e:=MainForm.GetEditorFromFileName(fname);
				if Assigned(e) then
					e.GotoLineNr(1);
			end else
				MainForm.actGotoImplDeclEditorExecute(self);
		end;
	end;
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

end.

