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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, CodeCompletion, CppParser, SynExportTeX, SynEditExport, SynExportRTF,
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
    fPreviousTabs : TList; // list of editor pointers
    fDblClickTime : Cardinal;
    fDblClickMousePos : TBufferCoord;

    fCompletionTimer: TTimer;
    fCompletionBox: TCodeCompletion;
    fFunctionTipTimer : TTimer;
    fFunctionTip: TCodeToolTip;
    fMouseOverTimer : TTimer;
    fAllowMouseOver : boolean;

    // These closing chars have already been typed, ignore manual closings
    WaitForParenths : integer; // 2 means true, 1 means 'handled by KeyDown', 0 means false
    WaitForArray : integer;
    WaitForCurly : integer;

    procedure EditorKeyPress(Sender: TObject; var Key: Char);
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditorKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditorDblClick(Sender: TObject);
    procedure EditorClick(Sender: TObject);
    procedure EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure EditorReplaceText(Sender: TObject;const aSearch, aReplace: AnsiString; Line, Column: integer;var Action: TSynReplaceAction);
    procedure EditorDropFiles(Sender: TObject; x, y: integer;aFiles: TStrings);
    procedure EditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure EditorExit(Sender : TObject);
    procedure EditorGutterClick(Sender: TObject; Button: TMouseButton;x, y, Line: integer; mark: TSynEditMark);
    procedure EditorSpecialLineColors(Sender: TObject; Line: integer;var Special: boolean; var FG, BG: TColor);

    procedure CompletionKeyPress(Sender: TObject; var Key: Char);

    procedure MouseOverTimer(Sender : TObject);

    procedure CompletionInsert(const append: AnsiString);
    procedure CompletionTimer(Sender: TObject);

    function FunctionTipAllowed : boolean;
    procedure FunctionTipTimer(Sender : TObject);
    function HandpointAllowed(var mousepos : TBufferCoord) : boolean;

    procedure SetFileName(const value: AnsiString);
    procedure EditorPaintTransient(Sender: TObject; Canvas: TCanvas; TransientType: TTransientType);
  public
    constructor Create(InProject : boolean;const Caption, Filename : AnsiString;DoOpen : boolean;IsRes: boolean = FALSE);
    destructor Destroy; override;

    // Recoded breakpoint stuff
    function HasBreakPoint(Line : integer): integer;
    procedure ToggleBreakPoint(Line : integer);
    procedure DebugAfterPaint(ACanvas: TCanvas; AClip: TRect;FirstLine, LastLine: integer);

    // Debugger callback
    procedure OnMouseOverEvalReady(const evalvalue : AnsiString);

    function Save : boolean;
    function SaveAs : boolean;
  
    procedure Activate;
    procedure GotoLine;
    procedure SetCaretPos(line,col : integer;settopline : boolean = true); // takes folds into account

    procedure ExportToHTML;
    procedure ExportToRTF;
    procedure ExportToTEX;

    procedure InsertString(Value: AnsiString;MoveCursor: boolean);
    procedure SetErrorFocus(Col, Line: integer);
    procedure SetActiveBreakpointFocus(Line: integer);
    procedure RemoveBreakpointFocus;
    procedure UpdateCaption(const NewCaption: AnsiString);
    procedure InsertDefaultText;

    function EvaluationPhrase(p : TBufferCoord): AnsiString;
    function CompletionPhrase(p : TBufferCoord): AnsiString;
    function GetFullFileName(const Line: AnsiString): AnsiString;
  
    procedure CommentSelection;
    procedure UncommentSelection;
    procedure ToggleCommentSelection;
    procedure IndentSelection;
    procedure UnindentSelection;

    procedure InitCompletion;
    procedure DestroyCompletion;

    procedure SetTabIndex(index : integer);

    property PreviousTabs: TList read fPreviousTabs;
    property FileName: AnsiString read fFileName write SetFileName;
    property InProject: boolean read fInProject write fInProject;
    property New: boolean read fNew write fNew;
    property IsRes: boolean read fRes write fRes;
    property Text: TSynEdit read fText write fText;
    property TabSheet: TTabSheet read fTabSheet write fTabSheet;
    property FunctionTip: TCodeToolTip read fFunctionTip;
    property CompletionBox: TCodeCompletion read fCompletionBox;
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
	I,line : integer;
	bp : PBreakPoint;

	procedure LinesInsertedList(Items : TListItems);
	var
		I : integer;
	begin
		for I := 0 to Items.Count - 1 do begin
			if SameFileName(e.fFileName,Items[i].SubItems[1]) then begin
				line := StrToIntDef(Items[i].Caption,-1);
				if (line >= FirstLine) then
					Items[i].Caption := IntToStr(line + Count);
			end;
		end;
	end;
begin
	for I := 0 to MainForm.fDebugger.BreakPointList.Count - 1 do begin
		bp := PBreakPoint(MainForm.fDebugger.BreakPointList.Items[I]);
		if (integer(bp^.editor) = integer(e)) and (bp^.line >= FirstLine) then
			Inc(bp^.line,Count);
	end;

	LinesInsertedList(MainForm.CompilerOutput.Items);
	LinesInsertedList(MainForm.ResourceOutput.Items);
	LinesInsertedList(MainForm.FindOutput.Items);
end;

procedure TDebugGutter.LinesDeleted(FirstLine, Count: integer);
var
	I,line : integer;
	bp : PBreakPoint;

	procedure LinesDeletedList(Items : TListItems);
	var
		I : integer;
	begin
		for I := Items.Count - 1 downto 0 do begin
			if SameFileName(e.fFileName,Items[i].SubItems[1]) then begin
				line := StrToIntDef(Items[i].Caption,-1);
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
	for I := MainForm.fDebugger.BreakPointList.Count -1 downto 0 do begin
		bp := PBreakPoint(MainForm.fDebugger.BreakPointList.Items[I]);
		if (integer(bp^.editor) = integer(e)) and (bp^.line >= FirstLine) then begin
			if (bp^.line >= FirstLine + Count) then
				Dec(bp^.line,Count)
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

constructor TEditor.Create(InProject : boolean;const Caption, Filename : AnsiString;DoOpen : boolean;IsRes: boolean = FALSE);
var
	s: AnsiString;
	I: integer;
	e: TEditor;
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

	// Remember previous tabs
	fPreviousTabs := TList.Create;
	if Assigned(MainForm.PageControl.ActivePage) then begin
		e := TEditor(MainForm.PageControl.ActivePage.Tag); // copy list of previous editor
		for I := 0 to e.PreviousTabs.Count - 1 do
			fPreviousTabs.Add(e.PreviousTabs[i]);
		fPreviousTabs.Add(Pointer(e)); // make current editor history too
	end;

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
	fText.PopupMenu := MainForm.EditorPopupMenu;
	fText.WantTabs := True;
	fText.ShowHint := True;
	fText.OnStatusChange:= EditorStatusChange;
	fText.OnReplaceText:= EditorReplaceText;
	fText.OnDropFiles:= EditorDropFiles;
	fText.OnDblClick := EditorDblClick;
	fText.OnClick := EditorClick;
	fText.OnMouseUp := EditorMouseUp;
	fText.OnMouseMove := EditorMouseMove;
	fText.OnGutterClick := EditorGutterClick;
	fText.OnSpecialLineColors := EditorSpecialLineColors;
	fText.OnExit := EditorExit;
	fText.OnPaintTransient := EditorPaintTransient;
	fText.MaxScrollWidth := 4096; // bug-fix #600748
	fText.MaxUndo := 4096;
	fText.BorderStyle := bsNone;

	fText.Gutter.LeftOffset := 4;
	fText.Gutter.RightOffset := 21;
	fText.Gutter.BorderStyle := gbsNone;

	// Set the variable options
	devEditor.AssignEditor(fText,fFileName);

	// Create a gutter
	fDebugGutter := TDebugGutter.Create(self);

	// Function parameter tips
	fFunctionTip := TCodeToolTip.Create(Application);
	fFunctionTip.Editor := fText;
	fFunctionTip.Parser := MainForm.CppParser;

	// Initialize code completion stuff
	InitCompletion;

	// Setup a monitor which keeps track of outside-of-editor changes
	MainForm.devFileMonitor.Files.Add(fFileName);
	MainForm.devFileMonitor.Refresh(True);

	// Set status bar for the first time
	EditorStatusChange(Self,[scInsertMode]);
end;

destructor TEditor.Destroy;
var
	I: integer;
	CurPage : TTabSheet;
	e: TEditor;
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

	// Open up the previously openend tab, not the first one...
	with MainForm.PageControl do begin
		if ActivePage = fTabSheet then begin // this is the current page...
			fTabSheet.Free; // remove old

			// Find the first tab in the history list that is still open
			for I := fPreviousTabs.Count - 1 downto 0 do begin
				e := MainForm.GetEditorFromTag(integer(fPreviousTabs[i]));
				if Assigned(e) then begin
					ActivePageIndex := e.TabSheet.PageIndex;
					break;
				end;
			end;
		end else begin // we are not the active page
			CurPage := ActivePage; // remember active page index
			fTabSheet.Free; // remove old
			ActivePage := CurPage;
		end;
	end;
	fPreviousTabs.Free;

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

	// this makes sure that the classbrowser is consistent
	MainForm.PageControlChange(MainForm.PageControl);
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
	RealLine: integer;
begin
	RealLine := fText.LineToUncollapsedLine(Line);
	if (RealLine = fActiveLine) then begin
		StrtoPoint(pt, devEditor.Syntax.Values[cABP]);
		BG:= pt.X;
		FG:= pt.Y;
		Special:= TRUE;
	end else if (HasBreakpoint(RealLine) <> -1) then begin
		StrtoPoint(pt, devEditor.Syntax.Values[cBP]);
		BG:= pt.X;
		FG:= pt.Y;
		Special := TRUE;
	end else if RealLine = fErrorLine then begin
		StrtoPoint(pt, devEditor.Syntax.Values[cErr]);
		BG:= pt.X;
		FG:= pt.Y;
		Special:= TRUE;
	end;
end;

procedure TEditor.DebugAfterPaint(ACanvas: TCanvas; AClip: TRect;FirstLine, LastLine: integer);
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

procedure TEditor.EditorReplaceText(Sender: TObject; const aSearch,aReplace: AnsiString; Line, Column: integer; var Action: TSynReplaceAction);
var
	pt : TPoint;
begin
	pt:= fText.ClienttoScreen(fText.RowColumnToPixels(DisplayCoord(Column, Line + 1)));
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
	fFunctionTip.ReleaseHandle;
end;

procedure TEditor.EditorStatusChange(Sender: TObject;Changes: TSynStatusChanges);
begin
	// scModified is only fired when the modified state changes
	if scModified in Changes then begin
		if fText.Modified then begin
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

		// Decrement character completion counter
		WaitForParenths := max(0,WaitForParenths - 1);
		WaitForArray := max(0,WaitForArray - 1);
		WaitForCurly := max(0,WaitForCurly - 1);

		// Reset mouseover timer if caret changes
		fAllowMouseOver := false;

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
	Result := not fText.IsScrolling and not fCompletionBox.Enabled and not fText.SelAvail and devEditor.ShowFunctionTip and Assigned(fText.Highlighter) and not fFunctionTip.ForceHide;
end;

procedure TEditor.FunctionTipTimer(Sender : TObject);
begin
	if FunctionTipAllowed then
		fFunctionTip.Show;
end;

procedure TEditor.ExportToHTML;
var
	SynExporterHTML : TSynExporterHTML;
	SaveFileName : AnsiString;
begin
	SynExporterHTML := TSynExporterHTML.Create(nil);
	try
		with TSaveDialog.Create(Application) do try

			Filter := SynExporterHTML.DefaultFilter;
			Title := Lang[ID_NV_EXPORT];
			DefaultExt := HTML_EXT;
			FileName := ChangeFileExt(fFileName,HTML_EXT);
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
	SynExporterRTF : TSynExporterRTF;
	SaveFileName : AnsiString;
begin
	SynExporterRTF := TSynExporterRTF.Create(nil);
	try
		with TSaveDialog.Create(Application) do try

			Filter:= SynExporterRTF.DefaultFilter;
			Title:= Lang[ID_NV_EXPORT];
			DefaultExt := RTF_EXT;
			FileName := ChangeFileExt(fFileName,RTF_EXT);
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
	SynExporterTEX : TSynExporterTEX;
	SaveFileName : AnsiString;
begin
	SynExporterTEX := TSynExporterTEX.Create(nil);
	try
		with TSaveDialog.Create(Application) do try

			Filter:= SynExporterTEX.DefaultFilter;
			Title:= Lang[ID_NV_EXPORT];
			DefaultExt := TEX_EXT;
			FileName := ChangeFileExt(fFileName,TEX_EXT);
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
			SetCaretPos(Line.Value,1);
	finally
		Free;
	end;
end;

procedure TEditor.InsertString(Value: AnsiString; MoveCursor: boolean);
var
	NewCursorPos: TBufferCoord;
	Char,Line,I : integer;
	P : PAnsiChar;
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
	SetCaretPos(fErrorLine,col,false);
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

		SetCaretPos(fActiveLine,1,false);
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
	if Assigned(fTabSheet) then begin // not needed?
		if NewCaption <> fTabSheet.Caption then begin
			fTabSheet.Caption:= NewCaption;
			MainForm.UpdateAppTitle;
		end;
	end;
end;

procedure TEditor.SetFileName(const value: AnsiString);
begin
	if value <> fFileName then begin
		fFileName:= value;
		UpdateCaption(ExtractFileName(fFileName));
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
			InsertString(ParseMacros(tmp.Text), false);
		finally
			tmp.Free;
		end;
	end;
end;

procedure TEditor.SetCaretPos(line, col: integer;settopline : boolean = true);
var
	fold : TSynEditFoldRange;
	collapsedline : integer;
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
		fText.TopLine := collapsedline;

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
			fCompletionBox.Search(CompletionPhrase(fText.CaretXY), fFileName);
		end else if Key = Char(VK_BACK) then begin
			fText.ExecuteCommand(ecDeleteLastChar,#0,nil); // Simulate backspace in editor
			fCompletionBox.Search(CompletionPhrase(fText.CaretXY), fFileName);
		end else if Key = Char(VK_ESCAPE) then begin
			fCompletionBox.Hide;
		end else if (Key in [Char(VK_RETURN),'(']) then begin  // Ending chars, don't insert
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
	P: TPoint;
	allowcompletion : boolean;
	indent : integer;
	attr : TSynHighlighterAttributes;
	s1,s2 : AnsiString;
	Ptr : PAnsiChar;
	HighlightPos : TBufferCoord;
begin

	// Doing this here instead of in EditorKeyDown to be able to delete some key messages
	if devEditor.CompleteSymbols and not fText.SelAvail then begin

		// Don't complete symbols inside strings or comments
		allowcompletion := true;

		// Empty line? Check if we're inside an uncompleted comment block, check last nonblank char
		HighlightPos := BufferCoord(fText.CaretX-1,fText.CaretY);
		while (HighlightPos.Line > 0) and (Length(fText.Lines[HighlightPos.Line-1]) = 0) do
			Dec(HighlightPos.Line);
		HighlightPos.Char := Length(fText.Lines[HighlightPos.Line-1]);

		if fText.GetHighlighterAttriAtRowCol(HighlightPos, s1, attr) then
			if (attr = fText.Highlighter.StringAttribute) or (attr = fText.Highlighter.CommentAttribute) or SameStr(attr.Name,'Character') then
				allowcompletion := false;

		if allowcompletion then begin

			// Check if we typed anything completable...
			if (Key = '(') and devEditor.ParentheseComplete then begin
				InsertString(')',false);
				WaitForParenths := 2;

				// immediately activate function hint
				if FunctionTipAllowed then
					fFunctionTip.Activated := true;
			end else if (Key = ')') and (WaitForParenths > 0) then begin
				fText.CaretXY := BufferCoord(fText.CaretX + 1,fText.CaretY);
				WaitForParenths := 0;
				Key:=#0;
			end else if (Key = '[') and devEditor.ArrayComplete then begin
				InsertString(']',false);
				WaitForArray := 2;
			end else if (Key = ']') and (WaitForArray > 0) then begin
				fText.CaretXY := BufferCoord(fText.CaretX + 1,fText.CaretY);
				WaitForArray := 0;
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
					WaitForCurly := 2;
					InsertString('}',false);
				end;
			end else if (Key = '}') and (WaitForCurly > 0) then begin
				fText.CaretXY := BufferCoord(fText.CaretX + 1,fText.CaretY);
				WaitForCurly := 2;
				Key:=#0;
			end else if (Key = '<') and devEditor.IncludeComplete then begin
				if StartsStr('#include',fText.LineText) then
					InsertString('>',false);
			end else if (Key = '"') and devEditor.IncludeComplete then begin
				if StartsStr('#include',fText.LineText) then
					InsertString('"',false);
			end else begin
				WaitForParenths := 0;
				WaitForArray := 0;
				WaitForCurly := 0;
			end;
		end else begin
			WaitForParenths := 0;
			WaitForArray := 0;
			WaitForCurly := 0;
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
				if IsKeyDown(VK_CONTROL) then begin

					// Delete space keypress
					Key := #0;

					P := fText.RowColumnToPixels(fText.DisplayXY);
					Inc(P.Y,16);
					fCompletionBox.Position := fText.ClientToScreen(P);

					// Force show
					fCompletionTimer.OnTimer(nil);
				end;
			end else
				fCompletionTimer.Enabled := False;
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
	p : TBufferCoord;
begin
	case(Key) of
		VK_CONTROL: begin
			if HandpointAllowed(p) then // Update the cursor if it is hovering above a keyword and ctrl is pressed
				fText.Cursor:=crHandPoint
			else
				fText.Cursor:=crIBeam;
		end;
		VK_ESCAPE: begin // Update function tip
			if ttoHideOnEsc in fFunctionTip.Options then begin
				fFunctionTip.ReleaseHandle;
				fFunctionTip.ForceHide := true;
			end;
		end;
	end;
end;

procedure TEditor.EditorKeyUp(Sender: TObject; var Key: Word;Shift: TShiftState);
begin
	if (Key = VK_CONTROL) then
		fText.Cursor:=crIBeam;
end;

procedure TEditor.CompletionTimer(Sender: TObject);
var
	M: TMemoryStream;
	s: AnsiString;
	attr: TSynHighlighterAttributes;
begin
	fCompletionTimer.Enabled := False;

	// Don't bother scanning inside strings or comments
	if(fText.GetHighlighterAttriAtRowCol(BufferCoord(fText.CaretX-1, fText.CaretY), s, attr)) then
		if (attr = fText.Highlighter.StringAttribute) or (attr = fText.Highlighter.CommentAttribute) then
			Exit;

	fCompletionBox.OnKeyPress := CompletionKeyPress;

	// Scan the current function body
	M := TMemoryStream.Create;
	try
		fText.UnCollapsedLines.SaveToStream(M);
		fCompletionBox.CurrentIndex := MainForm.CppParser.FindAndScanBlockAt(fFileName, fText.CaretY, M)
	finally
		M.Free;
	end;

	fCompletionBox.Search(CompletionPhrase(fText.CaretXY), fFileName);
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
	fCompletionBox.Enabled := devCodeCompletion.Enabled;

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
	end else begin
		fFunctionTip.ReleaseHandle; // hide
		FreeAndNil(fFunctionTipTimer); // remove timer, because we only have a limited number avaiable
	end;

	if devEditor.ParserHints or devData.WatchHint then begin
		if not Assigned(fMouseOverTimer) then
			fMouseOverTimer:=TTimer.Create(nil);
		fMouseOverTimer.Enabled:=True;
		fMouseOverTimer.OnTimer:=MouseOverTimer;
		fMouseOverTimer.Interval:=500;
	end else begin
		FreeAndNil(fMouseOverTimer);
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
	end else begin
		FreeAndNil(fCompletionTimer);
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
	phrasebegin,phraseend,len : integer;
	s : AnsiString;
begin
	result := '';

	if (p.Line >= 1) and (p.Line <= fText.Lines.Count) then begin
		s := fText.Lines[p.Line-1];
		len := Length(s);

		phrasebegin := p.Char - 1;
		phraseend := p.Char - 1;

		// Copy forward until end of identifier, accept array stuff at the end
		while(phraseend + 1 <= len) do begin
			if s[phraseend + 1] = '[' then begin
				if not FindComplement(s,'[',']',phraseend,1) then break;
			end else if (s[phraseend + 1] in fText.IdentChars) then
				Inc(phraseend)
			else
				break;
		end;

		// Copy backward until some chars
		while(phrasebegin > 0) and (phrasebegin <= len) do begin

			// Accept array and function stuff
			if s[phrasebegin] = ']' then begin
				if FindComplement(s,']','[',phrasebegin,-1) then
					Dec(phrasebegin)
				else
					break;
			end else if s[phrasebegin] = ')' then begin
				if FindComplement(s,')','(',phrasebegin,-1) then
					Dec(phrasebegin)
				else
					break;
			end;

			// Accept -> :: & .
			if (s[phrasebegin] in fText.IdentChars) or (s[phrasebegin] in ['.','&',':']) then
				Dec(phrasebegin)
			else if (phrasebegin > 1) and (s[phrasebegin-1] = '-') and (s[phrasebegin] = '>') then
				Dec(phrasebegin,2)
			else
				break;
		end;

		if phraseend > phrasebegin then
			result := Copy(s,phrasebegin + 1,phraseend-phrasebegin);
	end;
end;

function TEditor.CompletionPhrase(P : TBufferCoord): AnsiString;
var
	len,curpos,stripbeg : integer;
begin
	Result := EvaluationPhrase(p);
	len := Length(result);

	// strip array and function bits
	curpos := 1;
	while (curpos <= len) do begin
		if result[curpos] = '[' then begin
			stripbeg := curpos;
			if FindComplement(Result,'[',']',curpos,1) then
				Delete(Result,stripbeg,curpos - stripbeg + 1);
		end else if result[curpos] = '(' then begin
			stripbeg := curpos;
			if FindComplement(Result,'(',')',curpos,1) then
				Delete(Result,stripbeg,curpos - stripbeg + 1);
		end;
		Inc(curpos);
	end;

	// strip pointer bits
	curpos := 1;
	len := Length(result); // length can hanve changed
	while (curpos <= len) and (result[curpos] in ['*','&']) do
		Inc(curpos);
	Delete(Result,1,curpos-1);
end;

function TEditor.GetFullFileName(const Line: AnsiString) : AnsiString;
var
	walker, start: integer;
	filename : AnsiString;
begin
	walker := 0;
	repeat
		Inc(walker);
	until (walker = Length(line)) or (line[walker] in ['<','"']);
	start := walker + 1;

	repeat
		Inc(walker);
	until (walker = Length(line)) or (line[walker] in ['>','"']);

	filename := Copy(line, start, walker-start);
	if Length(filename) = 0 then begin
		result := '';
		exit;
	end;

	// assume std:: C++ header
	if (filename[1] = 'c') and (Pos('.h',filename) = 0) then begin
		Delete(filename,1,1); // remove 'c'
		filename := filename + '.h';
	end;

	result := MainForm.CppParser.GetFullFileName(filename);
	if (Length(result) <= 2) or (result[2] <> ':') then // no full path, so prepend path of active file
		result := ExtractFilePath(fFileName) + filename;
end;

procedure TEditor.CompletionInsert(const append: AnsiString);
var
	Statement: PStatement;
	FuncAddOn: AnsiString;
begin
	Statement := fCompletionBox.SelectedStatement;
	if not Assigned(Statement) then Exit;

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
	if (FuncAddOn<>'') and (Statement^._Args <> '()') and (Statement^._Args <> '(void)') then begin

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
		Exclude(fNewState,sfWaitForDragging);
		fText.StateFlags := fNewState;

		// Select the current line
		if fText.CaretY < fText.Lines.Count then begin
			fText.BlockBegin := BufferCoord(1, fText.CaretY);
			fText.BlockEnd := BufferCoord(1, fText.CaretY + 1);
		end else begin
			fText.BlockBegin := BufferCoord(1, fText.CaretY);
			fText.BlockEnd := BufferCoord(Length(fText.Lines[fText.CaretY-1])+1, fText.CaretY);
		end;
	end;
end;

procedure TEditor.EditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
	s,Line : AnsiString;
	p : TBufferCoord;
	st: PStatement;
	M : TMemoryStream;

	procedure ShowFileHint;
	var
		FileName: AnsiString;
	begin
		FileName := GetFullFileName(fText.Lines[p.Line - 1]);
		if (FileName <> '') and FileExists(FileName) then
			fText.Hint := FileName + ' - Ctrl+Click for more info'
		else
			fText.Hint := '';
	end;

	procedure ShowParserHint;
	begin
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
		end else if devEditor.ParserHints then begin

			// This piece of code changes the parser database, possibly making hints and code completion invalid...
			M := TMemoryStream.Create;
			try
				fText.UnCollapsedLines.SaveToStream(M);
				st := MainForm.CppParser.FindStatementOf(fFileName,CompletionPhrase(p),p.Line,M);
			finally
				M.Free;
			end;

			if Assigned(st) then begin
				fText.Hint := MainForm.CppParser.PrettyPrintStatement(st) + ' - ' + ExtractFileName(st^._FileName) + ' (' + IntToStr(st^._Line) + ') - Ctrl+Click for more info';
				fText.Hint := StringReplace(fText.Hint,'|',#5,[rfReplaceAll]); // vertical bar is used to split up short and long hint versions...
			end else // couldn't find anything? disable hint
				fText.Hint := '';
		end;
	end;

	procedure CancelHint;
	begin
		MainForm.fDebugger.OnEvalReady := nil;

		// disable editor hint
		Application.CancelHint;
		fCurrentWord := '';
		fText.Hint := '';

		// disable page control hint
		MainForm.fCurrentPageHint := '';
		MainForm.PageControl.Hint := '';
	end;
begin

	// If the mouse can be found INSIDE the window
	if HandpointAllowed(p) then begin

		s := fText.GetWordAtRowCol(p);

		// Don't rescan the same stuff over and over again (that's slow)
		if (s <> fCurrentWord) then begin

			// Different word? Cancel old now incorrect hint
			MainForm.fDebugger.OnEvalReady := nil;
			Application.CancelHint;
			fCurrentWord := s;

			// Cursor hovering above an identifier or preprocessor? Allow handpoint cursor
			if (ssCtrl in Shift) then
				fText.Cursor:=crHandPoint
			else
				fText.Cursor:=crIBeam;

			// Either show an include filename or parser info
			Line := Trim(fText.Lines[p.Line -1]);
			if StartsStr('#include',line) or StartsStr('# include',line) then // show filename hint
				ShowFileHint
			else
				ShowParserHint;
		end;
	end else begin
		fText.Cursor:=crIBeam;
		CancelHint;
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
	I,EndLine : integer;
begin
	oldbbegin := fText.BlockBegin;
	oldbend := fText.BlockEnd;
	oldcaret := fText.CaretXY;

	// Prevent repaints while we're busy
	fText.BeginUpdate;
	fText.BeginUndoBlock;

	// Ignore the last line the cursor is placed on
	if oldbend.Char = 1 then
		EndLine := max(oldbbegin.Line-1,oldbend.Line-2)
	else
		EndLine := oldbend.Line-1;

	for I := oldbbegin.Line-1 to EndLine do begin
		fText.Lines[i] := '//' + fText.Lines[i];
		fText.UndoList.AddChange(crInsert,
			BufferCoord(1,i+1),
			BufferCoord(3,i+1),
			'',smNormal);
	end;

	// When grouping similar commands, process one comment action per undo/redo
	fText.UndoList.AddChange(crNothing,BufferCoord(0,0),BufferCoord(0,0),'',smNormal);

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
	fText.EndUndoBlock;
	fText.EndUpdate;

	fText.UpdateCaret;
	fText.Modified := true;
end;

procedure TEditor.UncommentSelection;
var
	oldbbegin,oldbend,oldcaret : TBufferCoord;
	editcopy : AnsiString;
	I,J,EndLine : integer;
begin
	oldbbegin := fText.BlockBegin;
	oldbend := fText.BlockEnd;
	oldcaret := fText.CaretXY;

	// Prevent repaints while we're busy
	fText.BeginUpdate;
	fText.BeginUndoBlock;

	// Ignore the last line the cursor is placed on
	if oldbend.Char = 1 then
		EndLine := max(oldbbegin.Line-1,oldbend.Line-2)
	else
		EndLine := oldbend.Line-1;

	for I := oldbbegin.Line-1 to EndLine do begin
		editcopy := fText.Lines[i];

		// Find // after blanks only
		J := 1;
		while (J+1 <= length(editcopy)) and (editcopy[j] in [#0..#32]) do
			Inc(J);
		if (j+1 <= length(editcopy)) and (editcopy[j] = '/') and (editcopy[j+1] = '/') then begin
			Delete(EditCopy,J,2);
			fText.Lines[i] := editcopy;

			fText.UndoList.AddChange(crDelete,
				BufferCoord(J,i+1),
				BufferCoord(J+2,i+1),
				'//',smNormal);

			// Move begin of selection
			if (I = oldbbegin.Line-1) and (oldbbegin.Char > 1) then
				Dec(oldbbegin.Char,2);

			// Move end of selection
			if (I = oldbend.Line-1) and (oldbend.Char > 1) then
				Dec(oldbend.Char,2);

			// Move caret
			if (I = oldcaret.Line-1) and (oldcaret.Char > 1) then
				Dec(oldcaret.Char,2);
		end;
	end;

	// When grouping similar commands, process one uncomment action per undo/redo
	fText.UndoList.AddChange(crNothing,BufferCoord(0,0),BufferCoord(0,0),'',smNormal);

	fText.CaretXY := oldcaret;
	fText.BlockBegin := oldbbegin;
	fText.BlockEnd := oldbend;

	// Prevent repaints while we're busy
	fText.EndUndoBlock;
	fText.EndUpdate;

	fText.UpdateCaret;
	fText.Modified := true;
end;

procedure TEditor.ToggleCommentSelection;
var
	caretbegin, caretend : TBufferCoord;
	I,EndLine : integer;
begin
	caretbegin := fText.BlockBegin;
	caretend := fText.BlockEnd;

	// Ignore the last line the cursor is placed on
	if caretend.Char = 1 then
		EndLine := max(caretbegin.Line-1,caretbegin.Line-1)
	else
		EndLine := caretend.Line-1;

	// if everything is commented, then uncomment
	for I := caretbegin.Line-1 to EndLine do begin
		if not StartsStr('//',Trimleft(fText.Lines[i])) then begin
			CommentSelection;
			Exit;
		end;
	end;
	UncommentSelection;
end;

procedure TEditor.EditorMouseUp(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
var
	p: TDisplayCoord;
	line,FileName: AnsiString;
	e : TEditor;
begin
	// if ctrl+clicked
	if (ssCtrl in Shift) and (Button = mbLeft) and not fText.SelAvail then begin

		p := fText.PixelsToRowColumn(X,Y);
		if P.Row <= fText.Lines.Count then begin

			// reset the cursor
			fText.Cursor := crIBeam;

			// Try to open the header
			line := Trim(fText.Lines[p.Row-1]);
			if StartsStr('#include',line) then begin
				FileName := GetFullFileName(line);
				e := MainForm.GetEditorFromFileName(FileName);
				if Assigned(e) then
					e.SetCaretPos(1,1);
			end else
				MainForm.actGotoImplDeclEditorExecute(self);
		end;
	end;
end;

procedure TEditor.EditorPaintTransient(Sender: TObject; Canvas: TCanvas;TransientType: TTransientType);
const
	AllChars = ['{','[','(','}',']',')'];
	OpenChars = ['{','[','('];
	CloseChars = ['}',']',')'];
var
	P: TBufferCoord;
	S: AnsiString;
	Pix: TPoint;
	textrect : TRect;
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
			index := fText.RowColToCharIndex(P,false); // TODO: only compute inside first if?
			if fText.SelAvail and (index > fText.SelStart) and (index < fText.SelEnd) then begin // highlighted is inside selection
				Canvas.Brush.Color := fText.SelectedColor.Background;
				Canvas.Font.Color := fText.SelectedColor.Foreground;
			end else if devEditor.HighCurrLine and (P.Line = fText.CaretY) then begin // matching char is inside highlighted line
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
	if (fText.CaretX-1 > 0) and (fText.CaretX-1 <= len) and (fText.LineText[fText.CaretX-1] in AllChars) then
		P := BufferCoord(fText.CaretX-1,fText.CaretY)

	// Or after us?
	else if (fText.CaretX > 0) and (fText.CaretX <= len) and (fText.LineText[fText.CaretX] in AllChars) then
		P := BufferCoord(fText.CaretX,fText.CaretY);

	// Is the OpenChar before/after us highlighted as a symbol (not a comment or something)?
	if fText.GetHighlighterAttriAtRowCol(P, S, Attri) and (Attri = fText.Highlighter.SymbolAttribute) then begin

		Canvas.Brush.Style := bsSolid;
		Canvas.Font.Assign(fText.Font);
		Canvas.Font.Style := Attri.Style;

		// Draw here using this color
		SetRect;
		SetColors;

		// Redraw bracket
		ExtTextOut(Canvas.Handle,Pix.X,Pix.Y,ETO_OPAQUE,@textrect,PChar(S),Length(S),nil);

		// Draw corresponding bracket too
		P := fText.GetMatchingBracketEx(P);
		if (P.Char > 0) and (P.Line > 0) then begin
			S := fText.Lines[P.Line-1][P.Char];

			// Draw here using this color again
			SetRect;
			SetColors;

			// Mimic SynEdit drawing
			ExtTextOut(Canvas.Handle,Pix.X,Pix.Y,ETO_OPAQUE,@textrect,PChar(S),Length(S),nil);
		end;
	end;
	Canvas.Brush.Style := bsSolid;
end;

procedure TEditor.SetTabIndex(index : integer);
begin
	fTabSheet.PageIndex := index;
end;




function TEditor.HandpointAllowed(var mousepos : TBufferCoord) : boolean;
var
	s : AnsiString;
	HLAttr : TSynHighlighterAttributes;
begin
	result := false;

	// Only allow in the text area and don't allow when scrolling
	if fAllowMouseOver and fText.GetPositionOfMouse(mousepos) and (mousepos.Char < fText.GutterWidth) then begin

		// Only allow hand points in highlighted areas
		if fText.GetHighlighterAttriAtRowCol(mousepos, s, HLAttr) then begin

			// Only allow identifiers and preprocessor stuff
			if (HLAttr = fText.Highlighter.IdentifierAttribute) or (Assigned(HLAttr) and SameStr(HLAttr.Name,'Preprocessor')) then begin
				result := true;
			end;
		end;
	end;
end;

function TEditor.Save : boolean;
var
	wa: boolean;
begin
	Result := True;
	if FileExists(fFileName) and (FileGetAttr(fFileName) and faReadOnly <> 0) then begin
		// file is read-only
		if MessageDlg(Format(Lang[ID_MSG_FILEISREADONLY], [fFileName]), mtConfirmation, [mbYes, mbNo], 0) = mrNo then
			Exit;
		if FileSetAttr(fFileName, FileGetAttr(fFileName)-faReadOnly) <> 0 then begin
			MessageDlg(Format(Lang[ID_MSG_FILEREADONLYERROR], [fFileName]), mtError, [mbOk], 0);
			Exit;
		end;
	end;

	wa := MainForm.devFileMonitor.Active;
	MainForm.devFileMonitor.Deactivate;

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

		// Reparse
		MainForm.CppParser.ReParseFile(fFileName,InProject); // don't need to scan for the first time
	end else if fNew then
		Result := SaveAs; // we need a file name, use dialog

	if wa then
		MainForm.devFileMonitor.Activate;
end;

function TEditor.SaveAs : boolean;
begin
	Result := True;
	with TSaveDialog.Create(Application) do try

		Title := Lang[ID_NV_SAVEFILE];
		Filter := BuildFilter([FLT_CS,FLT_CPPS,FLT_HEADS,FLT_RES]);
		Options := Options + [ofOverwritePrompt];

		// select appropriate filter
		if GetFileTyp(fFileName) in [utcHead,utcppHead] then begin
			FilterIndex := 4; // .h
			DefaultExt := 'h';
		end else begin
			if Assigned(MainForm.fProject) then begin
				if MainForm.fProject.Options.useGPP then begin
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

		FileName := fFileName;
		if (fFileName <> '') then
			InitialDir := ExtractFilePath(fFileName)
		else if Assigned(MainForm.fProject) then
			InitialDir := MainForm.fProject.Directory;

		if Execute then begin
			try
				fText.UnCollapsedLines.SaveToFile(FileName);
				fText.Modified := false;
				fNew := false;
			except
				MessageDlg(Lang[ID_ERR_SAVEFILE] + '"' + FileName + '"', mtError, [mbOk], 0);
				Result := False;
			end;

			if assigned(MainForm.fProject) then
				MainForm.fProject.SaveUnitAs(MainForm.fProject.Units.IndexOf(fFileName), FileName) // index of old filename
			else
				fTabSheet.Caption:= ExtractFileName(FileName);

			fFileName := FileName;
			MainForm.UpdateAppTitle;

			// We haven't scanned it yet...
			MainForm.CppParser.AddFileToScan(FileName);
			MainForm.CppParser.ParseList;
		end else
			Result := False;
	finally
		Free;
	end;
end;

end.

