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
  SynCompletionProposal, StrUtils, SynEditTypes, SynEditHighlighter, CodeToolTip, SynAutoIndent;
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
    fFileName: string;
    fNew: boolean;
    fRes: boolean;
    fModified: boolean;
    fText: TSynEdit;
    fTabSheet: TTabSheet;
    fErrorLine: integer;
    fActiveLine: integer;
    fErrSetting: boolean;
    fDebugGutter: TDebugGutter;
    fOnBreakPointToggle: TBreakpointToggleEvent;
    fHintTimer: TTimer;
    fCurrentHint: string;
    fCurrentWord: string;
    //////// CODE-COMPLETION - mandrav /////////////
    fCompletionEatSpace: boolean;
    fTimer: TTimer;
    fTimerKey: Char;
    fCompletionBox: TCodeCompletion;
    fRunToCursorLine: integer;
    fLastParamFunc : TList;
    FCodeToolTip: TCodeToolTip;//** Modified by Peter **}
    FLastPos : TBufferCoord;
    FAutoIndent: TSynAutoIndent;
    procedure CompletionTimer( Sender: TObject );
    procedure EditorKeyPress( Sender: TObject; var Key: Char );
    procedure EditorKeyDown( Sender: TObject; var Key: Word; Shift: TShiftState );
    procedure EditorKeyUp( Sender: TObject; var Key: Word; Shift: TShiftState );
    procedure EditorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SetEditorText(Key: Char);
    procedure InitCompletion;
    procedure DestroyCompletion;
    function CurrentPhrase: string;
    function CheckAttributes(P: TBufferCoord; Phrase: string): boolean;
    //////// CODE-COMPLETION - mandrav - END ///////
    function GetModified: boolean;
    procedure SetModified(value: boolean);

    // RNC 07.02.2004 -- new methods, explaination at definition
    procedure TurnOffBreakpoint(line: integer);
    procedure TurnOnBreakpoint(line: integer);

    procedure EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure EditorSpecialLineColors(Sender: TObject; Line: integer;var Special: boolean; var FG, BG: TColor);
    procedure EditorGutterClick(Sender: TObject; Button: TMouseButton;x, y, Line: integer; mark: TSynEditMark);
    procedure EditorReplaceText(Sender: TObject;const aSearch, aReplace: string; Line, Column: integer;var Action: TSynReplaceAction);
    procedure EditorDropFiles(Sender: TObject; x, y: integer;aFiles: TStrings);
    procedure EditorDblClick(Sender: TObject);
    procedure EditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure EditorHintTimer(sender : TObject);

    procedure SetFileName(value: string);
    procedure DrawGutterImages(ACanvas: TCanvas; AClip: TRect;FirstLine, LastLine: integer);
    procedure EditorPaintTransient(Sender: TObject; Canvas: TCanvas; TransientType: TTransientType);
   public
    procedure Init(In_Project : boolean; Caption_, File_name : string; DoOpen : boolean; const IsRes: boolean = FALSE);
    destructor Destroy; override;
    // RNC set the breakpoints for this file when it is opened
    procedure SetBreakPointsOnOpen;
    // RNC 07-21-04
    // Add remove a breakpoint without calling OnBreakpointToggle
    function HasBreakPoint(line_number: integer): integer;
    procedure InsertBreakpoint(line: integer);
    procedure RemoveBreakpoint(line: integer);
    procedure InvalidateGutter;

    procedure Activate;
    function ToggleBreakPoint(Line: integer): boolean;
    procedure RunToCursor(Line : integer);
    procedure GotoLine;
    procedure GotoLineNr(Nr: integer);
    function Search(const isReplace: boolean): boolean;
    procedure SearchAgain;
    procedure Exportto(const isHTML: boolean);
    procedure InsertString(const Value: string; const move: boolean);
    function GetWordAtCursor: string;
    procedure SetErrorFocus(const Col, Line: integer);
    procedure SetActiveBreakpointFocus(const Line: integer);
    procedure RemoveBreakpointFocus;
    procedure UpdateCaption(NewCaption: string);
    procedure InsertDefaultText;
    procedure PaintMatchingBrackets(TransientType: TTransientType);

    procedure CommentSelection;
    procedure UncommentSelection;
    procedure IndentSelection;
    procedure UnindentSelection;

    procedure UpdateParser; // Must be called after recreating the parser
    //////// CODE-COMPLETION - mandrav /////////////
    procedure ReconfigCompletion;
    //////// CODE-COMPLETION - mandrav /////////////

    property OnBreakpointToggle: TBreakpointToggleEvent read fOnBreakpointToggle write fOnBreakpointToggle;
    property FileName: string read fFileName write SetFileName;
    property InProject: boolean read fInProject write fInProject;
    property New: boolean read fNew write fNew;
    property Modified: boolean read GetModified write SetModified;
    property IsRes: boolean read fRes write fRes;
    property Text: TSynEdit read fText write fText;
    property TabSheet: TTabSheet read fTabSheet write fTabSheet;
    property CodeToolTip: TCodeToolTip read FCodeToolTip; // added on 23rd may 2004 by peter_
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

procedure TEditor.Init(In_Project : boolean; Caption_, File_name : string;DoOpen : boolean; const IsRes: boolean = FALSE);
var
	s: string;
	pt: TPoint;
begin
	// Set generic options
	fModified := false;
	fErrorLine:= -1;
	fActiveLine:= -1;
	fRunToCursorLine := -1;
	fRes:= IsRes;
	fInProject := In_Project;
	fLastParamFunc := nil;
	if File_name = '' then
		fFileName := Caption_
	else
		fFileName := File_name;

	// Setup a new tab
	fTabSheet := TTabSheet.Create(MainForm.PageControl);
	fTabSheet.Caption := Caption_;
	fTabSheet.PageControl := MainForm.PageControl;
	fTabSheet.BorderWidth := 0;
	fTabSheet.Tag := integer(self); // Define an index for each tab

	// Set breakpoint events
	fOnBreakpointToggle := MainForm.OnBreakpointToggle;

	// Create an editor
	fText := TSynEdit.Create(fTabSheet);

	// Load the file using lines, we're not using CodeFolding here yet
	if (DoOpen) then begin
		try
			fText.Lines.LoadFromFile(FileName);
			fNew := False;
			if devData.Backups then begin
				s:= ExtractfileExt(FileName);
				Insert('~', s, AnsiPos('.', s) +1);
				Delete(s, Length(s) -1, 1);
				if devEditor.AppendNewline then
					with fText do
						if Lines.Count > 0 then
							if Lines[Lines.Count -1] <> '' then
								Lines.Add('');
				fText.Lines.SaveToFile(ChangeFileExt(FileName, s));
			end;
		except
			raise;
		end
	end else
		fNew := True;

	// Create a tooltip timer
	fHintTimer := TTimer.Create(Application);
	fHintTimer.Interval := 500;
	fHintTimer.OnTimer := EditorHintTimer;
	fHintTimer.Enabled := false;

	// Set a whole lot of data
	fText.Parent := fTabSheet;
	fText.Align := alClient;
	fText.Visible := True;
	fText.PopupMenu := MainForm.EditorPopupMenu;
	fText.WantTabs := True;
	fText.OnStatusChange:= EditorStatusChange;
	fText.OnSpecialLineColors:= EditorSpecialLineColors;
	fText.OnGutterClick:= EditorGutterClick;
	fText.OnReplaceText:= EditorReplaceText;
	fText.OnDropFiles:= EditorDropFiles;
	fText.OnDblClick:= EditorDblClick;
	fText.OnMouseDown := EditorMouseDown;
	fText.OnMouseMove := EditorMouseMove;
	fText.OnPaintTransient := EditorPaintTransient;
	fText.MaxScrollWidth:=4096; // bug-fix #600748
	fText.MaxUndo:=4096;
	fText.BorderStyle:=bsNone;

	// Initialise code folding
	with fText.CodeFolding do begin
		Enabled := True;
		IndentGuides := True;
		CaseSensitive := True;
		FolderBarColor := clBtnFace;
		FolderBarLinesColor := clBlack;
		CollapsingMarkStyle := TSynCollapsingMarkStyle(0);

		with FoldRegions do begin
			Add(rtChar, False, False, False, '{', '}', nil);
			Add(rtKeyword, True, False, False, '/*', '*/', nil);
			Add(rtKeyword, False, False, True, 'BEGIN', 'END', nil);
		end;
	end;

	// Apply folding for the first time
	fText.InitCodeFolding;
	fText.ReScanForFoldRanges;
	fText.GetUncollapsedStrings;

	// Set the current editor and highlighter
	devEditor.AssignEditor(fText);
	if not fNew then
		fText.Highlighter:= dmMain.GetHighlighter(fFileName)
	else if fRes then
		fText.Highlighter:= dmMain.Res
	else
		fText.Highlighter:= dmMain.cpp;

	// Set the text color
	StrtoPoint(pt, devEditor.Syntax.Values[cSel]);
	fText.SelectedColor.Background:= pt.X;
	fText.SelectedColor.Foreground:= pt.Y;

	// Create a gutter
	fDebugGutter := TDebugGutter.Create(self);

	// Initialize code completion
	InitCompletion;

	// Function parameter tips
	FCodeToolTip := TCodeToolTip.Create(Application);
	FCodeToolTip.Editor := FText;
	FCodeToolTip.Parser := MainForm.CppParser;

	// The Editor must have 'Auto Indent' activated  to use FAutoIndent.
	// It's under Tools >> Editor Options and then the General tab
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
	fCodeTooltip.Free;
	fAutoIndent.Free;
	fHintTimer.Free;
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
	if assigned(fTabSheet) then begin
		fTabSheet.PageControl.Show;
		fTabSheet.PageControl.ActivePage := fTabSheet;

		if fText.Visible then
			fText.SetFocus;
		if MainForm.ClassBrowser1.Enabled then
			MainForm.PageControlChange(MainForm.PageControl); // this makes sure that the classbrowser is consistent
	end;
end;

function TEditor.GetModified: boolean;
begin
  result:= fModified or fText.Modified;
end;

procedure TEditor.SetModified(value: boolean);
begin
  fModified:= value;
  fText.Modified:= Value;
end;

// RNC 07-21-04 These functions are used to turn off/on a breakpoint
// without calling RemoveBreakpoint or AddBreakpoint in fDebugger.
// This is helpful when trying to automitically remove a breakpoint
// when a user tries to add one while the debugger is running
procedure TEditor.InsertBreakpoint(line: integer);
begin
  if(line>0) and (line <= fText.UnCollapsedLines.Count) then
    begin
      fText.InvalidateLine(line);
      fText.InvalidateGutterLine(line);
    end;
end;

procedure TEditor.RemoveBreakpoint(line: integer);
begin
  if(line > 0) and (line <= fText.UnCollapsedLines.Count) then
    begin
      fText.InvalidateLine(line);
      // RNC new synedit stuff
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
procedure TEditor.EditorDropFiles(Sender: TObject; x, y: integer;
  aFiles: TStrings);
var
 sl: TStringList;
 idx, idx2: integer;
begin
  if devEditor.InsDropFiles then
   begin
     fText.CaretXY:= fText.DisplayToBufferPos(fText.PixelsToRowColumn(x, y));

     sl:= TStringList.Create;
     try
      for idx:= 0 to pred(aFiles.Count) do
       begin
         sl.LoadFromFile(aFiles[idx]);
         fText.SelText:= sl.Text;
       end;
     finally
      sl.Free;
     end;
   end
  else
   for idx:= 0 to pred(aFiles.Count) do
    begin
      idx2:= MainForm.FileIsOpen(aFiles[idx]);
      if idx2 = -1 then
       if GetFileTyp(aFiles[idx]) = utPrj then
        begin
          MainForm.OpenProject(aFiles[idx]);
          exit;
        end
       else
        MainForm.OpenFile(aFiles[idx])
      else
       TEditor(MainForm.PageControl.Pages[idx2].Tag).Activate;
    end;
end;

procedure TEditor.EditorDblClick(Sender: TObject);
begin
  if devEditor.DblClkLine then
   begin
     fText.BlockBegin:= BufferCoord(1, fText.CaretY);
     fText.BlockEnd:= BufferCoord(1, fText.CaretY +1);
   end;
end;

procedure TEditor.EditorGutterClick(Sender: TObject; Button: TMouseButton;
  x, y, Line: integer; mark: TSynEditMark);
begin
  ToggleBreakPoint(Line);
end;

procedure TEditor.EditorReplaceText(Sender: TObject; const aSearch,aReplace: string; Line, Column: integer; var Action: TSynReplaceAction);
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

procedure TEditor.EditorStatusChange(Sender: TObject;Changes: TSynStatusChanges);
begin
	if scModified in Changes then begin // scModified is only fired upon first change
		if Modified then begin
			MainForm.SetStatusbarMessage(Lang[ID_MODIFIED]);
			UpdateCaption('[*] '+ExtractfileName(fFileName));
			fText.ReScanForFoldRanges;
			fText.GetUncollapsedStrings;
		end else begin
			UpdateCaption(ExtractfileName(fFileName));
			MainForm.SetStatusbarMessage('');
		end;
	end;

	// Sluggish, but needed...
	if (scSelection in Changes) then
		MainForm.SetStatusbarLineCol;

	if Changes * [scCaretX, scCaretY] <> [] then begin

		// Prevent loading twice (on CaretX and CaretY change)
		if (fText.CaretX = FLastPos.Char) and (fText.CaretY = FLastPos.Line) then
			Exit;

		// Store old pos
		FLastPos.Char := fText.CaretX;
		FLastPos.Line := fText.CaretY;

		// Only on caret changes...
		if not fErrSetting and (fErrorLine <> -1) then begin
			fText.InvalidateLine(fErrorLine);
			fText.InvalidateGutterLine(fErrorLine);
			fErrorLine:= -1;
			fText.InvalidateLine(fErrorLine);
			fText.InvalidateGutterLine(fErrorLine);
		end;

		if FCodeToolTip.Activated or (not fText.SelAvail) and fText.Focused then
			FCodeToolTip.Show;
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

procedure TEditor.ExportTo(const isHTML: boolean);
begin
  if IsHTML then
   begin
     with dmMain.SaveDialog do
      begin
        Filter:= dmMain.SynExporterHTML.DefaultFilter;
        DefaultExt := HTML_EXT;
        Title:= Lang[ID_NV_EXPORT];
        if Execute then
         begin
           dmMain.ExportToHtml(fText.UnCollapsedLines, dmMain.SaveDialog.FileName);
           fText.BlockEnd:= fText.BlockBegin;
         end;
      end;
   end
  else
   with dmMain.SaveDialog do
    begin
      Filter:= dmMain.SynExporterRTF.DefaultFilter;
      Title:= Lang[ID_NV_EXPORT];
      DefaultExt := RTF_EXT;
      if Execute then
       begin
         dmMain.ExportToRtf(fText.UnCollapsedLines, dmMain.SaveDialog.FileName);
         fText.BlockEnd:= fText.BlockBegin;
       end;
    end;
end;

function TEditor.GetWordAtCursor: string;
begin
  result:= fText.GetWordAtRowCol(fText.CaretXY);
end;

{** Modified by Peter **}
procedure TEditor.GotoLine;
var
 GotoForm: TGotoLineForm;
begin
  GotoForm := TGotoLineForm.Create(FText);
  try
    GotoForm.Editor := FText;
    
    if GotoForm.ShowModal = mrOK then
     FText.CaretXY:= BufferCoord(FText.CaretX, GotoForm.Line.Value);
     
    Activate;
  finally
    GotoForm.Free;
  end;
end;

procedure TEditor.InsertString(const Value: string; const move: boolean);
var
	Line: string;
	idx,idx2: integer;
	pt: TBufferCoord;
	tmp: TStringList;
begin
	if not assigned(fText) then exit;
	pt:= fText.CaretXY;
	tmp:= TStringList.Create;
	try // move cursor to pipe '|', don't do that, might be used by code, use *|* instead
		tmp.Text:= Value;
		if Move then
			for idx:= 0 to pred(tmp.Count) do begin
				Line:= tmp[idx];
				idx2:= AnsiPos('*|*', Line);
				if idx2> 0 then begin
					delete(Line, idx2, 3);
					tmp[idx]:= Line;
					inc(pt.Line, idx);
					if idx = 0 then
						inc(pt.Char, idx2 -1)
					else
						pt.Char:= idx2;

					break;
				end;
			end;
		Line:= tmp.Text;
		Delete(Line,Length(Line)-1,2);
		fText.SelText:= Line;
		fText.CaretXY:= pt;
		fText.EnsureCursorPosVisible;
	finally
		tmp.Free;
	end;
end;

function TEditor.Search(const isReplace: boolean): boolean;
var
 s: string;
begin
  if devEditor.FindText then
   if (fText.SelText = '') then
    s:= GetWordAtCursor
   else
    s:= fText.SelText;

  with SearchCenter do
   begin
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

procedure TEditor.SetErrorFocus(const Col, Line: integer);
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

procedure TEditor.SetActiveBreakpointFocus(const Line: integer);
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

procedure TEditor.UpdateCaption(NewCaption: string);
begin
  if assigned(fTabSheet) then
   fTabSheet.Caption:= NewCaption;
end;

procedure TEditor.SetFileName(value: string);
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
  if FileExists(devDirs.Config + DEV_DEFAULTCODE_FILE) then
   begin
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

//////// CODE-COMPLETION - mandrav /////////////
procedure TEditor.EditorKeyPress(Sender: TObject; var Key: Char);
var
	P: TPoint;
begin
	if Key = char($7F) then // happens when doing ctrl+backspace with completion on
		exit;
	if fCompletionBox.Enabled then begin
		if not (Sender is TForm) then begin // TForm is the code-completion window
			fTimer.Enabled:=False;
			fTimerKey:=Key;
			case Key of
				'.': fTimer.Enabled:=True;
				'>': if (fText.CaretX > 1) and (Length(fText.LineText)>0) and (fText.LineText[fText.CaretX-1]='-') then fTimer.Enabled:=True;
				':': if (fText.CaretX > 1) and (Length(fText.LineText)>0) and (fText.LineText[fText.CaretX-1]=':') then fTimer.Enabled:=True;
				' ': if fCompletionEatSpace then Key:=#0; // eat space if it was ctrl+space (code-completion)
			end;
			P := fText.RowColumnToPixels(fText.DisplayXY);
			P.Y := P.Y + 16;

			P := fText.ClientToScreen(P);
			fCompletionBox.Position:=P;
		end else begin
			case Key of
{$IFDEF WIN32}
				Char(VK_Back): if fText.SelStart > 0 then begin
{$ENDIF}
{$IFDEF LINUX}
				Char(XK_BackSpace): if fText.SelStart > 0 then begin
{$ENDIF}
					fText.SelStart := fText.SelStart - 1;
					fText.SelEnd := fText.SelStart+1;
					fText.SelText := '';
					fCompletionBox.Search(nil, CurrentPhrase, fFileName);
				end;
{$IFDEF WIN32}
				Char(VK_Return): begin
{$ENDIF}
{$IFDEF LINUX}
				Char(XK_Return): begin
{$ENDIF}
					SetEditorText(Key);
					fCompletionBox.Hide;
				end;
				';', '(': begin
					SetEditorText(Key);
					fCompletionBox.Hide;
				end;
				'.', '>', ':': begin
					SetEditorText(Key);
					fCompletionBox.Search(nil, CurrentPhrase, fFileName);
				end;
				else if Key >= ' ' then begin
					fText.SelText := Key;

					fCompletionBox.Search(nil, CurrentPhrase, fFileName);
				end;
			end;
		end;
		fCompletionEatSpace:=False;
	end;
end;

// Looks like the completion stuff doesn't like eoTrimTrailingSpaces at all...
procedure TEditor.EditorKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
var
	M: TMemoryStream;
	attr: TSynHighlighterAttributes;
	s : string;
	allowcompletion : boolean;
	cursorpos : integer;

	// Key localisation
	keystate: TKeyboardState;
	localizedkey : string;
	charlen : integer;
begin
	// Indent/Unindent selected text with TAB key, like Visual C++ ...
{$IFDEF WIN32}
	if Key = VK_TAB then begin
{$ENDIF}
{$IFDEF LINUX}
	if Key = XK_TAB then begin
{$ENDIF}
		if FText.SelText <> '' then begin
			if FText.BlockBegin.Line <> FText.BlockEnd.Line then begin
				if not (ssShift in Shift) then
					FText.ExecuteCommand(ecBlockIndent, #0, nil)
				else
					FText.ExecuteCommand(ecBlockUnindent, #0, nil);
			end;
		end;
	end;

	// Code wordt niet toegevoegd in string, comment en onbekend bestandsformaat...
	if devEditor.AutoCloseBrace then begin

		// Allerhande voorwaarden
		allowcompletion := true;
		fText.GetHighlighterAttriAtRowCol(BufferCoord(fText.CaretX-1,fText.CaretY), s, attr);
		if Assigned(attr) or (Length(fText.LineText) = 0) then
			allowcompletion := true;
		if Assigned(attr) then
			if (attr = fText.Highlighter.StringAttribute) or (attr = fText.Highlighter.CommentAttribute) then
				allowcompletion := false;

		if allowcompletion then begin
			cursorpos:=fText.CaretX;

			// Kijken waar we op klikten
			GetKeyboardState(keystate);
			SetLength(localizedkey,2);

			// yes, you saw that right. I'm calling it twice. Have a look at this for an explanation
			// http://stackoverflow.com/questions/1964614/toascii-tounicode-in-a-keyboard-hook-destroys-dead-keys
			ToAscii(Key,MapVirtualKey(Key,0),keystate,@localizedkey[1],0);
			charlen := ToAscii(Key,MapVirtualKey(Key,0),keystate,@localizedkey[1],0);

			// Skip key combinations
			if charlen = 1 then
				SetLength(localizedkey,1);
			if charlen = 0 then Exit;

			// Check if we typed anything completable...
			if localizedkey = '(' then begin
				InsertString(')',false);
			end else if localizedkey = '[' then begin
				InsertString(']',false);
			end else if localizedkey = '*' then begin
				if (cursorpos > 1) and (fText.LineText[cursorpos-1] = '/') then
					InsertString('*/',false);
			end else if localizedkey = '{' then begin

				// If there's any text before the cursor...
				if (cursorpos > 1) then begin

					// See what the last nonwhite character before the cursor is
					repeat
						Dec(cursorpos);
					until (cursorpos=1) or not (fText.LineText[cursorpos] in [#9,#32]);

					// First check if the user is opening a function
					if (cursorpos > 0) and (fText.LineText[cursorpos] in [')']) or (Pos('else',fText.LineText)=(cursorpos-3)) then begin

						// Check indentation
						cursorpos:=0;
						repeat
							Inc(cursorpos);
						until not (fText.LineText[cursorpos] in [#9,#32]);

						InsertString(#13#10 + Copy(fText.LineText,1,cursorpos-1) + '}',false);
					end else if AnsiStartsStr('struct',TrimLeft(fText.LineText)) or
								AnsiStartsStr('union', TrimLeft(fText.LineText)) or
								AnsiStartsStr('class', TrimLeft(fText.LineText)) or
								AnsiStartsStr('enum',  TrimLeft(fText.LineText)) then begin

						// Check indentation too
						cursorpos:=0;
						repeat
							Inc(cursorpos);
						until not (fText.LineText[cursorpos] in [#9,#32]);

						InsertString(#13#10 + Copy(fText.LineText,1,cursorpos-1) + '};',false);
					end else if AnsiStartsStr('case',TrimLeft(fText.LineText)) then begin

						// Check indentation too
						cursorpos:=0;
						repeat
							Inc(cursorpos);
						until not (fText.LineText[cursorpos] in [#9,#32]);

						InsertString(#13#10 + Copy(fText.LineText,1,cursorpos-1) + #9 + 'break;' + #13#10 + Copy(fText.LineText,1,cursorpos-1) + '}',false);
					end else
						InsertString('}',false);
				end else
					InsertString(#13#10 + '}',false);
			end else if localizedkey = '<' then begin
				if AnsiStartsStr('#include',fText.LineText) then
					InsertString('>',false);
			end else if localizedkey = '"' then begin
				if AnsiStartsStr('#include',fText.LineText) then
					InsertString('"',false);
			end;
		end;
	end;

	if fCompletionBox.Enabled then begin
		fCompletionBox.OnKeyPress:=EditorKeyPress;
		if ssCtrl in Shift then
{$IFDEF WIN32}
			if Key=VK_SPACE then begin
{$ENDIF}
{$IFDEF LINUX}
			if Key=XK_SPACE then begin
{$ENDIF}
				Key:=0;
				if not (ssShift in Shift) then begin
					M:=TMemoryStream.Create;
					try
						fText.UnCollapsedLines.SaveToStream(M);
						fCompletionBox.CurrentClass:=MainForm.CppParser.FindAndScanBlockAt(fFileName, fText.CaretY, M);
					finally
						M.Free;
				end;
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
	curr: string;
begin
	fTimer.Enabled:=False;
	curr:=CurrentPhrase;

	if not CheckAttributes(BufferCoord(fText.CaretX-1, fText.CaretY), curr) then begin
		fTimerKey:=#0;
		Exit;
	end;

	M:=TMemoryStream.Create;
	try
		fText.UnCollapsedLines.SaveToStream(M);
		fCompletionBox.CurrentClass:=MainForm.CppParser.FindAndScanBlockAt(fFileName, fText.CaretY, M);
	finally
		M.Free;
	end;
	case fTimerKey of
	'.':
		fCompletionBox.Search(nil, curr, fFileName);
	'>':
		if fText.CaretX-2>=0 then
			if fText.LineText[fText.CaretX-2]='-' then // it makes a '->'
				fCompletionBox.Search(nil, curr, fFileName);
	':':
		if fText.CaretX-2>=0 then
			if fText.LineText[fText.CaretX-2]=':' then // it makes a '::'
				fCompletionBox.Search(nil, curr, fFileName);
	end;
	fTimerKey:=#0;
end;

procedure TEditor.ReconfigCompletion;
begin
  // re-read completion options
  fCompletionBox.Enabled:=devClassBrowsing.Enabled and devCodeCompletion.Enabled;
  if fCompletionBox.Enabled then
    InitCompletion
  else
    DestroyCompletion;
end;

procedure TEditor.DestroyCompletion;
begin
  if Assigned(fTimer) then
    FreeAndNil(fTimer);
end;

procedure TEditor.InitCompletion;
begin
  fCompletionBox:=MainForm.CodeCompletion;
  fCompletionBox.Enabled:=devCodeCompletion.Enabled;

  fText.OnKeyDown := EditorKeyDown; // This way tabs are also processed without Code Completion!
  if fCompletionBox.Enabled then begin
    fText.OnKeyPress := EditorKeyPress;
    fText.OnKeyUp := EditorKeyUp;
    fCompletionBox.OnKeyPress:=EditorKeyPress;
    fCompletionBox.Width:=devCodeCompletion.Width;
    fCompletionBox.Height:=devCodeCompletion.Height;

    if fTimer=nil then
      fTimer:=TTimer.Create(nil);
    fTimer.Enabled:=False;
    fTimer.OnTimer:=CompletionTimer;
    fTimer.Interval:=devCodeCompletion.Delay;
  end;
  fCompletionEatSpace:=False;
end;

function TEditor.CurrentPhrase: string;
var
  I: integer;
  AllowPar: boolean;
  NestedPar: integer;
begin
  I := fText.CaretX;
  Dec(I, 1);
  NestedPar:=0;
  AllowPar:=((Length(fText.LineText)>1) and (Copy(fText.LineText, I-1, 2) = ').')) or
             (Length(fText.LineText)>2) and (Copy(fText.LineText, I-2, 3) = ')->');
  while (I <> 0) and (fText.LineText <> '') and (fText.LineText[I] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '.', '-', '>', ':', '(', ')', {'[',} ']']) do begin
    if (fText.LineText[I]=')') then begin
      if not AllowPar then
        Break
      else
        Inc(NestedPar);
    end
    else if fText.LineText[I]='(' then begin
      if AllowPar then begin
        if NestedPar>0 then
          Dec(NestedPar)
        else
          AllowPar:=False;
      end
      else
        Break;
    end;
    Dec(I, 1);
  end;
  Result := Copy(fText.LineText, I + 1, fText.CaretX - I - 1);
end;

procedure TEditor.SetEditorText(Key: Char);
var
  Phrase: string;
  I, CurrSel: integer;
  ST: PStatement;
  FuncAddOn: string;
begin
  ST:=fCompletionBox.SelectedStatement;
  if fCompletionBox.SelectedIsFunction then begin
    if Key=';' then
      FuncAddOn := '();'
    else if Key='.' then
      FuncAddOn := '().'
    else if Key='>' then
      FuncAddOn := '()->'
    else
      FuncAddOn := '()';
  end
  else begin
{$IFDEF WIN32}
    if Key=Char(vk_Return) then
{$ENDIF}
{$IFDEF LINUX}
    if Key=Char(Xk_Return) then
{$ENDIF}
      FuncAddOn := ''
    else if Key='>' then
      FuncAddOn := '->'
    else if Key=':' then
      FuncAddOn := '::'
    else
      FuncAddOn := Key;
  end;

  if ST <> nil then begin
    Phrase := ST^._Command;

    // if already has a selection, delete it
    if fText.SelAvail then
      fText.SelText:='';

    // find the end of the word and delete from caretx to end of word
    CurrSel:=fText.SelEnd;
    I:=CurrSel;
    while (I<Length(fText.Text)) and (fText.Text[I] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) do
      Inc(I);
    fText.SelEnd:=I;
    fText.SelText:='';

    // find the start of the word
    CurrSel:=fText.SelStart;
    I:=CurrSel;
    while (I<>0) and (fText.Text[I] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) do
      Dec(I);
    fText.SelStart:=I;
    fText.SelEnd:=CurrSel;

    // don't add () if already there
    if fText.Text[CurrSel]='(' then
      FuncAddOn:='';

    fText.SelText:=Phrase+FuncAddOn;

    // if we added "()" move caret inside parenthesis
    // only if Key<>'.' and Key<>'>'
    // and function takes arguments...
    if (not (Key in ['.', '>']))
    and (FuncAddOn<>'')
    and ( (Length(ST^._Args)>2) or (ST^._Args='()') ) then begin
      fText.CaretX:=fText.CaretX-Length(FuncAddOn)+1;
    end;
  end;
end;

function TEditor.CheckAttributes(P: TBufferCoord; Phrase: string): boolean;
var
  token: string;
  attri: TSynHighlighterAttributes;
begin
  fText.GetHighlighterAttriAtRowCol(P, token, attri);
  Result:=not ((not Assigned(Attri)) or
    AnsiStartsStr('.', Phrase) or
    AnsiStartsStr('->', Phrase) or
    AnsiStartsStr('::', Phrase) or
    (
      Assigned(Attri) and
      (
        (Attri.Name = 'Preprocessor') or
        (Attri.Name = 'Comment') or
        (Attri.Name = 'String')
      )
    ));
end;

//////// CODE-COMPLETION - mandrav - END ///////

// variable info
procedure TEditor.EditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
	s, s1 : string;
	p : TBufferCoord;
	I, j : integer;
	attr : TSynHighlighterAttributes;
begin
	fHintTimer.Enabled := false;

	// If the rightclick menu is open, don't show up
//	if MainForm.EditorPopupMenu.PopupPoint.x = -1 then Exit;

	// Check if we're inside a comment or string by looking at text color. If we are, skip without showing a tooltip
	p := fText.DisplayToBufferPos(fText.PixelsToRowColumn(X, Y));
	if fText.GetHighlighterAttriAtRowCol(p, s, attr) then
		if (attr = fText.Highlighter.StringAttribute) or (attr = fText.Highlighter.CommentAttribute) then begin
			fText.Hint:='';
			Exit;
		end;

	if devEditor.ParserHints and (not MainForm.fDebugger.Executing) then begin // editing - show declaration of word under cursor in a hint
		p.Char := X;
		p.Line := Y;
		p := fText.DisplayToBufferPos(fText.PixelsToRowColumn(p.Char, p.Line));
		s := fText.GetWordAtRowCol(p);
	end else if devData.WatchHint and MainForm.fDebugger.Executing then begin // debugging - evaluate var under cursor and show value in a hint
		p := fText.DisplayToBufferPos(fText.PixelsToRowColumn(X, Y));
		I:=P.Char;
		s1:=fText.UnCollapsedLines[p.Line-1];
		if (I <> 0) and (s1 <> '') then begin
			j := Length(s1);
			while (I < j) and (s1[I] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) do
				Inc(I);
		end;
		P.Char:=I;
		Dec(I);
		if (s1 <> '') then
			while (I <> 0) and (s1[I] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '.', '-', '>', '&', '*']) do
				Dec(I, 1);
		s := Copy(s1, I + 1, p.Char - I - 1);
	//	if s<>MainForm.fDebugger.WatchVar then begin
	//		Application.HideHint;
	//		fCurrentHint := s;
	//	end;
	end;

	if (s <> '') and (not fHintTimer.Enabled) then begin
		fHintTimer.Enabled := true;
		fCurrentHint := s;
	end else if s = '' then
		fText.Hint:='';

	if s <> '' then begin
		if ssCtrl in Shift then
			fText.Cursor:=crHandPoint
		else
			fText.Cursor:=crIBeam;
	end else
		fText.Cursor:=crIBeam;
end;

procedure TEditor.EditorHintTimer(Sender : TObject);
var
	r : TRect;
	p: TPoint;
	st: PStatement;
	localfind : string;
	localfindpoint : TPoint;
	thisword : string;
begin
	fHintTimer.Enabled := false;
	p:=fText.ScreenToClient(Mouse.CursorPos);

	// If the mouse is outside of the editor, hide balloon
	if (p.X<=0) or (p.X>=fText.Width) or (p.Y<=0) or (p.Y>=fText.Height) then Exit;

	if (fCurrentHint <> '') and devEditor.ParserHints then begin

		// If we're editting, show information of word
		if not MainForm.fDebugger.Executing then begin

			// Skip if we're scanning for the second time...
			thisword := fText.WordAtMouse;
			if thisword = fCurrentWord then Exit;
			fCurrentWord := thisword;

			st:=MainForm.findstatement(localfind,localfindpoint,false);

			r.Left := Mouse.CursorPos.X;
			r.Top := Mouse.CursorPos.Y - 2*fText.LineHeight;
			r.Bottom := Mouse.CursorPos.Y + 10 + fText.LineHeight;
			r.Right := Mouse.CursorPos.X + 60;

			if localfind <> '' then begin
				fCurrentHint:=localfind + ' - ' + ExtractFileName(fFileName) + ' (' + inttostr(localfindpoint.y) + ') - Ctrl+Click to follow';
				fCompletionBox.ShowMsgHint(r, fCurrentHint);
			end else if Assigned(st) then begin
				fCurrentHint:=Trim(st^._FullText) + ' - ' + ExtractFileName(st^._FileName) + ' (' + inttostr(st^._Line) + ') - Ctrl+Click to follow';
				fCompletionBox.ShowMsgHint(r, fCurrentHint);
			end;

		// debugging - evaluate var under cursor and show value in a hint
		end else if devData.WatchHint and MainForm.fDebugger.Executing then begin
			MainForm.fDebugger.SendCommand(GDB_DISPLAY, fCurrentHint);
		//	Sleep(25);
		//	fText.Hint:=MainForm.fDebugger.WatchVar+': '+MainForm.fDebugger.WatchValue;
		//	fText.ShowHint:=True;
		//	Application.ActivateHint(Mouse.CursorPos);
		end;
	end;
end;

procedure TEditor.CommentSelection;
var
	S: string;
	Offset: integer;
	backup: TBufferCoord;
begin
	if Text.SelAvail then begin // has selection
		backup:=FText.CaretXY;
		Text.BeginUpdate;
		S:='//'+FText.SelText;
		Offset:=0;
		if S[Length(S)]=#10 then begin // if the selection ends with a newline, eliminate it
			if S[Length(S)-1]=#13 then // do we ignore 1 or 2 chars?
				Offset:=2
			else
				Offset:=1;
		end;
		if Offset = 2 then
			S:=StringReplace(S, #13#10, #13#10'//', [rfReplaceAll])
		else
			S:=StringReplace(S, #10, #10'//', [rfReplaceAll]);

		Text.SelText:=S;
		Text.EndUpdate;
		Text.CaretXY:=backup;
	end else // no selection; easy stuff ;)
		Text.LineText:='//'+Text.LineText;

	Text.UpdateCaret;
	Text.Modified:=True;
end;

{** Modified by Peter **}
procedure TEditor.IndentSelection;
begin
	if FText.BlockBegin.Line <> FText.BlockEnd.Line then
		FText.ExecuteCommand(ecBlockIndent, #0, nil)
	else
		FText.ExecuteCommand(ecTab,#0, nil);
end;

{** Modified by Peter **}
procedure TEditor.UnindentSelection;
begin
	if FText.BlockBegin.Line <> FText.BlockEnd.Line then
		FText.ExecuteCommand(ecBlockUnIndent, #0, nil)
	else
		FText.ExecuteCommand(ecShiftTab,#0, nil);
end;

{** Modified by Peter **}
procedure TEditor.UncommentSelection;

	function FirstCharIndex(const S: string; const start : integer): Integer;
	//  Get the index of the first non whitespace character in
	//  the string specified by S
	//  On success it returns the index, otherwise it returns 0
	var
		I: Integer;
	begin
		Result := 1;

		if S <> '' then
			for I := start to Length(S) do
				if not (S[I] in [#0..#32]) then begin
					Result := I;
					Break;
				end;
	end;

var
	I: Integer;
	Idx: Integer;
	selection: string;
begin
	// has selection
	if FText.SelAvail then begin

		// start an undoblock, so we can undo it afterwards!
		FText.BeginUndoBlock;

		// Scan the first line too
		selection := FText.SelText;
		Idx := FirstCharIndex(selection,1);
		if (selection[Idx]='/') and (selection[Idx+1]='/') then
			Delete(selection, Idx, 2);

		for I := 1 to Length(selection) do begin

			// selection might change during loop but Length(selection) in the for statement doesn't get updated
			if (I < Length(selection)) and (selection[I] = #10) then begin

				// Now that we've found a newline, check for a comment
				Idx := FirstCharIndex(selection,I);

				// check if the very first two letters in the string are '//'
				// if they are, then delete them from the string and set the
				// modified string to the stringlist ...
				if (selection[Idx]='/') and (selection[Idx+1]='/') then
					Delete(selection, Idx, 2);
			end;
		end;

		FText.SelText := selection;

		FText.EndUndoBlock;
		FText.UpdateCaret;
		FText.Modified:=True;
	end;
end;

procedure TEditor.EditorMouseDown(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
  procedure DoOpen(Fname: string; Line: integer; wrd: string);
  var
    e: TEditor;
    ws: integer;
  begin
    if fname = ExtractFileName(fname) then // no path info, so prepend path of active file
      fname:=ExtractFilePath(fFileName)+fname;

    // refer to the editor of the filename (will open if needed and made active)
    e:=MainForm.GetEditorFromFileName(fname);

    if Assigned(e) then begin
      // go to the specific line
      e.GotoLineNr(line);

      if wrd<>'' then begin
        // find the clicked word on this line and set the cursor on it ;)
        ws:=AnsiPos(wrd, e.Text.LineText);
        if ws>0 then
          e.Text.CaretX:=ws;
      end;

      // remove any selection made by Synedit.OnMouseDown
      fText.BlockBegin:=fText.CaretXY;
      fText.BlockEnd:=fText.BlockBegin;
    end;
  end;
var
	p: TPoint;
	s, s1: string;
	f1, f2: integer;
begin
	p.X := X;
	p.Y := Y;

	p.X := fText.PixelsToRowColumn(p.X, p.Y).Column;
	p.Y := fText.PixelsToRowColumn(p.X, p.Y).Row;
	s := fText.GetWordAtRowCol(BufferCoord(p.X, p.Y));

	// if ctrl+clicked
	if (ssCtrl in Shift) and (Button=mbLeft) then begin

		// reset the cursor
		fText.Cursor:=crIBeam;

		// see if it's #include
		s1:=fText.UnCollapsedLines[p.Y-1];
		s1:=StringReplace(s1, ' ', '', [rfReplaceAll]);
		if AnsiStartsStr('#include', s1) then begin
			// We've clicked an #include <>
			f1:=AnsiPos('<', s1);
			if f1>0 then begin
				Inc(f1);
				f2:=f1;
				while (f2<Length(s1)) and (s1[f2]<>'>') do
					Inc(f2);
				if s1[f2]<>'>' then
					Abort;
				f2:=f2-f1;
				DoOpen(MainForm.CppParser.GetFullFileName(Copy(s1, f1, f2)), 1, '');

				// the mousedown must *not* get to the SynEdit or else it repositions the caret!!!
				Abort;
			end;

			f1:=AnsiPos('"', s1);
			// We've clicked an #include ""
			if f1>0 then begin
				Inc(f1);
				f2:=f1;
				while (f2<Length(s1)) and (s1[f2]<>'"') do
					Inc(f2);
				if s1[f2]<>'"' then
					Abort;
				f2:=f2-f1;
				DoOpen(MainForm.CppParser.GetFullFileName(Copy(s1, f1, f2)), 1, '');

				// the mousedown must *not* get to the SynEdit or else it repositions the caret!!!
				Abort;
			end;

			// if we reached here, exit; we cannot locate and extract the filename...
			Exit;
		end; // #include

		MainForm.actGotoImplDeclEditorExecute(self);

		// the mousedown must *not* get to the SynEdit or else it repositions the caret!!!
		Abort;
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
	BracketSet = ['{','[','(','}',']',')'];
	OpenChars:array[0..2] of Char=('{','[','(');
	CloseChars:array[0..2] of Char=('}',']',')');
var
	P: TBufferCoord;
	Pix: TPoint;
	S: String;
	I: Integer;
	Attri: TSynHighlighterAttributes;
begin
	P := fText.CaretXY;
	fText.GetHighlighterAttriAtRowCol(P, S, Attri);
	if Assigned(Attri) and (fText.Highlighter.SymbolAttribute = Attri) and (fText.CaretX<=length(fText.LineText) + 1) then begin
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
	if (Assigned(fText.Highlighter)) and devEditor.Match and not fText.SelAvail then
		PaintMatchingBrackets(TransientType);
end;

// Editor needs to be told when class browser has been recreated otherwise AV !
procedure TEditor.UpdateParser;
begin
	FCodeToolTip.Parser := MainForm.CppParser;
end;

procedure TEditor.InvalidateGutter;
begin
	fText.InvalidateGutter;
end;

end.

