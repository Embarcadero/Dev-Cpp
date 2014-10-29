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

unit DebugReader;

interface

uses
{$IFDEF WIN32}
  Sysutils, Classes, Windows, StdCtrls,
  Dialogs, editor, ComCtrls, StrUtils, Forms;
{$ENDIF}
{$IFDEF LINUX}
Sysutils, Classes, debugreader,
version, QDialogs, QComCtrls, StrUtils, QForms;
{$ENDIF}

type
  TAnnotateType = (
    TPrePrompt, TPrompt, TPostPrompt,
    TSource,
    TDisplayBegin, TDisplayEnd,
    TDisplayExpression,
    TFrameSourceFile, TFrameSourceBegin, TFrameSourceLine, TFrameFunctionName, TFrameWhere,
    TFrameArgs,
    TFrameBegin, TFrameEnd,
    TErrorBegin, TErrorEnd,
    TArrayBegin, TArrayEnd,
    TElt, TEltRep, TEltRepEnd,
    TExit,
    TSignal, TSignalName, TSignalNameEnd, TSignalString, TSignalStringEnd,
    TValueHistoryValue, TValueHistoryBegin, TValueHistoryEnd,
    TArgBegin, TArgEnd, TArgValue, TArgNameEnd,
    TFieldBegin, TFieldEnd, TFieldValue, TFieldNameEnd,
    TInfoReg, TInfoAsm,
    TUnknown, TEOF);

  PWatchVar = ^TWatchVar;
  TWatchVar = record
    Name: AnsiString;
    gdbindex: integer;
    Node: TTreeNode;
  end;

  PBreakPoint = ^TBreakPoint;
  TBreakPoint = record
    line: integer;
    editor: TEditor;
  end;

  PTrace = ^TTrace;
  TTrace = record
    funcname: AnsiString;
    filename: AnsiString;
    line: AnsiString;
  end;

  PRegister = ^TRegister;
  TRegister = record
    name: AnsiString;
    valuehex: AnsiString;
    valuedec: AnsiString;
  end;

  TDebugReader = class(TThread)
  private
    fPipeRead: THandle;
    fRegisters: TList;
    fDisassembly: TStringList; // convert to TList with proper data formatting?
    fBacktrace: TList;
    fBreakpointList: TList;
    fWatchVarList: TList; // contains all parents
    fDebugView: TTreeView;
    fIndex: integer;
    fBreakPointLine: integer;
    fBreakPointFile: AnsiString;
    fOutput: AnsiString;
    fEvalValue: AnsiString;
    fSignal: AnsiString;

    // attempt to cut down on Synchronize calls
    dobacktraceready: boolean;
    dodisassemblerready: boolean;
    doregistersready: boolean;
    dorescanwatches: boolean;
    doevalready: boolean;
    doprocessexited: boolean;
    doupdatecpuwindow: boolean;
    doupdateexecution: boolean;
    doreceivedsignal: boolean;
    doreceivedsfwarning: boolean;

    // Signal handlers
    procedure HandleValueHistoryValue;
    procedure HandleSignal;
    procedure HandleExit;
    procedure HandleFrames;
    procedure HandleDisassembly;
    procedure HandleRegisters;
    procedure HandleError;
    procedure HandleDisplay;
    procedure HandleSource;

    // Evaluation tree output handlers
    procedure ProcessWatchOutput(WatchVar: PWatchVar);
    function ProcessEvalOutput: AnsiString;
    procedure ProcessDebugOutput;

    // synching with GUI
    procedure SyncFinishedParsing;

    // parsing
    procedure SkipSpaces; // skips space and tab
    procedure SkipToAnnotation; // skips until it finds #26#26 (GDB annotation for interfaces)
    function FindAnnotation(Annotation: TAnnotateType): boolean; // Finds the given annotation, returns false on EOF
    function GetNextAnnotation: TAnnotateType; // Returns the next annotation
    function GetLastAnnotation(const text: AnsiString; curpos, len: integer): TAnnotateType;
    // Returns the last annotation in given string
    function PeekNextAnnotation: TAnnotateType;
    // Returns the next annotation, but does not modify current scanning positions
    function GetNextWord: AnsiString; // copies the next word, stops when it finds chars 0..32
    function GetNextLine: AnsiString;
    // skips until enter sequence, skips ONE enter sequence, copies until next enter sequence
    function GetNextFilledLine: AnsiString;
    // skips until enter sequence, skips enter sequences, copies until next enter sequence
    function GetRemainingLine: AnsiString; // copies until enter sequence
    function GetAnnotation(const s: AnsiString): TAnnotateType; // converts string to TAnnotateType
  protected
    procedure Execute; override;
  public
    property Registers: TList read fRegisters write fRegisters;
    property Disassembly: TStringList read fDisassembly write fDisassembly;
    property Backtrace: TList read fBacktrace write fBacktrace;
    property PipeRead: THandle read fPipeRead write fPipeRead;
    property BreakPointList: TList read fBreakPointList write fBreakPointList;
    property WatchVarList: TList read fWatchVarList write fWatchVarList;
    property DebugView: TTreeView read fDebugView write fDebugView;
    property BreakPointFile : AnsiString read fBreakPointFile;
  end;

implementation

uses
  main, devcfg, CPUFrm, multilangsupport, debugger, utils, Controls, Math;

// macro for all the things that need to be done when we are finished parsing the current block

procedure TDebugReader.SyncFinishedParsing;
var
  SignalDialog: TForm;
  SignalCheck: TCheckBox;
  spawnedcpuform: boolean;
begin
  spawnedcpuform := false;

  // GDB determined that the source code is more recent than the executable. Ask the user if he wants to rebuild.
  if doreceivedsfwarning then begin
    if MessageDlg(Lang[ID_MSG_SOURCEMORERECENT], mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
      MainForm.Debugger.Stop;
      MainForm.actCompileExecute(nil);
      Exit;
    end;
  end;

  // The program to debug has stopped. Stop the debugger
  if doprocessexited then begin
    MainForm.Debugger.Stop;
    Exit;
  end;

  // An evaluation variable has been processed. Forward the results
  if doevalready and Assigned(MainForm.Debugger.OnEvalReady) then
    MainForm.Debugger.OnEvalReady(fEvalValue);

  // Delete unimportant stuff to reduce clutter
  fOutput := StringReplace(fOutput, #26, '->', [rfReplaceAll]);
  MainForm.DebugOutput.Lines.Add(fOutput);

  // Some part of the CPU form has been updated
  if Assigned(CPUForm) and not doreceivedsignal then begin
    if doregistersready then
      CPUForm.OnRegistersReady;

    if dodisassemblerready then
      CPUForm.OnAssemblerReady;

    if dobacktraceready then
      CPUForm.OnBacktraceReady;
  end;

  if doupdateexecution then begin
    MainForm.GotoBreakpoint(fBreakPointFile, fBreakPointLine); // set active line
    MainForm.Debugger.RefreshWatchVars; // update variable information
  end;

  if doreceivedsignal then begin
    SignalDialog := CreateMessageDialog(fSignal, mtError, [mbOk]);
    SignalCheck := TCheckBox.Create(SignalDialog);

    // Display it on top of everything
    SignalDialog.FormStyle := fsStayOnTop;

    SignalDialog.Height := 150;

    with SignalCheck do begin
      Parent := SignalDialog;
      Caption := 'Show CPU window';
      Top := Parent.ClientHeight - 22;
      Left := 8;
      Width := Parent.ClientWidth - 16;
      Checked := devData.ShowCPUSignal;
    end;

    MessageBeep(MB_ICONERROR);
    if SignalDialog.ShowModal = ID_OK then begin
      devData.ShowCPUSignal := SignalCheck.Checked;
      if SignalCheck.Checked and not Assigned(CPUForm) then begin
        MainForm.ViewCPUItemClick(nil);
        spawnedcpuform := true;
      end;
    end;

    SignalDialog.Free;
  end;

  // CPU form updates itself when spawned, don't update twice!
  if (doupdatecpuwindow and not spawnedcpuform) and Assigned(CPUForm) then begin
    MainForm.Debugger.SendCommand('disas', '');
    MainForm.Debugger.SendCommand('info registers', '');
    MainForm.Debugger.SendCommand('backtrace', '');
  end;
end;

procedure TDebugReader.SkipSpaces;
begin
  while fOutput[fIndex] in [#9, #32] do
    Inc(fIndex);
end;

procedure TDebugReader.SkipToAnnotation;
begin
  // Walk up to the next annotation
  while not (fOutput[fIndex] in [#26, #0]) do
    Inc(fIndex);

  // Crawl through the remaining ->'s
  while fOutput[fIndex] in [#26] do
    Inc(fIndex);
end;

function TDebugReader.FindAnnotation(Annotation: TAnnotateType): boolean;
var
  NextAnnotation: TAnnotateType;
begin
  result := False;

  repeat
    NextAnnotation := GetNextAnnotation;
    if NextAnnotation = TEOF then
      Exit; // return false
  until NextAnnotation = Annotation;

  result := True;
end;

function TDebugReader.GetNextWord: AnsiString;
begin
  Result := '';

  // Called when at a space? Skip over
  SkipSpaces;

  // Skip until a space
  while not (fOutput[fIndex] in [#0..#32]) do begin
    Result := Result + fOutput[fIndex];
    Inc(fIndex);
  end;
end;

function TDebugReader.GetRemainingLine: AnsiString;
begin
  Result := '';

  // Return part of line still ahead of us
  while not (fOutput[fIndex] in [#13, #10, #0]) do begin
    Result := Result + fOutput[fIndex];
    Inc(fIndex);
  end;
end;

function TDebugReader.GetNextLine: AnsiString;
begin
  Result := '';

  // Walk up to an enter sequence
  while not (fOutput[fIndex] in [#13, #10, #0]) do
    Inc(fIndex);

  // End of output. Exit
  if fOutput[fIndex] = #0 then
    Exit;

  // Skip ONE enter sequence (CRLF, CR, LF, etc.)
  if (fOutput[fIndex] = #13) and (fOutput[fIndex + 1] = #10) then // DOS
    Inc(fIndex, 2)
  else if fOutput[fIndex] = #13 then // UNIX
    Inc(fIndex)
  else if fOutput[fIndex] = #10 then // MAC
    Inc(fIndex);

  // Return next line
  Result := GetRemainingLine;
end;

function TDebugReader.GetNextFilledLine: AnsiString;
begin
  Result := '';

  // Walk up to an enter sequence
  while not (fOutput[fIndex] in [#13, #10, #0]) do
    Inc(fIndex);

  // Skip enter sequences (CRLF, CR, LF, etc.)
  while (fOutput[fIndex] in [#13, #10, #0]) do
    Inc(fIndex);

  // Return next line
  Result := GetRemainingLine;
end;

function TDebugReader.PeekNextAnnotation: TAnnotateType;
var
  IndexBackup: integer;
begin
  IndexBackup := fIndex; // do NOT modifiy curpos
  Result := GetNextAnnotation;
  fIndex := IndexBackup;
end;

function TDebugReader.GetLastAnnotation(const text: AnsiString; curpos, len: integer): TAnnotateType;
var
  s: AnsiString;
begin
  // Walk back until end of #26's
  while (curpos > 0) and not (text[curpos] in [#26]) do
    Dec(curpos);

  Inc(curpos);

  // Tiny rewrite of GetNextWord for special purposes
  s := '';
  while (curpos <= len) and not (text[curpos] in [#0..#32]) do begin
    s := s + text[curpos];
    Inc(curpos);
  end;

  Result := GetAnnotation(s);
end;

function TDebugReader.GetNextAnnotation: TAnnotateType;
begin
  // Skip until end of #26's, i.e. GDB formatted output
  SkipToAnnotation;

  // Get part this line, after #26#26
  Result := GetAnnotation(GetNextWord);
end;

function TDebugReader.GetAnnotation(const s: AnsiString): TAnnotateType;
var
  IndexBackup: integer;
  t: AnsiString;
begin
  if SameStr(s, 'pre-prompt') then
    result := TPrePrompt
  else if SameStr(s, 'prompt') then
    result := TPrompt
  else if SameStr(s, 'post-prompt') then begin
    result := TPostPrompt;

    IndexBackup := fIndex;
    t := GetNextFilledLine;
    fIndex := IndexBackup;

    // Hack fix to catch register dump
    if Assigned(fRegisters) then
      if StartsStr('rax ', t) or StartsStr('eax ', t) then
        result := TInfoReg;

    // Another hack to catch assembler
    if Assigned(fDisassembly) then
      if StartsStr('Dump of assembler code for function ', t) then
        result := TInfoAsm;

  end else if SameStr(s, 'error-begin') then
    result := TErrorBegin
  else if SameStr(s, 'error-end') then
    result := TErrorEnd
  else if SameStr(s, 'display-begin') then
    result := TDisplayBegin
  else if SameStr(s, 'display-expression') then
    result := TDisplayExpression
  else if SameStr(s, 'display-end') then
    result := TDisplayEnd
  else if SameStr(s, 'frame-source-begin') then
    result := TFrameSourceBegin
  else if SameStr(s, 'frame-source-file') then
    result := TFrameSourceFile
  else if SameStr(s, 'frame-source-line') then
    result := TFrameSourceLine
  else if SameStr(s, 'frame-function-name') then
    result := TFrameFunctionName
  else if SameStr(s, 'frame-args') then
    result := TFrameArgs
  else if SameStr(s, 'frame-begin') then
    result := TFrameBegin
  else if SameStr(s, 'frame-end') then
    result := TFrameEnd
  else if SameStr(s, 'frame-where') then
    result := TFrameWhere
  else if SameStr(s, 'source') then
    result := TSource
  else if SameStr(s, 'exited') then
    result := TExit
  else if SameStr(s, 'arg-begin') then
    result := TArgBegin
  else if SameStr(s, 'arg-name-end') then
    result := TArgNameEnd
  else if SameStr(s, 'arg-value') then
    result := TArgValue
  else if SameStr(s, 'arg-end') then
    result := TArgEnd
  else if SameStr(s, 'array-section-begin') then
    result := TArrayBegin
  else if SameStr(s, 'array-section-end') then
    result := TArrayEnd
  else if SameStr(s, 'elt') then
    result := TElt
  else if SameStr(s, 'elt-rep') then
    result := TEltRep
  else if SameStr(s, 'elt-rep-end') then
    result := TEltRepEnd
  else if SameStr(s, 'field-begin') then
    result := TFieldBegin
  else if SameStr(s, 'field-name-end') then
    result := TFieldNameEnd
  else if SameStr(s, 'field-value') then
    result := TFieldValue
  else if SameStr(s, 'field-end') then
    result := TFieldEnd
  else if SameStr(s, 'value-history-value') then
    result := TValueHistoryValue
  else if SameStr(s, 'value-history-begin') then
    result := TValueHistoryBegin
  else if SameStr(s, 'value-history-end') then
    result := TValueHistoryEnd
  else if SameStr(s, 'signal') then
    result := TSignal
  else if SameStr(s, 'signal-name') then
    result := TSignalName
  else if SameStr(s, 'signal-name-end') then
    result := TSignalNameEnd
  else if SameStr(s, 'signal-string') then
    result := TSignalString
  else if SameStr(s, 'signal-string-end') then
    result := TSignalStringEnd
  else if (fOutput[fIndex] = #0) then
    result := TEOF
  else
    result := TUnknown;
end;

procedure TDebugReader.ProcessWatchOutput(WatchVar: PWatchVar);
var
  S, NodeText: AnsiString;
  ParentNode: TTreeNode;
  ParentWasExpanded: boolean;
  I: integer;
  sl: TStringList;
begin
  // Expand if it was expanded or if it didn't have any children
  ParentWasExpanded := WatchVar^.Node.Expanded or not WatchVar^.Node.HasChildren;

  // Do not remove root node of watch variable
  WatchVar^.Node.DeleteChildren;
  ParentNode := WatchVar^.Node;
  ParentNode.Text := '';

  // Process output parsed by ProcessEvalStruct
  S := WatchVar^.Name + ' = ' + ProcessEvalOutput;
  // add placeholder name for variable name so we can format structs using one rule

// Add children based on indent
  sl := TStringList.Create;
  try
    sl.Text := S;
    for I := 0 to sl.Count - 1 do begin

      // Format node text. Remove trailing comma
      NodeText := Trim(sl[i]);
      if EndsStr(',', NodeText) then
        Delete(NodeText, Length(NodeText), 1);

      if EndsStr('{', NodeText) then begin // new member struct
        if ParentNode.Text = '' then // root node, replace text only
          ParentNode.Text := NodeText
        else
          ParentNode := DebugView.Items.AddChild(ParentNode, NodeText);
      end else if StartsStr('}', NodeText) then begin // end of struct, change parent
        ParentNode := ParentNode.Parent;
      end else begin // next parent member/child
        if ParentNode.Text = '' then // root node, replace text only
          ParentNode.Text := NodeText
        else
          DebugView.Items.AddChild(ParentNode, NodeText);
      end;
    end;
  finally
    sl.Free;
  end;

  // TODO: remember expansion state
  if ParentWasExpanded then
    WatchVar^.Node.Expand(false);
end;

function TDebugReader.ProcessEvalOutput: AnsiString;
var
  indent: integer;
  NextLine: AnsiString;
  NextAnnotation: TAnnotateType;
begin
  indent := 0;

  // First line gets special treatment
  Result := GetNextLine;
  if StartsStr('{', Result) then
    Inc(indent, 4);

  // Collect all data, add formatting in between
  repeat
    NextAnnotation := GetNextAnnotation;
    NextLine := GetNextLine;
    case NextAnnotation of

      // Change indent if { or } is found
      TFieldBegin: begin
          Result := Result + #13#10 + StringOfChar(' ', indent);
        end;
      TFieldValue: begin
          if StartsStr('{', NextLine) and (PeekNextAnnotation <> TArrayBegin) then
            Inc(indent, 4);
        end;
      TFieldEnd: begin
          if EndsStr('}', NextLine) then begin
            Dec(indent, 4);
            Result := Result + #13#10 + StringOfChar(' ', indent);
          end;
        end;
    end;
    Result := Result + NextLine;
  until NextAnnotation in [TEOF, TValueHistoryEnd, TDisplayEnd];
end;

procedure TDebugReader.HandleValueHistoryValue;
begin
  fEvalValue := ProcessEvalOutput;
  doevalready := true;
end;

procedure TDebugReader.HandleSignal;
begin
  fSignal := GetNextFilledLine; // Program received signal

  if not FindAnnotation(TSignalName) then
    Exit;

  fSignal := fSignal + GetNextFilledLine; // signal code

  if not FindAnnotation(TSignalNameEnd) then
    Exit;

  fSignal := fSignal + GetNextFilledLine; // comma

  if not FindAnnotation(TSignalString) then
    Exit;

  fSignal := fSignal + GetNextFilledLine; // user friendly description

  if not FindAnnotation(TSignalStringEnd) then
    Exit;

  fSignal := fSignal + GetNextFilledLine; // period

  doreceivedsignal := true;
end;

procedure TDebugReader.HandleExit;
begin
  doprocessexited := true;
end;

procedure TDebugReader.HandleFrames;
var
  s: AnsiString;
  trace: PTrace;
begin
  s := GetNextLine;

  // Is this a backtrace dump?
  if Assigned(fBacktrace) and StartsStr('#', s) then begin

    trace := new(PTrace);

    // Find function name
    if not FindAnnotation(TFrameFunctionName) then begin
      Dispose(PTrace(trace));
      Exit;
    end;

    trace^.funcname := GetNextLine;

    // Find argument list start
    if not FindAnnotation(TFrameArgs) then begin
      Dispose(PTrace(trace));
      Exit;
    end;

    // Arguments are either () or detailed list
    s := GetNextLine;

    while (PeekNextAnnotation = TArgBegin) do begin

      // argument name
      if not FindAnnotation(TArgBegin) then begin
        Dispose(PTrace(trace));
        Exit;
      end;

      s := s + GetNextLine;

      // =
      if not FindAnnotation(TArgNameEnd) then begin
        Dispose(PTrace(trace));
        Exit;
      end;

      s := s + ' ' + GetNextLine + ' '; // should be =

      // argument value
      if not FindAnnotation(TArgValue) then begin
        Dispose(PTrace(trace));
        Exit;
      end;

      s := s + GetNextLine;

      // argument end
      if not FindAnnotation(TArgEnd) then begin
        Dispose(PTrace(trace));
        Exit;
      end;

      s := s + GetNextLine;
    end;

    trace^.funcname := trace^.funcname + Trim(s);

    // If source info can't be found, skip
    if PeekNextAnnotation = TFrameSourceBegin then begin

      // Find filename
      if not FindAnnotation(TFrameSourceFile) then begin
        Dispose(PTrace(trace));
        Exit;
      end;

      trace^.filename := GetNextLine;

      // find line
      if not FindAnnotation(TFrameSourceLine) then begin
        Dispose(PTrace(trace));
        Exit;
      end;

      trace^.line := GetNextLine;
    end else begin
      trace^.filename := '';
      trace^.line := '';
    end;

    fBacktrace.Add(trace);

    // Skip over the remaining frame part...
    if not FindAnnotation(TFrameEnd) then
      Exit;

    // Not another one coming? Done!
    if PeekNextAnnotation <> TFrameBegin then begin

      // End of stack trace dump!
      dobacktraceready := true;
    end;
  end else
    doupdatecpuwindow := true;
end;

procedure TDebugReader.HandleDisassembly;
var
  s: AnsiString;
begin
  if not Assigned(fDisassembly) then
    Exit;

  // Get info message
  s := GetNextLine;

  // the full function name will be saved at index 0
  fDisassembly.Add(Copy(s, 37, Length(s) - 37));

  s := GetNextLine;

  // Add lines of disassembly
  while not SameStr('End of assembler dump.', s) and not SameStr(s, '') do begin
    fDisassembly.Add(s);
    s := GetNextLine;
  end;

  dodisassemblerready := true;
end;

procedure TDebugReader.HandleRegisters;
var
  s: AnsiString;
  reg: PRegister;
  x: integer;
begin
  if not Assigned(fRegisters) then
    Exit;

  // name(spaces)hexvalue(tab)decimalvalue
  s := GetNextFilledLine;

  repeat
    reg := new(PRegister);

    // Cut name from 1 to first space
    x := Pos(' ', s);
    reg^.name := Copy(s, 1, x - 1);
    Delete(s, 1, x - 1);

    // Remove spaces
    s := TrimLeft(s);

    // Cut hex value from 1 to first tab
    x := Pos(#9, s);
    reg^.valuehex := Copy(s, 1, x - 1);
    Delete(s, 1, x); // delete tab too
    s := TrimLeft(s);

    // Remaining part contains decimal value
    reg^.valuedec := s;

    fRegisters.Add(reg);

    s := GetNextLine;

  until SameStr('', s);

  doregistersready := true;
end;

procedure TDebugReader.HandleError;
var
  s, WatchName: AnsiString;
  Tail, Head, I: integer;
  WatchVar: PWatchVar;
begin
  s := GetNextLine; // error text
  if StartsStr('No symbol "', s) then begin
    Tail := Pos('"', s);
    Head := RPos('"', s);
    WatchName := Copy(s, Tail + 1, Head - Tail - 1);

    // Update current...
    for I := 0 to WatchVarList.Count - 1 do begin
      WatchVar := PWatchVar(WatchVarList.Items[I]);
      if SameStr(WatchVar^.name, WatchName) then begin

        WatchVar^.Node.Text := WatchVar^.Name + ' = Not found in current context';

        // Delete now invalid children
        WatchVar^.Node.DeleteChildren;

        dorescanwatches := true;
        break;
      end;
    end;
  end;
end;

procedure TDebugReader.HandleDisplay;
var
  s, WatchName: AnsiString;
  I: integer;
  WatchVar: PWatchVar;
begin
  s := GetNextLine; // watch index

  if not FindAnnotation(TDisplayExpression) then
    Exit;

  WatchName := GetNextLine; // watch name

  // Find parent we're talking about
  for I := 0 to WatchVarList.Count - 1 do begin
    WatchVar := PWatchVar(WatchVarList.Items[I]);
    if SameStr(WatchVar^.name, WatchName) then begin

      // Advance up to the value
      if not FindAnnotation(TDisplayExpression) then
        Exit;

      // Refresh GDB index so we can undisplay this by index
      WatchVar^.gdbindex := StrToInt(s);

      // Refresh members...
      ProcessWatchOutput(WatchVar);
      break;
    end;
  end;
end;

procedure TDebugReader.HandleSource;
var
  s: AnsiString;
  DelimPos, I: integer;
begin
  // source filename:line:offset:beg/middle/end:addr
  s := TrimLeft(GetRemainingLine);

  // remove offset, beg/middle/end, address
  for I := 1 to 3 do begin
    DelimPos := RPos(':', s);
    if DelimPos > 0 then
      Delete(s, DelimPos, MaxInt)
    else
      Exit; // Wrong format. Don't bother to continue
  end;

  // get line
  DelimPos := RPos(':', s);
  if DelimPos > 0 then begin
    fBreakPointLine := StrToIntDef(Copy(s, DelimPos + 1, MaxInt), 1);
    Delete(s, DelimPos, MaxInt);
  end;

  // get file
  fBreakPointFile := s;

  doupdateexecution := true;
  doupdatecpuwindow := true;
end;

procedure TDebugReader.ProcessDebugOutput;
var
  NextAnnotation: TAnnotateType;
begin
  // Only update once per update at most
  DebugView.Items.BeginUpdate;
  try

    dobacktraceready := false;
    dodisassemblerready := false;
    doregistersready := false;
    dorescanwatches := false;
    doevalready := false;
    doprocessexited := false;
    doupdateexecution := false;
    doreceivedsignal := false;
    doupdatecpuwindow := false;
    doreceivedsfwarning := false;

    // Global checks
    if Pos('warning: Source file is more recent than executable.', fOutput) > 0 then
      doreceivedsfwarning := true;

    fIndex := 1;
    repeat
      NextAnnotation := GetNextAnnotation;
      case NextAnnotation of
        TValueHistoryValue:
          HandleValueHistoryValue;
        TSignal:
          HandleSignal;
        TExit:
          HandleExit;
        TFrameBegin:
          HandleFrames;
        TInfoAsm:
          HandleDisassembly;
        TInfoReg:
          HandleRegisters;
        TErrorBegin:
          HandleError;
        TDisplayBegin:
          HandleDisplay;
        TSource:
          HandleSource
            //else
        //	break;
      end;
    until NextAnnotation = TEOF;

    // Only update once per update at most
  finally
    DebugView.Items.EndUpdate;
  end;

  Synchronize(SyncFinishedParsing);
end;

procedure TDebugReader.Execute;
var
  tmp: AnsiString;
  bytesread, totalbytesread: DWORD;
const
  chunklen = 1000; // GDB usually sends 4K blocks, disassembly easily takes up to 20K
begin

  bytesread := 0;
  totalbytesread := 0;

  while not Terminated do begin

    // Add chunklen bytes to length, and set chunklen extra bytes to zero
    SetLength(tmp, 1 + totalbytesread + chunklen);
    FillChar(tmp[1 + totalbytesread], chunklen + 1, 0);

    // ReadFile returns when there's something to read
    if not ReadFile(fPipeRead, tmp[1 + totalbytesread], chunklen, bytesread, nil) or (bytesread = 0) then
      break;

    Inc(totalbytesread, bytesread);

    if not Terminated then begin

      // Assume fragments don't end nicely with TErrorBegin or TPrompt
      if GetLastAnnotation(tmp, totalbytesread, 1 + totalbytesread + chunklen) in [TErrorBegin, TPrompt] then begin
        fOutput := tmp;
        ProcessDebugOutput;

        // Reset storage
        totalbytesread := 0;
      end;
    end;
  end;
end;

end.

