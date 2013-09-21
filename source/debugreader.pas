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

unit debugreader;

interface

uses 
{$IFDEF WIN32}
  Sysutils, Classes, Windows, ShellAPI, StdCtrls,
  version, Dialogs, editor, ComCtrls, StrUtils, Forms;
{$ENDIF}
{$IFDEF LINUX}
  Sysutils, Classes, debugreader,
  version, QDialogs, QComCtrls, StrUtils, QForms;
{$ENDIF}

type
  TAnnotateType = (TPrePrompt, TPrompt, TPostPrompt,
                   TSource,
                   TDisplayBegin, TDisplayEnd,
                   TDisplayExpression,
                   TFrameSourceFile, TFrameSourceBegin, TFrameSourceLine, TFrameFunctionName, TFrameWhere,
                   TFrameArgs,
                   TFrameBegin, TFrameEnd,
                   TErrorBegin, TErrorEnd,
                   TArrayBegin, TArrayEnd,
                   TElt,TEltRep, TEltRepEnd,
                   TExit,
                   TSignal,TSignalName,TSignalNameEnd,TSignalString,TSignalStringEnd,
                   TValueHistoryValue, TValueHistoryBegin, TValueHistoryEnd,
                   TArgBegin, TArgEnd, TArgValue, TArgNameEnd,
                   TFieldBegin, TFieldEnd, TFieldValue, TFieldNameEnd,
                   TInfoReg, TInfoAsm,
                   TUnknown,TEOF);

  PWatchVar = ^TWatchVar;
  TWatchVar = record
    name : AnsiString;
    value : AnsiString;
    gdbindex : integer;
    node : TTreeNode;
  end;

  PBreakPoint = ^TBreakPoint;
  TBreakPoint = record
    line : integer;
    editor : TEditor;
  end;

  PTrace = ^TTrace;
  TTrace = record
    funcname : AnsiString;
    filename : AnsiString;
    line : AnsiString;
  end;

  PRegister = ^TRegister;
  TRegister = record
    name : AnsiString;
    valuehex : AnsiString;
    valuedec : AnsiString;
  end;

  TDebugReader = class(TThread)
  public
    hPipeRead : THandle;
    Registers : TList;
    Disassembly : TStringList; // convert to TList with proper data formatting?
    Backtrace : TList;
    BreakpointList : TList;
    WatchVarList : TList; // contains all parents
    DebugTree : TTreeView;
  private
    curpos : integer;
    len : integer;
    bline : integer;
    bfile : AnsiString;
    gdbout : AnsiString;
    evalvalue : AnsiString;
    signal : AnsiString;
    prevannotation : TAnnotateType;
    nextannotation : TAnnotateType;

	// attempt to cut down on Synchronize calls
	dobacktraceready : boolean;
	dodisassemblerready : boolean;
	doregistersready : boolean;
	dorescanwatches : boolean;
	doevalready : boolean;
	doprocessexited : boolean;
	doupdatecpuwindow : boolean;
	doupdateexecution : boolean;
	doreceivedsignal : boolean;
	doreceivedsfwarning : boolean;

	// Evaluation tree output handlers
    procedure ProcessWatchStruct(parentnode : TTreeNode);
    function ProcessEvalStruct : AnsiString;

    procedure Analyze;

    // synching with GUI
    procedure SyncFinishedParsing;

    // parsing
    procedure SkipSpaces; // skips space and tab
    procedure SkipToAnnotation; // skips until it finds #26#26 (GDB annotation for interfaces)
    function FindAnnotation(an : TAnnotateType) : boolean; // Finds the given annotation, returns false on EOF
    function GetNextAnnotation : TAnnotateType; // Returns the next annotation
    function GetLastAnnotation(const text : AnsiString;curpos,len : integer) : TAnnotateType; // Returns the last annotation in given string
    function PeekNextAnnotation(nextcount : integer = 1) : TAnnotateType; // Returns the next annotation, but does not modify current scanning positions
    function PeekPrevAnnotation(prevcount : integer = 1) : TAnnotateType; // Returns the prev annotation, but does not modify current scanning positions
    function GetNextWord : AnsiString; // copies the next word, stops when it finds chars 0..32
    function GetNextLine : AnsiString; // skips until enter sequence, skips ONE enter sequence, copies until next enter sequence
    function GetNextFilledLine : AnsiString; // skips until enter sequence, skips enter sequences, copies until next enter sequence
    function GetRemainingLine : AnsiString; // copies until enter sequence
    function GetAnnotation(const s : AnsiString) : TAnnotateType; // converts string to TAnnotateType
  protected
    procedure Execute; override;
  end;

implementation

uses
  main, devcfg, CPUFrm, multilangsupport ,debugger, utils, Controls, Math;

// macro for all the things that need to be done when we are finished parsing the current block
procedure TDebugReader.SyncFinishedParsing;
var
	SignalDialog: TForm;
	SignalCheck: TCheckBox;
	spawnedcpuform : boolean;
begin
	spawnedcpuform := false;

	if doreceivedsfwarning then begin
		if MessageDlg(Lang[ID_MSG_SOURCEMORERECENT],mtConfirmation,[mbYes,mbNo],0) = mrYes then begin
			MainForm.fDebugger.Stop;
			MainForm.actCompileExecute(nil);
			Exit;
		end;
	end;

	if doprocessexited then begin
		MainForm.fDebugger.Stop;
		Exit;
	end;

	if doevalready and Assigned(MainForm.fDebugger.OnEvalReady) then
		MainForm.fDebugger.OnEvalReady(evalvalue);

	// Delete unimportant stuff to reduce clutter
	gdbout := StringReplace(gdbout,#26,'->',[rfReplaceAll]);
	MainForm.DebugOutput.Lines.Add(gdbout);

	if Assigned(CPUForm) and not doreceivedsignal then begin
		if doregistersready then
			CPUForm.OnRegistersReady;

		if dodisassemblerready then
			CPUForm.OnAssemblerReady;

		if dobacktraceready then
			CPUForm.OnBacktraceReady;
	end;

	if doupdateexecution then begin
		MainForm.GotoBreakpoint(bfile, bline); // set active line
		MainForm.fDebugger.RefreshWatchVars; // update variable information
	end;

	if doreceivedsignal then begin
		SignalDialog := CreateMessageDialog(signal, mtError, [mbOk]);
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
		MainForm.fDebugger.SendCommand('disas','');
		MainForm.fDebugger.SendCommand('info registers','');
		MainForm.fDebugger.SendCommand('backtrace','');
	end;
end;

procedure TDebugReader.SkipSpaces;
begin
	while (curpos < len) and (gdbout[curpos] in [#9,#32]) do
		Inc(curpos);
end;

procedure TDebugReader.SkipToAnnotation;
begin
	// Walk up to the next annotation
	while (curpos < len) and not (gdbout[curpos] in [#26]) do
		Inc(curpos);

	// Crawl through the remaining ->'s
	while (curpos < len) and (gdbout[curpos] in [#26]) do
		Inc(curpos);
end;

function TDebugReader.FindAnnotation(an : TAnnotateType) : boolean;
var
	curran : TAnnotateType;
begin
	result := false;

	repeat
		curran := GetNextAnnotation;
		if curran = TEOF then
			Exit;
	until curran = an;

	result := true;
end;

function TDebugReader.GetNextWord : AnsiString;
begin
	Result:='';

	// Called when at a space? Skip over
	SkipSpaces;

	while (curpos < len) and not (gdbout[curpos] in [#0..#32]) do begin
		Result := Result + gdbout[curpos];
		Inc(curpos);
	end;
end;

function TDebugReader.GetRemainingLine : AnsiString;
begin
	Result := '';

	// Return part of line still ahead of us
	while (curpos < len) and not (gdbout[curpos] in [#13, #10]) do begin
		Result := Result + gdbout[curpos];
		Inc(curpos);
	end;
end;

function TDebugReader.GetNextLine : AnsiString;
begin
	Result := '';

	// Walk up to an enter sequence
	while (curpos < len) and not (gdbout[curpos] in [#13, #10]) do
		Inc(curpos);

	// Skip ONE enter sequence (CRLF, CR, LF, etc.)
	if (curpos+1 < len) and (gdbout[curpos] = #13) and (gdbout[curpos+1] = #10) then // DOS
		Inc(curpos,2)
	else if (curpos < len) and (gdbout[curpos] = #13) then // UNIX
		Inc(curpos)
	else if (curpos < len) and (gdbout[curpos] = #10) then // MAC
		Inc(curpos);

	// Return next line
	Result := GetRemainingLine;
end;

function TDebugReader.GetNextFilledLine : AnsiString;
begin
	Result := '';

	// Walk up to an enter sequence
	while (curpos < len) and not (gdbout[curpos] in [#13, #10]) do
		Inc(curpos);

	// Skip enter sequences (CRLF, CR, LF, etc.)
	while (curpos < len) and (gdbout[curpos] in [#13, #10]) do
		Inc(curpos);

	// Return next line
	Result := GetRemainingLine;
end;

function TDebugReader.PeekPrevAnnotation(prevcount : integer) : TAnnotateType;
var
	oldpos, I : integer;
begin
	oldpos := curpos;

	// Go back to before a #26#26
	for I := 0 to prevcount -1 do begin
		while (curpos > 1) and not ((gdbout[curpos-1] in [#26]) and (gdbout[curpos] in [#26])) do
			Dec(curpos);
		Dec(curpos); // continue BEFORE a #26
	end;

	Result := GetNextAnnotation;
	prevannotation := Result;
	curpos := oldpos;
end;

function TDebugReader.PeekNextAnnotation(nextcount : integer) : TAnnotateType;
var
	oldpos, I : integer;
begin
	oldpos := curpos;

	// Go back to after a #26#26
	for I := 0 to nextcount -1 do begin
		while (curpos < len) and not ((gdbout[curpos] in [#26]) and (gdbout[curpos+1] in [#26])) do
			Inc(curpos);
		Inc(curpos); // continue AFTER a #26
	end;

	Result := GetNextAnnotation;
	nextannotation := Result;
	curpos := oldpos;
end;

function TDebugReader.GetLastAnnotation(const text : AnsiString;curpos,len : integer) : TAnnotateType;
var
	s : AnsiString;
begin
	// Walk back until end of #26's
	while (curpos > 0) and not (text[curpos] in [#26]) do
		Dec(curpos);

	Inc(curpos);

	// Tiny rewrite of GetNextWord for special purposes
	s := '';
	while (curpos < len) and not (text[curpos] in [#0..#32]) do begin
		s := s + text[curpos];
		Inc(curpos);
	end;

	Result := GetAnnotation(s);
end;

function TDebugReader.GetNextAnnotation : TAnnotateType;
begin
	// Skip until end of #26's, i.e. GDB formatted output
	SkipToAnnotation;

	// Get part this line, after #26#26
	Result := GetAnnotation(GetNextWord);
end;

function TDebugReader.GetAnnotation(const s : AnsiString) : TAnnotateType;
var
	oldpos : integer;
	t : AnsiString;
begin
	if SameStr(s,'pre-prompt') then
		result := TPrePrompt
	else if SameStr(s,'prompt') then
		result := TPrompt
	else if SameStr(s,'post-prompt') then begin
		result := TPostPrompt;

		oldpos := curpos;
		t := GetNextFilledLine;
		curpos := oldpos;

		// Hack fix to catch register dump
		if Assigned(Registers) then
			if StartsStr('rax ',t) or StartsStr('eax ',t) then
				result := TInfoReg;

		// Another hack to catch assembler
		if Assigned(Disassembly) then
			if StartsStr('Dump of assembler code for function ',t) then
				result := TInfoAsm;

	end else if SameStr(s,'error-begin') then
		result := TErrorBegin
	else if SameStr(s,'error-end') then
		result := TErrorEnd
	else if SameStr(s,'display-begin') then
		result := TDisplayBegin
	else if SameStr(s,'display-expression') then
		result := TDisplayExpression
	else if SameStr(s,'display-end') then
		result := TDisplayEnd
	else if SameStr(s,'frame-source-begin') then
		result := TFrameSourceBegin
	else if SameStr(s,'frame-source-file') then
		result := TFrameSourceFile
	else if SameStr(s,'frame-source-line') then
		result := TFrameSourceLine
	else if SameStr(s,'frame-function-name') then
		result := TFrameFunctionName
	else if SameStr(s,'frame-args') then
		result := TFrameArgs
	else if SameStr(s,'frame-begin') then
		result := TFrameBegin
	else if SameStr(s,'frame-end') then
		result := TFrameEnd
	else if SameStr(s,'frame-where') then
		result := TFrameWhere
	else if SameStr(s,'source') then
		result := TSource
	else if SameStr(s,'exited') then
		result := TExit
	else if SameStr(s,'arg-begin') then
		result := TArgBegin
	else if SameStr(s,'arg-name-end') then
		result := TArgNameEnd
	else if SameStr(s,'arg-value') then
		result := TArgValue
	else if SameStr(s,'arg-end') then
		result := TArgEnd
	else if SameStr(s,'array-section-begin') then
		result := TArrayBegin
	else if SameStr(s,'array-section-end') then
		result := TArrayEnd
	else if SameStr(s,'elt') then
		result := TElt
	else if SameStr(s,'elt-rep') then
		result := TEltRep
	else if SameStr(s,'elt-rep-end') then
		result := TEltRepEnd
	else if SameStr(s,'field-begin') then
		result := TFieldBegin
	else if SameStr(s,'field-name-end') then
		result := TFieldNameEnd
	else if SameStr(s,'field-value') then
		result := TFieldValue
	else if SameStr(s,'field-end') then
		result := TFieldEnd
	else if SameStr(s,'value-history-value') then
		result := TValueHistoryValue
	else if SameStr(s,'value-history-begin') then
		result := TValueHistoryBegin
	else if SameStr(s,'value-history-end') then
		result := TValueHistoryEnd
	else if SameStr(s,'signal') then
		result := TSignal
	else if SameStr(s,'signal-name') then
		result := TSignalName
	else if SameStr(s,'signal-name-end') then
		result := TSignalNameEnd
	else if SameStr(s,'signal-string') then
		result := TSignalString
	else if SameStr(s,'signal-string-end') then
		result := TSignalStringEnd
	else if (curpos = len) then
		result := TEOF
	else
		result := TUnknown;
end;

procedure TDebugReader.ProcessWatchStruct(parentnode : TTreeNode);
var
	evalout,s : AnsiString;
	parent : TTreeNode;
	curpos, len, indent, previndent : integer;

	// Similar to TDebugReader.GetRemainingLine, but skips enters too
	function GetRemainingLine : AnsiString;
	begin
		Result := '';

		// Return part of line still ahead of us
		while (curpos < len) and not (evalout[curpos] in [#13, #10]) do begin
			Result := Result + evalout[curpos];
			Inc(curpos);
		end;

		// Walk up to an enter sequence
		while (curpos < len) and not (evalout[curpos] in [#13, #10]) do
			Inc(curpos);

		// Skip enter sequences (CRLF, CR, LF, etc.)
		while (curpos < len) and (evalout[curpos] in [#13, #10]) do
			Inc(curpos);
	end;

	function GetLineIndent : integer;
	begin
		Result := 0;

		// Return part of line still ahead of us
		while (curpos < len) and (evalout[curpos] = #32) do begin
			Inc(result);
			Inc(curpos);
		end;
	end;

begin

	// Process output parsed by ProcessEvalStruct
	evalout := ProcessEvalStruct;

	parent := parentnode;
	curpos := 1;
	len := Length(evalout);
	previndent := 4; // starting node has already been added

	while curpos < len do begin
		indent := GetLineIndent;

		if indent > previndent then begin // set new parent
			parent := parent.GetLastChild;
		end else if indent < previndent then begin // return to old parent
			parent := parent.Parent;
			previndent := indent;
			s := GetRemainingLine;
			continue;
		end;

		s := GetRemainingLine;
		if not SameStr('};',s) then
			DebugTree.Items.AddChild(parent,s);

		previndent := indent;
	end;
end;

function TDebugReader.ProcessEvalStruct : AnsiString;
var
	i, indent : integer;
	s : AnsiString;
begin
	indent := 1;
	result := '';
	while curpos < len do begin
		case GetNextAnnotation of
			TFieldBegin : begin

				for i := 0 to (4*indent) - 1 do
					result := result + ' ';

				// Field name
				result := result + GetNextLine;

				// =
				if not FindAnnotation(TFieldNameEnd) then Exit;

				result := result + GetNextLine;

				// field value
				if not FindAnnotation(TFieldValue) then Exit;

				s := GetNextLine;
				result := result + s;

				case PeekNextAnnotation of
					TFieldBegin : begin// struct inside struct
						result := result + #13#10;
						Inc(indent);
					end;
					TArrayBegin : begin
						if PeekNextAnnotation(2) = TFieldBegin then begin // array of struct inside field
							result := result + #13#10;
							Inc(indent);
						end;
					end;
				end;
			end;

			TArrayBegin,TArrayEnd : begin
				s := GetNextLine;
				if PeekNextAnnotation = TFieldBegin then begin // struct inside array

					for i := 0 to (4*indent) - 1 do
						result := result + ' ';

					result := result + s + #13#10;

					Inc(indent);
				end else if PeekPrevAnnotation(3) = TFieldEnd then begin // end of struct inside array
					result := result + #13#10;

					for i := 0 to (4*(indent-1)) - 1 do
						result := result + ' ';

					result := result + s;

					Dec(indent);
				end else if PeekPrevAnnotation(4) = TFieldEnd then begin // end of repeated struct inside array
					result := result + #13#10;

					for i := 0 to (4*(indent-1)) - 1 do
						result := result + ' ';

					result := result + s;

					Dec(indent);
				end else begin
					result := result + s;
				end;
			end;

			TElt, TEltRep : begin
				s := GetNextLine;
				if PeekNextAnnotation = TFieldBegin then begin // struct inside array

					result := result + s + #13#10;

					Inc(indent);
				end else if PeekPrevAnnotation(3) = TFieldEnd then begin // end of struct inside array
					result := result + #13#10;

					result := result + s;

					Dec(indent);
				end else if PeekPrevAnnotation(4) = TFieldEnd then begin // end of repeated struct inside array
					result := result + #13#10;

					result := result + s;

					Dec(indent);
				end else begin
					result := result + s;
				end;
			end;

			// Add, complete current indent
			TFieldEnd : begin
				s := GetNextLine;

				result := result + ';' + #13#10;

				// End of structure, complete braces
				if PeekNextAnnotation <> TFieldBegin then begin
					for i := 0 to (4*(indent-1)) - 1 do
						result := result + ' ';

					result := result + s;
					Dec(indent);
				end;
			end;

			TDisplayEnd,TValueHistoryEnd : begin
				break;
			end;
		end;
	end;
end;

procedure TDebugReader.Analyze;
var
	s,t : AnsiString;
	i,x,y : integer;
	wparent : PWatchVar;
	reg : PRegister;
	trace : PTrace;
begin

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

	len := Length(gdbout);
	curpos := 1;

	// Global checks
	if Pos('warning: Source file is more recent than executable.',gdbout) > 0 then
		doreceivedsfwarning := true;

	while curpos < len do begin
		case GetNextAnnotation of
			TValueHistoryValue : begin
				evalvalue := GetNextLine; // value, might be empty
				if SameStr(evalvalue,'') then
					evalvalue := 'Error evaluating input'
				else begin
					case PeekNextAnnotation of
						TFieldBegin : begin
							evalvalue := evalvalue + #13#10 + ProcessEvalStruct;
						end;
						TArrayBegin : begin
							if PeekNextAnnotation(2) = TFieldBegin then
								evalvalue := evalvalue + #13#10 + ProcessEvalStruct
							else
								evalvalue := evalvalue + ProcessEvalStruct;
						end;
					end;
				end;
				doevalready := true;
			end;
			TSignal : begin

				// Assemble user string

				signal := GetNextFilledLine; // Program received signal

				if not FindAnnotation(TSignalName) then Exit;

				signal := signal + GetNextFilledLine; // signal code

				if not FindAnnotation(TSignalNameEnd) then Exit;

				signal := signal + GetNextFilledLine; // comma

				if not FindAnnotation(TSignalString) then Exit;

				signal := signal + GetNextFilledLine; // user friendly description

				if not FindAnnotation(TSignalStringEnd) then Exit;

				signal := signal + GetNextFilledLine; // period

				doreceivedsignal := true;
			end;
			TExit : begin
				doprocessexited := true;
			end;
			TFrameBegin : begin

				s := GetNextLine;

				// Is this a backtrace dump?
				if Assigned(Backtrace) and StartsStr('#',s) then begin

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

					Backtrace.Add(trace);

					// Skip over the remaining frame part...
					if not FindAnnotation(TFrameEnd) then Exit;

					// Not another one coming? Done!
					if PeekNextAnnotation <> TFrameBegin then begin

						// End of stack trace dump!
						dobacktraceready := true;
					end;
				end else
					doupdatecpuwindow := true;
			end;
			TInfoAsm : begin
				if Assigned(Disassembly) then begin

					// Get info message
					s := GetNextLine;

					// the full function name will be saved at index 0
					Disassembly.Add(Copy(s,37,Length(s)-37));

					s := GetNextLine;

					// Add lines of disassembly
					while not SameStr('End of assembler dump.',s) and not SameStr(s,'') do begin
						Disassembly.Add(s);
						s := GetNextLine;
					end;

					dodisassemblerready := true;
				end;
			end;
			TInfoReg : begin
				if Assigned(Registers) then begin

					// name(spaces)hexvalue(tab)decimalvalue
					s := GetNextFilledLine;

					repeat
						reg := new(PRegister);

						// Cut name from 1 to first space
						x := Pos(' ',s);
						reg^.name := Copy(s,1,x-1);
						Delete(s,1,x-1);

						// Remove spaces
						s := TrimLeft(s);


						// Cut hex value from 1 to first tab
						x := Pos(#9,s);
						reg^.valuehex := Copy(s,1,x-1);
						Delete(s,1,x); // delete tab too
						s := TrimLeft(s);

						// Remaining part contains decimal value
						reg^.valuedec := s;

						Registers.Add(reg);

						s := GetNextLine;

					until SameStr('',s);

					doregistersready := true;
				end;
			end;
			TErrorBegin : begin
				s := GetNextLine; // error text
				if StartsStr('No symbol "',s) then begin
					x := Pos('"',s);
					y := GetLastPos('"',s);
					t := Copy(s,x+1,y-x-1);

					// Update current...
					for I := 0 to WatchVarList.Count - 1 do begin
						wparent := PWatchVar(WatchVarList.Items[I]);
						if SameStr(wparent^.name,t) then begin

							DebugTree.Items.BeginUpdate;

							wparent^.value := 'Not found in current context';
							wparent^.node.Text := wparent^.name + ' = ' + wparent^.value;

							// Delete now invalid children
							wparent^.node.DeleteChildren;

							dorescanwatches := true;

							DebugTree.Items.EndUpdate;

							break;
						end;
					end;
				end;
			end;
			TDisplayBegin : begin

				s := GetNextLine; // watch index

				if not FindAnnotation(TDisplayExpression) then Exit;

				t := GetNextLine; // watch name

				// Find parent we're talking about
				for I := 0 to WatchVarList.Count - 1 do begin
					wparent := PWatchVar(WatchVarList.Items[I]);
					if SameStr(wparent^.name,t) then begin

						DebugTree.Items.BeginUpdate;

						if not FindAnnotation(TDisplayExpression) then Exit;

						wparent^.gdbindex := StrToInt(s);
						wparent^.value := GetNextLine;
						wparent^.node.Text := wparent^.name + ' = ' + wparent^.value;

						// Refresh members...
						wparent^.node.DeleteChildren;
						case PeekNextAnnotation of
							TFieldBegin : begin
								ProcessWatchStruct(wparent^.node);
							end;
							TArrayBegin : begin
								if PeekNextAnnotation(2) = TFieldBegin then
									ProcessWatchStruct(wparent^.node);
							end;
						end;

						DebugTree.Items.EndUpdate;

						break;
					end;
				end;
			end;
			TSource : begin // source filename:line:offset:beg/middle/end:addr
				s := TrimLeft(GetRemainingLine);

				// remove offset, beg/middle/end, address
				for I := 1 to 3 do begin
					x := Length(s);
					y := GetLastPos(':',s);
					if y > 0 then begin
						Delete(s,y,x-y+1);
					end;
				end;

				// get line
				x := Length(s);
				y := GetLastPos(':',s);
				if y > 0 then begin
					bline := StrToInt(Copy(s,y+1,x-y));
					Delete(s,y,x-y+1);
				end;

				// get file
				bfile := s;

				doupdateexecution := true;
				doupdatecpuwindow := true;
			end;
		end;
	end;

	Synchronize(SyncFinishedParsing);
end;

procedure TDebugReader.Execute;
var
	tmp : AnsiString;
	bytesread, totalbytesread : DWORD;
const
	chunklen = 1000; // GDB usually sends 4K blocks, disassembly easily takes up to 20K
begin

	bytesread := 0;
	totalbytesread := 0;

	while not Terminated do begin

		// Add chunklen bytes to length, and set chunklen extra bytes to zero
		SetLength(tmp,1 + totalbytesread + chunklen);
		FillChar(tmp[1 + totalbytesread],chunklen + 1,0);

		// ReadFile returns when there's something to read
		if not ReadFile(hPipeRead, tmp[1 + totalbytesread], chunklen, bytesread, nil) or (bytesread = 0) then break;

		Inc(totalbytesread,bytesread);

		if not Terminated then begin

			// Assume fragments don't end nicely with TErrorBegin or TPrompt
			if GetLastAnnotation(tmp,totalbytesread,1 + totalbytesread + chunklen) in [TErrorBegin,TPrompt] then begin
				gdbout := tmp; // todo: I sure hope this is a deep copy...
				Analyze;

				// Reset storage
				totalbytesread := 0;
			end;
		end;
	end;
end;

end.
