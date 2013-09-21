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
  Sysutils, Classes, Windows, ShellAPI,
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
                   TFrameSourceFile, TFrameSourceBegin, TFrameSourceLine, TFrameFunctionName,
                   TFrameArgs,
                   TFrameBegin,TFrameEnd,
                   TErrorBegin, TErrorEnd,
                   TExit,
                   TValueHistory,
                   TArgBegin, TArgEnd,TArgValue,TArgNameEnd,
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

  TDebugReader = class(TThread)
  public
    hPipeRead : THandle;
    Registers : TList;
    Disassembly : TStringList; // convert to TList with proper data formatting?
    Backtrace : TList;
    BreakpointList : TList;
    WatchVarList : TList;
    DebugTree : TTreeView;
    hintedvar : AnsiString;
  private
    curpos : integer;
    len : integer;
    bline : integer;
    bfile : AnsiString;
    gdbout : AnsiString;
    evalvalue : AnsiString;

	// attempt to cut down on Synchronize calls
	dobacktraceready : boolean;
	dodisassemblerready : boolean;
	doregistersready : boolean;
	dorescanwatches : boolean;
	doevalready : boolean;
	doprocessexited : boolean;
	doupdateexecution : boolean;

    procedure Analyze;

    // synching with GUI
    procedure SyncFinishedParsing;

    // parsing
    procedure SkipSpaces; // skips space and tab
    procedure SkipToAnnotation; // skips until it finds #26#26 (GDB annotation for interfaces)
    function FindAnnotation(an : TAnnotateType) : boolean; // Finds the given annotation, returns false on EOF
    function GetNextAnnotation : TAnnotateType; // Returns the next annotation
    function PeekAnnotation : TAnnotateType; // Returns the next annotation, but does not modify current scanning positions
    function GetNextWord : AnsiString; // copies the next word, stops when it finds chars 0..32
    function GetNextLine : AnsiString; // skips until enter sequence, skips ONE enter sequence, copies until next enter sequence
    function GetNextFilledLine : AnsiString; // skips until enter sequence, skips enter sequences, copies until next enter sequence
    function GetRemainingLine : AnsiString; // copies until enter sequence

  protected
    procedure Execute; override;
  end;

implementation

uses
  main, devcfg, CPUFrm, debugger, utils;

// macro for all the things that need to be done when we are finished parsing the current block
procedure TDebugReader.SyncFinishedParsing;
begin
	if doprocessexited then begin
		MainForm.fDebugger.Stop(nil);
		Exit;
	end;

	if doevalready then begin
		if Assigned(MainForm.fDebugger.OnEvalReady) then
			MainForm.fDebugger.OnEvalReady(evalvalue)
		else
			MainForm.EvalOutput.Text := evalvalue;
	end;

	// Delete unimportant stuff to reduce clutter
	gdbout := StringReplace(gdbout,#26,'->',[rfReplaceAll]);
	//gdbout := StringReplace(gdbout,'->->pre-prompt'#13#10,'',[rfReplaceAll]);
	//gdbout := StringReplace(gdbout,'->->prompt'#13#10,'',[rfReplaceAll]);
	//gdbout := StringReplace(gdbout,'->->post-prompt'#13#10,'',[rfReplaceAll]);
	MainForm.DebugOutput.Lines.Add(gdbout);
	//MainForm.DebugOutput.Lines.Add('-----------------------------');

	if doregistersready then
		CPUForm.OnRegistersReady;

	if dodisassemblerready then
		CPUForm.OnAssemblerReady;

	if dobacktraceready then
		CPUForm.OnBacktraceReady;

	if doupdateexecution then begin
		MainForm.GotoBreakpoint(bfile, bline); // set active line
		MainForm.fDebugger.RefreshWatchVars; // update variable information

		if Assigned(CPUForm) then begin
			MainForm.fDebugger.SendCommand('disas','');
			MainForm.fDebugger.SendCommand('info','registers');
			MainForm.fDebugger.SendCommand('backtrace','');
		end;
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
	if (curpos+1 < len) and (gdbout[curpos] = #13) and (gdbout[curpos] = #13) then // DOS
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

function TDebugReader.PeekAnnotation : TAnnotateType;
var
	oldpos : integer;
begin
	oldpos := curpos;
	Result := GetNextAnnotation;
	curpos := oldpos;
end;

function TDebugReader.GetNextAnnotation : TAnnotateType;
var
	s : AnsiString;
	oldpos : integer;
begin
	// Skip until end of #26's, i.e. GDB formatted output
	SkipToAnnotation;

	// Get part this line, after #26#26
	s := GetNextWord;

	if SameStr(s,'pre-prompt') then
		result := TPrePrompt
	else if SameStr(s,'prompt') then
		result := TPrompt
	else if SameStr(s,'post-prompt') then begin // todo: clean up
		result := TPostPrompt;

		oldpos := curpos;
		s := GetNextFilledLine;
		curpos := oldpos;

		// Hack fix to catch register dump
		if Assigned(Registers) and Assigned(CPUForm) then
			if StartsStr('rax ',s) or StartsStr('eax ',s) then
				result := TInfoReg;

		// Another hack to catch assembler
		if Assigned(Disassembly) and Assigned(CPUForm) then
			if StartsStr('Dump of assembler code for function ',s) then
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
	else if SameStr(s,'value-history-value') then
		result := TValueHistory
	else if (curpos = len) then
		result := TEOF
	else
		result := TUnknown;
end;

procedure TDebugReader.Analyze;
var
	s,t,u : AnsiString;
	i,x,y : integer; // dump
	wvar : PWatchVar;
	reg : PRegister;
	trace : PTrace;
begin

	evalvalue := '';
	len := Length(gdbout);
	curpos := 1;

	dobacktraceready := false;
	dodisassemblerready := false;
	doregistersready := false;
	dorescanwatches := false;
	doevalready := false;
	doprocessexited := false;
	doupdateexecution := false;

	while curpos < len do begin
		case GetNextAnnotation of
			TValueHistory : begin
				evalvalue := GetNextLine; // value, might be empty
				if SameStr(evalvalue,'') then
					evalvalue := 'Error evaluating input';

				doevalready := true;
			end;
			TExit : begin
				doprocessexited := true;
			end;
			TFrameBegin : begin
				s := GetNextFilledLine;
				if Assigned(Backtrace) and Assigned(CPUForm) and StartsStr('#',s) then begin

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
					if SameStr(s,'()') then begin // empty param list
						t := '()';
					end else begin // walk past args

						t := '(';
						while (PeekAnnotation = TArgBegin) do begin

							// argument name
							if not FindAnnotation(TArgBegin) then begin
								Dispose(PTrace(trace));
								Exit;
							end;

							t := t + GetNextFilledLine;

							// =
							if not FindAnnotation(TArgNameEnd) then begin
								Dispose(PTrace(trace));
								Exit;
							end;

							t := t + ' ' + GetNextFilledLine + ' '; // should be =

							// argument value
							if not FindAnnotation(TArgValue) then begin
								Dispose(PTrace(trace));
								Exit;
							end;

							t := t + GetNextFilledLine;

							// argument end
							if not FindAnnotation(TArgEnd) then begin
								Dispose(PTrace(trace));
								Exit;
							end;

							// check if we're done, else add comma
							if PeekAnnotation = TArgBegin then t := t + ', ';
						end;
						t := t + ')';
					end;

					trace^.funcname := trace^.funcname + t;

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

					Backtrace.Add(trace);

					// Skip over the remaining frame part...
					if not FindAnnotation(TFrameEnd) then Exit;

					// Not another one coming? Done!
					if PeekAnnotation <> TFrameBegin then begin

						// End of stack trace dump!
						dobacktraceready := true;
					end;
				end;
			end;
			TInfoAsm : begin

				// Skip info messages
				s := GetNextLine; // Dump of ... foo()

				// the current function name will be saved at index 0
				Disassembly.Add(Copy(s,37,Length(s)-37));

				s := GetNextLine;

				// Add lines of disassembly
				while not SameStr('End of assembler dump.',s) do begin
					Disassembly.Add(s);
					s := GetNextLine;
				end;

				// Is Disassembly assigned? Assume CPU window called for this
				dodisassemblerready := true;
			end;
			TInfoReg : begin
				// Scan all registers until end of gdb output

				s := GetNextFilledLine;

				repeat

					// name(spaces)hexvalue(tab)decimalvalue
					reg := new(PRegister);
					x := Pos(' ',s);
					if x > 0 then begin
						reg^.name := Copy(s,1,x-1);
						y := Pos(#9,s);
						if y > 0 then
							reg^.value := Copy(s,y+1,Length(s)-y+1);
					end;

					Registers.Add(reg);

					s := GetNextFilledLine;

				until StartsStr(#26#26,s);

				// Is Registers assigned? Assume CPU window called for this
				doregistersready := true;
			end;
			TErrorBegin : begin
				s := GetNextLine; // error text
				if StartsStr('No symbol "',s) then begin
					x := Pos('"',s);
					y := GetLastPos('"',s);
					t := Copy(s,x+1,y-x-1);

					// Update current...
					for I := 0 to WatchVarList.Count - 1 do begin
						wvar := PWatchVar(WatchVarList.Items[I]);
						if SameStr(wvar^.name,t) then begin

							wvar^.value := 'Not found in current context';
							wvar^.node.Text := wvar^.name + ' = ' + wvar^.value;
							break;
						end;
					end;

					dorescanwatches := true;
				end;
			end;
			TDisplayBegin : begin

				s := GetNextLine; // watch index

				if not FindAnnotation(TDisplayExpression) then Exit;

				t := GetNextLine; // variable name

				if not FindAnnotation(TDisplayExpression) then Exit;

				u := GetNextLine; // variable value (can be empty)
				if u = '' then
					u := 'Error evaluating variable';

				// Update...
				for I := 0 to WatchVarList.Count - 1 do begin
					wvar := PWatchVar(WatchVarList.Items[I]);
					if SameStr(wvar^.name,t) then begin

						wvar^.gdbindex := StrToInt(s);
						wvar^.value := u;

						wvar^.node.Text := wvar^.name + ' = ' + wvar^.value;

						break;
					end;
				end;
			end;
			TSource : begin // source filename:line:offset:beg/middle/end:addr
				s := TrimLeft(GetRemainingLine);

				// remove offset, beg/middle/end, addr
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
			end;
		end;
	end;

	Synchronize(SyncFinishedParsing);
end;

procedure TDebugReader.Execute;
var
	tmp : array [0..8192] of char; // 1 extra
	bytesread : DWORD;
begin
	bytesread := 0;
	while not Terminated do begin

		FillChar(tmp,bytesread+1,0);

		// ReadFile returns when there's something to read
		if not ReadFile(hPipeRead, tmp, 8192, bytesread, nil) or (bytesread = 0) then break;

		gdbout := gdbout + tmp;

		if not Terminated and (Pos('(gdb)',gdbout) > 0) then begin //(GDB does NOT always prompt when ready?)
			Analyze;
			gdbout := '';
		end;
	end;
end;

end.
