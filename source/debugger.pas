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

unit debugger;

interface

uses 
{$IFDEF WIN32}
  Sysutils, Windows, Messages, Classes, ShellAPI, Dialogs, Controls,
  debugreader, version, editor, ComCtrls;
{$ENDIF}
{$IFDEF LINUX}
  Sysutils, Classes, QDialogs, QControls,
  debugreader, version, editor, QComCtrls;
{$ENDIF}

type

  TEvalReadyEvent = procedure(const evalvalue : AnsiString) of object;

  TDebugger = class(TObject)
  private

    // GDB process info
    fOutputread : THandle;
    fOutputwrite : THandle;
    fInputread : THandle;
    fInputwrite : THandle;
    fProcessID : THandle;

    // GDB communication thread
    Reader : TDebugReader;

  public
    Executing : boolean;

    DebugTree : TTreeView;
    BreakPointList : TList;
    WatchVarList : TList;

    OnEvalReady : TEvalReadyEvent;

    constructor Create;
    destructor  Destroy; override;

    procedure Start;
    procedure Stop(Sender : TObject);
    procedure SendCommand(const command, params : AnsiString);

    // CPU window
    procedure SetRegisters(Listin : TList);
    procedure SetDisassembly(StringListin : TStringList);
    procedure SetBacktrace(Listin : TList);

    // Watch var tooltip
    procedure SetHintedWatchVar(const namein : AnsiString);

    // breakpoints
    procedure AddBreakPoint(i : integer); overload;
    procedure RemoveBreakPoint(i : integer); overload;

    procedure AddBreakPoint(Linein : integer;e : TEditor); overload;
    procedure RemoveBreakPoint(Linein : integer;e : TEditor); overload;

    // watch var
    procedure AddWatchVar(i : integer); overload;
    procedure RemoveWatchVar(i : integer); overload;

    function AddWatchVar(const namein : AnsiString) : PWatchVar; overload;
    procedure RemoveWatchVar(nodein : TTreeNode); overload;

    procedure RefreshWatchVars;
  end;

implementation

uses 
  main, devcfg, utils, MultiLangSupport, cpufrm, prjtypes, StrUtils;

constructor TDebugger.Create;
begin
	BreakPointList := TList.Create;
	WatchVarList := TList.Create;
	inherited;
end;

destructor TDebugger.Destroy;
var
	I : integer;
begin
	Stop(nil);

	// Remove watch vars (list is contained in UI component)
	for i := 0 to WatchVarList.Count - 1 do
		Dispose(PWatchVar(WatchVarList.Items[i]));
	WatchVarList.Free;

	// Remove the breakpoints
	for i := 0 to BreakPointList.Count - 1 do
		Dispose(PBreakPoint(BreakPointList.Items[i]));
	BreakPointList.Free;

	inherited;
end;

procedure TDebugger.SetHintedWatchVar(const namein : AnsiString);
begin
	Reader.hintedvar := namein;
end;

procedure TDebugger.SetRegisters(Listin : TList);
begin
	Reader.Registers := Listin;
end;

procedure TDebugger.SetDisassembly(StringListin : TStringList);
begin
	Reader.Disassembly := StringListin;
end;

procedure TDebugger.SetBacktrace(Listin : TList);
begin
	Reader.Backtrace := Listin;
end;

procedure TDebugger.Start;
var
	pi : TProcessInformation;
	si : TStartupInfo;
	sa : TSecurityAttributes;
	gdb : AnsiString;
begin
	Executing := true;

	// Set up the security attributes struct.
	sa.nLength := sizeof(TSecurityAttributes);
	sa.lpSecurityDescriptor := nil;
	sa.bInheritHandle := true;

	// Create the child output pipe.
	if not CreatePipe(fOutputread, fOutputwrite, @sa, 0) then
		MsgErr('CreatePipe output');

	if not SetHandleInformation(fOutputread,HANDLE_FLAG_INHERIT,0) then
		MsgErr('SetHandleInformation outputread');

	// Create the child input pipe.
	if not CreatePipe(fInputread, fInputwrite, @sa, 0) then
		MsgErr('CreatePipe input');

	if not SetHandleInformation(fInputwrite,HANDLE_FLAG_INHERIT,0) then
		MsgErr('SetHandleInformation inputwrite');

	// Set up the start up info struct.
	FillChar(si, sizeof(TStartupInfo), 0);
	si.cb := sizeof(TStartupInfo);
	si.dwFlags := STARTF_USESTDHANDLES;
	si.hStdInput := fInputread;
	si.hStdOutput := fOutputwrite;
	si.hStdError := fOutputwrite;
	si.wShowWindow := SW_HIDE;

	if (devCompiler.gdbName <> '') then
		gdb := devCompiler.gdbName
	else
		gdb := GDB_PROGRAM;

	if not CreateProcess(nil,PAnsiChar(gdb + ' --annotate=2 --silent'), nil, nil, true,CREATE_NEW_CONSOLE, nil, nil, si, pi) then begin
		MsgErr('Error launching ' + gdb);
		exit;
	end;

	fProcessID := pi.hProcess;

	// Create a thread that will read GDB output.
	Reader := TDebugReader.Create(true);
	Reader.hPipeRead := fOutputread;
	Reader.FreeOnTerminate := true;
	Reader.BreakpointList := BreakPointList;
	Reader.WatchVarList := WatchVarList;
	Reader.DebugTree := DebugTree;
	Reader.Resume;
end;

procedure TDebugger.Stop;
begin
	if Executing then begin
		Executing := false;

		// Close CPU window
		if Assigned(CPUForm) then
			CPUForm.Close;

		TerminateProcess(fProcessID, 0); // stop gdb

		Reader.Terminate;
		Reader := nil;

		// Free resources
		if not CloseHandle(fProcessID) then
			MsgErr('CloseHandle - gdb process');
		//if not CloseHandle(outputread) then // hangs?
		//	DisplayError('CloseHandle - output read');
		if not CloseHandle(fOutputwrite) then
			MsgErr('CloseHandle - output write');
		if not CloseHandle(fInputread) then
			MsgErr('CloseHandle - input read');
		//if not CloseHandle(inputwrite) then
		//	DisplayError('CloseHandle - input write');

		MainForm.RemoveActiveBreakpoints;
	end;
end;

procedure TDebugger.SendCommand(const command, params : AnsiString);
var
	P : array [0..512] of char;
	nBytesWrote : DWORD;
begin
	if Executing then begin

		// Convert command to C string
		if Length(params) > 0 then
			StrPCopy(P, command + ' ' + params + #10)
		else
			StrPCopy(P, command + #10);

		if not WriteFile(fInputwrite, P, strlen(P), nBytesWrote, nil) then
			MsgErr('Error writing to GDB');
	end;
end;

procedure TDebugger.AddBreakpoint(i : integer);
var
	arguments : AnsiString;
begin
	// "filename":linenum
	arguments := '"' + PBreakPoint(BreakPointList.Items[i])^.editor.FileName + '":' + inttostr(PBreakPoint(BreakPointList.Items[i])^.line);
	SendCommand('break',arguments);
end;

procedure TDebugger.RemoveBreakpoint(i : integer);
var
	arguments : AnsiString;
begin
	// "filename":linenum
	arguments := '"' + PBreakPoint(BreakPointList.Items[i])^.editor.FileName + '":' + inttostr(PBreakPoint(BreakPointList.Items[i])^.line);
	SendCommand('clear',arguments);
end;

procedure TDebugger.AddBreakPoint(linein : integer;e : TEditor);
var
	APBreakPoint : PBreakPoint;
begin
	APBreakPoint := new(PBreakPoint);
	with APBreakPoint^ do begin
		line := Linein;
		editor := e;
	end;
	BreakPointList.Add(APBreakPoint);

	// Debugger already running? Add it to GDB
	if Executing then
		AddBreakPoint(BreakPointList.Count-1);
end;

procedure TDebugger.RemoveBreakPoint(Linein : integer;e : TEditor);
var
	i : integer;
begin
	for i := 0 to BreakPointList.Count - 1 do begin
		if (PBreakPoint(BreakPointList.Items[i])^.line = Linein) and (PBreakPoint(BreakPointList.Items[i])^.editor = e) then begin

			// Debugger already running? Remove it from GDB
			if Executing then
				RemoveBreakPoint(i);

			// Remove from list
			Dispose(PBreakPoint(BreakPointList.Items[i]));
			BreakPointList.Delete(i);
			break;
		end;
	end;
end;

procedure TDebugger.AddWatchVar(i : integer);
begin
	SendCommand('display',PWatchVar(WatchVarList.Items[i])^.name);
end;

procedure TDebugger.RemoveWatchVar(i : integer);
begin
	SendCommand('undisplay',IntToStr(PWatchVar(WatchVarList.Items[i])^.gdbindex));
end;

function TDebugger.AddWatchVar(const namein : AnsiString) : PWatchVar;
var
	newnode : TTreeNode;
	I : integer;
	wvar : PWatchVar;
begin

	// Don't allow duplicates...
	for I := 0 to WatchVarList.Count - 1 do
		if SameStr(PWatchVar(WatchVarList.Items[i])^.name,namein) then begin
			result := WatchVarList.Items[i];
			Exit;
		end;

	// Add to list
	wvar := New(PWatchVar);
	with wvar^ do begin
		name := namein;
		value := 'Execute to evaluate';
		gdbindex := -1; // filled by GDB
	end;
	WatchVarList.Add(wvar);

	result := wvar;

	// Add to GUI
	newnode := DebugTree.Items.AddObject(nil,wvar^.name + ' = ' + wvar^.value,wvar);
	newnode.ImageIndex := 21;
	newnode.SelectedIndex := 21;

	// Refer to list from GUI
	with wvar^ do begin
		node := newnode;
	end;

	// Debugger already running? Add it to GDB
	if Executing then
		AddWatchVar(WatchVarList.Count-1);
end;

procedure TDebugger.RemoveWatchVar(nodein : TTreeNode);
var
	I : integer;
begin
	for i := 0 to WatchVarList.Count - 1 do begin
		if SameStr(PWatchVar(WatchVarList.Items[i])^.name,PWatchVar(nodein.Data)^.name) then begin

			// Debugger already running and GDB scanned this one? Remove it from GDB
			if Executing and (PWatchVar(WatchVarList.Items[i])^.gdbindex <> -1) then
				RemoveWatchVar(i);

			// Remove from list
			Dispose(PWatchVar(WatchVarList.Items[i]));
			WatchVarList.Delete(i);

			// Remove from UI
			DebugTree.Items.Delete(nodein);

			break;
		end;
	end;
end;

procedure TDebugger.RefreshWatchVars;
var
	I : integer;
begin
	// Variables that aren't found need to be re-displayed!
	for i := 0 to WatchVarList.Count - 1 do
		if SameStr(PWatchVar(WatchVarList.Items[i])^.value,'Not found in current context') then
			AddWatchVar(i);
end;

end.

