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
  Sysutils, Windows, Messages, Forms, Classes, ShellAPI, Dialogs, Controls,
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
    GDBcommandchanged : boolean;

    DebugTree : TTreeView;
    BreakPointList : TList;
    WatchVarList : TList;

    OnEvalReady : TEvalReadyEvent;

    constructor Create;
    destructor  Destroy; override;

    procedure Start;
    procedure Stop;
    procedure SendCommand(const command,params : AnsiString;viewinui : boolean = false);

    // CPU window
    procedure SetRegisters(Listin : TList);
    procedure SetDisassembly(StringListin : TStringList);
    procedure SetBacktrace(Listin : TList);

    // breakpoints
    procedure AddBreakPoint(i : integer); overload;
    procedure RemoveBreakPoint(i : integer); overload;

    procedure AddBreakPoint(Linein : integer;e : TEditor); overload;
    procedure RemoveBreakPoint(Linein : integer;e : TEditor); overload;

    procedure DeleteBreakPointsOf(editor : TEditor);

    // watch var
    procedure AddWatchVar(i : integer); overload;
    procedure RemoveWatchVar(i : integer); overload;

    procedure AddWatchVar(const namein : AnsiString); overload;
    procedure RemoveWatchVar(nodein : TTreeNode); overload;

    procedure RefreshWatchVars;
    procedure DeleteWatchVars(deleteparent : boolean);
  end;

implementation

uses 
  main, devcfg, utils, MultiLangSupport, cpufrm, prjtypes, StrUtils;

constructor TDebugger.Create;
begin
	inherited;
	BreakPointList := TList.Create;
	WatchVarList := TList.Create;
end;

destructor TDebugger.Destroy;
var
	I : integer;
begin
	Stop;

	for i := 0 to WatchVarList.Count - 1 do
		Dispose(PWatchVar(WatchVarList.Items[i]));
	WatchVarList.Free;

	// Remove the breakpoints
	for i := 0 to BreakPointList.Count - 1 do
		Dispose(PBreakPoint(BreakPointList.Items[i]));
	BreakPointList.Free;

	inherited;
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
	si.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW or STARTF_USESHOWWINDOW;
	si.hStdInput := fInputread;
	si.hStdOutput := fOutputwrite;
	si.hStdError := fOutputwrite;
	si.wShowWindow := SW_HIDE;

	// Load GDB exe used by project set
	MainForm.fCompiler.SwitchToProjectCompilerSet;
	gdb := devCompiler.gdbName;

	if not CreateProcess(nil, PAnsiChar('"' + devCompiler.BinDir + pd + gdb + '"' + ' --annotate=2 --silent'), nil, nil, true, CREATE_NEW_CONSOLE, nil, nil, si, pi) then begin
		MsgErr('Error launching:' + #13#10#13#10 + devCompiler.BinDir + pd + gdb + #13#10#13#10 + SysErrorMessage(GetLastError));
		Executing := false;
		MainForm.fCompiler.SwitchToOriginalCompilerSet;
		Exit;
	end;

	MainForm.fCompiler.SwitchToOriginalCompilerSet;

	fProcessID := pi.hProcess;

	// Create a thread that will read GDB output.
	Reader := TDebugReader.Create(true);
	Reader.hPipeRead := fOutputread;
	Reader.FreeOnTerminate := true;
	Reader.BreakpointList := BreakPointList;
	Reader.WatchVarList := WatchVarList;
	Reader.DebugTree := DebugTree;
	Reader.Resume;

	MainForm.UpdateAppTitle;

	Application.HintHidePause := 5000;
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

		MainForm.UpdateAppTitle;

		Application.HintHidePause := 2500;
	end;
end;

procedure TDebugger.SendCommand(const command,params : AnsiString;viewinui : boolean);
var
	P : PAnsiChar;
	nBytesWrote : DWORD;
begin
	if Executing then begin

		// Convert command to C string
		if Length(params) > 0 then begin
			GetMem(P,Length(command) + Length(params) + 3);
			StrPCopy(P, command + ' ' + params + #10)
		end else begin
			GetMem(P,Length(command) + 2);
			StrPCopy(P, command + #10);
		end;

		if not WriteFile(fInputwrite, P^, strlen(P), nBytesWrote, nil) then
			MsgErr('Error writing to GDB');

		if viewinui then
			if (not GDBcommandchanged) or (MainForm.edGdbCommand.Text = '') then begin
				// Convert command to C string
				if Length(params) > 0 then
					MainForm.edGdbCommand.Text := command + ' ' + params
				else
					MainForm.edGdbCommand.Text := command;

				GDBcommandchanged := false;
			end;

		FreeMem(P);
	end;
end;

procedure TDebugger.AddBreakpoint(i : integer);
var
	filename : AnsiString;
begin
	// "filename":linenum
	filename := StringReplace(PBreakPoint(BreakPointList.Items[i])^.editor.FileName,'\','/',[rfReplaceAll]);
	SendCommand('break','"' + filename + '":' + inttostr(PBreakPoint(BreakPointList.Items[i])^.line),true);
end;

procedure TDebugger.RemoveBreakpoint(i : integer);
var
	filename : AnsiString;
begin
	// "filename":linenum
	filename := StringReplace(PBreakPoint(BreakPointList.Items[i])^.editor.FileName,'\','/',[rfReplaceAll]);
	SendCommand('clear','"' + filename + '":' + inttostr(PBreakPoint(BreakPointList.Items[i])^.line),true);
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

procedure TDebugger.DeleteBreakPointsOf(editor : TEditor);
var
	I : integer;
begin
	// Breakpoints in closed files need to be deleted
	for i := BreakPointList.Count - 1 downto 0 do

		if PBreakPoint(BreakPointList.Items[i])^.editor = editor then begin

			// Remove from list
			Dispose(PBreakPoint(BreakPointList.Items[i]));
			BreakPointList.Delete(i);
		end;
end;

procedure TDebugger.AddWatchVar(i : integer);
begin
	SendCommand('display',PWatchVar(WatchVarList.Items[i])^.name, true);
end;

procedure TDebugger.RemoveWatchVar(i : integer);
begin
	SendCommand('undisplay',IntToStr(PWatchVar(WatchVarList.Items[i])^.gdbindex), true);
end;

procedure TDebugger.AddWatchVar(const namein : AnsiString);
var
	parentnode : TTreeNode;
	I : integer;
	wparent : PWatchVar;
begin

	// Don't allow duplicates...
	for I := 0 to WatchVarList.Count - 1 do
		if SameStr(PWatchVar(WatchVarList.Items[i])^.name,namein) then
			Exit;

	// Add parent to list
	wparent := New(PWatchVar);
	wparent^.name := namein;
	wparent^.value := 'Execute to evaluate';
	wparent^.gdbindex := -1; // filled by GDB
	WatchVarList.Add(wparent);

	// Add parent to GUI
	parentnode := DebugTree.Items.AddObject(nil,wparent^.name + ' = ' + wparent^.value,wparent);
	parentnode.ImageIndex := 21;
	parentnode.SelectedIndex := 21;

	// Refer to list from GUI
	wparent^.node := parentnode;

	// Debugger already running? Add it to GDB
	if Executing then
		AddWatchVar(WatchVarList.Count-1);
end;

procedure TDebugger.RemoveWatchVar(nodein : TTreeNode);
var
	I : integer;
	wparent : PWatchVar;
begin
	for i := 0 to WatchVarList.Count - 1 do begin
		wparent := PWatchVar(WatchVarList.Items[I]);

		if SameStr(wparent^.name,PWatchVar(nodein.Data)^.name) then begin

			// Debugger already running and GDB scanned this one? Remove it from GDB
			if Executing and (wparent^.gdbindex <> -1) then
				RemoveWatchVar(i);

			// Remove from UI
			nodein.DeleteChildren;
			nodein.Delete;

			// Remove from list
			Dispose(PWatchVar(WatchVarList.Items[i]));
			WatchVarList.Delete(i);

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

procedure TDebugger.DeleteWatchVars(deleteparent : boolean);
var
	I : integer;
	wparent : PWatchVar;
begin
	DebugTree.Items.BeginUpdate;
	for I := WatchVarList.Count - 1 downto 0 do begin
		wparent := PWatchVar(WatchVarList.Items[I]);

		if deleteparent then begin

			// Remove from UI
			if wparent^.node.HasChildren then
			wparent^.node.DeleteChildren;
				wparent^.node.Delete;

			// Remove from list
			Dispose(PWatchVar(WatchVarList.Items[i]));
			WatchVarList.Delete(i);
		end else begin

			// Remove from UI
			if wparent^.node.HasChildren then
				wparent^.node.DeleteChildren;

			// Leave parent node intact...
			wparent^.gdbindex := -1;
			wparent^.value := 'Execute to evaluate';
			wparent^.node.Text := wparent^.name + ' = ' + wparent^.value;
		end;
	end;
	DebugTree.Items.EndUpdate;
end;

end.

