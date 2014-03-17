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
  Sysutils, Windows, Messages, Forms, Classes, Controls,
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
    fOutputRead : THandle;
    fOutputWrite : THandle;
    fInputRead : THandle;
    fInputWrite : THandle;
    fProcessID : THandle;
    fExecuting : boolean;
    fCommandChanged : boolean;
    fDebugTree : TTreeView;
    fLeftPageIndexBackup : integer;
    fBreakPointList : TList;
    fWatchVarList : TList;
    fOnEvalReady : TEvalReadyEvent;
    fReader : TDebugReader;
  public
    constructor Create;
    destructor  Destroy; override;

    // Play/pause
    procedure Start;
    procedure Stop;
    procedure SendCommand(const command,params : AnsiString;viewinui : boolean = false);

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

    // Access
    property Executing : boolean read fExecuting write fExecuting;
    property LeftPageIndexBackup : integer read fLeftPageIndexBackup write fLeftPageIndexBackup;
    property WatchVarList : TList read fWatchVarList write fWatchVarList;
    property BreakPointList : TList read fBreakPointList write fBreakPointList;
    property CommandChanged : boolean read fCommandChanged write fCommandChanged;
    property DebugTree : TTreeView read fDebugTree write fDebugTree;
    property OnEvalReady : TEvalReadyEvent read fOnEvalReady write fOnEvalReady;
    property Reader : TDebugReader read fReader write fReader;
  end;

implementation

uses 
  main, devcfg, utils, cpufrm;

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

procedure TDebugger.Start;
var
	pi : TProcessInformation;
	si : TStartupInfo;
	sa : TSecurityAttributes;
	gdb : AnsiString;
	CompilerSet : TdevCompilerSet;
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

	// Use the GDB provided in the project if needed
	CompilerSet := devCompilerSets.CurrentSet;

	// Assume it's present in the first bin dir
	if CompilerSet.BinDir.Count > 0 then begin
		gdb := CompilerSet.BinDir[0] + pd + CompilerSet.gdbName;
		if not CreateProcess(nil, PAnsiChar('"' + gdb + '"' + ' --annotate=2 --silent'), nil, nil, true, CREATE_NEW_CONSOLE, nil, nil, si, pi) then begin
			MsgErr('Error launching:' + #13#10#13#10 + gdb + #13#10#13#10 + SysErrorMessage(GetLastError));
			Executing := false;
			Exit;
		end;
	end else
		MsgErr('Error launching debugger: no executable directory provided!');

	fProcessID := pi.hProcess;

	// Create a thread that will read GDB output.
	Reader := TDebugReader.Create(true);
	Reader.PipeRead := fOutputRead;
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

		if WatchVarList.Count = 0 then // nothing worth showing, restore view
			MainForm.LeftPageControl.ActivePageIndex := LeftPageIndexBackup;

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

procedure TDebugger.SendCommand(const Command, Params : AnsiString; ViewInUI : boolean);
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

		if ViewInUI then
			if (not CommandChanged) or (MainForm.edGdbCommand.Text = '') then begin
				// Convert command to C string
				if Length(params) > 0 then
					MainForm.edGdbCommand.Text := Command + ' ' + params
				else
					MainForm.edGdbCommand.Text := Command;

				CommandChanged := false;
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
//	wparent^.value := 'Execute to evaluate';
	wparent^.gdbindex := -1; // filled by GDB
	WatchVarList.Add(wparent);

	// Add parent to GUI
	parentnode := DebugTree.Items.AddObject(nil,wparent^.name + ' = Execute to evaluate',wparent);
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
		if PWatchVar(WatchVarList.Items[i])^.gdbindex = -1 then
			AddWatchVar(i); // resends command to display to GDB
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
			wparent^.node.Text := wparent^.name + ' = Execute to evaluate';
		end;
	end;
	DebugTree.Items.EndUpdate;
end;

end.

