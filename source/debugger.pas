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

    // CPU form variables
    Registers : TList;
    Disassembly : TStringList;
    Backtrace : TList;

    constructor Create;
    destructor  Destroy; override;

    procedure Start;
    procedure Stop(Sender : TObject);
    procedure SendCommand(const command, params : AnsiString);

    // CPU window
    procedure SetRegisters(Listin : TList);
    procedure SetDisassembly(StringListin : TStringList);
    procedure SetBacktrace(Listin : TList);

    // breakpoints
    procedure AddBreakPoint(i : integer);
    procedure RemoveBreakPoint(i : integer);
    procedure RemoveBreakpoints;

    // watch var
    procedure AddWatchVar(i : integer);
    procedure DeleteWatchVar(i : integer);
    procedure RefreshWatchVars;
  end;

implementation

uses 
  main, devcfg, utils, MultiLangSupport, cpufrm, prjtypes, StrUtils;

constructor TDebugger.Create;
begin
	inherited;
end;

destructor TDebugger.Destroy;
begin
	Stop(nil);
	inherited;
end;

procedure TDebugger.SetRegisters(Listin : TList);
begin
	Registers := Listin;
	Reader.Registers := Listin;
end;

procedure TDebugger.SetDisassembly(StringListin : TStringList);
begin
	Disassembly := StringListin;
	Reader.Disassembly := StringListin;
end;

procedure TDebugger.SetBacktrace(Listin : TList);
begin
	Backtrace := Listin;
	Reader.Backtrace := Listin;
end;

procedure TDebugger.Start;
var
	pi : TProcessInformation;
	si : TStartupInfo;
	sa : TSecurityAttributes;
	gdb : AnsiString;
begin
	Registers := nil;
	Disassembly := nil;
	Backtrace := nil;
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
		MsgErr('CreatePipeinput');

	if not SetHandleInformation(fInputwrite,HANDLE_FLAG_INHERIT,0) then
		MsgErr('SetHandleInformation inputwrite');

	// Set up the start up info struct.
	FillChar(si, sizeof(TStartupInfo), 0);
	si.cb := sizeof(TStartupInfo);
	si.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
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
	Reader.DebugTree := DebugTree;
	Reader.FreeOnTerminate := true;
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

		Registers := nil;
		Disassembly := nil;
		Backtrace := nil;

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

		//while not Reader.Idling do
		//	Sleep(1);

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

procedure TDebugger.RemoveBreakpoints;
begin
	SendCommand('delete','');
end;

procedure TDebugger.AddWatchVar(i : integer);
begin
	SendCommand('display',PWatchVar(MainForm.DebugTree.Items[i].Data)^.name);
end;

procedure TDebugger.DeleteWatchVar(i : integer);
begin
	SendCommand('undisplay',IntToStr(I));
end;

procedure TDebugger.RefreshWatchVars;
var
	I : integer;
begin
	// Variables that aren't found need to be re-displayed!
	for i := 0 to DebugTree.Items.Count - 1 do
		if SameStr(PWatchVar(DebugTree.Items[i].Data)^.value,'Not found in current context') then
			AddWatchVar(i);
end;

end.

