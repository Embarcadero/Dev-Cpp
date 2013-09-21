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
  Sysutils, Windows, Classes, ShellAPI, Dialogs, Controls,
  debugreader, debugwait, version, editor, ComCtrls;
{$ENDIF}
{$IFDEF LINUX}
  Sysutils, Classes, QDialogs, QControls,
  debugreader, debugwait, version, editor, QComCtrls;
{$ENDIF}

type
  TGdbBreakpoint = class
  public
    Index  : integer;
    Editor : TEditor;
    // RNC add the file that this breakpoint is in
    filename : string;
    Line   : integer;
    // RNC 07.02.2004 -- A variable to hold GDB's index for this breakpoint
    BreakPointIndex : integer;
  end;

  TDebugger = class
  constructor Create;
  destructor  Destroy; override;
  private
    fIncDirs: string;
    // RNC 07.02.2004 -- Increments each time a breakpoint is added to GDB.  And is the id for the next breakpoint in gdb
    breakPointCount : integer;

    function GetCallStack: TList;
    function GetWatchValue: string;
    function GetWatchVar: string;
  published
    property CallStack : TList read GetCallStack;
    property WatchVar: string read GetWatchVar;
    property WatchValue: string read GetWatchValue;

  public
    FileName  : string;
    Executing : boolean;
    DebugTree : TTreeView;
    InAssembler : boolean;
    Registers : TRegisters;
    OnRegistersReady : procedure of object;
    procedure Execute;
    procedure SendCommand(command, params : string);
    //RNC Change Add/Remove Breakpoint to take an index into the 1 array of breakpoints
    function AddBreakpoint(i:integer):integer;
    procedure RemoveBreakpoint(i : integer);
    procedure RemoveAllBreakpoints;
    procedure RefreshContext(); // tries to display variable considered not in current context
    procedure CloseDebugger(Sender: TObject);
    procedure AddIncludeDir(s: string);
    procedure ClearIncludeDirs;
    function  Idle : boolean;
    function  IsIdling : boolean;

    // RNC 07-02-2004 3 new functions:
    // Check to see if the debugger is currently stopped at a breakpoint
    function  IsBroken : boolean;
    // Set whether or not the debugger is currently at a breakpoint
    procedure SetBroken(b: boolean);
    // Check to see if the given line is already a breakpoint
    //RNC change to take a filename, not an editor.  an editor is destroyed when the
    // file is closed. however, the filename does not change.
    function BreakpointExists(filename: string; line: integer):boolean;

  protected
    hInputWrite : THandle;
    hOutputRead : THandle;
//    hStdIn      : THandle; // Handle to parents std input.
    hPid	: THandle; // GDB process id

    Reader      : TDebugReader;
    Wait        : TDebugWait;
    EventReady  : THandle;
    Breakpoints : TList;
    GdbBreakCount : integer;

    procedure DisplayError(s : string);
    procedure Launch(hChildStdOut, hChildStdIn,
                     hChildStdErr : THandle);
    procedure OnDebugFinish(Sender : TObject);
    procedure OnNoDebuggingSymbolsFound;
    procedure OnSourceMoreRecent;
    // RNC a function to be called if we have a valid-frame but no source file
    // to open up
    procedure InaccessibleFunction;
    procedure OnAsmCode(s : string);
    procedure OnAsmFunc(s : string);
    procedure OnAsmCodeEnd;
    procedure OnSegmentationFault;

  end;

implementation

uses 
  main, devcfg, MultiLangSupport, cpufrm, prjtypes, StrUtils;

constructor TDebugger.Create;
begin
  EventReady := CreateEvent(nil, false, false, nil);
  Executing := false;
  FileName := '';
  GdbBreakCount := 1;
  fIncDirs:='';
  InAssembler := false;
end;

destructor TDebugger.Destroy;
begin
  if (Executing) then
    CloseDebugger(nil);
  CloseHandle(EventReady);
  inherited Destroy;
end;

/////////////////////////////////
// RNC 07-02-04 Get/Set the flag as to whether or not the debugger is at a breakpoint
procedure TDebugger.SetBroken(b: boolean);
begin
  Wait.broken := b;
end;


function TDebugger.IsBroken : boolean;
begin
  if Wait = nil then
    Result := False
  else
    Result := Wait.broken;
end;
////////////////////////////////////

function TDebugger.Idle : boolean;
var i : integer;
begin
  i := 0;
  result := false;
  while not Reader.Idling do begin
    Sleep(20);
//    Application.ProcessMessages;
    i := i + 1;
    if (i = 200) then begin
      MessageDlg('Wait timeout for debug command', mtError, [mbOK], 0);
      Reader.Idling := True;
      result := true;
    end;
  end;
end;

function TDebugger.IsIdling : boolean;
begin
  result := Reader.Idling;
end;

procedure TDebugger.Execute;
var
  hOutputReadTmp, hOutputWrite,
  hInputWriteTmp, hInputRead,
  hErrorWrite : THandle;
  sa : TSecurityAttributes;
begin
  Executing := true;
  // Set up the security attributes struct.
  sa.nLength := sizeof(TSecurityAttributes);
  sa.lpSecurityDescriptor := nil;
  sa.bInheritHandle := true;
  // Create the child output pipe.
  if (not CreatePipe(hOutputReadTmp, hOutputWrite, @sa, 0)) then
    DisplayError('CreatePipe');
  // Create a duplicate of the output write handle for the std error
  // write handle. This is necessary in case the child application
  // closes one of its std output handles.
  if (not DuplicateHandle(GetCurrentProcess(), hOutputWrite,
                          GetCurrentProcess(), @hErrorWrite, 0,
                          true, DUPLICATE_SAME_ACCESS)) then
    DisplayError('DuplicateHandle');

  // Create the child input pipe.
  if (not CreatePipe(hInputRead, hInputWriteTmp, @sa, 0)) then
    DisplayError('CreatePipe');

  // Create new output read handle and the input write handles.
  // The Properties are set to FALSE, otherwise the child inherits the
  // properties and as a result non-closeable handles to the pipes
  // are created.
  if (not DuplicateHandle(GetCurrentProcess(), hOutputReadTmp,
                          GetCurrentProcess(), @hOutputRead, // Address of new handle.
                          0, false, // Make it uninheritable.
                          DUPLICATE_SAME_ACCESS)) then
    DisplayError('DuplicateHandle');

  if (not DuplicateHandle(GetCurrentProcess(), hInputWriteTmp,
                          GetCurrentProcess(), @hInputWrite, // Address of new handle.
                          0, false, // Make it uninheritable.
                          DUPLICATE_SAME_ACCESS)) then
    DisplayError('DupliateHandle');
  // Close inheritable copies of the handles you we not want to be
  // inherited.
  if (not CloseHandle(hOutputReadTmp)) then
    DisplayError('CloseHandle');
  if (not CloseHandle(hInputWriteTmp)) then
    DisplayError('CloseHandle');

  // Get std input handle so we can close it and force the ReadFile to
  // fail when you want the input thread to exit.
//  hStdIn := GetStdHandle(STD_INPUT_HANDLE);
//  if (hStdIn = INVALID_HANDLE_VALUE) then
//    DisplayError('GetStdHandle');

  Launch(hOutputWrite, hInputRead, hErrorWrite);

  // Close pipe handles (do not continue to modify the parent).
  // Make sure that no handles of the
  // output pipe are maintained in this process or else the pipe will
  // not close when the child process exits and the ReadFile will hang.
  if (not CloseHandle(hOutputWrite)) then
    DisplayError('CloseHandle');
  if (not CloseHandle(hInputRead)) then
    DisplayError('CloseHandle');
  if (not CloseHandle(hErrorWrite)) then
    DisplayError('CloseHandle');

  Reader := TDebugReader.Create(true);
  // Create a thread that will notice when an output is ready to be analyzed
  Wait := TDebugWait.Create(true);
  Wait.broken := true;    // RNC 07-02-2004 Set broken to true before the debugger has actually started.  This allows breapoints to be set
  Wait.OnNoDebuggingSymbols := OnNoDebuggingSymbolsFound;
  Wait.OnSourceMoreRecent := OnSourceMoreRecent;
  // RNC set DebugWait's InaccessibleFunction to this one
  Wait.InaccessibleFunction := InaccessibleFunction;
  Wait.OnAsmCode := OnAsmCode;
  Wait.OnAsmFunc := OnAsmFunc;
  Wait.OnAsmCodeEnd := OnAsmCodeEnd;
  Wait.OnSegmentationFault := OnSegmentationFault;
  Wait.Event := EventReady;
  Wait.DebugTree := DebugTree;
  Wait.Registers := @Registers;
  Wait.OnTerminate := OnDebugFinish;
  Wait.FreeOnTerminate := true;
  Wait.Reader := Reader;
  Wait.Resume;

  // Create a thread that will read the child's output.
  Reader.hPipeRead := hOutputRead;
  Reader.EventReady := EventReady;
  Reader.OnTerminate := CloseDebugger;
  Reader.FreeOnTerminate := true;
  Reader.Idling := true;
  Reader.Resume;
  // RNC 07-02-2004  set the breakPointCount to 0
  breakPointCount := 0;
end;

procedure TDebugger.DisplayError(s : string);
begin
  MessageDlg('Error with debugging process : ' + s, mtError, [mbOK], 0);
end;

procedure TDebugger.Launch(hChildStdOut, hChildStdIn,
                           hChildStdErr : THandle);
var
  pi : TProcessInformation;
  si : TStartupInfo;
  // RNC send the include directories to GDB
  inc : string;
  gdb : string;
begin
  // Set up the start up info struct.
  FillChar(si, sizeof(TStartupInfo), 0);
  si.cb := sizeof(TStartupInfo);
  si.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
  si.hStdOutput := hChildStdOut;
  si.hStdInput  := hChildStdIn;
  si.hStdError  := hChildStdErr;
  si.wShowWindow := SW_HIDE;

  if (devCompiler.gdbName <> '') then
    gdb := devCompiler.gdbName
  else
    gdb := GDB_PROGRAM;
  // Launch process
  // RNC add quotes to the include directories (in case they have spaces)
  inc := StringReplace(fIncDirs, '  -', '"  -', [rfReplaceAll]);
  inc := StringReplace(inc, '=', '="',[rfReplaceAll]);
  inc := inc + '"';
  if (not CreateProcess(nil,
                        pchar(gdb + ' --annotate=2 --silent'), nil, nil, true,
                        CREATE_NEW_CONSOLE, nil, nil, si, pi)) then begin
    DisplayError('Could not find program file ' + gdb);
    exit;
  end;
  hPid := pi.hProcess;
  // Close any unnecessary handles.
  if (not CloseHandle(pi.hThread)) then
    DisplayError('CloseHandle');
end;

procedure TDebugger.CloseDebugger(Sender: TObject);
begin
  if Executing then begin
    SetBroken(false);    // RNC 07-02-2004 Set broken to false when exiting
    Executing := false;
    // Force the read on the input to return by closing the stdin handle.
    //  if (not CloseHandle(hStdIn)) then
    //    DisplayError('CloseHandle - stdin');
    Wait.Stop := True;
    SetEvent(EventReady);
    TerminateProcess(hPid, 0);
    Wait.Terminate;
    Reader.Terminate;
    Reader := nil;
    Wait := nil;
    if (not CloseHandle(hPid)) then
      DisplayError('CloseHandle - gdb process');
    if (not CloseHandle(hOutputRead)) then
      DisplayError('CloseHandle - output read');
    if (not CloseHandle(hInputWrite)) then
      DisplayError('CloseHandle - input write');
    MainForm.RemoveActiveBreakpoints;
  end;
end;

procedure TDebugger.SendCommand(command, params : string);
var
  s : array [0..512] of char;
  nBytesWrote : DWORD;
  i, j : integer;
begin
//  CurrentCommand := command;
  Reader.Idling := False;
  i := 0;
  while i < length(command) do begin
    s[i] := command[i + 1];
    i := i + 1;
  end;
  s[i] := ' ';
  i := i + 1;
  j := 0;
  while (j < length(params)) do begin
    s[i] := params[j + 1];
    i := i + 1;
    j := j + 1;
  end;
  s[i] := #10;
  s[i + 1] := #0;
  if (not WriteFile(hInputWrite, s, strlen(s), nBytesWrote, nil)) then begin
    if (GetLastError() = ERROR_NO_DATA) then
      //showmessage('Debug finished') //Pipe was closed (normal exit path).
    else
      DisplayError('WriteFile');
  end;
  if Assigned(OnRegistersReady) then
    OnRegistersReady;
//  Idle;
end;

procedure TDebugger.OnDebugFinish(Sender : TObject);
begin
  if Executing then
    CloseDebugger(sender);
end;

procedure TDebugger.OnNoDebuggingSymbolsFound;
var
  opt: TCompilerOption;
  idx: integer;
  spos: integer;
  opts: TProjOptions;
begin
  CloseDebugger(nil);
  if (MessageDlg(Lang[ID_MSG_NODEBUGSYMBOLS], mtConfirmation, [mbYes, mbNo], 0) = mrYes) then begin
    if devCompiler.FindOption('-g3', opt, idx) then begin
      opt.optValue:=1;
      if not Assigned(MainForm.fProject) then
        devCompiler.Options[idx]:=opt; // set global debugging option only if not working with a project

      MainForm.SetProjCompOpt(idx, True); // set the project's correpsonding option too

      // remove "-s" from the linker''s command line
      if Assigned(MainForm.fProject) then begin
        opts:=MainForm.fProject.Options;
        // look for "-s" in all the possible ways
        // NOTE: can't just search for "-s" because we might get confused with
        //       some other option starting with "-s...."
        spos:=Pos('-s ', opts.cmdLines.Linker); // following more opts
        if spos=0 then
          spos:=Pos('-s'#13, opts.cmdLines.Linker); // end of line
        if spos=0 then
          spos:=Pos('-s_@@_', opts.cmdLines.Linker); // end of line (dev 4.9.7.3+)
        if (spos=0) and
           (Length(opts.cmdLines.Linker)>=2) and // end of string
           (Copy(opts.cmdLines.Linker, Length(opts.cmdLines.Linker)-1, 2) = '-s') then
          spos := Length(opts.cmdLines.Linker)-1;
        // if found, delete it
        if spos>0 then begin
          Delete(opts.cmdLines.Linker, spos, 2);
          MainForm.fProject.Options:=opts;
        end;
      end;
      if devCompiler.FindOption('-s', opt, idx) then begin
        opt.optValue := 0;
        if not Assigned(MainForm.fProject) then
          devCompiler.Options[idx]:=opt; // set global debugging option only if not working with a project
        MainForm.SetProjCompOpt(idx, False); // set the project's correpsonding option too
      end;
      MainForm.actRebuildExecute(nil);
    end;
  end;
end;

// RNC function to continue if we are stuck debugging places we can't see 
// (ie, we entered a DLL)
procedure TDebugger.InaccessibleFunction;
begin
  MainForm.actStepOverExecute(nil);
end;


procedure TDebugger.OnSourceMoreRecent;
begin
  if (MessageDlg(Lang[ID_MSG_SOURCEMORERECENT], mtConfirmation, [mbYes, mbNo], 0) = mrYes) then begin
    CloseDebugger(nil);
    MainForm.actCompileExecute(nil);
  end;
end;

procedure TDebugger.OnSegmentationFault;
begin
  MessageDlg(Lang[ID_MSG_SEGFAULT], mtWarning, [mbOk], 0);
end;

// RNC 07-02-2004
// If the running program is not broken and the user tries to add a breakpoint, display
// an error message saying that a breakpoint cannot be added while the program is running
//RNC changed to accept an index into the array of breakpoints
function TDebugger.AddBreakpoint(i : integer):integer;
begin
  Result:=-1;
  if (IsBroken = true) then begin
    inc(breakPointCount);
    SendCommand(GDB_BREAK, '"' + PBreakPointEntry(BreakPointList.Items[i])^.file_name + ':' + inttostr(PBreakPointEntry(BreakPointList.Items[i])^.line) + '"');
    Result:=breakPointCount;
  end;
end;

// RNC 07-02-2004 A function to return true or false depending on whether or not a breakpoint already exists
// RNC changed to check if a breakpoint exists by checking a filename vs the global list, not an editor
// editors change when a file is close, but the filename will not.  This makes sure that just becuse
// you close a file, the breakpoint will not disappear
function TDebugger.BreakpointExists(filename: string; line: integer):boolean;
var
  I: integer;
begin
  Result := false;
  for I := 0 to BreakPointList.Count-1 do
    if (PBreakPointEntry(BreakPointList[I])^.file_name = filename) and
       (PBreakPointEntry(BreakPointList[I])^.line = line) then begin
       Result:=true;
       Break;
    end;
end;

// RNC changed to take an index in the 1 list of breakpoints, not an editor
procedure TDebugger.RemoveBreakpoint(i: integer);
begin
if (IsBroken = true) then begin
  if Executing then begin
    SendCommand(GDB_DELETE, inttostr(i));
  end;
end;
end;

// RNC Change to remove breakpoints using new list
procedure TDebugger.RemoveAllBreakpoints;
var
  I: integer;
begin
  for I := 0 to BreakPointList.Count - 1 do begin
    if Executing then
      SendCommand(GDB_DELETE, inttostr(PBreakPointEntry(BreakPointList.Items[I])^.breakPointIndex));
  end;
  MainForm.RemoveAllBreakPointFromList();
end;

function TDebugger.GetCallStack: TList;
begin
  if Assigned(Wait) then
    Result:=Wait.CallStackList
  else
    Result:=nil;
end;

procedure TDebugger.AddIncludeDir(s: string);
begin
  if DirectoryExists(s) then
    fIncDirs:=fIncDirs+' --directory='+s+' ';
end;

procedure TDebugger.ClearIncludeDirs;
begin
  fIncDirs:='';
end;

procedure TDebugger.OnAsmCode(s : string);
begin
  if Assigned(CPUForm) then begin
    CPUForm.CodeList.Lines.Add(s);
  end;
end;

procedure TDebugger.OnAsmFunc(s : string);
begin
  if Assigned(CPUForm) then begin
    CPUForm.CodeList.ClearAll;
    CPUForm.edFunc.Text := s;
  end;
  InAssembler := true;
end;

procedure TDebugger.OnAsmCodeEnd;
begin
  InAssembler := false;
end;

function TDebugger.GetWatchValue: string;
begin
  if Assigned(Wait) then
    Result:=Wait.tmpWatchValue
  else
    Result:='';
end;

function TDebugger.GetWatchVar: string;
begin
  if Assigned(Wait) then
    Result:=Wait.tmpWatchVar
  else
    Result:='';
end;

procedure TDebugger.RefreshContext;
var i, k : integer;
    s : string;
begin
  if not Executing then
    exit;
  if Assigned(MainForm.DebugTree) then
    for i := 0 to DebugTree.Items.Count - 1 do begin
       k := AnsiPos(' = Not found in current context', DebugTree.Items[i].Text);
       if k > 0 then begin
         s := DebugTree.Items[i].Text;
         Delete(s, k, length(s) - k + 1);
         SendCommand(GDB_DISPLAY, s);
       end;
    end;
end;

end.

