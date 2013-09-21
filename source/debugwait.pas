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

unit debugwait;

interface

uses
{$IFDEF WIN32}
  Sysutils, Classes, Windows, ShellAPI, debugreader,
  version, Dialogs, ComCtrls, StrUtils, Forms;
{$ENDIF}
{$IFDEF LINUX}
  Sysutils, Classes, debugreader,
  version, QDialogs, QComCtrls, StrUtils, QForms;
{$ENDIF}

const SPACES = [' ', #9, #13, #10];

type
  TAnnotateType = (TPrompt, TPostPrompt, TUnknown,
                   TBreakpoint, TSource, TDisplayExpression,
                   TDisplayValue, TGdbError, TGdbErrorBegin,
                   TFieldName, TFieldValue, TFieldEnd, TDisplayEnd, TDisplayBegin,
                   TFrameFunc, TFrameArgs,
                   TFrameArgBegin, TFrameArgEnd,
                   TFrameSourceFile, TFrameSourceLine, TSignalString,
                   TArraySectionBegin, TArraySectionEnd);

  PCallStack = ^TCallStack;
  TCallStack = packed record
    Filename: string;
    Line: integer;
    FuncName: string;
    Args: string;
  end;

  type TRegister  = (EAX, EBX, ECX, EDX, ESI, EDI,
                     EBP, ESP, EIP, CS, DS, SS, ES);
  type TRegisters = array [TRegister] of string;

  TDebugNode = class(TObject)
  public
    Name  : string;
    Value : string;
    Next  : TList;

    constructor Create;
    destructor  Destroy; override;
  protected

  end;

  TDebugWait = class(TThread)
  public
    Event  : THandle;
    Reader : TDebugReader;
    Stop   : boolean;
    DebugTree : TTreeView;
    Registers : ^TRegisters;
    CallStackList: TList;
    CurrentDisplay : integer;
    tmpWatchVar: string;
    tmpWatchValue: string;
    OnNoDebuggingSymbols : procedure of object;
    OnSourceMoreRecent : procedure of object;
    // RNC A procedure to see if debugging went into a DLL that we do not have debug info for.
    InaccessibleFunction : procedure of object;
    OnAsmCode : procedure (s : string) of object;
    OnAsmFunc : procedure (s : string) of object;
    OnAsmCodeEnd : procedure of object;
    OnSegmentationFault: procedure of object;

    // RNC 07.02.02004 -- A variable to indicate that the debugger is on a breakpoint
    // meaning that more breakpoints can be added
    broken : boolean;

  protected
    pos      : integer;
    len      : integer;
    bline    : integer;
    bfile    : string;
    debugstr : string;
    CurNode  : TDebugNode;
    ExitCount : integer;
    FirstBreak : boolean;

    procedure Execute; override;
    procedure Analyze;

    procedure Debug;
    procedure BreakPointNotify;
    procedure SignalNotify;

    procedure SkipSpaces;
    procedure NextAnnotation;
    function  GetNextAnnotation : TAnnotateType;
    function  GetNextString : string;
    function  GetNextLine : string;
    procedure CatchExpression;
    procedure GetRegisters(name, value : string);
    procedure GetAsmCode;
    function  GetNextField(Node : TDebugNode; ExtraClose : integer) : integer;
    procedure UpdateDebugTree;
    procedure CreateDebugNodes(ParentNode : TTreeNode; Node : TDebugNode);
    procedure UpdateDebugNodes(item : TTreeNode; Node : TDebugNode);
    function  CreateClassNodes(node : TDebugNode; s : string; i : integer) : integer;

  end;

implementation

uses 
  main, devcfg, utils;

constructor TDebugNode.Create;
begin
  inherited;
  Next := TList.Create;
end;

destructor  TDebugNode.Destroy;
begin
  Next.Free;
  inherited;
end;

procedure TDebugWait.Debug;
begin
  // removes ctrl+Z and empty lines
  while (system.pos(#26, debugstr) <> 0) do
    Delete(debugstr, system.pos(#26, debugstr), 1);
  while (system.pos(#13#10#13#10, debugstr) <> 0) do
    Delete(debugstr, system.pos(#13#10#13#10, debugstr), 2);
  while (system.pos(#10#10, debugstr) <> 0) do
    Delete(debugstr, system.pos(#10#10, debugstr), 2);
  MainForm.DebugOutput.Lines.Add(debugstr);
end;

// RNC 07-02-2004
// Since you cannot add breakpoints while GDB is running, I created a variable
// broken to indicate that the debugger has stopped on a breakpoint.  (on the command line
// this would be when you can actually type at a prompt).  If this variable is true
// add breakpoints will succeed.  Otherewise a message box will appear saying that the
// debugger must be stopped to add breakpoints.  This is useful on event-driven apps
// where the user may think they can add breakpoints while the program is running
procedure TDebugWait.BreakPointNotify;
begin
  broken := true;
  if (FirstBreak) then begin
    FirstBreak := false;
    MainForm.RefreshContext;
  end;
  MainForm.GotoBreakpoint(bfile, bline);
end;

procedure TDebugWait.SignalNotify;
begin
  if (debugstr = T_SEGFAULT) and Assigned(OnSegmentationFault) then
    OnSegmentationFault
  // RNC 07-14-2004 I found out that if you build a GUI program without the -mwindows
  // flag in the linker, the program will open with a console window.  If you press
  // Ctrl-c in this console, you can pause or halt the debugger.  This is the same
  // as pressing ctrl-c in gdb on the command line.  If this occurs, then set broken to true
  // so breakpoints can be added in the halted state.
  else if debugstr = 'Interrupt' then
    broken:=true;
end;


procedure TDebugWait.SkipSpaces;
begin
  while (pos < len) and (Reader.Output[pos] in SPACES) do
    pos := pos + 1;
end;

procedure TDebugWait.NextAnnotation;
begin
  while (pos < len) and (Reader.Output[pos] <> #26) do
    pos := pos + 1;
end;

function TDebugWait.GetNextString : string;
begin
  Result:='';
  while (pos < len) and (not (Reader.Output[pos] in SPACES)) do begin
    Result := Result + Reader.Output[pos];
    pos := pos + 1;
  end;
end;



function TDebugWait.GetNextLine : string;
begin
  Result := '';
  while (pos < len) and (not (Reader.Output[pos] in [#13, #10])) do begin
    Result := Result + Reader.Output[pos];
    pos := pos + 1;
  end;
end;

function TDebugWait.GetNextAnnotation : TAnnotateType;
var s : string;
begin
  while (pos < len) and (Reader.Output[pos] = #26) do
    pos := pos + 1;
  s := GetNextString;
  if s = T_PROMPT then
    result := TPrompt
  else if s = T_BREAKPOINT then
    result := TBreakpoint
  else if s = T_SOURCE then
    result := TSource
  else if s = T_DISPLAY_EXPRESSION then
    result := TDisplayExpression
  else if s = T_DISPLAY_BEGIN then
    result := TDisplayExpression
  else if s = T_FIELD_NAME then
    result := TFieldName
  else if s = T_FIELD_VALUE then
    result := TFieldValue
  else if s = T_FIELD_END then
    result := TFieldEnd
  else if s = T_DISPLAY_END then
    result := TDisplayEnd
  else if s = T_DISPLAY_VALUE then
    result := TDisplayValue
  else if s = T_ARRAYSECTION_BEGIN then
    result := TArraySectionBegin
  else if s = T_ARRAYSECTION_END then
    result := TArraySectionEnd
  else if s = T_GDB_ERROR then
    result := TGdbError
  else if s = T_GDB_ERRORBEGIN then
    result := TGdbErrorBegin
  else if s = T_FRAME_FUNCNAME then
    result := TFrameFunc
  else if (s = T_FRAME_ARGS) or
          (s = T_FRAME_ARG_BEGIN) or
          (s = T_FRAME_ARG_NAME_END) or
          (s = T_FRAME_ARG_VALUE) or
          (s = T_FRAME_ARG_END) then begin
    result := TFrameArgs;
    if s = T_FRAME_ARG_VALUE then begin
                        SkipSpaces;
                        debugstr := GetNextString;
                        end;
    end
  else if s = T_FRAME_SOURCEFILE then
    result := TFrameSourceFile
  else if s = T_FRAME_SOURCELINE then
    result := TFrameSourceLine
  else if s = T_POST_PROMPT then
    result := TPostPrompt
  else if s = T_SIGNAL_STRING then
    result := TSignalString
  else
    result := TUnknown;
end;

procedure TDebugWait.GetRegisters(name, value : string);
begin
  if name = GDB_EAX then
    Registers[EAX] := value
  else if name = GDB_EBX then
    Registers[EBX] := value
  else if name = GDB_ECX then
    Registers[ECX] := value
  else if name = GDB_EDX then
    Registers[EDX] := value
  else if name = GDB_ESI then
    Registers[ESI] := value
  else if name = GDB_EDI then
    Registers[EDI] := value
  else if name = GDB_EBP then
    Registers[EBP] := value
  else if name = GDB_ESP then
    Registers[ESP] := value
  else if name = GDB_EIP then
    Registers[EIP] := value
  else if name = GDB_CS then
    Registers[CS] := value
  else if name = GDB_DS then
    Registers[DS] := value
  else if name = GDB_SS then
    Registers[SS] := value
  else if name = GDB_ES then
    Registers[ES] := value
end;

function CountChar(s : string; c : char) : integer;
var i : integer;
begin
  result := 0;
  for i := 1 to length(s) do
    if s[i] = c then
      result := result + 1;
end;

function TDebugWait.CreateClassNodes(node : TDebugNode; s : string; i : integer) : integer;
var parent, newnode : TDebugNode;
    j{, count}     : integer;
    first : boolean;
begin
  first := false;
  if (i = -1) then begin
    first := true;
    i := 1;
  end;
  parent := node;
{  count := 0;}
  result := 0;
  while (s[i] <> '') do begin
    if (s[i] = '{') then begin
      j := i + 1;
      while ((s[j] <> '') and (s[j] <> '}')) do begin
        j := j + 1;
      end;
      if s[j] = '}' then begin
        i := i + 1;
        continue;
      end;
{      count := count + 1;}
      newnode := TDebugNode.Create;
      i := i + 1;
      if (s[i] = '') then
        break;
      j := 1;
      newnode.Name := s;
      while ((s[i] <> '') and (s[i] <> '=') and (s[i] <> '{')) do begin
        newnode.Name[j] := s[i];
        i := i + 1;
        j := j + 1;
      end;
      newnode.Name[j] := #0;
      parent.Next.Add(newnode);
      while ((s[i] <> '') and (s[i] <> '{')) do
        i := i + 1;
      if (s[i] <> '') then
        result := CreateClassNodes(newnode, s, i);
      if ExitCount > 0 then begin
        ExitCount := ExitCount - 1;
        if result > 0 then
          result := result - 1
        else
          result := 0;
        exit;
      end;
      if first then
        result := GetNextField(newnode, result + 1)
      else
        result := GetNextField(newnode, result);
      if ExitCount > 0 then begin
        ExitCount := ExitCount - 1;
        if result > 0 then
          result := result - 1
        else
          result := 0;
        exit;
      end;
      break;
    end;
    i := i + 1;
  end;
end;

function TDebugWait.GetNextField(Node : TDebugNode; ExtraClose : integer) : integer;
var a : TAnnotateType;
    n : TDebugNode;
    s : string;
    Count, tmp : integer;
begin
  result := 0;
  repeat
    repeat
      NextAnnotation;
      a := GetNextAnnotation;
      if (pos >= len) then
        exit;
      if a = TArraySectionBegin then begin // get array
        //Node.Value := ' = ';
        GetNextLine;
        repeat
          SkipSpaces;
          s := GetNextLine;
          if (s <> '') and (s[1] = #26) and (System.pos(T_ARRAYSECTION_END, s) > 0) then  begin
            SkipSpaces;
            GetNextLine;
            exit;
          end
          else if (s[1] <> #26) then
            Node.Value := Node.Value + s;
        until (s = '');
        SkipSpaces;
        GetNextLine;
        exit;
      end;
    until (a = TPrompt) or (a = TFieldName);
    // eat character following field name
    SkipSpaces;
    s := GetNextLine;
    if (s <> '') and ((s[1] = '-') or (s[1] = '*')) then begin
      // read name
      SkipSpaces;
      s := GetNextLine;
    end;
    // special case : no name
    if (s <> '') and (s[1] = #26) then
      s := '';
    n := TDebugNode.Create;
    n.Name := s;
    repeat
      NextAnnotation;
      a := GetNextAnnotation;
      if (pos >= len) then
        exit;
    until (a = TPrompt) or (a = TFieldValue);
    SkipSpaces;
    s := GetNextLine;
    // check if class or structure
    Count := CountChar(s, '{') - CountChar(s, '}');
    // has inherited class description
    if (Count > 1) then
      ExtraClose := ExtraClose - CreateClassNodes(n, s, 1)
    // simple class or struct
    else if Count = 1 then
      ExtraClose := ExtraClose - GetNextField(n, Count - 1)
    else
      n.Value := s;
    if (n.Value <> '') then
      n.Value := ' = ' + n.Value;
    Node.Next.Add(n);
    if ExitCount > 0 then begin
      result := ExtraClose - 1;
      exit;
    end;
    repeat
      tmp := pos;
      NextAnnotation;
      a := GetNextAnnotation;
      if (pos >= len) then
        exit;
      if (a = TFieldName) then begin
        pos := tmp;
        GetNextField(n, Count - 1);
      end;
    until (a = TPrompt) or (a = TFieldEnd);
    SkipSpaces;
    s := GetNextLine;
    Count := CountChar(s, '}') - CountChar(s, '{');
    // go back from herited classes
    while (System.pos('},', s) > 0) do begin
      Delete(s, System.pos('},', s), 2);
      ExitCount := ExitCount + 1;
    end;
    if ExitCount > 0 then begin
      result := ExtraClose - (Count - ExitCount) - 1;
      exit;
    end;
  until (s = '') or (Count > ExtraClose);
  result := Count - ExtraClose - 1;
end;

procedure TDebugWait.CatchExpression;
var a : TAnnotateType;
    name, value, tmp : string;
    Node : TDebugNode;
    CountOpen, res : integer;
begin
  ExitCount := 0;
  // Get display number
  SkipSpaces;
  name := GetNextLine;
  try
    if IsNumeric(name) then
      CurrentDisplay := StrToInt(name);
  except
    CurrentDisplay := 1000; //dummy
  end;
  repeat
    NextAnnotation;
    a := GetNextAnnotation;
    if (pos >= len) then
      exit; // big error /!\
   until (a = TPrompt) or (a = TDisplayExpression);

  // Get variable name
  SkipSpaces;
  name := GetNextLine;
  repeat
    NextAnnotation;
    a := GetNextAnnotation;
   until (a = TPrompt) or (a = TDisplayExpression);

  Node := TDebugNode.Create;
  Node.Name := name;

  if AnsiStartsStr(#26#26 + 'error', name) then begin // check if an error occured
      if not devData.WatchError then begin
        Node.Free;
        Node := nil;
      end
      else begin
        Node.Name := 'Could not watch this variable';
      end;
  end
  else if AnsiStartsStr(#26#26, name) then begin
     Node.Free;
     Node := nil;
  end
  else if a = TDisplayExpression then begin
    SkipSpaces;
    value := GetNextLine;
    if (value = '(') then begin
      value := GetNextLine;
    end;
    CountOpen := CountChar(value, '{') - CountChar(value, '}');
    if (CountOpen > 1) then begin
      res := CreateClassNodes(Node, value, -1);
      GetNextField(Node, res);
    end
    else if CountOpen = 1 then begin // block of values (i.e. struct members following)
      GetNextField(Node, CountOpen - 1);
    end
    else if AnsiStartsStr(#26#26 + 'error', value) then begin // check if an error occured
      if not devData.WatchError then begin
        Node.Free;
        Node := nil;
      end
      else
        Node.Name := Node.Name + ' = Could not watch this variable';
    end
    else begin
      if AnsiStartsStr(#26#26, value) then
         value := '';
      repeat
        SkipSpaces;
        tmp := GetNextLine;
        if AnsiStartsStr(#26#26 + T_DISPLAY_END, tmp) then
          break;
        while AnsiStartsStr(#26#26, tmp) do begin
          SkipSpaces;
          tmp := GetNextLine();
        end;

        value := value + tmp;
        SkipSpaces;
        a := GetNextAnnotation;
      until (a = TPrompt) or (a = TDisplayEnd) or (tmp = '');
      if AnsiSameStr(Node.Name, value) then begin
        if not devData.WatchError then begin
          Node.Free;
          Node := nil;
        end
        else
          Node.Name := Node.Name + ' = Could not watch this variable';
      end
      else
        Node.Value := ' = ' + value;
    end;
  end;
  GetRegisters(name, value);

  CurNode := Node;
  Synchronize(UpdateDebugTree);

  tmpWatchVar := name;
  tmpWatchValue := Value;
end;

procedure TDebugWait.Analyze;
var an  : TAnnotateType;
    tmp : integer;

  procedure get_source_info(s : string);
  const DELIM = ':';
  var CatchDriveDelim : boolean;
      i, j : integer;
      cline : array [0..256] of char;
  begin
    CatchDriveDelim := false;
    i := 1;
    j := 0;
    bfile := s;
    while (i < length(s)) do begin
      if s[i] = DELIM then begin
        if CatchDriveDelim then begin
          Delete(bfile, i, length(bfile));
          i := i + 1;
          while (s[i] <> DELIM) and (i < length(s)) do begin
            cline[j] := s[i];
            j := j + 1;
            i := i + 1;
          end;
          cline[j] := #0;
          break;
        end
        else
          CatchDriveDelim := True
      end;
      i := i + 1;
    end;
    if IsNumeric(cline) then
      bline := StrToInt(cline)
    else
      bline := 0; //dummy
  end;

  var
    pcs: PCallStack;
    Node: TDebugNode;
begin
  pos := 1;
  len := Length(Reader.Output);
  while pos < len do begin
    SkipSpaces;
    if Reader.Output[pos] = #26 then begin // Ctrl + Z char
      an := GetNextAnnotation;
      case an of
        TPrompt     : begin
                        {debugstr := 'Prompt annotation';
                        Synchronize(Debug);}
                      end;
        {TBreakpoint : begin
                        debugstr := 'Breakpoint annotation';
                        SkipSpaces;
                        breakpointId := StrToInt(GetNextString);
                        //Synchronize(Debug);
                        Synchronize(BreakpointNotify);
                      end;}
        TSource     : begin
                        SkipSpaces;
                        debugstr := GetNextString;

                        // handle spaces in filenames - mandrav 8 Jul 2002
                        while (AnsiPos(':beg:', debugstr)=0) and (AnsiPos(':middle:', debugstr)=0) do begin
                          SkipSpaces;
                          debugstr := debugstr+' '+GetNextString;
                        end;

                        get_source_info(debugstr);
                        Synchronize(BreakpointNotify);
                      end;

        TFrameFunc  : begin
                        SkipSpaces;
                        debugstr := GetNextString;
                        pcs:=New(PCallStack);
                        pcs^.FuncName:=debugstr;
                        CallStackList.Add(pcs);
                      end;
        TFrameArgs : if CallStackList.Count>0 then begin
                        SkipSpaces;
                        debugstr := GetNextString;
                        pcs:=PCallStack(CallStackList[CallStackList.Count-1]);
                        if Assigned(pcs) then
                          pcs^.Args:=pcs^.Args+debugstr;
                      end;
        TFrameSourceFile  : if CallStackList.Count>0 then begin
                        SkipSpaces;
                        debugstr := GetNextString;
                        pcs:=PCallStack(CallStackList[CallStackList.Count-1]);
                        if Assigned(pcs) then
                          pcs^.Filename:=debugstr;
                      end;
        TFrameSourceLine  : if CallStackList.Count>0 then begin
                        SkipSpaces;
                        debugstr := GetNextString;
                        pcs:=PCallStack(CallStackList[CallStackList.Count-1]);
                        if Assigned(pcs) then begin
                          if (IsNumeric(debugstr)) then
                            pcs^.Line:=StrToIntDef(debugstr, -1)
                          else
                            pcs^.Line:= 0; //dummy
                        end;
                      end;

        TGdbError   : begin
                        SkipSpaces;
                        debugstr := GetNextString;
                        Synchronize(Debug);
                      end;

        TGdbErrorBegin : begin
                          SkipSpaces;
                          debugstr := GetNextLine;
                          Synchronize(Debug);
                          if (AnsiPos('in current context', debugstr) > 0) then begin
                            Delete(debugstr, 1, AnsiPos('"', debugstr));
                            tmp := AnsiPos('"', debugstr);
                            Delete(debugstr, tmp, length(debugstr) - tmp + 1);
                            Node := TDebugNode.Create;
                            Node.Name := debugstr;
                            Node.Value := ' = Not found in current context';
                            CurNode := Node;
                            Synchronize(UpdateDebugTree);
                          end;

                         end;

        TDisplayExpression : CatchExpression;

        TPostPrompt : begin // handle asm code
                        SkipSpaces;
                        tmp := pos;
                        debugstr := GetNextString;
                        if debugstr = T_DUMP then begin
                          SkipSpaces;
                          debugstr := GetNextString;
                          if (debugstr = T_OF) then begin
                            SkipSpaces;
                            debugstr := GetNextString;
                            if (debugstr = T_ASM) then
                              GetAsmCode
                            else
                              pos := tmp;
                          end
                          else
                            pos := tmp;
                        end
                        else pos := tmp;
                      end;
        TSignalString : begin
                          SkipSpaces;
                          debugstr := GetNextString;
                          Synchronize(SignalNotify);
                        end;
      end;
    end;
    pos := pos + 1;
  end;
end;

procedure TDebugWait.Execute;
var
  quit : boolean;
  I: integer;
begin
  CallStackList:=TList.Create;
  quit := false;
  stop := false;
  FirstBreak := true;
  while not quit do begin
    WaitForSingleObject(Event, INFINITE);
    if stop then
      exit;
    if Reader.Output <> GDB_PROMPT then begin
      if System.pos('No such file', Reader.Output) <> 0 then begin
        debugstr := 'Executable file could not be loaded in the debugger';
        Synchronize(debug);
      end;
      if (System.Pos('no debugging symbols found', Reader.Output) <> 0) or
         (System.Pos('No symbol table is loaded', Reader.Output) <> 0) then begin
        debugstr := 'You need to put debugging mode on before debugging';
        Synchronize(debug);
        Synchronize(OnNoDebuggingSymbols);
        exit;
      end
      else if System.pos('Program exited with code', Reader.Output) <> 0 then begin
        debugstr := 'Program exited with error...';
        Synchronize(debug);
        quit := true;
      end
      else if System.pos('Program exited normally', Reader.Output) <> 0 then begin
        debugstr := 'Program exited normally';
        Synchronize(debug);
        quit := true;
      end
      else if System.pos('The program is not being run', Reader.Output) <> 0 then begin
        debugstr := 'Program exited normally';
        Synchronize(debug);
        quit := true;
      end
      else if System.pos('file is more recent than executable', Reader.Output) <> 0 then begin
        debugstr := 'Source file is more recent than executable';
        Synchronize(debug);
        Synchronize(OnSourceMoreRecent);
      end
      // RNC if we have a frame-function-name but no source file to open, then we have probably jumped into a DLL or something else
      // Therefore call the InaccessibleFunction (which just continues)
      else if (System.pos('frame-function-name', Reader.Output) <> 0) and (System.pos('frame-source-file', Reader.Output) = 0) then begin
        debugstr := 'Execution moved to an unaccessible function';
        Synchronize(InaccessibleFunction);
      end;
      Analyze;  // Analyze output and act accordingly
      debugstr := Reader.Output;
      Synchronize(Debug);
    end;
    if Reader.Suspended then
      quit := true;
  end;
  for I:=0 to CallStackList.Count-1 do
    if Assigned(CallStackList[I]) then
      Dispose(PCallStack(CallStackList[I]));
  CallStackList.Free;
end;

procedure TDebugWait.GetAsmCode;
var s : string;
begin
  SkipSpaces;
  s := GetNextLine;
  delete(s, 1, length('code for function ')); // remove unecessary text
  delete(s, length(s), 1);
  if Assigned(OnAsmFunc) then
    OnAsmFunc(s);
  SkipSpaces;
  s := GetNextLine;
  while System.pos('End of assembler dump.', s) = 0 do begin
    if Assigned(OnAsmCode) then
      OnAsmCode(s);
    SkipSpaces;
    s := GetNextLine;
  end;
  if Assigned(OnAsmCodeEnd) then
    Synchronize(OnAsmCodeEnd);
end;

procedure TDebugWait.CreateDebugNodes(ParentNode : TTreeNode; Node : TDebugNode);
var i : integer;
    item : TTreeNode;
begin
  if ParentNode = nil then begin
    item := DebugTree.Items.Add(nil, Node.Name + Node.Value);
    item.ImageIndex := 21;
    item.SelectedIndex := 21;
    item.Data := ptr(CurrentDisplay);
  end
  else begin
    item := DebugTree.Items.AddChild(ParentNode, Node.Name + Node.Value);
    if (Node.Next.Count = 0) then begin
      item.ImageIndex := 32;
      item.SelectedIndex := 32;
    end
    else begin
      item.ImageIndex := 39;
      item.SelectedIndex := 39;
    end;
  end;
  for i := 0 to Node.Next.Count - 1 do
    CreateDebugNodes(item, TDebugNode(Node.Next.Items[i]));
  if ParentNode = nil then
    item.Expand(true);
  Node.Free;
end;

procedure TDebugWait.UpdateDebugNodes(item : TTreeNode; Node : TDebugNode);
var i : integer;
begin
  if system.pos('=', item.Text) > 0 then
    item.Text := Node.Name + Node.Value;
  i := 0;
  while (i < item.Count) and (i < Node.Next.Count) do begin
    UpdateDebugNodes(item[i], Node.Next[i]);
    i := i + 1;
  end;
  Node.Free
end;

procedure TDebugWait.UpdateDebugTree;
var i : integer;
    item : TTreeNode;
begin
  if Assigned(DebugTree) and Assigned(CurNode) then begin
    item := nil;
    for i := 0 to DebugTree.Items.Count - 1 do
      if (CurNode.Name = DebugTree.Items[i].Text) or
         AnsiStartsStr(CurNode.Name + ' ', DebugTree.Items[i].Text) or
         AnsiStartsStr('this->' + CurNode.Name + ' ', DebugTree.Items[i].Text) or
         AnsiStartsStr('this.' + CurNode.Name + ' ', DebugTree.Items[i].Text) then begin
        item := DebugTree.Items[i];
        break;
      end;
    if (Assigned(item)) then begin
      if item.Count <> CurNode.Next.Count then begin
        DebugTree.Items.Delete(item);
        Application.ProcessMessages;
        CreateDebugNodes(nil, CurNode);
      end
      else
        UpdateDebugNodes(item, CurNode);
    end
    else begin
      CreateDebugNodes(nil, CurNode);
    end;
  end;
end;

end.
