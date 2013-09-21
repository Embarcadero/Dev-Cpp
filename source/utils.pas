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

unit utils;

interface

uses
{$IFDEF WIN32}
 Windows, Classes, Sysutils, Forms, ShellAPI, Dialogs, SynEditHighlighter,
 Menus, Registry;
{$ENDIF}
{$IFDEF LINUX}
 Classes, Sysutils, QForms, QDialogs, QSynEditHighlighter,
 QMenus, Types;
{$ENDIF}

type
 PdevSearchResult = ^TdevSearchResult;
 TdevSearchResult = record
  pt: TPoint;
  InFile: string;
  msg: string;
 end;


 { File ID types }
 TUnitType = (utSrc, utHead, utRes, utPrj, utOther);
 TExUnitType = (utcSrc,      // c source file (.c)
                utcppSrc,    // c++ source file (.cpp)
                utcHead,     // c header (.h)
                utcppHead,   // c++ header (.hpp)
                utresHead,   // resouce header (.rh)
                utresComp,   // resource compiled (.res)
                utresSrc,    // resource source (.rc)
                utxPrj,
                utxOther);   // any others

 TFilterSet = (ftOpen, ftHelp, ftPrj, ftSrc, ftAll);

 TErrFunc = procedure(Msg: String) of Object;
 TLineOutputFunc = procedure(Line: String) of Object;
 TCheckAbortFunc = procedure(var AbortThread: boolean) of object;

function IsWinNT : boolean;

procedure FilesFromWildcard(Directory, Mask: string;
  var Files : TStringList; Subdirs, ShowDirs, Multitasking: Boolean);
function ExecuteFile(const FileName, Params, DefaultDir: string;
  ShowCmd: Integer): THandle;
function RunAndGetOutput(Cmd, WorkDir: string;
  ErrFunc: TErrFunc; LineOutputFunc: TLineOutputFunc;
  CheckAbortFunc: TCheckAbortFunc;
  ShowReturnValue: Boolean = True): string;
function GetShortName(FileName: string): string;
procedure ShowError(Msg: String);

function CommaStrToStr(s : string; formatstr : string) : string;
function IncludeQuoteIfSpaces(s : string) : string;
function IncludeQuoteIfNeeded(s : string) : string;


// Added by MikeB
procedure LoadFilefromResource(const FileName: string; ms: TMemoryStream);

function ValidateFile(const FileName: string; const WorkPath: string;
  const CheckDirs: boolean = FALSE): string;

function BuildFilter(var value: string; const FLTStyle: TFILTERSET): boolean; overload;
function BuildFilter(var value: string; const _filters: array of string): boolean; overload;

function CodeInstoStr(s: string): string;
function StrtoCodeIns(s: string): string;

procedure StrtoAttr(var Attr: TSynHighlighterAttributes; Value: string);
function AttrtoStr(const Attr: TSynHighlighterAttributes): string;

procedure StrtoPoint(var pt: TPoint; value: string);
function PointtoStr(const pt: TPoint): string;

function ListtoStr(const List: TStrings): string;
procedure StrtoList(s: string; const List: TStrings; const delimiter: char=';');

function GetFileTyp(const FileName: string): TUnitType;
function GetExTyp(const FileName: string): TExUnitType;

procedure SetPath(Add: string; const UseOriginal: boolean = TRUE);
function ExpandFileto(const FileName: string; const BasePath: string): string;
function FileSamePath(const FileName: string; const TestPath: string): boolean;
procedure CloneMenu(const FromMenu: TMenuItem; ToMenu: TMenuItem);

function GetLastPos(const SubStr: string; const S: string): integer;

function GenMakePath(FileName: String): String; overload;
function GenMakePath2(FileName: String): String;
function GenMakePath(FileName: String; EscapeSpaces,
                     EncloseInQuotes: Boolean): String; overload;

function GetRealPath(BrokenFileName: String; Directory: String = ''): String;

function CalcMod(Count: Integer): Integer;

function GetVersionString(FileName: string): string;

function CheckChangeDir(var Dir: string): boolean;

function GetAssociatedProgram(const Extension: string; var Filename, Description: string): boolean;

function IsNumeric(s : string) : boolean;

implementation

uses 
{$IFDEF WIN32}
  devcfg, version, Graphics, StrUtils, MultiLangSupport, main, editor;
{$ENDIF}
{$IFDEF LINUX}
  devcfg, version, QGraphics, StrUtils, MultiLangSupport, main, editor;
{$ENDIF}

procedure FilesFromWildcard(Directory, Mask: String;
  var Files : TStringList; Subdirs, ShowDirs, Multitasking: Boolean);
var
  SearchRec: TSearchRec;
  Attr, Error: Integer;
begin
  Directory := IncludeTrailingPathDelimiter(Directory);

  { First, find the required file... }
  Attr := faAnyFile;
  if ShowDirs = False then
     Attr := Attr - faDirectory;
  Error := FindFirst(Directory + Mask, Attr, SearchRec);
  if (Error = 0) then
  begin
     while (Error = 0) do
     begin
     { Found one! }
        Files.Add(Directory + SearchRec.Name);
        Error := FindNext(SearchRec);
        if Multitasking then
           Application.ProcessMessages;
     end;
     FindClose(SearchRec);
  end;

  { Then walk through all subdirectories. }
  if Subdirs then
  begin
     Error := FindFirst(Directory + '*.*', faAnyFile, SearchRec);
     if (Error = 0) then
     begin
        while (Error = 0) do
        begin
           { Found one! }
           if (SearchRec.Name[1] <> '.') and (SearchRec.Attr and
             faDirectory <> 0) then
              { We do this recursively! }
              FilesFromWildcard(Directory + SearchRec.Name, Mask, Files,
                Subdirs, ShowDirs, Multitasking);
           Error := FindNext(SearchRec);
        end;
     FindClose(SearchRec);
     end;
  end;
end;

function ExecuteFile(const FileName, Params, DefaultDir: string;
  ShowCmd: Integer): THandle;
begin
  Result := ShellExecute(Application.MainForm.Handle, nil,
    PChar(FileName), PChar(Params),
    PChar(DefaultDir), ShowCmd);
end;

function RunAndGetOutput(Cmd, WorkDir: string;
  ErrFunc: TErrFunc; LineOutputFunc: TLineOutputFunc;
  CheckAbortFunc: TCheckAbortFunc;
  ShowReturnValue: Boolean): string;
var
  tsi: TStartupInfo;
  tpi: TProcessInformation;
  nRead: DWORD;
  aBuf: array[0..101] of char;
  sa: TSecurityAttributes;
  hOutputReadTmp, hOutputRead, hOutputWrite, hInputWriteTmp, hInputRead,
  hInputWrite, hErrorWrite: THandle;
  FOutput: string;
  CurrentLine: String;
  bAbort: boolean;
begin
  FOutput := '';
  CurrentLine := '';
  sa.nLength := SizeOf(TSecurityAttributes);
  sa.lpSecurityDescriptor := nil;
  sa.bInheritHandle := True;

  CreatePipe(hOutputReadTmp, hOutputWrite, @sa, 0);
  DuplicateHandle(GetCurrentProcess(), hOutputWrite, GetCurrentProcess(),
                  @hErrorWrite, 0, true, DUPLICATE_SAME_ACCESS);
  CreatePipe(hInputRead, hInputWriteTmp, @sa, 0);

  // Create new output read handle and the input write handle. Set
  // the inheritance properties to FALSE. Otherwise, the child inherits
  // the these handles; resulting in non-closeable handles to the pipes
  // being created.
  DuplicateHandle(GetCurrentProcess(), hOutputReadTmp,  GetCurrentProcess(),
                  @hOutputRead,  0, false, DUPLICATE_SAME_ACCESS);
  DuplicateHandle(GetCurrentProcess(), hInputWriteTmp, GetCurrentProcess(),
                  @hInputWrite, 0, false, DUPLICATE_SAME_ACCESS);
  CloseHandle(hOutputReadTmp);
  CloseHandle(hInputWriteTmp);

  FillChar(tsi, SizeOf(TStartupInfo), 0);
  tsi.cb := SizeOf(TStartupInfo);
  tsi.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
  tsi.hStdInput := hInputRead;
  tsi.hStdOutput := hOutputWrite;
  tsi.hStdError := hErrorWrite;

  if not CreateProcess(nil, PChar(Cmd), @sa, @sa, true, 0, nil, PChar(WorkDir),
                        tsi, tpi) then begin
    result := 'Unable to run "' + Cmd + '": ' + SysErrorMessage(GetLastError);
    exit;
  end;
  CloseHandle(hOutputWrite);
  CloseHandle(hInputRead );
  CloseHandle(hErrorWrite);

  bAbort:=False;
  repeat
     if Assigned(CheckAbortFunc) then
        CheckAbortFunc(bAbort);
     if bAbort then begin
       TerminateProcess(tpi.hProcess, 1);
       Break;
     end;
     if (not ReadFile(hOutputRead, aBuf, 16, nRead, nil)) or (nRead = 0) then
     begin
        if GetLastError = ERROR_BROKEN_PIPE then
          Break
        else
          //MessageDlg('Pipe read error, could not execute file', mtError, [mbOK], 0);
          ErrFunc('Pipe read error, could not execute file');
     end;
     aBuf[nRead] := #0;
     FOutput := FOutput + PChar(@aBuf[0]);

     if Assigned(LineOutputFunc) then
     begin
         CurrentLine := CurrentLine + PChar(@aBuf[0]);
         if CurrentLine[Length(CurrentLine)] = #10 then
         begin
             Delete(CurrentLine, Length(CurrentLine), 1);
             LineOutputFunc(CurrentLine);
             CurrentLine := '';
         end;
     end;
  until False;
  GetExitCodeProcess(tpi.hProcess, nRead);
  if ShowReturnValue then
     Result := FOutput + ' ' + inttostr(nread)
  else
     Result := FOutput;

  CloseHandle(hOutputRead);
  CloseHandle(hInputWrite);
  CloseHandle(tpi.hProcess);
  CloseHandle(tpi.hThread);

end;

procedure SetPath(Add: string; const UseOriginal: boolean = TRUE);
var
 OldPath: array[0..PATH_LEN] of char;
 NewPath: string;
begin
  if add <> '' then
   if Add[length(Add)] <> ';' then
    Add:= Add +';';

  // PATH environment variable does *not* like quotes in paths...
  // Even if there are spaces in pathnames, it doesn't matter.
  // It splits them up by the ';'
  Add:=StringReplace(Add, '"', '', [rfReplaceAll]);

  if UseOriginal then
   NewPath:= Add+ devDirs.OriginalPath
  else
   begin
     GetEnvironmentVariable(pchar('PATH'), @OldPath, PATH_LEN);
     NewPath:= Add +string(OldPath);
   end;

  SetEnvironmentVariable(pchar('PATH'), pchar(NewPath));
end;

function ValidateFile(const FileName: string; const WorkPath: string;
  const CheckDirs: boolean = FALSE): string;
var
 fName: string;
 tmp: TStrings;
 idx: integer;
begin
  fName:= ExtractFileName(FileName);
  if FileExists(FileName) then
   result:= FileName
  else
   if FileExists(WorkPath +fName) then
    result:= WorkPath +fName
  else
   if FileExists(WorkPath +FileName) then
    result:= FileName
   else
    if CheckDirs then
     begin
       if (devDirs.Default <> '') and (FileExists(devDirs.Default +fName)) then
        result:= devDirs.Default +fName
       else
       if (devDirs.Exec <> '') and (FileExists(devDirs.Exec +fName)) then
        result:= devDirs.Exec +fName
       else
       if (devDirs.Help <> '') and (FileExists(devDirs.Help +fName)) then
        result:= devDirs.Help +fName
       else
       if (devDirs.Lang <> '') and (FileExists(devDirs.Lang +fName)) then
        result:= devDirs.Lang +fName
       else
       if (devDirs.Icons <> '') then
        begin
          tmp:= TStringList.Create;
          try
           StrtoList(devDirs.Icons, tmp);
           if tmp.Count> 0 then
            for idx:= 0 to pred(tmp.Count) do
             if FileExists(IncludeTrailingPathDelimiter(tmp[idx]) +fName) then
              begin
                result:= IncludeTrailingPathDelimiter(tmp[idx]) +fName;
                break;
              end;
          finally
           tmp.Free;
          end;
        end;
     end
    else
     result:= '';
end;

procedure LoadFilefromResource(const FileName: string; ms: TMemoryStream);
var
 HResInfo: HRSRC;
 hRes: THandle;
 Buffer: PChar;
 aName, Ext: string;
begin
  Ext:= ExtractFileExt(FileName);
  Ext:= copy(ext, 2, length(ext));
  aName:= ChangeFileExt(ExtractFileName(FileName), '');
  HResInfo:= FindResource(HInstance, pchar(aName), pchar(Ext));
  hres:= LoadResource(HInstance, HResInfo);
  if HRes = 0 then
   begin
     MessageBox(Application.MainForm.Handle,
       PChar(Format(Lang[ID_ERR_RESOURCE], [FileName, aName, Ext])),
       PChar(Lang[ID_ERROR]), MB_OK or MB_ICONERROR);
     exit;
   end;

  Buffer:= LockResource(HRes);
  ms.clear;
  ms.WriteBuffer(Buffer[0], SizeofResource(HInstance, HResInfo));
  ms.Seek(0, 0);
  UnlockResource(HRes);
  FreeResource(HRes);
end;

function GetShortName(FileName: string): string;
var
 pFileName: array[0..2048] of char;
begin
  GetShortPathName(pchar(FileName), pFileName, 2048);
  result:= strpas(pFileName);
end;

procedure ShowError(Msg: String);
begin
  Application.MessageBox(PChar(Msg), 'Error', MB_ICONHAND);
end;

function AddFilter(var value: string; const _Filter: string): boolean;
var
 idx: integer;
 s,
 LFilter: string;
begin
  result:= TRUE;
  try
   if _Filter = '' then exit;

   LFilter:= value;
   idx:= pos('|', LFilter);
   if idx> 0 then
    begin
      Insert(_Filter +'|', LFilter, AnsiPos(FLT_ALLFILES, LFIlter));
      s:= Copy(_Filter, AnsiPos('|', _Filter) +1, length(_Filter)) +';';
      Insert(s, LFilter, AnsiPos('|', LFilter) +1);
      if LFilter[Length(LFilter)] <> '|' then
       LFilter:= LFilter +'|';
    end;
   value:= LFilter;
  except
   result:= FALSE;
  end;
end;

function BuildFilter(var value: string; const FLTStyle: TFILTERSET): boolean; overload;
begin
  value:= FLT_BASE +FLT_ALLFILES;
  case FLTStyle of
   ftOpen: result:= BuildFilter(value, [FLT_PROJECTS, FLT_HEADS, FLT_CS, FLT_CPPS, FLT_RES]);
   ftHelp: result:= BuildFilter(value, [FLT_HELPS]);
   ftPrj: result:= BuildFilter(value, [FLT_PROJECTS]);
   ftSrc: result:= BuildFilter(value, [FLT_HEADS, FLT_RES, FLT_CS, FLT_CPPS]);
   ftAll: result:= BuildFilter(value, [FLT_PROJECTS, FLT_HEADS, FLT_RES, FLT_CS,
     FLT_CPPS]);
  else
   result:= TRUE;
  end;
end;

function BuildFilter(var value: string; const _filters: array of string): boolean; overload;
var
 idx: integer;
begin
  result:= FALSE;
  value:= FLT_BASE +FLT_ALLFILES;
  for idx:= 0 to high(_Filters) do
   if not AddFilter(value, _Filters[idx]) then
    exit;
  result:= TRUE;
end;

function CodeInstoStr(s: string): string;
begin
  result:= StringReplace(s, #13#10, '$_', [rfReplaceAll]);
end;

function StrtoCodeIns(s: string): string;
begin
  result:= StringReplace(s, '$_', #13#10, [rfReplaceAll]);
end;

procedure StrtoPoint(var pt: TPoint; value: string);
var
 tmp: TStringList;
begin
  tmp:= TStringList.Create;
  try
   tmp.CommaText:= value;
   if tmp.Count>= 2 then
    with pt do
     begin
       // x=foreground y=background
       x:= StringToColor(tmp[1]);
       y:= StringtoColor(tmp[0]);
     end;
  finally
   tmp.Free;
  end;
end;

function PointtoStr(const pt: TPoint): string;
begin
  result:= format('%d, %d', [pt.y, pt.x]);
end;

function AttrtoStr(const Attr: TSynHighlighterAttributes): string;
begin
  result:= format('%s, %s, %d, %d, %d',
           [ColortoString(Attr.Foreground),
            ColortoString(Attr.Background),
            ord(fsBold in Attr.Style),
            ord(fsItalic in Attr.Style),
            ord(fsUnderline in Attr.Style)]);
end;

procedure StrtoAttr(var Attr: TSynHighlighterAttributes; Value: string);
var
 tmp: TStringList;
begin
  tmp:= TStringList.Create;
  try
   tmp.commaText:= Value;
   if tmp.count = 5 then
    with attr do
     begin
       Foreground:= StringtoColor(tmp[0]);
       Background:= StringtoColor(tmp[1]);
       style:= [];
       if tmp[2] = '1' then
        style:= style +[fsbold]
       else
        style:= style -[fsbold];
       if tmp[3] = '1' then
        style:= style +[fsItalic]
       else
        style:= style -[fsItalic];
       if tmp[4] = '1' then
        style:= style +[fsUnderline]
       else
        style:= style -[fsUnderline];
     end;
  finally
   tmp.Free;
  end;
end;

(*procedure StrtoList(s: string; const List: TStrings; const delimiter: char=';');
begin
  List.BeginUpdate;
  try
   List.Clear;
   List.CommaText:= stringreplace(s, delimiter, ',', [rfReplaceAll, rfIgnoreCase]);
  finally
   List.EndUpdate;
  end;
end;*)

function CommaStrToStr(s : string; formatstr : string) : string;
var i : integer;
    tmp : string;
begin
  result := '';
  while pos(';', s) > 0 do begin
    i := pos(';', s);
    tmp := Copy(s, 1, i - 1);
    Delete(s, 1, i);
    result := format(formatstr, [result, tmp]);
  end;
  if s <> '' then
    result := format(formatstr, [result, s]);
end;

procedure StrtoList(s: string; const List: TStrings; const delimiter: char=';');
var tmp : string;
    i   : integer;
begin
  List.BeginUpdate;
  try
   List.Clear;
   while pos(delimiter, s) > 0 do begin
     i := pos(delimiter, s);
     tmp := Copy(s, 1, i - 1);
     Delete(s, 1, i);
     List.Add(tmp);
   end;
   if s <> '' then
     List.Add(s);
  finally
   List.EndUpdate;
  end;
end;

(*function ListtoStr(const List: TStrings): string;
begin
  if List.Count = 0 then
   result:= ''
  else
   result:= StringReplace(List.CommaText, ',', ';', [rfReplaceAll, rfIgnoreCase]);
end;*)

function ListtoStr(const List: TStrings): string;
var i : integer;
begin
  result := '';
  for i := 0 to List.Count - 1 do begin
    if i = 0 then
      result := List.Strings[0]
    else
      result := result + ';' + List.Strings[i];
  end;
end;

function GetFileTyp(const FileName: string): TUnitType;
var
 ext: string;
begin
  Ext:= ExtractfileExt(FileName);
  if AnsiCompareText(Ext, DEV_EXT) = 0 then
   result:= utPrj
  else
  if AnsiMatchText(ext, ['.c', '.cpp', '.cc', '.cxx', '.c++', '.cp']) then
   result:= utsrc
  else
  if AnsiMatchText(ext, ['.h', '.hpp', '.rh', '.hh', '.hxx']) then
   result:= utHead
  else
  if AnsiMatchText(ext, ['.res', '.rc']) then
   result:= utRes
  else
   result:= utOther;
end;

// this function sucks -- need to replace
function GetExTyp(const FileName: string): TExUnitType;
var
 idx: integer;
 s: string;
begin
  s:= ExtractFileExt(FileName);
  result:= utxother;
  for idx:= 0 to high(ctypes) do
   if AnsiCompareText(s, ctypes[idx]) = 0 then
    begin
      result:= utcsrc;
      exit;
    end;
  for idx:= 0 to high(cpptypes) do
   if AnsiCompareText(s, cpptypes[idx]) = 0 then
    begin
      result:= utcppsrc;
      exit;
    end;
  for idx:= 0 to high(headtypes) do
   if AnsiCompareText(s, headTypes[idx]) = 0 then
    begin
      case idx of
       0: result:= utchead;
       1: result:= utcppHead;
       2: result:= utresHead;
      end;
      exit;
    end;
  for idx:= 0 to high(restypes) do
   if AnsiCompareText(s, restypes[idx]) = 0 then
    begin
      case idx of
       0: result:= utresComp;
       1: result:= utresSrc;
      end;
      exit;
    end;
end;

// seems stupid now but I want to expand to account for .. chars
//in basepath and or filename
function ExpandFileto(const FileName: string; const BasePath: string): string;
var
 oldPath: string;
begin
  oldPath:= GetCurrentDir;
  try
   if DirectoryExists(BasePath) then begin
     chdir(BasePath);
     result:= ExpandFileName(FileName);
   end
   else
    Result:=Filename; // no luck...
  finally
   chdir(oldPath);
  end;
end;

function FileSamePath(const FileName: string; const TestPath: string): boolean;
var
 s1, s2: string;
begin
  result:= FALSE;
  s1:= ExtractFilePath(FileName);
  s2:= ExtractFilePath(TestPath);
  if (s1 = s2) then
   result:= TRUE
  else
   if (s1 = '') then
    result:= FileExists(s2 +FileName);
end;

procedure CloneMenu(const FromMenu: TMenuItem; ToMenu: TMenuItem);
var
 idx: integer;
 Item: TMenuItem;
begin
  ToMenu.Clear;
  if FromMenu.Count <= 0 then exit;
  for idx:= 0 to pred(FromMenu.Count) do
   begin
     Item:= TMenuItem.Create(ToMenu);
     with FromMenu.Items[idx] do
      begin
        Item.Caption:= Caption;
        Item.OnClick:= OnClick;
        Item.Tag:= Tag;
        Item.AutoCheck:= AutoCheck;
        Item.ShortCut:= ShortCut;
      end;
     ToMenu.Add(Item);
   end;
  ToMenu.Visible:= FromMenu.Visible;
end;

function GetLastPos(const SubStr: string; const s: string): integer;
var
  Last,
  Current: PAnsiChar;
begin
  result:= 0;
  Last:= nil;
  Current:= PAnsiChar(s);
  while (Current <> nil) and (Current^ <> #0) do
  begin
    Current:= AnsiStrPos(PAnsiChar(Current), PAnsiChar(SubStr));
    if Current <> nil then
    begin
      Last:= Current;
      inc(Current, length(SubStr));
    end;
  end;
  if Last <> nil then
   result:= abs((longint(PAnsiChar(s)) -longint(Last)) div sizeof(AnsiChar)) +1;
end;

{ GenMakePath: convert a filename to a format that can be used by make }
function GenMakePath(FileName: String): String;
begin
  Result := GenMakePath(FileName, False, True);
end;

function GenMakePath2(FileName: String): String;
begin
  Result := GenMakePath(FileName, True, False);
end;

function GenMakePath(FileName: String; EscapeSpaces,
                     EncloseInQuotes: Boolean): String;
begin
  Result := FileName;

  { Convert backslashes to slashes }
  Result := StringReplace(Result, '\', '/', [rfReplaceAll]);

  if EscapeSpaces then
    Result := StringReplace(Result, ' ', '\ ', [rfReplaceAll]);

  if EncloseInQuotes then
    if (Pos(' ', Result) > 0) then
      Result := '"' + Result + '"';
end;

function GetRealPath(BrokenFileName: String; Directory: String): String;
var
  e: TEditor;
begin
  Result := BrokenFileName;

  { There are 3 kinds of bad filenames:
    1: C:/Foo/Bar.txt              (must be backslashes)
    2: /C/WINDOWS/Desktop/foo.c    (WinUnix paths?)
    3: foo.c                       (not an absolute filename) }

  { First, check if this is a WinUnix path }
  if CompareText(Copy(Result, 1, 1), '/') = 0 then
  begin
      Delete(Result, 1, 2);
      Result[2] := ':';
      Insert('\', Result, 3);
  end;

  { Second, check if this is an absolute filename }
  if (Length(Result) < 4) or not ((LowerCase(Result)[1] in ['A'..'Z']) and (Result[2] = ':')) then
  begin
      { It's not. }
      if Length(Directory) = 0 then
      begin
          if Assigned(MainForm.fProject) then
              Result := ExpandFileTo(Result, MainForm.fProject.Directory)
          else begin
              e := MainForm.GetEditor;
              if (Assigned(e)) and (Length(ExtractFileDir(e.FileName)) > 0) then
                  Result := ExpandFileTo(Result, ExtractFileDir(e.FileName))
              else
                  Result := ExpandFileName(Result);
          end;
      end else
      begin
          Result := ExpandFileTo(Result, Directory);
      end;
  end;

  { Last, replace all slashes with backslahes }
{$IFDEF WIN32}
  StringReplace(Result, '/', '\', [rfReplaceAll]);
{$ENDIF}
end;

function CalcMod(Count: Integer): Integer;
begin
  if Count <= 15 then
      Result := 0
  else if Count <= 30 then
      Result := 2
  else if Count <= 65 then
      Result := 4
  else if Count <= 150 then
      Result := 8
  else if Count <= 300 then
      Result := 16
  else if Count <= 500 then
      Result := 32
  else if Count <= 750 then
      Result := 64
  else if Count <= 1500 then
      Result := 96
  else
      Result := 128;
end;

function IncludeQuoteIfSpaces(s : string) : string;
begin
  if pos(' ', s) > 0 then
    result := '"' + s + '"'
  else
    result := s;
end;

function IncludeQuoteIfNeeded(s : string) : string;
begin
  if pos('"', s) = 0 then
    result := '"' + s + '"'
  else
    result := s;
end;

// added by mandrav 13 Sep 2002
// returns the file version of the .exe specified by filename
// in the form x.x.x.x
function GetVersionString(FileName: string): string;
var
  Buf: Pointer;
  i: cardinal;
  P: pointer;
  pSize: cardinal;
  ffi: TVSFixedFileInfo;
begin
  Result := '';
  i := GetFileVersionInfoSize(PChar(FileName), i);
  if i = 0 then
    Exit;

  Buf := AllocMem(i);
  try
    if not GetFileVersionInfo(PChar(FileName), 0, i, Buf) then
      Exit;

    pSize := SizeOf(P);
    VerQueryValue(Buf, '\', p, pSize);

    ffi := TVSFixedFileInfo(p^);
    Result := Format('%d.%d.%d.%d', [
      HiWord(ffi.dwFileVersionMS),
        LoWord(ffi.dwFileVersionMS),
        HiWord(ffi.dwFileVersionLS),
        LoWord(ffi.dwFileVersionLS)]);
  finally
    FreeMem(Buf);
  end;
end;

function IsWinNT : boolean;
var ver : TOSVersionInfo;
begin
  ver.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  result := false;
  if GetVersionEx(ver) then begin
    if (ver.dwPlatformId = VER_PLATFORM_WIN32_NT) {and (ver.dwMajorVersion > 4) }then
      result := true;
  end
end;

// tries to change the current directory to Dir.
// Returns True if succesfull, False otherwise.
// If it succeeds, the Dir variable, holds the old dir.
function CheckChangeDir(var Dir: string): boolean;
var
  Old: string;
begin
  Old:=GetCurrentDir;
  Result:=SetCurrentDir(Dir);
  if Result then
    Dir:=Old;
end;

function GetAssociatedProgram(const Extension: string; var Filename, Description: string): boolean;
const
  NOVALUE = '$__NONE__$';
var
  R: TRegIniFile;
  Base, S: string;
begin
  Result:=False;
  R:=TRegIniFile.Create;
  try
    R.RootKey:=HKEY_CLASSES_ROOT;
    Base:=R.ReadString(Extension, '', NOVALUE);
    if S=NOVALUE then
      Exit;
    S:=R.ReadString(Base+'\shell\open\command', '', NOVALUE);
    if S=NOVALUE then
      Exit;
    Filename:=S; // filename probably contains args, e.g. Filename='"some where\my.exe" "%1"'

    Description:=ExtractFilename(S);
    Result:=True;
    S:=R.ReadString(Base+'\shell\open\ddeexec\application', '', NOVALUE);
    if S=NOVALUE then
      Description:='Default application'
    else
      Description:=S;
    if S='DEVCPP' then // avoid extensions registered to DevCpp ;)
      Result:=False;
  finally
    R.Free;
  end;
end;

function IsNumeric(s : string) : boolean;
var i : integer;
begin
  result := true;
  for i := 1 to length(s) do
    if not (s[i] in ['0'..'9']) then begin
      result := false;
      exit;
    end;
end;

end.

