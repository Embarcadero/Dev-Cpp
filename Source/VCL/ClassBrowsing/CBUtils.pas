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

unit CBUtils;

interface

uses
{$IFDEF WIN32}
  SysUtils, StrUtils, Classes;
{$ENDIF}
{$IFDEF LINUX}
SysUtils, StrUtils;
{$ENDIF}

const
  HeaderExts: array[0..6] of AnsiString = ('.h', '.hpp', '.rh', '.hh', '.hxx', '.inl', '');
  SourceExts: array[0..5] of AnsiString = ('.c', '.cpp', '.cc', '.cxx', '.c++', '.cp');

type
  PFileIncludes = ^TFileIncludes;
  TFileIncludes = record
    BaseFile: AnsiString;
    IncludeFiles: AnsiString; // "file","file" etc
  end;

  TStatementKind = (
    skClass,
    skFunction,
    skConstructor,
    skDestructor,
    skVariable,
    skTypedef,
    skEnum,
    skPreprocessor,
    skUnknown
    );
  TStatementKindSet = set of TStatementKind;

  TStatementScope = (
    ssGlobal,
    ssLocal,
    ssClassLocal
    );

  TStatementClassScope = (
    scsPublic,
    scsPrivate,
    scsProtected,
    scsNone
    );

  PStatement = ^TStatement;
  TStatement = record
    _Parent: PStatement; // parent class/struct/namespace
    _HintText: AnsiString; // text to force display when using PrettyPrintStatement
    _Type: AnsiString; // type "int"
    _Command: AnsiString; // identifier/name of statement "foo"
    _Args: AnsiString; // args "(int a,float b)"
    _Kind: TStatementKind; // kind of statement class/variable/function/etc
    _InheritsFromStatements: TList; // item below is converted to PStatements and stored here
    _InheritsFromStatementsText: AnsiString; // temporary storage for classes inherited from
    _Scope: TStatementScope; // global/local/classlocal
    _ClassScope: TStatementClassScope; // protected/private/public
    _HasDefinition: boolean; // definiton line/filename is valid
    _Line: integer; // declaration
    _DefinitionLine: integer; // definition
    _FileName: AnsiString; // declaration
    _DefinitionFileName: AnsiString; // definition
    _Visible: boolean; // visible in class browser or not
    _Temporary: boolean; // statements to be deleted after parsing
    _Loaded: boolean; // statement from cache
    _InProject: boolean; // statement in project
    _InSystemHeader: boolean; // statement in system header (#include <>)
  end;

  TProgressEvent = procedure(Sender: TObject; const FileName: AnsiString; Total, Current: integer) of object;
  TProgressEndEvent = procedure(Sender: TObject; Total: integer) of object;

  // These functions are about six times faster than the locale sensitive AnsiX() versions
function StartsStr(const subtext, text: AnsiString): boolean;
function StartsText(const subtext, text: AnsiString): boolean;

function SameStr(const s1, s2: AnsiString): boolean;
function SameText(const s1, s2: AnsiString): boolean;

function EndsStr(const subtext, text: AnsiString): boolean;
function EndsText(const subtext, text: AnsiString): boolean;

function ContainsStr(const text, subtext: AnsiString): boolean;
function ContainsText(const text, subtext: AnsiString): boolean;

// Same as StringReplace, but only replace first OldPattern (a lot faster)
function ReplaceFirstStr(const S, OldPattern, NewPattern: AnsiString): AnsiString;
function ReplaceFirstText(const S, OldPattern, NewPattern: AnsiString): AnsiString;

// Reverse Pos() function
function LastPos(const SubStr, S: AnsiString): integer;

// Fast implementation of StringReplace which does not use AnsiX (MBCS ready) comparison
function FastStringReplace(const S, OldPattern, NewPattern: AnsiString; Flags: TReplaceFlags): AnsiString;

// Fast implementation of IndexOf which does not use AnsiX comparison
function FastIndexOf(List: TStrings; const S: AnsiString): integer; overload;
function FastIndexOf(List: TStringlist; const S: AnsiString): integer; overload;

// Needed by Parser and Preprocessor (and class browser)
function IsSystemHeaderFile(const FileName: AnsiString; IncludePaths: TStringList): boolean;
function GetSystemHeaderFileName(const FileName: AnsiString; IncludePaths: TStringList): AnsiString; // <file.h>
function GetLocalHeaderFileName(const RelativeTo, FileName: AnsiString; ProjectIncludePaths: TStringList): AnsiString;
// "file.h"
function GetHeaderFileName(const RelativeTo, Line: AnsiString; IncludePaths, ProjectIncludePaths: TStringList):
  AnsiString; // both
function IsCfile(const Filename: AnsiString): boolean;
function IsHfile(const Filename: AnsiString): boolean;
procedure GetSourcePair(const FName: AnsiString; var CFile, HFile: AnsiString);
function IsIncludeLine(const Line: AnsiString): boolean;

implementation

function FastStringReplace(const S, OldPattern, NewPattern: AnsiString; Flags: TReplaceFlags): AnsiString;
var
  SearchStr, Patt, NewStr: AnsiString;
  Offset: Integer;
begin
  if rfIgnoreCase in Flags then begin
    SearchStr := UpperCase(S);
    Patt := UpperCase(OldPattern);
  end else begin
    SearchStr := S;
    Patt := OldPattern;
  end;
  NewStr := S;
  Result := '';
  while SearchStr <> '' do begin
    Offset := Pos(Patt, SearchStr);
    if Offset = 0 then begin
      Result := Result + NewStr;
      Break;
    end;
    Result := Result + Copy(NewStr, 1, Offset - 1) + NewPattern;
    NewStr := Copy(NewStr, Offset + Length(OldPattern), MaxInt);
    if not (rfReplaceAll in Flags) then begin
      Result := Result + NewStr;
      Break;
    end;
    SearchStr := Copy(SearchStr, Offset + Length(Patt), MaxInt);
  end;
end;

function FastIndexOf(List: TStrings; const S: AnsiString): integer;
begin
  with List do begin
    for Result := 0 to Count - 1 do
      if CompareText(List[Result], S) = 0 then
        Exit;
    Result := -1;
  end;
end;

function FastIndexOf(List: TStringlist; const S: AnsiString): integer;
begin
  with List do begin
    if not List.Sorted then
      Result := FastIndexOf(TStrings(List), S)
    else if not Find(S, Result) then
      Result := -1;
  end;
end;

function StartsStr(const subtext, text: AnsiString): boolean;
begin
  Result := SameStr(subtext, Copy(text, 1, Length(subtext)));
end;

function StartsText(const subtext, text: AnsiString): boolean;
begin
  Result := SameText(subtext, Copy(text, 1, Length(subtext)));
end;

function SameStr(const s1, s2: AnsiString): boolean;
begin
  Result := (CompareStr(s1, s2) = 0);
end;

function SameText(const s1, s2: AnsiString): boolean;
begin
  Result := (CompareText(s1, s2) = 0);
end;

function EndsStr(const subtext, text: AnsiString): boolean;
var
  SubTextLocation: Integer;
begin
  SubTextLocation := Length(text) - Length(subtext) + 1;
  if (SubTextLocation > 0) and (subtext <> '') then
    Result := StrComp(Pointer(subtext), Pointer(@text[SubTextLocation])) = 0
  else
    Result := False;
end;

function EndsText(const subtext, text: AnsiString): boolean;
var
  SubTextLocation: Integer;
begin
  SubTextLocation := Length(text) - Length(subtext) + 1;
  if (SubTextLocation > 0) and (subtext <> '') then
    Result := StrIComp(Pointer(subtext), Pointer(@text[SubTextLocation])) = 0
  else
    Result := False;
end;

function ContainsStr(const text, subtext: AnsiString): boolean;
begin
  Result := Pos(subtext, text) > 0;
end;

function ContainsText(const text, subtext: AnsiString): boolean;
begin
  Result := Pos(UpperCase(subtext), UpperCase(text)) > 0;
end;

function ReplaceFirstStr(const S, OldPattern, NewPattern: AnsiString): AnsiString;
var
  Offset: Integer;
begin
  Offset := Pos(OldPattern, S);
  if Offset = 0 then begin
    Result := S;
  end else begin
    // Copy the preceding stuff, append the new part, append old stuff after old pattern
    Result := Copy(S, 1, Offset - 1) + NewPattern + Copy(S, Offset + Length(OldPattern), MaxInt);
  end;
end;

function ReplaceFirstText(const S, OldPattern, NewPattern: AnsiString): AnsiString;
var
  Offset: Integer;
  UpperS, UpperOldPattern: string;
begin
  UpperS := UpperCase(S);
  UpperOldPattern := UpperCase(OldPattern);

  Offset := Pos(UpperOldPattern, UpperS);
  if Offset = 0 then begin
    Result := S;
  end else begin

    // Copy the preceding stuff, append the new part, append old stuff after old pattern
    Result := Copy(S, 1, Offset - 1) + NewPattern + Copy(S, Offset + Length(UpperOldPattern), MaxInt);
  end;
end;

function LastPos(const SubStr, s: AnsiString): integer;
begin
  result := Pos(ReverseString(SubStr), ReverseString(S));
  if result <> 0 then
    result := ((Length(S) - Length(SubStr)) + 1) - result + 1;
end;

function IsSystemHeaderFile(const FileName: AnsiString; IncludePaths: TStringList): boolean;
var
  FilePath: AnsiString;
  I: integer;
begin
  Result := false;

  // If it's a full file name, check if its directory is an include path
  if (Length(FileName) > 2) and (FileName[2] = ':') then begin // full file name
    Result := false;
    if FileExists(FileName) then begin // the file must exist
      FilePath := ExtractFileDir(FileName); // also extracts last \
      for I := 0 to IncludePaths.Count - 1 do begin
        if FilePath = IncludePaths[I] then begin // and its path
          Result := true;
          Exit;
        end;
      end;
    end;

    // Not a full file name, check if it's in a include path
  end else begin
    for I := 0 to IncludePaths.Count - 1 do
      if FileExists(IncludePaths[I] + '\' + FileName) then begin
        Result := true;
        Exit;
      end;
  end;
end;

function IsIncludeLine(const Line: AnsiString): boolean;
var
  TrimmedLine: AnsiString;
begin
  Result := False;
  TrimmedLine := Trim(Line);
  if (Length(TrimmedLine) > 0) and (TrimmedLine[1] = '#') then begin // it's a preprocessor line
    if StartsStr('include', TrimLeft(Copy(TrimmedLine, 2, MaxInt))) then begin // the first word after # is 'include'
      Result := True;
    end;
  end;
end;

function GetLocalHeaderFileName(const RelativeTo, FileName: AnsiString; ProjectIncludePaths: TStringList): AnsiString;
var
  I: integer;
  Dir: AnsiString;
begin
  Result := FileName;

  // Try to convert a C++ filename from cxxx to xxx.h (ignore std:: namespace versions)
  if StartsStr('c', Result) and not EndsStr('.h', Result) and not ContainsStr(Result, '.') then begin
    Delete(Result, 1, 1);
    Result := Result + '.h';
  end;

  // Search local directory
  Dir := ExtractFilePath(RelativeTo);
  if FileExists(Dir + Result) then begin // same dir as file
    Result := Dir + Result;
    Exit;
  end;

  // Search project include directories
  for I := 0 to ProjectIncludePaths.Count - 1 do
    if FileExists(ProjectIncludePaths[I] + '\' + FileName) then begin
      Result := ProjectIncludePaths[I] + '\' + FileName;
      Exit;
    end;

  Result := FileName; // signifies failure
end;

function GetSystemHeaderFileName(const FileName: AnsiString; IncludePaths: TStringList): AnsiString;
var
  I: integer;
begin
  Result := FileName;

  // Try to convert a C++ filename from cxxx to xxx.h (ignore std:: namespace versions)
  if StartsStr('c', Result) and not EndsStr('.h', Result) and not ContainsStr(Result, '.') then begin
    Delete(Result, 1, 1);
    Result := Result + '.h';
  end;

  // Search compiler include directories
  for I := 0 to IncludePaths.Count - 1 do
    if FileExists(IncludePaths[I] + '\' + FileName) then begin
      Result := IncludePaths[I] + '\' + FileName;
      Exit;
    end;

  Result := FileName; // signifies failure
end;

function GetHeaderFileName(const RelativeTo, Line: AnsiString; IncludePaths, ProjectIncludePaths: TStringList):
  AnsiString;
var
  OpenTokenPos, CloseTokenPos: integer;
begin
  Result := '';

  // Handle <>
  OpenTokenPos := Pos('<', Line);
  if OpenTokenPos > 0 then begin
    CloseTokenpos := Pos('>', Line);
    if CloseTokenPos > 0 then begin
      Result := Copy(Line, OpenTokenPos + 1, CloseTokenPos - OpenTokenPos - 1);
      Result := GetSystemHeaderFileName(Result, IncludePaths);
    end;
  end else begin

    // Try ""
    OpenTokenPos := Pos('"', Line);
    if OpenTokenPos > 0 then begin
      CloseTokenpos := Pos('"', Copy(Line, OpenTokenPos + 1, MaxInt));
      if CloseTokenPos > 0 then begin
        Inc(CloseTokenPos, OpenTokenPos);
        Result := Copy(Line, OpenTokenPos + 1, CloseTokenPos - OpenTokenPos - 1);
        Result := GetLocalHeaderFileName(RelativeTo, Result, ProjectIncludePaths);
      end;
    end;
  end;
end;

function IsCfile(const Filename: AnsiString): boolean;
var
  ext: AnsiString;
  i: integer;
begin
  result := false;

  ext := LowerCase(ExtractFileExt(Filename));
  for I := Low(SourceExts) to High(SourceExts) do
    if ext = SourceExts[i] then begin
      result := true;
      Exit;
    end;
end;

function IsHfile(const Filename: AnsiString): boolean;
var
  ext: AnsiString;
  i: integer;
begin
  result := false;
  if FileName = '' then
    Exit;

  // Files without an extension can be headers too
  ext := LowerCase(ExtractFileExt(Filename));
  for I := Low(HeaderExts) to High(HeaderExts) do
    if ext = HeaderExts[i] then begin
      result := true;
      Exit;
    end;
end;

procedure GetSourcePair(const FName: AnsiString; var CFile, HFile: AnsiString);
var
  i: integer;
begin
  if IsCfile(FName) then begin

    CFile := FName;
    HFile := '';

    // Find corresponding header
    for I := Low(HeaderExts) to High(HeaderExts) do
      if FileExists(ChangeFileExt(FName, HeaderExts[i])) then begin
        HFile := ChangeFileExt(FName, HeaderExts[i]);
        break;
      end;
  end else if IsHfile(FName) then begin

    HFile := FName;
    CFile := '';

    // Find corresponding source
    for I := Low(SourceExts) to High(SourceExts) do
      if FileExists(ChangeFileExt(FName, SourceExts[i])) then begin
        CFile := ChangeFileExt(FName, SourceExts[i]);
        break;
      end;
  end else begin
    CFile := FName;
    HFile := '';
  end;
end;

end.

