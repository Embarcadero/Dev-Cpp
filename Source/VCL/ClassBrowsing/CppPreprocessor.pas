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

unit CppPreprocessor;

interface

uses
{$IFDEF WIN32}
  Windows, Classes, SysUtils, StrUtils, ComCtrls, Math, IntList, cbutils;
{$ENDIF}
{$IFDEF LINUX}
Classes, SysUtils, StrUtils, QComCtrls;
{$ENDIF}

const
  LineChars: set of Char = [#13, #10];
  SpaceChars: set of Char = [' ', #9];
  OperatorChars: set of Char = ['+', '-', '*', '/', '!', '=', '<', '>', '&', '|', '^'];
  IdentChars: set of Char = ['A'..'Z', '0'..'9', 'a'..'z', '_', '*', '&', '~'];
  MacroIdentChars: set of Char = ['A'..'Z', 'a'..'z', '_'];
  Operators: array[0..14] of string = ('*', '/', '+', '-', '<', '<=', '>', '>=', '==', '!=', '&', '^', '|', '&&', '||');

type
  PFile = ^TFile;
  TFile = record
    Index: integer; // 0-based for programming convenience
    FileName: AnsiString;
    Buffer: TStringList; // do not concat them all
  end;

  PDefine = ^TDefine;
  TDefine = record
    Name: AnsiString;
    Args: AnsiString;
    Value: AnsiString;
    HardCoded: boolean; // if true, don't free memory (points to hard defines)
  end;

  TCppPreprocessor = class(TComponent)
  private
    fIndex: integer; // points to current file buffer. do not free
    fFileName: AnsiString; // idem
    fBuffer: TStringList; // idem
    fResult: TStringList;
    fCurrentIncludes: PFileIncludes;
    fPreProcIndex: integer;
    fIncludesList: TList;
    fHardDefines: TList; // set by "cpp -dM -E -xc NUL"
    fDefines: TList; // working set, editable
    fIncludes: TList; // list of files we've stepped into. last one is current file, first one is source file
    fBranchResults: TIntList;
    // list of branch results (boolean). last one is current branch, first one is outermost branch
    fIncludePaths: TStringList; // *pointer* to buffer of CppParser
    fProjectIncludePaths: TStringList;
    fScannedFiles: TStringList; // idem
    fParseSystem: boolean;
    fParseLocal: boolean;
    procedure PreprocessBuffer;
    procedure SkipToEndOfPreprocessor;
    procedure SkipToPreprocessor;
    function GetNextPreprocessor: AnsiString;
    procedure Simplify(var Output: AnsiString);
    procedure HandlePreprocessor(const Value: AnsiString);
    procedure HandleDefine(const Line: AnsiString);
    procedure HandleUndefine(const Line: AnsiString);
    procedure HandleBranch(const Line: AnsiString);
    procedure HandleInclude(const Line: AnsiString);
    function ExpandMacros(const Line: AnsiString): AnsiString;
    function RemoveSuffixes(const Input: AnsiString): AnsiString;
    // current file stuff
    function GetInclude(index: integer): PFile;
    procedure OpenInclude(const FileName: AnsiString; Stream: TMemoryStream = nil);
    property Includes[index: integer]: PFile read GetInclude; default;
    procedure CloseInclude;
    // branch stuff
    function GetCurrentBranch: boolean;
    procedure SetCurrentBranch(value: boolean);
    procedure RemoveCurrentBranch;
    function GetResult: AnsiString;
    // include stuff
    function GetFileIncludesEntry(const FileName: AnsiString): PFileIncludes;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddDefineByParts(const Name, Args, Value: AnsiString; HardCoded: boolean);
    procedure GetDefineParts(const Input: AnsiString; var Name, Args, Value: AnsiString);
    procedure AddDefineByLine(const Line: AnsiString; HardCoded: boolean);
    function GetDefine(const Name: AnsiString): PDefine;
    procedure Reset;
    procedure ResetDefines;
    procedure SetScanOptions(ParseSystem, ParseLocal: boolean);
    procedure SetIncludePaths(var List: TStringList);
    procedure SetProjectIncludePaths(var List: TStringList);
    procedure SetScannedFileList(var List: TStringList);
    procedure SetIncludesList(var List: TList);
    procedure PreprocessStream(const FileName: AnsiString; Stream: TMemoryStream);
    procedure PreprocessFile(const FileName: AnsiString);
    property Result: AnsiString read GetResult;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Dev-C++', [TCppPreprocessor]);
end;

constructor TCppPreprocessor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fIncludes := TList.Create;
  fHardDefines := TList.Create;
  fDefines := TList.Create;
  fBranchResults := TIntList.Create;
  fResult := TStringList.Create;
end;

destructor TCppPreprocessor.Destroy;
var
  I: integer;
begin
  for I := 0 to fIncludes.Count - 1 do begin
    PFile(fIncludes[i])^.Buffer.Free;
    Dispose(PFile(fIncludes[i]));
  end;
  fIncludes.Free;
  for I := 0 to fHardDefines.Count - 1 do
    Dispose(PDefine(fHardDefines[i]));
  fHardDefines.Free;
  for I := 0 to fDefines.Count - 1 do
    if not PDefine(fDefines[i])^.HardCoded then // has already been released
      Dispose(PDefine(fDefines[i]));
  fDefines.Free;
  fBranchResults.Free;
  fResult.Free;
  inherited Destroy;
end;

procedure TCppPreprocessor.Reset;
var
  I: integer;
begin
  fResult.Clear;

  // Clear extracted data
  for I := 0 to fIncludes.Count - 1 do begin
    PFile(fIncludes[i])^.Buffer.Free;
    Dispose(PFile(fIncludes[i]));
  end;
  fIncludes.Clear;
  fBranchResults.Clear;
  fCurrentIncludes := nil;
  ResetDefines; // do not throw away hardcoded
end;

function TCppPreprocessor.GetInclude(index: integer): PFile;
begin
  result := PFile(fIncludes[index]);
end;

procedure TCppPreprocessor.OpenInclude(const FileName: AnsiString; Stream: TMemoryStream = nil);
var
  FileItem: PFile;
  IsSystemFile: boolean;
  IncludeLine: AnsiString;
  I: integer;
begin
  // Backup old position if we're entering a new file
  if fIncludes.Count > 0 then
    PFile(fIncludes[fIncludes.Count - 1])^.Index := fIndex;

  // Add the new file to the includes of the current file
  // Only add items to the include list of the given file if the file hasn't been scanned yet
  // The above is fixed by checking for duplicates.
  // The proper way would be to do backtracking of files we have FINISHED scanned.
  // These are not the same files as the ones in fScannedFiles. We have STARTED scanning these.
  if Assigned(fCurrentIncludes) then
    with fCurrentIncludes^ do
      if not ContainsText(IncludeFiles, FileName) then
        IncludeFiles := IncludeFiles + AnsiQuotedStr(FileName, '"') + ',';

  // Create and add new buffer/position
  FileItem := new(PFile);
  FileItem^.Index := 0; // 0-based line counter
  FileItem^.FileName := FileName;
  FileItem^.Buffer := TStringList.Create;

  // Don't parse stuff we have already parsed
  if Assigned(Stream) or (FastIndexOf(fScannedFiles, FileName) = -1) then begin

    // Keep track of files we include here
    // Only create new items for files we have NOT scanned yet
    fCurrentIncludes := GetFileIncludesEntry(FileName);
    if not Assigned(fCurrentIncludes) then begin // do NOT create a new item for a file that's already in the list
      fCurrentIncludes := New(PFileIncludes);
      fCurrentIncludes^.BaseFile := FileName;
      fCurrentIncludes^.IncludeFiles := '';
      fIncludesList.Add(fCurrentIncludes);
    end;

    // Parse ONCE
    if not Assigned(Stream) then
      fScannedFiles.Add(FileName);

    // Only load up the file if we are allowed to parse it
    IsSystemFile := cbutils.IsSystemHeaderFile(FileName, fIncludePaths);
    if (fParseSystem and IsSystemFile) or (fParseLocal and not IsSystemFile) then begin
      if Assigned(Stream) then begin
        Stream.Position := 0; // start scanning from here
        FileItem^.Buffer.LoadFromStream(Stream)
      end else if FileExists(FileName) then
        FileItem^.Buffer.LoadFromFile(FileName); // load it now
    end;
  end;
  fIncludes.Add(FileItem);

  // Process it
  fIndex := FileItem^.Index;
  fFileName := FileItem^.FileName;
  fBuffer := FileItem^.Buffer;

  // Trim all lines
  for I := 0 to fBuffer.Count - 1 do
    fBuffer[i] := Trim(fBuffer[i]);

  // Update result file
  IncludeLine := '#include ' + FileName + ':1';
  if fIncludes.Count > 1 then // include from within a file
    fResult[fPreProcIndex] := IncludeLine
  else // new file
    fResult.Add(IncludeLine);
end;

procedure TCppPreprocessor.CloseInclude;
begin
  if fIncludes.Count > 0 then begin

    // Close current buffer
    PFile(fIncludes[fIncludes.Count - 1])^.Buffer.Free;
    Dispose(PFile(fIncludes[fIncludes.Count - 1]));
    fIncludes.Delete(fIncludes.Count - 1);

    if fIncludes.Count > 0 then begin

      // Continue where we left off
      fIndex := Includes[fIncludes.Count - 1]^.Index;
      fFileName := Includes[fIncludes.Count - 1]^.FileName;
      fBuffer := Includes[fIncludes.Count - 1]^.Buffer;
      // Point to previous buffer and start past the include we walked into

      // Start augmenting previous include list again
      fCurrentIncludes := GetFileIncludesEntry(fFileName);

      // Update result file (we've left the previous file)
      fResult.Add('#include ' + Includes[fIncludes.Count - 1]^.FileName + ':' + IntToStr(Includes[fIncludes.Count -
        1]^.Index + 1));
    end;
  end;
end;

function TCppPreprocessor.GetCurrentBranch: boolean;
begin
  if fBranchResults.Count > 0 then
    Result := boolean(fBranchResults[fBranchResults.Count - 1])
  else
    Result := True;
end;

procedure TCppPreprocessor.SetCurrentBranch(value: boolean);
begin
  fBranchResults.Add(integer(value));
end;

procedure TCppPreprocessor.RemoveCurrentBranch;
begin
  if fBranchResults.Count > 0 then
    fBranchResults.Delete(fBranchResults.Count - 1);
end;

procedure TCppPreprocessor.SetScanOptions(ParseSystem, ParseLocal: boolean);
begin
  fParseSystem := ParseSystem;
  fParseLocal := ParseLocal;
end;

procedure TCppPreprocessor.SetIncludesList(var List: TList);
begin
  fIncludesList := List;
end;

procedure TCppPreprocessor.SetIncludePaths(var List: TStringList);
begin
  fIncludePaths := List;
end;

procedure TCppPreprocessor.SetProjectIncludePaths(var List: TStringList);
begin
  fProjectIncludePaths := List;
end;

procedure TCppPreprocessor.SetScannedFileList(var List: TStringList);
begin
  fScannedFiles := List;
end;

procedure TCppPreprocessor.SkipToPreprocessor;
  function FirstLineChar(const Line: AnsiString): Char;
  begin
    if Length(Line) > 0 then
      Result := TrimLeft(Line)[1] // assume trimmed lines
    else
      Result := #0;
  end;
begin
  // Increment until a line begins with a #
  while (fIndex < fBuffer.Count) and (FirstLineChar(fBuffer[fIndex]) <> '#') do begin
    if GetCurrentBranch then // if not skipping, expand current macros
      fResult.Add(ExpandMacros(fBuffer[fIndex]))
    else // If skipping due to a failed branch, clear line
      fResult.Add('');
    Inc(fIndex);
  end;
end;

procedure TCppPreprocessor.SkipToEndOfPreprocessor;
  function LastLineChar(const Line: AnsiString): Char;
  begin
    if Length(Line) > 0 then
      Result := Line[Length(Line)] // assume trimmed lines
    else
      Result := #0;
  end;
begin
  // Skip until last char of line is NOT \ anymore
  while (fIndex < fBuffer.Count) and (LastLineChar(fBuffer[fIndex]) = '\') do begin
    Inc(fIndex);
  end;
end;

function TCppPreprocessor.GetNextPreprocessor: AnsiString;
var
  I: integer;
  PreProcFrom, PreProcTo: integer;
begin
  Result := '';

  SkipToPreprocessor; // skip until # at start of line
  PreProcFrom := fIndex;
  if PreProcFrom >= fBuffer.Count then // we've gone past the final #preprocessor line. Yay
    Exit;

  SkipToEndOfPreprocessor;
  PreProcTo := fIndex;

  // Calculate index to insert defines in in result file
  fPreProcIndex := (fResult.Count - 1) + 1; // offset by one for #include rootfile

  // Assemble whole line, including newlines
  for I := PreProcFrom to PreProcTo do begin
    Result := Result + fBuffer[i] + #13#10;
    fResult.Add(''); // defines resolve into empty files, except #define and #include
  end;

  // Step over
  Inc(fIndex);
end;

procedure TCppPreprocessor.Simplify(var Output: AnsiString);
var
  DelimPosFrom, DelimPosTo: integer;
begin
  // Remove #
  Output := Copy(Output, 2, MaxInt);

  // Remove newlines in concatenated expressions
  Output := FastStringReplace(Output, '\'#13, '', [rfReplaceAll]);
  Output := FastStringReplace(Output, '\'#10, '', [rfReplaceAll]);
  Output := FastStringReplace(Output, #13, '', [rfReplaceAll]);
  Output := FastStringReplace(Output, #10, '', [rfReplaceAll]);

  // Remove C-style comments
  while true do begin
    DelimPosFrom := Pos('/*', Output);
    if DelimPosFrom > 0 then begin
      DelimPosTo := PosEx('*/', Output, DelimPosFrom);
      if DelimPosTo > 0 then
        Delete(Output, DelimPosFrom, DelimPosTo - DelimPosFrom + Length('*/'))
      else
        break; // invalid syntax. ignore
    end else
      break;
  end;

  // Don't remove multiple spaces. This can ruin defines which explicity use multiple spaces in their values

  Output := Trim(Output); // removes spaces between # and the first word
end;

function TCppPreprocessor.GetDefine(const Name: AnsiString): PDefine;
var
  I: integer;
begin
  // Get the first define only. Multiple defines cause warnings now, so ignore them
  for I := 0 to fDefines.Count - 1 do begin
    if PDefine(fDefines[i])^.Name = Name then begin
      result := PDefine(fDefines[i]);
      Exit;
    end;
  end;
  result := nil;
end;

procedure TCppPreprocessor.AddDefineByParts(const Name, Args, Value: AnsiString; HardCoded: boolean);
var
  Item: PDefine;
begin
  // Do not check for duplicates. It's too slow
  Item := new(PDefine);
  Item^.Name := Name;
  Item^.Args := Args;
  Item^.Value := Value;
  Item^.HardCoded := HardCoded;
  if HardCoded then
    fHardDefines.Add(Item)
  else
    fDefines.Add(Item);
end;

// input should omit the define word

procedure TCppPreprocessor.GetDefineParts(const Input: AnsiString; var Name, Args, Value: AnsiString);
var
  I, Level, ArgStart: integer;
  S: AnsiString;
  IsFunction: boolean;
begin
  S := TrimLeft(Input);
  Name := '';
  Args := '';
  Value := '';

  // Rules:
  // When the character before the first opening brace is nonblank, a function is defined.
  // After that point, switch from name to args
  // The value starts after the first blank character outside of the outermost () pair

  I := 1;
  Level := 0;
  IsFunction := False;
  ArgStart := 0;
  while I <= Length(S) do begin

    // When we find the first opening brace, check if this is a function define
    if S[i] = '(' then begin
      Inc(Level);
      if (Level = 1) and not IsFunction then begin // found a function define!
        Name := Copy(S, 1, I - 1);
        ArgStart := I;
        IsFunction := True;
      end;
    end else if S[i] = ')' then begin
      Dec(Level);
    end else if (S[i] in SpaceChars) and (Level = 0) then
      break; // found the end of our idenfifier
    Inc(I);
  end;

  if IsFunction then begin
    // Name has already been found
    Args := Copy(S, ArgStart, I - ArgStart);
  end else begin
    Name := Trim(Copy(S, 1, I));
    Args := '';
  end;
  Value := TrimLeft(Copy(S, I + 1, MaxInt));
end;

procedure TCppPreprocessor.AddDefineByLine(const Line: AnsiString; HardCoded: boolean);
var
  Name, Args, Value, S: AnsiString;
begin
  // Remove define
  S := TrimLeft(Copy(Line, Length('define') + 1, MaxInt));

  // Get parts from generalized function
  GetDefineParts(S, Name, Args, Value);

  // Add to the list
  AddDefineByParts(Name, Args, Value, HardCoded);
end;

procedure TCppPreprocessor.ResetDefines;
var
  I: integer;
begin
  // Assign hard list to soft list
  for I := 0 to fDefines.Count - 1 do begin // remove memory first
    if not PDefine(fDefines[i])^.HardCoded then // memory belongs to hardcoded list
      Dispose(PDefine(fDefines[i]));
  end;
  fDefines.Clear;
  fDefines.Assign(fHardDefines); // then assign
end;

procedure TCppPreprocessor.HandlePreprocessor(const Value: AnsiString);
begin
  if StartsStr('define', Value) then
    HandleDefine(Value)
  else if StartsStr('undef', Value) then
    HandleUndefine(Value)
  else if StartsStr('if', Value) or StartsStr('else', Value) or StartsStr('elif', Value) or StartsStr('endif', Value)
    then
    HandleBranch(Value)
  else if StartsStr('include', Value) then
    HandleInclude(Value);
end;

procedure TCppPreprocessor.HandleDefine(const Line: AnsiString);
begin
  if GetCurrentBranch then begin
    AddDefineByLine(Line, false);
    fResult[fPreProcIndex] := '#' + Line; // add define to result file so the parser can handle it
  end;
end;

procedure TCppPreprocessor.HandleUndefine(const Line: AnsiString);
var
  Name: AnsiString;
  I: integer;
begin
  // Remove undef
  Name := TrimLeft(Copy(Line, Length('undef') + 1, MaxInt));

  // Remove from soft list only
  for I := fDefines.Count - 1 downto 0 do begin
    if PDefine(fDefines[i])^.Name = Name then begin
      if not PDefine(fDefines[i])^.HardCoded then // memory belongs to hardcoded list
        Dispose(PDefine(fDefines[i]));
      fDefines.Delete(i);
      break; // ignore overriden defines
    end;
  end;
end;

procedure TCppPreprocessor.HandleBranch(const Line: AnsiString);
var
  Name, IfLine: AnsiString;
  OldResult: boolean;
  I: integer;

  // Should start on top of the opening char
  function SkipBraces(const Line: AnsiString; var Index: integer; Step: integer = 1): boolean;
  var
    Level: integer;
  begin
    Level := 0;
    while (Index > 0) and (Index <= Length(Line)) do begin // Find the corresponding opening brace
      if Line[Index] = '(' then begin
        Inc(Level);
        if Level = 0 then begin
          Result := true;
          Exit;
        end;
      end else if Line[Index] = ')' then begin
        Dec(Level);
        if Level = 0 then begin
          Result := true;
          Exit;
        end;
      end;
      Inc(Index, Step);
    end;
    Result := false;
  end;

  // Expand any token that isn't a number
  function ExpandDefines(Line: AnsiString): AnsiString;
  var
    SearchPos, Head, Tail, NameStart, NameEnd: integer;
    Name, Args, InsertValue: AnsiString;
    Define: PDefine;

    function ExpandFunction(FunctionDefine: PDefine; const ArgValueString: AnsiString): AnsiString;
    var
      ArgNames, ArgValues: TStringList;
      I: integer;
    begin
      // Replace function by this string
      Result := FunctionDefine^.Value;

      // Replace names by values...
      ArgNames := TStringList.Create;
      ArgValues := TStringList.Create;
      try
        ExtractStrings([',', '(', ')'], [], PAnsiChar(FunctionDefine^.Args), ArgNames);
        ExtractStrings([',', '(', ')'], [], PAnsiChar(ArgValueString), ArgValues); // extract from Line string

        // If the argument count matches up, replace names by values
        if ArgNames.Count = ArgValues.Count then begin
          for I := 0 to ArgNames.Count - 1 do
            Result := StringReplace(Result, Trim(ArgNames[i]), Trim(ArgValues[i]), [rfReplaceAll]);
        end;
      finally
        ArgNames.Free;
        ArgValues.Free;
      end;
    end;
  begin
    SearchPos := 1;
    while (SearchPos <= Length(Line)) do begin

      // We have found an identifier. It is not a number suffix. Try to expand it
      if (Line[SearchPos] in MacroIdentChars) and ((SearchPos = 1) or not (Line[SearchPos - 1] in ['0'..'9'])) then begin
        Tail := SearchPos;
        Head := SearchPos;

        // Get identifier name (numbers are allowed, but not at the start
        while (Head <= Length(Line)) and ((Line[Head] in MacroIdentChars) or (Line[Head] in ['0'..'9'])) do
          Inc(Head);
        Name := Copy(Line, Tail, Head - Tail);
        NameStart := Tail;
        NameEnd := Head;

        // Skip over contents of these built-in functions
        if Name = 'defined' then begin
          Head := SearchPos + Length(Name);
          while (Head <= Length(Line)) and (Line[Head] in SpaceChars) do
            Inc(Head); // skip spaces

          // Skip over its arguments
          if SkipBraces(Line, Head) then begin
            SearchPos := Head;
          end else begin
            Line := ''; // broken line
            break;
          end;

          // We have found a regular define. Replace it by its value
        end else begin

          // Does it exist in the database?
          Define := GetDefine(Name);
          if not Assigned(Define) then begin
            InsertValue := '0';
          end else begin
            while (Head <= Length(Line)) and (Line[Head] in SpaceChars) do
              Inc(Head); // skip spaces

            // It is a function. Expand arguments
            if (Head <= Length(Line)) and (Line[Head] = '(') then begin
              Tail := Head;
              if SkipBraces(Line, Head) then begin
                Args := Copy(Line, Tail, Head - Tail + 1);
                InsertValue := ExpandFunction(Define, Args);
                NameEnd := Head + 1;
              end else begin
                Line := ''; // broken line
                break;
              end;

              // Replace regular define
            end else begin
              if Define^.Value <> '' then
                InsertValue := Define^.Value
              else
                InsertValue := '0';
            end;
          end;

          // Insert found value at place
          Delete(Line, NameStart, NameEnd - NameStart);
          Insert(InsertValue, Line, SearchPos);
        end;
      end else
        Inc(SearchPos);
    end;
    Result := Line;
  end;

  function EvaluateDefines(Line: AnsiString): AnsiString;
  var
    S: AnsiString;
    I, Head, Tail: integer;
    DefineResult, InvertResult: boolean;
  begin
    while true do begin
      I := Pos('defined', Line);
      if I > 0 then begin
        // Check for boolean inverter
        InvertResult := (I > 1) and (Line[I - 1] = '!');

        // Find expression enclosed in () or after space
        Tail := I + Length('defined'); // find (

        // Skip spaces after defined keyword
        while (Tail <= Length(Line)) and (Line[Tail] in SpaceChars) do
          Inc(Tail);

        // If we find an opening brace, find its closing brace
        Head := Tail;
        if (Head <= Length(Line)) and (Line[Head] = '(') then begin
          if not SkipBraces(Line, Head) then begin
            Result := '';
            Exit;
          end;
          S := Copy(Line, Tail + 1, Head - Tail - 1);
          // If we find an identifier, walk until it ends
        end else begin
          while (Head <= Length(Line)) and (Line[Head] in IdentChars) do // find end of identifier
            Inc(Head);
          S := Copy(Line, Tail, Head - Tail);
        end;

        // Delete expression from string
        Tail := I;
        if InvertResult then
          Dec(Tail);
        Delete(Line, Tail, Head - Tail + 1);

        // Evaludate and replace expression by 1 or 0 (true or false)
        DefineResult := Assigned(GetDefine(S));
        if (DefineResult and not InvertResult) or (not DefineResult and InvertResult) then
          Insert('1', Line, Tail)
        else
          Insert('0', Line, Tail);
      end else
        break;
    end;
    Result := Line;
  end;

  function EvaluateExpression(Line: AnsiString): AnsiString;
  var
    Head, Tail, EquatStart, EquatEnd, OperatorPos: integer;
    LeftOpValue, RightOpValue, ResultValue: Int64;
    LeftOp, RightOp, OperatorToken, ResultLine: AnsiString;

    function GetNextOperator(var Offset: integer): AnsiString;
    var
      I, PastOperatorEnd: integer;
    begin
      for I := Low(Operators) to High(Operators) do begin
        Offset := Pos(Operators[i], Line);

        // Is this operator present in the line?
        if Offset > 0 then begin

          // Aren't we misinterpreting && for & (for example)?
          PastOperatorEnd := Offset + Length(Operators[i]);
          if (PastOperatorEnd <= Length(Line)) and not (Line[PastOperatorEnd] in OperatorChars) then begin
            Result := Operators[i];
            Exit;
          end;
        end;
      end;
      Offset := 0;
      Result := '';
    end;
  begin
    // Find the first closing brace (this should leave us at the innermost brace pair)
    while true do begin
      Head := Pos(')', Line);
      if Head > 0 then begin
        Tail := Head;
        if SkipBraces(Line, Tail, -1) then begin // find the corresponding opening brace
          ResultLine := EvaluateExpression(Copy(Line, Tail + 1, Head - Tail - 1)); // evaluate this (without braces)
          Delete(Line, Tail, Head - Tail + 1); // Remove the old part AND braces
          Insert(ResultLine, Line, Tail); // and replace by result
        end else begin
          Result := '';
          Exit;
        end;
      end else
        break;
    end;

    // Then evaluate braceless part
    while true do begin
      OperatorToken := GetNextOperator(OperatorPos);
      if OperatorPos > 0 then begin

        // Get left operand
        Tail := OperatorPos - 1;
        while (Tail >= 0) and (Line[Tail] in SpaceChars) do
          Dec(Tail); // skip spaces
        Head := Tail;
        while (Head >= 0) and (Line[Head] in IdentChars) do
          Dec(Head); // step over identifier
        LeftOp := Copy(Line, Head + 1, Tail - Head);
        EquatStart := Head + 1; // marks begin of equation

        // Get right operand
        Tail := OperatorPos + Length(OperatorToken) + 1;
        while (Tail <= Length(Line)) and (Line[Tail] in SpaceChars) do
          Inc(Tail); // skip spaces
        Head := Tail;
        while (Head <= Length(Line)) and (Line[Head] in IdentChars) do
          Inc(Head); // step over identifier
        RightOp := Copy(Line, Tail, Head - Tail);
        EquatEnd := Head; // marks begin of equation

        // Evaluate after removing length suffixes...
        LeftOpValue := StrToIntDef(RemoveSuffixes(LeftOp), 0);
        RightOpValue := StrToIntDef(RemoveSuffixes(RightOp), 0);
        if OperatorToken = '*' then
          ResultValue := LeftOpValue * RightOpValue
        else if OperatorToken = '/' then
          ResultValue := LeftOpValue div RightOpValue // int division
        else if OperatorToken = '+' then
          ResultValue := LeftOpValue + RightOpValue
        else if OperatorToken = '-' then
          ResultValue := LeftOpValue - RightOpValue
        else if OperatorToken = '<' then
          ResultValue := integer(LeftOpValue < RightOpValue)
        else if OperatorToken = '<=' then
          ResultValue := integer(LeftOpValue <= RightOpValue)
        else if OperatorToken = '>' then
          ResultValue := integer(LeftOpValue > RightOpValue)
        else if OperatorToken = '>=' then
          ResultValue := integer(LeftOpValue >= RightOpValue)
        else if OperatorToken = '==' then
          ResultValue := integer(LeftOpValue = RightOpValue)
        else if OperatorToken = '!=' then
          ResultValue := integer(LeftOpValue <> RightOpValue)
        else if OperatorToken = '&' then
          ResultValue := integer(LeftOpValue and RightOpValue)
        else if OperatorToken = '^' then
          ResultValue := integer(LeftOpValue or RightOpValue)
        else if OperatorToken = '|' then
          ResultValue := integer(LeftOpValue xor RightOpValue)
        else if OperatorToken = '&&' then
          ResultValue := integer(LeftOpValue and RightOpValue)
        else if OperatorToken = '||' then
          ResultValue := integer(LeftOpValue or RightOpValue)
        else
          ResultValue := 0;

        // And replace by result in string form
        Delete(Line, EquatStart, EquatEnd - EquatStart);
        Insert(IntToStr(ResultValue), Line, EquatStart);
      end else
        break;
    end;
    Result := Line;
  end;

  function EvaluateIf(Line: AnsiString): boolean;
  begin
    Line := ExpandDefines(Line); // replace FOO by numerical value of FOO
    Line := EvaluateDefines(Line); // replace all defined() by 1 or 0
    Result := StrToIntDef(EvaluateExpression(Line), -1) > 0; // perform the remaining int arithmetic
  end;
begin
  if StartsStr('ifdef', Line) then begin
    if not GetCurrentBranch then // we are already inside an if that is NOT being taken
      SetCurrentBranch(false) // so don't take this one either
    else begin
      Name := TrimLeft(Copy(Line, Length('ifdef') + 1, MaxInt));
      SetCurrentBranch(Assigned(GetDefine(Name)));
    end;
  end else if StartsStr('ifndef', Line) then begin
    if not GetCurrentBranch then // we are already inside an if that is NOT being taken
      SetCurrentBranch(false) // so don't take this one either
    else begin
      Name := TrimLeft(Copy(Line, Length('ifndef') + 1, MaxInt));
      SetCurrentBranch(not Assigned(GetDefine(Name)));
    end;
  end else if StartsStr('if', Line) then begin
    if not GetCurrentBranch then // we are already inside an if that is NOT being taken
      SetCurrentBranch(false) // so don't take this one either
    else begin
      IfLine := TrimLeft(Copy(Line, Length('if') + 1, MaxInt)); // remove if
      SetCurrentBranch(EvaluateIf(IfLine));
    end;
  end else if StartsStr('else', Line) then begin
    // if a branch that is not at our level is false, ignore (that means we're skipping)
    for I := 0 to fBranchResults.Count - 2 do
      if fBranchResults[i] = 0 then // ignore it too
        Exit;

    // otherwise, process it
    OldResult := GetCurrentBranch; // take either if or else
    RemoveCurrentBranch;
    SetCurrentBranch(not OldResult);
  end else if StartsStr('elif', Line) then begin
    // if a branch that is not at our level is false, ignore
    for I := 0 to fBranchResults.Count - 2 do
      if fBranchResults[i] = 0 then // ignore it too
        Exit;

    OldResult := GetCurrentBranch; // take either if or else
    RemoveCurrentBranch;
    if OldResult then begin // don't take this one, previous if has been taken
      SetCurrentBranch(false);
    end else begin // previous ifs failed. try this one
      IfLine := TrimLeft(Copy(Line, Length('elif') + 1, MaxInt)); // remove elif
      SetCurrentBranch(EvaluateIf(IfLine));
    end;
  end else if StartsStr('endif', Line) then
    RemoveCurrentBranch;
end;

procedure TCppPreprocessor.HandleInclude(const Line: AnsiString);
var
  FileName: AnsiString;
begin
  if not GetCurrentBranch then // we're skipping due to a branch failure
    Exit;

  // Get full header file name
  FileName := cbutils.GetHeaderFileName(Includes[fIncludes.Count - 1]^.FileName, Line, fIncludePaths,
    fProjectIncludePaths);

  // And open a new entry
  OpenInclude(FileName);
end;

function TCppPreprocessor.ExpandMacros(const Line: AnsiString): AnsiString; //  Too slow at the moment, so disabling.
begin
  Result := Line;
end;

function TCppPreprocessor.RemoveSuffixes(const Input: AnsiString): AnsiString;
var
  I: integer;
begin
  Result := Input; // remove suffixes like L from integer values
  if Length(Input) > 0 then begin
    if not (Result[1] in ['0'..'9']) then
      Exit; // don't process names
    I := Length(Result);
    while (I >= 0) and (Result[i] in ['A'..'Z', 'a'..'z']) do // find first alphabetical character at end
      Dec(I);
    Delete(Result, I + 1, MaxInt); // remove from there
  end;
end;

function TCppPreprocessor.GetFileIncludesEntry(const FileName: AnsiString): PFileIncludes;
var
  I: integer;
begin
  Result := nil;
  for I := 0 to fIncludesList.Count - 1 do begin
    if SameText(PFileIncludes(fIncludesList[I])^.BaseFile, Filename) then begin
      Result := PFileIncludes(fIncludesList[I]);
      Exit;
    end;
  end;
end;

procedure TCppPreprocessor.PreprocessBuffer;
var
  S: AnsiString;
begin
  while fIncludes.Count > 0 do begin
    repeat
      S := GetNextPreprocessor;
      if StartsStr('#', S) then begin
        Simplify(S);
        if S <> '' then
          HandlePreprocessor(S);
      end;
    until S = '';
    CloseInclude;
  end;
end;

procedure TCppPreprocessor.PreprocessStream(const FileName: AnsiString; Stream: TMemoryStream);
begin
  Reset;
  OpenInclude(FileName, Stream);
  PreprocessBuffer;
end;

procedure TCppPreprocessor.PreprocessFile(const FileName: AnsiString);
begin
  Reset;
  OpenInclude(FileName, nil);
  PreprocessBuffer;
  //fResult.SaveToFile('C:\TCppPreprocessorResult' + ExtractFileName(FileName) + '.txt');
end;

function TCppPreprocessor.GetResult: AnsiString;
begin
  Result := fResult.Text; // sloooow
end;

end.

