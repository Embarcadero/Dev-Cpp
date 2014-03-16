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
  OperatorChars: set of Char = ['+', '-', '*', '/', '!', '=', '<', '>'];
  IdentChars: set of Char = ['A'..'Z', '0'..'9', 'a'..'z', '_', '*', '&', '~'];

type
  PFile = ^TFile;
  TFile = record
    Index : integer; // 0-based for programming convenience
    FileName : AnsiString;
    Buffer : TStringList; // do not concat them all
  end;

  PDefine = ^TDefine;
  TDefine = record
    Name : AnsiString;
    Args : AnsiString;
    Value : AnsiString;
    HardCoded : boolean; // if true, don't free memory (points to hard defines)
  end;

  TCppPreprocessor = class(TComponent)
  private
    fIndex : integer; // points to current file buffer. do not free
    fFileName: AnsiString; // idem
    fBuffer: TStringList; // idem
    fResult: TStringList;
    fPreProcIndex: integer;
    fIncludesRec : PIncludesRec;
    fHardDefines : TList; // set by "cpp -dM -E -xc NUL"
    fDefines : TList; // working set, editable
    fIncludes : TList; // list of files we've stepped into. last one is current file, first one is source file
    fBranchResults : TIntList; // list of branch results (boolean). last one is current branch, first one is outermost branch
    fIncludePaths : TStringList; // *pointer* to buffer of CppParser
    fScannedFiles : TStringList; // idem
    fParseSystem : boolean;
    fParseLocal : boolean;
    procedure PreprocessBuffer;
    procedure SkipToEndOfPreprocessor;
    procedure SkipToPreprocessor;
    function GetNextPreprocessor : AnsiString;
    procedure Simplify(var Output: AnsiString);
    procedure HandlePreprocessor(const Value : AnsiString);
    procedure HandleDefine(const Line : AnsiString);
    procedure HandleUndefine(const Line : AnsiString);
    procedure HandleBranch(const Line : AnsiString);
    procedure HandleInclude(const Line : AnsiString);
    function ExpandMacros(const Line : AnsiString) : AnsiString;
    function RemoveSuffixes(const Input : AnsiString) : AnsiString;
    // line checking stuff
    function FirstLineChar(const Line : AnsiString) : Char;
    function LastLineChar(const Line : AnsiString) : Char;
    // current file stuff
    function GetInclude(index : integer) : PFile;
    procedure OpenInclude(const FileName : AnsiString; Stream : TMemoryStream = nil);
    property Includes[index : integer]: PFile read GetInclude; default;
    procedure CloseInclude;
    // branch stuff
    function GetCurrentBranch : boolean;
    procedure SetCurrentBranch(value : boolean);
    procedure RemoveCurrentBranch;
    function GetResult : AnsiString;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddDefineByParts(const Name, Args, Value : AnsiString; HardCoded : boolean);
    procedure GetDefineParts(const Input : AnsiString; var Name, Args, Value : AnsiString);
    procedure AddDefineByLine(const Line : AnsiString; HardCoded : boolean);
    function GetDefine(const Name : AnsiString) : PDefine;
    procedure Reset;
    procedure ResetDefines;
    procedure SetScanOptions(ParseSystem, ParseLocal : boolean);
    procedure SetIncludePaths(var List : TStringList);
    procedure SetScannedFileList(var List : TStringList);
    procedure SetIncludesRec(var Rec : PIncludesRec);
    procedure PreprocessStream(const FileName: AnsiString; Stream: TMemoryStream);
    procedure PreprocessFile(const FileName: AnsiString);
    property Result : AnsiString read GetResult;
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
	I : integer;
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
	I : integer;
begin
	fResult.Clear;

	// Clear extracted data
	for I := 0 to fIncludes.Count - 1 do begin
		PFile(fIncludes[i])^.Buffer.Free;
		Dispose(PFile(fIncludes[i]));
	end;
	fIncludes.Clear;
	fBranchResults.Clear;
	ResetDefines; // do not throw away hardcoded
end;

function TCppPreprocessor.FirstLineChar(const Line : AnsiString) : Char;
begin
	if Length(Line) > 0 then
		Result := Line[1]
	else
		Result := #0;
end;

function TCppPreprocessor.LastLineChar(const Line : AnsiString) : Char;
begin
	if Length(Line) > 0 then
		Result := Line[Length(Line)]
	else
		Result := #0;
end;

function TCppPreprocessor.GetInclude(index : integer) : PFile;
begin
	result := PFile(fIncludes[index]);
end;

procedure TCppPreprocessor.OpenInclude(const FileName : AnsiString;Stream : TMemoryStream = nil);
var
	Item : PFile;
	IsSystemFile : boolean;
	IncludeLine : AnsiString;
begin
	// Backup old position if we're entering a new file
	if fIncludes.Count > 0 then
		PFile(fIncludes[fIncludes.Count - 1])^.Index := fIndex;

	// Create and add new
	Item := new(PFile);
	Item^.Index := 0; // 0-based line counter
	Item^.FileName := FileName;
	Item^.Buffer := TStringList.Create;

	// Don't parse stuff we have already parsed
	if (fScannedFiles.IndexOf(FileName) = -1) or Assigned(Stream) then begin

		// Parse ONCE
		if not Assigned(Stream) then
			fScannedFiles.Add(FileName);

		// Only load up the file if we are allowed to parse it
		IsSystemFile := cbutils.IsSystemHeaderFile(FileName,fIncludePaths);
		if (fParseSystem and IsSystemFile) or (fParseLocal and not IsSystemFile) then begin
			if Assigned(Stream) then begin
				Stream.Position := 0; // start scanning from here
				Item^.Buffer.LoadFromStream(Stream)
			end else if FileExists(FileName) then
				Item^.Buffer.LoadFromFile(FileName); // load it now
		end;
	end;
	fIncludes.Add(Item);

	// Process it
	fIndex := Item^.Index;
	fFileName := Item^.FileName;
	fBuffer := Item^.Buffer;

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
		PFile(fIncludes[fIncludes.Count - 1])^.Buffer.Free;
		Dispose(PFile(fIncludes[fIncludes.Count - 1]));
		fIncludes.Delete(fIncludes.Count - 1);

		if fIncludes.Count > 0 then begin

			// Continue where we left off
			fIndex := Includes[fIncludes.Count - 1]^.Index;
			fFileName := Includes[fIncludes.Count - 1]^.FileName;
			fBuffer := Includes[fIncludes.Count - 1]^.Buffer; // Point to previous buffer and start past the include we walked into

			// Update result file (we've left the previous file)
			fResult.Add('#include ' + Includes[fIncludes.Count - 1]^.FileName + ':' + IntToStr(Includes[fIncludes.Count - 1]^.Index + 1));
		end;
	end;
end;

function TCppPreprocessor.GetCurrentBranch : boolean;
begin
	if fBranchResults.Count > 0 then
		Result := boolean(fBranchResults[fBranchResults.Count - 1])
	else
		Result := True;
end;

procedure TCppPreprocessor.SetCurrentBranch(value : boolean);
begin
	fBranchResults.Add(integer(value));
end;

procedure TCppPreprocessor.RemoveCurrentBranch;
begin
	if fBranchResults.Count > 0 then
		fBranchResults.Delete(fBranchResults.Count - 1);
end;

procedure TCppPreprocessor.SetScanOptions(ParseSystem, ParseLocal : boolean);
begin
	fParseSystem := ParseSystem;
	fParseLocal := ParseLocal;
end;

procedure TCppPreprocessor.SetIncludesRec(var Rec : PIncludesRec);
begin
	fIncludesRec := Rec;
end;

procedure TCppPreprocessor.SetIncludePaths(var List : TStringList);
begin
	fIncludePaths := List;
end;

procedure TCppPreprocessor.SetScannedFileList(var List : TStringList);
begin
	fScannedFiles := List;
end;

procedure TCppPreprocessor.SkipToPreprocessor;
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
begin
	// Skip until last char of line is NOT \ anymore
	while (fIndex < fBuffer.Count) and (LastLineChar(fBuffer[fIndex]) = '\') do begin
		Inc(fIndex);
	end;
end;

function TCppPreprocessor.GetNextPreprocessor : AnsiString;
var
	I : integer;
	PreProcFrom, PreProcTo : integer;
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
	Output := Copy(Output,2,MaxInt);

	// Remove C-style comments
	while true do begin
		DelimPosFrom := Pos('/*',Output);
		if DelimPosFrom > 0 then begin
			DelimPosTo := PosEx('*/',Output,DelimPosFrom);
			if DelimPosTo > 0 then
				Delete(Output,DelimPosFrom,DelimPosTo - DelimPosFrom + Length('*/'))
			else
				break; // invalid syntax. ignore
		end else
			break;
	end;

	// Don't remove multiple spaces. This can ruin defines which explicity use multiple spaces in their values

	// Remove newlines
	Output := StringReplace(Output, '\'#13, '', [rfReplaceAll]);
	Output := StringReplace(Output, '\'#10, '', [rfReplaceAll]);
	Output := StringReplace(Output, #13, '', [rfReplaceAll]);
	Output := StringReplace(Output, #10, '', [rfReplaceAll]);

	Output := Trim(Output);
end;

function TCppPreprocessor.GetDefine(const Name : AnsiString) : PDefine;
var
	I : integer;
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

procedure TCppPreprocessor.AddDefineByParts(const Name, Args, Value : AnsiString; HardCoded : boolean);
var
	Item : PDefine;
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
procedure TCppPreprocessor.GetDefineParts(const Input : AnsiString; var Name, Args, Value : AnsiString);
var
	I, Level, ArgStart : integer;
	S : AnsiString;
	IsFunction : boolean;
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
				Name := Copy(S,1,I-1);
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
		Args := Copy(S,ArgStart,I-ArgStart);
	end else begin
		Name := Trim(Copy(S,1,I));
		Args := '';
	end;
	Value := TrimLeft(Copy(S,I + 1,MaxInt));
end;

procedure TCppPreprocessor.AddDefineByLine(const Line : AnsiString; HardCoded : boolean);
var
	Name, Args, Value, S : AnsiString;
begin
	// Remove define
	S := TrimLeft(Copy(Line,Length('define') + 1,MaxInt));

	// Get parts from generalized function
	GetDefineParts(S,Name,Args,Value);

	// Add to the list
	AddDefineByParts(Name,Args,Value,HardCoded);
end;

procedure TCppPreprocessor.ResetDefines;
var
	I : integer;
begin
	// Assign hard list to soft list
	for I := 0 to fDefines.Count - 1 do begin // remove memory first
		if not PDefine(fDefines[i])^.HardCoded then // memory belongs to hardcoded list
			Dispose(PDefine(fDefines[i]));
	end;
	fDefines.Clear;
	fDefines.Assign(fHardDefines); // then assign
end;

procedure TCppPreprocessor.HandlePreprocessor(const Value : AnsiString);
begin
	if StartsStr('define',Value) then
		HandleDefine(Value)
	else if StartsStr('undef',Value) then
		HandleUndefine(Value)
	else if StartsStr('if',Value) or StartsStr('else',Value) or StartsStr('elif',Value) or StartsStr('endif',Value) then
		HandleBranch(Value)
	else if StartsStr('include',Value) then
		HandleInclude(Value);
end;

procedure TCppPreprocessor.HandleDefine(const Line : AnsiString);
begin
	AddDefineByLine(Line,false);

	if GetCurrentBranch then
		fResult[fPreProcIndex] := '#' + Line; // add define to result file so the parser can handle it
end;

procedure TCppPreprocessor.HandleUndefine(const Line : AnsiString);
var
	Name : AnsiString;
	I : integer;
begin
	// Remove undef
	Name := TrimLeft(Copy(Line,Length('undef') + 1,MaxInt));

	// Remove from soft list only
	for I := fDefines.Count - 1 downto 0 do begin
		if PDefine(fDefines[i])^.Name = Name then begin
			if not PDefine(fDefines[i])^.HardCoded then // memory does not belong to hardcoded list
				Dispose(PDefine(fDefines[i]));
			fDefines.Delete(i);
			break; // ignore overriden defines 
		end;
	end;
end;

procedure TCppPreprocessor.HandleBranch(const Line : AnsiString);
var
	Name, IfLine : AnsiString;
	OldResult : boolean;
	I : integer;

	function EvaluateDefines(Line : AnsiString) : AnsiString;
	var
		S : AnsiString;
		I, Head, Tail : integer;
		DefineResult, InvertResult : boolean;
	begin
		while true do begin
			I := Pos('defined',Line);
			if I > 0 then begin
				// Check for boolean inverter
				InvertResult := (I > 1) and (Line[I - 1] = '!');

				// Find expression enclosed in () or after space
				Head := I + Length('defined'); // find (
				while (Head <= Length(Line)) and not (Line[Head] in IdentChars) do // skip spaces between defined and ( or identifier
					Inc(Head);
				Tail := Head;
				while (Tail <= Length(Line)) and (Line[Tail] in IdentChars) do // find end of identifier
					Inc(Tail);
				S := Copy(Line,Head,Tail - Head);

				// Delete expression from string
				Head := I;
				if InvertResult then
					Dec(Head);
				Delete(Line,Head,Tail - Head + 1);

				// Evaludate and replace expression by 1 or 0 (true or false)
				DefineResult := Assigned(GetDefine(S));
				if (DefineResult and not InvertResult) or (not DefineResult and InvertResult) then
					Insert('1',Line,Head)
				else
					Insert('0',Line,Head);
			end else
				break;
		end;
		Result := Line;
	end;

	function EvaluateExpressions(Line : AnsiString) : AnsiString;
	var
		Head, Tail, Level, EquatStart, EquatEnd, I : integer;
		LeftOpValue, RightOpValue, ResultValue : Int64;
		LeftOp, RightOp, Operator, ResultLine : AnsiString;

		function GetOperandValue(const Name : AnsiString) : integer;
		var
			Define : PDefine;
		begin
			Result := 0;
			if Length(Name) > 0 then begin
				if (Name[1] in ['0'..'9']) then // it's a number, don't try to find the define
					Result := StrToIntDef(RemoveSuffixes(Name),0)
				else begin // it's a word. try to get a define with the same name
					Define := GetDefine(Name);
					if Assigned(Define) then // this word is a define
						Result := StrToIntDef(RemoveSuffixes(Define^.Value),0)
				end;
			end;
		end;
	begin
		// Find the first closing brace (this should leave us at the innermost brace pair)
		while true do begin
			Head := Pos(')',Line);
			if Head > 0 then begin
				Level := 1;
				Tail := Head;
				while (Tail <= Length(Line)) do begin // Find the corresponding opening brace
					if Line[Tail] = ')' then
						Inc(Level)
					else if Line[Tail] = '(' then begin
						Dec(level);
						if level = 1 then
							break;
					end;
					Dec(Tail);
				end;
				ResultLine := EvaluateExpressions(Copy(Line,Tail + 1,Head - Tail - 1)); // evaluate this
				Delete(Line,Tail,Head - Tail + 1);
				Insert(ResultLine,Line,Tail + 1); // and replace by result
			end else
				break;
		end;

		// Then evaluate braceless part
		while true do begin
			I := Pos('+',Line); // evaluate plus
			if I = 0 then
				I := Pos('-',Line); // and minus
			if I = 0 then
				I := Pos('*',Line); // and multiply
			if I = 0 then
				I := Pos('/',Line); // and divide
			if I = 0 then
				I := Pos('>',Line);
			if I = 0 then
				I := Pos('<',Line);
			if I = 0 then
				I := Pos('==',Line);
			if I = 0 then
				I := Pos('!=',Line);
			if I > 0 then begin
				// Get left operand
				Tail := I - 1;
				while (Tail >= 0) and (Line[Tail] in SpaceChars) do
					Dec(Tail); // skip spaces
				Head := Tail;
				while (Head >= 0) and (Line[Head] in IdentChars) do
					Dec(Head); // step over identifier
				LeftOp := Copy(Line,Head,Tail - Head);
				EquatStart := Head + 1; // marks begin of equation

				// Get operator
				Tail := I;
				while (Tail <= Length(Line)) and (Line[Tail] in OperatorChars) do
					Inc(Tail); // skip operator
				Operator := Copy(Line,I,Tail - I);

				// Get right operand
				while (Tail <= Length(Line)) and (Line[Tail] in SpaceChars) do
					Inc(Tail); // skip spaces
				Head := Tail;
				while (Head <= Length(Line)) and (Line[Head] in IdentChars) do
					Inc(Head); // step over identifier
				RightOp := Copy(Line,Tail,Head - Tail + 1);
				EquatEnd := Head; // marks begin of equation

				// Evaluate after removing length suffixes...
				LeftOpValue := GetOperandValue(LeftOp);
				RightOpValue := GetOperandValue(RightOp);
				if Operator = '+' then
					ResultValue := LeftOpValue + RightOpValue
				else if Operator = '-' then
					ResultValue := LeftOpValue - RightOpValue
				else if Operator = '*' then
					ResultValue := LeftOpValue * RightOpValue
				else if Operator = '/' then
					ResultValue := LeftOpValue div RightOpValue // int division
				else if Operator = '>' then
					ResultValue := integer(LeftOpValue > RightOpValue)
				else if Operator = '<' then
					ResultValue := integer(LeftOpValue < RightOpValue)
				else if Operator = '==' then
					ResultValue := integer(LeftOpValue = RightOpValue)
				else if Operator = '!=' then
					ResultValue := integer(LeftOpValue <> RightOpValue)
				else if Operator = '>=' then
					ResultValue := integer(LeftOpValue >= RightOpValue)
				else if Operator = '<=' then
					ResultValue := integer(LeftOpValue <= RightOpValue)
				else
					ResultValue := 0;

				// And replace by result in string form
				Delete(Line,EquatStart,EquatEnd - EquatStart);
				Insert(IntToStr(ResultValue),Line,EquatStart);
			end else
				break;
		end;
		Result := Line;
	end;

	function EvaluateBooleans(Line : AnsiString) : boolean;
	var
		Head, Tail, Level : integer;
		NextOp : AnsiString;
	begin
		// Find the first opening brace (this should leave us at the outermost brace pair)
		Head := Pos('(',Line);
		if Head > 0 then begin
			Level := 1;
			Tail := Head;
			while (Tail <= Length(Line)) do begin // find corresponding closing brace
				if Line[Tail] = '(' then
					Inc(Level)
				else if Line[Tail] = ')' then begin
					Dec(level);
					if level = 1 then
						break;
				end;
				Inc(Tail);
			end;
			Result := EvaluateBooleans(Copy(Line,Head + 1,Tail - Head - 1)); // evaluate this
			Delete(Line,Head,Tail - Head);

			// Replace inside string
			if Result then
				Insert('1',Line,Head)
			else
				Insert('0',Line,Head);
		end; // no braces remain. evaluate inner part

		// Evaluate inner part
		Result := (Length(Line) > 0) and (Line[1] = '1');
		Tail := 2;
		while Tail <= Length(Line) do begin

			// Skip spaces
			while (Tail <= Length(Line)) and (Line[Tail] in SpaceChars) do
				Inc(Tail);

			// Store operator
			NextOp := Copy(Line,Tail,2);
			Inc(Tail,2);

			// Skip spaces
			while (Tail <= Length(Line)) and (Line[Tail] in SpaceChars) do
				Inc(Tail);// Skip spaces

			// Get operand
			if NextOp = '&&' then
				Result := Result and (Line[Tail] = '1')
			else if NextOp = '||' then
				Result := Result or (Line[Tail] = '1');
			Inc(Tail);
		end;
	end;

	function EvaluateIf(Line : AnsiString) : boolean;
	begin
		Line := EvaluateDefines(Line); // e.g. replace all defined() by 1 or 0
		Line := EvaluateExpressions(Line); // e.g. replace "MONKEY + 1" to value of MONKEY + 1
		Result := EvaluateBooleans(Line); // e.g. replace ((1 && 0) || 1) by true
	end;
begin
	if StartsStr('ifdef',Line) then begin
		if not GetCurrentBranch then // we are already inside an if that is NOT being taken
			SetCurrentBranch(false) // so don't take this one either
		else begin
			Name := TrimLeft(Copy(Line,Length('ifdef') + 1,MaxInt));
			SetCurrentBranch(Assigned(GetDefine(Name)));
		end;
	end else if StartsStr('ifndef',Line) then begin
		if not GetCurrentBranch then // we are already inside an if that is NOT being taken
			SetCurrentBranch(false) // so don't take this one either
		else begin
			Name := TrimLeft(Copy(Line,Length('ifndef') + 1,MaxInt));
			SetCurrentBranch(not Assigned(GetDefine(Name)));
		end;
	end else if StartsStr('if',Line) then begin
		if not GetCurrentBranch then // we are already inside an if that is NOT being taken
			SetCurrentBranch(false) // so don't take this one either
		else begin
			IfLine := TrimLeft(Copy(Line,Length('if')+1,MaxInt)); // remove if
			SetCurrentBranch(EvaluateIf(IfLine));
		end;
	end else if StartsStr('else',Line) then begin
		// if a branch that is not at our level is false, ignore (that means we're skipping)
		for I := 0 to fBranchResults.Count - 2 do
			if fBranchResults[i] = 0 then // ignore it too
				Exit;

		// otherwise, process it
		OldResult := GetCurrentBranch; // take either if or else
		RemoveCurrentBranch;
		SetCurrentBranch(not OldResult);
	end else if StartsStr('elif',Line) then begin
		// if a branch that is not at our level is false, ignore
		for I := 0 to fBranchResults.Count - 2 do
			if fBranchResults[i] = 0 then // ignore it too
				Exit;

		OldResult := GetCurrentBranch; // take either if or else
		RemoveCurrentBranch;
		if OldResult then begin // don't take this one, previous if has been taken
			SetCurrentBranch(false);
		end else begin // previous ifs failed. try this one
			IfLine := TrimLeft(Copy(Line,Length('elif')+1,MaxInt)); // remove elif
			SetCurrentBranch(EvaluateIf(IfLine));
		end;
	end else if StartsStr('endif',Line) then
		RemoveCurrentBranch;
end;

procedure TCppPreprocessor.HandleInclude(const Line : AnsiString);
var
	FileName : AnsiString;
begin
	if not GetCurrentBranch then // we're skipping due to a branch failure
		Exit;

	// Get full header file name
	FileName := cbutils.GetHeaderFileName(Includes[fIncludes.Count - 1]^.FileName,Line,fIncludePaths);

	// Add the new file to the includes of the current file
	with fIncludesRec^ do
		IncludeFiles := IncludeFiles + AnsiQuotedStr(FileName, '"') + ',';

	OpenInclude(FileName);
end;

function TCppPreprocessor.ExpandMacros(const Line : AnsiString) : AnsiString; //  Too slow at the moment, so disabling.
begin
	Result := Line;
end;
{var
	TokenBegin, TokenEnd, I : integer;
	Token : AnsiString;
	// Perform macro expansion.
	{if Expand and (TrimLeft(Value) <> '') then begin
		TokenBegin := 1;
		TokenEnd := 1;
		repeat
			// Add whitespace to result too
			while (TokenEnd <= Length(Value)) and not (Value[TokenEnd] in IdentChars) do
				Inc(TokenEnd);
			fResult := fResult + Copy(Value,TokenBegin,TokenEnd-TokenBegin);
			TokenBegin := TokenEnd;

			// Get next token separated by whitespace
			while (TokenEnd <= Length(Value)) and (Value[TokenEnd] in IdentChars) do
				Inc(TokenEnd);
			Token := Copy(Value,TokenBegin,TokenEnd - TokenBegin);
			TokenBegin := TokenEnd;

			// Perform expansion on token
			if Length(Token) > 0 then
				for I := 0 to fDefines.Count - 1 do begin
					if Token = PDefine(fDefines[I])^.Name then begin
						Token := PDefine(fDefines[I])^.Name;
						break;
					end;
				end;
			fResult := fResult + Token;
		until TokenEnd > Length(Value);
	end else
		fResult := fResult + Value;}

function TCppPreprocessor.RemoveSuffixes(const Input : AnsiString) : AnsiString;
var
	I : integer;
begin
	Result := Input; // remove suffixes like L from integer values
	if Length(Input) > 0 then begin
		if not (Result[1] in ['0'..'9']) then
			Exit; // don't process names
		I := Length(Result);
		while (I >= 0) and (Result[i] in ['A'..'Z','a'..'z']) do // find first alphabetical character at end
			Dec(I);
		Delete(Result,I + 1,MaxInt); // remove from there
	end;
end;

procedure TCppPreprocessor.PreprocessBuffer;
var
	S : AnsiString;
begin
	while fIncludes.Count > 0 do begin
		repeat
			S := GetNextPreprocessor;
			if StartsStr('#',S) then begin
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
	OpenInclude(FileName,Stream);
	PreprocessBuffer;
end;

procedure TCppPreprocessor.PreprocessFile(const FileName: AnsiString);
begin
	Reset;
	OpenInclude(FileName,nil);
	PreprocessBuffer;
end;

function TCppPreprocessor.GetResult : AnsiString;
begin
	Result := fResult.Text; // sloooow
end;

end.

