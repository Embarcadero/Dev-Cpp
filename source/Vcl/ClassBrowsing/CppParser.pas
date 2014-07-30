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

unit CppParser;

interface

uses
{$IFDEF WIN32}
  Dialogs, Windows, Classes, SysUtils, StrUtils, ComCtrls, IntList, CppTokenizer, CppPreprocessor, cbutils;
{$ENDIF}
{$IFDEF LINUX}
QDialogs, Classes, SysUtils, StrUtils, QComCtrls, U_IntList, CppTokenizer;
{$ENDIF}

type
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
    _ID: integer;
    _ParentID: integer;
    _FullText: AnsiString;
    _Type: AnsiString;
    _Args: AnsiString;
    _ScopelessCmd: AnsiString;
    _ScopeCmd: AnsiString;
    _Kind: TStatementKind;
    _InheritsFromIDs: AnsiString; // list of inheriting IDs, in comma-separated AnsiString form
    _InheritsFromClasses: AnsiString; // list of inheriting class names, in comma-separated AnsiString form
    _Scope: TStatementScope;
    _ClassScope: TStatementClassScope;
    _IsDeclaration: boolean;
    _DeclImplLine: integer;
    _Line: integer;
    _DeclImplFileName: AnsiString;
    _FileName: AnsiString;
    _Visible: boolean;
    _Temporary: boolean;
    _Loaded: boolean;
    _InProject: boolean;
    _InSystemHeader: boolean;
  end;

  POutstandingTypedef = ^TOutstandingTypedef;
  TOutstandingTypedef = record
    _WaitForTypedef: AnsiString;
    _ExistingID: integer;
  end;

  TCppParser = class(TComponent)
  private
    fEnabled: boolean;
    fNextID: integer;
    fScannedBaseIndex: integer; // keep count of cache files
    fBaseIndex: integer;
    fLevel: integer;
    fIndex: integer;
    fIsHeader: boolean;
    fIsSystemHeader: boolean;
    fCurrentFile: AnsiString;
    fLastID: integer;
    fCurrentClass: TIntList;
    fSkipList: TIntList;
    fCurrentClassLevel: TIntList;
    fClassScope: TStatementClassScope;
    fStatementList: TList;
    fOutstandingTypedefs: TList;
    fIncludesList: TList;
    fTokenizer: TCppTokenizer;
    fPreprocessor: TCppPreprocessor;
    fIncludePaths: TStringList;
    fProjectIncludePaths: TStringList;
    fProjectFiles: TStringList;
    fFilesToScan: TStringList;
    fScannedFiles: TStringList;
    fFileIncludes: TStringList;
    fCacheContents: TStringList;
    fParseLocalHeaders: boolean;
    fParseGlobalHeaders: boolean;
    fProjectDir: AnsiString;
    fOnBusy: TNotifyEvent;
    fOnUpdate: TNotifyEvent;
    fOnTotalProgress: TProgressEvent;
    fLaterScanning: boolean;
    fOnStartParsing: TNotifyEvent;
    fOnEndParsing: TNotifyEvent;
    fIsProjectFile: boolean;
    fInvalidatedIDs: TIntList;
    function AddStatement(
      ID: integer;
      ParentID: integer;
      const FileName: AnsiString;
      const FullText: AnsiString;
      const _Type: AnsiString; // "Type" is already in use
      const ScopeCmd: AnsiString;
      const Args: AnsiString;
      Line: integer;
      Kind: TStatementKind;
      Scope: TStatementScope;
      ClassScope: TStatementClassScope;
      Visible: boolean;
      AllowDuplicate: boolean;
      IsDeclaration: boolean): integer;
    function IsSystemHeaderFile(const FileName: AnsiString): boolean;
    function GetCurrentClass: integer;
    procedure SetInheritance(Index: integer);
    procedure SetCurrentClass(ID: integer);
    procedure RemoveCurrentClass;
    procedure CheckForSkipStatement;
    function SkipBraces(StartAt: integer): integer;
    function CheckForPreprocessor: boolean;
    function CheckForKeyword: boolean;
    function CheckForMember: boolean;
    function CheckForTypedef: boolean;
    function CheckForTypedefEnum: boolean;
    function CheckForTypedefStruct: boolean;
    function CheckForStructs: boolean;
    function CheckForMethod(var sType, sName, sArgs: AnsiString): boolean;
    function CheckForScope: boolean;
    function CheckForVar: boolean;
    function CheckForEnum: boolean;
    function GetScope: TStatementScope;
    procedure HandlePreprocessor;
    procedure HandleMember;
    procedure HandleOtherTypedefs;
    procedure HandleStructs(IsTypedef: boolean = False);
    procedure HandleMethod(const sType, sName, sArgs: AnsiString);
    procedure ScanMethodArgs(const ArgStr: AnsiString; const Filename: AnsiString; Line, ClassID: integer);
    procedure HandleScope;
    procedure HandleKeyword;
    procedure HandleVar;
    procedure HandleEnum;
    function HandleStatement: boolean;
    procedure Parse(const FileName: AnsiString; ManualUpdate: boolean = False; processInh: boolean = True; Stream:
      TMemoryStream = nil);
    procedure DeleteTemporaries;
    function FindFileIncludes(const Filename: AnsiString; DeleteIt: boolean = False): PFileIncludes;
  public
    procedure ResetDefines;
    procedure AddHardDefineByParts(const Name, Args, Value: AnsiString);
    procedure AddHardDefineByLine(const Line: AnsiString);
    procedure InvalidateFile(const FileName: AnsiString);
    procedure GetFileIncludes(const Filename: AnsiString; var List: TStringList);
    function IsCfile(const Filename: AnsiString): boolean;
    function IsHfile(const Filename: AnsiString): boolean;
    procedure GetSourcePair(const FName: AnsiString; var CFile, HFile: AnsiString);
    function GetImplementationLine(Statement: PStatement): integer;
    function GetImplementationFileName(Statement: PStatement): AnsiString;
    function GetDeclarationLine(Statement: PStatement): integer;
    function GetDeclarationFileName(Statement: PStatement): AnsiString;
    procedure GetClassesList(var List: TStrings);
    function SuggestMemberInsertionLine(ParentID: integer; Scope: TStatementClassScope; var AddScopeStr: boolean):
      integer;
    function GetSystemHeaderFileName(const FileName: AnsiString): AnsiString; // <file.h>
    function GetLocalHeaderFileName(const RelativeTo, FileName: AnsiString): AnsiString; // "file.h"
    function GetHeaderFileName(const RelativeTo, Line: AnsiString): AnsiString; // both
    function IsIncludeLine(const Line: AnsiString): boolean;
    procedure Load(const FileName: AnsiString; const relativeto: AnsiString);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ParseList;
    procedure ReParseFile(const FileName: AnsiString; InProject: boolean; OnlyIfNotParsed: boolean = False; UpdateView:
      boolean = True; Stream: TMemoryStream = nil);
    function StatementKindStr(Value: TStatementKind): AnsiString;
    function StatementClassScopeStr(Value: TStatementClassScope): AnsiString;
    function CheckIfCommandExists(const ScopelessCmd: AnsiString; Kind: TStatementKind; ParentID: integer): integer;
    procedure Reset(KeepLoaded: boolean = True);
    procedure ClearIncludePaths;
    procedure ClearProjectIncludePaths;
    procedure AddIncludePath(const Value: AnsiString);
    procedure AddProjectIncludePath(const Value: AnsiString);
    procedure AddFileToScan(Value: AnsiString; InProject: boolean = False);
    procedure Save(const FileName: AnsiString; const relativeto: AnsiString);
    procedure PostProcessInheritance;
    procedure ReProcessInheritance;
    function IndexOfStatement(ID: integer): integer;
    function Locate(const Full: AnsiString; WithScope: boolean): PStatement;
    function PrettyPrintStatement(st: PStatement): AnsiString;
    procedure FillListOfFunctions(const Full: AnsiString; List: TStringList);
    function FindAndScanBlockAt(const Filename: AnsiString; Row: integer; Stream: TMemoryStream): PStatement;
    function FindStatementOf(FileName, Phrase: AnsiString; Row: integer; Stream: TMemoryStream): PStatement; overload;
    function FindStatementOf(Phrase: AnsiString; CurrentClass: PStatement): PStatement; overload;
    function FindVariableOf(const Phrase: AnsiString; CurrentClass: PStatement): PStatement;
    function FindTypeStatementOf(const aType: AnsiString; CurrentClass: PStatement; MaxSearchIndex: integer = -1):
      PStatement;
    function GetClass(const Phrase: AnsiString): AnsiString;
    function GetMember(const Phrase: AnsiString): AnsiString;
    function GetOperator(const Phrase: AnsiString): AnsiString;
    function FindLastOperator(const Phrase: AnsiString): integer;
    procedure GetInheritanceIDs(st: PStatement; List: TIntList); overload;
    procedure GetInheritanceIDs(index: Integer; List: TIntList); overload;
  published
    property BaseIndex: integer read fBaseIndex write fBaseIndex;
    property Enabled: boolean read fEnabled write fEnabled;
    property OnUpdate: TNotifyEvent read fOnUpdate write fOnUpdate;
    property OnBusy: TNotifyEvent read fOnBusy write fOnBusy;
    property OnTotalProgress: TProgressEvent read fOnTotalProgress write fOnTotalProgress;
    property Tokenizer: TCppTokenizer read fTokenizer write fTokenizer;
    property Preprocessor: TCppPreprocessor read fPreprocessor write fPreprocessor;
    property Statements: TList read fStatementList write fStatementList;
    property ParseLocalHeaders: boolean read fParseLocalHeaders write fParseLocalHeaders;
    property ParseGlobalHeaders: boolean read fParseGlobalHeaders write fParseGlobalHeaders;
    property ScannedFiles: TStringList read fScannedFiles;
    property CacheContents: TStringList read fCacheContents;
    property ProjectDir: AnsiString read fProjectDir write fProjectDir;
    property OnStartParsing: TNotifyEvent read fOnStartParsing write fOnStartParsing;
    property OnEndParsing: TNotifyEvent read fOnEndParsing write fOnEndParsing;
    property FilesToScan: TStringList read fFilesToScan;
  end;

procedure Register;

implementation

uses
  DateUtils;

procedure Register;
begin
  RegisterComponents('Dev-C++', [TCppParser]);
end;

constructor TCppParser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fStatementList := TList.Create;
  fOutstandingTypedefs := TList.Create;
  fIncludesList := TList.Create;
  fFilesToScan := TStringList.Create;
  fScannedFiles := TStringList.Create;
  fIncludePaths := TStringList.Create;
  fProjectIncludePaths := TStringList.Create;
  fFileIncludes := TStringList.Create;
  fCacheContents := TStringList.Create;
  fProjectFiles := TStringList.Create;
  fInvalidatedIDs := TIntList.Create;
  fCurrentClass := TIntList.Create; // create once, then reuse
  fCurrentClassLevel := TIntList.Create;
  fSkipList := TIntList.Create;
  fParseLocalHeaders := False;
  fParseGlobalHeaders := False;
end;

destructor TCppParser.Destroy;
var
  i: Integer;
begin
  FreeAndNil(fInvalidatedIDs);
  FreeAndNil(fCurrentClass);
  FreeAndNil(fCurrentClassLevel);
  FreeAndNil(fSkipList);
  FreeAndNil(fProjectFiles);

  for i := 0 to fIncludesList.Count - 1 do
    Dispose(PFileIncludes(fIncludesList.Items[i]));
  FreeAndNil(fIncludesList);

  for i := 0 to fOutstandingTypedefs.Count - 1 do
    Dispose(POutstandingTypedef(fOutstandingTypedefs.Items[i]));
  FreeAndNil(fOutstandingTypedefs);

  for i := 0 to fStatementList.Count - 1 do
    Dispose(PStatement(fStatementList.Items[i]));
  FreeAndNil(fStatementList);

  FreeAndNil(fFilesToScan);
  FreeAndNil(fCacheContents);
  FreeAndNil(fScannedFiles);
  FreeAndNil(fIncludePaths);
  FreeAndNil(fProjectIncludePaths);
  FreeAndNil(fFileIncludes);

  inherited Destroy;
end;

function TCppParser.StatementClassScopeStr(Value: TStatementClassScope): AnsiString;
begin
  case Value of
    scsPublic: Result := 'public';
    scsPrivate: Result := 'private';
    scsProtected: Result := 'protected';
    scsNone: Result := '';
  end;
end;

function TCppParser.StatementKindStr(Value: TStatementKind): AnsiString;
begin
  case Value of
    skPreprocessor: Result := 'preprocessor';
    skVariable: Result := 'variable';
    skConstructor: Result := 'constructor';
    skDestructor: Result := 'destructor';
    skFunction: Result := 'function';
    skClass: Result := 'class';
    skTypedef: Result := 'typedef';
    skEnum: Result := 'enum';
    skUnknown: Result := 'unknown';
  end;
end;

function TCppParser.SkipBraces(StartAt: integer): integer;
var
  I, Level: integer;
begin
  I := StartAt;
  Level := 0; // assume we start on top of {
  while (I < fTokenizer.Tokens.Count) do begin
    case fTokenizer[I]^.Text[1] of
      '{': Inc(Level);
      '}': begin
          Dec(Level);
          if Level = 0 then begin
            Result := I;
            Exit;
          end;
        end;
    end;
    Inc(I);
  end;
  Result := StartAt;
end;

// This function takes up about 96% of our parsing time. That should change

function TCppParser.CheckIfCommandExists(const ScopelessCmd: AnsiString; Kind: TStatementKind; ParentID: integer):
  integer;
var
  I: integer;
  SearchKinds: set of TStatementKind;
  HeaderFileName, SourceFileName: AnsiString;
begin
  Result := -1;

  // When searching for functions, check constructors and destructors too
  if Kind in [skFunction, skConstructor, skDestructor] then
    SearchKinds := [skFunction, skConstructor, skDestructor]
  else
    SearchKinds := [Kind];

  // It must be present in these files
  GetSourcePair(fCurrentFile, SourceFileName, HeaderFileName);

  // we do a backward search, because most possible is to be found near the end ;) - if it exists :(
  for I := fStatementList.Count - 1 downto 0 do begin

    // Only do an expensive string compare with the right kinds and parentIDs
    if PStatement(fStatementList[I])^._Kind in SearchKinds then begin
      if PStatement(fStatementList[I])^._ParentID = ParentID then begin
        if PStatement(fStatementList[I])^._ScopelessCmd = ScopelessCmd then begin
          if ((CompareText(PStatement(fStatementList[I])^._FileName, SourceFileName) = 0) or
            // only if it belongs to the same file-pair
            (CompareText(PStatement(fStatementList[I])^._FileName, HeaderFileName) = 0)) then begin
            Result := I;
            Break;
          end;
        end;
      end;
    end;
  end;
end;

function TCppParser.AddStatement(
  ID: integer;
  ParentID: integer;
  const FileName: AnsiString;
  const FullText: AnsiString;
  const _Type: AnsiString; // "Type" is already in use
  const ScopeCmd: AnsiString;
  const Args: AnsiString;
  Line: integer;
  Kind: TStatementKind;
  Scope: TStatementScope;
  ClassScope: TStatementClassScope;
  Visible: boolean;
  AllowDuplicate: boolean;
  IsDeclaration: boolean): integer;
var
  Statement, ExistingStatement: PStatement;
  ExistingIndex, OperatorPos: integer;
  NewKind: TStatementKind;
  NewType, NewScopeCmd, NewScopelessCmd: AnsiString;

  function AddToList: integer;
  begin
    Statement := New(PStatement);
    with Statement^ do begin

      if ID = -1 then
        _ID := fNextID
      else
        _ID := ID;
      Result := _ID;

      _ParentID := ParentID;
      _FileName := FileName;
      _FullText := FullText;
      _ScopelessCmd := NewScopelessCmd;
      _ScopeCmd := NewScopeCmd;
      _Type := NewType;
      _Args := Args;
      _Line := Line;
      _Kind := Kind;
      _Scope := Scope;
      _ClassScope := ClassScope;
      _IsDeclaration := IsDeclaration;
      _DeclImplLine := Line;
      _DeclImplFileName := FileName;
      _Visible := Visible; // sets visibility in class browser
      _Loaded := False; // true if it's a cache file
      _Temporary := fLaterScanning; // true if it's added by a function body scan
      _InProject := fIsProjectFile;
      _InSystemHeader := fIsSystemHeader;
    end;
    fStatementList.Add(Statement);
    Inc(fNextID);
  end;
begin
  // Move '*', '&' to type rather than cmd (it's in the way for code-completion)
  NewType := _Type;
  NewScopeCmd := ScopeCmd;
  while (Length(NewScopeCmd) > 0) and (NewScopeCmd[1] in ['*', '&']) do begin
    NewType := NewType + NewScopeCmd[1];
    Delete(NewScopeCmd, 1, 1); // remove first
  end;

  NewKind := Kind;

  // Strip class prefix (e.g. MyClass::SomeFunc() = SomeFunc() )
  if Kind = skFunction then begin
    OperatorPos := Pos('::', NewScopeCmd);
    if OperatorPos > 0 then begin
      NewScopelessCmd := Copy(NewScopeCmd, OperatorPos + 2, MaxInt);
      if SameStr(Copy(NewScopeCmd, 1, OperatorPos - 1), NewScopelessCmd) then
        NewKind := skConstructor
      else if SameStr('~' + Copy(NewScopeCmd, 1, OperatorPos - 1), NewScopelessCmd) then
        NewKind := skDestructor;
    end else
      NewScopelessCmd := NewScopeCmd;
  end else
    NewScopelessCmd := NewScopeCmd;

  // Remove namespace stuff from type (until we support namespaces)
  if NewKind in [skFunction, skVariable] then begin
    OperatorPos := Pos('::', NewType);
    if OperatorPos > 0 then
      Delete(NewType, 1, OperatorPos + 1);
  end;

  // Only search for certain kinds of statements
  ExistingStatement := nil;
  if not AllowDuplicate then begin
    ExistingIndex := CheckIfCommandExists(NewScopelessCmd, Kind, ParentID);
    if ExistingIndex <> -1 then
      ExistingStatement := PStatement(fStatementList[ExistingIndex]);
  end;

  // We already have a statement with the same identifier...
  if Assigned(ExistingStatement) then begin

    // Functions receive special treatment...
    if (NewKind in [skFunction, skConstructor, skDestructor]) then begin

      // This could be a function definition/declaration pair
      if (IsDeclaration <> ExistingStatement^._IsDeclaration) and (ExistingStatement^._Args = Args) then begin
        ExistingStatement^._DeclImplLine := Line;
        ExistingStatement^._DeclImplFileName := FileName;
        if (NewKind in [skConstructor, skDestructor]) and (ExistingStatement^._Kind = skFunction) then
          ExistingStatement^._Kind := NewKind
        else if (NewKind = skFunction) and (Pos('::', ScopeCmd) > 0) then
          ExistingStatement^._ScopeCmd := ScopeCmd;
        Result := ExistingStatement^._ID;

        // Otherwise, assume overloading. Allow that.
      end else
        Result := AddToList;

      // Other duplicate statements are to be ignored completely
    end else begin
      Result := ExistingStatement^._ID;
    end;

    // No duplicates found. Proceed as usual
  end else
    Result := AddToList;
end;

function TCppParser.GetCurrentClass: integer;
begin
  if fCurrentClass.Count > 0 then
    Result := fCurrentClass[fCurrentClass.Count - 1]
  else
    Result := -1;
end;

procedure TCppParser.SetCurrentClass(ID: integer);
var
  ClassIndex: integer;
begin
  // Don't add duplicate scopes. Does this ever happen though?
  if (fCurrentClass.Count > 0) and (fCurrentClass[fCurrentClass.Count - 1] = ID) then
    Exit;

  // Add class to list. If ID is -1, this is an extern {} 'class' or namespace
  fCurrentClass.Add(ID);
  fCurrentClassLevel.Add(fLevel);

  // Check bounds before inserting...
  ClassIndex := IndexOfStatement(ID);

  // extern {}, namespace or class that doesn't exist
  if (ClassIndex = -1) then begin
    fClassScope := scsNone
  end else if PStatement(fStatementList[ClassIndex])^._Type = 'class' then
    fClassScope := scsPrivate // classes are private by default
  else
    fClassScope := scsPublic;
end;

procedure TCppParser.RemoveCurrentClass;
var
  UseIndex: integer;
begin
  if fCurrentClassLevel.Count > 0 then
    if fCurrentClassLevel[fCurrentClassLevel.Count - 1] = fLevel then begin
      fCurrentClass.Delete(fCurrentClass.Count - 1);
      fCurrentClassLevel.Delete(fCurrentClassLevel.Count - 1);
      if fCurrentClassLevel.Count = 0 then
        fClassScope := scsNone // no classes or structs remaining
      else begin

        // Check top level class
        UseIndex := IndexOfStatement(fCurrentClass[fCurrentClass.Count - 1]);
        if (UseIndex <> -1) and (PStatement(fStatementList[UseIndex])^._Type = 'class') then
          fClassScope := scsPrivate // classes are private by default
        else
          fClassScope := scsPublic;
      end;
    end;
end;

procedure TCppParser.SetInheritance(Index: integer);
  function CheckForScopeDecl(Index: integer): boolean;
  begin
    Result := (Index < fTokenizer.Tokens.Count - 1) and
      (SameStr(fTokenizer[Index]^.Text, 'public') or
      SameStr(fTokenizer[Index]^.Text, 'protected') or
      SameStr(fTokenizer[Index]^.Text, 'private'));
  end;
var
  sl: TStrings;
begin
  sl := TStringList.Create;
  try
    // at this point we are at ':' point in class declaration
    // we have to find the class referenced and return its ID...
    repeat
      if not CheckForScopeDecl(Index) then
        if not (fTokenizer[Index]^.Text[1] in [',', ':', '(']) then
          sl.Add(fTokenizer[Index]^.Text);
      Inc(Index);
    until (Index >= fTokenizer.Tokens.Count) or (fTokenizer[Index]^.Text[1] in ['{', ';']);
  finally
    PStatement(fStatementList[fStatementList.Count - 1])^._InheritsFromClasses := sl.CommaText;
    sl.Free;
  end;
end;

function TCppParser.GetScope: TStatementScope;
var
  CurrClass: integer;
begin
  // Function bodies...
  if fLaterScanning then begin
    Result := ssLocal;
    Exit;
  end;

  // Don't blindly trust levels. Namespaces and externs can have levels too
  CurrClass := GetCurrentClass;

  // Invalid class or namespace/extern
  if (CurrClass = -1) or (fLevel = 0) then
    Result := ssGlobal

    // We are inside a class body
  else if CurrClass <> -1 then
    Result := ssClassLocal

    // Everything else
  else
    Result := ssLocal;
end;

procedure TCppParser.CheckForSkipStatement;
var
  iSkip: integer;
begin
  iSkip := fSkipList.IndexOf(fIndex);
  if iSkip >= 0 then begin // skip to next ';'
    repeat
      Inc(fIndex);
    until (fIndex >= fTokenizer.Tokens.Count) or (fTokenizer[fIndex]^.Text[1] in [';']);
    Inc(fIndex); //skip ';'
    fSkipList.Delete(iSkip);
  end;
end;

function TCppParser.CheckForPreprocessor: boolean;
begin
  result := StartsStr('#', fTokenizer[fIndex]^.Text);
end;

// a few percent speed can be gained by storing the prefered action here and directly executing that in HandleKeyword
// however, CheckIfCommandExists is responsible for 95% of our slowness. fix that first.

function TCppParser.CheckForKeyword: boolean;
begin
  Result :=
    SameStr(fTokenizer[fIndex]^.Text, 'alignas') or // skip to )
  SameStr(fTokenizer[fIndex]^.Text, 'alignof') or // skip to )
  SameStr(fTokenizer[fIndex]^.Text, 'and') or // skip
  SameStr(fTokenizer[fIndex]^.Text, 'and_eq') or // skip
  SameStr(fTokenizer[fIndex]^.Text, 'asm') or // skip to }
  // auto is a type
  SameStr(fTokenizer[fIndex]^.Text, 'bitand') or // skip
  SameStr(fTokenizer[fIndex]^.Text, 'bitor') or // skip
  // bool is a type
  SameStr(fTokenizer[fIndex]^.Text, 'break') or // skip
  SameStr(fTokenizer[fIndex]^.Text, 'case') or // skip to :
  SameStr(fTokenizer[fIndex]^.Text, 'catch') or // skip to {
  // char is a type
  // char16_t is a type
  // char32_t is a type
  // class is handled elsewhere
  SameStr(fTokenizer[fIndex]^.Text, 'compl') or // skip
  SameStr(fTokenizer[fIndex]^.Text, 'const') or // skip
  SameStr(fTokenizer[fIndex]^.Text, 'constexpr') or // skip
  SameStr(fTokenizer[fIndex]^.Text, 'const_cast') or // skip
  SameStr(fTokenizer[fIndex]^.Text, 'continue') or // skip
  SameStr(fTokenizer[fIndex]^.Text, 'decltype') or // skip to )
  SameStr(fTokenizer[fIndex]^.Text, 'default') or // skip to :
  SameStr(fTokenizer[fIndex]^.Text, 'delete') or // skip to ;
  SameStr(fTokenizer[fIndex]^.Text, 'do') or // skip to {
  // double is a type
  SameStr(fTokenizer[fIndex]^.Text, 'dynamic_cast') or // skip
  SameStr(fTokenizer[fIndex]^.Text, 'else') or // skip
  // enum is handled elsewhere
  SameStr(fTokenizer[fIndex]^.Text, 'explicit') or // skip
  SameStr(fTokenizer[fIndex]^.Text, 'export') or // skip
  SameStr(fTokenizer[fIndex]^.Text, 'extern') or // skip
  SameStr(fTokenizer[fIndex]^.Text, 'false') or // skip
  // float is a type
  SameStr(fTokenizer[fIndex]^.Text, 'for') or // skip to ), ( when scanning code blocks
  SameStr(fTokenizer[fIndex]^.Text, 'friend') or // skip
  SameStr(fTokenizer[fIndex]^.Text, 'goto') or // skip to ;
  SameStr(fTokenizer[fIndex]^.Text, 'if') or // skip to )
  SameStr(fTokenizer[fIndex]^.Text, 'inline') or // skip
  // int is a type
  // long is a type
  SameStr(fTokenizer[fIndex]^.Text, 'mutable') or // skip
  SameStr(fTokenizer[fIndex]^.Text, 'namespace') or // skip to {
  SameStr(fTokenizer[fIndex]^.Text, 'new') or // skip to ;
  SameStr(fTokenizer[fIndex]^.Text, 'noexcept') or // skip
  SameStr(fTokenizer[fIndex]^.Text, 'not') or // skip
  SameStr(fTokenizer[fIndex]^.Text, 'not_eq') or // skip
  SameStr(fTokenizer[fIndex]^.Text, 'nullptr') or // skip
  // operator is handled elsewhere
  SameStr(fTokenizer[fIndex]^.Text, 'or') or // skip
  SameStr(fTokenizer[fIndex]^.Text, 'or_eq') or // skip
  // private is handled elsewhere
  // protected is handled elsewhere
  // public is handled elsewhere
  SameStr(fTokenizer[fIndex]^.Text, 'register') or // skip
  SameStr(fTokenizer[fIndex]^.Text, 'reinterpret_cast') or // skip
  SameStr(fTokenizer[fIndex]^.Text, 'return') or // skip to ;
  // short is a type
  // signed is a type
  SameStr(fTokenizer[fIndex]^.Text, 'sizeof') or // skip to )
  SameStr(fTokenizer[fIndex]^.Text, 'static') or // skip
  SameStr(fTokenizer[fIndex]^.Text, 'static_assert') or // skip
  SameStr(fTokenizer[fIndex]^.Text, 'static_cast') or // skip
  // struct is handled elsewhere
  SameStr(fTokenizer[fIndex]^.Text, 'switch') or // skip to )
  SameStr(fTokenizer[fIndex]^.Text, 'template') or // skip
  SameStr(fTokenizer[fIndex]^.Text, 'this') or // skip
  SameStr(fTokenizer[fIndex]^.Text, 'thread_local') or // skip
  SameStr(fTokenizer[fIndex]^.Text, 'throw') or // skip to ;
  SameStr(fTokenizer[fIndex]^.Text, 'true') or // skip
  SameStr(fTokenizer[fIndex]^.Text, 'try') or //skip to {
  // typedef is handled elsewhere
  SameStr(fTokenizer[fIndex]^.Text, 'typeid') or //skip to )
  SameStr(fTokenizer[fIndex]^.Text, 'typename') or //skip
  // union is handled elsewhere
  // unsigned is a type
  SameStr(fTokenizer[fIndex]^.Text, 'using') or // skip to ;
  SameStr(fTokenizer[fIndex]^.Text, 'virtual') or // skip
  // void is a type
  SameStr(fTokenizer[fIndex]^.Text, 'volatile') or // skip
  // wchar_t is a type
  SameStr(fTokenizer[fIndex]^.Text, 'while') or // skip to )
  SameStr(fTokenizer[fIndex]^.Text, 'xor') or // skip
  SameStr(fTokenizer[fIndex]^.Text, 'xor_eq'); // skip
end;

function TCppParser.CheckForMember: boolean;
begin
  Result := SameStr(fTokenizer[fIndex]^.Text[Length(fTokenizer[fIndex]^.Text)], '.');
end;

function TCppParser.CheckForTypedef: boolean;
begin
  Result := SameStr(fTokenizer[fIndex]^.Text, 'typedef');
end;

function TCppParser.CheckForEnum: boolean;
begin
  Result := SameStr(fTokenizer[fIndex]^.Text, 'enum');
end;

function TCppParser.CheckForTypedefEnum: boolean;
begin
  //we assume that typedef is the current index, so we check the next
  //should call CheckForTypedef first!!!
  Result := (fIndex < fTokenizer.Tokens.Count - 1) and
    SameStr(fTokenizer[fIndex + 1]^.Text, 'enum');
end;

function TCppParser.CheckForTypedefStruct: boolean;
begin
  //we assume that typedef is the current index, so we check the next
  //should call CheckForTypedef first!!!
  Result := (fIndex < fTokenizer.Tokens.Count - 1) and
    SameStr(fTokenizer[fIndex + 1]^.Text, 'struct') or
    SameStr(fTokenizer[fIndex + 1]^.Text, 'class') or
    SameStr(fTokenizer[fIndex + 1]^.Text, 'union');
end;

function TCppParser.CheckForStructs: boolean;
var
  I: integer;
begin
  Result := (fIndex < fTokenizer.Tokens.Count - 2) and
    SameStr(fTokenizer[fIndex]^.Text, 'struct') or
    SameStr(fTokenizer[fIndex]^.Text, 'class') or
    SameStr(fTokenizer[fIndex]^.Text, 'union');
  if Result then begin
    if fTokenizer[fIndex + 2]^.Text[1] <> ';' then begin // not: class something;
      I := fIndex;
      // the check for ']' was added because of this example:
      // struct option long_options[] = {
      //		{"debug", 1, 0, 'D'},
      //		{"info", 0, 0, 'i'},
      //    ...
      //  };
      while (I < fTokenizer.Tokens.Count) and not (fTokenizer[I]^.Text[Length(fTokenizer[I]^.Text)] in [';', ':', '{',
        '}', ',', ')', ']']) do
        Inc(I);
      if (I < fTokenizer.Tokens.Count) and not (fTokenizer[I]^.Text[1] in ['{', ':']) then
        Result := False;
    end;
  end;
end;

function TCppParser.CheckForMethod(var sType, sName, sArgs: AnsiString): boolean;
var
  fIndexBackup, CurrClassIndex, CurClassID, DelimPos: integer;
  bTypeOK, bNameOK, bArgsOK: boolean;
begin

  // Function template:
  // compiler directives (>= 0 words), added to type
  // type (>= 1 words)
  // name (1 word)
  // (argument list)
  // ; or {

  sType := ''; // should contain type "int"
  sName := ''; // should contain function name "foo::function"
  sArgs := ''; // should contain argument list "(int a)"

  bTypeOK := false;
  bNameOK := false;
  bArgsOK := false;

  // Don't modify index
  fIndexBackup := fIndex;

  // Gather data for the string parts
  while (fIndex < fTokenizer.Tokens.Count) and not (fTokenizer[fIndex]^.Text[1] in ['(', ';', ':', '{', '}', '#']) do
    begin

    // Skip some compiler extensions BEFORE our function type
    if not bTypeOK and StartsText('__mingw', fTokenizer[fIndex]^.Text) or SameStr('__attribute__',
      fTokenizer[fIndex]^.Text) then begin
      // TODO: this should be fixed by performing macro expansion
      if (fIndex + 1 < fTokenizer.Tokens.Count) and (fTokenizer[fIndex + 1]^.Text[1] = '(') then
        Inc(fIndex); // skip keyword. argument list is skipped at end of loop body

      // If the first brace is ahead, we've found the function name. Stop adding to type
    end else if (fIndex + 1 < fTokenizer.Tokens.Count) and (fTokenizer[fIndex + 1]^.Text[1] = '(') then
      begin // and start of a function
      sName := fTokenizer[fIndex]^.Text;
      sArgs := fTokenizer[fIndex + 1]^.Text;
      bTypeOK := sType <> '';
      bNameOK := sName <> '';
      bArgsOK := sArgs <> '';

      // Allow constructor/destructor too
      if not bTypeOk then begin

        // Check for constructor/destructor outside class body
        DelimPos := Pos('::', sName);
        if DelimPos > 0 then begin
          bTypeOK := true;
          sType := Copy(sName, 1, DelimPos - 1);
        end;
      end;

      // Are we inside a class body?
      if not bTypeOK then begin
        CurClassID := GetCurrentClass;
        if CurClassID <> -1 then begin
          CurrClassIndex := IndexOfStatement(CurClassID);
          if CurrClassIndex <> -1 then begin
            sType := fTokenizer[fIndex]^.Text;
            if sType[1] = '~' then
              Delete(sType, 1, 1);
            bTypeOK := (sType = PStatement(fStatementList[CurrClassIndex])^._ScopelessCmd);
          end;
        end;
      end;
      break;

      // Still walking through type
    end else {if IsValidIdentifier(fTokenizer[fIndex]^.Text) then} begin
      sType := sType + fTokenizer[fIndex]^.Text + ' ';
      bTypeOK := sType <> '';
    end;
    Inc(fIndex);
  end;

  fIndex := fIndexBackup;

  // Correct function, don't jump over
  if bTypeOK and bNameOK and bArgsOK then begin

    Result := true;
    sType := TrimRight(sType); // should contain type "int"
    sName := TrimRight(sName); // should contain function name "foo::function"
    sArgs := TrimRight(sArgs); // should contain argument list "(int a)"

    // TODO: check if this is function usage and skip if so?
  end else begin
    Result := false;
    //while (fIndex < fTokenizer.Tokens.Count) and not (fTokenizer[fIndex]^.Text[1] in ['{','}',',',';']) do
    //	Inc(fIndex);
  end;
end;

function TCppParser.CheckForScope: boolean;
begin
  Result := (fIndex < fTokenizer.Tokens.Count - 1) and
    (fTokenizer[fIndex + 1]^.Text = ':') and
    (SameStr(fTokenizer[fIndex]^.Text, 'public') or
    SameStr(fTokenizer[fIndex]^.Text, 'protected') or
    SameStr(fTokenizer[fIndex]^.Text, 'private'));
end;

function TCppParser.CheckForVar: boolean;
var
  I, fIndexBackup: integer;
begin
  // Be pessimistic
  Result := False;

  // Store old index
  fIndexBackup := fIndex;

  // Use fIndex so we can reuse checking functions
  if fIndex + 1 < fTokenizer.Tokens.Count then begin

    // Check the current and the next token
    for I := 0 to 1 do begin
      if CheckForKeyword or
        (fTokenizer[fIndex]^.Text[1] in ['#', ',', ';', ':', '{', '}', '!', '/', '+', '-', '<', '>']) or
        (fTokenizer[fIndex]^.Text[Length(fTokenizer[fIndex]^.Text)] = '.') or
      ((Length(fTokenizer[fIndex]^.Text) > 1) and
        (fTokenizer[fIndex]^.Text[Length(fTokenizer[fIndex]^.Text) - 1] = '-') and
        (fTokenizer[fIndex]^.Text[Length(fTokenizer[fIndex]^.Text)] = '>')) then begin

        // Reset index and fail
        fIndex := fIndexBackup;
        Exit; // fail

        // Could be a function pointer?
      end else if (fTokenizer[fIndex]^.Text[1] in ['(']) then begin

        // Quick fix: there must be a pointer operator in the first tiken
        if (fIndex + 1 >= fTokenizer.Tokens.Count) or
          (fTokenizer[fIndex + 1]^.Text[1] <> '(') or
          (Pos('*', fTokenizer[fIndex]^.Text) = 0) then begin

          // Reset index and fail
          fIndex := fIndexBackup;
          Exit; // fail
        end;
      end;
      Inc(fIndex);
    end;
  end;

  // Revert to the point we started at
  fIndex := fIndexBackup;

  // Fail if we do not find a comma or a semicolon or a ( (inline constructor)
  while fIndex < fTokenizer.Tokens.Count do begin
    if (fTokenizer[fIndex]^.Text[1] in ['#', '{', '}']) or CheckForKeyword then begin
      Break; // fail
    end else if (fIndex+1 < fTokenizer.Tokens.Count) and (fTokenizer[fIndex]^.Text[1] = '(') and (fTokenizer[fIndex]^.Text[2] = '(') then
      begin // TODO: is this used to remove __attribute stuff?
      Break;
    end else if fTokenizer[fIndex]^.Text[1] in [',', ';'] then begin
      Result := True;
      Break;
    end;
    Inc(fIndex);
  end;

  // Revert to the point we started at
  fIndex := fIndexBackup;
end;

procedure TCppParser.HandleMember;
begin
  repeat
    Inc(fIndex);
  until (fIndex >= fTokenizer.Tokens.Count) or (fTokenizer[fIndex]^.Text[1] in [';', '}']);
  Inc(fIndex);
end;

procedure TCppParser.HandleOtherTypedefs;
var
  NewType, OldType: AnsiString;
begin
  // Skip typedef word
  Inc(fIndex);

  // Walk up to first new word (before first comma or ;)
  while (fIndex + 1 < fTokenizer.Tokens.Count) and (not (fTokenizer[fIndex + 1]^.Text[1] in ['(', ',', ';'])) do begin
    OldType := OldType + fTokenizer[fIndex]^.Text + ' ';
    Inc(fIndex);
  end;
  OldType := TrimRight(OldType);

  // Add synonyms for old
  if (fIndex < fTokenizer.Tokens.Count) and (OldType <> '') then begin
    repeat
      // Support multiword typedefs
      if (fIndex + 1 < fTokenizer.Tokens.Count) and
        (fTokenizer[fIndex + 0]^.Text[1] = '(') and
        (fTokenizer[fIndex + 1]^.Text[1] = '(') then begin
        break; // TODO: do NOT handle function pointer defines
      end else if not (fTokenizer[fIndex]^.Text[1] in [',', ';']) then begin
        NewType := NewType + fTokenizer[fIndex]^.Text + ' '
      end else begin
        NewType := TrimRight(NewType);
        fLastID := AddStatement(
          -1, //UseID, // use same ID as synonym
          GetCurrentClass,
          fCurrentFile,
          'typedef ' + OldType + ' ' + NewType,
          OldType,
          NewType,
          '',
          fTokenizer[fIndex]^.Line,
          skTypedef,
          GetScope,
          fClassScope,
          False, // not visible in browser...
          True, // allow duplicates!
          False); // not a declaration
        NewType := '';
        if fTokenizer[fIndex]^.Text[1] = ';' then
          break;
      end;
      Inc(fIndex);
    until (fIndex >= fTokenizer.Tokens.Count);
  end;

  // Step over semicolon (saves one HandleStatement loop)
  Inc(fIndex);
end;

procedure TCppParser.HandlePreprocessor;
var
  DelimPos: integer;
  S, Name, Args, Value: AnsiString;
begin
  if StartsStr('#include ', fTokenizer[fIndex]^.Text) then begin // start of new file
    // format: #(spaces)include fullfilename:line
    // Strip keyword
    S := Copy(fTokenizer[fIndex]^.Text, Length('#include ') + 1, MaxInt);
    DelimPos := LastPos(':', S);
    if DelimPos > 3 then begin // ignore full file name stuff
      fCurrentFile := Copy(S, 1, DelimPos - 1);
      fIsSystemHeader := IsSystemHeaderFile(fCurrentFile);
      fIsProjectFile := fProjectFiles.IndexOf(fCurrentFile) <> -1;
      fIsHeader := IsHfile(fCurrentFile);

      // Mention progress to user
      if Assigned(fOnTotalProgress) and not fLaterScanning then
        fOnTotalProgress(Self, fCurrentFile, -1, -1);
    end;
  end else if StartsStr('#define ', fTokenizer[fIndex]^.Text) then begin

    // format: #define A B, remove define keyword
    S := TrimLeft(Copy(fTokenizer[fIndex]^.Text, Length('#define ') + 1, MaxInt));

    // Ask the preprocessor to cut parts up
    if Assigned(fPreprocessor) then
      fPreprocessor.GetDefineParts(S, Name, Args, Value);

    AddStatement(
      -1,
      -1, // defines don't belong to any scope
      fCurrentFile,
      '#define ' + Name + Args + ' ' + Value,
      '', // define has no type
      Name,
      Args,
      fTokenizer[FIndex]^.Line,
      skPreprocessor,
      ssGlobal,
      scsNone,
      False, // not visible
      True,
      False);
  end;
  Inc(fIndex);
end;

procedure TCppParser.HandleStructs(IsTypedef: boolean = False);
var
  S, S1, S2, Prefix, OldType, NewType: AnsiString;
  I: integer;
  IsStruct: boolean;
  UseID: integer;
begin
  S := fTokenizer[fIndex]^.Text;
  IsStruct := SameStr(S, 'struct') or SameStr(S, 'union');
  Prefix := S;
  Inc(fIndex); //skip 'struct'
  I := fIndex;
  UseID := -1;

  // Skip until the struct body starts
  while (I < fTokenizer.Tokens.Count) and not (fTokenizer[I]^.Text[1] in [';', '{']) do
    Inc(I);

  // Forward class/struct decl *or* typedef, e.g. typedef struct some_struct synonym1, synonym2;
  if (I < fTokenizer.Tokens.Count) and (fTokenizer[I]^.Text[1] = ';') then begin
    if IsTypedef then begin
      OldType := fTokenizer[fIndex]^.Text;
      repeat
        // Add definition statement for the synonym
        if (fIndex + 1 < fTokenizer.Tokens.Count) and (fTokenizer[fIndex + 1]^.Text[1] in [',', ';']) then begin
          NewType := fTokenizer[fIndex]^.Text;
          S := S + NewType + ' ';
          fLastID := AddStatement(
            -1,
            GetCurrentClass,
            fCurrentFile,
            'typedef ' + Prefix + ' ' + OldType + ' ' + NewType,
            OldType,
            NewType,
            '',
            fTokenizer[fIndex]^.Line,
            skTypedef,
            GetScope,
            fClassScope,
            False,
            True,
            False);
        end;
        Inc(fIndex);
      until (fIndex >= fTokenizer.Tokens.Count) or (fTokenizer[fIndex]^.Text[1] = ';');
    end;

    // normal class/struct decl
  end else begin
    if fTokenizer[fIndex]^.Text[1] <> '{' then begin
      S1 := '';
      S2 := '';
      repeat
        S := S + fTokenizer[fIndex]^.Text + ' ';
        if not (fTokenizer[fIndex]^.Text[1] in [',', ';', '{', ':']) then
          S1 := S1 + fTokenizer[fIndex]^.Text + ' ';
        if (fIndex + 1 < fTokenizer.Tokens.Count) and (fTokenizer[fIndex + 1]^.Text[1] = '(') then
          S2 := fTokenizer[fIndex]^.Text;
        if (fIndex + 1 < fTokenizer.Tokens.Count) and (fTokenizer[fIndex + 1]^.Text[1] in [',', ';', '{', ':']) then
          begin
          if S2 = '' then
            S2 := fTokenizer[fIndex]^.Text;
          S1 := TrimRight(S1);

          // TODO: fix this the nice way
          if (S1 = 'basic_string') and (S2 = 'basic_string') then begin
            S1 := 'string';
            S2 := 'string';
          end;

          if S1 <> '' then begin
            fLastID := AddStatement(
              -1,
              GetCurrentClass,
              fCurrentFile,
              Prefix + ' ' + S1,
              Prefix,
              S2,
              '',
              fTokenizer[fIndex]^.Line,
              skClass,
              GetScope,
              fClassScope,
              True,
              True,
              False);
          end;
          S1 := '';
        end;
        Inc(fIndex);
      until (fIndex >= fTokenizer.Tokens.Count) or (fTokenizer[fIndex]^.Text[1] in [':', '{', ';']);
      UseID := fLastID;
    end;

    // Walk to opening brace
    if (fIndex < fTokenizer.Tokens.Count) and (fTokenizer[fIndex]^.Text[1] = ':') then begin
      SetInheritance(fIndex); // set the _InheritsFromClasses value
      while (fIndex < fTokenizer.Tokens.Count) and (fTokenizer[fIndex]^.Text[1] <> '{') do // skip decl after ':'
        Inc(fIndex);
    end;

    // Check for struct names after '}'
    if IsStruct then begin

      // Walk to closing brace
      I := SkipBraces(fIndex);

      // Skip something?
      S1 := '';
      if (I + 1 < fTokenizer.Tokens.Count) and not (fTokenizer[I + 1]^.Text[1] in [';', '}']) then
        fSkipList.Add(I + 1);

      // Add synonyms after }
      if (I + 1 < fTokenizer.Tokens.Count) then begin
        repeat
          Inc(I);

          if not (fTokenizer[I]^.Text[1] in ['{', ',', ';']) then begin
            if (fTokenizer[I]^.Text[1] = '_') and (fTokenizer[I]^.Text[Length(fTokenizer[I]^.Text)] = '_') then begin
              // skip possible gcc attributes
              // start and end with 2 underscores (i.e. __attribute__)
              // so, to avoid slow checks of strings, we just check the first and last letter of the token
              // if both are underscores, we split
              Break;
            end else begin
              if fTokenizer[I]^.Text[Length(fTokenizer[I]^.Text)] = ']' then // cut-off array brackets
                S1 := S1 + Copy(fTokenizer[I]^.Text, 1, Pos('[', fTokenizer[I]^.Text) - 1) + ' '
              else if fTokenizer[I]^.Text[1] in ['*', '&'] then // do not add spaces after pointer operator
                S1 := S1 + fTokenizer[I]^.Text
              else
                S1 := S1 + fTokenizer[I]^.Text + ' ';
            end;
          end else begin
            S1 := TrimRight(S1);
            if S1 <> '' then begin
              fLastID := AddStatement(
                UseID,
                GetCurrentClass,
                fCurrentFile,
                Prefix + ' ' + S1,
                Prefix,
                S1,
                '',
                fTokenizer[I]^.Line,
                skClass,
                GetScope,
                fClassScope,
                True,
                False,
                False);
            end;
            UseID := fLastID; // TODO: should not be needed
            S1 := '';
          end;

          // Logging stuff
          if not (fTokenizer[I]^.Text[1] in [';', ',']) then
            S := S + ' ' + fTokenizer[I]^.Text;
        until (I >= fTokenizer.Tokens.Count - 1) or (fTokenizer[I]^.Text[1] in ['{', ';']);
      end;
    end;
    SetCurrentClass(fLastID);
  end;
end;

procedure TCppParser.HandleMethod(const sType, sName, sArgs: AnsiString);
var
  IsValid: boolean;
  I, DelimPos, CurrClassID, CurrClassIndex: integer;
  IsDeclaration: boolean;
begin
  IsValid := True;
  IsDeclaration := False;
  I := fIndex;

  // Skip over argument list
  while (fIndex + 2 < fTokenizer.Tokens.Count) and not (fTokenizer[fIndex]^.Text[1] in [';', ':', '{', '}']) do
    Inc(fIndex);

  // Check if this is a prototype
  CurrClassID := GetCurrentClass;
  if fTokenizer[fIndex]^.Text[1] in [';', '}'] then begin // prototype
    IsDeclaration := True;
    if not fIsHeader and (CurrClassID = -1) then // in a CPP file
      IsValid := False; // not valid
  end else begin

    // Find the function body start after the inherited constructor
    if fTokenizer[fIndex]^.Text[1] = ':' then
      while (fIndex < fTokenizer.Tokens.Count) and (not (fTokenizer[fIndex]^.Text[1] in [';', '{', '}'])) do
        Inc(fIndex);

    // Still a prototype
    if (fIndex < fTokenizer.Tokens.Count) and (fTokenizer[fIndex]^.Text[1] in [';', '}']) then begin
      IsDeclaration := True;
      if not fIsHeader and (CurrClassID = -1) then
        IsValid := False;
    end;
  end;

  if IsValid then begin

    // Use the class the function belongs to as the parent ID if the function is declared outside of the class body
    DelimPos := Pos('::', sName);
    if DelimPos > 0 then begin
      CurrClassIndex := CheckIfCommandExists(Copy(sName, 1, DelimPos - 1), skClass, CurrClassID);
      if CurrClassIndex <> -1 then
        CurrClassID := PStatement(fStatementList[CurrClassIndex])^._ID;
    end;

    // If this is a class function, check for duplicates to form declaration/definition pairs
    // If this is a global function, don't perform duplicate checks (allow overloading), even if we do not check arg lists
    fLastID := AddStatement(
      -1,
      CurrClassID,
      fCurrentFile,
      sType + ' ' + sName + ' ' + sArgs,
      sType,
      sName,
      sArgs,
      fTokenizer[fIndex - 1]^.Line,
      skFunction,
      GetScope,
      fClassScope,
      True,
      CurrClassID = -1, // allow dupes for globals
      IsDeclaration);
  end;

  // Don't parse the function's block now... It will be parsed when user presses ctrl+space inside it ;)
  if (fTokenizer[fIndex]^.Text[1] = '{') then
    fIndex := SkipBraces(fIndex) + 1 // add 1 so that '}' is not visible to parser
  else if (fTokenizer[fIndex]^.Text[1] = ';') then
    Inc(fIndex);
  if I = fIndex then // if not moved ahead, something is wrong but don't get stuck ;)
    if fIndex < fTokenizer.Tokens.Count then
      if not (fTokenizer[fIndex]^.Text[1] in ['{', '}']) then
        Inc(fIndex);
end;

procedure TCppParser.HandleScope;
begin
  if SameStr(fTokenizer[fIndex]^.Text, 'public') then
    fClassScope := scsPublic
  else if SameStr(fTokenizer[fIndex]^.Text, 'private') then
    fClassScope := scsPrivate
  else if SameStr(fTokenizer[fIndex]^.Text, 'protected') then
    fClassScope := scsProtected
  else
    fClassScope := scsNone;
  Inc(fIndex, 2); // the scope is followed by a ':'
end;

procedure TCppParser.HandleKeyword;
begin
  // Skip
  if SameStr(fTokenizer[fIndex]^.Text, 'and') or
    SameStr(fTokenizer[fIndex]^.Text, 'and_eq') or
    SameStr(fTokenizer[fIndex]^.Text, 'bitand') or
    SameStr(fTokenizer[fIndex]^.Text, 'bitor') or
    SameStr(fTokenizer[fIndex]^.Text, 'break') or
    SameStr(fTokenizer[fIndex]^.Text, 'compl') or
    SameStr(fTokenizer[fIndex]^.Text, 'const') or
    SameStr(fTokenizer[fIndex]^.Text, 'constexpr') or
    SameStr(fTokenizer[fIndex]^.Text, 'const_cast') or
    SameStr(fTokenizer[fIndex]^.Text, 'continue') or
    SameStr(fTokenizer[fIndex]^.Text, 'dynamic_cast') or
    SameStr(fTokenizer[fIndex]^.Text, 'else') or
    SameStr(fTokenizer[fIndex]^.Text, 'explicit') or
    SameStr(fTokenizer[fIndex]^.Text, 'export') or
    SameStr(fTokenizer[fIndex]^.Text, 'extern') or
    SameStr(fTokenizer[fIndex]^.Text, 'false') or
    SameStr(fTokenizer[fIndex]^.Text, 'for') or
    SameStr(fTokenizer[fIndex]^.Text, 'friend') or
    SameStr(fTokenizer[fIndex]^.Text, 'inline') or
    SameStr(fTokenizer[fIndex]^.Text, 'mutable') or
    SameStr(fTokenizer[fIndex]^.Text, 'noexcept') or
    SameStr(fTokenizer[fIndex]^.Text, 'not') or
    SameStr(fTokenizer[fIndex]^.Text, 'not_eq') or
    SameStr(fTokenizer[fIndex]^.Text, 'nullptr') or
    SameStr(fTokenizer[fIndex]^.Text, 'or') or
    SameStr(fTokenizer[fIndex]^.Text, 'or_eq') or
    SameStr(fTokenizer[fIndex]^.Text, 'register') or
    SameStr(fTokenizer[fIndex]^.Text, 'reinterpret_cast') or
    SameStr(fTokenizer[fIndex]^.Text, 'static') or
    SameStr(fTokenizer[fIndex]^.Text, 'static_assert') or
    SameStr(fTokenizer[fIndex]^.Text, 'static_cast') or
    SameStr(fTokenizer[fIndex]^.Text, 'template') or
    SameStr(fTokenizer[fIndex]^.Text, 'this') or
    SameStr(fTokenizer[fIndex]^.Text, 'thread_local') or
    SameStr(fTokenizer[fIndex]^.Text, 'true') or
    SameStr(fTokenizer[fIndex]^.Text, 'typename') or
    SameStr(fTokenizer[fIndex]^.Text, 'virtual') or
    SameStr(fTokenizer[fIndex]^.Text, 'volatile') or
    SameStr(fTokenizer[fIndex]^.Text, 'xor') or
    SameStr(fTokenizer[fIndex]^.Text, 'xor_eq') then begin
    Inc(fIndex);

    // Skip to ;
  end else if SameStr(fTokenizer[fIndex]^.Text, 'delete') or
    SameStr(fTokenizer[fIndex]^.Text, 'goto') or
    SameStr(fTokenizer[fIndex]^.Text, 'new') or
    SameStr(fTokenizer[fIndex]^.Text, 'return') or
    SameStr(fTokenizer[fIndex]^.Text, 'throw') or
    SameStr(fTokenizer[fIndex]^.Text, 'using') then begin
    repeat
      Inc(fIndex);
    until (fIndex >= fTokenizer.Tokens.Count) or (fTokenizer[fIndex]^.Text[1] in [';']);
    Inc(fIndex); // step over

    // Skip to :
  end else if SameStr(fTokenizer[fIndex]^.Text, 'case') or
    SameStr(fTokenizer[fIndex]^.Text, 'default') then begin

    repeat
      Inc(fIndex);
    until (fIndex >= fTokenizer.Tokens.Count) or (fTokenizer[fIndex]^.Text[1] in [':']);

    // Skip to )
  end else if SameStr(fTokenizer[fIndex]^.Text, 'alignas') or
    SameStr(fTokenizer[fIndex]^.Text, 'alignof') or
    SameStr(fTokenizer[fIndex]^.Text, 'decltype') or
    SameStr(fTokenizer[fIndex]^.Text, 'if') or
    SameStr(fTokenizer[fIndex]^.Text, 'sizeof') or
    SameStr(fTokenizer[fIndex]^.Text, 'switch') or
    SameStr(fTokenizer[fIndex]^.Text, 'typeid') or
    SameStr(fTokenizer[fIndex]^.Text, 'while') then begin

    repeat
      Inc(fIndex);
    until (fIndex >= fTokenizer.Tokens.Count) or (fTokenizer[fIndex]^.Text[Length(fTokenizer[fIndex]^.Text)] in [')']);
    Inc(fIndex); // step over

    // Skip to {
  end else if SameStr(fTokenizer[fIndex]^.Text, 'catch') or
    SameStr(fTokenizer[fIndex]^.Text, 'do') or
    SameStr(fTokenizer[fIndex]^.Text, 'namespace') or
    SameStr(fTokenizer[fIndex]^.Text, 'try') then begin

    repeat
      Inc(fIndex);
    until (fIndex >= fTokenizer.Tokens.Count) or (fTokenizer[fIndex]^.Text[1] in ['{']);

    // Skip to }
  end else if SameStr(fTokenizer[fIndex]^.Text, 'asm') then begin

    repeat
      Inc(fIndex);
    until (fIndex >= fTokenizer.Tokens.Count) or (fTokenizer[fIndex]^.Text[1] in ['}']);
    Inc(fIndex); // step over
  end;
end;

procedure TCppParser.HandleVar;
var
  LastType, Args, Cmd, S: AnsiString;
  IsFunctionPointer: boolean;
begin
  // Keep going and stop on top of the variable name
  LastType := '';
  IsFunctionPointer := False;
  repeat
    if (fIndex + 2 < fTokenizer.Tokens.Count) and (fTokenizer[fIndex + 1]^.Text[1] = '(') and (fTokenizer[fIndex +
      2]^.Text[1] = '(') then begin
      IsFunctionPointer := Pos('*', fTokenizer[fIndex + 1]^.Text) > 0;
      if not IsFunctionPointer then
        break; // inline constructor
    end else if (fIndex + 1 < fTokenizer.Tokens.Count) and (fTokenizer[fIndex + 1]^.Text[1] in ['(', ',', ';', ':', '}',
      '#']) then begin
      Break;
    end;

    // TODO: why is this needed?
    if (not SameStr(fTokenizer[fIndex]^.Text, 'struct')) and
      (not SameStr(fTokenizer[fIndex]^.Text, 'class')) and
      (not SameStr(fTokenizer[fIndex]^.Text, 'union')) then
      LastType := LastType + ' ' + fTokenizer[fIndex]^.Text;
    Inc(fIndex);
  until (fIndex >= fTokenizer.Tokens.Count) or IsFunctionPointer;
  LastType := Trim(LastType);

  // Don't bother entering the scanning loop when we have failed
  if fIndex >= fTokenizer.Tokens.Count then
    Exit;

  // Find the variable name
  repeat

    // Skip bit identifiers,
    // e.g.:
    // handle
    // unsigned short bAppReturnCode:8,reserved:6,fBusy:1,fAck:1
    // as
    // unsigned short bAppReturnCode,reserved,fBusy,fAck
    if (fIndex < fTokenizer.Tokens.Count) and (fTokenizer[fIndex]^.Text[1] = ':') then begin
      repeat
        Inc(fIndex);
      until (fIndex >= fTokenizer.Tokens.Count) or (fTokenizer[fIndex]^.Text[1] in [',', ';', '{', '}']);
    end;

    // Skip inline constructors,
    // e.g.:
    // handle
    // int a(3)
    // as
    // int a
    if (not IsFunctionPointer) and (fIndex < fTokenizer.Tokens.Count) and (fTokenizer[fIndex]^.Text[1] = '(') then begin
      while (fIndex < fTokenizer.Tokens.Count) and not (fTokenizer[fIndex]^.Text[1] in [',', ';', '{', '}']) do
        Inc(fIndex);
    end;

    // Did we stop on top of the variable name?
    if fIndex < fTokenizer.Tokens.Count then begin
      if not (fTokenizer[fIndex]^.Text[1] in [',', ';']) then begin
        if IsFunctionPointer and (fIndex + 1 < fTokenizer.Tokens.Count) then begin
          S := fTokenizer[fIndex]^.Text;
          Cmd := Trim(Copy(S, 3, Length(S) - 3)); // (*foo) -> foo
          Args := fTokenizer[fIndex + 1]^.Text; // (int a,int b)
          LastType := LastType + '(*)' + Args; // void(int a,int b)
          Inc(fIndex);
        end else if fTokenizer[fIndex]^.Text[Length(fTokenizer[fIndex]^.Text)] = ']' then begin //array; break args
          Cmd := Copy(fTokenizer[fIndex]^.Text, 1, Pos('[', fTokenizer[fIndex]^.Text) - 1);
          Args := Copy(fTokenizer[fIndex]^.Text, Pos('[', fTokenizer[fIndex]^.Text), Length(fTokenizer[fIndex]^.Text) -
            Pos('[', fTokenizer[fIndex]^.Text) + 1);
        end else begin
          Cmd := fTokenizer[fIndex]^.Text;
          Args := '';
        end;
        fLastID := AddStatement(
          -1,
          GetCurrentClass,
          fCurrentFile,
          LastType + ' ' + Cmd,
          LastType,
          Cmd,
          Args,
          fTokenizer[fIndex]^.Line,
          skVariable,
          GetScope,
          fClassScope,
          True,
          True,
          False);
      end;

      // Step over the variable name
      if not (fTokenizer[fIndex]^.Text[1] in [';', '{', '}']) then
        Inc(fIndex);
    end;
  until (fIndex >= fTokenizer.Tokens.Count) or (fTokenizer[fIndex]^.Text[1] in [';', '{', '}']);

  // Skip ; and ,
  if (fIndex < fTokenizer.Tokens.Count) and not (fTokenizer[fIndex]^.Text[1] in ['{', '}']) then
    Inc(fIndex);
end;

procedure TCppParser.HandleEnum;
var
  LastType: AnsiString;
  Args: AnsiString;
  Cmd: AnsiString;
  EnumName: AnsiString;
  I: integer;
begin
  EnumName := '';
  Inc(fIndex); //skip 'enum'
  if fTokenizer[fIndex]^.Text[1] = '{' then begin // enum {...} NAME

    // Skip to the closing brace
    I := SkipBraces(fIndex);

    // Have we found the name?
    if (fIndex + 1 < fTokenizer.Tokens.Count) and (fTokenizer[I]^.Text[1] = '}') then
      if fTokenizer[I + 1]^.Text[1] <> ';' then
        EnumName := EnumName + fTokenizer[I + 1]^.Text + ' ';
  end else begin // enum NAME {...};
    while (fIndex < fTokenizer.Tokens.Count) and (not (fTokenizer[fIndex]^.Text[1] in ['{', ';'])) do begin
      EnumName := EnumName + fTokenizer[fIndex]^.Text + ' ';
      Inc(fIndex);
    end;

    // An opening brace must be present after NAME
    if (fIndex >= fTokenizer.Tokens.Count) or (fTokenizer[fIndex]^.Text[1] <> '{') then
      Exit;

    // Skip opening brace
    Inc(fIndex);
  end;
  EnumName := Trim(EnumName);

  // Add statement for enum name too
  AddStatement(
    -1,
    GetCurrentClass,
    fCurrentFile,
    'enum ' + EnumName,
    'enum',
    EnumName,
    Args,
    fTokenizer[fIndex]^.Line,
    skTypedef,
    GetScope,
    fClassScope,
    False,
    True,
    False);

  // Call every member "enum NAME ITEMNAME"
  LastType := 'enum ' + Trim(EnumName);
  repeat
    if not (fTokenizer[fIndex]^.Text[1] in [',', ';']) then begin
      if fTokenizer[fIndex]^.Text[Length(fTokenizer[fIndex]^.Text)] = ']' then begin //array; break args
        Cmd := Copy(fTokenizer[fIndex]^.Text, 1, Pos('[', fTokenizer[fIndex]^.Text) - 1);
        Args := Copy(fTokenizer[fIndex]^.Text, Pos('[', fTokenizer[fIndex]^.Text), Length(fTokenizer[fIndex]^.Text) -
          Pos('[', fTokenizer[fIndex]^.Text) + 1);
      end else begin
        Cmd := fTokenizer[fIndex]^.Text;
        Args := '';
      end;
      fLastID := AddStatement(
        -1, // enum members are treated as globals for now
        GetCurrentClass,
        fCurrentFile,
        LastType + ' ' + fTokenizer[fIndex]^.Text,
        LastType,
        Cmd,
        Args,
        fTokenizer[fIndex]^.Line,
        skEnum,
        GetScope,
        fClassScope,
        False,
        True,
        False);
    end;
    Inc(fIndex);
  until (fIndex >= fTokenizer.Tokens.Count) or (fTokenizer[fIndex]^.Text[1] in [';', '{', '}']);

  // Step over closing brace
  if fTokenizer[fIndex]^.Text[1] = '}' then
    Inc(fIndex);
end;

function TCppParser.HandleStatement: boolean;
var
  S1, S2, S3: AnsiString;
begin
  if fTokenizer[fIndex]^.Text[1] = '{' then begin
    Inc(fLevel);
    Inc(fIndex);
  end else if fTokenizer[fIndex]^.Text[1] = '}' then begin
    Dec(fLevel);
    Inc(fIndex);
    RemoveCurrentClass;
  end else if CheckForPreprocessor then begin
    HandlePreprocessor;
  end else if CheckForKeyword then begin // includes template now
    HandleKeyword;
  end else if CheckForMember then begin
    HandleMember;
  end else if CheckForScope then begin
    HandleScope;
  end else if CheckForEnum then begin
    HandleEnum;
  end else if CheckForTypedef then begin
    if CheckForTypedefStruct then begin
      Inc(fIndex); // skip 'typedef'
      HandleStructs(True)
    end else if CheckForTypedefEnum then begin
      Inc(fIndex); // skip 'typedef'
      HandleEnum;
    end else
      HandleOtherTypedefs;
  end else if CheckForStructs then begin
    HandleStructs(False);
  end else if CheckForMethod(S1, S2, S3) then begin
    HandleMethod(S1, S2, S3); // don't recalculate parts
  end else if CheckForVar then begin
    HandleVar;
  end else
    Inc(fIndex);

  CheckForSkipStatement;

  Result := fIndex < fTokenizer.Tokens.Count;
end;

procedure TCppParser.Parse(const FileName: AnsiString; ManualUpdate: boolean = False; processInh: boolean = True;
  Stream: TMemoryStream = nil);
begin
  // Perform some validation before we start
  if not fEnabled then
    Exit;
  if not Assigned(Stream) and not (IsCfile(Filename) or IsHfile(Filename)) then // support only known C/C++ files
    Exit;
  if (fTokenizer = nil) or (fPreprocessor = nil) then
    Exit;

  // Start a timer here
  if (not ManualUpdate) and Assigned(fOnStartParsing) then
    fOnStartParsing(Self);

  // Preprocess the file...
  try
    // Let the preprocessor augment the include records
    fPreprocessor.SetIncludesList(fIncludesList);
    fPreprocessor.SetIncludePaths(fIncludePaths);
    fPreprocessor.SetScannedFileList(fScannedFiles);
    fPreprocessor.SetScanOptions(fParseGlobalHeaders, fParseLocalHeaders);
    if Assigned(Stream) then
      fPreprocessor.PreprocessStream(FileName, Stream)
    else
      fPreprocessor.PreprocessFile(FileName); // load contents from disk
  except
    if (not ManualUpdate) and Assigned(fOnEndParsing) then
      fOnEndParsing(Self);
    fPreprocessor.Reset; // remove buffers from memory
    Exit;
  end;

  // Tokenize the preprocessed buffer file
  try
    fTokenizer.TokenizeBuffer(PAnsiChar(fPreprocessor.Result));
    if fTokenizer.Tokens.Count = 0 then begin
      fPreprocessor.Reset;
      fTokenizer.Reset;
      Exit;
    end;
  except
    if (not ManualUpdate) and Assigned(fOnEndParsing) then
      fOnEndParsing(Self);
    fPreprocessor.Reset;
    fTokenizer.Reset;
    Exit;
  end;

  // Tokenize the token list
  fIndex := 0;
  fLevel := 0;
  fLastID := -1;
  fClassScope := scsNone;
  try
    repeat
    until not HandleStatement;
    if processInh then
      PostProcessInheritance;
  finally
    fSkipList.Clear; // remove data from memory, but reuse structures
    fCurrentClassLevel.Clear;
    FCurrentClass.Clear;
    fPreprocessor.Reset;
    fTokenizer.Reset;
    if (not ManualUpdate) and Assigned(fOnEndParsing) then
      fOnEndParsing(Self);
  end;
  if not ManualUpdate then
    if Assigned(fOnUpdate) then
      fOnUpdate(Self);
end;

procedure TCppParser.Reset(KeepLoaded: boolean = True);
var
  I: integer;
begin
  if Assigned(fOnBusy) then
    fOnBusy(Self);

  fFilesToScan.Clear;
  if Assigned(fTokenizer) then
    fTokenizer.Reset;

  // Delete everything that isn't cached
  if KeepLoaded then begin

    // Remove statements not in cache
    for I := fStatementList.Count - 1 downto fBaseIndex do begin
      Dispose(PStatement(fStatementList[I]));
      fStatementList.Delete(I);
    end;

    // Scanned files not in cache need to be deleted
    for I := fScannedFiles.Count - 1 downto fScannedBaseIndex do
      fScannedFiles.Delete(I);

    // Remove files included by cache
    for I := fIncludesList.Count - 1 downto fScannedBaseIndex do begin
      Dispose(PFileIncludes(fIncludesList[I]));
      fIncludesList.Delete(I);
    end;

  // Delete everything
  end else begin

    // Remove all statements
    for I := 0 to fStatementList.Count - 1 do
      Dispose(PStatement(fStatementList[I]));
    fStatementList.Clear;

    // We haven't scanned anything anymore
    fScannedFiles.Clear;

    // We don't include anything anymore
    for I := fIncludesList.Count - 1 downto 0 do
      Dispose(PFileIncludes(fIncludesList[I]));
    fIncludesList.Clear;

    // Clear the cache too
    fCacheContents.Clear;

    fNextID := 0;
    fBaseIndex := 0;
  end;

  fProjectFiles.Clear;

  if Assigned(fOnUpdate) then
    fOnUpdate(Self);
end;

procedure TCppParser.ParseList;
var
  I: integer;
begin
  if not fEnabled then
    Exit;
  if Assigned(fOnBusy) then
    fOnBusy(Self);
  if Assigned(fOnStartParsing) then
    fOnStartParsing(Self);
  try
    for I := 0 to fFilesToScan.Count - 1 do begin
      if Assigned(fOnTotalProgress) then
        fOnTotalProgress(Self, fFilesToScan[i], fFilesToScan.Count, i);
      if fScannedFiles.IndexOf(fFilesToScan[i]) = -1 then begin
        Parse(fFilesToScan[i], True, False);
      end;
    end;
    fFilesToScan.Clear;
    PostProcessInheritance;
    fStatementList.Pack;
  finally
    if Assigned(fOnEndParsing) then
      fOnEndParsing(Self);
  end;
  if Assigned(fOnUpdate) then
    fOnUpdate(Self);
end;

function TCppParser.GetSystemHeaderFileName(const FileName: AnsiString): AnsiString;
begin
  Result := cbutils.GetSystemHeaderFileName(FileName, fIncludePaths);
end;

function TCppParser.GetLocalHeaderFileName(const RelativeTo, FileName: AnsiString): AnsiString;
begin
  Result := cbutils.GetLocalHeaderFileName(RelativeTo, FileName);
end;

function TCppParser.GetHeaderFileName(const RelativeTo, Line: AnsiString): AnsiString;
begin
  Result := cbutils.GetHeaderFileName(RelativeTo, Line, fIncludePaths);
end;

function TCppParser.IsIncludeLine(const Line: AnsiString): boolean;
begin
  Result := cbutils.IsIncludeLine(Line);
end;

procedure TCppParser.AddFileToScan(Value: AnsiString; InProject: boolean);
begin
  Value := StringReplace(Value, '/', '\', [rfReplaceAll]); // only accept full file names

  // Update project listing
  if InProject then
    if fProjectFiles.IndexOf(Value) = -1 then
      fProjectFiles.Add(Value);

  // Only parse given file
  if fFilesToScan.IndexOf(Value) = -1 then // check scheduled files
    if fScannedFiles.IndexOf(Value) = -1 then // check files already parsed
      fFilesToScan.Add(Value);
end;

procedure TCppParser.AddIncludePath(const Value: AnsiString);
var
  S: AnsiString;
begin
  S := AnsiDequotedStr(Value, '"');
  if fIncludePaths.IndexOf(S) = -1 then
    fIncludePaths.Add(S);
end;

procedure TCppParser.AddProjectIncludePath(const Value: AnsiString);
var
  S: AnsiString;
begin
  S := AnsiDequotedStr(Value, '"');
  if fProjectIncludePaths.IndexOf(S) = -1 then
    fProjectIncludePaths.Add(S);
end;

procedure TCppParser.ClearIncludePaths;
begin
  fIncludePaths.Clear;
end;

procedure TCppParser.ClearProjectIncludePaths;
begin
  fProjectIncludePaths.Clear;
end;

procedure TCppParser.ResetDefines;
begin
  if Assigned(fPreprocessor) then
    fPreprocessor.ResetDefines;
end;

procedure TCppParser.AddHardDefineByParts(const Name, Args, Value: AnsiString);
begin
  if Assigned(fPreprocessor) then
    fPreprocessor.AddDefineByParts(Name, Args, Value, True);
end;

procedure TCppParser.AddHardDefineByLine(const Line: AnsiString);
begin
  if Assigned(fPreprocessor) then begin
    if Pos('#', Line) = 1 then
      fPreprocessor.AddDefineByLine(TrimLeft(Copy(Line, 2, MaxInt)), True)
    else
      fPreprocessor.AddDefineByLine(Line, True);
  end;
end;

function TCppParser.IsSystemHeaderFile(const FileName: AnsiString): boolean;
begin
  Result := cbutils.IsSystemHeaderFile(FileName, fIncludePaths);
end;

procedure TCppParser.ReParseFile(const FileName: AnsiString; InProject: boolean; OnlyIfNotParsed: boolean = False;
  UpdateView: boolean = True; Stream: TMemoryStream = nil);
var
  FName: AnsiString;
  CFile, HFile: AnsiString;
  I: integer;
begin
  if not fEnabled then
    Exit;
  FName := FileName;
  if OnlyIfNotParsed and (fScannedFiles.IndexOf(FName) <> -1) then
    Exit;
  if Assigned(fOnBusy) then
    fOnBusy(Self);

  // Always invalidate file pairs. If we don't, reparsing the header
  // screws up the information inside the source file
  if not Assigned(Stream) then
    GetSourcePair(FName, CFile, HFile);
  fInvalidatedIDs.Clear;
  if not Assigned(Stream) then begin
    InvalidateFile(CFile);
    InvalidateFile(HFile);
  end else
    InvalidateFile(FileName);

  if InProject then begin
    if (CFile <> '') and (fProjectFiles.IndexOf(CFile) = -1) then
      fProjectFiles.Add(CFile);
    if (HFile <> '') and (fProjectFiles.IndexOf(HFile) = -1) then
      fProjectFiles.Add(HFile);
  end else begin
    I := fProjectFiles.IndexOf(CFile);
    if I <> -1 then
      fProjectFiles.Delete(I);
    I := fProjectFiles.IndexOf(HFile);
    if I <> -1 then
      fProjectFiles.Delete(I);
  end;

  // Parse from disk or stream
  if Assigned(fOnStartParsing) then
    fOnStartParsing(Self);
  try
    if not Assigned(Stream) then begin
      if CFile = '' then
        Parse(HFile, True, True) // headers should be parsed via include
      else
        Parse(CFile, True, True); // headers should be parsed via include
    end else
      Parse(FileName, True, True, Stream); // or from stream
    fFilesToScan.Clear;
    ReProcessInheritance;
  finally
    if Assigned(fOnEndParsing) then
      fOnEndParsing(Self);
  end;
  if UpdateView then
    if Assigned(fOnUpdate) then
      fOnUpdate(Self);
end;

procedure TCppParser.InvalidateFile(const FileName: AnsiString);
var
  I: integer;
  I1: integer;
  P: PFileIncludes;
begin
  if Filename = '' then
    Exit;

  // POSSIBLE PROBLEM:
  // here we delete all the statements that belong to the specified file.
  // what happens with the statements that have _ParentID on one of these???
  // what happens with the statements that inherit from one of these???
  // POSSIBLE WORKAROUND 1: invalidate the other file too (don't like it much...)

  // delete statements of file
  I := 0;
  while I < fStatementList.Count do
    if SameText(PStatement(fStatementList[I])^._FileName, FileName) or
      SameText(PStatement(fStatementList[I])^._DeclImplFileName, FileName) then begin
      if PStatement(fStatementList[I])^._Kind = skClass then // only classes have inheritance
        fInvalidatedIDs.Add(PStatement(fStatementList[I])^._ID);

      Dispose(PStatement(fStatementList[I]));
      fStatementList.Delete(I);
    end else
      Inc(I);

  fStatementList.Pack;

  // delete it from scannedfiles
  I1 := fScannedFiles.IndexOf(FileName);
  if I1 <> -1 then
    fScannedFiles.Delete(I1);

  // remove its include files list
  P := FindFileIncludes(FileName, True);
  if Assigned(P) then
    Dispose(PFileIncludes(P));
end;

procedure TCppParser.Save(const FileName: AnsiString; const relativeto: AnsiString);
var
  I, I2, HowMany: integer;
  MAGIC: array[0..7] of Char;
  relative: AnsiString;
begin
  MAGIC := 'CPPP 0.2'; // Version 5.6.0 (and newer) use CPPP 0.2
  fCacheContents.Assign(fScannedFiles);
  fScannedBaseIndex := fCacheContents.Count;

  if FileExists(FileName) then
    DeleteFile(FileName);

  if fStatementList.Count = 0 then
    Exit; // don't bother

  // Try writing the whole file in one go
  with TMemoryStream.Create do try

      // At least reserve for all statements
      SetSize(fStatementList.Count * 500); // 500 bytes per statement

      // Use the memory buffer from here on
      Write(MAGIC, sizeof(MAGIC));

      // write statements
      HowMany := fStatementList.Count - 1;
      Write(HowMany, SizeOf(Integer));
      for I := 0 to HowMany do begin
        with PStatement(fStatementList[I])^ do begin

          // Write integer data...
          Write(_ID, SizeOf(integer));
          Write(_ParentID, SizeOf(integer));
          Write(_Kind, SizeOf(byte));
          Write(_Scope, SizeOf(integer));
          Write(_ClassScope, SizeOf(integer));
          Write(_IsDeclaration, SizeOf(boolean));
          Write(_DeclImplLine, SizeOf(integer));
          Write(_Line, SizeOf(integer));
          Write(_Visible, SizeOf(boolean));
          Write(_InSystemHeader, SizeOf(boolean));

          // Write data, including length
          I2 := Length(_FullText);
          Write(I2, SizeOf(Integer));
          Write(_FullText[1], I2); // can't write length and data in one call, not allowed to read [0] :(

          I2 := Length(_Type);
          Write(I2, SizeOf(Integer));
          Write(_Type[1], I2);

          I2 := Length(_ScopeCmd);
          Write(I2, SizeOf(Integer));
          Write(_ScopeCmd[1], I2);

          I2 := Length(_Args);
          Write(I2, SizeOf(Integer));
          Write(_Args[1], I2);

          I2 := Length(_ScopelessCmd);
          Write(I2, SizeOf(Integer));
          Write(_ScopelessCmd[1], I2);

          // Save RELATIVE filenames
          relative := ReplaceFirstText(_DeclImplFileName, relativeto, '%path%\');
          I2 := Length(relative);
          Write(I2, SizeOf(Integer));
          Write(relative[1], I2);

          // Save RELATIVE filenames
          relative := ReplaceFirstText(_FileName, relativeto, '%path%\');
          I2 := Length(relative);
          Write(I2, SizeOf(Integer));
          Write(relative[1], I2);

          I2 := Length(_InheritsFromIDs);
          Write(I2, SizeOf(Integer));
          Write(_InheritsFromIDs[1], I2);

          I2 := Length(_InheritsFromClasses);
          Write(I2, SizeOf(Integer));
          Write(_InheritsFromClasses[1], I2);
        end;
      end;

      // write scanned files (cache contents)
      HowMany := fScannedFiles.Count - 1;
      Write(HowMany, SizeOf(Integer));
      for I := 0 to HowMany do begin

        // Save RELATIVE filenames
        relative := ReplaceFirstText(fScannedFiles[I], relativeto, '%path%\');
        I2 := Length(relative);
        Write(I2, SizeOf(Integer));
        Write(relative[1], I2);
      end;

      // write file includes list for each file scanned
      HowMany := fIncludesList.Count - 1;
      Write(HowMany, SizeOf(Integer));
      for I := 0 to HowMany do begin

        // Save RELATIVE filenames
        relative := ReplaceFirstText(PFileIncludes(fIncludesList[I])^.BaseFile, relativeto, '%path%\');
        I2 := Length(relative);
        Write(I2, SizeOf(Integer));
        Write(relative[1], I2);

        // Save RELATIVE filenames
        relative := ReplaceFirstText(PFileIncludes(fIncludesList[I])^.IncludeFiles, relativeto, '%path%\');
        I2 := Length(relative);
        Write(I2, SizeOf(Integer));
        Write(relative[1], I2);
      end;

      // Shrink to fit
      SetSize(Position);

      SaveToFile(FileName);
    finally
      Free;
      fBaseIndex := fNextID;
    end;

  // Debug: save a readable table too
  {with TStringList.Create do try
   for I := 0 to fStatementList.Count - 1 do begin
    with PStatement(fStatementList[I])^ do begin
     Add(IntToStr(I) + #9 + IntToStr(_Line) + #9 + _FullText);
    end;
   end;
   SaveToFile('C:\cache.txt');
  finally
   Free;
  end;}
end;

procedure TCppParser.Load(const FileName: AnsiString; const relativeto: AnsiString);
var
  HowMany: integer;
  I, ItemLength: integer;
  MAGIC: array[0..7] of Char;
  Statement: PStatement;
  P: PFileIncludes;
  relative: AnsiString;
begin

  // Try reading the whole file in one go
  with TMemoryStream.Create do try

      if not FileExists(FileName) then
        Exit;

      LoadFromFile(FileName);

      if Size = 0 then
        Exit;

      // Use the memory buffer from here on
      Read(MAGIC, sizeof(MAGIC));

      // Version 5.6.0 uses CPPP 0.2
      if (MAGIC <> 'CPPP 0.1') and (MAGIC <> 'CPPP 0.2') then
        Exit;

      Read(HowMany, sizeof(Integer));
      for I := 0 to HowMany do begin
        Statement := New(PStatement);
        with Statement^ do begin

          // Read the actual statement
          Read(_ID, SizeOf(integer));
          Read(_ParentID, SizeOf(integer));
          Read(_Kind, SizeOf(byte));
          Read(_Scope, SizeOf(integer));
          Read(_ClassScope, SizeOf(integer));
          Read(_IsDeclaration, SizeOf(boolean));
          Read(_DeclImplLine, SizeOf(integer));
          Read(_Line, SizeOf(integer));

          if Magic = 'CPPP 0.2' then begin
            Read(_Visible, SizeOf(boolean));
            Read(_InSystemHeader, SizeOf(boolean));
          end else begin
            _Visible := False;
            _InSystemHeader := True;
          end;

          Read(ItemLength, SizeOf(Integer));
          SetLength(_FullText, ItemLength);
          Read(_FullText[1], ItemLength);

          Read(ItemLength, SizeOf(Integer));
          SetLength(_Type, ItemLength);
          Read(_Type[1], ItemLength);

          Read(ItemLength, SizeOf(Integer));
          SetLength(_ScopeCmd, ItemLength);
          Read(_ScopeCmd[1], ItemLength);

          Read(ItemLength, SizeOf(Integer));
          SetLength(_Args, ItemLength);
          Read(_Args[1], ItemLength);

          Read(ItemLength, SizeOf(Integer));
          SetLength(_ScopelessCmd, ItemLength);
          Read(_ScopelessCmd[1], ItemLength);

          // Load RELATIVE filenames
          Read(ItemLength, SizeOf(Integer));
          SetLength(_DeclImplFileName, ItemLength);
          Read(_DeclImplFileName[1], ItemLength);
          _DeclImplFileName := ReplaceFirstStr(_DeclImplFileName, '%path%\', relativeto);

          // Load RELATIVE filenames
          Read(ItemLength, SizeOf(Integer));
          SetLength(_FileName, ItemLength);
          Read(_FileName[1], ItemLength);
          _FileName := ReplaceFirstStr(_FileName, '%path%\', relativeto);

          Read(ItemLength, SizeOf(Integer));
          SetLength(_InheritsFromIDs, ItemLength);
          Read(_InheritsFromIDs[1], ItemLength);

          Read(ItemLength, SizeOf(Integer));
          SetLength(_InheritsFromClasses, ItemLength);
          Read(_InheritsFromClasses[1], ItemLength);

          // don't bother to read/write those
          _Loaded := True;
          _Temporary := False;
          _InProject := False;
        end;
        fStatementList.Add(Statement);
      end;

      // read scanned files - cache contents
      Read(HowMany, SizeOf(Integer));
      for I := 0 to HowMany do begin

        // Load RELATIVE filenames
        Read(ItemLength, SizeOf(Integer));
        SetLength(relative, ItemLength);
        Read(relative[1], ItemLength);
        relative := ReplaceFirstStr(relative, '%path%\', relativeto);

        fScannedFiles.Add(relative);
        fCacheContents.Add(relative);
      end;

      // read includes info for each scanned file
      Read(HowMany, SizeOf(Integer));
      for I := 0 to HowMany do begin
        P := New(PFileIncludes);

        // Load RELATIVE filenames
        Read(ItemLength, SizeOf(Integer));
        SetLength(relative, ItemLength);
        Read(relative[1], ItemLength);
        relative := ReplaceFirstStr(relative, '%path%\', relativeto);
        P^.BaseFile := relative;

        // Load RELATIVE filenames
        Read(ItemLength, SizeOf(Integer));
        SetLength(relative, ItemLength);
        Read(relative[1], ItemLength);
        relative := ReplaceFirstStr(relative, '%path%\', relativeto);
        P^.IncludeFiles := relative;

        fIncludesList.Add(P);
      end;
    finally
      Free; // cache file TMemoryStream, not 'this'
      fNextID := fStatementList.Count;
      fBaseIndex := fStatementList.Count;
      fScannedBaseIndex := fCacheContents.Count;
      PostProcessInheritance;
    end;
end;

procedure TCppParser.PostProcessInheritance;
var
  C, I, I1, I2: integer;
  sl: TStrings;
  S: AnsiString;
begin
  sl := TStringList.Create;
  try
    // For all classes that inherit from anything...
    for I := fBaseIndex to fStatementList.Count - 1 do begin
      if PStatement(fStatementList[I])^._Kind = skClass then begin
        if PStatement(fStatementList[I])^._InheritsFromClasses <> '' then begin

          // Assemble a stringlist of statements (in string form) it inherits from
          sl.CommaText := PStatement(fStatementList[I])^._InheritsFromClasses;
          S := '';
          C := 0;

          // Find the corresponding IDs and store them in S (separated by commas)
          for I1 := fStatementList.Count - 1 downto 0 do
            for I2 := 0 to sl.Count - 1 do
              if PStatement(fStatementList[I1])^._Kind = skClass then
                if SameStr(sl[I2], PStatement(fStatementList[I1])^._ScopelessCmd) then begin
                  S := S + IntToStr(PStatement(fStatementList[I1])^._ID) + ',';
                  Inc(C);
                  if C = sl.Count then // found all classes?
                    Break;
                end;

          // If the process completed, replace string list by ID list, and cut off ending ','
          if C = sl.Count then begin
            PStatement(fStatementList[I])^._InheritsFromClasses := '';
            if S <> '' then
              S := Copy(S, 1, Length(S) - 1);
            PStatement(fStatementList[I])^._InheritsFromIDs := S;
          end;
        end;
      end;
    end;
  finally
    sl.Free;
  end;
end;

procedure TCppParser.ReProcessInheritance;
var
  I, I1: integer;
  sl: TStringList;
begin

  // after reparsing a file, we have to reprocess inheritance,
  // because by invalidating the file, we might have deleted
  // some IDs that were inherited by other, valid, statements.
  // we need to re-adjust the IDs now...

  if fInvalidatedIDs.Count = 0 then
    Exit;
  sl := TStringList.Create;
  try
    sl.Sorted := True;
    sl.Duplicates := dupIgnore;
    for I := fBaseIndex to fStatementList.Count - 1 do
      for I1 := 0 to fInvalidatedIDs.Count - 1 do
        if Pos(IntToStr(fInvalidatedIDs[I1]), PStatement(Statements[I])^._InheritsFromIDs) > 0 then
          sl.Add(PStatement(Statements[I])^._FileName);
    for I := 0 to sl.Count - 1 do
      ReParseFile(sl[I], fProjectFiles.IndexOf(sl[I]) <> -1, False, False);
  finally
    sl.Free;
  end;
end;

function TCppParser.SuggestMemberInsertionLine(ParentID: integer; Scope: TStatementClassScope; var AddScopeStr:
  boolean): integer;
var
  I: integer;
  maxInScope: integer;
  maxInGeneral: integer;
begin
  // this function searches in the statements list for statements with
  // a specific _ParentID, and returns the suggested line in file for insertion
  // of a new var/method of the specified class scope. The good thing is that
  // if there is no var/method by that scope, it still returns the suggested
  // line for insertion (the last line in the class).
  maxInScope := -1;
  maxInGeneral := -1;
  for I := 0 to fStatementList.Count - 1 do
    if PStatement(fStatementList[I])^._ParentID = ParentID then begin
      if PStatement(fStatementList[I])^._IsDeclaration then begin
        if PStatement(fStatementList[I])^._Line > maxInGeneral then
          maxInGeneral := PStatement(fStatementList[I])^._Line;
        if PStatement(fStatementList[I])^._ClassScope = scope then
          if PStatement(fStatementList[I])^._Line > maxInScope then
            maxInScope := PStatement(fStatementList[I])^._Line;
      end else begin
        if PStatement(fStatementList[I])^._DeclImplLine > maxInGeneral then
          maxInGeneral := PStatement(fStatementList[I])^._Line;
        if PStatement(fStatementList[I])^._ClassScope = scope then
          if PStatement(fStatementList[I])^._DeclImplLine > maxInScope then
            maxInScope := PStatement(fStatementList[I])^._DeclImplLine;
      end;
    end;
  if maxInScope = -1 then begin
    AddScopeStr := True;
    Result := maxInGeneral;
  end else begin
    AddScopeStr := False;
    Result := maxInScope;
  end;
end;

function TCppParser.GetDeclarationFileName(Statement: PStatement): AnsiString;
begin
  if Statement^._IsDeclaration then
    Result := Statement^._FileName
  else
    Result := Statement^._DeclImplFileName;
end;

function TCppParser.GetDeclarationLine(Statement: PStatement): integer;
begin
  if Statement^._IsDeclaration then
    Result := Statement^._Line
  else
    Result := Statement^._DeclImplLine;
end;

function TCppParser.GetImplementationFileName(Statement: PStatement): AnsiString;
begin
  if Statement^._IsDeclaration then
    Result := Statement^._DeclImplFileName
  else
    Result := Statement^._FileName;
end;

function TCppParser.GetImplementationLine(Statement: PStatement): integer;
begin
  if Statement^._IsDeclaration then
    Result := Statement^._DeclImplLine
  else
    Result := Statement^._Line;
end;

procedure TCppParser.GetClassesList(var List: TStrings);
var
  I: integer;
begin
  // fills List with a list of all the known classes
  List.Clear;
  for I := fStatementList.Count - 1 downto 0 do
    if PStatement(fStatementList[I])^._Kind = skClass then
      List.AddObject(PStatement(fStatementList[I])^._ScopeCmd, Pointer(Statements[I]));
end;

function TCppParser.IndexOfStatement(ID: integer): integer;
var
  I: integer;
begin
  // Always perform a forward search. Otherwise multiple struct definitions will NOT work.
  for I := 0 to fStatementList.Count - 1 do
    if PStatement(fStatementList[I])^._ID = ID then begin
      Result := I;
      Exit;
    end;
  Result := -1;
end;

function TCppParser.Locate(const Full: AnsiString; WithScope: boolean): PStatement;
var
  I: integer;
begin
  Result := nil;

  // Evaluate if outside of loop (ONCE, not tens of thousands of times)
  if WithScope then begin
    for I := 0 to fStatementList.Count - 1 do begin
      if SameStr(Full, PStatement(fStatementList[I])^._ScopeCmd) then begin
        Result := PStatement(fStatementList[I]);
        Break;
      end;
    end;
  end else begin
    for I := 0 to fStatementList.Count - 1 do begin
      if SameStr(Full, PStatement(fStatementList[I])^._ScopelessCmd) then begin
        Result := PStatement(fStatementList[I]);
        Break;
      end;
    end;
  end;
end;

function TCppParser.FindAndScanBlockAt(const Filename: AnsiString; Row: integer; Stream: TMemoryStream): PStatement;
  function GetFuncStartLine(Index, StartLine: integer): integer;
  begin
    Result := Index;

    // Keep advancing until we find the start line
    while Index < fTokenizer.Tokens.Count do begin
      if fTokenizer[Index]^.Line = StartLine then begin

        // Find the opening brace from here
        while (Index < fTokenizer.Tokens.Count) and (fTokenizer[Index]^.Text[1] <> '{') do
          Inc(Index);

        // Found it before the file stopped? Yay
        if (Index < fTokenizer.Tokens.Count) and (fTokenizer[Index]^.Text[1] = '{') then begin
          Result := Index;
          Break;
        end;
      end;
      Inc(Index);
    end;
  end;
  function GetFuncEndLine(Index: integer): integer; // basic brace skipper
  var
    Level: integer;
  begin
    Level := 0; // when this goes negative, we 're there (we have skipped the opening brace already)
    while (Index < fTokenizer.Tokens.Count) and (Level >= 0) do begin
      if fTokenizer[Index]^.Text[1] = '{' then
        Inc(Level)
      else if fTokenizer[Index]^.Text[1] = '}' then
        Dec(Level);
      Inc(Index);
    end;
    Result := Index;
  end;
var
  I, ClosestIndex, ClosestLine, ParentIndex, ParentID, FuncStartIndex, FuncEndIndex: integer;
  Statement: PStatement;
begin
  Result := nil;

  if (fTokenizer = nil) or (fPreprocessor = nil) then
    Exit;

  // finds the function in the specified filename that contains the line Row, and parses it...
  DeleteTemporaries;
  ClosestLine := -1;
  ClosestIndex := -1;

  // Search for the current function/class
  for I := 0 to fStatementList.Count - 1 do begin
    Statement := fStatementList[I];

    // For any type, accept declarations
    if Statement^._Kind in [skClass, skFunction, skConstructor, skDestructor] then begin
      if (Statement^._Kind = skClass) and SameText(Statement^._FileName, Filename) then begin
        if (Statement^._Line <= Row) and (Statement^._Line > ClosestLine) then begin
          ClosestIndex := I;
          ClosestLine := Statement^._Line;
        end;
      end;

      // For classes, don't accept definitions
      if (Statement^._Kind <> skClass) and SameText(Statement^._DeclImplFileName, Filename) then begin
        if (Statement^._DeclImplLine <= Row) and (Statement^._DeclImplLine > ClosestLine) then begin
          ClosestIndex := I;
          ClosestLine := Statement^._DeclImplLine;
        end;
      end;
    end;
  end;

  // We have found the function or class body we are in
  if ClosestIndex <> -1 then begin

    // If we're not in a class, but in a member function, get the parent of the member function
    if PStatement(fStatementList[ClosestIndex])^._Kind = skClass then begin
      ParentID := PStatement(fStatementList[ClosestIndex])^._ID;
        // parent contains class of function we're in or class itself we're in
      ParentIndex := ClosestIndex;
    end else begin
      ParentID := PStatement(fStatementList[ClosestIndex])^._ParentID;
      ParentIndex := IndexOfStatement(ParentID);
    end;

    // The result is the class the function belongs to or the class body we're in
    if ParentIndex <> -1 then
      Result := fStatementList[ParentIndex]; // return the parent statement

    // Scan the function block if we're inside a function
    if PStatement(fStatementList[ClosestIndex])^._Kind in [skFunction, skConstructor, skDestructor] then begin

      // Preprocess the stream that contains the latest version of the current file (not on disk)
      fPreprocessor.SetIncludesList(fIncludesList);
      fPreprocessor.SetIncludePaths(fIncludePaths);
      fPreprocessor.SetScannedFileList(fScannedFiles);
      fPreprocessor.SetScanOptions(fParseGlobalHeaders, fParseLocalHeaders);
      fPreprocessor.PreProcessStream(FileName, Stream);

      // Tokenize the stream so we can find the start and end of the function body
      fTokenizer.TokenizeBuffer(PAnsiChar(fPreprocessor.Result));

      // Find start of the function block and start from the opening brace
      FuncStartIndex := GetFuncStartLine(0, ClosestLine);

      // Now find the end of the function block and check that the Row is still in scope
      FuncEndIndex := GetFuncEndLine(FuncStartIndex + 1);

      // if we 're past the end of function or class body, we are not in the scope...
      if (Row > fTokenizer[FuncEndIndex - 1]^.Line) or (Row < fTokenizer[FuncStartIndex]^.Line) then begin

        // Try again with the one item of the definition/declaraton pair
        ClosestLine := PStatement(fStatementList[ClosestIndex])^._DeclImplLine;
        FuncStartIndex := GetFuncStartLine(0, ClosestLine);
        FuncEndIndex := GetFuncEndLine(FuncStartIndex + 1);

        // Still failed? Quit
        if Row > fTokenizer[FuncEndIndex - 1]^.Line then
          Exit;
      end;

      // Set current file manually because we aren't parsing whole files
      fCurrentFile := Filename;
      fIsSystemHeader := IsSystemHeaderFile(fCurrentFile);
      fIsProjectFile := fProjectFiles.IndexOf(fCurrentFile) <> -1;
      fIsHeader := IsHfile(fCurrentFile);

      // We've found the function body. Scan it
      fIndex := FuncStartIndex;
      fLevel := 0;
      fLastID := -1;
      fClassScope := scsNone;
      fLaterScanning := True;
      repeat
        if fTokenizer[fIndex]^.Text[1] = '{' then begin
          Inc(fLevel);
          Inc(fIndex);
        end else if fTokenizer[fIndex]^.Text[1] = '}' then begin
          Dec(fLevel);
          Inc(fIndex);
          if fLevel < 0 then
            Break; // we've gone out of scope
        end else if CheckForPreprocessor then begin
          HandlePreprocessor;
        end else if CheckForKeyword then begin
          HandleKeyword;
        end else if CheckForEnum then begin
          HandleEnum;
        end else if CheckForVar then begin
          HandleVar;
        end else
          Inc(fIndex);

        CheckForSkipStatement;
      until (fIndex >= fTokenizer.Tokens.Count) or (fIndex >= FuncEndIndex);
      // add the all-important "this" pointer as a local variable
      if ParentIndex <> -1 then begin
        AddStatement(
          -1,
          ParentID,
          Filename,
          PStatement(fStatementList[ParentIndex])^._ScopeCmd + '* this',
          PStatement(fStatementList[ParentIndex])^._ScopeCmd + '*',
          'this',
          '',
          PStatement(fStatementList[ClosestIndex])^._DeclImplLine + 1,
          skVariable,
          ssClassLocal,
          scsPrivate,
          False,
          True,
          False);
      end;

      // Try to use arglist which includes names (implementation, not declaration)
      ScanMethodArgs(
        PStatement(fStatementList[ClosestIndex])^._Args,
        PStatement(fStatementList[ClosestIndex])^._DeclImplFileName,
        PStatement(fStatementList[ClosestIndex])^._DeclImplLine,
        PStatement(fStatementList[ClosestIndex])^._ParentID);

      // Everything scanned before this point should be removed
      fLaterScanning := False;
    end;
  end;
end;

function TCppParser.GetClass(const Phrase: AnsiString): AnsiString;
var
  I, FirstOp: integer;
begin
  // Obtain stuff before first operator
  FirstOp := Length(Phrase) + 1;
  for I := 1 to Length(Phrase) - 1 do begin
    if (phrase[i] = '-') and (Phrase[i + 1] = '>') then begin
      FirstOp := I;
      break;
    end else if (Phrase[i] = ':') and (Phrase[i + 1] = ':') then begin
      FirstOp := I;
      break;
    end else if (Phrase[i] = '.') then begin
      FirstOp := I;
      break;
    end;
  end;

  Result := Copy(Phrase, 1, FirstOp - 1);
end;

function TCppParser.GetMember(const Phrase: AnsiString): AnsiString;
var
  FirstOp, SecondOp, I: integer;
begin
  // Obtain stuff after first operator
  FirstOp := 0;
  for I := 1 to Length(Phrase) - 1 do begin
    if (phrase[i] = '-') and (Phrase[i + 1] = '>') then begin
      FirstOp := I + 2;
      break;
    end else if (Phrase[i] = ':') and (Phrase[i + 1] = ':') then begin
      FirstOp := I + 2;
      break;
    end else if (Phrase[i] = '.') then begin
      FirstOp := I + 1;
      break;
    end;
  end;

  if FirstOp = 0 then begin
    Result := '';
    Exit;
  end;

  // ... and before second op, if there is one
  SecondOp := 0;
  for I := firstop to Length(Phrase) - 1 do begin
    if (phrase[i] = '-') and (Phrase[i + 1] = '>') then begin
      SecondOp := I;
      break;
    end else if (Phrase[i] = ':') and (Phrase[i + 1] = ':') then begin
      SecondOp := I;
      break;
    end else if (Phrase[i] = '.') then begin
      SecondOp := I;
      break;
    end;
  end;

  if SecondOp = 0 then
    Result := Copy(Phrase, FirstOp, MaxInt)
  else
    Result := Copy(Phrase, FirstOp, SecondOp - FirstOp);
end;

function TCppParser.GetOperator(const Phrase: AnsiString): AnsiString;
var
  I: integer;
begin
  Result := '';
  // Find first operator
  for I := 1 to Length(Phrase) - 1 do begin
    if (phrase[i] = '-') and (Phrase[i + 1] = '>') then begin
      Result := '->';
      break;
    end else if (Phrase[i] = ':') and (Phrase[i + 1] = ':') then begin
      Result := '::';
      break;
    end else if (Phrase[i] = '.') then begin
      Result := '.';
      break;
    end;
  end;
end;

function TCppParser.FindLastOperator(const Phrase: AnsiString): integer;
var
  I: integer;
begin

  I := Length(phrase);

  // Obtain stuff after first operator
  while I > 0 do begin
    if (phrase[i + 1] = '>') and (phrase[i] = '-') then begin
      Result := i;
      Exit;
    end else if (phrase[i + 1] = ':') and (phrase[i] = ':') then begin
      Result := i;
      Exit;
    end else if (phrase[i] = '.') then begin
      Result := i;
      Exit;
    end;
    Dec(i);
  end;
  Result := 0;
end;

procedure TCppParser.GetInheritanceIDs(st: PStatement; List: TIntList);
var
  sl: TStringList;
  I, tmp, donecount: integer;
begin
  if not Assigned(st) then
    Exit;

  // Walk the whole inheritance tree, all branches, to the bottom
  sl := TStringList.Create;
  try
    // Add first list
    sl.CommaText := st^._InheritsFromIDs;
    for I := 0 to sl.Count - 1 do
      List.Add(StrToIntDef(sl[i], -1));
    donecount := 0;

    // then process inheritance of new items
    while donecount < List.Count do begin
      tmp := IndexOfStatement(List[donecount]); // slooow
      if tmp <> -1 then begin

        // Add inheritance of inherited
        sl.CommaText := PStatement(fStatementList[tmp])^._InheritsFromIDs;
        for I := 0 to sl.Count - 1 do
          List.Add(StrToIntDef(sl[i], -1));
      end;
      Inc(donecount);
    end;
  finally
    sl.Free;
  end;
end;

procedure TCppParser.GetInheritanceIDs(index: Integer; List: TIntList);
begin
  if index <> -1 then
    GetInheritanceIDs(PStatement(fStatementList[index]), List);
end;

function TCppParser.PrettyPrintStatement(st: PStatement): AnsiString;
begin
  if st^._ClassScope <> scsNone then begin
    if st^._Type <> '' then begin
      result := StatementClassScopeStr(st^._ClassScope) + ' ' + st^._Type + ' ' + st^._ScopeCmd + st^._Args;
    end else begin
      result := StatementClassScopeStr(st^._ClassScope) + ' ' + st^._ScopeCmd + st^._Args;
    end;
  end else begin
    result := st^._FullText;
  end;
end;

procedure TCppParser.FillListOfFunctions(const Full: AnsiString; List: TStringList);
var
  I: integer;
  st: PStatement;
begin
  List.Clear;

  // Tweaked for specific use by CodeToolTip. Also avoids AnsiString compares whenever possible
  for I := fStatementList.Count - 1 downto 0 do begin // Prefer user declared names
    st := PStatement(fStatementList[I]);
    if st^._Kind in [skFunction, skConstructor, skDestructor] then begin

      // Also add Win32 Ansi/Wide variants...
      if SameStr(Full, st^._ScopelessCmd) or
        SameStr(Full + 'A', st^._ScopelessCmd) or
        SameStr(Full + 'W', st^._ScopelessCmd) then begin
        List.Add(PrettyPrintStatement(st));
      end;
    end;
  end;
end;

function TCppParser.FindTypeStatementOf(const aType: AnsiString; CurrentClass: PStatement; MaxSearchIndex: integer =
  -1): PStatement;
var
  I, position: integer;
  s: AnsiString;
begin
  // Remove pointer stuff from type
  s := aType; // 'Type' is a keyword
  position := Length(s);
  while (position > 0) and (s[position] in ['*', '&']) do
    Dec(position);
  if position <> Length(s) then
    Delete(s, position + 1, Length(s) - 1);

  // Strip template stuff
  position := Pos('<', s);
  if position > 0 then
    Delete(s, position, MaxInt);

  // Use last word only (strip 'const', 'static', etc)
  position := LastPos(' ', s);
  if position > 0 then
    Delete(s, 1, position);

  // Prevent infinite loops for typedef recursion
  if MaxSearchIndex = -1 then
    MaxSearchIndex := fStatementList.Count - 1;

  for I := MaxSearchIndex downto 0 do begin
    if PStatement(fStatementList[I])^._ParentID = -1 then begin // TODO: type definitions can have scope too
      if PStatement(fStatementList[I])^._Kind = skClass then begin // these have type 'class'
        // We have found the statement of the type directly
        if SameStr(PStatement(fStatementList[I])^._ScopelessCmd, s) then begin
          result := fStatementList[I]; // 'class foo'
          Exit;
        end;
      end else if PStatement(fStatementList[I])^._Kind in [skVariable, skFunction] then begin
        if SameStr(PStatement(fStatementList[I])^._Type, '') then begin
          // We have found a variable with the same name, search for type
          if SameStr(PStatement(fStatementList[I])^._ScopelessCmd, s) then begin
            result := fStatementList[I];
            Exit;
          end;
        end;
      end else if PStatement(fStatementList[I])^._Kind = skTypedef then begin
        // We have found a variable with the same name, search for type
        if SameStr(PStatement(fStatementList[I])^._ScopelessCmd, s) then begin
          result := FindTypeStatementOf(PStatement(fStatementList[I])^._Type, CurrentClass, I - 1);
          if result = nil then // found end of typedef trail, return result
            result := fStatementList[I];
          Exit;
        end;
      end;
    end;
  end;

  Result := nil;
end;

function TCppParser.FindVariableOf(const Phrase: AnsiString; CurrentClass: PStatement): PStatement;
var
  I: integer;
  inheritanceIDs: TIntList;
begin
  // First, assume the parentword is a type (type names have more priority than variables)
  result := FindTypeStatementOf(Phrase, CurrentClass);
  if Assigned(result) then
    Exit;

  // Check local variables
  for I := fStatementList.Count - 1 downto 0 do begin
    // Class members
    if (PStatement(fStatementList[I])^._Scope = ssClassLocal) and Assigned(CurrentClass) and
      (PStatement(fStatementList[I])^._ParentID = CurrentClass^._ID) then begin
      if SameStr(PStatement(fStatementList[I])^._ScopelessCmd, Phrase) then begin
        result := fStatementList[I];
        Exit;
      end;
      // Local scope variables (includes function arguments)
    end else if (PStatement(fStatementList[I])^._Scope = ssLocal) then begin
      if SameStr(PStatement(fStatementList[I])^._ScopelessCmd, Phrase) then begin
        result := fStatementList[I];
        Exit;
      end;
    end;
  end;

  // Then, assume the variable belongs to the current scope/class, if there is one
  if Assigned(CurrentClass) then begin
    inheritanceIDs := TIntList.Create;
    try
      // Accept inherited stuff from current class too
      GetInheritanceIDs(CurrentClass, inheritanceIDs); // slow

      // Start scanning backwards, because owner data is found there
      for I := fStatementList.Count - 1 downto 0 do begin
        if PStatement(fStatementList[I])^._ParentID = CurrentClass^._ID then begin
          if SameStr(PStatement(fStatementList[I])^._ScopelessCmd, Phrase) then begin
            result := fStatementList[I];
            Exit;
          end;
        end else if (inheritanceIDs.IndexOf(PStatement(fStatementList[I])^._ParentID) <> -1) then
          begin // try inheritance
          // hide private stuff?
          if SameStr(PStatement(fStatementList[I])^._ScopelessCmd, Phrase) then begin
            result := fStatementList[I];
            Exit;
          end;
        end;
      end;
    finally
      inheritanceIDs.Free;
    end;
  end;

  // What remains are globals. Just do a raw scan...
  for I := fStatementList.Count - 1 downto 0 do begin // prefer globals inside source files
    if PStatement(fStatementList[I])^._Scope = ssGlobal then begin
      if SameStr(PStatement(fStatementList[I])^._ScopelessCmd, Phrase) then begin
        result := fStatementList[I];
        Exit;
      end;
    end;
  end;

  Result := nil;
end;

function TCppParser.FindStatementOf(Phrase: AnsiString; CurrentClass: PStatement): PStatement;
var
  parenttype: PStatement;
  i: integer;
  parentword, memberword, operator: AnsiString;
    inheritanceIDs: TIntList; // reuse?
  begin

    // Get the FIRST class and member, surrounding the FIRST operator
    parentword := GetClass(Phrase);
    operator := GetOperator(Phrase);
      memberword := GetMember(Phrase);

      // Determine which variable we are dealing with
      result := FindVariableOf(parentword, CurrentClass);
      if not Assigned(result) then
        Exit;

      // Then determine which type it has (so we can use it as a parent ID)
      if result^._Kind = skClass then begin // already found type
        parenttype := result;
      end else begin
        parenttype := FindTypeStatementOf(result^._Type, CurrentClass);
        if not Assigned(parenttype) then
          Exit;
      end;

      inheritanceIDs := TIntList.Create;
    try
      // Walk the chain of operators
      while (memberword <> '') do begin

        inheritanceIDs.Clear;
        GetInheritanceIDs(parenttype, inheritanceIDs);

        // Add members of this type
        for I := 0 to fStatementList.Count - 1 do begin
          if PStatement(fStatementList[I])^._ParentID = parenttype^._ID then begin
            if SameStr(PStatement(fStatementList[I])^._ScopelessCmd, memberword) then begin
              result := fStatementList[I];
              break; // there can be only one with an equal name
            end;
          end else if (inheritanceIDs.IndexOf(PStatement(fStatementList[I])^._ParentID) <> -1) then
            begin // try inheritance
            // hide private stuff?
            if SameStr(PStatement(fStatementList[I])^._ScopelessCmd, memberword) then begin
              result := fStatementList[I];
              break;
            end;
          end;
        end;

        // next operator
        Delete(phrase, 1, Length(parentword) + Length(operator));

        // Get the NEXT member, surrounding the next operator
        parentword := GetClass(Phrase);
        operator := GetOperator(Phrase);
          memberword := GetMember(Phrase);

          // Don't bother finding types
          if memberword = '' then
            break;

          // At this point, we have a list of statements that conform to the a(operator)b demand.
          // Now make these statements "a(operator)b" the parents, so we can use them as filters again
          parenttype := FindTypeStatementOf(result^._Type, CurrentClass);
          if not Assigned(parenttype) then
            Exit;
      end;
    finally
      inheritanceIDs.Free;
    end;
  end;

  function TCppParser.FindStatementOf(FileName, Phrase: AnsiString; Row: integer; Stream: TMemoryStream): PStatement;
  begin
    Result := FindStatementOf(Phrase, FindAndScanBlockAt(FileName, Row, Stream));
  end;

  procedure TCppParser.DeleteTemporaries;
  var
    I: integer;
  begin
    for I := fStatementList.Count - 1 downto fBaseIndex do begin
      if PStatement(fStatementList[I])^._Temporary then begin
        Dispose(PStatement(fStatementList[I]));
        fStatementList.Delete(I);
      end;
    end;
  end;

  procedure TCppParser.ScanMethodArgs(const ArgStr: AnsiString; const Filename: AnsiString; Line, ClassID: integer);
  var
    I, ParamStart, SpacePos, BracePos: integer;
    S: AnsiString;
  begin

    // Split up argument string by ,
    I := 2; // assume it starts with ( and ends with )
    ParamStart := I;

    while I <= Length(ArgStr) do begin
      if (ArgStr[i] = ',') or ((I = Length(ArgStr)) and (ArgStr[i] = ')')) then begin

        // We've found "int* a" for example
        S := Trim(Copy(ArgStr, ParamStart, I - ParamStart));

        // Can be a function pointer. If so, scan after last )
        BracePos := LastPos(')', S);
        if BracePos > 0 then // it's a function pointer...
          SpacePos := Pos(' ', Copy(S, BracePos, MaxInt)) // start search at brace
        else
          SpacePos := Pos(' ', S); // Cut up at last space

        if SpacePos > 0 then begin
          AddStatement(
            -1,
            ClassID,
            Filename,
            S, // 'int* a'
            Copy(S, 1, SpacePos - 1), // 'int*'
            Copy(S, SpacePos + 1, MaxInt), // a
            '',
            Line,
            skVariable,
            ssLocal,
            scsPrivate,
            False, // not visible in class browser
            True, // allow duplicates
            False);
        end;

        ParamStart := I + 1; // step over ,
      end;
      Inc(I);
    end;
  end;

  function TCppParser.FindFileIncludes(const Filename: AnsiString; DeleteIt: boolean): PFileIncludes;
  var
    I: integer;
  begin
    Result := nil;
    for I := 0 to fIncludesList.Count - 1 do
      if SameText(PFileIncludes(fIncludesList[I])^.BaseFile, Filename) then begin
        Result := PFileIncludes(fIncludesList[I]);
        if DeleteIt then
          fIncludesList.Delete(I);
        Break;
      end;
  end;

  function TCppParser.IsCfile(const Filename: AnsiString): boolean;
  begin
    result := cbutils.IsCfile(FileName);
  end;

  function TCppParser.IsHfile(const Filename: AnsiString): boolean;
  begin
    result := cbutils.IsHfile(Filename);
  end;

  procedure TCppParser.GetSourcePair(const FName: AnsiString; var CFile, HFile: AnsiString);
  begin
    cbutils.GetSourcePair(FName, CFile, HFile);
  end;

  procedure TCppParser.GetFileIncludes(const Filename: AnsiString; var List: TStringList);

    procedure RecursiveFind(const FileName: AnsiString);
    var
      I: integer;
      P: PFileIncludes;
      sl: TStrings;
    begin
      if FileName = '' then
        Exit;
      List.Add(FileName);

      // Find the files this file includes
      P := FindFileIncludes(FileName);
      if Assigned(P) then begin

        // recursively search included files
        sl := TStringList.Create;
        try
          // For each file this file includes, perform the same trick
          sl.CommaText := P^.IncludeFiles;
          for I := 0 to sl.Count - 2 do // Last one is always an empty item
            if FastIndexOf(List, sl[I]) = -1 then
              RecursiveFind(sl[I]);
        finally
          sl.Free;
        end;
      end;
    end;
  begin
    List.Clear;
    List.Sorted := false;
    RecursiveFind(Filename);
  end;

end.

