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
  Dialogs, Windows, Classes, SysUtils, StrUtils, ComCtrls, U_IntList, CppTokenizer;
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

  TStatementScope = (ssGlobal, ssLocal, ssClassLocal);
  TStatementClassScope = (scsPublic, scsPublished, scsPrivate, scsProtected, scsNone);

  TLogStatementEvent = procedure(Sender: TObject; Msg: string) of object;

  PStatement = ^TStatement;
  TStatement = record
    _ID: integer;
    _ParentID: integer;
    _FullText: string;
    _Type: string;
    _Command: string;
    _Args: string;
    _MethodArgs: string;
    _ScopelessCmd: string;
    _ScopeCmd: string;
    _Kind: TStatementKind;
    _InheritsFromIDs: string; // list of inheriting IDs, in comma-separated string form
    _InheritsFromClasses: string; // list of inheriting class names, in comma-separated string form
    _Scope: TStatementScope;
    _ClassScope: TStatementClassScope;
    _IsDeclaration: boolean;
    _DeclImplLine: integer;
    _Line: integer;
    _DeclImplFileName: string;
    _FileName: string;
    _Visible: boolean;
    _NoCompletion: boolean;
    _Valid: boolean;
    _Temporary: boolean;
    _Loaded: boolean;
    _InProject: boolean;
  end;

  POutstandingTypedef = ^TOutstandingTypedef;
  TOutstandingTypedef = packed record
    _WaitForTypedef: string;
    _ExistingID: integer;
  end;

  PIncludesRec = ^TIncludesRec;
  TIncludesRec = packed record
    BaseFile: string;
    IncludeFiles: string;
  end;

  TCppParser = class(TComponent)
  private
    fEnabled: boolean;
    fInClass: integer;
    fNextID: integer;
    fBaseIndex: integer;
    fLevel: integer;
    fIndex: integer;
    fIsHeader: boolean;
    fCurrentFile: TFileName;
    fLastID: integer;
    fLastStatementKind: TStatementKind;
    fCurrentClass: TIntList;
    fSkipList: TIntList;
    fCurrentClassLevel: TIntList;
    fClassScope: TStatementClassScope;
    fStatementList: TList;
    fOutstandingTypedefs: TList;
    fIncludesList: TList;
    fTokenizer: TCppTokenizer;
    fVisible: boolean;
    fIncludePaths: TStringList;
    fProjectIncludePaths: TStringList;
    fProjectFiles: TStringList;
    fFilesToScan: TStrings;
    fScannedFiles: TStringList;
    fFileIncludes: TStringList;
    fCacheContents: TStringList;
    fParseLocalHeaders: boolean;
    fParseGlobalHeaders: boolean;
    fReparsing: boolean;
    fProjectDir: string;
    fOnLogStatement: TLogStatementEvent;
    fOnBusy: TNotifyEvent;
    fOnUpdate: TNotifyEvent;
    fOnFileProgress: TProgressEvent;
    fOnTotalProgress: TProgressEvent;
    fLogStatements: boolean;
    fLaterScanning: boolean;
    fThisPointerID: integer;
    fOnStartParsing: TNotifyEvent;
    fOnEndParsing: TNotifyEvent;
    fIsProjectFile: boolean;
    fInvalidatedIDs: TIntList;
    function AddStatement(ID,
      ParentID: integer;
      Filename: TFileName;
      FullText,
      StType,
      StCommand,
      StArgs: string;
      Line: integer;
      Kind: TStatementKind;
      Scope: TStatementScope;
      ClassScope: TStatementClassScope;
      VisibleStatement: boolean = True;
      AllowDuplicate: boolean = True;
      IsDeclaration: boolean = False;
      IsValid: boolean = True): integer;
    procedure InvalidateFile(FileName: TFileName);
    function IsGlobalFile(Value: string): boolean;
    function GetClassID(Value: string; Kind: TStatementKind): integer;
    procedure ClearOutstandingTypedefs;
    function CheckForOutstandingTypedef(Value: string): integer;
    procedure AddToOutstandingTypedefs(Value: string; ID: integer);
    function GetCurrentClass: integer;
    procedure SetInheritance(Index: integer);
    procedure SetCurrentClass(ID: integer);
    procedure RemoveCurrentClass;
    procedure CheckForSkipStatement;
    function SkipBraces(StartAt: integer): integer;
    function CheckForKeyword: boolean;
    function CheckForMember: boolean;
    function CheckForTypedef: boolean;
    function CheckForTypedefStruct: boolean;
    function CheckForStructs: boolean;
    function CheckForTemplate: boolean;
    function CheckForUnion: boolean;
    function CheckForMethod: boolean;
    function CheckForScope: boolean;
    function CheckForPreprocessor: boolean;
    function CheckForVar: boolean;
    function CheckForEnum: boolean;
    function GetScope: TStatementScope;
    procedure HandleMember;
    procedure HandleTemplate;
    procedure HandleUnion;
    procedure HandleOtherTypedefs;
    procedure HandleStructs(IsTypedef: boolean = False);
    procedure HandleMethod;
    function ScanMethodArgs(ArgStr: string; AddTemps: boolean; Filename: string; Line, ClassID: integer): string;
    procedure HandleScope;
    procedure HandlePreprocessor;
    procedure HandleKeyword;
    procedure HandleVar;
    procedure HandleEnum;
    function HandleStatement: boolean;
    procedure Parse(FileName: TFileName; IsVisible: boolean; ManualUpdate: boolean = False; processInh: boolean = True); overload;
    procedure DeleteTemporaries;
    function FindIncludeRec(Filename: string; DeleteIt: boolean = False): PIncludesRec;
  public
    function GetFileIncludes(Filename: string): string;
    function IsCfile(Filename: string): boolean;
    function IsHfile(Filename: string): boolean;
    procedure GetSourcePair(FName: string; var CFile, HFile: string);
    function GetImplementationLine(Statement: PStatement): integer;
    function GetImplementationFileName(Statement: PStatement): string;
    function GetDeclarationLine(Statement: PStatement): integer;
    function GetDeclarationFileName(Statement: PStatement): string;
    procedure GetClassesList(var List: TStrings);
    function SuggestMemberInsertionLine(ParentID: integer; Scope: TStatementClassScope; var AddScopeStr: boolean): integer;
    function GetFullFilename(Value: string): string;
    procedure Load(FileName: TFileName);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Parse(FileName: TFileName); overload;
    procedure ParseList;
    procedure ReParseFile(FileName: TFileName; InProject: boolean; OnlyIfNotParsed: boolean = False; UpdateView: boolean = True);
    function StatementKindStr(Value: TStatementKind): string;
    function StatementScopeStr(Value: TStatementScope): string;
    function StatementClassScopeStr(Value: TStatementClassScope): string;
    function CheckIfCommandExists(Value: string; Kind: TStatementKind; UseParent: boolean = False; ParID: integer = -1): integer;
    procedure Reset(KeepLoaded: boolean = True);
    procedure ClearIncludePaths;
    procedure ClearProjectIncludePaths;
    procedure AddIncludePath(Value: string);
    procedure AddProjectIncludePath(Value: string);
    procedure AddFileToScan(Value: string; InProject: boolean = False);
    procedure Save(FileName: TFileName);
    procedure ScanAndSaveGlobals(FileName: TFileName);
    procedure PostProcessInheritance;
    procedure ReProcessInheritance;
    function IndexOfStatement(ID: integer): integer;
    function Locate(Full: string; WithScope: boolean): PStatement;
    function FillListOf(Full: string; WithScope: boolean; List: TList): boolean;
    function FindAndScanBlockAt(Filename: string; Row: integer; Stream: TStream = nil): integer;
    function GetThisPointerID: integer;
  published
    property Enabled: boolean read fEnabled write fEnabled;
    property OnUpdate: TNotifyEvent read fOnUpdate write fOnUpdate;
    property OnBusy: TNotifyEvent read fOnBusy write fOnBusy;
    property OnLogStatement: TLogStatementEvent read fOnLogStatement write fOnLogStatement;
    property OnFileProgress: TProgressEvent read fOnFileProgress write fOnFileProgress;
    property OnTotalProgress: TProgressEvent read fOnTotalProgress write fOnTotalProgress;
    property Tokenizer: TCppTokenizer read fTokenizer write fTokenizer;
    property Statements: TList read fStatementList write fStatementList;
    property ParseLocalHeaders: boolean read fParseLocalHeaders write fParseLocalHeaders;
    property ParseGlobalHeaders: boolean read fParseGlobalHeaders write fParseGlobalHeaders;
    property ScannedFiles: TStringList read fScannedFiles;
    property CacheContents: TStringList read fCacheContents;
    property LogStatements: boolean read fLogStatements write fLogStatements;
    property ProjectDir: string read fProjectDir write fProjectDir;
    property OnStartParsing: TNotifyEvent read fOnStartParsing write fOnStartParsing;
    property OnEndParsing: TNotifyEvent read fOnEndParsing write fOnEndParsing;
    property FilesToScan: TStrings read fFilesToScan;
  end;

implementation

{$IFDEF LINUX}
uses Libc;
{$ENDIF}

//helper functions for cross platform compilation
{$IFDEF WIN32}
type
  myTickCount = cardinal;

function myGetTickCount: myTickCount;
begin
  result := GetTickCount;
end;

function myGetSecsSickTick(lastTick: myTickCount): integer;
begin
  result := Round((GetTickCount - lastTick) / 1000);
end;
{$ENDIF}
{$IFDEF LINUX}
type
  myTickCount = integer;

function myGetTickCount: myTickCount;
var
  buf: tms;
begin
  result := Libc.times(buf);
end;

function myGetSecsSickTick(lastTick: myTickCount): integer;
var
  buf: tms;
begin
  result := Round((Libc.times(buf) - lastTick) / CLOCKS_PER_SEC);
end;
{$ENDIF}

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

  fParseLocalHeaders := False;
  fParseGlobalHeaders := False;
  fReparsing := False;
  fLogStatements := False;

  fInClass := 0;
  fNextID := 0;
  fBaseIndex := 0;
  fThisPointerID := -1;
  fEnabled := True;
  fLaterScanning := False;
end;

destructor TCppParser.Destroy;
var
  i: Integer;
begin
  FreeAndNil(fInvalidatedIDs);
  FreeAndNil(fProjectFiles);

  for i := 0 to fIncludesList.Count -1 do
    Dispose(PIncludesRec(fIncludesList.Items[i]));
  FreeAndNil(fIncludesList);

  for i := 0 to fOutstandingTypedefs.Count -1 do
    Dispose(POutstandingTypedef(fOutstandingTypedefs.Items[i]));
  FreeAndNil(fOutstandingTypedefs);

  for i := 0 to fStatementList.Count -1 do
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

function TCppParser.StatementClassScopeStr(Value: TStatementClassScope): string;
begin
  case Value of
    scsPublic: Result := 'scsPublic';
    scsPublished: Result := 'scsPublished';
    scsPrivate: Result := 'scsPrivate';
    scsProtected: Result := 'scsProtected';
    scsNone: Result := 'scsNone';
  end;
end;

function TCppParser.StatementScopeStr(Value: TStatementScope): string;
begin
  case Value of
    ssGlobal: Result := 'ssGlobal';
    ssClassLocal: Result := 'ssClassLocal';
    ssLocal: Result := 'ssLocal';
  end;
end;

function TCppParser.StatementKindStr(Value: TStatementKind): string;
begin
  case Value of
    skPreprocessor: Result := 'Preprocessor';
    skVariable: Result := 'Variable';
    skConstructor: Result := 'Constructor';
    skDestructor: Result := 'Destructor';
    skFunction: Result := 'Function';
    skClass: Result := 'Class';
    skTypedef: Result := 'Typedef';
    skEnum: Result := 'Enum';
    skUnknown: Result := 'Unknown';
  end;
end;

function TCppParser.GetClassID(Value: string; Kind: TStatementKind): integer;
begin
  Result := CheckIfCommandExists(Value, Kind);
end;

procedure TCppParser.ClearOutstandingTypedefs;
begin
  while fOutstandingTypedefs.Count > 0 do
    if POutstandingTypedef(fOutstandingTypedefs[fOutstandingTypedefs.Count - 1]) <> nil then begin
      Dispose(POutstandingTypedef(fOutstandingTypedefs[fOutstandingTypedefs.Count - 1]));
      fOutstandingTypedefs.Delete(fOutstandingTypedefs.Count - 1);
    end
    else
      fOutstandingTypedefs.Delete(fOutstandingTypedefs.Count - 1);
  fOutstandingTypedefs.Clear;
end;

function TCppParser.CheckForOutstandingTypedef(Value: string): integer;
var
  I: integer;
begin
  I := 0;
  Result := -1;
  while I < fOutstandingTypedefs.Count do begin
    if POutstandingTypedef(fOutstandingTypedefs[I])^._WaitForTypedef = Value then begin
      Result := POutstandingTypedef(fOutstandingTypedefs[I])^._ExistingID;
      // free memory
      Dispose(POutstandingTypedef(fOutstandingTypedefs[I]));
      // delete it too!
      fOutstandingTypedefs.Delete(I);
      Break;
    end;
    Inc(I);
  end;
end;

procedure TCppParser.AddToOutstandingTypedefs(Value: string; ID: integer);
var
  ot: POutstandingTypedef;
begin
  ot := New(POutstandingTypedef);
  ot^._WaitForTypedef := Value;
  ot^._ExistingID := ID;
  fOutstandingTypedefs.Add(ot);
end;

function TCppParser.SkipBraces(StartAt: integer): integer;
var
  I1: integer;
begin
  if PToken(fTokenizer.Tokens[StartAt])^.Text[1] = '{' then begin
    I1 := 1;
    repeat
      Inc(StartAt);
      if PToken(fTokenizer.Tokens[StartAt])^.Text[1] = '{' then
        Inc(I1)
      else if PToken(fTokenizer.Tokens[StartAt])^.Text[1] = '}' then
        Dec(I1)
      else if PToken(fTokenizer.Tokens[StartAt])^.Text[1] = #0 then
        I1 := 0; // exit immediately
    until (I1 = 0);
  end;
  Result := StartAt;
end;

function TCppParser.CheckIfCommandExists(Value: string; Kind: TStatementKind; UseParent: boolean; ParID: integer): integer;
var
  I: integer;
  srch: set of TStatementKind;
  fH, fC: string;
begin
  Result := -1;
  srch := [];
  // if it is function, include the other types too
  if Kind in [skFunction, skConstructor, skDestructor] then
    srch := [skFunction, skConstructor, skDestructor]
  else
    Include(srch, Kind); // add to set
  GetSourcePair(fCurrentFile, fC, fH);
  // we do a backward search, because most possible is to be found near the end ;) - if it exists :(
  for I := fStatementList.Count - 1 downto fBaseIndex do begin
    if (PStatement(fStatementList[I])^._Kind in srch) and
      (PStatement(fStatementList[I])^._Command = Value) and
      ((not UseParent) or (UseParent and (PStatement(fStatementList[I])^._ParentID = ParID))) and
      ((AnsiCompareText(PStatement(fStatementList[I])^._FileName, fC) = 0) or // only if it belongs to the same file-pair
      (AnsiCompareText(PStatement(fStatementList[I])^._FileName, fH) = 0)) then begin
      Result := I;
      Break;
    end;
  end;
end;

function TCppParser.AddStatement(ID,
  ParentID: integer;
  Filename: TFileName;
  FullText,
  StType,
  StCommand,
  StArgs: string;
  Line: integer;
  Kind: TStatementKind;
  Scope: TStatementScope;
  ClassScope: TStatementClassScope;
  VisibleStatement: boolean = True;
  AllowDuplicate: boolean = True;
  IsDeclaration: boolean = False;
  IsValid: boolean = True): integer;
var
  Statement: PStatement;
  StScopeLess: string;
  ExistingID: integer;
  NewKind: TStatementKind;
begin
  // move '*', '&' to type rather than cmd (it's in the way for code-completion)
  while (Length(StCommand) > 0) and
    (stCommand[1] in ['*', '&']) do begin
    StType := StType + StCommand[1];
    StCommand := Copy(StCommand, 2, Length(StCommand) - 1);
  end;

  NewKind := Kind;

  // strip class prefix (e.g. MyClass::SomeFunc() = SomeFunc() )
  if Kind = skFunction then begin
    if AnsiPos('::', StCommand) > 0 then begin
      StScopeless := Copy(StCommand, AnsiPos('::', StCommand) + 2, Length(StCommand) - AnsiPos('::', StCommand) + 3);
      if AnsiCompareStr(Copy(StCommand, 1, AnsiPos('::', StCommand) - 1), StScopeless) = 0 then
        NewKind := skConstructor
      else if AnsiCompareStr('~' + Copy(StCommand, 1, AnsiPos('::', StCommand) - 1), StScopeless) = 0 then
        NewKind := skDestructor;
    end
    else
      StScopeless := StCommand;
  end
  else
    StScopeless := StCommand;

  //only search for certain kinds of statements
  if not AllowDuplicate {and not fIsHeader} then
    ExistingID := CheckIfCommandExists(StScopeless, Kind) //, True, ParentID)
  else
    ExistingID := -1;

  if (ExistingID <> -1) and (IsDeclaration <> PStatement(fStatementList[ExistingID])^._IsDeclaration) then begin // if it existed before, set the decl_impl index
    PStatement(fStatementList[ExistingID])^._DeclImplLine := Line;
    PStatement(fStatementList[ExistingID])^._DeclImplFileName := FileName;
    if (NewKind in [skConstructor, skDestructor]) and (PStatement(fStatementList[ExistingID])^._Kind = skFunction) then
      PStatement(fStatementList[ExistingID])^._Kind := NewKind;
    if (Kind = skFunction) and (AnsiPos('::', StCommand) > 0) then
      PStatement(fStatementList[ExistingID])^._ScopeCmd := StCommand;
    Result := ExistingID;
  end
  else begin // or else...
    Statement := New(PStatement);
    with Statement^ do begin
      if ID = -1 then
        _ID := fNextID //fStatementList.Count
      else
        _ID := ID;
      Result := _ID;
      _ParentID := ParentID;
      _FileName := FileName;
      _FullText := FullText;
      _ScopelessCmd := StScopeless;
      _ScopeCmd := StCommand;
      _Type := StType;
      _Command := StCommand;
      _Args := StArgs;
//      if Kind = skFunction then
//        _MethodArgs := ScanMethodArgs(StArgs, fLaterScanning, fCurrentFile, Line, ParentID)
//      else
      _MethodArgs := StArgs;
      _Line := Line;
      _Kind := NewKind;
      _Scope := Scope;
      _ClassScope := ClassScope;
      _IsDeclaration := IsDeclaration;
      _DeclImplLine := Line;
      _DeclImplFileName := FileName;
      _Visible := fVisible and VisibleStatement;
      _Valid := IsValid;
      _Loaded := False;
      _Temporary := fLaterScanning;
      _NoCompletion := (NewKind = skFunction) and AnsiStartsStr('operator', StScopeless);
      _InProject := fIsProjectFile;
    end;
    fStatementList.Add(Statement);
    Inc(fNextID);
  end;
end;

function TCppParser.GetCurrentClass: integer;
begin
  if fCurrentClass.Count > 0 then
    Result := fCurrentClass[fCurrentClass.Count - 1]
  else
    Result := -1;
end;

procedure TCppParser.SetCurrentClass(ID: integer);
begin
  if fCurrentClass.Count > 0 then begin
    if fCurrentClass[fCurrentClass.Count - 1] <> ID then begin
      fCurrentClass.Add(ID);
      fCurrentClassLevel.Add(fLevel);
      fClassScope := scsPublic;
    end;
  end
  else begin
    fCurrentClass.Add(ID);
    fCurrentClassLevel.Add(fLevel);
    fClassScope := scsPublic;
  end;
end;

procedure TCppParser.RemoveCurrentClass;
begin
  if fCurrentClassLevel.Count > 0 then
    if fCurrentClassLevel[fCurrentClassLevel.Count - 1] = fLevel then begin
      fCurrentClass.Delete(fCurrentClass.Count - 1);
      fCurrentClassLevel.Delete(fCurrentClassLevel.Count - 1);
      if fCurrentClassLevel.Count = 0 then
        fClassScope := scsNone
      else
        fClassScope := scsPublic;
    end;
end;

procedure TCppParser.SetInheritance(Index: integer);
  function CheckForScopeDecl(Index: integer): boolean;
  begin
    Result := (Index < fTokenizer.Tokens.Count - 1) and
      ((PToken(fTokenizer.Tokens[Index])^.Text = 'public') or
      (PToken(fTokenizer.Tokens[Index])^.Text = 'published') or
      (PToken(fTokenizer.Tokens[Index])^.Text = 'protected') or
      (PToken(fTokenizer.Tokens[Index])^.Text = 'private') or
      (PToken(fTokenizer.Tokens[Index])^.Text = '__public') or
      (PToken(fTokenizer.Tokens[Index])^.Text = '__published') or
      (PToken(fTokenizer.Tokens[Index])^.Text = '__protected') or
      (PToken(fTokenizer.Tokens[Index])^.Text = '__private'));
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
        if not (pToken(fTokenizer.Tokens[Index])^.Text[1] in [',', ':', '(']) then
          sl.Add(pToken(fTokenizer.Tokens[Index])^.Text);
      Inc(Index);
    until pToken(fTokenizer.Tokens[Index])^.Text[1] in ['{', ';', #0];
  finally
    pStatement(fStatementList[fStatementList.Count - 1])^._InheritsFromClasses := sl.CommaText;
    sl.Free;
  end;
end;

procedure TCppParser.CheckForSkipStatement;
var
  iSkip: integer;
begin
  iSkip := fSkipList.IndexOf(fIndex);
  if iSkip >= 0 then begin // skip to next ';'
    repeat
      Inc(fIndex);
    until pToken(fTokenizer.Tokens[fIndex])^.Text[1] in [';', #0];
    Inc(fIndex); //skip ';'
    fSkipList.Delete(iSkip);
  end;
end;

function TCppParser.CheckForKeyword: boolean;
begin
  Result := (PToken(fTokenizer.Tokens[fIndex])^.Text = 'static') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'STATIC') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'const') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'CONST') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'extern') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'virtual') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'if') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'else') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'return') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'case') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'switch') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'default') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'break') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'new') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'delete') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'while') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'for') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'do') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'throw') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'try') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'catch') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'using') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'friend');
end;

function TCppParser.CheckForMember: boolean;
begin
  Result := PToken(fTokenizer.Tokens[fIndex])^.Text[Length(PToken(fTokenizer.Tokens[fIndex])^.Text)] = '.';
end;

function TCppParser.CheckForTypedef: boolean;
begin
  Result := PToken(fTokenizer.Tokens[fIndex])^.Text = 'typedef';
end;

function TCppParser.CheckForEnum: boolean;
begin
  Result := PToken(fTokenizer.Tokens[fIndex])^.Text = 'enum';
end;

function TCppParser.CheckForTypedefStruct: boolean;
begin
  //we assume that typedef is the current index, so we check the next
  //should call CheckForTypedef first!!!
  Result := (fIndex < fTokenizer.Tokens.Count - 1) and
    (PToken(fTokenizer.Tokens[fIndex + 1])^.Text = 'struct') or
    (PToken(fTokenizer.Tokens[fIndex + 1])^.Text = 'class');
//    (PToken(fTokenizer.Tokens[fIndex + 1])^.Text = 'union');
end;

function TCppParser.CheckForStructs: boolean;
var
  I: integer;
begin
  Result := (PToken(fTokenizer.Tokens[fIndex])^.Text = 'struct') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'class');
//    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'union');
  if Result then begin
    if PToken(fTokenizer.Tokens[fIndex + 2])^.Text[1] <> ';' then begin // not: class something;
      I := fIndex;
    // the check for ']' was added because of this example:
    // struct option long_options[] = {
    //		{"debug", 1, 0, 'D'},
    //		{"info", 0, 0, 'i'},
    //    ...
    //  };
      while not (PToken(fTokenizer.Tokens[I])^.Text[Length(PToken(fTokenizer.Tokens[I])^.Text)] in [';', ':', '{', '}', ',', ')', ']']) do
        Inc(I);
      if not (PToken(fTokenizer.Tokens[I])^.Text[1] in ['{', ':']) then
        Result := False;
    end;
  end;
end;

function TCppParser.CheckForTemplate: boolean;
begin
  Result := (PToken(fTokenizer.Tokens[fIndex])^.Text = 'template') or
    (Copy(PToken(fTokenizer.Tokens[fIndex])^.Text, 1, 9) = 'template<');
end;

function TCppParser.CheckForUnion: boolean;
begin
  Result := (PToken(fTokenizer.Tokens[fIndex])^.Text = 'union');
end;

function TCppParser.CheckForMethod: boolean;
var
  I, I1: integer;
  JumpOver: boolean;
begin
  if PToken(fTokenizer.Tokens[fIndex])^.Text[1] = '(' then begin
    Result := False;
    Exit;
//  end
//  else if PToken(fTokenizer.Tokens[fIndex])^.Text = 'operator' then begin // skip over operator functions
//    repeat
//      Inc(fIndex);
//    until PToken(fTokenizer.Tokens[fIndex])^.Text[1] in [';', '{', '}', #0];
//    fIndex := SkipBraces(fIndex);
//    Result := False;
//    Exit;
  end;
  I := fIndex;
  Result := False;
  JumpOver := False;
  while (I < fTokenizer.Tokens.Count) and not (PToken(fTokenizer.Tokens[I])^.Text[1] in ['{', '}', ';', ',', #0]) do begin
    if (PToken(fTokenizer.Tokens[I])^.Text[Length(PToken(fTokenizer.Tokens[I])^.Text)] = '.') or
    ((Length(PToken(fTokenizer.Tokens[I])^.Text) > 1) and
      (PToken(fTokenizer.Tokens[I])^.Text[Length(PToken(fTokenizer.Tokens[I])^.Text)] = '>') and
      (PToken(fTokenizer.Tokens[I])^.Text[Length(PToken(fTokenizer.Tokens[I])^.Text) - 1] = '-')) then begin
      Result := False;
      JumpOver := True;
      Break;
    end
    // ignore operator functions
//    else if (AnsiCompareStr(PToken(fTokenizer.Tokens[I])^.Text, 'operator') = 0) or
//      (AnsiCompareStr(PToken(fTokenizer.Tokens[I])^.Text, 'operator*') = 0) or
//      (AnsiCompareStr(PToken(fTokenizer.Tokens[I])^.Text, 'operator[]') = 0) or
//      AnsiEndsStr('::operator', PToken(fTokenizer.Tokens[I])^.Text) or
//      AnsiEndsStr('::operator*', PToken(fTokenizer.Tokens[I])^.Text) or
//      AnsiEndsStr('::operator[]', PToken(fTokenizer.Tokens[I])^.Text) then begin
//      Result := False;
//      JumpOver := True;
//      Break;
//    end
    else if PToken(fTokenizer.Tokens[I])^.Text[1] = '(' then begin
      Result := PToken(fTokenizer.Tokens[I + 1])^.Text[1] in [':', ';', '{', '}'];
      if not Result then begin
        if PToken(fTokenizer.Tokens[I + 1])^.Text[1] = '(' then
          Result := False
        else if (I < fTokenizer.Tokens.Count - 2) then begin // situations where e.g. 'const' might follow...
          I1 := fIndex;
          fIndex := I + 1;
          if not CheckForScope then
            Result := PToken(fTokenizer.Tokens[I + 2])^.Text[1] in [':', ';', '{', '}'];
          fIndex := I1;
        end;
      end;
      Break;
    end;
    Inc(I);
  end;
  if JumpOver then
    while not (PToken(fTokenizer.Tokens[fIndex])^.Text[1] in ['{', '}', ';', ',', #0]) do
      Inc(fIndex);
end;

function TCppParser.CheckForScope: boolean;
begin
  Result := (fIndex < fTokenizer.Tokens.Count - 1) and
    (PToken(fTokenizer.Tokens[fIndex + 1])^.Text = ':') and
    ((PToken(fTokenizer.Tokens[fIndex])^.Text = 'public') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'published') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'protected') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'private') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = '__public') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = '__published') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = '__protected') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = '__private'));
end;

function TCppParser.CheckForPreprocessor: boolean;
begin
  Result := PToken(fTokenizer.Tokens[fIndex])^.Text[1] = '#';
end;

function TCppParser.CheckForVar: boolean;
var
  I: integer;
begin
  if fIndex < fTokenizer.Tokens.Count - 1 then
    for I := 0 to 1 do // check the current and the next token
      if CheckForKeyword or
        (PToken(fTokenizer.Tokens[fIndex + I])^.Text[1] in [',', ';', ':', '{', '}', '!', '/', '+', '-']) or
        (PToken(fTokenizer.Tokens[fIndex + I])^.Text[Length(PToken(fTokenizer.Tokens[fIndex + I])^.Text)] = '.') or
      ((Length(PToken(fTokenizer.Tokens[fIndex + I])^.Text) > 1) and
        (PToken(fTokenizer.Tokens[fIndex + I])^.Text[Length(PToken(fTokenizer.Tokens[fIndex + I])^.Text) - 1] = '-') and
        (PToken(fTokenizer.Tokens[fIndex + I])^.Text[Length(PToken(fTokenizer.Tokens[fIndex + I])^.Text)] = '>')) then begin
        Result := False;
        Exit;
      end;

  I := fIndex;
  Result := True;
  while I < fTokenizer.Tokens.Count - 1 do begin
    if (PToken(fTokenizer.Tokens[I])^.Text[1] in ['{', '}', '(']) or
      CheckForKeyword then begin
      Result := False;
      Break;
    end
    else if PToken(fTokenizer.Tokens[I])^.Text[1] in [',', ';'] then
      Break;
    Inc(I);
  end;
end;

function TCppParser.GetScope: TStatementScope;
begin
  if fLaterScanning then begin
    Result := ssLocal;
    Exit;
  end;
  if fLevel = 0 then
    Result := ssGlobal
  else begin
    if GetCurrentClass <> -1 then
      Result := ssClassLocal
    else
      Result := ssLocal;
//      Result := ssGlobal;
  end;
end;

procedure TCppParser.HandleMember;
begin
  repeat
    Inc(fIndex);
  until (fIndex >= fTokenizer.Tokens.Count) or (PToken(fTokenizer.Tokens[fIndex])^.Text[1] in [';', '}']);
  Inc(fIndex);
end;

procedure TCppParser.HandleUnion;
begin
  // goto '{' or ';'
  while (fIndex < fTokenizer.Tokens.Count) and
    not (PToken(fTokenizer.Tokens[fIndex])^.Text[Length(PToken(fTokenizer.Tokens[fIndex])^.Text)] in ['{', '}', ';']) do
    Inc(fIndex);
end;

procedure TCppParser.HandleTemplate;
begin
  // goto '{' or ';'
  while (fIndex < fTokenizer.Tokens.Count) and
    not (PToken(fTokenizer.Tokens[fIndex])^.Text[Length(PToken(fTokenizer.Tokens[fIndex])^.Text)] in ['>', '{', ';']) do
    Inc(fIndex);

  if PToken(fTokenizer.Tokens[fIndex])^.Text[1] = '{' then begin
    Inc(fIndex);

    // we just skip over the template ;)
    while (fIndex < fTokenizer.Tokens.Count) and
      not (PToken(fTokenizer.Tokens[fIndex])^.Text[1] = '}') do begin
      if PToken(fTokenizer.Tokens[fIndex])^.Text[1] = '{' then //recurse
        HandleTemplate;
      Inc(fIndex);
    end;
  end
  else
    Inc(fIndex); // probably on "class" keyword
end;

procedure TCppParser.HandleOtherTypedefs;
begin
  // just skip them...
  while (fIndex < fTokenizer.Tokens.Count) and (not (PToken(fTokenizer.Tokens[fIndex])^.Text[1] in [';', #0])) do
    Inc(fIndex);
end;

procedure TCppParser.HandleStructs(IsTypedef: boolean = False);
var
  S, S1, S2, Prefix, StructName: string;
  I, I1, cID: integer;
  IsStruct: boolean;
  UseID: integer;
  NameVisible: boolean;
begin
  NameVisible := True;
  S := PToken(fTokenizer.Tokens[fIndex])^.Text;
  IsStruct := (S = 'struct'); // or (S = 'union');
  S := S + ' ';
  Prefix := S;
  Inc(fIndex); //skip 'struct'
  I := fIndex;
  UseID := -1;
  while (I < fTokenizer.Tokens.Count) and not (PToken(fTokenizer.Tokens[I])^.Text[1] in [';', '{']) do
    Inc(I);

  // forward class/struct decl *or* typedef, e.g. typedef struct some_struct synonym1, synonym2;
  if (I < fTokenizer.Tokens.Count) and (PToken(fTokenizer.Tokens[I])^.Text[1] = ';') then begin
    StructName := PToken(fTokenizer.Tokens[fIndex])^.Text;
    if IsTypedef then begin
      repeat
        if (fIndex + 1 < fTokenizer.Tokens.Count) and (PToken(fTokenizer.Tokens[fIndex + 1])^.Text[1] in [',', ';']) then begin
          S := S + PToken(fTokenizer.Tokens[fIndex])^.Text + ' ';
          // TODO: there is a possibility to have a typedef struct arg1 arg2
          // where arg1 is declared later in the code (is it C-legal???).
          // So GetClassID now will return -1 and arg2
          // will appear as struct but will be empty :(
          // I should use a custom TList to add arg1 in it
          // and then, every new struct *decl* check the list before to see
          // if it is included in it. In that case, we use
          // the ID of the TList for the new struct and then remove
          // the arg1 from the TList...

          // DECISION: should the TList, be emptied at the end of file,
          // or remain? Maybe it is declared in another file...
          cID := GetClassID(StructName, skClass);
          fLastID := AddStatement(cID,
            GetCurrentClass,
            fCurrentFile,
            Prefix + PToken(fTokenizer.Tokens[fIndex])^.Text,
            Prefix,
            PToken(fTokenizer.Tokens[fIndex])^.Text,
            '',
            PToken(fTokenizer.Tokens[fIndex])^.Line,
            skClass,
            GetScope,
            fClassScope,
            NameVisible,
            False);
          NameVisible := False;
          if cID = -1 then
            AddToOutstandingTypedefs(StructName, fLastID);
        end;
        Inc(fIndex);
      until (fIndex >= fTokenizer.Tokens.Count) or (PToken(fTokenizer.Tokens[fIndex])^.Text[1] = ';');
      // removed support for forward decls in version 1.6
    end;
  end

  // normal class/struct decl
  else begin
    Inc(fInClass);
    if PToken(fTokenizer.Tokens[fIndex])^.Text[1] <> '{' then begin
      S1 := '';
      S2 := '';
      repeat
        S := S + PToken(fTokenizer.Tokens[fIndex])^.Text + ' ';
        if not (PToken(fTokenizer.Tokens[fIndex])^.Text[1] in [',', ';', '{', ':']) then
          S1 := S1 + PToken(fTokenizer.Tokens[fIndex])^.Text + ' ';
        if (fIndex + 1 < fTokenizer.Tokens.Count) and (PToken(fTokenizer.Tokens[fIndex + 1])^.Text[1] = '(') then
          S2 := PToken(fTokenizer.Tokens[fIndex])^.Text;
        if (fIndex + 1 < fTokenizer.Tokens.Count) and (PToken(fTokenizer.Tokens[fIndex + 1])^.Text[1] in [',', ';', '{', ':']) then begin
          if S2 = '' then
            S2 := PToken(fTokenizer.Tokens[fIndex])^.Text;
          if Trim(S1) <> '' then begin
            cID := GetClassID(Trim(S1), skClass);
            if cID = -1 then
              cID := CheckForOutstandingTypedef(Trim(S1));
            fLastID := AddStatement(cID, //UseID,
              GetCurrentClass,
              fCurrentFile,
              Prefix + Trim(S1),
              Prefix,
              S2,
              '',
              PToken(fTokenizer.Tokens[fIndex])^.Line,
              skClass,
              GetScope,
              fClassScope,
              NameVisible,
              False);
            NameVisible := False;
          end;
          S1 := '';
        end;
        Inc(fIndex);
      until (fIndex >= fTokenizer.Tokens.Count) or (PToken(fTokenizer.Tokens[fIndex])^.Text[1] in [':', '{', ';']);
      UseID := fLastID;
    end;

    if (fIndex < fTokenizer.Tokens.Count) and (PToken(fTokenizer.Tokens[fIndex])^.Text[1] = ':') then begin
      SetInheritance(fIndex); // set the _InheritsFromClasses value
      while (fIndex < fTokenizer.Tokens.Count) and (PToken(fTokenizer.Tokens[fIndex])^.Text[1] <> '{') do // skip decl after ':'
        Inc(fIndex);
    end;

    // check for struct names after '}'
    if IsStruct then begin
      I := SkipBraces(fIndex);

      S1 := '';
      if (I + 1 < fTokenizer.Tokens.Count) and (PToken(fTokenizer.Tokens[I + 1])^.Text[1] <> ';') then
        fSkipList.Add(I + 1);
      if (I + 1 < fTokenizer.Tokens.Count) then
      repeat
        Inc(I);

        if not (PToken(fTokenizer.Tokens[I])^.Text[1] in ['{', ',', ';']) then begin
          if PToken(fTokenizer.Tokens[I])^.Text[1] = '#' then begin
            I1 := fIndex;
            fIndex := I;
            HandlePreprocessor;
            fIndex := I1;
          end
          else if (PToken(fTokenizer.Tokens[I])^.Text[1] = '_') and
            (PToken(fTokenizer.Tokens[I])^.Text[Length(PToken(fTokenizer.Tokens[I])^.Text)] = '_') then
            // skip possible gcc attributes
            // start and end with 2 underscores (i.e. __attribute__)
            // so, to avoid slow checks of strings, we just check the first and last letter of the token
            // if both are underscores, we split
            Break
          else begin
            if PToken(fTokenizer.Tokens[I])^.Text[Length(PToken(fTokenizer.Tokens[I])^.Text)] = ']' then // cut-off array brackets
              S1 := S1 + Copy(PToken(fTokenizer.Tokens[I])^.Text, 1, AnsiPos('[', PToken(fTokenizer.Tokens[I])^.Text) - 1) + ' '
            else
              S1 := S1 + PToken(fTokenizer.Tokens[I])^.Text + ' ';
          end;
        end
        else begin
          if Trim(S1) <> '' then begin
            if UseID <> -1 then
              cID := UseID
            else
              cID := CheckForOutstandingTypedef(Trim(S1));
            fLastID := AddStatement(cID,
              GetCurrentClass,
              fCurrentFile,
              Prefix + Trim(S1),
              Prefix,
              Trim(S1),
              '',
              PToken(fTokenizer.Tokens[I])^.Line,
              skClass,
              GetScope,
              fClassScope,
              NameVisible,
              True);
            NameVisible := False;
          end;
          UseID := fLastID;
          S1 := '';
        end;

        if not (PToken(fTokenizer.Tokens[I])^.Text[1] in [';', ',', '#']) then
          S := S + ' ' + PToken(fTokenizer.Tokens[I])^.Text;
      until (I >= fTokenizer.Tokens.Count) or (PToken(fTokenizer.Tokens[I])^.Text[1] in ['{', ';']);
    end;
    SetCurrentClass(fLastID);
  end;
  if fLogStatements then
    if Assigned(fOnLogStatement) then
      fOnLogStatement(Self, '[parser   ]: -C- ' + Format('%4d ', [PToken(fTokenizer.Tokens[fIndex - 1])^.Line]) + StringOfChar(' ', fLevel) + Trim(S));
end;

procedure TCppParser.HandleMethod;
var
  S, S1, S2, S3: string;
  bTypeOK, bOthersOK: boolean;
  IsValid: boolean;
  CurrClass: integer;
  I: integer;
  IsDeclaration: boolean;
begin
  IsValid := True;
  S := '';
  S1 := '';
  S2 := '';
  S3 := '';
  bTypeOK := False;
  bOthersOK := False;
  CurrClass := GetCurrentClass;
  I := fIndex;
  while (fIndex < fTokenizer.Tokens.Count) and (not (PToken(fTokenizer.Tokens[fIndex])^.Text[1] in [';', ':', '{', '}', #0])) do begin
    if PToken(fTokenizer.Tokens[fIndex])^.Text[1] <> '#' then // jump-over preprocessor directives in definition
      S := S + PToken(fTokenizer.Tokens[fIndex])^.Text + ' ';

    if not bTypeOK and
      (PToken(fTokenizer.Tokens[fIndex + 1])^.Text[1] <> '(') and
      ((fIndex < fTokenizer.Tokens.Count - 2) and (PToken(fTokenizer.Tokens[fIndex + 2])^.Text[1] <> '(')) then //type
      S1 := S1 + PToken(fTokenizer.Tokens[fIndex])^.Text + ' '
    else if not bTypeOK and
      (PToken(fTokenizer.Tokens[fIndex + 1])^.Text[1] <> '(') and
      ((fIndex < fTokenizer.Tokens.Count - 2) and (PToken(fTokenizer.Tokens[fIndex + 2])^.Text[1] = '(')) then //type
      S1 := S1 + PToken(fTokenizer.Tokens[fIndex])^.Text + ' '
    else if not bOthersOK and
      (PToken(fTokenizer.Tokens[fIndex + 1])^.Text[1] = '(') and
      ((fIndex < fTokenizer.Tokens.Count - 2) and (PToken(fTokenizer.Tokens[fIndex + 2])^.Text[1] <> '(')) then begin //command
      S2 := PToken(fTokenizer.Tokens[fIndex])^.Text;
      S3 := PToken(fTokenizer.Tokens[fIndex + 1])^.Text;
      bTypeOK := True;
    end;

    Inc(fIndex);
  end;
  IsDeclaration := False;
  if PToken(fTokenizer.Tokens[fIndex])^.Text[1] in [';', '}'] then begin
    IsDeclaration := True;
    if not fIsHeader and (CurrClass = -1) then
      IsValid := False;
  end
  else begin
    if PToken(fTokenizer.Tokens[fIndex])^.Text[1] = ':' then
      while (fIndex < fTokenizer.Tokens.Count) and (not (PToken(fTokenizer.Tokens[fIndex])^.Text[1] in [';', '{', '}', #0])) do
        Inc(fIndex);
    if PToken(fTokenizer.Tokens[fIndex])^.Text[1] in [';', '}'] then begin
      IsDeclaration := True;
      if not fIsHeader and (CurrClass = -1) then
        IsValid := False;
    end;
  end;
  if not bTypeOK then
    S1 := '';
  if IsValid then
    fLastID := AddStatement(-1,
      CurrClass,
      fCurrentFile,
      S,
      Trim(S1),
      Trim(S2),
      Trim(S3),
      PToken(fTokenizer.Tokens[fIndex - 1])^.Line,
      skFunction,
      GetScope,
      fClassScope,
      True,
      False,
      IsDeclaration);
  // don't parse the function's block now... It will be parsed when user presses ctrl+space inside it ;)
  if (PToken(fTokenizer.Tokens[fIndex])^.Text[1] = '{') then
    fIndex := SkipBraces(fIndex) + 1; // add 1 so that '}' is not visible to parser
  if fLogStatements then
    if Assigned(fOnLogStatement) and IsValid then
      fOnLogStatement(Self, '[parser   ]: -M- ' + Format('%4d ', [PToken(fTokenizer.Tokens[fIndex - 1])^.Line]) + StringOfChar(' ', fLevel) + Trim(S));
  if I = fIndex then // if not moved ahead, something is wrong but don't get stuck ;)
    if fIndex < fTokenizer.Tokens.Count then
      if not (PToken(fTokenizer.Tokens[fIndex])^.Text[1] in ['{', '}', #0]) then
        Inc(fIndex);
end;

procedure TCppParser.HandleScope;
begin
  if (PToken(fTokenizer.Tokens[fIndex])^.Text = 'public') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = '__public') then
    fClassScope := scsPublic
  else if (PToken(fTokenizer.Tokens[fIndex])^.Text = 'published') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = '__published') then
    fClassScope := scsPublished
  else if (PToken(fTokenizer.Tokens[fIndex])^.Text = 'private') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = '__private') then
    fClassScope := scsPrivate
  else if (PToken(fTokenizer.Tokens[fIndex])^.Text = 'protected') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = '__protected') then
    fClassScope := scsProtected
  else
    fClassScope := scsNone;
  if fLogStatements then
    if Assigned(fOnLogStatement) then
      fOnLogStatement(Self, '[parser   ]: -S- ' + Format('%4d ', [PToken(fTokenizer.Tokens[fIndex])^.Line]) + StringOfChar(' ', fLevel) + PToken(fTokenizer.Tokens[fIndex])^.Text);
  Inc(fIndex, 2); // the scope is followed by a ':'
end;

procedure TCppParser.HandlePreprocessor;
var
  sl: TStringList;
  Index: integer;
  FName: string;
  FullFName: string;
  StrFullText: string;
  StrArgs: string;
  StrCommand: string;
  IsGlobal: boolean;
  OpenBracketPos: Integer;
  I: Integer;
begin
  sl := TStringList.Create;
  try
    ExtractStrings([' '], [' '], @(PToken(fTokenizer.Tokens[fIndex])^.Text)[1], sl);
    if sl.Count > 0 then begin
      // INCLUDES
      if sl[0] = '#include' then
        Index := 1
      else if (sl.Count > 1) and (sl[0] = '#') and (sl[1] = 'include') then
        Index := 2
      else
        Index := -1;
      if Index <> -1 then begin
        FName := StringReplace(sl[Index], '<', '', [rfReplaceAll]);
        FName := StringReplace(FName, '>', '', [rfReplaceAll]);
        FName := StringReplace(FName, '"', '', [rfReplaceAll]);
        FullFName := LowerCase(ExpandFileName(GetFullFileName(FName)));
        PIncludesRec(fIncludesList[fIncludesList.Count - 1])^.IncludeFiles := PIncludesRec(fIncludesList[fIncludesList.Count - 1])^.IncludeFiles + AnsiQuotedStr(FullFName, '"') + ',';
        IsGlobal := IsGlobalFile(FullFName);
        if {not fReparsing and}((fParseGlobalHeaders and IsGlobal) or (fParseLocalHeaders and not IsGlobal)) then begin
          AddFileToScan(FullFName);
          if fLogStatements then
            if Assigned(fOnLogStatement) then
              fOnLogStatement(Self, '[parser   ]: -P- ' + Format('%4d ', [PToken(fTokenizer.Tokens[fIndex])^.Line]) + StringOfChar(' ', fLevel) + Format('#INCLUDE %s (scheduled to be scanned)', [FName]));
        end;
      end

      // DEFINITIONS
      else 
      begin
        if sl[0] = '#define' then
        begin
          Index := 1
        end
        else 
        begin
          if (sl.Count > 1) and (sl[0] = '#') and (sl[1] = 'define') then
          begin
            Index := 2
          end
          else 
          begin
            Index := -1
          end
        end;
        
        // modified by peter_
        if (Index <> -1) and (sl.Count > Index + 1) then
        begin
          StrFullText := sl[Index];
          OpenBracketPos := AnsiPos('(', StrFullText);
          
          // Is it a #define with arguments, like 'foo(a, b)' ?
          if OpenBracketPos > 0 then
          begin
            I := Index+1;

            // Because of the call to ExtractStrings, a few lines
            // above, the define could be seperated into several
            // strings. This would result into:
            // 1) foo(a,
            // 2) b)
            // Because this is kinda wrong, we have to loop through
            // the List and merge our FullText again in order to get
            // this: foo(a, b)
            while AnsiPos(')', StrFullText) = 0 do
            begin
              StrFullText := StrFullText + sl[I];
              Inc(I);
            end;

            // Copy '(a, b)' out of 'foo(a, b)'
            StrArgs := Copy(StrFullText, OpenBracketPos, Length(StrFullText)-OpenBracketPos+1);

            // Copy 'foo' out of 'foo(a, b)'
            StrCommand := Copy(StrFullText, 1, OpenBracketPos-1);
          end
          else
          begin
            // In case the #define has no arguments, the Command is just
            // the same as the define name!
            StrCommand := StrFullText;

            // and we don't have an argument
            StrArgs := '';
          end;

          AddStatement(-1 {GetClassID(StrCommand, skPreprocessor)},
                       GetCurrentClass,
                       FCurrentFile,
                       StrFullText,
                       '',
                       StrCommand,
                       StrArgs,
                       PToken(FTokenizer.Tokens[FIndex])^.Line,
                       skPreprocessor,
                       GetScope,
                       FClassScope,
                       False,
                       True)
        end

        // All OTHER
        else 
        begin
          if fLogStatements then
          begin
            if Assigned(fOnLogStatement) then
            begin
              fOnLogStatement(Self, '[parser   ]: -P- ' +
                Format('%4d ', [PToken(fTokenizer.Tokens[fIndex])^.Line]) +
                StringOfChar(' ', fLevel) + 'Unknown definition: ' +
                PToken(fTokenizer.Tokens[fIndex])^.Text)
            end
          end
        end;
      end

    end
    else 
    begin
      if fLogStatements then
      begin
        if Assigned(fOnLogStatement) then
        begin
          fOnLogStatement(Self, '[parser   ]: -P- ' +
            Format('%4d ', [PToken(fTokenizer.Tokens[fIndex])^.Line]) +
            StringOfChar(' ', fLevel) + 'Unknown definition: ' +
            PToken(fTokenizer.Tokens[fIndex])^.Text)
        end
      end
    end;
  finally
    sl.Free;
  end;
  Inc(fIndex);
end;

procedure TCppParser.HandleKeyword;
begin
  if (PToken(fTokenizer.Tokens[fIndex])^.Text = 'static') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'STATIC') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'const') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'CONST') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'extern') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'virtual') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'else') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'break') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'new') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'try') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'do') then
    Inc(fIndex) //skip it
  else if (PToken(fTokenizer.Tokens[fIndex])^.Text = 'if') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'switch') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'while') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'for') then begin //skip to ')'
    repeat
      Inc(fIndex);
    until (fIndex >= fTokenizer.Tokens.Count) or (PToken(fTokenizer.Tokens[fIndex])^.Text[Length(PToken(fTokenizer.Tokens[fIndex])^.Text)] in [#0, ')']);
    Inc(fIndex);
  end
  else if (PToken(fTokenizer.Tokens[fIndex])^.Text = 'case') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'default') then begin //skip to ':'
    repeat
      Inc(fIndex);
    until (fIndex >= fTokenizer.Tokens.Count) or (PToken(fTokenizer.Tokens[fIndex])^.Text[Length(PToken(fTokenizer.Tokens[fIndex])^.Text)] in [#0, ':', '}']);
    if (fIndex >= fTokenizer.Tokens.Count) then
      exit;
    if PToken(fTokenizer.Tokens[fIndex])^.Text[1] = ':' then
      Inc(fIndex);
  end
  else if (PToken(fTokenizer.Tokens[fIndex])^.Text = 'return') or //skip to ';'
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'delete') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'throw') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'using') or
    (PToken(fTokenizer.Tokens[fIndex])^.Text = 'friend') then begin
    repeat
      Inc(fIndex);
    until (fIndex >= fTokenizer.Tokens.Count) or (PToken(fTokenizer.Tokens[fIndex])^.Text[1] in [#0, ';', '}']);
    if (fIndex >= fTokenizer.Tokens.Count) then
      exit;
    if PToken(fTokenizer.Tokens[fIndex])^.Text = ';' then
      Inc(fIndex);
  end
  else if (PToken(fTokenizer.Tokens[fIndex])^.Text = 'catch') then begin //skip to '{'
    repeat
      Inc(fIndex);
    until (fIndex >= fTokenizer.Tokens.Count) or (PToken(fTokenizer.Tokens[fIndex])^.Text[1] in [#0, '{', '}']);
  end;
end;

procedure TCppParser.HandleVar;
var
  LastType: string;
  Args: string;
  Cmd: string;
begin
  LastType := '';
  repeat
    if (fIndex < fTokenizer.Tokens.Count - 1) and (PToken(fTokenizer.Tokens[fIndex + 1])^.Text[1] in [',', ';', ':', '}']) then
      Break;
    if (PToken(fTokenizer.Tokens[fIndex])^.Text <> 'struct') and
      (PToken(fTokenizer.Tokens[fIndex])^.Text <> 'class') then
//      (PToken(fTokenizer.Tokens[fIndex])^.Text <> 'union') then
      LastType := Trim(LastType + ' ' + PToken(fTokenizer.Tokens[fIndex])^.Text);
    Inc(fIndex);
  until fIndex = fTokenizer.Tokens.Count;
  if fIndex = fTokenizer.Tokens.Count then
    Exit;

  repeat
    // skip bit identifiers,
    // e.g.:
    // handle
    // unsigned short bAppReturnCode:8,reserved:6,fBusy:1,fAck:1
    // as
    // unsigned short bAppReturnCode,reserved,fBusy,fAck
    if PToken(fTokenizer.Tokens[fIndex])^.Text[1] = ':' then
      repeat
        Inc(fIndex);
      until (fIndex >= fTokenizer.Tokens.Count) or (PToken(fTokenizer.Tokens[fIndex])^.Text[1] in [',', ';', '{', '}']);   // CL: added check for fIndex validity
    if fIndex = fTokenizer.Tokens.Count then
      Exit;
    if not (PToken(fTokenizer.Tokens[fIndex])^.Text[1] in [',', ';']) then begin
      if fLogStatements then
        if Assigned(fOnLogStatement) then
          fOnLogStatement(Self, '[parser   ]: -V- ' + Format('%4d ', [PToken(fTokenizer.Tokens[fIndex])^.Line]) + StringOfChar(' ', fLevel) + Trim(LastType + ' ' + PToken(fTokenizer.Tokens[fIndex])^.Text));
      if PToken(fTokenizer.Tokens[fIndex])^.Text[Length(PToken(fTokenizer.Tokens[fIndex])^.Text)] = ']' then begin //array; break args
        Cmd := Copy(PToken(fTokenizer.Tokens[fIndex])^.Text, 1, AnsiPos('[', PToken(fTokenizer.Tokens[fIndex])^.Text) - 1);
        Args := Copy(PToken(fTokenizer.Tokens[fIndex])^.Text, AnsiPos('[', PToken(fTokenizer.Tokens[fIndex])^.Text), Length(PToken(fTokenizer.Tokens[fIndex])^.Text) - AnsiPos('[', PToken(fTokenizer.Tokens[fIndex])^.Text) + 1);
      end
      else begin
        Cmd := PToken(fTokenizer.Tokens[fIndex])^.Text;
        Args := '';
      end;
      fLastID := AddStatement(-1,
        GetCurrentClass,
        fCurrentFile,
        LastType + ' ' + PToken(fTokenizer.Tokens[fIndex])^.Text,
        LastType,
        Cmd,
        Args,
        PToken(fTokenizer.Tokens[fIndex])^.Line,
        skVariable,
        GetScope,
        fClassScope,
        True, //GetCurrentClass <> -1,
        True);
    end;
    if not (PToken(fTokenizer.Tokens[fIndex])^.Text[1] in [';', '{', '}']) then
      Inc(fIndex);
  until (fIndex >= fTokenizer.Tokens.Count) or (PToken(fTokenizer.Tokens[fIndex])^.Text[1] in [';', '{', '}']);
  if (fIndex >= fTokenizer.Tokens.Count) then
    exit;
  if not (PToken(fTokenizer.Tokens[fIndex])^.Text[1] in ['{', '}']) then
    Inc(fIndex);
end;

procedure TCppParser.HandleEnum;
var
  LastType: string;
  Args: string;
  Cmd: string;
  I: integer;
begin
  LastType := 'enum ';
  Inc(fIndex); //skip 'enum'
  if PToken(fTokenizer.Tokens[fIndex])^.Text[1] = '{' then begin // enum {...} NAME
    I := fIndex;
    repeat
      Inc(I);
    until (I >= fTokenizer.Tokens.Count) or (PToken(fTokenizer.Tokens[I])^.Text[1] in ['}', #0]);
    if (I >= fTokenizer.Tokens.Count) then
      exit;
    if PToken(fTokenizer.Tokens[I])^.Text[1] = '}' then
      if PToken(fTokenizer.Tokens[I + 1])^.Text[1] <> ';' then
        LastType := LastType + PToken(fTokenizer.Tokens[I + 1])^.Text + ' ';
  end
  else // enum NAME {...};
    while not (PToken(fTokenizer.Tokens[fIndex])^.Text[1] in ['{', ';', #0]) do begin
      LastType := LastType + PToken(fTokenizer.Tokens[fIndex])^.Text + ' ';
      Inc(fIndex);
      if (fIndex >= fTokenizer.Tokens.Count) then
        exit;
    end;
  LastType := Trim(LastType);

  if PToken(fTokenizer.Tokens[fIndex])^.Text[1] = '{' then begin
    Inc(fIndex);

    repeat
      if not (PToken(fTokenizer.Tokens[fIndex])^.Text[1] in [',', '#', ';']) then begin
        if fLogStatements then
          if Assigned(fOnLogStatement) then
            fOnLogStatement(Self, '[parser   ]: -E- ' + Format('%4d ', [PToken(fTokenizer.Tokens[fIndex])^.Line]) + StringOfChar(' ', fLevel) + Trim(LastType + ' ' + PToken(fTokenizer.Tokens[fIndex])^.Text));
        if PToken(fTokenizer.Tokens[fIndex])^.Text[Length(PToken(fTokenizer.Tokens[fIndex])^.Text)] = ']' then begin //array; break args
          Cmd := Copy(PToken(fTokenizer.Tokens[fIndex])^.Text, 1, AnsiPos('[', PToken(fTokenizer.Tokens[fIndex])^.Text) - 1);
          Args := Copy(PToken(fTokenizer.Tokens[fIndex])^.Text, AnsiPos('[', PToken(fTokenizer.Tokens[fIndex])^.Text), Length(PToken(fTokenizer.Tokens[fIndex])^.Text) - AnsiPos('[', PToken(fTokenizer.Tokens[fIndex])^.Text) + 1);
        end
        else begin
          Cmd := PToken(fTokenizer.Tokens[fIndex])^.Text;
          Args := '';
        end;
        fLastID := AddStatement(-1,
          GetCurrentClass,
          fCurrentFile,
          LastType + ' ' + PToken(fTokenizer.Tokens[fIndex])^.Text,
          LastType,
          Cmd,
          Args,
          PToken(fTokenizer.Tokens[fIndex])^.Line,
          skEnum,
          GetScope,
          fClassScope,
          False,
          True);
      end;
      if PToken(fTokenizer.Tokens[fIndex])^.Text[1] = '#' then
        HandlePreprocessor;
      Inc(fIndex);
      if (fIndex >= fTokenizer.Tokens.Count) then
        exit;
    until PToken(fTokenizer.Tokens[fIndex])^.Text[1] in [';', '{', '}'];
    if PToken(fTokenizer.Tokens[fIndex])^.Text[1] = '}' then
      Inc(fIndex);
  end;
end;

function TCppParser.HandleStatement: boolean;
begin
  if PToken(fTokenizer.Tokens[fIndex])^.Text[1] = '{' then begin
    Inc(fLevel, 2);
    Inc(fIndex);
    if fInClass > 0 then
      Inc(fInClass);
  end
  else if PToken(fTokenizer.Tokens[fIndex])^.Text = '}' then begin
    Dec(fLevel, 2);
    Inc(fIndex);
    if fInClass > 0 then
      Dec(fInClass);
    RemoveCurrentClass;
  end
  else if CheckForPreprocessor then begin
    HandlePreprocessor;
  end
  else if CheckForMember then begin
    HandleMember;
  end
  else if CheckForKeyword then begin
    HandleKeyword;
  end
  else if CheckForScope then begin
    HandleScope;
  end
  else if CheckForEnum then begin
    HandleEnum;
  end
  else if CheckForTypedef then begin
    if CheckForTypedefStruct then begin
      Inc(fIndex); //skip typedef
      HandleStructs(True);
    end
    else
      HandleOtherTypedefs;
  end
  else if CheckForTemplate then begin
    HandleTemplate;
  end
  else if CheckForUnion then begin
    HandleUnion;
  end
  else if CheckForStructs then begin
    HandleStructs(False);
  end
  else if CheckForMethod then begin
    HandleMethod;
  end
  else if CheckForVar then begin
    HandleVar;
  end
  else
    Inc(fIndex);

  CheckForSkipStatement;

  if Assigned(fOnFileProgress) then
    fOnFileProgress(Self, fCurrentFile, fTokenizer.Tokens.Count, fIndex);

  Result := fIndex < fTokenizer.Tokens.Count;
end;

procedure TCppParser.Parse(FileName: TFileName; IsVisible: boolean; ManualUpdate: boolean = False; processInh: boolean = True);
var
  sTime: myTickCount;
  P: PIncludesRec;
begin
  if not fEnabled then
    Exit;
  if not (IsCfile(Filename) or IsHfile(Filename)) then // support only known C/C++ files
    Exit;
  if fTokenizer = nil then
    Exit;
  if (not ManualUpdate) and Assigned(fOnStartParsing) then
    fOnStartParsing(Self);
  ClearOutstandingTypedefs;
  sTime := myGetTickCount;
  if Assigned(fOnLogStatement) then
    fOnLogStatement(Self, '[parser   ]: Parsing ' + FileName);
  fTokenizer.Reset;
  try
    fTokenizer.Tokenize(FileName);
    if fTokenizer.Tokens.Count = 0 then
      Exit;
  except
    if (not ManualUpdate) and Assigned(fOnEndParsing) then
      fOnEndParsing(Self);
    Exit;
  end;
  fCurrentFile := LowerCase(FileName);
  fIsProjectFile := fProjectFiles.IndexOf(fCurrentFile) <> -1;
  fIndex := 0;
  fLevel := 0;
  fLastID := -1;
  P := New(PIncludesRec);
  P^.BaseFile := fCurrentFile;
  P^.IncludeFiles := '';
  fIncludesList.Add(P);
  fIsHeader := IsHfile(Filename);
  fCurrentClass := TIntList.Create;
  fCurrentClassLevel := TIntList.Create;
  fSkipList := TIntList.Create;
  fLastStatementKind := skUnknown;
  if Assigned(fOnFileProgress) then
    fOnFileProgress(Self, fCurrentFile, fTokenizer.Tokens.Count, 0);
  fVisible := IsVisible;
  try
    try
      repeat
      until not HandleStatement;
      if processInh then
        PostProcessInheritance;
      if Assigned(fOnFileProgress) then
        fOnFileProgress(Self, fCurrentFile, 0, 0);
      fScannedFiles.Add(FileName);
      if Assigned(fOnLogStatement) then
        fOnLogStatement(Self, Format('[parser   ]: Done in %2.3f seconds.', [myGetSecsSickTick(sTime)]));
    except
      if Assigned(fOnLogStatement) then
        fOnLogStatement(Self, Format('[parser   ]: Error scanning file %s', [FileName]));
    end;
  finally
    // remove last comma
    Delete(PIncludesRec(fIncludesList[fIncludesList.Count - 1])^.IncludeFiles, Length(PIncludesRec(fIncludesList[fIncludesList.Count - 1])^.IncludeFiles), 1);
    fSkipList.Clear;
    fCurrentClassLevel.Clear;
    FCurrentClass.Clear;
    FreeAndNil(fSkipList);
    FreeAndNil(fCurrentClassLevel);
    FreeAndNil(FCurrentClass);
    if (not ManualUpdate) and Assigned(fOnEndParsing) then
      fOnEndParsing(Self);
  end;
  fIsProjectFile := False;
  if not ManualUpdate then
    if Assigned(fOnUpdate) then
      fOnUpdate(Self);
end;

procedure TCppParser.Reset(KeepLoaded: boolean = True);
var
  I: integer;
  I1: integer;
begin
  if Assigned(fOnBusy) then
    fOnBusy(Self);
  ClearOutstandingTypedefs;
  fFilesToScan.Clear;
  if Assigned(fTokenizer) then
    fTokenizer.Reset;

  if Assigned(fOnStartParsing) then
    fOnStartParsing(Self);

  if KeepLoaded then
    I := fBaseIndex
  else
    I := 0;
  while I < fStatementList.Count do
    if not KeepLoaded or (KeepLoaded and not PStatement(fStatementList[I])^._Loaded) then begin
      I1 := fScannedFiles.IndexOf(PStatement(fStatementList[I])^._Filename);
      if I1 = -1 then
        I1 := fScannedFiles.IndexOf(PStatement(fStatementList[I])^._DeclImplFileName);
      if I1 <> -1 then
        fScannedFiles.Delete(I1);
      I1 := fCacheContents.IndexOf(PStatement(fStatementList[I])^._Filename);
      if I1 = -1 then
        I1 := fCacheContents.IndexOf(PStatement(fStatementList[I])^._DeclImplFileName);
      if I1 <> -1 then
        fCacheContents.Delete(I1);
      Dispose(PStatement(fStatementList[I]));
      fStatementList.Delete(I);
    end
    else
      Inc(I);
  fStatementList.Pack;

  if not KeepLoaded then begin
    while fIncludesList.Count > 0 do begin
      if Assigned(fIncludesList[fIncludesList.Count - 1]) then
        Dispose(PIncludesRec(fIncludesList[fIncludesList.Count - 1]));
      fIncludesList.Delete(fIncludesList.Count - 1);
    end;
    fNextID := 0;
    fBaseIndex := 0;
  end;

  fProjectFiles.Clear;

  if Assigned(fOnEndParsing) then
    fOnEndParsing(Self);

  if Assigned(fOnUpdate) then
    fOnUpdate(Self);
end;

procedure TCppParser.Parse(FileName: TFileName);
var
  sTime: myTickCount;
  IsVisible: boolean;
  bLocal: boolean;
  bGlobal: boolean;
begin
  if not fEnabled then
    Exit;
  if Filename = '' then
    Exit;
  if Assigned(fOnLogStatement) then
    fOnLogStatement(Self, '[parser   ]: Starting.');
  AddFileToScan(FileName);
  bLocal := ParseLocalHeaders;
  bGlobal := ParseGlobalHeaders;
  ParseLocalHeaders := False;
  ParseGlobalHeaders := False;
  sTime := myGetTickCount;
  if Assigned(fOnStartParsing) then
    fOnStartParsing(Self);
  try
    while fFilesToScan.Count > 0 do begin
      if Assigned(fOnTotalProgress) then
        fOnTotalProgress(Self, fFilesToScan[0], fFilesToScan.Count, 1);
      IsVisible := not IsGlobalFile(fFilesToScan[0]);
      Parse(fFilesToScan[0], IsVisible, True);
      fFilesToScan.Delete(0);
    end;
  finally
    if Assigned(fOnEndParsing) then
      fOnEndParsing(Self);
  end;
  fStatementList.Pack;
  ParseLocalHeaders := bLocal;
  ParseGlobalHeaders := bGlobal;
  if Assigned(fOnTotalProgress) then
    fOnTotalProgress(Self, '', 0, 0);
  if Assigned(fOnLogStatement) then
    fOnLogStatement(Self, Format('[parser   ]: Total parsing done in %2.3f seconds.', [myGetSecsSickTick(sTime)]));
  if Assigned(fOnUpdate) then
    fOnUpdate(Self);
end;

procedure TCppParser.ParseList;
var
  sTime: myTickCount;
  IsVisible: boolean;

begin
  if not fEnabled then
    Exit;
  if Assigned(fOnBusy) then
    fOnBusy(Self);
  if Assigned(fOnLogStatement) then
    fOnLogStatement(Self, '[parser   ]: Starting.');
  sTime := myGetTickCount;
  if Assigned(fOnStartParsing) then
    fOnStartParsing(Self);
  try
    while fFilesToScan.Count > 0 do begin
      if Assigned(fOnTotalProgress) then
        fOnTotalProgress(Self, fFilesToScan[0], fFilesToScan.Count, 1);
      if fScannedFiles.IndexOf(fFilesToScan[0]) = -1 then begin
        IsVisible := not IsGlobalFile(fFilesToScan[0]);
        Parse(fFilesToScan[0], IsVisible, True, False);
      end;
      fFilesToScan.Delete(0);
    end;
    PostProcessInheritance;
  finally
    if Assigned(fOnEndParsing) then
      fOnEndParsing(Self);
  end;
  fStatementList.Pack;
  if Assigned(fOnTotalProgress) then
    fOnTotalProgress(Self, '', 0, 0);
  if Assigned(fOnLogStatement) then
    fOnLogStatement(Self, Format('[parser   ]: Total parsing done in %2.3f seconds.', [myGetSecsSickTick(sTime)]));
  if Assigned(fOnUpdate) then
    fOnUpdate(Self);
end;

function TCppParser.GetFullFilename(Value: string): string;
var
  I: integer;
  tmp: string;
begin
  Result := '';
  tmp := ExtractFilePath(fCurrentFile);
  if FileExists(tmp + Value) then // same dir with file
    Result := tmp + Value
  else if FileExists(fProjectDir + Value) then //search in project dir
    Result := fProjectDir + Value
  else begin //search in included dirs
    for I := 0 to fIncludePaths.Count - 1 do
      if FileExists(fIncludePaths[I] + '\' + Value) then begin
        Result := fIncludePaths[I] + '\' + Value;
        Break;
      end;
    for I := 0 to fProjectIncludePaths.Count - 1 do
      if FileExists(fProjectIncludePaths[I] + '\' + Value) then begin
        Result := fProjectIncludePaths[I] + '\' + Value;
        Break;
      end;
  end;
  if Result = '' then // not found...
    Result := Value;

  if Result = '' then begin
    if Assigned(fOnLogStatement) then
      fOnLogStatement(Self, '[parser   ]: ' + Format('File %s not found...', [Value]));
    Result := Value;
  end;
  Result := StringReplace(Result, '/', '\', [rfReplaceAll]);
end;

function TCppParser.IsCfile(Filename: string): boolean;
var
  ext: string;
begin
  ext := LowerCase(ExtractFileExt(Filename));
  Result := (ext = '.cpp') or (ext = '.c') or (ext = '.cc');
end;

function TCppParser.IsHfile(Filename: string): boolean;
var
  ext: string;
begin
  ext := LowerCase(ExtractFileExt(Filename));
  Result := (ext = '.h') or (ext = '.hpp') or (ext = '.hh') or (ext = '');
end;

procedure TCppParser.GetSourcePair(FName: string; var CFile, HFile: string);
begin
  if IsCfile(FName) then begin
    CFile := FName;
    if FileExists(ChangeFileExt(FName, '.h')) then
      HFile := ChangeFileExt(FName, '.h')
    else if FileExists(ChangeFileExt(FName, '.hpp')) then
      HFile := ChangeFileExt(FName, '.hpp')
    else if FileExists(ChangeFileExt(FName, '.hh')) then
      HFile := ChangeFileExt(FName, '.hh')
    else
      HFile := '';
  end
  else if IsHfile(FName) then begin
    HFile := FName;
    if FileExists(ChangeFileExt(FName, '.c')) then
      CFile := ChangeFileExt(FName, '.c')
    else if FileExists(ChangeFileExt(FName, '.cpp')) then
      CFile := ChangeFileExt(FName, '.cpp')
    else if FileExists(ChangeFileExt(FName, '.cc')) then
      CFile := ChangeFileExt(FName, '.cc')
    else
      CFile := '';
  end
  else begin
    CFile := FName;
    HFile := '';
  end;
end;

procedure TCppParser.AddFileToScan(Value: string; InProject: boolean);
var
  FName: string;
  CFile, HFile: string;
begin
  FName := StringReplace(Value, '/', '\', [rfReplaceAll]);
  FName := GetFullFilename(LowerCase(FName));

  if InProject then
    fProjectFiles.Add(FName);

  // automatically add header and impl file
  CFile := '';
  HFile := '';
  if IsCfile(FName) then
    GetSourcePair(FName, CFile, HFile)
  else if IsHfile(FName) then
    HFile := FName;

  if HFile <> '' then
    if fFilesToScan.IndexOf(HFile) = -1 then // check scheduled files
      if fScannedFiles.IndexOf(HFile) = -1 then // check files already parsed
        fFilesToScan.Add(HFile);

  if CFile <> '' then
    if fFilesToScan.IndexOf(CFile) = -1 then // check scheduled files
      if fScannedFiles.IndexOf(CFile) = -1 then // check files already parsed
        fFilesToScan.Add(CFile);
end;

procedure TCppParser.AddIncludePath(Value: string);
var
  S: string;
begin
  S := AnsiDequotedStr(LowerCase(Value), '"');
  if fIncludePaths.IndexOf(S) = -1 then
    fIncludePaths.Add(S);
end;

procedure TCppParser.AddProjectIncludePath(Value: string);
var
  S: string;
begin
  S := AnsiDequotedStr(LowerCase(Value), '"');
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

function TCppParser.IsGlobalFile(Value: string): boolean;
var
  I: integer;
begin
  Result := False;
  for I := 0 to fIncludePaths.Count - 1 do
    if AnsiStartsText(fIncludePaths[I], Value) then begin
      Result := True;
      Break;
    end;
end;

procedure TCppParser.ReParseFile(FileName: TFileName; InProject: boolean; OnlyIfNotParsed: boolean; UpdateView: boolean);
var
  FName: string;
  CFile, HFile: string;
  IsVisible: boolean;
  I: integer;
begin
  if not fEnabled then
    Exit;
  FName := LowerCase(FileName);
  if OnlyIfNotParsed and (fScannedFiles.IndexOf(FName) <> -1) then
    Exit;
  if Assigned(fOnBusy) then
    fOnBusy(Self);

  CFile := '';
  HFile := '';
  if IsCfile(FName) then
    GetSourcePair(FName, CFile, HFile)
  else if IsHfile(FName) then
    HFile := FName;

  fInvalidatedIDs.Clear;
  InvalidateFile(CFile);
  InvalidateFile(HFile);
  if InProject then begin
    if (CFile <> '') and (fProjectFiles.IndexOf(CFile) = -1) then
      fProjectFiles.Add(CFile);
    if (HFile <> '') and (fProjectFiles.IndexOf(HFile) = -1) then
      fProjectFiles.Add(HFile);
  end
  else begin
    I := fProjectFiles.IndexOf(CFile);
    if I <> -1 then
      fProjectFiles.Delete(I);
    I := fProjectFiles.IndexOf(HFile);
    if I <> -1 then
      fProjectFiles.Delete(I);
  end;
  fFilesToScan.Clear;
  fReparsing := True;
  Parse(HFile, not IsGlobalFile(HFile), True);
  Parse(CFile, not IsGlobalFile(CFile), True);

  if Assigned(fOnStartParsing) then
    fOnStartParsing(Self);
  try
    I := 0;
    while I < fFilesToScan.Count do begin
      if Assigned(fOnTotalProgress) then
        fOnTotalProgress(Self, fFilesToScan[I], fFilesToScan.Count, 1);
      if fScannedFiles.IndexOf(fFilesToScan[I]) = -1 then begin
        IsVisible := not IsGlobalFile(fFilesToScan[I]);
        Parse(fFilesToScan[I], IsVisible, True);
        fFilesToScan.Delete(I);
      end
      else
        Inc(I);
    end;
    ReProcessInheritance;
  finally
    if Assigned(fOnEndParsing) then
      fOnEndParsing(Self);
  end;

  fReparsing := False;
  if UpdateView then
    if Assigned(fOnUpdate) then
      fOnUpdate(Self);
end;

procedure TCppParser.InvalidateFile(FileName: TFileName);
var
  I: integer;
  I1: integer;
  P: PIncludesRec;
begin
  if Filename = '' then
    Exit;

  // POSSIBLE PROBLEM:
  // here we delete all the statements that belong to the specified file.
  // what happens with the statements that have _ParentID on one of these???
  // what happens with the statements that inherit from one of these???
  // POSSIBLE WORKAROUND 1: invalidate the other file too (don't like it much...)
  I := 0;
  // delete statements from file
  while I < fStatementList.Count do
    if (AnsiCompareStr(PStatement(fStatementList[I])^._FileName, FileName) = 0) or
      (AnsiCompareStr(PStatement(fStatementList[I])^._DeclImplFileName, FileName) = 0) then begin
      if PStatement(fStatementList[I])^._Kind = skClass then // only classes have inheritance
        fInvalidatedIDs.Add(PStatement(fStatementList[I])^._ID);
      Dispose(PStatement(fStatementList[I]));
      fStatementList.Delete(I);
    end
    else
      Inc(I);
  fStatementList.Pack;
  // delete it from scannedfiles
  I1 := fScannedFiles.IndexOf(FileName);
  if I1 <> -1 then
    fScannedFiles.Delete(I1);
  // remove its include files list
  P := FindIncludeRec(FileName, True);
  if Assigned(P) then
    Dispose(P);
end;

procedure TCppParser.ScanAndSaveGlobals(FileName: TFileName);
var
  I: Integer;
  SR: TSearchRec;
  Path: string;
begin
  Reset;
  for I := 0 to fIncludePaths.Count - 1 do begin
    Path := StringReplace(fIncludePaths[I] + '\', '"', '', [rfReplaceAll]);
    if FindFirst(Path + '*.h', faAnyFile, SR) = 0 then begin
      repeat
        AddFileToScan(Path + SR.Name);
      until FindNext(SR) <> 0;
      FindClose(SR);
    end;
    if FindFirst(Path + '*.hpp', faAnyFile, SR) = 0 then begin
      repeat
        AddFileToScan(Path + SR.Name);
      until FindNext(SR) <> 0;
      FindClose(SR);
    end;
  end;
  ParseLocalHeaders := False;
  ParseGlobalHeaders := False;
  ParseList;
  Save(FileName);
end;

procedure TCppParser.Save(FileName: TFileName);
var
  hFile: integer;
  I, I2, tmp, HowMany: integer;
  MAGIC: array[0..7] of Char;
  P: array[0..4095] of Char;
begin
  MAGIC := 'CPPP 0.1';
  fCacheContents.Assign(fScannedFiles);
  if FileExists(FileName) then
    DeleteFile(FileName);
  hFile := FileCreate(FileName);
  if hFile > 0 then begin
    FileWrite(hFile, MAGIC, SizeOf(MAGIC));

    // write statements
    HowMany := fStatementList.Count - 1;
    FileWrite(hFile, HowMany, SizeOf(Integer));
    for I := 0 to fStatementList.Count - 1 do begin
      with PStatement(fStatementList[I])^ do begin
        if Length(_FullText) > SizeOf(P) then begin
          tmp := FileSeek(hFile, 0, 1);  // retrieve currrent pos
          FileSeek(hFile, SizeOf(Magic), 0); // seek to the number of statements
          HowMany := HowMany - 1;
          FileWrite(hFile, HowMany, SizeOf(Integer)); // write new number of statements
          FileSeek(hFile, tmp, 0); // seek to original offset
          Continue;
        end;
        FileWrite(hFile, _ID, SizeOf(integer));
        FileWrite(hFile, _ParentID, SizeOf(integer));
        FileWrite(hFile, _Kind, SizeOf(byte));
        FileWrite(hFile, _Scope, SizeOf(integer));
        FileWrite(hFile, _ClassScope, SizeOf(integer));
        FileWrite(hFile, _IsDeclaration, SizeOf(boolean));
        FileWrite(hFile, _DeclImplLine, SizeOf(integer));
        FileWrite(hFile, _Line, SizeOf(integer));
        I2 := Length(_FullText);
        FileWrite(hFile, I2, SizeOf(Integer));
        StrPCopy(P, _FullText);
        FileWrite(hFile, P, I2);
        I2 := Length(_Type);
        FileWrite(hFile, I2, SizeOf(Integer));
        StrPCopy(P, _Type);
        FileWrite(hFile, P, I2);
        I2 := Length(_Command);
        FileWrite(hFile, I2, SizeOf(Integer));
        StrPCopy(P, _Command);
        FileWrite(hFile, P, I2);
        I2 := Length(_Args);
        FileWrite(hFile, I2, SizeOf(Integer));
        StrPCopy(P, _Args);
        FileWrite(hFile, P, I2);
        I2 := Length(_ScopelessCmd);
        FileWrite(hFile, I2, SizeOf(Integer));
        StrPCopy(P, _ScopelessCmd);
        FileWrite(hFile, P, I2);
        I2 := Length(_DeclImplFileName);
        FileWrite(hFile, I2, SizeOf(Integer));
        StrPCopy(P, _DeclImplFileName);
        FileWrite(hFile, P, I2);
        I2 := Length(_FileName);
        FileWrite(hFile, I2, SizeOf(Integer));
        StrPCopy(P, _FileName);
        FileWrite(hFile, P, I2);
        I2 := Length(_InheritsFromIDs);
        FileWrite(hFile, I2, SizeOf(Integer));
        StrPCopy(P, _InheritsFromIDs);
        FileWrite(hFile, P, I2);
        I2 := Length(_InheritsFromClasses);
        FileWrite(hFile, I2, SizeOf(Integer));
        StrPCopy(P, _InheritsFromClasses);
        FileWrite(hFile, P, I2);
      end;
    end;

    // write scanned files (cache contents)
    I := fScannedFiles.Count - 1;
    FileWrite(hFile, I, SizeOf(Integer));
    for I := 0 to fScannedFiles.Count - 1 do begin
      I2 := Length(fScannedFiles[I]);
      FileWrite(hFile, I2, SizeOf(Integer));
      StrPCopy(P, fScannedFiles[I]);
      FileWrite(hFile, P, I2);
    end;

    // write file includes list for each file scanned
    I := fIncludesList.Count - 1;
    FileWrite(hFile, I, SizeOf(Integer));
    for I := 0 to fIncludesList.Count - 1 do begin
      I2 := Length(PIncludesRec(fIncludesList[I])^.BaseFile);
      FileWrite(hFile, I2, SizeOf(Integer));
      StrPCopy(P, PIncludesRec(fIncludesList[I])^.BaseFile);
      FileWrite(hFile, P, I2);
      I2 := Length(PIncludesRec(fIncludesList[I])^.IncludeFiles);
      FileWrite(hFile, I2, SizeOf(Integer));
      StrPCopy(P, PIncludesRec(fIncludesList[I])^.IncludeFiles);
      FileWrite(hFile, P, I2);
    end;
    FileClose(hFile);
  end;
  fBaseIndex := fNextID;
end;

procedure TCppParser.Load(FileName: TFileName);
var
  hFile: integer;
  HowMany: integer;
  I, I2: integer;
  MAGIC: array[0..7] of Char;
  Statement: PStatement;
  Buf: array[0..4095] of Char;
  ID_offset, ID_last: integer;
  P: PIncludesRec;
begin
  Reset;
  ID_Offset := fNextID;
  ID_Last := ID_Offset;
  hFile := FileOpen(FileName, fmOpenRead);
  if hFile > 0 then begin
    FileRead(hFile, MAGIC, SizeOf(MAGIC));
    if MAGIC = 'CPPP 0.1' then begin
      // read statements
      FileRead(hFile, HowMany, SizeOf(Integer));
      for I := 0 to HowMany do begin
        Statement := New(PStatement);
        with Statement^ do begin
          FileRead(hFile, _ID, SizeOf(integer));
          FileRead(hFile, _ParentID, SizeOf(integer));
          FileRead(hFile, _Kind, SizeOf(byte));
          FileRead(hFile, _Scope, SizeOf(integer));
          FileRead(hFile, _ClassScope, SizeOf(integer));
          FileRead(hFile, _IsDeclaration, SizeOf(boolean));
          FileRead(hFile, _DeclImplLine, SizeOf(integer));
          FileRead(hFile, _Line, SizeOf(integer));

          FileRead(hFile, I2, SizeOf(Integer));
          FillChar(Buf, SizeOf(Buf), 0);
          FileRead(hFile, Buf, I2);
          _FullText := Buf;
          FileRead(hFile, I2, SizeOf(Integer));
          FillChar(Buf, SizeOf(Buf), 0);
          FileRead(hFile, Buf, I2);
          _Type := Buf;
          FileRead(hFile, I2, SizeOf(Integer));
          FillChar(Buf, SizeOf(Buf), 0);
          FileRead(hFile, Buf, I2);
          _Command := Buf;
          FileRead(hFile, I2, SizeOf(Integer));
          FillChar(Buf, SizeOf(Buf), 0);
          FileRead(hFile, Buf, I2);
          _Args := Buf;
          FileRead(hFile, I2, SizeOf(Integer));
          FillChar(Buf, SizeOf(Buf), 0);
          FileRead(hFile, Buf, I2);
          _ScopelessCmd := Buf;
          FileRead(hFile, I2, SizeOf(Integer));
          FillChar(Buf, SizeOf(Buf), 0);
          FileRead(hFile, Buf, I2);
          _DeclImplFileName := Buf;
          FileRead(hFile, I2, SizeOf(Integer));
          FillChar(Buf, SizeOf(Buf), 0);
          FileRead(hFile, Buf, I2);
          _FileName := Buf;
          FileRead(hFile, I2, SizeOf(Integer));
          FillChar(Buf, SizeOf(Buf), 0);
          FileRead(hFile, Buf, I2);
          _InheritsFromIDs := Buf;
          FileRead(hFile, I2, SizeOf(Integer));
          FillChar(Buf, SizeOf(Buf), 0);
          FileRead(hFile, Buf, I2);
          _InheritsFromClasses := Buf;
          _Loaded := True;
          _NoCompletion := False;
          _Temporary := False;
          _Visible := False;
          _Valid := True;
          _InProject := False;

          // adjust IDs
          if _ID <> -1 then begin
            _ID := _ID + ID_Offset;
            ID_Last := _ID;
          end;
          if _ParentID <> -1 then
            _ParentID := _ParentID + ID_Offset;
        end;
        fStatementList.Add(Statement);
      end;

      // read scanned files - cache contents
      FileRead(hFile, HowMany, SizeOf(Integer));
      for I := 0 to HowMany do begin
        FileRead(hFile, I2, SizeOf(Integer));
        FillChar(Buf, SizeOf(Buf), 0);
        FileRead(hFile, Buf, I2);
        if fScannedFiles.IndexOf(Buf) = -1 then
          fScannedFiles.Add(Buf);
        if fCacheContents.IndexOf(Buf) = -1 then
          fCacheContents.Add(Buf);
      end;

      // read includes info for each scanned file
      FileRead(hFile, HowMany, SizeOf(Integer));
      for I := 0 to HowMany do begin
        P := New(PIncludesRec);
        FileRead(hFile, I2, SizeOf(Integer));
        FillChar(Buf, SizeOf(Buf), 0);
        FileRead(hFile, Buf, I2);
        P^.BaseFile := Buf;
        FileRead(hFile, I2, SizeOf(Integer));
        FillChar(Buf, SizeOf(Buf), 0);
        FileRead(hFile, Buf, I2);
        P^.IncludeFiles := Buf;
        fIncludesList.Add(P);
      end;
    end;
    FileClose(hFile);
  end;
  fNextID := ID_Last + 1;
  fBaseIndex := fStatementList.Count;
  PostProcessInheritance;
end;

procedure TCppParser.PostProcessInheritance;
var
  C, I, I1, I2: integer;
  sl: TStrings;
  S: string;
begin
  sl := TStringList.Create;
  try
    for I := fBaseIndex to fStatementList.Count - 1 do begin
      if PStatement(fStatementList[I])^._Kind = skClass then
        if PStatement(fStatementList[I])^._InheritsFromClasses <> '' then begin
          sl.CommaText := PStatement(fStatementList[I])^._InheritsFromClasses;
          S := '';
          C := 0;
          for I1 := 0 to fStatementList.Count - 1 do
            for I2 := 0 to sl.Count - 1 do
              if PStatement(fStatementList[I1])^._Kind = skClass then
                if AnsiCompareText(sl[I2], PStatement(fStatementList[I1])^._ScopelessCmd) = 0 then begin
                  S := S + IntToStr(PStatement(fStatementList[I1])^._ID) + ',';
                  Inc(C, 1);
                  if C = sl.Count then // found all classes?
                    Break;
                end;
          if C = sl.Count then begin // found all classes?
            PStatement(fStatementList[I])^._InheritsFromClasses := '';
            if S <> '' then
              S := Copy(S, 1, Length(S) - 1); // cut-off ending ','
            PStatement(fStatementList[I])^._InheritsFromIDs := S;
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

function TCppParser.SuggestMemberInsertionLine(ParentID: integer;
  Scope: TStatementClassScope; var AddScopeStr: boolean): integer;
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
  for I := 0 to Statements.Count - 1 do
    if PStatement(Statements[I])^._ParentID = ParentID then begin
      if PStatement(Statements[I])^._IsDeclaration then begin
        if PStatement(Statements[I])^._Line > maxInGeneral then
          maxInGeneral := PStatement(Statements[I])^._Line;
        if PStatement(Statements[I])^._ClassScope = scope then
          if PStatement(Statements[I])^._Line > maxInScope then
            maxInScope := PStatement(Statements[I])^._Line;
      end
      else begin
        if PStatement(Statements[I])^._DeclImplLine > maxInGeneral then
          maxInGeneral := PStatement(Statements[I])^._Line;
        if PStatement(Statements[I])^._ClassScope = scope then
          if PStatement(Statements[I])^._DeclImplLine > maxInScope then
            maxInScope := PStatement(Statements[I])^._DeclImplLine;
      end;
    end;
  if maxInScope = -1 then begin
    AddScopeStr := True;
    Result := maxInGeneral;
  end
  else begin
    AddScopeStr := False;
    Result := maxInScope;
  end;
end;

function TCppParser.GetDeclarationFileName(Statement: PStatement): string;
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

function TCppParser.GetImplementationFileName(
  Statement: PStatement): string;
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
  if not Assigned(List) then
    Exit;

  List.Clear;
  for I := 0 to Statements.Count - 1 do
    if PStatement(Statements[I])^._Kind = skClass then
      List.AddObject(PStatement(Statements[I])^._Command, Pointer(Statements[I]));
end;

function TCppParser.IndexOfStatement(ID: integer): integer;
var
  I: integer;
begin
  Result := -1;
  for I := 0 to Statements.Count - 1 do
    if PStatement(Statements[I])^._ID = ID then begin
      Result := I;
      Break;
    end;
end;

function TCppParser.Locate(Full: string; WithScope: boolean): PStatement;
var
  I: integer;
begin
  Result := nil;
  for I := 0 to fStatementList.Count - 1 do begin
    if WithScope then begin
      if AnsiCompareStr(Full, PStatement(fStatementList[I])^._ScopeCmd) = 0 then begin
        Result := PStatement(fStatementList[I]);
        Break;
      end;
    end
    else begin
      if AnsiCompareStr(Full, PStatement(fStatementList[I])^._Command) = 0 then begin
        Result := PStatement(fStatementList[I]);
        Break;
      end;
    end;
  end;
end;

function TCppParser.FillListOf(Full: string; WithScope: boolean; List: TList): boolean;
var
  I: integer;
begin
  Result := False;
  if not Assigned(List) then
    Exit;
  List.Clear;
  if Full = '' then begin
    List.Assign(fStatementList);
    Result := True;
    Exit;
  end;
  for I := 0 to fStatementList.Count - 1 do begin
    if WithScope then begin
      if AnsiCompareStr(Full, PStatement(fStatementList[I])^._ScopeCmd) = 0 then begin
        Result := True;
        List.Add(PStatement(fStatementList[I]));
      end;
    end
    else begin
      if AnsiCompareStr(Full, PStatement(fStatementList[I])^._Command) = 0 then begin
        Result := True;
        List.Add(PStatement(fStatementList[I]));
      end;
    end;
  end;
end;

function TCppParser.FindAndScanBlockAt(Filename: string; Row: integer; Stream: TStream): integer;
  function GetFuncStartLine(const Index, StartLine: integer): integer;
  var
    idx: integer;
  begin
    idx := Index;
    Result := Index;
    while idx < fTokenizer.Tokens.Count do begin
      if PToken(fTokenizer.Tokens[idx])^.Line = StartLine then begin
        while (idx < fTokenizer.Tokens.Count) and (PToken(fTokenizer.Tokens[idx])^.Text[1] <> '{') do
          Inc(idx);
        if (idx < fTokenizer.Tokens.Count) and (PToken(fTokenizer.Tokens[idx])^.Text[1] = '{') then begin
          Result := idx; // + 1;
          Break;
        end;
      end;
      Inc(idx);
    end;
  end;
  function GetFuncEndLine(const Index: integer): integer;
  var
    iLevel: integer;
    idx: integer;
  begin
    idx := Index;
    iLevel := 0; // when this goes negative, we 're there (we have skipped the opening brace already)
    while (idx < fTokenizer.Tokens.Count) and (iLevel >= 0) do begin
      if PToken(fTokenizer.Tokens[idx])^.Text[1] = '{' then
        Inc(iLevel)
      else if PToken(fTokenizer.Tokens[idx])^.Text[1] = '}' then
        Dec(iLevel);
      Inc(idx);
    end;
    Result := idx;
  end;
var
  I: integer;
  ClosestStatement: integer;
  ClosestLine: integer;
  Done: boolean;
  loFilename: string;
  sExt: string;
begin
  // finds the function in the specified filename that contains the line Row,
  // and parses it...
  DeleteTemporaries;
  Result := -1;
  ClosestLine := -1;
  ClosestStatement := -1;
  loFilename := LowerCase(Filename);
  for I := 0 {fBaseIndex} to fStatementList.Count - 1 do
    if PStatement(fStatementList[I])^._Kind in [skFunction, skConstructor, skDestructor] then
      if (AnsiCompareText(PStatement(fStatementList[I])^._FileName, loFilename) = 0) then begin
        if (PStatement(fStatementList[I])^._Line <= Row) and (PStatement(fStatementList[I])^._Line > ClosestLine) then begin
          ClosestStatement := I;
          ClosestLine := PStatement(fStatementList[I])^._Line;
        end;
      end
      else if (AnsiCompareText(PStatement(fStatementList[I])^._DeclImplFileName, loFilename) = 0) then begin
        if (PStatement(fStatementList[I])^._DeclImplLine <= Row) and (PStatement(fStatementList[I])^._DeclImplLine > ClosestLine) then begin
          ClosestStatement := I;
          ClosestLine := PStatement(fStatementList[I])^._DeclImplLine;
        end;
      end;

  if (ClosestStatement <> -1) then begin
    // found!
    Result := IndexOfStatement(PStatement(fStatementList[ClosestStatement])^._ParentID);
    fTokenizer.Reset;
    if Assigned(Stream) then
      fTokenizer.Tokenize(Stream)
    else
      fTokenizer.Tokenize(Filename);
    fIndex := 0;
    fLevel := 0;
    Done := False;

    // find start of function and start from the opening brace
    fIndex := GetFuncStartLine(0, ClosestLine);
    // now find the end of the function and check that the Row is still in scope
    I := GetFuncEndLine(fIndex + 1);

    // if we 're past the end of function, we are not in the scope...
    if (Row > PToken(fTokenizer.Tokens[I - 1])^.Line) or (Row < PToken(fTokenizer.Tokens[fIndex])^.Line) then begin
      ClosestLine := PStatement(fStatementList[ClosestStatement])^._DeclImplLine;
      fIndex := GetFuncStartLine(0, ClosestLine);
      I := GetFuncEndLine(fIndex + 1);
      if PToken(fTokenizer.Tokens[I - 1])^.Line < Row then begin
        Result := -1;
        Exit;
      end;
    end;

    fLaterScanning := True;
    fCurrentFile := loFileName;
    fLastID := -1;
    sExt := ExtractFileExt(loFileName);
    fIsHeader := (sExt = '.h') or (sExt = '.hpp') or (sExt = '.hh');
    fCurrentClass := TIntList.Create;
    fCurrentClassLevel := TIntList.Create;
    fSkipList := TIntList.Create;
    fLastStatementKind := skUnknown;
    try
      // add the all-important "this" pointer as a local variable
      if Result <> -1 then
        fThisPointerID := AddStatement(-1,
          PStatement(fStatementList[ClosestStatement])^._ParentID, //Result,
          Filename,
          PStatement(fStatementList[Result])^._Command + '* this',
          PStatement(fStatementList[Result])^._Command + '*',
          'this',
          '',
          1,
          skVariable,
          ssClassLocal,
          scsPrivate,
          False,
          True);
      // add the function's args
      ScanMethodArgs(PStatement(fStatementList[ClosestStatement])^._Args,
        True,
        Filename,
        PStatement(fStatementList[ClosestStatement])^._Line,
        PStatement(fStatementList[ClosestStatement])^._ParentID);
      repeat
        if PToken(fTokenizer.Tokens[fIndex])^.Text[1] = '{' then begin
          Inc(fLevel, 2);
          Inc(fIndex);
        end
        else if PToken(fTokenizer.Tokens[fIndex])^.Text = '}' then begin
          Dec(fLevel, 2);
          Inc(fIndex);
          Done := fLevel < 0;
        end
        else if CheckForPreprocessor then begin
          HandlePreprocessor;
        end
        else if CheckForKeyword then begin
          HandleKeyword;
        end
        else if CheckForEnum then begin
          HandleEnum;
        end
        else if CheckForVar then begin
          HandleVar;
        end
        else
          Inc(fIndex);

        CheckForSkipStatement;

        // stop at cursor line - everything beyond it, is out of scope ;)
        Done := Done or (fIndex >= fTokenizer.Tokens.Count) or (PToken(fTokenizer.Tokens[fIndex])^.Line >= Row);
      until Done;
    finally
      fSkipList.Clear;
      fCurrentClassLevel.Clear;
      FCurrentClass.Clear;
      FreeAndNil(fSkipList);
      FreeAndNil(fCurrentClassLevel);
      FreeAndNil(FCurrentClass);
    end;
  end;
  fLaterScanning := False;
end;

procedure TCppParser.DeleteTemporaries;
var
  I: integer;
begin
  I := fBaseIndex;
  while I < fStatementList.Count do begin
    if PStatement(fStatementList[I])^._Temporary then begin
      Dispose(PStatement(fStatementList[I]));
      fStatementList.Delete(I);
    end
    else
      Inc(I);
  end;
  fThisPointerID := -1;
end;

function TCppParser.ScanMethodArgs(ArgStr: string; AddTemps: boolean; Filename: string; Line, ClassID: integer): string;
  function GetWordAt(Str: string; var Index: integer; JustPeek: boolean): string;
  var
    IdxBkp: integer;
  begin
    Result := '';
    IdxBkp := Index;
    if Str = '' then
      Exit;
    if Length(Str) < Index then
      Exit;

    // first skip leading spaces
    while (Index <= Length(Str)) and (Str[Index] in [' ', #9, #10, #13]) do
      Inc(Index);

    // now get the word at Index
    if (Index <= Length(Str)) and (Str[Index] in ['&', '*']) then begin
      while (Index <= Length(Str)) and (Str[Index] in ['&', '*']) do begin
        Result := Result + Str[Index];
        Inc(Index);
      end;
    end
    else begin
      while (Index <= Length(Str)) and not (Str[Index] in [' ', #9, #10, #13]) do begin
        Result := Result + Str[Index];
        Inc(Index);
      end;
    end;
    if JustPeek then
      Index := IdxBkp;
  end;

  function IsKnown(Str: string): boolean;
  begin
    // standard C types
    Result := (AnsiCompareStr(Str, '*') = 0) or
      (AnsiCompareStr(Str, '&') = 0) or
      (AnsiCompareStr(Str, 'int') = 0) or
      (AnsiCompareStr(Str, 'bool') = 0) or
      (AnsiCompareStr(Str, 'char') = 0) or
      (AnsiCompareStr(Str, 'uint') = 0) or
      (AnsiCompareStr(Str, 'uint8') = 0) or
      (AnsiCompareStr(Str, 'uint16') = 0) or
      (AnsiCompareStr(Str, 'uint32') = 0) or
      (AnsiCompareStr(Str, '...') = 0) or
      (AnsiCompareStr(Str, 'const') = 0) or
      (AnsiCompareStr(Str, 'static') = 0);
  end;

  procedure AddSt(_Type, _Ident: string);
  begin
    AddStatement(-1,
      ClassID,
      Filename,
      _Type + ' ' + _Ident,
      _Type,
      _Ident,
      '',
      Line,
      skVariable,
      ssClassLocal,
      scsPrivate,
      False,
      True);
  end;
var
  idx: integer;
  tmpStr: string;
  S: string;
  LastType: string;
begin
  // ArgStr contains the method args enclosed in parenthesis.
  // What we 'll do here is remove the vars from the args.
  // e.g. (char * tmp, int x) will be (char *, int)
  // what's important and will help us here is this rule:
  // because we don't know the type of the var and it may consist
  // of one, two or more words, we 'll strip out the last word
  // before a ',' or ')'. The identifier is only one word ;)
  // The tough part is that we don't know if this is a declaration
  // or an implementation, so we might encounter a function call
  // inside the ArgStr. But this will be taken care of in GetWordAt.
  Result := ArgStr;
  if (ArgStr = '') or (ArgStr[1] <> '(') or (ArgStr[Length(ArgStr)] <> ')') then
    Exit;

  Result := '';
  LastType := '';
  tmpStr := Copy(ArgStr, 2, Length(ArgStr) - 1); // remove '('
  idx := 1;
  S := '';
  repeat
    S := GetWordAt(tmpStr, idx, False);
    if S <> '' then begin
      // check if we got a ','
      if S[Length(S)] in [',', ')'] then begin
        if Length(S) > 1 then begin
          if IsKnown(Copy(S, 1, Length(S) - 1)) then
            Result := Result + S + ' '
          else begin
            Result := Trim(Result) + S[Length(S)] + ' '; // identifier
            if AddTemps then
              AddSt(Trim(LastType), Copy(S, 1, Length(S) - 1));
          end;
        end
        else
          Result := Trim(Result) + S + ' ';
        LastType := '';
      end
      else if (GetWordAt(tmpStr, idx, True) = ',') or
        (GetWordAt(tmpStr, idx, True) = ')') or
        (GetWordAt(tmpStr, idx, True) = '') then begin // this peeks at the following word
        // this is the last word before the comma
        // we must check that it is not a known type
        // (don't forget: the declaration might *not* have identifiers in it)
        if IsKnown(S) then
          Result := Result + S + ' '
        else if AddTemps then // identifier
          AddSt(Trim(LastType), S);
        LastType := '';
      end
      else begin
        Result := Result + S + ' ';
        LastType := LastType + S + ' ';
      end;
    end;
  until S = '';
  Result := '(' + Trim(Result);
end;

function TCppParser.FindIncludeRec(Filename: string; DeleteIt: boolean): PIncludesRec;
var
  I: integer;
begin
  Result := nil;
  for I := 0 to fIncludesList.Count - 1 do
    if PIncludesRec(fIncludesList[I])^.BaseFile = Filename then begin
      Result := PIncludesRec(fIncludesList[I]);
      if DeleteIt then
        fIncludesList.Delete(I);
      Break;
    end;
end;

function TCppParser.GetFileIncludes(Filename: string): string;
  procedure RecursiveFind(Fname: string);
  var
    I: integer;
    P: PIncludesRec;
    sl: TStrings;
  begin
    if Fname = '' then
      Exit;
    fFileIncludes.Add(FName);
    P := FindIncludeRec(Fname);
    if Assigned(P) then begin
      // recursively search included files
      sl := TStringList.Create;
      try
        sl.CommaText := P^.IncludeFiles;
        for I := 0 to sl.Count - 1 do
          if fFileIncludes.IndexOf(sl[I]) = -1 then begin
            fFileIncludes.Add(sl[I]);
            RecursiveFind(sl[I]);
          end;
      finally
        sl.Free;
      end;
    end;
  end;
begin
  // returns a ';' separated list of all included files in file Filename
  Result := '';
  fFileIncludes.Clear;
  fFileIncludes.Sorted := True;
  fFileIncludes.Duplicates := dupIgnore;

  RecursiveFind(LowerCase(Filename));
  Result := fFileIncludes.CommaText;
end;

function TCppParser.GetThisPointerID: integer;
begin
  Result := fThisPointerID;
end;

end.

