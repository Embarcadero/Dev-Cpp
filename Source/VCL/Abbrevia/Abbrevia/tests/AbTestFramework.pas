(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Abbrevia
 *
 * The Initial Developer of the Original Code is
 * Robert Love
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Craig Peterson
 *
 * ***** END LICENSE BLOCK ***** *)

unit AbTestFramework;

{$I AbDefine.inc}

interface

uses
  TestFramework,
{$IFDEF LibcAPI}
  Libc,
{$ENDIF}
{$IFDEF FPCUnixAPI}
  BaseUnix, Unix,
{$ENDIF}
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, TypInfo,
  AbBrowse;

type
  TAbDirEntry = record
    Name: string;
    Size: Int64;
  end;
  TAbDirEntries = array of TAbDirEntry;

  TAbTestCase = class(TTestCase)
  private
    FTempDirCreated: Boolean;
  protected
    function GetTestTempDir : string;
    function GetWindowsDir : string;
    procedure CheckFilesMatch(const AFileName1, aFileName2 : string;
      const aMsg : string = '');
    procedure CheckFileMatchesStream(const aFileName : string; aStream : TStream;
      const aMsg : string = '');
    procedure CheckStreamMatch(aStream1, aStream2 : TStream; const aMsg : string = '');
    procedure CheckFileExists(aFileName : string);
    procedure CheckDirExists(aFileName : string);
    procedure CreateDummyFile(aFileName : string; aSize : Integer);
    procedure SetUp; override;
    procedure TearDown; override;
    class function FilesInDirectory(const aDir : string) : TAbDirEntries;
    procedure CheckDirMatch(aDir1, aDir2 : string);
    // Call this routine with GREAT Caution!!!!
    procedure DelTree(aDir : string);

  public
    class function TestFileDir: string;
    class function CanterburyDir: string;
    class function CanterburySourceDir: string;
    class function MPLDir: string;

    property TestTempDir : string
      read GetTestTempDir;
  end;

  TAbCompTestCase = class(TAbTestCase)
  private
    // RTTI function so that they match from version to version of delphi.
    function AbGetPropList(TypeInfo : PTypeInfo; out PropList : PPropList) : Integer;
    function AbGetPropValue(Instance : TObject; const PropName : string; PreferStrings : Boolean = True) : Variant;

  protected
    function StreamComponent(aComp : TComponent) : string;
    function UnStreamComponent(const aCompStr : string; Instance : TComponent = nil) : TComponent;
    procedure CompareComponentProps(aComp1, aComp2 : TPersistent;
      const aIgnoreProps: string = '');
    procedure TestComponentLink(AComponent: TComponent;
      const APropName: string; APropClass: TComponentClass);
    procedure TestDefaultStreaming(AComponent: TComponent);
  end;

  TAbTestMeter = class(TComponent, IAbProgressMeter)
    procedure DoProgress(Progress : Byte);
    procedure Reset;
  end;


implementation

{$WARN SYMBOL_PLATFORM OFF}

uses
{$IFDEF PosixAPI}
  Posix.SysStat,
{$ENDIF}
  Math, SysUtils, IOUtils, Variants,
  AbUtils;

var
  ExePath : string;

{ ===== TAbTestCase ======================================================== }
procedure TAbTestCase.CheckDirMatch(aDir1, aDir2 : string);
var
  d1, d2 : TAbDirEntries;
  i, j, cmp : Integer;
begin
  // When listing directories with Unicode filenames in pre-Unicode Delphis,
  // the directory listing will include files with names names containing
  // best-fit mappings that prevent actually opening those files.  The Unicode
  // tests all use 0-byte files, so when decompressing we can just check that
  // the files exist and the sizes match, and when compressing we skip files
  // that FileExists fails for so we don't fail the test for files we can't open.
  d1 := FilesInDirectory(aDir1);
  d2 := FilesInDirectory(aDir2);
  i := 0;
  j := 0;
  while (i < Length(d1)) or (j < Length(d2)) do begin
    if i = Length(d1) then cmp := 1
    else if j = Length(d2) then cmp := -1
    else cmp := CompareText(d1[i].Name, d2[j].Name); // Allow case insensitive matches on case-sensitive filesystems
    if cmp < 0 then begin
      Check(not FileExists(TPath.Combine(aDir1, d1[i].Name)),
        d1[i].Name + ' is missing in ' + aDir2);
      Inc(i);
    end
    else if cmp > 0 then begin
      Check(not FileExists(TPath.Combine(aDir2, d2[j].Name)),
        d2[j].Name + ' is missing in ' + aDir1);
      Inc(j);
    end
    else begin
      CheckEquals(d1[i].Size, d2[j].Size, d1[i].Name + 'sizes do not match');
      if (d1[i].Size > 0) or (d2[j].Size > 0) then
        CheckFilesMatch(TPath.Combine(aDir1, d1[i].Name), TPath.Combine(aDir2, d2[i].Name),
          d1[i].Name + ' does not match');
      Inc(i);
      Inc(j);
    end;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbTestCase.CheckFileExists(aFileName : string);
begin
  Check(FileExists(aFileName), 'Unable to locate file: ' + aFileName);
end;
{ -------------------------------------------------------------------------- }
procedure TAbTestCase.CheckDirExists(aFileName : string);
begin
  Check(DirectoryExists(aFileName), 'Unable to locate directory: ' + aFileName);
end;
{ -------------------------------------------------------------------------- }
procedure TAbTestCase.CheckFilesMatch(const aFileName1, aFileName2: string;
  const aMsg: string = '');
var
  Stream1, Stream2 : TStream;
begin
  Stream1 := TFileStream.Create(aFileName1, fmOpenRead or fmShareDenyWrite);
  try
    Stream2 := TFileStream.Create(AFileName2, fmOpenRead or fmShareDenyWrite);
    try
      CheckStreamMatch(Stream1, Stream2, aMsg);
    finally
      Stream2.Free;
    end;
  finally
    Stream1.Free;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbTestCase.CheckFileMatchesStream(const aFileName : string;
  aStream : TStream; const aMsg : string = '');
var
  FileStream : TStream;
begin
  FileStream := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
  try
    CheckStreamMatch(FileStream, aStream, aMsg);
  finally
    FileStream.Free;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbTestCase.CheckStreamMatch(aStream1, aStream2 : TStream;
  const aMsg: string = '');
var
  I, BytesRead, BufSize : Integer;
  Buf1, Buf2 : array of Byte;
begin
  CheckEquals(aStream1.Size, aStream2.Size, aMsg);
  if aStream1.Size = 0 then
    Exit;
  aStream1.Seek(0, soFromBeginning);
  aStream2.Seek(0, soFromBeginning);
  BufSize := Min(aStream1.Size, 32768);
  SetLength(Buf1, BufSize);
  SetLength(Buf2, BufSize);
  while True do begin
    BytesRead := aStream1.Read(Buf1[0], BufSize);
    Check(BytesRead = aStream2.Read(Buf2[0], BufSize), 'Bytes read mismatch');
    if BytesRead = 0 then
      Exit;
    if not CompareMem(Pointer(Buf1), Pointer(Buf2), BytesRead) then
      for i := 0 to BytesRead - 1 do
        if Buf1[i] <> Buf2[i] then
          FailEquals(IntToHex(Buf1[i], 2), IntToHex(Buf2[i], 2), 'Bytes do not match: ' + aMsg);
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbTestCase.CreateDummyFile(aFileName : string; aSize : Integer);
var
  fs : TFileStream;
  bf : pointer;
begin
  fs := TFileStream.Create(aFileName, fmCreate);
  try
    GetMem(bf, aSize + 1);
    try
      // Fill with dummy data might be better in the future to fill less compressable
      // data.
      FillChar(bf^, aSize, 26);
      Fs.Write(bf^, aSize);
    finally
      FreeMem(bf, aSize + 1);
    end;
  finally
    FS.Free;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbTestCase.DelTree(aDir : string);
var
  SR : TSearchRec;
  Directories, Files : TStringList; // FindFirst/FindNext locks directories
  I : Integer;
begin
  // Force Slash
  if aDir[Length(aDir)] <> PathDelim then
    aDir := aDir + PathDelim;
  // If a File is found
  if FindFirst(aDir + '*', faAnyFile, SR) = 0 then begin
    Directories := TStringList.Create;
    Files := TStringList.Create;
    try
      repeat
        if (SR.Attr and faDirectory > 0) then begin
          if (SR.Name <> '.') and (SR.Name <> '..') then
            Directories.Add(aDir + SR.Name)
        end
        else
          Files.Add(aDir + SR.Name);
        // Make sure we can delete files and traverse directories
        {$IFDEF MSWINDOWS}
        if (SR.Attr and faReadOnly) <> 0 then
          FileSetAttr(aDir + SR.Name, SR.Attr and not faReadOnly);
        {$ENDIF}
        {$IFDEF UNIX}
        {$IFDEF FPCUnixAPI}
        if fpS_ISDIR(SR.Mode) and (SR.Mode and (S_IWUSR or S_IXUSR) <> S_IWUSR or S_IXUSR) then
          FpChmod(PAnsiChar(AbSysString(aDir + SR.Name)), SR.Mode or S_IWUSR or S_IXUSR);
        {$ELSE}
        if S_ISDIR(SR.Mode) and (SR.Mode and (S_IWUSR or S_IXUSR) <> S_IWUSR or S_IXUSR) then
          chmod(PAnsiChar(AbSysString(aDir + SR.Name)), SR.Mode or S_IWUSR or S_IXUSR);
        {$ENDIF !FPCUnixAPI}
        {$ENDIF UNIX}
      until FindNext(SR) <> 0;
      // Close search to free locks on files/directories
      FindClose(SR);
      // Delete all files in directory
      for I := 0 to Files.Count - 1 do
        if not DeleteFile(Files[I]) then
          raise Exception.CreateFmt('Unable to delete "%s"', [Files[I]]);
      // Recursivly call DelTree to get rid of subdirectories
      for I := 0 to Directories.Count -1 do
        DelTree(Directories[I]);
      // Finally remove the directory
      if not RemoveDir(aDir) then
        raise Exception.CreateFmt('Unable to delete "%s"', [aDir]);
    finally
      Directories.Free;
      Files.Free;
    end;
  end; { If File Found with FindFirst }
end;
{ -------------------------------------------------------------------------- }
class function TAbTestCase.FilesInDirectory(const aDir : string) : TAbDirEntries;
var
  SR : TSearchRec;
  i, j: Integer;
  Temp: TAbDirEntry;
begin
  // Retrieve both files and sizes for files.  See CheckDirMatch for details.
  if not DirectoryExists(aDir) then
    raise ETestFailure.Create('Directory Requested does not exist : ' + aDir);
  Result := nil;
  if FindFirst(TPath.Combine(aDir,  '*'), faAnyFile, SR) = 0 then begin
    repeat
      if (SR.Attr and faDirectory = 0) and  // Don't include sub directories
         (Pos('?', SR.Name) = 0) then begin // Don't include Unicode filenames in ANSI builds
        SetLength(Result, Length(Result) + 1);
        with Result[Length(Result) - 1] do begin
          Name := SR.Name;
          Size := SR.Size;
        end;
      end;
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
  // Sort results
  for i := Low(Result) to High(Result) do
    for j := Succ(i) to High(Result) do
      if CompareText(Result[i].Name, Result[j].Name) > 0 then begin
        Temp := Result[i];
        Result[i] := Result[j];
        Result[j] := Temp
      end;
end;
{ -------------------------------------------------------------------------- }
class function TAbTestCase.TestFileDir: string;
begin
  // May want to place in ini file in the future but this will do for now
  Result := ExePath + 'testfiles' + PathDelim;
end;
{ -------------------------------------------------------------------------- }
class function TAbTestCase.CanterburyDir: string;
begin
  Result := TestFileDir + 'Canterbury' + PathDelim;
end;
{ -------------------------------------------------------------------------- }
class function TAbTestCase.CanterburySourceDir: string;
begin
  Result := CanterburyDir + 'source' + PathDelim;
end;
{ -------------------------------------------------------------------------- }
class function TAbTestCase.MPLDir: string;
begin
  Result := TestFileDir + 'MPL' + PathDelim
end;
{ -------------------------------------------------------------------------- }
function TAbTestCase.GetTestTempDir: string;
begin
  Result := TestFileDir + 'temp' + PathDelim;
  if not FTempDirCreated then begin
    CreateDir(Result);
    FTempDirCreated := True;
  end;
end;
{ -------------------------------------------------------------------------- }
function TAbTestCase.GetWindowsDir: string;
{$IFDEF MSWINDOWS}
var
  aDirBuf : Array[0..MAX_PATH] of Char;
{$ENDIF}
begin
// Windows Directory is used to find
 {$IFDEF UNIX}
   result := '/etc/'
 {$ELSE}
   GetWindowsDirectory(aDirBuf,SizeOf(aDirBuf));
   result := IncludeTrailingPathDelimiter(string(aDirBuf));
 {$ENDIF}
end;
{ -------------------------------------------------------------------------- }
procedure TAbTestCase.SetUp;
begin
  inherited;
end;
{ -------------------------------------------------------------------------- }
procedure TAbTestCase.TearDown;
begin
  inherited;
  if FTempDirCreated then begin
    try
      DelTree(TestTempDir);
    except
      // Retry after a delay to handle something keeping the files open (e.g., anti-virus)
      Sleep(500);
      DelTree(TestTempDir);
    end;
    FTempDirCreated := False;
  end;
end;

{ ===== TAbCompTestCase ==================================================== }
function TAbCompTestCase.StreamComponent(aComp : TComponent) : string;
// The Following was cut and paste out of the Delphi Help File.
var
  BinStream : TMemoryStream;
  StrStream : TStringStream;
begin
  BinStream := TMemoryStream.Create;
  try
    StrStream := TStringStream.Create('');
    try
      BinStream.WriteComponent(aComp);
      BinStream.Seek(0, soFromBeginning);
      ObjectBinaryToText(BinStream, StrStream);
      StrStream.Seek(0, soFromBeginning);
      Result := StrStream.DataString;
    finally
      StrStream.Free;
    end;
  finally
    BinStream.Free
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCompTestCase.CompareComponentProps(aComp1, aComp2 : TPersistent;
  const aIgnoreProps: string = '');

  function GetSubComp(aComp: TPersistent; const PName: string): TObject;
  var
    PropInfo: PPropInfo;
    Intf: IInterface;
    Ref: IInterfaceComponentReference;
  begin
    Result := nil;
    PropInfo := GetPropInfo(aComp.ClassInfo, PName);
    if Assigned(PropInfo) then
      if PropInfo.PropType^.Kind = tkClass then
        Result := TObject(GetOrdProp(aComp, PropInfo))
      else begin
        Intf := GetInterfaceProp(aComp, PropInfo);
        if Supports(Intf, IInterfaceComponentReference, Ref) then
          Result := Ref.GetComponent;
      end;
  end;

var
  PList1, PList2 : PPropList;
  PListCnt1, PListCnt2: Integer;
  I: Integer;
  SubComp1, SubComp2: TObject;
  PName1, PName2: string;
  IgnoreProps: TStringList;
begin
  // Check all published properties to see if same.
  PListCnt1 := AbGetPropList(PTypeInfo(aComp1.ClassInfo),PList1);
  PListCnt2 := AbGetPropList(PTypeInfo(aComp2.ClassInfo),PList2);
  IgnoreProps := TStringList.Create;
  try
    IgnoreProps.CommaText := aIgnoreProps;
    // The following should not fail but it here just in case!
    Check(PListCnt1 = PListCnt2, aComp1.ClassName + ' streaming is really screwed up!');
    for I := 0 to PListCnt1 -1 do
    begin
      PName1 := string(PList1^[I]^.Name);
      PName2 := string(PList2^[I]^.Name);
      if IgnoreProps.IndexOf(PName1) >= 0 then
        Continue;
      case PList1^[I]^.PropType^.Kind of
        tkClass, tkInterface:
          begin
            SubComp1 := GetSubComp(aComp1, PName1);
            SubComp2 := GetSubComp(aComp2, PName2);
            Check(Assigned(SubComp1) = Assigned(SubComp2),
              'Stream Problem with ' +aComp1.ClassName + '.' + PName2);
            if Assigned(SubComp1) and (SubComp1 is TPersistent) and (SubComp2 is TPersistent) then
              CompareComponentProps(SubComp1 as TPersistent, SubComp2 as TPersistent);
          end;
        else
          Check(AbGetPropValue(aComp1, PName1) = AbGetPropValue(aComp2, PName2),
            'Stream Problem with ' + aComp1.ClassName + '.' + PName1)
      end;
    end;
  finally
    IgnoreProps.Free;
    FreeMem(PList1);
    FreeMem(PList2);
  end;
end;
{ -------------------------------------------------------------------------- }
function TAbCompTestCase.UnStreamComponent(const aCompStr : string;
                                           Instance : TComponent) : TComponent;
// The Following was Cut and Paste from the Delphi Help file
var
  StrStream:TStringStream;
  BinStream: TMemoryStream;
  ErrStream  : TFileStream; 
begin
{$IF COMPILERVERSION < 32}
  Result := nil;
{$ENDIF}

  StrStream := TStringStream.Create(aCompStr);
  try
    BinStream := TMemoryStream.Create;
    try
      try
        ObjectTextToBinary(StrStream, BinStream);
      except
        on E : EParserError do
          begin
            ErrStream := TFileStream.Create('parse.err', fmCreate);
            StrStream.Seek(0, soFromBeginning);
            ErrStream.CopyFrom(StrStream, StrStream.Size);
            ErrStream.Free;
            Fail('Check parse.err ' + E.Message,nil);
            raise;
          end
      end;
      BinStream.Seek(0, soFromBeginning);
      Result := BinStream.ReadComponent(Instance);
    finally
      BinStream.Free;
    end;
  finally
    StrStream.Free;
  end;
end;
{ -------------------------------------------------------------------------- }
function TAbCompTestCase.AbGetPropList(TypeInfo: PTypeInfo;
  out PropList: PPropList): Integer;
begin
  Result := GetTypeData(TypeInfo)^.PropCount;
  if Result > 0 then
  begin
    GetMem(PropList, Result * SizeOf(Pointer));
    GetPropInfos(TypeInfo, PropList);
  end
  else
    PropList := nil;
end;
{ -------------------------------------------------------------------------- }
function TAbCompTestCase.AbGetPropValue(Instance: TObject;
  const PropName: string; PreferStrings: Boolean): Variant;
var
  PropInfo: PPropInfo;
  S: TIntegerSet;
  TInfo: PTypeInfo;
  I: Integer;
begin
  // assume failure
  Result := Null;

  // get the prop info
  PropInfo := GetPropInfo(Instance.ClassInfo, PropName);
  if PropInfo = nil then
    Raise Exception.Create('Property "' + PropName + '" was not found.')
  else
  begin
    // return the right type
    case PropInfo^.PropType^.Kind of
      tkInteger, tkChar, tkWChar, tkClass:
        Result := GetOrdProp(Instance, PropInfo);
      {$IFDEF FPC}
      tkBool:
        Result := Boolean(GetOrdProp(Instance, PropInfo));
      {$ENDIF}
      tkEnumeration:
        if PreferStrings then
          Result := GetEnumName(PropInfo^.PropType{$IFNDEF FPC}^{$ENDIF}, GetOrdProp(Instance, PropInfo))
        {$IFNDEF FPC}
        else if GetTypeData(PropInfo^.PropType^)^.BaseType^ = TypeInfo(Boolean) then
          Result := Boolean(GetOrdProp(Instance, PropInfo))
        {$ENDIF}
        else
          Result := GetOrdProp(Instance, PropInfo);
      tkSet:
        if PreferStrings then
          begin
            Result := '';
            Integer(S) := GetOrdProp(Instance, PropInfo);
            TInfo := GetTypeData(PropInfo^.PropType{$IFNDEF FPC}^{$ENDIF})^.CompType{$IFNDEF FPC}^{$ENDIF};
            for I := 0 to SizeOf(Integer) * 8 - 1 do
            if I in S then
            begin
              if Result <> '' then
                Result := Result + ',';
              Result := Result + GetEnumName(TInfo, I);
            end;
          end
        else
          Result := GetOrdProp(Instance, PropInfo);
      tkFloat:
        Result := GetFloatProp(Instance, PropInfo);
      tkMethod:
        Result := PropInfo^.PropType^.Name;
      tkString, tkLString {$IFDEF FPC}, tkAString{$ENDIF}:
        Result := GetStrProp(Instance, PropInfo);
      tkWString:
        Result := GetWideStrProp(Instance, PropInfo);
      tkVariant:
        Result := GetVariantProp(Instance, PropInfo);
      tkInt64:
        Result := GetInt64Prop(Instance, PropInfo);
      tkDynArray:
        DynArrayToVariant(Result, Pointer(GetOrdProp(Instance, PropInfo)), PropInfo^.PropType);
      {$IF DECLARED(tkUString)}
      tkUString:
        Result := GetStrProp(Instance, PropInfo);
      {$IFEND}
      // Unsupported FPC property types
      // tkUnknown,tkArray,tkRecord,tkInterface,tkObject,tkQWord,tkInterfaceRaw
    else
      raise Exception.Create('Invalid Property Type: ' + string(PropInfo.PropType^.Name));
    end;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCompTestCase.TestComponentLink(AComponent: TComponent;
  const APropName: string; APropClass: TComponentClass);
  {- Create a component of the given type, assign it to the property, then
     free the object and test that the property has been nilled. }
var
  PropIntf: IInterface;
  PropObject: TComponent;
begin
  PropObject := APropClass.Create(AComponent.Owner);
  if PropType(AComponent, APropName) = tkInterface then begin
    SetInterfaceProp(AComponent, APropName, PropObject);
    PropIntf := GetInterfaceProp(AComponent, APropName);
    Check((PropIntf <> nil) and PropObject.IsImplementorOf(PropIntf),
      Format('SetInterfaceProp failed for %s.%s', [AComponent.ClassName, APropName]));
    PropIntf := nil;
    PropObject.Free;
    CheckNull(GetInterfaceProp(AComponent, APropName),
      Format('Notification does not work for %s.%s', [AComponent.ClassName, APropName]));
  end
  else begin
    SetObjectProp(AComponent, APropName, PropObject);
    Check(GetObjectProp(AComponent, APropName) = PropObject,
      Format('SetObjectProp failed for %s.%s', [AComponent.ClassName, APropName]));
    PropObject.Free;
    CheckNull(GetObjectProp(AComponent, APropName),
      Format('Notification does not work for %s.%s', [AComponent.ClassName, APropName]));
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbCompTestCase.TestDefaultStreaming(AComponent: TComponent);
var
  CompStr: string;
  CompTest: TComponent;
begin
  RegisterClass(TComponentClass(AComponent.ClassType));
  CompStr := StreamComponent(AComponent);
  CompTest := UnStreamComponent(CompStr);
  CompareComponentProps(AComponent, CompTest);
  UnRegisterClass(TComponentClass(AComponent.ClassType));
end;

{ ===== TAbCompTestCase ==================================================== }
procedure TAbTestMeter.DoProgress(Progress : Byte);
begin
  // No-op
end;
{ -------------------------------------------------------------------------- }
procedure TAbTestMeter.Reset;
begin
  // No-op
end;


initialization
  // Cache on startup;  on Linux ParamStr(0) may not be fully qualified, and
  // the tests change the working directory.
  ExePath := ExtractFilePath(ExpandFileName(ParamStr(0)))

end.
