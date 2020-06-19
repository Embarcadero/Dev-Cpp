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
 * Craig Peterson
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

unit AbGzTypTests;

{$I AbDefine.inc}

interface

uses
  Classes, AbArcTypTests, AbTestFrameWork, AbArcTyp, AbGzTyp, AbUtils;

type
  TAbGzipExtraFieldTests = class(TAbTestCase)
  private
    FExtraField: TAbGzipExtraField;
    FGzHeader: TAbGzHeader;
    procedure CheckFieldEquals(const aExpectedData; aExpectedSize : Word);
    procedure CheckMemEquals(aExpectedData : Pointer; aExpectedSize : Integer;
      aActualData : Pointer; aActualSize : Integer; const aMsg : string);
    procedure CheckSubfieldCount(aExpected : Integer);
    procedure CheckSubfieldEquals(aID : TAbGzExtraFieldSubID;
      const aExpectedData; aExpectedSize : Word);
    procedure CheckSubfieldNotFound(aID : TAbGzExtraFieldSubID);

  protected
    procedure SetUp; override;
    procedure TearDown; override;

    function StrToTAbGzExtraFieldSubID(Const S: string): TAbGzExtraFieldSubID;
  published
    procedure TextEmptyField;
    procedure TestAddZeroLengthAttr;
    procedure TestAddAttr;
    procedure TestAddTwoAttr;
    procedure TestDeleteFirstSubfield;
    procedure TestDeleteLastSubfield;
    procedure TestDeleteMiddleSubfield;
    procedure TestDeleteAllSubfields;
    procedure TestReplaceSameSize;
    procedure TestReplaceDiffSize;
  end;

  TAbGzipArchiveTests = class(TAbArchiveTests)
  protected
    class function ArchiveClass: TAbArchiveClass; override;
    class function ArchiveExt: string; override;
    class function ArchiveType: TAbArchiveType; override;
    class function VerifyArchive(aStream: TStream): TAbArchiveType; override;
  end;

  TAbGzippedTarArchiveTests = class(TAbArchiveMultiFileTests)
  protected
    class function CreateArchive(const aFileName : string; aMode : Word): TAbArchive;
      override;
    class function CreateArchive(aStream : TStream; const aArchiveName : string): TAbArchive;
      override;
    class function ArchiveClass: TAbArchiveClass; override;
    class function ArchiveExt: string; override;
    class function ArchiveType: TAbArchiveType; override;
    class function VerifyArchive(aStream: TStream): TAbArchiveType; override;
  end;

implementation

uses
  SysUtils, TestFrameWork;

{----------------------------------------------------------------------------}
{ TAbGzipExtraFieldTests }
{----------------------------------------------------------------------------}
procedure TAbGzipExtraFieldTests.SetUp;
begin
  inherited;
  FillChar(FGzHeader, SizeOf(FGzHeader), 0);
  FExtraField := TAbGzipExtraField.Create(@FGzHeader);
end;
function TAbGzipExtraFieldTests.StrToTAbGzExtraFieldSubID(const S: string): TAbGzExtraFieldSubID;
var
  iCount: Integer;
begin
  for iCount := Low(Result) to High(Result) do
    Result[iCount] := Ord(S.Chars[iCount]);
end;

{ -------------------------------------------------------------------------- }
procedure TAbGzipExtraFieldTests.TearDown;
begin
  FreeAndNil(FExtraField);
  inherited;
end;
{ -------------------------------------------------------------------------- }
procedure TAbGzipExtraFieldTests.CheckFieldEquals(const aExpectedData;
  aExpectedSize: Word);
const
  AB_GZ_FLAG_FEXTRA = $04;
begin
  CheckMemEquals(@aExpectedData, aExpectedSize, Pointer(FExtraField.Buffer),
    Length(FExtraField.Buffer), 'ExtraField');
  Check((aExpectedSize > 0) = ((FGzHeader.Flags and AB_GZ_FLAG_FEXTRA) = AB_GZ_FLAG_FEXTRA),
    'Header flag not set correctly');
end;
{ -------------------------------------------------------------------------- }
procedure TAbGzipExtraFieldTests.CheckMemEquals(aExpectedData : Pointer;
  aExpectedSize : Integer; aActualData : Pointer; aActualSize : Integer;
  const aMsg : string);
var
  ExpectedHex, ActualHex: string;
begin
  CheckEquals(aExpectedSize, aActualSize, aMsg + ' size incorrect');
  if (aExpectedSize > 0) and
     not CompareMem(aExpectedData, aActualData, aExpectedSize) then begin
    SetLength(ExpectedHex, aExpectedSize * 2);
    BinToHex(aExpectedData, PChar(ExpectedHex), aExpectedSize);
    SetLength(ActualHex, aActualSize * 2);
    BinToHex(aActualData, PChar(ActualHex), aActualSize);
    FailNotSame('$' + ExpectedHex, '$' + ActualHex, aMsg + ' data incorrect');
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbGzipExtraFieldTests.CheckSubfieldCount(aExpected: Integer);
begin
  CheckEquals(aExpected, FExtraField.Count, 'Subfield count incorrect');
end;
{ -------------------------------------------------------------------------- }
procedure TAbGzipExtraFieldTests.CheckSubfieldEquals(aID : TAbGzExtraFieldSubID;
  const aExpectedData; aExpectedSize: Word);
var
  Data: Pointer;
  Size: Word;
begin
  Check(FExtraField.Get(aID, Data, Size), 'Subfield ' + Char(aID) + ' not found');
  CheckMemEquals(@aExpectedData, aExpectedSize, Data, Size, 'Subfield');
end;
{ -------------------------------------------------------------------------- }
procedure TAbGzipExtraFieldTests.CheckSubfieldNotFound(aID : TAbGzExtraFieldSubID);
var
  Data : Pointer;
  Size : Word;
begin
  CheckFalse(FExtraField.Get(aID, Data, Size),
    Char(aID) + ' found when it shouldn''t exist');
end;
{ -------------------------------------------------------------------------- }
procedure TAbGzipExtraFieldTests.TextEmptyField;
begin
  CheckSubfieldCount(0);
  CheckSubfieldNotFound(StrToTAbGzExtraFieldSubID('Ab'));
  CheckFieldEquals(nil^, 0);
end;
{ -------------------------------------------------------------------------- }
procedure TAbGzipExtraFieldTests.TestAddZeroLengthAttr;
const
  ExpectedBuf: array[0..3] of Byte = (65, 98, 0, 0);
begin
  FExtraField.Put(StrToTAbGzExtraFieldSubID('Ab'), nil^, 0);
  CheckSubfieldCount(1);
  CheckSubfieldEquals(StrToTAbGzExtraFieldSubID('Ab'), nil^, 0);
  CheckSubfieldNotFound(StrToTAbGzExtraFieldSubID('AB'));
  CheckFieldEquals(ExpectedBuf, SizeOf(ExpectedBuf));
end;
{ -------------------------------------------------------------------------- }
procedure TAbGzipExtraFieldTests.TestAddAttr;
const
  ExpectedBuf: array[0..7] of Byte = (65, 98, 4, 0, $44, $33, $22, $11);
var
  Data: LongWord;
begin
  Data := $11223344;
  FExtraField.Put(StrToTAbGzExtraFieldSubID('Ab'), Data, SizeOf(Data));
  CheckSubfieldCount(1);
  CheckSubfieldEquals(StrToTAbGzExtraFieldSubID('Ab'), Data, SizeOf(Data));
  CheckFieldEquals(ExpectedBuf, SizeOf(ExpectedBuf));
end;
{ -------------------------------------------------------------------------- }
procedure TAbGzipExtraFieldTests.TestAddTwoAttr;
const
  ExpectedBuf: array[0..11] of Byte = (65, 49, 0, 0, 65, 50, 4, 0, $44, $33, $22, $11);
var
  Data: LongWord;
begin
  FExtraField.Put(StrToTAbGzExtraFieldSubID('A1'), nil^, 0);
  Data := $11223344;
  FExtraField.Put(StrToTAbGzExtraFieldSubID('A2'), Data, SizeOf(Data));
  CheckSubfieldCount(2);
  CheckSubfieldEquals(StrToTAbGzExtraFieldSubID('A1'), nil^, 0);
  CheckSubfieldEquals(StrToTAbGzExtraFieldSubID('A2'), Data, SizeOf(Data));
  CheckFieldEquals(ExpectedBuf, SizeOf(ExpectedBuf));
end;
{ -------------------------------------------------------------------------- }
procedure TAbGzipExtraFieldTests.TestDeleteFirstSubfield;
const
  ExpectedBuf: array[0..7] of Byte = (65, 50, 4, 0, $88, $77, $66, $55);
var
  Data: LongWord;
begin
  Data := $11223344;
  FExtraField.Put(StrToTAbGzExtraFieldSubID('A1'), Data, SizeOf(Data));
  Data := $55667788;
  FExtraField.Put(StrToTAbGzExtraFieldSubID('A2'), Data, SizeOf(Data));
  FExtraField.Delete(StrToTAbGzExtraFieldSubID('A1'));
  CheckSubfieldCount(1);
  CheckSubfieldNotFound(StrToTAbGzExtraFieldSubID('A1'));
  CheckSubfieldEquals(StrToTAbGzExtraFieldSubID('A2'), Data, SizeOf(Data));
  CheckFieldEquals(ExpectedBuf, SizeOf(ExpectedBuf));
end;
{ -------------------------------------------------------------------------- }
procedure TAbGzipExtraFieldTests.TestDeleteLastSubfield;
const
  ExpectedBuf: array[0..5] of Byte = (65, 49, 2, 0, $22, $11);
var
  Data1: Word;
  Data2: LongWord;
begin
  Data1 := $1122;
  FExtraField.Put(StrToTAbGzExtraFieldSubID('A1'), Data1, SizeOf(Data1));
  Data2 := $33445566;
  FExtraField.Put(StrToTAbGzExtraFieldSubID('A2'), Data2, SizeOf(Data2));
  FExtraField.Delete(StrToTAbGzExtraFieldSubID('A2'));
  CheckSubfieldCount(1);
  CheckSubfieldEquals(StrToTAbGzExtraFieldSubID('A1'), Data1, SizeOf(Data1));
  CheckSubfieldNotFound(StrToTAbGzExtraFieldSubID('A2'));
  CheckFieldEquals(ExpectedBuf, Length(ExpectedBuf));
end;
{ -------------------------------------------------------------------------- }
procedure TAbGzipExtraFieldTests.TestDeleteMiddleSubfield;
const
  ExpectedBuf: array[0..17] of Byte =
    (65, 49, 2, 0, $22, $11, 65, 51, 8, 0, $EF, $CD, $AB, $89, $67, $45, $23, $01);
var
  Data1: Word;
  Data2: LongWord;
  Data3: Int64;
begin
  Data1 := $1122;
  FExtraField.Put(StrToTAbGzExtraFieldSubID('A1'), Data1, SizeOf(Data1));
  Data2 := $33445566;
  FExtraField.Put(StrToTAbGzExtraFieldSubID('A2'), Data2, SizeOf(Data2));
  Data3 := $0123456789ABCDEF;
  FExtraField.Put(StrToTAbGzExtraFieldSubID('A3'), Data3, SizeOf(Data3));
  FExtraField.Delete(StrToTAbGzExtraFieldSubID('A2'));
  CheckSubfieldEquals(StrToTAbGzExtraFieldSubID('A1'), Data1, SizeOf(Data1));
  CheckSubfieldNotFound(StrToTAbGzExtraFieldSubID('A2'));
  CheckSubfieldEquals(StrToTAbGzExtraFieldSubID('A3'), Data3, SizeOf(Data3));
  CheckFieldEquals(ExpectedBuf, SizeOf(ExpectedBuf));
end;
{ -------------------------------------------------------------------------- }
procedure TAbGzipExtraFieldTests.TestDeleteAllSubfields;
var
  Data: Word;
begin
  Data := $1122;
  FExtraField.Put(StrToTAbGzExtraFieldSubID('A1'), Data, SizeOf(Data));
  FExtraField.Put(StrToTAbGzExtraFieldSubID('A2'), Data, SizeOf(Data));
  FExtraField.Delete(StrToTAbGzExtraFieldSubID('A1'));
  FExtraField.Delete(StrToTAbGzExtraFieldSubID('A2'));
  CheckSubfieldCount(0);
  CheckFieldEquals(nil^, 0);
end;
{----------------------------------------------------------------------------}
procedure TAbGzipExtraFieldTests.TestReplaceSameSize;
var
  Data: Word;
begin
  Data := $1122;
  FExtraField.Put(StrToTAbGzExtraFieldSubID('A1'), Data, SizeOf(Data));
  Data := $3344;
  FExtraField.Put(StrToTAbGzExtraFieldSubID('A2'), Data, SizeOf(Data));
  Data := $5566;
  FExtraField.Put(StrToTAbGzExtraFieldSubID('A3'), Data, SizeOf(Data));
  Data := $7788;
  FExtraField.Put(StrToTAbGzExtraFieldSubID('A2'), Data, SizeOf(Data));
  CheckSubfieldCount(3);
  Data := $1122;
  CheckSubfieldEquals(StrToTAbGzExtraFieldSubID('A1'), Data, SizeOf(Data));
  Data := $7788;
  CheckSubfieldEquals(StrToTAbGzExtraFieldSubID('A2'), Data, SizeOf(Data));
  Data := $5566;
  CheckSubfieldEquals(StrToTAbGzExtraFieldSubID('A3'), Data, SizeOf(Data));
end;
{----------------------------------------------------------------------------}
procedure TAbGzipExtraFieldTests.TestReplaceDiffSize;
var
  Data1, SmallData2, Data3: Word;
  BigData2: LongWord;
begin
  Data1 := $1122;
  FExtraField.Put(StrToTAbGzExtraFieldSubID('A1'), Data1, SizeOf(Data1));
  SmallData2 := $3344;
  FExtraField.Put(StrToTAbGzExtraFieldSubID('A2'), SmallData2, SizeOf(SmallData2));
  Data3 := $5566;
  FExtraField.Put(StrToTAbGzExtraFieldSubID('A3'), Data3, SizeOf(Data3));
  BigData2 := $01234567;
  FExtraField.Put(StrToTAbGzExtraFieldSubID('A2'), BigData2, SizeOf(BigData2));
  CheckSubfieldCount(3);
  CheckSubfieldEquals(StrToTAbGzExtraFieldSubID('A1'), Data1, SizeOf(Data1));
  CheckSubfieldEquals(StrToTAbGzExtraFieldSubID('A2'), BigData2, SizeOf(BigData2));
  CheckSubfieldEquals(StrToTAbGzExtraFieldSubID('A3'), Data3, SizeOf(Data3));
end;

{----------------------------------------------------------------------------}
{ TAbGzipArchiveTests }
{----------------------------------------------------------------------------}
class function TAbGzipArchiveTests.ArchiveClass: TAbArchiveClass;
begin
  Result := TAbGzipArchive;
end;
{ -------------------------------------------------------------------------- }
class function TAbGzipArchiveTests.ArchiveExt: string;
begin
  Result := '.gz';
end;
{ -------------------------------------------------------------------------- }
class function TAbGzipArchiveTests.ArchiveType: TAbArchiveType;
begin
  Result := atGzip;
end;
{ -------------------------------------------------------------------------- }
class function TAbGzipArchiveTests.VerifyArchive(aStream: TStream): TAbArchiveType;
begin
  Result := VerifyGzip(aStream);
end;

{----------------------------------------------------------------------------}
{ TAbGzippedTarArchiveTests }
{----------------------------------------------------------------------------}
class function TAbGzippedTarArchiveTests.CreateArchive(const aFileName : string;
  aMode : Word): TAbArchive;
begin
  Result := inherited CreateArchive(aFileName, aMode);
  TAbGzipArchive(Result).TarAutoHandle := True;
  TAbGzipArchive(Result).IsGzippedTar := True;
end;
{----------------------------------------------------------------------------}
class function TAbGzippedTarArchiveTests.CreateArchive(aStream : TStream;
  const aArchiveName : string): TAbArchive;
begin
  Result := inherited CreateArchive(aStream, aArchiveName);
  TAbGzipArchive(Result).TarAutoHandle := True;
  TAbGzipArchive(Result).IsGzippedTar := True;
end;
{----------------------------------------------------------------------------}
class function TAbGzippedTarArchiveTests.ArchiveClass: TAbArchiveClass;
begin
  Result := TAbGzipArchive;
end;
{----------------------------------------------------------------------------}
class function TAbGzippedTarArchiveTests.ArchiveExt: string;
begin
  Result := '.tgz';
end;
{ -------------------------------------------------------------------------- }
class function TAbGzippedTarArchiveTests.ArchiveType: TAbArchiveType;
begin
  Result := atGzippedTar;
end;
{ -------------------------------------------------------------------------- }
class function TAbGzippedTarArchiveTests.VerifyArchive(aStream: TStream): TAbArchiveType;
begin
  Result := VerifyGzip(aStream);
end;
{----------------------------------------------------------------------------}

initialization

  TestFramework.RegisterTest('Abbrevia.Low-Level Compression Test Suite',
    TAbGzipExtraFieldTests.Suite);
  TestFramework.RegisterTest('Abbrevia.Low-Level Compression Test Suite',
    TAbGzipArchiveTests.Suite);
  TestFramework.RegisterTest('Abbrevia.Low-Level Compression Test Suite',
    TAbGzippedTarArchiveTests.Suite);

end.