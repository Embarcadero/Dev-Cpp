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

unit AbBrowseTests;

{$I AbDefine.inc}

interface

uses
  TestFramework, AbTestFrameWork, AbBrowse, AbUtils;

type
  TAbBaseBrowserTests = class(TAbCompTestCase)
  public
    class function Suite: ITestSuite; override;
  end;

  TAbDetermineArcTypeTest = class(TAbTestCase)
  private
    FFilename: string;
    FExpectedType: TAbArchiveType;
    procedure CheckEquals(expected, actual: TAbArchiveType; const msg: string);
  public
    constructor Create(const aFilename: string; aExpectedType: TAbArchiveType); reintroduce;
  published
    procedure TestDetermineArcType;
  end;


implementation

uses
  Classes, SysUtils, TypInfo;

function ArcTypeToStr(aValue: TAbArchiveType): string;
begin
  Result := GetEnumName(TypeInfo(TAbArchiveType), Integer(aValue));
end;

{ TAbBaseBrowserTests }

class function TAbBaseBrowserTests.Suite: ITestSuite;
const
  Files: array[TAbArchiveType] of string = (
    'MPL-1_1.txt', 'MPL.zip', '', 'MPL.exe', 'MPL.tar', 'MPL.gz', 'MPL.tgz',
    'MPL.cab', 'MPL.bz2', 'MPL.tbz');
var
  arcType: TAbArchiveType;
begin
  Result := TTestSuite.Create('AbDetermineArcType tests');
  for arcType := Low(arcType) to High(arcType) do
    if Files[arcType] <> '' then
      Suite.AddTest(TAbDetermineArcTypeTest.Create(
        MPLDir + Files[arcType], arcType));
  Suite.AddTest(TAbDetermineArcTypeTest.Create(
    MPLDir + 'MPL.lnx', atSelfExtZip));
  Suite.AddTest(TAbDetermineArcTypeTest.Create(
    CanterburyDir + 'Split' + PathDelim + 'Split.z01', atSpannedZip));
  Suite.AddTest(TAbDetermineArcTypeTest.Create(
    CanterburyDir + 'Split' + PathDelim + 'Split.zip', atSpannedZip));
  Suite.AddTest(TAbDetermineArcTypeTest.Create(
    TestFileDir + 'Test0001.cab', atCab));
end;

{ TAbDetermineArcTypeTest }

constructor TAbDetermineArcTypeTest.Create(const aFilename: string;
  aExpectedType: TAbArchiveType);
begin
  inherited Create('TestDetermineArcType');
  FTestName := Format('%s (%s)',
    [ExtractFileName(aFilename), ArcTypeToStr(aExpectedType)]);
  FFilename := aFilename;
  FExpectedType := aExpectedType;
  {$IFNDEF MSWINDOWS}
  if FExpectedType = atCab then
    FExpectedType := atUnknown;
  {$ENDIF}
end;

procedure TAbDetermineArcTypeTest.CheckEquals(expected, actual: TAbArchiveType;
  const msg: string);
begin
  FCheckCalled := True;
  if expected <> actual then
    FailNotSame(ArcTypeToStr(expected), ArcTypeToStr(actual), msg);
end;

procedure TAbDetermineArcTypeTest.TestDetermineArcType;
var
  Stream: TFileStream;
  TmpFile: string;
begin
  CheckEquals(FExpectedType, AbDetermineArcType(FFilename, FExpectedType),
    'Checking filename with asserted type');
  CheckEquals(FExpectedType, AbDetermineArcType(FFilename, atUnknown),
    'Checking filename without asserted type');
  Stream := TFileStream.Create(FFilename, fmOpenRead);
  try
    CheckEquals(FExpectedType, AbDetermineArcType(Stream), 'Checking stream');
  finally
    Stream.Free;
  end;
  TmpFile := TestTempDir + ChangeFileExt(ExtractFileName(FFilename), '.tmp');
  AbCopyFile(FFilename, TmpFile, True);
  CheckEquals(FExpectedType, AbDetermineArcType(TmpFile, atUnknown),
    'Checking filename with unknown extension');
end;

initialization

  TestFramework.RegisterTest('Abbrevia.Component Level Test Suite',
    TAbBaseBrowserTests.Suite);

end.

