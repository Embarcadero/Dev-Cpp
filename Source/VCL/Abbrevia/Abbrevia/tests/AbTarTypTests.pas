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

 unit AbTarTypTests;

{$I AbDefine.inc}

interface

uses
  Classes, TestFramework, AbArcTypTests, AbTestFrameWork, AbUtils;

type
  TAbTarArchiveTests = class(TAbArchiveMultiFileTests)
  protected
    class function ArchiveClass: TAbArchiveClass; override;
    class function ArchiveExt: string; override;
    class function ArchiveType: TAbArchiveType; override;
    class function VerifyArchive(aStream: TStream): TAbArchiveType; override;
  public
    class function Suite: ITestSuite; override;
  end;

implementation

uses
  SysUtils, AbTarTyp;

{----------------------------------------------------------------------------}
{ TAbTarArchiveTests }
{----------------------------------------------------------------------------}
class function TAbTarArchiveTests.ArchiveClass: TAbArchiveClass;
begin
  Result := TAbTarArchive;
end;
{ -------------------------------------------------------------------------- }
class function TAbTarArchiveTests.ArchiveExt: string;
begin
  Result := '.tar';
end;
{ -------------------------------------------------------------------------- }
class function TAbTarArchiveTests.ArchiveType: TAbArchiveType;
begin
  Result := atTar;
end;
{ -------------------------------------------------------------------------- }
class function TAbTarArchiveTests.VerifyArchive(aStream: TStream): TAbArchiveType;
begin
  Result := VerifyTar(aStream);
end;
{ -------------------------------------------------------------------------- }
class function TAbTarArchiveTests.Suite: ITestSuite;
begin
  Result := inherited Suite;
  //This test doesn't work.
//  {$IF DEFINED(UNICODE) OR NOT DEFINED(MSWINDOWS)}
//  Result.AddTest(TAbArchiveDecompressTest.Create(Self, 'Decompress Unicode',
//    TestFileDir + 'Unicode' + PathDelim + 'UTF-8.tar'));
//  {$IFEND}
end;
{----------------------------------------------------------------------------}

initialization

  TestFramework.RegisterTest('Abbrevia.Low-Level Compression Test Suite',
    TAbTarArchiveTests.Suite);

end.
