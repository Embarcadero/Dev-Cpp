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

unit AbBzip2TypTests;

{$I AbDefine.inc}

interface

uses
  Classes, AbArcTypTests, AbArcTyp, AbUtils;

type
  TAbBzip2ArchiveTests = class(TAbArchiveTests)
  protected
    class function ArchiveClass: TAbArchiveClass; override;
    class function ArchiveExt: string; override;
    class function ArchiveType: TAbArchiveType; override;
    class function VerifyArchive(aStream: TStream): TAbArchiveType; override;
  end;

  TAbBzippedTarArchiveTests = class(TAbArchiveMultiFileTests)
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
  SysUtils, TestFrameWork, AbBzip2Typ;

{----------------------------------------------------------------------------}
{ TAbBzip2ArchiveTests }
{----------------------------------------------------------------------------}
class function TAbBzip2ArchiveTests.ArchiveClass: TAbArchiveClass;
begin
  Result := TAbBzip2Archive;
end;
{ -------------------------------------------------------------------------- }
class function TAbBzip2ArchiveTests.ArchiveExt: string;
begin
  Result := '.bz2';
end;
{ -------------------------------------------------------------------------- }
class function TAbBzip2ArchiveTests.ArchiveType: TAbArchiveType;
begin
  Result := atBzip2;
end;
{ -------------------------------------------------------------------------- }
class function TAbBzip2ArchiveTests.VerifyArchive(aStream: TStream): TAbArchiveType;
begin
  Result := VerifyBzip2(aStream);
end;

{----------------------------------------------------------------------------}
{ TAbBzippedTarArchiveTests }
{----------------------------------------------------------------------------}
class function TAbBzippedTarArchiveTests.CreateArchive(const aFileName : string;
  aMode : Word): TAbArchive;
begin
  Result := inherited CreateArchive(aFileName, aMode);
  TAbBzip2Archive(Result).TarAutoHandle := True;
  TAbBzip2Archive(Result).IsBzippedTar := True;
end;
{----------------------------------------------------------------------------}
class function TAbBzippedTarArchiveTests.CreateArchive(aStream : TStream;
  const aArchiveName : string): TAbArchive;
begin
  Result := inherited CreateArchive(aStream, aArchiveName);
  TAbBzip2Archive(Result).TarAutoHandle := True;
  TAbBzip2Archive(Result).IsBzippedTar := True;
end;
{----------------------------------------------------------------------------}
class function TAbBzippedTarArchiveTests.ArchiveClass: TAbArchiveClass;
begin
  Result := TAbBzip2Archive;
end;
{----------------------------------------------------------------------------}
class function TAbBzippedTarArchiveTests.ArchiveExt: string;
begin
  Result := '.tbz';
end;
{ -------------------------------------------------------------------------- }
class function TAbBzippedTarArchiveTests.ArchiveType: TAbArchiveType;
begin
  Result := atBzippedTar;
end;
{ -------------------------------------------------------------------------- }
class function TAbBzippedTarArchiveTests.VerifyArchive(aStream: TStream): TAbArchiveType;
begin
  Result := VerifyBzip2(aStream);
end;
{----------------------------------------------------------------------------}

initialization

  TestFramework.RegisterTest('Abbrevia.Low-Level Compression Test Suite',
    TAbBzip2ArchiveTests.Suite);
  TestFramework.RegisterTest('Abbrevia.Low-Level Compression Test Suite',
    TAbBzippedTarArchiveTests.Suite);

end.
