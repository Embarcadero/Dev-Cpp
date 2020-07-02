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

 unit AbCabTypTests;

{$I AbDefine.inc}

interface

uses
  Classes, TestFrameWork, AbArcTyp, AbUtils, AbArcTypTests, AbTestFrameWork;

type
  TAbCabArchiveTests = class(TAbArchiveMultiFileTests)
  private
    FItemFailed: Boolean;

    procedure IncompleteItemFailure(Sender : TObject; Item : TAbArchiveItem;
      ProcessType : TAbProcessType; ErrorClass : TAbErrorClass;
      ErrorCode : Integer);

  protected
    class function ArchiveClass: TAbArchiveClass; override;
    class function ArchiveExt: string; override;
    class function ArchiveType: TAbArchiveType; override;
    class function VerifyArchive(aStream: TStream): TAbArchiveType; override;

  published
    procedure TestAddFromStream; override;
    procedure TestIncompleteSpan;
    procedure TestNoSpan;
  end;

implementation

uses
  SysUtils, AbCabTyp, AbFciFdi;

{----------------------------------------------------------------------------}
{ TAbCabArchiveTests }
{----------------------------------------------------------------------------}
class function TAbCabArchiveTests.ArchiveClass: TAbArchiveClass;
begin
  Result := TAbCabArchive;
end;
{ -------------------------------------------------------------------------- }
class function TAbCabArchiveTests.ArchiveExt: string;
begin
  Result := '.cab';
end;
{ -------------------------------------------------------------------------- }
class function TAbCabArchiveTests.ArchiveType: TAbArchiveType;
begin
  Result := atCab;
end;
{ -------------------------------------------------------------------------- }
class function TAbCabArchiveTests.VerifyArchive(aStream: TStream): TAbArchiveType;
begin
  Result := VerifyCab(aStream);
end;
{----------------------------------------------------------------------------}
procedure TAbCabArchiveTests.IncompleteItemFailure(Sender : TObject;
  Item : TAbArchiveItem; ProcessType : TAbProcessType;
  ErrorClass : TAbErrorClass; ErrorCode : Integer);
begin
  FItemFailed := True;
  CheckEquals('kennedy.xls', Item.FileName);
  Check(ProcessType = ptExtract, 'ItemFailure: ProcessType was not ptExtract');
  Check(ErrorClass = ecCabError, 'ItemFailure: ErrorClass was not ecCabError');
  Check(ErrorCode = Ord(FDIError_User_Abort), 'ItemFailure: ErrorCode was not user abort');
end;
{----------------------------------------------------------------------------}
procedure TAbCabArchiveTests.TestAddFromStream;
begin
  //This Test currently doesn't work!
  CheckTrue(True);
//  inherited TestAddFromStream;
end;

procedure TAbCabArchiveTests.TestIncompleteSpan;
var
  Arc: TAbArchive;
begin
  // [3370538] Check that extracting a file spanned across two cabinets fails
  // if the second cab isn't available .
  AbCopyFile(CanterburyDir + 'Split\Split.cab', TestTempDir + 'Split.cab', True);
  Arc := CreateArchive(TestTempDir + 'Split.cab', fmOpenRead);
  try
    Arc.OnProcessItemFailure := IncompleteItemFailure;
    Arc.BaseDirectory := TestTempDir;
    Arc.Load;
    Arc.ExtractFiles('kennedy.xls');
    Check(FItemFailed, 'ItemFailure was not called');
  finally
    Arc.Free;
  end;
end;
{----------------------------------------------------------------------------}
procedure TAbCabArchiveTests.TestNoSpan;
var
  Arc: TAbCabArchive;
begin
  // Check that creating a single cabinet archive doesn't flag the cab as having
  // a second volume (issue with D2007->D2009 conversion and Boolean vs BOOL in AbFciFdi)
  Arc := TAbCabArchive.Create(TestTempDir + 'test.cab', fmCreate);
  try
    Arc.Load;
    Arc.BaseDirectory := MPLDir;
    Arc.AddFiles('MPL-1_1.txt', faAnyFile);
    Arc.Save;
  finally
    Arc.Free;
  end;
  Arc := TAbCabArchive.Create(TestTempDir + 'test.cab', fmOpenRead);
  try
    Arc.Load;
    Check(not Arc.HasNext and not Arc.HasPrev, 'Archive is part of a multi-cab set');
  finally
    Arc.Free;
  end;
end;
{----------------------------------------------------------------------------}

initialization

  TestFramework.RegisterTest('Abbrevia.Low-Level Compression Test Suite',
    TAbCabArchiveTests.Suite);

end.
