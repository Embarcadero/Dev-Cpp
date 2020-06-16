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
 *
 * ***** END LICENSE BLOCK ***** *)

unit AbUnzPrcTests;
{$I AbDefine.inc}
interface

uses
  Classes, AbTestFramework;

type

  TAbUnzPrcTests = class(TabTestCase)
  private
    FUnCompressedStream : TMemoryStream;
    FCompressedStream   : TMemoryStream;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
    function TestStreamDir: string;
  published
    procedure InflateStreamSimpleTest1;
    procedure InflateStreamSimpleTest2;
    procedure InflateStreamFileTest;
  end;

implementation

uses
  SysUtils, TestFrameWork, AbUnzPrc, AbTestConsts;

{ TAbUnzPrcTests }

procedure TAbUnzPrcTests.InflateStreamFileTest;
var
  FS : TFileStream;
begin
  FS := TFileStream.Create(TestStreamDir + 'Testdoc1.cmp', fmOpenRead);
  try
    InflateStream(FS, FUnCompressedStream);
  finally
    FS.Free;
  end;
  CheckFileMatchesStream(TestStreamDir + 'TestDoc1.txt', FUnCompressedStream);
end;

procedure TAbUnzPrcTests.InflateStreamSimpleTest1;
var
 I : Integer;
 b : byte;
begin
  FCompressedStream.Write(CompressedBuf1,SizeOf(CompressedBuf1));
  FCompressedStream.Seek(0,soFromBeginning);
  InflateStream(FCompressedStream,FUnCompressedStream);

  check(FUnCompressedStream.Size = sizeOf(UnCompressedBuf1),'FUnCompressedStream.Size ('+IntToStr(FUnCompressedStream.Size)+ ') should be sizeOf(UnCompressedBuf1)');
  FUnCompressedStream.Seek(0,soFromBeginning); // Move to start;
  for I := 0 to sizeOf(UnCompressedBuf1)-1 do
   begin
     FUnCompressedStream.Read(b,1);
     check(B = UnCompressedBuf1[I], 'Byte [' + IntToStr(I) + '] of Buffer is incorrect, Expecting:' + IntTostr(UnCompressedBuf1[I]) + ' found :'  + IntToStr(B));
   end;
end;

procedure TAbUnzPrcTests.InflateStreamSimpleTest2;
var
 I : Integer;
 b : byte;
begin
  FCompressedStream.Write(CompressedBuf2,SizeOf(CompressedBuf2));
  FCompressedStream.Seek(0,soFromBeginning);
  InflateStream(FCompressedStream,FUnCompressedStream);

  check(FUnCompressedStream.Size = sizeOf(UnCompressedBuf2),'FUnCompressedStream.Size ('+IntToStr(FUnCompressedStream.Size)+ ') should be sizeOf(UnCompressedBuf2)');
  FUnCompressedStream.Seek(0,soFromBeginning); // Move to start;
  for I := 0 to sizeOf(UnCompressedBuf2)-1 do
   begin
     FUnCompressedStream.Read(b,1);
     check(B = UnCompressedBuf2[I], 'Byte [' + IntToStr(I) + '] of Buffer is incorrect, Expecting:' + IntTostr(UnCompressedBuf2[I]) + ' found :'  + IntToStr(B));
   end;
end;

procedure TAbUnzPrcTests.SetUp;
begin
  inherited;
  FUnCompressedStream := TMemoryStream.Create;
  FCompressedStream   := TMemoryStream.Create;
end;


procedure TAbUnzPrcTests.TearDown;
begin
  FUnCompressedStream.Free;
  FCompressedStream.Free;
  inherited;
end;

function TAbUnzPrcTests.TestStreamDir: string;
begin
  Result := TestFileDir + 'StreamTests' + PathDelim;
end;

initialization

  TestFramework.RegisterTest('Abbrevia.AbUnzPrc Suite',
    TAbUnzPrcTests.Suite);

end.

