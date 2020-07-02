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

unit AbZipPrcTests;

{$I AbDefine.inc}

interface

uses
  Classes,
  AbTestFrameWork;

type
  TAbZipPrcTests = class(TAbTestCase)
  private
    FUnCompressedStream : TMemoryStream;
    FCompressedStream   : TMemoryStream;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  public
    function TestStreamDir: string;

  published
    procedure DeflateStreamSimpleTest1;
    procedure DeflateStreamSimpleTest2;
    procedure DeflateStreamFileTest;
    procedure DeflateStreamInflateStreamTest;
  end;

implementation

uses
  SysUtils,
  TestFrameWork,
  AbTestConsts, AbUnzPrc, AbZipPrc;

{ TAbZipPrcTests }

procedure TAbZipPrcTests.SetUp;
begin
  inherited;
  FUnCompressedStream := TMemoryStream.create;
  FCompressedStream := TMemoryStream.create;
end;

procedure TAbZipPrcTests.TearDown;
begin
  FUnCompressedStream.Free;
  FCompressedStream.Free;
  inherited;
end;

procedure TAbZipPrcTests.DeflateStreamSimpleTest1;
var
  I : Integer;
  b : byte;
begin
  FUnCompressedStream.Write(UncompressedBuf1, SizeOf(UncompressedBuf1));
  FUnCompressedStream.Seek(0, soFromBeginning);
  DeflateStream(FUnCompressedStream, FCompressedStream);
  Check(FCompressedStream.Size = SizeOf(CompressedBuf1),
    'FCompressedStream.Size (' + IntToStr(FCompressedStream.Size) + ') should be sizeOf(CompressedBuf1)');
  FCompressedStream.Seek(0, soFromBeginning);
  for I := 0 to SizeOf(CompressedBuf1) - 1 do begin
    FCompressedStream.Read(b, 1);
    Check(B = CompressedBuf1[I], 'Byte [' + IntToStr(I) + '] of Buffer is incorrect, Expecting:' + IntTostr(CompressedBuf1[I]) + ' found :'  + IntToStr(B));
  end;
end;

procedure TAbZipPrcTests.DeflateStreamSimpleTest2;
var
  I : Integer;
  b : Byte;
begin
  FUnCompressedStream.Write(UncompressedBuf2,SizeOf(UncompressedBuf2));
  FUnCompressedStream.Seek(0, soFromBeginning);
  DeflateStream(FUnCompressedStream, FCompressedStream);
  Check(FCompressedStream.Size = SizeOf(CompressedBuf2),
    'FCompressedStream.Size ('+IntToStr(FCompressedStream.Size)+ ') should be sizeOf(CompressedBuf2)');
  FCompressedStream.Seek(0, soFromBeginning); // Move to start;
  for I := 0 to SizeOf(CompressedBuf2) - 1 do begin
    FCompressedStream.Read(b,1);
    Check(B = CompressedBuf2[I], 'Byte [' + IntToStr(I) + '] of Buffer is incorrect, Expecting:' + IntTostr(CompressedBuf2[I]) + ' found :'  + IntToStr(B));
  end;
end;

procedure TAbZipPrcTests.DeflateStreamFileTest;
var
  FS : TFileStream;
begin
  FS := TFileStream.Create(TestStreamDir + 'TestDoc1.txt', fmOpenRead);
  try
    DeflateStream(FS, FCompressedStream);
  finally
    FS.Free;
  end;
  CheckFileMatchesStream(TestStreamDir + 'Testdoc1.cmp', FCompressedStream);
end;

procedure TAbZipPrcTests.DeflateStreamInflateStreamTest;
var
  I : Integer;
  Buf : Integer;
  FUnCompressedStream2 : TMemoryStream;
begin
  for I := 0 to 50000 do begin
    if Odd(I) then
      Buf := 4545424
    else Buf := I;
    FUnCompressedStream.Write(Buf,SizeOf(Buf));
  end;
  FUnCompressedStream.Seek(0,soFromBeginning);
  DeflateStream(FUnCompressedStream,FCompressedStream);
  FUnCompressedStream2 := TMemoryStream.Create;
  try
    FCompressedStream.Seek(0,soFromBeginning);
    InflateStream(FCompressedStream,FUnCompressedStream2);
    FUnCompressedStream.Seek(0,soFromBeginning);
    FUnCompressedStream2.Seek(0,soFromBeginning);
    CheckStreamMatch(FUnCompressedStream,FUnCompressedStream2,'DeflateInflateTest - Streams did not match');
  finally
    FUnCompressedStream2.Free;
  end;
end;

function TAbZipPrcTests.TestStreamDir: string;
begin
  Result := TestFileDir + 'StreamTests' + PathDelim;
end;

initialization

  TestFramework.RegisterTest('Abbrevia.AbZipPrc Suite',
    TAbZipPrcTests.Suite);

end.

