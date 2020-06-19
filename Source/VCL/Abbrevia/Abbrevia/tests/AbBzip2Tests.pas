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
 * The Initial Developer of the Original Code is Craig Peterson
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Craig Peterson <capeterson@users.sourceforge.net>
 *
 * ***** END LICENSE BLOCK ***** *)

unit AbBzip2Tests;

{$I AbDefine.inc}

interface

uses
  Classes, AbTestFramework;

type

  TAbBzip2Tests = class(TAbTestCase)
  private
    procedure TestDecompress(aStream: TStream; aExpectedFile: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDecompressionStream;
    procedure TestCompressionStream;
  end;

implementation

uses
  SysUtils, TestFrameWork, AbBzip2;

{ TAbVMStrmTests }

procedure TAbBzip2Tests.SetUp;
begin
  inherited;
end;

procedure TAbBzip2Tests.TearDown;
begin
  inherited;
end;

procedure TAbBzip2Tests.TestDecompress(aStream: TStream; aExpectedFile: string);
var
  Buf: array[0..1023] of Byte;
  BytesRead: Integer;
  Bz2DStream, OutStream: TStream;
begin
  OutStream := TMemoryStream.Create;
  try
    Bz2DStream := TBZDecompressionStream.Create(aStream);
    try
      // Bzip2 streams don't support .Size
      BytesRead := Bz2DStream.Read(Buf, SizeOf(Buf));
      while BytesRead > 0 do begin
        OutStream.WriteBuffer(Buf, BytesRead);
        BytesRead := Bz2DStream.Read(Buf, SizeOf(Buf));
      end;
    finally
      Bz2DStream.Free;
    end;
    OutStream.Position := 0;
    CheckFileMatchesStream(aExpectedFile, OutStream);
  finally
    OutStream.Free;
  end;
end;

procedure TAbBzip2Tests.TestDecompressionStream;
var
  InStream: TStream;
begin
  InStream := TFileStream.Create(MPLDir + 'MPL.bz2', fmOpenRead or fmShareDenyNone);
  try
    TestDecompress(InStream, MPLDir + 'MPL-1_1.txt');
  finally
    InStream.Free;
  end;
end;

procedure TAbBzip2Tests.TestCompressionStream;
var
  Bz2CStream, CompressedStream, MPLStream: TStream;
begin
  CompressedStream := TMemoryStream.Create;
  try
    Bz2CStream := TBZCompressionStream.Create(bs5, CompressedStream);
    try
      MPLStream := TFileStream.Create(MPLDir + 'MPL-1_1.txt', fmOpenRead or fmShareDenyNone);
      try
        Bz2CStream.CopyFrom(MPLStream, 0);
      finally
        MPLStream.Free;
      end;
    finally
      Bz2CStream.Free;
    end;
    CompressedStream.Position := 0;
    TestDecompress(CompressedStream, MPLDir + 'MPL-1_1.txt');
  finally
    CompressedStream.Free;
  end;
end;

initialization

  TestFramework.RegisterTest('Abbrevia.Low-Level Compression Test Suite',
    TAbBzip2Tests.Suite);
 
end.

 