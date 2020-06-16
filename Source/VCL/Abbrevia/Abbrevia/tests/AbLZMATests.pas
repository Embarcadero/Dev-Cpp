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
 * Portions created by the Initial Developer are Copyright (C) 2011
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Craig Peterson <capeterson@users.sourceforge.net>
 *
 * ***** END LICENSE BLOCK ***** *)

unit AbLZMATests;

{$I AbDefine.inc}

interface

uses
  AbTestFramework;

type

  TAbLZMATests = class(TAbTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDecodeStream;
    procedure TestEncodeStream;
    {$IFDEF HasAdvancedRecords}
    procedure TestDecompressionStream;
    procedure TestCompressionStream;
    {$ENDIF}
  end;

implementation

uses
  Classes, SysUtils, TestFrameWork,
  {$IFDEF HasAdvancedRecords}AbLZMAStream,{$ENDIF} AbLZMA;

{ TAbLZMATests }

procedure TAbLZMATests.SetUp;
begin
  inherited;
end;

procedure TAbLZMATests.TearDown;
begin
  inherited;
end;

procedure TAbLZMATests.TestDecodeStream;
var
  SrcStream, DesStream: TStream;
begin
  DesStream := TMemoryStream.Create;
  try
    SrcStream := TFileStream.Create(MPLDir + 'MPL.lzma', fmOpenRead or fmShareDenyNone);
    try
      LzmaDecodeStream(SrcStream, DesStream);
    finally
      SrcStream.Free;
    end;
    CheckFileMatchesStream(MPLDir + 'MPL-1_1.txt', DesStream);
  finally
    DesStream.Free;
  end;
end;

procedure TAbLZMATests.TestEncodeStream;
var
  MPLStream, CompStream, DecompStream: TStream;
begin
  MPLStream := TFileStream.Create(MPLDir + 'MPL-1_1.txt', fmOpenRead or fmShareDenyNone);
  try
    DecompStream := TMemoryStream.Create;
    try
      CompStream := TMemoryStream.Create;
      try
        LzmaEncodeStream(MPLStream, CompStream, MPLStream.Size);
        CompStream.Position := 0;
        LzmaDecodeStream(CompStream, DecompStream);
      finally
        CompStream.Free;
      end;
      CheckStreamMatch(MPLStream, DecompStream);
    finally
      DecompStream.Free;
    end;
  finally
    MPLStream.Free;
  end;
end;

{$IFDEF HasAdvancedRecords}
procedure TAbLZMATests.TestDecompressionStream;
var
  InStream, LZDStream: TStream;
begin
  InStream := TFileStream.Create(MPLDir + 'MPL.lzma', fmOpenRead or fmShareDenyNone);
  try
    LZDStream := TAbLZMADecompressionStream.Create(InStream);
    try
      CheckFileMatchesStream(MPLDir + 'MPL-1_1.txt', LZDStream);
    finally
      LZDStream.Free;
    end;
  finally
    InStream.Free;
  end;
end;

procedure TAbLZMATests.TestCompressionStream;
var
  LZCStream, LZDStream, CompressedStream, MPLStream: TStream;
begin
  CompressedStream := TMemoryStream.Create;
  try
    LZCStream := TAbLZMACompressionStream.Create(CompressedStream);
    try
      MPLStream := TFileStream.Create(MPLDir + 'MPL-1_1.txt', fmOpenRead or fmShareDenyNone);
      try
        LZCStream.CopyFrom(MPLStream, 0);
      finally
        MPLStream.Free;
      end;
    finally
      LZCStream.Free;
    end;
    CompressedStream.Position := 0;
    LZDStream := TAbLZMADecompressionStream.Create(CompressedStream);
    try
      CheckFileMatchesStream(MPLDir + 'MPL-1_1.txt', LZDStream);
    finally
      LZDStream.Free;
    end;
  finally
    CompressedStream.Free;
  end;
end;
{$ENDIF}

initialization

  TestFramework.RegisterTest('Abbrevia.Low-Level Compression Test Suite',
    TAbLZMATests.Suite);
 
end.

 