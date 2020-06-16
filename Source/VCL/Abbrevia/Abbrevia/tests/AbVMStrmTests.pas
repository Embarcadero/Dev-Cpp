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

unit AbVMStrmTests;

{$I AbDefine.inc}

interface

uses
  AbTestFramework;

type

  TAbVMStrmTests = class(TAbTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestReadWrite;
    procedure TestEmptyStream;
  end;

implementation

uses
  SysUtils, TestFrameWork, AbVMStrm;

{ TAbVMStrmTests }

procedure TAbVMStrmTests.SetUp;
begin
  inherited;
end;

procedure TAbVMStrmTests.TearDown;
begin
  inherited;
end;

procedure TAbVMStrmTests.TestReadWrite;
var
  Buf: array[0..255] of Byte;
  i: Integer;
  Stream: TAbVirtualMemoryStream;
begin
  Stream := TAbVirtualMemoryStream.Create;
  try
    for i := Low(Buf) to High(Buf) do
      Buf[i] := i;
    CheckEquals(Stream.Write(Buf, SizeOf(Buf)), SizeOf(Buf), 'Stream write failed');
    FillChar(Buf, SizeOf(Buf), 0);
    Stream.Position := 0;
    CheckEquals(Stream.Read(Buf, SizeOf(Buf)), SizeOf(Buf), 'Stream read failed');
    for i := Low(Buf) to High(Buf) do
      if Buf[i] <> i then
        FailEquals(IntToStr(i), IntToStr(Buf[i]));
  finally
    Stream.Free;
  end;
end;

procedure TAbVMStrmTests.TestEmptyStream;
var
  Buf: array[0..255] of Byte;
  Stream: TAbVirtualMemoryStream;
begin
  Stream := TAbVirtualMemoryStream.Create;
  try
    FillChar(Buf, SizeOf(Buf), $CC);
    Stream.WriteBuffer(Buf, SizeOf(Buf));
    Stream.Size := 0;
    CheckEquals(Stream.Read(Buf, SizeOf(Buf)), 0);
  finally
    Stream.Free;
  end;
end;

initialization

  TestFramework.RegisterTest('Abbrevia.Utility Classes',
    TAbVMStrmTests.Suite);
 
end.

 