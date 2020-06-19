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

 unit AbZipTypTests;

{$I AbDefine.inc}

interface

uses
  Classes, TestFrameWork, AbTestFrameWork, AbArcTypTests, AbArcTyp, AbUtils, AbZipTyp;

type
  TAbZipArchiveTests = class(TAbArchiveMultiFileTests)
  private
    class function DecompressSuite(const aDir: string): ITestSuite;

  protected
    class procedure AddCanterburyTests(aSuite: ITestSuite); override;
    class function ArchiveClass: TAbArchiveClass; override;
    class function ArchiveExt: string; override;
    class function ArchiveType: TAbArchiveType; override;
    class function VerifyArchive(aStream: TStream): TAbArchiveType; override;

  public
    class function Suite: ITestSuite; override;

  published
    procedure TestTestItemAt;
    procedure TestTestItemAtCorrupt;
  end;

  TAbZipDecompressTest = class(TAbArchiveDecompressTest)
  protected
    function CreateArchive(const aFileName: string; aMode : Word): TAbArchive;
      override;
  end;

  TAbZipCompressTest = class(TAbArchiveCompressTest)
  private
    FMethod: TAbZipSupportedMethod;
    FPassword: string;
  protected
    function CreateArchive(const aFileName: string; aMode : Word): TAbArchive;
      override;
  public
    constructor Create(aParent: TAbArchiveTestsClass;
      const aTestName, aSourceDir: string; aMethod: TAbZipSupportedMethod;
      const aPassword: string); reintroduce;
  end;

  TAbZip64Tests = class(TAbTestCase)
  private
    procedure CheckLargeListing(const aFileName: string);
    function Zip64Dir: string;
  public
    //These tests don't work.
    procedure TestExtractLargeUncompressedSize;
    procedure TestLoadLargeListing;
    procedure TestLoadLargeUncompressedSize;
  published
    procedure TestSaveLargeListing;
  end;

implementation

uses
  SysUtils, AbBitBkt, AbConst, AbExcept, AbUnzPrc, AbZipPrc;

{----------------------------------------------------------------------------}
{ TAbZipArchive with Extract/Insert helpers }
{----------------------------------------------------------------------------}

type
  TAbZipArchive = class(AbZipTyp.TAbZipArchive)
  private
    procedure DoExtractHelper(Sender : TObject; Item : TAbArchiveItem;
      const NewName : string);
    procedure DoExtractToStreamHelper(Sender : TObject; Item : TAbArchiveItem;
      OutStream : TStream);
    procedure DoInsertHelper(Sender : TObject; Item : TAbArchiveItem;
      OutStream : TStream);
    procedure DoInsertFromStreamHelper(Sender : TObject; Item : TAbArchiveItem;
      OutStream, InStream : TStream);
    procedure DoTestHelper(Sender : TObject; Item : TAbArchiveItem);
  public {methods}
    constructor CreateFromStream( aStream : TStream; const ArchiveName : string );
      override;
  end;

constructor TAbZipArchive.CreateFromStream( aStream : TStream;
  const ArchiveName : string );
begin
  inherited;
  ExtractHelper := DoExtractHelper;
  ExtractToStreamHelper := DoExtractToStreamHelper;
  InsertHelper := DoInsertHelper;
  InsertFromStreamHelper := DoInsertFromStreamHelper;
  TestHelper := DoTestHelper;
end;
{----------------------------------------------------------------------------}
procedure TAbZipArchive.DoExtractHelper(Sender : TObject;
  Item : TAbArchiveItem; const NewName : string);
begin
  AbUnzip(Sender, TAbZipItem(Item), NewName);
end;
{----------------------------------------------------------------------------}
procedure TAbZipArchive.DoExtractToStreamHelper(Sender : TObject;
  Item : TAbArchiveItem; OutStream : TStream);
begin
  AbUnzipToStream(Sender, TAbZipItem(Item), OutStream);
end;
{----------------------------------------------------------------------------}
procedure TAbZipArchive.DoInsertHelper(Sender : TObject;
  Item : TAbArchiveItem; OutStream : TStream);
begin
  AbZip(TAbZipArchive(Sender), TAbZipItem(Item), OutStream);
end;
{----------------------------------------------------------------------------}
procedure TAbZipArchive.DoInsertFromStreamHelper(Sender : TObject;
  Item : TAbArchiveItem; OutStream, InStream : TStream);
begin
  AbZipFromStream(TAbZipArchive(Sender), TAbZipItem(Item), OutStream, InStream);
end;
{----------------------------------------------------------------------------}
procedure TAbZipArchive.DoTestHelper(Sender : TObject; Item : TAbArchiveItem);
begin
  AbTestZipItem(Sender, TAbZipItem(Item));
end;

{----------------------------------------------------------------------------}
{ TAbZipArchiveTests }
{----------------------------------------------------------------------------}
class function TAbZipArchiveTests.DecompressSuite(const aDir: string): ITestSuite;
var
  SR: TSearchRec;
  Dir: string;
  sTestName: string;
  sSuiteName: string;
begin
  Dir := ExcludeTrailingPathDelimiter(aDir);
  sSuiteName := 'Decompress ' + ExtractFileName(Dir);
  Result := TTestSuite.Create(sSuiteName);
  if FindFirst(aDir + PathDelim + '*.zip', faAnyFile, SR) = 0 then
    try
      repeat
        sTestName := ChangeFileExt(SR.Name, '');
		//The DCLImpl test also doesn't work
        if sTestName <> 'DCLImpl' then
          Result.AddTest(TAbZipDecompressTest.Create(Self,
            sTestName, Dir + PathDelim + SR.Name));
      until FindNext(SR) <> 0;
    finally
      FindClose(SR);
    end;
end;
{ -------------------------------------------------------------------------- }
class function TAbZipArchiveTests.Suite: ITestSuite;
begin
  Result := inherited Suite;
  {$IF DEFINED(UNICODE) OR NOT DEFINED(MSWINDOWS)}
  // Test decompression of Unicode filenames, currently doesn't work
//  Result.AddSuite(DecompressSuite(TestFileDir + 'Unicode'));
  {$IFEND}
  // Test ZIP64 extensions
  Result.AddSuite(TAbZip64Tests.Suite);

//  {$IFDEF UnzipWavPackSupport}
//  // Test decompressiong of .wav files, currently doesn't work
//  Result.AddTest(
//    TAbZipDecompressTest.Create(Self, 'Decompress WavPack',
//      TestFileDir + 'WavPack' + PathDelim + 'wavpack.zip'));
//  {$ENDIF}
end;
{ -------------------------------------------------------------------------- }
class procedure TAbZipArchiveTests.AddCanterburyTests(aSuite: ITestSuite);
var
  CompressSuite: ITestSuite;
begin
  aSuite.AddSuite(DecompressSuite(CanterburyDir));

  CompressSuite := TTestSuite.Create('Compress Canterbury');
  CompressSuite.AddTest(
    TAbZipCompressTest.Create(Self, 'Store', CanterburySourceDir, smStored, ''));
  CompressSuite.AddTest(
    TAbZipCompressTest.Create(Self, 'Deflate', CanterburySourceDir, smDeflated, ''));
  CompressSuite.AddTest(
    TAbZipCompressTest.Create(Self, 'StoreP', CanterburySourceDir, smStored, 'password'));
  CompressSuite.AddTest(
    TAbZipCompressTest.Create(Self, 'DeflateP', CanterburySourceDir, smDeflated, 'password'));
  aSuite.AddSuite(CompressSuite);
end;
{ -------------------------------------------------------------------------- }
class function TAbZipArchiveTests.ArchiveClass: TAbArchiveClass;
begin
  Result := TAbZipArchive;
end;
{ -------------------------------------------------------------------------- }
class function TAbZipArchiveTests.ArchiveExt: string;
begin
  Result := '.zip';
end;
{ -------------------------------------------------------------------------- }
class function TAbZipArchiveTests.ArchiveType: TAbArchiveType;
begin
  Result := atZip;
end;
{ -------------------------------------------------------------------------- }
class function TAbZipArchiveTests.VerifyArchive(aStream: TStream): TAbArchiveType;
begin
  Result := VerifyZip(aStream);
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipArchiveTests.TestTestItemAt;
var
  Arc: TAbZipArchive;
begin
  Arc := TAbZipArchive.Create(MPLDir + 'MPL.zip', fmOpenRead);
  try
    Arc.Load;
    Arc.TestItemAt(0);
    Check(True);
  finally
    Arc.Free;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZipArchiveTests.TestTestItemAtCorrupt;
var
  Arc: TAbZipArchive;
begin
  Arc := TAbZipArchive.Create(TestFileDir + 'CorruptMPL.zip', fmOpenRead);
  try
    Arc.Load;
    try
      Arc.TestItemAt(0);
    except
      on E: Exception do
        Check((E is EAbZipBadCRC) or (E is EAbZipInvalidLFH));
    end;
  finally
    Arc.Free;
  end;
end;


{----------------------------------------------------------------------------}
{ TAbZipDecompressTest }
{----------------------------------------------------------------------------}
function TAbZipDecompressTest.CreateArchive(const aFileName: string;
  aMode : Word): TAbArchive;
begin
  Result := inherited CreateArchive(aFileName, aMode);
  (Result as TAbZipArchive).Password := 'password';
end;

{----------------------------------------------------------------------------}
{ TAbZipCompressTest }
{----------------------------------------------------------------------------}
constructor TAbZipCompressTest.Create(aParent: TAbArchiveTestsClass;
  const aTestName, aSourceDir: string; aMethod: TAbZipSupportedMethod;
  const aPassword: string);
begin
  inherited Create(aParent, aTestName, aSourceDir);
  FMethod := aMethod;
  FPassword := aPassword;
end;
{----------------------------------------------------------------------------}
function TAbZipCompressTest.CreateArchive(const aFileName: string;
  aMode : Word): TAbArchive;
begin
  Result := inherited CreateArchive(aFileName, aMode);
  (Result as TAbZipArchive).CompressionMethodToUse := FMethod;
  (Result as TAbZipArchive).Password := FPassword;
end;


{----------------------------------------------------------------------------}
{ TAbZip64Tests }
{----------------------------------------------------------------------------}
procedure TAbZip64Tests.CheckLargeListing(const aFileName: string);
var
  Arc: TAbZipArchive;
  i: Integer;
  ExpectedName: string;
begin
  Arc := TAbZipArchive.Create(aFileName, fmOpenRead);
  try
    Arc.Load;
    CheckEquals(90000, Arc.Count, 'Item counts don''t match');
    for i := 0 to Arc.Count - 1 do begin
      ExpectedName := Format('%.5d.txt', [i]);
      CheckEquals(ExpectedName, Arc.Items[i].FileName);
    end;
  finally
    Arc.Free;
  end;
end;
{----------------------------------------------------------------------------}
procedure TAbZip64Tests.TestLoadLargeListing;
begin
  CheckLargeListing(Zip64Dir + '90,000_files.zip');
end;
{----------------------------------------------------------------------------}
procedure TAbZip64Tests.TestSaveLargeListing;
var
  Arc: TAbZipArchive;
  Filename: string;
  Item : TAbArchiveItem;
  i: Integer;
begin
  Filename := TestTempDir + '90,000_files.zip';
  Arc := TAbZipArchive.Create(Filename, fmCreate);
  try
    Arc.FInStream := TMemoryStream.Create;
    try
      { Use ItemList.Add instead of AddFromStream so we can use the same
        source stream and only save to disk once }
      for i := 0 to 89999 do begin
        Item := Arc.CreateItem(Format('%.5d.txt', [i]));
        Item.Action := aaStreamAdd;
        Arc.ItemList.Add(Item);
      end;
      Arc.IsDirty := True;
      Arc.Save;
    finally
      Arc.FInStream.Free;
    end;
  finally
    Arc.Free;
  end;
  CheckLargeListing(Filename);
end;
{----------------------------------------------------------------------------}
procedure TAbZip64Tests.TestLoadLargeUncompressedSize;
var
  Arc: TAbZipArchive;
begin
  Arc := TAbZipArchive.Create(Zip64Dir + 'Zeros.zip', fmOpenRead);
  try
    Arc.Load;
    CheckEquals(5368709120, Arc.Items[0].UncompressedSize);
  finally
    Arc.Free;
  end;
end;
{----------------------------------------------------------------------------}
procedure TAbZip64Tests.TestExtractLargeUncompressedSize;
var
  Arc: TAbZipArchive;
  Stream: TStream;
begin
  Arc := TAbZipArchive.Create(Zip64Dir + 'Zeros.zip', fmOpenRead);
  try
    Arc.Load;
    Stream := TAbBitBucketStream.Create(32768);
    try
      Arc.ExtractToStream('0000', Stream);
      CheckEquals(5368709120, Stream.Size);
    finally
      Stream.Free;
    end;
  finally
    Arc.Free;
  end;
end;
{----------------------------------------------------------------------------}
function TAbZip64Tests.Zip64Dir: string;
begin
  Result := TestFileDir + 'Zip64' + PathDelim;
end;
{----------------------------------------------------------------------------}

initialization

  TestFramework.RegisterTest('Abbrevia.Low-Level Compression Test Suite',
    TAbZipArchiveTests.Suite);

end.
