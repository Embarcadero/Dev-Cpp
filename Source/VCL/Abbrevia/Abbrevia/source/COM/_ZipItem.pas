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
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

unit _ZipItem;

interface

uses
  ComObj, Abbrevia_TLB, AbZipTyp, AbZipKit;

type
  TZipItem = class(TAutoIntfObject, IZipItem)
  private
    FOwner  : TAbZipItem;
    FParent : TAbZipKit;
  public
    constructor Create(AOwner : TAbZipItem; AParent : TAbZipKit);
  protected
    {IArchiveItem}
    function  Get_Action: TArchiveAction; safecall;
    function  Get_CompressedSize: Integer; safecall;
    function  Get_CRC32: Integer; safecall;
    function  Get_CRC32St: WideString; safecall;
    function  Get_DiskFileName: WideString; safecall;
    function  Get_DiskPath: WideString; safecall;
    function  Get_ExternalFileAttributes: TFileAttributes; safecall;
    procedure Set_ExternalFileAttributes(Value: TFileAttributes); safecall;
    function  Get_FileName: WideString; safecall;
    procedure Set_FileName(const Value: WideString); safecall;
    function  Get_IsEncrypted: WordBool; safecall;
    function  Get_LastModFileDateTime: TDateTime; safecall;
    function  Get_StoredPath: WideString; safecall;
    function  Get_Tagged: WordBool; safecall;
    procedure Set_Tagged(Value: WordBool); safecall;
    function  Get_UnCompressedSize: Integer; safecall;
    function  Get_Password: WideString; safecall;
    procedure Set_Password(const Value: WideString); safecall;

    {IZipItem}
    function  Get_CompressionMethod: TZipCompressionMethod; safecall;
    function  Get_CompressionRatio: Double; safecall;
    function  Get_DeflateOption: TZipDeflateOption; safecall;
    function  Get_DictionarySize: TZipDictionarySize; safecall;
    function  Get_DiskNumberStart: Integer; safecall;
    function  Get_ExtraField: WideString; safecall;
    procedure Set_ExtraField(const Value: WideString); safecall;
    function  Get_FileComment: WideString; safecall;
    procedure Set_FileComment(const Value: WideString); safecall;
    function  Get_InternalFileAttributes: Integer; safecall;
    procedure Set_InternalFileAttributes(Value: Integer); safecall;
    function  Get_VersionMadeBy: Integer; safecall;
    function  Get_VersionNeededToExtract: Integer; safecall;
  end;

implementation

uses
  ComServ, SysUtils;

{------------------------------------------------------------------------------}
constructor TZipItem.Create(AOwner : TAbZipItem; AParent : TAbZipKit);
begin
  inherited Create(ComServer.TypeLib, IZipItem);
  FOwner := AOwner;
  FParent := AParent;
end;
{------------------------------------------------------------------------------}
{IArchiveItem}
{------------------------------------------------------------------------------}
function  TZipItem.Get_Action: TArchiveAction;
begin
  Result := TArchiveAction(FOwner.Action);
end;
{------------------------------------------------------------------------------}
function  TZipItem.Get_CompressedSize: Integer;
begin
  result := FOwner.CompressedSize;
end;
{------------------------------------------------------------------------------}
function  TZipItem.Get_CRC32: Integer;
begin
  result := FOwner.CRC32;
end;
{------------------------------------------------------------------------------}
function  TZipItem.Get_CRC32St: WideString;
begin
  result := IntToHex(FOwner.CRC32, 8);
end;
{------------------------------------------------------------------------------}
function  TZipItem.Get_DiskFileName: WideString;
begin
  result := FOwner.DiskFileName;
end;
{------------------------------------------------------------------------------}
function  TZipItem.Get_DiskPath: WideString;
begin
  result := FOwner.DiskPath;
end;
{------------------------------------------------------------------------------}
function  TZipItem.Get_ExternalFileAttributes: TFileAttributes;
begin
  result := TFileAttributes(FOwner.ExternalFileAttributes);
end;
{------------------------------------------------------------------------------}
procedure TZipItem.Set_ExternalFileAttributes(Value: TFileAttributes);
begin
  FOwner.ExternalFileAttributes := LongInt(Value);
  FParent.ZipArchive.IsDirty := True;
end;
{------------------------------------------------------------------------------}
function  TZipItem.Get_FileName: WideString;
begin
  result := FOwner.FileName;
end;
{------------------------------------------------------------------------------}
procedure TZipItem.Set_FileName(const Value: WideString);
begin
  FOwner.FileName := Value;
end;
{------------------------------------------------------------------------------}
function  TZipItem.Get_IsEncrypted: WordBool;
begin
  result := FOwner.IsEncrypted;
end;
{------------------------------------------------------------------------------}
function  TZipItem.Get_LastModFileDateTime: TDateTime;
begin
  result := FileDateToDateTime((FOwner.LastModFileDate shl 16) + FOwner.LastModFileTime);
end;
{------------------------------------------------------------------------------}
function  TZipItem.Get_StoredPath: WideString;
begin
  result := FOwner.StoredPath;
end;
{------------------------------------------------------------------------------}
function  TZipItem.Get_Tagged: WordBool;
begin
  result := FOwner.Tagged;
end;
{------------------------------------------------------------------------------}
procedure TZipItem.Set_Tagged(Value: WordBool);
begin
  FOwner.Tagged := Value;
end;
{------------------------------------------------------------------------------}
function  TZipItem.Get_UnCompressedSize: Integer;
begin
  result := FOwner.UncompressedSize;
end;
{------------------------------------------------------------------------------}
function  TZipItem.Get_Password: WideString;
begin
  Result := WideString(FParent.Password);
end;
{------------------------------------------------------------------------------}
procedure TZipItem.Set_Password(const Value: WideString);
begin
  FParent.Password := AnsiString(Value);
  FParent.ZipArchive.IsDirty := True;
end;
{------------------------------------------------------------------------------}
{IZipItem}
{------------------------------------------------------------------------------}
function  TZipItem.Get_CompressionMethod: TZipCompressionMethod;
begin
  Result := TZipCompressionMethod(FOwner.CompressionMethod);
end;
{------------------------------------------------------------------------------}
function  TZipItem.Get_CompressionRatio: Double;
begin
  result := FOwner.CompressionRatio;
end;
{------------------------------------------------------------------------------}
function  TZipItem.Get_DeflateOption: TZipDeflateOption;
begin
  result := TZipDeflateOption(FOwner.DeflationOption);
end;
{------------------------------------------------------------------------------}
function  TZipItem.Get_DictionarySize: TZipDictionarySize;
begin
  result := TZipDictionarySize(FOwner.DictionarySize);
end;
{------------------------------------------------------------------------------}
function  TZipItem.Get_DiskNumberStart: Integer;
begin
  result := FOwner.DiskNumberStart;
end;
{------------------------------------------------------------------------------}
function  TZipItem.Get_ExtraField: WideString;
begin
  result := '';
end;
{------------------------------------------------------------------------------}
procedure TZipItem.Set_ExtraField(const Value: WideString);
begin

end;
{------------------------------------------------------------------------------}
function  TZipItem.Get_FileComment: WideString;
begin
  result := WideString(FOwner.FileComment);
end;
{------------------------------------------------------------------------------}
procedure TZipItem.Set_FileComment(const Value: WideString);
begin
  FOwner.FileComment := AnsiString(Value);
  FParent.ZipArchive.IsDirty := True;
end;
{------------------------------------------------------------------------------}
function  TZipItem.Get_InternalFileAttributes: Integer;
begin
  result := FOwner.InternalFileAttributes;
end;
{------------------------------------------------------------------------------}
procedure TZipItem.Set_InternalFileAttributes(Value: Integer);
begin
  FOwner.InternalFileAttributes := Value;
  FParent.ZipArchive.IsDirty := True;
end;
{------------------------------------------------------------------------------}
function  TZipItem.Get_VersionMadeBy: Integer;
begin
  result := FOwner.VersionMadeBy;
end;
{------------------------------------------------------------------------------}
function  TZipItem.Get_VersionNeededToExtract: Integer;
begin
  result := FOwner.VersionNeededToExtract;
end;
{------------------------------------------------------------------------------}


end.
