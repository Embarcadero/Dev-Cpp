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

unit _GZipItem;

interface

uses
  ComObj, Abbrevia_TLB, AbGzTyp, AbZipKit;

type

  TGZipItem = class(TAutoIntfObject, IGZipItem)
  private
    FOwner  : TAbGzipItem;
    FParent : TAbZipKit;
  public
    constructor Create(AOwner : TAbGzipItem; AParent : TAbZipKit);
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

    {IGZipItem}
    function  Get_CompressionMethod: Byte; safecall;
    procedure Set_CompressionMethod(Value: Byte); safecall;
    function  Get_ExtraField: WideString; safecall;
    procedure Set_ExtraField(const Value: WideString); safecall;
    function  Get_ExtraFlags: Byte; safecall;
    procedure Set_ExtraFlags(Value: Byte); safecall;
    function  Get_FileComment: WideString; safecall;
    procedure Set_FileComment(const Value: WideString); safecall;
    function  Get_FileSystem: TFileSystem; safecall;
    procedure Set_FileSystem(Value: TFileSystem); safecall;
    function  Get_Flags: Byte; safecall;
    procedure Set_Flags(Value: Byte); safecall;
    function  Get_HeaderCRC: Integer; safecall;
  end;


implementation

uses
  ComServ, {StStrL,} SysUtils;

{------------------------------------------------------------------------------}
constructor TGzipItem.Create(AOwner : TAbGzipItem; AParent : TAbZipKit);
begin
  inherited Create(ComServer.TypeLib, IGZipItem);
  FOwner := AOwner;
  FParent := AParent;
end;
{------------------------------------------------------------------------------}
{IArchiveItem}
{------------------------------------------------------------------------------}
function  TGzipItem.Get_Action: TArchiveAction;
begin
  Result := TArchiveAction(FOwner.Action);
end;
{------------------------------------------------------------------------------}
function  TGzipItem.Get_CompressedSize: Integer;
begin
  result := FOwner.CompressedSize;
end;
{------------------------------------------------------------------------------}
function  TGzipItem.Get_CRC32: Integer;
begin
  result := FOwner.CRC32;
end;
{------------------------------------------------------------------------------}
function  TGzipItem.Get_CRC32St: WideString;
begin
  result := IntToHex(FOwner.CRC32, 8);
end;
{------------------------------------------------------------------------------}
function  TGzipItem.Get_DiskFileName: WideString;
begin
  result := FOwner.DiskFileName;
end;
{------------------------------------------------------------------------------}
function  TGzipItem.Get_DiskPath: WideString;
begin
  result := FOwner.DiskPath;
end;
{------------------------------------------------------------------------------}
function  TGzipItem.Get_ExternalFileAttributes: TFileAttributes;
begin
  result := TFileAttributes(FOwner.ExternalFileAttributes);
end;
{------------------------------------------------------------------------------}
procedure TGzipItem.Set_ExternalFileAttributes(Value: TFileAttributes);
begin
  FOwner.ExternalFileAttributes := LongInt(Value);
  FParent.ZipArchive.IsDirty := True;
end;
{------------------------------------------------------------------------------}
function  TGzipItem.Get_FileName: WideString;
begin
  result := FOwner.FileName;
end;
{------------------------------------------------------------------------------}
procedure TGzipItem.Set_FileName(const Value: WideString);
begin
  FOwner.FileName := Value;
end;
{------------------------------------------------------------------------------}
function  TGzipItem.Get_IsEncrypted: WordBool;
begin
  result := FOwner.IsEncrypted;
end;
{------------------------------------------------------------------------------}
function  TGzipItem.Get_LastModFileDateTime: TDateTime;
begin
  result := FileDateToDateTime((FOwner.LastModFileDate shl 16) + FOwner.LastModFileTime);
end;
{------------------------------------------------------------------------------}
function  TGzipItem.Get_StoredPath: WideString;
begin
  result := FOwner.StoredPath;
end;
{------------------------------------------------------------------------------}
function  TGzipItem.Get_Tagged: WordBool;
begin
  result := FOwner.Tagged;
end;
{------------------------------------------------------------------------------}
procedure TGzipItem.Set_Tagged(Value: WordBool);
begin
  FOwner.Tagged := Value;
end;
{------------------------------------------------------------------------------}
function  TGzipItem.Get_UnCompressedSize: Integer;
begin
  result := FOwner.UncompressedSize;
end;
{------------------------------------------------------------------------------}
function  TGzipItem.Get_Password: WideString;
begin
  {!!!}
  //result := FOwner.Password;
end;
{------------------------------------------------------------------------------}
procedure TGzipItem.Set_Password(const Value: WideString);
begin
  {!!!}
  //FOwner.Password := Value;
  //FParent.ZipArchive.IsDirty := True;
end;
{------------------------------------------------------------------------------}
{IGZipItem}
{------------------------------------------------------------------------------}
function  TGzipItem.Get_CompressionMethod: Byte;
begin
  result := FOwner.CompressionMethod;
end;
{------------------------------------------------------------------------------}
procedure TGzipItem.Set_CompressionMethod(Value: Byte);
begin

end;
{------------------------------------------------------------------------------}
function  TGzipItem.Get_ExtraField: WideString;
begin
  result := '';
end;
{------------------------------------------------------------------------------}
procedure TGzipItem.Set_ExtraField(const Value: WideString);
begin

end;
{------------------------------------------------------------------------------}
function  TGzipItem.Get_ExtraFlags: Byte;
begin
  result := FOwner.ExtraFlags;
end;
{------------------------------------------------------------------------------}
procedure TGzipItem.Set_ExtraFlags(Value: Byte);
begin
  FOwner.ExtraFlags := Value;
  FParent.ZipArchive.IsDirty := True;
end;
{------------------------------------------------------------------------------}
function  TGzipItem.Get_FileComment: WideString;
begin
  result := WideString(FOwner.FileComment);
end;
{------------------------------------------------------------------------------}
procedure TGzipItem.Set_FileComment(const Value: WideString);
begin
  FOwner.FileComment := AnsiString(Value);
  FParent.ZipArchive.IsDirty := True;
end;
{------------------------------------------------------------------------------}
function  TGzipItem.Get_FileSystem: TFileSystem;
begin
  result := TFileSystem(FOwner.FileSystem);
end;
{------------------------------------------------------------------------------}
procedure TGzipItem.Set_FileSystem(Value: TFileSystem);
begin
  FOwner.FileSystem := TAbGzFileSystem(Value);
  FParent.ZipArchive.IsDirty := True;
end;
{------------------------------------------------------------------------------}
function  TGzipItem.Get_Flags: Byte;
begin
  result := FOwner.Flags;
end;
{------------------------------------------------------------------------------}
procedure TGzipItem.Set_Flags(Value: Byte);
begin

end;
{------------------------------------------------------------------------------}
function  TGzipItem.Get_HeaderCRC: Integer;
begin
  result := 0;
end;
{------------------------------------------------------------------------------}




end.
