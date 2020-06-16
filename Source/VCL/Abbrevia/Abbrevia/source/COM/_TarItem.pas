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

unit _TarItem;

interface

uses
  ComObj, Abbrevia_TLB, AbTarTyp, AbZipKit;

type

  TTarItem = class(TAutoIntfObject, ITarItem)
  private
    FOwner  : TAbTarItem;
    FParent : TAbZipKit;
  public
    constructor Create(AOwner : TAbTarItem; AParent : TAbZipKit);
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

    {ITarItem}
    function  Get_DevMajor: Integer; safecall;
    procedure Set_DevMajor(Value: Integer); safecall;
    function  Get_DevMinor: Integer; safecall;
    procedure Set_DevMinor(Value: Integer); safecall;
    function  Get_GroupID: Integer; safecall;
    procedure Set_GroupID(Value: Integer); safecall;
    function  Get_GroupName: WideString; safecall;
    procedure Set_GroupName(const Value: WideString); safecall;
    function  Get_LinkFlag: Byte; safecall;
    procedure Set_LinkFlag(Value: Byte); safecall;
    function  Get_LinkName: WideString; safecall;
    procedure Set_LinkName(const Value: WideString); safecall;
    function  Get_Mode: Integer; safecall;
    procedure Set_Mode(Value: Integer); safecall;
    function  Get_UserID: Integer; safecall;
    procedure Set_UserID(Value: Integer); safecall;
    function  Get_UserName: WideString; safecall;
    procedure Set_UserName(const Value: WideString); safecall;

  end;


implementation

uses
  ComServ, {StStrL,} SysUtils;

{------------------------------------------------------------------------------}
constructor TTarItem.Create(AOwner : TAbTarItem; AParent : TAbZipKit);
begin
  inherited Create(ComServer.TypeLib, ITarItem);
  FOwner := AOwner;
  FParent := AParent;
end;
{------------------------------------------------------------------------------}
{IArchiveItem}
{------------------------------------------------------------------------------}
function  TTarItem.Get_Action: TArchiveAction;
begin
  Result := TArchiveAction(FOwner.Action);
end;
{------------------------------------------------------------------------------}
function  TTarItem.Get_CompressedSize: Integer;
begin
  result := FOwner.CompressedSize;
end;
{------------------------------------------------------------------------------}
function  TTarItem.Get_CRC32: Integer;
begin
  result := FOwner.CRC32;
end;
{------------------------------------------------------------------------------}
function  TTarItem.Get_CRC32St: WideString;
begin
  result := IntToHex(FOwner.CRC32, 8);
end;
{------------------------------------------------------------------------------}
function  TTarItem.Get_DiskFileName: WideString;
begin
  result := FOwner.DiskFileName;
end;
{------------------------------------------------------------------------------}
function  TTarItem.Get_DiskPath: WideString;
begin
  result := FOwner.DiskPath;
end;
{------------------------------------------------------------------------------}
function  TTarItem.Get_ExternalFileAttributes: TFileAttributes;
begin
  result := TFileAttributes(FOwner.ExternalFileAttributes);
end;
{------------------------------------------------------------------------------}
procedure TTarItem.Set_ExternalFileAttributes(Value: TFileAttributes);
begin
  FOwner.ExternalFileAttributes := LongInt(Value);
  FParent.ZipArchive.IsDirty := True;
end;
{------------------------------------------------------------------------------}
function  TTarItem.Get_FileName: WideString;
begin
  result := FOwner.FileName;
end;
{------------------------------------------------------------------------------}
procedure TTarItem.Set_FileName(const Value: WideString);
begin
  FOwner.FileName := Value;
end;
{------------------------------------------------------------------------------}
function  TTarItem.Get_IsEncrypted: WordBool;
begin
  result := FOwner.IsEncrypted;
end;
{------------------------------------------------------------------------------}
function  TTarItem.Get_LastModFileDateTime: TDateTime;
begin
  result := FileDateToDateTime((FOwner.LastModFileDate shl 16) + FOwner.LastModFileTime);
end;
{------------------------------------------------------------------------------}
function  TTarItem.Get_StoredPath: WideString;
begin
  result := FOwner.StoredPath;
end;
{------------------------------------------------------------------------------}
function  TTarItem.Get_Tagged: WordBool;
begin
  result := FOwner.Tagged;
end;
{------------------------------------------------------------------------------}
procedure TTarItem.Set_Tagged(Value: WordBool);
begin
  FOwner.Tagged := Value;
end;
{------------------------------------------------------------------------------}
function  TTarItem.Get_UnCompressedSize: Integer;
begin
  result := FOwner.UncompressedSize;
end;
{------------------------------------------------------------------------------}
function  TTarItem.Get_Password: WideString;
begin
  {!!!}
  //result := FOwner.Password;
end;
{------------------------------------------------------------------------------}
procedure TTarItem.Set_Password(const Value: WideString);
begin
  {!!!}
  //FOwner.Password := Value;
  //FParent.ZipArchive.IsDirty := True;
end;
{------------------------------------------------------------------------------}
{ITarItem}
{------------------------------------------------------------------------------}
function  TTarItem.Get_DevMajor: Integer;
begin
  result := FOwner.DevMajor;
end;
{------------------------------------------------------------------------------}
procedure TTarItem.Set_DevMajor(Value: Integer);
begin
  FOwner.DevMajor := Value;
  FParent.ZipArchive.IsDirty := True;
end;
{------------------------------------------------------------------------------}
function  TTarItem.Get_DevMinor: Integer;
begin
  result := FOwner.DevMinor;
end;
{------------------------------------------------------------------------------}
procedure TTarItem.Set_DevMinor(Value: Integer);
begin
  FOwner.DevMinor := Value;
  FParent.ZipArchive.IsDirty := True;
end;
{------------------------------------------------------------------------------}
function  TTarItem.Get_GroupID: Integer;
begin
  result := FOwner.GroupID;
end;
{------------------------------------------------------------------------------}
procedure TTarItem.Set_GroupID(Value: Integer);
begin
  FOwner.GroupID := Value;
  FParent.ZipArchive.IsDirty := True;
end;
{------------------------------------------------------------------------------}
function  TTarItem.Get_GroupName: WideString;
begin
  result := FOwner.GroupName;
end;
{------------------------------------------------------------------------------}
procedure TTarItem.Set_GroupName(const Value: WideString);
begin
  FOwner.GroupName := Value;
  FParent.ZipArchive.IsDirty := True;
end;
{------------------------------------------------------------------------------}
function  TTarItem.Get_LinkFlag: Byte;
begin
  result := Byte(FOwner.LinkFlag);
end;
{------------------------------------------------------------------------------}
procedure TTarItem.Set_LinkFlag(Value: Byte);
begin
  FOwner.LinkFlag := AnsiChar(Value);
  FParent.ZipArchive.IsDirty := True;
end;
{------------------------------------------------------------------------------}
function  TTarItem.Get_LinkName: WideString;
begin
  result := FOwner.LinkName;
end;
{------------------------------------------------------------------------------}
procedure TTarItem.Set_LinkName(const Value: WideString);
begin
  FOwner.LinkName := Value;
  FParent.ZipArchive.IsDirty := True;
end;
{------------------------------------------------------------------------------}
function  TTarItem.Get_Mode: Integer;
begin
  result := FOwner.Mode;
end;
{------------------------------------------------------------------------------}
procedure TTarItem.Set_Mode(Value: Integer);
begin
  FOwner.Mode := Value;
  FParent.ZipArchive.IsDirty := True;
end;
{------------------------------------------------------------------------------}
function  TTarItem.Get_UserID: Integer;
begin
  result := FOwner.UserID;
end;
{------------------------------------------------------------------------------}
procedure TTarItem.Set_UserID(Value: Integer);
begin
  FOwner.UserID := Value;
  FParent.ZipArchive.IsDirty := True;
end;
{------------------------------------------------------------------------------}
function  TTarItem.Get_UserName: WideString;
begin
  result := FOwner.UserName;
end;
{------------------------------------------------------------------------------}
procedure TTarItem.Set_UserName(const Value: WideString);
begin
  FOwner.UserName := Value;
  FParent.ZipArchive.IsDirty := True;
end;
{------------------------------------------------------------------------------}


end.
