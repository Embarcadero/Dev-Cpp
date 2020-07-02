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

unit Abbrevia_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// $Rev: 491 $
// File generated on 7/23/2009 9:45:45 PM from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\Abbrevia\COM\abbrevia.dll
// LIBID: {AF804E20-4043-499E-BB14-237B9F26F89F}
// LCID: 0
// Helpfile: C:\Abbrevia\COM\abrv-com.hlp
// HelpString: TurboPower Abbrevia Compression Library v3.03
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINDOWS\System32\stdole2.tlb)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, Graphics, StdVCL, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  AbbreviaMajorVersion = 5;
  AbbreviaMinorVersion = 0;

  LIBID_Abbrevia: TGUID = '{AF804E20-4043-499E-BB14-237B9F26F89F}';

  IID_IZipItem: TGUID = '{851699A1-422A-4C65-8E08-D0499ACDD834}';
  IID_IGZipItem: TGUID = '{8FA78CE0-FD29-441E-9777-93B63EF1A9EE}';
  IID_ITarItem: TGUID = '{729E9F52-C489-4A41-A770-4E2C5282AE39}';
  IID_IZipKit: TGUID = '{B7480A7F-4E27-4B45-9FE6-224B60295A0C}';
  DIID_IZipKitEvents: TGUID = '{F094D5F4-3A52-45AE-9D86-4409611DD29E}';
  CLASS_ZipItem: TGUID = '{650989D8-F0FF-4C71-83C3-92556F4329F5}';
  CLASS_GZipItem: TGUID = '{2B35BB50-D9C7-4669-B18E-943B5199FD8E}';
  CLASS_TarItem: TGUID = '{2DF3E624-0E6C-42CF-8041-676B9A06375E}';
  CLASS_ZipKit: TGUID = '{730B4B32-9127-492A-BF02-196A7E6B4E1B}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum TArchiveAction
type
  TArchiveAction = TOleEnum;
const
  aaFailed = $00000000;
  aaNone = $00000001;
  aaAdd = $00000002;
  aaDelete = $00000003;
  aaFreshen = $00000004;
  aaMove = $00000005;
  aaStreamAdd = $00000006;

// Constants for enum TArchiveStatus
type
  TArchiveStatus = TOleEnum;
const
  asInvalid = $00000000;
  asIdle = $00000001;
  asBusy = $00000002;

// Constants for enum TErrorClass
type
  TErrorClass = TOleEnum;
const
  eclAbbrevia = $00000000;
  eclInOutError = $00000001;
  eclFileError = $00000002;
  eclFileCreateError = $00000003;
  eclFileOpenError = $00000004;
  eclOther = $00000005;

// Constants for enum TFileAttributes
type
  TFileAttributes = TOleEnum;
const
  faReadOnly = $00000001;
  faHidden = $00000002;
  faSysFile = $00000004;
  faVolumeID = $00000008;
  faDirectory = $00000010;
  faArchive = $00000020;

// Constants for enum TProcessType
type
  TProcessType = TOleEnum;
const
  ptAdd = $00000000;
  ptDelete = $00000001;
  ptExtract = $00000002;
  ptFreshen = $00000003;
  ptMove = $00000004;
  ptReplace = $00000005;

// Constants for enum TStoreOptions
type
  TStoreOptions = TOleEnum;
const
  soStripDrive = $00000001;
  soStripPath = $00000002;
  soRemoveDots = $00000004;
  soRecurse = $00000008;
  soFreshen = $00000010;
  soReplace = $00000020;

// Constants for enum TZipCompressionMethod
type
  TZipCompressionMethod = TOleEnum;
const
  cmStored = $00000000;
  cmShrunk = $00000001;
  cmReduced1 = $00000002;
  cmReduced2 = $00000003;
  cmReduced3 = $00000004;
  cmReduced4 = $00000005;
  cmImploded = $00000006;
  cmTokenized = $00000007;
  cmDeflated = $00000008;
  cmEnhancedDeflated = $00000009;
  cmDCLImploded = $0000000A;
  cmBestMethod = $0000000B;

// Constants for enum TZipDeflateOption
type
  TZipDeflateOption = TOleEnum;
const
  doInvalid = $00000000;
  doNormal = $00000001;
  doMaximum = $00000002;
  doFast = $00000003;
  doSuperFast = $00000004;

// Constants for enum TZipDictionarySize
type
  TZipDictionarySize = TOleEnum;
const
  dsInvalid = $00000000;
  ds4K = $00000001;
  ds8K = $00000002;

// Constants for enum TZipExtractOptions
type
  TZipExtractOptions = TOleEnum;
const
  eoCreateDirs = $00000000;
  eoRestorePath = $00000001;

// Constants for enum TZipSupportMethod
type
  TZipSupportMethod = TOleEnum;
const
  smStored = $00000000;
  smDeflated = $00000001;
  smBestMethod = $00000002;

// Constants for enum TErrorCode
type
  TErrorCode = TOleEnum;
const
  ecDuplicateName = $00000000;
  ecInvalidPassword = $00000001;
  ecNoSuchDirectory = $00000002;
  ecUnknownCompressionMethod = $00000003;
  ecUserAbort = $00000004;
  ecZipBadCRC = $00000005;
  ecZipVersionNumber = $00000006;
  ecSpannedItemNotFound = $00000007;

// Constants for enum TArchiveType
type
  TArchiveType = TOleEnum;
const
  atUnknown = $00000000;
  atZip = $00000001;
  atSelfExtZip = $00000002;
  atTar = $00000003;
  atGZip = $00000004;
  atGZippedTar = $00000005;
  atCab = $00000006;

// Constants for enum TFileSystem
type
  TFileSystem = TOleEnum;
const
  fsFAT = $00000000;
  fsAmiga = $00000001;
  fsVMS = $00000002;
  fsUnix = $00000003;
  fsVM_CMS = $00000004;
  fsAtariTOS = $00000005;
  fsHPFS = $00000006;
  fsMacintosh = $00000007;
  fsZSystem = $00000008;
  fsCP_M = $00000009;
  fsTOPS20 = $0000000A;
  fsNTFS = $0000000B;
  fsQDOS = $0000000C;
  fsAcornRISCOS = $0000000D;
  fsUnknown = $0000000E;
  fsUndefined = $0000000F;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IZipItem = interface;
  IZipItemDisp = dispinterface;
  IGZipItem = interface;
  IGZipItemDisp = dispinterface;
  ITarItem = interface;
  ITarItemDisp = dispinterface;
  IZipKit = interface;
  IZipKitDisp = dispinterface;
  IZipKitEvents = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  ZipItem = IZipItem;
  GZipItem = IGZipItem;
  TarItem = ITarItem;
  ZipKit = IZipKit;


// *********************************************************************//
// Interface: IZipItem
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {851699A1-422A-4C65-8E08-D0499ACDD834}
// *********************************************************************//
  IZipItem = interface(IDispatch)
    ['{851699A1-422A-4C65-8E08-D0499ACDD834}']
    function Get_Action: TArchiveAction; safecall;
    function Get_CompressedSize: Integer; safecall;
    function Get_CRC32: Integer; safecall;
    function Get_DiskFileName: WideString; safecall;
    function Get_DiskPath: WideString; safecall;
    function Get_ExternalFileAttributes: TFileAttributes; safecall;
    procedure Set_ExternalFileAttributes(Value: TFileAttributes); safecall;
    function Get_FileName: WideString; safecall;
    procedure Set_FileName(const Value: WideString); safecall;
    function Get_IsEncrypted: WordBool; safecall;
    function Get_LastModFileDateTime: TDateTime; safecall;
    function Get_StoredPath: WideString; safecall;
    function Get_Tagged: WordBool; safecall;
    procedure Set_Tagged(Value: WordBool); safecall;
    function Get_UnCompressedSize: Integer; safecall;
    function Get_CRC32St: WideString; safecall;
    function Get_Password: WideString; safecall;
    procedure Set_Password(const Value: WideString); safecall;
    function Get_CompressionMethod: TZipCompressionMethod; safecall;
    function Get_CompressionRatio: Double; safecall;
    function Get_DeflateOption: TZipDeflateOption; safecall;
    function Get_DictionarySize: TZipDictionarySize; safecall;
    function Get_DiskNumberStart: Integer; safecall;
    function Get_ExtraField: WideString; safecall;
    procedure Set_ExtraField(const Value: WideString); safecall;
    function Get_FileComment: WideString; safecall;
    procedure Set_FileComment(const Value: WideString); safecall;
    function Get_InternalFileAttributes: Integer; safecall;
    procedure Set_InternalFileAttributes(Value: Integer); safecall;
    function Get_VersionMadeBy: Integer; safecall;
    function Get_VersionNeededToExtract: Integer; safecall;
    property Action: TArchiveAction read Get_Action;
    property CompressedSize: Integer read Get_CompressedSize;
    property CRC32: Integer read Get_CRC32;
    property DiskFileName: WideString read Get_DiskFileName;
    property DiskPath: WideString read Get_DiskPath;
    property ExternalFileAttributes: TFileAttributes read Get_ExternalFileAttributes write Set_ExternalFileAttributes;
    property FileName: WideString read Get_FileName write Set_FileName;
    property IsEncrypted: WordBool read Get_IsEncrypted;
    property LastModFileDateTime: TDateTime read Get_LastModFileDateTime;
    property StoredPath: WideString read Get_StoredPath;
    property Tagged: WordBool read Get_Tagged write Set_Tagged;
    property UnCompressedSize: Integer read Get_UnCompressedSize;
    property CRC32St: WideString read Get_CRC32St;
    property Password: WideString read Get_Password write Set_Password;
    property CompressionMethod: TZipCompressionMethod read Get_CompressionMethod;
    property CompressionRatio: Double read Get_CompressionRatio;
    property DeflateOption: TZipDeflateOption read Get_DeflateOption;
    property DictionarySize: TZipDictionarySize read Get_DictionarySize;
    property DiskNumberStart: Integer read Get_DiskNumberStart;
    property ExtraField: WideString read Get_ExtraField write Set_ExtraField;
    property FileComment: WideString read Get_FileComment write Set_FileComment;
    property InternalFileAttributes: Integer read Get_InternalFileAttributes write Set_InternalFileAttributes;
    property VersionMadeBy: Integer read Get_VersionMadeBy;
    property VersionNeededToExtract: Integer read Get_VersionNeededToExtract;
  end;

// *********************************************************************//
// DispIntf:  IZipItemDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {851699A1-422A-4C65-8E08-D0499ACDD834}
// *********************************************************************//
  IZipItemDisp = dispinterface
    ['{851699A1-422A-4C65-8E08-D0499ACDD834}']
    property Action: TArchiveAction readonly dispid 1;
    property CompressedSize: Integer readonly dispid 2;
    property CRC32: Integer readonly dispid 3;
    property DiskFileName: WideString readonly dispid 4;
    property DiskPath: WideString readonly dispid 5;
    property ExternalFileAttributes: TFileAttributes dispid 6;
    property FileName: WideString dispid 7;
    property IsEncrypted: WordBool readonly dispid 8;
    property LastModFileDateTime: TDateTime readonly dispid 9;
    property StoredPath: WideString readonly dispid 10;
    property Tagged: WordBool dispid 11;
    property UnCompressedSize: Integer readonly dispid 12;
    property CRC32St: WideString readonly dispid 13;
    property Password: WideString dispid 14;
    property CompressionMethod: TZipCompressionMethod readonly dispid 15;
    property CompressionRatio: Double readonly dispid 16;
    property DeflateOption: TZipDeflateOption readonly dispid 17;
    property DictionarySize: TZipDictionarySize readonly dispid 18;
    property DiskNumberStart: Integer readonly dispid 19;
    property ExtraField: WideString dispid 20;
    property FileComment: WideString dispid 21;
    property InternalFileAttributes: Integer dispid 22;
    property VersionMadeBy: Integer readonly dispid 23;
    property VersionNeededToExtract: Integer readonly dispid 24;
  end;

// *********************************************************************//
// Interface: IGZipItem
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8FA78CE0-FD29-441E-9777-93B63EF1A9EE}
// *********************************************************************//
  IGZipItem = interface(IDispatch)
    ['{8FA78CE0-FD29-441E-9777-93B63EF1A9EE}']
    function Get_Action: TArchiveAction; safecall;
    function Get_CompressedSize: Integer; safecall;
    function Get_CRC32: Integer; safecall;
    function Get_DiskFileName: WideString; safecall;
    function Get_DiskPath: WideString; safecall;
    function Get_ExternalFileAttributes: TFileAttributes; safecall;
    procedure Set_ExternalFileAttributes(Value: TFileAttributes); safecall;
    function Get_FileName: WideString; safecall;
    procedure Set_FileName(const Value: WideString); safecall;
    function Get_IsEncrypted: WordBool; safecall;
    function Get_LastModFileDateTime: TDateTime; safecall;
    function Get_StoredPath: WideString; safecall;
    function Get_Tagged: WordBool; safecall;
    procedure Set_Tagged(Value: WordBool); safecall;
    function Get_UnCompressedSize: Integer; safecall;
    function Get_CRC32St: WideString; safecall;
    function Get_Password: WideString; safecall;
    procedure Set_Password(const Value: WideString); safecall;
    function Get_CompressionMethod: Byte; safecall;
    procedure Set_CompressionMethod(Value: Byte); safecall;
    function Get_ExtraField: WideString; safecall;
    procedure Set_ExtraField(const Value: WideString); safecall;
    function Get_ExtraFlags: Byte; safecall;
    procedure Set_ExtraFlags(Value: Byte); safecall;
    function Get_FileComment: WideString; safecall;
    procedure Set_FileComment(const Value: WideString); safecall;
    function Get_FileSystem: TFileSystem; safecall;
    procedure Set_FileSystem(Value: TFileSystem); safecall;
    function Get_Flags: Byte; safecall;
    procedure Set_Flags(Value: Byte); safecall;
    function Get_HeaderCRC: Integer; safecall;
    property Action: TArchiveAction read Get_Action;
    property CompressedSize: Integer read Get_CompressedSize;
    property CRC32: Integer read Get_CRC32;
    property DiskFileName: WideString read Get_DiskFileName;
    property DiskPath: WideString read Get_DiskPath;
    property ExternalFileAttributes: TFileAttributes read Get_ExternalFileAttributes write Set_ExternalFileAttributes;
    property FileName: WideString read Get_FileName write Set_FileName;
    property IsEncrypted: WordBool read Get_IsEncrypted;
    property LastModFileDateTime: TDateTime read Get_LastModFileDateTime;
    property StoredPath: WideString read Get_StoredPath;
    property Tagged: WordBool read Get_Tagged write Set_Tagged;
    property UnCompressedSize: Integer read Get_UnCompressedSize;
    property CRC32St: WideString read Get_CRC32St;
    property Password: WideString read Get_Password write Set_Password;
    property CompressionMethod: Byte read Get_CompressionMethod write Set_CompressionMethod;
    property ExtraField: WideString read Get_ExtraField write Set_ExtraField;
    property ExtraFlags: Byte read Get_ExtraFlags write Set_ExtraFlags;
    property FileComment: WideString read Get_FileComment write Set_FileComment;
    property FileSystem: TFileSystem read Get_FileSystem write Set_FileSystem;
    property Flags: Byte read Get_Flags write Set_Flags;
    property HeaderCRC: Integer read Get_HeaderCRC;
  end;

// *********************************************************************//
// DispIntf:  IGZipItemDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8FA78CE0-FD29-441E-9777-93B63EF1A9EE}
// *********************************************************************//
  IGZipItemDisp = dispinterface
    ['{8FA78CE0-FD29-441E-9777-93B63EF1A9EE}']
    property Action: TArchiveAction readonly dispid 1;
    property CompressedSize: Integer readonly dispid 2;
    property CRC32: Integer readonly dispid 3;
    property DiskFileName: WideString readonly dispid 4;
    property DiskPath: WideString readonly dispid 5;
    property ExternalFileAttributes: TFileAttributes dispid 6;
    property FileName: WideString dispid 7;
    property IsEncrypted: WordBool readonly dispid 8;
    property LastModFileDateTime: TDateTime readonly dispid 9;
    property StoredPath: WideString readonly dispid 10;
    property Tagged: WordBool dispid 11;
    property UnCompressedSize: Integer readonly dispid 12;
    property CRC32St: WideString readonly dispid 13;
    property Password: WideString dispid 14;
    property CompressionMethod: Byte dispid 15;
    property ExtraField: WideString dispid 16;
    property ExtraFlags: Byte dispid 17;
    property FileComment: WideString dispid 18;
    property FileSystem: TFileSystem dispid 19;
    property Flags: Byte dispid 20;
    property HeaderCRC: Integer readonly dispid 21;
  end;

// *********************************************************************//
// Interface: ITarItem
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {729E9F52-C489-4A41-A770-4E2C5282AE39}
// *********************************************************************//
  ITarItem = interface(IDispatch)
    ['{729E9F52-C489-4A41-A770-4E2C5282AE39}']
    function Get_Action: TArchiveAction; safecall;
    function Get_CompressedSize: Integer; safecall;
    function Get_CRC32: Integer; safecall;
    function Get_DiskFileName: WideString; safecall;
    function Get_DiskPath: WideString; safecall;
    function Get_ExternalFileAttributes: TFileAttributes; safecall;
    procedure Set_ExternalFileAttributes(Value: TFileAttributes); safecall;
    function Get_FileName: WideString; safecall;
    procedure Set_FileName(const Value: WideString); safecall;
    function Get_IsEncrypted: WordBool; safecall;
    function Get_LastModFileDateTime: TDateTime; safecall;
    function Get_StoredPath: WideString; safecall;
    function Get_Tagged: WordBool; safecall;
    procedure Set_Tagged(Value: WordBool); safecall;
    function Get_UnCompressedSize: Integer; safecall;
    function Get_CRC32St: WideString; safecall;
    function Get_Password: WideString; safecall;
    procedure Set_Password(const Value: WideString); safecall;
    function Get_DevMajor: Integer; safecall;
    procedure Set_DevMajor(Value: Integer); safecall;
    function Get_DevMinor: Integer; safecall;
    procedure Set_DevMinor(Value: Integer); safecall;
    function Get_GroupID: Integer; safecall;
    procedure Set_GroupID(Value: Integer); safecall;
    function Get_GroupName: WideString; safecall;
    procedure Set_GroupName(const Value: WideString); safecall;
    function Get_LinkFlag: Byte; safecall;
    procedure Set_LinkFlag(Value: Byte); safecall;
    function Get_LinkName: WideString; safecall;
    procedure Set_LinkName(const Value: WideString); safecall;
    function Get_Mode: Integer; safecall;
    procedure Set_Mode(Value: Integer); safecall;
    function Get_UserID: Integer; safecall;
    procedure Set_UserID(Value: Integer); safecall;
    function Get_UserName: WideString; safecall;
    procedure Set_UserName(const Value: WideString); safecall;
    property Action: TArchiveAction read Get_Action;
    property CompressedSize: Integer read Get_CompressedSize;
    property CRC32: Integer read Get_CRC32;
    property DiskFileName: WideString read Get_DiskFileName;
    property DiskPath: WideString read Get_DiskPath;
    property ExternalFileAttributes: TFileAttributes read Get_ExternalFileAttributes write Set_ExternalFileAttributes;
    property FileName: WideString read Get_FileName write Set_FileName;
    property IsEncrypted: WordBool read Get_IsEncrypted;
    property LastModFileDateTime: TDateTime read Get_LastModFileDateTime;
    property StoredPath: WideString read Get_StoredPath;
    property Tagged: WordBool read Get_Tagged write Set_Tagged;
    property UnCompressedSize: Integer read Get_UnCompressedSize;
    property CRC32St: WideString read Get_CRC32St;
    property Password: WideString read Get_Password write Set_Password;
    property DevMajor: Integer read Get_DevMajor write Set_DevMajor;
    property DevMinor: Integer read Get_DevMinor write Set_DevMinor;
    property GroupID: Integer read Get_GroupID write Set_GroupID;
    property GroupName: WideString read Get_GroupName write Set_GroupName;
    property LinkFlag: Byte read Get_LinkFlag write Set_LinkFlag;
    property LinkName: WideString read Get_LinkName write Set_LinkName;
    property Mode: Integer read Get_Mode write Set_Mode;
    property UserID: Integer read Get_UserID write Set_UserID;
    property UserName: WideString read Get_UserName write Set_UserName;
  end;

// *********************************************************************//
// DispIntf:  ITarItemDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {729E9F52-C489-4A41-A770-4E2C5282AE39}
// *********************************************************************//
  ITarItemDisp = dispinterface
    ['{729E9F52-C489-4A41-A770-4E2C5282AE39}']
    property Action: TArchiveAction readonly dispid 1;
    property CompressedSize: Integer readonly dispid 2;
    property CRC32: Integer readonly dispid 3;
    property DiskFileName: WideString readonly dispid 4;
    property DiskPath: WideString readonly dispid 5;
    property ExternalFileAttributes: TFileAttributes dispid 6;
    property FileName: WideString dispid 7;
    property IsEncrypted: WordBool readonly dispid 8;
    property LastModFileDateTime: TDateTime readonly dispid 9;
    property StoredPath: WideString readonly dispid 10;
    property Tagged: WordBool dispid 11;
    property UnCompressedSize: Integer readonly dispid 12;
    property CRC32St: WideString readonly dispid 13;
    property Password: WideString dispid 14;
    property DevMajor: Integer dispid 15;
    property DevMinor: Integer dispid 16;
    property GroupID: Integer dispid 17;
    property GroupName: WideString dispid 18;
    property LinkFlag: Byte dispid 19;
    property LinkName: WideString dispid 20;
    property Mode: Integer dispid 21;
    property UserID: Integer dispid 22;
    property UserName: WideString dispid 23;
  end;

// *********************************************************************//
// Interface: IZipKit
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B7480A7F-4E27-4B45-9FE6-224B60295A0C}
// *********************************************************************//
  IZipKit = interface(IDispatch)
    ['{B7480A7F-4E27-4B45-9FE6-224B60295A0C}']
    procedure Add(const FileMask: WideString; const ExclusionMask: WideString; SearchAttr: Integer); safecall;
    procedure AddFromStream(const FileName: WideString; Stream: OleVariant); safecall;
    function Get_AutoSave: WordBool; safecall;
    procedure Set_AutoSave(Value: WordBool); safecall;
    function Get_BaseDirectory: WideString; safecall;
    procedure Set_BaseDirectory(const Value: WideString); safecall;
    procedure ClearTags; safecall;
    function Get_CompressionMethodToUse: TZipSupportMethod; safecall;
    procedure Set_CompressionMethodToUse(Value: TZipSupportMethod); safecall;
    function Get_Count: Integer; safecall;
    function Get_DeflateOption: TZipDeflateOption; safecall;
    procedure Set_DeflateOption(Value: TZipDeflateOption); safecall;
    procedure Delete(const FileMask: WideString; const ExclusionMask: WideString); safecall;
    procedure DeleteAt(Index: Integer); safecall;
    procedure DeleteTaggedItems; safecall;
    function Get_DOSMode: WordBool; safecall;
    procedure Set_DOSMode(Value: WordBool); safecall;
    procedure Extract(const FileMask: WideString; const ExclusionMask: WideString); safecall;
    procedure ExtractAt(Index: Integer; const NewName: WideString); safecall;
    function Get_ExtractOptions: TZipExtractOptions; safecall;
    procedure Set_ExtractOptions(Value: TZipExtractOptions); safecall;
    procedure ExtractTaggedItems; safecall;
    function Get_FileName: WideString; safecall;
    procedure Set_FileName(const Value: WideString); safecall;
    function Find(const FileName: WideString): Integer; safecall;
    procedure Freshen(const FileMask: WideString; const ExclusionMask: WideString); safecall;
    procedure FreshenTaggedItems; safecall;
    function Get_Item(Index: Integer): IDispatch; safecall;
    function Get_LogFile: WideString; safecall;
    procedure Set_LogFile(const Value: WideString); safecall;
    function Get_Logging: WordBool; safecall;
    procedure Set_Logging(Value: WordBool); safecall;
    function Get_Password: WideString; safecall;
    procedure Set_Password(const Value: WideString); safecall;
    function Get_PasswordRetries: Byte; safecall;
    procedure Set_PasswordRetries(Value: Byte); safecall;
    procedure Replace(const FileMask: WideString); safecall;
    procedure Save; safecall;
    function Get_Spanned: WordBool; safecall;
    function Get_SpanningThreshold: Integer; safecall;
    procedure Set_SpanningThreshold(Value: Integer); safecall;
    function Get_Status: TArchiveStatus; safecall;
    function Get_StoreOptions: TStoreOptions; safecall;
    procedure Set_StoreOptions(Value: TStoreOptions); safecall;
    procedure TagItems(const FileMask: WideString); safecall;
    function Get_TempDirectory: WideString; safecall;
    procedure Set_TempDirectory(const Value: WideString); safecall;
    procedure TestTaggedItems; safecall;
    procedure UntagItems(const FileMask: WideString); safecall;
    function Get_ZipFileComment: WideString; safecall;
    procedure Set_ZipFileComment(const Value: WideString); safecall;
    function License(const Key: WideString): WordBool; safecall;
    function Get__NewEnum: IUnknown; safecall;
    function ExtractToStream(const FileName: WideString): OleVariant; safecall;
    function Get_CompressionType: TArchiveType; safecall;
    procedure Set_CompressionType(Value: TArchiveType); safecall;
    function Get_TarAutoHandle: WordBool; safecall;
    procedure Set_TarAutoHandle(Value: WordBool); safecall;
    property AutoSave: WordBool read Get_AutoSave write Set_AutoSave;
    property BaseDirectory: WideString read Get_BaseDirectory write Set_BaseDirectory;
    property CompressionMethodToUse: TZipSupportMethod read Get_CompressionMethodToUse write Set_CompressionMethodToUse;
    property Count: Integer read Get_Count;
    property DeflateOption: TZipDeflateOption read Get_DeflateOption write Set_DeflateOption;
    property DOSMode: WordBool read Get_DOSMode write Set_DOSMode;
    property ExtractOptions: TZipExtractOptions read Get_ExtractOptions write Set_ExtractOptions;
    property FileName: WideString read Get_FileName write Set_FileName;
    property Item[Index: Integer]: IDispatch read Get_Item;
    property LogFile: WideString read Get_LogFile write Set_LogFile;
    property Logging: WordBool read Get_Logging write Set_Logging;
    property Password: WideString read Get_Password write Set_Password;
    property PasswordRetries: Byte read Get_PasswordRetries write Set_PasswordRetries;
    property Spanned: WordBool read Get_Spanned;
    property SpanningThreshold: Integer read Get_SpanningThreshold write Set_SpanningThreshold;
    property Status: TArchiveStatus read Get_Status;
    property StoreOptions: TStoreOptions read Get_StoreOptions write Set_StoreOptions;
    property TempDirectory: WideString read Get_TempDirectory write Set_TempDirectory;
    property ZipFileComment: WideString read Get_ZipFileComment write Set_ZipFileComment;
    property _NewEnum: IUnknown read Get__NewEnum;
    property CompressionType: TArchiveType read Get_CompressionType write Set_CompressionType;
    property TarAutoHandle: WordBool read Get_TarAutoHandle write Set_TarAutoHandle;
  end;

// *********************************************************************//
// DispIntf:  IZipKitDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B7480A7F-4E27-4B45-9FE6-224B60295A0C}
// *********************************************************************//
  IZipKitDisp = dispinterface
    ['{B7480A7F-4E27-4B45-9FE6-224B60295A0C}']
    procedure Add(const FileMask: WideString; const ExclusionMask: WideString; SearchAttr: Integer); dispid 1;
    procedure AddFromStream(const FileName: WideString; Stream: OleVariant); dispid 7;
    property AutoSave: WordBool dispid 3;
    property BaseDirectory: WideString dispid 4;
    procedure ClearTags; dispid 5;
    property CompressionMethodToUse: TZipSupportMethod dispid 6;
    property Count: Integer readonly dispid 2;
    property DeflateOption: TZipDeflateOption dispid 8;
    procedure Delete(const FileMask: WideString; const ExclusionMask: WideString); dispid 9;
    procedure DeleteAt(Index: Integer); dispid 10;
    procedure DeleteTaggedItems; dispid 11;
    property DOSMode: WordBool dispid 12;
    procedure Extract(const FileMask: WideString; const ExclusionMask: WideString); dispid 13;
    procedure ExtractAt(Index: Integer; const NewName: WideString); dispid 14;
    property ExtractOptions: TZipExtractOptions dispid 15;
    procedure ExtractTaggedItems; dispid 16;
    property FileName: WideString dispid 17;
    function Find(const FileName: WideString): Integer; dispid 18;
    procedure Freshen(const FileMask: WideString; const ExclusionMask: WideString); dispid 19;
    procedure FreshenTaggedItems; dispid 20;
    property Item[Index: Integer]: IDispatch readonly dispid 0;
    property LogFile: WideString dispid 23;
    property Logging: WordBool dispid 24;
    property Password: WideString dispid 25;
    property PasswordRetries: Byte dispid 26;
    procedure Replace(const FileMask: WideString); dispid 27;
    procedure Save; dispid 28;
    property Spanned: WordBool readonly dispid 29;
    property SpanningThreshold: Integer dispid 30;
    property Status: TArchiveStatus readonly dispid 31;
    property StoreOptions: TStoreOptions dispid 32;
    procedure TagItems(const FileMask: WideString); dispid 33;
    property TempDirectory: WideString dispid 34;
    procedure TestTaggedItems; dispid 35;
    procedure UntagItems(const FileMask: WideString); dispid 36;
    property ZipFileComment: WideString dispid 37;
    function License(const Key: WideString): WordBool; dispid 38;
    property _NewEnum: IUnknown readonly dispid $FFFFFFFC;
    function ExtractToStream(const FileName: WideString): OleVariant; dispid 21;
    property CompressionType: TArchiveType dispid 40;
    property TarAutoHandle: WordBool dispid 41;
  end;

// *********************************************************************//
// DispIntf:  IZipKitEvents
// Flags:     (4096) Dispatchable
// GUID:      {F094D5F4-3A52-45AE-9D86-4409611DD29E}
// *********************************************************************//
  IZipKitEvents = dispinterface
    ['{F094D5F4-3A52-45AE-9D86-4409611DD29E}']
    procedure OnArchiveItemProgress(const Item: IDispatch; Progress: Byte; var Abort: WordBool); dispid 1;
    procedure OnArchiveProgress(Progress: Byte; var Abort: WordBool); dispid 2;
    procedure OnChange; dispid 3;
    procedure OnConfirmOverwrite(var Name: WideString; var Confirm: WordBool); dispid 4;
    procedure OnConfirmProcessItem(const Item: IDispatch; ProcessType: TProcessType; 
                                   var Confirm: WordBool); dispid 5;
    procedure OnConfirmSave(var Confirm: WordBool); dispid 6;
    procedure OnLoad; dispid 7;
    procedure OnNeedPassword(var NewPassword: WideString); dispid 8;
    procedure OnProcessItemFailure(const Item: IDispatch; ProcessType: TProcessType; 
                                   ErrorClass: TErrorClass; ErrorCode: TErrorCode; 
                                   const ErrorString: WideString); dispid 9;
    procedure OnRequestBlankDisk(var Abort: WordBool); dispid 10;
    procedure OnRequestImage(ImageNumber: Integer; var ImageName: WideString; var Abort: WordBool); dispid 11;
    procedure OnRequestLastDisk(var Abort: WordBool); dispid 12;
    procedure OnRequestNthDisk(DiskNumber: Integer; var Abort: WordBool); dispid 13;
    procedure OnSave; dispid 14;
  end;

// *********************************************************************//
// The Class CoZipItem provides a Create and CreateRemote method to          
// create instances of the default interface IZipItem exposed by              
// the CoClass ZipItem. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoZipItem = class
    class function Create: IZipItem;
    class function CreateRemote(const MachineName: string): IZipItem;
  end;

// *********************************************************************//
// The Class CoGZipItem provides a Create and CreateRemote method to          
// create instances of the default interface IGZipItem exposed by              
// the CoClass GZipItem. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoGZipItem = class
    class function Create: IGZipItem;
    class function CreateRemote(const MachineName: string): IGZipItem;
  end;

// *********************************************************************//
// The Class CoTarItem provides a Create and CreateRemote method to          
// create instances of the default interface ITarItem exposed by              
// the CoClass TarItem. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoTarItem = class
    class function Create: ITarItem;
    class function CreateRemote(const MachineName: string): ITarItem;
  end;

// *********************************************************************//
// The Class CoZipKit provides a Create and CreateRemote method to          
// create instances of the default interface IZipKit exposed by              
// the CoClass ZipKit. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoZipKit = class
    class function Create: IZipKit;
    class function CreateRemote(const MachineName: string): IZipKit;
  end;

implementation

uses ComObj;

class function CoZipItem.Create: IZipItem;
begin
  Result := CreateComObject(CLASS_ZipItem) as IZipItem;
end;

class function CoZipItem.CreateRemote(const MachineName: string): IZipItem;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ZipItem) as IZipItem;
end;

class function CoGZipItem.Create: IGZipItem;
begin
  Result := CreateComObject(CLASS_GZipItem) as IGZipItem;
end;

class function CoGZipItem.CreateRemote(const MachineName: string): IGZipItem;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_GZipItem) as IGZipItem;
end;

class function CoTarItem.Create: ITarItem;
begin
  Result := CreateComObject(CLASS_TarItem) as ITarItem;
end;

class function CoTarItem.CreateRemote(const MachineName: string): ITarItem;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_TarItem) as ITarItem;
end;

class function CoZipKit.Create: IZipKit;
begin
  Result := CreateComObject(CLASS_ZipKit) as IZipKit;
end;

class function CoZipKit.CreateRemote(const MachineName: string): IZipKit;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ZipKit) as IZipKit;
end;

end.
