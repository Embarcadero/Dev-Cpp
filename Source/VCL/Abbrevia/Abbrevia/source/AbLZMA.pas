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
 * Pierre le Riche <pierre_le_riche@users.sourceforge.net>
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* ABBREVIA: AbLZMA.pas                                  *}
{*********************************************************}
{* ABBREVIA: Lzma compression/decompression procedures.  *}
{*********************************************************}

unit AbLZMA;

{$I AbDefine.inc}

interface

{$IFDEF MSWINDOWS}

uses
  Classes, Windows, SysUtils, AbCrtl, AbUtils;

{ Raw LZMA decompression =================================================== }

{ Decompresses the LZMA compressed data in ASrc to ADes.  ASrc should not have
  the header used by the other compression/decompression routines, and
  AProperties should contain any necessary data. }
procedure LzmaDecodeStream(AProperties: PByte; APropSize: Integer; ASrc, ADes: TStream;
  AUncompressedSize: Int64 = -1); overload;


{ Stream compression and decompression (taken from LzmaUtil.c) ============= }

procedure LzmaDecodeStream(ASourceStream, ATargetStream: TStream); overload;
procedure LzmaEncodeStream(ASourceStream, ATargetStream: TStream; ASourceSize: Int64);


{ In-memory compression and decompression ================================== }

{ Given a pointer to the compressed data, this will return the size of the
  decompressed data. }
function LzmaGetUncompressedSize(APCompressedData: Pointer; ACompressedSize: Integer): Integer;

{ Decompresses the LZMA compressed data at APCompressedData to the buffer
  pointed to by APUncompressedData.  The buffer at APUncompressedData should be
  large enough to hold the number of bytes as returned by LzmaGetDecompressedSize. }
procedure LzmaDecodeBuffer(APCompressedData: Pointer; ACompressedSize: Integer;
  APUncompressedData: Pointer);

{ Compresses the data at APUncompressedData to the buffer at APCompressedData,
  and returns the number of bytes written. If ACompressedDataBufferCapacity is
  less than the number of bytes required to store the entire compressed stream,
  or any other error occurs, then an exception is raised. (A safe number for
  ACompressedDataBufferCapacity is slightly more than AUncompressedDataBufferSize.)
  Leave ACompressionLevel and ADictionarySize at -1 in order to use the default
  values (5 and 16MB respectively). }
function LzmaEncodeBuffer(APUncompressedData: Pointer; AUncompressedSize: Integer;
  APCompressedData: Pointer; ACompressedDataBufferCapacity: Integer;
  ACompressionLevel: Integer = -1; ADictionarySize: Integer = -1): Integer;


{ Types.h declarations ===================================================== }

const
  SZ_OK = 0;
  SZ_ERROR_DATA = 1;
  SZ_ERROR_MEM = 2;
  SZ_ERROR_CRC = 3;
  SZ_ERROR_UNSUPPORTED = 4;
  SZ_ERROR_PARAM = 5;
  SZ_ERROR_INPUT_EOF = 6;
  SZ_ERROR_OUTPUT_EOF = 7;
  SZ_ERROR_READ = 8;
  SZ_ERROR_WRITE = 9;
  SZ_ERROR_PROGRESS = 10;
  SZ_ERROR_FAIL = 11;
  SZ_ERROR_THREAD = 12;
  SZ_ERROR_ARCHIVE = 16;
  SZ_ERROR_NO_ARCHIVE = 17;

type
  SRes = Integer;

  ISeqInStream = packed record
    Read: function(p: Pointer; var buf; var size: size_t): SRes; cdecl;
  end;
  PISeqInStream = ^ISeqInStream;

  ISeqOutStream = packed record
    Write: function(p: Pointer; const buf; size: size_t): size_t; cdecl;
  end;
  PISeqOutStream = ^ISeqOutStream;

  ICompressProgress = packed record
    Progress: function(p: Pointer; inSize, outSize: Int64): SRes; cdecl;
  end;
  PICompressProgress = ^ICompressProgress;

  ISzAlloc = packed record
    Alloc: function(p: Pointer; size: size_t): Pointer; cdecl;
    Free: procedure(p: Pointer; address: Pointer); cdecl;
  end;
  PISzAlloc = ^ISzAlloc;


{ LzmaDec.h declarations =================================================== }

type
  CLzmaProb = Word;

// LZMA Properties
const
  LZMA_PROPS_SIZE = 5;

type
  CLzmaProps = packed record
    lc, lp, pb: Cardinal;
    dicSize: UInt32;
  end;

// LZMA Decoder state
const
  LZMA_REQUIRED_INPUT_MAX = 20;

type
  CLzmaDec = packed record
    prop: CLzmaProps;
    probs: ^CLzmaProb;
    dic: PByte;
    buf: PByte;
    range, code: UInt32;
    dicPos: size_t;
    dicBufSize: size_t;
    processedPos: UInt32;
    checkDicSize: UInt32;
    state: Cardinal;
    reps: array[0..3] of UInt32;
    remainLen: Cardinal;
    needFlush: Integer;
    needInitState: Integer;
    numProbs: UInt32;
    tempBufSize: Cardinal;
    tempBuf: array[0..LZMA_REQUIRED_INPUT_MAX - 1] of Byte;
  end;

type
  ELzmaFinishMode = Integer;

const
  LZMA_FINISH_ANY = 0; // finish at any point
  LZMA_FINISH_END = 1; // block must be finished at the end

type
  ELzmaStatus = Integer;

const
  LZMA_STATUS_NOT_SPECIFIED = 0;               // use main error code instead
  LZMA_STATUS_FINISHED_WITH_MARK = 1;          // stream was finished with end mark.
  LZMA_STATUS_NOT_FINISHED = 3;                // stream was not finished
  LZMA_STATUS_NEEDS_MORE_INPUT = 4;            // you must provide more input bytes
  LZMA_STATUS_MAYBE_FINISHED_WITHOUT_MARK = 5; // there is probability that stream was finished without end mark

procedure LzmaDec_Construct(var p: CLzmaDec); cdecl;
procedure LzmaDec_Init(var p: CLzmaDec); cdecl; external;
function LzmaDec_DecodeToBuf(var p: CLzmaDec; dest: PByte; var destLen: size_t;
  src: PByte; var srcLen: size_t; finishMode: ELzmaFinishMode;
  var status: ELzmaStatus): SRes; cdecl; external;
function LzmaDec_Allocate(var state: CLzmaDec; prop: PByte; propsSize: Integer;
  alloc: PISzAlloc): SRes; cdecl; external;
procedure LzmaDec_Free(var state: CLzmaDec; alloc: PISzAlloc); cdecl; external;

// One call decoding interface
function LzmaDecode(dest: PByte; var destLen: size_t; src: PByte;
  var srcLen: size_t; propData: PByte; propSize: Integer;
  finishMode: ELzmaFinishMode; var status: ELzmaStatus; 
  alloc: PISzAlloc): SRes; cdecl; external;


{ LzmaEnc.h declarations =================================================== }

type
  CLzmaEncHandle = Pointer;

  CLzmaEncProps = packed record
    level: Integer;         // 0 <= level <= 9
    dictSize: UInt32;       // (1 << 12) <= dictSize <= (1 << 27) for 32-bit version
                            // (1 << 12) <= dictSize <= (1 << 30) for 64-bit version
                            // default = (1 << 24)
    lc: Integer;            // 0 <= lc <= 8, default = 3
    lp: Integer;            // 0 <= lp <= 4, default = 0
    pb: Integer;            // 0 <= pb <= 4, default = 2
    algo: Integer;          // 0 - fast, 1 - normal, default = 1
    fb: Integer;            // 5 <= fb <= 273, default = 32
    btMode: Integer;        // 0 - hashChain Mode, 1 - binTree mode - normal, default = 1
    numHashBytes: Integer;  // 2, 3 or 4, default = 4
    mc: UInt32;             // 1 <= mc <= (1 << 30), default = 32
    writeEndMark: Cardinal; // 0 - do not write EOPM, 1 - write EOPM, default = 0
    numThreads: Integer;    // 1 or 2, default = 2
  end;

procedure LzmaEncProps_Init(var p: CLzmaEncProps); cdecl; external;
function LzmaEnc_Create(Alloc: PISzAlloc): CLzmaEncHandle; cdecl; external;
procedure LzmaEnc_Destroy(p: CLzmaEncHandle; Alloc, allocBig: PISzAlloc); cdecl; external;
function LzmaEnc_SetProps(p: CLzmaEncHandle; var props: CLzmaEncProps): SRes; cdecl; external;
function LzmaEnc_WriteProperties(p: CLzmaEncHandle; properties: PByte;
  var size: size_t): SRes; cdecl; external;
function LzmaEnc_Encode(p: CLzmaEncHandle; outStream: PISeqOutStream;
  inStream: PISeqInStream; Progress: PICompressProgress;
  Alloc, allocBig: PISzAlloc): SRes; cdecl; external;
function LzmaEnc_MemEncode(p: CLzmaEncHandle; dest: PByte; var destLen: size_t;
  src: PByte; srcLen: size_t; writeEndMark: Integer; Progress: PICompressProgress;
  Alloc, allocBig: PISzAlloc): SRes; cdecl; external;

// One call encoding interface
function LzmaEncode(dest: PByte; var destLen: size_t; src: PByte;
  srcLen: size_t; var props: CLzmaEncProps; propsEncoded: PByte;
  var propsSize: size_t; writeEndMark: Integer; progress: PICompressProgress;
  alloc: pISzAlloc; allocBig: PISzAlloc): SRes; cdecl; external;


{ LzFind.h declarations ==================================================== }

procedure MatchFinder_NeedMove; external;
procedure MatchFinder_GetPointerToCurrentPos; external;
procedure MatchFinder_MoveBlock; external;
procedure MatchFinder_ReadIfRequired; external;
procedure MatchFinder_Construct; external;
procedure MatchFinder_Create; external;
procedure MatchFinder_Free; external;
procedure MatchFinder_Normalize3; external;
procedure MatchFinder_ReduceOffsets; external;
procedure GetMatchesSpec1; external;
procedure MatchFinder_Init; external;
procedure MatchFinder_CreateVTable; external;


{ LzFindMt.h declarations ================================================== }

procedure MatchFinderMt_Construct; external;
procedure MatchFinderMt_Destruct; external;
procedure MatchFinderMt_Create; external;
procedure MatchFinderMt_CreateVTable; external;
procedure MatchFinderMt_ReleaseStream; external;


{ Lzma header fields ======================================================= }

type
  // The condensed compression properties
  TLZMAPropertyData = array[0..LZMA_PROPS_SIZE - 1] of Byte;

  // The header usually stored in front of LZMA compressed data
  TLZMAHeader = packed record
    PropertyData: TLZMAPropertyData;
    UncompressedSize: Int64;
  end;
  PLZMAHeader = ^TLZMAHeader;


{ Error handling =========================================================== }

type
  EAbLZMAException = class(Exception);

procedure LzmaCheck(AResultCode: SRes);
procedure RaiseLzmaException(AResultCode: SRes);


{ Linker directives ======================================================== }

{$WARN BAD_GLOBAL_SYMBOL OFF}
{$IF DEFINED(WIN32)}
  {$L Win32\LzFind.obj}
  {$L Win32\LzFindMt.obj}
  {$L Win32\LzmaDec.obj}
  {$L Win32\LzmaEnc.obj}
  {$L Win32\Threads.obj}
{$ELSEIF DEFINED(WIN64)}
  {$L Win64\LzFind.obj}
  {$L Win64\LzFindMt.obj}
  {$L Win64\LzmaDec.obj}
  {$L Win64\LzmaEnc.obj}
  {$L Win64\Threads.obj}
{$IFEND}

{$ENDIF}

implementation

{$IFDEF MSWINDOWS}

{ Error handling =========================================================== }

procedure LzmaCheck(AResultCode: SRes);
begin
  if AResultCode <> SZ_OK then
    RaiseLzmaException(AResultCode);
end;
{ -------------------------------------------------------------------------- }
procedure RaiseLzmaException(AResultCode: SRes);
begin
  case AResultCode of
    SZ_ERROR_DATA: raise EAbLZMAException.Create('LZMA Data Error.');
    SZ_ERROR_MEM: raise EAbLZMAException.Create('LZMA Memory Error.');
    SZ_ERROR_CRC: raise EAbLZMAException.Create('LZMA CRC Error.');
    SZ_ERROR_UNSUPPORTED: raise EAbLZMAException.Create('LZMA "Unsupported" Error.');
    SZ_ERROR_PARAM: raise EAbLZMAException.Create('LZMA Parameter Error.');
    SZ_ERROR_INPUT_EOF: raise EAbLZMAException.Create('LZMA Input EOF Error.');
    SZ_ERROR_OUTPUT_EOF: raise EAbLZMAException.Create('LZMA Output EOF Error.');
    SZ_ERROR_READ: raise EAbLZMAException.Create('LZMA Read Error.');
    SZ_ERROR_WRITE: raise EAbLZMAException.Create('LZMA Write Error.');
    SZ_ERROR_PROGRESS: raise EAbLZMAException.Create('LZMA Progress Error.');
    SZ_ERROR_FAIL: raise EAbLZMAException.Create('LZMA "Fail" Error.');
    SZ_ERROR_THREAD: raise EAbLZMAException.Create('LZMA Thread Error.');
    SZ_ERROR_ARCHIVE: raise EAbLZMAException.Create('LZMA Archive Error.');
    SZ_ERROR_NO_ARCHIVE: raise EAbLZMAException.Create('LZMA "No Archive" Error.');
  else
    raise EAbLZMAException.CreateFmt('Unknown LZMA error (%d)', [AResultCode]);
  end;
end;


{ Helper Routines ========================================================== }

procedure LzmaDec_Construct(var p: CLzmaDec); cdecl;
begin
  p.dic := nil;
  p.probs := nil;
end;
{ -------------------------------------------------------------------------- }
function SzAlloc(p: Pointer; size: size_t): Pointer; cdecl;
begin
  Result := GetMemory(size);
end;
{ -------------------------------------------------------------------------- }
procedure SzFree(p, address: Pointer); cdecl;
begin
  FreeMemory(address);
end;

var
  DelphiMMInterface: ISzAlloc = (Alloc: SzAlloc; Free: SzFree);


{ CSeq*Stream implementation =============================================== }

type
  CSeqInStream = packed record
    Intf: ISeqInStream;
    Stream: TStream;
  end;

  CSeqOutStream = packed record
    Intf: ISeqOutStream;
    Stream: TStream;
  end;
{ -------------------------------------------------------------------------- }
function ISeqInStream_Read(p: Pointer; var buf; var size: size_t): SRes; cdecl;
begin
  try
    size := CSeqInStream(p^).Stream.Read(buf, size);
    Result := SZ_OK;
  except
    Result := SZ_ERROR_DATA;
  end;
end;
{ -------------------------------------------------------------------------- }
function ISeqOutStream_Write(p: Pointer; const buf; size: size_t): size_t; cdecl;
begin
  try
    Result := CSeqOutStream(p^).Stream.Write(buf, size);
  except
    Result := 0;
  end;
end;


{ Raw LZMA decompression =================================================== }

{ Decompress an Lzma compressed stream. Based on LzmaUtil.c::Decode2 }
function LzmaDecode2(var aState: CLzmaDec; aOutStream, aInStream: TStream;
  aUncompressedSize: Int64 = -1): SRes;
const
  IN_BUF_SIZE = 1 shl 16;
  OUT_BUF_SIZE = 1 shl 16;
var
  LHasSize: Boolean;
  LInBuf: array [0..IN_BUF_SIZE - 1] of Byte;
  LOutBuf: array [0..OUT_BUF_SIZE - 1] of Byte;
  LInPos, LInSize, LOutPos: size_t;
  LInProcessed, LOutProcessed: size_t;
  LFinishMode: ELzmaFinishMode;
  LStatus: ELzmaStatus;
begin
  Result := 0;
  LHasSize := aUncompressedSize <> -1;
  LInPos := 0;
  LInSize := 0;
  LOutPos := 0;

  LzmaDec_Init(aState);
  while True do
  begin
    if LInPos = LInSize then
    begin
      LInSize := aInStream.Read(LInBuf, IN_BUF_SIZE);
      LInPos := 0;
      if LInSize = 0 then
        Break;
    end
    else
    begin
      LInProcessed := LInSize - LInPos;
      LOutProcessed := OUT_BUF_SIZE - LOutPos;
      LFinishMode := LZMA_FINISH_ANY;
      if LHasSize and (LOutProcessed > aUncompressedSize) then
      begin
        LOutProcessed := size_t(aUncompressedSize);
        LFinishMode := LZMA_FINISH_END;
      end;
      Result := LzmaDec_DecodeToBuf(aState, @LOutBuf[LOutPos], LOutProcessed,
        @LInBuf[LInPos], LInProcessed, LFinishMode, LStatus);
      Inc(LInPos, LInProcessed);
      Inc(LOutPos, LOutProcessed);
      Dec(aUncompressedSize, LOutProcessed);

      if (aOutStream <> nil) and (aOutStream.Write(LOutBuf, LOutPos) <> LOutPos) then
      begin
        Result := SZ_ERROR_WRITE;
        Exit;
      end;

      LOutPos := 0;

      if (Result <> SZ_OK) or (LHasSize and (aUncompressedSize = 0)) then
        Exit;

      if (LInProcessed = 0) and (LOutProcessed = 0) then
      begin
        if LHasSize or (LStatus <> LZMA_STATUS_FINISHED_WITH_MARK) then
          Result := SZ_ERROR_DATA;
        Exit;
      end;
    end;
  end;
end;
{ -------------------------------------------------------------------------- }
{ Decompress an LZMA compressed stream. Pass AUncompressedSize = -1 if the
  uncompressed size is not known. }
procedure LzmaDecodeStream(AProperties: PByte; APropSize: Integer;
  ASrc, ADes: TStream; AUncompressedSize: Int64);
var
  LLZMADecState: CLzmaDec;
begin
  LzmaDec_Construct(LLZMADecState);
  try
    LzmaCheck(LzmaDec_Allocate(LLZMADecState, AProperties, APropSize, @DelphiMMInterface));
    LzmaCheck(LzmaDecode2(LLZMADecState, ADes, ASrc, AUncompressedSize));
  finally
    LzmaDec_Free(LLZMADecState, @DelphiMMInterface);
  end;
end;


{ Stream to stream compression and decompression =========================== }

{ Decompresses streams compressed with the LZMA SDK's LzmaUtil.exe.
  Based on LzmaUtil.c::Decode }
procedure LzmaDecodeStream(ASourceStream, ATargetStream: TStream);
var
  LUncompressedSize: Int64;
  // Header: 5 bytes of LZMA properties and 8 bytes of uncompressed size
  LHeader: TLZMAHeader;
begin
  // Read and parse header
  ASourceStream.ReadBuffer(LHeader, SizeOf(LHeader));
  LUncompressedSize := LHeader.UncompressedSize;

  LzmaDecodeStream(PByte(@LHeader.PropertyData), LZMA_PROPS_SIZE, ASourceStream,
    ATargetStream, LUncompressedSize);
end;
{ -------------------------------------------------------------------------- }
{ Compresses a stream so it's compatible with the LZMA SDK's LzmaUtil.exe.
  Based on LzmaUtil.c::Encode }
procedure LzmaEncodeStream(ASourceStream, ATargetStream: TStream; ASourceSize: Int64);
var
  LEncHandle: CLzmaEncHandle;
  LEncProps: CLzmaEncProps;
  LHeader: TLZMAHeader;
  LPropDataSize: size_t;
  LInStreamRec: CSeqInStream;
  LOutStreamRec: CSeqOutStream;
begin
  LInStreamRec.Intf.Read := ISeqInStream_Read;
  LInStreamRec.Stream := ASourceStream;
  LOutStreamRec.Intf.Write := ISeqOutStream_Write;
  LOutStreamRec.Stream := ATargetStream;

  LEncHandle := LzmaEnc_Create(@DelphiMMInterface);
  if LEncHandle = nil then
    LzmaCheck(SZ_ERROR_MEM);

  try
    LzmaEncProps_Init(LEncProps);

    LzmaCheck(LzmaEnc_SetProps(LEncHandle, LEncProps));

    LPropDataSize := LZMA_PROPS_SIZE;

    LzmaCheck(LzmaEnc_WriteProperties(LEncHandle, PByte(@LHeader.PropertyData),
      LPropDataSize));

    LHeader.UncompressedSize := ASourceSize;

    ATargetStream.WriteBuffer(LHeader, SizeOf(LHeader));

    LzmaCheck(LzmaEnc_Encode(LEncHandle, @LOutStreamRec.Intf,
      @LInStreamRec.Intf, nil, @DelphiMMInterface, @DelphiMMInterface));

  finally
    LzmaEnc_Destroy(LEncHandle, @DelphiMMInterface, @DelphiMMInterface);
  end;
end;


{ In-memory compression and decompression ================================== }

{ Given a pointer to the compressed data, this will return the size of the
  decompressed data. }
function LzmaGetUncompressedSize(APCompressedData: Pointer; ACompressedSize: Integer): Integer;
begin
  if ACompressedSize <= SizeOf(TLZMAHeader) then
    raise EAbLZMAException.Create('The LZMA compressed data is invalid (not enough bytes)');

  Result := PLZMAHeader(APCompressedData).UncompressedSize;
end;
{ -------------------------------------------------------------------------- }
{ Decompresses the LZMA compressed data at APCompressedData to the buffer
  pointed to by APUncompressedData.  The buffer at APUncompressedData should be
  large enough to hold the number of bytes as returned by LzGetDecompressedSize. }
procedure LzmaDecodeBuffer(APCompressedData: Pointer; ACompressedSize: Integer;
  APUncompressedData: Pointer);
var
  LPropertyData: TLZMAPropertyData;
  LUncompressedSize: Int64;
  LInputByteCount, LOutputByteCount: size_t;
  LStatus: ELzmaStatus;
begin
  if ACompressedSize <= SizeOf(TLZMAHeader) then
    raise EAbLZMAException.Create('The LZMA compressed data is invalid (not enough bytes)');

  // Read the header from the compressed data.
  LPropertyData := PLZMAHeader(APCompressedData).PropertyData;
  LUncompressedSize := PLZMAHeader(APCompressedData).UncompressedSize;
  Inc(PAnsiChar(APCompressedData), SizeOf(TLZMAHeader));
  Dec(ACompressedSize, SizeOf(TLZMAHeader));

  // Decompress from the input to the output buffer. This will change the byte
  // count variables to the actual number of bytes consumed/written.
  LInputByteCount := ACompressedSize;
  LOutputByteCount := LUncompressedSize;
  LzmaCheck(LzmaDecode(APUncompressedData, LOutputByteCount,
    APCompressedData, LInputByteCount, PByte(@LPropertyData), LZMA_PROPS_SIZE,
    LZMA_FINISH_END, LStatus, @DelphiMMInterface));

  // Check that the input buffer was fully consumed and the output buffer was filled up.
  if (LOutputByteCount <> LUncompressedSize) or (LInputByteCount <> ACompressedSize) then
    raise EAbLZMAException.Create('LZMA decompression data error');
end;
{ -------------------------------------------------------------------------- }
{ Compresses the data at APUncompressedData to the buffer at APCompressedData,
  and returns the number of bytes written. If ACompressedDataBufferCapacity is
  less than the number of bytes required to store the entire compressed stream,
  or any other error occurs, then an exception is raised. (A safe number for
  ACompressedDataBufferCapacity is slightly more than AUncompressedDataBufferSize.) 
  Leave ACompressionLevel and ADictionarySize at -1 in order to use the default
  values (5 and 16MB respectively). }
function LzmaEncodeBuffer(APUncompressedData: Pointer; AUncompressedSize: Integer;
  APCompressedData: Pointer;
  ACompressedDataBufferCapacity, ACompressionLevel, ADictionarySize: Integer): Integer;
var
  LEncProps: CLzmaEncProps;
  LPropsSize: size_t;
  LPOutBuf: PByte;
  LOutputBytes: size_t;
begin
  if ACompressedDataBufferCapacity <= SizeOf(TLZMAHeader) then
    raise EAbLZMAException.Create('LZMA output buffer too small');

  // Set the uncompressed size in the header
  PLZMAHeader(APCompressedData).UncompressedSize := AUncompressedSize;

  // Set the properties
  LzmaEncProps_Init(LEncProps);
  if ACompressionLevel >= 0 then
    LEncProps.level := ACompressionLevel;
  if ADictionarySize >= 0 then
    LEncProps.dictSize := ADictionarySize;

  LPOutBuf := PByte(PtrUInt(APCompressedData) + SizeOf(TLZMAHeader));
  LOutputBytes := ACompressedDataBufferCapacity - SizeOf(TLZMAHeader);
  LPropsSize := LZMA_PROPS_SIZE;
  LzmaCheck(LzmaEncode(LPOutBuf, LOutputBytes, APUncompressedData,
    AUncompressedSize, LEncProps, APCompressedData, LPropsSize, 0, nil,
    @DelphiMMInterface, @DelphiMMInterface));

  Result := LOutputBytes + SizeOf(TLZMAHeader);
end;

initialization
  // The LZMA routines are multithreaded and use the Delphi memory manager.
  IsMultiThread := True;

{$ENDIF}

end.
