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
  * The Initial Developer of the Original Code is Pierre le Riche
  *
  * Portions created by the Initial Developer are Copyright (C) 2011
  * the Initial Developer. All Rights Reserved.
  *
  * Contributor(s):
  * Pierre le Riche <pierre_le_riche@users.sourceforge.net>
  * Craig Peterson <capeterson@users.sourceforge.net>
  *
  * ***** END LICENSE BLOCK *****


Usage:
  LZMA Compression:
  1) Create a TAbLZMACompressionStream, passing as parameter to the constructor
     the output stream where you want the compressed data stored.
  2) Write the data that you want to compress to the TAbLZMACompressionStream.
     Compression occurs in a background thread.
  3) (Optional) Notify the background compression thread that no more data will
     be written by calling NoMoreDataToCompress. Poll the IsBusy method to
     determine whether the background thread is still busy.
  4) Free the TAbLZMACompressionStream to finish up and release resources. The
     compressed data will now be available in the output stream.

  LZMA Decompression:
  1) Create a TAbLZMADecompressionStream, passing as parameter to the constructor
     the stream that contains the compressed data.
  2) Read the decompressed data from TAbLZMADecompressionStream.
  3) Free the TAbLZMADecompressionStream to finish up and release resources.

*)

unit AbLZMAStream;

{$I AbDefine.inc}

interface

{$IFDEF MSWINDOWS}

uses
  Windows, Classes, SysUtils, AbLZMA, AbUtils;

const
  {The size of the intermediate buffers for compressed and decompressed data.}
  CompressedDataBufferSize = 16 * 1024;
  UncompressedDataBufferSize = 32 * 1024;
  {When reading/writing very small blocks from/to a (de)compression stream an
   intermediate buffer is used to buffer the small IO operations in order to
   improve performance. Reads and writes larger than this size are unbuffered
   and handled by the (de)compression algorithm directly. This value must be
   smaller than the compressed and uncompressed data buffers.}
  MaximumBlockSizeForBufferedIO = 1024;

type

  {------------LZMA compression stream------------}

  TAbLZMACompressionStream = class;

  {The background compression thread.}
  TAbLZMACompressionThread = class(TThread)
  protected
    FCompressionStream: TAbLZMACompressionStream;
  public
    procedure Execute; override;
  end;

  {Buffers queued for compression by the background compression thread.}
  PAbQueuedBuffer = ^TAbQueuedBuffer;
  TAbQueuedBuffer = packed record
    PreviousBuffer, NextBuffer: PAbQueuedBuffer;
    DataSize: Integer;
    {Adds this buffer to the compression queue for the given compression stream.
     It is assumed that the compression stream has acquired the buffer critical
     section.}
    procedure QueueBuffer(ACompressionStream: TAbLZMACompressionStream);
    {Removes this buffer from the compression queue}
    procedure UnQueueBuffer;
    {Returns a pointer to the data the given offset into the buffer}
    function GetDataPointer(AOffset: Integer): Pointer;
  end;

  TAbLZMACompressionStream = class(TStream)
  protected
    FOutputStream: TStream;
    {The critical section used to control access to the buffers that are queued
     for compression. The main thread and the compression thread may not access
     the buffer queue at the same time.}
    FBufferCriticalSection: TRTLCriticalSection;
    {This semaphore is signalled by the main thread when it added a workload
     for the compression thread (usually when a buffer has been added to
     compress).}
    FPendingWorkSemaphore: THandle;
    {The LZMA compression handle}
    FLZMAEncHandle: CLzmaEncHandle;
    {The background thread used to perform the compression}
    FCompressionThread: TAbLZMACompressionThread;
    {The error code returned by the compression method. 0 = Success.}
    FCompressionErrorCode: Integer;
    {The intermediate compression buffer used to aggregate small writes. When
     NoMoreDataToCompress is called this buffer is freed, so no more data may
     be written.}
    FPIntermediateCompressionBuffer: PAbQueuedBuffer;
    FIntermediateCompressionBufferAvailableBytes: Integer;
    {The circular linked list of buffers that are queued for compression.}
    FQueuedData: TAbQueuedBuffer;
    {The number of bytes of buffer FQueuedData.NextBuffer that has already been
     submitted to the compressor.}
    FCurrentBufferBytesSubmitted: Integer;
    {The position in the output stream where the uncompressed size must be
     stored.}
    FOutputStreamHeaderSizeFieldPosition: Int64;
    {The total number of bytes written to the compression stream}
    FTotalBytesWritten: Int64;
    {Wakes up the compression thread by signalling the "pending work semaphore"}
    procedure WakeCompressionThread; inline;
  public
    constructor Create(AOutputStream: TStream; ACompressionLevel: Integer = 5;
      ADictionarySize: Integer = 65536);
    destructor Destroy; override;
    {Reading is not supported and will raise an exception.}
    function Read(var ABuffer; ACount: Integer): Integer; override;
    {Submits data to the compression queue.}
    function Write(const ABuffer; ACount: Integer): Integer; override;
    {Will raise an exception if an attempt is made to seek off the current
     position.}
    function Seek(AOffset: Integer; AOrigin: Word): Integer; override;
    function Seek(const AOffset: Int64; AOrigin: TSeekOrigin): Int64; override;
    {Signals the compression thread that no more data will be submitted.
     Calling write after NoMoreDataToCompress has been called will raise an
     exception.}
    procedure NoMoreDataToCompress;
    {Calls NoMoreDataToCompress and then waits for the background compression
     process to complete, returning the value of ErrorCode (0 = success).}
    function WaitForCompressionToFinish: Integer;
    {Returns True if the background thread is still busy compressing data. Will
     always return True until NoMoreDataToCompress is called.}
    function IsBusy: Boolean;
    {-------------Public properties---------------}
    {The error code returned by the compression method. 0 = Success.}
    property ErrorCode: Integer read FCompressionErrorCode;
  end;

  {------------LZMA decompression stream------------}

  TAbLZMADecompressionStream = class(TStream)
  protected
    FSourceStream: TStream;
    {The intermediate buffers for compressed and uncompressed data
     respectively.}
    FCompressedDataBuffer: array[0..CompressedDataBufferSize - 1] of Byte;
    FUncompressedDataBuffer: array[0..UncompressedDataBufferSize - 1] of Byte;
    {Read buffer control: Used to speed up frequent small reads via
     FUncompressedDataBuffer.}
    FReadBufferSize: Integer;
    FReadBufferAvailableBytes: Integer;
    {The current size and position into FCompressedDataBuffer}
    FCompressedDataBufferSize: Integer;
    FCompressedDataBufferPosition: Integer;
    {The uncompressed size according to the header.}
    FUncompressedSize: Int64;
    {The total number of bytes that have been decompressed.}
    FBytesDecompressed: Int64;
    {The LZMA decompression state}
    FLzmaState: CLzmaDec;
    {Decompresses data from the compressed source to the buffer pointed to by
     APBuffer. Returns the number of actual bytes stored (which may be less
     than the requested size if the end of the compressed stream was reached).}
    function InternalDecompressToBuffer(APBuffer: Pointer; ABufferSize: Integer): Integer;
    {---Property getters/setters---}
    function GetBytesRead: Int64;
    function GetSize: Int64; override;
  public
    constructor Create(ASourceStream: TStream);
    destructor Destroy; override;
    function Read(var ABuffer; ACount: Integer): Integer; override;
    {Writing to a decompression stream is not allowed}
    function Write(const ABuffer; ACount: Integer): Integer; override;
    {Will raise an exception if an attempt is made to seek off the current
     position.}
    function Seek(AOffset: Integer; AOrigin: Word): Integer; override;
    function Seek(const AOffset: Int64; AOrigin: TSeekOrigin): Int64; override;
    {---Public properties---}
    {The number of decompressed bytes read from the decompression stream.}
    property BytesRead: Int64 read GetBytesRead;
  end;

{$ENDIF}

implementation

{$IFDEF MSWINDOWS}

uses
  AbCrtl;

{------------Memory management-------------}

function SzAlloc(p: Pointer; size: size_t): Pointer; cdecl;
begin
  Result := GetMemory(size);
end;

procedure SzFree(p, address: Pointer); cdecl;
begin
  FreeMemory(address);
end;

var
  DelphiMMInterface: ISzAlloc = (Alloc: SzAlloc; Free: SzFree);

{------------Compression "interface"-------------}

type

  {The "interfaces" for the input and output streams}
  CSeqInStream_Compress = packed record
    Intf: ISeqInStream;
    CompressionStream: TAbLZMACompressionStream;
  end;

  CSeqOutStream_Compress = packed record
    Intf: ISeqOutStream;
    OutputStream: TStream;
  end;

function ISeqInStream_Compress_Read(p: Pointer; var buf; var size: size_t): SRes; cdecl;
var
  LDoNotWaitForMoreData: Boolean;
  LStream: TAbLZMACompressionStream;
  LPSourceBuf, LPTargetBuf: PAnsiChar;
  LTargetSpace, LSourceBytesAvail: Integer;
  LPCurBuf: PAbQueuedBuffer;
begin
  try
    LTargetSpace := size;
    LPTargetBuf := @buf;
    LStream := CSeqInStream_Compress(p^).CompressionStream;
    while True do
    begin
      {Copy any buffered data to the LZMA buffer, returning the number of bytes
       written}
      EnterCriticalSection(LStream.FBufferCriticalSection);
      try
        {If the write buffer has been freed that the main thread will not add
         any more buffers for compression.}
        LDoNotWaitForMoreData := LStream.FPIntermediateCompressionBuffer = nil;

        {Copy as much queued data to the LZMA compression buffer as we have (or
         will fit).}
        while True do
        begin
          LPCurBuf := LStream.FQueuedData.NextBuffer;
          {No buffers left? -> Break the loop}
          if LPCurBuf = @LStream.FQueuedData then
            Break;
          {Can this buffer be submitted in its entirety, or only a part?}
          LPSourceBuf := LPCurBuf.GetDataPointer(LStream.FCurrentBufferBytesSubmitted);
          LSourceBytesAvail := LPCurBuf.DataSize - LStream.FCurrentBufferBytesSubmitted;
          if LSourceBytesAvail > LTargetSpace then
          begin
            {Submit only part of the buffer}
            System.Move(LPSourceBuf^, LPTargetBuf^, LTargetSpace);
            Inc(LStream.FCurrentBufferBytesSubmitted, LTargetSpace);
            LTargetSpace := 0;
            Break;
          end
          else
          begin
            {Submit all the remaining bytes in the buffer and free it.}
            System.Move(LPSourceBuf^, LPTargetBuf^, LSourceBytesAvail);
            Inc(LPTargetBuf, LSourceBytesAvail);
            Dec(LTargetSpace, LSourceBytesAvail);
            LStream.FCurrentBufferBytesSubmitted := 0;
            LPCurBuf.UnQueueBuffer;
            FreeMem(LPCurBuf);
          end;
        end;
      finally
        LeaveCriticalSection(LStream.FBufferCriticalSection);
      end;

      {If data was submitted to the compressor, or the main thread indicated
       that compression is complete then the loop is broken.}
      if (LTargetSpace <> size) or LDoNotWaitForMoreData then
        Break;
      {No data currently queued, but there may still be more coming: Wait for
       the main thread to notify this thread that more work is pending.}
      WaitForSingleObject(LStream.FPendingWorkSemaphore, INFINITE);
    end;
    {Update the number of bytes written}
    Dec(size, LTargetSpace);
    Result := SZ_OK;
  except
    Result := SZ_ERROR_DATA;
  end;
end;

function ISeqOutStream_Compress_Write(p: Pointer; const buf; size: size_t): size_t; cdecl;
begin
  try
    Result := CSeqOutStream_Compress(p^).OutputStream.Write(buf, size);
  except
    Result := 0;
  end;
end;

{ TAbQueuedBuffer }

function TAbQueuedBuffer.GetDataPointer(AOffset: Integer): Pointer;
begin
  Result := Pointer(PtrUInt(@Self) + SizeOf(TAbQueuedBuffer) + PtrUInt(AOffset));
end;

procedure TAbQueuedBuffer.QueueBuffer(ACompressionStream: TAbLZMACompressionStream);
begin
  PreviousBuffer:= ACompressionStream.FQueuedData.PreviousBuffer;
  NextBuffer:= @ACompressionStream.FQueuedData;
  ACompressionStream.FQueuedData.PreviousBuffer.NextBuffer := @Self;
  ACompressionStream.FQueuedData.PreviousBuffer := @Self;
end;

procedure TAbQueuedBuffer.UnQueueBuffer;
begin
  PreviousBuffer.NextBuffer := NextBuffer;
  NextBuffer.PreviousBuffer := PreviousBuffer;
  PreviousBuffer := nil;
  NextBuffer := nil;
end;

{ TAbLZMACompressionStream }

constructor TAbLZMACompressionStream.Create(AOutputStream: TStream; ACompressionLevel,
  ADictionarySize: Integer);
var
  LLZMAProps: CLzmaEncProps;
  LLZMAPropData: TLZMAPropertyData;
  LHeaderSize: size_t;
begin
  inherited Create;

  FOutputStream := AOutputStream;

  {Initialize the linked list of buffers.}
  FQueuedData.PreviousBuffer := @FQueuedData;
  FQueuedData.NextBuffer := @FQueuedData;

  {Allocate the intermediate compression buffer}
  GetMem(FPIntermediateCompressionBuffer, UncompressedDataBufferSize + SizeOf(TAbQueuedBuffer));
  FIntermediateCompressionBufferAvailableBytes := UncompressedDataBufferSize;

  {Initialize the critical section used to control access to the queued data
   buffer.}
  InitializeCriticalSection(FBufferCriticalSection);
  {Create the semaphore used to put the worker thread to sleep when the input
   buffer is empty.}
  FPendingWorkSemaphore := CreateSemaphore(nil, 0, 1, nil);

  {Create the LZMA encoder}
  FLZMAEncHandle := LzmaEnc_Create(@DelphiMMInterface);
  if FLZMAEncHandle = nil then
    raise Exception.Create('Unable to allocate memory for the LZMA compressor.');

  {Set the compression properties}
  LzmaEncProps_Init(LLZMAProps);
  LLZMAProps.level := ACompressionLevel;
  LLZMAProps.dictSize := ADictionarySize;
  LzmaCheck(LzmaEnc_SetProps(FLZMAEncHandle, LLZMAProps));

  {Store the header in the output stream, making note of the position in the
   stream where the uncompressed size will be stored when compression is
   completed.}
  LHeaderSize := LZMA_PROPS_SIZE;
  LzmaCheck(LzmaEnc_WriteProperties(FLZMAEncHandle, PByte(@LLZMAPropData), LHeaderSize));
  FOutputStream.WriteBuffer(LLZMAPropData, LHeaderSize);
  FOutputStreamHeaderSizeFieldPosition := FOutputStream.Position;
  FOutputStream.WriteBuffer(FTotalBytesWritten, SizeOf(FTotalBytesWritten));

  {Create and start the compression thread.}
  FCompressionThread := TAbLZMACompressionThread.Create(True);
  FCompressionThread.FCompressionStream := Self;
  FCompressionThread.Start;
end;

destructor TAbLZMACompressionStream.Destroy;
var
  LPBuf: PAbQueuedBuffer;
  LOldPos: Int64;
begin
  WaitForCompressionToFinish;

  {If something went wrong during creation of this object before the thread was
   created, then the encoder handle may be non-nil.}
  if FLZMAEncHandle <> nil then
  begin
    LzmaEnc_Destroy(FLZMAEncHandle, @DelphiMMInterface, @DelphiMMInterface);
    FLZMAEncHandle := nil;
  end;

  {Free the critical section and semaphore}
  DeleteCriticalSection(FBufferCriticalSection);
  CloseHandle(FPendingWorkSemaphore);

  {Free the intermediate compression buffer if something went wrong before the
   thread could be created.}
  FreeMem(FPIntermediateCompressionBuffer);

  {If compression failed there may be uncompressed data in the queue: free
   those buffers.}
  while True do
  begin
    LPBuf := FQueuedData.NextBuffer;
    if LPBuf = @FQueuedData then
      Break;
    LPBuf.UnQueueBuffer;
    FreeMem(LPBuf);
  end;

  {Unpdate the uncompressed size in the header}
  if FTotalBytesWritten > 0 then
  begin
    LOldPos := FOutputStream.Position;
    FOutputStream.Position := FOutputStreamHeaderSizeFieldPosition;
    FOutputStream.WriteBuffer(FTotalBytesWritten, SizeOf(FTotalBytesWritten));
    FOutputStream.Position := LOldPos;
  end;

  inherited Destroy;
end;

function TAbLZMACompressionStream.IsBusy: Boolean;
begin
  Result := (FCompressionThread <> nil) and (not FCompressionThread.Finished);
end;

procedure TAbLZMACompressionStream.NoMoreDataToCompress;
var
  LUnqueuedBytes: Integer;
begin
  if FPIntermediateCompressionBuffer <> nil then
  begin
    EnterCriticalSection(FBufferCriticalSection);
    try
      {No more data may be submitted at this point. Set the flag to indicate
       this, and wake the compression thread so that it can finish up.}
      LUnqueuedBytes := UncompressedDataBufferSize - FIntermediateCompressionBufferAvailableBytes;
      if LUnqueuedBytes > 0 then
      begin
        FPIntermediateCompressionBuffer.DataSize := LUnqueuedBytes;
        FPIntermediateCompressionBuffer.QueueBuffer(Self);
      end
      else
        FreeMem(FPIntermediateCompressionBuffer);
      {The temporary buffer is always released, so no further writes may be
       performed.}
      FPIntermediateCompressionBuffer := nil;
    finally
      LeaveCriticalSection(FBufferCriticalSection);
    end;
    {Wake up the compression thread so it can finish the compression process.}
    WakeCompressionThread;
  end;
end;

function TAbLZMACompressionStream.Read(var ABuffer; ACount: Integer): Integer;
begin
  raise Exception.Create('The compression stream does not support reading.');
end;

function TAbLZMACompressionStream.Seek(const AOffset: Int64;
  AOrigin: TSeekOrigin): Int64;
begin
  Result := FTotalBytesWritten;
  if ((AOrigin <> soBeginning) or (AOffset <> Result))
    and ((AOrigin = soBeginning) or (AOffset <> 0)) then
  begin
    raise Exception.Create('The compression stream does not support seeking away from the current position.');
  end;
end;

function TAbLZMACompressionStream.Seek(AOffset: Integer; AOrigin: Word): Integer;
begin
  Result := Seek(Int64(AOffset), TSeekOrigin(AOrigin));
end;

function TAbLZMACompressionStream.WaitForCompressionToFinish: Integer;
begin
  if FCompressionThread <> nil then
  begin
    {Notify the thread that no further data will be submitted.}
    NoMoreDataToCompress;
    {Wait for the compression thread to complete normally and then free it.}
    FCompressionThread.WaitFor;
    FreeAndNil(FCompressionThread);
  end;
  Result := FCompressionErrorCode;
end;

procedure TAbLZMACompressionStream.WakeCompressionThread;
begin
  ReleaseSemaphore(FPendingWorkSemaphore, 1, nil);
end;

function TAbLZMACompressionStream.Write(const ABuffer; ACount: Integer): Integer;
var
  LPSource: PAnsiChar;
  LPBufData: Pointer;
  LPLargeBuf: PAbQueuedBuffer;
begin
  if FPIntermediateCompressionBuffer = nil then
    raise Exception.Create('Write may not be called after NoMoreDataToCompress.');

  if ACount <= 0 then
  begin
    Result := 0;
    Exit;
  end;

  LPSource := @ABuffer;
  {Get a pointer to the position in the intermediate buffer to be written.}
  LPBufData := FPIntermediateCompressionBuffer.GetDataPointer(
    UncompressedDataBufferSize - FIntermediateCompressionBufferAvailableBytes);
  if FIntermediateCompressionBufferAvailableBytes > ACount then
  begin
    {Copy the data into the intermediate buffer and exit.}
    System.Move(LPSource^, LPBufData^, ACount);
    Dec(FIntermediateCompressionBufferAvailableBytes, ACount);
    Result := ACount;
  end
  else
  begin
    {Fill up the intermediate buffer}
    System.Move(LPSource^, LPBufData^, FIntermediateCompressionBufferAvailableBytes);
    Dec(ACount, FIntermediateCompressionBufferAvailableBytes);
    Inc(LPSource, FIntermediateCompressionBufferAvailableBytes);
    Result := FIntermediateCompressionBufferAvailableBytes;
    {If we get here the current intermediate buffer is now full, and must be
     queued.}
    EnterCriticalSection(FBufferCriticalSection);
    try
      {Insert this buffer into the compression queue.}
      FPIntermediateCompressionBuffer.DataSize := UncompressedDataBufferSize;
      FPIntermediateCompressionBuffer.QueueBuffer(Self);
      {Allocate a new intermediate compression buffer}
      GetMem(FPIntermediateCompressionBuffer, UncompressedDataBufferSize + SizeOf(TAbQueuedBuffer));
      FIntermediateCompressionBufferAvailableBytes := UncompressedDataBufferSize;
      {Should the remaining data be copied into the intermediate compression
       buffer, or is it too large and must it be queued separately?}
      if ACount < UncompressedDataBufferSize then
      begin
        LPBufData := FPIntermediateCompressionBuffer.GetDataPointer(0);
        System.Move(LPSource^, LPBufData^, ACount);
        Dec(FIntermediateCompressionBufferAvailableBytes, ACount);
      end
      else
      begin
        {The remaining data is larger than the intermediate buffer: queue it
         separately}
        GetMem(LPLargeBuf, ACount + SizeOf(TAbQueuedBuffer));
        LPLargeBuf.DataSize := ACount;
        LPLargeBuf.QueueBuffer(Self);
        {Copy the data across}
        LPBufData := LPLargeBuf.GetDataPointer(0);
        System.Move(LPSource^, LPBufData^, ACount);
      end;
      {Update the number of bytes written}
      Inc(Result, ACount);
    finally
      LeaveCriticalSection(FBufferCriticalSection);
    end;
    {Wake up the compression thread to compress the newly queued data}
    WakeCompressionThread;
  end;

  Inc(FTotalBytesWritten, Result);
end;

{ TAbLZMACompressionThread }

procedure TAbLZMACompressionThread.Execute;
var
  LInStreamRec: CSeqInStream_Compress;
  LOutStreamRec: CSeqOutStream_Compress;
begin
  {Call the compression function and save the error code}
  LInStreamRec.Intf.Read := ISeqInStream_Compress_Read;
  LInStreamRec.CompressionStream := FCompressionStream;
  LOutStreamRec.Intf.Write := ISeqOutStream_Compress_Write;
  LOutStreamRec.OutputStream := FCompressionStream.FOutputStream;
  FCompressionStream.FCompressionErrorCode := LzmaEnc_Encode(FCompressionStream.FLZMAEncHandle,
    @LOutStreamRec.Intf, @LInStreamRec.Intf, nil, @DelphiMMInterface, @DelphiMMInterface);
  {Free the compression handle}
  LzmaEnc_Destroy(FCompressionStream.FLZMAEncHandle, @DelphiMMInterface, @DelphiMMInterface);
  FCompressionStream.FLZMAEncHandle := nil;
end;

{ TAbLZMADecompressionStream }

constructor TAbLZMADecompressionStream.Create(ASourceStream: TStream);
var
  LLZMAPropData: TLZMAPropertyData;
begin
  inherited Create;

  FSourceStream := ASourceStream;

  {Read the header and uncompressed size from the compressed data stream.}
  FSourceStream.ReadBuffer(LLZMAPropData, LZMA_PROPS_SIZE);
  FSourceStream.ReadBuffer(FUncompressedSize, SizeOf(FUncompressedSize));

  {Initialize the decompressor using the information from the header}
  LzmaDec_Construct(FLzmaState);
  LzmaCheck(LzmaDec_Allocate(FLzmaState, PByte(@LLZMAPropData), LZMA_PROPS_SIZE,
    @DelphiMMInterface));
  LzmaDec_Init(FLzmaState);
end;

destructor TAbLZMADecompressionStream.Destroy;
var
  LUnusedBytes: Integer;
begin
  {Release all decompression resources.}
  LzmaDec_Free(FLzmaState, @DelphiMMInterface);

  {Any unconsumed bytes in the compressed input buffer should be returned to
   the source stream.}
  LUnusedBytes := FCompressedDataBufferSize - FCompressedDataBufferPosition;
  if LUnusedBytes > 0 then
    FSourceStream.Position := FSourceStream.Position - LUnusedBytes;

  inherited Destroy;
end;

function TAbLZMADecompressionStream.GetBytesRead: Int64;
begin
  Result := FBytesDecompressed - FReadBufferAvailableBytes;
end;

function TAbLZMADecompressionStream.GetSize: Int64;
begin
  Result := FUncompressedSize;
end;

function TAbLZMADecompressionStream.InternalDecompressToBuffer(APBuffer: Pointer;
  ABufferSize: Integer): Integer;
var
  LInputBytesProcessed, LOutputBytesProcessed: size_t;
  LFinishMode: Integer;
  LStatus: ELzmaStatus;
begin
  Result := 0;
  {Any more data to decompress to the output buffer?}
  while ABufferSize > 0 do
  begin
    {Read more compressed data into the compressed data buffer, if required.}
    if FCompressedDataBufferPosition >= FCompressedDataBufferSize then
    begin
      FCompressedDataBufferSize := FSourceStream.Read(FCompressedDataBuffer,
        CompressedDataBufferSize);
      FCompressedDataBufferPosition := 0;
    end;

    {Initialize the "processed byte count" variables to the sizes of the input
     and output buffers.}
    LInputBytesProcessed := FCompressedDataBufferSize - FCompressedDataBufferPosition;
    LOutputBytesProcessed := ABufferSize;
    {We may not read more bytes than the number of uncompressed bytes according
     to the header.}
    if (FUncompressedSize - FBytesDecompressed) <= LOutputBytesProcessed then
    begin
      LOutputBytesProcessed := FUncompressedSize - FBytesDecompressed;
      LFinishMode := LZMA_FINISH_END;
    end
    else
      LFinishMode := LZMA_FINISH_ANY;

    {Decompress from the input to the output buffer}
    LzmaCheck(LzmaDec_DecodeToBuf(FLzmaState, APBuffer,
      LOutputBytesProcessed, @FCompressedDataBuffer[FCompressedDataBufferPosition],
      LInputBytesProcessed, LFinishMode, LStatus));

    {Update the input and output buffer stats}
    Inc(FCompressedDataBufferPosition, LInputBytesProcessed);
    Inc(PAnsiChar(APBuffer), LOutputBytesProcessed);
    Dec(ABufferSize, LOutputBytesProcessed);

    {Update the number of bytes decompressed}
    Inc(Result, LOutputBytesProcessed);
    Inc(FBytesDecompressed, LOutputBytesProcessed);

    {Was all the data decompressed? If so, break the loop.}
    if FUncompressedSize = FBytesDecompressed then
      Break;

    {Was nothing from the input or output streams processed? If so, then
     something has gone wrong.}
    if (LInputBytesProcessed = 0) and (LOutputBytesProcessed = 0) then
      raise Exception.Create('LZMA decompression data error');

  end;
end;

function TAbLZMADecompressionStream.Read(var ABuffer; ACount: Integer): Integer;
var
  LBytesAlreadyRead: Integer;
begin
  {Anything to read?}
  if ACount > 0 then
  begin
    {Do we have enough data in the read buffer to satisfy the request?}
    if FReadBufferAvailableBytes >= ACount then
    begin
      {Enough data in the buffer: Fill the output buffer.}
      System.Move(PAnsiChar(@FUncompressedDataBuffer)[FReadBufferSize - FReadBufferAvailableBytes],
        ABuffer, ACount);
      {Subtract from the available bytes in the read buffer.}
      Dec(FReadBufferAvailableBytes, ACount);
      {Successfully read the number of bytes requested}
      Result := ACount;
    end
    else
    begin
      {Not enough bytes available in the read buffer: Is there anything
       available in the uncompressed data buffer? If so, then transfer what we
       have.}
      if FReadBufferAvailableBytes > 0 then
      begin
        {There is some data in the buffer: Read everything}
        System.Move(PAnsiChar(@FUncompressedDataBuffer)[FReadBufferSize - FReadBufferAvailableBytes],
          ABuffer, FReadBufferAvailableBytes);
        LBytesAlreadyRead := FReadBufferAvailableBytes;
        FReadBufferAvailableBytes := 0;
      end
      else
        LBytesAlreadyRead := 0;
      {If we get here it means the read buffer has been emptied and some data
       still has to be read: Do we need to fill up the read buffer again, or do
       we read directly into the target buffer? Large reads bypass the read
       buffering mechanism.}
      if ACount <= MaximumBlockSizeForBufferedIO then
      begin
        {Try to fill the read buffer again}
        FReadBufferSize := InternalDecompressToBuffer(@FUncompressedDataBuffer, UncompressedDataBufferSize);
        FReadBufferAvailableBytes := FReadBufferSize;
        {No more data available? If so we're done.}
        if FReadBufferAvailableBytes = 0 then begin
          Result := LBytesAlreadyRead;
          Exit;
        end;
        {Is enough data now available?}
        if FReadBufferAvailableBytes >= (ACount - LBytesAlreadyRead) then
        begin
          {Enough data in the buffer: Fill the output buffer.}
          System.Move(FUncompressedDataBuffer,
            PAnsiChar(@ABuffer)[LBytesAlreadyRead],
            ACount - LBytesAlreadyRead);
          {Subtract from the available bytes in the read buffer and return the
           number of bytes read.}
          Dec(FReadBufferAvailableBytes, ACount - LBytesAlreadyRead);
          {Successfully read the number of bytes requested}
          Result := ACount;
        end
        else
        begin
          {Enough data is still not available (the end of the compressed stream
           has been reached): Read what we can.}
          System.Move(FUncompressedDataBuffer,
            PAnsiChar(@ABuffer)[LBytesAlreadyRead],
            FReadBufferAvailableBytes);
          Inc(LBytesAlreadyRead, FReadBufferAvailableBytes);
          FReadBufferAvailableBytes := 0;
          Result := LBytesAlreadyRead;
        end;
      end
      else
      begin
        {Decompress directly into the output buffer.}
        Result := InternalDecompressToBuffer(
          @PAnsiChar(@ABuffer)[LBytesAlreadyRead],
          ACount - LBytesAlreadyRead) + LBytesAlreadyRead;
      end;
    end;
  end
  else
    Result := 0;
end;

function TAbLZMADecompressionStream.Seek(const AOffset: Int64; AOrigin: TSeekOrigin): Int64;
begin
  Result := GetBytesRead;
  if ((AOrigin <> soBeginning) or (AOffset <> Result))
    and ((AOrigin <> soCurrent) or (AOffset <> 0)) then
  begin
    raise Exception.Create('Decompression streams do not support seeking away '
      + 'from the current position.');
  end;
end;

function TAbLZMADecompressionStream.Seek(AOffset: Integer; AOrigin: Word): Integer;
begin
  Result := Seek(Int64(AOffset), TSeekOrigin(AOrigin));
end;

function TAbLZMADecompressionStream.Write(const ABuffer; ACount: Integer): Integer;
begin
  raise Exception.Create('Writing to a LZMA decompression stream is not supported.');
end;

{$ENDIF}

end.
