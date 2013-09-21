{*******************************************************}
{                                                       }
{     BZIP2 Data Compression Interface Unit             }
{                                                       }
{     Similar to ZLIB Data Compression Interface Unit   }
{                                                       }
{*******************************************************}

{*******************************************************}
{  Note from Hongli Lai:                                }
{                                                       }
{  Source code of the .obj files can be found in the    }
{  bzip2 source code package:                           }
{  http://sources.redhat.com/bzip2/                     }
{                                                       }
{  I found this .pas file at:                           }
{  http://www.torry.net/compressother.htm               }
{*******************************************************}

unit BZip2;

interface

uses Sysutils, Classes;

type
  TAlloc = function(opaque: Pointer; Items, Size: Integer): Pointer; cdecl;
  TFree = procedure(opaque, Block: Pointer); cdecl;

  // Internal structure.  Ignore.
  TBZStreamRec = packed record
    next_in: PChar; // next input byte
    avail_in: Integer; // number of bytes available at next_in
    total_in: Integer; // total nb of input bytes read so far

    next_out: PChar; // next output byte should be put here
    avail_out: Integer; // remaining free space at next_out
    total_out: Integer; // total nb of bytes output so far

    state: Pointer;

    bzalloc: TAlloc; // used to allocate the internal state
    bzfree: TFree; // used to free the internal state
    opaque: Pointer;
  end;

  // Abstract ancestor class
  TCustomBZip2Stream = class(TStream)
  private
    FStrm: TStream;
    FStrmPos: Integer;
    FOnProgress: TNotifyEvent;
    FBZRec: TBZStreamRec;
    FBuffer: array[Word] of Char;
  protected
    procedure Progress(Sender: TObject); dynamic;
    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
    constructor Create(Strm: TStream);
  end;

{ TBZCompressionStream compresses data on the fly as data is written to it, and
  stores the compressed data to another stream.

  TBZCompressionStream is write-only and strictly sequential. Reading from the
  stream will raise an exception. Using Seek to move the stream pointer
  will raise an exception.

  Output data is cached internally, written to the output stream only when
  the internal output buffer is full.  All pending output data is flushed
  when the stream is destroyed.

  The Position property returns the number of uncompressed bytes of
  data that have been written to the stream so far.

  CompressionRate returns the on-the-fly percentage by which the original
  data has been compressed:  (1 - (CompressedBytes / UncompressedBytes)) * 100
  If raw data size = 100 and compressed data size = 25, the CompressionRate
  is 75%

  The OnProgress event is called each time the output buffer is filled and
  written to the output stream.  This is useful for updating a progress
  indicator when you are writing a large chunk of data to the compression
  stream in a single call.}


  TBlockSize100k = (bs1, bs2, bs3, bs4, bs5, bs6, bs7, bs8, bs9);

  TBZCompressionStream = class(TCustomBZip2Stream)
  private
    function GetCompressionRate: Single;
  public
    constructor Create(BlockSize100k: TBlockSize100k; Dest: TStream);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    property CompressionRate: Single read GetCompressionRate;
    property OnProgress;
  end;

{ TDecompressionStream decompresses data on the fly as data is read from it.

  Compressed data comes from a separate source stream.  TDecompressionStream
  is read-only and unidirectional; you can seek forward in the stream, but not
  backwards.  The special case of setting the stream position to zero is
  allowed.  Seeking forward decompresses data until the requested position in
  the uncompressed data has been reached.  Seeking backwards, seeking relative
  to the end of the stream, requesting the size of the stream, and writing to
  the stream will raise an exception.

  The Position property returns the number of bytes of uncompressed data that
  have been read from the stream so far.

  The OnProgress event is called each time the internal input buffer of
  compressed data is exhausted and the next block is read from the input stream.
  This is useful for updating a progress indicator when you are reading a
  large chunk of data from the decompression stream in a single call.}

  TBZDecompressionStream = class(TCustomBZip2Stream)
  public
    constructor Create(Source: TStream);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    property OnProgress;
  end;

{ CompressBuf compresses data, buffer to buffer, in one call.
   In: InBuf = ptr to compressed data
       InBytes = number of bytes in InBuf
  Out: OutBuf = ptr to newly allocated buffer containing decompressed data
       OutBytes = number of bytes in OutBuf   }
procedure BZCompressBuf(const InBuf: Pointer; InBytes: Integer;
  out OutBuf: Pointer; out OutBytes: Integer);


{ DecompressBuf decompresses data, buffer to buffer, in one call.
   In: InBuf = ptr to compressed data
       InBytes = number of bytes in InBuf
       OutEstimate = zero, or est. size of the decompressed data
  Out: OutBuf = ptr to newly allocated buffer containing decompressed data
       OutBytes = number of bytes in OutBuf   }
procedure BZDecompressBuf(const InBuf: Pointer; InBytes: Integer;
  OutEstimate: Integer; out OutBuf: Pointer; out OutBytes: Integer);

type
  EBZip2Error = class(Exception);
  EBZCompressionError = class(EBZip2Error);
  EBZDecompressionError = class(EBZip2Error);

implementation

{$L blocks.obj} //blocksort
{$L huffman.obj}
{$L compress.obj}
{$L decompress.obj} //decompress
{$L bzlib.obj}

procedure _hbMakeCodeLengths; external;
procedure _blockSort; external;
procedure _hbCreateDecodeTables; external;
procedure _hbAssignCodes; external;
procedure _compressBlock; external;
procedure _decompress; external;

const
  BZ_RUN = 0;
  BZ_FLUSH = 1;
  BZ_FINISH = 2;
  BZ_OK = 0;
  BZ_RUN_OK = 1;
  BZ_FLUSH_OK = 2;
  BZ_FINISH_OK = 3;
  BZ_STREAM_END = 4;
  BZ_SEQUENCE_ERROR = (-1);
  BZ_PARAM_ERROR = (-2);
  BZ_MEM_ERROR = (-3);
  BZ_DATA_ERROR = (-4);
  BZ_DATA_ERROR_MAGIC = (-5);
  BZ_IO_ERROR = (-6);
  BZ_UNEXPECTED_EOF = (-7);
  BZ_OUTBUFF_FULL = (-8);

  BZ_BLOCK_SIZE_100K = 9;

  _rNums: array[0..511] of Longint = (
    619, 720, 127, 481, 931, 816, 813, 233, 566, 247,
    985, 724, 205, 454, 863, 491, 741, 242, 949, 214,
    733, 859, 335, 708, 621, 574, 73, 654, 730, 472,
    419, 436, 278, 496, 867, 210, 399, 680, 480, 51,
    878, 465, 811, 169, 869, 675, 611, 697, 867, 561,
    862, 687, 507, 283, 482, 129, 807, 591, 733, 623,
    150, 238, 59, 379, 684, 877, 625, 169, 643, 105,
    170, 607, 520, 932, 727, 476, 693, 425, 174, 647,
    73, 122, 335, 530, 442, 853, 695, 249, 445, 515,
    909, 545, 703, 919, 874, 474, 882, 500, 594, 612,
    641, 801, 220, 162, 819, 984, 589, 513, 495, 799,
    161, 604, 958, 533, 221, 400, 386, 867, 600, 782,
    382, 596, 414, 171, 516, 375, 682, 485, 911, 276,
    98, 553, 163, 354, 666, 933, 424, 341, 533, 870,
    227, 730, 475, 186, 263, 647, 537, 686, 600, 224,
    469, 68, 770, 919, 190, 373, 294, 822, 808, 206,
    184, 943, 795, 384, 383, 461, 404, 758, 839, 887,
    715, 67, 618, 276, 204, 918, 873, 777, 604, 560,
    951, 160, 578, 722, 79, 804, 96, 409, 713, 940,
    652, 934, 970, 447, 318, 353, 859, 672, 112, 785,
    645, 863, 803, 350, 139, 93, 354, 99, 820, 908,
    609, 772, 154, 274, 580, 184, 79, 626, 630, 742,
    653, 282, 762, 623, 680, 81, 927, 626, 789, 125,
    411, 521, 938, 300, 821, 78, 343, 175, 128, 250,
    170, 774, 972, 275, 999, 639, 495, 78, 352, 126,
    857, 956, 358, 619, 580, 124, 737, 594, 701, 612,
    669, 112, 134, 694, 363, 992, 809, 743, 168, 974,
    944, 375, 748, 52, 600, 747, 642, 182, 862, 81,
    344, 805, 988, 739, 511, 655, 814, 334, 249, 515,
    897, 955, 664, 981, 649, 113, 974, 459, 893, 228,
    433, 837, 553, 268, 926, 240, 102, 654, 459, 51,
    686, 754, 806, 760, 493, 403, 415, 394, 687, 700,
    946, 670, 656, 610, 738, 392, 760, 799, 887, 653,
    978, 321, 576, 617, 626, 502, 894, 679, 243, 440,
    680, 879, 194, 572, 640, 724, 926, 56, 204, 700,
    707, 151, 457, 449, 797, 195, 791, 558, 945, 679,
    297, 59, 87, 824, 713, 663, 412, 693, 342, 606,
    134, 108, 571, 364, 631, 212, 174, 643, 304, 329,
    343, 97, 430, 751, 497, 314, 983, 374, 822, 928,
    140, 206, 73, 263, 980, 736, 876, 478, 430, 305,
    170, 514, 364, 692, 829, 82, 855, 953, 676, 246,
    369, 970, 294, 750, 807, 827, 150, 790, 288, 923,
    804, 378, 215, 828, 592, 281, 565, 555, 710, 82,
    896, 831, 547, 261, 524, 462, 293, 465, 502, 56,
    661, 821, 976, 991, 658, 869, 905, 758, 745, 193,
    768, 550, 608, 933, 378, 286, 215, 979, 792, 961,
    61, 688, 793, 644, 986, 403, 106, 366, 905, 644,
    372, 567, 466, 434, 645, 210, 389, 550, 919, 135,
    780, 773, 635, 389, 707, 100, 626, 958, 165, 504,
    920, 176, 193, 713, 857, 265, 203, 50, 668, 108,
    645, 990, 626, 197, 510, 357, 358, 850, 858, 364,
    936, 638
    );

  _crc32Table: array[0..255] of Longint = (
    $00000000, $04C11DB7, $09823B6E, $0D4326D9,
    $130476DC, $17C56B6B, $1A864DB2, $1E475005,
    $2608EDB8, $22C9F00F, $2F8AD6D6, $2B4BCB61,
    $350C9B64, $31CD86D3, $3C8EA00A, $384FBDBD,
    $4C11DB70, $48D0C6C7, $4593E01E, $4152FDA9,
    $5F15ADAC, $5BD4B01B, $569796C2, $52568B75,
    $6A1936C8, $6ED82B7F, $639B0DA6, $675A1011,
    $791D4014, $7DDC5DA3, $709F7B7A, $745E66CD,
    -$67DC4920, -$631D54A9, -$6E5E7272, -$6A9F6FC7,
    -$74D83FC4, -$70192275, -$7D5A04AE, -$799B191B,
    -$41D4A4A8, -$4515B911, -$48569FCA, -$4C97827F,
    -$52D0D27C, -$5611CFCD, -$5B52E916, -$5F93F4A3,
    -$2BCD9270, -$2F0C8FD9, -$224FA902, -$268EB4B7,
    -$38C9E4B4, -$3C08F905, -$314BDFDE, -$358AC26B,
    -$0DC57FD8, -$09046261, -$044744BA, -$0086590F,
    -$1EC1090C, -$1A0014BD, -$17433266, -$13822FD3,
    $34867077, $30476DC0, $3D044B19, $39C556AE,
    $278206AB, $23431B1C, $2E003DC5, $2AC12072,
    $128E9DCF, $164F8078, $1B0CA6A1, $1FCDBB16,
    $018AEB13, $054BF6A4, $0808D07D, $0CC9CDCA,
    $7897AB07, $7C56B6B0, $71159069, $75D48DDE,
    $6B93DDDB, $6F52C06C, $6211E6B5, $66D0FB02,
    $5E9F46BF, $5A5E5B08, $571D7DD1, $53DC6066,
    $4D9B3063, $495A2DD4, $44190B0D, $40D816BA,
    -$535A3969, -$579B24E0, -$5AD80207, -$5E191FB2,
    -$405E4FB5, -$449F5204, -$49DC74DB, -$4D1D696E,
    -$7552D4D1, -$7193C968, -$7CD0EFBF, -$7811F20A,
    -$6656A20D, -$6297BFBC, -$6FD49963, -$6B1584D6,
    -$1F4BE219, -$1B8AFFB0, -$16C9D977, -$1208C4C2,
    -$0C4F94C5, -$088E8974, -$05CDAFAB, -$010CB21E,
    -$39430FA1, -$3D821218, -$30C134CF, -$3400297A,
    -$2A47797D, -$2E8664CC, -$23C54213, -$27045FA6,
    $690CE0EE, $6DCDFD59, $608EDB80, $644FC637,
    $7A089632, $7EC98B85, $738AAD5C, $774BB0EB,
    $4F040D56, $4BC510E1, $46863638, $42472B8F,
    $5C007B8A, $58C1663D, $558240E4, $51435D53,
    $251D3B9E, $21DC2629, $2C9F00F0, $285E1D47,
    $36194D42, $32D850F5, $3F9B762C, $3B5A6B9B,
    $0315D626, $07D4CB91, $0A97ED48, $0E56F0FF,
    $1011A0FA, $14D0BD4D, $19939B94, $1D528623,
    -$0ED0A9F2, -$0A11B447, -$075292A0, -$03938F29,
    -$1DD4DF2E, -$1915C29B, -$1456E444, -$1097F9F5,
    -$28D8444A, -$2C1959FF, -$215A7F28, -$259B6291,
    -$3BDC3296, -$3F1D2F23, -$325E09FC, -$369F144D,
    -$42C17282, -$46006F37, -$4B4349F0, -$4F825459,
    -$51C5045E, -$550419EB, -$58473F34, -$5C862285,
    -$64C99F3A, -$6008828F, -$6D4BA458, -$698AB9E1,
    -$77CDE9E6, -$730CF453, -$7E4FD28C, -$7A8ECF3D,
    $5D8A9099, $594B8D2E, $5408ABF7, $50C9B640,
    $4E8EE645, $4A4FFBF2, $470CDD2B, $43CDC09C,
    $7B827D21, $7F436096, $7200464F, $76C15BF8,
    $68860BFD, $6C47164A, $61043093, $65C52D24,
    $119B4BE9, $155A565E, $18197087, $1CD86D30,
    $029F3D35, $065E2082, $0B1D065B, $0FDC1BEC,
    $3793A651, $3352BBE6, $3E119D3F, $3AD08088,
    $2497D08D, $2056CD3A, $2D15EBE3, $29D4F654,
    -$3A56D987, -$3E97C432, -$33D4E2E9, -$3715FF60,
    -$2952AF5B, -$2D93B2EE, -$20D09435, -$24118984,
    -$1C5E343F, -$189F298A, -$15DC0F51, -$111D12E8,
    -$0F5A42E3, -$0B9B5F56, -$06D8798D, -$0219643C,
    -$764702F7, -$72861F42, -$7FC53999, -$7B042430,
    -$6543742B, -$6182699E, -$6CC14F45, -$680052F4,
    -$504FEF4F, -$548EF2FA, -$59CDD421, -$5D0CC998,
    -$434B9993, -$478A8426, -$4AC9A2FD, -$4E08BF4C
    );

procedure _bz_internal_error(errcode: Integer); cdecl;
begin
  raise EBZip2Error.CreateFmt('Compression Error %d', [errcode]);
end;

function _malloc(size: Integer): Pointer; cdecl;
begin
  GetMem(Result, Size);
end;

procedure _free(block: Pointer); cdecl;
begin
  FreeMem(block);
end;

// deflate compresses data

function bzCompressInit(var strm: TBZStreamRec; blockSize100k: Integer;
  verbosity: Integer; workFactor: Integer): Integer; stdcall; external;

function bzCompress(var strm: TBZStreamRec; action: Integer): Integer; stdcall; external;

function bzCompressEnd(var strm: TBZStreamRec): Integer; stdcall; external;

function bzBuffToBuffCompress(dest: Pointer; var destLen: Integer; source: Pointer;
  sourceLen, blockSize100k, verbosity, workFactor: Integer): Integer; stdcall; external;

// inflate decompresses data

function bzDecompressInit(var strm: TBZStreamRec; verbosity: Integer;
  small: Integer): Integer; stdcall; external;

function bzDecompress(var strm: TBZStreamRec): Integer; stdcall; external;

function bzDecompressEnd(var strm: TBZStreamRec): Integer; stdcall; external;

function bzBuffToBuffDecompress(dest: Pointer; var destLen: Integer; source: Pointer;
  sourceLen, small, verbosity: Integer): Integer; stdcall; external;


function bzip2AllocMem(AppData: Pointer; Items, Size: Integer): Pointer; cdecl;
begin
  GetMem(Result, Items * Size);
end;

procedure bzip2FreeMem(AppData, Block: Pointer); cdecl;
begin
  FreeMem(Block);
end;

{
function zlibCheck(code: Integer): Integer;
begin
  Result := code;
  if code < 0 then
    raise EZlibError.Create('error');    //!!
end;
}

function CCheck(code: Integer): Integer;
begin
  Result := code;
  if code < 0 then
    raise EBZCompressionError.CreateFmt('error %d', [code]); //!!
end;

function DCheck(code: Integer): Integer;
begin
  Result := code;
  if code < 0 then
    raise EBZDecompressionError.CreateFmt('error %d', [code]); //!!
end;


procedure BZCompressBuf(const InBuf: Pointer; InBytes: Integer;
  out OutBuf: Pointer; out OutBytes: Integer);
var
  strm: TBZStreamRec;
  P: Pointer;
begin
  FillChar(strm, sizeof(strm), 0);
  strm.bzalloc := bzip2AllocMem;
  strm.bzfree := bzip2FreeMem;
  OutBytes := ((InBytes + (InBytes div 10) + 12) + 255) and not 255;
  GetMem(OutBuf, OutBytes);
  try
    strm.next_in := InBuf;
    strm.avail_in := InBytes;
    strm.next_out := OutBuf;
    strm.avail_out := OutBytes;
    CCheck(bzCompressInit(strm, 9, 0, 0));
    try
      while CCheck(bzCompress(strm, BZ_FINISH)) <> BZ_STREAM_END do
      begin
        P := OutBuf;
        Inc(OutBytes, 256);
        ReallocMem(OutBuf, OutBytes);
        strm.next_out := PChar(Integer(OutBuf) + (Integer(strm.next_out) - Integer(P)));
        strm.avail_out := 256;
      end;
    finally
      CCheck(bzCompressEnd(strm));
    end;
    ReallocMem(OutBuf, strm.total_out);
    OutBytes := strm.total_out;
  except
    FreeMem(OutBuf);
    raise
  end;
end;


procedure BZDecompressBuf(const InBuf: Pointer; InBytes: Integer;
  OutEstimate: Integer; out OutBuf: Pointer; out OutBytes: Integer);
var
  strm: TBZStreamRec;
  P: Pointer;
  BufInc: Integer;
begin
  FillChar(strm, sizeof(strm), 0);
  strm.bzalloc := bzip2AllocMem;
  strm.bzfree := bzip2FreeMem;
  BufInc := (InBytes + 255) and not 255;
  if OutEstimate = 0 then
    OutBytes := BufInc
  else
    OutBytes := OutEstimate;
  GetMem(OutBuf, OutBytes);
  try
    strm.next_in := InBuf;
    strm.avail_in := InBytes;
    strm.next_out := OutBuf;
    strm.avail_out := OutBytes;
    DCheck(bzDecompressInit(strm, 0, 0));
    try
      while DCheck(bzDecompress(strm)) <> BZ_STREAM_END do
      begin
        P := OutBuf;
        Inc(OutBytes, BufInc);
        ReallocMem(OutBuf, OutBytes);
        strm.next_out := PChar(Integer(OutBuf) + (Integer(strm.next_out) - Integer(P)));
        strm.avail_out := BufInc;
      end;
    finally
      DCheck(bzDecompressEnd(strm));
    end;
    ReallocMem(OutBuf, strm.total_out);
    OutBytes := strm.total_out;
  except
    FreeMem(OutBuf);
    raise
  end;
end;

// TCustomBZip2Stream

constructor TCustomBZip2Stream.Create(Strm: TStream);
begin
  inherited Create;
  FStrm := Strm;
  FStrmPos := Strm.Position;
  FBZRec.bzalloc := bzip2AllocMem;
  FBZRec.bzfree := bzip2FreeMem;
end;

procedure TCustomBZip2Stream.Progress(Sender: TObject);
begin
  if Assigned(FOnProgress) then FOnProgress(Sender);
end;


// TBZCompressionStream

constructor TBZCompressionStream.Create(BlockSize100k: TBlockSize100k; Dest: TStream);
const
  BlockSizes: array[TBlockSize100k] of ShortInt = (1, 2, 3, 4, 5, 6, 7, 8, 9);
begin
  inherited Create(Dest);
  FBZRec.next_out := FBuffer;
  FBZRec.avail_out := sizeof(FBuffer);
  CCheck(bzCompressInit(FBZRec, BlockSizes[BlockSize100k], 0, 0));
end;

destructor TBZCompressionStream.Destroy;
begin
  FBZRec.next_in := nil;
  FBZRec.avail_in := 0;
  try
    if FStrm.Position <> FStrmPos then FStrm.Position := FStrmPos;
    while (CCheck(bzCompress(FBZRec, BZ_FINISH)) <> BZ_STREAM_END)
      and (FBZRec.avail_out = 0) do
    begin
      FStrm.WriteBuffer(FBuffer, sizeof(FBuffer));
      FBZRec.next_out := FBuffer;
      FBZRec.avail_out := sizeof(FBuffer);
    end;
    if FBZRec.avail_out < sizeof(FBuffer) then
      FStrm.WriteBuffer(FBuffer, sizeof(FBuffer) - FBZRec.avail_out);
  finally
    bzCompressEnd(FBZRec);
  end;
  inherited Destroy;
end;

function TBZCompressionStream.Read(var Buffer; Count: Longint): Longint;
begin
  raise EBZCompressionError.Create('Invalid stream operation');
end;

function TBZCompressionStream.Write(const Buffer; Count: Longint): Longint;
begin
  FBZRec.next_in := @Buffer;
  FBZRec.avail_in := Count;
  if FStrm.Position <> FStrmPos then FStrm.Position := FStrmPos;
  while (FBZRec.avail_in > 0) do
  begin
    CCheck(bzCompress(FBZRec, BZ_RUN));
    if FBZRec.avail_out = 0 then
    begin
      FStrm.WriteBuffer(FBuffer, sizeof(FBuffer));
      FBZRec.next_out := FBuffer;
      FBZRec.avail_out := sizeof(FBuffer);
      FStrmPos := FStrm.Position;
    end;
    Progress(Self);
  end;
  Result := Count;
end;

function TBZCompressionStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  if (Offset = 0) and (Origin = soFromCurrent) then
    Result := FBZRec.total_in
  else
    raise EBZCompressionError.Create('Invalid stream operation');
end;

function TBZCompressionStream.GetCompressionRate: Single;
begin
  if FBZRec.total_in = 0 then
    Result := 0
  else
    Result := (1.0 - (FBZRec.total_out / FBZRec.total_in)) * 100.0;
end;


// TDecompressionStream

constructor TBZDecompressionStream.Create(Source: TStream);
begin
  inherited Create(Source);
  FBZRec.next_in := FBuffer;
  FBZRec.avail_in := 0;
  DCheck(bzDecompressInit(FBZRec, 0, 0));
end;

destructor TBZDecompressionStream.Destroy;
begin
  bzDecompressEnd(FBZRec);
  inherited Destroy;
end;

function TBZDecompressionStream.Read(var Buffer; Count: Longint): Longint;
begin
  FBZRec.next_out := @Buffer;
  FBZRec.avail_out := Count;
  if FStrm.Position <> FStrmPos then FStrm.Position := FStrmPos;
  while (FBZRec.avail_out > 0) do
  begin
    if FBZRec.avail_in = 0 then
    begin
      FBZRec.avail_in := FStrm.Read(FBuffer, sizeof(FBuffer));
      if FBZRec.avail_in = 0 then
      begin
        Result := Count - FBZRec.avail_out;
        Exit;
      end;
      FBZRec.next_in := FBuffer;
      FStrmPos := FStrm.Position;
    end;
    CCheck(bzDecompress(FBZRec));
    Progress(Self);
  end;
  Result := Count;
end;

function TBZDecompressionStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise EBZDecompressionError.Create('Invalid stream operation');
end;

function TBZDecompressionStream.Seek(Offset: Longint; Origin: Word): Longint;
var
  I: Integer;
  Buf: array[0..4095] of Char;
begin
  if (Offset = 0) and (Origin = soFromBeginning) then
  begin
    DCheck(bzDecompressEnd(FBZRec));
    DCheck(bzDecompressInit(FBZRec, 0, 0));
    FBZRec.next_in := FBuffer;
    FBZRec.avail_in := 0;
    FStrm.Position := 0;
    FStrmPos := 0;
  end
  else if ((Offset >= 0) and (Origin = soFromCurrent)) or
    (((Offset - FBZRec.total_out) > 0) and (Origin = soFromBeginning)) then
  begin
    if Origin = soFromBeginning then Dec(Offset, FBZRec.total_out);
    if Offset > 0 then
    begin
      for I := 1 to Offset div sizeof(Buf) do
        ReadBuffer(Buf, sizeof(Buf));
      ReadBuffer(Buf, Offset mod sizeof(Buf));
    end;
  end
  else
    raise EBZDecompressionError.Create('Invalid stream operation');
  Result := FBZRec.total_out;
end;

end.



