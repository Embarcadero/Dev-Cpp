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

{*********************************************************}
{* ABBREVIA: AbWavPack.pas                               *}
{*********************************************************}
{* ABBREVIA: WavPack decompression procedures            *}
{*********************************************************}

unit AbWavPack;

{$I AbDefine.inc}

interface

{$IFDEF MSWINDOWS}

uses
  Classes;

// Decompress a WavPack compressed stream from aSrc and write to aDes.
// aSrc must not allow reads past the compressed data.
procedure DecompressWavPack(aSrc, aDes: TStream);

{$ENDIF}

implementation

{$IFDEF MSWINDOWS}

uses
  AbCrtl,
  System.Math,
  System.AnsiStrings,
  System.SysUtils;

type
  PUInt32 = ^UInt32;

// Compile using
//   bcc32 -DWIN32 -DNO_USE_FSTREAMS -c -w-8004 -w-8012 -w-8017 -w-8057 -w-8065 *.c
//
// In wavpack_local.h remove the line "#define FASTCALL __fastcall"

{ C runtime library ======================================================== }

function fabs(x: Double): Double; cdecl;
begin
  if x < 0 then Result := -1
  else Result := x
end;

function floor2(x: Double): Integer; cdecl;
begin
  Result := System.Math.Floor(x);
end;

function labs(n: Integer): Integer; cdecl;
begin
  if n < 0 then Result := -n
  else Result := n;
end;

function _stricmp(str1, str2: PAnsiChar): Integer; cdecl;
begin
  Result := System.AnsiStrings.StrIComp(str1, str2);
end;

function strncmp(str1, str2: PAnsiChar; num: Integer): Integer; cdecl;
begin
  Result := System.AnsiStrings.StrLComp(str1, str2, num);
end;

{ Forward declarations ===================================================== }

// bits.c
procedure bs_open_read; external;
procedure bs_close_read; external;
procedure bs_open_write; external;
procedure bs_close_write; external;
procedure little_endian_to_native; external;
procedure native_to_little_endian; external;

// extra1.c
procedure execute_mono; external;

// extra2.c
procedure execute_stereo; external;

// float.c
procedure float_values; external;
procedure read_float_info; external;
procedure scan_float_data; external;
procedure send_float_data; external;
procedure WavpackFloatNormalize; external;
procedure write_float_info; external;

// metadata.c
procedure add_to_metadata; external;
procedure copy_metadata; external;
procedure free_metadata; external;
procedure process_metadata; external;
procedure read_metadata_buff; external;
procedure write_metadata_block; external;

// pack.c
procedure pack_block; external;
procedure pack_init; external;

// tags.c
procedure load_tag; external;
procedure valid_tag; external;

// unpack.c
procedure check_crc_error; external;
procedure free_tag; external;
procedure unpack_init; external;
procedure unpack_samples; external;

// unpack3.c
procedure free_stream3; external;
procedure get_version3; external;
procedure get_sample_index3; external;
procedure open_file3; external;
procedure seek_sample3; external;
procedure unpack_samples3; external;

// words.c
procedure exp2s; external;
procedure flush_word; external;
procedure get_word; external;
procedure get_words_lossless; external;
procedure init_words; external;
procedure log2s; external;
procedure log2buffer; external;
procedure nosend_word; external;
procedure read_hybrid_profile; external;
procedure read_entropy_vars; external;
procedure restore_weight; external;
procedure scan_word; external;
procedure send_word; external;
procedure send_words_lossless; external;
procedure store_weight; external;
procedure write_entropy_vars; external;
procedure write_hybrid_profile; external;


{ Linker derectives ======================================================== }

{$IF DEFINED(WIN32)}
  {$L Win32\wv_bits.obj}
  {$L Win32\wv_extra1.obj}
  {$L Win32\wv_extra2.obj}
  {$L Win32\wv_float.obj}
  {$L Win32\wv_metadata.obj}
  {$L Win32\wv_pack.obj}
  {$L Win32\wv_tags.obj}
  {$L Win32\wv_unpack.obj}
  {$L Win32\wv_unpack3.obj}
  {$L Win32\wv_words.obj}
  {$L Win32\wv_wputils.obj}
{$ELSEIF DEFINED(WIN64)}
  {$L Win64\wv_bits.obj}
  {$L Win64\wv_extra1.obj}
  {$L Win64\wv_extra2.obj}
  {$L Win64\wv_float.obj}
  {$L Win64\wv_metadata.obj}
  {$L Win64\wv_pack.obj}
  {$L Win64\wv_tags.obj}
  {$L Win64\wv_unpack.obj}
  {$L Win64\wv_unpack3.obj}
  {$L Win64\wv_words.obj}
  {$L Win64\wv_wputils.obj}
{$IFEND}

{ wavpack_local.h ========================================================== }

const
  OPEN_WVC       = $1;     // open/read "correction" file
  OPEN_TAGS      = $2;     // read ID3v1 / APEv2 tags (seekable file)
  OPEN_WRAPPER   = $4;     // make audio wrapper available (i.e. RIFF)
  OPEN_2CH_MAX   = $8;     // open multichannel as stereo (no downmix)
  OPEN_NORMALIZE = $10;    // normalize floating point data to +/- 1.0
  OPEN_STREAMING = $20;    // "streaming" mode blindly unpacks blocks
                           // w/o regard to header file position info
  OPEN_EDIT_TAGS = $40;    // allow editing of tags

type
  int32_t = Integer;
  uint32_t = UInt32;

  WavpackStreamReader = record
    read_bytes: function(id, data: Pointer; bcount: int32_t): int32_t; cdecl;
    get_pos: function(id: Pointer): uint32_t; cdecl;
    set_pos_abs: function(id: Pointer; pos: uint32_t): Integer; cdecl;
    set_pos_rel: function(id: Pointer; delta: int32_t; mode: Integer): Integer; cdecl;
    push_back_byte: function(id: Pointer; c: Integer): Integer; cdecl;
    get_length: function(id: Pointer): uint32_t; cdecl;
    can_seek: function(id: Pointer): Integer; cdecl;
    write_bytes: function(id, data: Pointer; bcount: int32_t): int32_t; cdecl;
  end;

  WavpackContext = Pointer;


{ wputils.c ================================================================ }

function WavpackOpenFileInputEx(const reader: WavpackStreamReader;
  wv_id, wvc_id: Pointer; error: PAnsiChar; flags, norm_offset: Integer): WavpackContext;
  cdecl; external;

function WavpackGetWrapperBytes(wpc: WavpackContext): uint32_t; cdecl; external;
function WavpackGetWrapperData(wpc: WavpackContext): PByte; cdecl; external;
procedure WavpackFreeWrapper (wpc: WavpackContext); cdecl; external;

procedure WavpackSeekTrailingWrapper(wpc: WavpackContext); cdecl; external;

function WavpackGetNumSamples(wpc: WavpackContext): uint32_t; cdecl; external;
function WavpackGetNumChannels(wpc: WavpackContext): Integer; cdecl; external;
function WavpackGetBytesPerSample (wpc: WavpackContext): Integer; cdecl; external;

function WavpackUnpackSamples(wpc: WavpackContext; buffer: Pointer;
  samples: uint32_t): uint32_t; cdecl; external;

function WavpackCloseFile(wpc: WavpackContext): WavpackContext; cdecl; external;


{ TWavPackStream implementation ============================================ }

type
  PWavPackStream = ^TWavPackStream;
  TWavPackStream = record
    HasPushedByte: Boolean;
    PushedByte: Byte;
    Stream: TStream;
  end;
{ -------------------------------------------------------------------------- }
function TWavPackStream_read_bytes(id, data: Pointer; bcount: int32_t): int32_t; cdecl;
begin
  if PWavPackStream(id).HasPushedByte then begin
    PByte(data)^ := PWavPackStream(id).PushedByte;
    PWavPackStream(id).HasPushedByte := False;
    Inc(PByte(data));
    Dec(bcount);
    if bcount = 0 then
      Result := 1
    else
      Result := PWavPackStream(id).Stream.Read(data^, bcount) + 1;
  end
  else
    Result := PWavPackStream(id).Stream.Read(data^, bcount);
end;
{ -------------------------------------------------------------------------- }
function TWavPackStream_get_pos(id: Pointer): uint32_t; cdecl;
begin
  Result := PWavPackStream(id).Stream.Position;
end;
{ -------------------------------------------------------------------------- }
function TWavPackStream_set_pos_abs(id: Pointer; pos: uint32_t): Integer; cdecl;
begin
  PWavPackStream(id).Stream.Position := pos;
  Result := 0;
end;
{ -------------------------------------------------------------------------- }
function TWavPackStream_set_pos_rel(id: Pointer; delta: int32_t;
  mode: Integer): Integer; cdecl;
begin
  PWavPackStream(id).Stream.Seek(delta, mode);
  Result := 1;
end;
{ -------------------------------------------------------------------------- }
function TWavPackStream_push_back_byte(id: Pointer; c: Integer): Integer; cdecl;
begin
  Assert(not PWavPackStream(id).HasPushedByte);
  PWavPackStream(id).HasPushedByte := True;
  PWavPackStream(id).PushedByte := Byte(c);
  Result := 1;
end;
{ -------------------------------------------------------------------------- }
function TWavPackStream_get_length(id: Pointer): uint32_t; cdecl;
begin
  Result := PWavPackStream(id).Stream.Size;
end;
{ -------------------------------------------------------------------------- }
function TWavPackStream_can_seek(id: Pointer): Integer; cdecl;
begin
  Result := 1;
end;
{ -------------------------------------------------------------------------- }
function TWavPackStream_write_bytes(id, data: Pointer;
  bcount: int32_t): int32_t; cdecl;
begin
  Result := PWavPackStream(id).Stream.Write(data^, bcount);
end;


{ Decompression routines =================================================== }

{ -------------------------------------------------------------------------- }
// Reformat samples from longs in processor's native endian mode to
// little-endian data with (possibly) less than 4 bytes / sample.
//
// Based on wvunpack.c::format_samples.
// Conversions simplified since we only support little-endian processors
function FormatSamples(bps: Integer; dst, src: PByte; samcnt: uint32_t): PByte;
var
  sample: UInt32;
begin
  while samcnt > 0 do begin
    Dec(samcnt);
    // Get next sample
    sample := PUInt32(src)^;
    // Convert and write to output
    case bps of
      1: begin
        dst^ := sample + 128;
      end;
      2: begin
        PWord(dst)^ := sample;
      end;
      3: begin
        PByteArray(dst)[0] := sample;
        PByteArray(dst)[1] := sample shr 8;
        PByteArray(dst)[2] := sample shr 16;
      end;
      4: begin
        PUInt32(dst)^ := sample;
      end;
    end;
    Inc(src, SizeOf(UInt32));
    Inc(dst, bps);
  end;
  Result := dst;
end;
{ -------------------------------------------------------------------------- }
// Decompress a WavPack compressed stream from aSrc and write to aDes.
// aSrc must not allow reads past the compressed data.
//
// Based on wvunpack.c::unpack_file()
procedure DecompressWavPack(aSrc, aDes: TStream);
type
  PtrInt = {$IF DEFINED(CPUX64)}Int64{$ELSE}Integer{$IFEND};
const
  OutputBufSize = 256 * 1024;
var
  StreamReader: WavpackStreamReader;
  Context: WavpackContext;
  Src: TWavpackStream;
  Error: array[0..79] of AnsiChar;
  SamplesToUnpack, SamplesUnpacked: uint32_t;
  NumChannels, bps, BytesPerSample: Integer;
  OutputBuf, OutputPtr: PByte;
  DecodeBuf: Pointer;
begin
  OutputBuf := nil;
  DecodeBuf := nil;

  StreamReader.read_bytes := TWavPackStream_read_bytes;
  StreamReader.get_pos := TWavPackStream_get_pos;
  StreamReader.set_pos_abs := TWavPackStream_set_pos_abs;
  StreamReader.set_pos_rel := TWavPackStream_set_pos_rel;
  StreamReader.push_back_byte := TWavPackStream_push_back_byte;
  StreamReader.get_length := TWavPackStream_get_length;
  StreamReader.can_seek := TWavPackStream_can_seek;
  StreamReader.write_bytes := TWavPackStream_write_bytes;

  FillChar(Src, SizeOf(Src), 0);
  Src.Stream := aSrc;

  Context := WavpackOpenFileInputEx(StreamReader, @Src, nil, Error, OPEN_WRAPPER, 0);
  if Context = nil then
    raise Exception.Create('WavPack decompression failed: ' + Error);
  try
    // Write .wav header
    if WavpackGetWrapperBytes(Context) > 0 then begin
      aDes.WriteBuffer(WavpackGetWrapperData(Context)^, WavpackGetWrapperBytes(Context));
      WavpackFreeWrapper(Context);
    end;

    NumChannels := WavpackGetNumChannels(Context);
    bps := WavpackGetBytesPerSample(Context);
    BytesPerSample := NumChannels * bps;

    GetMem(OutputBuf, OutputBufSize);
    OutputPtr := OutputBuf;
    GetMem(DecodeBuf, 4096 * NumChannels * SizeOf(Integer));

    repeat
      // Unpack samples
      SamplesToUnpack := (OutputBufSize - (PtrInt(OutputPtr) - PtrInt(OutputBuf))) div BytesPerSample;
      if (SamplesToUnpack > 4096) then
        SamplesToUnpack := 4096;
      SamplesUnpacked := WavpackUnpackSamples(Context, DecodeBuf, SamplesToUnpack);

      // Convert from 32-bit integers down to appriopriate bit depth
      // and copy to output buffer.
      if (SamplesUnpacked > 0) then
        OutputPtr := FormatSamples(bps, OutputPtr, DecodeBuf,
          SamplesUnpacked * uint32_t(NumChannels));

      // Write output when it's full or when we're done
      if (SamplesUnpacked = 0) or
         ((OutputBufSize - (PtrInt(OutputPtr) - PtrInt(OutputBuf))) < BytesPerSample) then begin
        aDes.WriteBuffer(OutputBuf^, PtrInt(OutputPtr) - PtrInt(OutputBuf));
        OutputPtr := OutputBuf;
      end;
    until (SamplesUnpacked = 0);

    // Write .wav footer
    while WavpackGetWrapperBytes(Context) > 0 do begin
      try
        aDes.WriteBuffer(WavpackGetWrapperData(Context)^,
          WavpackGetWrapperBytes(Context));
      finally
        WavpackFreeWrapper(Context);
      end;
      // Check for more RIFF data
      WavpackUnpackSamples (Context, DecodeBuf, 1);
    end;
  finally
    if DecodeBuf <> nil then
      FreeMemory(DecodeBuf);
    if OutputBuf <> nil then
      FreeMemory(OutputBuf);
    WavpackCloseFile(Context);
  end;
end;

{$ENDIF}

end.

