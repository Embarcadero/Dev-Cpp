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
{* ABBREVIA: AbPPMd.pas                                  *}
{*********************************************************}
{* ABBREVIA: PPMd decompression                          *}
{*********************************************************}

unit AbPPMd;

{$I AbDefine.inc}

interface

{$IFDEF MSWINDOWS}

uses
  Classes;

procedure DecompressPPMd(aSrc, aDes: TStream);

{$ENDIF}

implementation

{$IFDEF MSWINDOWS}

uses
  AbCrtl,
  SysUtils,
  AbExcept;


// Compiled with:
//   Release: bcc32 -q -c *.c
//   Debug:   bcc32 -q -c -v -y *.c


{ Linker derectives ======================================================== }

// Don't re-order these;  it will cause linker errors
{$IF DEFINED(WIN32)}
  {$L Win32\PPMdVariantI.obj}
  {$L Win32\PPMdContext.obj}
  {$L Win32\PPMdSubAllocatorVariantI.obj}
  {$L Win32\CarrylessRangeCoder.obj}
{$ELSEIF DEFINED(WIN64)}
  {$L Win64\PPMdVariantI.obj}
  {$L Win64\PPMdContext.obj}
  {$L Win64\PPMdSubAllocatorVariantI.obj}
  {$L Win64\CarrylessRangeCoder.obj}
{$IFEND}


{ CarrylessRangeCoder.h ==================================================== }

type
  PInStream = ^TInStream;
  TInStream = record
	  nextByte: function(Self: PInStream): Byte; cdecl;
    // Private data
    stream: TStream;
    InPos: Integer;
    InCount: Integer;
    InBuf: array[0..4097] of Byte;
  end;
{ -------------------------------------------------------------------------- }
function TInStream_NextByte(Self: PInStream): Byte; cdecl;
begin
  if Self.InPos = Self.InCount then begin
    Self.InCount := Self.stream.Read(Self.InBuf, SizeOf(Self.InBuf));
    if Self.InCount = 0 then
      raise EAbReadError.Create;
    Self.InPos := 0;
  end;
  Result := Self.InBuf[Self.InPos];
  Inc(Self.InPos);
end;
{ -------------------------------------------------------------------------- }
function TInStream_Create(aStream: TStream): PInStream;
begin
  GetMem(Result, SizeOf(TInStream));
  Result.nextByte := TInStream_NextByte;
  Result.stream := aStream;
  Result.InPos := 0;
  Result.InCount := 0;
end;


{ PPMdVariantI.h =========================================================== }

type
  PPMdModelVariantI = Pointer;

function CreatePPMdModelVariantI(const input: TInStream;
  suballocsize, maxorder, restoration: Integer): PPMdModelVariantI; cdecl; external;
procedure FreePPMdModelVariantI(Self: PPMdModelVariantI); cdecl; external;

function NextPPMdVariantIByte(Self: PPMdModelVariantI): Integer; cdecl; external;


{ Decompression routines =================================================== }

procedure DecompressPPMd(aSrc, aDes: TStream);
const
  OutBufSize = 4096;
var
  nextByte: Integer;
  params: word;
  ppmd: PPMdModelVariantI;
  Src: PInStream;
  OutBuf: PByteArray;
  OutPos: Integer;
begin
  Src := TInStream_Create(aSrc);
  try
    GetMem(OutBuf, OutBufSize);
    try
      OutPos := 0;

      ASrc.ReadBuffer(Params, SizeOf(Params));// Pkzip stream header
      ppmd := CreatePPMdModelVariantI(Src^,
        (((Params shr 4) and $FF) + 1) shl 20,// sub-allocator size
        (Params and $0F) + 1,                 // model order
        Params shr 12);                       // model restoration method
      try
          while True do begin
            nextByte := NextPPMdVariantIByte(ppmd);
            if nextByte < 0 then Break;
            OutBuf[OutPos] := Byte(nextByte);
            Inc(OutPos);
            if OutPos = OutBufSize then begin
              aDes.WriteBuffer(OutBuf^, OutBufSize);
              OutPos := 0;
            end;
          end;
          if OutPos > 0 then
            aDes.WriteBuffer(OutBuf^, OutPos);
      finally
        FreePPMdModelVariantI(ppmd);
      end;
    finally
      FreeMem(OutBuf);
    end;
  finally
    FreeMem(Src);
  end;
end;

{$ENDIF}

end.
