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
 * Craig Peterson <capeterson@users.sourceforge.net>
 *
 * Portions created by the Initial Developer are Copyright (C) 2011
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* ABBREVIA: AbCharset.pas                               *}
{*********************************************************}
{* ABBREVIA: Types and routines for working with various *}
{*   character encodings.                                *}
{*********************************************************}

unit AbCharset;

{$I AbDefine.inc}

interface

uses
  SysUtils{$IFDEF MSWINDOWS}, Windows{$ENDIF};

type
  TAbCharSet = (csASCII, csANSI, csUTF8);

function AbDetectCharSet(const aValue: TBytes): TAbCharSet;

function AbIsOEM(const aValue: TBytes): Boolean;

function AbRawBytesToString(const aValue: TBytes): string;

function AbStringToUnixBytes(const aValue: string): TBytes;

function AbSysCharSetIsUTF8: Boolean;

{$IFDEF MSWINDOWS}
function AbTryEncode(const aValue: UnicodeString; aCodePage: UINT;
  aAllowBestFit: Boolean; out aResult: AnsiString): Boolean;
{$ENDIF}

implementation

function AbDetectCharSet(const aValue: TBytes): TAbCharSet;
var
  i, TrailCnt: Integer;
begin
  Result := csASCII;
  TrailCnt := 0;
  for i := 0 to Length(aValue) - 1 do begin
    if aValue[i] >= $80 then
      Result := csANSI;
    if TrailCnt > 0 then
      if aValue[i] in [$80..$BF] then
        Dec(TrailCnt)
      else Exit
    else if aValue[i] in [$80..$BF] then
      Exit
    else
      case aValue[i] of
        $C0..$C1, $F5..$FF: Exit;
        $C2..$DF: TrailCnt := 1;
        $E0..$EF: TrailCnt := 2;
        $F0..$F4: TrailCnt := 3;
      end;
  end;
  if (TrailCnt = 0) and (Result = csANSI) then
    Result := csUTF8;
end;
{ -------------------------------------------------------------------------- }
function AbIsOEM(const aValue: TBytes): Boolean;
// Detect whether a string of bytes is likely to be the system's ANSI or OEM codepage
{$IFDEF MSWINDOWS}
const
  // Byte values of alpha-numeric characters in OEM and ANSI codepages.
  // Excludes NBSP, ordinal indicators, exponents, the florin symbol, and, for
  // ANSI codepages matched to certain OEM ones, the micro character.
  //
  // US (OEM 437, ANSI 1252)
  Oem437AnsiChars =
    [138, 140, 142, 154, 156, 158, 159, 181, 192..214, 216..246, 248..255];
  Oem437OemChars =
    [128..154, 160..165, 224..235, 237, 238];
  // Arabic (OEM 720, ANSI 1256)
  Oem720AnsiChars =
    [129, 138, 140..144, 152, 154, 156, 159, 170, 181, 192..214, 216..239, 244,
     249, 251, 252, 255];
  Oem720OemChars =
    [130, 131, 133, 135..140, 147, 149..155, 157..173, 224..239];
  // Greek (OEM 737, ANSI 1253)
  Oem737AnsiChars =
    [162, 181, 184..186, 188, 190..209, 211..254];
  Oem737OemChars =
    [128..175, 224..240, 244, 245];
  // Baltic Rim (OEM 775, ANSI 1257)
  Oem775AnsiChars =
    [168, 170, 175, 184, 186, 191..214, 216..246, 248..254];
  Oem775OemChars =
    [128..149, 151..155, 157, 160..165, 173, 181..184, 189, 190, 198, 199,
     207..216, 224..238];
  // Western European (OEM 850, ANSI 1252)
  Oem850AnsiChars =
    [138, 140, 142, 154, 156, 158, 159, 192..214, 216..246, 248..255];
  Oem850OemChars =
    [128..155, 157, 160..165, 181..183, 198, 199, 208..216, 222, 224..237];
  // Central & Eastern European (OEM 852, ANSI 1250)
  Oem852AnsiChars =
    [138, 140..143, 154, 156..159, 163, 165, 170, 175, 179, 185, 186, 188,
     190..214, 216..246, 248..254];
  Oem852OemChars =
    [128..157, 159..169, 171..173, 181..184, 189, 190, 198, 199, 208..216, 221,
     222, 224..238, 251..253];
  // Cyrillic (OEM 855, ANSI 1251)
  Oem855AnsiChars =
    [128, 129, 131, 138, 140..144, 154, 156..159, 161..163, 165, 168, 170, 175,
     178..180, 184, 186, 188..255];
  Oem855OemChars =
    [128..173, 181..184, 189, 190, 198, 199, 208..216, 221, 222, 224..238,
     241..252];
  // Turkish (OEM 857, ANSI 1254)
  Oem857AnsiChars =
    [138, 140, 154, 156, 159, 192..214, 216..246, 248..255];
  Oem857OemChars =
    [128..155, 157..167, 181..183, 198, 199, 210..212, 214..216, 222, 224..230,
     233..237];
  // Hebrew (OEM 862, ANSI 1255)
  Oem862AnsiChars =
    [181, 212..214, 224..250];
  Oem862OemChars =
    [128..154, 160..165, 224..235, 237, 238];
  // Cyrillic CIS (OEM 866, ANSI 1251)
  Oem866AnsiChars =
    [128, 129, 131, 138, 140..144, 154, 156..159, 161..163, 165, 168, 170, 175,
     178..181, 184, 186, 188..255];
  Oem866OemChars =
    [128..175, 224..247];
var
  AnsiChars, OemChars: set of Byte;
  IsANSI: Boolean;
  i: Integer;
begin
  case GetOEMCP of
    437:
    begin
      AnsiChars := Oem437AnsiChars;
      OemChars := Oem437OemChars;
    end;
    720:
    begin
      AnsiChars := Oem720AnsiChars;
      OemChars := Oem720OemChars;
    end;
    737:
    begin
      AnsiChars := Oem737AnsiChars;
      OemChars := Oem737OemChars;
    end;
    775:
    begin
      AnsiChars := Oem775AnsiChars;
      OemChars := Oem775OemChars;
    end;
    850:
    begin
      AnsiChars := Oem850AnsiChars;
      OemChars := Oem850OemChars;
    end;
    852:
    begin
      AnsiChars := Oem852AnsiChars;
      OemChars := Oem852OemChars;
    end;
    855:
    begin
      AnsiChars := Oem855AnsiChars;
      OemChars := Oem855OemChars;
    end;
    857:
    begin
      AnsiChars := Oem857AnsiChars;
      OemChars := Oem857OemChars;
    end;
    862:
    begin
      AnsiChars := Oem862AnsiChars;
      OemChars := Oem862OemChars;
    end;
    866:
    begin
      AnsiChars := Oem866AnsiChars;
      OemChars := Oem866OemChars;
    end;
    else
    begin
      Result := False;
      Exit;
    end;
  end;

  IsANSI := True;
  Result := True;
  for i := 0 to Length(aValue) - 1 do
    if Ord(aValue[i]) >= $80 then
    begin
      if IsANSI then
        IsANSI := Ord(aValue[i]) in AnsiChars;
      if Result then
        Result := Ord(aValue[i]) in OemChars;
      if not IsANSI and not Result then
        Break
    end;
  if IsANSI then
    Result := False;
end;
{$ELSE !MSWINDOWS}
begin
  Result := False;
end;
{$ENDIF !MSWINDOWS}
{ -------------------------------------------------------------------------- }
function AbSysCharSetIsUTF8: Boolean;
begin
  {$IFDEF MACOS}
  Result := True;
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Result := False;
  {$ENDIF}
  {$IFDEF ANDROID}
  Result := True;
  {$ENDIF}
  {$IFDEF LINUX}
  Result := True;
  {$ENDIF}
end;
{ -------------------------------------------------------------------------- }
function AbRawBytesToString(const aValue: TBytes): string;
// Detect encoding of raw bytes and convert to a string
begin
  case AbDetectCharSet(aValue) of
    csASCII:
      Result := TEncoding.ASCII.GetString(aValue);

    csANSI: begin
      {$IFDEF MSWINDOWS}
      if AbIsOEM(aValue) then begin
        SetLength(Result, Length(aValue));
        OemToCharBuff(PAnsiChar(@aValue[0]), PChar(Result), Length(Result));
      end
      else
      {$ENDIF}
        Result := TEncoding.ANSI.GetString(aValue);
    end;

    csUTF8:
      Result := TEncoding.UTF8.GetString(aValue);
  end;
end;
{ -------------------------------------------------------------------------- }
function AbStringToUnixBytes(const aValue: string): TBytes;
// Convert from a string to an appropriate encoding for Unix archive types (tar/gz)
// Based on testing the system encoding should be used on Linux, and UTF-8
// everywhere else.  Windows apps don't agree on whether to use ANSI, OEM, or UTF-8.
begin
  Result := TEncoding.UTF8.GetBytes(aValue);
end;
{ -------------------------------------------------------------------------- }
{$IFDEF MSWINDOWS}
function AbTryEncode(const aValue: UnicodeString; aCodePage: UINT;
  aAllowBestFit: Boolean; out aResult: AnsiString): Boolean;
// Try to encode the given Unicode string as the requested codepage
const
  WC_NO_BEST_FIT_CHARS = $00000400;
  Flags: array[Boolean] of DWORD = (WC_NO_BEST_FIT_CHARS, 0);
var
  UsedDefault: BOOL;
begin
  if not aAllowBestFit and not CheckWin32Version(4, 1) then
    Result := False
  else begin
    SetLength(aResult, WideCharToMultiByte(aCodePage, Flags[aAllowBestFit],
      PWideChar(aValue), Length(aValue), nil, 0, nil, @UsedDefault));
    SetLength(aResult, WideCharToMultiByte(aCodePage, Flags[aAllowBestFit],
      PWideChar(aValue), Length(aValue), PAnsiChar(aResult),
      Length(aResult), nil, @UsedDefault));
    Result := not UsedDefault;
  end;
end;
{$ENDIF MSWINDOWS}

end.

