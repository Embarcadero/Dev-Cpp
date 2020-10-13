{******************************************************************}
{ SVG common                                                       }
{                                                                  }
{ home page : http://www.mwcs.de                                   }
{ email     : martin.walter@mwcs.de                                }
{                                                                  }
{ date      : 05-04-2005                                           }
{                                                                  }
{ Use of this file is permitted for commercial and non-commercial  }
{ use, as long as the author is credited.                          }
{ This file (c) 2005, 2008 Martin Walter                           }
{                                                                  }
{ Thanks to:                                                       }
{ Kiriakos Vlahos (type conversion and utility functions)          }
{ Kiriakos Vlahos (functions from mormot2 for best performances)   }
{                                                                  }
{ This Software is distributed on an "AS IS" basis, WITHOUT        }
{ WARRANTY OF ANY KIND, either express or implied.                 }
{                                                                  }
{ *****************************************************************}

unit SVGCommon;

interface

uses
  Winapi.Windows,
  Winapi.GDIPAPI,
  Winapi.GDIPOBJ,
  System.Types,
  SVGTypes;


function TryStrToTFloat(const S: string; out Value: TFloat): Boolean;

function StrToTFloat(const S: string): TFloat;

// type conversion functions
function ToGPRectF(R: TRect): TGPRectF; overload; inline;
function ToGPRectF(R: TRectF): TGPRectF; overload; inline;
function FromGPRectF(R: TGPRectF): TRectF; inline;

// Utility functions
function HasValue(F: TFloat): Boolean; overload; inline;
function HasValue(I: Integer): Boolean; overload; inline;
function FittedRect(const Bounds: TGPRectF; const Width, Height: Single): TGPRectF;

implementation

uses
  System.SysUtils;


{ *** Start Code from the mormot2 project https://github.com/synopse/mORMot2 }
const
  POW10: array[-31..33] of Extended = (
    1E-31, 1E-30, 1E-29, 1E-28, 1E-27, 1E-26, 1E-25, 1E-24, 1E-23, 1E-22,
    1E-21, 1E-20, 1E-19, 1E-18, 1E-17, 1E-16, 1E-15, 1E-14, 1E-13, 1E-12,
    1E-11, 1E-10, 1E-9,  1E-8,  1E-7,  1E-6,  1E-5,  1E-4,  1E-3,  1E-2,
    1E-1,  1E0,   1E1,   1E2,   1E3,   1E4,   1E5,   1E6,   1E7,   1E8,
    1E9,   1E10,  1E11,  1E12,  1E13,  1E14,  1E15,  1E16,  1E17,  1E18,
    1E19,  1E20,  1E21,  1E22,  1E23,  1E24,  1E25,  1E26,  1E27,  1E28,
    1E29,  1E30,  1E31,  0,     -1);

function HugePower10(exponent: integer): Extended; inline;
var
  e: Extended;
begin
  result := POW10[0]; // 1
  if exponent < 0 then
  begin
    e := POW10[-1];  // 0.1
    exponent := -exponent;
  end
  else
    e := POW10[1];   // 10
  repeat
    while exponent and 1 = 0 do
    begin
      exponent := exponent shr 1;
      e := sqr(e);
    end;
    result := result * e;
    dec(exponent);
  until exponent = 0;
end;

/// get the extended floating point value stored in P^
// - set the err content to the index of any faulty character, 0 if conversion
// was successful (same as the standard val function)
function GetExtended(P: PUTF8Char; out err: integer): Extended;
var
  digit: byte;
  frac, exp: NativeInt;
  c: AnsiChar;
  flags: set of (fNeg, fNegExp, fValid);
  v: Int64; // allows 64-bit resolution for the digits (match 80-bit extended)
label
  e;
begin
  byte(flags) := 0;
  v := 0;
  frac := 0;
  if P = nil then
    goto e;
  c := P^;
  if c = ' ' then
    repeat
      inc(P);
      c := P^;
    until c <> ' '; // trailing spaces
  if c = '+' then
  begin
    inc(P);
    c := P^;
  end
  else if c = '-' then
  begin
    inc(P);
    c := P^;
    include(flags, fNeg);
  end;
  digit := 18; // max Int64 resolution
  repeat
    inc(P);
    if (c >= '0') and (c <= '9') then
    begin
      if digit <> 0 then
      begin
        dec(c, ord('0'));
        {$ifdef CPU64}
        v := v * 10;
        {$else}
        v := v shl 3 + v + v;
        {$endif}
        inc(v, byte(c));
        dec(digit); // over-required digits are just ignored
        include(flags, fValid);
        if frac <> 0 then
          dec(frac); // digits after '.'
        c := P^;
        continue;
      end;
      if frac >= 0 then
        inc(frac); // handle #############00000
      c := P^;
      continue;
    end;
    if c <> '.' then
      break;
    if frac > 0 then
      goto e;
    dec(frac);
    c := P^;
  until false;
  if frac < 0 then
    inc(frac); // adjust digits after '.'
  if (c = 'E') or (c = 'e') then
  begin
    exp := 0;
    exclude(flags, fValid);
    c := P^;
    if c = '+' then
      inc(P)
    else if c = '-' then
    begin
      inc(P);
      include(flags, fNegExp);
    end;
    repeat
      c := P^;
      inc(P);
      if (c < '0') or (c > '9') then
        break;
      dec(c, ord('0'));
      exp := (exp * 10) + byte(c);
      include(flags, fValid);
    until false;
    if fNegExp in flags then
      dec(frac, exp)
    else
      inc(frac, exp);
  end;
  if (fValid in flags) and (c = #0) then
    err := 0
  else
e:  err := 1; // return the (partial) value even if not ended with #0
  if (frac >= -31) and (frac <= 31) then
    result := POW10[frac]
  else
    result := HugePower10(frac);
  if fNeg in flags then
    result := result * POW10[33]; // * -1
  result := result * v;
end;

function ToFloat(P: PAnsiChar; out value: TFloat): boolean; inline;
var
  err: integer;
begin
  value := GetExtended(P, err);
  result := err = 0;
end;
{ *** End Code from the mormot2 project https://github.com/synopse/mORMot2 }


function TryStrToTFloat(const S: string; out Value: TFloat): Boolean;
// The SVG standard does not allow for comma decimal separators
var
  S1: UTF8String;
begin
  S1 := UTF8String(S);

  Result := ToFloat(PUTF8Char(S1), Value);
  if not Result then
    Value := 0;
end;

function StrToTFloat(const S: string): TFloat;
begin
  TryStrToTFloat(S, Result);
end;

function ToGPRectF(R: TRect): TGPRectF;
var
  LLeft: Single;
begin
  LLeft := R.Left;
  Result := WinApi.GDIPAPI.MakeRect(LLeft, R.Top, R.Width, R.Height);
end;


function ToGPRectF(R: TRectF): TGPRectF;
begin
  with R do
    Result := WinApi.GDIPAPI.MakeRect(Left, Top, Width, Height);
end;

function FromGPRectF(R: TGPRectF): TRectF;
begin
  with R do
    Result := TRectF.Create(X, Y, X + Width, Y + Height);
end;

function HasValue(F: TFloat): Boolean;
begin
  Result := F <> UndefinedFloat;
end;

function HasValue(I: Integer): Boolean; overload; inline;
begin
  Result := I <> UndefinedInt;
end;


function FittedRect(const Bounds: TGPRectF; const Width, Height: Single): TGPRectF;
begin
  Result := ToGPRectF(TRectF.Create(0, 0, Width, Height).FitInto(FromGPRectF(Bounds)));
end;


end.
