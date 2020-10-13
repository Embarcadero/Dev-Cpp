{******************************************************************}
{ Parse of SVG property values                                     }
{                                                                  }
{ home page : http://www.mwcs.de                                   }
{ email     : martin.walter@mwcs.de                                }
{                                                                  }
{ date      : 05-04-2008                                           }
{                                                                  }
{ Use of this file is permitted for commercial and non-commercial  }
{ use, as long as the author is credited.                          }
{ This file (c) 2005, 2008 Martin Walter                           }
{                                                                  }
{ Thanks to:                                                       }
{ Kiriakos Vlahos (fixed GetFactor)                                }
{ Kiriakos Vlahos (Added parse length in percent)                  }
{ Kiriakos Vlahos (Refactoring parsing)                            }
{ Kiriakos Vlahos (Refactoring parsing Font)                       }
{                                                                  }
{ This Software is distributed on an "AS IS" basis, WITHOUT        }
{ WARRANTY OF ANY KIND, either express or implied.                 }
{                                                                  }
{ *****************************************************************}

unit SVGParse;

interface

uses
  System.Types,
  System.Classes,
  System.Generics.Collections,
  SVGTypes;

function ParseAngle(const Angle: string): TFloat;

function ParseByte(const S: string): Byte;

function ParsePercent(const S: string): TFloat;

function ParseInteger(const S: string): Integer;

function ParseLength(const S: string): TFloat; overload;
function ParseLength(const S: string; var IsPercent: Boolean): TFloat; overload;

function ParseUnit(const S: string): TSVGUnit;

function GetFactor(const SVGUnit: TSVGUnit): TFloat;

function ParseDRect(const S: string): TRectF;

function ParseURI(const URI: string; EmptyOnFail: Boolean = True): string;

function ParseTransform(const ATransform: string): TAffineMatrix;

function ParseDisplay(const ADisplay: string): TTriStateBoolean;

function ParseVisibility(const AVisibility: string): TTriStateBoolean;

function ParseClass(const AClass: string): TArray<string>;

function ParseGradientUnits(const AGradientUnit: string): TGradientUnits;

function ParseFontWeight(const S: string): Integer;

procedure ParseTextDecoration(const S: string; var TD: TTextDecoration);

function ParseFontStyle(AFontStyle: string): Integer;

implementation

uses
  Winapi.Windows,
  System.SysUtils,
  System.Math,
  System.StrUtils,
  SVGCommon;

function ParseAngle(const Angle: string): TFloat;
var
  D: TFloat;
  C: Integer;
  S: string;
begin
  if Angle <> '' then
  begin
    S := Angle;
    C := Pos('deg', S);
    if C <> 0 then
    begin
      S := LeftStr(S, C - 1);
      if TryStrToTFloat(S, D) then
        Result := DegToRad(D)
      else
        Result := 0;
      Exit;
    end;

    C := Pos('rad', S);
    if C <> 0 then
    begin
      TryStrToTFloat(S, Result);
      Exit;
    end;

    C := Pos('grad', S);
    if C <> 0 then
    begin
      S := LeftStr(S, C - 1);
      if TryStrToTFloat(S, D) then
        Result := GradToRad(D)
      else
        Result := 0;
      Exit;
    end;

    if TryStrToTFloat(S, D) then
      Result := DegToRad(D)
    else
      Result := 0;
  end else
    Result := 0;
end;

function ParseByte(const S: string): Byte;
begin
  Result := ParseInteger(S);
end;

function ParsePercent(const S: string): TFloat;
begin
  Result := -1;
  if S = '' then
    Exit;

  if S[Length(S)] = '%' then
    Result := StrToTFloat(LeftStr(S, Length(S) - 1)) / 100
  else
    Result := StrToTFloat(S);
end;

function ParseInteger(const S: string): Integer;
begin
  Result := StrToInt(S);
end;

function ParseLength(const S: string): TFloat;
Var
  IsPercent: Boolean;
begin
   Result := ParseLength(S, IsPercent);
end;

function ParseLength(const S: string; var IsPercent: Boolean): TFloat; overload;
var
  SVGUnit: TSVGUnit;
  Factor: TFloat;
begin
  SVGUnit := ParseUnit(S);
  IsPercent := SVGUnit = suPercent;

  Factor := GetFactor(SVGUnit);
  case SVGUnit of
    suNone: Result := StrToTFloat(S);
    suPercent: Result := StrToTFloat(Copy(S, 1, Length(S) - 1)) * Factor;
    else
      Result := StrToTFloat(Copy(S, 1, Length(S) - 2)) * Factor;
  end;
end;

function ParseUnit(const S: string): TSVGUnit;
Var
  LastTwo: string;
begin
  Result := suNone;
  LastTwo := Copy(S, Length(S) - 1, 2);
  if LastTwo = 'px' then Result := suPx
  else if LastTwo = 'pt' then Result := suPt
  else if LastTwo = 'pc' then Result := suPC
  else if LastTwo = 'mm' then Result := suMM
  else if LastTwo = 'cm' then Result := suCM
  else if LastTwo = 'in' then Result := suIN
  else if LastTwo = 'em' then Result := suEM
  else if LastTwo = 'ex' then Result := suEX
  else if Copy(S, Length(S), 1) = '%' then Result := suPercent;
end;

function GetFactor(const SVGUnit: TSVGUnit): TFloat;
begin
  case SVGUnit of
    suPX: Result := 1;
    suPT: Result := 1.3333;     // 96 / 72
    suPC: Result := 16;         // 1pc = 12 pt
    suMM: Result := 3.77952756; //  96 / 25.4
    suCM: Result := 37.7952756; // 10 mm
    suIN: Result := 96;         // 96 px per inch
    suEM: Result := 16;         // 1 -> font size    12pt = 16 px
    suEX: Result := 16;         // 1 -> font height
    suPercent: Result := 1/100;
    else
      Result := 1;
  end;
end;

function GetValues(const S: string; const Delimiter: Char): TStrings;
var
  C: Integer;
begin
  Result := TStringList.Create;
  Result.Delimiter := Delimiter;
  Result.DelimitedText := S;

  for C := Result.Count - 1 downto 0 do
  begin
    if Result[C] = '' then
    begin
      Result.Delete(C);
    end;
  end;
end;

function ParseDRect(const S: string): TRectF;
var
  SL: TStrings;
begin
  FillChar(Result, SizeOf(Result), 0);

  SL := GetValues(Trim(S), ' ');

  try
    if SL.Count = 4 then
    begin
      Result.Left := ParseLength(SL[0]);
      Result.Top := ParseLength(SL[1]);
      Result.Width := ParseLength(SL[2]);
      Result.Height := ParseLength(SL[3]);
    end;
  finally
    SL.Free;
  end;
end;

function ParseURI(const URI: string; EmptyOnFail: Boolean): string;
var
  S: string;
begin
  if EmptyOnFail then
    Result := ''
  else
    Result := URI;
  if URI <> '' then
  begin
    S := Trim(URI);
    if (Copy(S, 1, 5) = 'url(#') and (S[Length(S)] = ')') then
      Result := Copy(S, 6, Length(S) - 6);
  end;
end;

function GetMatrix(const S: string): TAffineMatrix;
var
  SL: TStrings;
begin
  Result := TAffineMatrix.Identity;
  SL := GetValues(S, ',');
  try
    if SL.Count = 6 then
    begin
      Result.m11 := StrToTFloat(SL[0]);
      Result.m12 := StrToTFloat(SL[1]);
      Result.m21 := StrToTFloat(SL[2]);
      Result.m22 := StrToTFloat(SL[3]);
      Result.dx := StrToTFloat(SL[4]);
      Result.dy := StrToTFloat(SL[5]);
    end;
  finally
    SL.Free;
  end;
end;

function GetTranslate(const S: string): TAffineMatrix;
var
  SL: TStrings;
begin
  FillChar(Result, SizeOf(Result), 0);
  SL := GetValues(S, ',');
  try
    if SL.Count = 1 then
      SL.Add('0');

    if SL.Count = 2 then
    begin
      Result := TAffineMatrix.CreateTranslation(StrToTFloat(SL[0]), StrToTFloat(SL[1]));
    end;
  finally
    SL.Free;
  end;
end;

function GetScale(const S: string): TAffineMatrix;
var
  SL: TStrings;
begin
  FillChar(Result, SizeOf(Result), 0);
  SL := GetValues(S, ',');
  try
    if SL.Count = 1 then
      SL.Add(SL[0]);
    if SL.Count = 2 then
    begin
      Result := TAffineMatrix.CreateScaling(StrToTFloat(SL[0]), StrToTFloat(SL[1]));
    end;
  finally
    SL.Free;
  end;
end;

function GetRotation(const S: string): TAffineMatrix;
var
  SL: TStrings;
  X, Y, Angle: TFloat;
begin
  X := 0;
  Y := 0;
  Angle := 0;
  SL := GetValues(S, ',');
  try
    if SL.Count > 0 then
    begin
      Angle := ParseAngle(SL[0]);

      if SL.Count = 3 then
      begin
        X := StrToTFloat(SL[1]);
        Y := StrToTFloat(SL[2]);
      end else
      begin
        X := 0;
        Y := 0;
      end;
    end;
  finally
    SL.Free;
  end;

  Result := TAffineMatrix.CreateTranslation(X, Y);
  Result := TAffineMatrix.CreateRotation(Angle) * Result;
  Result := TAffineMatrix.CreateTranslation(-X, -Y) * Result;
end;

function GetSkewX(const S: string): TAffineMatrix;
var
  SL: TStrings;
begin
  FillChar(Result, SizeOf(Result), 0);

  SL := GetValues(S, ',');
  try
    if SL.Count = 1 then
    begin
      Result := TAffineMatrix.Identity;
      Result.m21 := Tan(StrToTFloat(SL[0]));
    end;
  finally
    SL.Free;
  end;
end;

function GetSkewY(const S: string): TAffineMatrix;
var
  SL: TStrings;
begin
  FillChar(Result, SizeOf(Result), 0);

  SL := GetValues(S, ',');
  try
    if SL.Count = 1 then
    begin
      Result := TAffineMatrix.Identity;
      Result.m12 := Tan(StrToTFloat(SL[0]));
    end;
  finally
    SL.Free;
  end;
end;

function ParseTransform(const ATransform: string): TAffineMatrix;
var
  Start: Integer;
  Stop: Integer;
  TType: string;
  Values: string;
  S: string;
  M: TAffineMatrix;
begin
  FillChar(Result, SizeOf(Result), 0);

  S := Trim(ATransform);

  while S <> '' do
  begin
    Start := Pos('(', S);
    Stop := Pos(')', S);
    if (Start = 0) or (Stop = 0) then
      Exit;
    TType := Trim(Copy(S, 1, Start - 1));
    Values := Trim(Copy(S, Start + 1, Stop - Start - 1));
    Values := StringReplace(Values, ' ', ',', [rfReplaceAll]);

    if TType = 'matrix' then
    begin
      M := GetMatrix(Values);
    end
    else if TType = 'translate' then
    begin
      M := GetTranslate(Values);
    end
    else if TType = 'scale' then
    begin
      M := GetScale(Values);
    end
    else if TType = 'rotate' then
    begin
      M := GetRotation(Values);
    end
    else if TType = 'skewX' then
    begin
      M := GetSkewX(Values);
    end
    else if TType = 'skewY' then
    begin
      M := GetSkewY(Values);
    end;

    if not M.IsEmpty then
    begin
      if Result.IsEmpty then
        Result := M
      else
        Result := M * Result;
    end;

    S := Trim(Copy(S, Stop + 1, Length(S)));
  end;
end;

function ParseDisplay(const ADisplay: string): TTriStateBoolean;
begin
  if ADisplay = 'inherit' then
    Result := tbInherit
  else if ADisplay = 'none' then
    Result := tbFalse
  else
    Result := tbTrue;
end;

function ParseVisibility(const AVisibility: string): TTriStateBoolean;
begin
  if AVisibility = 'inherit' then
    Result := tbInherit
  else if AVisibility = 'visible' then
    Result := tbTrue
  else
    Result := tbFalse;
end;

function ParseClass(const AClass: string): TArray<string>;

  {$IF CompilerVersion < 28}
  procedure DeleteElement(var A: TArray<string>; const Index: Cardinal;
      Count: Cardinal = 1);
  var
    ALength: Cardinal;
    i: Cardinal;
  begin
    ALength := Length(A);
    for i := Index + Count to ALength - 1 do
      A[i - Count] := A[i];
    SetLength(A, ALength - Count);
  end;
  {$IFEND}

Var
  I: Integer;
begin
  Result := AClass.Split([',']);
  for I := Length(Result) - 1 downto 0 do
  begin
    Result[I] := Trim(Result[I]);
    if Result[I] = '' then
      {$IF CompilerVersion < 28}
      DeleteElement(Result, I, 1);
      {$ELSE}
      System.Delete(Result, I , 1);
      {$IFEND}
  end;
end;

function ParseGradientUnits(const AGradientUnit: string): TGradientUnits;
begin
  Result := guObjectBoundingBox;  // 'objectBoundingBox' default
  if AGradientUnit = 'userSpaceOnUse' then
    Result := guUserSpaceOnUse
end;

function ParseFontWeight(const S: string): Integer;
begin
  Result := FW_NORMAL;
  if S = 'normal' then Result := FW_NORMAL
  else if S = 'bold' then Result := FW_BOLD
  else if S = 'bolder' then Result := FW_EXTRABOLD
  else if S = 'lighter' then Result := FW_LIGHT
  else TryStrToInt(S, Result);
end;

procedure ParseTextDecoration(const S: string; var TD: TTextDecoration);
Var
  SL: TStringList;
begin
 SL := TStringList.Create;
 try
   SL.Delimiter := ' ';
   SL.DelimitedText := S;

   if SL.IndexOf('underline') > -1 then
   begin
     Exclude(TD, tdInherit);
     Include(TD, tdUnderLine);
   end;

   if SL.IndexOf('overline') > -1 then
   begin
     Exclude(TD, tdInherit);
     Include(TD, tdOverLine);
   end;

   if SL.IndexOf('line-through') > -1 then
   begin
     Exclude(TD, tdInherit);
     Include(TD, tdStrikeOut);
   end;

   if SL.IndexOf('none') > -1 then
     TD := [];
   finally
     SL.Free;
   end;
end;

function ParseFontStyle(AFontStyle: string): Integer;
begin
   Result := FontNormal;
   if AFontStyle = 'italic' then
     Result := SVGTypes.FontItalic;
end;


end.
