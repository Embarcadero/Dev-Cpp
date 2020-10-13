{******************************************************************}
{ Color helper unit                                                }
{                                                                  }
{ home page : http://www.mwcs.de                                   }
{ email     : martin.walter@mwcs.de                                }
{                                                                  }
{ date      : 26-04-2005                                           }
{                                                                  }
{ Use of this file is permitted for commercial and non-commercial  }
{ use, as long as the author is credited.                          }
{ This file (c) 2005 Martin Walter                                 }
{                                                                  }
{ Thanks to:                                                       }
{ Carlo Barazzetta (Changed color ref)                             }
{ Kiriakos Vlahos (optimization of ConvertColor)                   }
{ Kiriakos Vlahos (conversion from TSVGColor to TColor)            }
{ Kiriakos Vlahos (Fixed currentColor and default fillcolor)       }
{                                                                  }
{ This Software is distributed on an "AS IS" basis, WITHOUT        }
{ WARRANTY OF ANY KIND, either express or implied.                 }
{                                                                  }
{ *****************************************************************}

unit SVGColor;

interface

uses
  Winapi.Windows
  , Winapi.GDIPAPI
  , System.UITypes
  , System.Classes;

function GetSVGColor(const ASVGColorName: string): TColor;
function GetSVGGrayscale(aColor : TColor) : TColor;
function ConvertColor(Color: TColor; Alpha: Byte): Cardinal; inline;
procedure AssignSVGColorList(AList: TStrings);

Var
  SVGColorList: TStringList;

implementation

uses
  System.SysUtils
  , System.UIConsts
  , Vcl.Graphics
  , SVGTypes
  ;
{$IF CompilerVersion < 29}
function AlphaColorToColor(const Color: TAlphaColor): TColor;
begin
  TColorRec(Result).R := TAlphaColorRec(Color).R;
  TColorRec(Result).G := TAlphaColorRec(Color).G;
  TColorRec(Result).B := TAlphaColorRec(Color).B;
  TColorRec(Result).A := 0;
end;
{$IFEND}

function IsHex(const S: string): Boolean;
var
  C: Integer;
  Help: string;
begin
  Result := False;
  if S[1] = '#' then
    Help := Copy(S, 2, Length(S))
  else
    Help := S;
  for C := 1 to Length(Help) do
    if not (((Help[C] >= '0') and (Help[C] <= '9')) or
            ((Help[C] >= 'A') and (Help[C] <= 'F')) or
            ((Help[C] >= 'a') and (Help[C] <= 'f'))) then
    Exit;

  Result := True;
end;

function IsDecimal(const S: string): Boolean;
var
  C: Integer;
begin
  Result := False;
  for C := 1 to Length(S) do
    if not ((S[C] >= '0') and (S[C] <= '9')) then
      Exit;
  Result := True;
end;

function DecodeSVGColorToInt(const S: string): Integer;
var
  C: Integer;
  Percent: Boolean;
  Help: string;
begin
  Result := SVG_INHERIT_COLOR;
  Help := '0' + S;
  Percent := False;
  if Help[Length(Help)] = '%' then
  begin
    Help := Copy(Help, 1, Length(Help) - 1);
    Percent := True;
  end;

  C := SVG_INHERIT_COLOR;
  if IsDecimal(Help) then
    C := StrToInt(Help)
  else
    if IsHex(Help) then
      C := StrToInt('$' + Help);
  if C = SVG_INHERIT_COLOR then
    Exit;
  if C > 255 then
    C := 255;
  if Percent then
  begin
    if C > 100 then
      C := 100;
    C := Round(C * 2.55);
  end;
  Result := C;
end;

function DecodeRGB(const S: string): Integer;
var
  RS, GS, BS: string;
  RGB: string;
  R, B, G: Integer;
begin
  Result := SVG_INHERIT_COLOR;
  if not ((Copy(S, 1, 4) = 'rgb(') and (S[Length(S)] = ')')) then
    Exit;

  RGB := Copy(S, 5, Length(S) - 5);
  RGB := Trim(RGB);

  RS := Copy(RGB, 1, Pos(',', RGB) - 1);
  RGB := Copy(RGB, Pos(',', RGB) + 1, Length(RGB));
  RGB := Trim(RGB);

  GS := Copy(RGB, 1, Pos(',', RGB) - 1);
  RGB := Copy(RGB, Pos(',', RGB) + 1, Length(RGB));
  RGB := Trim(RGB);

  BS := RGB;

  R := DecodeSVGColorToInt(RS);
  G := DecodeSVGColorToInt(GS);
  B := DecodeSVGColorToInt(BS);

  if (R = -1) or (G = -1) or (B = -1) then
    Exit;

  Result := Winapi.Windows.RGB(R, G, B);
end;

function CharToInt(const Ch: Char): Integer;
begin
  Result := Ord(Ch);
  if (Result > 47) and (Result < 58) then
    Dec(Result, 48)
  else
    if (Result > 64) and (Result < 71) then
      Dec(Result, 55)
    else
      if (Result > 96) and (Result < 103) then
        Dec(Result, 87)
      else
        Result := 0;
end;

function PrepareHex(const S: string): string;
var
  C: Integer;
  Help: string;
begin
  if S[1] = '#' then
    Help := Copy(S, 2, Length(S))
  else
    Help := S;
  if Length(Help) > 6 then
    Help := Copy(Help, 1, 6);

  if Length(Help) = 3 then
  begin
    Help := IntToHex(CharToInt(Help[1]) * 17, 2) +
         IntToHex(CharToInt(Help[2]) * 17, 2) +
         IntToHex(CharToInt(Help[3]) * 17, 2);
  end;

  Result := '$';

  for C := 0 to 2 do
    Result := Result + Copy(Help, 5 - C * 2, 2);
end;

function GetSVGColor(const ASVGColorName: string): TColor;
var
  C: Integer;
  Index: Integer;
  Color: string;
begin
  if ASVGColorName = '' then
  begin
    Result := SVG_INHERIT_COLOR;
    Exit;
  end;
  if SameText(ASVGColorName, 'none') then
  begin
    Result := SVG_NONE_COLOR;
    Exit;
  end;
  if SameText(ASVGColorName, 'inherit') then
  begin
    Result := SVG_INHERIT_COLOR;
    Exit;
  end;

  Index := SVGColorList.IndexOf(ASVGColorName);
  if Index >= 0 then
  begin
    Result := TColor(SVGColorList.Objects[Index]);
    Exit;
  end;

  if IsHex(ASVGColorName) and (not IsDecimal(ASVGColorName)) then
    Color := PrepareHex(ASVGColorName)
  else
    Color := ASVGColorName;

  if TryStrToInt(Color, C) then
  begin
    Result := C;
  end
  else
  begin
    Result := DecodeRGB(Color);
  end;
end;

// Converts any color to grayscale
function GetSVGGrayscale(aColor : TColor) : TColor;
var
  LGray : byte;
begin
  // Ignore reserved color values : "INHERIT" (TColors.SysDefault) and "none" (TColors.SysNone) .
  if (aColor = SVG_INHERIT_COLOR) or (aColor = SVG_NONE_COLOR) then exit(aColor);

  // get the luminance according to https://www.w3.org/TR/AERT/#color-contrast
  LGray  := round((0.299 * GetRValue(aColor)) + (0.587 * GetGValue(aColor)) + (0.114 * GetBValue(aColor)));

  // set the result to the new grayscale color including the alpha info
  Result := (aColor and $FF000000) or rgb(LGray, LGray, LGray);
end;

function ConvertColor(Color: TColor; Alpha: Byte): Cardinal;
begin
  with TColors(Color) do
    Result := Winapi.GDIPAPI.MakeColor(Alpha, R, G, B);
end;

procedure AssignSVGColorList(AList: TStrings);
begin
  AList.Assign(SVGColorList);
  AList.InsertObject(0, 'Inherit',  TObject(SVG_INHERIT_COLOR));
end;

procedure StoreToList(Sender: TObject; const S: String);
Var
  Color: NativeInt;
begin
  Color := AlphaColorToColor(StringToAlphaColor(S));
  TStringList(Sender).AddObject(S, TObject(Color));
end;

procedure CreateSVGColorList;
Var
  M: TMethod;
begin
  SVGColorList := TStringList.Create;
{$IF CompilerVersion > 29}
  SVGColorList.Options := SVGColorList.Options - [soUseLocale];
{$IFEND}
  SVGColorList.CaseSensitive := False;
  SVGColorList.Sorted := True;
  SVGColorList.Duplicates := TDuplicates.dupIgnore;
  M.Data := SVGColorList;
  M.Code := @StoreToList;
  GetAlphaColorValues(TGetStrProc(M));
  SVGColorList.Delete(SVGColorList.Count - 1);
  // RSP-30408
  SVGColorList.AddObject('Cyan', TObject(TColors.Cyan));
  SVGColorList.AddObject('Darkgrey', TObject(TColors.Darkgrey));
  SVGColorList.AddObject('Dimgrey', TObject(TColors.Dimgrey));
  SVGColorList.AddObject('Grey', TObject(TColors.Grey));
  SVGColorList.AddObject('Lightgrey', TObject(TColors.Lightgrey));
  SVGColorList.AddObject('Lightslategrey', TObject(TColors.Lightslategrey));
  SVGColorList.AddObject('Magenta', TObject(TColors.Magenta));
  SVGColorList.AddObject('Slategrey', TObject(TColors.Slategrey));
end;

initialization
CreateSVGColorList;
finalization
SVGColorList.Free;
end.
