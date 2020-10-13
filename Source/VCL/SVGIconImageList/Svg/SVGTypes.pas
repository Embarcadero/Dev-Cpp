{******************************************************************}
{ SVG types                                                        }
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
{                                                                  }
{ Thanks to:                                                       }
{ Kiriakos Vlahos (New Types)                                      }
{ Kiriakos Vlahos (Enhanced TSVG attributes)                       }
{ Kiriakos Vlahos (added TSVGElementFeature)                       }
{ Kiriakos Vlahos (Added TSVGElementFeatures)                      }
{                                                                  }
{ This Software is distributed on an "AS IS" basis, WITHOUT        }
{ WARRANTY OF ANY KIND, either express or implied.                 }
{                                                                  }
{ *****************************************************************}

unit SVGTypes;

interface

uses
  System.Math,
  System.Types,
  System.UITypes,
  Winapi.GDIPOBJ;

const
  SVG_INHERIT_COLOR = TColors.SysDefault;
  SVG_NONE_COLOR = TColors.SysNone;

  FontNormal = 0;
  FontItalic = 1;

  MaxTFloat = MaxSingle;
  UndefinedFloat = -340282346638528859811704183484516925440.0;  //Single.MinValue
  UndefinedInt = -2147483648; // Integer.MinValue

type
  //Redefine to compile with older Delphi Versions
  {$IFDEF NEXTGEN}
  PUTF8Char = _PAnsiChar;
  {$ELSE}
  PUTF8Char = PAnsiChar;
  {$ENDIF}

  TFloat = single;

  TSVGElementFeature = (sefMayHaveChildren, sefNeedsPainting, sefChildrenNeedPainting, sefHasPath);
  TSVGElementFeatures = set of TSVGElementFeature;

  TListOfPoints = array of TPointF;

  TRectarray = packed array of TRectF;
  PRectArray = ^TRectArray;

  TTextDecoration = set of (tdInherit, tdUnderLine, tdOverLine, tdStrikeOut);

  TTextPathMethod = (tpmAlign, tpmStretch);

  TTextPathSpacing = (tpsAuto, tpsExact);

  TSVGUnit = (suNone, suPX, suPT, suPC, suMM, suCM, suIN, suEM, suEX, suPercent);

  TGradientUnits = (guObjectBoundingBox, guUserSpaceOnUse);

  TLengthType = (ltHorz, ltVert, ltOther);

  TTriStateBoolean = (tbFalse, tbTrue, tbInherit);

TSVGAttribute  = (saId,
                  saX,
                  saY,
                  saX1,
                  saY1,
                  saX2,
                  saY2,
                  saCx,
                  saCy,
                  saD,
                  saDx,
                  saDy,
                  saFx,
                  saFy,
                  saR,
                  saRx,
                  saRy,
                  saStyle,
                  saClass,
                  saXlinkHref,
                  saHref,
                  saPoints,
                  saGradientUnits,
                  saGradientTransform,
                  saVisibility,
                  saVersion,
                  saWidth,
                  saHeight,
                  saViewBox,
                  saTransform,
                  saOffset,
                  saStopOpacity,
                  saStopColor,
                  saSpacing,
                  saStartOffset,
                  saMethod,
                  saStrokeWidth,
                  saLineWidth,
                  saOpacity,
                  saStrokeOpacity,
                  saFillOpacity,
                  saColor,
                  saStroke,
                  saFill,
                  saClipPath,
                  saStrokeLinejoin,
                  saStrokeLinecap,
                  saStrokeMiterlimit,
                  saStrokeDashoffset,
                  saStrokeDasharray,
                  saFillRule,
                  saFontFamily,
                  saFontWeight,
                  saFontSize,
                  saTextDecoration,
                  saFontStyle,
                  saDisplay);

  TAffineMatrix = record
  public
    m11: Single;
    m12: Single;
    m21: Single;
    m22: Single;
    dx: Single;
    dy: Single;

    constructor Create(_m11, _m12, _m21, _m22, _dx, _dy: Single);
    constructor FromGPMatrix(GPMatrix: TGPMatrix);

    function IsEmpty: Boolean;
    function IsIdentity: Boolean;
    function ToGPMatrix: TGPMatrix;

    class function CreateRotation(const AAngle: Single): TAffineMatrix; static;
    class function CreateScaling(const AScaleX, AScaleY: Single): TAffineMatrix; static;
    class function CreateTranslation(const ADeltaX, ADeltaY: Single): TAffineMatrix; static;

    class operator Multiply(const AMatrix1, AMatrix2: TAffineMatrix): TAffineMatrix;
    class operator Multiply(const APoint: TPointF; const AMatrix: TAffineMatrix): TPointF;
  end;

  TAffineMatrixConstants = record helper for TAffineMatrix
    const Empty: TAffineMatrix = (m11: 0; m12: 0; m21: 0; m22: 0; dx: 0; dy: 0);
    const Identity: TAffineMatrix = (m11: 1; m12: 0; m21: 0; m22: 1; dx: 0; dy: 0);
  end;

implementation

Uses
  System.SysUtils;



{ TAffineMatrix }

constructor TAffineMatrix.Create(_m11, _m12, _m21, _m22, _dx, _dy: Single);
begin
  Self.m11 := _m11;
  Self.m12 := _m12;
  Self.m21 := _m21;
  Self.m22 := _m22;
  Self.dx := _dx;
  Self.dy := _dy;
end;

class function TAffineMatrix.CreateRotation(
  const AAngle: Single): TAffineMatrix;

  procedure SinCosSingle(const Theta: Single; var Sin, Cos: Single);
  var
  {$IF SizeOf(Extended) > SizeOf(Double)}
    S, C: Extended;
  {$ELSE}
    S, C: Double;
  {$ENDIF}
  begin
    System.SineCosine(Theta, S, C);
    Sin := S;
    Cos := C;
  end;
var
  Sine, Cosine: Single;
begin
  SinCosSingle(AAngle, Sine, Cosine);

  Result := Identity;
  Result.m11 := Cosine;
  Result.m12 := Sine;
  Result.m21 := -Sine;
  Result.m22 := Cosine;
end;

class function TAffineMatrix.CreateScaling(const AScaleX,
  AScaleY: Single): TAffineMatrix;
begin
  Result := Identity;
  Result.m11 := AScaleX;
  Result.m22 := AScaleY;
end;

class function TAffineMatrix.CreateTranslation(const ADeltaX,
  ADeltaY: Single): TAffineMatrix;
begin
  Result := Identity;
  Result.dx := ADeltaX;
  Result.dy := ADeltaY;
end;

constructor TAffineMatrix.FromGPMatrix(GPMatrix: TGPMatrix);
Var
  MA: Winapi.GDIPOBJ.TMatrixArray;
begin
  GPMatrix.GetElements(MA);

  Self.m11 := MA[0];
  Self.m12 := MA[1];
  Self.m21 := MA[2];
  Self.m22 := MA[3];
  Self.dx := MA[4];
  Self.dy := MA[5];
end;

function TAffineMatrix.IsEmpty: Boolean;
begin
  Result := CompareMem(@Self, @TAffineMatrix.Empty, SizeOf(TAffineMatrix));
end;

function TAffineMatrix.IsIdentity: Boolean;
begin
  Result := CompareMem(@Self, @TAffineMatrix.Identity, SizeOf(TAffineMatrix));
end;

class operator TAffineMatrix.Multiply(const APoint: TPointF;
  const AMatrix: TAffineMatrix): TPointF;
begin
  Result.X := APoint.X * AMatrix.m11 + APoint.Y * AMatrix.m21 + AMatrix.dx;
  Result.Y := APoint.X * AMatrix.m12 + APoint.Y * AMatrix.m22 + AMatrix.dy;
end;

function TAffineMatrix.ToGPMatrix: TGPMatrix;
begin
  Result := TGPMatrix.Create(m11, m12, m21, m22, dx, dy);
end;

class operator TAffineMatrix.Multiply(const AMatrix1,
  AMatrix2: TAffineMatrix): TAffineMatrix;
begin
  Result.m11 := AMatrix1.m11 * AMatrix2.m11 + AMatrix1.m12 * AMatrix2.m21;
  Result.m12 := AMatrix1.m11 * AMatrix2.m12 + AMatrix1.m12 * AMatrix2.m22;
  Result.m21 := AMatrix1.m21 * AMatrix2.m11 + AMatrix1.m22 * AMatrix2.m21;
  Result.m22 := AMatrix1.m21 * AMatrix2.m12 + AMatrix1.m22 * AMatrix2.m22;
  Result.dx := AMatrix1.dx * AMatrix2.m11 + AMatrix1.dy * AMatrix2.m21 + AMatrix2.dx;
  Result.dy := AMatrix1.dx * AMatrix2.m12 + AMatrix1.dy * AMatrix2.m22 + AMatrix2.dy;
end;

end.
