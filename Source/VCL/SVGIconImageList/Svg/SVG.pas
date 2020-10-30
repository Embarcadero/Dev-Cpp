{******************************************************************}
{ SVG Image                                                        }
{                                                                  }
{ home page : http://www.mwcs.de                                   }
{ email     : martin.walter@mwcs.de                                }
{                                                                  }
{ date      : 05-06-2020                                           }
{                                                                  }
{ version   : 0.69c                                                }
{                                                                  }
{ Use of this file is permitted for commercial and non-commercial  }
{ use, as long as the author is credited.                          }
{ This file (c) 2005, 2008 Martin Walter                           }
{                                                                  }
{ Thanks to:                                                       }
{ Bart Vandromme (parsing errors)                                  }
{ Chris Ueberall (parsing errors)                                  }
{ Elias Zurschmiede (font error)                                   }
{ Christopher Cerny  (Dash Pattern)                                }
{ Carlo Barazzetta (fixed transform)                               }
{ Carlo Barazzetta (fixed style display none)                      }
{ Carlo Barazzetta (added fixedcolor and grayscale)                }
{ Kiriakos Vlahos (fixed CalculateMatrices)                        }
{ Kiriakos Vlahos (added FillMode property)                        }
{ Kiriakos Vlahos (fixed SetBounds based on ViewBox)               }
{ Kiriakos Vlahos (added 'fill-rule' presentation attribute)       }
{ Kiriakos Vlahos (fixed loadlength)                               }
{ Kiriakos Vlahos (Fixed currentColor and default fillcolor)       }
{ Kiriakos Vlahos (Replacement of MSXLML with XMLite)              }
{ Kiriakos Vlahos (Refactoring parsing)                            }
{ Kiriakos Vlahos (Refactoring parsing Font)                       }
{ Kiriakos Vlahos (Increase performances using Dictionary)         }
{                                                                  }
{ This Software is distributed on an "AS IS" basis, WITHOUT        }
{ WARRANTY OF ANY KIND, either express or implied.                 }
{                                                                  }
{ *****************************************************************}

unit SVG;

{.$DEFINE USE_TEXT} // Define to use "real" text instead of paths

interface

uses
  Winapi.Windows,
  Winapi.GDIPOBJ,
  Winapi.GDIPAPI,
  System.UITypes,
  System.Classes,
  System.Math,
  System.Generics.Collections,
  {$IF CompilerVersion > 27}System.NetEncoding,{$ELSE}IdCoderMIME,{$IFEND}
  System.Types,
  XMLLite,
  GDIPOBJ2,
  GDIPKerning,
  GDIPPathText,
  SVGTypes,
  SVGStyle,
  SVGColor;

resourcestring
  SVG_ERROR_PARSING_SVG_TEXT = 'Error parsing SVG Text: %s';
  SVG_ERROR_TAG_SVG = 'Error: Tag "svg" not found.';
type
  TSVG = class;
  TSVGObject = class;

  TSVGObjectProc = reference to function(Obj: TSVGObject): Boolean;

  TSVGObjectClass = class of TSVGObject;
  TSVGObject = class abstract (TPersistent)
  private
    FItems: TList;
    FVisible: TTriStateBoolean;
    FDisplay: TTriStateBoolean;
    FParent: TSVGObject;
    FID: string;
    FObjectName: string;
    FClasses: TArray<string>;

    function GetCount: Integer;
    function GetDepth: Integer;
    procedure SetItem(const Index: Integer; const Item: TSVGObject);
    function GetItem(const Index: Integer): TSVGObject;

    function GetDisplay: TTriStateBoolean;
    function GetVisible: TTriStateBoolean;
    function GetObjectStyle: TStyle;
  protected
    FStyle: TStyle;
    procedure ReadChildren(const Reader: IXMLReader);
    class function New(Parent: TSVGObject): TSVGObject;
    procedure AssignTo(Dest: TPersistent); override;
    function GetRoot: TSVG;
    class var SVGElements: TDictionary<string, TSVGObjectClass>;
    class var SVGAttributes: TDictionary<string, TSVGAttribute>;
    class constructor Create;
    class destructor Destroy;
  public
    constructor Create; overload; virtual;
    constructor Create(Parent: TSVGObject); overload;
    destructor Destroy; override;
    procedure Clear; virtual;
    function Clone(Parent: TSVGObject): TSVGObject;
    function Add(Item: TSVGObject): Integer;
    procedure Delete(Index: Integer);
    function Remove(Item: TSVGObject): Integer;
    function IndexOf(Item: TSVGObject): Integer;
    function FindByID(const Name: string): TSVGObject; virtual;
    procedure CalculateMatrices;
    function ObjectBounds(IncludeStroke: Boolean = False;
      ApplyTranform: Boolean = False): TRectF; virtual;
    function Traverse(Proc: TSVGObjectProc): TSVGObject;

    procedure PaintToGraphics(Graphics: TGPGraphics); virtual; abstract;
    procedure PaintToPath(Path: TGPGraphicsPath); virtual; abstract;
    function ReadInAttr(SVGAttr: TSVGAttribute; const AttrValue: string): Boolean; virtual;
    procedure ReadIn(const Reader: IXMLReader); virtual;

    property Items[const Index: Integer]: TSVGObject read GetItem write SetItem; default;
    property Count: Integer read GetCount;
    property Depth: Integer read GetDepth;

    property Display: TTriStateBoolean read GetDisplay write FDisplay;
    property Visible: TTriStateBoolean read GetVisible write FVisible;
    property Parent: TSVGObject read FParent;
    property ID: string read FID;
    property ObjectName: string read FObjectName;
    property ObjectStyle: TStyle read GetObjectStyle;
    class function Features: TSVGElementFeatures; virtual;
  end;

  TSVGMatrix = class abstract (TSVGObject)
  private
    FLocalMatrix: TAffineMatrix;
    FFullMatrix: TAffineMatrix;
    procedure SetPureMatrix(const Value: TAffineMatrix);

    function Transform(const P: TPointF): TPointF; overload;
  protected
    procedure CalcMatrix; virtual;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure Clear; override;
    function ReadInAttr(SVGAttr: TSVGAttribute; const AttrValue: string): Boolean; override;
    property FullMatrix: TAffineMatrix read FFullMatrix;
    property LocalMatrix: TAffineMatrix read FLocalMatrix write SetPureMatrix;
  end;

  TSVGBasic = class abstract (TSVGMatrix)
  private
    FFillColor: TColor;
    FStrokeColor: TColor;
    FFillOpacity: TFloat;
    FStrokeOpacity: TFloat;
    FStrokeWidth: TFloat;
    FStrokeLineJoin: string;
    FStrokeLineCap: string;
    FStrokeMiterLimit: TFloat;
    FStrokeDashOffset: TFloat;
    FStrokeDashArray: TSingleDynArray;
    FArrayNone: Boolean;

    FFontName: string;
    FFontSize: TFloat;
    FFontWeight: Integer;
    FFontStyle: Integer;
    FTextDecoration: TTextDecoration;

    FPath: TGPGraphicsPath2;
    FFillMode: TFillMode;
    FX: TFloat;
    FY: TFloat;
    FWidth: TFloat;
    FHeight: TFloat;
    FStyleChanged: Boolean;

    function IsFontAvailable: Boolean;
    procedure SetStrokeDashArray(const S: string);

    function GetFillColor: TColor;
    function GetStrokeColor: TColor;
    function GetFillOpacity: TFloat;
    function GetStrokeOpacity: TFloat;
    function GetStrokeWidth: TFloat;
    function GetClipURI: string;
    function GetStrokeLineCap: TLineCap;
    function GetStrokeDashCap: TDashCap;
    function GetStrokeLineJoin: TLineJoin;
    function GetStrokeMiterLimit: TFloat;
    function GetStrokeDashOffset: TFloat;
    function GetStrokeDashArray: TSingleDynArray;

    function GetFontName: string;
    function GetFontWeight: Integer;
    function GetFontSize: TFloat;
    function GetFontStyle: Integer;
    function GetTextDecoration: TTextDecoration;
    procedure UpdateStyle;
    procedure OnStyleChanged(Sender: TObject);
    procedure SetClipPath(Graphics: TGPGraphics);
  protected
    FRX: TFloat;
    FRY: TFloat;
    FFillURI: string;
    FStrokeURI: string;
    FClipURI: string;
    FLineWidth: TFloat;
    FColorInterpolation: TFloat;
    FColorRendering: TFloat;

    procedure ParseLengthAttr(const AttrValue: string;
      LengthType: TLengthType; var X: TFloat);
    procedure AssignTo(Dest: TPersistent); override;
    procedure ReadStyle(Style: TStyle); virtual;
    procedure ProcessFontAttribute(SVGAttr: TSVGAttribute; const Value: string);
    function GetGPPath: TGPGraphicsPath2;
    procedure ConstructPath; virtual;

    function GetFillBrush: TGPBrush;
    function GetStrokeBrush: TGPBrush;
    function GetStrokePen(const StrokeBrush: TGPBrush): TGPPen;

    procedure BeforePaint(const Graphics: TGPGraphics; const Brush: TGPBrush;
      const Pen: TGPPen); virtual;
    procedure AfterPaint(const Graphics: TGPGraphics; const Brush: TGPBrush;
      const Pen: TGPPen); virtual;
  public
    constructor Create; override;
    procedure Clear; override;
    procedure PaintToGraphics(Graphics: TGPGraphics); override;

    procedure PaintToPath(Path: TGPGraphicsPath); override;
    procedure ReadIn(const Reader: IXMLReader); override;
    function ReadInAttr(SVGAttr: TSVGAttribute; const AttrValue: string): Boolean; override;
    function ObjectBounds(IncludeStroke: Boolean = False;
      ApplyTranform: Boolean = False): TRectF; override;

    property Root: TSVG read GetRoot;

    property FillColor: TColor read GetFillColor write FFillColor;
    property FillMode: TFillMode read fFillMode write fFillMode;
    property StrokeColor: TColor read GetStrokeColor write FStrokeColor;
    property FillOpacity: TFloat read GetFillOpacity write FFillOpacity;
    property StrokeOpacity: TFloat read GetStrokeOpacity write FStrokeOpacity;
    property StrokeWidth: TFloat read GetStrokeWidth write FStrokeWidth;
    property ClipURI: string read GetClipURI write FClipURI;
    property StrokeURI: string read FStrokeURI write FStrokeURI;
    property X: TFloat read FX write FX;
    property Y: TFloat read FY write FY;
    property Width: TFloat read FWidth write FWidth;
    property Height: TFloat read FHeight write FHeight;
    property RX: TFloat read FRX write FRX;
    property RY: TFloat read FRY write FRY;
    property GPPath: TGPGraphicsPath2 read GetGPPath;

    property StrokeLineCap: TLineCap read GetStrokeLineCap;
    property StrokeLineJoin: TLineJoin read GetStrokeLineJoin;
    property StrokeMiterLimit: TFloat read GetStrokeMiterLimit write FStrokeMiterLimit;
    property StrokeDashOffset: TFloat read GetStrokeDashOffset write FStrokeDashOffset;

    property FontName: string read GetFontName write FFontName;
    property FontSize: TFloat read GetFontSize write FFontSize;
    property FontWeight: Integer read GetFontWeight write FFontWeight;
    property FontStyle: Integer read GetFontStyle write FFontStyle;
    property TextDecoration: TTextDecoration read GetTextDecoration write FTextDecoration;
end;

  TSVG = class(TSVGBasic)
  strict private
    FRootBounds: TGPRectF;
    FInitialMatrix: TAffineMatrix;
    FSource: string;
    FAngle: TFloat;
    FAngleMatrix: TAffineMatrix;
    FViewBox: TRectF;
    FFileName: string;
    FGrayscale: Boolean;
    FFixedColor: TColor;
    FIdDict: TDictionary<string, TSVGObject>;

    procedure SetViewBox(const Value: TRectF);

    procedure SetSVGOpacity(Opacity: TFloat);
    procedure SetAngle(Angle: TFloat);
    procedure Paint(const Graphics: TGPGraphics; Rects: PRectArray;
      RectCount: Integer);
  private
    FStyles: TStyleList;
    procedure SetFixedColor(const Value: TColor);
    procedure ReloadFromText;
    procedure SetGrayscale(const Value: boolean);
  protected
    procedure CalcMatrix; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure ReadStyles(const Reader: IXMLReader);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Clear; override;
    procedure ReadIn(const Reader: IXMLReader); override;
    function ReadInAttr(SVGAttr: TSVGAttribute; const AttrValue: string): Boolean; override;

    procedure DeReferenceUse;
    function GetStyleValue(const Name, Key: string): string;
    function FindByID(const Name: string): TSVGObject; override;

    procedure LoadFromText(const Text: string);
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream); overload;
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);

    procedure SetBounds(const Bounds: TGPRectF);
    procedure PaintTo(DC: HDC; Bounds: TGPRectF;
      Rects: PRectArray; RectCount: Integer); overload;
    procedure PaintTo(MetaFile: TGPMetaFile; Bounds: TGPRectF;
      Rects: PRectArray; RectCount: Integer); overload;
    procedure PaintTo(Graphics: TGPGraphics; Bounds: TGPRectF;
      Rects: PRectArray; RectCount: Integer); overload;
    procedure PaintTo(DC: HDC; aLeft, aTop, aWidth, aHeight : single); overload;
    function RenderToIcon(Size: Integer): HICON;
    function RenderToBitmap(Width, Height: Integer): HBITMAP;

    property InitialMatrix: TAffineMatrix read FInitialMatrix write FInitialMatrix;
    property SVGOpacity: TFloat write SetSVGOpacity;
    property Source: string read FSource;
    property Angle: TFloat read FAngle write SetAngle;
    property ViewBox: TRectF read FViewBox write SetViewBox;
    property Grayscale: boolean read FGrayscale write SetGrayscale;
    property FixedColor: TColor read FFixedColor write SetFixedColor;
    property IdDict : TDictionary<string, TSVGObject> read FIdDict;

    class function Features: TSVGElementFeatures; override;
  end;

  TSVGContainer = class(TSVGBasic)
  public
    class function Features: TSVGElementFeatures; override;
  end;

  TSVGSwitch = class(TSVGContainer)
  {
     The <switch> SVG element evaluates any requiredFeatures, requiredExtensions
     and systemLanguage attributes on its direct child elements in order, and
     then renders the first child where these attributes evaluate to true.

     None of these is implemented.
  }
  end;

  TSVGDefs = class(TSVGBasic)
  public
    class function Features: TSVGElementFeatures; override;
  end;

  TSVGUse = class(TSVGBasic)
  private
    FReference: string;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Construct;
  public
    procedure Clear; override;
    function ReadInAttr(SVGAttr: TSVGAttribute; const AttrValue: string): Boolean; override;

    class function Features: TSVGElementFeatures; override;
  end;

  TSVGShape = class abstract(TSVGBasic)
    class function Features: TSVGElementFeatures; override;
  end;

  TSVGRect = class(TSVGShape)
  protected
    procedure ConstructPath; override;
  public
    procedure ReadIn(const Reader: IXMLReader); override;
    function ObjectBounds(IncludeStroke: Boolean = False;
      ApplyTranform: Boolean = False): TRectF; override;
  end;

  TSVGLine = class(TSVGShape)
  protected
    procedure ConstructPath; override;
  public
    function ReadInAttr(SVGAttr: TSVGAttribute; const AttrValue: string): Boolean; override;
    function ObjectBounds(IncludeStroke: Boolean = False;
      ApplyTranform: Boolean = False): TRectF; override;
  end;

  TSVGPolyLine = class(TSVGShape)
  private
    FPoints: TListOfPoints;
    FPointCount: Integer;
    procedure ConstructPoints(const S: string);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure ConstructPath; override;
  public
    constructor Create; override;
    procedure Clear; override;
    function ReadInAttr(SVGAttr: TSVGAttribute; const AttrValue: string): Boolean; override;
    function ObjectBounds(IncludeStroke: Boolean = False;
      ApplyTranform: Boolean = False): TRectF; override;
  end;

  TSVGPolygon = class(TSVGPolyLine)
  protected
    procedure ConstructPath; override;
  public
  end;

  TSVGEllipse = class(TSVGShape)
  protected
    FCX: TFloat;
    FCY: TFloat;
    procedure ConstructPath; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure Clear; override;
    function ReadInAttr(SVGAttr: TSVGAttribute; const AttrValue: string): Boolean; override;
    function ObjectBounds(IncludeStroke: Boolean = False;
      ApplyTranform: Boolean = False): TRectF; override;
    property CX: TFloat read FCX write FCX;
    property CY: TFloat read FCY write FCY;
  end;

  TSVGPath = class(TSVGShape)
  private
    function SeparateValues(const ACommand: Char; const S: string; CommandList:
        TList<Char>; ValueList: TList<TFloat>): Boolean;
    procedure Split(const S: string; CommandList: TList<Char>;  ValueList: TList<TFloat>);
  protected
    procedure ConstructPath; override;
  public
    function ReadInAttr(SVGAttr: TSVGAttribute; const AttrValue: string): Boolean; override;
    function ObjectBounds(IncludeStroke: Boolean = False;
      ApplyTranform: Boolean = False): TRectF; override;
  end;

  TSVGImage = class(TSVGBasic)
  private
    FImageURI: string;
    FFileName: string;
    FImage: TGPImage;
    FStream: TMemoryStream;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    procedure Clear; override;
    procedure PaintToGraphics(Graphics: TGPGraphics); override;
    procedure ReadIn(const Reader: IXMLReader); override;
    function ReadInAttr(SVGAttr: TSVGAttribute; const AttrValue: string): Boolean; override;
    function ObjectBounds(IncludeStroke: Boolean = False;
      ApplyTranform: Boolean = False): TRectF; override;
    property Data: TMemoryStream read FStream;

    class function Features: TSVGElementFeatures; override;
  end;

  TSVGCustomText = class abstract (TSVGBasic)
  private
    FText: string;
    FUnderlinePath: TGPGraphicsPath;
    FStrikeOutPath: TGPGraphicsPath;

    FFontHeight: TFloat;
    FDX: TFloat;
    FDY: TFloat;

    FHasX: Boolean;
    FHasY: Boolean;

    function GetCompleteWidth: TFloat;
    procedure SetSize; virtual;
    function GetFont: TGPFont;
    function GetFontFamily(const FontName: string): TGPFontFamily;

    function IsInTextPath: Boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure ConstructPath; override;
    procedure ParseNode(const Reader: IXMLReader); virtual;
    procedure BeforePaint(const Graphics: TGPGraphics; const Brush: TGPBrush;
      const Pen: TGPPen); override;
    procedure AfterPaint(const Graphics: TGPGraphics; const Brush: TGPBrush;
      const Pen: TGPPen); override;

    procedure ReadTextNodes(const Reader: IXMLReader); virtual;
  public
    constructor Create; override;
    procedure Clear; override;
    procedure ReadIn(const Reader: IXMLReader); override;
    function ReadInAttr(SVGAttr: TSVGAttribute; const AttrValue: string): Boolean; override;
    function ObjectBounds(IncludeStroke: Boolean = False;
      ApplyTranform: Boolean = False): TRectF; override;
    procedure PaintToGraphics(Graphics: TGPGraphics); override;

    property DX: TFloat read FDX write FDX;
    property DY: TFloat read FDY write FDY;
    property FontHeight: TFloat read FFontHeight write FFontHeight;
    property Text: string read FText write FText;

    class function Features: TSVGElementFeatures; override;
  end;

  TSVGText = class(TSVGCustomText)
  public
  end;

  TSVGTSpan = class(TSVGCustomText)
  end;

  TSVGTextPath = class(TSVGCustomText)
  private
    FOffset: TFloat;
    FOffsetIsPercent: Boolean;
    FPathRef: string;
    FMethod: TTextPathMethod;
    FSpacing: TTextPathSpacing;
  protected
    procedure ConstructPath; override;
  public
    procedure Clear; override;
    function ReadInAttr(SVGAttr: TSVGAttribute; const AttrValue: string): Boolean; override;
  end;

  TSVGClipPath = class(TSVGBasic)
  private
    FClipPath: TGPGraphicsPath;
  protected
    procedure ConstructClipPath;
    function GetClipPath: TGPGraphicsPath;
  public
    destructor Destroy; override;
    procedure Clear; override;
    function ReadInAttr(SVGAttr: TSVGAttribute; const AttrValue: string): Boolean; override;

    property ClipPath: TGPGraphicsPath read GetClipPath;

    class function Features: TSVGElementFeatures; override;
  end;

implementation

uses
  WinApi.ActiveX,
  System.SysUtils,
  System.Variants,
  System.StrUtils,
  System.Character,
  SVGParse,
  SVGPaint,
  SVGPath,
  SVGCommon;

{$REGION 'TSVGObject'}
constructor TSVGObject.Create;
begin
  inherited;
  FParent := nil;
  Clear;
end;

constructor TSVGObject.Create(Parent: TSVGObject);
begin
  Create;
  if Assigned(Parent) then
  begin
    Parent.Add(Self);
  end;
end;

destructor TSVGObject.Destroy;
begin
  Clear;
  if Assigned(FItems) then
    FItems.Free;

  if Assigned(FParent) then
  begin
    FParent.Remove(Self);
  end;

  if Assigned(FStyle) then
    FStyle.Free;

  inherited;
end;

procedure TSVGObject.CalculateMatrices;
var
  C: Integer;
begin
  if Self is TSVGMatrix then
    TSVGMatrix(Self).CalcMatrix;

  for C := 0 to Count - 1 do
  begin
    TSVGObject(FItems[C]).CalculateMatrices;
  end;
end;

procedure TSVGObject.Clear;
begin
  while Count > 0 do
  begin
    Items[0].Free;
  end;

  FVisible := tbTrue;
  FDisplay := tbTrue;
  FID := '';

  SetLength(FClasses, 0);
  if Assigned(FStyle) then FStyle.Clear;
  FObjectName := '';
end;

function TSVGObject.Clone(Parent: TSVGObject): TSVGObject;
var
  C: Integer;
begin
  Result := New(Parent);
  Result.Assign(Self);

  for C := 0 to Count - 1 do
    GetItem(C).Clone(Result);
end;

class constructor TSVGObject.Create;
begin
  SVGElements := TDictionary<string, TSVGObjectClass>.Create(64);
  SVGElements.Add('g', TSVGContainer);
  SVGElements.Add('switch', TSVGSwitch);
  SVGElements.Add('defs', TSVGDefs);
  SVGElements.Add('use', TSVGUse);
  SVGElements.Add('rect', TSVGRect);
  SVGElements.Add('line', TSVGLine);
  SVGElements.Add('polyline', TSVGPolyLine);
  SVGElements.Add('polygon', TSVGPolygon);
  SVGElements.Add('circle', TSVGEllipse);
  SVGElements.Add('ellipse', TSVGEllipse);
  SVGElements.Add('path', TSVGPath);
  SVGElements.Add('image', TSVGImage);
  SVGElements.Add('text', TSVGText);
  SVGElements.Add('tspan', TSVGTSpan);
  SVGElements.Add('textPath', TSVGTextPath);
  SVGElements.Add('clipPath', TSVGClipPath);
  SVGElements.Add('linearGradient', TSVGLinearGradient);
  SvgElements.Add('radialGradient', TSVGRadialGradient);
  SvgElements.Add('stop', TSVGStop);

  SVGAttributes := TDictionary<string, TSVGAttribute>.Create(256);
  SVGAttributes.Add('id', saId);
  SVGAttributes.Add('x', saX);
  SVGAttributes.Add('y', saY);
  SVGAttributes.Add('x1', saX1);
  SVGAttributes.Add('y1', saY1);
  SVGAttributes.Add('x2', saX2);
  SVGAttributes.Add('y2', saY2);
  SVGAttributes.Add('cx', saCx);
  SVGAttributes.Add('cy', saCy);
  SVGAttributes.Add('d', saD);
  SVGAttributes.Add('dx', saDx);
  SVGAttributes.Add('dy', saDy);
  SVGAttributes.Add('fx', saFx);
  SVGAttributes.Add('fy', saFy);
  SVGAttributes.Add('r', saR);
  SVGAttributes.Add('rx', saRx);
  SVGAttributes.Add('ry', saRy);
  SVGAttributes.Add('style', saStyle);
  SVGAttributes.Add('class', saClass);
  SVGAttributes.Add('xlink:href', saXlinkHref);
  SVGAttributes.Add('href', saHref);
  SVGAttributes.Add('points', saPoints);
  SVGAttributes.Add('gradientUnits', saGradientUnits);
  SVGAttributes.Add('gradientTransform', saGradientTransform);
  SVGAttributes.Add('visibility', saVisibility);
  SVGAttributes.Add('version', saVersion);
  SVGAttributes.Add('width', saWidth);
  SVGAttributes.Add('height', saHeight);
  SVGAttributes.Add('viewBox', saViewBox);
  SVGAttributes.Add('transform', saTransform);
  SVGAttributes.Add('offset', saOffset);
  SVGAttributes.Add('stop-opacity', saStopOpacity);
  SVGAttributes.Add('stop-color', saStopColor);
  SVGAttributes.Add('startOffset', saStartOffset);
  SVGAttributes.Add('method', saMethod);
  SVGAttributes.Add('spacing', saSpacing);
  SVGAttributes.Add('stroke-width', saStrokeWidth);
  SVGAttributes.Add('line-width', saLineWidth);
  SVGAttributes.Add('opacity', saOpacity);
  SVGAttributes.Add('stroke-opacity', saStrokeOpacity);
  SVGAttributes.Add('fill-opacity', saFillOpacity);
  SVGAttributes.Add('color', saColor);
  SVGAttributes.Add('stroke', saStroke);
  SVGAttributes.Add('fill', saFill);
  SVGAttributes.Add('clip-path', saClipPath);
  SVGAttributes.Add('stroke-linejoin', saStrokeLinejoin);
  SVGAttributes.Add('stroke-linecap', saStrokeLinecap);
  SVGAttributes.Add('stroke-miterlimit', saStrokeMiterlimit);
  SVGAttributes.Add('stroke-dashoffset', saStrokeDashoffset);
  SVGAttributes.Add('stroke-dasharray', saStrokeDasharray);
  SVGAttributes.Add('fill-rule', saFillRule);
  SVGAttributes.Add('font-family', saFontFamily);
  SVGAttributes.Add('font-weight', saFontWeight);
  SVGAttributes.Add('font-size', saFontSize);
  SVGAttributes.Add('text-decoration', saTextDecoration);
  SVGAttributes.Add('font-style', saFontStyle);
  SVGAttributes.Add('display', saDisplay);
end;

function TSVGObject.Add(Item: TSVGObject): Integer;
begin
  if FItems = nil then
    FItems := TList.Create;
  Result := FItems.Add(Item);
  Item.FParent := Self;
end;

procedure TSVGObject.Delete(Index: Integer);
var
  Item: TSVGBasic;
begin
  if (Index >= 0) and (Index < Count) then
  begin
    Item := FItems[Index];
    FItems.Delete(Index);
    Remove(Item);
  end;
end;

class destructor TSVGObject.Destroy;
begin
  SVGElements.Free;
  SVGAttributes.Free;
end;

function TSVGObject.Remove(Item: TSVGObject): Integer;
begin
  if Assigned(FItems) then
    Result := FItems.Remove(Item)
  else
    Result := -1;
  if Assigned(Item) then
  begin
    if Item.FParent = Self then
      Item.FParent := nil;
  end;
end;

function TSVGObject.IndexOf(Item: TSVGObject): Integer;
begin
  if Assigned(FItems) then
    Result := FItems.IndexOf(Item)
  else
    Result := -1;
end;

class function TSVGObject.New(Parent: TSVGObject): TSVGObject;
begin
  // Create(Parent) will call the virtual Create and the appropriate \
  // constructor will be used.
  // You call New from an instance of a type
  Result := Self.Create(Parent);
end;

function TSVGObject.ObjectBounds(IncludeStroke, ApplyTranform: Boolean): TRectF;
begin
  Result := TRectF.Create(0, 0, 0, 0);
end;

class function TSVGObject.Features: TSVGElementFeatures;
begin
  Result := [];
end;

function TSVGObject.FindByID(const Name: string): TSVGObject;
begin
  Result := Traverse(
    function(SVG: TSVGObject): Boolean
    begin
      Result := ('#' + SVG.FID = Name) or (SVG.FID = Name);
    end);
end;

procedure TSVGObject.AssignTo(Dest: TPersistent);
var
  SVG: TSVGObject;
begin
  if (Dest is TSVGObject) then
  begin
    SVG := Dest as TSVGObject;
    SVG.FVisible := FVisible;
    SVG.FDisplay := FDisplay;
    SVG.FID := FID;
    SVG.FObjectName := FObjectName;

    FreeAndNil(SVG.FStyle);
    if Assigned(FStyle) then
      SVG.FStyle := FStyle.Clone;
    SVG.FClasses := Copy(FClasses, 0);
  end;
end;

function TSVGObject.GetCount: Integer;
begin
  if Assigned(FItems) then
    Result := FItems.Count
  else
    Result := 0;
end;

procedure TSVGObject.SetItem(const Index: Integer; const Item: TSVGObject);
begin
  if (Index >= 0) and (Index < Count) then
    FItems[Index] := Item;
end;

function TSVGObject.Traverse(Proc: TSVGObjectProc): TSVGObject;
Var
  I: Integer;
begin
  Result := nil;
  if Proc(Self) then Exit(Self);
  for I := 0 to Count - 1 do
  begin
    Result := Items[I].Traverse(Proc);
    if Assigned(Result) then Exit;
  end;
end;

function TSVGObject.GetItem(const Index: Integer): TSVGObject;
begin
  if (Index >= 0) and (Index < Count) then
    Result := FItems[Index]
  else
    Result := nil;
end;

function TSVGObject.GetObjectStyle: TStyle;
begin
  if not Assigned(FStyle) then
  begin
    FStyle := TStyle.Create;
    if Self is TSVGBasic then
      FStyle.OnChange := TSVGBasic(Self).OnStyleChanged;
  end;
  Result := FStyle;
end;

function TSVGObject.GetRoot: TSVG;
var
  Temp: TSVGObject;
begin
  Temp := Self;

  while Assigned(Temp) and (not (Temp is TSVG)) do
    Temp := Temp.FParent;

  Result := TSVG(Temp);
end;

function TSVGObject.GetDepth: Integer;
Var
  SVG : TSVGObject;
begin
  Result := 1;
  SVG := Self;
  while Assigned(SVG.Parent) do
  begin
    Inc(Result);
    SVG := SVG.Parent;
  end;
end;

function TSVGObject.GetDisplay: TTriStateBoolean;
// if display is false in an element, children also will not be displayed
var
  SVG: TSVGObject;
begin
  Result := tbTrue;
  SVG := Self;
  while Assigned(SVG) do
  begin
    if SVG.FDisplay = tbFalse then Exit(tbFalse);
    SVG := SVG.FParent;
  end;
end;

function TSVGObject.GetVisible: TTriStateBoolean;
var
  SVG: TSVGObject;
begin
  Result := tbTrue;
  SVG := Self;
  while Assigned(SVG) do
  begin
    if SVG.FVisible <> tbInherit then Exit(SVG.FVisible);
    SVG := SVG.FParent;
  end;
end;

function TSVGObject.ReadInAttr(SVGAttr: TSVGAttribute;
  const AttrValue: string): Boolean;
begin
  Result := True;
  case SVGAttr of
    saId: FID := AttrValue;
    saVisibility: FVisible := ParseVisibility(AttrValue);
    saStyle: ObjectStyle.SetValues(AttrValue);
    saClass: FClasses := ParseClass(AttrValue);
    saDisplay: FDisplay := ParseDisplay(AttrValue);
  else
    Result := False;
  end;
end;

procedure TSVGObject.ReadChildren(const Reader: IXMLReader);
var
  SVG: TSVGObject;
  LRoot: TSVG;
  NodeName: string;
  SVGObjecClass: TSVGObjectClass;
  pName: PWidechar;
  Len: LongWord;
  StartDepth, EndDepth: LongWord;
  NodeType: XmlNodeType;
begin
  if Reader.IsEmptyElement then Exit;
  StartDepth := Depth;
  Repeat
    if not EXmlLite.IsOK(Reader.Read(NodeType)) then break;
    case NodeType of
      XmlNodeType.None: Break;
      XmlNodeType.EndElement: Reader.GetDepth(EndDepth);
      XmlNodeType.Element:
        begin
          SVG := nil;
          Reader.GetLocalName(pName, Len);
          SetString(NodeName, pName, Len);

          if SvgElements.TryGetValue(NodeName, SVGObjecClass) then
            SVG := SVGObjecClass.Create(Self)
          else if NodeName = 'style' then
          begin
            LRoot := GetRoot;
            LRoot.ReadStyles(Reader);
          end;

          if Assigned(SVG) then
          begin
            SVG.ReadIn(Reader);
          end;
        end;
    end;
  Until ((Nodetype = XmlNodeType.EndElement) and (EndDepth = StartDepth));
end;

procedure TSVGObject.ReadIn(const Reader: IXMLReader);
var
  AttrName: string;
  AttrValue: string;
  pName: PWidechar;
  pValue: PWidechar;
  Len: LongWord;
  HRes: HRESULT;
  SVGAttr: TSVGAttribute;
begin
  Reader.GetLocalName(pName, Len);
  SetString(FObjectName, pName, Len);

  HRes := Reader.MoveToFirstAttribute;
  while HRes = S_OK do
  begin
    Reader.GetLocalName(pName, Len);
    SetString(AttrName, pName, Len);
    Reader.GetValue(pValue, Len);
    SetString(AttrValue, pValue, Len);

    if not (SVGAttributes.TryGetValue(AttrName, SVGAttr) and
    ReadInAttr(SVGAttr, AttrValue)) and not AttrName.StartsWith('xmlns')
    then
      ObjectStyle.AddStyle(AttrName, AttrValue);
    HRes := Reader.MoveToNextAttribute;
  end;
  Reader.MoveToElement;

  if sefMayHaveChildren in Features then
    ReadChildren(Reader);
end;
{$ENDREGION}

{$REGION 'TSVGMatrix'}
procedure TSVGMatrix.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TSVGMatrix then
  begin
    TSVGMatrix(Dest).FLocalMatrix := FLocalMatrix;
  end;
end;

procedure TSVGMatrix.CalcMatrix;
var
  SVG: TSVGObject;
begin
  SVG := Parent;
  while Assigned(SVG) do
  begin
    if SVG is TSVGMatrix then break;
    SVG := SVG.FParent;
  end;

  if Assigned(SVG) and not TSVGMatrix(SVG).FullMatrix.IsEmpty then
    FFullMatrix := TSVGMatrix(SVG).FullMatrix
  else
    FillChar(FFullMatrix, SizeOf(FFullMatrix), 0);

  if not FLocalMatrix.IsEmpty then begin
    if not FFullMatrix.IsEmpty then
      FFullMatrix := FLocalMatrix * FFullMatrix
    else
      FFullMatrix := FLocalMatrix;
  end;
end;

procedure TSVGMatrix.Clear;
begin
  inherited;
  FillChar(FLocalMatrix, SizeOf(FLocalMatrix), 0);
  FillChar(FFullMatrix, SizeOf(FFullMatrix), 0);
end;

function TSVGMatrix.ReadInAttr(SVGAttr: TSVGAttribute;
  const AttrValue: string): Boolean;
begin
  Result := True;
  if SVGAttr = saTransform then
    FLocalMatrix := ParseTransform(AttrValue)
  else
    Result := inherited;
end;

procedure TSVGMatrix.SetPureMatrix(const Value: TAffineMatrix);
begin
  FLocalMatrix := Value;
end;

function TSVGMatrix.Transform(const P: TPointF): TPointF;
begin
  if not FFullMatrix.IsEmpty then
    Result := P * FFullMatrix
  else
    Result := P;
end;

{$ENDREGION}

{$REGION 'TSVGBasic'}
constructor TSVGBasic.Create;
begin
  inherited;
  FPath := nil;
  SetLength(FStrokeDashArray, 0);
end;

procedure TSVGBasic.BeforePaint(const Graphics: TGPGraphics;
  const Brush: TGPBrush; const Pen: TGPPen);
Var
  SolidBrush : TGPBrush;
begin
  if (Brush is TGPPathGradientBrush) and (GPPath <> nil) and (FFillColor <> SVG_INHERIT_COLOR) then
  begin
    // Fill with solid color
    SolidBrush :=  TGPSolidBrush.Create(TGPColor(FFillColor));
    try
      Graphics.FillPath(SolidBrush, GPPath);
    finally
      SolidBrush.Free;
      FFillColor := SVG_INHERIT_COLOR;
    end;
  end;
 end;

procedure TSVGBasic.Clear;
begin
  inherited;

  FX := 0;
  FY := 0;
  FWidth := 0;
  FHeight := 0;
  FRX := UndefinedFloat;
  FRY := UndefinedFloat;
  FFillURI := '';
  FStrokeURI := '';
  FillColor := SVG_INHERIT_COLOR;
  StrokeColor := SVG_INHERIT_COLOR;
  // default SVG fill-rule is nonzero
  FFillMode := FillModeWinding;

  StrokeWidth := UndefinedFloat;

  FStrokeOpacity := 1;
  FFillOpacity := 1;
  FLineWidth := UndefinedFloat;

  FStrokeLineJoin := '';
  FStrokeLineCap := '';
  FStrokeMiterLimit := UndefinedFloat;
  FStrokeDashOffset := UndefinedFloat;

  SetLength(FStrokeDashArray, 0);
  FArrayNone := False;

  FFontName := '';
  FFontSize := UndefinedFloat;
  FFontWeight := UndefinedInt;
  FFontStyle := UndefinedInt;

  FTextDecoration := [tdInherit];

  FreeAndNil(FPath);
end;

procedure TSVGBasic.PaintToGraphics(Graphics: TGPGraphics);
var
  Brush, StrokeBrush: TGPBrush;
  Pen: TGPPen;

  TGP: TGPMatrix;
begin
  if (GPPath = nil) {or (GPPath.GetLastStatus <> OK)} then
    Exit;

  try
    SetClipPath(Graphics);

    TGP := FullMatrix.ToGPMatrix;
    try
      Graphics.SetTransform(TGP);
    finally
      TGP.Free;
    end;

    if FStyleChanged then
      UpdateStyle;

    Brush := GetFillBrush;
    try
      StrokeBrush := GetStrokeBrush;
      Pen := GetStrokePen(StrokeBrush);

      try
        BeforePaint(Graphics, Brush, Pen);
        if Assigned(Brush) and (Brush.GetLastStatus = OK) then
          Graphics.FillPath(Brush, GPPath);

        if Assigned(Pen) and (Pen.GetLastStatus = OK) then
          Graphics.DrawPath(Pen, GPPath);

        AfterPaint(Graphics, Brush, Pen);
      finally
        Pen.Free;
        StrokeBrush.Free;
      end;
    finally
      Brush.Free;
    end;

  finally
    Graphics.ResetTransform;
    Graphics.ResetClip;
  end;
end;

procedure TSVGBasic.PaintToPath(Path: TGPGraphicsPath);
var
  M: TGPMatrix;
  I: Integer;
  Obj: TSVGObject;
begin
  for I := 0 to Count - 1 do
  begin
    Obj := TSVGBasic(Items[I]);
    Obj.PaintToPath(Path);
  end;

  if (sefHasPath in Features) and Assigned(GPPath) then
    Path.AddPath(GPPath, False);

  if not LocalMatrix.IsEmpty then
  begin
    M := LocalMatrix.ToGPMatrix;
    Path.Transform(M);
    M.Free;
  end;
end;

function TSVGBasic.ObjectBounds(IncludeStroke: Boolean; ApplyTranform: Boolean): TRectF;
begin
  Result := TRectF.Create(TPointF.Create(FX, FY), FWidth, FHeight);
end;

procedure TSVGBasic.SetClipPath(Graphics: TGPGraphics);
var
  ClipRoot: TSVGBasic;
  ClipPath: TGPGraphicsPath;
  TGP: TGPMatrix;
  ParentSVG : TSVGBasic;
  URI: string;
begin
  URI := ClipURI;
  if URI <> '' then
  begin
    ClipRoot := TSVGBasic(Root.FindByID(URI));
    if Assigned(ClipRoot) and (ClipRoot is TSVGClipPath) then
    begin
      ClipPath := TSVGClipPath(ClipRoot).ClipPath;
      ParentSVG := ClipRoot.Parent as TSVGBasic;
      if not ParentSVG.FullMatrix.IsEmpty then
      begin
        TGP := ParentSVG.FullMatrix.ToGPMatrix;
        Graphics.SetTransform(TGP);
        TGP.Free;
      end;
      Graphics.SetClip(ClipPath);
      Graphics.ResetTransform;
    end;
  end;
end;

procedure TSVGBasic.OnStyleChanged(Sender: TObject);
begin
  FStyleChanged := True;
end;

procedure TSVGBasic.UpdateStyle;
var
  LRoot: TSVG;
  C: Integer;
  Style: TStyle;
begin
  LRoot := GetRoot;
  for C := -2 to Length(FClasses) do
  begin
    case C of
      -2: Style := LRoot.FStyles.GetStyleByName(FObjectName);
      -1: Style := LRoot.FStyles.GetStyleByName('#' + FID);
      else
        begin
          if C < Length(FClasses) then
          begin
            if Assigned(LRoot) then
            begin
              Style := LRoot.FStyles.GetStyleByName(FObjectName + '.' + FClasses[C]);
              if Style = nil then
              begin
                Style := LRoot.FStyles.GetStyleByName('.' + FClasses[C]);
                if Style = nil then
                  Style := LRoot.FStyles.GetStyleByName(FClasses[C]);
              end;
            end else
              Style := nil;
          end else
            Style := FStyle;
          end;
        end;

    if Assigned(Style) then
      ReadStyle(Style);
  end;


  if FFillURI <> '' then
  begin
    if LRoot.Grayscale then
      FFillColor   := GetSVGGrayscale(GetSVGColor(FFillURI))
    else
    begin
      FFillColor   := GetSVGColor(FFillURI);
      if (LRoot.FixedColor <> SVG_INHERIT_COLOR) and
         (FFillColor <> SVG_INHERIT_COLOR) and (FFillColor <> SVG_NONE_COLOR)
      then
        FFillColor := LRoot.FixedColor;
    end;

    FFillURI := ParseURI(FFillURI);
  end;

  if FStrokeURI <> '' then
  begin
    if LRoot.Grayscale then
      FStrokeColor   := GetSVGGrayscale(GetSVGColor(FStrokeURI))
    else
    begin
      FStrokeColor   := GetSVGColor(FStrokeURI);
      if (LRoot.FixedColor <> SVG_INHERIT_COLOR) and
         (FStrokeColor <> SVG_INHERIT_COLOR) and (FStrokeColor <> SVG_NONE_COLOR)
      then
        FStrokeColor := LRoot.FixedColor;
    end;

    FStrokeURI := ParseURI(FStrokeURI);
  end;

  FStyleChanged := False;
end;

procedure TSVGBasic.ReadIn(const Reader: IXMLReader);
begin
  inherited;

  if not HasValue(FRX) and HasValue(FRY) then
  begin
    FRX := FRY;
  end;

  if not HasValue(FRY) and HasValue(FRX) then
  begin
    FRY := FRX;
  end;

  UpdateStyle;
end;

function TSVGBasic.ReadInAttr(SVGAttr: TSVGAttribute;
  const AttrValue: string): Boolean;
begin
  Result := True;
  case SVGAttr of
    saFill: FFillURI := AttrValue;
    saStroke: FStrokeURI := AttrValue;
    saX: ParseLengthAttr(AttrValue, ltHorz, FX);
    saY: ParseLengthAttr(AttrValue, ltVert, FY);
    saWidth: ParseLengthAttr(AttrValue, ltHorz, FWidth);
    saHeight: ParseLengthAttr(AttrValue, ltVert, FHeight);
    saRx: ParseLengthAttr(AttrValue, ltOther, FRX);
    saRy: ParseLengthAttr(AttrValue, ltOther, FRY);
    saFontFamily,
    saFontWeight,
    saFontSize,
    saTextDecoration,
    saFontStyle: ProcessFontAttribute(SVGAttr, AttrValue);
  else
    Result := inherited;
  end;
end;

procedure TSVGBasic.AfterPaint(const Graphics: TGPGraphics;
  const Brush: TGPBrush; const Pen: TGPPen);
begin

end;

procedure TSVGBasic.AssignTo(Dest: TPersistent);
var
  C, L: Integer;
begin
  inherited;

  if Dest is TSVGBasic then
  begin
    TSVGBasic(Dest).FFillColor := FFillColor;
    TSVGBasic(Dest).FStrokeColor := FStrokeColor;
    TSVGBasic(Dest).FFillOpacity := FFillOpacity;
    TSVGBasic(Dest).FStrokeOpacity := FStrokeOpacity;
    TSVGBasic(Dest).FStrokeWidth := FStrokeWidth;
    TSVGBasic(Dest).FStrokeLineJoin := FStrokeLineJoin;
    TSVGBasic(Dest).FStrokeLineCap := FStrokeLineCap;
    TSVGBasic(Dest).FStrokeMiterLimit := FStrokeMiterLimit;
    TSVGBasic(Dest).FStrokeDashOffset := FStrokeDashOffset;

    TSVGBasic(Dest).FFontName := FFontName;
    TSVGBasic(Dest).FFontSize := FFontSize;
    TSVGBasic(Dest).FFontWeight := FFontWeight;
    TSVGBasic(Dest).FFontStyle := FFontStyle;
    TSVGBasic(Dest).FTextDecoration := FTextDecoration;
    TSVGBasic(Dest).FFillMode := FFillMode;

    L := Length(FStrokeDashArray);
    if L > 0 then
    begin
      SetLength(TSVGBasic(Dest).FStrokeDashArray, L);
      for C := 0 to L - 1 do
        TSVGBasic(Dest).FStrokeDashArray[C] := FStrokeDashArray[C];
    end;

    TSVGBasic(Dest).FArrayNone := FArrayNone;

    if Assigned(FPath) then
      TSVGBasic(Dest).FPath := FPath.Clone;

    TSVGBasic(Dest).FRX := FRX;
    TSVGBasic(Dest).FRY := FRY;
    TSVGBasic(Dest).FFillURI := FFillURI;
    TSVGBasic(Dest).FStrokeURI := FStrokeURI;
    TSVGBasic(Dest).FClipURI := FClipURI;
    TSVGBasic(Dest).FLineWidth := FLineWidth;
    TSVGBasic(Dest).FColorInterpolation := FColorInterpolation;
    TSVGBasic(Dest).FColorRendering := FColorRendering;

    TSVGBasic(Dest).FX := FX;
    TSVGBasic(Dest).FY := FY;
    TSVGBasic(Dest).FWidth := Width;
    TSVGBasic(Dest).FHeight := Height;
  end;
end;

procedure TSVGBasic.ProcessFontAttribute(SVGAttr: TSVGAttribute; const Value: string);
begin
   case SVGAttr of
     saFontFamily: FFontName := Value;
     saFontWeight: FFontWeight := ParseFontWeight(Value);
     saFontSize: FFontSize := ParseLength(Value);
     saTextDecoration: ParseTextDecoration(Value, FTextDecoration);
     saFontStyle: FFontStyle := ParseFontStyle(Value);
   end;
end;

procedure TSVGBasic.ReadStyle(Style: TStyle);
var
  I: integer;
  Key : string;
  Value: string;
  SVGAttr: TSVGAttribute;
begin
  for I := 0 to Style.Count - 1 do
  begin
    Key := Style.Keys[I];
    if Key = '' then Continue;

    Value := Style.ValuesByNum[I];
    if Value = '' then Continue;

    if SVGAttributes.TryGetValue(Key, SVGAttr) then
    begin
      case SVGAttr of
        saStrokeWidth:
          FStrokeWidth := ParseLength(Value);
        saLineWidth:
          FLineWidth := ParseLength(Value);
        saOpacity:
          begin
            FStrokeOpacity := ParsePercent(Value);
            FFillOpacity := FStrokeOpacity;
          end;
        saStrokeOpacity:
          FStrokeOpacity := ParsePercent(Value);
        saFillOpacity:
          FFillOpacity := ParsePercent(Value);
        saColor:
          begin
            FStrokeURI := Value;
            FFillURI := Value;
          end;
        saStroke:
          FStrokeURI := Value;
        saFill:
          FFillURI := Value;
        saClipPath:
          FClipURI := ParseURI(Value, False);
        saStrokeLinejoin:
          FStrokeLineJoin := Value;
        saStrokeLinecap:
          FStrokeLineCap := Value;
        saStrokeMiterlimit:
          begin
            if not TryStrToTFloat(Value, FStrokeMiterLimit) then
              FStrokeMiterLimit := 0;
          end;
        saStrokeDashoffset:
          if not TryStrToTFloat(Value, FStrokeDashOffset) then
            FStrokeDashOffset := 0;
        saStrokeDasharray:
          SetStrokeDashArray(Value);
        saFillRule:
          if SameText(Value, 'evenodd') then
            fFillMode := FillModeAlternate
          else
            fFillMode := FillModeWinding;
        saFontFamily,
        saFontWeight,
        saFontSize,
        saTextDecoration,
        saFontStyle: ProcessFontAttribute(SVGAttr, Value);
        saDisplay:
          if Value = 'none' then
            FDisplay := tbFalse;
      end;
    end;
  end;
end;

procedure TSVGBasic.SetStrokeDashArray(const S: string);
var
  C, E: Integer;
  SL: TStringList;
  D: TFloat;
begin
  SetLength(FStrokeDashArray, 0);

  FArrayNone := False;
  if Trim(S) = 'none' then
  begin
    FArrayNone := True;
    Exit;
  end;

  SL := TStringList.Create;
  try
    SL.Delimiter := ',';
    SL.DelimitedText := S;

    for C := SL.Count - 1 downto 0 do
    begin
      SL[C] := Trim(SL[C]);
      if SL[C] = '' then
        SL.Delete(C);
    end;

    if SL.Count = 0 then
    begin
      Exit;
    end;

    if SL.Count mod 2 = 1 then
    begin
      E := SL.Count;
      for C := 0 to E - 1 do
        SL.Add(SL[C]);
    end;

    SetLength(FStrokeDashArray, SL.Count);

    for C := 0 to SL.Count - 1 do
    begin
      if not TryStrToTFloat(SL[C], D) then
        D := 0;
      FStrokeDashArray[C] := D;
    end;
  finally
    SL.Free;
  end;
end;

function TSVGBasic.GetFillBrush: TGPBrush;
var
  Color: Integer;
  Opacity: Integer;
  Filler: TSVGObject;
begin
  Result := nil;
  Color := FillColor;
  if Color = SVG_INHERIT_COLOR then
    Color := 0;
  Opacity := Round(255 * FillOpacity);

  if FFillURI <> '' then
  begin
    Filler := GetRoot.FindByID(FFillURI);
    if Assigned(Filler) and (Filler is TSVGFiller) then
      Result := TSVGFiller(Filler).GetBrush(Opacity, Self);
  end else
    if (Color <> SVG_INHERIT_COLOR) and (Color <> SVG_NONE_COLOR) then
      Result := TGPSolidBrush.Create(ConvertColor(Color, Opacity));
end;

function TSVGBasic.GetFillColor: TColor;
var
  SVG: TSVGObject;
begin
  Result := SVG_INHERIT_COLOR;
  SVG := Self;
  while Assigned(SVG) do
  begin
    if (SVG is TSVGBasic) and (TSVGBasic(SVG).FFillColor <> SVG_INHERIT_COLOR)  then
    begin
      Result := TSVGBasic(SVG).FFillColor;
      Break;
    end;
    SVG := SVG.FParent;
  end;
end;

function TSVGBasic.GetStrokeBrush: TGPBrush;
var
  Color: Integer;
  Opacity: Integer;
  Filler: TSVGObject;
begin
  Result := nil;
  Color := StrokeColor;
  Opacity := Round(255 * StrokeOpacity);

  if FStrokeURI <> '' then
  begin
    Filler := GetRoot.FindByID(FStrokeURI);
    if Assigned(Filler) and (Filler is TSVGFiller) then
      Result := TSVGFiller(Filler).GetBrush(Opacity, Self, True);
  end else
    if (Color <> SVG_INHERIT_COLOR) and (Color <> SVG_NONE_COLOR) then
      Result := TGPSolidBrush.Create(ConvertColor(Color, Opacity));
end;

function TSVGBasic.GetStrokeColor: TColor;
var
  SVG: TSVGObject;
begin
  Result := SVG_NONE_COLOR;
  SVG := Self;
  while Assigned(SVG) do
  begin
    if (SVG is TSVGBasic) and (TSVGBasic(SVG).FStrokeColor <> SVG_INHERIT_COLOR)  then
    begin
      Result := TSVGBasic(SVG).FStrokeColor;
      Break;
    end;
    SVG := SVG.FParent;
  end;
end;

function TSVGBasic.GetFillOpacity: TFloat;
var
  SVG: TSVGObject;
begin
  Result := 1;
  SVG := Self;
  while Assigned(SVG) do
  begin
    if (SVG is TSVGBasic) and HasValue(TSVGBasic(SVG).FFillOpacity) then
      Result := Result * TSVGBasic(SVG).FFillOpacity;
    SVG := SVG.FParent;
  end;
end;

function TSVGBasic.GetStrokeOpacity: TFloat;
var
  SVG: TSVGObject;
begin
  Result := 1;
  SVG := Self;
  while Assigned(SVG) do
  begin
    if (SVG is TSVGBasic) and HasValue(TSVGBasic(SVG).FStrokeOpacity) then
      Result := Result * TSVGBasic(SVG).FStrokeOpacity;
    SVG := SVG.FParent;
  end;
end;

function TSVGBasic.GetStrokePen(const StrokeBrush: TGPBrush): TGPPen;
var
  Pen: TGPPen;
  PenWidth : TFloat;
  DashArray: TSingleDynArray;
  StrokeDashCap: TDashCap;
  I: Integer;
begin
  PenWidth := GetStrokeWidth;
  if Assigned(StrokeBrush) and (StrokeBrush.GetLastStatus = OK) and (PenWidth > 0) then
  begin
    StrokeDashCap := GetStrokeDashCap;
    Pen := TGPPen.Create(0, PenWidth);
    Pen.SetLineJoin(GetStrokeLineJoin);
    Pen.SetMiterLimit(GetStrokeMiterLimit);
    Pen.SetLineCap(GetStrokeLineCap, GetStrokeLineCap, StrokeDashCap);

    DashArray := GetStrokeDashArray;
    if Length(DashArray) > 0 then
    begin
      // The length of each dash and space in the dash pattern is the product of
      // the element value in the array and the width of the Pen object.
      // https://docs.microsoft.com/en-us/windows/win32/api/gdipluspen/nf-gdipluspen-pen-setdashpattern
      // Also it appears that GDI does not adjust for DashCap
      for I := Low(DashArray) to High(DashArray) do
      begin
        DashArray[I] := DashArray[I] / PenWidth;
        if StrokeDashCap <> DashCapFlat then
        begin
          if Odd(I) then
            DashArray[I] := DashArray[I] - 1
          else
            DashArray[I] := DashArray[I] + 1;
        end;
      end;

      Pen.SetDashStyle(DashStyleCustom);
      Pen.SetDashPattern(PSingle(DashArray), Length(DashArray));
      Pen.SetDashOffset(GetStrokeDashOffset);
    end;

    Pen.SetBrush(StrokeBrush);
    Result := Pen;
  end else
    Result := nil;
end;

function TSVGBasic.GetStrokeWidth: TFloat;
var
  SVG: TSVGObject;
begin
  Result := 1;   // default
  SVG := Self;
  while Assigned(SVG) do
  begin
    if (SVG is TSVGBasic) and HasValue(TSVGBasic(SVG).FStrokeWidth) then
    begin
      Result := TSVGBasic(SVG).FStrokeWidth;
      Break;
    end;
    SVG := SVG.FParent;
  end;
end;

function TSVGBasic.GetTextDecoration: TTextDecoration;
var
  SVG: TSVGObject;
begin
  Result := [];
  SVG := Self;
  while Assigned(SVG)  do
  begin
    if (SVG is TSVGBasic) and not (tdInherit in TSVGBasic(SVG).FTextDecoration) then
    begin
      Result := TSVGBasic(SVG).FTextDecoration;
      Break;
    end;
    SVG := SVG.FParent;
  end;
end;

function TSVGBasic.IsFontAvailable: Boolean;
var
  FF: TGPFontFamily;
begin
  FF := TGPFontFamily.Create(GetFontName);
  Result :=  FF.GetLastStatus = OK;
  FF.Free;
end;

procedure TSVGBasic.ParseLengthAttr(const AttrValue: string;
  LengthType: TLengthType; var X: TFloat);
Var
  IsPercent: Boolean;
begin
  IsPercent := False;
  X := ParseLength(AttrValue, IsPercent);
  if IsPercent then
    with Root.ViewBox do
      case LengthType of
        ltHorz: X := X * Width;
        ltVert: X := X * Height;
        ltOther: X := X * Sqrt(Sqr(Width) + Sqr(Height))/Sqrt(2);
      end;
end;

function TSVGBasic.GetClipURI: string;
var
  SVG: TSVGObject;
begin
  Result := '';

  SVG := Self;
  while Assigned(SVG) do
  begin
    if (SVG is TSVGBasic) and (TSVGBasic(SVG).FClipURI <> '')  then
    begin
      Result := TSVGBasic(SVG).FClipURI;
      Break;
    end;
    SVG := SVG.FParent;
  end;
end;

function TSVGBasic.GetStrokeLineCap: TLineCap;
var
  SVG: TSVGObject;
begin
  Result := LineCapFlat;

  SVG := Self;
  while Assigned(SVG) do
  begin
    if (SVG is TSVGBasic) and (TSVGBasic(SVG).FStrokeLineCap <> '')  then
    begin
      if TSVGBasic(SVG).FStrokeLineCap = 'round' then
        Result := LineCapRound
      else if TSVGBasic(SVG).FStrokeLineCap = 'square' then
        Result := LineCapSquare;
      Break;
    end;
    SVG := SVG.FParent;
  end;
end;

function TSVGBasic.GetStrokeDashCap: TDashCap;
var
  SVG: TSVGObject;
begin
  Result := TDashCap.DashCapFlat;

  SVG := Self;
  while Assigned(SVG) do
  begin
    if (SVG is TSVGBasic) and  (TSVGBasic(SVG).FStrokeLineCap <> '') then
    begin
      if TSVGBasic(SVG).FStrokeLineCap = 'round' then
        Result := TDashCap.DashCapRound;

      Break;
    end;
    SVG := SVG.FParent;
  end;

  if Assigned(SVG) then
  begin
    if TSVGBasic(SVG).FStrokeLineCap = 'round' then
    begin
      Result := TDashCap.DashCapRound;
    end;
  end;
end;

function TSVGBasic.GetStrokeLineJoin: TLineJoin;
var
  SVG: TSVGObject;
begin
  Result := LineJoinMiterClipped;

  SVG := Self;
  while Assigned(SVG) do
  begin
    if (SVG is TSVGBasic) and (TSVGBasic(SVG).FStrokeLineJoin <> '')  then
    begin
      if TSVGBasic(SVG).FStrokeLineJoin = 'round' then
        Result := LineJoinRound
      else if TSVGBasic(SVG).FStrokeLineJoin = 'bevel' then
        Result := LineJoinBevel;
      Break;
    end;
    SVG := SVG.FParent;
  end;
end;

function TSVGBasic.GetStrokeMiterLimit: TFloat;
var
  SVG: TSVGObject;
begin
  Result := 4;

  SVG := Self;
  while Assigned(SVG) do
  begin
    if (SVG is TSVGBasic) and HasValue(TSVGBasic(SVG).FStrokeMiterLimit)  then
    begin
      Result := TSVGBasic(SVG).FStrokeMiterLimit;
      Break;
    end;
    SVG := SVG.FParent;
  end;
end;

function TSVGBasic.GetStrokeDashOffset: TFloat;
var
  SVG: TSVGObject;
begin
  Result := 0;

  SVG := Self;
  while Assigned(SVG) do
  begin
    if (SVG is TSVGBasic) and HasValue(TSVGBasic(SVG).FStrokeDashOffset) then
    begin
      Result := TSVGBasic(SVG).FStrokeDashOffset;
      Break;
    end;
    SVG := SVG.FParent;
  end;
end;

function TSVGBasic.GetStrokeDashArray: TSingleDynArray;
var
  SVG: TSVGObject;
begin
  SetLength(Result, 0);

  SVG := Self;
  while Assigned(SVG) do
  begin
    if (SVG is TSVGBasic) and ((Length(TSVGBasic(SVG).FStrokeDashArray) > 0) or
        TSVGBasic(SVG).FArrayNone) then
    begin
      if TSVGBasic(SVG).FArrayNone then Exit;
      Result := Copy(TSVGBasic(SVG).FStrokeDashArray, 0);
      Break;
    end;
    SVG := SVG.FParent;
  end;

end;

function TSVGBasic.GetFontName: string;
var
  SVG: TSVGObject;
begin
  Result := 'Arial';

  SVG := Self;
  while Assigned(SVG) do
  begin
    if  (SVG is TSVGBasic) and (TSVGBasic(SVG).FFontName <> '') then
    begin
      Result := TSVGBasic(SVG).FFontName;
      Break;
    end;
    SVG := SVG.FParent;
  end;
end;

function TSVGBasic.GetFontWeight: Integer;
var
  SVG: TSVGObject;
begin
  Result := FW_NORMAL;

  SVG := Self;
  while Assigned(SVG) do
  begin
    if (SVG is TSVGBasic) and HasValue(TSVGBasic(SVG).FFontWeight) then
    begin
      Result := TSVGBasic(SVG).FFontWeight;
      Break;
    end;
    SVG := SVG.FParent;
  end;
end;

function TSVGBasic.GetGPPath: TGPGraphicsPath2;
begin
  if (FPath = nil) and (sefHasPath in Features) then
    ConstructPath;
  Result := FPath;
end;

function TSVGBasic.GetFontSize: TFloat;
var
  SVG: TSVGObject;
begin
  Result := 11;

  SVG := Self;
  while Assigned(SVG) do
  begin
    if (SVG is TSVGBasic) and HasValue(TSVGBasic(SVG).FFontSize) then
    begin
      Result := TSVGBasic(SVG).FFontSize;
      Break;
    end;
    SVG := SVG.FParent;
  end;
end;

function TSVGBasic.GetFontStyle: Integer;
var
  SVG: TSVGObject;
begin
  Result := 0;

  SVG := Self;
  while Assigned(SVG)  do
  begin
    if (SVG is TSVGBasic) and HasValue(TSVGBasic(SVG).FFontStyle) then
    begin
      Result := TSVGBasic(SVG).FFontStyle;
      Break;
    end;
    SVG := SVG.FParent;
  end;
end;

procedure TSVGBasic.ConstructPath;
begin
  FreeAndNil(FPath);
  FPath := TGPGraphicsPath2.Create(FFillMode);
end;

{$ENDREGION}

{$REGION 'TSVG'}
procedure TSVG.LoadFromText(const Text: string);
var
  Reader: IXMLReader;
  NodeType: XmlNodeType;
  Name: PWideChar;
  Len: LongWord;
  LOldText: string;
begin
  LOldText := FSource;
  Clear;
  if Text = '' then Exit;
  try
    Reader := CreateXmlLite.Data(Text).Reader;
    while not Reader.IsEOF do
    begin
      if not EXmlLite.IsOK(Reader.Read(NodeType)) then break;
      if NodeType = XmlNodeType.Element then
      begin
        Reader.GetLocalName(Name, Len);
        try
          if Name = 'svg' then
          begin
            FSource := Text;
            ReadIn(Reader);
            break;
          end
          else
            raise Exception.Create(SVG_ERROR_TAG_SVG);
        except
          on E: Exception do
          begin
            FSource := LOldText;
            raise Exception.CreateFmt(SVG_ERROR_PARSING_SVG_TEXT, [E.Message]);
          end;
        end;
      end;
    end;
  except
    FSource := '';
    raise;
  end;
end;

procedure TSVG.LoadFromFile(const FileName: string);
var
  St: TFileStream;
begin
  St := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(St);
    FFileName := FileName;
  finally
    St.Free;
  end;
end;

procedure TSVG.LoadFromStream(Stream: TStream);
var
  Size: Integer;
  Buffer: TBytes;
begin
  Stream.Position := 0;
  Size := Stream.Size;
  SetLength(Buffer, Size);
  Stream.Read(Buffer, 0, Size);
  LoadFromText(TEncoding.UTF8.GetString(Buffer));
end;

procedure TSVG.SaveToFile(const FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TSVG.SaveToStream(Stream: TStream);
var
  Buffer: TBytes;
begin
  Buffer := TEncoding.UTF8.GetBytes(FSource);
  Stream.WriteBuffer(Buffer, Length(Buffer));
end;

procedure TSVG.PaintTo(DC: HDC; Bounds: TGPRectF;
  Rects: PRectArray; RectCount: Integer);
var
  Graphics: TGPGraphics;
begin
  Graphics := TGPGraphics.Create(DC);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    Graphics.SetPixelOffsetMode(PixelOffsetModeHalf);
    PaintTo(Graphics, Bounds, Rects, RectCount);
  finally
    Graphics.Free;
  end;
end;

procedure TSVG.PaintTo(MetaFile: TGPMetaFile; Bounds: TGPRectF;
  Rects: PRectArray; RectCount: Integer);
var
  Graphics: TGPGraphics;
begin
  Graphics := TGPGraphics.Create(MetaFile);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    Graphics.SetPixelOffsetMode(PixelOffsetModeHalf);
    PaintTo(Graphics, Bounds, Rects, RectCount);
  finally
    Graphics.Free;
  end;
end;

procedure TSVG.PaintTo(Graphics: TGPGraphics; Bounds: TGPRectF;
  Rects: PRectArray; RectCount: Integer);
var
  M: TGPMatrix;
begin
  M := TGPMatrix.Create;
  try
    Graphics.GetTransform(M);
    try
      FInitialMatrix := TAffineMatrix.FromGPMatrix(M);
      SetBounds(Bounds);

      Paint(Graphics, Rects, RectCount);
    finally
      Graphics.SetTransform(M);
    end;
  finally
    M.Free;
  end;
end;

procedure TSVG.PaintTo(DC: HDC; aLeft, aTop, aWidth, aHeight : single);
begin
  PaintTo(DC, MakeRect(aLeft, aTop, aWidth, aHeight), nil, 0);
end;

constructor TSVG.Create;
begin
  inherited;
  FStyles := TStyleList.Create;
  FillChar(FInitialMatrix, SizeOf(FInitialMatrix), 0);
  FGrayscale  := False;
  FFixedColor := SVG_INHERIT_COLOR;
  FIdDict := TDictionary<string, TSVGObject>.Create;
end;

destructor TSVG.Destroy;
begin
  FreeAndNil(FStyles);
  FreeAndNil(FidDict);
  inherited;
end;

class function TSVG.Features: TSVGElementFeatures;
begin
  Result := [sefMayHaveChildren, sefChildrenNeedPainting];
end;

function TSVG.FindByID(const Name: string): TSVGObject;
begin
   if not FIdDict.TryGetValue(Name, Result) then
   begin
     Result := inherited FindById(Name);
     FIdDict.Add(Name, Result);
   end;
end;

procedure TSVG.Clear;
begin
  inherited;

  FSource := '';

  if Assigned(FStyles) then
    FStyles.Clear;
  if Assigned(FIdDict) then
    FIdDict.Clear;

  FillChar(FViewBox, SizeOf(FViewBox), 0);
  FillChar(FInitialMatrix, SizeOf(FInitialMatrix), 0);

  FX := 0;
  FY := 0;
  FWidth := 0;
  FHeight := 0;

  FRX := 0;
  FRY := 0;

  FFillColor := SVG_INHERIT_COLOR;
  FFillURI := 'black';  //default fill color
  FFillOpacity := 1;
  FStrokeColor := SVG_NONE_COLOR;
  FStrokeWidth := 1;
  FStrokeOpacity := 1;

  FAngle := 0;
  FillChar(FAngleMatrix, SizeOf(TAffineMatrix), 0);

  FLineWidth := 1;

  FFileName := '';
end;

procedure TSVG.SetSVGOpacity(Opacity: TFloat);
begin
  StrokeOpacity := Opacity;
  FillOpacity := Opacity;
end;

procedure TSVG.SetViewBox(const Value: TRectF);
begin
  if FViewBox <> Value then
  begin
    FViewBox := Value;
    ReloadFromText;
  end;
end;

procedure TSVG.SetAngle(Angle: TFloat);
var
  X: Single;
  Y: Single;
begin
  if not SameValue(FAngle, Angle) then
  begin
    FAngle := Angle;
    X := Width / 2;
    Y := Height / 2;
    FAngleMatrix := TAffineMatrix.CreateTranslation(X, Y) * TAffineMatrix.CreateRotation(Angle) *
      TAffineMatrix.CreateTranslation(-X, -Y);
  end;
end;

procedure TSVG.SetBounds(const Bounds: TGPRectF);
begin
  FRootBounds := Bounds;

  CalculateMatrices;
end;

procedure TSVG.ReloadFromText;
var
  LSource: string;
begin
  LSource := FSource;
  LoadFromText(LSource);
end;

procedure TSVG.SetFixedColor(const Value: TColor);
begin
  if FFixedColor <> Value then
  begin
    FFixedColor := Value;
    if FFixedColor < 0 then
      FFixedColor := GetSysColor(fFixedColor and $000000FF);
    ReloadFromText;
  end;
end;

procedure TSVG.SetGrayscale(const Value: boolean);
begin
  if FGrayscale <> Value then
  begin
    FGrayscale := Value;
    ReloadFromText;
  end;
end;

procedure TSVG.Paint(const Graphics: TGPGraphics; Rects: PRectArray;
  RectCount: Integer);

  procedure PaintItem(const Item: TSVGObject);

    function NeedsPainting: Boolean;
    begin
      Result := (sefNeedsPainting in Item.Features) and
        (Item.Display = tbTrue) and (Item.Visible = tbTrue);
    end;

    function InBounds: Boolean;
    var
      C: Integer;
      Bounds: TRectF;
    begin
      Result := True;
      if RectCount > 0 then
      begin
        for C := 0 to RectCount - 1 do
        begin
          Bounds := Item.ObjectBounds(True, True);
          if Bounds.IntersectsWith(Rects^[C]) then
            Exit;
        end;
        Result := False;
      end;
    end;

  var
    C: Integer;
    LItem: TSVGObject;
  begin
    if NeedsPainting and ((RectCount = 0) or InBounds) then
      Item.PaintToGraphics(Graphics);

    if sefChildrenNeedPainting in Item.Features then
      for C := 0 to Item.Count - 1 do
      begin
        LItem := Item[C];
        if Item is TSVGBasic then PaintItem(LItem);
      end;
  end;

begin
  PaintItem(Self);
end;

procedure TSVG.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TSVG then
  begin
    TSVG(Dest).FViewBox := FViewBox;
    TSVG(Dest).FSource := Source;

    FreeAndNil(TSVG(Dest).FStyles);
    TSVG(Dest).FStyles := FStyles.Clone;
    TSVG(Dest).FFileName := FFileName;
    TSVG(Dest).FGrayscale := FGrayscale;
    TSVG(Dest).FFixedColor := FFixedColor;
  end;
end;

procedure TSVG.ReadStyles(const Reader: IXMLReader);
var
  S: string;
  Classes: TArray<string>;
  Cls: string;
  NodeType: XmlNodeType;
  pValue: PWideChar;
  Len: LongWord;
begin
  if Reader.IsEmptyElement then Exit;

  while EXmlLite.IsOK(Reader.Read(NodeType)) do
  begin
    case NodeType of
      XmlNodeType.Text,
      XmlNodeType.CDATA:
        begin
          Reader.GetValue(pValue, Len);
          SetString(S, pValue, Len);
          break
        end;
      XmlNodeType.None,
      XmlNodeType.EndElement:  Break;
    end;
  end;

  if S = '' then Exit;

  ProcessStyleSheet(S);

  Classes  := S.Split([SLineBreak], MaxInt, TStringSplitOptions.None);
  for Cls in Classes do
  begin
    S := Trim(Cls);
    if S <> '' then
      FStyles.Add(S);
  end;
end;

function TSVG.RenderToBitmap(Width, Height: Integer): HBITMAP;
var
  Bitmap: TGPBitmap;
  Graphics: TGPGraphics;
  R: TGPRectF;
begin
  Result := 0;
  if (Width = 0) or (Height = 0) then
    Exit;

  Bitmap := TGPBitmap.Create(Width, Height);
  try
    Graphics := TGPGraphics.Create(Bitmap);
    try
      Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
      Graphics.SetPixelOffsetMode(PixelOffsetModeHalf);
      R := FittedRect(MakeRect(0.0, 0.0, Width, Height), FWidth, FHeight);
      PaintTo(Graphics, R, nil, 0);
    finally
      Graphics.Free;
    end;
    Bitmap.GetHBITMAP(MakeColor(255, 255, 255), Result);
  finally
    Bitmap.Free;
  end;
end;

function TSVG.RenderToIcon(Size: Integer): HICON;
var
  Bitmap: TGPBitmap;
  Graphics: TGPGraphics;
  R: TGPRectF;
begin
  Result := 0;
  if (Size = 0) then
    Exit;

  Bitmap := TGPBitmap.Create(Size, Size);
  try
    Graphics := TGPGraphics.Create(Bitmap);
    try
      Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
      Graphics.SetPixelOffsetMode(PixelOffsetModeHalf);
      R := FittedRect(MakeRect(0.0, 0, Size, Size), Width, Height);
      PaintTo(Graphics, R, nil, 0);
    finally
      Graphics.Free;
    end;
    Bitmap.GetHICON(Result);
  finally
    Bitmap.Free;
  end;
end;

procedure TSVG.CalcMatrix;
var
  ViewBoxMatrix: TAffineMatrix;
  BoundsMatrix: TAffineMatrix;
  ScaleMatrix: TAffineMatrix;
  ScaleX, ScaleY: TFloat;
begin
  ViewBoxMatrix := TAffineMatrix.CreateTranslation(-FViewBox.Left, -FViewBox.Top);
  BoundsMatrix := TAffineMatrix.CreateTranslation(FRootBounds.X, FRootBounds.Y);

  if (FViewBox.Width > 0) and (FRootBounds.Width > 0) then
    ScaleX := (FRootBounds.Width) / FViewBox.Width
  else
    ScaleX := 1;
  if (FViewBox.Height > 0) and (FRootBounds.Height > 0) then
    ScaleY := (FRootBounds.Height)/ FViewBox.Height
  else
    ScaleY := 1;
  ScaleMatrix := TAffineMatrix.CreateScaling(ScaleX, ScaleY);

  if not FInitialMatrix.IsEmpty then
  begin
    FFullMatrix := FInitialMatrix
  end
  else
  begin
    FFullMatrix := TAffineMatrix.Identity;
  end;

  // The order is important
  // First the ViewBox transformations are applied (translate first and then scale)
  // Then the Bounds translation is applied.  (the order is from left to right)
  FFullMatrix := ViewBoxMatrix * ScaleMatrix * BoundsMatrix * FFullMatrix;
  if not FAngleMatrix.IsEmpty then
    FFullMatrix := FAngleMatrix * FFullMatrix;

  if not FLocalMatrix.IsEmpty then
    FFullMatrix := FLocalMatrix * FFullMatrix;
end;

procedure TSVG.ReadIn(const Reader: IXMLReader);
begin
  inherited;

  if FViewBox.IsEmpty then
  begin
    FViewBox.Width := FWidth;
    FViewBox.Height := FHeight;
  end;
  //Fix for SVG without width and height but with viewBox
  if (FWidth = 0) and (FHeight = 0) then
  begin
    FWidth := FViewBox.Width;
    FHeight := FViewBox.Height;
  end;

  DeReferenceUse;
end;

function TSVG.ReadInAttr(SVGAttr: TSVGAttribute; const AttrValue: string): Boolean;
{
  % width and height do not make sense in stand-alone svgs
  and they centainly do not refer to % size of the svg content
  When svg's are embedded in a web page for instance the %s
  correspond to the % of the Web page size
}
begin
  Result := True;
  case SVGAttr of
    saVersion: begin end; // ignore
    saWidth:
      begin
        if (ParseUnit(AttrValue) = suPercent) then
          FWidth := 0
        else
          ParseLengthAttr(AttrValue, ltHorz, FWidth);
      end;
    saHeight:
      begin
        if (ParseUnit(AttrValue) = suPercent) then
          FHeight := 0
        else
          ParseLengthAttr(AttrValue, ltVert, FHeight)
      end;
    saViewBox:
      begin
        if AttrValue <> '' then
          FViewBox := ParseDRect(AttrValue);
      end;
  else
    Result := inherited;
  end;
end;

procedure TSVG.DeReferenceUse;
begin
  Traverse(
    function(SVG: TSVGObject): Boolean
      begin
        Result := False;
        if SVG is TSVGUse then
          TSVGUse(SVG).Construct;
      end);
end;

function TSVG.GetStyleValue(const Name, Key: string): string;
var
  Style: TStyle;
begin
  Result := '';
  Style := FStyles.GetStyleByName(Name);
  if Assigned(Style) then
    Result := Style[Key];
end;
{$ENDREGION}

{$REGION 'TSVGContainer'}
class function TSVGContainer.Features: TSVGElementFeatures;
begin
  Result := [sefMayHaveChildren, sefChildrenNeedPainting];
end;
{$ENDREGION}

{$REGION 'TSVGDefs'}
class function TSVGDefs.Features: TSVGElementFeatures;
begin
  Result := [sefMayHaveChildren];
end;
{$ENDREGION}

{$REGION 'TSVGUse'}
procedure TSVGUse.Construct;
var
  Container: TSVGContainer;
  SVG: TSVGObject;
  Matrix: TAffineMatrix;
begin
  while Count > 0 do
    GetItem(0).Free;

  SVG := nil;
  if FReference <> '' then
    SVG := GetRoot.FindByID(FReference);

  if Assigned(SVG) then
  begin
    Matrix := TAffineMatrix.CreateTranslation(X, Y);

    Container := TSVGContainer.Create(Self);
    Container.FObjectName := 'g';
    Container.FLocalMatrix := Matrix;
    SVG.Clone(Container);

    Container.Traverse(
    function(SVG: TSVGObject): Boolean
      begin
        Result := False;
        if SVG is TSVGUse then
          TSVGUse(SVG).Construct;
      end);
  end;
end;

class function TSVGUse.Features: TSVGElementFeatures;
begin
  Result := [sefChildrenNeedPainting];
end;

procedure TSVGUse.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TSVGUse then
  begin
    TSVGUse(Dest).FReference := FReference;
  end;
end;

procedure TSVGUse.Clear;
begin
  inherited;
  FReference := '';
end;


function TSVGUse.ReadInAttr(SVGAttr: TSVGAttribute;
  const AttrValue: string): Boolean;
begin
  Result := True;
  if SVGAttr = saXlinkHref then FReference := AttrValue
  else if SVGAttr = saHref then FReference := AttrValue
  else
    Result := inherited;
end;
{$ENDREGION}

{$REGION 'TSVGRect'}

procedure TSVGRect.ReadIn(const Reader: IXMLReader);
begin
  inherited;

  if FRX > FWidth / 2 then
    FRX := FWidth / 2;

  if FRY > FHeight / 2 then
    FRY := FHeight / 2;
end;

function TSVGRect.ObjectBounds(IncludeStroke: Boolean; ApplyTranform: Boolean): TRectF;
var
  SW: TFloat;
begin
  if IncludeStroke then
    SW := Max(0, GetStrokeWidth) / 2
  else
    SW := 0;

  Result.TopLeft := TPointF.Create(FX - SW, FY - SW);
  Result.BottomRight := TPointF.Create(FX + FWidth + SW, FY + Height + SW);

  if ApplyTranform then begin
    Result.TopLeft := Transform(Result.TopLeft);
    Result.BottomRight := Transform(Result.BottomRight);
  end;
end;

procedure TSVGRect.ConstructPath;
begin
  inherited;

  if (FRX <= 0) and (FRY <= 0) then
    FPath.AddRectangle(MakeRect(FX, FY, FWidth, FHeight))
  else
    FPath.AddRoundRect(FX, FY, FWidth, FHeight, FRX, FRY);
end;
{$ENDREGION}

{$REGION 'TSVGLine'}
function TSVGLine.ReadInAttr(SVGAttr: TSVGAttribute;
  const AttrValue: string): Boolean;
begin
  Result := True;
  case SVGAttr of
    saX1: ParseLengthAttr(AttrValue, ltHorz, FX);
    saY1: ParseLengthAttr(AttrValue, ltVert, FY);
    saX2: ParseLengthAttr(AttrValue, ltHorz, FWidth);
    saY2: ParseLengthAttr(AttrValue, ltVert, FHeight);
  else
    Result := inherited;
  end;
end;

function TSVGLine.ObjectBounds(IncludeStroke: Boolean; ApplyTranform: Boolean): TRectF;
var
  SW: TFloat;
  Left, Top, Right, Bottom: TFloat;
begin
  if IncludeStroke then
    SW := Max(0, GetStrokeWidth) / 2
  else
    SW := 0;
  Left := Min(X, Width) - SW;
  Top := Min(Y, Height) - SW;
  Right := Max(X, Width) + SW;
  Bottom := Max(Y, Height) + SW;

  Result.TopLeft := TPointF.Create(Left, Top);
  Result.BottomRight := TPointF.Create(Right, Bottom);

  if ApplyTranform then begin
    Result.TopLeft := Transform(Result.TopLeft);
    Result.BottomRight := Transform(Result.BottomRight);
  end;
end;

procedure TSVGLine.ConstructPath;
begin
  inherited;
  FPath.AddLine(X, Y, Width, Height);
end;
{$ENDREGION}

{$REGION 'TSVGPolyLine'}
constructor TSVGPolyLine.Create;
begin
  inherited;
  FPointCount := 0;
end;

function TSVGPolyLine.ObjectBounds(IncludeStroke: Boolean; ApplyTranform: Boolean): TRectF;
var
  Left, Top, Right, Bottom: TFloat;
  C: Integer;
  SW: TFloat;
begin
  Left := MaxTFloat;
  Top := MaxTFloat;
  Right := -MaxTFloat;
  Bottom := -MaxTFloat;
  for C := 0 to FPointCount - 1 do
  begin
    if FPoints[C].X < Left then
      Left := FPoints[C].X;

    if FPoints[C].X > Right then
      Right := FPoints[C].X;

    if FPoints[C].Y < Top then
      Top := FPoints[C].Y;

    if FPoints[C].Y > Bottom then
      Bottom := FPoints[C].Y;
  end;

  if IncludeStroke then
    SW := Max(0, GetStrokeWidth) / 2
  else
    SW := 0;

  Result.TopLeft := TPointF.Create(Left - SW, Top - SW);
  Result.BottomRight := TPointF.Create(Right + SW, Bottom + SW);

  if ApplyTranform then begin
    Result.TopLeft := Transform(Result.TopLeft);
    Result.BottomRight := Transform(Result.BottomRight);
  end;
end;

procedure TSVGPolyLine.Clear;
begin
  inherited;

  SetLength(FPoints, 0);
  FPointCount := 0;
end;

procedure TSVGPolyLine.AssignTo(Dest: TPersistent);
var
  C: Integer;
begin
  inherited;
  if Dest is TSVGPolyLine then
  begin
    TSVGPolyLine(Dest).FPointCount := FPointCount;

    if Assigned(FPoints) then
    begin
      SetLength(TSVGPolyLine(Dest).FPoints, FPointCount);
      for C := 0 to FPointCount - 1 do
      begin
        TSVGPolyLine(Dest).FPoints[C].X := FPoints[C].X;
        TSVGPolyLine(Dest).FPoints[C].Y := FPoints[C].Y;
      end;
    end;
  end;
end;

procedure TSVGPolyLine.ConstructPoints(const S: string);
var
  SL: TStrings;
  C: Integer;
begin
  SL := TStringList.Create;
  SL.Delimiter := ' ';
  SL.DelimitedText := S;

  for C := SL.Count - 1 downto 0 do
    if SL[C] = '' then
      SL.Delete(C);

  if SL.Count mod 2 = 1 then
  begin
    SL.Free;
    Exit;
  end;

  SetLength(FPoints, 0);

  FPointCount := SL.Count div 2;
  SetLength(FPoints, FPointCount);

  for C := 0 to FPointCount - 1 do
  begin
    if not TryStrToTFloat(SL[C * 2], FPoints[C].X) then
      FPoints[C].X := 0;
    if not TryStrToTFloat(SL[C * 2 + 1], FPoints[C].Y) then
      FPoints[C].Y := 0;
  end;

  SL.Free;
end;

function TSVGPolyLine.ReadInAttr(SVGAttr: TSVGAttribute;
  const AttrValue: string): Boolean;
var
  S: string;
begin
  Result := True;
  if SVGAttr = saPoints then
  begin
    S := AttrValue;
    S := StringReplace(S, ',', ' ', [rfReplaceAll]);
    S := StringReplace(S, '-', ' -', [rfReplaceAll]);

    ConstructPoints(S);
  end
  else
    Result := inherited;
end;

procedure TSVGPolyLine.ConstructPath;
var
  C: Integer;
begin
  if FPoints = nil then
    Exit;
  inherited;

  for C := 1 to FPointCount - 1 do
    FPath.AddLine(FPoints[C - 1].X, FPoints[C - 1].Y, FPoints[C].X, FPoints[C].Y);
end;
{$ENDREGION}

{$REGION 'TSVGPolygon'}
procedure TSVGPolygon.ConstructPath;
begin
  if FPoints = nil then
    Exit;
  inherited;

  FPath.CloseFigure;
end;
{$ENDREGION}

{$REGION 'TSVGEllipse'}
function TSVGEllipse.ReadInAttr(SVGAttr: TSVGAttribute;
  const AttrValue: string): Boolean;
begin
  Result := True;
  case SVGAttr of
    saCx: ParseLengthAttr(AttrValue, ltHorz, FCX);
    saCy: ParseLengthAttr(AttrValue, ltVert, FCY);
    saR:
      begin
        if FObjectName = 'circle' then
        begin
          ParseLengthAttr(AttrValue, ltOther,  FRX);
          FRY := FRX;
        end;
      end;
  else
    Result := inherited;
  end;
end;

function TSVGEllipse.ObjectBounds(IncludeStroke: Boolean; ApplyTranform: Boolean): TRectF;
var
  SW: TFloat;
begin
  if IncludeStroke then
    SW := Max(0, GetStrokeWidth) / 2
  else
    SW := 0;

  Result.TopLeft := TPointF.Create(FCX - FRX - SW, FCY - FRY - SW);
  Result.BottomRight := TPointF.Create(FCX + FRX + SW, FCY + FRY + SW);

  if ApplyTranform then begin
    Result.TopLeft := Transform(Result.TopLeft);
    Result.BottomRight := Transform(Result.BottomRight);
  end;
end;

procedure TSVGEllipse.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TSVGEllipse then
  begin
    TSVGEllipse(Dest).FCX := FCX;
    TSVGEllipse(Dest).FCY := FCY;
  end;
end;

procedure TSVGEllipse.Clear;
begin
  inherited;
  FCX := UndefinedFloat;
  FCY := UndefinedFloat;
end;

procedure TSVGEllipse.ConstructPath;
begin
  inherited;
  FPath.AddEllipse(FCX - FRX, FCY - FRY, 2 * FRX, 2 * FRY);
end;
{$ENDREGION}

{$REGION 'TSVGPath'}
function TSVGPath.ObjectBounds(IncludeStroke: Boolean; ApplyTranform: Boolean): TRectF;
var
  C: Integer;
  R: TRectF;
  Left, Top, Right, Bottom: TFloat;
  Found: Boolean;
  SW: TFloat;
begin
  Left := MaxTFloat;
  Top := MaxTFloat;
  Right := -MaxTFloat;
  Bottom := -MaxTFloat;
  Found := False;

  for C := 0 to Count - 1 do
  begin
    R := TSVGPathElement(Items[C]).GetBounds;
    if (R.Width <> 0) or (R.Height <> 0) then
    begin
      Found := True;
      Left := Min(Left, R.Left);
      Top := Min(Top, R.Top);
      Right := Max(Right, R.Left + R.Width);
      Bottom := Max(Bottom, R.Top + R.Height);
    end;
  end;

  if not Found then
  begin
    Left := 0;
    Top := 0;
    Right := 0;
    Bottom := 0;
  end;

  if IncludeStroke then
    SW := Max(0, GetStrokeWidth) / 2
  else
    SW := 0;

  Result.TopLeft := TPointF.Create(Left - SW, Top - SW);
  Result.BottomRight := TPointF.Create(Right + SW, Bottom + SW);

  if ApplyTranform then begin
    Result.TopLeft := Transform(Result.TopLeft);
    Result.BottomRight := Transform(Result.BottomRight);
  end;
end;

procedure TSVGPath.ConstructPath;
var
  C: Integer;
  Element: TSVGPathElement;
begin
  inherited;

  for C := 0 to Count - 1 do
  begin
    Element := TSVGPathElement(Items[C]);
    Element.AddToPath(FPath);
  end;
end;

function TSVGPath.SeparateValues(const ACommand: Char;
  const S: string; CommandList: TList<Char>;  ValueList: TList<TFloat>): Boolean;
var
  I, NumStart: Integer;
  HasDot: Boolean;
  HasExp: Boolean;
  OldCount: Integer;
  AddedCount: Integer;
  ExpectedValueCount: Integer;
  RepeatCommand: Char;
begin
  OldCount := ValueList.Count;

  HasDot := False;
  HasExp := False;
  NumStart := 1;

  for I := 1 to S.Length do
  begin
    case S[I] of
      '.':
        begin
          if HasDot or HasExp then
          begin
            ValueList.Add(StrToTFloat(Copy(S, NumStart, I - NumStart)));
            NumStart := I;
            HasExp := False;
          end;
          HasDot := True;
        end;
      '0'..'9': ;
      '+', '-':
        begin
          if I > NumStart then
          begin
            if not HasExp or (UpCase(S[I-1]) <> 'E') then begin
              ValueList.Add(StrToTFloat(Copy(S, NumStart, I-NumStart)));
              HasDot := False;
              HasExp := False;
              NumStart := I;
            end
          end;
        end;
      'E', 'e':
        HasExp := True;
      ' ', ',', #9, #$A, #$D:
        begin
          if I > NumStart then
          begin
            ValueList.Add(StrToTFloat(Copy(S, NumStart, I-NumStart)));
            HasDot := False;
            HasExp := False;
          end;
          NumStart := I + 1;
        end;
    end;
  end;

  if S.Length  + 1 > NumStart then
  begin
    ValueList.Add(StrToTFloat(Copy(S, NumStart, S.Length + 1 - NumStart)));
  end;

  AddedCount := ValueList.Count - OldCount;

  case AnsiChar(ACommand) of
    'A', 'a':                     ExpectedValueCount := 7;
    'C', 'c':                     ExpectedValueCount := 6;
    'S', 's', 'Q', 'q':           ExpectedValueCount := 4;
    'T', 't', 'M', 'm', 'L', 'l': ExpectedValueCount := 2;
    'H', 'h', 'V', 'v':           ExpectedValueCount := 1;
  else
    ExpectedValueCount := 0;
  end;

  Result := ((ExpectedValueCount = 0) and (AddedCount = 0)) or
    ((AddedCount >= ExpectedValueCount) and (AddedCount mod ExpectedValueCount = 0));

  if Result then
  begin
    CommandList.Add(ACommand);
    if AddedCount > ExpectedValueCount then
    begin
      case ACommand of
        'M': RepeatCommand := 'L';
        'm': RepeatCommand := 'l';
      else
        RepeatCommand := ACommand;
      end;
      for I := 2 to AddedCount div ExpectedValueCount do
        CommandList.Add(RepeatCommand);
    end;
  end;
end;

procedure TSVGPath.Split(const S: string; CommandList: TList<Char>;  ValueList: TList<TFloat>);
var
  Part: string;
  Found: Integer;
  StartIndex: Integer;
  SLength: Integer;
const
  IDs: TSysCharSet = ['M', 'm', 'L', 'l', 'H', 'h', 'V', 'v',
    'C', 'c', 'S', 's', 'Q', 'q', 'T', 't', 'A', 'a', 'Z', 'z'];
begin
  StartIndex := 1;
  SLength := Length(S);
  while StartIndex <= SLength do
  begin
    if not (AnsiChar(S[StartIndex]) in IDs) then Exit;  // invalid path
    Found := StartIndex + 1;
    while (Found <= SLength) and not (AnsiChar(S[Found]) in IDs) do
      Inc(Found);
    Part := Trim(Copy(S, StartIndex + 1, Found - StartIndex -1));
    if not SeparateValues(S[StartIndex], Part, CommandList, ValueList) then
      Break;
    StartIndex := Found;
  end;
end;

function TSVGPath.ReadInAttr(SVGAttr: TSVGAttribute;
  const AttrValue: string): Boolean;
var
  C: Integer;
  Command: Char;
  VListPos: Integer;

  CommandList: TList<Char>;
  ValueList: TList<TFloat>;

  Element: TSVGPathElement;
  LastElement: TSVGPathElement;
begin
  Result := True;
  if SVGAttr = saD then
  begin
    if AttrValue.Length = 0 then
      Exit;
  end else
  begin
    Result := inherited;
    Exit;
  end;

  CommandList := TList<Char>.Create;
  ValueList := TList<TFloat>.Create;
  try
    Split(AttrValue, CommandList, ValueList);

    LastElement := nil;
    VListPos := 0;

    for C := 0 to CommandList.Count-1 do
    begin
      Command := CommandList[C];

      case AnsiChar(Command) of
        'M', 'm': Element := TSVGPathMove.Create(Self);

        'L', 'l': Element := TSVGPathLine.Create(Self);

        'H', 'h', 'V', 'v': Element := TSVGPathLine.Create(Self);

        'C', 'c': Element := TSVGPathCurve.Create(Self);

        'S', 's', 'Q', 'q': Element := TSVGPathCurve.Create(Self);

        'T', 't': Element := TSVGPathCurve.Create(Self);

        'A', 'a': Element := TSVGPathEllipticArc.Create(Self);

        'Z', 'z': Element := TSVGPathClose.Create(Self);

      else
        Element := nil;
      end;

      if Assigned(Element) then
      begin
        Element.Read(Command, ValueList, VListPos, LastElement);
        LastElement := Element;
      end;
    end;
  finally
    CommandList.Free;
    ValueList.Free;
  end;
end;

{$ENDREGION}

{$REGION 'TSVGImage'}
constructor TSVGImage.Create;
begin
  inherited;
  FImage := nil;
  FStream := nil;
end;

class function TSVGImage.Features: TSVGElementFeatures;
begin
  Result := [sefNeedsPainting];
end;

function TSVGImage.ObjectBounds(IncludeStroke: Boolean; ApplyTranform: Boolean): TRectF;
var
  SW: TFloat;
begin
  if IncludeStroke then
    SW := Max(0, GetStrokeWidth) / 2
  else
    SW := 0;

  Result.TopLeft := TPointF.Create(X - SW, Y - SW);
  Result.BottomRight := TPointF.Create(X + Width + SW, Y + Height + SW);

  if ApplyTranform then begin
    Result.TopLeft := Transform(Result.TopLeft);
    Result.BottomRight := Transform(Result.BottomRight);
  end;
end;

procedure TSVGImage.Clear;
begin
  inherited;
  FreeAndNil(FImage);
  FreeAndNil(FStream);
  FFileName := '';
end;

procedure TSVGImage.AssignTo(Dest: TPersistent);
var
  SA: IStream;
begin
  if Dest is TSVGImage then
  begin
    TSVGImage(Dest).FFileName := FFileName;
    if Assigned(TSVGImage(Dest).FStream) then
      TSVGImage(Dest).FStream.Free;
    TSVGImage(Dest).FStream := TMemoryStream.Create;
    if Assigned(FStream) then
    begin
      FStream.Position := 0;
      TSVGImage(Dest).FStream.LoadFromStream(FStream);
      TSVGImage(Dest).FStream.Position := 0;
      SA := TStreamAdapter.Create(TSVGImage(Dest).FStream, soReference);
      FImage := TGPImage.Create(SA);
    end
    else
    begin
      TSVGImage(Dest).FStream.LoadFromFile(FFileName);
      TSVGImage(Dest).FStream.Position := 0;
      SA := TStreamAdapter.Create(TSVGImage(Dest).FStream, soReference);
      FImage := TGPImage.Create(SA);
    end;
  end
  else
    inherited;
end;

procedure TSVGImage.PaintToGraphics(Graphics: TGPGraphics);
var
  TGP: TGPMatrix;
  ImAtt: TGPImageAttributes;
  ColorMatrix: TColorMatrix;

begin
  if FImage = nil then
    Exit;

  SetClipPath(Graphics);

  TGP := FullMatrix.ToGPMatrix;
  Graphics.SetTransform(TGP);
  TGP.Free;

  FillChar(ColorMatrix, Sizeof(ColorMatrix), 0);
  ColorMatrix[0, 0] := 1;
  ColorMatrix[1, 1] := 1;
  ColorMatrix[2, 2] := 1;
  ColorMatrix[3, 3] := GetFillOpacity;
  ColorMatrix[4, 4] := 1;

  ImAtt := TGPImageAttributes.Create;
  ImAtt.SetColorMatrix(colorMatrix, ColorMatrixFlagsDefault,
    ColorAdjustTypeDefault);

  Graphics.DrawImage(FImage, MakeRect(X, Y, Width, Height),
    0, 0, FImage.GetWidth, FImage.GetHeight, UnitPixel, ImAtt);

  ImAtt.Free;

  Graphics.ResetTransform;
  Graphics.ResetClip;
end;

procedure TSVGImage.ReadIn(const Reader: IXMLReader);

  function IsValid(var S: string): Boolean;
  var
    Semicolon: Integer;
  begin
    Result := False;
    if StartsStr('data:', S) then
    begin
      S := Copy(S, 6, MaxInt);
      Semicolon := Pos(';', S);
      if Semicolon = 0 then
        Exit;
      if Copy(S, Semicolon, 8) = ';base64,' then
      begin
        S := Copy(S, Semicolon + 8, MaxInt);
        Result := True;
      end;
    end;
  end;

var
  S: string;
  SA: IStream;
  SS: TStringStream;
  {$IF CompilerVersion < 28}
  Decoder64: TIdDecoderMIME;
  {$IFEND}
begin
  inherited;

  S := FImageURI;

  if IsValid(S) then
  begin
    SS := TStringStream.Create(S);
    try
      FStream := TMemoryStream.Create;
      {$IF CompilerVersion > 27}
      TNetEncoding.Base64.Decode(SS, FStream);
      {$ELSE}
        Decoder64 := TIdDecoderMIME.Create(nil);
        Try
          Decoder64.DecodeStream(S, FStream);
        Finally
          Decoder64.Free;
        End;
      {$IFEND}
      FStream.Position := 0;
      SA := TStreamAdapter.Create(FStream, soReference);
      FImage := TGPImage.Create(SA);
      FImage.GetLastStatus;
    finally
      SS.Free;
    end;
  end
  else if FileExists(S) then
  begin
    FFileName := S;
    FStream := TMemoryStream.Create;
    FStream.LoadFromFile(FFileName);
    FStream.Position := 0;
    SA := TStreamAdapter.Create(FStream, soReference);
    FImage := TGPImage.Create(SA);
    FImage.GetLastStatus;
  end;
end;

function TSVGImage.ReadInAttr(SVGAttr: TSVGAttribute;
  const AttrValue: string): Boolean;
begin
  Result := True;
  if SVGAttr = saXlinkHref then FImageURI := AttrValue
  else if SVGAttr = saHref then FImageURI := AttrValue
  else
    Result := inherited;
end;
{$ENDREGION}

{$REGION 'TSVGCustomText'}
constructor TSVGCustomText.Create;
begin
  inherited;
  FDX := 0;
  FDY := 0;
end;

class function TSVGCustomText.Features: TSVGElementFeatures;
begin
  // It has children but it needs special treatment
  Result := [sefNeedsPainting, sefHasPath, sefChildrenNeedPainting];
end;

procedure TSVGCustomText.BeforePaint(const Graphics: TGPGraphics;
  const Brush: TGPBrush; const Pen: TGPPen);
begin
  inherited;
  if Assigned(FUnderlinePath) then
  begin
    if Assigned(Brush) and (Brush.GetLastStatus = OK) then
    begin
      Graphics.FillPath(Brush, FUnderlinePath);
    end;

    if Assigned(Pen) and (Pen.GetLastStatus = OK) then
    begin
      Graphics.DrawPath(Pen, FUnderlinePath);
    end;
  end;
end;

function TSVGCustomText.ObjectBounds(IncludeStroke: Boolean; ApplyTranform: Boolean): TRectF;
var
  SW: TFloat;
begin
  if IncludeStroke then
    SW := Max(0, GetStrokeWidth) / 2
  else
    SW := 0;

  Result.TopLeft := TPointF.Create(X - SW, Y - FFontHeight - SW);
  Result.BottomRight := TPointF.Create(X + Width + SW, Y - FFontHeight + Height + SW);

  if ApplyTranform then begin
    Result.TopLeft := Transform(Result.TopLeft);
    Result.BottomRight := Transform(Result.BottomRight);
  end;
end;

procedure TSVGCustomText.Clear;
begin
  inherited;
  FreeAndNil(FUnderlinePath);
  FreeAndNil(FStrikeOutPath);
  FText := '';
  FFontHeight := 0;
  FDX := 0;
  FDY := 0;
  FHasX := False;
  FHasY := False;
end;

function TSVGCustomText.GetCompleteWidth: TFloat;
var
  C: Integer;
begin
  Result := Width;
  for C := 0 to Count - 1 do
  begin
    if GetItem(C) is TSVGCustomText then
    begin
      Result := Result + TSVGCustomText(GetItem(C)).GetCompleteWidth;
    end;
  end;
end;

function TSVGCustomText.GetFont: TGPFont;
var
  FF: TGPFontFamily;
  FontStyle: Winapi.GDIPAPI.TFontStyle;
  TD: TTextDecoration;
//  Font: HFont;

{  function CreateFont: HFont;
  var
    LogFont: TLogFont;
  begin
    with LogFont do
    begin
      lfHeight := Round(GetFont_Size);
      lfWidth := 0;
      lfEscapement := 0;
      lfOrientation := 0;
      lfWeight := GetFont_Weight;

      lfItalic := GetFont_Style;

      TD := GetText_Decoration;

      if tdUnderLine in TD then
        lfUnderline := 1
      else
        lfUnderline := 0;

      if tdStrikeOut in TD then
        lfStrikeOut := 1
      else
        lfStrikeOut := 0;

      lfCharSet := 1;
      lfOutPrecision := OUT_DEFAULT_PRECIS;
      lfClipPrecision := CLIP_DEFAULT_PRECIS;
      lfQuality := DEFAULT_QUALITY;
      lfPitchAndFamily := DEFAULT_PITCH;
      StrPCopy(lfFaceName, GetFont_Name);
    end;
    Result := CreateFontIndirect(LogFont);
  end;}

begin
  FF := GetFontFamily(GetFontName);

  FontStyle := FontStyleRegular;
  if GetFontWeight = FW_BOLD then
    FontStyle := FontStyle or FontStyleBold;

  if GetFontStyle = 1 then
    FontStyle := FontStyle or FontStyleItalic;

  TD := GetTextDecoration;

  if tdUnderLine in TD then
    FontStyle := FontStyle or FontStyleUnderline;

  if tdStrikeOut in TD then
    FontStyle := FontStyle or FontStyleStrikeout;

  FFontHeight := FF.GetCellAscent(FontStyle) / FF.GetEmHeight(FontStyle);
  FFontHeight := FFontHeight * GetFontSize;

  Result := TGPFont.Create(FF, GetFontSize, FontStyle, UnitPixel);
  FF.Free;
end;

function TSVGCustomText.GetFontFamily(const FontName: string): TGPFontFamily;
var
  FF: TGPFontFamily;
  C: Integer;
  FN: string;
begin
  FF := TGPFontFamily.Create(FontName);
  if FF.GetLastStatus <> OK then
  begin
    FreeAndNil(FF);

    C := Pos('-', FontName);
    if (C <> 0) then
    begin
      FN := Copy(FontName, 1, C - 1);
      FF := TGPFontFamily.Create(FN);
      if FF.GetLastStatus <> OK then
        FreeAndNil(FF);
    end;
  end;
  if not Assigned(FF) then
    FF := TGPFontFamily.Create('Arial');

  Result := FF;
end;

function TSVGCustomText.IsInTextPath: Boolean;
var
  Item: TSVGObject;
begin
  Result := True;
  Item := Self;
  while Assigned(Item) do
  begin
    if Item is TSVGTextPath then
      Exit;
    Item := Item.Parent;
  end;
  Result := False;
end;

procedure TSVGCustomText.SetSize;
var
  Graphics: TGPGraphics;
  SF: TGPStringFormat;
  Font: TGPFont;
  Rect: TGPRectF;
  Index: Integer;
  Previous: TSVGCustomText;
  DC: HDC;
begin
  DC := GetDC(0);
  Graphics := TGPGraphics.Create(DC);

  Font := GetFont;

  SF := TGPStringFormat.Create(StringFormatFlagsMeasureTrailingSpaces);

  Graphics.MeasureString(FText, -1, Font, MakePoint(0.0, 0), SF, Rect);

  Rect.Width := KerningText.MeasureText(FText, Font);

  SF.Free;

  Graphics.Free;
  ReleaseDC(0, DC);

  Font.Free;

  FWidth := 0;
  FHeight := 0;

  if Assigned(FParent) and (FParent is TSVGCustomText) then
  begin
    Index := FParent.IndexOf(Self);

    Previous := nil;
    if (Index > 0) and (FParent[Index - 1] is TSVGCustomText) then
      Previous := TSVGCustomText(FParent[Index - 1]);

    if (Index = 0) and (FParent is TSVGCustomText) then
      Previous := TSVGCustomText(FParent);

    if Assigned(Previous) then
    begin
      if not FHasX then
        FX := Previous.X + Previous.GetCompleteWidth;

      if not FHasY then
        FY := Previous.Y;
    end;
  end;

  FX := FX + FDX;
  FY := FY + FDY;

  FWidth := Rect.Width;
  FHeight := Rect.Height;
end;

procedure TSVGCustomText.AfterPaint(const Graphics: TGPGraphics;
  const Brush: TGPBrush; const Pen: TGPPen);
begin
  inherited;
  if Assigned(FStrikeOutPath) then
  begin
    if Assigned(Brush) and (Brush.GetLastStatus = OK) then
      Graphics.FillPath(Brush, FStrikeOutPath);

    if Assigned(Pen) and (Pen.GetLastStatus = OK) then
      Graphics.DrawPath(Pen, FStrikeOutPath);
  end;
end;

procedure TSVGCustomText.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TSVGCustomText then
  begin
    TSVGCustomText(Dest).FText := FText;
    TSVGCustomText(Dest).FFontHeight := FFontHeight;
    TSVGCustomText(Dest).FDX := FDX;
    TSVGCustomText(Dest).FDY := FDY;
  end;
end;

procedure TSVGCustomText.ConstructPath;
var
  FF: TGPFontFamily;
  FontStyle: Winapi.GDIPAPI.TFontStyle;
  SF: TGPStringFormat;
  TD: TTextDecoration;
begin
  FreeAndNil(FUnderlinePath);
  FreeAndNil(FStrikeOutPath);

  if IsInTextPath then
    Exit;

  if FText = '' then
    Exit;

  inherited;

  FF := GetFontFamily(GetFontName);

  FontStyle := FontStyleRegular;
  if FFontWeight = FW_BOLD then
    FontStyle := FontStyle or FontStyleBold;

  if GetFontStyle = 1 then
    FontStyle := FontStyle or FontStyleItalic;

  TD := GetTextDecoration;

  if tdUnderLine in TD then
  begin
    FontStyle := FontStyle or FontStyleUnderline;
    FUnderlinePath := TGPGraphicsPath.Create;
  end;

  if tdStrikeOut in TD then
  begin
    FontStyle := FontStyle or FontStyleStrikeout;
    FStrikeOutPath := TGPGraphicsPath.Create;
  end;

  SF := TGPStringFormat.Create(TGPStringFormat.GenericTypographic);
  SF.SetFormatFlags(StringFormatFlagsMeasureTrailingSpaces);

  KerningText.AddToPath(FPath, FUnderlinePath, FStrikeOutPath,
    FText, FF, FontStyle, GetFontSize,
    MakePoint(X, Y - FFontHeight), SF);

  SF.Free;
  FF.Free;
end;

procedure TSVGCustomText.PaintToGraphics(Graphics: TGPGraphics);
{$IFDEF USE_TEXT}
var
  Font: TGPFont;
  SF: TGPStringFormat;
  Brush: TGPBrush;

  TGP: TGPMatrix;
{$ENDIF}
begin
{$IFDEF USE_TEXT}
  // not tested

  if FText = '' then
    Exit;

  try
    SetClipPath(Graphics);

    TGP := Matrix.ToGPMatrix;
    Graphics.SetTransform(TGP);
    TGP.Free;

    SF := TGPStringFormat.Create(TGPStringFormat.GenericTypographic);
    SF.SetFormatFlags(StringFormatFlagsMeasureTrailingSpaces);

    Brush := GetFillBrush;
    if Assigned(Brush) and (Brush.GetLastStatus = OK) then
    try
      Font := GetFont;
      try
        KerningText.AddToGraphics(Graphics, FText, Font, MakePoint(X, Y - FFontHeight), SF, Brush);
      finally
        Font.Free;
      end;
    finally
      Brush.Free;
    end;

    SF.Free;
  finally
    Graphics.ResetTransform;
    Graphics.ResetClip;
  end;
{$ELSE}
  inherited;
{$ENDIF}
end;

procedure TSVGCustomText.ParseNode(const Reader: IXMLReader);
var
  TSpan: TSVGTSpan;
  TextPath: TSVGTextPath;
  pName: PWidechar;
  pValue: PWidechar;
  Len: LongWord;
  NodeName: string;
  NodeType: XmlNodeType;
begin
  Reader.GetNodeType(NodeType);

  if NodeType = XmlNodeType.Text then
  begin
    Reader.GetValue(pValue, Len);
    TSpan := TSVGTSpan.Create(Self);
    TSpan.Assign(Self);
    FillChar(TSpan.FLocalMatrix, SizeOf(TSpan.FLocalMatrix), 0);
    SetString(TSpan.FText, pValue, Len);
    TSpan.SetSize;
  end
  else if NodeType = XmlNodeType.Element then
  begin
    Reader.GetLocalName(pName, Len);
    SetString(NodeName, pName, Len);
    if NodeName = 'tspan' then
    begin
      TSpan := TSVGTSpan.Create(Self);
      TSpan.Assign(Self);
      FillChar(TSpan.FLocalMatrix, SizeOf(TSpan.FLocalMatrix), 0);
      TSpan.SetSize;
      TSpan.ReadIn(Reader);
    end
    else if NodeName = 'textPath' then
    begin
      TextPath := TSVGTextPath.Create(Self);
      TextPath.Assign(Self);
      FillChar(TextPath.FLocalMatrix, SizeOf(TextPath.FLocalMatrix), 0);
      TextPath.ReadIn(Reader);
    end;
  end;
end;

procedure TSVGCustomText.ReadIn(const Reader: IXMLReader);

  procedure ProcessFontName;
  var
    Bold, Italic: Integer;
    FN: string;
  begin
    if FFontName = '' then Exit;

    Bold := Pos('Bold', FFontName);
    Italic := Pos('Italic', FFontName);

    FN := FFontName;

    // Check for Bold
    if Bold <> 0 then
    begin
      FFontName := Copy(FN, 1, Bold - 1) + Copy(FN, Bold + 4, MaxInt);
      if Copy(FFontName, Length(FFontName), 1) = '-' then
        FFontName := Copy(FFontName, 1, Length(FFontName) - 1);
      if IsFontAvailable then
      begin
        FFontWeight := FW_BOLD;
        Exit;
      end;
      if Copy(FFontName, Length(FFontName) - 1, 2) = 'MT' then
      begin
        FFontName := Copy(FFontName, 1, Length(FFontName) - 2);
        if Copy(FFontName, Length(FFontName), 1) = '-' then
          FFontName := Copy(FFontName, 1, Length(FFontName) - 1);
        if IsFontAvailable then
        begin
          FFontWeight := FW_BOLD;
          Exit;
        end;
      end;
    end;

    // Check for Italic
    if Italic <> 0 then
    begin
      FFontName := Copy(FN, 1, Italic - 1) + Copy(FN, Italic + 6, MaxInt);
      if Copy(FFontName, Length(FFontName), 1) = '-' then
        FFontName := Copy(FFontName, 1, Length(FFontName) - 1);
      if IsFontAvailable then
      begin
        FFontStyle := SVGTypes.FontItalic;
        Exit;
      end;
      if Copy(FFontName, Length(FFontName) - 1, 2) = 'MT' then
      begin
        FFontName := Copy(FFontName, 1, Length(FFontName) - 2);
        if Copy(FFontName, Length(FFontName), 1) = '-' then
          FFontName := Copy(FFontName, 1, Length(FFontName) - 1);
        if IsFontAvailable then
        begin
          FFontStyle := SVGTypes.FontItalic;
          Exit;
        end;
      end;
    end;

    // Check for Bold and Italic
    if (Bold <> 0) and (Italic <> 0) then
    begin
      FFontName := Copy(FN, 1, Bold - 1) + Copy(FN, Bold + 4, MaxInt);
      if Copy(FFontName, Length(FFontName), 1) = '-' then
        FFontName := Copy(FFontName, 1, Length(FFontName) - 1);
      Italic := Pos('Italic', FFontName);

      FFontName := Copy(FFontName, 1, Italic - 1) + Copy(FFontName, Italic + 6, MaxInt);
      if Copy(FFontName, Length(FFontName), 1) = '-' then
        FFontName := Copy(FFontName, 1, Length(FFontName) - 1);

      if IsFontAvailable then
      begin
        FFontWeight := FW_BOLD;
        FFontStyle := SVGTypes.FontItalic;
        Exit;
      end;
      if Copy(FFontName, Length(FFontName) - 1, 2) = 'MT' then
      begin
        FFontName := Copy(FFontName, 1, Length(FFontName) - 2);
        if Copy(FFontName, Length(FFontName), 1) = '-' then
          FFontName := Copy(FFontName, 1, Length(FFontName) - 1);
        if IsFontAvailable then
        begin
          FFontWeight := FW_BOLD;
          FFontStyle := SVGTypes.FontItalic;
          Exit;
        end;
      end;
    end;

    FFontName := FN;
    if Copy(FFontName, Length(FFontName) - 1, 2) = 'MT' then
    begin
      FFontName := Copy(FFontName, 1, Length(FFontName) - 2);
      if Copy(FFontName, Length(FFontName), 1) = '-' then
        FFontName := Copy(FFontName, 1, Length(FFontName) - 1);
      if IsFontAvailable then
        Exit;
    end;

    FFontName := FN;
  end;

begin
  inherited;
  ProcessFontName;
  ReadTextNodes(Reader);
end;

function TSVGCustomText.ReadInAttr(SVGAttr: TSVGAttribute;
  const AttrValue: string): Boolean;
begin
  Result := True;
  case SVGAttr of
    saX:
      begin
        ParseLengthAttr(AttrValue, ltHorz, FX);
        FHasX := True;
      end;
    saY:
      begin
        ParseLengthAttr(AttrValue, ltVert, FY);
        FHasY := True;
      end;
    saDx: ParseLengthAttr(AttrValue, ltHorz, FDX);
    saDy: ParseLengthAttr(AttrValue, ltVert, FDY);
  else
    Result := inherited;
  end;
end;

procedure TSVGCustomText.ReadTextNodes(const Reader: IXMLReader);
var
  StartDepth, EndDepth: LongWord;
  NodeType: XmlNodeType;
begin
  if Reader.IsEmptyElement then Exit;
  StartDepth := Depth;
  Repeat
    if not EXmlLite.IsOK(Reader.Read(NodeType)) then break;
    case NodeType of
      XmlNodeType.None: Break;
      XmlNodeType.EndElement: Reader.GetDepth(EndDepth);
      XmlNodeType.Element,
      XmlNodeType.Text: ParseNode(Reader);
    end;
  Until ((Nodetype = XmlNodeType.EndElement) and (EndDepth = StartDepth));
end;
{$ENDREGION}

{$REGION 'TSVGTextPath'}
procedure TSVGTextPath.Clear;
begin
  inherited;
  FOffset := 0;
  FPathRef := '';
  FMethod := tpmAlign;
  FSpacing := tpsAuto;
end;

procedure TSVGTextPath.ConstructPath;
var
  GuidePath: TSVGPath;
  Position: TFloat;
  X, Y: TFloat;

  procedure RenderTextElement(const Element: TSVGCustomText);
  var
    C: Integer;
    FF: TGPFontFamily;
    FontStyle: Winapi.GDIPAPI.TFontStyle;
    SF: TGPStringFormat;
    PT: TGPPathText;
    Matrix: TGPMatrix;
    Size: TFloat;
  begin
    FreeAndNil(Element.FUnderlinePath);
    FreeAndNil(Element.FStrikeOutPath);
    FreeAndNil(Element.FPath);
    if Element.FText <> '' then
    begin
      FF := GetFontFamily(Element.GetFontName);

      FontStyle := FontStyleRegular;
      if Element.FFontWeight = FW_BOLD then
        FontStyle := FontStyle or FontStyleBold;

      if Element.GetFontStyle = 1 then
        FontStyle := FontStyle or FontStyleItalic;

      SF := TGPStringFormat.Create(TGPStringFormat.GenericTypographic);
      SF.SetFormatFlags(StringFormatFlagsMeasureTrailingSpaces);

      PT := TGPPathText.Create(GuidePath.GPPath);

      if not Element.FLocalMatrix.IsEmpty then
        Matrix := Element.FLocalMatrix.ToGPMatrix
      else
        Matrix := nil;

      X := X + Element.FDX;
      Y := Y + Element.FDY;
      if (X <> 0) or (Y <> 0) then
      begin
        if not Assigned(Matrix) then
          Matrix := TGPMatrix.Create;
        Matrix.Translate(X, Y);
      end;

      PT.AdditionalMatrix := Matrix;
      Element.FPath := TGPGraphicsPath2.Create;

      Size := Element.GetFontSize;
      Position := Position +
        // TODO Is Trim needed here?
        PT.AddPathText(Element.FPath, Trim(Element.FText), Position,
          FF, FontStyle, Size, SF);

      PT.Free;

      Matrix.Free;

      SF.Free;
      FF.Free;
    end;

    for C := 0 to Element.Count - 1 do
      if Element[C] is TSVGCustomText then
        RenderTextElement(TSVGCustomText(Element[C]));
  end;

begin
  inherited;

  GuidePath := nil;
  if FPathRef <> '' then
  begin
    if FPathRef[1] = '#' then
    begin
      GuidePath := TSVGPath(GetRoot.FindByID(Copy(FPathRef, 2, MaxInt)));
    end;
  end;

  if GuidePath = nil then
    Exit;

  if FOffsetIsPercent and (FOffset <> 0) then
    Position := TGPPathText.GetPathLength(GuidePath.GPPath) * FOffset
  else
    Position := FOffset;

  X := FDX;
  Y := FDY;

  RenderTextElement(Self);
end;

function TSVGTextPath.ReadInAttr(SVGAttr: TSVGAttribute;
  const AttrValue: string): Boolean;
begin
  Result := True;
  case SVGAttr of
    saXlinkHref: FPathRef := AttrValue;
    saHref: FPathRef := AttrValue;
    saStartOffset: FOffset := ParseLength(AttrValue, FOffsetIsPercent);
    saMethod:
      begin
      if AttrValue = 'stretch' then
        FMethod := tpmStretch;
      end;
    saSpacing:
      begin
      if AttrValue = 'exact' then
        FSpacing := tpsExact;
      end;
  else
    Result := inherited;
  end;
end;
{$ENDREGION}

{$REGION 'TSVGClipPath'}
procedure TSVGClipPath.Clear;
begin
  inherited;
  FreeAndNil(FClipPath);
end;

procedure TSVGClipPath.ConstructClipPath;
begin
  FClipPath := TGPGraphicsPath.Create;
  PaintToPath(FClipPath);
end;

destructor TSVGClipPath.Destroy;
begin
  FreeAndNil(FClipPath);
  inherited;
end;

class function TSVGClipPath.Features: TSVGElementFeatures;
begin
  Result := [sefMayHaveChildren]
end;

function TSVGClipPath.GetClipPath: TGPGraphicsPath;
begin
  if not Assigned(FClipPath) then
    ConstructClipPath;
  Result := FClipPath;
end;

function TSVGClipPath.ReadInAttr(SVGAttr: TSVGAttribute;
  const AttrValue: string): Boolean;
begin
  Result := inherited;
  if FID <> '' then
    if not Root.IdDict.ContainsKey(FID) then
      Root.IdDict.Add(FID, Self);
end;

{$ENDREGION}

{$REGION 'TSVGShape'}
class function TSVGShape.Features: TSVGElementFeatures;
begin
  Result := [sefNeedsPainting, sefHasPath];
end;
{$ENDREGION}

end.
