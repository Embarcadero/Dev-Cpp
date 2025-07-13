(* cairo delphi & freepascal binding
 *
 * This library is free software; you can redistribute it and/or
 * modify it either under the terms of the GNU Lesser General Public
 * License version 2.1 as published by the Free Software Foundation
 * (the "LGPL") or, at your option, under the terms of the Mozilla
 * Public License Version 1.1 (the "MPL"). If you do not alter this
 * notice, a recipient may use your version of this file under either
 * the MPL or the LGPL.
 *
 * You should have received a copy of the LGPL along with this library
 * in the file COPYING-LGPL-2.1; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * You should have received a copy of the MPL along with this library
 * in the file COPYING-MPL-1.1
 *
 * The contents of this file are subject to the Mozilla Public License
 * Version 1.1 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * This software is distributed on an "AS IS" basis, WITHOUT WARRANTY
 * OF ANY KIND, either express or implied. See the LGPL or the MPL for
 * the specific language governing rights and limitations.
 *
 * Author(s):
 *   Henri Gourvest <hgourvest@progdigy.com>
 *)

unit cairo;
{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cairo.inc}

interface
uses sysutils, classes, cairolib
{$IFDEF WIN32}
, windows
{$ENDIF}
{$IFDEF CAIRO_HAS_RSVG_FUNCTIONS}
,rsvg
{$ENDIF}
;

type
  ECairoException = class(Exception)
  private
    FStatus: TCairoStatus;
  public
    constructor Create(AStatus: TCairoStatus); virtual;
    property Status: TCairoStatus read FStatus;
  end;

  ICairoFontFace = interface
    ['{67C6474B-3B01-4330-976F-E0F3A6A5F287}']
    function GetUserData(key: Pointer): Pointer;
    procedure SetUserData(key: Pointer; userData: Pointer; destroyFunc: TCairoDestroyFunc);
    function GetStatus: TCairoStatus;
    function GetType: TCairoFontType;

    property Status: TCairoStatus read GetStatus;
    property FontType: TCairoFontType read GetType;
  end;

  TCairoFontFace = class(TInterfacedObject, ICairoFontFace)
  private
    FFontFace: PCairoFontFace;
    class function init(fontface: PCairoFontFace): TCairoFontFace;
    constructor CreateInternal(fontface: PCairoFontFace);
  protected
    function GetUserData(key: Pointer): Pointer;
    procedure SetUserData(key: Pointer; userData: Pointer; destroyFunc: TCairoDestroyFunc);
    function GetStatus: TCairoStatus;
    function GetType: TCairoFontType;
  public
    destructor Destroy; override;
    property FontFace: PCairoFontFace read FFontFace;
  end;

  ICairoToyFontFace = interface(ICairoFontFace)
  ['{1A1EF26C-EA53-4151-A8B6-FED2BA11FBC2}']
    function GetFamily: AnsiString;
    function GetSlant: TCairoFontSlant;
    function GetWeight: TCairoFontWeight;

    property Family: AnsiString read GetFamily;
    property Slant: TCairoFontSlant read GetSlant;
    property Weight: TCairoFontWeight read GetWeight;
  end;

  TCairoToyFontFace = class(TCairoFontFace, ICairoToyFontFace)
  protected
    function GetFamily: AnsiString;
    function GetSlant: TCairoFontSlant;
    function GetWeight: TCairoFontWeight;
  public
    constructor Create(const family: AnsiString; slant: TCairoFontSlant; weight: TCairoFontWeight);
  end;

{$ifdef CAIRO_HAS_WIN32_FONT}
  TCairoWin32FontFace = class(TCairoFontFace)
  public
    constructor CreateForLogfontw(logfont: PLOGFONTW);
    constructor CreateForHfont(font: HFONT);
    constructor CreateForLogfontwHfont(logfont: PLOGFONTW; font: HFONT);
  end;
{$endif}

  ICairoFontOptions = interface
  ['{103D190B-93AB-492C-A144-CAD73CC21135}']
    function GetUserData: Pointer;
    function Copy: ICairoFontOptions;
    function GetStatus: TCairoStatus;
    procedure Merge(other: ICairoFontOptions);
    function Equal(other: ICairoFontOptions): Boolean;
    function GetHash: Cardinal;
    procedure SetAntialias(antialias: TCairoAntialias);
    function GetAntialias: TCairoAntialias;
    procedure SetSubpixelOrder(subpixelOrder: TCairoSubpixelOrder);
    function GetSubpixelOrder: TCairoSubpixelOrder;
    procedure SetHintStyle(hintStyle: TCairoHintStyle);
    function GetHintStyle: TCairoHintStyle;
    procedure SetHintMetrics(hintMetrics: TCairoHintMetrics);
    function GetHintMetrics: TCairoHintMetrics;

    property Antialias: TCairoAntialias read GetAntialias write SetAntialias;
    property SubpixelOrder: TCairoSubpixelOrder read GetSubpixelOrder write SetSubpixelOrder;
    property HintStyle: TCairoHintStyle read GetHintStyle write SetHintStyle;
    property HintMetrics: TCairoHintMetrics read GetHintMetrics write SetHintMetrics;
  end;

  TCairoUserFontFace = class(TCairoFontFace)
  public
    constructor Create; virtual;
  end;

  TCairoFontOptions = class(TInterfacedObject, ICairoFontOptions)
  private
    FFontOptions: PCairoFontOptions;
    constructor CreateInternal(options: PCairoFontOptions);
  protected
    function GetUserData: Pointer;
    function Copy: ICairoFontOptions;
    function GetStatus: TCairoStatus;
    procedure Merge(other: ICairoFontOptions);
    function Equal(other: ICairoFontOptions): Boolean;
    function GetHash: Cardinal;
    procedure SetAntialias(antialias: TCairoAntialias);
    function GetAntialias: TCairoAntialias;
    procedure SetSubpixelOrder(subpixelOrder: TCairoSubpixelOrder);
    function GetSubpixelOrder: TCairoSubpixelOrder;
    procedure SetHintStyle(hintStyle: TCairoHintStyle);
    function GetHintStyle: TCairoHintStyle;
    procedure SetHintMetrics(hintMetrics: TCairoHintMetrics);
    function GetHintMetrics: TCairoHintMetrics;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property FontOptions: PCairoFontOptions read FFontOptions;
  end;

  ICairoScaledFont = interface(ICairoFontFace)
  ['{EA646BC8-2E6A-4288-BBA2-C68AE29795EB}']
    function GetStatus: TCairoStatus;
    function GetType: TCairoFontType;
    procedure Extents(extent: TCairoFontExtents);
    procedure TextExtents(utf8: UTF8String; extent: TCairoTextExtents);
    procedure GlyphExtents(glyphs: PCairoGlyph; numGlyphs: Integer; extent: TCairoTextExtents);
    procedure TextToGlyphs(x, y: Double; utf8: UTF8String = '';
      glyphs: PPCairoGlyph = nil; numGlyphs: Integer = 0;
      clusters: PPCairoTextCluster = nil; numClusters: Integer = 0;
      clusterFlags: PCairoTextClusterFlags = nil);
    function GetFontFace: ICairoFontFace;
    procedure GetFontMatrix(var fontMatrix: TCairoMatrix);
    procedure GetCtm(var ctm: TCairoMatrix);
    procedure GetScaleMatrix(var scaleMatrix: TCairoMatrix);
    procedure GetFontOptions(options: ICairoFontOptions);
  end;

  TCairoScaledFont = class(TInterfacedObject, ICairoScaledFont)
  private
    FScaledFont: PCairoScaledFont;
    class function init(scaledfont: PCairoScaledFont): TCairoScaledFont;
  protected
    function GetUserData(key: Pointer): Pointer;
    procedure SetUserData(key: Pointer; userData: Pointer; destroyFunc: TCairoDestroyFunc);
    function GetStatus: TCairoStatus;
    function GetType: TCairoFontType;
    procedure Extents(extent: TCairoFontExtents);
    procedure TextExtents(utf8: UTF8String; extent: TCairoTextExtents);
    procedure GlyphExtents(glyphs: PCairoGlyph; numGlyphs: Integer; extent: TCairoTextExtents);
    procedure TextToGlyphs(x, y: Double; utf8: UTF8String = '';
      glyphs: PPCairoGlyph = nil; numGlyphs: Integer = 0;
      clusters: PPCairoTextCluster = nil; numClusters: Integer = 0;
      clusterFlags: PCairoTextClusterFlags = nil);
    function GetFontFace: ICairoFontFace;
    procedure GetFontMatrix(var fontMatrix: TCairoMatrix);
    procedure GetCtm(var ctm: TCairoMatrix);
    procedure GetScaleMatrix(var scaleMatrix: TCairoMatrix);
    procedure GetFontOptions(options: ICairoFontOptions);
  public
    constructor CreateInternal(scaledfont: PCairoScaledFont);
    constructor Create(fontFace: ICairoFontFace; fontMatrix, ctm: TCairoMatrix; options: ICairoFontOptions); virtual;
    destructor Destroy; override;
    property ScaledFont: PCairoScaledFont read FScaledFont;
  end;

  TUserScaledFont = class(TCairoScaledFont)
  protected
    function UserTextToGlyphs(utf8: PAnsiChar; utf8Len: Integer; glyphs: PPCairoGlyph; numGlyphs: PInteger; clusters: PPCairoTextCluster; numClusters: PInteger; clusterFlags: PCairoTextClusterFlags): TCairoStatus; virtual; abstract;
    function UserInit(cr: PCairo; extent: PCairoFontExtents): TCairoStatus; virtual; abstract;
    function UserRenderGlyph(glyph: Cardinal; cr: PCairo; extent: PCairoTextExtents): TCairoStatus; virtual; abstract;
    function UserUnicodeToGlyph(unicode: Cardinal; glyphIndex: PCardinal): TCairoStatus; virtual; abstract;
  public
    constructor Create(fontFace: ICairoFontFace; fontMatrix, ctm: TCairoMatrix; options: ICairoFontOptions); override;
  end;

{$ifdef CAIRO_HAS_WIN32_FONT}
  ICairoWin32ScaledFont = interface(ICairoScaledFont)
  ['{E456AD11-E68D-46BE-BB5E-455184F75B68}']
    procedure SelectFont(hdc: HDC);
    procedure DoneFont;
    function GetMetricsFactor: Double;
    procedure GetLogicalToDevice(var logicalToDevice: TCairoMatrix);
    procedure GetDeviceToLogical(var deviceToLogical: TCairoMatrix);
  end;
{$endif}

{$ifdef CAIRO_HAS_WIN32_FONT}
  TCairoWin32ScaledFont = class(TCairoScaledFont, ICairoWin32ScaledFont)
  protected
    procedure SelectFont(hdc: HDC);
    procedure DoneFont;
    function GetMetricsFactor: Double;
    procedure GetLogicalToDevice(var logicalToDevice: TCairoMatrix);
    procedure GetDeviceToLogical(var deviceToLogical: TCairoMatrix);
  end;
{$endif}

  ICairoDevice = interface
  ['{F7FCAD08-427E-4772-BFF7-7A0543280DC5}']
    function GetType: TCairoDeviceType;
    function GetStatus: TCairoStatus;
    function Acquire: TCairoStatus;
    procedure Release;
    procedure Flush;
    procedure Finish;
  end;

  TCairoDevice = class(TInterfacedObject, ICairoDevice)
  private
    FDevice: PCairoDevice;
    class function init(device: PCairoDevice): TCairoDevice;
    constructor CreateInternal(device: PCairoDevice);
  protected
    function GetType: TCairoDeviceType;
    function GetStatus: TCairoStatus;
    function Acquire: TCairoStatus;
    procedure Release;
    procedure Flush;
    procedure Finish;
  public
    destructor Destroy; override;
  end;

  ICairoSurface = interface
  ['{CE917E0C-97B2-434E-8C72-CD7930C4BB67}']
    function GetUserData(key: Pointer): Pointer;
    procedure SetUserData(key: Pointer; userData: Pointer; destroyFunc: TCairoDestroyFunc);
    function CreateSimilar(content: TCairoContent; width, height: Integer): ICairoSurface;
    function CreateForRectangle(x, y, width, height: Double): ICairoSurface;
    procedure Finish;
    function GetStatus: TCairoStatus;
    function GetType: TCairoSurfaceType;
    function GetContent: TCairoContent;
{$ifdef CAIRO_HAS_PNG_FUNCTIONS}
    procedure WriteToPNG(const filename: string);
    procedure WriteToPNGStream(stream: TStream);
{$endif}
    procedure GetFontOptions(options: ICairoFontOptions);
    procedure Flush;
    procedure MarkDirty;
    procedure MarkDirtyRectangle(x, y, width, height: Integer);
    procedure SetDeviceOffset(xOffset, yOffset: Double);
    procedure GetDeviceOffset(var xOffset, yOffset: Double);
    procedure SetFallbackResolution(xPixelsPerInch, yPixelsPerInch: Double);
    procedure GetFallbackResolution(var xPixelsPerInch, yPixelsPerInch: Double);
    procedure CopyPage;
    procedure ShowPage;
    function HasShowTextGlyphs: Boolean;
    function GetDevice: ICairoDevice;
    procedure GetMimeData(const mime_type: PAnsiChar; out data: Pointer; out length: Cardinal);
    function SetMimeData(const mime_type: PAnsiChar; const data : Pointer; length: Cardinal;	destroy: TCairoDestroyFunc; closure: Pointer): TCairoStatus;

    property Status: TCairoStatus read GetStatus;
    property SurfaceType: TCairoSurfaceType read GetType;
    property Content: TCairoContent read GetContent;
    property Device: ICairoDevice read GetDevice;
  end;

  TCairoSurface = class(TInterfacedObject, ICairoSurface)
  private
    FSurface: PCairoSurface;
    class function init(surface: PCairoSurface): TCairoSurface;
    constructor CreateInternal(surface: PCairoSurface);
  protected
    function GetUserData(key: Pointer): Pointer;
    procedure SetUserData(key: Pointer; userData: Pointer; destroyFunc: TCairoDestroyFunc);
    function CreateSimilar(content: TCairoContent; width, height: Integer): ICairoSurface;
    function CreateForRectangle(x, y, width, height: Double): ICairoSurface;
    procedure Finish;
    function GetStatus: TCairoStatus;
    function GetType: TCairoSurfaceType;
    function GetContent: TCairoContent;
{$ifdef CAIRO_HAS_PNG_FUNCTIONS}
    procedure WriteToPNG(const filename: string);
    procedure WriteToPNGStream(stream: TStream);
{$endif}
    procedure GetFontOptions(options: ICairoFontOptions);
    procedure Flush;
    procedure MarkDirty;
    procedure MarkDirtyRectangle(x, y, width, height: Integer);
    procedure SetDeviceOffset(xOffset, yOffset: Double);
    procedure GetDeviceOffset(var xOffset, yOffset: Double);
    procedure SetFallbackResolution(xPixelsPerInch, yPixelsPerInch: Double);
    procedure GetFallbackResolution(var xPixelsPerInch, yPixelsPerInch: Double);
    procedure CopyPage;
    procedure ShowPage;
    function HasShowTextGlyphs: Boolean;
    function GetDevice: ICairoDevice;
    procedure GetMimeData(const mimeType: PAnsiChar; out data: Pointer; out length: Cardinal);
    function SetMimeData(const mimeType: PAnsiChar; const data : Pointer; length: Cardinal;	destroy: TCairoDestroyFunc; closure: Pointer): TCairoStatus;
  public
    constructor CreateRecording(content: TCairoContent; const extents: PCairoRectangle);
//function cairo_recording_surface_create(content: TCairoContent; const extents: PCairoRectangle): PCairoSurface; cdecl;
//procedure cairo_recording_surface_ink_extents(surface: PCairoSurface; x0, y0, width, height: PDouble); cdecl;

    destructor Destroy; override;
    property Surface: PCairoSurface read FSurface;
  end;

  ICairoPattern = interface
    ['{F93A9DAE-6E01-4B92-A13F-3CCCFA6B0EEA}']
    function GetStatus: TCairoStatus;
    function GetUserData(key: Pointer): Pointer;
    procedure SetUserData(key: Pointer; userData: Pointer; destroyFunc: TCairoDestroyFunc);

    function GetType: TCairoPatternType;
    procedure AddColorStopRGB(offset, red, green, blue: Double);
    procedure AddColorStopRGBA(offset, red, green, blue, alpha: Double);
    procedure AddColorStop(offset: Double; color: Cardinal);
    procedure SetMatrix(matrix: TCairoMatrix);
    procedure GetMatrix(var matrix: TCairoMatrix);

    procedure SetExtend(extend: TCairoExtend);
    function GetExtend: TCairoExtend;

    procedure SetFilter(filter: TCairoFilter);
    function GetFilter: TCairoFilter;
    procedure GetRGBA(red, green, blue, alpha: PDouble);
    function GetSurface: ICairoSurface;
    procedure GetColorStopRGBA(index: Integer; offset, red, green, blue, alpha: PDouble);
    function GetColorStopCount: Integer;
    procedure GetLinearPoints(x0, y0, x1, y1: PDouble);
    procedure GetRadialCircles(x0, y0, r0, x1, y1, r1: PDouble);

    property Extend: TCairoExtend read GetExtend write SetExtend;
    property Status: TCairoStatus read GetStatus;
    property Filter: TCairoFilter read GetFilter write SetFilter;
    property Surface: ICairoSurface read GetSurface;
  end;

  TCairoPattern = class(TInterfacedObject, ICairoPattern)
  private
    FPattern: PCairoPattern;
    class function init(pattern: PCairoPattern): TCairoPattern;
    constructor CreateInternal(pattern: PCairoPattern);
  protected
    function GetStatus: TCairoStatus;
    function GetUserData(key: Pointer): Pointer;
    procedure SetUserData(key: Pointer; userData: Pointer; destroyFunc: TCairoDestroyFunc);

    function GetType: TCairoPatternType;
    procedure AddColorStopRGB(offset, red, green, blue: Double);
    procedure AddColorStopRGBA(offset, red, green, blue, alpha: Double);
    procedure AddColorStop(offset: Double; color: Cardinal);
    procedure SetMatrix(matrix: TCairoMatrix);
    procedure GetMatrix(var matrix: TCairoMatrix);

    procedure SetExtend(extend: TCairoExtend);
    function GetExtend: TCairoExtend;

    procedure SetFilter(filter: TCairoFilter);
    function GetFilter: TCairoFilter;
    procedure GetRGBA(red, green, blue, alpha: PDouble);
    function GetSurface: ICairoSurface;
    procedure GetColorStopRGBA(index: Integer; offset, red, green, blue, alpha: PDouble);
    function GetColorStopCount: Integer;
    procedure GetLinearPoints(x0, y0, x1, y1: PDouble);
    procedure GetRadialCircles(x0, y0, r0, x1, y1, r1: PDouble);
  public
    constructor CreateRGB(red, green, blue: Double);
    constructor CreateRGBA(red, green, blue, alpha: Double);
    constructor CreateForSurface(surface: ICairoSurface);
    constructor CreateLinear(x0, y0, x1, y1: Double);
    constructor CreateRadial(cx0, cy0, radius0, cx1, cy1, radius1: Double);
    destructor Destroy; override;
    property Pattern: PCairoPattern read FPattern;
  end;

{$ifdef CAIRO_HAS_WIN32_SURFACE}
  IWin32Surface = interface(ICairoSurface)
  ['{94C8B7E7-4AD4-4EA2-90BE-D9E2FC59725E}']
    function GetDC: HDC;
    function GetImage: ICairoSurface;
  end;
{$endif}

{$ifdef CAIRO_HAS_WIN32_SURFACE}
  TWin32Surface = class(TCairoSurface, IWin32Surface)
  protected
    function GetDC: HDC;
    function GetImage: ICairoSurface;
  public
    constructor CreateHDC(hdc: HDC); virtual;
    constructor CreatePrinting(hdc: HDC); virtual;
    constructor CreateWithDDB(hdc: HDC; format: TCairoFormat; width, height: Integer); virtual;
    constructor CreateWithDIB(format: TCairoFormat; width: Integer; height: Integer); virtual;
  end;
{$ENDIF}

  IImageSurface = interface(ICairoSurface)
  ['{4D4E061E-D4D5-4DBB-9A09-036724AB15F0}']
    function GetData: Pointer;
    function GetFormat: TCairoFormat;
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetStride: Integer;

    property Data: Pointer read GetData;
    property Format: TCairoFormat read GetFormat;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Stride: Integer read GetStride;
  end;

  TImageSurface = class(TCairoSurface, IImageSurface)
  protected
    function GetData: Pointer;
    function GetFormat: TCairoFormat;
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetStride: Integer;
  public
    constructor Create(format: TCairoFormat; width, height: Integer);
    constructor CreateForData(data: Pointer; format: TCairoFormat; width, height, stride: Integer);
{$ifdef CAIRO_HAS_PNG_FUNCTIONS}
    constructor CreateFromPNG(const filename: string); overload;
    constructor CreateFromPNG(stream: TStream); overload;
{$endif}
{$ifdef CAIRO_HAS_RSVG_FUNCTIONS}
    constructor CreateFromSVG(const filename: string; format: TCairoFormat; scale: double); overload;
    constructor CreateFromSVG(stream: TStream; format: TCairoFormat; scale: double); overload;
{$endif}
  end;

{$ifdef CAIRO_HAS_SVG_SURFACE}
  ISVGSurface = interface(ICairoSurface)
    ['{8A00FEC6-0C3D-4F0B-A781-BA764731FC71}']
    procedure RestrictToVersion(version: TCairoSVGVersion);
  end;
{$endif}

{$ifdef CAIRO_HAS_SVG_SURFACE}
  TSVGSurface = class(TCairoSurface, ISVGSurface)
  private
    FStream: TStream;
  protected
    procedure RestrictToVersion(version: TCairoSVGVersion);
  public
    constructor Create(stream: TStream; widthInPoints, heightInPoints: Double; owned: boolean = false); overload;
    constructor Create(filename: TFileName; widthInPoints, heightInPoints: Double); overload;
    destructor Destroy; override;
  end;
{$endif}

{$ifdef CAIRO_HAS_PDF_SURFACE}
  IPDFSurface = interface(ICairoSurface)
    ['{AEFBC266-74E8-418A-9908-C3C118157C28}']
    procedure SetSize(widthInPoints, heightInPoints: Double);
    procedure RestrictToVersion(version: TCairoPdfVersion);
  end;
{$endif}

{$ifdef CAIRO_HAS_PDF_SURFACE}
  TPDFSurface = class(TCairoSurface, IPDFSurface)
  private
    FStream: TStream;
  protected
    procedure SetSize(widthInPoints, heightInPoints: Double);
    procedure RestrictToVersion(version: TCairoPdfVersion);
  public
    constructor Create(stream: TStream; widthInPoints, heightInPoints: Double; owned: boolean = false); overload;
    constructor Create(filename: TFileName; widthInPoints, heightInPoints: Double); overload;
    destructor Destroy; override;
  end;
{$endif}

{$ifdef CAIRO_HAS_PS_SURFACE}
  IPostScriptSurface = interface(ICairoSurface)
    ['{F763BE75-E19C-4F48-8C5B-AC220D9FE3F4}']
    procedure RestrictToLevel(level: TCairoPSLevel);
    procedure SetEPS(eps: Boolean);
    function GetEPS: Boolean;
    procedure SetSize(widthInPoints, heightInPoints: Double);
    procedure DscComment(const comment: AnsiString);
    procedure DscBeginSetup;
    procedure DscBeginPageSetup;

    property EPS: Boolean read GetEPS write SetEPS;
  end;
{$endif}

{$ifdef CAIRO_HAS_PS_SURFACE}
  TPostScriptSurface = class(TCairoSurface, IPostScriptSurface)
  private
    FStream: TStream;
  protected
    procedure RestrictToLevel(level: TCairoPSLevel);
    procedure SetEPS(eps: Boolean);
    function GetEPS: Boolean;
    procedure SetSize(widthInPoints, heightInPoints: Double);
    procedure DscComment(const comment: AnsiString);
    procedure DscBeginSetup;
    procedure DscBeginPageSetup;
  public
    constructor Create(stream: TStream; widthInPoints, heightInPoints: Double; owned: boolean = false); overload;
    constructor Create(const filename: string; widthInPoints, heightInPoints: Double); overload;
    destructor Destroy; override;
  end;
{$endif}

  ICairoContext = interface
    ['{CDF9B3F9-ED80-4300-9D2C-4F0B233A9912}']
    function GetStatus: TCairoStatus;
    (* Functions for manipulating state objects *)
    function GetUserData(key: Pointer): Pointer;
    procedure SetUserData(key: Pointer; userData: Pointer; destroyFunc: TCairoDestroyFunc);
    procedure Save;
    procedure Restore;
    procedure PushGroup;
    procedure PushGroupWithContent(content: TCairoContent);
    function PopGroup: ICairoPattern;
    procedure PopGroupToSource;

    (* Modify state *)
    procedure SetOperator(op: TCairoOperator);
    procedure SetSource(source: ICairoPattern);
    procedure SetSourceColor(color: Cardinal);
    procedure SetSourceRGB(red, green, blue: Double);
    procedure SetSourceRGBA(red, green, blue, alpha: Double);
    procedure SetSourceSurface(surface: ICairoSurface; x, y: Double);
    procedure SetTolerance(tolerance: Double);
    procedure SetAntialias(antialias: TCairoAntialias);
    procedure SetFillRule(fillRule: TCairoFillRule);
    procedure SetLineWidth(width: Double);
    procedure SetLineCap(lineCap: TCairoLineCap);
    procedure SetLineJoin(lineJoin: TCairoLineJoin);
    procedure SetDash(dashes: PDouble; numDashes: Integer; offset: Double);
    procedure SetMiterLimit(limit: Double);
    procedure Translate(tx, ty: Double);
    procedure Scale(sx, sy: Double);
    procedure Rotate(angle: Double);
    procedure Transform(matrix: TCairoMatrix);
    procedure SetMatrix(matrix: TCairoMatrix);
    procedure IdentityMatrix;
    procedure UserToDevice(var x, y: Double);
    procedure UserToDeviceDistance(var dx, dy: Double);
    procedure DeviceToUser(var x, y: Double);
    procedure DeviceToUserDistance(var dx, dy: Double);

    (* Path creation functions *)
    procedure NewPath;
    procedure MoveTo(x, y: Double);
    procedure NewSubPath;
    procedure LineTo(x, y: Double);
    procedure CurveTo(x1, y1, x2, y2, x3, y3: Double);
    procedure Arc(xc, yc, radius, angle1, angle2: Double);
    procedure ArcNegative(xc, yc, radius, angle1, angle2: Double);
    procedure RelMoveTo(dx, dy: Double);
    procedure RelLineTo(dx, dy: Double);
    procedure RelCurveTo(dx1, dy1, dx2, dy2, dx3, dy3: Double);
    procedure Rectangle(x, y, width, height: Double);
    procedure ClosePath;
    procedure PathExtents(x1, y1, x2, y2: PDouble);

    (* Painting functions *)
    procedure Paint;
    procedure PaintWithAlpha(alpha: Double);
    procedure Mask(pattern: ICairoPattern);
    procedure MaskSurface(surface: ICairoSurface; surfaceX, surfaceY: Double);
    procedure Stroke;
    procedure StrokePreserve;
    procedure Fill;
    procedure FillPreserve;
    procedure CopyPage;
    procedure ShowPage;

    (* Insideness testing *)
    function InStroke(x, y: Double): Boolean;
    function InFill(x, y: Double): Boolean;
    function InClip(x, y: Double): Boolean;

    (* Rectangular extents *)
    procedure StrokeExtents(x1, y1, x2, y2: PDouble);
    procedure FillExtents(x1, y1, x2, y2: PDouble);

    (* Clipping *)
    procedure ResetClip;
    procedure Clip;
    procedure ClipPreserve;
    procedure ClipExtents(x1, y1, x2, y2: PDouble);
    function CopyClipRectangleList: PCairoRectangleList; // destroy it !

    (* Font/Text functions *)

    procedure SelectFontFace(const family: AnsiString; slant: TCairoFontSlant; weight: TCairoFontWeight);
    procedure SetFontSize(size: Double);
    procedure SetFontMatrix(matrix: TCairoMatrix);
    procedure GetFontMatrix(var matrix: TCairoMatrix);
    procedure SetFontOptions(options: ICairoFontOptions);
    function GetFontOptions: ICairoFontOptions;
    procedure SetFontFace(fontFace: ICairoFontFace);
    function GetFontFace: ICairoFontFace;
    procedure SetScaledFont(scaledFont: ICairoScaledFont);
    function GetScaledFont: ICairoScaledFont;
    procedure ShowText(utf8: UTF8String);
    procedure ShowGlyphs(glyphs: PCairoGlyph; numGlyphs: Integer);
    procedure ShowTextGlyphs(utf8: UTF8String; glyphs: PCairoGlyph;
      numGlyphs: Integer; clusters: PCairoTextCluster; numClusters: Integer;
      clusterFlags: TCairoTextClusterFlags);
    procedure TextPath(utf8: UTF8String);
    procedure GlyphPath(glyphs: PCairoGlyph; numGlyphs: Integer);
    procedure TextExtents(utf8: UTF8String; extent: PCairoTextExtents);
    procedure GlyphExtents(glyphs: PCairoGlyph; numGlyphs: Integer; extent: PCairoTextExtents);
    procedure FontExtents(extents: PCairoFontExtents);

    (* Query functions *)
    function GetOperator: TCairoOperator;
    function GetSource: ICairoPattern;
    function GetTolerance: Double;
    function GetAntialias: TCairoAntialias;
    function HasCurrentPoint: Boolean;
    procedure GetCurrentPoint(x: PDouble; y: PDouble);
    function GetFillRule: TCairoFillRule;
    function GetLineWidth: Double;
    function GetLineCap: TCairoLineCap;
    function GetLineJoin: TCairoLineJoin;
    function GetMiterLimit: Double;
    function GetDashCount: Integer;
    procedure GetDash(dashes: PDouble; offset: PDouble);
    procedure GetMatrix(var matrix: TCairoMatrix);
    function GetTarget: ICairoSurface;
    function GetGroupTarget: ICairoSurface;

    (* path *)
    function CopyPath: PCairoPath; // destroy it !
    function CopyPathFlat: PCairoPath; // destroy it !
    procedure AppendPath(path: PCairoPath);

    (* SVG *)
{$IFDEF CAIRO_HAS_RSVG_FUNCTIONS}
    function RenderSVG(svg: IRSVGObject; id: RawByteString = ''): Boolean;
{$ENDIF}

    property Op: TCairoOperator read GetOperator write SetOperator;
    property Source: ICairoPattern read GetSource write SetSource;
    property Tolerance: Double read GetTolerance write SetTolerance;
    property Antialias: TCairoAntialias read GetAntialias write SetAntialias;
    property FillRule: TCairoFillRule read GetFillRule write SetFillRule;
    property LineWidth: Double read GetLineWidth write SetLineWidth;
    property LineCap: TCairoLineCap read GetLineCap write SetLineCap;
    property LineJoin: TCairoLineJoin read GetLineJoin write SetLineJoin;
    property MiterLimit: Double read GetMiterLimit write SetMiterLimit;
    property DashCount: Integer read GetDashCount;
    property Target: ICairoSurface read GetTarget;
    property GroupTarget: ICairoSurface read GetGroupTarget;
    property Status: TCairoStatus read GetStatus;
    property FontFace: ICairoFontFace read GetFontFace write SetFontFace;
  end;

  TCairoContext = class(TInterfacedObject, ICairoContext)
  private
    FContext: PCairo;
    //class function init(context: PCairo): TCairoContext;
    constructor CreateInternal(context: PCairo);
  protected
    function GetStatus: TCairoStatus;

    (* Functions for manipulating state objects *)
    function GetUserData(key: Pointer): Pointer;
    procedure SetUserData(key: Pointer; userData: Pointer; destroyFunc: TCairoDestroyFunc);
    procedure Save;
    procedure Restore;
    procedure PushGroup;
    procedure PushGroupWithContent(content: TCairoContent);
    function PopGroup: ICairoPattern;
    procedure PopGroupToSource;

    (* Modify state *)
    procedure SetOperator(op: TCairoOperator);
    procedure SetSource(source: ICairoPattern);
    procedure SetSourceColor(color: Cardinal);
    procedure SetSourceRGB(red, green, blue: Double);
    procedure SetSourceRGBA(red, green, blue, alpha: Double);
    procedure SetSourceSurface(surface: ICairoSurface; x, y: Double);
    procedure SetTolerance(tolerance: Double);
    procedure SetAntialias(antialias: TCairoAntialias);
    procedure SetFillRule(fillRule: TCairoFillRule);
    procedure SetLineWidth(width: Double);
    procedure SetLineCap(lineCap: TCairoLineCap);
    procedure SetLineJoin(lineJoin: TCairoLineJoin);
    procedure SetDash(dashes: PDouble; numDashes: Integer; offset: Double);
    procedure SetMiterLimit(limit: Double);
    procedure Translate(tx, ty: Double);
    procedure Scale(sx, sy: Double);
    procedure Rotate(angle: Double);
    procedure Transform(matrix: TCairoMatrix);
    procedure SetMatrix(matrix: TCairoMatrix);
    procedure IdentityMatrix;
    procedure UserToDevice(var x, y: Double);
    procedure UserToDeviceDistance(var dx, dy: Double);
    procedure DeviceToUser(var x, y: Double);
    procedure DeviceToUserDistance(var dx, dy: Double);

    (* Path creation functions *)
    procedure NewPath;
    procedure MoveTo(x, y: Double);
    procedure NewSubPath;
    procedure LineTo(x, y: Double);
    procedure CurveTo(x1, y1, x2, y2, x3, y3: Double);
    procedure Arc(xc, yc, radius, angle1, angle2: Double);
    procedure ArcNegative(xc, yc, radius, angle1, angle2: Double);
    procedure RelMoveTo(dx, dy: Double);
    procedure RelLineTo(dx, dy: Double);
    procedure RelCurveTo(dx1, dy1, dx2, dy2, dx3, dy3: Double);
    procedure Rectangle(x, y, width, height: Double);
    procedure ClosePath;
    procedure PathExtents(x1, y1, x2, y2: PDouble);

    (* Painting functions *)
    procedure Paint;
    procedure PaintWithAlpha(alpha: Double);
    procedure Mask(pattern: ICairoPattern);
    procedure MaskSurface(surface: ICairoSurface; surfaceX, surfaceY: Double);
    procedure Stroke;
    procedure StrokePreserve;
    procedure Fill;
    procedure FillPreserve;
    procedure CopyPage;
    procedure ShowPage;

    (* Insideness testing *)
    function InStroke(x, y: Double): Boolean;
    function InFill(x, y: Double): Boolean;
    function InClip(x, y: Double): Boolean;

    (* Rectangular extents *)
    procedure StrokeExtents(x1, y1, x2, y2: PDouble);
    procedure FillExtents(x1, y1, x2, y2: PDouble);

    (* Clipping *)
    procedure ResetClip;
    procedure Clip;
    procedure ClipPreserve;
    procedure ClipExtents(x1, y1, x2, y2: PDouble);
    function CopyClipRectangleList: PCairoRectangleList; // destroy it !

    (* Font/Text functions *)

    procedure SelectFontFace(const family: AnsiString; slant: TCairoFontSlant; weight: TCairoFontWeight);
    procedure SetFontSize(size: Double);
    procedure SetFontMatrix(matrix: TCairoMatrix);
    procedure GetFontMatrix(var matrix: TCairoMatrix);
    procedure SetFontOptions(options: ICairoFontOptions);
    function GetFontOptions: ICairoFontOptions;
    procedure SetFontFace(fontFace: ICairoFontFace);
    function GetFontFace: ICairoFontFace;
    procedure SetScaledFont(scaledFont: ICairoScaledFont);
    function GetScaledFont: ICairoScaledFont;
    procedure ShowText(utf8: UTF8String);
    procedure ShowGlyphs(glyphs: PCairoGlyph; numGlyphs: Integer);
    procedure ShowTextGlyphs(utf8: UTF8String; glyphs: PCairoGlyph;
      numGlyphs: Integer; clusters: PCairoTextCluster; numClusters: Integer;
      clusterFlags: TCairoTextClusterFlags);
    procedure TextPath(utf8: UTF8String);
    procedure GlyphPath(glyphs: PCairoGlyph; numGlyphs: Integer);
    procedure TextExtents(utf8: UTF8String; extent: PCairoTextExtents);
    procedure GlyphExtents(glyphs: PCairoGlyph; numGlyphs: Integer; extent: PCairoTextExtents);
    procedure FontExtents(extents: PCairoFontExtents);

    (* Query functions *)
    function GetOperator: TCairoOperator;
    function GetSource: ICairoPattern;
    function GetTolerance: Double;
    function GetAntialias: TCairoAntialias;
    function HasCurrentPoint: Boolean;
    procedure GetCurrentPoint(x: PDouble; y: PDouble);
    function GetFillRule: TCairoFillRule;
    function GetLineWidth: Double;
    function GetLineCap: TCairoLineCap;
    function GetLineJoin: TCairoLineJoin;
    function GetMiterLimit: Double;
    function GetDashCount: Integer;
    procedure GetDash(dashes: PDouble; offset: PDouble);
    procedure GetMatrix(var matrix: TCairoMatrix);
    function GetTarget: ICairoSurface;
    function GetGroupTarget: ICairoSurface;

    (* path *)
    function CopyPath: PCairoPath; // destroy it !
    function CopyPathFlat: PCairoPath; // destroy it !
    procedure AppendPath(path: PCairoPath);

    (* SVG *)
{$IFDEF CAIRO_HAS_RSVG_FUNCTIONS}
    function RenderSVG(svg: IRSVGObject; id: RawByteString = ''): Boolean;
{$ENDIF}

  public
    constructor Create(surface: ICairoSurface);
    destructor Destroy; override;
    property Context: PCairo read FContext;
  end;

  ICairoRegion = interface
    ['{B9EF38A7-452F-492F-8B3C-CFE69A355DC3}']
    function GetUserData: Pointer;
    function Copy: ICairoRegion;
    function Equal(r: ICairoRegion): Boolean;
    function Status: TCairoStatus;
    procedure GetExtents(extents: PCairoRectangleInt);
    function NumRectangles: Integer;
    procedure GetRectangle(nth: Integer; rectangle: PCairoRectangleInt);
    function IsEmpty: Boolean;
    function ContainsRectangle(const rectangle: PCairoRectangleInt): TCairoRegionOverlap;
    function ContainsPoint(x, y: Integer): Boolean;
    procedure Translate(dx, dy: Integer);
    function Subtract(other: ICairoRegion): TCairoStatus;
    function SubtractRectangle(const rectangle: PCairoRectangleInt): TCairoStatus;
    function Intersect(other: ICairoRegion): TCairoStatus;
    function IntersectRectangle(const rectangle: PCairoRectangleInt): TCairoStatus;
    function Union(other: ICairoRegion): TCairoStatus;
    function UnionRectangle(const rectangle: PCairoRectangleInt): TCairoStatus;
    function XorRegion(other: ICairoRegion): TCairoStatus;
    function XorRectangle(const rectangle: PCairoRectangleInt): TCairoStatus;
  end;

  TCairoRegion = class(TInterfacedObject, ICairoRegion)
  private
    FRegion: PCairoRegion;
    constructor CreateInternal(region: PCairoRegion);
  protected
    function GetUserData: Pointer;
    function Copy: ICairoRegion;
    function Equal(r: ICairoRegion): Boolean;
    function Status: TCairoStatus;
    procedure GetExtents(extents: PCairoRectangleInt);
    function NumRectangles: Integer;
    procedure GetRectangle(nth: Integer; rectangle: PCairoRectangleInt);
    function IsEmpty: Boolean;
    function ContainsRectangle(const rectangle: PCairoRectangleInt): TCairoRegionOverlap;
    function ContainsPoint(x, y: Integer): Boolean;
    procedure Translate(dx, dy: Integer);
    function Subtract(other: ICairoRegion): TCairoStatus;
    function SubtractRectangle(const rectangle: PCairoRectangleInt): TCairoStatus;
    function Intersect(other: ICairoRegion): TCairoStatus;
    function IntersectRectangle(const rectangle: PCairoRectangleInt): TCairoStatus;
    function Union(other: ICairoRegion): TCairoStatus;
    function UnionRectangle(const rectangle: PCairoRectangleInt): TCairoStatus;
    function XorRegion(other: ICairoRegion): TCairoStatus;
    function XorRectangle(const rectangle: PCairoRectangleInt): TCairoStatus;
  public
    constructor Create;
    constructor CreateRectangle(const rectangle: PCairoRectangleInt);
    constructor CreateRectangles(const rects: PCairoRectangleInt; count: Integer);
    destructor Destroy; override;
  end;


function CairoFormatStrideForWidth(format: TCairoFormat; width: Integer): Integer;
procedure CairoCheck(AStatus: TCairoStatus);

const
  aclAliceBlue            = $FFF0F8FF;
  aclAntiqueWhite         = $FFFAEBD7;
  aclAqua                 = $FF00FFFF;
  aclAquamarine           = $FF7FFFD4;
  aclAzure                = $FFF0FFFF;
  aclBeige                = $FFF5F5DC;
  aclBisque               = $FFFFE4C4;
  aclBlack                = $FF000000;
  aclBlanchedAlmond       = $FFFFEBCD;
  aclBlue                 = $FF0000FF;
  aclBlueViolet           = $FF8A2BE2;
  aclBrown                = $FFA52A2A;
  aclBurlyWood            = $FFDEB887;
  aclCadetBlue            = $FF5F9EA0;
  aclChartreuse           = $FF7FFF00;
  aclChocolate            = $FFD2691E;
  aclCoral                = $FFFF7F50;
  aclCornflowerBlue       = $FF6495ED;
  aclCornsilk             = $FFFFF8DC;
  aclCrimson              = $FFDC143C;
  aclCyan                 = $FF00FFFF;
  aclDarkBlue             = $FF00008B;
  aclDarkCyan             = $FF008B8B;
  aclDarkGoldenrod        = $FFB8860B;
  aclDarkGray             = $FFA9A9A9;
  aclDarkGreen            = $FF006400;
  aclDarkKhaki            = $FFBDB76B;
  aclDarkMagenta          = $FF8B008B;
  aclDarkOliveGreen       = $FF556B2F;
  aclDarkOrange           = $FFFF8C00;
  aclDarkOrchid           = $FF9932CC;
  aclDarkRed              = $FF8B0000;
  aclDarkSalmon           = $FFE9967A;
  aclDarkSeaGreen         = $FF8FBC8B;
  aclDarkSlateBlue        = $FF483D8B;
  aclDarkSlateGray        = $FF2F4F4F;
  aclDarkTurquoise        = $FF00CED1;
  aclDarkViolet           = $FF9400D3;
  aclDeepPink             = $FFFF1493;
  aclDeepSkyBlue          = $FF00BFFF;
  aclDimGray              = $FF696969;
  aclDodgerBlue           = $FF1E90FF;
  aclFirebrick            = $FFB22222;
  aclFloralWhite          = $FFFFFAF0;
  aclForestGreen          = $FF228B22;
  aclFuchsia              = $FFFF00FF;
  aclGainsboro            = $FFDCDCDC;
  aclGhostWhite           = $FFF8F8FF;
  aclGold                 = $FFFFD700;
  aclGoldenrod            = $FFDAA520;
  aclGray                 = $FF808080;
  aclGreen                = $FF008000;
  aclGreenYellow          = $FFADFF2F;
  aclHoneydew             = $FFF0FFF0;
  aclHotPink              = $FFFF69B4;
  aclIndianRed            = $FFCD5C5C;
  aclIndigo               = $FF4B0082;
  aclIvory                = $FFFFFFF0;
  aclKhaki                = $FFF0E68C;
  aclLavender             = $FFE6E6FA;
  aclLavenderBlush        = $FFFFF0F5;
  aclLawnGreen            = $FF7CFC00;
  aclLemonChiffon         = $FFFFFACD;
  aclLightBlue            = $FFADD8E6;
  aclLightCoral           = $FFF08080;
  aclLightCyan            = $FFE0FFFF;
  aclLightGoldenrodYellow = $FFFAFAD2;
  aclLightGray            = $FFD3D3D3;
  aclLightGreen           = $FF90EE90;
  aclLightPink            = $FFFFB6C1;
  aclLightSalmon          = $FFFFA07A;
  aclLightSeaGreen        = $FF20B2AA;
  aclLightSkyBlue         = $FF87CEFA;
  aclLightSlateGray       = $FF778899;
  aclLightSteelBlue       = $FFB0C4DE;
  aclLightYellow          = $FFFFFFE0;
  aclLime                 = $FF00FF00;
  aclLimeGreen            = $FF32CD32;
  aclLinen                = $FFFAF0E6;
  aclMagenta              = $FFFF00FF;
  aclMaroon               = $FF800000;
  aclMediumAquamarine     = $FF66CDAA;
  aclMediumBlue           = $FF0000CD;
  aclMediumOrchid         = $FFBA55D3;
  aclMediumPurple         = $FF9370DB;
  aclMediumSeaGreen       = $FF3CB371;
  aclMediumSlateBlue      = $FF7B68EE;
  aclMediumSpringGreen    = $FF00FA9A;
  aclMediumTurquoise      = $FF48D1CC;
  aclMediumVioletRed      = $FFC71585;
  aclMidnightBlue         = $FF191970;
  aclMintCream            = $FFF5FFFA;
  aclMistyRose            = $FFFFE4E1;
  aclMoccasin             = $FFFFE4B5;
  aclNavajoWhite          = $FFFFDEAD;
  aclNavy                 = $FF000080;
  aclOldLace              = $FFFDF5E6;
  aclOlive                = $FF808000;
  aclOliveDrab            = $FF6B8E23;
  aclOrange               = $FFFFA500;
  aclOrangeRed            = $FFFF4500;
  aclOrchid               = $FFDA70D6;
  aclPaleGoldenrod        = $FFEEE8AA;
  aclPaleGreen            = $FF98FB98;
  aclPaleTurquoise        = $FFAFEEEE;
  aclPaleVioletRed        = $FFDB7093;
  aclPapayaWhip           = $FFFFEFD5;
  aclPeachPuff            = $FFFFDAB9;
  aclPeru                 = $FFCD853F;
  aclPink                 = $FFFFC0CB;
  aclPlum                 = $FFDDA0DD;
  aclPowderBlue           = $FFB0E0E6;
  aclPurple               = $FF800080;
  aclRed                  = $FFFF0000;
  aclRosyBrown            = $FFBC8F8F;
  aclRoyalBlue            = $FF4169E1;
  aclSaddleBrown          = $FF8B4513;
  aclSalmon               = $FFFA8072;
  aclSandyBrown           = $FFF4A460;
  aclSeaGreen             = $FF2E8B57;
  aclSeaShell             = $FFFFF5EE;
  aclSienna               = $FFA0522D;
  aclSilver               = $FFC0C0C0;
  aclSkyBlue              = $FF87CEEB;
  aclSlateBlue            = $FF6A5ACD;
  aclSlateGray            = $FF708090;
  aclSnow                 = $FFFFFAFA;
  aclSpringGreen          = $FF00FF7F;
  aclSteelBlue            = $FF4682B4;
  aclTan                  = $FFD2B48C;
  aclTeal                 = $FF008080;
  aclThistle              = $FFD8BFD8;
  aclTomato               = $FFFF6347;
  aclTransparent          = $00FFFFFF;
  aclTurquoise            = $FF40E0D0;
  aclViolet               = $FFEE82EE;
  aclWheat                = $FFF5DEB3;
  aclWhite                = $FFFFFFFF;
  aclWhiteSmoke           = $FFF5F5F5;
  aclYellow               = $FFFFFF00;
  aclYellowGreen          = $FF9ACD32;

implementation
{$IFDEF CAIRO_HAS_RSVG_FUNCTIONS}
uses rsvglib, math;
{$ENDIF}

procedure CairoCheck(AStatus: TCairoStatus);
begin
  if AStatus <> CAIRO_STATUS_SUCCESS then
    raise ECairoException.Create(AStatus);
end;

function WriteStream(stream: TStream; data: Pointer; length: Cardinal): TCairoStatus; cdecl;
begin
  if stream.Write(data^, length) = Integer(length) then
    Result := CAIRO_STATUS_SUCCESS else
    Result := CAIRO_STATUS_WRITE_ERROR;
end;

function ReadStream(stream: TStream; data: Pointer; length: Cardinal): TCairoStatus; cdecl;
begin
  if stream.Read(data^, length) = Integer(length) then
    Result := CAIRO_STATUS_SUCCESS else
    Result := CAIRO_STATUS_READ_ERROR;
end;

function CairoFormatStrideForWidth(format: TCairoFormat; width: Integer): Integer;
begin
  Result := cairo_format_stride_for_width(format, width)
end;

{ TWin32Surface }

{$ifdef CAIRO_HAS_WIN32_SURFACE}
constructor TWin32Surface.CreateHDC(hdc: HDC);
begin
  inherited CreateInternal(cairo_win32_surface_create(hdc));
end;
{$endif}

{$ifdef CAIRO_HAS_WIN32_SURFACE}
constructor TWin32Surface.CreatePrinting(hdc: HDC);
begin
  inherited CreateInternal(cairo_win32_printing_surface_create(hdc));
end;
{$endif}

{$ifdef CAIRO_HAS_WIN32_SURFACE}
constructor TWin32Surface.CreateWithDDB(hdc: HDC; format: TCairoFormat; width,
  height: Integer);
begin
  inherited CreateInternal(cairo_win32_surface_create_with_ddb(hdc, format, width, height));
end;
{$endif}

{$ifdef CAIRO_HAS_WIN32_SURFACE}
constructor TWin32Surface.CreateWithDIB(format: TCairoFormat; width,
  height: Integer);
begin
  inherited CreateInternal(cairo_win32_surface_create_with_dib(format, width, height));
end;
{$endif}

{$ifdef CAIRO_HAS_WIN32_SURFACE}
function TWin32Surface.GetDC: HDC;
begin
  Result := cairo_win32_surface_get_dc(FSurface);
end;
{$endif}

{$ifdef CAIRO_HAS_WIN32_SURFACE}
function TWin32Surface.GetImage: ICairoSurface;
begin
  Result := init(cairo_win32_surface_get_image(FSurface))
end;
{$endif}

{ TCairoSurface }

procedure TCairoSurface.CopyPage;
begin
  cairo_surface_copy_page(FSurface);
end;

function TCairoSurface.CreateForRectangle(x, y, width,
  height: Double): ICairoSurface;
begin
  Result := init(cairo_surface_create_for_rectangle(FSurface, x, y, width, height))
end;

constructor TCairoSurface.CreateInternal(surface: PCairoSurface);
begin
  FSurface := surface;
  cairo_surface_set_user_data(FSurface, nil, Self, nil);
end;

constructor TCairoSurface.CreateRecording(content: TCairoContent;
  const extents: PCairoRectangle);
begin
  CreateInternal(cairo_recording_surface_create(content, extents));
end;

function TCairoSurface.CreateSimilar(content: TCairoContent; width,
  height: Integer): ICairoSurface;
begin
  Result := init(cairo_surface_create_similar(FSurface, content, width, height));
end;

destructor TCairoSurface.Destroy;
begin
  cairo_surface_set_user_data(FSurface, nil, nil, nil);
  cairo_surface_destroy(FSurface);
  inherited;
end;

procedure TCairoSurface.Finish;
begin
  cairo_surface_finish(FSurface);
end;

procedure TCairoSurface.Flush;
begin
  cairo_surface_flush(FSurface);
end;

function TCairoSurface.GetContent: TCairoContent;
begin
  Result := cairo_surface_get_content(FSurface)
end;

function TCairoSurface.GetDevice: ICairoDevice;
begin
  Result := TCairoDevice.init(cairo_surface_get_device(FSurface));
end;

procedure TCairoSurface.GetDeviceOffset(var xOffset, yOffset: Double);
begin
  cairo_surface_get_device_offset(FSurface, @xOffset, @yOffset);
end;

procedure TCairoSurface.GetFallbackResolution(var xPixelsPerInch,
  yPixelsPerInch: Double);
begin
  cairo_surface_get_fallback_resolution(FSurface, @xPixelsPerInch, @yPixelsPerInch);
end;

procedure TCairoSurface.GetFontOptions(options: ICairoFontOptions);
begin
  cairo_surface_get_font_options(FSurface, TCairoFontOptions(options.GetUserData).FFontOptions);
end;

procedure TCairoSurface.GetMimeData(const mimeType: PAnsiChar;
  out data: Pointer; out length: Cardinal);
begin
  cairo_surface_get_mime_data(FSurface, mimeType, data, length);
end;

function TCairoSurface.GetType: TCairoSurfaceType;
begin
  Result := cairo_surface_get_type(FSurface);
end;

function TCairoSurface.GetUserData(key: Pointer): Pointer;
begin
  Result := cairo_surface_get_user_data(FSurface, key);
end;

function TCairoSurface.HasShowTextGlyphs: Boolean;
begin
  Result := cairo_surface_has_show_text_glyphs(FSurface) <> 0;
end;

class function TCairoSurface.init(surface: PCairoSurface): TCairoSurface;
begin
  Result := TCairoSurface(cairo_surface_get_user_data(surface, nil));
  if Result = nil then
  begin
    cairo_surface_reference(surface);
    Result := TCairoSurface.CreateInternal(surface);
  end;
end;

procedure TCairoSurface.MarkDirty;
begin
  cairo_surface_mark_dirty(FSurface);
end;

procedure TCairoSurface.MarkDirtyRectangle(x, y, width, height: Integer);
begin
  cairo_surface_mark_dirty_rectangle(FSurface, x, y, width, height);
end;

procedure TCairoSurface.SetDeviceOffset(xOffset, yOffset: double);
begin
  cairo_surface_set_device_offset(FSurface, xOffset, yOffset);
end;

procedure TCairoSurface.SetFallbackResolution(xPixelsPerInch,
  yPixelsPerInch: double);
begin
  cairo_surface_set_fallback_resolution(FSurface, xPixelsPerInch, yPixelsPerInch);
end;

function TCairoSurface.SetMimeData(const mimeType: PAnsiChar;
  const data: Pointer; length: Cardinal; destroy: TCairoDestroyFunc;
  closure: Pointer): TCairoStatus;
begin
  Result := cairo_surface_set_mime_data(FSurface, mimeType, data, length, destroy, closure)
end;

procedure TCairoSurface.SetUserData(key, userData: Pointer;
  destroyFunc: TCairoDestroyFunc);
begin
  Assert(key <> nil, 'null key is reserved');
  CairoCheck(cairo_surface_set_user_data(FSurface, key, userData, destroyFunc));
end;

procedure TCairoSurface.ShowPage;
begin
  cairo_surface_show_page(FSurface);
end;

function TCairoSurface.GetStatus: TCairoStatus;
begin
  result := cairo_surface_status(FSurface)
end;

{$ifdef CAIRO_HAS_PNG_FUNCTIONS}
procedure TCairoSurface.WriteToPNGStream(stream: TStream);
begin
  CairoCheck(cairo_surface_write_to_png_stream(FSurface, TCairoWriteFunc(@writestream), stream));
end;
{$endif}

{$ifdef CAIRO_HAS_PNG_FUNCTIONS}
procedure TCairoSurface.WriteToPNG(const filename: string);
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(filename, fmCreate, fmShareExclusive);
  try
    WriteToPNGStream(stream);
  finally
    stream.Free;
  end;
end;
{$endif}

{ ECairoException }

constructor ECairoException.Create(AStatus: TCairoStatus);
begin
  FStatus := AStatus;
  inherited Create(string(cairo_status_to_string(AStatus)));
end;

{ TCairoImageSurface }

constructor TImageSurface.Create(format: TCairoFormat; width, height: Integer);
begin
  inherited CreateInternal(cairo_image_surface_create(format, width, height));
end;

constructor TImageSurface.CreateForData(data: Pointer; format: TCairoFormat;
  width, height, stride: Integer);
begin
  inherited CreateInternal(cairo_image_surface_create_for_data(data, format, width, height, stride));
end;

{$ifdef CAIRO_HAS_PNG_FUNCTIONS}
constructor TImageSurface.CreateFromPNG(const filename: string);
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(filename, fmOpenRead, fmShareDenyWrite);
  try
    CreateFromPNG(stream);
  finally
    stream.Free;
  end;
end;
{$endif}

{$ifdef CAIRO_HAS_PNG_FUNCTIONS}
constructor TImageSurface.CreateFromPNG(stream: TStream);
begin
  inherited CreateInternal(cairo_image_surface_create_from_png_stream(TCairoReadFunc(@ReadStream), stream));
end;
{$endif}

{$ifdef CAIRO_HAS_RSVG_FUNCTIONS}
constructor TImageSurface.CreateFromSVG(const filename: string; format: TCairoFormat; scale: double);
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(filename, fmOpenRead, fmShareDenyWrite);
  try
    CreateFromSVG(stream, format, scale);
  finally
    stream.Free;
  end;
end;

constructor TImageSurface.CreateFromSVG(stream: TStream; format: TCairoFormat; scale: double);
var
  handle: PRsvgHandle;
  dim: TRsvgDimensionData;
  newctx: PCairo;
  buffer: Pointer;
begin
  if stream is TCustomMemoryStream then
    handle := rsvg_handle_new_from_data(TCustomMemoryStream(stream).Memory, stream.Size, nil) else
    begin
      GetMem(buffer, stream.Size);
      try
        stream.Read(buffer^, stream.Size);
        handle := rsvg_handle_new_from_data(buffer, stream.Size, nil);
      finally
        FreeMem(buffer);
      end;
    end;
  try
    Assert(handle <> nil);
    rsvg_handle_get_dimensions(handle, @dim);
    inherited CreateInternal(cairo_image_surface_create(format, round(dim.width * scale), round(dim.height * scale)));
    newctx := cairo_create(FSurface);
    try
      cairo_scale(newctx, scale, scale);
      rsvg_handle_render_cairo(handle, newctx);
    finally
      cairo_destroy(newctx);
    end;
  finally
    rsvg_handle_free(handle);
  end;
end;
{$endif}

function TImageSurface.GetData: Pointer;
begin
  Result := cairo_image_surface_get_data(FSurface);
end;

function TImageSurface.GetFormat: TCairoFormat;
begin
  Result := cairo_image_surface_get_format(FSurface);
end;

function TImageSurface.GetHeight: Integer;
begin
  Result := cairo_image_surface_get_height(FSurface);
end;

function TImageSurface.GetStride: Integer;
begin
  Result := cairo_image_surface_get_stride(FSurface);
end;

function TImageSurface.GetWidth: Integer;
begin
  Result := cairo_image_surface_get_width(FSurface);
end;

{ TSVGSurface }

{$ifdef CAIRO_HAS_SVG_SURFACE}
constructor TSVGSurface.Create(stream: TStream; widthInPoints, heightInPoints: Double; owned: boolean);
begin
  inherited CreateInternal(cairo_svg_surface_create_for_stream(TCairoWriteFunc(@WriteStream), stream, widthInPoints, heightInPoints));
  if owned then
    FStream := stream else
    FStream := nil;
end;
{$endif}

{$ifdef CAIRO_HAS_SVG_SURFACE}
constructor TSVGSurface.Create(filename: TFileName; widthInPoints, heightInPoints: Double);
begin
  Create(TFileStream.Create(filename, fmCreate, fmShareExclusive), widthInPoints, heightInPoints, True);
end;
{$endif}

{$ifdef CAIRO_HAS_SVG_SURFACE}
destructor TSVGSurface.Destroy;
begin
  inherited;
  if FStream <> nil then
    FStream.Free;
end;
{$endif}

{$ifdef CAIRO_HAS_SVG_SURFACE}
procedure TSVGSurface.RestrictToVersion(version: TCairoSVGVersion);
begin
  cairo_svg_surface_restrict_to_version(FSurface, version);
end;
{$endif}

{ TPDFSurface }

{$ifdef CAIRO_HAS_PDF_SURFACE}
constructor TPDFSurface.Create(stream: TStream; widthInPoints,
  heightInPoints: Double; owned: boolean);
begin
  inherited CreateInternal(cairo_pdf_surface_create_for_stream(TCairoWriteFunc(@WriteStream), stream, widthInPoints, heightInPoints));
  if owned then
    FStream := stream else
    FStream := nil;
end;
{$endif}

{$ifdef CAIRO_HAS_PDF_SURFACE}
constructor TPDFSurface.Create(filename: TFileName; widthInPoints,
  heightInPoints: Double);
begin
  Create(TFileStream.Create(filename, fmCreate, fmShareExclusive), widthInPoints, heightInPoints, True);
end;
{$endif}

{$ifdef CAIRO_HAS_PDF_SURFACE}
destructor TPDFSurface.Destroy;
begin
  inherited;
  if FStream <> nil then
    FStream.Free;
end;
{$endif}

{$ifdef CAIRO_HAS_PDF_SURFACE}
procedure TPDFSurface.SetSize(widthInPoints, heightInPoints: Double);
begin
  cairo_pdf_surface_set_size(FSurface, widthInPoints, heightInPoints);
end;
{$endif}

{$ifdef CAIRO_HAS_PDF_SURFACE}
procedure TPDFSurface.RestrictToVersion(version: TCairoPdfVersion);
begin
  cairo_pdf_surface_restrict_to_version(FSurface, version);
end;
{$endif}

{ TPostScriptSurface }

{$ifdef CAIRO_HAS_PS_SURFACE}
constructor TPostScriptSurface.Create(stream: TStream; widthInPoints,
  heightInPoints: Double; owned: boolean);
begin
  inherited CreateInternal(cairo_ps_surface_create_for_stream(TCairoWriteFunc(@WriteStream), stream, widthInPoints, heightInPoints));
  if owned then
    FStream := stream else
    FStream := nil;
end;
{$endif}

{$ifdef CAIRO_HAS_PS_SURFACE}
constructor TPostScriptSurface.Create(const filename: string; widthInPoints,
  heightInPoints: Double);
begin
  Create(TFileStream.Create(filename, fmCreate, fmShareExclusive), widthInPoints, heightInPoints, True);
end;
{$endif}

{$ifdef CAIRO_HAS_PS_SURFACE}
destructor TPostScriptSurface.Destroy;
begin
  inherited;
  if FStream <> nil then
    FStream.Free;
end;
{$endif}

{$ifdef CAIRO_HAS_PS_SURFACE}
procedure TPostScriptSurface.DscBeginPageSetup;
begin
  cairo_ps_surface_dsc_begin_page_setup(FSurface);
end;
{$endif}

{$ifdef CAIRO_HAS_PS_SURFACE}
procedure TPostScriptSurface.DscBeginSetup;
begin
  cairo_ps_surface_dsc_begin_setup(FSurface);
end;
{$endif}

{$ifdef CAIRO_HAS_PS_SURFACE}
procedure TPostScriptSurface.DscComment(const comment: AnsiString);
begin
  cairo_ps_surface_dsc_comment(FSurface, PAnsiChar(comment));
end;
{$endif}

{$ifdef CAIRO_HAS_PS_SURFACE}
function TPostScriptSurface.GetEPS: Boolean;
begin
  Result := cairo_ps_surface_get_eps(FSurface) <> 0;
end;
{$endif}

{$ifdef CAIRO_HAS_PS_SURFACE}
procedure TPostScriptSurface.RestrictToLevel(level: TCairoPSLevel);
begin
  cairo_ps_surface_restrict_to_level(FSurface, level);
end;
{$endif}

{$ifdef CAIRO_HAS_PS_SURFACE}
procedure TPostScriptSurface.SetEPS(eps: Boolean);
begin
  cairo_ps_surface_set_eps(FSurface, ord(eps));
end;
{$endif}

{$ifdef CAIRO_HAS_PS_SURFACE}
procedure TPostScriptSurface.SetSize(widthInPoints, heightInPoints: Double);
begin
  cairo_ps_surface_set_size(FSurface, widthInPoints, heightInPoints);
end;
{$endif}

{ TCairoContext }

procedure TCairoContext.AppendPath(path: PCairoPath);
begin
  cairo_append_path(FContext, path)
end;

{$IFDEF CAIRO_HAS_RSVG_FUNCTIONS}
function TCairoContext.RenderSVG(svg: IRSVGObject; id: RawByteString): Boolean;
begin
  if id <> '' then
    Result := rsvg_handle_render_cairo_sub(svg.Handle, FContext, PAnsiChar(id)) else
    Result := rsvg_handle_render_cairo(svg.Handle, FContext);
end;
{$ENDIF}


procedure TCairoContext.Arc(xc, yc, radius, angle1, angle2: Double);
begin
  cairo_arc(FContext, xc, yc, radius, angle1, angle2);
end;

procedure TCairoContext.ArcNegative(xc, yc, radius, angle1, angle2: Double);
begin
  cairo_arc_negative(FContext, xc, yc, radius, angle1, angle2);
end;

procedure TCairoContext.Clip;
begin
  cairo_clip(FContext);
end;

procedure TCairoContext.ClipExtents(x1, y1, x2, y2: PDouble);
begin
  cairo_clip_extents(FContext, x1, y1, x2, y2);
end;

procedure TCairoContext.ClipPreserve;
begin
  cairo_clip_preserve(FContext);
end;

procedure TCairoContext.ClosePath;
begin
  cairo_close_path(FContext);
end;

function TCairoContext.CopyClipRectangleList: PCairoRectangleList;
begin
  Result := cairo_copy_clip_rectangle_list(FContext);
end;

procedure TCairoContext.CopyPage;
begin
  cairo_copy_page(FContext);
end;

function TCairoContext.CopyPath: PCairoPath;
begin
  Result := cairo_copy_path(FContext);
end;

function TCairoContext.CopyPathFlat: PCairoPath;
begin
  Result := cairo_copy_path_flat(FContext);
end;

constructor TCairoContext.Create(surface: ICairoSurface);
begin
  CreateInternal(cairo_create(TCairoSurface(surface.GetUserData(nil)).FSurface));
end;

constructor TCairoContext.CreateInternal(context: PCairo);
begin
  FContext := context;
  cairo_set_user_data(FContext, nil, Self, nil);
end;

procedure TCairoContext.CurveTo(x1, y1, x2, y2, x3, y3: Double);
begin
  cairo_curve_to(FContext, x1, y1, x2, y2, x3, y3);
end;

destructor TCairoContext.Destroy;
begin
  cairo_set_user_data(FContext, nil, nil, nil);
  cairo_destroy(FContext);
  inherited;
end;

procedure TCairoContext.DeviceToUser(var x, y: Double);
begin
  cairo_device_to_user(FContext, @x, @y);
end;

procedure TCairoContext.DeviceToUserDistance(var dx, dy: Double);
begin
  cairo_device_to_user_distance(FContext, @dx, @dy);
end;

procedure TCairoContext.Fill;
begin
  cairo_fill(FContext);
end;

procedure TCairoContext.FillExtents(x1, y1, x2, y2: PDouble);
begin
  cairo_fill_extents(FContext, x1, y1, x2, y2);
end;

procedure TCairoContext.FillPreserve;
begin
  cairo_fill_preserve(FContext);
end;

procedure TCairoContext.FontExtents(extents: PCairoFontExtents);
begin
  cairo_font_extents(FContext, extents);
end;

function TCairoContext.GetAntialias: TCairoAntialias;
begin
  Result := cairo_get_antialias(FContext);
end;

procedure TCairoContext.GetCurrentPoint(x, y: PDouble);
begin
  cairo_get_current_point(FContext, x, y);
end;

procedure TCairoContext.GetDash(dashes, offset: PDouble);
begin
  cairo_get_dash(FContext, dashes, offset);
end;

function TCairoContext.GetDashCount: Integer;
begin
  Result := cairo_get_dash_count(FContext);
end;

function TCairoContext.GetFillRule: TCairoFillRule;
begin
  Result := cairo_get_fill_rule(FContext);
end;

function TCairoContext.GetFontFace: ICairoFontFace;
begin
  Result := TCairoFontFace.init(cairo_get_font_face(FContext));
end;

procedure TCairoContext.GetFontMatrix(var matrix: TCairoMatrix);
begin
  cairo_get_font_matrix(FContext, @matrix);
end;

function TCairoContext.GetFontOptions: ICairoFontOptions;
var
  fontoptions: PCairoFontOptions;
begin
  fontoptions := cairo_font_options_create;
  cairo_get_font_options(FContext, fontoptions);
  Result := TCairoFontOptions.CreateInternal(fontoptions);
end;

function TCairoContext.GetGroupTarget: ICairoSurface;
begin
  Result := TcairoSurface.init(cairo_get_group_target(FContext));
end;

function TCairoContext.GetLineCap: TCairoLineCap;
begin
  Result := cairo_get_line_cap(FContext);
end;

function TCairoContext.GetLineJoin: TCairoLineJoin;
begin
  Result := cairo_get_line_join(FContext);
end;

function TCairoContext.GetLineWidth: Double;
begin
  Result := cairo_get_line_width(FContext);
end;

procedure TCairoContext.GetMatrix(var matrix: TCairoMatrix);
begin
  cairo_get_matrix(FContext, @matrix);
end;

function TCairoContext.GetMiterLimit: Double;
begin
  Result := cairo_get_miter_limit(FContext);
end;

function TCairoContext.GetOperator: TCairoOperator;
begin
  Result := cairo_get_operator(FContext)
end;

function TCairoContext.GetScaledFont: ICairoScaledFont;
begin
  Result := TCairoScaledFont.init(cairo_get_scaled_font(FContext))
end;

function TCairoContext.GetSource: ICairoPattern;
begin
  Result := TCairoPattern.init(cairo_get_source(FContext));
end;

function TCairoContext.GetStatus: TCairoStatus;
begin
  Result := cairo_status(FContext);
end;

function TCairoContext.GetTarget: ICairoSurface;
begin
  Result := TCairoSurface.init(cairo_get_target(FContext))
end;

function TCairoContext.GetTolerance: Double;
begin
  Result := cairo_get_tolerance(FContext);
end;

function TCairoContext.GetUserData(key: Pointer): Pointer;
begin
  Result := cairo_get_user_data(FContext, key);
end;

procedure TCairoContext.GlyphExtents(glyphs: PCairoGlyph;
  numGlyphs: Integer; extent: PCairoTextExtents);
begin
  cairo_glyph_extents(FContext, glyphs, numGlyphs, extent);
end;

procedure TCairoContext.GlyphPath(glyphs: PCairoGlyph;
  numGlyphs: Integer);
begin
  cairo_glyph_path(FContext, glyphs, numGlyphs);
end;

function TCairoContext.HasCurrentPoint: Boolean;
begin
  Result := cairo_has_current_point(FContext) <> 0;
end;

procedure TCairoContext.IdentityMatrix;
begin
  cairo_identity_matrix(FContext);
end;

function TCairoContext.InClip(x, y: Double): Boolean;
begin
  Result := cairo_in_clip(FContext, x, y) <> 0;
end;

function TCairoContext.InFill(x, y: Double): Boolean;
begin
  Result := cairo_in_fill(FContext, x, y) <> 0;
end;

//class function TCairoContext.init(context: PCairo): TCairoContext;
//begin
//  Result := cairo_get_user_data(context, nil);
//  if Result = nil then
//    Result := TCairoContext.CreateInternal(context);
//end;

function TCairoContext.InStroke(x, y: Double): Boolean;
begin
  Result := cairo_in_stroke(FContext, x, y) <> 0;
end;

procedure TCairoContext.LineTo(x, y: Double);
begin
  cairo_line_to(FContext, x, y);
end;

procedure TCairoContext.Mask(pattern: ICairoPattern);
begin
  cairo_mask(FContext, TCairoPattern(pattern.GetUserData(nil)).FPattern);
end;

procedure TCairoContext.MaskSurface(surface: ICairoSurface; surfaceX,
  surfaceY: Double);
begin
  cairo_mask_surface(FContext, TCairoSurface(surface.GetUserData(nil)).FSurface, surfaceX, surfaceY);
end;

procedure TCairoContext.MoveTo(x, y: Double);
begin
  cairo_move_to(FContext, x, y);
end;

procedure TCairoContext.NewPath;
begin
  cairo_new_path(FContext);
end;

procedure TCairoContext.NewSubPath;
begin
  cairo_new_sub_path(FContext);
end;

procedure TCairoContext.Paint;
begin
  cairo_paint(FContext);
end;

procedure TCairoContext.PaintWithAlpha(alpha: Double);
begin
  cairo_paint_with_alpha(FContext, alpha);
end;

procedure TCairoContext.PathExtents(x1, y1, x2, y2: PDouble);
begin
  cairo_path_extents(FContext, x1, y1, x2, y2);
end;

function TCairoContext.PopGroup: ICairoPattern;
begin
  Result := TCairoPattern.init(cairo_pop_group(FContext));
end;

procedure TCairoContext.PopGroupToSource;
begin
  cairo_pop_group_to_source(FContext)
end;

procedure TCairoContext.PushGroup;
begin
  cairo_push_group(FContext);
end;

procedure TCairoContext.PushGroupWithContent(content: TCairoContent);
begin
  cairo_push_group_with_content(FContext, content);
end;

procedure TCairoContext.Rectangle(x, y, width, height: Double);
begin
  cairo_rectangle(FContext, x, y, width, height);
end;

procedure TCairoContext.RelCurveTo(dx1, dy1, dx2, dy2, dx3, dy3: Double);
begin
  cairo_rel_curve_to(FContext, dx1, dy1, dx2, dy2, dx3, dy3);
end;

procedure TCairoContext.RelLineTo(dx, dy: Double);
begin
  cairo_rel_line_to(FContext, dx, dy);
end;

procedure TCairoContext.RelMoveTo(dx, dy: Double);
begin
  cairo_rel_move_to(FContext, dx, dy);
end;

procedure TCairoContext.ResetClip;
begin
  cairo_reset_clip(FContext);
end;

procedure TCairoContext.Restore;
begin
  cairo_restore(FContext);
end;

procedure TCairoContext.Rotate(angle: Double);
begin
  cairo_rotate(FContext, angle);
end;

procedure TCairoContext.Save;
begin
  cairo_save(FContext);
end;

procedure TCairoContext.Scale(sx, sy: Double);
begin
  cairo_scale(FContext, sx, sy);
end;

procedure TCairoContext.SelectFontFace(const family: AnsiString;
  slant: TCairoFontSlant; weight: TCairoFontWeight);
begin
  cairo_select_font_face(FContext, PAnsiChar(family), slant, weight);
end;

procedure TCairoContext.SetAntialias(antialias: TCairoAntialias);
begin
  cairo_set_antialias(FContext, antialias);
end;

procedure TCairoContext.SetDash(dashes: PDouble; numDashes: Integer;
  offset: Double);
begin
  cairo_set_dash(FContext, dashes, numDashes, offset);
end;

procedure TCairoContext.SetFillRule(fillRule: TCairoFillRule);
begin
  cairo_set_fill_rule(FContext, fillRule);
end;

procedure TCairoContext.SetFontFace(fontFace: ICairoFontFace);
begin
   cairo_set_font_face(FContext, TCairoFontFace(fontface.GetUserData(nil)).FFontFace);
end;

procedure TCairoContext.SetFontMatrix(matrix: TCairoMatrix);
begin
  cairo_set_font_matrix(FContext, @matrix);
end;

procedure TCairoContext.SetFontOptions(options: ICairoFontOptions);
begin
  cairo_set_font_options(FContext, TCairoFontOptions(options.GetUserData).FFontOptions);
end;

procedure TCairoContext.SetFontSize(size: Double);
begin
  cairo_set_font_size(FContext, size)
end;

procedure TCairoContext.SetLineCap(lineCap: TCairoLineCap);
begin
  cairo_set_line_cap(FContext, lineCap);
end;

procedure TCairoContext.SetLineJoin(lineJoin: TCairoLineJoin);
begin
  cairo_set_line_join(FContext, lineJoin);
end;

procedure TCairoContext.SetLineWidth(width: Double);
begin
  cairo_set_line_width(FContext, width);
end;

procedure TCairoContext.SetMatrix(matrix: TCairoMatrix);
begin
  cairo_set_matrix(FContext, @matrix);
end;

procedure TCairoContext.SetMiterLimit(limit: Double);
begin
  cairo_set_miter_limit(FContext, limit);
end;

procedure TCairoContext.SetOperator(op: TCairoOperator);
begin
  cairo_set_operator(FContext, op);
end;

procedure TCairoContext.SetScaledFont(scaledFont: ICairoScaledFont);
begin
  cairo_set_scaled_font(FContext, TCairoScaledFont(scaledFont.GetUserData(nil)).FScaledFont);
end;

procedure TCairoContext.SetSource(source: ICairoPattern);
begin
  cairo_set_source(FContext, TCairoPattern(source.GetUserData(nil)).FPattern);
end;

procedure TCairoContext.SetSourceColor(color: Cardinal);
var
  alpha, red, green, blue: byte;
begin
  alpha := (color shr 24) and $FF;
  red := (color shr 16) and $FF;
  green := (color shr 8) and $FF;
  blue := color and $FF;
  if alpha = $ff then
    SetSourceRGB(red/255, green/255, blue/255) else
    SetSourceRGBA(red/255, green/255, blue/255, alpha/255);
end;

procedure TCairoContext.SetSourceRGB(red, green, blue: Double);
begin
  cairo_set_source_rgb(FContext, red, green, blue);
end;

procedure TCairoContext.SetSourceRGBA(red, green, blue, alpha: Double);
begin
  cairo_set_source_rgba(FContext, red, green, blue, alpha);
end;

procedure TCairoContext.SetSourceSurface(surface: ICairoSurface; x, y: Double);
begin
  cairo_set_source_surface(FContext, TCairoSurface(surface.GetUserData(nil)).FSurface, x, y);
end;

procedure TCairoContext.SetTolerance(tolerance: Double);
begin
  cairo_set_tolerance(FContext, tolerance);
end;

procedure TCairoContext.SetUserData(key, userData: Pointer;
  destroyFunc: TCairoDestroyFunc);
begin
  Assert(key <> nil, 'null key is reserved');
  CairoCheck(cairo_set_user_data(FContext, key, userData, destroyFunc));
end;

procedure TCairoContext.ShowGlyphs(glyphs: PCairoGlyph; numGlyphs: Integer);
begin
  cairo_show_glyphs(FContext, glyphs, numGlyphs);
end;

procedure TCairoContext.ShowPage;
begin
  cairo_show_page(FContext);
end;

procedure TCairoContext.ShowText(utf8: UTF8String);
begin
  cairo_show_text(FContext, PAnsiChar(utf8));
end;

procedure TCairoContext.ShowTextGlyphs(utf8: UTF8String;
  glyphs: PCairoGlyph; numGlyphs: Integer; clusters: PCairoTextCluster;
  numClusters: Integer; clusterFlags: TCairoTextClusterFlags);
begin
  cairo_show_text_glyphs(FContext, PAnsiChar(utf8), Length(utf8), glyphs,
    numGlyphs, clusters, numClusters, clusterFlags);
end;

procedure TCairoContext.Stroke;
begin
  cairo_stroke(FContext);
end;

procedure TCairoContext.StrokeExtents(x1, y1, x2, y2: PDouble);
begin
  cairo_stroke_extents(FContext, x1, y1, x2, y2);
end;

procedure TCairoContext.StrokePreserve;
begin
  cairo_stroke_preserve(FContext);
end;

procedure TCairoContext.TextExtents(utf8: UTF8String;
  extent: PCairoTextExtents);
begin
  cairo_text_extents(FContext, PAnsiChar(utf8), extent);
end;

procedure TCairoContext.TextPath(utf8: UTF8String);
begin
  cairo_text_path(FContext, PAnsiChar(utf8));
end;

procedure TCairoContext.Transform(matrix: TCairoMatrix);
begin
  cairo_transform(FContext, @matrix)
end;

procedure TCairoContext.Translate(tx, ty: Double);
begin
  cairo_translate(FContext, tx, ty);
end;

procedure TCairoContext.UserToDevice(var x, y: Double);
begin
  cairo_user_to_device(FContext, @x, @y);
end;

procedure TCairoContext.UserToDeviceDistance(var dx, dy: Double);
begin
  cairo_user_to_device_distance(FContext, @dx, @dy);
end;

{ TCairoFontOptions }

function TCairoFontOptions.Copy: ICairoFontOptions;
begin
  Result := TCairoFontOptions.CreateInternal(cairo_font_options_copy(FFontOptions));
end;

constructor TCairoFontOptions.Create;
begin
  CreateInternal(cairo_font_options_create);
end;

constructor TCairoFontOptions.CreateInternal(options: PCairoFontOptions);
begin
  FFontOptions := options;
end;

destructor TCairoFontOptions.Destroy;
begin
  cairo_font_options_destroy(FFontOptions);
  inherited;
end;

function TCairoFontOptions.Equal(other: ICairoFontOptions): Boolean;
begin
  Result := cairo_font_options_equal(FFontOptions, TCairoFontOptions(other.GetUserData).FFontOptions) <> 0;
end;

function TCairoFontOptions.GetAntialias: TCairoAntialias;
begin
  Result := cairo_font_options_get_antialias(FFontOptions);
end;

function TCairoFontOptions.GetHash: Cardinal;
begin
  Result := cairo_font_options_hash(FFontOptions);
end;

function TCairoFontOptions.GetHintMetrics: TCairoHintMetrics;
begin
  Result := cairo_font_options_get_hint_metrics(FFontOptions);
end;

function TCairoFontOptions.GetHintStyle: TCairoHintStyle;
begin
  Result := cairo_font_options_get_hint_style(FFontOptions);
end;

function TCairoFontOptions.GetStatus: TCairoStatus;
begin
  Result := cairo_font_options_status(FFontOptions);
end;

function TCairoFontOptions.GetSubpixelOrder: TCairoSubpixelOrder;
begin
  Result := cairo_font_options_get_subpixel_order(FFontOptions);
end;

function TCairoFontOptions.GetUserData: Pointer;
begin
  Result := Self;
end;

procedure TCairoFontOptions.Merge(other: ICairoFontOptions);
begin
  cairo_font_options_merge(FFontOptions, TCairoFontOptions(other.GetUserData).FFontOptions);
end;

procedure TCairoFontOptions.SetAntialias(antialias: TCairoAntialias);
begin
  cairo_font_options_set_antialias(FFontOptions, antialias);
end;

procedure TCairoFontOptions.SetHintMetrics(hintMetrics: TCairoHintMetrics);
begin
  cairo_font_options_set_hint_metrics(FFontOptions, hintMetrics);
end;

procedure TCairoFontOptions.SetHintStyle(hintStyle: TCairoHintStyle);
begin
  cairo_font_options_set_hint_style(FFontOptions, hintStyle);
end;

procedure TCairoFontOptions.SetSubpixelOrder(
  subpixelOrder: TCairoSubpixelOrder);
begin
  cairo_font_options_set_subpixel_order(FFontOptions, subpixelOrder);
end;

{ TCairoPattern }

procedure TCairoPattern.AddColorStop(offset: Double; color: Cardinal);
var
  r, g, b, a: Byte;
begin
  a := color shr 24;
  r := color shr 16;
  g := color shr 8;
  b := color;
  if a = $ff then
    cairo_pattern_add_color_stop_rgb(FPattern, offset, r/255, g/255, b/255);
    cairo_pattern_add_color_stop_rgba(FPattern, offset, r/255, g/255, b/255, a/255);
end;

procedure TCairoPattern.AddColorStopRGB(offset, red, green, blue: Double);
begin
  cairo_pattern_add_color_stop_rgb(FPattern, offset, red, green, blue);
end;

procedure TCairoPattern.AddColorStopRGBA(offset, red, green, blue,
  alpha: Double);
begin
  cairo_pattern_add_color_stop_rgba(FPattern, offset, red, green, blue, alpha);
end;

constructor TCairoPattern.CreateForSurface(surface: ICairoSurface);
begin
  CreateInternal(cairo_pattern_create_for_surface(TCairoSurface(surface.GetUserData(nil)).FSurface));
end;

constructor TCairoPattern.CreateInternal(pattern: PCairoPattern);
begin
  FPattern := pattern;
  cairo_pattern_set_user_data(FPattern, nil, Self, nil);
end;

constructor TCairoPattern.CreateLinear(x0, y0, x1, y1: Double);
begin
  CreateInternal(cairo_pattern_create_linear(x0, y0, x1, y1));
end;

constructor TCairoPattern.CreateRadial(cx0, cy0, radius0, cx1, cy1,
  radius1: Double);
begin
  CreateInternal(cairo_pattern_create_radial(cx0, cy0, radius0, cx1, cy1, radius1));
end;

constructor TCairoPattern.CreateRGB(red, green, blue: Double);
begin
  CreateInternal(cairo_pattern_create_rgb(red, green, blue));
end;

constructor TCairoPattern.CreateRGBA(red, green, blue, alpha: Double);
begin
  CreateInternal(cairo_pattern_create_rgba(red, green, blue, alpha));
end;

destructor TCairoPattern.Destroy;
begin
  cairo_pattern_set_user_data(FPattern, nil, nil, nil);
  cairo_pattern_destroy(FPattern);
  inherited;
end;

function TCairoPattern.GetColorStopCount: Integer;
begin
  CairoCheck(cairo_pattern_get_color_stop_count(FPattern, @Result));
end;

procedure TCairoPattern.GetColorStopRGBA(index: Integer; offset, red, green,
  blue, alpha: PDouble);
begin
  CairoCheck(cairo_pattern_get_color_stop_rgba(FPattern, index, offset, red, green, blue, alpha));
end;

function TCairoPattern.GetExtend: TCairoExtend;
begin
  Result := cairo_pattern_get_extend(FPattern);
end;

function TCairoPattern.GetFilter: TCairoFilter;
begin
  Result := cairo_pattern_get_filter(FPattern);
end;

procedure TCairoPattern.GetLinearPoints(x0, y0, x1, y1: PDouble);
begin
  CairoCheck(cairo_pattern_get_linear_points(FPattern, x0, y0, x1, y1));
end;

procedure TCairoPattern.GetMatrix(var matrix: TCairoMatrix);
begin
  cairo_pattern_get_matrix(FPattern, @matrix);
end;

procedure TCairoPattern.GetRadialCircles(x0, y0, r0, x1, y1, r1: PDouble);
begin
  CairoCheck(cairo_pattern_get_radial_circles(FPattern, x0, y0, r0, x1, y1, r1));
end;

procedure TCairoPattern.GetRGBA(red, green, blue, alpha: PDouble);
begin
  CairoCheck(cairo_pattern_get_rgba(FPattern, red, green, blue, alpha));
end;

function TCairoPattern.GetStatus: TCairoStatus;
begin
  Result := cairo_pattern_status(FPattern);
end;

function TCairoPattern.GetSurface: ICairoSurface;
var
  psurface: PCairoSurface;
begin
  CairoCheck(cairo_pattern_get_surface(FPattern, psurface));
  Result := TCairoSurface.init(psurface);
end;

function TCairoPattern.GetType: TCairoPatternType;
begin
  Result := cairo_pattern_get_type(FPattern);
end;

function TCairoPattern.GetUserData(key: Pointer): Pointer;
begin
  Result := cairo_pattern_get_user_data(FPattern, key);
end;

procedure TCairoPattern.SetExtend(extend: TCairoExtend);
begin
  cairo_pattern_set_extend(FPattern, extend);
end;

procedure TCairoPattern.SetFilter(filter: TCairoFilter);
begin
  cairo_pattern_set_filter(FPattern, filter);
end;

procedure TCairoPattern.SetMatrix(matrix: TCairoMatrix);
begin
  cairo_pattern_set_matrix(FPattern, @matrix);
end;

procedure TCairoPattern.SetUserData(key, userData: Pointer;
  destroyFunc: TCairoDestroyFunc);
begin
  Assert(key <> nil, 'null key is reserved');
  CairoCheck(cairo_pattern_set_user_data(FPattern, key, userData, destroyFunc));
end;

class function TCairoPattern.init(pattern: PCairoPattern): TCairoPattern;
begin
  Result := TCairoPattern(cairo_pattern_get_user_data(pattern, nil));
  if Result = nil then
  begin
    cairo_pattern_reference(pattern);
    Result := TCairoPattern.CreateInternal(pattern);
  end;
end;

{ TCairoFontFace }

constructor TCairoFontFace.CreateInternal(fontface: PCairoFontFace);
begin
  FFontFace := fontface;
  cairo_font_face_set_user_data(FFontFace, nil, Self, nil);
end;

destructor TCairoFontFace.Destroy;
begin
  cairo_font_face_set_user_data(FFontFace, nil, nil, nil);
  cairo_font_face_destroy(FFontFace);
  inherited;
end;

function TCairoFontFace.GetStatus: TCairoStatus;
begin
  Result := cairo_font_face_status(FFontFace);
end;

function TCairoFontFace.GetType: TCairoFontType;
begin
  Result := cairo_font_face_get_type(FFontFace);
end;

function TCairoFontFace.GetUserData(key: Pointer): Pointer;
begin
  Result := cairo_font_face_get_user_data(FFontFace, key);
end;

class function TCairoFontFace.init(
  fontface: PCairoFontFace): TCairoFontFace;
begin
  Result := TCairoFontFace(cairo_font_face_get_user_data(fontface, nil));
  if Result = nil then
  begin
    cairo_font_face_reference(fontface);
    Result := TCairoFontFace.CreateInternal(fontface);
  end;
end;

procedure TCairoFontFace.SetUserData(key, userData: Pointer;
  destroyFunc: TCairoDestroyFunc);
begin
  Assert(key <> nil, 'null key is reserved');
  CairoCheck(cairo_font_face_set_user_data(FFontFace, key, userData, destroyFunc));
end;

{ TCairoScaledFont }

constructor TCairoScaledFont.Create(fontFace: ICairoFontFace; fontMatrix, ctm: TCairoMatrix; options: ICairoFontOptions);
begin
  CreateInternal(
    cairo_scaled_font_create(
      TCairoFontFace(fontFace.GetUserData(nil)).FFontFace,
      @fontMatrix, @ctm,
      TCairoFontOptions(options.GetUserData).FFontOptions
    ));
end;

constructor TCairoScaledFont.CreateInternal(scaledfont: PCairoScaledFont);
begin
  FScaledFont := scaledfont;
  cairo_scaled_font_set_user_data(FScaledFont, nil, Self, nil);
end;

destructor TCairoScaledFont.Destroy;
begin
  cairo_scaled_font_set_user_data(FScaledFont, nil, nil, nil);
  cairo_scaled_font_destroy(FScaledFont);
  inherited;
end;

procedure TCairoScaledFont.Extents(extent: TCairoFontExtents);
begin
  cairo_scaled_font_extents(FScaledFont, @extent);
end;

procedure TCairoScaledFont.GetCtm(var ctm: TCairoMatrix);
begin
  cairo_scaled_font_get_ctm(FScaledFont, @ctm);
end;

function TCairoScaledFont.GetFontFace: ICairoFontFace;
begin
  Result := TCairoFontFace.init(cairo_scaled_font_get_font_face(FScaledFont));
end;

procedure TCairoScaledFont.GetFontMatrix(var fontMatrix: TCairoMatrix);
begin
  cairo_scaled_font_get_font_matrix(FScaledFont, @fontMatrix);
end;

procedure TCairoScaledFont.GetFontOptions(options: ICairoFontOptions);
begin
  cairo_scaled_font_get_font_options(FScaledFont, TCairoFontOptions(options.GetUserData).FFontOptions);
end;

procedure TCairoScaledFont.GetScaleMatrix(var scaleMatrix: TCairoMatrix);
begin
  cairo_scaled_font_get_scale_matrix(FScaledFont, @scaleMatrix);
end;

function TCairoScaledFont.GetStatus: TCairoStatus;
begin
  Result := cairo_scaled_font_status(FScaledFont);
end;

function TCairoScaledFont.GetType: TCairoFontType;
begin
  Result := cairo_scaled_font_get_type(FScaledFont);
end;

function TCairoScaledFont.GetUserData(key: Pointer): Pointer;
begin
  Result := cairo_scaled_font_get_user_data(FScaledFont, key);
end;

procedure TCairoScaledFont.GlyphExtents(glyphs: PCairoGlyph; numGlyphs: Integer;
  extent: TCairoTextExtents);
begin
  cairo_scaled_font_glyph_extents(FScaledFont, glyphs, numGlyphs, @extent);
end;

class function TCairoScaledFont.init(
  scaledfont: PCairoScaledFont): TCairoScaledFont;
begin
  Result := TCairoScaledFont(cairo_scaled_font_get_user_data(scaledfont, nil));
  if Result = nil then
  begin
    cairo_scaled_font_reference(scaledfont);
    Result := TCairoScaledFont.CreateInternal(scaledfont);
  end;
end;

procedure TCairoScaledFont.SetUserData(key, userData: Pointer;
  destroyFunc: TCairoDestroyFunc);
begin
  Assert(key <> nil, 'null key is reserved');
  CairoCheck(cairo_scaled_font_set_user_data(FScaledFont, key, userData, destroyFunc));
end;

procedure TCairoScaledFont.TextExtents(utf8: UTF8String;
  extent: TCairoTextExtents);
begin
  cairo_scaled_font_text_extents(FScaledFont, PAnsiChar(utf8), @extent);
end;

procedure TCairoScaledFont.TextToGlyphs(x, y: Double; utf8: UTF8String;
  glyphs: PPCairoGlyph; numGlyphs: Integer;
  clusters: PPCairoTextCluster; numClusters: Integer;
  clusterFlags: PCairoTextClusterFlags);
begin
  CairoCheck(cairo_scaled_font_text_to_glyphs(FScaledFont, x, y, PAnsiChar(utf8),
    Length(utf8), glyphs, @numGlyphs, clusters, @numClusters, clusterFlags));
end;

{ TCairoToyFont }

constructor TCairoToyFontFace.Create(const family: AnsiString;
  slant: TCairoFontSlant; weight: TCairoFontWeight);
begin
  inherited CreateInternal(cairo_toy_font_face_create(PAnsiChar(family), slant, weight));
end;

function TCairoToyFontFace.GetFamily: AnsiString;
begin
  Result := cairo_toy_font_face_get_family(FFontFace);
end;

function TCairoToyFontFace.GetSlant: TCairoFontSlant;
begin
  Result := cairo_toy_font_face_get_slant(FFontFace);
end;

function TCairoToyFontFace.GetWeight: TCairoFontWeight;
begin
  Result := cairo_toy_font_face_get_weight(FFontFace);
end;


{ TCairoWin32FontFace }

{$ifdef CAIRO_HAS_WIN32_FONT}
constructor TCairoWin32FontFace.CreateForHfont(font: HFONT);
begin
  inherited CreateInternal(cairo_win32_font_face_create_for_hfont(font));
end;
{$endif}

{$ifdef CAIRO_HAS_WIN32_FONT}
constructor TCairoWin32FontFace.CreateForLogfontw(logfont: PLOGFONTW);
begin
  inherited CreateInternal(cairo_win32_font_face_create_for_logfontw(logfont));
end;
{$endif}

{$ifdef CAIRO_HAS_WIN32_FONT}
constructor TCairoWin32FontFace.CreateForLogfontwHfont(logfont: PLOGFONTW;
  font: HFONT);
begin
  inherited  CreateInternal(cairo_win32_font_face_create_for_logfontw_hfont(logfont, font));
end;
{$endif}

{ TCairoWin32ScaledFont }

{$ifdef CAIRO_HAS_WIN32_FONT}
procedure TCairoWin32ScaledFont.DoneFont;
begin
  cairo_win32_scaled_font_done_font(FScaledFont);
end;
{$endif}

{$ifdef CAIRO_HAS_WIN32_FONT}
procedure TCairoWin32ScaledFont.GetDeviceToLogical(
  var deviceToLogical: TCairoMatrix);
begin
  cairo_win32_scaled_font_get_device_to_logical(FScaledFont, @deviceToLogical);
end;
{$endif}

{$ifdef CAIRO_HAS_WIN32_FONT}
procedure TCairoWin32ScaledFont.GetLogicalToDevice(
  var logicalToDevice: TCairoMatrix);
begin
  cairo_win32_scaled_font_get_logical_to_device(FScaledFont, @logicalToDevice);
end;
{$endif}

{$ifdef CAIRO_HAS_WIN32_FONT}
function TCairoWin32ScaledFont.GetMetricsFactor: Double;
begin
  Result := cairo_win32_scaled_font_get_metrics_factor(FScaledFont);
end;
{$endif}

{$ifdef CAIRO_HAS_WIN32_FONT}
procedure TCairoWin32ScaledFont.SelectFont(hdc: HDC);
begin
  CairoCheck(cairo_win32_scaled_font_select_font(FScaledFont, hdc));
end;
{$endif}

{ TCairoUserFontFace }

constructor TCairoUserFontFace.Create;
begin
  inherited CreateInternal(cairo_user_font_face_create);
end;

{ TUserScaledFont }

function cairoUserScaledFontInit(scaledFont: PCairoScaledFont; cr: PCairo; extent: PCairoFontExtents): TCairoStatus; cdecl;
begin
  Result := TUserScaledFont(cairo_scaled_font_get_user_data(scaledFont, nil)).UserInit(cr, extent);
end;

function cairoUserScaledFontRenderGlyph(scaledFont: PCairoScaledFont; glyph: Cardinal; cr: PCairo; extent: PCairoTextExtents): TCairoStatus; cdecl;
begin
  Result := TUserScaledFont(cairo_scaled_font_get_user_data(scaledFont, nil)).UserRenderGlyph(glyph, cr, extent);
end;

function cairoUserScaledFontTextToGlyphs(scaledFont: PCairoScaledFont; utf8: PAnsiChar; utf8Len: Integer; glyphs: PPCairoGlyph; numGlyphs: PInteger; clusters: PPCairoTextCluster; numClusters: PInteger; clusterFlags: PCairoTextClusterFlags): TCairoStatus; cdecl;
begin
  Result := TUserScaledFont(cairo_scaled_font_get_user_data(scaledFont, nil)).UserTextToGlyphs(utf8, utf8Len, glyphs, numGlyphs, clusters, numClusters, clusterFlags);
end;

function cairoUserScaledFontUnicodeToGlyph(scaledFont: PCairoScaledFont; unicode: Cardinal; glyphIndex: PCardinal): TCairoStatus; cdecl;
begin
  Result := TUserScaledFont(cairo_scaled_font_get_user_data(scaledFont, nil)).UserUnicodeToGlyph(unicode, glyphIndex);
end;

{ TUserScaledFont }

constructor TUserScaledFont.Create(fontFace: ICairoFontFace; fontMatrix,
  ctm: TCairoMatrix; options: ICairoFontOptions);
begin
  inherited;
  with TCairoFontFace(fontFace.GetUserData(nil)) do
  begin
    cairo_user_font_face_set_init_func(FFontFace, @cairoUserScaledFontInit);
    cairo_user_font_face_set_render_glyph_func(FFontFace, @cairoUserScaledFontRenderGlyph);
    cairo_user_font_face_set_text_to_glyphs_func(FFontFace, @cairoUserScaledFontTextToGlyphs);
    cairo_user_font_face_set_unicode_to_glyph_func(FFontFace, @cairoUserScaledFontUnicodeToGlyph);
  end;
end;

{ TCairoDevice }

function TCairoDevice.Acquire: TCairoStatus;
begin
  Result := cairo_device_acquire(FDevice);
end;

constructor TCairoDevice.CreateInternal(device: PCairoDevice);
begin
  FDevice := device;
  cairo_device_set_user_data(device, nil, Self, nil);
end;

destructor TCairoDevice.Destroy;
begin
  cairo_device_set_user_data(FDevice, nil, nil, nil);
  cairo_device_destroy(FDevice);
  inherited;
end;

procedure TCairoDevice.Finish;
begin
  cairo_device_finish(FDevice);
end;

procedure TCairoDevice.Flush;
begin
  cairo_device_flush(FDevice);
end;

function TCairoDevice.GetStatus: TCairoStatus;
begin
  Result := cairo_device_status(FDevice);
end;

function TCairoDevice.GetType: TCairoDeviceType;
begin
  Result := cairo_device_get_type(FDevice);
end;

class function TCairoDevice.init(device: PCairoDevice): TCairoDevice;
begin
  if device <> nil then
  begin
    Result := TCairoDevice(cairo_device_get_user_data(device, nil));
    if Result = nil then
    begin
      cairo_device_reference(device);
      Result := TCairoDevice.CreateInternal(device);
    end;
  end else
    Result := nil;
end;

procedure TCairoDevice.Release;
begin
  cairo_device_release(FDevice);
end;

{ TCairoRegion }

function TCairoRegion.ContainsPoint(x, y: Integer): Boolean;
begin
  Result := cairo_region_contains_point(FRegion, x, y) <> 0;
end;

function TCairoRegion.ContainsRectangle(
  const rectangle: PCairoRectangleInt): TCairoRegionOverlap;
begin
  Result := cairo_region_contains_rectangle(FRegion, rectangle)
end;

function TCairoRegion.Copy: ICairoRegion;
begin
//  Result := CreateInternal(cairo_region_copy(FRegion))
end;

constructor TCairoRegion.Create;
begin
  CreateInternal(cairo_region_create);
end;

constructor TCairoRegion.CreateInternal(region: PCairoRegion);
begin
  FRegion := region;
end;

constructor TCairoRegion.CreateRectangle(const rectangle: PCairoRectangleInt);
begin
  CreateInternal(cairo_region_create_rectangle(rectangle));
end;

constructor TCairoRegion.CreateRectangles(const rects: PCairoRectangleInt;
  count: Integer);
begin
  CreateInternal(cairo_region_create_rectangles(rects, count));
end;

destructor TCairoRegion.Destroy;
begin
  cairo_region_destroy(FRegion);
  inherited;
end;

function TCairoRegion.Equal(r: ICairoRegion): Boolean;
begin
  Result := cairo_region_equal(FRegion, r.GetUserData) <> 0
end;

procedure TCairoRegion.GetExtents(extents: PCairoRectangleInt);
begin
  cairo_region_get_extents(FRegion, extents);
end;

procedure TCairoRegion.GetRectangle(nth: Integer;
  rectangle: PCairoRectangleInt);
begin
  cairo_region_get_rectangle(FRegion, nth, rectangle);
end;

function TCairoRegion.GetUserData: Pointer;
begin
  Result := FRegion;
end;

function TCairoRegion.Intersect(other: ICairoRegion): TCairoStatus;
begin
  Result := cairo_region_intersect(FRegion, other.GetUserData);
end;

function TCairoRegion.IntersectRectangle(
  const rectangle: PCairoRectangleInt): TCairoStatus;
begin
  Result := cairo_region_intersect_rectangle(FRegion, rectangle);
end;

function TCairoRegion.IsEmpty: Boolean;
begin
  Result := cairo_region_is_empty(FRegion) <> 0;
end;

function TCairoRegion.NumRectangles: Integer;
begin
  Result := cairo_region_num_rectangles(FRegion);
end;

function TCairoRegion.Status: TCairoStatus;
begin
  Result := cairo_region_status(FRegion);
end;

function TCairoRegion.Subtract(other: ICairoRegion): TCairoStatus;
begin
  Result := cairo_region_subtract(FRegion, other.GetUserData);
end;

function TCairoRegion.SubtractRectangle(
  const rectangle: PCairoRectangleInt): TCairoStatus;
begin
  Result := cairo_region_subtract_rectangle(FRegion, rectangle);
end;

procedure TCairoRegion.Translate(dx, dy: Integer);
begin
  cairo_region_translate(FRegion, dx, dy);
end;

function TCairoRegion.Union(other: ICairoRegion): TCairoStatus;
begin
  Result := cairo_region_union(FRegion, other.GetUserData);
end;

function TCairoRegion.UnionRectangle(
  const rectangle: PCairoRectangleInt): TCairoStatus;
begin
  Result := cairo_region_union_rectangle(FRegion, rectangle);
end;

function TCairoRegion.XorRectangle(
  const rectangle: PCairoRectangleInt): TCairoStatus;
begin
  Result := cairo_region_xor_rectangle(FRegion, rectangle);
end;

function TCairoRegion.XorRegion(other: ICairoRegion): TCairoStatus;
begin
  Result := cairo_region_xor(FRegion, other.GetUserData)
end;

end.
