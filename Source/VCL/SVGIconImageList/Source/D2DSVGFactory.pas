{-----------------------------------------------------------------------------
 Unit Name: D2DSVGHandler
 Author:    PyScripter
 Purpose:   High-level encapsuation of Direct2D Svg functionality
 History:
-----------------------------------------------------------------------------}
unit D2DSVGFactory;

interface
Uses
  Winapi.D2D1,
  SVGInterfaces;

// Factory Methods
function GetD2DSVGFactory: ISVGFactory;
function RenderTarget: ID2D1DCRenderTarget;

// Support functions
function WinSvgSupported: Boolean;

implementation

Uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.ActiveX,
  Winapi.DxgiFormat,
  Winapi.D2DMissing,
  System.Types,
  System.UITypes,
  System.UIConsts,
  System.SysUtils,
  System.Classes,
  System.RegularExpressions;

resourcestring
  D2D_ERROR_NOT_AVAILABLE    = 'Windows SVG support is not available';
  D2D_ERROR_PARSING_SVG_TEXT = 'Error parsing SVG Text: %s';
  D2D_ERROR_UNSUPPORTED_SVG  = '<style> or <text> elements and class="" attributes are not supported by Windows SVG';

type

  TD2DSVG = class(TInterfacedObject, ISVG)
  private
    fSource: String;
    fWidth: Single;
    fHeight: Single;
    fFixedColor: TColor;
    fGrayScale: Boolean;
    fSvgDoc: ID2D1SvgDocument;
    // property access methods
    function GetWidth: Single;
    function GetHeight: Single;
    function GetOpacity: Single;
    procedure SetOpacity(const Opacity: Single);
    function GetGrayScale: Boolean;
    procedure SetGrayScale(const IsGrayScale: Boolean);
    function GetFixedColor: TColor;
    procedure SetFixedColor(const Color: TColor);
    function GetSource: string;
    procedure SetSource(const ASource: string);
    // procedures and functions
    function IsEmpty: Boolean;
    procedure Clear;
    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(const FileName: string);
    procedure PaintTo(DC: HDC; R: TRectF; KeepAspectRatio: Boolean = True);
    procedure LoadFromSource;
    procedure SourceFromStream(Stream: TStream);
    procedure SvgFromStream(Stream: TStream);
    {$IFDEF CheckForUnsupportedSvg}
    procedure CheckForUnsupportedSvg;
    {$ENDIF}
  public
    constructor Create;
  end;

  TD2DSVGHandler = class(TInterfacedObject, ISVGFactory)
    function NewSvg: ISVG;
    // class stuff
    class var SingletonD2DFactory: ID2D1Factory;
    class var SingletonRenderTarget: ID2D1DCRenderTarget;
    class function D2DFactory(factoryType: TD2D1FactoryType=D2D1_FACTORY_TYPE_SINGLE_THREADED;
      factoryOptions: PD2D1FactoryOptions=nil): ID2D1Factory; static;
    class function RT: ID2D1DCRenderTarget; static;
  end;

{$INCLUDE SVGIconImageList.inc}

{ TD2DSVG }

{$IFDEF CheckForUnsupportedSvg}
procedure TD2DSVG.CheckForUnsupportedSvg;
const
  cRegEx = '(\<(style|text)|class=\")';
begin
  if TRegEx.IsMatch(FSource, cRegEx, [roIgnoreCase]) then
    raise Exception.CreateRes(@D2D_ERROR_UNSUPPORTED_SVG);
end;
{$ENDIF}

procedure TD2DSVG.Clear;
Const
  EmptySvg = '<svg xmlns="http://www.w3.org/2000/svg"></svg>';
begin
  SetSource(EmptySvg);
end;

constructor TD2DSVG.Create;
begin
  inherited;
  fFixedColor:= TColors.SysDefault; // clDefault
end;

procedure TD2DSVG.SvgFromStream(Stream: TStream);
var
  XStream: IStream;
  DeviceContext5: ID2D1DeviceContext5;
  Root: ID2D1SvgElement;
  ViewBox: D2D1_SVG_VIEWBOX;
begin
  fsvgDoc := nil;
  XStream := TStreamAdapter.Create(Stream, soReference);
  if Supports(RenderTarget, ID2D1DeviceContext5, DeviceContext5) then
  begin
    {
       The Svg Viewport is specified by the width and height
       properties of the Svg.
       CreateSvgDocument needs initial values which are
       not used if width and height are specified at the root.
    }
    if not Succeeded(DeviceContext5.CreateSvgDocument(XStream, D2D1SizeF(100, 100), // some initial values
    fSvgDoc)) then
      RaiseLastOSError;
    fsvgDoc.GetRoot(Root);
    if Root.IsAttributeSpecified('width', nil) then
      Root.GetAttributeValue('width', D2D1_SVG_ATTRIBUTE_POD_TYPE_FLOAT, @fWidth, SizeOf(fWidth));
    if Root.IsAttributeSpecified('height', nil) then
      Root.GetAttributeValue('height', D2D1_SVG_ATTRIBUTE_POD_TYPE_FLOAT, @fHeight, SizeOf(fHeight));
    // If width or height are missing try to get them from the Viewbox
    if ((fWidth = 0) or (fHeight = 0)) and (Root.GetAttributeValue('viewBox', D2D1_SVG_ATTRIBUTE_POD_TYPE_VIEWBOX, @ViewBox, SizeOf(ViewBox)) = S_OK) then
    begin
      if fWidth = 0 then
        fWidth := ViewBox.width;
      if fHeight = 0 then
        fHeight := ViewBox.height;
    end;
    // update the initial Viewport
    if (fWidth > 0) and (fHeight > 0) then
      fSvgDoc.SetViewportSize(D2D1SizeF(fWidth, fHeight));
  end
  else
    raise Exception.CreateRes(@D2D_ERROR_NOT_AVAILABLE);
end;

procedure TD2DSVG.LoadFromFile(const FileName: string);
Var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TD2DSVG.LoadFromSource;
var
  MStream: TMemoryStream;
begin
  fSvgDoc := nil;
  if fSource = '' then Exit;

{$IFDEF CheckForUnsupportedSvg}
  CheckForUnsupportedSvg;
{$ENDIF}

  try
    MStream := TMemoryStream.Create;
    try
      SaveToStream(MStream);
      MStream.Position := 0;
      SvgFromStream(MStream);
    finally
      MStream.Free;
    end;
  except
    on E: Exception do
      raise Exception.CreateFmt(D2D_ERROR_PARSING_SVG_TEXT, [E.Message]);
  end;
end;

function TD2DSVG.GetFixedColor: TColor;
begin
  Result := fFixedColor;
end;

function TD2DSVG.GetGrayScale: Boolean;
begin
  Result := fGrayScale;
end;

function TD2DSVG.GetHeight: Single;
begin
  Result := fHeight;
end;

function TD2DSVG.GetOpacity: Single;
Var
  Root: ID2D1SvgElement;
begin
  Result := 1;
  if Assigned(fSvgDoc) then begin
    fSvgDoc.GetRoot(Root);
    if Assigned(Root) then
      Root.GetAttributeValue('opacity', D2D1_SVG_ATTRIBUTE_POD_TYPE_FLOAT,
        @Result, SizeOf(Result));
  end;
end;

function TD2DSVG.GetSource: string;
begin
  Result := FSource;
end;

function TD2DSVG.GetWidth: Single;
begin
  Result := fWidth;
end;

function TD2DSVG.IsEmpty: Boolean;
Var
  Root: ID2D1SvgElement;
begin
  if fSvgDoc = nil then Exit(True);

  fSvgDoc.GetRoot(Root);
  Result := not Root.HasChildren;
end;

procedure TD2DSVG.LoadFromStream(Stream: TStream);
Var
  OldPos : Int64;
begin
  // read and save the Source
  OldPos := Stream.Position;
  SourceFromStream(Stream);
  // Restore Position
  Stream.Position := OldPos;
  // Now create the SVG
  SvgFromStream(Stream);
end;

procedure TD2DSVG.PaintTo(DC: HDC; R: TRectF; KeepAspectRatio: Boolean);
var
  Matrix : TD2DMatrix3X2F;
  SvgRect : TRectF;
  RT: ID2D1DCRenderTarget;
  Ratio: Single;
begin
  if not Assigned(fSvgDoc) then Exit;

  SvgRect:= R;
  if (fWidth > 0) and (fHeight > 0) then begin
    if KeepAspectRatio then
    begin
      SvgRect := TRectF.Create(0, 0, fWidth, fHeight);
      SvgRect := SvgRect.FitInto(R, Ratio);
      Matrix := TD2DMatrix3X2F.Scale(1/Ratio, 1/Ratio, Point(0, 0));
    end
    else
       Matrix := TD2DMatrix3X2F.Scale(R.Width/fWidth,
        R.Height/fHeight, Point(0, 0));
  end;
  RT := RenderTarget;
  RT.BindDC(DC, SvgRect.Round);
  RT.BeginDraw;
  try
    if (fWidth > 0) and (fHeight > 0) then
      RT.SetTransform(Matrix);
    (RT as ID2D1DeviceContext5).DrawSvgDocument(fSvgDoc);
  finally
    RT.EndDraw;
  end;
end;

procedure TD2DSVG.SaveToFile(const FileName: string);
Var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmCreate or fmOpenWrite);
  try
    SaveToStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TD2DSVG.SaveToStream(Stream: TStream);
var
  Buffer: TBytes;
begin
  Buffer := TEncoding.UTF8.GetBytes(FSource);
  Stream.WriteBuffer(Buffer, Length(Buffer))
end;

type
  TSvgElementProc = reference to procedure(const Element: ID2D1SvgElement);

procedure TransformSvgElement(const Element: ID2D1SvgElement; Proc: TSvgElementProc);
Var
  Child, NextChild: ID2D1SvgElement;
begin
  Proc(Element);

  Element.GetFirstChild(Child);
  while Assigned(Child) do
  begin
    // Recursively recolor the subtree starting with this child node.
    TransformSvgElement(Child, Proc);
    Element.GetNextChild(Child, NextChild);
    Child := NextChild;
  end;
end;

procedure RecolorSubtree(const Element: ID2D1SvgElement; NewColor: TD2D1ColorF);
begin
  TransformSvgElement(Element,
    procedure(const Element: ID2D1SvgElement)

      procedure RecolorAttribute(Attr: PWideChar);
      Var
        IsInherited: Bool;
      begin
        if Element.IsAttributeSpecified(Attr, @IsInherited) and not IsInherited then
          Element.SetAttributeValue(Attr, D2D1_SVG_ATTRIBUTE_POD_TYPE_COLOR,
              @NewColor, SizeOf(NewColor));
      end;

    begin
      RecolorAttribute('fill');
      RecolorAttribute('stroke');
      RecolorAttribute('stop-color');
    end);
end;

procedure TD2DSVG.SetFixedColor(const Color: TColor);
Var
  Root: ID2D1SvgElement;
  NewColor: TD2D1ColorF;
begin
  if Color = fFixedColor then Exit;

  if (fGrayScale and (Color <> TColors.SysDefault)) or
    ((fFixedColor <> TColors.SysDefault) and (Color = TColors.SysDefault))
  then
    LoadFromSource;

  if Color < 0  then
    fFixedColor := GetSysColor(Color and $000000FF)
  else
    fFixedColor := Color;

  fGrayScale := False;
  if (FFixedColor <> TColors.SysDefault) and Assigned(fSvgDoc) then
  begin
    fSvgDoc.GetRoot(Root);

    with TColors(fFixedColor) do
      NewColor :=  D2D1ColorF(r/255, g/255, b/255, 1);

    Root.SetAttributeValue('fill', D2D1_SVG_ATTRIBUTE_POD_TYPE_COLOR,
              @NewColor, SizeOf(NewColor));
    RecolorSubtree(Root, NewColor);
  end;
end;

// Converts any color to grayscale
function GrayScaleColor(Color : TD2D1ColorF) : TD2D1ColorF;
var
  LGray : Single;
begin
  // get the luminance according to https://www.w3.org/TR/AERT/#color-contrast
  LGray  := 0.299 * Color.R + 0.587 * Color.G + 0.114 * Color.B;
  // set the result to the new grayscale color including the alpha info
  Result := D2D1ColorF(LGray, LGray, LGray, Color.A);
end;

procedure GrayScaleSubtree(const Element: ID2D1SvgElement);
begin
  TransformSvgElement(Element,
    procedure(const Element: ID2D1SvgElement)
    Var
      OldColor: TD2D1ColorF;
      NewColor: TD2D1ColorF;

      procedure GrayScaleAttribute(Attr: PWideChar);
      Var
        IsInherited: Bool;
      begin
        if Element.IsAttributeSpecified(Attr, @IsInherited)  and not IsInherited then
        begin
          if Succeeded(Element.GetAttributeValue(Attr, D2D1_SVG_ATTRIBUTE_POD_TYPE_COLOR,
            @OldColor, SizeOf(OldColor)))
          then
          begin
            NewColor := GrayScaleColor(OldColor);
            Assert(Succeeded(Element.SetAttributeValue(Attr, D2D1_SVG_ATTRIBUTE_POD_TYPE_COLOR,
              @NewColor, SizeOf(NewColor))));
          end;
        end;
      end;

    begin
      GrayScaleAttribute('fill');
      GrayScaleAttribute('stroke');
      GrayScaleAttribute('stop-color');
    end);
end;

procedure TD2DSVG.SetGrayScale(const IsGrayScale: Boolean);
Var
  Root: ID2D1SvgElement;
begin
  if IsGrayScale = fGrayScale then Exit;
  if fGrayScale or (fFixedColor <> TColors.SysDefault) then
    LoadFromSource;

  fGrayScale := IsGrayScale;
  fFixedColor := TColors.SysDefault;

  if fGrayScale then
  begin
    fSvgDoc.GetRoot(Root);
    GrayScaleSubtree(Root);
  end;
end;

procedure TD2DSVG.SetOpacity(const Opacity: Single);
Var
  Root: ID2D1SvgElement;
begin
  if Assigned(fSvgDoc) then begin
    fSvgDoc.GetRoot(Root);
    if Assigned(Root) then
      Root.SetAttributeValue('opacity', D2D1_SVG_ATTRIBUTE_POD_TYPE_FLOAT,
        @Opacity, SizeOf(Opacity));
  end;
end;

procedure TD2DSVG.SetSource(const ASource: string);
begin
  if FSource <> ASource then
  begin
    FSource := ASource;
    LoadFromSource;
  end;
end;

procedure TD2DSVG.SourceFromStream(Stream: TStream);
var
  Size: Integer;
  Buffer: TBytes;
begin
  Size := Stream.Size - Stream.Position;
  SetLength(Buffer, Size);
  Stream.Read(Buffer, 0, Size);
  FSource := TEncoding.UTF8.GetString(Buffer);
end;

{ TD2DHandler }

function TD2DSVGHandler.NewSvg: ISVG;
begin
  Result := TD2DSVG.Create;
end;

class function TD2DSVGHandler.D2DFactory(factoryType: TD2D1FactoryType;
  factoryOptions: PD2D1FactoryOptions): ID2D1Factory;
var
  LD2DFactory: ID2D1Factory;
begin
  if SingletonD2DFactory = nil then
  begin
    if not Succeeded(D2D1CreateFactory(factoryType, IID_ID2D1Factory,
      factoryOptions, LD2DFactory))
    then
      RaiseLastOSError;

    if InterlockedCompareExchangePointer(Pointer(SingletonD2DFactory), Pointer(LD2DFactory), nil) = nil then
      LD2DFactory._AddRef;
  end;
  Result := SingletonD2DFactory;
end;

class function TD2DSVGHandler.RT: ID2D1DCRenderTarget;
Var
  RenderTarget: ID2D1DCRenderTarget;
begin
  if SingletonRenderTarget = nil then
  begin
    if not Succeeded(D2DFactory.CreateDCRenderTarget(
      D2D1RenderTargetProperties(
        {$IFDEF GPUSupprt}
        D2D1_RENDER_TARGET_TYPE_DEFAULT,
        {$ELSE}
        D2D1_RENDER_TARGET_TYPE_SOFTWARE, // much faster in my desktop with a slow GPU
        {$ENDIF}
        D2D1PixelFormat(DXGI_FORMAT_B8G8R8A8_UNORM, D2D1_ALPHA_MODE_PREMULTIPLIED),
        0, 0, D2D1_RENDER_TARGET_USAGE_GDI_COMPATIBLE),
        RenderTarget))
    then
      RaiseLastOSError;

    if InterlockedCompareExchangePointer(Pointer(SingletonRenderTarget),
      Pointer(RenderTarget), nil) = nil
    then
      SingletonRenderTarget._AddRef;
  end;
  Result := SingletonRenderTarget;
end;

// Factory methods
function GetD2DSVGFactory: ISVGFactory;
begin
  Result := TD2DSVGHandler.Create;
end;

function RenderTarget: ID2D1DCRenderTarget;
begin
  Result := TD2DSVGHandler.RT;
end;

// Support functions
function WinSvgSupported: Boolean;
begin
  Result := TOSVersion.Check(10, 0) and (TOSVersion.Build >= 15063);
end;

end.
