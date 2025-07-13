{-----------------------------------------------------------------------------
 Unit Name: CairoSVGFactory
 Author:    Lübbe Onken
 Purpose:   High-level encapsulation of Cario Svg functionality using
            the cairo and rsvg libraries
-----------------------------------------------------------------------------}

unit CairoSVGFactory;

interface

uses
  SVGInterfaces;

// Factory Methods
function GetCairoSVGFactory: ISVGFactory;

resourcestring
  CAIRO_ERROR_PARSING_SVG_TEXT = 'Error parsing SVG Text: %s';

implementation

Uses
  cairo,
  cairolib,
  rsvg,
  Winapi.Windows,
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  SvgTypes,
  SvgCommon,
  Svg;

type
  TCairoSVG = class(TInterfacedObject, ISVG)
  private const
    cEmptySvg = '<svg xmlns="http://www.w3.org/2000/svg"></svg>';
  private
    FSource   : string;
    FSvgObject: IRSVGObject;
    FHeight   : Single;
    FWidth    : Single;
    // property access methods
    function GetWidth: Single;
    function GetHeight: Single;
    function GetOpacity: Single;
    function GetFixedColor: TColor;
    function GetGrayScale: Boolean;
    function GetSource: string;
    procedure SetOpacity(const AOpacity: Single);
    procedure SetGrayScale(const IsGrayScale: Boolean);
    procedure SetFixedColor(const AColor: TColor);
    procedure SetSource(const ASource: string);
    // procedures and functions
    function IsEmpty: Boolean;
    procedure Clear;
    procedure SaveToStream(AStream: TStream);
    procedure SaveToFile(const AFileName: string);
    procedure LoadFromSource;
    procedure LoadFromStream(AStream: TStream);
    procedure LoadFromFile(const AFileName: string);
    procedure PaintTo(DC: HDC; R: TRectF; KeepAspectRatio: Boolean = True);
  public
    constructor Create;
  end;

  TCairoSVGFactory = class(TInterfacedObject, ISVGFactory)
    function NewSvg: ISVG;
  end;

// Factory methods
function GetCairoSVGFactory: ISVGFactory;
begin
  Result := TCairoSVGFactory.Create;
end;

{ TCairoSVGFactory }

function TCairoSVGFactory.NewSvg: ISVG;
begin
  Result := TCairoSVG.Create;
end;

{ TCairoSVG }

procedure TCairoSVG.Clear;
begin
  SetSource(cEmptySvg);
end;

constructor TCairoSVG.Create;
begin
  FSvgObject := TRSVGObject.Create;
end;

function TCairoSVG.GetFixedColor: TColor;
begin

end;

function TCairoSVG.GetGrayScale: Boolean;
begin

end;

function TCairoSVG.GetHeight: Single;
begin
  Result := FHeight;
end;

function TCairoSVG.GetOpacity: Single;
begin

end;

function TCairoSVG.GetSource: string;
begin
  Result := FSource;
end;

function TCairoSVG.GetWidth: Single;
begin
  Result := FWidth;
end;

function TCairoSVG.IsEmpty: Boolean;
begin
  Result := (FSource = '') or (FSource = cEmptySvg) or (FSvgObject = Nil);
end;

procedure TCairoSVG.LoadFromFile(const AFileName: string);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    LoadFromStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TCairoSVG.LoadFromSource;
var
  LStream: TMemoryStream;
begin
  if FSource = '' then
    Clear;

  LStream := TMemoryStream.Create;
  try
    SaveToStream(LStream);
    LStream.Position := 0;
    LoadFromStream(LStream);
  finally
    LStream.Free;
  end;
end;

procedure TCairoSVG.LoadFromStream(AStream: TStream);
begin
  try
    FSvgObject := TRSVGObject.Create(AStream);
    FHeight := FSvgObject.Dimensions.height;
    FWidth := FSvgObject.Dimensions.width;
  except
    on E: Exception do
      raise Exception.CreateFmt(CAIRO_ERROR_PARSING_SVG_TEXT, [E.Message]);
  end;
end;

procedure TCairoSVG.PaintTo(DC: HDC; R: TRectF; KeepAspectRatio: Boolean);
var
  LSurface: IWin32Surface;
  LContext: ICairoContext;
  LSvgRect: TRectF;
  Ratio   : Single;
begin
  if not Assigned(FSvgObject) then
    Exit;

  LSurface := TWin32Surface.CreateHDC(DC);

  LContext := TCairoContext.Create(LSurface);
  LContext.Antialias := CAIRO_ANTIALIAS_SUBPIXEL;

  LSvgRect := R;
  if (FWidth > 0) and (FHeight > 0) then
    begin
      if KeepAspectRatio then
        begin
          LSvgRect := TRectF.Create(0, 0, FWidth, FHeight);
          LSvgRect := LSvgRect.FitInto(R, Ratio);
          LContext.Scale(1 / Ratio, 1 / Ratio);
        end
      else
        LContext.Scale(R.Width / FWidth, R.Height / fHeight);
    end;

  LContext.RenderSVG(FSvgObject);
end;

procedure TCairoSVG.SaveToFile(const AFileName: string);
var
  LFileStream: TFileStream;
begin
  LFileStream := TFileStream.Create(AFileName, fmCreate or fmOpenWrite);
  try
    SaveToStream(LFileStream);
  finally
    LFileStream.Free;
  end;
end;

procedure TCairoSVG.SaveToStream(AStream: TStream);
var
  LBuffer: TBytes;
begin
  LBuffer := TEncoding.UTF8.GetBytes(FSource);
  AStream.WriteBuffer(LBuffer, Length(LBuffer))
end;

procedure TCairoSVG.SetFixedColor(const AColor: TColor);
begin

end;

procedure TCairoSVG.SetGrayScale(const IsGrayScale: Boolean);
begin

end;

procedure TCairoSVG.SetOpacity(const AOpacity: Single);
begin

end;

procedure TCairoSVG.SetSource(const ASource: string);
begin
  if FSource <> ASource then
    begin
      FSource := ASource;
      LoadFromSource;
    end;
end;

end.
