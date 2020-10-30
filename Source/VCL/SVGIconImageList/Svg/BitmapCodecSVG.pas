// This unit requires at least Delphi 10.3.2 to work
unit BitmapCodecSVG;

interface

implementation

uses
  Winapi.Windows, Winapi.GDIPAPI, Winapi.GDIPOBJ,
  System.Types, System.SysUtils, System.Classes,
  FMX.Types, FMX.Graphics, FMX.Surfaces,
  SVGTypes, SVG;

type
  TBitmapCodecSVG = class(TCustomBitmapCodec)
  strict private
    function CopyToSurface(const ASVG: TSVG; const Bitmap: TBitmapSurface): Boolean;
    class procedure InitGDIPlus;
  public
    class constructor Create;
    class function GetImageSize(const AFileName: string): TPointF; override;
    class function IsValid(const AStream: TStream): Boolean; override;
    function LoadFromFile(const AFileName: string; const Bitmap: TBitmapSurface;
      const MaxSizeLimit: Cardinal): Boolean; override;
    function SaveToFile(const AFileName: string; const Bitmap: TBitmapSurface;
      const SaveParams: PBitmapCodecSaveParams = nil): Boolean; override;
    function LoadThumbnailFromFile(const AFileName: string; const AFitWidth, AFitHeight: Single;
      const UseEmbedded: Boolean; const Bitmap: TBitmapSurface): Boolean; override;
    function LoadFromStream(const AStream: TStream; const Bitmap: TBitmapSurface;
      const MaxSizeLimit: Cardinal): Boolean; override;
    function SaveToStream(const AStream: TStream; const Bitmap: TBitmapSurface; const Extension: string;
      const SaveParams: PBitmapCodecSaveParams = nil): Boolean; override;
  end;

const
  SSVGImageExtension = '.svg';                 // do not localize
  SVSVG = 'SVG files';

procedure DebugGDIPlus(level: DebugEventLevel; message: PChar); stdcall;
begin
  OutputDebugString(PChar('DebugGDIPlus ' + message));
end;


class procedure TBitmapCodecSVG.InitGDIPlus;
var
  Status: TStatus;
begin
  // Initialize StartupInput structure
  StartupInput.DebugEventCallback := DebugGDIPlus;
  StartupInput.SuppressBackgroundThread := False;
  StartupInput.SuppressExternalCodecs   := False;
  StartupInput.GdiplusVersion := 1;

  Status := GdiplusStartup(gdiplusToken, @StartupInput, nil);
  OutputDebugString(PChar('InitGDIPlus.Status=' + IntToStr(Ord(Status))));
end;

class constructor TBitmapCodecSVG.Create;
begin
end;

class function TBitmapCodecSVG.GetImageSize(const AFileName: string): TPointF;
var
  SVG: TSVG;
begin
  SVG := TSVG.Create;
  try
    try
      SVG.LoadFromFile(AFileName);
      Result := TPointF.Create(SVG.Width, SVG.Height);
    finally
      SVG.Free;
    end;
  except
    Result := TPointF.Create(0, 0);
  end;
end;

class function TBitmapCodecSVG.IsValid(const AStream: TStream): Boolean;
var
  SVG: TSVG;
begin
  OutputDebugString('IsValid');
  SVG := TSVG.Create;
  try
    try
      SVG.LoadFromStream(AStream);
      Result := (SVG.Width > 0) and (SVG.Height > 0);
      OutputDebugString(PChar(BoolToStr(Result, True)));
    finally
      SVG.Free;
    end;
  except
    Result := False;
  end;
end;

function TBitmapCodecSVG.LoadFromFile(const AFileName: string; const Bitmap: TBitmapSurface;
  const MaxSizeLimit: Cardinal): Boolean;
var
  Stream: TFileStream;
begin
  OutputDebugString('LoadFromFile');
  Stream := TFileStream.Create(AFileName, fmOpenRead);
  try
    Result := LoadFromStream(Stream, Bitmap, MaxSizeLimit);
  finally
    Stream.Free;
  end;
end;

function TBitmapCodecSVG.SaveToFile(const AFileName: string; const Bitmap: TBitmapSurface;
  const SaveParams: PBitmapCodecSaveParams = nil): Boolean;
begin
  Result := False;
end;

function TBitmapCodecSVG.LoadThumbnailFromFile(const AFileName: string;
  const AFitWidth, AFitHeight: Single; const UseEmbedded: Boolean;
  const Bitmap: TBitmapSurface): Boolean;
begin
  OutputDebugString('LoadThumbnailFromFile');
  Result := False;
end;

function TBitmapCodecSVG.CopyToSurface(const ASVG: TSVG; const Bitmap: TBitmapSurface): Boolean;
var
  GPGraphics: TGPGraphics;
  GPBitmap: TGPBitmap;
  GPRectF: TGPRectF;
  RectArray: TRectarray;
  GPRect: TGPRect;
  GPBitmapData: Winapi.GDIPAPI.TBitmapData;
  Source: PByte;
  Dest: PByte;
  Y: Integer;
  Status: TStatus;
  IntWidth: Integer;
  IntHeight: Integer;
begin
  Result := False;
  IntWidth := Trunc(ASVG.Width);
  IntHeight := Trunc(ASVG.Height);
  OutputDebugString(PChar('CopyToSurface ' + IntToStr(IntWidth) + ' ' + IntToStr(IntHeight)));
  GPBitmap := TGPBitmap.Create(IntWidth, IntHeight);
  Status := GPBitmap.GetLastStatus;
  if Status <> TStatus.Ok then
  begin
    OutputDebugString(PChar('GPBitmap ' + IntToStr(Ord(Status))));
    Exit;
  end;

  GPGraphics := TGPGraphics.Create(GPBitmap);
  try
    Status := GPGraphics.SetSmoothingMode(SmoothingModeAntiAlias);
    OutputDebugString(PChar('SetSmoothingMode ' + IntToStr(Ord(Status))));
    Status := GPGraphics.SetPixelOffsetMode(PixelOffsetModeHalf);
    OutputDebugString(PChar('PixelOffsetModeHalf ' + IntToStr(Ord(Status))));

    GPRectF.X := 0;
    GPRectF.Y := 0;
    GPRectF.Width := ASVG.Width;
    GPRectF.Height := ASVG.Height;

    RectArray := TRectArray.Create(TRect.Create(0, 0, Trunc(ASVG.Width), Trunc(ASVG.Height)));
    ASVG.PaintTo(GPGraphics, GPRectF, @RectArray, 1);

    GPRect.X := 0;
    GPRect.Y := 0;
    GPRect.Width := GPBitmap.GetWidth;
    GPRect.Height := GPBitmap.GetHeight;

    OutputDebugString(PChar('CopyToSurface.GPRectWidth '+ IntToStr(GPRect.Width)));
    Status := GPBitmap.LockBits(GPRect, ImageLockModeRead, PixelFormat32bppPARGB, GPBitmapData);
    if Status = TStatus.Ok then
    begin
      Bitmap.SetSize(Trunc(ASVG.Width), Trunc(ASVG.Height), TPixelFormat.BGRA);
      Source := GPBitmapData.Scan0;
      Dest := Bitmap.Bits;
      for Y := 0 to GPBitmapData.Height - 1 do
      begin
        Move(Source^, Dest^, GPBitmapData.Stride);
        Source := Source + GPBitmapData.Stride;
        Dest := Dest + Bitmap.Pitch;
      end;

      GPBitmap.UnlockBits(GPBitmapData);
      Result := True;
    end
    else
    begin
      OutputDebugString(PChar('CopyToSurface.Lockbits error ' + IntToStr(Ord(Status))));
    end;
  finally
    GPGraphics.Free;
    GPBitmap.Free;
  end;
end;

function TBitmapCodecSVG.LoadFromStream(const AStream: TStream; const Bitmap: TBitmapSurface;
  const MaxSizeLimit: Cardinal): Boolean;
var
  SVG: TSVG;
begin
  InitGDIPlus;
  OutputDebugString('LoadFromStream');
  try
    SVG := TSVG.Create;
    try
      SVG.LoadFromStream(AStream);
      Result := CopyToSurface(SVG, Bitmap);
    finally
      SVG.Free;
    end;
  except
    on E: Exception do
    begin
      OutputDebugString(PChar('LoadFromStream.E ' + E.Message));
      Result := False;
    end;
  end;
end;

function TBitmapCodecSVG.SaveToStream(const AStream: TStream; const Bitmap: TBitmapSurface;
  const Extension: string; const SaveParams: PBitmapCodecSaveParams = nil): Boolean;
begin
  Result := False;
end;

initialization
  TBitmapCodecManager.RegisterBitmapCodecClass(SSVGImageExtension, SVSVG, True,
    TBitmapCodecSVG);
finalization
  TBitmapCodecManager.UnregisterBitmapCodecClass(SSVGImageExtension);
end.
