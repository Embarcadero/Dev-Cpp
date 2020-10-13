{******************************************************************************}
{                                                                              }
{       SVGIconImageList: An extended ImageList for Delphi/VCL                 }
{       to simplify use of SVG Icons (resize, opacity and more...)             }
{                                                                              }
{       Copyright (c) 2019-2020 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{       Contributors: Vincent Parrett, Kiriakos Vlahos                         }
{                                                                              }
{       https://github.com/EtheaDev/SVGIconImageList                           }
{                                                                              }
{******************************************************************************}
{       Original version (c) 2005, 2008 Martin Walter with license:            }
{       Use of this file is permitted for commercial and non-commercial        }
{       use, as long as the author is credited.                                }
{       home page: http://www.mwcs.de                                          }
{       email    : martin.walter@mwcs.de                                       }
{******************************************************************************}
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License");             }
{  you may not use this file except in compliance with the License.            }
{  You may obtain a copy of the License at                                     }
{                                                                              }
{      http://www.apache.org/licenses/LICENSE-2.0                              }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS,           }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{******************************************************************************}
unit SVGIconImageList;

interface

{$INCLUDE SVGIconImageList.inc}

uses
  Classes
  , SysUtils
  , ImgList
  , Windows
  , Graphics
{$IFDEF HiDPISupport}
  , Messaging
{$ENDIF}
  , Forms
  , SVGInterfaces
  , SVGIconItems
  , SVGIconImageListBase;

type
  TSVGIconItem = SVGIconItems.TSVGIconItem;
  TSVGIconItems = SVGIconItems.TSVGIconItems;


  {TSVGIconImageList}
  TSVGIconImageList = class(TSVGIconImageListBase)
  private
    FSVGItems: TSVGIconItems;
  protected
    function GetSVGIconItems: TSVGIconItems; override;
    //procedure SetSVGIconItems(const Value: TSVGIconItems); override;
    procedure ReadImageData(Stream: TStream);
    procedure WriteImageData(Stream: TStream);
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoAssign(const Source: TPersistent); override;
  public
    procedure RecreateBitmaps; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Add(const ASVG: ISVG; const AIconName: string;
       const AGrayScale: Boolean = False;
       const AFixedColor: TColor = SVG_INHERIT_COLOR): Integer;
    procedure Delete(const Index: Integer);
    procedure Remove(const Name: string);
    procedure ClearIcons; override;
    procedure SaveToFile(const AFileName: string);
    procedure PaintTo(const ACanvas: TCanvas; const AIndex: Integer;
      const X, Y, AWidth, AHeight: Single; AEnabled: Boolean = True); override;
  published
    //Publishing properties of Custom Class
    property Width;
    property Height;
    property Size;
    property OnChange;
    //New properties
    property SVGIconItems;
    property Opacity: Byte read FOpacity write SetOpacity default 255;
    property FixedColor: TColor read FFixedColor write SetFixedColor default SVG_INHERIT_COLOR;
    property GrayScale: Boolean read FGrayScale write SetGrayScale default False;
    property DisabledGrayScale: Boolean read FDisabledGrayScale write SetDisabledGrayScale default True;
    property DisabledOpacity: Byte read FDisabledOpacity write SetDisabledOpacity default 125;
    /// <summary>
    /// Enable and disable scaling with form
    /// </summary>
    {$IFDEF HiDPISupport}
    property Scaled;
    {$ENDIF}
  end;


implementation

uses
  System.Types
  , CommCtrl
  , Math
  , ComCtrls
  , SVGIconVirtualImageList;


{ TSVGIconImageList }

function TSVGIconImageList.Add(const ASVG: ISVG;
  const AIconName: string; const AGrayScale: Boolean = False;
  const AFixedColor: TColor = SVG_INHERIT_COLOR): Integer;
var
  Item: TSVGIconItem;
begin
  FSVGItems.BeginUpdate;
  try
    Item := FSVGItems.Add;
    Item.SVG := ASVG;
    Item.IconName := AIconName;
    Item.FixedColor := AFixedColor;
    Item.GrayScale := AGrayScale;
  finally
    FSVGItems.EndUpdate;
  end;
  Result := FSVGItems.Count - 1;
end;


procedure TSVGIconImageList.ClearIcons;
begin
  BeginUpdate;
  try
    FSVGItems.Clear;
    inherited Clear;
  finally
    EndUpdate;
  end;
end;

constructor TSVGIconImageList.Create(AOwner: TComponent);
begin
  inherited;
  FSVGItems := TSVGIconItems.Create(Self);
end;

procedure TSVGIconImageList.Delete(const Index: Integer);
begin
  //Don't call inherited method of ImageList, to avoid errors
  if (Index >= 0) and (Index < FSVGItems.Count) then
    FSVGItems.Delete(Index);
end;

destructor TSVGIconImageList.Destroy;
begin
  FreeAndNil(FSVGItems);
  inherited;
end;


procedure TSVGIconImageList.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('Images', ReadImageData, WriteImageData, False);
end;


procedure TSVGIconImageList.DoAssign(const Source: TPersistent);
var
  LVirtualList : TSVGIconVirtualImageList;
begin
  inherited;
  if Source is TSVGIconImageList then
  begin
    FSVGItems.Assign(TSVGIconImageList(Source).FSVGItems);
  end
  else if Source is TSVGIconVirtualImageList then
  begin
    LVirtualList := TSVGIconVirtualImageList(Source);
    if LVirtualList.ImageCollection <> nil then
    begin
      FSVGItems.Assign(LVirtualList.ImageCollection.SVGIconItems);
    end;
  end;
end;

procedure TSVGIconImageList.PaintTo(const ACanvas: TCanvas; const AIndex: Integer;
  const X, Y, AWidth, AHeight: Single; AEnabled: Boolean = True);
var
  LSVG: ISVG;
  LItem: TSVGIconItem;
  LOpacity: Byte;
begin
  if (AIndex >= 0) and (AIndex < FSVGItems.Count) then
  begin
    LItem := FSVGItems[AIndex];
    LSVG := LItem.SVG;
    if LItem.FixedColor <> SVG_INHERIT_COLOR then
      LSVG.FixedColor := LItem.FixedColor
    else
      LSVG.FixedColor := FFixedColor;
    LOpacity := FOpacity;
    if AEnabled then
    begin
      if LItem.GrayScale or FGrayScale then
        LSVG.Grayscale := True
      else
        LSVG.Grayscale := False;
    end
    else
    begin
      if FDisabledGrayScale then
        LSVG.Grayscale := True
      else
        LOpacity := FDisabledOpacity;
    end;
    LSVG.Opacity := LOpacity / 255;
    LSVG.PaintTo(ACanvas.Handle, TRectF.Create(TPointF.Create(X, Y), AWidth, AHeight));
    LSVG.Opacity := 1;
  end;
end;

procedure TSVGIconImageList.ReadImageData(Stream: TStream);
var
  LStream: TMemoryStream;
  LCount, LSize: Integer;
  LSVG: ISVG;
  C: Integer;
  LPos: Int64;
  LIconName: string;
  LTag: TBytes;
  LFixedColorStr: AnsiString;
  LGrayScale: Boolean;
  LFixedColor: TColor;
begin
  //Only for backward compatibility: load images stored in old format
  LStream := nil;
  LSVG := nil;
  BeginUpdate;
  try
    LStream := TMemoryStream.Create;
    //Read Count of Images
    if Stream.Read(LCount, SizeOf(Integer)) > 0 then
    begin
      for C := 0 to LCount - 1 do
      begin
        LSVG := GlobalSVGFactory.NewSvg;
        try
          //Read IconName
          Stream.Read(LSize, SizeOf(Integer));
          SetLength(LIconName, LSize);
          Stream.Read(PChar(LIconName)^, LSize * SizeOf(Char));
          //Read SVG Stream Size
          Stream.Read(LSize, SizeOf(Integer));
          LStream.CopyFrom(Stream, LSize);
          //Read SVG Stream data
          try
            LSVG.LoadFromStream(LStream);
          except
            on E: Exception do
              raise Exception.CreateFmt('Detected "old" binary image stream of %s! '+
                'You must disable Direct2D (removing $DEFINE PreferNativeSvgSupport from SVGIconImageList.inc), '+
                'recompile SVGIconImageList packages, and try again',
                [Owner.Name+'.'+Self.Name]);
          end;
          //Check for FixedColor attribute
          LPos := Stream.Position;
          LFixedColor := SVG_INHERIT_COLOR;
          SetLength(LTag, 10);
          Stream.Read(Pointer(LTag)^, 10);
          SetString(LFixedColorStr, PAnsiChar(@LTag[0]), 10);
          if LFixedColorStr = 'FixedColor' then
            //Read Fixed Color value
            Stream.Read(LFixedColor, SizeOf(Integer))
          else
            Stream.Position := LPos;

          //Check for GrayScale attribute
          LPos := Stream.Position;
          LGrayScale := False;
          SetLength(LTag, 9);
          Stream.Read(Pointer(LTag)^, 9);
          SetString(LFixedColorStr, PAnsiChar(@LTag[0]), 9);
          if LFixedColorStr = 'GrayScale' then
            LGrayScale := True
          else
            Stream.Position := LPos;

          Add(LSVG, LIconName, LGrayScale, LFixedColor);
          LStream.Clear;
        finally
          LSVG := nil;
        end;
      end;
    end;
  finally
    LStream.Free;
    EndUpdate;
  end;
end;

procedure TSVGIconImageList.RecreateBitmaps;
var
  C: Integer;
  LBitmap: TBitmap;
  LItem: TSVGIconItem;
begin
  if not Assigned(FSVGItems) or
    ([csLoading, csDestroying, csUpdating] * ComponentState <> [])
  then
    Exit;

  ImageList_Remove(Handle, -1);
  if (Width > 0) and (Height > 0) then
  begin
    HandleNeeded;
    for C := 0 to FSVGItems.Count - 1 do
    begin
      LItem := FSVGItems[C];
      LBitmap := LItem.GetBitmap(Width, Height, FFixedColor, FOpacity, FGrayScale);
      try
        ImageList_Add(Handle, LBitmap.Handle, 0);
      finally
        LBitmap.Free;
      end;
    end;
  end;
end;

procedure TSVGIconImageList.Remove(const Name: string);
begin
  Delete(IndexOf(Name));
end;

procedure TSVGIconImageList.SaveToFile(const AFileName: string);
var
  LImageStrip: TBitmap;
  LImageCount: Integer;
  LStripWidth, LStripHeight: Integer;

  procedure CreateLImageStrip(var AStrip: TBitmap);
  var
    I, J, K: Integer;
  begin
    with AStrip do
    begin
      Canvas.Brush.Color := clNone;
      Canvas.FillRect(Rect(0, 0, AStrip.Width, AStrip.Height));
      J := 0;
      K := 0;
      for I := 0 to Self.Count - 1 do
      begin
        Draw(Canvas, J * Width, K * Height, I, dsTransparent, itImage);
        Inc(J);
        if J >= LStripWidth then
        begin
          J := 0;
          Inc(K);
        end;
      end;
    end;
  end;

  procedure CalcDimensions(ACount: Integer; var AWidth, AHeight: Integer);
  var
    X: Single;
  begin
    X := Sqrt(ACount);
    AWidth := Trunc(X);
    if Frac(X) > 0 then
      Inc(AWidth);
    X := ACount / AWidth;
    AHeight := Trunc(X);
    if Frac(X) > 0 then
      Inc(AHeight);
  end;

begin
  LImageStrip := TBitmap.Create;
  try
    LImageCount := Count;
    CalcDimensions(LImageCount, LStripWidth, LStripHeight);
    LImageStrip.Width := LStripWidth * Width;
    LImageStrip.Height := LStripHeight * Height;
    CreateLImageStrip(LImageStrip);
    LImageStrip.SaveToFile(AFileName);
  finally
    LImageStrip.Free;
  end;
end;

function TSVGIconImageList.GetSVGIconItems: TSVGIconItems;
begin
  Result := fSVGItems;
end;

procedure TSVGIconImageList.WriteImageData(Stream: TStream);
begin
  Exit;
end;


end.
