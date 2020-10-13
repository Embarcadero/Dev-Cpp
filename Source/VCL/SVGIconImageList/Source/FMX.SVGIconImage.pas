{******************************************************************************}
{                                                                              }
{       SVG Image in TPicture: useful to show a Scalable Vector Graphic        }
{                                                                              }
{       Copyright (c) 2019-2020 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{       Contributors:                                                          }
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
unit FMX.SVGIconImage;

interface

{$INCLUDE SVGIconImageList.inc}

uses
  System.Classes
  , System.UITypes
  , System.Rtti
  , System.Messaging
  , System.ImageList
  , System.Types
  , FMX.Controls
  , FMX.ImgList
  , FMX.MultiResBitmap
  , FMX.Types
  , FMX.Graphics
  , FMX.Objects
  , SVG
  ;

type
  TSVGIconFixedMultiResBitmap = class;

  TSVGIconFixedBitmapItem = class(TFixedBitmapItem)
  private
    FSize: Integer;
    FOpacity: Single;
    FOwnerCollection: TSVGIconFixedMultiResBitmap;
    FIconName: string;
    FSVG: TSVG;
    function StoreOpacity: Boolean;
    procedure SetBitmap(const AValue: TBitmapOfItem);
    function GetBitmap: TBitmapOfItem;
    procedure SetSize(const AValue: Integer);
    procedure SetOpacity(const AValue: Single);
    procedure SetIconName(const AValue: string);
    function GetSVGText: string;
    procedure SetSVGText(const Value: string);
  protected
    function BitmapStored: Boolean; override;
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure DrawSVGIcon;
    property SVG: TSVG read FSVG;
  published
    property Bitmap: TBitmapOfItem read GetBitmap write SetBitmap stored False;
    property Opacity: Single read FOpacity write SetOpacity stored StoreOpacity;
    property Size: Integer read FSize write SetSize;
    property IconName: string read FIconName write SetIconName;
    property SVGText: string read GetSVGText write SetSVGText;
  end;

  TSVGIconFixedBitmapItemClass = class of TSVGIconFixedBitmapItem;
  TSVGIconImage = class;

  TSVGIconFixedMultiResBitmap = class(TFixedMultiResBitmap)
  private
    FOwnerImage: TSVGIconImage;
    procedure UpdateImageSize(const ASize: Integer);
  public
    constructor Create(AOwner: TPersistent; ItemClass: TSVGIconFixedBitmapItemClass); overload;
    constructor Create(AOwner: TPersistent); overload;
  end;

  TSVGIconImage = class(TImage)
  private
    FSVGIconMultiResBitmap: TSVGIconFixedMultiResBitmap;
    function GetBitmapSize: Integer;
    procedure SetBitmapSize(const AValue: Integer);
  protected
    function CreateMultiResBitmap: TFixedMultiResBitmap; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(X, Y, AWidth, AHeight: Single); override;
  published
    property BitmapSize: Integer read GetBitmapSize write SetBitmapSize;
  end;

implementation

uses
  System.Math
  , FMX.SVGIconImageList
  , System.RTLConsts
  , System.SysUtils
  , System.Character
  , FMX.Forms
  , FMX.Consts
  , GDIPOBJ2;

{ TSVGIconFixedMultiResBitmap }

constructor TSVGIconFixedMultiResBitmap.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TSVGIconFixedBitmapItem);
  if AOwner is TSVGIconImage then
    FOwnerImage := TSVGIconImage(AOwner);
end;

constructor TSVGIconFixedMultiResBitmap.Create(AOwner: TPersistent; ItemClass: TSVGIconFixedBitmapItemClass);
begin
  inherited Create(AOwner, ItemClass);
  if AOwner is TSVGIconImage then
    FOwnerImage := TSVGIconImage(AOwner);
end;

procedure TSVGIconFixedMultiResBitmap.UpdateImageSize(const ASize: Integer);
var
  I, J: Integer;
  LItem: TFixedBitmapItem;
begin
  for I := 0 to ScaleList.Count - 1 do
  begin
    for J := 0 to Count - 1 do
    begin
      LItem := Items[J];
      if LItem is TSVGIconFixedBitmapItem then
        TSVGIconFixedBitmapItem(LItem).Size := ASize;
    end;
  end;
end;

{ TSVGIconFixedBitmapItem }

function TSVGIconFixedBitmapItem.BitmapStored: Boolean;
begin
  Result := False;
end;

constructor TSVGIconFixedBitmapItem.Create(Collection: TCollection);
begin
  inherited;
  if Collection is TSVGIconFixedMultiResBitmap then
    FOwnerCollection := Collection as TSVGIconFixedMultiResBitmap;
  FSize := 16;
  FOpacity := 1;
  FSVG := TSVG.Create;
end;

destructor TSVGIconFixedBitmapItem.Destroy;
begin
  FSVG.DisposeOf;
  inherited;
end;

procedure TSVGIconFixedBitmapItem.DrawSVGIcon;
var
  LBitmap: TBitmap;
  LBitmapSize: Single;
begin
  LBitmap := inherited Bitmap;
  LBitmap.Clear(talphacolors.Null);
  LBitmapSize := Size * Scale;
  LBitmap.Width  := Round(LBitmapSize);
  LBitmap.Height := Round(LBitmapSize);
  PaintToBitmap(LBitmap, FSVG);
end;

function TSVGIconFixedBitmapItem.GetBitmap: TBitmapOfItem;
begin
  DrawSVGIcon;
  Result := inherited Bitmap;
end;

function TSVGIconFixedBitmapItem.GetDisplayName: string;
begin
  Result := FIconName;
end;

function TSVGIconFixedBitmapItem.GetSVGText: string;
begin
  Result := FSVG.Source;
end;

procedure TSVGIconFixedBitmapItem.SetBitmap(const AValue: TBitmapOfItem);
begin
  inherited Bitmap.Assign(AValue);
  inherited Bitmap.BitmapScale := Scale;
end;

procedure TSVGIconFixedBitmapItem.SetIconName(const AValue: string);
begin
  FIconName := AValue;
end;

procedure TSVGIconFixedBitmapItem.SetOpacity(const AValue: Single);
begin
  FOpacity := AValue;
  DrawSVGIcon;
end;

procedure TSVGIconFixedBitmapItem.SetSize(const AValue: Integer);
begin
  if (Trunc(AValue) > 0) and (AValue <> FSize) then
  begin
    FSize := AValue;
    DrawSVGIcon;
  end;
end;

procedure TSVGIconFixedBitmapItem.SetSVGText(const Value: string);
begin
  FSVG.LoadFromText(Value);
end;

function TSVGIconFixedBitmapItem.StoreOpacity: Boolean;
begin
  Result := FOpacity <> 1;
end;

{ TSVGIconImage }

constructor TSVGIconImage.Create(AOwner: TComponent);
begin
  inherited;
  DisableInterpolation := True;
  FSVGIconMultiResBitmap := MultiResBitmap as TSVGIconFixedMultiResBitmap;
end;

function TSVGIconImage.CreateMultiResBitmap: TFixedMultiResBitmap;
begin
  Result := TSVGIconFixedMultiResBitmap.Create(Self, TSVGIconFixedBitmapItem);
end;

destructor TSVGIconImage.Destroy;
begin
  inherited;
  FSVGIconMultiResBitmap := nil;
end;

function TSVGIconImage.GetBitmapSize: Integer;
begin
  Result := Round(Inherited width);
end;

procedure TSVGIconImage.SetBitmapSize(const AValue: Integer);
begin
  if AValue <> 0 then
    FSVGIconMultiResBitmap.UpdateImageSize(AValue);
end;

procedure TSVGIconImage.SetBounds(X, Y, AWidth, AHeight: Single);
begin
  inherited;
  BitmapSize := Trunc(Min(AWidth, AHeight));
end;

initialization
  RegisterFmxClasses([TSVGIconImage]);

  StartClassGroup(TFmxObject);
  ActivateClassGroup(TFmxObject);
  GroupDescendentsWith(FMX.SVGIconImage.TSVGIconImage, TFmxObject);

end.
