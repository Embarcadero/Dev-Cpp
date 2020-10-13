{******************************************************************************}
{                                                                              }
{       SVGIconImageList: An extended ImageList for Delphi/VCL                 }
{       to simplify use of SVG Icons (resize, opacity and more...)             }
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
unit FMX.SVGIconImageList;

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
  , SVGTypes
  , SVGColor
  ;

const
  SVGIconImageListVersion = '2.1.0';

resourcestring
  ERROR_LOADING_FILES = 'SVG error loading files:';

type
  TSVGIconMultiResBitmap = class;
  TSVGIconImageList = class;
  TSVGIconSourceItem = class;

  TSVGIconBitmapItem = class(TCustomBitmapItem)
  private
    FSize: Integer;
    FOwnerMultiResBitmap: TSVGIconMultiResBitmap;
    procedure SetBitmap(const AValue: TBitmapOfItem);
    function GetBitmap: TBitmapOfItem;
    procedure SetSize(const AValue: Integer);
    procedure DrawSVGIcon;
    function GetOpacity: single;
    function GetSize: Integer;
    function GetSVG: TSVG;
    function GetGrayScale: Boolean;
    function GetSVGColor: TColor;
  protected
    function BitmapStored: Boolean; override;
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    property SVG: TSVG read GetSVG;
  published
    property Bitmap: TBitmapOfItem read GetBitmap write SetBitmap stored False;
    property Scale;
    property Size: Integer read GetSize write SetSize default 32;
    //Readonly properties from Source Item
    property Opacity: single read GetOpacity stored false;
    property FixedColor: TColor read GetSVGColor stored false;
    property GrayScale: Boolean read GetGrayScale stored false;
  end;

  TSVGIconBitmapItemClass = class of TSVGIconBitmapItem;

  TSVGIconMultiResBitmap = class(TMultiResBitmap)
  private
    FOwnerSourceItem: TSVGIconSourceItem;
    procedure UpdateImageSize(const ASize: Integer);
  protected
    constructor Create(AOwner: TPersistent; ItemClass: TSVGIconBitmapItemClass); overload;
  public
  end;

  {TSVGIconSourceItem}
  TSVGIconSourceItem = class(TCustomSourceItem)
  private
    FOwnerImageList: TSVGIconImageList;
    FSVG: TSVG;
    FOpacity: single;
    FFixedColor: TColor;
    FGrayScale: Boolean;
    procedure UpdateAllItems;
    procedure SetOpacity(const AValue: single);
    procedure AutoSizeBitmap(const ASize: Integer);
    function GetIconName: string;
    procedure SetIconName(const Value: string);
    function GetOpacity: single;
    function GetDestinationItem: TCustomDestinationItem;
    procedure SetSVG(const Value: TSVG);
    procedure SetSVGText(const Value: string);
    function GetSVGText: string;
    procedure SetFixedColor(const Value: TColor);
    procedure SetGrayScale(const Value: Boolean);
    function GetFixedColor: TColor;
    function GetGrayScale: Boolean;
  protected
    function GetDisplayName: string; override;
    function CreateMultiResBitmap: TMultiResBitmap; override;
    function StoreOpacity: Boolean; virtual;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property SVG: TSVG read FSVG write SetSVG;
  published
    property MultiResBitmap;
    property IconName: string read GetIconName write SetIconName;
    property Opacity: single read GetOpacity write SetOpacity stored StoreOpacity;
    property SVGText: string read GetSVGText write SetSVGText;
    property FixedColor: TColor read GetFixedColor write SetFixedColor default SVG_INHERIT_COLOR;
    property GrayScale: Boolean read GetGrayScale write SetGrayScale default False;
  end;

  {TSVGIconImageList}
  TSVGIconImageList = class(TCustomImageList)
  private
    FSize: Integer;
    FAutoSizeBitmaps: Boolean;
    FOpacity: single;
    FFixedColor: TColor;
    FGrayScale: Boolean;
    function StoreOpacity: Boolean;
    procedure SetAutoSizeBitmaps(const Value: Boolean);
    procedure UpdateSourceItems;
    procedure UpdateDestination(Size: TSize; const Index: Integer);
    procedure SetOpacity(const Value: single);
    function GetSize: Integer;
    procedure SetSize(const Value: Integer);
    procedure SetFixedColor(const Value: TColor);
    procedure SetGrayScale(const Value: Boolean);
  protected
    procedure Loaded; override;
    function CreateSource: TSourceCollection; override;
    function DoBitmap(Size: TSize; const Index: Integer): TBitmap; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    procedure DeleteIcon(const AIndex: Integer);
    function InsertIcon(const AIndex: Integer;
      const ASVGText: string; const AIconName: string = ''): TSVGIconSourceItem;
    function CloneIcon(const AIndex: Integer; const AInsertIndex: Integer = -1): TSVGIconSourceItem;
    //Multiple icons methods
    function LoadFromFiles(const AFileNames: TStrings;
      const AAppend: Boolean = True): Integer;
    procedure ClearIcons; virtual;
    procedure UpdateIconAttributes(const ASize: Integer; const AOpacity: Single); overload;
  published
    property Source;
    property Destination;
    property OnChange;
    property OnChanged;
    property Size: Integer read GetSize write SetSize default 32;
    property AutoSizeBitmaps: Boolean read FAutoSizeBitmaps write SetAutoSizeBitmaps default True;
    property Opacity: single read FOpacity write SetOpacity stored StoreOpacity;
    property FixedColor: TColor read FFixedColor write SetFixedColor default SVG_INHERIT_COLOR;
    property GrayScale: Boolean read FGrayScale write SetGrayScale default False;
  end;

procedure PaintToBitmap(const ABitmap: TBitmap; const ASVG: TSVG);

implementation

uses
  System.Math
  , System.RTLConsts
  , System.SysUtils
  , FMX.Forms
  , FMX.Consts
  , Winapi.GDIPOBJ
  , Winapi.GDIPAPI
  ;


procedure PaintToBitmap(const ABitmap: TBitmap; const ASVG: TSVG);
var
  GPGraphics: TGPGraphics;
  GPBitmap: TGPBitmap;
  GPRectF: TGPRectF;
  RectArray: TRectarray;
  GPRect: TGPRect;
  GPBitmapData: Winapi.GDIPAPI.TBitmapData;
  BitmapData: FMX.Graphics.TBitmapData;
  Bitmap: TBitmap;
  Source: PByte;
  Dest: PByte;
  Y: Integer;
begin
  GPBitmap := TGPBitmap.Create(ABitmap.Canvas.Width, ABitmap.Canvas.Height);
  GPGraphics := TGPGraphics.Create(GPBitmap);
  try
    GPGraphics.SetSmoothingMode(SmoothingModeAntiAlias);
    GPRectF.X := 0;
    GPRectF.Y := 0;
    GPRectF.Width := ABitmap.Width;
    GPRectF.Height := ABitmap.Height;

    RectArray := TRectArray.Create(TRect.Create(0, 0, ABitmap.Width, ABitmap.Height));
    ASVG.PaintTo(GPGraphics, GPRectF, @RectArray, 1);

    GPRect.X := 0;
    GPRect.Y := 0;
    GPRect.Width := GPBitmap.GetWidth;
    GPRect.Height := GPBitmap.GetHeight;

    GPBitmap.LockBits(GPRect, ImageLockModeRead, PixelFormat32bppPARGB, GPBitmapData);

    Bitmap := TBitmap.Create(GPRect.Width, GPRect.Height);
    try
      Bitmap.Map(TMapAccess.Write, BitmapData);

      Source := GPBitmapData.Scan0;
      Dest := BitmapData.Data;
      for Y := 0 to GPBitmapData.Height - 1 do
      begin
        Move(Source^, Dest^, GPBitmapData.Stride);
        Source := Source + GPBitmapData.Stride;
        Dest := Dest + BitmapData.Pitch;
      end;

      Bitmap.Unmap(BitmapData);

      ABitmap.Canvas.BeginScene;
      Try
        ABitmap.Clear(TAlphaColors.Null);
        ABitmap.Canvas.DrawBitmap(Bitmap, TRectF.Create(0, 0, ABitmap.Width, ABitmap.Height),
          TRectF.Create(0, 0, ABitmap.Width, ABitmap.Height), 100);
      Finally
        ABitmap.Canvas.EndScene;
      End;
    finally
      Bitmap.Free;
    end;

    GPBitmap.UnlockBits(GPBitmapData);
  finally
    GPGraphics.Free;
    GPBitmap.Free;
  end;
end;

{ TSVGIconBitmapItem }

function TSVGIconBitmapItem.BitmapStored: Boolean;
begin
  Result := False;
end;

constructor TSVGIconBitmapItem.Create(Collection: TCollection);
begin
  inherited;
  if Collection is TSVGIconMultiResBitmap then
    FOwnerMultiResBitmap := Collection as TSVGIconMultiResBitmap;
  FSize := 32;
end;

procedure TSVGIconBitmapItem.DrawSVGIcon;
var
  LBitmap: TBitmap;
  LBitmapSize: Integer;
begin
  LBitmap := inherited Bitmap;
  LBitmapSize := Round(Size * Scale);
  LBitmap.Width  := LBitmapSize;
  LBitmap.Height := LBitmapSize;
  SVG.SVGOpacity := Opacity;
  SVG.FixedColor := FixedColor;
  SVG.Grayscale := GrayScale;
  PaintToBitmap(LBitmap, SVG);
end;

function TSVGIconBitmapItem.GetBitmap: TBitmapOfItem;
begin
  DrawSVGIcon;
  Result := inherited Bitmap;
end;

function TSVGIconBitmapItem.GetDisplayName: string;
begin
  Result := Format('%s - %dx%d - Scale: %s',
    [FOwnerMultiResBitmap.FOwnerSourceItem.Name,
     Size, Size, FloatToStr(Scale)]);
end;

function TSVGIconBitmapItem.GetGrayScale: Boolean;
begin
  Result := FOwnerMultiResBitmap.FOwnerSourceItem.GrayScale;
end;

function TSVGIconBitmapItem.GetOpacity: single;
begin
  Result := FOwnerMultiResBitmap.FOwnerSourceItem.Opacity;
end;

function TSVGIconBitmapItem.GetSize: Integer;
begin
  Result := FSize;
end;

function TSVGIconBitmapItem.GetSVG: TSVG;
begin
  Result := FOwnerMultiResBitmap.FOwnerSourceItem.SVG;
end;

function TSVGIconBitmapItem.GetSVGColor: TColor;
begin
  Result := FOwnerMultiResBitmap.FOwnerSourceItem.FixedColor;
end;

procedure TSVGIconBitmapItem.SetBitmap(const AValue: TBitmapOfItem);
begin
  inherited Bitmap.Assign(AValue);
  inherited Bitmap.BitmapScale := Scale;
end;

procedure TSVGIconBitmapItem.SetSize(const AValue: Integer);
begin
  if (AValue > 0) and (AValue <> FSize) then
  begin
    FSize := AValue;
    DrawSVGIcon;
  end;
end;

{ TSVGIconMultiResBitmap }

constructor TSVGIconMultiResBitmap.Create(AOwner: TPersistent;
  ItemClass: TSVGIconBitmapItemClass);
begin
  inherited Create(AOwner, ItemClass);
  if (AOwner is TSVGIconSourceItem) then
    FOwnerSourceItem := TSVGIconSourceItem(AOwner)
  else
    FOwnerSourceItem := nil;
end;

procedure TSVGIconMultiResBitmap.UpdateImageSize(const ASize: Integer);
var
  I, J: Integer;
  LItem: TSVGIconBitmapItem;
begin
  for I := 0 to ScaleList.Count - 1 do
  begin
    for J := 0 to Count - 1 do
    begin
      LItem := Items[J] as TSVGIconBitmapItem;
      if LItem.Size <> ASize then
      begin
        LItem.Size := ASize;
        LItem.DrawSVGIcon;
      end;
    end;
  end;
end;

{ TSVGIconSourceItem }

procedure TSVGIconSourceItem.Assign(Source: TPersistent);
begin
  if Source is TSVGIconSourceItem then
  begin
    FOpacity := TSVGIconSourceItem(Source).FOpacity;
    FFixedColor := TSVGIconSourceItem(Source).FFixedColor;
    FGrayScale := TSVGIconSourceItem(Source).FGrayScale;
    FSVG.LoadFromText(TSVGIconSourceItem(Source).SVG.Source);
  end;
  inherited;
end;

procedure TSVGIconSourceItem.AutoSizeBitmap(const ASize: Integer);
begin
  //If present, delete multiple items
  while MultiResBitmap.Count > 1 do
    MultiResBitmap.Delete(MultiResBitmap.Count-1);
  //Add only one item
  if MultiResBitmap.Count = 0 then
    MultiResBitmap.Add;
  (MultiResBitmap as TSVGIconMultiResBitmap).UpdateImageSize(ASize);
end;

constructor TSVGIconSourceItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FSVG := TSVG.Create;
  FOpacity := -1;
  FixedColor := SVG_INHERIT_COLOR;
  FGrayScale := False;
  UpdateAllItems;
end;

function TSVGIconSourceItem.CreateMultiResBitmap: TMultiResBitmap;
begin
  Result := TSVGIconMultiResBitmap.Create(self, TSVGIconBitmapItem);
  FOwnerImageList := Result.ImageList as TSVGIconImageList;
end;

destructor TSVGIconSourceItem.Destroy;
begin
  FSVG.DisposeOf;
  inherited;
end;

function TSVGIconSourceItem.GetDisplayName: string;
begin
  Result := Format('%d.%s', [Index, Name])
end;

function TSVGIconSourceItem.GetFixedColor: TColor;
begin
  if FFixedColor = SVG_INHERIT_COLOR then
    Result := FOwnerImageList.FixedColor
  else
    Result := FFixedColor;
end;

function TSVGIconSourceItem.GetGrayScale: Boolean;
begin
  if not FGrayScale then
    Result := FOwnerImageList.FGrayScale
  else
    Result := FGrayScale;
end;

function TSVGIconSourceItem.GetIconName: string;
begin
  Result := inherited Name;
end;

function TSVGIconSourceItem.GetOpacity: single;
begin
  if FOpacity = -1 then
    Result := FOwnerImageList.FOpacity
  else
    Result := FOpacity;
end;

function TSVGIconSourceItem.GetSVGText: string;
begin
  Result := SVG.Source;
end;

function TSVGIconSourceItem.GetDestinationItem: TCustomDestinationItem;
var
  LDest: TCustomDestinationItem;
begin
  Result := nil;
  if FOwnerImageList.Destination.Count > Index then
  begin
    LDest := FOwnerImageList.Destination.Items[Index];
    if (LDest.LayersCount > 0) and
      SameText(LDest.Layers[0].Name, IconName) then
      Result := LDest;
  end;
end;

procedure TSVGIconSourceItem.SetFixedColor(const Value: TColor);
begin
  if FFixedColor <> Value then
  begin
    FFixedColor := Value;
    UpdateAllItems;
  end;
end;

procedure TSVGIconSourceItem.SetGrayScale(const Value: Boolean);
begin
  if FGrayScale <> Value then
  begin
    FGrayScale := Value;
    UpdateAllItems;
  end;
end;

procedure TSVGIconSourceItem.SetIconName(const Value: string);
var
  LDest: TCustomDestinationItem;
begin
  if Value <> Name then
  begin
    LDest := GetDestinationItem;
    inherited Name := Value;
    if Assigned(LDest) then
      LDest.Layers[0].Name := Value;
  end;
end;

procedure TSVGIconSourceItem.SetOpacity(const AValue: single);
begin
  if Assigned(FOwnerImageList) and (AValue = FOwnerImageList.Opacity) then
  begin
    FOpacity := -1;
  end
  else
    FOpacity := AValue;
  UpdateAllItems;
end;

procedure TSVGIconSourceItem.SetSVG(const Value: TSVG);
begin
  if not SameText(FSVG.Source, Value.Source) then
  begin
    FSVG.LoadFromText(Value.Source);
    UpdateAllItems;
  end;
end;

procedure TSVGIconSourceItem.SetSVGText(const Value: string);
begin
  FSVG.LoadFromText(Value);
  UpdateAllItems;
end;

function TSVGIconSourceItem.StoreOpacity: Boolean;
begin
  Result := (FOwnerImageList = nil) or (FOpacity <> FOwnerImageList.FOpacity);
end;

procedure TSVGIconSourceItem.UpdateAllItems;
var
  I: Integer;
  LItem: TSVGIconBitmapItem;
  LSize: TSize;
begin
  for I := 0 to MultiResBitmap.Count -1 do
  begin
    LItem := MultiResBitmap.Items[I] as TSVGIconBitmapItem;
    Litem.DrawSVGIcon;
    if (I=0) and (FOwnerImageList <> nil) then
    begin
      LItem.Size := FOwnerImageList.Size;
      LSize.cx := LItem.Size;
      LSize.cy := LItem.Size;
      FOwnerImageList.UpdateDestination(LSize, Index);
    end;
  end;
end;

{ TSVGIconImageList }

function TSVGIconImageList.InsertIcon(const AIndex: Integer;
  const ASVGText: string; const AIconName: string = ''): TSVGIconSourceItem;
var
  LItem: TSVGIconSourceItem;
  LDest: TCustomDestinationItem;
begin
  LItem := Self.Source.Insert(AIndex) as TSVGIconSourceItem;
  Result := LItem;
  LItem.MultiResBitmap.Add;
  LItem.SVGText := ASVGText;
  LDest := Self.Destination.Insert(AIndex);
  try
    if AIconName <> '' then
      LItem.Name := AIconName;
  finally
    with LDest.Layers.Add do
      Name := LItem.Name;
  end;
end;

function TSVGIconImageList.CloneIcon(const AIndex: Integer; const AInsertIndex: Integer = -1): TSVGIconSourceItem;
var
  LItem: TSVGIconSourceItem;
  LNewIndex: Integer;
begin
  LItem := Self.Source.Items[AIndex] as TSVGIconSourceItem;

  if AInsertIndex >= 0 then LNewIndex := AInsertIndex
  else LNewIndex := AIndex;

  Result := InsertIcon(LNewIndex, LItem.SVGText);
  Result.Opacity := LItem.Opacity;
  Result.FixedColor := LItem.FixedColor;
  Result.GrayScale := LItem.GrayScale;
  Result.SVG.LoadFromText(LItem.SVG.Source);

  // Result.Assign(Self.Destination.Items[AIndex]);

  UpdateSourceItems;
end;

function TSVGIconImageList.LoadFromFiles(const AFileNames: TStrings;
  const AAppend: Boolean = True): Integer;
var
  LIndex: Integer;
  LSVG: TSVG;
  LIconName, LFileName: string;
  LItem: TSVGIconSourceItem;
  LErrors: string;
begin
  Result := 0;
  LSVG := TSVG.Create;
  try
    if not AAppend then
      ClearIcons;
    for LIndex := 0 to AFileNames.Count - 1 do
    begin
      LFileName := AFileNames[LIndex];
      LSVG.LoadFromFile(LFileName);
      LIconName := ChangeFileExt(ExtractFileName(LFileName), '');
      try
        LItem := InsertIcon(Source.Count, LSVG.Source, LIconName);
        LItem.SVG := LSVG;
        Inc(Result);
      except
        on E: Exception do
          LErrors := LErrors + Format('%s (%s)',[E.Message, LFileName]) + sLineBreak;
      end;
    end;
    if LErrors <> '' then
      raise Exception.Create(ERROR_LOADING_FILES+sLineBreak+LErrors);
  finally
    LSVG.Free;
  end;
end;

procedure TSVGIconImageList.Assign(Source: TPersistent);
begin
  if Source is TSVGIconImageList then
  begin
    FSize := TSVGIconImageList(Source).FSize;
    Opacity := TSVGIconImageList(Source).Opacity;
    FFixedColor := TSVGIconImageList(Source).FFixedColor;
    FGrayScale := TSVGIconImageList(Source).FGrayScale;
    FAutoSizeBitmaps := TSVGIconImageList(Source).FAutoSizeBitmaps;
  end;
  inherited;
end;

procedure TSVGIconImageList.ClearIcons;
begin
  Source.Clear;
  Destination.Clear;
end;

constructor TSVGIconImageList.Create(AOwner: TComponent);
begin
  inherited;
  FAutoSizeBitmaps := True;
  FOpacity := 1;
  FSize := 32;
  FixedColor := SVG_INHERIT_COLOR;
  FGrayScale := False;
end;

function TSVGIconImageList.CreateSource: TSourceCollection;
begin
  Result := TSourceCollection.Create(self, TSVGIconSourceItem);
end;

procedure TSVGIconImageList.UpdateDestination(Size: TSize;
  const Index: Integer);
var
  LDestItem: TDestinationItem;
  LSourceItem: TSVGIconSourceItem;
  LIndex: Integer;
  LSize: Integer;
begin
  while Index > Destination.Count-1 do
    Destination.Add;
  LDestItem := Destination.Items[Index] as TDestinationItem;
  if LDestItem.Layers.Count > 0 then
  begin
    LIndex := Source.indexOf(LDestItem.Layers[0].Name);
    if LIndex >= 0 then
    begin
      LSourceItem := Source.Items[LIndex] as TSVGIconSourceItem;
      if Assigned(LSourceItem) then
      begin
        if FAutoSizeBitmaps then
        begin
          LSize := Min(Size.cx, Size.cy);
          LSourceItem.AutoSizeBitmap(LSize);
        end
        else
          LSize := LSourceItem.FOwnerImageList.Size;
        LDestItem.Layers[0].SourceRect.Top := 0;
        LDestItem.Layers[0].SourceRect.Left := 0;
        LDestItem.Layers[0].SourceRect.Right := LSize;
        LDestItem.Layers[0].SourceRect.Bottom := LSize;
      end;
    end;
  end;
end;

procedure TSVGIconImageList.UpdateIconAttributes(const ASize: Integer;
  const AOpacity: Single);
//var
//  I: Integer;
//  LSVGIconItem: TSVGIconSourceItem;
begin
  (*
  Self.Size := ASize;
  for I := 0 to Source.Count -1 do
  begin
    LSVGIconItem := Source.Items[I] as TSVGIconSourceItem;
    LSVGIconItem.UpdateIconAttributes(FOpacity);
  end;
  *)
end;

procedure TSVGIconImageList.DeleteIcon(const AIndex: Integer);
var
  LDest: TCustomDestinationItem;
  LSourceItem: TSVGIconSourceItem;
begin
  LSourceItem := Source.Items[AIndex] as TSVGIconSourceItem;
  if Assigned(LSourceItem) then
  begin
    LDest := LSourceItem.GetDestinationItem;
    Source.Delete(AIndex);
    if Assigned(LDest) then
      Destination.Delete(AIndex);
  end;
end;

function TSVGIconImageList.DoBitmap(Size: TSize;
  const Index: Integer): TBitmap;
begin
  UpdateDestination(Size, Index);
  Result := inherited DoBitmap(Size, Index);
end;

function TSVGIconImageList.GetSize: Integer;
begin
  Result := FSize;
end;

procedure TSVGIconImageList.Loaded;
begin
  inherited;
  UpdateSourceItems;
end;

procedure TSVGIconImageList.SetAutoSizeBitmaps(const Value: Boolean);
begin
  FAutoSizeBitmaps := Value;
  if (Count > 0) then
    UpdateSourceItems;
end;

procedure TSVGIconImageList.SetFixedColor(const Value: TColor);
begin
  if FFixedColor <> Value then
  begin
    FFixedColor := Value;
    UpdateSourceItems;
  end;
end;

procedure TSVGIconImageList.SetGrayScale(const Value: Boolean);
begin
  if FGrayScale <> Value then
  begin
    FGrayScale := Value;
    UpdateSourceItems;
  end;
end;

procedure TSVGIconImageList.UpdateSourceItems;
var
  I: Integer;
  LSourceItem: TSVGIconSourceItem;
begin
  for I := 0 to Source.Count -1 do
  begin
    LSourceItem := Source[I] as TSVGIconSourceItem;
    if LSourceItem.FOpacity = -1 then
      LSourceItem.Opacity := FOpacity;
    if not LSourceItem.GrayScale then
      LSourceItem.GrayScale := FGrayScale;
    if LSourceItem.FixedColor = SVG_INHERIT_COLOR then
      LSourceItem.FixedColor := FFixedColor;
    LSourceItem.UpdateAllItems;
  end;
end;

procedure TSVGIconImageList.SetOpacity(const Value: single);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    UpdateSourceItems;
  end;
end;

procedure TSVGIconImageList.SetSize(const Value: Integer);
begin
  if FSize <> Value then
  begin
    FSize := Value;
    UpdateSourceItems;
  end;
end;

function TSVGIconImageList.StoreOpacity: Boolean;
begin
  Result := FOpacity <> 1;
end;

initialization
  RegisterFmxClasses([TSVGIconImageList]);

  StartClassGroup(TFmxObject);
  ActivateClassGroup(TFmxObject);
  GroupDescendentsWith(FMX.SVGIconImageList.TSVGIconImageList, TFmxObject);

end.
