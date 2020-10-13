{******************************************************************************}
{                                                                              }
{       SVGIconImageList: An extended ImageList for Delphi/VCL                 }
{       to simplify use of SVG Icons (resize, opacity and more...)             }
{                                                                              }
{       Copyright (c) 2019-2020 (Ethea S.r.l.)                                 }
{       Author: Vincent Parrett                                                }
{       Contributors: Carlo Barazzetta, Kiriakos Vlahos                        }
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
unit SVGIconVirtualImageList;

interface

{$INCLUDE SVGIconImageList.inc}

uses
  WinApi.Windows,
  Winapi.CommCtrl,
  System.Classes,
  System.Messaging,
  Vcl.Controls,
  Vcl.Graphics,
{$IFDEF D10_3+}
  Vcl.VirtualImageList,
{$ENDIF}
  SVGInterfaces,
  SVGIconImageListBase,
  SVGIconImageCollection;

type
  TSVGIconVirtualImageList = class(TSVGIconImageListBase)
  private
    FImageCollection : TSVGIconImageCollection;
  protected
    // override abstract methods
    function GetSVGIconItems: TSVGIconItems; override;
    procedure RecreateBitmaps; override;
    procedure SetImageCollection(const value: TSVGIconImageCollection);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure DoAssign(const source : TPersistent); override;
    function GetCount: Integer; override;

  public
    procedure PaintTo(const ACanvas: TCanvas; const AIndex: Integer;
      const X, Y, AWidth, AHeight: Single; AEnabled: Boolean = True); override;

  published
    //Publishing properties of Custom Class
    property OnChange;
    //New properties
    property Opacity;
    property Width;
    property Height;
    property Size;
    property FixedColor;
    property GrayScale;
    property DisabledGrayScale;
    property DisabledOpacity;
    property ImageCollection : TSVGIconImageCollection read FImageCollection write SetImageCollection;

    {$IFDEF HiDPISupport}
    property Scaled;
    {$ENDIF}
  end;


implementation

uses
  System.Types,
  System.UITypes,
  System.Math,
  System.SysUtils,
  Vcl.Forms,
  Vcl.ImgList,
  SVGIconImageList;

{ TSVGIconVirtualImageList }

procedure TSVGIconVirtualImageList.DoAssign(const source: TPersistent);
begin
  inherited;
  if Source is TSVGIconImageList then
  begin
    if FImageCollection <> nil then
      FImageCollection.SVGIconItems.Assign(TSVGIconImageList(Source).SVGIconItems);
  end
  else if Source is TSVGIconVirtualImageList then
    SetImageCollection(TSVGIconVirtualImageList(Source).FImageCollection);
end;

function TSVGIconVirtualImageList.GetCount: Integer;
begin
  if FImageCollection <> nil then
    result := FImageCollection.SVGIconItems.Count
  else
    result := 0;
end;

function TSVGIconVirtualImageList.GetSVGIconItems: TSVGIconItems;
begin
  if Assigned(FImageCollection) then
    Result := FImageCollection.SVGIconItems
  else
    Result := nil;
end;

procedure TSVGIconVirtualImageList.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FImageCollection) then
  begin
    BeginUpdate;
    try
      FImageCollection := nil;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TSVGIconVirtualImageList.PaintTo(const ACanvas: TCanvas;
  const AIndex: Integer; const X, Y, AWidth, AHeight: Single; AEnabled: Boolean);
var
  LSVG: ISVG;
  LItem: TSVGIconItem;
  LOpacity: Byte;
begin
  if (FImageCollection <> nil) and (AIndex >= 0) and (AIndex < FImageCollection.SVGIconItems.Count) then
  begin
    LItem := FImageCollection.SVGIconItems[AIndex];
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

procedure TSVGIconVirtualImageList.RecreateBitmaps;
var
  C: Integer;
  LItem: TSVGIconItem;
  BitMap: TBitmap;
  LFixedColor: TColor;
  LGrayScale: Boolean;
begin
  if not Assigned(FImageCollection) or
    ([csLoading, csDestroying, csUpdating] * ComponentState <> [])
  then
    Exit;

  ImageList_Remove(Handle, -1);
  if (Width > 0) and (Height > 0) then
  begin
    HandleNeeded;
    if FImageCollection.FixedColor <> SVG_INHERIT_COLOR then
      LFixedColor := FImageCollection.FixedColor
    else
      LFixedColor := FFixedColor;
    if FGrayScale or FImageCollection.GrayScale then
      LGrayscale := True
    else
      LGrayscale := False;
    for C := 0 to FImageCollection.SVGIconItems.Count - 1 do
    begin
      LItem := FImageCollection.SVGIconItems[C];
      Bitmap := LItem.GetBitmap(Width, Height, LFixedColor, FOpacity, LGrayScale);
      try
        ImageList_Add(Handle, Bitmap.Handle, 0);
      finally
        Bitmap.Free;
      end;
    end;
  end;
end;

procedure TSVGIconVirtualImageList.SetImageCollection(const value: TSVGIconImageCollection);
begin
  if FImageCollection <> Value then
  begin
    if FImageCollection <> nil then
      FImageCollection.RemoveFreeNotification(Self);
    FImageCollection := Value;
    if FImageCollection <> nil then
      FImageCollection.FreeNotification(Self);
    Change;
  end;
end;

end.
