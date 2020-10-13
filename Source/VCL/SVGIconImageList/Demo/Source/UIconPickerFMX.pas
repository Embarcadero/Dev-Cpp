{******************************************************************************}
{                                                                              }
{       Icon Fonts ImageList: An extended ImageList for Delphi/FireMonkey      }
{       to simplify use of Icons (resize, colors and more...)                  }
{                                                                              }
{       Copyright (c) 2019-2020 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{                                                                              }
{       https://github.com/EtheaDev/IconFontsImageList                         }
{                                                                              }
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
unit UIconPickerFMX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.ImageList, FMX.ImgList, FMX.StdCtrls, FMX.Layouts, FMX.ExtCtrls,
  FMX.Colors, FMX.Controls.Presentation, FMX.Edit, FMX.ListBox, FMX.Objects,
  FMX.ScrollBox, FMX.Memo, SVG;

type
  TIconPicker = class(TForm)
    Preview: TGroupBox;
    Image16x16: TImage;
    Image32x32: TImage;
    Image64x64: TImage;
    Label1: TLabel;
    edtPath: TEdit;
    Image90x90: TImage;
    Label2: TLabel;
    edtFileName: TEdit;
    Image120x120: TImage;
    Memo: TMemo;
    LoadButton: TButton;
    Image100x100: TImage;
    procedure LoadButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MemoExit(Sender: TObject);
  private
    FSVG: TSVG;
    procedure UpdatePreview;
    procedure DrawIcon(const Size: Integer;
      const ImageViewer: TImage);
  public

    { Public declarations }
  end;

var
  IconPicker: TIconPicker;

implementation

{$R *.fmx}

uses
  Winapi.GDIPOBJ
  , Winapi.GDIPAPI
  , SVGTypes;

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

procedure TIconPicker.DrawIcon(const Size: Integer;
  const ImageViewer: TImage);
begin
  ImageViewer.Bitmap.Width := Size;
  ImageViewer.Bitmap.Height := Size;
  PaintToBitmap(ImageViewer.Bitmap, FSVG);
end;

procedure TIconPicker.FormCreate(Sender: TObject);
begin
  FSVG := TSVG.Create;
end;

procedure TIconPicker.LoadButtonClick(Sender: TObject);
var
  LFileName: string;
begin
  LFileName := edtPath.Text + edtFileName.Text;
  FSVG.LoadFromFile(LFileName);
  UpdatePreview;
end;

procedure TIconPicker.MemoExit(Sender: TObject);
begin
  FSVG.LoadFromText(Memo.Lines.Text);
  UpdatePreview;
end;

procedure TIconPicker.UpdatePreview;
begin
  Memo.Lines.Text := FSVG.Source;
(*
  DrawIcon( 16, Image16x16);
  DrawIcon( 32, Image32x32);
  DrawIcon( 64, Image64x64);
  DrawIcon( 90, Image90x90);
  DrawIcon(120, Image120x120);
*)
  DrawIcon(100, Image100x100);
end;



end.
