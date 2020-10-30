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
unit SVGIconImageCollection;

interface

{$INCLUDE SVGIconImageList.inc}

uses
  System.Types
  , System.UITypes
  , System.Classes
  , WinApi.Windows
  , Vcl.Graphics
{$IFDEF D10_3+}
  , Vcl.BaseImageCollection
{$ENDIF}
  , SvgInterfaces
  , SVGIconItems
  ;

//alias to make usage simpler (less uses clause entries).
type
  TSVGIconItem = SVGIconItems.TSVGIconItem;
  TSVGIconItems = SVGIconItems.TSVGIconItems;

type
  {$IFDEF D10_3+}
  TSVGIconImageCollection = class(TCustomImageCollection)
  {$ELSE}
  TSVGIconImageCollection = class(TComponent)
  {$ENDIF}
  private
    FSVGItems: TSVGIconItems;
    FFixedColor: TColor;
    FGrayScale: Boolean;
    FAntiAliasColor: TColor;
    procedure SetSVGIconItems(const Value: TSVGIconItems);
    procedure SetFixedColor(const Value: TColor);
    procedure SetGrayScale(const Value: Boolean);
    procedure SetAntiAliasColor(const Value: TColor);

  protected
    {$IFDEF D10_3+}
    function GetCount: Integer; override;
    {$ENDIF}

    procedure ReadLeft(Reader: TReader);
    procedure ReadTop(Reader: TReader);
    procedure WriteLeft(Writer: TWriter);
    procedure WriteTop(Writer: TWriter);

    procedure DefineProperties(Filer: TFiler); override;

  public
    procedure SetColors(AFixedColor: TColor; AAntiAliasColor: TColor = clBtnFace);
    {$IFDEF D10_3+}
    function IsIndexAvailable(AIndex: Integer): Boolean; override;
    function GetIndexByName(const AName: String): Integer; override;
    function GetNameByIndex(AIndex: Integer): String; override;
    function GetBitmap(AIndex: Integer; AWidth, AHeight: Integer): TBitmap; override;
    procedure Draw(ACanvas: TCanvas; ARect: TRect; AIndex: Integer; AProportional: Boolean = False); override;
    {$ELSE}
    procedure Change;
    procedure Draw(ACanvas: TCanvas; ARect: TRect; AIndex: Integer; AProportional: Boolean = False);
    {$ENDIF}

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    function LoadFromFiles(const AFileNames: TStrings;
      const AAppend: Boolean = True): Integer;

    function Add(const ASVG: ISVG; const AIconName: string;
       const AGrayScale: Boolean = False;
       const AFixedColor: TColor = SVG_INHERIT_COLOR): Integer;
    procedure Delete(const Index: Integer); overload;
    procedure Delete(const ACategory: String; AStartIndex, AEndIndex: Integer); overload;
    procedure Remove(const Name: string);
    function IndexOf(const Name: string): Integer;
    procedure ClearIcons;

    function LoadFromResource(const hInstance : THandle; const ResourceName : string; const IconName : string) : integer;
    function LoadFromString(const Source : string; const IconName : string) : integer;

  published
    property SVGIconItems: TSVGIconItems read FSVGItems write SetSVGIconItems;
    property FixedColor: TColor read FFixedColor write SetFixedColor default SVG_INHERIT_COLOR;
    property AntiAliasColor: TColor read FAntiAliasColor write SetAntiAliasColor default clBtnFace;
    property GrayScale: Boolean read FGrayScale write SetGrayScale default False;
  end;

implementation

uses
  System.SysUtils
  , System.Messaging;

{ TSVGIconImageCollection }

function TSVGIconImageCollection.Add(const ASVG: ISVG; const AIconName: string;
  const AGrayScale: Boolean; const AFixedColor: TColor): Integer;
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

procedure TSVGIconImageCollection.Assign(Source: TPersistent);
begin
  if Source is TSVGIconImageCollection then
  begin
    FFixedColor := TSVGIconImageCollection(Source).FFixedColor;
    FGrayScale := TSVGIconImageCollection(Source).FGrayScale;
    FSVGItems.Assign(TSVGIconImageCollection(Source).SVGIconItems)
  end
  else if Source is TSVGIconItems then
    FSVGItems.Assign(Source)
  else
    inherited;
end;

procedure TSVGIconImageCollection.ClearIcons;
begin
  FSVGItems.BeginUpdate;
  try
    FSVGItems.Clear;
  finally
    FSVGItems.EndUpdate;
  end;
end;

constructor TSVGIconImageCollection.Create(AOwner: TComponent);
begin
  inherited;
  FSVGItems := TSVGIconItems.Create(Self);
  FFixedColor := SVG_INHERIT_COLOR;
  FAntiAliasColor := clBtnFace;
  FGrayScale := False;
end;

procedure TSVGIconImageCollection.DefineProperties(Filer: TFiler);
var
  Ancestor: TComponent;
  Info: Longint;
begin
  Info := 0;
  Ancestor := TComponent(Filer.Ancestor);
  if Ancestor <> nil then
    Info := Ancestor.DesignInfo;
  Filer.DefineProperty('Left', ReadLeft, WriteLeft, LongRec(DesignInfo).Lo <> LongRec(Info).Lo);
  Filer.DefineProperty('Top', ReadTop, WriteTop, LongRec(DesignInfo).Hi <> LongRec(Info).Hi);
end;

procedure TSVGIconImageCollection.Delete(const Index: Integer);
begin
  if (Index >= 0) and (Index < FSVGItems.Count) then
  begin
    FSVGItems.Delete(Index);
  end;
end;

procedure TSVGIconImageCollection.Delete(const ACategory: String; AStartIndex, AEndIndex: Integer);
var
  I: Integer;
begin
  if FSVGItems.Count = 0 then
    Exit;

  if (ACategory = '') and (AStartIndex <= 0) and ((AEndIndex < 0) or (AEndIndex >= FSVGItems.Count - 1)) then
  begin
    FSVGItems.Clear;
    Exit;
  end;

  if AStartIndex < 0 then
    AStartIndex := 0;
  if (AEndIndex < 0) or (AEndIndex > FSVGItems.Count - 1) then
    AEndIndex := FSVGItems.Count - 1;

  FSVGItems.BeginUpdate;
  try
    for I := AEndIndex downto AStartIndex do
      if (ACategory = '') or SameText(ACategory, FSVGItems[I].Category) then
      begin
          FSVGItems.Delete(I);
      end;
  finally
    FSVGItems.EndUpdate;
  end;
end;

destructor TSVGIconImageCollection.Destroy;
begin
  FSVGItems.Free;
  inherited;
end;

function TSVGIconImageCollection.IndexOf(const Name: string): Integer;
begin
  for Result := 0 to FSVGItems.Count - 1 do
    if FSVGItems[Result].IconName = Name then
      Exit;
  Result := -1;
end;

function TSVGIconImageCollection.LoadFromFiles(const AFileNames: TStrings;
  const AAppend: Boolean): Integer;
begin
  SVGIconItems.BeginUpdate;
  try
    Result := SVGIconItems.LoadFromFiles(AFileNames, AAppend);
  finally
    SVGIconItems.EndUpdate;
  end;
end;

function TSVGIconImageCollection.LoadFromResource(const hInstance: THandle; const ResourceName, IconName: string) : integer;
var
  ResStream: TResourceStream;
  Svg : ISVG;
begin
  resStream := TResourceStream.Create(hInstance, ResourceName, RT_RCDATA);
  try
    Svg := GlobalSVGFactory.NewSvg;
    Svg.LoadFromStream(ResStream);
    result := Add(Svg, IconName);
  finally
    ResStream.Free;
  end;
end;

function TSVGIconImageCollection.LoadFromString(const Source,  IconName: string): integer;
var
  Svg : ISVG;
begin
  Svg := GlobalSVGFactory.NewSvg;
  Svg.Source := Source;
  result := Add(Svg, IconName);
end;

procedure TSVGIconImageCollection.ReadLeft(Reader: TReader);
var
  FDesignInfo: LongInt;
begin
  FDesignInfo := DesignInfo;
  LongRec(FDesignInfo).Lo := Reader.ReadInteger;
  DesignInfo := FDesignInfo;
end;

procedure TSVGIconImageCollection.ReadTop(Reader: TReader);
var
  FDesignInfo: LongInt;
begin
  FDesignInfo := DesignInfo;
  LongRec(FDesignInfo).Hi := Reader.ReadInteger;
  DesignInfo := FDesignInfo;
end;

procedure TSVGIconImageCollection.Remove(const Name: string);
begin
  Delete(IndexOf(Name));
end;

procedure TSVGIconImageCollection.SetAntiAliasColor(const Value: TColor);
begin
  if FAntiAliasColor <> Value then
  begin
    FSVGItems.BeginUpdate;
    try
      FAntiAliasColor := Value;
    finally
      FSVGItems.EndUpdate;
    end;
  end;
end;

procedure TSVGIconImageCollection.SetColors(AFixedColor,
  AAntiAliasColor: TColor);
begin
  FSVGItems.BeginUpdate;
  try
    FixedColor := AFixedColor;
    AntiAliasColor := AAntiAliasColor;
  finally
    FSVGItems.EndUpdate;
  end;
end;

procedure TSVGIconImageCollection.SetFixedColor(const Value: TColor);
begin
  if FFixedColor <> Value then
  begin
    FSVGItems.BeginUpdate;
    try
      FFixedColor := Value;
      if FFixedColor <> SVG_INHERIT_COLOR then
        FGrayScale := False;
    finally
      FSVGItems.EndUpdate;
    end;
  end;
end;

procedure TSVGIconImageCollection.SetGrayScale(const Value: Boolean);
begin
  if FGrayScale <> Value then
  begin
    FSVGItems.BeginUpdate;
    try
      FGrayScale := Value;
      if FGrayScale then
        FixedColor := SVG_INHERIT_COLOR;
    finally
      FSVGItems.EndUpdate;
    end;
  end;
end;

procedure TSVGIconImageCollection.SetSVGIconItems(const Value: TSVGIconItems);
begin
  //shouldn't this use assign?
  //FSVGItems := Value;

  if FSVGItems <> Value then
  begin
    FSVGItems.Assign(Value);
  end;
end;

procedure TSVGIconImageCollection.WriteLeft(Writer: TWriter);
begin
  Writer.WriteInteger(LongRec(DesignInfo).Lo);
end;

procedure TSVGIconImageCollection.WriteTop(Writer: TWriter);
begin
  Writer.WriteInteger(LongRec(DesignInfo).Hi);
end;

{$IFDEF D10_3+}
function TSVGIconImageCollection.GetCount: Integer;
begin
  if Assigned(FSVGItems) then
    Result := FSVGItems.Count
  else
    Result := 0;
end;

function TSVGIconImageCollection.GetNameByIndex(AIndex: Integer): String;
begin
  if (AIndex >= 0) and (AIndex < Count) then
    Result := FSVGItems[AIndex].IconName
  else
    result := '';
end;

function TSVGIconImageCollection.GetIndexByName(const AName: String): Integer;
var
  I: Integer;
  S: String;
begin
  Result := -1;
  S := LowerCase(AName);
  for I := 0 to FSVGItems.Count - 1 do
    if LowerCase(FSVGItems[I].IconName) = S then
      Exit(I);
end;

function TSVGIconImageCollection.IsIndexAvailable(AIndex: Integer): Boolean;
begin
  Result := (Count > 0) and (AIndex >= 0) and (AIndex < Count);
end;

function TSVGIconImageCollection.GetBitmap(AIndex: Integer; AWidth, AHeight: Integer): TBitmap;
begin
  if (AIndex >= 0) and (AIndex < FSVGItems.Count ) then
    Result := FSVGItems[AIndex].GetBitmap(AWidth, AHeight, FFixedColor, 255,
      FGrayScale, FAntiAliasColor)
  else
    Result := nil;
end;

{$ELSE}
procedure TSVGIconImageCollection.Change;
begin
  FSVGItems.BeginUpdate;
  FSVGItems.EndUpdate;
end;
{$ENDIF}

procedure TSVGIconImageCollection.Draw(ACanvas: TCanvas; ARect: TRect; AIndex: Integer;
  AProportional: Boolean = False);
var
  LItem: TSVGIconItem;
  LSVG: ISVG;
begin
  LItem := FSVGItems.Items[AIndex];
  LSVG := LItem.SVG;
  if LItem.FixedColor <> SVG_INHERIT_COLOR then
    LSVG.FixedColor := LItem.FixedColor
  else
    LSVG.FixedColor := FFixedColor;
  if LItem.GrayScale or FGrayScale then
    LSVG.Grayscale := True
  else
    LSVG.Grayscale := False;
  LSVG.Opacity := 1;

  LSVG.PaintTo(ACanvas.Handle, TRectF.Create(ARect), AProportional);
end;

end.
