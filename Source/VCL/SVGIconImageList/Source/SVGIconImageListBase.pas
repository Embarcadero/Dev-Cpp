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
unit SVGIconImageListBase;

interface

{$INCLUDE SVGIconImageList.inc}

uses
  System.Classes,
  System.Messaging,
  WinApi.Windows,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ImgList,
  System.UITypes,   // after ImgList to avoid deprecation warnings
  SVGIconItems,
  SvgInterfaces;

const
  SVGIconImageListVersion = '2.2.0';
  DEFAULT_SIZE = 16;

type
  TSVGIconImageListBase = class(TDragImageList)
  protected
    {$IFDEF HiDPISupport}
    {$IFNDEF D10_4+}
    FScaled: Boolean;
    {$ENDIF}
    FDPIChangedMessageID: Integer;
    {$ENDIF}
    FSVGItemsUpdateMessageID: Integer;
    FOpacity: Byte;
    FFixedColor: TColor;
    FAntiAliasColor: TColor;
    FGrayScale: Boolean;
    FDisabledGrayScale: Boolean;
    FDisabledOpacity: Byte;
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    procedure SetOpacity(const Value: Byte);
    function GetSize: Integer;

    function GetSVGIconItems: TSVGIconItems; virtual; abstract;
    procedure SetSVGIconItems(const Value: TSVGIconItems); virtual;

    function GetImages(Index: Integer): ISVG; virtual;
    function GetNames(Index: Integer): string; virtual;

    procedure SetImages(Index: Integer; const Value: ISVG); virtual;
    procedure SetNames(Index: Integer; const Value: string); virtual;
    procedure SetSize(const Value: Integer);
    procedure SetFixedColor(const Value: TColor);
    procedure SetAntiAliasColor(const Value: TColor);
    procedure SetGrayScale(const Value: Boolean);
    procedure SetDisabledGrayScale(const Value: Boolean);
    procedure SetDisabledOpacity(const Value: Byte);
    function StoreWidth: Boolean;
    function StoreHeight: Boolean;
    function StoreSize: Boolean;

    procedure ReadLeft(Reader: TReader);
    procedure ReadTop(Reader: TReader);
    procedure WriteLeft(Writer: TWriter);
    procedure WriteTop(Writer: TWriter);

    function IndexOf(const Name: string): Integer;virtual;

    procedure PaintTo(const ACanvas: TCanvas; const AIndex: Integer;
      const X, Y, AWidth, AHeight: Single; AEnabled: Boolean = True); overload; virtual; abstract;
    procedure PaintTo(const ACanvas: TCanvas; const AName: string;
      const X, Y, AWidth, AHeight: Single; AEnabled: Boolean = True); overload;

    procedure DefineProperties(Filer: TFiler); override;
    procedure DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean = True); override;
    procedure Loaded; override;
    function GetCount: Integer; {$IF CompilerVersion > 28}override;{$ELSE}virtual;{$ENDIF}

    procedure RecreateBitmaps; virtual; abstract;
    {$IF CompilerVersion < 29}
    procedure Change; override;
    {$ELSE}
    procedure DoChange; override;
    {$IFEND}

    procedure ClearIcons; virtual;

  {$IFDEF HiDPISupport}
    procedure DPIChangedMessageHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);
  {$ENDIF}
    procedure SVGItemsUpdateMessageHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);

    procedure AssignTo(Dest: TPersistent); override;
    procedure DoAssign(const Source: TPersistent); virtual;
  public
    constructor Create(AOwner : TComponent);override;
    destructor Destroy;override;
    procedure Assign(Source: TPersistent); override;
    function LoadFromFiles(const AFileNames: TStrings;
      const AAppend: Boolean = True): Integer;
    procedure DPIChanged(Sender: TObject; const OldDPI, NewDPI: Integer); virtual;
    {$IFDEF D10_4+}
    function IsImageNameAvailable: Boolean; override;
    function GetIndexByName(const AName: TImageName): TImageIndex; override;
    function GetNameByIndex(AIndex: TImageIndex): TImageName; override;
    {$ENDIF}

    property SVGIconItems: TSVGIconItems read GetSVGIconItems write SetSVGIconItems;
    property Count: Integer read GetCount;
    property Opacity: Byte read FOpacity write SetOpacity default 255;
    property Width: Integer read GetWidth write SetWidth stored StoreWidth default DEFAULT_SIZE;
    property Height: Integer read GetHeight write SetHeight stored StoreHeight default DEFAULT_SIZE;
    property Size: Integer read GetSize write SetSize stored StoreSize default DEFAULT_SIZE;
    property FixedColor: TColor read FFixedColor write SetFixedColor default SVG_INHERIT_COLOR;
    property AntiAliasColor: TColor read FAntiAliasColor write SetAntiAliasColor default clBtnFace;
    property GrayScale: Boolean read FGrayScale write SetGrayScale default False;
    property DisabledGrayScale: Boolean read FDisabledGrayScale write SetDisabledGrayScale default True;
    property DisabledOpacity: Byte read FDisabledOpacity write SetDisabledOpacity default 125;

    {$IFDEF HiDPISupport}
    {$IFNDEF D10_4+}
    property Scaled: Boolean read FScaled write FScaled default True;
    {$ENDIF}
    {$ENDIF}
    property Images[Index: Integer]: ISVG read GetImages write SetImages;
    property Names[Index: Integer]: string read GetNames write SetNames;
  published
    property ColorDepth default cd32Bit;
  end;

implementation

uses
  System.Math,
  System.SysUtils,
  Vcl.ComCtrls,
  Vcl.Forms;

{ TSVGIconImageListBase }

procedure TSVGIconImageListBase.Assign(Source: TPersistent);
begin
  if Source is TSVGIconImageListBase then
  begin
    BeginUpdate;
    try
      Width := TSVGIconImageListBase(Source).Width;
      Height := TSVGIconImageListBase(Source).Height;
      FOpacity := TSVGIconImageListBase(Source).FOpacity;
      FFixedColor := TSVGIconImageListBase(Source).FFixedColor;
      FAntiAliasColor := TSVGIconImageListBase(Source).FAntiAliasColor;
      FGrayScale := TSVGIconImageListBase(Source).FGrayScale;
      DoAssign(Source);
    finally
     EndUpdate;
    end;
  end else if Source is TSVGIconItems then begin
    if Assigned(SVGIconItems) then
      SVGIconItems.Assign(Source);
  end else
    inherited;
end;

procedure TSVGIconImageListBase.AssignTo(Dest: TPersistent);
begin
  if (Dest is TSVGIconItems) then begin
    if Assigned(SVGIconItems) then
      Dest.Assign(SVGIconItems)
  end else
    inherited;
end;

procedure TSVGIconImageListBase.ClearIcons;
begin
  //do nothing
end;

constructor TSVGIconImageListBase.Create(AOwner: TComponent);
begin
  inherited;
  ColorDepth := cd32Bit;
  Width := DEFAULT_SIZE;
  Height := DEFAULT_SIZE;
  FOpacity := 255;
  FFixedColor := SVG_INHERIT_COLOR;
  FAntiAliasColor := clBtnFace;
  FGrayScale := False;
  {$IFDEF HiDPISupport}
  FScaled := True;
  FDPIChangedMessageID := TMessageManager.DefaultManager.SubscribeToMessage(TChangeScaleMessage, DPIChangedMessageHandler);
  {$ENDIF}
  FSVGItemsUpdateMessageID := TMessageManager.DefaultManager.SubscribeToMessage(TSVGItemsUpdateMessage, SVGItemsUpdateMessageHandler);
  FDisabledGrayScale := True;
  FDisabledOpacity := 125;
end;

procedure TSVGIconImageListBase.DefineProperties(Filer: TFiler);
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

destructor TSVGIconImageListBase.Destroy;
begin
  {$IFDEF HiDPISupport}
  TMessageManager.DefaultManager.Unsubscribe(TChangeScaleMessage, FDPIChangedMessageID);
  {$ENDIF}
  TMessageManager.DefaultManager.Unsubscribe(TSVGItemsUpdateMessage, FSVGItemsUpdateMessageID);
  inherited;
end;

procedure TSVGIconImageListBase.DoAssign(const Source: TPersistent);
begin
  //do nothing.. TSVGIconImageList will override;
end;

{$IF CompilerVersion < 29}
procedure TSVGIconImageListBase.Change;
{$ELSE}
procedure TSVGIconImageListBase.DoChange;
{$IFEND}
begin
  RecreateBitmaps;
  inherited;
end;

procedure TSVGIconImageListBase.DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean);
begin
  PaintTo(Canvas, Index, X, Y, Width, Height, Enabled);
end;

procedure TSVGIconImageListBase.DPIChanged(Sender: TObject; const OldDPI, NewDPI: Integer);
var
  LSizeScaled: Integer;
  LWidthScaled, LHeightScaled: Integer;
begin
  if Width = Height then
  begin
    LSizeScaled := MulDiv(Size, NewDPI, OldDPI);
    {$IFDEF D10_3+}
    FScaling := True;
    try
      SetSize(LSizeScaled);
    finally
      FScaling := False;
    end;
    {$ELSE}
      SetSize(LSizeScaled);
    {$ENDIF}
  end
  else
  begin
    LWidthScaled := MulDiv(Width, NewDPI, OldDPI);
    LHeightScaled := MulDiv(Height, NewDPI, OldDPI);
    {$IFDEF D10_3+}
    FScaling := True;
    try
      if (Width <> LWidthScaled) or (Height <> LHeightScaled) then
      begin
        Width := LWidthScaled;
        Height := LHeightScaled;
      end;
    finally
      FScaling := False;
    end;
    {$ELSE}
       if (Width <> LWidthScaled) or (Height <> LHeightScaled) then
       begin
         Width := LWidthScaled;
         Height := LHeightScaled;
       end;
    {$ENDIF}
  end;
end;

function TSVGIconImageListBase.GetCount: Integer;
Var
  Items: TSVGIconItems;
begin
  Items := SVGIconItems;
  if Assigned(Items) then
    Result := Items.Count
  else
    Result := 0;
end;

function TSVGIconImageListBase.GetHeight: Integer;
begin
  Result := inherited Height;
end;

function TSVGIconImageListBase.GetImages(Index: Integer): ISVG;
Var
  Items: TSVGIconItems;
begin
  Items := SVGIconItems;
  if Assigned(Items) and (Index >= 0) and (Index < Items.Count) then
    Result := Items[Index].SVG
  else
    Result := nil;
end;

function TSVGIconImageListBase.GetNames(Index: Integer): string;
Var
  Items: TSVGIconItems;
begin
  Items := SVGIconItems;
  if Assigned(Items) and (Index >= 0) and (Index < Items.Count) then
    Result := Items[Index].IconName
  else
    Result := '';
end;

function TSVGIconImageListBase.GetSize: Integer;
begin
  Result := Max(Width, Height);
end;

function TSVGIconImageListBase.GetWidth: Integer;
begin
  Result := inherited Width;
end;

function TSVGIconImageListBase.IndexOf(const Name: string): Integer;
Var
  Items: TSVGIconItems;
  Item: TSVGIconItem;
begin
  Items := SVGIconItems;
  if not Assigned(Items) then Exit(-1);

  Item := Items.GetIconByName(Name);
  if Assigned(Item) then
    Result := Item.Index
  else
    Result := -1;
end;

{$IFDEF D10_4+}
function TSVGIconImageListBase.IsImageNameAvailable: Boolean;
begin
  Result := true;
end;

function TSVGIconImageListBase.GetIndexByName(const AName: TImageName): TImageIndex;
begin
  Result := IndexOf(AName);
end;

function TSVGIconImageListBase.GetNameByIndex(AIndex: TImageIndex): TImageName;
begin
  Result := GetNames(AIndex);
end;
{$ENDIF}

procedure TSVGIconImageListBase.Loaded;
begin
  inherited;
  Change;
end;

function TSVGIconImageListBase.LoadFromFiles(const AFileNames: TStrings;
  const AAppend: Boolean = True): Integer;
begin
  BeginUpdate;
  try
    if Assigned(SVGIconItems) then
      Result := SVGIconItems.LoadFromFiles(AFileNames, AAppend)
    else
      Result := 0;
  finally
    EndUpdate;
  end;
end;

procedure TSVGIconImageListBase.PaintTo(const ACanvas: TCanvas; const AName: string; const X, Y, AWidth, AHeight: Single; AEnabled: Boolean);
var
  LIndex: Integer;
begin
  LIndex := IndexOf(AName);
  PaintTo(ACanvas, LIndex, X, Y, AWidth, AHeight, AEnabled);
end;

procedure TSVGIconImageListBase.ReadLeft(Reader: TReader);
var
  FDesignInfo: LongInt;
begin
  FDesignInfo := DesignInfo;
  LongRec(FDesignInfo).Lo := Reader.ReadInteger;
  DesignInfo := FDesignInfo;
end;

procedure TSVGIconImageListBase.ReadTop(Reader: TReader);
var
  FDesignInfo: LongInt;
begin
  FDesignInfo := DesignInfo;
  LongRec(FDesignInfo).Hi := Reader.ReadInteger;
  DesignInfo := FDesignInfo;
end;


procedure TSVGIconImageListBase.SetDisabledGrayScale(const Value: Boolean);
begin
  if FDisabledGrayScale <> Value then
  begin
    FDisabledGrayScale := Value;
    Change;
  end;
end;

procedure TSVGIconImageListBase.SetDisabledOpacity(const Value: Byte);
begin
  if FDisabledOpacity <> Value then
  begin
    FDisabledOpacity := Value;
    Change;
  end;
end;

procedure TSVGIconImageListBase.SetFixedColor(const Value: TColor);
begin
  if FFixedColor <> Value then
  begin
    FFixedColor := Value;
    if FFixedColor <> SVG_INHERIT_COLOR then
      FGrayScale := False;
    Change;
  end;
end;

procedure TSVGIconImageListBase.SetAntiAliasColor(const Value: TColor);
begin
  if FAntiAliasColor <> Value then
  begin
    FAntiAliasColor := Value;
  end;
end;

procedure TSVGIconImageListBase.SetGrayScale(const Value: Boolean);
begin
  if FGrayScale <> Value then
  begin
    FGrayScale := Value;
    if FGrayScale then
      FFixedColor := SVG_INHERIT_COLOR;
    Change;
  end;
end;

procedure TSVGIconImageListBase.SetHeight(const Value: Integer);
begin
  if Height <> Value then
  begin
    inherited Height := Value;
    Change;
  end;
end;

procedure TSVGIconImageListBase.SetImages(Index: Integer; const Value: ISVG);
Var
  Items: TSVGIconItems;
begin
  Items := SVGIconItems;
  if Assigned(Items) and (Index >= 0) and (Index < Items.Count) then
  begin
    if Items[Index].SVG <> Value then
      Items[Index].SVG := Value;
  end;
end;

procedure TSVGIconImageListBase.SetNames(Index: Integer; const Value: string);
Var
  Items: TSVGIconItems;
begin
  Items := SVGIconItems;
  if Assigned(Items) and (Index >= 0) and (Index < Items.Count) then
    Items[Index].IconName := Value;
end;

procedure TSVGIconImageListBase.SetOpacity(const Value: Byte);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    Change;
  end;
end;

procedure TSVGIconImageListBase.SetSize(const Value: Integer);
begin
  if (Height <> Value) or (Width <> Value) then
  begin
    BeginUpdate;
    try
      Width := Value;
      Height := Value;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TSVGIconImageListBase.SetSVGIconItems(const Value: TSVGIconItems);
begin
  if Assigned(SvgIconItems) then
    SvgIconItems.Assign(Value);
end;

procedure TSVGIconImageListBase.SetWidth(const Value: Integer);
begin
  if Width <> Value then
  begin
    inherited Width := Value;
    Change;
  end;
end;

function TSVGIconImageListBase.StoreHeight: Boolean;
begin
  Result := (Width <> Height) and (Height <> DEFAULT_SIZE);
end;

function TSVGIconImageListBase.StoreSize: Boolean;
begin
  Result := (Width = Height) and (Width <> DEFAULT_SIZE);
end;

function TSVGIconImageListBase.StoreWidth: Boolean;
begin
  Result := (Width <> Height) and (Width <> DEFAULT_SIZE);
end;

procedure TSVGIconImageListBase.WriteLeft(Writer: TWriter);
begin
  Writer.WriteInteger(LongRec(DesignInfo).Lo);
end;

procedure TSVGIconImageListBase.WriteTop(Writer: TWriter);
begin
  Writer.WriteInteger(LongRec(DesignInfo).Hi);
end;

{$IFDEF HiDPISupport}
procedure TSVGIconImageListBase.DPIChangedMessageHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);
var
  LWidthScaled, LHeightScaled: Integer;
begin
  if FScaled and (TChangeScaleMessage(Msg).Sender = Owner) then
  begin
    LWidthScaled := MulDiv(Width, TChangeScaleMessage(Msg).M, TChangeScaleMessage(Msg).D);
    LHeightScaled := MulDiv(Height, TChangeScaleMessage(Msg).M, TChangeScaleMessage(Msg).D);
    FScaling := True;
    try
      if (Width <> LWidthScaled) or (Height <> LHeightScaled) then
      begin
        BeginUpdate;
        try
          Width := LWidthScaled;
          Height := LHeightScaled;
        finally
          EndUpdate;
        end;
        Change;
      end;
    finally
      FScaling := False;
    end;
  end;
end;
{$ENDIF}

procedure TSVGIconImageListBase.SVGItemsUpdateMessageHandler(const Sender: TObject;
  const Msg: System.Messaging.TMessage);
var
  items : TSVGIconItems;
begin
  items := SVGIconItems;
  if TObject(items) = Sender then
    Change;
end;

end.
