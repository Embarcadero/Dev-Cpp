{******************************************************************************}
{                                                                              }
{       SVGIconImage Registration for Components and Editors                   }
{                                                                              }
{       Copyright (c) 2019-2020 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{       Contributors: Vincent Parrett, Kiriakos Vlahos                         }
{                                                                              }
{       https://github.com/EtheaDev/SVGIconsImageList                          }
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
unit SVGIconImageRegister;

interface

uses
  Classes
  , DesignIntf
  , DesignEditors
  , VCLEditors
  , Vcl.ImgList
  , Vcl.Graphics
  , System.Types;

type
  TSVGIconImageListCompEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TSVGIconVirtualImageListCompEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;


  TSVGIconImageCollectionCompEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;


  TSVGIconImageListProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

  TSVGIconCollectionListProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

  TSVGIconImageCompEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TSVGTextProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

  TSVGImageIndexPropertyEditor = class(TIntegerProperty, ICustomPropertyListDrawing)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetImageListAt(Index: Integer): TCustomImageList; virtual;
    // ICustomPropertyListDrawing
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas;
      var AHeight: Integer);
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
      var AWidth: Integer);
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean);
  end;

procedure Register;

implementation

uses
  SysUtils
  , System.UITypes
  , Winapi.ShellApi
  , Winapi.Windows
  , SVGIconImage
  , SVGIconImageListBase
  , SVGIconImageList
  , SVGIconItems
  , SVGIconVirtualImageList
  , SVGIconImageCollection
  , SVGIconImageListEditorUnit
  , SVGTextPropertyEditorUnit;

{ TSVGIconImageListCompEditor }
procedure TSVGIconImageListCompEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  if Index = 0 then
  begin
    if EditSVGIconImageList(Component as TSVGIconImageList) then
      Designer.Modified;
  end
  else if Index = 1 then
  begin
    ShellExecute(0, 'open',
      PChar('https://github.com/EtheaDev/SVGIconImageList/wiki/Home'), nil, nil,
      SW_SHOWNORMAL)
  end;
end;

function TSVGIconImageListCompEditor.GetVerb(Index: Integer): string;
begin
  Result := '';
  case Index of
    0: Result := 'SVG I&con ImageList Editor...';
    1: Result := Format('Ver. %s - (c) Ethea S.r.l. - show help...',[SVGIconImageListVersion]);
  end;
end;

function TSVGIconImageListCompEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

{ TSVGIconImageListProperty }

procedure TSVGIconImageListProperty.Edit;
var
  SVGImageList: TSVGIconImageList;
begin
  SVGImageList := TSVGIconImageList(GetComponent(0));
  if EditSVGIconImageList(SVGImageList) then
    Modified;
end;

function TSVGIconImageListProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

function TSVGIconImageListProperty.GetValue: string;
begin
  Result := 'SVGImages';
end;

{ TSVGIconCollectionListProperty }

procedure TSVGIconCollectionListProperty.Edit;
var
  SVGImageCollection: TSVGIconImageCollection;
begin
  SVGImageCollection := TSVGIconImageCollection(GetComponent(0));
  if EditSVGIconImageCollection(SVGImageCollection) then
    Modified;
end;

function TSVGIconCollectionListProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

function TSVGIconCollectionListProperty.GetValue: string;
begin
  Result := 'SVGImageCollection';
end;

{ TSVGTextProperty }

procedure TSVGTextProperty.Edit;
var
  LSVGText: string;
  LComponent: TPersistent;
begin
  LComponent := GetComponent(0);
  if LComponent is TSVGIconItem then
    LSVGText := TSVGIconItem(LComponent).SVGText
  else if LComponent is TSVGIconImage then
    LSVGText := TSVGIconImage(LComponent).SVGText
  else
    Exit;
  if EditSVGTextProperty(LSVGText) then
  begin
    if LComponent is TSVGIconItem then
      TSVGIconItem(LComponent).SVGText := LSVGText
    else if LComponent is TSVGIconImage then
      TSVGIconImage(LComponent).SVGText := LSVGText;
    Modified;
  end;
  inherited;
end;

function TSVGTextProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TSVGTextProperty.GetValue: string;
begin
  Result := 'Click to edit SVG Text';
end;

procedure Register;
begin
  RegisterComponents('Ethea',
    [TSVGIconImage,
     TSVGIconImageList,
     TSVGIconVirtualImageList,
     TSVGIconImageCollection]);

  RegisterComponentEditor(TSVGIconImageList, TSVGIconImageListCompEditor);
  RegisterComponentEditor(TSVGIconVirtualImageList, TSVGIconVirtualImageListCompEditor);
  RegisterComponentEditor(TSVGIconImageCollection, TSVGIconImageCollectionCompEditor);
  RegisterComponentEditor(TSVGIconImage, TSVGIconImageCompEditor);
  RegisterPropertyEditor(TypeInfo(TSVGIconItems), TSVGIconImageList, 'SVGIconItems', TSVGIconImageListProperty);
  RegisterPropertyEditor(TypeInfo(TSVGIconItems), TSVGIconImageCollection, 'SVGIconItems', TSVGIconCollectionListProperty);
  RegisterPropertyEditor(TypeInfo(string), TSVGIconItem, 'SVGText', TSVGTextProperty);
  RegisterPropertyEditor(TypeInfo(string), TSVGIconImage, 'SVGText', TSVGTextProperty);
  RegisterPropertyEditor(TypeInfo(System.UITypes.TImageIndex), TSVGIconImage, 'ImageIndex',
    TSVGImageIndexPropertyEditor);
end;

{ TSVGIconImageCollectionCompEditor }

procedure TSVGIconImageCollectionCompEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  if Index = 0 then
  begin
    if EditSVGIconImageCollection(Component as TSVGIconImageCollection) then
      Designer.Modified;
  end
  else if Index = 1 then
  begin
    ShellExecute(0, 'open',
      PChar('https://github.com/EtheaDev/SVGIconImageList/wiki/Home'), nil, nil,
      SW_SHOWNORMAL)
  end;

end;

function TSVGIconImageCollectionCompEditor.GetVerb(Index: Integer): string;
begin
  Result := '';
  case Index of
    0: Result := 'SVG I&con ImageCollection Editor...';
    1: Result := Format('Ver. %s - (c) Ethea S.r.l. - show help...',[SVGIconImageListVersion]);
  end;
end;

function TSVGIconImageCollectionCompEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

{ TSVGIconVirtualImageListCompEditor }

procedure TSVGIconVirtualImageListCompEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  if Index = 0 then
  begin
    if EditSVGIconVirtualImageList(Component as TSVGIconVirtualImageList) then
      Designer.Modified;
  end
  else if Index = 1 then
  begin
    ShellExecute(0, 'open',
      PChar('https://github.com/EtheaDev/SVGIconImageList/wiki/Home'), nil, nil,
      SW_SHOWNORMAL)
  end;
end;

function TSVGIconVirtualImageListCompEditor.GetVerb(Index: Integer): string;
begin
  Result := '';
  case Index of
    0: Result := 'SVG I&con VirtualImageList Editor...';
    1: Result := Format('Ver. %s - (c) Ethea S.r.l. - show help...',[SVGIconImageListVersion]);
  end;
end;

function TSVGIconVirtualImageListCompEditor.GetVerbCount: Integer;
begin
  result := 2;
end;

{ TSVGIconImageCompEditor }

procedure TSVGIconImageCompEditor.ExecuteVerb(Index: Integer);
var
  LSVGText: string;
begin
  inherited;
  if Index = 0 then
  begin
    LSVGText := (Component as TSVGIconImage).SVGText;
    if EditSVGTextProperty(LSVGText) then
    begin
      TSVGIconImage(Component).ImageList := nil;
      TSVGIconImage(Component).SVGText := LSVGText;
      Designer.Modified;
    end;
  end
  else if Index = 1 then
  begin
    ShellExecute(0, 'open',
      PChar('https://github.com/EtheaDev/SVGIconImageList/wiki/Home'), nil, nil,
      SW_SHOWNORMAL)
  end;
end;

function TSVGIconImageCompEditor.GetVerb(Index: Integer): string;
begin
  Result := '';
  case Index of
    0: Result := 'SVG I&conImage Editor...';
    1: Result := Format('Ver. %s - (c) Ethea S.r.l. - show help...',[SVGIconImageListVersion]);
  end;
end;

function TSVGIconImageCompEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

{ TSVGImageIndexPropertyEditor }

function TSVGImageIndexPropertyEditor.GetImageListAt(Index: Integer): TCustomImageList;
var
  LComponent: TPersistent;
begin
  Result := nil;
  LComponent := GetComponent(Index);
  if LComponent is TSVGIconImage then
    Result := TSVGIconImage(LComponent).ImageList;
end;

function TSVGImageIndexPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paRevertable];
end;

procedure TSVGImageIndexPropertyEditor.GetValues(Proc: TGetStrProc);
var
  ImgList: TCustomImageList;
  I: Integer;
begin
  ImgList := GetImageListAt(0);
  if Assigned(ImgList) then
    for I := 0 to ImgList.Count -1 do
      Proc(IntToStr(I));
end;

procedure TSVGImageIndexPropertyEditor.ListDrawValue(const Value: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
  ImgList: TCustomImageList;
  X: Integer;
begin
  ImgList := GetImageListAt(0);
  ACanvas.FillRect(ARect);
  X := ARect.Left + 2;
  if Assigned(ImgList) then
  begin
    ImgList.Draw(ACanvas, X, ARect.Top + 2, StrToInt(Value));
    Inc(X, ImgList.Width);
  end;
  ACanvas.TextOut(X + 3, ARect.Top + 1, Value);
end;

procedure TSVGImageIndexPropertyEditor.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
var
  ImgList: TCustomImageList;
begin
  ImgList := GetImageListAt(0);
  AHeight := ACanvas.TextHeight(Value) + 2;
  if Assigned(ImgList) and (ImgList.Height + 4 > AHeight) then
    AHeight := ImgList.Height + 4;
end;

procedure TSVGImageIndexPropertyEditor.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
var
  ImgList: TCustomImageList;
begin
  ImgList := GetImageListAt(0);
  AWidth := ACanvas.TextWidth(Value) + 4;
  if Assigned(ImgList) then
    Inc(AWidth, ImgList.Width);
end;

end.
