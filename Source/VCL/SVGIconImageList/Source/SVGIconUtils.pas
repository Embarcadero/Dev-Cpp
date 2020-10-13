{******************************************************************************}
{                                                                              }
{       Icon SVG ImageList: An extended ImageList for Delphi/VCL               }
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
unit SVGIconUtils;

interface

{$INCLUDE SVGIconImageList.inc}

uses
  Classes
  , ImgList
  , SVGIconImageListBase
  , SVGIconImageList
  , Graphics
  , ComCtrls;

function UpdateSVGIconListView(const AListView: TListView;
  const ACategory: string = '';
  const AIncludeIndex: Boolean = True): Integer;
function UpdateSVGIconListViewCaptions(const AListView: TListView;
  const AShowCaption: Boolean = True): Integer;

implementation

uses
  SysUtils
  , Windows
  , Themes
  , SVGIconImageCOllection
  {$IFDEF D10_3}
  , VirtualImageList
  {$ENDIF}
  ;

function UpdateSVGIconListView(const AListView: TListView;
  const ACategory: string = '';
  const AIncludeIndex: Boolean = True): Integer;
var
  I: Integer;
  LItem: TSVGIconItem;
  LListItem: TListItem;
  LImageList: TCustomImageList;

  function GetItemCaption: string;
  begin
    if AIncludeIndex then
      Result := Format('%d.%s', [LItem.Index, LItem.Name])
    else
      Result := Format('%s', [LItem.Name]);
  end;

begin
  LImageList := AListView.LargeImages as TCustomImageList;
  AListView.Items.BeginUpdate;
  try
    AListView.Clear;
    Result := LImageList.Count;
    for I := 0 to Result -1 do
    begin
      if (LImageList is TSVGIconImageListBase) then
        LItem := TSVGIconImageListBase(LImageList).SVGIconItems[I]
      {$IFDEF D10_3}
      else if (LImageList is TVirtualImageList) and
        (TVirtualImageList(LImageList).ImageCollection is TSVGIconImageCollection) then
        LItem := TSVGIconImageCollection(TVirtualImageList(LImageList).ImageCollection).SVGIconItems[I]
      {$ENDIF}
      else
        Continue;
      if (ACategory = '') or
       (LowerCase(ACategory) = LowerCase(LItem.Category)) then
      begin
        LListItem := AListView.Items.Add;
        LListItem.Caption := GetItemCaption;
        LListItem.ImageIndex := I;
      end;
    end;
  finally
    AListView.Items.EndUpdate;
  end;
end;

function UpdateSVGIconListViewCaptions(const AListView: TListView;
  const AShowCaption: Boolean = True): Integer;
var
  I: Integer;
  LItem: TSVGIconItem;
  {$IFDEF D10_3}
  LVirtualItem: TVirtualImageListItem;
  {$ENDIF}
  LListItem: TListItem;
  LImageList: TCustomImageList;
begin
  LImageList := AListView.LargeImages as TCustomImageList;
  AListView.Items.BeginUpdate;
  try
    Result := LImageList.Count;
    for I := 0 to Result -1 do
    begin
      if (LImageList is TSVGIconImageListBase) then
      begin
        LItem := TSVGIconImageListBase(LImageList).SVGIconItems[I];
        LListItem := AListView.Items[I];
        if AShowCaption then
        begin
          LListItem.Caption := Format('%d.%s',
            [LItem.Index, LItem.IconName]);
        end
        else
          LListItem.Caption := '';
      end;
      {$IFDEF D10_3}
      if (LImageList is TVirtualImageList) then
      begin
        LVirtualItem := TVirtualImageList(LImageList).Images.Items[I];
        LListItem := AListView.Items[I];
        if AShowCaption then
        begin
          LListItem.Caption := Format('%d.%s',
            [LVirtualItem.Index, LVirtualItem.Name]);
        end
        else
          LListItem.Caption := '';
      end;
      {$ENDIF}
    end;
  finally
    AListView.Items.EndUpdate;
  end;
end;


end.
