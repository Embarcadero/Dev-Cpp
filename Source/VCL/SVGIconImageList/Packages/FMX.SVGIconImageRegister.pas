{******************************************************************************}
{                                                                              }
{       SVG Icon ImageList: An extended ImageList for Delphi                   }
{       to simplify use of Icons (resize, opacity and more...)                 }
{                                                                              }
{       Copyright (c) 2019-2020 (Ethea S.r.l.)                                 }
{       Contributors:                                                          }
{         Carlo Barazzetta                                                     }
{                                                                              }
{       https://github.com/EtheaDev/SVGIconImageList                           }
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
unit FMX.SVGIconImageRegister;

{$INCLUDE ..\Source\SVGIconImageList.inc}

interface

uses
  SysUtils
  , Classes
  , DesignIntf
  , DesignEditors;

type
  TSVGIconImageListCompEditorFMX = class (TComponentEditor)
  private
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
    procedure Edit; override;
  end;

procedure Register;

implementation

uses
  FMX.SVGIconImageList
  {$IFDEF D10_3+}
  , FmxAnimationEditors
  {$ENDIF}
  , FMX.SVGIconImage
  , Winapi.ShellApi
  , Winapi.Windows
  , FMX.SVGIconImageListEditorUnit
  ;

{ TSVGIconImageListCompEditorFMX }

procedure TSVGIconImageListCompEditorFMX.Edit;
begin
  inherited;
end;

procedure TSVGIconImageListCompEditorFMX.ExecuteVerb(Index: Integer);
begin
  inherited;
  if Index = 0 then
  begin
    if EditSVGIconImageList(Component as TSVGIconImageList) then
      Designer.Modified;
  end
  else
    ShellExecute(0, 'open',
      PChar('https://github.com/EtheaDev/SVGIconImageList/wiki/Home'), nil, nil,
      SW_SHOWNORMAL)
end;

function TSVGIconImageListCompEditorFMX.GetVerb(Index: Integer): string;
begin
  Result := '';
  case Index of
    0: Result := 'SVG I&con VirtualImageList Editor...';
    1: Result := Format('Ver. %s - (c) Ethea S.r.l. - show help...',[SVGIconImageListVersion]);
  end;
end;

function TSVGIconImageListCompEditorFMX.GetVerbCount: Integer;
begin
  Result := 2;
end;

procedure Register;
begin
  {$IFDEF D10_3+}
  RegisterPropertyEditor(TypeInfo(Single), TSVGIconBitmapItem, '', TFmxFloatProperty);
  RegisterPropertyEditor(TypeInfo(Single), TSVGIconSourceItem, '', TFmxFloatProperty);
  RegisterPropertyEditor(TypeInfo(Single), TSVGIconImageList, '', TFmxFloatProperty);
  {$ENDIF}

  RegisterComponents('Ethea',
  [TSVGIconImage,
   TSVGIconImageList
  ]);
  RegisterComponentEditor(TSVGIconImageList, TSVGIconImageListCompEditorFMX);
end;

end.
