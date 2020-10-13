{******************************************************************************}
{                                                                              }
{       SVGTextPropertyEditorUnit: A property editor for SVGText               }
{       to simplify use of setting SVGText value                               }
{                                                                              }
{       Copyright (c) 2019-2020 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
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
unit SVGTextPropertyEditorUnit;

interface

{$INCLUDE SVGIconImageList.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ImgList,
  StdCtrls, Buttons, StdActns,
  ActnList, ExtCtrls, ComCtrls, ToolWin,
  Spin, SVGIconImage, Vcl.ExtDlgs;

type
  TSVGTextPropertyEditorForm = class(TForm)
    paBottom: TPanel;
    SVGTextMemo: TMemo;
    paButtons: TPanel;
    CancelButton: TButton;
    OKButton: TButton;
    HelpButton: TButton;
    RightSplitter: TSplitter;
    paImage: TPanel;
    paTitle: TPanel;
    LoadButton: TButton;
    SaveButton: TButton;
    OpenDialog: TOpenPictureDialog;
    SaveDialog: TSavePictureDialog;
    ImagePanel: TPanel;
    SVGIconImage: TSVGIconImage;
    BottomPanel: TPanel;
    ProportionalCheckBox: TCheckBox;
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure paImageResize(Sender: TObject);
    procedure SVGTextMemoChange(Sender: TObject);
    procedure LoadButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure ProportionalCheckBoxClick(Sender: TObject);
  private
    procedure UpdateImage;
    procedure UpdateGUI;
    function GetSVGText: string;
    procedure SetSVGText(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    property SVGText: string read GetSVGText write SetSVGText;
  end;

function EditSVGTextProperty(var ASVGText: string): boolean;

implementation

{$R *.dfm}

uses
  Themes
  , Math
  {$IFDEF DXE3+}
  , UITypes
  {$ENDIF}
  , ShellAPI
  , SVG;

var
  SavedBounds: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);


function EditSVGTextProperty(var ASVGText: string): boolean;
var
  LForm: TSVGTextPropertyEditorForm;
begin
  Result := False;
  LForm := TSVGTextPropertyEditorForm.Create(nil);
  try
    LForm.SVGText := ASVGText;
    if LForm.ShowModal = mrOk then
    begin
      Result := True;
      ASVGText := LForm.SVGText;
    end;
    SavedBounds := LForm.BoundsRect;
  finally
    LForm.Free;
  end;
end;

constructor TSVGTextPropertyEditorForm.Create(AOwner: TComponent);
begin
  inherited;
  ;
end;

procedure TSVGTextPropertyEditorForm.FormResize(Sender: TObject);
begin
  paImage.Width := ClientWidth div 4;
end;

procedure TSVGTextPropertyEditorForm.FormShow(Sender: TObject);
begin
  if SavedBounds.Right - SavedBounds.Left > 0 then
    SetBounds(SavedBounds.Left, SavedBounds.Top, SavedBounds.Width, SavedBounds.Height);

  if SVGTextMemo.CanFocus then
    SVGTextMemo.SetFocus;
  UpdateGUI;
end;

function TSVGTextPropertyEditorForm.GetSVGText: string;
begin
  Result := SVGTextMemo.Lines.Text;
end;

procedure TSVGTextPropertyEditorForm.HelpButtonClick(Sender: TObject);
begin
  ShellExecute(handle, 'open',
    PChar('https://github.com/EtheaDev/SVGIconImageList/wiki/SVGText-Editor'), nil, nil,
    SW_SHOWNORMAL)
end;

procedure TSVGTextPropertyEditorForm.LoadButtonClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    SVGIconImage.LoadFromFile(OpenDialog.FileName);
    SVGText := SVGIconImage.SVGText;
  end;
end;

procedure TSVGTextPropertyEditorForm.paImageResize(Sender: TObject);
begin
  paTitle.Caption := Format('w:%d-h:%d',
   [SVGIconImage.Width, SVGIconImage.Height]);
  SVGIconImage.Hint := paTitle.Caption;
end;

procedure TSVGTextPropertyEditorForm.ProportionalCheckBoxClick(Sender: TObject);
begin
  SVGIconImage.Proportional := ProportionalCheckBox.Checked;
end;

procedure TSVGTextPropertyEditorForm.SaveButtonClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    SVGIconImage.SVG.SaveToFile(SaveDialog.FileName);
end;

procedure TSVGTextPropertyEditorForm.SetSVGText(const Value: string);
begin
  SVGTextMemo.Lines.Text := Value;
  UpdateImage;
end;

procedure TSVGTextPropertyEditorForm.SVGTextMemoChange(Sender: TObject);
begin
  UpdateImage;
end;

procedure TSVGTextPropertyEditorForm.updateGUI;
begin
  SVGIconImage.Repaint;
end;

procedure TSVGTextPropertyEditorForm.UpdateImage;
begin
  SVGIconImage.SVGText := SVGTextMemo.Lines.Text;
  SVGIconImage.Repaint;
end;

end.
