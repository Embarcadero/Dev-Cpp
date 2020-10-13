{******************************************************************************}
{                                                                              }
{       SVG Icon ImageList: An extended ImageList for Delphi/VCL               }
{       to simplify use of Icons (resize, colors and more...)                  }
{                                                                              }
{       Copyright (c) 2019-2020 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{       Contributors:                                                          }
{         Nicola Tambascia, Vincent Parrett                                    }
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
unit UMain;

interface

{$INCLUDE SVGIconImageList.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ImgList,
  StdCtrls, Buttons, StdActns,
  ActnList, ExtCtrls, ComCtrls, ToolWin,
  Spin, SVGIconImageList, SVGIconImage, Vcl.ExtDlgs,
  System.Actions,
  System.ImageList, //if you are compiling with older version than XE7 remove this line
  SVGIconImageListBase, SVGIconImageCollection, SVGIconVirtualImageList,
  {$IFDEF D10_3+}Vcl.VirtualImageList,{$ENDIF}
  UDataModule;

type
  TMainForm = class(TForm)
    ActionList: TActionList;
    ChangeIconAction: TAction;
    Panel1: TPanel;
    SelectThemeRadioGroup: TRadioGroup;
    TopToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    paButtons: TPanel;
    DeleteButton: TButton;
    ChangeIconButton: TButton;
    ClientPanel: TPanel;
    TreeView: TTreeView;
    ImageView: TListView;
    ImageListLabel: TLabel;
    LoadGroupBox: TGroupBox;
    BuildFromFilesButton: TButton;
    DeleteIconAction: TAction;
    SliderPanel: TPanel;
    TrackBar: TTrackBar;
    IconSizeLabel: TLabel;
    ButtonsPanel: TPanel;
    ClearButton: TButton;
    ShowImageEditorButton: TButton;
    ColorDialog: TColorDialog;
    DisabledAction: TAction;
    OpenDialog: TOpenPictureDialog;
    SVGIconImage: TSVGIconImage;
    Splitter: TSplitter;
    ColorGroupBox: TGroupBox;
    FixedColorComboBox: TColorBox;
    GrayScaleCheckBox: TCheckBox;
    SVGIconVirtualImageList: TSVGIconVirtualImageList;
    NewFormButton: TButton;
    NewFormAction: TAction;
    tmrTrackbar: TTimer;
    procedure ChangeIconActionExecute(Sender: TObject);
    procedure SelectThemeRadioGroupClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ShowImageEditorButtonClick(Sender: TObject);
    procedure TrackBarChange(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure DeleteIconActionExecute(Sender: TObject);
    procedure ChangeColorActionExecute(Sender: TObject);
    procedure ImageViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure BuildFromFilesButtonClick(Sender: TObject);
    procedure SVGIconImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure paButtonsResize(Sender: TObject);
    procedure GrayScaleCheckBoxClick(Sender: TObject);
    procedure FixedColorComboBoxSelect(Sender: TObject);
    procedure NewFormActionExecute(Sender: TObject);
    procedure tmrTrackbarTimer(Sender: TObject);
  private
    {$IFDEF D10_3+}
    VirtualImageList: TVirtualImageList;
    {$ENDIF}
    FUpdating: Boolean;
    FSVGIconImageListHot: TSVGIconImageList;
    FSVGIconImageListDisabled: TSVGIconImageList;
    {$IFDEF HiDPISupport}
    procedure FormAfterMonitorDpiChanged(Sender: TObject; OldDPI, NewDPI: Integer);
    {$ENDIF}
    procedure UpdateButtons;
    procedure UpdateGUI;
    procedure UpdateListView;
    procedure UpdateTreeView;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  Themes
  {$IFDEF DXE3+}
  , UITypes
  {$ENDIF}
  , SVGColor
  , SVGIconUtils
  , SVGIconImageListEditorUnit;

procedure TMainForm.UpdateButtons;
begin
  DeleteButton.Action := DeleteIconAction;
  ChangeIconButton.Action := ChangeIconAction;
  NewFormButton.Action := NewFormAction;
end;

procedure TMainForm.UpdateListView;
var
  LItemsCount: Integer;
begin
  LItemsCount := UpdateSVGIconListView(ImageView);
  ImageListLabel.Caption := Format('SVG Image List Preview: %d icons',[LItemsCount]);
end;

procedure TMainForm.BuildFromFilesButtonClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    Screen.Cursor := crHourGlass;
    try
      SVGIconVirtualImageList.LoadFromFiles(OpenDialog.Files);
    finally
      Screen.Cursor := crDefault;
    end;
  end;
  UpdateGUI;
end;

procedure TMainForm.ChangeColorActionExecute(Sender: TObject);
begin
  ColorDialog.Color := SVGIconVirtualImageList.FixedColor;
  if ColorDialog.Execute then
    SVGIconVirtualImageList.FixedColor := ColorDialog.Color;
  UpdateGUI;
end;

procedure TMainForm.ChangeIconActionExecute(Sender: TObject);
var
  LAction: TAction;
begin
  //Detach Action
  ChangeIconButton.Action := nil;
  LAction := Sender as TAction;
  //Change icon of the connected action
  LAction.ImageIndex := LAction.ImageIndex+1;
  //Attach Action
  ChangeIconButton.Action := ChangeIconAction;
end;

procedure TMainForm.ClearButtonClick(Sender: TObject);
begin
  //Clear Collection
  {$IFDEF D10_3+}
  VirtualImageList.Clear;
  {$ENDIF}
  ImageDataModule.SVGIconImageCollection.ClearIcons;
  UpdateGUI;
end;

procedure TMainForm.DeleteIconActionExecute(Sender: TObject);
begin
  if SVGIconVirtualImageList.SVGIconItems.Count > 0 then
  begin
    SVGIconVirtualImageList.SVGIconItems.Delete(0);
    UpdateGUI;
  end;
end;

{$IFDEF HiDPISupport}
procedure TMainForm.FormAfterMonitorDpiChanged(Sender: TObject; OldDPI, NewDPI: Integer);
begin
  UpdateGUI;
end;
{$ENDIF}

procedure TMainForm.FixedColorComboBoxSelect(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    if FixedColorComboBox.ItemIndex >= 0 then
    begin
      {$IFDEF D10_3+}
      //Warning: native VirtualImageList can change FixedColor only at Collection level
      //so the changes are affected to all forms opened!
      (VirtualImageList.ImageCollection as TSVGIconImageCollection).FixedColor :=
        FixedColorComboBox.Selected;
      {$ENDIF}
      //Change FixedColor at ImageList (affected only to current form)
      SVGIconVirtualImageList.FixedColor := FixedColorComboBox.Selected;
      UpdateGUI;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  FSVGIconImageListHot := TSVGIconImageList.Create(Self);
  FSVGIconImageListDisabled := TSVGIconImageList.Create(Self);

  {$IFDEF HiDPISupport}
  OnAfterMonitorDpiChanged := FormAfterMonitorDpiChanged;
  {$ENDIF}

  //Increase performance during drawing of SVG Image
  SvgIconImage.DoubleBuffered := True;

  {$IFDEF D10_3+}
  ////Test use of native VirtualImageList
  VirtualImageList := TVirtualImageList.Create(Self);
  VirtualImageList.ImageCollection := ImageDataModule.SVGIconImageCollection;
  //VirtualImageList.PreserveItems := True;
  ActionList.Images := VirtualImageList;
  TopToolBar.Images := VirtualImageList;
  {$ENDIF}

  //Build available VCL Styles
  SelectThemeRadioGroup.Items.Clear;
  for I := 0 to High(TStyleManager.StyleNames) do
    SelectThemeRadioGroup.Items.Add(TStyleManager.StyleNames[I]);
  TStringList(SelectThemeRadioGroup.Items).Sort;
  SelectThemeRadioGroup.OnClick := nil;
  try
    SelectThemeRadioGroup.ItemIndex := SelectThemeRadioGroup.Items.IndexOf({$IFDEF D10_1+}'Windows10'{$ELSE}'Windows'{$ENDIF});
  finally
    SelectThemeRadioGroup.OnClick := SelectThemeRadioGroupClick;
  end;
  SelectThemeRadioGroupClick(SelectThemeRadioGroup);

  TreeView.Items[0].Expand(True);
  TreeView.Items[2].Expand(True);

  TrackBar.Position := SVGIconVirtualImageList.Height;
  TrackBarChange(TrackBar);
end;

procedure TMainForm.GrayScaleCheckBoxClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    {$IFDEF D10_3+}
    //Warning: native VirtualImageList can change FixedColor only at Collection level
    //so the changes are affected to all forms opened!
    (VirtualImageList.ImageCollection as TSVGIconImageCollection).GrayScale:=
      GrayScaleCheckBox.Checked;
    {$ENDIF}
    //Change GrayScale at ImageList (affected only to current form)
    SVGIconVirtualImageList.GrayScale := GrayScaleCheckBox.Checked;
    UpdateGUI;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.SelectThemeRadioGroupClick(Sender: TObject);
var
  LStyleName: string;
begin
  Screen.Cursor := crHourGlass;
  try
    LStyleName := SelectThemeRadioGroup.Items[SelectThemeRadioGroup.ItemIndex];
    {$IFDEF DXE+}
    TStyleManager.TrySetStyle(LStyleName);
    //UpdateIconFontsColorByStyle(IconFontsImageList); TODO
    {$ELSE}
    SelectThemeRadioGroup.Visible := False;
    {$ENDIF}

    UpdateGUI;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.ImageViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  SVGIconImage.ImageIndex := Item.Index;
end;

procedure TMainForm.NewFormActionExecute(Sender: TObject);
begin
  With TMainForm.Create(Application) do
    Show;
end;

procedure TMainForm.paButtonsResize(Sender: TObject);
begin
  SVGIconImage.Height := SVGIconImage.width;
end;

procedure TMainForm.ShowImageEditorButtonClick(Sender: TObject);
begin
  //Image Editor for ImageCollection
  //if EditSVGIconImageCollection(ImageDataModule.SVGIconImageCollection) then
  //  UpdateGUI;

  //Image Editor for VirtualImageList
  if EditSVGIconVirtualImageList(SVGIconVirtualImageList) then
    UpdateGUI;
end;

procedure TMainForm.SVGIconImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    SVGIconImage.ImageIndex := SVGIconImage.ImageIndex + 1
  else
    SVGIconImage.ImageIndex := SVGIconImage.ImageIndex - 1;
end;

procedure TMainForm.tmrTrackbarTimer(Sender: TObject);
begin
  // Disable trackbar while updating. Otherwise a second event will be fired
  TrackBar.Enabled := false;
  try
    tmrTrackbar.Enabled := false;
    //Resize all icons into ImageList
    SVGIconVirtualImageList.Size := TrackBar.Position;
    UpdateGUI;
  finally
    TrackBar.Enabled := true;
  end;
end;

procedure TMainForm.updateGUI;
var
  LSize: Integer;
  {$IFDEF D10_3+}
  I: Integer;
  LItem: TSVGIconItem;
  {$ENDIF}
begin
  if FUpdating then
    Exit;
  FUpdating := True;
  try
    LSize := SVGIconVirtualImageList.Height;
    {$IFDEF D10_3+}
    VirtualImageList.Clear;
    VirtualImageList.SetSize(LSize, LSize);
    //Fill VirtualImageList with all icons in the collection of SVGIconVirtualImageList
    for I := 0 to SVGIconVirtualImageList.SVGIconItems.Count -1 do
    begin
      LItem := SVGIconVirtualImageList.SVGIconItems[I];
      VirtualImageList.Add(LItem.IconName, I, False);
    end;
    {$ENDIF+}
    IconSizeLabel.Caption := Format('Icons size: %d',[LSize]);
    TopToolBar.ButtonHeight := LSize + 2;
    TopToolBar.ButtonWidth := LSize + 2;
    TopToolBar.Height := LSize + 6;
    TreeView.Indent := LSize;
    Splitter.MinSize := DeleteButton.Width + 8;

    UpdateButtons;
    UpdateListView;
    UpdateTreeView;
    GrayScaleCheckBox.Checked := SVGIconVirtualImageList.GrayScale;
    FixedColorComboBox.Selected := SVGIconVirtualImageList.FixedColor;
  finally
    FUpdating := False;
  end;
end;

procedure TMainForm.UpdateTreeView;
var
  LItem: TTreeNode;
  I: Integer;
begin
  for I := 0 to TreeView.Items.Count - 1 do
  begin
    LItem := TreeView.Items[I];
    if SVGIconVirtualImageList.SVGIconItems.Count > LItem.ImageIndex then
    begin
      LItem.Text := SVGIconVirtualImageList.SVGIconItems.Items[LItem.ImageIndex].IconName;
    end
    else
    begin
      LItem.Text := '';
    end;
  end;
end;

procedure TMainForm.TrackBarChange(Sender: TObject);
begin
  tmrTrackbar.Enabled := true;
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
