object RemoveUnitForm: TRemoveUnitForm
  Left = 687
  Top = 393
  Width = 354
  Height = 317
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Remove from project'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    338
    279)
  PixelsPerInch = 96
  TextHeight = 13
  object UnitList: TListBox
    Left = 0
    Top = 0
    Width = 338
    Height = 251
    Align = alCustom
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 0
    OnKeyPress = UnitListKeyPress
  end
  object DelBtn: TButton
    Left = 132
    Top = 252
    Width = 75
    Height = 25
    Anchors = [akBottom]
    Caption = 'Delete'
    TabOrder = 1
    OnClick = DelBtnClick
  end
end
