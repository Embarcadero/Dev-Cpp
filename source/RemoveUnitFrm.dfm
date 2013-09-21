object RemoveUnitForm: TRemoveUnitForm
  Left = 687
  Top = 393
  BorderIcons = [biSystemMenu, biMaximize]
  BorderStyle = bsDialog
  Caption = 'Remove from project'
  ClientHeight = 279
  ClientWidth = 338
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
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
    Anchors = [akRight]
    Caption = 'Delete'
    TabOrder = 1
    OnClick = DelBtnClick
  end
end
