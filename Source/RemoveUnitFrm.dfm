object RemoveUnitForm: TRemoveUnitForm
  Left = 687
  Top = 393
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Remove from project'
  ClientHeight = 196
  ClientWidth = 342
  Color = clBtnFace
  Constraints.MinHeight = 90
  Constraints.MinWidth = 180
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object UnitList: TListBox
    Left = 0
    Top = 0
    Width = 342
    Height = 169
    Align = alClient
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 0
    OnKeyPress = UnitListKeyPress
  end
  object paBottom: TPanel
    Left = 0
    Top = 169
    Width = 342
    Height = 27
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      342
      27)
    object DelBtn: TButton
      Left = 136
      Top = 1
      Width = 75
      Height = 25
      Anchors = [akTop]
      Caption = 'Delete'
      TabOrder = 0
      OnClick = DelBtnClick
    end
  end
end
