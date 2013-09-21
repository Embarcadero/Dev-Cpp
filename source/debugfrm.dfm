object DebugForm: TDebugForm
  Left = 426
  Top = 232
  Width = 613
  Height = 328
  Caption = 'DebugForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  DesignSize = (
    597
    290)
  PixelsPerInch = 96
  TextHeight = 13
  object lvItems: TListView
    Left = 0
    Top = 0
    Width = 597
    Height = 257
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelInner = bvNone
    BevelOuter = bvNone
    Columns = <
      item
        Caption = 'Variable'
        Width = 80
      end
      item
        Caption = 'Value'
      end>
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    ViewStyle = vsReport
  end
  object btnClose: TButton
    Left = 519
    Top = 264
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    TabOrder = 1
    OnClick = btnCloseClick
  end
end
