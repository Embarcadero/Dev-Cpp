object DebugForm: TDebugForm
  Left = 255
  Top = 118
  Width = 514
  Height = 230
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
    506
    203)
  PixelsPerInch = 96
  TextHeight = 13
  object lvItems: TListView
    Left = 0
    Top = 0
    Width = 506
    Height = 169
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Variable'
      end
      item
        Caption = 'Value'
      end>
    TabOrder = 0
    ViewStyle = vsReport
  end
  object btnClose: TButton
    Left = 428
    Top = 174
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    TabOrder = 1
    OnClick = btnCloseClick
  end
end
