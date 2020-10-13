object FrmMain: TFrmMain
  Left = 708
  Top = 357
  Caption = 'TWebBrowser with Vcl Styles'
  ClientHeight = 384
  ClientWidth = 535
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    535
    384)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 39
    Height = 13
    Caption = 'Address'
  end
  object Label2: TLabel
    Left = 8
    Top = 51
    Width = 29
    Height = 13
    Caption = 'Styles'
  end
  object WebBrowser1: TWebBrowser
    Left = 8
    Top = 97
    Width = 519
    Height = 279
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    OnNavigateComplete2 = WebBrowser1NavigateComplete2
    OnDocumentComplete = WebBrowser1DocumentComplete
    ExplicitWidth = 662
    ExplicitHeight = 281
    ControlData = {
      4C000000A4350000D61C00000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object Button1: TButton
    Left = 452
    Top = 22
    Width = 75
    Height = 25
    Caption = 'Go'
    TabOrder = 1
    OnClick = Button1Click
  end
  object EditAddress: TEdit
    Left = 8
    Top = 24
    Width = 438
    Height = 21
    TabOrder = 2
    Text = 'http://www.google.com/ncr'
  end
  object ComboBoxStyles: TComboBox
    Left = 8
    Top = 70
    Width = 129
    Height = 21
    Style = csDropDownList
    TabOrder = 3
    OnChange = ComboBoxStylesChange
  end
end
