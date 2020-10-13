object FrmMain: TFrmMain
  Left = 752
  Top = 177
  Caption = 'Demo'
  ClientHeight = 183
  ClientWidth = 176
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 32
    Top = 16
    Width = 113
    Height = 25
    Caption = 'ODBC Dialog'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 32
    Top = 47
    Width = 113
    Height = 25
    Caption = 'DataLink Dialog'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 32
    Top = 78
    Width = 113
    Height = 25
    Caption = 'Network Connect'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 32
    Top = 109
    Width = 113
    Height = 25
    Caption = 'Network DisConnect'
    TabOrder = 3
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 32
    Top = 140
    Width = 113
    Height = 25
    Caption = 'Dialog Object Select'
    TabOrder = 4
    OnClick = Button5Click
  end
end
