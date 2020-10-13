object Form1: TForm1
  Left = 699
  Top = 271
  Caption = 'Main'
  ClientHeight = 124
  ClientWidth = 331
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object SpeedButton1: TSpeedButton
    Left = 48
    Top = 56
    Width = 241
    Height = 22
    Caption = 'Show My Window'
    OnClick = SpeedButton1Click
  end
  object CheckBox1: TCheckBox
    Left = 48
    Top = 24
    Width = 113
    Height = 17
    Caption = 'Enable SysControls'
    Checked = True
    State = cbChecked
    TabOrder = 0
    OnClick = CheckBox1Click
  end
end
