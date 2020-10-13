object Form1: TForm1
  Left = 341
  Top = 221
  Caption = 'Form1'
  ClientHeight = 202
  ClientWidth = 447
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
    Left = 112
    Top = 112
    Width = 161
    Height = 22
    Caption = 'Show Find Dialog'
    OnClick = SpeedButton1Click
  end
  object FindDialog1: TFindDialog
    Left = 280
    Top = 48
  end
end
