object Form1: TForm1
  Left = 679
  Top = 314
  BorderStyle = bsSingle
  Caption = 'Demo'
  ClientHeight = 81
  ClientWidth = 292
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
  object SpeedButton1: TSpeedButton
    Left = 56
    Top = 40
    Width = 169
    Height = 22
    Caption = 'Show OpenDialog'
    OnClick = SpeedButton1Click
  end
  object Label1: TLabel
    Left = 32
    Top = 8
    Width = 238
    Height = 13
    Caption = 'This App  show a dialog without style the NC area'
  end
  object OpenDialog1: TOpenDialog
    Left = 248
    Top = 16
  end
end
