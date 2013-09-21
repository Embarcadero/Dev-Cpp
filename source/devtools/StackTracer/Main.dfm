object Form1: TForm1
  Left = 192
  Top = 107
  Width = 709
  Height = 412
  Caption = 'StackTracer'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  DesignSize = (
    701
    385)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 44
    Width = 145
    Height = 49
    AutoSize = False
    Caption = 
      '2. Enter addresses from bug report (copy-paste string under sect' +
      'ion "Stack trace"):'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 156
    Top = 12
    Width = 32
    Height = 13
    Caption = 'Label2'
  end
  object Memo1: TMemo
    Left = 156
    Top = 40
    Width = 537
    Height = 53
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Memo2: TMemo
    Left = 156
    Top = 100
    Width = 537
    Height = 277
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 137
    Height = 25
    Caption = '1. Select map file...'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 100
    Width = 137
    Height = 25
    Caption = '3. See results'
    TabOrder = 3
    OnClick = Button2Click
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'map'
    Filter = 'Map files (*.map)|*.map'
    Left = 200
    Top = 4
  end
end
