object ExtractionProgress: TExtractionProgress
  Left = 192
  Top = 107
  BorderIcons = [biMinimize, biMaximize]
  BorderStyle = bsDialog
  Caption = 'Extraction progress'
  ClientHeight = 54
  ClientWidth = 344
  Color = clWindow
  Ctl3D = False
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Heebo'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 17
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 332
    Height = 17
    Caption = 
      'Please wait while Package Manager is preparing the installation.' +
      '...'
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 24
    Width = 328
    Height = 19
    Smooth = True
    TabOrder = 0
  end
  object Memo1: TMemo
    Left = 8
    Top = 56
    Width = 313
    Height = 145
    ScrollBars = ssVertical
    TabOrder = 1
  end
end
