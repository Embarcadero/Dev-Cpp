object frmBenchmark: TfrmBenchmark
  Left = 0
  Top = 0
  Caption = 'Benchmark'
  ClientHeight = 342
  ClientWidth = 719
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lblLoops: TLabel
    Left = 304
    Top = 316
    Width = 28
    Height = 13
    Caption = '&Loops'
    FocusControl = speLoops
  end
  object memOutput: TMemo
    Left = 24
    Top = 24
    Width = 385
    Height = 281
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
  end
  object btnClear: TButton
    Left = 24
    Top = 311
    Width = 100
    Height = 25
    Caption = '&Clear'
    TabOrder = 1
    OnClick = btnClearClick
  end
  object btnLoad: TButton
    Left = 130
    Top = 311
    Width = 100
    Height = 25
    Caption = 'L&oad Image'
    TabOrder = 2
    OnClick = btnLoadClick
  end
  object SVGIconImage: TSVGIconImage
    Left = 424
    Top = 24
    Width = 281
    Height = 281
    AutoSize = False
    ImageList = imlIcons
  end
  object btnRunBenchmark: TButton
    Left = 447
    Top = 309
    Width = 100
    Height = 25
    Caption = '&Benchmark'
    TabOrder = 4
    OnClick = btnRunBenchmarkClick
  end
  object speLoops: TSpinEdit
    Left = 360
    Top = 311
    Width = 81
    Height = 22
    MaxValue = 999
    MinValue = 10
    TabOrder = 5
    Value = 100
  end
  object SVGIconImageCollection: TSVGIconImageCollection
    SVGIconItems = <>
    Left = 48
    Top = 40
  end
  object imlIcons: TVirtualImageList
    AutoFill = True
    DisabledGrayscale = False
    DisabledSuffix = '_Disabled'
    Images = <>
    ImageCollection = SVGIconImageCollection
    Left = 48
    Top = 88
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '*.svg'
    Left = 48
    Top = 136
  end
end
