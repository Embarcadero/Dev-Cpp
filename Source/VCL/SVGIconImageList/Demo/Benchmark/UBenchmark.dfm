object frmBenchmark: TfrmBenchmark
  Left = 0
  Top = 0
  Caption = 'Benchmark'
  ClientHeight = 545
  ClientWidth = 784
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object splHorizontal: TSplitter
    Left = 0
    Top = 421
    Width = 784
    Height = 4
    Cursor = crVSplit
    Align = alBottom
    AutoSnap = False
    Beveled = True
    MinSize = 100
    ExplicitTop = 422
  end
  object memOutput: TMemo
    Left = 0
    Top = 425
    Width = 784
    Height = 120
    Align = alBottom
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object SVGIconImage: TSVGIconImage
    Left = 0
    Top = 0
    Width = 634
    Height = 421
    AutoSize = False
    ParentDoubleBuffered = False
    DoubleBuffered = True
    ImageList = SVGIconVirtualImageList
    Align = alClient
  end
  object pnlButtons: TPanel
    Left = 634
    Top = 0
    Width = 150
    Height = 421
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 2
    object btnClear: TButton
      AlignWithMargins = True
      Left = 10
      Top = 386
      Width = 130
      Height = 30
      Margins.Left = 10
      Margins.Top = 5
      Margins.Right = 10
      Margins.Bottom = 5
      Align = alBottom
      Caption = '&Clear Output'
      TabOrder = 0
      OnClick = btnClearClick
    end
    object btnLoad: TButton
      AlignWithMargins = True
      Left = 10
      Top = 5
      Width = 130
      Height = 30
      Margins.Left = 10
      Margins.Top = 5
      Margins.Right = 10
      Margins.Bottom = 5
      Align = alTop
      Caption = 'L&oad Image'
      TabOrder = 1
      OnClick = btnLoadClick
    end
    object btnRunBenchmark: TButton
      AlignWithMargins = True
      Left = 10
      Top = 346
      Width = 130
      Height = 30
      Margins.Left = 10
      Margins.Top = 5
      Margins.Right = 10
      Margins.Bottom = 5
      Align = alBottom
      Caption = '&Benchmark'
      TabOrder = 2
      OnClick = btnRunBenchmarkClick
    end
    object chkGrayScale: TCheckBox
      AlignWithMargins = True
      Left = 10
      Top = 256
      Width = 130
      Height = 20
      Margins.Left = 10
      Margins.Top = 5
      Margins.Right = 10
      Margins.Bottom = 5
      Align = alBottom
      Caption = '&Grayscale'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object chkFixedColor: TCheckBox
      AlignWithMargins = True
      Left = 10
      Top = 286
      Width = 130
      Height = 20
      Margins.Left = 10
      Margins.Top = 5
      Margins.Right = 10
      Margins.Bottom = 5
      Align = alBottom
      Caption = '&Fixed Color'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
    object pnlLoops: TPanel
      Left = 0
      Top = 311
      Width = 150
      Height = 30
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 5
      object lblLoops: TLabel
        AlignWithMargins = True
        Left = 10
        Top = 8
        Width = 32
        Height = 17
        Margins.Left = 10
        Margins.Top = 8
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alLeft
        Caption = '&Loops'
        FocusControl = speLoops
        ExplicitHeight = 15
      end
      object speLoops: TSpinEdit
        AlignWithMargins = True
        Left = 52
        Top = 5
        Width = 88
        Height = 24
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 10
        Margins.Bottom = 5
        MaxValue = 999
        MinValue = 1
        TabOrder = 0
        Value = 50
      end
    end
    object grpFactory: TRadioGroup
      AlignWithMargins = True
      Left = 10
      Top = 45
      Width = 130
      Height = 105
      Margins.Left = 10
      Margins.Top = 5
      Margins.Right = 10
      Margins.Bottom = 5
      Align = alTop
      Caption = 'SVG Factory'
      TabOrder = 6
      OnClick = grpFactoryClick
    end
    object chkDrawVisible: TCheckBox
      AlignWithMargins = True
      Left = 10
      Top = 227
      Width = 130
      Height = 20
      Margins.Left = 10
      Margins.Top = 5
      Margins.Right = 10
      Margins.Bottom = 5
      Align = alBottom
      Caption = '&Draw visible'
      Checked = True
      State = cbChecked
      TabOrder = 7
      ExplicitTop = 230
    end
  end
  object SVGIconImageCollection: TSVGIconImageCollection
    SVGIconItems = <>
    Left = 48
    Top = 40
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '*.svg'
    Left = 48
    Top = 136
  end
  object SVGIconVirtualImageList: TSVGIconVirtualImageList
    ImageCollection = SVGIconImageCollection
    Left = 48
    Top = 88
  end
end
