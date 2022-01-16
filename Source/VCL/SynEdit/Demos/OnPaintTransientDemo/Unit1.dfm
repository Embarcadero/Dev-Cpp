object Form1: TForm1
  Left = 385
  Top = 196
  Width = 696
  Height = 480
  Caption = 'OnPaintTransient Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Editor: TSynEdit
    Left = 0
    Top = 41
    Width = 688
    Height = 405
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 0
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Terminal'
    Gutter.Font.Style = []
    Highlighter = SynJavaSyn1
    OnPaintTransient = EditorPaintTransient
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 688
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Button2: TButton
      Left = 16
      Top = 8
      Width = 75
      Height = 25
      Caption = '&Open'
      TabOrder = 0
      OnClick = Button2Click
    end
    object Button1: TButton
      Left = 152
      Top = 8
      Width = 137
      Height = 25
      Caption = 'Toggle Enabled'
      TabOrder = 1
      Visible = False
      OnClick = Button1Click
    end
  end
  object SynJavaSyn1: TSynJavaSyn
    Enabled = False
    Left = 192
    Top = 88
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Java files|*.java'
    Left = 104
    Top = 8
  end
end
