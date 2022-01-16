object Form1: TForm1
  Left = 66
  Top = 78
  Width = 584
  Height = 362
  Caption = 'Autocompletion demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = True
  Position = poDefault
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object SynEdit1: TSynEdit
    Left = 0
    Top = 0
    Width = 576
    Height = 308
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
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Highlighter = SynPasSyn1
    Lines.Strings = (
      '// Press <Ctrl+J> to invoke autocompletion, and <Ctrl+Z> to undo'
      ''
      'arrayc'
      'classf'
      'pro')
    RemovedKeystrokes = <
      item
        Command = ecLineBreak
        ShortCut = 8205
      end
      item
        Command = ecContextHelp
        ShortCut = 112
      end>
    AddedKeystrokes = <>
  end
  object SynPasSyn1: TSynPasSyn
    Left = 276
    Top = 104
  end
  object MainMenu1: TMainMenu
    Left = 276
    Top = 56
    object miNewForm: TMenuItem
      Caption = '&New form'
      OnClick = miNewFormClick
    end
  end
end
