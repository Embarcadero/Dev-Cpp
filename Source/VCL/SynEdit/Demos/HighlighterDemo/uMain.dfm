object Form1: TForm1
  Left = 268
  Top = 107
  Caption = 'Highlighter demo'
  ClientHeight = 446
  ClientWidth = 688
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object SynEdit1: TSynEdit
    Left = 0
    Top = 0
    Width = 688
    Height = 446
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
    RemovedKeystrokes = <
      item
        Command = ecCut
        ShortCut = 8238
      end
      item
        Command = ecPaste
        ShortCut = 8237
      end
      item
        Command = ecDeleteChar
        ShortCut = 46
      end
      item
        Command = ecContextHelp
        ShortCut = 112
      end
      item
        Command = ecPaste
        ShortCut = 16470
      end
      item
        Command = ecCut
        ShortCut = 16472
      end
      item
        Command = ecBlockIndent
        ShortCut = 24649
      end
      item
        Command = ecBlockUnindent
        ShortCut = 24661
      end
      item
        Command = ecLineBreak
        ShortCut = 16461
      end
      item
        Command = ecInsertLine
        ShortCut = 16462
      end
      item
        Command = ecDeleteWord
        ShortCut = 16468
      end>
    AddedKeystrokes = <
      item
        Command = ecPaste
        ShortCut = 8238
      end
      item
        Command = ecDeleteChar
        ShortCut = 8237
      end
      item
        Command = ecCut
        ShortCut = 46
      end
      item
        Command = ecContextHelp
        ShortCut = 16496
      end
      item
        Command = ecBlockIndent
        ShortCut = 16470
      end
      item
        Command = ecLineBreak
        ShortCut = 16472
      end
      item
        Command = ecInsertLine
        ShortCut = 24649
      end
      item
        Command = ecDeleteWord
        ShortCut = 24661
      end
      item
        Command = ecBlockUnindent
        ShortCut = 16461
      end
      item
        Command = ecPaste
        ShortCut = 16462
      end
      item
        Command = ecCut
        ShortCut = 16468
      end>
  end
end
