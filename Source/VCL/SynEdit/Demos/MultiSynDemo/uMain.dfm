object frmMain: TfrmMain
  Left = 192
  Top = 107
  Width = 564
  Height = 395
  Caption = 'MultiHighlight HTML Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object SynEdit1: TSynEdit
    Left = 185
    Top = 0
    Width = 371
    Height = 322
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Pitch = fpFixed
    Font.Style = []
    TabOrder = 0
    Gutter.AutoSize = True
    Gutter.DigitCount = 2
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.ShowLineNumbers = True
    Gutter.Width = 10
    Highlighter = SynMultiSyn1
    Lines.Strings = (
      'SynEdit1')
    Options = [eoAutoIndent, eoKeepCaretX, eoShowScrollHint, eoSmartTabs, eoTabsToSpaces, eoTrimTrailingSpaces]
    WantTabs = True
    OnStatusChange = SynEdit1StatusChange
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
  object StatusBar1: TStatusBar
    Left = 0
    Top = 322
    Width = 556
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 322
    Align = alLeft
    TabOrder = 2
    object Label1: TLabel
      Left = 1
      Top = 1
      Width = 183
      Height = 81
      Align = alTop
      AutoSize = False
      Caption = 
        'Use CTRL+SPACE to bring up the Completion Proposal. Use SHIFT+SP' +
        'ACE to invoke the AutoComplete for one of the keywords below.'
      WordWrap = True
    end
    object ListBox1: TListBox
      Left = 1
      Top = 82
      Width = 183
      Height = 239
      Align = alClient
      ItemHeight = 13
      Items.Strings = (
        '!--'
        '!doct'
        'a'
        'applet'
        'frame'
        'head'
        'img'
        'link'
        'script'
        'style'
        'table'
        'td'
        'th'
        'tr'
        'ul'
        '')
      TabOrder = 0
      OnDblClick = ListBox1DblClick
    end
  end
  object SynCssSyn1: TSynCssSyn
    CommentAttri.Foreground = clGray
    NumberAttri.Foreground = clFuchsia
    AttributeAttri.Foreground = clNavy
    StringAttri.Foreground = clBlue
    StringAttri.Style = [fsBold]
    SymbolAttri.Foreground = clNavy
    SymbolAttri.Style = [fsBold]
    Left = 272
    Top = 64
  end
  object SynMultiSyn1: TSynMultiSyn
    Schemes = <
      item
        StartExpr = '<style type="text/css">'
        EndExpr = '</style>'
        Highlighter = SynCssSyn1
        MarkerAttri.Background = clNone
        MarkerAttri.Foreground = clPurple
        SchemeName = 'CSS Full'
      end
      item
        StartExpr = '<style>'
        EndExpr = '</style>'
        Highlighter = SynCssSyn1
        MarkerAttri.Background = clNone
        MarkerAttri.Foreground = clFuchsia
        SchemeName = 'CSS Style'
      end
      item
        StartExpr = '<script language="JavaScript" type="text/javascript">'
        EndExpr = '</script>'
        Highlighter = SynJScriptSyn1
        MarkerAttri.Background = clNone
        MarkerAttri.Foreground = clMaroon
        SchemeName = 'JS Full'
      end
      item
        StartExpr = '<script>'
        EndExpr = '</script>'
        Highlighter = SynJScriptSyn1
        MarkerAttri.Background = clNone
        MarkerAttri.Foreground = clRed
        SchemeName = 'JS Script'
      end>
    DefaultHighlighter = SynHTMLSyn1
    Left = 208
    Top = 64
  end
  object SynHTMLSyn1: TSynHTMLSyn
    DefaultFilter = 'HTML Document (*.htm,*.html)|*.htm;*.html'
    CommentAttri.Foreground = clGray
    CommentAttri.Style = [fsItalic]
    SymbolAttri.Foreground = clNavy
    SymbolAttri.Style = []
    Left = 240
    Top = 64
  end
  object SynJScriptSyn1: TSynJScriptSyn
    DefaultFilter = 'JavaScript files (*.js)|*.js'
    CommentAttri.Foreground = clGray
    IdentifierAttri.Foreground = clTeal
    IdentifierAttri.Style = [fsBold]
    KeyAttri.Foreground = clPurple
    NumberAttri.Foreground = clRed
    StringAttri.Foreground = clBlue
    SymbolAttri.Foreground = clGreen
    SymbolAttri.Style = [fsBold]
    Left = 304
    Top = 64
  end
  object SynCompletionProposal1: TSynCompletionProposal
    NbLinesInWindow = 6
    ClSelect = clInactiveCaption
    Width = 262
    EndOfTokenChr = '()[].'
    TriggerChars = '.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clBtnText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = [fsBold]
    Columns = <>
    ShortCut = 16416
    Editor = SynEdit1
    Left = 208
    Top = 96
  end
  object SynAutoComplete1: TSynAutoComplete
    EndOfTokenChr = '()[].'
    Editor = SynEdit1
    ShortCut = 8224
    Options = []
    Left = 240
    Top = 96
  end
  object MainMenu1: TMainMenu
    Left = 208
    Top = 32
    object File1: TMenuItem
      Caption = 'File'
      object New1: TMenuItem
        Caption = 'New'
        OnClick = New1Click
      end
      object Open1: TMenuItem
        Caption = 'Open'
        OnClick = Open1Click
      end
      object Save1: TMenuItem
        Caption = 'Save'
        OnClick = Save1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object About1: TMenuItem
        Caption = 'About'
        OnClick = About1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 240
    Top = 32
  end
  object SaveDialog1: TSaveDialog
    Left = 272
    Top = 32
  end
end
