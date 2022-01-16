object Form1: TForm1
  Left = 160
  Top = 108
  Width = 576
  Height = 393
  Caption = 'SynEdit export demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = menuMain
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object SynEdit1: TSynEdit
    Left = 0
    Top = 0
    Width = 568
    Height = 320
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
    RemovedKeystrokes = <
      item
        Command = ecDeleteLastChar
        ShortCut = 8200
      end
      item
        Command = ecDeleteLastWord
        ShortCut = 16392
      end
      item
        Command = ecUndo
        ShortCut = 32776
      end
      item
        Command = ecRedo
        ShortCut = 40968
      end
      item
        Command = ecLineBreak
        ShortCut = 13
      end
      item
        Command = ecLineBreak
        ShortCut = 8205
      end
      item
        Command = ecTab
        ShortCut = 9
      end
      item
        Command = ecShiftTab
        ShortCut = 8201
      end
      item
        Command = ecContextHelp
        ShortCut = 112
      end
      item
        Command = ecSelectAll
        ShortCut = 16449
      end
      item
        Command = ecCopy
        ShortCut = 16451
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
      end
      item
        Command = ecDeleteLine
        ShortCut = 16473
      end
      item
        Command = ecDeleteEOL
        ShortCut = 24665
      end
      item
        Command = ecUndo
        ShortCut = 16474
      end
      item
        Command = ecRedo
        ShortCut = 24666
      end
      item
        Command = ecGotoMarker0
        ShortCut = 16432
      end
      item
        Command = ecGotoMarker1
        ShortCut = 16433
      end
      item
        Command = ecGotoMarker2
        ShortCut = 16434
      end
      item
        Command = ecGotoMarker3
        ShortCut = 16435
      end
      item
        Command = ecGotoMarker4
        ShortCut = 16436
      end
      item
        Command = ecGotoMarker5
        ShortCut = 16437
      end
      item
        Command = ecGotoMarker6
        ShortCut = 16438
      end
      item
        Command = ecGotoMarker7
        ShortCut = 16439
      end
      item
        Command = ecGotoMarker8
        ShortCut = 16440
      end
      item
        Command = ecGotoMarker9
        ShortCut = 16441
      end
      item
        Command = ecSetMarker0
        ShortCut = 24624
      end
      item
        Command = ecSetMarker1
        ShortCut = 24625
      end
      item
        Command = ecSetMarker2
        ShortCut = 24626
      end
      item
        Command = ecSetMarker3
        ShortCut = 24627
      end
      item
        Command = ecSetMarker4
        ShortCut = 24628
      end
      item
        Command = ecSetMarker5
        ShortCut = 24629
      end
      item
        Command = ecSetMarker6
        ShortCut = 24630
      end
      item
        Command = ecSetMarker7
        ShortCut = 24631
      end
      item
        Command = ecSetMarker8
        ShortCut = 24632
      end
      item
        Command = ecSetMarker9
        ShortCut = 24633
      end
      item
        Command = ecNormalSelect
        ShortCut = 24654
      end
      item
        Command = ecColumnSelect
        ShortCut = 24643
      end
      item
        Command = ecLineSelect
        ShortCut = 24652
      end>
    AddedKeystrokes = <
      item
        Command = ecDeleteLastWord
        ShortCut = 8200
      end
      item
        Command = ecUndo
        ShortCut = 16392
      end
      item
        Command = ecRedo
        ShortCut = 32776
      end
      item
        Command = ecLineBreak
        ShortCut = 40968
      end
      item
        Command = ecSelectAll
        ShortCut = 13
      end
      item
        Command = ecCopy
        ShortCut = 16449
      end
      item
        Command = ecBlockIndent
        ShortCut = 16451
      end
      item
        Command = ecLineBreak
        ShortCut = 24649
      end
      item
        Command = ecInsertLine
        ShortCut = 16461
      end
      item
        Command = ecDeleteWord
        ShortCut = 16462
      end
      item
        Command = ecBlockUnindent
        ShortCut = 16468
      end
      item
        Command = ecPaste
        ShortCut = 24661
      end
      item
        Command = ecCut
        ShortCut = 16470
      end
      item
        Command = ecDeleteLine
        ShortCut = 16472
      end
      item
        Command = ecDeleteEOL
        ShortCut = 16473
      end
      item
        Command = ecUndo
        ShortCut = 24665
      end
      item
        Command = ecRedo
        ShortCut = 16474
      end
      item
        Command = ecGotoMarker0
        ShortCut = 24666
      end
      item
        Command = ecGotoMarker1
        ShortCut = 16432
      end
      item
        Command = ecGotoMarker2
        ShortCut = 16433
      end
      item
        Command = ecGotoMarker3
        ShortCut = 16434
      end
      item
        Command = ecGotoMarker4
        ShortCut = 16435
      end
      item
        Command = ecGotoMarker5
        ShortCut = 16436
      end
      item
        Command = ecGotoMarker6
        ShortCut = 16437
      end
      item
        Command = ecGotoMarker7
        ShortCut = 16438
      end
      item
        Command = ecGotoMarker8
        ShortCut = 16439
      end
      item
        Command = ecGotoMarker9
        ShortCut = 16440
      end
      item
        Command = ecSetMarker0
        ShortCut = 16441
      end
      item
        Command = ecSetMarker1
        ShortCut = 24624
      end
      item
        Command = ecSetMarker2
        ShortCut = 24625
      end
      item
        Command = ecSetMarker3
        ShortCut = 24626
      end
      item
        Command = ecSetMarker4
        ShortCut = 24627
      end
      item
        Command = ecSetMarker5
        ShortCut = 24628
      end
      item
        Command = ecSetMarker6
        ShortCut = 24629
      end
      item
        Command = ecSetMarker7
        ShortCut = 24630
      end
      item
        Command = ecSetMarker8
        ShortCut = 24631
      end
      item
        Command = ecSetMarker9
        ShortCut = 24632
      end
      item
        Command = ecNormalSelect
        ShortCut = 24633
      end
      item
        Command = ecColumnSelect
        ShortCut = 24654
      end
      item
        Command = ecLineSelect
        ShortCut = 24643
      end
      item
        Command = ecTab
        ShortCut = 24652
      end
      item
        Command = ecShiftTab
        ShortCut = 9
      end
      item
        Command = ecMatchBracket
        ShortCut = 8201
      end>
  end
  object Statusbar: TStatusBar
    Left = 0
    Top = 320
    Width = 568
    Height = 19
    Panels = <>
  end
  object menuMain: TMainMenu
    Left = 40
    Top = 112
    object mFile: TMenuItem
      Caption = '&File'
      object miFileOpen: TMenuItem
        Caption = '&Open...'
        ShortCut = 16463
        OnClick = miFileOpenClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object miFileExit: TMenuItem
        Caption = 'Exit'
        ShortCut = 32883
        OnClick = miFileExitClick
      end
    end
    object mExport: TMenuItem
      Caption = '&Export'
      OnClick = mExportClick
      object miExportAsHTML: TMenuItem
        Caption = 'As &HTML'
        OnClick = miExportAsClicked
      end
      object miExportAsRTF: TMenuItem
        Caption = 'As &RTF'
        OnClick = miExportAsClicked
      end
      object miExportAllFormats: TMenuItem
        Caption = '&All formats'
        OnClick = miExportAsClicked
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object miExportToFile: TMenuItem
        Caption = 'Export to &file...'
        OnClick = miExportToFileClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object miExportClipboardNative: TMenuItem
        Caption = 'Copy &native format to clipboard'
        OnClick = miExportClipboardNativeClick
      end
      object miExportClipboardText: TMenuItem
        Caption = 'Copy as &text to clipboard'
        OnClick = miExportClipboardTextClick
      end
    end
  end
  object dlgFileOpen: TOpenDialog
    Left = 40
    Top = 148
  end
  object dlgFileSaveAs: TSaveDialog
    Title = 'Export file as'
    Left = 76
    Top = 148
  end
  object SynExporterHTML1: TSynExporterHTML
    Color = clWindow
    DefaultFilter = 'HTML Document (*.htm,*.html)|*.htm;*.html'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Title = 'Untitled'
    UseBackground = True
    Left = 40
    Top = 184
  end
  object SynExporterRTF1: TSynExporterRTF
    Color = clWindow
    DefaultFilter = 'Rich Text Format (*.rtf)|*.rtf'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Title = 'Untitled'
    UseBackground = True
    Left = 76
    Top = 184
  end
  object SynCppSyn1: TSynCppSyn
    DefaultFilter = 'C++ files (*.cpp,*.h,*.hpp)|*.cpp;*.h;*.hpp'
    Left = 40
    Top = 8
  end
  object SynDfmSyn1: TSynDfmSyn
    DefaultFilter = 'Delphi/C++ Builder Form Files (*.dfm)|*.dfm'
    Left = 72
    Top = 8
  end
  object SynPasSyn1: TSynPasSyn
    Left = 104
    Top = 8
  end
end
