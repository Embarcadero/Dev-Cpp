object Form1: TForm1
  Left = 140
  Top = 102
  Width = 556
  Height = 394
  Caption = 'Form1'
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 548
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object DBNavigator1: TDBNavigator
      Left = 4
      Top = 4
      Width = 240
      Height = 25
      DataSource = DataSource1
      TabOrder = 0
    end
    object DBEdit1: TDBEdit
      Left = 256
      Top = 5
      Width = 153
      Height = 24
      DataSource = DataSource1
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
  end
  object DBSynEdit1: TDBSynEdit
    Left = 0
    Top = 33
    Width = 548
    Height = 327
    DataSource = DataSource1
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    TabOrder = 1
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
  object DataSource1: TDataSource
    DataSet = Table1
    OnDataChange = DataSource1DataChange
    Left = 72
    Top = 92
  end
  object Table1: TTable
    AfterOpen = UpdateHighlighterNeeded
    AfterEdit = UpdateHighlighterNeeded
    AfterPost = UpdateHighlighterNeeded
    AfterDelete = UpdateHighlighterNeeded
    OnUpdateRecord = Table1UpdateRecord
    Left = 112
    Top = 92
  end
  object SynPasSyn1: TSynPasSyn
    Left = 72
    Top = 152
  end
  object SynPerlSyn1: TSynPerlSyn
    DefaultFilter = 'Perl files (*.pl,*.pm,*.cgi)|*.pl;*.pm;*.cgi'
    Left = 112
    Top = 152
  end
end
