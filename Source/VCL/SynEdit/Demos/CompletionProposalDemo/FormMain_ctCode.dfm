object Form1: TForm1
  Left = 212
  Top = 100
  Width = 776
  Height = 586
  Caption = 'SynEdit Code Completion Demo for ctCode'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 753
    Height = 537
    ActivePage = CodeCompletion
    TabOrder = 0
    object CodeCompletion: TTabSheet
      Caption = 'CodeCompletion'
      object Label3: TLabel
        Left = 8
        Top = 19
        Width = 64
        Height = 13
        Caption = 'BiggestWord:'
      end
      object Label4: TLabel
        Left = 224
        Top = 19
        Width = 23
        Height = 13
        Caption = 'Title:'
      end
      object SynTest: TSynEdit
        Left = 416
        Top = 8
        Width = 321
        Height = 497
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        TabOrder = 9
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Terminal'
        Gutter.Font.Style = []
        Gutter.Visible = False
        Lines.Strings = (
          'This is a Demo to show you how the'
          'Code Completion component works'
          'when the default kind is ctCode.'
          ''
          'BiggestWord: If you are using'
          '  PrettyText then this is the biggest'
          '  word that will show up before the'
          '  *bold* words'
          ''
          'CaseSensitive: makes the text you'
          '  type and the matching in the'
          '  dropdown list case sensitive'
          ''
          'AnsiStrings  : Use Ansi string'
          '  comparisons instead of default'
          '  string comparisons'
          ''
          'UsePrettyText: Allows you to format'
          '  the text displayed in the dropdown.'
          '  Please refer to the tsyncompletion-'
          '  proposal.html file for a description'
          '  of the available commands.'
          ''
          'UseInsertList: Lets you display one'
          '  thing in the dropdown and insert'
          '  another thing when they choose an'
          '  item.  Like in Delphi, the'
          '  dropdown might display'
          '  "procedure foo(AVariable: Integer)"'
          '  and only insert foo when you'
          '  select it.  The InsertList must'
          '  have as many items as the ItemList'
          '  or you will get a list index out'
          '  of bounds error when you select'
          '  an item outside of the range.'
          ''
          'LimitToMatchedText: Limits the'
          '  dropdown to the items matching the'
          '  text you have typed, similar to the'
          '  way the D6 does it.')
        RemovedKeystrokes = <
          item
            Command = ecContextHelp
            ShortCut = 112
          end>
        AddedKeystrokes = <
          item
            Command = ecContextHelp
            ShortCut = 16496
          end>
      end
      object edBiggestWord: TEdit
        Left = 80
        Top = 16
        Width = 121
        Height = 21
        TabOrder = 0
        Text = 'constructor'
        OnChange = edBiggestWordChange
      end
      object cbCase: TCheckBox
        Tag = 1
        Left = 8
        Top = 40
        Width = 129
        Height = 17
        Caption = 'Case Sensitive'
        TabOrder = 2
        OnClick = CheckBoxClick
      end
      object cbPrettyText: TCheckBox
        Tag = 3
        Left = 8
        Top = 64
        Width = 129
        Height = 17
        Caption = 'Use Pretty Text'
        Checked = True
        State = cbChecked
        TabOrder = 3
        OnClick = CheckBoxClick
      end
      object cbUseInsertList: TCheckBox
        Tag = 4
        Left = 8
        Top = 88
        Width = 129
        Height = 17
        Caption = 'Use Insert List'
        Checked = True
        State = cbChecked
        TabOrder = 4
        OnClick = CheckBoxClick
      end
      object cbLimitToMatchedText: TCheckBox
        Tag = 5
        Left = 8
        Top = 112
        Width = 129
        Height = 17
        Caption = 'Limit To Matched Text'
        Checked = True
        State = cbChecked
        TabOrder = 5
        OnClick = CheckBoxClick
      end
      object SynEdit1: TSynEdit
        Left = 5
        Top = 160
        Width = 401
        Height = 344
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        TabOrder = 8
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Terminal'
        Gutter.Font.Style = []
        Lines.Strings = (
          'Use Ctrl+Space to activate Code Completion'
          'with a shortcut, or use the '#39'.'#39' key'
          'to activate it with a timer'
          ''
          '')
        RemovedKeystrokes = <
          item
            Command = ecContextHelp
            ShortCut = 112
          end>
        AddedKeystrokes = <
          item
            Command = ecContextHelp
            ShortCut = 16496
          end>
      end
      object edTitle: TEdit
        Left = 256
        Top = 16
        Width = 145
        Height = 21
        TabOrder = 1
        Text = 'Completion Proposal Demo'
        OnChange = edTitleChange
      end
      object Button3: TButton
        Left = 240
        Top = 112
        Width = 75
        Height = 25
        Caption = 'Font'
        TabOrder = 6
        OnClick = Button3Click
      end
      object Button4: TButton
        Left = 328
        Top = 112
        Width = 75
        Height = 25
        Caption = 'Title Font'
        TabOrder = 7
        OnClick = Button4Click
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Insert and Item Lists'
      ImageIndex = 1
      object Label1: TLabel
        Left = 8
        Top = 8
        Width = 57
        Height = 13
        Caption = 'Insert List'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label2: TLabel
        Left = 8
        Top = 240
        Width = 45
        Height = 13
        Caption = 'ItemList'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object mmoInsert: TMemo
        Left = 8
        Top = 24
        Width = 729
        Height = 169
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'mmoInsert')
        ParentFont = False
        TabOrder = 0
      end
      object mmoItem: TMemo
        Left = 8
        Top = 256
        Width = 729
        Height = 209
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'mmoItem')
        ParentFont = False
        TabOrder = 1
        WantTabs = True
      end
      object Button1: TButton
        Left = 8
        Top = 200
        Width = 137
        Height = 25
        Caption = 'Update Insert List'
        TabOrder = 2
        OnClick = Button1Click
      end
      object Button2: TButton
        Left = 8
        Top = 472
        Width = 137
        Height = 25
        Caption = 'Update Item List'
        TabOrder = 3
        OnClick = Button2Click
      end
    end
  end
  object scpDemo: TSynCompletionProposal
    Options = [scoLimitToMatchedText, scoUseInsertList, scoUsePrettyText, scoUseBuiltInTimer, scoEndCharCompletion, scoCompleteWithTab, scoCompleteWithEnter]
    Width = 262
    EndOfTokenChr = '()[]. '
    TriggerChars = '.'
    Title = 'Completion Proposal Demo'
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
    Columns = <
      item
      end>
    ShortCut = 16416
    Editor = SynEdit1
    Left = 216
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 348
    Top = 80
  end
end
