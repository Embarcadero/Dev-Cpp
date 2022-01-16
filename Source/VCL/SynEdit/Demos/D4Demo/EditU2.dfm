object DemoMainForm: TDemoMainForm
  Left = 287
  Top = 202
  Width = 685
  Height = 440
  Caption = 'SynEdit General Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Visible = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 677
    Height = 113
    ActivePage = TabSheet2
    Align = alTop
    TabOrder = 0
    object tabFile: TTabSheet
      Caption = 'File'
      object outFilename: TLabel
        Left = 88
        Top = 12
        Width = 42
        Height = 13
        Caption = 'Filename'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        Visible = False
      end
      object btnLoad: TButton
        Left = 8
        Top = 6
        Width = 75
        Height = 25
        Caption = '&Load'
        TabOrder = 0
        OnClick = btnLoadClick
      end
      object cbReadonly: TCheckBox
        Left = 8
        Top = 35
        Width = 73
        Height = 17
        Caption = '&ReadOnly'
        TabOrder = 1
        OnClick = cbReadonlyClick
      end
    end
    object tabDisplay: TTabSheet
      Caption = 'Display'
      object Label12: TLabel
        Left = 107
        Top = 36
        Width = 56
        Height = 13
        Caption = '&Right Edge:'
        FocusControl = inpRightEdge
      end
      object Label13: TLabel
        Left = 325
        Top = 42
        Width = 53
        Height = 13
        Caption = 'Scroll B&ars:'
        FocusControl = cbxScrollBars
      end
      object Label14: TLabel
        Left = 409
        Top = 12
        Width = 27
        Height = 13
        Caption = '&Color:'
        FocusControl = cbxColor
      end
      object Label15: TLabel
        Left = 409
        Top = 36
        Width = 78
        Height = 13
        Caption = 'Sel. &Foreground:'
        FocusControl = cbxForeground
      end
      object Label16: TLabel
        Left = 409
        Top = 60
        Width = 82
        Height = 13
        Caption = 'Sel. &Background:'
        FocusControl = cbxBackground
      end
      object Label26: TLabel
        Left = 107
        Top = 12
        Width = 86
        Height = 13
        Caption = 'E&xtra line spacing:'
      end
      object Label28: TLabel
        Left = 107
        Top = 60
        Width = 83
        Height = 13
        Caption = 'Right Edge Color:'
        FocusControl = cbxREColor
      end
      object cbHideSelection: TCheckBox
        Left = 8
        Top = 11
        Width = 97
        Height = 17
        Caption = 'Hide &Selection'
        TabOrder = 0
        OnClick = cbHideSelectionClick
      end
      object inpRightEdge: TSpinEdit
        Left = 195
        Top = 33
        Width = 49
        Height = 22
        MaxLength = 3
        MaxValue = 999
        MinValue = 0
        TabOrder = 1
        Value = 80
        OnChange = inpRightEdgeChange
      end
      object cbScrollPastEOL: TCheckBox
        Left = 8
        Top = 35
        Width = 97
        Height = 17
        Caption = 'Scroll Past &EOL'
        TabOrder = 2
        OnClick = cbScrollPastEOLClick
      end
      object cbxScrollBars: TComboBox
        Left = 325
        Top = 57
        Width = 81
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 3
        OnChange = cbxScrollBarsChange
        Items.Strings = (
          'None'
          'Horizontal'
          'Vertical'
          'Both')
      end
      object cbxColor: TComboBox
        Left = 494
        Top = 9
        Width = 128
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 4
        OnChange = cbxColorChange
      end
      object cbxForeground: TComboBox
        Left = 494
        Top = 33
        Width = 128
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 5
        OnChange = cbxForegroundChange
      end
      object cbxBackground: TComboBox
        Left = 494
        Top = 57
        Width = 128
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 6
        OnChange = cbxBackgroundChange
      end
      object btnFont: TButton
        Left = 263
        Top = 9
        Width = 121
        Height = 25
        Caption = 'F&ont'
        TabOrder = 7
        OnClick = btnFontClick
      end
      object inpExtraLineSpacing: TSpinEdit
        Left = 195
        Top = 9
        Width = 49
        Height = 22
        MaxLength = 3
        MaxValue = 99
        MinValue = -99
        TabOrder = 8
        Value = 0
        OnChange = inpExtraLineSpacingChange
      end
      object cbxREColor: TComboBox
        Left = 195
        Top = 57
        Width = 128
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 9
        OnChange = cbxREColorChange
      end
      object cbHalfPageScroll: TCheckBox
        Left = 8
        Top = 59
        Width = 97
        Height = 17
        Caption = '&HalfPage Scroll'
        TabOrder = 10
        OnClick = cbHalfPageScrollClick
      end
    end
    object tabEdit: TTabSheet
      Caption = 'Edit'
      object Label29: TLabel
        Left = 8
        Top = 61
        Width = 53
        Height = 13
        Caption = '&Tab Width:'
        FocusControl = inpTabWidth
      end
      object cbAutoIndent: TCheckBox
        Left = 8
        Top = 11
        Width = 97
        Height = 17
        Caption = '&AutoIndent'
        TabOrder = 0
        OnClick = cbAutoIndentClick
      end
      object cbWantTabs: TCheckBox
        Left = 8
        Top = 35
        Width = 97
        Height = 17
        Caption = '&Want Tabs'
        TabOrder = 1
        OnClick = cbWantTabsClick
      end
      object inpTabWidth: TSpinEdit
        Left = 72
        Top = 57
        Width = 57
        Height = 22
        MaxValue = 16
        MinValue = 1
        TabOrder = 2
        Value = 2
        OnChange = inpTabWidthChange
      end
      object cbDragDropEdit: TCheckBox
        Left = 148
        Top = 11
        Width = 129
        Height = 17
        Caption = 'Drag and drop editing'
        TabOrder = 3
        OnClick = cbDragDropEditClick
      end
    end
    object tabSearch: TTabSheet
      Caption = 'Search'
      object lblSearchResult: TLabel
        Left = 268
        Top = 12
        Width = 481
        Height = 13
        AutoSize = False
        Caption = 'lblSearchResult'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        Visible = False
      end
      object btnSearch: TButton
        Left = 8
        Top = 6
        Width = 75
        Height = 25
        Caption = '&Search...'
        TabOrder = 0
        OnClick = btnSearchClick
      end
      object btnSearchNext: TButton
        Left = 92
        Top = 6
        Width = 75
        Height = 25
        Caption = '&Next'
        Enabled = False
        TabOrder = 1
        OnClick = btnSearchNextPrevClick
      end
      object btnSearchPrev: TButton
        Left = 176
        Top = 6
        Width = 75
        Height = 25
        Caption = '&Previous'
        Enabled = False
        TabOrder = 2
        OnClick = btnSearchNextPrevClick
      end
      object btnReplace: TButton
        Left = 8
        Top = 42
        Width = 75
        Height = 25
        Caption = '&Replace...'
        TabOrder = 3
        OnClick = btnReplaceClick
      end
    end
    object tabCaret: TTabSheet
      Caption = 'Caret'
      object Label7: TLabel
        Left = 8
        Top = 12
        Width = 57
        Height = 13
        Caption = '&Insert Caret:'
        FocusControl = cbxInsertCaret
      end
      object Label8: TLabel
        Left = 8
        Top = 36
        Width = 76
        Height = 13
        Caption = '&Overwrite Caret:'
        FocusControl = cbxOverwriteCaret
      end
      object cbxInsertCaret: TComboBox
        Left = 88
        Top = 9
        Width = 97
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnChange = cbxInsertCaretChange
        Items.Strings = (
          'Vertical Line'
          'Horizontal Line'
          'Half Block'
          'Block')
      end
      object cbxOverwriteCaret: TComboBox
        Left = 88
        Top = 33
        Width = 97
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 1
        OnChange = cbxOverwriteCaretChange
        Items.Strings = (
          'Vertical Line'
          'Horizontal Line'
          'Half Block'
          'Block')
      end
      object cbInsertMode: TCheckBox
        Left = 8
        Top = 59
        Width = 80
        Height = 17
        Caption = 'Insert &Mode'
        TabOrder = 2
        OnClick = cbInsertModeClick
      end
    end
    object tabGutter: TTabSheet
      Caption = 'Gutter'
      object Label5: TLabel
        Left = 8
        Top = 12
        Width = 59
        Height = 13
        Caption = 'Gutter &Color:'
        FocusControl = cbxGutterColor
      end
      object Label6: TLabel
        Left = 8
        Top = 36
        Width = 63
        Height = 13
        Caption = 'Gutter &Width:'
        FocusControl = inpGutterWidth
      end
      object Label30: TLabel
        Left = 8
        Top = 60
        Width = 55
        Height = 13
        Caption = '&Digit Count:'
        FocusControl = inpDigitCount
      end
      object Label31: TLabel
        Left = 240
        Top = 12
        Width = 52
        Height = 13
        Caption = 'L&eft Offset:'
        FocusControl = inpLeftOffset
      end
      object Label32: TLabel
        Left = 240
        Top = 36
        Width = 59
        Height = 13
        Caption = '&Right Offset:'
        FocusControl = inpRightOffset
      end
      object cbxGutterColor: TComboBox
        Left = 80
        Top = 9
        Width = 137
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 0
        OnChange = cbxGutterColorChange
      end
      object cbLineNumbers: TCheckBox
        Left = 392
        Top = 11
        Width = 97
        Height = 17
        Caption = 'Line &Numbers'
        TabOrder = 3
        OnClick = cbLineNumbersClick
      end
      object cbLeadingZeros: TCheckBox
        Left = 392
        Top = 35
        Width = 97
        Height = 17
        Caption = '&Leading Zeros'
        TabOrder = 4
        OnClick = cbLeadingZerosClick
      end
      object cbZeroStart: TCheckBox
        Left = 392
        Top = 59
        Width = 97
        Height = 17
        Caption = '&Zero Start'
        TabOrder = 5
        OnClick = cbZeroStartClick
      end
      object inpGutterWidth: TSpinEdit
        Left = 80
        Top = 33
        Width = 57
        Height = 22
        MaxValue = 100
        MinValue = 0
        TabOrder = 1
        Value = 2
        OnChange = inpGutterWidthChange
      end
      object inpDigitCount: TSpinEdit
        Left = 80
        Top = 57
        Width = 57
        Height = 22
        MaxLength = 2
        MaxValue = 99
        MinValue = 1
        TabOrder = 2
        Value = 4
        OnChange = inpDigitCountChange
      end
      object inpLeftOffset: TSpinEdit
        Left = 312
        Top = 9
        Width = 57
        Height = 22
        MaxValue = 100
        MinValue = 0
        TabOrder = 6
        Value = 16
        OnChange = inpLeftOffsetChange
      end
      object inpRightOffset: TSpinEdit
        Left = 312
        Top = 31
        Width = 57
        Height = 22
        MaxLength = 2
        MaxValue = 99
        MinValue = 1
        TabOrder = 7
        Value = 2
        OnChange = inpRightOffsetChange
      end
      object cbAutoSize: TCheckBox
        Left = 504
        Top = 11
        Width = 97
        Height = 17
        Caption = 'Auto &Size'
        TabOrder = 8
        OnClick = cbAutoSizeClick
      end
      object cbGutterVisible: TCheckBox
        Left = 504
        Top = 35
        Width = 97
        Height = 17
        Caption = '&Visible'
        TabOrder = 9
        OnClick = cbGutterVisibleClick
      end
      object cbUseFontStyle: TCheckBox
        Left = 504
        Top = 59
        Width = 97
        Height = 17
        Caption = '&Use font style'
        TabOrder = 10
        OnClick = cbUseFontStyleClick
      end
    end
    object tabBookmarks: TTabSheet
      Caption = 'Marks'
      object Label4: TLabel
        Left = 144
        Top = 12
        Width = 56
        Height = 13
        Caption = '&Left Margin:'
      end
      object SpeedButton1: TSpeedButton
        Tag = 10
        Left = 312
        Top = 56
        Width = 23
        Height = 22
        AllowAllUp = True
        GroupIndex = 1
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          0400000000008000000000000000000000001000000010000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
          DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
          DDDDDDDD999DDDDDDDDDDD9999999DDDDDDDD999999999DDDDDDD999999999DD
          DDDD99000000099DDDDD99000000099DDDDD99000000099DDDDDD999999999DD
          DDDDD999999999DDDDDDDD9999999DDDDDDDDDDD999DDDDDDDDD}
        OnClick = SpeedButtonClick
      end
      object SpeedButton2: TSpeedButton
        Tag = 11
        Left = 336
        Top = 56
        Width = 23
        Height = 22
        AllowAllUp = True
        GroupIndex = 1
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          0400000000008000000000000000000000001000000010000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
          DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD20DD200DDD
          02DDDDD2022220D02DDDDDDD00A22200DDDDDDDDD2A6220DDDDDDD22D2AA220D
          22DDDDD002AA22000DDDDDDDD2AA22DDDDDDDDDD002AA000DDDDDDD028220082
          0DDDDDD2D912019D2DDDDDDDDD1A21DDDDDDDDDDD0D22D0DDDDD}
        OnClick = SpeedButtonClick
      end
      object SpeedButton3: TSpeedButton
        Tag = 12
        Left = 360
        Top = 56
        Width = 23
        Height = 22
        AllowAllUp = True
        GroupIndex = 1
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          0400000000008000000000000000000000001000000010000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
          DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD3333333333DDDDD38BBBBBBBB
          83DDDD3BBBB00BBBB3DDDD3BBBBBBBBBB3DDDD38BBB00BBB83DDDDD3BBB00BBB
          3DDDDDD78BB00BB87DDDDDDD3BB00BB3DDDDDDDD78B00B87DDDDDDDDD3BBBB3D
          DDDDDDDDD37BB73DDDDDDDDDDD3333DDDDDDDDDDDDDDDDDDDDDD}
        OnClick = SpeedButtonClick
      end
      object SpeedButton4: TSpeedButton
        Tag = 13
        Left = 384
        Top = 56
        Width = 23
        Height = 22
        AllowAllUp = True
        GroupIndex = 1
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          0400000000008000000000000000000000001000000010000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
          DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD44DDD
          DDDDDDDDDDEC4DDDDDDDDDDDDDDDDDDDDDDDDDDDDDEC4DDDDDDDDDDDDDEC4DDD
          DDDDDDDDDDEC4DDDDDDDDDDDDDDEC4DDDDDDDDDDD44DEC4DDDDDDDDDEC4DEC4D
          DDDDDDDDEC44CC4DDDDDDDDDDECCC4DDDDDDDDDDDDEEEDDDDDDD}
        OnClick = SpeedButtonClick
      end
      object SpeedButton5: TSpeedButton
        Tag = 14
        Left = 408
        Top = 56
        Width = 23
        Height = 22
        AllowAllUp = True
        GroupIndex = 1
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          0400000000008000000000000000000000001000000010000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
          DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
          DDDDDDDDDDDDDCCEDDDDDDCDDDDDDDCCEDDDDDDCDDDDDDCCCEDDDDDDCCCCCCCC
          CCEDDDDCDDDDDDCCCEDDDDCDDDDDDDCCEDDDDDDDDDDDDCCEDDDDDDDDDDDDDDDD
          DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD}
        OnClick = SpeedButtonClick
      end
      object Label25: TLabel
        Left = 144
        Top = 36
        Width = 41
        Height = 13
        Caption = '&X Offset:'
      end
      object cbEnableKeys: TCheckBox
        Left = 8
        Top = 11
        Width = 97
        Height = 17
        Caption = 'Enable &Keys'
        TabOrder = 0
        OnClick = cbEnableKeysClick
      end
      object cbGlyphsVisible: TCheckBox
        Left = 8
        Top = 35
        Width = 97
        Height = 17
        Caption = '&Glyphs Visible'
        TabOrder = 1
        OnClick = cbGlyphsVisibleClick
      end
      object inpLeftMargin: TSpinEdit
        Left = 208
        Top = 9
        Width = 57
        Height = 22
        MaxValue = 100
        MinValue = 0
        TabOrder = 3
        Value = 2
        OnChange = inpLeftMarginChange
      end
      object cbInternalImages: TCheckBox
        Left = 8
        Top = 59
        Width = 97
        Height = 17
        Caption = '&Internal Images'
        TabOrder = 2
        OnClick = cbInternalImagesClick
      end
      object inpXOffset: TSpinEdit
        Left = 208
        Top = 33
        Width = 57
        Height = 22
        MaxValue = 100
        MinValue = 0
        TabOrder = 4
        Value = 12
        OnChange = inpXOffsetChange
      end
      object Label2: TMemo
        Left = 312
        Top = 8
        Width = 225
        Height = 45
        TabStop = False
        BorderStyle = bsNone
        Lines.Strings = (
          'Ctrl+Shift+<number> sets/moves/clears, '
          'Ctrl+<number> jumps.'
          'Click into gutter to set/clear gutter marks.')
        ParentColor = True
        ReadOnly = True
        TabOrder = 5
      end
    end
    object tabUndo: TTabSheet
      Caption = 'Undo'
      object Label11: TLabel
        Left = 8
        Top = 40
        Width = 52
        Height = 13
        Caption = 'Ma&x Undo:'
        FocusControl = inpMaxUndo
      end
      object Label19: TLabel
        Left = 144
        Top = 12
        Width = 374
        Height = 13
        Caption = 
          'Shortcut for Undo is Alt+Backspace. Shortcut for Redo is Alt+Shi' +
          'ft+Backspace.'
        WordWrap = True
      end
      object btnUndo: TButton
        Left = 8
        Top = 6
        Width = 57
        Height = 25
        Caption = '&Undo'
        Enabled = False
        TabOrder = 0
        OnClick = btnUndoClick
      end
      object inpMaxUndo: TSpinEdit
        Left = 72
        Top = 37
        Width = 57
        Height = 22
        MaxValue = 9999
        MinValue = 0
        TabOrder = 1
        Value = 10
        OnChange = inpMaxUndoChange
      end
      object btnRedo: TButton
        Left = 72
        Top = 6
        Width = 57
        Height = 25
        Caption = '&Redo'
        Enabled = False
        TabOrder = 2
        OnClick = btnRedoClick
      end
    end
    object tabHighlighter: TTabSheet
      Caption = 'Highlighter'
      object Label1: TLabel
        Left = 8
        Top = 38
        Width = 63
        Height = 13
        Caption = '&Use Settings:'
        FocusControl = cbxSettingsSelect
      end
      object Label3: TLabel
        Left = 8
        Top = 1
        Width = 53
        Height = 13
        Caption = '&Highlighter:'
        FocusControl = cbxHighlighterSelect
      end
      object Label23: TLabel
        Left = 328
        Top = 1
        Width = 84
        Height = 13
        Caption = '&Foreground Color:'
        FocusControl = cbxAttrForeground
      end
      object Label24: TLabel
        Left = 328
        Top = 38
        Width = 88
        Height = 13
        Caption = 'Bac&kground Color:'
        FocusControl = cbxAttrBackground
      end
      object Label22: TLabel
        Left = 224
        Top = 1
        Width = 42
        Height = 13
        Caption = 'A&ttribute:'
        FocusControl = cbxAttrSelect
      end
      object cbxHighlighterSelect: TComboBox
        Left = 8
        Top = 15
        Width = 105
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 0
        OnChange = cbxHighlighterSelectChange
      end
      object cbxSettingsSelect: TComboBox
        Left = 8
        Top = 52
        Width = 105
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 1
        OnChange = cbxSettingsSelectChange
      end
      object cbxAttrSelect: TComboBox
        Left = 224
        Top = 15
        Width = 97
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 5
        OnChange = cbxAttrSelectChange
        Items.Strings = (
          'Assembler'
          'Comment'
          'Identifier'
          'Number'
          'Operator'
          'Pragma'
          'Preprocessor'
          'Reserved word'
          'Symbol'
          'Space'
          'String'
          'Variable')
      end
      object cbxAttrBackground: TComboBox
        Left = 328
        Top = 52
        Width = 121
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 6
        OnChange = cbxAttrBackgroundChange
      end
      object cbxAttrForeground: TComboBox
        Left = 328
        Top = 15
        Width = 121
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 7
        OnChange = cbxAttrForegroundChange
      end
      object btnKeywords: TButton
        Left = 120
        Top = 54
        Width = 89
        Height = 19
        Caption = '&Reserved Words'
        TabOrder = 4
        OnClick = btnKeywordsClick
      end
      object grbAttrComments: TGroupBox
        Left = 457
        Top = 32
        Width = 161
        Height = 49
        Caption = ' Comments'
        TabOrder = 9
        object cbCommentsBas: TCheckBox
          Left = 104
          Top = 13
          Width = 41
          Height = 17
          Caption = 'B&as'
          TabOrder = 4
          OnClick = cbCommentsClick
        end
        object cbCommentsAsm: TCheckBox
          Left = 56
          Top = 27
          Width = 41
          Height = 17
          Caption = 'As&m'
          TabOrder = 3
          OnClick = cbCommentsClick
        end
        object cbCommentsPas: TCheckBox
          Left = 8
          Top = 27
          Width = 41
          Height = 17
          Caption = '&Pas'
          TabOrder = 1
          OnClick = cbCommentsClick
        end
        object cbCommentsAnsi: TCheckBox
          Left = 8
          Top = 13
          Width = 41
          Height = 17
          Caption = 'A&nsi'
          TabOrder = 0
          OnClick = cbCommentsClick
        end
        object cbCommentsC: TCheckBox
          Left = 56
          Top = 13
          Width = 41
          Height = 17
          Caption = '&C'
          TabOrder = 2
          OnClick = cbCommentsClick
        end
      end
      object grbAttrStyle: TGroupBox
        Left = 457
        Top = 0
        Width = 161
        Height = 33
        Caption = ' Style '
        TabOrder = 8
        object cbStyleBold: TCheckBox
          Left = 8
          Top = 13
          Width = 33
          Height = 17
          Caption = '&B'
          TabOrder = 0
          OnClick = cbFontStyleClick
        end
        object cbStyleStrikeOut: TCheckBox
          Left = 110
          Top = 13
          Width = 33
          Height = 17
          Caption = '&S'
          TabOrder = 3
          OnClick = cbFontStyleClick
        end
        object cbStyleUnderline: TCheckBox
          Left = 76
          Top = 13
          Width = 31
          Height = 17
          Caption = '&U'
          TabOrder = 2
          OnClick = cbFontStyleClick
        end
        object cbStyleItalic: TCheckBox
          Left = 42
          Top = 13
          Width = 28
          Height = 17
          Caption = '&I'
          TabOrder = 1
          OnClick = cbFontStyleClick
        end
      end
      object btnSaveToReg: TButton
        Left = 120
        Top = 16
        Width = 89
        Height = 19
        Caption = 'Sa&ve to Reg'
        TabOrder = 2
        OnClick = btnSaveToRegClick
      end
      object btnLoadFromReg: TButton
        Left = 120
        Top = 35
        Width = 89
        Height = 19
        Caption = '&Load from Reg'
        TabOrder = 3
        OnClick = btnLoadFromRegClick
      end
    end
    object tabExporter: TTabSheet
      Caption = 'Export'
      object Label27: TLabel
        Left = 8
        Top = 31
        Width = 42
        Height = 13
        Caption = '&Exporter:'
        FocusControl = cbxExporterSelect
      end
      object cbxExporterSelect: TComboBox
        Left = 8
        Top = 50
        Width = 136
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 1
        OnChange = cbxExporterSelectChange
      end
      object cbExportSelected: TCheckBox
        Left = 8
        Top = 11
        Width = 121
        Height = 17
        Caption = 'E&xport selected only'
        TabOrder = 0
      end
      object btnExportToFile: TButton
        Left = 160
        Top = 14
        Width = 110
        Height = 24
        Caption = 'Export to &file...'
        TabOrder = 2
        OnClick = btnExportToFileClick
      end
      object btnExportToClipboard: TButton
        Left = 160
        Top = 48
        Width = 110
        Height = 24
        Caption = 'Export to clipboard'
        TabOrder = 3
        OnClick = btnExportToClipboardClick
      end
    end
    object tabInfo: TTabSheet
      Caption = 'Info'
      object Label9: TLabel
        Left = 8
        Top = 12
        Width = 46
        Height = 13
        Caption = 'Left &Char:'
        FocusControl = inpLeftChar
      end
      object Label10: TLabel
        Left = 8
        Top = 60
        Width = 47
        Height = 13
        Caption = 'Line &Text:'
        FocusControl = inpLineText
      end
      object Label17: TLabel
        Left = 192
        Top = 12
        Width = 38
        Height = 13
        Caption = 'Caret &X:'
        FocusControl = inpCaretX
      end
      object Label18: TLabel
        Left = 192
        Top = 36
        Width = 38
        Height = 13
        Caption = 'Caret &Y:'
        FocusControl = inpCaretY
      end
      object Label20: TLabel
        Left = 8
        Top = 36
        Width = 45
        Height = 13
        Caption = 'Top &Line:'
        FocusControl = inpTopLine
      end
      object Label21: TLabel
        Left = 376
        Top = 12
        Width = 54
        Height = 13
        Caption = 'Line Count:'
      end
      object inpLineText: TEdit
        Left = 56
        Top = 57
        Width = 457
        Height = 21
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 2
      end
      object outLineCount: TEdit
        Left = 440
        Top = 9
        Width = 73
        Height = 21
        ParentColor = True
        ReadOnly = True
        TabOrder = 5
      end
      object inpLeftChar: TSpinEdit
        Left = 56
        Top = 9
        Width = 65
        Height = 22
        MaxValue = 1024
        MinValue = 0
        TabOrder = 0
        Value = 0
        OnChange = inpLeftCharChange
      end
      object inpTopLine: TSpinEdit
        Left = 56
        Top = 33
        Width = 65
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 1
        Value = 0
        OnChange = inpTopLineChange
      end
      object inpCaretX: TSpinEdit
        Left = 232
        Top = 9
        Width = 65
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 3
        Value = 0
        OnChange = inpCaretXChange
      end
      object inpCaretY: TSpinEdit
        Left = 232
        Top = 33
        Width = 65
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 4
        Value = 0
        OnChange = inpCaretYChange
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'Keyboard'
      object KeyCmdList: TListView
        Left = 8
        Top = 2
        Width = 254
        Height = 80
        Columns = <
          item
            Caption = 'Command'
            Width = 0
          end
          item
            Caption = 'Keystroke'
            Width = 0
          end>
        ColumnClick = False
        HideSelection = False
        ReadOnly = True
        TabOrder = 0
        ViewStyle = vsReport
      end
      object btnEdit: TButton
        Left = 268
        Top = 6
        Width = 75
        Height = 25
        Caption = '&Edit'
        TabOrder = 1
        OnClick = btnEditClick
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Completion'
      ImageIndex = 14
      object Label34: TLabel
        Left = 8
        Top = 45
        Width = 42
        Height = 13
        Caption = 'A&ttribute:'
        FocusControl = cbCompletionAttr
      end
      object Label35: TLabel
        Left = 112
        Top = 45
        Width = 27
        Height = 13
        Caption = '&Color:'
        FocusControl = cbxCompletionColor
      end
      object Memo2: TMemo
        Left = 8
        Top = 4
        Width = 269
        Height = 21
        BorderStyle = bsNone
        Lines.Strings = (
          'Press Ctrl+Space for Completion Proposal demo.')
        ParentColor = True
        TabOrder = 0
      end
      object Memo3: TMemo
        Left = 320
        Top = 4
        Width = 273
        Height = 57
        BorderStyle = bsNone
        Lines.Strings = (
          'Type "bg" (without quotes) and press Shift+Space for '
          'Auto Complete demo.'
          'Type "findfirst" (without quotes) and press Shift+Space '
          'for another Auto Complete demo.')
        ParentColor = True
        TabOrder = 1
      end
      object cbShrinkList: TCheckBox
        Left = 8
        Top = 24
        Width = 193
        Height = 17
        Caption = '&Shrink List (like Delphi 6)'
        Checked = True
        State = cbChecked
        TabOrder = 2
        OnClick = cbShrinkListClick
      end
      object cbCompletionAttr: TComboBox
        Left = 8
        Top = 59
        Width = 97
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 3
        OnChange = cbCompletionAttrChange
        Items.Strings = (
          'Background'
          'Text'
          'Selected'
          'SelectedText')
      end
      object cbxCompletionColor: TComboBox
        Left = 112
        Top = 59
        Width = 121
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 4
        OnChange = cbxCompletionColorChange
      end
    end
    object tabEvents: TTabSheet
      Caption = 'Events'
      object cbEnableEventLog: TCheckBox
        Left = 8
        Top = 11
        Width = 128
        Height = 17
        Caption = 'Enable &Event Logging'
        TabOrder = 0
        OnClick = cbEnableEventLogClick
      end
      object lbEventLog: TListBox
        Left = 144
        Top = 8
        Width = 225
        Height = 72
        ItemHeight = 13
        TabOrder = 2
      end
      object cbMouse: TCheckBox
        Left = 392
        Top = 8
        Width = 97
        Height = 17
        Caption = '&Mouse events'
        TabOrder = 3
      end
      object cbDrag: TCheckBox
        Left = 392
        Top = 40
        Width = 145
        Height = 17
        Caption = '&Drag and drop events'
        Checked = True
        State = cbChecked
        TabOrder = 5
      end
      object cbKeyboard: TCheckBox
        Left = 392
        Top = 24
        Width = 193
        Height = 17
        Caption = '&Keyboard and Command events'
        Checked = True
        State = cbChecked
        TabOrder = 4
      end
      object cbOther: TCheckBox
        Left = 392
        Top = 56
        Width = 97
        Height = 17
        Caption = '&Other events'
        Checked = True
        State = cbChecked
        TabOrder = 6
      end
      object btnClear: TButton
        Left = 8
        Top = 55
        Width = 75
        Height = 25
        Caption = '&Clear'
        TabOrder = 1
        OnClick = btnClearClick
      end
    end
    object tabAbout: TTabSheet
      Caption = 'About'
      object Label33: TLabel
        Left = 8
        Top = 7
        Width = 172
        Height = 24
        Caption = 'SynEdit demo app'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clMaroon
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Memo1: TMemo
        Left = 8
        Top = 44
        Width = 593
        Height = 29
        BorderStyle = bsNone
        Lines.Strings = (
          'Written by Primoz Gabrijelcic, primoz.gabrijelcic@altavista.net.'
          
            'Contributors: Carlos Wijders, Brad Stowers, Heedong Lim, Michael' +
            ' Hieke.')
        ParentColor = True
        TabOrder = 0
      end
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 387
    Width = 677
    Height = 19
    Panels = <
      item
        Width = 64
      end
      item
        Alignment = taCenter
        Width = 60
      end
      item
        Alignment = taCenter
        Width = 64
      end
      item
        Bevel = pbNone
        Width = 50
      end>
    SimplePanel = False
  end
  object SynEditor: TSynEdit
    Left = 0
    Top = 113
    Width = 677
    Height = 274
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 2
    OnClick = SynEditorClick
    OnDblClick = SynEditorDblClick
    OnDragDrop = SynEditorDragDrop
    OnDragOver = SynEditorDragOver
    OnEndDrag = SynEditorEndDrag
    OnEnter = SynEditorEnter
    OnExit = SynEditorExit
    OnKeyDown = SynEditorKeyDown
    OnKeyPress = SynEditorKeyPress
    OnKeyUp = SynEditorKeyUp
    OnMouseDown = SynEditorMouseDown
    OnMouseMove = SynEditorMouseMove
    OnMouseUp = SynEditorMouseUp
    OnStartDrag = SynEditorStartDrag
    BookMarkOptions.BookmarkImages = ImageList1
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Terminal'
    Gutter.Font.Style = []
    Options = [eoAutoIndent, eoDragDropEditing, eoDropFiles, eoScrollPastEol, eoShowScrollHint, eoSmartTabs, eoTabsToSpaces]
    OnChange = SynEditorChange
    OnCommandProcessed = SynEditorCommandProcessed
    OnContextHelp = SynEditorContextHelp
    OnDropFiles = SynEditorDropFiles
    OnGutterClick = SynEditorGutterClick
    OnPaint = SynEditorPaint
    OnPlaceBookmark = SynEditorPlaceBookmark
    OnProcessCommand = SynEditorProcessCommand
    OnProcessUserCommand = SynEditorProcessUserCommand
    OnReplaceText = SynEditorReplaceText
    OnStatusChange = SynEditorStatusChange
    RemovedKeystrokes = <
      item
        Command = ecDeleteLastChar
        ShortCut = 8200
      end
      item
        Command = ecLineBreak
        ShortCut = 8205
      end
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
  object OpenDialog1: TOpenDialog
    Left = 40
    Top = 132
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MinFontSize = 0
    MaxFontSize = 0
    Options = [fdEffects, fdFixedPitchOnly]
    Left = 72
    Top = 132
  end
  object ImageList1: TImageList
    Left = 104
    Top = 132
    Bitmap = {
      494C01010F001300040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000004000000001002000000000000040
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000080
      8000008080000080800000808000008080000080800000808000008080000080
      8000008080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000808000C0C0
      C00000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF00C0C0C0000080800000000000000000000000000000000000000000000000
      0000000000000000000000000000800000008000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000080800000FF
      FF0000FFFF0000FFFF0000FFFF00000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF000080800000000000000000000000000000000000000000000000
      00000000000000000000FFFF0000FF0000008000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000FF000000FFFF
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000080800000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF000080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF0000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      0000FFFF00000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000808000C0C0
      C00000FFFF0000FFFF0000FFFF00000000000000000000FFFF0000FFFF0000FF
      FF00C0C0C0000080800000000000000000000000000000000000000000000000
      00000000000000000000FFFF0000FF0000008000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF00
      0000000000000000000000000000000000000000000000000000FF000000FF00
      0000FF000000FFFF000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000080
      800000FFFF0000FFFF0000FFFF00000000000000000000FFFF0000FFFF0000FF
      FF00008080000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFF0000FF0000008000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      0000FF000000FF000000FFFF0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000C0C0C00000FFFF0000FFFF00000000000000000000FFFF0000FFFF00C0C0
      C000808080000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFF0000FF0000008000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF00
      0000000000000000000000000000000000000000000000000000FF000000FF00
      0000FF000000FFFF000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000080800000FFFF0000FFFF00000000000000000000FFFF0000FFFF000080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFF0000FF00000080000000000000000000
      0000000000000000000000000000000000000000000000000000FF0000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      0000FFFF00000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000080808000C0C0C00000FFFF00000000000000000000FFFF00C0C0C0008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000008000000080000000C0C0C000FFFF0000FF000000800000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000FF000000FFFF
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000080800000FFFF0000FFFF0000FFFF0000FFFF00008080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFF0000FF00000080000000C0C0C000FFFF0000FF000000800000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000008080008080800000FFFF0000FFFF0080808000008080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFF0000FF0000008000000080000000FF000000FF000000800000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000808000008080000080800000808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFF0000FF000000FF000000FF00000080000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFF0000FFFF0000FFFF000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF0000000000
      00000000000000000000000000000000000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF0000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000C0C0C0008080
      0000008000008080000000800000808000000080000080800000FF000000FF00
      00000000000000000000000000000000000000000000FF000000C0C0C0008080
      0000008000008080000000800000808000000080000080800000FF000000FF00
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000C0C0C0000080
      00008080000000FFFF0000FFFF0000FFFF008080000000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      00008080000000800000808000000080000000FFFF0000800000FF0000008080
      8000FF0000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000C0C0C0008080
      000000FFFF0080800000008000008080000000FFFF0080800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0008080
      00000080000080800000008000008080000000FFFF0080800000FF0000008080
      8000FF0000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008000000000
      0000000000000000000000800000000000000000000000000000000000000000
      00000000000000800000000000000000000000000000FF000000C0C0C0000080
      000000FFFF0000800000808000000080000000FFFF0000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      00008080000000800000808000000080000000FFFF0000800000FF0000008080
      8000FF0000000000000000000000000000000000000000000000000000000000
      00000000FF000000FF000000FF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000080
      0000000000000080000000800000008000000080000000000000000000000000
      00000080000000000000000000000000000000000000FF000000C0C0C0008080
      000000FFFF0080800000008000008080000000FFFF0080800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0008080
      00000080000000FFFF0000FFFF008080000000FFFF0080800000FF0000008080
      8000FF00000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000FF0000008000000080000000800000000000000000
      00000000000000000000000000000000000000000000FF000000C0C0C0008080
      00008080000000FFFF0000FFFF0000FFFF008080000000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      000000FFFF00008000008080000000FFFF0000FFFF0000800000FF0000008080
      8000FF000000000000000000000000000000000000000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000080000000FF0000808000000080000000800000000000000000
      00000000000000000000000000000000000000000000FF000000C0C0C0008080
      000000FFFF0080800000008000008080000000FFFF0080800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0008080
      000000FFFF0080800000008000008080000000FFFF0080800000FF0000008080
      8000FF000000000000000000000000000000000000000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF00000000000000
      0000000000000000000000000000000000000000000000000000008000000080
      0000000000000080000000FF000000FF00000080000000800000000000000000
      00000080000000800000000000000000000000000000FF000000C0C0C0000080
      000000FFFF0000800000808000000080000000FFFF0000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      000000FFFF0000800000808000000080000000FFFF0000800000FF0000008080
      8000FF0000000000000000000000000000000000FF000000FF00000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000080000000FF000000FF00000080000000800000000000000000
      00000000000000000000000000000000000000000000FF000000C0C0C0008080
      00000080000000FFFF0000FFFF0000FFFF000080000080800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0008080
      00000080000000FFFF0000FFFF0000FFFF000080000080800000FF0000008080
      8000FF0000000000000000000000000000000000FF000000FF00000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000080000000FF000000FF00000080000000800000000000000000
      00000000000000000000000000000000000000000000FF000000C0C0C0000080
      0000808000000080000080800000008000008080000000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      0000808000000080000080800000008000008080000000800000FF0000008080
      8000FF0000000000000000000000000000000000FF000000FF00000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000080000000FF000000FF000000000000000000000000
      00000000000000000000000000000000000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF0000008080
      8000FF000000000000000000000000000000000000000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000800000C0C0C00000800000008000000000000000000000C0C0C0000080
      0000000000000000000000000000000000000000000000000000FF000000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C0008080
      8000FF0000000000000000000000000000000000000000000000FF000000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C0008080
      8000FF000000000000000000000000000000000000000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000080
      0000000000000000FF00000080000080000000000000000080000000FF000000
      000000800000000000000000000000000000000000000000000000000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      000000000000000000000000000000000000000000000000000000000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000800000FF00000080000000008000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF000000FF000000FF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000008000000080000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF0000000000
      00000000000000000000000000000000000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF0000000000
      00000000000000000000000000000000000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF0000000000
      00000000000000000000000000000000000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF0000000000
      00000000000000000000000000000000000000000000FF000000C0C0C0008080
      0000008000008080000000800000808000000080000080800000FF000000FF00
      00000000000000000000000000000000000000000000FF000000C0C0C0008080
      0000008000008080000000800000808000000080000080800000FF000000FF00
      00000000000000000000000000000000000000000000FF000000C0C0C0008080
      0000008000008080000000800000808000000080000080800000FF000000FF00
      00000000000000000000000000000000000000000000FF000000C0C0C0008080
      0000008000008080000000800000808000000080000080800000FF000000FF00
      00000000000000000000000000000000000000000000FF000000C0C0C0000080
      000080800000008000008080000000FFFF008080000000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      00008080000000FFFF0000FFFF0000FFFF008080000000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      00008080000000FFFF0000FFFF0000FFFF008080000000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      0000808000000080000000FFFF00008000008080000000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0008080
      000000800000808000000080000000FFFF000080000080800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0008080
      000000FFFF0080800000008000008080000000FFFF0080800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0008080
      000000FFFF0080800000008000008080000000FFFF0080800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0008080
      0000008000008080000000FFFF00808000000080000080800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      000080800000008000008080000000FFFF008080000000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      00008080000000800000808000000080000000FFFF0000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      000000FFFF0000800000808000000080000000FFFF0000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      0000808000000080000000FFFF00008000008080000000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0008080
      000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0080800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0008080
      00000080000080800000008000008080000000FFFF0080800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0008080
      000000FFFF0080800000008000008080000000FFFF0080800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0008080
      000000800000808000000080000000FFFF000080000080800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      000000FFFF00008000008080000000FFFF008080000000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      000000FFFF0000FFFF0000FFFF0000FFFF008080000000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      000000FFFF0000FFFF0000FFFF0000FFFF008080000000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      000080800000008000008080000000FFFF008080000000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0008080
      000000FFFF00808000000080000000FFFF000080000080800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0008080
      000000FFFF008080000000800000808000000080000080800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0008080
      000000FFFF008080000000800000808000000080000080800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0008080
      00000080000080800000008000008080000000FFFF0080800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      000000FFFF00008000008080000000FFFF008080000000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      000000FFFF000080000080800000008000008080000000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      000000FFFF0000800000808000000080000000FFFF0000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      000000FFFF0000800000808000000080000000FFFF0000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0008080
      000000FFFF00808000000080000000FFFF000080000080800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0008080
      000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0080800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0008080
      00000080000000FFFF0000FFFF0000FFFF000080000080800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0008080
      000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0080800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      0000808000000080000080800000008000008080000000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      0000808000000080000080800000008000008080000000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      0000808000000080000080800000008000008080000000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      0000808000000080000080800000008000008080000000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF0000008080
      8000FF0000000000000000000000000000000000000000000000FF000000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C0008080
      8000FF0000000000000000000000000000000000000000000000FF000000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C0008080
      8000FF0000000000000000000000000000000000000000000000FF000000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C0008080
      8000FF0000000000000000000000000000000000000000000000FF000000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C0008080
      8000FF000000000000000000000000000000000000000000000000000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      000000000000000000000000000000000000000000000000000000000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      000000000000000000000000000000000000000000000000000000000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      000000000000000000000000000000000000000000000000000000000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF0000000000
      00000000000000000000000000000000000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF0000000000
      00000000000000000000000000000000000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF0000000000
      00000000000000000000000000000000000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF0000000000
      00000000000000000000000000000000000000000000FF000000C0C0C0008080
      0000008000008080000000800000808000000080000080800000FF000000FF00
      00000000000000000000000000000000000000000000FF000000C0C0C0008080
      0000008000008080000000800000808000000080000080800000FF000000FF00
      00000000000000000000000000000000000000000000FF000000C0C0C0008080
      0000008000008080000000800000808000000080000080800000FF000000FF00
      00000000000000000000000000000000000000000000FF000000C0C0C0008080
      0000008000008080000000800000808000000080000080800000FF000000FF00
      00000000000000000000000000000000000000000000FF000000C0C0C0000080
      00008080000000FFFF0000FFFF0000FFFF008080000000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      00008080000000FFFF0000FFFF0000FFFF008080000000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0008080
      000000FFFF0080800000008000008080000000FFFF0080800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0008080
      0000008000008080000000FFFF00808000000080000080800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0008080
      000000FFFF008080000000800000808000000080000080800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0008080
      000000FFFF0080800000008000008080000000FFFF0080800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      000000FFFF0000800000808000000080000000FFFF0000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      0000808000000080000000FFFF00008000008080000000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      00008080000000FFFF0080800000008000008080000000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      00008080000000800000808000000080000000FFFF0000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0008080
      000000FFFF0080800000008000008080000000FFFF0080800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0008080
      0000008000008080000000FFFF00808000000080000080800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0008080
      0000008000008080000000FFFF00808000000080000080800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0008080
      00000080000080800000008000008080000000FFFF0080800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      000000FFFF0000800000808000000080000000FFFF0000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      0000808000000080000000FFFF00008000008080000000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      000080800000008000008080000000FFFF008080000000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      0000808000000080000000FFFF0000FFFF008080000000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0008080
      000000FFFF0080800000008000008080000000FFFF0080800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0008080
      000000FFFF008080000000FFFF00808000000080000080800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0008080
      00000080000080800000008000008080000000FFFF0080800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0008080
      00000080000080800000008000008080000000FFFF0080800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      000000FFFF0000800000808000000080000000FFFF0000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      00008080000000FFFF0000FFFF00008000008080000000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      000000FFFF0000800000808000000080000000FFFF0000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      000000FFFF0000800000808000000080000000FFFF0000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0008080
      00000080000000FFFF0000FFFF0000FFFF000080000080800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0008080
      0000008000008080000000FFFF00808000000080000080800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0008080
      00000080000000FFFF0000FFFF0000FFFF000080000080800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0008080
      00000080000000FFFF0000FFFF0000FFFF000080000080800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      0000808000000080000080800000008000008080000000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      0000808000000080000080800000008000008080000000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      0000808000000080000080800000008000008080000000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000C0C0C0000080
      0000808000000080000080800000008000008080000000800000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF0000008080
      8000FF00000000000000000000000000000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF0000008080
      8000FF0000000000000000000000000000000000000000000000FF000000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C0008080
      8000FF0000000000000000000000000000000000000000000000FF000000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C0008080
      8000FF0000000000000000000000000000000000000000000000FF000000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C0008080
      8000FF0000000000000000000000000000000000000000000000FF000000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C0008080
      8000FF000000000000000000000000000000000000000000000000000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      000000000000000000000000000000000000000000000000000000000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      000000000000000000000000000000000000000000000000000000000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      000000000000000000000000000000000000000000000000000000000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000400000000100010000000000000200000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFFFFFF0000FFFFFFFFFFFF0000
      FFFFFFFFFFFF0000E007FFFFFFFF0000C003FE7FFFFF0000C003FC7FFF8F0000
      C003FFFFDFC70000C003FC7FEFC30000E007FC7FF0010000E007FC7FEFC30000
      F00FFE3FDFC70000F00FF81FFF8F0000F81FF01FFFFF0000F81FF01FFFFF0000
      FC3FF83FFFFF0000FFFFFC7FFFFF0000FFFFFFFFFFFFFFFF801F801FFFFFFFFF
      800F800FFFFFFFFF80078007FFFFFFFF80078007FFFFCC7380078007F1FFE027
      80078007C07FF00F80078007803FF81F80078007803FC81380078007001FE007
      80078007001FF83F80078007001FF00F80078007803FE007C007C007803FE817
      E00FE00FC07FFC3FFFFFFFFFF1FFFA5FFFFFFFFFFFFFFFFF801F801F801F801F
      800F800F800F800F800780078007800780078007800780078007800780078007
      8007800780078007800780078007800780078007800780078007800780078007
      800780078007800780078007800780078007800780078007C007C007C007C007
      E00FE00FE00FE00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF801F801F801F801F
      800F800F800F800F800780078007800780078007800780078007800780078007
      8007800780078007800780078007800780078007800780078007800780078007
      800780078007800780078007800780078007800780078007C007C007C007C007
      E00FE00FE00FE00FFFFFFFFFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
  object FindDialog1: TFindDialog
    OnFind = DoFindText
    Left = 136
    Top = 132
  end
  object ReplaceDialog1: TReplaceDialog
    OnFind = DoFindText
    OnReplace = DoReplaceText
    Left = 168
    Top = 132
  end
  object SaveDialog1: TSaveDialog
    Title = 'Export to'
    Left = 200
    Top = 132
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
    UseBackground = False
    Left = 300
    Top = 164
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
    UseBackground = False
    Left = 332
    Top = 164
  end
  object SynCompletionProposal1: TSynCompletionProposal
    Options = [scoLimitToMatchedText, scoEndCharCompletion]
    ItemList.Strings = (
      'SynEdit'
      'SynCompletionProposal demo'
      ''
      'Select line, then press Enter.'
      'Line will be copied to the editor.')
    NbLinesInWindow = 6
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
    Editor = SynEditor
    Left = 392
    Top = 164
  end
  object SynAutoComplete1: TSynAutoComplete
    AutoCompleteList.Strings = (
      'bg'
      '=begin'
      '=  |'
      '=end;'
      'findfirst'
      '=err := FindFirst('#39'*.*'#39',0,S);'
      '=if err = 0 then begin'
      '=  repeat'
      '=    |'
      '=    err := FindNext(S);'
      '=  until err <> 0;'
      '=  FindClose(S);'
      '=end;')
    EndOfTokenChr = '()[].'
    Editor = SynEditor
    ShortCut = 8224
    Options = []
    Left = 428
    Top = 164
  end
  object SynHC11Syn1: TSynHC11Syn
    DefaultFilter = '68HC11 Assembler files (*.hc11,*.asm,*.asc)|*.HC11;*.ASM;*.ASC'
    Left = 72
    Top = 240
  end
  object SynADSP21xxSyn1: TSynADSP21xxSyn
    DefaultFilter = 'DSP files (*.dsp,*.inc)|*.DSP;*.INC'
    Left = 104
    Top = 240
  end
  object SynAWKSyn1: TSynAWKSyn
    DefaultFilter = 'AWK Script (*.awk)|*.awk'
    Left = 136
    Top = 240
  end
  object SynBaanSyn1: TSynBaanSyn
    DefaultFilter = 'Baan 4GL files (*.cln)|*.cln'
    Left = 168
    Top = 240
  end
  object SynCppSyn1: TSynCppSyn
    DefaultFilter = 'C++ files (*.cpp,*.h,*.hpp)|*.cpp;*.h;*.hpp'
    Left = 200
    Top = 240
  end
  object SynCacheSyn1: TSynCacheSyn
    DefaultFilter = 'Cache files (*.mac,*.inc,*.int)|*.mac;*.inc;*.int'
    Left = 232
    Top = 240
  end
  object SynCACSyn1: TSynCACSyn
    DefaultFilter = 'CA-Clipper files (*.prg, *.ch, *.inc)|*.prg;*.ch;*.inc'
    Left = 264
    Top = 240
  end
  object SynCssSyn1: TSynCssSyn
    Left = 296
    Top = 240
  end
  object SynDfmSyn1: TSynDfmSyn
    DefaultFilter = 'Delphi/C++ Builder Form Files (*.dfm)|*.dfm'
    Left = 328
    Top = 240
  end
  object SynFortranSyn1: TSynFortranSyn
    Left = 360
    Top = 240
  end
  object SynFoxproSyn1: TSynFoxproSyn
    Left = 392
    Top = 240
  end
  object SynGalaxySyn1: TSynGalaxySyn
    DefaultFilter = 'Galaxy files (*.gtv,*.galrep,*.txt)|*.gtv;*.galrep;*.txt'
    KeyWords.Strings = (
      '#END'
      '#GALAXY'
      'A'
      'ANONYMOUS'
      'AUTOUNLOAD'
      'B'
      'BATTLEPROTOCOL'
      'C'
      'CAP'
      'CARGO'
      'COL'
      'COMPRESS'
      'D'
      'DRIVE'
      'E'
      'EMP'
      'F'
      'FLEET'
      'FLEETTABLES'
      'G'
      'GALAXYTV'
      'GPLUS'
      'GROUPFORECAST'
      'H'
      'I'
      'J'
      'L'
      'M'
      'MACHINEREPORT'
      'MAT'
      'N'
      'NAMECASE'
      'NO'
      'O'
      'OPTIONS'
      'P'
      'PLANETFORECAST'
      'PRODTABLE'
      'PRODUCE'
      'Q'
      'R'
      'ROUTESFORECAST'
      'S'
      'SEND'
      'SHIELDS'
      'SHIPTYPEFORECAST'
      'SORTGROUPS'
      'T'
      'TWOCOL'
      'U'
      'UNDERSCORES'
      'V'
      'W'
      'WAR'
      'WEAPONS'
      'X'
      'Y'
      'Z')
    Left = 424
    Top = 240
  end
  object SynDmlSyn1: TSynDmlSyn
    DefaultFilter = 'GEMBASE files (*.dml,*.gem)|*.DML;*.GEM'
    Left = 456
    Top = 240
  end
  object SynGeneralSyn1: TSynGeneralSyn
    Comments = []
    DetectPreprocessor = False
    IdentifierChars = '_0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    Left = 520
    Top = 240
  end
  object SynHTMLSyn1: TSynHTMLSyn
    DefaultFilter = 'HTML Document (*.htm,*.html)|*.htm;*.html'
    Left = 552
    Top = 240
  end
  object SynIniSyn1: TSynIniSyn
    Left = 584
    Top = 240
  end
  object SynInnoSyn1: TSynInnoSyn
    DefaultFilter = 'Inno Setup Script files (*.iss)|*.iss'
    Left = 40
    Top = 272
  end
  object SynJavaSyn1: TSynJavaSyn
    DefaultFilter = 'Java files (*.java)|*.java'
    Left = 72
    Top = 272
  end
  object SynJScriptSyn1: TSynJScriptSyn
    DefaultFilter = 'Javascript files (*.js)|*.js'
    Left = 104
    Top = 272
  end
  object SynKixSyn1: TSynKixSyn
    DefaultFilter = 'Kix Scripts (*.kix)|*.kix'
    Left = 136
    Top = 272
  end
  object SynVBScriptSyn1: TSynVBScriptSyn
    DefaultFilter = 'VBScript files (*.vbs)|*.vbs'
    Left = 168
    Top = 272
  end
  object SynBatSyn1: TSynBatSyn
    DefaultFilter = 'MS-DOS Batch Files (*.bat)|*.bat'
    Left = 200
    Top = 272
  end
  object SynPerlSyn1: TSynPerlSyn
    DefaultFilter = 'Perl files (*.pl,*.pm,*.cgi)|*.pl;*.pm;*.cgi'
    Left = 232
    Top = 272
  end
  object SynPHPSyn1: TSynPHPSyn
    DefaultFilter = 
      'PHP files (*.php,*.php3,*.phtml,*.inc)|*.php;*.php3;*.phtml;*.in' +
      'c'
    Left = 264
    Top = 272
  end
  object SynProgressSyn1: TSynProgressSyn
    DefaultFilter = 'Progress Files (*.w,*.p,*.i)|*.w;*.p;*.i'
    Left = 296
    Top = 272
  end
  object SynPythonSyn1: TSynPythonSyn
    DefaultFilter = 'Python files (*.py)|*.py'
    Left = 328
    Top = 272
  end
  object SynSQLSyn1: TSynSQLSyn
    DefaultFilter = 'SQL files (*.sql)|*.sql'
    SQLDialect = sqlSybase
    Left = 360
    Top = 272
  end
  object SynSMLSyn1: TSynSMLSyn
    Left = 392
    Top = 272
  end
  object SynTclTkSyn1: TSynTclTkSyn
    DefaultFilter = 'Tcl/Tk files (*.tcl)|*.tcl'
    Left = 424
    Top = 272
  end
  object SynVBSyn1: TSynVBSyn
    DefaultFilter = 'Visual Basic files (*.bas)|*.bas'
    Left = 456
    Top = 272
  end
  object SynAsmSyn1: TSynAsmSyn
    Left = 488
    Top = 272
  end
  object SynEditPythonBehaviour1: TSynEditPythonBehaviour
    Left = 328
    Top = 304
  end
  object SynModelicaSyn1: TSynModelicaSyn
    DefaultFilter = 'Modelica files (*.mo)|*.mo'
    Left = 520
    Top = 272
  end
  object SynM3Syn1: TSynM3Syn
    DefaultFilter = 'Modula-3 files (*.m3)|*.m3'
    Left = 552
    Top = 272
  end
  object SynGWScriptSyn1: TSynGWScriptSyn
    DefaultFilter = 'GW-TEL Script Files (*.gws)|*.gws'
    Left = 584
    Top = 272
  end
  object SynCPMSyn1: TSynCPMSyn
    Left = 40
    Top = 240
  end
  object SynIdlSyn1: TSynIdlSyn
    Left = 488
    Top = 240
  end
  object SynHP48Syn1: TSynHP48Syn
    DefaultFilter = 'HP48 Files (*.s,*.sou,*.a,*.hp)|*.s;*.sou;*.a;*.hp'
    BaseRange = rsRpl
    Left = 616
    Top = 240
  end
  object SynPasSyn1: TSynPasSyn
    Left = 616
    Top = 272
  end
  object SynSDDSyn1: TSynSDDSyn
    Left = 40
    Top = 304
  end
  object SynMsgSyn1: TSynMsgSyn
    Left = 72
    Top = 304
  end
  object SynUnrealSyn1: TSynUnrealSyn
    Left = 104
    Top = 304
  end
  object SynXMLSyn1: TSynXMLSyn
    DefaultFilter = 
      'XML Document (*.xml,*.xsd,*.xsl,*.xslt,*.dtd)|*.xml;*.xsd;*.xsl;' +
      '*.xslt;*.dtd'
    WantBracesParsed = False
    Left = 136
    Top = 304
  end
  object SynSTSyn1: TSynSTSyn
    DefaultFilter = 'Pascal Files (*.pas,*.dpr,*.dpk,*.inc)|*.pas;*.dpr;*.dpk;*.inc'
    Left = 168
    Top = 304
  end
end
