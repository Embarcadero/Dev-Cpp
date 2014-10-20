object EditorOptForm: TEditorOptForm
  Left = 879
  Top = 375
  BorderStyle = bsDialog
  Caption = 'Editor Options'
  ClientHeight = 492
  ClientWidth = 484
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    484
    492)
  PixelsPerInch = 96
  TextHeight = 15
  object PagesMain: TPageControl
    Left = 0
    Top = 0
    Width = 485
    Height = 455
    ActivePage = tabGeneral
    TabOrder = 0
    OnChange = PagesMainChange
    object tabGeneral: TTabSheet
      Caption = 'General'
      object grpMargin: TGroupBox
        Left = 264
        Top = 220
        Width = 201
        Height = 90
        Caption = '  Right Margin  '
        TabOrder = 2
        object lblMarginWidth: TLabel
          Left = 8
          Top = 43
          Width = 32
          Height = 15
          Caption = 'Width'
        end
        object lblMarginColor: TLabel
          Left = 99
          Top = 43
          Width = 29
          Height = 15
          Caption = 'Color'
        end
        object cbMarginVis: TCheckBox
          Left = 8
          Top = 20
          Width = 185
          Height = 17
          Caption = 'Visible'
          TabOrder = 0
        end
        object edMarginWidth: TSpinEdit
          Left = 16
          Top = 59
          Width = 60
          Height = 24
          MaxValue = 0
          MinValue = 0
          TabOrder = 1
          Value = 0
        end
        object cpMarginColor: TColorBox
          Left = 96
          Top = 58
          Width = 97
          Height = 22
          DefaultColorColor = cl3DLight
          Style = [cbStandardColors, cbCustomColor, cbPrettyNames]
          ItemHeight = 16
          TabOrder = 2
        end
      end
      object grpEditorOpts: TGroupBox
        Left = 9
        Top = 3
        Width = 456
        Height = 190
        Caption = '  Editor Options  '
        TabOrder = 0
        object cbFunctionHint: TCheckBox
          Left = 241
          Top = 166
          Width = 208
          Height = 17
          Caption = 'Show function hints'
          TabOrder = 15
        end
        object cbTrimTrailingSpaces: TCheckBox
          Left = 8
          Top = 166
          Width = 209
          Height = 17
          Caption = 'Trim Trailing Spaces'
          TabOrder = 7
        end
        object cbAutoIndent: TCheckBox
          Left = 8
          Top = 26
          Width = 209
          Height = 17
          Caption = 'Auto Indent'
          TabOrder = 0
        end
        object cbAddIndent: TCheckBox
          Left = 8
          Top = 46
          Width = 209
          Height = 17
          Caption = 'Add indent to {} and :'
          TabOrder = 2
        end
        object cbDropFiles: TCheckBox
          Left = 8
          Top = 126
          Width = 209
          Height = 17
          Caption = 'Insert Dropped Files'
          TabOrder = 5
        end
        object cbEHomeKey: TCheckBox
          Left = 241
          Top = 26
          Width = 208
          Height = 17
          Caption = 'Enhance home key'
          TabOrder = 8
        end
        object cbInsertMode: TCheckBox
          Left = 8
          Top = 66
          Width = 209
          Height = 17
          Caption = 'Insert Mode'
          TabOrder = 1
        end
        object cbParserHints: TCheckBox
          Left = 241
          Top = 146
          Width = 208
          Height = 17
          Caption = 'Show editor hints'
          TabOrder = 14
        end
        object cbHalfPage: TCheckBox
          Left = 241
          Top = 106
          Width = 208
          Height = 17
          Caption = 'Half Page Scrolling'
          TabOrder = 12
        end
        object cbGroupUndo: TCheckBox
          Left = 8
          Top = 106
          Width = 209
          Height = 17
          Caption = 'Group Undo'
          TabOrder = 4
        end
        object cbFindText: TCheckBox
          Left = 8
          Top = 86
          Width = 209
          Height = 17
          Caption = 'Find Text at Cursor'
          TabOrder = 3
        end
        object cbPastEOL: TCheckBox
          Left = 241
          Top = 66
          Width = 208
          Height = 17
          Caption = 'Cursor Past EOL'
          TabOrder = 10
        end
        object cbPastEOF: TCheckBox
          Left = 241
          Top = 46
          Width = 208
          Height = 17
          Caption = 'Cursor Past EOF'
          TabOrder = 9
        end
        object cbScrollHint: TCheckBox
          Left = 241
          Top = 126
          Width = 208
          Height = 17
          Caption = 'Scroll Hint'
          TabOrder = 13
        end
        object cbSmartScroll: TCheckBox
          Left = 241
          Top = 86
          Width = 208
          Height = 17
          Caption = 'Scollbars on need'
          TabOrder = 11
        end
        object cbSpecialChars: TCheckBox
          Left = 8
          Top = 146
          Width = 209
          Height = 17
          Caption = 'Show Special Line Chars'
          TabOrder = 6
        end
      end
      object grpCaret: TGroupBox
        Left = 9
        Top = 220
        Width = 248
        Height = 90
        Caption = '  Caret  '
        TabOrder = 1
        object lblInsertCaret: TLabel
          Left = 8
          Top = 16
          Width = 61
          Height = 15
          Caption = 'Insert caret:'
        end
        object lblOverCaret: TLabel
          Left = 8
          Top = 39
          Width = 83
          Height = 15
          Caption = 'Overwrite caret:'
        end
        object cboInsertCaret: TComboBox
          Left = 136
          Top = 12
          Width = 100
          Height = 23
          Style = csDropDownList
          ItemHeight = 15
          TabOrder = 0
          Items.Strings = (
            'Vertical Line'
            'Horizontal Line'
            'Half Block'
            'Block')
        end
        object cboOverwriteCaret: TComboBox
          Left = 136
          Top = 36
          Width = 100
          Height = 23
          Style = csDropDownList
          ItemHeight = 15
          TabOrder = 1
          Items.Strings = (
            'Vertical Line'
            'Horizontal Line'
            'Half Block'
            'Block')
        end
        object cbMatch: TCheckBox
          Left = 8
          Top = 63
          Width = 233
          Height = 17
          Caption = 'Highlight matching symbols'
          TabOrder = 2
        end
      end
      object grpHighCurLine: TGroupBox
        Left = 264
        Top = 332
        Width = 201
        Height = 84
        Caption = 'Highlight current line'
        TabOrder = 4
        object cbHighlightColor: TLabel
          Left = 99
          Top = 35
          Width = 29
          Height = 15
          Caption = 'Color'
        end
        object cbHighCurrLine: TCheckBox
          Left = 8
          Top = 20
          Width = 185
          Height = 17
          Caption = 'Enabled'
          TabOrder = 0
          OnClick = cbHighCurrLineClick
        end
        object cpHighColor: TColorBox
          Left = 96
          Top = 50
          Width = 97
          Height = 22
          DefaultColorColor = 16777164
          Style = [cbStandardColors, cbIncludeDefault, cbCustomColor, cbPrettyNames]
          ItemHeight = 16
          TabOrder = 1
        end
      end
      object grpTabs: TGroupBox
        Left = 9
        Top = 332
        Width = 248
        Height = 84
        Caption = '  Tabs  '
        TabOrder = 3
        object lblTabSize: TLabel
          Left = 176
          Top = 32
          Width = 46
          Height = 15
          Caption = 'Tab Size:'
        end
        object seTabSize: TSpinEdit
          Left = 176
          Top = 52
          Width = 57
          Height = 24
          MaxValue = 64
          MinValue = 0
          TabOrder = 2
          Value = 0
        end
        object cbUseTabs: TCheckBox
          Left = 8
          Top = 20
          Width = 169
          Height = 17
          Caption = 'Use Tab Character'
          TabOrder = 0
        end
        object cbSmartTabs: TCheckBox
          Left = 8
          Top = 40
          Width = 161
          Height = 17
          Caption = 'Smart Tabs'
          TabOrder = 1
        end
      end
    end
    object tabDisplay: TTabSheet
      Caption = 'Fonts'
      object ScrollHint: TLabel
        Left = 0
        Top = 382
        Width = 473
        Height = 33
        Alignment = taCenter
        AutoSize = False
        Caption = 
          'It is also possible to edit text size by using Control+Scroll, j' +
          'ust like in browsers!'
      end
      object grpGutter: TGroupBox
        Left = 9
        Top = 160
        Width = 456
        Height = 185
        Caption = '  Gutter  '
        TabOrder = 1
        DesignSize = (
          456
          185)
        object lblGutterFont: TLabel
          Left = 8
          Top = 91
          Width = 27
          Height = 15
          Anchors = [akLeft, akRight, akBottom]
          Caption = 'Font:'
        end
        object lblGutterWidth: TLabel
          Left = 360
          Top = 33
          Width = 89
          Height = 14
          Anchors = [akLeft, akRight, akBottom]
          AutoSize = False
          Caption = 'Gutter Width'
          WordWrap = True
        end
        object lblGutterFontSize: TLabel
          Left = 360
          Top = 91
          Width = 20
          Height = 15
          Anchors = [akLeft, akRight, akBottom]
          Caption = 'Size'
        end
        object cbLeadZero: TCheckBox
          Left = 175
          Top = 66
          Width = 162
          Height = 15
          Caption = 'Show Leading Zeros'
          TabOrder = 5
        end
        object cbFirstZero: TCheckBox
          Left = 175
          Top = 46
          Width = 162
          Height = 15
          Caption = 'Start at Zero'
          TabOrder = 4
        end
        object cbLineNum: TCheckBox
          Left = 175
          Top = 26
          Width = 162
          Height = 15
          Caption = 'Show Line Numbers'
          TabOrder = 3
          OnClick = cbLineNumClick
        end
        object cbGutterVis: TCheckBox
          Left = 8
          Top = 26
          Width = 145
          Height = 15
          Caption = 'Visible'
          TabOrder = 0
        end
        object cbGutterAuto: TCheckBox
          Left = 8
          Top = 46
          Width = 145
          Height = 15
          Caption = 'Auto Size'
          TabOrder = 1
        end
        object cbGutterFnt: TCheckBox
          Left = 8
          Top = 66
          Width = 145
          Height = 15
          Caption = 'Use Custom Font'
          TabOrder = 2
          OnClick = cbGutterFntClick
        end
        object cboGutterFont: TComboBox
          Left = 12
          Top = 107
          Width = 341
          Height = 66
          AutoComplete = False
          Style = csOwnerDrawVariable
          ItemHeight = 60
          Sorted = True
          TabOrder = 6
          OnDrawItem = cboGutterFontDrawItem
        end
        object edGutterSize: TSpinEdit
          Left = 360
          Top = 107
          Width = 80
          Height = 24
          MaxValue = 999
          MinValue = 1
          TabOrder = 7
          Value = 10
          OnChange = edGutterSizeChange
        end
        object edGutterWidth: TSpinEdit
          Left = 360
          Top = 49
          Width = 80
          Height = 24
          MaxValue = 999
          MinValue = 1
          TabOrder = 8
          Value = 32
        end
      end
      object grpEditorFont: TGroupBox
        Left = 9
        Top = 3
        Width = 456
        Height = 109
        Caption = '  Editor Font  '
        TabOrder = 0
        object lblEditorSize: TLabel
          Left = 360
          Top = 16
          Width = 23
          Height = 15
          Caption = 'Size:'
        end
        object lblEditorFont: TLabel
          Left = 8
          Top = 16
          Width = 27
          Height = 15
          Caption = 'Font:'
        end
        object cboEditorFont: TComboBox
          Left = 12
          Top = 32
          Width = 341
          Height = 66
          AutoComplete = False
          Style = csOwnerDrawVariable
          ItemHeight = 60
          Sorted = True
          TabOrder = 0
          OnChange = cboEditorFontChange
          OnDrawItem = cboEditorFontDrawItem
        end
        object edEditorSize: TSpinEdit
          Left = 360
          Top = 32
          Width = 80
          Height = 24
          MaxValue = 999
          MinValue = 1
          TabOrder = 1
          Value = 10
          OnChange = edEditorSizeChange
        end
      end
    end
    object tabSyntax: TTabSheet
      Caption = 'Colors'
      object lblForeground: TLabel
        Left = 9
        Top = 182
        Width = 65
        Height = 15
        Caption = 'Foreground:'
      end
      object lblBackground: TLabel
        Left = 9
        Top = 228
        Width = 67
        Height = 15
        Caption = 'Background:'
      end
      object lblSpeed: TLabel
        Left = 9
        Top = 366
        Width = 107
        Height = 15
        Caption = 'Color Speed Setting:'
      end
      object btnSaveSyntax: TSpeedButton
        Left = 8
        Top = 386
        Width = 23
        Height = 23
        Hint = 'Save color theme'
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000CE0E0000D80E0000000000000000000000FF0000FF00
          00FF0000FF0000FF0000000000000000000000000000FF0000FF0000FF0000FF
          0000FF0000FF0000FF0000FF0000FF0000FF0000FF00000000666148A89F77DD
          DDDD9B9A8F00000000000000FF0000FF0000FF0000FF0000FF0000FF0000FF00
          00FF0000FF00000000A89F77A89F77E9E9E9B2B0A7D5D4D29392894848480000
          0000FF0000FF0000FF0000FF0000FF0000FF00000000B9B294A89F77756E534A
          473AACA47EDADAD5E0E0E0B4B4B476736500000000FF0000FF0000FF0000FF00
          00FF00000000A89F77A89F77756E53F3F3F3F1F1F1E7E7E7E1E1E1B4B2A96661
          48635E464A463400000000FF0000FF00000000B2AA87F0EFE8EBE9E0A89F7763
          5E46ADABA4EAEAEAE4E4E4646360A89F77A89F7700000000000000FF0000FF00
          000000EBE9E0FFFFFFFFFFFFF5F4F0A89F77A89F77635E465A574B787255A89F
          77A89F7700000000FF0000FF00000000B2AA87F2F1EBFFFFFFFFFFFFFFFFFFF5
          F4F0EBE9E0A89F77A89F77756E53756E5300000000000000FF0000FF00000000
          EBE9E0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7F6F2EBE9E0A89F77A89F
          7700000000FF0000FF000000008D8A78F2F1EBFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFF0EFE800000000000000FF0000FF00000000B9B294
          DFDCCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEEECE40000
          0000FF0000FF0000FF0000FF00000000000000C9C4AED5D1BFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFF9F8F500000000000000FF0000FF0000FF0000FF0000FF00
          00FF00000000000000C9C4AED5D1BFD5D1BFFFFFFFF4F3EEA89F7700000000FF
          0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF00000000000000D5
          D1BFC5C1A8EDECE400000000000000FF0000FF0000FF0000FF0000FF0000FF00
          00FF0000FF0000FF0000FF0000FF00000000000000A89F7700000000FF0000FF
          0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000FF0000
          FF0000FF0000000000000000FF0000FF0000FF0000FF0000FF00}
        ParentShowHint = False
        ShowHint = True
        OnClick = btnSaveSyntaxClick
      end
      object CppEdit: TSynEdit
        Left = 144
        Top = 7
        Width = 329
        Height = 343
        Align = alCustom
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        TabOrder = 3
        Gutter.AutoSize = True
        Gutter.BorderStyle = gbsNone
        Gutter.DigitCount = 2
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -12
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.LeftOffset = 4
        Gutter.RightOffset = 21
        Gutter.ShowLineNumbers = True
        Gutter.Width = 32
        Gutter.GradientEndColor = clBackground
        HideSelection = True
        Highlighter = cpp
        UseCodeFolding = True
        Lines.Strings = (
          '#include <iostream>'
          '#include <conio.h>'
          ''
          'int main(int argc, char **argv)'
          '{'
          #9'int numbers[20];'
          #9'float average, total; //breakpoint'
          #9'for (int i = 0; i <= 19; i++)'
          #9'{ // active breakpoint'
          #9#9'numbers[i] = i;'
          #9#9'Total += i; // error line'
          #9'}'
          #9'average = total / 20; // comment'
          #9'cout << "total: " << total << "\nAverage: " << average;'
          #9'getch();'
          '}')
        Options = [eoAutoIndent, eoDisableScrollArrows, eoHideShowScrollbars, eoNoCaret, eoNoSelection, eoSmartTabs, eoTrimTrailingSpaces]
        ReadOnly = True
        RightEdge = 0
        ScrollHintFormat = shfTopToBottom
        TabWidth = 4
        WantTabs = True
        OnGutterClick = OnGutterClick
        OnSpecialLineColors = CppEditSpecialLineColors
        OnStatusChange = cppEditStatusChange
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
        AddedKeystrokes = <>
      end
      object ElementList: TListBox
        Left = 7
        Top = 7
        Width = 130
        Height = 169
        ImeName = 'CN'#177'??i(CN'#177'U)'
        IntegralHeight = True
        ItemHeight = 15
        Items.Strings = (
          'Comment'
          'Identifier'
          'Keyword'
          'Number'
          'Background'
          'String'
          'Symbol'
          'WhiteSpace'
          'Directives')
        TabOrder = 0
        OnClick = ElementListClick
      end
      object grpStyle: TGroupBox
        Left = 15
        Top = 275
        Width = 110
        Height = 73
        Caption = '  Style:  '
        TabOrder = 2
        object cbBold: TCheckBox
          Left = 8
          Top = 15
          Width = 100
          Height = 17
          Caption = 'Bold'
          TabOrder = 0
          OnClick = StyleChange
        end
        object cbItalic: TCheckBox
          Left = 8
          Top = 32
          Width = 100
          Height = 17
          Caption = 'Italic'
          TabOrder = 1
          OnClick = StyleChange
        end
        object cbUnderlined: TCheckBox
          Left = 8
          Top = 50
          Width = 100
          Height = 17
          Caption = 'Underlined'
          TabOrder = 2
          OnClick = StyleChange
        end
      end
      object cboQuickColor: TComboBox
        Left = 32
        Top = 386
        Width = 130
        Height = 23
        Style = csDropDownList
        ItemHeight = 15
        TabOrder = 1
        OnSelect = cboQuickColorSelect
        Items.Strings = (
          'Classic'
          'Classic Plus'
          'Twilight'
          'Ocean'
          'Visual Studio'
          'Borland'
          'Matrix'
          'Obsidian'
          'GSS Hacker'
          'Obvilion'
          'PlasticCodeWrap')
      end
      object edSyntaxExt: TEdit
        Left = 180
        Top = 386
        Width = 285
        Height = 23
        TabOrder = 4
      end
      object cbSyntaxHighlight: TCheckBox
        Left = 175
        Top = 360
        Width = 300
        Height = 17
        Caption = 'Use Syntax Highlighting'
        TabOrder = 5
        OnClick = cbSyntaxHighlightClick
      end
      object cpForeground: TColorBox
        Left = 17
        Top = 202
        Width = 110
        Height = 22
        Style = [cbStandardColors, cbCustomColor, cbPrettyNames]
        ItemHeight = 16
        TabOrder = 6
        OnChange = StyleChange
      end
      object cpBackground: TColorBox
        Left = 17
        Top = 246
        Width = 110
        Height = 22
        Style = [cbStandardColors, cbCustomColor, cbPrettyNames]
        ItemHeight = 16
        TabOrder = 7
        OnChange = StyleChange
      end
    end
    object tabCode: TTabSheet
      Caption = 'Snippets'
      object PagesSnippets: TPageControl
        Left = 0
        Top = 0
        Width = 477
        Height = 425
        ActivePage = tabCPInserts
        Align = alClient
        TabOrder = 0
        object tabCPInserts: TTabSheet
          Caption = 'Inserts'
          object btnAdd: TButton
            Left = 384
            Top = 45
            Width = 78
            Height = 24
            Caption = 'Add'
            TabOrder = 0
            OnClick = btnAddClick
          end
          object btnRemove: TButton
            Left = 382
            Top = 77
            Width = 80
            Height = 24
            Caption = 'Remove'
            TabOrder = 1
            OnClick = btnRemoveClick
          end
          object CodeIns: TSynEdit
            Left = 0
            Top = 184
            Width = 469
            Height = 211
            Align = alBottom
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Courier New'
            Font.Style = []
            TabOrder = 2
            TabStop = False
            Gutter.AutoSize = True
            Gutter.BorderStyle = gbsNone
            Gutter.Font.Charset = DEFAULT_CHARSET
            Gutter.Font.Color = clWindowText
            Gutter.Font.Height = -12
            Gutter.Font.Name = 'Courier New'
            Gutter.Font.Style = []
            Gutter.LeftOffset = 4
            Gutter.RightOffset = 21
            Gutter.ShowLineNumbers = True
            Highlighter = cpp
            UseCodeFolding = True
            Options = [eoAutoIndent, eoHideShowScrollbars, eoKeepCaretX, eoSmartTabs, eoTabIndent, eoTrimTrailingSpaces]
            TabWidth = 4
            WantTabs = True
            OnStatusChange = CodeInsStatusChange
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
          object lvCodeIns: TStringGrid
            Left = 8
            Top = 8
            Width = 369
            Height = 137
            ColCount = 3
            DefaultColWidth = 115
            DefaultRowHeight = 18
            FixedCols = 0
            RowCount = 2
            Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs, goAlwaysShowEditor]
            ScrollBars = ssVertical
            TabOrder = 3
            OnSelectCell = lvCodeInsSelectCell
          end
        end
        object tabCPDefault: TTabSheet
          Caption = 'Default Insert'
          object cbDefaultCode: TCheckBox
            Left = 4
            Top = 2
            Width = 461
            Height = 17
            Caption = 'Insert Default Code into Empty Projects'
            TabOrder = 0
          end
          object seDefault: TSynEdit
            Left = 0
            Top = 24
            Width = 468
            Height = 331
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Courier New'
            Font.Style = []
            TabOrder = 1
            TabStop = False
            Gutter.AutoSize = True
            Gutter.BorderStyle = gbsNone
            Gutter.Font.Charset = DEFAULT_CHARSET
            Gutter.Font.Color = clWindowText
            Gutter.Font.Height = -12
            Gutter.Font.Name = 'Courier New'
            Gutter.Font.Style = []
            Gutter.LeftOffset = 4
            Gutter.RightOffset = 21
            Gutter.ShowLineNumbers = True
            Highlighter = cpp
            Options = [eoAutoIndent, eoHideShowScrollbars, eoKeepCaretX, eoSmartTabs, eoTabIndent, eoTrimTrailingSpaces]
            TabWidth = 4
            WantTabs = True
            OnStatusChange = CodeInsStatusChange
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
        end
      end
    end
    object tabCBCompletion: TTabSheet
      Caption = 'Completion'
      object PagesCompletion: TPageControl
        Left = 0
        Top = 0
        Width = 477
        Height = 425
        ActivePage = tabCodeCompletion
        Align = alClient
        TabOrder = 0
        object tabCodeCompletion: TTabSheet
          Caption = 'Code Completion'
          ImageIndex = 1
          object lblRefreshHint: TLabel
            Left = 188
            Top = 346
            Width = 261
            Height = 15
            Alignment = taCenter
            Caption = 'Please refresh the cache when updating Dev-C++'
            WordWrap = True
          end
          object btnFileBrowse: TSpeedButton
            Left = 381
            Top = 310
            Width = 23
            Height = 22
            Glyph.Data = {
              36030000424D3603000000000000360000002800000010000000100000000100
              18000000000000030000120B0000120B00000000000000000000BFBFBFBFBFBF
              BFBFBFBFBFBFBFBFBF000000BFBFBFBFBFBFBFBFBFBFBFBF0000000000000000
              00000000000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF000000BF
              BFBF000000BFBFBF0000005DCCFF5DCCFF5DCCFF000000BFBFBFBFBFBFBFBFBF
              BFBFBFBFBFBFBFBFBF000000BFBFBFBFBFBFBFBFBFBFBFBF6868680000000000
              00000000000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
              BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
              BFBFBFBFBFBFBFBFBF000000BFBFBFBFBFBFBFBFBFBFBFBF0000000000000000
              00000000000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF000000BF
              BFBF000000BFBFBF0000005DCCFF5DCCFF5DCCFF000000BFBFBFBFBFBFBFBFBF
              BFBFBFBFBFBFBFBFBF000000BFBFBFBFBFBFBFBFBFBFBFBF6868680000000000
              00000000000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
              BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF000000000000
              000000000000000000000000000000000000000000000000000000BFBFBFBFBF
              BFBFBFBFBFBFBFBFBFBF00000000AEFF0096DB0096DB0096DB0096DB0096DB00
              96DB0096DB0082BE000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBF0000005DCCFF
              00AEFF00AEFF00AEFF00AEFF00AEFF00AEFF00AEFF0096DB000000BFBFBFBFBF
              BFBFBFBFBFBFBFBFBFBF0000005DCCFF00AEFF00AEFF00AEFF00AEFF00AEFF00
              AEFF00AEFF0096DB000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBF0000005DCCFF
              00AEFF00AEFF00AEFF00AEFF00AEFF00AEFF00AEFF0096DB000000BFBFBFBFBF
              BFBFBFBFBFBFBFBFBFBF0000005DCCFF00AEFF00AEFF5DCCFF5DCCFF5DCCFF5D
              CCFF5DCCFF00AEFF000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBF686868BDEBFF
              5DCCFF5DCCFF000000000000000000000000000000000000BFBFBFBFBFBFBFBF
              BFBFBFBFBFBFBFBFBFBFBFBFBF000000000000000000BFBFBFBFBFBFBFBFBFBF
              BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF}
            OnClick = btnFileBrowseClick
          end
          object lblCompletionColor: TLabel
            Left = 328
            Top = 108
            Width = 97
            Height = 15
            Caption = 'Background color:'
          end
          object lblCompletionDelay: TLabel
            Left = 8
            Top = 108
            Width = 59
            Height = 15
            Caption = 'Delay (ms):'
          end
          object btnCCCadd: TButton
            Left = 409
            Top = 310
            Width = 55
            Height = 22
            Caption = 'Add'
            TabOrder = 0
            OnClick = btnCCCaddClick
          end
          object btnCCCdelete: TButton
            Left = 10
            Top = 342
            Width = 80
            Height = 25
            Caption = 'Clear'
            TabOrder = 1
            OnClick = btnCCCdeleteClick
          end
          object btnCCCrefresh: TButton
            Left = 92
            Top = 342
            Width = 80
            Height = 25
            Caption = 'Refresh'
            TabOrder = 2
            OnClick = btnCCCrefreshClick
          end
          object chkCCCache: TCheckBox
            Left = 8
            Top = 176
            Width = 457
            Height = 17
            Caption = 'Use code-completion cache'
            TabOrder = 3
            OnClick = chkCCCacheClick
          end
          object chkEnableCompletion: TCheckBox
            Left = 8
            Top = 8
            Width = 457
            Height = 17
            Caption = 'Enable code-completion'
            TabOrder = 4
            OnClick = chkEnableCompletionClick
          end
          object cpCompletionBackground: TColorBox
            Left = 333
            Top = 128
            Width = 112
            Height = 22
            DefaultColorColor = clWhite
            Style = [cbStandardColors, cbCustomColor, cbPrettyNames]
            ItemHeight = 16
            TabOrder = 5
          end
          object edIncludeFile: TEdit
            Left = 8
            Top = 310
            Width = 370
            Height = 23
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGray
            Font.Height = -12
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
            TabOrder = 6
            Text = 'Example: stdio.h;C:\myfile.h;'
            OnChange = edIncludeFileChange
            OnClick = edIncludeFileClick
            OnKeyPress = edIncludeFileKeyPress
          end
          object gbCBEngine: TGroupBox
            Left = 8
            Top = 36
            Width = 457
            Height = 65
            Caption = 'Engine behaviour'
            TabOrder = 7
            object chkCBParseLocalH: TCheckBox
              Left = 8
              Top = 20
              Width = 345
              Height = 17
              Caption = 'Scan local files referenced in #include'#39's'
              TabOrder = 0
            end
            object chkCBParseGlobalH: TCheckBox
              Left = 8
              Top = 40
              Width = 345
              Height = 17
              Caption = 'Scan global files referenced in #include'#39's'
              TabOrder = 1
            end
          end
          object lbCCC: TListBox
            Left = 8
            Top = 200
            Width = 457
            Height = 105
            ItemHeight = 15
            ParentShowHint = False
            ShowHint = True
            Sorted = True
            TabOrder = 8
          end
          object pbCCCache: TProgressBar
            Left = 8
            Top = 370
            Width = 457
            Height = 20
            Smooth = True
            TabOrder = 9
            Visible = False
          end
          object tbCompletionDelay: TTrackBar
            Left = 16
            Top = 132
            Width = 297
            Height = 37
            Max = 2000
            Min = 1
            ParentShowHint = False
            Frequency = 50
            Position = 1000
            ShowHint = False
            TabOrder = 10
            TickMarks = tmBoth
            OnChange = tbCompletionDelayChange
          end
        end
        object tabSymbolCompletion: TTabSheet
          Caption = 'Symbol Completion'
          ImageIndex = 2
          object grpSpecific: TGroupBox
            Left = 16
            Top = 40
            Width = 273
            Height = 201
            Caption = 'Specific completion options'
            TabOrder = 1
            object cbParenth: TCheckBox
              Left = 16
              Top = 48
              Width = 240
              Height = 17
              Caption = 'Complete parentheses '#39'()'#39
              TabOrder = 1
            end
            object cbBraces: TCheckBox
              Left = 16
              Top = 24
              Width = 240
              Height = 17
              Caption = 'Complete braces '#39'{}'#39
              TabOrder = 0
            end
            object cbInclude: TCheckBox
              Left = 16
              Top = 72
              Width = 240
              Height = 17
              Caption = 'Complete includes '#39'<>'#39
              TabOrder = 2
            end
            object cbComments: TCheckBox
              Left = 16
              Top = 120
              Width = 240
              Height = 17
              Caption = 'Complete multiline comments '#39'/**/'#39
              TabOrder = 4
            end
            object cbArray: TCheckBox
              Left = 16
              Top = 96
              Width = 240
              Height = 17
              Caption = 'Complete square braces '#39'[]'#39
              TabOrder = 3
            end
            object cbSingleQuotes: TCheckBox
              Left = 16
              Top = 144
              Width = 240
              Height = 17
              Caption = 'Complete single quotes '#39#39#39#39
              TabOrder = 5
            end
            object cbDoubleQuotes: TCheckBox
              Left = 16
              Top = 168
              Width = 240
              Height = 17
              Caption = 'Complete double quotes '#39'""'#39
              TabOrder = 6
            end
          end
          object cbSymbolComplete: TCheckBox
            Left = 8
            Top = 8
            Width = 465
            Height = 17
            Caption = 'Enable symbol completion'
            TabOrder = 0
            OnClick = cbSymbolCompleteClick
          end
          object cbDeleteCompleted: TCheckBox
            Left = 8
            Top = 256
            Width = 449
            Height = 17
            Caption = 'Delete completed symbols as pairs'
            TabOrder = 2
          end
        end
      end
    end
    object tabAutosave: TTabSheet
      Caption = 'Autosave'
      ImageIndex = 5
      object cbAutoSave: TCheckBox
        Left = 8
        Top = 8
        Width = 457
        Height = 17
        Caption = 'Enable editor autosave'
        TabOrder = 0
        OnClick = cbAutoSaveClick
      end
      object OptionsGroup: TGroupBox
        Left = 8
        Top = 32
        Width = 457
        Height = 385
        Caption = ' Options '
        TabOrder = 1
        object SaveInterval: TLabel
          Left = 16
          Top = 36
          Width = 42
          Height = 15
          Caption = 'Interval:'
        end
        object lblTimeStampExample: TLabel
          Left = 24
          Top = 296
          Width = 47
          Height = 15
          Caption = 'Example:'
        end
        object MinutesDelay: TTrackBar
          Left = 144
          Top = 24
          Width = 297
          Height = 40
          Max = 60
          Min = 1
          PageSize = 1
          Position = 1
          TabOrder = 0
          TickMarks = tmBoth
          OnChange = MinutesDelayChange
        end
        object FileOptions: TRadioGroup
          Left = 16
          Top = 80
          Width = 239
          Height = 97
          Caption = 'Files'
          Items.Strings = (
            'Save only the currently visible file'
            'Save all open files after each interval'
            'Save all project files')
          TabOrder = 1
        end
        object NameOptions: TRadioGroup
          Left = 16
          Top = 188
          Width = 239
          Height = 97
          Caption = 'Filenames'
          Items.Strings = (
            'Overwrite file'
            'Append UNIX timestamp'
            'Append formatted timestamp')
          TabOrder = 2
          OnClick = NameOptionsClick
        end
      end
    end
  end
  object btnOk: TBitBtn
    Left = 210
    Top = 460
    Width = 85
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 1
    OnClick = btnOkClick
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333330000333333333333333333333333F33333333333
      00003333344333333333333333388F3333333333000033334224333333333333
      338338F3333333330000333422224333333333333833338F3333333300003342
      222224333333333383333338F3333333000034222A22224333333338F338F333
      8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
      33333338F83338F338F33333000033A33333A222433333338333338F338F3333
      0000333333333A222433333333333338F338F33300003333333333A222433333
      333333338F338F33000033333333333A222433333333333338F338F300003333
      33333333A222433333333333338F338F00003333333333333A22433333333333
      3338F38F000033333333333333A223333333333333338F830000333333333333
      333A333333333333333338330000333333333333333333333333333333333333
      0000}
    NumGlyphs = 2
  end
  object btnCancel: TBitBtn
    Left = 300
    Top = 460
    Width = 85
    Height = 25
    Anchors = [akRight, akBottom]
    TabOrder = 2
    OnClick = btnCancelClick
    Kind = bkCancel
  end
  object btnHelp: TBitBtn
    Left = 390
    Top = 460
    Width = 85
    Height = 25
    Anchors = [akRight, akBottom]
    Enabled = False
    TabOrder = 3
    OnClick = btnHelpClick
    Kind = bkHelp
  end
  object cpp: TSynCppSyn
    DefaultFilter = 'C++ Files (*.c,*.cpp,*.h,*.hpp)|*.c;*.cpp;*.h;*.hpp'
    Left = 5
    Top = 458
  end
  object CppParserCopy: TCppParser
    BaseIndex = 0
    Enabled = True
    OnTotalProgress = CppParser1TotalProgress
    Tokenizer = CppTokenizerCopy
    Preprocessor = CppPreprocessorCopy
    ParseLocalHeaders = True
    ParseGlobalHeaders = True
    OnStartParsing = CppParser1StartParsing
    OnEndParsing = CppParser1EndParsing
    Left = 40
    Top = 458
  end
  object CppPreprocessorCopy: TCppPreprocessor
    Left = 72
    Top = 456
  end
  object CppTokenizerCopy: TCppTokenizer
    Left = 104
    Top = 456
  end
end
