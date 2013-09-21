object EditorOptForm: TEditorOptForm
  Left = 758
  Top = 442
  HelpType = htKeyword
  BorderStyle = bsDialog
  Caption = 'Editor Options'
  ClientHeight = 412
  ClientWidth = 464
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    464
    412)
  PixelsPerInch = 96
  TextHeight = 13
  object PagesMain: TPageControl
    Left = 0
    Top = 0
    Width = 464
    Height = 375
    ActivePage = tabCode
    TabOrder = 0
    object tabGeneral: TTabSheet
      Caption = 'General'
      object cbSyntaxHighlight: TCheckBox
        Left = 16
        Top = 280
        Width = 273
        Height = 17
        Caption = 'Use Syntax Highlighting'
        TabOrder = 0
        OnClick = cbSyntaxHighlightClick
      end
      object edSyntaxExt: TEdit
        Left = 12
        Top = 308
        Width = 277
        Height = 21
        TabOrder = 1
      end
      object grpMargin: TGroupBox
        Left = 248
        Top = 188
        Width = 201
        Height = 84
        Caption = '  Right Margin  '
        TabOrder = 2
        object lblMarginWidth: TLabel
          Left = 8
          Top = 35
          Width = 28
          Height = 13
          Caption = 'Width'
        end
        object lblMarginColor: TLabel
          Left = 91
          Top = 35
          Width = 24
          Height = 13
          Caption = 'Color'
        end
        object cpMarginColor: TColorPickerButton
          Left = 91
          Top = 50
          Width = 70
          Height = 22
          DefaultText = 'default'
          PopupSpacing = 8
          ShowSystemColors = False
          OnDefaultSelect = cpMarginColorDefaultSelect
          OnHint = cpMarginColorHint
        end
        object cbMarginVis: TCheckBox
          Left = 8
          Top = 16
          Width = 75
          Height = 17
          Caption = 'Visible'
          TabOrder = 0
        end
        object edMarginWidth: TSpinEdit
          Left = 8
          Top = 51
          Width = 60
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 1
          Value = 0
        end
      end
      object grpEditorOpts: TGroupBox
        Left = 9
        Top = 3
        Width = 440
        Height = 182
        Caption = '  Editor Options  '
        TabOrder = 4
        object lblTabSize: TLabel
          Left = 160
          Top = 48
          Width = 45
          Height = 13
          Caption = 'Tab Size:'
        end
        object cbFunctionHint: TCheckBox
          Left = 241
          Top = 154
          Width = 184
          Height = 17
          Caption = 'Show function hints'
          TabOrder = 0
        end
        object cbTrimTrailingSpaces: TCheckBox
          Left = 8
          Top = 154
          Width = 177
          Height = 17
          Caption = 'Trim Trailing Spaces'
          TabOrder = 1
        end
        object cbAutoIndent: TCheckBox
          Left = 8
          Top = 18
          Width = 177
          Height = 17
          Hint = 'caret will position to first non-whitespace of preceeding line'
          Caption = 'Auto Indent'
          TabOrder = 2
        end
        object cbDoubleLine: TCheckBox
          Left = 241
          Top = 69
          Width = 184
          Height = 17
          Hint = 'double clicking a line selects it'
          Caption = 'Double Click Line'
          TabOrder = 3
        end
        object cbDropFiles: TCheckBox
          Left = 8
          Top = 120
          Width = 177
          Height = 17
          Caption = 'Insert Dropped Files'
          TabOrder = 4
        end
        object cbEHomeKey: TCheckBox
          Left = 241
          Top = 18
          Width = 184
          Height = 17
          Hint = 'enhances home key positioning, similar to visual studio'
          Caption = 'Enhance home key'
          TabOrder = 5
        end
        object cbInsertMode: TCheckBox
          Left = 8
          Top = 35
          Width = 145
          Height = 17
          Hint = 'editor is in insert mode on start'
          Caption = 'Insert Mode'
          TabOrder = 6
        end
        object cbParserHints: TCheckBox
          Left = 241
          Top = 137
          Width = 184
          Height = 17
          Caption = 'Show editor hints'
          TabOrder = 7
        end
        object cbHalfPage: TCheckBox
          Left = 241
          Top = 103
          Width = 184
          Height = 17
          Hint = 'page up/down will move text by half a page instead of full page'
          Caption = 'Half Page Scrolling'
          TabOrder = 8
        end
        object cbGroupUndo: TCheckBox
          Left = 8
          Top = 103
          Width = 177
          Height = 17
          Hint = 'handle all changes of same type as single action'
          Caption = 'Group Undo'
          TabOrder = 9
        end
        object cbFindText: TCheckBox
          Left = 8
          Top = 86
          Width = 184
          Height = 17
          Hint = 'inserts text at cursor into text to find of search dialog'
          Caption = 'Find Text at Cursor'
          TabOrder = 10
        end
        object cbPastEOL: TCheckBox
          Left = 241
          Top = 52
          Width = 184
          Height = 17
          Hint = 'allows cursor position past end of line'
          Caption = 'Cursor Past EOL'
          TabOrder = 11
        end
        object cbPastEOF: TCheckBox
          Left = 241
          Top = 35
          Width = 184
          Height = 17
          Hint = 'allow cursor position past end of file'
          Caption = 'Cursor Past EOF'
          TabOrder = 12
        end
        object cbScrollHint: TCheckBox
          Left = 241
          Top = 120
          Width = 184
          Height = 17
          Hint = 'shows current line when scrolling'
          Caption = 'Scroll Hint'
          TabOrder = 13
        end
        object cbSmartScroll: TCheckBox
          Left = 241
          Top = 86
          Width = 184
          Height = 17
          Hint = 'show scrollbars only when content is available'
          Caption = 'Scollbars on need'
          TabOrder = 14
        end
        object cbSpecialChars: TCheckBox
          Left = 8
          Top = 137
          Width = 177
          Height = 17
          Caption = 'Show Special Line Chars'
          TabOrder = 15
        end
        object cbUseTabs: TCheckBox
          Left = 8
          Top = 52
          Width = 137
          Height = 17
          Caption = 'Use Tab Character'
          TabOrder = 16
        end
        object cbSmartTabs: TCheckBox
          Left = 8
          Top = 69
          Width = 177
          Height = 17
          Hint = 
            'on tab cursor is moved to first nonblank space of preceeding lin' +
            'e'
          Caption = 'Smart Tabs'
          TabOrder = 17
        end
        object seTabSize: TSpinEdit
          Left = 160
          Top = 68
          Width = 57
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 18
          Value = 0
        end
      end
      object grpCaret: TGroupBox
        Left = 9
        Top = 188
        Width = 235
        Height = 84
        Caption = '  Caret  '
        TabOrder = 3
        object lblInsertCaret: TLabel
          Left = 8
          Top = 16
          Width = 56
          Height = 13
          Caption = 'Insert caret:'
        end
        object lblOverCaret: TLabel
          Left = 8
          Top = 39
          Width = 75
          Height = 13
          Caption = 'Overwrite caret:'
        end
        object cboInsertCaret: TComboBox
          Left = 120
          Top = 12
          Width = 100
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
          OnDblClick = cboDblClick
          Items.Strings = (
            'Vertical Line'
            'Horizontal Line'
            'Half Block'
            'Block')
        end
        object cboOverwriteCaret: TComboBox
          Left = 120
          Top = 35
          Width = 100
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 1
          OnDblClick = cboDblClick
          Items.Strings = (
            'Vertical Line'
            'Horizontal Line'
            'Half Block'
            'Block')
        end
        object cbMatch: TCheckBox
          Left = 8
          Top = 60
          Width = 217
          Height = 17
          Caption = 'Highlight matching parenthesis/braces'
          TabOrder = 2
        end
      end
      object HighCurLineBox: TGroupBox
        Left = 296
        Top = 272
        Width = 153
        Height = 65
        Caption = 'Highlight current line'
        TabOrder = 5
        object cpHighColor: TColorPickerButton
          Left = 67
          Top = 35
          Width = 70
          Height = 22
          DefaultText = 'default'
          PopupSpacing = 8
          ShowSystemColors = False
          OnDefaultSelect = cpMarginColorDefaultSelect
          OnHint = cpMarginColorHint
        end
        object HighCurLineLabel: TLabel
          Left = 24
          Top = 38
          Width = 24
          Height = 13
          Caption = 'Color'
        end
        object cbHighCurrLine: TCheckBox
          Left = 8
          Top = 16
          Width = 73
          Height = 17
          Caption = 'Enabled'
          TabOrder = 0
          OnClick = cbHighCurrLineClick
        end
      end
    end
    object tabDisplay: TTabSheet
      Caption = 'Display'
      object ScrollHint: TLabel
        Left = 0
        Top = 304
        Width = 449
        Height = 33
        Alignment = taCenter
        AutoSize = False
        Caption = 
          'It is also possible to edit text size by using Control+Scroll, j' +
          'ust like in browsers!'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object grpGutter: TGroupBox
        Left = 6
        Top = 128
        Width = 443
        Height = 159
        Caption = '  Gutter  '
        TabOrder = 1
        DesignSize = (
          443
          159)
        object lblGutterFont: TLabel
          Left = 8
          Top = 65
          Width = 24
          Height = 13
          Anchors = [akLeft, akRight, akBottom]
          Caption = 'Font:'
        end
        object lblGutterWidth: TLabel
          Left = 375
          Top = 65
          Width = 28
          Height = 13
          Anchors = [akLeft, akRight, akBottom]
          Caption = 'Width'
          WordWrap = True
        end
        object lblGutterFontSize: TLabel
          Left = 256
          Top = 65
          Width = 20
          Height = 13
          Anchors = [akLeft, akRight, akBottom]
          Caption = 'Size'
        end
        object cbLeadZero: TCheckBox
          Left = 199
          Top = 48
          Width = 190
          Height = 15
          Caption = 'Show Leading Zeros'
          TabOrder = 5
        end
        object cbFirstZero: TCheckBox
          Left = 199
          Top = 32
          Width = 190
          Height = 15
          Caption = 'Start at Zero'
          TabOrder = 4
        end
        object cbLineNum: TCheckBox
          Left = 199
          Top = 16
          Width = 190
          Height = 15
          Caption = 'Show Line Numbers'
          TabOrder = 3
          OnClick = cbLineNumClick
        end
        object cbGutterVis: TCheckBox
          Left = 8
          Top = 16
          Width = 190
          Height = 15
          Caption = 'Visible'
          TabOrder = 0
        end
        object cbGutterAuto: TCheckBox
          Left = 8
          Top = 32
          Width = 190
          Height = 15
          Hint = 'Gutter width adapts to line number width'
          Caption = 'Auto Size'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
        end
        object cbGutterFnt: TCheckBox
          Left = 8
          Top = 48
          Width = 190
          Height = 15
          Caption = 'Use Custom Font'
          TabOrder = 2
          OnClick = cbGutterFntClick
        end
        object pnlGutterPreview: TPanel
          Left = 12
          Top = 108
          Width = 418
          Height = 40
          Anchors = [akLeft, akRight, akBottom]
          BevelOuter = bvLowered
          Caption = 'Gutter Font'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          TabOrder = 8
        end
        object cboGutterFont: TComboBox
          Left = 12
          Top = 81
          Width = 240
          Height = 21
          Anchors = [akLeft, akRight, akBottom]
          ItemHeight = 0
          TabOrder = 6
          OnChange = FontChange
          OnDblClick = cboDblClick
        end
        object cboGutterSize: TComboBox
          Left = 256
          Top = 81
          Width = 112
          Height = 21
          Anchors = [akLeft, akRight, akBottom]
          ItemHeight = 0
          TabOrder = 7
          OnChange = FontSizeChange
          OnDblClick = cboDblClick
        end
        object edGutterWidth: TSpinEdit
          Left = 376
          Top = 81
          Width = 57
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 9
          Value = 0
        end
      end
      object grpEditorFont: TGroupBox
        Left = 6
        Top = 6
        Width = 443
        Height = 109
        Caption = '  Editor Font  '
        TabOrder = 0
        DesignSize = (
          443
          109)
        object lblEditorSize: TLabel
          Left = 256
          Top = 16
          Width = 23
          Height = 13
          Caption = 'Size:'
        end
        object lblEditorFont: TLabel
          Left = 8
          Top = 16
          Width = 24
          Height = 13
          Caption = 'Font:'
        end
        object cboEditorFont: TComboBox
          Left = 12
          Top = 32
          Width = 240
          Height = 21
          ItemHeight = 0
          TabOrder = 0
          OnDblClick = cboDblClick
          OnSelect = FontChange
        end
        object cboEditorSize: TComboBox
          Left = 256
          Top = 32
          Width = 80
          Height = 21
          ItemHeight = 0
          TabOrder = 1
          OnChange = FontSizeChange
          OnDblClick = cboDblClick
        end
        object pnlEditorPreview: TPanel
          Left = 12
          Top = 60
          Width = 418
          Height = 40
          Anchors = [akLeft, akRight, akBottom]
          BevelOuter = bvLowered
          Caption = 'Editor Font'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
        end
      end
    end
    object tabSyntax: TTabSheet
      Caption = 'Syntax'
      object cpForeground: TColorPickerButton
        Left = 161
        Top = 23
        Width = 110
        Height = 22
        CustomText = 'Custom'
        DefaultText = 'Default'
        PopupSpacing = 8
        ShowSystemColors = True
        OnChange = StyleChange
        OnDefaultSelect = DefaultSelect
        OnHint = PickerHint
      end
      object cpBackground: TColorPickerButton
        Left = 161
        Top = 76
        Width = 110
        Height = 22
        CustomText = 'Custom'
        DefaultText = 'Default'
        PopupSpacing = 8
        ShowSystemColors = True
        OnChange = StyleChange
        OnDefaultSelect = DefaultSelect
        OnHint = PickerHint
      end
      object lblForeground: TLabel
        Left = 153
        Top = 8
        Width = 57
        Height = 13
        Caption = 'Foreground:'
      end
      object lblBackground: TLabel
        Left = 153
        Top = 59
        Width = 61
        Height = 13
        Caption = 'Background:'
      end
      object lblElements: TLabel
        Left = 7
        Top = 8
        Width = 41
        Height = 13
        Caption = 'Element:'
      end
      object lblSpeed: TLabel
        Left = 313
        Top = 8
        Width = 97
        Height = 13
        Caption = 'Color Speed Setting:'
      end
      object btnSaveSyntax: TSpeedButton
        Left = 304
        Top = 23
        Width = 21
        Height = 21
        Hint = 'Save custom syntax settings'
        Flat = True
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
        Left = 7
        Top = 152
        Width = 442
        Height = 185
        Align = alCustom
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        TabOrder = 3
        Gutter.AutoSize = True
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.RightOffset = 21
        Gutter.ShowLineNumbers = True
        Gutter.UseFontStyle = False
        HideSelection = True
        Highlighter = cpp
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
        Top = 23
        Width = 138
        Height = 121
        ImeName = 'CN'#177'??i(CN'#177'U)'
        IntegralHeight = True
        ItemHeight = 13
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
        Left = 327
        Top = 53
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
        Left = 328
        Top = 23
        Width = 117
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 1
        OnSelect = cboQuickColorSelect
        Items.Strings = (
          'Classic'
          'Classic Plus'
          'Twilight'
          'Ocean'
          'Visual Studio'
          'Borland'
          'Matrix')
      end
    end
    object tabCode: TTabSheet
      Caption = 'Code'
      object codepages: TPageControl
        Left = 0
        Top = 0
        Width = 456
        Height = 347
        ActivePage = tabCPInserts
        Align = alClient
        TabOrder = 0
        object tabCPInserts: TTabSheet
          Caption = 'Snippets'
          object btnAdd: TButton
            Left = 350
            Top = 13
            Width = 80
            Height = 24
            Caption = 'Add'
            TabOrder = 1
            OnClick = btnAddClick
          end
          object btnEdit: TButton
            Left = 350
            Top = 53
            Width = 80
            Height = 24
            Caption = 'Edit'
            TabOrder = 2
            OnClick = btnEditClick
          end
          object btnRemove: TButton
            Left = 350
            Top = 93
            Width = 80
            Height = 24
            Caption = 'Remove'
            TabOrder = 3
            OnClick = btnRemoveClick
          end
          object lvCodeins: TListView
            Left = 2
            Top = 8
            Width = 327
            Height = 113
            BevelOuter = bvRaised
            BevelKind = bkSoft
            BorderStyle = bsNone
            Columns = <
              item
                Caption = 'Menu text'
                Width = 100
              end
              item
                Caption = 'Section'
              end
              item
                Caption = 'Description'
                Width = 157
              end>
            ColumnClick = False
            GridLines = True
            HideSelection = False
            RowSelect = True
            TabOrder = 0
            ViewStyle = vsReport
            OnColumnClick = lvCodeinsColumnClick
            OnCompare = lvCodeinsCompare
            OnSelectItem = lvCodeinsSelectItem
          end
          object CodeIns: TSynEdit
            Left = 0
            Top = 131
            Width = 448
            Height = 188
            Align = alBottom
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Courier New'
            Font.Style = []
            TabOrder = 4
            TabStop = False
            Gutter.AutoSize = True
            Gutter.Font.Charset = DEFAULT_CHARSET
            Gutter.Font.Color = clWindowText
            Gutter.Font.Height = -11
            Gutter.Font.Name = 'Courier New'
            Gutter.Font.Style = []
            Gutter.RightOffset = 21
            Gutter.ShowLineNumbers = True
            Gutter.UseFontStyle = False
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
        object tabSymbols: TTabSheet
          Caption = 'Symbol completion'
          ImageIndex = 2
          object grpSpecific: TGroupBox
            Left = 16
            Top = 40
            Width = 257
            Height = 153
            Caption = 'Specific completion options'
            TabOrder = 0
            object cbParenth: TCheckBox
              Left = 16
              Top = 48
              Width = 193
              Height = 17
              Caption = 'Complete parentheses '#39'()'#39
              TabOrder = 0
            end
            object cbBraces: TCheckBox
              Left = 16
              Top = 24
              Width = 193
              Height = 17
              Caption = 'Complete braces '#39'{}'#39
              TabOrder = 1
            end
            object cbInclude: TCheckBox
              Left = 16
              Top = 72
              Width = 193
              Height = 17
              Caption = 'Complete includes '#39'<>'#39' ;  ""'
              TabOrder = 2
            end
            object cbComments: TCheckBox
              Left = 16
              Top = 120
              Width = 193
              Height = 17
              Caption = 'Complete multiline comments '#39'/**/'#39
              TabOrder = 3
            end
            object cbArray: TCheckBox
              Left = 16
              Top = 96
              Width = 193
              Height = 17
              Caption = 'Complete square braces '#39'[]'#39
              TabOrder = 4
            end
          end
          object cbSymbolComplete: TCheckBox
            Left = 8
            Top = 8
            Width = 241
            Height = 17
            Caption = 'Enable symbol completion'
            TabOrder = 1
            OnClick = cbSymbolCompleteClick
          end
        end
        object tabCPDefault: TTabSheet
          Caption = 'Default Source'
          object seDefault: TSynEdit
            Left = 0
            Top = 0
            Width = 448
            Height = 299
            Align = alClient
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Courier New'
            Font.Style = []
            TabOrder = 0
            TabStop = False
            Gutter.AutoSize = True
            Gutter.Font.Charset = DEFAULT_CHARSET
            Gutter.Font.Color = clWindowText
            Gutter.Font.Height = -11
            Gutter.Font.Name = 'Courier New'
            Gutter.Font.Style = []
            Gutter.RightOffset = 21
            Gutter.ShowLineNumbers = True
            Gutter.UseFontStyle = False
            Highlighter = cpp
            Options = [eoAutoIndent, eoHideShowScrollbars, eoKeepCaretX, eoSmartTabs, eoTabIndent, eoTrimTrailingSpaces]
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
          object Panel1: TPanel
            Left = 0
            Top = 299
            Width = 448
            Height = 20
            Align = alBottom
            BevelOuter = bvNone
            TabOrder = 1
            object cbDefaultintoprj: TCheckBox
              Left = 4
              Top = 2
              Width = 395
              Height = 17
              Caption = 'Insert Default Code into Empty Projects'
              TabOrder = 0
            end
          end
        end
      end
    end
    object tabClassBrowsing: TTabSheet
      Caption = 'Class browsing'
      object chkEnableClassBrowser: TCheckBox
        Left = 8
        Top = 4
        Width = 361
        Height = 17
        Caption = 'Enable class browser'
        TabOrder = 0
        OnClick = chkEnableClassBrowserClick
      end
      object ClassCodePage: TPageControl
        Left = 0
        Top = 24
        Width = 453
        Height = 319
        ActivePage = tabCBCompletion
        TabOrder = 1
        OnChange = ClassCodePageChange
        object tabCBBrowser: TTabSheet
          Caption = 'Class browsing'
          object lblClassBrowserSample: TLabel
            Left = 24
            Top = 156
            Width = 38
            Height = 13
            Caption = 'Sample:'
          end
          object ClassBrowser1: TClassBrowser
            Left = 68
            Top = 156
            Width = 333
            Height = 125
            Images = dmMain.ClassImages
            ReadOnly = True
            Indent = 19
            TabOrder = 2
            ShowFilter = sfAll
            ItemImages.Globals = -1
            ItemImages.Classes = 1
            ItemImages.VariablePrivate = 2
            ItemImages.VariableProtected = 3
            ItemImages.VariablePublic = 4
            ItemImages.VariablePublished = 4
            ItemImages.MethodPrivate = 5
            ItemImages.MethodProtected = 6
            ItemImages.MethodPublic = 7
            ItemImages.MethodPublished = 7
            ItemImages.InheritedMethodProtected = 0
            ItemImages.InheritedMethodPublic = 0
            ItemImages.InheritedVariableProtected = 0
            ItemImages.InheritedVariablePublic = 0
            UseColors = True
            ShowInheritedMembers = False
          end
          object gbCBEngine: TGroupBox
            Left = 8
            Top = 12
            Width = 433
            Height = 65
            Caption = 'Engine behaviour'
            TabOrder = 0
            object chkCBParseGlobalH: TCheckBox
              Left = 8
              Top = 40
              Width = 345
              Height = 17
              Caption = 'Scan global files referenced in #include'#39's'
              TabOrder = 1
            end
            object chkCBParseLocalH: TCheckBox
              Left = 8
              Top = 20
              Width = 345
              Height = 17
              Caption = 'Scan local files referenced in #include'#39's'
              TabOrder = 0
            end
          end
          object gbCBView: TGroupBox
            Left = 8
            Top = 84
            Width = 433
            Height = 65
            Caption = 'View options'
            TabOrder = 1
            object chkCBUseColors: TCheckBox
              Left = 8
              Top = 20
              Width = 345
              Height = 17
              Caption = 'Use colors'
              TabOrder = 0
              OnClick = chkCBUseColorsClick
            end
            object chkCBShowInherited: TCheckBox
              Left = 8
              Top = 40
              Width = 345
              Height = 17
              Caption = 'Show inherited members'
              TabOrder = 1
              OnClick = chkCBShowInheritedClick
            end
          end
        end
        object tabCBCompletion: TTabSheet
          Caption = 'Code completion'
          object lblCompletionDelay: TLabel
            Left = 8
            Top = 36
            Width = 52
            Height = 13
            Caption = 'Delay (ms):'
          end
          object cpCompletionBackground: TColorPickerButton
            Left = 301
            Top = 55
            Width = 112
            Height = 22
            CustomText = 'Custom'
            DefaultText = 'Default'
            PopupSpacing = 8
            ShowSystemColors = True
            OnChange = StyleChange
            OnDefaultSelect = DefaultSelect
            OnHint = PickerHint
          end
          object lblCompletionColor: TLabel
            Left = 296
            Top = 36
            Width = 87
            Height = 13
            Caption = 'Background color:'
          end
          object btnCCCnew: TSpeedButton
            Left = 8
            Top = 264
            Width = 101
            Height = 25
            Caption = 'Add files'
            OnClick = btnCCCnewClick
          end
          object btnCCCdelete: TSpeedButton
            Left = 112
            Top = 264
            Width = 101
            Height = 25
            Caption = 'Clear'
            OnClick = btnCCCdeleteClick
          end
          object tbCompletionDelay: TTrackBar
            Left = 16
            Top = 52
            Width = 257
            Height = 37
            Max = 10000
            Min = 1
            ParentShowHint = False
            Frequency = 500
            Position = 1000
            ShowHint = False
            TabOrder = 1
            TickMarks = tmBoth
            OnChange = tbCompletionDelayChange
          end
          object chkEnableCompletion: TCheckBox
            Left = 8
            Top = 12
            Width = 361
            Height = 17
            Caption = 'Enable code-completion'
            TabOrder = 0
            OnClick = chkEnableCompletionClick
          end
          object chkCCCache: TCheckBox
            Left = 8
            Top = 96
            Width = 433
            Height = 17
            Caption = 'Use code-completion cache'
            TabOrder = 2
            OnClick = chkCCCacheClick
          end
          object lbCCC: TListBox
            Left = 8
            Top = 116
            Width = 433
            Height = 145
            ItemHeight = 13
            ParentShowHint = False
            ShowHint = True
            Sorted = True
            TabOrder = 3
          end
          object pbCCCache: TProgressBar
            Left = 216
            Top = 264
            Width = 225
            Height = 25
            BorderWidth = 1
            TabOrder = 4
            Visible = False
          end
        end
      end
    end
    object tabAutosave: TTabSheet
      Caption = 'Autosave'
      ImageIndex = 5
      object EnableDisableAutosave: TCheckBox
        Left = 8
        Top = 8
        Width = 385
        Height = 17
        Caption = 'Enable editor autosave'
        TabOrder = 0
        OnClick = EnableDisableAutosaveClick
      end
      object OptionsGroup: TGroupBox
        Left = 8
        Top = 32
        Width = 441
        Height = 297
        Caption = ' Options '
        TabOrder = 1
        object SaveInterval: TLabel
          Left = 16
          Top = 30
          Width = 38
          Height = 13
          Caption = 'Interval:'
        end
        object MinutesDelay: TTrackBar
          Left = 136
          Top = 16
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
          Left = 18
          Top = 64
          Width = 239
          Height = 65
          Caption = 'Files'
          Items.Strings = (
            'Save the currently visible file'
            'Save all other open files too')
          TabOrder = 1
        end
      end
    end
  end
  object btnOk: TBitBtn
    Left = 200
    Top = 380
    Width = 80
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
    Left = 290
    Top = 380
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    TabOrder = 2
    OnClick = btnCancelClick
    Kind = bkCancel
  end
  object btnHelp: TBitBtn
    Left = 380
    Top = 380
    Width = 80
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
    Top = 378
  end
  object CppTokenizer: TCppTokenizer
    LogTokens = False
    Left = 40
    Top = 376
  end
  object CppParser: TCppParser
    Enabled = True
    OnTotalProgress = CppParser1TotalProgress
    Tokenizer = CppTokenizer
    ParseLocalHeaders = True
    ParseGlobalHeaders = True
    LogStatements = False
    OnStartParsing = CppParser1StartParsing
    OnEndParsing = CppParser1EndParsing
    Left = 80
    Top = 376
  end
end
