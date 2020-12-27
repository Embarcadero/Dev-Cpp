object EditorOptForm: TEditorOptForm
  Left = 879
  Top = 375
  BorderStyle = bsDialog
  Caption = 'Editor Options'
  ClientHeight = 492
  ClientWidth = 484
  Color = clWindow
  Ctl3D = False
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
        Top = 235
        Width = 201
        Height = 92
        Caption = '  Right Margin  '
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 2
        object lblMarginWidth: TLabel
          Left = 8
          Top = 41
          Width = 32
          Height = 15
          Caption = 'Width'
        end
        object lblMarginColor: TLabel
          Left = 99
          Top = 41
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
          Top = 59
          Width = 97
          Height = 22
          DefaultColorColor = cl3DLight
          Style = [cbStandardColors, cbCustomColor, cbPrettyNames]
          TabOrder = 2
        end
      end
      object grpEditorOpts: TGroupBox
        Left = 9
        Top = 3
        Width = 456
        Height = 222
        Caption = '  Editor Options  '
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 0
        object lbNewDocEncoding: TLabel
          Left = 8
          Top = 190
          Width = 139
          Height = 15
          Caption = 'New Document Encoding:'
        end
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
        object cbNewDocEncoding: TComboBox
          Left = 182
          Top = 186
          Width = 267
          Height = 23
          Style = csDropDownList
          TabOrder = 16
        end
      end
      object grpCaret: TGroupBox
        Left = 9
        Top = 235
        Width = 248
        Height = 92
        Caption = '  Caret  '
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 1
        object lblInsertCaret: TLabel
          Left = 8
          Top = 17
          Width = 61
          Height = 15
          Caption = 'Insert caret:'
        end
        object lblOverCaret: TLabel
          Left = 8
          Top = 41
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
          TabOrder = 0
          Items.Strings = (
            'Vertical Line'
            'Horizontal Line'
            'Half Block'
            'Block')
        end
        object cboOverwriteCaret: TComboBox
          Left = 136
          Top = 38
          Width = 100
          Height = 23
          Style = csDropDownList
          TabOrder = 1
          Items.Strings = (
            'Vertical Line'
            'Horizontal Line'
            'Half Block'
            'Block')
        end
        object cbMatch: TCheckBox
          Left = 8
          Top = 65
          Width = 233
          Height = 17
          Caption = 'Highlight matching symbols'
          TabOrder = 2
        end
      end
      object grpHighCurLine: TGroupBox
        Left = 264
        Top = 338
        Width = 201
        Height = 84
        Caption = 'Highlight current line'
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 4
        object cbHighlightColor: TLabel
          Left = 99
          Top = 32
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
          Top = 51
          Width = 97
          Height = 22
          DefaultColorColor = 16777164
          Style = [cbStandardColors, cbIncludeDefault, cbCustomColor, cbPrettyNames]
          TabOrder = 1
        end
      end
      object grpTabs: TGroupBox
        Left = 9
        Top = 338
        Width = 248
        Height = 84
        Caption = '  Tabs  '
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 3
        object lblTabSize: TLabel
          Left = 176
          Top = 32
          Width = 44
          Height = 15
          Caption = 'Tab Size:'
        end
        object seTabSize: TSpinEdit
          Left = 176
          Top = 51
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
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
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
        Top = 155
        Width = 456
        Height = 189
        Caption = '  Gutter  '
        TabOrder = 1
        DesignSize = (
          456
          189)
        object lblGutterFont: TLabel
          Left = 8
          Top = 95
          Width = 27
          Height = 15
          Anchors = [akLeft, akRight, akBottom]
          Caption = 'Font:'
          ExplicitTop = 91
        end
        object lblGutterWidth: TLabel
          Left = 360
          Top = 38
          Width = 89
          Height = 14
          Anchors = [akLeft, akRight, akBottom]
          AutoSize = False
          Caption = 'Gutter Width'
          WordWrap = True
        end
        object lblGutterFontSize: TLabel
          Left = 360
          Top = 93
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
          Top = 113
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
          Top = 112
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
          Top = 57
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
        Height = 141
        Caption = '  Editor Font  '
        TabOrder = 0
        DesignSize = (
          456
          141)
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
          Top = 37
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
          Top = 36
          Width = 80
          Height = 24
          MaxValue = 999
          MinValue = 1
          TabOrder = 1
          Value = 10
          OnChange = edEditorSizeChange
        end
        object cbLigatures: TCheckBox
          Left = 8
          Top = 114
          Width = 345
          Height = 15
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Use font ligatures is available'
          Checked = True
          State = cbChecked
          TabOrder = 2
        end
      end
    end
    object tabSyntax: TTabSheet
      Caption = 'Colors'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
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
        Left = 11
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
        ImageIndex = 6
        ImageName = 'iconsnew-04'
        Images = dmMain.SVGImageListMenuStyle
        Flat = True
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
        Ctl3D = False
        ParentCtl3D = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        Font.Quality = fqClearTypeNatural
        TabOrder = 3
        CodeFolding.ShowCollapsedLine = True
        UseCodeFolding = False
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
        Height = 167
        Ctl3D = False
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
        ParentCtl3D = False
        TabOrder = 0
        OnClick = ElementListClick
      end
      object grpStyle: TGroupBox
        Left = 15
        Top = 275
        Width = 110
        Height = 77
        Caption = '  Style:  '
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 2
        object cbBold: TCheckBox
          Left = 8
          Top = 17
          Width = 100
          Height = 17
          Caption = 'Bold'
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 0
          OnClick = StyleChange
        end
        object cbItalic: TCheckBox
          Left = 8
          Top = 35
          Width = 100
          Height = 17
          Caption = 'Italic'
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 1
          OnClick = StyleChange
        end
        object cbUnderlined: TCheckBox
          Left = 8
          Top = 54
          Width = 100
          Height = 17
          Caption = 'Underlined'
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 2
          OnClick = StyleChange
        end
      end
      object cboQuickColor: TComboBox
        Left = 40
        Top = 387
        Width = 133
        Height = 23
        Style = csDropDownList
        Ctl3D = False
        ParentCtl3D = False
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
          'PlasticCodeWrap'
          'Monokai'
          'Monokai Fresh'
          'Visual Studio Dark')
      end
      object edSyntaxExt: TEdit
        Left = 182
        Top = 386
        Width = 285
        Height = 21
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 4
      end
      object cbSyntaxHighlight: TCheckBox
        Left = 182
        Top = 360
        Width = 300
        Height = 17
        Caption = 'Use Syntax Highlighting'
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 5
        OnClick = cbSyntaxHighlightClick
      end
      object cpForeground: TColorBox
        Left = 17
        Top = 202
        Width = 110
        Height = 22
        Style = [cbStandardColors, cbCustomColor, cbPrettyNames]
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 6
        OnChange = StyleChange
      end
      object cpBackground: TColorBox
        Left = 17
        Top = 246
        Width = 110
        Height = 22
        Style = [cbStandardColors, cbCustomColor, cbPrettyNames]
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 7
        OnChange = StyleChange
      end
    end
    object tabCode: TTabSheet
      Caption = 'Snippets'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
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
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
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
            Ctl3D = False
            ParentCtl3D = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Courier New'
            Font.Style = []
            Font.Quality = fqClearTypeNatural
            TabOrder = 2
            TabStop = False
            CodeFolding.ShowCollapsedLine = True
            UseCodeFolding = False
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
          object lvCodeIns: TStringGrid
            Left = 8
            Top = 8
            Width = 369
            Height = 137
            ColCount = 3
            Ctl3D = False
            DefaultColWidth = 115
            DefaultRowHeight = 18
            FixedCols = 0
            RowCount = 2
            Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs, goAlwaysShowEditor]
            ParentCtl3D = False
            ScrollBars = ssVertical
            TabOrder = 3
            OnSelectCell = lvCodeInsSelectCell
          end
        end
        object tabCPDefault: TTabSheet
          Caption = 'Default Insert'
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object cbDefaultCode: TCheckBox
            Left = 4
            Top = 2
            Width = 461
            Height = 17
            Caption = 'Insert Default Code into Empty Projects'
            Ctl3D = False
            ParentCtl3D = False
            TabOrder = 0
          end
          object seDefault: TSynEdit
            Left = 0
            Top = 24
            Width = 468
            Height = 331
            Ctl3D = False
            ParentCtl3D = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Courier New'
            Font.Style = []
            Font.Quality = fqClearTypeNatural
            TabOrder = 1
            TabStop = False
            CodeFolding.ShowCollapsedLine = True
            UseCodeFolding = False
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
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object PagesCompletion: TPageControl
        Left = 0
        Top = 0
        Width = 477
        Height = 425
        ActivePage = tabSymbolCompletion
        Align = alClient
        TabOrder = 0
        object tabCodeCompletion: TTabSheet
          Caption = 'Code Completion'
          ImageIndex = 1
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
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
          object chkEnableCompletion: TCheckBox
            Left = 8
            Top = 8
            Width = 457
            Height = 17
            Caption = 'Enable code-completion'
            Ctl3D = False
            ParentCtl3D = False
            TabOrder = 0
            OnClick = chkEnableCompletionClick
          end
          object cpCompletionBackground: TColorBox
            Left = 333
            Top = 128
            Width = 112
            Height = 22
            DefaultColorColor = clWhite
            Style = [cbStandardColors, cbCustomColor, cbPrettyNames]
            Ctl3D = False
            ParentCtl3D = False
            TabOrder = 1
          end
          object gbCBEngine: TGroupBox
            Left = 8
            Top = 36
            Width = 457
            Height = 65
            Caption = 'Engine behaviour'
            TabOrder = 2
            object chkCBParseLocalH: TCheckBox
              Left = 8
              Top = 20
              Width = 345
              Height = 17
              Caption = 'Scan local files referenced in #include'#39's'
              Ctl3D = False
              ParentCtl3D = False
              TabOrder = 0
            end
            object chkCBParseGlobalH: TCheckBox
              Left = 8
              Top = 40
              Width = 345
              Height = 17
              Caption = 'Scan global files referenced in #include'#39's'
              Ctl3D = False
              ParentCtl3D = False
              TabOrder = 1
            end
          end
          object tbCompletionDelay: TTrackBar
            Left = 16
            Top = 132
            Width = 297
            Height = 37
            Ctl3D = False
            Max = 2000
            Min = 1
            ParentCtl3D = False
            ParentShowHint = False
            Frequency = 50
            Position = 1000
            ShowHint = False
            TabOrder = 3
            TickMarks = tmBoth
            OnChange = tbCompletionDelayChange
          end
        end
        object tabSymbolCompletion: TTabSheet
          Caption = 'Symbol Completion'
          ImageIndex = 2
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object grpSpecific: TGroupBox
            Left = 16
            Top = 40
            Width = 273
            Height = 201
            Caption = 'Specific completion options'
            Ctl3D = False
            ParentCtl3D = False
            TabOrder = 1
            object cbParenth: TCheckBox
              Left = 16
              Top = 50
              Width = 240
              Height = 17
              Caption = 'Complete parentheses '#39'()'#39
              Ctl3D = False
              ParentCtl3D = False
              TabOrder = 1
            end
            object cbBraces: TCheckBox
              Left = 16
              Top = 26
              Width = 240
              Height = 17
              Caption = 'Complete braces '#39'{}'#39
              Ctl3D = False
              ParentCtl3D = False
              TabOrder = 0
            end
            object cbInclude: TCheckBox
              Left = 16
              Top = 74
              Width = 240
              Height = 17
              Caption = 'Complete includes '#39'<>'#39
              Ctl3D = False
              ParentCtl3D = False
              TabOrder = 2
            end
            object cbComments: TCheckBox
              Left = 16
              Top = 122
              Width = 240
              Height = 17
              Caption = 'Complete multiline comments '#39'/**/'#39
              Ctl3D = False
              ParentCtl3D = False
              TabOrder = 4
            end
            object cbArray: TCheckBox
              Left = 16
              Top = 98
              Width = 240
              Height = 17
              Caption = 'Complete square braces '#39'[]'#39
              Ctl3D = False
              ParentCtl3D = False
              TabOrder = 3
            end
            object cbSingleQuotes: TCheckBox
              Left = 16
              Top = 146
              Width = 240
              Height = 17
              Caption = 'Complete single quotes '#39#39#39#39
              Ctl3D = False
              ParentCtl3D = False
              TabOrder = 5
            end
            object cbDoubleQuotes: TCheckBox
              Left = 16
              Top = 170
              Width = 240
              Height = 17
              Caption = 'Complete double quotes '#39'""'#39
              Ctl3D = False
              ParentCtl3D = False
              TabOrder = 6
            end
          end
          object cbSymbolComplete: TCheckBox
            Left = 8
            Top = 8
            Width = 465
            Height = 17
            Caption = 'Enable symbol completion'
            Ctl3D = False
            ParentCtl3D = False
            TabOrder = 0
            OnClick = cbSymbolCompleteClick
          end
          object cbDeleteCompleted: TCheckBox
            Left = 8
            Top = 256
            Width = 449
            Height = 17
            Caption = 'Delete completed symbols as pairs'
            Ctl3D = False
            ParentCtl3D = False
            TabOrder = 2
          end
        end
      end
    end
    object tabAutosave: TTabSheet
      Caption = 'Autosave'
      ImageIndex = 5
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object cbAutoSave: TCheckBox
        Left = 8
        Top = 8
        Width = 457
        Height = 17
        Caption = 'Enable editor autosave'
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 0
        OnClick = cbAutoSaveClick
      end
      object OptionsGroup: TGroupBox
        Left = 8
        Top = 32
        Width = 457
        Height = 385
        Caption = ' Options '
        Ctl3D = False
        ParentCtl3D = False
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
          Width = 48
          Height = 15
          Caption = 'Example:'
        end
        object MinutesDelay: TTrackBar
          Left = 151
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
          Height = 100
          Caption = 'Files'
          Items.Strings = (
            'Save only the currently visible file'
            'Save all open files after each interval'
            'Save all project files')
          TabOrder = 1
        end
        object NameOptions: TRadioGroup
          Left = 16
          Top = 190
          Width = 239
          Height = 100
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
    Images = dmMain.SVGImageListMenuStyle
    ModalResult = 1
    TabOrder = 1
    OnClick = btnOkClick
  end
  object btnCancel: TBitBtn
    Left = 300
    Top = 460
    Width = 85
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    NumGlyphs = 2
    TabOrder = 2
    OnClick = btnCancelClick
  end
  object btnHelp: TBitBtn
    Left = 390
    Top = 460
    Width = 85
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    Enabled = False
    NumGlyphs = 2
    TabOrder = 3
    OnClick = btnHelpClick
  end
  object cpp: TSynCppSyn
    DefaultFilter = 'C++ Files (*.c,*.cpp,*.h,*.hpp)|*.c;*.cpp;*.h;*.hpp'
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 5
    Top = 458
  end
end
