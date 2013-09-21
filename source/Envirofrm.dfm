object EnviroForm: TEnviroForm
  Left = 743
  Top = 198
  BorderStyle = bsDialog
  Caption = 'Environment Options'
  ClientHeight = 404
  ClientWidth = 418
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  DesignSize = (
    418
    404)
  PixelsPerInch = 96
  TextHeight = 13
  object PagesMain: TdevPages
    Left = 8
    Top = 8
    Width = 400
    Height = 353
    ActivePage = tabGeneral
    Anchors = [akLeft, akTop, akRight, akBottom]
    OnChange = PagesMainChange
    object tabGeneral: TdevPage
      Left = 0
      Top = 23
      Width = 400
      Height = 330
      HorzScrollBar.Smooth = True
      HorzScrollBar.Tracking = True
      VertScrollBar.Smooth = True
      VertScrollBar.Tracking = True
      Align = alClient
      BevelKind = bkTile
      ParentShowHint = False
      ShowHint = False
      TabOrder = 0
      Caption = 'General'
      DesignSize = (
        396
        326)
      object cbBackups: TCheckBox
        Left = 8
        Top = 29
        Width = 379
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Create File Backups'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
      end
      object cbMinOnRun: TCheckBox
        Left = 8
        Top = 50
        Width = 379
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Minimize on Run'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
      end
      object cbDefCpp: TCheckBox
        Left = 8
        Top = 8
        Width = 379
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Default to C++ on New Project'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
      end
      object cbShowBars: TCheckBox
        Left = 8
        Top = 72
        Width = 379
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Show Toolbars in Full Screen'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
      end
      object cbShowMenu: TCheckBox
        Left = 8
        Top = 93
        Width = 379
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Show Menu in Full Screen'
        Enabled = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
      end
      object rgbAutoOpen: TRadioGroup
        Left = 8
        Top = 141
        Width = 189
        Height = 90
        Caption = '   Auto Open  '
        Items.Strings = (
          'All Project Files'
          'Only First Project File'
          'None')
        TabOrder = 5
      end
      object cbdblFiles: TCheckBox
        Left = 8
        Top = 114
        Width = 379
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Double Click to Open Files'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 6
      end
      object gbDebugger: TGroupBox
        Left = 207
        Top = 141
        Width = 182
        Height = 90
        Caption = '  Debug Variable Browser  '
        TabOrder = 7
        object cbWatchHint: TCheckBox
          Left = 8
          Top = 22
          Width = 170
          Height = 27
          Caption = 'Watch variable under mouse'
          TabOrder = 0
        end
        object cbWatchError: TCheckBox
          Left = 8
          Top = 56
          Width = 170
          Height = 17
          Caption = 'Report watch errors'
          TabOrder = 1
        end
      end
    end
    object tabInterface: TdevPage
      Left = 0
      Top = 23
      Width = 400
      Height = 330
      HorzScrollBar.Smooth = True
      HorzScrollBar.Tracking = True
      VertScrollBar.Smooth = True
      VertScrollBar.Tracking = True
      Align = alClient
      BevelKind = bkTile
      ParentShowHint = False
      ShowHint = False
      TabOrder = 2
      Visible = False
      Caption = 'Interface'
      object lblLang: TLabel
        Left = 224
        Top = 8
        Width = 170
        Height = 13
        AutoSize = False
        Caption = 'Language'
      end
      object lblTheme: TLabel
        Left = 224
        Top = 64
        Width = 170
        Height = 13
        AutoSize = False
        Caption = 'Theme :'
      end
      object lblmsgTabs: TLabel
        Left = 8
        Top = 64
        Width = 210
        Height = 13
        AutoSize = False
        Caption = 'Message Window Tabs:'
      end
      object lblMRU: TLabel
        Left = 8
        Top = 8
        Width = 210
        Height = 13
        AutoSize = False
        Caption = 'Max Files in History List:'
      end
      object cboLang: TComboBox
        Left = 231
        Top = 25
        Width = 154
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
      end
      object cboTheme: TComboBox
        Left = 232
        Top = 80
        Width = 153
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 1
      end
      object cboTabsTop: TComboBox
        Left = 12
        Top = 80
        Width = 75
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 2
        Items.Strings = (
          'Bottom'
          'Top')
      end
      object seMRUMax: TSpinEdit
        Left = 12
        Top = 24
        Width = 60
        Height = 22
        MaxLength = 2
        MaxValue = 12
        MinValue = 0
        TabOrder = 3
        Value = 0
      end
      object rgbOpenStyle: TRadioGroup
        Left = 8
        Top = 155
        Width = 186
        Height = 90
        Caption = '  Open/Save Dialog Style  '
        Items.Strings = (
          'Windows 2k (sidebar)'
          'Windows 9x'
          'Windows 3.1')
        TabOrder = 4
      end
      object cbNoSplashScreen: TCheckBox
        Left = 8
        Top = 112
        Width = 289
        Height = 17
        Caption = 'No Splash Screen on startup'
        TabOrder = 5
      end
      object cbXPTheme: TCheckBox
        Left = 8
        Top = 128
        Width = 297
        Height = 17
        Caption = 'Use XP Theme'
        TabOrder = 6
      end
      object gbProgress: TGroupBox
        Left = 203
        Top = 155
        Width = 184
        Height = 90
        Caption = 'Compilation Progress Window '
        TabOrder = 7
        object cbShowProgress: TCheckBox
          Left = 14
          Top = 30
          Width = 163
          Height = 17
          Caption = '&Show during compilation'
          TabOrder = 0
        end
        object cbAutoCloseProgress: TCheckBox
          Left = 14
          Top = 54
          Width = 163
          Height = 17
          Caption = '&Auto close after compile'
          TabOrder = 1
        end
      end
    end
    object tabPaths: TdevPage
      Left = 0
      Top = 23
      Width = 400
      Height = 330
      HorzScrollBar.Smooth = True
      HorzScrollBar.Tracking = True
      VertScrollBar.Smooth = True
      VertScrollBar.Tracking = True
      Align = alClient
      BevelKind = bkTile
      ParentShowHint = False
      ShowHint = False
      TabOrder = 1
      Visible = False
      Caption = 'Files && Directories'
      object lblUserDir: TLabel
        Left = 8
        Top = 93
        Width = 111
        Height = 13
        Caption = 'User'#39's Default Directory'
      end
      object lblTemplatesDir: TLabel
        Left = 8
        Top = 141
        Width = 94
        Height = 13
        Caption = 'Templates Directory'
      end
      object lblSplash: TLabel
        Left = 8
        Top = 279
        Width = 101
        Height = 13
        Caption = 'Splash Screen Image'
      end
      object lblIcoLib: TLabel
        Left = 8
        Top = 189
        Width = 80
        Height = 13
        Caption = 'Icon Library Path'
      end
      object lblLangPath: TLabel
        Left = 8
        Top = 234
        Width = 73
        Height = 13
        Caption = 'Language Path'
      end
      object btnDefBrws: TSpeedButton
        Tag = 1
        Left = 366
        Top = 108
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
        OnClick = BrowseClick
      end
      object btnOutputbrws: TSpeedButton
        Tag = 2
        Left = 365
        Top = 157
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
        OnClick = BrowseClick
      end
      object btnBrwIcon: TSpeedButton
        Tag = 3
        Left = 365
        Top = 205
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
        OnClick = BrowseClick
      end
      object btnBrwLang: TSpeedButton
        Tag = 5
        Left = 365
        Top = 250
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
        OnClick = BrowseClick
      end
      object btnBrwSplash: TSpeedButton
        Tag = 4
        Left = 365
        Top = 295
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
        OnClick = BrowseClick
      end
      object edUserDir: TEdit
        Left = 16
        Top = 109
        Width = 344
        Height = 21
        ReadOnly = True
        TabOrder = 1
        Text = 'edUserDir'
      end
      object edTemplatesDir: TEdit
        Left = 16
        Top = 157
        Width = 344
        Height = 21
        ReadOnly = True
        TabOrder = 2
        Text = 'edTemplatesDir'
      end
      object edSplash: TEdit
        Left = 16
        Top = 295
        Width = 344
        Height = 21
        ReadOnly = True
        TabOrder = 3
        Text = 'edSplash'
      end
      object edIcoLib: TEdit
        Left = 16
        Top = 205
        Width = 344
        Height = 21
        ReadOnly = True
        TabOrder = 4
        Text = 'edIcoLib'
      end
      object edLang: TEdit
        Left = 16
        Top = 250
        Width = 344
        Height = 21
        ReadOnly = True
        TabOrder = 5
        Text = 'edLang'
      end
      object gbAltConfig: TGroupBox
        Left = 8
        Top = 12
        Width = 381
        Height = 73
        Caption = ' Alternate Configuration File '
        TabOrder = 0
        object btnAltConfig: TSpeedButton
          Tag = 7
          Left = 350
          Top = 42
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
          OnClick = BrowseClick
        end
        object chkAltConfig: TCheckBox
          Left = 12
          Top = 20
          Width = 361
          Height = 17
          Caption = 'Use this alternate configuration file'
          TabOrder = 0
          OnClick = chkAltConfigClick
        end
        object edAltConfig: TEdit
          Left = 32
          Top = 42
          Width = 313
          Height = 21
          TabOrder = 1
          Text = 'edAltConfig'
        end
      end
    end
    object tabExternal: TdevPage
      Left = 0
      Top = 23
      Width = 400
      Height = 330
      HorzScrollBar.Smooth = True
      HorzScrollBar.Tracking = True
      VertScrollBar.Smooth = True
      VertScrollBar.Tracking = True
      Align = alClient
      BevelKind = bkTile
      TabOrder = 5
      Visible = False
      Caption = 'External Programs'
      DesignSize = (
        396
        326)
      object lblExternal: TLabel
        Left = 8
        Top = 8
        Width = 148
        Height = 13
        Caption = 'External programs associations:'
      end
      object btnExtAdd: TSpeedButton
        Left = 98
        Top = 286
        Width = 99
        Height = 25
        Anchors = [akBottom]
        Caption = 'Add'
        OnClick = btnExtAddClick
      end
      object btnExtDel: TSpeedButton
        Left = 199
        Top = 286
        Width = 99
        Height = 25
        Anchors = [akBottom]
        Caption = 'Delete'
        OnClick = btnExtDelClick
      end
      object vleExternal: TValueListEditor
        Left = 16
        Top = 24
        Width = 360
        Height = 255
        Anchors = [akLeft, akTop, akRight, akBottom]
        KeyOptions = [keyEdit, keyAdd, keyDelete]
        Options = [goVertLine, goHorzLine, goEditing, goAlwaysShowEditor, goThumbTracking]
        TabOrder = 0
        TitleCaptions.Strings = (
          'Extension'
          'External program')
        OnEditButtonClick = vleExternalEditButtonClick
        OnValidate = vleExternalValidate
        ColWidths = (
          72
          282)
      end
    end
    object tabAssocs: TdevPage
      Left = 0
      Top = 23
      Width = 400
      Height = 330
      HorzScrollBar.Smooth = True
      HorzScrollBar.Tracking = True
      VertScrollBar.Smooth = True
      VertScrollBar.Tracking = True
      Align = alClient
      BevelKind = bkTile
      ParentShowHint = False
      ShowHint = False
      TabOrder = 3
      Visible = False
      Caption = 'File Associations'
      DesignSize = (
        396
        326)
      object lblAssocFileTypes: TLabel
        Left = 8
        Top = 8
        Width = 51
        Height = 13
        Caption = 'File Types:'
      end
      object lblAssocDesc: TLabel
        Left = 16
        Top = 262
        Width = 355
        Height = 49
        Anchors = [akLeft, akRight, akBottom]
        AutoSize = False
        Caption = 
          'Just check or un-check for which file types Dev-C++ will be regi' +
          'stered as the default application to open them...'
        WordWrap = True
      end
      object lstAssocFileTypes: TCheckListBox
        Left = 16
        Top = 24
        Width = 356
        Height = 235
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        TabOrder = 0
      end
    end
    object tabCVS: TdevPage
      Left = 0
      Top = 23
      Width = 400
      Height = 330
      HorzScrollBar.Smooth = True
      HorzScrollBar.Tracking = True
      VertScrollBar.Smooth = True
      VertScrollBar.Tracking = True
      Align = alClient
      BevelKind = bkTile
      ParentShowHint = False
      ShowHint = False
      TabOrder = 4
      Visible = False
      Caption = 'CVS Support'
      object lblCVSExec: TLabel
        Left = 8
        Top = 9
        Width = 82
        Height = 13
        Caption = 'CVS Program File'
      end
      object lblCVSCompression: TLabel
        Left = 8
        Top = 53
        Width = 89
        Height = 13
        Caption = 'Compression Level'
      end
      object btnCVSExecBrws: TSpeedButton
        Tag = 6
        Left = 365
        Top = 25
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
        OnClick = BrowseClick
      end
      object edCVSExec: TEdit
        Left = 16
        Top = 25
        Width = 345
        Height = 21
        TabOrder = 0
        Text = 'edCVSExec'
      end
      object spnCVSCompression: TSpinEdit
        Left = 16
        Top = 68
        Width = 53
        Height = 22
        MaxLength = 1
        MaxValue = 9
        MinValue = 0
        TabOrder = 1
        Value = 0
      end
      object chkCVSUseSSH: TCheckBox
        Left = 16
        Top = 100
        Width = 373
        Height = 17
        Caption = 'Use SSH instead of RSH'
        Checked = True
        State = cbChecked
        TabOrder = 2
      end
    end
  end
  object btnOk: TBitBtn
    Left = 161
    Top = 372
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
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
    Left = 241
    Top = 372
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    TabOrder = 3
    Kind = bkCancel
  end
  object btnHelp: TBitBtn
    Left = 332
    Top = 372
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    TabOrder = 1
    OnClick = btnHelpClick
    Kind = bkHelp
  end
  object dlgPic: TOpenPictureDialog
    Left = 134
    Top = 298
  end
  object XPMenu: TXPMenu
    DimLevel = 30
    GrayLevel = 10
    Font.Charset = ANSI_CHARSET
    Font.Color = clMenuText
    Font.Height = -11
    Font.Name = 'Microsoft Sans Serif'
    Font.Style = []
    Color = clBtnFace
    DrawMenuBar = False
    IconBackColor = clBtnFace
    MenuBarColor = clBtnFace
    SelectColor = clHighlight
    SelectBorderColor = clHighlight
    SelectFontColor = clMenuText
    DisabledColor = clInactiveCaption
    SeparatorColor = clBtnFace
    CheckedColor = clHighlight
    IconWidth = 24
    DrawSelect = True
    UseSystemColors = True
    UseDimColor = False
    OverrideOwnerDraw = False
    Gradient = False
    FlatMenu = False
    AutoDetect = True
    Active = False
    Left = 168
    Top = 296
  end
end
