object LangForm: TLangForm
  Left = 485
  Top = 245
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Dev-C++ first time configuration'
  ClientHeight = 295
  ClientWidth = 416
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object OkBtn: TBitBtn
    Left = 144
    Top = 264
    Width = 265
    Height = 25
    Caption = '&Next'
    Default = True
    TabOrder = 0
    OnClick = OkBtnClick
    Glyph.Data = {
      36030000424D3603000000000000360000002800000010000000100000000100
      18000000000000030000120B0000120B00000000000000000000BFBFBFBFBFBF
      BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
      BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF000000BFBFBFBF
      BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
      BFBFBFBFBFBF000000009836000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
      BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF00000000A13900A13900983600
      0000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
      0000008FFF8F00C54600B03F00B03F009836000000BFBFBFBFBFBFBFBFBFBFBF
      BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF0000008FFF8F00C54600B03F00
      B03F009836000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
      BFBFBFBFBFBF0000008FFF8F00C54600B03F00B03F009836000000BFBFBFBFBF
      BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF0000008FFF8F00
      B03F00B03F00A139009836000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
      BFBFBFBFBFBFBFBFBFBFBFBF00000000B03F00B03F00B03F00A1390098360000
      00BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF00000000B03F00
      B03F00B03F00B03F00A139000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
      BFBFBFBFBFBF00000000C54600B03F00B03F00B03F00A139000000BFBFBFBFBF
      BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF00000000C54600C54600B03F00
      B03F00B03F000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
      0000008FFF8F00DD0000C54600C54600C546000000BFBFBFBFBFBFBFBFBFBFBF
      BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF0000008FFF8F00DD0000C54600
      0000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
      BFBFBFBFBFBF0000008FFF8F000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
      BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF000000BFBFBFBF
      BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF}
  end
  object PicPanel: TPanel
    Left = 8
    Top = 8
    Width = 128
    Height = 281
    BevelInner = bvLowered
    BevelOuter = bvNone
    TabOrder = 1
    object Image2: TImage
      Left = 1
      Top = 1
      Width = 126
      Height = 279
      Align = alClient
      AutoSize = True
    end
  end
  object FirstPanel: TPanel
    Left = 144
    Top = 8
    Width = 266
    Height = 249
    BevelOuter = bvNone
    TabOrder = 2
    object Label1: TLabel
      Left = 0
      Top = 0
      Width = 261
      Height = 39
      Caption = 
        'This is the first time you have launched Dev-C++. You may config' +
        'ure the startup settings now, or later change them from the Envi' +
        'ronment options in the Tools menu.'
      WordWrap = True
    end
    object GroupBox1: TGroupBox
      Left = 0
      Top = 48
      Width = 265
      Height = 121
      Caption = 'Select your language :'
      TabOrder = 1
    end
    object ThemeGroupBox: TGroupBox
      Left = 0
      Top = 176
      Width = 265
      Height = 73
      Caption = 'Select your Dev-C++ theme :'
      TabOrder = 2
      object ThemeBox: TComboBox
        Left = 16
        Top = 24
        Width = 153
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnChange = ThemeBoxChange
      end
      object PreviewBtn: TBitBtn
        Left = 178
        Top = 24
        Width = 75
        Height = 22
        Caption = '&Preview'
        TabOrder = 1
        OnClick = PreviewBtnClick
      end
      object XPCheckBox: TCheckBox
        Left = 16
        Top = 50
        Width = 97
        Height = 17
        Caption = '&Use XP Theme'
        TabOrder = 2
        OnClick = XPCheckBoxClick
      end
    end
    object ListBox: TListBox
      Left = 8
      Top = 64
      Width = 249
      Height = 97
      ItemHeight = 13
      TabOrder = 0
    end
  end
  object FinishPanel: TPanel
    Left = 144
    Top = 8
    Width = 266
    Height = 249
    BevelOuter = bvNone
    TabOrder = 5
    Visible = False
    object Label6: TLabel
      Left = 4
      Top = 0
      Width = 250
      Height = 26
      Caption = 
        'Dev-C++ has been configured successfully, you may now click OK t' +
        'o proceed to its loading.'
      WordWrap = True
    end
    object Label4: TLabel
      Left = 5
      Top = 40
      Width = 251
      Height = 52
      Caption = 
        'If you need help using Dev-C++, please refer to the Dev-C++ help' +
        ' file in the Help menu. You will also find there a FAQ (which is' +
        ' very important to read in case you have a problem) and a C tuto' +
        'rial.'
      WordWrap = True
    end
    object Label7: TLabel
      Left = 4
      Top = 104
      Width = 251
      Height = 52
      Caption = 
        'You can also download packages (like libraries or tools) to use ' +
        'with Dev-C++, and upgrade to the latest version by using WebUpda' +
        'te, which you will find in Tools menu, Check for Updates/Package' +
        's.'
      WordWrap = True
    end
  end
  object CachePanel: TPanel
    Left = 144
    Top = 8
    Width = 266
    Height = 249
    BevelOuter = bvNone
    TabOrder = 3
    Visible = False
    object Label2: TLabel
      Left = 4
      Top = 0
      Width = 249
      Height = 39
      Caption = 
        'You decided to use the code completion feature. To optimize this' +
        ' process, it is recommended to create a cache of the standard he' +
        'aders files.'
      WordWrap = True
    end
    object Label3: TLabel
      Left = 4
      Top = 64
      Width = 247
      Height = 52
      Caption = 
        'Do you want to create the code completion cache now? This can t' +
        'ake several minutes. It is possible to create this cache later i' +
        'n Editor Options, Class Browsing, Completion.'
      WordWrap = True
    end
    object ProgressPanel: TPanel
      Left = 0
      Top = 134
      Width = 265
      Height = 114
      BevelOuter = bvNone
      TabOrder = 1
      Visible = False
      object ParseLabel: TLabel
        Left = 16
        Top = 24
        Width = 65
        Height = 13
        Caption = 'Parsing files...'
        WordWrap = True
      end
      object pbCCCache: TProgressBar
        Left = 14
        Top = 62
        Width = 235
        Height = 16
        Min = 0
        Max = 100
        TabOrder = 0
        Visible = False
      end
    end
    object BuildPanel: TPanel
      Left = 0
      Top = 134
      Width = 265
      Height = 114
      BevelOuter = bvNone
      TabOrder = 0
      object LoadBtn: TSpeedButton
        Left = 228
        Top = 79
        Width = 22
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
        OnClick = LoadBtnClick
      end
      object YesCache: TRadioButton
        Left = 8
        Top = 12
        Width = 241
        Height = 17
        Caption = 'Yes, create the cache now'
        Checked = True
        TabOrder = 0
        TabStop = True
      end
      object NoCache: TRadioButton
        Left = 8
        Top = 30
        Width = 233
        Height = 17
        Caption = 'No, do not create the cache'
        TabOrder = 1
      end
      object DirCheckBox: TCheckBox
        Left = 8
        Top = 60
        Width = 249
        Height = 17
        Caption = 'Use this directory instead of the standard one:'
        TabOrder = 2
        OnClick = DirCheckBoxClick
      end
      object DirEdit: TEdit
        Left = 32
        Top = 80
        Width = 192
        Height = 21
        Color = clInactiveCaptionText
        Enabled = False
        TabOrder = 3
      end
    end
  end
  object SecondPanel: TPanel
    Left = 144
    Top = 8
    Width = 266
    Height = 249
    BevelOuter = bvNone
    TabOrder = 4
    Visible = False
    object SecondLabel: TLabel
      Left = 4
      Top = 0
      Width = 258
      Height = 52
      Caption = 
        'Dev-C++ can retrieve information from headers files, to help you' +
        ' find  functions, classes and variables prototypes easily, throu' +
        'gh a class browser and a code completion list. '
      WordWrap = True
    end
    object Label5: TLabel
      Left = 4
      Top = 72
      Width = 235
      Height = 65
      Caption = 
        'Although this feature is useful, it requires more CPU power and ' +
        'memory, and may not be suitable for all developers. Do you want ' +
        'to use it ? You can enable or disable it later in Editor Options' +
        ', Class Browser.'
      WordWrap = True
    end
    object YesClassBrowser: TRadioButton
      Left = 20
      Top = 168
      Width = 169
      Height = 17
      Caption = 'Yes, I want to use this feature'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object NoClassBrowser: TRadioButton
      Left = 20
      Top = 192
      Width = 209
      Height = 17
      Caption = 'No, I prefer to use Dev-C++ without it'
      TabOrder = 1
    end
  end
  object PopupMenu: TPopupMenu
    Left = 96
    Top = 136
    object N1: TMenuItem
      ImageIndex = 0
    end
    object TMenuItem
      ImageIndex = 1
    end
    object TMenuItem
      ImageIndex = 2
    end
    object TMenuItem
      ImageIndex = 3
    end
    object TMenuItem
      ImageIndex = 4
    end
    object TMenuItem
      ImageIndex = 5
    end
    object TMenuItem
      ImageIndex = 6
    end
    object TMenuItem
      ImageIndex = 7
    end
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
    Left = 64
    Top = 136
  end
end
