object LangForm: TLangForm
  Left = 650
  Top = 146
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Dev-C++ first time configuration'
  ClientHeight = 320
  ClientWidth = 540
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object ThemeImage: TImage
    Left = 0
    Top = 0
    Width = 240
    Height = 320
    Center = True
  end
  object OkBtn: TBitBtn
    Left = 250
    Top = 280
    Width = 280
    Height = 30
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
  object FirstPanel: TPanel
    Left = 240
    Top = 0
    Width = 300
    Height = 275
    BevelOuter = bvNone
    TabOrder = 1
    object LanguageInfo: TLabel
      Left = 8
      Top = 230
      Width = 284
      Height = 48
      Alignment = taCenter
      AutoSize = False
      Caption = 
        'You can change the interface language at Tools, Environment Opti' +
        'ons and color styles at Tools, Editor Options, Colors.'
      WordWrap = True
    end
    object GroupBox1: TGroupBox
      Left = 16
      Top = 12
      Width = 268
      Height = 122
      Caption = 'Select your language:'
      TabOrder = 1
      object ListBox: TListBox
        Left = 8
        Top = 20
        Width = 250
        Height = 93
        ItemHeight = 15
        TabOrder = 0
      end
    end
    object ThemeGroupBox: TGroupBox
      Left = 16
      Top = 144
      Width = 268
      Height = 81
      Caption = 'Select your Dev-C++ theme:'
      TabOrder = 0
      object InterfaceLbl: TLabel
        Left = 8
        Top = 24
        Width = 46
        Height = 15
        Caption = 'Interface'
      end
      object EditorLbl: TLabel
        Left = 8
        Top = 52
        Width = 31
        Height = 15
        Caption = 'Editor'
      end
      object ThemeBox: TComboBox
        Left = 80
        Top = 22
        Width = 177
        Height = 23
        Style = csDropDownList
        ItemHeight = 15
        TabOrder = 0
        OnChange = ThemeChange
      end
      object EditorBox: TComboBox
        Left = 80
        Top = 50
        Width = 177
        Height = 23
        Style = csDropDownList
        ItemHeight = 15
        TabOrder = 1
        OnChange = ThemeChange
      end
    end
  end
  object FinishPanel: TPanel
    Left = 240
    Top = 0
    Width = 300
    Height = 275
    BevelOuter = bvNone
    TabOrder = 4
    Visible = False
    object Finish2: TLabel
      Left = 8
      Top = 64
      Width = 273
      Height = 73
      AutoSize = False
      Caption = 
        'If you need help using Dev-C++, please refer to the Dev-C++ help' +
        ' file in the Help menu. You will also find there a FAQ (which is' +
        ' very important to read in case you have a problem).'
      WordWrap = True
    end
    object Finish3: TLabel
      Left = 8
      Top = 146
      Width = 284
      Height = 75
      AutoSize = False
      Caption = 
        'You can also download packages (like libraries or tools) to use ' +
        'with Dev-C++, and upgrade to the latest version by using WebUpda' +
        'te, which you will find in Tools menu, Check for Updates/Package' +
        's.'
      WordWrap = True
    end
    object Finish1: TLabel
      Left = 8
      Top = 8
      Width = 284
      Height = 45
      AutoSize = False
      Caption = 
        'Dev-C++ has been configured successfully, you may now click OK t' +
        'o proceed to its loading.'
      WordWrap = True
    end
  end
  object CachePanel: TPanel
    Left = 240
    Top = 0
    Width = 300
    Height = 275
    BevelOuter = bvNone
    TabOrder = 2
    Visible = False
    object CacheInfo1: TLabel
      Left = 8
      Top = 8
      Width = 284
      Height = 49
      AutoSize = False
      Caption = 
        'To optimize code completion, it is recommended to create a cache' +
        ' of frequently used headers.'
      WordWrap = True
    end
    object CacheInfo2: TLabel
      Left = 8
      Top = 52
      Width = 284
      Height = 45
      AutoSize = False
      Caption = 
        'It is possible add headers to cache later at Tools, Editor Optio' +
        'ns, Class Browsing, Completion.'
      WordWrap = True
    end
    object ProgressPanel: TPanel
      Left = 0
      Top = 100
      Width = 300
      Height = 175
      BevelOuter = bvNone
      TabOrder = 1
      Visible = False
      object ParseLabel: TLabel
        Left = 8
        Top = 100
        Width = 284
        Height = 32
        AutoSize = False
        Caption = 'Parsing files...'
        WordWrap = True
      end
      object pbCCCache: TProgressBar
        Left = 8
        Top = 136
        Width = 284
        Height = 30
        Smooth = True
        TabOrder = 0
      end
    end
    object BuildPanel: TPanel
      Left = 0
      Top = 100
      Width = 300
      Height = 175
      BevelOuter = bvNone
      TabOrder = 0
      object YesCache: TRadioButton
        Left = 16
        Top = 8
        Width = 268
        Height = 17
        Caption = 'Cache all headers (please be patient)'
        TabOrder = 0
      end
      object NoCache: TRadioButton
        Left = 16
        Top = 152
        Width = 268
        Height = 17
        Caption = 'Don'#39't cache anything'
        TabOrder = 1
      end
      object AltCache: TRadioButton
        Left = 16
        Top = 32
        Width = 268
        Height = 17
        Caption = 'Only cache these files and their includes:'
        Checked = True
        TabOrder = 2
        TabStop = True
      end
      object AltFileList: TListBox
        Left = 8
        Top = 56
        Width = 201
        Height = 81
        Columns = 2
        ItemHeight = 15
        Items.Strings = (
          'assert.h'
          'complex.h'
          'ctype.h'
          'errno.h'
          'fenv.h'
          'float.h'
          'inttypes.h'
          'limits.h'
          'locale.h'
          'math.h'
          'setjmp.h'
          'signal.h'
          'stdarg.h'
          'stddef.h'
          'stdint.h'
          'stdio.h'
          'stdlib.h'
          'string.h'
          'time.h'
          'wchar.h'
          'wctype.h'
          'windows.h')
        MultiSelect = True
        TabOrder = 3
      end
      object ButtonAddFile: TButton
        Left = 212
        Top = 56
        Width = 80
        Height = 23
        Caption = 'Add file'
        TabOrder = 4
        OnClick = ButtonAddFileClick
      end
      object ButtonRemove: TButton
        Left = 212
        Top = 112
        Width = 80
        Height = 23
        Caption = 'Remove'
        TabOrder = 5
        OnClick = ButtonRemoveClick
      end
      object ButtonAddFolder: TButton
        Left = 212
        Top = 84
        Width = 80
        Height = 23
        Caption = 'Add folder'
        TabOrder = 6
        OnClick = ButtonAddFolderClick
      end
    end
  end
  object SecondPanel: TPanel
    Left = 240
    Top = 0
    Width = 300
    Height = 275
    BevelOuter = bvNone
    TabOrder = 3
    Visible = False
    object ClassBrowserInfo1: TLabel
      Left = 8
      Top = 8
      Width = 284
      Height = 60
      AutoSize = False
      Caption = 
        'Dev-C++ can retrieve information from header files to help you f' +
        'ind functions, classes and variables easily, through a class bro' +
        'wser and a code completion list.'
      WordWrap = True
    end
    object ClassBrowserInfo2: TLabel
      Left = 8
      Top = 88
      Width = 284
      Height = 60
      AutoSize = False
      Caption = 
        'Although this feature is useful, it requires more CPU power and ' +
        'memory. Do you want to use it? You can enable or disable it late' +
        'r in Tools, Editor Options, Class Browser.'
      WordWrap = True
    end
    object YesClassBrowser: TRadioButton
      Left = 16
      Top = 184
      Width = 268
      Height = 17
      Caption = 'Yes, I want to use this feature'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object NoClassBrowser: TRadioButton
      Left = 16
      Top = 208
      Width = 268
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
end
