object LangForm: TLangForm
  Left = 649
  Top = 343
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Dev-C++ first time configuration'
  ClientHeight = 295
  ClientWidth = 533
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
  object ThemeImage: TImage
    Left = 0
    Top = 0
    Width = 241
    Height = 295
    AutoSize = True
  end
  object OkBtn: TBitBtn
    Left = 248
    Top = 264
    Width = 281
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
  object FirstPanel: TPanel
    Left = 248
    Top = 8
    Width = 281
    Height = 249
    BevelOuter = bvNone
    TabOrder = 1
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
      Width = 281
      Height = 113
      Caption = 'Select your language:'
      TabOrder = 1
      object ListBox: TListBox
        Left = 8
        Top = 20
        Width = 265
        Height = 77
        ItemHeight = 13
        TabOrder = 0
      end
    end
    object ThemeGroupBox: TGroupBox
      Left = 0
      Top = 168
      Width = 281
      Height = 81
      Caption = 'Select your Dev-C++ theme:'
      TabOrder = 0
      object InterfaceLbl: TLabel
        Left = 8
        Top = 24
        Width = 42
        Height = 13
        Caption = 'Interface'
      end
      object EditorLbl: TLabel
        Left = 8
        Top = 48
        Width = 27
        Height = 13
        Caption = 'Editor'
      end
      object ThemeBox: TComboBox
        Left = 80
        Top = 22
        Width = 185
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnChange = ThemeChange
      end
      object EditorBox: TComboBox
        Left = 80
        Top = 46
        Width = 185
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 1
        OnChange = ThemeChange
      end
    end
  end
  object FinishPanel: TPanel
    Left = 248
    Top = 8
    Width = 281
    Height = 249
    BevelOuter = bvNone
    TabOrder = 4
    Visible = False
    object Label4: TLabel
      Left = 5
      Top = 48
      Width = 266
      Height = 52
      Caption = 
        'If you need help using Dev-C++, please refer to the Dev-C++ help' +
        ' file in the Help menu. You will also find there a FAQ (which is' +
        ' very important to read in case you have a problem).'
      WordWrap = True
    end
    object Label7: TLabel
      Left = 4
      Top = 120
      Width = 266
      Height = 52
      Caption = 
        'You can also download packages (like libraries or tools) to use ' +
        'with Dev-C++, and upgrade to the latest version by using WebUpda' +
        'te, which you will find in Tools menu, Check for Updates/Package' +
        's.'
      WordWrap = True
    end
    object Label6: TLabel
      Left = 5
      Top = 0
      Width = 250
      Height = 26
      Caption = 
        'Dev-C++ has been configured successfully, you may now click OK t' +
        'o proceed to its loading.'
      WordWrap = True
    end
  end
  object CachePanel: TPanel
    Left = 248
    Top = 8
    Width = 281
    Height = 249
    BevelOuter = bvNone
    TabOrder = 2
    Visible = False
    object Label2: TLabel
      Left = 4
      Top = 0
      Width = 254
      Height = 39
      Caption = 
        'To optimize code completion, it is recommended to create a cache' +
        ' of the standard headers files. This will allow you to browse th' +
        'ese headers too.'
      WordWrap = True
    end
    object Label3: TLabel
      Left = 4
      Top = 48
      Width = 252
      Height = 39
      Caption = 
        'It is possible to cache headers later in Editor Options, Class B' +
        'rowsing, Completion. Caching all headers can take several minute' +
        's when using TDM-GCC.'
      WordWrap = True
    end
    object ProgressPanel: TPanel
      Left = 0
      Top = 100
      Width = 281
      Height = 147
      BevelOuter = bvNone
      TabOrder = 1
      Visible = False
      object ParseLabel: TLabel
        Left = 4
        Top = 80
        Width = 260
        Height = 28
        AutoSize = False
        Caption = 'Parsing files...'
        WordWrap = True
      end
      object pbCCCache: TProgressBar
        Left = 4
        Top = 112
        Width = 269
        Height = 22
        Smooth = True
        TabOrder = 0
      end
    end
    object BuildPanel: TPanel
      Left = 0
      Top = 100
      Width = 280
      Height = 147
      BevelOuter = bvNone
      TabOrder = 0
      object YesCache: TRadioButton
        Left = 8
        Top = 4
        Width = 257
        Height = 17
        Caption = 'Yes, cache the default include folder'
        Checked = True
        TabOrder = 0
        TabStop = True
      end
      object NoCache: TRadioButton
        Left = 8
        Top = 128
        Width = 257
        Height = 17
        Caption = 'No, don'#39't cache anything'
        TabOrder = 1
      end
      object AltCache: TRadioButton
        Left = 8
        Top = 24
        Width = 257
        Height = 17
        Caption = 'Only cache these files and their includes:'
        TabOrder = 2
      end
      object AltFileList: TListBox
        Left = 4
        Top = 48
        Width = 197
        Height = 73
        Columns = 2
        ItemHeight = 13
        Items.Strings = (
          'stdio.h'
          'stdlib.h'
          'math.h'
          'string.h'
          'time.h'
          'windows.h'
          'windowsx.h'
          'd3d9.h'
          'd3dx9.h'
          'gdiplus.h')
        TabOrder = 3
      end
      object ButtonAddFile: TButton
        Left = 204
        Top = 48
        Width = 75
        Height = 23
        Caption = 'Add file'
        TabOrder = 4
        OnClick = ButtonAddFileClick
      end
      object ButtonRemove: TButton
        Left = 204
        Top = 96
        Width = 75
        Height = 23
        Caption = 'Remove'
        TabOrder = 5
        OnClick = ButtonRemoveClick
      end
      object ButtonAddFolder: TButton
        Left = 204
        Top = 72
        Width = 75
        Height = 23
        Caption = 'Add folder'
        TabOrder = 6
        OnClick = ButtonAddFolderClick
      end
    end
  end
  object SecondPanel: TPanel
    Left = 248
    Top = 8
    Width = 281
    Height = 249
    BevelOuter = bvNone
    TabOrder = 3
    Visible = False
    object SecondLabel: TLabel
      Left = 4
      Top = 8
      Width = 258
      Height = 39
      Caption = 
        'Dev-C++ can retrieve information from headers files, to help you' +
        ' find functions, classes and variables easily, through a class b' +
        'rowser and a code completion list.'
      WordWrap = True
    end
    object Label5: TLabel
      Left = 4
      Top = 72
      Width = 266
      Height = 39
      Caption = 
        'Although this feature is useful, it requires more CPU power and ' +
        'memory. Do you want to use it? You can enable or disable it late' +
        'r in Editor Options, Class Browser.'
      WordWrap = True
    end
    object YesClassBrowser: TRadioButton
      Left = 36
      Top = 168
      Width = 209
      Height = 17
      Caption = 'Yes, I want to use this feature'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object NoClassBrowser: TRadioButton
      Left = 36
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
end
