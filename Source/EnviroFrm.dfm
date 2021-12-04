object EnviroForm: TEnviroForm
  Left = 835
  Top = 465
  BorderStyle = bsDialog
  Caption = 'Environment Options'
  ClientHeight = 462
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
    462)
  PixelsPerInch = 96
  TextHeight = 15
  object btnHelp: TBitBtn
    Left = 391
    Top = 431
    Width = 85
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    Enabled = False
    TabOrder = 2
    OnClick = btnHelpClick
  end
  object btnOk: TBitBtn
    Left = 211
    Top = 431
    Width = 85
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    DoubleBuffered = False
    ModalResult = 1
    NumGlyphs = 2
    TabOrder = 1
    OnClick = btnOkClick
  end
  object btnCancel: TBitBtn
    Left = 301
    Top = 431
    Width = 85
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    Images = dmMain.SVGImageListMenuStyle
    ModalResult = 2
    NumGlyphs = 2
    TabOrder = 3
    OnClick = btnCancelClick
  end
  object PagesMain: TPageControl
    Left = 0
    Top = 0
    Width = 484
    Height = 425
    ActivePage = tabGeneral
    HotTrack = True
    TabOrder = 0
    object tabGeneral: TTabSheet
      Caption = 'General'
      ParentShowHint = False
      ShowHint = False
      DesignSize = (
        476
        395)
      object lblMRU: TLabel
        Left = 332
        Top = 13
        Width = 127
        Height = 15
        Caption = 'Max Files in History List:'
      end
      object lblMsgTabs: TLabel
        Left = 302
        Top = 62
        Width = 160
        Height = 15
        AutoSize = False
        Caption = 'Editor Tab Location:'
      end
      object lblLang: TLabel
        Left = 302
        Top = 114
        Width = 52
        Height = 15
        Caption = 'Language'
      end
      object lblTheme: TLabel
        Left = 302
        Top = 165
        Width = 36
        Height = 15
        Caption = 'Theme'
        Visible = False
      end
      object cbBackups: TCheckBox
        Left = 16
        Top = 37
        Width = 265
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Create File Backups'
        Ctl3D = False
        ParentCtl3D = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
      end
      object cbMinOnRun: TCheckBox
        Left = 16
        Top = 58
        Width = 265
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Minimize on Run'
        Ctl3D = False
        ParentCtl3D = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
      end
      object cbDefCpp: TCheckBox
        Left = 16
        Top = 16
        Width = 265
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Default to C++ on New Project'
        Ctl3D = False
        ParentCtl3D = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
      end
      object cbShowBars: TCheckBox
        Left = 16
        Top = 80
        Width = 265
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Show Toolbars in Full Screen'
        Ctl3D = False
        ParentCtl3D = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
      end
      object cbMultiLineTab: TCheckBox
        Left = 16
        Top = 101
        Width = 265
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Enable Editor Multiline Tabs'
        Ctl3D = False
        ParentCtl3D = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
      end
      object rgbAutoOpen: TRadioGroup
        Left = 246
        Top = 272
        Width = 215
        Height = 109
        Caption = 'Auto Open'
        Ctl3D = False
        Items.Strings = (
          'All project files'
          'Only first project file'
          'Opened files at previous closing'
          'None')
        ParentCtl3D = False
        TabOrder = 13
      end
      object gbDebugger: TGroupBox
        Left = 15
        Top = 240
        Width = 215
        Height = 53
        Caption = 'Debug Variable Browser'
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 7
        object cbWatchHint: TCheckBox
          Left = 14
          Top = 22
          Width = 195
          Height = 17
          Caption = 'Watch variable under mouse'
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 0
        end
      end
      object gbProgress: TGroupBox
        Left = 15
        Top = 311
        Width = 215
        Height = 70
        Caption = 'Compilation Progress Window '
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 8
        object cbShowProgress: TCheckBox
          Left = 14
          Top = 22
          Width = 195
          Height = 17
          Caption = '&Show during compilation'
          TabOrder = 0
        end
        object cbAutoCloseProgress: TCheckBox
          Left = 14
          Top = 43
          Width = 195
          Height = 17
          Caption = '&Auto close after compile'
          TabOrder = 1
        end
      end
      object seMRUMax: TSpinEdit
        Left = 408
        Top = 34
        Width = 51
        Height = 24
        MaxLength = 2
        MaxValue = 0
        MinValue = 0
        TabOrder = 9
        Value = 0
      end
      object cboTabsTop: TComboBox
        Left = 302
        Top = 84
        Width = 160
        Height = 23
        Style = csDropDownList
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 10
        Items.Strings = (
          'Top'
          'Bottom'
          'Left'
          'Right')
      end
      object cboLang: TComboBox
        Left = 302
        Top = 136
        Width = 160
        Height = 23
        Style = csDropDownList
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 11
      end
      object cboTheme: TComboBox
        Left = 302
        Top = 183
        Width = 160
        Height = 23
        Style = csDropDownList
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 12
        Visible = False
      end
      object cbPauseConsole: TCheckBox
        Left = 16
        Top = 122
        Width = 265
        Height = 17
        Caption = 'Pause console programs after return'
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 5
      end
      object cbCheckAssocs: TCheckBox
        Left = 16
        Top = 143
        Width = 265
        Height = 17
        Caption = 'Check file associations on startup'
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 6
      end
    end
    object tabPaths: TTabSheet
      Caption = 'Directories'
      ParentShowHint = False
      ShowHint = False
      object lblUserDir: TLabel
        Left = 8
        Top = 80
        Width = 400
        Height = 15
        AutoSize = False
        Caption = 'User'#39's Default Directory'
      end
      object lblTemplatesDir: TLabel
        Left = 8
        Top = 144
        Width = 400
        Height = 15
        AutoSize = False
        Caption = 'Templates Directory'
      end
      object lblSplash: TLabel
        Left = 8
        Top = 337
        Width = 400
        Height = 15
        AutoSize = False
        Caption = 'Splash Screen Image'
      end
      object lblIcoLib: TLabel
        Left = 8
        Top = 208
        Width = 400
        Height = 15
        AutoSize = False
        Caption = 'Icon Library Path'
      end
      object lblLangPath: TLabel
        Left = 8
        Top = 273
        Width = 400
        Height = 15
        AutoSize = False
        Caption = 'Language Path'
      end
      object btnDefBrws: TSpeedButton
        Tag = 1
        Left = 438
        Top = 101
        Width = 23
        Height = 22
        ImageIndex = 59
        ImageName = 'iconsnew-65'
        Images = dmMain.SVGImageListMenuStyle
        Flat = True
        OnClick = BrowseClick
      end
      object btnOutputbrws: TSpeedButton
        Tag = 2
        Left = 437
        Top = 165
        Width = 23
        Height = 22
        ImageIndex = 59
        ImageName = 'iconsnew-65'
        Images = dmMain.SVGImageListMenuStyle
        Flat = True
        OnClick = BrowseClick
      end
      object btnBrwIcon: TSpeedButton
        Tag = 3
        Left = 437
        Top = 229
        Width = 23
        Height = 22
        ImageIndex = 59
        ImageName = 'iconsnew-65'
        Images = dmMain.SVGImageListMenuStyle
        Flat = True
        OnClick = BrowseClick
      end
      object btnBrwLang: TSpeedButton
        Tag = 5
        Left = 437
        Top = 294
        Width = 23
        Height = 22
        ImageIndex = 59
        ImageName = 'iconsnew-65'
        Images = dmMain.SVGImageListMenuStyle
        Flat = True
        OnClick = BrowseClick
      end
      object btnBrwSplash: TSpeedButton
        Tag = 4
        Left = 437
        Top = 358
        Width = 23
        Height = 22
        ImageIndex = 59
        ImageName = 'iconsnew-65'
        Images = dmMain.SVGImageListMenuStyle
        Flat = True
        OnClick = BrowseClick
      end
      object lblOptionsDir: TLabel
        Left = 8
        Top = 10
        Width = 457
        Height = 31
        AutoSize = False
        Caption = 
          'Current Options directory. Click the button to reset Embarcadero' +
          ' Dev-C++.'
      end
      object edUserDir: TEdit
        Left = 16
        Top = 101
        Width = 409
        Height = 21
        Ctl3D = False
        ParentCtl3D = False
        ReadOnly = True
        TabOrder = 0
        Text = 'edUserDir'
      end
      object edTemplatesDir: TEdit
        Left = 16
        Top = 165
        Width = 409
        Height = 21
        Ctl3D = False
        ParentCtl3D = False
        ReadOnly = True
        TabOrder = 1
        Text = 'edTemplatesDir'
      end
      object edSplash: TEdit
        Left = 16
        Top = 358
        Width = 409
        Height = 21
        Ctl3D = False
        ParentCtl3D = False
        ReadOnly = True
        TabOrder = 2
        Text = 'edSplash'
      end
      object edIcoLib: TEdit
        Left = 16
        Top = 229
        Width = 409
        Height = 21
        Ctl3D = False
        ParentCtl3D = False
        ReadOnly = True
        TabOrder = 3
        Text = 'edIcoLib'
      end
      object edLang: TEdit
        Left = 16
        Top = 294
        Width = 409
        Height = 21
        Ctl3D = False
        ParentCtl3D = False
        ReadOnly = True
        TabOrder = 4
        Text = 'edLang'
      end
      object edOptionsDir: TEdit
        Left = 16
        Top = 34
        Width = 281
        Height = 21
        Ctl3D = False
        ParentCtl3D = False
        ReadOnly = True
        TabOrder = 5
        Text = 'edOptionsDir'
      end
      object btnResetDev: TButton
        Left = 304
        Top = 32
        Width = 155
        Height = 25
        Caption = 'Remove settings and exit'
        TabOrder = 6
        OnClick = btnResetDevClick
      end
    end
    object tabExternal: TTabSheet
      Caption = 'External Programs'
      DesignSize = (
        476
        395)
      object lblExternal: TLabel
        Left = 8
        Top = 8
        Width = 166
        Height = 15
        Caption = 'External programs associations:'
      end
      object btnExtAdd: TSpeedButton
        Left = 128
        Top = 364
        Width = 99
        Height = 25
        Anchors = [akBottom]
        Caption = 'Add'
        Flat = True
        OnClick = btnExtAddClick
      end
      object btnExtDel: TSpeedButton
        Left = 261
        Top = 364
        Width = 99
        Height = 25
        Anchors = [akBottom]
        Caption = 'Delete'
        Flat = True
        OnClick = btnExtDelClick
      end
      object vleExternal: TValueListEditor
        Left = 28
        Top = 30
        Width = 417
        Height = 330
        Anchors = [akLeft, akTop, akRight, akBottom]
        Ctl3D = False
        KeyOptions = [keyEdit, keyAdd, keyDelete]
        Options = [goVertLine, goHorzLine, goEditing, goAlwaysShowEditor, goThumbTracking]
        ParentCtl3D = False
        TabOrder = 0
        TitleCaptions.Strings = (
          'Extension'
          'External program')
        OnEditButtonClick = vleExternalEditButtonClick
        OnValidate = vleExternalValidate
        ColWidths = (
          84
          329)
      end
    end
    object tabAssocs: TTabSheet
      Caption = 'File Associations'
      ParentShowHint = False
      ShowHint = False
      DesignSize = (
        476
        395)
      object lblAssocFileTypes: TLabel
        Left = 8
        Top = 8
        Width = 53
        Height = 15
        Caption = 'File Types:'
      end
      object lblAssocDesc: TLabel
        Left = 0
        Top = 350
        Width = 476
        Height = 43
        Alignment = taCenter
        AutoSize = False
        Caption = 
          'Just check or un-check for which file types Embarcadero Dev-C++ ' +
          'will be registered as the default application to open them...'
        WordWrap = True
      end
      object lstAssocFileTypes: TCheckListBox
        Left = 28
        Top = 30
        Width = 417
        Height = 317
        Anchors = [akLeft, akTop, akRight, akBottom]
        Ctl3D = False
        ItemHeight = 15
        ParentCtl3D = False
        TabOrder = 0
      end
    end
    object TabAppearance: TTabSheet
      Caption = 'Appearance'
      ImageIndex = 4
      object LblStyle: TLabel
        Left = 5
        Top = 14
        Width = 159
        Height = 17
        AutoSize = False
        Caption = 'Themes'
      end
      object UIfontlabel: TLabel
        Left = 138
        Top = 64
        Width = 24
        Height = 15
        Caption = 'Font'
      end
      object lblSize: TLabel
        Left = 319
        Top = 64
        Width = 20
        Height = 15
        Caption = 'Size'
      end
      object cbUIfont: TComboBox
        Left = 138
        Top = 87
        Width = 172
        Height = 26
        AutoComplete = False
        Style = csOwnerDrawVariable
        DropDownCount = 10
        ItemHeight = 20
        Sorted = True
        TabOrder = 0
        OnDrawItem = cbUIfontDrawItem
      end
      object cbUIfontsize: TComboBox
        Left = 319
        Top = 87
        Width = 144
        Height = 26
        AutoComplete = False
        Style = csOwnerDrawVariable
        DropDownCount = 10
        ItemHeight = 20
        TabOrder = 1
        OnChange = cbUIfontsizeChange
        OnDrawItem = cbUIfontsizeDrawItem
        Items.Strings = (
          '7'
          '8'
          '9'
          '10'
          '11'
          '12'
          '13'
          '14'
          '15'
          '16')
      end
      object ListBoxStyle: TListBox
        Left = 5
        Top = 36
        Width = 119
        Height = 77
        Ctl3D = False
        ItemHeight = 15
        Items.Strings = (
          'Windows Classic'
          'Windows 10'
          'Slate Gray'
          'Blue Whale'
          'Black Pearl'
          'Glossy'
          'Calypso'
          'Flat UI Light'
          'Material Patterns Blue')
        ParentCtl3D = False
        TabOrder = 2
        OnClick = ListBoxStyleClick
      end
      object Panel1: TPanel
        Left = 0
        Top = 143
        Width = 476
        Height = 249
        DoubleBuffered = True
        ParentBackground = False
        ParentDoubleBuffered = False
        TabOrder = 3
        object Button1: TButton
          Left = 158
          Top = 16
          Width = 75
          Height = 25
          Caption = 'Button1'
          TabOrder = 0
        end
        object ComboBox1: TComboBox
          Left = 158
          Top = 95
          Width = 145
          Height = 23
          TabOrder = 1
          Text = 'ComboBox1'
        end
        object CheckBox1: TCheckBox
          Left = 158
          Top = 58
          Width = 97
          Height = 17
          Caption = 'CheckBox1'
          TabOrder = 2
        end
        object PageControl1: TPageControl
          Left = 1
          Top = 1
          Width = 136
          Height = 247
          ActivePage = TabSheet1
          Align = alLeft
          TabOrder = 3
          object TabSheet1: TTabSheet
            Caption = 'TabSheet1'
          end
          object TabSheet2: TTabSheet
            Caption = 'TabSheet2'
            ImageIndex = 1
          end
        end
        object Button2: TButton
          Left = 239
          Top = 16
          Width = 75
          Height = 25
          Caption = 'Button2'
          Enabled = False
          TabOrder = 4
        end
        object Button3: TButton
          Left = 320
          Top = 16
          Width = 75
          Height = 25
          Caption = 'Button3'
          TabOrder = 5
        end
        object RadioGroup1: TRadioGroup
          Left = 154
          Top = 136
          Width = 185
          Height = 97
          Caption = 'RadioGroup1'
          Items.Strings = (
            'Item 1'
            'Item 2')
          TabOrder = 6
        end
      end
    end
  end
  object dlgPic: TOpenPictureDialog
    Left = 14
    Top = 426
  end
end
