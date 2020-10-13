object ProjectOptionsFrm: TProjectOptionsFrm
  Left = 820
  Top = 561
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Project Options'
  ClientHeight = 382
  ClientWidth = 534
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
  PixelsPerInch = 96
  TextHeight = 15
  object btnOk: TBitBtn
    Left = 260
    Top = 350
    Width = 85
    Height = 25
    Caption = '&OK'
    Images = dmMain.SVGImageListMenuStyle
    ModalResult = 1
    NumGlyphs = 2
    TabOrder = 0
    OnClick = btnOkClick
  end
  object btnCancel: TBitBtn
    Left = 350
    Top = 350
    Width = 85
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    NumGlyphs = 2
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object btnHelp: TBitBtn
    Left = 440
    Top = 350
    Width = 85
    Height = 25
    Caption = '&Help'
    NumGlyphs = 2
    TabOrder = 2
    OnClick = btnHelpClick
  end
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 534
    Height = 342
    ActivePage = tabGeneral
    TabOrder = 3
    object tabGeneral: TTabSheet
      Caption = 'General'
      object lblPrjName: TLabel
        Left = 8
        Top = 13
        Width = 35
        Height = 15
        Caption = 'Name:'
      end
      object lblFname: TLabel
        Left = 8
        Top = 48
        Width = 51
        Height = 15
        Caption = 'Filename:'
      end
      object lblPrjFname: TLabel
        Left = 88
        Top = 48
        Width = 425
        Height = 33
        AutoSize = False
        Caption = 'lblPrjFname'
      end
      object lblUnits: TLabel
        Left = 8
        Top = 120
        Width = 30
        Height = 15
        Caption = 'Units:'
      end
      object lblPrjUnits: TLabel
        Left = 88
        Top = 120
        Width = 425
        Height = 33
        AutoSize = False
        Caption = 'lblPrjUnits'
      end
      object lblPrjOutputFname: TLabel
        Left = 88
        Top = 84
        Width = 425
        Height = 33
        AutoSize = False
        Caption = 'lblPrjOutputFname'
      end
      object lblPrjOutput: TLabel
        Left = 8
        Top = 84
        Width = 41
        Height = 15
        Caption = 'Output:'
      end
      object grpIcon: TGroupBox
        Left = 12
        Top = 166
        Width = 245
        Height = 138
        Caption = '  Icon:  '
        TabOrder = 0
        object btnIconBrowse: TBitBtn
          Left = 128
          Top = 60
          Width = 105
          Height = 30
          Hint = 'Select a custom icon'
          Caption = 'Browse...'
          ImageIndex = 59
          ImageName = 'iconsnew-65'
          Images = dmMain.SVGImageListMenuStyle
          TabOrder = 0
          WordWrap = True
          StyleElements = [seFont, seClient]
          OnClick = btnIconBrowseClick
        end
        object btnIconLib: TBitBtn
          Left = 128
          Top = 21
          Width = 105
          Height = 30
          Hint = 'Select a icon from Dev-C++'#39's icon collection'
          Caption = 'Library'
          ImageIndex = 58
          ImageName = 'iconsnew-64'
          Images = dmMain.SVGImageListMenuStyle
          TabOrder = 1
          OnClick = btnIconLibClick
        end
        object btnRemoveIcon: TBitBtn
          Left = 128
          Top = 97
          Width = 105
          Height = 30
          Hint = 'Do not use an icon for this project'
          Caption = 'Remove'
          Enabled = False
          ImageIndex = 5
          ImageName = 'iconsnew-31'
          Images = dmMain.SVGImageListMenuStyle
          TabOrder = 2
          OnClick = btnRemoveIconClick
        end
        object Panel1: TPanel
          Left = 38
          Top = 50
          Width = 44
          Height = 44
          BevelOuter = bvLowered
          TabOrder = 3
          object IconPreview: TImage
            Left = 6
            Top = 6
            Width = 32
            Height = 32
            Center = True
            Stretch = True
          end
        end
      end
      object grpType: TGroupBox
        Left = 272
        Top = 166
        Width = 245
        Height = 138
        Caption = '  Type:  '
        TabOrder = 1
        object lstType: TListBox
          Left = 8
          Top = 20
          Width = 225
          Height = 62
          IntegralHeight = True
          ItemHeight = 15
          Items.Strings = (
            'Win32 GUI'
            'Win32 Console'
            'Win32 Static Lib'
            'Win32 DLL')
          TabOrder = 0
          OnClick = lstTypeClick
        end
        object chkSupportXP: TCheckBox
          Left = 7
          Top = 115
          Width = 230
          Height = 17
          Caption = 'Support Windows XP Themes'
          TabOrder = 1
        end
        object chkDefCpp: TCheckBox
          Left = 7
          Top = 92
          Width = 230
          Height = 17
          Caption = 'Default to C++ when creating new files'
          TabOrder = 2
        end
      end
      object edProjectName: TEdit
        Left = 87
        Top = 10
        Width = 426
        Height = 21
        Hint = 
          'Note: The exe filename is taken from the project filename, not t' +
          'his name'
        TabOrder = 2
      end
    end
    object tabFiles: TTabSheet
      Caption = 'Files'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lvFiles: TTreeView
        Left = 8
        Top = 8
        Width = 209
        Height = 297
        HideSelection = False
        Indent = 19
        MultiSelect = True
        MultiSelectStyle = [msControlSelect, msShiftSelect, msSiblingOnly]
        ReadOnly = True
        TabOrder = 0
        OnChange = lvFilesChange
      end
      object grpUnitOptions: TGroupBox
        Left = 224
        Top = 8
        Width = 297
        Height = 297
        Caption = 'Unit Options'
        TabOrder = 1
        object lblPriority: TLabel
          Left = 8
          Top = 20
          Width = 153
          Height = 20
          AutoSize = False
          Caption = 'Build priority:'
        end
        object chkCompile: TCheckBox
          Left = 8
          Top = 44
          Width = 217
          Height = 17
          Caption = 'Include in compilation process'
          Enabled = False
          TabOrder = 0
          OnClick = chkCompileClick
        end
        object chkCompileCpp: TCheckBox
          Left = 8
          Top = 80
          Width = 217
          Height = 17
          Caption = 'Compile unit as C++'
          Enabled = False
          TabOrder = 1
          OnClick = chkCompileClick
        end
        object chkOverrideBuildCmd: TCheckBox
          Left = 8
          Top = 100
          Width = 217
          Height = 17
          Caption = 'Override build command'
          Enabled = False
          TabOrder = 2
          OnClick = chkCompileClick
        end
        object txtOverrideBuildCmd: TMemo
          Left = 8
          Top = 122
          Width = 281
          Height = 167
          Enabled = False
          ScrollBars = ssBoth
          TabOrder = 3
          WordWrap = False
          OnChange = txtOverrideBuildCmdChange
        end
        object chkLink: TCheckBox
          Left = 8
          Top = 62
          Width = 217
          Height = 17
          Caption = 'Include in linking process'
          Enabled = False
          TabOrder = 4
          OnClick = chkCompileClick
        end
        object spnPriority: TSpinEdit
          Left = 168
          Top = 16
          Width = 85
          Height = 24
          Enabled = False
          MaxValue = 0
          MinValue = 0
          TabOrder = 5
          Value = 0
          OnChange = spnPriorityChange
        end
      end
    end
    object tabCompiler: TTabSheet
      Caption = 'Compiler'
      object lblCompilerSet: TLabel
        Left = 8
        Top = 4
        Width = 95
        Height = 15
        Caption = 'Base compiler set:'
      end
      object OptionsTip: TLabel
        Left = 0
        Top = 268
        Width = 521
        Height = 17
        Alignment = taCenter
        AutoSize = False
        Caption = 'For more information about GCC'#39's options, please visit'
      end
      object OptionsLink: TLabel
        Left = 0
        Top = 288
        Width = 521
        Height = 21
        Cursor = crHandPoint
        Alignment = taCenter
        AutoSize = False
        Caption = 'http://gcc.gnu.org/onlinedocs/gcc/Option-Summary.html'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsUnderline]
        ParentColor = False
        ParentFont = False
        OnClick = OptionsLinkClick
      end
      object lblCompilerHint: TLabel
        Left = 8
        Top = 56
        Width = 209
        Height = 15
        Caption = 'Customize (applies to this project only):'
      end
      inline CompOptionsFrame1: TCompOptionsFrame
        Left = 0
        Top = 78
        Width = 526
        Height = 187
        HorzScrollBar.Visible = False
        VertScrollBar.Visible = False
        TabOrder = 0
        ExplicitTop = 78
        ExplicitWidth = 526
        ExplicitHeight = 187
        inherited tabs: TTabControl
          Width = 526
          Height = 187
          ExplicitWidth = 526
          ExplicitHeight = 187
          inherited vle: TCompOptionsList
            Width = 518
            Height = 177
            DefaultColWidth = 215
            ParentShowHint = False
            ExplicitWidth = 518
            ExplicitHeight = 177
            ColWidths = (
              215
              215)
          end
        end
      end
      object cmbCompiler: TComboBox
        Left = 16
        Top = 24
        Width = 495
        Height = 23
        Style = csDropDownList
        TabOrder = 1
        OnChange = cmbCompilerChange
      end
    end
    object tabCompOpts: TTabSheet
      Caption = 'Options'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lblAdditions: TLabel
        Left = 6
        Top = 6
        Width = 200
        Height = 15
        Caption = '  Additional Command Line Options:  '
      end
      object lblCompiler: TLabel
        Left = 10
        Top = 24
        Width = 63
        Height = 15
        Caption = 'C Compiler:'
      end
      object lblCppCompiler: TLabel
        Left = 190
        Top = 24
        Width = 79
        Height = 15
        Caption = 'C++ Compiler:'
      end
      object lblLinker: TLabel
        Left = 368
        Top = 24
        Width = 32
        Height = 15
        Caption = 'Linker'
        WordWrap = True
      end
      object edCompiler: TMemo
        Left = 10
        Top = 40
        Width = 175
        Height = 265
        Lines.Strings = (
          'edCompiler')
        ScrollBars = ssBoth
        TabOrder = 0
      end
      object edCppCompiler: TMemo
        Left = 190
        Top = 40
        Width = 175
        Height = 265
        Lines.Strings = (
          'edCppCompiler')
        ScrollBars = ssBoth
        TabOrder = 1
      end
      object edLinker: TMemo
        Left = 368
        Top = 40
        Width = 148
        Height = 225
        Lines.Strings = (
          'edLinker')
        ScrollBars = ssBoth
        TabOrder = 2
      end
      object btnAddLib: TBitBtn
        Left = 368
        Top = 272
        Width = 148
        Height = 30
        Caption = 'Add Library or Object'
        ImageIndex = 59
        ImageName = 'iconsnew-65'
        Images = dmMain.SVGImageListMenuStyle
        TabOrder = 3
        OnClick = btnAddLibClick
      end
    end
    object tabFilesDir: TTabSheet
      Caption = 'Directories'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object SubTabs: TTabControl
        Left = 0
        Top = 0
        Width = 526
        Height = 312
        Align = alClient
        TabOrder = 0
        Tabs.Strings = (
          'Libraries'
          'Include Directories'
          'Resource Directories')
        TabIndex = 0
        OnChange = SubTabsChange
        object btnDirDown: TSpeedButton
          Left = 495
          Top = 160
          Width = 23
          Height = 22
          ImageIndex = 57
          ImageName = 'iconsnew-53'
          Images = dmMain.SVGImageListMenuStyle
          Enabled = False
          Flat = True
          OnClick = UpDownClick
        end
        object btnDirUp: TSpeedButton
          Left = 495
          Top = 130
          Width = 23
          Height = 22
          ImageIndex = 56
          ImageName = 'iconsnew-52'
          Images = dmMain.SVGImageListMenuStyle
          Enabled = False
          Flat = True
          OnClick = UpDownClick
        end
        object btnBrowse: TSpeedButton
          Left = 495
          Top = 250
          Width = 23
          Height = 22
          ImageIndex = 59
          ImageName = 'iconsnew-65'
          Images = dmMain.SVGImageListMenuStyle
          Flat = True
          OnClick = BrowseClick
        end
        object lstDirList: TListBox
          Left = 4
          Top = 26
          Width = 485
          Height = 220
          ItemHeight = 15
          TabOrder = 0
          OnClick = ListClick
        end
        object btnDirDelInval: TButton
          Tag = 4
          Left = 384
          Top = 281
          Width = 100
          Height = 23
          Caption = 'Delete Invalid'
          TabOrder = 1
          OnClick = ButtonClick
        end
        object edDirEntry: TEdit
          Left = 4
          Top = 250
          Width = 485
          Height = 21
          TabOrder = 2
          OnChange = EditChange
        end
        object btnDirReplace: TButton
          Tag = 1
          Left = 54
          Top = 281
          Width = 100
          Height = 23
          Caption = '&Replace'
          Enabled = False
          TabOrder = 3
          OnClick = ButtonClick
        end
        object btnDirDelete: TButton
          Tag = 3
          Left = 274
          Top = 281
          Width = 100
          Height = 23
          Caption = '&Delete'
          Enabled = False
          TabOrder = 4
          OnClick = ButtonClick
        end
        object btnDirAdd: TButton
          Tag = 2
          Left = 164
          Top = 281
          Width = 100
          Height = 23
          Caption = '&Add'
          Enabled = False
          TabOrder = 5
          OnClick = ButtonClick
        end
      end
    end
    object tabOutputDir: TTabSheet
      Caption = 'Output'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lblOverrideOutput: TLabel
        Left = 10
        Top = 234
        Width = 136
        Height = 15
        Caption = 'Override output filename:'
      end
      object btnExeOutDir: TSpeedButton
        Left = 492
        Top = 54
        Width = 23
        Height = 22
        ImageIndex = 59
        ImageName = 'iconsnew-65'
        Images = dmMain.SVGImageListMenuStyle
        Flat = True
        OnClick = BrowseExecutableOutDirClick
      end
      object btnLogOutputDir: TSpeedButton
        Left = 492
        Top = 194
        Width = 23
        Height = 22
        ImageIndex = 59
        ImageName = 'iconsnew-65'
        Images = dmMain.SVGImageListMenuStyle
        Flat = True
        OnClick = btnLogOutputDirClick
      end
      object btnObjOutDir: TSpeedButton
        Left = 492
        Top = 124
        Width = 23
        Height = 22
        ImageIndex = 59
        ImageName = 'iconsnew-65'
        Images = dmMain.SVGImageListMenuStyle
        Flat = True
        OnClick = BrowseLogDirClick
      end
      object lblExeOutput: TLabel
        Left = 10
        Top = 24
        Width = 149
        Height = 15
        Caption = '&Executable output directory:'
        FocusControl = edExeOutput
      end
      object lblLogOutput: TLabel
        Left = 10
        Top = 164
        Width = 122
        Height = 15
        Caption = '&Compiler log autosave:'
        FocusControl = edObjOutput
      end
      object lblObjOutput: TLabel
        Left = 10
        Top = 94
        Width = 146
        Height = 15
        Caption = '&Object file output directory:'
        FocusControl = edObjOutput
      end
      object edOverridenOutput: TEdit
        Left = 34
        Top = 264
        Width = 470
        Height = 21
        TabOrder = 0
      end
      object chkLogOutput: TCheckBox
        Left = 10
        Top = 196
        Width = 17
        Height = 17
        TabOrder = 1
        OnClick = chkLogOutputClick
      end
      object edExeOutput: TEdit
        Left = 34
        Top = 54
        Width = 440
        Height = 21
        TabOrder = 2
      end
      object edLogOutput: TEdit
        Left = 34
        Top = 194
        Width = 440
        Height = 21
        TabOrder = 3
      end
      object edObjOutput: TEdit
        Left = 34
        Top = 124
        Width = 440
        Height = 21
        TabOrder = 4
      end
      object chkOverrideOutput: TCheckBox
        Left = 10
        Top = 266
        Width = 17
        Height = 17
        TabOrder = 5
        OnClick = chkLogOutputClick
      end
    end
    object tabMakefile: TTabSheet
      Caption = 'Makefile'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object btnCustomMakeBrowse: TSpeedButton
        Left = 490
        Top = 54
        Width = 23
        Height = 22
        ImageIndex = 59
        ImageName = 'iconsnew-65'
        Images = dmMain.SVGImageListMenuStyle
        Flat = True
        OnClick = btnCustomMakeBrowseClick
      end
      object btnMakDown: TSpeedButton
        Left = 495
        Top = 200
        Width = 23
        Height = 22
        ImageIndex = 57
        ImageName = 'iconsnew-53'
        Images = dmMain.SVGImageListMenuStyle
        Enabled = False
        Flat = True
        OnClick = btnMakClick
      end
      object btnMakeBrowse: TSpeedButton
        Left = 495
        Top = 256
        Width = 23
        Height = 22
        ImageIndex = 59
        ImageName = 'iconsnew-65'
        Images = dmMain.SVGImageListMenuStyle
        Flat = True
        OnClick = btnMakeBrowseClick
      end
      object btnMakUp: TSpeedButton
        Left = 495
        Top = 170
        Width = 23
        Height = 22
        ImageIndex = 56
        ImageName = 'iconsnew-52'
        Images = dmMain.SVGImageListMenuStyle
        Enabled = False
        Flat = True
        OnClick = btnMakClick
      end
      object IncMakeLabel: TLabel
        Left = 12
        Top = 119
        Width = 231
        Height = 15
        Caption = '&Include the following files into the Makefile:'
      end
      object InfoMakeBtn: TSpeedButton
        Left = 13
        Top = 88
        Width = 500
        Height = 25
        Caption = 'Information about customizing the Makefile...'
        ImageIndex = 46
        ImageName = 'iconsnew-29'
        Images = dmMain.SVGImageListMenuStyle
        Flat = True
        NumGlyphs = 2
        OnClick = InfoMakeBtnClick
      end
      object btnMakAdd: TButton
        Tag = 2
        Left = 164
        Top = 287
        Width = 100
        Height = 23
        Caption = '&Add'
        Enabled = False
        TabOrder = 0
        OnClick = MakButtonClick
      end
      object btnMakDelete: TButton
        Tag = 3
        Left = 274
        Top = 287
        Width = 100
        Height = 23
        Caption = '&Delete'
        Enabled = False
        TabOrder = 1
        OnClick = MakButtonClick
      end
      object btnMakDelInval: TButton
        Tag = 4
        Left = 384
        Top = 287
        Width = 100
        Height = 23
        Caption = 'Delete Invalid'
        Enabled = False
        TabOrder = 2
        OnClick = MakButtonClick
      end
      object btnMakReplace: TButton
        Tag = 1
        Left = 54
        Top = 287
        Width = 100
        Height = 23
        Caption = '&Replace'
        Enabled = False
        TabOrder = 3
        OnClick = MakButtonClick
      end
      object cbUseCustomMakefile: TCheckBox
        Left = 16
        Top = 24
        Width = 497
        Height = 17
        Caption = 'Use custom Makefile (do not generate a Makefile, use this one)'
        TabOrder = 4
        OnClick = cbUseCustomMakefileClick
      end
      object edCustomMakefile: TEdit
        Left = 32
        Top = 54
        Width = 440
        Height = 21
        ReadOnly = True
        TabOrder = 5
        Text = 'edCustomMakefile'
      end
      object edMakeInclude: TEdit
        Left = 4
        Top = 257
        Width = 485
        Height = 21
        TabOrder = 6
        OnChange = edMakeIncludeChange
      end
      object lbMakeIncludes: TListBox
        Left = 4
        Top = 140
        Width = 485
        Height = 113
        ItemHeight = 15
        TabOrder = 7
        OnClick = lbMakeIncludesClick
        OnDrawItem = lbMakeIncludesDrawItem
      end
    end
    object tabVersion: TTabSheet
      Caption = 'Version Info'
      object chkVersionInfo: TCheckBox
        Left = 12
        Top = 8
        Width = 509
        Height = 17
        Caption = 'Include version info in project'
        TabOrder = 0
        OnClick = chkVersionInfoClick
      end
      object grpVersion: TGroupBox
        Left = 3
        Top = 31
        Width = 505
        Height = 278
        Caption = 'Version details'
        TabOrder = 1
        object lblVerMajor: TLabel
          Left = 12
          Top = 20
          Width = 34
          Height = 15
          Caption = 'Major:'
        end
        object lblVerMinor: TLabel
          Left = 72
          Top = 20
          Width = 35
          Height = 15
          Caption = 'Minor:'
        end
        object lblVerRel: TLabel
          Left = 132
          Top = 20
          Width = 42
          Height = 15
          Caption = 'Release:'
        end
        object lblVerBuild: TLabel
          Left = 192
          Top = 20
          Width = 30
          Height = 15
          Caption = 'Build:'
        end
        object lblVerLang: TLabel
          Left = 280
          Top = 20
          Width = 55
          Height = 15
          Caption = 'Language:'
        end
        object spnMajor: TSpinEdit
          Left = 12
          Top = 38
          Width = 57
          Height = 24
          MaxValue = 0
          MinValue = 0
          TabOrder = 0
          Value = 0
          OnChange = SetFileVersion
        end
        object spnMinor: TSpinEdit
          Left = 72
          Top = 38
          Width = 57
          Height = 24
          MaxValue = 0
          MinValue = 0
          TabOrder = 1
          Value = 0
          OnChange = SetFileVersion
        end
        object spnRelease: TSpinEdit
          Left = 132
          Top = 38
          Width = 57
          Height = 24
          MaxValue = 0
          MinValue = 0
          TabOrder = 2
          Value = 0
          OnChange = SetFileVersion
        end
        object spnBuild: TSpinEdit
          Left = 192
          Top = 38
          Width = 57
          Height = 24
          MaxValue = 0
          MinValue = 0
          TabOrder = 3
          Value = 0
          OnChange = SetFileVersion
        end
        object vleVersion: TValueListEditor
          Left = 12
          Top = 92
          Width = 477
          Height = 175
          DisplayOptions = [doAutoColResize, doKeyColFixed]
          FixedCols = 1
          Options = [goVertLine, goHorzLine, goColSizing, goEditing, goAlwaysShowEditor, goThumbTracking]
          TabOrder = 7
          ColWidths = (
            150
            323)
        end
        object cmbLangID: TComboBox
          Left = 280
          Top = 38
          Width = 209
          Height = 23
          Style = csDropDownList
          Sorted = True
          TabOrder = 4
        end
        object chkAutoIncBuild: TCheckBox
          Left = 12
          Top = 67
          Width = 269
          Height = 17
          Caption = 'Auto-increment build number on compile'
          TabOrder = 5
        end
        object chkSyncProduct: TCheckBox
          Left = 300
          Top = 67
          Width = 181
          Height = 17
          Caption = 'Sync product with file version'
          TabOrder = 6
          OnClick = SetFileVersion
        end
      end
    end
  end
  object dlgOpen: TOpenDialog
    Filter = 
      'Object files (*.o;*.obj)|*.o;*.obj|Lib files (*.a;*.lib)|*.a;*.l' +
      'ib|Resource file (.rc)|*.rc|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofAllowMultiSelect]
    Title = 'Open object file'
    Left = 38
    Top = 342
  end
  object dlgPic: TOpenPictureDialog
    DefaultExt = 'ico'
    Filter = 'Icons (*.ico)|*.ico'
    Title = 'Open icon'
    Left = 3
    Top = 342
  end
  object dlgMakeInclude: TOpenDialog
    Filter = 'Makefile Addons (*.mak)|*.mak|All Files (*.*)|*.*'
    Left = 72
    Top = 344
  end
  object dlgCustomMake: TOpenDialog
    Filter = 'All Files (*.*)|*.*'
    FilterIndex = 0
    Left = 108
    Top = 344
  end
end
