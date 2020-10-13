object FormatterOptionsForm: TFormatterOptionsForm
  Left = 412
  Top = 756
  BorderStyle = bsDialog
  Caption = 'Formatter Options'
  ClientHeight = 600
  ClientWidth = 600
  Color = clWindow
  Ctl3D = False
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    600
    600)
  PixelsPerInch = 96
  TextHeight = 15
  object lblPoweredBy: TLabel
    Left = 0
    Top = 572
    Width = 321
    Height = 15
    Alignment = taCenter
    AutoSize = False
    Caption = 'Powered by AStyle'
  end
  object lblPreview: TLabel
    Left = 8
    Top = 296
    Width = 44
    Height = 15
    Caption = 'Preview:'
  end
  object btnOk: TBitBtn
    Left = 329
    Top = 568
    Width = 85
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&OK'
    Images = dmMain.SVGImageListMessageStyle
    ModalResult = 1
    NumGlyphs = 2
    TabOrder = 1
    OnClick = btnOkClick
  end
  object btnCancel: TBitBtn
    Left = 419
    Top = 568
    Width = 85
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Cancel'
    ModalResult = 2
    NumGlyphs = 2
    TabOrder = 2
    OnClick = btnCancelClick
  end
  object btnHelp: TBitBtn
    Left = 509
    Top = 568
    Width = 85
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Help'
    NumGlyphs = 2
    TabOrder = 3
    OnClick = btnHelpClick
  end
  object synExample: TSynEdit
    Left = 8
    Top = 314
    Width = 584
    Height = 249
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 0
    CodeFolding.CollapsedLineColor = clGrayText
    CodeFolding.FolderBarLinesColor = clGrayText
    CodeFolding.ShowCollapsedLine = True
    CodeFolding.IndentGuidesColor = clGray
    CodeFolding.IndentGuides = True
    UseCodeFolding = False
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.RightOffset = 21
    Lines.Strings = (
      'int Foo(bool isBar) {'
      #9'if(isBar) {'
      #9#9'bar();'
      #9#9'return 1;'
      #9'} else {'
      #9#9'return 0;'
      #9'}'
      '}')
    ReadOnly = True
    FontSmoothing = fsmNone
  end
  object grpOptions: TGroupBox
    Left = 8
    Top = 8
    Width = 584
    Height = 263
    Caption = 'Options'
    TabOrder = 4
    object lblBracketStyle: TLabel
      Left = 16
      Top = 24
      Width = 69
      Height = 15
      Caption = 'Bracket style:'
    end
    object lblIndentStyle: TLabel
      Left = 16
      Top = 56
      Width = 64
      Height = 15
      Caption = 'Indent style:'
    end
    object lblTabWidth: TLabel
      Left = 16
      Top = 88
      Width = 54
      Height = 15
      Caption = 'Tab width:'
    end
    object lblCommand: TLabel
      Left = 16
      Top = 154
      Width = 226
      Height = 15
      Caption = 'Final command (add customizations here):'
    end
    object bvCustom: TBevel
      Left = 20
      Top = 146
      Width = 544
      Height = 2
    end
    object lblIndentParts: TLabel
      Left = 280
      Top = 16
      Width = 184
      Height = 15
      Caption = 'Indent the following kinds of code:'
    end
    object cmbBracketStyle: TComboBox
      Left = 104
      Top = 20
      Width = 145
      Height = 23
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = '(do not modify)'
      OnChange = OptionChange
      Items.Strings = (
        '(do not modify)'
        'Allman'
        'Java'
        'K&R'
        'Stroustrup'
        'Whitesmith'
        'Banner'
        'GNU'
        'Linux'
        'Horstmann'
        'OTBS'
        'Pico'
        'Lisp')
    end
    object cmbIndentStyle: TComboBox
      Left = 104
      Top = 52
      Width = 145
      Height = 23
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 1
      Text = '(do not modify)'
      OnChange = OptionChange
      Items.Strings = (
        '(do not modify)'
        'Spaces'
        'Tabs'
        'Force Tab'
        'Force Tab X')
    end
    object spinTabWidth: TSpinEdit
      Left = 130
      Top = 84
      Width = 50
      Height = 24
      MaxValue = 20
      MinValue = 2
      TabOrder = 2
      Value = 4
      OnChange = OptionChange
    end
    object chkClasses: TCheckBox
      Left = 288
      Top = 40
      Width = 129
      Height = 17
      Caption = 'Classes'
      TabOrder = 3
      OnClick = OptionChange
    end
    object chkSwitches: TCheckBox
      Left = 288
      Top = 64
      Width = 129
      Height = 17
      Caption = 'Switches'
      TabOrder = 4
      OnClick = OptionChange
    end
    object chkNamespace: TCheckBox
      Left = 432
      Top = 40
      Width = 129
      Height = 17
      Caption = 'Namespaces'
      TabOrder = 6
      OnClick = OptionChange
    end
    object chkCases: TCheckBox
      Left = 288
      Top = 88
      Width = 129
      Height = 17
      Caption = 'Cases'
      TabOrder = 5
      OnClick = OptionChange
    end
    object chkLabels: TCheckBox
      Left = 432
      Top = 64
      Width = 129
      Height = 17
      Caption = 'Labels'
      TabOrder = 7
      OnClick = OptionChange
    end
    object chkPreprocessor: TCheckBox
      Left = 432
      Top = 88
      Width = 129
      Height = 17
      Caption = 'Preprocessor'
      TabOrder = 8
      OnClick = OptionChange
    end
    object memFullCommand: TMemo
      Left = 16
      Top = 174
      Width = 552
      Height = 81
      TabOrder = 9
      OnChange = CommandChange
    end
    object spinMaxLineLength: TSpinEdit
      Left = 130
      Top = 116
      Width = 50
      Height = 24
      MaxValue = 200
      MinValue = 50
      TabOrder = 10
      Value = 80
      OnChange = OptionChange
    end
    object chkMaxLineLength: TCheckBox
      Left = 16
      Top = 120
      Width = 112
      Height = 17
      Caption = 'Max line length:'
      TabOrder = 11
      OnClick = OptionChange
    end
  end
end
