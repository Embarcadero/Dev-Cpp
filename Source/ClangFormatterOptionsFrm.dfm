object ClangFormatterOptionsForm: TClangFormatterOptionsForm
  Left = 412
  Top = 756
  BorderStyle = bsDialog
  Caption = 'Formatter Options - Clang-Format '
  ClientHeight = 569
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
    569)
  PixelsPerInch = 96
  TextHeight = 15
  object lblPoweredBy: TLabel
    Left = 0
    Top = 539
    Width = 232
    Height = 15
    Alignment = taCenter
    AutoSize = False
    Caption = 'Powered by Clang-Format'
  end
  object lblPreview: TLabel
    Left = 8
    Top = 263
    Width = 44
    Height = 15
    Caption = 'Preview:'
  end
  object btnOk: TBitBtn
    Left = 329
    Top = 537
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
    Top = 537
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
    Top = 537
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
    Top = 281
    Width = 584
    Height = 249
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Font.Quality = fqClearTypeNatural
    TabOrder = 0
    CodeFolding.ShowCollapsedLine = True
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
  end
  object grpOptions: TGroupBox
    Left = 8
    Top = 8
    Width = 584
    Height = 249
    Caption = 'Options'
    TabOrder = 4
    object lblBracketAlignmentStyle: TLabel
      Left = 280
      Top = 24
      Width = 129
      Height = 15
      Caption = 'Bracket Alignment Style:'
    end
    object lblCommand: TLabel
      Left = 16
      Top = 138
      Width = 226
      Height = 15
      Caption = 'Final command (add customizations here):'
    end
    object bvCustom: TBevel
      Left = 20
      Top = 130
      Width = 544
      Height = 2
    end
    object lblBasedOnStyle: TLabel
      Left = 16
      Top = 24
      Width = 78
      Height = 15
      Caption = 'Based on style:'
    end
    object cmbBracketAlignmentStyle: TComboBox
      Left = 415
      Top = 21
      Width = 145
      Height = 23
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = '(do not modify)'
      OnChange = OptionChange
      Items.Strings = (
        '(do not modify)'
        'Align'
        'Don'#8217't align'
        'Always break')
    end
    object spIndentWidth: TSpinEdit
      Left = 130
      Top = 58
      Width = 50
      Height = 24
      MaxValue = 20
      MinValue = 2
      TabOrder = 1
      Value = 4
      OnChange = OptionChange
    end
    object chkBracedListStyle: TCheckBox
      Left = 280
      Top = 60
      Width = 129
      Height = 17
      Caption = 'Braced List Style'
      TabOrder = 2
      OnClick = OptionChange
    end
    object memFullCommand: TMemo
      Left = 16
      Top = 159
      Width = 552
      Height = 81
      TabOrder = 3
      OnChange = CommandChange
    end
    object spColumnLimit: TSpinEdit
      Left = 394
      Top = 91
      Width = 50
      Height = 24
      MaxValue = 200
      MinValue = 50
      TabOrder = 4
      Value = 80
      OnChange = OptionChange
    end
    object chkColumnLimit: TCheckBox
      Left = 280
      Top = 95
      Width = 112
      Height = 17
      Caption = 'Column Limit:'
      TabOrder = 5
      OnClick = OptionChange
    end
    object coBasedOnStyle: TComboBox
      Left = 104
      Top = 21
      Width = 145
      Height = 23
      Style = csDropDownList
      ItemIndex = 1
      TabOrder = 6
      Text = 'LLVM'
      OnChange = OptionChange
      Items.Strings = (
        '(do not modify)'
        'LLVM'
        'Google'
        'Chromium'
        'Mozilla'
        'WebKit'
        'Microsoft'
        'GNU')
    end
    object spTabWidth: TSpinEdit
      Left = 130
      Top = 93
      Width = 50
      Height = 24
      MaxValue = 20
      MinValue = 2
      TabOrder = 7
      Value = 4
      OnChange = OptionChange
    end
    object chkIndentWidth: TCheckBox
      Left = 16
      Top = 60
      Width = 112
      Height = 17
      Caption = 'Indent width:'
      TabOrder = 8
      OnClick = OptionChange
    end
    object chkTabWidth: TCheckBox
      Left = 16
      Top = 93
      Width = 112
      Height = 17
      Caption = 'Tab width:'
      TabOrder = 9
      OnClick = OptionChange
    end
  end
  object BtnExecute: TBitBtn
    Left = 238
    Top = 537
    Width = 85
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'E&xecute'
    Images = dmMain.SVGImageListMessageStyle
    ModalResult = 1
    NumGlyphs = 2
    TabOrder = 5
    OnClick = btnExecuteClick
  end
end
