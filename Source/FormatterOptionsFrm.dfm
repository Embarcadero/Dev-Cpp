object FormatterOptionsForm: TFormatterOptionsForm
  Left = 1825
  Top = 289
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Formatter Options'
  ClientHeight = 550
  ClientWidth = 600
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  ShowHint = True
  OnCreate = FormCreate
  DesignSize = (
    600
    550)
  PixelsPerInch = 96
  TextHeight = 15
  object lblPoweredBy: TLabel
    Left = 0
    Top = 522
    Width = 321
    Height = 15
    Alignment = taCenter
    AutoSize = False
    Caption = 'Powered by AStyle'
  end
  object lblPreview: TLabel
    Left = 8
    Top = 246
    Width = 44
    Height = 15
    Caption = 'Preview:'
  end
  object btnOk: TBitBtn
    Left = 330
    Top = 518
    Width = 85
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&OK'
    Default = True
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
    Left = 420
    Top = 518
    Width = 85
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
    OnClick = btnCancelClick
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      333333333333333333333333000033338833333333333333333F333333333333
      0000333911833333983333333388F333333F3333000033391118333911833333
      38F38F333F88F33300003339111183911118333338F338F3F8338F3300003333
      911118111118333338F3338F833338F3000033333911111111833333338F3338
      3333F8330000333333911111183333333338F333333F83330000333333311111
      8333333333338F3333383333000033333339111183333333333338F333833333
      00003333339111118333333333333833338F3333000033333911181118333333
      33338333338F333300003333911183911183333333383338F338F33300003333
      9118333911183333338F33838F338F33000033333913333391113333338FF833
      38F338F300003333333333333919333333388333338FFF830000333333333333
      3333333333333333333888330000333333333333333333333333333333333333
      0000}
    NumGlyphs = 2
  end
  object btnHelp: TBitBtn
    Left = 510
    Top = 518
    Width = 85
    Height = 25
    Anchors = [akLeft, akBottom]
    TabOrder = 3
    OnClick = btnHelpClick
    Kind = bkHelp
  end
  object synExample: TSynEdit
    Left = 8
    Top = 264
    Width = 584
    Height = 249
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 0
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
    Height = 233
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
      Width = 56
      Height = 15
      Caption = 'Tab width:'
    end
    object lblCommand: TLabel
      Left = 16
      Top = 124
      Width = 226
      Height = 15
      Caption = 'Final command (add customizations here):'
    end
    object bvCustom: TBevel
      Left = 20
      Top = 116
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
      ItemHeight = 15
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
      ItemHeight = 15
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
      Left = 104
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
      Top = 144
      Width = 552
      Height = 81
      TabOrder = 9
      OnChange = CommandChange
    end
  end
end
