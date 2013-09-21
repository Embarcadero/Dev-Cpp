object NewVarForm: TNewVarForm
  Left = 221
  Top = 80
  BorderStyle = bsDialog
  Caption = 'New variable'
  ClientHeight = 449
  ClientWidth = 349
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 12
    Width = 58
    Height = 13
    Caption = 'Return type:'
  end
  object Label2: TLabel
    Left = 8
    Top = 36
    Width = 70
    Height = 13
    Caption = 'Variable name:'
  end
  object Label3: TLabel
    Left = 52
    Top = 172
    Width = 31
    Height = 13
    Alignment = taRightJustify
    Caption = 'Name:'
  end
  object Label4: TLabel
    Left = 52
    Top = 224
    Width = 31
    Height = 13
    Alignment = taRightJustify
    Caption = 'Name:'
  end
  object Label6: TLabel
    Left = 8
    Top = 68
    Width = 62
    Height = 13
    Caption = 'Implement in:'
  end
  object rgScope: TRadioGroup
    Left = 8
    Top = 96
    Width = 333
    Height = 41
    Caption = 'Access scope'
    Columns = 4
    Items.Strings = (
      'Private'
      'Protected'
      'Public'
      'Published')
    TabOrder = 3
  end
  object cmbType: TComboBox
    Left = 88
    Top = 8
    Width = 253
    Height = 21
    AutoDropDown = True
    ItemHeight = 13
    Sorted = True
    TabOrder = 0
    Text = 'cmbType'
    OnChange = cmbTypeChange
    Items.Strings = (
      'bool'
      'double'
      'float'
      'int')
  end
  object txtName: TEdit
    Left = 88
    Top = 32
    Width = 253
    Height = 21
    TabOrder = 1
    Text = 'txtName'
    OnChange = cmbTypeChange
  end
  object chkReadFunc: TCheckBox
    Left = 8
    Top = 148
    Width = 333
    Height = 17
    Caption = 'Create member function to read from this variable'
    TabOrder = 4
    OnClick = chkReadFuncClick
  end
  object chkWriteFunc: TCheckBox
    Left = 8
    Top = 200
    Width = 333
    Height = 17
    Caption = 'Create member function to write to this variable'
    TabOrder = 7
    OnClick = chkWriteFuncClick
  end
  object txtReadFunc: TEdit
    Left = 88
    Top = 168
    Width = 253
    Height = 21
    TabOrder = 6
    Text = 'txtReadFunc'
    OnChange = cmbTypeChange
  end
  object txtWriteFunc: TEdit
    Left = 88
    Top = 220
    Width = 253
    Height = 21
    TabOrder = 9
    Text = 'txtWriteFunc'
    OnChange = cmbTypeChange
  end
  object btnCreate: TButton
    Left = 95
    Top = 416
    Width = 75
    Height = 25
    Caption = 'Create'
    Default = True
    ModalResult = 1
    TabOrder = 11
    OnClick = btnCreateClick
  end
  object btnCancel: TButton
    Left = 179
    Top = 416
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 12
  end
  object cmbClass: TComboBox
    Left = 88
    Top = 64
    Width = 253
    Height = 21
    AutoDropDown = True
    ItemHeight = 13
    Sorted = True
    TabOrder = 2
    Text = 'cmbClass'
    Items.Strings = (
      'bool'
      'double'
      'float'
      'int'
      'void')
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 252
    Width = 333
    Height = 153
    Caption = 'Comment'
    TabOrder = 10
    object Label5: TLabel
      Left = 8
      Top = 20
      Width = 56
      Height = 13
      Caption = 'Description:'
    end
    object Label7: TLabel
      Left = 8
      Top = 128
      Width = 26
      Height = 13
      Caption = 'Style:'
    end
    object memDescr: TMemo
      Left = 16
      Top = 36
      Width = 309
      Height = 81
      Lines.Strings = (
        'memDescr')
      ScrollBars = ssBoth
      TabOrder = 0
      OnChange = memDescrChange
    end
    object cmbComment: TComboBox
      Left = 40
      Top = 124
      Width = 205
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 1
      Text = '/** doxygen style comment */'
      Items.Strings = (
        '/** doxygen style comment */'
        '/* C style comment */'
        '// C++ style comment')
    end
  end
  object chkInlineR: TCheckBox
    Left = 288
    Top = 148
    Width = 53
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Inline'
    Enabled = False
    TabOrder = 5
  end
  object chkInlineW: TCheckBox
    Left = 288
    Top = 200
    Width = 53
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Inline'
    Enabled = False
    TabOrder = 8
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
    Left = 136
    Top = 56
  end
end
