object frmReplace: TfrmReplace
  Left = 264
  Top = 180
  ActiveControl = cboFindText
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Replace Text'
  ClientHeight = 251
  ClientWidth = 343
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblFind: TLabel
    Left = 8
    Top = 12
    Width = 56
    Height = 13
    Caption = 'Text to find:'
    FocusControl = cboFindText
  end
  object lblReplace: TLabel
    Left = 10
    Top = 42
    Width = 65
    Height = 13
    Caption = 'Replace with:'
  end
  object cboFindText: TComboBox
    Left = 96
    Top = 8
    Width = 237
    Height = 21
    ItemHeight = 13
    TabOrder = 0
  end
  object grpOptions: TGroupBox
    Left = 8
    Top = 64
    Width = 160
    Height = 80
    Caption = ' Options '
    TabOrder = 2
    object cbMatchCase: TCheckBox
      Left = 8
      Top = 16
      Width = 150
      Height = 16
      Caption = 'C&ase sensitive'
      TabOrder = 0
    end
    object cbWholeWord: TCheckBox
      Left = 8
      Top = 36
      Width = 150
      Height = 16
      Caption = '&Whole words only'
      TabOrder = 1
    end
    object cbPrompt: TCheckBox
      Left = 8
      Top = 54
      Width = 150
      Height = 17
      Caption = '&Prompt on Replace'
      TabOrder = 2
    end
  end
  object btnReplace: TButton
    Left = 8
    Top = 219
    Width = 80
    Height = 24
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 6
    OnClick = btnReplaceClick
  end
  object btnCancel: TButton
    Left = 254
    Top = 219
    Width = 80
    Height = 24
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 8
    OnClick = btnCancelClick
  end
  object grpDirection: TGroupBox
    Left = 175
    Top = 64
    Width = 160
    Height = 80
    Caption = ' Direction '
    TabOrder = 3
    object rbForward: TRadioButton
      Left = 8
      Top = 16
      Width = 105
      Height = 17
      Caption = 'Forwar&d'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object rbBackward: TRadioButton
      Left = 8
      Top = 36
      Width = 105
      Height = 17
      Caption = '&Backward'
      TabOrder = 1
    end
  end
  object grpScope: TGroupBox
    Left = 8
    Top = 150
    Width = 160
    Height = 60
    Caption = ' Scope '
    TabOrder = 4
    object rbGlobal: TRadioButton
      Left = 8
      Top = 16
      Width = 105
      Height = 17
      Caption = '&Global'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object rbSelectedOnly: TRadioButton
      Left = 8
      Top = 36
      Width = 105
      Height = 17
      Caption = '&Selected only'
      TabOrder = 1
    end
  end
  object grpOrigin: TGroupBox
    Left = 175
    Top = 150
    Width = 160
    Height = 60
    Caption = ' Origin '
    TabOrder = 5
    object rbFromCursor: TRadioButton
      Left = 8
      Top = 16
      Width = 105
      Height = 17
      Caption = '&From cursor'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object rbEntireScope: TRadioButton
      Left = 8
      Top = 36
      Width = 105
      Height = 17
      Caption = '&Entire scope'
      TabOrder = 1
    end
  end
  object cboReplaceText: TComboBox
    Left = 97
    Top = 38
    Width = 236
    Height = 21
    ItemHeight = 13
    TabOrder = 1
  end
  object btnReplaceAll: TButton
    Left = 94
    Top = 219
    Width = 74
    Height = 24
    Caption = 'Replace &All'
    ModalResult = 8
    TabOrder = 7
    OnClick = btnReplaceClick
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
