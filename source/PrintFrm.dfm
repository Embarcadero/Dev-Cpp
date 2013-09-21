object PrintForm: TPrintForm
  Left = 283
  Top = 193
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Print file'
  ClientHeight = 185
  ClientWidth = 432
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btnCancel: TBitBtn
    Left = 348
    Top = 156
    Width = 75
    Height = 24
    Caption = '&Cancel'
    TabOrder = 1
    Kind = bkCancel
  end
  object btnOk: TBitBtn
    Left = 270
    Top = 156
    Width = 75
    Height = 24
    Caption = '&OK'
    TabOrder = 0
    Kind = bkOK
  end
  object grpParams: TGroupBox
    Left = 8
    Top = 8
    Width = 417
    Height = 76
    Caption = 'Parameters : '
    TabOrder = 2
    object cbColors: TCheckBox
      Left = 8
      Top = 16
      Width = 145
      Height = 17
      Caption = '&Colors'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object cbHighlight: TCheckBox
      Left = 8
      Top = 32
      Width = 153
      Height = 17
      Caption = '&Highlight'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object rbLN: TRadioButton
      Left = 180
      Top = 32
      Width = 200
      Height = 17
      Caption = 'Print line numbers'
      Enabled = False
      TabOrder = 2
    end
    object rbLNMargin: TRadioButton
      Left = 180
      Top = 48
      Width = 229
      Height = 17
      Caption = 'Print line numbers in margin'
      Enabled = False
      TabOrder = 3
    end
    object cbWordWrap: TCheckBox
      Left = 8
      Top = 48
      Width = 161
      Height = 17
      Caption = '&Word wrap'
      TabOrder = 4
    end
    object cbLineNum: TCheckBox
      Left = 168
      Top = 16
      Width = 200
      Height = 17
      Caption = 'Line Numbers'
      TabOrder = 5
      OnClick = cbLineNumClick
    end
  end
  object grpPages: TGroupBox
    Left = 8
    Top = 85
    Width = 417
    Height = 63
    Caption = 'Pages :'
    TabOrder = 3
    object lblCopies: TLabel
      Left = 8
      Top = 16
      Width = 89
      Height = 13
      Caption = 'Number of copies :'
    end
    object seCopies: TSpinEdit
      Left = 16
      Top = 32
      Width = 75
      Height = 22
      MaxValue = 10000000
      MinValue = 1
      TabOrder = 0
      Value = 1
    end
    object cbSelection: TCheckBox
      Left = 168
      Top = 16
      Width = 200
      Height = 17
      Caption = 'Print &selection only'
      TabOrder = 1
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
    Left = 136
    Top = 56
  end
end
