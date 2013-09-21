object CVSPasswdForm: TCVSPasswdForm
  Left = 255
  Top = 115
  BorderStyle = bsDialog
  Caption = 'CVS Password Required'
  ClientHeight = 142
  ClientWidth = 249
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 56
    Width = 152
    Height = 13
    Caption = 'Please enter the CVS password:'
  end
  object Label2: TLabel
    Left = 8
    Top = 8
    Width = 235
    Height = 41
    AutoSize = False
    Caption = 
      'The CVS server requires a password to be supplied to allow conne' +
      'ction.'
    WordWrap = True
  end
  object txtPass: TEdit
    Left = 8
    Top = 76
    Width = 233
    Height = 21
    PasswordChar = '*'
    TabOrder = 0
  end
  object btnOK: TButton
    Left = 87
    Top = 108
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    TabStop = False
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
