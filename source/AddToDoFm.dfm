object AddToDoForm: TAddToDoForm
  Left = 192
  Top = 107
  BorderStyle = bsToolWindow
  Caption = 'Add To-Do item'
  ClientHeight = 205
  ClientWidth = 293
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
    Top = 8
    Width = 56
    Height = 13
    Caption = 'Description:'
  end
  object Label2: TLabel
    Left = 8
    Top = 124
    Width = 34
    Height = 13
    Caption = 'Priority:'
  end
  object Label3: TLabel
    Left = 132
    Top = 124
    Width = 25
    Height = 13
    Caption = 'User:'
  end
  object memDescr: TMemo
    Left = 8
    Top = 24
    Width = 277
    Height = 89
    Lines.Strings = (
      'memDescr')
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object spnPri: TSpinEdit
    Left = 8
    Top = 140
    Width = 113
    Height = 22
    MaxLength = 1
    MaxValue = 9
    MinValue = 1
    TabOrder = 1
    Value = 1
  end
  object btnOK: TButton
    Left = 67
    Top = 172
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 3
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 151
    Top = 172
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object txtUser: TEdit
    Left = 132
    Top = 140
    Width = 153
    Height = 21
    TabOrder = 2
    Text = 'txtUser'
    OnKeyPress = txtUserKeyPress
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
