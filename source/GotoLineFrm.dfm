object GotoLineForm: TGotoLineForm
  Left = 467
  Top = 408
  ActiveControl = Line
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Go to line...'
  ClientHeight = 79
  ClientWidth = 246
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnKeyUp = FormKeyUp
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GotoLabel: TLabel
    Left = 11
    Top = 12
    Width = 89
    Height = 13
    Caption = 'Go to line number :'
  end
  object Line: TSpinEdit
    Left = 12
    Top = 28
    Width = 137
    Height = 22
    MaxValue = 2
    MinValue = 1
    TabOrder = 0
    Value = 1
    OnKeyDown = LineKeyDown
  end
  object BtnOK: TButton
    Left = 162
    Top = 12
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object BtnCancel: TButton
    Left = 162
    Top = 45
    Width = 75
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
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
    Left = 110
    Top = 12
  end
end
