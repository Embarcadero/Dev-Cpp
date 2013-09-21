object WindowListForm: TWindowListForm
  Left = 462
  Top = 283
  Width = 354
  Height = 317
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Window List'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 0
    Width = 346
    Height = 6
    Align = alTop
    AutoSize = False
  end
  object Label2: TLabel
    Left = 0
    Top = 6
    Width = 6
    Height = 243
    Align = alLeft
    AutoSize = False
  end
  object Label3: TLabel
    Left = 340
    Top = 6
    Width = 6
    Height = 243
    Align = alRight
    AutoSize = False
  end
  object Panel: TPanel
    Left = 0
    Top = 249
    Width = 346
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      346
      41)
    object OkBtn: TBitBtn
      Left = 158
      Top = 10
      Width = 91
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
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
    object CancelBtn: TBitBtn
      Left = 249
      Top = 10
      Width = 91
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Cancel'
      TabOrder = 1
      Kind = bkCancel
    end
  end
  object GroupBox: TGroupBox
    Left = 6
    Top = 6
    Width = 334
    Height = 243
    Align = alClient
    Caption = 'Select the window you want to edit :'
    TabOrder = 1
    object Label4: TLabel
      Left = 2
      Top = 15
      Width = 330
      Height = 3
      Align = alTop
      AutoSize = False
    end
    object Label5: TLabel
      Left = 2
      Top = 18
      Width = 6
      Height = 217
      Align = alLeft
      AutoSize = False
    end
    object Label6: TLabel
      Left = 326
      Top = 18
      Width = 6
      Height = 217
      Align = alRight
      AutoSize = False
    end
    object Label7: TLabel
      Left = 2
      Top = 235
      Width = 330
      Height = 6
      Align = alBottom
      AutoSize = False
    end
    object UnitList: TListBox
      Left = 8
      Top = 18
      Width = 318
      Height = 212
      Align = alClient
      IntegralHeight = True
      ItemHeight = 13
      TabOrder = 0
      OnDblClick = UnitListDblClick
      OnKeyDown = UnitListKeyDown
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
