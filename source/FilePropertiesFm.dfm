object FilePropertiesForm: TFilePropertiesForm
  Left = 287
  Top = 160
  BorderStyle = bsDialog
  Caption = 'File properties'
  ClientHeight = 265
  ClientWidth = 409
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel7: TBevel
    Left = 8
    Top = 8
    Width = 393
    Height = 213
    Style = bsRaised
  end
  object Bevel8: TBevel
    Left = 208
    Top = 168
    Width = 185
    Height = 21
  end
  object Bevel2: TBevel
    Left = 208
    Top = 144
    Width = 185
    Height = 21
  end
  object Bevel3: TBevel
    Left = 16
    Top = 120
    Width = 185
    Height = 21
  end
  object Bevel4: TBevel
    Left = 16
    Top = 144
    Width = 185
    Height = 21
  end
  object Bevel6: TBevel
    Left = 208
    Top = 120
    Width = 185
    Height = 21
  end
  object Bevel5: TBevel
    Left = 16
    Top = 168
    Width = 185
    Height = 21
  end
  object Bevel1: TBevel
    Left = 16
    Top = 16
    Width = 377
    Height = 97
  end
  object Label1: TLabel
    Left = 24
    Top = 48
    Width = 45
    Height = 13
    Caption = 'Filename:'
  end
  object Label2: TLabel
    Left = 24
    Top = 24
    Width = 47
    Height = 13
    Caption = 'In project:'
  end
  object Label3: TLabel
    Left = 20
    Top = 124
    Width = 78
    Height = 13
    Caption = 'Total lines in file:'
  end
  object Label4: TLabel
    Left = 20
    Top = 148
    Width = 96
    Height = 13
    Caption = 'Actual lines of code:'
  end
  object Label5: TLabel
    Left = 20
    Top = 172
    Width = 71
    Height = 13
    Caption = 'Comment lines:'
  end
  object Label6: TLabel
    Left = 212
    Top = 148
    Width = 40
    Height = 13
    Caption = 'File size:'
  end
  object Label7: TLabel
    Left = 212
    Top = 124
    Width = 56
    Height = 13
    Caption = 'Empty lines:'
  end
  object lblFilename: TLabel
    Left = 100
    Top = 48
    Width = 285
    Height = 13
    AutoSize = False
    Caption = 'lblFilename'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblProject: TLabel
    Left = 100
    Top = 24
    Width = 285
    Height = 13
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblSize: TLabel
    Left = 381
    Top = 148
    Width = 5
    Height = 13
    Alignment = taRightJustify
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblTotal: TLabel
    Left = 189
    Top = 124
    Width = 5
    Height = 13
    Alignment = taRightJustify
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblCode: TLabel
    Left = 190
    Top = 148
    Width = 5
    Height = 13
    Alignment = taRightJustify
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblComments: TLabel
    Left = 190
    Top = 172
    Width = 5
    Height = 13
    Alignment = taRightJustify
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblEmpty: TLabel
    Left = 382
    Top = 124
    Width = 5
    Height = 13
    Alignment = taRightJustify
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label8: TLabel
    Left = 212
    Top = 172
    Width = 65
    Height = 13
    Caption = 'Included files:'
  end
  object lblIncludes: TLabel
    Left = 381
    Top = 172
    Width = 5
    Height = 13
    Alignment = taRightJustify
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label9: TLabel
    Left = 24
    Top = 72
    Width = 44
    Height = 13
    Caption = 'Absolute:'
  end
  object Label10: TLabel
    Left = 24
    Top = 92
    Width = 42
    Height = 13
    Caption = 'Relative:'
  end
  object lblAbsolute: TLabel
    Left = 100
    Top = 72
    Width = 285
    Height = 13
    AutoSize = False
  end
  object lblRelative: TLabel
    Left = 100
    Top = 92
    Width = 285
    Height = 13
    AutoSize = False
  end
  object Bevel9: TBevel
    Left = 16
    Top = 192
    Width = 377
    Height = 21
  end
  object Label11: TLabel
    Left = 20
    Top = 196
    Width = 54
    Height = 13
    Caption = 'Timestamp:'
  end
  object lblTimestamp: TLabel
    Left = 381
    Top = 196
    Width = 5
    Height = 13
    Alignment = taRightJustify
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object btnOK: TButton
    Left = 167
    Top = 232
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = btnOKClick
  end
  object cmbFiles: TComboBox
    Left = 100
    Top = 44
    Width = 289
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnClick = cmbFilesClick
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
    Left = 152
    Top = 80
  end
end
