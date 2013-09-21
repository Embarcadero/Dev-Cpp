object ImportMSVCForm: TImportMSVCForm
  Left = 192
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Import MS Visual C++(tm) project'
  ClientHeight = 221
  ClientWidth = 365
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lbSelect: TLabel
    Left = 8
    Top = 8
    Width = 137
    Height = 13
    Caption = 'Select Visual C++ project file:'
  end
  object btnBrowse: TSpeedButton
    Left = 336
    Top = 24
    Width = 23
    Height = 22
    Caption = '...'
    OnClick = btnBrowseClick
  end
  object txtVC: TEdit
    Left = 8
    Top = 24
    Width = 329
    Height = 21
    TabOrder = 0
    Text = 'txtVC'
    OnChange = txtDevChange
  end
  object gbOptions: TGroupBox
    Left = 8
    Top = 56
    Width = 349
    Height = 121
    Caption = '  Import options  '
    TabOrder = 1
    object lbConf: TLabel
      Left = 16
      Top = 20
      Width = 108
      Height = 13
      Caption = 'Configuration to import:'
    end
    object lbDev: TLabel
      Left = 16
      Top = 68
      Width = 122
      Height = 13
      Caption = 'Dev-C++ project filename:'
    end
    object btnBrowseDev: TSpeedButton
      Left = 314
      Top = 84
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = btnBrowseDevClick
    end
    object cmbConf: TComboBox
      Left = 16
      Top = 36
      Width = 321
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
    end
    object txtDev: TEdit
      Left = 16
      Top = 84
      Width = 297
      Height = 21
      TabOrder = 1
      Text = 'txtDev'
      OnChange = txtDevChange
    end
  end
  object btnImport: TButton
    Left = 103
    Top = 188
    Width = 75
    Height = 25
    Caption = 'Import'
    Default = True
    TabOrder = 2
    OnClick = btnImportClick
  end
  object btnCancel: TButton
    Left = 187
    Top = 188
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object OpenDialog1: TOpenDialog
    Filter = 'MSVC++ files|*.dsp'
    Left = 212
    Top = 48
  end
  object SaveDialog1: TSaveDialog
    Left = 268
    Top = 124
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
