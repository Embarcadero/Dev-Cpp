object ProcessListForm: TProcessListForm
  Left = 511
  Top = 339
  BorderIcons = []
  Caption = 'Attach to process'
  ClientHeight = 98
  ClientWidth = 409
  Color = clWindow
  Ctl3D = False
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 15
  object MainLabel: TLabel
    Left = 16
    Top = 12
    Width = 168
    Height = 15
    Caption = 'Attach to the following process:'
  end
  object OKBtn: TBitBtn
    Left = 246
    Top = 64
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    NumGlyphs = 2
    TabOrder = 0
  end
  object CancelBtn: TBitBtn
    Left = 326
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    NumGlyphs = 2
    TabOrder = 1
  end
  object ProcessCombo: TComboBox
    Left = 16
    Top = 32
    Width = 385
    Height = 23
    Style = csDropDownList
    TabOrder = 2
  end
end
