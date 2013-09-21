object ProcessListForm: TProcessListForm
  Left = 511
  Top = 339
  Width = 425
  Height = 137
  BorderIcons = []
  Caption = 'Attach to process'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
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
    TabOrder = 0
    Kind = bkOK
  end
  object CancelBtn: TBitBtn
    Left = 326
    Top = 64
    Width = 75
    Height = 25
    TabOrder = 1
    Kind = bkCancel
  end
  object ProcessCombo: TComboBox
    Left = 16
    Top = 32
    Width = 385
    Height = 23
    Style = csDropDownList
    ItemHeight = 15
    TabOrder = 2
  end
end
