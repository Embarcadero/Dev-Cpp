object ModifyVarForm: TModifyVarForm
  Left = 389
  Top = 262
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Modify variable watch'
  ClientHeight = 135
  ClientWidth = 297
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object VarNameLabel: TLabel
    Left = 8
    Top = 8
    Width = 86
    Height = 13
    Caption = 'Variable to modify:'
  end
  object ValueLabel: TLabel
    Left = 8
    Top = 56
    Width = 54
    Height = 13
    Caption = 'New value:'
  end
  object OkBtn: TBitBtn
    Left = 136
    Top = 104
    Width = 75
    Height = 25
    TabOrder = 0
    Kind = bkOK
  end
  object CancelBtn: TBitBtn
    Left = 214
    Top = 104
    Width = 75
    Height = 25
    TabOrder = 1
    Kind = bkCancel
  end
  object NameEdit: TEdit
    Left = 8
    Top = 26
    Width = 281
    Height = 21
    TabOrder = 2
  end
  object ValueEdit: TEdit
    Left = 8
    Top = 74
    Width = 281
    Height = 21
    TabOrder = 3
  end
end
