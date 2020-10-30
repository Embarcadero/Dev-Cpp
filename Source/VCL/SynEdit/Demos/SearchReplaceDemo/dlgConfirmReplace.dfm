object ConfirmReplaceDialog: TConfirmReplaceDialog
  Left = 176
  Top = 158
  BorderStyle = bsDialog
  Caption = 'Confirm replace'
  ClientHeight = 98
  ClientWidth = 328
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lblConfirmation: TLabel
    Left = 60
    Top = 12
    Width = 261
    Height = 44
    AutoSize = False
    WordWrap = True
  end
  object Image1: TImage
    Left = 16
    Top = 16
    Width = 32
    Height = 32
  end
  object btnReplace: TButton
    Left = 8
    Top = 67
    Width = 75
    Height = 23
    Caption = '&Yes'
    Default = True
    ModalResult = 6
    TabOrder = 0
  end
  object btnSkip: TButton
    Left = 87
    Top = 67
    Width = 75
    Height = 23
    Caption = '&No'
    ModalResult = 7
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 166
    Top = 67
    Width = 75
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btnReplaceAll: TButton
    Left = 245
    Top = 67
    Width = 75
    Height = 23
    Caption = 'Yes to &all'
    ModalResult = 10
    TabOrder = 3
  end
end
