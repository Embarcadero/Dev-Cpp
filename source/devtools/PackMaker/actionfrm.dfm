object ActionForm: TActionForm
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Choose an Action'
  ClientHeight = 109
  ClientWidth = 252
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 8
    Width = 233
    Height = 57
    Caption = 'What would you like to do?'
    ItemIndex = 0
    Items.Strings = (
      '&Create a new package'
      '&Open an existing package')
    TabOrder = 0
  end
  object BitBtn1: TBitBtn
    Left = 41
    Top = 76
    Width = 81
    Height = 25
    TabOrder = 1
    Kind = bkOK
  end
  object BitBtn2: TBitBtn
    Left = 129
    Top = 76
    Width = 81
    Height = 25
    TabOrder = 2
    Kind = bkCancel
  end
end
