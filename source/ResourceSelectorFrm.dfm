object SelectResource: TSelectResource
  Left = 292
  Top = 134
  BorderStyle = bsDialog
  Caption = 'Select Resource File'
  ClientHeight = 243
  ClientWidth = 299
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
  object List: TListBox
    Left = 8
    Top = 32
    Width = 177
    Height = 201
    ItemHeight = 13
    TabOrder = 1
    OnClick = ListClick
    OnDblClick = ListDblClick
  end
  object OkBtn: TBitBtn
    Left = 200
    Top = 8
    Width = 89
    Height = 25
    TabOrder = 2
    Kind = bkOK
  end
  object Cancel: TBitBtn
    Left = 200
    Top = 40
    Width = 89
    Height = 25
    TabOrder = 3
    Kind = bkCancel
  end
  object Edit1: TEdit
    Left = 8
    Top = 8
    Width = 177
    Height = 21
    TabOrder = 0
    OnChange = Edit1Change
  end
end
