object Form2: TForm2
  Left = 345
  Top = 197
  BorderStyle = bsDialog
  Caption = 'Reserved Words'
  ClientHeight = 320
  ClientWidth = 271
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lbKeywords: TListBox
    Left = 8
    Top = 8
    Width = 177
    Height = 305
    ItemHeight = 13
    TabOrder = 0
  end
  object btnLoad: TButton
    Left = 192
    Top = 104
    Width = 75
    Height = 25
    Caption = '&Load'
    TabOrder = 1
    OnClick = btnLoadClick
  end
  object btnClose: TButton
    Left = 192
    Top = 136
    Width = 75
    Height = 25
    Caption = '&Close'
    ModalResult = 1
    TabOrder = 2
  end
  object btnAdd: TButton
    Left = 192
    Top = 8
    Width = 75
    Height = 25
    Caption = '&Add'
    TabOrder = 3
    OnClick = btnAddClick
  end
  object btnEdit: TButton
    Left = 192
    Top = 40
    Width = 75
    Height = 25
    Caption = '&Edit'
    TabOrder = 4
    OnClick = btnEditClick
  end
  object btnDelete: TButton
    Left = 192
    Top = 72
    Width = 75
    Height = 25
    Caption = '&Delete'
    TabOrder = 5
    OnClick = btnDeleteClick
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Text Files (*.txt)|*.txt|All Files (*.*)|*.*'
    Left = 192
    Top = 272
  end
end
