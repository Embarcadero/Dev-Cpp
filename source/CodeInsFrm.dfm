object frmCodeEdit: TfrmCodeEdit
  Left = 649
  Top = 284
  BorderStyle = bsToolWindow
  Caption = 'Code Insert Entry'
  ClientHeight = 152
  ClientWidth = 354
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 337
    Height = 106
    Shape = bsFrame
  end
  object lblMenu: TLabel
    Left = 16
    Top = 16
    Width = 59
    Height = 15
    Caption = 'Menu Text:'
  end
  object lblSec: TLabel
    Left = 16
    Top = 56
    Width = 42
    Height = 15
    Caption = 'Section:'
  end
  object lblDesc: TLabel
    Left = 110
    Top = 56
    Width = 60
    Height = 15
    Caption = 'Description'
  end
  object edMenuText: TEdit
    Left = 24
    Top = 32
    Width = 305
    Height = 23
    TabOrder = 0
    OnChange = edMenuTextChange
  end
  object seSection: TSpinEdit
    Left = 24
    Top = 72
    Width = 67
    Height = 24
    MaxValue = 0
    MinValue = 0
    TabOrder = 1
    Value = 0
  end
  object btnOk: TBitBtn
    Left = 185
    Top = 121
    Width = 75
    Height = 25
    Enabled = False
    TabOrder = 3
    OnClick = btnOkClick
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 271
    Top = 121
    Width = 75
    Height = 25
    TabOrder = 4
    Kind = bkCancel
  end
  object edDesc: TEdit
    Left = 112
    Top = 74
    Width = 217
    Height = 23
    TabOrder = 2
  end
end
