object Form1: TForm1
  Left = 468
  Top = 301
  Caption = 'Custom Draw Popup Menu'
  ClientHeight = 202
  ClientWidth = 447
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PopupMenu = PopupMenu1
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 136
    Top = 80
    Width = 150
    Height = 13
    Caption = 'Right Click to Show PopupMenu'
  end
  object PopupMenu1: TPopupMenu
    Left = 312
    Top = 104
    object I1: TMenuItem
      Caption = 'Item 1'
    end
    object I2: TMenuItem
      Caption = 'Default Item'
      Default = True
    end
    object I3: TMenuItem
      Caption = 'Disabled Item'
      Enabled = False
    end
    object I4: TMenuItem
      Caption = 'Item 4'
    end
  end
end
