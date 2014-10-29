object MenuForm: TMenuForm
  Left = 334
  Top = 208
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Create start menu item'
  ClientHeight = 192
  ClientWidth = 229
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 229
    Height = 192
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 84
      Height = 13
      Caption = 'Menu item name :'
    end
    object lblIcon: TLabel
      Left = 8
      Top = 108
      Width = 27
      Height = 13
      Caption = 'Icon :'
    end
    object Label2: TLabel
      Left = 8
      Top = 58
      Width = 37
      Height = 13
      Caption = 'Target :'
    end
    object edName: TEdit
      Left = 8
      Top = 24
      Width = 209
      Height = 21
      TabOrder = 0
      Text = 'MyApp'
    end
    object edIcon: TEdit
      Left = 8
      Top = 124
      Width = 209
      Height = 21
      TabOrder = 2
      Text = '<app>\MyApp.ico'
    end
    object OkBtn: TBitBtn
      Left = 64
      Top = 160
      Width = 75
      Height = 25
      TabOrder = 3
      Kind = bkOK
    end
    object CancelBtn: TBitBtn
      Left = 144
      Top = 160
      Width = 75
      Height = 25
      TabOrder = 4
      Kind = bkCancel
    end
    object edTarget: TEdit
      Left = 8
      Top = 74
      Width = 209
      Height = 21
      TabOrder = 1
      Text = '<app>\MyApp.exe'
    end
  end
end
