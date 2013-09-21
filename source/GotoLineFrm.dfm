object GotoLineForm: TGotoLineForm
  Left = 467
  Top = 408
  ActiveControl = Line
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Go to line...'
  ClientHeight = 79
  ClientWidth = 246
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 15
  object GotoLabel: TLabel
    Left = 11
    Top = 12
    Width = 102
    Height = 15
    Caption = 'Go to line number :'
  end
  object Line: TSpinEdit
    Left = 12
    Top = 28
    Width = 137
    Height = 24
    MaxValue = 2
    MinValue = 1
    TabOrder = 0
    Value = 1
    OnKeyDown = LineKeyDown
  end
  object BtnOK: TButton
    Left = 162
    Top = 12
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object BtnCancel: TButton
    Left = 162
    Top = 45
    Width = 75
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
