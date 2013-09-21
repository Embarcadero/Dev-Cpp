object CVSPasswdForm: TCVSPasswdForm
  Left = 577
  Top = 303
  BorderStyle = bsDialog
  Caption = 'CVS Password Required'
  ClientHeight = 92
  ClientWidth = 263
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 163
    Height = 15
    Caption = 'Please enter the CVS password:'
  end
  object txtPass: TEdit
    Left = 8
    Top = 28
    Width = 249
    Height = 23
    PasswordChar = '*'
    TabOrder = 0
  end
  object btnOK: TButton
    Left = 87
    Top = 60
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    TabStop = False
  end
end
