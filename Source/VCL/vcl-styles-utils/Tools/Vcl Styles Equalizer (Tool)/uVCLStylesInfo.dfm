object FrmVCLStyleInfoDialog: TFrmVCLStyleInfoDialog
  Left = 802
  Top = 497
  BorderStyle = bsDialog
  Caption = 'VCL Style Info'
  ClientHeight = 187
  ClientWidth = 284
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 27
    Height = 13
    Caption = 'Name'
  end
  object Label2: TLabel
    Left = 145
    Top = 8
    Width = 33
    Height = 13
    Caption = 'Author'
  end
  object Label3: TLabel
    Left = 8
    Top = 48
    Width = 60
    Height = 13
    Caption = 'Author EMail'
  end
  object Label4: TLabel
    Left = 145
    Top = 48
    Width = 55
    Height = 13
    Caption = 'Author URL'
  end
  object Label5: TLabel
    Left = 8
    Top = 88
    Width = 35
    Height = 13
    Caption = 'Version'
  end
  object EditName: TEdit
    Left = 8
    Top = 24
    Width = 121
    Height = 21
    TabOrder = 0
  end
  object EditAuthor: TEdit
    Left = 145
    Top = 24
    Width = 121
    Height = 21
    TabOrder = 1
  end
  object EditEMail: TEdit
    Left = 8
    Top = 64
    Width = 121
    Height = 21
    TabOrder = 2
  end
  object EditURL: TEdit
    Left = 145
    Top = 64
    Width = 121
    Height = 21
    TabOrder = 3
  end
  object EditVersion: TEdit
    Left = 8
    Top = 104
    Width = 121
    Height = 21
    TabOrder = 4
  end
  object Button1: TButton
    Left = 8
    Top = 144
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 5
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 89
    Top = 144
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 6
    OnClick = Button2Click
  end
end
