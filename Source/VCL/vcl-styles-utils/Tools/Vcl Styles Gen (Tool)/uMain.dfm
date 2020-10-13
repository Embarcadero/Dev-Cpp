object FrmMain: TFrmMain
  Left = 0
  Top = 0
  Caption = 'Batch Vcl Styles generator'
  ClientHeight = 333
  ClientWidth = 324
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ImageVCLStyle: TImage
    Left = 8
    Top = 59
    Width = 306
    Height = 170
  end
  object Label1: TLabel
    Left = 8
    Top = 13
    Width = 45
    Height = 13
    Caption = 'Vcl Styles'
  end
  object Label2: TLabel
    Left = 8
    Top = 284
    Width = 67
    Height = 13
    Caption = 'Folder Output'
  end
  object Button1: TButton
    Left = 241
    Top = 301
    Width = 75
    Height = 25
    Caption = 'Generate'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 235
    Width = 306
    Height = 17
    TabOrder = 1
  end
  object ComboBoxVclStyles: TComboBox
    Left = 8
    Top = 32
    Width = 225
    Height = 21
    Style = csDropDownList
    TabOrder = 2
    OnChange = ComboBoxVclStylesChange
  end
  object EditPath: TEdit
    Left = 8
    Top = 303
    Width = 225
    Height = 21
    TabOrder = 3
  end
  object CheckBoxSepia: TCheckBox
    Left = 8
    Top = 258
    Width = 97
    Height = 17
    Caption = 'Sepia'
    TabOrder = 4
  end
end
