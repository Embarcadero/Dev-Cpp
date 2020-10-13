object FrmMain: TFrmMain
  Left = 431
  Top = 301
  Caption = 'Simple Demo'
  ClientHeight = 234
  ClientWidth = 323
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
  object Label1: TLabel
    Left = 8
    Top = 184
    Width = 303
    Height = 13
    Caption = 'Press the below button to see how the current style is modified'
  end
  object RadioButton1: TRadioButton
    Left = 8
    Top = 8
    Width = 113
    Height = 17
    Caption = 'RadioButton1'
    TabOrder = 0
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 31
    Width = 97
    Height = 17
    Caption = 'CheckBox1'
    TabOrder = 1
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 62
    Width = 289
    Height = 105
    ActivePage = TabSheet1
    TabOrder = 2
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
      object Edit1: TEdit
        Left = 3
        Top = 16
        Width = 121
        Height = 21
        TabOrder = 0
        Text = 'Edit1'
      end
      object Button2: TButton
        Left = 130
        Top = 12
        Width = 75
        Height = 25
        Caption = 'Make me red'
        TabOrder = 1
        OnClick = Button2Click
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'TabSheet2'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
  end
  object Button3: TButton
    Left = 8
    Top = 203
    Width = 75
    Height = 25
    Caption = 'Press Me'
    TabOrder = 3
    OnClick = Button3Click
  end
end
