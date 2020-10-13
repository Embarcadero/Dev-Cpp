object FrmMain: TFrmMain
  Left = 529
  Top = 187
  Caption = 'Demo VCL Styles Colors'
  ClientHeight = 372
  ClientWidth = 557
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Edit1: TEdit
    Left = 8
    Top = 8
    Width = 121
    Height = 21
    Color = clAqua
    TabOrder = 0
    Text = 'Edit1'
  end
  object Edit2: TEdit
    Left = 8
    Top = 35
    Width = 121
    Height = 21
    Enabled = False
    TabOrder = 1
    Text = 'Edit2'
  end
  object Memo1: TMemo
    Left = 147
    Top = 8
    Width = 394
    Height = 89
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object Memo2: TMemo
    Left = 147
    Top = 103
    Width = 394
    Height = 88
    Color = clYellow
    Lines.Strings = (
      'Memo2')
    ScrollBars = ssBoth
    TabOrder = 3
  end
  object MaskEdit1: TMaskEdit
    Left = 8
    Top = 62
    Width = 121
    Height = 21
    TabOrder = 4
    Text = 'MaskEdit1'
  end
  object LabeledEdit1: TLabeledEdit
    Left = 8
    Top = 127
    Width = 121
    Height = 21
    EditLabel.Width = 61
    EditLabel.Height = 13
    EditLabel.Caption = 'LabeledEdit1'
    TabOrder = 5
  end
  object ButtonedEdit1: TButtonedEdit
    Left = 8
    Top = 205
    Width = 121
    Height = 21
    RightButton.Visible = True
    TabOrder = 6
    Text = 'ButtonedEdit1'
  end
  object ButtonedEdit2: TButtonedEdit
    Left = 8
    Top = 232
    Width = 121
    Height = 21
    Color = clLime
    TabOrder = 7
    Text = 'ButtonedEdit2'
  end
  object LabeledEdit2: TLabeledEdit
    Left = 8
    Top = 170
    Width = 121
    Height = 21
    Color = clMoneyGreen
    EditLabel.Width = 61
    EditLabel.Height = 13
    EditLabel.Caption = 'LabeledEdit2'
    TabOrder = 8
  end
  object MaskEdit2: TMaskEdit
    Left = 8
    Top = 89
    Width = 121
    Height = 21
    Color = clSilver
    TabOrder = 9
    Text = 'MaskEdit2'
  end
  object Button1: TButton
    Left = 8
    Top = 328
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 10
    OnClick = Button1Click
  end
  object DBMemo1: TDBMemo
    Left = 147
    Top = 197
    Width = 382
    Height = 56
    Color = clMoneyGreen
    TabOrder = 11
  end
end
