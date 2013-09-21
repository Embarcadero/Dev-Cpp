object PrintForm: TPrintForm
  Left = 623
  Top = 463
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Print file'
  ClientHeight = 194
  ClientWidth = 432
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object btnCancel: TBitBtn
    Left = 348
    Top = 164
    Width = 75
    Height = 24
    Caption = '&Cancel'
    TabOrder = 1
    Kind = bkCancel
  end
  object btnOk: TBitBtn
    Left = 270
    Top = 164
    Width = 75
    Height = 24
    Caption = '&OK'
    TabOrder = 0
    Kind = bkOK
  end
  object grpParams: TGroupBox
    Left = 8
    Top = 8
    Width = 417
    Height = 76
    Caption = 'Parameters : '
    TabOrder = 2
    object cbColors: TCheckBox
      Left = 8
      Top = 16
      Width = 145
      Height = 17
      Caption = '&Colors'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object cbHighlight: TCheckBox
      Left = 8
      Top = 32
      Width = 153
      Height = 17
      Caption = '&Highlight'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object rbLN: TRadioButton
      Left = 180
      Top = 16
      Width = 200
      Height = 17
      Caption = 'Print line numbers'
      Checked = True
      TabOrder = 2
      TabStop = True
    end
    object rbLNMargin: TRadioButton
      Left = 180
      Top = 32
      Width = 229
      Height = 17
      Caption = 'Print line numbers in margin'
      TabOrder = 3
    end
    object cbWordWrap: TCheckBox
      Left = 8
      Top = 48
      Width = 161
      Height = 17
      Caption = '&Word wrap'
      TabOrder = 4
    end
    object rbNoLN: TRadioButton
      Left = 180
      Top = 48
      Width = 200
      Height = 17
      Caption = 'Don'#39't print line numbers'
      TabOrder = 5
    end
  end
  object grpPages: TGroupBox
    Left = 8
    Top = 85
    Width = 417
    Height = 76
    Caption = 'Pages :'
    TabOrder = 3
    object lblCopies: TLabel
      Left = 8
      Top = 20
      Width = 101
      Height = 15
      Caption = 'Number of copies :'
    end
    object seCopies: TSpinEdit
      Left = 16
      Top = 40
      Width = 75
      Height = 24
      MaxValue = 10000000
      MinValue = 1
      TabOrder = 0
      Value = 1
    end
    object cbSelection: TCheckBox
      Left = 152
      Top = 20
      Width = 200
      Height = 17
      Caption = 'Print &selection only'
      TabOrder = 1
    end
  end
end
