object FileForm: TFileForm
  Left = 331
  Top = 183
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Add file or directory'
  ClientHeight = 158
  ClientWidth = 216
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
    Width = 216
    Height = 158
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object lblSource: TLabel
      Left = 8
      Top = 10
      Width = 40
      Height = 13
      Caption = 'Source :'
    end
    object lblDest: TLabel
      Left = 8
      Top = 56
      Width = 201
      Height = 26
      Caption = 
        'Destination (if destination is a directory, be sure to put a \ a' +
        't the end) :'
      WordWrap = True
    end
    object LoadBtn: TSpeedButton
      Left = 184
      Top = 26
      Width = 23
      Height = 22
      Flat = True
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000120B0000120B00000000000000000000BFBFBFBFBFBF
        BFBFBFBFBFBFBFBFBF000000BFBFBFBFBFBFBFBFBFBFBFBF0000000000000000
        00000000000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF000000BF
        BFBF000000BFBFBF0000005DCCFF5DCCFF5DCCFF000000BFBFBFBFBFBFBFBFBF
        BFBFBFBFBFBFBFBFBF000000BFBFBFBFBFBFBFBFBFBFBFBF6868680000000000
        00000000000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
        BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
        BFBFBFBFBFBFBFBFBF000000BFBFBFBFBFBFBFBFBFBFBFBF0000000000000000
        00000000000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF000000BF
        BFBF000000BFBFBF0000005DCCFF5DCCFF5DCCFF000000BFBFBFBFBFBFBFBFBF
        BFBFBFBFBFBFBFBFBF000000BFBFBFBFBFBFBFBFBFBFBFBF6868680000000000
        00000000000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
        BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF000000000000
        000000000000000000000000000000000000000000000000000000BFBFBFBFBF
        BFBFBFBFBFBFBFBFBFBF00000000AEFF0096DB0096DB0096DB0096DB0096DB00
        96DB0096DB0082BE000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBF0000005DCCFF
        00AEFF00AEFF00AEFF00AEFF00AEFF00AEFF00AEFF0096DB000000BFBFBFBFBF
        BFBFBFBFBFBFBFBFBFBF0000005DCCFF00AEFF00AEFF00AEFF00AEFF00AEFF00
        AEFF00AEFF0096DB000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBF0000005DCCFF
        00AEFF00AEFF00AEFF00AEFF00AEFF00AEFF00AEFF0096DB000000BFBFBFBFBF
        BFBFBFBFBFBFBFBFBFBF0000005DCCFF00AEFF00AEFF5DCCFF5DCCFF5DCCFF5D
        CCFF5DCCFF00AEFF000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBF686868BDEBFF
        5DCCFF5DCCFF000000000000000000000000000000000000BFBFBFBFBFBFBFBF
        BFBFBFBFBFBFBFBFBFBFBFBFBF000000000000000000BFBFBFBFBFBFBFBFBFBF
        BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF}
      OnClick = LoadBtnClick
    end
    object edSource: TEdit
      Left = 8
      Top = 26
      Width = 172
      Height = 21
      TabOrder = 0
    end
    object OkBtn: TBitBtn
      Left = 56
      Top = 124
      Width = 75
      Height = 25
      TabOrder = 1
      Kind = bkOK
    end
    object CancelBtn: TBitBtn
      Left = 136
      Top = 124
      Width = 75
      Height = 25
      TabOrder = 2
      Kind = bkCancel
    end
    object edDest: TComboBox
      Left = 8
      Top = 88
      Width = 201
      Height = 21
      ItemHeight = 13
      TabOrder = 3
      Text = '<app>\'
      Items.Strings = (
        '<app>\'
        '<src>\'
        '<win>\'
        '<sys>\')
    end
  end
  object OpenDialog: TOpenDialog
    Filter = 'All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Title = 'Select file :'
    Left = 80
    Top = 8
  end
end
