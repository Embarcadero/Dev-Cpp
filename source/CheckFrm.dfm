object CheckForm: TCheckForm
  Left = 255
  Top = 87
  ActiveControl = grpTask
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Check for Dev-C++ update'
  ClientHeight = 346
  ClientWidth = 328
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poOwnerFormCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object grpTask: TGroupBox
    Left = 8
    Top = 8
    Width = 313
    Height = 41
    Caption = 'Task:'
    TabOrder = 0
    object L: TLabel
      Left = 24
      Top = 16
      Width = 3
      Height = 13
    end
  end
  object grpResults: TGroupBox
    Left = 7
    Top = 61
    Width = 313
    Height = 244
    Caption = 'Results:'
    TabOrder = 1
    object lblRelVer: TLabel
      Left = 8
      Top = 24
      Width = 79
      Height = 13
      Caption = 'Release version:'
    end
    object lblNeed: TLabel
      Left = 8
      Top = 40
      Width = 132
      Height = 13
      Caption = 'Needed version to use with:'
    end
    object lblDesc: TLabel
      Left = 8
      Top = 56
      Width = 105
      Height = 13
      Caption = 'Description of release:'
    end
    object Release: TLabel
      Left = 96
      Top = 24
      Width = 5
      Height = 13
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Need_version: TLabel
      Left = 149
      Top = 40
      Width = 5
      Height = 13
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblSites: TLabel
      Left = 8
      Top = 144
      Width = 200
      Height = 13
      Caption = 'Download site (double-click to download) :'
    end
    object Memo: TMemo
      Left = 8
      Top = 72
      Width = 297
      Height = 57
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
    end
    object SiteList: TListBox
      Left = 8
      Top = 160
      Width = 297
      Height = 73
      ItemHeight = 13
      TabOrder = 1
      OnDblClick = SiteListDblClick
    end
  end
  object btnOk: TBitBtn
    Left = 244
    Top = 314
    Width = 75
    Height = 25
    Caption = '&OK'
    TabOrder = 2
    OnClick = btnOkClick
    Kind = bkOK
  end
end
