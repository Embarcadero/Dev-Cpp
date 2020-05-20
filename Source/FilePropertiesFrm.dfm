object FilePropertiesForm: TFilePropertiesForm
  Left = 446
  Top = 320
  BorderStyle = bsDialog
  Caption = 'File properties'
  ClientHeight = 333
  ClientWidth = 436
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  PopupMenu = PropertiesPop
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object Bevel7: TBevel
    Left = 8
    Top = 8
    Width = 420
    Height = 289
    Style = bsRaised
  end
  object Bevel8: TBevel
    Left = 220
    Top = 220
    Width = 200
    Height = 30
  end
  object Bevel2: TBevel
    Left = 220
    Top = 182
    Width = 200
    Height = 30
  end
  object Bevel4: TBevel
    Left = 16
    Top = 182
    Width = 200
    Height = 30
  end
  object Bevel6: TBevel
    Left = 220
    Top = 144
    Width = 200
    Height = 30
  end
  object Bevel5: TBevel
    Left = 16
    Top = 220
    Width = 200
    Height = 30
  end
  object Bevel1: TBevel
    Left = 16
    Top = 16
    Width = 404
    Height = 121
  end
  object lblFileName: TLabel
    Left = 24
    Top = 24
    Width = 51
    Height = 15
    Caption = 'Filename:'
  end
  object lblProject: TLabel
    Left = 24
    Top = 54
    Width = 53
    Height = 15
    Caption = 'In project:'
  end
  object lblTotalLines: TLabel
    Left = 20
    Top = 150
    Width = 87
    Height = 15
    Caption = 'Total lines in file:'
  end
  object lblCodeLines: TLabel
    Left = 20
    Top = 188
    Width = 107
    Height = 15
    Caption = 'Actual lines of code:'
  end
  object lblCommentLines: TLabel
    Left = 20
    Top = 226
    Width = 84
    Height = 15
    Caption = 'Comment lines:'
  end
  object lblFileSize: TLabel
    Left = 228
    Top = 188
    Width = 43
    Height = 15
    Caption = 'File size:'
  end
  object lblEmptyLines: TLabel
    Left = 228
    Top = 150
    Width = 64
    Height = 15
    Caption = 'Empty lines:'
  end
  object lblIncludes: TLabel
    Left = 228
    Top = 226
    Width = 73
    Height = 15
    Caption = 'Included files:'
  end
  object lblAbsolute: TLabel
    Left = 24
    Top = 82
    Width = 50
    Height = 15
    Caption = 'Absolute:'
  end
  object lblRelative: TLabel
    Left = 24
    Top = 110
    Width = 44
    Height = 15
    Caption = 'Relative:'
  end
  object Bevel9: TBevel
    Left = 16
    Top = 258
    Width = 401
    Height = 30
  end
  object lblTimeStamp: TLabel
    Left = 20
    Top = 264
    Width = 62
    Height = 15
    Caption = 'Timestamp:'
  end
  object Bevel3: TBevel
    Left = 16
    Top = 144
    Width = 200
    Height = 30
  end
  object edFileSize: TEdit
    Left = 350
    Top = 186
    Width = 65
    Height = 23
    ReadOnly = True
    TabOrder = 8
  end
  object edTotalLines: TEdit
    Left = 146
    Top = 148
    Width = 65
    Height = 23
    ReadOnly = True
    TabOrder = 4
  end
  object edCodeLines: TEdit
    Left = 146
    Top = 186
    Width = 65
    Height = 23
    ReadOnly = True
    TabOrder = 5
  end
  object edCommentLines: TEdit
    Left = 146
    Top = 224
    Width = 65
    Height = 23
    ReadOnly = True
    TabOrder = 6
  end
  object edEmptyLines: TEdit
    Left = 350
    Top = 148
    Width = 65
    Height = 23
    ReadOnly = True
    TabOrder = 7
  end
  object edIncludes: TEdit
    Left = 350
    Top = 224
    Width = 65
    Height = 23
    ReadOnly = True
    TabOrder = 9
  end
  object edTimestamp: TEdit
    Left = 176
    Top = 262
    Width = 238
    Height = 23
    ReadOnly = True
    TabOrder = 10
  end
  object edProject: TEdit
    Left = 100
    Top = 50
    Width = 316
    Height = 23
    AutoSize = False
    ReadOnly = True
    TabOrder = 1
  end
  object edAbsolute: TEdit
    Left = 100
    Top = 78
    Width = 316
    Height = 23
    AutoSize = False
    ReadOnly = True
    TabOrder = 2
  end
  object edRelative: TEdit
    Left = 100
    Top = 106
    Width = 316
    Height = 23
    AutoSize = False
    ReadOnly = True
    TabOrder = 3
  end
  object btnOK: TButton
    Left = 181
    Top = 304
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'OK'
    Default = True
    TabOrder = 11
    OnClick = btnOKClick
  end
  object cmbFiles: TComboBox
    Left = 100
    Top = 22
    Width = 316
    Height = 23
    Style = csDropDownList
    TabOrder = 0
    OnClick = cmbFilesClick
  end
  object PropertiesPop: TPopupMenu
    Left = 400
    Top = 304
    object PropertiesCopy: TMenuItem
      Caption = 'Copy'
      ShortCut = 16451
      OnClick = PropertiesCopyClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object PropertiesSelAll: TMenuItem
      Caption = 'Select All'
      ShortCut = 16449
      OnClick = PropertiesSelAllClick
    end
  end
end
