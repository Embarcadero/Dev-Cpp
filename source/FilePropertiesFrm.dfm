object FilePropertiesForm: TFilePropertiesForm
  Left = 626
  Top = 341
  BorderStyle = bsDialog
  Caption = 'File properties'
  ClientHeight = 265
  ClientWidth = 409
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
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object Bevel7: TBevel
    Left = 8
    Top = 8
    Width = 393
    Height = 213
    Style = bsRaised
  end
  object Bevel8: TBevel
    Left = 208
    Top = 168
    Width = 185
    Height = 21
  end
  object Bevel2: TBevel
    Left = 208
    Top = 144
    Width = 185
    Height = 21
  end
  object Bevel3: TBevel
    Left = 16
    Top = 120
    Width = 185
    Height = 21
  end
  object Bevel4: TBevel
    Left = 16
    Top = 144
    Width = 185
    Height = 21
  end
  object Bevel6: TBevel
    Left = 208
    Top = 120
    Width = 185
    Height = 21
  end
  object Bevel5: TBevel
    Left = 16
    Top = 168
    Width = 185
    Height = 21
  end
  object Bevel1: TBevel
    Left = 16
    Top = 16
    Width = 377
    Height = 97
  end
  object Label1: TLabel
    Left = 24
    Top = 24
    Width = 51
    Height = 15
    Caption = 'Filename:'
  end
  object Label2: TLabel
    Left = 24
    Top = 48
    Width = 53
    Height = 15
    Caption = 'In project:'
  end
  object Label3: TLabel
    Left = 20
    Top = 124
    Width = 89
    Height = 15
    Caption = 'Total lines in file:'
  end
  object Label4: TLabel
    Left = 20
    Top = 148
    Width = 107
    Height = 15
    Caption = 'Actual lines of code:'
  end
  object Label5: TLabel
    Left = 20
    Top = 172
    Width = 84
    Height = 15
    Caption = 'Comment lines:'
  end
  object Label6: TLabel
    Left = 212
    Top = 148
    Width = 43
    Height = 15
    Caption = 'File size:'
  end
  object Label7: TLabel
    Left = 212
    Top = 124
    Width = 64
    Height = 15
    Caption = 'Empty lines:'
  end
  object lblSize: TLabel
    Left = 381
    Top = 148
    Width = 5
    Height = 13
    Alignment = taRightJustify
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblTotal: TLabel
    Left = 189
    Top = 124
    Width = 5
    Height = 13
    Alignment = taRightJustify
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblCode: TLabel
    Left = 190
    Top = 148
    Width = 5
    Height = 13
    Alignment = taRightJustify
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblComments: TLabel
    Left = 190
    Top = 172
    Width = 5
    Height = 13
    Alignment = taRightJustify
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblEmpty: TLabel
    Left = 382
    Top = 124
    Width = 5
    Height = 13
    Alignment = taRightJustify
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label8: TLabel
    Left = 212
    Top = 172
    Width = 73
    Height = 15
    Caption = 'Included files:'
  end
  object lblIncludes: TLabel
    Left = 381
    Top = 172
    Width = 5
    Height = 13
    Alignment = taRightJustify
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label9: TLabel
    Left = 24
    Top = 72
    Width = 50
    Height = 15
    Caption = 'Absolute:'
  end
  object Label10: TLabel
    Left = 24
    Top = 96
    Width = 44
    Height = 15
    Caption = 'Relative:'
  end
  object Bevel9: TBevel
    Left = 16
    Top = 192
    Width = 377
    Height = 21
  end
  object Label11: TLabel
    Left = 20
    Top = 196
    Width = 63
    Height = 15
    Caption = 'Timestamp:'
  end
  object lblTimestamp: TLabel
    Left = 381
    Top = 196
    Width = 5
    Height = 13
    Alignment = taRightJustify
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblProject: TEdit
    Left = 100
    Top = 44
    Width = 288
    Height = 20
    AutoSize = False
    ReadOnly = True
    TabOrder = 4
  end
  object lblAbsolute: TEdit
    Left = 100
    Top = 68
    Width = 288
    Height = 20
    AutoSize = False
    ReadOnly = True
    TabOrder = 2
  end
  object lblRelative: TEdit
    Left = 100
    Top = 92
    Width = 288
    Height = 20
    AutoSize = False
    ReadOnly = True
    TabOrder = 3
  end
  object btnOK: TButton
    Left = 167
    Top = 232
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = btnOKClick
  end
  object cmbFiles: TComboBox
    Left = 100
    Top = 20
    Width = 289
    Height = 23
    Style = csDropDownList
    ItemHeight = 15
    TabOrder = 0
    OnClick = cmbFilesClick
  end
end
