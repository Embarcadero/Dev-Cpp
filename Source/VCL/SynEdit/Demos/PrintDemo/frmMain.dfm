object PrintDemoForm: TPrintDemoForm
  Left = 291
  Top = 210
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'SynEdit printing demo'
  ClientHeight = 312
  ClientWidth = 542
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 12
    Top = 16
    Width = 54
    Height = 13
    Caption = 'File to print:'
  end
  object SpeedButton1: TSpeedButton
    Left = 516
    Top = 12
    Width = 21
    Height = 21
    Caption = '...'
    OnClick = SpeedButton1Click
  end
  object Label2: TLabel
    Left = 12
    Top = 48
    Width = 38
    Height = 13
    Caption = 'Header:'
  end
  object Label3: TLabel
    Left = 12
    Top = 132
    Width = 33
    Height = 13
    Caption = 'Footer:'
  end
  object Label4: TLabel
    Left = 12
    Top = 216
    Width = 23
    Height = 13
    Caption = 'Title:'
  end
  object Label5: TLabel
    Left = 312
    Top = 208
    Width = 53
    Height = 29
    AutoSize = False
    Caption = 'Print date and time:'
    WordWrap = True
  end
  object eFileName: TEdit
    Left = 88
    Top = 12
    Width = 429
    Height = 21
    TabOrder = 0
    OnChange = eFileNameChange
  end
  object memoHeader: TMemo
    Left = 88
    Top = 44
    Width = 449
    Height = 73
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      '$TITLE$$RIGHT$Printed $DATETIME$'
      '$LINE$')
    ParentFont = False
    TabOrder = 1
  end
  object memoFooter: TMemo
    Left = 88
    Top = 128
    Width = 449
    Height = 73
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      '$LINE$'
      '$CENTER$Page: $PAGENUM$/$PAGECOUNT$')
    ParentFont = False
    TabOrder = 2
  end
  object btnPrint: TButton
    Left = 462
    Top = 278
    Width = 75
    Height = 24
    Caption = 'Print'
    Enabled = False
    TabOrder = 10
    OnClick = btnPrintClick
  end
  object btnHeaderFont: TButton
    Left = 9
    Top = 278
    Width = 120
    Height = 24
    Caption = 'Header font...'
    TabOrder = 9
    OnClick = btnHeaderFontClick
  end
  object cbUseHighlighter: TCheckBox
    Left = 180
    Top = 252
    Width = 129
    Height = 17
    Caption = 'Use syntax highlighter'
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
  object cbPrintBlackAndWhite: TCheckBox
    Left = 12
    Top = 252
    Width = 157
    Height = 17
    Caption = 'Print in black and white only'
    TabOrder = 5
  end
  object cbPrintLineNumbers: TCheckBox
    Left = 320
    Top = 252
    Width = 109
    Height = 17
    Caption = 'Print line numbers'
    TabOrder = 7
  end
  object cbWordWrap: TCheckBox
    Left = 440
    Top = 252
    Width = 97
    Height = 17
    Caption = 'Wrap long lines'
    TabOrder = 8
  end
  object eTitle: TEdit
    Left = 88
    Top = 212
    Width = 201
    Height = 24
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    Text = 'SynEdit print demo'
  end
  object ePrintDateTime: TEdit
    Left = 368
    Top = 212
    Width = 169
    Height = 24
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
  end
  object btnFooterFont: TButton
    Left = 137
    Top = 278
    Width = 120
    Height = 24
    Caption = 'Footer font...'
    TabOrder = 11
    OnClick = btnFooterFontClick
  end
  object dlgFileOpen: TOpenDialog
    DefaultExt = 'pas'
    Title = 'File to print'
    Left = 388
    Top = 52
  end
  object SynPasSyn1: TSynPasSyn
    Left = 312
    Top = 76
  end
  object dlgFilePrint: TPrintDialog
    Options = [poPageNums]
    Left = 424
    Top = 52
  end
  object dlgSelectFont: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 460
    Top = 52
  end
end
