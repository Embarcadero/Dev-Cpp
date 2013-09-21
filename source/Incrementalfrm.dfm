object frmIncremental: TfrmIncremental
  Left = 862
  Top = 564
  Anchors = []
  AutoSize = True
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Incremental Search'
  ClientHeight = 25
  ClientWidth = 282
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Edit: TEdit
    Left = 0
    Top = 0
    Width = 233
    Height = 24
    Color = clBtnFace
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnChange = EditChange
    OnKeyDown = EditKeyDown
    OnKeyPress = EditKeyPress
  end
  object btnPrev: TButton
    Left = 232
    Top = 0
    Width = 25
    Height = 25
    Caption = '<'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Courier'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    OnClick = btnPrevClick
  end
  object btnNext: TButton
    Left = 257
    Top = 0
    Width = 25
    Height = 25
    Caption = '>'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Courier'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    OnClick = btnNextClick
  end
end
