object frmIncremental: TfrmIncremental
  Left = 278
  Top = 241
  Anchors = []
  AutoSize = True
  BorderStyle = bsNone
  Caption = 'Incremental Search'
  ClientHeight = 24
  ClientWidth = 201
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Edit: TEdit
    Left = 0
    Top = 0
    Width = 201
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
  object ActionList1: TActionList
    Left = 123
    Top = 3
    object SearchAgain: TAction
      Caption = 'SearchAgain'
      ShortCut = 16460
      OnExecute = SearchAgainExecute
    end
  end
end
