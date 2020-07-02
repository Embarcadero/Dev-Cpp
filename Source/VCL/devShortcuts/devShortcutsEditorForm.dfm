object frmShortcutsEditor: TfrmShortcutsEditor
  Left = 1468
  Top = 423
  BorderStyle = bsDialog
  Caption = 'Configure Shortcuts'
  ClientHeight = 520
  ClientWidth = 470
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblTip: TLabel
    Left = 10
    Top = 454
    Width = 450
    Height = 26
    AutoSize = False
    Caption = 
      'Click on an item and press the shortcut you desire. Press Esc or' +
      ' Delete to clear the shortcut. Please avoid the use of Alt in sh' +
      'ortcuts.'
    WordWrap = True
  end
  object lvShortcuts: TListView
    Left = 8
    Top = 8
    Width = 454
    Height = 440
    Columns = <
      item
        Caption = 'Menu entry'
        Width = 325
      end
      item
        Caption = 'Shortcut assigned'
        Width = 108
      end>
    ColumnClick = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnCustomDrawItem = lvShortcutsCustomDrawItem
    OnExit = lvShortcutsExit
    OnKeyDown = lvShortcutsKeyDown
  end
  object btnCancel: TButton
    Left = 380
    Top = 490
    Width = 85
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object btnOk: TButton
    Left = 290
    Top = 490
    Width = 85
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object btnResetAll: TButton
    Left = 8
    Top = 490
    Width = 85
    Height = 25
    Caption = 'Reset All'
    TabOrder = 3
    OnClick = btnResetAllClick
  end
  object btnResetCurrent: TButton
    Left = 98
    Top = 490
    Width = 85
    Height = 25
    Caption = 'Reset Current'
    TabOrder = 4
    OnClick = btnResetCurrentClick
  end
end
