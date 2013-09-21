object frmShortcutsEditor: TfrmShortcutsEditor
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Configure Shortcuts'
  ClientHeight = 362
  ClientWidth = 375
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
  object lblTitle: TLabel
    Left = 0
    Top = 0
    Width = 375
    Height = 33
    Align = alTop
    AutoSize = False
    Caption = ' Click on an item and press the shortcut you desire!'
    Color = clGray
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clCream
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object lblTip: TLabel
    Left = 4
    Top = 20
    Width = 192
    Height = 13
    Caption = 'Tip: press "Escape" to clear a shortcut...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clSilver
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object lvShortcuts: TListView
    Left = 0
    Top = 33
    Width = 375
    Height = 287
    Align = alClient
    BevelKind = bkFlat
    BorderStyle = bsNone
    Columns = <
      item
        Caption = 'Menu entry'
        Width = 250
      end
      item
        Caption = 'Shortcut assigned'
        Width = 106
      end>
    ColumnClick = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnCustomDrawItem = lvShortcutsCustomDrawItem
    OnCustomDrawSubItem = lvShortcutsCustomDrawSubItem
    OnKeyDown = lvShortcutsKeyDown
  end
  object Panel1: TPanel
    Left = 0
    Top = 320
    Width = 375
    Height = 42
    Align = alBottom
    TabOrder = 1
    object btnOk: TButton
      Left = 212
      Top = 8
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TButton
      Left = 292
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
end
