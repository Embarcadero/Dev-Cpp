object frmShortcutsEditor: TfrmShortcutsEditor
  Left = 1163
  Top = 384
  BorderStyle = bsDialog
  Caption = 'Configure Shortcuts'
  ClientHeight = 470
  ClientWidth = 375
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 15
  object lvShortcuts: TListView
    Left = 0
    Top = 0
    Width = 375
    Height = 433
    BevelKind = bkFlat
    BorderStyle = bsNone
    Columns = <
      item
        Caption = 'Menu entry'
        Width = 250
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
    Left = 292
    Top = 440
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object btnOk: TButton
    Left = 204
    Top = 440
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
end
