object frmShortcutsEditor: TfrmShortcutsEditor
  Left = 473
  Top = 271
  BorderStyle = bsDialog
  Caption = 'Configure Shortcuts'
  ClientHeight = 470
  ClientWidth = 380
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lvShortcuts: TListView
    Left = 0
    Top = 0
    Width = 380
    Height = 433
    BevelKind = bkFlat
    BorderStyle = bsNone
    Columns = <
      item
        Caption = 'Menu entry'
        Width = 255
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
    Left = 290
    Top = 440
    Width = 85
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object btnOk: TButton
    Left = 200
    Top = 440
    Width = 85
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object btnDefault: TButton
    Left = 8
    Top = 440
    Width = 85
    Height = 25
    Caption = 'Defaults'
    TabOrder = 3
    OnClick = btnDefaultClick
  end
end
