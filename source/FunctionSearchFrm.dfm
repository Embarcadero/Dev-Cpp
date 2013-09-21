object FunctionSearchForm: TFunctionSearchForm
  Left = 694
  Top = 563
  BorderStyle = bsDialog
  Caption = 'Goto function...'
  ClientHeight = 377
  ClientWidth = 469
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    469
    377)
  PixelsPerInch = 96
  TextHeight = 15
  object lblSearch: TLabel
    Left = 4
    Top = 8
    Width = 56
    Height = 15
    Caption = 'Search for:'
  end
  object lvEntries: TListView
    Left = 0
    Top = 32
    Width = 469
    Height = 345
    Align = alCustom
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelInner = bvNone
    BevelOuter = bvNone
    Columns = <
      item
        Width = 30
      end
      item
        Caption = 'Type'
      end
      item
        Caption = 'Function'
        Width = 300
      end
      item
        Caption = 'Line'
      end>
    ColumnClick = False
    HideSelection = False
    LargeImages = dmMain.ClassImages
    ReadOnly = True
    RowSelect = True
    SmallImages = dmMain.ClassImages
    StateImages = dmMain.ClassImages
    TabOrder = 0
    TabStop = False
    ViewStyle = vsReport
    OnCompare = lvEntriesCompare
    OnDblClick = lvEntriesDblClick
  end
  object txtSearch: TEdit
    Left = 84
    Top = 4
    Width = 379
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnChange = txtSearchChange
    OnKeyDown = txtSearchKeyDown
    OnKeyPress = txtSearchKeyPress
  end
end
