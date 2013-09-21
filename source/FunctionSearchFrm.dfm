object FunctionSearchForm: TFunctionSearchForm
  Left = 717
  Top = 386
  BorderStyle = bsDialog
  Caption = 'Goto function...'
  ClientHeight = 332
  ClientWidth = 451
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
  PixelsPerInch = 96
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 451
    Height = 29
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      451
      29)
    object Label1: TLabel
      Left = 4
      Top = 8
      Width = 56
      Height = 15
      Caption = 'Search for:'
    end
    object txtSearch: TEdit
      Left = 84
      Top = 4
      Width = 361
      Height = 23
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = txtSearchChange
      OnExit = txtSearchExit
      OnKeyDown = txtSearchKeyDown
    end
  end
  object lvEntries: TListView
    Left = 0
    Top = 29
    Width = 451
    Height = 303
    Align = alClient
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
    HideSelection = False
    LargeImages = dmMain.ClassImages
    ReadOnly = True
    RowSelect = True
    SmallImages = dmMain.ClassImages
    StateImages = dmMain.ClassImages
    TabOrder = 1
    TabStop = False
    ViewStyle = vsReport
    OnCompare = lvEntriesCompare
    OnDblClick = lvEntriesDblClick
  end
end
