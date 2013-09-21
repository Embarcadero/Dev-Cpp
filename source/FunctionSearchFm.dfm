object FunctionSearchForm: TFunctionSearchForm
  Left = 196
  Top = 109
  BorderStyle = bsDialog
  Caption = 'Goto function...'
  ClientHeight = 329
  ClientWidth = 451
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
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
      Width = 52
      Height = 13
      Caption = 'Search for:'
    end
    object txtSearch: TEdit
      Left = 84
      Top = 4
      Width = 361
      Height = 21
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
    Height = 300
    Align = alClient
    Columns = <
      item
        Width = 32
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
  object XPMenu: TXPMenu
    DimLevel = 30
    GrayLevel = 10
    Font.Charset = ANSI_CHARSET
    Font.Color = clMenuText
    Font.Height = -11
    Font.Name = 'Microsoft Sans Serif'
    Font.Style = []
    Color = clBtnFace
    DrawMenuBar = False
    IconBackColor = clBtnFace
    MenuBarColor = clBtnFace
    SelectColor = clHighlight
    SelectBorderColor = clHighlight
    SelectFontColor = clMenuText
    DisabledColor = clInactiveCaption
    SeparatorColor = clBtnFace
    CheckedColor = clHighlight
    IconWidth = 24
    DrawSelect = True
    UseSystemColors = True
    UseDimColor = False
    OverrideOwnerDraw = False
    Gradient = False
    FlatMenu = False
    AutoDetect = True
    Active = False
    Left = 136
    Top = 56
  end
end
