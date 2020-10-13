object WindowListForm: TWindowListForm
  Left = 549
  Top = 446
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Window List'
  ClientHeight = 399
  ClientWidth = 600
  Color = clWindow
  Ctl3D = False
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel: TPanel
    Left = 0
    Top = 364
    Width = 600
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      600
      35)
    object OkBtn: TBitBtn
      Left = 412
      Top = 4
      Width = 91
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&OK'
      ModalResult = 1
      NumGlyphs = 2
      TabOrder = 0
    end
    object CancelBtn: TBitBtn
      Left = 503
      Top = 4
      Width = 91
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Cancel'
      ModalResult = 2
      NumGlyphs = 2
      TabOrder = 1
    end
  end
  object UnitList: TListView
    Left = 8
    Top = 5
    Width = 585
    Height = 358
    Align = alCustom
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Name'
        Width = -1
        WidthType = (
          -1)
      end
      item
        Caption = 'Path'
        Width = -1
        WidthType = (
          -1)
      end>
    ReadOnly = True
    RowSelect = True
    SortType = stText
    TabOrder = 1
    ViewStyle = vsReport
    OnColumnClick = UnitListColumnClick
    OnCompare = UnitListCompare
    OnDblClick = UnitListDblClick
    OnKeyDown = UnitListKeyDown
  end
end
