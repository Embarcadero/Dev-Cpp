object ViewToDoForm: TViewToDoForm
  Left = 486
  Top = 308
  BorderStyle = bsSizeToolWin
  Caption = 'To-Do list'
  ClientHeight = 243
  ClientWidth = 588
  Color = clWindow
  Constraints.MinHeight = 136
  Constraints.MinWidth = 394
  Ctl3D = False
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    588
    243)
  PixelsPerInch = 96
  TextHeight = 15
  object lblFilter: TLabel
    Left = 8
    Top = 199
    Width = 29
    Height = 15
    Anchors = [akLeft, akBottom]
    Caption = 'Filter:'
  end
  object lv: TListView
    Left = 0
    Top = 0
    Width = 589
    Height = 180
    Anchors = [akLeft, akTop, akRight, akBottom]
    Checkboxes = True
    Columns = <
      item
        Caption = 'Done'
        Width = 42
      end
      item
        Caption = 'Priority'
        Width = 46
      end
      item
        Caption = 'Description'
        Width = 263
      end
      item
        Caption = 'Filename'
        Width = 144
      end
      item
        Caption = 'User'
        Width = 90
      end>
    ReadOnly = True
    RowSelect = True
    SortType = stBoth
    TabOrder = 0
    ViewStyle = vsReport
    OnColumnClick = lvColumnClick
    OnCompare = lvCompare
    OnCustomDrawItem = lvCustomDrawItem
    OnCustomDrawSubItem = lvCustomDrawSubItem
    OnDblClick = lvDblClick
    OnMouseDown = lvMouseDown
  end
  object btnClose: TButton
    Left = 502
    Top = 211
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    TabOrder = 1
    OnClick = btnCloseClick
  end
  object chkNoDone: TCheckBox
    Left = 8
    Top = 227
    Width = 289
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Don'#39't show items marked as done'
    TabOrder = 2
    OnClick = chkNoDoneClick
  end
  object cmbFilter: TComboBox
    Left = 52
    Top = 195
    Width = 245
    Height = 23
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    TabOrder = 3
    OnChange = cmbFilterChange
    Items.Strings = (
      'All files (in project and not)'
      'Open files only (in project and not)'
      'All project files'
      'Open project files only'
      'Non-project open files'
      'Current file only')
  end
end
