object CompOptionsFrame: TCompOptionsFrame
  Left = 0
  Top = 0
  Width = 573
  Height = 520
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  TabOrder = 0
  object tabs: TTabControl
    Left = 0
    Top = 0
    Width = 573
    Height = 520
    Align = alClient
    TabOrder = 0
    OnChange = tabsChange
    object vle: TCompOptionsList
      Left = 4
      Top = 6
      Width = 565
      Height = 510
      Align = alClient
      BorderStyle = bsNone
      DefaultRowHeight = 22
      DisplayOptions = [doKeyColFixed]
      DropDownRows = 40
      FixedCols = 1
      Options = [goEditing, goAlwaysShowEditor]
      ScrollBars = ssNone
      TabOrder = 0
      OnSetEditText = vleSetEditText
      ColWidths = (
        199
        364)
    end
  end
end
