object CompOptionsFrame: TCompOptionsFrame
  Left = 0
  Top = 0
  Width = 573
  Height = 420
  TabOrder = 0
  OnResize = FrameResize
  object Splitter1: TSplitter
    Left = 121
    Top = 0
    Width = 4
    Height = 420
    Cursor = crHSplit
    OnMoved = FrameResize
  end
  object tv: TTreeView
    Left = 0
    Top = 0
    Width = 121
    Height = 420
    Align = alLeft
    HideSelection = False
    Indent = 19
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    OnChange = tvChange
  end
  object vle: TValueListEditor
    Left = 125
    Top = 0
    Width = 448
    Height = 420
    Align = alClient
    DisplayOptions = []
    FixedCols = 1
    Options = [goColSizing, goEditing, goAlwaysShowEditor, goThumbTracking]
    TabOrder = 1
    TitleCaptions.Strings = (
      'Option'
      'Value')
    OnSetEditText = vleSetEditText
  end
end
