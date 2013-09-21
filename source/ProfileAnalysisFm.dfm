object ProfileAnalysisForm: TProfileAnalysisForm
  Left = 119
  Top = 97
  Width = 649
  Height = 480
  Caption = 'Profile analysis'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnPaint = FormPaint
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 412
    Width = 641
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      641
      41)
    object btnClose: TButton
      Left = 283
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akBottom]
      Cancel = True
      Caption = 'Close'
      TabOrder = 0
      OnClick = btnCloseClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 641
    Height = 412
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Parsing profiling results - Please wait...'
    TabOrder = 1
    object PageControl1: TPageControl
      Left = 0
      Top = 0
      Width = 641
      Height = 412
      ActivePage = tabFlat
      Align = alClient
      TabIndex = 0
      TabOrder = 0
      OnChange = PageControl1Change
      object tabFlat: TTabSheet
        Caption = 'Flat output'
        object Splitter2: TSplitter
          Left = 0
          Top = 244
          Width = 633
          Height = 8
          Cursor = crVSplit
          Align = alBottom
        end
        object memFlat: TMemo
          Left = 0
          Top = 252
          Width = 633
          Height = 132
          Align = alBottom
          Color = clBtnFace
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          ReadOnly = True
          ScrollBars = ssBoth
          TabOrder = 0
        end
        object lvFlat: TListView
          Left = 0
          Top = 0
          Width = 633
          Height = 244
          Align = alClient
          Columns = <
            item
              Caption = 'Function name'
              Width = 200
            end
            item
              Alignment = taRightJustify
              Caption = '% time'
            end
            item
              Alignment = taRightJustify
              Caption = 'Cumul. secs'
              Width = 75
            end
            item
              Alignment = taRightJustify
              Caption = 'Self secs'
              Width = 75
            end
            item
              Alignment = taRightJustify
              Caption = 'Calls'
              Width = 60
            end
            item
              Alignment = taRightJustify
              Caption = 'Self ts/call'
              Width = 75
            end
            item
              Alignment = taRightJustify
              Caption = 'Total ts/call'
              Width = 75
            end>
          ReadOnly = True
          TabOrder = 1
          ViewStyle = vsReport
          OnClick = lvFlatClick
          OnCustomDrawItem = lvFlatCustomDrawItem
          OnMouseMove = lvFlatMouseMove
        end
      end
      object tabGraph: TTabSheet
        Caption = 'Call graph'
        ImageIndex = 1
        object Splitter1: TSplitter
          Left = 0
          Top = 244
          Width = 633
          Height = 8
          Cursor = crVSplit
          Align = alBottom
        end
        object memGraph: TMemo
          Left = 0
          Top = 252
          Width = 633
          Height = 132
          Align = alBottom
          Color = clBtnFace
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          ReadOnly = True
          ScrollBars = ssBoth
          TabOrder = 0
        end
        object lvGraph: TListView
          Left = 0
          Top = 0
          Width = 633
          Height = 244
          Align = alClient
          Columns = <
            item
              Caption = 'Function name'
              Width = 250
            end
            item
              Alignment = taRightJustify
              Caption = 'Index'
            end
            item
              Alignment = taRightJustify
              Caption = '% time'
              Width = 75
            end
            item
              Alignment = taRightJustify
              Caption = 'Self'
              Width = 75
            end
            item
              Alignment = taRightJustify
              Caption = 'Children'
              Width = 75
            end
            item
              Alignment = taRightJustify
              Caption = 'Called'
              Width = 75
            end>
          ReadOnly = True
          TabOrder = 1
          ViewStyle = vsReport
          OnClick = lvFlatClick
          OnCustomDrawItem = lvGraphCustomDrawItem
          OnMouseMove = lvFlatMouseMove
        end
      end
    end
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
