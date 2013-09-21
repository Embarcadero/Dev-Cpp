object ProfileAnalysisForm: TProfileAnalysisForm
  Left = 992
  Top = 474
  Width = 649
  Height = 531
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
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object MainPanel: TPanel
    Left = 0
    Top = 0
    Width = 633
    Height = 493
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Parsing profiling results - Please wait...'
    TabOrder = 0
    object ProfilePageControl: TPageControl
      Left = 0
      Top = 0
      Width = 633
      Height = 493
      ActivePage = tabFlat
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnChange = ProfilePageControlChange
      object tabFlat: TTabSheet
        Caption = 'Flat output'
        object Splitter2: TSplitter
          Left = 0
          Top = 323
          Width = 625
          Height = 8
          Cursor = crVSplit
          Align = alBottom
        end
        object memFlat: TMemo
          Left = 0
          Top = 331
          Width = 625
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
          Width = 625
          Height = 323
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
          GridLines = True
          ReadOnly = True
          RowSelect = True
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          ViewStyle = vsReport
          OnAdvancedCustomDraw = lvFlatAdvancedCustomDraw
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
          Top = 323
          Width = 625
          Height = 8
          Cursor = crVSplit
          Align = alBottom
        end
        object memGraph: TMemo
          Left = 0
          Top = 331
          Width = 625
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
          Width = 625
          Height = 323
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
          GridLines = True
          ReadOnly = True
          RowSelect = True
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          ViewStyle = vsReport
          OnAdvancedCustomDraw = lvGraphAdvancedCustomDraw
          OnClick = lvFlatClick
          OnCustomDrawItem = lvGraphCustomDrawItem
          OnMouseMove = lvFlatMouseMove
        end
      end
      object tabOpts: TTabSheet
        Caption = 'Profiling Options'
        ImageIndex = 2
        object FuncHiding: TGroupBox
          Left = 8
          Top = 16
          Width = 281
          Height = 105
          Caption = ' Function Hiding '
          TabOrder = 0
          object PrevText: TLabel
            Left = 16
            Top = 78
            Width = 176
            Height = 15
            Caption = 'Supress functions called less than'
          end
          object PostText: TLabel
            Left = 240
            Top = 78
            Width = 29
            Height = 15
            Caption = 'times'
          end
          object chkHideNotCalled: TCheckBox
            Left = 16
            Top = 24
            Width = 257
            Height = 17
            Caption = 'Hide functions not called long enough'
            Checked = True
            State = cbChecked
            TabOrder = 0
            OnClick = commandUpdate
          end
          object chkSuppressStatic: TCheckBox
            Left = 16
            Top = 48
            Width = 257
            Height = 17
            Caption = 'Suppress statically declared (private) functions'
            TabOrder = 1
            OnClick = commandUpdate
          end
          object spnMinCount: TSpinEdit
            Left = 184
            Top = 75
            Width = 49
            Height = 24
            MaxValue = 999999999
            MinValue = 0
            TabOrder = 2
            Value = 1
            OnChange = commandUpdate
          end
        end
        object btnApply: TButton
          Left = 208
          Top = 128
          Width = 75
          Height = 25
          Caption = 'Apply'
          TabOrder = 1
          OnClick = btnApplyClick
        end
        object CustomCommands: TGroupBox
          Left = 296
          Top = 16
          Width = 321
          Height = 81
          Caption = ' Custom Commands '
          TabOrder = 2
          object chkCustom: TCheckBox
            Left = 16
            Top = 24
            Width = 249
            Height = 17
            Caption = 'Use these commands instead:'
            TabOrder = 0
            OnClick = chkCustomClick
          end
          object editCustom: TEdit
            Left = 16
            Top = 48
            Width = 289
            Height = 23
            Enabled = False
            TabOrder = 1
            Text = 'editCustom'
          end
        end
      end
    end
  end
end
