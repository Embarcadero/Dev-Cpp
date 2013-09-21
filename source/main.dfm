object MainForm: TMainForm
  Left = 821
  Top = 332
  Width = 848
  Height = 685
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu
  OldCreateOrder = False
  Position = poDefault
  OnClose = FormClose
  OnContextPopup = FormContextPopup
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnPaint = FormPaint
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object SplitterLeft: TSplitter
    Left = 193
    Top = 72
    Width = 2
    Height = 422
    AutoSnap = False
    MinSize = 45
    ResizeStyle = rsUpdate
  end
  object SplitterBottom: TSplitter
    Left = 0
    Top = 494
    Width = 832
    Height = 2
    Cursor = crVSplit
    Align = alBottom
    OnCanResize = SplitterBottomCanResize
    OnMoved = SplitterBottomMoved
  end
  object MessageControl: TPageControl
    Left = 0
    Top = 496
    Width = 832
    Height = 112
    ActivePage = DebugSheet
    Align = alBottom
    Constraints.MinHeight = 1
    Images = dmMain.MenuImages_NewLook
    MultiLine = True
    PopupMenu = MessagePopup
    TabOrder = 0
    OnChange = MessageControlChange
    OnChanging = MessageControlChanging
    OnContextPopup = MessageControlContextPopup
    object CompSheet: TTabSheet
      BorderWidth = 2
      Caption = 'Compiler'
      ImageIndex = 28
      object CompilerOutput: TListView
        Left = 0
        Top = 0
        Width = 820
        Height = 80
        Align = alClient
        BevelOuter = bvRaised
        BorderStyle = bsNone
        Columns = <
          item
            Caption = 'Line'
            Width = 40
          end
          item
            Caption = 'Col'
            Width = 40
          end
          item
            Caption = 'Unit'
            Width = 200
          end
          item
            Caption = 'Message'
            Width = 480
          end>
        ColumnClick = False
        GridLines = True
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        ViewStyle = vsReport
        OnDblClick = CompilerOutputDblClick
        OnKeyDown = CompilerOutputKeyDown
      end
    end
    object ResSheet: TTabSheet
      BorderWidth = 2
      Caption = 'Resource'
      ImageIndex = 2
      object ResourceOutput: TListBox
        Left = 0
        Top = 0
        Width = 820
        Height = 80
        Align = alClient
        BevelKind = bkSoft
        BorderStyle = bsNone
        ItemHeight = 13
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
      end
    end
    object LogSheet: TTabSheet
      BorderWidth = 2
      Caption = 'Compile log'
      ImageIndex = 43
      object InfoGroupBox: TGroupBox
        Left = 0
        Top = 0
        Width = 257
        Height = 80
        Align = alLeft
        Caption = 'Information :'
        TabOrder = 0
        object ErrorLabel: TLabel
          Left = 8
          Top = 20
          Width = 56
          Height = 13
          Caption = 'Total errors:'
        end
        object SizeOfOutput: TLabel
          Left = 8
          Top = 44
          Width = 84
          Height = 13
          Caption = 'Size of output file:'
        end
        object btnAbortCompilation: TSpeedButton
          Left = 8
          Top = 64
          Width = 241
          Height = 20
          Action = actAbortCompilation
          Flat = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object SizeFile: TEdit
          Left = 96
          Top = 40
          Width = 153
          Height = 21
          ReadOnly = True
          TabOrder = 1
          Text = '0'
        end
        object TotalErrors: TEdit
          Left = 96
          Top = 16
          Width = 153
          Height = 21
          ReadOnly = True
          TabOrder = 0
          Text = '0'
        end
      end
      object CompResGroupBox: TGroupBox
        Left = 257
        Top = 0
        Width = 563
        Height = 80
        Align = alClient
        Caption = 'Compile log :'
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 1
        DesignSize = (
          563
          80)
        object LogOutput: TMemo
          Left = 7
          Top = 16
          Width = -1423
          Height = 62
          Anchors = [akLeft, akTop, akRight, akBottom]
          BevelInner = bvSpace
          BevelKind = bkTile
          BorderStyle = bsNone
          ReadOnly = True
          ScrollBars = ssVertical
          TabOrder = 0
        end
      end
    end
    object DebugSheet: TTabSheet
      BorderWidth = 2
      Caption = 'Debugging'
      ImageIndex = 32
      object DebugSubPages: TPageControl
        Left = 0
        Top = 0
        Width = 820
        Height = 80
        ActivePage = tabVars
        Align = alClient
        Style = tsFlatButtons
        TabOrder = 0
        OnChange = DebugSubPagesChange
        object tabVars: TTabSheet
          Caption = 'Debug'
          object PanelDebug: TPanel
            Left = 468
            Top = 0
            Width = 156
            Height = 49
            Align = alLeft
            BevelOuter = bvLowered
            TabOrder = 0
            object AddWatchBtn: TSpeedButton
              Left = 4
              Top = 3
              Width = 148
              Height = 20
              Action = actAddWatch
              Flat = True
              Glyph.Data = {
                36030000424D3603000000000000360000002800000010000000100000000100
                18000000000000030000120B0000120B00000000000000000000AFAFAFAFAFAF
                AFAFAFAFAFAF8081812F34345A5B5BA6A6A6AFAFAFAFAFAFAFAFAFAFAFAFA1A1
                A1858585A6A6A6AFAFAFAFAFAFAFAFAFAFAFAF777777606A6ACEE5E582909074
                79796E6E6EAFAFAFAFAFAFA6A6A63D3D3D000000878787AFAFAFAFAFAFAFAFAF
                AFAFAF3A3A3AB4C9C9D8F1F1D4ECECB2C6C65D6666696969AFAFAF3A3A3A0000
                00454545A4A4A4AFAFAFAFAFAFAFAFAF6B6B6B8D9C9CD2EAEAD2EAEAC4DADAB3
                C8C8ABBEBE6670700E0E0E0404044D4D4DABABABAFAFAFAFAFAFAFAFAF9E9E9E
                6C6F6FB2C4C4CDE5E5A7BABA667272475050414848494C4C1515154B54547C7C
                7CAFAFAFAFAFAFAFAFAFAFAFAF878787555E5EC6DDDDBFD6D6636F6F82919197
                A9A9869696616C6C353B3BBACFCF7580803A3A3A949494AFAFAFAFAFAF282A2A
                9DABABC5DCDCB3C7C7444B4BC4D3D3AFC0C096A7A7859494333838C5DBDBD5ED
                ED99AAAA262929AFAFAFAFAFAF101111737E7EB9CFCFB2C7C74C5555CEDBDBCC
                DADAA3B6B68D9E9E3F4646C7DEDED6EEEE9CAEAE4E5151AFAFAFAFAFAF202121
                7C8181879595B1C7C76C7878818E8EC2D3D3A6BABA6E7A7A727F7FD2EAEAA8BB
                BB595C5C9F9F9FAFAFAFAFAFAF333535DCE4E47478788A9A9AAABFBF717F7F3C
                43433B4242788686C2D8D8BFD5D5464949A4A4A4AFAFAFAFAFAFAFAFAF343434
                D0D1D1787F7FAFC5C5C1D7D7C5DDDDCBE3E3D0E8E8D3EBEBD6EFEF6B77776161
                61AFAFAFAFAFAFAFAFAFAFAFAF26262682828293A1A1BCD3D3C0D7D7C3DADAC6
                DEDECCE4E4D0E8E897A6A66E7070AFAFAFAFAFAFAFAFAFAFAFAFAFAFAF757575
                1E1E1E6C7878B3C9C9BFD5D5C2D8D8C4DBDBC8E0E0B8CECE4148489D9D9DAFAF
                AFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAF5B5B5B383A3A758383A0B2B2C0
                D7D7BDD2D26B7575858585AFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAF
                AFAFAFAFAFAFA6A6A65B5B5B4B4F4F5761618D9D9D575757AFAFAFAFAFAFAFAF
                AFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAF97979785
                85853A3D3D979797AFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAFAF}
            end
            object RemoveWatchBtn: TSpeedButton
              Left = 4
              Top = 26
              Width = 148
              Height = 20
              Action = actRemoveWatch
              Flat = True
              Glyph.Data = {
                36030000424D3603000000000000360000002800000010000000100000000100
                18000000000000030000120B0000120B00000000000000000000AAAAAAAAAAAA
                AAAAAA000000000000000000000000000000000000000000000000080808AAAA
                AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA00000075977D72947A718F786E8D746B
                8A7268856F67826D647D69000000080808080808AAAAAAAAAAAAAAAAAA000000
                789A809AB2A05F786596AD9A5C72606B8A724C5B4F67836D536858617A670000
                00080808080808AAAAAAAAAAAA000000637D68B4C5B75F786596AD9A4E5F526D
                8A724C5C4F67836D48584C637A68000000080808080808080808AAAAAA000000
                637D68B4C6B75F786596AD9A4E5F526D8A724C5C4F67836E48584D637A680000
                00080808080808080808AAAAAA000000637D68B4C6B95F786596AD9B4F5F526D
                8A724C5C4F67836E49584D637B68000000080808080808080808AAAAAA000000
                637D68B4C6B95F786596AD9B4F5F526D8A724C5C4F68836E49584D637B680000
                00080808080808AAAAAAAAAAAA000000637D68B4C6B95F786597AE9B4F5F526D
                8A724C5C4F68846E49584D637B68000000AAAAAAAAAAAAAAAAAAAAAAAA000000
                637D68252525252525252525252525252525252525252525252525637B680000
                00AAAAAAAAAAAAAAAAAAAAAAAA0000002525256E8B73637B68637B685A6F5E52
                64574553494B5B4F3D453F252525000000AAAAAAAAAAAAAAAAAA000000547363
                6B8C748DAA948AA58F8AA58F74967C74967C68866F617966586E5D4B5A4D4552
                48000000AAAAAAAAAAAA00000074967CABC0B0C8D6CED6E1DAB1C4BB94ADA091
                AB9E8FAA9B8EA79A8CA59874967C576B5B000000AAAAAAAAAAAA0000008DA596
                D2DED7B5C9BE98B3A496B1A295B0A094ADA091AB9E8FAA9B8EA79A8CA5988AA3
                97000000AAAAAAAAAAAA000000ADC4B6B7CCBF9BB6A7B5C9BE5E6F65282E2B31
                393522262491AB9E8FAA9B8EA79A8CA598000000AAAAAAAAAAAAAAAAAA252525
                0000009EBAA8ABC1B4DDE7E17D8A83869F90434E4894ADA091AB9E0000000000
                00AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA00000000000000000000000000
                0000000000000000000000AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA}
            end
          end
          object DebugPanel: TPanel
            Left = 0
            Top = 0
            Width = 156
            Height = 49
            Align = alLeft
            BevelOuter = bvLowered
            TabOrder = 1
            object NextStepBtn: TSpeedButton
              Left = 4
              Top = 3
              Width = 148
              Height = 20
              Action = actNextStep
              Flat = True
              Glyph.Data = {
                36040000424D3604000000000000360000002800000010000000100000000100
                2000000000000004000000000000000000000000000000000000FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
                000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
                0000316339000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
                00007BAD840052845A00295A31000000000000000000FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
                000084B5940073A584006B9C73004A7B5200315A39000000000000000000FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
                00008CBD9C007BAD840073A584007BAD84006B9C73004A7B520042734A000000
                000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
                00008CBD9C007BAD84007BAD84007BAD84007BA584007BAD84006B9C73004A7B
                5200425A42000000000000000000FF00FF00FF00FF00FF00FF00FF00FF000000
                000094BD9C0084AD8C007BAD840084AD8C007BAD84007BAD840073A57B007BA5
                84006B9C73004A845A00315A39000000000000000000FF00FF00FF00FF002929
                2900FFFFFF00EFF7EF00EFF7EF00EFF7EF00EFF7EF00E7EFE700DEE7DE00D6E7
                D600DEE7DE00EFF7EF00ADC6B5000000000000000000FF00FF00FF00FF000000
                0000FFFFFF00EFF7EF00EFF7EF00EFF7EF00EFF7EF00EFF7EF00E7EFE700EFF7
                F700C6DECE001818180000000000FF00FF00FF00FF00FF00FF00FF00FF000000
                0000FFFFFF00F7F7F700F7F7F700EFF7EF00EFF7EF00F7F7F700C6DECE000000
                000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
                0000FFFFFF00F7F7F700EFF7EF00EFF7F700CEDECE000000000000000000FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
                0000FFFFFF00EFF7F700D6E7D6000000000000000000FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
                0000DEEFE7000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
                000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
            end
            object StepIntoBtn: TSpeedButton
              Left = 4
              Top = 26
              Width = 148
              Height = 20
              Action = actStepSingle
              Flat = True
              Glyph.Data = {
                36040000424D3604000000000000360000002800000010000000100000000100
                2000000000000004000000000000000000000000000000000000FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF0000000000FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00000000002942310000000000FF00FF00FF00FF00FF00FF00FF00
                FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00000000002942310000000000FF00FF00FF00FF00FF00
                FF000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF0000000000008400000084000000000000000000000000
                000000A5000000B5000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF000000000000000000009400000094000000A5
                000000A5000000C6000000B5000000000000FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000000000000000
                000000A5000000B5000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
            end
          end
          object DebugPanel2: TPanel
            Left = 156
            Top = 0
            Width = 156
            Height = 49
            Align = alLeft
            BevelOuter = bvLowered
            TabOrder = 2
            object StepOverBtn: TSpeedButton
              Left = 4
              Top = 3
              Width = 148
              Height = 20
              Action = actStepOver
              Flat = True
              Glyph.Data = {
                36040000424D3604000000000000360000002800000010000000100000000100
                2000000000000004000000000000000000000000000000000000FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF0000000000FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00000000002942310000000000FF00FF00FF00FF00FF00FF00FF00
                FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00000000002942310000000000FF00FF00FF00FF00FF00
                FF000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF0000000000008400000084000000000000000000000000
                000000A5000000B5000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF000000000000000000009400000094000000A5
                000000A5000000C6000000B5000000000000FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000000000000000
                000000A5000000B5000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
            end
            object RunToCursorBtn: TSpeedButton
              Left = 4
              Top = 26
              Width = 148
              Height = 20
              Action = actRunToCursor
              Flat = True
              Glyph.Data = {
                36040000424D3604000000000000360000002800000010000000100000000100
                2000000000000004000000000000000000000000000000000000FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF0000000000FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00000000002942310000000000FF00FF00FF00FF00FF00FF00FF00
                FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00000000002942310000000000FF00FF00FF00FF00FF00
                FF000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF0000000000008400000084000000000000000000000000
                000000A5000000B5000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF000000000000000000009400000094000000A5
                000000A5000000C6000000B5000000000000FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000000000000000
                000000A5000000B5000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
            end
          end
          object DebugPanel3: TPanel
            Left = 312
            Top = 0
            Width = 156
            Height = 49
            Align = alLeft
            BevelOuter = bvLowered
            TabOrder = 3
            object DDebugBtn: TSpeedButton
              Left = 4
              Top = 3
              Width = 148
              Height = 20
              Action = actDebug
              Flat = True
              Glyph.Data = {
                36040000424D3604000000000000360000002800000010000000100000000100
                2000000000000004000000000000000000000000000000000000FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00F7F7F700D6D6
                D600E7E7E700FF00FF00EFEFF700FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00EFEFEF00CECECE00ADADAD00ADADAD00A5A5
                A500737373009494AD00D6D6EF00FF00FF00FF00FF00FF00FF00FF00FF00EFEF
                FF00F7F7FF00FFFFFF00D6D6D6009C9C9C00636363004A4A4A00313939005A5A
                5A003131840052528400E7E7E700FF00FF00FF00FF00FF00FF00FF00FF00E7E7
                E700A5A5C600C6C6D6005A635A00393939002929290021212100212929001821
                520010107B004A4A5200C6C6C600F7F7F700FF00FF00FF00FF00FF00FF00EFEF
                EF00636384004242A50021294A00182121003942390031393900212939000808
                8C0021293100424242007B7B7B0094949400F7F7F700FF00FF00FF00FF00F7F7
                F7008C8C8C0039427B000000AD002931420031393900424252000000A5002131
                63002939390039424200636363007B7B7B00F7F7F700FF00FF00FF00FF00FF00
                FF00CECECE0042524A0018298C000808A50018215A0018189400081094004263
                5A0039524A003142420039393900A5A5A500FFFFFF00FF00FF00FF00FF00F7F7
                F700A5A5A5009CA59C004A737B000000AD000000AD0008089C00424A5A005A84
                7B004A6B630039524A006B6B6B00C6C6C600FFFFFF00FF00FF00FF00FF00FFFF
                FF0084848400BDC6C6007B949C002939AD000000AD000000AD00181894005A7B
                94005A847B0042635A00636B6B00CECECE00FF00FF00FF00FF00FF00FF00FF00
                FF00C6C6C600393994000808AD003942B500A5BDCE006B73AD000000AD000000
                AD0039529C004A6B63004A525200BDBDBD00FF00FF00FF00FF00FF00FF00EFEF
                F7005252CE000000AD000000AD00BDCECE00C6D6D600C6DED600737B8C003142
                A5002939A50052736B00393939008C8C8C00EFEFEF00FF00FF008C8CDE001010
                B5000000AD000000AD009494DE00EFEFEF00A5BDB500B5CEC600A5BDBD007B9C
                9C005A7B730063736B00ADADAD0094949400EFEFEF00FF00FF000000B5000000
                AD001818B500BDBDEF00FF00FF00E7E7E7005A5A5A00849C9C0094B5AD00738C
                840039635A0063736B00EFEFEF00A5A5A500FF00FF00FF00FF00ADADE7009C9C
                DE00F7F7FF00FF00FF00FF00FF00F7F7F700737373009CADAD0052848C007384
                8400C6CECE00EFEFEF00FF00FF00EFEFEF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FFFFFF0094949400E7E7E700FFFFFF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00E7E7E700EFEFEF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
            end
            object StopExecBtn: TSpeedButton
              Left = 4
              Top = 26
              Width = 148
              Height = 20
              Action = actStopExecute
              Flat = True
              Glyph.Data = {
                36040000424D3604000000000000360000002800000010000000100000000100
                2000000000000004000000000000000000000000000000000000FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00F7F7F700D6D6
                D600E7E7E700FF00FF00EFEFF700FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00EFEFEF00CECECE00ADADAD00ADADAD00A5A5
                A500737373009494AD00D6D6EF00FF00FF00FF00FF00FF00FF00FF00FF00EFEF
                FF00F7F7FF00FFFFFF00D6D6D6009C9C9C00636363004A4A4A00313939005A5A
                5A003131840052528400E7E7E700FF00FF00FF00FF00FF00FF00FF00FF00E7E7
                E700A5A5C600C6C6D6005A635A00393939002929290021212100212929001821
                520010107B004A4A5200C6C6C600F7F7F700FF00FF00FF00FF00FF00FF00EFEF
                EF00636384004242A50021294A00182121003942390031393900212939000808
                8C0021293100424242007B7B7B0094949400F7F7F700FF00FF00FF00FF00F7F7
                F7008C8C8C0039427B000000AD002931420031393900424252000000A5002131
                63002939390039424200636363007B7B7B00F7F7F700FF00FF00FF00FF00FF00
                FF00CECECE0042524A0018298C000808A50018215A0018189400081094004263
                5A0039524A003142420039393900A5A5A500FFFFFF00FF00FF00FF00FF00F7F7
                F700A5A5A5009CA59C004A737B000000AD000000AD0008089C00424A5A005A84
                7B004A6B630039524A006B6B6B00C6C6C600FFFFFF00FF00FF00FF00FF00FFFF
                FF0084848400BDC6C6007B949C002939AD000000AD000000AD00181894005A7B
                94005A847B0042635A00636B6B00CECECE00FF00FF00FF00FF00FF00FF00FF00
                FF00C6C6C600393994000808AD003942B500A5BDCE006B73AD000000AD000000
                AD0039529C004A6B63004A525200BDBDBD00FF00FF00FF00FF00FF00FF00EFEF
                F7005252CE000000AD000000AD00BDCECE00C6D6D600C6DED600737B8C003142
                A5002939A50052736B00393939008C8C8C00EFEFEF00FF00FF008C8CDE001010
                B5000000AD000000AD009494DE00EFEFEF00A5BDB500B5CEC600A5BDBD007B9C
                9C005A7B730063736B00ADADAD0094949400EFEFEF00FF00FF000000B5000000
                AD001818B500BDBDEF00FF00FF00E7E7E7005A5A5A00849C9C0094B5AD00738C
                840039635A0063736B00EFEFEF00A5A5A500FF00FF00FF00FF00ADADE7009C9C
                DE00F7F7FF00FF00FF00FF00FF00F7F7F700737373009CADAD0052848C007384
                8400C6CECE00EFEFEF00FF00FF00EFEFEF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FFFFFF0094949400E7E7E700FFFFFF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00E7E7E700EFEFEF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
            end
          end
        end
        object tabBacktrace: TTabSheet
          Caption = 'Backtrace'
          ImageIndex = 1
          object lvBacktrace: TListView
            Left = 0
            Top = 0
            Width = 812
            Height = 49
            Align = alClient
            Columns = <
              item
                Caption = 'Function name'
                Width = 150
              end
              item
                Caption = 'Arguments'
                Width = 250
              end
              item
                Caption = 'Filename'
                Width = 150
              end
              item
                Caption = 'Line'
              end>
            ReadOnly = True
            RowSelect = True
            TabOrder = 0
            ViewStyle = vsReport
            OnCustomDrawItem = lvBacktraceCustomDrawItem
            OnDblClick = lvBacktraceDblClick
            OnMouseMove = lvBacktraceMouseMove
          end
        end
        object tabDebugOutput: TTabSheet
          Caption = 'Output'
          ImageIndex = 2
          object DebugOutput: TMemo
            Left = 0
            Top = 22
            Width = 812
            Height = 27
            Align = alClient
            Lines.Strings = (
              'Debugger output')
            ReadOnly = True
            ScrollBars = ssVertical
            TabOrder = 0
          end
          object GdbOutputPanel: TPanel
            Left = 0
            Top = 0
            Width = 812
            Height = 22
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 1
            object lblSendCommandGdb: TLabel
              Left = 4
              Top = 0
              Width = 118
              Height = 13
              Caption = 'Send command to GDB :'
            end
            object edGdbCommand: TEdit
              Left = 160
              Top = 0
              Width = 233
              Height = 21
              TabOrder = 0
              OnKeyPress = edGdbCommandKeyPress
            end
            object GdbCommandBtn: TButton
              Left = 398
              Top = 0
              Width = 62
              Height = 18
              Caption = 'Send'
              TabOrder = 1
              OnClick = GdbCommandBtnClick
            end
          end
        end
      end
    end
    object FindSheet: TTabSheet
      BorderWidth = 2
      Caption = 'Find results'
      ImageIndex = 21
      object FindOutput: TListView
        Left = 0
        Top = 0
        Width = 820
        Height = 80
        Align = alClient
        BevelOuter = bvNone
        BorderStyle = bsNone
        Columns = <
          item
            Caption = 'Line'
            Width = 40
          end
          item
            Caption = 'Col'
            Width = 40
          end
          item
            Caption = 'Unit'
            Width = 200
          end
          item
            Caption = 'Message'
            Width = 480
          end>
        ColumnClick = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -7
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        GridLines = True
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        ViewStyle = vsReport
        OnDblClick = FindOutputDblClick
        OnKeyDown = FindOutputKeyDown
      end
    end
    object CloseSheet: TTabSheet
      Caption = 'Close'
      ImageIndex = 9
    end
  end
  object ControlBar1: TControlBar
    Left = 0
    Top = 16
    Width = 832
    Height = 56
    Align = alTop
    AutoDock = False
    AutoSize = True
    BevelInner = bvNone
    BevelKind = bkSoft
    RowSize = 28
    TabOrder = 1
    OnContextPopup = ControlBar1ContextPopup
    object tbMain: TToolBar
      Left = 11
      Top = 2
      Width = 177
      Height = 22
      AutoSize = True
      Caption = 'Main'
      Constraints.MaxWidth = 177
      Constraints.MinWidth = 177
      DragKind = dkDock
      EdgeBorders = []
      EdgeInner = esNone
      EdgeOuter = esNone
      Flat = True
      Images = dmMain.MenuImages_NewLook
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Wrapable = False
      object NewProjectBtn: TToolButton
        Left = 0
        Top = 0
        Action = actNewProject
      end
      object OpenBtn: TToolButton
        Left = 23
        Top = 0
        Action = actOpen
      end
      object ToolButton3: TToolButton
        Left = 46
        Top = 0
        Width = 8
        Caption = 'ToolButton3'
        ImageIndex = 2
        Style = tbsSeparator
      end
      object NewFileBtn: TToolButton
        Left = 54
        Top = 0
        Action = actNewSource
      end
      object SaveUnitBtn: TToolButton
        Left = 77
        Top = 0
        Action = actSave
      end
      object SaveAllBtn: TToolButton
        Left = 100
        Top = 0
        Action = actSaveAll
      end
      object CloseBtn: TToolButton
        Left = 123
        Top = 0
        Action = actClose
      end
      object ToolButton7: TToolButton
        Left = 146
        Top = 0
        Width = 8
        Caption = 'ToolButton7'
        ImageIndex = 5
        Style = tbsSeparator
      end
      object PrintBtn: TToolButton
        Left = 154
        Top = 0
        Action = actPrint
      end
    end
    object tbCompile: TToolBar
      Left = 469
      Top = 2
      Width = 148
      Height = 22
      AutoSize = True
      Caption = 'Compile and Run'
      Constraints.MaxWidth = 148
      Constraints.MinWidth = 148
      DragKind = dkDock
      EdgeBorders = []
      EdgeInner = esNone
      EdgeOuter = esNone
      Flat = True
      Images = dmMain.MenuImages_NewLook
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      Wrapable = False
      object CompileBtn: TToolButton
        Left = 0
        Top = 0
        Action = actCompile
      end
      object RunBtn: TToolButton
        Left = 23
        Top = 0
        Action = actRun
      end
      object CompileAndRunBtn: TToolButton
        Left = 46
        Top = 0
        Action = actCompRun
      end
      object RebuildAllBtn: TToolButton
        Left = 69
        Top = 0
        Action = actRebuild
      end
      object ToolButton2: TToolButton
        Left = 92
        Top = 0
        Width = 8
        Caption = 'ToolButton2'
        ImageIndex = 33
        Style = tbsSeparator
      end
      object DebugBtn: TToolButton
        Left = 100
        Top = 0
        Action = actDebug
      end
      object ProfileBtn: TToolButton
        Left = 123
        Top = 0
        Action = actProfileProject
      end
    end
    object tbProject: TToolBar
      Left = 378
      Top = 2
      Width = 78
      Height = 22
      AutoSize = True
      Caption = 'Project'
      Constraints.MaxWidth = 78
      Constraints.MinWidth = 78
      DragKind = dkDock
      EdgeBorders = []
      EdgeInner = esNone
      EdgeOuter = esNone
      Flat = True
      Images = dmMain.MenuImages_NewLook
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      Wrapable = False
      object AddToProjectBtn: TToolButton
        Left = 0
        Top = 0
        Action = actProjectAdd
      end
      object RemoveFromProjectBtn: TToolButton
        Left = 23
        Top = 0
        Action = actProjectRemove
      end
      object ToolButton20: TToolButton
        Left = 46
        Top = 0
        Width = 8
        Caption = 'ToolButton20'
        ImageIndex = 2
        Style = tbsSeparator
      end
      object ProjectOptionsBtn: TToolButton
        Left = 54
        Top = 0
        Action = actProjectOptions
      end
    end
    object tbEdit: TToolBar
      Left = 201
      Top = 2
      Width = 47
      Height = 22
      AutoSize = True
      Caption = 'Edit'
      Constraints.MaxWidth = 47
      Constraints.MinWidth = 47
      DragKind = dkDock
      EdgeBorders = []
      EdgeInner = esNone
      EdgeOuter = esNone
      Flat = True
      Images = dmMain.MenuImages_NewLook
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      Wrapable = False
      object UndoBtn: TToolButton
        Left = 0
        Top = 0
        Action = actUndo
      end
      object RedoBtn: TToolButton
        Left = 23
        Top = 0
        Action = actRedo
      end
    end
    object tbSearch: TToolBar
      Left = 261
      Top = 2
      Width = 104
      Height = 22
      AutoSize = True
      Caption = 'Search'
      Constraints.MaxWidth = 104
      Constraints.MinWidth = 104
      DragKind = dkDock
      EdgeBorders = []
      EdgeInner = esNone
      EdgeOuter = esNone
      Flat = True
      Images = dmMain.MenuImages_NewLook
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      Wrapable = False
      object FindBtn: TToolButton
        Left = 0
        Top = 0
        Action = actFind
      end
      object Replacebtn: TToolButton
        Left = 23
        Top = 0
        Action = actReplace
      end
      object FindNextBtn: TToolButton
        Left = 46
        Top = 0
        Action = actFindNext
      end
      object ToolButton1: TToolButton
        Left = 69
        Top = 0
        Width = 8
        Caption = 'ToolButton1'
        ImageIndex = 25
        Style = tbsSeparator
      end
      object GotoLineBtn: TToolButton
        Left = 77
        Top = 0
        Action = actGoto
      end
    end
    object tbSpecials: TToolBar
      Left = 11
      Top = 30
      Width = 243
      Height = 22
      AutoSize = True
      ButtonWidth = 60
      Caption = 'Specials'
      Constraints.MaxWidth = 243
      Constraints.MinWidth = 243
      DragKind = dkDock
      EdgeBorders = []
      EdgeInner = esNone
      EdgeOuter = esNone
      Flat = True
      Images = dmMain.SpecialImages_NewLook
      List = True
      ShowCaptions = True
      TabOrder = 5
      Wrapable = False
      object NewAllBtn: TToolButton
        Left = 0
        Top = 0
        Caption = 'New'
        ImageIndex = 0
        OnClick = NewAllBtnClick
      end
      object InsertBtn: TToolButton
        Left = 60
        Top = 0
        Caption = 'Insert'
        ImageIndex = 1
        OnClick = InsertBtnClick
      end
      object ToggleBtn: TToolButton
        Left = 120
        Top = 0
        Caption = 'Toggle'
        ImageIndex = 2
        OnClick = ToggleBtnClick
      end
      object GotoBtn: TToolButton
        Left = 180
        Top = 0
        Caption = 'Goto'
        ImageIndex = 3
        OnClick = GotoBtnClick
      end
    end
    object tbClasses: TToolBar
      Left = 267
      Top = 30
      Width = 452
      Height = 22
      AutoSize = True
      Caption = 'tbClasses'
      Constraints.MaxWidth = 452
      Constraints.MinWidth = 452
      EdgeBorders = []
      EdgeInner = esNone
      EdgeOuter = esNone
      Flat = True
      TabOrder = 6
      Wrapable = False
      object cmbClasses: TComboBox
        Left = 0
        Top = 0
        Width = 232
        Height = 22
        Style = csDropDownList
        DropDownCount = 16
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ItemHeight = 14
        ParentFont = False
        Sorted = True
        TabOrder = 0
        OnChange = cmbClassesChange
      end
      object cmbMembers: TComboBox
        Left = 232
        Top = 0
        Width = 219
        Height = 22
        Style = csDropDownList
        DropDownCount = 16
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ItemHeight = 14
        ParentFont = False
        Sorted = True
        TabOrder = 1
        OnChange = cmbMembersChange
      end
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 608
    Width = 832
    Height = 19
    Panels = <
      item
        Width = 70
      end
      item
        Width = 50
      end
      item
        Width = 80
      end
      item
        Width = 50
      end>
    ParentFont = True
    UseSystemFont = False
  end
  object PageControl: TPageControl
    Left = 195
    Top = 72
    Width = 637
    Height = 422
    Align = alClient
    MultiLine = True
    PopupMenu = EditorPopupMenu
    TabOrder = 3
    OnChange = PageControlChange
    OnChanging = PageControlChanging
    OnDragDrop = PageControlDragDrop
    OnDragOver = PageControlDragOver
    OnMouseDown = PageControlMouseDown
  end
  object pnlFull: TPanel
    Left = 0
    Top = 0
    Width = 832
    Height = 16
    Align = alTop
    BevelOuter = bvNone
    Caption = 
      'Dev-C++ version 5.0.0.4 Fullscreen. Press F10 to toggle this bar' +
      ', F11 to toggle Toolbars or F12 to toggle Fullscreen.'
    TabOrder = 4
    Visible = False
    DesignSize = (
      832
      16)
    object btnFullScrRevert: TSpeedButton
      Left = 815
      Top = 0
      Width = 16
      Height = 16
      Anchors = [akTop, akRight]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = [fsBold]
      Glyph.Data = {
        C6000000424DC60000000000000076000000280000000A0000000A0000000100
        0400000000005000000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDD77
        7777D00DDDD00D777777D000DD000D777777DD000000DD777777DDD0000DDD77
        7777DDD0000DDD777777DD000000DD777777D000DD000D777777D00DDDD00D77
        7777DDDDDDDDDD777777}
      ParentFont = False
      OnClick = btnFullScrRevertClick
    end
  end
  object devFileMonitor1: TdevFileMonitor
    Left = 112
    Top = 152
    Width = 0
    Height = 0
    Active = False
    OnNotifyChange = devFileMonitor1NotifyChange
  end
  object LeftPageControl: TPageControl
    Left = 0
    Top = 72
    Width = 193
    Height = 422
    ActivePage = ProjectSheet
    Align = alLeft
    Images = dmMain.ProjectImage_NewLook
    TabOrder = 6
    object ProjectSheet: TTabSheet
      Caption = 'Project'
      ImageIndex = -1
      object ProjectView: TTreeView
        Left = 0
        Top = 0
        Width = 185
        Height = 394
        Align = alClient
        Anchors = [akLeft, akTop, akBottom]
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        ChangeDelay = 1
        DragMode = dmAutomatic
        HideSelection = False
        HotTrack = True
        Images = dmMain.ProjectImage_Gnome
        Indent = 19
        MultiSelect = True
        MultiSelectStyle = [msControlSelect, msShiftSelect]
        ReadOnly = True
        RightClickSelect = True
        SortType = stText
        TabOrder = 0
        OnClick = ProjectViewClick
        OnCompare = ProjectViewCompare
        OnContextPopup = ProjectViewContextPopup
        OnDblClick = ProjectViewDblClick
        OnDragDrop = ProjectViewDragDrop
        OnDragOver = ProjectViewDragOver
        OnKeyDown = ProjectViewKeyDown
        OnKeyPress = ProjectViewKeyPress
        OnMouseDown = ProjectViewMouseDown
      end
    end
    object ClassSheet: TTabSheet
      Caption = 'Classes'
      ImageIndex = -1
      object ClassBrowser1: TClassBrowser
        Left = 0
        Top = 0
        Width = 185
        Height = 394
        Align = alClient
        Images = dmMain.ClassImages
        ReadOnly = True
        Indent = 19
        TabOrder = 0
        PopupMenu = BrowserPopup
        BorderStyle = bsNone
        ShowFilter = sfAll
        OnSelect = ClassBrowser1Select
        Parser = CppParser1
        ItemImages.Globals = 0
        ItemImages.Classes = 1
        ItemImages.VariablePrivate = 2
        ItemImages.VariableProtected = 3
        ItemImages.VariablePublic = 4
        ItemImages.VariablePublished = 4
        ItemImages.MethodPrivate = 5
        ItemImages.MethodProtected = 6
        ItemImages.MethodPublic = 7
        ItemImages.MethodPublished = 7
        ItemImages.InheritedMethodProtected = 8
        ItemImages.InheritedMethodPublic = 10
        ItemImages.InheritedVariableProtected = 9
        ItemImages.InheritedVariablePublic = 11
        UseColors = True
        ShowInheritedMembers = False
      end
    end
    object DebugLeftSheet: TTabSheet
      Caption = 'Debug'
      ImageIndex = -1
      object DebugTree: TTreeView
        Left = 0
        Top = 0
        Width = 185
        Height = 394
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        ChangeDelay = 1
        Images = dmMain.MenuImages_NewLook
        Indent = 19
        MultiSelectStyle = []
        PopupMenu = DebugVarsPopup
        ReadOnly = True
        RightClickSelect = True
        TabOrder = 0
        OnKeyDown = DebugTreeKeyDown
      end
    end
  end
  object MainMenu: TMainMenu
    Images = dmMain.MenuImages_NewLook
    Left = 294
    Top = 79
    object FileMenu: TMenuItem
      Action = actFileMenu
      object mnuNew: TMenuItem
        Caption = 'New'
        object NewSourceFileItem: TMenuItem
          Tag = 2
          Action = actNewSource
        end
        object NewprojectItem: TMenuItem
          Action = actNewProject
        end
        object N13: TMenuItem
          Caption = '-'
        end
        object NewresourcefileItem: TMenuItem
          Action = actNewRes
        end
        object NewTemplateItem: TMenuItem
          Action = actNewTemplate
        end
        object N5: TMenuItem
          Caption = '-'
        end
        object Class1: TMenuItem
          Action = actBrowserNewClass
          Caption = 'Class...'
        end
      end
      object N34: TMenuItem
        Caption = '-'
      end
      object OpenprojectItem: TMenuItem
        Tag = 1
        Action = actOpen
      end
      object ReOpenItem: TMenuItem
        AutoHotkeys = maManual
        Caption = '&Reopen'
        ImageIndex = 39
        object ClearhistoryItem: TMenuItem
          Action = actHistoryClear
        end
        object N11: TMenuItem
          Caption = '-'
          Enabled = False
        end
      end
      object N12: TMenuItem
        Caption = '-'
        Enabled = False
      end
      object SaveUnitItem: TMenuItem
        Tag = 3
        Action = actSave
      end
      object SaveUnitAsItem: TMenuItem
        Action = actSaveAs
      end
      object SaveprojectasItem: TMenuItem
        Action = actSaveProjectAs
      end
      object SaveallItem: TMenuItem
        Action = actSaveAll
      end
      object N33: TMenuItem
        Caption = '-'
      end
      object CloseItem: TMenuItem
        Tag = 4
        Action = actClose
      end
      object CloseAll2: TMenuItem
        Action = actCloseAll
      end
      object CloseprojectItem: TMenuItem
        Action = actCloseProject
      end
      object N35: TMenuItem
        Caption = '-'
      end
      object Properties1: TMenuItem
        Action = actFileProperties
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object ImportItem: TMenuItem
        Caption = '&Import'
        object ImportMSVisualCproject1: TMenuItem
          Action = actImportMSVC
        end
      end
      object ExportItem: TMenuItem
        Caption = '&Export'
        ImageIndex = 12
        object HTMLItem: TMenuItem
          Action = actXHTML
        end
        object RTFItem: TMenuItem
          Action = actXRTF
        end
        object N19: TMenuItem
          Caption = '-'
        end
        object ProjecttoHTMLItem: TMenuItem
          Action = actXProject
        end
      end
      object N43: TMenuItem
        Caption = '-'
      end
      object PrintItem: TMenuItem
        Tag = 5
        Action = actPrint
      end
      object PrinterSetupItem: TMenuItem
        Action = actPrintSU
      end
      object N3: TMenuItem
        Caption = '-'
        Enabled = False
      end
      object ExitItem: TMenuItem
        Action = actExit
        GroupIndex = 9
      end
    end
    object EditMenu: TMenuItem
      Action = actEditMenu
      object UndoItem: TMenuItem
        Tag = 6
        Action = actUndo
      end
      object RedoItem: TMenuItem
        Action = actRedo
      end
      object N4: TMenuItem
        Caption = '-'
        Enabled = False
      end
      object CutItem: TMenuItem
        Action = actCut
        AutoHotkeys = maAutomatic
        AutoLineReduction = maAutomatic
      end
      object CopyItem: TMenuItem
        Action = actCopy
      end
      object PasteItem: TMenuItem
        Action = actPaste
      end
      object SelectallItem: TMenuItem
        Action = actSelectAll
      end
      object N23: TMenuItem
        Caption = '-'
      end
      object Swapheadersource2: TMenuItem
        Action = actSwapHeaderSource
      end
      object N14: TMenuItem
        Caption = '-'
      end
      object InsertItem: TMenuItem
        Caption = '&Insert'
        object DateTimeMenuItem: TMenuItem
          Caption = '&Date/Time'
          OnClick = DateTimeMenuItemClick
        end
        object CommentheaderMenuItem: TMenuItem
          Caption = '&Comment header'
          OnClick = CommentheaderMenuItemClick
        end
        object N999: TMenuItem
          Caption = '-'
        end
      end
      object ToggleBookmarksItem: TMenuItem
        Caption = 'Toggle &Bookmarks'
        ImageIndex = 19
      end
      object GotoBookmarksItem: TMenuItem
        Caption = '&Goto Bookmarks'
        ImageIndex = 20
      end
      object N26: TMenuItem
        Caption = '-'
      end
      object Comment1: TMenuItem
        Action = actComment
        ShortCut = 16574
      end
      object Uncomment1: TMenuItem
        Action = actUncomment
        ShortCut = 16556
      end
      object N27: TMenuItem
        Caption = '-'
      end
      object Indent1: TMenuItem
        Action = actIndent
      end
      object Unindent1: TMenuItem
        Action = actUnindent
      end
    end
    object SearchMenu: TMenuItem
      Action = actSearchMenu
      object FindItem: TMenuItem
        Tag = 7
        Action = actFind
      end
      object FindinallfilesItem: TMenuItem
        Action = actFindAll
      end
      object FindnextItem: TMenuItem
        Action = actFindNext
      end
      object ReplaceItem: TMenuItem
        Action = actReplace
      end
      object IncrementalSearch1: TMenuItem
        Action = actIncremental
      end
      object N7: TMenuItem
        Caption = '-'
        Enabled = False
      end
      object Gotofunction1: TMenuItem
        Action = actGotoFunction
      end
      object GotolineItem: TMenuItem
        Action = actGoto
      end
    end
    object ViewMenu: TMenuItem
      Action = actViewMenu
      object ProjectManagerItem: TMenuItem
        Action = actProjectManager
        AutoCheck = True
      end
      object StatusbarItem: TMenuItem
        Action = actStatusbar
        AutoCheck = True
      end
      object CompileroutputItem: TMenuItem
        Caption = '&Compiler Output'
        object AlwaysShowItem: TMenuItem
          Action = actCompOutput
          AutoCheck = True
          GroupIndex = 1
        end
        object N37: TMenuItem
          Caption = '-'
          GroupIndex = 1
        end
        object ShowonlywhenneededItem: TMenuItem
          Action = actCompOnNeed
          AutoCheck = True
          GroupIndex = 1
        end
      end
      object ToolbarsItem: TMenuItem
        Caption = '&Toolbars'
        ImageIndex = 44
        object ToolMainItem: TMenuItem
          AutoCheck = True
          Caption = '&Main'
          Checked = True
          OnClick = ToolbarClick
        end
        object ToolEditItem: TMenuItem
          AutoCheck = True
          Caption = 'Edit'
          Checked = True
          OnClick = ToolbarClick
        end
        object ToolSearchItem: TMenuItem
          AutoCheck = True
          Caption = 'Search'
          Checked = True
          OnClick = ToolbarClick
        end
        object N2: TMenuItem
          Caption = '-'
        end
        object ToolCompileandRunItem: TMenuItem
          AutoCheck = True
          Caption = '&Compile and Run'
          Checked = True
          OnClick = ToolbarClick
        end
        object ToolProjectItem: TMenuItem
          AutoCheck = True
          Caption = '&Project'
          Checked = True
          OnClick = ToolbarClick
        end
        object N9: TMenuItem
          Caption = '-'
        end
        object ToolSpecialsItem: TMenuItem
          AutoCheck = True
          Caption = '&Specials'
          Checked = True
          OnClick = ToolbarClick
        end
        object N17: TMenuItem
          Caption = '-'
        end
        object ToolClassesItem: TMenuItem
          AutoCheck = True
          Caption = 'Classes'
          Checked = True
          OnClick = ToolbarClick
        end
      end
      object oDolist1: TMenuItem
        Action = actViewToDoList
      end
      object N63: TMenuItem
        Caption = '-'
      end
      object FloatingPojectManagerItem: TMenuItem
        Caption = '&Floating Project Manager'
        OnClick = FloatingPojectManagerItemClick
      end
      object FloatingReportwindowItem: TMenuItem
        Caption = 'Floating &Report window'
        OnClick = FloatingReportwindowItemClick
      end
      object N57: TMenuItem
        Caption = '-'
      end
      object GotoprojectmanagerItem: TMenuItem
        Caption = 'Go to Project &Manager'
        ShortCut = 16497
        OnClick = actGotoProjectManagerExecute
      end
      object GoToClassBrowserItem: TMenuItem
        Caption = 'Go to Class &Browser'
        ShortCut = 16498
        OnClick = GoToClassBrowserItemClick
      end
    end
    object ProjectMenu: TMenuItem
      Action = actProjectMenu
      object NewunitinprojectItem: TMenuItem
        Tag = 2
        Action = actProjectNew
      end
      object AddtoprojectItem: TMenuItem
        Action = actProjectAdd
      end
      object RemovefromprojectItem: TMenuItem
        Action = actProjectRemove
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object ProjectoptionsItem: TMenuItem
        Action = actProjectOptions
      end
    end
    object ExecuteMenu: TMenuItem
      Action = actExecuteMenu
      object CompileItem: TMenuItem
        Tag = 8
        Action = actCompile
      end
      object Compilecurrentfile1: TMenuItem
        Action = actCompileCurrentFile
      end
      object RunItem: TMenuItem
        Tag = 9
        Action = actRun
      end
      object mnuExecParameters: TMenuItem
        Action = actExecParams
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object CompileandRunItem: TMenuItem
        Action = actCompRun
      end
      object RebuildallItem: TMenuItem
        Action = actRebuild
      end
      object SyntaxCheckItem: TMenuItem
        Action = actSyntaxCheck
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object CleanItem: TMenuItem
        Action = actClean
      end
      object N29: TMenuItem
        Caption = '-'
      end
      object Profileanalysis1: TMenuItem
        Action = actProfileProject
      end
      object DeleteProfilingInformation: TMenuItem
        Action = actDeleteProfileProject
        Caption = 'Delete Profiling Information'
      end
      object N25: TMenuItem
        Caption = '-'
      end
      object Programreset1: TMenuItem
        Action = actProgramReset
      end
    end
    object DebugMenu: TMenuItem
      Action = actDebugMenu
      object DebugItem: TMenuItem
        Tag = 10
        Action = actDebug
      end
      object AttachtoprocessItem: TMenuItem
        Action = actAttachProcess
      end
      object StopExecution1: TMenuItem
        Action = actStopExecute
        ShortCut = 49265
      end
      object mnuDebugParameters: TMenuItem
        Action = actExecParams
      end
      object N18: TMenuItem
        Caption = '-'
      end
      object TogglebreakpointItem: TMenuItem
        Action = actBreakPoint
      end
      object DbgNextItem: TMenuItem
        Action = actNextStep
      end
      object DbgSingleStep: TMenuItem
        Action = actStepSingle
        ShortCut = 8310
      end
      object StepoverItem: TMenuItem
        Action = actStepOver
      end
      object RuntocursorItem: TMenuItem
        Action = actRunToCursor
        ShortCut = 8307
      end
      object N21: TMenuItem
        Caption = '-'
      end
      object AddwatchItem: TMenuItem
        Action = actAddWatch
      end
      object WatchItem: TMenuItem
        Action = actWatchItem
      end
      object ViewCPUItem: TMenuItem
        Action = actViewCPU
      end
    end
    object ToolsMenu: TMenuItem
      Action = actToolsMenu
      object CompileroptionsItem: TMenuItem
        Tag = 11
        Action = actCompOptions
      end
      object EnvironmentoptionsItem: TMenuItem
        Tag = 12
        Action = actEnviroOptions
      end
      object EditorOptions1: TMenuItem
        Action = actEditorOptions
      end
      object N20: TMenuItem
        Caption = '-'
      end
      object Configureshortcuts1: TMenuItem
        Action = actConfigShortcuts
      end
      object ConfiguretoolsItem: TMenuItem
        Action = actConfigTools
      end
      object N24: TMenuItem
        Caption = '-'
      end
      object CheckforupdatesItem: TMenuItem
        Action = actUpdateCheck
      end
      object mnuToolSep1: TMenuItem
        Caption = '-'
      end
      object PackageManagerItem: TMenuItem
        Bitmap.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000120B0000120B00000000000000000000BFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF00000000000000
          0000000000000000000000000000000000000000000000000000BFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBF000000DFDFDFB8B8B8B8B8B8B8B8B8B8B8B8B8B8B8B8B8
          B8B8B8B8969696000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBF000000F3F3F3DF
          DFDFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFB8B8B8000000BFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBF000000F3F3F3DFDFDF656565CFCFCF6565656565656565
          65CFCFCFB8B8B8000000000000000000000000000000000000000000F3F3F3DF
          DFDFDFDFDFDFDFDFDFDFDFCFCFCFCFCFCFCFCFCFB8B8B80000000000004CC6FF
          4CB5E64CB5E64CB5E6000000F3F3F3DFDFDF656565656565DFDFDF6565656565
          65CFCFCFB8B8B80000000000008DDBFF4CC6FF4CC6FF4CC6FF000000FFFFFFF3
          F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3DFDFDF0000000000008DDBFF
          4CC6FF4CC6FF4CC6FF6868680000000000000000000000000000000000000000
          000000000000000000000000008DDBFF4CC6FF4CC6FF4CC6FF4CC6FF4CC6FF24
          607B4CC6FF4CC6FF4CC6FF4CC6FF4CC6FF4CB5E6000000BFBFBF000000296D8C
          24607B24607B24607B24607B24607B24607B24607B24607B24607B24607B2460
          7B1F536B000000BFBFBF0000008DDBFF4CC6FF4CC6FF4CC6FF4CC6FF4CC6FF24
          607B4CC6FF4CC6FF226C8B226C8B226C8B4CB5E6000000BFBFBF0000008DDBFF
          4CC6FF4CC6FF4CC6FF4CC6FF4CC6FF24607B4CC6FF4CC6FF226C8B226C8B226C
          8B4CB5E6000000BFBFBF0000008DDBFF4CC6FF4CC6FF4CC6FF4CC6FF4CC6FF24
          607B4CC6FF4CC6FF226C8B226C8B226C8B4CB5E6000000BFBFBF000000D1F1FF
          8DDBFF8DDBFF8DDBFF8DDBFF8DDBFF296D8C8DDBFF8DDBFF8DDBFF8DDBFF8DDB
          FF4CC6FF000000BFBFBF68686800000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000BFBFBF}
        Caption = '&Package Manager'
        OnClick = PackageManagerItemClick
      end
    end
    object mnuCVS: TMenuItem
      Caption = 'CVS'
      OnClick = mnuCVSClick
      object mnuCVSCurrent: TMenuItem
        Caption = 'Current File...'
        object mnuCVSUpdate: TMenuItem
          Tag = 3
          Action = actCVSUpdate
        end
        object mnuCVSDiff: TMenuItem
          Tag = 3
          Action = actCVSDiff
        end
        object N53: TMenuItem
          Caption = '-'
        end
        object mnuCVSCommit: TMenuItem
          Tag = 3
          Action = actCVSCommit
        end
        object N56: TMenuItem
          Caption = '-'
        end
        object mnuCVSLog: TMenuItem
          Tag = 3
          Action = actCVSLog
        end
        object N61: TMenuItem
          Caption = '-'
        end
        object mnuCVSAdd: TMenuItem
          Tag = 3
          Action = actCVSAdd
        end
        object mnuCVSRemove: TMenuItem
          Tag = 3
          Action = actCVSRemove
        end
      end
      object mnuCVSWhole: TMenuItem
        Caption = 'Whole Project...'
        object mnuCVSUpdateP: TMenuItem
          Tag = 4
          Action = actCVSUpdate
        end
        object mnuCVSDiffP: TMenuItem
          Tag = 4
          Action = actCVSDiff
        end
        object N58: TMenuItem
          Caption = '-'
        end
        object Commit1: TMenuItem
          Tag = 4
          Action = actCVSCommit
        end
        object N46: TMenuItem
          Caption = '-'
        end
        object mnuCVSLogP: TMenuItem
          Tag = 4
          Action = actCVSLog
        end
      end
      object N65: TMenuItem
        Caption = '-'
      end
      object Login1: TMenuItem
        Action = actCVSLogin
      end
      object Logout1: TMenuItem
        Action = actCVSLogout
      end
      object N66: TMenuItem
        Caption = '-'
      end
      object mnuCVSImportP: TMenuItem
        Tag = 4
        Action = actCVSImport
      end
      object mnuCVSCheckoutP: TMenuItem
        Tag = 4
        Action = actCVSCheckout
      end
    end
    object WindowMenu: TMenuItem
      Action = actWindowMenu
      object CloseAllItem: TMenuItem
        Action = actCloseAll
      end
      object N28: TMenuItem
        Caption = '-'
      end
      object FullscreenmodeItem: TMenuItem
        Action = actFullScreen
        AutoCheck = True
      end
      object N36: TMenuItem
        Caption = '-'
      end
      object NextItem: TMenuItem
        Action = actNext
        ShortCut = 16393
      end
      object PreviousItem: TMenuItem
        Action = actPrev
        ShortCut = 24585
      end
      object N32: TMenuItem
        Caption = '-'
      end
      object ListItem: TMenuItem
        Caption = '&List...'
        OnClick = ListItemClick
      end
    end
    object HelpMenu: TMenuItem
      Action = actHelpMenu
      SubMenuImages = dmMain.HelpImages_Gnome
      GroupIndex = 9
      object HelpMenuItem: TMenuItem
        Caption = '&Help on Dev-C++'
        OnClick = HelpMenuItemClick
      end
      object ips1: TMenuItem
        Action = actShowTips
      end
      object AboutDevCppItem: TMenuItem
        Tag = 18
        Action = actAbout
      end
    end
  end
  object EditorPopupMenu: TPopupMenu
    OnPopup = EditorPopupMenuPopup
    Left = 403
    Top = 212
    object GotoDeclEditor: TMenuItem
      Action = actGotoDeclEditor
      Caption = 'Goto Declaration'
    end
    object GotoDefineEditor: TMenuItem
      Action = actGotoImplEditor
      Caption = 'Goto Definition'
    end
    object N15: TMenuItem
      Caption = '-'
    end
    object Close1: TMenuItem
      Action = actClose
    end
    object CloseAll1: TMenuItem
      Action = actCloseAll
    end
    object Closeallexceptthis1: TMenuItem
      Action = actCloseAllButThis
    end
    object N16: TMenuItem
      Caption = '-'
    end
    object UndoPopItem: TMenuItem
      Action = actUndo
    end
    object RedoPopItem: TMenuItem
      Action = actRedo
    end
    object MenuItem1: TMenuItem
      Caption = '-'
    end
    object CutPopItem: TMenuItem
      Action = actCut
    end
    object CopyPopItem: TMenuItem
      Action = actCopy
    end
    object PastePopItem: TMenuItem
      Action = actPaste
    end
    object SelectAllPopItem: TMenuItem
      Action = actSelectAll
    end
    object DeletePopItem: TMenuItem
      Action = actDelete
    end
    object N22: TMenuItem
      Caption = '-'
    end
    object Swapheadersource1: TMenuItem
      Action = actSwapHeaderSource
    end
    object MenuItem2: TMenuItem
      Caption = '-'
    end
    object InsertPopItem: TMenuItem
      Caption = '&Insert'
      ImageIndex = 30
      object CommentheaderPopItem: TMenuItem
        Caption = 'Comment header'
      end
      object DateandtimePopItem: TMenuItem
        Caption = 'Date and time'
      end
      object MenuItem3: TMenuItem
        Caption = '-'
      end
    end
    object TogglebookmarksPopItem: TMenuItem
      Caption = '&Toggle bookmarks'
      ImageIndex = 31
    end
    object GotobookmarksPopItem: TMenuItem
      Caption = '&Goto bookmarks'
      ImageIndex = 32
    end
    object N41: TMenuItem
      Caption = '-'
    end
    object ToggleBreakpointPopupItem: TMenuItem
      Action = actBreakPoint
    end
    object AddWatchPopupItem: TMenuItem
      Action = actAddWatch
    end
    object Runtocursor1: TMenuItem
      Action = actRunToCursor
    end
    object N38: TMenuItem
      Caption = '-'
    end
    object AddToDoitem1: TMenuItem
      Action = actAddToDo
    end
    object N45: TMenuItem
      Caption = '-'
    end
    object CVS1: TMenuItem
      Caption = 'CVS'
      object mnuCVSUpdate3: TMenuItem
        Tag = 3
        Action = actCVSUpdate
      end
      object mnuCVSDiff3: TMenuItem
        Tag = 3
        Action = actCVSDiff
      end
      object N60: TMenuItem
        Caption = '-'
      end
      object mnuCVSCommit3: TMenuItem
        Tag = 3
        Action = actCVSCommit
      end
      object N47: TMenuItem
        Caption = '-'
      end
      object mnuCVSLog3: TMenuItem
        Tag = 3
        Action = actCVSLog
      end
    end
    object N50: TMenuItem
      Caption = '-'
    end
    object mnuFileProps: TMenuItem
      Action = actFileProperties
    end
  end
  object UnitPopup: TPopupMenu
    MenuAnimation = [maBottomToTop]
    Left = 11
    Top = 135
    object RemoveFilefromprojectPopItem: TMenuItem
      Action = actUnitRemove
    end
    object RenamefilePopItem: TMenuItem
      Action = actUnitRename
    end
    object N30: TMenuItem
      Caption = '-'
    end
    object OpenPopItem: TMenuItem
      Action = actUnitOpen
    end
    object mnuOpenWith: TMenuItem
      Caption = 'Open with'
      OnClick = mnuOpenWithClick
    end
    object ClosefilePopItem: TMenuItem
      Action = actUnitClose
    end
    object N40: TMenuItem
      Caption = '-'
    end
    object Addfile1: TMenuItem
      Action = actProjectAdd
    end
    object N44: TMenuItem
      Caption = '-'
    end
    object Addfolder1: TMenuItem
      Action = actProjectNewFolder
    end
    object Renamefolder1: TMenuItem
      Action = actProjectRenameFolder
    end
    object Removefolder1: TMenuItem
      Action = actProjectRemoveFolder
    end
    object N49: TMenuItem
      Caption = '-'
    end
    object CVS3: TMenuItem
      Caption = 'CVS'
      object mnuCVSUpdate2: TMenuItem
        Tag = 2
        Action = actCVSUpdate
      end
      object mnuCVSDiff2: TMenuItem
        Tag = 2
        Action = actCVSDiff
      end
      object N52: TMenuItem
        Caption = '-'
      end
      object mnuCVSCommit2: TMenuItem
        Tag = 2
        Action = actCVSCommit
      end
      object N51: TMenuItem
        Caption = '-'
      end
      object mnuCVSLog2: TMenuItem
        Tag = 2
        Action = actCVSLog
      end
      object N62: TMenuItem
        Caption = '-'
      end
      object mnuCVSAdd2: TMenuItem
        Tag = 2
        Action = actCVSAdd
      end
      object mnuCVSRemove2: TMenuItem
        Tag = 2
        Action = actCVSRemove
      end
    end
    object N54: TMenuItem
      Caption = '-'
    end
    object mnuUnitProperties: TMenuItem
      Action = actFileProperties
    end
  end
  object ProjectPopup: TPopupMenu
    MenuAnimation = [maBottomToTop]
    Left = 11
    Top = 99
    object NewunitinprojectPopItem: TMenuItem
      Tag = 2
      Action = actProjectNew
    end
    object AddtoprojectPopItem: TMenuItem
      Action = actProjectAdd
    end
    object RemovefromprojectPopItem: TMenuItem
      Action = actProjectRemove
    end
    object N39: TMenuItem
      Caption = '-'
    end
    object Newfolder1: TMenuItem
      Action = actProjectNewFolder
    end
    object MenuItem18: TMenuItem
      Caption = '-'
    end
    object ProjectoptionsPopItem: TMenuItem
      Action = actProjectOptions
    end
    object N48: TMenuItem
      Caption = '-'
    end
    object CVS2: TMenuItem
      Caption = 'CVS'
      object mnuCVSUpdate1: TMenuItem
        Tag = 1
        Action = actCVSUpdate
      end
      object mnuCVSDiff1: TMenuItem
        Tag = 1
        Action = actCVSDiff
      end
      object N59: TMenuItem
        Caption = '-'
      end
      object mnuCVSCommit1: TMenuItem
        Tag = 1
        Action = actCVSCommit
      end
      object N55: TMenuItem
        Caption = '-'
      end
      object mnuCVSLog1: TMenuItem
        Tag = 1
        Action = actCVSLog
      end
    end
  end
  object alMain: TActionList
    Images = dmMain.MenuImages_NewLook
    Left = 624
    Top = 164
    object actViewCPU: TAction
      Category = 'Debug'
      Caption = '&View CPU Window'
      OnExecute = ViewCPUItemClick
      OnUpdate = actUpdateDebuggerRunning
    end
    object actNewSource: TAction
      Tag = 1
      Category = 'File'
      Caption = '&Source File'
      ImageIndex = 1
      ShortCut = 16462
      OnExecute = actNewSourceExecute
    end
    object actNewProject: TAction
      Tag = 2
      Category = 'File'
      Caption = '&Project...'
      ImageIndex = 0
      OnExecute = actNewProjectExecute
    end
    object actRunToCursor: TAction
      Category = 'Debug'
      Caption = 'Run to &cursor'
      ImageIndex = 24
      ShortCut = 16499
      OnExecute = actRunToCursorExecute
      OnUpdate = actUpdateEmptyEditor
    end
    object actNewRes: TAction
      Tag = 3
      Category = 'File'
      Caption = '&Resource File'
      ImageIndex = 2
      OnExecute = actNewResExecute
    end
    object actNewTemplate: TAction
      Tag = 4
      Category = 'File'
      Caption = '&Template...'
      ImageIndex = 3
      OnExecute = actNewTemplateExecute
      OnUpdate = actNewTemplateUpdate
    end
    object actOpen: TAction
      Tag = 1
      Category = 'File'
      Caption = '&Open project or file...'
      ImageIndex = 4
      ShortCut = 16463
      OnExecute = actOpenExecute
    end
    object actHistoryClear: TAction
      Tag = 2
      Category = 'File'
      Caption = '&Clear History'
      ImageIndex = 5
      OnExecute = actHistoryClearExecute
    end
    object actSave: TAction
      Tag = 3
      Category = 'File'
      Caption = '&Save'
      ImageIndex = 6
      ShortCut = 16467
      OnExecute = actSaveExecute
      OnUpdate = actSaveUpdate
    end
    object actSaveAs: TAction
      Tag = 4
      Category = 'File'
      Caption = 'Save &As'
      ImageIndex = 7
      ShortCut = 16507
      OnExecute = actSaveAsExecute
      OnUpdate = actSaveAsUpdate
    end
    object actSaveProjectAs: TAction
      Category = 'File'
      Caption = 'Save project as...'
      ImageIndex = 7
      OnExecute = actSaveProjectAsExecute
      OnUpdate = actUpdateProject
    end
    object actSaveAll: TAction
      Tag = 5
      Category = 'File'
      Caption = 'Save A&ll'
      ImageIndex = 8
      OnExecute = actSaveAllExecute
      OnUpdate = actUpdatePageorProject
    end
    object actClose: TAction
      Tag = 7
      Category = 'File'
      Caption = '&Close'
      ImageIndex = 9
      ShortCut = 16471
      OnExecute = actCloseExecute
      OnUpdate = actUpdatePageCount
    end
    object actCloseAll: TAction
      Tag = 11
      Category = 'File'
      Caption = 'Close All'
      OnExecute = actCloseAllExecute
      OnUpdate = actUpdatePageCount
    end
    object actCloseProject: TAction
      Tag = 6
      Category = 'File'
      Caption = 'Close Project'
      OnExecute = actCloseProjectExecute
      OnUpdate = actUpdateProject
    end
    object actXHTML: TAction
      Tag = 1
      Category = 'File'
      Caption = 'to &HTML'
      OnExecute = actXHTMLExecute
      OnUpdate = actUpdatePageCount
    end
    object actXRTF: TAction
      Tag = 2
      Category = 'File'
      Caption = 'to &RTF'
      OnExecute = actXRTFExecute
      OnUpdate = actUpdatePageCount
    end
    object actXProject: TAction
      Tag = 3
      Category = 'File'
      Caption = '&Project to HTML'
      OnExecute = actXProjectExecute
      OnUpdate = actUpdateProject
    end
    object actPrint: TAction
      Tag = 8
      Category = 'File'
      Caption = '&Print'
      ImageIndex = 10
      ShortCut = 16464
      OnExecute = actPrintExecute
      OnUpdate = actUpdateEmptyEditor
    end
    object actPrintSU: TAction
      Tag = 9
      Category = 'File'
      Caption = 'Prin&ter Setup...'
      OnExecute = actPrintSUExecute
    end
    object actExit: TAction
      Tag = 10
      Category = 'File'
      Caption = 'E&xit Dev-C++'
      ImageIndex = 11
      OnExecute = actExitExecute
    end
    object actUndo: TAction
      Tag = 1
      Category = 'Edit'
      Caption = '&Undo'
      ImageIndex = 13
      ShortCut = 16474
      OnExecute = actUndoExecute
      OnUpdate = actUndoUpdate
    end
    object actRedo: TAction
      Tag = 2
      Category = 'Edit'
      Caption = '&Redo'
      ImageIndex = 14
      ShortCut = 24666
      OnExecute = actRedoExecute
      OnUpdate = actRedoUpdate
    end
    object actCut: TAction
      Tag = 3
      Category = 'Edit'
      Caption = 'C&ut'
      ImageIndex = 15
      ShortCut = 16472
      OnExecute = actCutExecute
      OnUpdate = actCutUpdate
    end
    object actCopy: TAction
      Tag = 4
      Category = 'Edit'
      Caption = '&Copy'
      ImageIndex = 16
      ShortCut = 16451
      OnExecute = actCopyExecute
      OnUpdate = actCopyUpdate
    end
    object actPaste: TAction
      Tag = 5
      Category = 'Edit'
      Caption = '&Paste'
      ImageIndex = 17
      ShortCut = 16470
      OnExecute = actPasteExecute
      OnUpdate = actPasteUpdate
    end
    object actSelectAll: TAction
      Tag = 6
      Category = 'Edit'
      Caption = '&Select All'
      ShortCut = 16449
      OnExecute = actSelectAllExecute
      OnUpdate = actUpdateEmptyEditor
    end
    object actDelete: TAction
      Category = 'Edit'
      Caption = 'Delete'
      OnExecute = actDeleteExecute
    end
    object actFind: TAction
      Tag = 1
      Category = 'Search'
      Caption = '&Find'
      ImageIndex = 21
      ShortCut = 16454
      OnExecute = actFindExecute
      OnUpdate = actUpdateEmptyEditor
    end
    object actFindAll: TAction
      Tag = 2
      Category = 'Search'
      Caption = 'Fin&d in all Files'
      ShortCut = 41030
      OnExecute = actFindAllExecute
      OnUpdate = actUpdatePageorProject
    end
    object actReplace: TAction
      Tag = 3
      Category = 'Search'
      Caption = '&Replace'
      ImageIndex = 22
      ShortCut = 16466
      OnExecute = actReplaceExecute
      OnUpdate = actUpdateEmptyEditor
    end
    object actFindNext: TAction
      Tag = 4
      Category = 'Search'
      Caption = '&Search Again'
      ImageIndex = 23
      ShortCut = 114
      OnExecute = actFindNextExecute
      OnUpdate = actFindNextUpdate
    end
    object actIncremental: TAction
      Category = 'Search'
      Caption = 'Incremental Search'
      ShortCut = 16457
      OnExecute = actIncrementalExecute
      OnUpdate = actUpdateEmptyEditor
    end
    object actGoto: TAction
      Tag = 5
      Category = 'Search'
      Caption = '&Go to line...'
      ImageIndex = 24
      ShortCut = 16455
      OnExecute = actGotoExecute
      OnUpdate = actUpdateEmptyEditor
    end
    object actProjectManager: TAction
      Category = 'View'
      AutoCheck = True
      Caption = '&Project Manager'
      OnExecute = actProjectManagerExecute
    end
    object actStatusbar: TAction
      Category = 'View'
      AutoCheck = True
      Caption = '&Statusbar'
      OnExecute = actStatusbarExecute
    end
    object actCompOutput: TAction
      Category = 'View'
      AutoCheck = True
      Caption = '&Always show Compiler Output'
      GroupIndex = 2
      OnExecute = actCompOutputExecute
    end
    object actCompOnNeed: TAction
      Category = 'View'
      AutoCheck = True
      Caption = '&Show only when needed'
      GroupIndex = 2
      OnExecute = actCompOnNeedExecute
    end
    object actProjectNew: TAction
      Tag = 1
      Category = 'Project'
      Caption = '&New Unit'
      ImageIndex = 1
      OnExecute = actProjectNewExecute
      OnUpdate = actUpdateProject
    end
    object actProjectAdd: TAction
      Tag = 2
      Category = 'Project'
      Caption = '&Add file...'
      ImageIndex = 25
      OnExecute = actProjectAddExecute
      OnUpdate = actUpdateProject
    end
    object actProjectRemove: TAction
      Tag = 3
      Category = 'Project'
      Caption = '&Remove file...'
      ImageIndex = 26
      OnExecute = actProjectRemoveExecute
      OnUpdate = actUpdateProject
    end
    object actProjectOptions: TAction
      Tag = 5
      Category = 'Project'
      Caption = '&Options...'
      ImageIndex = 27
      ShortCut = 32848
      OnExecute = actProjectOptionsExecute
      OnUpdate = actUpdateProject
    end
    object actProjectMakeFile: TAction
      Category = 'Project'
      Caption = 'Edit &Makefile'
      OnExecute = actProjectMakeFileExecute
      OnUpdate = actUpdateProject
    end
    object actProjectSource: TAction
      Tag = 6
      Category = 'Project'
      Caption = 'Source'
      OnExecute = actProjectSourceExecute
      OnUpdate = actUpdateProject
    end
    object actCompile: TAction
      Tag = 1
      Category = 'Execute'
      Caption = '&Compile'
      ImageIndex = 28
      ShortCut = 16504
      OnExecute = actCompileExecute
      OnUpdate = actCompileUpdate
    end
    object actRun: TAction
      Tag = 2
      Category = 'Execute'
      Caption = '&Run'
      ImageIndex = 31
      ShortCut = 16505
      OnExecute = actRunExecute
      OnUpdate = actRunUpdate
    end
    object actCompRun: TAction
      Tag = 3
      Category = 'Execute'
      Caption = 'Compile &and Run'
      ImageIndex = 33
      ShortCut = 120
      OnExecute = actCompRunExecute
      OnUpdate = actRunUpdate
    end
    object actRebuild: TAction
      Tag = 4
      Category = 'Execute'
      Caption = 'R&ebuild All'
      ImageIndex = 30
      ShortCut = 16506
      OnExecute = actRebuildExecute
      OnUpdate = actCompileUpdate
    end
    object actClean: TAction
      Tag = 5
      Category = 'Execute'
      Caption = 'C&lean'
      OnExecute = actCleanExecute
      OnUpdate = actCompileUpdate
    end
    object actDebug: TAction
      Tag = 6
      Category = 'Debug'
      Caption = '&Debug'
      ImageIndex = 32
      ShortCut = 119
      OnExecute = actDebugExecute
      OnUpdate = actDebugUpdate
    end
    object actCompOptions: TAction
      Tag = 1
      Category = 'Tools'
      Caption = '&Compiler Options...'
      ImageIndex = 34
      OnExecute = actCompOptionsExecute
    end
    object actEnviroOptions: TAction
      Tag = 2
      Category = 'Tools'
      Caption = '&Environment Options...'
      ImageIndex = 35
      OnExecute = actEnviroOptionsExecute
    end
    object actEditorOptions: TAction
      Tag = 3
      Category = 'Tools'
      Caption = 'E&ditor Options...'
      ImageIndex = 36
      OnExecute = actEditorOptionsExecute
    end
    object actConfigTools: TAction
      Tag = 4
      Category = 'Tools'
      Caption = 'Configure &Tools...'
      ImageIndex = 37
      OnExecute = actConfigToolsExecute
    end
    object actFullScreen: TAction
      Tag = 1
      Category = 'Window'
      AutoCheck = True
      Caption = '&Full screen mode'
      ImageIndex = 38
      ShortCut = 123
      OnExecute = actFullScreenExecute
    end
    object actNext: TAction
      Tag = 2
      Category = 'Window'
      Caption = '&Next'
      ImageIndex = 39
      ShortCut = 117
      OnExecute = actNextExecute
      OnUpdate = actUpdatePageCount
    end
    object actPrev: TAction
      Tag = 3
      Category = 'Window'
      Caption = '&Previous'
      ImageIndex = 40
      ShortCut = 116
      OnExecute = actPrevExecute
      OnUpdate = actUpdatePageCount
    end
    object actUpdateCheck: TAction
      Category = 'Help'
      Caption = '&Check for Updates/Packages'
      ImageIndex = 41
      OnExecute = actUpdateCheckExecute
    end
    object actAbout: TAction
      Category = 'Help'
      Caption = 'About...'
      ImageIndex = 42
      OnExecute = actAboutExecute
    end
    object actHelpCustomize: TAction
      Category = 'Help'
      Caption = 'Customize...'
    end
    object actUnitRemove: TAction
      Tag = 1
      Category = 'Project'
      Caption = '&Remove from project'
      OnExecute = actUnitRemoveExecute
      OnUpdate = actUpdateProject
    end
    object actUnitRename: TAction
      Tag = 2
      Category = 'Project'
      Caption = 'Re&name file'
      OnExecute = actUnitRenameExecute
      OnUpdate = actUpdateProject
    end
    object actUnitHeader: TAction
      Tag = 5
      Category = 'Project'
      Caption = 'Open &Header'
      OnUpdate = actUpdateProject
    end
    object actUnitOpen: TAction
      Tag = 4
      Category = 'Project'
      Caption = '&Open'
      OnExecute = actUnitOpenExecute
      OnUpdate = actUpdateProject
    end
    object actUnitClose: TAction
      Tag = 3
      Category = 'Project'
      Caption = '&Close'
      OnExecute = actUnitCloseExecute
      OnUpdate = actUpdateProject
    end
    object actShowBars: TAction
      Caption = 'Show Toolbars'
      ShortCut = 122
      OnExecute = actShowBarsExecute
    end
    object actBreakPoint: TAction
      Category = 'Debug'
      Caption = 'Toggle Breakpoint'
      ShortCut = 16500
      OnExecute = actBreakPointExecute
      OnUpdate = actUpdateEmptyEditor
    end
    object actAddWatch: TAction
      Category = 'Debug'
      Caption = '&Add watch'
      ImageIndex = 21
      ShortCut = 115
      OnExecute = actAddWatchExecute
      OnUpdate = actUpdatePageCount
    end
    object actEditWatch: TAction
      Category = 'Debug'
      Caption = '&Edit watch'
      ImageIndex = 36
    end
    object actNextStep: TAction
      Category = 'Debug'
      Caption = '&Next Step'
      ImageIndex = 18
      ShortCut = 118
      OnExecute = actNextStepExecute
      OnUpdate = actUpdateDebuggerRunning
    end
    object actStepOver: TAction
      Category = 'Debug'
      Caption = '&Continue'
      ImageIndex = 14
      ShortCut = 16502
      OnExecute = actStepOverExecute
      OnUpdate = actUpdateDebuggerRunning
    end
    object actWatchItem: TAction
      Category = 'Debug'
      Caption = '&Watch variables'
      ShortCut = 16471
      OnExecute = actWatchItemExecute
      OnUpdate = actUpdatePageorProject
    end
    object actRemoveWatch: TAction
      Category = 'Debug'
      Caption = '&Remove Watch'
      ImageIndex = 5
      OnExecute = actRemoveWatchExecute
    end
    object actStopExecute: TAction
      Category = 'Debug'
      Caption = 'Stop Execution'
      ImageIndex = 11
      ShortCut = 32881
      OnExecute = actForceStopExecuteExecute
      OnUpdate = actUpdateDebuggerRunning
    end
    object actFileMenu: TAction
      Caption = '&File'
      OnExecute = actFileMenuExecute
    end
    object actEditMenu: TAction
      Caption = '&Edit'
      OnExecute = actFileMenuExecute
    end
    object actSearchMenu: TAction
      Caption = '&Search'
      OnExecute = actFileMenuExecute
    end
    object actViewMenu: TAction
      Caption = '&View'
      OnExecute = actFileMenuExecute
    end
    object actProjectMenu: TAction
      Caption = '&Project'
      OnExecute = actFileMenuExecute
    end
    object actExecuteMenu: TAction
      Caption = 'E&xecute'
      OnExecute = actFileMenuExecute
    end
    object actDebugMenu: TAction
      Caption = '&Debug'
      OnExecute = actFileMenuExecute
    end
    object actToolsMenu: TAction
      Caption = '&Tools'
      OnExecute = actToolsMenuExecute
    end
    object actWindowMenu: TAction
      Caption = '&Window'
      OnExecute = actWindowMenuExecute
    end
    object actHelpMenu: TAction
      Caption = '&Help'
      OnExecute = actFileMenuExecute
    end
    object actSwapHeaderSource: TAction
      Category = 'Edit'
      Caption = '&Swap header/source'
      OnExecute = actSwapHeaderSourceExecute
      OnUpdate = actSwapHeaderSourceUpdate
    end
    object actSyntaxCheck: TAction
      Category = 'Execute'
      Caption = '&Syntax Check'
      OnExecute = actSyntaxCheckExecute
      OnUpdate = actCompileUpdate
    end
    object actConfigShortcuts: TAction
      Category = 'Tools'
      Caption = 'Configure &shortcuts'
      ImageIndex = 31
      OnExecute = actConfigShortcutsExecute
    end
    object actProgramReset: TAction
      Category = 'Execute'
      Caption = 'Program reset'
      ShortCut = 32881
      OnExecute = actProgramResetExecute
      OnUpdate = actProgramResetUpdate
    end
    object actComment: TAction
      Category = 'Edit'
      Caption = 'Comment'
      ShortCut = 49342
      OnExecute = actCommentExecute
      OnUpdate = actUpdateEmptyEditor
    end
    object actUncomment: TAction
      Category = 'Edit'
      Caption = 'Uncomment'
      ShortCut = 49340
      OnExecute = actUncommentExecute
      OnUpdate = actUpdateEmptyEditor
    end
    object actIndent: TAction
      Category = 'Edit'
      Caption = 'Indent'
      ShortCut = 9
      OnExecute = actIndentExecute
      OnUpdate = actUpdateEmptyEditor
    end
    object actUnindent: TAction
      Category = 'Edit'
      Caption = 'Unindent'
      ShortCut = 8201
      OnExecute = actUnindentExecute
      OnUpdate = actUpdateEmptyEditor
    end
    object actGotoFunction: TAction
      Category = 'Search'
      Caption = 'Goto function'
      ImageIndex = 44
      ShortCut = 24647
      OnExecute = actGotoFunctionExecute
      OnUpdate = actUpdateEmptyEditor
    end
    object actBrowserGotoDecl: TAction
      Category = 'ClassBrowser'
      Caption = 'Goto declaration'
      OnExecute = actBrowserGotoDeclExecute
      OnUpdate = actBrowserGotoDeclUpdate
    end
    object actBrowserGotoImpl: TAction
      Category = 'ClassBrowser'
      Caption = 'Goto implementation'
      OnExecute = actBrowserGotoImplExecute
      OnUpdate = actBrowserGotoImplUpdate
    end
    object actBrowserNewClass: TAction
      Category = 'ClassBrowser'
      Caption = 'New class'
      OnExecute = actBrowserNewClassExecute
      OnUpdate = actBrowserNewClassUpdate
    end
    object actBrowserNewMember: TAction
      Category = 'ClassBrowser'
      Caption = 'New member function'
      OnExecute = actBrowserNewMemberExecute
      OnUpdate = actBrowserNewMemberUpdate
    end
    object actBrowserNewVar: TAction
      Category = 'ClassBrowser'
      Caption = 'New variable'
      OnExecute = actBrowserNewVarExecute
      OnUpdate = actBrowserNewVarUpdate
    end
    object actBrowserViewAll: TAction
      Category = 'ClassBrowser'
      Caption = 'All files'
      Checked = True
      OnExecute = actBrowserViewAllExecute
      OnUpdate = actBrowserViewAllUpdate
    end
    object actBrowserViewProject: TAction
      Category = 'ClassBrowser'
      Caption = 'Project files'
      OnExecute = actBrowserViewProjectExecute
      OnUpdate = actBrowserViewAllUpdate
    end
    object actBrowserViewCurrent: TAction
      Category = 'ClassBrowser'
      Caption = 'Current file'
      OnExecute = actBrowserViewCurrentExecute
      OnUpdate = actBrowserViewAllUpdate
    end
    object actProfileProject: TAction
      Category = 'Execute'
      Caption = 'Profile analysis'
      ImageIndex = 43
      OnExecute = actProfileProjectExecute
      OnUpdate = actRunUpdate
    end
    object actBrowserAddFolder: TAction
      Category = 'ClassBrowser'
      Caption = 'Add folder'
      OnExecute = actBrowserAddFolderExecute
      OnUpdate = actBrowserAddFolderUpdate
    end
    object actBrowserRemoveFolder: TAction
      Category = 'ClassBrowser'
      Caption = 'Remove folder'
      OnExecute = actBrowserRemoveFolderExecute
      OnUpdate = actBrowserAddFolderUpdate
    end
    object actBrowserRenameFolder: TAction
      Category = 'ClassBrowser'
      Caption = 'Rename folder'
      OnExecute = actBrowserRenameFolderExecute
      OnUpdate = actBrowserAddFolderUpdate
    end
    object actCloseAllButThis: TAction
      Category = 'File'
      Caption = 'Close all except this'
      OnExecute = actCloseAllButThisExecute
      OnUpdate = actUpdatePageCount
    end
    object actStepSingle: TAction
      Category = 'Debug'
      Caption = 'Step into'
      OnExecute = actStepSingleExecute
      OnUpdate = actUpdateDebuggerRunning
    end
    object actFileProperties: TAction
      Category = 'File'
      Caption = 'Properties'
      OnExecute = actFilePropertiesExecute
      OnUpdate = actUpdatePageCount
    end
    object actViewToDoList: TAction
      Category = 'View'
      Caption = 'To-Do list...'
      OnExecute = actViewToDoListExecute
      OnUpdate = actUpdatePageorProject
    end
    object actAddToDo: TAction
      Category = 'Edit'
      Caption = 'Add To-Do item...'
      ShortCut = 24660
      OnExecute = actAddToDoExecute
      OnUpdate = actUpdatePageorProject
    end
    object actProjectNewFolder: TAction
      Category = 'Project'
      Caption = 'Add folder'
      OnExecute = actProjectNewFolderExecute
      OnUpdate = actUpdateProject
    end
    object actProjectRemoveFolder: TAction
      Category = 'Project'
      Caption = 'Remove folder'
      OnExecute = actProjectRemoveFolderExecute
      OnUpdate = actUpdateProject
    end
    object actProjectRenameFolder: TAction
      Category = 'Project'
      Caption = 'Rename folder'
      OnExecute = actProjectRenameFolderExecute
      OnUpdate = actUpdateProject
    end
    object actImportMSVC: TAction
      Category = 'File'
      Caption = 'Import MS Visual C++ project'
      OnExecute = actImportMSVCExecute
    end
    object actExecParams: TAction
      Category = 'Execute'
      Caption = 'Parameters...'
      OnExecute = actExecParamsExecute
      OnUpdate = actDebugUpdate
    end
    object actShowTips: TAction
      Category = 'Help'
      Caption = 'Tips'
      OnExecute = actShowTipsExecute
    end
    object actBrowserUseColors: TAction
      Category = 'ClassBrowser'
      Caption = 'Use colors'
      Checked = True
      OnExecute = actBrowserUseColorsExecute
    end
    object actAbortCompilation: TAction
      Category = 'Execute'
      Caption = 'Abort compilation'
      OnExecute = actAbortCompilationExecute
      OnUpdate = actAbortCompilationUpdate
    end
    object actCVSImport: TAction
      Category = 'CVS'
      Caption = 'Import'
      OnExecute = actCVSImportExecute
    end
    object actCVSCheckout: TAction
      Category = 'CVS'
      Caption = 'Checkout'
      OnExecute = actCVSCheckoutExecute
    end
    object actCVSUpdate: TAction
      Category = 'CVS'
      Caption = 'Update'
      OnExecute = actCVSUpdateExecute
      OnUpdate = actUpdatePageorProject
    end
    object actCVSCommit: TAction
      Category = 'CVS'
      Caption = 'Commit'
      OnExecute = actCVSCommitExecute
      OnUpdate = actUpdatePageorProject
    end
    object actCVSDiff: TAction
      Category = 'CVS'
      Caption = 'Diff'
      OnExecute = actCVSDiffExecute
      OnUpdate = actUpdatePageorProject
    end
    object actCVSLog: TAction
      Category = 'CVS'
      Caption = 'Log'
      OnExecute = actCVSLogExecute
      OnUpdate = actUpdatePageorProject
    end
    object actCVSAdd: TAction
      Category = 'CVS'
      Caption = 'Add'
      OnExecute = actCVSAddExecute
      OnUpdate = actUpdatePageorProject
    end
    object actCVSRemove: TAction
      Category = 'CVS'
      Caption = 'Remove'
      OnExecute = actCVSRemoveExecute
      OnUpdate = actUpdatePageorProject
    end
    object actBrowserShowInherited: TAction
      Category = 'ClassBrowser'
      Caption = 'Show inherited members'
      OnExecute = actBrowserShowInheritedExecute
    end
    object actCVSLogin: TAction
      Category = 'CVS'
      Caption = 'Login'
      OnExecute = actCVSLoginExecute
    end
    object actCVSLogout: TAction
      Category = 'CVS'
      Caption = 'Logout'
      OnExecute = actCVSLogoutExecute
    end
    object actCompileCurrentFile: TAction
      Category = 'Execute'
      Caption = 'Compile current file'
      ImageIndex = 28
      ShortCut = 24696
      OnExecute = actCompileCurrentFileExecute
      OnUpdate = actCompileCurrentFileUpdate
    end
    object actAttachProcess: TAction
      Category = 'Debug'
      Caption = 'Attach to process...'
      OnExecute = actAttachProcessExecute
      OnUpdate = actAttachProcessUpdate
    end
    object actModifyWatch: TAction
      Category = 'Debug'
      Caption = '&Modify value'
      ImageIndex = 37
      OnExecute = actModifyWatchExecute
      OnUpdate = actModifyWatchUpdate
    end
    object actDeleteProfileProject: TAction
      Category = 'Execute'
      Caption = 'actDeleteProfileProject'
      ImageIndex = 11
      OnExecute = actDeleteProfileProjectExecute
      OnUpdate = actDeleteProfRunUpdate
    end
    object actGotoDeclEditor: TAction
      Category = 'ClassBrowser'
      Caption = 'actGotoDeclEditor'
      OnExecute = actGotoImplDeclEditorExecute
    end
    object actGotoImplEditor: TAction
      Category = 'ClassBrowser'
      Caption = 'actGotoImplEditor'
      OnExecute = actGotoImplDeclEditorExecute
    end
    object actHideFSBar: TAction
      Category = 'Window'
      Caption = 'actHideFSBar'
      ShortCut = 121
      OnExecute = actHideFSBarExecute
    end
  end
  object ApplicationEvents1: TApplicationEvents
    OnDeactivate = ApplicationEvents1Deactivate
    OnIdle = ApplicationEvents1Idle
    Left = 626
    Top = 202
  end
  object MessagePopup: TPopupMenu
    OnPopup = MessagePopupPopup
    Left = 203
    Top = 451
    object MsgCopyItem: TMenuItem
      Caption = '&Copy'
      ShortCut = 16451
      OnClick = actMsgCopyExecute
    end
    object MsgCopyAllItem: TMenuItem
      Caption = 'Copy &All'
      ShortCut = 24643
      OnClick = actMsgCopyAllExecute
    end
    object MsgSaveAllItem: TMenuItem
      Caption = 'Save All'
      ShortCut = 16467
      OnClick = actMsgSaveAllExecute
    end
    object MsgClearItem: TMenuItem
      Caption = 'C&lear'
      OnClick = actMsgClearExecute
    end
  end
  object CppTokenizer1: TCppTokenizer
    LogTokens = False
    Left = 60
    Top = 132
  end
  object CppParser1: TCppParser
    Enabled = True
    OnTotalProgress = CppParser1TotalProgress
    Tokenizer = CppTokenizer1
    ParseLocalHeaders = False
    ParseGlobalHeaders = False
    LogStatements = False
    OnStartParsing = CppParser1StartParsing
    OnEndParsing = CppParser1EndParsing
    Left = 60
    Top = 196
  end
  object CodeCompletion1: TCodeCompletion
    Parser = CppParser1
    Color = clWhite
    Width = 320
    Height = 240
    Enabled = True
    HintTimeout = 4000
    MinWidth = 256
    MinHeight = 128
    MaxWidth = 640
    MaxHeight = 480
    OnResize = CodeCompletion1Resize
    OnlyGlobals = False
    CurrentClass = 0
    Left = 60
    Top = 164
  end
  object devShortcuts1: TdevShortcuts
    Filename = 'devshortcuts.cfg'
    AlternateColor = 14737632
    MultiLangStrings.Caption = 'Configure Shortcuts'
    MultiLangStrings.Title = ' Click on an item and press the shortcut you desire!'
    MultiLangStrings.Tip = 'Tip: press "Escape" to clear a shortcut...'
    MultiLangStrings.HeaderEntry = 'Menu entry'
    MultiLangStrings.HeaderShortcut = 'Shortcut assigned'
    MultiLangStrings.OK = 'OK'
    MultiLangStrings.Cancel = 'Cancel'
    Left = 628
    Top = 280
  end
  object BrowserPopup: TPopupMenu
    Left = 56
    Top = 100
    object mnuBrowserGotoDecl: TMenuItem
      Action = actBrowserGotoDecl
    end
    object mnuBrowserGotoImpl: TMenuItem
      Action = actBrowserGotoImpl
      Caption = 'Goto Definition'
      Default = True
    end
    object mnuBrowserSep1: TMenuItem
      Caption = '-'
    end
    object mnuBrowserNewClass: TMenuItem
      Action = actBrowserNewClass
    end
    object mnuBrowserSep2: TMenuItem
      Caption = '-'
    end
    object mnuBrowserNewMember: TMenuItem
      Action = actBrowserNewMember
    end
    object mnuBrowserNewVariable: TMenuItem
      Action = actBrowserNewVar
    end
    object N31: TMenuItem
      Caption = '-'
    end
    object mnuBrowserAddFolder: TMenuItem
      Action = actBrowserAddFolder
    end
    object mnuBrowserRemoveFolder: TMenuItem
      Action = actBrowserRemoveFolder
    end
    object mnuBrowserRenameFolder: TMenuItem
      Action = actBrowserRenameFolder
    end
    object mnuBrowserSep3: TMenuItem
      Caption = '-'
    end
    object mnuBrowserViewMode: TMenuItem
      Caption = 'View mode'
      object mnuBrowserViewAll: TMenuItem
        Action = actBrowserViewAll
        RadioItem = True
      end
      object mnuBrowserViewProject: TMenuItem
        Action = actBrowserViewProject
      end
      object mnuBrowserViweCurrent: TMenuItem
        Action = actBrowserViewCurrent
        RadioItem = True
      end
      object N42: TMenuItem
        Caption = '-'
      end
      object Usecolors1: TMenuItem
        Action = actBrowserUseColors
      end
      object Showinheritedmembers1: TMenuItem
        Action = actBrowserShowInherited
      end
    end
  end
  object DebugVarsPopup: TPopupMenu
    OnPopup = DebugVarsPopupPopup
    Left = 104
    Top = 100
    object AddwatchPop: TMenuItem
      Action = actAddWatch
    end
    object ModifyWatchPop: TMenuItem
      Action = actModifyWatch
    end
    object N67: TMenuItem
      Caption = '-'
    end
    object RemoveWatchPop: TMenuItem
      Action = actRemoveWatch
    end
    object ClearallWatchPop: TMenuItem
      Caption = '&Clear all'
      OnClick = ClearallWatchPopClick
    end
  end
  object DevCppDDEServer: TDdeServerConv
    OnExecuteMacro = DevCppDDEServerExecuteMacro
    Left = 628
    Top = 240
  end
  object HelpPop: TPopupMenu
    Left = 422
    Top = 84
    object HelponDevPopupItem: TMenuItem
      Caption = '&Help on Dev-C++'
      OnClick = HelpMenuItemClick
    end
  end
end
