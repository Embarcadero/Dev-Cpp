object MainForm: TMainForm
  Left = 916
  Top = 169
  Caption = 
    'SVG Icon ImageList Demo - Copyright (c) Ethea S.r.l. - Apache 2.' +
    '0 Open Source License'
  ClientHeight = 547
  ClientWidth = 709
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 400
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter: TSplitter
    Left = 626
    Top = 38
    Height = 509
    Align = alRight
    AutoSnap = False
    MinSize = 80
    ExplicitLeft = 9
    ExplicitTop = 9
    ExplicitHeight = 427
  end
  object Panel1: TPanel
    Left = 0
    Top = 38
    Width = 202
    Height = 509
    Align = alLeft
    TabOrder = 0
    object SelectThemeRadioGroup: TRadioGroup
      Left = 1
      Top = 1
      Width = 200
      Height = 272
      Align = alClient
      Caption = 'Select Theme/Color'
      TabOrder = 0
      OnClick = SelectThemeRadioGroupClick
    end
    object LoadGroupBox: TGroupBox
      Left = 1
      Top = 273
      Width = 200
      Height = 59
      Align = alBottom
      Caption = 'Load SVG from disk'
      TabOrder = 1
      object BuildFromFilesButton: TButton
        Left = 3
        Top = 23
        Width = 189
        Height = 30
        Caption = 'Build from SVG files...'
        TabOrder = 0
        OnClick = BuildFromFilesButtonClick
      end
    end
    object SliderPanel: TPanel
      Left = 1
      Top = 373
      Width = 200
      Height = 62
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 3
      object IconSizeLabel: TLabel
        Left = 8
        Top = 3
        Width = 51
        Height = 13
        Caption = 'Icons size:'
      end
      object TrackBar: TTrackBar
        Left = 0
        Top = 23
        Width = 200
        Height = 39
        Align = alBottom
        Max = 128
        Min = 12
        Frequency = 8
        Position = 32
        PositionToolTip = ptBottom
        TabOrder = 0
        OnChange = TrackBarChange
      end
    end
    object ButtonsPanel: TPanel
      Left = 1
      Top = 332
      Width = 200
      Height = 41
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
      object ClearButton: TButton
        Left = 5
        Top = 5
        Width = 76
        Height = 30
        Caption = 'Clear Icons'
        TabOrder = 0
        OnClick = ClearButtonClick
      end
      object ShowImageEditorButton: TButton
        Left = 86
        Top = 5
        Width = 106
        Height = 30
        Caption = 'Show Image Editor'
        TabOrder = 1
        OnClick = ShowImageEditorButtonClick
      end
    end
    object ColorGroupBox: TGroupBox
      Left = 1
      Top = 435
      Width = 200
      Height = 73
      Align = alBottom
      Caption = 'Fixed color'
      TabOrder = 4
      object FixedColorComboBox: TColorBox
        Left = 10
        Top = 19
        Width = 178
        Height = 22
        DefaultColorColor = clDefault
        NoneColorColor = clNone
        Selected = clDefault
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames, cbCustomColors]
        TabOrder = 0
        OnSelect = FixedColorComboBoxSelect
      end
      object GrayScaleCheckBox: TCheckBox
        Left = 10
        Top = 50
        Width = 97
        Height = 17
        Caption = 'GrayScale'
        TabOrder = 1
        OnClick = GrayScaleCheckBoxClick
      end
    end
  end
  object TopToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 709
    Height = 38
    AutoSize = True
    ButtonHeight = 38
    ButtonWidth = 39
    Images = SVGIconVirtualImageList
    TabOrder = 1
    Transparent = False
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      Action = DisabledAction
    end
    object ToolButton2: TToolButton
      Left = 39
      Top = 0
      Action = DeleteIconAction
    end
    object ToolButton3: TToolButton
      Left = 78
      Top = 0
      ImageIndex = 2
    end
    object ToolButton4: TToolButton
      Left = 117
      Top = 0
      ImageIndex = 3
    end
    object ToolButton5: TToolButton
      Left = 156
      Top = 0
      ImageIndex = 4
    end
    object ToolButton6: TToolButton
      Left = 195
      Top = 0
      Action = ChangeIconAction
    end
    object ToolButton7: TToolButton
      Left = 234
      Top = 0
      Caption = 'Change Color'
      Enabled = False
      ImageIndex = 7
      OnClick = ChangeColorActionExecute
    end
  end
  object paButtons: TPanel
    Left = 629
    Top = 38
    Width = 80
    Height = 509
    Align = alRight
    TabOrder = 2
    OnResize = paButtonsResize
    object SVGIconImage: TSVGIconImage
      Left = 1
      Top = 428
      Width = 78
      Height = 80
      Hint = 'Click left - right mouse button to change icon into SVGIconImage'
      Margins.Left = 10
      Margins.Top = 10
      Margins.Right = 10
      Margins.Bottom = 10
      AutoSize = False
      ImageList = SVGIconVirtualImageList
      ImageIndex = 100
      Align = alBottom
      OnMouseDown = SVGIconImageMouseDown
    end
    object DeleteButton: TButton
      Left = 3
      Top = 5
      Width = 73
      Height = 60
      Action = DeleteIconAction
      ImageAlignment = iaTop
      Images = SVGIconVirtualImageList
      TabOrder = 0
    end
    object ChangeIconButton: TButton
      Left = 3
      Top = 71
      Width = 73
      Height = 60
      Action = ChangeIconAction
      ImageAlignment = iaTop
      Images = SVGIconVirtualImageList
      TabOrder = 1
    end
    object NewFormButton: TButton
      Left = 3
      Top = 137
      Width = 73
      Height = 60
      Action = NewFormAction
      ImageAlignment = iaTop
      Images = SVGIconVirtualImageList
      TabOrder = 2
    end
  end
  object ClientPanel: TPanel
    Left = 202
    Top = 38
    Width = 424
    Height = 509
    Align = alClient
    TabOrder = 3
    object ImageListLabel: TLabel
      Left = 1
      Top = 209
      Width = 422
      Height = 13
      Align = alTop
      Alignment = taCenter
      Caption = 'Image List Icons Preview'
      ExplicitWidth = 119
    end
    object TreeView: TTreeView
      Left = 1
      Top = 1
      Width = 422
      Height = 208
      Align = alTop
      Images = SVGIconVirtualImageList
      Indent = 35
      TabOrder = 0
      Items.NodeData = {
        0303000000240000000100000001000000FFFFFFFFFFFFFFFF00000000000000
        000100000001036F006E0065002C0000000400000004000000FFFFFFFFFFFFFF
        FF00000000000000000000000001076F006E0065002D006F006E006500240000
        000200000002000000FFFFFFFFFFFFFFFF000000000000000002000000010374
        0077006F002C0000000500000005000000FFFFFFFFFFFFFFFF00000000000000
        00000000000107740077006F0020006F006E0065002C00000006000000070000
        0000000000FFFFFFFF0000000000000000000000000107740077006F00200074
        0077006F00280000000300000003000000FFFFFFFFFFFFFFFF00000000000000
        0000000000010574006800720065006500}
    end
    object ImageView: TListView
      Left = 1
      Top = 222
      Width = 422
      Height = 286
      Align = alClient
      Columns = <>
      IconOptions.AutoArrange = True
      Items.ItemData = {
        05CC0000000500000000000000FFFFFFFFFFFFFFFF00000000FFFFFFFF000000
        00034D0061006E0001000000FFFFFFFFFFFFFFFF00000000FFFFFFFF00000000
        0557006F006D0061006E0002000000FFFFFFFFFFFFFFFF00000000FFFFFFFF00
        00000008430061006C0065006E0064006100720003000000FFFFFFFFFFFFFFFF
        00000000FFFFFFFF000000000B49006E0066006F0072006D006100740069006F
        006E0004000000FFFFFFFFFFFFFFFF00000000FFFFFFFF000000000A43006100
        6C00630075006C00610074006F007200}
      LargeImages = SVGIconVirtualImageList
      SmallImages = SVGIconVirtualImageList
      TabOrder = 1
      OnSelectItem = ImageViewSelectItem
      ExplicitLeft = 5
      ExplicitTop = 242
      ExplicitWidth = 423
      ExplicitHeight = 272
    end
  end
  object ActionList: TActionList
    Images = SVGIconVirtualImageList
    Left = 248
    Top = 424
    object ChangeIconAction: TAction
      Category = 'Edit'
      Caption = 'Change icon'
      ImageIndex = 255
      OnExecute = ChangeIconActionExecute
    end
    object DeleteIconAction: TAction
      Category = 'Edit'
      Caption = 'Delete Icon'
      ImageIndex = 39
      OnExecute = DeleteIconActionExecute
    end
    object DisabledAction: TAction
      Category = 'Edit'
      Caption = 'Disabled'
      Enabled = False
      ImageIndex = 0
    end
    object NewFormAction: TAction
      Category = 'View'
      Caption = 'New Form'
      ImageIndex = 107
      OnExecute = NewFormActionExecute
    end
  end
  object ColorDialog: TColorDialog
    Left = 472
    Top = 136
  end
  object SVGIconVirtualImageList: TSVGIconVirtualImageList
    Size = 32
    ImageCollection = ImageDataModule.SVGIconImageCollection
    Left = 424
    Top = 336
  end
  object OpenDialog: TOpenPictureDialog
    Filter = 'Scalable Vector Graphics (*.svg)|*.svg'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 472
    Top = 72
  end
  object tmrTrackbar: TTimer
    Enabled = False
    Interval = 200
    OnTimer = tmrTrackbarTimer
    Left = 250
    Top = 486
  end
end
