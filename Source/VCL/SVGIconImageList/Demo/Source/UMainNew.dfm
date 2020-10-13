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
  OnAfterMonitorDpiChanged = FormAfterMonitorDpiChanged
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
    Images = VirtualImageList
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
      ImageList = VirtualImageList
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
      Images = VirtualImageList
      TabOrder = 0
    end
    object ChangeIconButton: TButton
      Left = 3
      Top = 71
      Width = 73
      Height = 60
      Action = ChangeIconAction
      ImageAlignment = iaTop
      Images = VirtualImageList
      TabOrder = 1
    end
    object NewFormButton: TButton
      Left = 3
      Top = 137
      Width = 73
      Height = 60
      Action = NewFormAction
      ImageAlignment = iaTop
      Images = VirtualImageList
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
      Images = VirtualImageList
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
      LargeImages = VirtualImageList
      SmallImages = VirtualImageList
      TabOrder = 1
      OnSelectItem = ImageViewSelectItem
      ExplicitLeft = 5
      ExplicitTop = 242
      ExplicitWidth = 423
      ExplicitHeight = 272
    end
  end
  object ActionList: TActionList
    Images = VirtualImageList
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
  object VirtualImageList: TVirtualImageList
    DisabledGrayscale = False
    DisabledSuffix = '_Disabled'
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'Business\businessman'
        Disabled = False
        Name = 'business\businessman'
      end
      item
        CollectionIndex = 1
        CollectionName = 'Business\businesswoman'
        Disabled = False
        Name = 'business\businesswoman'
      end
      item
        CollectionIndex = 2
        CollectionName = 'calendar'
        Disabled = False
        Name = 'calendar'
      end
      item
        CollectionIndex = 3
        CollectionName = 'about'
        Disabled = False
        Name = 'about'
      end
      item
        CollectionIndex = 4
        CollectionName = 'calculator'
        Disabled = False
        Name = 'calculator'
      end
      item
        CollectionIndex = 5
        CollectionName = 'cell_phone'
        Disabled = False
        Name = 'cell_phone'
      end
      item
        CollectionIndex = 6
        CollectionName = 'contacts'
        Disabled = False
        Name = 'contacts'
      end
      item
        CollectionIndex = 7
        CollectionName = 'advertising'
        Disabled = False
        Name = 'advertising'
      end
      item
        CollectionIndex = 8
        CollectionName = 'alphabetical_sorting_az'
        Disabled = False
        Name = 'alphabetical_sorting_az'
      end
      item
        CollectionIndex = 9
        CollectionName = 'alphabetical_sorting_za'
        Disabled = False
        Name = 'alphabetical_sorting_za'
      end
      item
        CollectionIndex = 10
        CollectionName = 'android_os'
        Disabled = False
        Name = 'android_os'
      end
      item
        CollectionIndex = 11
        CollectionName = 'answers'
        Disabled = False
        Name = 'answers'
      end
      item
        CollectionIndex = 12
        CollectionName = 'approval'
        Disabled = False
        Name = 'approval'
      end
      item
        CollectionIndex = 13
        CollectionName = 'approve'
        Disabled = False
        Name = 'approve'
      end
      item
        CollectionIndex = 14
        CollectionName = 'area_chart'
        Disabled = False
        Name = 'area_chart'
      end
      item
        CollectionIndex = 15
        CollectionName = 'assistant'
        Disabled = False
        Name = 'assistant'
      end
      item
        CollectionIndex = 16
        CollectionName = 'audio_file'
        Disabled = False
        Name = 'audio_file'
      end
      item
        CollectionIndex = 17
        CollectionName = 'automatic'
        Disabled = False
        Name = 'automatic'
      end
      item
        CollectionIndex = 18
        CollectionName = 'automotive'
        Disabled = False
        Name = 'automotive'
      end
      item
        CollectionIndex = 19
        CollectionName = 'bad_decision'
        Disabled = False
        Name = 'bad_decision'
      end
      item
        CollectionIndex = 20
        CollectionName = 'bar_chart'
        Disabled = False
        Name = 'bar_chart'
      end
      item
        CollectionIndex = 21
        CollectionName = 'bearish'
        Disabled = False
        Name = 'bearish'
      end
      item
        CollectionIndex = 22
        CollectionName = 'binoculars'
        Disabled = False
        Name = 'binoculars'
      end
      item
        CollectionIndex = 23
        CollectionName = 'biohazard'
        Disabled = False
        Name = 'biohazard'
      end
      item
        CollectionIndex = 24
        CollectionName = 'biomass'
        Disabled = False
        Name = 'biomass'
      end
      item
        CollectionIndex = 25
        CollectionName = 'biotech'
        Disabled = False
        Name = 'biotech'
      end
      item
        CollectionIndex = 26
        CollectionName = 'bookmark'
        Disabled = False
        Name = 'bookmark'
      end
      item
        CollectionIndex = 27
        CollectionName = 'briefcase'
        Disabled = False
        Name = 'briefcase'
      end
      item
        CollectionIndex = 28
        CollectionName = 'bullish'
        Disabled = False
        Name = 'bullish'
      end
      item
        CollectionIndex = 29
        CollectionName = 'Business\business'
        Disabled = False
        Name = 'business\business'
      end
      item
        CollectionIndex = 30
        CollectionName = 'Business\business_contact'
        Disabled = False
        Name = 'business\business_contact'
      end
      item
        CollectionIndex = 31
        CollectionName = 'butting_in'
        Disabled = False
        Name = 'butting_in'
      end
      item
        CollectionIndex = 32
        CollectionName = 'cable_release'
        Disabled = False
        Name = 'cable_release'
      end
      item
        CollectionIndex = 33
        CollectionName = 'call_transfer'
        Disabled = False
        Name = 'call_transfer'
      end
      item
        CollectionIndex = 34
        CollectionName = 'callback'
        Disabled = False
        Name = 'callback'
      end
      item
        CollectionIndex = 35
        CollectionName = 'camcorder'
        Disabled = False
        Name = 'camcorder'
      end
      item
        CollectionIndex = 36
        CollectionName = 'camcorder_pro'
        Disabled = False
        Name = 'camcorder_pro'
      end
      item
        CollectionIndex = 37
        CollectionName = 'camera'
        Disabled = False
        Name = 'camera'
      end
      item
        CollectionIndex = 38
        CollectionName = 'camera_addon'
        Disabled = False
        Name = 'camera_addon'
      end
      item
        CollectionIndex = 39
        CollectionName = 'cancel'
        Disabled = False
        Name = 'cancel'
      end
      item
        CollectionIndex = 40
        CollectionName = 'candle_sticks'
        Disabled = False
        Name = 'candle_sticks'
      end
      item
        CollectionIndex = 41
        CollectionName = 'capacitor'
        Disabled = False
        Name = 'capacitor'
      end
      item
        CollectionIndex = 42
        CollectionName = 'cd_logo'
        Disabled = False
        Name = 'cd_logo'
      end
      item
        CollectionIndex = 43
        CollectionName = 'charge_battery'
        Disabled = False
        Name = 'charge_battery'
      end
      item
        CollectionIndex = 44
        CollectionName = 'checkmark'
        Disabled = False
        Name = 'checkmark'
      end
      item
        CollectionIndex = 45
        CollectionName = 'circuit'
        Disabled = False
        Name = 'circuit'
      end
      item
        CollectionIndex = 46
        CollectionName = 'clapperboard'
        Disabled = False
        Name = 'clapperboard'
      end
      item
        CollectionIndex = 47
        CollectionName = 'clear_filters'
        Disabled = False
        Name = 'clear_filters'
      end
      item
        CollectionIndex = 48
        CollectionName = 'close_up_mode'
        Disabled = False
        Name = 'close_up_mode'
      end
      item
        CollectionIndex = 49
        CollectionName = 'cloth'
        Disabled = False
        Name = 'cloth'
      end
      item
        CollectionIndex = 50
        CollectionName = 'collaboration'
        Disabled = False
        Name = 'collaboration'
      end
      item
        CollectionIndex = 51
        CollectionName = 'collect'
        Disabled = False
        Name = 'collect'
      end
      item
        CollectionIndex = 52
        CollectionName = 'combo_chart'
        Disabled = False
        Name = 'combo_chart'
      end
      item
        CollectionIndex = 53
        CollectionName = 'command_line'
        Disabled = False
        Name = 'command_line'
      end
      item
        CollectionIndex = 54
        CollectionName = 'comments'
        Disabled = False
        Name = 'comments'
      end
      item
        CollectionIndex = 55
        CollectionName = 'compact_camera'
        Disabled = False
        Name = 'compact_camera'
      end
      item
        CollectionIndex = 56
        CollectionName = 'conference_call'
        Disabled = False
        Name = 'conference_call'
      end
      item
        CollectionIndex = 57
        CollectionName = 'crystal_oscillator'
        Disabled = False
        Name = 'crystal_oscillator'
      end
      item
        CollectionIndex = 58
        CollectionName = 'currency_exchange'
        Disabled = False
        Name = 'currency_exchange'
      end
      item
        CollectionIndex = 59
        CollectionName = 'cursor'
        Disabled = False
        Name = 'cursor'
      end
      item
        CollectionIndex = 60
        CollectionName = 'customer_support'
        Disabled = False
        Name = 'customer_support'
      end
      item
        CollectionIndex = 61
        CollectionName = 'dam'
        Disabled = False
        Name = 'dam'
      end
      item
        CollectionIndex = 62
        CollectionName = 'data_sheet'
        Disabled = False
        Name = 'data_sheet'
      end
      item
        CollectionIndex = 63
        CollectionName = 'debt'
        Disabled = False
        Name = 'debt'
      end
      item
        CollectionIndex = 64
        CollectionName = 'department'
        Disabled = False
        Name = 'department'
      end
      item
        CollectionIndex = 65
        CollectionName = 'deployment'
        Disabled = False
        Name = 'deployment'
      end
      item
        CollectionIndex = 66
        CollectionName = 'diploma_1'
        Disabled = False
        Name = 'diploma_1'
      end
      item
        CollectionIndex = 67
        CollectionName = 'diploma_2'
        Disabled = False
        Name = 'diploma_2'
      end
      item
        CollectionIndex = 68
        CollectionName = 'display'
        Disabled = False
        Name = 'display'
      end
      item
        CollectionIndex = 69
        CollectionName = 'document'
        Disabled = False
        Name = 'document'
      end
      item
        CollectionIndex = 70
        CollectionName = 'donate'
        Disabled = False
        Name = 'donate'
      end
      item
        CollectionIndex = 71
        CollectionName = 'doughnut_chart'
        Disabled = False
        Name = 'doughnut_chart'
      end
      item
        CollectionIndex = 72
        CollectionName = 'down'
        Disabled = False
        Name = 'down'
      end
      item
        CollectionIndex = 73
        CollectionName = 'down_left'
        Disabled = False
        Name = 'down_left'
      end
      item
        CollectionIndex = 74
        CollectionName = 'down_right'
        Disabled = False
        Name = 'down_right'
      end
      item
        CollectionIndex = 75
        CollectionName = 'download'
        Disabled = False
        Name = 'download'
      end
      item
        CollectionIndex = 76
        CollectionName = 'dribbble'
        Disabled = False
        Name = 'dribbble'
      end
      item
        CollectionIndex = 77
        CollectionName = 'dvd_logo'
        Disabled = False
        Name = 'dvd_logo'
      end
      item
        CollectionIndex = 78
        CollectionName = 'electrical_sensor'
        Disabled = False
        Name = 'electrical_sensor'
      end
      item
        CollectionIndex = 79
        CollectionName = 'electrical_threshold'
        Disabled = False
        Name = 'electrical_threshold'
      end
      item
        CollectionIndex = 80
        CollectionName = 'electricity'
        Disabled = False
        Name = 'electricity'
      end
      item
        CollectionIndex = 81
        CollectionName = 'electro_devices'
        Disabled = False
        Name = 'electro_devices'
      end
      item
        CollectionIndex = 82
        CollectionName = 'electronics'
        Disabled = False
        Name = 'electronics'
      end
      item
        CollectionIndex = 83
        CollectionName = 'empty_battery'
        Disabled = False
        Name = 'empty_battery'
      end
      item
        CollectionIndex = 84
        CollectionName = 'empty_filter'
        Disabled = False
        Name = 'empty_filter'
      end
      item
        CollectionIndex = 85
        CollectionName = 'empty_trash'
        Disabled = False
        Name = 'empty_trash'
      end
      item
        CollectionIndex = 86
        CollectionName = 'end_call'
        Disabled = False
        Name = 'end_call'
      end
      item
        CollectionIndex = 87
        CollectionName = 'engineering'
        Disabled = False
        Name = 'engineering'
      end
      item
        CollectionIndex = 88
        CollectionName = 'entering_heaven_alive'
        Disabled = False
        Name = 'entering_heaven_alive'
      end
      item
        CollectionIndex = 89
        CollectionName = 'expand'
        Disabled = False
        Name = 'expand'
      end
      item
        CollectionIndex = 90
        CollectionName = 'export'
        Disabled = False
        Name = 'export'
      end
      item
        CollectionIndex = 91
        CollectionName = 'external'
        Disabled = False
        Name = 'external'
      end
      item
        CollectionIndex = 92
        CollectionName = 'factory'
        Disabled = False
        Name = 'factory'
      end
      item
        CollectionIndex = 93
        CollectionName = 'factory_breakdown'
        Disabled = False
        Name = 'factory_breakdown'
      end
      item
        CollectionIndex = 94
        CollectionName = 'faq'
        Disabled = False
        Name = 'faq'
      end
      item
        CollectionIndex = 95
        CollectionName = 'feed_in'
        Disabled = False
        Name = 'feed_in'
      end
      item
        CollectionIndex = 96
        CollectionName = 'feedback'
        Disabled = False
        Name = 'feedback'
      end
      item
        CollectionIndex = 97
        CollectionName = 'file'
        Disabled = False
        Name = 'file'
      end
      item
        CollectionIndex = 98
        CollectionName = 'filing_cabinet'
        Disabled = False
        Name = 'filing_cabinet'
      end
      item
        CollectionIndex = 99
        CollectionName = 'filled_filter'
        Disabled = False
        Name = 'filled_filter'
      end
      item
        CollectionIndex = 100
        CollectionName = 'Delphi_Product icon'
        Disabled = False
        Name = 'Delphi_Product icon'
      end
      item
        CollectionIndex = 101
        CollectionName = 'film'
        Disabled = False
        Name = 'film'
      end
      item
        CollectionIndex = 102
        CollectionName = 'film_reel'
        Disabled = False
        Name = 'film_reel'
      end
      item
        CollectionIndex = 103
        CollectionName = 'flash_auto'
        Disabled = False
        Name = 'flash_auto'
      end
      item
        CollectionIndex = 104
        CollectionName = 'flash_on'
        Disabled = False
        Name = 'flash_on'
      end
      item
        CollectionIndex = 105
        CollectionName = 'flow_chart'
        Disabled = False
        Name = 'flow_chart'
      end
      item
        CollectionIndex = 106
        CollectionName = 'folder'
        Disabled = False
        Name = 'folder'
      end
      item
        CollectionIndex = 107
        CollectionName = 'frame'
        Disabled = False
        Name = 'frame'
      end
      item
        CollectionIndex = 108
        CollectionName = 'full_battery'
        Disabled = False
        Name = 'full_battery'
      end
      item
        CollectionIndex = 109
        CollectionName = 'full_trash'
        Disabled = False
        Name = 'full_trash'
      end
      item
        CollectionIndex = 110
        CollectionName = 'gallery'
        Disabled = False
        Name = 'gallery'
      end
      item
        CollectionIndex = 111
        CollectionName = 'genealogy'
        Disabled = False
        Name = 'genealogy'
      end
      item
        CollectionIndex = 112
        CollectionName = 'generic_sorting_asc'
        Disabled = False
        Name = 'generic_sorting_asc'
      end
      item
        CollectionIndex = 113
        CollectionName = 'generic_sorting_desc'
        Disabled = False
        Name = 'generic_sorting_desc'
      end
      item
        CollectionIndex = 114
        CollectionName = 'globe'
        Disabled = False
        Name = 'globe'
      end
      item
        CollectionIndex = 115
        CollectionName = 'good_decision'
        Disabled = False
        Name = 'good_decision'
      end
      item
        CollectionIndex = 116
        CollectionName = 'google'
        Disabled = False
        Name = 'google'
      end
      item
        CollectionIndex = 117
        CollectionName = 'graduation_cap'
        Disabled = False
        Name = 'graduation_cap'
      end
      item
        CollectionIndex = 118
        CollectionName = 'grid'
        Disabled = False
        Name = 'grid'
      end
      item
        CollectionIndex = 119
        CollectionName = 'headset'
        Disabled = False
        Name = 'headset'
      end
      item
        CollectionIndex = 120
        CollectionName = 'heat_map'
        Disabled = False
        Name = 'heat_map'
      end
      item
        CollectionIndex = 121
        CollectionName = 'high_battery'
        Disabled = False
        Name = 'high_battery'
      end
      item
        CollectionIndex = 122
        CollectionName = 'high_priority'
        Disabled = False
        Name = 'high_priority'
      end
      item
        CollectionIndex = 123
        CollectionName = 'home'
        Disabled = False
        Name = 'home'
      end
      item
        CollectionIndex = 124
        CollectionName = 'icons8_cup'
        Disabled = False
        Name = 'icons8_cup'
      end
      item
        CollectionIndex = 125
        CollectionName = 'idea'
        Disabled = False
        Name = 'idea'
      end
      item
        CollectionIndex = 126
        CollectionName = 'image_file'
        Disabled = False
        Name = 'image_file'
      end
      item
        CollectionIndex = 127
        CollectionName = 'import'
        Disabled = False
        Name = 'import'
      end
      item
        CollectionIndex = 128
        CollectionName = 'in_transit'
        Disabled = False
        Name = 'in_transit'
      end
      item
        CollectionIndex = 129
        CollectionName = 'info'
        Disabled = False
        Name = 'info'
      end
      item
        CollectionIndex = 130
        CollectionName = 'inspection'
        Disabled = False
        Name = 'inspection'
      end
      item
        CollectionIndex = 131
        CollectionName = 'integrated_webcam'
        Disabled = False
        Name = 'integrated_webcam'
      end
      item
        CollectionIndex = 132
        CollectionName = 'internal'
        Disabled = False
        Name = 'internal'
      end
      item
        CollectionIndex = 133
        CollectionName = 'invite'
        Disabled = False
        Name = 'invite'
      end
      item
        CollectionIndex = 134
        CollectionName = 'ipad'
        Disabled = False
        Name = 'ipad'
      end
      item
        CollectionIndex = 135
        CollectionName = 'iphone'
        Disabled = False
        Name = 'iphone'
      end
      item
        CollectionIndex = 136
        CollectionName = 'key'
        Disabled = False
        Name = 'key'
      end
      item
        CollectionIndex = 137
        CollectionName = 'kindle'
        Disabled = False
        Name = 'kindle'
      end
      item
        CollectionIndex = 138
        CollectionName = 'landscape'
        Disabled = False
        Name = 'landscape'
      end
      item
        CollectionIndex = 139
        CollectionName = 'leave'
        Disabled = False
        Name = 'leave'
      end
      item
        CollectionIndex = 140
        CollectionName = 'left'
        Disabled = False
        Name = 'left'
      end
      item
        CollectionIndex = 141
        CollectionName = 'left_down2'
        Disabled = False
        Name = 'left_down2'
      end
      item
        CollectionIndex = 142
        CollectionName = 'left_up2'
        Disabled = False
        Name = 'left_up2'
      end
      item
        CollectionIndex = 143
        CollectionName = 'library'
        Disabled = False
        Name = 'library'
      end
      item
        CollectionIndex = 144
        CollectionName = 'light_at_the_end_of_tunnel'
        Disabled = False
        Name = 'light_at_the_end_of_tunnel'
      end
      item
        CollectionIndex = 145
        CollectionName = 'like'
        Disabled = False
        Name = 'like'
      end
      item
        CollectionIndex = 146
        CollectionName = 'like_placeholder'
        Disabled = False
        Name = 'like_placeholder'
      end
      item
        CollectionIndex = 147
        CollectionName = 'line_chart'
        Disabled = False
        Name = 'line_chart'
      end
      item
        CollectionIndex = 148
        CollectionName = 'link'
        Disabled = False
        Name = 'link'
      end
      item
        CollectionIndex = 149
        CollectionName = 'linux'
        Disabled = False
        Name = 'linux'
      end
      item
        CollectionIndex = 150
        CollectionName = 'list'
        Disabled = False
        Name = 'list'
      end
      item
        CollectionIndex = 151
        CollectionName = 'lock'
        Disabled = False
        Name = 'lock'
      end
      item
        CollectionIndex = 152
        CollectionName = 'lock_landscape'
        Disabled = False
        Name = 'lock_landscape'
      end
      item
        CollectionIndex = 153
        CollectionName = 'lock_portrait'
        Disabled = False
        Name = 'lock_portrait'
      end
      item
        CollectionIndex = 154
        CollectionName = 'low_battery'
        Disabled = False
        Name = 'low_battery'
      end
      item
        CollectionIndex = 155
        CollectionName = 'low_priority'
        Disabled = False
        Name = 'low_priority'
      end
      item
        CollectionIndex = 156
        CollectionName = 'make_decision'
        Disabled = False
        Name = 'make_decision'
      end
      item
        CollectionIndex = 157
        CollectionName = 'manager'
        Disabled = False
        Name = 'manager'
      end
      item
        CollectionIndex = 158
        CollectionName = 'medium_priority'
        Disabled = False
        Name = 'medium_priority'
      end
      item
        CollectionIndex = 159
        CollectionName = 'menu'
        Disabled = False
        Name = 'menu'
      end
      item
        CollectionIndex = 160
        CollectionName = 'middle_battery'
        Disabled = False
        Name = 'middle_battery'
      end
      item
        CollectionIndex = 161
        CollectionName = 'mind_map'
        Disabled = False
        Name = 'mind_map'
      end
      item
        CollectionIndex = 162
        CollectionName = 'missed_call'
        Disabled = False
        Name = 'missed_call'
      end
      item
        CollectionIndex = 163
        CollectionName = 'mms'
        Disabled = False
        Name = 'mms'
      end
      item
        CollectionIndex = 164
        CollectionName = 'money_transfer'
        Disabled = False
        Name = 'money_transfer'
      end
      item
        CollectionIndex = 165
        CollectionName = 'multiple_cameras'
        Disabled = False
        Name = 'multiple_cameras'
      end
      item
        CollectionIndex = 166
        CollectionName = 'multiple_devices'
        Disabled = False
        Name = 'multiple_devices'
      end
      item
        CollectionIndex = 167
        CollectionName = 'multiple_inputs'
        Disabled = False
        Name = 'multiple_inputs'
      end
      item
        CollectionIndex = 168
        CollectionName = 'multiple_smartphones'
        Disabled = False
        Name = 'multiple_smartphones'
      end
      item
        CollectionIndex = 169
        CollectionName = 'music'
        Disabled = False
        Name = 'music'
      end
      item
        CollectionIndex = 170
        CollectionName = 'neutral_decision'
        Disabled = False
        Name = 'neutral_decision'
      end
      item
        CollectionIndex = 171
        CollectionName = 'neutral_trading'
        Disabled = False
        Name = 'neutral_trading'
      end
      item
        CollectionIndex = 172
        CollectionName = 'news'
        Disabled = False
        Name = 'news'
      end
      item
        CollectionIndex = 173
        CollectionName = 'next'
        Disabled = False
        Name = 'next'
      end
      item
        CollectionIndex = 174
        CollectionName = 'nfc_sign'
        Disabled = False
        Name = 'nfc_sign'
      end
      item
        CollectionIndex = 175
        CollectionName = 'night_landscape'
        Disabled = False
        Name = 'night_landscape'
      end
      item
        CollectionIndex = 176
        CollectionName = 'night_portrait'
        Disabled = False
        Name = 'night_portrait'
      end
      item
        CollectionIndex = 177
        CollectionName = 'no_idea'
        Disabled = False
        Name = 'no_idea'
      end
      item
        CollectionIndex = 178
        CollectionName = 'no_video'
        Disabled = False
        Name = 'no_video'
      end
      item
        CollectionIndex = 179
        CollectionName = 'nook'
        Disabled = False
        Name = 'nook'
      end
      item
        CollectionIndex = 180
        CollectionName = 'numerical_sorting_12'
        Disabled = False
        Name = 'numerical_sorting_12'
      end
      item
        CollectionIndex = 181
        CollectionName = 'numerical_sorting_21'
        Disabled = False
        Name = 'numerical_sorting_21'
      end
      item
        CollectionIndex = 182
        CollectionName = 'ok'
        Disabled = False
        Name = 'ok'
      end
      item
        CollectionIndex = 183
        CollectionName = 'old_time_camera'
        Disabled = False
        Name = 'old_time_camera'
      end
      item
        CollectionIndex = 184
        CollectionName = 'online_support'
        Disabled = False
        Name = 'online_support'
      end
      item
        CollectionIndex = 185
        CollectionName = 'opened_folder'
        Disabled = False
        Name = 'opened_folder'
      end
      item
        CollectionIndex = 186
        CollectionName = 'org_unit'
        Disabled = False
        Name = 'org_unit'
      end
      item
        CollectionIndex = 187
        CollectionName = 'organization'
        Disabled = False
        Name = 'organization'
      end
      item
        CollectionIndex = 188
        CollectionName = 'package'
        Disabled = False
        Name = 'package'
      end
      item
        CollectionIndex = 189
        CollectionName = 'paid'
        Disabled = False
        Name = 'paid'
      end
      item
        CollectionIndex = 190
        CollectionName = 'panorama'
        Disabled = False
        Name = 'panorama'
      end
      item
        CollectionIndex = 191
        CollectionName = 'parallel_tasks'
        Disabled = False
        Name = 'parallel_tasks'
      end
      item
        CollectionIndex = 192
        CollectionName = 'phone'
        Disabled = False
        Name = 'phone'
      end
      item
        CollectionIndex = 193
        CollectionName = 'phone_android'
        Disabled = False
        Name = 'phone_android'
      end
      item
        CollectionIndex = 194
        CollectionName = 'photo_reel'
        Disabled = False
        Name = 'photo_reel'
      end
      item
        CollectionIndex = 195
        CollectionName = 'picture'
        Disabled = False
        Name = 'picture'
      end
      item
        CollectionIndex = 196
        CollectionName = 'pie_chart'
        Disabled = False
        Name = 'pie_chart'
      end
      item
        CollectionIndex = 197
        CollectionName = 'planner'
        Disabled = False
        Name = 'planner'
      end
      item
        CollectionIndex = 198
        CollectionName = 'plus'
        Disabled = False
        Name = 'plus'
      end
      item
        CollectionIndex = 199
        CollectionName = 'podium_with_audience'
        Disabled = False
        Name = 'podium_with_audience'
      end
      item
        CollectionIndex = 200
        CollectionName = 'podium_with_speaker'
        Disabled = False
        Name = 'podium_with_speaker'
      end
      item
        CollectionIndex = 201
        CollectionName = 'podium_without_speaker'
        Disabled = False
        Name = 'podium_without_speaker'
      end
      item
        CollectionIndex = 202
        CollectionName = 'portrait_mode'
        Disabled = False
        Name = 'portrait_mode'
      end
      item
        CollectionIndex = 203
        CollectionName = 'previous'
        Disabled = False
        Name = 'previous'
      end
      item
        CollectionIndex = 204
        CollectionName = 'print'
        Disabled = False
        Name = 'print'
      end
      item
        CollectionIndex = 205
        CollectionName = 'privacy'
        Disabled = False
        Name = 'privacy'
      end
      item
        CollectionIndex = 206
        CollectionName = 'process'
        Disabled = False
        Name = 'process'
      end
      item
        CollectionIndex = 207
        CollectionName = 'puzzle'
        Disabled = False
        Name = 'puzzle'
      end
      item
        CollectionIndex = 208
        CollectionName = 'questions'
        Disabled = False
        Name = 'questions'
      end
      item
        CollectionIndex = 209
        CollectionName = 'radar_plot'
        Disabled = False
        Name = 'radar_plot'
      end
      item
        CollectionIndex = 210
        CollectionName = 'rating'
        Disabled = False
        Name = 'rating'
      end
      item
        CollectionIndex = 211
        CollectionName = 'ratings'
        Disabled = False
        Name = 'ratings'
      end
      item
        CollectionIndex = 212
        CollectionName = 'reading'
        Disabled = False
        Name = 'reading'
      end
      item
        CollectionIndex = 213
        CollectionName = 'reading_ebook'
        Disabled = False
        Name = 'reading_ebook'
      end
      item
        CollectionIndex = 214
        CollectionName = 'reddit'
        Disabled = False
        Name = 'reddit'
      end
      item
        CollectionIndex = 215
        CollectionName = 'redo'
        Disabled = False
        Name = 'redo'
      end
      item
        CollectionIndex = 216
        CollectionName = 'refresh'
        Disabled = False
        Name = 'refresh'
      end
      item
        CollectionIndex = 217
        CollectionName = 'registered_trademark'
        Disabled = False
        Name = 'registered_trademark'
      end
      item
        CollectionIndex = 218
        CollectionName = 'remove_image'
        Disabled = False
        Name = 'remove_image'
      end
      item
        CollectionIndex = 219
        CollectionName = 'reuse'
        Disabled = False
        Name = 'reuse'
      end
      item
        CollectionIndex = 220
        CollectionName = 'right'
        Disabled = False
        Name = 'right'
      end
      item
        CollectionIndex = 221
        CollectionName = 'right_down2'
        Disabled = False
        Name = 'right_down2'
      end
      item
        CollectionIndex = 222
        CollectionName = 'right_up2'
        Disabled = False
        Name = 'right_up2'
      end
      item
        CollectionIndex = 223
        CollectionName = 'rotate_camera'
        Disabled = False
        Name = 'rotate_camera'
      end
      item
        CollectionIndex = 224
        CollectionName = 'rotate_to_landscape'
        Disabled = False
        Name = 'rotate_to_landscape'
      end
      item
        CollectionIndex = 225
        CollectionName = 'rotate_to_portrait'
        Disabled = False
        Name = 'rotate_to_portrait'
      end
      item
        CollectionIndex = 226
        CollectionName = 'rules'
        Disabled = False
        Name = 'rules'
      end
      item
        CollectionIndex = 227
        CollectionName = 'safe'
        Disabled = False
        Name = 'safe'
      end
      item
        CollectionIndex = 228
        CollectionName = 'sales_performance'
        Disabled = False
        Name = 'sales_performance'
      end
      item
        CollectionIndex = 229
        CollectionName = 'scatter_plot'
        Disabled = False
        Name = 'scatter_plot'
      end
      item
        CollectionIndex = 230
        CollectionName = 'search'
        Disabled = False
        Name = 'search'
      end
      item
        CollectionIndex = 231
        CollectionName = 'self_service_kiosk'
        Disabled = False
        Name = 'self_service_kiosk'
      end
      item
        CollectionIndex = 232
        CollectionName = 'selfie'
        Disabled = False
        Name = 'selfie'
      end
      item
        CollectionIndex = 233
        CollectionName = 'serial_tasks'
        Disabled = False
        Name = 'serial_tasks'
      end
      item
        CollectionIndex = 234
        CollectionName = 'service_mark'
        Disabled = False
        Name = 'service_mark'
      end
      item
        CollectionIndex = 235
        CollectionName = 'services'
        Disabled = False
        Name = 'services'
      end
      item
        CollectionIndex = 236
        CollectionName = 'settings'
        Disabled = False
        Name = 'settings'
      end
      item
        CollectionIndex = 237
        CollectionName = 'share'
        Disabled = False
        Name = 'share'
      end
      item
        CollectionIndex = 238
        CollectionName = 'shipped'
        Disabled = False
        Name = 'shipped'
      end
      item
        CollectionIndex = 239
        CollectionName = 'shop'
        Disabled = False
        Name = 'shop'
      end
      item
        CollectionIndex = 240
        CollectionName = 'sim_card'
        Disabled = False
        Name = 'sim_card'
      end
      item
        CollectionIndex = 241
        CollectionName = 'sim_card_chip'
        Disabled = False
        Name = 'sim_card_chip'
      end
      item
        CollectionIndex = 242
        CollectionName = 'slr_back_side'
        Disabled = False
        Name = 'slr_back_side'
      end
      item
        CollectionIndex = 243
        CollectionName = 'smartphone_tablet'
        Disabled = False
        Name = 'smartphone_tablet'
      end
      item
        CollectionIndex = 244
        CollectionName = 'sms'
        Disabled = False
        Name = 'sms'
      end
      item
        CollectionIndex = 245
        CollectionName = 'sound_recording_copyright'
        Disabled = False
        Name = 'sound_recording_copyright'
      end
      item
        CollectionIndex = 246
        CollectionName = 'speaker'
        Disabled = False
        Name = 'speaker'
      end
      item
        CollectionIndex = 247
        CollectionName = 'sports_mode'
        Disabled = False
        Name = 'sports_mode'
      end
      item
        CollectionIndex = 248
        CollectionName = 'stack_of_photos'
        Disabled = False
        Name = 'stack_of_photos'
      end
      item
        CollectionIndex = 249
        CollectionName = 'start'
        Disabled = False
        Name = 'start'
      end
      item
        CollectionIndex = 250
        CollectionName = 'steam'
        Disabled = False
        Name = 'steam'
      end
      item
        CollectionIndex = 251
        CollectionName = 'stumbleupon'
        Disabled = False
        Name = 'stumbleupon'
      end
      item
        CollectionIndex = 252
        CollectionName = 'support'
        Disabled = False
        Name = 'support'
      end
      item
        CollectionIndex = 253
        CollectionName = 'survey'
        Disabled = False
        Name = 'survey'
      end
      item
        CollectionIndex = 254
        CollectionName = 'switch_camera'
        Disabled = False
        Name = 'switch_camera'
      end
      item
        CollectionIndex = 255
        CollectionName = 'synchronize'
        Disabled = False
        Name = 'synchronize'
      end
      item
        CollectionIndex = 256
        CollectionName = 'tablet_android'
        Disabled = False
        Name = 'tablet_android'
      end
      item
        CollectionIndex = 257
        CollectionName = 'template'
        Disabled = False
        Name = 'template'
      end
      item
        CollectionIndex = 258
        CollectionName = 'timeline'
        Disabled = False
        Name = 'timeline'
      end
      item
        CollectionIndex = 259
        CollectionName = 'todo_list'
        Disabled = False
        Name = 'todo_list'
      end
      item
        CollectionIndex = 260
        CollectionName = 'touchscreen_smartphone'
        Disabled = False
        Name = 'touchscreen_smartphone'
      end
      item
        CollectionIndex = 261
        CollectionName = 'trademark'
        Disabled = False
        Name = 'trademark'
      end
      item
        CollectionIndex = 262
        CollectionName = 'tree_structure'
        Disabled = False
        Name = 'tree_structure'
      end
      item
        CollectionIndex = 263
        CollectionName = 'two_smartphones'
        Disabled = False
        Name = 'two_smartphones'
      end
      item
        CollectionIndex = 264
        CollectionName = 'undo'
        Disabled = False
        Name = 'undo'
      end
      item
        CollectionIndex = 265
        CollectionName = 'unlock'
        Disabled = False
        Name = 'unlock'
      end
      item
        CollectionIndex = 266
        CollectionName = 'up'
        Disabled = False
        Name = 'up'
      end
      item
        CollectionIndex = 267
        CollectionName = 'up_left'
        Disabled = False
        Name = 'up_left'
      end
      item
        CollectionIndex = 268
        CollectionName = 'up_right'
        Disabled = False
        Name = 'up_right'
      end
      item
        CollectionIndex = 269
        CollectionName = 'upload'
        Disabled = False
        Name = 'upload'
      end
      item
        CollectionIndex = 270
        CollectionName = 'usb'
        Disabled = False
        Name = 'usb'
      end
      item
        CollectionIndex = 271
        CollectionName = 'video_call'
        Disabled = False
        Name = 'video_call'
      end
      item
        CollectionIndex = 272
        CollectionName = 'video_file'
        Disabled = False
        Name = 'video_file'
      end
      item
        CollectionIndex = 273
        CollectionName = 'video_projector'
        Disabled = False
        Name = 'video_projector'
      end
      item
        CollectionIndex = 274
        CollectionName = 'view_details'
        Disabled = False
        Name = 'view_details'
      end
      item
        CollectionIndex = 275
        CollectionName = 'vip'
        Disabled = False
        Name = 'vip'
      end
      item
        CollectionIndex = 276
        CollectionName = 'vlc'
        Disabled = False
        Name = 'vlc'
      end
      item
        CollectionIndex = 277
        CollectionName = 'voice_presentation'
        Disabled = False
        Name = 'voice_presentation'
      end
      item
        CollectionIndex = 278
        CollectionName = 'voicemail'
        Disabled = False
        Name = 'voicemail'
      end
      item
        CollectionIndex = 279
        CollectionName = 'webcam'
        Disabled = False
        Name = 'webcam'
      end
      item
        CollectionIndex = 280
        CollectionName = 'wi-fi_logo'
        Disabled = False
        Name = 'wi-fi_logo'
      end
      item
        CollectionIndex = 281
        CollectionName = 'wikipedia'
        Disabled = False
        Name = 'wikipedia'
      end
      item
        CollectionIndex = 282
        CollectionName = 'workflow'
        Disabled = False
        Name = 'workflow'
      end>
    ImageCollection = ImageDataModule.SVGIconImageCollection
    Width = 32
    Height = 32
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
