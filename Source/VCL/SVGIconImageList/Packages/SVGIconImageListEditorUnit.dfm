object SVGIconImageListEditor: TSVGIconImageListEditor
  Left = 392
  Top = 450
  Caption = 'SVG Icon ImageList Editor %s - Copyright Ethea S.r.l.'
  ClientHeight = 619
  ClientWidth = 784
  Color = clBtnFace
  Constraints.MinHeight = 600
  Constraints.MinWidth = 700
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object BottomSplitter: TSplitter
    Left = 0
    Top = 398
    Width = 784
    Height = 4
    Cursor = crVSplit
    Align = alBottom
    AutoSnap = False
    MinSize = 200
  end
  object paTop: TPanel
    Left = 0
    Top = 0
    Width = 784
    Height = 377
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object paClient: TPanel
      Left = 0
      Top = 0
      Width = 784
      Height = 377
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object ImageListGroupBox: TGroupBox
        Left = 0
        Top = 0
        Width = 784
        Height = 62
        Align = alTop
        Caption = 'Properties of ImageList'
        TabOrder = 0
        object SizeLabel: TLabel
          Left = 8
          Top = 15
          Width = 80
          Height = 15
          AutoSize = False
          Caption = 'Size (in pixel)'
          Transparent = True
        end
        object WidthLabel: TLabel
          Left = 94
          Top = 15
          Width = 80
          Height = 15
          AutoSize = False
          Caption = 'Width (in pixel)'
          Transparent = True
        end
        object HeightLabel: TLabel
          Left = 181
          Top = 15
          Width = 84
          Height = 15
          AutoSize = False
          Caption = 'Height (in pixel)'
          Transparent = True
        end
        object SizeSpinEdit: TSpinEdit
          Left = 8
          Top = 32
          Width = 81
          Height = 24
          MaxValue = 0
          MinValue = 0
          TabOrder = 0
          Value = 0
          OnChange = SizeSpinEditChange
        end
        object WidthSpinEdit: TSpinEdit
          Left = 94
          Top = 32
          Width = 81
          Height = 24
          MaxValue = 0
          MinValue = 0
          TabOrder = 1
          Value = 0
          OnChange = WidthSpinEditChange
        end
        object HeightSpinEdit: TSpinEdit
          Left = 181
          Top = 32
          Width = 81
          Height = 24
          MaxValue = 0
          MinValue = 0
          TabOrder = 2
          Value = 0
          OnChange = HeightSpinEditChange
        end
      end
      object paImages: TPanel
        Left = 0
        Top = 62
        Width = 784
        Height = 315
        Align = alClient
        TabOrder = 1
        object CategorySplitter: TSplitter
          Left = 186
          Top = 1
          Width = 4
          Height = 136
          AutoSnap = False
          MinSize = 150
        end
        object ImageListGroup: TGroupBox
          Left = 190
          Top = 1
          Width = 450
          Height = 313
          Align = alClient
          Caption = '%d Icons collection'
          TabOrder = 1
          object ImageView: TListView
            Left = 2
            Top = 17
            Width = 446
            Height = 294
            Align = alClient
            Columns = <>
            DragMode = dmAutomatic
            FullDrag = True
            HideSelection = False
            IconOptions.AutoArrange = True
            MultiSelect = True
            ReadOnly = True
            TabOrder = 0
            OnDragDrop = ImageViewDragDrop
            OnDragOver = ImageViewDragOver
            OnKeyDown = ImageViewKeyDown
            OnSelectItem = ImageViewSelectItem
          end
        end
        object paButtons: TPanel
          Left = 640
          Top = 1
          Width = 143
          Height = 313
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 2
          object AddButton: TButton
            Left = 4
            Top = 17
            Width = 135
            Height = 25
            Caption = '&Add...'
            TabOrder = 0
            OnClick = AddButtonClick
          end
          object ReplaceButton: TButton
            Left = 4
            Top = 48
            Width = 135
            Height = 25
            Caption = '&Replace...'
            TabOrder = 1
            OnClick = ReplaceButtonClick
          end
          object ClearAllButton: TButton
            Left = 4
            Top = 107
            Width = 135
            Height = 25
            Caption = '&Clear all'
            Enabled = False
            TabOrder = 3
            OnClick = ClearAllButtonClick
          end
          object ExportButton: TButton
            Left = 4
            Top = 138
            Width = 135
            Height = 25
            Caption = '&Export...'
            Enabled = False
            TabOrder = 4
            OnClick = ExportButtonClick
          end
          object SetCategoriesButton: TButton
            Left = 4
            Top = 169
            Width = 135
            Height = 25
            Caption = '&Set Categories...'
            TabOrder = 5
            OnClick = SetCategoriesButtonClick
          end
          object DeleteAllButton: TButton
            Left = 4
            Top = 78
            Width = 135
            Height = 25
            Caption = 'D&elete'
            Enabled = False
            TabOrder = 2
            OnClick = DeleteAllButtonClick
          end
        end
        object ImagesPanel: TPanel
          Left = 1
          Top = 1
          Width = 185
          Height = 313
          Align = alLeft
          BevelOuter = bvNone
          TabOrder = 0
          object CategoryGroupBox: TGroupBox
            Left = 0
            Top = 0
            Width = 185
            Height = 169
            Align = alClient
            Caption = 'Images/Categories'
            TabOrder = 0
            object CategoryListBox: TListBox
              Left = 2
              Top = 17
              Width = 181
              Height = 150
              Align = alClient
              ItemHeight = 15
              TabOrder = 0
              OnClick = CategoryListBoxClick
            end
          end
          object PropertiesGroupBox: TGroupBox
            Left = 0
            Top = 158
            Width = 185
            Height = 159
            Align = alBottom
            Caption = 'Global properties'
            TabOrder = 1
            object FixedColorLabel: TLabel
              Left = 6
              Top = 19
              Width = 63
              Height = 15
              AutoSize = False
              Caption = 'Fixed Color'
              Transparent = True
            end
            object OpacityLabel: TLabel
              Left = 6
              Top = 105
              Width = 80
              Height = 15
              AutoSize = False
              Caption = 'Opacity (255-0)'
              Transparent = True
            end
            object AntiAliasColorLabel: TLabel
              Left = 6
              Top = 62
              Width = 99
              Height = 15
              AutoSize = False
              Caption = 'Antialias Color'
              Transparent = True
            end
            object FixedColorComboBox: TColorBox
              Left = 6
              Top = 36
              Width = 147
              Height = 22
              DefaultColorColor = clDefault
              NoneColorColor = clNone
              Selected = clNone
              Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames, cbCustomColors]
              TabOrder = 0
              OnSelect = FixedColorComboBoxSelect
            end
            object GrayScaleCheckBox: TCheckBox
              Left = 73
              Top = 128
              Width = 85
              Height = 17
              Caption = 'GrayScale'
              TabOrder = 3
              OnClick = GrayScaleCheckBoxClick
            end
            object OpacitySpinEdit: TSpinEdit
              Left = 5
              Top = 122
              Width = 64
              Height = 24
              MaxValue = 255
              MinValue = 0
              TabOrder = 2
              Value = 0
              OnChange = OpacitySpinEditChange
            end
            object AntialiasColorComboBox: TColorBox
              Left = 6
              Top = 79
              Width = 147
              Height = 22
              DefaultColorColor = clDefault
              NoneColorColor = clNone
              Selected = clNone
              Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames, cbCustomColors]
              TabOrder = 1
              OnSelect = AntialiasColorComboBoxSelect
            end
          end
        end
      end
    end
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 581
    Width = 784
    Height = 38
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      784
      38)
    object OKButton: TButton
      Left = 418
      Top = 6
      Width = 85
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = OkButtonClick
    end
    object ApplyButton: TButton
      Left = 601
      Top = 6
      Width = 85
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Apply'
      TabOrder = 2
      OnClick = ApplyButtonClick
    end
    object CancelButton: TButton
      Left = 510
      Top = 6
      Width = 85
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object HelpButton: TButton
      Left = 693
      Top = 6
      Width = 85
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Help'
      TabOrder = 3
      OnClick = HelpButtonClick
    end
    object SVGErrorStaticText: TStaticText
      Left = 6
      Top = 3
      Width = 406
      Height = 30
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      TabOrder = 4
      Transparent = False
    end
  end
  object paIcon: TPanel
    Left = 0
    Top = 381
    Width = 784
    Height = 200
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object IconButtonsPanel: TPanel
      Left = 691
      Top = 0
      Width = 93
      Height = 200
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      object NewButton: TButton
        Left = 2
        Top = 6
        Width = 85
        Height = 25
        Caption = '&New'
        TabOrder = 0
        OnClick = NewButtonClick
      end
      object ReformatXMLButton: TButton
        Left = 2
        Top = 152
        Width = 85
        Height = 25
        Caption = 'Reformat &XML'
        TabOrder = 2
        OnClick = ReformatXMLButtonClick
      end
      object DeleteButton: TButton
        Left = 2
        Top = 37
        Width = 85
        Height = 25
        Caption = '&Delete'
        TabOrder = 1
        OnClick = DeleteButtonClick
      end
    end
    object ItemGroupBox: TGroupBox
      Left = 0
      Top = 0
      Width = 691
      Height = 200
      Align = alClient
      Caption = 'Selected Icon'
      TabOrder = 0
      DesignSize = (
        691
        200)
      object IconNameLabel: TLabel
        Left = 176
        Top = 11
        Width = 87
        Height = 15
        AutoSize = False
        Caption = 'Name'
        Transparent = True
      end
      object IconFixedColorLabel: TLabel
        Left = 415
        Top = 11
        Width = 63
        Height = 15
        AutoSize = False
        Caption = 'Fixed Color'
        Transparent = True
      end
      object IconIndexLabel: TLabel
        Left = 643
        Top = 11
        Width = 38
        Height = 15
        AutoSize = False
        Caption = 'Index'
        Transparent = True
      end
      object CategoryLabel: TLabel
        Left = 295
        Top = 11
        Width = 87
        Height = 15
        AutoSize = False
        Caption = 'Category'
        Transparent = True
      end
      object IconPanel: TPanel
        Left = 10
        Top = 28
        Width = 160
        Height = 160
        BevelOuter = bvNone
        BorderWidth = 2
        BorderStyle = bsSingle
        Color = clWindow
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 0
        object IconImage: TSVGIconImage
          Left = 2
          Top = 2
          Width = 154
          Height = 154
          AutoSize = False
          Align = alClient
        end
      end
      object NameEdit: TEdit
        Left = 176
        Top = 28
        Width = 114
        Height = 23
        TabOrder = 1
        OnExit = NameEditExit
      end
      object SVGText: TMemo
        Left = 176
        Top = 54
        Width = 507
        Height = 134
        Hint = 'SVG Text content'
        Anchors = [akLeft, akTop, akRight, akBottom]
        ScrollBars = ssBoth
        TabOrder = 6
        OnChange = SVGTextChange
        OnEnter = SVGTextEnter
        OnExit = SVGTextExit
        OnKeyDown = SVGTextKeyDown
      end
      object FixedColorItemComboBox: TColorBox
        Left = 415
        Top = 28
        Width = 136
        Height = 22
        NoneColorColor = clNone
        Selected = clDefault
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames, cbCustomColors]
        TabOrder = 3
        OnSelect = FixedColorItemComboBoxSelect
      end
      object GrayScaleItemCheckBox: TCheckBox
        Left = 563
        Top = 30
        Width = 85
        Height = 17
        Caption = 'GrayScale'
        TabOrder = 4
        OnClick = GrayScaleItemCheckBoxClick
      end
      object IconIndexEdit: TEdit
        Left = 643
        Top = 25
        Width = 40
        Height = 23
        Enabled = False
        TabOrder = 5
        OnExit = NameEditExit
      end
      object CategoryEdit: TEdit
        Left = 295
        Top = 28
        Width = 114
        Height = 23
        TabOrder = 2
        OnExit = CategoryEditExit
      end
    end
  end
  object SaveDialog: TSavePictureDialog
    DefaultExt = 'svg'
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Options = [ofOverwritePrompt, ofPathMustExist, ofEnableSizing]
    Left = 392
    Top = 96
  end
end
