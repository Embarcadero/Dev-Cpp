object SVGTextPropertyEditorForm: TSVGTextPropertyEditorForm
  Left = 916
  Top = 169
  Caption = 
    'SVGText Property Editor - Copyright (c) Ethea S.r.l. - Apache 2.' +
    '0 Open Source License'
  ClientHeight = 256
  ClientWidth = 739
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object RightSplitter: TSplitter
    Left = 550
    Top = 0
    Width = 4
    Height = 223
    Align = alRight
    AutoSnap = False
    MinSize = 16
    ExplicitLeft = 582
    ExplicitHeight = 309
  end
  object paBottom: TPanel
    Left = 0
    Top = 223
    Width = 739
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object paButtons: TPanel
      Left = 320
      Top = 0
      Width = 419
      Height = 33
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object CancelButton: TButton
        Left = 254
        Top = 3
        Width = 75
        Height = 25
        Cancel = True
        Caption = '&Cancel'
        ModalResult = 2
        TabOrder = 3
      end
      object OKButton: TButton
        Left = 173
        Top = 3
        Width = 75
        Height = 25
        Caption = '&OK'
        Default = True
        ModalResult = 1
        TabOrder = 2
      end
      object HelpButton: TButton
        Left = 336
        Top = 3
        Width = 74
        Height = 25
        Caption = '&Help'
        TabOrder = 4
        OnClick = HelpButtonClick
      end
      object LoadButton: TButton
        Left = 5
        Top = 3
        Width = 74
        Height = 25
        Caption = '&Load...'
        TabOrder = 0
        OnClick = LoadButtonClick
      end
      object SaveButton: TButton
        Left = 85
        Top = 3
        Width = 74
        Height = 25
        Caption = '&Save...'
        TabOrder = 1
        OnClick = SaveButtonClick
      end
    end
  end
  object SVGTextMemo: TMemo
    Left = 0
    Top = 0
    Width = 550
    Height = 223
    Align = alClient
    BevelOuter = bvNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
    OnChange = SVGTextMemoChange
  end
  object paImage: TPanel
    Left = 554
    Top = 0
    Width = 185
    Height = 223
    Align = alRight
    BevelOuter = bvLowered
    BorderWidth = 1
    TabOrder = 2
    OnResize = paImageResize
    object paTitle: TPanel
      Left = 2
      Top = 2
      Width = 181
      Height = 24
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
    end
    object ImagePanel: TPanel
      AlignWithMargins = True
      Left = 2
      Top = 26
      Width = 181
      Height = 171
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alClient
      BevelOuter = bvLowered
      Color = clWindow
      ParentBackground = False
      TabOrder = 1
      ExplicitHeight = 195
      object SVGIconImage: TSVGIconImage
        Left = 1
        Top = 1
        Width = 179
        Height = 169
        AutoSize = False
        Center = False
        Proportional = True
        Stretch = True
        Opacity = 255
        Scale = 1.000000000000000000
        ImageIndex = 0
        Align = alClient
        ExplicitLeft = 6
        ExplicitTop = 28
        ExplicitWidth = 175
        ExplicitHeight = 189
      end
    end
    object BottomPanel: TPanel
      Left = 2
      Top = 197
      Width = 181
      Height = 24
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
      ExplicitTop = 2
      object ProportionalCheckBox: TCheckBox
        Left = 8
        Top = 4
        Width = 161
        Height = 17
        Caption = 'Proportional'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = ProportionalCheckBoxClick
      end
    end
  end
  object OpenDialog: TOpenPictureDialog
    Filter = 'Scalable Vector Graphics (*.svg)|*.svg'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 400
    Top = 24
  end
  object SaveDialog: TSavePictureDialog
    DefaultExt = 'svg'
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Options = [ofOverwritePrompt, ofPathMustExist, ofEnableSizing]
    Left = 456
    Top = 24
  end
end
