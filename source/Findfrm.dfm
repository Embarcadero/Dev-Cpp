object frmFind: TfrmFind
  Left = 1201
  Top = 406
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Find Text'
  ClientHeight = 259
  ClientWidth = 305
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  DesignSize = (
    305
    259)
  PixelsPerInch = 96
  TextHeight = 13
  object btnFind: TButton
    Left = 8
    Top = 229
    Width = 100
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Find'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = btnFindClick
  end
  object btnCancel: TButton
    Left = 197
    Top = 229
    Width = 100
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object FindTabs: TTabControl
    Left = 0
    Top = 0
    Width = 305
    Height = 222
    Align = alTop
    TabOrder = 2
    Tabs.Strings = (
      'Find'
      'Find in Files')
    TabIndex = 0
    OnChange = FindTabsChange
    object lblFind: TLabel
      Left = 8
      Top = 29
      Width = 56
      Height = 13
      Caption = '&Text to find:'
      FocusControl = cboFindText
    end
    object cboFindText: TComboBox
      Left = 8
      Top = 46
      Width = 289
      Height = 21
      ItemHeight = 13
      TabOrder = 0
    end
    object grpOptions: TGroupBox
      Left = 8
      Top = 74
      Width = 140
      Height = 69
      Caption = '  Options:  '
      TabOrder = 1
      object cbMatchCase: TCheckBox
        Left = 8
        Top = 16
        Width = 120
        Height = 17
        Caption = 'C&ase sensitive'
        TabOrder = 0
      end
      object cbWholeWord: TCheckBox
        Left = 8
        Top = 40
        Width = 120
        Height = 17
        Caption = '&Whole words only'
        TabOrder = 1
      end
    end
    object grpDirection: TGroupBox
      Left = 156
      Top = 74
      Width = 140
      Height = 68
      Caption = '  Direction:  '
      TabOrder = 2
      object rbBackward: TRadioButton
        Left = 8
        Top = 40
        Width = 110
        Height = 17
        Caption = '&Backward'
        TabOrder = 0
      end
      object rbForward: TRadioButton
        Left = 7
        Top = 17
        Width = 110
        Height = 17
        Caption = '&Forward'
        Checked = True
        TabOrder = 1
        TabStop = True
      end
    end
    object grpWhere: TGroupBox
      Left = 156
      Top = 74
      Width = 140
      Height = 68
      Caption = '  Where:  '
      TabOrder = 3
      object rbProjectFiles: TRadioButton
        Left = 7
        Top = 17
        Width = 110
        Height = 17
        Caption = 'Files in Project'
        Checked = True
        TabOrder = 0
        TabStop = True
      end
      object rbOpenFIles: TRadioButton
        Left = 8
        Top = 40
        Width = 110
        Height = 17
        Caption = 'Open Files'
        TabOrder = 1
      end
    end
    object grpScope: TGroupBox
      Left = 8
      Top = 148
      Width = 140
      Height = 64
      Caption = '  Scope:  '
      TabOrder = 4
      object rbEntireScope: TRadioButton
        Left = 8
        Top = 40
        Width = 110
        Height = 17
        Caption = 'Entire &scope'
        TabOrder = 0
      end
      object rbGlobal: TRadioButton
        Left = 8
        Top = 16
        Width = 110
        Height = 17
        Caption = '&Global'
        Checked = True
        TabOrder = 1
        TabStop = True
      end
    end
    object grpOrigin: TGroupBox
      Left = 156
      Top = 148
      Width = 140
      Height = 64
      Caption = '  Origin:  '
      TabOrder = 5
      object rbFromCursor: TRadioButton
        Left = 8
        Top = 16
        Width = 110
        Height = 17
        Caption = 'From &cursor'
        Checked = True
        TabOrder = 0
        TabStop = True
      end
      object rbSelectedOnly: TRadioButton
        Left = 8
        Top = 40
        Width = 110
        Height = 17
        Caption = '&Selected only'
        TabOrder = 1
      end
    end
  end
end
