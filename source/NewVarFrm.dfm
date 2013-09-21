object NewVarForm: TNewVarForm
  Left = 1171
  Top = 586
  BorderStyle = bsDialog
  Caption = 'New variable'
  ClientHeight = 374
  ClientWidth = 444
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblType: TLabel
    Left = 8
    Top = 12
    Width = 58
    Height = 13
    Caption = 'Return type:'
  end
  object lblName: TLabel
    Left = 8
    Top = 36
    Width = 70
    Height = 13
    Caption = 'Variable name:'
  end
  object lblImplementIn: TLabel
    Left = 8
    Top = 60
    Width = 62
    Height = 13
    Caption = 'Implement in:'
  end
  object rgScope: TRadioGroup
    Left = 8
    Top = 88
    Width = 425
    Height = 41
    Caption = 'Access scope'
    Columns = 3
    ItemIndex = 0
    Items.Strings = (
      'Private'
      'Protected'
      'Public')
    TabOrder = 3
  end
  object txtType: TEdit
    Left = 88
    Top = 8
    Width = 348
    Height = 21
    TabOrder = 0
    OnChange = txtTypeChange
  end
  object txtName: TEdit
    Left = 88
    Top = 32
    Width = 348
    Height = 21
    TabOrder = 1
    OnChange = txtTypeChange
  end
  object btnCreate: TButton
    Left = 142
    Top = 344
    Width = 75
    Height = 25
    Caption = 'Create'
    Default = True
    ModalResult = 1
    TabOrder = 4
    OnClick = btnCreateClick
  end
  object btnCancel: TButton
    Left = 227
    Top = 344
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object cmbClass: TComboBox
    Left = 88
    Top = 56
    Width = 348
    Height = 21
    AutoDropDown = True
    ItemHeight = 13
    Sorted = True
    TabOrder = 2
    Items.Strings = (
      'bool'
      'double'
      'float'
      'int'
      'void')
  end
  object grpReadFunc: TGroupBox
    Left = 8
    Top = 160
    Width = 425
    Height = 70
    TabOrder = 6
    object Label3: TLabel
      Left = 8
      Top = 20
      Width = 31
      Height = 13
      Alignment = taRightJustify
      Caption = 'Name:'
    end
    object chkInlineR: TCheckBox
      Left = 8
      Top = 44
      Width = 53
      Height = 17
      Caption = 'Inline'
      Enabled = False
      TabOrder = 0
    end
    object txtReadFunc: TEdit
      Left = 84
      Top = 16
      Width = 333
      Height = 21
      TabOrder = 1
      OnChange = txtTypeChange
      OnKeyUp = txtReadFuncKeyUp
    end
  end
  object grpWriteFunc: TGroupBox
    Left = 8
    Top = 260
    Width = 425
    Height = 70
    TabOrder = 7
    object Label4: TLabel
      Left = 8
      Top = 20
      Width = 31
      Height = 13
      Alignment = taRightJustify
      Caption = 'Name:'
    end
    object chkInlineW: TCheckBox
      Left = 8
      Top = 44
      Width = 53
      Height = 17
      Caption = 'Inline'
      Enabled = False
      TabOrder = 0
    end
    object txtWriteFunc: TEdit
      Left = 84
      Top = 16
      Width = 333
      Height = 21
      TabOrder = 1
      OnChange = txtTypeChange
      OnKeyUp = txtWriteFuncKeyUp
    end
  end
  object chkReadFunc: TCheckBox
    Left = 12
    Top = 140
    Width = 333
    Height = 17
    Caption = 'Create member function to read from this variable'
    TabOrder = 8
    OnClick = chkReadFuncClick
  end
  object chkWriteFunc: TCheckBox
    Left = 12
    Top = 240
    Width = 333
    Height = 17
    Caption = 'Create member function to write to this variable'
    TabOrder = 9
    OnClick = chkWriteFuncClick
  end
end
