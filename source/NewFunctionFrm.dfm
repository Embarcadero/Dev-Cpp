object NewFunctionForm: TNewFunctionForm
  Left = 297
  Top = 417
  BorderStyle = bsDialog
  Caption = 'New member function'
  ClientHeight = 270
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
  object lblReturnType: TLabel
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
  object lblArguments: TLabel
    Left = 8
    Top = 60
    Width = 53
    Height = 13
    Caption = 'Arguments:'
  end
  object lblImplementIn: TLabel
    Left = 8
    Top = 84
    Width = 62
    Height = 13
    Caption = 'Implement in:'
  end
  object rgScope: TRadioGroup
    Left = 8
    Top = 112
    Width = 121
    Height = 97
    Caption = 'Access scope'
    ItemIndex = 2
    Items.Strings = (
      'Private'
      'Protected'
      'Public')
    TabOrder = 4
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
  object txtArguments: TEdit
    Left = 88
    Top = 56
    Width = 348
    Height = 21
    TabOrder = 2
  end
  object btnCreate: TButton
    Left = 134
    Top = 240
    Width = 75
    Height = 25
    Caption = 'Create'
    Default = True
    ModalResult = 1
    TabOrder = 7
    OnClick = btnCreateClick
  end
  object btnCancel: TButton
    Left = 219
    Top = 240
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 8
  end
  object cmbClass: TComboBox
    Left = 88
    Top = 80
    Width = 348
    Height = 21
    AutoDropDown = True
    ItemHeight = 13
    Sorted = True
    TabOrder = 3
    Items.Strings = (
      'bool'
      'double'
      'float'
      'int'
      'void')
  end
  object chkToDo: TCheckBox
    Left = 12
    Top = 216
    Width = 421
    Height = 17
    Caption = 'Add TODO item'
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
  object grpAttr: TGroupBox
    Left = 136
    Top = 112
    Width = 177
    Height = 97
    Caption = 'Attributes'
    TabOrder = 5
    object chkStatic: TCheckBox
      Left = 12
      Top = 20
      Width = 157
      Height = 17
      Caption = 'Static'
      TabOrder = 0
      OnClick = chkStaticClick
    end
    object chkVirtual: TCheckBox
      Left = 12
      Top = 44
      Width = 69
      Height = 17
      Caption = 'Virtual'
      TabOrder = 1
      OnClick = chkStaticClick
    end
    object chkPure: TCheckBox
      Left = 96
      Top = 44
      Width = 57
      Height = 17
      Caption = 'Pure'
      TabOrder = 2
      OnClick = chkStaticClick
    end
    object chkInline: TCheckBox
      Left = 12
      Top = 68
      Width = 157
      Height = 17
      Caption = 'Inline'
      TabOrder = 3
      OnClick = chkStaticClick
    end
  end
end
