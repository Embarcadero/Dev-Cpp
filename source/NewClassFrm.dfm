object NewClassForm: TNewClassForm
  Left = 1000
  Top = 72
  BorderStyle = bsDialog
  Caption = 'New class'
  ClientHeight = 338
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
  object lblClassName: TLabel
    Left = 8
    Top = 12
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object lblCppFile: TLabel
    Left = 8
    Top = 228
    Width = 76
    Height = 13
    Caption = 'Source filename'
  end
  object lblHFile: TLabel
    Left = 8
    Top = 256
    Width = 80
    Height = 13
    Caption = 'Header filename:'
  end
  object btnBrowseCpp: TSpeedButton
    Left = 412
    Top = 224
    Width = 23
    Height = 22
    Caption = '...'
    OnClick = btnBrowseCppClick
  end
  object btnBrowseH: TSpeedButton
    Left = 412
    Top = 252
    Width = 23
    Height = 22
    Caption = '...'
    OnClick = btnBrowseCppClick
  end
  object lblArguments: TLabel
    Left = 8
    Top = 36
    Width = 53
    Height = 13
    Caption = 'Arguments:'
  end
  object txtName: TEdit
    Left = 88
    Top = 8
    Width = 348
    Height = 21
    TabOrder = 0
    OnChange = txtNameChange
    OnKeyPress = txtNameKeyPress
  end
  object grpInheritance: TGroupBox
    Left = 8
    Top = 116
    Width = 425
    Height = 93
    Caption = 'Inheritance'
    TabOrder = 5
    object lblAccess: TLabel
      Left = 8
      Top = 20
      Width = 70
      Height = 13
      Caption = 'Access scope:'
    end
    object lblInherit: TLabel
      Left = 8
      Top = 44
      Width = 82
      Height = 13
      Caption = 'Inherit from class:'
    end
    object lblHeaderFile: TLabel
      Left = 8
      Top = 68
      Width = 80
      Height = 13
      Caption = 'Header filename:'
    end
    object cmbClass: TComboBox
      Left = 124
      Top = 40
      Width = 293
      Height = 21
      Enabled = False
      ItemHeight = 13
      TabOrder = 1
      OnChange = cmbClassChange
    end
    object cmbScope: TComboBox
      Left = 124
      Top = 16
      Width = 293
      Height = 21
      Style = csDropDownList
      Enabled = False
      ItemHeight = 13
      TabOrder = 0
      Items.Strings = (
        'private'
        'protected'
        'public')
    end
    object txtIncFile: TEdit
      Left = 124
      Top = 64
      Width = 293
      Height = 21
      Enabled = False
      TabOrder = 2
    end
  end
  object txtCppFile: TEdit
    Left = 132
    Top = 224
    Width = 277
    Height = 21
    TabOrder = 6
    OnChange = txtCppFileChange
  end
  object txtHFile: TEdit
    Left = 132
    Top = 252
    Width = 277
    Height = 21
    TabOrder = 7
    OnChange = txtCppFileChange
  end
  object chkAddToProject: TCheckBox
    Left = 8
    Top = 284
    Width = 425
    Height = 17
    Caption = 'Add to current project'
    Checked = True
    State = cbChecked
    TabOrder = 8
  end
  object btnCreate: TButton
    Left = 142
    Top = 308
    Width = 75
    Height = 25
    Caption = 'Create'
    Default = True
    ModalResult = 1
    TabOrder = 9
    OnClick = btnCreateClick
  end
  object btnCancel: TButton
    Left = 227
    Top = 308
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 10
  end
  object chkInherit: TCheckBox
    Left = 8
    Top = 92
    Width = 425
    Height = 17
    Caption = 'Inherit from another class'
    TabOrder = 4
    OnClick = chkInheritClick
  end
  object txtArgs: TEdit
    Left = 88
    Top = 32
    Width = 348
    Height = 21
    TabOrder = 1
  end
  object chkConstruct: TCheckBox
    Left = 88
    Top = 62
    Width = 169
    Height = 17
    Caption = 'Create Constructor'
    TabOrder = 2
  end
  object chkDestruct: TCheckBox
    Left = 264
    Top = 62
    Width = 169
    Height = 17
    Caption = 'Create Destructor'
    TabOrder = 3
  end
end
