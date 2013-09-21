object NewClassForm: TNewClassForm
  Left = 771
  Top = 293
  BorderStyle = bsDialog
  Caption = 'New class'
  ClientHeight = 500
  ClientWidth = 369
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
  object Label1: TLabel
    Left = 8
    Top = 12
    Width = 57
    Height = 13
    Caption = 'Class name:'
  end
  object Label4: TLabel
    Left = 8
    Top = 228
    Width = 76
    Height = 13
    Caption = 'Source filename'
  end
  object Label5: TLabel
    Left = 8
    Top = 256
    Width = 80
    Height = 13
    Caption = 'Header filename:'
  end
  object btnBrowseCpp: TSpeedButton
    Left = 338
    Top = 224
    Width = 23
    Height = 22
    Caption = '...'
    OnClick = btnBrowseCppClick
  end
  object btnBrowseH: TSpeedButton
    Left = 338
    Top = 252
    Width = 23
    Height = 22
    Caption = '...'
    OnClick = btnBrowseCppClick
  end
  object Label9: TLabel
    Left = 8
    Top = 36
    Width = 53
    Height = 13
    Caption = 'Arguments:'
  end
  object Label10: TLabel
    Left = 8
    Top = 64
    Width = 62
    Height = 13
    Caption = 'Construction:'
  end
  object txtName: TEdit
    Left = 88
    Top = 8
    Width = 273
    Height = 21
    TabOrder = 0
    Text = 'txtName'
    OnChange = txtNameChange
    OnKeyPress = txtNameKeyPress
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 116
    Width = 353
    Height = 93
    Caption = 'Inheritance'
    TabOrder = 3
    object Label2: TLabel
      Left = 16
      Top = 20
      Width = 70
      Height = 13
      Caption = 'Access scope:'
    end
    object Label3: TLabel
      Left = 16
      Top = 44
      Width = 82
      Height = 13
      Caption = 'Inherit from class:'
    end
    object Label6: TLabel
      Left = 16
      Top = 68
      Width = 80
      Height = 13
      Caption = 'Header filename:'
    end
    object cmbClass: TComboBox
      Left = 124
      Top = 40
      Width = 221
      Height = 21
      ItemHeight = 13
      TabOrder = 1
      Text = 'cmbClass'
      OnChange = cmbClassChange
    end
    object cmbScope: TComboBox
      Left = 124
      Top = 16
      Width = 221
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 2
      TabOrder = 0
      Text = 'public'
      Items.Strings = (
        'private'
        'protected'
        'public'
        'published')
    end
    object txtIncFile: TEdit
      Left = 124
      Top = 64
      Width = 221
      Height = 21
      TabOrder = 2
      Text = 'txtIncFile'
    end
  end
  object txtCppFile: TEdit
    Left = 132
    Top = 224
    Width = 205
    Height = 21
    TabOrder = 4
    Text = 'txtCppFile'
    OnChange = txtCppFileChange
  end
  object txtHFile: TEdit
    Left = 132
    Top = 252
    Width = 205
    Height = 21
    TabOrder = 5
    Text = 'txtHFile'
    OnChange = txtCppFileChange
  end
  object chkAddToProject: TCheckBox
    Left = 8
    Top = 284
    Width = 353
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Add to current project'
    TabOrder = 6
  end
  object btnCreate: TButton
    Left = 105
    Top = 468
    Width = 75
    Height = 25
    Caption = 'Create'
    Default = True
    ModalResult = 1
    TabOrder = 8
    OnClick = btnCreateClick
  end
  object btnCancel: TButton
    Left = 189
    Top = 468
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 9
  end
  object chkInherit: TCheckBox
    Left = 8
    Top = 92
    Width = 353
    Height = 17
    Caption = 'Inherit from another class'
    TabOrder = 2
    OnClick = chkInheritClick
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 304
    Width = 357
    Height = 153
    Caption = 'Comment'
    TabOrder = 7
    object Label7: TLabel
      Left = 8
      Top = 20
      Width = 56
      Height = 13
      Caption = 'Description:'
    end
    object Label8: TLabel
      Left = 8
      Top = 128
      Width = 26
      Height = 13
      Caption = 'Style:'
    end
    object memDescr: TMemo
      Left = 16
      Top = 36
      Width = 333
      Height = 81
      Lines.Strings = (
        'memDescr')
      ScrollBars = ssBoth
      TabOrder = 0
    end
    object cmbComment: TComboBox
      Left = 40
      Top = 124
      Width = 217
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 1
      Text = '/** doxygen style comment */'
      Items.Strings = (
        '/** doxygen style comment */'
        '/* C style comment */'
        '// C++ style comment')
    end
  end
  object txtArgs: TEdit
    Left = 88
    Top = 32
    Width = 273
    Height = 21
    TabOrder = 1
    Text = 'txtArgs'
  end
  object chkConstruct: TCheckBox
    Left = 88
    Top = 62
    Width = 121
    Height = 17
    Caption = 'Create Constructor'
    TabOrder = 10
  end
  object chkDestruct: TCheckBox
    Left = 232
    Top = 62
    Width = 121
    Height = 17
    Caption = 'Create Destructor'
    TabOrder = 11
  end
  object SaveDialog1: TSaveDialog
    Left = 332
    Top = 464
  end
end
