object ToolEditForm: TToolEditForm
  Left = 778
  Top = 377
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Edit tool'
  ClientHeight = 304
  ClientWidth = 484
  Color = clWindow
  Ctl3D = False
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  DesignSize = (
    484
    304)
  PixelsPerInch = 96
  TextHeight = 15
  object lblTitle: TLabel
    Left = 8
    Top = 12
    Width = 22
    Height = 15
    Caption = 'Title'
  end
  object lblProg: TLabel
    Left = 8
    Top = 40
    Width = 46
    Height = 15
    Caption = 'Program'
  end
  object lblWorkDir: TLabel
    Left = 8
    Top = 69
    Width = 96
    Height = 15
    Caption = 'Working Directory'
  end
  object lblParam: TLabel
    Left = 8
    Top = 98
    Width = 59
    Height = 15
    Caption = 'Parameters'
  end
  object lblMacros: TLabel
    Left = 7
    Top = 157
    Width = 93
    Height = 15
    Caption = 'Available Macros:'
  end
  object btnProg: TSpeedButton
    Left = 451
    Top = 36
    Width = 23
    Height = 22
    ImageIndex = 59
    ImageName = 'iconsnew-65'
    Images = dmMain.SVGImageListMenuStyle
    Flat = True
    OnClick = btnProgClick
  end
  object btnWorkDir: TSpeedButton
    Left = 451
    Top = 65
    Width = 23
    Height = 22
    ImageIndex = 59
    ImageName = 'iconsnew-65'
    Images = dmMain.SVGImageListMenuStyle
    Flat = True
    OnClick = btnWorkDirClick
  end
  object lblDesc: TMemo
    Left = 168
    Top = 180
    Width = 305
    Height = 51
    Color = clBtnFace
    Enabled = False
    ReadOnly = True
    TabOrder = 10
  end
  object edTitle: TEdit
    Left = 120
    Top = 8
    Width = 353
    Height = 21
    TabOrder = 0
  end
  object edProgram: TEdit
    Left = 120
    Top = 36
    Width = 321
    Height = 21
    TabOrder = 1
    OnChange = edProgramChange
    OnEnter = EditEnter
  end
  object edWorkDir: TEdit
    Left = 120
    Top = 65
    Width = 321
    Height = 21
    TabOrder = 2
    OnEnter = EditEnter
  end
  object edParams: TEdit
    Left = 120
    Top = 94
    Width = 353
    Height = 21
    TabOrder = 3
    OnChange = edParamsChange
    OnEnter = EditEnter
  end
  object btnCancel: TBitBtn
    Left = 304
    Top = 272
    Width = 85
    Height = 24
    Anchors = [akLeft, akBottom]
    Caption = '&Cancel'
    Images = dmMain.SVGImageListMenuStyle
    ModalResult = 2
    NumGlyphs = 2
    TabOrder = 8
    OnClick = btnCancelClick
  end
  object btnOk: TBitBtn
    Left = 214
    Top = 272
    Width = 81
    Height = 24
    Anchors = [akLeft, akBottom]
    Caption = '&OK'
    ModalResult = 1
    NumGlyphs = 2
    TabOrder = 7
  end
  object lstMacro: TListBox
    Left = 8
    Top = 180
    Width = 153
    Height = 77
    Color = 14548990
    IntegralHeight = True
    ItemHeight = 15
    Items.Strings = (
      '<DEFAULT>'
      '<EXECPATH>'
      '<EXENAME>'
      '<PROJECTPATH>'
      '<PROJECTFILE>'
      '<PROJECTNAME>'
      '<SOURCENAME>'
      '<SOURCEPATH>'
      '<SOURCESPCLIST>'
      '<WORDXY>'
      '<DEVCPPVERSION>')
    TabOrder = 4
    OnClick = lstMacroClick
    OnDblClick = btnInsertClick
  end
  object btnInsert: TBitBtn
    Left = 170
    Top = 236
    Width = 103
    Height = 24
    Caption = '&Insert Macro'
    ImageIndex = 60
    ImageName = 'iconsnew-66'
    Images = dmMain.SVGImageListMenuStyle
    NumGlyphs = 2
    TabOrder = 5
    OnClick = btnInsertClick
  end
  object btnHelp: TBitBtn
    Left = 399
    Top = 272
    Width = 75
    Height = 24
    Anchors = [akLeft, akBottom]
    Caption = '&Help'
    NumGlyphs = 2
    TabOrder = 6
    OnClick = HelpClick
  end
  object ParamText: TEdit
    Left = 8
    Top = 128
    Width = 465
    Height = 23
    AutoSize = False
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 9
  end
end
