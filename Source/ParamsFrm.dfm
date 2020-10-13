object ParamsForm: TParamsForm
  Left = 581
  Top = 259
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Parameters'
  ClientHeight = 170
  ClientWidth = 297
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
  PixelsPerInch = 96
  TextHeight = 15
  object grpParameters: TGroupBox
    Left = 8
    Top = 8
    Width = 281
    Height = 56
    Caption = 'Parameters to pass to your program :'
    TabOrder = 0
    object ParamEdit: TEdit
      Left = 8
      Top = 22
      Width = 264
      Height = 21
      TabOrder = 0
    end
  end
  object grpHost: TGroupBox
    Left = 8
    Top = 72
    Width = 281
    Height = 56
    Caption = 'Host Application :'
    TabOrder = 1
    object LoadBtn: TSpeedButton
      Left = 248
      Top = 21
      Width = 22
      Height = 22
      ImageIndex = 59
      ImageName = 'iconsnew-65'
      Images = dmMain.SVGImageListMenuStyle
      Flat = True
      OnClick = LoadBtnClick
    end
    object HostEdit: TEdit
      Left = 8
      Top = 22
      Width = 234
      Height = 21
      TabOrder = 0
    end
  end
  object OkBtn: TBitBtn
    Left = 136
    Top = 138
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    NumGlyphs = 2
    TabOrder = 2
  end
  object CancelBtn: TBitBtn
    Left = 214
    Top = 138
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    NumGlyphs = 2
    TabOrder = 3
  end
end
