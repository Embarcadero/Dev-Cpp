object BuildForm: TBuildForm
  Left = 269
  Top = 240
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Building Package...'
  ClientHeight = 45
  ClientWidth = 265
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox: TGroupBox
    Left = 0
    Top = 0
    Width = 265
    Height = 45
    Align = alClient
    Caption = 'Progress... '
    TabOrder = 0
    object ProgressBar: TProgressBar
      Left = 8
      Top = 18
      Width = 249
      Height = 16
      Min = 0
      Max = 10
      Step = 1
      TabOrder = 0
    end
  end
end
