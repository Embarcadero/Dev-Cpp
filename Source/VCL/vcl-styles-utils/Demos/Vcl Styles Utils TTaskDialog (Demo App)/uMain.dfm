object FrmTaskDlgMain: TFrmTaskDlgMain
  Left = 413
  Top = 324
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Main'
  ClientHeight = 151
  ClientWidth = 429
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LblStyles: TLabel
    Left = 8
    Top = 13
    Width = 39
    Height = 13
    Caption = 'Styles : '
  end
  object BtnCommandLinks: TSpeedButton
    Left = 322
    Top = 63
    Width = 97
    Height = 22
    Caption = 'Command Links'
    OnClick = BtnCommandLinksClick
  end
  object BtnFooter: TSpeedButton
    Left = 322
    Top = 91
    Width = 97
    Height = 22
    Caption = 'Footer'
    OnClick = BtnFooterClick
  end
  object BtnHiperLink: TSpeedButton
    Left = 219
    Top = 91
    Width = 97
    Height = 22
    Caption = 'HiperLink'
    OnClick = BtnHiperLinkClick
  end
  object BtnCustomIcon: TSpeedButton
    Left = 115
    Top = 91
    Width = 97
    Height = 22
    Caption = 'Custom Icon'
    OnClick = BtnCustomIconClick
  end
  object BtnQuestion: TSpeedButton
    Left = 115
    Top = 63
    Width = 97
    Height = 22
    Caption = 'Question'
    OnClick = BtnQuestionClick
  end
  object BtnExpamdButton: TSpeedButton
    Left = 8
    Top = 91
    Width = 97
    Height = 22
    Caption = 'Expand button'
    OnClick = BtnExpamdButtonClick
  end
  object BtnCheckBox: TSpeedButton
    Left = 8
    Top = 119
    Width = 97
    Height = 22
    Caption = 'CheckBox'
    OnClick = BtnCheckBoxClick
  end
  object BtnCustomButtons: TSpeedButton
    Left = 218
    Top = 63
    Width = 98
    Height = 22
    Caption = 'Custom Buttons'
    OnClick = BtnCustomButtonsClick
  end
  object BtnHello: TSpeedButton
    Left = 8
    Top = 63
    Width = 97
    Height = 22
    Caption = 'Hello World'
    OnClick = BtnHelloClick
  end
  object ComboBox1: TComboBox
    Left = 53
    Top = 10
    Width = 244
    Height = 21
    Style = csDropDownList
    TabOrder = 0
    OnSelect = ComboBox1Select
  end
  object CheckBoxEnableSysControls: TCheckBox
    Left = 8
    Top = 37
    Width = 112
    Height = 17
    Caption = 'Enable SysControls'
    Checked = True
    State = cbChecked
    TabOrder = 1
    OnClick = CheckBoxEnableSysControlsClick
  end
  object BrnRadioButtons: TButton
    Left = 115
    Top = 119
    Width = 98
    Height = 22
    Caption = 'Radio Buttons'
    TabOrder = 2
    OnClick = BrnRadioButtonsClick
  end
  object BtnProgress: TButton
    Left = 218
    Top = 119
    Width = 97
    Height = 22
    Caption = 'ProgressBar'
    TabOrder = 3
    OnClick = BtnProgressClick
  end
  object BtnMarquee: TButton
    Left = 322
    Top = 119
    Width = 97
    Height = 22
    Caption = 'Marquee'
    TabOrder = 4
    OnClick = BtnMarqueeClick
  end
  object CheckBoxHookDialogsIcons: TCheckBox
    Left = 126
    Top = 37
    Width = 112
    Height = 17
    Caption = 'Hook Dialog Icons'
    Checked = True
    State = cbChecked
    TabOrder = 5
    OnClick = CheckBoxHookDialogsIconsClick
  end
end
