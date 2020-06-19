object DirDlg: TDirDlg
  Left = 219
  Top = 178
  BorderStyle = bsDialog
  Caption = 'Select Directory'
  ClientHeight = 262
  ClientWidth = 384
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 40
    Width = 281
    Height = 217
    Shape = bsFrame
  end
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 281
    Height = 25
    BevelInner = bvLowered
    TabOrder = 4
  end
  object OKBtn: TButton
    Left = 300
    Top = 8
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object CancelBtn: TButton
    Left = 300
    Top = 38
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object DriveComboBox1: TDriveComboBox
    Left = 24
    Top = 56
    Width = 249
    Height = 19
    DirList = DirectoryListBox1
    TabOrder = 2
  end
  object DirectoryListBox1: TDirectoryListBox
    Left = 24
    Top = 80
    Width = 249
    Height = 169
    ItemHeight = 16
    TabOrder = 3
    OnChange = DirectoryListBox1Change
  end
end
