object CompileProgressForm: TCompileProgressForm
  Left = 721
  Top = 525
  BorderIcons = []
  BorderStyle = bsToolWindow
  Caption = 'Compile Progress'
  ClientHeight = 195
  ClientWidth = 582
  Color = clBtnFace
  Constraints.MinHeight = 229
  Constraints.MinWidth = 284
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    582
    195)
  PixelsPerInch = 96
  TextHeight = 15
  object Bevel5: TBevel
    Left = 70
    Top = 94
    Width = 60
    Height = 21
    Anchors = []
  end
  object Bevel6: TBevel
    Left = 260
    Top = 94
    Width = 60
    Height = 21
    Anchors = []
  end
  object Bevel3: TBevel
    Left = 70
    Top = 66
    Width = 250
    Height = 21
    Anchors = []
  end
  object Bevel2: TBevel
    Left = 70
    Top = 37
    Width = 250
    Height = 21
    Anchors = []
  end
  object Bevel1: TBevel
    Left = 70
    Top = 8
    Width = 250
    Height = 21
    Anchors = []
  end
  object infoCompiler: TLabel
    Left = 8
    Top = 12
    Width = 52
    Height = 15
    Anchors = []
    Caption = 'Compiler:'
  end
  object lblCompiler: TLabel
    Left = 76
    Top = 12
    Width = 62
    Height = 15
    Anchors = []
    Caption = 'lblCompiler'
  end
  object infoStatus: TLabel
    Left = 8
    Top = 41
    Width = 35
    Height = 15
    Anchors = []
    Caption = 'Status:'
  end
  object lblStatus: TLabel
    Left = 76
    Top = 41
    Width = 45
    Height = 15
    Anchors = []
    Caption = 'lblStatus'
  end
  object infoFile: TLabel
    Left = 8
    Top = 70
    Width = 21
    Height = 15
    Anchors = []
    Caption = 'File:'
  end
  object lblFile: TLabel
    Left = 76
    Top = 70
    Width = 31
    Height = 15
    Anchors = []
    Caption = 'lblFile'
  end
  object infoErrors: TLabel
    Left = 8
    Top = 98
    Width = 33
    Height = 15
    Anchors = []
    Caption = 'Errors:'
  end
  object infoWarnings: TLabel
    Left = 192
    Top = 98
    Width = 53
    Height = 15
    Anchors = []
    Caption = 'Warnings:'
  end
  object lblErr: TLabel
    Left = 76
    Top = 98
    Width = 48
    Height = 15
    Alignment = taRightJustify
    Anchors = []
    AutoSize = False
    Caption = 'lblErr'
  end
  object lblWarn: TLabel
    Left = 266
    Top = 98
    Width = 48
    Height = 15
    Alignment = taRightJustify
    Anchors = []
    AutoSize = False
    Caption = 'lblWarn'
  end
  object btnClose: TButton
    Left = 202
    Top = 158
    Width = 200
    Height = 30
    Anchors = []
    Cancel = True
    Caption = 'Cancel'
    Default = True
    TabOrder = 0
  end
  object pb: TProgressBar
    Left = 4
    Top = 125
    Width = 574
    Height = 24
    Anchors = []
    Step = 1
    TabOrder = 1
  end
  object memoMiniLog: TMemo
    Left = 328
    Top = 9
    Width = 249
    Height = 105
    Anchors = []
    Color = clBtnFace
    Lines.Strings = (
      'Memo1')
    ReadOnly = True
    TabOrder = 2
    WantReturns = False
    WordWrap = False
  end
end
