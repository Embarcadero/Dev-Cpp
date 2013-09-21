object CompileProgressForm: TCompileProgressForm
  Left = 318
  Top = 275
  BorderIcons = []
  BorderStyle = bsToolWindow
  Caption = 'Compile Progress'
  ClientHeight = 202
  ClientWidth = 276
  Color = clBtnFace
  Constraints.MinHeight = 229
  Constraints.MinWidth = 284
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnShow = FormShow
  DesignSize = (
    276
    202)
  PixelsPerInch = 96
  TextHeight = 13
  object btnClose: TButton
    Left = 100
    Top = 171
    Width = 75
    Height = 25
    Anchors = [akBottom]
    Cancel = True
    Caption = 'Cancel'
    Default = True
    TabOrder = 0
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 4
    Width = 275
    Height = 147
    ActivePage = TabSheet1
    Anchors = [akLeft, akTop, akRight, akBottom]
    Style = tsFlatButtons
    TabIndex = 0
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'Progress'
      DesignSize = (
        267
        116)
      object Bevel4: TBevel
        Left = 0
        Top = 0
        Width = 267
        Height = 116
        Anchors = [akLeft, akTop, akRight, akBottom]
        Style = bsRaised
      end
      object Bevel5: TBevel
        Left = 56
        Top = 88
        Width = 69
        Height = 21
      end
      object Bevel6: TBevel
        Left = 188
        Top = 88
        Width = 69
        Height = 21
      end
      object Bevel3: TBevel
        Left = 55
        Top = 60
        Width = 202
        Height = 21
        Anchors = [akLeft, akTop, akRight]
      end
      object Bevel2: TBevel
        Left = 55
        Top = 36
        Width = 202
        Height = 21
        Anchors = [akLeft, akTop, akRight]
      end
      object Bevel1: TBevel
        Left = 56
        Top = 8
        Width = 201
        Height = 21
        Anchors = [akLeft, akTop, akRight]
      end
      object Label1: TLabel
        Left = 8
        Top = 12
        Width = 43
        Height = 13
        Caption = 'Compiler:'
      end
      object lblCompiler: TLabel
        Left = 60
        Top = 12
        Width = 50
        Height = 13
        Caption = 'lblCompiler'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label3: TLabel
        Left = 8
        Top = 40
        Width = 33
        Height = 13
        Caption = 'Status:'
      end
      object lblStatus: TLabel
        Left = 60
        Top = 40
        Width = 40
        Height = 13
        Caption = 'lblStatus'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label5: TLabel
        Left = 8
        Top = 64
        Width = 19
        Height = 13
        Caption = 'File:'
      end
      object lblFile: TLabel
        Left = 60
        Top = 64
        Width = 34
        Height = 13
        Caption = 'lblFile'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label2: TLabel
        Left = 8
        Top = 92
        Width = 30
        Height = 13
        Caption = 'Errors:'
      end
      object Label4: TLabel
        Left = 136
        Top = 92
        Width = 48
        Height = 13
        Caption = 'Warnings:'
      end
      object lblErr: TLabel
        Left = 60
        Top = 92
        Width = 61
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'lblErr'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object lblWarn: TLabel
        Left = 192
        Top = 92
        Width = 60
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'lblWarn'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Log'
      ImageIndex = 1
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 267
        Height = 116
        Align = alClient
        Color = clBtnFace
        Font.Charset = GREEK_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'Memo1')
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
        WantReturns = False
        WordWrap = False
      end
    end
  end
  object pb: TProgressBar
    Left = 4
    Top = 151
    Width = 268
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    Min = 0
    Max = 100
    TabOrder = 2
  end
end
