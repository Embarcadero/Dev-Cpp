object frmExceptionsAnalyzer: TfrmExceptionsAnalyzer
  Left = 240
  Top = 172
  BorderStyle = bsDialog
  Caption = 'Oops!'
  ClientHeight = 369
  ClientWidth = 465
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Shape1: TShape
    Left = 0
    Top = 0
    Width = 465
    Height = 45
    Align = alTop
    Pen.Style = psClear
  end
  object lblError: TLabel
    Left = 84
    Top = 68
    Width = 373
    Height = 53
    AutoSize = False
    Caption = 'lblError'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
  end
  object lblTitle: TLabel
    Left = 52
    Top = 16
    Width = 304
    Height = 13
    Caption = 'An unexpected error has occured in the application...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
  object lblAddressTitle: TLabel
    Left = 8
    Top = 52
    Width = 41
    Height = 13
    Caption = 'Address:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lblAddress: TLabel
    Left = 84
    Top = 52
    Width = 48
    Height = 13
    Caption = 'lblAddress'
  end
  object lblErrorTitle: TLabel
    Left = 8
    Top = 68
    Width = 70
    Height = 13
    Caption = 'Error message:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object btnSend: TSpeedButton
    Left = 96
    Top = 130
    Width = 83
    Height = 25
    Caption = '&Send bug report'
    ParentShowHint = False
    ShowHint = True
    OnClick = btnSendClick
  end
  object btnView: TSpeedButton
    Left = 8
    Top = 130
    Width = 83
    Height = 25
    AllowAllUp = True
    GroupIndex = 1
    Caption = '&View bug report'
    ParentShowHint = False
    ShowHint = True
    OnClick = btnViewClick
  end
  object Bevel1: TBevel
    Left = 8
    Top = 160
    Width = 449
    Height = 2
  end
  object Bevel2: TBevel
    Left = 0
    Top = 45
    Width = 465
    Height = 2
    Align = alTop
  end
  object Image1: TImage
    Left = 8
    Top = 4
    Width = 32
    Height = 32
    AutoSize = True
    Picture.Data = {
      055449636F6E0000010001002020000001000800B00800001600000028000000
      2000000040000000010008000000000080040000000000000000000000010000
      0000000000000000000080000080000000808000800000008000800080800000
      C0C0C000C0DCC000F0CAA600CCFFFF0099FFFF0066FFFF0033FFFF00FFCCFF00
      CCCCFF0099CCFF0066CCFF0033CCFF0000CCFF00FF99FF00CC99FF009999FF00
      6699FF003399FF000099FF00FF66FF00CC66FF009966FF006666FF003366FF00
      0066FF00FF33FF00CC33FF009933FF006633FF003333FF000033FF00CC00FF00
      9900FF006600FF003300FF00FFFFCC00CCFFCC0099FFCC0066FFCC0066FFCC00
      33FFCC0000FFCC00FFCCCC00CCCCCC0099CCCC0066CCCC0033CCCC0000CCCC00
      FF99CC00CC99CC009999CC006699CC003399CC000099CC00FF66CC00CC66CC00
      9966CC006666CC003366CC000066CC00FF33CC00CC33CC009933CC006633CC00
      3333CC000033CC00FF00CC00CC00CC009900CC006600CC003300CC000000CC00
      FFFF9900CCFF990099FF990066FF990033FF990000FF9900FFCC9900CCCC9900
      99CC990066CC990033CC990000CC9900FF999900CC9999009999990066999900
      3399990000999900FF669900CC66990099669900666699003366990000669900
      FF339900CC33990099339900663399003333990000339900FF009900CC009900
      99009900660099003300990000009900FFFF6600CCFF660099FF660066FF6600
      33FF660000FF6600FFCC6600CCCC660099CC660066CC660033CC660000CC6600
      FF996600CC99660099996600669966003399660000996600FF666600CC666600
      99666600666666003366660000666600FF336600CC3366009933660066336600
      3333660000336600FF006600CC00660099006600660066003300660000006600
      FFFF3300CCFF330099FF330066FF330033FF330000FF3300FFCC3300CCCC3300
      99CC330066CC330033CC330000CC3300FF993300CC9933009999330066993300
      3399330000993300FF663300CC66330099663300666633003366330000663300
      FF333300CC33330099333300663333003333330000333300FF003300CC003300
      99003300660033003300330000003300CCFF000099FF000066FF000033FF0000
      FFCC0000CCCC000099CC000066CC000033CC000000CC0000FF990000CC990000
      99990000669900003399000000990000FF660000CC6600009966000066660000
      0066000033660000FF330000CC33000099330000663300003333000000330000
      CC0000009900000066000000330000000000DD000000BB000000AA0000008800
      0000770000005500000044000000220000DD000000BB000000AA000000880000
      00770000005500000044000000220000DDDDDD00555555007777770077777700
      44444400222222001111110077000000550000004400000022000000F0FBFF00
      A4A0A000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000
      FFFFFF0000000000000000000000000000000000000000000000000000000000
      00000000000000000000F1F10000000000000000000000000000000000000000
      0000000000000000F100F0F0B3F0F0F000000000000000000000000000000000
      0000000000000000F1F0F0B3B3B3B3B3B3B30000000000000000000000240000
      0000000000000000F0F0B3B3B33333EDEDB3F000000000000000000000000000
      0000000000000000F0B3B3B3ED330A0A33EDED000000000000240000001E0000
      00002400000000F1F0B3B3B333330A0A0A0A0A000000000000001E0000000000
      00000000000000F0B3B3EDED3333330A0A0A0A33000000000000000000180000
      1E000000000000F1F0B3EDEDED0A0A0A0A0A0A0A000000000000001800000018
      00000000000000F1F0B3EDED3333330A0A0A0A33000000000000000000000000
      0000000000000000F1F0B3B333330A330A0A33ED000000000000EC0718001800
      1E00240000000000F1B3B3B3ED3333330AEDED000000000000EC000000000000
      000000000000000000F1B3B3B3B3EDEDEDEDB30000000000EC00000018000018
      000000000000000000F1F1B3F0EDB3B3F0F0000000000000EC00001E00000000
      000000000000000000F1F1F1B3F0F0F100000000000000EC0000000000001E00
      001E000000000000000000F10000000000000000000000EC0000240000000000
      0000240000000000000000000000000000000000000000EC0000000000000000
      00000000000000000000000000000000000000000000EC000000000000000000
      00000000000000000000000000000000000000000000EC000000000000D8D8D8
      000000000000000000000000000000000000000000EC000000000000D8FFFFFF
      D80000000000000000000000000000000000000000EC0000000000D8FF55312A
      FFD80000000000000000000000EC0000000000ECEC000000000000D8FFA37F55
      2AD8000000000000000000000000ECECECECEC0000000000000000D8CBA3A355
      FFD800000000000000000000000000000000000000000000000000D8CBCBA355
      31D80000000000000000000000000000000000000000000000000000D8CBCB7F
      D8000000000000000000000000000000000000000000000000000000D8CBA9CB
      D800000000000000000000000000000000000000000000000000000000D8CBCB
      D800000000000000000000000000000000000000000000000000000000D8CBD8
      0000000000000000000000000000000000000000000000000000000000D8CBD8
      000000000000000000000000000000000000000000000000000000000000D800
      000000000000000000000000000000000000000000000000000000000000D800
      0000000000000000000000000000000000000000000000000000000000000000
      00000000F80FFFFFE003FFFFC001FFFF8000FFBF8000FFFF00007BBD00007DFF
      00007FB700007EEF00007FFF00007C5500007BFF8000F76F8000F6FFC001EFDB
      E003EDFDF80FEFFFFC1FDFFFFC1FDF8FFF7FBF07FF7FBE03FFBE7E03FFC1FE03
      FFFFFE03FFFFFF07FFFFFF07FFFFFF87FFFFFF8FFFFFFF8FFFFFFFDFFFFFFFDF
      FFFFFFFF0000000000000000}
  end
  object btnHelp: TSpeedButton
    Left = 384
    Top = 130
    Width = 73
    Height = 25
    Caption = '&Help'
    OnClick = btnHelpClick
  end
  object btnClose: TButton
    Left = 196
    Top = 130
    Width = 83
    Height = 25
    Cancel = True
    Caption = '&Continue'
    Default = True
    ModalResult = 1
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
  end
  object btnTerminate: TButton
    Left = 284
    Top = 130
    Width = 83
    Height = 25
    Caption = '&Terminate'
    ModalResult = 3
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 168
    Width = 449
    Height = 197
    ActivePage = tabMemory
    Style = tsFlatButtons
    TabIndex = 2
    TabOrder = 2
    TabStop = False
    object tabProgram: TTabSheet
      Caption = 'Program'
      ImageIndex = 1
      object Label1: TLabel
        Left = 12
        Top = 16
        Width = 66
        Height = 13
        Caption = 'Program path:'
      end
      object lblProgramPath: TLabel
        Left = 100
        Top = 16
        Width = 71
        Height = 13
        Caption = 'lblProgramPath'
      end
      object Label3: TLabel
        Left = 12
        Top = 36
        Width = 79
        Height = 13
        Caption = 'Program version:'
      end
      object lblProgramVersion: TLabel
        Left = 100
        Top = 36
        Width = 84
        Height = 13
        Caption = 'lblProgramVersion'
      end
    end
    object tabMachine: TTabSheet
      Caption = 'Machine'
      ImageIndex = 2
      object Label2: TLabel
        Left = 12
        Top = 16
        Width = 41
        Height = 13
        Caption = 'Platform:'
      end
      object lblPlatform: TLabel
        Left = 100
        Top = 16
        Width = 48
        Height = 13
        Caption = 'lblPlatform'
      end
      object Label5: TLabel
        Left = 12
        Top = 36
        Width = 55
        Height = 13
        Caption = 'OS version:'
      end
      object lblOSversion: TLabel
        Left = 100
        Top = 36
        Width = 59
        Height = 13
        Caption = 'lblOSversion'
      end
      object Label7: TLabel
        Left = 12
        Top = 56
        Width = 69
        Height = 13
        Caption = 'Additional info:'
      end
      object lblAdditionalInfo: TLabel
        Left = 100
        Top = 56
        Width = 74
        Height = 13
        Caption = 'lblAdditionalInfo'
      end
      object Label9: TLabel
        Left = 12
        Top = 76
        Width = 77
        Height = 13
        Caption = 'Computer name:'
      end
      object lblComputerName: TLabel
        Left = 100
        Top = 76
        Width = 83
        Height = 13
        Caption = 'lblComputerName'
      end
    end
    object tabMemory: TTabSheet
      Caption = 'Memory'
      ImageIndex = 3
      object Label10: TLabel
        Left = 166
        Top = 144
        Width = 63
        Height = 13
        Caption = 'Memory load:'
      end
      object lblMemoryLoad: TLabel
        Left = 238
        Top = 144
        Width = 71
        Height = 13
        Caption = 'lblMemoryLoad'
      end
      object GroupBox1: TGroupBox
        Left = 12
        Top = 16
        Width = 133
        Height = 117
        Caption = 'Physical memory'
        TabOrder = 0
        object Label4: TLabel
          Left = 4
          Top = 24
          Width = 27
          Height = 13
          Caption = 'Total:'
        end
        object lblTotalPhys: TLabel
          Left = 52
          Top = 24
          Width = 73
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'lblTotalPhys'
          WordWrap = True
        end
        object Label8: TLabel
          Left = 4
          Top = 44
          Width = 32
          Height = 13
          Caption = 'In use:'
        end
        object lblUsedPhys: TLabel
          Left = 52
          Top = 44
          Width = 73
          Height = 29
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'lblUsedPhys'
          WordWrap = True
        end
        object Label11: TLabel
          Left = 4
          Top = 80
          Width = 24
          Height = 13
          Caption = 'Free:'
        end
        object lblFreePhys: TLabel
          Left = 52
          Top = 80
          Width = 73
          Height = 29
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'lblFreePhys'
          WordWrap = True
        end
      end
      object GroupBox2: TGroupBox
        Left = 152
        Top = 16
        Width = 133
        Height = 117
        Caption = 'Virtual memory'
        TabOrder = 1
        object Label6: TLabel
          Left = 12
          Top = 24
          Width = 27
          Height = 13
          Caption = 'Total:'
        end
        object lblTotalVirt: TLabel
          Left = 52
          Top = 24
          Width = 73
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'lblTotalVirt'
          WordWrap = True
        end
        object Label12: TLabel
          Left = 12
          Top = 44
          Width = 32
          Height = 13
          Caption = 'In use:'
        end
        object lblUsedVirt: TLabel
          Left = 52
          Top = 44
          Width = 73
          Height = 29
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'lblUsedVirt'
          WordWrap = True
        end
        object Label14: TLabel
          Left = 12
          Top = 80
          Width = 24
          Height = 13
          Caption = 'Free:'
        end
        object lblFreeVirt: TLabel
          Left = 52
          Top = 80
          Width = 73
          Height = 29
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'lblFreeVirt'
          WordWrap = True
        end
      end
      object GroupBox3: TGroupBox
        Left = 292
        Top = 16
        Width = 133
        Height = 117
        Caption = 'Cache memory'
        TabOrder = 2
        object Label16: TLabel
          Left = 12
          Top = 24
          Width = 27
          Height = 13
          Caption = 'Total:'
        end
        object lblTotalCache: TLabel
          Left = 52
          Top = 24
          Width = 73
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'lblTotalCache'
          WordWrap = True
        end
        object Label18: TLabel
          Left = 12
          Top = 44
          Width = 32
          Height = 13
          Caption = 'In use:'
        end
        object lblUsedCache: TLabel
          Left = 52
          Top = 44
          Width = 73
          Height = 29
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'lblUsedCache'
          WordWrap = True
        end
        object Label20: TLabel
          Left = 12
          Top = 80
          Width = 24
          Height = 13
          Caption = 'Free:'
        end
        object lblFreeCache: TLabel
          Left = 52
          Top = 80
          Width = 73
          Height = 29
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'lblFreeCache'
          WordWrap = True
        end
      end
    end
    object tabStackTrace: TTabSheet
      Caption = 'StackTrace'#169
      object memStackTrace: TMemo
        Left = 0
        Top = 0
        Width = 441
        Height = 166
        Align = alClient
        Font.Charset = GREEK_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'StackTrace')
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object tabFullReport: TTabSheet
      Caption = 'Bug report'
      ImageIndex = 4
      object memBugReport: TMemo
        Left = 0
        Top = 0
        Width = 441
        Height = 166
        Align = alClient
        Font.Charset = GREEK_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'StackTrace')
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
  end
  object XPMenu: TXPMenu
    DimLevel = 30
    GrayLevel = 10
    Font.Charset = ANSI_CHARSET
    Font.Color = clMenuText
    Font.Height = -11
    Font.Name = 'Microsoft Sans Serif'
    Font.Style = []
    Color = clBtnFace
    DrawMenuBar = False
    IconBackColor = clBtnFace
    MenuBarColor = clBtnFace
    SelectColor = clHighlight
    SelectBorderColor = clHighlight
    SelectFontColor = clMenuText
    DisabledColor = clInactiveCaption
    SeparatorColor = clBtnFace
    CheckedColor = clHighlight
    IconWidth = 24
    DrawSelect = True
    UseSystemColors = True
    UseDimColor = False
    OverrideOwnerDraw = False
    Gradient = False
    FlatMenu = False
    AutoDetect = True
    Active = False
    Left = 168
    Top = 64
  end
end
