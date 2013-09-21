object CPUForm: TCPUForm
  Left = 140
  Top = 57
  Width = 569
  Height = 476
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'CPU Window'
  Color = clBtnFace
  Constraints.MinHeight = 476
  Constraints.MinWidth = 569
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    561
    449)
  PixelsPerInch = 96
  TextHeight = 13
  object gbAsm: TGroupBox
    Left = 8
    Top = 8
    Width = 402
    Height = 433
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Assembler Code :'
    TabOrder = 0
    DesignSize = (
      402
      433)
    object lblFunc: TLabel
      Left = 8
      Top = 19
      Width = 47
      Height = 13
      Caption = 'Function: '
    end
    object edFunc: TEdit
      Left = 88
      Top = 16
      Width = 305
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnKeyPress = edFuncKeyPress
    end
    object CodeList: TSynEdit
      Left = 8
      Top = 44
      Width = 385
      Height = 381
      Cursor = crIBeam
      Anchors = [akLeft, akTop, akRight, akBottom]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier New'
      Font.Pitch = fpFixed
      Font.Style = []
      ParentColor = False
      ParentFont = False
      TabOrder = 1
      Gutter.Font.Charset = DEFAULT_CHARSET
      Gutter.Font.Color = clWindowText
      Gutter.Font.Height = -11
      Gutter.Font.Name = 'Terminal'
      Gutter.Font.Style = []
      Gutter.Visible = False
      Keystrokes = <
        item
          Command = ecUp
          ShortCut = 38
        end
        item
          Command = ecSelUp
          ShortCut = 8230
        end
        item
          Command = ecScrollUp
          ShortCut = 16422
        end
        item
          Command = ecDown
          ShortCut = 40
        end
        item
          Command = ecSelDown
          ShortCut = 8232
        end
        item
          Command = ecScrollDown
          ShortCut = 16424
        end
        item
          Command = ecLeft
          ShortCut = 37
        end
        item
          Command = ecSelLeft
          ShortCut = 8229
        end
        item
          Command = ecWordLeft
          ShortCut = 16421
        end
        item
          Command = ecSelWordLeft
          ShortCut = 24613
        end
        item
          Command = ecRight
          ShortCut = 39
        end
        item
          Command = ecSelRight
          ShortCut = 8231
        end
        item
          Command = ecWordRight
          ShortCut = 16423
        end
        item
          Command = ecSelWordRight
          ShortCut = 24615
        end
        item
          Command = ecPageDown
          ShortCut = 34
        end
        item
          Command = ecSelPageDown
          ShortCut = 8226
        end
        item
          Command = ecPageBottom
          ShortCut = 16418
        end
        item
          Command = ecSelPageBottom
          ShortCut = 24610
        end
        item
          Command = ecPageUp
          ShortCut = 33
        end
        item
          Command = ecSelPageUp
          ShortCut = 8225
        end
        item
          Command = ecPageTop
          ShortCut = 16417
        end
        item
          Command = ecSelPageTop
          ShortCut = 24609
        end
        item
          Command = ecLineStart
          ShortCut = 36
        end
        item
          Command = ecSelLineStart
          ShortCut = 8228
        end
        item
          Command = ecEditorTop
          ShortCut = 16420
        end
        item
          Command = ecSelEditorTop
          ShortCut = 24612
        end
        item
          Command = ecLineEnd
          ShortCut = 35
        end
        item
          Command = ecSelLineEnd
          ShortCut = 8227
        end
        item
          Command = ecEditorBottom
          ShortCut = 16419
        end
        item
          Command = ecSelEditorBottom
          ShortCut = 24611
        end
        item
          Command = ecToggleMode
          ShortCut = 45
        end
        item
          Command = ecCopy
          ShortCut = 16429
        end
        item
          Command = ecCut
          ShortCut = 8238
        end
        item
          Command = ecPaste
          ShortCut = 8237
        end
        item
          Command = ecDeleteChar
          ShortCut = 46
        end
        item
          Command = ecDeleteLastChar
          ShortCut = 8
        end
        item
          Command = ecDeleteLastChar
          ShortCut = 8200
        end
        item
          Command = ecDeleteLastWord
          ShortCut = 16392
        end
        item
          Command = ecUndo
          ShortCut = 32776
        end
        item
          Command = ecRedo
          ShortCut = 40968
        end
        item
          Command = ecLineBreak
          ShortCut = 13
        end
        item
          Command = ecLineBreak
          ShortCut = 8205
        end
        item
          Command = ecTab
          ShortCut = 9
        end
        item
          Command = ecShiftTab
          ShortCut = 8201
        end
        item
          Command = ecContextHelp
          ShortCut = 16496
        end
        item
          Command = ecSelectAll
          ShortCut = 16449
        end
        item
          Command = ecCopy
          ShortCut = 16451
        end
        item
          Command = ecPaste
          ShortCut = 16470
        end
        item
          Command = ecCut
          ShortCut = 16472
        end
        item
          Command = ecBlockIndent
          ShortCut = 24649
        end
        item
          Command = ecBlockUnindent
          ShortCut = 24661
        end
        item
          Command = ecLineBreak
          ShortCut = 16461
        end
        item
          Command = ecInsertLine
          ShortCut = 16462
        end
        item
          Command = ecDeleteWord
          ShortCut = 16468
        end
        item
          Command = ecDeleteLine
          ShortCut = 16473
        end
        item
          Command = ecDeleteEOL
          ShortCut = 24665
        end
        item
          Command = ecUndo
          ShortCut = 16474
        end
        item
          Command = ecRedo
          ShortCut = 24666
        end
        item
          Command = ecGotoMarker0
          ShortCut = 16432
        end
        item
          Command = ecGotoMarker1
          ShortCut = 16433
        end
        item
          Command = ecGotoMarker2
          ShortCut = 16434
        end
        item
          Command = ecGotoMarker3
          ShortCut = 16435
        end
        item
          Command = ecGotoMarker4
          ShortCut = 16436
        end
        item
          Command = ecGotoMarker5
          ShortCut = 16437
        end
        item
          Command = ecGotoMarker6
          ShortCut = 16438
        end
        item
          Command = ecGotoMarker7
          ShortCut = 16439
        end
        item
          Command = ecGotoMarker8
          ShortCut = 16440
        end
        item
          Command = ecGotoMarker9
          ShortCut = 16441
        end
        item
          Command = ecSetMarker0
          ShortCut = 24624
        end
        item
          Command = ecSetMarker1
          ShortCut = 24625
        end
        item
          Command = ecSetMarker2
          ShortCut = 24626
        end
        item
          Command = ecSetMarker3
          ShortCut = 24627
        end
        item
          Command = ecSetMarker4
          ShortCut = 24628
        end
        item
          Command = ecSetMarker5
          ShortCut = 24629
        end
        item
          Command = ecSetMarker6
          ShortCut = 24630
        end
        item
          Command = ecSetMarker7
          ShortCut = 24631
        end
        item
          Command = ecSetMarker8
          ShortCut = 24632
        end
        item
          Command = ecSetMarker9
          ShortCut = 24633
        end
        item
          Command = ecNormalSelect
          ShortCut = 24654
        end
        item
          Command = ecColumnSelect
          ShortCut = 24643
        end
        item
          Command = ecLineSelect
          ShortCut = 24652
        end
        item
          Command = ecMatchBracket
          ShortCut = 24642
        end>
      Options = [eoAutoIndent, eoNoCaret, eoScrollPastEol, eoShowScrollHint, eoSmartTabs, eoTabsToSpaces, eoTrimTrailingSpaces, eoSmartTabDelete]
      ReadOnly = True
    end
  end
  object gbSyntax: TGroupBox
    Left = 417
    Top = 8
    Width = 137
    Height = 57
    Anchors = [akTop, akRight]
    Caption = 'Assembler Syntax :'
    TabOrder = 1
    object rbIntel: TRadioButton
      Tag = 1
      Left = 80
      Top = 24
      Width = 49
      Height = 17
      Caption = 'Intel'
      TabOrder = 0
      OnClick = rbSyntaxClick
    end
    object rbATT: TRadioButton
      Left = 16
      Top = 24
      Width = 57
      Height = 17
      Caption = 'AT&&T'
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = rbSyntaxClick
    end
  end
  object CloseBtn: TBitBtn
    Left = 417
    Top = 416
    Width = 137
    Height = 25
    Anchors = [akRight, akBottom]
    TabOrder = 2
    Kind = bkClose
  end
  object gbRegisters: TGroupBox
    Left = 417
    Top = 72
    Width = 138
    Height = 337
    Anchors = [akTop, akRight]
    Caption = 'Registers :'
    TabOrder = 3
    object lblEIP: TLabel
      Left = 8
      Top = 212
      Width = 23
      Height = 13
      Caption = 'EIP :'
    end
    object lblEAX: TLabel
      Left = 8
      Top = 20
      Width = 27
      Height = 13
      Caption = 'EAX :'
    end
    object lblEBX: TLabel
      Left = 8
      Top = 44
      Width = 27
      Height = 13
      Caption = 'EBX :'
    end
    object lblECX: TLabel
      Left = 8
      Top = 68
      Width = 27
      Height = 13
      Caption = 'ECX :'
    end
    object lblEDX: TLabel
      Left = 8
      Top = 92
      Width = 28
      Height = 13
      Caption = 'EDX :'
    end
    object lblESI: TLabel
      Left = 8
      Top = 116
      Width = 23
      Height = 13
      Caption = 'ESI :'
    end
    object lblEDI: TLabel
      Left = 8
      Top = 140
      Width = 24
      Height = 13
      Caption = 'EDI :'
    end
    object lblEBP: TLabel
      Left = 8
      Top = 164
      Width = 27
      Height = 13
      Caption = 'EBP :'
    end
    object lblESP: TLabel
      Left = 8
      Top = 188
      Width = 27
      Height = 13
      Caption = 'ESP :'
    end
    object lblCS: TLabel
      Left = 8
      Top = 236
      Width = 20
      Height = 13
      Caption = 'CS :'
    end
    object lblDS: TLabel
      Left = 8
      Top = 260
      Width = 21
      Height = 13
      Caption = 'DS :'
    end
    object lblSS: TLabel
      Left = 8
      Top = 284
      Width = 20
      Height = 13
      Caption = 'SS :'
    end
    object lblES: TLabel
      Left = 8
      Top = 308
      Width = 20
      Height = 13
      Caption = 'ES :'
    end
    object EIPText: TEdit
      Left = 40
      Top = 208
      Width = 90
      Height = 21
      ReadOnly = True
      TabOrder = 0
    end
    object EAXText: TEdit
      Left = 40
      Top = 16
      Width = 90
      Height = 21
      ReadOnly = True
      TabOrder = 1
    end
    object EBXText: TEdit
      Left = 40
      Top = 40
      Width = 90
      Height = 21
      ReadOnly = True
      TabOrder = 2
    end
    object ECXText: TEdit
      Left = 40
      Top = 64
      Width = 90
      Height = 21
      ReadOnly = True
      TabOrder = 3
    end
    object EDXText: TEdit
      Left = 40
      Top = 88
      Width = 90
      Height = 21
      ReadOnly = True
      TabOrder = 4
    end
    object ESIText: TEdit
      Left = 40
      Top = 112
      Width = 90
      Height = 21
      ReadOnly = True
      TabOrder = 5
    end
    object EDIText: TEdit
      Left = 40
      Top = 136
      Width = 90
      Height = 21
      ReadOnly = True
      TabOrder = 6
    end
    object EBPText: TEdit
      Left = 40
      Top = 160
      Width = 90
      Height = 21
      ReadOnly = True
      TabOrder = 7
    end
    object ESPText: TEdit
      Left = 40
      Top = 184
      Width = 90
      Height = 21
      ReadOnly = True
      TabOrder = 8
    end
    object CSText: TEdit
      Left = 40
      Top = 232
      Width = 90
      Height = 21
      ReadOnly = True
      TabOrder = 9
    end
    object DSText: TEdit
      Left = 40
      Top = 256
      Width = 90
      Height = 21
      ReadOnly = True
      TabOrder = 10
    end
    object SSText: TEdit
      Left = 40
      Top = 280
      Width = 90
      Height = 21
      ReadOnly = True
      TabOrder = 11
    end
    object ESText: TEdit
      Left = 40
      Top = 304
      Width = 90
      Height = 21
      ReadOnly = True
      TabOrder = 12
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
    Left = 144
    Top = 120
  end
end
