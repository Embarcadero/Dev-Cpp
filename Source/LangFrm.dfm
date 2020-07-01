object LangForm: TLangForm
  Left = 563
  Top = 516
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Embarcadero Dev-C++ first time configuration'
  ClientHeight = 320
  ClientWidth = 560
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object OkBtn: TBitBtn
    Left = 270
    Top = 280
    Width = 280
    Height = 30
    Caption = '&Next'
    Default = True
    Glyph.Data = {
      36030000424D3603000000000000360000002800000010000000100000000100
      18000000000000030000120B0000120B00000000000000000000BFBFBFBFBFBF
      BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
      BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF000000BFBFBFBF
      BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
      BFBFBFBFBFBF000000009836000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
      BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF00000000A13900A13900983600
      0000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
      0000008FFF8F00C54600B03F00B03F009836000000BFBFBFBFBFBFBFBFBFBFBF
      BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF0000008FFF8F00C54600B03F00
      B03F009836000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
      BFBFBFBFBFBF0000008FFF8F00C54600B03F00B03F009836000000BFBFBFBFBF
      BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF0000008FFF8F00
      B03F00B03F00A139009836000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
      BFBFBFBFBFBFBFBFBFBFBFBF00000000B03F00B03F00B03F00A1390098360000
      00BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF00000000B03F00
      B03F00B03F00B03F00A139000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
      BFBFBFBFBFBF00000000C54600B03F00B03F00B03F00A139000000BFBFBFBFBF
      BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF00000000C54600C54600B03F00
      B03F00B03F000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
      0000008FFF8F00DD0000C54600C54600C546000000BFBFBFBFBFBFBFBFBFBFBF
      BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF0000008FFF8F00DD0000C54600
      0000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
      BFBFBFBFBFBF0000008FFF8F000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
      BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF000000BFBFBFBF
      BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF}
    TabOrder = 0
    OnClick = OkBtnClick
  end
  object LangPanel: TPanel
    Left = 260
    Top = 0
    Width = 300
    Height = 275
    BevelOuter = bvNone
    TabOrder = 1
    object lblLangInfo: TLabel
      Left = 8
      Top = 230
      Width = 284
      Height = 48
      Alignment = taCenter
      AutoSize = False
      Caption = 
        'You can later change the language at Tools >> Environment Option' +
        's >> General.'
      WordWrap = True
    end
    object grpLanguages: TGroupBox
      Left = 16
      Top = 12
      Width = 268
      Height = 213
      Caption = 'Select your language:'
      TabOrder = 0
      object lbLanguages: TListBox
        Left = 8
        Top = 20
        Width = 250
        Height = 181
        ItemHeight = 15
        TabOrder = 0
      end
    end
  end
  object FinishPanel: TPanel
    Left = 260
    Top = 0
    Width = 300
    Height = 275
    BevelOuter = bvNone
    TabOrder = 2
    Visible = False
    object Finish2: TLabel
      Left = 8
      Top = 64
      Width = 273
      Height = 73
      AutoSize = False
      Caption = 
        'If you need help using Embarcadero Dev-C++, please refer to the ' +
        'Embarcadero Dev-C++ help file in the Help menu or send the devel' +
        'oper a message (he doesn'#39't mind!).'
      WordWrap = True
    end
    object Finish3: TLabel
      Left = 8
      Top = 146
      Width = 284
      Height = 75
      AutoSize = False
      Caption = 
        'You can also download packages (like libraries or tools) to use ' +
        'with Embarcadero Dev-C++ using WebUpdate, which you will find in' +
        ' Tools menu >> Check for Packages.'
      WordWrap = True
    end
    object Finish1: TLabel
      Left = 8
      Top = 8
      Width = 284
      Height = 45
      AutoSize = False
      Caption = 
        'Embarcadero Dev-C++ has been configured successfully, you may no' +
        'w click OK to proceed to its loading.'
      WordWrap = True
    end
  end
  object synExample: TSynEdit
    Left = 0
    Top = 0
    Width = 260
    Height = 320
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 3
    CodeFolding.CollapsedLineColor = clGrayText
    CodeFolding.FolderBarLinesColor = clGrayText
    CodeFolding.ShowCollapsedLine = False
    CodeFolding.IndentGuidesColor = clGray
    CodeFolding.IndentGuides = True
    UseCodeFolding = False
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.RightOffset = 21
    Lines.Strings = (
      '#include <iostream>'
      ''
      'int main(int argc, char** argv) {'
      #9'std::cout << "Hello world!\n";'
      #9'return 0;'
      '}')
    FontSmoothing = fsmNone
  end
  object EditPanel: TPanel
    Left = 260
    Top = 0
    Width = 300
    Height = 275
    BevelOuter = bvNone
    TabOrder = 4
    Visible = False
    object lblEditInfo: TLabel
      Left = 8
      Top = 230
      Width = 284
      Height = 48
      Alignment = taCenter
      AutoSize = False
      Caption = 
        'You can later change themes at Tools >> Editor Options >> Fonts/' +
        'Colors.'
      WordWrap = True
    end
    object grpThemes: TGroupBox
      Left = 16
      Top = 12
      Width = 268
      Height = 205
      Caption = 'Select your theme:'
      TabOrder = 0
      object lblIcons: TLabel
        Left = 8
        Top = 104
        Width = 31
        Height = 15
        Caption = 'Icons:'
      end
      object lblColor: TLabel
        Left = 8
        Top = 72
        Width = 32
        Height = 15
        Caption = 'Color:'
      end
      object lblFont: TLabel
        Left = 8
        Top = 32
        Width = 27
        Height = 15
        Caption = 'Font:'
      end
      object cmbIcons: TComboBox
        Left = 56
        Top = 102
        Width = 201
        Height = 23
        Style = csDropDownList
        TabOrder = 0
        OnChange = cmbIconsChange
      end
      object cmbColors: TComboBox
        Left = 56
        Top = 70
        Width = 201
        Height = 23
        Style = csDropDownList
        TabOrder = 1
        OnChange = ColorChange
        Items.Strings = (
          'Classic'
          'Classic Plus'
          'Twilight'
          'Ocean'
          'Visual Studio'
          'Borland'
          'Matrix'
          'Obsidian'
          'GSS Hacker'
          'Obvilion')
      end
      object cmbFont: TComboBox
        Left = 56
        Top = 22
        Width = 201
        Height = 36
        AutoComplete = False
        Style = csOwnerDrawVariable
        ItemHeight = 30
        Sorted = True
        TabOrder = 2
        OnChange = FontChange
        OnDrawItem = cmbFontDrawItem
      end
      object tbExample: TToolBar
        Left = 8
        Top = 130
        Width = 250
        Height = 64
        Align = alNone
        AutoSize = True
        BorderWidth = 5
        Caption = 'Main'
        DragKind = dkDock
        Images = dmMain.MenuImages_NewLook
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
        object NewFileBtn: TToolButton
          Left = 0
          Top = 0
          Caption = '&Source File'
          ImageIndex = 1
        end
        object OpenBtn: TToolButton
          Left = 23
          Top = 0
          ImageIndex = 4
        end
        object SaveUnitBtn: TToolButton
          Left = 46
          Top = 0
          ImageIndex = 6
        end
        object SaveAllBtn: TToolButton
          Left = 69
          Top = 0
          ImageIndex = 8
        end
        object CloseBtn: TToolButton
          Left = 92
          Top = 0
          ImageIndex = 9
        end
        object PrintBtn: TToolButton
          Left = 115
          Top = 0
          ImageIndex = 10
        end
        object UndoBtn: TToolButton
          Left = 138
          Top = 0
          ImageIndex = 13
        end
        object RedoBtn: TToolButton
          Left = 161
          Top = 0
          ImageIndex = 14
        end
        object FindBtn: TToolButton
          Left = 184
          Top = 0
          ImageIndex = 21
        end
        object ReplaceBtn: TToolButton
          Left = 207
          Top = 0
          ImageIndex = 22
          Wrap = True
        end
        object FindNextBtn: TToolButton
          Left = 0
          Top = 22
          ImageIndex = 44
        end
        object GotoLineBtn: TToolButton
          Left = 23
          Top = 22
          ImageIndex = 24
        end
        object CompileBtn: TToolButton
          Left = 46
          Top = 22
          ImageIndex = 28
        end
        object RunBtn: TToolButton
          Left = 69
          Top = 22
          ImageIndex = 31
        end
        object CompileAndRunBtn: TToolButton
          Left = 92
          Top = 22
          ImageIndex = 33
        end
        object RebuildAllBtn: TToolButton
          Left = 115
          Top = 22
          ImageIndex = 30
        end
        object DebugBtn: TToolButton
          Left = 138
          Top = 22
          ImageIndex = 32
        end
        object ProfileBtn: TToolButton
          Left = 161
          Top = 22
          ImageIndex = 43
        end
        object ProfilingInforBtn: TToolButton
          Left = 184
          Top = 22
          ImageIndex = 47
        end
        object InsertBtn: TToolButton
          Left = 207
          Top = 22
          ImageIndex = 18
        end
      end
    end
  end
end
