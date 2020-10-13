object LangForm: TLangForm
  Left = 563
  Top = 516
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Embarcadero Dev-C++ first time configuration'
  ClientHeight = 320
  ClientWidth = 560
  Color = clWindow
  Ctl3D = False
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
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
    ImageIndex = 39
    ImageName = 'iconsnew-34'
    Images = dmMain.SVGImageListMenuStyle
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
        Ctl3D = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ItemHeight = 15
        ParentCtl3D = False
        ParentFont = False
        TabOrder = 0
        StyleElements = [seClient, seBorder]
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
    CodeFolding.ShowCollapsedLine = True
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
      Height = 212
      Caption = 'Select your theme:'
      TabOrder = 0
      object lblTheme: TLabel
        Left = 8
        Top = 104
        Width = 39
        Height = 15
        Caption = 'Theme:'
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
      object VirtualImageTheme: TVirtualImage
        Left = 3
        Top = 135
        Width = 383
        Height = 103
        ImageCollection = dmMain.ImageThemeColection
        ImageWidth = 0
        ImageHeight = 0
        ImageIndex = 0
        ImageName = 'Windows Classic'
      end
      object cmbTheme: TComboBox
        Left = 56
        Top = 102
        Width = 201
        Height = 23
        Style = csDropDownList
        TabOrder = 0
        OnChange = cmbThemeChange
        Items.Strings = (
          'Windows Classic'
          'Windows 10'
          'Slate Gray'
          'Blue Whale')
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
          'Obvilion'
          'PlasticCodeWrap'
          'Monokai'
          'Monokai Fresh')
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
    end
  end
end
