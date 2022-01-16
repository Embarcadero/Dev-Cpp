object FormFoldingDemo: TFormFoldingDemo
  Left = 0
  Top = 0
  Caption = 'Folding Demo'
  ClientHeight = 402
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ActionMainMenuBar: TActionMainMenuBar
    Left = 0
    Top = 0
    Width = 635
    Height = 25
    UseSystemFont = False
    ActionManager = ActionManager
    Caption = 'ActionMainMenuBar'
    Color = clMenuBar
    ColorMap.DisabledFontColor = 7171437
    ColorMap.HighlightColor = clWhite
    ColorMap.BtnSelectedFont = clBlack
    ColorMap.UnusedColor = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    Spacing = 0
  end
  object SynEdit: TSynEdit
    Left = 0
    Top = 25
    Width = 635
    Height = 377
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Pitch = fpFixed
    Font.Style = []
    PopupMenu = PopupActionBar
    TabOrder = 1
    CodeFolding.GutterShapeSize = 11
    CodeFolding.CollapsedLineColor = clGrayText
    CodeFolding.FolderBarLinesColor = clGrayText
    CodeFolding.IndentGuidesColor = clGray
    CodeFolding.IndentGuides = True
    CodeFolding.ShowCollapsedLine = False
    CodeFolding.ShowHintMark = True
    UseCodeFolding = False
    Gutter.AutoSize = True
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clGrayText
    Gutter.Font.Height = -12
    Gutter.Font.Name = 'Consolas'
    Gutter.Font.Style = []
    Gutter.GradientStartColor = clWindowText
    Gutter.GradientEndColor = clWindow
    Lines.Strings = (
      
        'This project demonstrates the code folding capabilities of Syned' +
        'it.'
      
        'Use the menu to open one of the demo files in the project direct' +
        'ory.'
      '- demo.cpp'
      '- demo.js'
      '- demo.py'
      
        'Then select "View, Code Folding" to activate Code Foldind and tr' +
        'y the '
      'folding commands under the View menu.'
      ''
      'SynEdit folding commands and their default shorcuts:'
      '  AddKey(ecFoldAll, VK_OEM_MINUS, [ssCtrl, ssShift]);'
      '  AddKey(ecUnfoldAll,  VK_OEM_PLUS, [ssCtrl, ssShift]);'
      '  AddKey(ecFoldNearest, VK_OEM_2, [ssCtrl]);  // Divide '#39'/'#39
      '  AddKey(ecUnfoldNearest, VK_OEM_2, [ssCtrl, ssShift]);'
      '  AddKey(ecFoldLevel1, ord('#39'K'#39'), [ssCtrl], Ord('#39'1'#39'), [ssCtrl]);'
      '  AddKey(ecFoldLevel2, ord('#39'K'#39'), [ssCtrl], Ord('#39'2'#39'), [ssCtrl]);'
      '  AddKey(ecFoldLevel3, ord('#39'K'#39'), [ssCtrl], Ord('#39'3'#39'), [ssCtrl]);'
      
        '  AddKey(ecUnfoldLevel1, ord('#39'K'#39'), [ssCtrl, ssShift], Ord('#39'1'#39'), ' +
        '[ssCtrl, ssShift]);'
      
        '  AddKey(ecUnfoldLevel2, ord('#39'K'#39'), [ssCtrl, ssShift], Ord('#39'2'#39'), ' +
        '[ssCtrl, ssShift]);'
      
        '  AddKey(ecUnfoldLevel3, ord('#39'K'#39'), [ssCtrl, ssShift], Ord('#39'3'#39'), ' +
        '[ssCtrl, ssShift]);'
      ''
      
        'Note: The JavaScript, DWS and Python highlighters are Code Foldi' +
        'ng enabled, but'
      
        'the C++ highlighter is not.  Code folding for C++ is provided by' +
        ' a Synedit '
      'event handler (ScanForFoldRanges).'
      ''
      
        'You can find technical information about the implementation of c' +
        'ode folding'
      'in the unit SynEditCodeFolding.pas.')
    Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabIndent, eoTabsToSpaces, eoTrimTrailingSpaces]
    TabWidth = 4
    OnGutterGetText = SynEditGutterGetText
    OnStatusChange = SynEditStatusChange
    FontSmoothing = fsmNone
  end
  object ActionManager: TActionManager
    ActionBars = <
      item
        Items = <
          item
            Items = <
              item
                Action = ActionFileOpen
                ImageIndex = 7
                ShortCut = 16463
              end
              item
                Action = ActionSave
                Caption = '&Save'
              end
              item
                Action = ActionFileSaveAs
                ImageIndex = 30
              end
              item
                Caption = '-'
              end
              item
                Action = ActionFilePrintSetup
              end
              item
                Action = ActionFilePageSetup
                Caption = 'Pa&ge Setup...'
              end
              item
                Action = ActionDialogPrintDlg
                Caption = 'P&rint...'
                ImageIndex = 14
                ShortCut = 16464
              end
              item
                Caption = '-'
              end
              item
                Action = ActionFileExit
                ImageIndex = 43
              end>
            Caption = '&File'
          end
          item
            Items = <
              item
                Action = ActionEditCut
                ImageIndex = 0
                ShortCut = 16472
              end
              item
                Action = ActionEditCopy
                ImageIndex = 1
                ShortCut = 16451
              end
              item
                Action = ActionEditPaste
                ImageIndex = 2
                ShortCut = 16470
              end
              item
                Action = ActionEditSelectAll
                ShortCut = 16449
              end
              item
                Action = ActionEditUndo
                ImageIndex = 3
                ShortCut = 16474
              end
              item
                Action = ActionEditDelete
                ImageIndex = 5
                ShortCut = 46
              end>
            Caption = '&Edit'
          end
          item
            Items = <
              item
                Action = ActionDialogFontEdit
              end
              item
                Action = ActionGutterLines
                Caption = '&Gutter Lines'
              end
              item
                Caption = '-'
              end
              item
                Items = <
                  item
                    Action = ActionCPP
                  end
                  item
                    Action = ActionDWS
                  end
                  item
                    Action = ActionJavaScript
                  end
                  item
                    Action = ActionPython
                  end
                  item
                    Action = ActionPascal
                  end
                  item
                    Action = ActionXML
                  end>
                Caption = '&Highlighter'
                UsageCount = 1
              end
              item
                Caption = '-'
              end
              item
                Action = ActionCodeFolding
                Caption = '&Code Folding'
              end
              item
                Items = <
                  item
                    Action = ActionShowCollapsedLines
                    Caption = '&Collapsed Lines'
                  end
                  item
                    Action = ActionShowCollapsedMarks
                    Caption = 'C&ollapsed Marks'
                  end
                  item
                    Caption = '-'
                  end
                  item
                    Action = ActionFoldShapeSize
                    Caption = '&Gutter Square Size..'
                  end>
                Caption = 'Fo&lding Options'
                UsageCount = 1
              end
              item
                Caption = '-'
              end
              item
                Items = <
                  item
                    Action = ActionFoldAll
                    Caption = '&All'
                  end
                  item
                    Action = ActionFoldNearest
                    Caption = '&Nearest'
                  end
                  item
                    Action = ActionFoldRegions
                    Caption = '&Regions'
                  end
                  item
                    Action = ActionFoldLevel1
                    Caption = '&Level 1'
                  end
                  item
                    Action = ActionFoldLevel2
                    Caption = 'L&evel 2'
                  end
                  item
                    Action = ActionFoldLevel3
                    Caption = 'Le&vel 3'
                  end>
                Caption = 'F&old'
                UsageCount = 1
              end
              item
                Items = <
                  item
                    Action = ActionUnFoldAll
                    Caption = '&All'
                  end
                  item
                    Action = ActionUnfoldNearest
                    Caption = '&Nearest'
                  end
                  item
                    Action = ActionUnfoldRegions
                    Caption = '&Regions'
                  end
                  item
                    Action = ActionUnfoldLevel1
                    Caption = '&Level 1'
                  end
                  item
                    Action = ActionUnfoldLevel2
                    Caption = 'L&evel 2'
                  end
                  item
                    Action = ActionUnfoldLevel3
                    Caption = 'Le&vel 3'
                  end>
                Caption = '&Unfold'
                UsageCount = 1
              end>
            Caption = '&View'
          end>
        ActionBar = ActionMainMenuBar
      end>
    OnUpdate = ActionManagerUpdate
    Left = 440
    Top = 32
    StyleName = 'Platform Default'
    object ActionShowCollapsedLines: TAction
      Category = 'Folding Options'
      AutoCheck = True
      Caption = 'Collapsed Lines'
      OnExecute = ActionShowCollapsedLinesExecute
    end
    object ActionFileOpen: TFileOpen
      Category = 'File'
      Caption = '&Open...'
      Hint = 'Open|Opens an existing file'
      ImageIndex = 7
      ShortCut = 16463
      OnAccept = ActionFileOpenAccept
    end
    object ActionSave: TAction
      Category = 'File'
      Caption = 'Save'
      OnExecute = ActionSaveExecute
    end
    object ActionFileSaveAs: TFileSaveAs
      Category = 'File'
      Caption = 'Save &As...'
      Dialog.Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
      Hint = 'Save As|Saves the active file with a new name'
      ImageIndex = 30
      OnAccept = ActionFileSaveAsAccept
    end
    object ActionFilePrintSetup: TFilePrintSetup
      Category = 'File'
      Caption = 'Print Set&up...'
      Hint = 'Print Setup'
    end
    object ActionFilePageSetup: TFilePageSetup
      Category = 'File'
      Caption = 'Page Set&up...'
      Dialog.MinMarginLeft = 0
      Dialog.MinMarginTop = 0
      Dialog.MinMarginRight = 0
      Dialog.MinMarginBottom = 0
      Dialog.MarginLeft = 2500
      Dialog.MarginTop = 2500
      Dialog.MarginRight = 2500
      Dialog.MarginBottom = 2500
      Dialog.PageWidth = 21590
      Dialog.PageHeight = 27940
    end
    object ActionDialogPrintDlg: TPrintDlg
      Category = 'File'
      Caption = '&Print...'
      ImageIndex = 14
      ShortCut = 16464
      OnAccept = ActionDialogPrintDlgAccept
    end
    object ActionFileExit: TFileExit
      Category = 'File'
      Caption = 'E&xit'
      Hint = 'Exit|Quits the application'
      ImageIndex = 43
    end
    object ActionEditCut: TEditCut
      Category = 'Edit'
      Caption = 'Cu&t'
      Hint = 'Cut|Cuts the selection and puts it on the Clipboard'
      ImageIndex = 0
      ShortCut = 16472
    end
    object ActionEditCopy: TEditCopy
      Category = 'Edit'
      Caption = '&Copy'
      Hint = 'Copy|Copies the selection and puts it on the Clipboard'
      ImageIndex = 1
      ShortCut = 16451
    end
    object ActionEditPaste: TEditPaste
      Category = 'Edit'
      Caption = '&Paste'
      Hint = 'Paste|Inserts Clipboard contents'
      ImageIndex = 2
      ShortCut = 16470
    end
    object ActionEditSelectAll: TEditSelectAll
      Category = 'Edit'
      Caption = 'Select &All'
      Hint = 'Select All|Selects the entire document'
      ShortCut = 16449
    end
    object ActionEditUndo: TEditUndo
      Category = 'Edit'
      Caption = '&Undo'
      Hint = 'Undo|Reverts the last action'
      ImageIndex = 3
      ShortCut = 16474
    end
    object ActionEditDelete: TEditDelete
      Category = 'Edit'
      Caption = '&Delete'
      Hint = 'Delete|Erases the selection'
      ImageIndex = 5
      ShortCut = 46
    end
    object ActionDialogFontEdit: TFontEdit
      Category = 'View'
      Caption = 'Select &Font...'
      Dialog.Font.Charset = DEFAULT_CHARSET
      Dialog.Font.Color = clWindowText
      Dialog.Font.Height = -11
      Dialog.Font.Name = 'Tahoma'
      Dialog.Font.Style = []
      Dialog.Options = [fdEffects, fdFixedPitchOnly]
      Hint = 'Font Select'
      BeforeExecute = ActionDialogFontEditBeforeExecute
    end
    object ActionGutterLines: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'Gutter Lines'
      OnExecute = ActionGutterLinesExecute
    end
    object ActionCPP: TAction
      Category = 'Highlighter'
      Caption = '&C++'
      OnExecute = ActionCPPExecute
    end
    object ActionJavaScript: TAction
      Category = 'Highlighter'
      Caption = '&JavaScript'
      OnExecute = ActionJavaScriptExecute
    end
    object ActionPython: TAction
      Category = 'Highlighter'
      Caption = 'P&ython'
      OnExecute = ActionPythonExecute
    end
    object ActionCodeFolding: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'Code Folding'
      OnExecute = ActionCodeFoldingExecute
    end
    object ActionFoldAll: TAction
      Category = 'Fold'
      Caption = 'All'
      OnExecute = ActionFoldExecute
      OnUpdate = ActionFoldUpdate
    end
    object ActionUnFoldAll: TAction
      Category = 'Unfold'
      Caption = 'All'
      OnExecute = ActionFoldExecute
      OnUpdate = ActionFoldUpdate
    end
    object ActionFoldNearest: TAction
      Category = 'Fold'
      Caption = 'Nearest'
      OnExecute = ActionFoldExecute
      OnUpdate = ActionFoldUpdate
    end
    object ActionFoldRegions: TAction
      Category = 'Fold'
      Caption = 'Regions'
      OnExecute = ActionFoldExecute
      OnUpdate = ActionFoldUpdate
    end
    object ActionFoldLevel1: TAction
      Category = 'Fold'
      Caption = 'Level 1'
      OnExecute = ActionFoldExecute
      OnUpdate = ActionFoldUpdate
    end
    object ActionFoldLevel2: TAction
      Category = 'Fold'
      Caption = 'Level 2'
      OnExecute = ActionFoldExecute
      OnUpdate = ActionFoldUpdate
    end
    object ActionFoldLevel3: TAction
      Category = 'Fold'
      Caption = 'Level 3'
      OnExecute = ActionFoldExecute
      OnUpdate = ActionFoldUpdate
    end
    object ActionUnfoldNearest: TAction
      Category = 'Unfold'
      Caption = 'Nearest'
      OnExecute = ActionFoldExecute
      OnUpdate = ActionFoldUpdate
    end
    object ActionUnfoldRegions: TAction
      Category = 'Unfold'
      Caption = 'Regions'
      OnExecute = ActionFoldExecute
      OnUpdate = ActionFoldUpdate
    end
    object ActionUnfoldLevel1: TAction
      Category = 'Unfold'
      Caption = 'Level 1'
      OnExecute = ActionFoldExecute
      OnUpdate = ActionFoldUpdate
    end
    object ActionUnfoldLevel2: TAction
      Category = 'Unfold'
      Caption = 'Level 2'
      OnExecute = ActionFoldExecute
      OnUpdate = ActionFoldUpdate
    end
    object ActionUnfoldLevel3: TAction
      Category = 'Unfold'
      Caption = 'Level 3'
      OnExecute = ActionFoldExecute
      OnUpdate = ActionFoldUpdate
    end
    object ActionDWS: TAction
      Category = 'Highlighter'
      Caption = '&DelphiWebScript'
      OnExecute = ActionDWSExecute
    end
    object ActionShowCollapsedMarks: TAction
      Category = 'Folding Options'
      AutoCheck = True
      Caption = 'Collapsed Marks'
      Checked = True
      OnExecute = ActionShowCollapsedMarksExecute
    end
    object ActionFoldShapeSize: TAction
      Category = 'Folding Options'
      Caption = 'Gutter Square Size..'
      OnExecute = ActionFoldShapeSizeExecute
    end
    object ActionPascal: TAction
      Category = 'Highlighter'
      Caption = '&Pascal'
      OnExecute = ActionPascalExecute
    end
    object ActionXML: TAction
      Category = 'Highlighter'
      Caption = '&XML'
      OnExecute = ActionXMLExecute
    end
  end
  object PopupActionBar: TPopupActionBar
    Left = 532
    Top = 32
    object N1: TMenuItem
      Caption = '-'
    end
    object MenuItemCut: TMenuItem
      Action = ActionEditCut
    end
    object MenuItemCopy: TMenuItem
      Action = ActionEditCopy
    end
    object MenuItemPaste: TMenuItem
      Action = ActionEditPaste
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object MenuItemFold: TMenuItem
      Caption = 'Fold'
      object MenuItemFoldAll: TMenuItem
        Action = ActionFoldAll
      end
      object MenuItemFoldNearest: TMenuItem
        Action = ActionFoldNearest
      end
      object MenuItemFoldRanges: TMenuItem
        Action = ActionFoldRegions
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object MenuItemFoldLevel1: TMenuItem
        Action = ActionFoldLevel1
      end
      object MenuItemFoldLevel2: TMenuItem
        Action = ActionFoldLevel2
      end
      object MenuItemFoldLevel3: TMenuItem
        Action = ActionFoldLevel3
      end
    end
    object MenuItemUnfold: TMenuItem
      Caption = 'Unfold'
      object MenuItemUnfoldAll: TMenuItem
        Action = ActionUnFoldAll
      end
      object MenuItemUnfoldNearest: TMenuItem
        Action = ActionUnfoldNearest
      end
      object MenuItemUnfoldRanges: TMenuItem
        Action = ActionUnfoldRegions
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object MenuItemUnfoldLevel1: TMenuItem
        Action = ActionUnfoldLevel1
      end
      object MenuItemUnfoldLevel2: TMenuItem
        Action = ActionUnfoldLevel2
      end
      object MenuItemUnfoldLevel3: TMenuItem
        Action = ActionUnfoldLevel3
      end
    end
  end
  object SynEditPrint: TSynEditPrint
    Copies = 1
    Header.DefaultFont.Charset = DEFAULT_CHARSET
    Header.DefaultFont.Color = clBlack
    Header.DefaultFont.Height = -13
    Header.DefaultFont.Name = 'Arial'
    Header.DefaultFont.Style = []
    Footer.DefaultFont.Charset = DEFAULT_CHARSET
    Footer.DefaultFont.Color = clBlack
    Footer.DefaultFont.Height = -13
    Footer.DefaultFont.Name = 'Arial'
    Footer.DefaultFont.Style = []
    Margins.Left = 25.000000000000000000
    Margins.Right = 15.000000000000000000
    Margins.Top = 25.000000000000000000
    Margins.Bottom = 25.000000000000000000
    Margins.Header = 15.000000000000000000
    Margins.Footer = 15.000000000000000000
    Margins.LeftHFTextIndent = 2.000000000000000000
    Margins.RightHFTextIndent = 2.000000000000000000
    Margins.HFInternalMargin = 0.500000000000000000
    Margins.MirrorMargins = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    TabWidth = 8
    Color = clWhite
    Left = 440
    Top = 176
  end
  object SynCppSyn: TSynCppSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 532
    Top = 80
  end
  object SynJScriptSyn: TSynJScriptSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 440
    Top = 80
  end
  object SynPythonSyn: TSynPythonSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 440
    Top = 128
  end
  object PythonBehaviour: TSynEditPythonBehaviour
    Editor = SynEdit
    Left = 532
    Top = 128
  end
  object SynDWSSyn: TSynDWSSyn
    DefaultFilter = 'DWScript Files (*.dws;*.pas;*.inc)|*.dws;*.pas;*.inc'
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 532
    Top = 176
  end
  object SynPasSyn: TSynPasSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 440
    Top = 232
  end
  object SynXMLSyn: TSynXMLSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    WantBracesParsed = False
    Left = 532
    Top = 232
  end
end
