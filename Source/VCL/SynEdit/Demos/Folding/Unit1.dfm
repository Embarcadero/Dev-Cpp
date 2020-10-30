object Form1: TForm1
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
  object ActionMainMenuBar1: TActionMainMenuBar
    Left = 0
    Top = 0
    Width = 635
    Height = 25
    UseSystemFont = False
    ActionManager = ActionManager1
    Caption = 'ActionMainMenuBar1'
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
  object SynEdit1: TSynEdit
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
    Font.Quality = fqClearTypeNatural
    PopupMenu = PopupActionBar1
    TabOrder = 1
    UseCodeFolding = False
    Gutter.AutoSize = True
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -13
    Gutter.Font.Name = 'Consolas'
    Gutter.Font.Style = []
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
      
        'Note: The JavaScript, and Python highlighters are Code Folding e' +
        'nabled, but'
      
        'the C++ highlighter is not.  Code folding for C++ is provided by' +
        ' a Synedit '
      'event handler (ScanForFoldRanges).'
      ''
      
        'You can find technical information about the implementation of c' +
        'ode folding'
      'in the unit SynEditCodeFolding.pas.')
    Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabIndent, eoTabsToSpaces, eoTrimTrailingSpaces]
    TabWidth = 4
  end
  object ActionManager1: TActionManager
    ActionBars = <
      item
        Items = <
          item
            Items = <
              item
                Action = FileOpen1
                ImageIndex = 7
                ShortCut = 16463
              end
              item
                Action = ActSave
                Caption = '&Save'
              end
              item
                Action = FileSaveAs1
                ImageIndex = 30
              end
              item
                Caption = '-'
              end
              item
                Action = FilePrintSetup1
              end
              item
                Action = FilePageSetup1
                Caption = 'Pa&ge Setup...'
              end
              item
                Action = DialogPrintDlg1
                Caption = 'P&rint...'
                ImageIndex = 14
                ShortCut = 16464
              end
              item
                Caption = '-'
              end
              item
                Action = FileExit1
                ImageIndex = 43
              end>
            Caption = '&File'
          end
          item
            Items = <
              item
                Action = EditCut1
                ImageIndex = 0
                ShortCut = 16472
              end
              item
                Action = EditCopy1
                ImageIndex = 1
                ShortCut = 16451
              end
              item
                Action = EditPaste1
                ImageIndex = 2
                ShortCut = 16470
              end
              item
                Action = EditSelectAll1
                ShortCut = 16449
              end
              item
                Action = EditUndo1
                ImageIndex = 3
                ShortCut = 16474
              end
              item
                Action = EditDelete1
                ImageIndex = 5
                ShortCut = 46
              end>
            Caption = '&Edit'
          end
          item
            Items = <
              item
                Action = DialogFontEdit1
              end
              item
                Action = actGutterLines
                Caption = '&Gutter Lines'
              end
              item
                Caption = '-'
              end
              item
                Items = <
                  item
                    Action = actCPP
                    Caption = '&C++'
                  end
                  item
                    Action = actJavaScript
                    Caption = '&JavaScript'
                  end
                  item
                    Action = actPython
                    Caption = '&Python'
                  end>
                Caption = '&Highlighter'
                UsageCount = 1
              end
              item
                Caption = '-'
              end
              item
                Action = actCodeFolding
                Caption = '&Code Folding'
              end
              item
                Items = <
                  item
                    Action = actShowCollapsedLines
                    Caption = '&Collapsed Lines'
                  end
                  item
                    Action = actShowCollapsedMarks
                    Caption = 'C&ollapsed Marks'
                  end
                  item
                    Caption = '-'
                  end
                  item
                    Action = actFoldShapeSize
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
                    Action = actFoldAll
                    Caption = '&All'
                  end
                  item
                    Action = actFoldNearest
                    Caption = '&Nearest'
                  end
                  item
                    Action = actFoldRegions
                    Caption = '&Regions'
                  end
                  item
                    Action = actFoldLevel1
                    Caption = '&Level 1'
                  end
                  item
                    Action = actFoldLevel2
                    Caption = 'L&evel 2'
                  end
                  item
                    Action = actFoldLevel3
                    Caption = 'Le&vel 3'
                  end>
                Caption = 'F&old'
                UsageCount = 1
              end
              item
                Items = <
                  item
                    Action = actUnFoldAll
                    Caption = '&All'
                  end
                  item
                    Action = actUnfoldNearest
                    Caption = '&Nearest'
                  end
                  item
                    Action = actUnfoldRegions
                    Caption = '&Regions'
                  end
                  item
                    Action = actUnfoldLevel1
                    Caption = '&Level 1'
                  end
                  item
                    Action = actUnfoldLevel2
                    Caption = 'L&evel 2'
                  end
                  item
                    Action = actUnfoldLevel3
                    Caption = 'Le&vel 3'
                  end>
                Caption = '&Unfold'
                UsageCount = 1
              end>
            Caption = '&View'
          end>
        ActionBar = ActionMainMenuBar1
      end>
    OnUpdate = ActionManager1Update
    Left = 464
    Top = 32
    StyleName = 'Platform Default'
    object actShowCollapsedLines: TAction
      Category = 'Folding Options'
      AutoCheck = True
      Caption = 'Collapsed Lines'
      OnExecute = actShowCollapsedLinesExecute
    end
    object FileOpen1: TFileOpen
      Category = 'File'
      Caption = '&Open...'
      Hint = 'Open|Opens an existing file'
      ImageIndex = 7
      ShortCut = 16463
      OnAccept = FileOpen1Accept
    end
    object ActSave: TAction
      Category = 'File'
      Caption = 'Save'
      OnExecute = ActSaveExecute
    end
    object FileSaveAs1: TFileSaveAs
      Category = 'File'
      Caption = 'Save &As...'
      Dialog.Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
      Hint = 'Save As|Saves the active file with a new name'
      ImageIndex = 30
      OnAccept = FileSaveAs1Accept
    end
    object FilePrintSetup1: TFilePrintSetup
      Category = 'File'
      Caption = 'Print Set&up...'
      Hint = 'Print Setup'
    end
    object FilePageSetup1: TFilePageSetup
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
    object DialogPrintDlg1: TPrintDlg
      Category = 'File'
      Caption = '&Print...'
      ImageIndex = 14
      ShortCut = 16464
      OnAccept = DialogPrintDlg1Accept
    end
    object FileExit1: TFileExit
      Category = 'File'
      Caption = 'E&xit'
      Hint = 'Exit|Quits the application'
      ImageIndex = 43
    end
    object EditCut1: TEditCut
      Category = 'Edit'
      Caption = 'Cu&t'
      Hint = 'Cut|Cuts the selection and puts it on the Clipboard'
      ImageIndex = 0
      ShortCut = 16472
    end
    object EditCopy1: TEditCopy
      Category = 'Edit'
      Caption = '&Copy'
      Hint = 'Copy|Copies the selection and puts it on the Clipboard'
      ImageIndex = 1
      ShortCut = 16451
    end
    object EditPaste1: TEditPaste
      Category = 'Edit'
      Caption = '&Paste'
      Hint = 'Paste|Inserts Clipboard contents'
      ImageIndex = 2
      ShortCut = 16470
    end
    object EditSelectAll1: TEditSelectAll
      Category = 'Edit'
      Caption = 'Select &All'
      Hint = 'Select All|Selects the entire document'
      ShortCut = 16449
    end
    object EditUndo1: TEditUndo
      Category = 'Edit'
      Caption = '&Undo'
      Hint = 'Undo|Reverts the last action'
      ImageIndex = 3
      ShortCut = 16474
    end
    object EditDelete1: TEditDelete
      Category = 'Edit'
      Caption = '&Delete'
      Hint = 'Delete|Erases the selection'
      ImageIndex = 5
      ShortCut = 46
    end
    object DialogFontEdit1: TFontEdit
      Category = 'View'
      Caption = 'Select &Font...'
      Dialog.Font.Charset = DEFAULT_CHARSET
      Dialog.Font.Color = clWindowText
      Dialog.Font.Height = -11
      Dialog.Font.Name = 'Tahoma'
      Dialog.Font.Style = []
      Dialog.Options = [fdEffects, fdFixedPitchOnly]
      Dialog.OnApply = DialogFontEdit1FontDialogApply
      Hint = 'Font Select'
      BeforeExecute = DialogFontEdit1BeforeExecute
    end
    object actGutterLines: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'Gutter Lines'
      OnExecute = actGutterLinesExecute
    end
    object actCPP: TAction
      Category = 'Highlighter'
      Caption = 'C++'
      OnExecute = actCPPExecute
    end
    object actJavaScript: TAction
      Category = 'Highlighter'
      Caption = 'JavaScript'
      OnExecute = actJavaScriptExecute
    end
    object actPython: TAction
      Category = 'Highlighter'
      Caption = 'Python'
      OnExecute = actPythonExecute
    end
    object actCodeFolding: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'Code Folding'
      OnExecute = actCodeFoldingExecute
    end
    object actFoldAll: TAction
      Category = 'Fold'
      Caption = 'All'
      OnExecute = actFoldExecute
      OnUpdate = actFoldUpdate
    end
    object actUnFoldAll: TAction
      Category = 'Unfold'
      Caption = 'All'
      OnExecute = actFoldExecute
      OnUpdate = actFoldUpdate
    end
    object actFoldNearest: TAction
      Category = 'Fold'
      Caption = 'Nearest'
      OnExecute = actFoldExecute
      OnUpdate = actFoldUpdate
    end
    object actFoldRegions: TAction
      Category = 'Fold'
      Caption = 'Regions'
      OnExecute = actFoldExecute
      OnUpdate = actFoldUpdate
    end
    object actFoldLevel1: TAction
      Category = 'Fold'
      Caption = 'Level 1'
      OnExecute = actFoldExecute
      OnUpdate = actFoldUpdate
    end
    object actFoldLevel2: TAction
      Category = 'Fold'
      Caption = 'Level 2'
      OnExecute = actFoldExecute
      OnUpdate = actFoldUpdate
    end
    object actFoldLevel3: TAction
      Category = 'Fold'
      Caption = 'Level 3'
      OnExecute = actFoldExecute
      OnUpdate = actFoldUpdate
    end
    object actUnfoldNearest: TAction
      Category = 'Unfold'
      Caption = 'Nearest'
      OnExecute = actFoldExecute
      OnUpdate = actFoldUpdate
    end
    object actUnfoldRegions: TAction
      Category = 'Unfold'
      Caption = 'Regions'
      OnExecute = actFoldExecute
      OnUpdate = actFoldUpdate
    end
    object actUnfoldLevel1: TAction
      Category = 'Unfold'
      Caption = 'Level 1'
      OnExecute = actFoldExecute
      OnUpdate = actFoldUpdate
    end
    object actUnfoldLevel2: TAction
      Category = 'Unfold'
      Caption = 'Level 2'
      OnExecute = actFoldExecute
      OnUpdate = actFoldUpdate
    end
    object actUnfoldLevel3: TAction
      Category = 'Unfold'
      Caption = 'Level 3'
      OnExecute = actFoldExecute
      OnUpdate = actFoldUpdate
    end
    object actShowCollapsedMarks: TAction
      Category = 'Folding Options'
      AutoCheck = True
      Caption = 'Collapsed Marks'
      Checked = True
      OnExecute = actShowCollapsedMarksExecute
    end
    object actFoldShapeSize: TAction
      Category = 'Folding Options'
      Caption = 'Gutter Square Size..'
      OnExecute = actFoldShapeSizeExecute
    end
  end
  object PopupActionBar1: TPopupActionBar
    Left = 528
    Top = 32
    object N1: TMenuItem
      Caption = '-'
    end
    object Cut1: TMenuItem
      Action = EditCut1
    end
    object Copy1: TMenuItem
      Action = EditCopy1
    end
    object Paste1: TMenuItem
      Action = EditPaste1
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Fold1: TMenuItem
      Caption = 'Fold'
      object All1: TMenuItem
        Action = actFoldAll
      end
      object Nearest1: TMenuItem
        Action = actFoldNearest
      end
      object Ranges1: TMenuItem
        Action = actFoldRegions
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Level11: TMenuItem
        Action = actFoldLevel1
      end
      object Level21: TMenuItem
        Action = actFoldLevel2
      end
      object Level31: TMenuItem
        Action = actFoldLevel3
      end
    end
    object Unfold1: TMenuItem
      Caption = 'Unfold'
      object All2: TMenuItem
        Action = actUnFoldAll
      end
      object Nearest2: TMenuItem
        Action = actUnfoldNearest
      end
      object Ranges2: TMenuItem
        Action = actUnfoldRegions
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Level12: TMenuItem
        Action = actUnfoldLevel1
      end
      object Level22: TMenuItem
        Action = actUnfoldLevel2
      end
      object Level32: TMenuItem
        Action = actUnfoldLevel3
      end
    end
  end
  object SynEditPrint1: TSynEditPrint
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
    Left = 472
    Top = 176
  end
  object SynCppSyn1: TSynCppSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 524
    Top = 80
  end
  object SynJScriptSyn1: TSynJScriptSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 472
    Top = 80
  end
  object SynPythonSyn1: TSynPythonSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 480
    Top = 128
  end
  object SynEditPythonBehaviour1: TSynEditPythonBehaviour
    Editor = SynEdit1
    Left = 536
    Top = 128
  end
end
