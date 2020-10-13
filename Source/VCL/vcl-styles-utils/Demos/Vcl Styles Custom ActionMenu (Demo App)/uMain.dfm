object Form4: TForm4
  Left = 517
  Top = 293
  Caption = 'Demo'
  ClientHeight = 243
  ClientWidth = 527
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PopupMenu = PopupActionBar1
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ActionMainMenuBar1: TActionMainMenuBar
    Left = 0
    Top = 0
    Width = 527
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
  object Button1: TButton
    Left = 8
    Top = 192
    Width = 75
    Height = 25
    Caption = 'Press Me'
    TabOrder = 1
    OnClick = Button1Click
  end
  object ActionManager1: TActionManager
    ActionBars = <
      item
        Items = <
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
                Action = FileOpen1
                ImageIndex = 7
                ShortCut = 16463
              end
              item
                Action = FileOpenWith1
                Caption = 'O&pen with...'
              end
              item
                Action = FileSaveAs1
                ImageIndex = 30
              end
              item
                Action = FilePrintSetup1
              end
              item
                Action = FilePageSetup1
                Caption = 'Pa&ge Setup...'
              end
              item
                Action = FileRun1
              end
              item
                Action = FileExit1
                ImageIndex = 43
              end
              item
                Action = BrowseForFolder1
                Caption = '&BrowseForFolder1'
              end>
            Caption = '&File'
          end
          item
            Items = <
              item
                Action = FormatRichEditBold1
                ImageIndex = 31
                ShortCut = 16450
              end
              item
                Action = FormatRichEditItalic1
                ImageIndex = 29
                ShortCut = 16457
              end
              item
                Action = FormatRichEditUnderline1
                ImageIndex = 28
                ShortCut = 16469
              end
              item
                Action = FormatRichEditStrikeOut1
                ImageIndex = 44
              end
              item
                Action = FormatRichEditBullets1
                Caption = 'Bu&llets'
                ImageIndex = 38
              end
              item
                Action = FormatRichEditAlignLeft1
                Caption = '&Align Left'
                ImageIndex = 35
              end
              item
                Action = FormatRichEditAlignRight1
                ImageIndex = 36
              end
              item
                Action = FormatRichEditAlignCenter1
                ImageIndex = 37
              end
              item
                Action = FileOpen1
                ImageIndex = 7
                ShortCut = 16463
              end
              item
                Action = FileOpenWith1
                Caption = 'O&pen with...'
              end
              item
                Action = FileSaveAs1
                Caption = 'Sa&ve As...'
                ImageIndex = 30
              end
              item
                Action = FilePrintSetup1
                Caption = 'Pri&nt Setup...'
              end
              item
                Action = FilePageSetup1
                Caption = 'Pa&ge Setup...'
              end
              item
                Action = FileRun1
                Caption = 'Run...'
              end
              item
                Action = FileExit1
                ImageIndex = 43
              end
              item
                Action = BrowseForFolder1
                Caption = 'Bro&wseForFolder1'
              end>
            Caption = 'F&ormat'
          end
          item
            Items = <
              item
                Action = HelpContents1
                ImageIndex = 40
              end
              item
                Action = HelpTopicSearch1
                ImageIndex = 9
              end
              item
                Action = HelpOnHelp1
              end
              item
                Action = HelpContextAction1
                Caption = 'H&elpContextAction1'
                ImageIndex = 11
              end>
            Caption = '&Help'
          end
          item
            Items = <
              item
                Action = WindowClose1
              end
              item
                Action = WindowCascade1
                ImageIndex = 17
              end
              item
                Action = WindowTileHorizontal1
                ImageIndex = 15
              end
              item
                Action = WindowTileVertical1
                ImageIndex = 16
              end
              item
                Action = WindowMinimizeAll1
              end
              item
                Action = WindowArrange1
              end>
            Caption = '&Window'
          end>
        ActionBar = ActionMainMenuBar1
      end>
    Left = 344
    Top = 104
    StyleName = 'Platform Default'
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
    object FormatRichEditBold1: TRichEditBold
      Category = 'Format'
      AutoCheck = True
      Caption = '&Bold'
      Hint = 'Bold'
      ImageIndex = 31
      ShortCut = 16450
    end
    object FormatRichEditItalic1: TRichEditItalic
      Category = 'Format'
      AutoCheck = True
      Caption = '&Italic'
      Hint = 'Italic'
      ImageIndex = 29
      ShortCut = 16457
    end
    object FormatRichEditUnderline1: TRichEditUnderline
      Category = 'Format'
      AutoCheck = True
      Caption = '&Underline'
      Hint = 'Underline'
      ImageIndex = 28
      ShortCut = 16469
    end
    object FormatRichEditStrikeOut1: TRichEditStrikeOut
      Category = 'Format'
      AutoCheck = True
      Caption = '&Strikeout'
      Hint = 'Strikeout'
      ImageIndex = 44
    end
    object FormatRichEditBullets1: TRichEditBullets
      Category = 'Format'
      AutoCheck = True
      Caption = '&Bullets'
      Hint = 'Bullets|Inserts a bullet on the current line'
      ImageIndex = 38
    end
    object FormatRichEditAlignLeft1: TRichEditAlignLeft
      Category = 'Format'
      AutoCheck = True
      Caption = 'Align &Left'
      Hint = 'Align Left|Aligns text at the left indent'
      ImageIndex = 35
    end
    object FormatRichEditAlignRight1: TRichEditAlignRight
      Category = 'Format'
      AutoCheck = True
      Caption = 'Align &Right'
      Hint = 'Align Right|Aligns text at the right indent'
      ImageIndex = 36
    end
    object FormatRichEditAlignCenter1: TRichEditAlignCenter
      Category = 'Format'
      AutoCheck = True
      Caption = '&Center'
      Hint = 'Center|Centers text between margins'
      ImageIndex = 37
    end
    object HelpContents1: THelpContents
      Category = 'Help'
      Caption = '&Contents'
      Enabled = False
      Hint = 'Help Contents'
      ImageIndex = 40
    end
    object HelpTopicSearch1: THelpTopicSearch
      Category = 'Help'
      Caption = '&Topic Search'
      Enabled = False
      Hint = 'Topic Search'
      ImageIndex = 9
    end
    object HelpOnHelp1: THelpOnHelp
      Category = 'Help'
      Caption = '&Help on Help'
      Enabled = False
      Hint = 'Help on help'
    end
    object HelpContextAction1: THelpContextAction
      Category = 'Help'
      Caption = 'HelpContextAction1'
      Enabled = False
      ImageIndex = 11
    end
    object WindowClose1: TWindowClose
      Category = 'Window'
      Caption = 'C&lose'
      Enabled = False
      Hint = 'Close'
    end
    object WindowCascade1: TWindowCascade
      Category = 'Window'
      Caption = '&Cascade'
      Enabled = False
      Hint = 'Cascade'
      ImageIndex = 17
    end
    object WindowTileHorizontal1: TWindowTileHorizontal
      Category = 'Window'
      Caption = 'Tile &Horizontally'
      Enabled = False
      Hint = 'Tile Horizontal'
      ImageIndex = 15
    end
    object WindowTileVertical1: TWindowTileVertical
      Category = 'Window'
      Caption = '&Tile Vertically'
      Enabled = False
      Hint = 'Tile Vertical'
      ImageIndex = 16
    end
    object WindowMinimizeAll1: TWindowMinimizeAll
      Category = 'Window'
      Caption = '&Minimize All'
      Enabled = False
      Hint = 'Minimize All'
    end
    object WindowArrange1: TWindowArrange
      Category = 'Window'
      Caption = '&Arrange'
      Enabled = False
    end
    object FileOpen1: TFileOpen
      Category = 'Format'
      Caption = '&Open...'
      Hint = 'Open|Opens an existing file'
      ImageIndex = 7
      ShortCut = 16463
    end
    object FileOpenWith1: TFileOpenWith
      Category = 'Format'
      Caption = 'Open with...'
    end
    object FileSaveAs1: TFileSaveAs
      Category = 'Format'
      Caption = 'Save &As...'
      Hint = 'Save As|Saves the active file with a new name'
      ImageIndex = 30
    end
    object FilePrintSetup1: TFilePrintSetup
      Category = 'Format'
      Caption = 'Print Set&up...'
      Hint = 'Print Setup'
    end
    object FilePageSetup1: TFilePageSetup
      Category = 'Format'
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
    object FileRun1: TFileRun
      Category = 'Format'
      Browse = False
      BrowseDlg.Title = 'Run'
      Caption = '&Run...'
      Hint = 'Run|Runs an application'
      Operation = 'open'
      ShowCmd = scShowNormal
    end
    object FileExit1: TFileExit
      Category = 'Format'
      Caption = 'E&xit'
      Hint = 'Exit|Quits the application'
      ImageIndex = 43
    end
    object BrowseForFolder1: TBrowseForFolder
      Category = 'Format'
      Caption = 'BrowseForFolder1'
      DialogCaption = 'BrowseForFolder1'
      BrowseOptions = []
    end
  end
  object PopupActionBar1: TPopupActionBar
    Left = 184
    Top = 96
  end
end
