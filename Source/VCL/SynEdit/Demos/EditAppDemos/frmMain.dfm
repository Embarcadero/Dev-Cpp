object MainForm: TMainForm
  Left = 186
  Top = 133
  Width = 514
  Height = 346
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = mnuMain
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TStatusBar
    Left = 0
    Top = 273
    Width = 506
    Height = 19
    Action = actUpdateStatusBarPanels
    Panels = <
      item
        Alignment = taCenter
        Width = 84
      end
      item
        Alignment = taCenter
        Width = 72
      end
      item
        Alignment = taCenter
        Width = 84
      end
      item
        Width = 50
      end>
  end
  object mnuMain: TMainMenu
    Left = 28
    Top = 32
    object mFile: TMenuItem
      Caption = '&File'
      OnClick = mFileClick
      object miFileNew: TMenuItem
        Action = actFileNew
      end
      object miFileOpen: TMenuItem
        Action = actFileOpen
      end
      object mRecentFiles: TMenuItem
        Caption = '&Recent Files'
        OnClick = mRecentFilesClick
        object miFileMRU1: TMenuItem
          Caption = '[MRU1]'
          OnClick = OnOpenMRUFile
        end
        object miFileMRU2: TMenuItem
          Caption = '[MRU2]'
          OnClick = OnOpenMRUFile
        end
        object miFileMRU3: TMenuItem
          Caption = '[MRU3]'
          OnClick = OnOpenMRUFile
        end
        object miFileMRU4: TMenuItem
          Caption = '[MRU4]'
          OnClick = OnOpenMRUFile
        end
        object miFileMRU5: TMenuItem
          Caption = '[MRU5]'
          OnClick = OnOpenMRUFile
        end
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object miFileSave: TMenuItem
        Action = CommandsDataModule.actFileSave
      end
      object miFileSaveAs: TMenuItem
        Action = CommandsDataModule.actFileSaveAs
      end
      object miFileClose: TMenuItem
        Action = CommandsDataModule.actFileClose
      end
      object miFileCloseAll: TMenuItem
        Action = actFileCloseAll
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object miFilePrint: TMenuItem
        Action = CommandsDataModule.actFilePrint
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object miFileExit: TMenuItem
        Action = actFileExit
      end
    end
    object mEdit: TMenuItem
      Caption = '&Edit'
      object miEditUndo: TMenuItem
        Action = CommandsDataModule.actEditUndo
      end
      object miEditRedo: TMenuItem
        Action = CommandsDataModule.actEditRedo
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object miEditCut: TMenuItem
        Action = CommandsDataModule.actEditCut
      end
      object miEditCopy: TMenuItem
        Action = CommandsDataModule.actEditCopy
      end
      object miEditPaste: TMenuItem
        Action = CommandsDataModule.actEditPaste
      end
      object miEditDelete: TMenuItem
        Action = CommandsDataModule.actEditDelete
      end
      object miEditSelectAll: TMenuItem
        Action = CommandsDataModule.actEditSelectAll
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object miEditFind: TMenuItem
        Action = CommandsDataModule.actSearchFind
      end
      object miEditFindNext: TMenuItem
        Action = CommandsDataModule.actSearchFindNext
      end
      object miEditFindPrev: TMenuItem
        Action = CommandsDataModule.actSearchFindPrev
      end
      object miEditReplace: TMenuItem
        Action = CommandsDataModule.actSearchReplace
      end
    end
    object mView: TMenuItem
      Caption = '&View'
      object miViewStatusbar: TMenuItem
        Action = actViewStatusbar
      end
    end
  end
  object actlStandard: TActionList
    Left = 28
    Top = 108
    object actFileNew: TAction
      Category = 'File'
      Caption = '&New'
      ShortCut = 16462
      OnExecute = actFileNewExecute
      OnUpdate = actFileNewOrOpenUpdate
    end
    object actFileOpen: TAction
      Category = 'File'
      Caption = '&Open...'
      ShortCut = 16463
      OnExecute = actFileOpenExecute
      OnUpdate = actFileNewOrOpenUpdate
    end
    object actFileCloseAll: TAction
      Category = 'File'
      Caption = 'Close All Fi&les'
      Enabled = False
      ShortCut = 24691
      OnExecute = actFileCloseAllExecute
      OnUpdate = actFileCloseAllUpdate
    end
    object actFileExit: TAction
      Category = 'File'
      Caption = 'E&xit'
      ShortCut = 32883
      OnExecute = actFileExitExecute
    end
    object actViewStatusbar: TAction
      Category = 'View'
      Caption = '&Status Bar'
      OnExecute = actViewStatusbarExecute
      OnUpdate = actViewStatusbarUpdate
    end
    object actUpdateStatusBarPanels: TAction
      Caption = 'actUpdateStatusBarPanels'
      OnUpdate = actUpdateStatusBarPanelsUpdate
    end
  end
end
