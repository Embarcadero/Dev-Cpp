inherited MDIMainForm: TMDIMainForm
  Caption = 'Multiple Document Edit Demo'
  FormStyle = fsMDIForm
  OldCreateOrder = True
  WindowMenu = mWindow
  PixelsPerInch = 96
  TextHeight = 13
  inherited mnuMain: TMainMenu
    object mWindow: TMenuItem
      Caption = '&Window'
      object miWindowCascade: TMenuItem
        Action = actWindowCascade
      end
      object miWindowTile: TMenuItem
        Action = actWindowTileHorz
      end
      object miWindowTileVert: TMenuItem
        Action = actWindowTileVert
      end
      object miWindowArrange: TMenuItem
        Action = actWindowArrange
      end
    end
  end
  object actlWindow: TActionList
    Left = 28
    Top = 72
    object actWindowTileHorz: TWindowTileHorizontal
      Category = 'Window'
      Caption = '&Tile'
      Enabled = False
      ImageIndex = 15
    end
    object actWindowTileVert: TWindowTileVertical
      Category = 'Window'
      Caption = 'Tile &Vertically'
      Enabled = False
      ImageIndex = 16
    end
    object actWindowArrange: TWindowArrange
      Category = 'Window'
      Caption = '&Arrange Icons'
      Enabled = False
    end
    object actWindowCascade: TWindowCascade
      Category = 'Window'
      Caption = '&Cascade'
      Enabled = False
      ImageIndex = 17
    end
  end
end
