object CVSForm: TCVSForm
  Left = 238
  Top = 155
  Width = 562
  Height = 460
  BorderIcons = [biSystemMenu]
  Caption = 'CVS'
  Color = clBtnFace
  Constraints.MinHeight = 414
  Constraints.MinWidth = 407
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    554
    433)
  PixelsPerInch = 96
  TextHeight = 13
  object devPages1: TdevPages
    Left = 8
    Top = 8
    Width = 538
    Height = 382
    ActivePage = tabFiles
    Anchors = [akLeft, akTop, akRight, akBottom]
    object tabImport: TdevPage
      Left = 0
      Top = 23
      Width = 538
      Height = 359
      HorzScrollBar.Smooth = True
      HorzScrollBar.Tracking = True
      VertScrollBar.Smooth = True
      VertScrollBar.Tracking = True
      Align = alClient
      BevelKind = bkTile
      TabOrder = 0
      Visible = False
      Caption = 'Import'
      DesignSize = (
        534
        355)
      object lblCVSImportDir: TLabel
        Left = 12
        Top = 16
        Width = 75
        Height = 13
        Caption = 'Import directory:'
      end
      object lblImpAction: TLabel
        Left = 12
        Top = 232
        Width = 33
        Height = 13
        Caption = 'Action:'
      end
      object lblImpVendor: TLabel
        Left = 12
        Top = 72
        Width = 37
        Height = 13
        Caption = 'Vendor:'
      end
      object lblImpRelease: TLabel
        Left = 12
        Top = 96
        Width = 42
        Height = 13
        Caption = 'Release:'
      end
      object lblImpMsg: TLabel
        Left = 12
        Top = 120
        Width = 66
        Height = 13
        Caption = 'Log message:'
      end
      object lblImpModule: TLabel
        Left = 12
        Top = 44
        Width = 67
        Height = 13
        Caption = 'Module name:'
      end
      object txtCVSImportDir: TEdit
        Left = 116
        Top = 12
        Width = 384
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        ReadOnly = True
        TabOrder = 0
        Text = 'txtCVSImportDir'
      end
      object btnCVSImportBrws: TButton
        Tag = 6
        Left = 501
        Top = 12
        Width = 20
        Height = 21
        Anchors = [akTop, akRight]
        Caption = '. . .'
        TabOrder = 1
        OnClick = btnCVSImportBrwsClick
      end
      object vle: TValueListEditor
        Left = 116
        Top = 220
        Width = 404
        Height = 124
        Anchors = [akLeft, akRight, akBottom]
        FixedCols = 1
        Options = [goVertLine, goHorzLine, goColSizing, goEditing, goAlwaysShowEditor, goThumbTracking]
        TabOrder = 6
        TitleCaptions.Strings = (
          'Extension'
          'Action')
        OnGetPickList = vleGetPickList
        ColWidths = (
          94
          304)
      end
      object txtImpVendor: TEdit
        Left = 116
        Top = 68
        Width = 121
        Height = 21
        TabOrder = 3
        Text = 'txtImpVendor'
        OnChange = txtImpModuleChange
      end
      object txtImpRelease: TEdit
        Left = 116
        Top = 92
        Width = 121
        Height = 21
        TabOrder = 4
        Text = 'txtImpRelease'
        OnChange = txtImpModuleChange
      end
      object memImpMsg: TMemo
        Left = 116
        Top = 120
        Width = 404
        Height = 89
        Anchors = [akLeft, akTop, akRight, akBottom]
        Lines.Strings = (
          'memImpMsg')
        ScrollBars = ssBoth
        TabOrder = 5
        WantTabs = True
        WordWrap = False
      end
      object txtImpModule: TEdit
        Left = 116
        Top = 40
        Width = 121
        Height = 21
        TabOrder = 2
        Text = 'txtImpModule'
        OnChange = txtImpModuleChange
      end
    end
    object tabCheckout: TdevPage
      Left = 0
      Top = 23
      Width = 538
      Height = 359
      HorzScrollBar.Smooth = True
      HorzScrollBar.Tracking = True
      VertScrollBar.Smooth = True
      VertScrollBar.Tracking = True
      Align = alClient
      BevelKind = bkTile
      TabOrder = 3
      Visible = False
      Caption = 'Checkout'
      DesignSize = (
        534
        355)
      object lblCOModule: TLabel
        Left = 12
        Top = 16
        Width = 67
        Height = 13
        Caption = 'Module name:'
      end
      object lblCODir: TLabel
        Left = 12
        Top = 104
        Width = 92
        Height = 13
        Caption = 'Checkout directory:'
      end
      object txtCOmodule: TEdit
        Left = 116
        Top = 12
        Width = 213
        Height = 21
        TabOrder = 0
        Text = 'txtCOmodule'
        OnChange = txtCOmoduleChange
      end
      object txtCOdir: TEdit
        Left = 116
        Top = 100
        Width = 340
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        ReadOnly = True
        TabOrder = 3
        Text = 'txtCOdir'
        OnChange = txtCOmoduleChange
      end
      object btnCOBrws: TButton
        Tag = 6
        Left = 457
        Top = 100
        Width = 20
        Height = 21
        Anchors = [akTop, akRight]
        Caption = '. . .'
        TabOrder = 4
        OnClick = btnCOBrwsClick
      end
      object chkCOrecurse: TCheckBox
        Left = 12
        Top = 136
        Width = 464
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Recurse into sub-directories'
        Checked = True
        State = cbChecked
        TabOrder = 5
      end
      object txtCOModuleAs: TEdit
        Left = 116
        Top = 36
        Width = 213
        Height = 21
        Enabled = False
        TabOrder = 2
        Text = 'txtCOModuleAs'
      end
      object chkCOModuleAs: TCheckBox
        Left = 12
        Top = 40
        Width = 97
        Height = 17
        Caption = 'Checkout as:'
        TabOrder = 1
        OnClick = chkCOModuleAsClick
      end
      object cmbCOBeforeDate: TComboBox
        Left = 180
        Top = 164
        Width = 165
        Height = 21
        ItemHeight = 13
        TabOrder = 6
        Text = 'cmbCOBeforeDate'
      end
      object chkCOBeforeDate: TCheckBox
        Left = 12
        Top = 168
        Width = 161
        Height = 17
        Caption = 'Before date:'
        TabOrder = 7
        OnClick = chkCOBeforeDateClick
      end
      object chkCORevision: TCheckBox
        Left = 12
        Top = 192
        Width = 161
        Height = 17
        Caption = 'Revision/branch/tag:'
        TabOrder = 8
        OnClick = chkCORevisionClick
      end
      object cmbCORevision: TComboBox
        Left = 180
        Top = 188
        Width = 165
        Height = 21
        ItemHeight = 13
        TabOrder = 9
        Text = 'cmbCORevision'
      end
      object chkCOMostRecent: TCheckBox
        Left = 12
        Top = 228
        Width = 337
        Height = 17
        Caption = 'If not found, get the most recent'
        TabOrder = 10
      end
    end
    object tabCommit: TdevPage
      Left = 0
      Top = 23
      Width = 538
      Height = 359
      HorzScrollBar.Smooth = True
      HorzScrollBar.Tracking = True
      VertScrollBar.Smooth = True
      VertScrollBar.Tracking = True
      Align = alClient
      BevelKind = bkTile
      TabOrder = 4
      Visible = False
      Caption = 'Commit'
      DesignSize = (
        534
        355)
      object lblCommitMsg: TLabel
        Left = 12
        Top = 16
        Width = 46
        Height = 13
        Caption = 'Message:'
      end
      object memCommitMsg: TMemo
        Left = 116
        Top = 12
        Width = 360
        Height = 285
        Anchors = [akLeft, akTop, akRight, akBottom]
        ScrollBars = ssBoth
        TabOrder = 0
        WantTabs = True
        WordWrap = False
      end
    end
    object tabUpdate: TdevPage
      Left = 0
      Top = 23
      Width = 538
      Height = 359
      HorzScrollBar.Smooth = True
      HorzScrollBar.Tracking = True
      VertScrollBar.Smooth = True
      VertScrollBar.Tracking = True
      Align = alClient
      BevelKind = bkTile
      TabOrder = 5
      Visible = False
      Caption = 'Update'
      DesignSize = (
        534
        355)
      object chkUpdRecurse: TCheckBox
        Left = 12
        Top = 16
        Width = 440
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Recurse into sub-directories'
        Checked = True
        State = cbChecked
        TabOrder = 0
      end
      object chkUpdResetSticky: TCheckBox
        Left = 12
        Top = 44
        Width = 440
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Reset any sticky options'
        TabOrder = 1
      end
      object chkUpdCreateDirs: TCheckBox
        Left = 12
        Top = 72
        Width = 440
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Create missing directories'
        Checked = True
        State = cbChecked
        TabOrder = 2
      end
      object chkUpdCleanCopy: TCheckBox
        Left = 12
        Top = 128
        Width = 440
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Drop changes made locally'
        TabOrder = 4
      end
      object grpUpdRevisions: TGroupBox
        Left = 12
        Top = 184
        Width = 468
        Height = 113
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Other revisions:'
        TabOrder = 5
        object chkBeforeDate: TCheckBox
          Left = 12
          Top = 24
          Width = 161
          Height = 17
          Caption = 'Before date:'
          TabOrder = 0
          OnClick = chkBeforeDateClick
        end
        object chkRevision: TCheckBox
          Left = 12
          Top = 48
          Width = 161
          Height = 17
          Caption = 'Revision/branch/tag:'
          TabOrder = 1
          OnClick = chkRevisionClick
        end
        object chkMostRecent: TCheckBox
          Left = 12
          Top = 84
          Width = 337
          Height = 17
          Caption = 'If not found, get the most recent'
          TabOrder = 2
        end
        object cmbBeforeDate: TComboBox
          Left = 180
          Top = 20
          Width = 165
          Height = 21
          ItemHeight = 0
          TabOrder = 3
          Text = 'cmbBeforeDate'
        end
        object cmbRevision: TComboBox
          Left = 180
          Top = 44
          Width = 165
          Height = 21
          ItemHeight = 0
          TabOrder = 4
          Text = 'cmbRevision'
        end
      end
      object chkUpdPrune: TCheckBox
        Left = 12
        Top = 100
        Width = 440
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Remove empty directories'
        Checked = True
        State = cbChecked
        TabOrder = 3
      end
    end
    object tabDiff: TdevPage
      Left = 0
      Top = 23
      Width = 538
      Height = 359
      HorzScrollBar.Smooth = True
      HorzScrollBar.Tracking = True
      VertScrollBar.Smooth = True
      VertScrollBar.Tracking = True
      Align = alClient
      BevelKind = bkTile
      TabOrder = 6
      Visible = False
      Caption = 'Diff'
      DesignSize = (
        534
        355)
      object chkDiffRecurse: TCheckBox
        Left = 12
        Top = 16
        Width = 440
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Recurse into sub-directories'
        Checked = True
        State = cbChecked
        TabOrder = 0
      end
      object chkDiffUnified: TCheckBox
        Left = 12
        Top = 44
        Width = 440
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Perform a unified diff'
        TabOrder = 1
      end
      object grpDiff: TGroupBox
        Left = 12
        Top = 76
        Width = 468
        Height = 157
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Diff revisions'
        TabOrder = 2
        object lblDiffRev1: TLabel
          Left = 32
          Top = 100
          Width = 111
          Height = 13
          Caption = 'Revision/branch/tag 1:'
        end
        object lblDiffRev2: TLabel
          Left = 32
          Top = 124
          Width = 111
          Height = 13
          Caption = 'Revision/branch/tag 2:'
        end
        object txtDiffRev1: TEdit
          Left = 172
          Top = 96
          Width = 85
          Height = 21
          Enabled = False
          TabOrder = 0
          Text = 'txtDiffRev1'
        end
        object txtDiffRev2: TEdit
          Left = 172
          Top = 120
          Width = 85
          Height = 21
          Enabled = False
          TabOrder = 2
          Text = 'txtDiffRev2'
        end
        object chkDiffDate1: TCheckBox
          Left = 260
          Top = 96
          Width = 93
          Height = 17
          Caption = 'Is date'
          Enabled = False
          TabOrder = 1
        end
        object chkDiffDate2: TCheckBox
          Left = 260
          Top = 120
          Width = 93
          Height = 17
          Caption = 'Is date'
          Enabled = False
          TabOrder = 3
        end
        object rgbDiff: TRadioButton
          Left = 12
          Top = 24
          Width = 333
          Height = 17
          Caption = 'Compare local file with the same remote revision/tag'
          Checked = True
          TabOrder = 4
          TabStop = True
          OnClick = rgbDiff1Click
        end
        object rgbDiff1: TRadioButton
          Left = 12
          Top = 48
          Width = 333
          Height = 17
          Caption = 'Compare local file with another remote revision'
          TabOrder = 5
          OnClick = rgbDiff1Click
        end
        object rgbDiff2: TRadioButton
          Left = 12
          Top = 72
          Width = 333
          Height = 17
          Caption = 'Compare two remote revisions'
          TabOrder = 6
          OnClick = rgbDiff1Click
        end
      end
    end
    object tabLog: TdevPage
      Left = 0
      Top = 23
      Width = 538
      Height = 359
      HorzScrollBar.Smooth = True
      HorzScrollBar.Tracking = True
      VertScrollBar.Smooth = True
      VertScrollBar.Tracking = True
      Align = alClient
      BevelKind = bkTile
      TabOrder = 7
      Visible = False
      Caption = 'Log'
      DesignSize = (
        534
        355)
      object chkLogRecurse: TCheckBox
        Left = 12
        Top = 16
        Width = 484
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Recurse into sub-directories'
        Checked = True
        State = cbChecked
        TabOrder = 0
      end
      object chkLogDefBranch: TCheckBox
        Left = 12
        Top = 44
        Width = 484
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Log the default branch'
        TabOrder = 1
      end
      object chkLogRCS: TCheckBox
        Left = 12
        Top = 72
        Width = 484
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'List only the RCS filenames'
        TabOrder = 2
      end
      object chkLogNoTag: TCheckBox
        Left = 12
        Top = 96
        Width = 484
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Don'#39't print tag names'
        TabOrder = 3
      end
      object grpLogFilter: TGroupBox
        Left = 12
        Top = 132
        Width = 508
        Height = 101
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Filter'
        TabOrder = 4
        object chkLogFbyRev: TCheckBox
          Left = 16
          Top = 24
          Width = 121
          Height = 17
          Caption = 'By revision/tag:'
          TabOrder = 0
          OnClick = chkLogFbyRevClick
        end
        object chkLogFbyDate: TCheckBox
          Left = 16
          Top = 48
          Width = 121
          Height = 17
          Caption = 'By date:'
          TabOrder = 2
          OnClick = chkLogFbyDateClick
        end
        object chkLogFbyUser: TCheckBox
          Left = 16
          Top = 72
          Width = 121
          Height = 17
          Caption = 'By user:'
          TabOrder = 4
          OnClick = chkLogFbyUserClick
        end
        object cmbLogFbyRev: TComboBox
          Left = 140
          Top = 20
          Width = 201
          Height = 21
          Enabled = False
          ItemHeight = 13
          TabOrder = 1
          Text = 'cmbLogFbyRev'
          Items.Strings = (
            '1.18'
            ':1.24'
            '1.24:'
            '1.18:1.24'
            'release1-0'
            'HEAD')
        end
        object cmbLogFbyDate: TComboBox
          Left = 140
          Top = 44
          Width = 201
          Height = 21
          Enabled = False
          ItemHeight = 0
          TabOrder = 3
          Text = 'cmbLogFbyDate'
        end
        object cmbLogFbyUser: TComboBox
          Left = 140
          Top = 68
          Width = 201
          Height = 21
          Enabled = False
          ItemHeight = 13
          TabOrder = 5
          Text = 'cmbLogFbyUser'
          Items.Strings = (
            'joe'
            'doe'
            'joe,doe')
        end
      end
    end
    object tabAdd: TdevPage
      Left = 0
      Top = 23
      Width = 538
      Height = 359
      HorzScrollBar.Smooth = True
      HorzScrollBar.Tracking = True
      VertScrollBar.Smooth = True
      VertScrollBar.Tracking = True
      Align = alClient
      BevelKind = bkTile
      TabOrder = 9
      Visible = False
      Caption = 'Add'
      DesignSize = (
        534
        355)
      object lblAddMsg: TLabel
        Left = 12
        Top = 16
        Width = 46
        Height = 13
        Caption = 'Message:'
      end
      object memAddMsg: TMemo
        Left = 116
        Top = 12
        Width = 360
        Height = 285
        Anchors = [akLeft, akTop, akRight, akBottom]
        ScrollBars = ssBoth
        TabOrder = 0
        WantTabs = True
        WordWrap = False
      end
    end
    object tabRemove: TdevPage
      Left = 0
      Top = 23
      Width = 538
      Height = 359
      HorzScrollBar.Smooth = True
      HorzScrollBar.Tracking = True
      VertScrollBar.Smooth = True
      VertScrollBar.Tracking = True
      Align = alClient
      BevelKind = bkTile
      TabOrder = 10
      Visible = False
      Caption = 'Remove'
      DesignSize = (
        534
        355)
      object chkRemove: TCheckBox
        Left = 12
        Top = 16
        Width = 440
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Remove the file from disk also...'
        TabOrder = 0
      end
    end
    object tabFiles: TdevPage
      Left = 0
      Top = 23
      Width = 538
      Height = 359
      HorzScrollBar.Smooth = True
      HorzScrollBar.Tracking = True
      VertScrollBar.Smooth = True
      VertScrollBar.Tracking = True
      Align = alClient
      BevelKind = bkTile
      TabOrder = 11
	  Visible = False
      Caption = 'Files'
      DesignSize = (
        534
        355)
      object lblFiles: TLabel
        Left = 12
        Top = 16
        Width = 24
        Height = 13
        Caption = 'Files:'
      end
      object lstFiles: TCheckListBox
        Left = 116
        Top = 12
        Width = 403
        Height = 329
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ItemHeight = 14
        ParentFont = False
        Sorted = True
        Style = lbOwnerDrawFixed
        TabOrder = 0
        OnDrawItem = lstFilesDrawItem
      end
    end
    object tabRepos: TdevPage
      Left = 0
      Top = 23
      Width = 538
      Height = 359
      HorzScrollBar.Smooth = True
      HorzScrollBar.Tracking = True
      VertScrollBar.Smooth = True
      VertScrollBar.Tracking = True
      Align = alClient
      BevelKind = bkTile
      TabOrder = 1
	  Visible = False
      Caption = 'Repository'
      DesignSize = (
        534
        355)
      object lblRep: TLabel
        Left = 12
        Top = 16
        Width = 76
        Height = 13
        Caption = 'Access method:'
      end
      object grpRepDetails: TGroupBox
        Left = 12
        Top = 44
        Width = 512
        Height = 165
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Repository details'
        TabOrder = 0
        object lblMethod: TLabel
          Left = 16
          Top = 20
          Width = 76
          Height = 13
          Caption = 'Access method:'
        end
        object lblUser: TLabel
          Left = 16
          Top = 44
          Width = 25
          Height = 13
          Caption = 'User:'
        end
        object lblServer: TLabel
          Left = 16
          Top = 68
          Width = 34
          Height = 13
          Caption = 'Server:'
        end
        object lblDir: TLabel
          Left = 16
          Top = 116
          Width = 83
          Height = 13
          Caption = 'Remote directory:'
        end
        object lblRepos: TLabel
          Left = 16
          Top = 140
          Width = 329
          Height = 17
          AutoSize = False
          Caption = 'lblRepos'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lblPort: TLabel
          Left = 16
          Top = 92
          Width = 60
          Height = 13
          Caption = 'Port number:'
        end
        object txtUser: TEdit
          Left = 120
          Top = 40
          Width = 225
          Height = 21
          TabOrder = 0
          Text = 'txtUser'
          OnChange = cmbMethodChange
        end
        object txtServer: TEdit
          Left = 120
          Top = 64
          Width = 225
          Height = 21
          TabOrder = 1
          Text = 'txtServer'
          OnChange = cmbMethodChange
        end
        object txtDir: TEdit
          Left = 120
          Top = 112
          Width = 225
          Height = 21
          TabOrder = 3
          Text = 'txtDir'
          OnChange = cmbMethodChange
        end
        object cmbMethod: TComboBox
          Left = 120
          Top = 16
          Width = 105
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          ItemIndex = 1
          TabOrder = 4
          Text = 'pserver'
          OnChange = cmbMethodChange
          Items.Strings = (
            'local'
            'pserver'
            'ext'
            'sspi')
        end
        object txtPort: TEdit
          Left = 120
          Top = 88
          Width = 225
          Height = 21
          TabOrder = 2
          Text = 'txtPort'
          OnChange = cmbMethodChange
        end
      end
      object cmbRepos: TComboBox
        Left = 116
        Top = 12
        Width = 408
        Height = 21
        AutoDropDown = True
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 0
        TabOrder = 1
        Text = 'cmbRepos'
        OnChange = cmbReposChange
      end
    end
    object tabGlobal: TdevPage
      Left = 0
      Top = 23
      Width = 538
      Height = 359
      HorzScrollBar.Smooth = True
      HorzScrollBar.Tracking = True
      VertScrollBar.Smooth = True
      VertScrollBar.Tracking = True
      Align = alClient
      BevelKind = bkTile
      TabOrder = 2
      Visible = False
      Caption = 'Global Options'
      DesignSize = (
        534
        355)
      object lblCompression: TLabel
        Left = 12
        Top = 16
        Width = 63
        Height = 13
        Caption = 'Compression:'
      end
      object spnCompression: TSpinEdit
        Left = 172
        Top = 12
        Width = 49
        Height = 22
        MaxLength = 1
        MaxValue = 9
        MinValue = 0
        TabOrder = 0
        Value = 0
      end
      object chkUseSSH: TCheckBox
        Left = 12
        Top = 44
        Width = 464
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Use SSH instead of RSH'
        Checked = True
        State = cbChecked
        TabOrder = 1
      end
    end
    object tabOutput: TdevPage
      Left = 0
      Top = 23
      Width = 538
      Height = 359
      HorzScrollBar.Smooth = True
      HorzScrollBar.Tracking = True
      VertScrollBar.Smooth = True
      VertScrollBar.Tracking = True
      Align = alClient
      BevelKind = bkTile
      TabOrder = 8
      Visible = False
      Caption = 'CVS Output'
      DesignSize = (
        534
        355)
      object memOutput: TRichEdit
        Left = 4
        Top = 4
        Width = 526
        Height = 344
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'memOutput')
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
        WantReturns = False
        WordWrap = False
      end
    end
  end
  object btnOK: TButton
    Left = 357
    Top = 400
    Width = 91
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 453
    Top = 400
    Width = 91
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = btnCancelClick
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
    Top = 288
  end
end
