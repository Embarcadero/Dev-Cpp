{
    This file is part of Dev-C++
    Copyright (c) 2004 Bloodshed Software

    Dev-C++ is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Dev-C++ is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Dev-C++; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$APPTYPE GUI}
unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, System.UITypes,
  Menus, StdCtrls, ComCtrls, ToolWin, ExtCtrls, Buttons, utils, SynEditPrint,
  Project, editor, DateUtils, compiler, ActnList, ToolFrm, AppEvnts,
  debugger, ClassBrowser, CodeCompletion, CppParser, CppTokenizer, SyncObjs,
  StrUtils, SynEditTypes, devFileMonitor, devMonitorTypes, DdeMan, EditorList,
  devShortcuts, debugreader, ExceptionFrm, CommCtrl, devcfg, SynEditTextBuffer,
  CppPreprocessor, CBUtils, StatementList, FormatterOptionsFrm, System.Actions;

type
  TRunEndAction = (reaNone, reaProfile);
  TCompSuccessAction = (csaNone, csaRun, csaDebug, csaProfile);

  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    NewprojectItem: TMenuItem;
    NewTemplateItem: TMenuItem;
    N34: TMenuItem;
    OpenprojectItem: TMenuItem;
    ClearhistoryItem: TMenuItem;
    N11: TMenuItem;
    NewSourceFileItem: TMenuItem;
    SaveUnitItem: TMenuItem;
    SaveUnitAsItem: TMenuItem;
    SaveallItem: TMenuItem;
    N33: TMenuItem;
    CloseprojectItem: TMenuItem;
    CloseItem: TMenuItem;
    ExportItem: TMenuItem;
    HTMLItem: TMenuItem;
    RTFItem: TMenuItem;
    N19: TMenuItem;
    ProjecttoHTMLItem: TMenuItem;
    PrintItem: TMenuItem;
    PrinterSetupItem: TMenuItem;
    N3: TMenuItem;
    ExitItem: TMenuItem;
    EditMenu: TMenuItem;
    UndoItem: TMenuItem;
    RedoItem: TMenuItem;
    N4: TMenuItem;
    CutItem: TMenuItem;
    CopyItem: TMenuItem;
    PasteItem: TMenuItem;
    ToggleBookmarksItem: TMenuItem;
    GotoBookmarksItem: TMenuItem;
    SelectallItem: TMenuItem;
    SearchMenu: TMenuItem;
    FindItem: TMenuItem;
    ReplaceItem: TMenuItem;
    N7: TMenuItem;
    GotolineItem: TMenuItem;
    ViewMenu: TMenuItem;
    ProjectManagerItem: TMenuItem;
    StatusbarItem: TMenuItem;
    ToolbarsItem: TMenuItem;
    ToolMainItem: TMenuItem;
    ToolCompileandRunItem: TMenuItem;
    ToolProjectItem: TMenuItem;
    ToolSpecialsItem: TMenuItem;
    ProjectMenu: TMenuItem;
    NewunitinprojectItem: TMenuItem;
    AddtoprojectItem: TMenuItem;
    RemovefromprojectItem: TMenuItem;
    N6: TMenuItem;
    ProjectoptionsItem: TMenuItem;
    ExecuteMenu: TMenuItem;
    CompileItem: TMenuItem;
    RunItem: TMenuItem;
    N10: TMenuItem;
    CompileandRunItem: TMenuItem;
    RebuildallItem: TMenuItem;
    N8: TMenuItem;
    CompileroptionsItem: TMenuItem;
    EnvironmentoptionsItem: TMenuItem;
    ToolsMenu: TMenuItem;
    ConfiguretoolsItem: TMenuItem;
    WindowMenu: TMenuItem;
    CloseAllItem: TMenuItem;
    N28: TMenuItem;
    FullscreenmodeItem: TMenuItem;
    N36: TMenuItem;
    NextItem: TMenuItem;
    PreviousItem: TMenuItem;
    N32: TMenuItem;
    HelpMenu: TMenuItem;
    AboutDevCppItem: TMenuItem;
    MessageControl: TPageControl;
    CompSheet: TTabSheet;
    ResSheet: TTabSheet;
    ResourceOutput: TListView;
    LogSheet: TTabSheet;
    ToolbarDock: TControlBar;
    tbMain: TToolBar;
    OpenBtn: TToolButton;
    tbCompile: TToolBar;
    CleanItem: TMenuItem;
    NewFileBtn: TToolButton;
    SaveBtn: TToolButton;
    CloseBtn: TToolButton;
    ToolButton7: TToolButton;
    PrintBtn: TToolButton;
    CompileBtn: TToolButton;
    RunBtn: TToolButton;
    CompileAndRunBtn: TToolButton;
    DebugBtn: TToolButton;
    RebuildAllBtn: TToolButton;
    tbProject: TToolBar;
    AddToProjectBtn: TToolButton;
    RemoveFromProjectBtn: TToolButton;
    ToolButton20: TToolButton;
    ProjectOptionsBtn: TToolButton;
    CloseSheet: TTabSheet;
    SaveAllBtn: TToolButton;
    SplitterLeft: TSplitter;
    EditorPopup: TPopupMenu;
    UndoPopItem: TMenuItem;
    RedoPopItem: TMenuItem;
    MenuItem1: TMenuItem;
    CutPopItem: TMenuItem;
    CopyPopItem: TMenuItem;
    PastePopItem: TMenuItem;
    MenuItem2: TMenuItem;
    InsertPopItem: TMenuItem;
    TogglebookmarksPopItem: TMenuItem;
    GotobookmarksPopItem: TMenuItem;
    SelectAllPopItem: TMenuItem;
    UnitPopup: TPopupMenu;
    RemoveFilefromprojectPopItem: TMenuItem;
    RenamefilePopItem: TMenuItem;
    N30: TMenuItem;
    ClosefilePopItem: TMenuItem;
    ProjectPopup: TPopupMenu;
    NewunitinprojectPopItem: TMenuItem;
    AddtoprojectPopItem: TMenuItem;
    RemovefromprojectPopItem: TMenuItem;
    MenuItem18: TMenuItem;
    ProjectoptionsPopItem: TMenuItem;
    InfoGroupBox: TPanel;
    Statusbar: TStatusbar;
    FindSheet: TTabSheet;
    FindOutput: TListView;
    FindinallfilesItem: TMenuItem;
    N20: TMenuItem;
    mnuNew: TMenuItem;
    N13: TMenuItem;
    ActionList: TActionList;
    actNewSource: TAction;
    actNewProject: TAction;
    actNewTemplate: TAction;
    actOpen: TAction;
    actHistoryClear: TAction;
    actSave: TAction;
    actSaveAs: TAction;
    actSaveAll: TAction;
    actClose: TAction;
    actCloseAll: TAction;
    actCloseProject: TAction;
    actExportHTML: TAction;
    actExportRTF: TAction;
    actExportProject: TAction;
    actPrint: TAction;
    actPrintSU: TAction;
    actExit: TAction;
    actUndo: TAction;
    actRedo: TAction;
    actCut: TAction;
    actCopy: TAction;
    actPaste: TAction;
    actSelectAll: TAction;
    actFind: TAction;
    actFindAll: TAction;
    actReplace: TAction;
    actGotoLine: TAction;
    actProjectManager: TAction;
    actStatusbar: TAction;
    actProjectNew: TAction;
    actProjectAdd: TAction;
    actProjectRemove: TAction;
    actProjectOptions: TAction;
    actCompile: TAction;
    actRun: TAction;
    actCompRun: TAction;
    actRebuild: TAction;
    actClean: TAction;
    actDebug: TAction;
    actCompOptions: TAction;
    actEnviroOptions: TAction;
    actEditorOptions: TAction;
    actConfigTools: TAction;
    actFullScreen: TAction;
    actNext: TAction;
    actPrev: TAction;
    actAbout: TAction;
    actProjectSource: TAction;
    actUnitRemove: TAction;
    actUnitRename: TAction;
    actUnitHeader: TAction;
    actUnitOpen: TAction;
    actUnitClose: TAction;
    EditorOptionsItem: TMenuItem;
    tbEdit: TToolBar;
    UndoBtn: TToolButton;
    RedoBtn: TToolButton;
    tbSearch: TToolBar;
    FindBtn: TToolButton;
    ReplaceBtn: TToolButton;
    FindNextBtn: TToolButton;
    GotoLineBtn: TToolButton;
    OpenPopItem: TMenuItem;
    ToolEditItem: TMenuItem;
    ToolSearchItem: TMenuItem;
    N2: TMenuItem;
    N9: TMenuItem;
    tbSpecials: TToolBar;
    actProjectMakeFile: TAction;
    MessagePopup: TPopupMenu;
    MsgCopyItem: TMenuItem;
    MsgCopyAllItem: TMenuItem;
    MsgSaveAllItem: TMenuItem;
    MsgClearItem: TMenuItem;
    actBreakPoint: TAction;
    actIncremental: TAction;
    IncrementalSearch1: TMenuItem;
    actShowBars: TAction;
    EditorPageControlLeft: TPageControl;
    Close1: TMenuItem;
    N16: TMenuItem;
    DebugSheet: TTabSheet;
    actAddWatch: TAction;
    actEditWatch: TAction;
    actNextLine: TAction;
    actStepOver: TAction;
    actWatchItem: TAction;
    actRemoveWatch: TAction;
    actStopExecute: TAction;
    InsertBtn: TToolButton;
    ToggleBtn: TToolButton;
    GotoBtn: TToolButton;
    CppParser: TCppParser;
    CodeCompletion: TCodeCompletion;
    N22: TMenuItem;
    Swapheadersource1: TMenuItem;
    N23: TMenuItem;
    actSwapHeaderSource: TAction;
    InsertItem: TMenuItem;
    SyntaxCheckItem: TMenuItem;
    actSyntaxCheck: TAction;
    Shortcuts: TdevShortcuts;
    actConfigdevShortcuts: TAction;
    ConfiguredevShortcuts1: TMenuItem;
    DateTimeMenuItem: TMenuItem;
    N25: TMenuItem;
    Programreset1: TMenuItem;
    CommentheaderMenuItem: TMenuItem;
    actComment: TAction;
    actUncomment: TAction;
    actIndent: TAction;
    actUnindent: TAction;
    N26: TMenuItem;
    Comment1: TMenuItem;
    Uncomment1: TMenuItem;
    Indent1: TMenuItem;
    Unindent1: TMenuItem;
    N27: TMenuItem;
    actGotoFunction: TAction;
    Gotofunction1: TMenuItem;
    BrowserPopup: TPopupMenu;
    mnuBrowserGotoDecl: TMenuItem;
    mnuBrowserGotoImpl: TMenuItem;
    mnuBrowserSep1: TMenuItem;
    mnuBrowserNewClass: TMenuItem;
    mnuBrowserNewMember: TMenuItem;
    mnuBrowserNewVariable: TMenuItem;
    mnuBrowserViewMode: TMenuItem;
    mnuBrowserViewAll: TMenuItem;
    mnuBrowserViewCurrent: TMenuItem;
    actBrowserGotoDeclaration: TAction;
    actBrowserGotoDefinition: TAction;
    actBrowserNewClass: TAction;
    actBrowserNewMember: TAction;
    actBrowserNewVar: TAction;
    actBrowserViewAll: TAction;
    actBrowserViewCurrent: TAction;
    actProfile: TAction;
    N29: TMenuItem;
    Profileanalysis1: TMenuItem;
    N24: TMenuItem;
    N31: TMenuItem;
    actBrowserAddFolder: TAction;
    actBrowserRemoveFolder: TAction;
    actBrowserRenameFolder: TAction;
    actCloseAllButThis: TAction;
    CloseAll1: TMenuItem;
    Closeallexceptthis1: TMenuItem;
    CloseAll2: TMenuItem;
    actStepLine: TAction;
    DebugPopup: TPopupMenu;
    AddwatchPop: TMenuItem;
    RemoveWatchPop: TMenuItem;
    FileMonitor: TdevFileMonitor;
    actFileProperties: TAction;
    N35: TMenuItem;
    N1: TMenuItem;
    Properties1: TMenuItem;
    actViewToDoList: TAction;
    actAddToDo: TAction;
    AddToDoitem1: TMenuItem;
    N38: TMenuItem;
    oDolist1: TMenuItem;
    N39: TMenuItem;
    actProjectNewFolder: TAction;
    actProjectRemoveFolder: TAction;
    actProjectRenameFolder: TAction;
    Newfolder1: TMenuItem;
    N40: TMenuItem;
    actImportMSVC: TAction;
    ImportItem: TMenuItem;
    N41: TMenuItem;
    ToggleBreakpointPopupItem: TMenuItem;
    AddWatchPopupItem: TMenuItem;
    actViewCPU: TAction;
    actExecParams: TAction;
    mnuExecParameters: TMenuItem;
    DevCppDDEServer: TDdeServerConv;
    actShowTips: TAction;
    ShowTipsItem: TMenuItem;
    N42: TMenuItem;
    HelpMenuItem: TMenuItem;
    actBrowserViewProject: TAction;
    mnuBrowserViewProject: TMenuItem;
    N43: TMenuItem;
    PackageManagerItem: TMenuItem;
    btnAbortCompilation: TSpeedButton;
    actAbortCompilation: TAction;
    N45: TMenuItem;
    N48: TMenuItem;
    ListItem: TMenuItem;
    mnuFileProps: TMenuItem;
    mnuUnitProperties: TMenuItem;
    N63: TMenuItem;
    actBrowserShowInherited: TAction;
    Showinheritedmembers1: TMenuItem;
    LeftPageControl: TPageControl;
    LeftProjectSheet: TTabSheet;
    ProjectView: TTreeView;
    LeftClassSheet: TTabSheet;
    ClassBrowser: TClassBrowser;
    AddWatchBtn: TButton;
    FloatingPojectManagerItem: TMenuItem;
    actSaveProjectAs: TAction;
    SaveprojectasItem: TMenuItem;
    mnuOpenWith: TMenuItem;
    tbClasses: TToolBar;
    cmbClasses: TComboBox;
    cmbMembers: TComboBox;
    cmbCompilers: TComboBox;
    N17: TMenuItem;
    ToolClassesItem: TMenuItem;
    LeftDebugSheet: TTabSheet;
    DebugView: TTreeView;
    StepOverBtn: TButton;
    DebugStartPanel: TPanel;
    DDebugBtn: TSpeedButton;
    StopExecBtn: TSpeedButton;
    N67: TMenuItem;
    FloatingReportwindowItem: TMenuItem;
    actAttachProcess: TAction;
    ModifyWatchPop: TMenuItem;
    actModifyWatch: TAction;
    ClearallWatchPop: TMenuItem;
    CompilerOutput: TListView;
    N5: TMenuItem;
    NewClassItem: TMenuItem;
    DeleteProfilingInformation: TMenuItem;
    actDeleteProfile: TAction;
    GotoDefineEditor: TMenuItem;
    GotoDeclEditor: TMenuItem;
    N15: TMenuItem;
    actGotoDeclEditor: TAction;
    actGotoImplEditor: TAction;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ProfileBtn: TToolButton;
    ProfilingInforBtn: TToolButton;
    CompResGroupBox: TPanel;
    LogOutput: TMemo;
    N64: TMenuItem;
    CollapseAll: TMenuItem;
    UncollapseAll: TMenuItem;
    actCollapse: TAction;
    actUnCollapse: TAction;
    actInsert: TAction;
    actToggle: TAction;
    actGoto: TAction;
    TEXItem: TMenuItem;
    actExportTex: TAction;
    NextLineBtn: TButton;
    IntoLineBtn: TButton;
    lblSendCommandGdb: TLabel;
    edGdbCommand: TComboBox;
    DebugOutput: TMemo;
    DebugSendPanel: TPanel;
    ViewCPUBtn: TButton;
    EvaluateInput: TComboBox;
    lblEvaluate: TLabel;
    EvalOutput: TMemo;
    SkipFuncBtn: TButton;
    actSkipFunction: TAction;
    IntoInsBtn: TButton;
    actNextIns: TAction;
    NextInsBtn: TButton;
    actStepIns: TAction;
    MsgPasteItem: TMenuItem;
    actMsgCopy: TAction;
    actMsgCopyAll: TAction;
    actMsgPaste: TAction;
    actMsgClear: TAction;
    actMsgSaveAll: TAction;
    actReplaceAll: TAction;
    ReplaceAll1: TMenuItem;
    N72: TMenuItem;
    actMsgCut: TAction;
    actMsgCut1: TMenuItem;
    N71: TMenuItem;
    N73: TMenuItem;
    N74: TMenuItem;
    MsgSellAllItem: TMenuItem;
    actMsgSelAll: TAction;
    actNewClass: TAction;
    actSearchAgain: TAction;
    actSearchAgain1: TMenuItem;
    N75: TMenuItem;
    ToolButton3: TToolButton;
    SplitterBottom: TSplitter;
    N76: TMenuItem;
    N12: TMenuItem;
    Abortcompilation1: TMenuItem;
    oggleBreakpoint1: TMenuItem;
    N18: TMenuItem;
    StopBtn: TToolButton;
    ToolButton5: TToolButton;
    actRevSearchAgain: TAction;
    SearchAgainBackwards1: TMenuItem;
    actDeleteLine: TAction;
    pbCompilation: TProgressBar;
    N21: TMenuItem;
    actToggleComment: TAction;
    ToggleComment1: TMenuItem;
    ToolCompilersItem: TMenuItem;
    tbCompilers: TToolBar;
    actDuplicateLine: TAction;
    N37: TMenuItem;
    DuplicateLine1: TMenuItem;
    DeleteLine1: TMenuItem;
    CppPreprocessor: TCppPreprocessor;
    CppTokenizer: TCppTokenizer;
    actBrowserViewIncludes: TAction;
    mnuBrowserViewInclude: TMenuItem;
    actMoveSelUp: TAction;
    actMoveSelDown: TAction;
    N68: TMenuItem;
    actMoveSelUp1: TMenuItem;
    actMoveSelDown1: TMenuItem;
    actCodeCompletion: TAction;
    actPackageCheck: TAction;
    actPackageManager: TAction;
    actHelp: TAction;
    FolderPopup: TPopupMenu;
    Addfolder2: TMenuItem;
    Renamefolder2: TMenuItem;
    Removefolder2: TMenuItem;
    N44: TMenuItem;
    Addfile1: TMenuItem;
    N49: TMenuItem;
    N69: TMenuItem;
    CloseProject1: TMenuItem;
    SourceFile1: TMenuItem;
    chkShortenPaths: TCheckBox;
    actShortenCompPaths: TAction;
    actSyntaxCheckFile: TAction;
    SyntaxCheckCurrentFile1: TMenuItem;
    PageControlPanel: TPanel;
    EditorPageControlRight: TPageControl;
    EditorPageControlSplitter: TSplitter;
    actSwapEditor: TAction;
    N57: TMenuItem;
    Movetootherview1: TMenuItem;
    CloseAllBtn: TToolButton;
    N14: TMenuItem;
    MoveToOtherViewItem: TMenuItem;
    SwapHeaderSourceItem: TMenuItem;
    actOpenFolder: TAction;
    N77: TMenuItem;
    actOpenFolder1: TMenuItem;
    actGotoBreakPoint: TAction;
    Abortcompilation2: TMenuItem;
    actToggleCommentInline: TAction;
    actCommentInlineSel1: TMenuItem;
    FormatMenu: TMenuItem;
    actFormatCurrentFile: TAction;
    FormatCurrentFile1: TMenuItem;
    actFormatOptions: TAction;
    actFormatOptions1: TMenuItem;
    N46: TMenuItem;
    actRunTests: TAction;
    DonateItem: TMenuItem;
    actDonate: TAction;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure ToggleBookmarkClick(Sender: TObject);
    procedure GotoBookmarkClick(Sender: TObject);
    procedure MessageControlChange(Sender: TObject);
    procedure ProjectViewContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure ToolbarDockClick(Sender: TObject);
    procedure ToolbarDockContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure SplitterBottomMoved(Sender: TObject);
    procedure actNewSourceExecute(Sender: TObject);
    procedure actNewProjectExecute(Sender: TObject);
    procedure actNewTemplateExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actHistoryClearExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actSaveAsExecute(Sender: TObject);
    procedure actSaveAllExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure actCloseAllExecute(Sender: TObject);
    procedure actCloseProjectExecute(Sender: TObject);
    procedure actExportHTMLExecute(Sender: TObject);
    procedure actExportRTFExecute(Sender: TObject);
    procedure actExportTexExecute(Sender: TObject);
    procedure actExportProjectExecute(Sender: TObject);
    procedure actPrintExecute(Sender: TObject);
    procedure actPrintSUExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actUndoExecute(Sender: TObject);
    procedure actRedoExecute(Sender: TObject);
    procedure actCutExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
    procedure actProjectManagerExecute(Sender: TObject);
    procedure actStatusBarExecute(Sender: TObject);
    procedure actFullScreenExecute(Sender: TObject);
    procedure actNextExecute(Sender: TObject);
    procedure actPrevExecute(Sender: TObject);
    procedure actCompOptionsExecute(Sender: TObject);
    procedure actEditorOptionsExecute(Sender: TObject);
    procedure actConfigToolsExecute(Sender: TObject);
    procedure actUnitRemoveExecute(Sender: TObject);
    procedure actUnitRenameExecute(Sender: TObject);
    procedure actUnitOpenExecute(Sender: TObject);
    procedure actUnitCloseExecute(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure actProjectNewExecute(Sender: TObject);
    procedure actProjectAddExecute(Sender: TObject);
    procedure actProjectRemoveExecute(Sender: TObject);
    procedure actProjectOptionsExecute(Sender: TObject);
    procedure actProjectSourceExecute(Sender: TObject);
    procedure actFindExecute(Sender: TObject);
    procedure actFindAllExecute(Sender: TObject);
    procedure actReplaceExecute(Sender: TObject);
    procedure actGotoLineExecute(Sender: TObject);
    procedure actCompileExecute(Sender: TObject);
    procedure actRunExecute(Sender: TObject);
    procedure actCompRunExecute(Sender: TObject);
    procedure actRebuildExecute(Sender: TObject);
    procedure actCleanExecute(Sender: TObject);
    procedure actDebugExecute(Sender: TObject);
    procedure actEnviroOptionsExecute(Sender: TObject);
    procedure actProjectMakeFileExecute(Sender: TObject);
    procedure actMsgCopyExecute(Sender: TObject);
    procedure actMsgClearExecute(Sender: TObject);
    procedure actMsgHideExecute(Sender: TObject);
    procedure actUpdatePageCount(Sender: TObject); // enable on pagecount> 0
    procedure actUpdateProject(Sender: TObject); // enable on fproject assigned
    procedure actUpdateMakeFile(Sender: TObject); // enable on fproject assigned and compilation allowed
    procedure actUpdatePageorProject(Sender: TObject); // enable on either of above
    procedure actUpdateEmptyEditor(Sender: TObject); // enable on unempty editor
    procedure actUpdateDebuggerRunning(Sender: TObject); // enable when debugger running
    procedure actUpdateDeleteWatch(Sender: TObject); // enable on watch var selection
    procedure actIncrementalExecute(Sender: TObject);
    procedure CompilerOutputDblClick(Sender: TObject);
    procedure FindOutputDblClick(Sender: TObject);
    procedure actShowBarsExecute(Sender: TObject);
    procedure btnFullScrRevertClick(Sender: TObject);
    procedure FormContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure actAddWatchExecute(Sender: TObject);
    procedure ProjectViewClick(Sender: TObject);
    procedure actNextLineExecute(Sender: TObject);
    procedure actRemoveWatchExecute(Sender: TObject);
    procedure actStepOverExecute(Sender: TObject);
    procedure actStopExecuteExecute(Sender: TObject);
    procedure actUndoUpdate(Sender: TObject);
    procedure actRedoUpdate(Sender: TObject);
    procedure actCutUpdate(Sender: TObject);
    procedure actCopyUpdate(Sender: TObject);
    procedure actPasteUpdate(Sender: TObject);
    procedure actSaveUpdate(Sender: TObject);
    procedure actSaveAsUpdate(Sender: TObject);
    procedure actFileMenuExecute(Sender: TObject);
    procedure actToolsMenuExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure ClassBrowserSelect(Sender: TObject; Filename: TFileName; Line: Integer);
    procedure CppParserTotalProgress(Sender: TObject; const FileName: string; Total, Current: Integer);
    procedure CodeCompletionResize(Sender: TObject);
    procedure actSwapHeaderSourceExecute(Sender: TObject);
    procedure actSyntaxCheckExecute(Sender: TObject);
    procedure EditorPageControlChange(Sender: TObject);
    procedure actConfigdevShortcutsExecute(Sender: TObject);
    procedure DateTimeMenuItemClick(Sender: TObject);
    procedure CommentheaderMenuItemClick(Sender: TObject);
    procedure EditorPageControlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure actNewTemplateUpdate(Sender: TObject);
    procedure actCommentExecute(Sender: TObject);
    procedure actUncommentExecute(Sender: TObject);
    procedure actIndentExecute(Sender: TObject);
    procedure actUnindentExecute(Sender: TObject);
    procedure EditorPageControlDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure EditorPageControlDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept:
      Boolean);
    procedure actGotoFunctionExecute(Sender: TObject);
    procedure actBrowserGotoDeclarationUpdate(Sender: TObject);
    procedure actBrowserGotoDefinitionUpdate(Sender: TObject);
    procedure actBrowserGotoDeclarationExecute(Sender: TObject);
    procedure actBrowserGotoDefinitionExecute(Sender: TObject);
    procedure actBrowserNewClassUpdate(Sender: TObject);
    procedure actBrowserNewMemberUpdate(Sender: TObject);
    procedure actBrowserNewVarUpdate(Sender: TObject);
    procedure actBrowserViewAllUpdate(Sender: TObject);
    procedure actBrowserNewClassExecute(Sender: TObject);
    procedure actBrowserNewMemberExecute(Sender: TObject);
    procedure actBrowserNewVarExecute(Sender: TObject);
    procedure actBrowserViewAllExecute(Sender: TObject);
    procedure actBrowserViewCurrentExecute(Sender: TObject);
    procedure actProfileExecute(Sender: TObject);
    procedure actCloseAllButThisExecute(Sender: TObject);
    procedure actStepLineExecute(Sender: TObject);
    procedure lvBacktraceCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var
      DefaultDraw: Boolean);
    procedure lvBacktraceMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure actRunUpdate(Sender: TObject);
    procedure actCompileRunUpdate(Sender: TObject);
    procedure actCompileUpdate(Sender: TObject);
    procedure FileMonitorNotifyChange(Sender: TObject; ChangeType: TdevMonitorChangeType; Filename: String);
    procedure actFilePropertiesExecute(Sender: TObject);
    procedure actViewToDoListExecute(Sender: TObject);
    procedure actAddToDoExecute(Sender: TObject);
    procedure actProjectNewFolderExecute(Sender: TObject);
    procedure actProjectRemoveFolderExecute(Sender: TObject);
    procedure actProjectRenameFolderExecute(Sender: TObject);
    procedure ProjectViewDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure ProjectViewDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure actImportMSVCExecute(Sender: TObject);
    procedure ViewCPUItemClick(Sender: TObject);
    procedure edGdbCommandKeyPress(Sender: TObject; var Key: Char);
    procedure actExecParamsExecute(Sender: TObject);
    procedure DevCppDDEServerExecuteMacro(Sender: TObject; Msg: TStrings);
    procedure actShowTipsExecute(Sender: TObject);
    procedure CppParserStartParsing(Sender: TObject);
    procedure CppParserEndParsing(Sender: TObject; Total: Integer);
    procedure actBrowserViewProjectExecute(Sender: TObject);
    procedure actAbortCompilationUpdate(Sender: TObject);
    procedure actAbortCompilationExecute(Sender: TObject);
    procedure ProjectViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure RemoveItem(Node: TTreeNode);
    procedure ProjectViewChanging(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
    procedure actOpenEditorByTagExecute(Sender: TObject);
    procedure actWindowMenuExecute(Sender: TObject);
    procedure actGotoProjectManagerExecute(Sender: TObject);
    procedure ListItemClick(Sender: TObject);
    procedure ProjectViewCompare(Sender: TObject; Node1, Node2: TTreeNode; Data: Integer; var Compare: Integer);
    procedure ProjectViewKeyPress(Sender: TObject; var Key: Char);
    procedure ProjectViewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GoToClassBrowserItemClick(Sender: TObject);
    procedure actBrowserShowInheritedExecute(Sender: TObject);
    procedure ReportWindowClose(Sender: TObject; var Action: TCloseAction);
    procedure FloatingPojectManagerItemClick(Sender: TObject);
    procedure actSaveProjectAsExecute(Sender: TObject);
    procedure mnuOpenWithClick(Sender: TObject);
    procedure cmbClassesChange(Sender: TObject);
    procedure cmbMembersChange(Sender: TObject);
    procedure CompilerOutputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FindOutputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DebugViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DebugPopupPopup(Sender: TObject);
    procedure FloatingReportwindowItemClick(Sender: TObject);
    procedure actAttachProcessUpdate(Sender: TObject);
    procedure actAttachProcessExecute(Sender: TObject);
    procedure actModifyWatchExecute(Sender: TObject);
    procedure actModifyWatchUpdate(Sender: TObject);
    procedure ClearallWatchPopClick(Sender: TObject);
    procedure actMsgCopyAllExecute(Sender: TObject);
    procedure actMsgSaveAllExecute(Sender: TObject);
    procedure actDeleteProfileExecute(Sender: TObject);
    procedure actGotoImplDeclEditorExecute(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled:
      Boolean);
    procedure ImportCBCprojectClick(Sender: TObject);
    procedure CompilerOutputAdvancedCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
      Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure cmbGenericDropDown(Sender: TObject);
    procedure NewFileBtnClick(Sender: TObject);
    procedure actUnCollapseExecute(Sender: TObject);
    procedure actCollapseExecute(Sender: TObject);
    procedure actInsertExecute(Sender: TObject);
    procedure actToggleExecute(Sender: TObject);
    procedure actGotoExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EditorPageControlMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure actBreakPointExecute(Sender: TObject);
    procedure EvaluateInputKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure actSkipFunctionExecute(Sender: TObject);
    procedure actNextInsExecute(Sender: TObject);
    procedure actStepInsExecute(Sender: TObject);
    procedure actMsgPasteExecute(Sender: TObject);
    procedure actUpdateDebuggerRunningCPU(Sender: TObject);
    procedure actUpdateEmptyEditorFindForm(Sender: TObject);
    procedure actReplaceAllExecute(Sender: TObject);
    procedure DebugViewAdvancedCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; Stage:
      TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
    procedure FindOutputAdvancedCustomDrawSubItem(Sender: TCustomListView; Item: TListItem; SubItem: Integer; State:
      TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure actMsgCutExecute(Sender: TObject);
    procedure FindOutputAdvancedCustomDraw(Sender: TCustomListView; const ARect: TRect; Stage: TCustomDrawStage; var
      DefaultDraw: Boolean);
    procedure CompilerOutputAdvancedCustomDraw(Sender: TCustomListView; const ARect: TRect; Stage: TCustomDrawStage; var
      DefaultDraw: Boolean);
    procedure actMsgSelAllExecute(Sender: TObject);
    procedure actSearchAgainExecute(Sender: TObject);
    procedure FindOutputDeletion(Sender: TObject; Item: TListItem);
    procedure CompilerOutputDeletion(Sender: TObject; Item: TListItem);
    procedure ResourceOutputDeletion(Sender: TObject; Item: TListItem);
    procedure actStopExecuteUpdate(Sender: TObject);
    procedure actUpdateIndent(Sender: TObject);
    procedure actRevSearchAgainExecute(Sender: TObject);
    procedure actDeleteLineExecute(Sender: TObject);
    procedure actToggleCommentExecute(Sender: TObject);
    procedure cmbCompilersChange(Sender: TObject);
    procedure actDuplicateLineExecute(Sender: TObject);
    procedure actBrowserViewIncludesExecute(Sender: TObject);
    procedure actMoveSelUpExecute(Sender: TObject);
    procedure actMoveSelDownExecute(Sender: TObject);
    procedure actCodeCompletionUpdate(Sender: TObject);
    procedure actCodeCompletionExecute(Sender: TObject);
    procedure actPackageManagerExecute(Sender: TObject);
    procedure actHelpExecute(Sender: TObject);
    procedure actShortenCompPathsExecute(Sender: TObject);
    procedure actSyntaxCheckFileExecute(Sender: TObject);
    procedure LeftPageControlChange(Sender: TObject);
    procedure actSwapEditorExecute(Sender: TObject);
    procedure actSwapEditorUpdate(Sender: TObject);
    procedure actSaveAllUpdate(Sender: TObject);
    procedure PageControlPanelResize(Sender: TObject);
    procedure actOpenFolderExecute(Sender: TObject);
    procedure actGotoBreakPointExecute(Sender: TObject);
    procedure actToggleCommentInlineExecute(Sender: TObject);
    procedure actToggleCommentInlineUpdate(Sender: TObject);
    procedure actFormatCurrentFileExecute(Sender: TObject);
    procedure actFormatOptionsExecute(Sender: TObject);
    procedure FindOutputSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure actRunTestsExecute(Sender: TObject);
    procedure WMCopyData(var Message: TMessage); message WM_COPYDATA;
    procedure actDonateExecute(Sender: TObject);
  private
    fPreviousHeight: integer; // stores MessageControl height to be able to restore to previous height
    fTools: TToolController; // tool list controller
    fProjectToolWindow: TForm; // floating left tab control
    fReportToolWindow: TForm; // floating bottom tab control
    WindowPlacement: TWindowPlacement; // idem
    fFirstShow: boolean; // true for first WM_SHOW, false for others
    fRunEndAction: TRunEndAction; // determines what to do when program execution finishes
    fCompSuccessAction: TCompSuccessAction; // determines what to do when compilation finishes
    fParseStartTime: Cardinal; // TODO: move to CppParser?
    fOldCompilerToolbarIndex: integer;
    fAutoSaveTimer: TTimer;
    fProject: TProject;
    fDebugger: TDebugger;
    fCompiler: TCompiler;
    fEditorList: TEditorList;
    fCurrentPageHint: String;
    fLogOutputRawData: TStringList;
    fCriticalSection: TCriticalSection; // protects fFilesToOpen
    fFilesToOpen: TStringList; // files to open on show
    function ParseToolParams(s: String): String;
    procedure BuildBookMarkMenus;
    procedure SetHints;
    procedure MRUClick(Sender: TObject);
    procedure CodeInsClick(Sender: TObject);
    procedure ToolItemClick(Sender: TObject);
    procedure WMDropFiles(var msg: TMessage); message WM_DROPFILES;
    procedure LogEntryProc(const Msg: String);
    procedure CompOutputProc(const _Line, _Col, _Unit, _Message: String);
    procedure CompResOutputProc(const _Line, _Col, _Unit, _Message: String);
    procedure CompEndProc;
    procedure CompSuccessProc;
    procedure RunEndProc;
    procedure LoadText;
    procedure OpenUnit;
    function PrepareForRun(ForcedCompileTarget: TTarget = ctInvalid): Boolean;
    function PrepareForCompile(ForcedCompileTarget: TTarget = ctInvalid): Boolean;
    function PrepareForClean(ForcedCompileTarget: TTarget = ctInvalid): Boolean;
    procedure LoadTheme;
    procedure CheckForDLLProfiling;
    procedure ProjectWindowClose(Sender: TObject; var Action: TCloseAction);
    procedure BuildOpenWith;
    procedure RebuildClassesToolbar;
    procedure PrepareDebugger;
    procedure ClearCompileMessages;
    procedure ClearMessageControl;
    procedure UpdateClassBrowsing;
    function ParseParameters(const Parameters: WideString): Integer;
  public
    procedure ScanActiveProject;
    procedure UpdateCompilerList;
    function GetCompileTarget: TTarget;
    procedure UpdateAppTitle;
    procedure OpenCloseMessageSheet(Open: boolean);
    procedure OpenFile(const FileName: String);
    procedure OpenFileList(List: TStringList);
    procedure OpenProject(const s: String);
    procedure GotoBreakpoint(const FileName: String; Line: integer);
    procedure RemoveActiveBreakpoints;
    procedure AddFindOutputItem(const line, col, filename, msg, keyword: String);
    procedure EditorSaveTimer(sender: TObject);
    procedure OnInputEvalReady(const evalvalue: String);
    procedure SetStatusbarLineCol;
    procedure SetStatusbarMessage(const msg: String);

    // Hide variables
    property AutoSaveTimer: TTimer read fAutoSaveTimer write fAutoSaveTimer;
    property Project: TProject read fProject write fProject;
    property Debugger: TDebugger read fDebugger write fDebugger;
    property EditorList: TEditorList read fEditorList write fEditorList;
    property CurrentPageHint: String read fCurrentPageHint write fCurrentPageHint;
  end;

var
  MainForm: TMainForm;

implementation

uses
  ShellAPI, IniFiles, Clipbrd, MultiLangSupport, version,
  DataFrm, NewProjectFrm, AboutFrm, PrintFrm,
  CompOptionsFrm, EditorOptFrm, IncrementalFrm, EnviroFrm,
  SynEdit, Math, ImageTheme, SynEditKeyCmds, Instances,
  Types, FindFrm, ProjectTypes, devExec, Tests,
  NewTemplateFrm, FunctionSearchFrm, NewFunctionFrm, NewVarFrm, NewClassFrm,
  ProfileAnalysisFrm, FilePropertiesFrm, AddToDoFrm, ViewToDoFrm,
  ImportMSVCFrm, ImportCBFrm, CPUFrm, FileAssocs, TipOfTheDayFrm,
  WindowListFrm, RemoveUnitFrm, ParamsFrm, ProcessListFrm, SynEditHighlighter;

{$R *.dfm}

procedure TMainForm.LoadTheme;
begin
  if devImageThemes.IndexOf(devData.Theme) = -1 then
    devData.Theme := devImageThemes.Themes[0].Title; // 0 = New look (see ImageTheme.pas)

  // make sure the theme in question is in the list
  if devImageThemes.IndexOf(devData.Theme) <> -1 then begin
    devImageThemes.ActivateTheme(devData.Theme);

    with devImageThemes do begin
      // Misc items images
      ActionList.Images := CurrentTheme.MenuImages;
      MainMenu.Images := CurrentTheme.MenuImages;
      MessageControl.Images := CurrentTheme.MenuImages;

      // Set toolbar images
      tbMain.Images := CurrentTheme.MenuImages;
      tbCompile.Images := CurrentTheme.MenuImages;
      tbProject.Images := CurrentTheme.MenuImages;
      tbClasses.Images := CurrentTheme.MenuImages;
      tbedit.Images := CurrentTheme.MenuImages;
      tbSearch.Images := CurrentTheme.MenuImages;
      tbSpecials.Images := CurrentTheme.MenuImages;
      tbCompilers.Images := CurrentTheme.MenuImages;

      // Set left control images
      ProjectView.Images := CurrentTheme.ProjectImages;
      ClassBrowser.Images := CurrentTheme.BrowserImages;
      DebugView.Images := CurrentTheme.MenuImages;

      // Set left control and editor popup images
      ProjectPopup.Images := CurrentTheme.MenuImages;
      UnitPopup.Images := CurrentTheme.MenuImages;
      FolderPopup.Images := CurrentTheme.MenuImages;
      BrowserPopup.Images := CurrentTheme.MenuImages;
      DebugPopup.Images := CurrentTheme.MenuImages;
      EditorPopup.Images := CurrentTheme.MenuImages;
    end;
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Try to close the current project. If it stays open (user says cancel), stop quitting
  if Assigned(fProject) then
    actCloseProjectExecute(Self);
  if Assigned(fProject) then begin
    Action := caNone;
    Exit;
  end;

  // Try to close all editors. If some are left open, stop quitting
  actCloseAllExecute(Self);
  if fEditorList.PageCount > 0 then begin
    Action := caNone;
    Exit;
  end;

  // Remember toolbar placement
  devData.LeftActivePage := LeftPageControl.ActivePageIndex;
  devData.ToolbarMainX := tbMain.Left;
  devData.ToolbarMainY := tbMain.Top;
  devData.ToolbarEditX := tbEdit.Left;
  devData.ToolbarEditY := tbEdit.Top;
  devData.ToolbarCompileX := tbCompile.Left;
  devData.ToolbarCompileY := tbCompile.Top;
  devData.ToolbarProjectX := tbProject.Left;
  devData.ToolbarProjectY := tbProject.Top;
  devData.ToolbarSpecialsX := tbSpecials.Left;
  devData.ToolbarSpecialsY := tbSpecials.Top;
  devData.ToolbarSearchX := tbSearch.Left;
  devData.ToolbarSearchY := tbSearch.Top;
  devData.ToolbarClassesX := tbClasses.Left;
  devData.ToolbarClassesY := tbClasses.Top;
  devData.ToolbarCompilersX := tbCompilers.Left;
  devData.ToolbarCompilersY := tbCompilers.Top;

  // Save left page control states
  devData.ProjectWidth := LeftPageControl.Width;
  devData.OutputHeight := fPreviousHeight;
  devData.ProjectFloat := Assigned(fProjectToolWindow) and fProjectToolWindow.Visible;
  devData.MessageFloat := Assigned(fReportToolWindow) and fReportToolWindow.Visible;

  // Remember window placement
  devData.WindowState.GetPlacement(Self.Handle);
  if Assigned(fProjectToolWindow) then
    devData.ProjectWindowState.GetPlacement(fProjectToolWindow.Handle);
  if Assigned(fReportToolWindow) then
    devData.ReportWindowState.GetPlacement(fReportToolWindow.Handle);

  SaveOptions;

  Action := caFree;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fFilesToOpen);
  FreeAndNil(fCriticalSection);
  FreeAndNil(fLogOutputRawData);
  FreeAndNil(fTools);
  FreeAndNil(fAutoSaveTimer);
  FreeAndNil(devImageThemes);
  FreeAndNil(fEditorList);
  FreeAndNil(fCompiler);
  FreeAndNil(fDebugger);
  FreeAndNil(dmMain);
  devExecutor.Free; // sets itself to nil
  Lang.Free; // sets itself to nil
  DestroyOptions;
end;

procedure TMainForm.BuildBookMarkMenus;
var
  idx: integer;
  Text: String;
  Shortcutnumber: Word;
  GItem, TItem: TMenuItem;
begin
  Text := Lang[ID_MARKTEXT];
  ToggleBookMarksItem.Clear;
  GotoBookmarksItem.Clear;
  for idx := 1 to 9 do begin
    Shortcutnumber := Ord(inttostr(idx)[1]);

    TItem := TMenuItem.Create(ToggleBookmarksItem);
    TItem.Caption := format('%s &%d', [Text, idx]);
    TItem.OnClick := ToggleBookmarkClick;
    TItem.Tag := idx;
    TItem.ShortCut := ShortCut(Shortcutnumber, [ssCtrl]);
    ToggleBookmarksItem.Add(TItem);

    GItem := TMenuItem.Create(GotoBookmarksItem);
    GItem.Caption := TItem.Caption;
    GItem.OnClick := GotoBookmarkClick;
    GItem.Tag := idx;
    GItem.ShortCut := ShortCut(Shortcutnumber, [ssAlt]);
    GotoBookmarksItem.Add(GItem);
  end;

  CloneMenu(ToggleBookmarksItem, TogglebookmarksPopItem);
  CloneMenu(GotoBookmarksItem, GotobookmarksPopItem);
end;

procedure TMainForm.SetHints;
var
  idx: integer;
begin
  for idx := 0 to pred(ActionList.ActionCount) do
    TCustomAction(ActionList.Actions[idx]).Hint := StripHotKey(TCustomAction(ActionList.Actions[idx]).Caption);
end;

procedure TMainForm.WMDropFiles(var msg: TMessage);
var
  I, Count: integer;
  FileNameBuffer: array[0..260] of char;
  MessageHandle: THandle;
  FileList: TStringList;
begin
  try
    // Get drop information
    MessageHandle := THandle(msg.wParam);
    Count := DragQueryFile(MessageHandle, $FFFFFFFF, nil, 0);

    // Build file list
    FileList := TStringList.Create;
    try
      for I := 0 to Count - 1 do begin
        DragQueryFile(MessageHandle, I, FileNameBuffer, SizeOf(FileNameBuffer));
        FileList.Add(FileNameBuffer);
      end;
      OpenFileList(FileList);
      Application.BringToFront;
    finally
      FileList.Free;
    end;
  finally
    msg.Result := 0;
    DragFinish(THandle(msg.WParam));
  end;
end;

procedure TMainForm.LoadText;
var
  len: integer;
begin
  // Set interface font
  Font.Name := devData.InterfaceFont;
  Font.Size := devData.InterfaceFontSize;

  // Menus
  FileMenu.Caption := Lang[ID_MNU_FILE];
  EditMenu.Caption := Lang[ID_MNU_EDIT];
  SearchMenu.Caption := Lang[ID_MNU_SEARCH];
  ViewMenu.Caption := Lang[ID_MNU_VIEW];
  ProjectMenu.Caption := Lang[ID_MNU_PROJECT];
  ExecuteMenu.Caption := Lang[ID_MNU_EXECUTE];
  ToolsMenu.Caption := Lang[ID_MNU_TOOLS];
  WindowMenu.Caption := Lang[ID_MNU_WINDOW];
  HelpMenu.Caption := Lang[ID_MNU_HELP];

  // File menu
  mnuNew.Caption := Lang[ID_SUB_NEW];

  // New submenu
  NewFileBtn.Hint := Lang[ID_HINT_NEW];
  actNewSource.Caption := Lang[ID_ITEM_NEWSOURCE];
  actNewProject.Caption := Lang[ID_ITEM_NEWPROJECT];
  actNewTemplate.Caption := Lang[ID_ITEM_NEWTEMPLATE];
  actNewClass.Caption := Lang[ID_ITEM_NEWCLASS];

  // Top level
  actOpen.Caption := Lang[ID_ITEM_OPEN];
  actSave.Caption := Lang[ID_ITEM_SAVEFILE];
  actSaveAs.Caption := Lang[ID_ITEM_SAVEAS];
  actSaveProjectAs.Caption := Lang[ID_ITEM_SAVEASPROJECT];
  actSaveAll.Caption := Lang[ID_ITEM_SAVEALL];
  actClose.Caption := Lang[ID_ITEM_CLOSEFILE];
  actCloseProject.Caption := Lang[ID_ITEM_CLOSEPROJECT];
  actCloseAll.Caption := Lang[ID_ITEM_CLOSEALL];
  actFileProperties.Caption := Lang[ID_ITEM_PROPERTIES];

  // Import submenu
  ImportItem.Caption := Lang[ID_SUB_IMPORT];
  actImportMSVC.Caption := Lang[ID_MSVC_MENUITEM];

  // Exprt submenu
  ExportItem.Caption := Lang[ID_SUB_EXPORT];
  actExportHTML.Caption := Lang[ID_ITEM_EXPORTHTML];
  actExportRTF.Caption := Lang[ID_ITEM_EXPORTRTF];
  actExportTex.Caption := Lang[ID_ITEM_EXPORTTEX];
  actExportProject.Caption := Lang[ID_ITEM_EXPORTPROJECT];

  // Top level
  actPrint.Caption := Lang[ID_ITEM_PRINT];
  actPrintSU.Caption := Lang[ID_ITEM_PRINTSETUP];
  actHistoryClear.Caption := Lang[ID_ITEM_CLEARHISTORY];
  actExit.Caption := Lang[ID_ITEM_EXIT];

  actMsgCut.Caption := Lang[ID_ITEM_CUT];
  actMsgCopy.Caption := Lang[ID_ITEM_COPY];
  actMsgCopyAll.Caption := Lang[ID_ITEM_COPYALL];
  actMsgPaste.Caption := Lang[ID_ITEM_PASTE];
  actMsgSelAll.Caption := Lang[ID_ITEM_SELECTALL];
  actMsgSaveAll.Caption := Lang[ID_ITEM_SAVEALL];
  actMsgClear.Caption := Lang[ID_ITEM_CLEAR];

  // Edit menu
  actUndo.Caption := Lang[ID_ITEM_UNDO];
  actRedo.Caption := Lang[ID_ITEM_REDO];
  actCut.Caption := Lang[ID_ITEM_CUT];
  actCopy.Caption := Lang[ID_ITEM_COPY];
  actPaste.Caption := Lang[ID_ITEM_PASTE];
  actSelectAll.Caption := Lang[ID_ITEM_SELECTALL];

  // Insert submenu
  actInsert.Caption := Lang[ID_TB_INSERT];
  DateTimeMenuItem.Caption := Lang[ID_ITEM_DATETIME];
  CommentHeaderMenuItem.Caption := Lang[ID_ITEM_COMMENTHEADER];

  // Top level
  actToggle.Caption := Lang[ID_TB_TOGGLE];
  actGoto.Caption := Lang[ID_TB_GOTO];

  actComment.Caption := Lang[ID_ITEM_COMMENTSELECTION];
  actUncomment.Caption := Lang[ID_ITEM_UNCOMMENTSELECTION];
  actToggleComment.Caption := Lang[ID_ITEM_TOGGLECOMMENT];
  actToggleCommentInline.Caption := Lang[ID_ITEM_TOGGLECOMMENTINLINE];
  actIndent.Caption := Lang[ID_ITEM_INDENTSELECTION];
  actUnindent.Caption := Lang[ID_ITEM_UNINDENTSELECTION];
  actCollapse.Caption := Lang[ID_ITEM_COLLAPSEALL];
  actUnCollapse.Caption := Lang[ID_ITEM_UNCOLLAPSEALL];
  actDuplicateLine.Caption := Lang[ID_ITEM_DUPLICATELINE];
  actDeleteLine.Caption := Lang[ID_ITEM_DELETELINE];
  actMoveSelUp.Caption := Lang[ID_ITEM_MOVESELUP];
  actMoveSelDown.Caption := Lang[ID_ITEM_MOVESELDOWN];

  // Search Menu
  actFind.Caption := Lang[ID_ITEM_FIND];
  actFindAll.Caption := Lang[ID_ITEM_FINDINALL];
  actReplace.Caption := Lang[ID_ITEM_REPLACE];
  actReplaceAll.Caption := Lang[ID_ITEM_REPLACEFILES];
  actSearchAgain.Caption := Lang[ID_ITEM_FINDNEXT];
  actRevSearchAgain.Caption := Lang[ID_ITEM_FINDPREV];
  actIncremental.Caption := Lang[ID_ITEM_INCREMENTAL];
  actGotoFunction.Caption := Lang[ID_ITEM_GOTOFUNCTION];
  actGotoLine.Caption := Lang[ID_ITEM_GOTO];

  // View Menu
  actProjectManager.Caption := Lang[ID_ITEM_PROJECTVIEW];
  actStatusbar.Caption := Lang[ID_ITEM_STATUSBAR];
  actSwapHeaderSource.Caption := Lang[ID_ITEM_SWAPHEADERSOURCE];
  actSwapEditor.Caption := Lang[ID_ITEM_SWAPEDITOR];

  // Toolbar submenu
  ToolBarsItem.Caption := Lang[ID_SUB_TOOLBARS];
  ToolMainItem.Caption := Lang[ID_TOOLMAIN];
  ToolEditItem.Caption := Lang[ID_TOOLEDIT];
  ToolSearchItem.Caption := Lang[ID_TOOLSEARCH];
  ToolCompileAndRunItem.Caption := Lang[ID_TOOLCOMPRUN];
  ToolProjectItem.Caption := Lang[ID_TOOLPROJECT];
  ToolSpecialsItem.Caption := Lang[ID_TOOLSPECIAL];
  ToolClassesItem.Caption := Lang[ID_LP_CLASSES];
  ToolCompilersItem.Caption := Lang[ID_TOOLCOMPILERS];

  // Top level
  actViewToDoList.Caption := Lang[ID_VIEWTODO_MENUITEM];
  FloatingPojectManagerItem.Caption := Lang[ID_ITEM_FLOATWINDOW];
  FloatingReportWindowItem.Caption := Lang[ID_ITEM_FLOATREPORT];

  // Project menu
  actProjectNew.Caption := Lang[ID_ITEM_NEWFILE];
  actProjectAdd.Caption := Lang[ID_ITEM_ADDFILE];
  actProjectRemove.Caption := Lang[ID_ITEM_REMOVEFILE];
  actProjectOptions.Caption := Lang[ID_ITEM_PROJECTOPTIONS];
  actProjectMakeFile.Caption := Lang[ID_ITEM_EDITMAKE];

  // Execute menu
  actCompile.Caption := Lang[ID_ITEM_COMP];
  actRun.Caption := Lang[ID_ITEM_RUN];
  actCompRun.Caption := Lang[ID_ITEM_COMPRUN];
  actRebuild.Caption := Lang[ID_ITEM_REBUILD];
  actExecParams.Caption := Lang[ID_ITEM_EXECPARAMS];
  actSyntaxCheck.Caption := Lang[ID_ITEM_SYNTAXCHECK];
  actSyntaxCheckFile.Caption := Lang[ID_ITEM_SYNTAXCHECKFILE];
  actClean.Caption := Lang[ID_ITEM_CLEAN];
  actProfile.Caption := Lang[ID_ITEM_PROFILE];
  actDebug.Caption := Lang[ID_ITEM_DEBUG];
  actGotoBreakPoint.Caption := Lang[ID_ITEM_GOTOBREAKPOINT];
  actBreakPoint.Caption := Lang[ID_ITEM_TOGGLEBREAK];
  actDeleteProfile.Caption := Lang[ID_ITEM_DELPROFINFORMATION];
  actAbortCompilation.Caption := Lang[ID_ITEM_ABORTCOMP];

  // Tools menu
  actCompOptions.Caption := Lang[ID_ITEM_COMPOPTIONS];
  actEnviroOptions.Caption := Lang[ID_ITEM_ENVIROOPTIONS];
  actEditorOptions.Caption := Lang[ID_ITEM_EDITOROPTIONS];
  actConfigTools.Caption := Lang[ID_ITEM_TOOLCONFIG];
  actConfigdevShortcuts.Caption := Lang[ID_ITEM_SHORTCUTSCONFIG];
  actPackageCheck.Caption := Lang[ID_ITEM_UPDATECHECK];
  actPackageManager.Caption := Lang[ID_ITEM_PACKMAN];

  // Formatter menu
  actFormatCurrentFile.Caption := Lang[ID_FORMATTER_FORMATCURFILE];
  actFormatOptions.Caption := Lang[ID_FORMATTER_MENU];

  // Window menu
  if devData.FullScreen then
    actFullScreen.Caption := Lang[ID_ITEM_FULLSCRBACK]
  else
    actFullScreen.Caption := Lang[ID_ITEM_FULLSCRMODE];
  actNext.Caption := Lang[ID_ITEM_NEXT];
  actPrev.Caption := Lang[ID_ITEM_PREV];
  ListItem.Caption := Lang[ID_ITEM_LIST];

  // Help menu
  actHelp.Caption := Lang[ID_ITEM_HELPDEVCPP];
  actAbout.Caption := Lang[ID_ITEM_ABOUT];
  actShowTips.Caption := Lang[ID_ITEM_TIP];
  actDonate.Caption := Lang[ID_ITEM_DONATE];

  // Debugging buttons
  actAddWatch.Caption := Lang[ID_ITEM_WATCHADD];
  actEditWatch.Caption := Lang[ID_ITEM_WATCHEDIT];
  actModifyWatch.Caption := Lang[ID_ITEM_MODIFYVALUE];
  actRemoveWatch.Caption := Lang[ID_ITEM_WATCHREMOVE];
  actNextLine.Caption := Lang[ID_ITEM_STEPNEXT];
  actStepLine.Caption := Lang[ID_ITEM_STEPINTO];
  actStepOver.Caption := Lang[ID_ITEM_STEPOVER];
  actWatchItem.Caption := Lang[ID_ITEM_WATCHITEMS];
  actStopExecute.Caption := Lang[ID_ITEM_STOPEXECUTION];
  actViewCPU.Caption := Lang[ID_ITEM_CPUWINDOW];
  ClearallWatchPop.Caption := Lang[ID_ITEM_CLEARALL];
  actNextIns.Caption := Lang[ID_ITEM_NEXTINS];
  actStepIns.Caption := Lang[ID_ITEM_STEPINS];
  actSkipFunction.Caption := Lang[ID_ITEM_SKIPFUNCTION];

  // Project/Unit/Folder popup
  actUnitRemove.Caption := Lang[ID_POP_REMOVE];
  actUnitRename.Caption := Lang[ID_POP_RENAME];
  actUnitOpen.Caption := Lang[ID_POP_OPEN];
  actUnitClose.Caption := Lang[ID_POP_CLOSE];
  actProjectNewFolder.Caption := Lang[ID_POP_ADDFOLDER];
  actProjectRemoveFolder.Caption := Lang[ID_POP_REMOVEFOLDER];
  actProjectRenameFolder.Caption := Lang[ID_POP_RENAMEFOLDER];
  mnuOpenWith.Caption := Lang[ID_POP_OPENWITH];

  // Editor popup
  actGotoDeclEditor.Caption := Lang[ID_POP_GOTODECL];
  actGotoImplEditor.Caption := Lang[ID_POP_GOTOIMPL];
  actCloseAllButThis.Caption := Lang[ID_POP_CLOSEALLBUTTHIS];
  actOpenFolder.Caption := Lang[ID_POP_OPENCONTAINING];
  actAddToDo.Caption := Lang[ID_POP_ADDTODOITEM];

  // Class Browser Popup
  actBrowserGotoDeclaration.Caption := Lang[ID_POP_GOTODECL];
  actBrowserGotoDefinition.Caption := Lang[ID_POP_GOTOIMPL];
  actBrowserNewClass.Caption := Lang[ID_POP_NEWCLASS];
  actBrowserNewMember.Caption := Lang[ID_POP_NEWMEMBER];
  actBrowserNewVar.Caption := Lang[ID_POP_NEWVAR];
  mnuBrowserViewMode.Caption := Lang[ID_POP_VIEWMODE];
  actBrowserViewAll.Caption := Lang[ID_POP_VIEWALLFILES];
  actBrowserViewProject.Caption := Lang[ID_POP_VIEWPROJECT];
  actBrowserViewCurrent.Caption := Lang[ID_POP_VIEWCURRENT];
  actBrowserAddFolder.Caption := Lang[ID_POP_ADDFOLDER];
  actBrowserRemoveFolder.Caption := Lang[ID_POP_REMOVEFOLDER];
  actBrowserRenameFolder.Caption := Lang[ID_POP_RENAMEFOLDER];
  actBrowserShowInherited.Caption := Lang[ID_POP_SHOWINHERITED];

  // Message Control tabs
  CompSheet.Caption := Lang[ID_SHEET_COMP];
  ResSheet.Caption := Lang[ID_SHEET_RES];
  LogSheet.Caption := Lang[ID_SHEET_COMPLOG];
  DebugSheet.Caption := Lang[ID_SHEET_DEBUG];
  FindSheet.Caption := Lang[ID_SHEET_FIND];
  CloseSheet.Caption := Lang[ID_SHEET_CLOSE];

  // Compiler Tab
  CompilerOutput.Columns[0].Caption := Lang[ID_COL_LINE];
  CompilerOutput.Columns[1].Caption := Lang[ID_COL_COL];
  CompilerOutput.Columns[2].Caption := Lang[ID_COL_FILE];
  CompilerOutput.Columns[3].Caption := Lang[ID_COL_MSG];

  // Resource Tab
  ResourceOutput.Columns[0].Caption := Lang[ID_COL_LINE];
  ResourceOutput.Columns[1].Caption := Lang[ID_COL_COL];
  ResourceOutput.Columns[2].Caption := Lang[ID_COL_FILE];
  ResourceOutput.Columns[3].Caption := Lang[ID_COL_MSG];

  // Debug Tab
  lblSendCommandGdb.Caption := Lang[ID_DEB_SENDGDBCOMMAND];
  lblEvaluate.Caption := Lang[ID_DEB_EVALUATE];

  // Adapt UI spacing to translation text length
  len := Canvas.TextWidth(lblSendCommandGdb.Caption);
  edGdbCommand.Left := len + 10;
  edGdbCommand.Width := DebugOutput.Width - len - 6;

  len := Canvas.TextWidth(lblEvaluate.Caption);
  EvaluateInput.Left := len + 10;
  EvaluateInput.Width := EvalOutput.Width - len - 6;

  // Find Results Tab
  FindOutput.Columns[0].Caption := '';
  FindOutput.Columns[1].Caption := Lang[ID_COL_LINE];
  FindOutput.Columns[2].Caption := Lang[ID_COL_COL];
  FindOutput.Columns[3].Caption := Lang[ID_COL_FILE];
  FindOutput.Columns[4].Caption := Lang[ID_COL_MSG];

  // Left page control
  LeftProjectSheet.Caption := Lang[ID_LP_PROJECT];
  LeftClassSheet.Caption := Lang[ID_LP_CLASSES];
  LeftDebugSheet.Caption := Lang[ID_SHEET_DEBUG];

  BuildBookMarkMenus;
  SetHints;
end;

procedure TMainForm.SetStatusbarLineCol;
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then begin
    Statusbar.Panels[0].Text := Format(Lang[ID_STATUSBARPLUS],
      [e.Text.CaretY,
      e.Text.DisplayX,
        e.Text.SelLength,
        e.Text.Lines.Count,
        e.Text.Text.Length]);
  end else begin
    StatusBar.Panels.BeginUpdate;
    try
      Statusbar.Panels[0].Text := '';
      Statusbar.Panels[1].Text := '';
      StatusBar.Panels[2].Text := '';
    finally
      StatusBar.Panels.EndUpdate; // redraw once
    end;
  end;
end;

procedure TMainForm.SetStatusbarMessage(const msg: String);
begin
  Statusbar.Panels[2].Text := msg;
end;

procedure TMainForm.ToggleBookmarkClick(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  with Sender as TMenuItem do
    if Assigned(e) then begin
      Checked := not Checked;
      if (Parent = ToggleBookmarksItem) then
        TogglebookmarksPopItem.Items[Tag - 1].Checked := Checked
      else
        TogglebookmarksItem.Items[Tag - 1].Checked := Checked;
      if Checked then
        e.Text.SetBookMark(Tag, e.Text.CaretX, e.Text.CaretY)
      else
        e.Text.ClearBookMark(Tag);
    end;
end;

procedure TMainForm.GotoBookmarkClick(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if assigned(e) then
    e.Text.GotoBookMark(TMenuItem(Sender).Tag);
end;

procedure TMainForm.OpenCloseMessageSheet(Open: boolean);
begin
  if Assigned(fReportToolWindow) then
    Exit;

  // Switch between open and close
  with MessageControl do
    if Open then
      Height := fPreviousHeight // remember height
    else begin
      Height := Height - CompSheet.Height; // only show the tab captions
      ActivePageIndex := -1;
    end;
  CloseSheet.TabVisible := Open;
  SplitterBottom.Visible := Open;
  Statusbar.Top := Self.ClientHeight;
end;

procedure TMainForm.MessageControlChange(Sender: TObject);
begin
  if MessageControl.ActivePage = CloseSheet then begin
    if Assigned(fReportToolWindow) then begin
      fReportToolWindow.Close;
      MessageControl.ActivePageIndex := 0;
    end else
      OpenCloseMessageSheet(false);
  end else
    OpenCloseMessageSheet(true);
end;

procedure TMainForm.MRUClick(Sender: TObject);
var
  s: String;
begin
  s := PMRUItem(dmMain.MRU[TMenuItem(Sender).Tag])^.FileName;
  if FileExists(s) then begin
    if GetFileTyp(s) = utPrj then
      OpenProject(s)
    else
      OpenFile(s);
  end else
    MessageDlg(Format(Lang[ID_ERR_RENAMEDDELETED], [s]), mtInformation, [mbOK], 0);
end;

procedure TMainForm.CodeInsClick(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    e.InsertString(dmMain.CodeInserts[TMenuItem(Sender).Tag].Line, TRUE);
end;

procedure TMainForm.ToolItemClick(Sender: TObject);
var
  idx: integer;
begin // TODO: ask on SO
  idx := (Sender as TMenuItem).Tag;

  with fTools.ToolList[idx]^ do
    ExecuteFile(ParseToolParams(Exec), ParseToolParams(Params), ParseToolParams(WorkDir), SW_SHOW);
end;

procedure TMainForm.OpenProject(const s: String);
var
  s2: String;
begin
  if Assigned(fProject) then begin
    if fProject.Name = '' then
      s2 := fProject.FileName
    else
      s2 := fProject.Name;
    if MessageDlg(format(Lang[ID_MSG_CLOSEPROJECTPROMPT], [s2]), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      actCloseProject.Execute
    else
      exit;
  end;

  // Only update class browser once
  ClassBrowser.BeginUpdate;
  try
    fProject := TProject.Create(s, DEV_INTERNAL_OPEN);
    if fProject.FileName <> '' then begin
      dmMain.RemoveFromHistory(s);

      // if project manager isn't open then open it
      if not devData.ShowLeftPages then
        actProjectManager.Execute;

      CheckForDLLProfiling;
      UpdateAppTitle;
      UpdateCompilerList;
      ScanActiveProject;
    end else begin
      fProject.Free;
      fProject := nil;
    end;
  finally
    ClassBrowser.EndUpdate;
  end;
end;

procedure TMainForm.OpenFile(const FileName: String);
var
  e: TEditor;
begin
  // Don't bother opening duplicates
  e := fEditorList.FileIsOpen(FileName);
  if Assigned(e) then begin
    e.Activate;
    Exit;
  end;

  // Issue an error if it doesn't exist
  if not FileExists(FileName) then begin
    MessageBox(Application.Handle, PChar(Format(Lang[ID_ERR_FILENOTFOUND], [FileName])), PChar(Lang[ID_ERROR]),
      MB_ICONHAND);
    Exit;
  end;

  // Open the file in an editor
  e := fEditorList.NewEditor(FileName, False, False);
  if Assigned(fProject) then begin
    if (not SameFileName(fProject.FileName, FileName)) and (fProject.GetUnitFromString(FileName) = -1) then
      dmMain.RemoveFromHistory(FileName);
  end else begin
    dmMain.RemoveFromHistory(FileName);
  end;
  e.Activate;

  // Parse it after is has been shown so the user will not see random unpainted stuff for a while.
  if not Assigned(fProject) then
    CppParser.ParseFile(e.FileName, e.InProject, True);
end;

procedure TMainForm.OpenFileList(List: TStringList);
var
  I: integer;
begin
  if List.Count = 0 then
    Exit;

  // Check if there is a project file inside the list
  for I := 0 to List.Count - 1 do begin
    if GetFileTyp(List[I]) = utPrj then begin
      OpenProject(List[I]); // open only the first found project
      Exit;
    end;
  end;

  // Didn't find a project? Open the whole list
  ClassBrowser.BeginUpdate;
  try
    fEditorList.BeginUpdate;
    try
      for I := 0 to List.Count - 1 do
        OpenFile(List[I]); // open all files
    finally
      fEditorList.EndUpdate;
    end;
  finally
    ClassBrowser.EndUpdate;
  end;
end;

procedure TMainForm.AddFindOutputItem(const line, col, filename, msg, keyword: String);
var
  ListItem: TListItem;
begin
  ListItem := FindOutput.Items.Add;
  ListItem.Caption := '';
  ListItem.Data := Pointer(Length(keyword));
  ListItem.SubItems.Add(line);
  ListItem.SubItems.Add(col);
  ListItem.SubItems.Add(filename);
  ListItem.SubItems.Add(msg);
end;

function TMainForm.ParseToolParams(s: String): String;
resourcestring
  cEXEName = '<EXENAME>';
  cPrjName = '<PROJECTNAME>';
  cPrjFile = '<PROJECTFILE>';
  cPrjPath = '<PROJECTPATH>';
  cCurSrc = '<SOURCENAME>';
  cSrcPath = '<SOURCEPATH>';
  cDevVer = '<DEVCPPVERSION>';

  cDefault = '<DEFAULT>';
  cExecDir = '<EXECPATH>';
  cSrcList = '<SOURCESPCLIST>';
  cWordxy = '<WORDXY>';
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;

  // <DEFAULT>
  s := StringReplace(s, cDefault, devDirs.Default, [rfReplaceAll]);

  // <EXECPATH>
  s := Stringreplace(s, cExecDir, devDirs.Exec, [rfReplaceAll]);

  // <DEVCPPVERISON>
  s := StringReplace(s, cDevVer, DEVCPP_VERSION, [rfReplaceAll]);

  if assigned(fProject) then begin
    // <EXENAME>
    s := StringReplace(s, cEXEName, '"' + fProject.Executable + '"', [rfReplaceAll]);

    // <PROJECTNAME>
    s := StringReplace(s, cPrjName, fProject.Name, [rfReplaceAll]);

    // <PROJECTFILE>
    s := StringReplace(s, cPrjFile, fProject.FileName, [rfReplaceAll]);

    // <PROJECTPATH>
    s := StringReplace(s, cPrjPath, fProject.Directory, [rfReplaceAll]);

    if Assigned(e) then begin
      // <SOURCENAME>
      s := StringReplace(s, cCurSrc, e.FileName, [rfReplaceAll]);

      // <SOURCEPATH>
      s := StringReplace(s, cSrcPath, ExtractFilePath(e.FileName), [rfReplaceAll]);
    end;

    // <SOURCESPCLIST>
    s := StringReplace(s, cSrcList, fProject.ListUnitStr(' '), [rfReplaceAll]);
  end else if assigned(e) then begin
    // <EXENAME>
    s := StringReplace(s, cEXEName, '"' + ChangeFileExt(e.FileName, EXE_EXT) + '"', [rfReplaceAll]);

    // <PROJECTNAME>
    s := StringReplace(s, cPrjName, e.FileName, [rfReplaceAll]);

    // <PRJECTFILE>
    s := StringReplace(s, cPrjFile, e.FileName, [rfReplaceAll]);

    // <PROJECTPATH>
    s := StringReplace(s, cPrjPath, ExtractFilePath(e.FileName), [rfReplaceAll]);

    // <SOURCENAME>
    s := StringReplace(s, cCurSrc, e.FileName, [rfReplaceAll]);

    // <SOURCEPATH>
    s := StringReplace(s, cSrcPath, ExtractFilePath(e.FileName), [rfReplaceAll]);

    // <WORDXY>
    s := StringReplace(s, cWordXY, e.Text.WordAtCursor, [rfReplaceAll]);
  end;

  // clear unchanged macros

  if not assigned(fProject) then
    s := StringReplace(s, cSrcList, '', [rfReplaceAll]);

  if not assigned(e) then begin
    s := StringReplace(s, cCurSrc, '', [rfReplaceAll]);
    s := StringReplace(s, cWordXY, '', [rfReplaceAll]);
    // if no editor assigned return users default directory
    s := StringReplace(s, cSrcPath, devDirs.Default, [rfReplaceAll]);
  end;

  if not assigned(fProject) and not assigned(e) then begin
    s := StringReplace(s, cEXEName, '', [rfReplaceAll]);
    s := StringReplace(s, cPrjName, '', [rfReplaceAll]);
    s := StringReplace(s, cPrjFile, '', [rfReplaceAll]);
    s := StringReplace(s, cPrjPath, '', [rfReplaceAll]);
  end;

  Result := s;
end;

procedure TMainForm.CompOutputProc(const _Line, _Col, _Unit, _Message: String);
begin
  with CompilerOutput.Items.Add do begin
    Caption := _Line;
    SubItems.Add(_Col);
    SubItems.Add(GetRealPath(_Unit));
    SubItems.Add(_Message);
  end;

  // Update tab caption
  CompSheet.Caption := Lang[ID_SHEET_COMP] + ' (' + IntToStr(CompilerOutput.Items.Count) + ')'
end;

procedure TMainForm.CompResOutputProc(const _Line, _Col, _Unit, _Message: String);
begin
  with ResourceOutput.Items.Add do begin
    Caption := _Line;
    SubItems.Add(_Col);
    SubItems.Add(GetRealPath(_Unit));
    SubItems.Add(_Message);
  end;

  // Update tab caption
  ResSheet.Caption := Lang[ID_SHEET_RES] + ' (' + IntToStr(ResourceOutput.Items.Count) + ')'
end;

procedure TMainForm.CompEndProc;
var
  I: integer;
begin
  // Close it if there's nothing to show
  if (CompilerOutput.Items.Count = 0) and (ResourceOutput.Items.Count = 0) and devData.AutoCloseProgress then begin
    OpenCloseMessageSheet(FALSE)

    // Or open it if there is anything to show
  end else begin
    if (CompilerOutput.Items.Count > 0) then begin
      if MessageControl.ActivePage <> CompSheet then
        MessageControl.ActivePage := CompSheet;
    end else if (ResourceOutput.Items.Count > 0) then begin
      if MessageControl.ActivePage <> ResSheet then
        MessageControl.ActivePage := ResSheet;
    end;
    OpenCloseMessageSheet(TRUE);
  end;

  // Jump to problem location, sorted by significance
  if fCompiler.ErrorCount > 0 then begin

    // First try to find errors
    for I := 0 to CompilerOutput.Items.Count - 1 do begin

      // Caption equals the 'Line' column
      if not SameStr(CompilerOutput.Items[I].Caption, '') then begin

        if StartsStr('[Error]', CompilerOutput.Items[I].SubItems[2]) then begin

          // This item has a line number, proceed to set cursor properly
          CompilerOutput.Selected := CompilerOutput.Items[I];
          CompilerOutput.Selected.MakeVisible(False);
          CompilerOutputDblClick(CompilerOutput);
          Exit;
        end;
      end;
    end;

    // Then try to find warnings
    for I := 0 to CompilerOutput.Items.Count - 1 do begin
      if not SameStr(CompilerOutput.Items[I].Caption, '') then begin
        if StartsStr('[Warning]', CompilerOutput.Items[I].SubItems[2]) then begin
          CompilerOutput.Selected := CompilerOutput.Items[I];
          CompilerOutput.Selected.MakeVisible(False);
          CompilerOutputDblClick(CompilerOutput);
          Exit;
        end;
      end;
    end;

    // Then try to find anything with a line number...
    for I := 0 to CompilerOutput.Items.Count - 1 do begin
      if not SameStr(CompilerOutput.Items[I].Caption, '') then begin
        CompilerOutput.Selected := CompilerOutput.Items[I];
        CompilerOutput.Selected.MakeVisible(False);
        CompilerOutputDblClick(CompilerOutput);
        Exit;
      end;
    end;

    // Then try to find a resource error
    if ResourceOutput.Items.Count > 0 then begin
      ResourceOutput.Selected := ResourceOutput.Items[0];
      ResourceOutput.Selected.MakeVisible(False);
      CompilerOutputDblClick(ResourceOutput);
    end;
  end;
end;

procedure TMainForm.CompSuccessProc;
begin
  case fCompSuccessAction of
    csaRun: begin
        fCompiler.Run;
      end;
    csaDebug: begin
        actDebug.Execute;
      end;
    csaProfile: begin
        actProfile.Execute
      end;
  end;
  fCompSuccessAction := csaNone;
end;

procedure TMainForm.RunEndProc;
begin
  case fRunEndAction of
    reaProfile: begin
        if not Assigned(ProfileAnalysisForm) then
          ProfileAnalysisForm := TProfileAnalysisForm.Create(Self);
        ProfileAnalysisForm.Show;
      end;
  end;
  fRunEndAction := reaNone;
end;

procedure TMainForm.LogEntryProc(const Msg: String);
var
  BackSlashMsg: String;
begin
  // Keep original, but use backslash everywhere
  BackSlashMsg := StringReplace(Msg, '/', '\', [rfReplaceAll]);
  fLogOutputRawData.Add(BackSlashMsg);

  // Immediately add to UI component
  if devData.ShortenCompPaths then
    LogOutput.Lines.Add(ShortenLogOutput(BackSlashMsg))
  else
    LogOutput.Lines.Add(Msg);
end;

procedure TMainForm.ProjectViewContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
var
  pt: TPoint;
  Node: TTreeNode;
begin
  if not assigned(fProject) or devData.FullScreen then
    exit;

  pt := ProjectView.ClientToScreen(MousePos);
  Node := ProjectView.GetNodeAt(MousePos.X, MousePos.Y);
  if not assigned(Node) then
    exit;
  ProjectView.Selected := Node;

  // Is this the project node?
  if Node.Level = 0 then
    ProjectPopup.Popup(pt.x, pt.Y)

    // This is either a folder or a file
  else if Node.Data <> Pointer(-1) then begin
    BuildOpenWith;
    UnitPopup.Popup(pt.X, pt.Y);
  end else begin
    FolderPopup.Popup(pt.X, pt.Y);
  end;
  Handled := TRUE;
end;

procedure TMainForm.OpenUnit;
var
  Node: TTreeNode;
  i: integer;
  pt: TPoint;
  e: TEditor;
begin
  // Determine what node we have clicked on
  if Assigned(ProjectView.Selected) then
    Node := ProjectView.Selected
  else begin
    pt := ProjectView.ScreenToClient(Mouse.CursorPos);
    Node := ProjectView.GetNodeAt(pt.x, pt.y);
  end;

  // Open the file that belongs to said node
  if Assigned(Node) and (integer(Node.Data) <> -1) then begin

    // Do not accept the project node
    if (Node.Level >= 1) then begin
      i := integer(Node.Data);

      // Only open the file if it hasn't been opened yet
      e := fEditorList.FileIsOpen(fProject.Units[i].FileName, TRUE);
      if not Assigned(e) then
        e := fProject.OpenUnit(i);

      // Activate it
      if Assigned(e) then
        e.Activate;
    end;
  end;
end;

procedure TMainForm.ProjectViewClick(Sender: TObject);
var
  e: TEditor;
begin
  if not Assigned(ProjectView.Selected) then
    Exit;
  if ProjectView.Selected.Data <> Pointer(-1) then
    OpenUnit
  else begin
    e := fEditorList.GetEditor;
    if Assigned(e) then
      e.Activate;
  end;
end;

procedure TMainForm.actNewSourceExecute(Sender: TObject);
var
  NewEditor: TEditor;
begin
  if Assigned(fProject) then begin
    if MessageDlg(Lang[ID_MSG_NEWFILE], mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
      actProjectNewExecute(Sender);
      exit;
    end;
  end;

  NewEditor := fEditorList.NewEditor('', False, True);
  NewEditor.InsertDefaultText;
  NewEditor.Activate;
end;

procedure TMainForm.actNewProjectExecute(Sender: TObject);
var
  s: String;
begin
  with TNewProjectForm.Create(nil) do try
    rbCpp.Checked := devData.DefCpp;
    rbC.Checked := not rbCpp.Checked;
    if ShowModal = mrOk then begin
      if cbDefault.Checked then
        devData.DefCpp := rbCpp.Checked;

      // Take care of the currently opened project
      if Assigned(fProject) then begin
        if fProject.Name = '' then
          s := fProject.FileName
        else
          s := fProject.Name;

        // Ask if the user wants to close the current one. If not, abort
        if MessageDlg(format(Lang[ID_MSG_CLOSECREATEPROJECT], [s]), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        begin
          actCloseProject.Execute;
          if Assigned(fProject) then Exit;
        end
        else
          Exit;
      end;

      // Ask the user where he wants to save
      with TSaveDialog.Create(nil) do try
        Filter := FLT_PROJECTS;
        InitialDir := devDirs.Default;
        FileName := edProjectName.Text + DEV_EXT; // guess initial name
        Options := Options + [ofOverwritePrompt];
        DefaultExt := 'dev';

        // The user pressed the OK key. Save initial copy to disk
        if Execute then begin
          s := FileName;

          // Create an empty project
          fProject := TProject.Create(s, edProjectName.Text);

          // Assign the selected template to it
          if not fProject.AssignTemplate(s, GetTemplate) then begin
            FreeAndNil(fProject);
            MessageBox(Application.Handle, PChar(Lang[ID_ERR_TEMPLATE]), PChar(Lang[ID_ERROR]), MB_OK or
              MB_ICONERROR);
            Exit;
          end;

          // Save project preferences to disk. Don't force save all editors yet
          fProject.SaveOptions;
        end;
      finally
        Free;
      end;
    end;
  finally
    Free;
  end;
end;

procedure TMainForm.actNewTemplateExecute(Sender: TObject);
begin
  // change to save cur project as template maybe?
  with TNewTemplateForm.Create(Self) do begin
    TempProject := fProject;
    ShowModal;
  end;
end;

procedure TMainForm.actOpenExecute(Sender: TObject);
begin
  with TOpenDialog.Create(Self) do try
    Filter := BuildFilter([FLT_PROJECTS, FLT_CS, FLT_CPPS, FLT_RES, FLT_HEADS]);
    Title := Lang[ID_NV_OPENFILE];
    Options := Options + [ofAllowMultiSelect];

    // Open all provided files
    if Execute then
      OpenFileList(TStringList(Files));
  finally
    Free;
  end;
end;

procedure TMainForm.actHistoryClearExecute(Sender: TObject);
begin
  dmMain.ClearHistory;
end;

procedure TMainForm.actSaveExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    e.Save;
end;

procedure TMainForm.actSaveAsExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    e.SaveAs;
end;

procedure TMainForm.actSaveAllExecute(Sender: TObject);
var
  I: integer;
  e: TEditor;
begin
  // Pause the change notifier
  FileMonitor.BeginUpdate;
  try
    // Make changes to files
    if Assigned(fProject) then begin
      fProject.SaveAll;
      UpdateAppTitle;
      if CppParser.Statements.Count = 0 then // only scan entire project if it has not already been scanned...
        ScanActiveProject;
    end;

    // Make changes to files
    for I := 0 to pred(EditorList.PageCount) do begin
      e := fEditorList[I];
      if e.Text.Modified and not e.InProject then
        if not e.Save then
          Break;
    end;
  finally
    FileMonitor.EndUpdate;
  end;
end;

procedure TMainForm.actCloseExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    fEditorList.CloseEditor(e);
end;

procedure TMainForm.actCloseAllExecute(Sender: TObject);
begin
  ClassBrowser.BeginUpdate;
  try
    fEditorList.CloseAll; // PageControlChange triggers other UI updates
  finally
    ClassBrowser.EndUpdate;
  end;
end;

procedure TMainForm.actCloseProjectExecute(Sender: TObject);
var
  s: String;
begin
  // Stop executing program
  actStopExecuteExecute(Self);

  // Only update file monitor once (and ignore updates)
  FileMonitor.BeginUpdate;
  try
    // TODO: should we save watches?
    if fProject.Modified then begin
      if fProject.Name = '' then
        s := fProject.FileName
      else
        s := fProject.Name;

      case MessageDlg(Format(Lang[ID_MSG_SAVEPROJECT], [s]), mtConfirmation, mbYesNoCancel, 0) of
        mrYes: begin
            fProject.SaveAll; // do NOT save layout twice
          end;
        mrNo: begin
            fProject.Modified := FALSE;
            fProject.SaveLayout;
          end;
        mrCancel: begin
            fProject.SaveLayout;
            Exit;
          end;
      end;
    end else
      fProject.SaveLayout; // always save layout, but not when SaveAll has been called

    // Remember it
    dmMain.AddtoHistory(fProject.FileName);

    // Only update class browser once
    ClassBrowser.BeginUpdate;
    try
      // Only update page control once
      fEditorList.BeginUpdate;
      try
        FreeandNil(fProject);
      finally
        fEditorList.EndUpdate;
      end;

      // Clear project browser
      ProjectView.Items.Clear;

      // Clear error browser
      ClearMessageControl;

      // Clear class browser
      ClassBrowser.ProjectDir := '';
      CppParser.Reset;

      // Because fProject was assigned during editor closing, force update trigger again
      EditorPageControlLeft.OnChange(EditorPageControlLeft);
    finally
      ClassBrowser.EndUpdate;
    end;
  finally
    FileMonitor.EndUpdate;
  end;
end;

procedure TMainForm.actExportHTMLExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if assigned(e) then
    e.ExportToHTML;
end;

procedure TMainForm.actExportRTFExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if assigned(e) then
    e.ExportToRTF;
end;

procedure TMainForm.actExportTexExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if assigned(e) then
    e.ExportToTEX;
end;

procedure TMainForm.actExportProjectExecute(Sender: TObject);
begin
  if assigned(fProject) then
    fProject.ExportToHTML;
end;

procedure TMainForm.actPrintExecute(Sender: TObject);
var
  e: TEditor;
  vColors,
  vHighlight,
  vWordWrap,
  vLineNumbers,
  vLineNumbersMargins : boolean;
begin
  e := fEditorList.GetEditor;
  if not Assigned(e) then
    Exit; // Action checks this...

  with TPrintForm.Create(Self) do try
    if ShowModal = mrOk then begin
      with TSynEditPrint.Create(Self) do try
        vColors := cbColors.Checked;
        vHighlight := cbHighlight.Checked;
        vWordWrap := cbWordWrap.Checked;
        vLineNumbers := rbLN.Checked;
        vLineNumbersMargins := rbLNMargin.Checked;

        SynEdit := e.Text;
        Copies := seCopies.Value;
        Wrap := vWordWrap;
        Highlight := vHighlight;
        SelectedOnly := cbSelection.Checked;
        Colors := vColors;
        LineNumbers := rbLN.Checked or rbLNMargin.Checked;
        Highlighter := e.Text.Highlighter;
        LineNumbersInMargin := vLineNumbersMargins;
        TabWidth := devEditor.TabSize;
        Title := ExtractFileName(e.FileName);
        if Assigned(e.Text.Highlighter) and Assigned(e.Text.Highlighter.WhitespaceAttribute) then
          Color := e.Text.Highlighter.WhitespaceAttribute.Background;
        PrintEx;
      finally
        Free;
      end;

      devData.PrintColors := vColors;
      devData.PrintHighlight := vHighlight;
      devData.PrintWordWrap := vWordWrap;
      devData.PrintLineNumbers := vLineNumbers;
      devData.PrintLineNumbersMargins := vLineNumbersMargins;
    end;
  finally
    Close;
  end;
end;

procedure TMainForm.actPrintSUExecute(Sender: TObject);
var
  PrinterSetupDialog: TPrinterSetupDialog;
begin
  try
    PrinterSetupDialog := TPrinterSetupDialog.Create(Self);
    PrinterSetupDialog.Execute;
    // frees itself on close
  except
    MessageDlg(Lang[ID_ENV_PRINTERFAIL], mtError, [mbOK], 0);
  end;
end;

procedure TMainForm.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.actUndoExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    e.Text.Undo;
end;

procedure TMainForm.actRedoExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    e.Text.Redo;
end;

procedure TMainForm.actCutExecute(Sender: TObject);
var
  e: TEditor;
  oldbottomline: integer;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then begin
    oldbottomline := e.Text.TopLine + e.Text.LinesInWindow;
    e.Text.CutToClipboard;
    if (e.Text.TopLine + e.Text.LinesInWindow) <> oldbottomline then
      e.Text.Repaint; // fix for repaint fail
  end;
end;

procedure TMainForm.actCopyExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    e.Text.CopyToClipboard;
end;

procedure TMainForm.actPasteExecute(Sender: TObject);
var
  e: TEditor;
  oldbottomline: integer;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then begin
    oldbottomline := e.Text.TopLine + e.Text.LinesInWindow;
    e.Text.PasteFromClipboard;
    if (e.Text.TopLine + e.Text.LinesInWindow) <> oldbottomline then
      e.Text.Repaint; // fix for repaint fail
  end;
end;

procedure TMainForm.actSelectAllExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if assigned(e) then
    e.Text.SelectAll;
end;

procedure TMainForm.actDeleteExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    e.Text.ClearSelection;
end;

procedure TMainForm.actProjectManagerExecute(Sender: TObject);
begin
  // Hide/show this first, or otherwhise it'll show up to the left of ProjectToolWindow
  SplitterLeft.Visible := actProjectManager.Checked;
  if Assigned(fProjectToolWindow) then
    fProjectToolWindow.Close;
  LeftPageControl.Visible := actProjectManager.Checked;
  devData.ShowLeftPages := actProjectManager.Checked;
end;

procedure TMainForm.actStatusbarExecute(Sender: TObject);
begin
  devData.Statusbar := actStatusbar.Checked;
  Statusbar.Visible := actStatusbar.Checked;
  Statusbar.Top := Self.ClientHeight;
end;

procedure TMainForm.btnFullScrRevertClick(Sender: TObject);
begin
  actFullScreen.Execute;
end;

procedure TMainForm.actFullScreenExecute(Sender: TObject);
var
  Active: TWinControl;
begin
  // Remember focus
  Active := Screen.ActiveControl;

  devData.FullScreen := FullScreenModeItem.Checked;
  if devData.FullScreen then begin

    // Remember old window position
    WindowPlacement.length := sizeof(WINDOWPLACEMENT);
    GetWindowPlacement(Self.Handle, @WindowPlacement);

    // Hide stuff the user has hidden
    BorderStyle := bsNone;
    FullScreenModeItem.Caption := Lang[ID_ITEM_FULLSCRBACK];
    ToolbarDock.Visible := devData.ShowBars;

    // set size to hide form menu
    // works with multi monitors now.
    SetBounds(
      (Left + Monitor.WorkAreaRect.Left) - ClientOrigin.X,
      (Top + Monitor.WorkAreaRect.Top) - ClientOrigin.Y,
      Monitor.Width + (Width - ClientWidth),
      Monitor.Height + (Height - ClientHeight));

    // Put hint in status bar
    SetStatusbarMessage(Format(Lang[ID_FULLSCREEN_MSGNEW], [ShortCutToText(actFullScreen.ShortCut),
      ShortCutToText(actShowBars.ShortCut)]));
  end else begin

    // Reset old window position
    WindowPlacement.length := sizeof(WINDOWPLACEMENT);
    SetWindowPlacement(Self.Handle, @WindowPlacement);

    // Reset borders etc
    BorderStyle := bsSizeable;
    FullScreenModeItem.Caption := Lang[ID_ITEM_FULLSCRMODE];
    ToolbarDock.Visible := TRUE;
  end;

  // Remember focus
  if Active.CanFocus then
    Active.SetFocus;
end;

procedure TMainForm.actNextExecute(Sender: TObject);
begin
  EditorList.SelectNextPage;
end;

procedure TMainForm.actPrevExecute(Sender: TObject);
begin
  EditorList.SelectPrevPage;
end;

procedure TMainForm.actCompOptionsExecute(Sender: TObject);
begin
  with TCompOptForm.Create(nil) do try
    if ShowModal = mrOk then begin
      CheckForDLLProfiling;
      UpdateCompilerList;
    end;
  finally
    Free;
  end;
end;

procedure TMainForm.actEditorOptionsExecute(Sender: TObject);
var
  I: integer;
  e1, e2: TEditor;
begin
  with TEditorOptForm.Create(nil) do try
    if ShowModal = mrOk then begin

      // Apply editor options
      dmMain.UpdateHighlighter;
      for I := 0 to EditorList.PageCount - 1 do begin
        e1 := EditorList.Editors[i];
        if Assigned(e1) then begin
          devEditor.AssignEditor(e1.Text, e1.FileName);
          e1.InitCompletion;
        end;
      end;

      // Repaint current editor to show new colors
      fEditorList.GetVisibleEditors(e1, e2);
      if Assigned(e1) then
        e1.Text.Repaint;
      if Assigned(e2) then
        e2.Text.Repaint;

      // Only do a lengthy reparse if completion options have changed
      if taCompletion in AccessedTabs then begin
        UpdateClassBrowsing;
        ScanActiveProject;
      end;
    end;
  finally
    Free;
  end;
end;

procedure TMainForm.actConfigToolsExecute(Sender: TObject);
begin
  fTools.Edit;
end;

procedure TMainForm.actUnitRemoveExecute(Sender: TObject);
var
  idx: integer;
  node: TTreeNode;
begin
  if not assigned(fProject) then
    exit;
  while ProjectView.SelectionCount > 0 do begin
    node := ProjectView.Selections[0];
    if not assigned(node) or (node.Level < 1) then
      Continue;
    if node.Data = Pointer(-1) then
      Continue;

    idx := integer(node.Data);

    if not fProject.RemoveEditor(idx, true) then
      exit;
  end;
end;

procedure TMainForm.actUnitRenameExecute(Sender: TObject);
var
  I, ProjIndex: integer;
  OldName, NewName, CurDir: String;
  e: TEditor;
begin
  if not assigned(fProject) then
    exit;
  if not assigned(ProjectView.Selected) or
    (ProjectView.Selected.Level < 1) then
    exit;

  if ProjectView.Selected.Data = Pointer(-1) then
    Exit;

  // Get the original name
  I := integer(ProjectView.Selected.Data);
  OldName := fProject.Units[I].FileName;
  NewName := ExtractFileName(OldName);
  CurDir := ExtractFilePath(OldName);

  // Do we want to rename?
  if InputQuery(Lang[ID_RENAME], Lang[ID_MSG_FILERENAME], NewName) and (ExtractFileName(NewName) <> '') then begin

    NewName := ExpandFileto(NewName, CurDir);

    // Only continue if the user says so...
    if FileExists(NewName) and not SameFileName(OldName, NewName) then
      begin // don't remove when changing case for example
      if MessageDlg(Lang[ID_MSG_FILEEXISTS], mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin

        // Close the target file...
        e := fEditorList.FileIsOpen(NewName);
        if Assigned(e) then
          fEditorList.CloseEditor(e);

        // Remove it from the current project...
        projindex := fProject.Units.IndexOf(NewName);
        if projindex <> -1 then
          fProject.RemoveEditor(projindex, false);

        // All references to the file are removed. Delete the file from disk
        DeleteFile(NewName);

        // User didn't want to overwrite
      end else
        Exit;
    end;

    // Target filename does not exist anymore. Do a rename
    try
      // change name in project file first (no actual file renaming on disk)
      fProject.SaveUnitAs(I, NewName);

      // remove old file from monitor list
      FileMonitor.UnMonitor(OldName);

      // Finally, we can rename without issues
      RenameFile(OldName, NewName);

      // Add new filename to file minitor
      FileMonitor.Monitor(NewName);
    except
      MessageDlg(Format(Lang[ID_ERR_RENAMEFILE], [OldName]), mtError, [mbok], 0);
    end;
  end;
end;

procedure TMainForm.actUnitOpenExecute(Sender: TObject);
var
  I: integer;
  e: TEditor;
begin
  if not Assigned(fProject) then
    Exit;
  if not Assigned(ProjectView.Selected) or (ProjectView.Selected.Level < 1) then
    Exit;
  if ProjectView.Selected.Data = Pointer(-1) then
    Exit;

  I := integer(ProjectView.Selected.Data);
  e := fEditorList.FileIsOpen(fProject.Units[I].FileName, TRUE);
  if Assigned(e) then
    e.Activate
  else
    fProject.OpenUnit(I);
end;

procedure TMainForm.actUnitCloseExecute(Sender: TObject);
var
  e: TEditor;
begin
  if Assigned(fProject) and Assigned(ProjectView.Selected) then begin
    e := fEditorList.FileIsOpen(fProject.Units[integer(ProjectView.Selected.Data)].FileName, TRUE);
    if Assigned(e) then
      fEditorList.CloseEditor(e);
  end;
end;

procedure TMainForm.actAboutExecute(Sender: TObject);
begin
  with TAboutForm.Create(Self) do begin
    ShowModal;
  end;
end;

procedure TMainForm.actProjectNewExecute(Sender: TObject);
var
  idx: integer;
  FolderNode: TTreeNode;
begin
  idx := -1;
  if Assigned(fProject) then begin
    if Assigned(ProjectView.Selected) and (ProjectView.Selected.Data = Pointer(-1)) then
      FolderNode := ProjectView.Selected
    else
      FolderNode := fProject.Node;
    idx := fProject.NewUnit(FALSE, FolderNode);
  end;
  if idx <> -1 then
    with fProject.OpenUnit(idx) do begin
      Activate;
      Text.Modified := True;
    end;
end;

procedure TMainForm.actProjectAddExecute(Sender: TObject);
var
  idx: integer;
  FolderNode: TTreeNode;
begin
  if not Assigned(fProject) then
    Exit;

  // Let the user point to a file
  with TOpenDialog.Create(nil) do try
    Title := Lang[ID_NV_OPENADD];
    Filter := BuildFilter([FLT_CS, FLT_CPPS, FLT_RES, FLT_HEADS]);
    Options := Options + [ofAllowMultiSelect];

    // If sucessful, add to selected node
    if Execute then begin
      if Assigned(ProjectView.Selected) and (ProjectView.Selected.Data = Pointer(-1)) then
        FolderNode := ProjectView.Selected
      else
        FolderNode := fProject.Node;

      // Add all files
      for idx := 0 to Files.Count - 1 do begin
        fProject.AddUnit(Files[idx], FolderNode, false); // add under folder
        CppParser.AddFileToScan(Files[idx], true);
      end;

      // Rebuild project tree and parse
      fProject.RebuildNodes;
      CppParser.ParseFileList;
    end;
  finally
    Free;
  end;
end;

{ end XXXKF changed }

procedure TMainForm.actProjectRemoveExecute(Sender: TObject);
var
  I: integer;
begin
  with TRemoveUnitForm.Create(MainForm) do begin

    // Add list of project files
    for i := 0 to fProject.Units.Count - 1 do
      UnitList.Items.Add(fProject.Units[i].FileName);

    ShowModal;
  end;
end;

procedure TMainForm.actProjectOptionsExecute(Sender: TObject);
begin
  if Assigned(fProject) then begin
    if fProject.ShowOptions = mrOk then begin
      UpdateAppTitle;
      UpdateCompilerList;
    end;
  end;
end;

procedure TMainForm.actProjectSourceExecute(Sender: TObject);
begin
  if assigned(fProject) then
    OpenFile(fProject.FileName);
end;

procedure TMainForm.actFindExecute(Sender: TObject);
var
  e: TEditor;
  s: String;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then begin

    // Create it when needed!
    if not Assigned(FindForm) then
      FindForm := TFindForm.Create(Self);

    s := e.Text.SelText;
    if s = '' then
      s := e.Text.WordAtCursor;

    FindForm.TabIndex := 0;
    FindForm.cboFindText.Text := s;
    FindForm.Show;
  end;
end;

procedure TMainForm.actFindAllExecute(Sender: TObject);
var
  e: TEditor;
  s: String;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then begin

    // Create it when needed!
    if not Assigned(FindForm) then
      FindForm := TFindForm.Create(Self);

    s := e.Text.SelText;
    if s = '' then
      s := e.Text.WordAtCursor;

    FindForm.TabIndex := 1;
    FindForm.cboFindText.Text := s;
    FindForm.Show;
  end;
end;

procedure TMainForm.actReplaceExecute(Sender: TObject);
var
  e: TEditor;
  s: String;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then begin

    // Create it when needed!
    if not Assigned(FindForm) then
      FindForm := TFindForm.Create(Self);

    s := e.Text.SelText;
    if s = '' then
      s := e.Text.WordAtCursor;

    FindForm.TabIndex := 2;
    FindForm.cboFindText.Text := s;
    FindForm.Show;
  end;
end;

procedure TMainForm.actReplaceAllExecute(Sender: TObject);
var
  e: TEditor;
  s: String;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then begin

    // Create it when needed!
    if not Assigned(FindForm) then
      FindForm := TFindForm.Create(Self);

    s := e.Text.SelText;
    if s = '' then
      s := e.Text.WordAtCursor;

    FindForm.TabIndex := 3;
    FindForm.cboFindText.Text := s;
    FindForm.Show;
  end;
end;

procedure TMainForm.actGotoLineExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    e.GotoLine;
end;

function TMainForm.GetCompileTarget: TTarget;
var
  e: TEditor;
begin
  // Check if the current file belongs to a project
  e := fEditorList.GetEditor;
  if Assigned(e) then begin
    // Treat makefiles as InProject files too
    if Assigned(fProject) and (e.InProject or (fProject.MakeFileName = e.FileName)) then begin
      Result := ctProject;
    end else begin
      Result := ctFile;
    end;

    // No editors have been opened. Check if a project is open
  end else if Assigned(fProject) then begin
    Result := ctProject;

    // No project, no editor...
  end else begin
    Result := ctNone;
  end;
end;

function TMainForm.PrepareForRun(ForcedCompileTarget: TTarget = ctInvalid): Boolean;
begin
  Result := False;

  // Determine what to run
  fCompiler.Project := nil;
  fCompiler.SourceFile := '';
  fCompiler.CompilerSet := devCompilerSets.CompilationSet;
  if ForcedCompileTarget <> ctInvalid then
    fCompiler.Target := ForcedCompileTarget
  else
    fCompiler.Target := GetCompileTarget;
  case fCompiler.Target of
    ctInvalid:
      Exit;
    ctNone:
      Exit;
    ctFile: begin
        fCompiler.SourceFile := fEditorList.GetEditor.FileName;
      end;
    ctProject: begin
        fCompiler.Project := fProject;
      end;
  end;

  Result := True;
end;

function TMainForm.PrepareForCompile(ForcedCompileTarget: TTarget = ctInvalid): Boolean;
var
  i: Integer;
  e: TEditor;
begin
  Result := False;

  ClearCompileMessages;

  // always show compilation log (no intrusive windows anymore)
  if devData.ShowProgress then begin
    OpenCloseMessageSheet(True);
    MessageControl.ActivePage := LogSheet;
  end;

  // Set PATH variable (not overriden by SetCurrentDir)
  if Assigned(devCompilerSets.CompilationSet) then begin // should NOT be false
    if devCompilerSets.CompilationSet.BinDir.Count > 0 then
      SetPath(devCompilerSets.CompilationSet.BinDir[0])
    else begin
      LogEntryProc(Format(Lang[ID_LOG_NOBINDIRABORT], [devCompilerSets.CompilationSet.Name]));
      Exit; // returns false
    end;
  end;

  // Determine what to compile
  fCompiler.Project := nil;
  fCompiler.SourceFile := '';
  fCompiler.CompilerSet := devCompilerSets.CompilationSet;
  if ForcedCompileTarget <> ctInvalid then
    fCompiler.Target := ForcedCompileTarget
  else
    fCompiler.Target := GetCompileTarget;
  case fCompiler.Target of
    ctInvalid: begin
        Exit;
      end;
    ctNone: begin
        LogEntryProc(Lang[ID_LOG_NOTHINGABORT]);
        Exit;
      end;
    ctFile: begin
        e := fEditorList.GetEditor; // always succeeds if ctFile is returned
        if not Assigned(e) then
        begin
          ProjectViewClick(nil);
          e := fEditorList.GetEditor;
        end;

        if (not Assigned(e)) or (not e.Save) then
          Exit;

        fCompiler.SourceFile := e.FileName;
      end;
    ctProject: begin
        fCompiler.Project := fProject;

        // Save all files that are in a project
        if not fProject.SaveUnits then
          Exit;
        UpdateAppTitle;
        if CppParser.Statements.Count = 0 then // only scan entire project if it has not already been scanned...
          ScanActiveProject;

        // Check if saves have been succesful
        for i := 0 to EditorList.PageCount - 1 do begin // check if saving failed
          e := fEditorList.Editors[i];
          if e.InProject and e.Text.Modified then
            Exit;
        end;

        // Increment build number automagically
        if fProject.Options.VersionInfo.AutoIncBuildNr then
          fProject.IncrementBuildNumber;
        fProject.BuildPrivateResource;
      end;
  end;

  Result := True;
end;

function TMainForm.PrepareForClean(ForcedCompileTarget: TTarget = ctInvalid): Boolean;
var
  e: TEditor;
begin
  Result := False;

  ClearCompileMessages;

  // always show compilation log (no intrusive windows anymore)
  if devData.ShowProgress then begin
    OpenCloseMessageSheet(True);
    MessageControl.ActivePage := LogSheet;
  end;

  // Set PATH variable (not overriden by SetCurrentDir)
  if Assigned(devCompilerSets.CompilationSet) then begin // should NOT be false
    if devCompilerSets.CompilationSet.BinDir.Count > 0 then
      SetPath(devCompilerSets.CompilationSet.BinDir[0])
    else begin
      LogEntryProc(Format(Lang[ID_LOG_NOBINDIRABORT], [devCompilerSets.CompilationSet.Name]));
      Exit; // returns false
    end;
  end;

  // Determine what to compile
  fCompiler.Project := nil;
  fCompiler.SourceFile := '';
  fCompiler.CompilerSet := devCompilerSets.CompilationSet;
  if ForcedCompileTarget <> ctInvalid then
    fCompiler.Target := ForcedCompileTarget
  else
    fCompiler.Target := GetCompileTarget;
  case fCompiler.Target of
    ctInvalid: begin
        Exit;
      end;
    ctNone: begin
        LogEntryProc(Lang[ID_LOG_NOTHINGABORT]);
        Exit;
      end;
    ctFile: begin
        e := fEditorList.GetEditor; // always succeeds if ctFile is returned
        fCompiler.SourceFile := e.FileName;
      end;
    ctProject: begin
        fCompiler.Project := fProject;
      end;
  end;

  Result := True;
end;

procedure TMainForm.actCompileExecute(Sender: TObject);
begin
  actStopExecuteExecute(nil);
  if fCompiler.Compiling then begin
    MessageDlg(Lang[ID_MSG_ALREADYCOMP], mtInformation, [mbOK], 0);
    Exit;
  end;
  if not PrepareForCompile then
    Exit;
  fCompiler.Compile;
end;

procedure TMainForm.actRunExecute(Sender: TObject);
begin
  actStopExecuteExecute(nil);
  if fCompiler.Compiling then begin
    MessageDlg(Lang[ID_MSG_ALREADYCOMP], mtInformation, [mbOK], 0);
    Exit;
  end;
  if not PrepareForRun then
    Exit;
  fCompiler.Run;
end;

procedure TMainForm.actCompRunExecute(Sender: TObject);
begin
  actStopExecuteExecute(nil);
  if fCompiler.Compiling then begin
    MessageDlg(Lang[ID_MSG_ALREADYCOMP], mtInformation, [mbOK], 0);
    Exit;
  end;
  if not PrepareForCompile then
    Exit;

  fCompSuccessAction := csaRun;
  fCompiler.Compile;
end;

procedure TMainForm.actRebuildExecute(Sender: TObject);
begin
  actStopExecuteExecute(nil);
  if fCompiler.Compiling then begin
    MessageDlg(Lang[ID_MSG_ALREADYCOMP], mtInformation, [mbOK], 0);
    Exit;
  end;
  if not PrepareForCompile then
    Exit;
  fCompiler.RebuildAll;
end;

procedure TMainForm.actCleanExecute(Sender: TObject);
begin
  actStopExecuteExecute(nil);
  if fCompiler.Compiling then begin
    MessageDlg(Lang[ID_MSG_ALREADYCOMP], mtInformation, [mbOK], 0);
    Exit;
  end;
  if not PrepareForClean then
    Exit;
  fCompiler.Clean;
end;

procedure TMainForm.PrepareDebugger;
begin
  fDebugger.Stop;

  // Clear logs
  DebugOutput.Clear;
  EvalOutput.Clear;

  // Restore when no watch vars are shown
  fDebugger.LeftPageIndexBackup := MainForm.LeftPageControl.ActivePageIndex;

  // Focus on the debugging buttons
  LeftPageControl.ActivePage := LeftDebugSheet;
  MessageControl.ActivePage := DebugSheet;
  OpenCloseMessageSheet(True);

  // Reset watch vars
  fDebugger.DeleteWatchVars(false);
end;

procedure TMainForm.actDebugExecute(Sender: TObject);
var
  e: TEditor;
  i: integer;
  filepath: String;
  DebugEnabled, StripEnabled: boolean;
begin
  case GetCompileTarget of
    ctProject: begin
        // Check if we enabled proper options
        DebugEnabled := fProject.GetCompilerOption('-g3') <> '0';
        StripEnabled := fProject.GetCompilerOption('-s') <> '0';

        // Ask the user if he wants to enable debugging...
        if (not DebugEnabled or StripEnabled) and (MessageDlg(Lang[ID_MSG_NODEBUGSYMBOLS], mtConfirmation, [mbYes,
          mbNo], 0) = mrYes) then begin

          // Enable debugging, disable stripping
          fProject.SetCompilerOption('-g3', '1');
          fProject.SetCompilerOption('-s', '0');

          fCompSuccessAction := csaDebug;
          actRebuildExecute(nil);
          Exit;
        end;

        // Did we compile?
        if not FileExists(fProject.Executable) then begin
          if MessageDlg(Lang[ID_ERR_PROJECTNOTCOMPILEDSUGGEST], mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
            actCompileExecute(nil);
          end;
          exit;
        end;

        // Did we choose a host application for our DLL?
        if fProject.Options.typ = dptDyn then begin
          if fProject.Options.HostApplication = '' then begin
            MessageDlg(Lang[ID_ERR_HOSTMISSING], mtWarning, [mbOK], 0);
            exit;
          end else if not FileExists(fProject.Options.HostApplication) then begin
            MessageDlg(Lang[ID_ERR_HOSTNOTEXIST], mtWarning, [mbOK], 0);
            exit;
          end;
        end;

        // Reset UI, remove invalid breakpoints
        PrepareDebugger;

        filepath := fProject.Executable;

        fDebugger.Start;
        fDebugger.SendCommand('file', '"' + StringReplace(filepath, '\', '/', [rfReplaceAll]) + '"');

        if fProject.Options.typ = dptDyn then
          fDebugger.SendCommand('exec-file', '"' + StringReplace(fProject.Options.HostApplication, '\', '/',
            [rfReplaceAll])
            + '"');
      end;
    ctFile: begin
        // Check if we enabled proper options
        with devCompilerSets.CompilationSet do begin
          DebugEnabled := GetOption('-g3') <> '0';
          StripEnabled := GetOption('-s') <> '0';
        end;

        // Ask the user if he wants to enable debugging...
        if (not DebugEnabled or StripEnabled) and (MessageDlg(Lang[ID_MSG_NODEBUGSYMBOLS], mtConfirmation, [mbYes,
          mbNo], 0) = mrYes) then begin

          // Enable debugging, disable stripping
          with devCompilerSets.CompilationSet do begin
            SetOption('-g3', '1');
            SetOption('-s', '0');
          end;

          // Save changes to compiler set
          devCompilerSets.SaveSet(devCompilerSets.CompilationSetIndex);

          fCompSuccessAction := csaDebug;
          actRebuildExecute(nil);
          Exit;
        end;

        e := fEditorList.GetEditor;
        if Assigned(e) then begin

          // Did we compile?
          if not FileExists(ChangeFileExt(e.FileName, EXE_EXT)) then begin
            if MessageDlg(Lang[ID_ERR_SRCNOTCOMPILEDSUGGEST], mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
              fCompSuccessAction := csaDebug;
              actCompileExecute(nil);
            end;
            Exit;
          end;

          // Did we save?
          if e.Text.Modified then // if file is modified
            if not e.Save then // save it first
              Exit;

          PrepareDebugger;

          filepath := ChangeFileExt(e.FileName, EXE_EXT);

          fDebugger.Start;
          fDebugger.SendCommand('file', '"' + StringReplace(filepath, '\', '/', [rfReplaceAll]) + '"');
        end;
      end;
    ctNone: Exit;
  end;

  // Add library folders
  with devCompilerSets.CompilationSet do begin
    for I := 0 to LibDir.Count - 1 do
      fDebugger.SendCommand('dir', '"' + StringReplace(LibDir[i], '\', '/', [rfReplaceAll]) + '"');

    // Add include folders
    for I := 0 to CDir.Count - 1 do
      fDebugger.SendCommand('dir', '"' + StringReplace(CDir[i], '\', '/', [rfReplaceAll]) + '"');

    // Add more include folders, duplicates will be added/moved to front of list
    for I := 0 to CppDir.Count - 1 do
      fDebugger.SendCommand('dir', '"' + StringReplace(CppDir[i], '\', '/', [rfReplaceAll]) + '"');
  end;

  // Add breakpoints and watch vars
  for i := 0 to fDebugger.WatchVarList.Count - 1 do
    fDebugger.AddWatchVar(i);

  for i := 0 to fDebugger.BreakPointList.Count - 1 do
    fDebugger.AddBreakpoint(i);

  // Run the debugger
  fDebugger.SendCommand('set', 'width 0'); // don't wrap output, very annoying
  fDebugger.SendCommand('set', 'new-console on');
  fDebugger.SendCommand('set', 'confirm off');
  fDebugger.SendCommand('cd', ExtractFileDir(filepath)); // restore working directory
  case GetCompileTarget of
    ctNone:
      Exit;
    ctFile:
      fDebugger.SendCommand('run', fCompiler.RunParams);
    ctProject:
      fDebugger.SendCommand('run', fProject.Options.CmdLineArgs);
  end;
end;

procedure TMainForm.actEnviroOptionsExecute(Sender: TObject);
begin
  with TEnviroForm.Create(nil) do try
    if ShowModal = mrOk then begin
      // Update pagecontrol
      EditorList.SetPreferences(devData.MsgTabs, devData.MultiLineTab);

      // Update fullscreen options
      if devData.FullScreen then
        ToolbarDock.Visible := devData.ShowBars;

      // Pick new language if we have to (long process)
      if devData.LangChange then begin
        Lang.SetLang(devData.Language);
        LoadText;
      end;

      // Load new icons, also only if we have to
      if devData.ThemeChange then
        Loadtheme;
      Shortcuts.Filename := devDirs.Config + DEV_SHORTCUTS_FILE;

      // Rebuild recent file list (max count could have changed
      dmMain.RebuildMRU;
    end;
  finally
    Close;
  end;
end;

procedure TMainForm.actUpdatePageCount(Sender: TObject);
begin
  TCustomAction(Sender).Enabled := fEditorList.PageCount > 0;
end;

procedure TMainForm.actUpdateDeleteWatch(Sender: TObject);
begin
  TCustomAction(Sender).Enabled := Assigned(DebugView.Selected);
end;

procedure TMainForm.actUpdatePageorProject(Sender: TObject);
begin
  TCustomAction(Sender).Enabled := Assigned(fProject) or (fEditorList.PageCount > 0);
end;

procedure TMainForm.actCompileUpdate(Sender: TObject);
begin
  TCustomAction(Sender).Enabled := (not fCompiler.Compiling) and (GetCompileTarget <> ctNone) and
    Assigned(devCompilerSets.CompilationSet);

  if (Sender = actSyntaxCheckFile) and TCustomAction(Sender).Enabled then
    TCustomAction(Sender).Enabled := (fEditorList.GetEditor <> nil) or Assigned(ProjectView.Selected);
end;

procedure TMainForm.actUpdateProject(Sender: TObject);
begin
  TCustomAction(Sender).Enabled := assigned(fProject);
end;

procedure TMainForm.actUpdateMakeFile(Sender: TObject);
begin
  TCustomAction(Sender).Enabled := assigned(fProject) and (not fCompiler.Compiling) and (GetCompileTarget <> ctNone) and
    Assigned(devCompilerSets.CompilationSet);
end;

procedure TMainForm.actUpdateEmptyEditor(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  TCustomAction(Sender).Enabled := Assigned(e) and not e.Text.Text.IsEmpty;
end;

procedure TMainForm.actUpdateDebuggerRunning(Sender: TObject);
begin
  TCustomAction(Sender).Enabled := fDebugger.Executing;
end;

procedure TMainForm.actUpdateDebuggerRunningCPU(Sender: TObject);
begin
  TCustomAction(Sender).Enabled := fDebugger.Executing and not Assigned(CPUForm);
end;

procedure TMainForm.actUpdateEmptyEditorFindForm(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  TCustomAction(Sender).Enabled := Assigned(e) and (not Assigned(FindForm) or not FindForm.Showing);
end;

procedure TMainForm.actRunUpdate(Sender: TObject);
begin
  if Assigned(fProject) then
    TCustomAction(Sender).Enabled := (GetCompileTarget <> ctNone) and (fProject.Options.typ <> dptStat) and (not
      fCompiler.Compiling)
  else
    TCustomAction(Sender).Enabled := (GetCompileTarget <> ctNone) and (not fCompiler.Compiling);
end;

procedure TMainForm.actCompileRunUpdate(Sender: TObject);
begin
  if Assigned(fProject) then
    TCustomAction(Sender).Enabled := (fProject.Options.typ <> dptStat) and (not fCompiler.Compiling) and
      (GetCompileTarget <> ctNone) and Assigned(devCompilerSets.CompilationSet)
  else
    TCustomAction(Sender).Enabled := (not fCompiler.Compiling) and (GetCompileTarget <> ctNone) and
      Assigned(devCompilerSets.CompilationSet)
end;

procedure TMainForm.ToolbarDockClick(Sender: TObject);
begin
  tbMain.Visible := ToolMainItem.Checked;
  tbEdit.Visible := ToolEditItem.Checked;
  tbCompile.Visible := ToolCompileandRunItem.Checked;
  tbProject.Visible := ToolProjectItem.Checked;
  tbSpecials.Visible := ToolSpecialsItem.Checked;
  tbSearch.Visible := ToolSearchItem.Checked;
  tbClasses.Visible := ToolClassesItem.Checked;
  tbCompilers.Visible := ToolCompilersItem.Checked;

  devData.ToolbarMain := ToolMainItem.Checked;
  devData.ToolbarEdit := ToolEditItem.Checked;
  devData.ToolbarCompile := ToolCompileandRunItem.Checked;
  devData.ToolbarProject := ToolProjectItem.Checked;
  devData.ToolbarSpecials := ToolSpecialsItem.Checked;
  devData.ToolbarSearch := ToolSearchItem.Checked;
  devData.ToolbarClasses := ToolClassesItem.Checked;
  devData.ToolbarCompilers := ToolCompilersItem.Checked;
end;

procedure TMainForm.ToolbarDockContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
var
  pt: TPoint;
begin
  pt := ToolbarDock.ClientToScreen(MousePos);
  TrackPopupMenu(ToolbarsItem.Handle, TPM_LEFTALIGN or TPM_LEFTBUTTON,
    pt.x, pt.y, 0, Self.Handle, nil);
  Handled := TRUE;
end;

procedure TMainForm.SplitterBottomMoved(Sender: TObject);
begin
  fPreviousHeight := MessageControl.Height;
end;

procedure TMainForm.actProjectMakeFileExecute(Sender: TObject);
begin
  // Fake a project compilation step
  ClearCompileMessages;
  fCompiler.Target := ctProject;
  fCompiler.Project := fProject;
  fCompiler.CompilerSet := devCompilerSets.CompilationSet;
  fCompiler.BuildMakeFile;

  // Show the results
  OpenFile(fCompiler.MakeFile);
end;

procedure TMainForm.actMsgCutExecute(Sender: TObject);
begin
  case MessageControl.ActivePageIndex of
    3: begin
        if EvaluateInput.Focused then begin
          Clipboard.AsText := EvaluateInput.SelText;
          EvaluateInput.SelText := '';
        end else if edGDBcommand.Focused then begin
          Clipboard.AsText := edGDBCommand.SelText;
          edGDBCommand.SelText := '';
        end;
      end;
  end;
end;

procedure TMainForm.actMsgCopyExecute(Sender: TObject);
begin
  case MessageControl.ActivePageIndex of
    0:
      Clipboard.AsText := GetPrettyLine(CompilerOutput);
    1:
      Clipboard.AsText := GetPrettyLine(ResourceOutput);
    2: begin
        if LogOutput.Focused then
          LogOutput.CopyToClipboard;
      end;
    3: begin
        if EvaluateInput.Focused then
          Clipboard.AsText := EvaluateInput.SelText
        else if EvalOutput.Focused then
          EvalOutput.CopyToClipboard
        else if edGDBcommand.Focused then
          Clipboard.AsText := edGDBCommand.SelText
        else if DebugOutput.Focused then
          DebugOutput.CopyToClipboard;
      end;
    4:
      Clipboard.AsText := GetPrettyLine(FindOutput);
  end;
end;

procedure TMainForm.actMsgCopyAllExecute(Sender: TObject);
var
  i: integer;
begin
  case MessageControl.ActivePageIndex of
    0: begin
        ClipBoard.AsText := '';
        for i := 0 to pred(CompilerOutput.Items.Count) do
          Clipboard.AsText := Clipboard.AsText + GetPrettyLine(CompilerOutput, i) + #13#10;
      end;
    1: begin
        ClipBoard.AsText := '';
        for i := 0 to pred(ResourceOutput.Items.Count) do
          Clipboard.AsText := Clipboard.AsText + GetPrettyLine(ResourceOutput, i) + #13#10;
      end;
    2:
      LogOutput.CopyToClipboard;
    3: begin
        if EvaluateInput.Focused then
          Clipboard.AsText := EvaluateInput.Text
        else if EvalOutput.Focused then
          Clipboard.AsText := EvalOutput.Text
        else if edGDBcommand.Focused then
          Clipboard.AsText := edGDBcommand.Text
        else if DebugOutput.Focused then
          Clipboard.AsText := DebugOutput.Text
      end;
    4: begin
        ClipBoard.AsText := '';
        for i := 0 to pred(FindOutput.Items.Count) do
          Clipboard.AsText := Clipboard.AsText + GetPrettyLine(FindOutput, i) + #13#10;
      end;
  end;
end;

procedure TMainForm.actMsgPasteExecute(Sender: TObject);
begin
  case MessageControl.ActivePageIndex of
    3: begin
        if EvaluateInput.Focused then
          EvaluateInput.SelText := ClipBoard.AsText
        else if edGDBcommand.Focused then
          edGdbCommand.SelText := ClipBoard.AsText
      end;
  end;
end;

procedure TMainForm.actMsgSelAllExecute(Sender: TObject);
begin
  case MessageControl.ActivePageIndex of
    2: begin
        if LogOutput.Focused then
          LogOutput.SelectAll;
      end;
    3: begin
        if EvaluateInput.Focused then
          EvaluateInput.SelectAll
        else if edGdbCommand.Focused then
          edGdbCommand.SelectAll
        else if EvalOutput.Focused then
          EvalOutput.SelectAll
        else if DebugOutput.Focused then
          DebugOutput.SelectAll;
      end;
  end;
end;

procedure TMainForm.actMsgSaveAllExecute(Sender: TObject);
var
  i: integer;
  fulloutput: String;
  Stream: TFileStream;
  e: TEditor;
begin
  fulloutput := '';
  with TSaveDialog.Create(self) do try
    case MessageControl.ActivePageIndex of
      0: begin
          FileName := 'Formatted Compiler Output';
          for i := 0 to CompilerOutput.Items.Count - 1 do
            fulloutput := fulloutput + GetPrettyLine(CompilerOutput, i) + #13#10;
        end;
      1: begin
          FileName := 'Resource Error Log';
          for i := 0 to ResourceOutput.Items.Count - 1 do
            fulloutput := fulloutput + GetPrettyLine(ResourceOutput, i) + #13#10;
        end;
      2: begin
          FileName := 'Raw Build Log';
          if Length(LogOutput.Text) > 0 then
            fulloutput := LogOutput.Text;
        end;
      3: begin
          FileName := 'Raw GDB Output';
          if Length(DebugOutput.Text) > 0 then
            fulloutput := DebugOutput.Text;
        end;
      4: begin
          FileName := 'Find Results';
          for i := 0 to FindOutput.Items.Count - 1 do
            fulloutput := fulloutput + GetPrettyLine(FindOutput, i) + #13#10;
        end;
    end;

    if Length(fulloutput) > 0 then begin

      Title := Lang[ID_NV_SAVEFILE];
      Filter := BuildFilter([FLT_TEXTS]);
      DefaultExt := 'txt';
      FilterIndex := 1;
      Options := Options + [ofOverwritePrompt];

      if Assigned(fProject) then begin
        InitialDir := fProject.Directory;
      end else begin
        e := fEditorList.GetEditor;
        if Assigned(e) then
          InitialDir := ExtractFilePath(e.FileName)
        else
          InitialDir := 'C:\';
      end;

      if Execute then begin
        Stream := TFileStream.Create(FileName, fmCreate);
        try
          Stream.Write(fulloutput[1], Length(fulloutput));
        finally
          Stream.Free;
        end;
      end;
    end;
  finally
    Free;
  end;
end;

procedure TMainForm.actMsgClearExecute(Sender: TObject);
begin
  case MessageControl.ActivePageIndex of
    0:
      CompilerOutput.Items.Clear;
    1:
      ResourceOutput.Items.Clear;
    2:
      LogOutput.Clear;
    3: begin
        if EvaluateInput.Focused then
          EvaluateInput.Clear
        else if EvalOutput.Focused then
          EvalOutput.Clear
        else if edGDBcommand.Focused then
          edGDBcommand.Clear
        else if DebugOutput.Focused then
          DebugOutput.Clear;
      end;
    4: begin
        FindOutput.Clear;
      end;
  end;
end;

procedure TMainForm.actMsgHideExecute(Sender: TObject);
begin
  OpenCloseMessageSheet(MessageControl.Height <> fPreviousHeight);
end;

procedure TMainForm.actIncrementalExecute(Sender: TObject);
var
  pt: TPoint;
  e: TEditor;
  PageControl: TPageControl;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then begin

    // Only create the form when we need to do so
    if not Assigned(IncrementalForm) then
      IncrementalForm := TIncrementalForm.Create(self);

    PageControl := e.PageControl;
    pt := ClienttoScreen(point(PageControl.Left + PageControl.Width - IncrementalForm.Width - 10, PageControl.Top));

    IncrementalForm.Left := pt.x;
    IncrementalForm.Top := pt.y;
    IncrementalForm.editor := e.Text;
    IncrementalForm.Show;
  end;
end;

procedure TMainForm.CompilerOutputDblClick(Sender: TObject);
var
  Col, Line: integer;
  selection: TListItem;
  errorfiletab: TEditor;
begin
  selection := TListView(Sender).Selected; // used by compile and resource lists
  if Assigned(selection) then begin
    Line := StrToIntDef(selection.Caption, 1);
    Col := StrToIntDef(selection.SubItems[0], 1);
    errorfiletab := fEditorList.GetEditorFromFileName(selection.SubItems[1]);

    if Assigned(errorfiletab) then begin
      errorfiletab.SetErrorFocus(Col, Line);
      errorfiletab.Activate;
    end;
  end;
end;

procedure TMainForm.FindOutputDblClick(Sender: TObject);
var
  col, line: integer;
  e: TEditor;
  selected: TListItem;
begin
  selected := FindOutPut.Selected;
  if Assigned(selected) and not SameStr(selected.Caption, '') then begin
    Col := StrToIntDef(selected.SubItems[1], 1);
    Line := StrToIntDef(selected.SubItems[0], 1);

    // And open up
    e := fEditorList.GetEditorFromFileName(selected.SubItems[2]);
    if Assigned(e) then begin

      // Position the caret
      // uncollapse folds if needed
      e.SetCaretPosAndActivate(Line, Col);

      // Then select the word we searched for
      e.Text.SetSelWord;
      e.Text.CaretXY := e.Text.BlockBegin;
    end;
  end;
end;

procedure TMainForm.actShowBarsExecute(Sender: TObject);
begin
  if devData.FullScreen then
    ToolbarDock.Visible := not ToolbarDock.Visible;
end;

procedure TMainForm.FormContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
var
  pt: TPoint;
begin
  pt := ClientToScreen(MousePos);
  TrackPopupMenu(ViewMenu.Handle, TPM_LEFTALIGN or TPM_LEFTBUTTON, pt.x, pt.y, 0, Self.Handle, nil);
  Handled := TRUE;
end;

procedure TMainForm.actAddWatchExecute(Sender: TObject);
var
  s: String;
  e: TEditor;
begin
  s := '';
  e := fEditorList.GetEditor;
  if Assigned(e) then begin
    if e.Text.SelAvail then
      s := e.Text.SelText
    else begin
      s := e.Text.WordAtCursor;
      if not InputQuery(Lang[ID_NV_ADDWATCH], Lang[ID_NV_ENTERVAR], s) then
        Exit;
    end;
    if s <> '' then
      fDebugger.AddWatchVar(s);
  end;
end;

procedure TMainForm.actNextLineExecute(Sender: TObject);
begin
  if fDebugger.Executing then begin
    fDebugger.SendCommand('next', '', true);
  end;
end;

procedure TMainForm.actStepLineExecute(Sender: TObject);
begin
  if fDebugger.Executing then begin
    fDebugger.SendCommand('step', '', true);
  end;
end;

procedure TMainForm.actRemoveWatchExecute(Sender: TObject);
var
  node: TTreeNode;
begin
  node := DebugView.Selected;
  if Assigned(node) then begin

    // Retrieve topmost node
    while Assigned(node.Parent) do
      node := node.Parent;

    fDebugger.RemoveWatchVar(node);
  end;
end;

procedure TMainForm.RemoveActiveBreakpoints;
var
  i: integer;
begin
  for i := 0 to EditorList.PageCount - 1 do
    fEditorList[i].RemoveBreakpointFocus;
end;

procedure TMainForm.GotoBreakpoint(const FileName: String; Line: integer);
var
  e: TEditor;
begin
  // Remove line focus in files left behind
  RemoveActiveBreakpoints;

  // Then active the current line in the current file
  e := fEditorList.GetEditorFromFileName(StringReplace(FileName, '/', '\', [rfReplaceAll]));
  if Assigned(e) then begin
    e.SetActiveBreakpointFocus(Line);
    e.Activate;
  end;
  Application.BringToFront;
end;

procedure TMainForm.actStepOverExecute(Sender: TObject);
begin
  if fDebugger.Executing then begin
    RemoveActiveBreakpoints;
    fDebugger.SendCommand('continue', '', true);
  end;
end;

procedure TMainForm.actStopExecuteExecute(Sender: TObject);
begin
  if fDebugger.Executing then
    fDebugger.Stop
  else if devExecutor.Running then
    devExecutor.Reset;
end;

procedure TMainForm.actUndoUpdate(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  actUndo.Enabled := assigned(e) and e.Text.CanUndo;
end;

procedure TMainForm.actRedoUpdate(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  actRedo.enabled := assigned(e) and e.Text.CanRedo;
end;

procedure TMainForm.actCutUpdate(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  actCut.Enabled := assigned(e) and e.Text.SelAvail;
end;

procedure TMainForm.actCopyUpdate(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  actCopy.Enabled := Assigned(e) and e.Text.SelAvail;
end;

procedure TMainForm.actPasteUpdate(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  actPaste.Enabled := Assigned(e) and e.Text.CanPaste;
end;

procedure TMainForm.actSaveUpdate(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  actSave.Enabled := Assigned(e) and (e.Text.Modified or (e.FileName = ''));
end;

procedure TMainForm.actSaveAsUpdate(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  actSaveAs.Enabled := Assigned(e);
end;

procedure TMainForm.ClearCompileMessages;
begin
  CompilerOutput.Clear;
  ResourceOutput.Clear;
  LogOutput.Clear;
  fLogOutputRawData.Clear;
  DebugOutput.Clear;
end;

procedure TMainForm.ClearMessageControl;
begin
  FindOutput.Items.Clear; // don't clear this when compiling...
  ClearCompileMessages;
end;

procedure TMainForm.actFileMenuExecute(Sender: TObject);
begin
  //	dummy event to keep menu active
end;

procedure TMainForm.actToolsMenuExecute(Sender: TObject);
var
  I, J: integer;
begin
  for I := (ToolsMenu.IndexOf(PackageManagerItem) + 2) to ToolsMenu.Count - 1 do begin
    J := ToolsMenu.Items[I].tag;
    ToolsMenu.Items[I].Enabled := FileExists(ParseToolParams(fTools.ToolList[J]^.Exec));
    if not ToolsMenu.Items[I].Enabled then
      ToolsMenu.Items[I].Caption := fTools.ToolList[J]^.Title + ' (Tool not found)';
  end;
end;

procedure TMainForm.UpdateCompilerList;
var
  I: integer;
begin
  cmbCompilers.Items.BeginUpdate;
  try
    cmbCompilers.Clear;

    // Populate list
    for I := 0 to devCompilerSets.Count - 1 do
      cmbCompilers.Items.Add(devCompilerSets[i].Name);

    // Select current compiler
    cmbCompilers.ItemIndex := devCompilerSets.CompilationSetIndex;

    // Mention change to compiler sets
    //devCompilerSets.OnCompilerSetChanged(
    //  devCompilerSets[fOldCompilerToolbarIndex],
    //  devCompilerSets[cmbCompilers.ItemIndex]);

    // Remember current index
    fOldCompilerToolbarIndex := cmbCompilers.ItemIndex;
  finally
    cmbCompilers.Items.EndUpdate;
  end;
end;

procedure TMainForm.UpdateClassBrowsing;
var
  I: integer;
begin
  // Configure parser
  CppParser.Reset;
  CppParser.Tokenizer := CppTokenizer;
  CppParser.Preprocessor := CppPreprocessor;
  CppParser.Enabled := devCodeCompletion.Enabled;
  CppParser.ParseLocalHeaders := devCodeCompletion.ParseLocalHeaders;
  CppParser.ParseGlobalHeaders := devCodeCompletion.ParseGlobalHeaders;
  CppParser.OnStartParsing := CppParserStartParsing;
  CppParser.OnEndParsing := CppParserEndParsing;
  CppParser.OnTotalProgress := CppParserTotalProgress;

  // Set options depending on the current compiler set
  // TODO: do this every time OnCompilerSetChanged
  if Assigned(devCompilerSets.DefaultSet) then
    with devCompilerSets.DefaultSet do begin
      CppParser.ClearIncludePaths;
      for I := 0 to CDir.Count - 1 do
        CppParser.AddIncludePath(CDir[I]);
      for I := 0 to CppDir.Count - 1 do
        CppParser.AddIncludePath(CppDir[I]);
      for I := 0 to DefInclude.Count - 1 do // Add default include dirs last, just like gcc does
        CppParser.AddIncludePath(DefInclude[I]); // TODO: retrieve those directories in devcfg

      // Set defines
      CppParser.ResetDefines;
      for I := 0 to Defines.Count - 1 do
        CppParser.AddHardDefineByLine(Defines[i]); // predefined constants from -dM -E
    end;

  // Configure code completion
  CodeCompletion.Color := devCodeCompletion.BackColor;
  CodeCompletion.Width := devCodeCompletion.Width;
  CodeCompletion.Height := devCodeCompletion.Height;

  // Only attempt to redraw once
  ClassBrowser.BeginUpdate;
  try
    ClassBrowser.ShowFilter := TShowFilter(devClassBrowsing.ShowFilter);
    ClassBrowser.ShowInheritedMembers := devClassBrowsing.ShowInheritedMembers;
    ClassBrowser.TabVisible := LeftPageControl.ActivePageIndex = 1;
  finally
    ClassBrowser.EndUpdate;
  end;

  // Configure class browser actions
  actBrowserViewAll.Checked := ClassBrowser.ShowFilter = sfAll;
  actBrowserViewProject.Checked := ClassBrowser.ShowFilter = sfProject;
  actBrowserViewCurrent.Checked := ClassBrowser.ShowFilter = sfCurrent;
  actBrowserViewIncludes.Checked := ClassBrowser.ShowFilter = sfSystemFiles;
  actBrowserShowInherited.Checked := ClassBrowser.ShowInheritedMembers;
end;

procedure TMainForm.ScanActiveProject;
var
  I: integer;
  e: TEditor;
begin
  with CppParser do begin
    Reset;
    if Assigned(fProject) then begin
      ProjectDir := fProject.Directory;
      for I := 0 to fProject.Units.Count - 1 do
        AddFileToScan(fProject.Units[I].FileName, True);
      ClearProjectIncludePaths;
      for I := 0 to fProject.Options.Includes.Count - 1 do
        AddProjectIncludePath(fProject.Options.Includes[I]);
    end else begin
      e := fEditorList.GetEditor;
      if Assigned(e) then begin
        ProjectDir := ExtractFilePath(e.FileName);
        AddFileToScan(e.FileName);
      end else
        ProjectDir := '';
    end;
    ParseFileList;
  end;
end;

procedure TMainForm.ClassBrowserSelect(Sender: TObject; Filename: TFileName; Line: Integer);
var
  e: TEditor;
begin
  e := fEditorList.GetEditorFromFilename(FileName);
  if Assigned(e) then begin
    e.SetCaretPosAndActivate(Line, 1);
  end;
end;

procedure TMainForm.CodeCompletionResize(Sender: TObject);
begin
  devCodeCompletion.Width := CodeCompletion.Width;
  devCodeCompletion.Height := CodeCompletion.Height;
end;

procedure TMainForm.actSwapHeaderSourceExecute(Sender: TObject);
var
  FromEditor, ToEditor: TEditor;
  CFile, HFile, ToFile: String;
  iscfile, ishfile: boolean;
begin
  FromEditor := fEditorList.GetEditor;
  if not Assigned(FromEditor) then
    Exit;

  iscfile := CppParser.IsCfile(FromEditor.FileName);
  ishfile := CppParser.IsHfile(FromEditor.FileName);

  CppParser.GetSourcePair(FromEditor.FileName, CFile, HFile);
  if iscfile then begin
    ToFile := HFile
  end else if ishfile then begin
    ToFile := CFile
  end else begin
    Exit; // don't know file type, don't bother searching
  end;

  // Fetch the file we want to go to
  ToEditor := fEditorList.FileIsOpen(ToFile);
  if Assigned(ToEditor) then
    ToEditor.Activate
  else begin
    // We need to open a new editor
    ToEditor := fEditorList.GetEditorFromFileName(ToFile);
    if Assigned(ToEditor) then begin

      // Move the editors next to each other if possible
      // Do not try to do so if they are visible in separate editors
      if FromEditor.PageControl = ToEditor.PageControl then begin

        // Move source file to the left of the header file
        if iscfile then begin
          ToEditor.TabSheet.PageIndex := min(FromEditor.PageControl.PageCount - 1, FromEditor.TabSheet.PageIndex + 1);
        end else begin
          ToEditor.TabSheet.PageIndex := max(0, FromEditor.TabSheet.PageIndex);
        end;
      end;
      ToEditor.Activate;
    end else if iscfile then begin
      SetStatusBarMessage(Lang[ID_MSG_CORRESPONDINGHEADER]);
    end else if ishfile then begin
      SetStatusBarMessage(Lang[ID_MSG_CORRESPONDINGSOURCE]);
    end;
  end;
end;

procedure TMainForm.actSyntaxCheckExecute(Sender: TObject);
begin
  actStopExecuteExecute(nil);
  if fCompiler.Compiling then begin
    MessageDlg(Lang[ID_MSG_ALREADYCOMP], mtInformation, [mbOK], 0);
    Exit;
  end;
  if not PrepareForCompile then
    Exit;
  fCompiler.CheckSyntax;
end;

procedure TMainForm.EditorSaveTimer(Sender: TObject);
var
  e: TEditor;
  i: integer;
  EditorSaveList: TList;
  NewFileName: String;
begin
  EditorSaveList := TList.Create;
  try
    // Assemble a list of files that need saving
    case devEditor.AutoSaveFilter of
      0: begin // Save current file
          e := fEditorList.GetEditor;
          if Assigned(e) and not e.New then // don't save untitled files
            EditorSaveList.Add(e);
        end;
      1: begin // Save all open files
          for I := 0 to EditorList.PageCount - 1 do begin
            e := fEditorList[i];
            if Assigned(e) and not e.New then
              EditorSaveList.Add(e);
          end;
        end;
      2: begin // Save all project files
          for I := 0 to EditorList.PageCount - 1 do begin
            e := fEditorList[i];
            if Assigned(e) and e.InProject and not e.New then
              EditorSaveList.Add(e);
          end;
        end;
    end;

    // Then process the list
    // For each file, get original filename, determine copy filename, and save the copy
    for I := 0 to EditorSaveList.Count - 1 do begin
      e := TEditor(EditorSaveList[i]);
      case devEditor.AutoSaveMode of
        0: begin // overwrite (standard save)
            if e.Text.Modified and e.Save then
              SetStatusbarMessage(Format(Lang[ID_AUTOSAVEDFILE], [e.FileName]));
          end;
        1: begin // append UNIX timestamp (backup copy, don't update class browser)
            NewFileName := ChangeFileExt(e.FileName, '.' + IntToStr(DateTimeToUnix(Now)) + ExtractFileExt(e.FileName));
            e.Text.Lines.SaveToFile(NewFileName);
            SetStatusbarMessage(Format(Lang[ID_AUTOSAVEDFILEAS], [e.FileName, NewFileName]));
          end;
        2: begin // append formatted timestamp (backup copy, don't update class browser)
            NewFileName := ChangeFileExt(e.FileName, '.' + FormatDateTime('yyyy mm dd hh mm ss', Now) +
              ExtractFileExt(e.FileName));
            e.Text.Lines.SaveToFile(NewFileName);
            SetStatusbarMessage(Format(Lang[ID_AUTOSAVEDFILEAS], [e.FileName, NewFileName]));
          end;
      end;
    end;
  finally
    // Do not save members.
    EditorSaveList.Free;
  end;
end;

procedure TMainForm.EditorPageControlChange(Sender: TObject);
var
  e: TEditor;
  PageControl: TPageControl;
  I: integer;
begin
  // Determine who sent this message
  PageControl := TPageControl(Sender);
  e := fEditorList.GetEditor(-1, PageControl);
  if Assigned(e) then begin
    // Update focus so user can keep typing
    if e.Text.CanFocus then // TODO: can fail for some reason
      e.Text.SetFocus; // this should trigger then OnEnter even of the Text control

    // No editors are visible
  end else begin
    // Set title bar to current file
    UpdateAppTitle;

    // Set classbrowser to current file
    ClassBrowser.CurrentFile := '';

    // Set compiler selector to current file
    UpdateCompilerList;

    // Update status bar
    SetStatusbarLineCol;

    // Update bookmark menu
    for i := 1 to 9 do begin
      TogglebookmarksPopItem.Items[i - 1].Checked := false;
      TogglebookmarksItem.Items[i - 1].Checked := false;
    end;

    // Update focus of incremental search
    if Assigned(IncrementalForm) and IncrementalForm.Showing then
      IncrementalForm.Close;
  end;
end;

procedure TMainForm.actConfigdevShortcutsExecute(Sender: TObject);
begin
  Shortcuts.Edit(
    Lang[ID_SC_CAPTION], // window
    Lang[ID_SC_HDRENTRY], // column 1
    Lang[ID_SC_HDRSHORTCUT], // column 2
    Lang[ID_SC_NEWTIP], // hint
    Lang[ID_BTN_OK], // OK button
    Lang[ID_BTN_CANCEL], // Cancel button
    Lang[ID_SC_RESETALL], // Reset All button
    Lang[ID_SC_RESETCURRENT], // Reset Current button
    Lang[ID_SC_NEWREPLACEHINT], // messagebox text when replacing
    Lang[ID_SC_CONFIRMALL], // messagebox text when setting to defaults
    Lang[ID_SC_CONFIRMONE], // messagebox text when setting to defaults
    Lang[ID_SC_BUTTON]); // Button/Misc
end;

procedure TMainForm.DateTimeMenuItemClick(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    e.InsertString(FormatDateTime('dd/mm/yy hh:nn', Now), TRUE);
end;

procedure TMainForm.CommentheaderMenuItemClick(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then begin
    e.InsertString('/*' + #13#10 +
      '	Name: ' + #13#10 +
      '	Copyright: ' + #13#10 +
      '	Author: ' + #13#10 +
      '	Date: ' + FormatDateTime('dd/mm/yy hh:nn', Now) + #13#10 +
      '	Description: ' + #13#10 +
      '*/' + #13#10, true);
  end;
end;

procedure TMainForm.EditorPageControlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y:
  Integer);
var
  PageIndex: integer;
  SenderPageControl: TPageControl;
  e: TEditor;
begin
  SenderPageControl := TPageControl(Sender);
  if Button = mbRight then begin // select new tab even with right mouse button
    PageIndex := SenderPageControl.IndexOfTabAt(X, Y);
    if PageIndex <> -1 then begin
      SenderPageControl.ActivePageIndex := PageIndex;
      SenderPageControl.OnChange(SenderPageControl);
    end;
  end else if Button = mbMiddle then begin
    PageIndex := SenderPageControl.IndexOfTabAt(X, Y);
    if PageIndex <> -1 then begin
      e := fEditorList.GetEditor(PageIndex, SenderPageControl);
      if Assigned(e) then
        fEditorList.CloseEditor(e);
    end;
  end else // see if it's a drag operation
    SenderPageControl.Pages[SenderPageControl.ActivePageIndex].BeginDrag(False);
end;

procedure TMainForm.actNewTemplateUpdate(Sender: TObject);
begin
  actNewTemplate.Enabled := Assigned(fProject);
end;

procedure TMainForm.actCommentExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    e.Text.CommandProcessor(TSynEditEx.ecComment, #0, nil);
end;

procedure TMainForm.actUncommentExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    e.Text.CommandProcessor(TSynEditEx.ecUncomment, #0, nil);
end;

procedure TMainForm.actToggleCommentExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    e.Text.CommandProcessor(TSynEditEx.ecToggleComment, #0, nil);
end;

procedure TMainForm.actIndentExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    e.IndentSelection;
end;

procedure TMainForm.actUnindentExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    e.UnindentSelection;
end;

procedure TMainForm.EditorPageControlDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  I: integer;
  SenderPageControl: TPageControl;
begin
  SenderPageControl := TPageControl(Sender);
  I := SenderPageControl.IndexOfTabAt(X, Y);
  if (Source is TTabSheet) and (I <> SenderPageControl.ActivePageIndex) then
    SenderPageControl.Pages[SenderPageControl.ActivePageIndex].PageIndex := I;
end;

procedure TMainForm.EditorPageControlDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept:
  Boolean);
var
  I: integer;
  SenderPageControl: TPageControl;
begin
  SenderPageControl := TPageControl(Sender);
  I := SenderPageControl.IndexOfTabAt(X, Y);
  Accept := (Source is TTabSheet) and (I <> SenderPageControl.ActivePageIndex);
end;

procedure TMainForm.actGotoFunctionExecute(Sender: TObject);
var
  e: TEditor;
  st: PStatement;
begin
  e := fEditorList.GetEditor;
  if not Assigned(e) then
    Exit;

  with TFunctionSearchForm.Create(Self) do try

    fFileName := e.FileName;
    fParser := CppParser;

    if ShowModal = mrOK then begin
      st := PStatement(lvEntries.Selected.Data);
      if Assigned(st) then begin
        if st^._HasDefinition then
          e.SetCaretPosAndActivate(st^._DefinitionLine, 1)
        else
          e.SetCaretPosAndActivate(st^._Line, 1);
      end;
    end;
  finally
    Free;
  end;
end;

procedure TMainForm.actBrowserGotoDeclarationUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(ClassBrowser.Selected);
end;

procedure TMainForm.actBrowserGotoDefinitionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(ClassBrowser.Selected);
end;

procedure TMainForm.actBrowserNewClassUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(fProject);
end;

procedure TMainForm.actBrowserNewMemberUpdate(Sender: TObject);
begin
  if Assigned(ClassBrowser.Selected) and Assigned(ClassBrowser.Selected.Data) then
    TCustomAction(Sender).Enabled := (PStatement(ClassBrowser.Selected.Data)^._Kind = skClass)
  else
    TCustomAction(Sender).Enabled := False;
end;

procedure TMainForm.actBrowserNewVarUpdate(Sender: TObject);
begin
  if Assigned(ClassBrowser.Selected) and Assigned(ClassBrowser.Selected.Data) then
    TCustomAction(Sender).Enabled := (PStatement(ClassBrowser.Selected.Data)^._Kind = skClass)
  else
    TCustomAction(Sender).Enabled := False;
end;

procedure TMainForm.actBrowserViewAllUpdate(Sender: TObject);
begin
  TCustomAction(Sender).Enabled := True;
end;

procedure TMainForm.actBrowserGotoDeclarationExecute(Sender: TObject);
var
  Editor: TEditor;
  Node: TTreeNode;
  FileName: String;
  Line: integer;
  Statement: PStatement;
begin
  Node := ClassBrowser.Selected;
  Statement := PStatement(Node.Data);
  FileName := Statement^._FileName;
  Line := Statement^._Line;
  Editor := fEditorList.GetEditorFromFileName(FileName);
  if Assigned(Editor) then begin
    Editor.SetCaretPosAndActivate(Line, 1);
  end;
end;

procedure TMainForm.actBrowserGotoDefinitionExecute(Sender: TObject);
var
  Editor: TEditor;
  Node: TTreeNode;
  FileName: String;
  Line: integer;
  Statement: PStatement;
begin
  Node := ClassBrowser.Selected;
  Statement := PStatement(Node.Data);
  FileName := Statement^._DefinitionFileName;
  Line := Statement^._DefinitionLine;
  Editor := fEditorList.GetEditorFromFileName(FileName);
  if Assigned(Editor) then begin
    Editor.SetCaretPosAndActivate(Line, 1);
  end;
end;

procedure TMainForm.actBrowserNewClassExecute(Sender: TObject);
begin
  with TNewClassForm.Create(Self) do try
    ShowModal;
  finally
    Close;
  end;
end;

procedure TMainForm.actBrowserNewMemberExecute(Sender: TObject);
begin
  with TNewFunctionForm.Create(Self) do try
    ShowModal;
  finally
    Close;
  end;
end;

procedure TMainForm.actBrowserNewVarExecute(Sender: TObject);
begin
  with TNewVarForm.Create(Self) do try
    ShowModal;
  finally
    Close;
  end;
end;

procedure TMainForm.actBrowserViewAllExecute(Sender: TObject);
var
  e: TEditor;
begin
  ClassBrowser.ShowFilter := sfAll;
  actBrowserViewAll.Checked := True;
  actBrowserViewProject.Checked := False;
  actBrowserViewCurrent.Checked := False;
  actBrowserViewIncludes.Checked := False;
  e := fEditorList.GetEditor;
  if Assigned(e) then
    ClassBrowser.CurrentFile := e.FileName
  else
    ClassBrowser.CurrentFile := '';
  devClassBrowsing.ShowFilter := Ord(ClassBrowser.ShowFilter);
end;

procedure TMainForm.actBrowserViewProjectExecute(Sender: TObject);
var
  e: TEditor;
begin
  ClassBrowser.ShowFilter := sfProject;
  actBrowserViewAll.Checked := False;
  actBrowserViewProject.Checked := True;
  actBrowserViewCurrent.Checked := False;
  actBrowserViewIncludes.Checked := False;
  e := fEditorList.GetEditor;
  if Assigned(e) then
    ClassBrowser.CurrentFile := e.FileName
  else
    ClassBrowser.CurrentFile := '';
  devClassBrowsing.ShowFilter := Ord(ClassBrowser.ShowFilter);
end;

procedure TMainForm.actBrowserViewCurrentExecute(Sender: TObject);
var
  e: TEditor;
begin
  ClassBrowser.ShowFilter := sfCurrent;
  actBrowserViewAll.Checked := False;
  actBrowserViewProject.Checked := False;
  actBrowserViewCurrent.Checked := True;
  actBrowserViewIncludes.Checked := False;
  e := fEditorList.GetEditor;
  if Assigned(e) then
    ClassBrowser.CurrentFile := e.FileName
  else
    ClassBrowser.CurrentFile := '';
  devClassBrowsing.ShowFilter := Ord(ClassBrowser.ShowFilter);
end;

procedure TMainForm.actBrowserViewIncludesExecute(Sender: TObject);
var
  e: TEditor;
begin
  ClassBrowser.ShowFilter := sfSystemFiles;
  actBrowserViewAll.Checked := False;
  actBrowserViewProject.Checked := False;
  actBrowserViewCurrent.Checked := False;
  actBrowserViewIncludes.Checked := True;
  e := fEditorList.GetEditor;
  if Assigned(e) then
    ClassBrowser.CurrentFile := e.FileName
  else
    ClassBrowser.CurrentFile := '';
  devClassBrowsing.ShowFilter := Ord(ClassBrowser.ShowFilter);
end;

procedure TMainForm.actProfileExecute(Sender: TObject);
var
  ProfilingEnabled, StripEnabled: boolean;
  path: String;
  e: TEditor;
begin
  case GetCompileTarget of
    ctProject: begin
        // Check if we enabled proper options
        ProfilingEnabled := fProject.GetCompilerOption('-pg') <> '0';
        StripEnabled := fProject.GetCompilerOption('-s') <> '0';

        // Ask the user if he wants to enable profiling
        if (not ProfilingEnabled or StripEnabled) and (MessageDlg(Lang[ID_MSG_NOPROFILE], mtConfirmation, [mbYes, mbNo],
          0) = mrYes) then begin

          // Enable profiling, disable stripping
          fProject.SetCompilerOption('-pg', '1');
          fProject.SetCompilerOption('-s', '0');

          fCompSuccessAction := csaProfile;
          actRebuildExecute(nil);
          Exit;
        end;
      end;
    ctFile: begin
        // Check if we enabled proper options
        with devCompilerSets.CompilationSet do begin
          ProfilingEnabled := GetOption('-pg') <> '0';
          StripEnabled := GetOption('-s') <> '0';
        end;

        // Ask the user if he wants to enable profiling
        if (not ProfilingEnabled or StripEnabled) and (MessageDlg(Lang[ID_MSG_NOPROFILE], mtConfirmation, [mbYes, mbNo],
          0) = mrYes) then begin

          // Enable profiling, disable stripping
          with devCompilerSets.CompilationSet do begin
            SetOption('-pg', '1');
            SetOption('-s', '0');
          end;

          // Save changes to compiler set
          devCompilerSets.SaveSet(devCompilerSets.CompilationSetIndex);

          fCompSuccessAction := csaProfile;
          actRebuildExecute(nil);
          Exit;
        end;
      end;
  end;

  // If we're done setting up options, check if there's profiling data already
  path := '';
  case GetCompileTarget of
    ctProject: begin
        path := ExtractFilePath(fProject.Executable) + GPROF_CHECKFILE;
      end;
    ctFile: begin
        e := fEditorList.GetEditor;
        if Assigned(e) then
          path := ExtractFilePath(ChangeFileExt(e.FileName, EXE_EXT)) + GPROF_CHECKFILE;
      end;
    ctNone: Exit;
  end;

  //Gather data by running our project...
  fRunEndAction := reaProfile;
  if not FileExists(path) then begin
    fRunEndAction := reaProfile;
    actRunExecute(nil);
  end else begin // If the data is there, open up the form
    RunEndProc;
  end;
end;

procedure TMainForm.actCloseAllButThisExecute(Sender: TObject);
begin
  ClassBrowser.BeginUpdate;
  try
    fEditorList.CloseAllButThis;
  finally
    ClassBrowser.EndUpdate;
  end;
end;

procedure TMainForm.lvBacktraceCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if not (cdsSelected in State) then begin
    if Assigned(Item.Data) then
      Sender.Canvas.Font.Color := clBlue
    else
      Sender.Canvas.Font.Color := clWindowText;
  end;
end;

procedure TMainForm.lvBacktraceMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  It: TListItem;
begin
  with Sender as TListView do begin
    It := GetItemAt(X, Y);
    if Assigned(It) and Assigned(It.Data) then
      Cursor := crHandPoint
    else
      Cursor := crDefault;
  end;
end;

procedure TMainForm.FileMonitorNotifyChange(Sender: TObject; ChangeType: TdevMonitorChangeType; Filename: String);
var
  e: TEditor;
  p: TBufferCoord;
begin
  // Deactivate monitoring for this file. One message is enough
  FileMonitor.BeginUpdate;
  try
    case ChangeType of
      mctChanged: begin
          Application.Restore;
          if MessageDlg(Format(Lang[ID_ERR_FILECHANGED], [Filename]), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
            begin
            e := fEditorList.GetEditorFromFileName(Filename);
            if Assigned(e) then begin
              p := e.Text.CaretXY;
              e.Text.Lines.LoadFromFile(Filename);
              if (p.Line <= e.Text.Lines.Count) then
                e.Text.CaretXY := p;
            end;
          end;
        end;
      mctDeleted: begin
          Application.Restore;
          case MessageDlg(Format(Lang[ID_ERR_RENAMEDDELETEDKEEP], [Filename]), mtInformation, [mbYes, mbNo], 0) of
            mrYes: begin
                e := fEditorList.FileIsOpen(Filename);
                if Assigned(e) then
                  e.Text.Modified := True;
              end;
            mrNo: begin
                e := fEditorList.FileIsOpen(Filename);
                if Assigned(e) then
                  fEditorList.CloseEditor(e);
              end;
          end;
        end;
    end;
  finally
    FileMonitor.EndUpdate;
  end;
end;

procedure TMainForm.actFilePropertiesExecute(Sender: TObject);
begin
  with TFilePropertiesForm.Create(Self) do begin
    if TAction(Sender).ActionComponent = mnuUnitProperties then
      if Assigned(fProject) and
        Assigned(ProjectView.Selected) and
        (ProjectView.Selected.Data <> Pointer(-1)) then
        SetFile(fProject.Units[integer(ProjectView.Selected.Data)].FileName);
    ShowModal;
  end;
end;

procedure TMainForm.actViewToDoListExecute(Sender: TObject);
begin
  TViewToDoForm.Create(Self).ShowModal;
end;

procedure TMainForm.actAddToDoExecute(Sender: TObject);
begin
  TAddToDoForm.Create(Self).ShowModal;
end;

procedure TMainForm.actProjectNewFolderExecute(Sender: TObject);
var
  fp, S: String;
begin
  S := 'New folder';
  if InputQuery(Lang[ID_POP_ADDFOLDER], Lang[ID_MSG_ADDBROWSERFOLDER], S) and (S <> '') then begin
    fp := fProject.GetFolderPath(ProjectView.Selected);
    if fp <> '' then
      fProject.AddFolder(fp + '/' + S)
    else
      fProject.AddFolder(S);
  end;
end;

procedure TMainForm.actProjectRemoveFolderExecute(Sender: TObject);
begin
  // Useless check...
  if not Assigned(fProject) then
    exit;

  // Check if the current item is a folder
  if Assigned(ProjectView.Selected) and (ProjectView.Selected.Data = Pointer(-1)) then

    // Ask if we want to remove...
    if MessageDlg(Lang[ID_MSG_REMOVEBROWSERFOLDER], mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
      ProjectView.Items.BeginUpdate;
      try
        fProject.RemoveFolder(ProjectView.Selected);
      finally
        ProjectView.Items.EndUpdate;
      end;
    end;
end;

procedure TMainForm.actProjectRenameFolderExecute(Sender: TObject);
var
  S: String;
begin
  if Assigned(ProjectView.Selected) and (ProjectView.Selected.Data = Pointer(-1)) then begin
    S := ProjectView.Selected.Text;
    if InputQuery(Lang[ID_POP_RENAMEFOLDER], Lang[ID_MSG_RENAMEBROWSERFOLDER], S) and (S <> '') then begin
      ProjectView.Selected.Text := S;
      fProject.UpdateFolders;
    end;
  end;
end;

{ begin XXXKF changed }

procedure TMainForm.ProjectViewDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  Item: TTreeNode;
  Hits: THitTests;
begin
  Hits := ProjectView.GetHitTestInfoAt(X, Y);
  if ([htOnItem, htOnRight, htToRight] * Hits) <> [] then
    Item := ProjectView.GetNodeAt(X, Y)
  else
    Item := nil;
  Accept :=
    (
    (Sender = ProjectView) and Assigned(ProjectView.Selected) and Assigned(Item) //and
    // drop node is a folder or the project
    and ((Item.Data = Pointer(-1)) or (Item.SelectedIndex = 0))
    );
end;

procedure TMainForm.ProjectViewDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  Item: TTreeNode;
  srcNode: TTreeNode;
  WasExpanded: Boolean;
  I: integer;
begin
  if not (Source is TTreeView) then
    exit;
  if ([htOnItem, htOnRight, htToRight] * ProjectView.GetHitTestInfoAt(X, Y)) <> [] then
    Item := ProjectView.GetNodeAt(X, Y)
  else
    Item := nil;
  for I := 0 to ProjectView.SelectionCount - 1 do begin
    srcNode := ProjectView.Selections[I];
    if not Assigned(Item) then begin
      fProject.Units[Integer(srcNode.Data)].Folder := '';
      srcNode.MoveTo(ProjectView.Items[0], naAddChild);
    end else begin
      if srcNode.Data <> Pointer(-1) then begin
        if Item.Data = Pointer(-1) then
          fProject.Units[Integer(srcNode.Data)].Folder := fProject.GetFolderPath(Item)
        else
          fProject.Units[Integer(srcNode.Data)].Folder := '';
      end;
      WasExpanded := Item.Expanded;
      ProjectView.Items.BeginUpdate;
      srcNode.MoveTo(Item, naAddChild);
      if not WasExpanded then
        Item.Collapse(False);
      Item.AlphaSort(False);
      ProjectView.Items.EndUpdate;

      fProject.UpdateFolders;
    end;
  end;
end;

procedure TMainForm.actImportMSVCExecute(Sender: TObject);
begin
  with TImportMSVCForm.Create(Self) do begin
    if ShowModal = mrOK then
      OpenProject(GetFilename);
  end;
end;

procedure TMainForm.ViewCPUItemClick(Sender: TObject);
begin
  if not Assigned(CPUForm) then begin
    CPUForm := TCPUForm.Create(self);
    CPUForm.Show;
  end;
end;

procedure TMainForm.edGdbCommandKeyPress(Sender: TObject; var Key: Char);
begin
  if fDebugger.Executing then begin
    if Key = Chr(VK_RETURN) then begin
      if Length(edGDBCommand.Text) > 0 then begin

        // Disable key, but make sure not to remove selected text
        EvaluateInput.SelStart := 0;
        EvaluateInput.SelLength := 0;
        Key := #0;

        fDebugger.SendCommand(edGDBCommand.Text, '');

        if edGDBCommand.Items.IndexOf(edGDBCommand.Text) = -1 then
          edGDBCommand.AddItem(edGDBCommand.Text, nil);
      end;
    end;

    // View command examples only when edit is empty or when the UI itself added the current command
    fDebugger.CommandChanged := true;
  end;
end;

procedure TMainForm.CheckForDLLProfiling;
var
  prof: boolean;
begin
  if not Assigned(fProject) then
    Exit;

  // If this is a DLL / static link library project, then check for profiling
  if (fProject.Options.typ in [dptDyn, dptStat]) then begin

    // Check if profiling is enabled
    prof := fProject.GetCompilerOption('-pg') <> '0';

    // Check for the existence of "-lgmon" in project's linker options
    if prof and (Pos('-lgmon', fProject.Options.LinkerCmd) = 0) then begin

      // Warn the user that we should update its project options and include
      // -lgmon in linker options, or else the compilation will fail
      if MessageDlg('You have profiling enabled in Compiler Options and you are ' +
        'working on a dynamic or static library. Do you want to add a special ' +
        'linker option in your project to allow compilation with profiling enabled? ' +
        'If you choose No, and you do not disable profiling from the Compiler Options ' +
        'chances are that your library''s compilation will fail...', mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
        fProject.Options.LinkerCmd := fProject.Options.LinkerCmd + ' -lgmon';
        fProject.Modified := True;
      end;
    end;
  end;
end;

procedure TMainForm.actExecParamsExecute(Sender: TObject);
begin
  with TParamsForm.Create(self) do try
    case GetCompileTarget of
      ctNone, ctFile:
        ParamEdit.Text := fCompiler.RunParams;
      ctProject: begin
          HostEdit.Text := fProject.Options.HostApplication;
          ParamEdit.Text := fProject.Options.CmdLineArgs;
        end;
    end;
    if not Assigned(fProject) or (fProject.Options.typ <> dptDyn) then
      DisableHost;
    if (ShowModal = mrOk) then begin
      case GetCompileTarget of
        ctNone, ctFile:
          fCompiler.RunParams := ParamEdit.Text;
        ctProject: begin
            if (HostEdit.Enabled) then
              fProject.Options.HostApplication := HostEdit.Text;
            fProject.Options.CmdLineArgs := ParamEdit.Text;
            fProject.Modified := true;
          end;
      end;
    end;
  finally
    Free;
  end;
end;

procedure TMainForm.DevCppDDEServerExecuteMacro(Sender: TObject; Msg: TStrings);
var
  filename: String;
  i, n: Integer;
begin
  if Msg.Count > 0 then begin
    for i := 0 to Msg.Count - 1 do begin
      filename := Msg[i];
      if Pos('[Open(', filename) = 1 then begin
        n := Pos('"', filename);
        if n > 0 then begin
          Delete(filename, 1, n);
          n := Pos('"', filename);
          if n > 0 then
            Delete(filename, n, maxint);
          try
            OpenFile(filename);
          except
          end;
        end;
      end;
    end;
    Application.BringToFront;
  end;
end;

procedure TMainForm.actShowTipsExecute(Sender: TObject);
begin
  with TTipOfTheDayForm.Create(Self) do
    ShowModal;
end;

procedure TMainForm.CppParserStartParsing(Sender: TObject);
var
  e: TEditor;
begin
  // Update UI before the parser starts working
  Application.ProcessMessages;

  // Loading...
  Screen.Cursor := crHourglass;

  // Pause updating the class browser (which will be invalidated)
  ClassBrowser.BeginUpdate;

  // Hide anything that uses the database (which will be invalidated)
  e := fEditorList.GetEditor;
  if Assigned(e) then begin
    e.FunctionTip.ReleaseHandle;
    e.CompletionBox.Hide;
  end;

  fParseStartTime := GetTickCount;
end;

procedure TMainForm.CppParserTotalProgress(Sender: TObject; const FileName: string; Total, Current: Integer);
var
  ShowStep: Integer;
begin
  // Mention every 5% progress
  ShowStep := Total div 20;

  // For Total = 1, avoid division by zero
  if ShowStep = 0 then
    ShowStep := 1;

  // Only show if needed (it's a very slow operation)
  if (Current mod ShowStep = 0) or (Current = 1) then begin
    SetStatusBarMessage(Format(Lang[ID_PARSINGFILECOUNT], [Current, Total, Filename]));
    Application.ProcessMessages;
  end;
end;

procedure TMainForm.CppParserEndParsing(Sender: TObject; Total: Integer);
var
  ParseTimeFloat, ParsingFrequency: Extended;
begin
  Screen.Cursor := crDefault;

  // Done. Rebuild stuff that uses the database
  RebuildClassesToolbar;

  // The class browser can redraw again
  ClassBrowser.EndUpdate;

  // do this work only if this was the last file scanned
  ParseTimeFloat := (GetTickCount - fParseStartTime) / 1000;
  if Total > 1 then begin
    if ParseTimeFloat <> 0 then
      ParsingFrequency := Total / ParseTimeFloat
    else
      ParsingFrequency := 999;
    SetStatusbarMessage(Format(Lang[ID_DONEPARSINGINCOUNT], [Total, ParseTimeFloat, ParsingFrequency]))
  end else
    SetStatusbarMessage(Format(Lang[ID_DONEPARSINGIN], [ParseTimeFloat]));
end;

procedure TMainForm.UpdateAppTitle;
var
  e: TEditor;
begin
  if Assigned(fProject) then begin
    if fDebugger.Executing then begin
      Caption := Format('%s - [%s] - [Debugging] - %s %s',
        [fProject.Name, ExtractFilename(fProject.Filename), DEVCPP, DEVCPP_VERSION]);
      Application.Title := Format('%s - [Debugging] - %s', [fProject.Name, DEVCPP]);
    end else if devExecutor.Running then begin
      Caption := Format('%s - [%s] - [Executing] - %s %s',
        [fProject.Name, ExtractFilename(fProject.Filename), DEVCPP, DEVCPP_VERSION]);
      Application.Title := Format('%s - [Executing] - %s', [fProject.Name, DEVCPP]);
    end else if fCompiler.Compiling then begin
      Caption := Format('%s - [%s] - [Compiling] - %s %s',
        [fProject.Name, ExtractFilename(fProject.Filename), DEVCPP, DEVCPP_VERSION]);
      Application.Title := Format('%s - [Compiling] - %s', [fProject.Name, DEVCPP]);
    end else begin
      Caption := Format('%s - [%s] - %s %s',
        [fProject.Name, ExtractFilename(fProject.Filename), DEVCPP, DEVCPP_VERSION]);
      Application.Title := Format('%s - %s', [fProject.Name, DEVCPP]);
    end;
  end else begin
    e := fEditorList.GetEditor;
    if Assigned(e) then begin
      if fDebugger.Executing then begin
        Caption := Format('%s - [Debugging] - %s %s', [e.FileName, DEVCPP, DEVCPP_VERSION]);
        Application.Title := Format('%s - [Debugging] - %s', [ExtractFileName(e.FileName), DEVCPP]);
      end else if devExecutor.Running then begin
        Caption := Format('%s - [Executing] - %s %s', [e.FileName, DEVCPP, DEVCPP_VERSION]);
        Application.Title := Format('%s - [Executing] - %s', [ExtractFileName(e.FileName), DEVCPP]);
      end else if fCompiler.Compiling then begin
        Caption := Format('%s - [Compiling] - %s %s', [e.FileName, DEVCPP, DEVCPP_VERSION]);
        Application.Title := Format('%s - [Compiling] - %s', [ExtractFileName(e.FileName), DEVCPP]);
      end else begin
        Caption := Format('%s - %s %s', [e.FileName, DEVCPP, DEVCPP_VERSION]);
        Application.Title := Format('%s - %s', [ExtractFileName(e.FileName), DEVCPP]);
      end;
    end else begin
      Caption := Format('%s %s', [DEVCPP, DEVCPP_VERSION]);
      Application.Title := Format('%s', [DEVCPP]);
    end;
  end;
end;

procedure TMainForm.actAbortCompilationUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := fCompiler.Compiling;
end;

procedure TMainForm.actAbortCompilationExecute(Sender: TObject);
begin
  fCompiler.AbortThread;
end;

{ begin XXXKF }

procedure TMainForm.actWindowMenuExecute(Sender: TObject);
var
  Item: TMenuItem;
  E: TEditor;
  i: integer;
  Act: TAction;
begin
  // Remove old items
  while WindowMenu.Count > 8 do begin
    Item := WindowMenu.Items[7];
    WindowMenu.Remove(Item);
    Item.Destroy;
  end;

  // Add new items, 9 at most
  for i := 0 to min(9, pred(EditorList.PageCount)) do begin
    e := fEditorList[i];
    if Assigned(e) then begin
      Item := TMenuItem.Create(self);

      // Create an action that executes when clicking on the item
      Act := TAction.Create(Item);
      Act.Name := 'dynactOpenEditorByTag';
      Act.Tag := Integer(e);
      Act.OnExecute := actOpenEditorByTagExecute;

      // Attach it to the new menu item
      Item.Action := Act;
      Item.Caption := '&' + IntToStr(i) + ' ' + e.FileName;
      if e.Text.Modified then
        Item.Caption := Item.Caption + ' *';
      WindowMenu.Insert(WindowMenu.Count - 1, Item);
    end;
  end;
end;

procedure TMainForm.RemoveItem(Node: TTreeNode);
begin
  if Assigned(Node) and (Node.Level >= 1) then begin
    if Node.Data = Pointer(-1) then
      actProjectRemoveFolderExecute(nil)
    else
      fProject.RemoveEditor(integer(Node.Data), true);
  end;
end;

procedure TMainForm.ProjectViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_DELETE) and Assigned(ProjectView.Selected) then
    RemoveItem(ProjectView.Selected)
  else if (Key = VK_ESCAPE) and (Shift = []) then begin
    if (EditorList.PageCount > 0) then
      fEditorList.GetEditor(-1).Activate;
  end else if (Key = VK_ESCAPE) and (Shift = [ssShift]) then begin
    actProjectManager.Checked := False;
    actProjectManagerExecute(nil);
    if (EditorList.PageCount > 0) then
      fEditorList.GetEditor(-1).Activate;
  end else if (Key = VK_RETURN) and Assigned(ProjectView.Selected) then begin
    if ProjectView.Selected.Data <> Pointer(-1) then begin { if not a directory }
      OpenUnit;
      if Shift = [ssShift] then begin
        {
         crap hack, SHIFT+ENTER=open file and close projman
         I *really* don't think it's the acceptable interface idea.
         Can't find a better one though.
        }
        actProjectManager.Checked := False;
        actProjectManagerExecute(nil);
      end;
    end;
  end;
end;

procedure TMainForm.ProjectViewChanging(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
begin
  Node.MakeVisible;
end;

procedure TMainForm.actOpenEditorByTagExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditorFromTag(TAction(Sender).Tag);
  if Assigned(e) then
    e.Activate;
end;

{ end XXXKF }

procedure TMainForm.actGotoProjectManagerExecute(Sender: TObject);
begin
  if not actProjectManager.Checked then begin
    actProjectManager.Checked := True;
    actProjectManagerExecute(nil);
  end;
  LeftPageControl.ActivePageIndex := 0;
  ProjectView.SetFocus;
end;

procedure TMainForm.ListItemClick(Sender: TObject);
var
  I: integer;
  e: TEditor;
  item: TListItem;
begin
  with TWindowListForm.Create(self) do try
    // Fill here instead of in Window itself?
    UnitList.Items.BeginUpdate;
    try
      for I := 0 to fEditorList.PageCount - 1 do begin
        e := fEditorList[i];

        item := UnitList.Items.Add;
        item.Caption := ExtractFileName(e.FileName);
        item.SubItems.Add(e.FileName);
      end;
    finally
      UnitList.Items.EndUpdate;
    end;

    if (ShowModal = mrOk) and (UnitList.ItemIndex <> -1) then begin
      e := fEditorList[UnitList.ItemIndex];
      if Assigned(e) then
        e.Activate;
    end;
  finally
    Close;
  end;
end;

procedure TMainForm.ProjectViewCompare(Sender: TObject; Node1,
  Node2: TTreeNode; Data: Integer; var Compare: Integer);
begin
  if (Node1.Data = pointer(-1)) and (Node2.Data = pointer(-1)) then
    Compare := CompareStr(Node1.Text, Node2.Text)
  else if Node1.Data = pointer(-1) then
    Compare := -1
  else if Node2.Data = pointer(-1) then
    Compare := +1
  else
    Compare := CompareStr(Node1.Text, Node2.Text);
end;

procedure TMainForm.ProjectViewKeyPress(Sender: TObject; var Key: Char);
begin
  // fixs an annoying bug/behavior of the tree ctrl (a beep on enter key)
  if Key = #13 then
    Key := #0;
end;

procedure TMainForm.ProjectViewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // bug-fix: when *not* clicking on an item, re-opens the last clicked file-node
  // this was introduced in the latest commit by XXXKF (?)
  if (Button = mbLeft) and not (htOnItem in ProjectView.GetHitTestInfoAt(X, Y)) then
    ProjectView.Selected := nil;
end;

procedure TMainForm.GoToClassBrowserItemClick(Sender: TObject);
begin
  if not actProjectManager.Checked then begin
    actProjectManager.Checked := True;
    actProjectManagerExecute(nil);
  end;
  LeftPageControl.ActivePageIndex := 1;
  if ClassBrowser.Visible then
    ClassBrowser.SetFocus;
end;

procedure TMainForm.actBrowserShowInheritedExecute(Sender: TObject);
begin
  actBrowserShowInherited.Checked := not actBrowserShowInherited.Checked;
  devClassBrowsing.ShowInheritedMembers := actBrowserShowInherited.Checked;
  ClassBrowser.ShowInheritedMembers := actBrowserShowInherited.Checked;
  ClassBrowser.Refresh;
end;

procedure TMainForm.ProjectWindowClose(Sender: TObject; var Action: TCloseAction);
begin
  FloatingPojectManagerItem.Checked := False;

  LeftPageControl.Align := alLeft;
  LeftPageControl.Visible := actProjectManager.Checked;
  LeftPageControl.Parent := Self;

  fProjectToolWindow.Free;
  fProjectToolWindow := nil;

  if assigned(fProject) then
    fProject.SetNodeValue(ProjectView.TopItem); // nodes needs to be recreated

end;

procedure TMainForm.ReportWindowClose(Sender: TObject; var Action: TCloseAction);
begin
  FloatingReportWindowItem.Checked := False;

  MessageControl.Align := alBottom;
  MessageControl.Parent := Self;
  MessageControl.Top := SplitterBottom.Top;
  Statusbar.Top := MessageControl.Top + MessageControl.Height;

  fReportToolWindow.Free;
  fReportToolWindow := nil;
end;

procedure TMainForm.FloatingPojectManagerItemClick(Sender: TObject);
begin
  FloatingPojectManagerItem.Checked := not FloatingPojectManagerItem.Checked;

  if Assigned(fProjectToolWindow) then
    fProjectToolWindow.Close
  else begin
    fProjectToolWindow := TForm.Create(self);
    fProjectToolWindow.Caption := Lang[ID_TB_PROJECT];
    fProjectToolWindow.FormStyle := fsStayOnTop;
    fProjectToolWindow.OnClose := ProjectWindowClose;
    fProjectToolWindow.BorderStyle := bsSizeable;
    fProjectToolWindow.BorderIcons := [biSystemMenu];
    var SPos := LeftPageControl.ClientToScreen(TPoint.Zero);
    var LeftPageControlW := LeftPageControl.Width;
    var LeftPageControlH := LeftPageControl.Height;

    LeftPageControl.Parent := fProjectToolWindow;
    LeftPageControl.Align := alClient;
    LeftPageControl.Visible := True;

    fProjectToolWindow.Show;
    fProjectToolWindow.Left := SPos.X;
    fProjectToolWindow.Top := SPos.Y;
    fProjectToolWindow.ClientWidth := LeftPageControlW;
    fProjectToolWindow.ClientHeight := LeftPageControlH;

    if assigned(fProject) then
      fProject.SetNodeValue(ProjectView.TopItem); // nodes needs to be recreated
  end;
end;

procedure TMainForm.actSaveProjectAsExecute(Sender: TObject);
begin
  if not Assigned(fProject) then
    Exit;

  with TSaveDialog.Create(nil) do try

    Filter := FLT_PROJECTS;
    Options := Options + [ofOverwritePrompt];
    Title := Lang[ID_NV_SAVEPROJECT];

    if Execute then begin
      fProject.FileName := FileName;
      fProject.SaveAll;
      UpdateAppTitle;
    end;
  finally
    Free;
  end;
end;

procedure TMainForm.BuildOpenWith;
var
  idx, idx2: integer;
  item: TMenuItem;
  ext, s, s1: String;
begin
  mnuOpenWith.Clear;
  if not assigned(fProject) then
    exit;
  if not assigned(ProjectView.Selected) or
    (ProjectView.Selected.Level < 1) then
    exit;
  if ProjectView.Selected.Data = Pointer(-1) then
    Exit;
  idx := integer(ProjectView.Selected.Data);

  ext := ExtractFileExt(fProject.Units[idx].FileName);
  idx2 := devExternalPrograms.AssignedProgram(ext);
  if idx2 <> -1 then begin
    item := TMenuItem.Create(nil);
    item.Caption := ExtractFilename(devExternalPrograms.ProgramName[idx2]);
    item.Tag := idx2;
    item.OnClick := mnuOpenWithClick;
    mnuOpenWith.Add(item);
  end;
  if GetAssociatedProgram(ext, s, s1) then begin
    item := TMenuItem.Create(nil);
    item.Caption := '-';
    item.Tag := -1;
    item.OnClick := mnuOpenWithClick;
    mnuOpenWith.Add(item);
    item := TMenuItem.Create(nil);
    item.Caption := s1;
    item.Tag := -1;
    item.OnClick := mnuOpenWithClick;
    mnuOpenWith.Add(item);
  end;
end;

procedure TMainForm.mnuOpenWithClick(Sender: TObject);
var
  idx, idx2: integer;
  item: TMenuItem;
  e: TEditor;
begin
  if (Sender = mnuOpenWith) and (mnuOpenWith.Count > 0) then
    Exit;
  if not Assigned(fProject) then
    Exit;
  if not assigned(ProjectView.Selected) or
    (ProjectView.Selected.Level < 1) then
    exit;
  if ProjectView.Selected.Data = Pointer(-1) then
    Exit;
  idx2 := integer(ProjectView.Selected.Data);

  item := TMenuItem(Sender);
  if item = mnuOpenWith then begin
    idx := -2;
    with TOpenDialog.Create(Self) do try
      Filter := FLT_ALLFILES;
      if Execute then
        idx := devExternalPrograms.AddProgram(ExtractFileExt(fProject.Units[idx2].FileName), Filename);
    finally
      Free;
    end;
  end else
    idx := item.Tag;

  e := fEditorList.FileIsOpen(fProject.Units[idx2].FileName, TRUE);
  if Assigned(e) then
    fEditorList.CloseEditor(e);

  if idx > -1 then // devcpp-based
    ShellExecute(0, 'open',
      PChar(devExternalPrograms.ProgramName[idx]),
      PChar(fProject.Units[idx2].FileName),
      PChar(ExtractFilePath(fProject.Units[idx2].FileName)),
      SW_SHOW)
      // idx=-2 means we prompted the user for a program, but didn't select one
  else if idx <> -2 then // registry-based
    ShellExecute(0, 'open',
      PChar(fProject.Units[idx2].FileName),
      nil,
      PChar(ExtractFilePath(fProject.Units[idx2].FileName)),
      SW_SHOW);
end;

procedure TMainForm.RebuildClassesToolbar;
var
  Node: PStatementNode;
  Statement: PStatement;
  OldSelection: String;
begin
  OldSelection := cmbClasses.Text;

  cmbClasses.Items.BeginUpdate;
  try
    cmbClasses.Clear;

    // Add globals list
    cmbClasses.Items.AddObject('(globals)', nil);

    // Add all classes inside the current project
    Node := CppParser.Statements.FirstNode;
    while Assigned(Node) do begin
      Statement := Node^.Data;
      if Statement^._InProject and (Statement^._Kind = skClass) then // skClass equals struct + typedef struct + class
        cmbClasses.Items.AddObject(Statement^._Command, Pointer(Statement));
      Node := Node^.NextNode;
    end;

    // if we can't find the old selection anymore (-> -1), default to 0 (globals)
    cmbClasses.ItemIndex := max(0, cmbClasses.Items.IndexOf(OldSelection));
    cmbClassesChange(cmbClasses);
  finally
    cmbClasses.Items.EndUpdate;
  end;
end;

procedure TMainForm.cmbClassesChange(Sender: TObject);
var
  Node: PStatementNode;
  Statement, ParentStatement: PStatement;
  procedure AddStatementKind(AddKind: TStatementKind);
  begin
    Node := CppParser.Statements.FirstNode;
    while Assigned(Node) do begin
      Statement := Node^.Data;
      if (Statement^._Parent = ParentStatement) and Statement^._InProject and (Statement^._Kind = AddKind) then
        cmbMembers.Items.AddObject(CppParser.StatementKindStr(AddKind) + ' ' + Statement^._Command + Statement^._Args +
          ' : ' + Statement^._Type,
          Pointer(Statement));
      Node := Node^.NextNode;
    end;
  end;
begin
  cmbMembers.Items.BeginUpdate;
  try
    cmbMembers.Clear;

    // Select what parent we need to display info for
    if cmbClasses.ItemIndex > 0 then
      ParentStatement := PStatement(cmbClasses.Items.Objects[cmbClasses.ItemIndex]) // any class
    else if cmbClasses.ItemIndex = 0 then
      ParentStatement := nil // '(globals)'
    else
      Exit; // -1?

    // Show functions that belong to this scope or class/struct
    AddStatementKind(skConstructor);
    AddStatementKind(skDestructor);
    AddStatementKind(skFunction);
  finally
    cmbMembers.Items.EndUpdate;
  end;
end;

procedure TMainForm.cmbMembersChange(Sender: TObject);
var
  Statement: PStatement;
  Line: Integer;
  e: TEditor;
  FileName: String;
begin
  if cmbMembers.ItemIndex = -1 then // sometimes happens too?
    Exit;

  Statement := PStatement(cmbMembers.Items.Objects[cmbMembers.ItemIndex]);
  if not Assigned(Statement) then
    Exit;

  // Go to definition if applicable
  Line := Statement^._DefinitionLine;
  FileName := Statement^._DefinitionFileName;

  e := fEditorList.GetEditorFromFilename(FileName);
  if Assigned(e) then begin
    e.SetCaretPosAndActivate(Line, 1);
  end;
end;

procedure TMainForm.CompilerOutputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    CompilerOutputDblClick(sender);
end;

procedure TMainForm.FindOutputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    FindOutputDblClick(sender);
end;

procedure TMainForm.DebugViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
    actRemoveWatchExecute(sender);
end;

procedure TMainForm.DebugPopupPopup(Sender: TObject);
begin
  RemoveWatchPop.Enabled := Assigned(DebugView.Selected);
end;

procedure TMainForm.FloatingReportwindowItemClick(Sender: TObject);
begin
  FloatingReportWindowItem.Checked := not FloatingReportWindowItem.Checked;
  if assigned(fReportToolWindow) then
    fReportToolWindow.Close
  else begin
    OpenCloseMessageSheet(true);
    if MessageControl.ActivePage = CloseSheet then
      MessageControl.ActivePageIndex := 0;

    fReportToolWindow := TForm.Create(self);
    fReportToolWindow.Caption := Lang[ID_TB_REPORT];
    fReportToolWindow.FormStyle := fsStayOnTop;
    fReportToolWindow.OnClose := ReportWindowClose;
    fReportToolWindow.BorderStyle := bsSizeable;
    fReportToolWindow.BorderIcons := [biSystemMenu];

    var SPos := MessageControl.ClientToScreen(TPoint.Zero);
    var MessageControlW := MessageControl.Width;
    var MessageControlH := MessageControl.Height;

    MessageControl.Parent := fReportToolWindow;
    MessageControl.Align := TAlign.alClient;

    fReportToolWindow.Show;

    fReportToolWindow.Left := SPos.X;
    fReportToolWindow.Top := SPos.Y;
    fReportToolWindow.ClientWidth := MessageControlW;
    fReportToolWindow.ClientHeight := MessageControlH;
  end;
end;

procedure TMainForm.actAttachProcessUpdate(Sender: TObject);
begin
  if assigned(fProject) and (fProject.Options.typ = dptDyn) then begin
    TCustomAction(Sender).Visible := true;
    TCustomAction(Sender).Enabled := not devExecutor.Running;
  end else
    TCustomAction(Sender).Visible := false;
end;

procedure TMainForm.actAttachProcessExecute(Sender: TObject);
var
  s: String;
begin
  PrepareDebugger;
  if assigned(fProject) then begin
    if not FileExists(fProject.Executable) then begin
      MessageDlg(Lang[ID_ERR_PROJECTNOTCOMPILED], mtWarning, [mbOK], 0);
      exit;
    end;

    try
      ProcessListForm := TProcessListForm.Create(self);
      if (ProcessListForm.ShowModal = mrOK) and (ProcessListForm.ProcessCombo.ItemIndex > -1) then begin
        s := IntToStr(integer(ProcessListForm.ProcessList[ProcessListForm.ProcessCombo.ItemIndex]));

        fDebugger.Start;
        fDebugger.SendCommand('file', '"' + StringReplace(fProject.Executable, '\', '\\', [rfReplaceAll]) + '"');
        fDebugger.SendCommand('attach', s);
      end
    finally
      ProcessListForm.Free;
    end;
  end;
end;

procedure TMainForm.actModifyWatchExecute(Sender: TObject);
var
  curnode: TTreeNode;
  fullname: String;
  value: String;

  function GetNodeName(node: TTreeNode): String;
  var
    epos: integer;
  begin
    Result := '';
    epos := Pos(' = ', node.Text);
    if epos > 0 then
      Result := Copy(node.Text, 1, epos - 1);
  end;

  function GetNodeValue(node: TTreeNode): String;
  var
    epos: integer;
  begin
    Result := '';
    epos := Pos(' = ', node.Text);
    if epos > 0 then
      Result := Copy(node.Text, epos + 3, Length(node.Text) - epos);
  end;

begin
  curnode := DebugView.Selected;
  if Assigned(curnode) then begin // only edit members

    fullname := GetNodeName(curnode);

    // Assemble full name including parents
    while Assigned(curnode.Parent) do begin
      curnode := curnode.Parent;
      if not StartsStr('{', GetNodeName(curnode)) then
        fullname := GetNodeName(curnode) + '.' + fullname;
    end;

    value := GetNodeValue(DebugView.Selected);

    if InputQuery(Lang[ID_NV_MODIFYVALUE], fullname, value) then
      fDebugger.SendCommand('set variable', fullname + ' = ' + value, true);
  end;
end;

procedure TMainForm.actModifyWatchUpdate(Sender: TObject);
begin
  TCustomAction(Sender).Enabled := Assigned(DebugView.Selected) and fDebugger.Executing;
end;

procedure TMainForm.ClearallWatchPopClick(Sender: TObject);
begin
  fDebugger.DeleteWatchVars(true);
end;

procedure TMainForm.actDeleteProfileExecute(Sender: TObject);
var
  path: String;
  e: TEditor;
begin
  path := '';
  case GetCompileTarget of
    ctProject: begin
        path := ExtractFilePath(fProject.Executable) + GPROF_CHECKFILE;
      end;
    ctFile: begin
        e := fEditorList.GetEditor;
        if Assigned(e) then
          path := ExtractFilePath(ChangeFileExt(e.FileName, EXE_EXT)) + GPROF_CHECKFILE;
      end;
    ctNone: Exit;
  end;

  if path <> '' then begin
    if DeleteFile(PChar(path)) then begin
      SetStatusbarMessage(Format(Lang[ID_DELETEDPROFDATA], [path]));
    end else
      SetStatusbarMessage(Format(Lang[ID_COULDNOTFINDPROFDATA], [path]));
  end;
end;

procedure TMainForm.actGotoImplDeclEditorExecute(Sender: TObject);
var
  statement: PStatement;
  filename, phrase: String;
  line: integer;
  M: TMemoryStream;
  e: TEditor;
begin
  e := fEditorList.GetEditor;

  if Assigned(e) then begin

    // Exit early, don't bother creating a stream (which is slow)
    phrase := e.GetWordAtPosition(e.Text.CaretXY, wpInformation);
    if Phrase = '' then
      Exit;

    // When searching using menu shortcuts, the caret is set to the proper place
    // When searching using ctrl+click, the cursor is set properly too, so do NOT use WordAtMouse
    M := TMemoryStream.Create;
    try
      e.Text.Lines.SaveToStream(M);
      statement := CppParser.FindStatementOf(
        e.FileName,
        phrase, e.Text.CaretY, M);
    finally
      M.Free;
    end;

    // Otherwise scan the returned class browser statement
    if Assigned(statement) then begin

      // If the caret position was used
      if Sender = e then begin // Ctrl+Click from current editor

        // Switching between declaration and definition
        if (e.Text.CaretY = statement^._DefinitionLine) and SameFileName(e.FileName, statement^._DefinitionFileName) then
          begin // clicked on declaration, move to definition
          filename := statement^._FileName;
          line := statement^._Line;
        end else begin // clicked anywhere, go to declaration
          filename := statement^._DefinitionFileName;
          line := statement^._DefinitionLine;
        end;

        // Menu item or mouse cursor
      end else begin

        // Switching between declaration and definition
        if Pos('Decl', TCustomAction(Sender).Name) > 0 then begin
          filename := statement^._FileName;
          line := statement^._Line;
        end else begin
          filename := statement^._DefinitionFileName;
          line := statement^._DefinitionLine;
        end;
      end;

      // Go to the location
      e := fEditorList.GetEditorFromFileName(filename);
      if Assigned(e) then begin
        e.SetCaretPosAndActivate(line, 1);
      end;

      SetStatusbarMessage('');
    end else
      SetStatusbarMessage(Format(Lang[ID_INFONOTFOUND], [phrase]));
  end;
end;

procedure TMainForm.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var
  Handled: Boolean);
var
  e: TEditor;
  State: TKeyboardState;
  I: integer;
begin

  // Check if control is pressed
  GetKeyboardState(State);
  e := fEditorList.GetEditor;

  // Check if we're focussing on an editor and holding ctrl
  if Assigned(e) and (Screen.ActiveControl = e.Text) and ((State[vk_Control] and 128) <> 0) then begin

    for I := 0 to pred(EditorList.PageCount) do begin
      e := fEditorList[I];

      // If so, set the font size of all editors...
      if WheelDelta > 0 then begin
        e.Text.Font.Size := Max(1, e.Text.Font.Size + 1);
        e.Text.Gutter.Font.Size := Max(1, e.Text.Gutter.Font.Size + 1);
      end else begin
        e.Text.Font.Size := Max(1, e.Text.Font.Size - 1);
        e.Text.Gutter.Font.Size := Max(1, e.Text.Gutter.Font.Size - 1);
      end;
    end;

    // And set the corresponding option
    if WheelDelta > 0 then begin
      devEditor.Font.Size := Max(1, devEditor.Font.Size + 1);
      devEditor.Gutterfont.Size := Max(1, devEditor.Gutterfont.Size + 1);
    end else if WheelDelta < 0 then begin
      devEditor.Font.Size := Max(1, devEditor.Font.Size - 1);
      devEditor.Gutterfont.Size := Max(1, devEditor.Gutterfont.Size - 1);
    end;

    // We don't like to send the actual scrolling message to the editor when zooming
    Abort;
  end;
end;

procedure TMainForm.ImportCBCprojectClick(Sender: TObject);
begin
  with TImportCBForm.Create(Self) do begin
    if ShowModal = mrOK then
      OpenProject(GetFilename);
  end;
end;

procedure TMainForm.CompilerOutputAdvancedCustomDrawItem(Sender: TCustomListView; Item: TListItem; State:
  TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
var
  lowersubitem: String;
begin
  if StartsStr('[Warning] ', Item.SubItems[2]) then
    Sender.Canvas.Font.Color := TColor($0066FF) // Orange
  else if StartsStr('[Error] ', Item.SubItems[2]) then
    Sender.Canvas.Font.Color := clRed
  else if StartsStr('[Hint] ', Item.SubItems[2]) then
    Sender.Canvas.Font.Color := clYellow;

  // Make direction stuff bold
  lowersubitem := Trim(LowerCase(Item.SubItems[2]));
  if StartsStr('in ', lowersubitem) or
    StartsStr('at ', lowersubitem) then // direction stuff
    Sender.Canvas.Font.Style := [fsBold];
end;

procedure TMainForm.cmbGenericDropDown(Sender: TObject);
var
  widestwidth, I: integer;
  ComboBox: TComboBox;
begin
  ComboBox := TComboBox(Sender);

  // Set to default width first...
  SendMessage(ComboBox.Handle, CB_SETDROPPEDWIDTH, 0, 0);

  widestwidth := 0;

  // Fix, cmbMembers.Canvas.Font was set to Tahoma for some reason?
  ComboBox.Canvas.Font.Name := 'Courier New';

  // get the max needed with of the items in dropdown state
  for I := 0 to ComboBox.Items.Count - 1 do
    widestwidth := Max(widestwidth, ComboBox.Canvas.TextWidth(ComboBox.Items[I]) + 8); // padding

  if (widestwidth > ComboBox.Width) then begin

    // Add scrollbar width
    if ComboBox.DropDownCount < ComboBox.Items.Count then
      widestwidth := widestwidth + GetSystemMetrics(SM_CXVSCROLL);

    SendMessage(ComboBox.Handle, CB_SETDROPPEDWIDTH, widestwidth, 0);
  end;
end;

procedure TMainForm.NewFileBtnClick(Sender: TObject);
var
  pt: TPoint;
begin
  pt := tbMain.ClientToScreen(point(NewFileBtn.Left, NewFileBtn.Top + NewFileBtn.Height));
  TrackPopupMenu(mnuNew.Handle, TPM_LEFTALIGN or TPM_LEFTBUTTON, pt.X, pt.y, 0, Self.Handle, nil);
end;

procedure TMainForm.actUnCollapseExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    e.Text.UncollapseAll;
end;

procedure TMainForm.actCollapseExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    e.Text.CollapseAll;
end;

procedure TMainForm.actInsertExecute(Sender: TObject);
var
  pt: TPoint;
begin
  pt := tbSpecials.ClientToScreen(point(Insertbtn.Left, Insertbtn.Top + Insertbtn.Height));
  TrackPopupMenu(InsertItem.Handle, TPM_LEFTALIGN or TPM_LEFTBUTTON, pt.X, pt.Y, 0, Self.Handle, nil);
end;

procedure TMainForm.actToggleExecute(Sender: TObject);
var
  pt: TPoint;
begin
  pt := tbSpecials.ClientToScreen(point(Togglebtn.Left, Togglebtn.Top + togglebtn.Height));
  TrackPopupMenu(ToggleBookmarksItem.Handle, TPM_LEFTALIGN or TPM_LEFTBUTTON, pt.x, pt.y, 0, Self.Handle, nil);
end;

procedure TMainForm.actGotoExecute(Sender: TObject);
var
  pt: TPoint;
begin
  pt := tbSpecials.ClientToScreen(point(Gotobtn.Left, Gotobtn.Top + Gotobtn.Height));
  TrackPopupMenu(GotoBookmarksItem.Handle, TPM_LEFTALIGN or TPM_LEFTBUTTON, pt.x, pt.y, 0, Self.Handle, nil);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  fFirstShow := true;

  // Backup PATH variable
  devDirs.OriginalPath := GetEnvironmentVariable('PATH');

  // Create a compiler
  fCompiler := TCompiler.Create;
  with fCompiler do begin
    OnLogEntry := LogEntryProc;
    OnOutput := CompOutputProc;
    OnResOutput := CompResOutputProc;
    OnCompEnd := CompEndProc;
    OnCompSuccess := CompSuccessProc;
    OnRunEnd := RunEndProc;
  end;

  // Remember long version of paths
  fLogOutputRawData := TStringList.Create;

  // Create a debugger
  fDebugger := TDebugger.Create;
  with fDebugger do begin
    DebugView := Self.DebugView;
  end;

  // Create an editor manager
  fEditorList := TEditorList.Create;
  with fEditorList do begin
    LeftPageControl := Self.EditorPageControlLeft;
    RightPageControl := Self.EditorPageControlRight;
    Splitter := Self.EditorPageControlSplitter;
    Panel := Self.PageControlPanel;
  end;

  // Custom tools
  fTools := TToolController.Create;
  with fTools do begin
    Menu := ToolsMenu;
    Offset := ToolsMenu.IndexOf(PackageManagerItem);
    ToolClick := ToolItemClick;
    BuildMenu;
  end;

  // Don't create the autosave timer when we don't need it
  if devEditor.EnableAutoSave then begin
    AutoSaveTimer := TTimer.Create(nil);
    with AutoSaveTimer do begin
      OnTimer := EditorSaveTimer;
      Interval := devEditor.Interval * 60 * 1000; // miliseconds to minutes
      Enabled := devEditor.EnableAutoSave;
    end;
  end;

  // Create critical section to support a single instance
  fCriticalSection := TCriticalSection.Create;
  fFilesToOpen := TStringList.Create;

  // Apply shortcuts BEFORE TRANSLATING!!!
  with Shortcuts do begin
    Filename := devDirs.Config + DEV_SHORTCUTS_FILE;
    Load(ActionList);
  end;

  // Accept file drags
  DragAcceptFiles(Self.Handle, true);

  // Set left page control to previous state
  actProjectManager.Checked := devData.ShowLeftPages;
  LeftPageControl.ActivePageIndex := devData.LeftActivePage;
  actProjectManagerExecute(nil);
  LeftPageControl.Width := devData.ProjectWidth;
  if devData.ProjectFloat then begin
    FloatingPojectManagerItem.Click;
    devData.ProjectWindowState.SetPlacement(fProjectToolWindow.Handle);
  end;

  // Set bottom page control to previous state
  fPreviousHeight := devData.OutputHeight;

  if devData.MessageFloat then begin
    FloatingReportwindowItem.Click;
    devData.ReportWindowState.SetPlacement(fReportToolWindow.Handle);
  end;
  actShortenCompPaths.Checked := devData.ShortenCompPaths;

  // Set statusbar to previous state
  actStatusbar.Checked := devData.Statusbar;
  actStatusbarExecute(nil);

  // Set toolbars to previous state.
  // 1) position
  tbMain.Left := devData.ToolbarMainX;
  tbMain.Top := devData.ToolbarMainY;
  tbEdit.Left := devData.ToolbarEditX;
  tbEdit.Top := devData.ToolbarEditY;
  tbCompile.Left := devData.ToolbarCompileX;
  tbCompile.Top := devData.ToolbarCompileY;
  tbProject.Left := devData.ToolbarProjectX;
  tbProject.Top := devData.ToolbarProjectY;
  tbSpecials.Left := devData.ToolbarSpecialsX;
  tbSpecials.Top := devData.ToolbarSpecialsY;
  tbSearch.Left := devData.ToolbarSearchX;
  tbSearch.Top := devData.ToolbarSearchY;
  tbClasses.Left := devData.ToolbarClassesX;
  tbClasses.Top := devData.ToolbarClassesY;
  tbCompilers.Left := devData.ToolbarCompilersX;
  tbCompilers.Top := devData.ToolbarCompilersY;

  // Set toolbars to previous state.
  // 2) Visibility
  tbMain.Visible := devData.ToolbarMain;
  tbEdit.Visible := devData.ToolbarEdit;
  tbCompile.Visible := devData.ToolbarCompile;
  tbProject.Visible := devData.ToolbarProject;
  tbSpecials.Visible := devData.ToolbarSpecials;
  tbSearch.Visible := devData.ToolbarSearch;
  tbClasses.Visible := devData.ToolbarClasses;
  tbCompilers.Visible := devData.ToolbarCompilers;

  // Set toolbars to previous state.
  // 3) UI components
  ToolMainItem.Checked := devData.ToolbarMain;
  ToolEditItem.Checked := devData.ToolbarEdit;
  ToolCompileandRunItem.Checked := devData.ToolbarCompile;
  ToolProjectItem.Checked := devData.ToolbarProject;
  ToolSpecialsItem.Checked := devData.ToolbarSpecials;
  ToolSearchItem.Checked := devData.ToolbarSearch;
  ToolClassesItem.Checked := devData.ToolbarClasses;
  ToolCompilersItem.Checked := devData.ToolbarCompilers;

  // PageControl settings
  fEditorList.SetPreferences(devData.MsgTabs, devData.MultiLineTab);

  // Create datamod
  dmMain := TdmMain.Create(Self);
  with dmMain do begin
    MRUMenu := ClearhistoryItem;
    MRUClick := Self.MRUClick;
    CodeMenu := InsertItem;
    CodePop := InsertPopItem;
    CodeClick := CodeInsClick;
    CodeOffset := 2;
    LoadDataMod;
  end;

  // Create icon themes
  devImageThemes := TDevImageThemeFactory.Create;
  devImageThemes.LoadFromDirectory(devDirs.Themes);
  LoadTheme;

  // Set languages and other first time stuff
  if devData.First or (devData.Language = '') then
    Lang.SelectLanguage
  else
    Lang.Open(devData.Language);

  // Load bookmarks, captions and hints
  LoadText;

  // Load the current compiler set
  devCompilerSets.LoadSets;

  // Update toolbar
  UpdateCompilerList;

  // Try to fix the file associations. Needs write access to registry, which might cause exceptions to be thrown
  DDETopic := DevCppDDEServer.Name;
  if devData.CheckAssocs then begin
    try
      CheckAssociations(true); // check and fix
    except
      MessageBox(Application.Handle, PChar(Lang[ID_ENV_UACERROR]), PChar(Lang[ID_ERROR]), MB_OK);
      devData.CheckAssocs := false; // don't bother again
    end;
  end;

  // Configure parser, code completion, class browser
  UpdateClassBrowsing;

  // Showing for the first time? Maximize
  if devData.First then
    WindowState := wsMaximized
  else // Remember window placement
    devData.WindowState.SetPlacement(Self.Handle);

  // We need this variable during the whole startup process
  devData.First := FALSE;

  //test change hint color
  application.HintColor := clCream;
end;

procedure TMainForm.EditorPageControlMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  PageIndex: integer;
  newhint: String;
  e: TEditor;
  SenderPageControl: TPageControl;
begin
  SenderPageControl := TPageControl(Sender);
  PageIndex := SenderPageControl.IndexOfTabAt(X, Y);
  if PageIndex <> -1 then begin
    e := fEditorList.GetEditor(PageIndex, SenderPageControl);
    if Assigned(e) then
      newhint := e.FileName;
  end else
    newhint := '';

  // Only call CancelHint when the hint changes
  if (newhint <> fCurrentPageHint) then begin
    Application.CancelHint;
    fCurrentPageHint := newhint;
    SenderPageControl.Hint := newhint;
  end;
end;

procedure TMainForm.actBreakPointExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    e.ToggleBreakPoint(e.Text.CaretY);
end;

procedure TMainForm.OnInputEvalReady(const evalvalue: String);
begin
  EvalOutput.Text := evalvalue;
  EvalOutput.Font.Color := clWindowText;
  MainForm.fDebugger.OnEvalReady := nil;
end;

procedure TMainForm.EvaluateInputKeyPress(Sender: TObject; var Key: Char);
begin
  if fDebugger.Executing then begin
    if Key = Chr(VK_RETURN) then begin
      if Length(EvaluateInput.Text) > 0 then begin

        // Disable key, but make sure not to remove selected text
        EvaluateInput.SelStart := 0;
        EvaluateInput.SelLength := 0;
        Key := #0;

        fDebugger.OnEvalReady := OnInputEvalReady;
        fDebugger.SendCommand('print', EvaluateInput.Text, true);

        // Tell the user we're updating...
        EvalOutput.Font.Color := clGrayText;

        // Add to history
        if EvaluateInput.Items.IndexOf(EvaluateInput.Text) = -1 then
          EvaluateInput.AddItem(EvaluateInput.Text, nil);
      end;
    end;
  end;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  FileCount: Integer;
begin
  if not fFirstShow then
    Exit;

  fFirstShow := false;

  // Below a possible fix for the disappearing of controls in MessageControl is given:
  // Toggle visibility of bottom page control here.
  // If we do this in OnCreate, it messes with alignment options
  OpenCloseMessageSheet(False);

  // Open files passed to us (HAS to be done at FormShow)
  // This includes open file commands send to us via WMCopyData
  FileCount := ParseParameters(GetCommandLine);

  // Open them according to OpenFileList rules
  OpenFileList(fFilesToOpen);
  if FileCount = 0 then
    UpdateAppTitle;
  fFilesToOpen.Clear;

  // do not show tips if Dev-C++ is launched with a file and only slow
  // when the form shows for the first time, not when going fullscreen too
  if devData.ShowTipsOnStart and (FileCount = 0) then
    actShowTips.Execute;
end;

procedure TMainForm.actSkipFunctionExecute(Sender: TObject);
begin
  if fDebugger.Executing then begin
    fDebugger.SendCommand('finish', '', true);
  end;
end;

procedure TMainForm.actNextInsExecute(Sender: TObject);
begin
  if fDebugger.Executing then begin
    fDebugger.SendCommand('nexti', '', true);
  end;
end;

procedure TMainForm.actStepInsExecute(Sender: TObject);
begin
  if fDebugger.Executing then begin
    fDebugger.SendCommand('stepi', '', true);
  end;
end;

procedure TMainForm.DebugViewAdvancedCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
var
  curnode: TTreeNode;
begin
  curnode := node;
  while Assigned(curnode.Parent) do
    curnode := curnode.Parent;

  // TODO: why does this happen?
  if not Assigned(curnode.Data) then
    Exit;

  // Paint top level items in red if they aren't found
  if (curnode.Level = 0) and (PWatchVar(curnode.Data)^.gdbindex = -1) then begin
    if cdsSelected in State then
      Sender.Canvas.Font.Color := clWhite
    else
      Sender.Canvas.Font.Color := clRed;
  end else begin
    if cdsSelected in State then
      Sender.Canvas.Font.Color := clWhite
    else
      Sender.Canvas.Font.Color := clBlack;
  end;
end;

procedure TMainForm.FindOutputAdvancedCustomDrawSubItem(Sender: TCustomListView; Item: TListItem; SubItem: Integer;
  State: TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
var
  LineToDraw: String;
  BoldStart, BoldLen, i: integer;
  Rect: TRect;
  OldBrushColor, OldFontColor: TColor;

  procedure Draw(const s: String);
  var
    sizerect: TRect;
  begin
    DrawText(Sender.Canvas.Handle, PChar(s), Length(s), rect, DT_EXPANDTABS or DT_NOCLIP or DT_NOPREFIX);

    // Get text extent
    FillChar(sizerect, sizeof(sizerect), 0);
    DrawText(Sender.Canvas.Handle, PChar(s), Length(s), sizerect, DT_CALCRECT or DT_EXPANDTABS or DT_NOCLIP or
      DT_NOPREFIX);
    Inc(rect.Left, sizerect.Right - sizerect.Left + 1); // 1 extra pixel for extra width caused by bold
  end;
begin
  // Draw the current line marker in bold
  {if SubItem = 0 then begin
    Sender.Canvas.Font.Style := [fsBold];
    Sender.Canvas.Refresh;
    DefaultDraw := True;

  // Draw the find result in bold
  end else}
  if SubItem = 4 then begin

    // Get rect of subitem to draw
    Rect := Item.DisplayRect(drBounds);
    for i := 0 to SubItem - 1 do begin
      Rect.Left := Rect.Left + Sender.Column[i].Width;
      Rect.Right := Rect.Right + Sender.Column[i].Width;
    end;

    // Draw blue highlight
    OldBrushColor := Sender.Canvas.Brush.Color;
    OldFontColor := Sender.Canvas.Font.Color;
    if (cdsSelected in State) then begin
      Sender.Canvas.Brush.Color := clHighlight;
      Sender.Canvas.Font.Color := clWhite;
      Sender.Canvas.FillRect(rect);
    end;

    // Get text to draw
    LineToDraw := Item.SubItems[SubItem - 1];

    // Make text location appear 'native', like Windows would draw it
    OffsetRect(Rect, 4, 2);

    // Get location of bold part
    BoldStart := StrToIntDef(Item.SubItems[1], 1);
    BoldLen := Integer(Item.Data);

    // Draw part before bold highlight
    Draw(Copy(LineToDraw, 1, BoldStart - 1));

    // Enable bold
    Sender.Canvas.Font.Style := [fsBold];
    Sender.Canvas.Refresh;

    // Draw bold highlight
    Draw(Copy(LineToDraw, BoldStart, BoldLen));

    // Disable bold
    Sender.Canvas.Font.Style := [];
    Sender.Canvas.Refresh;

    // Draw part after bold highlight
    Draw(Copy(LineToDraw, BoldStart + BoldLen, MaxInt));

    // Restore colors
    Sender.Canvas.Brush.Color := OldBrushColor;
    Sender.Canvas.Font.Color := OldFontColor;

    DefaultDraw := false;
  end else
    DefaultDraw := true;
end;

procedure TMainForm.FindOutputAdvancedCustomDraw(Sender: TCustomListView; const ARect: TRect; Stage: TCustomDrawStage;
  var DefaultDraw: Boolean);
begin
  SendMessage(FindOutput.Handle, WM_CHANGEUISTATE, MAKEWPARAM(UIS_SET, UISF_HIDEFOCUS), 0);
end;

procedure TMainForm.CompilerOutputAdvancedCustomDraw(Sender: TCustomListView; const ARect: TRect; Stage:
  TCustomDrawStage; var DefaultDraw: Boolean);
begin
  SendMessage(CompilerOutput.Handle, WM_CHANGEUISTATE, MAKEWPARAM(UIS_SET, UISF_HIDEFOCUS), 0);
end;

procedure TMainForm.actSearchAgainExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if not Assigned(e) then
    Exit;

  // Repeat action if it was a finding action
  if Assigned(FindForm) then begin
    with FindForm do begin
      if FindTabs.TabIndex < 2 then begin // it's a find action

        // Disable entire scope searching
        if grpOrigin.Visible then
          rbEntireScope.Checked := false;

        // Always search forwards
        if grpDirection.Visible then
          rbForward.Checked := true;

        btnExecute.Click;
        e.Text.SetFocus;
      end;
    end;
  end;
end;

procedure TMainForm.actRevSearchAgainExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if not Assigned(e) then
    Exit;

  // Repeat action if it was a finding action
  if Assigned(FindForm) then begin
    with FindForm do begin
      if FindTabs.TabIndex < 2 then begin // it's a find action

        // Disable entire scope searching
        if grpOrigin.Visible then
          rbEntireScope.Checked := false;

        // Always search backwards
        if grpDirection.Visible then
          rbBackward.Checked := true;

        btnExecute.Click;
        e.Activate;
      end;
    end;
  end;
end;

procedure TMainForm.FindOutputDeletion(Sender: TObject; Item: TListItem);
begin
  // Form is being destroyed, don't use Lang which has been freed already
  // TODO: however, for some reason Lang might already have been destroyed
  // before Terminated is set to true. This causes the Lang singleton to be
  // recreated, which in turn fails because it needs a valid devDirs, which
  // has also already been deleted.
  // FIX: check for Assigned(devDirs) here
  if Application.Terminated or not Assigned(devDirs) then
    Exit;
  if FindOutput.Items.Count > 1 then
    FindSheet.Caption := Lang[ID_SHEET_FIND] + ' (' + IntToStr(FindOutput.Items.Count - 1) + ')'
  else
    FindSheet.Caption := Lang[ID_SHEET_FIND];
end;

procedure TMainForm.CompilerOutputDeletion(Sender: TObject; Item: TListItem);
begin
  if Application.Terminated or not Assigned(devDirs) then
    Exit;
  if CompilerOutput.Items.Count > 1 then
    CompSheet.Caption := Lang[ID_SHEET_COMP] + ' (' + IntToStr(CompilerOutput.Items.Count - 1) + ')'
  else
    CompSheet.Caption := Lang[ID_SHEET_COMP];
end;

procedure TMainForm.ResourceOutputDeletion(Sender: TObject; Item: TListItem);
begin
  if Application.Terminated or not Assigned(devDirs) then
    Exit;
  if ResourceOutput.Items.Count > 1 then
    ResSheet.Caption := Lang[ID_SHEET_RES] + ' (' + IntToStr(ResourceOutput.Items.Count - 1) + ')'
  else
    ResSheet.Caption := Lang[ID_SHEET_RES];
end;

procedure TMainForm.actStopExecuteUpdate(Sender: TObject);
begin
  if Assigned(fProject) then
    TCustomAction(Sender).Enabled := (not (fProject.Options.typ = dptStat) and devExecutor.Running) or
      fDebugger.Executing
  else
    TCustomAction(Sender).Enabled := ((EditorList.PageCount > 0) and devExecutor.Running) or fDebugger.Executing;
end;

procedure TMainForm.actUpdateIndent(Sender: TObject);
begin // special action to prevent tabs from being processed when using the find form
  TCustomAction(Sender).Enabled := (EditorList.PageCount > 0) and (not Assigned(FindForm) or not FindForm.Showing);
end;

procedure TMainForm.actDeleteLineExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    e.Text.CommandProcessor(ecDeleteLine, #0, nil);
end;

procedure TMainForm.cmbCompilersChange(Sender: TObject);
var
  index: integer;
  e: TEditor;

  procedure ChangeProjectCompilerSet;
  begin
    // If true, continue, otherwise, revert selection change
    if MessageDlg(Lang[ID_POPT_DISCARDCUSTOM], mtConfirmation, [mbYes, mbNo], 0) = mrNo then begin // user cancels change
      TComboBox(Sender).ItemIndex := fOldCompilerToolbarIndex; // undo index change
      Exit;
    end;

    // Notify project
    // Discard changes made to the old set...
    fProject.Options.CompilerSet := index;
    fProject.Options.CompilerOptions := devCompilerSets[index].INIOptions;
    fProject.Modified := true;
  end;

  procedure ChangeNonProjectCompilerSet;
  begin
    // Notify compiler list
    devCompilerSets.DefaultSetIndex := index;
    devCompilerSets.SaveSetList;
  end;
begin
  index := TComboBox(Sender).ItemIndex;
  if index = fOldCompilerToolbarIndex then
    Exit;

  // Check if the current file belongs to a project
  e := fEditorList.GetEditor;
  if Assigned(e) then begin
    if e.InProject and Assigned(fProject) then begin
      ChangeProjectCompilerSet;
    end else begin
      ChangeNonProjectCompilerSet;
    end;

    // No editors have been opened. Check if a project is open
  end else if Assigned(fProject) then begin
    ChangeProjectCompilerSet;

    // No project, no editor, modify global
  end else begin
    ChangeNonProjectCompilerSet;
  end;

  // Mention change to compiler sets
  //devCompilerSets.OnCompilerSetChanged(
  //  devCompilerSets[fOldCompilerToolbarIndex],
  //  devCompilerSets[index]);

  fOldCompilerToolbarIndex := index;
end;

procedure TMainForm.actDuplicateLineExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    e.Text.CommandProcessor(TSynEditEx.ecDuplicateLine, #0, nil);
end;

procedure TMainForm.actMoveSelUpExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    e.Text.CommandProcessor(TSynEditEx.ecMoveSelUp, #0, nil);
end;

procedure TMainForm.actMoveSelDownExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    e.Text.CommandProcessor(TSynEditEx.ecMoveSelDown, #0, nil);
end;

procedure TMainForm.actCodeCompletionUpdate(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  TCustomAction(Sender).Enabled := Assigned(e) and e.Text.Focused;
end;

procedure TMainForm.actCodeCompletionExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    e.ShowCompletion;
end;

procedure TMainForm.actPackageManagerExecute(Sender: TObject);
var
  s: String;
begin
  s := IncludeTrailingBackslash(devDirs.Exec) + PACKMAN_PROGRAM;
  if FileExists(s) then
    ExecuteFile(s, '', IncludeTrailingBackslash(devDirs.Exec), SW_SHOW);
end;

procedure TMainForm.actHelpExecute(Sender: TObject);
begin
  OpenHelpFile('index.htm');
end;

procedure TMainForm.actShortenCompPathsExecute(Sender: TObject);
var
  I: integer;
begin
  devData.ShortenCompPaths := not devData.ShortenCompPaths;

  // Re-add data to undo/redo shortening
  LogOutput.Lines.BeginUpdate;
  try
    LogOutput.Lines.Clear;
    for I := 0 to fLogOutputRawData.Count - 1 do begin
      if devData.ShortenCompPaths then
        LogOutput.Lines.Add(ShortenLogOutput(fLogOutputRawData[i]))
      else
        LogOutput.Lines.Add(fLogOutputRawData[i]);
    end;
  finally
    LogOutput.Lines.EndUpdate;
  end;
end;

procedure TMainForm.actSyntaxCheckFileExecute(Sender: TObject);
begin
  actStopExecuteExecute(Self);
  if fCompiler.Compiling then begin
    MessageDlg(Lang[ID_MSG_ALREADYCOMP], mtInformation, [mbOK], 0);
    Exit;
  end;
  if not PrepareForCompile(ctFile) then
    Exit;
  fCompiler.CheckSyntax;
end;

procedure TMainForm.LeftPageControlChange(Sender: TObject);
begin
  ClassBrowser.TabVisible := LeftPageControl.ActivePage = LeftClassSheet;
end;

procedure TMainForm.actSwapEditorExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    fEditorList.SwapEditor(e);
end;

procedure TMainForm.actSwapEditorUpdate(Sender: TObject);
begin
  TCustomAction(Sender).Enabled := fEditorList.PageCount > 1;
end;

procedure TMainForm.actSaveAllUpdate(Sender: TObject);
var
  e: TEditor;
  I: integer;
begin
  for I := 0 to fEditorList.PageCount - 1 do begin
    e := fEditorList[i];
    if Assigned(e) and (e.Text.Modified or (e.FileName = '')) then begin
      TCustomAction(Sender).Enabled := True;
      Exit;
    end;
  end;
  TCustomAction(Sender).Enabled := False;
end;

procedure TMainForm.PageControlPanelResize(Sender: TObject);
begin
  fEditorList.OnPanelResize(Sender);
end;

procedure TMainForm.actOpenFolderExecute(Sender: TObject);
var
  e: TEditor;
  Folder: String;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then begin
    Folder := ExtractFilePath(e.FileName);
    if Folder <> '' then
      ShellExecute(Application.Handle, 'open', 'explorer.exe', PChar(Folder), nil, SW_SHOWNORMAL);
  end;
end;

procedure TMainForm.actGotoBreakPointExecute(Sender: TObject);
var
  e: TEditor;
  FileName: String;
begin
  FileName := fDebugger.BreakPointFile;
  if FileName <> '' then begin
    e := fEditorList.GetEditorFromFileName(FileName);
    if Assigned(e) then begin
      e.GotoActiveBreakpoint;
      e.Activate;
    end;
  end;
end;

procedure TMainForm.actToggleCommentInlineExecute(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  if Assigned(e) then
    e.Text.CommandProcessor(TSynEditEx.ecCommentInline, #0, nil);
end;

procedure TMainForm.actToggleCommentInlineUpdate(Sender: TObject);
var
  e: TEditor;
begin
  e := fEditorList.GetEditor;
  TCustomAction(Sender).Enabled := Assigned(e) and e.Text.SelAvail;
end;

procedure TMainForm.actFormatCurrentFileExecute(Sender: TObject);
var
  e: TEditor;
  OldCaretXY: TBufferCoord;
  OldTopLine: integer;
begin
  if devFormatter.Validate then begin
    e := fEditorList.GetEditor;
    if Assigned(e) then begin
      // Save for undo list creation
      OldTopLine := e.Text.TopLine;
      OldCaretXY := e.Text.CaretXY;

      devFormatter.FormatMemory(e, devFormatter.FullCommand);

      // Attempt to not scroll view
      e.Text.TopLine := OldTopLine;
      e.Text.CaretXY := OldCaretXY;
    end;
  end else
    MessageDlg(Lang[ID_FORMATTER_NOTVALID], mtWarning, [mbOK], 0);
end;

procedure TMainForm.actFormatOptionsExecute(Sender: TObject);
begin
  with TFormatterOptionsForm.Create(nil) do try
    if ShowModal = mrOk then begin
    end;
  finally
    Free;
  end;
end;

procedure TMainForm.FindOutputSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if Selected then
    Item.Caption := '>'
  else
    Item.Caption := '';
end;

procedure TMainForm.actRunTestsExecute(Sender: TObject);
begin
  with TTestClass.Create do try
    TestAll;
  finally
    Free;
  end;
end;

procedure TMainForm.WMCopyData(var Message: TMessage);
var
  MessageData: String;
begin
  MessageData := GetSentStructData(Message);
  if MessageData <> '' then begin
    fCriticalSection.Acquire;
    try
      ParseParameters(MessageData); // not thread safe

      // Ff the window has already opened, force open here
      // Otherwise, these files get added to the big file of files to open
      // when the main instance executes FormShow.
      if not fFirstShow then begin
        OpenFileList(fFilesToOpen);
        fFilesToOpen.Clear;
      end;
      Application.BringToFront;
    finally
      fCriticalSection.Release;
    end;
  end;
end;

function TMainForm.ParseParameters(const Parameters: WideString): Integer;
type
  TPWideCharArray = array[0..0] of PWideChar;
var
  I, ParameterCount: Integer;
  ParameterList: PPWideChar;
  Item: WideString;
begin
  Result := 0;

  // Convert string to list of items
  ParameterList := CommandLineToArgvW(PWideChar(Parameters), ParameterCount);
  if not Assigned(ParameterList) then
    Exit;

  // Walk the list
  I := 1; // skip first one
  while I < ParameterCount do begin
    Item := TPWideCharArray(ParameterList^)[i];

    // Skip the configuration redirect stuff
    if Item = '-c' then begin
      I := I + 2;
      Continue;
    end;

    fFilesToOpen.Add(Item);
    Inc(I);
  end;
  Result := fFilesToOpen.Count;

  // Free list of pointers
  LocalFree(Cardinal(ParameterList));
end;

procedure TMainForm.actDonateExecute(Sender: TObject);
begin
  ShellExecute(GetDesktopWindow(), 'open',
    PChar('https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=7FD675DNV8KKJ'), nil, nil,
    SW_SHOWNORMAL);
end;

end.


