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
{$IFDEF WIN32}
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	Menus, StdCtrls, ComCtrls, ToolWin, ExtCtrls, Buttons, utils,
	Project, editor, compiler, ActnList, ToolFrm, AppEvnts,
	debugger, ClassBrowser, CodeCompletion, CppParser, CppTokenizer,
	StrUtils, SynEditTypes, devFileMonitor, devMonitorTypes, DdeMan,
	CVSFrm, devShortcuts, debugreader, CommCtrl, devcfg, VistaAltFixUnit;
{$ENDIF}
{$IFDEF LINUX}
	SysUtils, Classes, QGraphics, QControls, QForms, QDialogs,
	QMenus, QStdCtrls, QComCtrls, QExtCtrls, QButtons, utils,
	project, editor, compiler, QActnList,
	debugger, ClassBrowser, CodeCompletion, CppParser, CppTokenizer,
	devShortcuts, StrUtils, devFileMonitor, devMonitorTypes,
	CVSFm, Types;
{$ENDIF}

type
	TMainForm = class(TForm)
		MainMenu: TMainMenu;
		FileMenu: TMenuItem;
		NewprojectItem: TMenuItem;
		NewTemplateItem: TMenuItem;
		N34: TMenuItem;
		OpenprojectItem: TMenuItem;
		ReOpenItem: TMenuItem;
		ClearhistoryItem: TMenuItem;
		N11: TMenuItem;
		NewSourceFileItem: TMenuItem;
		NewresourcefileItem: TMenuItem;
		N12: TMenuItem;
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
		N14: TMenuItem;
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
		CompileroutputItem: TMenuItem;
		AlwaysShowItem: TMenuItem;
		N37: TMenuItem;
		ShowonlywhenneededItem: TMenuItem;
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
		DebugItem: TMenuItem;
		CompileroptionsItem: TMenuItem;
		EnvironmentoptionsItem: TMenuItem;
		ToolsMenu: TMenuItem;
		ConfiguretoolsItem: TMenuItem;
		mnuToolSep1: TMenuItem;
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
		ResourceOutput: TListBox;
		LogSheet: TTabSheet;
		Toolbar: TControlBar;
		tbMain: TToolBar;
		OpenBtn: TToolButton;
		tbCompile: TToolBar;
		CleanItem: TMenuItem;
		NewFileBtn: TToolButton;
		SaveUnitBtn: TToolButton;
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
		EditorPopupMenu: TPopupMenu;
		UndoPopItem: TMenuItem;
		RedoPopItem: TMenuItem;
		MenuItem1: TMenuItem;
		CutPopItem: TMenuItem;
		CopyPopItem: TMenuItem;
		PastePopItem: TMenuItem;
		MenuItem2: TMenuItem;
		InsertPopItem: TMenuItem;
		CommentheaderPopItem: TMenuItem;
		DateandtimePopItem: TMenuItem;
		MenuItem3: TMenuItem;
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
		ErrorLabel: TLabel;
		SizeOfOutput: TLabel;
		FindSheet: TTabSheet;
		FindOutput: TListView;
		FindinallfilesItem: TMenuItem;
		N20: TMenuItem;
		mnuNew: TMenuItem;
		N13: TMenuItem;
		ActionList: TActionList;
		actNewSource: TAction;
		actNewProject: TAction;
		actNewRes: TAction;
		actNewTemplate: TAction;
		actOpen: TAction;
		actHistoryClear: TAction;
		actSave: TAction;
		actSaveAs: TAction;
		actSaveAll: TAction;
		actClose: TAction;
		actCloseAll: TAction;
		actCloseProject: TAction;
		actXHTML: TAction;
		actXRTF: TAction;
		actXProject: TAction;
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
		actCompOutput: TAction;
		actCompOnNeed: TAction;
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
		actUpdateCheck: TAction;
		actAbout: TAction;
		actProjectSource: TAction;
		actUnitRemove: TAction;
		actUnitRename: TAction;
		actUnitHeader: TAction;
		actUnitOpen: TAction;
		actUnitClose: TAction;
		EditorOptions1: TMenuItem;
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
    PageControl: TPageControl;
		Close1: TMenuItem;
		N16: TMenuItem;
		DebugMenu: TMenuItem;
		N18: TMenuItem;
		DbgNextItem: TMenuItem;
		StepoverItem: TMenuItem;
		N21: TMenuItem;
		DebugSheet: TTabSheet;
		actAddWatch: TAction;
		actEditWatch: TAction;
		pnlFull: TPanel;
		btnFullScrRevert: TSpeedButton;
		actNextLine: TAction;
		actStepOver: TAction;
		actWatchItem: TAction;
		actRemoveWatch: TAction;
		actStopExecute: TAction;
		StopExecution1: TMenuItem;
		InsertBtn: TToolButton;
		ToggleBtn: TToolButton;
		GotoBtn: TToolButton;
		actFileMenu: TAction;
		actEditMenu: TAction;
		actSearchMenu: TAction;
		actViewMenu: TAction;
		actProjectMenu: TAction;
		actExecuteMenu: TAction;
		actDebugMenu: TAction;
		actToolsMenu: TAction;
		actWindowMenu: TAction;
		actHelpMenu: TAction;
		CppTokenizer: TCppTokenizer;
		CppParser: TCppParser;
		CodeCompletion: TCodeCompletion;
		N22: TMenuItem;
		Swapheadersource1: TMenuItem;
		N23: TMenuItem;
		Swapheadersource2: TMenuItem;
		actSwapHeaderSource: TAction;
		SizeFile: TEdit;
		TotalErrors: TEdit;
		InsertItem: TMenuItem;
		SyntaxCheckItem: TMenuItem;
		actSyntaxCheck: TAction;
		devShortcuts: TdevShortcuts;
		actConfigdevShortcuts: TAction;
		ConfiguredevShortcuts1: TMenuItem;
		DateTimeMenuItem: TMenuItem;
		actProgramReset: TAction;
		N25: TMenuItem;
		Programreset1: TMenuItem;
		CommentheaderMenuItem: TMenuItem;
		SplitterBottom: TSplitter;
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
		mnuBrowserSep3: TMenuItem;
		mnuBrowserViewMode: TMenuItem;
		mnuBrowserViewAll: TMenuItem;
		mnuBrowserViweCurrent: TMenuItem;
		actBrowserGotoDecl: TAction;
		actBrowserGotoImpl: TAction;
		actBrowserNewClass: TAction;
		actBrowserNewMember: TAction;
		actBrowserNewVar: TAction;
		actBrowserViewAll: TAction;
		actBrowserViewCurrent: TAction;
		actProfileProject: TAction;
		N29: TMenuItem;
		Profileanalysis1: TMenuItem;
		N24: TMenuItem;
		CheckforupdatesItem: TMenuItem;
		N31: TMenuItem;
		actBrowserAddFolder: TAction;
		actBrowserRemoveFolder: TAction;
		mnuBrowserAddFolder: TMenuItem;
		mnuBrowserRemoveFolder: TMenuItem;
		actBrowserRenameFolder: TAction;
		mnuBrowserRenameFolder: TMenuItem;
		actCloseAllButThis: TAction;
		CloseAll1: TMenuItem;
		Closeallexceptthis1: TMenuItem;
		CloseAll2: TMenuItem;
		actStepLine: TAction;
		DbgSingleStep: TMenuItem;
		DebugVarsPopup: TPopupMenu;
		AddwatchPop: TMenuItem;
		RemoveWatchPop: TMenuItem;
		devFileMonitor: TdevFileMonitor;
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
		Addfolder1: TMenuItem;
		Removefolder1: TMenuItem;
		Renamefolder1: TMenuItem;
		actImportMSVC: TAction;
		ImportItem: TMenuItem;
		N41: TMenuItem;
		ToggleBreakpointPopupItem: TMenuItem;
		AddWatchPopupItem: TMenuItem;
		ViewCPUItem: TMenuItem;
		actViewCPU: TAction;
		actExecParams: TAction;
		mnuExecParameters: TMenuItem;
		DevCppDDEServer: TDdeServerConv;
		actShowTips: TAction;
		ShowTipsItem: TMenuItem;
		N42: TMenuItem;
		Usecolors1: TMenuItem;
		actBrowserUseColors: TAction;
		HelpMenuItem: TMenuItem;
		actBrowserViewProject: TAction;
		mnuBrowserViewProject: TMenuItem;
		N43: TMenuItem;
		PackageManagerItem: TMenuItem;
		btnAbortCompilation: TSpeedButton;
		actAbortCompilation: TAction;
		N44: TMenuItem;
		Addfile1: TMenuItem;
		actCVSImport: TAction;
		actCVSCheckout: TAction;
		actCVSUpdate: TAction;
		actCVSCommit: TAction;
		actCVSDiff: TAction;
		actCVSLog: TAction;
		N45: TMenuItem;
		CVS1: TMenuItem;
		N47: TMenuItem;
		mnuCVSLog3: TMenuItem;
		mnuCVSUpdate3: TMenuItem;
		mnuCVSDiff3: TMenuItem;
		mnuCVSCommit3: TMenuItem;
		N48: TMenuItem;
		CVS2: TMenuItem;
		mnuCVSCommit1: TMenuItem;
		mnuCVSUpdate1: TMenuItem;
		mnuCVSDiff1: TMenuItem;
		N55: TMenuItem;
		mnuCVSLog1: TMenuItem;
		N49: TMenuItem;
		CVS3: TMenuItem;
		mnuCVSCommit2: TMenuItem;
		mnuCVSUpdate2: TMenuItem;
		mnuCVSDiff2: TMenuItem;
		N51: TMenuItem;
		mnuCVSLog2: TMenuItem;
		mnuCVS: TMenuItem;
		mnuCVSUpdate: TMenuItem;
		mnuCVSDiff: TMenuItem;
		N53: TMenuItem;
		mnuCVSCommit: TMenuItem;
		N56: TMenuItem;
		mnuCVSLog: TMenuItem;
		mnuCVSCurrent: TMenuItem;
		mnuCVSWhole: TMenuItem;
		mnuCVSImportP: TMenuItem;
		mnuCVSCheckoutP: TMenuItem;
		mnuCVSUpdateP: TMenuItem;
		mnuCVSDiffP: TMenuItem;
		N58: TMenuItem;
		mnuCVSLogP: TMenuItem;
		N52: TMenuItem;
		N59: TMenuItem;
		N60: TMenuItem;
		N46: TMenuItem;
		Commit1: TMenuItem;
		ListItem: TMenuItem;
		GotoprojectmanagerItem: TMenuItem;
		N50: TMenuItem;
		mnuFileProps: TMenuItem;
		N54: TMenuItem;
		mnuUnitProperties: TMenuItem;
		actCVSAdd: TAction;
		actCVSRemove: TAction;
		N61: TMenuItem;
		mnuCVSAdd: TMenuItem;
		mnuCVSRemove: TMenuItem;
		N62: TMenuItem;
		mnuCVSAdd2: TMenuItem;
		mnuCVSRemove2: TMenuItem;
		N63: TMenuItem;
		GoToClassBrowserItem: TMenuItem;
		actBrowserShowInherited: TAction;
		Showinheritedmembers1: TMenuItem;
		actCVSLogin: TAction;
		actCVSLogout: TAction;
		N65: TMenuItem;
		Login1: TMenuItem;
		Logout1: TMenuItem;
		N66: TMenuItem;
		LeftPageControl: TPageControl;
		ProjectSheet: TTabSheet;
		ProjectView: TTreeView;
		ClassSheet: TTabSheet;
		ClassBrowser: TClassBrowser;
		AddWatchBtn: TButton;
		RemoveWatchBtn: TButton;
		FloatingPojectManagerItem: TMenuItem;
		actCompileCurrentFile: TAction;
		Compilecurrentfile1: TMenuItem;
		actSaveProjectAs: TAction;
		SaveprojectasItem: TMenuItem;
		mnuOpenWith: TMenuItem;
		tbClasses: TToolBar;
		cmbClasses: TComboBox;
		cmbMembers: TComboBox;
		N17: TMenuItem;
		ToolClassesItem: TMenuItem;
		DebugLeftSheet: TTabSheet;
		DebugTree: TTreeView;
		StepOverBtn: TButton;
		DebugStartPanel: TPanel;
		DDebugBtn: TSpeedButton;
		StopExecBtn: TSpeedButton;
		N67: TMenuItem;
		FloatingReportwindowItem: TMenuItem;
		N57: TMenuItem;
		AttachtoprocessItem: TMenuItem;
		actAttachProcess: TAction;
		ModifyWatchPop: TMenuItem;
		actModifyWatch: TAction;
		ClearallWatchPop: TMenuItem;
		CompilerOutput: TListView;
		N5: TMenuItem;
		Class1: TMenuItem;
		DeleteProfilingInformation: TMenuItem;
		actDeleteProfileProject: TAction;
		GotoDefineEditor: TMenuItem;
		GotoDeclEditor: TMenuItem;
		N15: TMenuItem;
		actGotoDeclEditor: TAction;
		actGotoImplEditor: TAction;
		actHideFSBar: TAction;
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
		actXTex: TAction;
		NextLineBtn: TButton;
		IntoLineBtn: TButton;
		lblSendCommandGdb: TLabel;
    edGdbCommand: TComboBox;
		DebugOutput: TMemo;
		DebugSendPanel: TPanel;
		ViewCPUBtn: TButton;
		ModifyWatchBtn: TButton;
		N68: TMenuItem;
		Abortcompilation1: TMenuItem;
		Modifywatch1: TMenuItem;
		Removewatch1: TMenuItem;
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
		Skipfunction1: TMenuItem;
		N69: TMenuItem;
		actNextIns1: TMenuItem;
		Intoinstruction1: TMenuItem;
		N70: TMenuItem;
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

		procedure FormClose(Sender: TObject; var Action: TCloseAction);
		procedure FormDestroy(Sender: TObject);
		procedure SetStatusbarLineCol;
		procedure SetStatusbarMessage(const msg:AnsiString);
		procedure ToggleBookmarkClick(Sender: TObject);
		procedure GotoBookmarkClick(Sender: TObject);
		procedure MessageControlChange(Sender: TObject);
		procedure ProjectViewContextPopup(Sender: TObject; MousePos: TPoint;var Handled: Boolean);
		procedure ProjectViewDblClick(Sender: TObject);
		procedure ToolbarClick(Sender: TObject);
		procedure ToolbarContextPopup(Sender: TObject; MousePos: TPoint;var Handled: Boolean);
		procedure SplitterBottomMoved(Sender: TObject);
		procedure actNewSourceExecute(Sender: TObject);
		procedure actNewProjectExecute(Sender: TObject);
		procedure actNewResExecute(Sender: TObject);
		procedure actNewTemplateExecute(Sender: TObject);
		procedure actOpenExecute(Sender: TObject);
		procedure actHistoryClearExecute(Sender: TObject);
		procedure actSaveExecute(Sender: TObject);
		procedure actSaveAsExecute(Sender: TObject);
		procedure actSaveAllExecute(Sender: TObject);
		procedure actCloseExecute(Sender: TObject);
		procedure actCloseAllExecute(Sender: TObject);
		procedure actCloseProjectExecute(Sender: TObject);
		procedure actXHTMLExecute(Sender: TObject);
		procedure actXRTFExecute(Sender: TObject);
		procedure actXTexExecute(Sender: TObject);
		procedure actXProjectExecute(Sender: TObject);
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
		procedure actCompOutputExecute(Sender: TObject);
		procedure actCompOnNeedExecute(Sender: TObject);
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
		procedure actUpdateCheckExecute(Sender: TObject);
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
		procedure actUpdatePageCount(Sender: TObject);			// enable on pagecount> 0
		procedure actUpdateProject(Sender: TObject);			// enable on fproject assigned
		procedure actUpdatePageProject(Sender: TObject);		// enable on both above
		procedure actUpdatePageorProject(Sender: TObject);		// enable on either of above
		procedure actUpdateEmptyEditor(Sender: TObject);		// enable on unempty editor
		procedure actUpdateDebuggerRunning(Sender: TObject);	// enable when debugger running
		procedure actUpdateDeleteWatch(Sender: TObject);		// enable on watch var selection
		procedure actIncrementalExecute(Sender: TObject);
		procedure CompilerOutputDblClick(Sender: TObject);
		procedure FindOutputDblClick(Sender: TObject);
		procedure actShowBarsExecute(Sender: TObject);
		procedure btnFullScrRevertClick(Sender: TObject);
		procedure FormContextPopup(Sender: TObject; MousePos: TPoint;var Handled: Boolean);
		procedure actAddWatchExecute(Sender: TObject);
		procedure ProjectViewClick(Sender: TObject);
		procedure actNextLineExecute(Sender: TObject);
		procedure actRemoveWatchExecute(Sender: TObject);
		procedure actStepOverExecute(Sender: TObject);
		procedure actForceStopExecuteExecute(Sender: TObject);
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
		procedure ClassBrowser1Select(Sender: TObject; Filename: TFileName;Line: Integer);
		procedure CppParserTotalProgress(Sender: TObject;const FileName: string;Total, Current: Integer);
		procedure CodeCompletionResize(Sender: TObject);
		procedure actSwapHeaderSourceUpdate(Sender: TObject);
		procedure actSwapHeaderSourceExecute(Sender: TObject);
		procedure actSyntaxCheckExecute(Sender: TObject);
		procedure actUpdateExecute(Sender: TObject);
		procedure PageControlChange(Sender: TObject);
		procedure actConfigdevShortcutsExecute(Sender: TObject);
		procedure DateTimeMenuItemClick(Sender: TObject);
		procedure actProgramResetExecute(Sender: TObject);
		procedure actProgramResetUpdate(Sender: TObject);
		procedure CommentheaderMenuItemClick(Sender: TObject);
		procedure PageControlMouseDown(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
		procedure actNewTemplateUpdate(Sender: TObject);
		procedure actCommentExecute(Sender: TObject);
		procedure actUncommentExecute(Sender: TObject);
		procedure actIndentExecute(Sender: TObject);
		procedure actUnindentExecute(Sender: TObject);
		procedure PageControlDragOver(Sender, Source: TObject; X, Y: Integer;State: TDragState; var Accept: Boolean);
		procedure PageControlDragDrop(Sender, Source: TObject; X, Y: Integer);
		procedure actGotoFunctionExecute(Sender: TObject);
		procedure actBrowserGotoDeclUpdate(Sender: TObject);
		procedure actBrowserGotoImplUpdate(Sender: TObject);
		procedure actBrowserGotoDeclExecute(Sender: TObject);
		procedure actBrowserGotoImplExecute(Sender: TObject);
		procedure actBrowserNewClassUpdate(Sender: TObject);
		procedure actBrowserNewMemberUpdate(Sender: TObject);
		procedure actBrowserNewVarUpdate(Sender: TObject);
		procedure actBrowserViewAllUpdate(Sender: TObject);
		procedure actBrowserNewClassExecute(Sender: TObject);
		procedure actBrowserNewMemberExecute(Sender: TObject);
		procedure actBrowserNewVarExecute(Sender: TObject);
		procedure actBrowserViewAllExecute(Sender: TObject);
		procedure actBrowserViewCurrentExecute(Sender: TObject);
		procedure actProfileProjectExecute(Sender: TObject);
		procedure actBrowserAddFolderExecute(Sender: TObject);
		procedure actBrowserRemoveFolderExecute(Sender: TObject);
		procedure actBrowserAddFolderUpdate(Sender: TObject);
		procedure actBrowserRenameFolderExecute(Sender: TObject);
		procedure actCloseAllButThisExecute(Sender: TObject);
		procedure actStepLineExecute(Sender: TObject);
		procedure lvBacktraceCustomDrawItem(Sender: TCustomListView;Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
		procedure lvBacktraceMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
		procedure actRunUpdate(Sender: TObject);
		procedure actCompileRunUpdate(Sender: TObject);
		procedure actCompileUpdate(Sender: TObject);
		procedure devFileMonitorNotifyChange(Sender: TObject;ChangeType: TdevMonitorChangeType; Filename: string);
		procedure actFilePropertiesExecute(Sender: TObject);
		procedure actViewToDoListExecute(Sender: TObject);
		procedure actAddToDoExecute(Sender: TObject);
		procedure actProjectNewFolderExecute(Sender: TObject);
		procedure actProjectRemoveFolderExecute(Sender: TObject);
		procedure actProjectRenameFolderExecute(Sender: TObject);
		procedure ProjectViewDragOver(Sender, Source: TObject; X, Y: Integer;State: TDragState; var Accept: Boolean);
		procedure ProjectViewDragDrop(Sender, Source: TObject; X, Y: Integer);
		procedure actImportMSVCExecute(Sender: TObject);
		procedure ViewCPUItemClick(Sender: TObject);
		procedure edGdbCommandKeyPress(Sender: TObject; var Key: Char);
		procedure actExecParamsExecute(Sender: TObject);
		procedure DevCppDDEServerExecuteMacro(Sender: TObject; Msg: TStrings);
		procedure actShowTipsExecute(Sender: TObject);
		procedure actBrowserUseColorsExecute(Sender: TObject);
		procedure HelpMenuItemClick(Sender: TObject);
		procedure CppParserStartParsing(Sender: TObject);
		procedure CppParserEndParsing(Sender: TObject);
		procedure actBrowserViewProjectExecute(Sender: TObject);
		procedure PackageManagerItemClick(Sender: TObject);
		procedure actAbortCompilationUpdate(Sender: TObject);
		procedure actAbortCompilationExecute(Sender: TObject);
		procedure ProjectViewKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
		procedure RemoveItem(Node: TTreeNode);
		procedure ProjectViewChanging(Sender: TObject; Node: TTreeNode;var AllowChange: Boolean);
		procedure dynactOpenEditorByTagExecute(Sender: TObject);
		procedure actWindowMenuExecute(Sender: TObject);
		procedure actGotoProjectManagerExecute(Sender: TObject);
		procedure actCVSImportExecute(Sender: TObject);
		procedure actCVSCheckoutExecute(Sender: TObject);
		procedure actCVSUpdateExecute(Sender: TObject);
		procedure actCVSCommitExecute(Sender: TObject);
		procedure actCVSDiffExecute(Sender: TObject);
		procedure actCVSLogExecute(Sender: TObject);
		procedure ListItemClick(Sender: TObject);
		procedure ProjectViewCompare(Sender: TObject; Node1, Node2: TTreeNode;Data: Integer; var Compare: Integer);
		procedure ProjectViewKeyPress(Sender: TObject; var Key: Char);
		procedure ProjectViewMouseDown(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
		procedure actCVSAddExecute(Sender: TObject);
		procedure actCVSRemoveExecute(Sender: TObject);
		procedure GoToClassBrowserItemClick(Sender: TObject);
		procedure actBrowserShowInheritedExecute(Sender: TObject);
		procedure actCVSLoginExecute(Sender: TObject);
		procedure actCVSLogoutExecute(Sender: TObject);
		procedure ReportWindowClose(Sender: TObject; var Action: TCloseAction);
		procedure FloatingPojectManagerItemClick(Sender: TObject);
		procedure actCompileCurrentFileExecute(Sender: TObject);
		procedure actCompileCurrentFileUpdate(Sender: TObject);
		procedure actSaveProjectAsExecute(Sender: TObject);
		procedure mnuOpenWithClick(Sender: TObject);
		procedure cmbClassesChange(Sender: TObject);
		procedure cmbMembersChange(Sender: TObject);
		procedure CompilerOutputKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
		procedure FindOutputKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
		procedure DebugTreeKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
		procedure DebugVarsPopupPopup(Sender: TObject);
		procedure FloatingReportwindowItemClick(Sender: TObject);
		procedure actAttachProcessUpdate(Sender: TObject);
		procedure actAttachProcessExecute(Sender: TObject);
		procedure actModifyWatchExecute(Sender: TObject);
		procedure actModifyWatchUpdate(Sender: TObject);
		procedure ClearallWatchPopClick(Sender: TObject);
		procedure mnuCVSClick(Sender: TObject);
		procedure actMsgCopyAllExecute(Sender: TObject);
		procedure actMsgSaveAllExecute(Sender: TObject);
		procedure actDeleteProfileProjectExecute(Sender: TObject);
		procedure actGotoImplDeclEditorExecute(Sender: TObject);
		procedure actHideFSBarExecute(Sender: TObject);
		procedure UpdateSplash(const text : AnsiString);
		procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
		procedure ImportCBCprojectClick(Sender: TObject);
		procedure CompilerOutputAdvancedCustomDrawItem(Sender: TCustomListView;Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;var DefaultDraw: Boolean);
		procedure cmbMembersDropDown(Sender: TObject);
		procedure NewFileBtnClick(Sender: TObject);
		procedure actUnCollapseExecute(Sender: TObject);
		procedure actCollapseExecute(Sender: TObject);
		procedure actInsertExecute(Sender: TObject);
		procedure actToggleExecute(Sender: TObject);
		procedure actGotoExecute(Sender: TObject);
		procedure actEditMenuUpdate(Sender: TObject);
		procedure FormCreate(Sender: TObject);
		procedure PageControlMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
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
		procedure DebugTreeAdvancedCustomDrawItem(Sender: TCustomTreeView;Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;var PaintImages, DefaultDraw: Boolean);
		procedure FindOutputAdvancedCustomDrawSubItem(Sender: TCustomListView;Item: TListItem; SubItem: Integer; State: TCustomDrawState;Stage: TCustomDrawStage; var DefaultDraw: Boolean);
		procedure actMsgCutExecute(Sender: TObject);
		procedure FindOutputAdvancedCustomDraw(Sender: TCustomListView;const ARect: TRect; Stage: TCustomDrawStage;var DefaultDraw: Boolean);
		procedure CompilerOutputAdvancedCustomDraw(Sender: TCustomListView;const ARect: TRect; Stage: TCustomDrawStage;var DefaultDraw: Boolean);
		procedure actMsgSelAllExecute(Sender: TObject);
		procedure FormActivate(Sender: TObject);
	private
		fPreviousHeight   : integer; // stores MessageControl height to be able to restore to previous height
		fTools            : TToolController; // tool list controller
		ProjectToolWindow : TForm; // floating left tab control
		ReportToolWindow  : TForm; // floating bottom tab control
		OldLeft           : integer; // stores position of window when going fullscreen
		OldTop            : integer; // idem
		OldWidth          : integer; // idem
		OldHeight         : integer; // idem
		WindowPlacement   : TWindowPlacement; // idem
		fFirstShow        : boolean; // true for first WM_SHOW, false for others
		fFirstActivate    : boolean; // see above
		fShowTips         : boolean;
		fRemoveOptions    : boolean;
		fOptionsDir       : AnsiString;
		function ParseParams(s : AnsiString) : AnsiString;
		procedure BuildBookMarkMenus;
		procedure SetHints;
		procedure MRUClick(Sender: TObject);
		procedure CodeInsClick(Sender: TObject);
		procedure ToolItemClick(Sender: TObject);
		procedure WMDropFiles(var msg: TMessage); message WM_DROPFILES;
		procedure LogEntryProc(const msg: AnsiString);
		procedure CompOutputProc(const _Line, _Col, _Unit, _Message: AnsiString);
		procedure CompResOutputProc(const _Line, _Unit, _Message: AnsiString);
		procedure CompSuccessProc;
		procedure LoadText;
		function SaveFileAs(e : TEditor): Boolean;
		procedure OpenUnit;
		function PrepareForCompile: Boolean;
		procedure LoadTheme;
		procedure ScanActiveProject;
		procedure CheckForDLLProfiling;
		procedure DoCVSAction(Sender: TObject; whichAction: TCVSAction);
		procedure ProjectWindowClose(Sender: TObject; var Action: TCloseAction);
		procedure SetupProjectView;
		procedure BuildOpenWith;
		procedure RebuildClassesToolbar;
		procedure PrepareDebugger;
		procedure ClearMessageControl;
	public
		AutoSaveTimer : TTimer;
		fProject : TProject;
		fDebugger : TDebugger;
		fCompiler : TCompiler;

		procedure InitClassBrowser(ReloadCache: boolean);
		procedure UpdateAppTitle;
		procedure OpenCloseMessageSheet(_Show: boolean);
		function SaveFile(e : TEditor): Boolean;
		procedure OpenFile(const s : AnsiString);
		procedure OpenProject(const s: AnsiString);
		function FileIsOpen(const s: AnsiString; inprj: boolean = false): integer;
		function GetEditor(index: integer = -1): TEditor;
		function GetEditorFromFileName(const ffile : AnsiString) : TEditor;
		procedure GotoBreakpoint(const bfile: AnsiString; bline: integer);
		procedure RemoveActiveBreakpoints;
		procedure AddFindOutputItem(const line, col, filename, msg, keyword: AnsiString);
		procedure SetProjCompOpt(idx: integer; Value: char);
		function CheckCompileOption(const option : AnsiString;var optionstructout : PCompilerOption;var optionindexout : integer) : boolean;
		function CloseEditor(index : integer): Boolean;
		procedure EditorSaveTimer(sender : TObject);
		procedure OnInputEvalReady(const evalvalue : AnsiString);
end;

var
	MainForm : TMainForm;

implementation

uses
{$IFDEF WIN32}
	ShellAPI, IniFiles, Clipbrd, MultiLangSupport, version,
	datamod, NewProjectFrm, AboutFrm, PrintFrm,
	CompOptionsFrm, EditorOptFrm, IncrementalFrm, EnviroFrm,
	SynEdit, Math, ImageTheme,
	Types, FindFrm, Prjtypes, devExec,
	NewTemplateFrm, FunctionSearchFrm, NewMemberFrm, NewVarFrm, NewClassFrm,
	ProfileAnalysisFrm, FilePropertiesFrm, AddToDoFrm, ViewToDoFrm,
	ImportMSVCFrm, ImportCBFrm, CPUFrm, FileAssocs, TipOfTheDayFrm, SplashFrm,
	WindowListFrm, devThemes, RemoveUnitFrm, ParamsFrm, WebUpdate, ProcessListFrm, SynEditHighlighter;
{$ENDIF}
{$IFDEF LINUX}
	Xlib, IniFiles, QClipbrd, MultiLangSupport, version,
	devcfg, datamod, NewProjectFrm, AboutFrm, PrintFrm,
	CompOptionsFrm, EditorOptfrm, Incrementalfrm, Search_Center, Envirofrm,
	QSynEdit, QSynEditTypes,
	debugfrm, Prjtypes, devExec,
	NewTemplateFm, FunctionSearchFm, NewMemberFm, NewVarFm, NewClassFm,
	ProfileAnalysisFm, FilePropertiesFm, AddToDoFm, ViewToDoFm,
	ImportMSVCFm, CPUFrm, FileAssocs, TipOfTheDayFm, Splash,
	WindowListFrm, ParamsFrm, WebUpdate, ProcessListFrm, ModifyVarFrm;
{$ENDIF}

{$R *.dfm}

procedure TMainForm.LoadTheme;
var
	Idx: Integer;
begin
	if devImageThemes.IndexOf(devData.Theme) = -1 then
		devData.Theme := devImageThemes.Themes[0].Title; // 0 = New look (see ImageTheme.pas)

	// make sure the theme in question is in the list
	Idx := devImageThemes.IndexOf(devData.Theme);
	if Idx <> -1 then begin
		devImageThemes.ActivateTheme(devData.Theme);

		with devImageThemes do begin
			ActionList.Images      := CurrentTheme.MenuImages;
			MainMenu.Images        := CurrentTheme.MenuImages;
			ProjectView.Images     := CurrentTheme.ProjectImages;
			MessageControl.Images  := CurrentTheme.MenuImages;
			tbMain.Images          := CurrentTheme.MenuImages;
			tbCompile.Images       := CurrentTheme.MenuImages;
			tbProject.Images       := CurrentTheme.MenuImages;
			tbClasses.Images       := CurrentTheme.MenuImages;
			tbedit.Images          := CurrentTheme.MenuImages;
			tbSearch.Images        := CurrentTheme.MenuImages;
			tbSpecials.Images      := CurrentTheme.SpecialImages;
			HelpMenu.SubMenuImages := CurrentTheme.HelpImages;
			DebugVarsPopup.Images  := CurrentTheme.MenuImages;
			ClassBrowser.Images    := CurrentTheme.BrowserImages;
		end;
	end;

	fTools.BuildMenu;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	if assigned(fProject) then
		actCloseProject.Execute;
	if Assigned(fProject) then begin
		Action := caNone;
		Exit;
	end;

	if PageControl.PageCount > 0 then
		actCloseAll.execute;
	if PageControl.PageCount > 0 then begin
		Action := caNone;
		Exit;
	end;

	devData.ClassView:=LeftPageControl.ActivePage=ClassSheet;
	devData.ToolbarMainX:=tbMain.Left;
	devData.ToolbarMainY:=tbMain.Top;
	devData.ToolbarEditX:=tbEdit.Left;
	devData.ToolbarEditY:=tbEdit.Top;
	devData.ToolbarCompileX:=tbCompile.Left;
	devData.ToolbarCompileY:=tbCompile.Top;
	devData.ToolbarProjectX:=tbProject.Left;
	devData.ToolbarProjectY:=tbProject.Top;
	devData.ToolbarSpecialsX:=tbSpecials.Left;
	devData.ToolbarSpecialsY:=tbSpecials.Top;
	devData.ToolbarSearchX:=tbSearch.Left;
	devData.ToolbarSearchY:=tbSearch.Top;
	devData.ToolbarClassesX:=tbClasses.Left;
	devData.ToolbarClassesY:=tbClasses.Top;

	devData.ProjectWidth:=LeftPageControl.Width;
	devData.OutputHeight:=fPreviousHeight;

	if fRemoveOptions then
		fOptionsDir := devDirs.Config;

	// Save and free options
	SaveOptions;
	fTools.Free; // This one needs devDirs!
	FinalizeOptions;

	Action := caFree;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
	fostruct : SHFILEOPSTRUCT;
	DirFrom,DirTo : array[0..MAX_PATH] of char;
begin
	// Prevent singleton reinit
	DontRecreateSingletons := true;

	// Free non-singletons
	devImageThemes.Free;
	devTheme.Free;
	fCompiler.Free;
	fDebugger.Free;
	devExecutor.Free;
	dmMain.Free;

	// Free these singletons on the very last
	Lang.Free;
	devData.Free;

	// Copy and delete
	if fRemoveOptions then begin

		FillChar(DirFrom, Sizeof(DirFrom),0);
		StrPCopy(DirFrom, fOptionsDir);

		FillChar(DirTo, Sizeof(DirFrom),0);
		StrPCopy(DirTo, ExcludeTrailingBackslash(fOptionsDir) + 'Backup' + pd);

		FillChar(fostruct,Sizeof(fostruct),0);
		with fostruct do begin
			Wnd := 0;
			pFrom := @DirFrom;
			pTo := @DirTo;
			wFunc := FO_COPY;
			fFlags := FOF_ALLOWUNDO or FOF_SILENT or FOF_NOCONFIRMATION;
		end;
		SHFileOperation(fostruct);

		FillChar(fostruct,Sizeof(fostruct),0);
		with fostruct do begin
			Wnd := 0;
			pFrom := @DirFrom;
			wFunc := FO_DELETE;
			fFlags := FOF_ALLOWUNDO or FOF_SILENT or FOF_NOCONFIRMATION;
		end;
		SHFileOperation(fostruct);
	end;
end;

procedure TMainForm.BuildBookMarkMenus;
var
	idx: integer;
	Text: AnsiString;
	Shortcutnumber : Word;
	GItem,TItem: TMenuItem;
begin
	Text:= Lang[ID_MARKTEXT];
	ToggleBookMarksItem.Clear;
	GotoBookmarksItem.Clear;
	for idx:= 1 to 9 do begin
		Shortcutnumber := Ord(inttostr(idx)[1]);

		TItem:= TMenuItem.Create(ToggleBookmarksItem);
		TItem.Caption:= format('%s &%d', [Text, idx]);
		TItem.OnClick:= ToggleBookmarkClick;
		TItem.Tag:= idx;
		TItem.ShortCut:= ShortCut(Shortcutnumber,[ssCtrl]);
		ToggleBookmarksItem.Add(TItem);

		GItem:= TMenuItem.Create(GotoBookmarksItem);
		GItem.Caption:= TItem.Caption;
		GItem.OnClick:= GotoBookmarkClick;
		GItem.Tag:= idx;
		GItem.ShortCut:= ShortCut(Shortcutnumber,[ssAlt]);
		GotoBookmarksItem.Add(GItem);
	end;

	CloneMenu(ToggleBookmarksItem, TogglebookmarksPopItem);
	CloneMenu(GotoBookmarksItem, GotobookmarksPopItem);
end;

procedure TMainForm.SetHints;
var
	idx: integer;
begin
	for idx:= 0 to pred(ActionList.ActionCount) do
		TCustomAction(ActionList.Actions[idx]).Hint := StripHotKey(TCustomAction(ActionList.Actions[idx]).Caption);
end;

// allows user to drop files from explorer on to form
procedure TMainForm.WMDropFiles(var msg: TMessage);
var
	idx,
	idx2,
	count: integer;
	szFileName: array[0..260] of char;
	pt: TPoint;
	hdl: THandle;
	ProjectFN: AnsiString;
begin
	try
		ProjectFN := '';
		hdl:= THandle(msg.wParam);
		count:= DragQueryFile(hdl, $FFFFFFFF, nil, 0);
		DragQueryPoint(hdl, pt);

		for idx:= 0 to pred(count) do begin
			DragQueryFile(hdl, idx, szFileName, sizeof(szFileName));

			// Is there a project?
			if SameText(ExtractFileExt(szFileName), DEV_EXT) then begin
				ProjectFN := szFileName;
				Break;
			end;
		end;

		if Length(ProjectFN) > 0 then
			OpenProject(ProjectFN)
		else
			for idx:= 0 to pred(count) do begin
				DragQueryFile(hdl, idx, szFileName, sizeof(szFileName));
				idx2:= FileIsOpen(szFileName);
				if idx2 <> -1 then
					TEditor(PageControl.Pages[idx2].Tag).Activate
				else // open file
					OpenFile(szFileName)
				end;
	finally
		msg.Result:= 0;
		DragFinish(THandle(msg.WParam));
	end;
end;

procedure TMainForm.LoadText;
var
	len : integer;
begin
	// Set interface font
	Font.Name := devData.InterfaceFont;
	Font.Size := devData.InterfaceFontSize;

	// Menus
	FileMenu.Caption :=					Lang[ID_MNU_FILE];
	EditMenu.Caption :=					Lang[ID_MNU_EDIT];
	SearchMenu.Caption:=				Lang[ID_MNU_SEARCH];
	ViewMenu.Caption:=					Lang[ID_MNU_VIEW];
	ProjectMenu.Caption:=				Lang[ID_MNU_PROJECT];
	ExecuteMenu.Caption:=				Lang[ID_MNU_EXECUTE];
	DebugMenu.Caption:=					Lang[ID_MNU_DEBUG];
	ToolsMenu.Caption:=					Lang[ID_MNU_TOOLS];
	WindowMenu.Caption:=				Lang[ID_MNU_WINDOW];
	HelpMenu.Caption:=					Lang[ID_MNU_HELP];

	// file menu
	mnuNew.Caption:=					Lang[ID_SUB_NEW];
	NewFileBtn.Hint:=					Lang[ID_HINT_NEW];
	actNewSource.Caption:=				Lang[ID_ITEM_NEWSOURCE];
	actNewProject.Caption:=				Lang[ID_ITEM_NEWPROJECT];
	actNewTemplate.Caption:=			Lang[ID_ITEM_NEWTEMPLATE];
	actNewRes.Caption:=					Lang[ID_ITEM_NEWRESOURCE];

	actOpen.Caption:=					Lang[ID_ITEM_OPEN];
	ReOpenItem.Caption:=				Lang[ID_SUB_REOPEN];
	actHistoryClear.Caption:=			Lang[ID_ITEM_CLEARHISTORY];
	actSave.Caption:=					Lang[ID_ITEM_SAVEFILE];
	actSaveAs.Caption:=					Lang[ID_ITEM_SAVEAS];
	SaveProjectAsItem.Caption :=		Lang[ID_ITEM_SAVEASPROJECT];
	actSaveAll.Caption:=				Lang[ID_ITEM_SAVEALL];
	actClose.Caption:=					Lang[ID_ITEM_CLOSEFILE];
	actCloseAll.Caption:=				Lang[ID_ITEM_CLOSEALL];
	actCloseProject.Caption:=			Lang[ID_ITEM_CLOSEPROJECT];

	actMsgCut.Caption :=				Lang[ID_ITEM_CUT];
	actMsgCopy.Caption :=				Lang[ID_ITEM_COPY];
	actMsgCopyAll.Caption :=			Lang[ID_ITEM_COPYALL];
	actMsgPaste.Caption :=				Lang[ID_ITEM_PASTE];
	actMsgSelAll.Caption :=				Lang[ID_ITEM_SELECTALL];
	actMsgSaveAll.Caption :=			Lang[ID_ITEM_SAVEALL];
	actMsgClear.Caption :=				Lang[ID_ITEM_CLEAR];

	actFileProperties.Caption:=			Lang[ID_ITEM_PROPERTIES];

	ImportItem.Caption:=				Lang[ID_SUB_IMPORT];
	actImportMSVC.Caption:=				Lang[ID_MSVC_MENUITEM];

	ExportItem.Caption:=				Lang[ID_SUB_EXPORT];
	actXHTML.Caption:=					Lang[ID_ITEM_EXPORTHTML];
	actXRTF.Caption:=					Lang[ID_ITEM_EXPORTRTF];
	actXTex.Caption:=					Lang[ID_ITEM_EXPORTTEX];
	actXProject.Caption:=				Lang[ID_ITEM_EXPORTPROJECT];

	actPrint.Caption:=					Lang[ID_ITEM_PRINT];
	actPrintSU.Caption:=				Lang[ID_ITEM_PRINTSETUP];
	actExit.Caption:=					Lang[ID_ITEM_EXIT];

	// Edit menu
	actUndo.Caption:=					Lang[ID_ITEM_UNDO];
	actRedo.Caption:=					Lang[ID_ITEM_REDO];
	actCut.Caption:=					Lang[ID_ITEM_CUT];
	actCopy.Caption:=					Lang[ID_ITEM_COPY];
	actPaste.Caption:=					Lang[ID_ITEM_PASTE];
	actSelectAll.Caption:=				Lang[ID_ITEM_SELECTALL];
	InsertItem.Caption:=				Lang[ID_SUB_INSERT];
	ToggleBookmarksItem.Caption:=		Lang[ID_SUB_TOGGLEMARKS];
	GotoBookMarksItem.Caption:=			Lang[ID_SUB_GOTOMARKS];
	DateTimeMenuItem.Caption:=			Lang[ID_ITEM_DATETIME];
	CommentHeaderMenuItem.Caption:=		Lang[ID_ITEM_COMMENTHEADER];
	actComment.Caption:=				Lang[ID_ITEM_COMMENTSELECTION];
	actUncomment.Caption:=				Lang[ID_ITEM_UNCOMMENTSELECTION];
	actIndent.Caption:=					Lang[ID_ITEM_INDENTSELECTION];
	actUnindent.Caption:=				Lang[ID_ITEM_UNINDENTSELECTION];
	actSwapHeaderSource.Caption:=		Lang[ID_ITEM_SWAPHEADERSOURCE];
	actCollapse.Caption:=				Lang[ID_ITEM_COLLAPSEALL];
	actUnCollapse.Caption:=				Lang[ID_ITEM_UNCOLLAPSEALL];

	// Mainform toolbar buttons
	actInsert.Caption:=					Lang[ID_TB_INSERT];
	actToggle.Caption:=					Lang[ID_TB_TOGGLE];
	actGoto.Caption:=					Lang[ID_TB_GOTO];

	// Search Menu
	actFind.Caption:=					Lang[ID_ITEM_FIND];
	actFindAll.Caption:=				Lang[ID_ITEM_FINDINALL];
	actReplace.Caption:=				Lang[ID_ITEM_REPLACE];
	actReplaceAll.Caption:=				Lang[ID_ITEM_REPLACEFILES];
	actGotoLine.Caption:=				Lang[ID_ITEM_GOTO];
	actIncremental.Caption:=			Lang[ID_ITEM_INCREMENTAL];
	actGotoFunction.Caption:=			Lang[ID_ITEM_GOTOFUNCTION];

	// View Menu
	actProjectManager.Caption:=			Lang[ID_ITEM_PROJECTVIEW];
	actStatusbar.Caption:=				Lang[ID_ITEM_Statusbar];
	CompilerOutputItem.Caption:=		Lang[ID_SUB_COMPOUTPUT];
	ToolBarsItem.Caption:=				Lang[ID_SUB_TOOLBARS];
	actCompOutput.Caption:=				Lang[ID_ITEM_COMPOUTALWAYS];
	actCompOnNeed.Caption:=				Lang[ID_ITEM_COMPOUTONNEED];

	ToolMainItem.Caption:=				Lang[ID_TOOLMAIN];
	ToolEditItem.Caption:=				Lang[ID_TOOLEDIT];
	ToolSearchItem.Caption:=			Lang[ID_TOOLSEARCH];
	ToolCompileAndRunItem.Caption:=		Lang[ID_TOOLCOMPRUN];
	ToolProjectItem.Caption:=			Lang[ID_TOOLPROJECT];
	ToolSpecialsItem.Caption:=			Lang[ID_TOOLSPECIAL];
	ToolClassesItem.Caption:=			Lang[ID_LP_CLASSES];

	tbMain.Caption:=					Lang[ID_TOOLMAIN];
	tbEdit.Caption:=					Lang[ID_TOOLEDIT];
	tbSearch.Caption:=					Lang[ID_TOOLSEARCH];
	tbCompile.Caption:=					Lang[ID_TOOLCOMPRUN];
	tbProject.Caption:=					Lang[ID_TOOLPROJECT];
	tbSpecials.Caption:=				Lang[ID_TOOLSPECIAL];
	actViewToDoList.Caption:=			Lang[ID_VIEWTODO_MENUITEM];
	FloatingPojectManagerItem.Caption:=	Lang[ID_ITEM_FLOATWINDOW];
	FloatingReportWindowItem.Caption:=	Lang[ID_ITEM_FLOATREPORT];
	GotoprojectmanagerItem.Caption :=	Lang[ID_ITEM_GOTOPROJECTVIEW];
	GoToClassBrowserItem.Caption :=		Lang[ID_ITEM_GOTOCLASSBROWSER];

	// Project menu
	actProjectNew.Caption:=				Lang[ID_ITEM_NEWFILE];
	actProjectAdd.Caption:=				Lang[ID_ITEM_ADDFILE];
	actProjectRemove.Caption:=			Lang[ID_ITEM_REMOVEFILE];
	actProjectOptions.Caption:=			Lang[ID_ITEM_PROJECTOPTIONS];
	actProjectMakeFile.Caption:=		Lang[ID_ITEM_EDITMAKE];

	// Execute menu
	actCompile.Caption:=				Lang[ID_ITEM_COMP];
	actCompileCurrentFile.Caption:=		Lang[ID_ITEM_COMPCURRENT];
	actRun.Caption:=					Lang[ID_ITEM_RUN];
	actCompRun.Caption:=				Lang[ID_ITEM_COMPRUN];
	actRebuild.Caption:=				Lang[ID_ITEM_REBUILD];
	actClean.Caption:=					Lang[ID_ITEM_CLEAN];
	actSyntaxCheck.Caption:=			Lang[ID_ITEM_SYNTAXCHECK];
	actProgramReset.Caption:=			Lang[ID_ITEM_PROGRAMRESET];
	actProfileProject.Caption:=			Lang[ID_ITEM_PROFILE];
	actDeleteProfileProject.Caption:=	Lang[ID_ITEM_DELPROFINFORMATION];
	actAbortCompilation.Caption:=		Lang[ID_ITEM_ABORTCOMP];
	actExecParams.Caption:=				Lang[ID_ITEM_EXECPARAMS];

	// Debug menu
	actDebug.Caption:=					Lang[ID_ITEM_DEBUG];
	actBreakPoint.Caption:=				Lang[ID_ITEM_TOGGLEBREAK];
	actAddWatch.Caption:=				Lang[ID_ITEM_WATCHADD];
	actEditWatch.Caption:=				Lang[ID_ITEM_WATCHEDIT];
	actModifyWatch.Caption :=			Lang[ID_ITEM_MODIFYVALUE];
	actRemoveWatch.Caption:=			Lang[ID_ITEM_WATCHREMOVE];
	actNextLine.Caption:=				Lang[ID_ITEM_STEPNEXT];
	actStepLine.Caption:=				Lang[ID_ITEM_STEPINTO];
	actStepOver.Caption:=				Lang[ID_ITEM_STEPOVER];
	actWatchItem.Caption:=				Lang[ID_ITEM_WATCHITEMS];
	actStopExecute.Caption:=			Lang[ID_ITEM_STOPEXECUTION];
	actViewCPU.Caption:=				Lang[ID_ITEM_CPUWINDOW];
	ClearallWatchPop.Caption :=			Lang[ID_ITEM_CLEARALL];
	actNextIns.Caption:=				Lang[ID_ITEM_NEXTINS];
	actStepIns.Caption:=				Lang[ID_ITEM_STEPINS];
	actSkipFunction.Caption:=			Lang[ID_ITEM_SKIPFUNCTION];

	// Tools menu
	actCompOptions.Caption:=			Lang[ID_ITEM_COMPOPTIONS];
	actEnviroOptions.Caption:=			Lang[ID_ITEM_ENVIROOPTIONS];
	actEditorOptions.Caption:=			Lang[ID_ITEM_EDITOROPTIONS];
	actConfigTools.Caption:=			Lang[ID_ITEM_TOOLCONFIG];
	actConfigdevShortcuts.Caption:=		Lang[ID_ITEM_SHORTCUTSCONFIG];

	// CVS menu
	mnuCVSCurrent.Caption:=				Lang[ID_ITEM_CVSCURRENT];
	mnuCVSWhole.Caption:=				Lang[ID_ITEM_CVSWHOLE];
	actCVSImport.Caption:=				Lang[ID_CVS_IMPORT];
	actCVSCheckout.Caption:=			Lang[ID_CVS_CHECKOUT];
	actCVSUpdate.Caption:=				Lang[ID_CVS_UPDATE];
	actCVSCommit.Caption:=				Lang[ID_CVS_COMMIT];
	actCVSDiff.Caption:=				Lang[ID_CVS_DIFF];
	actCVSLog.Caption:=					Lang[ID_CVS_LOG];
	actCVSAdd.Caption:=					Lang[ID_CVS_ADD];
	actCVSRemove.Caption:=				Lang[ID_CVS_REMOVE];

	// Window menu
	if devData.fullScreen then
		actFullScreen.Caption:=			Lang[ID_ITEM_FULLSCRBACK]
	else
		actFullScreen.Caption:=			Lang[ID_ITEM_FULLSCRMODE];
	actNext.Caption:=					Lang[ID_ITEM_NEXT];
	actPrev.Caption:=					Lang[ID_ITEM_PREV];
	ListItem.Caption :=					Lang[ID_ITEM_LIST];

	// Help menu
	HelpMenuItem.Caption:=				Lang[ID_ITEM_HELPDEVCPP];
	actUpdateCheck.Caption:=			Lang[ID_ITEM_UPDATECHECK];
	actAbout.Caption:=					Lang[ID_ITEM_ABOUT];
	actShowTips.Caption:=				Lang[ID_TIPS_CAPTION];

	// units pop
	actUnitRemove.Caption:=				Lang[ID_POP_REMOVE];
	actUnitRename.Caption:=				Lang[ID_POP_RENAME];
	actUnitOpen.Caption:=				Lang[ID_POP_OPEN];
	actUnitClose.Caption:=				Lang[ID_POP_CLOSE];
	actProjectNewFolder.Caption:=		Lang[ID_POP_ADDFOLDER];
	actProjectRemoveFolder.Caption:=	Lang[ID_POP_REMOVEFOLDER];
	actProjectRenameFolder.Caption:=	Lang[ID_POP_RENAMEFOLDER];
	mnuOpenWith.Caption:=				Lang[ID_POP_OPENWITH];

	// editor pop
	UndoPopItem.Caption:=				Lang[ID_ITEM_UNDO];
	RedoPopItem.Caption:=				Lang[ID_ITEM_REDO];
	CutPopItem.Caption:=				Lang[ID_ITEM_CUT];
	CopyPopItem.Caption:=				Lang[ID_ITEM_COPY];
	PastePopItem.Caption:=				Lang[ID_ITEM_PASTE];
	SelectAllPopItem.Caption:=			Lang[ID_ITEM_SELECTALL];
	InsertPopItem.Caption:=				Lang[ID_SUB_INSERT];
	TogglebookmarksPopItem.Caption:=	Lang[ID_SUB_TOGGLEMARKS];
	GotobookmarksPopItem.Caption:=		Lang[ID_SUB_GOTOMARKS];
	actCloseAllButThis.Caption:=		Lang[ID_ITEM_CLOSEALLBUTTHIS];
	actAddToDo.Caption:=				Lang[ID_ADDTODO_MENUITEM];

	// class browser popup
	actBrowserGotoDecl.Caption:=		Lang[ID_POP_GOTODECL];
	actBrowserGotoImpl.Caption:=		Lang[ID_POP_GOTOIMPL];
	actBrowserNewClass.Caption:=		Lang[ID_POP_NEWCLASS];
	actBrowserNewMember.Caption:=		Lang[ID_POP_NEWMEMBER];
	actBrowserNewVar.Caption:=			Lang[ID_POP_NEWVAR];
	mnuBrowserViewMode.Caption:=		Lang[ID_POP_VIEWMODE];
	actBrowserViewAll.Caption:=			Lang[ID_POP_VIEWALLFILES];
	actBrowserViewProject.Caption:=		Lang[ID_POP_VIEWPROJECT];
	actBrowserViewCurrent.Caption:=		Lang[ID_POP_VIEWCURRENT];
	actBrowserAddFolder.Caption:=		Lang[ID_POP_ADDFOLDER];
	actBrowserRemoveFolder.Caption:=	Lang[ID_POP_REMOVEFOLDER];
	actBrowserRenameFolder.Caption:=	Lang[ID_POP_RENAMEFOLDER];
	actBrowserUseColors.Caption:=		Lang[ID_POP_USECOLORS];
	actBrowserShowInherited.Caption:=	Lang[ID_POP_SHOWINHERITED];

	actGotoDeclEditor.Caption:=			Lang[ID_POP_GOTODECL];
	actGotoImplEditor.Caption:=			Lang[ID_POP_GOTOIMPL];

	// Message Control tabs
	CompSheet.Caption :=				Lang[ID_SHEET_COMP];
	CloseSheet.Caption :=				Lang[ID_SHEET_CLOSE];
	ResSheet.Caption :=					Lang[ID_SHEET_RES];
	LogSheet.Caption :=					Lang[ID_SHEET_COMPLOG];
	FindSheet.Caption :=				Lang[ID_SHEET_FIND];
	DebugSheet.Caption:=				Lang[ID_SHEET_DEBUG];
	DebugLeftSheet.Caption:=			Lang[ID_SHEET_DEBUG];

	// controls
	CompilerOutput.Columns[0].Caption:=	Lang[ID_COL_LINE];
	CompilerOutput.Columns[1].Caption:=	Lang[ID_COL_COL];
	CompilerOutput.Columns[2].Caption:=	Lang[ID_COL_FILE];
	CompilerOutput.Columns[3].Caption:=	Lang[ID_COL_MSG];
	FindOutput.Columns[0].Caption :=	Lang[ID_COL_LINE];
	FindOutput.Columns[1].Caption :=	Lang[ID_COL_COL];
	FindOutput.Columns[2].Caption :=	Lang[ID_COL_FILE];
	FindOutput.Columns[3].Caption :=	Lang[ID_COL_MSG];
	ErrorLabel.Caption :=				Lang[ID_TOTALERRORS];
	SizeOfOutput.Caption :=				Lang[ID_OUTPUTSIZE];
	ProjectSheet.Caption :=				Lang[ID_LP_PROJECT];
	ClassSheet.Caption :=				Lang[ID_LP_CLASSES];

	lblSendCommandGdb.Caption:=			Lang[ID_DEB_SENDGDBCOMMAND];
	lblEvaluate.Caption:=				Lang[ID_DEB_EVALUATE];

	// Adapt UI spacing to translation text length
	len := Canvas.TextWidth(lblSendCommandGdb.Caption);
	edGdbCommand.Left := len + 10;
	edGdbCommand.Width := DebugOutput.Width - len - 6;

	len := Canvas.TextWidth(lblEvaluate.Caption);
	EvaluateInput.Left := len + 10;
	EvaluateInput.Width := EvalOutput.Width - len - 6;

	pnlFull.Caption:=					Format(Lang[ID_FULLSCREEN_MSG], [DEVCPP, DEVCPP_VERSION]);

	BuildBookMarkMenus;
	SetHints;
end;

function TMainForm.FileIsOpen(const s: AnsiString; inPrj: boolean = FALSE): integer;
var
	e: TEditor;
begin
	for result:= 0 to pred(PageControl.PageCount) do begin
		e:= GetEditor(result);
		if e.filename <> '' then begin
			if SameText(e.FileName, s) then
				if (not inprj) or (e.InProject) then exit;
			end
		else
			if SameText(e.TabSheet.Caption, ExtractfileName(s)) then
				if (not inprj) or (e.InProject) then exit;
	end;
	result:= -1;
end;

function TMainForm.SaveFileAs(e : TEditor): Boolean;
var
 dext,
 flt,
 s: AnsiString;
 idx: integer;
 CFilter, CppFilter, HFilter: Integer;
begin
	Result := True;
	idx:= -1;
	if assigned(fProject) then begin
		if e.FileName = '' then
			idx:= fProject.GetUnitFromString(e.TabSheet.Caption)
		else
			idx:= fProject.Units.Indexof(e.FileName);
		if fProject.Options.UseGPP then begin
			BuildFilter(flt, [FLT_CPPS, FLT_CS, FLT_HEADS]);
			dext:= CPP_EXT;
			CFilter := 3;
			CppFilter := 2;
			HFilter := 4;
		end else begin
			BuildFilter(flt, [FLT_CS, FLT_CPPS, FLT_HEADS]);
			dext:= C_EXT;
			CFilter := 2;
			CppFilter := 3;
			HFilter := 4;
		end;
		if e.IsRes then begin
			BuildFilter(flt, [FLT_RES]);
			dext:= RC_EXT;
			CFilter := 2;
			CppFilter := 2;
			HFilter := 2;
		end;
	end else begin
		BuildFilter(flt, ftAll);
		if e.IsRes then
			dext:= RC_EXT
		else
			dext:= CPP_EXT;
		CFilter := 5;
		CppFilter := 6;
		HFilter := 3;
	end;

	if e.FileName = '' then
		s:= e.TabSheet.Caption
	else
		s:= e.FileName;

	with dmMain.SaveDialog do begin
		Title:= Lang[ID_NV_SAVEFILE];
		Filter:= flt;

		// select appropriate filter
		if (CompareText(ExtractFileExt(s), '.h')   = 0) or
		   (CompareText(ExtractFileExt(s), '.hpp') = 0) or
		   (CompareText(ExtractFileExt(s), '.hh')  = 0) then begin
			 FilterIndex := HFilter;
		end else begin
			if Assigned(fProject) then
				if fProject.Options.useGPP then
					FilterIndex := CppFilter
				else
					FilterIndex := CFilter
			else
				FilterIndex := CppFilter;
		end;

		FileName:= s;
		s:= ExtractFilePath(s);
		if (s <> '') or not Assigned(fProject) then
			InitialDir:= s
		else
			InitialDir:=fProject.Directory;

		if Execute then begin
			s:= FileName;
			if FileExists(s) and (MessageDlg(Lang[ID_MSG_FILEEXISTS],mtWarning, [mbYes, mbNo], 0) = mrNo) then
				exit;

			e.FileName := s;

			try
				e.Text.UnCollapsedLines.SaveToFile(s);
				e.Text.Modified := FALSE;
				e.New:= FALSE;
			except
				MessageDlg(Lang[ID_ERR_SAVEFILE] +' "' +s +'"', mtError, [mbOk], 0);
				Result := False;
			end;

			if assigned(fProject) then
				fProject.SaveUnitAs(idx, e.FileName)
			else
				e.TabSheet.Caption:= ExtractFileName(e.FileName);

			if ClassBrowser.Enabled then begin

				// We haven't scanned it yet...
				CppParser.AddFileToScan(e.FileName);
				CppParser.ParseList;
			end;
		end else
			Result := False;
	 end;
end;

function TMainForm.SaveFile(e : TEditor): Boolean;
var
	idx: Integer;
	wa: boolean;
begin
	Result := True;
	if FileExists(e.FileName) and (FileGetAttr(e.FileName) and faReadOnly <> 0) then begin
		// file is read-only
		if MessageDlg(Format(Lang[ID_MSG_FILEISREADONLY], [e.FileName]), mtConfirmation, [mbYes, mbNo], 0) = mrNo then
			Exit;
		if FileSetAttr(e.FileName, FileGetAttr(e.FileName)-faReadOnly) <> 0 then begin
			MessageDlg(Format(Lang[ID_MSG_FILEREADONLYERROR], [e.FileName]), mtError, [mbOk], 0);
			Exit;
		end;
	end;

	wa:=devFileMonitor.Active;
	devFileMonitor.Deactivate;

	if (not e.new) and e.Text.Modified then begin
		if Assigned(fProject) and e.InProject then begin
			try
				idx:= fProject.GetUnitFromEditor(e);
				if idx = -1 then
					MessageDlg(Format(Lang[ID_ERR_SAVEFILE], [e.FileName]), mtError, [mbOk], 0)
				else
					fProject.units[idx].Save;
			except
				MessageDlg(Format(Lang[ID_ERR_SAVEFILE], [e.FileName]), mtError, [mbOk], 0);
				Result := False;
				Exit;
			end;
			try
				if (idx <> -1) and ClassBrowser.Enabled then begin
					CppParser.ReParseFile(e.FileName,false); // don't need to scan for the first time
				end;
			except
				MessageDlg(Format('Error reparsing file %s', [e.FileName]), mtError, [mbOk], 0);
				Result := False;
			end;
		end else // stand alone file (should have fullpath in e.filename)
			try
				e.Text.UnCollapsedLines.SaveToFile(e.FileName);
				e.Text.Modified := false;

				if ClassBrowser.Enabled then begin
					CppParser.ReParseFile(e.FileName,false); // don't need to scan for the first time
				end;
			except
				MessageDlg(Format(Lang[ID_ERR_SAVEFILE], [e.FileName]), mtError, [mbOk], 0);
				Result := False;
			end
	end else if e.New then
		Result := SaveFileAs(e);
	if wa then
		devFileMonitor.Activate;
end;

function TMainForm.CloseEditor(index: integer): Boolean;
var
	e: TEditor;
	s : AnsiString;
begin
	Result := False;
	e := GetEditor(index);
	if assigned(e) then begin

		// Ask user if he wants to save
		if e.Text.Modified then begin

			if e.FileName = '' then
				s:= e.TabSheet.Caption
			else
				s:= e.FileName;

			case MessageDlg(format(Lang[ID_MSG_ASKSAVECLOSE], [s]),mtConfirmation, mbYesNoCancel, 0) of
				mrYes:
					SaveFile(e);
				mrCancel:
					Exit; // stop closing
			end;
		end;

		// We're allowed to close stuff...
		Result := True;
		if not e.InProject then begin
			dmMain.AddtoHistory(e.FileName);
			FreeAndNil(e);
		end else begin
			if e.IsRes and not Assigned(fProject) then
				FreeAndNil(e)
			else if assigned(fProject) then
				fProject.CloseUnit(fProject.Units.Indexof(e));
		end;

		SetStatusbarLineCol;
		UpdateAppTitle;

		e:=GetEditor;
		if Assigned(e) then begin
			ClassBrowser.CurrentFile:=e.FileName;
			e.Text.SetFocus;
		end else if (ClassBrowser.ShowFilter=sfCurrent) or not Assigned(fProject) then begin
			CppParser.Reset;
			ClassBrowser.Clear;
		end;
	end;
end;

procedure TMainForm.SetStatusbarLineCol;
var
	e: TEditor;
begin
	e:= GetEditor;
	if Assigned(e) then begin
		Statusbar.Panels[0].Text := format(Lang[ID_STATUSBARPLUS],[ e.Text.GetRealLineNumber(e.Text.DisplayY),
																	e.Text.DisplayX,
																	e.Text.SelLength,
																	e.Text.UnCollapsedLines.Count,
																	e.Text.UnCollapsedLinesLength]);
	end else begin
		PageControl.Visible := false;
		Statusbar.Panels[0].Text := '';
		Statusbar.Panels[1].Text := '';
		StatusBar.Panels[2].Text := '';
	end;
end;

procedure TMainForm.SetStatusbarMessage(const msg:AnsiString);
begin
	Statusbar.Panels[2].Text:= msg;
end;

procedure TMainForm.ToggleBookmarkClick(Sender: TObject);
var
 e: TEditor;
begin
	e:= GetEditor;
	with Sender as TMenuItem do
		if Assigned(e) then begin
			Checked:= not Checked;
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
	e:=GetEditor;
	if assigned(e) then
		e.Text.GotoBookMark((Sender as TMenuItem).Tag);
end;

procedure TMainForm.OpenCloseMessageSheet(_Show: boolean);
begin
	if Assigned(ReportToolWindow) then
		exit;

	// Switch between open and close
	with MessageControl do
		if _Show then
			Height := fPreviousHeight
		else begin
			Height := Height - CompSheet.Height;
			ActivePageIndex:= -1;
		end;
	CloseSheet.TabVisible:= _Show;
	SplitterBottom.Visible := _Show;
	Statusbar.Top:= Self.ClientHeight;
end;

procedure TMainForm.MessageControlChange(Sender: TObject);
begin
	if MessageControl.ActivePage = CloseSheet then begin
		if Assigned(ReportToolWindow) then begin
			ReportToolWindow.Close;
			MessageControl.ActivePageIndex := 0;
		end else
			OpenCloseMessageSheet(false);
	end else
		OpenCloseMessageSheet(true);
end;

procedure TMainForm.MRUClick(Sender: TObject);
var
	s : AnsiString;
begin
	s:= dmMain.MRU[TMenuItem(Sender).Tag];
	if GetFileTyp(s) = utPrj then
		OpenProject(s)
	else
		OpenFile(s);
end;

procedure TMainForm.CodeInsClick(Sender: TObject);
var
 e: TEditor;
begin
	e:= GetEditor;
	if assigned(e) then begin
		e.Text.BeginUpdate;
		e.InsertString(dmMain.CodeInserts[TMenuItem(Sender).Tag].Line, TRUE);
		e.Text.EndUpdate;
	end;
end;

procedure TMainForm.ToolItemClick(Sender: TObject);
var
	idx: integer;
begin
	idx:= (Sender as TMenuItem).Tag;
//	MsgBox('icon index:' + inttostr((Sender as TMenuItem).ImageIndex),(Sender as TMenuItem).Caption);

	with fTools.ToolList[idx]^ do
		ExecuteFile(ParseParams(Exec), ParseParams(Params), ParseParams(WorkDir), SW_SHOW);
end;

procedure TMainForm.OpenProject(const s: AnsiString);
var
	s2: AnsiString;
begin
	if Assigned(fProject) then begin
		if fProject.Name = '' then
			s2:= fProject.FileName
		else
			s2:= fProject.Name;
		if (MessageDlg(format(Lang[ID_MSG_CLOSEPROJECTPROMPT], [s2]), mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
			actCloseProject.Execute
		else
			exit;
	end;

	fProject := TProject.Create(s, DEV_INTERNAL_OPEN);
	if fProject.FileName <> '' then begin
		fCompiler.Project:= fProject;
		fCompiler.RunParams:=fProject.CmdLineArgs;
		fCompiler.Target:=ctProject;

		dmMain.RemoveFromHistory(s);

		// if project manager isn't open then open it
		if not devData.ProjectView then
			actProjectManager.Execute;

		CheckForDLLProfiling;

		UpdateAppTitle;
		ScanActiveProject;
	end else begin
		fProject.Free;
		fProject := nil;
	end;
end;

procedure TMainForm.OpenFile(const s : AnsiString);
var
	e: TEditor;
	idx : integer;
begin
	idx := FileIsOpen(s);
	if (idx <> -1) then begin
		GetEditor(idx).Activate;
		exit;
	end;

	if not FileExists(s) then begin
		MessageBox(Application.Handle,PAnsiChar(Format(Lang[ID_ERR_FILENOTFOUND],[s])),PChar(Lang[ID_ERROR]), MB_ICONHAND);
		Exit;
	end;

	e:= TEditor.Create;
	e.Init(FALSE, ExtractFileName(s), s, TRUE);
	if assigned(fProject) then begin
		if NotSameText(fProject.FileName,s) and (fProject.GetUnitFromString(s) = -1) then
			dmMain.RemoveFromHistory(s);
	end else begin
		dmMain.RemoveFromHistory(s);
	end;
	e.Activate;
	if not assigned(fProject) then
		CppParser.ReParseFile(e.FileName, e.InProject, True);
end;

procedure TMainForm.AddFindOutputItem(const line, col, filename, msg, keyword: AnsiString);
var
	ListItem : TListItem;
begin
	ListItem := FindOutput.Items.Add;
	ListItem.Caption := line;
	ListItem.Data := Pointer(Length(keyword));
	ListItem.SubItems.Add(col);
	ListItem.SubItems.Add(filename);
	ListItem.SubItems.Add(msg);
end;

function TMainForm.ParseParams(s : AnsiString) : AnsiString;
resourcestring
 cEXEName = '<EXENAME>';
 cPrjName = '<PROJECTNAME>';
 cPrjFile = '<PROJECTFILE>';
 cPrjPath = '<PROJECTPATH>';
 cCurSrc  = '<SOURCENAME>';
 cSrcPath = '<SOURCEPATH>';
 cDevVer  = '<DEVCPPVERSION>';

 cDefault = '<DEFAULT>';
 cExecDir = '<EXECPATH>';
 cSrcList = '<SOURCESPCLIST>';
 cWordxy  = '<WORDXY>';

var
	e: TEditor;
begin
	e:= GetEditor;

	// <DEFAULT>
	s:= StringReplace(s, cDefault, devDirs.Default, [rfReplaceAll]);

	// <EXECPATH>
	s:= Stringreplace(s, cExecDir, devDirs.Exec, [rfReplaceAll]);

	// <DEVCPPVERISON>
	s:= StringReplace(s, cDevVer, DEVCPP_VERSION, [rfReplaceAll]);

	if assigned(fProject) then begin
		// <EXENAME>
		s:= StringReplace(s, cEXEName, '"' + fProject.Executable + '"', [rfReplaceAll]);

		// <PROJECTNAME>
		s:= StringReplace(s, cPrjName, fProject.Name, [rfReplaceAll]);

		// <PROJECTFILE>
		s:= StringReplace(s, cPrjFile, fProject.FileName, [rfReplaceAll]);

		// <PROJECTPATH>
		s:= StringReplace(s, cPrjPath, fProject.Directory, [rfReplaceAll]);

		if Assigned(e) then begin
			// <SOURCENAME>
			s:= StringReplace(s, cCurSrc, e.FileName, [rfReplaceAll]);

			// <SOURCEPATH>
			if e.FileName = '' then
				s:= StringReplace(s, cSrcPath, devDirs.Default, [rfReplaceAll])
			else
				s:= StringReplace(s, cSrcPath, ExtractFilePath(e.FileName), [rfReplaceAll]);
		end;

		// <SOURCESPCLIST>
		s:= StringReplace(s, cSrcList, fProject.ListUnitStr(' '), [rfReplaceAll]);
	end else if assigned(e) then begin
		// <EXENAME>
		s:= StringReplace(s, cEXEName, '"' + ChangeFileExt(e.FileName, EXE_EXT) + '"', [rfReplaceAll]);

		// <PROJECTNAME>
		s:= StringReplace(s, cPrjName, e.FileName, [rfReplaceAll]);

		// <PRJECTFILE>
		s:= StringReplace(s, cPrjFile, e.FileName, [rfReplaceAll]);

		// <PROJECTPATH>
		s:= StringReplace(s, cPrjPath, ExtractFilePath(e.FileName), [rfReplaceAll]);

		// <SOURCENAME>
		s:= StringReplace(s, cCurSrc, e.FileName, [rfReplaceAll]);

		// <SOURCEPATH>
		// if fActiveEditor is "untitled"/new file return users default directory
		if e.FileName = '' then
			s:= StringReplace(s, cSrcPath, devDirs.Default, [rfReplaceAll])
		else
			s:= StringReplace(s, cSrcPath, ExtractFilePath(e.FileName), [rfReplaceAll]);

			// <WORDXY>
			s:= StringReplace(s, cWordXY, e.Text.WordAtCursor, [rfReplaceAll]);
		end;

	// clear unchanged macros

	if not assigned(fProject) then
		s:= StringReplace(s, cSrcList, '', [rfReplaceAll]);

	if not assigned(e) then begin
		s:= StringReplace(s, cCurSrc, '', [rfReplaceAll]);
		s:= StringReplace(s, cWordXY, '', [rfReplaceAll]);
		// if no editor assigned return users default directory
		s:= StringReplace(s, cSrcPath, devDirs.Default, [rfReplaceAll]);
	end;

	if not assigned(fProject) and not assigned(e) then begin
		s:= StringReplace(s, cEXEName, '', [rfReplaceAll]);
		s:= StringReplace(s, cPrjName, '', [rfReplaceAll]);
		s:= StringReplace(s, cPrjFile, '', [rfReplaceAll]);
		s:= StringReplace(s, cPrjPath, '', [rfReplaceAll]);
	end;

	Result := s;
end;

procedure TMainForm.CompOutputProc(const _Line, _Col, _Unit, _Message: AnsiString);
begin
	with CompilerOutput.Items.Add do begin
		Caption:= _Line;
		SubItems.Add(_Col);
		SubItems.Add(GetRealPath(_Unit));
		SubItems.Add(_Message);
	end;
end;

procedure TMainForm.CompResOutputProc(const _Line, _Unit, _Message: AnsiString);
begin
	if (_Line <> '') and (_Unit <> '') then
		ResourceOutput.Items.Add('Line ' + _Line + ' in file ' + _Unit + ' : ' + _Message)
	else
		ResourceOutput.Items.Add(_Message);
end;

procedure TMainForm.CompSuccessProc;
var
	F: TSearchRec;
	HasSize : boolean;
	I: integer;
begin
	// Close it if there's nothing to show
	if (CompilerOutput.Items.Count = 0) and actCompOnNeed.Checked then
		OpenCloseMessageSheet(FALSE)

	// Or open it if there is anything to show
	else begin
		if MessageControl.ActivePage <> CompSheet then
			MessageControl.ActivePage := CompSheet;
		OpenCloseMessageSheet(TRUE);
	end;

	if fCompiler.ErrorCount = 0 then begin
		TotalErrors.Text := '0';
		HasSize := False;
		if Assigned(fProject) then begin
			FindFirst(fProject.Executable, faAnyFile, F);
			HasSize := FileExists(fProject.Executable);
		end else if PageControl.PageCount > 0 then begin
			FindFirst(ChangeFileExt(GetEditor.FileName, EXE_EXT), faAnyFile, F);
			HasSize := FileExists(ChangeFileExt(GetEditor.FileName, EXE_EXT));
		end;
		if HasSize then begin
			SizeFile.Text := IntToStr(F.Size) + ' ' + Lang[ID_BYTES];
			if F.Size > 1024*1024 then begin
				SizeFile.Text := SizeFile.Text + ' (' + IntToStr((F.Size div 1024) div 1024) + ' MiB)';
			end else if F.Size > 1024 then
				SizeFile.Text := SizeFile.Text + ' (' + IntToStr(F.Size div 1024) + ' KiB)';
		end else
			SizeFile.Text := '0';
	end else begin
		for I := 0 to CompilerOutput.Items.Count-1 do begin

			// Caption equals the 'Line' column
			if not SameStr(CompilerOutput.Items[I].Caption,'') then begin

				// This item has a line number, proceed to set cursor properly
				CompilerOutput.Selected:=CompilerOutput.Items[I];
				CompilerOutputDblClick(nil);
				Break;
			end;
		end;
	end;
end;

procedure TMainForm.LogEntryProc(const msg: AnsiString);
begin
	LogOutput.Lines.Add(msg);
end;

procedure TMainForm.ProjectViewContextPopup(Sender: TObject;MousePos: TPoint; var Handled: Boolean);
var
	pt: TPoint;
	Node: TTreeNode;
begin
	if not assigned(fProject) or devData.FullScreen then exit;
	pt:= ProjectView.ClientToScreen(MousePos);
	Node:= ProjectView.GetNodeAt(MousePos.X, MousePos.Y);
	if not assigned(Node) then exit;
	ProjectView.Selected:= Node;
	if Node.Level = 0 then
		ProjectPopup.Popup(pt.x, pt.Y)
	else begin
		BuildOpenWith;
		RemoveFilefromprojectPopItem.Visible:=Node.Data<>Pointer(-1);
		RenamefilePopItem.Visible:=Node.Data<>Pointer(-1);
		OpenPopItem.Visible:=Node.Data<>Pointer(-1);
		ClosefilePopItem.Visible:=Node.Data<>Pointer(-1);
		Removefolder1.Visible:=Node.Data=Pointer(-1);
		Renamefolder1.Visible:=Node.Data=Pointer(-1);
		Addfile1.Visible:=Node.Data=Pointer(-1);
		mnuUnitProperties.Visible:=Node.Data<>Pointer(-1);
		UnitPopup.Popup(pt.X, pt.Y);
	end;
	Handled:= TRUE;
end;

procedure TMainForm.OpenUnit;
var
	Node: TTreeNode;
	i, idx: integer;
	pt: TPoint;
	e: TEditor;
begin
	if assigned(ProjectView.Selected) then
		Node:= ProjectView.Selected
	else begin
		pt:= ProjectView.ScreenToClient(Mouse.CursorPos);
		Node:= ProjectView.GetNodeAt(pt.x, pt.y);
	end;
	if assigned(Node) { begin XXXKF } and (integer(Node.Data)<>-1) { end XXXKF } then
		if (Node.Level >= 1) then begin
			i:= integer(Node.Data);
			idx:= FileIsOpen(fProject.Units[i].FileName, TRUE);
			if idx> -1 then
				e:= GetEditor(idx)
			else
				e:= fProject.OpenUnit(i);
			if assigned(e) then
				e.Activate;
		end;
end;

procedure TMainForm.ProjectViewClick(Sender: TObject);
var
	e: TEditor;
begin
	if devData.DblFiles then exit;
	if not Assigned(ProjectView.Selected) then Exit;
	if ProjectView.Selected.Data<>Pointer(-1) then
		OpenUnit
	else begin
		e:=GetEditor;
		if Assigned(e) then
			e.Activate;
	end;
end;

procedure TMainForm.ProjectViewDblClick(Sender: TObject);
begin
	if devData.dblFiles then
		OpenUnit;
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

	NewEditor:= TEditor.Create;
	NewEditor.Init(FALSE, Lang[ID_UNTITLED] + inttostr(dmMain.GetNewFileNumber), '', FALSE);
	NewEditor.InsertDefaultText;
	NewEditor.Activate;
end;

procedure TMainForm.actNewProjectExecute(Sender: TObject);
var
	s: AnsiString;
begin
	with TNewProjectForm.Create(Self) do
	try
		rbCpp.Checked := devData.DefCpp;
		rbC.Checked := not rbCpp.Checked;
		if ShowModal = mrOk then begin
			if (cbDefault.Checked) then
				devData.DefCpp := rbCpp.Checked;
			if assigned(fProject) then begin
				if fProject.Name = '' then
					s:= fProject.FileName
				else
					s:= fProject.Name;
					if MessageDlg(format(Lang[ID_MSG_CLOSECREATEPROJECT], [s]), mtConfirmation,[mbYes, mbNo], 0) = mrYes then
						actCloseProject.Execute
					else
						Exit;
			end;
			s := edProjectName.Text + DEV_EXT;
			with dmMain.SaveDialog do begin
				Filter:= FLT_PROJECTS;
				InitialDir:= devDirs.Default;
				FileName:= s;
				if not Execute then
					Exit;
				s:= FileName;
				if FileExists(s) then begin
					if MessageDlg(Lang[ID_MSG_FILEEXISTS],mtWarning, [mbYes, mbNo], 0) = mrYes then
						DeleteFile(s)
					else
						Exit;
				end;
			end;

			fProject:= TProject.Create(s, edProjectName.Text);
			if not fProject.AssignTemplate(s, GetTemplate) then begin
				fProject.Free;
				MessageBox(Application.Handle, PAnsiChar(Lang[ID_ERR_TEMPLATE]), PAnsiChar(Lang[ID_ERROR]), MB_OK or MB_ICONERROR);
				Exit;
			end;
			fCompiler.Project:= fProject;
			fProject.SaveProjectFile;

			if not devData.ProjectView then
				actProjectManager.Execute;
		end;
	finally
		Free;
	end;
end;

procedure TMainForm.actNewResExecute(Sender: TObject);
var
	NewEditor: TEditor;
	InProject: Boolean;
	fname: AnsiString;
	res: TTreeNode;
	NewUnit : TProjUnit;
begin
	if Assigned(fProject) then
		InProject := Application.MessageBox(PAnsiChar(Lang[ID_MSG_NEWRES]), 'New Resource', MB_ICONQUESTION + MB_YESNO) = mrYes
	else
		InProject := False;

	fname:=Lang[ID_UNTITLED] +inttostr(dmMain.GetNewFileNumber) + '.rc';
	NewEditor := TEditor.Create;
	NewEditor.Init(InProject, fname, '',FALSE, TRUE);
	NewEditor.Activate;

	if InProject and Assigned(fProject) then begin
		res:=fProject.FolderNodeFromName('Resources');
		NewUnit := fProject.AddUnit(fname, res, True);
		NewUnit.Editor := NewEditor;
	end;
	// new editor with resource file
end;

procedure TMainForm.actNewTemplateExecute(Sender: TObject);
begin
	// change to save cur project as template maybe?
	with TNewTemplateForm.Create(Self) do begin
		TempProject:=fProject;
		ShowModal;
	end;
end;

procedure TMainForm.actOpenExecute(Sender: TObject);
var
	idx,prj: integer;
	flt: AnsiString;
begin
	prj:= -1;
	if not BuildFilter(flt, ftOpen) then
		if not BuildFilter(flt, [FLT_PROJECTS, FLT_HEADS, FLT_CS, FLT_CPPS, FLT_RES]) then
			flt:= FLT_ALLFILES;

	with dmMain.OpenDialog do begin
		Filter:= flt;
		Title:= Lang[ID_NV_OPENFILE];
		if Execute and (Files.Count > 0) then begin
			for idx:= 0 to pred(Files.Count) do
				if CompareText(ExtractFileExt(Files[idx]), DEV_EXT) = 0 then begin
					prj:= idx;
					break;
				end;
			if prj = -1 then // not found
				for idx:= 0 to pred(Files.Count) do
					OpenFile(Files[idx]) // open all files
			else
				OpenProject(Files[prj]); // else open found project
		end;
	end;
end;

procedure TMainForm.actHistoryClearExecute(Sender: TObject);
begin
	dmMain.ClearHistory;
end;

procedure TMainForm.actSaveExecute(Sender: TObject);
begin
	SaveFile(GetEditor);
end;

procedure TMainForm.actSaveAsExecute(Sender: TObject);
begin
	SaveFileAs(GetEditor);
end;

procedure TMainForm.actSaveAllExecute(Sender: TObject);
var
	idx: integer;
	wa: boolean;
	e : TEditor;
begin
	wa:=devFileMonitor.Active;
	devFileMonitor.Deactivate;
	if assigned(fProject) then begin
		fProject.Save;
		UpdateAppTitle;
		if CppParser.Statements.Count=0 then // only scan entire project if it has not already been scanned...
			ScanActiveProject;
	end;

	for idx:= 0 to pred(PageControl.PageCount) do begin
		e := GetEditor(idx);
		if e.Text.Modified and ((not e.InProject) or e.IsRes) then
			if not SaveFile(GetEditor(idx)) then
				Break;
	end;

	if wa then
		devFileMonitor.Activate;
end;

procedure TMainForm.actCloseExecute(Sender: TObject);
begin
	CloseEditor(PageControl.ActivePageIndex);
end;

procedure TMainForm.actCloseAllExecute(Sender: TObject);
var
	idx: integer;
begin
	for idx := PageControl.PageCount - 1 downto 0 do
		if not CloseEditor(0) then
			Break;
end;

procedure TMainForm.actCloseProjectExecute(Sender: TObject);
var
	s: AnsiString;
	wa: boolean;
begin
	actStopExecute.Execute;
	wa:=devFileMonitor.Active;
	devFileMonitor.Deactivate;

	// save project layout anyway ;)
	fProject.CmdLineArgs:=fCompiler.RunParams;
	fProject.SaveLayout;

	// TODO: should we save watches?
	if fProject.Modified then begin
		if fProject.Name = '' then
			s:= fProject.FileName
		else
			s:= fProject.Name;

		case MessageDlg(format(Lang[ID_MSG_SAVEPROJECT], [s]), mtConfirmation, mbYesNoCancel, 0) of
			mrYes: fProject.Save;
			mrNo: fProject.Modified := FALSE;
			mrCancel: exit;
		end;
	end;

	fCompiler.Project:= nil;
	dmMain.AddtoHistory(fProject.FileName);

	FreeandNil(fProject);
	ProjectView.Items.Clear;
	ClearMessageControl;
	UpdateAppTitle;
	ClassBrowser.ProjectDir:='';
	CppParser.Reset;

	SetStatusbarLineCol;

	if wa then
		devFileMonitor.Activate;
end;

procedure TMainForm.actXHTMLExecute(Sender: TObject);
var
	e: TEditor;
begin
	e:= GetEditor;
	if assigned(e) then
		e.Exportto(0);
end;

procedure TMainForm.actXRTFExecute(Sender: TObject);
var
	e: TEditor;
begin
	e:= GetEditor;
	if assigned(e) then
		e.Exportto(1);
end;

procedure TMainForm.actXTexExecute(Sender: TObject);
var
	e: TEditor;
begin
	e:= GetEditor;
	if assigned(e) then
		e.Exportto(2);
end;

procedure TMainForm.actXProjectExecute(Sender: TObject);
begin
	if assigned(fProject) then
		fProject.Exportto(true);
end;

procedure TMainForm.actPrintExecute(Sender: TObject);
var
	e: TEditor;
begin
	e:= GetEditor;
	if assigned(e) then
		with TPrintForm.Create(Self) do begin
			if ShowModal = mrOk then begin
				with dmMain.SynEditPrint do begin
					SynEdit:= e.Text;
					Highlighter:= e.Text.Highlighter;
					Colors:= cbColors.Checked;
					Highlight:= cbHighlight.Checked;
					Wrap:= cbWordWrap.Checked;
					LineNumbers:= not rbNoLN.checked;
					LineNumbersInMargin:= rbLNMargin.Checked;
					Copies:= seCopies.Value;
					SelectedOnly:= cbSelection.Checked;
					Print;
				end;
				devData.PrintColors := cbColors.Checked;
				devData.PrintHighlight := cbHighlight.Checked;
				devData.PrintWordWrap := cbWordWrap.Checked;
				devData.PrintLineNumbers := rbLN.Checked;
				devData.PrintLineNumbersMargins := rbLNMargin.Checked;
			end;
		end;
end;

procedure TMainForm.actPrintSUExecute(Sender: TObject);
begin
	try
		dmMain.PrinterSetupDialog.Execute;
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
	e:= GetEditor;
	if assigned(e) then
		e.Text.Undo;
end;

procedure TMainForm.actRedoExecute(Sender: TObject);
var
	e: TEditor;
begin
	e:= GetEditor;
	if assigned(e) then
		e.Text.Redo;
end;

procedure TMainForm.actCutExecute(Sender: TObject);
var
	e: TEditor;
	oldbottomline : integer;
begin
	e:= GetEditor;
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
	e:=GetEditor;
	if Assigned(e) then
		e.Text.CopyToClipboard;
end;

procedure TMainForm.actPasteExecute(Sender: TObject);
var
	e: TEditor;
	oldtopline : integer;
begin
	e:= GetEditor;
	if Assigned(e) then begin
		oldtopline := e.Text.TopLine;
		e.Text.PasteFromClipboard;
		if e.Text.TopLine <> oldtopline then
			e.Text.Repaint; // fix for repaint fail
	end;
end;

procedure TMainForm.actSelectAllExecute(Sender: TObject);
var
	e: TEditor;
begin
	e:= GetEditor;
	if assigned(e) then
		e.Text.SelectAll;
end;

procedure TMainForm.actDeleteExecute(Sender: TObject);
var
	e: TEditor;
begin
	e:= GetEditor;
	if Assigned(e) then
		e.Text.ClearSelection;
end;

procedure TMainForm.actProjectManagerExecute(Sender: TObject);
begin
	// Hide/show this first, or otherwhise it'll show up to the left of ProjectToolWindow
	SplitterLeft.Visible:= actProjectManager.Checked;
	//if (MessageControl <> self) and assigned(ProjectToolWindow) then
	//	ProjectToolWindow.Close;
	LeftPageControl.Visible:= actProjectManager.Checked;
	devData.ProjectView:= actProjectManager.Checked;
end;

procedure TMainForm.actStatusbarExecute(Sender: TObject);
begin
	devData.Statusbar:= actStatusbar.Checked;
	Statusbar.Visible:= actStatusbar.Checked;
	Statusbar.Top:= Self.ClientHeight;
end;

procedure TMainForm.actCompOutputExecute(Sender: TObject);
begin
	if AlwaysShowItem.Checked then
		OpenCloseMessageSheet(TRUE)
	else if not actCompOnNeed.Checked then
		OpenCloseMessageSheet(FALSE);

	if actCompOutput.Checked then
		actCompOnNeed.Checked := False
	else if (not actCompOutput.Checked) and (not actCompOnNeed.Checked) then
		OpenCloseMessageSheet(False);
	devData.ShowOutput:=actCompOutput.Checked;
	devData.OutputOnNeed:=actCompOnNeed.Checked;
end;

procedure TMainForm.actCompOnNeedExecute(Sender: TObject);
begin
	if actCompOnNeed.Checked or ((not actCompOutput.Checked) and (not actCompOnNeed.Checked)) then
		OpenCloseMessageSheet(False);
	devData.ShowOutput:=actCompOutput.Checked;
	devData.OutputOnNeed:=actCompOnNeed.Checked;
end;

procedure TMainForm.btnFullScrRevertClick(Sender: TObject);
begin
	actFullScreen.Execute;
end;

procedure TMainForm.actFullScreenExecute(Sender: TObject);
begin
	devData.FullScreen:= FullScreenModeItem.Checked;
	if devData.FullScreen then begin
		OldLeft := Left;
		OldTop := Top;
		OldWidth := Width;
		OldHeight := Height;
		GetWindowPlacement(Self.Handle, @WindowPlacement);
		BorderStyle:= bsNone;
		FullScreenModeItem.Caption:= Lang[ID_ITEM_FULLSCRBACK];
		Toolbar.Visible:= devData.ShowBars;
		pnlFull.Visible:= TRUE;

		// set size to hide form menu
		//works with multi monitors now.
		SetBounds(
			(Left +Monitor.WorkAreaRect.Left) - ClientOrigin.X,
			(Top+ Monitor.WorkAreaRect.Top) - ClientOrigin.Y,
			Monitor.Width + (Width - ClientWidth),
			Monitor.Height+ (Height - ClientHeight));
	end else begin
		Left := OldLeft;
		Top := OldTop;
		Width := OldWidth;
		Height := OldHeight;

		SetWindowPlacement(Self.Handle, @WindowPlacement);
		BorderStyle:= bsSizeable;
		FullScreenModeItem.Caption:= Lang[ID_ITEM_FULLSCRMODE];
		Toolbar.Visible:= TRUE;

		pnlFull.Visible:= FALSE;
	end;
end;

procedure TMainForm.actNextExecute(Sender: TObject);
begin
	PageControl.SelectNextPage(TRUE);
end;

procedure TMainForm.actPrevExecute(Sender: TObject);
begin
	PageControl.SelectNextPage(FALSE);
end;

procedure TMainForm.actCompOptionsExecute(Sender: TObject);
begin
	with TCompOptForm.Create(Self) do
		try
			ShowModal;
			CheckForDLLProfiling;
		finally
			Close;
		end;
end;

procedure TMainForm.actEditorOptionsExecute(Sender: TObject);
var
	I : integer;
	e : TEditor;
begin
	with TEditorOptForm.Create(Self) do
		try
			if ShowModal = mrOk then begin
				dmMain.UpdateHighlighter;
				for I := 0 to PageControl.PageCount - 1 do begin
					e := TEditor(PageControl.Pages[I].Tag);
					devEditor.AssignEditor(e.Text,e.FileName);
					e.InitCompletion;
				end;

				// Cache options have changed...
				if chkCCCache.Tag = 1 then begin
					CppParser.Reset(true);
				end;

				InitClassBrowser(chkCCCache.Tag = 1);

				// Cache options have changed, do a rescan
				if chkCCCache.Tag = 1 then begin
					ClassBrowser.Clear;
					ScanActiveProject;
				end;

				// if class-browsing is disabled, clear the class-browser
				// if there is no active editor, clear the class-browser
				e := GetEditor;
				if not ClassBrowser.Enabled or (not Assigned(e) and (ClassBrowser.ShowFilter = sfCurrent)) then
					ClassBrowser.Clear;
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
	if not assigned(fProject) then exit;
{$IFDEF WIN32}
	while ProjectView.SelectionCount>0 do begin
		node:=ProjectView.Selections[0];
{$ENDIF}
{$IFDEF LINUX}
	while ProjectView.SelCount>0 do begin
		node:=ProjectView.Selected[0];
{$ENDIF}
		if not assigned(node) or (node.Level < 1) then
			Continue;
		if node.Data=Pointer(-1) then
			Continue;

		idx:= integer(node.Data);

		if not fProject.Remove(idx, true) then
			exit;
	end;
end;

procedure TMainForm.actUnitRenameExecute(Sender: TObject);
var
 idx: integer;
 oldName,
 NewName: AnsiString;
begin
	if not assigned(fProject) then exit;
	if not assigned(ProjectView.Selected) or
		 (ProjectView.Selected.Level < 1) then exit;

	if ProjectView.Selected.Data=Pointer(-1) then
		Exit;

	idx:= integer(ProjectView.Selected.Data);
	OldName:= fProject.Units[idx].FileName;
	NewName:= ExtractFileName(OldName);

	if InputQuery(Lang[ID_RENAME], Lang[ID_MSG_FILERENAME], NewName) and
		(ExtractFileName(NewName) <> '') and
		(NewName <> OldName) then
	 try
		 chdir(ExtractFilePath(OldName));
		 // change in project first so on failure
		 // file isn't already renamed
		 fProject.SaveUnitAs(idx, ExpandFileto(NewName, GetCurrentDir));
		 devFileMonitor.Deactivate; // deactivate for renaming or a message will raise
		 Renamefile(OldName, NewName);
		 devFileMonitor.Activate;
	 except
		 MessageDlg(format(Lang[ID_ERR_RENAMEFILE], [OldName]), mtError, [mbok], 0);
	 end;
end;

procedure TMainForm.actUnitOpenExecute(Sender: TObject);
var
	 idx, idx2: integer;
begin
	if not assigned(fProject) then
		Exit;
	if not assigned(ProjectView.Selected) or (ProjectView.Selected.Level < 1) then
		Exit;
	if ProjectView.Selected.Data=Pointer(-1) then
		Exit;

	idx := integer(ProjectView.Selected.Data);
	idx2 := FileIsOpen(fProject.Units[idx].FileName, TRUE);
	if idx2 > -1 then
		GetEditor(idx2).Activate
	else
		fProject.OpenUnit(idx);
end;

procedure TMainForm.actUnitCloseExecute(Sender: TObject);
var
 idx: integer;
begin
	if assigned(fProject) and assigned(ProjectView.Selected) then begin
		idx:= FileIsOpen(fProject.Units[integer(ProjectView.Selected.Data)].FileName, TRUE);
		if idx> -1 then
			CloseEditor(idx);
	end;
end;

procedure TMainForm.actUpdateCheckExecute(Sender: TObject);
begin
	with TWebUpdateForm.Create(self) do begin
		ShowModal;
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
begin
	idx:= -1;
	if assigned(fProject) then
		idx:= fProject.NewUnit(FALSE);
	if idx > -1 then
		with fProject.OpenUnit(idx) do begin
			Activate;
			Text.Modified:=True;
		end;
end;

{ begin XXXKF changed }

procedure TMainForm.actProjectAddExecute(Sender: TObject);
var
 flt: AnsiString;
 idx: integer;
 FolderNode : TTreeNode;
begin
	if not assigned(fProject) then exit;

	if not BuildFilter(flt, [FLT_CS, FLT_CPPS, FLT_RES, FLT_HEADS]) then BuildFilter(flt, ftAll);

	with dmMain.OpenDialog do begin
		Title:= Lang[ID_NV_OPENADD];
		Filter:= flt;
		if Execute then begin
			if Assigned(ProjectView.Selected) and (ProjectView.Selected.Data=Pointer(-1)) then
				FolderNode := ProjectView.Selected
			else
				FolderNode := fProject.Node;

			for idx:= 0 to pred(Files.Count) do begin
				fProject.AddUnit(Files[idx], FolderNode, false); // add under folder
				CppParser.AddFileToScan(Files[idx]);
			end;

			fProject.RebuildNodes;
			CppParser.ParseList;
		end;
	end;
end;

{ end XXXKF changed }

procedure TMainForm.actProjectRemoveExecute(Sender: TObject);
var
	I : integer;
begin
	with TRemoveUnitForm.Create(MainForm) do begin

		// Add list of project files
		for i := 0 to fProject.Units.Count - 1 do
			UnitList.Items.Append(fProject.Units[i].FileName);

		ShowModal;
	end;
end;

procedure TMainForm.actProjectOptionsExecute(Sender: TObject);
begin
	if assigned(fProject) then begin
		fProject.ShowOptions;
		UpdateAppTitle;
	end;
end;

procedure TMainForm.actProjectSourceExecute(Sender: TObject);
begin
	if assigned(fProject) then
		OpenFile(fProject.FileName);
end;

procedure TMainForm.actFindExecute(Sender: TObject);
var
	e : TEditor;
	s : AnsiString;
begin
	e := GetEditor;
	if Assigned(e) then begin

		// Create it when needed!
		if not Assigned(frmFind) then
			frmFind := TfrmFind.Create(Application);

		s := e.Text.SelText;
		if s = '' then
			s := e.Text.WordAtCursor;

		frmFind.TabIndex := 0;
		frmFind.cboFindText.Text := s;
		frmFind.Show;
	end;
end;

procedure TMainForm.actFindAllExecute(Sender: TObject);
var
	e : TEditor;
	s : AnsiString;
begin
	e := GetEditor;
	if Assigned(e) then begin

		// Create it when needed!
		if not Assigned(frmFind) then
			frmFind := TfrmFind.Create(Application);

		s := e.Text.SelText;
		if s = '' then
			s := e.Text.WordAtCursor;

		frmFind.TabIndex := 1;
		frmFind.cboFindText.Text := s;
		frmFind.Show;
	end;
end;

procedure TMainForm.actReplaceExecute(Sender: TObject);
var
	e : TEditor;
	s : AnsiString;
begin
	e := GetEditor;
	if Assigned(e) then begin

		// Create it when needed!
		if not Assigned(frmFind) then
			frmFind := TfrmFind.Create(Application);

		s := e.Text.SelText;
		if s = '' then
			s := e.Text.WordAtCursor;

		frmFind.TabIndex := 2;
		frmFind.cboFindText.Text := s;
		frmFind.Show;
	end;
end;

procedure TMainForm.actReplaceAllExecute(Sender: TObject);
var
	e : TEditor;
	s : AnsiString;
begin
	e := GetEditor;
	if Assigned(e) then begin

		// Create it when needed!
		if not Assigned(frmFind) then
			frmFind := TfrmFind.Create(Application);

		s := e.Text.SelText;
		if s = '' then
			s := e.Text.WordAtCursor;

		frmFind.TabIndex := 3;
		frmFind.cboFindText.Text := s;
		frmFind.Show;
	end;
end;

procedure TMainForm.actGotoLineExecute(Sender: TObject);
var
	e: TEditor;
begin
	e:= GetEditor;
	if Assigned(e) then
		e.GotoLine;
end;

function TMainForm.PrepareForCompile: Boolean;
var
 e: TEditor;
 i: Integer;
begin
	Result := False;
	if Assigned(fProject) and (fProject.Units.Count = 0) then begin
		MessageDlg('Why in the world are you trying to compile an empty project? ;-)', mtWarning,[mbOK], 0);
		Exit;
	end;

	ClearMessageControl;
	SizeFile.Text:= '';
	TotalErrors.Text:= '0';

	if not devData.ShowProgress then begin
		// if no compile progress window, open the compiler output
		OpenCloseMessageSheet(True);
		MessageControl.ActivePage:= LogSheet;
	end;

	e:= GetEditor;
	fCompiler.Target:= ctNone;

	if Assigned(fProject) then
		// no matter if the editor file is not in project,
		// the target is ctProject since we have a project open...
		fCompiler.Target:= ctProject
	else if Assigned(e) and (GetFileTyp(e.Filename) in [utcSrc, utcppSrc, utResSrc]) or e.new then
		fCompiler.Target:= ctFile;

	if fCompiler.Target in [ctFile,ctNone] then begin
		if not SaveFile(e) then
			Exit;
		fCompiler.SourceFile:= e.FileName;
	end else if fCompiler.Target = ctProject then begin
		actSaveAllExecute(Self);
		for i := 0 to pred(PageControl.PageCount) do begin
			e := GetEditor(i);
			if (e.InProject) and (e.Text.Modified) then
				Exit;
		end;
	end;

	fCompiler.PerfectDepCheck := not devCompiler.FastDep;

	if Assigned(fProject) then begin
		if fProject.Options.VersionInfo.AutoIncBuildNr then
			fProject.IncrementBuildNumber;
		fProject.BuildPrivateResource;
	end;

	Result := True;
end;

procedure TMainForm.actCompileExecute(Sender: TObject);
begin
	if fCompiler.Compiling then begin
		MessageDlg(Lang[ID_MSG_ALREADYCOMP], mtInformation, [mbOK], 0);
		Exit;
	end;
	if not PrepareForCompile then
		Exit;
	if fCompiler.Target = ctProject then
		DeleteFile(fProject.Executable);
	fCompiler.Compile;
end;

procedure TMainForm.actRunExecute(Sender: TObject);
var
	e: TEditor;
begin
	e:= GetEditor;
	fCompiler.Target:= ctNone;

	if assigned(fProject) then begin
		if assigned(e) and not e.InProject then
			fCompiler.Target:= ctFile
		else
			fCompiler.Target:= ctProject;
	end else if assigned(e) then
		fCompiler.Target:= ctFile;

	if fCompiler.Target = ctFile then
		fCompiler.SourceFile:= e.FileName;

	fCompiler.Run;
end;

procedure TMainForm.actCompRunExecute(Sender: TObject);
begin
	if fCompiler.Compiling then begin
		MessageDlg(Lang[ID_MSG_ALREADYCOMP], mtInformation, [mbOK], 0);
		Exit;
	end;
	if not PrepareForCompile then
		Exit;

	fCompiler.CompileAndRun;
end;

procedure TMainForm.actRebuildExecute(Sender: TObject);
begin
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
	ClearMessageControl;
	if fCompiler.Compiling then begin
		MessageDlg(Lang[ID_MSG_ALREADYCOMP], mtInformation, [mbOK], 0);
		Exit;
	end;
	fCompiler.Clean;
end;

procedure TMainForm.PrepareDebugger;
begin
	fDebugger.Stop;

	// Clear logs
	DebugOutput.Clear;
	EvalOutput.Clear;

	// Focus on the debugging buttons
	LeftPageControl.ActivePage := DebugLeftSheet;
	MessageControl.ActivePage := DebugSheet;
	OpenCloseMessageSheet(True);

	// Reset watch vars
	fDebugger.DeleteWatchVars(false);
end;

procedure TMainForm.actDebugExecute(Sender: TObject);
resourcestring
	LibAppend = '%s -I"%s"';
var
	e: TEditor;
	i: integer;
	dirs : TStringList;
	optD,optS : PCompilerOption;
	debug,strip : boolean;
	idxD,idxS : integer;
begin

	// Check if we enabled proper options
	debug := CheckCompileOption('-g3',optD,idxD);
	strip := CheckCompileOption('-s',optS,idxS);

	if not debug or strip then begin
		if MessageDlg(Lang[ID_MSG_NODEBUGSYMBOLS], mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin

			// ENABLE debugging
			if Assigned(fProject) then
				SetProjCompOpt(idxD, '1')
			else
				devCompiler.SetOption(optD,idxD,'1');

			// DISABLE stripping
			if Assigned(fProject) then
				SetProjCompOpt(idxS, '0')
			else
				devCompiler.SetOption(optS,idxS,'0');

			if not Assigned(fProject) then
				devCompiler.SaveSet(devCompiler.CurrentIndex);

			actRebuildExecute(nil);
			Exit;
		end;
	end;

	if Assigned(fProject) then begin

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

		fDebugger.Start;
		fDebugger.SendCommand('file','"' + StringReplace(fProject.Executable, '\', '/', [rfReplaceAll]) + '"');

		if fProject.Options.typ = dptDyn then
			fDebugger.SendCommand('exec-file', '"' + StringReplace(fProject.Options.HostApplication, '\', '/', [rfReplaceAll]) +'"');
	end else begin
		e := GetEditor;
		if assigned(e) then begin

			// Did we compile?
			if not FileExists(ChangeFileExt(e.FileName, EXE_EXT)) then begin
				if MessageDlg(Lang[ID_ERR_SRCNOTCOMPILEDSUGGEST], mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
					actCompileExecute(nil);
				end;
				Exit;
			end;

			// Did we save?
			if e.Text.Modified then // if file is modified
				if not SaveFile(e) then // save it first
					Exit;

			PrepareDebugger;

			fDebugger.Start;
			fDebugger.SendCommand('file', '"' + StringReplace(ChangeFileExt(e.FileName,EXE_EXT), '\', '/', [rfReplaceAll]) + '"');
		end;
	end;

	dirs := TStringList.Create;

	// hack fix due to:
	// http://stackoverflow.com/questions/1335027/delphi-stringlist-delimiter-is-always-a-space-character-even-if-delimiter-is-se
	ExtractStrings([';'],[],PChar(devCompiler.LibDir),dirs);
	for I := 0 to dirs.Count - 1 do
		fDebugger.SendCommand('dir','"' + StringReplace(dirs[i],'\','/',[rfReplaceAll]) + '"');

	// Add include folders
	ExtractStrings([';'],[],PChar(devCompiler.CDir),dirs);
	for I := 0 to dirs.Count - 1 do
		fDebugger.SendCommand('dir','"' + StringReplace(dirs[i],'\','/',[rfReplaceAll]) + '"');

	// Add more include folders, duplicates will be added/moved to front of list
	ExtractStrings([';'],[],PChar(devCompiler.CppDir),dirs);
	for I := 0 to dirs.Count - 1 do
		fDebugger.SendCommand('dir','"' + StringReplace(dirs[i],'\','/',[rfReplaceAll]) + '"');

	dirs.Free;

	// Add breakpoints and watch vars
	for i := 0 to fDebugger.WatchVarList.Count - 1 do
		fDebugger.AddWatchVar(i);

	for i := 0 to fDebugger.BreakPointList.Count - 1 do
		fDebugger.AddBreakpoint(i);

	// Run the debugger
	fDebugger.SendCommand('set','new-console on');
	fDebugger.SendCommand('set','confirm off');
	fDebugger.SendCommand('run',fCompiler.RunParams);
end;

procedure TMainForm.actEnviroOptionsExecute(Sender: TObject);
begin
	with TEnviroForm.Create(Self) do try
		if ShowModal = mrOk then begin
			if devData.MsgTabs = 0 then
				PageControl.TabPosition:= tpTop
			else if devData.MsgTabs = 1 then
				PageControl.TabPosition:= tpBottom
			else if devData.MsgTabs = 2 then
				PageControl.TabPosition:= tpLeft
			else if devData.MsgTabs = 3 then
				PageControl.TabPosition:= tpRight;
			SetupProjectView;

			PageControl.MultiLine:=devData.MultiLineTab;

			if devData.FullScreen then
				Toolbar.Visible:= devData.ShowBars;

			if devData.LangChange then begin
				Lang.SetLang(devData.Language);
				LoadText;
			end;
			if devData.ThemeChange then
				Loadtheme;
			devShortcuts.Filename:=devDirs.Config + DEV_SHORTCUTS_FILE;
		end;
	finally
		Close;
	end;
end;

procedure TMainForm.actUpdatePageCount(Sender: TObject);
begin
	TCustomAction(Sender).Enabled := PageControl.PageCount > 0;
end;

procedure TMainForm.actUpdateDeleteWatch(Sender: TObject);
begin
	TCustomAction(Sender).Enabled := Assigned(DebugTree.Selected);
end;

procedure TMainForm.actUpdatePageorProject(Sender: TObject);
begin
	TCustomAction(Sender).Enabled := assigned(fProject) or (PageControl.PageCount > 0);
end;

procedure TMainForm.actCompileUpdate(Sender: TObject);
begin
	TCustomAction(Sender).Enabled := (assigned(fProject) or (PageControl.PageCount > 0)) and not devExecutor.Running and not fDebugger.Executing and not fCompiler.Compiling and (devCompiler.Sets.Count > 0);
end;

procedure TMainForm.actUpdatePageProject(Sender: TObject);
begin
	TCustomAction(Sender).Enabled := assigned(fProject) and (PageControl.PageCount > 0);
end;

procedure TMainForm.actUpdateProject(Sender: TObject);
begin
	TCustomAction(Sender).Enabled := assigned(fProject);
end;

procedure TMainForm.actUpdateEmptyEditor(Sender: TObject);
var
	e: TEditor;
begin
	e := GetEditor;
	TCustomAction(Sender).Enabled := Assigned(e) and not IsEmpty(e.Text);
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
	e := GetEditor;
	TCustomAction(Sender).Enabled := Assigned(e) and not IsEmpty(e.Text) and (not Assigned(frmFind) or not frmFind.Showing);
end;

procedure TMainForm.ToolbarClick(Sender: TObject);
begin
	tbMain.Visible:= ToolMainItem.Checked;
	tbEdit.Visible:= ToolEditItem.Checked;
	tbCompile.Visible:= ToolCompileandRunItem.Checked;
	tbProject.Visible:= ToolProjectItem.Checked;
	tbSpecials.Visible:= ToolSpecialsItem.Checked;
	tbSearch.Visible:= ToolSearchItem.Checked;
	tbClasses.Visible:= ToolClassesItem.Checked;

	devData.ToolbarMain:=ToolMainItem.Checked;
	devData.ToolbarEdit:=ToolEditItem.Checked;
	devData.ToolbarCompile:=ToolCompileandRunItem.Checked;
	devData.ToolbarProject:=ToolProjectItem.Checked;
	devData.ToolbarSpecials:=ToolSpecialsItem.Checked;
	devData.ToolbarSearch:=ToolSearchItem.Checked;
	devData.ToolbarClasses:=ToolClassesItem.Checked;
end;

procedure TMainForm.ToolbarContextPopup(Sender: TObject;MousePos: TPoint; var Handled: Boolean);
var
	pt: TPoint;
begin
	pt:= Toolbar.ClientToScreen(MousePos);
	TrackPopupMenu(ToolbarsItem.Handle, TPM_LEFTALIGN or TPM_LEFTBUTTON,
		pt.x, pt.y, 0, Self.Handle, nil);
	Handled:= TRUE;
end;

procedure TMainForm.SplitterBottomMoved(Sender: TObject);
begin
	fPreviousHeight := MessageControl.Height;
end;

procedure TMainForm.actProjectMakeFileExecute(Sender: TObject);
begin
	fCompiler.Project:= fProject;
	fCompiler.BuildMakeFile;
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
			if Resourceoutput.ItemIndex <> -1 then
				Clipboard.AsText := ResourceOutput.Items[ResourceOutput.ItemIndex];
		2:
			LogOutput.CopyToClipboard;
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
	i:integer;
begin
	case MessageControl.ActivePageIndex of
		0: begin
			ClipBoard.AsText := '';
			for i:=0 to pred(CompilerOutput.Items.Count) do
				Clipboard.AsText := Clipboard.AsText + GetPrettyLine(CompilerOutput,i);
		end;
		1:
			for i:=0 to pred(ResourceOutput.Items.Count) do
				Clipboard.AsText:= Clipboard.AsText + ResourceOutput.Items[I] + #13#10;
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
			for i:=0 to pred(FindOutput.Items.Count) do
				Clipboard.AsText := Clipboard.AsText + GetPrettyLine(FindOutput,i);
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
			if TotalErrors.Focused then
				TotalErrors.SelectAll
			else if SizeFile.Focused then
				SizeFile.SelectAll
			else if LogOutput.Focused then
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
	i:integer;
	temp,temp2:AnsiString;
	Stream: TFileStream;
	savedialog : TSaveDialog;
	e : TEditor;
begin
	temp := '';
	savedialog := TSaveDialog.Create(self);
	try
		case MessageControl.ActivePageIndex of
			0: begin
				savedialog.FileName:= 'Formatted Compiler Output';
				for i := 0 to CompilerOutput.Items.Count - 1 do begin
					temp2 := CompilerOutput.Items[i].Caption + #10 + CompilerOutput.Items[i].SubItems.Text;
					temp2 := ReplaceFirstStr(temp2,#10,#9);
					temp2 := ReplaceFirstStr(temp2,#13#10,#9);
					temp2 := ReplaceFirstStr(temp2,#13#10,#9);
					temp := temp + temp2;
				end;
			end;
			1: begin
				savedialog.FileName:= 'Resource Error Log';
				for i := 0 to ResourceOutput.Items.Count - 1 do begin
					temp := temp + ResourceOutput.Items[I];
				end;
			end;
			2: begin
				savedialog.FileName:= 'Raw Build Log';
				if Length(LogOutput.Text) > 0 then
					temp := LogOutput.Text;
			end;
			3: begin
				savedialog.FileName:= 'Raw GDB Output';
				if Length(DebugOutput.Text) > 0 then
					temp := DebugOutput.Text;
			end;
			4: begin
				savedialog.FileName:= 'Find Results';
				for i:=0 to pred(FindOutput.Items.Count) do begin
					temp2 := FindOutput.Items[i].Caption + #10 + FindOutput.Items[i].SubItems.Text;
					temp2 := ReplaceFirstStr(temp2,#10,#9);
					temp2 := ReplaceFirstStr(temp2,#13#10,#9);
					temp2 := ReplaceFirstStr(temp2,#13#10,#9);
					temp := temp + temp2;
				end;
			end;
		end;

		if Length(temp) > 0 then begin
			savedialog.Title:= Lang[ID_NV_SAVEFILE];
			savedialog.Filter:= 'Text file|*.txt';
			savedialog.DefaultExt := 'txt';
			savedialog.FilterIndex:=1;
			saveDialog.Options := saveDialog.Options + [ofOverwritePrompt];

			if Assigned(fProject) then begin
				savedialog.InitialDir:=fProject.Directory;
			end else begin
				e:=GetEditor;
				if Assigned(e) then
					savedialog.InitialDir:=ExtractFilePath(e.FileName)
				else
					saveDialog.InitialDir:='C:\';
			end;

			if savedialog.Execute then begin
				Stream := TFileStream.Create(savedialog.FileName, fmCreate);
				try
					Stream.Write(temp[1], Length(temp));
				finally
					Stream.Free;
				end;
			end;
		end;
	finally
		savedialog.Free;
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
		4:
			FindOutput.Items.Clear;
	end;
end;

procedure TMainForm.actMsgHideExecute(Sender: TObject);
begin
	OpenCloseMessageSheet(MessageControl.Height <> fPreviousHeight);
end;

procedure TMainForm.actIncrementalExecute(Sender: TObject);
var
	pt: TPoint;
	e : TEditor;
begin
	e := GetEditor;
	if Assigned(e) then begin

		pt := ClienttoScreen(point(PageControl.Left, PageControl.Top));

		// Only create the form when we need to do so
		if not Assigned(frmIncremental) then
			frmIncremental := TfrmIncremental.Create(self);

		frmIncremental.Left:= pt.x;
		frmIncremental.Top:= pt.y;
		frmIncremental.editor:= e.Text;
		frmIncremental.Show;
	end;
end;

procedure TMainForm.CompilerOutputDblClick(Sender: TObject);
var
	Col, Line: integer;
	selection : TListItem;
	errorfiletab: TEditor;
begin
	selection := CompilerOutput.Selected;
	if Assigned(selection) then begin
		Line := StrToIntDef(selection.Caption,1);
		Col := StrToIntDef(selection.SubItems[0],1);
		errorfiletab := GetEditorFromFileName(selection.SubItems[1]);

		if Assigned(errorfiletab) then begin
			errorfiletab.Activate;
			errorfiletab.SetErrorFocus(Col, Line);
		end;
	end;
end;

procedure TMainForm.FindOutputDblClick(Sender: TObject);
var
	col, line: integer;
	e: TEditor;
	selected : TListItem;
begin
	selected := FindOutPut.Selected;
	if Assigned(selected) and not SameStr(selected.Caption,'') then begin
		Col:= StrToIntDef(selected.SubItems[0],1);
		Line:= StrToIntDef(selected.Caption,1);

		e := GetEditorFromFileName(selected.SubItems[1]);

		if Assigned(e) then begin
			e.Text.CaretXY:= BufferCoord(col, line);
			e.Text.SetSelWord;
			e.Text.CaretXY:= e.Text.BlockBegin;
			e.Activate;
		end;
	end;
end;

procedure TMainForm.actShowBarsExecute(Sender: TObject);
begin
	if devData.FullScreen then begin
		Toolbar.Visible:= not Toolbar.Visible;
		if Toolbar.Visible then
			pnlFull.Top:= -2;
	end;
end;

procedure TMainForm.FormContextPopup(Sender: TObject; MousePos: TPoint;var Handled: Boolean);
var
	pt: TPoint;
begin
	pt:= ClientToScreen(MousePos);
	TrackPopupMenu(ViewMenu.Handle, TPM_LEFTALIGN or TPM_LEFTBUTTON,pt.x, pt.y, 0, Self.Handle, nil);
	Handled:= TRUE;
end;

function TMainForm.GetEditor(index: integer): TEditor;
var
	i: integer;
begin
	if index = -1 then
		i := PageControl.ActivePageIndex
	else
		i := index;

	if (PageControl.PageCount <= 0) or (i < 0) then
		result:= nil
	else
		result:= TEditor(PageControl.Pages[i].Tag);
end;

procedure TMainForm.actAddWatchExecute(Sender: TObject);
var
	s: AnsiString;
	e: TEditor;
begin
	s:='';
	e:=GetEditor;
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
	node : TTreeNode;
begin
	node := DebugTree.Selected;
	if Assigned(node) then begin

		// Retrieve topmost node
		while Assigned(node.Parent) do
			node := node.Parent;

		fDebugger.RemoveWatchVar(node);
	end;
end;

procedure TMainForm.RemoveActiveBreakpoints;
var
	i : integer;
begin
	for i := 0 to PageControl.PageCount - 1 do
		GetEditor(i).RemoveBreakpointFocus;
end;

procedure TMainForm.GotoBreakpoint(const bfile : AnsiString; bline : integer);
var
	e : TEditor;
begin
	// Remove line focus in files left behind
	RemoveActiveBreakpoints;

	// Then active the current line in the current file
	e := GetEditorFromFileName(StringReplace(bfile, '/', '\', [rfReplaceAll]));
	if assigned(e) then begin
		e.Activate;
		e.SetActiveBreakpointFocus(bline);
	end;
	Application.BringToFront;
end;

procedure TMainForm.actStepOverExecute(Sender: TObject);
begin
	if fDebugger.Executing then begin
		RemoveActiveBreakpoints;
		fDebugger.SendCommand('continue','', true);
	end;
end;

procedure TMainForm.actForceStopExecuteExecute(Sender: TObject);
begin
	fDebugger.Stop;
end;

procedure TMainForm.actUndoUpdate(Sender: TObject);
var
	e: TEditor;
begin
	e:= GetEditor;
	actUndo.Enabled := assigned(e) and e.Text.CanUndo;
end;

procedure TMainForm.actRedoUpdate(Sender: TObject);
var
	e: TEditor;
begin
	e:= GetEditor;
	actRedo.enabled:= assigned(e) and e.Text.CanRedo;
end;

procedure TMainForm.actCutUpdate(Sender: TObject);
var
	e: TEditor;
begin
	e:= GetEditor;
	actCut.Enabled:= assigned(e) and e.Text.SelAvail;
end;

procedure TMainForm.actCopyUpdate(Sender: TObject);
var
	e: TEditor;
begin
	e:= GetEditor;
	actCopy.Enabled:= assigned(e) and e.Text.SelAvail;
end;

procedure TMainForm.actPasteUpdate(Sender: TObject);
var
	e: TEditor;
begin
	e:= GetEditor;
	actPaste.Enabled:= assigned(e) and e.Text.CanPaste;
end;

procedure TMainForm.actSaveUpdate(Sender: TObject);
var
	e: TEditor;
begin
	e:= GetEditor;
	actSave.Enabled:= assigned(e) and (e.Text.Modified or (e.FileName = ''));
end;

procedure TMainForm.actSaveAsUpdate(Sender: TObject);
var
	e: TEditor;
begin
	e:= GetEditor;
	actSaveAs.Enabled:= assigned(e);
end;

procedure TMainForm.ClearMessageControl;
begin
	CompilerOutput.Items.Clear;
	FindOutput.Items.Clear;
	ResourceOutput.Clear;
	LogOutput.Clear;
	DebugOutput.Clear;
end;

procedure TMainForm.actFileMenuExecute(Sender: TObject);
begin
//	dummy event to keep menu active
end;

procedure TMainForm.actToolsMenuExecute(Sender: TObject);
var
	I, J: integer;
begin
	for I := (ToolsMenu.IndexOf(PackageManagerItem)+2) to ToolsMenu.Count - 1 do begin
		J := ToolsMenu.Items[I].tag;
		ToolsMenu.Items[I].Enabled:= FileExists(ParseParams(fTools.ToolList[J]^.Exec));
		if not ToolsMenu.Items[I].Enabled then
			ToolsMenu.Items[I].Caption := fTools.ToolList[J]^.Title + ' (Tool not found)';
	end;
end;

function TMainForm.GetEditorFromFileName(const ffile: AnsiString) : TEditor;
var
	index, index2 : integer; //mandrav
	e: TEditor;
begin
	result := nil;

	// First, check wether the file is already open
	for index := 0 to PageControl.PageCount - 1 do begin
		e := GetEditor(index);
		if Assigned(e) then begin
			//ExpandFileName reduces all the "\..\" in the path
			if SameFileName(e.FileName, ExpandFileName(ffile)) then begin
				Result := e;
				Exit;
			end;
		end;
	end;

	if fCompiler.Target in [ctFile, ctNone] then begin
		if FileExists(ffile) then
			OpenFile(ffile);
		index := FileIsOpen(ffile);
		if index <> -1 then
			result := GetEditor(index);
	end else if (fCompiler.Target = ctProject) and Assigned(fProject) then begin
		index := fProject.GetUnitFromString(ffile);
		if index <> -1 then begin
			//mandrav
			index2 := FileIsOpen(ExpandFileName(fProject.Directory + ffile), TRUE);
			if index2 = -1 then
				result := fProject.OpenUnit( index )
			else
				result := GetEditor( index2 );
			//mandrav - end
		end else begin
			if FileExists(ffile) then
				OpenFile(ffile);
			index := FileIsOpen(ffile);
			if index <> -1 then
				result := GetEditor(index);
		end;
	end;
end;

procedure TMainForm.UpdateSplash(const text : AnsiString);
begin
	if Assigned(SplashForm) then
		SplashForm.Statusbar.SimpleText := 'Bloodshed Dev-C++ 4.9.9.2 (Orwell update '+ DEVCPP_VERSION + ') ' + text;
end;

procedure TMainForm.InitClassBrowser(ReloadCache: boolean);
var
	sl: TStringList;
	I: integer;
begin
	CppParser.Enabled := devClassBrowsing.Enabled;
	CppParser.ParseLocalHeaders := devClassBrowsing.ParseLocalHeaders;
	CppParser.ParseGlobalHeaders := devClassBrowsing.ParseGlobalHeaders;
	CppParser.Tokenizer := CppTokenizer;
	CppParser.OnStartParsing := CppParserStartParsing;
	CppParser.OnEndParsing := CppParserEndParsing;
	CppParser.OnTotalProgress := CppParserTotalProgress;

	// Add the include dirs to the parser
	sl := TStringList.Create;
	ExtractStrings([';'], [' '], PAnsiChar(devCompiler.CDir), sl);
	for I := 0 to sl.Count - 1 do
		CppParser.AddIncludePath(sl[I]);

	sl.Clear;
	ExtractStrings([';'], [' '], PAnsiChar(devCompiler.CppDir), sl);
	for I := 0 to sl.Count - 1 do
		CppParser.AddIncludePath(sl[I]);
	sl.Free;

	// This takes up about 99% of our time
	if devCodeCompletion.UseCacheFiles and ReloadCache then begin
		try
			Application.ProcessMessages;
			CppParser.Load(devDirs.Config + DEV_COMPLETION_CACHE,devDirs.Exec);
		except
			if MessageDlg(
				Format(Lang[ID_ENV_CACHEFAIL],[devDirs.Config,ExcludeTrailingBackslash(devDirs.Config) + 'Backup' + pd]),
				mtConfirmation,[mbYes,mbNo],0) = mrYes then

				// Close the form on show, because
				// Calling Close or Free over here (inside WM_CREATE) will not exit the process properly
				// Calling PostQuitMessage frees the thread, but leaves handles to settings files open,
				// so we can't remove the settings folder...
				fRemoveOptions := true;
		end;
	end;

	CodeCompletion.Color := devCodeCompletion.BackColor;
	CodeCompletion.Width := devCodeCompletion.Width;
	CodeCompletion.Height := devCodeCompletion.Height;

	ClassBrowser.Enabled := devClassBrowsing.Enabled;
	ClassBrowser.ShowFilter := TShowFilter(devClassBrowsing.ShowFilter);
	ClassBrowser.ShowInheritedMembers := devClassBrowsing.ShowInheritedMembers;
	ClassBrowser.UseColors := devClassBrowsing.UseColors;
	ClassBrowser.ClassFoldersFile := DEV_CLASSFOLDERS_FILE;

	actBrowserViewAll.Checked := ClassBrowser.ShowFilter=sfAll;
	actBrowserViewProject.Checked := ClassBrowser.ShowFilter=sfProject;
	actBrowserViewCurrent.Checked := ClassBrowser.ShowFilter=sfCurrent;
	actBrowserUseColors.Checked := ClassBrowser.UseColors;
	actBrowserShowInherited.Checked := ClassBrowser.ShowInheritedMembers;
end;

procedure TMainForm.ScanActiveProject;
var
 I: integer;
 I1: Cardinal;
 e: TEditor;
begin
	Application.ProcessMessages;

	if not ClassBrowser.Enabled then
		Exit;
	if Assigned(fProject) then
		ClassBrowser.ProjectDir:=fProject.Directory
	else begin
		e:=GetEditor;
		if Assigned(e) then
			ClassBrowser.ProjectDir:=ExtractFilePath(e.FileName)
		else
			ClassBrowser.ProjectDir:='';
	end;
	I1:=GetTickCount;
	with CppParser do begin
		Reset;
		if Assigned(fProject) then begin
			ProjectDir:=fProject.Directory;
			for I:=0 to fProject.Units.Count-1 do
				AddFileToScan(fProject.Units[I].FileName, True);
			for I:=0 to fProject.Options.Includes.Count-1 do
				AddProjectIncludePath(fProject.Options.Includes[I]);
		end else begin
			e:=GetEditor;
			if Assigned(e) then begin
				ProjectDir:=ExtractFilePath(e.FileName);
				AddFileToScan(e.FileName);
			end else
				ProjectDir:='';
		end;
		ParseList;
		if PageControl.ActivePageIndex>-1 then begin
			e:=GetEditor(PageControl.ActivePageIndex);
			if Assigned(e) then
				ClassBrowser.CurrentFile:=e.FileName;
		end;
	end;
	SetStatusbarMessage(Lang[ID_DONEPARSING] + ' in ' + FormatFloat('#,###,##0.00', (GetTickCount-I1)/1000)+' seconds');
end;

procedure TMainForm.ClassBrowser1Select(Sender: TObject;Filename: TFileName; Line: Integer);
var
	E: TEditor;
begin
	// mandrav
	E := GetEditorFromFilename(FileName);
	if Assigned(E) then
		E.GotoLineNr(Line);
end;

procedure TMainForm.CppParserTotalProgress(Sender: TObject;const FileName: string; Total, Current: Integer);
begin
	if FileName <> '' then
		SetStatusBarMessage('Parsing '+ Filename)
	else
		SetStatusBarMessage(Lang[ID_DONEPARSING]);
	Statusbar.Repaint;
end;

procedure TMainForm.CodeCompletionResize(Sender: TObject);
begin
	devCodeCompletion.Width:=CodeCompletion.Width;
	devCodeCompletion.Height:=CodeCompletion.Height;
end;

procedure TMainForm.actSwapHeaderSourceUpdate(Sender: TObject);
begin
	actSwapHeaderSource.Enabled := PageControl.PageCount > 0;
end;

procedure TMainForm.actSwapHeaderSourceExecute(Sender: TObject);
var
	e: TEditor;
	FromFile,ToFile: AnsiString;
	i : integer;
	toheader,tosource : boolean;
begin
	e:= GetEditor;
	if not Assigned(e) then
		Exit;

	FromFile := e.FileName;

	if GetFileTyp(FromFile) in [utcSrc,utcppSrc] then begin
		toheader := true;
		tosource := false;
	end else if GetFileTyp(e.FileName) in [utcHead,utcppHead] then begin
		tosource := true;
		toheader := false;
	end else
		Exit;

	if Assigned(fProject) then begin

		// Check project list
		for i := 0 to fProject.Units.Count - 1 do begin
			if ((GetFileTyp(fProject.Units[i].FileName) in [utcHead,utcppHead]) and toheader) or
               ((GetFileTyp(fProject.Units[i].FileName) in [utcSrc,utcppSrc])   and tosource) then begin

				// Compare name without extension...
				if AnsiCompareFileName(ChangeFileExt(ExtractFileName(FromFile),''),ChangeFileExt(ExtractFileName(fProject.Units[i].FileName),'')) = 0 then begin
					ToFile := fProject.Units[i].FileName;
					break;
				end;
			end;
		end;
	end else begin

		// Check opened files
		for i := 0 to PageControl.PageCount - 1 do begin
			e := GetEditor(i);
			if Assigned(e) then begin

				if ((GetFileTyp(e.FileName) in [utcHead,utcppHead]) and toheader) or
                   ((GetFileTyp(e.FileName) in [utcSrc,utcppSrc])   and tosource) then begin

					// Compare name without extension...
					if AnsiCompareFileName(ChangeFileExt(ExtractFileName(FromFile),''),ChangeFileExt(ExtractFileName(e.FileName),'')) = 0 then begin
						ToFile := e.FileName;
						break;
					end;
				end;
			end;
		end;
	end;

	e := GetEditorFromFileName(ToFile);
	if Assigned(e) then
		e.Activate
	else
		SetStatusBarMessage('Could not find corresponding header/source file!');
end;

procedure TMainForm.actSyntaxCheckExecute(Sender: TObject);
begin
	if fCompiler.Compiling then begin
		MessageDlg(Lang[ID_MSG_ALREADYCOMP], mtInformation, [mbOK], 0);
		Exit;
	end;
	if not PrepareForCompile then
		Exit;
	fCompiler.CheckSyntax;
end;

procedure TMainForm.actUpdateExecute(Sender: TObject);
begin
	if Assigned(fProject) then
		TCustomAction(Sender).Enabled := not (fProject.Options.typ = dptStat)
	else
		TCustomAction(Sender).Enabled := PageControl.PageCount > 0;
end;

procedure TMainForm.actRunUpdate(Sender: TObject);
begin
	if Assigned(fProject) then
		TCustomAction(Sender).Enabled := not (fProject.Options.typ = dptStat) and not devExecutor.Running and not fDebugger.Executing and not fCompiler.Compiling
	else
		TCustomAction(Sender).Enabled := (PageControl.PageCount > 0) and not devExecutor.Running and not fDebugger.Executing and not fCompiler.Compiling;
end;

procedure TMainForm.actCompileRunUpdate(Sender: TObject);
begin
	if Assigned(fProject) then
		TCustomAction(Sender).Enabled := not (fProject.Options.typ = dptStat) and not devExecutor.Running and not fDebugger.Executing and not fCompiler.Compiling and (devCompiler.Sets.Count > 0)
	else
		TCustomAction(Sender).Enabled := (PageControl.PageCount > 0) and not devExecutor.Running and not fDebugger.Executing and not fCompiler.Compiling and (devCompiler.Sets.Count > 0);
end;

procedure TMainForm.EditorSaveTimer(Sender : TObject);
var
	e : TEditor;
begin
	e:=GetEditor;
	if Assigned(e) then begin
		if devEditor.SaveType = 0 then begin
			if SaveFile(e) then
				SetStatusbarMessage('Autosaved file "' + e.FileName + '"')
		end else if devEditor.SaveType = 1 then begin
			actSaveAllExecute(nil);
			SetStatusbarMessage('Autosaved all open files');
		end;
	end;
end;

procedure TMainForm.PageControlChange(Sender: TObject);
var
	e: TEditor;
	i, x, y : integer;
begin
	if PageControl.ActivePageIndex <> -1 then begin
		e:=GetEditor(PageControl.ActivePageIndex);
		if Assigned(e) then begin
			e.Text.SetFocus;

			// Keep window title updated
			UpdateAppTitle;

			// keep Status bar updated
			SetStatusbarLineCol;

			if ClassBrowser.Enabled then
				ClassBrowser.CurrentFile:=e.FileName;

			for i := 1 to 9 do
				if e.Text.GetBookMark(i, x, y) then begin
					TogglebookmarksPopItem.Items[i - 1].Checked := true;
					TogglebookmarksItem.Items[i - 1].Checked := true;
				end else begin
					TogglebookmarksPopItem.Items[i - 1].Checked := false;
					TogglebookmarksItem.Items[i - 1].Checked := false;
				end;
		end;
	end;
end;

procedure TMainForm.actConfigdevShortcutsExecute(Sender: TObject);
begin
	if(devShortcuts.FileName[2] <> ':') then // if relative
		devShortcuts.FileName := devdirs.Exec+devShortcuts.FileName;
	devShortcuts.Edit;
end;

procedure TMainForm.DateTimeMenuItemClick(Sender: TObject);
var
	e : TEditor;
begin
	e := GetEditor;
	if Assigned(e) then
		e.InsertString(FormatDateTime('dd/mm/yy hh:nn', Now), TRUE);
end;

procedure TMainForm.actProgramResetExecute(Sender: TObject);
begin
	devExecutor.Reset;
end;

procedure TMainForm.actProgramResetUpdate(Sender: TObject);
begin
	if Assigned(fProject) then // TODO: make one boolean statement
		TCustomAction(Sender).Enabled := not (fProject.Options.typ = dptStat) and devExecutor.Running
	else
		TCustomAction(Sender).Enabled := (PageControl.PageCount > 0) and devExecutor.Running;
end;

procedure TMainForm.CommentheaderMenuItemClick(Sender: TObject);
var
	e : TEditor;
begin
	e := GetEditor;
	if assigned(e) then begin
		e.InsertString('/*' + #13#10 +
						'	Name: ' + #13#10 +
						'	Copyright: ' + #13#10 +
						'	Author: ' + #13#10 +
						'	Date: ' + FormatDateTime('dd/mm/yy hh:nn', Now) + #13#10 +
						'	Description: ' + #13#10 +
						'*/' + #13#10, true);
	end;
end;

procedure TMainForm.PageControlMouseDown(Sender: TObject;Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
	thispage : integer;
begin
	thispage:=PageControl.IndexOfTabAt(X, Y);
	if Button = mbRight then begin // select new tab even with right mouse button
		if thispage <> -1 then begin
			PageControl.ActivePageIndex:=thispage;
			PageControlChange(nil);
		end;
	end else if Button = mbMiddle then begin
		if thispage <> -1 then begin
			CloseEditor(thispage);
		end;
	end else // see if it's a drag operation
		PageControl.Pages[PageControl.ActivePageIndex].BeginDrag(False);
end;

procedure TMainForm.actNewTemplateUpdate(Sender: TObject);
begin
	actNewTemplate.Enabled:=Assigned(fProject);
end;

procedure TMainForm.actCommentExecute(Sender: TObject);
var
	e: TEditor;
begin
	e:=GetEditor;
	if Assigned(e) then
		e.CommentSelection;
end;

procedure TMainForm.actUncommentExecute(Sender: TObject);
var
	e: TEditor;
begin
	e:=GetEditor;
	if Assigned(e) then
		e.UncommentSelection;
end;

procedure TMainForm.actIndentExecute(Sender: TObject);
var
	e: TEditor;
begin
	e:=GetEditor;
	if Assigned(e) then
		e.IndentSelection;
end;

procedure TMainForm.actUnindentExecute(Sender: TObject);
var
	e: TEditor;
begin
	e:=GetEditor;
	if Assigned(e) then
		e.UnindentSelection;
end;

procedure TMainForm.PageControlDragOver(Sender, Source: TObject; X,Y: Integer; State: TDragState; var Accept: Boolean);
var
	I: integer;
begin
	I:=PageControl.IndexOfTabAt(X, Y);
	Accept:=(Source is TTabSheet) and (I<>PageControl.ActivePageIndex);
end;

procedure TMainForm.PageControlDragDrop(Sender, Source: TObject; X,Y: Integer);
var
	I: integer;
begin
	I:=PageControl.IndexOfTabAt(X, Y);
	if (Source is TTabSheet) and (I<>PageControl.ActivePageIndex) then
		PageControl.Pages[PageControl.ActivePageIndex].PageIndex:=I;
end;

procedure TMainForm.actGotoFunctionExecute(Sender: TObject);
var
	e: TEditor;
	st: PStatement;
begin
	with TFunctionSearchForm.Create(Self) do begin
		fFileName:=GetEditor.FileName;
		fParser:=CppParser;
		if ShowModal=mrOK then begin
			st:=PStatement(lvEntries.Selected.Data);
			//make sure statement still exists
			if fParser.Statements.IndexOf(lvEntries.Selected.Data) = -1 then
				lvEntries.Selected.Data := nil;
			e:=GetEditor;
			if Assigned(e) and Assigned(st) then begin
				if st^._IsDeclaration then
					e.GotoLineNr(st^._DeclImplLine)
				else
					e.GotoLineNr(st^._Line);
			end;
		end;
	end;
end;

procedure TMainForm.actBrowserGotoDeclUpdate(Sender: TObject);
begin
	(Sender as TAction).Enabled:=Assigned(ClassBrowser.Selected);
end;

procedure TMainForm.actBrowserGotoImplUpdate(Sender: TObject);
begin
	if Assigned(ClassBrowser) and ClassBrowser.Enabled and Assigned(ClassBrowser.Selected) and Assigned(ClassBrowser.Selected.Data) then

		//check if node.data still in statements
		if CppParser.Statements.IndexOf(ClassBrowser.Selected.Data) >=0 then
			TCustomAction(Sender).Enabled := (PStatement(ClassBrowser.Selected.Data)^._Kind<>skClass)
		else begin
			ClassBrowser.Selected.Data := nil;
			TCustomAction(Sender).Enabled := False;
		end else
			TCustomAction(Sender).Enabled := False;
end;

procedure TMainForm.actBrowserNewClassUpdate(Sender: TObject);
begin
	(Sender as TAction).Enabled:=ClassBrowser.Enabled and Assigned(fProject);
end;

procedure TMainForm.actBrowserNewMemberUpdate(Sender: TObject);
begin
	if Assigned(ClassBrowser.Selected) and Assigned(ClassBrowser.Selected.Data) then

		//check if node.data still in statements
		if CppParser.Statements.IndexOf(ClassBrowser.Selected.Data) >= 0 then
			TCustomAction(Sender).Enabled := (PStatement(ClassBrowser.Selected.Data)^._Kind=skClass)
		else begin
			ClassBrowser.Selected.Data := nil;
			TCustomAction(Sender).Enabled := False;
		end else
			TCustomAction(Sender).Enabled := False;
end;

procedure TMainForm.actBrowserNewVarUpdate(Sender: TObject);
begin
	if Assigned(ClassBrowser.Selected) and Assigned(ClassBrowser.Selected.Data) then
		//check if node.data still in statements
		if CppParser.Statements.IndexOf(ClassBrowser.Selected.Data) >= 0 then
			TCustomAction(Sender).Enabled := (PStatement(ClassBrowser.Selected.Data)^._Kind=skClass)
		else begin
			ClassBrowser.Selected.Data := nil;
			TCustomAction(Sender).Enabled := False;
		end else
			TCustomAction(Sender).Enabled := False;
end;

procedure TMainForm.actBrowserAddFolderUpdate(Sender: TObject);
begin
	TCustomAction(Sender).Enabled := ClassBrowser.Enabled and Assigned(fProject);
end;

procedure TMainForm.actBrowserViewAllUpdate(Sender: TObject);
begin
	TCustomAction(Sender).Enabled := True;
end;

procedure TMainForm.actBrowserGotoDeclExecute(Sender: TObject);
var
	e: TEditor;
	Node: TTreeNode;
	fname: AnsiString;
	line: integer;
begin
	Node:=ClassBrowser.Selected;
	fName:=CppParser.GetDeclarationFileName(Node.Data);
	line:=CppParser.GetDeclarationLine(Node.Data);
	e:=GetEditorFromFileName(fname);
	if Assigned(e) then
		e.GotoLineNr(line);
end;

procedure TMainForm.actBrowserGotoImplExecute(Sender: TObject);
var
	e: TEditor;
	Node: TTreeNode;
	fname: AnsiString;
	line: integer;
begin
	Node:=ClassBrowser.Selected;
	fName:=CppParser.GetImplementationFileName(Node.Data);
	line:=CppParser.GetImplementationLine(Node.Data);
	e:=GetEditorFromFileName(fname);
	if Assigned(e) then
		e.GotoLineNr(line);
end;

procedure TMainForm.actBrowserNewClassExecute(Sender: TObject);
begin
	TNewClassForm.Create(Self).ShowModal;
end;

procedure TMainForm.actBrowserNewMemberExecute(Sender: TObject);
begin
	TNewMemberForm.Create(Self).ShowModal;
end;

procedure TMainForm.actBrowserNewVarExecute(Sender: TObject);
begin
	TNewVarForm.Create(Self).ShowModal;
end;

procedure TMainForm.actBrowserViewAllExecute(Sender: TObject);
begin
	ClassBrowser.ShowFilter:=sfAll;
	actBrowserViewAll.Checked:=True;
	actBrowserViewProject.Checked:=False;
	actBrowserViewCurrent.Checked:=False;
	devClassBrowsing.ShowFilter:=Ord(sfAll);
end;

procedure TMainForm.actBrowserViewProjectExecute(Sender: TObject);
begin
	ClassBrowser.ShowFilter:=sfProject;
	actBrowserViewAll.Checked:=False;
	actBrowserViewProject.Checked:=True;
	actBrowserViewCurrent.Checked:=False;
	devClassBrowsing.ShowFilter:=Ord(sfProject);
end;

procedure TMainForm.actBrowserViewCurrentExecute(Sender: TObject);
var
	e: TEditor;
begin
	ClassBrowser.ShowFilter:=sfCurrent;
	actBrowserViewAll.Checked:=False;
	actBrowserViewProject.Checked:=False;
	actBrowserViewCurrent.Checked:=True;
	e:=GetEditor;
	if Assigned(e) then
		ClassBrowser.CurrentFile:=e.FileName
	else
		ClassBrowser.CurrentFile:='';
	devClassBrowsing.ShowFilter:=Ord(sfCurrent);
end;

procedure TMainForm.actProfileProjectExecute(Sender: TObject);
var
	optP, optS: PCompilerOption;
	idxP, idxS: integer;
	prof, strp: boolean;
	path : AnsiString;
	e : TEditor;
begin

	prof := CheckCompileOption('-pg',optP,idxP);
	strp := CheckCompileOption('-s',optS,idxS);

	if not prof or strp then begin
		if MessageDlg(Lang[ID_MSG_NOPROFILE], mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin

			// ENABLE profiling
			if Assigned(fProject) then
				SetProjCompOpt(idxP,'1')
			else
				devCompiler.SetOption(optP,idxP,'1');

			// DISABLE stripping
			if Assigned(fProject) then
				SetProjCompOpt(idxS,'0')
			else
				devCompiler.SetOption(optS,idxS,'0');

			if not Assigned(fProject) then
				devCompiler.SaveSet(devCompiler.CurrentIndex);

			actRebuildExecute(nil);
			Exit;
		end;
	end;

	// If we're done setting up options, check if there's profiling data already
	path := '';
	if Assigned(fProject) then begin
		path := ExtractFilePath(fProject.Executable) + GPROF_CHECKFILE;
	end else begin
		e:=GetEditor;
		if Assigned(e) then
			path := ExtractFilePath(ChangeFileExt(e.FileName, EXE_EXT)) + GPROF_CHECKFILE;
	end;

	// Ask the user if he wants to generate data...
	if not FileExists(path) then begin
		if MessageDlg(Lang[ID_MSG_NORUNPROFILE], mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
			actRunExecute(nil);
		end;
		Exit;
	end;

	// If the data is there, open up the form
	if not Assigned(ProfileAnalysisForm) then
		ProfileAnalysisForm := TProfileAnalysisForm.Create(Self);

	ProfileAnalysisForm.Show;
end;

procedure TMainForm.actBrowserAddFolderExecute(Sender: TObject);
var
	S: AnsiString;
	Node: TTreeNode;
begin
	if ClassBrowser.FolderCount>=MAX_CUSTOM_FOLDERS then begin
		MessageDlg(Format(Lang[ID_POP_MAXFOLDERS], [MAX_CUSTOM_FOLDERS]), mtError, [mbOk], 0);
		Exit;
	end;

	Node:=ClassBrowser.Selected;
	S:='New folder';
	if InputQuery(Lang[ID_POP_ADDFOLDER], Lang[ID_MSG_ADDBROWSERFOLDER], S) and (S<>'') then
		ClassBrowser.AddFolder(S, Node);
end;

procedure TMainForm.actBrowserRemoveFolderExecute(Sender: TObject);
begin
	if Assigned(ClassBrowser.Selected) and (ClassBrowser.Selected.ImageIndex=ClassBrowser.ItemImages.Globals) then
		if MessageDlg(Lang[ID_MSG_REMOVEBROWSERFOLDER], mtConfirmation, [mbYes, mbNo], 0)=mrYes then
			ClassBrowser.RemoveFolder(ClassBrowser.Selected.Text);
end;

procedure TMainForm.actBrowserRenameFolderExecute(Sender: TObject);
var
	S: AnsiString;
begin
	if Assigned(ClassBrowser.Selected) and (ClassBrowser.Selected.ImageIndex=ClassBrowser.ItemImages.Globals) then begin
		S:=ClassBrowser.Selected.Text;
		if InputQuery(Lang[ID_POP_RENAMEFOLDER], Lang[ID_MSG_RENAMEBROWSERFOLDER], S) and (S<>'') then
			ClassBrowser.RenameFolder(ClassBrowser.Selected.Text, S);
	end;
end;

procedure TMainForm.actCloseAllButThisExecute(Sender: TObject);
var
	I,current,count: integer;
begin
	current := PageControl.ActivePageIndex;
	count := PageControl.PageCount;

	for I := count - 1 downto 0 do
		if I <> current then
			if not CloseEditor(I) then
				Break;
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

procedure TMainForm.lvBacktraceMouseMove(Sender: TObject;Shift: TShiftState; X, Y: Integer);
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

procedure TMainForm.devFileMonitorNotifyChange(Sender: TObject;ChangeType: TdevMonitorChangeType; Filename: string);
var
	e: TEditor;
	p : TBufferCoord;
begin
	case ChangeType of
		mctChanged: begin
			if MessageDlg(Filename + ' has changed. Reload from disk?', mtConfirmation, [mbYes, mbNo], 0)=mrYes then begin
				e:=GetEditorFromFileName(Filename);
				if Assigned(e) then begin
					p := e.Text.CaretXY;
					e.Text.Lines.LoadFromFile(Filename);
					if (p.Line <= e.Text.Lines.Count) then
						 e.Text.CaretXY := p;
				end;
			end;
		end;
		mctDeleted : begin
			MessageDlg(Filename + ' has been renamed or deleted...', mtInformation, [mbOk], 0);
		end;
	end;
end;

procedure TMainForm.actFilePropertiesExecute(Sender: TObject);
begin
	with TFilePropertiesForm.Create(Self) do begin
		if TAction(Sender).ActionComponent=mnuUnitProperties then
			if Assigned(fProject) and
				 Assigned(ProjectView.Selected) and
				 (ProjectView.Selected.Data<>Pointer(-1)) then
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
	fp, S: AnsiString;
begin
	S:='New folder';
	if InputQuery(Lang[ID_POP_ADDFOLDER], Lang[ID_MSG_ADDBROWSERFOLDER], S) and (S<>'') then begin
		fp:=fProject.GetFolderPath(ProjectView.Selected);
		if fp<>'' then
			fProject.AddFolder(fp+'/'+S)
		else
			fProject.AddFolder(S);
	end;
end;

procedure TMainForm.actProjectRemoveFolderExecute(Sender: TObject);
var
	idx: integer;
	node: TTreeNode;
begin
	if not assigned(fProject) then exit;
	if Assigned(ProjectView.Selected) and (ProjectView.Selected.Data=Pointer(-1)) then
		if MessageDlg(Lang[ID_MSG_REMOVEBROWSERFOLDER], mtConfirmation, [mbYes, mbNo], 0)=mrYes then begin
			ProjectView.Items.BeginUpdate;
			while ProjectView.Selected.Count > 0 do begin
				node := ProjectView.Selected.Item[0];
				if not assigned(node) then
					Continue;
				if (node.Data = Pointer(-1)) or (node.Level < 1) then
				 Continue;
				idx:= integer(node.Data);
				if not fProject.Remove(idx, true) then
					exit;
			end;
			ProjectView.TopItem.AlphaSort(False);
			ProjectView.Selected.Delete;
			ProjectView.Items.EndUpdate;
			fProject.UpdateFolders;
		end;
end;

procedure TMainForm.actProjectRenameFolderExecute(Sender: TObject);
var
	S: AnsiString;
begin
	if Assigned(ProjectView.Selected) and (ProjectView.Selected.Data=Pointer(-1)) then begin
		S:=ProjectView.Selected.Text;
		if InputQuery(Lang[ID_POP_RENAMEFOLDER], Lang[ID_MSG_RENAMEBROWSERFOLDER], S) and (S<>'') then begin
			ProjectView.Selected.Text:=S;
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
	Hits:=ProjectView.GetHitTestInfoAt(X, Y);
	if ([htOnItem, htOnRight, htToRight]*Hits)<>[] then
		Item:=ProjectView.GetNodeAt(X, Y)
	else
		Item:=nil;
	Accept:=
		(
			(Sender=ProjectView) and Assigned(ProjectView.Selected) and Assigned(Item) //and
			// drop node is a folder or the project
			and ( (Item.Data=Pointer(-1)) or (Item.SelectedIndex=0) )
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
	if not (Source is TTreeView) then exit;
	if ([htOnItem, htOnRight, htToRight]*ProjectView.GetHitTestInfoAt(X, Y))<>[] then
		Item:=ProjectView.GetNodeAt(X, Y)
	else
		Item:=nil;
	for I:=0 to ProjectView.SelectionCount-1 do begin
		srcNode:=ProjectView.Selections[I];
		if not Assigned(Item) then begin
			fProject.Units[Integer(srcNode.Data)].Folder:='';
			srcNode.MoveTo(ProjectView.Items[0], naAddChild);
		end else begin
			if srcNode.Data<>Pointer(-1) then begin
				if Item.Data=Pointer(-1) then
					fProject.Units[Integer(srcNode.Data)].Folder:=fProject.GetFolderPath(Item)
				else
					fProject.Units[Integer(srcNode.Data)].Folder:='';
			end;
			WasExpanded:=Item.Expanded;
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
		if ShowModal=mrOK then
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
				Key := #0;
				fDebugger.SendCommand(edGDBCommand.Text,'');

				if edGDBCommand.Items.IndexOf(edGDBCommand.Text) = -1 then
					edGDBCommand.AddItem(edGDBCommand.Text,nil);
			end;
		end;

		// View command examples only when edit is empty or when the UI itself added the current command
		fDebugger.GDBcommandchanged := true;
	end;
end;

procedure TMainForm.CheckForDLLProfiling;
var
	opts: TProjOptions;
	opt: PCompilerOption;
	idx: integer;
begin
	if not Assigned(fProject) then
		Exit;

	// profiling not enabled
	if not devCompiler.FindOption('-pg', opt, idx) then
		Exit;

	if (fProject.Options.typ in [dptDyn, dptStat]) and (opt.optValue > 0) then begin
		// project is a lib or a dll, and profiling is enabled
		// check for the existence of "-lgmon" in project's linker options
		if Pos('-lgmon', fProject.Options.cmdLines.Linker) = 0 then
			// does not have -lgmon
			// warn the user that we should update its project options and include
			// -lgmon in linker options, or else the compilation will fail
			if MessageDlg('You have profiling enabled in Compiler Options and you are '+
			'working on a dynamic or static library. Do you want to add a special '+
			'linker option in your project to allow compilation with profiling enabled? '+
			'If you choose No, and you do not disable profiling from the Compiler Options '+
			'chances are that your library''s compilation will fail...', mtConfirmation,
			[mbYes, mbNo], 0)=mrYes then begin
				opts:=fProject.Options;
				opts.cmdLines.Linker:=fProject.Options.cmdLines.Linker+' -lgmon';
				fProject.Options:=opts;
				fProject.Modified:=True;
			end;
	end;
end;

procedure TMainForm.actExecParamsExecute(Sender: TObject);
begin
	with TParamsForm.Create(self) do begin
		ParamEdit.Text := fCompiler.RunParams;
		if Assigned(fProject) then
			HostEdit.Text := fProject.Options.HostApplication;
		if not Assigned(fProject) or (fProject.Options.typ <> dptDyn) then
			DisableHost;
		if (ShowModal = mrOk) then begin
			fCompiler.RunParams := ParamEdit.Text;
			if (HostEdit.Enabled) then
				fProject.SetHostApplication(HostEdit.Text);
		end;
		Free;
	end;
end;

procedure TMainForm.DevCppDDEServerExecuteMacro(Sender: TObject;
	Msg: TStrings);
var
	filename: AnsiString;
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
		Show;
end;

procedure TMainForm.actBrowserUseColorsExecute(Sender: TObject);
begin
	actBrowserUseColors.Checked:=not actBrowserUseColors.Checked;
	devClassBrowsing.UseColors:=actBrowserUseColors.Checked;
	ClassBrowser.UseColors:=actBrowserUseColors.Checked;
	ClassBrowser.Refresh;
end;

procedure TMainForm.HelpMenuItemClick(Sender: TObject);
begin
	OpenHelpFile;
end;

procedure TMainForm.CppParserStartParsing(Sender: TObject);
begin
	Screen.Cursor:=crHourglass;
end;

procedure TMainForm.CppParserEndParsing(Sender: TObject);
begin
	Screen.Cursor:=crDefault;

	// do this work only if this was the last file scanned
	if CppParser.FilesToScan.Count = 0 then begin
		SetStatusBarMessage(Lang[ID_DONEPARSING]);

		RebuildClassesToolbar;
	end;
end;

procedure TMainForm.UpdateAppTitle;
var
	e : TEditor;
begin
	if Assigned(fProject) then begin
		if fDebugger.Executing then begin
			Caption := Format('%s - [%s] - [Debugging] - %s %s',
				[fProject.Name, ExtractFilename(fProject.Filename), DEVCPP, DEVCPP_VERSION]);
			Application.Title := Format('%s - [Debugging] - %s', [fProject.Name, DEVCPP]);
		end else if devExecutor.Running then begin
			Caption :=  Format('%s - [%s] - [Executing] - %s %s',
				[fProject.Name, ExtractFilename(fProject.Filename), DEVCPP, DEVCPP_VERSION]);
			Application.Title := Format('%s - [Executing] - %s', [fProject.Name, DEVCPP]);
		end else if fCompiler.Compiling then begin
			Caption :=  Format('%s - [%s] - [Compiling] - %s %s',
				[fProject.Name, ExtractFilename(fProject.Filename), DEVCPP, DEVCPP_VERSION]);
			Application.Title := Format('%s - [Compiling] - %s', [fProject.Name, DEVCPP]);
		end else begin
			Caption :=  Format('%s - [%s] - %s %s',
				[fProject.Name, ExtractFilename(fProject.Filename), DEVCPP, DEVCPP_VERSION]);
			Application.Title := Format('%s - %s', [fProject.Name, DEVCPP]);
		end;
	end else begin
		e := GetEditor;
		if Assigned(e) then begin
			if fDebugger.Executing then begin
				Caption :=  Format('%s - [Debugging] - %s %s',[e.FileName, DEVCPP, DEVCPP_VERSION]);
				Application.Title := Format('%s - [Debugging] - %s', [ExtractFileName(e.FileName), DEVCPP]);
			end else if devExecutor.Running then begin
				Caption :=  Format('%s - [Executing] - %s %s',[e.FileName, DEVCPP, DEVCPP_VERSION]);
				Application.Title := Format('%s - [Executing] - %s', [ExtractFileName(e.FileName), DEVCPP]);
			end else if fCompiler.Compiling then begin
				Caption :=  Format('%s - [Compiling] - %s %s',[e.FileName, DEVCPP, DEVCPP_VERSION]);
				Application.Title := Format('%s - [Compiling] - %s', [ExtractFileName(e.FileName), DEVCPP]);
			end else begin
				Caption :=  Format('%s - %s %s',[e.FileName, DEVCPP, DEVCPP_VERSION]);
				Application.Title := Format('%s - %s', [ExtractFileName(e.FileName), DEVCPP]);
			end;
		end else begin
			Caption := Format('%s %s',[DEVCPP, DEVCPP_VERSION]);
			Application.Title := Format('%s', [DEVCPP]);
		end;
	end;
end;

procedure TMainForm.PackageManagerItemClick(Sender: TObject);
var
	s : AnsiString;
begin
	s := IncludeTrailingBackslash(devDirs.Exec) + PACKMAN_PROGRAM;
	ExecuteFile(s, '', IncludeTrailingBackslash(devDirs.Exec), SW_SHOW)
end;

procedure TMainForm.actAbortCompilationUpdate(Sender: TObject);
begin
	TAction(Sender).Enabled:=fCompiler.Compiling;
end;

procedure TMainForm.actAbortCompilationExecute(Sender: TObject);
begin
	fCompiler.AbortThread;
end;

{ begin XXXKF }

procedure TMainForm.actWindowMenuExecute(Sender: TObject);
var
	Item : TMenuItem;
	E : TEditor;
	i : integer;
	Act : TAction;
begin
	while WindowMenu.Count>8 do begin
		Item:=WindowMenu.Items[7];
		WindowMenu.Remove(Item);
		Item.Destroy;
	end;
	for i:= 0 to pred(PageControl.PageCount) do begin
		if (i>=9) then break;
		e:= GetEditor(i);
		Item:=TMenuItem.Create(self);
		Act:=TAction.Create(Item);
		//Act.ActionList:=ActionList;
		Act.Name:='dynactOpenEditorByTag';
		Act.Tag:=i;
		Act.OnExecute:=dynactOpenEditorByTagExecute;
		Item.Action:=Act;
		if e.FileName='' then
			Item.Caption:='&'+chr(49+i)+' Unnamed'
		else
			Item.Caption:='&'+chr(49+i)+' '+e.FileName;
		if e.Text.Modified then
			Item.Caption:=Item.Caption+' *';
		WindowMenu.Insert(WindowMenu.Count - 1,Item);
	end;
end;

procedure TMainForm.RemoveItem(Node: TTreeNode);
begin
	if Assigned(Node) and (Node.Level >= 1) then begin
		if Node.Data=Pointer(-1) then
			actProjectRemoveFolderExecute(nil)
		else
			fProject.Remove(integer(Node.Data), true);
	end;
end;

procedure TMainForm.ProjectViewKeyDown(Sender: TObject; var Key: Word;
	Shift: TShiftState);
begin
	if (Key=VK_DELETE) and Assigned(ProjectView.Selected) then
		RemoveItem(ProjectView.Selected)
	else if (Key=VK_ESCAPE) and (Shift=[]) then begin
		if PageControl.Visible And PageControl.Enabled And (PageControl.ActivePage<>nil) then
			 GetEditor(-1).Activate;
	end else if (Key=VK_ESCAPE) and (Shift=[ssShift]) then begin
		actProjectManager.Checked:=False;
		actProjectManagerExecute(nil);
		if PageControl.Visible And PageControl.Enabled And (PageControl.ActivePage<>nil) then
			 GetEditor(-1).Activate;
	end else if (Key=VK_RETURN) and Assigned(ProjectView.Selected) then begin
		if ProjectView.Selected.Data<>Pointer(-1) then begin{ if not a directory }
			OpenUnit;
			if Shift=[ssShift] then begin
				{
					crap hack, SHIFT+ENTER=open file and close projman
					I *really* don't think it's the acceptable interface idea.
					Can't find a better one though.
				}
				actProjectManager.Checked:=False;
				actProjectManagerExecute(nil);
			end;
		end;
	end;
end;

procedure TMainForm.ProjectViewChanging(Sender: TObject; Node: TTreeNode;var AllowChange: Boolean);
begin
	Node.MakeVisible;
end;

procedure TMainForm.dynactOpenEditorByTagExecute(Sender: TObject);
var
	e:TEditor;
begin
	e:=GetEditor(TAction(Sender).Tag);
	if Assigned(e) then
		e.Activate;
end;

{ end XXXKF }

procedure TMainForm.actGotoProjectManagerExecute(Sender: TObject);
begin
	if not actProjectManager.Checked then
	begin
		actProjectManager.Checked:=True;
		actProjectManagerExecute(nil);
	end;
	LeftPageControl.ActivePageIndex := 0;
	ProjectView.SetFocus;
end;

procedure TMainForm.actCVSImportExecute(Sender: TObject);
begin
	DoCVSAction(Sender, caImport);
end;

procedure TMainForm.actCVSCheckoutExecute(Sender: TObject);
begin
	DoCVSAction(Sender, caCheckout);
end;

procedure TMainForm.actCVSUpdateExecute(Sender: TObject);
var
	wa: boolean;
	I: integer;
	e: TEditor;
	pt: TBufferCoord;
begin
	actSaveAll.Execute;
	wa:=devFileMonitor.Active;
	devFileMonitor.Deactivate;

	DoCVSAction(Sender, caUpdate);

	// Refresh CVS Changes
	for I:=0 to PageControl.PageCount-1 do begin
		e:=GetEditor(I);
		if Assigned(e) and FileExists(e.FileName) then begin
			pt:=e.Text.CaretXY;
			e.Text.Lines.LoadFromFile(e.FileName);
			e.Text.CaretXY:=pt;
		end;
	end;

	if wa then
		devFileMonitor.Activate;
end;

procedure TMainForm.actCVSCommitExecute(Sender: TObject);
begin
	DoCVSAction(Sender, caCommit);
end;

procedure TMainForm.actCVSDiffExecute(Sender: TObject);
begin
	DoCVSAction(Sender, caDiff);
end;

procedure TMainForm.actCVSLogExecute(Sender: TObject);
begin
	DoCVSAction(Sender, caLog);
end;

procedure TMainForm.actCVSAddExecute(Sender: TObject);
begin
	DoCVSAction(Sender, caAdd);
end;

procedure TMainForm.actCVSRemoveExecute(Sender: TObject);
begin
	DoCVSAction(Sender, caRemove);
end;

procedure TMainForm.actCVSLoginExecute(Sender: TObject);
begin
	DoCVSAction(Sender, caLogin);
end;

procedure TMainForm.actCVSLogoutExecute(Sender: TObject);
begin
	DoCVSAction(Sender, caLogout);
end;

procedure TMainForm.DoCVSAction(Sender: TObject; whichAction: TCVSAction);
var
	e: TEditor;
	idx: integer;
	all: AnsiString;
	S: AnsiString;
begin
	S:='';
	if not FileExists(devCVSHandler.Executable) then begin
		MessageDlg(Lang[ID_MSG_CVSNOTCONFIGED], mtWarning, [mbOk], 0);
		Exit;
	end;
	e:=GetEditor;
	if TAction(Sender).ActionComponent.Tag = 1 then // project popup
		S:=IncludeQuoteIfSpaces(fProject.FileName)
	else if TAction(Sender).ActionComponent.Tag = 2 then begin // unit popup
		S:='';
		if ProjectView.Selected.Data=Pointer(-1) then begin // folder
			for idx:=0 to fProject.Units.Count-1 do
				if StartsStr(fProject.GetFolderPath(ProjectView.Selected), fProject.Units[idx].Folder) then
					S:=S+IncludeQuoteIfSpaces(fProject.Units[idx].FileName)+',';
			if S='' then
				S:='ERR';
		end else
			S:=IncludeQuoteIfSpaces(fProject.Units[Integer(ProjectView.Selected.Data)].FileName);
	end else if TAction(Sender).ActionComponent.Tag = 3 then // editor popup or [CVS menu - current file]
		S:=IncludeQuoteIfSpaces(e.FileName)
	else if TAction(Sender).ActionComponent.Tag = 4 then begin // CVS menu - whole project
		if Assigned(fProject) then
			S:=ExtractFilePath(fProject.FileName)
		else begin
			if Assigned(e) then
				S:=ExtractFilePath(e.FileName)
			else
				S:=ExtractFilePath(devDirs.Default);
		end;
		S:=IncludeQuoteIfSpaces(S);
	end;

	if not Assigned(fProject) then
		all:=IncludeQuoteIfSpaces(S)
	else begin
		all:=IncludeQuoteIfSpaces(fProject.FileName)+',';
		for idx:=0 to fProject.Units.Count-1 do
			all:=all+IncludeQuoteIfSpaces(fProject.Units[idx].FileName)+',';
		Delete(all, Length(all), 1);
	end;

	with TCVSForm.Create(Self) do begin
		CVSAction:=whichAction;
		Files.CommaText:=S;
		AllFiles.CommaText:=all;
		ShowModal;
	end;
end;

procedure TMainForm.ListItemClick(Sender: TObject);
var
	w : TWindowListForm;
	i : integer;
begin
	w := TWindowListForm.Create(self);
	try
		for i := 0 to PageControl.PageCount - 1 do
			w.UnitList.Items.Add(PageControl.Pages[i].Caption);
		if (w.ShowModal = mrOk) and (w.UnitList.ItemIndex > -1) then begin
			PageControl.ActivePageIndex := w.UnitList.ItemIndex;
		end;
	finally
		w.Free;
	end;
end;
{ begin XXXKF }
procedure TMainForm.ProjectViewCompare(Sender: TObject; Node1,
	Node2: TTreeNode; Data: Integer; var Compare: Integer);
begin
	if (Node1.Data=pointer(-1)) and (Node2.Data=pointer(-1)) then
		Compare:=CompareStr(Node1.Text,Node2.Text)
	else
	if Node1.Data=pointer(-1) then Compare:=-1
	else
	if Node2.Data=pointer(-1) then Compare:=+1
	else
		Compare:=CompareStr(Node1.Text,Node2.Text);
end;
{ end XXXKF }

procedure TMainForm.ProjectViewKeyPress(Sender: TObject; var Key: Char);
begin
	// fixs an annoying bug/behavior of the tree ctrl (a beep on enter key)
	if Key = #13 then
		Key := #0;
end;

procedure TMainForm.ProjectViewMouseDown(Sender: TObject;
	Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	// bug-fix: when *not* clicking on an item, re-opens the last clicked file-node
	// this was introduced in the latest commit by XXXKF (?)
	if (Button = mbLeft) and not (htOnItem in ProjectView.GetHitTestInfoAt(X, Y)) then
		ProjectView.Selected:=nil;
end;

procedure TMainForm.GoToClassBrowserItemClick(Sender: TObject);
begin
	if not actProjectManager.Checked then
	begin
		actProjectManager.Checked:=True;
		actProjectManagerExecute(nil);
	end;
	LeftPageControl.ActivePageIndex := 1;
	if ClassBrowser.Visible And ClassBrowser.Enabled then
		ClassBrowser.SetFocus;
end;

procedure TMainForm.actBrowserShowInheritedExecute(Sender: TObject);
begin
	actBrowserShowInherited.Checked:=not actBrowserShowInherited.Checked;
	devClassBrowsing.ShowInheritedMembers:=actBrowserShowInherited.Checked;
	ClassBrowser.ShowInheritedMembers:=actBrowserShowInherited.Checked;
	ClassBrowser.Refresh;
end;

procedure TMainForm.ProjectWindowClose(Sender: TObject; var Action: TCloseAction);
begin
	FloatingPojectManagerItem.Checked := False;
	LeftPageControl.Visible := false;
	(Sender as TForm).RemoveControl(LeftPageControl);

	LeftPageControl.Left := 0;
	LeftPageControl.Top := Toolbar.Height;
	LeftPageControl.Align := alLeft;
	LeftPageControl.Visible := true;
	InsertControl(LeftPageControl);
	ProjectToolWindow.Free;
	ProjectToolWindow := nil;

	if assigned(fProject) then
		fProject.SetNodeValue(ProjectView.TopItem); // nodes needs to be recreated
end;

procedure TMainForm.ReportWindowClose(Sender: TObject; var Action: TCloseAction);
begin
	FloatingReportWindowItem.Checked := False;
	MessageControl.Visible := false;
	(Sender as TForm).RemoveControl(MessageControl);

	MessageControl.Left := 0;
	MessageControl.Top := SplitterBottom.Top;
	MessageControl.Align := alBottom;
	MessageControl.Visible := true;
	InsertControl(MessageControl);
	Statusbar.Top := MessageControl.Top + MessageControl.Height;
	ReportToolWindow.Free;
	ReportToolWindow := nil;
end;

procedure TMainForm.FloatingPojectManagerItemClick(Sender: TObject);
begin
	FloatingPojectManagerItem.Checked := not FloatingPojectManagerItem.Checked;
	if assigned(ProjectToolWindow) then
		ProjectToolWindow.Close
	else begin
		ProjectToolWindow := TForm.Create(self);
		with ProjectToolWindow do begin
			Caption := Lang[ID_TB_PROJECT];
			Top := self.Top + LeftPageControl.Top;
			Left := self.Left + LeftPageControl.Left;
			Height := LeftPageControl.Height;
			Width := LeftPageControl.Width;
			FormStyle := fsStayOnTop;
			OnClose := ProjectWindowClose;
			BorderStyle := bsSizeable;
			BorderIcons := [biSystemMenu];
			LeftPageControl.Visible := false;
			self.RemoveControl(LeftPageControl);

			LeftPageControl.Left := 0;
			LeftPageControl.Top := 0;
			LeftPageControl.Align := alClient;
			LeftPageControl.Visible := true;
			ProjectToolWindow.InsertControl(LeftPageControl);

			ProjectToolWindow.Show;
			if assigned(fProject) then
				fProject.SetNodeValue(ProjectView.TopItem); // nodes needs to be recreated
		end;
	end;
end;

function TMainForm.CheckCompileOption(const option : AnsiString;var optionstructout : PCompilerOption;var optionindexout : integer) : boolean;
begin
	Result := false;

	// Try to find the value in the predefined list
	if devCompiler.FindOption(option, optionstructout, optionindexout) then begin // the option exists...

		// Search project options...
		if Assigned(fProject) then begin
			if fProject.Options.CompilerOptions[optionindexout + 1] <> '0' then
				Result := true;
		end else begin
			Result := optionstructout^.optValue > 0;
		end;
	end;
end;

procedure TMainForm.SetProjCompOpt(idx: integer; Value: char);
var
	OptionsCopy : TProjOptions;
begin
	if Assigned(fProject) then begin

		// todo: obtain pointer instead of copy
		OptionsCopy := fProject.Options;

		if OptionsCopy.CompilerOptions[idx+1] <> Value then begin
			OptionsCopy.CompilerOptions[idx+1] := Value;
			fProject.Modified := true;
			fProject.Options := OptionsCopy;
		end;
	end;
end;

procedure TMainForm.SetupProjectView;
begin
	if devData.DblFiles then begin
		ProjectView.HotTrack:=False;
		ProjectView.MultiSelect:=True;
	end else begin
		ProjectView.HotTrack:=True;
		ProjectView.MultiSelect:=False;
	end;
end;

procedure TMainForm.actCompileCurrentFileExecute(Sender: TObject);
var
	e: TEditor;
begin
	if fCompiler.Compiling then begin
		MessageDlg(Lang[ID_MSG_ALREADYCOMP], mtInformation, [mbOK], 0);
		Exit;
	end;

	e:=GetEditor;
	if not Assigned(e) then
		Exit;
	if not PrepareForCompile then
		Exit;
	fCompiler.Compile(e.FileName);
end;

procedure TMainForm.actCompileCurrentFileUpdate(Sender: TObject);
begin
	TCustomAction(Sender).Enabled:= (assigned(fProject) and (PageControl.PageCount > 0)) and not devExecutor.Running and not fDebugger.Executing and not fCompiler.Compiling;
end;

procedure TMainForm.actSaveProjectAsExecute(Sender: TObject);
begin
	if not Assigned(fProject) then
		Exit;
	with dmMain.SaveDialog do begin
		Filter:= FLT_PROJECTS;
		if Execute then begin
			fProject.FileName:=FileName;
			fProject.Save;
			UpdateAppTitle;
		end;
	end;
end;

procedure TMainForm.BuildOpenWith;
var
 idx, idx2: integer;
 item: TMenuItem;
 ext, s, s1: AnsiString;
begin
	mnuOpenWith.Clear;
	if not assigned(fProject) then exit;
	if not assigned(ProjectView.Selected) or
		 (ProjectView.Selected.Level	< 1) then exit;
	if ProjectView.Selected.Data=Pointer(-1) then
		Exit;
	idx:= integer(ProjectView.Selected.Data);

	ext:=ExtractFileExt(fProject.Units[idx].FileName);
	idx2:=devExternalPrograms.AssignedProgram(ext);
	if idx2<>-1 then begin
		item:=TMenuItem.Create(nil);
		item.Caption:=ExtractFilename(devExternalPrograms.ProgramName[idx2]);
		item.Tag:=idx2;
		item.OnClick:=mnuOpenWithClick;
		mnuOpenWith.Add(item);
	end;
	if GetAssociatedProgram(ext, s, s1) then begin
		item:=TMenuItem.Create(nil);
		item.Caption:='-';
		item.Tag:=-1;
		item.OnClick:=mnuOpenWithClick;
		mnuOpenWith.Add(item);
		item:=TMenuItem.Create(nil);
		item.Caption:=s1;
		item.Tag:=-1;
		item.OnClick:=mnuOpenWithClick;
		mnuOpenWith.Add(item);
	end;
end;

procedure TMainForm.mnuOpenWithClick(Sender: TObject);
var
	idx, idx2, idx3: integer;
	item: TMenuItem;
begin
	if (Sender=mnuOpenWith) and (mnuOpenWith.Count>0) then
		Exit;
	if not Assigned(fProject) then
		Exit;
	if not assigned(ProjectView.Selected) or
		 (ProjectView.Selected.Level	< 1) then exit;
	if ProjectView.Selected.Data=Pointer(-1) then
		Exit;
	idx2:= integer(ProjectView.Selected.Data);

	item:=TMenuItem(Sender);
	if item = mnuOpenWith then begin
		idx:=-2;
		with dmMain.OpenDialog do begin
			Filter:=FLT_ALLFILES;
			if Execute then
				idx:=devExternalPrograms.AddProgram(ExtractFileExt(fProject.Units[idx2].FileName), Filename);
		end;
	end
	else
		idx:=item.Tag;
	idx3:= FileIsOpen(fProject.Units[idx2].FileName, TRUE);
	if idx3 > -1 then
		CloseEditor(idx3);

	if idx>-1 then // devcpp-based
		ShellExecute(0, 'open',
								PAnsiChar(devExternalPrograms.ProgramName[idx]),
								PAnsiChar(fProject.Units[idx2].FileName),
								PAnsiChar(ExtractFilePath(fProject.Units[idx2].FileName)),
								SW_SHOW)
	// idx=-2 means we prompted the user for a program, but didn't select one
	else if idx<>-2 then // registry-based
		ShellExecute(0, 'open',
								PAnsiChar(fProject.Units[idx2].FileName),
								nil,
								PAnsiChar(ExtractFilePath(fProject.Units[idx2].FileName)),
								SW_SHOW);
end;

procedure TMainForm.RebuildClassesToolbar;
var
	I: integer;
	st: PStatement;
	OldSelection: AnsiString;
begin
	OldSelection:=cmbClasses.Text;

	cmbClasses.Items.BeginUpdate;
	cmbClasses.Clear;
	cmbClasses.Items.Add('(globals)');

	for I:=0 to CppParser.Statements.Count-1 do begin
		st:=PStatement(CppParser.Statements[I]);
		if st^._InProject and (st^._Kind = skClass) then // skClass equals struct + typedef + class
			cmbClasses.Items.AddObject(st^._ScopelessCmd, Pointer(st^._ID));
	end;

	cmbClasses.Items.EndUpdate;

	// if we can't finx the old selection anymore (-> -1), default to 0 (globals)
	cmbClasses.ItemIndex := max(0,cmbClasses.Items.IndexOf(OldSelection));
	cmbClassesChange(cmbClasses);
end;

procedure TMainForm.cmbClassesChange(Sender: TObject);
var
	I, ParentID: integer;
	st: PStatement;
begin
	cmbMembers.Items.BeginUpdate;
	cmbMembers.Clear;

	// Support '(globals)'
	if cmbClasses.ItemIndex > 0 then
		ParentID := Integer(cmbClasses.Items.Objects[cmbClasses.ItemIndex])
	else if cmbClasses.ItemIndex = 0 then// '(globals)'
		ParentID := -1
	else begin // -1?
		cmbMembers.Items.EndUpdate;
		Exit;
	end;

	// If we selected a class
	if(ParentID <> -1) and (PStatement(CppParser.Statements[CppParser.IndexOfStatement(ParentID)])^._Type = 'class ') then begin

		// Don't allow variables of classes
		for I:=0 to CppParser.Statements.Count-1 do begin
			st:=PStatement(CppParser.Statements[I]);
			if (st^._ParentID = ParentID) and st^._InProject and (st^._Kind in [skConstructor, skDestructor, skFunction]) then
				cmbMembers.Items.AddObject(st^._ScopelessCmd + st^._Args + ' : ' + st^._Type, Pointer(I));
		end;
	end else begin

		// Allow variables of (globals) and structs
		for I:=0 to CppParser.Statements.Count-1 do begin
			st:=PStatement(CppParser.Statements[I]);
			if (st^._ParentID = ParentID) and st^._InProject and (st^._Type <> '') then // Skip defines too
				cmbMembers.Items.AddObject(st^._ScopelessCmd + st^._Args + ' : ' + st^._Type, Pointer(I));
		end;
	end;

	cmbMembers.Items.EndUpdate;
end;

procedure TMainForm.cmbMembersChange(Sender: TObject);
var
	I: integer;
	st: PStatement;
	e: TEditor;
	fname: AnsiString;
begin
	I:=Integer(cmbMembers.Items.Objects[cmbMembers.ItemIndex]);
	st:=PStatement(CppParser.Statements[I]);
	if not Assigned(st) then
		Exit;

	if st^._IsDeclaration then begin
		I:=st^._DeclImplLine;
		fname:=st^._DeclImplFileName;
	end else begin
		I:=st^._Line;
		fname:=st^._FileName;
	end;

	e := GetEditorFromFilename(fname);
	if Assigned(e) then
		e.GotoLineNr(I);
end;

procedure TMainForm.CompilerOutputKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
begin
{$IFDEF WIN32}
	if Key = VK_RETURN then
{$ENDIF}
{$IFDEF LINUX}
	if Key = XK_RETURN then
{$ENDIF}
		CompilerOutputDblClick(sender);
end;

procedure TMainForm.FindOutputKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
begin
{$IFDEF WIN32}
	if Key = VK_RETURN then
{$ENDIF}
{$IFDEF LINUX}
	if Key = XK_RETURN then
{$ENDIF}
		FindOutputDblClick(sender);
end;

procedure TMainForm.DebugTreeKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
begin
{$IFDEF WIN32}
	if Key = VK_DELETE then
{$ENDIF}
{$IFDEF LINUX}
	if Key = XK_DELETE then
{$ENDIF}
		actRemoveWatchExecute(sender);
end;

procedure TMainForm.DebugVarsPopupPopup(Sender: TObject);
begin
	RemoveWatchPop.Enabled := Assigned(DebugTree.Selected);
end;

procedure TMainForm.FloatingReportwindowItemClick(Sender: TObject);
begin
	FloatingReportWindowItem.Checked := not FloatingReportWindowItem.Checked;
	if assigned(ReportToolWindow) then
		ReportToolWindow.Close
	else begin
		OpenCloseMessageSheet(true);
		if MessageControl.ActivePage = CloseSheet then
			MessageControl.ActivePageIndex := 0;
		ReportToolWindow := TForm.Create(self);
		with ReportToolWindow do begin
			Caption := Lang[ID_TB_REPORT];
			Top := self.Top + MessageControl.Top;
			Left := self.Left + MessageControl.Left;
			Height := MessageControl.Height;
			Width := MessageControl.Width;
			FormStyle := fsStayOnTop;
			OnClose := ReportWindowClose;
			BorderStyle := bsSizeable;
			BorderIcons := [biSystemMenu];
			MessageControl.Visible := false;
			self.RemoveControl(MessageControl);

			MessageControl.Left := 0;
			MessageControl.Top := 0;
			MessageControl.Align := alClient;
			MessageControl.Visible := true;
			ReportToolWindow.InsertControl(MessageControl);

			ReportToolWindow.Show;
		end;
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
	s : AnsiString;
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
				fDebugger.SendCommand('file', '"' +StringReplace(fProject.Executable, '\', '\\', [rfReplaceAll]) +'"');
				fDebugger.SendCommand('attach', s);
			end
		finally
			ProcessListForm.Free;
		end;
	end;
end;

procedure TMainForm.actModifyWatchExecute(Sender: TObject);
var
	curnode : TTreeNode;
	fullname: AnsiString;
	value : AnsiString;

	function GetNodeName(node : TTreeNode) : AnsiString;
	var
		epos : integer;
	begin
		Result := '';
		epos := Pos(' = ',node.Text);
		if epos > 0 then
			Result := Copy(node.Text,1,epos - 1);
	end;

	function GetNodeValue(node : TTreeNode) : AnsiString;
	var
		epos : integer;
	begin
		Result := '';
		epos := Pos(' = ',node.Text);
		if epos > 0 then
			Result := Copy(node.Text,epos + 3,Length(node.Text) - epos);
	end;

begin
	curnode := DebugTree.Selected;
	if Assigned(curnode) then begin // only edit members

		fullname := GetNodeName(curnode);

		// Assemble full name including parents
		while Assigned(curnode.Parent) do begin
			curnode := curnode.Parent;
			if not StartsStr('{',GetNodeName(curnode)) then
				fullname := GetNodeName(curnode) + '.' + fullname;
		end;

		value := GetNodeValue(DebugTree.Selected);

		if InputQuery(Lang[ID_NV_MODIFYVALUE],fullname,value) then
			fDebugger.SendCommand('set variable',fullname + ' = ' + value, true);
	end;
end;

procedure TMainForm.actModifyWatchUpdate(Sender: TObject);
begin
	TCustomAction(Sender).Enabled := Assigned(DebugTree.Selected) and fDebugger.Executing;
end;

procedure TMainForm.ClearallWatchPopClick(Sender: TObject);
begin
	fDebugger.DeleteWatchVars(true);
end;

procedure TMainForm.mnuCVSClick(Sender: TObject);
begin
	mnuCVSCurrent.Enabled := PageControl.PageCount > 0;
end;

procedure TMainForm.actDeleteProfileProjectExecute(Sender: TObject);
var
	path : AnsiString;
	e : TEditor;
begin
	path := '';
	if Assigned(fProject) then begin
		path := ExtractFilePath(fProject.Executable)+GPROF_CHECKFILE;
	end else begin
		e:=GetEditor;
		if Assigned(e) then
			path := ExtractFilePath(ChangeFileExt(e.FileName, EXE_EXT))+GPROF_CHECKFILE;
	end;

	if path <> '' then begin
		if DeleteFile(PAnsiChar(path)) then begin
			SetStatusbarMessage('Deleted profiling data file "' + path + '"');
		end else
			SetStatusbarMessage('Could not find profiling file "' + path + '"!');
	end;
end;

procedure TMainForm.actGotoImplDeclEditorExecute(Sender: TObject);
var
	statement : PStatement;
	filename,phrase : AnsiString;
	line : integer;
	M : TMemoryStream;
	e : TEditor;
begin
	e := GetEditor;

	if Assigned(e) then begin

		// Exit early, don't bother creating a stream (which is slow)
		phrase := e.CompletionPhrase(e.Text.CaretXY);
		if Phrase = '' then Exit;

		// When searching using menu shortcuts, the caret is set to the proper place
		// When searching using ctrl+click, the cursor is set properly too, so do NOT use WordAtMouse
		M:=TMemoryStream.Create;
		try
			e.Text.UnCollapsedLines.SaveToStream(M);
			statement := CppParser.FindStatementOf(
				e.FileName,
				phrase,e.Text.CaretY,M);
		finally
			M.Free;
		end;

		// Otherwise scan the returned class browser statement
		if Assigned(statement) then begin

			// If the caret position was used
			if Sender = e then begin // Ctrl+Click from current editor

				// Switching between declaration and definition
				if e.Text.CaretY <> statement^._Line then begin
					filename:=statement^._FileName;
					line:=statement^._Line;
				end else begin
					filename:=statement^._DeclImplFileName;
					line:=statement^._DeclImplLine;
				end;

			// Menu item or mouse cursor
			end else begin

				// Switching between declaration and definition
				if Pos('Decl',TCustomAction(Sender).Name) > 0 then begin
					filename:=statement^._FileName;
					line:=statement^._Line;
				end else begin
					filename:=statement^._DeclImplFileName;
					line:=statement^._DeclImplLine;
				end;
			end;

			// Go to the location
			e := GetEditorFromFileName(filename);
			if Assigned(e) then
				e.GotoLineNr(line);

			SetStatusbarMessage('');
		end else
			SetStatusbarMessage('Could not find declaration...');
	end;
end;

procedure TMainForm.actHideFSBarExecute(Sender: TObject);
begin
	if devData.FullScreen then
		if pnlFull.Height <> 0 then
			pnlFull.Height := 0
		else
			pnlFull.Height := 16;
end;

procedure TMainForm.FormMouseWheel(Sender: TObject; Shift: TShiftState;WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
	e : TEditor;
	State : TKeyboardState;
	I : integer;
begin

	// Check if control is pressed
	GetKeyboardState(State);
	e := GetEditor;

	// Check if we're focussing on an editor and holding ctrl
	if Assigned(e) and (Screen.ActiveControl = e.Text) and ((State[vk_Control] and 128) <> 0) then begin

		for I:=0 to pred(PageControl.PageCount) do begin
			e := GetEditor(I);

			// If so, set the font size of all editors...
			if WheelDelta > 0 then begin
				e.Text.Font.Size := Max(1,e.Text.Font.Size + 1);
				e.Text.Gutter.Font.Size := Max(1,e.Text.Gutter.Font.Size + 1);
			end else begin
				e.Text.Font.Size := Max(1,e.Text.Font.Size - 1);
				e.Text.Gutter.Font.Size := Max(1,e.Text.Gutter.Font.Size - 1);
			end;
		end;

		// And set the corresponding option
		if WheelDelta > 0 then begin;
			devEditor.Font.Size := Max(1,devEditor.Font.Size + 1);
			devEditor.Gutterfont.Size := Max(1,devEditor.Gutterfont.Size + 1);
		end else if WheelDelta < 0 then begin
			devEditor.Font.Size := Max(1,devEditor.Font.Size - 1);
			devEditor.Gutterfont.Size := Max(1,devEditor.Gutterfont.Size - 1);
		end;

		// We don't like to send the actual scrolling message to the editor when zooming
		Abort;
	end;
end;

procedure TMainForm.ImportCBCprojectClick(Sender: TObject);
begin
	with TImportCBForm.Create(Self) do begin
		if ShowModal=mrOK then
			OpenProject(GetFilename);
	end;
end;

procedure TMainForm.CompilerOutputAdvancedCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;Stage: TCustomDrawStage; var DefaultDraw: Boolean);
begin
	// Color...
	if ContainsStr(Item.SubItems[2],'[Warning]') then
		Sender.Canvas.Font.Color := TColor($0066FF); // Orange
	if ContainsStr(Item.SubItems[2],'[Error]') then
		Sender.Canvas.Font.Color := clRed;
	if ContainsStr(Item.SubItems[2],'[Hint]') then
		Sender.Canvas.Font.Color := clYellow;

	// In function/member/etc
	if StartsStr('in ',LowerCase(Item.SubItems[2])) or StartsStr('at ',LowerCase(Item.SubItems[2])) then
		Sender.Canvas.Font.Style := [fsBold];
end;

procedure TMainForm.cmbMembersDropDown(Sender: TObject);
var
	widestwidth, I : integer;
begin
	// Set to default width first...
	SendMessage(cmbMembers.Handle, CB_SETDROPPEDWIDTH, 0, 0);

	widestwidth := 0;

	// Fix, cmbMembers.Canvas.Font was set to Tahoma for some reason?
	cmbMembers.Canvas.Font.Name := 'Courier New';

	// get the max needed with of the items in dropdown state
	for I := 0 to cmbMembers.Items.Count-1 do
		widestwidth := Max(widestwidth,cmbMembers.Canvas.TextWidth(cmbMembers.Items[I]) + 8); // padding

	if(widestwidth > cmbMembers.Width) then begin

		// Add scrollbar width
		if cmbMembers.DropDownCount < cmbMembers.Items.Count then
			widestwidth := widestwidth + GetSystemMetrics(SM_CXVSCROLL);

		SendMessage(cmbMembers.Handle, CB_SETDROPPEDWIDTH, widestwidth, 0);
	end;
end;

procedure TMainForm.NewFileBtnClick(Sender: TObject);
var
	pt: TPoint;
begin
	pt:= tbMain.ClientToScreen(point(NewFileBtn.Left, NewFileBtn.Top + NewFileBtn.Height));
	TrackPopupMenu(mnuNew.Handle, TPM_LEFTALIGN or TPM_LEFTBUTTON,pt.X, pt.y, 0, Self.Handle, nil);
end;

procedure TMainForm.actUnCollapseExecute(Sender: TObject);
var
	e : TEditor;
begin
	e:=GetEditor;
	if Assigned(e) then
		e.Text.UncollapseAll;
end;

procedure TMainForm.actCollapseExecute(Sender: TObject);
var
	e : TEditor;
begin
	e:=GetEditor;
	if Assigned(e) then
		e.Text.CollapseAll;
end;

procedure TMainForm.actInsertExecute(Sender: TObject);
var
	pt: TPoint;
begin
	pt:= tbSpecials.ClientToScreen(point(Insertbtn.Left, Insertbtn.Top +Insertbtn.Height));
	TrackPopupMenu(InsertItem.Handle, TPM_LEFTALIGN or TPM_LEFTBUTTON, pt.X, pt.Y, 0, Self.Handle, nil);
end;

procedure TMainForm.actToggleExecute(Sender: TObject);
var
	pt: TPoint;
begin
	pt:= tbSpecials.ClientToScreen(point(Togglebtn.Left, Togglebtn.Top +togglebtn.Height));
	TrackPopupMenu(ToggleBookmarksItem.Handle, TPM_LEFTALIGN or TPM_LEFTBUTTON,pt.x, pt.y, 0, Self.Handle, nil);
end;

procedure TMainForm.actGotoExecute(Sender: TObject);
var
	pt: TPoint;
begin
	pt:= tbSpecials.ClientToScreen(point(Gotobtn.Left, Gotobtn.Top +Gotobtn.Height));
	TrackPopupMenu(GotoBookmarksItem.Handle, TPM_LEFTALIGN or TPM_LEFTBUTTON, pt.x, pt.y, 0, Self.Handle, nil);
end;

procedure TMainForm.actEditMenuUpdate(Sender: TObject);
begin
	InsertItem.Enabled := PageControl.PageCount > 0;
	ToggleBookmarksItem.Enabled := PageControl.PageCount > 0;
	GotoBookmarksItem.Enabled := PageControl.PageCount > 0;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
	fFirstShow := true;
	fFirstActivate := true;

	// Create all data structures
	UpdateSplash('Applying settings...');

	// Fix Alt key painting problems
	//TVistaAltFix.Create(Self); // causes too much flicker :(

	DDETopic:=DevCppDDEServer.Name;
	CheckAssociations; // register file associations and DDE services <-- !!!
	DragAcceptFiles(Self.Handle, TRUE);
	dmMain:= TdmMain.Create(Self);

	// Set paths
	devDirs.OriginalPath := GetEnvironmentVariable('PATH');

	// Set left panel to previous state
	actProjectManager.Checked:= devData.ProjectView;
	if devData.ClassView then
		LeftPageControl.ActivePage:=ClassSheet
	else
		LeftPageControl.ActivePage:=ProjectSheet;
	actProjectManagerExecute(nil);
	LeftPageControl.Width:=devData.ProjectWidth;

	// Set statusbar to previous state
	actStatusbar.Checked:= devData.Statusbar;
	actStatusbarExecute(nil);

	// Add compiler events
	fCompiler:= TCompiler.Create;
	fCompiler.OnLogEntry := LogEntryProc;
	fCompiler.OnOutput := CompOutputProc;
	fCompiler.OnResOutput := CompResOutputProc;
	fCompiler.OnSuccess:= CompSuccessProc;

	// Create a debugger
	fDebugger := TDebugger.Create;
	fDebugger.DebugTree := DebugTree;

	// Set bottom panel height
	MessageControl.Height:=devData.OutputHeight;
	fPreviousHeight:= MessageControl.Height;

	// Custom tools
	fTools:= TToolController.Create;
	with fTools do begin
		Menu:= ToolsMenu;
		Offset:= ToolsMenu.Indexof(PackageManagerItem);
		ToolClick:= ToolItemClick;
	//	BuildMenu; // Done by themes
	end;

	// Create custom theme loader
	devTheme := TdevTheme.Create;

	// Create icon themes
	devImageThemes := TDevImageThemeFactory.Create;
	devImageThemes.LoadFromDirectory(devDirs.Themes);

	// Set languages and other first time stuff
	if devData.First or (devData.Language = '') then begin
		Lang.SelectLanguage;
		if devData.ThemeChange then
			LoadTheme;
		devData.FileDate := FileAge(Application.ExeName);
		devData.First:= FALSE;
	end else begin
		Lang.Open(devData.Language);
	end;

	// Load bookmarks, captions and hints
	LoadText;

	// Apply shortcuts
	devShortcuts.Filename:=devDirs.Config + DEV_SHORTCUTS_FILE;
	devShortcuts.Load;

	// Some more options
	actCompOnNeed.Checked:=devData.OutputOnNeed;
	actCompOutput.Checked:=devData.ShowOutput;

	// Toolbar visibility
	ToolMainItem.checked:= devData.ToolbarMain;
	ToolEditItem.Checked:= devData.ToolbarEdit;
	ToolCompileandRunItem.Checked:= devData.ToolbarCompile;
	ToolProjectItem.Checked:= devData.ToolbarProject;
	ToolSpecialsItem.Checked:= devData.ToolbarSpecials;
	ToolSearchItem.Checked:= devData.ToolbarSearch;
	ToolClassesItem.Checked:= devData.ToolbarClasses;
	ToolbarClick(nil);

	// Toolbar positions
	tbMain.Left:= devData.ToolbarMainX;
	tbMain.Top:= devData.ToolbarMainY;
	tbEdit.Left:= devData.ToolbarEditX;
	tbEdit.Top:= devData.ToolbarEditY;
	tbCompile.Left:= devData.ToolbarCompileX;
	tbCompile.Top:= devData.ToolbarCompileY;
	tbProject.Left:= devData.ToolbarProjectX;
	tbProject.Top:= devData.ToolbarProjectY;
	tbSpecials.Left:= devData.ToolbarSpecialsX;
	tbSpecials.Top:= devData.ToolbarSpecialsY;
	tbSearch.Left:= devData.ToolbarSearchX;
	tbSearch.Top:= devData.ToolbarSearchY;
	tbClasses.Left:= devData.ToolbarClassesX;
	tbClasses.Top:= devData.ToolbarClassesY;

	// PageControl settings
	if devData.MsgTabs = 0 then
		PageControl.TabPosition:= tpTop
	else if devData.MsgTabs = 1 then
		PageControl.TabPosition:= tpBottom
	else if devData.MsgTabs = 2 then
		PageControl.TabPosition:= tpLeft
	else if devData.MsgTabs = 3 then
		PageControl.TabPosition:= tpRight;
	PageControl.MultiLine:= devData.MultiLineTab;

	// Create an autosave timer
	AutoSaveTimer := TTimer.Create(Application);
	AutoSaveTimer.Interval := devEditor.Interval*60*1000; // ms to minutes of course
	AutoSaveTimer.OnTimer := EditorSaveTimer;
	AutoSaveTimer.Enabled := devEditor.EnableAutoSave;

	// Create some more things?
	LoadTheme;
	dmMain.MRUMenu:= ReOpenItem;
	dmMain.MRUOffset:= 2;
	dmMain.MRUMax:= devData.MRUMax;
	dmMain.MRUClick:= MRUClick;
	dmMain.CodeMenu:= InsertItem;
	dmMain.CodePop:= InsertPopItem;
	dmMain.CodeClick:= CodeInsClick;
	dmMain.CodeOffset:= 2;
	dmMain.LoadDataMod;

	SetupProjectView;

	// Initialize class browser (takes much longer than all the other stuff above)
	UpdateSplash('Initializing class browser...');

	InitClassBrowser(true);
end;

procedure TMainForm.PageControlMouseMove(Sender: TObject;Shift: TShiftState; X, Y: Integer);
var
	thispage : integer;
	e : TEditor;
begin
	thispage := PageControl.IndexOfTabAt(X, Y);
	if thispage <> -1 then begin
		e := GetEditor(thispage);
		if Assigned(e) then
			PageControl.Hint := e.FileName;
	end else
		PageControl.Hint := '';
end;

procedure TMainForm.actBreakPointExecute(Sender: TObject);
var
	e : TEditor;
begin
	e :=  GetEditor;
	if Assigned(e) then
		e.ToggleBreakPoint(e.Text.CaretY);
end;

procedure TMainForm.OnInputEvalReady(const evalvalue : AnsiString);
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
				Key := #0;
				fDebugger.OnEvalReady := OnInputEvalReady;
				fDebugger.SendCommand('print',EvaluateInput.Text,true);

				// Tell the user we're updating...
				EvalOutput.Font.Color := clGrayText;

				// Add to history
				if EvaluateInput.Items.IndexOf(EvaluateInput.Text) = -1 then
					EvaluateInput.AddItem(EvaluateInput.Text,nil);
			end;
		end;
	end;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
	I : integer;
begin
	if fFirstShow then begin
		fFirstShow := false;

		if fRemoveOptions then Close;

		MessageControl.ActivePageIndex:= 0;
		OpenCloseMessageSheet(devData.ShowOutput);

		// Open files passed to us (HAS to be done at FormShow)
		i := 1;
		fShowTips := true;
		while I <= ParamCount do begin

				// Skip the config stuff
			if (ParamStr(i) = '-c') then // -c
				i := i + 2;

			// Open the files passed to Dev-C++
			if FileExists(ParamStr(i)) then begin
				if GetFileTyp(ParamStr(i)) = utPrj then begin
					OpenProject(ParamStr(i));
					break; // only open 1 project
				end else
					OpenFile(ParamStr(i));
				fShowTips := false;
			end;
			inc(i);
		end;

		UpdateAppTitle;
	end;
end;

procedure TMainForm.actSkipFunctionExecute(Sender: TObject);
begin
	if fDebugger.Executing then begin
		fDebugger.SendCommand('finish','', true);
	end;
end;

procedure TMainForm.actNextInsExecute(Sender: TObject);
begin
	if fDebugger.Executing then begin
		fDebugger.SendCommand('nexti','', true);
	end;
end;

procedure TMainForm.actStepInsExecute(Sender: TObject);
begin
	if fDebugger.Executing then begin
		fDebugger.SendCommand('stepi','', true);
	end;
end;

procedure TMainForm.DebugTreeAdvancedCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
var
	curnode : TTreeNode;
begin
	curnode := node;
	while Assigned(curnode.Parent) do
		curnode := curnode.Parent;

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

procedure TMainForm.FindOutputAdvancedCustomDrawSubItem(Sender: TCustomListView; Item: TListItem; SubItem: Integer;State: TCustomDrawState; Stage: TCustomDrawStage;var DefaultDraw: Boolean);
var
	boldstart,boldlen,i : integer;
	rect : TRect;
	oldbcolor,oldfcolor : TColor;

	procedure Draw(const s : AnsiString);
	var
		sizerect : TRect;
	begin
		DrawText(Sender.Canvas.Handle,PAnsiChar(s),Length(s),rect,DT_EXPANDTABS or DT_NOCLIP or DT_NOPREFIX);

		// Get text extent
		FillChar(sizerect,sizeof(sizerect),0);
		DrawText(Sender.Canvas.Handle,PAnsiChar(s),Length(s),sizerect,DT_CALCRECT or DT_EXPANDTABS or DT_NOCLIP or DT_NOPREFIX);
		Inc(rect.Left,sizerect.Right-sizerect.Left+1); // 1 extra pixel for extra width caused by bold
	end;
begin
	if not SameStr(Item.Caption,'') and (SubItem = 3) then begin

		// shut up compiler warning...
		oldbcolor := 0;
		oldfcolor := 0;

		boldstart := StrToInt(Item.SubItems[0]);
		boldlen := Integer(Item.Data);

		// Get rect of subitem
		rect := Item.DisplayRect(drBounds);

		for i := 0 to 2 do begin
			rect.Left := rect.Left + Sender.Column[i].Width;
			rect.Right := rect.Right + Sender.Column[i].Width;
		end;

		// Draw blue highlight
		if (cdsSelected in State) then begin
			oldbcolor := Sender.Canvas.Brush.Color;
			oldfcolor := Sender.Canvas.Font.Color;
			Sender.Canvas.Brush.Color := clHighlight;
			Sender.Canvas.Font.Color := clWhite;
			Sender.Canvas.FillRect(rect);
		end;

		// Make text appear 'native', like Windows would draw it
		OffsetRect(rect,1,1);

		// Draw part before bold highlight
		Draw(Copy(Item.SubItems[2],1,boldstart-1));

		// Enable bold
		Sender.Canvas.Font.Style := [fsBold];
		Sender.Canvas.Refresh;

		// Draw bold highlight
		Draw(Copy(Item.SubItems[2],boldstart,boldlen));

		// Disable bold
		Sender.Canvas.Font.Style := [];
		Sender.Canvas.Refresh;

		// Draw part after bold highlight
		Draw(Copy(Item.SubItems[2],boldstart+boldlen,Length(Item.SubItems[2])-boldstart-boldlen+1));

		if (cdsSelected in State) then begin
			Sender.Canvas.Brush.Color := oldbcolor;
			Sender.Canvas.Font.Color := oldfcolor;
		end;

		DefaultDraw := false;
	end;
end;

procedure TMainForm.FindOutputAdvancedCustomDraw(Sender: TCustomListView;const ARect: TRect; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
begin
	SendMessage(FindOutput.Handle,WM_CHANGEUISTATE, MAKEWPARAM(UIS_SET,UISF_HIDEFOCUS), 0);
end;

procedure TMainForm.CompilerOutputAdvancedCustomDraw(Sender: TCustomListView; const ARect: TRect; Stage: TCustomDrawStage;var DefaultDraw: Boolean);
begin
	SendMessage(CompilerOutput.Handle,WM_CHANGEUISTATE, MAKEWPARAM(UIS_SET,UISF_HIDEFOCUS), 0);
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
	if fFirstActivate then begin
		fFirstActivate := false;

		// do not show tips if Dev-C++ is launched with a file and only slow
		// when the form shows for the first time, not when going fullscreen too
		if devData.ShowTipsOnStart and fShowTips then
			actShowTips.Execute;
	end;
end;

end.


