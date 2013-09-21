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

unit devcfg;

interface

uses
{$IFDEF WIN32}
  Dialogs, Windows, Classes, Graphics, SynEdit, editor, CFGData, IniFiles, prjtypes, Math;
{$ENDIF}
{$IFDEF LINUX}
  QDialogs, Classes, QGraphics, QSynEdit, CFGData, IniFiles, Math, prjtypes;
{$ENDIF}

const
	BoolValYesNo: array[boolean] of AnsiString = ('No', 'Yes');
	ValueToChar: array[0..27] of char = ('0', '1', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h',
                                         'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
                                         's', 't', 'u', 'v', 'w', 'x', 'y', 'z');

type
	// the comments are an example of the record
	PCompilerOption = ^TCompilerOption;
	TCompilerOption = record
		optName: integer; // language table index of "Generate debugging info"
		optSection: integer; // language table index of "C options"
		optIsC: boolean;
		optIsCpp: boolean; // True (C++ option?) - can be both C and C++ option...
		optIsLinker: boolean; // Is it a linker param
		optValue: Integer; // True
		optSetting: AnsiString; // "-g3"
		optChoices : TStringList; // replaces "Yes/No" standard choices (max 30 different choices)
	end;

	// compiler-set configuration
	TdevCompiler = class(TPersistent)
	private
		// Names of all compilers
		fSets: TStrings;
		fCurrentSet : integer;

		// Exe Directories
		fgccName : AnsiString;
		fgppName : AnsiString;
		fmakeName : AnsiString;
		fgdbName : AnsiString;
		fwindresName : AnsiString;
		fdllwrapName : AnsiString;
		fgprofName : AnsiString;

		// Folders
		fBinDir : AnsiString;
		fCDir : AnsiString;
		fCppDir : AnsiString;
		fLibDir : AnsiString;

		// User Parameters
		fCompAdd : boolean;
		fLinkAdd : boolean;
		fCompOpt : AnsiString;
		flinkOpt : AnsiString;

		fDelay : integer;
		fFastDep : boolean;

		function GetGCC : AnsiString;
		function GetGPP : AnsiString;
		function GetMake : AnsiString;
		function GetGDB : AnsiString;
		function GetWindres : AnsiString;
		function GetDllWrap : AnsiString;
		function GetGPROF : AnsiString;

	public

		fOptionString : AnsiString; // options in INI format
		fOptionList: TList; // options in usable memory format

		constructor Create;
		destructor Destroy; override;
		procedure SettoDefaults(const setname,dir : AnsiString);

		// Set stuff
		procedure LoadSet(Index: integer);
		procedure SaveSet(Index: integer);
		procedure ClearSet;

		procedure ReadSets;
		procedure WriteSets;

		property Sets: TStrings read fSets write fSets;
		property CurrentIndex: integer read fCurrentSet write fCurrentSet;

		// Option utils
		procedure AddDefaultOptions;
		procedure AddOption(nameindex,sectionindex: integer; IsC, IsCpp, IsLinker: boolean; Value: integer;const Setting : AnsiString; Choices: TStringList);
		function FindOption(const Setting: AnsiString; var opt: PCompilerOption; var Index: integer): boolean;
		procedure SetOption(option : PCompilerOption;index : integer;newvalue : char);

		// converts
		procedure OptionStringToList(var input : AnsiString);
		procedure OptionListToString;

		// utils
		function CharToValue(c : char) : integer;

		property Delay: integer read fDelay write fDelay;
		property FastDep: boolean read fFastDep write fFastDep;

		property gccName: AnsiString read GetGCC write fgccName;
		property gppName: AnsiString read GetGPP write fgppName;
		property makeName: AnsiString read GetMake write fmakeName;
		property gdbName: AnsiString read GetGDB write fgdbName;
		property windresName: AnsiString read GetWindres write fwindresName;
		property dllwrapName: AnsiString read GetDllWrap write fdllwrapName;
		property gprofName: AnsiString read GetGPROF write fgprofName;

		property BinDir: AnsiString read fBinDir write fBinDir;
		property CDir: AnsiString read fCDir write fCDir;
		property CppDir: AnsiString read fCppDir write fCppDir;
		property LibDir: AnsiString read fLibDir write fLibDir;

		property AddtoComp: boolean read fCompAdd write fCompAdd;
		property AddtoLink: boolean read fLinkAdd write fLinkAdd;
		property CompOpts: AnsiString read fCompOpt write fCompOpt;
		property LinkOpts: AnsiString read fLinkOpt write fLinkOpt;
	end;

  // code-completion window size and other config
  TdevCodeCompletion = class(TPersistent)
  private
    fWidth: integer;
    fHeight: integer;
    fDelay: integer;
    fBackColor: integer;
    fEnabled: boolean;
    fUseCacheFiles: boolean;
    fCacheFiles: TStrings;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SettoDefaults;
    procedure SaveSettings;
    procedure LoadSettings;
  published
    property Width: integer read fWidth write fWidth;
    property Height: integer read fHeight write fHeight;
    property Delay: integer read fDelay write fDelay;
    property BackColor: integer read fBackColor write fBackColor;
    property Enabled: boolean read fEnabled write fEnabled;
    property UseCacheFiles: boolean read fUseCacheFiles write fUseCacheFiles;
    property CacheFiles: TStrings read fCacheFiles write fCacheFiles;
  end;

  // class-browsing view style
  TdevClassBrowsing = class(TPersistent)
  private
    fCBViewStyle: integer;
    fEnabled: boolean;
    fParseLocalHeaders: boolean;
    fParseGlobalHeaders: boolean;
    fShowFilter: integer; // 0 - show all, 1 - show project, 2 - show current
    fUseColors: boolean;
    fShowInheritedMembers: boolean;
  public
    constructor Create;
    procedure SettoDefaults;
    procedure SaveSettings;
    procedure LoadSettings;
  published
    property Enabled: boolean read fEnabled write fEnabled;
    property ViewStyle: integer read fCBViewStyle write fCBViewStyle;
    property ParseLocalHeaders: boolean read fParseLocalHeaders write fParseLocalHeaders;
    property ParseGlobalHeaders: boolean read fParseGlobalHeaders write fParseGlobalHeaders;
    property ShowFilter: integer read fShowFilter write fShowFilter;
    property UseColors: boolean read fUseColors write fUseColors;
    property ShowInheritedMembers: boolean read fShowInheritedMembers write fShowInheritedMembers;
  end;

  // CVS handling module
  TdevCVSHandler = class(TPersistent)
  private
    fRepositories: TStrings;
    fExecutable: AnsiString;
    fCompression: byte;
    fUseSSH: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SettoDefaults;
    procedure SaveSettings;
    procedure LoadSettings;
  published
    property Repositories: TStrings read fRepositories write fRepositories;
    property Executable: AnsiString read fExecutable write fExecutable;
    property Compression: byte read fCompression write fCompression;
    property UseSSH: boolean read fUseSSH write fUseSSH;
  end;

  TdevExternalPrograms = class(TPersistent)
  private
    fDummy: boolean;
    fPrograms: TStrings;
    function GetProgramName(Index: integer): AnsiString;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SettoDefaults;
    procedure SaveSettings;
    procedure LoadSettings;

    property ProgramName[Index: integer]: AnsiString read GetProgramName;
    function AssignedProgram(const ext: AnsiString): integer;
    function AddProgram(ext, prog: AnsiString): integer;
  published
    property Dummy: boolean read fDummy write fDummy;
    property Programs: TStrings read fPrograms write fPrograms;
  end;

  // global directories
  TdevDirs = class(TPersistent)
  private
    fThemes: AnsiString;            // Themes Directory
    fIcons: AnsiString;             // Icon Library
    fHelp: AnsiString;              // Help
    fLang: AnsiString;              // Language
    fTemp: AnsiString;              // Templates
    fDefault: AnsiString;           // user defined default
    fExec: AnsiString;              // dev-c start
    fConfig : AnsiString;           // config files directory
    fOldPath: AnsiString;           // Enviroment Path at program start
  public
    constructor Create;
    procedure SettoDefaults;
    procedure SaveSettings;
    procedure LoadSettings;

    property OriginalPath: AnsiString read fOldPath write fOldPath;
  published
    property Exec: AnsiString read fExec write fExec;
    property Config: AnsiString read fConfig write fConfig;
    property Default: AnsiString read fDefault write fDefault;
    property Help: AnsiString read fHelp write fHelp;
    property Icons: AnsiString read fIcons write fIcons;
    property Lang: AnsiString read fLang write fLang;
    property Templates: AnsiString read fTemp write fTemp;
    property Themes: AnsiString read fThemes write fThemes;
  end;

  // editor options -- syntax, synedit options, etc...
  TdevEditor = class(TPersistent)
  private
    fUseSyn: boolean;           // use syntax highlighting
    fSynExt: AnsiString;        // semi-colon seperated list of highlight ext's
    fFont: TFont;               // Editor Font
    fGutterFont: TFont;         // Gutter font
    fInsertCaret: integer;      // Editor insert caret
    fOverwriteCaret: integer;   // Editor overwrite caret
    fTabSize: integer;          // Editor Tab Size
    fGutterSize: integer;       // Width of Left margin gutter
    fMarginSize: integer;       // Width of right margin

    fCustomGutter: boolean;     // Use Selected Gutter font
    fGutterAuto: boolean;       // Gutter Auto Sizes
    fShowGutter: boolean;       // Show Left gutter in editor
    fLineNumbers: boolean;      // Show Line Numbers
    fLeadZero: boolean;         // Show leading zero's in line nums
    fFirstisZero: boolean;      // First line is zero

    fMarginVis: boolean;        // Toggle right margin line

    fShowScrollHint: boolean;   // Show line number when scrolling
    fShowScrollbars: boolean;   // Show Scroll bars
    fHalfPage: boolean;         // PgUp/PgDn move half a page

    fPastEOF: boolean;          // Cursor moves past end of file
    fPastEOL: boolean;          // Cursor moves past end of lines
    fdblLine: boolean;          // Double Click selects a line
    fFindText: boolean;         // Text at cursor defaults in find dialog
    fEHomeKey: boolean;         // Home key like visual studio
    fGroupUndo: boolean;        // treat same undo's as single undo
    fInsDropFiles: boolean;     // Insert files when drag/dropped else open
    fInsertMode: boolean;       // Editor defaults to insert mode
    fAutoIndent: boolean;       // Auto-indent code lines
    fSmartTabs: boolean;        // Tab to next no whitespace char
    fSpecialChar: boolean;      // special line characters visible
    fUseTabs: boolean;          // convert tabs to spaces
    fShowFunctionTip: boolean;  // show function tip
    fMarginColor: TColor;       // Color of right margin
    fSyntax: TStrings;          // Holds attributes settings
    fDefaultCode: boolean;      // Insert Default Source Code into new files
    fParserHints: boolean;      // Show parser's hint for the word under the cursor
    fMatch : boolean;           // Highlight matching parenthesis
    fHighCurrLine: boolean;     // Highlight current line
    fHighColor: TColor;         // Color of current line when highlighted
    fTrimTrailingSpaces : boolean;

    // Autosave
    fEnableAutoSave : boolean;
    fInterval : integer;
    fSaveType : integer;

    // Symbol completion
    fBraceComplete : boolean;
    fParentheseComplete : boolean;
    fIncludeComplete : boolean;
    fCommentComplete : boolean;
    fArrayComplete : boolean;
    fCompleteSymbols : boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SettoDefaults;
    procedure SaveSettings;
    procedure LoadSettings;

    procedure AssignEditor(editor : TSynEdit;const FileName : AnsiString);
  published
    //Editor props
    property AutoIndent: boolean read fAutoIndent write fAutoIndent;
    property InsertMode: boolean read fInsertMode write fInsertMode;
    property UseTabs: boolean read fUseTabs write fUseTabs;
    property SmartTabs: boolean read fSmartTabs write fSmartTabs;
    property GroupUndo: boolean read fGroupUndo write fGroupUndo;
    property EHomeKey: boolean read fEHomeKey write fEHomeKey;
    property PastEOF: boolean read fPastEOF write fPastEOF;
    property PastEOL: boolean read fPastEOL write fPastEOL;
    property DblClkLine: boolean read fdblLine write fdblLine;
    property FindText: boolean read fFindText write fFindText;
    property Scrollbars: boolean read fShowScrollbars write fShowScrollbars;
    property HalfPageScroll: boolean read fHalfPage write fHalfPage;
    property ScrollHint: boolean read fShowScrollHint write fShowScrollHint;
    property SpecialChars: boolean read fSpecialChar write fSpecialChar;
    property ShowFunctionTip: boolean read fShowFunctionTip write fShowFunctionTip;
    property TrimTrailingSpaces: boolean read fTrimTrailingSpaces write fTrimTrailingSpaces;

    property TabSize: integer read fTabSize write fTabSize;
    property MarginVis: boolean read fMarginVis write fMarginVis;
    property MarginSize: integer read fMarginSize write fMarginSize;
    property MarginColor: TColor read fMarginColor write fMarginColor;
    property InsertCaret: integer read fInsertCaret write fInsertCaret;
    property OverwriteCaret: integer read fOverwriteCaret write fOverwriteCaret;
    property InsDropFiles: boolean read fInsDropFiles write fInsDropFiles;
    property Font: TFont read fFont write fFont;

    // Gutter options
    property GutterVis: boolean read fShowGutter write fShowGutter;
    property GutterAuto: boolean read fGutterAuto write fGutterAuto;
    property LineNumbers: boolean read fLineNumbers write fLineNumbers;
    property LeadZero: boolean read fLeadZero write fLeadZero;
    property FirstLineZero: boolean read fFirstisZero write fFirstisZero;
    property Gutterfnt: boolean read fCustomGutter write fCustomGutter;
    property GutterSize: integer read fGutterSize write fGutterSize;
    property Gutterfont: TFont read fGutterfont write fGutterFont;

    // syntax
    property UseSyntax: boolean read fUseSyn write fUseSyn;
    property SyntaxExt: AnsiString read fSynExt write fSynExt;
    property Syntax: TStrings read fSyntax write fSyntax;

    // other
    property DefaultCode: boolean read fDefaultCode write fDefaultCode;
    property ParserHints: boolean read fParserHints write fParserHints;
    property Match: boolean read fMatch write fMatch;
    property HighCurrLine: boolean read fHighCurrLine write fHighCurrLine;
    property HighColor: TColor read fHighColor write fHighColor;

    // Autosave
    property EnableAutoSave: boolean read fEnableAutoSave write fEnableAutoSave;
    property Interval: integer read fInterval write fInterval;
    property SaveType: integer read fSaveType write fSaveType;

    // Brace completion
    property BraceComplete: boolean read fBraceComplete write fBraceComplete;
    property ParentheseComplete: boolean read fParentheseComplete write fParentheseComplete;
    property IncludeComplete: boolean read fIncludeComplete write fIncludeComplete;
    property CommentComplete: boolean read fCommentComplete write fCommentComplete;
    property ArrayComplete: boolean read fArrayComplete write fArrayComplete;
    property CompleteSymbols: boolean read fCompleteSymbols write fCompleteSymbols;
  end;

  // master option object -- contains program globals
  TdevData = class(TConfigData)
  private
    fLang: AnsiString;                // Language file
    fTheme: AnsiString;               // Theme file
    fFindCols: AnsiString;            // Find Column widths (comma sep)
    fCompCols: AnsiString;            // Compiler Column Widths (comma sep)
    fMsgTabs: integer;                // Editor Tabs
    fMinOnRun: boolean;               // Minimize IDE on run
    fMRUMax: integer;                 // Max number of files in history list
    fBackup: boolean;                 // Create backup files
    fAutoOpen: integer;               // Auto Open Project Files Style
    fShowProject: boolean;            // Show the project explorer
    fProjectWidth: integer;
    fClassView: boolean;              // if true, shows the class view, else shows the file view
    fOutput: boolean;                 // show compiler message window
    fOutputOnNeed: boolean;           // show compiler messages only when problem
    fOutputHeight: integer;           // the height of the output window
    fStatusbar: boolean;              // Statusbar Visible
    fFullScr: boolean;                // IDE is Full screen
    fShowBars: boolean;               // Show toolbars in FullScreen mode
    fMultiLineTab: boolean;           // Show multiline tabs
    fDefCpp: boolean;                 // Default to C++ project (compile with g++)
    fFirst: boolean;                  // first run of dev-c
    fSplash: AnsiString;                  // user selected splash screen
    fdblFiles: boolean;               // double click opens files out of project manager
    fLangChange: boolean;             // flag for language change
    fthemeChange: boolean;            // did the theme change?
    fNoSplashScreen : boolean;        // disable splash screen
    fInterfaceFont : AnsiString;
    fInterfaceFontSize : integer;
    fConsolePause : boolean;

    fToolbarMain: boolean;            // These ones follow the enable/x-offset/y-offset patern
    fToolbarMainX: integer;
    fToolbarMainY: integer;
    fToolbarEdit: boolean;
    fToolbarEditX: integer;
    fToolbarEditY: integer;
    fToolbarCompile: boolean;
    fToolbarCompileX: integer;
    fToolbarCompileY: integer;
    fToolbarProject: boolean;
    fToolbarProjectX: integer;
    fToolbarProjectY: integer;
    fToolbarSpecials: boolean;
    fToolbarSpecialsX: integer;
    fToolbarSpecialsY: integer;
    fToolbarSearch: boolean;
    fToolbarSearchX: integer;
    fToolbarSearchY: integer;
    fToolbarClasses: boolean;
    fToolbarClassesX: integer;
    fToolbarClassesY: integer;

		// file associations (see FileAssocs.pas)
		fAssociateCpp: boolean;
		fAssociateC: boolean;
		fAssociateHpp: boolean;
		fAssociateH: boolean;
		fAssociateDev: boolean;
		fAssociateRc: boolean;
		fAssociateTemplate: boolean;

		// More misc stuff
		fShowTipsOnStart: boolean;
		fLastTip: integer;
		fFileDate : integer;              // Dev-C++ File Date for checking old configurations
		fShowProgress : boolean;          // Show progress window during compile
		fAutoCloseProgress : boolean;     // Auto close progress bar window after compile

		// Printer
		fPrintColors : boolean;           // print colors
		fPrintHighlight : boolean;
		fPrintWordWrap : boolean;
		fPrintLineNumbers : boolean;
		fPrintLineNumbersMargins : boolean;

		// Some debug options
		fWatchHint : boolean;             // watch variable under mouse
		fUseATTSyntax : boolean;
		fShowCPUSignal : boolean; // show CPU window on signal
		fCPURegisterCol1 : integer; // width of column 1
		fCPURegisterCol2 : integer; // width of column 1
		fCPURegisterCol3 : integer; // width of column 1

		// Search preferences
		fCaseSensitive : boolean;
		fWholewords : boolean;
		fPromptReplace : boolean;
		fScopeIsSelected : boolean; // false == Global
		fOriginEntireScope : boolean; // false == from cursor
		fSearchWhere : integer; // 0 == project files, 1 == open files, 2 == current file
		fDirBackward : boolean;

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure SettoDefaults; override;

    class function DevData: TDevData;
    property LangChange: boolean read fLangChange write fLangChange;
    property ThemeChange: boolean read fThemeChange write fThemeChange;
  published
    property Language: AnsiString read fLang write fLang;
    property Theme: AnsiString read fTheme write fTheme;
    property First: boolean read fFirst write fFirst;
    property Splash: AnsiString read fSplash write fSplash;
    property MRUMax: integer read fMRUMax write fMRUMax;
    property DblFiles: boolean read fDblFiles write fDblFiles;
    property NoSplashScreen: boolean read fNoSplashScreen write fNoSplashScreen;

    //Execution
    property MinOnRun: boolean read fMinOnRun write fMinOnRun;
    property ConsolePause: boolean read fConsolePause write fConsolePause;
    property BackUps: boolean read fBackup write fBackup;
    property AutoOpen: integer read fAutoOpen write fAutoOpen;

    //Windows
    property MsgTabs: integer read fMsgTabs write fMsgTabs;
    property InterfaceFont: AnsiString read fInterfaceFont write fInterfaceFont;
    property InterfaceFontSize: integer read fInterfaceFontSize write fInterfaceFontSize;
    property ShowBars: boolean read fShowbars write fShowbars;
    property MultiLineTab: boolean read fMultiLineTab write fMultiLineTab;

    //Running Status Options
    property DefCpp: boolean read fDefCpp write fDefCpp;
    property ShowOutput: boolean read fOutput write fOutput;
    property OutputOnNeed: boolean read fOutputOnNeed write fOutputOnNeed;
    property OutputHeight: integer read fOutputHeight write fOutputHeight;
    property ProjectView: boolean read fShowProject write fShowProject;
    property ClassView: boolean read fClassView write fClassView;
    property ProjectWidth: integer read fProjectWidth write fProjectWidth;
    property Statusbar: boolean read fStatusbar write fStatusbar;
    property FullScreen: boolean read fFullScr write fFullScr;
    property FindCols: AnsiString read fFindCols write fFindCols;
    property CompCols: AnsiString read fCompCols write fCompCols;

    //Toolbars
    property ToolbarMain: boolean read fToolbarMain write fToolbarMain;
    property ToolbarMainX: integer read fToolbarMainX write fToolbarMainX;
    property ToolbarMainY: integer read fToolbarMainY write fToolbarMainY;
    property ToolbarEdit: boolean read fToolbarEdit write fToolbarEdit;
    property ToolbarEditX: integer read fToolbarEditX write fToolbarEditX;
    property ToolbarEditY: integer read fToolbarEditY write fToolbarEditY;
    property ToolbarCompile: boolean read fToolbarCompile write fToolbarCompile;
    property ToolbarCompileX: integer read fToolbarCompileX write fToolbarCompileX;
    property ToolbarCompileY: integer read fToolbarCompileY write fToolbarCompileY;
    property ToolbarProject: boolean read fToolbarProject write fToolbarProject;
    property ToolbarProjectX: integer read fToolbarProjectX write fToolbarProjectX;
    property ToolbarProjectY: integer read fToolbarProjectY write fToolbarProjectY;
    property ToolbarSpecials: boolean read fToolbarSpecials write fToolbarSpecials;
    property ToolbarSpecialsX: integer read fToolbarSpecialsX write fToolbarSpecialsX;
    property ToolbarSpecialsY: integer read fToolbarSpecialsY write fToolbarSpecialsY;
    property ToolbarSearch: boolean read fToolbarSearch write fToolbarSearch;
    property ToolbarSearchX: integer read fToolbarSearchX write fToolbarSearchX;
    property ToolbarSearchY: integer read fToolbarSearchY write fToolbarSearchY;
    property ToolbarClasses: boolean read fToolbarClasses write fToolbarClasses;
    property ToolbarClassesX: integer read fToolbarClassesX write fToolbarClassesX;
    property ToolbarClassesY: integer read fToolbarClassesY write fToolbarClassesY;

    // file associations
    property AssociateCpp: boolean read fAssociateCpp write fAssociateCpp;
    property AssociateC: boolean read fAssociateC write fAssociateC;
    property AssociateHpp: boolean read fAssociateHpp write fAssociateHpp;
    property AssociateH: boolean read fAssociateH write fAssociateH;
    property AssociateDev: boolean read fAssociateDev write fAssociateDev;
    property AssociateRc: boolean read fAssociateRc write fAssociateRc;
    property AssociateTemplate: boolean read fAssociateTemplate write fAssociateTemplate;

    // tip of the day
    property ShowTipsOnStart: boolean read fShowTipsOnStart write fShowTipsOnStart;
    property LastTip: integer read fLastTip write fLastTip;
    property FileDate: integer read fFileDate write fFileDate;

    // progress window
    property ShowProgress: boolean read fShowProgress write fShowProgress;
    property AutoCloseProgress: boolean read fAutoCloseProgress write fAutoCloseProgress;

    //  Printer
    property PrintColors: boolean read fPrintColors write fPrintColors;
    property PrintHighlight : boolean read fPrintHighlight write fPrintHighlight;
    property PrintWordWrap : boolean read fPrintWordWrap write fPrintWordWrap;
    property PrintLineNumbers : boolean read fPrintLineNumbers write fPrintLineNumbers;
    property PrintLineNumbersMargins : boolean read fPrintLineNumbersMargins write fPrintLineNumbersMargins;

    // General debugging options
    property WatchHint : boolean read fWatchHint write fWatchHint;
    property UseATTSyntax : boolean read fUseATTSyntax write fUseATTSyntax;
    property ShowCPUSignal : boolean read fShowCPUSignal write fShowCPUSignal;
    property CPURegisterCol1 : integer read fCPURegisterCol1 write fCPURegisterCol1;
    property CPURegisterCol2 : integer read fCPURegisterCol2 write fCPURegisterCol2;
    property CPURegisterCol3 : integer read fCPURegisterCol3 write fCPURegisterCol3;

    // Search preferences
    property CaseSensitive : boolean read fCaseSensitive write fCaseSensitive;
    property Wholewords : boolean read fWholewords write fWholewords;
    property PromptReplace : boolean read fPromptReplace write fPromptReplace;
    property ScopeIsSelected : boolean read fScopeIsSelected write fScopeIsSelected;
    property OriginEntireScope : boolean read fOriginEntireScope write fOriginEntireScope;
    property SearchWhere : integer read fSearchWhere write fSearchWhere;
    property DirBackward : boolean read fDirBackward write fDirBackward;
  end;

function devData: TdevData;

procedure InitializeOptions;
procedure SaveOptions;
procedure FinalizeOptions;
procedure CheckForAltConfigFile(const filename: AnsiString);
procedure UpdateAltConfigFile;

var
  devCompiler: TdevCompiler = nil;
  devDirs: TdevDirs = nil;
  devEditor: TdevEditor = nil;
  devCodeCompletion: TdevCodeCompletion = nil;
  devClassBrowsing: TdevClassBrowsing = nil;
  devCVSHandler: TdevCVSHandler = nil;
  devExternalPrograms: TdevExternalPrograms = nil;


	// %APPDATA% storgage, usage of -c, %APPDATA% not found, but not portable
	ConfigMode             : (CFG_APPDATA, CFG_PARAM, CFG_EXEFOLDER) = CFG_APPDATA;

	StandardConfigFile     : AnsiString;
	UseAltConfigFile       : boolean;
	AltConfigFile          : AnsiString;
	DontRecreateSingletons : boolean;

implementation

uses
{$IFDEF WIN32}
  MultiLangSupport, datamod, SysUtils, StrUtils, Forms, main, compiler, Controls, version, utils, SynEditMiscClasses,
  FileAssocs;
{$ENDIF}
{$IFDEF LINUX}
  MultiLangSupport, SysUtils, StrUtils, QForms, QControls, version, utils, QSynEditMiscClasses,
  FileAssocs, Types;
{$ENDIF}

procedure InitializeOptions;
var
	compilername : AnsiString;
begin
	if not assigned(devDirs) then
		devDirs:= TdevDirs.Create;

	if not assigned(devCompiler) then
		devCompiler:= TdevCompiler.Create;

	// load available compiler sets on first run
	if devData.First then begin

		devCompiler.Sets.Clear;

		// Assume 64bit compilers are put in the MinGW64 folder
		if DirectoryExists(devDirs.Exec + 'MinGW64\') then begin

			compilername := GetInfoOfCompiler(devDirs.Exec + 'MinGW64\bin\');
			if compilername = '' then
				compilername := 'MinGW64';

			devCompiler.Sets.Add(compilername + ' 64-bit');
			devCompiler.SettoDefaults(compilername + ' 64-bit','MinGW64');
			devCompiler.SaveSet(devCompiler.Sets.Count-1);

			devCompiler.Sets.Add(compilername + ' 32-bit');
			devCompiler.SettoDefaults(compilername + ' 32-bit','MinGW64');
			devCompiler.SaveSet(devCompiler.Sets.Count-1);
		end;
		if DirectoryExists(devDirs.Exec + 'MinGW32\') then begin

			compilername := GetInfoOfCompiler(devDirs.Exec + 'MinGW32\bin\');
			if compilername = '' then
				compilername := 'MinGW32';

			devCompiler.Sets.Add(compilername + ' 32-bit');
			devCompiler.SettoDefaults(compilername + ' 32-bit','MinGW32');
			devCompiler.SaveSet(devCompiler.Sets.Count-1);
		end;

		// Write the compiler list
		devCompiler.WriteSets;
	end;

	// Load the current compiler set, if there is any
	devCompiler.LoadSet(devCompiler.fCurrentSet);

	if not assigned(devEditor) then
		devEditor:= TdevEditor.Create;

	if not assigned(devCodeCompletion) then
		devCodeCompletion:= TdevCodeCompletion.Create;

	if not assigned(devClassBrowsing) then
		devClassBrowsing:= TdevClassBrowsing.Create;

	if not assigned(devCVSHandler) then
		devCVSHandler:= TdevCVSHandler.Create;

	if not assigned(devExternalPrograms) then
		devExternalPrograms:= TdevExternalPrograms.Create;
end;

procedure SaveOptions;
begin
  devData.SaveConfigData;
  devDirs.SaveSettings;
  // devCompiler saving is done by CompOptForm
  devEditor.SaveSettings;
  devCodeCompletion.SaveSettings;
  devClassBrowsing.SaveSettings;
  devCVSHandler.SaveSettings;
  devExternalPrograms.SaveSettings;
end;

procedure FinalizeOptions;
begin
  // devData is freed last
  devDirs.Free;
  devCompiler.Free;
  devEditor.Free;
  devCodeCompletion.Free;
  devClassBrowsing.Free;
  devCVSHandler.Free;
  devExternalPrograms.Free;
end;

procedure CheckForAltConfigFile(const filename: AnsiString);
var
    Ini: TIniFile;
begin
  UseAltConfigFile:=false;
  AltConfigFile:='';
  if not FileExists(filename) then
    Exit;
  Ini:=TIniFile.Create(filename);
  try
    UseAltConfigFile:=Ini.ReadBool('Options', 'UseAltConfigFile', false);
    AltConfigFile:=Ini.ReadString('Options', 'AltConfigFile', '');
  finally
    Ini.Free;
  end;
end;

procedure UpdateAltConfigFile;
var
    Ini: TIniFile;
begin
  Ini:=TIniFile.Create(StandardConfigFile);
  try
    Ini.WriteBool('Options', 'UseAltConfigFile', UseAltConfigFile);
    Ini.WriteString('Options', 'AltConfigFile', AltConfigFile);
  finally
    Ini.Free;
  end;
end;

 { TDevData - Singleton pattern }
var
 fdevData: TdevData = nil;
 fExternal: boolean = TRUE;

function devData: TdevData;
begin
	if not assigned(fdevData) and not DontRecreateSingletons then begin
		fExternal:= FALSE;
		try
			fdevData:= TdevData.Create(nil);
		finally
			fExternal:= TRUE;
		end;
	end;
	result:= fDevData;
end;

class function TdevData.devData: TdevData;
begin
  result:= devcfg.devData;
end;
(*
  raises an exception when:
   1 - try to create without call to devdata function
         i.e. opt:= TdevData.Create; -- will raise
   2 - if already created -- should never see
*)

// add strings to lang file
constructor TdevData.Create(aOwner: Tcomponent);
begin
	if assigned(fdevData) then
		raise Exception.Create('devData already created');
	if fExternal then
		raise Exception.Create('devData Externally Created');
	inherited Create(aOwner);
	IgnoreProperties.Add('Style');
	IgnoreProperties.Add('Exec');
	IgnoreProperties.Add('Config');
	SettoDefaults;
end;

destructor TdevData.Destroy;
begin
	fdevData:= nil;
	inherited;
end;

procedure TdevData.SettoDefaults;

  function getAssociation(I: integer): Boolean;
  begin
    Result := CheckFiletype('.' + Associations[I, 0],
      'DevCpp.' + Associations[I, 0],
      Associations[I, 1],
      'open',
      Application.Exename + ' "%1"');
  end;

begin
  fFirst:= TRUE;
  fLang:= 'English.lng';
  fFindCols:= '75, 75, 120, 150';
  fCompCols:= '75, 75, 120, 150';
  fMsgTabs:= 0; // Top
  fMRUMax:= 10;
  fMinOnRun:= FALSE;
  fBackup:= FALSE;
  fAutoOpen:= 0;
  fShowProject:= TRUE;
  fClassView:= FALSE;
  fProjectWidth:=161;
  fOutput:= FALSE;
  fOutputOnNeed:= TRUE;
  fOutputHeight:=183;
  fStatusbar:= TRUE;
  fShowBars:= FALSE;
  fMultiLineTab:= TRUE;
  fDefCpp:= TRUE;
  fdblFiles:= FALSE;
  fConsolePause:=TRUE;

	fToolbarMain:=TRUE;
	fToolbarMainX:=11;
	fToolbarMainY:=2;
	fToolbarEdit:=TRUE;
	fToolbarEditX:=173;
	fToolbarEditY:=2;
	fToolbarCompile:=TRUE;
	fToolbarCompileX:=441;
	fToolbarCompileY:=2;
	fToolbarProject:=TRUE;
	fToolbarProjectX:=350;
	fToolbarProjectY:=2;
	fToolbarSpecials:=TRUE;
	fToolbarSpecialsX:=624;
	fToolbarSpecialsY:=2;
	fToolbarSearch:=TRUE;
	fToolbarSearchX:=233;
	fToolbarSearchY:=2;
	fToolbarClasses:=TRUE;
	fToolbarClassesX:=11;
	fToolbarClassesY:=30;

	// Office 2007 / Vista support
	if Screen.Fonts.IndexOf('Segoe UI') <> -1 then begin
		fInterfaceFontSize := 9;
		fInterfaceFont := 'Segoe UI';
	end else begin
		fInterfaceFontSize := 8;
		fInterfaceFont := 'MS Sans Serif';
	end;

  //read associations set by installer as defaults
  fAssociateC := getAssociation(0);
  fAssociateCpp := getAssociation(1);
  fAssociateH := getAssociation(2);
  fAssociateHpp := getAssociation(3);
  fAssociateDev := getAssociation(4);
  fAssociateRc := getAssociation(5);
  fAssociateTemplate := getAssociation(6);

  fShowTipsOnStart := TRUE;
  fLastTip := 0;
  fFileDate := 0;
  fShowProgress := TRUE;
  fAutoCloseProgress := FALSE;
  fPrintColors := TRUE;
  fPrintHighlight := TRUE;
  fPrintWordWrap := FALSE;
  fPrintLineNumbers := TRUE;
  fPrintLineNumbersMargins := FALSE;

	// Debug stuff
	fWatchHint := false;
	fUseATTSyntax := true;
	fShowCPUSignal := true;
	fCPURegisterCol1 := 70;
	fCPURegisterCol2 := 104;
	fCPURegisterCol3 := 4;

	// Search stuff
	fCaseSensitive := false;
	fWholewords := false;
	fPromptReplace := false;
	fScopeIsSelected := false;
	fOriginEntireScope := false;
	fSearchWhere := 1;
	fDirBackward := false;
end;

{ TdevCompiler }

constructor TdevCompiler.Create;
begin
	inherited;
	fSets := TStringList.Create;
	fOptionList := TList.Create;
	AddDefaultOptions;
	OptionListToString;
	ReadSets;
end;

destructor TdevCompiler.Destroy;
var
	I : integer;
begin
	for I := 0 to fOptionList.Count - 1 do begin
		if Assigned(PCompilerOption(fOptionList[I])^.optChoices) then
			PCompilerOption(fOptionList[I])^.optChoices.Free;
		Dispose(PCompilerOption(fOptionList[I]));
	end;
	fOptionList.Free;
	fSets.Free;
	inherited;
end;

procedure TdevCompiler.OptionListToString;
var
	I : integer;
begin
	fOptionString := '';
	for I := 0 to fOptionList.Count - 1 do
		fOptionString := fOptionString + ValueToChar[PCompilerOption(fOptionList[I])^.optValue];
end;

procedure TdevCompiler.OptionStringToList(var input : AnsiString);
var
	I: integer;
begin
	for I := 0 to fOptionList.Count - 1 do
		if I < Length(input) then // set option in list
			PCompilerOption(fOptionList[I])^.optValue := CharToValue(input[I+1])
		else
			input := input + '0'; // append value so string is complete
end;

procedure TdevCompiler.SetOption(option : PCompilerOption;index : integer;newvalue : char);
begin
	if Assigned(option) then
		option^.optValue := CharToValue(newvalue);

	if index+1 <= Length(fOptionString) then
		fOptionString[index+1] := newvalue;
		// complete string?
end;

procedure TdevCompiler.LoadSet(Index: integer);
var
	key : AnsiString;
begin
	// Load the current index from disk
	key := 'CompilerSets_' + IntToStr(Index);

	// Programs
	fgccName     := devData.LoadSettingS(key, GCC_PROGRAM);
	fgppName     := devData.LoadSettingS(key, GPP_PROGRAM);
	fgdbName     := devData.LoadSettingS(key, GDB_PROGRAM);
	fmakeName    := devData.LoadSettingS(key, MAKE_PROGRAM);
	fwindresName := devData.LoadSettingS(key, WINDRES_PROGRAM);
	fdllwrapName := devData.LoadSettingS(key, DLLWRAP_PROGRAM);
	fgprofName   := devData.LoadSettingS(key, GPROF_PROGRAM);

	// If nothing was found, select defaults
	if fgccName=''     then fgccName:=     GCC_PROGRAM;
	if fgppName=''     then fgppName:=     GPP_PROGRAM;
	if fgdbName=''     then fgdbName:=     GDB_PROGRAM;
	if fmakeName=''    then fmakeName:=    MAKE_PROGRAM;
	if fwindresName='' then fwindresName:= WINDRES_PROGRAM;
	if fdllwrapName='' then fdllwrapName:= DLLWRAP_PROGRAM;
	if fgprofName=''   then fgprofName:=   GPROF_PROGRAM;

	// Load the option string
	fOptionString := devData.LoadSettingS(key, 'Options');

	// Convert to better list format
	OptionStringToList(fOptionString);

	// Extra parameters
	fCompOpt := devData.LoadSettingS(key, 'CompOpt');
	fLinkOpt := devData.LoadSettingS(key, 'LinkOpt');
	fCompAdd := devData.LoadSettingB(key, 'CompAdd');
	fLinkAdd := devData.LoadSettingB(key, 'LinkAdd');

	// Misc. (general tab)
	fDelay :=   StrToIntDef(devData.LoadSettingS(key, 'Delay'),0);
	fFastDep := devData.LoadSettingB(key, 'FastDep','1');

	// Directories
	fBinDir := devData.LoadSettingS(key, 'Bins');
	fCDir   := devData.LoadSettingS(key, 'C');
	fCppDir := devData.LoadSettingS(key, 'Cpp');
	fLibDir := devData.LoadSettingS(key, 'Lib');

	// Directories
	fBinDir := ReplaceFirstStr(fBinDir, '%path%\',devDirs.Exec);
	fCDir   := ReplaceFirstStr(fCDir,   '%path%\',devDirs.Exec);
	fCppDir := ReplaceFirstStr(fCppDir, '%path%\',devDirs.Exec);
	fLibDir := ReplaceFirstStr(fLibDir, '%path%\',devDirs.Exec);

	devCompiler.CurrentIndex := Index;

	if devDirs.OriginalPath = '' then // first time only
		devDirs.OriginalPath := GetEnvironmentVariable('PATH');

	SetPath(fBinDir);
end;

procedure TdevCompiler.SaveSet(Index: integer);
var
	key: AnsiString;
begin
	with devData do begin
		key := 'CompilerSets_' + IntToStr(Index);

		// Programs
		SaveSettingS(key, GCC_PROGRAM,     fgccName);
		SaveSettingS(key, GPP_PROGRAM,     fgppName);
		SaveSettingS(key, GDB_PROGRAM,     fgdbName);
		SaveSettingS(key, MAKE_PROGRAM,    fmakeName);
		SaveSettingS(key, WINDRES_PROGRAM, fwindresName);
		SaveSettingS(key, DLLWRAP_PROGRAM, fdllwrapName);
		SaveSettingS(key, GPROF_PROGRAM,   fgprofName);

		// Save option string
		SaveSettingS(key, 'Options',       fOptionString);

		// Save extra 'general' options
		SaveSettingS(key, 'CompOpt',       fCompOpt);
		SaveSettingS(key, 'LinkOpt',       fLinkOpt);
		SaveSettingB(key, 'CompAdd',       fCompAdd);
		SaveSettingB(key, 'LinkAdd',       fLinkAdd);

		SaveSettingS(key, 'Delay',         inttostr(fDelay));
		SaveSettingB(key, 'FastDep',       fFastDep);

		// Paths
		SaveSettingS(key, 'Bins',  ReplaceFirstStr(fBinDir,devDirs.fExec,'%path%\'));
		SaveSettingS(key, 'C',     ReplaceFirstStr(fCDir,  devDirs.fExec,'%path%\'));
		SaveSettingS(key, 'Cpp',   ReplaceFirstStr(fCppDir,devDirs.fExec,'%path%\'));
		SaveSettingS(key, 'Lib',   ReplaceFirstStr(fLibDir,devDirs.fExec,'%path%\'));
	end;
end;

procedure TdevCompiler.ClearSet;
begin
	fgccName := '';
	fgppName := '';
	fgdbName := '';
	fmakeName := '';
	fwindresName := '';
	fdllwrapName := '';
	fgprofName := '';

	fOptionString := '';
	OptionStringToList(fOptionString);

	fCompOpt := '';
	fLinkOpt := '';
	fCompAdd := false;
	fLinkAdd := false;

	fDelay := 0;
	fFastDep := false;

	fBinDir := '';
	fCDir := '';
	fCppDir := '';
	fLibDir := '';
end;

procedure TdevCompiler.SettoDefaults(const setname,dir : AnsiString);
var
	optP : PCompilerOption;
	idxP : integer;
begin

	// Store program names
	fgccName := GCC_PROGRAM;
	fgppName := GPP_PROGRAM;

	// Assume a special gdb32 is provided for 32bit users
	if ContainsStr(setname,'TDM-GCC') and ContainsStr(setname,'32-bit') then
		fgdbName := GDB32_PROGRAM
	else
		fgdbName := GDB_PROGRAM;

	fmakeName := MAKE_PROGRAM;
	fwindresName := WINDRES_PROGRAM;
	fdllwrapName := DLLWRAP_PROGRAM;
	fgprofName := GPROF_PROGRAM;

	// Command line text
	fCompAdd := FALSE;
	fLinkAdd := TRUE;
	fCompOpt :='';

	// MinGW32 requires special treatment
	if ContainsStr(setname,'MinGW') then
		fLinkOpt := '-static-libstdc++ -static-libgcc'
	else
		fLinkOpt := '-static-libgcc';

	if ContainsStr(setname,'MinGW') and ContainsStr(setname,'32-bit') then begin
		fBinDir:= devDirs.fExec + dir + '\bin';
		fLibDir:= devDirs.fExec + dir + '\lib';
		fCDir  := devDirs.fExec + dir + '\include';
		fCppDir:= devDirs.fExec + dir + '\include';
	end else begin
		fBinDir:= devDirs.fExec + dir + '\bin';
		if ContainsStr(setname,'TDM-GCC') and ContainsStr(setname,'32-bit') then
			fLibDir:= devDirs.fExec + dir + '\x86_64-w64-mingw32\lib32'
		else
			fLibDir:= devDirs.fExec + dir + '\x86_64-w64-mingw32\lib';
		fCDir  := devDirs.fExec + dir + '\x86_64-w64-mingw32\include';
		fCppDir:= devDirs.fExec + dir + '\x86_64-w64-mingw32\include';
	end;

	// Makefile
	fDelay := 0;
	fFastDep := TRUE;

	// Edit option string and list
	if FindOption('-',optP,idxP) then begin // -m is used my -mINSTRUCTIONSET, so use - instead
		if ContainsStr(setname,'TDM-GCC') and ContainsStr(setname,'32-bit') then
			SetOption(optP,idxP,'1')
		else
			SetOption(optP,idxP,'0');
	end;
end;

procedure TdevCompiler.ReadSets;
var
	Ini: TIniFile;
	sl: TStringList;
	I: integer;
begin
	fSets.Clear;
	Ini:=TIniFile.Create(devData.INIFile);
	sl:=TStringList.Create;
	try

		// Read current list of sets
		Ini.ReadSectionValues('CompilerSets', sl);
		for I := 0 to sl.Count - 1 do
			if not SameStr(sl.Names[I],'Current') then
				fSets.Add(sl.Values[sl.Names[I]])
			else
				CurrentIndex := StrToInt(sl.Values[sl.Names[I]]);

	finally
		sl.Free;
		Ini.Free;
	end;
end;

procedure TdevCompiler.WriteSets;
var
	Ini: TIniFile;
	I: integer;
begin
	Ini:=TIniFile.Create(devData.INIFile);
	try
		Ini.EraseSection('CompilerSets');

		// Save the list of compilers
		for I := 0 to fSets.Count-1 do
			Ini.WriteString('CompilerSets', IntToStr(I), fSets[I]);

		// Save the current index
		Ini.WriteInteger('CompilerSets','Current',devCompiler.CurrentIndex);
	finally
		Ini.Free;
	end;
end;

function TdevCompiler.GetGCC : AnsiString;
begin
	result := fgccName;
	if SameStr(result,'') then
		result := GCC_PROGRAM;
end;

function TdevCompiler.GetGPP : AnsiString;
begin
	result := fgppName;
	if SameStr(result,'') then
		result := GPP_PROGRAM;
end;

function TdevCompiler.GetMake : AnsiString;
begin
	result := fmakeName;
	if SameStr(result,'') then
		result := MAKE_PROGRAM;
end;

function TdevCompiler.GetGDB : AnsiString;
begin
	result := fgdbname;
	if SameStr(result,'') then
		result := GDB_PROGRAM;
end;

function TdevCompiler.GetWindres : AnsiString;
begin
	result := fwindresname;
	if SameStr(result,'') then
		result := WINDRES_PROGRAM;
end;

function TdevCompiler.GetDllWrap : AnsiString;
begin
	result := fdllwrapname;
	if SameStr(result,'') then
		result := DLLWRAP_PROGRAM;
end;

function TdevCompiler.GetGPROF : AnsiString;
begin
	result := fgprofname;
	if SameStr(result,'') then
		result := GPROF_PROGRAM;
end;

procedure TdevCompiler.AddDefaultOptions;
var
	sl : TStringList;
begin

	// C options
	AddOption(ID_COPT_ANSIC,       ID_COPT_GRP_C, True,  True,  False, 0, '-ansi', nil);
	AddOption(ID_COPT_NOASM,       ID_COPT_GRP_C, True,  True,  False, 0, '-fno-asm', nil);
	AddOption(ID_COPT_TRADITIONAL, ID_COPT_GRP_C, True,  True,  False, 0, '-traditional-cpp', nil);

	// Optimization
	sl := TStringList.Create;
	sl.Add(''); // /!\ Must contain a starting empty value in order to do not have always to pass the parameter
	sl.Add('This CPU=native');
	sl.Add('i386=i386');
	sl.Add('i486=i486');
	sl.Add('i586=i586');
	sl.Add('i686=i686');
	sl.Add('Pentium=pentium');
	sl.Add('Pentium MMX=pentium-mmx');
	sl.Add('Pentium Pro=pentiumpro');
	sl.Add('Pentium 2=pentium2');
	sl.Add('Pentium 3=pentium3');
	sl.Add('Pentium 4=pentium4');
	sl.Add('Conroe=core2');
	sl.Add('Nehalem=corei7');
	sl.Add('Sandy=corei7-avx');
	sl.Add('K6=k6');
	sl.Add('K6-2=k6-2');
	sl.Add('K6-3=k6-3');
	sl.Add('Athlon=athlon');
	sl.Add('Athlon Tbird=athlon-tbird');
	sl.Add('Athlon 4=athlon-4');
	sl.Add('Athlon XP=athlon-xp');
	sl.Add('Athlon MP=athlon-mp');
	sl.Add('K8=k8');
	sl.Add('K8 Rev.E=k8-sse3');
	sl.Add('K10=barcelona');
	sl.Add('Bulldozer=bdver1');
	AddOption(ID_COPT_ARCH, ID_COPT_GRP_CODEGEN, True, True, False, 0, '-march=', sl);

	// Optimization
	sl := TStringList.Create;
	sl.Add(''); // /!\ Must contain a starting empty value in order to do not have always to pass the parameter
	sl.Add('This CPU=native');
	sl.Add('i386=i386');
	sl.Add('i486=i486');
	sl.Add('i586=i586');
	sl.Add('i686=i686');
	sl.Add('Pentium=pentium');
	sl.Add('Pentium MMX=pentium-mmx');
	sl.Add('Pentium Pro=pentiumpro');
	sl.Add('Pentium 2=pentium2');
	sl.Add('Pentium 3=pentium3');
	sl.Add('Pentium 4=pentium4');
	sl.Add('Conroe=core2');
	sl.Add('Nehalem=corei7');
	sl.Add('Sandy=corei7-avx');
	sl.Add('K6=k6');
	sl.Add('K6-2=k6-2');
	sl.Add('K6-3=k6-3');
	sl.Add('Athlon=athlon');
	sl.Add('Athlon Tbird=athlon-tbird');
	sl.Add('Athlon 4=athlon-4');
	sl.Add('Athlon XP=athlon-xp');
	sl.Add('Athlon MP=athlon-mp');
	sl.Add('K8=k8');
	sl.Add('K8 Rev.E=k8-sse3');
	sl.Add('K10=barcelona');
	sl.Add('Bulldozer=bdver1');
	AddOption(ID_COPT_TUNE, ID_COPT_GRP_CODEGEN, True, True, False, 0, '-mtune=', sl);

	// Built-in processor functions
	sl := TStringList.Create;
	sl.Add(''); // /!\ Must contain a starting empty value in order to do not have always to pass the parameter
	sl.Add('MMX=mmx');
	sl.Add('3D Now=3dnow');
	sl.Add('SSE=sse');
	sl.Add('SSE2=sse2');
	sl.Add('SSE3=sse3');
	sl.Add('SSSE3=ssse3');
	sl.Add('SSE4=sse4');
	sl.Add('SSE4A=sse4a');
	sl.Add('SSE4.1=sse4.1');
	sl.Add('SSE4.2=sse4.2');
	sl.Add('AVX=avx');
	sl.Add('FMA4=fma4');
	sl.Add('XOP=xop');
	sl.Add('AES=aes');
	AddOption(ID_COPT_BUILTINPROC, ID_COPT_GRP_CODEGEN, True, True, False, 0, '-m', sl);

	// Optimization
	sl := TStringList.Create;
	sl.Add('');
	sl.Add('Low=1');
	sl.Add('Med=2');
	sl.Add('High=3');
	sl.Add('Highest (fast)=fast');
	sl.Add('Size (s)=s');
	AddOption(ID_COPT_OPTIMIZE, ID_COPT_GRP_CODEGEN, True, True, False, 0, '-O', sl);

	// 32bit/64bit
	sl := TStringList.Create;
	sl.Add('');
	sl.Add('32bit=m32');
	sl.Add('64bit=m64');
	AddOption(ID_COPT_PTRWIDTH, ID_COPT_GRP_CODEGEN, True, True, True, 0, '-', sl);

	// C++ Standards
	sl := TStringList.Create;
	sl.Add(''); // Passing nothing effectively lets the compiler decide
	sl.Add('ISO C90=c90');
	sl.Add('ISO C99=c99');
	sl.Add('ISO C++=c++98');
	sl.Add('ISO C++11=c++0x');
	sl.Add('GNU C90=gnu90');
	sl.Add('GNU C99=gnu99');
	sl.Add('GNU C++=gnu++98');
	sl.Add('GNU C++11=gnu++0x');
	AddOption(ID_COPT_STD, ID_COPT_GRP_CODEGEN, True, True, False, 0, '-std=', sl);

	// Warnings
	AddOption(ID_COPT_WARNING,      ID_COPT_GRP_WARN, True,  True,  False, 0, '-w', nil);
	AddOption(ID_COPT_WARNINGPLUS,  ID_COPT_GRP_WARN, True,  True,  False, 0, '-Wall', nil);
	AddOption(ID_COPT_WARNINGEX,    ID_COPT_GRP_WARN, True,  True,  False, 0, '-Wextra', nil);
	AddOption(ID_COPT_ISOCONFORM,   ID_COPT_GRP_WARN, True,  True,  False, 0, '-pedantic', nil);
	AddOption(ID_COPT_SYNTAXONLY,   ID_COPT_GRP_WARN, True,  True,  False, 0, '-fsyntax-only', nil);
	AddOption(ID_COPT_TREATASERROR, ID_COPT_GRP_WARN, True,  True,  False, 0, '-Werror', nil);
	AddOption(ID_COPT_FAILONFIRST,  ID_COPT_GRP_WARN, True,  True,  False, 0, '-Wfatal-errors', nil);

	// Profiling
	AddOption(ID_COPT_PROFILE, ID_COPT_PROFILING, True,  True,  True,  0, '-pg', nil);

	// Linker
	AddOption(ID_COPT_OBJC,   ID_COPT_LINKERTAB, False, False, True,  0, '-lobjc', nil);
	AddOption(ID_COPT_DEBUG,  ID_COPT_LINKERTAB, True,  True,  True,  0, '-g3', nil);
	AddOption(ID_COPT_NOLIBS, ID_COPT_LINKERTAB, True,  True,  True,  0, '-nostdlib', nil);
	AddOption(ID_COPT_WIN32,  ID_COPT_LINKERTAB, True,  True,  True,  0, '-mwindows', nil);
	AddOption(ID_COPT_STRIP,  ID_COPT_LINKERTAB, False, False, True,  0, '-s', nil);

	// Output
	AddOption(ID_COPT_MEM,      ID_COPT_GRP_OUTPUT, True,  True,  False, 0, '-fverbose-asm', nil);
	AddOption(ID_COPT_ASSEMBLY, ID_COPT_GRP_OUTPUT, True,  True,  False, 0, '-S', nil);
	AddOption(ID_COPT_PIPES,    ID_COPT_GRP_OUTPUT, True,  True,  False, 0, '-pipe', nil);
end;

procedure TdevCompiler.AddOption(nameindex,sectionindex: integer; IsC, IsCpp, IsLinker: boolean; Value: integer;const Setting : AnsiString; Choices: TStringList);
var
	option: PCompilerOption;
begin
	option := New(PCompilerOption);
	with option^ do begin
		optName := nameindex;
		optSection := sectionindex;
		optIsC := IsC;
		optIsCpp := IsCpp;
		optIsLinker := IsLinker;
		optValue := Value;
		optSetting := Setting;
		optChoices := Choices;
	end;
	fOptionList.Add(option);
end;

function TdevCompiler.FindOption(const Setting: AnsiString; var opt: PCompilerOption; var Index: integer): boolean;
var
	I: integer;
begin
	Result:=False;
	for I := 0 to fOptionList.Count - 1 do
		if SameStr(PCompilerOption(fOptionList[I])^.optSetting,Setting) then begin
			opt := PCompilerOption(fOptionList[I]);
			Index := I;
			Result := True;
			Break;
		end;
end;

function TdevCompiler.CharToValue(c : char) : integer;
begin
	if c in ['a'..'z'] then
		result := integer(c) - integer('a') + 2
	else if (StrToIntDef(c, 0) = 1) then
		result := 1
	else
		result := 0;
end;

{ TDevDirs }

constructor TdevDirs.Create;
begin
	inherited Create;
	SettoDefaults;
	LoadSettings;
end;

procedure TdevDirs.SettoDefaults;
begin
	fExec:= IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
	fConfig := fExec;

	fHelp   := fExec + HELP_DIR;
	fIcons  := fExec + ICON_DIR;
	fLang   := fExec + LANGUAGE_DIR;
	fTemp   := fExec + TEMPLATE_DIR;
	fThemes := fExec + THEME_DIR;
end;

procedure TdevDirs.LoadSettings;
begin
	devData.LoadObject(Self,'Directories');

	fConfig := ExtractFilePath(devData.INIFile);
	fHelp   := ReplaceFirstStr(fHelp,  '%path%\',fExec);
	fIcons  := ReplaceFirstStr(fIcons, '%path%\',fExec);
	fLang   := ReplaceFirstStr(fLang,  '%path%\',fExec);
	fTemp   := ReplaceFirstStr(fTemp,  '%path%\',fExec);
	fThemes := ReplaceFirstStr(fThemes,'%path%\',fExec);
end;

procedure TdevDirs.SaveSettings;
begin
  fHelp :=  ReplaceFirstStr(fHelp,  fExec,'%path%\');
  fIcons:=  ReplaceFirstStr(fIcons, fExec,'%path%\');
  fLang:=   ReplaceFirstStr(fLang,  fExec,'%path%\');
  fTemp:=   ReplaceFirstStr(fTemp,  fExec,'%path%\');
  fThemes:= ReplaceFirstStr(fThemes,fExec,'%path%\');

  devData.SaveObject(Self,'Directories');

  fHelp :=  ReplaceFirstStr(fHelp,  '%path%\',fExec);
  fIcons:=  ReplaceFirstStr(fIcons, '%path%\',fExec);
  fLang:=   ReplaceFirstStr(fLang,  '%path%\',fExec);
  fTemp:=   ReplaceFirstStr(fTemp,  '%path%\',fExec);
  fThemes:= ReplaceFirstStr(fThemes,'%path%\',fExec);
end;

constructor TdevEditor.Create;
begin
	inherited;

	fFont:= TFont.Create;
	fGutterfont:= TFont.Create;
	fSyntax:= TStringList.Create;
	TStringList(fSynTax).Duplicates:= dupIgnore;

	SettoDefaults;
	LoadSettings;
end;

destructor TdevEditor.Destroy;
begin
	fFont.Free;
	fGutterfont.Free;
	fSyntax.Free;
	inherited;
end;

procedure TdevEditor.LoadSettings;
begin
	devData.LoadObject(Self,'Editor');
end;

procedure TdevEditor.SaveSettings;
begin
	devData.SaveObject(Self,'Editor');
end;

procedure TdevEditor.SettoDefaults;
begin
	// General
	fAutoIndent:= TRUE;
	fInsertMode:= TRUE;
	fUseTabs:= TRUE;
	fSmartTabs:= FALSE;
	fGroupUndo:= TRUE;
	fInsDropFiles:= FALSE;
	fSpecialChar:= FALSE;

	// General #2
	fEHomeKey:= TRUE;
	fPastEOF:= FALSE;
	fPastEOL:= FALSE;
	fdblLine:= FALSE;
	fFindText:= TRUE;
	fShowScrollbars:= TRUE; // Show as needed
	fHalfPage:= FALSE;
	fShowScrollHint:= TRUE;
	fParserHints:= TRUE; // Editor hints
	fShowFunctionTip:= TRUE;
	fTrimTrailingSpaces:= FALSE;

	// Caret
	fInsertCaret:= 0;
	fOverwriteCaret:= 3;
	fMatch := TRUE;

	// Margin
	fMarginVis:= TRUE;
	fMarginSize:= 80;
	fMarginColor:= cl3DLight;

	// Misc.
	fUseSyn:= TRUE;
	//last ; is for files with no extension
	//which should be treated as cpp header files
	fSynExt:= 'c;cpp;h;hpp;cc;cxx;cp;hp;rh;fx;inl;;';
	fHighCurrLine:= TRUE;
	fHighColor:= $FFFFCC; // Light Turquoise
	fTabSize:= 4;

	// Display
	fFont.name:= 'Courier New';
	fFont.Size:= 10;

	// Display #2
	fShowGutter:= TRUE;
	fGutterAuto:= TRUE;
	fCustomGutter:= FALSE;
	fLineNumbers:= TRUE;
	fFirstisZero:= FALSE;
	fLeadZero:= FALSE;
	fGutterFont.Name:= 'Courier New';
	fGutterFont.Size:= 10;
	fGutterSize:= 1;

	// Autosave
	fEnableAutoSave := FALSE;
	Interval := 10;
	fSaveType := 0;

	// Symbol completion
	fBraceComplete := TRUE;
	fParentheseComplete := TRUE;
	fIncludeComplete := TRUE;
	fCommentComplete := FALSE;
	fArrayComplete := TRUE;
	fCompleteSymbols := TRUE;
end;

procedure TdevEditor.AssignEditor(editor : TSynEdit;const FileName : AnsiString);
var
	pt: TPoint;
begin
	with Editor do begin
		BeginUpdate;
		try
			TabWidth:= fTabSize;
			Font.Assign(fFont);
			with Gutter do begin
				Font.Assign(fGutterFont);
				DigitCount:= fGutterSize;
				Visible:= fShowGutter;
				AutoSize:= fGutterAuto;
				ShowLineNumbers:= fLineNumbers;
				LeadingZeros:= fLeadZero;
				ZeroStart:= fFirstisZero;

				// Select a highlighter
				Highlighter := dmMain.GetHighlighter(FileName);

				// Set gutter color
				if Assigned(Highlighter) then begin
					StrtoPoint(pt, fSyntax.Values[cGut]);
					Color:= pt.x;
					Font.Color:= pt.y;
				end else begin // editor not colored, pick defaults
					Color:= clBtnFace;
					Font.Color:= clBlack;
				end;
			end;

			// Set selection color
			if Assigned(Highlighter) then begin
				StrtoPoint(pt, devEditor.Syntax.Values[cSel]);
				SelectedColor.Background:= pt.X;
				SelectedColor.Foreground:= pt.Y;
			end else begin // editor not colored, pick defaults
				SelectedColor.Background:= clNavy;
				SelectedColor.Foreground:= clWhite;
			end;

			// Set folding bar color
			if Assigned(Highlighter) then begin
				StrtoPoint(pt, devEditor.Syntax.Values[cFld]);
				CodeFolding.FolderBarLinesColor := pt.y;
			end else begin // editor not colored, pick defaults
				CodeFolding.FolderBarLinesColor := clBlack;
			end;

			if fMarginVis then
				RightEdge:= fMarginSize
			else
				RightEdge:= 0;

			RightEdgeColor:= fMarginColor;

			InsertCaret:= TSynEditCaretType(fInsertCaret);
			OverwriteCaret:= TSynEditCaretType(fOverwriteCaret);

			ScrollHintFormat:= shfTopToBottom;

			if HighCurrLine and Assigned(Highlighter) then
				ActiveLineColor := HighColor
			else
				ActiveLineColor := clNone;

			Options := [
				eoAltSetsColumnMode, eoDisableScrollArrows,
				eoDragDropEditing, eoDropFiles, eoKeepCaretX, eoTabsToSpaces,
				eoRightMouseMovesCursor, eoScrollByOneLess, eoAutoSizeMaxScrollWidth
			];

			//Optional synedit options in devData
			if fAutoIndent then
				Options := Options + [eoAutoIndent];
			if fEHomeKey then
				Options := Options + [eoEnhanceHomeKey];
			if fGroupUndo then
				Options := Options + [eoGroupUndo];
			if fHalfPage then
				Options := Options + [eoHalfPageScroll];
			if fShowScrollbars then
				Options := Options + [eoHideShowScrollbars];
			if fPastEOF then
				Options := Options + [eoScrollPastEOF];
			if fPastEOL then
				Options := Options + [eoScrollPastEOL];
			if fShowScrollHint then
				Options := Options + [eoScrollHintFollows,eoShowScrollHint];
			if fSmartTabs then
				Options := Options + [eoSmartTabs];
			if fSmartTabs then
				Options := Options + [eoSmartTabDelete];
			if fUseTabs then
				Options := Options - [eoTabsToSpaces];
			if fSpecialChar then
				Options := Options + [eoShowSpecialChars];
			if fTrimTrailingSpaces then
				Options := Options + [eoTrimTrailingSpaces];
		finally
			EndUpdate;
		end;
	end;
end;

constructor TdevCodeCompletion.Create;
begin
	inherited Create;
	fCacheFiles:=TStringList.Create;
	SettoDefaults;
	LoadSettings;
end;

destructor TdevCodeCompletion.Destroy;
begin
  fCacheFiles.Free;
end;

procedure TdevCodeCompletion.LoadSettings;
begin
  devData.LoadObject(Self,'CodeCompletion');
end;

procedure TdevCodeCompletion.SaveSettings;
begin
  devData.SaveObject(Self,'CodeCompletion');
end;

procedure TdevCodeCompletion.SettoDefaults;
begin
	fWidth:=320;
	fHeight:=240;
	fDelay:=400;
	fBackColor:=clWindow;
	fEnabled:=True;
	fUseCacheFiles:=False;
end;

{ TdevClassBrowsing }

constructor TdevClassBrowsing.Create;
begin
	inherited Create;
	SettoDefaults;
	LoadSettings;
end;

procedure TdevClassBrowsing.LoadSettings;
begin
  devData.LoadObject(Self,'ClassBrowsing');
end;

procedure TdevClassBrowsing.SaveSettings;
begin
  devData.SaveObject(Self,'ClassBrowsing');
end;

procedure TdevClassBrowsing.SettoDefaults;
begin
	fEnabled:=True;
	fCBViewStyle:=0;

	fParseLocalHeaders:=False;
	fParseGlobalHeaders:=False;
	fShowFilter:=0;
	fUseColors:=True;
	fShowInheritedMembers:=False;
end;

{ TdevCVSHandler }

constructor TdevCVSHandler.Create;
begin
 inherited Create;
 fRepositories:=TStringList.Create;
 SettoDefaults;
 LoadSettings;
end;

destructor TdevCVSHandler.Destroy;
begin
  fRepositories.Free;
end;

procedure TdevCVSHandler.LoadSettings;
begin
  devData.LoadObject(Self,'CVSHandler');
end;

procedure TdevCVSHandler.SaveSettings;
begin
  devData.SaveObject(Self,'CVSHandler');
end;

procedure TdevCVSHandler.SettoDefaults;
begin
   fExecutable:='cvs.exe';
   fCompression:=9;
   fUseSSH:=True;
end;

{ TdevExternalPrograms }

function TdevExternalPrograms.AddProgram(ext, prog: AnsiString): integer;
var
  idx: integer;
begin
  if ext='' then begin
    Result:=-1;
    Exit;
  end;

  idx:=AssignedProgram(ext);
  if idx=-1 then
    Result:=fPrograms.Add(ext+'='+prog)
  else begin
    fPrograms.Values[fPrograms.Names[idx]]:=prog;
    Result:=idx;
  end;
end;

function TdevExternalPrograms.AssignedProgram(const ext: AnsiString): integer;
var
	I: integer;
begin
	Result:=-1;
	for I:=0 to fPrograms.Count-1 do
		if SameText(fPrograms.Names[I],ext) then begin
			Result:=I;
			Break;
		end;
end;

constructor TdevExternalPrograms.Create;
begin
 inherited Create;
 fPrograms:=TStringList.Create;
 SettoDefaults;
 LoadSettings;
end;

destructor TdevExternalPrograms.Destroy;
begin
  fPrograms.Free;
end;

function TdevExternalPrograms.GetProgramName(Index: integer): AnsiString;
begin
  Result:=fPrograms.Values[fPrograms.Names[Index]];
end;

procedure TdevExternalPrograms.LoadSettings;
begin
  devData.LoadObject(Self,'ExternalPrograms');
end;

procedure TdevExternalPrograms.SaveSettings;
begin
  devData.SaveObject(Self,'ExternalPrograms');
end;

procedure TdevExternalPrograms.SetToDefaults;
begin
  inherited;

end;

end.
