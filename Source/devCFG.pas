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

unit devCFG;

interface

uses
  Dialogs, Windows, Classes, Graphics, SynEdit, editor, CFGData, IniFiles, ProjectTypes, Math, ShellAPI, ShlObj,
  ComCtrls, System.UITypes, Vcl.Themes, SynEditTextBuffer, System.SysUtils;

const
  BoolValYesNo: array[boolean] of String = ('No', 'Yes');

type
  // the comments are an example of the record
  PCompilerOption = ^TCompilerOption;
  TCompilerOption = record
    Name: integer; // language table index of "Generate debugging info"
    Section: integer; // language table index of "C options"
    IsC: boolean;
    IsCpp: boolean; // True (C++ option?) - can be both C and C++ option...
    IsLinker: boolean; // Is it a linker param
    Value: Integer; // True
    Setting: String; // "-g3"
    Choices: TStringList; // replaces "Yes/No" standard choices (max 30 different choices)
  end;

  TdevCompilerSet = class(TPersistent)
  private
    // Executables, most are hardcoded
    fgccName: String;
    fgppName: String;
    fmakeName: String;
    fgdbName: String;
    fwindresName: String;
    fgprofName: String;

    // Directories, mostly hardcoded too
    fBinDir: TStringList;
    fCDir: TStringList;
    fCppDir: TStringList;
    fLibDir: TStringList;

    // Misc. properties
    fDumpMachine: String; // "x86_64-w64-mingw32", "mingw32" etc
    fVersion: String; // "4.7.1"
    fType: String; // "TDM-GCC", "MinGW"
    fName: String; // "TDM-GCC 4.7.1 Release"
    fFolder: String; // TDM-GCC-64, MinGW64, MinGW32
    fDefInclude: TStringList; // default include dir
    fDefines: TStringList; // list of predefined constants

    // User settings
    fCompAdd: boolean;
    fLinkAdd: boolean;
    fCompOpt: String;
    flinkOpt: String;

    // Options
    fOptions: TList;

    // Initialization
    procedure SetProperties(const BinDir, BinFile: String);
    procedure SetExecutables;
    procedure SetDirectories;
    procedure SetUserInput;
    procedure SetOptions;

    // Converts to and from memory format
    function GetINIOptions: TCompilerOptionsValues;
    procedure SetINIOptions(const value: TCompilerOptionsValues);

    // Validation
    function Validate: boolean; // returns true if valid
    function ValidateDirs: boolean; // idem
    function ValidateExes: boolean; // idem
  public
    constructor Create; overload; // create empty shell
    constructor Create(const CompilerFolder: String); overload; // create, and let if configure itself
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    // Clearing and saving to disk is done by TdevCompilerSet
    procedure Clear;

    // Option utils
    procedure AddOption(Name, Section: integer; IsC, IsCpp, IsLinker: boolean; Value: integer; const Setting:
      String; Choices: TStringList);
    function GetOption(const Option: String): Integer;
    function FindOption(const Option: String; var opt: PCompilerOption; var Index: integer): boolean;
    procedure SetOption(const Option: String; Value: Integer); overload;
    procedure SetOption(Option: PCompilerOption; Value: Integer); overload;

    // Obtain response from compiler
    function GetCompilerOutput(const BinDir, BinFile, Input: String): String;

    // Executables
    property gccName: String read fgccName write fgccName;
    property gppName: String read fgppName write fgppName;
    property makeName: String read fmakeName write fmakeName;
    property gdbName: String read fgdbName write fgdbName;
    property windresName: String read fwindresName write fwindresName;
    property gprofName: String read fgprofName write fgprofName;

    // Directories
    property BinDir: TStringList read fBinDir write fBinDir;
    property CDir: TStringList read fCDir write fCDir;
    property CppDir: TStringList read fCppDir write fCppDir;
    property LibDir: TStringList read fLibDir write fLibDir;

    // Properties
    property Name: String read fName write fName;
    property DefInclude: TStringList read fDefInclude;
    property Defines: TStringList read fDefines;

    // Options
    property Options: TList read fOptions write fOptions;
    property INIOptions: TCompilerOptionsValues read GetINIOptions write SetINIOptions;

    // User settings
    property AddtoComp: boolean read fCompAdd write fCompAdd;
    property AddtoLink: boolean read fLinkAdd write fLinkAdd;
    property CompOpts: String read fCompOpt write fCompOpt;
    property LinkOpts: String read fLinkOpt write fLinkOpt;
  end;

  // compiler-set configuration
  TdevCompilerSets = class(TPersistent)
  private
    fList: TList; // list of TdevCompilerSet
    fDefaultIndex: integer;
    function GetCompilationSet: TdevCompilerSet;
    function GetCompilationSetIndex: Integer;
    function GetDefaultSet: TdevCompilerSet; // returns regular current set
  public
    constructor Create;
    destructor Destroy; override;

    // sigle set management in memory
//    procedure OnCompilerSetChanged(OldSet, NewSet: TdevCompilerSet);
    procedure LoadSet(Index: integer; const SetName: String = '');
    procedure SaveSet(Index: integer);
    function AddSet: TdevCompilerSet; overload; // add empty shell
    function AddSet(const Folder: String): TdevCompilerSet; overload; // add empty shell, let it configure itself
    function AddSet(compilerset: TdevCompilerSet): TdevCompilerSet; overload; // add deep copy
    function GetSet(Index: integer): TdevCompilerSet;
    procedure DeleteSet(Index: integer);
    function Count: integer;

    // all set management on disk
    procedure LoadSets; // load everything from disk
    procedure SaveSets; // save everything to disk
    procedure SaveSetList; // only save the CompilerSets section
    procedure FindSets;
    procedure ClearSets;

    // access to list
    property CompilationSet: TdevCompilerSet read GetCompilationSet;
    property CompilationSetIndex: Integer read GetCompilationSetIndex;
    property DefaultSet: TdevCompilerSet read GetDefaultSet;
    property DefaultSetIndex: Integer read fDefaultIndex write fDefaultIndex;
    property Sets[index: integer]: TdevCompilerSet read GetSet; default;
  end;

  // code-completion window size and other config
  TdevCodeCompletion = class(TPersistent)
  private
    fWidth: integer;
    fHeight: integer;
    fDelay: integer;
    fBackColor: integer;
    fEnabled: boolean;
    fParseLocalHeaders: boolean;
    fParseGlobalHeaders: boolean;
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
    property ParseLocalHeaders: boolean read fParseLocalHeaders write fParseLocalHeaders;
    property ParseGlobalHeaders: boolean read fParseGlobalHeaders write fParseGlobalHeaders;
  end;

  // class-browsing view style
  TdevClassBrowsing = class(TPersistent)
  private
    fShowFilter: integer;
    fShowInheritedMembers: boolean;
  public
    constructor Create;
    procedure SettoDefaults;
    procedure SaveSettings;
    procedure LoadSettings;
  published
    property ShowFilter: integer read fShowFilter write fShowFilter;
    property ShowInheritedMembers: boolean read fShowInheritedMembers write fShowInheritedMembers;
  end;

  // Options for AStyle
  TdevFormatter = class(TPersistent)
  private
    fBracketStyle: Integer;
    fIndentStyle: Integer;
    fTabWidth: Integer;
    fMaxLineLength: Integer;
    fModifyMaxLineLength: Boolean;
    fIndentClasses: Boolean;
    fIndentSwitches: Boolean;
    fIndentCases: Boolean;
    fIndentNamespaces: Boolean;
    fIndentLabels: Boolean;
    fIndentPreprocessor: Boolean;
    fFullCommand: String; // includes customizations
    fAStyleDir: String;
    fAStyleFile: String;
  public
    constructor Create;
    procedure SettoDefaults;
    procedure SaveSettings;
    procedure LoadSettings;
    function Validate: Boolean; // check if AStyle.exe can be found
    function FormatMemory(Editor: TEditor; const OverrideCommand: String): String; // apply formatting
    function FormatFile(const FileName, OverrideCommand: String): String; // apply formatting
    function GetVersion: String;
    function GetFullCommand: String;
  published
    property BracketStyle: Integer read fBracketStyle write fBracketStyle;
    property IndentStyle: Integer read fIndentStyle write fIndentStyle;
    property TabWidth: Integer read fTabWidth write fTabWidth;
    property MaxLineLength: Integer read fMaxLineLength write fMaxLineLength;
    property ModifyMaxLineLength: Boolean read fModifyMaxLineLength write fModifyMaxLineLength;
    property IndentClasses: Boolean read fIndentClasses write fIndentClasses;
    property IndentSwitches: Boolean read fIndentSwitches write fIndentSwitches;
    property IndentCases: Boolean read fIndentCases write fIndentCases;
    property IndentNamespaces: Boolean read fIndentNamespaces write fIndentNamespaces;
    property IndentLabels: Boolean read fIndentLabels write fIndentLabels;
    property IndentPreprocessor: Boolean read fIndentPreprocessor write fIndentPreprocessor;
    property FullCommand: String read fFullCommand write fFullCommand;
    property AStyleDir: String read fAStyleDir write fAStyleDir;
    property AStyleFile: String read fAStyleFile write fAStyleFile;
  end;

  // List of programs to use for unknown file extensions
  TdevExternalPrograms = class(TPersistent)
  private
    fDummy: boolean;
    fPrograms: TStrings;
    function GetProgramName(Index: integer): String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SettoDefaults;
    procedure SaveSettings;
    procedure LoadSettings;

    property ProgramName[Index: integer]: String read GetProgramName;
    function AssignedProgram(const ext: String): integer;
    function AddProgram(ext, prog: String): integer;
  published
    property Dummy: boolean read fDummy write fDummy;
    property Programs: TStrings read fPrograms write fPrograms;
  end;

  // global directories
  TdevDirs = class(TPersistent)
  private
    fThemes: String; // Themes Directory
    fIcons: String; // Icon Library
    fHelp: String; // Help
    fLang: String; // Language
    fTemp: String; // Templates
    fDefault: String; // user defined default
    fExec: String; // dev-c start
    fConfig: String; // config files directory
    fOldPath: String; // Enviroment Path at program start
  public
    constructor Create;
    procedure SettoDefaults;
    procedure SaveSettings;
    procedure LoadSettings;

    // don't bother to remember, read on startup
    property OriginalPath: String read fOldPath write fOldPath;
    property Exec: String read fExec write fExec;
    property Config: String read fConfig write fConfig;
  published
    property Default: String read fDefault write fDefault;
    property Help: String read fHelp write fHelp;
    property Icons: String read fIcons write fIcons;
    property Lang: String read fLang write fLang;
    property Templates: String read fTemp write fTemp;
    property Themes: String read fThemes write fThemes;
  end;

  // editor options -- syntax, synedit options, etc...
  TdevEditor = class(TPersistent)
  private
    fUseSyn: boolean; // use syntax highlighting
    fSynExt: String; // semi-colon seperated list of highlight ext's
    fFont: TFont; // Editor Font
    fShowLigatures: Boolean; //Font ligatures
    fGutterFont: TFont; // Gutter font
    fInsertCaret: integer; // Editor insert caret
    fOverwriteCaret: integer; // Editor overwrite caret
    fTabSize: integer; // Editor Tab Size
    fGutterSize: integer; // Width of Left margin gutter
    fMarginSize: integer; // Width of right margin

    fCustomGutter: boolean; // Use Selected Gutter font
    fGutterAuto: boolean; // Gutter Auto Sizes
    fShowGutter: boolean; // Show Left gutter in editor
    fLineNumbers: boolean; // Show Line Numbers
    fLeadZero: boolean; // Show leading zero's in line nums
    fFirstisZero: boolean; // First line is zero

    fMarginVis: boolean; // Toggle right margin line

    fShowScrollHint: boolean; // Show line number when scrolling
    fShowScrollbars: boolean; // Show Scroll bars
    fHalfPage: boolean; // PgUp/PgDn move half a page

    fPastEOF: boolean; // Cursor moves past end of file
    fPastEOL: boolean; // Cursor moves past end of lines
    fFindText: boolean; // Text at cursor defaults in find dialog
    fEHomeKey: boolean; // Home key like visual studio
    fGroupUndo: boolean; // treat same undo's as single undo
    fInsDropFiles: boolean; // Insert files when drag/dropped else open
    fInsertMode: boolean; // Editor defaults to insert mode
    fAutoIndent: boolean; // Auto-indent code lines
    fAddIndent: boolean; // Add indent when typing { and :
    fSmartTabs: boolean; // Tab to next no whitespace char
    fSpecialChar: boolean; // special line characters visible
    fUseTabs: boolean; // convert tabs to spaces
    fShowFunctionTip: boolean; // show function tip
    fMarginColor: TColor; // Color of right margin
    fSyntax: TStrings; // Holds attributes settings
    fDefaultCode: boolean; // Insert Default Source Code into new files
    fParserHints: boolean; // Show parser's hint for the word under the cursor
    fMatch: boolean; // Highlight matching parenthesis
    fHighCurrLine: boolean; // Highlight current line
    fHighColor: TColor; // Color of current line when highlighted
    fTrimTrailingSpaces: boolean;

    // Autosave
    fEnableAutoSave: boolean;
    fInterval: integer;
    fAutoSaveFilter: integer;
    fAutoSaveMode: integer;

    // Symbol completion
    fBraceComplete: boolean;
    fParentheseComplete: boolean;
    fIncludeComplete: boolean;
    fCommentComplete: boolean;
    fArrayComplete: boolean;
    fSingleQuoteComplete: boolean;
    fDoubleQuoteComplete: boolean;
    fCompleteSymbols: boolean;
    fDeleteSymbolPairs: boolean;
    fNewDocEncoding: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SettoDefaults;
    procedure SaveSettings;
    procedure LoadSettings;

    procedure AssignEditor(editor: TSynEdit; const FileName: String);

    function GetNewDocEncoding: TEncoding;

  published
    property AutoIndent: boolean read fAutoIndent write fAutoIndent;
    property AddIndent: boolean read fAddIndent write fAddIndent;
    property InsertMode: boolean read fInsertMode write fInsertMode;
    property UseTabs: boolean read fUseTabs write fUseTabs;
    property SmartTabs: boolean read fSmartTabs write fSmartTabs;
    property GroupUndo: boolean read fGroupUndo write fGroupUndo;
    property EHomeKey: boolean read fEHomeKey write fEHomeKey;
    property PastEOF: boolean read fPastEOF write fPastEOF;
    property PastEOL: boolean read fPastEOL write fPastEOL;
    property FindText: boolean read fFindText write fFindText;
    property Scrollbars: boolean read fShowScrollbars write fShowScrollbars;
    property HalfPageScroll: boolean read fHalfPage write fHalfPage;
    property ScrollHint: boolean read fShowScrollHint write fShowScrollHint;
    property SpecialChars: boolean read fSpecialChar write fSpecialChar;
    property ShowFunctionTip: boolean read fShowFunctionTip write fShowFunctionTip;
    property TrimTrailingSpaces: boolean read fTrimTrailingSpaces write fTrimTrailingSpaces;
    property NewDocEncoding: string read fNewDocEncoding write fNewDocEncoding;

    property TabSize: integer read fTabSize write fTabSize;
    property MarginVis: boolean read fMarginVis write fMarginVis;
    property MarginSize: integer read fMarginSize write fMarginSize;
    property MarginColor: TColor read fMarginColor write fMarginColor;
    property InsertCaret: integer read fInsertCaret write fInsertCaret;
    property OverwriteCaret: integer read fOverwriteCaret write fOverwriteCaret;
    property InsDropFiles: boolean read fInsDropFiles write fInsDropFiles;
    property Font: TFont read fFont write fFont;
    property ShowLigatures: Boolean read fShowLigatures write fShowLigatures;

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
    property SyntaxExt: String read fSynExt write fSynExt;
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
    property AutoSaveFilter: integer read fAutoSaveFilter write fAutoSaveFilter;
    property AutoSaveMode: integer read fAutoSaveMode write fAutoSaveMode;

    // Brace completion
    property BraceComplete: boolean read fBraceComplete write fBraceComplete;
    property ParentheseComplete: boolean read fParentheseComplete write fParentheseComplete;
    property IncludeComplete: boolean read fIncludeComplete write fIncludeComplete;
    property CommentComplete: boolean read fCommentComplete write fCommentComplete;
    property ArrayComplete: boolean read fArrayComplete write fArrayComplete;
    property SingleQuoteComplete: boolean read fSingleQuoteComplete write fSingleQuoteComplete;
    property DoubleQuoteComplete: boolean read fDoubleQuoteComplete write fDoubleQuoteComplete;
    property CompleteSymbols: boolean read fCompleteSymbols write fCompleteSymbols;
    property DeleteSymbolPairs: boolean read fDeleteSymbolPairs write fDeleteSymbolPairs;
  end;

  TWindowState = class(TPersistent)
  private
    fLeft: integer;
    fTop: integer;
    fRight: integer;
    fBottom: integer;
    fShowCmd: integer;
    fFlags: integer;
    fStruct: TWindowPlacement; // don't store on disk!
  public
    constructor Create;
    procedure GetPlacement(Source: HWND);
    procedure SetPlacement(Destination: HWND);
  published
    property Left: integer read fLeft write fLeft; // TWindowPlacement parts
    property Top: integer read fTop write fTop;
    property Right: integer read fRight write fRight;
    property Bottom: integer read fBottom write fBottom;
    property ShowCmd: integer read fShowCmd write fShowCmd;
    property Flags: integer read fFlags write fFlags;
  end;

  // master option object -- contains program globals
  TdevData = class(TConfigData)
  private
    fLang: String; // Language file
    fTheme: String; // Theme file
    fFindCols: String; // Find Column widths (comma sep)
    fCompCols: String; // Compiler Column Widths (comma sep)
    fMsgTabs: TTabPosition; // Editor Tabs
    fMinOnRun: boolean; // Minimize IDE on run
    fMRUMax: integer; // Max number of files in history list
    fBackup: boolean; // Create backup files
    fAutoOpen: integer; // Auto Open Project Files Style
    fShowLeftPages: boolean; // Show the left page control
    fProjectWidth: integer; // Width of project browser
    fLeftActivePage: integer; // 0 == project, 1 == class, 2 == debug
    fOutputHeight: integer; // the height of the output window
    fStatusbar: boolean; // Statusbar Visible
    fFullScr: boolean; // IDE is Full screen
    fShowBars: boolean; // Show toolbars in FullScreen mode
    fMultiLineTab: boolean; // Show multiline tabs
    fDefCpp: boolean; // Default to C++ project (compile with g++)
    fFirst: boolean; // first run of dev-c
    fSplash: String; // user selected splash screen
    fdblFiles: boolean; // double click opens files out of project manager
    fLangChange: boolean; // flag for language change
    fthemeChange: boolean; // did the theme change?
    fInterfaceFont: String; // UI font
    fInterfaceFontSize: integer; // UI font size
    fConsolePause: boolean; // pause console program after return
    fShortenCompPaths: boolean; // shorten compiler paths in compiler log
    fStyle: integer; // Delphi Style
    fStyleChange: boolean;

    // TWindowPlacement parts
    fWindowState: TWindowState;
    fReportWindowState: TWindowState;
    fProjectWindowState: TWindowState;

    fToolbarMain: boolean; // These ones follow the enable/x-offset/y-offset patern
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
    fToolbarCompilers: boolean;
    fToolbarCompilersX: integer;
    fToolbarCompilersY: integer;

    // file associations (see FileAssocs.pas)
    fAssociateC: boolean;
    fAssociateCpp: boolean;
    fAssociateCxx: boolean;
    fAssociateH: boolean;
    fAssociateHpp: boolean;
    fAssociateHxx: boolean;
    fAssociateDev: boolean;
    fAssociateRc: boolean;
    fAssociateTemplate: boolean;
    fCheckAssocs: boolean;

    // More misc stuff
    fShowTipsOnStart: boolean;
    fLastTip: integer;
    fShowProgress: boolean; // Show progress window during compile
    fAutoCloseProgress: boolean; // Auto close progress bar window after compile

    // Printer
    fPrintColors: boolean; // print colors
    fPrintHighlight: boolean;
    fPrintWordWrap: boolean;
    fPrintLineNumbers: boolean;
    fPrintLineNumbersMargins: boolean;

    // Some debug options
    fWatchHint: boolean; // watch variable under mouse
    fUseATTSyntax: boolean;
    fShowCPUSignal: boolean; // show CPU window on signal
    fCPURegisterCol1: integer; // width of column 1
    fCPURegisterCol2: integer; // width of column 1
    fCPURegisterCol3: integer; // width of column 1

    // Search preferences
    fCaseSensitive: boolean;
    fWholewords: boolean;
    fPromptReplace: boolean;
    fScopeIsSelected: boolean; // false == Global
    fOriginEntireScope: boolean; // false == from cursor
    fSearchWhere: integer; // 0 == project files, 1 == open files, 2 == current file
    fDirBackward: boolean;

    // Floating windows
    fProjectFloat: boolean;
    fMessageFloat: boolean;

    FIsPortable: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SettoDefaults; override;
    property LangChange: boolean read fLangChange write fLangChange;
    property ThemeChange: boolean read fThemeChange write fThemeChange;
    property IsPortable: Boolean read FIsPortable write FIsPortable;
    property StyleChange: boolean read fStyleChange write fStyleChange;
  published
    property Language: String read fLang write fLang;
    property Theme: String read fTheme write fTheme;
    property First: boolean read fFirst write fFirst;
    property Splash: String read fSplash write fSplash;
    property MRUMax: integer read fMRUMax write fMRUMax;
    property ShortenCompPaths: boolean read fShortenCompPaths write fShortenCompPaths;
    property Style: integer read FStyle write FStyle;

    //Execution
    property MinOnRun: boolean read fMinOnRun write fMinOnRun;
    property ConsolePause: boolean read fConsolePause write fConsolePause;
    property BackUps: boolean read fBackup write fBackup;
    property AutoOpen: integer read fAutoOpen write fAutoOpen;

    //Windows
    property MsgTabs: TTabPosition read fMsgTabs write fMsgTabs;
    property InterfaceFont: String read fInterfaceFont write fInterfaceFont;
    property InterfaceFontSize: integer read fInterfaceFontSize write fInterfaceFontSize;
    property ShowBars: boolean read fShowbars write fShowbars;
    property MultiLineTab: boolean read fMultiLineTab write fMultiLineTab;

    // TWindowPlacement parts
    property WindowState: TWindowState read fWindowState write fWindowState;
    property ReportWindowState: TWindowState read fReportWindowState write fReportWindowState;
    property ProjectWindowState: TWindowState read fProjectWindowState write fProjectWindowState;

    //Running Status Options
    property DefCpp: boolean read fDefCpp write fDefCpp;
    property OutputHeight: integer read fOutputHeight write fOutputHeight;
    property ShowLeftPages: boolean read fShowLeftPages write fShowLeftPages;
    property LeftActivePage: integer read fLeftActivePage write fLeftActivePage;
    property ProjectWidth: integer read fProjectWidth write fProjectWidth;
    property Statusbar: boolean read fStatusbar write fStatusbar;
    property FullScreen: boolean read fFullScr write fFullScr;
    property FindCols: String read fFindCols write fFindCols;
    property CompCols: String read fCompCols write fCompCols;

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
    property ToolbarCompilers: boolean read fToolbarCompilers write fToolbarCompilers;
    property ToolbarCompilersX: integer read fToolbarCompilersX write fToolbarCompilersX;
    property ToolbarCompilersY: integer read fToolbarCompilersY write fToolbarCompilersY;

    // file associations
    property AssociateC: boolean read fAssociateC write fAssociateC;
    property AssociateCpp: boolean read fAssociateCpp write fAssociateCpp;
    property AssociateCxx: boolean read fAssociateCxx write fAssociateCxx;
    property AssociateH: boolean read fAssociateH write fAssociateH;
    property AssociateHpp: boolean read fAssociateHpp write fAssociateHpp;
    property AssociateHxx: boolean read fAssociateHxx write fAssociateHxx;
    property AssociateDev: boolean read fAssociateDev write fAssociateDev;
    property AssociateRc: boolean read fAssociateRc write fAssociateRc;
    property AssociateTemplate: boolean read fAssociateTemplate write fAssociateTemplate;
    property CheckAssocs: boolean read fCheckAssocs write fCheckAssocs;

    // tip of the day
    property ShowTipsOnStart: boolean read fShowTipsOnStart write fShowTipsOnStart;
    property LastTip: integer read fLastTip write fLastTip;

    // progress window
    property ShowProgress: boolean read fShowProgress write fShowProgress;
    property AutoCloseProgress: boolean read fAutoCloseProgress write fAutoCloseProgress;

    //  Printer
    property PrintColors: boolean read fPrintColors write fPrintColors;
    property PrintHighlight: boolean read fPrintHighlight write fPrintHighlight;
    property PrintWordWrap: boolean read fPrintWordWrap write fPrintWordWrap;
    property PrintLineNumbers: boolean read fPrintLineNumbers write fPrintLineNumbers;
    property PrintLineNumbersMargins: boolean read fPrintLineNumbersMargins write fPrintLineNumbersMargins;

    // General debugging options
    property WatchHint: boolean read fWatchHint write fWatchHint;
    property UseATTSyntax: boolean read fUseATTSyntax write fUseATTSyntax;
    property ShowCPUSignal: boolean read fShowCPUSignal write fShowCPUSignal;
    property CPURegisterCol1: integer read fCPURegisterCol1 write fCPURegisterCol1;
    property CPURegisterCol2: integer read fCPURegisterCol2 write fCPURegisterCol2;
    property CPURegisterCol3: integer read fCPURegisterCol3 write fCPURegisterCol3;

    // Search preferences
    property CaseSensitive: boolean read fCaseSensitive write fCaseSensitive;
    property Wholewords: boolean read fWholewords write fWholewords;
    property PromptReplace: boolean read fPromptReplace write fPromptReplace;
    property ScopeIsSelected: boolean read fScopeIsSelected write fScopeIsSelected;
    property OriginEntireScope: boolean read fOriginEntireScope write fOriginEntireScope;
    property SearchWhere: integer read fSearchWhere write fSearchWhere;
    property DirBackward: boolean read fDirBackward write fDirBackward;

    // Floating windows
    property ProjectFloat: boolean read fProjectFloat write fProjectFloat;
    property MessageFloat: boolean read fMessageFloat write fMessageFloat;
  end;

function devData: TdevData;

procedure CreateOptions;
procedure SaveOptions;
procedure DestroyOptions;
procedure RemoveOptionsDir(const Directory: String);

var
  devCompilerSets: TdevCompilerSets = nil;
  devDirs: TdevDirs = nil;
  devEditor: TdevEditor = nil;
  devCodeCompletion: TdevCodeCompletion = nil;
  devClassBrowsing: TdevClassBrowsing = nil;
  devExternalPrograms: TdevExternalPrograms = nil;
  devFormatter: TdevFormatter = nil;

  TDM_GCC_64_Folder: string = 'TDM-GCC-64';

implementation

uses
  MultiLangSupport, DataFrm, StrUtils, Forms, main, compiler, Controls, version, utils, SynEditMiscClasses,
  FileAssocs, TypInfo, DateUtils, Types, System.IOUtils, Vcl.ExtDlgs;

procedure CreateOptions;
var
  I: integer;
begin
  if not Assigned(devDirs) then
    devDirs := TdevDirs.Create;

  if not Assigned(devCompilerSets) then
    devCompilerSets := TdevCompilerSets.Create;

  // load available compiler sets on first run
  if devData.First then begin

    // Obtain list of default compilers
    devCompilerSets.FindSets;
    devCompilerSets.DefaultSetIndex := -1;

    // Pick a proper default
    if IsWindows64 then begin // TDM-GCC x64, MinGW32, ???
      for I := 0 to devCompilerSets.Count - 1 do begin
        if ContainsText(devCompilerSets[i].Name, 'TDM-GCC') then begin
          devCompilerSets.DefaultSetIndex := i;
          break;
        end;
      end;

      // Pick the 'release' build
      if (devCompilerSets.DefaultSetIndex = -1) and (devCompilerSets.Count > 0) then
        devCompilerSets.DefaultSetIndex := 0;

    end else begin // TDM-GCC x86, MinGW32, ???
      for I := 0 to devCompilerSets.Count - 1 do begin
        if ContainsText(devCompilerSets[i].Name, 'TDM-GCC') and ContainsText(devCompilerSets[i].Name, '32-bit') then
          begin
          devCompilerSets.DefaultSetIndex := i;
          break;
        end;
      end;

      // Again, pick the 'release' build
      if (devCompilerSets.DefaultSetIndex = -1) and (devCompilerSets.Count > 0) then
        devCompilerSets.DefaultSetIndex := 0; // pick any
    end;
    devCompilerSets.SaveSets; // save everything to disk
  end;

  if not assigned(devEditor) then
    devEditor := TdevEditor.Create;

  if not Assigned(devCodeCompletion) then
    devCodeCompletion := TdevCodeCompletion.Create;

  if not Assigned(devClassBrowsing) then
    devClassBrowsing := TdevClassBrowsing.Create;

  if not Assigned(devExternalPrograms) then
    devExternalPrograms := TdevExternalPrograms.Create;

  if not Assigned(devFormatter) then
    devFormatter := TdevFormatter.Create;
end;

procedure SaveOptions;
begin
  devData.SaveSelf;
  devDirs.SaveSettings;
  // devCompiler saving is done by CompOptForm
  devEditor.SaveSettings;
  devCodeCompletion.SaveSettings;
  devClassBrowsing.SaveSettings;
  devExternalPrograms.SaveSettings;
  devFormatter.SaveSettings;
end;

procedure DestroyOptions;
begin
  devData.Free;
  FreeAndNil(devDirs);
  FreeAndNil(devCompilerSets);
  FreeAndNil(devEditor);
  FreeAndNil(devCodeCompletion);
  FreeAndNil(devClassBrowsing);
  FreeAndNil(devExternalPrograms);
  FreeAndNil(devFormatter);
end;

procedure RemoveOptionsDir(const Directory: String);
var
  fostruct: SHFILEOPSTRUCT;
  DirFrom, DirTo: array[0..MAX_PATH] of char;
begin
  // Copy and delete
  FillChar(DirFrom, Sizeof(DirFrom), 0);
  StrPCopy(DirFrom, Directory);

  FillChar(DirTo, Sizeof(DirFrom), 0);
  StrPCopy(DirTo, ExcludeTrailingBackslash(Directory) + 'Backup' + pd);

  FillChar(fostruct, Sizeof(fostruct), 0);
  with fostruct do begin
    Wnd := 0;
    pFrom := @DirFrom;
    pTo := @DirTo;
    wFunc := FO_COPY;
    fFlags := FOF_ALLOWUNDO or FOF_SILENT or FOF_NOCONFIRMATION;
  end;
  SHFileOperation(fostruct);

  FillChar(fostruct, Sizeof(fostruct), 0);
  with fostruct do begin
    Wnd := 0;
    pFrom := @DirFrom;
    wFunc := FO_DELETE;
    fFlags := FOF_ALLOWUNDO or FOF_SILENT or FOF_NOCONFIRMATION;
  end;
  SHFileOperation(fostruct);
end;

var
  fdevDataSingleton: TdevData = nil;

function devData: TdevData;
begin
  if not Assigned(fdevDataSingleton) and not Application.Terminated then
    fdevDataSingleton := TdevData.Create;
  result := fdevDataSingleton; // assume constructor succeeded
end;

constructor TdevData.Create;
begin
  inherited Create;
  fWindowState := TWindowState.Create;
  fReportWindowState := TWindowState.Create;
  fProjectWindowState := TWindowState.Create;
  SettoDefaults;
end;

destructor TdevData.Destroy;
begin
  fWindowState.Free;
  fReportWindowState.Free;
  fProjectWindowState.Free;
  fdevDataSingleton := nil;
  inherited;
end;

procedure TdevData.SettoDefaults;
var
  osinfo: TOSVersionInfo;

  function getAssociation(I: integer): Boolean;
  begin
    Result := CheckFiletype('.' + Associations[I, 0],
      'DevCpp.' + Associations[I, 0],
      Associations[I, 1],
      'open',
      Application.Exename + ' "%1"');
  end;

begin
  fFirst := TRUE;
  fLang := 'English.lng';
  fFindCols := '75, 75, 120, 150';
  fCompCols := '75, 75, 120, 150';
  fMsgTabs := tpTop; // Top
  fMRUMax := 15;
  fMinOnRun := FALSE;
  fBackup := FALSE;
  fAutoOpen := 2; // Reopen
  fShowLeftPages := TRUE;
  fLeftActivePage := 0;
  fProjectWidth := 161;
  fOutputHeight := 183;
  fStatusbar := TRUE;
  fShowBars := FALSE;
  fMultiLineTab := TRUE;
  fDefCpp := TRUE;
  fdblFiles := FALSE;
  fConsolePause := TRUE;
  fShortenCompPaths := False;
  fStyle := 0;
  fTheme := 'NewLook';

  // TODO: retrieve directly from visual editor
  fToolbarMain := TRUE;
  fToolbarMainX := 11;
  fToolbarMainY := 2;
  fToolbarEdit := TRUE;
  fToolbarEditX := 196;
  fToolbarEditY := 2;
  fToolbarCompile := TRUE;
  fToolbarCompileX := 464;
  fToolbarCompileY := 2;
  fToolbarProject := TRUE;
  fToolbarProjectX := 373;
  fToolbarProjectY := 2;
  fToolbarSpecials := TRUE;
  fToolbarSpecialsX := 11;
  fToolbarSpecialsY := 30;
  fToolbarSearch := TRUE;
  fToolbarSearchX := 233;
  fToolbarSearchY := 2;
  fToolbarClasses := TRUE;
  fToolbarClassesX := 95;
  fToolbarClassesY := 30;
  fToolbarCompilers := TRUE;
  fToolbarCompilersX := 686;
  fToolbarCompilersY := 2;

  // Office 2007 / Vista support
  osinfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(osinfo);
  if (Screen.Fonts.IndexOf('Heebo') <> -1) and (osinfo.dwMajorVersion >= 6) then begin
    fInterfaceFontSize := 10;
    fInterfaceFont := 'Heebo';
  end else begin
    fInterfaceFontSize := 8;
    fInterfaceFont := 'Roboto';
  end;

  //read associations set by installer as defaults
  fAssociateC := getAssociation(0);
  fAssociateCpp := getAssociation(1);
  fAssociateCxx := getAssociation(2);
  fAssociateH := getAssociation(3);
  fAssociateHpp := getAssociation(4);
  fAssociateHxx := getAssociation(5);
  fAssociateDev := getAssociation(6);
  fAssociateRc := getAssociation(7);
  fAssociateTemplate := getAssociation(8);
  fCheckAssocs := false;

  fShowTipsOnStart := FALSE; // due to popular demand
  fLastTip := 0;
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
  fCPURegisterCol3 := 10;

  // Search stuff
  fCaseSensitive := false;
  fWholewords := false;
  fPromptReplace := false;
  fScopeIsSelected := false;
  fOriginEntireScope := false;
  fSearchWhere := 1;
  fDirBackward := false;

  // Floating windows
  fMessageFloat := false;
  fProjectFloat := false;

  FIsPortable := False;
end;

{ TWindowState }

constructor TWindowState.Create;
begin
  inherited Create;
  fStruct.length := sizeof(WINDOWPLACEMENT);
  flags := -1; // -1 means GetWindowPlacement isn't called yet
end;

procedure TWindowState.GetPlacement(Source: HWND);
begin
  if GetWindowPlacement(Source, @fStruct) then begin
    fShowCmd := fStruct.showCmd;
    fFlags := fStruct.flags;

    // Attempt to correct for Aero Snap. GetWindowPlacement ignores this functionality
    // Only do that if the window is not minimized/maximized
    if fStruct.showCmd = SW_SHOWNORMAL then
      GetWindowRect(Source, fStruct.rcNormalPosition);

    // Apply fixed aero values
    fLeft := fStruct.rcNormalPosition.Left;
    fTop := fStruct.rcNormalPosition.Top;
    fRight := fStruct.rcNormalPosition.Right;
    fBottom := fStruct.rcNormalPosition.Bottom;
  end;
end;

procedure TWindowState.SetPlacement(Destination: HWND);
begin
  if flags <> -1 then begin
    fStruct.rcNormalPosition.Left := fLeft;
    fStruct.rcNormalPosition.Top := fTop;
    fStruct.rcNormalPosition.Right := fRight;
    fStruct.rcNormalPosition.Bottom := fBottom;
    fStruct.showCmd := fShowCmd;
    fStruct.flags := fFlags;
    SetWindowPlacement(Destination, @fStruct);
  end;
end;

{ TdevCompilerSet }

constructor TdevCompilerSet.Create;
begin
  inherited Create;

  // Create stuff on heap
  fBinDir := TStringList.Create;
  fCDir := TStringList.Create;
  fCppDir := TStringList.Create;
  fLibDir := TStringList.Create;
  fOptions := TList.Create;

  // Misc.
  fDefInclude := TStringList.Create;
  fDefines := TStringList.Create;

  // Do not set properties yet, as we don't know where to look for gcc.exe

  // Add default options list though, no info needed
  SetOptions;
end;

constructor TdevCompilerSet.Create(const CompilerFolder: String);
begin
  inherited Create;

  // Create stuff on heap
  fBinDir := TStringList.Create;
  fCDir := TStringList.Create;
  fCppDir := TStringList.Create;
  fLibDir := TStringList.Create;
  fOptions := TList.Create;

  // Misc.
  fDefInclude := TStringList.Create;
  fDefines := TStringList.Create;

  // Set properties, assume bin\gcc.exe exists (it is our helper)
  SetProperties(CompilerFolder + pd + 'bin', GCC_PROGRAM);

  // Depending on properties, set default exes
  SetExecutables;

  // Again depending on properties, set directories
  SetDirectories;

  // Same story, user input
  SetUserInput;

  // load options
  SetOptions;
end;

destructor TdevCompilerSet.Destroy;
var
  I: integer;
begin
  // delete options
  for I := 0 to fOptions.Count - 1 do begin
    if Assigned(PCompilerOption(fOptions[I])^.Choices) then
      PCompilerOption(fOptions[I])^.Choices.Free;
    Dispose(PCompilerOption(fOptions[I]));
  end;
  fOptions.Free;

  // delete directories
  fBinDir.Free;
  fCDir.Free;
  fCppDir.Free;
  fLibDir.Free;

  // Etc.
  fDefInclude.Free;
  fDefines.Free;

  inherited;
end;

procedure TdevCompilerSet.Assign(Source: TPersistent);
var
  input: TdevCompilerSet;
begin
  input := TdevCompilerSet(Source);

  // Executables
  fgccName := input.fgccName;
  fgppName := input.fgppName;
  fmakeName := input.fmakeName;
  fgdbName := input.fgdbName;
  fwindresName := input.windresName;
  fgprofName := input.fgprofName;

  // Directories
  fBinDir.Assign(input.fBinDir);
  fCDir.Assign(input.fCDir);
  fCppDir.Assign(input.fCppDir);
  fLibDir.Assign(input.fLibDir);

  // Misc. properties
  fDumpMachine := input.fDumpMachine;
  fVersion := input.fVersion;
  fType := input.fType;
  fName := input.fName;
  fFolder := input.fFolder;
  fDefInclude.Assign(input.fDefInclude);
  fDefines.Assign(input.fDefines);

  // User input
  fCompAdd := input.fCompAdd;
  fCompOpt := input.fCompOpt;
  fLinkAdd := input.fLinkAdd;
  flinkOpt := input.fLinkOpt;

  // Option list
  INIOptions := input.INIOptions;
end;

procedure TdevCompilerSet.SetProperties(const BinDir, BinFile: String);
var
  output {,DummyFileName,Cmd}: String;
  DelimPos1, DelimPos2 {,DummyHandle}: integer;
begin
  // GCC binaries not found../
  if not FileExists(BinDir + pd + BinFile) then
    Exit;

  // Obtain version number and compiler distro etc
  output := GetCompilerOutput(BinDir + pd, BinFile, '-v');
  DelimPos1 := Pos('gcc version ', output);
  if DelimPos1 = 0 then
    Exit; // unknown binary

  // Find version number
  Inc(DelimPos1, Length('gcc version '));
  DelimPos2 := DelimPos1;
  while (DelimPos2 <= Length(output)) and not CharInSet(output[DelimPos2], [#0..#32]) do
    Inc(DelimPos2);
  fVersion := Copy(output, DelimPos1, DelimPos2 - DelimPos1);

  // Set compiler folder
  DelimPos1 := RPos(pd, BinDir);
  if DelimPos1 > 0 then
    fFolder := Copy(BinDir, 1, DelimPos1 - 1);

  // Find compiler builder
  DelimPos1 := DelimPos2;
  while (DelimPos1 <= Length(output)) and not (output[DelimPos1] = '(') do
    Inc(DelimPos1);
  while (DelimPos2 <= Length(output)) and not (output[DelimPos2] = ')') do
    Inc(DelimPos2);
  fType := Copy(output, DelimPos1 + 1, DelimPos2 - DelimPos1 - 1);

  // Assemble user friendly name if we don't have one yet
  if fName = '' then begin
    if ContainsStr(fType, 'tdm64') then
      fName := 'TDM-GCC ' + fVersion
    else if ContainsStr(fType, 'tdm') then
      fName := 'TDM-GCC ' + fVersion
    else if ContainsStr(fType, 'GCC') then
      fName := 'MinGW GCC ' + fVersion;
  end;

  // Obtain compiler target
  fDumpMachine := GetCompilerOutput(BinDir + pd, BinFile, '-dumpmachine');

  // Find default include directories
  {if FileExists(BinDir + pd + "cpp.exe") then begin
   output := GetCompilerOutput(BinDir + pd,"cpp.exe","-v");
   DelimPos1 := Pos("#include <...> search starts here:",output);
   if DelimPos1 > 0 then begin
    DelimPos2 := ("End of search list.",output);
    if DelimPos2 > 0 then begin
     S := Copy(output,DelimPos1,DelimPos2-DelimPos1);
     fDefInclude.Clear;
     ExtractStrings([#10],[],PChar(S),fDefInclude);
    end;
   end;
  end;}

  // Obtain default includes when changing current file?
  // Don't use CommaText:
  // http://stackoverflow.com/questions/1335027/delphi-stringlist-delimiter-is-always-a-space-character-even-if-delimiter-is-se
  output := GetCompilerOutput(BinDir + pd, 'cpp.exe', '-dM -E -x c++ -std=gnu++11 NUL');
  // TODO: use command of current file
  ExtractStrings([#10], [], PChar(output), fDefines);
end;

procedure TdevCompilerSet.SetExecutables;
begin
  // Set executables
  fgccName := GCC_PROGRAM;
  fgppName := GPP_PROGRAM;
  fgdbName := GDB_PROGRAM;
  fmakeName := MAKE_PROGRAM;
  fwindresName := WINDRES_PROGRAM;
  fgprofName := GPROF_PROGRAM;
end;

procedure TdevCompilerSet.SetDirectories;
  procedure AddExistingDirectory(var list: TStringList; const Directory: String);
  begin
    if DirectoryExists(Directory) then
      list.Add(Directory);
  end;
begin
  // Add both the default and the autoconf directories
  AddExistingDirectory(fBinDir, fFolder + pd + 'bin');
  AddExistingDirectory(fLibDir, fFolder + pd + 'lib');
  AddExistingDirectory(fCDir, fFolder + pd + 'include');
  AddExistingDirectory(fCppDir, fFolder + pd + 'include');

  // Try to obtain our target/autoconf folder
  if fDumpMachine <> '' then begin
    AddExistingDirectory(fBinDir, fFolder + pd + fDumpMachine + pd + 'bin');
    AddExistingDirectory(fLibDir, fFolder + pd + fDumpMachine + pd + 'lib');

    // Regular include folder
    AddExistingDirectory(fCDir, fFolder + pd + fDumpMachine + pd + 'include');
    AddExistingDirectory(fCppDir, fFolder + pd + fDumpMachine + pd + 'include');

    // Other include folder?
    AddExistingDirectory(fCDir,
      fFolder + pd + 'lib' + pd + 'gcc' + pd + fDumpMachine + pd + fVersion + pd + 'include');
    AddExistingDirectory(fCppDir,
      fFolder + pd + 'lib' + pd + 'gcc' + pd + fDumpMachine + pd + fVersion + pd + 'include');

    // C++ only folder
    AddExistingDirectory(fCppDir,
      fFolder + pd + 'lib' + pd + 'gcc' + pd + fDumpMachine + pd + fVersion + pd + 'include' + pd + 'c++');
  end;
end;

procedure TdevCompilerSet.SetUserInput;
begin
  // User input
  fCompAdd := FALSE;
  fLinkAdd := TRUE;
  fCompOpt := '';

  // MinGW32 requires special treatment
  if ContainsStr(fName, 'MinGW') or ContainsStr(fName, 'TDC-GCC') then
    fLinkOpt := '-static-libstdc++ -static-libgcc'
  else
    fLinkOpt := '-static-libgcc';
end;

procedure TdevCompilerSet.SetOptions;
var
  sl: TStringList;
begin

  // C options
  AddOption(ID_COPT_ANSIC, ID_COPT_GRP_C, True, True, False, 0, '-ansi', nil);
  AddOption(ID_COPT_NOASM, ID_COPT_GRP_C, True, True, False, 0, '-fno-asm', nil);
  AddOption(ID_COPT_TRADITIONAL, ID_COPT_GRP_C, True, True, False, 0, '-traditional-cpp', nil);

  // Optimization
  sl := TStringList.Create;
  sl.Add(''); // /!\ Must contain a starting empty value in order to do not have always to pass the parameter
  sl.Add('This CPU=native');
  sl.Add('x86-64: A generic CPU with 64-bit extensions=x86-64');
  sl.Add('i386: Original Intel i386 CPU=i386');
  sl.Add('i486: Intel i486 CPU. (No scheduling is implemented for this chip.)=i486');
  sl.Add('i586: Intel Pentium CPU with no MMX support.=i586');
  sl.Add('pentium: Intel Pentium CPU with no MMX support.=pentium');
  sl.Add('lakemont: Intel Lakemont MCU, based on Intel Pentium CPU.=lakemont');
  sl.Add('pentium-mmx: Intel Pentium MMX CPU, based on Pentium core with MMX instruction set support.=pentium-mmx');
  sl.Add('pentiumpro: Intel Pentium Pro CPU.=pentiumpro');
  sl.Add('i686: When used with -march, the Pentium Pro instruction set is used, so the code runs on all i686 family chips. When used with -mtune, it has the same meaning as ‘generic’.=i686');
  sl.Add('pentium2: Intel Pentium II CPU, based on Pentium Pro core with MMX instruction set support.=pentium2');
  sl.Add('pentium3: Intel Pentium III CPU, based on Pentium Pro core with MMX and SSE instruction set support.=pentium3');
  sl.Add('pentium3m: Intel Pentium III CPU, based on Pentium Pro core with MMX and SSE instruction set support.=pentium3m');
  sl.Add('pentium-m: Intel Pentium M; low-power version of Intel Pentium III CPU with MMX, SSE and SSE2 instruction set support. Used by Centrino notebooks.=pentium-m');
  sl.Add('pentium4: Intel Pentium 4 CPU with MMX, SSE and SSE2 instruction set support.=pentium4');
  sl.Add('pentium4m: Intel Pentium 4 CPU with MMX, SSE and SSE2 instruction set support.=pentium4m');
  sl.Add('prescott: Improved version of Intel Pentium 4 CPU with MMX, SSE, SSE2 and SSE3 instruction set support.=prescott');
  sl.Add('nocona: Improved version of Intel Pentium 4 CPU with 64-bit extensions, MMX, SSE, SSE2 and SSE3 instruction set support.=nocona');
  sl.Add('core2: Intel Core 2 CPU with 64-bit extensions, MMX, SSE, SSE2, SSE3 and SSSE3 instruction set support.=core2');
  sl.Add('nehalem: Intel Nehalem CPU with 64-bit extensions, MMX, SSE, SSE2, SSE3, SSSE3, SSE4.1, SSE4.2 and POPCNT instruction set support.=nehalem');
  sl.Add('westmere: Intel Westmere CPU with 64-bit extensions, MMX, SSE, SSE2, SSE3, SSSE3, SSE4.1, SSE4.2, POPCNT, AES and PCLMUL instruction set support.=westmere');
  sl.Add('sandybridge: Intel Sandy Bridge CPU with 64-bit extensions, MMX, SSE, SSE2, SSE3, SSSE3, SSE4.1, SSE4.2, POPCNT, AVX, AES and PCLMUL instruction set support.=sandybridge');
  sl.Add('ivybridge: Intel Ivy Bridge CPU with 64-bit extensions, MMX, SSE, SSE2, SSE3, SSSE3, SSE4.1, SSE4.2, POPCNT, AVX, AES, PCLMUL, FSGSBASE, RDRND and F16C instruction set support.=ivybridge');
  sl.Add('haswell: Intel Haswell CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3, SSE4.1, SSE4.2, POPCNT, AVX, AVX2, AES, PCLMUL, FSGSBASE, RDRND, FMA, BMI, BMI2 and F16C instruction set support.=haswell');
  sl.Add('broadwell: Intel Broadwell CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3, SSE4.1, SSE4.2, POPCNT, AVX, AVX2, AES, PCLMUL, FSGSBASE, RDRND, FMA, BMI, BMI2, F16C, RDSEED, ADCX and PREFETCHW instruction set support.=broadwell');
  sl.Add('skylake: Intel Skylake CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3, SSE4.1, SSE4.2, POPCNT, AVX, AVX2, AES, PCLMUL, FSGSBASE, RDRND, FMA, BMI, BMI2, F16C, RDSEED, ADCX, PREFETCHW, CLFLUSHOPT, XSAVEC and ' + 'XSAVES instruction set support.=skylake');
  sl.Add('bonnell: Intel Bonnell CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3 and SSSE3 instruction set support.=bonnell');
  sl.Add('silvermont: Intel Silvermont CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3, SSE4.1, SSE4.2, POPCNT, AES, PCLMUL and RDRND instruction set support.=silvermont');
  sl.Add('goldmont: Intel Goldmont CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3, SSE4.1, SSE4.2, POPCNT, AES, PCLMUL, RDRND, XSAVE, XSAVEOPT and FSGSBASE instruction set support.=goldmont');
  sl.Add('goldmont-plus: Intel Goldmont Plus CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3, SSE4.1, SSE4.2, POPCNT, AES, PCLMUL, RDRND, XSAVE, XSAVEOPT, FSGSBASE, PTWRITE, RDPID, SGX and UMIP instruction set support.=goldmont-plus');
  sl.Add('tremont: Intel Tremont CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3, SSE4.1, SSE4.2, POPCNT, AES, PCLMUL, RDRND, XSAVE, XSAVEOPT, FSGSBASE, PTWRITE, RDPID, SGX, UMIP, GFNI-SSE, CLWB and ENCLV instruction set support.=tremont');
  sl.Add('knl: Intel Knight’s Landing CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3, SSE4.1, SSE4.2, POPCNT, AVX, AVX2, AES, PCLMUL, FSGSBASE, RDRND, FMA, BMI, BMI2, F16C, RDSEED, ADCX, PREFETCHW, AVX512F, AVX512PF, ' + 'AVX512ER and AVX512CD instruction set support.=knl');
  sl.Add('knm: Intel Knights Mill CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3, SSE4.1, SSE4.2, POPCNT, AVX, AVX2, AES, PCLMUL, FSGSBASE, RDRND, FMA, BMI, BMI2, F16C, RDSEED, ADCX, PREFETCHW, AVX512F, AVX512PF, AVX512ER, ' + 'AVX512CD, AVX5124VNNIW, AVX5124FMAPS and AVX512VPOPCNTDQ instruction set support.=knm');
  sl.Add('avx512: Intel Skylake Server CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3, SSE4.1, SSE4.2, POPCNT, PKU, AVX, AVX2, AES, PCLMUL, FSGSBASE, RDRND, FMA, BMI, BMI2, F16C, RDSEED, ADCX, PREFETCHW, CLFLUSHOPT, XSAVEC, XSAVES, ' + 'AVX512F, CLWB, AVX512VL, AVX512BW, AVX512DQ and AVX512CD instruction set support.=skylake-avx512');
  sl.Add('cannonlake: Intel Cannonlake Server CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3, SSE4.1, SSE4.2, POPCNT, PKU, AVX, AVX2, AES, PCLMUL, FSGSBASE, RDRND, FMA, BMI, BMI2, F16C, RDSEED, ADCX, PREFETCHW, CLFLUSHOPT, XSAVEC, XSAVES, ' + 'AVX512F, AVX512VL, AVX512BW, AVX512DQ, AVX512CD, AVX512VBMI, AVX512IFMA, SHA and UMIP instruction set support.=cannonlake');
  sl.Add('icelake-client: Intel Icelake Client CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3, SSE4.1, SSE4.2, POPCNT, PKU, AVX, AVX2, AES, PCLMUL, FSGSBASE, RDRND, FMA, BMI, BMI2, F16C, RDSEED, ADCX, PREFETCHW, CLFLUSHOPT, XSAVEC, XSAVES, ' + 'AVX512F, AVX512VL, AVX512BW, AVX512DQ, AVX512CD, AVX512VBMI, AVX512IFMA, SHA, CLWB, UMIP, RDPID, GFNI, AVX512VBMI2, AVX512VPOPCNTDQ, AVX512BITALG, AVX512VNNI, VPCLMULQDQ, VAES instruction set support.=icelake-client');
  sl.Add('icelake-server: Intel Icelake Server CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3, SSE4.1, SSE4.2, POPCNT, PKU, AVX, AVX2, AES, PCLMUL, FSGSBASE, RDRND, FMA, BMI, BMI2, F16C, RDSEED, ADCX, PREFETCHW, CLFLUSHOPT, XSAVEC, XSAVES, ' + 'AVX512F, AVX512VL, AVX512BW, AVX512DQ, AVX512CD, AVX512VBMI, AVX512IFMA, SHA, CLWB, UMIP, RDPID, GFNI, AVX512VBMI2, AVX512VPOPCNTDQ, AVX512BITALG, AVX512VNNI, VPCLMULQDQ, VAES, PCONFIG and WBNOINVD instruction set support.=icelake-server');
  sl.Add('cascadelake: Intel Cascadelake CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3, SSE4.1, SSE4.2, POPCNT, PKU, AVX, AVX2, AES, PCLMUL, FSGSBASE, RDRND, FMA, BMI, BMI2, F16C, RDSEED, ADCX, PREFETCHW, CLFLUSHOPT, XSAVEC, XSAVES, ' + 'AVX512F, CLWB, AVX512VL, AVX512BW, AVX512DQ, AVX512CD and AVX512VNNI instruction set support.=cascadelake');
  sl.Add('cooperlake: Intel cooperlake CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3, SSE4.1, SSE4.2, POPCNT, PKU, AVX, AVX2, AES, PCLMUL, FSGSBASE, RDRND, FMA, BMI, BMI2, F16C, RDSEED, ADCX, PREFETCHW, CLFLUSHOPT, XSAVEC, XSAVES, ' + 'AVX512F, CLWB, AVX512VL, AVX512BW, AVX512DQ, AVX512CD, AVX512VNNI and AVX512BF16 instruction set support.=cooperlake');
  sl.Add('tigerlake: Intel Tigerlake CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3, SSE4.1, SSE4.2, POPCNT, PKU, AVX, AVX2, AES, PCLMUL, FSGSBASE, RDRND, FMA, BMI, BMI2, F16C, RDSEED, ADCX, PREFETCHW, CLFLUSHOPT, XSAVEC, XSAVES, ' + 'AVX512F, AVX512VL, AVX512BW, AVX512DQ, AVX512CD, AVX512VBMI, AVX512IFMA, SHA, CLWB, UMIP, RDPID, GFNI, AVX512VBMI2, AVX512VPOPCNTDQ, AVX512BITALG, AVX512VNNI, ' + 'VPCLMULQDQ, VAES, PCONFIG, WBNOINVD, MOVDIRI, MOVDIR64B and AVX512VP2INTERSECT instruction set support.=tigerlake');
  sl.Add('sapphirerapids: Intel sapphirerapids CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3, SSE4.1, SSE4.2, POPCNT, PKU, AVX, AVX2, AES, PCLMUL, FSGSBASE, RDRND, FMA, BMI, BMI2, F16C, RDSEED, ADCX, PREFETCHW, CLFLUSHOPT, XSAVEC, ' + 'XSAVES, AVX512F, CLWB, AVX512VL, AVX512BW, AVX512DQ, AVX512CD, AVX512VNNI, AVX512BF16, MOVDIRI, MOVDIR64B, AVX512VP2INTERSECT, ENQCMD, CLDEMOTE, PTWRITE, WAITPKG, SERIALIZE and TSXLDTRK instruction set support.=sapphirerapids');
  sl.Add('alderlake: Intel Alderlake CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3, SSE4.1, SSE4.2, POPCNT, AVX, AVX2, AES, PCLMUL, FSGSBASE, RDRND, FMA, CLDEMOTE, PTWRITE, WAITPKG and SERIALIZE instruction set support.=alderlake');
  sl.Add('k6: AMD K6 CPU with MMX instruction set support.=k6');
  sl.Add('k6-2: Improved versions of AMD K6 CPU with MMX and 3DNow! instruction set support.=k6-2');
  sl.Add('k6-3: Improved versions of AMD K6 CPU with MMX and 3DNow! instruction set support.=k6-3');
  sl.Add('athlon: AMD Athlon CPU with MMX, 3dNOW!, enhanced 3DNow! and SSE prefetch instructions support.=athlon');
  sl.Add('athlon-tbird: AMD Athlon CPU with MMX, 3dNOW!, enhanced 3DNow! and SSE prefetch instructions support.=athlon-tbird');
  sl.Add('athlon-4: Improved AMD Athlon CPU with MMX, 3DNow!, enhanced 3DNow! and full SSE instruction set support.=athlon-4');
  sl.Add('athlon-xp: Improved AMD Athlon CPU with MMX, 3DNow!, enhanced 3DNow! and full SSE instruction set support.=athlon-xp');
  sl.Add('athlon-mp: Improved AMD Athlon CPU with MMX, 3DNow!, enhanced 3DNow! and full SSE instruction set support.=athlon-mp');
  sl.Add('k8: Processors based on the AMD K8 core with x86-64 instruction set support, including the AMD Opteron, Athlon 64, and Athlon 64 FX processors. (This supersets MMX, SSE, SSE2, 3DNow!, enhanced 3DNow! and 64-bit instruction set extensions.)=k8');
  sl.Add('opteron: Processors based on the AMD K8 core with x86-64 instruction set support, including the AMD Opteron, Athlon 64, and Athlon 64 FX processors. (This supersets MMX, SSE, SSE2, 3DNow!, enhanced 3DNow! and ' + '64-bit instruction set extensions.)=opteron');
  sl.Add('athlon64: Processors based on the AMD K8 core with x86-64 instruction set support, including the AMD Opteron, Athlon 64, and Athlon 64 FX processors. (This supersets MMX, SSE, SSE2, 3DNow!, enhanced 3DNow! and ' + '64-bit instruction set extensions.)=athlon64');
  sl.Add('athlon-fx: Processors based on the AMD K8 core with x86-64 instruction set support, including the AMD Opteron, Athlon 64, and Athlon 64 FX processors. (This supersets MMX, SSE, SSE2, 3DNow!, enhanced 3DNow! and ' + '64-bit instruction set extensions.)=athlon-fx');
  sl.Add('k8-sse3: Improved versions of AMD K8 cores with SSE3 instruction set support.=k8-sse3');
  sl.Add('opteron-sse3: Improved versions of AMD K8 cores with SSE3 instruction set support.=opteron-sse3');
  sl.Add('athlon64-sse3: Improved versions of AMD K8 cores with SSE3 instruction set support.=athlon64-sse3');
  sl.Add('amdfam10: CPUs based on AMD Family 10h cores with x86-64 instruction set support. (This supersets MMX, SSE, SSE2, SSE3, SSE4A, 3DNow!, enhanced 3DNow!, ABM and 64-bit instruction set extensions.)=amdfam10');
  sl.Add('barcelona: CPUs based on AMD Family 10h cores with x86-64 instruction set support. (This supersets MMX, SSE, SSE2, SSE3, SSE4A, 3DNow!, enhanced 3DNow!, ABM and 64-bit instruction set extensions.)=barcelona');
  sl.Add('bdver1: CPUs based on AMD Family 15h cores with x86-64 instruction set support. (This supersets FMA4, AVX, XOP, LWP, AES, PCLMUL, CX16, MMX, SSE, SSE2, SSE3, SSE4A, SSSE3, SSE4.1, SSE4.2, ABM and 64-bit instruction set extensions.)=bdver1');
  sl.Add('bdver2: AMD Family 15h core based CPUs with x86-64 instruction set support. (This supersets BMI, TBM, F16C, FMA, FMA4, AVX, XOP, LWP, AES, PCLMUL, CX16, MMX, SSE, SSE2, SSE3, SSE4A, SSSE3, SSE4.1, SSE4.2, ABM and ' + '64-bit instruction set extensions.)=bdver2');
  sl.Add('bdver3: AMD Family 15h core based CPUs with x86-64 instruction set support. (This supersets BMI, TBM, F16C, FMA, FMA4, FSGSBASE, AVX, XOP, LWP, AES, PCLMUL, CX16, MMX, SSE, SSE2, SSE3, SSE4A, SSSE3, SSE4.1, ' + 'SSE4.2, ABM and 64-bit instruction set extensions.)=bdver3');
  sl.Add('bdver4: AMD Family 15h core based CPUs with x86-64 instruction set support. (This supersets BMI, BMI2, TBM, F16C, FMA, FMA4, FSGSBASE, AVX, AVX2, XOP, LWP, AES, PCLMUL, CX16, MOVBE, MMX, SSE, SSE2, SSE3, SSE4A, ' + 'SSSE3, SSE4.1, SSE4.2, ABM and 64-bit instruction set extensions.)=bdver4');
  sl.Add('znver1: AMD Family 17h core based CPUs with x86-64 instruction set support. (This supersets BMI, BMI2, F16C, FMA, FSGSBASE, AVX, AVX2, ADCX, RDSEED, MWAITX, SHA, CLZERO, AES, PCLMUL, CX16, MOVBE, MMX, SSE, SSE2, ' + 'SSE3, SSE4A, SSSE3, SSE4.1, SSE4.2, ABM, XSAVEC, XSAVES, CLFLUSHOPT, POPCNT, and 64-bit instruction set extensions.)=znver1');
  sl.Add('znver2: AMD Family 17h core based CPUs with x86-64 instruction set support. (This supersets BMI, BMI2, CLWB, F16C, FMA, FSGSBASE, AVX, AVX2, ADCX, RDSEED, MWAITX, SHA, CLZERO, AES, PCLMUL, CX16, MOVBE, MMX, SSE, SSE2, ' + 'SSE3, SSE4A, SSSE3, SSE4.1, SSE4.2, ABM, XSAVEC, XSAVES, CLFLUSHOPT, POPCNT, RDPID, WBNOINVD, and 64-bit instruction set extensions.)=znver2');
  sl.Add('btver1: CPUs based on AMD Family 14h cores with x86-64 instruction set support. (This supersets MMX, SSE, SSE2, SSE3, SSSE3, SSE4A, CX16, ABM and 64-bit instruction set extensions.)=btver1');
  sl.Add('btver2: CPUs based on AMD Family 16h cores with x86-64 instruction set support. This includes MOVBE, F16C, BMI, AVX, PCLMUL, AES, SSE4.2, SSE4.1, CX16, ABM, SSE4A, SSSE3, SSE3, SSE2, SSE, MMX and 64-bit instruction set extensions.=btver2');
  sl.Add('winchip-c6: IDT WinChip C6 CPU, dealt in same way as i486 with additional MMX instruction set support.=winchip-c6');
  sl.Add('winchip2: IDT WinChip 2 CPU, dealt in same way as i486 with additional MMX and 3DNow! instruction set support.=winchip2');
  sl.Add('c3: VIA C3 CPU with MMX and 3DNow! instruction set support. (No scheduling is implemented for this chip.)=c3');
  sl.Add('c3-2: VIA C3-2 (Nehemiah/C5XL) CPU with MMX and SSE instruction set support. (No scheduling is implemented for this chip.)=c3-2');
  sl.Add('c7: VIA C7 (Esther) CPU with MMX, SSE, SSE2 and SSE3 instruction set support. (No scheduling is implemented for this chip.)=c7');
  sl.Add('samuel-2: VIA Eden Samuel 2 CPU with MMX and 3DNow! instruction set support. (No scheduling is implemented for this chip.)=samuel-2');
  sl.Add('nehemiah: VIA Eden Nehemiah CPU with MMX and SSE instruction set support. (No scheduling is implemented for this chip.)=nehemiah');
  sl.Add('esther: VIA Eden Esther CPU with MMX, SSE, SSE2 and SSE3 instruction set support. (No scheduling is implemented for this chip.)=esther');
  sl.Add('eden-x2: VIA Eden X2 CPU with x86-64, MMX, SSE, SSE2 and SSE3 instruction set support. (No scheduling is implemented for this chip.)=eden-x2');
  sl.Add('eden-x4: VIA Eden X4 CPU with x86-64, MMX, SSE, SSE2, SSE3, SSSE3, SSE4.1, SSE4.2, AVX and AVX2 instruction set support. (No scheduling is implemented for this chip.)=eden-x4');
  sl.Add('nano: Generic VIA Nano CPU with x86-64, MMX, SSE, SSE2, SSE3 and SSSE3 instruction set support. (No scheduling is implemented for this chip.)=nano');
  sl.Add('nano-1000: VIA Nano 1xxx CPU with x86-64, MMX, SSE, SSE2, SSE3 and SSSE3 instruction set support. (No scheduling is implemented for this chip.)=nano-1000');
  sl.Add('nano-2000: VIA Nano 2xxx CPU with x86-64, MMX, SSE, SSE2, SSE3 and SSSE3 instruction set support. (No scheduling is implemented for this chip.)=nano-2000');
  sl.Add('nano-3000: VIA Nano 3xxx CPU with x86-64, MMX, SSE, SSE2, SSE3, SSSE3 and SSE4.1 instruction set support. (No scheduling is implemented for this chip.)=nano-3000');
  sl.Add('nano-x2: VIA Nano Dual Core CPU with x86-64, MMX, SSE, SSE2, SSE3, SSSE3 and SSE4.1 instruction set support. (No scheduling is implemented for this chip.)=nano-x2');
  sl.Add('nano-x4: VIA Nano Quad Core CPU with x86-64, MMX, SSE, SSE2, SSE3, SSSE3 and SSE4.1 instruction set support. (No scheduling is implemented for this chip.)=nano-x4');
  sl.Add('geode: AMD Geode embedded processor with MMX and 3DNow! instruction set support.=geode');

  AddOption(ID_COPT_ARCH, ID_COPT_GRP_CODEGEN, True, True, False, 0, '-march=', sl);

  // Optimization
  var sl2 := TStringList.Create;
  sl2.Assign(sl);
  sl2.Insert(2, 'generic: Produce code optimized for the most common IA32/AMD64/EM64T processors=generic');
  sl2.Insert(3, 'intel: Produce code optimized for the most current Intel processors, which are Haswell and Silvermont for this version of GCC=intel');

  AddOption(ID_COPT_TUNE, ID_COPT_GRP_CODEGEN, True, True, False, 0, '-mtune=', sl2);

  // Built-in processor functions
  sl := TStringList.Create;
  sl.Add(''); // /!\ Must contain a starting empty value in order to do not have always to pass the parameter
  sl.Add('cmov: CMOV instruction.=cmov');
  sl.Add('mmx: MMX instructions.=mmx');
  sl.Add('m3dnow: 3DNow=3dnow');
  sl.Add('m3dnowa: 3DNow=3dnowa');
  sl.Add('popcnt: POPCNT instruction.=popcnt');
  sl.Add('sse: SSE instructions.=sse');
  sl.Add('sse2: SSE2 instructions.=sse2');
  sl.Add('sse3: SSE3 instructions.=sse3');
  sl.Add('ssse3: SSSE3 instructions.=ssse3');
  sl.Add('sse4.1: SSE4.1 instructions.=sse4.1');
  sl.Add('sse4.2: SSE4.2 instructions.=sse4.2');
  sl.Add('avx: AVX instructions.=avx');
  sl.Add('avx2: AVX2 instructions.=avx2');
  sl.Add('sse4a: SSE4A instructions.=sse4a');
  sl.Add('fma4: FMA4 instructions.=fma4');
  sl.Add('xop: XOP instructions.=xop');
  sl.Add('fma: FMA instructions.=fma');
  sl.Add('avx512f: AVX512F instructions.=avx512f');
  sl.Add('bmi: BMI instructions.=bmi');
  sl.Add('bmi2: BMI2 instructions.=bmi2');
  sl.Add('aes: AES instructions.=aes');
  sl.Add('pclmul: PCLMUL instructions.=pclmul');
  sl.Add('avx512vl: AVX512VL instructions.=avx512vl');
  sl.Add('avx512bw: AVX512BW instructions.=avx512bw');
  sl.Add('avx512dq: AVX512DQ instructions.=avx512dq');
  sl.Add('avx512cd: AVX512CD instructions.=avx512cd');
  sl.Add('avx512er: AVX512ER instructions.=avx512er');
  sl.Add('avx512pf: AVX512PF instructions.=avx512pf');
  sl.Add('avx512vbmi: AVX512VBMI instructions.=avx512vbmi');
  sl.Add('avx512ifma: AVX512IFMA instructions.=avx512ifma');
  sl.Add('avx5124vnniw: AVX5124VNNIW instructions.=avx5124vnniw');
  sl.Add('avx5124fmaps: AVX5124FMAPS instructions.=avx5124fmaps');
  sl.Add('avx512vpopcntdq: AVX512VPOPCNTDQ instructions.=avx512vpopcntdq');
  sl.Add('avx512vbmi2: AVX512VBMI2 instructions.=avx512vbmi2');
  sl.Add('gfni: GFNI instructions.=gfni');
  sl.Add('vpclmulqdq: VPCLMULQDQ instructions.=vpclmulqdq');
  sl.Add('avx512vnni: AVX512VNNI instructions.=avx512vnni');
  sl.Add('avx512bitalg: AVX512BITALG instructions.');

  AddOption(ID_COPT_BUILTINPROC, ID_COPT_GRP_CODEGEN, True, True, False, 0, '-m', sl);

  // Optimization
  sl := TStringList.Create;
  sl.Add('');
  sl.Add('Low=1');
  sl.Add('Med=2');
  sl.Add('High=3');
  sl.Add('Highest (fast)=fast');
  sl.Add('Size (s)=s');
  sl.Add('Debug (g)=g');
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
  sl.Add('ISO C++11=c++11');
  sl.Add('GNU C90=gnu90');
  sl.Add('GNU C99=gnu99');
  sl.Add('GNU C++=gnu++98');
  sl.Add('GNU C++11=gnu++11');
  sl.Add('GNU C++14=gnu++14');
  sl.Add('GNU C++17=gnu++17');
  sl.Add('GNU C++2a=gnu++2a');

  AddOption(ID_COPT_STD, ID_COPT_GRP_CODEGEN, True, True, False, 0, '-std=', sl);

  // Warnings
  AddOption(ID_COPT_WARNING, ID_COPT_GRP_WARN, True, True, False, 0, '-w', nil);
  AddOption(ID_COPT_WARNINGPLUS, ID_COPT_GRP_WARN, True, True, False, 0, '-Wall', nil);
  AddOption(ID_COPT_WARNINGEX, ID_COPT_GRP_WARN, True, True, False, 0, '-Wextra', nil);
  AddOption(ID_COPT_ISOCONFORM, ID_COPT_GRP_WARN, True, True, False, 0, '-pedantic', nil);
  AddOption(ID_COPT_SYNTAXONLY, ID_COPT_GRP_WARN, True, True, False, 0, '-fsyntax-only', nil);
  AddOption(ID_COPT_TREATASERROR, ID_COPT_GRP_WARN, True, True, False, 0, '-Werror', nil);
  AddOption(ID_COPT_FAILONFIRST, ID_COPT_GRP_WARN, True, True, False, 0, '-Wfatal-errors', nil);

  // Profiling
  AddOption(ID_COPT_PROFILE, ID_COPT_PROFILING, True, True, True, 0, '-pg', nil);

  // Linker
  AddOption(ID_COPT_OBJC, ID_COPT_LINKERTAB, False, False, True, 0, '-lobjc', nil);
  AddOption(ID_COPT_DEBUG, ID_COPT_LINKERTAB, True, True, True, 0, '-g3', nil);
  AddOption(ID_COPT_NOLIBS, ID_COPT_LINKERTAB, True, True, True, 0, '-nostdlib', nil);
  AddOption(ID_COPT_WIN32, ID_COPT_LINKERTAB, True, True, True, 0, '-mwindows', nil);
  AddOption(ID_COPT_STRIP, ID_COPT_LINKERTAB, False, False, True, 0, '-s', nil);

  // Output
  AddOption(ID_COPT_MEM, ID_COPT_GRP_OUTPUT, True, True, False, 0, '-fverbose-asm', nil);
  AddOption(ID_COPT_ASSEMBLY, ID_COPT_GRP_OUTPUT, True, True, False, 0, '-S', nil);
  AddOption(ID_COPT_PIPES, ID_COPT_GRP_OUTPUT, True, True, False, 0, '-pipe', nil);
end;

procedure TdevCompilerSet.AddOption(Name, Section: integer; IsC, IsCpp, IsLinker: boolean; Value: integer; const
  Setting: String; Choices: TStringList);
var
  option: PCompilerOption;
begin
  option := New(PCompilerOption);
  option^.Name := Name;
  option^.Section := Section;
  option^.IsC := IsC;
  option^.IsCpp := IsCpp;
  option^.IsLinker := IsLinker;
  option^.Value := Value;
  option^.Setting := Setting;
  option^.Choices := Choices;
  fOptions.Add(option);
end;

function TdevCompilerSet.GetOption(const Option: String): Integer;
var
  OptionStruct: PCompilerOption;
  OptionIndex: integer;
begin
  if FindOption(Option, OptionStruct, OptionIndex) then
    result := OptionStruct^.Value
  else
    result := 0;
end;

function TdevCompilerSet.FindOption(const Option: String; var opt: PCompilerOption; var Index: integer): boolean;
var
  I: integer;
begin
  Result := False;
  for I := 0 to fOptions.Count - 1 do
    if SameStr(PCompilerOption(fOptions[I])^.Setting, Option) then begin
      opt := PCompilerOption(fOptions[I]);
      Index := I;
      Result := True;
      Break;
    end;
end;

function TdevCompilerSet.GetINIOptions: TCompilerOptionsValues;
var
  I: integer;
begin
  Result := [];
  for I := 0 to fOptions.Count - 1 do
    Result := Result + [PCompilerOption(fOptions[I])^.Value];
end;

procedure TdevCompilerSet.SetINIOptions(const value: TArray<Integer>);
var
  I: integer;
begin
  for I := 0 to fOptions.Count - 1 do
    PCompilerOption(fOptions[I])^.Value := value.OptionValue[I];
end;

procedure TdevCompilerSet.SetOption(const Option: String; Value: Integer);
var
  OptionStruct: PCompilerOption;
  OptionIndex: integer;
begin
  if FindOption(Option, OptionStruct, OptionIndex) then
    SetOption(OptionStruct, Value);
end;

procedure TdevCompilerSet.SetOption(Option: PCompilerOption; Value: Integer);
begin
  Option^.Value := Value;
end;

function TdevCompilerSet.GetCompilerOutput(const BinDir, BinFile, Input: String): String;
begin
  result := Trim(RunAndGetOutput(BinDir + pd + BinFile + ' ' + Input, BinDir, nil, nil, false))
end;

function TdevCompilerSet.ValidateDirs: boolean;
var
  msg, goodbin, badbin, goodlib, badlib, goodinc, badinc, goodinccpp, badinccpp: String;

  procedure CheckDirs(dirlist: TStringList; var gooddirs: String; var baddirs: String);
  var
    i: integer;
  begin
    gooddirs := '';
    baddirs := '';

    for I := 0 to dirlist.Count - 1 do begin
      if not DirectoryExists(dirlist[i]) then begin
        if Length(baddirs) > 0 then
          baddirs := baddirs + ';' + dirlist[i]
        else
          baddirs := dirlist[i];
      end else begin
        if Length(gooddirs) > 0 then
          gooddirs := gooddirs + ';' + dirlist[i]
        else
          gooddirs := dirlist[i];
      end;
    end;
  end;

  procedure RemoveDirs(var dirlist: TStringList; baddirs: String);
  var
    i, index: integer;
    baddirlist: TStringList;
  begin
    baddirlist := TStringList.Create;
    try
      ExtractStrings([';'], [], PChar(baddirs), baddirlist);
      for I := 0 to baddirlist.Count - 1 do begin
        index := dirlist.IndexOf(baddirlist[i]);
        if index <> -1 then
          dirlist.Delete(index);
      end;
    finally
      baddirlist.Free;
    end;
  end;

  procedure AddUnique(var list: TStringList; const entry: String);
  begin
    if (list.IndexOf(entry) = -1) and DirectoryExists(entry) then
      list.Add(entry);
  end;
begin
  result := true;

  // Check if we can find the directories the user pointed to
  msg := '';
  if fBinDir.Count > 0 then begin // we need some bin dir, so treat count=0 as an error too
    CheckDirs(fBinDir, goodbin, badbin);
    if badbin <> '' then begin
      msg := msg + Format(Lang[ID_COMPVALID_DIRNOTFOUND], [Lang[ID_COMPVALID_BINARY]]) + #13#10;
      msg := msg + StringReplace(badbin, ';', #13#10, [rfReplaceAll]);
      msg := msg + #13#10 + #13#10;
    end;
  end else begin
    msg := msg + Format(Lang[ID_COMPVALID_DIRNOTSET], [Lang[ID_COMPVALID_BINARY]]);
    msg := msg + #13#10 + #13#10;
  end;

  CheckDirs(fCDir, goodinc, badinc);
  if badinc <> '' then begin
    msg := msg + Format(Lang[ID_COMPVALID_DIRNOTFOUND], [Lang[ID_COMPVALID_CINCLUDE]]) + #13#10;
    msg := msg + StringReplace(badinc, ';', #13#10, [rfReplaceAll]);
    msg := msg + #13#10 + #13#10;
  end;
  CheckDirs(fCppDir, goodinccpp, badinccpp);
  if badinccpp <> '' then begin
    msg := msg + Format(Lang[ID_COMPVALID_DIRNOTFOUND], [Lang[ID_COMPVALID_CPPINCLUDE]]) + #13#10;
    msg := msg + StringReplace(badinccpp, ';', #13#10, [rfReplaceAll]);
    msg := msg + #13#10 + #13#10;
  end;
  CheckDirs(fLibDir, goodlib, badlib);
  if badlib <> '' then begin
    msg := msg + Format(Lang[ID_COMPVALID_DIRNOTFOUND], [Lang[ID_COMPVALID_LIBRARY]]) + #13#10;
    msg := msg + StringReplace(badlib, ';', #13#10, [rfReplaceAll]);
    msg := msg + #13#10 + #13#10;
  end;
  if msg <> '' then begin
    msg := Format(Lang[ID_COMPVALID_CHECKINGSET], [Name]) + #13#10#13#10 + msg + Lang[ID_COMPVALID_DIRFIXSUGGESTION];

    // If confirmed, insert working dirs into default path list
    if MessageDlg(msg, mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin

      // Remove invalid
      RemoveDirs(fBinDir, badbin);
      RemoveDirs(fLibDir, badlib);
      RemoveDirs(fCDir, badinc);
      RemoveDirs(fCppDir, badinccpp);

      // If we know the folder, we should be able to predict dirs (copy from SetDirectories)
      if fFolder <> '' then begin

        // Add both the default and the autoconf directories
        AddUnique(fBinDir, fFolder + pd + 'bin');
        AddUnique(fLibDir, fFolder + pd + 'lib');
        AddUnique(fCDir, fFolder + pd + 'include');
        AddUnique(fCppDir, fFolder + pd + 'include');

        // Try to obtain our target/autoconf folder
        if fDumpMachine <> '' then begin
          AddUnique(fBinDir, fFolder + pd + fDumpMachine + pd + 'bin');
          AddUnique(fLibDir, fFolder + pd + fDumpMachine + pd + 'lib');
          AddUnique(fCDir, fFolder + pd + fDumpMachine + pd + 'include');
          AddUnique(fCppDir, fFolder + pd + fDumpMachine + pd + 'include');

          // Custom STL folder?
          // Currently, Dev-C++ is a bit slow at parsing this shit
          AddUnique(fCppDir, fFolder + pd + 'lib' + pd + 'gcc' + pd + fDumpMachine + pd + fVersion + pd + 'include' + pd
            + 'c++');
        end;

        // Otherwise, make some guesses based on the set name...
      end else if ContainsStr(fName, 'MinGW') and ContainsStr(fName, '32-bit') then begin

        // Add defaults
        if (badbin <> '') or (fBinDir.Count = 0) then
          AddUnique(fBinDir, devDirs.fExec + 'MinGW32' + pd + 'bin');
        if badlib <> '' then
          AddUnique(fLibDir, devDirs.fExec + 'MinGW32' + pd + 'lib');
        if badinc <> '' then
          AddUnique(fCDir, devDirs.fExec + 'MinGW32' + pd + 'include');
        if badinccpp <> '' then
          AddUnique(fCppDir, devDirs.fExec + 'MinGW32' + 'include');

      end else if ContainsStr(fName, 'TDM-GCC-64') then begin

        // Add defaults
        if (badbin <> '') or (fBinDir.Count = 0) then
          AddUnique(fBinDir, devDirs.fExec + 'TDM-GCC-64\bin');
        if badlib <> '' then begin
          if ContainsStr(fName, 'TDM-GCC') and ContainsStr(fName, '32-bit') then
            AddUnique(fLibDir, devDirs.fExec + 'TDM-GCC-64' + pd + 'x86_64-w64-mingw32' + pd + 'lib32')
          else
            AddUnique(fLibDir, devDirs.fExec + 'TDM-GCC-64' + pd + 'x86_64-w64-mingw32' + pd + 'lib');
        end;
        if badinc <> '' then
          AddUnique(fCDir, devDirs.fExec + 'TDM-GCC-64' + 'x86_64-w64-mingw32' + pd + 'include');
        if badinccpp <> '' then
          AddUnique(fCppDir, devDirs.fExec + 'TDM-GCC-64' + pd + 'x86_64-w64-mingw32' + pd + 'include');

      end else begin

        // Add defaults
        if (badbin <> '') or (fBinDir.Count = 0) then
          AddUnique(fBinDir, devDirs.fExec + 'MinGW64\bin');
        if badlib <> '' then begin
          if ContainsStr(fName, 'TDM-GCC') and ContainsStr(fName, '32-bit') then
            AddUnique(fLibDir, devDirs.fExec + 'MinGW64' + pd + 'x86_64-w64-mingw32' + pd + 'lib32')
          else
            AddUnique(fLibDir, devDirs.fExec + 'MinGW64' + pd + 'x86_64-w64-mingw32' + pd + 'lib');
        end;
        if badinc <> '' then
          AddUnique(fCDir, devDirs.fExec + 'MinGW64' + 'x86_64-w64-mingw32' + pd + 'include');
        if badinccpp <> '' then
          AddUnique(fCppDir, devDirs.fExec + 'MinGW64' + pd + 'x86_64-w64-mingw32' + pd + 'include');

      end;

      result := false; // not valid
    end;
  end;
end;

function TdevCompilerSet.ValidateExes: boolean;
var
  msg: String;
  I: integer;

  function FindFile(dirlist: TStringList; const FileName: String): boolean;
  var
    i: integer;
  begin
    result := false;
    if FileName = '' then begin
      Result := True;
      Exit;
    end; // Accept if filename is empty (when no binary is set)

    for I := 0 to dirlist.Count - 1 do begin
      if FileExists(dirlist[i] + pd + FileName) then begin
        result := true;
        Exit;
      end;
    end;
  end;
begin
  result := true;

  // Don't bother checking exes if the dir is not set
  if fBinDir.Count = 0 then
    Exit;

  // now check some exes
  msg := '';
  if not FindFile(fBinDir, fgccName) then begin
    msg := msg + Format(Lang[ID_COMPVALID_BINNOTFOUND], [Lang[ID_COMPVALID_CCOMP], fgccName]) + #13#10;
  end;
  if not FindFile(fBinDir, fgppName) then begin
    msg := msg + Format(Lang[ID_COMPVALID_BINNOTFOUND], [Lang[ID_COMPVALID_CPPCOMP], fgppName]) + #13#10;
  end;
  if not FindFile(fBinDir, fgdbName) then begin
    msg := msg + Format(Lang[ID_COMPVALID_BINNOTFOUND], [Lang[ID_COMPVALID_DEBUGGER], fgdbName]) + #13#10;
  end;
  if not FindFile(fBinDir, fgprofName) then begin
    msg := msg + Format(Lang[ID_COMPVALID_BINNOTFOUND], [Lang[ID_COMPVALID_PROFILER], fgprofName]) + #13#10;
  end;
  if not FindFile(fBinDir, fmakeName) then begin
    msg := msg + Format(Lang[ID_COMPVALID_BINNOTFOUND], [Lang[ID_COMPVALID_MAKE], fmakeName]) + #13#10;
  end;
  if not FindFile(fBinDir, fwindresName) then begin
    msg := msg + Format(Lang[ID_COMPVALID_BINNOTFOUND], [Lang[ID_COMPVALID_WINDRES], fwindresName]) + #13#10;
  end;
  if msg <> '' then begin
    msg := Format(Lang[ID_COMPVALID_CHECKINGSET], [Name]) + #13#10#13#10 + msg + #13#10 + Lang[ID_COMPVALID_DIRSEARCHED]
      + #13#10;
    for I := 0 to fBinDir.Count - 1 do
      msg := msg + fBinDir[i] + #13#10;
    msg := msg + #13#10 + Lang[ID_COMPVALID_BINFIXSUGGESTION];
    MessageDlg(msg, mtWarning, [mbOK], 0);
    result := false; // not valid
  end;
end;

function TdevCompilerSet.Validate: boolean;
begin
  result := ValidateDirs and ValidateExes; // return true if valid
end;

procedure TdevCompilerSet.Clear;
begin
  fgccName := '';
  fgppName := '';
  fgdbName := '';
  fmakeName := '';
  fwindresName := '';
  fgprofName := '';

  fCompOpt := '';
  fLinkOpt := '';
  fCompAdd := false;
  fLinkAdd := false;

  fBinDir.Clear;
  fCDir.Clear;
  fCppDir.Clear;
  fLibDir.Clear;
end;

{ TdevCompilerSets }

constructor TdevCompilerSets.Create;
begin
  inherited;
  fList := TList.Create;
  fDefaultIndex := -1;
end;

destructor TdevCompilerSets.Destroy;
begin
  ClearSets;
  fList.Free;
  inherited;
end;

function TdevCompilerSets.GetCompilationSet: TdevCompilerSet;
var
  Index: integer;
begin
  Index := GetCompilationSetIndex;

  // Range check...
  if (index >= 0) and (index < fList.Count) then
    result := TdevCompilerSet(fList[index])
  else
    result := nil;
end;

function TdevCompilerSets.GetCompilationSetIndex: Integer;
begin
  Result := -1;
  if Assigned(MainForm) then begin
    case MainForm.GetCompileTarget of
      ctNone:
        Result := fDefaultIndex;
      ctFile:
        Result := fDefaultIndex;
      ctProject:
        Result := MainForm.Project.Options.CompilerSet;
    end;
  end else
    Result := fDefaultIndex;
end;

function TdevCompilerSets.GetDefaultSet: TdevCompilerSet;
var
  Index: integer;
begin
  Index := fDefaultIndex;

  // Range check...
  if (index >= 0) and (index < fList.Count) then
    result := TdevCompilerSet(fList[index])
  else
    result := nil;
end;
{
procedure TdevCompilerSets.OnCompilerSetChanged(OldSet, NewSet: TdevCompilerSet);
var
  I: Integer;
  OldGCCPath, NewGCCPath: String;
begin
  // Get old gcc directory
  if Assigned(OldSet) and (OldSet.BinDir.Count > 0) then
    OldGCCPath := IncludeTrailingPathDelimiter(OldSet.BinDir[0]) + OldSet.gccName;

  // Get new gcc directory
  if Assigned(NewSet) and (NewSet.BinDir.Count > 0) then
    NewGCCPath := IncludeTrailingPathDelimiter(NewSet.BinDir[0]) + NewSet.gccName;

  // If not equal, rescan because it's a different compiler installation
  if NewGCCPath = OldGCCPath then
    Exit;

  with MainForm do begin
    CppParser.Reset;

    // Set include paths
    CppParser.ClearIncludePaths;
    for I := 0 to NewSet.CDir.Count - 1 do
      CppParser.AddIncludePath(NewSet.CDir[I]);
    for I := 0 to NewSet.CppDir.Count - 1 do
      CppParser.AddIncludePath(NewSet.CppDir[I]);
    for I := 0 to NewSet.DefInclude.Count - 1 do // Add default include dirs last, just like gcc does
      CppParser.AddIncludePath(NewSet.DefInclude[I]); // TODO: retrieve those directories in devcfg

    // Set defines
    CppParser.ResetDefines;
    for I := 0 to NewSet.Defines.Count - 1 do
      CppParser.AddHardDefineByLine(NewSet.Defines[i]); // predefined constants from -dM -E

    // Rescan work
    ScanActiveProject;
  end;
end;
}

procedure TdevCompilerSets.LoadSet(index: integer; const SetName: String = '');
var
  key: String;
  I: Integer;
  Item: TdevCompilerSet;

  procedure ReadDirList(list: TStringList; const entry: String);
  var
    i: integer;
  begin
    devData.ReadDelimitedString(key, entry, list);

    // Resolve relative paths
    for I := 0 to list.Count - 1 do
      list[i] := ReplaceFirstStr(list[i], '%path%\', devDirs.Exec);
  end;
begin
  // Can be caused by read error
  if (index < 0) or (index >= fList.Count) then
    Exit;

  // Load the current index from disk
  key := 'CompilerSets_' + IntToStr(index);

  // Programs
  with TdevCompilerSet(fList[index]) do begin

    // Load name from elsewhere in settings file (backward compat etc.)
    if SetName <> '' then
      fName := SetName;

    // Load executable names
    fgccName := devData.ReadS(key, GCC_PROGRAM);
    fgppName := devData.ReadS(key, GPP_PROGRAM);
    fgdbName := devData.ReadS(key, GDB_PROGRAM);
    fmakeName := devData.ReadS(key, MAKE_PROGRAM);
    fwindresName := devData.ReadS(key, WINDRES_PROGRAM);
    fgprofName := devData.ReadS(key, GPROF_PROGRAM);

    // Load the option in string format
    INIOptions := TCompilerOptionsValues.FromString(devData.ReadS(key, 'Options'));

    // Extra parameters
    fCompOpt := devData.ReadS(key, 'CompOpt');
    fLinkOpt := devData.ReadS(key, 'LinkOpt');
    fCompAdd := devData.ReadB(key, 'CompAdd');
    fLinkAdd := devData.ReadB(key, 'LinkAdd');

    // Directories, undo relative stuff
    ReadDirList(fBinDir, 'Bins');
    ReadDirList(fLibDir, 'Lib');
    ReadDirList(fCDir, 'C');
    ReadDirList(fCppDir, 'Cpp');

    // Set properties for current gcc path
    if fBinDir.Count > 0 then begin
      // Check if this directory has already been used to set properties
      for I := 0 to devCompilerSets.Count - 1 do begin
        if I <> index then begin
          Item := devCompilerSets[i];
          if (Item.BinDir.Count > 0) and (Item.BinDir[0] = fBinDir[0]) and (Item.gccName = fgccName) then begin
            fVersion := Item.fVersion;
            fFolder := Item.fFolder;
            fType := Item.fType;
            if fName = '' then
              fName := Item.fName;
            fDumpMachine := Item.fDumpMachine;
            //fDefInclude.Assign(Item.fDefInclude);
            fDefines.Assign(Item.fDefines);
          end else
            SetProperties(fBinDir[0], fgccName); // read properties from disk
        end;
      end;
    end;
  end;
end;

procedure TdevCompilerSets.SaveSet(Index: integer);
var
  key: String;

  procedure WriteDirList(list: TStringList; const entry: String);
  var
    I: integer;
    sl: TStringList;
  begin
    sl := TStringList.Create;
    try
      sl.Assign(list); // work on a local copy
      for I := 0 to sl.Count - 1 do
        sl[i] := ReplaceFirstStr(sl[i], devDirs.Exec, '%path%\');

      // Convert stringlist to string
      devData.WriteDelimitedString(key, entry, sl);
    finally
      sl.Free;
    end;
  end;

begin
  key := 'CompilerSets_' + IntToStr(Index);

  with TdevCompilerSet(fList[index]) do begin

    // Programs
    devData.Write(key, GCC_PROGRAM, fgccName);
    devData.Write(key, GPP_PROGRAM, fgppName);
    devData.Write(key, GDB_PROGRAM, fgdbName);
    devData.Write(key, MAKE_PROGRAM, fmakeName);
    devData.Write(key, WINDRES_PROGRAM, fwindresName);
    devData.Write(key, GPROF_PROGRAM, fgprofName);

    // Save option string
    devData.Write(key, 'Options', INIOptions.ToString);

    // Save extra 'general' options
    devData.Write(key, 'CompOpt', fCompOpt);
    devData.Write(key, 'LinkOpt', fLinkOpt);
    devData.Write(key, 'CompAdd', fCompAdd);
    devData.Write(key, 'LinkAdd', fLinkAdd);

    // Paths
    WriteDirList(fBinDir, 'Bins');
    WriteDirList(fCDir, 'C');
    WriteDirList(fCppDir, 'Cpp');
    WriteDirList(fLibDir, 'Lib');
  end;
end;

function TdevCompilerSets.AddSet: TdevCompilerSet;
begin
  result := TdevCompilerSet.Create;
  fList.Add(result);
end;

function TdevCompilerSets.AddSet(const Folder: String): TdevCompilerSet;
begin
  result := TdevCompilerSet.Create(Folder);
  fList.Add(result);
end;

function TdevCompilerSets.AddSet(compilerset: TdevCompilerSet): TdevCompilerSet;
begin
  result := TdevCompilerSet.Create;
  result.Assign(compilerset); // deep copy
  fList.Add(result);
end;

function TdevCompilerSets.GetSet(index: integer): TdevCompilerSet;
begin
  result := TdevCompilerSet(fList[index]);
end;

procedure TdevCompilerSets.DeleteSet(Index: integer);
var
  I: integer;
begin
  // Erase all sections at and above from disk
  for I := Index to fList.Count - 1 do
    devData.EraseSection('CompilerSets_' + IntToStr(I));

  // Update memory
  TdevCompilerSet(fList[index]).Free;
  fList.Delete(index);

  // Save new memory
  for I := Index to fList.Count - 1 do
    SaveSet(I);
end;

function TdevCompilerSets.Count: integer;
begin
  result := fList.Count;
end;

procedure TdevCompilerSets.LoadSets;
var
  I, SetToActivate: integer;
  sl: TStringList;
  CurrentSet: TdevCompilerSet;
begin
  // Don't append, but replace
  ClearSets;

  sl := TStringList.Create;
  try
    // First list, then current index
    devData.ReadStrings('CompilerSets', sl);

    // Populate list
    SetToActivate := -1;
    for I := 0 to sl.Count - 1 do
      if not SameStr(sl.Names[I], 'Current') then begin
        fList.Add(TdevCompilerSet.Create); // add empty shell
        LoadSet(StrToInt(sl.Names[I]), sl.Values[sl.Names[I]]); // backwards compatibility: save Name here and pass it
      end else
        SetToActivate := StrToIntDef(sl.Values[sl.Names[I]], -1);

    // Activate the current set after everything has been loaded
    DefaultSetIndex := SetToActivate;

    // Validate and load the current set
    CurrentSet := GetDefaultSet;

    // Check if it is usable
    if Assigned(CurrentSet) and CurrentSet.Validate then begin
      SaveSet(DefaultSetIndex);
      if CurrentSet.BinDir.Count > 0 then begin
        CurrentSet.SetProperties(CurrentSet.BinDir[0], CurrentSet.gccName);
      end;
    end;
  finally
    sl.Free;
  end;
end;

procedure TdevCompilerSets.SaveSets;
var
  I: integer;
begin
  // Write all sets separately
  for I := 0 to fList.Count - 1 do
    SaveSet(i);

  // Then save the summary
  SaveSetList;
end;

procedure TdevCompilerSets.SaveSetList;
var
  sl: TStringList;
  I: integer;
begin
  // For the summary, compiler list first
  sl := TStringList.Create;
  try
    for I := 0 to fList.Count - 1 do
      sl.Add(TdevCompilerSet(fList[i]).Name);
    devData.WriteStrings('CompilerSets', sl);
  finally
    sl.Free;
  end;

  // Then current index
  if fDefaultIndex >= fList.Count then
    fDefaultIndex := -1;
  devData.Write('CompilerSets', 'Current', fDefaultIndex);
end;

procedure TdevCompilerSets.FindSets;
var
  BaseSet: TdevCompilerSet;
  BaseName: String;
  option: PCompilerOption;
  index, I: integer;
begin
  // Assume 64bit compilers are put in the TDM-GCC (MinGW64) folder
  TDM_GCC_64_Folder := TPath.Combine(devDirs.Exec, TDM_GCC_64_Folder);

  if TDirectory.Exists(TDM_GCC_64_Folder) then begin

    // we only require GCC.exe to be present
    if TFile.Exists(TPath.Combine(TPath.Combine(TDM_GCC_64_Folder, 'bin'), GCC_PROGRAM)) then begin

      // Default, release profile
      BaseSet := AddSet(TDM_GCC_64_Folder);
      BaseName := BaseSet.Name;
      with BaseSet do begin
        Name := BaseName + ' 64-bit Release';
      end;

      // Debug profile
      with AddSet(BaseSet) do begin
        Name := BaseName + ' 64-bit Debug';
        if FindOption('-g3', option, index) then
          SetOption(option, 1);
      end;

      // Profiling profile
      with AddSet(BaseSet) do begin
        Name := BaseName + ' 64-bit Profiling';
        if FindOption('-pg', option, index) then
          SetOption(option, 1);
      end;

      // Default, 32bit release profile
      BaseSet := AddSet(BaseSet);
      with BaseSet do begin
        Name := BaseName + ' 32-bit Release';
        if FindOption('-', option, index) then // -m is used my -mINSTRUCTIONSET, so use - instead
          SetOption(option, 1);

        // Hack fix, but works...
        fgdbName := GDB32_PROGRAM;
        for I := fLibDir.Count - 1 downto 0 do
          if DirectoryExists(fLibDir[i] + '32') then
            fLibDir[i] := fLibDir[i] + '32'
          else
            fLibDir.Delete(i);
      end;

      // Debug profile
      with AddSet(BaseSet) do begin
        Name := BaseName + ' 32-bit Debug';
        if FindOption('-g3', option, index) then
          SetOption(option, 1);
      end;

      // Profiling profile
      with AddSet(BaseSet) do begin
        Name := BaseName + ' 32-bit Profiling';
        if FindOption('-pg', option, index) then
          SetOption(option, 1);
      end;
    end;
  end;

end;

procedure TdevCompilerSets.ClearSets;
var
  I: integer;
begin
  for I := 0 to fList.Count - 1 do
    TdevCompilerSet(fList[i]).Free;
  fList.Clear;
end;

{ TDevDirs }

constructor TdevDirs.Create;
begin
  inherited Create;
  SettoDefaults;
  LoadSettings;
end;

procedure TdevDirs.SettoDefaults;
var
  DocumentsPath: array[0..MAX_PATH] of Char;
const
  CSIDL_MYDOCUMENTS = $05;
begin
  fExec := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
  fConfig := fExec;

  fHelp := fExec + HELP_DIR;
  fIcons := fExec + ICON_DIR;
  fLang := fExec + LANGUAGE_DIR;
  fTemp := fExec + TEMPLATE_DIR;
  fThemes := fExec + THEME_DIR;

  if devData.IsPortable then
  begin
    fDefault := TPath.Combine(fExec, 'Projects');
    TDirectory.CreateDirectory(fDefault);
  end
  else
  begin
    // Get my documents folder
    if SHGetSpecialFolderPath(Application.Handle, DocumentsPath, CSIDL_MYDOCUMENTS, false) then
      fDefault := DocumentsPath
    else
      fDefault := fExec;
  end;
end;

procedure TdevDirs.LoadSettings;
begin
  devData.ReadObject('Directories', Self);

  fConfig := ExtractFilePath(devData.INIFileName);
  fHelp := ReplaceFirstStr(fHelp, '%path%\', fExec);
  fIcons := ReplaceFirstStr(fIcons, '%path%\', fExec);
  fLang := ReplaceFirstStr(fLang, '%path%\', fExec);
  fTemp := ReplaceFirstStr(fTemp, '%path%\', fExec);
  fThemes := ReplaceFirstStr(fThemes, '%path%\', fExec);
end;

procedure TdevDirs.SaveSettings;
begin
  fHelp := ReplaceFirstStr(fHelp, fExec, '%path%\');
  fIcons := ReplaceFirstStr(fIcons, fExec, '%path%\');
  fLang := ReplaceFirstStr(fLang, fExec, '%path%\');
  fTemp := ReplaceFirstStr(fTemp, fExec, '%path%\');
  fThemes := ReplaceFirstStr(fThemes, fExec, '%path%\');

  devData.WriteObject('Directories', Self);

  fHelp := ReplaceFirstStr(fHelp, '%path%\', fExec);
  fIcons := ReplaceFirstStr(fIcons, '%path%\', fExec);
  fLang := ReplaceFirstStr(fLang, '%path%\', fExec);
  fTemp := ReplaceFirstStr(fTemp, '%path%\', fExec);
  fThemes := ReplaceFirstStr(fThemes, '%path%\', fExec);
end;

constructor TdevEditor.Create;
begin
  inherited;

  fFont := TFont.Create;
  fGutterfont := TFont.Create;
  fSyntax := TStringList.Create;
  TStringList(fSynTax).Duplicates := dupIgnore;

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

function TdevEditor.GetNewDocEncoding: TEncoding;
begin
  Result := StandardEncodingFromName(fNewDocEncoding);
end;

procedure TdevEditor.LoadSettings;
begin
  devData.ReadObject('Editor', Self);
end;

procedure TdevEditor.SaveSettings;
begin
  devData.WriteObject('Editor', Self);
end;

procedure TdevEditor.SettoDefaults;
begin
  // General
  fAutoIndent := TRUE;
  fAddIndent := TRUE;
  fInsertMode := TRUE;
  fUseTabs := TRUE;
  fSmartTabs := FALSE;
  fGroupUndo := TRUE;
  fInsDropFiles := FALSE;
  fSpecialChar := FALSE;

  // General #2
  fEHomeKey := TRUE;
  fPastEOF := FALSE;
  fPastEOL := FALSE;
  fFindText := TRUE;
  fShowScrollbars := TRUE; // Show as needed
  fHalfPage := FALSE;
  fShowScrollHint := TRUE;
  fParserHints := TRUE; // Editor hints
  fShowFunctionTip := TRUE;
  fTrimTrailingSpaces := FALSE;
  fNewDocEncoding := DefaultEncodingNames[0]; //ANSI
  
  // Caret
  fInsertCaret := 0;
  fOverwriteCaret := 3;
  fMatch := TRUE;

  // Margin
  fMarginVis := False;
  fMarginSize := 80; // disable by default, receiving lots of complaints about it enabled
  fMarginColor := cl3DLight;

  // Misc.
  fUseSyn := TRUE;
  fSynExt := 'c;cpp;h;hpp;cc;cxx;cp;hp;rh;fx;inl;tcc;win;;'; //last ; is for files with no extension
  fHighCurrLine := TRUE;
  fHighColor := $FFFFCC; // Light Turquoise
  fTabSize := 4;

  // Display
  fFont.name := 'Consolas';
  fFont.Size := 10;
  //fFont.name := 'Source Code Pro';
  //fFont.Size := 10;
  fShowLigatures := True;

  // Display #2
  fShowGutter := TRUE;
  fGutterAuto := TRUE;
  fCustomGutter := FALSE;
  fLineNumbers := TRUE;
  fFirstisZero := FALSE;
  fLeadZero := FALSE;
  fGutterFont.Name := 'Consolas';
  fGutterFont.Size := 10;
  //fGutterFont.name := 'Source Code Pro';
  //fGutterFont.Size := 10;
  fGutterSize := 1;

  // Autosave
  fEnableAutoSave := FALSE;
  Interval := 10;
  fAutoSaveFilter := 0;
  fAutoSaveMode := 0;

  // Symbol completion
  fCompleteSymbols := TRUE;
  fBraceComplete := TRUE;
  fParentheseComplete := TRUE;
  fIncludeComplete := TRUE;
  fArrayComplete := TRUE;
  fCommentComplete := FALSE;
  fSingleQuoteComplete := TRUE;
  fDoubleQuoteComplete := TRUE;
  fDeleteSymbolPairs := TRUE;
end;

procedure TdevEditor.AssignEditor(editor: TSynEdit; const FileName: String);
var
  pt: TPoint;
begin
  with Editor do begin
    BeginUpdate;
    try
      // Set text area properties
      WantTabs := True;
      //MaxScrollWidth := 4096; // bug-fix #600748
      MaxUndo := 4096;
      BorderStyle := bsNone;
      Font.Quality := TFontQuality.fqClearType; //FontSmoothing := fsmClearType;

      // Select highlighter based on filename (a lot depends on this)
      Highlighter := dmMain.GetHighlighter(FileName);
      TabWidth := fTabSize;
      Font.Assign(fFont);

      // Set selection color
      if Assigned(Highlighter) then begin
        StrtoPoint(pt, devEditor.Syntax.Values[cSel]);
        SelectedColor.Background := pt.X;
        SelectedColor.Foreground := pt.Y;
      end else begin // editor not colored, pick defaults
        SelectedColor.Background := clNavy; //StyleServices.GetStyleColor(scGenericBackground);// clNavy;
        SelectedColor.Foreground := clWhite; //StyleServices.GetStyleFontColor(sfWindowTextNormal); // clWhite
      end;

      // Set code folding
      if Assigned(Highlighter) then begin
        StrtoPoint(pt, devEditor.Syntax.Values[cFld]);
        CodeFolding.FolderBarLinesColor := pt.y;
        if Editor is TSynEditEx then
          OnScanForFoldRanges := TSynEditEx(EDitor).DoScanForFoldRanges;
        UseCodeFolding := True;
      end else begin
        UseCodeFolding := False;
        OnScanForFoldRanges := nil;
      end;

      // More stuff
      if fMarginVis then
        RightEdge := fMarginSize
      else
        RightEdge := 0;
      RightEdgeColor := fMarginColor;
      InsertCaret := TSynEditCaretType(fInsertCaret);
      OverwriteCaret := TSynEditCaretType(fOverwriteCaret);
      ScrollHintFormat := shfTopToBottom;
      if HighCurrLine and Assigned(Highlighter) then
        ActiveLineColor := HighColor
      else
        ActiveLineColor := clNone;

      // Set gutter properties
      with Gutter do begin
        LeftOffset := 4;
        RightOffset := 21;
        BorderStyle := gbsNone;
        Font.Assign(fGutterFont);
        DigitCount := fGutterSize;
        Visible := fShowGutter;
        AutoSize := fGutterAuto;
        ShowLineNumbers := fLineNumbers;
        LeadingZeros := fLeadZero;
        ZeroStart := fFirstisZero;
        if Assigned(Highlighter) then begin
          StrtoPoint(pt, fSyntax.Values[cGut]);
          Color := pt.x;
          Font.Color := pt.y;
        end else begin // editor not colored, pick defaults
          Color := clBtnFace;// StyleServices.GetStyleColor(scGenericBackground); //clBtnFace;
          Font.Color := clBlack; //StyleServices.GetStyleFontColor(sfWindowTextNormal); //clBlack;
        end;
      end;

      // Set option enum
      Options := [
        eoAltSetsColumnMode, eoDisableScrollArrows,
        eoDragDropEditing, eoDropFiles, eoKeepCaretX, eoTabsToSpaces,
        eoRightMouseMovesCursor, eoScrollByOneLess {eoAutoSizeMaxScrollWidth}
        ];

      // Optional synedit options in devData
      if fAutoIndent then
        Options := Options + [eoAutoIndent];
      if (Editor is TSynEditEx) then
        TSynEditEx(Editor).eoAddIndent := fAddIndent;
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
        Options := Options + [eoScrollHintFollows, eoShowScrollHint];
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
      if fShowLigatures then
        Options := Options + [eoShowLigatures];

    finally
      EndUpdate;
    end;
  end;
end;

constructor TdevCodeCompletion.Create;
begin
  inherited Create;
  SettoDefaults;
  LoadSettings;
end;

destructor TdevCodeCompletion.Destroy;
begin
end;

procedure TdevCodeCompletion.LoadSettings;
begin
  devData.ReadObject('CodeCompletion', Self);
end;

procedure TdevCodeCompletion.SaveSettings;
begin
  devData.WriteObject('CodeCompletion', Self);
end;

procedure TdevCodeCompletion.SettoDefaults;
begin
  fWidth := 320;
  fHeight := 240;
  fDelay := 180;
  fBackColor := clWindow;
  fEnabled := True;
  fParseLocalHeaders := True;
  fParseGlobalHeaders := True;
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
  devData.ReadObject('ClassBrowsing', Self);
end;

procedure TdevClassBrowsing.SaveSettings;
begin
  devData.WriteObject('ClassBrowsing', Self);
end;

procedure TdevClassBrowsing.SettoDefaults;
begin
  fShowFilter := 2; // sfCurrent
  fShowInheritedMembers := False;
end;

{ TdevFormatter }

constructor TdevFormatter.Create;
begin
  inherited Create;
  SettoDefaults;
  LoadSettings;
end;

procedure TdevFormatter.LoadSettings;
begin
  devData.ReadObject('Formatter', Self);
end;

procedure TdevFormatter.SaveSettings;
begin
  devData.WriteObject('Formatter', Self);
end;

procedure TdevFormatter.SettoDefaults;
begin
  fBracketStyle := 2; // Java
  fIndentStyle := 2; // Tabs
  fTabWidth := 4;
  fModifyMaxLineLength := False;
  fMaxLineLength := 80;
  fIndentClasses := True;
  fIndentSwitches := True;
  fIndentCases := False;
  fIndentNamespaces := True;
  fIndentLabels := False;
  fIndentPreprocessor := True;
  fFullCommand := GetFullCommand; // includes customizations
  fAStyleDir := 'AStyle\';
  fAStyleFile := 'AStyle.exe';
end;

function TdevFormatter.GetFullCommand: String;
begin
  Result := '';

  // Add bracket style
  if fBracketStyle > 0 then
    Result := Result + ' -A' + IntToStr(fBracketStyle);

  // Add indent style and tab width
  case fIndentStyle of
    1: Result := Result + ' --indent=spaces=' + IntToStr(fTabWidth);
    2: Result := Result + ' --indent=tab=' + IntToStr(fTabWidth);
    3: Result := Result + ' --indent=force-tab=' + IntToStr(fTabWidth);
    4: Result := Result + ' --indent=force-tab-x=' + IntToStr(fTabWidth);
  end;

  // Add line length
  if fModifyMaxLineLength then
    Result := Result + ' --max-code-length=' + IntToStr(fMaxLineLength);

  // Add indentation options
  if fIndentClasses then
    Result := Result + ' --indent-classes';
  if fIndentSwitches then
    Result := Result + ' --indent-switches';
  if fIndentCases then
    Result := Result + ' --indent-cases';
  if fIndentNamespaces then
    Result := Result + ' --indent-namespaces';
  if fIndentLabels then
    Result := Result + ' --indent-labels';
  if fIndentPreprocessor then
    Result := Result + ' --indent-preprocessor';

  Result := TrimLeft(Result);
end;

function TdevFormatter.Validate: Boolean;
begin
  Result := False;

  // Check if AStyle.exe is where it should be
  if not DirectoryExists(devDirs.Exec + fAStyleDir) then
    Exit;
  if not FileExists(devDirs.Exec + fAStyleDir + fAStyleFile) then
    Exit;

  Result := True;
end;

function TdevFormatter.FormatMemory(Editor: TEditor; const OverrideCommand: String): String;
var
  FileName: String;
  DummyEditor: TSynEdit;
begin
  // Store file backup in AStyle dir and format that file
  FileName := TPath.Combine(TPath.GetTempPath,ExtractFileName(Editor.FileName)); //devDirs.Exec + fAStyleDir
  Editor.Text.Lines.SaveToFile(FileName);
  FormatFile(FileName, OverrideCommand);

  // Load formatted file into dummy
  DummyEditor := TSynEdit.Create(nil);
  try
    // Use replace selection trick to preserve undo list
    DummyEditor.Lines.LoadFromFile(FileName);

    // Use replace all functionality
    Editor.Text.BeginUpdate;
    try
      Editor.Text.SelectAll;
      Editor.Text.SelText := DummyEditor.Lines.Text; // do NOT use Lines.LoadFromFile which is not undo-able
    finally
      Editor.Text.EndUpdate; // repaint once
    end;
  finally
    DummyEditor.Free;
  end;
end;

function TdevFormatter.FormatFile(const FileName, OverrideCommand: String): String;
var
  RunCommand, WorkingDir: String;
begin
  WorkingDir := devDirs.Exec + fAStyleDir;
  RunCommand := fAStyleFile + ' ' + OverrideCommand + ' "' + FileName + '"';
  Result := RunAndGetOutput(WorkingDir + RunCommand, TPath.GetTempPath, nil, nil, False);
end;

function TdevFormatter.GetVersion: String;
var
  RunCommand, WorkingDir: String;
begin
  WorkingDir := devDirs.Exec + fAStyleDir;
  RunCommand := fAStyleFile + ' --version';
  Result := RunAndGetOutput(WorkingDir + RunCommand, TPath.GetTempPath, nil, nil, False);
end;

{ TdevExternalPrograms }

function TdevExternalPrograms.AddProgram(ext, prog: String): integer;
var
  idx: integer;
begin
  if ext = '' then begin
    Result := -1;
    Exit;
  end;

  idx := AssignedProgram(ext);
  if idx = -1 then
    Result := fPrograms.Add(ext + '=' + prog)
  else begin
    fPrograms.Values[fPrograms.Names[idx]] := prog;
    Result := idx;
  end;
end;

function TdevExternalPrograms.AssignedProgram(const ext: String): integer;
var
  I: integer;
begin
  Result := -1;
  for I := 0 to fPrograms.Count - 1 do
    if SameText(fPrograms.Names[I], ext) then begin
      Result := I;
      Break;
    end;
end;

constructor TdevExternalPrograms.Create;
begin
  inherited Create;
  fPrograms := TStringList.Create;
  SettoDefaults;
  LoadSettings;
end;

destructor TdevExternalPrograms.Destroy;
begin
  fPrograms.Free;
end;

function TdevExternalPrograms.GetProgramName(Index: integer): String;
begin
  Result := fPrograms.Values[fPrograms.Names[Index]];
end;

procedure TdevExternalPrograms.LoadSettings;
begin
  devData.ReadObject('ExternalPrograms', Self);
end;

procedure TdevExternalPrograms.SaveSettings;
begin
  devData.WriteObject('ExternalPrograms', Self);
end;

procedure TdevExternalPrograms.SetToDefaults;
begin
  inherited;
end;

end.

