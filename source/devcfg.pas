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

(*
 unit: devCFG.pas
 programmer: Michael Berg
 date: 9.1.1 - 12.10.1
 description: Singletion pattern object for control of dev-c options.
                To initialize call initializeOptions function.
                To save call SaveOptions function.

 ToUse: just call the option you want:
           i.e. devDirs.ExecDir:= NewValue;
           or   DevCppDir:=  devDirs.ExecDir;

        do not create instance explicitly:
           i.e. opt:= TdevData.Create;
*)
unit devcfg;

interface

uses
{$IFDEF WIN32}
  Dialogs, Windows, Classes, Graphics, SynEdit, CFGData, CFGTypes, IniFiles, prjtypes;
{$ENDIF}
{$IFDEF LINUX}
  QDialogs, Classes, QGraphics, QSynEdit, CFGData, CFGTypes, IniFiles, prjtypes;
{$ENDIF}

const
  BoolValYesNo: array[boolean] of string = ('No', 'Yes');
  BoolVal10: array[0..27] of string = ('0', '1', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h',   // Had to use letters for multiple choices
                                       'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
                                       's', 't', 'u', 'v', 'w', 'x', 'y', 'z');

type
  // the comments are an example of the record
  PCompilerOption = ^TCompilerOption;
  TCompilerOption = packed record
    optName: string; // "Generate debugging info"
    optIsGroup: boolean; // False
    optIsC: boolean; // True  (C option?)
    optIsCpp: boolean; // True (C++ option?) - can be both C and C++ option...
    optIsLinker: boolean; // Is it a linker param
    optValue: Integer; // True
    optSetting: string; // "-g3"
    optSection: string; // "Linker options"
    optExcludeFromTypes: TProjTypeSet; // [dptGUI] (don't show option if project is of type dptGUI)
    optChoices : TStringList; // replaces "Yes/No" standard choices (max 26 different choices)
  end;

 // compiler-set configuration
 TdevCompilerSet = class(TCFGOptions)
 private
   fSets: TStrings;
   fgccName : string;
   fgppName : string;
   fgdbName : string;
   fmakeName : string;
   fwindresName : string;
   fdllwrapName : string;
   fgprofName : string;
   fBinDir : string;
   fCDir : string;
   fCppDir : string;
   fLibDir : string;
   fOptions : string;
   //RNC
   fCompAdd: boolean;          // add fcmdopts to compiler command line
   fLinkAdd: boolean;          // add flinkopts to linker command line
   fCmdOptions : string;
   fLinkOptions : string;

   procedure WriteSets;
   procedure UpdateSets;
 public
   constructor Create;
   destructor Destroy; override;
   procedure SettoDefaults; override;
   procedure SaveSettings; override;
   procedure LoadSettings; override;
   procedure SaveSet(Index: integer);
   procedure SaveSetDirs(Index: integer);
   procedure SaveSetProgs(Index: integer);
   procedure LoadSet(Index: integer);
   procedure LoadSetDirs(Index: integer);
   procedure LoadSetProgs(Index: integer);
   procedure AssignToCompiler;
   property Name;
   function SetName(Index: integer): string;
   property Sets: TStrings read fSets write fSets;
 published
   property gccName: string read fgccName write fgccName;
   property gppName: string read fgppName write fgppName;
   property gdbName: string read fgdbName write fgdbName;
   property makeName: string read fmakeName write fmakeName;
   property windresName: string read fwindresName write fwindresName;
   property dllwrapName: string read fdllwrapName write fdllwrapName;
   property gprofName: string read fgprofName write fgprofName;
   property BinDir: string read fBinDir write fBinDir;
   property CDir: string read fCDir write fCDir;
   property CppDir: string read fCppDir write fCppDir;
   property LibDir: string read fLibDir write fLibDir;
   property OptionsStr: string read fOptions write fOptions;
//RNC
   property CmdOpts: string read fCmdOptions write fCmdOptions;
   property LinkOpts: string read fLinkOptions write fLinkOptions;
   property AddtoComp: boolean read fCompAdd write fCompAdd;
   property AddtoLink: boolean read fLinkAdd write fLinkAdd;

 end;

 // compiler options
 TdevCompiler = class(TCFGOptions)
  private
   fUseParams: boolean;        // Use fparams when running prog
   fIntermediate: string;      // directory for mid-compile files -- if needed
   fOutputDir: string;         // directory to place compiled files
   fRunParams: string;         // params to send on execution

   // program filenames
   fgccName : string;
   fgppName : string;
   fgdbName : string;
   fmakeName : string;
   fwindresName : string;
   fdllwrapName : string;
   fgprofName : string;
   fCompilerSet: integer;

   //Compiler options
   fOptions: TList;

   //Makefile
   fFastDep: Boolean;

   //Debugger
   fModified: boolean;         // has options been changed since last compile

   fcmdOpts: string;           // command-line adds for compiler
   flinkopts: string;          // command-line adds for linker

   fSaveLog: boolean;          // Save Compiler Output
   fDelay: integer;            // delay in milliseconds -- for compiling
    procedure SetCompilerSet(const Value: integer);
    function GetOptions(Index: integer): TCompilerOption;
    procedure SetOptions(Index: integer; const Value: TCompilerOption);
    function GetOptionStr: string;
    procedure SetOptionStr(const Value: string);
  protected
    procedure AddDefaultOptions;
  public
   constructor Create;
   destructor Destroy; override;
   procedure SettoDefaults; override;
   procedure SaveSettings; override;
   procedure LoadSettings; override;
   property Name;
   property Modified: boolean read fModified write fModified;
    procedure AddOption(_Name: string; _IsGroup, _IsC, _IsCpp, IsLinker: boolean; _Value: integer; _Setting, _Section: string; ExcludeFromTypes: TProjTypeSet; Choices: TStringList);
    function OptionsCount: integer;
    procedure ClearOptions;
    procedure DeleteOption(Index: integer);
    property Options[Index: integer]: TCompilerOption read GetOptions write SetOptions;
    property OptionStr: string read GetOptionStr write SetOptionStr;
    function FindOption(Setting: string; var opt: TCompilerOption; var Index: integer): boolean; // returns the option with setting=<Setting>
    procedure ChangeOptionsLang;
    function ConvertCharToValue(c : char) : integer;
  published
   property FastDep: Boolean read fFastDep write fFastDep;

//   property AddtoComp: boolean read fCompAdd write fCompAdd;
//   property AddtoLink: boolean read fLinkAdd write fLinkAdd;
//RNC
//   property CmdOpts: string read fCmdOpts write fCmdOpts;
//   property LinkOpts: string read fLinkopts write fLinkOpts;

   property RunParams: string read fRunParams write fRunParams;
   property OutputDir: string read fOutputDir write fOutputDir;          // ** unused
   property Intermediate: string read fIntermediate write fIntermediate; // ** unused
   property UseExecParams: boolean read fUseParams write fUseParams;
   property SaveLog: boolean read fSaveLog write fSaveLog;
   property Delay: integer read fDelay write fDelay;

   property gccName: string read fgccName write fgccName;
   property gppName: string read fgppName write fgppName;
   property gdbName: string read fgdbName write fgdbName;
   property makeName: string read fmakeName write fmakeName;
   property windresName: string read fwindresName write fwindresName;
   property dllwrapName: string read fdllwrapName write fdllwrapName;
   property gprofName: string read fgprofName write fgprofName;

   property CompilerSet: integer read fCompilerSet write SetCompilerSet;

 end;

 // code-completion window size and other config
 TdevCodeCompletion = class(TCFGOptions)
 private
   fWidth: integer;
   fHeight: integer;
   fDelay: integer;
   fBackColor: integer;
   fEnabled: boolean;
   fUseCacheFiles: boolean;
   fCacheFiles: TStrings;
   procedure SetDelay(Value: integer);
 public
   constructor Create;
   destructor Destroy; override;
   procedure SettoDefaults; override;
   procedure SaveSettings; override;
   procedure LoadSettings; override;
   property Name;
 published
   property Width: integer read fWidth write fWidth;
   property Height: integer read fHeight write fHeight;
   property Delay: integer read fDelay write SetDelay;
   property BackColor: integer read fBackColor write fBackColor;
   property Enabled: boolean read fEnabled write fEnabled;
   property UseCacheFiles: boolean read fUseCacheFiles write fUseCacheFiles;
   property CacheFiles: TStrings read fCacheFiles write fCacheFiles;
 end;

 // class-browsing view style
 TdevClassBrowsing = class(TCFGOptions)
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
   procedure SettoDefaults; override;
   procedure SaveSettings; override;
   procedure LoadSettings; override;
   property Name;
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
 TdevCVSHandler = class(TCFGOptions)
 private
   fRepositories: TStrings;
   fExecutable: string;
   fCompression: byte;
   fUseSSH: boolean;
 public
   constructor Create;
   destructor Destroy; override;
   procedure SettoDefaults; override;
   procedure SaveSettings; override;
   procedure LoadSettings; override;
   property Name;
 published
   property Repositories: TStrings read fRepositories write fRepositories;
   property Executable: string read fExecutable write fExecutable;
   property Compression: byte read fCompression write fCompression;
   property UseSSH: boolean read fUseSSH write fUseSSH;
 end;

 TdevExternalPrograms = class(TCFGOptions)
 private
   fDummy: boolean;
   fPrograms: TStrings;
   function GetProgramName(Index: integer): string;
 public
   constructor Create;
   destructor Destroy; override;
   procedure SaveSettings; override;
   procedure LoadSettings; override;
   procedure SetToDefaults; override;
   property Name;
   property ProgramName[Index: integer]: string read GetProgramName;
   function AssignedProgram(ext: string): integer;
   function AddProgram(ext, prog: string): integer;
 published
   property Dummy: boolean read fDummy write fDummy;
   property Programs: TStrings read fPrograms write fPrograms;
 end;

 // global directories
 TdevDirs = class(TCFGOptions)
  private
   fThemes: string;            // Themes Directory
   fIcons: string;             // Icon Library
   fHelp: string;              // Help
   fLang: string;              // Language
   fTemp: string;              // Templates
   fDefault: string;           // user defined default
   fExec: string;              // dev-c start
   fConfig : string;           // config files directory
   fBinDir: string;            // compiler location
   fCDir: string;              // c includes
   fCppDir: string;            // c++ includes
   fLibDir: string;            // Libraries
   fMingw: string;             // Mingw root -- should be set in installer if mingw included
   fOldPath: string;           // Enviroment Path at program start
   procedure FixPaths;
  public
   constructor Create;
   procedure SettoDefaults; override;
   procedure SaveSettings; override;
   procedure LoadSettings; override;
   property Name;
   property OriginalPath: string read fOldPath write fOldPath;
  published
   property Exec: string read fExec write fExec;
   property Config: string read fConfig write fConfig;
   property Bins: string read fBinDir write fBinDir;
   property Default: string read fDefault write fDefault;
   property C: string read fCDir write fCDir;
   property Cpp: string read fCppDir write fCppDir;
   property Help: string read fHelp write fHelp;
   property Icons: string read fIcons write fIcons;
   property Lang: string read fLang write fLang;
   property Lib: string read fLibDir write fLibDir;
   property Templates: string read fTemp write fTemp;
   property Themes: string read fThemes write fThemes;
  end;

 // editor options -- syntax, synedit options, etc...
 TdevEditor = class(TCFGOptions)
  private
   fUseSyn: boolean;           // use syntax highlighting
   fSynExt: string;            // semi-colon seperated list of highlight ext's
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
   fTrailBlanks: boolean;      // Blanks past EOL are not trimmed
   fdblLine: boolean;          // Double Click selects a line
   fFindText: boolean;         // Text at cursor defaults in find dialog
   fEHomeKey: boolean;         // Home key like visual studio
   fGroupUndo: boolean;        // treat same undo's as single undo
   fInsDropFiles: boolean;     // Insert files when drag/dropped else open
   fInsertMode: boolean;       // Editor defaults to insert mode
   fAutoIndent: boolean;       // Auto-indent code lines
   fSmartTabs: boolean;        // Tab to next no whitespace char
   fSmartUnindent: boolean;    // on backspace move to prev non-whitespace char
   fSpecialChar: boolean;      // special line characters visible
   fAppendNewline: boolean;    // append newline character to the end of line
   fTabtoSpaces: boolean;      // convert tabs to spaces
   fAutoCloseBrace: boolean;   // insert closing braces
   fMarginColor: TColor;       // Color of right margin
   fSyntax: TStrings;          // Holds attributes settings
   fDefaultIntoPrj: boolean;   // Insert Default Source Code into "empty" project
   fParserHints: boolean;      // Show parser's hint for the word under the cursor
   fMatch : boolean;           // Highlight matching parenthesis
   fHighCurrLine: boolean;     // Highlight current line
   fHighColor: TColor;         // Color of current line when highlighted

  public
   constructor Create;
   destructor Destroy; override;
   procedure SettoDefaults; override;
   procedure SaveSettings; override;
   procedure LoadSettings; override;
   procedure AssignEditor(Editor: TSynEdit);
   property Name;
  published
   //Editor props
   property AutoIndent: boolean read fAutoIndent write fAutoIndent;
   property InsertMode: boolean read fInsertMode write fInsertMode;
   property TabToSpaces: boolean read fTabToSpaces write fTabtoSpaces;
   property SmartTabs: boolean read fSmartTabs write fSmartTabs;
   property SmartUnindent: boolean read fSmartUnindent write fSmartUnindent;
   property TrailBlank: boolean read fTrailBlanks write fTrailBlanks;
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
   property AppendNewline: boolean read fAppendNewline write fAppendNewline;
   property AutoCloseBrace: boolean read fAutoCloseBrace write fAutoCloseBrace;

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
   property SyntaxExt: string read fSynExt write fSynExt;
   property Syntax: TStrings read fSyntax write fSyntax;

   // other
   property DefaulttoPrj: boolean read fDefaultIntoPrj write fDefaultIntoPrj;

   property ParserHints: boolean read fParserHints write fParserHints;
   property Match: boolean read fMatch write fMatch;
   property HighCurrLine: boolean read fHighCurrLine write fHighCurrLine;
   property HighColor: TColor read fHighColor write fHighColor;

 end;

 // master option object -- contains program globals
 TdevData = class(TConfigData)
  private
   fVersion: string;                 // The configuration file's version
   fLang: string;                    // Language file
   fTheme: string;                   // Theme file
   fFindCols: string;                // Find Column widths (comma sep)
   fCompCols: string;                // Compiler Column Widths (comma sep)
   fMsgTabs: boolean;                // Message Control Tabs (Top/Bottom)
   fMinOnRun: boolean;               // Minimize IDE on run
   fOpenStyle: integer;              // Open Dialog Style
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
   fShowMenu: boolean;               // Show Main Menu in Full Screen Mode
   fDefCpp: boolean;                 // Default to C++ project (compile with g++)
   fFirst: boolean;                  // first run of dev-c
   fSplash: string;                  // user selected splash screen
   fWinPlace: TWindowPlacement;      // Main forms size, state and position.
   fdblFiles: boolean;               // double click opens files out of project manager
   fLangChange: boolean;             // flag for language change
   fthemeChange: boolean;            // did the theme changed
   fNoSplashScreen : boolean;        // disable splash screen   
   fToolbarMain: boolean;
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
   fToolbarOptions: boolean;
   fToolbarOptionsX: integer;
   fToolbarOptionsY: integer;
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
   // tip of the day
   fShowTipsOnStart: boolean;
   fLastTip: integer;
   fXPTheme : boolean;                  // Use XP theme
   fFileDate : integer;                 // Dev-C++ File Date for checking old configurations
   fShowProgress : boolean;             // Show progress window during compile
   fAutoCloseProgress : boolean;        // Auto close progress bar window after compile
   // Printer
   fPrintColors         : boolean;      // print colors
   fPrintHighlight      : boolean;
   fPrintWordWrap       : boolean;
   fPrintLineNumbers    : boolean;
   fPrintLineNumbersMargins : boolean;

   // Debug variable browser
   fWatchHint           : boolean;      // watch variable under mouse
   fWatchError          : boolean;      // report watch errors
  public
   constructor Create(aOwner: TComponent); override;
   destructor Destroy; override;
   procedure SettoDefaults; override;
   procedure SaveConfigData; override;
   procedure ReadConfigData; override;

   class function DevData: TDevData;
   property WindowPlacement: TWindowPlacement read fWinPlace write fWinPlace;
   property LangChange: boolean read fLangChange write fLangChange;
   property ThemeChange: boolean read fThemeChange write fThemeChange;
  published
   property Version: string read fVersion write fVersion;
   property Language: string read fLang write fLang;
   property Theme: string read fTheme write fTheme;
   property First: boolean read fFirst write fFirst;
   property Splash: string read fSplash write fSplash;
   property MRUMax: integer read fMRUMax write fMRUMax;
   property DblFiles: boolean read fDblFiles write fDblFiles;
   property NoSplashScreen: boolean read fNoSplashScreen write fNoSplashScreen;

   //Execution
   property MinOnRun: boolean read fMinOnRun write fMinOnRun;
   property OpenStyle: integer read fOpenStyle write fOpenStyle;

   property BackUps: boolean read fBackup write fBackup;
   property AutoOpen: integer read fAutoOpen write fAutoOpen;

   //Windows
   property MsgTabs: boolean read fMsgTabs write fMsgTabs;

   property ShowBars: boolean read fShowbars write fShowbars;
   property ShowMenu: boolean read fShowMenu write fShowMenu;

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
   property FindCols: string read fFindCols write fFindCols;
   property CompCols: string read fCompCols write fCompCols;

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
   property ToolbarOptions: boolean read fToolbarOptions write fToolbarOptions;
   property ToolbarOptionsX: integer read fToolbarOptionsX write fToolbarOptionsX;
   property ToolbarOptionsY: integer read fToolbarOptionsY write fToolbarOptionsY;
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

   property XPTheme: boolean read fXPTheme write fXPTheme;
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

   // Variable debug browser
   property WatchHint : boolean read fWatchHint write fWatchHint;
   property WatchError : boolean read fWatchError write fWatchError;
 end;

function DevData: TdevData;

procedure InitializeOptions;
procedure SaveOptions;
procedure FinalizeOptions;
procedure ResettoDefaults;
procedure CheckForAltConfigFile(filename: string);
procedure UpdateAltConfigFile;

var
 devCompiler: TdevCompiler = nil;
 devCompilerSet: TDevCompilerSet = nil;
 devDirs: TdevDirs = nil;
 devEditor: TdevEditor = nil;
 devCodeCompletion: TdevCodeCompletion = nil;
 devClassBrowsing: TdevClassBrowsing = nil;
 devCVSHandler: TdevCVSHandler = nil;
 devExternalPrograms: TdevExternalPrograms = nil;

 // Permanent alternate config file (need to be global vars)
 ConfigMode          : (CFG_NORMAL, CFG_PARAM, CFG_USER) = CFG_NORMAL;
 StandardConfigFile  : string;
 UseAltConfigFile    : boolean;
 AltConfigFile       : string;

implementation

uses
{$IFDEF WIN32}
  MultiLangSupport, SysUtils, Forms, Controls, version, utils, SynEditMiscClasses,
  datamod, FileAssocs;
{$ENDIF}
{$IFDEF LINUX}
  MultiLangSupport, SysUtils, QForms, QControls, version, utils, QSynEditMiscClasses,
  datamod, FileAssocs, Types;
{$ENDIF}

function ValidatePaths(dirList: String; var badDirs: String): String;
//checks if directories in provided ; delimited list exist
//returns filtered out dirList with only existing paths
//badDirs returns ; delimited list of non existing dirs
//also remove duplicates and empty entries
var
  strs: TStrings;
  i,j: Integer;
  currdir: String;

  function makeFullPath(dir: String): String;
  begin
    Result := dir;
    //check if full path
{$IFDEF WIN32}
    if Length(dir) > 1 then
      if dir[2] = ':' then
        Exit;
    if Length(dir) > 0 then
      if dir[1] = '\' then
        Exit;
{$ENDIF}
{$IFDEF LINUX}
    if Length(dir) > 0 then
      if dir[1] = '/' then
        Exit;
{$ENDIF}
    //otherwise just add path
    Result := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))
      + Result;
  end;

begin
  Result := '';
  badDirs := '';

  //needed to confirm relative paths
  currdir := GetCurrentDir;
  SetCurrentDir(ExtractFilePath(Application.ExeName));

  strs := TStringList.Create;
  repeat
    if Pos(';', dirList) = 0 then
      strs.Add(dirList)
    else
    begin
      strs.Add(Copy(dirList, 1, Pos(';', dirList) -1));
      Delete(dirList, 1, Pos(';', dirList));
    end;
  until Pos(';', dirList) = 0;

  //eliminate duplicates
  for i := strs.Count -2 downto 0 do
    for j := strs.Count -1 downto i + 1 do
      if (Trim(strs[j]) = '') or
        ( makeFullPath(Trim(strs[i])) = makeFullPath(Trim(strs[j])) ) then
          strs.Delete(j);

  //check the directories
  for i := strs.Count -1 downto 0 do
  begin
    if DirectoryExists(strs[i]) then
      Result := Result + ';' + strs[i]
    else
      badDirs := badDirs + ';' + strs[i];
  end;

  if Length(Result) > 0 then
    if Result[1] = ';' then
      Delete(Result, 1, 1);
  if Length(badDirs) > 0 then
    if badDirs[1] = ';' then
      Delete(badDirs, 1, 1);

  FreeAndNil(strs);

  SetCurrentDir(currdir);
end;

procedure InitializeOptions;
begin
  if not assigned(devDirs) then
  devDirs:= TdevDirs.Create;

  if not assigned(devCompilerSet) then
  devCompilerSet:= TdevCompilerSet.Create;

  if not assigned(devCompiler) then
  devCompiler:= TdevCompiler.Create;

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

  // load the preferred compiler set
  if devCompilerSet.Sets.Count=0 then begin
    // init first-run
    devCompilerSet.Sets.Add(DEFCOMPILERSET);
    devCompilerSet.WriteSets;
    devCompilerSet.SaveSet(0);
  end;
  devCompilerSet.LoadSet(devCompiler.CompilerSet);
  devCompilerSet.AssignToCompiler;
  devCompilerSet.SaveSet(devCompiler.CompilerSet);
end;

procedure SaveOptions;
begin
  devData.SaveConfigData;
  devDirs.SaveSettings;
  devCompiler.SaveSettings;
  devEditor.SaveSettings;
  devCodeCompletion.SaveSettings;
  devClassBrowsing.SaveSettings;
  devCVSHandler.SaveSettings;
  devExternalPrograms.SaveSettings;
end;

procedure ResettoDefaults;
begin
  devData.SettoDefaults;
  devCompiler.SettoDefaults;
  devCompilerSet.SettoDefaults;
  devDirs.SettoDefaults;
  devEditor.SettoDefaults;
  devCodeCompletion.SettoDefaults;
  devClassBrowsing.SettoDefaults;
  devExternalPrograms.SetToDefaults;
end;

procedure FinalizeOptions;
begin
  devCompiler.SaveSettings;
  devCompiler.Free;

  if Assigned(devCompiler) then
    devCompilerSet.Free;

  devDirs.SaveSettings;
  devDirs.Free;

  devEditor.SaveSettings;
  devEditor.Free;

  devCodeCompletion.SaveSettings;
  devCodeCompletion.Free;

  devClassBrowsing.SaveSettings;
  devClassBrowsing.Free;

  devCVSHandler.SaveSettings;
  devCVSHandler.Free;

  devExternalPrograms.SaveSettings;
  devExternalPrograms.Free;
end;

procedure CheckForAltConfigFile(filename: string);
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
//C:\Documents and Settings\MANDRAVELLOS\Local Settings\Application Data\Copy of devcpp-1.ini
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
  if not assigned(fdevData) then
   begin
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
   raise Exception.Create('Dev Data already created');
  if fExternal then
   raise Exception.Create('Dev Data Externally Created');
  inherited Create(aOwner);
  IgnoreProperties.Add('Style');

  SettoDefaults;
end;

destructor TdevData.Destroy;
begin
  fdevData:= nil;
  inherited;
end;

procedure TdevData.ReadConfigData;
begin
  inherited;
  LoadWindowPlacement('Position', fWinPlace);
end;

procedure TdevData.SaveConfigData;
begin
  inherited;
  SaveWindowPlacement('Position', fWinPlace);
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
  fVersion:=''; // this is filled in MainForm.Create()
  fFirst:= TRUE;
  fLang:= DEFAULT_LANG_FILE;
  fFindCols:= '75, 75, 120, 150';
  fCompCols:= '75, 75, 120';
  fMsgTabs:= TRUE; // Top
  fMRUMax:= 10;
  fMinOnRun:= FALSE;
  fBackup:= FALSE;
  fAutoOpen:= 2;
  fShowProject:= TRUE;
  fClassView:= False;
  fProjectWidth:=161;
  fOutput:= false;
  fOutputOnNeed:= true;
  fOutputHeight:=120;
  fStatusbar:= TRUE;
  fShowBars:= FALSE;
  fShowMenu:= TRUE;
  fDefCpp:= TRUE;
  fOpenStyle:= 0;
  fdblFiles:= FALSE;
  fToolbarMain:=TRUE;
  fToolbarMainX:=11;
  fToolbarMainY:=2;
  fToolbarEdit:=TRUE;
  fToolbarEditX:=201;
  fToolbarEditY:=2;
  fToolbarCompile:=TRUE;
  fToolbarCompileX:=11;
  fToolbarCompileY:=30;
  fToolbarProject:=TRUE;
  fToolbarProjectX:=375;
  fToolbarProjectY:=2;
  fToolbarOptions:=TRUE;
  fToolbarOptionsX:=143;
  fToolbarOptionsY:=30;
  fToolbarSpecials:=TRUE;
  fToolbarSpecialsX:=202;
  fToolbarSpecialsY:=30;
  fToolbarSearch:=TRUE;
  fToolbarSearchX:=261;
  fToolbarSearchY:=2;
  fToolbarClasses:=TRUE;
  fToolbarClassesX:=11;
  fToolbarClassesY:=58;

  //read associations set by installer as defaults
  fAssociateC := getAssociation(0);
  fAssociateCpp := getAssociation(1);
  fAssociateH := getAssociation(2);
  fAssociateHpp := getAssociation(3);
  fAssociateDev := getAssociation(4);
  fAssociateRc := getAssociation(5);
  fAssociateTemplate := getAssociation(6);
  
  fShowTipsOnStart:=True;
  fLastTip:=0;
  fXPTheme := false;
  fFileDate := 0;
  fShowProgress := true;
  fAutoCloseProgress := false;
  fPrintColors := true;
  fPrintHighlight := true;
  fPrintWordWrap := false;
  fPrintLineNumbers := false;
  fPrintLineNumbersMargins := false;
  fWatchHint := true;
  fWatchError := true;
end;


{ TCompilerOpts }

procedure TdevCompiler.AddDefaultOptions;
var i : integer;
    sl : TStringList;
begin
  // WARNING: do *not* re-arrange the options. Their values are written to the ini file
  // with the same order. If you change the order here, the next time the configuration
  // is read, it will assign the values to the *wrong* options...
  // Anyway, the tree that displays the options is sorted, so no real reason to re-arrange
  // anything here ;)
  //
  // NOTE: As you see, to indicate sub-groups use the "/" char...
  for i := 0 to fOptions.Count - 1 do begin
    if Assigned(PCompilerOption(fOptions.Items[i]).optChoices) then
      PCompilerOption(fOptions.Items[i]).optChoices.Free;
    Dispose(fOptions.Items[i]);
  end;
  fOptions.Clear;

  AddOption(Lang[ID_COPT_ANSIC], False, True, True, False, 0, '-ansi', Lang[ID_COPT_GRP_C], [], nil);
  AddOption(Lang[ID_COPT_TRADITIONAL], False, True, True, False, 0, '-traditional-cpp', Lang[ID_COPT_GRP_C], [], nil);
  AddOption(Lang[ID_COPT_WARNING], False, True, True, False, 0, '-w', Lang[ID_COPT_GRP_C], [], nil);
  AddOption(Lang[ID_COPT_ACCESS], False, True, True, False, 0, '-fno-access-control', Lang[ID_COPT_GRP_CPP], [], nil);
  AddOption(Lang[ID_COPT_DOLLAR], False, True, True, False, 0, '-fdollar-in-identifiers', Lang[ID_COPT_GRP_CPP], [], nil);
  AddOption(Lang[ID_COPT_HEURISTICS], False, True, True, False, 0, '-fsave-memoized', Lang[ID_COPT_GRP_CPP], [], nil);
  AddOption(Lang[ID_COPT_EXCEPT], False, True, True, False, 0, '-fexceptions', Lang[ID_COPT_GRP_CODEGEN], [], nil);
  AddOption(Lang[ID_COPT_DBLFLOAT], False, True, True, False, 0, '-fshort-double', Lang[ID_COPT_GRP_CODEGEN], [], nil);
  AddOption(Lang[ID_COPT_MEM], False, True, True, False, 0, '-fverbose-asm', Lang[ID_COPT_GRP_CODEGEN], [], nil);
  AddOption(Lang[ID_COPT_OPTMINOR], False, True, True, False, 0, '-fexpensive-optimizations', Lang[ID_COPT_GRP_OPTIMIZE], [], nil);
  AddOption(Lang[ID_COPT_OPT1], True, True, True, False, 0, '-O1', Lang[ID_COPT_GRP_OPTIMIZE]+'/'+Lang[ID_COPT_FURTHEROPTS], [], nil);
  AddOption(Lang[ID_COPT_OPTMORE], True, True, True, False, 0, '-O2', Lang[ID_COPT_GRP_OPTIMIZE]+'/'+Lang[ID_COPT_FURTHEROPTS], [], nil);
  AddOption(Lang[ID_COPT_OPTBEST], True, True, True, False, 0, '-O3', Lang[ID_COPT_GRP_OPTIMIZE]+'/'+Lang[ID_COPT_FURTHEROPTS], [], nil);
  AddOption(Lang[ID_COPT_PROFILE], False, True, True, False, 0, '-pg', Lang[ID_COPT_PROFILING], [], nil);
  AddOption(Lang[ID_COPT_OBJC], False, False, False, True, 0, '-lobjc', Lang[ID_COPT_LINKERTAB], [], nil);
  AddOption(Lang[ID_COPT_DEBUG], False, True, True, True, 0, '-g3', Lang[ID_COPT_LINKERTAB], [], nil);
  AddOption(Lang[ID_COPT_NOLIBS], False, True, True, True, 0, '-nostdlib', Lang[ID_COPT_LINKERTAB], [], nil);
  AddOption(Lang[ID_COPT_WIN32], False, True, True, True, 0, '-mwindows', Lang[ID_COPT_LINKERTAB], [dptGUI], nil);
  AddOption(Lang[ID_COPT_ERRORLINE], False, True, True, True, 0, '-fmessage-length=0', Lang[ID_COPT_GRP_C], [], nil);
  AddOption(Lang[ID_COPT_STRIP], False, False, False, True, 0, '-s', Lang[ID_COPT_LINKERTAB], [], nil);

  // Architecture params
  sl := TStringList.Create;
  sl.Add(''); // /!\ Must contain a starting empty value in order to do not have always to pass the parameter
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
  sl.Add('K6=k6');
  sl.Add('K6-2=k6-2');
  sl.Add('K6-3=k6-3');
  sl.Add('Athlon=athlon');
  sl.Add('Athlon Tbird=athlon-tbird');
  sl.Add('Athlon 4=athlon-4');
  sl.Add('Athlon XP=athlon-xp');
  sl.Add('Athlon MP=athlon-mp');
  sl.Add('Winchip C6=winchip-c6');
  sl.Add('Winchip 2=winchip2');
  sl.Add('K8=k8');
  sl.Add('C3=c3');
  sl.Add('C3-2=c3-2');

  AddOption(Lang[ID_COPT_ARCH], False, True, True, True, 0, '-march=', Lang[ID_COPT_GRP_CODEGEN], [], sl);

  // Built-in processor functions
  sl := TStringList.Create;
  sl.Add(''); // /!\ Must contain a starting empty value in order to do not have always to pass the parameter
  sl.Add('MMX=mmx');
  sl.Add('SSE=sse');
  sl.Add('SSE 2=sse2');
  sl.Add('PNI=pni');
  sl.Add('3D Now=3dnow');

  AddOption(Lang[ID_COPT_BUILTINPROC], False, True, True, True, 0, '-m', Lang[ID_COPT_GRP_CODEGEN], [], sl);
end;

procedure TdevCompiler.AddOption(_Name: string; _IsGroup, _IsC, _IsCpp, IsLinker: boolean; _Value: integer;
  _Setting, _Section: string; ExcludeFromTypes: TProjTypeSet; Choices: TStringList);
var
  P: PCompilerOption;
begin
  P := New(PCompilerOption);
  with P^ do begin
    optName := _Name;
    optIsGroup := _IsGroup;
    optIsC := _IsC;
    optIsCpp := _IsCpp;
    optIsLinker := IsLinker;
    optValue := _Value;
    optSetting := _Setting;
    optSection := _Section;
    optExcludeFromTypes := ExcludeFromTypes;
    optChoices := Choices;
  end;
  fOptions.Add(P);
end;

procedure TdevCompiler.ChangeOptionsLang;
begin
  ClearOptions;
  AddDefaultOptions;
  LoadSettings;
end;

procedure TdevCompiler.ClearOptions;
begin
  while fOptions.Count > 0 do begin
    if Assigned(PCompilerOption(fOptions[0]).optChoices) then
      PCompilerOption(fOptions[0]).optChoices.Free;
    if Assigned(fOptions[0]) then
      Dispose(fOptions[0]);
    fOptions.Delete(0);
  end;
end;

constructor TdevCompiler.Create;
begin
  inherited;
  fOptions := TList.Create;
  SettoDefaults;
  LoadSettings;
end;

procedure TdevCompiler.DeleteOption(Index: integer);
begin
  if Assigned(PCompilerOption(fOptions[Index]).optChoices) then
    PCompilerOption(fOptions[Index]).optChoices.Free;
  if Assigned(fOptions[Index]) then
    Dispose(fOptions[Index]);
  fOptions.Delete(Index);
end;

destructor TdevCompiler.Destroy;
begin
  ClearOptions;
  fOptions.Free;
  inherited;
end;

function TdevCompiler.FindOption(Setting: string; var opt: TCompilerOption; var Index: integer): boolean;
var
  I: integer;
begin
  Result:=False;
  for I:=0 to fOptions.Count-1 do
    if Options[I].optSetting = Setting then begin
      opt:=Options[I];
      Index:=I;
      Result:=True;
      Break;
    end;
end;

function TdevCompiler.GetOptions(Index: integer): TCompilerOption;
begin
  Result := TCompilerOption(fOptions[Index]^);
end;

function TdevCompiler.GetOptionStr: string;
var
  I: integer;
begin
  Result:='';
   for I := 0 to OptionsCount - 1 do
     Result:=Result+BoolVal10[Options[I].optValue];
end;

procedure TdevCompiler.LoadSettings;
var
 s,
 key: string;
 I: integer;
 opt: TCompilerOption;
begin
  with devData do
   begin
     key:= 'Compiler';
     fUseParams:= LoadBoolSetting(key, 'UseParams');
     fIntermediate:= LoadSetting(key, 'InterDir');
     fOutputDir:= LoadSetting(key, 'OutputDir');
     fRunParams:= LoadSetting(key, 'RunParams');
     //fCompAdd:= LoadBoolSetting(key, 'CompAdd');
     //fLinkAdd:= LoadBoolSetting(key, 'LinkAdd');
     fcmdOpts:= LoadSetting(key, 'cmdline');
     flinkopts:= LoadSetting(key, 'LinkLine');

     fSaveLog:= LoadBoolSetting(key, 'Log');
     s:= LoadSetting(key, 'Delay');
     if s <> '' then fDelay:= strtoint(s);

     CompilerSet := StrToIntDef(LoadSetting(key, 'CompilerSet'), 0);

     S := LoadSetting(key, 'Options');
     for I := 0 to fOptions.Count - 1 do begin
       opt := Options[I];
       if (I < Length(S)) and (StrToIntDef(S[I+1], 0) = 1) then
         opt.optValue := 1
       else
         opt.optValue := 0;
       Options[I] := opt;
     end;

     key:= 'Makefile';
     fFastDep:= LoadboolSetting(fFastDep, key, 'FastDep');
   end;
end;

function TdevCompiler.OptionsCount: integer;
begin
  Result := fOptions.Count;
end;

procedure TdevCompiler.SaveSettings;
var
 S, key: string;
 I: integer;
begin
  with devData do
   begin
     key:= 'Compiler';
     SaveboolSetting(key, 'UseParams', fUseParams);
     SaveSetting(key, 'InterDir', fIntermediate);
     SaveSetting(key, 'OutputDir', fOutputDir);
     SaveSetting(key, 'RunParams', fRunParams);
     //SaveBoolSetting(key, 'CompAdd', fCompAdd);
     //SaveBoolSetting(key, 'LinkAdd', fLinkAdd);
     //SaveSetting(key, 'cmdline', fcmdOpts);
     //SaveSetting(key, 'LinkLine', flinkopts);

     SaveBoolSetting(key, 'Log', fSaveLog);
     SaveSetting(key, 'Delay', inttostr(fDelay));

     SaveSetting(key, GCC_PROGRAM, fgccName);
     SaveSetting(key, GPP_PROGRAM, fgppName);
     SaveSetting(key, GDB_PROGRAM, fgdbName);
     SaveSetting(key, MAKE_PROGRAM, fmakeName);
     SaveSetting(key, WINDRES_PROGRAM, fwindresName);
     SaveSetting(key, DLLWRAP_PROGRAM, fdllwrapName);
     SaveSetting(key, GPROF_PROGRAM, fgprofName);
     SaveSetting(key, 'CompilerSet', IntToStr(fCompilerSet));

     S := '';
     for I := 0 to fOptions.Count - 1 do
       with PCompilerOption(fOptions[I])^ do
         S := S + BoolVal10[optValue];
     SaveSetting(key, 'Options', S);

     key:= 'Makefile';
     SaveBoolSetting(key, 'FastDep', fFastDep);
   end;
end;

procedure TdevCompiler.SetCompilerSet(const Value: integer);
begin
 if fCompilerSet=Value then Exit;
 if not Assigned(devCompilerSet) then
   devCompilerSet:=TdevCompilerSet.Create;
 devCompilerSet.LoadSet(Value);
// Programs
  fCompilerSet:=Value;
  if devDirs.OriginalPath = '' then // first time only
    devDirs.OriginalPath := GetEnvironmentVariable('PATH');
  SetPath(devDirs.Bins);
//  devCompilerSet.LoadSet(Value);
//  fgccName := devCompilerSet.gccName;
//  fgppName := devCompilerSet.gppName;
//  fgdbName := devCompilerSet.gdbName;
//  fmakeName := devCompilerSet.makeName;
//  fwindresName := devCompilerSet.windresName;
//  fdllwrapName := devCompilerSet.dllwrapName;
//  fgprofName := devCompilerSet.gprofName;
  // TODO: basedir
end;

procedure TdevCompiler.SetOptions(Index: integer;
  const Value: TCompilerOption);
begin
  with TCompilerOption(fOptions[Index]^) do begin
    optName := Value.optName;
    optIsGroup := Value.optIsGroup;
    optIsC := Value.optIsC;
    optIsCpp := Value.optIsCpp;
    optValue := Value.optValue;
    optSetting := Value.optSetting;
    optSection := Value.optSection;
  end;
end;

procedure TdevCompiler.SetOptionStr(const Value: string);
var
  I: integer;
begin
   for I := 0 to fOptions.Count - 1 do
     if (I < Length(Value)) then begin
       PCompilerOption(fOptions[I])^.optValue := ConvertCharToValue(Value[I + 1]);
     end;
end;

function TdevCompiler.ConvertCharToValue(c : char) : integer;
begin
  if c in ['a'..'z'] then
    result := integer(c) - integer('a') + 2
  else if (StrToIntDef(c, 0) = 1) then
    result := 1
  else
    result := 0;
end;

procedure TdevCompiler.SettoDefaults;
begin
  fRunParams:= '';
  fUseParams:= FALSE;
  fModified:= TRUE;
  fSaveLog:= FALSE;
  //fCompAdd:= FALSE;
  //fLinkAdd:= FALSE;
  fcmdOpts:= '';
  flinkOpts:= '';
  fDelay:= 0;

  // makefile
  fFastDep:= TRUE;

  // Programs
  fgccName := GCC_PROGRAM;
  fgppName := GPP_PROGRAM;
  fgdbName := GDB_PROGRAM;
  fmakeName := MAKE_PROGRAM;
  fwindresName := WINDRES_PROGRAM;
  fdllwrapName := DLLWRAP_PROGRAM;
  fgprofName := GPROF_PROGRAM;
  fCompilerSet:=0;

  AddDefaultOptions;
end;


{ TDevDirs }

constructor TdevDirs.Create;
begin
 inherited Create;
 Name:= OPT_DIRS;
 SettoDefaults;
 LoadSettings;
end;

procedure TdevDirs.SettoDefaults;
var
  tempstr: String;
begin
  fDefault:= IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
  fBinDir:= ValidatePaths(fDefault + BIN_DIR, tempstr);
  fCDir:= ValidatePaths(fDefault + C_INCLUDE_DIR, tempstr);
  fCppDir:= ValidatePaths(fDefault
    + StringReplace(CPP_INCLUDE_DIR, ';', ';' + fDefault, [rfReplaceAll]), tempstr);
  fLibDir:= ValidatePaths(fDefault + LIB_DIR, tempstr);

  fExec:= ExtractFilePath(Application.ExeName);
  fConfig:= fExec;
  fHelp:= fExec + HELP_DIR;
  fIcons:= fExec + ICON_DIR;
  fLang:= fExec + LANGUAGE_DIR;
  fTemp:= fExec + TEMPLATE_DIR;
  fThemes:= fExec + THEME_DIR;
end;

procedure TdevDirs.LoadSettings;
begin
  devData.LoadObject(Self);
  fExec:= ExtractFilePath(Application.ExeName);
  if fHelp = '' then fHelp:= fExec +HELP_DIR;
  if fIcons = '' then fIcons:= fExec +ICON_DIR;
  if fLang = '' then fLang:= fExec +LANGUAGE_DIR;
  if fTemp = '' then fTemp:= fExec +TEMPLATE_DIR;
  if fThemes = '' then  fThemes:= fExec +THEME_DIR;
  FixPaths;
end;

procedure TdevDirs.SaveSettings;
begin
  fHelp:= ExtractRelativePath(fExec, fHelp);
  fIcons:= ExtractRelativePath(fExec, fIcons);
  fLang:= ExtractRelativePath(fExec, fLang);
  fTemp:= ExtractRelativePath(fExec, fTemp);
  fMingw:= ExtractRelativePath(fExec, fMingw);
  fThemes:= ExtractRelativePath(fExec, fThemes);
  devData.SaveObject(Self);
  FixPaths;
end;

procedure TdevDirs.FixPaths;
begin
  // if we are called by double-clicking a .dev file in explorer,
  // we must perform the next checks or else the dirs are
  // really screwed up...
  // Basically it checks if it is a relative path (as it should be).
  // If so, it prepends the base Dev-C++ directory...
  if ExtractFileDrive(fHelp)='' then
    fHelp:=fExec+fHelp;
  if ExtractFileDrive(fIcons)='' then
    fIcons:=fExec+fIcons;
  if ExtractFileDrive(fLang)='' then
    fLang:=fExec+fLang;
  if ExtractFileDrive(fTemp)='' then
    fTemp:=fExec+fTemp;
  if ExtractFileDrive(fThemes)='' then
    fThemes:=fExec+fThemes;
  if ExtractFileDrive(fMingw)='' then
    fMingw:=fExec+fMingw;
end;

{ TDevEditor }

constructor TdevEditor.Create;
begin
  inherited;
  Name:= OPT_EDITOR;

  fFont:= TFont.Create;
  fGutterfont:= TFont.Create;
  fSyntax:= TStringList.Create;
  TStringList(fSynTax).Duplicates:=  dupIgnore;
  SettoDefaults;
  LoadSettings;
end;

destructor TdevEditor.Destroy;
begin
  fFont.Free;
  fGutterfont.Free;
  fSynTax.Free;
  inherited;
end;

procedure TdevEditor.LoadSettings;
begin
  devData.LoadObject(Self);
end;

procedure TdevEditor.SaveSettings;
begin
  devData.SaveObject(Self);
end;

procedure TdevEditor.SettoDefaults;
begin
  fFont.name:= 'Courier New';
  fFont.Size:= 10;
  fTabSize:= 4;
  fShowGutter:= TRUE;
  fCustomGutter:= TRUE;
  fGutterSize:= 32;
  fGutterFont.Name:= 'Terminal';
  fGutterFont.Size:= 9;
  fGutterAuto:= FALSE;

  fInsertCaret:= 0;
  fOverwriteCaret:= 0;

  fMarginVis:= TRUE;
  fMarginSize:= 80;
  fMarginColor:= cl3DLight;

  fGroupUndo:= TRUE;

  fLineNumbers:= FALSE;
  fLeadZero:= FALSE;
  fFirstisZero:= FALSE;
  fEHomeKey:= FALSE;

  fShowScrollHint:= TRUE;
  fShowScrollbars:= TRUE;
  fHalfPage:= FALSE;

  fPastEOF:= FALSE;
  fPastEOL:= FALSE;
  fTrailBlanks:= FALSE;
  fdblLine:= FALSE;
  fFindText:= TRUE;

  fAutoCloseBrace:= FALSE;  // not working well/turned off

  fInsertMode:= TRUE;
  fAutoIndent:= TRUE;
  fSmartTabs:= TRUE;
  fSmartUnindent:= TRUE;
  fTabtoSpaces:= TRUE;

  fUseSyn:= TRUE;
  //last ; is for files with no extension
  //which should be treated as cpp header files
  fSynExt:= 'c;cpp;h;hpp;cc;cxx;cp;hp;rh;';

  fParserHints:= TRUE;
  fMatch := false;

  fHighCurrLine := True;
  fHighColor := $FFFFCC; //Light Turquoise

  fAppendNewline := True;
end;

procedure TdevEditor.AssignEditor(Editor: TSynEdit);
var
 pt: TPoint;
 x: integer;
begin
  if (not assigned(Editor)) or (not (Editor is TCustomSynEdit)) then exit;
  with Editor do
   begin
     BeginUpdate;
     try
      TabWidth:= fTabSize;

      Font.Assign(fFont);
      with Gutter do
       begin
         UseFontStyle:= fCustomGutter;
         Font.Assign(fGutterFont);
         Width:= fGutterSize;
         Visible:= fShowGutter;
         AutoSize:= fGutterAuto;
         ShowLineNumbers:= fLineNumbers;
         LeadingZeros:= fLeadZero;
         ZeroStart:= fFirstisZero;
         x:= fSyntax.IndexofName(cGut);
         if x <> -1 then
          begin
            StrtoPoint(pt, fSyntax.Values[cGut]);
            Color:= pt.x;
            Font.Color:= pt.y;
          end;
       end;

      if fMarginVis then
       RightEdge:= fMarginSize
      else
       RightEdge:= 0;

      RightEdgeColor:= fMarginColor;

      InsertCaret:= TSynEditCaretType(fInsertCaret);
      OverwriteCaret:= TSynEditCaretType(fOverwriteCaret);

      ScrollHintFormat:= shfTopToBottom;

      if HighCurrLine then
        ActiveLineColor := HighColor
      else
        ActiveLineColor := clNone;

      Options := [
        eoAltSetsColumnMode, eoDisableScrollArrows,
        eoDragDropEditing, eoDropFiles, eoKeepCaretX,
        //eoAutoSizeMaxLeftChar was replaced by eoAutoSizeMaxScrollWidth
        eoRightMouseMovesCursor, eoScrollByOneLess, eoAutoSizeMaxScrollWidth
        {eoNoCaret, eoNoSelection, eoScrollHintFollows, }
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
        Options := Options + [eoShowScrollHint];
      if fSmartTabs then
        Options := Options + [eoSmartTabs];
      if fSmartTabs then
        Options := Options + [eoSmartTabDelete];
      if fTabtoSpaces then
        Options := Options + [eoTabsToSpaces];
      if fTrailBlanks then
        Options := Options + [eoTrimTrailingSpaces];
      if fSpecialChar then
        Options := Options + [eoShowSpecialChars];

     finally
      EndUpdate;
     end;
   end;
end;


{ TdevCodeCompletion }

constructor TdevCodeCompletion.Create;
begin
 inherited Create;
 Name:= 'CodeCompletion';
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
  devData.LoadObject(Self);
end;

procedure TdevCodeCompletion.SaveSettings;
begin
  devData.SaveObject(Self);
end;

procedure TdevCodeCompletion.SetDelay(Value: integer);
begin
  if Value=0 then
    fDelay:=1 // minimum 1 ms
  else
    fDelay:=Value;
end;

procedure TdevCodeCompletion.SettoDefaults;
begin
  fWidth:=320;
  fHeight:=240;
  fDelay:=500;
  fBackColor:=clWindow;
  fEnabled:=True;
  fUseCacheFiles:=False;
end;

{ TdevClassBrowsing }

constructor TdevClassBrowsing.Create;
begin
 inherited Create;
 Name:= 'ClassBrowsing';
 SettoDefaults;
 LoadSettings;
end;

procedure TdevClassBrowsing.LoadSettings;
begin
  devData.LoadObject(Self);
end;

procedure TdevClassBrowsing.SaveSettings;
begin
  devData.SaveObject(Self);
end;

procedure TdevClassBrowsing.SettoDefaults;
begin
  fCBViewStyle:=0;
  fEnabled:=True;
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
 Name:= 'CVSHandler';
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
  devData.LoadObject(Self);
end;

procedure TdevCVSHandler.SaveSettings;
begin
  devData.SaveObject(Self);
end;

procedure TdevCVSHandler.SettoDefaults;
begin
   fExecutable:='cvs.exe';
   fCompression:=4;
   fUseSSH:=True;
end;

{ TdevCompilerSet }

procedure TdevCompilerSet.AssignToCompiler;
begin
  devCompiler.gccName:=devCompilerSet.gccName;
  devCompiler.gppName:=devCompilerSet.gppName;
  devCompiler.gdbName:=devCompilerSet.gdbName;
  devCompiler.makeName:=devCompilerSet.makeName;
  devCompiler.windresName:=devCompilerSet.windresName;
  devCompiler.dllwrapName:=devCompilerSet.dllwrapName;
  devCompiler.gprofName:=devCompilerSet.gprofName;
  //RNC
  devCompiler.fcmdOpts:=devCompilerSet.fCmdOptions;
  devCompiler.flinkopts:=devCompilerSet.fLinkOptions;

  // we have to set the devDirs too
  devDirs.Bins:=devCompilerSet.BinDir;
  devDirs.C:=devCompilerSet.CDir;
  devDirs.Cpp:=devCompilerSet.CppDir;
  devDirs.Lib:=devCompilerSet.LibDir;


  devCompiler.OptionStr:=fOptions;
end;

constructor TdevCompilerSet.Create;
begin
  inherited;
  fSets:=TStringList.Create;
  UpdateSets;
  SettoDefaults;
end;

destructor TdevCompilerSet.Destroy;
begin
  fSets.Free;
  inherited;
end;

procedure TdevCompilerSet.LoadSet(Index: integer);
begin
  LoadSetProgs(Index);
  LoadSetDirs(Index);
end;

procedure TdevCompilerSet.LoadSetDirs(Index: integer);
var
  key: string;
  goodBinDir, goodCDir, goodCppDir, goodLibDir: String;
  msg: String;
  tempStr: String;
  maindir: String;
  makeSig, mingwmakeSig: String;

{
  function isPathInList(pathList, path: String): Boolean;
  var
    i: Integer;
    strs: TStrings;
  begin
    Result := False;
    strs := TStringList.Create;
    strs.Delimiter := ';';
    strs.DelimitedText := pathList;
    for i := 0 to strs.Count -1 do
      if strs[i] = path then
      begin
        Result := True;
        break;
      end;
    strs.Free;
  end;
}

begin
  if Index<0 then Exit;
  with devData do
   begin
     key:= OPT_COMPILERSETS+'_'+IntToStr(Index);
     // dirs
     fBinDir := LoadSetting(key, 'Bins');
     if fBinDir='' then fBinDir:=devDirs.Bins;
     fCDir := LoadSetting(key, 'C');
     if fCDir='' then fCDir:=devDirs.C;
     fCppDir := LoadSetting(key, 'Cpp');
     if fCppDir='' then fCppDir:=devDirs.Cpp;
     fLibDir := LoadSetting(key, 'Lib');
     if fLibDir='' then fLibDir:=devDirs.Lib;

     //check for valid paths
     msg := '';
     goodBinDir := ValidatePaths(fBinDir, tempStr);
     if tempStr <> '' then
     begin
       msg := msg + 'Following Bin directories don''t exist:' + #13#10;
       msg := msg + StringReplace(tempStr, ';', #13#10, [rfReplaceAll]);
       msg := msg + #13#10 + #13#10;
     end;
     goodCDir := ValidatePaths(fCDir, tempStr);
     if tempStr <> '' then
     begin
       msg := msg + 'Following C Include directories don''t exist:' + #13#10;
       msg := msg + StringReplace(tempStr, ';', #13#10, [rfReplaceAll]);
       msg := msg + #13#10 + #13#10;
     end;
     goodCppDir := ValidatePaths(fCppDir, tempStr);
     if tempStr <> '' then
     begin
       msg := msg + 'Following C++ Include directories don''t exist:' + #13#10;
       msg := msg + StringReplace(tempStr, ';', #13#10, [rfReplaceAll]);
       msg := msg + #13#10 + #13#10;
     end;
     goodLibDir := ValidatePaths(fLibDir, tempStr);
     if tempStr <> '' then
     begin
       msg := msg + 'Following Libs directories don''t exist:' + #13#10;
       msg := msg + StringReplace(tempStr, ';', #13#10, [rfReplaceAll]);
       msg := msg + #13#10 + #13#10;
     end;
     if msg <> '' then
     begin
       msg := msg + 'Would you like Dev-C++ to remove them for you '
         + 'and add the default paths to the remaining existing paths?' + #13#10
         + 'Leaving those directories will lead to problems during compilation '
         + 'of any projects created with Dev-C++' + #13#10
         + #13#10
         + 'Unless you know exactly what you''re doing, it is recommended '
         + 'that you click Yes';

       if MessageDlg(msg, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
       begin
         fBinDir := goodBinDir;
         fCDir := goodCDir;
         fCppDir := goodCppDir;
         fLibDir := goodLibDir;

         //additionally insert default paths:
         maindir := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
         tempStr := '';
         fBinDir := maindir + BIN_DIR + ';' + fBinDir;
         fBinDir := ValidatePaths(fBinDir, tempStr);
         devDirs.Bins := fBinDir;
         fCDir := C_INCLUDE_DIR + ';' + fCDir;
         fCDir := ValidatePaths(fCDir, tempStr);
         devDirs.C := fCDir;
         fCppDir := CPP_INCLUDE_DIR + ';' + fCppDir;
         fCppDir := ValidatePaths(fCppDir, tempStr);
         devDirs.Cpp := fCppDir;
         fLibDir := LIB_DIR + ';' + fLibDir;
         fLibDir := ValidatePaths(fLibDir, tempStr);
         devDirs.Lib := fLibDir;
       end;
     end;
   end;

  //check if make is in path + bins directory
  if devDirs.OriginalPath = '' then // first time only
    devDirs.OriginalPath := GetEnvironmentVariable('PATH');
  SetPath(devDirs.Bins);
  makeSig := RunAndGetOutput(devCompilerSet.makeName + ' --v',
    ExtractFileDir(Application.ExeName), nil, nil, nil);

  if Pos('GNU Make', makeSig) = 0 then
  begin
    //check for mingw32-make first before adding the path to bin dir
    //process it later
    mingwmakeSig := RunAndGetOutput('mingw32-make --v',
      ExtractFileDir(Application.ExeName), nil, nil, nil);

    //check if make is found if Dev-C++'s bin directory is added to path
    SetPath(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))
     + BIN_DIR);
    makeSig := RunAndGetOutput(devCompilerSet.makeName + ' --v',
      ExtractFileDir(Application.ExeName), nil, nil, nil);
    if Pos('GNU Make', makeSig) > 0 then
    begin
      msg := 'Dev-C++ was unable to find GNU Make with current settings '
        + 'however there''s GNU Make in Dev-C++''s bin directory. '
        + 'Would you like to add this path to current Bin path?'
        + #13#10#13#10
        + 'Unless you know exactly what you''re doing, it is recommended '
        + 'that you click Yes';
      if MessageDlg(msg, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
       fBinDir := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))
         + BIN_DIR + ';' + fBinDir;
    end

    //check if "mingw32-make" is available and is GNU Make
    else if Pos('GNU Make', mingwmakeSig) > 0 then
    begin
      msg := 'Dev-C++ was unable to find GNU Make with current settings '
        + 'however there''s mingw32-make that seems to be GNU Make. '
        + 'Would you like Dev-C++ to adjust the settings for you to '
        + 'use mingw32-make?'
        + #13#10#13#10
        + 'Unless you know exactly what you''re doing, it is recommended '
        + 'that you click Yes';
      if MessageDlg(msg, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      begin
        devCompilerSet.makeName := 'mingw32-make';
        devCompiler.makeName := 'mingw32-make';
      end;
    end

    //check if "mingw32-make" is available in bin directory
    else
    begin
      mingwmakeSig := RunAndGetOutput('mingw32-make --v',
        ExtractFileDir(Application.ExeName), nil, nil, nil);
      if Pos('GNU Make', mingwmakeSig) > 0 then
      begin
        msg := 'Dev-C++ was unable to find GNU Make in PATH '
          + 'or in Dev-C++''s bin directory'
          + 'however there''s mingw32-make that seems to be GNU Make in bin directory. '
          + 'Would you like Dev-C++ to adjust the settings for you to '
          + 'use mingw32-make and adjust current Bin path?'
          + #13#10#13#10
          + 'Unless you know exactly what you''re doing, it is recommended '
          + 'that you click Yes';
        if MessageDlg(msg, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        begin
          fBinDir := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))
            + BIN_DIR + ';' + fBinDir;
          devCompilerSet.makeName := 'mingw32-make';
          devCompiler.makeName := 'mingw32-make';
        end;
      end;
    end;

    //if nothing's found just display the warning message
    if (Pos('GNU Make', mingwmakeSig) = 0)
      and (Pos('GNU Make', makeSig) = 0) then
    begin
      msg := 'There doesn''t seem to be GNU Make file in PATH '
        + 'or in Dev-C++''s Bin path. Please make sure that you have '
        + 'GNU Make and adjust Bin setting or system PATH environment '
        + 'variable and that make setting in Compiler Option contains '
        + 'correct filename, otherwise you will not be able to compile '
        + 'anything.' ;
      MessageDlg(msg, mtConfirmation, [mbOK], 0);
    end;

  end;
end;

procedure TdevCompilerSet.LoadSetProgs(Index: integer);
var
 key: string;
begin
  if Index<0 then Exit;
  with devData do
   begin
     key:= OPT_COMPILERSETS+'_'+IntToStr(Index);
     // Programs
     fgccName := LoadSetting(key, GCC_PROGRAM);
     if fgccName='' then fgccName:=GCC_PROGRAM;
     fgppName := LoadSetting(key, GPP_PROGRAM);
     if fgppName='' then fgppName:=GPP_PROGRAM;
     fgdbName := LoadSetting(key, GDB_PROGRAM);
     if fgdbName='' then fgdbName:=GDB_PROGRAM;
     fmakeName := LoadSetting(key, MAKE_PROGRAM);
     if fmakeName='' then fmakeName:=MAKE_PROGRAM;
     fwindresName := LoadSetting(key, WINDRES_PROGRAM);
     if fwindresName='' then fwindresName:=WINDRES_PROGRAM;
     fdllwrapName := LoadSetting(key, DLLWRAP_PROGRAM);
     if fdllwrapName='' then fdllwrapName:=DLLWRAP_PROGRAM;
     fgprofName := LoadSetting(key, GPROF_PROGRAM);
     if fgprofName='' then fgprofName:=GPROF_PROGRAM;
     fOptions := LoadSetting(key, 'Options');

     //RNC
    fCmdOptions:= LoadSetting(key, 'cmdline');
    fLinkOptions:=LoadSetting(key, 'LinkLine');
    fCompAdd:= LoadBoolSetting(key, 'CompAdd');
    fLinkAdd:= LoadBoolSetting(key, 'LinkAdd');

   end;
end;

procedure TdevCompilerSet.LoadSettings;
begin
  LoadSet(0);
end;

procedure TdevCompilerSet.SaveSet(Index: integer);
begin
  SaveSetProgs(Index);
  SaveSetDirs(Index);
end;

procedure TdevCompilerSet.SaveSetDirs(Index: integer);
var
 key: string;
begin
  if Index<0 then Exit;
  with devData do
   begin
     key:= OPT_COMPILERSETS+'_'+IntToStr(Index);
     // dirs
     SaveSetting(key, 'Bins', fBinDir);
     SaveSetting(key, 'C', fCDir);
     SaveSetting(key, 'Cpp', fCppDir);
     SaveSetting(key, 'Lib', fLibDir);
   end;
end;

procedure TdevCompilerSet.SaveSetProgs(Index: integer);
var
 key: string;
begin
  if Index<0 then Exit;
  with devData do
   begin
     key:= OPT_COMPILERSETS+'_'+IntToStr(Index);
     // Programs
     SaveSetting(key, GCC_PROGRAM, fgccName);
     SaveSetting(key, GPP_PROGRAM, fgppName);
     SaveSetting(key, GDB_PROGRAM, fgdbName);
     SaveSetting(key, MAKE_PROGRAM, fmakeName);
     SaveSetting(key, WINDRES_PROGRAM, fwindresName);
     SaveSetting(key, DLLWRAP_PROGRAM, fdllwrapName);
     SaveSetting(key, GPROF_PROGRAM, fgprofName);
     SaveSetting(key, 'Options', fOptions);
     SaveSetting(key, 'cmdline', fCmdOptions);
     SaveSetting(key, 'LinkLine', fLinkOptions);
     SaveBoolSetting(key, 'CompAdd', fCompAdd);
     SaveBoolSetting(key, 'LinkAdd', fLinkAdd);
   end;
end;

procedure TdevCompilerSet.SaveSettings;
begin
  WriteSets;
end;

function TdevCompilerSet.SetName(Index: integer): string;
begin
  if (Index>=0) and (Index<devCompilerSet.Sets.Count) then
    Result:=devCompilerSet.Sets[Index]
  else
    Result:=DEFCOMPILERSET;
end;

procedure TdevCompilerSet.SettoDefaults;
begin
  // Programs
  fgccName := GCC_PROGRAM;
  fgppName := GPP_PROGRAM;
  fgdbName := GDB_PROGRAM;
  fmakeName := MAKE_PROGRAM;
  fwindresName := WINDRES_PROGRAM;
  fdllwrapName := DLLWRAP_PROGRAM;
  fgprofName := GPROF_PROGRAM;
  fCompAdd:= FALSE;
  fLinkAdd:= FALSE;
  fCmdOptions:='';
  fLinkOptions:='';


  // dirs
  fBinDir := devDirs.Bins;
  fCDir := devDirs.C;
  fCppDir := devDirs.Cpp;
  fLibDir := devDirs.Lib;

  fOptions:='';
end;

procedure TdevCompilerSet.UpdateSets;
var
  Ini: TIniFile;
  sl: TStringList;
  I: integer;
begin
  fSets.Clear;
  Ini:=TIniFile.Create(devData.INIFile);
  sl:=TStringList.Create;
  try
    Ini.ReadSectionValues(OPT_COMPILERSETS, sl);
    for I:=0 to sl.Count-1 do
      fSets.Add(sl.Values[sl.Names[I]]);
  finally
    sl.Free;
    Ini.Free;
  end;
end;

procedure TdevCompilerSet.WriteSets;
var
  Ini: TIniFile;
  I: integer;
begin
  Ini:=TIniFile.Create(devData.INIFile);
  try
    Ini.EraseSection(OPT_COMPILERSETS);
    for I:=0 to fSets.Count-1 do
      Ini.WriteString(OPT_COMPILERSETS, IntToStr(I), fSets[I]);
  finally
    Ini.Free;
  end;
end;

{ TdevExternalPrograms }

function TdevExternalPrograms.AddProgram(ext, prog: string): integer;
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

function TdevExternalPrograms.AssignedProgram(ext: string): integer;
var
  I: integer;
begin
  Result:=-1;
  for I:=0 to fPrograms.Count-1 do
    if UpperCase(fPrograms.Names[I])=UpperCase(ext) then begin
      Result:=I;
      Break;
    end;
end;

constructor TdevExternalPrograms.Create;
begin
 inherited Create;
 Name:= 'ExternalPrograms';
 fPrograms:=TStringList.Create;
 SettoDefaults;
 LoadSettings;
end;

destructor TdevExternalPrograms.Destroy;
begin
  fPrograms.Free;
end;

function TdevExternalPrograms.GetProgramName(Index: integer): string;
begin
  Result:=fPrograms.Values[fPrograms.Names[Index]];
end;

procedure TdevExternalPrograms.LoadSettings;
begin
  devData.LoadObject(Self);
end;

procedure TdevExternalPrograms.SaveSettings;
begin
  devData.SaveObject(Self);
end;

procedure TdevExternalPrograms.SetToDefaults;
begin
  inherited;

end;

end.
