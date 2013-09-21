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

unit version;

interface

//path delimiter
const
{$IFDEF WIN32}
	pd                   = '\';
{$ENDIF}
{$IFDEF LINUX}
	pd                   = '/';
{$ENDIF}

	// misc strings
	DEVCPP               = 'Dev-C++';
	DEVCPP_VERSION       = '5.2.0.3';
	DEVCPP_BUILDTIME     = 'June 10 2012 - 13:28';
	DEFAULT_LANG_FILE    = 'English.lng';
	HTTP                 = 'http://';
	DEV_INTERNAL_OPEN    = '$__DEV_INTERNAL_OPEN';
	DEV_SEARCHLOOP       = '$__DEV_SEARCH_LOOP';
	DEV_COMPLETION_CACHE = 'cache.ccc';
	DEV_DEFAULTCODE_FILE = 'defaultcode.cfg';
	DEV_SHORTCUTS_FILE   = 'devShortcuts.cfg';
	DEV_CLASSFOLDERS_FILE= 'classfolders.dcf';
	DEV_WEBMIRRORS_FILE  = 'mirrors.cfg';
	DEV_MAKE_FILE        = 'Makefile.win';
	DEV_TOOLS_FILE       = 'tools.ini';
	DEV_HELP_INI         = 'devhelp.ini';
	DEV_CODEINS_FILE     = 'devcpp.ci';
	DEV_MAINHELP_FILE    = 'devcpp.htm';
	DEV_GNOME_THEME      = 'Gnome';
	DEV_NEWLOOK_THEME    = 'New Look';
	DEV_BLUE_THEME       = 'Blue';
	DEV_INTERNAL_THEME   = 'New Look';

	// default directories
	BIN_DIR32            = '%path%' + pd + 'MinGW32' + pd + 'bin';
	LIB_DIR32            = '%path%' + pd + 'MinGW32' + pd + 'lib';
	C_INCLUDE_DIR32      = '%path%' + pd + 'MinGW32' + pd + 'include';
	CPP_INCLUDE_DIR32    = '%path%' + pd + 'MinGW32' + pd + 'include';

	BIN_DIR64            = '%path%' + pd + 'MinGW64' + pd + 'bin';
	LIB_DIR64            = '%path%' + pd + 'MinGW64' + pd + 'x86_64-w64-mingw32' + pd + 'lib';
	C_INCLUDE_DIR64      = '%path%' + pd + 'MinGW64' + pd + 'x86_64-w64-mingw32' + pd + 'include';
	CPP_INCLUDE_DIR64    = '%path%' + pd + 'MinGW64' + pd + 'x86_64-w64-mingw32' + pd + 'include';

	LIB_DIR64ALT         = '%path%' + pd + 'MinGW64' + pd + 'x86_64-w64-mingw32' + pd + 'lib32';

	LANGUAGE_DIR         = 'Lang' + pd;
	ICON_DIR             = 'Icons' + pd;
	HELP_DIR             = 'Help' + pd;
	TEMPLATE_DIR         = 'Templates' + pd;
	THEME_DIR            = 'Themes' + pd;
	PACKAGES_DIR         = 'Packages' + pd;

	// file fxtensions
	LIB_EXT              = '.a';
	OBJ_EXT              = '.o';
	DLL_EXT              = '.dll';
	EXE_EXT              = '.exe';
	DEV_EXT              = '.dev';
	HTML_EXT             = '.html';
	RTF_EXT              = '.rtf';
	TEX_EXT              = '.tex';
	INI_EXT              = '.ini';
	TEMPLATE_EXT         = '.template';
	SYNTAX_EXT           = '.syntax';
	C_EXT                = '.c';
	CPP_EXT              = '.cpp';
	CC_EXT               = '.cc';
	CXX_EXT              = '.cxx';
	CP2_EXT              = '.c++';
	CP_EXT               = '.cp';
	H_EXT                = '.h';
	HPP_EXT              = '.hpp';
	RC_EXT               = '.rc';
	RES_EXT              = '.res';
	RH_EXT               = '.rh';

	// programs
	MAKE_PROGRAM         = 'mingw32-make.exe';
	GCC_PROGRAM          = 'gcc.exe';
	GPP_PROGRAM          = 'g++.exe';
	GDB_PROGRAM          = 'gdb.exe';
	WINDRES_PROGRAM      = 'windres.exe';
	DLLWRAP_PROGRAM      = 'dllwrap.exe';
	GPROF_PROGRAM        = 'gprof.exe';
	PACKMAN_PROGRAM      = 'packman.exe';

	// option sections
	OPT_DIRS             = 'Directories';
	OPT_EDITOR           = 'Editor';
	OPT_HISTORY          = 'History';
	OPT_OPTIONS          = 'Options';
	OPT_POS              = 'Positions';
	OPT_START            = 'Startup';
	OPT_COMPILERSETS     = 'CompilerSets';
	WEBUPDATE_SECTION    = 'WEBUPDATE';

	DEFCOMPILERSET32     = 'MinGW GCC 4.6.2 32-bit';
	DEFCOMPILERSET64     = 'TDM-GCC 4.6.1 64-bit';
	DEFCOMPILERSET64ALT  = 'TDM-GCC 4.6.1 32-bit';

	// Filters
	FLT_BASE             = 'All known Files||';
	FLT_ALLFILES         = 'All files (*.*)|*.*|';
	FLT_PROJECTS         = 'Dev-C++ project (*.dev)|*.dev';
	FLT_HEADS            = 'Header files (*.h;*.hpp;*.rh;*.hh)|*.h;*.hpp;*.rh;*.hh';
	FLT_CS               = 'C source files (*.c)|*.c';
	FLT_CPPS             = 'C++ source files (*.cpp;*.cc;*.cxx;*.c++;*.cp)|*.cpp;*.cc;*.cxx;*.c++;*.cp';
	FLT_RES              = 'Resource scripts (*.rc)|*.rc';
	FLT_HELPS            = 'Help files (*.hlp;*.chm;*.col)|*.hlp;*.chm;*.col|HTML files (*.htm;*.html)|*.htm;*.html|Text files (*.doc;*.rtf;*.txt)|*.doc;*.rtf;*.txt|All files (*.*)|*.*';
	FLT_MSVCPROJECTS     = 'MS Visual C++ projects (*.dsp)|*.dsp';
	FLT_CBPROJECTS       = 'Code::Blocks projects (*.cbp)|*.cbp';

	cBP                  = 'Breakpoints';
	cErr                 = 'Error line';
	cABP                 = 'Active breakpoints';
	cGut                 = 'Gutter';
	cSel                 = 'Selected text';
	cFld                 = 'Folding lines';

	PATH_LEN             = 512;
	CONFIG_PARAM         = '-c'; 

	// GDB commands and Displays
	GDB_FILE             = 'file';
	GDB_EXECFILE         = 'exec-file';
	GDB_RUN              = 'run';
	GDB_BREAK            = 'break';
	GDB_CONTINUE         = 'continue';
	GDB_NEXT             = 'next';
	GDB_STEP             = 'step';
	GDB_DISPLAY          = 'display';
	GDB_UNDISPLAY        = 'undisplay';
	GDB_PRINT            = 'print';
	GDB_SETVAR           = 'set var';
	GDB_DELETE           = 'delete';
	GDB_PROMPT           = '(gdb) ';
	GDB_BACKTRACE        = 'bt';
	GDB_KILL             = 'kill';
	GDB_DISASSEMBLE      = 'disassemble';
	GDB_SETFLAVOR        = 'set disassembly-flavor';
	GDB_INTEL            = 'intel';
	GDB_ATT              = 'att';
	GDB_REG              = 'displ/x';
	GDB_EAX              = '$eax';
	GDB_EBX              = '$ebx';
	GDB_ECX              = '$ecx';
	GDB_EDX              = '$edx';
	GDB_ESI              = '$esi';
	GDB_EDI              = '$edi';
	GDB_EBP              = '$ebp';
	GDB_ESP              = '$esp';
	GDB_EIP              = '$eip';
	GDB_CS               = '$cs';
	GDB_DS               = '$ds';
	GDB_SS               = '$ss';
	GDB_ES               = '$es';
	GDB_FS               = '$fs';
	GDB_GS               = '$gs';
	GDB_SETARGS          = 'set args';
	GDB_ATTACH           = 'attach';
	GDB_SET              = 'set'; 

	T_PROMPT             = 'prompt';
	T_BREAKPOINT         = 'breakpoint';
	T_SOURCE             = 'source';
	T_DISPLAY_EXPRESSION = 'display-expression';
	T_DISPLAY_BEGIN      = 'display-begin';
	T_DISPLAY_VALUE      = 'display-value';
	T_DISPLAY_END        = 'display-end';
	T_FIELD_NAME         = 'field-begin';
	T_FIELD_VALUE        = 'field-value';
	T_FIELD_END          = 'field-end';
	T_GDB_ERROR          = 'error';
	T_GDB_ERRORBEGIN     = 'error-begin';
	T_FRAME_FUNCNAME     = 'frame-function-name';
	T_FRAME_ARGS         = 'frame-args';
	T_FRAME_ARG_BEGIN    = 'arg-begin';
	T_FRAME_ARG_NAME_END = 'arg-name-end';
	T_FRAME_ARG_VALUE    = 'arg-value';
	T_FRAME_ARG_END      = 'arg-end';
	T_FRAME_SOURCEFILE   = 'frame-source-file';
	T_FRAME_SOURCELINE   = 'frame-source-line';
	T_DUMP               = 'Dump';
	T_OF                 = 'of';
	T_POST_PROMPT        = 'post-prompt';
	T_ASM                = 'assembler';
	T_SIGNAL_STRING      = 'signal-AnsiString';
	T_SEGFAULT           = 'Segmentation';
	T_ARRAYSECTION_BEGIN = 'array-section-begin';
	T_ARRAYSECTION_END   = 'array-section-end';

	// GPROF commands and displays
	GPROF_CHECKFILE      = 'gmon.out';
	GPROF_CMD_GENFLAT    = '-p';
	GPROF_CMD_GENMAP     = '-q';

implementation

end.
