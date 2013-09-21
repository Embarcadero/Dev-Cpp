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
 Changes: 12.2.1 - 12.5.1
 Programmer: Mike Berg
 Description:  Changed ids to cover all know strings from interface as of
               cvs - 11.20.1. (including my changes :)
               Rewrote to introduce numbering system.
               Menu items:                    100
               non-visual:                    300
               Dialogs:                       400
               Global messages/prompts:       1000
               error messages                 1500
               misc strings                   2000
*)

(*
 notes:
  comp  = 0;
  res  = 0;
  sections are marked with { -- --} comments
  "replaces" comment show ids that are
    replaced by the ONE below it

  comments at the EOL containing format
  specifiers are what the programs expect
  in the string.
  e.g. // %s          -- program inserts a string
       // %s %s       -- program inserts 2 strings
       // %s %d       -- program inserts a string then a integer
  see delphi help file (format string) for more info
  on various types.
*)

const
  { -- main form items -- }
  // menu captions
  ID_MNU_FILE                       = 1;
  ID_MNU_EDIT                       = 2;
  ID_MNU_SEARCH                     = 3;
  ID_MNU_VIEW                       = 4;
  ID_MNU_PROJECT                    = 5;
  ID_MNU_EXECUTE                    = 6;
  ID_MNU_TOOLS                      = 7;
  ID_MNU_WINDOW                     = 8;
  ID_MNU_HELP                       = 9;
  ID_MNU_DEBUG                      = 10;
  //free to 119 for top level menus

  //file menu
  ID_SUB_NEW                        = 1001;
  ID_ITEM_NEWPROJECT                = 1002;
  ID_ITEM_NEWSOURCE                 = 1003;
  ID_ITEM_NEWRESOURCE               = 1004;
  ID_ITEM_NEWTEMPLATE               = 1005;
  ID_ITEM_NEWCLASS                  = 1006;
  ID_HINT_NEW                       = 1007;
  //free to 129 for sub new items

  ID_ITEM_OPEN                      = 2001;
  ID_SUB_REOPEN                     = 2002;
  ID_ITEM_CLEARHISTORY              = 2003;
  ID_ITEM_SAVEFILE                  = 2004;
  ID_ITEM_SAVEAS                    = 2005;
  ID_ITEM_SAVEALL                   = 2006;
  ID_ITEM_CLOSEFILE                 = 2007;
  ID_ITEM_CLOSEPROJECT              = 2008;
  ID_ITEM_PROPERTIES                = 2009;
  ID_ITEM_SAVEASPROJECT             = 2010;

  ID_SUB_EXPORT                     = 2011;
  ID_ITEM_EXPORTHTML                = 2012;
  ID_ITEM_EXPORTRTF                 = 2013;
  ID_ITEM_EXPORTPROJECT             = 2014;
  //free to 148 for export sub items
  ID_SUB_IMPORT                     = 3001;

  ID_ITEM_PRINT                     = 3002;
  ID_ITEM_PRINTSETUP                = 3003;
  ID_ITEM_EXIT                      = 3004;
  //free to 159 for file menu items

  //edit menu
  ID_ITEM_DECL                      = 4001;
  ID_ITEM_DEFINE                    = 4002;
  ID_ITEM_UNDO                      = 4003;
  ID_ITEM_REDO                      = 4004;
  ID_ITEM_CUT                       = 4005;
  ID_ITEM_COPY                      = 4006;
  ID_ITEM_PASTE                     = 4007;
  ID_ITEM_SELECTALL                 = 4008;
  ID_ITEM_DELETE                    = 4009;
  ID_SUB_INSERT                     = 4010;
  //sub items are dynamic

  ID_SUB_TOGGLEMARKS                = 4011;
  ID_SUB_GOTOMARKS                  = 4012;
  ID_MARKTEXT                       = 4013;
  ID_ITEM_DATETIME                  = 4014;
  ID_ITEM_COMMENTHEADER             = 4015;

  //free to 174 for edit menu items
  ID_ITEM_COMMENTSELECTION          = 22001;
  ID_ITEM_UNCOMMENTSELECTION        = 22002;
  ID_ITEM_INDENTSELECTION           = 22003;
  ID_ITEM_UNINDENTSELECTION         = 22004;
  ID_ITEM_SWAPHEADERSOURCE          = 22005;
  ID_ITEM_COLLAPSEALL               = 22006;
  ID_ITEM_UNCOLLAPSEALL             = 22007;

  //search menu
  ID_ITEM_FIND                      = 5001;
  ID_ITEM_FINDINALL                 = 5002;
  ID_ITEM_REPLACE                   = 5003;
  ID_ITEM_FINDNEXT                  = 5004;
  ID_ITEM_GOTO                      = 5005;
  ID_ITEM_INCREMENTAL               = 5006;
  ID_ITEM_GOTOFUNCTION              = 5007;

  //view menu
  ID_ITEM_FLOATWINDOW               = 5008;
  ID_ITEM_GOTOPROJECTVIEW           = 5009;
  ID_ITEM_GOTOCLASSBROWSER          = 5010;
  ID_ITEM_PROJECTVIEW               = 5011;
  ID_ITEM_Statusbar                 = 5012;
  ID_SUB_COMPOUTPUT                 = 5013;
  ID_ITEM_COMPOUTALWAYS             = 5014;
  ID_ITEM_COMPOUTONNEED             = 5015;
  ID_SUB_TOOLBARS                   = 5016;

  // toolbars
  ID_TOOLMAIN                       = 5017;
  ID_TOOLCOMPRUN                    = 5018;
  ID_TOOLPROJECT                    = 5019;
  ID_TOOLOPTIONS                    = 5020;
  ID_TOOLSPECIAL                    = 5021;
  ID_TOOLEDIT                       = 5022;
  ID_TOOLSEARCH                     = 5023;
  ID_ITEM_FLOATREPORT               = 5024;
  //free to 199 for view menu

  //project menu
  ID_ITEM_NEWFILE                   = 6001;
  ID_ITEM_ADDFILE                   = 6002;
  ID_ITEM_REMOVEFILE                = 6003;
  ID_ITEM_EDITRES                   = 6004;
  ID_ITEM_PROJECTOPTIONS            = 6005;
  ID_ITEM_EDITMAKE                  = 6006;
  ID_ITEM_ADDRESOURCE               = 6007;
  ID_ITEM_REMOVERESOURCE            = 6008;
  //free to 209 for project menu items

  //execute menu
  ID_ITEM_COMP                      = 7001;
  ID_ITEM_RUN                       = 7002;
  ID_ITEM_COMPRUN                   = 7003;
  ID_ITEM_REBUILD                   = 7004;
  ID_ITEM_CLEAN                     = 7005;
  ID_ITEM_RESULTS                   = 7006;
  ID_ITEM_SYNTAXCHECK               = 7007;
  ID_ITEM_PROGRAMRESET              = 7008;
  ID_ITEM_PROFILE                   = 7009;
  ID_ITEM_ABORTCOMP                 = 7010;
  ID_ITEM_DELPROFINFORMATION        = 7016;
  //free to 219 for execute menu
  //more IDs at 2900

  //options menu removed
  //tools menu
  ID_ITEM_COMPOPTIONS               = 7011;
  ID_ITEM_ENVIROOPTIONS             = 7012;
  ID_ITEM_EDITOROPTIONS             = 7013;
  ID_ITEM_TOOLCONFIG                = 7014;
  ID_ITEM_devShortcutsCONFIG           = 7015;
  // tool items dynamic
  //free to 229 for tool menu items

  //window menu
  ID_ITEM_FULLSCRMODE               = 8001;
  ID_ITEM_FULLSCRBACK               = 8002;
  ID_ITEM_NEXT                      = 8003;
  ID_ITEM_PREV                      = 8004;
  ID_ITEM_CLOSEALL                  = 8005;
  ID_FULLSCREEN_MSG                 = 8006;
  ID_ITEM_CLOSEALLBUTTHIS           = 8007;
  ID_ITEM_LIST                      = 8008;
  //free to 239 for window items

  //help menu
  ID_ITEM_ABOUT                     = 9001;
  ID_ITEM_UPDATECHECK               = 9002;
  ID_ITEM_CUSTOM                    = 9003;
  ID_ITEM_HELPDEVCPP                = 9004;

  //rest dynamic from help.ini

  //debug menu
  ID_ITEM_DEBUG                     = 10001;
  ID_ITEM_TOGGLEBREAK               = 10002;
  ID_ITEM_STEPNEXT                  = 10003;
  ID_ITEM_STEPOVER                  = 10004;
  ID_ITEM_WATCHADD                  = 10005;
  ID_ITEM_WATCHITEMS                = 10006;
  ID_ITEM_STOPEXECUTION             = 10007;
  ID_ITEM_WATCHEDIT                 = 10008;
  ID_ITEM_WATCHREMOVE               = 10009;
  ID_ITEM_STEPINTO                  = 10010;
  ID_ITEM_RUNTOCURSOR               = 10011;
  ID_ITEM_CPUWINDOW                 = 10012;
  ID_ITEM_ATTACHPROCESS             = 10013;
  ID_ITEM_MODIFYVALUE               = 10014;
  ID_ITEM_CLEARALL                  = 10015;

  //free to 259 for menu items

  // context menus
  // project popup
  ID_POP_NEWFILE                    = 10016;
  ID_POP_ADDFILE                    = 10017;
  ID_POP_REMOVEFILE                 = 10018;
  ID_POP_PRJOPTIONS                 = 10019;
  ID_POP_EDITRES                    = 10020;
  //free to 269 for project popup

  //unit popup
  ID_POP_REMOVE                     = 11001;
  ID_POP_RENAME                     = 11002;
  ID_POP_CLOSE                      = 11003;
  ID_POP_OPEN                       = 11004;
  ID_POP_OPEN2                      = 11005;
  //free to 279 for unit popup

  //editor popup
    // close file from unitpop
    // open header from unitpop
    // edit menu items from edit menu

  //toggle, goto, help created dynamically
  //new pulled from file-new sub menu

  //class browser popup
  ID_POP_GOTODECL                   = 12001;
  ID_POP_GOTOIMPL                   = 12002;
  ID_POP_NEWCLASS                   = 12003;
  ID_POP_NEWMEMBER                  = 12004;
  ID_POP_NEWVAR                     = 12005;
  ID_POP_VIEWMODE                   = 12006;
  ID_POP_VIEWALLFILES               = 12007;
  ID_POP_VIEWCURRENT                = 12008;
  ID_POP_ADDFOLDER                  = 12009;
  ID_POP_REMOVEFOLDER               = 12010;
  ID_POP_RENAMEFOLDER               = 12011;
  ID_POP_MAXFOLDERS                 = 12012;
  ID_POP_USECOLORS                  = 12013;
  ID_POP_VIEWPROJECT                = 12014;
  ID_POP_SHOWINHERITED              = 12015;
  ID_POP_OPENWITH                   = 12016;
  // free to 295 for class browser popup

  // free to 299 for context menu items

  //message control
  //tab captions
  ID_SHEET_COMP                     = 13001;
  ID_SHEET_RES                      = 13002;
  ID_SHEET_COMPLOG                  = 13003;
  ID_SHEET_FIND                     = 13004;
  ID_SHEET_CLOSE                    = 13005;
  ID_SHEET_DEBUG                    = 13006;
  // free to 310 for new sheets

  // pop menu
  ID_SHEET_POP_CLEAR                = 14001;
  ID_SHEET_POP_COPY                 = 14002;
  ID_SHEET_POP_HIDE                 = 14003;

  //column headers
  ID_COL_LINE                       = 15001;
  ID_COL_COL                        = 16002;
  ID_COL_FILE                       = 15002;
  ID_COL_ARGS                       = 15004;
  ID_COL_MSG                        = 15003;
  ID_COL_FLINE                      = 15003;
  ID_COL_FCOL                       = 16002;
  ID_COL_FFILE                      = 16002;
  ID_COL_FMSG                       = 16002;
  ID_COL_WATCHNAME                  = 16003;
  ID_COL_WATCHVAL                   = 16004;

  // may want to remap id_col_f* to own values
  // for true custom titles

  //compile log sheet
  ID_GRP_INFO                       = 16005;
  ID_GRP_COMPRESULTS                = 16006;
  ID_TOTALERRORS                    = 16007;
  ID_OUTPUTSIZE                     = 16008;
  //free to 329 for compile log sheet

  ID_DEB_WATCHES                    = 17001;
  ID_DEB_BACKTRACE                  = 17002;
  ID_DEB_OUTPUT                     = 17003;
  ID_DEB_SENDGDBCOMMAND             = 17004;
  ID_DEB_SEND                       = 17005;
  //free to 339 for column titles

  //specials toolbar
  ID_TB_NEW                         = 18001;
  ID_TB_INSERT                      = 18002;
  ID_TB_TOGGLE                      = 18003;
  ID_TB_GOTO                        = 18004;
  ID_TB_PROJECT                     = 18005;
  ID_TB_DEBUG                       = 18006;
  ID_TB_REPORT                      = 18007;
  //free to 349 for specials bar

  //non-visual controls (save/load dialogs etc...)
  //captions
  ID_NV_OPENFILE                    = 19001;
  ID_NV_OPENADD                     = 19002;
  ID_NV_EXPORT                      = 19003;
  ID_NV_SAVEFILE                    = 19004;
  ID_NV_SAVEPROJECT                 = 19005;
  ID_NV_HELPFILE                    = 19006;

  ID_NV_ADDWATCH                    = 20001;
  ID_NV_ENTERVAR                    = 20002;
  ID_NV_MODIFYVAR                   = 20003;
  ID_NV_NEWVAR                      = 20004;
  ID_NV_MODIFYVALUE                 = 20005;
  ID_NV_VARNAME                     = 20006;
  ID_NV_VARVALUE                    = 20007;

  //NOTE: need system for filters

  // Project Manager/Class Browser
  ID_LP_PROJECT                     = 21001;
  ID_LP_CLASSES                     = 21002;

  //free to 399 for main form items

{ -- end main form -- }


{ -- forms -- }

{  Editor options form }
    // caption
  ID_EOPT                           = 23001;
    // Tab captions
  ID_EOPT_GENTAB                    = 23002;
  ID_EOPT_DISPLAYTAB                = 23003;
  ID_EOPT_SYNTAXTAB                 = 23004;
  ID_EOPT_CODETAB                   = 23005;
  ID_EOPT_COMPLETIONTAB             = 23006;
  ID_EOPT_BROWSERTAB                = 23007;
  ID_EOPT_AUTOSAVETAB               = 23008;

    // sub-tab captions
  ID_EOPT_CPINSERTS                 = 24001;
  ID_EOPT_CPDEFAULT                 = 24002;
  ID_EOPT_AUTOSAVEFILE              = 24003;
  ID_EOPT_OPTIONS                   = 24004;
  ID_EOPT_CPSYMBOLS                 = 24005;

    // General Tab Items
  ID_EOPT_SAVESYNTAXQUESTION        = 25001;
  ID_EOPT_EDOPTIONS                 = 25002;
  ID_EOPT_AUTOINDENT                = 25003;
  ID_EOPT_INSERTMODE                = 25004;
  ID_EOPT_TAB2SPC                   = 25005;
  ID_EOPT_SMARTTABS                 = 25006;
  ID_EOPT_TRAILBLANKS               = 25008;
  ID_EOPT_GROUPUNDO                 = 25009;
  ID_EOPT_EHOMEKEY                  = 25010;
  ID_EOPT_PASTEOF                   = 25011;
  ID_EOPT_PASTEOL                   = 25012;
  ID_EOPT_DBLCLKLINE                = 25013;
  ID_EOPT_FINDTEXT                  = 25014;
  ID_EOPT_SMARTSCROLL               = 25015;
  ID_EOPT_HALFPAGE                  = 25016;
  ID_EOPT_SCROLLHINT                = 25017;
  ID_EOPT_USESYNTAX                 = 25018;
  ID_EOPT_DROPFILES                 = 25019;
  ID_EOPT_TABSIZE                   = 25020;
  ID_EOPT_MARGIN                    = 25021;
  ID_EOPT_VISIBLE                   = 25022;
  ID_EOPT_WIDTH                     = 25023;
  ID_EOPT_COLOR                     = 25024;
  ID_EOPT_CARET                     = 25025;
  ID_EOPT_INSCARET                  = 25026;
  ID_EOPT_OVERCARET                 = 25027;
  ID_EOPT_CARET1                    = 25028;
  ID_EOPT_CARET2                    = 25029;
  ID_EOPT_CARET3                    = 25030;
  ID_EOPT_CARET4                    = 25031;
  ID_EOPT_HIGHCURLINE               = 25032;
  ID_EOPT_TRIMTRAILINGSPACES        = 25033;
  ID_EOPT_GENERICENABLED            = 25034;
  ID_EOPT_GUTTERWIDTH               = 25035;

  ID_EOPT_SPECIALCHARS              = 26001;
  ID_EOPT_PARSERHINTS               = 26002;
  ID_EOPT_MATCH                     = 26003;
  ID_EOPT_APPENDNEWLINE             = 26004;
  ID_EOPT_HIGHLIGHTCURRLINE         = 26005;
  ID_EOPT_HINTHIGHLIGHT             = 26006;
  ID_EOPT_HINTWHITESPACE            = 26007;
  ID_EOPT_CLOSEBRACE                = 26008;

  // Display Tab
  ID_EOPT_CTRLSCROLLHINT            = 26009;
  ID_EOPT_EDFONT                    = 26010;
  ID_EOPT_FONT                      = 26011;
  ID_EOPT_SIZE                      = 26012;
  ID_EOPT_EDITORPRE                 = 26013;
  ID_EOPT_GUTTER                    = 26014;
  ID_EOPT_GUTTERAUTO                = 26015;
  ID_EOPT_LINENUM                   = 26016;
  ID_EOPT_LEADZERO                  = 26017;
  ID_EOPT_FIRSTZERO                 = 26018;
  ID_EOPT_GUTTERFNT                 = 26019;
  ID_EOPT_GUTTERPRE                 = 26020;

  // Syntax tab
  ID_EOPT_FORE                      = 26021;
  ID_EOPT_BACK                      = 26022;
  ID_EOPT_STYLE                     = 26023;
  ID_EOPT_BOLD                      = 26024;
  ID_EOPT_ITALIC                    = 26025;
  ID_EOPT_UNDERLINE                 = 26026;
  ID_EOPT_SPEED                     = 26027;
  ID_EOPT_ELEMENTS                  = 26028;
  ID_EOPT_SAVESYNTAX                = 26029;


    // Code Tab -- Inserts
  ID_EOPT_CICODE                    = 26030;
  ID_EOPT_CIMENU                    = 26031;
  ID_EOPT_CISECTION                 = 26032;
  ID_EOPT_CIDESC                    = 26033;
  ID_EOPT_DEFCODE                   = 26034;

    // Completion Tab
  ID_EOPT_CCCACHECHECK              = 26035;
  ID_EOPT_CCCACHELABEL              = 26036;
  ID_EOPT_COMPLETIONENABLE          = 26037;
  ID_EOPT_COMPLETIONDELAY           = 26038;
  ID_EOPT_COMPLETIONCOLOR           = 26039;

    // Class browsing Tab
  // 490 is empty now - you can use it ;)
  ID_EOPT_BROWSERLOCAL              = 26041;
  ID_EOPT_BROWSERSAMPLE             = 26042;
  ID_EOPT_BROWSERENGINE             = 26043;
  ID_EOPT_BROWSERVIEW               = 26044;
  ID_EOPT_BROWSERENABLE             = 26045;
  ID_EOPT_BROWSERGLOBAL             = 26046;

  ID_EOPT_ENABLEAUTOSAVE            = 26047;
  ID_EOPT_AUTOSAVEINTERNAL          = 26048;
  ID_EOPT_SAVEALLFILES              = 26049;
  ID_EOPT_SAVEONLYOPENFILE          = 26040;

  ID_NP                             = 26050;
  ID_NP_DESC                        = 26051;

  ID_EOPT_SYMBOLCOMPLETE            = 26052;
  ID_EOPT_SYMBOLGROUP               = 26053;
  ID_EOPT_SYMBOLBRACES              = 26054;
  ID_EOPT_SYMBOLPARENT              = 26055;
  ID_EOPT_SYMBOLINCLUDE             = 26056;
  ID_EOPT_SYMBOLSQUARE              = 26057;
  ID_EOPT_SYMBOLCOMMENT             = 26058;

  // new project
  ID_NP_PRJNAME                     = 27001;
  ID_NP_DEFAULTC                    = 27002;
  ID_NP_DEFAULTCPP                  = 27003;
  ID_NP_MAKEDEFAULT                 = 27004;
  ID_NP_PRJSHEET                    = 27005;
  ID_NP_PRJOPTIONS                  = 27006;
  
  // free to 519 for new project form

  //remove unit
  ID_RU                             = 28001;
  ID_RU_TEXT                        = 28002;
  //free to 524 for remove form

  // Goto line form
  ID_GOTO_CAPTION                   = 29001;
  ID_GOTO_TEXT                      = 29002;
  //free to 529 for goto form

  // Tool config form
  ID_TF                             = 30001;
  ID_TF_LABEL                       = 30002;
  // free to 534 for tool config form

  // Tool edit form
  ID_TE                             = 31001;
  ID_TE_TITLE                       = 31002;
  ID_TE_PROG                        = 31003;
  ID_TE_WORK                        = 31004;
  ID_TE_PARAM                       = 31005;
  ID_TE_INSERT                      = 31006;
  ID_TE_AVAIL                       = 31007;
  //free to 544 for tools edit form

  // Icon Lib Form
  ID_IF                             = 32001;
  ID_IF_USEICO                      = 32002;
  // free to 549 for icon library form

  // Configure devShortcuts component
  ID_SC_CAPTION                     = 33001;
  ID_SC_TITLE                       = 33002;
  ID_SC_TIP                         = 33003;
  ID_SC_HDRENTRY                    = 33004;
  ID_SC_HDRSHORTCUT                 = 33005;
  ID_SC_CANCEL                      = 33006;
  ID_SC_OK                          = 33007;

  // free to 559 for more forms.

  // Tool edit form macros
  ID_ET_MACROS                      = 34001;
  // free to 579 for macro desc (thats 19 max!)

  // About form
  ID_AB_CAPTION                     = 35001;
  ID_AB_AUTHOR2                     = 35002;
  ID_AB_LICENSE                     = 35003;
  ID_AB_WEBCAP                      = 35004;
  ID_AB_LBLBLOODSITE                = 35005;
  ID_AB_LBLMINGWSITE                = 35006;
  ID_AB_LBLFORUM                    = 35007;
  ID_AB_LBLMAIL                     = 35008;
  ID_AB_LBLEMAIL                    = 35009;
  ID_AB_BLOODSITE                   = 35010;
  ID_AB_MINGWSITE                   = 35011;
  ID_AB_FORUMS                      = 35012;
  ID_AB_MAILLIST                    = 35013;
  ID_AB_AUTHOR                      = 35014;
  ID_AB_UPDATE                      = 35015;
  ID_AB_LBLBLOG                     = 35016;
  ID_AB_BLOG                        = 35017;

  //Code Inserts Edit
  ID_CIE_EDCAPTION                  = 36001;
  ID_CIE_ADDCAPTION                 = 36002;
  ID_CIE_MENU                       = 36003;
  ID_CIE_DESC                       = 36004;
  ID_CIE_SEC                        = 36005;

  // Environment Options form
  ID_ENV_COMPPROGRESSWINDOW         = 37001;
  ID_ENV_SHOWPROGRESS               = 37002;
  ID_ENV_AUTOCLOSEPROGRESS          = 37003;
  ID_ENV_NOSPLASH                   = 37004;
  ID_ENV                            = 37005;
  ID_ENV_GENTAB                     = 37006;
  ID_ENV_PATHTAB                    = 37007;
  ID_ENV_AUTOOPEN                   = 37008;
  ID_ENV_DEFCPP                     = 37009;
  ID_ENV_SHOWBARS                   = 37010;
  ID_ENV_BACKUPS                    = 37011;
  ID_ENV_MINONRUN                   = 37012;
  ID_ENV_OPENSTYLE                  = 37013;
  ID_ENV_OPEN2K                     = 37014;
  ID_ENV_OPEN9X                     = 37015;
  ID_ENV_OPEN31                     = 37016;
  ID_ENV_LANGUAGE                   = 37017;
  ID_ENV_MSGTABS                    = 37018;
  ID_ENV_MRU                        = 37019;
  ID_ENV_USERDIR                    = 37020;
  ID_ENV_OUTPUTDIR                  = 37021;
  ID_ENV_ICOLIB                     = 37022;
  ID_ENV_SPLASH                     = 37023;
  ID_ENV_SELUSERDIR                 = 37024;
  ID_ENV_SELOUTPUTDIR               = 37025;
  ID_ENV_SELICOLIB                  = 37026;
  ID_ENV_AUTOALL                    = 37027;
  ID_ENV_AUTOFIRST                  = 37028;
  ID_ENV_AUTONONE                   = 37029;
  ID_ENV_MULTILINETABS              = 37030;
  ID_ENV_DBLFILES                   = 37031;
  ID_ENV_SELLANGDIR                 = 37032;
  ID_ENV_THEME                      = 37033;
  ID_ENV_INTERFACETAB               = 37034;
  // more at 2800

  // Find Form
  ID_FIND                           = 37035;
  ID_FIND_FINDTAB                   = 37036;
  ID_FIND_FINDALLTAB                = 37037;
  ID_FIND_TEXT                      = 37038;
  ID_FIND_GRP_OPTIONS               = 37039;
  ID_FIND_CASE                      = 37040;
  ID_FIND_WWORD                     = 37041;
  ID_FIND_GRP_WHERE                 = 37042;
  ID_FIND_PRJFILES                  = 37043;
  ID_FIND_OPENFILES                 = 37044;
  ID_FIND_GRP_SCOPE                 = 37045;
  ID_FIND_GLOBAL                    = 37046;
  ID_FIND_SELONLY                   = 37047;
  ID_FIND_GRP_ORIGIN                = 37048;
  ID_FIND_CURSOR                    = 37049;
  ID_FIND_ENTIRE                    = 37050;
  ID_FIND_GRP_DIRECTION             = 37051;
  ID_FIND_FORE                      = 37052;
  ID_FIND_BACK                      = 37053;

  ID_ENV_PAUSECONSOLE               = 37054;
  ID_ENV_AUTOREMEMBER               = 37055;

  // Replace form
  ID_RPLC                           = 38001;
  ID_RPLC_FINDTEXT                  = 38002;
  ID_RPLC_REPLACETEXT               = 38003;
  ID_RPLC_GRP_OPTIONS               = 38004;
  ID_RPLC_CASE                      = 38005;
  ID_RPLC_WHOLEWORD                 = 38006;
  ID_RPLC_PROMPT                    = 38007;
  ID_RPLC_GRP_DIRECTION             = 38008;
  ID_RPLC_FORWARD                   = 38009;
  ID_RPLC_BACKWARD                  = 38010;
  ID_RPLC_GRP_SCOPE                 = 38011;
  ID_RPLC_GLOBAL                    = 38012;
  ID_RPLC_SELONLY                   = 38013;
  ID_RPLC_GRP_ORIGIN                = 38014;
  ID_RPLC_CURSOR                    = 38015;
  ID_RPLC_ENTIRE                    = 38016;

  // Goto function form
  ID_GF_TEXT                        = 39001;
  ID_GF_TYPE                        = 39002;
  ID_GF_FUNCTION                    = 39003;
  ID_GF_LINE                        = 39004;

  // compiler options form
  ID_COPT                           = 40001;
  ID_COPT_COMPTAB                   = 40002;
  ID_COPT_DIRTAB                    = 40003;
  ID_COPT_CODEGENTAB                = 40004;
  ID_COPT_LINKERTAB                 = 40005;
  ID_COPT_PROGRAMSTAB               = 40006;
  ID_COPT_COMPILERTIP               = 40007;
  // free to 709 for new tabs

  ID_COPT_BIN                       = 41001;
  ID_COPT_LIB                       = 41002;
  ID_COPT_INCC                      = 41003;
  ID_COPT_INCCPP                    = 41004;

  // compiler tab
  ID_COPT_GRP_C                     = 42001;
  ID_COPT_GRP_CPP                   = 42002;
  ID_COPT_GRP_COMP                  = 42003;

  // free to 725 for new groups
  ID_COPT_ANSIC                     = 43001;
  ID_COPT_TRADITIONAL               = 43002;
  ID_COPT_WARNING                   = 43003;
  ID_COPT_ACCESS                    = 43004;
  ID_COPT_DOLLAR                    = 43005;
  ID_COPT_HEURISTICS                = 43006;
// not used anymore  ID_COPT_CYGWIN  = 43007;
  ID_COPT_ADDCOMP                   = 43008;
  ID_COPT_DELAY                     = 43009;
  ID_COPT_DELAYMSG                  = 43010;
  ID_COPT_ERRORLINE                 = 43011;

  //CodeGen Tab
  ID_COPT_GRP_CODEGEN               = 44001;
  ID_COPT_GRP_OPTIMIZE              = 44002;
  ID_COPT_EXCEPT                    = 45001;
  ID_COPT_DBLFLOAT                  = 45002;
  ID_COPT_MEM                       = 45003;
  ID_COPT_OPT1                      = 45004;
  ID_COPT_OPTBEST                   = 45005;
  ID_COPT_OPTMINOR                  = 45006;
  ID_COPT_MAKEFILEGEN               = 45007;
  ID_COPT_FASTDEP                   = 45008;
  ID_COPT_PROFILING                 = 45009;
  ID_COPT_PROFILE                   = 45010;

  //Linker Tab
  ID_COPT_OBJC                      = 45011;
  ID_COPT_DEBUG                     = 45012;
  ID_COPT_NOLIBS                    = 45013;
  ID_COPT_WIN32                     = 45014;
  ID_COPT_LINKADD                   = 45015;

  //CodeGen Tab (again!)
  ID_COPT_NOOPT                     = 45016;
  ID_COPT_OPTMORE                   = 45017;

  // Programs Tab
  ID_COPT_PROGRAMS                  = 46001;
  ID_COPT_COMPSETS                  = 46002;
  ID_COPT_NEWCOMPSET                = 46003;
  ID_COPT_PROMPTNEWCOMPSET          = 46004;
  ID_COPT_CANTDELETECOMPSET         = 46005;
  ID_COPT_DELETECOMPSET             = 46006;
  ID_COPT_RENAMECOMPSET             = 46007;
  ID_COPT_PROMPTRENAMECOMPSET       = 46008;
  ID_COPT_FURTHEROPTS               = 46009;

  // Help Menu Editor Form
  ID_HE                             = 47001;
  ID_HE_COL1                        = 47002;
  ID_HE_COL2                        = 47003;
  ID_HE_GRP_OPTIONS                 = 47004;
  ID_HE_ICON                        = 47005;
  ID_HE_GRP_MENU                    = 47006;
  ID_HE_SEC1                        = 47007;
  ID_HE_SEC2                        = 47008;
  ID_HE_ONPOP                       = 47009;
  ID_HE_GRP_ACTIONS                 = 47010;
  ID_HE_SEARCHWORD                  = 47011;
  ID_HE_AFFECTF1                    = 47012;

  ID_COPT_WARNINGPLUS               = 47013;
  ID_COPT_WARNINGEX                 = 47014;
  ID_COPT_ISOCONFORM                = 47015;
  ID_COPT_GRP_WARN                  = 47016;
  ID_COPT_SYNTAXONLY                = 47017;
  ID_COPT_TREATASERROR              = 47018;
  ID_COPT_FAILONFIRST               = 47019;

  // Print form
  ID_PRT                            = 48001;
  ID_PRT_GRP_PARAMS                 = 48002;
  ID_PRT_GRP_PAGES                  = 48003;
  ID_PRT_COLORS                     = 48004;
  ID_PRT_HIGHLIGHT                  = 48005;
  ID_PRT_WORDWRAP                   = 48006;
  ID_PRT_LINENUM                    = 48007;
  ID_PRT_PRTLINENUM                 = 48008;
  ID_PRT_PRTLINENUMMAR              = 48009;
  ID_PRT_COPIES                     = 48010;
  ID_PRT_SELONLY                    = 48011;
  ID_PRT_NOPRTLINENUMMAR            = 48012;

  //Check form
  ID_UCF                            = 49001;
  ID_UCF_GRP_TASK                   = 49002;
  ID_UCF_GRP_RESULTS                = 49003;
  ID_UCF_RELEASE                    = 49004;
  ID_UCF_NEED                       = 49005;
  ID_UCF_DESC                       = 49006;
  ID_UCF_SITES                      = 49007;

  //AddToDo form
  ID_ADDTODO_MENUITEM               = 50001;
  ID_ADDTODO_DESCRIPTION            = 50002;
  ID_ADDTODO_PRIORITY               = 50003;
  ID_ADDTODO_USER                   = 50004;

  //ViewToDo form
  ID_VIEWTODO_MENUITEM              = 51001;
  ID_VIEWTODO_FILENAME              = 51002;
  ID_VIEWTODO_DONE                  = 51003;
  ID_VIEWTODO_NOSHOWDONE            = 51004;
  ID_VIEWTODO_FILTER                = 51005;
  ID_VIEWTODO_FILTERONE             = 51006;
  ID_VIEWTODO_FILTERTWO             = 51007;
  ID_VIEWTODO_FILTERTHREE           = 51008;
  ID_VIEWTODO_FILTERFOUR            = 51009;
  ID_VIEWTODO_FILTERFIVE            = 51010;
  ID_VIEWTODO_FILTERSIX             = 51011;

  //Import MSVC++ form
  ID_MSVC_MENUITEM                  = 52001;
  ID_MSVC_SELECTMSVC                = 52002;
  ID_MSVC_OPTIONS                   = 52003;
  ID_MSVC_CONFIGURATION             = 52004;
  ID_MSVC_SELECTDEV                 = 52005;
  ID_MSVC_MSG_SUCCESS               = 52006;
  ID_MSVC_MSG_CANTLOCATETARGET      = 52007;

  //Import CB form
  ID_CB_MENUITEM                    = 52101;
  ID_CB_SELECTCB                    = 52102;
  ID_CB_OPTIONS                     = 52103;
  ID_CB_CONFIGURATION               = 52104;
  ID_CB_SELECTDEV                   = 52105;
  ID_CB_MSG_SUCCESS                 = 52106;
  ID_CB_MSG_CANTLOCATETARGET        = 52107;

  //Project Options
  ID_POPT                           = 53001;
  ID_POPT_GENTAB                    = 53002;
  ID_POPT_DIRTAB                    = 53003;
  ID_POPT_OUTTAB                    = 53023;
  ID_POPT_MAKTAB                    = 56010;
  ID_POPT_COMPTAB                   = 56013;
  //general tab
  ID_POPT_PRJNAME                   = 53004;
  ID_POPT_GRP_ICON                  = 53005;
  ID_POPT_ICOLIB                    = 53006;
  ID_POPT_ICOBRWSE                  = 53007;
  ID_POPT_GRP_TYPE                  = 53008;
  ID_POPT_TYPE1                     = 53009;
  ID_POPT_TYPE2                     = 53010;
  ID_POPT_TYPE3                     = 53011;
  ID_POPT_TYPE4                     = 53012;
  ID_POPT_USEGPP                    = 53013;
  ID_POPT_ADDITIONAL                = 53014;
  ID_POPT_COMP                      = 53015;
  ID_POPT_LINKER                    = 53016;
  ID_POPT_LIBDIRS                   = 53017;
  ID_POPT_RESFILES                  = 53018;
  ID_POPT_INCDIRS                   = 53019;
  ID_POPT_OPENICO                   = 53020;
  ID_POPT_OPENOBJ                   = 53021;
  ID_POPT_RESDIRS                   = 53022;

  // build output tab
  ID_POPT_OUTDIRGRP                 = 53024;
  ID_POPT_EXEOUT                    = 53025;
  ID_POPT_OBJOUT                    = 53026;
  ID_POPT_OVERRIDEOUT               = 53027;
  ID_POPT_ADDLIBRARY                = 53028;
  ID_POPT_UNITSFORMAT               = 53029;

  //New template form
  ID_NEWTPL_NAME                    = 54001;
  ID_NEWTPL_DESCRIPTION             = 54002;
  ID_NEWTPL_CATEGORY                = 54003;
  ID_NEWTPL_PROJECTNAME             = 54004;
  ID_NEWTPL_FILES                   = 54005;
  ID_NEWTPL_COMPILER                = 54006;
  ID_NEWTPL_LINKER                  = 54007;
  ID_NEWTPL_ICONS                   = 54008;
  ID_NEWTPL_PAGETEMPLATE            = 54009;
  ID_NEWTPL_PAGEFILES               = 54010;
  ID_NEWTPL_PAGEEXTRAS              = 54011;
  ID_NEWTPL_TEMPLATEICON            = 54012;
  ID_NEWTPL_PROJECTICON             = 54013;
  ID_NEWTPL_CREATE                  = 54014;
  ID_NEWTPL_CAPTION                 = 54015;
  ID_NEWTPL_INCDIR                  = 54016;
  ID_NEWTPL_LIBDIR                  = 54017;
  ID_NEWTPL_RESDIR                  = 54018;

  //New variable form
  ID_NEWVAR_VARTYPE                 = 55001;
  ID_NEWVAR_VARNAME                 = 55002;
  ID_NEWVAR_IMPLIN                  = 55003;
  ID_NEWVAR_SCOPE                   = 55004;
  ID_NEWVAR_CREATEREADFUNC          = 55005;
  ID_NEWVAR_CREATEWRITEFUNC         = 55006;
  ID_NEWVAR_FUNCNAME                = 55007;
  ID_NEWVAR_COMMENTS                = 55008;
  ID_NEWVAR_COMMENTSDESCR           = 55009;
  ID_NEWVAR_COMMENTSSTYLE           = 55010;
  ID_NEWVAR_BTN_CREATE              = 55011;
  ID_NEWVAR_BTN_CANCEL              = 55012;
  ID_NEWVAR_MSG_NOCLASS             = 55013;
  ID_NEWVAR_MSG_NOIMPL              = 55014;
  ID_NEWVAR_MSG_NOLINE              = 55015;

  //New member form
  ID_NEWMEMB_MEMBNAME               = 55016;
  ID_NEWMEMB_ARGS                   = 55017;

  //New class form
  ID_NEWCLASS_NAME                  = 55018;
  ID_NEWCLASS_INHERIT               = 55019;
  ID_NEWCLASS_INHERITANCE           = 55020;
  ID_NEWCLASS_INHERIT_FROM          = 55021;
  ID_NEWCLASS_INHERIT_HDR           = 55022;
  ID_NEWCLASS_CPPFILE               = 55023;
  ID_NEWCLASS_HFILE                 = 55024;
  ID_NEWCLASS_ADDTOPROJ             = 55025;

  //New member form
  ID_NEWMEMB_ATTRS                  = 55026;
  // free to 979

  //File properties form
  ID_PROPS_FILENAME                 = 56001;
  ID_PROPS_INPROJECT                = 56002;
  ID_PROPS_TOTAL                    = 56003;
  ID_PROPS_CODE                     = 56004;
  ID_PROPS_COMMENTS                 = 56005;
  ID_PROPS_INCLUDES                 = 56006;
  ID_PROPS_EMPTY                    = 56007;
  ID_PROPS_FILESIZE                 = 56008;
  ID_PROPS_ABSOLUTE                 = 56009;
  // 989 is used elsewhere >:
  ID_PROPS_RELATIVE                 = 56011;
  ID_PROPS_TIMESTAMP                = 56012;
  // 992 is used elsewhere >:

  //Profile analysis form
  ID_PROF_CAPTION                   = 57001;
  ID_PROF_TABFLAT                   = 57002;
  ID_PROF_TABGRAPH                  = 57003;
  ID_PROF_TABOPTS                   = 57004;
  ID_PROF_FUNCHIDING                = 57005;
  ID_PROF_HIDENOTLONG               = 57006;
  ID_PROF_SUPPRESSTATIC             = 57007;
  ID_PROF_PREVTEXT                  = 57008;
  ID_PROF_POSTTEXT                  = 57009;
  ID_PROF_APPLY                     = 57010;
  ID_PROF_CUSTOMCOMMANDS            = 57011;
  ID_PROF_CUSTOMUSE                 = 57012;

{ -- global strings  -- }
  //messages and prompts - 1000+

  ID_MSG_TEXTNOTFOUND               = 58001;
  ID_MSG_COULDNOTOPENFILE           = 58002;
  ID_MSG_SEARCHREPLACEPROMPT        = 58003;
  ID_MSG_FILEINPROJECT              = 58004;
  ID_MSG_CLOSEPROJECTPROMPT         = 58005;
  ID_MSG_ASKSAVECLOSE               = 58006;
  ID_MSG_CLOSECREATEPROJECT         = 58007;
  ID_MSG_FILERENAME                 = 58008;
  ID_MSG_ALREADYCOMP                = 58009;
  ID_MSG_SAVEPROJECT                = 58010;
  ID_MSG_FILEEXISTS                 = 58011;
  ID_MSG_NEWFILE                    = 58012;
  ID_MSG_NEWRES                     = 58013;
  ID_MSG_NODEBUGSYMBOLS             = 58014;
  ID_MSG_NOPROFILE                  = 58015;
  ID_MSG_NORUNPROFILE               = 58016;
  ID_MSG_ADDBROWSERFOLDER           = 58017;
  ID_MSG_REMOVEBROWSERFOLDER        = 58018;
  ID_MSG_RENAMEBROWSERFOLDER        = 58019;
  ID_MSG_FILEISREADONLY             = 58020;
  ID_MSG_FILEREADONLYERROR          = 58021;
  ID_MSG_SEGFAULT                   = 58022;
  ID_MSG_SOURCEMORERECENT           = 58023;
  ID_MSG_ATTACH                     = 58024;

  //errors 1500+
  ID_ERR_OPENFILE                   = 59001;
  ID_ERR_SAVEFILE                   = 59002;
  ID_ERR_RENAMEFILE                 = 59003;

  ID_ERR_SEARCHCANNOTBEEMPTY        = 59004;
  ID_ERR_NOTEXECUTABLE              = 59005;
  ID_ERR_PROJECTNOTCOMPILED         = 59006;
  ID_ERR_SRCNOTCOMPILED             = 59007;
  ID_ERR_NOMAKEFILE                 = 59008;
  ID_ERR_CLEANFAILED                = 59009;
  ID_ERR_TEMPLATE                   = 59010;
  ID_ERR_RESOURCE                   = 59011;
  ID_ERR_TEMPFNF                    = 59012;
  ID_ERR_NOTEMPLATE                 = 59013;
  ID_ERR_FILENOTFOUND               = 59014;
  ID_ERR_HOSTMISSING                = 59015;
  ID_ERR_HOSTNOTEXIST               = 59016;

  //other 2000+
  ID_UNTITLED                       = 60001;
  ID_READONLY                       = 60002;
  ID_INSERT                         = 60003;
  ID_OVERWRITE                      = 60004;
  ID_NEWPROJECT                     = 60005;
  ID_SPECIFY                        = 60006;
  ID_LINECOUNT                      = 60007;
  ID_RENAME                         = 60008;
  ID_EXECUTING                      = 60009;
  ID_EXECUTIONTERM                  = 60010;
  ID_BYTES                          = 60011;
  ID_MODIFIED                       = 60012;
  ID_ERROR                          = 60013;
  ID_INFO                           = 60014;
  ID_WARN                           = 60015;
  ID_QUESTION                       = 60016;
  ID_COMPILESUCCESS                 = 60017;
  ID_COMPILEABORT                   = 60018;
  ID_STATUSBARPLUS                  = 60019;
  ID_DONEPARSING                    = 60020;

{ -- Button Captions -- }
  ID_BTN_OK                         = 61001;
  ID_BTN_CANCEL                     = 61002;
  ID_BTN_HELP                       = 61003;
  ID_BTN_DEFAULT                    = 61004;
  ID_BTN_YES                        = 61005;
  ID_BTN_NO                         = 61006;
  ID_BTN_ADD                        = 61007;
  ID_BTN_EDIT                       = 61008;
  ID_BTN_REMOVE                     = 61009;
  ID_BTN_DELETE                     = 61010;
  ID_BTN_CLOSE                      = 61011;
  ID_BTN_AUTHOR                     = 61012;
  ID_BTN_FIND                       = 61013;
  ID_BTN_REPLACE                    = 61014;
  ID_BTN_DELINVAL                   = 61015;
  ID_BTN_REPLACEALL                 = 61016;
  ID_BTN_BROWSE                     = 61017;
  ID_BTN_REMOVEICON                 = 61018;
  ID_BTN_RENAME                     = 61019;
  ID_BTN_IMPORT                     = 61020;
  ID_BTN_FLOATDEBUG                 = 61021;
  ID_BTN_CLEAR                      = 61022;
  // free to 2600

{ -- end buttons -- }

{ CPU Window }
  ID_CPU_CAPTION                    = 62001;
  ID_CPU_ASMCODE                    = 62002;
  ID_CPU_FUNC                       = 62003;
  ID_CPU_SYNTAX                     = 62004;
  ID_CPU_REGISTERS                  = 62005;

{ Tip Of The Day }
  ID_TIPS_CAPTION                   = 63001;
  ID_TIPS_DIDYOUKNOW                = 63002;
  ID_TIPS_NOTIPSTODISPLAY           = 63003;
  ID_TIPS_DONTSHOWTIPS              = 63004;
  ID_TIPS_NEXTTIP                   = 63005;
  ID_TIPS_PREVIOUSTIP               = 63006;

{ Environment Options }
  ID_ENV_FASSTAB                    = 64001;
  ID_ENV_FASSTYPES                  = 64002;
  ID_ENV_FASSDESC                   = 64003;
  ID_ENV_CVSTAB                     = 64004;
  ID_ENV_CVSEXE                     = 64005;
  ID_ENV_CVSCOMPR                   = 64006;
  ID_ENV_CVSUSESSH                  = 64007;
  ID_ENV_TEMPLATESDIR               = 64008;
  ID_ENV_SELTEMPLATESDIR            = 64009;
  ID_ENV_XPTHEME                    = 64010;
  ID_ENV_EXTERNALS                  = 64011;
  ID_ENV_EXTERNPROG                 = 64012;
  ID_ENV_EXTERNEXT                  = 64013;
  ID_ENV_EXTERNPROGASSOCS           = 64014;
  ID_ENV_WATCHHINT                  = 64015;
  ID_ENV_WATCHERROR                 = 64016;
  ID_ENV_DEBUGGER                   = 64017;
  ID_ENV_GBALTCONFIG                = 64018;
  ID_ENV_USEALTCONFIG               = 64019;
  ID_ENV_CONFIGCHANGED              = 64020;
  ID_ENV_UACERROR                   = 64021;
  ID_ENV_UIFONT                     = 64022;

{ Execute menu }
  ID_ITEM_EXECPARAMS                = 65001;
  ID_ITEM_COMPCURRENT               = 65002;

{ Window list form }
  ID_WL                             = 66001;
  ID_WL_SELECT                      = 66002;

{ CVS Menu }
  ID_ITEM_CVSCURRENT                = 67001;
  ID_ITEM_CVSWHOLE                  = 67002;
  ID_MSG_CVSNOTCONFIGED             = 67003;

{ CVS form }
  ID_CVS_IMPORTTAB                  = 68001;
  ID_CVS_REPOSITORYTAB              = 68002;
  ID_CVS_GLOBALTAB                  = 68003;
  ID_CVS_CHECKOUTTAB                = 68004;
  ID_CVS_COMMITTAB                  = 68005;
  ID_CVS_UPDATETAB                  = 68006;
  ID_CVS_DIFFTAB                    = 68007;
  ID_CVS_LOGTAB                     = 68008;
  ID_CVS_OUTPUTTAB                  = 68009;
  ID_CVS_IMPORT                     = 68010;
  ID_CVS_CHECKOUT                   = 68011;
  ID_CVS_COMMIT                     = 68012;
  ID_CVS_UPDATE                     = 68013;
  ID_CVS_DIFF                       = 68014;
  ID_CVS_LOG                        = 68015;
  ID_CVS_IMPDIR                     = 68016;
  ID_CVS_IMPACTION                  = 68017;
  ID_CVS_IMPVENDOR                  = 68018;
  ID_CVS_IMPRELEASE                 = 68019;
  ID_CVS_LOGMSG                     = 68020;
  ID_CVS_MODULE                     = 68021;
  ID_CVS_IMPEXT                     = 68022;
  ID_CVS_COAS                       = 68023;
  ID_CVS_CODIR                      = 68024;
  ID_CVS_RECURSE                    = 68025;
  ID_CVS_RESETSTICKY                = 68026;
  ID_CVS_CREATEDIRS                 = 68027;
  ID_CVS_PRUNEEMPTY                 = 68028;
  ID_CVS_GETCLEAN                   = 68029;
  ID_CVS_BEFOREDATE                 = 68030;
  ID_CVS_REVISION                   = 68031;
  ID_CVS_GETMOSTRECENT              = 68032;
  ID_CVS_OTHERREVISIONS             = 68033;
  ID_CVS_DIFFUNIFIED                = 68034;
  ID_CVS_DIFF1                      = 68035;
  ID_CVS_DIFF2                      = 68036;
  ID_CVS_DIFF3                      = 68037;
  ID_CVS_ISDATE                     = 68038;
  ID_CVS_LOGDEFBRANCH               = 68039;
  ID_CVS_LOGRCS                     = 68040;
  ID_CVS_LOGNOTAG                   = 68041;
  ID_CVS_LOGFILTER                  = 68042;
  ID_CVS_LOGBYREV                   = 68043;
  ID_CVS_LOGBYDATE                  = 68044;
  ID_CVS_LOGBYUSER                  = 68045;
  ID_CVS_REPOSITORY                 = 68046;
  ID_CVS_REPDETAILS                 = 68047;
  ID_CVS_REPMETHOD                  = 68048;
  ID_CVS_REPUSER                    = 68049;
  ID_CVS_REPSERVER                  = 68050;
  ID_CVS_REPDIR                     = 68051;
  ID_CVS_ADD                        = 68052;
  ID_CVS_REMOVE                     = 68053;
  ID_CVS_ADDTAB                     = 68054;
  ID_CVS_REMOVETAB                  = 68055;
  ID_CVS_REMOVEFILE                 = 68056;

{ Parameters form }
  ID_PARAM_CAPTION                  = 69001;
  ID_PARAM_PARAMS                   = 69002;
  ID_PARAM_HOST                     = 69003;

{ Project options form (continued) }
  ID_POPT_FILESTAB                  = 70001;
  ID_POPT_PROJFILES                 = 70002;
  ID_POPT_PROJOPTS                  = 70003;
  ID_POPT_DEBUG                     = 70004;
  ID_POPT_PROFILE                   = 70005;
  ID_POPT_COMPINFO                  = 70006;
  ID_POPT_SUPPORTXP                 = 70007;
  ID_POPT_UNITOPTS                  = 70008;
  ID_POPT_COMPUNIT                  = 70009;
  ID_POPT_UNITUSEGPP                = 70010;
  ID_POPT_VERTAB                    = 70011;
  ID_POPT_INCLUDEVERSION            = 70012;
  ID_POPT_VDETAILS                  = 70013;
  ID_POPT_VMAJOR                    = 70014;
  ID_POPT_VMINOR                    = 70015;
  ID_POPT_VRELEASE                  = 70016;
  ID_POPT_VBUILD                    = 70017;
  ID_POPT_VLANG                     = 70018;
  ID_POPT_VADDITIONAL               = 70019;
  ID_POPT_OVERRIDEBUILDCMD          = 70020;
  ID_POPT_OUTPUTFILENAME            = 70021;
  ID_POPT_LINKUNIT                  = 70022;
  ID_POPT_BUILDPRIORITY             = 70023;
  ID_POPT_VAUTOINCBUILDNR           = 70024;
  ID_POPT_USECUSTOMMAKEFILE         = 70025;
  ID_POPT_INFOCUSTOMMAKEFILE        = 70026;
  ID_POPT_INCFILEMAKEFILE           = 70027;
  ID_POPT_SYNCPRODUCT               = 70028;
  // free to 3400

  // New compilter options
  ID_COPT_STRIP                     = 71001;
  ID_COPT_ARCH                      = 71002;
  ID_COPT_BUILTINPROC               = 71003;
  ID_COPT_OPTIMIZE                  = 71004;
  ID_COPT_STD                       = 71005;
  ID_COPT_PTRWIDTH                  = 71006;

  // Compile progress window
  ID_CMPRPG_COMPILER                = 72001;
  ID_CMPPRG_STATUS                  = 72002;
  ID_CMPPRG_FILE                    = 72003;
  ID_CMPPRG_ERRORS                  = 72004;
  ID_CMPPRG_WARNINGS                = 72005;
