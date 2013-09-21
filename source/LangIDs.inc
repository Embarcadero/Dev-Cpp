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
  comp      = 0;
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
  ID_MNU_FILE                           = 1;
  ID_MNU_EDIT                           = 2;
  ID_MNU_SEARCH                         = 3;
  ID_MNU_VIEW                           = 4;
  ID_MNU_PROJECT                        = 5;
  ID_MNU_EXECUTE                        = 6;
  ID_MNU_TOOLS                          = 7;
  ID_MNU_WINDOW                         = 8;
  ID_MNU_HELP                           = 9;
  ID_MNU_DEBUG                          = 10;
  //free to 119 for top level menus

  //file menu
  ID_SUB_NEW                            = 1001;
  ID_ITEM_NEWPROJECT                    = 1002;
  ID_ITEM_NEWSOURCE                     = 1003;
  ID_ITEM_NEWRESOURCE                   = 1004;
  ID_ITEM_NEWTEMPLATE                   = 1005;
  ID_ITEM_NEWCLASS                      = 1006;
  ID_HINT_NEW                           = 1007;
  //free to 129 for sub new items

  ID_ITEM_OPEN                          = 2001;
  ID_SUB_REOPEN                         = 2002;
  ID_ITEM_CLEARHISTORY                  = 2003;
  ID_ITEM_SAVEFILE                      = 2004;
  ID_ITEM_SAVEAS                        = 2005;
  ID_ITEM_SAVEALL                       = 2006;
  ID_ITEM_CLOSEFILE                     = 2007;
  ID_ITEM_CLOSEPROJECT                  = 2008;
  ID_ITEM_PROPERTIES                    = 2009;
  ID_ITEM_SAVEASPROJECT                 = 2010;

  ID_SUB_EXPORT                         = 2011;
  ID_ITEM_EXPORTHTML                    = 2012;
  ID_ITEM_EXPORTRTF                     = 2013;
  ID_ITEM_EXPORTPROJECT                 = 2014;
  ID_ITEM_EXPORTTEX                     = 2015;

  //free to 148 for export sub items
  ID_SUB_IMPORT                         = 3001;

  ID_ITEM_PRINT                         = 3002;
  ID_ITEM_PRINTSETUP                    = 3003;
  ID_ITEM_EXIT                          = 3004;
  //free to 159 for file menu items

  //edit menu
  ID_ITEM_DECL                          = 4001;
  ID_ITEM_DEFINE                        = 4002;
  ID_ITEM_UNDO                          = 4003;
  ID_ITEM_REDO                          = 4004;
  ID_ITEM_CUT                           = 4005;
  ID_ITEM_COPY                          = 4006;
  ID_ITEM_PASTE                         = 4007;
  ID_ITEM_SELECTALL                     = 4008;
  ID_ITEM_DELETE                        = 4009;
  ID_SUB_INSERT                         = 4010;
  //sub items are dynamic

  ID_SUB_TOGGLEMARKS                    = 4011;
  ID_SUB_GOTOMARKS                      = 4012;
  ID_MARKTEXT                           = 4013;
  ID_ITEM_DATETIME                      = 4014;
  ID_ITEM_COMMENTHEADER                 = 4015;
  ID_ITEM_COPYALL                       = 4016;
  ID_ITEM_CLEAR                         = 4017;

  //free to 174 for edit menu items
  ID_ITEM_COMMENTSELECTION              = 22001;
  ID_ITEM_UNCOMMENTSELECTION            = 22002;
  ID_ITEM_INDENTSELECTION               = 22003;
  ID_ITEM_UNINDENTSELECTION             = 22004;
  ID_ITEM_SWAPHEADERSOURCE              = 22005;
  ID_ITEM_COLLAPSEALL                   = 22006;
  ID_ITEM_UNCOLLAPSEALL                 = 22007;

  //search menu
  ID_ITEM_FIND                          = 5001;
  ID_ITEM_FINDINALL                     = 5002;
  ID_ITEM_REPLACE                       = 5003;
  ID_ITEM_FINDNEXT                      = 5004;
  ID_ITEM_GOTO                          = 5005;
  ID_ITEM_INCREMENTAL                   = 5006;
  ID_ITEM_GOTOFUNCTION                  = 5007;

  //view menu
  ID_ITEM_FLOATWINDOW                   = 5008;
  ID_ITEM_GOTOPROJECTVIEW               = 5009;
  ID_ITEM_GOTOCLASSBROWSER              = 5010;
  ID_ITEM_PROJECTVIEW                   = 5011;
  ID_ITEM_Statusbar                     = 5012;
  ID_SUB_COMPOUTPUT                     = 5013;
  ID_ITEM_COMPOUTALWAYS                 = 5014;
  ID_ITEM_COMPOUTONNEED                 = 5015;
  ID_SUB_TOOLBARS                       = 5016;

  // toolbars
  ID_TOOLMAIN                           = 5017;
  ID_TOOLCOMPRUN                        = 5018;
  ID_TOOLPROJECT                        = 5019;
  ID_TOOLOPTIONS                        = 5020;
  ID_TOOLSPECIAL                        = 5021;
  ID_TOOLEDIT                           = 5022;
  ID_TOOLSEARCH                         = 5023;
  ID_ITEM_FLOATREPORT                   = 5024;
  ID_ITEM_REPLACEFILES                  = 5025;
  //free to 199 for view menu

  //project menu
  ID_ITEM_NEWFILE                       = 6001;
  ID_ITEM_ADDFILE                       = 6002;
  ID_ITEM_REMOVEFILE                    = 6003;
  ID_ITEM_EDITRES                       = 6004;
  ID_ITEM_PROJECTOPTIONS                = 6005;
  ID_ITEM_EDITMAKE                      = 6006;
  ID_ITEM_ADDRESOURCE                   = 6007;
  ID_ITEM_REMOVERESOURCE                = 6008;
  //free to 209 for project menu items

  //execute menu
  ID_ITEM_COMP                          = 7001;
  ID_ITEM_RUN                           = 7002;
  ID_ITEM_COMPRUN                       = 7003;
  ID_ITEM_REBUILD                       = 7004;
  ID_ITEM_CLEAN                         = 7005;
  ID_ITEM_RESULTS                       = 7006;
  ID_ITEM_SYNTAXCHECK                   = 7007;
  ID_ITEM_PROGRAMRESET                  = 7008;
  ID_ITEM_PROFILE                       = 7009;
  ID_ITEM_ABORTCOMP                     = 7010;
  ID_ITEM_DELPROFINFORMATION            = 7016;
  //free to 219 for execute menu
  //more IDs at 2900

  //options menu removed
  //tools menu
  ID_ITEM_COMPOPTIONS                   = 7011;
  ID_ITEM_ENVIROOPTIONS                 = 7012;
  ID_ITEM_EDITOROPTIONS                 = 7013;
  ID_ITEM_TOOLCONFIG                    = 7014;
  ID_ITEM_SHORTCUTSCONFIG               = 7015;
  // tool items dynamic
  //free to 229 for tool menu items

  //window menu
  ID_ITEM_FULLSCRMODE                   = 8001;
  ID_ITEM_FULLSCRBACK                   = 8002;
  ID_ITEM_NEXT                          = 8003;
  ID_ITEM_PREV                          = 8004;
  ID_ITEM_CLOSEALL                      = 8005;
  ID_FULLSCREEN_MSG                     = 8006;
  ID_ITEM_CLOSEALLBUTTHIS               = 8007;
  ID_ITEM_LIST                          = 8008;
  //free to 239 for window items

  //help menu
  ID_ITEM_ABOUT                         = 9001;
  ID_ITEM_UPDATECHECK                   = 9002;
  ID_ITEM_CUSTOM                        = 9003;
  ID_ITEM_HELPDEVCPP                    = 9004;

  //rest dynamic from help.ini

  //debug menu
  ID_ITEM_DEBUG                         = 10001;
  ID_ITEM_TOGGLEBREAK                   = 10002;
  ID_ITEM_STEPNEXT                      = 10003;
  ID_ITEM_STEPOVER                      = 10004;
  ID_ITEM_WATCHADD                      = 10005;
  ID_ITEM_WATCHITEMS                    = 10006;
  ID_ITEM_STOPEXECUTION                 = 10007;
  ID_ITEM_WATCHEDIT                     = 10008;
  ID_ITEM_WATCHREMOVE                   = 10009;
  ID_ITEM_STEPINTO                      = 10010;
  ID_ITEM_RUNTOCURSOR                   = 10011;
  ID_ITEM_CPUWINDOW                     = 10012;
  ID_ITEM_ATTACHPROCESS                 = 10013;
  ID_ITEM_MODIFYVALUE                   = 10014;
  ID_ITEM_CLEARALL                      = 10015;

  //free to 259 for menu items

  // context menus
  // project popup
  ID_POP_NEWFILE                        = 10016;
  ID_POP_ADDFILE                        = 10017;
  ID_POP_REMOVEFILE                     = 10018;
  ID_POP_PRJOPTIONS                     = 10019;
  ID_POP_EDITRES                        = 10020;
  ID_ITEM_NEXTINS                       = 10021;
  ID_ITEM_STEPINS                       = 10022;
  ID_ITEM_SKIPFUNCTION                  = 10023;
  //free to 269 for project popup

  //unit popup
  ID_POP_REMOVE                         = 11001;
  ID_POP_RENAME                         = 11002;
  ID_POP_CLOSE                          = 11003;
  ID_POP_OPEN                           = 11004;
  ID_POP_OPEN2                          = 11005;
  //free to 279 for unit popup

  //editor popup
    // close file from unitpop
    // open header from unitpop
    // edit menu items from edit menu

  //toggle, goto, help created dynamically
  //new pulled from file-new sub menu

  //class browser popup
  ID_POP_GOTODECL                       = 12001;
  ID_POP_GOTOIMPL                       = 12002;
  ID_POP_NEWCLASS                       = 12003;
  ID_POP_NEWMEMBER                      = 12004;
  ID_POP_NEWVAR                         = 12005;
  ID_POP_VIEWMODE                       = 12006;
  ID_POP_VIEWALLFILES                   = 12007;
  ID_POP_VIEWCURRENT                    = 12008;
  ID_POP_ADDFOLDER                      = 12009;
  ID_POP_REMOVEFOLDER                   = 12010;
  ID_POP_RENAMEFOLDER                   = 12011;
  ID_POP_MAXFOLDERS                     = 12012;
  ID_POP_USECOLORS                      = 12013;
  ID_POP_VIEWPROJECT                    = 12014;
  ID_POP_SHOWINHERITED                  = 12015;
  ID_POP_OPENWITH                       = 12016;
  // free to 295 for class browser popup

  // free to 299 for context menu items

  //message control
  //tab captions
  ID_SHEET_COMP                         = 13001;
  ID_SHEET_RES                          = 13002;
  ID_SHEET_COMPLOG                      = 13003;
  ID_SHEET_FIND                         = 13004;
  ID_SHEET_CLOSE                        = 13005;
  ID_SHEET_DEBUG                        = 13006;
  // free to 310 for new sheets

  // pop menu
  ID_SHEET_POP_CLEAR                    = 14001;
  ID_SHEET_POP_COPY                     = 14002;
  ID_SHEET_POP_HIDE                     = 14003;

  //column headers
  ID_COL_LINE                           = 15001;
  ID_COL_COL                            = 16002;
  ID_COL_FILE                           = 15002;
  ID_COL_ARGS                           = 15004;
  ID_COL_MSG                            = 15003;
  ID_COL_FLINE                          = 15003;
  ID_COL_FCOL                           = 16002;
  ID_COL_FFILE                          = 16002;
  ID_COL_FMSG                           = 16002;
  ID_COL_WATCHNAME                      = 16003;
  ID_COL_WATCHVAL                       = 16004;

  // may want to remap id_col_f* to own values
  // for true custom titles

  //compile log sheet
  ID_GRP_INFO                           = 16005;
  ID_GRP_COMPRESULTS                    = 16006;
  ID_TOTALERRORS                        = 16007;
  ID_OUTPUTSIZE                         = 16008;
  //free to 329 for compile log sheet

  ID_DEB_WATCHES                        = 17001;
  ID_DEB_BACKTRACE                      = 17002;
  ID_DEB_OUTPUT                         = 17003;
  ID_DEB_SENDGDBCOMMAND                 = 17004;
  ID_DEB_SEND                           = 17005;
  ID_DEB_EVALUATE                       = 17006;
  //free to 339 for column titles

  //specials toolbar
  ID_TB_NEW                             = 18001;
  ID_TB_INSERT                          = 18002;
  ID_TB_TOGGLE                          = 18003;
  ID_TB_GOTO                            = 18004;
  ID_TB_PROJECT                         = 18005;
  ID_TB_DEBUG                           = 18006;
  ID_TB_REPORT                          = 18007;
  //free to 349 for specials bar

  //non-visual controls (save/load dialogs etc...)
  //captions
  ID_NV_OPENFILE                        = 19001;
  ID_NV_OPENADD                         = 19002;
  ID_NV_EXPORT                          = 19003;
  ID_NV_SAVEFILE                        = 19004;
  ID_NV_SAVEPROJECT                     = 19005;
  ID_NV_HELPFILE                        = 19006;

  ID_NV_ADDWATCH                        = 20001;
  ID_NV_ENTERVAR                        = 20002;
  ID_NV_MODIFYVAR                       = 20003;
  ID_NV_NEWVAR                          = 20004;
  ID_NV_MODIFYVALUE                     = 20005;
  ID_NV_VARNAME                         = 20006;
  ID_NV_VARVALUE                        = 20007;

  //NOTE: need system for filters

  // Project Manager/Class Browser
  ID_LP_PROJECT                         = 21001;
  ID_LP_CLASSES                         = 21002;

  //free to 399 for main form items

{ -- end main form -- }


{ -- forms -- }

{  Editor options form }
    // caption
  ID_EOPT                               = 23001;
    // Tab captions
  ID_EOPT_GENTAB                        = 23002;
  ID_EOPT_DISPLAYTAB                    = 23003;
  ID_EOPT_SYNTAXTAB                     = 23004;
  ID_EOPT_CODETAB                       = 23005;
  ID_EOPT_COMPLETIONTAB                 = 23006;
  ID_EOPT_BROWSERTAB                    = 23007;
  ID_EOPT_AUTOSAVETAB                   = 23008;

    // sub-tab captions
  ID_EOPT_CPINSERTS                     = 24001;
  ID_EOPT_CPDEFAULT                     = 24002;
  ID_EOPT_AUTOSAVEFILE                  = 24003;
  ID_EOPT_OPTIONS                       = 24004;
  ID_EOPT_CPSYMBOLS                     = 24005;

    // General Tab Items
  ID_EOPT_SAVESYNTAXQUESTION            = 25001;
  ID_EOPT_EDOPTIONS                     = 25002;
  ID_EOPT_AUTOINDENT                    = 25003;
  ID_EOPT_INSERTMODE                    = 25004;
  ID_EOPT_TAB2SPC                       = 25005;
  ID_EOPT_SMARTTABS                     = 25006;
  ID_EOPT_TRAILBLANKS                   = 25008;
  ID_EOPT_GROUPUNDO                     = 26001;
  ID_EOPT_EHOMEKEY                      = 26002;
  ID_EOPT_PASTEOF                       = 26003;
  ID_EOPT_PASTEOL                       = 26004;
  ID_EOPT_DBLCLKLINE                    = 26005;
  ID_EOPT_FINDTEXT                      = 26006;
  ID_EOPT_SMARTSCROLL                   = 26007;
  ID_EOPT_HALFPAGE                      = 26008;
  ID_EOPT_SCROLLHINT                    = 26009;
  ID_EOPT_USESYNTAX                     = 26010;
  ID_EOPT_DROPFILES                     = 26011;
  ID_EOPT_TABSIZE                       = 26012;
  ID_EOPT_MARGIN                        = 26013;
  ID_EOPT_VISIBLE                       = 26014;
  ID_EOPT_WIDTH                         = 26015;
  ID_EOPT_COLOR                         = 26016;
  ID_EOPT_CARET                         = 26017;
  ID_EOPT_INSCARET                      = 26018;
  ID_EOPT_OVERCARET                     = 26019;
  ID_EOPT_CARET1                        = 26020;
  ID_EOPT_CARET2                        = 26021;
  ID_EOPT_CARET3                        = 26022;
  ID_EOPT_CARET4                        = 26023;
  ID_EOPT_HIGHCURLINE                   = 26024;
  ID_EOPT_TRIMTRAILINGSPACES            = 26025;
  ID_EOPT_GENERICENABLED                = 26026;
  ID_EOPT_GUTTERWIDTH                   = 26027;

  ID_EOPT_SPECIALCHARS                  = 27001;
  ID_EOPT_PARSERHINTS                   = 27002;
  ID_EOPT_MATCH                         = 27003;
  ID_EOPT_APPENDNEWLINE                 = 27004;
  ID_EOPT_HIGHLIGHTCURRLINE             = 27005;
  ID_EOPT_HINTHIGHLIGHT                 = 27006;
  ID_EOPT_HINTWHITESPACE                = 27007;
  ID_EOPT_CLOSEBRACE                    = 27008;

  // Display Tab
  ID_EOPT_CTRLSCROLLHINT                = 27009;
  ID_EOPT_EDFONT                        = 27010;
  ID_EOPT_FONT                          = 27011;
  ID_EOPT_SIZE                          = 27012;
  ID_EOPT_EDITORPRE                     = 27013;
  ID_EOPT_GUTTER                        = 27014;
  ID_EOPT_GUTTERAUTO                    = 27015;
  ID_EOPT_LINENUM                       = 27016;
  ID_EOPT_LEADZERO                      = 27017;
  ID_EOPT_FIRSTZERO                     = 27018;
  ID_EOPT_GUTTERFNT                     = 27019;
  ID_EOPT_GUTTERPRE                     = 27020;

  // Syntax tab
  ID_EOPT_FORE                          = 27021;
  ID_EOPT_BACK                          = 27022;
  ID_EOPT_STYLE                         = 27023;
  ID_EOPT_BOLD                          = 27024;
  ID_EOPT_ITALIC                        = 27025;
  ID_EOPT_UNDERLINE                     = 27026;
  ID_EOPT_SPEED                         = 27027;
  ID_EOPT_ELEMENTS                      = 27028;
  ID_EOPT_SAVESYNTAX                    = 27029;


    // Code Tab -- Inserts
  ID_EOPT_CICODE                        = 27030;
  ID_EOPT_CIMENU                        = 27031;
  ID_EOPT_CISECTION                     = 27032;
  ID_EOPT_CIDESC                        = 27033;
  ID_EOPT_DEFCODE                       = 27034;

    // Completion Tab
  ID_EOPT_CCCACHECHECK                  = 27035;
  ID_EOPT_CCCACHELABEL                  = 27036;
  ID_EOPT_COMPLETIONENABLE              = 27037;
  ID_EOPT_COMPLETIONDELAY               = 27038;
  ID_EOPT_COMPLETIONCOLOR               = 27039;

    // Class browsing Tab
  // 490 is empty now - you can use it ;)
  ID_EOPT_BROWSERLOCAL                  = 27041;
  ID_EOPT_BROWSERSAMPLE                 = 27042;
  ID_EOPT_BROWSERENGINE                 = 27043;
  ID_EOPT_BROWSERVIEW                   = 27044;
  ID_EOPT_BROWSERENABLE                 = 27045;
  ID_EOPT_BROWSERGLOBAL                 = 27046;

  ID_EOPT_ENABLEAUTOSAVE                = 27047;
  ID_EOPT_AUTOSAVEINTERNAL              = 27048;
  ID_EOPT_SAVEALLFILES                  = 27049;
  ID_EOPT_SAVEONLYOPENFILE              = 27040;

  ID_NP                                 = 27050;
  ID_NP_DESC                            = 27051;

  ID_EOPT_SYMBOLCOMPLETE                = 27052;
  ID_EOPT_SYMBOLGROUP                   = 27053;
  ID_EOPT_SYMBOLBRACES                  = 27054;
  ID_EOPT_SYMBOLPARENT                  = 27055;
  ID_EOPT_SYMBOLINCLUDE                 = 27056;
  ID_EOPT_SYMBOLSQUARE                  = 27057;
  ID_EOPT_SYMBOLCOMMENT                 = 27058;

  // new project
  ID_NP_PRJNAME                         = 28001;
  ID_NP_DEFAULTC                        = 28002;
  ID_NP_DEFAULTCPP                      = 28003;
  ID_NP_MAKEDEFAULT                     = 28004;
  ID_NP_PRJSHEET                        = 28005;
  ID_NP_PRJOPTIONS                      = 28006;
  
  // free to 519 for new project form

  //remove unit
  ID_RU                                 = 29001;
  ID_RU_TEXT                            = 29002;
  //free to 524 for remove form

  // Goto line form
  ID_GOTO_CAPTION                       = 30001;
  ID_GOTO_TEXT                          = 30002;
  //free to 529 for goto form

  // Tool config form
  ID_TF                                 = 31001;
  ID_TF_LABEL                           = 31002;
  // free to 534 for tool config form

  // Tool edit form
  ID_TE                                 = 32001;
  ID_TE_TITLE                           = 32002;
  ID_TE_PROG                            = 32003;
  ID_TE_WORK                            = 32004;
  ID_TE_PARAM                           = 32005;
  ID_TE_INSERT                          = 32006;
  ID_TE_AVAIL                           = 32007;
  //free to 544 for tools edit form

  // Icon Lib Form
  ID_IF                                 = 33001;
  ID_IF_USEICO                          = 33002;
  // free to 549 for icon library form

  // Configure devShortcuts component
  ID_SC_CAPTION                         = 34001;
  ID_SC_TITLE                           = 34002;
  ID_SC_TIP                             = 34003;
  ID_SC_HDRENTRY                        = 34004;
  ID_SC_HDRSHORTCUT                     = 34005;
  ID_SC_CANCEL                          = 34006;
  ID_SC_OK                              = 34007;

  // free to 559 for more forms.

  // Tool edit form macros
  ID_ET_MACROS                          = 35001;
  // free to 579 for macro desc (thats 19 max!)

  // About form
  ID_AB_CAPTION                         = 36001;
  ID_AB_AUTHOR2                         = 36002;
  ID_AB_LICENSE                         = 36003;
  ID_AB_WEBCAP                          = 36004;
  ID_AB_LBLBLOODSITE                    = 36005;
  ID_AB_LBLMINGWSITE                    = 36006;
  ID_AB_LBLFORUM                        = 36007;
  ID_AB_LBLMAIL                         = 36008;
  ID_AB_LBLEMAIL                        = 36009;
  ID_AB_BLOODSITE                       = 36010;
  ID_AB_MINGWSITE                       = 36011;
  ID_AB_FORUMS                          = 36012;
  ID_AB_MAILLIST                        = 36013;
  ID_AB_AUTHOR                          = 36014;
  ID_AB_UPDATE                          = 36015;
  ID_AB_LBLBLOG                         = 36016;
  ID_AB_BLOG                            = 36017;

  //Code Inserts Edit
  ID_CIE_EDCAPTION                      = 37001;
  ID_CIE_ADDCAPTION                     = 37002;
  ID_CIE_MENU                           = 37003;
  ID_CIE_DESC                           = 37004;
  ID_CIE_SEC                            = 37005;

  // Environment Options form
  ID_ENV_COMPPROGRESSWINDOW             = 38001;
  ID_ENV_SHOWPROGRESS                   = 38002;
  ID_ENV_AUTOCLOSEPROGRESS              = 38003;
  ID_ENV_NOSPLASH                       = 38004;
  ID_ENV                                = 38005;
  ID_ENV_GENTAB                         = 38006;
  ID_ENV_PATHTAB                        = 38007;
  ID_ENV_AUTOOPEN                       = 38008;
  ID_ENV_DEFCPP                         = 38009;
  ID_ENV_SHOWBARS                       = 38010;
  ID_ENV_BACKUPS                        = 38011;
  ID_ENV_MINONRUN                       = 38012;
  ID_ENV_OPENSTYLE                      = 38013;
  ID_ENV_OPEN2K                         = 38014;
  ID_ENV_OPEN9X                         = 38015;
  ID_ENV_OPEN31                         = 38016;
  ID_ENV_LANGUAGE                       = 38017;
  ID_ENV_MSGTABS                        = 38018;
  ID_ENV_MRU                            = 38019;
  ID_ENV_USERDIR                        = 38020;
  ID_ENV_OUTPUTDIR                      = 38021;
  ID_ENV_ICOLIB                         = 38022;
  ID_ENV_SPLASH                         = 38023;
  ID_ENV_SELUSERDIR                     = 38024;
  ID_ENV_SELOUTPUTDIR                   = 38025;
  ID_ENV_SELICOLIB                      = 38026;
  ID_ENV_AUTOALL                        = 38027;
  ID_ENV_AUTOFIRST                      = 38028;
  ID_ENV_AUTONONE                       = 38029;
  ID_ENV_MULTILINETABS                  = 38030;
  ID_ENV_DBLFILES                       = 38031;
  ID_ENV_SELLANGDIR                     = 38032;
  ID_ENV_THEME                          = 38033;
  ID_ENV_INTERFACETAB                   = 38034;
  // more at 2800

  // Find Form
  ID_FIND                               = 38035;
  ID_FIND_FINDTAB                       = 38036;
  ID_FIND_FINDALLTAB                    = 38037;
  ID_FIND_TEXT                          = 38038;
  ID_FIND_GRP_OPTIONS                   = 38039;
  ID_FIND_CASE                          = 38040;
  ID_FIND_WWORD                         = 38041;
  ID_FIND_GRP_WHERE                     = 38042;
  ID_FIND_PRJFILES                      = 38043;
  ID_FIND_OPENFILES                     = 38044;
  ID_FIND_GRP_SCOPE                     = 38045;
  ID_FIND_GLOBAL                        = 38046;
  ID_FIND_SELONLY                       = 38047;
  ID_FIND_GRP_ORIGIN                    = 38048;
  ID_FIND_CURSOR                        = 38049;
  ID_FIND_ENTIRE                        = 38050;
  ID_FIND_GRP_DIRECTION                 = 38051;
  ID_FIND_FORE                          = 38052;
  ID_FIND_BACK                          = 38053;

  ID_ENV_PAUSECONSOLE                   = 38054;
  ID_ENV_AUTOREMEMBER                   = 38055;

  ID_FIND_REPLACE                       = 38056;
  ID_FIND_REPLACEFILES                  = 38057;
  ID_FIND_REPLACEWITH                   = 38058;
  ID_FIND_PROMPTREPLACE                 = 38059;
  ID_FIND_CURRENTFILE                   = 38060;

  // Goto function form
  ID_GF_TEXT                            = 40001;
  ID_GF_TYPE                            = 40002;
  ID_GF_FUNCTION                        = 40003;
  ID_GF_LINE                            = 40004;

  // compiler options form
  ID_COPT                               = 41001;
  ID_COPT_COMPTAB                       = 41002;
  ID_COPT_DIRTAB                        = 41003;
  ID_COPT_CODEGENTAB                    = 41004;
  ID_COPT_LINKERTAB                     = 41005;
  ID_COPT_PROGRAMSTAB                   = 41006;
  ID_COPT_COMPILERTIP                   = 41007;
  // free to 709 for new tabs

  ID_COPT_BIN                           = 42001;
  ID_COPT_LIB                           = 42002;
  ID_COPT_INCC                          = 42003;
  ID_COPT_INCCPP                        = 42004;

  // compiler tab
  ID_COPT_GRP_C                         = 43001;
  ID_COPT_GRP_CPP                       = 43002;
  ID_COPT_GRP_COMP                      = 43003;
  ID_COPT_GRP_OUTPUT                    = 43004;

  // free to 725 for new groups
  ID_COPT_ANSIC                         = 44001;
  ID_COPT_TRADITIONAL                   = 44002;
  ID_COPT_WARNING                       = 44003;
  ID_COPT_ACCESS                        = 44004;
  ID_COPT_DOLLAR                        = 44005;
  ID_COPT_HEURISTICS                    = 44006;
// not used anymore  ID_COPT_CYGWIN      = 44007;
  ID_COPT_ADDCOMP                       = 44008;
  ID_COPT_DELAY                         = 44009;
  ID_COPT_DELAYMSG                      = 44010;
  ID_COPT_ERRORLINE                     = 44011;
  ID_COPT_NOASM                         = 44012;
  ID_COPT_NULNEW                        = 44013;
  ID_COPT_CSPACE                        = 44014;

  //CodeGen Tab
  ID_COPT_GRP_CODEGEN                   = 45001;
  ID_COPT_GRP_OPTIMIZE                  = 45002;
  ID_COPT_EXCEPT                        = 46001;
  ID_COPT_DBLFLOAT                      = 46002;
  ID_COPT_MEM                           = 46003;
  ID_COPT_OPT1                          = 46004;
  ID_COPT_OPTBEST                       = 46005;
  ID_COPT_OPTMINOR                      = 46006;
  ID_COPT_MAKEFILEGEN                   = 46007;
  ID_COPT_FASTDEP                       = 46008;
  ID_COPT_PROFILING                     = 46009;
  ID_COPT_PROFILE                       = 46010;

  //Linker Tab
  ID_COPT_OBJC                          = 46011;
  ID_COPT_DEBUG                         = 46012;
  ID_COPT_NOLIBS                        = 46013;
  ID_COPT_WIN32                         = 46014;
  ID_COPT_LINKADD                       = 46015;

  //CodeGen Tab (again!)
  ID_COPT_NOOPT                         = 46016;
  ID_COPT_OPTMORE                       = 46017;
  ID_COPT_ASSEMBLY                      = 46018;
  ID_COPT_PIPES                         = 46019;

  // Programs Tab
  ID_COPT_PROGRAMS                      = 47001;
  ID_COPT_COMPSETS                      = 47002;
  ID_COPT_NEWCOMPSET                    = 47003;
  ID_COPT_PROMPTNEWCOMPSET              = 47004;
  ID_COPT_CANTDELETECOMPSET             = 47005;
  ID_COPT_DELETECOMPSET                 = 47006;
  ID_COPT_RENAMECOMPSET                 = 47007;
  ID_COPT_PROMPTRENAMECOMPSET           = 47008;
  ID_COPT_FURTHEROPTS                   = 47009;

  // Help Menu Editor Form
  ID_HE                                 = 48001;
  ID_HE_COL1                            = 48002;
  ID_HE_COL2                            = 48003;
  ID_HE_GRP_OPTIONS                     = 48004;
  ID_HE_ICON                            = 48005;
  ID_HE_GRP_MENU                        = 48006;
  ID_HE_SEC1                            = 48007;
  ID_HE_SEC2                            = 48008;
  ID_HE_ONPOP                           = 48009;
  ID_HE_GRP_ACTIONS                     = 48010;
  ID_HE_SEARCHWORD                      = 48011;
  ID_HE_AFFECTF1                        = 48012;

  ID_COPT_WARNINGPLUS                   = 48013;
  ID_COPT_WARNINGEX                     = 48014;
  ID_COPT_ISOCONFORM                    = 48015;
  ID_COPT_GRP_WARN                      = 48016;
  ID_COPT_SYNTAXONLY                    = 48017;
  ID_COPT_TREATASERROR                  = 48018;
  ID_COPT_FAILONFIRST                   = 48019;

  // Print form
  ID_PRT                                = 49001;
  ID_PRT_GRP_PARAMS                     = 49002;
  ID_PRT_GRP_PAGES                      = 49003;
  ID_PRT_COLORS                         = 49004;
  ID_PRT_HIGHLIGHT                      = 49005;
  ID_PRT_WORDWRAP                       = 49006;
  ID_PRT_LINENUM                        = 49007;
  ID_PRT_PRTLINENUM                     = 49008;
  ID_PRT_PRTLINENUMMAR                  = 49009;
  ID_PRT_COPIES                         = 49010;
  ID_PRT_SELONLY                        = 49011;
  ID_PRT_NOPRTLINENUMMAR                = 49012;

  //Check form
  ID_UCF                                = 50001;
  ID_UCF_GRP_TASK                       = 50002;
  ID_UCF_GRP_RESULTS                    = 50003;
  ID_UCF_RELEASE                        = 50004;
  ID_UCF_NEED                           = 50005;
  ID_UCF_DESC                           = 50006;
  ID_UCF_SITES                          = 50007;

  //AddToDo form
  ID_ADDTODO_MENUITEM                   = 51001;
  ID_ADDTODO_DESCRIPTION                = 51002;
  ID_ADDTODO_PRIORITY                   = 51003;
  ID_ADDTODO_USER                       = 51004;

  //ViewToDo form
  ID_VIEWTODO_MENUITEM                  = 52001;
  ID_VIEWTODO_FILENAME                  = 52002;
  ID_VIEWTODO_DONE                      = 52003;
  ID_VIEWTODO_NOSHOWDONE                = 52004;
  ID_VIEWTODO_FILTER                    = 52005;
  ID_VIEWTODO_FILTERONE                 = 52006;
  ID_VIEWTODO_FILTERTWO                 = 52007;
  ID_VIEWTODO_FILTERTHREE               = 52008;
  ID_VIEWTODO_FILTERFOUR                = 52009;
  ID_VIEWTODO_FILTERFIVE                = 52010;
  ID_VIEWTODO_FILTERSIX                 = 52011;

  //Import MSVC++ form
  ID_MSVC_MENUITEM                      = 53001;
  ID_MSVC_SELECTMSVC                    = 53002;
  ID_MSVC_OPTIONS                       = 53003;
  ID_MSVC_CONFIGURATION                 = 53004;
  ID_MSVC_SELECTDEV                     = 53005;
  ID_MSVC_MSG_SUCCESS                   = 53006;
  ID_MSVC_MSG_CANTLOCATETARGET          = 53007;

  //Import CB form
  ID_CB_MENUITEM                        = 54001;
  ID_CB_SELECTCB                        = 54002;
  ID_CB_OPTIONS                         = 54003;
  ID_CB_CONFIGURATION                   = 54004;
  ID_CB_SELECTDEV                       = 54005;
  ID_CB_MSG_SUCCESS                     = 54006;
  ID_CB_MSG_CANTLOCATETARGET            = 54007;

  //Project Options
  ID_POPT                               = 55001;
  ID_POPT_GENTAB                        = 55002;
  ID_POPT_DIRTAB                        = 55003;
  ID_POPT_OUTTAB                        = 55023;
  ID_POPT_MAKTAB                        = 58010;
  ID_POPT_COMPTAB                       = 58013;
  //general tab
  ID_POPT_PRJNAME                       = 55004;
  ID_POPT_GRP_ICON                      = 55005;
  ID_POPT_ICOLIB                        = 55006;
  ID_POPT_ICOBRWSE                      = 55007;
  ID_POPT_GRP_TYPE                      = 55008;
  ID_POPT_TYPE1                         = 55009;
  ID_POPT_TYPE2                         = 55010;
  ID_POPT_TYPE3                         = 55011;
  ID_POPT_TYPE4                         = 55012;
  ID_POPT_USEGPP                        = 55013;
  ID_POPT_ADDITIONAL                    = 55014;
  ID_POPT_COMP                          = 55015;
  ID_POPT_LINKER                        = 55016;
  ID_POPT_LIBDIRS                       = 55017;
  ID_POPT_RESFILES                      = 55018;
  ID_POPT_INCDIRS                       = 55019;
  ID_POPT_OPENICO                       = 55020;
  ID_POPT_OPENOBJ                       = 55021;
  ID_POPT_RESDIRS                       = 55022;

  // build output tab
  ID_POPT_OUTDIRGRP                     = 55024;
  ID_POPT_EXEOUT                        = 55025;
  ID_POPT_OBJOUT                        = 55026;
  ID_POPT_OVERRIDEOUT                   = 55027;
  ID_POPT_ADDLIBRARY                    = 55028;
  ID_POPT_UNITSFORMAT                   = 55029;

  //New template form
  ID_NEWTPL_NAME                        = 56001;
  ID_NEWTPL_DESCRIPTION                 = 56002;
  ID_NEWTPL_CATEGORY                    = 56003;
  ID_NEWTPL_PROJECTNAME                 = 56004;
  ID_NEWTPL_FILES                       = 56005;
  ID_NEWTPL_COMPILER                    = 56006;
  ID_NEWTPL_LINKER                      = 56007;
  ID_NEWTPL_ICONS                       = 56008;
  ID_NEWTPL_PAGETEMPLATE                = 56009;
  ID_NEWTPL_PAGEFILES                   = 56010;
  ID_NEWTPL_PAGEEXTRAS                  = 56011;
  ID_NEWTPL_TEMPLATEICON                = 56012;
  ID_NEWTPL_PROJECTICON                 = 56013;
  ID_NEWTPL_CREATE                      = 56014;
  ID_NEWTPL_CAPTION                     = 56015;
  ID_NEWTPL_INCDIR                      = 56016;
  ID_NEWTPL_LIBDIR                      = 56017;
  ID_NEWTPL_RESDIR                      = 56018;

  //New variable form
  ID_NEWVAR_VARTYPE                     = 57001;
  ID_NEWVAR_VARNAME                     = 57002;
  ID_NEWVAR_IMPLIN                      = 57003;
  ID_NEWVAR_SCOPE                       = 57004;
  ID_NEWVAR_CREATEREADFUNC              = 57005;
  ID_NEWVAR_CREATEWRITEFUNC             = 57006;
  ID_NEWVAR_FUNCNAME                    = 57007;
  ID_NEWVAR_COMMENTS                    = 57008;
  ID_NEWVAR_COMMENTSDESCR               = 57009;
  ID_NEWVAR_COMMENTSSTYLE               = 57010;
  ID_NEWVAR_BTN_CREATE                  = 57011;
  ID_NEWVAR_BTN_CANCEL                  = 57012;
  ID_NEWVAR_MSG_NOCLASS                 = 57013;
  ID_NEWVAR_MSG_NOIMPL                  = 57014;
  ID_NEWVAR_MSG_NOLINE                  = 57015;

  //New member form
  ID_NEWMEMB_MEMBNAME                   = 57016;
  ID_NEWMEMB_ARGS                       = 57017;

  //New class form
  ID_NEWCLASS_NAME                      = 57018;
  ID_NEWCLASS_INHERIT                   = 57019;
  ID_NEWCLASS_INHERITANCE               = 57020;
  ID_NEWCLASS_INHERIT_FROM              = 57021;
  ID_NEWCLASS_INHERIT_HDR               = 57022;
  ID_NEWCLASS_CPPFILE                   = 57023;
  ID_NEWCLASS_HFILE                     = 57024;
  ID_NEWCLASS_ADDTOPROJ                 = 57025;

  //New member form
  ID_NEWMEMB_ATTRS                      = 57026;
  // free to 979

  //File properties form
  ID_PROPS_FILENAME                     = 58001;
  ID_PROPS_INPROJECT                    = 58002;
  ID_PROPS_TOTAL                        = 58003;
  ID_PROPS_CODE                         = 58004;
  ID_PROPS_COMMENTS                     = 58005;
  ID_PROPS_INCLUDES                     = 58006;
  ID_PROPS_EMPTY                        = 58007;
  ID_PROPS_FILESIZE                     = 58008;
  ID_PROPS_ABSOLUTE                     = 58009;
  // 989 is used elsewhere >:
  ID_PROPS_RELATIVE                     = 58011;
  ID_PROPS_TIMESTAMP                    = 58012;
  // 992 is used elsewhere >:

  //Profile analysis form
  ID_PROF_CAPTION                       = 59001;
  ID_PROF_TABFLAT                       = 59002;
  ID_PROF_TABGRAPH                      = 59003;
  ID_PROF_TABOPTS                       = 59004;
  ID_PROF_FUNCHIDING                    = 59005;
  ID_PROF_HIDENOTLONG                   = 59006;
  ID_PROF_SUPPRESSTATIC                 = 59007;
  ID_PROF_PREVTEXT                      = 59008;
  ID_PROF_POSTTEXT                      = 59009;
  ID_PROF_APPLY                         = 59010;
  ID_PROF_CUSTOMCOMMANDS                = 59011;
  ID_PROF_CUSTOMUSE                     = 59012;

{ -- global strings  -- }
  //messages and prompts - 1000+

  ID_MSG_TEXTNOTFOUND                   = 60001;
  ID_MSG_COULDNOTOPENFILE               = 60002;
  ID_MSG_SEARCHREPLACEPROMPT            = 60003;
  ID_MSG_FILEINPROJECT                  = 60004;
  ID_MSG_CLOSEPROJECTPROMPT             = 60005;
  ID_MSG_ASKSAVECLOSE                   = 60006;
  ID_MSG_CLOSECREATEPROJECT             = 60007;
  ID_MSG_FILERENAME                     = 60008;
  ID_MSG_ALREADYCOMP                    = 60009;
  ID_MSG_SAVEPROJECT                    = 60010;
  ID_MSG_FILEEXISTS                     = 60011;
  ID_MSG_NEWFILE                        = 60012;
  ID_MSG_NEWRES                         = 60013;
  ID_MSG_NODEBUGSYMBOLS                 = 60014;
  ID_MSG_NOPROFILE                      = 60015;
  ID_MSG_NORUNPROFILE                   = 60016;
  ID_MSG_ADDBROWSERFOLDER               = 60017;
  ID_MSG_REMOVEBROWSERFOLDER            = 60018;
  ID_MSG_RENAMEBROWSERFOLDER            = 60019;
  ID_MSG_FILEISREADONLY                 = 60020;
  ID_MSG_FILEREADONLYERROR              = 60021;
  ID_MSG_SEGFAULT                       = 60022;
  ID_MSG_SOURCEMORERECENT               = 60023;
  ID_MSG_ATTACH                         = 60024;

  //errors 1500+
  ID_ERR_OPENFILE                       = 61001;
  ID_ERR_SAVEFILE                       = 61002;
  ID_ERR_RENAMEFILE                     = 61003;

  ID_ERR_SEARCHCANNOTBEEMPTY            = 61004;
  ID_ERR_NOTEXECUTABLE                  = 61005;
  ID_ERR_PROJECTNOTCOMPILED             = 61006;
  ID_ERR_SRCNOTCOMPILED                 = 61007;
  ID_ERR_NOMAKEFILE                     = 61008;
  ID_ERR_CLEANFAILED                    = 61009;
  ID_ERR_TEMPLATE                       = 61010;
  ID_ERR_RESOURCE                       = 61011;
  ID_ERR_TEMPFNF                        = 61012;
  ID_ERR_NOTEMPLATE                     = 61013;
  ID_ERR_FILENOTFOUND                   = 61014;
  ID_ERR_HOSTMISSING                    = 61015;
  ID_ERR_HOSTNOTEXIST                   = 61016;
  ID_ERR_PROJECTNOTCOMPILEDSUGGEST      = 61017;
  ID_ERR_SRCNOTCOMPILEDSUGGEST          = 61018;

  //other 2000+
  ID_UNTITLED                           = 62001;
  ID_READONLY                           = 62002;
  ID_INSERT                             = 62003;
  ID_OVERWRITE                          = 62004;
  ID_NEWPROJECT                         = 62005;
  ID_SPECIFY                            = 62006;
  ID_LINECOUNT                          = 62007;
  ID_RENAME                             = 62008;
  ID_EXECUTING                          = 62009;
  ID_EXECUTIONTERM                      = 62010;
  ID_BYTES                              = 62011;
  ID_MODIFIED                           = 62012;
  ID_ERROR                              = 62013;
  ID_INFO                               = 62014;
  ID_WARN                               = 62015;
  ID_QUESTION                           = 62016;
  ID_COMPILESUCCESS                     = 62017;
  ID_COMPILEABORT                       = 62018;
  ID_STATUSBARPLUS                      = 62019;
  ID_DONEPARSING                        = 62020;
  ID_SEARCHCOUNT                        = 62021;

{ -- Button Captions -- }
  ID_BTN_OK                             = 63001;
  ID_BTN_CANCEL                         = 63002;
  ID_BTN_HELP                           = 63003;
  ID_BTN_DEFAULT                        = 63004;
  ID_BTN_YES                            = 63005;
  ID_BTN_NO                             = 63006;
  ID_BTN_ADD                            = 63007;
  ID_BTN_EDIT                           = 63008;
  ID_BTN_REMOVE                         = 63009;
  ID_BTN_DELETE                         = 63010;
  ID_BTN_CLOSE                          = 63011;
  ID_BTN_AUTHOR                         = 63012;
  ID_BTN_FIND                           = 63013;
  ID_BTN_REPLACE                        = 63014;
  ID_BTN_DELINVAL                       = 63015;
  ID_BTN_REPLACEALL                     = 63016;
  ID_BTN_BROWSE                         = 63017;
  ID_BTN_REMOVEICON                     = 63018;
  ID_BTN_RENAME                         = 63019;
  ID_BTN_IMPORT                         = 63020;
  ID_BTN_FLOATDEBUG                     = 63021;
  ID_BTN_CLEAR                          = 63022;
  // free to 2600

{ -- end buttons -- }

{ CPU Window }
  ID_CPU_CAPTION                        = 64001;
  ID_CPU_ASMCODE                        = 64002;
  ID_CPU_FUNC                           = 64003;
  ID_CPU_SYNTAX                         = 64004;
  ID_CPU_REGISTERS                      = 64005;

{ Tip Of The Day }
  ID_TIPS_CAPTION                       = 65001;
  ID_TIPS_DIDYOUKNOW                    = 65002;
  ID_TIPS_NOTIPSTODISPLAY               = 65003;
  ID_TIPS_DONTSHOWTIPS                  = 65004;
  ID_TIPS_NEXTTIP                       = 65005;
  ID_TIPS_PREVIOUSTIP                   = 65006;

{ Environment Options }
  ID_ENV_FASSTAB                        = 66001;
  ID_ENV_FASSTYPES                      = 66002;
  ID_ENV_FASSDESC                       = 66003;
  ID_ENV_CVSTAB                         = 66004;
  ID_ENV_CVSEXE                         = 66005;
  ID_ENV_CVSCOMPR                       = 66006;
  ID_ENV_CVSUSESSH                      = 66007;
  ID_ENV_TEMPLATESDIR                   = 66008;
  ID_ENV_SELTEMPLATESDIR                = 66009;
  ID_ENV_XPTHEME                        = 66010;
  ID_ENV_EXTERNALS                      = 66011;
  ID_ENV_EXTERNPROG                     = 66012;
  ID_ENV_EXTERNEXT                      = 66013;
  ID_ENV_EXTERNPROGASSOCS               = 66014;
  ID_ENV_WATCHHINT                      = 66015;
  ID_ENV_WATCHERROR                     = 66016;
  ID_ENV_DEBUGGER                       = 66017;
  ID_ENV_GBALTCONFIG                    = 66018;
  ID_ENV_USEALTCONFIG                   = 66019;
  ID_ENV_CONFIGCHANGED                  = 66020;
  ID_ENV_UACERROR                       = 66021;
  ID_ENV_UIFONT                         = 66022;

{ Execute menu }
  ID_ITEM_EXECPARAMS                    = 67001;
  ID_ITEM_COMPCURRENT                   = 67002;

{ Window list form }
  ID_WL                                 = 68001;
  ID_WL_SELECT                          = 68002;

{ CVS Menu }
  ID_ITEM_CVSCURRENT                    = 69001;
  ID_ITEM_CVSWHOLE                      = 69002;
  ID_MSG_CVSNOTCONFIGED                 = 69003;

{ CVS form }
  ID_CVS_IMPORTTAB                      = 70001;
  ID_CVS_REPOSITORYTAB                  = 70002;
  ID_CVS_GLOBALTAB                      = 70003;
  ID_CVS_CHECKOUTTAB                    = 70004;
  ID_CVS_COMMITTAB                      = 70005;
  ID_CVS_UPDATETAB                      = 70006;
  ID_CVS_DIFFTAB                        = 70007;
  ID_CVS_LOGTAB                         = 70008;
  ID_CVS_OUTPUTTAB                      = 70009;
  ID_CVS_IMPORT                         = 70010;
  ID_CVS_CHECKOUT                       = 70011;
  ID_CVS_COMMIT                         = 70012;
  ID_CVS_UPDATE                         = 70013;
  ID_CVS_DIFF                           = 70014;
  ID_CVS_LOG                            = 70015;
  ID_CVS_IMPDIR                         = 70016;
  ID_CVS_IMPACTION                      = 70017;
  ID_CVS_IMPVENDOR                      = 70018;
  ID_CVS_IMPRELEASE                     = 70019;
  ID_CVS_LOGMSG                         = 70020;
  ID_CVS_MODULE                         = 70021;
  ID_CVS_IMPEXT                         = 70022;
  ID_CVS_COAS                           = 70023;
  ID_CVS_CODIR                          = 70024;
  ID_CVS_RECURSE                        = 70025;
  ID_CVS_RESETSTICKY                    = 70026;
  ID_CVS_CREATEDIRS                     = 70027;
  ID_CVS_PRUNEEMPTY                     = 70028;
  ID_CVS_GETCLEAN                       = 70029;
  ID_CVS_BEFOREDATE                     = 70030;
  ID_CVS_REVISION                       = 70031;
  ID_CVS_GETMOSTRECENT                  = 70032;
  ID_CVS_OTHERREVISIONS                 = 70033;
  ID_CVS_DIFFUNIFIED                    = 70034;
  ID_CVS_DIFF1                          = 70035;
  ID_CVS_DIFF2                          = 70036;
  ID_CVS_DIFF3                          = 70037;
  ID_CVS_ISDATE                         = 70038;
  ID_CVS_LOGDEFBRANCH                   = 70039;
  ID_CVS_LOGRCS                         = 70040;
  ID_CVS_LOGNOTAG                       = 70041;
  ID_CVS_LOGFILTER                      = 70042;
  ID_CVS_LOGBYREV                       = 70043;
  ID_CVS_LOGBYDATE                      = 70044;
  ID_CVS_LOGBYUSER                      = 70045;
  ID_CVS_REPOSITORY                     = 70046;
  ID_CVS_REPDETAILS                     = 70047;
  ID_CVS_REPMETHOD                      = 70048;
  ID_CVS_REPUSER                        = 70049;
  ID_CVS_REPSERVER                      = 70050;
  ID_CVS_REPDIR                         = 70051;
  ID_CVS_ADD                            = 70052;
  ID_CVS_REMOVE                         = 70053;
  ID_CVS_ADDTAB                         = 70054;
  ID_CVS_REMOVETAB                      = 70055;
  ID_CVS_REMOVEFILE                     = 70056;

{ Parameters form }
  ID_PARAM_CAPTION                      = 71001;
  ID_PARAM_PARAMS                       = 71002;
  ID_PARAM_HOST                         = 71003;

{ Project options form (continued) }
  ID_POPT_FILESTAB                      = 72001;
  ID_POPT_PROJFILES                     = 72002;
  ID_POPT_PROJOPTS                      = 72003;
  ID_POPT_DEBUG                         = 72004;
  ID_POPT_PROFILE                       = 72005;
  ID_POPT_COMPINFO                      = 72006;
  ID_POPT_SUPPORTXP                     = 72007;
  ID_POPT_UNITOPTS                      = 72008;
  ID_POPT_COMPUNIT                      = 72009;
  ID_POPT_UNITUSEGPP                    = 72010;
  ID_POPT_VERTAB                        = 72011;
  ID_POPT_INCLUDEVERSION                = 72012;
  ID_POPT_VDETAILS                      = 72013;
  ID_POPT_VMAJOR                        = 72014;
  ID_POPT_VMINOR                        = 72015;
  ID_POPT_VRELEASE                      = 72016;
  ID_POPT_VBUILD                        = 72017;
  ID_POPT_VLANG                         = 72018;
  ID_POPT_VADDITIONAL                   = 72019;
  ID_POPT_OVERRIDEBUILDCMD              = 72020;
  ID_POPT_OUTPUTFILENAME                = 72021;
  ID_POPT_LINKUNIT                      = 72022;
  ID_POPT_BUILDPRIORITY                 = 72023;
  ID_POPT_VAUTOINCBUILDNR               = 72024;
  ID_POPT_USECUSTOMMAKEFILE             = 72025;
  ID_POPT_INFOCUSTOMMAKEFILE            = 72026;
  ID_POPT_INCFILEMAKEFILE               = 72027;
  ID_POPT_SYNCPRODUCT                   = 72028;
  ID_POPT_DEFCPP                        = 72029;
  // free to 3400

  // New compilter options
  ID_COPT_STRIP                         = 73001;
  ID_COPT_ARCH                          = 73002;
  ID_COPT_BUILTINPROC                   = 73003;
  ID_COPT_OPTIMIZE                      = 73004;
  ID_COPT_STD                           = 73005;
  ID_COPT_PTRWIDTH                      = 73006;
  ID_COPT_TUNE                          = 73007;

  // Compile progress window
  ID_CMPRPG_COMPILER                    = 74001;
  ID_CMPPRG_STATUS                      = 74002;
  ID_CMPPRG_FILE                        = 74003;
  ID_CMPPRG_ERRORS                      = 74004;
  ID_CMPPRG_WARNINGS                    = 74005;
  ID_CMPPRG_CAPTION                     = 74006;
