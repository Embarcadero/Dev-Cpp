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
  comp = compiler
  res = resource
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
  ID_MNU_FILE                      = 100;
  ID_MNU_EDIT                      = 101;
  ID_MNU_SEARCH                    = 102;
  ID_MNU_VIEW                      = 103;
  ID_MNU_PROJECT                   = 104;
  ID_MNU_EXECUTE                   = 105;
  ID_MNU_TOOLS                     = 106;
  ID_MNU_WINDOW                    = 107;
  ID_MNU_HELP                      = 108;
  ID_MNU_DEBUG                     = 109;
  //free to 119 for top level menus

  //file menu
  ID_SUB_NEW                       = 120;
  ID_ITEM_NEWPROJECT               = 121;
  ID_ITEM_NEWSOURCE                = 122;
  ID_ITEM_NEWRESOURCE              = 123;
  ID_ITEM_NEWTEMPLATE              = 124;
  //free to 129 for sub new items

  ID_ITEM_OPEN                     = 130;
  ID_SUB_REOPEN                    = 131;
  ID_ITEM_CLEARHISTORY             = 132;
  ID_ITEM_SAVEFILE                 = 133;
  ID_ITEM_SAVEAS                   = 134;
  ID_ITEM_SAVEALL                  = 135;
  ID_ITEM_CLOSEFILE                = 136;
  ID_ITEM_CLOSEPROJECT             = 137;
  ID_ITEM_PROPERTIES               = 138;
  ID_ITEM_SAVEASPROJECT            = 139;

  ID_SUB_EXPORT                    = 140;
  ID_ITEM_EXPORTHTML               = 141;
  ID_ITEM_EXPORTRTF                = 142;
  ID_ITEM_EXPORTPROJECT            = 143;
  //free to 148 for export sub items
  ID_SUB_IMPORT                    = 149;

  ID_ITEM_PRINT                    = 150;
  ID_ITEM_PRINTSETUP               = 151;
  ID_ITEM_EXIT                     = 152;
  //free to 159 for file menu items

  //edit menu
  ID_ITEM_UNDO                     = 160;
  ID_ITEM_REDO                     = 161;
  ID_ITEM_CUT                      = 162;
  ID_ITEM_COPY                     = 163;
  ID_ITEM_PASTE                    = 164;
  ID_ITEM_SELECTALL                = 165;
  ID_ITEM_DELETE                   = 166;
  ID_SUB_INSERT                    = 167;
  //sub items are dynamic

  ID_SUB_TOGGLEMARKS               = 168;
  ID_SUB_GOTOMARKS                 = 169;
  ID_MARKTEXT                      = 170;
  ID_ITEM_DATETIME                 = 171;
  ID_ITEM_COMMENTHEADER            = 172;
  //free to 174 for edit menu items
  ID_ITEM_COMMENTSELECTION         = 380;
  ID_ITEM_UNCOMMENTSELECTION       = 381;
  ID_ITEM_INDENTSELECTION          = 382;
  ID_ITEM_UNINDENTSELECTION        = 383;
  ID_ITEM_SWAPHEADERSOURCE         = 384;

  //search menu
  ID_ITEM_FIND                     = 175;
  ID_ITEM_FINDINALL                = 176;
  ID_ITEM_REPLACE                  = 177;
  ID_ITEM_FINDNEXT                 = 178;
  ID_ITEM_GOTO                     = 179;
  ID_ITEM_INCREMENTAL              = 180;
  ID_ITEM_GOTOFUNCTION             = 181;

  //view menu
  ID_ITEM_FLOATWINDOW              = 182;
  ID_ITEM_GOTOPROJECTVIEW          = 183;
  ID_ITEM_GOTOCLASSBROWSER         = 184;
  ID_ITEM_PROJECTVIEW              = 185;
  ID_ITEM_STATUSBAR                = 186;
  ID_SUB_COMPOUTPUT                = 187;
  ID_ITEM_COMPOUTALWAYS            = 188;
  ID_ITEM_COMPOUTONNEED            = 189;
  ID_SUB_TOOLBARS                  = 190;

  // toolbars
  ID_TOOLMAIN                      = 191;
  ID_TOOLCOMPRUN                   = 192;
  ID_TOOLPROJECT                   = 193;
  ID_TOOLOPTIONS                   = 194;
  ID_TOOLSPECIAL                   = 195;
  ID_TOOLEDIT                      = 196;
  ID_TOOLSEARCH                    = 197;
  ID_ITEM_FLOATREPORT              = 198;
  //free to 199 for view menu

  //project menu
  ID_ITEM_NEWFILE                  = 200;
  ID_ITEM_ADDFILE                  = 201;
  ID_ITEM_REMOVEFILE               = 202;
  ID_ITEM_EDITRES                  = 203;
  ID_ITEM_PROJECTOPTIONS           = 204;
  ID_ITEM_EDITMAKE                 = 205;
  ID_ITEM_ADDRESOURCE              = 206;
  ID_ITEM_REMOVERESOURCE           = 207;
  //free to 209 for project menu items

  //execute menu
  ID_ITEM_COMP                     = 210;
  ID_ITEM_RUN                      = 211;
  ID_ITEM_COMPRUN                  = 212;
  ID_ITEM_REBUILD                  = 213;
  ID_ITEM_CLEAN                    = 214;
  ID_ITEM_RESULTS                  = 215;
  ID_ITEM_SYNTAXCHECK              = 216;
  ID_ITEM_PROGRAMRESET             = 217;
  ID_ITEM_PROFILE                  = 218;
  ID_ITEM_ABORTCOMP                = 219;
  //free to 219 for execute menu
  //more IDs at 2900

  //options menu removed
  //tools menu
  ID_ITEM_COMPOPTIONS              = 220;
  ID_ITEM_ENVIROOPTIONS            = 221;
  ID_ITEM_EDITOROPTIONS            = 222;
  ID_ITEM_TOOLCONFIG               = 223;
  ID_ITEM_SHORTCUTSCONFIG          = 224;
  // tool items dynamic
  //free to 229 for tool menu items

  //window menu
  ID_ITEM_FULLSCRMODE              = 230;
  ID_ITEM_FULLSCRBACK              = 231;
  ID_ITEM_NEXT                     = 232;
  ID_ITEM_PREV                     = 233;
  ID_ITEM_CLOSEALL                 = 234;
  ID_FULLSCREEN_MSG                = 235;
  ID_ITEM_CLOSEALLBUTTHIS          = 236;
  ID_ITEM_LIST                     = 237;     
  //free to 239 for window items

  //help menu
  ID_ITEM_ABOUT                    = 240;
  ID_ITEM_UPDATECHECK              = 241;
  ID_ITEM_CUSTOM                   = 242;
  ID_ITEM_HELPDEVCPP               = 243;     

  //rest dynamic from help.ini

  //debug menu
  ID_ITEM_DEBUG                    = 245; // moved from execute menu
  ID_ITEM_TOGGLEBREAK              = 246;
  ID_ITEM_STEPNEXT                 = 247;
  ID_ITEM_STEPOVER                 = 248;
  ID_ITEM_WATCHADD                 = 249;
  ID_ITEM_WATCHITEMS               = 250;
  ID_ITEM_STOPEXECUTION            = 251;
  ID_ITEM_WATCHEDIT                = 252;
  ID_ITEM_WATCHREMOVE              = 253;
  ID_ITEM_STEPINTO                 = 254;
  ID_ITEM_RUNTOCURSOR              = 255;
  ID_ITEM_CPUWINDOW                = 256;
  ID_ITEM_ATTACHPROCESS            = 257;
  ID_ITEM_MODIFYVALUE              = 258;
  ID_ITEM_CLEARALL                 = 259;

  //free to 259 for menu items

  // context menus
  // project popup
  ID_POP_NEWFILE                   = 260;
  ID_POP_ADDFILE                   = 261;
  ID_POP_REMOVEFILE                = 262;
  ID_POP_PRJOPTIONS                = 263;
  ID_POP_EDITRES                   = 264;
  //free to 269 for project popup

  //unit popup
  ID_POP_REMOVE                    = 270;
  ID_POP_RENAME                    = 271;
  ID_POP_CLOSE                     = 272;
  ID_POP_OPEN                      = 273;
  ID_POP_OPEN2                     = 274; // opens header if source
  //free to 279 for unit popup

  //editor popup
    // close file from unitpop
    // open header from unitpop
    // edit menu items from edit menu

  //toggle, goto, help created dynamically
  //new pulled from file-new sub menu

  //class browser popup
  ID_POP_GOTODECL                  = 280;
  ID_POP_GOTOIMPL                  = 281;
  ID_POP_NEWCLASS                  = 282;
  ID_POP_NEWMEMBER                 = 283;
  ID_POP_NEWVAR                    = 284;
  ID_POP_VIEWMODE                  = 285;
  ID_POP_VIEWALLFILES              = 286;
  ID_POP_VIEWCURRENT               = 287;
  ID_POP_ADDFOLDER                 = 288;
  ID_POP_REMOVEFOLDER              = 289;
  ID_POP_RENAMEFOLDER              = 290;
  ID_POP_MAXFOLDERS                = 291;
  ID_POP_USECOLORS                 = 292;
  ID_POP_VIEWPROJECT               = 293;
  ID_POP_SHOWINHERITED             = 294;
  ID_POP_OPENWITH                  = 295;
  // free to 295 for class browser popup

  // free to 299 for context menu items

  //message control
  //tab captions
  ID_SHEET_COMP                    = 300;
  ID_SHEET_RES                     = 301;
  ID_SHEET_COMPLOG                 = 302;
  ID_SHEET_FIND                    = 303;
  ID_SHEET_CLOSE                   = 304;
  ID_SHEET_DEBUG                   = 305;
  // free to 310 for new sheets

  // pop menu
  ID_SHEET_POP_CLEAR               = 310;
  ID_SHEET_POP_COPY                = 311;
  ID_SHEET_POP_HIDE                = 312;

  //column headers
  ID_COL_LINE                      = 315;
  ID_COL_FILE                      = 316;
  ID_COL_ARGS                      = 318;
  ID_COL_MSG                       = 317;
  ID_COL_FLINE                     = ID_COL_LINE;
  ID_COL_FCOL                      = 321;
  ID_COL_FFILE                     = ID_COL_FILE;
  ID_COL_FMSG                      = ID_COL_MSG;
  ID_COL_WATCHNAME                 = 322;
  ID_COL_WATCHVAL                  = 323;

  // may want to remap id_col_f* to own values
  // for true custom titles

  //compile log sheet
  ID_GRP_INFO                      = 324;
  ID_GRP_COMPRESULTS               = 325;
  ID_TOTALERRORS                   = 326;
  ID_OUTPUTSIZE                    = 327;
  //free to 329 for compile log sheet

  ID_DEB_WATCHES                   = 330;
  ID_DEB_BACKTRACE                 = 331;
  ID_DEB_OUTPUT                    = 332;
  ID_DEB_SENDGDBCOMMAND            = 333;
  ID_DEB_SEND                      = 334;
  //free to 339 for column titles

  //specials toolbar
  ID_TB_NEW                        = 340;
  ID_TB_INSERT                     = 341;
  ID_TB_TOGGLE                     = 342;
  ID_TB_GOTO                       = 343;
  ID_TB_PROJECT                    = 344;
  ID_TB_DEBUG                      = 345;
  ID_TB_REPORT                     = 346;
  //free to 349 for specials bar

  //non-visual controls (save/load dialogs etc...)
  //captions
  ID_NV_OPENFILE                   = 350;
  ID_NV_OPENADD                    = 351;
  ID_NV_EXPORT                     = 352;
  ID_NV_SAVEFILE                   = 353;
  ID_NV_SAVEPROJECT                = 354;
  ID_NV_HELPFILE                   = 355; // was 363, but it was ID 355 in .lng !!! - mandrav

  ID_NV_ADDWATCH                   = 357;
  ID_NV_ENTERVAR                   = 358;
  ID_NV_MODIFYVAR                  = 359;
  ID_NV_NEWVAR                     = 360;
  ID_NV_MODIFYVALUE                = 361;
  ID_NV_VARNAME                    = 362;
  ID_NV_VARVALUE                   = 363;

  //NOTE: need system for filters

  // Project Manager/Class Browser
  ID_LP_PROJECT                    = 370;
  ID_LP_CLASSES                    = 371;

  //free to 399 for main form items

{ -- end main form -- }


{ -- forms -- }

{  Editor options form }
    // caption
  ID_EOPT                          = 400;
    // Tab captions
  ID_EOPT_GENTAB                   = 401;
  ID_EOPT_DISPLAYTAB               = 402;
  ID_EOPT_SYNTAXTAB                = 403;
  ID_EOPT_CODETAB                  = 404;
  ID_EOPT_COMPLETIONTAB            = 405;
  ID_EOPT_BROWSERTAB               = 406;

    // sub-tab captions
  ID_EOPT_CPINSERTS                = 410;
  ID_EOPT_CPDEFAULT                = 411;

    // General Tab Items
  ID_EOPT_EDOPTIONS                = 420;
  ID_EOPT_AUTOINDENT               = 421;
  ID_EOPT_INSERTMODE               = 422;
  ID_EOPT_TAB2SPC                  = 423;
  ID_EOPT_SMARTTABS                = 424;
  ID_EOPT_SMARTUN                  = 425;
  ID_EOPT_TRAILBLANKS              = 426;
  ID_EOPT_GROUPUNDO                = 427;
  ID_EOPT_EHOMEKEY                 = 428;
  ID_EOPT_PASTEOF                  = 429;
  ID_EOPT_PASTEOL                  = 430;
  ID_EOPT_DBLCLKLINE               = 431;
  ID_EOPT_FINDTEXT                 = 432;
  ID_EOPT_SMARTSCROLL              = 433;
  ID_EOPT_HALFPAGE                 = 434;
  ID_EOPT_SCROLLHINT               = 435;
  ID_EOPT_USESYNTAX                = 436;
  ID_EOPT_DROPFILES                = 437;
  ID_EOPT_TABSIZE                  = 438;
  ID_EOPT_MARGIN                   = 439;
  ID_EOPT_VISIBLE                  = 440;
  ID_EOPT_WIDTH                    = 441;
  ID_EOPT_COLOR                    = 442;
  ID_EOPT_CARET                    = 443;
  ID_EOPT_INSCARET                 = 444;
  ID_EOPT_OVERCARET                = 445;
  ID_EOPT_CARET1                   = 446;
  ID_EOPT_CARET2                   = 447;
  ID_EOPT_CARET3                   = 448;
  ID_EOPT_CARET4                   = 449;
  ID_EOPT_SPECIALCHARS             = 451;
  ID_EOPT_PARSERHINTS              = 452;
  ID_EOPT_MATCH                    = 453;
  ID_EOPT_APPENDNEWLINE            = 454;
  ID_EOPT_HIGHLIGHTCURRLINE        = 455;
  ID_EOPT_HINTHIGHLIGHT            = 456;
  ID_EOPT_HINTWHITESPACE           = 457;


    // Display Tab
  ID_EOPT_EDFONT                   = 460;
  ID_EOPT_FONT                     = 461;
  ID_EOPT_SIZE                     = 462;
  ID_EOPT_EDITORPRE                = 463;
  ID_EOPT_GUTTER                   = 464;
  ID_EOPT_GUTTERAUTO               = 465;
  ID_EOPT_LINENUM                  = 466;
  ID_EOPT_LEADZERO                 = 467;
  ID_EOPT_FIRSTZERO                = 468;
  ID_EOPT_GUTTERFNT                = 469;
  ID_EOPT_GUTTERPRE                = 470;

    // Syntax tab
  ID_EOPT_FORE                     = 471;
  ID_EOPT_BACK                     = 472;
  ID_EOPT_STYLE                    = 473;
  ID_EOPT_BOLD                     = 474;
  ID_EOPT_ITALIC                   = 475;
  ID_EOPT_UNDERLINE                = 476;
  ID_EOPT_SPEED                    = 477;
  ID_EOPT_ELEMENTS                 = 478;
  ID_EOPT_SAVESYNTAX               = 479;
  ID_EOPT_SAVESYNTAXQUESTION       = 419;

    // Code Tab -- Inserts
  ID_EOPT_CICODE                   = 480;
  ID_EOPT_CIMENU                   = 481;
  ID_EOPT_CISECTION                = 482;
  ID_EOPT_CIDESC                   = 483;
  ID_EOPT_DEFCODEPRJ               = 484;

    // Completion Tab
  ID_EOPT_CCCACHECHECK             = 485;
  ID_EOPT_CCCACHELABEL             = 486;
  ID_EOPT_COMPLETIONENABLE         = 487;
  ID_EOPT_COMPLETIONDELAY          = 488;
  ID_EOPT_COMPLETIONCOLOR          = 489;

    // Class browsing Tab
  // 490 is empty now - you can use it ;)
  ID_EOPT_BROWSERLOCAL             = 491;
  ID_EOPT_BROWSERSAMPLE            = 492;
  ID_EOPT_BROWSERENGINE            = 493;
  ID_EOPT_BROWSERVIEW              = 494;
  ID_EOPT_BROWSERENABLE	      	   = 495;
  ID_EOPT_BROWSERGLOBAL            = 496;
  // free to 499 for editor options

  // new project
  ID_NP                            = 500;
  ID_NP_DESC                       = 501;
  ID_NP_PRJNAME                    = 503;
  ID_NP_DEFAULTC                   = 504;
  ID_NP_DEFAULTCPP                 = 505;
  ID_NP_MAKEDEFAULT                = 506;
  ID_NP_PRJSHEET                   = 507;
  ID_NP_PRJOPTIONS                 = 508;
  
  // free to 519 for new project form

  //remove unit
  ID_RU                            = 520;
  ID_RU_TEXT                       = 521;
  //free to 524 for remove form

  // Goto line form
  ID_GOTO_CAPTION                  = 525;
  ID_GOTO_TEXT                     = 526;
  //free to 529 for goto form

  // Tool config form
  ID_TF                            = 530;
  ID_TF_LABEL                      = 531;
  // free to 534 for tool config form

  // Tool edit form
  ID_TE                            = 535;
  ID_TE_TITLE                      = 536;
  ID_TE_PROG                       = 537;
  ID_TE_WORK                       = 538;
  ID_TE_PARAM                      = 539;
  ID_TE_INSERT                     = 540;
  ID_TE_AVAIL                      = 541;
  //free to 544 for tools edit form

  // Icon Lib Form
  ID_IF                            = 545;
  ID_IF_USEICO                     = 546;
  // free to 549 for icon library form

  // Configure Shortcuts component
  ID_SC_CAPTION                    = 550;
  ID_SC_TITLE                      = 551;
  ID_SC_TIP                        = 552;
  ID_SC_HDRENTRY                   = 553;
  ID_SC_HDRSHORTCUT                = 554;
  ID_SC_CANCEL                     = 555;
  ID_SC_OK                         = 556;

  // free to 559 for more forms.

  // Tool edit form macros
  ID_ET_MACROS                     = 560; // +9
  // free to 579 for macro desc (thats 19 max!)

  // About form
  ID_AB_CAPTION                    = 580;
  ID_AB_AUTHOR2                    = 581;
  ID_AB_LICENSE                    = 582;
  ID_AB_WEBCAP                     = 583;
  ID_AB_LBLBLOODSITE               = 584;
  ID_AB_LBLMINGWSITE               = 585;
  ID_AB_LBLFORUM                   = 586;
  ID_AB_LBLMAIL                    = 587;
  ID_AB_LBLEMAIL                   = 588;
  ID_AB_BLOODSITE                  = 589;
  ID_AB_MINGWSITE                  = 590;
  ID_AB_FORUMS                     = 591;
  ID_AB_MAILLIST                   = 592;
  ID_AB_AUTHOR                     = 593;
  ID_AB_UPDATE                     = 594;

  //Code Inserts Edit
  ID_CIE_EDCAPTION                 = 600;
  ID_CIE_ADDCAPTION                = 601;
  ID_CIE_MENU                      = 602;
  ID_CIE_DESC                      = 603;
  ID_CIE_SEC                       = 604;

  // Environment Options form
  ID_ENV_COMPPROGRESSWINDOW        = 606;
  ID_ENV_SHOWPROGRESS              = 607;
  ID_ENV_AUTOCLOSEPROGRESS         = 608;
  ID_ENV_NOSPLASH                  = 609;
  ID_ENV                           = 610;
  ID_ENV_GENTAB                    = 611;
  ID_ENV_PATHTAB                   = 612;
  ID_ENV_AUTOOPEN                  = 613;
  ID_ENV_DEFCPP                    = 614;
  ID_ENV_SHOWBARS                  = 615;
  ID_ENV_BACKUPS                   = 616;
  ID_ENV_MINONRUN                  = 617;
  ID_ENV_OPENSTYLE                 = 618;
  ID_ENV_OPEN2K                    = 619;
  ID_ENV_OPEN9X                    = 620;
  ID_ENV_OPEN31                    = 621;
  ID_ENV_LANGUAGE                  = 622;
  ID_ENV_MSGTABS                   = 623;
  ID_ENV_MRU                       = 624;
  ID_ENV_USERDIR                   = 625;
  ID_ENV_OUTPUTDIR                 = 626;
  ID_ENV_ICOLIB                    = 627;
  ID_ENV_SPLASH                    = 628;
  ID_ENV_SELUSERDIR                = 629;
  ID_ENV_SELOUTPUTDIR              = 630;
  ID_ENV_SELICOLIB                 = 631;
  ID_ENV_AUTOALL                   = 632;
  ID_ENV_AUTOFIRST                 = 633;
  ID_ENV_AUTONONE                  = 634;
  ID_ENV_SHOWMENU                  = 635;
  ID_ENV_DBLFILES                  = 636;
  ID_ENV_SELLANGDIR                = 637;
  ID_ENV_THEME                     = 638;
  ID_ENV_INTERFACETAB              = 639;
  // more at 2800

  // Find Form
  ID_FIND                          = 640;
  ID_FIND_FINDTAB                  = 641;
  ID_FIND_FINDALLTAB               = 642;
  ID_FIND_TEXT                     = 643;
  ID_FIND_GRP_OPTIONS              = 644;
  ID_FIND_CASE                     = 645;
  ID_FIND_WWORD                    = 646;
  ID_FIND_GRP_WHERE                = 647;
  ID_FIND_PRJFILES                 = 648;
  ID_FIND_OPENFILES                = 649;
  ID_FIND_GRP_SCOPE                = 650;
  ID_FIND_GLOBAL                   = 651;
  ID_FIND_SELONLY                  = 652;
  ID_FIND_GRP_ORIGIN               = 653;
  ID_FIND_CURSOR                   = 654;
  ID_FIND_ENTIRE                   = 655;
  ID_FIND_GRP_DIRECTION            = 656;
  ID_FIND_FORE                     = 657;
  ID_FIND_BACK                     = 658;

  // Replace form
  ID_RPLC                          = 670;
  ID_RPLC_FINDTEXT                 = 671;
  ID_RPLC_REPLACETEXT              = 672;
  ID_RPLC_GRP_OPTIONS              = 673;
  ID_RPLC_CASE                     = 674;
  ID_RPLC_WHOLEWORD                = 675;
  ID_RPLC_PROMPT                   = 676;
  ID_RPLC_GRP_DIRECTION            = 677;
  ID_RPLC_FORWARD                  = 678;
  ID_RPLC_BACKWARD                 = 679;
  ID_RPLC_GRP_SCOPE                = 680;
  ID_RPLC_GLOBAL                   = 681;
  ID_RPLC_SELONLY                  = 682;
  ID_RPLC_GRP_ORIGIN               = 683;
  ID_RPLC_CURSOR                   = 684;
  ID_RPLC_ENTIRE                   = 685;

  // Goto function form
  ID_GF_TEXT                       = 695;
  ID_GF_TYPE                       = 696;
  ID_GF_FUNCTION                   = 697;
  ID_GF_LINE                       = 698;

  // compiler options form
  ID_COPT                          = 700;
  ID_COPT_COMPTAB                  = 701;
  ID_COPT_DIRTAB                   = 702;
  ID_COPT_CODEGENTAB               = 703;
  ID_COPT_LINKERTAB                = 704;
  ID_COPT_PROGRAMSTAB              = 705;
  // free to 709 for new tabs

  ID_COPT_BIN                      = 710;
  ID_COPT_LIB                      = 711;
  ID_COPT_INCC                     = 712;
  ID_COPT_INCCPP                   = 713;

  // compiler tab
  ID_COPT_GRP_C                    = 720;
  ID_COPT_GRP_CPP                  = 721;
  ID_COPT_GRP_COMP                 = 722;
  // free to 725 for new groups
  ID_COPT_ANSIC                    = 725;
  ID_COPT_TRADITIONAL              = 726;
  ID_COPT_WARNING                  = 727;
  ID_COPT_ACCESS                   = 728;
  ID_COPT_DOLLAR                   = 729;
  ID_COPT_HEURISTICS               = 730;
// not used anymore  ID_COPT_CYGWIN = 731;
  ID_COPT_ADDCOMP                  = 732;
  ID_COPT_DELAY                    = 733;
  ID_COPT_DELAYMSG                 = 734;
  ID_COPT_ERRORLINE                = 735;     

  //CodeGen Tab
  ID_COPT_GRP_CODEGEN              = 740;
  ID_COPT_GRP_OPTIMIZE             = 741;
  ID_COPT_EXCEPT                   = 745;
  ID_COPT_DBLFLOAT                 = 746;
  ID_COPT_MEM                      = 747;
  ID_COPT_OPT1                     = 748;
  ID_COPT_OPTBEST                  = 749;
  ID_COPT_OPTMINOR                 = 750;
  ID_COPT_MAKEFILEGEN              = 751;
  ID_COPT_FASTDEP                  = 752;
  ID_COPT_PROFILING                = 753;
  ID_COPT_PROFILE                  = 754;

  //Linker Tab
  ID_COPT_OBJC                     = 755;
  ID_COPT_DEBUG                    = 756;
  ID_COPT_NOLIBS                   = 757;
  ID_COPT_WIN32                    = 758;
  ID_COPT_LINKADD                  = 759;

  //CodeGen Tab (again!)
  ID_COPT_NOOPT                    = 760;
  ID_COPT_OPTMORE                  = 761;

  // Programs Tab
  ID_COPT_PROGRAMS                 = 770;
  ID_COPT_COMPSETS                 = 771;
  ID_COPT_NEWCOMPSET               = 772;
  ID_COPT_PROMPTNEWCOMPSET         = 773;
  ID_COPT_CANTDELETECOMPSET        = 774;
  ID_COPT_DELETECOMPSET            = 775;
  ID_COPT_RENAMECOMPSET            = 776;
  ID_COPT_PROMPTRENAMECOMPSET      = 777;
  ID_COPT_FURTHEROPTS              = 778;

  // free to 780 for compiler options

  // end compiler options

  // Help Menu Editor Form
  ID_HE                            = 780;
  ID_HE_COL1                       = 781;
  ID_HE_COL2                       = 782;
  ID_HE_GRP_OPTIONS                = 783;
  ID_HE_ICON                       = 784;
  ID_HE_GRP_MENU                   = 785;
  ID_HE_SEC1                       = 786;
  ID_HE_SEC2                       = 787;
  ID_HE_ONPOP                      = 788;
  ID_HE_GRP_ACTIONS                = 789;
  ID_HE_SEARCHWORD                 = 790;
  ID_HE_AFFECTF1                   = 791;


  // Print form
  ID_PRT                           = 800;
  ID_PRT_GRP_PARAMS                = 801;
  ID_PRT_GRP_PAGES                 = 802;
  ID_PRT_COLORS                    = 803;
  ID_PRT_HIGHLIGHT                 = 804;
  ID_PRT_WORDWRAP                  = 805;
  ID_PRT_LINENUM                   = 806;
  ID_PRT_PRTLINENUM                = 807;
  ID_PRT_PRTLINENUMMAR             = 808;
  ID_PRT_COPIES                    = 809;
  ID_PRT_SELONLY                   = 810;

  //Check form
  ID_UCF                           = 820;
  ID_UCF_GRP_TASK                  = 821;
  ID_UCF_GRP_RESULTS               = 822;
  ID_UCF_RELEASE                   = 823;
  ID_UCF_NEED                      = 824;
  ID_UCF_DESC                      = 825;
  ID_UCF_SITES                     = 826;

  //AddToDo form
  ID_ADDTODO_MENUITEM              = 830; // menu
  ID_ADDTODO_DESCRIPTION           = 831;
  ID_ADDTODO_PRIORITY              = 832;
  ID_ADDTODO_USER                  = 833;

  //ViewToDo form
  ID_VIEWTODO_MENUITEM             = 835; // menu
  ID_VIEWTODO_FILENAME             = 836;
  ID_VIEWTODO_DONE                 = 837;
  ID_VIEWTODO_NOSHOWDONE           = 838;
  ID_VIEWTODO_FILTER               = 839;
  ID_VIEWTODO_FILTERONE            = 840;
  ID_VIEWTODO_FILTERTWO            = 841;
  ID_VIEWTODO_FILTERTHREE          = 842;
  ID_VIEWTODO_FILTERFOUR           = 843;
  ID_VIEWTODO_FILTERFIVE           = 844;
  ID_VIEWTODO_FILTERSIX            = 845;

  //Import MSVC++ form
  ID_MSVC_MENUITEM                 = 850; // menu
  ID_MSVC_SELECTMSVC               = 851;
  ID_MSVC_OPTIONS                  = 852;
  ID_MSVC_CONFIGURATION            = 853;
  ID_MSVC_SELECTDEV                = 854;
  ID_MSVC_MSG_SUCCESS              = 855;
  ID_MSVC_MSG_CANTLOCATETARGET     = 856;

  //Project Options
  ID_POPT                          = 900;
  ID_POPT_GENTAB                   = 901;
  ID_POPT_DIRTAB                   = 902;
  ID_POPT_OUTTAB                   = 922;
  ID_POPT_MAKTAB                   = 989;
  ID_POPT_COMPTAB                  = 992;
  //general tab
  ID_POPT_PRJNAME                  = 903;
  ID_POPT_GRP_ICON                 = 904;
  ID_POPT_ICOLIB                   = 905;
  ID_POPT_ICOBRWSE                 = 906;
  ID_POPT_GRP_TYPE                 = 907;
  ID_POPT_TYPE1                    = 908;
  ID_POPT_TYPE2                    = 909;
  ID_POPT_TYPE3                    = 910;
  ID_POPT_TYPE4                    = 911;
  ID_POPT_USEGPP                   = 912;
  ID_POPT_ADDITIONAL               = 913;
  ID_POPT_COMP                     = 914;
  ID_POPT_LINKER                   = 915;
  ID_POPT_LIBDIRS                  = 916;
  ID_POPT_RESDIRS                  = 921;
  ID_POPT_INCDIRS                  = 918;
  ID_POPT_OPENICO                  = 919;
  ID_POPT_OPENOBJ                  = 920;
  ID_POPT_RESFILES                 = 917;
  // build output tab
  ID_POPT_OUTDIRGRP                = 923;
  ID_POPT_EXEOUT                   = 924;
  ID_POPT_OBJOUT                   = 925;
  ID_POPT_OVERRIDEOUT              = 926;
  ID_POPT_ADDLIBRARY               = 927;
  ID_POPT_UNITSFORMAT              = 928;

  //New template form
  ID_NEWTPL_NAME                   = 930;
  ID_NEWTPL_DESCRIPTION            = 931;
  ID_NEWTPL_CATEGORY               = 932;
  ID_NEWTPL_PROJECTNAME            = 933;
  ID_NEWTPL_FILES                  = 934;
  ID_NEWTPL_COMPILER               = 935;
  ID_NEWTPL_LINKER                 = 936;
  ID_NEWTPL_ICONS                  = 937;
  ID_NEWTPL_PAGETEMPLATE           = 938;
  ID_NEWTPL_PAGEFILES              = 939;
  ID_NEWTPL_PAGEEXTRAS             = 940;
  ID_NEWTPL_TEMPLATEICON           = 941;
  ID_NEWTPL_PROJECTICON            = 942;
  ID_NEWTPL_CREATE                 = 943;
  ID_NEWTPL_CAPTION                = 944;
  ID_NEWTPL_INCDIR                 = 945;
  ID_NEWTPL_LIBDIR                 = 946;
  ID_NEWTPL_RESDIR                 = 947;

  //New variable form
  ID_NEWVAR_VARTYPE                = 950;
  ID_NEWVAR_VARNAME                = 951;
  ID_NEWVAR_IMPLIN                 = 952;
  ID_NEWVAR_SCOPE                  = 953;
  ID_NEWVAR_CREATEREADFUNC         = 954;
  ID_NEWVAR_CREATEWRITEFUNC        = 955;
  ID_NEWVAR_FUNCNAME               = 956;
  ID_NEWVAR_COMMENTS               = 957;
  ID_NEWVAR_COMMENTSDESCR          = 958;
  ID_NEWVAR_COMMENTSSTYLE          = 959;
  ID_NEWVAR_BTN_CREATE             = 960;
  ID_NEWVAR_BTN_CANCEL             = 961;
  ID_NEWVAR_MSG_NOCLASS            = 962;
  ID_NEWVAR_MSG_NOIMPL             = 963;
  ID_NEWVAR_MSG_NOLINE             = 964;

  //New member form
  ID_NEWMEMB_MEMBNAME              = 965;
  ID_NEWMEMB_ARGS                  = 966;

  //New class form
  ID_NEWCLASS_NAME                 = 967;
  ID_NEWCLASS_INHERIT              = 968;
  ID_NEWCLASS_INHERITANCE          = 969;
  ID_NEWCLASS_INHERIT_FROM         = 970;
  ID_NEWCLASS_INHERIT_HDR          = 971;
  ID_NEWCLASS_CPPFILE              = 972;
  ID_NEWCLASS_HFILE                = 973;
  ID_NEWCLASS_ADDTOPROJ            = 974;

  //New member form
  ID_NEWMEMB_ATTRS                 = 975;
  // free to 979

  //File properties form
  ID_PROPS_FILENAME                = 980;
  ID_PROPS_INPROJECT               = 981;
  ID_PROPS_TOTAL                   = 982;
  ID_PROPS_CODE                    = 983;
  ID_PROPS_COMMENTS                = 984;
  ID_PROPS_INCLUDES                = 985;
  ID_PROPS_EMPTY                   = 986;
  ID_PROPS_FILESIZE                = 987;
  ID_PROPS_ABSOLUTE                = 988;
  // 989 is used elsewhere >:
  ID_PROPS_RELATIVE                = 990;
  ID_PROPS_TIMESTAMP               = 991;
  // 992 is used elsewhere >:

  //Profile analysis form
  ID_PROF_CAPTION                  = 995;
  ID_PROF_TABFLAT                  = 996;
  ID_PROF_TABGRAPH                 = 997;

{ -- global strings  -- }
  //messages and prompts - 1000+

  ID_MSG_TEXTNOTFOUND              = 1000; // %s
  ID_MSG_COULDNOTOPENFILE          = 1001; // %s
  ID_MSG_SEARCHREPLACEPROMPT       = 1002; // %s
  ID_MSG_FILEINPROJECT             = 1003; // %s %s
  ID_MSG_CLOSEPROJECTPROMPT        = 1004; // %s
  ID_MSG_ASKSAVECLOSE              = 1005; // %s
  ID_MSG_CLOSECREATEPROJECT        = 1006; // %s
  ID_MSG_FILERENAME                = 1007;
  ID_MSG_ALREADYCOMP               = 1008;
  ID_MSG_SAVEPROJECT               = 1009; // %s
  ID_MSG_FILEEXISTS                = 1010;
  ID_MSG_NEWFILE                   = 1011;
  ID_MSG_NEWRES                    = 1012;
  ID_MSG_NODEBUGSYMBOLS            = 1013;
  ID_MSG_NOPROFILE                 = 1014;
  ID_MSG_NORUNPROFILE              = 1015;
  ID_MSG_ADDBROWSERFOLDER          = 1016;
  ID_MSG_REMOVEBROWSERFOLDER       = 1017;
  ID_MSG_RENAMEBROWSERFOLDER       = 1018;
  ID_MSG_FILEISREADONLY            = 1019;
  ID_MSG_FILEREADONLYERROR         = 1020;
  ID_MSG_SEGFAULT                  = 1021;
  ID_MSG_SOURCEMORERECENT          = 1022;
  ID_MSG_ATTACH                    = 1023;

  //errors 1500+
  ID_ERR_OPENFILE                  = 1500; // %s
  ID_ERR_SAVEFILE                  = 1501; // %s
  ID_ERR_RENAMEFILE                = 1502; // %s

  ID_ERR_SEARCHCANNOTBEEMPTY       = 1503;
  ID_ERR_NOTEXECUTABLE             = 1504;
  ID_ERR_PROJECTNOTCOMPILED        = 1505;
  ID_ERR_SRCNOTCOMPILED            = 1506;
  ID_ERR_NOMAKEFILE                = 1507;
  ID_ERR_CLEANFAILED               = 1508;
  ID_ERR_TEMPLATE                  = 1509;
  ID_ERR_RESOURCE                  = 1510; // %s %s %s
  ID_ERR_TEMPFNF                   = 1511; // %s
  ID_ERR_NOTEMPLATE                = 1512;
  ID_ERR_FILENOTFOUND              = 1513; // %s
  ID_ERR_HOSTMISSING               = 1514;
  ID_ERR_HOSTNOTEXIST              = 1515;

  //other 2000+
  ID_UNTITLED                      = 2000;
  ID_READONLY                      = 2001;
  ID_INSERT                        = 2002;
  ID_OVERWRITE                     = 2003;
  ID_NEWPROJECT                    = 2004; // %d
  ID_SPECIFY                       = 2005;
  ID_LINECOUNT                     = 2006; // %d
  ID_RENAME                        = 2007;
  ID_EXECUTING                     = 2008; // %s
  ID_EXECUTIONTERM                 = 2009;
  ID_BYTES                         = 2010;
  ID_MODIFIED                      = 2011;
  ID_ERROR                         = 2012;
  ID_INFO                          = 2013;
  ID_WARN                          = 2014;
  ID_QUESTION                      = 2015;
  ID_COMPILESUCCESS                = 2016;
  ID_COMPILEABORT                  = 2017;

{ -- Button Captions -- }
  ID_BTN_OK                        = 2500;
  ID_BTN_CANCEL                    = 2501;
  ID_BTN_HELP                      = 2502;
  ID_BTN_DEFAULT                   = 2503;
  ID_BTN_YES                       = 2504;
  ID_BTN_NO                        = 2505;
  ID_BTN_ADD                       = 2506;
  ID_BTN_EDIT                      = 2507;
  ID_BTN_REMOVE                    = 2508;
  ID_BTN_DELETE                    = 2509;
  ID_BTN_CLOSE                     = 2510;
  ID_BTN_AUTHOR                    = 2511;
  ID_BTN_FIND                      = 2512;
  ID_BTN_REPLACE                   = 2513;
  ID_BTN_DELINVAL                  = 2514;
  ID_BTN_REPLACEALL                = 2515;
  ID_BTN_BROWSE                    = 2516;
  ID_BTN_REMOVEICON                = 2517;
  ID_BTN_RENAME                    = 2518;
  ID_BTN_IMPORT                    = 2519;
  ID_BTN_FLOATDEBUG                = 2520;
  ID_BTN_CLEAR                     = 2521; 
  // free to 2600

{ -- end buttons -- }

{ CPU Window }
  ID_CPU_CAPTION                   = 2600;
  ID_CPU_ASMCODE                   = 2601;
  ID_CPU_FUNC                      = 2602;
  ID_CPU_SYNTAX                    = 2603;
  ID_CPU_REGISTERS                 = 2604;               

{ Tip Of The Day }
  ID_TIPS_CAPTION                  = 2700;
  ID_TIPS_DIDYOUKNOW               = 2701;
  ID_TIPS_NOTIPSTODISPLAY          = 2702;
  ID_TIPS_DONTSHOWTIPS             = 2703;
  ID_TIPS_NEXTTIP                  = 2704;
  ID_TIPS_PREVIOUSTIP              = 2705;

{ Environment Options }
  ID_ENV_FASSTAB                   = 2800;
  ID_ENV_FASSTYPES                 = 2801;
  ID_ENV_FASSDESC                  = 2802;
  ID_ENV_CVSTAB                    = 2803;
  ID_ENV_CVSEXE                    = 2804;
  ID_ENV_CVSCOMPR                  = 2805;
  ID_ENV_CVSUSESSH                 = 2806;
  ID_ENV_TEMPLATESDIR              = 2807;
  ID_ENV_SELTEMPLATESDIR           = 2808;
  ID_ENV_XPTHEME                   = 2809;
  ID_ENV_EXTERNALS                 = 2810;
  ID_ENV_EXTERNPROG                = 2811;
  ID_ENV_EXTERNEXT                 = 2812;
  ID_ENV_EXTERNPROGASSOCS          = 2813;
  ID_ENV_WATCHHINT                 = 2814;
  ID_ENV_WATCHERROR                = 2815;
  ID_ENV_DEBUGGER                  = 2816;
  ID_ENV_GBALTCONFIG               = 2817;
  ID_ENV_USEALTCONFIG              = 2818;
  ID_ENV_CONFIGCHANGED             = 2819;

{ Execute menu }
  ID_ITEM_EXECPARAMS               = 2900;
  ID_ITEM_COMPCURRENT              = 2901;

{ Window list form }
  ID_WL                            = 2910;
  ID_WL_SELECT                     = 2911;

{ CVS Menu }
  ID_ITEM_CVSCURRENT               = 2990;
  ID_ITEM_CVSWHOLE                 = 2991;
  ID_MSG_CVSNOTCONFIGED            = 2992;

{ CVS form }
  ID_CVS_IMPORTTAB                 = 3000;
  ID_CVS_REPOSITORYTAB             = 3001;
  ID_CVS_GLOBALTAB                 = 3002;
  ID_CVS_CHECKOUTTAB               = 3003;
  ID_CVS_COMMITTAB                 = 3004;
  ID_CVS_UPDATETAB                 = 3005;
  ID_CVS_DIFFTAB                   = 3006;
  ID_CVS_LOGTAB                    = 3007;
  ID_CVS_OUTPUTTAB                 = 3008;
  ID_CVS_IMPORT                    = 3009;
  ID_CVS_CHECKOUT                  = 3010;
  ID_CVS_COMMIT                    = 3011;
  ID_CVS_UPDATE                    = 3012;
  ID_CVS_DIFF                      = 3013;
  ID_CVS_LOG                       = 3014;
  ID_CVS_IMPDIR                    = 3015;
  ID_CVS_IMPACTION                 = 3016;
  ID_CVS_IMPVENDOR                 = 3017;
  ID_CVS_IMPRELEASE                = 3018;
  ID_CVS_LOGMSG                    = 3019;
  ID_CVS_MODULE                    = 3020;
  ID_CVS_IMPEXT                    = 3021;
  ID_CVS_COAS                      = 3022;
  ID_CVS_CODIR                     = 3023;
  ID_CVS_RECURSE                   = 3024;
  ID_CVS_RESETSTICKY               = 3025;
  ID_CVS_CREATEDIRS                = 3026;
  ID_CVS_PRUNEEMPTY                = 3027;
  ID_CVS_GETCLEAN                  = 3028;
  ID_CVS_BEFOREDATE                = 3029;
  ID_CVS_REVISION                  = 3030;
  ID_CVS_GETMOSTRECENT             = 3031;
  ID_CVS_OTHERREVISIONS            = 3032;
  ID_CVS_DIFFUNIFIED               = 3033;
  ID_CVS_DIFF1                     = 3034;
  ID_CVS_DIFF2                     = 3035;
  ID_CVS_DIFF3                     = 3036;
  ID_CVS_ISDATE                    = 3037;
  ID_CVS_LOGDEFBRANCH              = 3038;
  ID_CVS_LOGRCS                    = 3039;
  ID_CVS_LOGNOTAG                  = 3040;
  ID_CVS_LOGFILTER                 = 3041;
  ID_CVS_LOGBYREV                  = 3042;
  ID_CVS_LOGBYDATE                 = 3043;
  ID_CVS_LOGBYUSER                 = 3044;
  ID_CVS_REPOSITORY                = 3045;
  ID_CVS_REPDETAILS                = 3046;
  ID_CVS_REPMETHOD                 = 3047;
  ID_CVS_REPUSER                   = 3048;
  ID_CVS_REPSERVER                 = 3049;
  ID_CVS_REPDIR                    = 3050;
  ID_CVS_ADD                       = 3051;
  ID_CVS_REMOVE                    = 3052;
  ID_CVS_ADDTAB                    = 3053;
  ID_CVS_REMOVETAB                 = 3054;
  ID_CVS_REMOVEFILE                = 3055;

{ Parameters form }
  ID_PARAM_CAPTION                 = 3100;
  ID_PARAM_PARAMS                  = 3101;
  ID_PARAM_HOST                    = 3102;

{ Project options form (continued) }
  ID_POPT_FILESTAB                 = 3200;
  ID_POPT_PROJFILES                = 3201;
  ID_POPT_PROJOPTS                 = 3202;
  ID_POPT_DEBUG                    = 3203;
  ID_POPT_PROFILE                  = 3204;
  ID_POPT_COMPINFO                 = 3205;
  ID_POPT_SUPPORTXP                = 3206;
  ID_POPT_UNITOPTS                 = 3207;
  ID_POPT_COMPUNIT                 = 3208;
  ID_POPT_UNITUSEGPP               = 3209;
  ID_POPT_VERTAB                   = 3210;
  ID_POPT_INCLUDEVERSION           = 3211;
  ID_POPT_VDETAILS                 = 3212;
  ID_POPT_VMAJOR                   = 3213;
  ID_POPT_VMINOR                   = 3214;
  ID_POPT_VRELEASE                 = 3215;
  ID_POPT_VBUILD                   = 3216;
  ID_POPT_VLANG                    = 3217;
  ID_POPT_VADDITIONAL              = 3218;
  ID_POPT_OVERRIDEBUILDCMD         = 3219;
  ID_POPT_OUTPUTFILENAME           = 3220;
  ID_POPT_LINKUNIT                 = 3221;
  ID_POPT_BUILDPRIORITY            = 3222;
  ID_POPT_VAUTOINCBUILDNR          = 3223;
  ID_POPT_USECUSTOMMAKEFILE        = 3224;
  ID_POPT_INFOCUSTOMMAKEFILE       = 3225;
  ID_POPT_INCFILEMAKEFILE          = 3226;
  // free to 3400

  // New compilter options
  ID_COPT_STRIP                    = 3400;
  ID_COPT_ARCH                     = 3401;
  ID_COPT_BUILTINPROC              = 3402;     
