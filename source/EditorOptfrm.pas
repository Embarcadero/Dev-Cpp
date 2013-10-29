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

unit EditorOptFrm;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, Spin,
  SynEdit, SynEditHighlighter, SynHighlighterCpp,
  Buttons, ClassBrowser, CppParser, CppTokenizer, StrUtils, Grids;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Variants, Classes, QGraphics, QControls, QForms,
  QDialogs, QComCtrls, QStdCtrls, QExtCtrls, ColorPickerButton,
  QSynEdit, QSynEditHighlighter, QSynHighlighterCpp, QCheckLst,
  QButtons, ClassBrowser, CppParser, CppTokenizer, StrUtils, Types;
{$ENDIF}

type
  TEditorOptForm = class(TForm)
    PagesMain: TPageControl;
    tabDisplay: TTabSheet;
    grpGutter: TGroupBox;
    cbGutterVis: TCheckBox;
    cbGutterAuto: TCheckBox;
    cbLineNum: TCheckBox;
    cbFirstZero: TCheckBox;
    cbLeadZero: TCheckBox;
    cbGutterFnt: TCheckBox;
    lblGutterFont: TLabel;
    cboGutterFont: TComboBox;
    lblGutterWidth: TLabel;
    lblGutterFontSize: TLabel;
    edGutterSize: TSpinEdit;
    tabGeneral: TTabSheet;
    tabSyntax: TTabSheet;
    lblForeground: TLabel;
    lblBackground: TLabel;
    CppEdit: TSynEdit;
    ElementList: TListBox;
    grpStyle: TGroupBox;
    cbBold: TCheckBox;
    cbItalic: TCheckBox;
    cbUnderlined: TCheckBox;
    cpp: TSynCppSyn;
    grpEditorFont: TGroupBox;
    lblEditorSize: TLabel;
    lblEditorFont: TLabel;
    cboEditorFont: TComboBox;
    edEditorSize: TSpinEdit;
    grpMargin: TGroupBox;
    lblMarginWidth: TLabel;
    lblMarginColor: TLabel;
    cbMarginVis: TCheckBox;
    grpCaret: TGroupBox;
    lblInsertCaret: TLabel;
    lblOverCaret: TLabel;
    cboInsertCaret: TComboBox;
    cboOverwriteCaret: TComboBox;
    tabCode: TTabSheet;
    codepages: TPageControl;
    tabCPInserts: TTabSheet;
    tabCPDefault: TTabSheet;
    btnAdd: TButton;
    btnRemove: TButton;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    cboQuickColor: TComboBox;
    lblSpeed: TLabel;
    CodeIns: TSynEdit;
    btnSaveSyntax: TSpeedButton;
    tabCBCompletion: TTabSheet;
    lblCompletionDelay: TLabel;
    lblCompletionColor: TLabel;
    tbCompletionDelay: TTrackBar;
    chkEnableCompletion: TCheckBox;
    chkCCCache: TCheckBox;
    btnCCCadd: TButton;
    btnCCCdelete: TButton;
    CppParser: TCppParser;
    lbCCC: TListBox;
    pbCCCache: TProgressBar;
    cbMatch: TCheckBox;
    grpEditorOpts: TGroupBox;
    edMarginWidth: TSpinEdit;
    edGutterWidth: TSpinEdit;
    cbHighCurrLine: TCheckBox;
    cbSpecialChars: TCheckBox;
    cbSmartScroll: TCheckBox;
    cbScrollHint: TCheckBox;
    cbPastEOL: TCheckBox;
    cbPastEOF: TCheckBox;
    cbParserHints: TCheckBox;
    cbInsertMode: TCheckBox;
    cbHalfPage: TCheckBox;
    cbGroupUndo: TCheckBox;
    cbFindText: TCheckBox;
    cbEHomeKey: TCheckBox;
    cbDropFiles: TCheckBox;
    cbAddIndent: TCheckBox;
    cbAutoIndent: TCheckBox;
    cbTrimTrailingSpaces: TCheckBox;
    ScrollHint: TLabel;
    tabAutosave: TTabSheet;
    cbAutoSave: TCheckBox;
    OptionsGroup: TGroupBox;
    SaveInterval: TLabel;
    MinutesDelay: TTrackBar;
    FileOptions: TRadioGroup;
    grpHighCurLine: TGroupBox;
    tabSymbols: TTabSheet;
    cbBraces: TCheckBox;
    cbParenth: TCheckBox;
    cbInclude: TCheckBox;
    cbComments: TCheckBox;
    cbArray: TCheckBox;
    cbFunctionHint: TCheckBox;
    grpSpecific: TGroupBox;
    cbSymbolComplete: TCheckBox;
    edSyntaxExt: TEdit;
    cbSyntaxHighlight: TCheckBox;
    grpTabs: TGroupBox;
    seTabSize: TSpinEdit;
    lblTabSize: TLabel;
    cbUseTabs: TCheckBox;
    cbSmartTabs: TCheckBox;
    cbHighlightColor: TLabel;
    cbDefaultCode: TCheckBox;
    seDefault: TSynEdit;
    NameOptions: TRadioGroup;
    lvCodeIns: TStringGrid;
    gbCBEngine: TGroupBox;
    chkCBParseGlobalH: TCheckBox;
    chkCBParseLocalH: TCheckBox;
    lblTimeStampExample: TLabel;
    btnCCCrefresh: TButton;
    lblRefreshHint: TLabel;
    cpMarginColor: TColorBox;
    cpHighColor: TColorBox;
    cpForeground: TColorBox;
    cpBackground: TColorBox;
    cpCompletionBackground: TColorBox;
    edIncludeFile: TEdit;
    btnFileBrowse: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure SetGutter;
    procedure ElementListClick(Sender: TObject);
    procedure cppEditStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure StyleChange(Sender: TObject);
    procedure cbLineNumClick(Sender: TObject);
    procedure cbSyntaxHighlightClick(Sender: TObject);
    procedure cbGutterFntClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure CodeInsStatusChange(Sender: TObject;Changes: TSynStatusChanges);
    procedure cboQuickColorSelect(Sender: TObject);
    procedure CppEditSpecialLineColors(Sender: TObject; Line: Integer;var Special: Boolean; var FG, BG: TColor);
    procedure tbCompletionDelayChange(Sender: TObject);
    procedure chkEnableCompletionClick(Sender: TObject);
    procedure btnSaveSyntaxClick(Sender: TObject);
    procedure btnCCCaddClick(Sender: TObject);
    procedure btnCCCdeleteClick(Sender: TObject);
    procedure chkCCCacheClick(Sender: TObject);
    procedure CppParser1StartParsing(Sender: TObject);
    procedure CppParser1EndParsing(Sender: TObject);
    procedure CppParser1TotalProgress(Sender: TObject; const FileName: string;Total, Current: Integer);
    procedure OnGutterClick(Sender: TObject; Button: TMouseButton; X, Y,Line: Integer; Mark: TSynEditMark);
    procedure cbHighCurrLineClick(Sender: TObject);
    procedure cbAutoSaveClick(Sender: TObject);
    procedure MinutesDelayChange(Sender: TObject);
    procedure cbSymbolCompleteClick(Sender: TObject);
    procedure cboEditorFontDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure cboGutterFontDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure edEditorSizeChange(Sender: TObject);
    procedure edGutterSizeChange(Sender: TObject);
    procedure cboEditorFontChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lvCodeInsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure PagesMainChange(Sender: TObject);
    procedure NameOptionsClick(Sender: TObject);
    procedure btnCCCrefreshClick(Sender: TObject);
    procedure btnFileBrowseClick(Sender: TObject);
    procedure edIncludeFileKeyPress(Sender: TObject; var Key: Char);
    procedure edIncludeFileChange(Sender: TObject);
    procedure edIncludeFileClick(Sender: TObject);
  private
    ffgColor: TColor;
    fbgColor: TColor;
    fUpdate: boolean;
    fGutColor: TPoint;
    fBPColor: TPoint;
    fErrColor: TPoint;
    fABPColor: TPoint;
    fSelColor: TPoint;
    fFoldColor : TPoint;
    HasProgressStarted : boolean;
    procedure LoadFonts;
    procedure LoadText;
    procedure LoadCodeIns;
    procedure SaveCodeIns;
    procedure ClearCodeIns;
    procedure GetOptions;
    procedure UpdateCIButtons;
    procedure LoadSyntax(const Value: AnsiString);
    procedure FillSyntaxSets;
    procedure FillCCC;
  end;

implementation

uses 
{$IFDEF WIN32}
  shlobj, MultiLangSupport, devcfg, version, utils, math, CommCtrl, DateUtils, CodeInsList, datamod, IniFiles, editor,
  main;
{$ENDIF}
{$IFDEF LINUX}
  Xlib, MultiLangSupport, devcfg, version, utils, CodeIns, datamod, IniFiles, editor,
  main;
{$ENDIF}

{$R *.dfm}
const
 cBreakLine  = 7;
 cABreakLine = 9;
 cErrorLine  = 11;
 cSelection  = 15;

{ ---------- Form Events ---------- }

procedure TEditorOptForm.FormCreate(Sender: TObject);
begin
	LoadText;

	// Make editors look similar to main ones
	CppEdit.Font.Assign(devEditor.Font);
	CodeIns.Font.Assign(devEditor.Font);
	seDefault.Font.Assign(devEditor.Font);

	CppEdit.Gutter.Font.Assign(devEditor.Gutterfont);
	CodeIns.Gutter.Font.Assign(devEditor.Gutterfont);
	seDefault.Gutter.Font.Assign(devEditor.Gutterfont);

	// Code snippets
	LoadCodeIns;

	// Font dropdown filling
	LoadFonts;

	// Load color themes
	FillSyntaxSets;

	// Read ini file
	GetOptions;

	// Update code insertion
	UpdateCIButtons;

	// Set defaults of color buttons, don't want all system colors too
	cpMarginColor.Items.InsertObject(1,'Default',TObject(cpMarginColor.DefaultColorColor));
	cpHighColor.Items.InsertObject(1,'Default',TObject(cpHighColor.DefaultColorColor));
	cpCompletionBackground.Items.InsertObject(1,'Default',TObject(cpCompletionBackground.DefaultColorColor));
end;

procedure TEditorOptForm.cboEditorFontDrawItem(Control: TWinControl;Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
	alignleft : integer;
	aligntop : integer;
begin
	with TComboBox(Control) do begin
		Canvas.Font.Name := Items.Strings[Index];
		Canvas.Font.Size := edEditorSize.Value;
		Canvas.FillRect(Rect);
		alignleft := (Rect.Right - Rect.Left) div 2 - Canvas.TextWidth(Canvas.Font.Name) div 2;
		aligntop  := Rect.Top + (Rect.Bottom - Rect.Top) div 2 - Canvas.TextHeight(Canvas.Font.Name) div 2;
		Canvas.TextOut(alignleft, aligntop,Canvas.Font.Name);
	end;
end;

procedure TEditorOptForm.cboGutterFontDrawItem(Control: TWinControl;Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
	alignleft : integer;
	aligntop : integer;
begin
	with TComboBox(Control) do begin

		if cbGutterFnt.Checked then begin
			Canvas.Font.Name := Items.Strings[Index];
			Canvas.Font.Size := edGutterSize.Value;
		end else begin
			Canvas.Font.Name := cboEditorFont.Text;
			Canvas.Font.Size := edEditorSize.Value;
		end;
		Canvas.FillRect(Rect);
		alignleft := (Rect.Right - Rect.Left) div 2 - Canvas.TextWidth(Canvas.Font.Name) div 2;
		aligntop  := Rect.Top + (Rect.Bottom - Rect.Top) div 2 - Canvas.TextHeight(Canvas.Font.Name) div 2;
		Canvas.TextOut(alignleft, aligntop,Canvas.Font.Name);
	end;
end;

procedure TEditorOptForm.cboEditorFontChange(Sender: TObject);
begin
	if not cbGutterfnt.Checked then
		cboGutterFont.ItemIndex := cboEditorFont.ItemIndex;
	cboGutterFont.Repaint;
end;

procedure TEditorOptForm.edEditorSizeChange(Sender: TObject);
begin
	if not cbGutterfnt.Checked then
		edGutterSize.Value := edEditorSize.Value;
	cboEditorFont.Repaint;
	cboGutterFont.Repaint;
end;

procedure TEditorOptForm.edGutterSizeChange(Sender: TObject);
begin
	cboGutterFont.Repaint;
end;

procedure TEditorOptForm.cbGutterFntClick(Sender: TObject);
begin
	cboGutterFont.Enabled:= cbGutterFnt.Checked;
	edGutterSize.Enabled:= cbGutterfnt.Checked;
	if not cbGutterfnt.Checked then begin
		cboGutterFont.ItemIndex := cboEditorFont.ItemIndex;
		edGutterSize.Value := edEditorSize.Value;
	end;
end;

// Fill listboxes with available fonts
procedure TEditorOptForm.LoadFonts;
begin
	// Sum up all the available fonts
	cboEditorFont.Items.Assign(Screen.Fonts);
	cboGutterFont.Items:= cboEditorFont.Items;

	cbLineNumClick(nil);
	cbGutterFntClick(nil);
end;

{ ---------- Form Init/Done Methods ----------}

procedure TEditorOptForm.LoadText;
begin
	// Set interface font
	Font.Name := devData.InterfaceFont;
	Font.Size := devData.InterfaceFontSize;

  btnOk.Caption:=                Lang[ID_BTN_OK];
  btnCancel.Caption:=            Lang[ID_BTN_CANCEL];
  btnHelp.Caption:=              Lang[ID_BTN_HELP];

  Caption:=                      Lang[ID_EOPT];
  tabGeneral.Caption:=           Lang[ID_EOPT_GENTAB];
  tabDisplay.Caption:=           Lang[ID_EOPT_DISPLAYTAB];
  tabSyntax.Caption:=            Lang[ID_EOPT_SYNTAXTAB];
  tabCode.Caption:=              Lang[ID_EOPT_CODETAB];
  tabCBCompletion.Caption:=      Lang[ID_EOPT_COMPLETIONTAB];
  tabAutosave.Caption:=          Lang[ID_EOPT_AUTOSAVETAB];

  // sub tabs
  tabCPInserts.Caption:=         Lang[ID_EOPT_CPINSERTS];
  tabCPDefault.Caption:=         Lang[ID_EOPT_CPDEFAULT];
  tabSymbols.Caption:=           Lang[ID_EOPT_CPSYMBOLS];

// General Tab
  grpEditorOpts.Caption:=        ' '+Lang[ID_EOPT_EDOPTIONS]+' ';
  cbAutoIndent.Caption:=         Lang[ID_EOPT_AUTOINDENT2];
  cbInsertMode.Caption:=         Lang[ID_EOPT_INSERTMODE];
  cbUseTabs.Caption:=            Lang[ID_EOPT_TAB2SPC];
  cbSmartTabs.Caption:=          Lang[ID_EOPT_SMARTTABS];
  cbGroupUndo.Caption:=          Lang[ID_EOPT_GROUPUNDO];
  cbDropFiles.Caption:=          Lang[ID_EOPT_DROPFILES];
  cbSpecialChars.Caption:=       Lang[ID_EOPT_SPECIALCHARS];
  cbTrimTrailingSpaces.Caption:= Lang[ID_EOPT_TRIMTRAILINGSPACES];
  cbEHomeKey.Caption:=           Lang[ID_EOPT_EHOMEKEY];
  cbPastEOF.Caption:=            Lang[ID_EOPT_PASTEOF];
  cbPastEOL.Caption:=            Lang[ID_EOPT_PASTEOL];
  cbAddIndent.Caption:=          Lang[ID_EOPT_ADDINDENT];
  cbFindText.Caption:=           Lang[ID_EOPT_FINDTEXT];
  cbSmartScroll.Caption:=        Lang[ID_EOPT_SMARTSCROLL];
  cbHalfPage.Caption:=           Lang[ID_EOPT_HALFPAGE];
  cbScrollHint.Caption:=         Lang[ID_EOPT_SCROLLHINT];
  cbParserHints.Caption:=        Lang[ID_EOPT_PARSERHINTS];
  cbFunctionHint.Caption:=       Lang[ID_EOPT_CLOSEBRACE];
  ScrollHint.Caption:=           Lang[ID_EOPT_CTRLSCROLLHINT];
  cbSyntaxHighlight.Caption:=    Lang[ID_EOPT_USESYNTAX];
  lblTabSize.Caption:=           Lang[ID_EOPT_TABSIZE];

  grpMargin.Caption:=            ' '+Lang[ID_EOPT_MARGIN]+' ';
  cbMarginVis.Caption:=          Lang[ID_EOPT_GENERICENABLED];
  lblMarginWidth.Caption:=       Lang[ID_EOPT_WIDTH];
  lblMarginColor.Caption:=       Lang[ID_EOPT_COLOR];
  grpHighCurLine.Caption:=       Lang[ID_EOPT_HIGHCURLINE];
  cbHighlightColor.Caption:=     Lang[ID_EOPT_COLOR];

  grpCaret.Caption:=             ' '+Lang[ID_EOPT_CARET]+' ';
  lblInsertCaret.Caption:=       Lang[ID_EOPT_INSCARET];
  lblOverCaret.Caption:=         Lang[ID_EOPT_OVERCARET];
  cbMatch.Caption:=              Lang[ID_EOPT_MATCH];

  cbHighCurrLine.Caption :=      Lang[ID_EOPT_GENERICENABLED];

  cboInsertCaret.Clear;
  cboInsertCaret.Items.Append(Lang[ID_EOPT_CARET1]);
  cboInsertCaret.Items.Append(Lang[ID_EOPT_CARET2]);
  cboInsertCaret.Items.Append(Lang[ID_EOPT_CARET3]);
  cboInsertCaret.Items.Append(Lang[ID_EOPT_CARET4]);

  cboOverwriteCaret.Clear;
  cboOverwriteCaret.Items.Append(Lang[ID_EOPT_CARET1]);
  cboOverwriteCaret.Items.Append(Lang[ID_EOPT_CARET2]);
  cboOverwriteCaret.Items.Append(Lang[ID_EOPT_CARET3]);
  cboOverwriteCaret.Items.Append(Lang[ID_EOPT_CARET4]);

// Display Tab
  grpEditorFont.Caption:=        ' '+Lang[ID_EOPT_EDFONT]+' ';
  lblEditorFont.Caption:=        Lang[ID_EOPT_FONT];
  lblEditorSize.Caption:=        Lang[ID_EOPT_SIZE];

  grpGutter.Caption:=            ' '+Lang[ID_EOPT_GUTTER]+' ';
  cbGutterVis.Caption:=          Lang[ID_EOPT_VISIBLE];
  cbGutterAuto.Caption:=         Lang[ID_EOPT_GUTTERAUTO];
  cbLineNum.Caption:=            Lang[ID_EOPT_LINENUM];
  cbLeadZero.Caption:=           Lang[ID_EOPT_LEADZERO];
  cbFirstZero.Caption:=          Lang[ID_EOPT_FIRSTZERO];
  cbGutterFnt.Caption:=          Lang[ID_EOPT_GUTTERFNT];
  lblGutterWidth.Caption:=       Lang[ID_EOPT_GUTTERWIDTH];
  lblGutterFont.Caption:=        Lang[ID_EOPT_FONT];
  lblGutterFontSize.Caption:=    Lang[ID_EOPT_SIZE];

// Syntax tab
  lblForeground.Caption:=        Lang[ID_EOPT_FORE];
  lblBackground.Caption:=        Lang[ID_EOPT_BACK];
  grpStyle.Caption:=             ' '+Lang[ID_EOPT_STYLE] +' ';
  cbBold.Caption:=               Lang[ID_EOPT_BOLD];
  cbItalic.Caption:=             Lang[ID_EOPT_ITALIC];
  cbUnderlined.Caption:=         Lang[ID_EOPT_UNDERLINE];
  lblSpeed.Caption:=             Lang[ID_EOPT_SPEED];
  btnSaveSyntax.Hint:=           Lang[ID_EOPT_SAVESYNTAX];

// Code Tab
  lvCodeIns.Cols[0][0] := Lang[ID_EOPT_CIMENU];
  lvCodeIns.Cols[1][0] := Lang[ID_EOPT_CISECTION];
  lvCodeIns.Cols[2][0] := Lang[ID_EOPT_CIDESC];
  cbDefaultCode.Caption:=        Lang[ID_EOPT_DEFCODE];

  cbSymbolComplete.Caption:=     Lang[ID_EOPT_SYMBOLCOMPLETE];
  grpSpecific.Caption:=          ' '+Lang[ID_EOPT_SYMBOLGROUP]+' ';
  cbBraces.Caption:=             Lang[ID_EOPT_SYMBOLBRACES];
  cbParenth.Caption:=            Lang[ID_EOPT_SYMBOLPARENT];
  cbInclude.Caption:=            Lang[ID_EOPT_SYMBOLINCLUDE];
  cbArray.Caption:=              Lang[ID_EOPT_SYMBOLSQUARE];
  cbComments.Caption:=           Lang[ID_EOPT_SYMBOLCOMMENT];

  // Completion Tab
  chkEnableCompletion.Caption:=  Lang[ID_EOPT_COMPLETIONENABLE];
  lblCompletionColor.Caption:=   Lang[ID_EOPT_COMPLETIONCOLOR];
  gbCBEngine.Caption:=           ' '+Lang[ID_EOPT_BROWSERENGINE]+' ';
  chkCBParseLocalH.Caption:=     Lang[ID_EOPT_BROWSERLOCAL];
  chkCBParseGlobalH.Caption:=    Lang[ID_EOPT_BROWSERGLOBAL];
  chkCCCache.Caption:=           Lang[ID_EOPT_CCCACHECHECK];
  btnCCCadd.Caption:=            Lang[ID_BTN_ADD];
  btnCCCdelete.Caption:=         Lang[ID_BTN_CLEAR];
  btnCCCrefresh.Caption:=        Lang[ID_BTN_REFRESH];
  lblRefreshHint.Caption:=       Lang[ID_EOPT_REFRESHHINT];

  // Code snippet tab
  btnAdd.Caption:=               Lang[ID_BTN_ADD];
  btnRemove.Caption:=            Lang[ID_BTN_REMOVE];

  // Autosave
  cbAutoSave.Caption:=           Lang[ID_EOPT_ENABLEAUTOSAVE];
  OptionsGroup.Caption:=         ' '+Lang[ID_EOPT_OPTIONS]+' ';

  FileOptions.Caption:=          ' '+Lang[ID_EOPT_AUTOSAVEFILE]+' ';
  FileOptions.Items[0]:=         Lang[ID_EOPT_AUTOSAVEONLYOPENFILE];
  FileOptions.Items[1]:=         Lang[ID_EOPT_AUTOSAVEALLFILES];
  FileOptions.Items[2]:=         Lang[ID_EOPT_AUTOSAVEPROJECT];

  NameOptions.Caption:=          ' '+Lang[ID_EOPT_AUTOSAVEMODE]+' ';
  NameOptions.Items[0]:=         Lang[ID_EOPT_AUTOSAVEOVERWRITE];
  NameOptions.Items[1]:=         Lang[ID_EOPT_AUTOSAVEUNIX];
  NameOptions.Items[2]:=         Lang[ID_EOPT_AUTOSAVETIME];

	tbCompletionDelayChange(nil);
	MinutesDelayChange(nil);
	NameOptionsClick(nil);
end;

procedure TEditorOptForm.GetOptions;
var
	aName: AnsiString;
	attr: TSynHighlighterAttributes;
	a, idx: integer;
begin
	with devEditor do begin
		cboEditorFont.ItemIndex:=       cboEditorFont.Items.IndexOf(Font.Name);
		edEditorSize.Value:=            Font.Size;

		cbGutterFnt.Checked:=           Gutterfnt;
		cboGutterFont.ItemIndex:=       cboGutterFont.Items.IndexOf(Gutterfont.Name);
		edGutterSize.Value:=            GutterFont.Size;

     cbGutterAuto.Checked:=          GutterAuto;
     cbGutterVis.Checked:=           GutterVis;
     edGutterWidth.Value:=           GutterSize;
     cbLineNum.Checked:=             LineNumbers;
     cbLeadZero.Checked:=            LeadZero;
     cbFirstZero.Checked:=           FirstLineZero;

     cbAutoIndent.Checked:=          AutoIndent;
     cbAddIndent.Checked:=           AddIndent;
     cbInsertMode.Checked:=          InsertMode;
     cbUseTabs.Checked:=             UseTabs;
     cbSmartTabs.Checked:=           SmartTabs;
     cbGroupUndo.Checked:=           GroupUndo;
     cbEHomeKey.Checked:=            EHomeKey;
     cbPastEOF.Checked:=             PastEOF;
     cbPastEOL.Checked:=             PastEOL;
     cbFindText.Checked:=            FindText;
     cbSmartScroll.Checked:=         Scrollbars;
     cbHalfPage.Checked:=            HalfPageScroll;
     cbScrollHint.Checked:=          ScrollHint;
     cbSpecialChars.Checked:=        SpecialChars;
     cbFunctionHint.Checked:=        ShowFunctionTip;
     cbTrimTrailingSpaces.Checked:=  TrimTrailingSpaces;

     cbMarginVis.Checked:=           MarginVis;
     edMarginWidth.Value:=           MarginSize;
     cpMarginColor.Selected:=        MarginColor;

     seTabSize.Value:=               TabSize;
     cbSyntaxHighlight.Checked:=     UseSyntax;
     edSyntaxExt.Text:=              SyntaxExt;
     cboInsertCaret.ItemIndex:=      InsertCaret;
     cboOverwriteCaret.ItemIndex:=   OverwriteCaret;
     cbDropFiles.Checked:=           InsDropFiles;

     cbParserHints.Checked:=         ParserHints;
     cbMatch.Checked :=              Match;
     cbDefaultCode.Checked :=        DefaultCode;

     cbHighCurrLine.Checked :=       HighCurrLine;
     cpHighColor.Selected :=   HighColor;
     cpHighColor.Enabled :=          cbHighCurrLine.Checked;

		StrtoPoint(fSelColor,  Syntax.Values[cSel]);
		StrtoPoint(fGutColor,  Syntax.Values[cGut]);
		StrtoPoint(fbpColor,   Syntax.Values[cBP]);
		StrtoPoint(fErrColor,  Syntax.Values[cErr]);
		StrtoPoint(fABPColor,  Syntax.Values[cABP]);
		StrtoPoint(fFoldColor, Syntax.Values[cFld]);

		// Completion
		cbArray.Checked := ArrayComplete;
		cbBraces.Checked := BraceComplete;
		cbComments.Checked := CommentComplete;
		cbInclude.Checked := IncludeComplete;
		cbParenth.Checked := ParentheseComplete;
		cbSymbolComplete.Checked := CompleteSymbols;

		cbArray.Enabled := cbSymbolComplete.Checked;
		cbBraces.Enabled := cbSymbolComplete.Checked;
		cbComments.Enabled := cbSymbolComplete.Checked;
		cbInclude.Enabled := cbSymbolComplete.Checked;
		cbParenth.Enabled := cbSymbolComplete.Checked;
	end;

	for idx:= 0 to pred(cpp.AttrCount) do begin
		aName:= cpp.Attribute[idx].Name;
		a:= devEditor.Syntax.IndexOfName(aName);
		if a <> -1 then begin
			Attr:= TSynHighlighterAttributes.Create(aName);
			try
				StrtoAttr(Attr, devEditor.Syntax.Values[aName]);
				cpp.Attribute[idx].Assign(attr);
			finally
				Attr.Free;
			end;
		end else
			devEditor.Syntax.Append(aName);
	end;

	ElementList.Clear;
	for idx:= 0 to pred(cpp.AttrCount) do
		ElementList.Items.Append(cpp.Attribute[idx].Name);

	// selection color
	if devEditor.Syntax.IndexofName(cSel) = -1 then
		devEditor.Syntax.Append(cSel);
	ElementList.Items.Append(cSel);

	// gutter colors
	if devEditor.Syntax.IndexofName(cGut) = -1 then
		devEditor.Syntax.Append(cGut);
	ElementList.Items.Append(cGut);

	// breakpoint
	if devEditor.Syntax.IndexOfName(cBP) = -1 then
		devEditor.Syntax.Append(cBP);
	ElementList.Items.Append(cBP);

	// error line
	if devEditor.Syntax.IndexOfName(cErr) = -1 then
		devEditor.Syntax.Append(cErr);
	ElementList.Items.Append(cErr);

	// active breakpoint
	if devEditor.Syntax.IndexOfName(cABP) = -1 then
		devEditor.Syntax.Append(cABP);
	ElementList.Items.Append(cABP);

	// folding color
	if devEditor.Syntax.IndexofName(cFld) = -1 then
		devEditor.Syntax.Append(cFld);
	ElementList.Items.Append(cFld);

	ffgColor:= cpp.WhitespaceAttribute.Foreground;
	fbgColor:= cpp.WhitespaceAttribute.Background;

	if ElementList.Items.Count > 0 then begin
		ElementList.ItemIndex:= 0;
		ElementListClick(nil);
	end;

	if FileExists(devDirs.Config + DEV_DEFAULTCODE_FILE) then
		seDefault.Lines.LoadFromFile(devDirs.Config + DEV_DEFAULTCODE_FILE);

	// Code completion
	chkEnableCompletion.OnClick:=nil;
	chkEnableCompletion.Checked:=devCodeCompletion.Enabled;
	chkEnableCompletion.OnClick:=chkEnableCompletionClick;
	tbCompletionDelay.Position:=devCodeCompletion.Delay;
	cpCompletionBackground.Selected:=devCodeCompletion.BackColor;
	tbCompletionDelay.Enabled:=chkEnableCompletion.Checked;
	cpCompletionBackground.Enabled:=chkEnableCompletion.Checked;
	chkCCCache.Checked:=devCodeCompletion.UseCacheFiles;
	chkCCCache.Tag:=0; // mark un-modified
	chkCCCache.Enabled:=chkEnableCompletion.Checked;
	lbCCC.Enabled:=chkCCCache.Checked and chkEnableCompletion.Checked;
	btnCCCadd.Enabled:=chkCCCache.Checked and chkEnableCompletion.Checked;
	btnCCCdelete.Enabled:=chkCCCache.Checked and chkEnableCompletion.Checked;

	// Parser options
	chkCBParseLocalH.Checked := devCodeCompletion.ParseLocalHeaders;
	chkCBParseGlobalH.Checked := devCodeCompletion.ParseGlobalHeaders;

	// Autosave
	MinutesDelay.Position := devEditor.Interval;
	FileOptions.ItemIndex := devEditor.AutoSaveFilter;
	NameOptions.ItemIndex := devEditor.AutoSaveMode;
	cbAutoSave.Checked := devEditor.EnableAutoSave;

	cbAutoSaveClick(nil);

	SetGutter;
end;

procedure TEditorOptForm.btnOkClick(Sender: TObject);
var
 s, aName: AnsiString;
 a, idx: integer;
begin
	with devEditor do begin
		AutoIndent:=          cbAutoIndent.Checked;
		AddIndent:=           cbAddIndent.Checked;
		InsertMode:=          cbInsertMode.Checked;
		UseTabs:=             cbUseTabs.Checked;
		SmartTabs:=           cbSmartTabs.Checked;
		GroupUndo:=           cbGroupUndo.Checked;
		EHomeKey:=            cbEHomeKey.Checked;
		PastEOF:=             cbPastEOF.Checked;
		PastEOL:=             cbPastEOL.Checked;
		FindText:=            cbFindText.Checked;
		Scrollbars:=          cbSmartScroll.Checked;
		HalfPageScroll:=      cbHalfPage.Checked;
		ScrollHint:=          cbScrollHint.Checked;
		SpecialChars:=        cbSpecialChars.Checked;
		ShowFunctionTip:=     cbFunctionHint.Checked;
		TrimTrailingSpaces:=  cbTrimTrailingSpaces.Checked;

		MarginVis:=           cbMarginVis.Checked;
		MarginSize:=          edMarginWidth.Value;
		MarginColor:=         cpMarginColor.Selected;
		InsertCaret:=         cboInsertCaret.ItemIndex;
		OverwriteCaret:=      cboOverwriteCaret.ItemIndex;
		Match:=               cbMatch.Checked;

		HighCurrLine:=        cbHighCurrLine.Checked;
		HighColor:=           cpHighColor.Selected;

		UseSyntax:=           cbSyntaxHighlight.Checked;
		SyntaxExt:=           edSyntaxExt.Text;
		TabSize:=             seTabSize.Value;

		Font.Name:=           cboEditorFont.Text;
		Font.Size:=           edEditorSize.Value;

		Gutterfont.Name:=     cboGutterFont.Text;
		GutterFont.Size:=     edGutterSize.Value;

		Gutterfnt:=           cbGutterFnt.Checked;
		GutterAuto:=          cbGutterAuto.Checked;
		GutterVis:=           cbGutterVis.Checked;
		GutterSize:=          edGutterWidth.Value;
		LineNumbers:=         cbLineNum.Checked;
		LeadZero:=            cbLeadZero.Checked;
		FirstLineZero:=       cbFirstZero.Checked;
		InsDropFiles:=        cbDropFiles.Checked;

		ParserHints:=         cbParserHints.Checked;

		// Completion
		ArrayComplete:=       cbArray.Checked;
		BraceComplete:=       cbBraces.Checked;
		CommentComplete:=     cbComments.Checked;
		IncludeComplete:=     cbInclude.Checked;
		ParentheseComplete:=  cbParenth.Checked;
		CompleteSymbols:=     cbSymbolComplete.Checked;

		DefaultCode:=         cbDefaultCode.Checked;

		// Autosave
		EnableAutoSave:=      cbAutoSave.Checked;
		Interval:=            MinutesDelay.Position;
		AutoSaveFilter:=      FileOptions.ItemIndex;
		AutoSaveMode:=        NameOptions.ItemIndex;

		// load in attributes
		for idx:= 0 to pred(cpp.AttrCount) do begin
			aName:= cpp.Attribute[idx].Name;
			a:= Syntax.IndexOfName(aName);
			if a = -1 then
				Syntax.Append(format('%s=%s',[aName, AttrtoStr(cpp.Attribute[idx])]))
			else
				Syntax.Values[aName]:= AttrtoStr(cpp.Attribute[idx]);
		end;

		// selected text
		s:= PointtoStr(fSelColor);
		a:= Syntax.IndexofName(cSel);
		if a = -1 then
			Syntax.Append(format('%s=%s', [cSel, s]))
		else
			Syntax.Values[cSel]:= s;

		// gutter
		s:= PointtoStr(fGutColor);
		a:= Syntax.IndexofName(cGut);
		if a = -1 then
			Syntax.Append(format('%s=%s', [cGut, s]))
		else
			Syntax.Values[cGut]:= s;

		// breakpoints
		s:= PointtoStr(fbpColor);
		a:= Syntax.IndexofName(cBP);
		if a = -1 then
			Syntax.Append(format('%s=%s', [cBP, s]))
		else
			Syntax.Values[cBP]:= s;

		// error line
		s:= PointtoStr(fErrColor);
		a:= Syntax.IndexofName(cErr);
		if a = -1 then
			Syntax.Append(format('%s=%s', [cErr, s]))
		else
			Syntax.Values[cErr]:= s;

		// active breakpoint
		s:= PointtoStr(fAbpColor);
		a:= Syntax.IndexofName(cABP);
		if a = -1 then
			Syntax.Append(format('%s=%s', [cABP, s]))
		else
			Syntax.Values[cABP]:= s;

		// fold bar
		s:= PointtoStr(fFoldColor);
		a:= Syntax.IndexofName(cFld);
		if a = -1 then
			Syntax.Append(format('%s=%s', [cFld, s]))
		else
			Syntax.Values[cFld]:= s;
	end;

	// Save our code snippet even if we opted not to use it (user may want to keep it)
	if not IsEmpty(seDefault) then
		seDefault.Lines.SavetoFile(devDirs.Config + DEV_DEFAULTCODE_FILE)
	else
		DeleteFile(devDirs.Config + DEV_DEFAULTCODE_FILE);

	SaveCodeIns;

	with devCodeCompletion do begin
		Enabled:=chkEnableCompletion.Checked;
		Delay:=tbCompletionDelay.Position;
		BackColor:=cpCompletionBackground.Selected;
		UseCacheFiles:=chkCCCache.Checked;
		ParseLocalHeaders:=chkCBParseLocalH.Checked;
		ParseGlobalHeaders:=chkCBParseGlobalH.Checked;
	end;

	// Only create the timer if autosaving is enabled
	if devEditor.EnableAutoSave then begin
		if not Assigned(MainForm.AutoSaveTimer) then
			MainForm.AutoSaveTimer := TTimer.Create(nil);
		MainForm.AutoSaveTimer.Interval := devEditor.Interval*60*1000; // miliseconds to minutes
		MainForm.AutoSaveTimer.Enabled := devEditor.EnableAutoSave;
		MainForm.AutoSaveTimer.OnTimer := MainForm.EditorSaveTimer;
	end else
		FreeAndNil(MainForm.AutoSaveTimer);

	SaveOptions;
	dmMain.LoadDataMod;
end;

procedure TEditorOptForm.btnHelpClick(Sender: TObject);
begin
	OpenHelpFile;
end;

procedure TEditorOptForm.btnCancelClick(Sender: TObject);
begin
	Close;
end;

{ ---------- Syntax Style Methods ---------- }

procedure TEditorOptForm.SetGutter;
begin
	// update preview
	cppedit.Gutter.Color:= fgutColor.x;
	cppedit.Gutter.Font.Color:= fgutColor.y;
	cppedit.CodeFolding.FolderBarLinesColor := fFoldColor.y;

	// update snippet edit
	CodeIns.Gutter.Color:= fgutColor.x;
	CodeIns.Gutter.Font.Color:= fgutColor.y;
	CodeIns.CodeFolding.FolderBarLinesColor := fFoldColor.y;

	// update default source edit
	seDefault.Gutter.Color:= fgutColor.x;
	seDefault.Gutter.Font.Color:= fgutColor.y;
	seDefault.CodeFolding.FolderBarLinesColor := fFoldColor.y;
end;

procedure TEditorOptForm.ElementListClick(Sender: TObject);
var
	pt: TPoint;
begin
	// Special additions not directly exposed by TSynHighlighter
	if ElementList.ItemIndex > pred(cpp.AttrCount) then begin
		fUpdate:= FALSE;

		cpBackground.Enabled := True;

		if SameText(ElementList.Items[ElementList.ItemIndex], cSel) then
			pt:= fSelColor
		else if SameText(ElementList.Items[ElementList.ItemIndex], cBP) then
			pt:= fBPColor
		else if SameText(ElementList.Items[ElementList.ItemIndex], cErr) then
			pt:= fErrColor
		else if SameText(ElementList.Items[ElementList.ItemIndex], cABP) then
			pt:= fABPColor
		else if SameText(ElementList.Items[ElementList.ItemIndex], cGut) then
			pt:= fGutColor
		else if SameText(ElementList.Items[ElementList.ItemIndex], cFld) then begin
			pt:= fFoldColor;
			cpBackground.Enabled := false;
		end;

		cpBackground.Selected:= pt.x;
		cpForeground.Selected:= pt.y;

		cbBold.Checked:= False;
		cbItalic.Checked:= False;
		cbUnderlined.Checked:= False;

		cbBold.Enabled := False;
		cbItalic.Enabled:= False;
		cbUnderlined.Enabled:= False;

		fUpdate:= TRUE;
	end else if ElementList.ItemIndex <> -1 then begin // regular SynEdit attributes
		with Cpp.Attribute[ElementList.ItemIndex] do begin
			fUpdate:= FALSE;

			if Foreground = clNone then
				cpForeground.Selected:= ffgcolor //clNone
			else
				cpForeground.Selected:= Foreground;
			if Background = clNone then
				cpBackground.Selected:= fbgcolor //clNone
			else
				cpBackground.Selected:= Background;

			cpBackground.Enabled := True;

			cbBold.Enabled := True;
			cbItalic.Enabled:= True;
			cbUnderlined.Enabled:= True;

			cbBold.Checked:= fsBold in Style;
			cbItalic.Checked:= fsItalic in Style;
			cbUnderlined.Checked:= fsUnderline in Style;

			fUpdate:= TRUE;
		end;
	end;

	// These two don't always update for some reason
	cpBackground.Repaint;
	cpForeground.Repaint;
end;

procedure TEditorOptForm.StyleChange(Sender: TObject);
var
	attr: TSynHighlighterAttributes;
	pt: TPoint;
	s: AnsiString;
begin
	if not fUpdate then exit;
	if ElementList.ItemIndex < 0 then exit;
	if ElementList.ItemIndex > pred(cpp.AttrCount) then begin
		pt.x:= cpBackground.Selected;
		pt.y:= cpForeground.Selected;

		// use local AnsiString just to ease readability
		s:= ElementList.Items[ElementList.ItemIndex];

		// if either value is clnone set to Whitespace color values
		if pt.x = clNone then pt.x:= fbgColor;
		if pt.y = clNone then pt.y:= ffgColor;

		if SameText(s, cSel) then
			fSelColor:= pt
		else if SameText(s, cBP) then
			fBPColor:= pt
		else if SameText(s, cABP) then
			fABPColor:= pt
		else if SameText(s, cerr) then
			fErrColor:= pt
		else if SameText(s, cGut) then begin
			fGutColor:= pt;
			SetGutter;
		end else if SameText(s, cFld) then begin
			fFoldColor:= pt;
			SetGutter;
		end;
	end else begin
		Attr:= TSynHighlighterAttributes.Create(ElementList.Items[ElementList.ItemIndex]);
		Attr.Assign(cpp.Attribute[ElementList.ItemIndex]);
		with Attr do try
			Foreground:= cpForeground.Selected;
			Background:= cpBackground.Selected;

			// Update default color
			if SameText(Name, 'WhiteSpace') then begin
				ffgColor:= Foreground;
				fbgColor:= Background;
			end;

			Style:= [];
			if cbBold.checked then Style:= Style + [fsBold];
			if cbItalic.Checked then Style:= Style + [fsItalic];
			if cbUnderlined.Checked then Style:= Style + [fsUnderline];

			cpp.Attribute[ElementList.ItemIndex].Assign(Attr);
		finally
			Free;
		end;
	end;

	cppEdit.Repaint;
end;

procedure TEditorOptForm.cppEditStatusChange(Sender: TObject;
    Changes: TSynStatusChanges);
var
 Token: AnsiString;
 attr: TSynHighlighterAttributes;
begin
  if assigned(cppEdit.Highlighter) and
    (Changes *[scAll, scCaretX, scCaretY] <> []) then
   case cppEdit.CaretY of
    cSelection:
     begin
       ElementList.ItemIndex:= ElementList.Items.Indexof(cSel);
       ElementListClick(Self);
     end;
    cBreakLine:
     begin
       ElementList.ItemIndex:= ElementList.Items.Indexof(cBP);
       ElementListClick(Self);
     end;
    cABreakLine:
     begin
       ElementList.ItemIndex:= ElementList.Items.Indexof(cABP);
       ElementListClick(Self);
     end;
    cErrorLine:
     begin
       ElementList.ItemIndex:= ElementList.Items.Indexof(cErr);
       ElementListClick(Self);
     end;
    else
     begin
       if not cppEdit.GetHighlighterAttriAtRowCol(cppEdit.CaretXY, Token, Attr) then
        Attr:= cppEdit.Highlighter.WhiteSpaceAttribute;
       if assigned(Attr) then
        begin
          ElementList.ItemIndex:= ElementList.Items.Indexof(Attr.Name);
          ElementListClick(Self);
        end;
     end;
   end;
end;

procedure TEditorOptForm.CppEditSpecialLineColors(Sender: TObject;
  Line: Integer; var Special: Boolean; var FG, BG: TColor);
begin
  case Line of
   cSelection:
    begin
      if fSelColor.x <> clNone then
       BG:= fSelColor.x;
      if fSelColor.y <> clNone then
       FG:= fSelColor.y;
      Special:= TRUE;
    end;
   cBreakLine:
    begin
      if fBPColor.x <> clNone then
       BG:= fBPColor.x;
      if fBPColor.y <> clNone then
       FG:= fBPColor.y;
      Special:= TRUE;
    end;
   cABreakLine:
    begin
      if fABPColor.x <> clNone then
       BG:= fABPColor.X;
      if fABPColor.y <> clNone then
       FG:= fABPColor.y;
      Special:= TRUE;
    end;
   cErrorLine:
    begin
      if fErrColor.x <> clNone then
       BG:= fErrColor.x;
      if fErrColor.y <> clNone then
       FG:= fErrColor.y;
      Special:= TRUE;
    end;
  end;
end;

procedure TEditorOptForm.cbLineNumClick(Sender: TObject);
begin
	cbLeadZero.Enabled:= cbLineNum.Checked;
	cbFirstZero.Enabled:= cbLineNum.Checked;
end;

procedure TEditorOptForm.cbSyntaxHighlightClick(Sender: TObject);
begin
	edSyntaxExt.Enabled:= cbSyntaxHighlight.Checked;
end;

procedure TEditorOptForm.cboQuickColorSelect(Sender: TObject);
var
	offset: integer;
	i: integer;
	attr: TSynHighlighterAttributes;
begin
	if cboQuickColor.ItemIndex > 10 then begin // 10 == number of built-in styles
		// custom style; load from disk
		LoadSyntax(cboQuickColor.Items[cboQuickColor.ItemIndex]);
	end else begin

		offset:= cboQuickColor.ItemIndex * 1000;
		for i:= 0 to pred(cpp.AttrCount) do begin
			attr:= TSynHighlighterAttributes.Create(cpp.Attribute[i].Name);
			try
				StrtoAttr(Attr, LoadStr(i + offset + 1));
				cpp.Attribute[i].Assign(Attr);
			finally
				Attr.Free;
			end;
		end;

		StrtoPoint(fBPColor,   LoadStr(offset+17)); // breakpoints
		StrtoPoint(fErrColor,  LoadStr(offset+18)); // error line
		StrtoPoint(fABPColor,  LoadStr(offset+19)); // active breakpoint
		StrtoPoint(fgutColor,  LoadStr(offset+20)); // gutter
		StrtoPoint(fSelColor,  LoadStr(offset+21)); // selected text
		StrtoPoint(fFoldColor, LoadStr(offset+22)); // folding bar lines
	end;

	SetGutter;
	cppEdit.Repaint;
	ElementListClick(nil);
end;

{ ---------- Code insert methods ---------- }

procedure TEditorOptForm.btnAddClick(Sender: TObject);
begin
	CodeIns.ClearAll; // clear example editor

	lvCodeIns.RowCount := lvCodeIns.RowCount + 1; // add blank row, assume fixedrows remains at 1

	// Fill
	lvCodeIns.Objects[0,lvCodeIns.RowCount-1] := TStringList.Create;
	lvCodeIns.Cells[0,lvCodeIns.RowCount-1] := '';
	lvCodeIns.Cells[1,lvCodeIns.RowCount-1] := '';
	lvCodeIns.Cells[2,lvCodeIns.RowCount-1] := '';

	lvCodeIns.Row := lvCodeIns.RowCount-1; // set selection
	lvCodeIns.Col := 0; // set selection
	lvCodeIns.SetFocus;

	UpdateCIButtons;
end;

procedure TEditorOptForm.btnRemoveClick(Sender: TObject);
var
	I : integer;
	sl : TStringList;
begin
	if (lvCodeIns.Row >= lvCodeIns.FixedRows) then begin
		if (lvCodeIns.RowCount > 2) then begin // remove completely
			dmMain.CodeInserts.Delete(lvCodeIns.Row);

			// Delete object containing text too
			TStringList(lvCodeIns.Objects[0,lvCodeIns.Row]).Free;
			lvCodeIns.Objects[0,lvCodeIns.Row] := nil;

			for I := lvCodeIns.Row to lvCodeins.RowCount - 2 do
				lvCodeIns.Rows[i].Assign(lvCodeIns.Rows[i + 1]); // moves objects too

			lvCodeIns.RowCount := lvCodeIns.RowCount - 1;
		end else begin // leave blank row
			sl := TStringList(lvCodeIns.Objects[0,lvCodeIns.Row]); // leave blank data
			sl.Clear;
			lvCodeIns.Rows[lvCodeIns.RowCount-1].Text := ''; // removes data pointer
			lvCodeIns.Objects[0,lvCodeIns.Row] := sl;
		end;

		lvCodeIns.Repaint;
		CodeIns.ClearAll;
		UpdateCIButtons;
	end;
end;

procedure TEditorOptForm.UpdateCIButtons;
begin
	btnAdd.Enabled := true;
	btnRemove.Enabled := lvCodeIns.Row > 0;
end;

procedure TEditorOptForm.lvCodeInsSelectCell(Sender: TObject; ACol,ARow: Integer; var CanSelect: Boolean);
begin
	CodeIns.ClearAll;
	if (lvCodeIns.Row >= lvCodeIns.FixedRows) then begin
		CodeIns.Text:= StrToCodeIns(TStringList(lvCodeIns.Objects[0,ARow]).Text); // store code in first column object
		UpdateCIButtons;
	end;
end;

procedure TEditorOptForm.CodeInsStatusChange(Sender: TObject;Changes: TSynStatusChanges);
begin
	if (lvCodeIns.Row >= lvCodeIns.FixedRows) then begin
		if (scModified in Changes) then begin
			TStringList(lvCodeIns.Objects[0,lvCodeIns.Row]).Text := CodeInstoStr(CodeIns.Text); // store text in hidden field
			CodeIns.Modified:= false;
		end;
	end;
end;

procedure TEditorOptForm.LoadCodeIns;
var
	ins: PCodeIns;
	I : integer;
	sl : TStringList;
	canselect : boolean;
begin
	lvCodeIns.FixedRows := 0;
	lvCodeIns.RowCount := 1; // re-add fixed row later on

	for I := 0 to dmMain.CodeInserts.Count - 1 do begin
		lvCodeIns.RowCount := lvCodeIns.RowCount + 1; // add new blank row

		// Don't forget to delete!
		sl := TStringList.Create;

		// Fill cols
		ins := dmMain.CodeInserts[I];
		sl.Text := ins^.Line;
		lvCodeIns.Objects[0,lvCodeIns.RowCount-1] := sl;
		lvCodeIns.Cells[0,lvCodeIns.RowCount-1] := ins^.Caption;
		lvCodeIns.Cells[1,lvCodeIns.RowCount-1] := IntToStr(ins^.Sep);
		lvCodeIns.Cells[2,lvCodeIns.RowCount-1] := ins^.Desc;
	end;

	// Add empty, but configured row
	if lvCodeIns.RowCount = 1 then begin
		lvCodeIns.RowCount := lvCodeIns.RowCount + 1;

		// Fill
		lvCodeIns.Objects[0,lvCodeIns.RowCount-1] := TStringList.Create;
		lvCodeIns.Cells[0,lvCodeIns.RowCount-1] := '';
		lvCodeIns.Cells[1,lvCodeIns.RowCount-1] := '';
		lvCodeIns.Cells[2,lvCodeIns.RowCount-1] := '';
	end;

	lvCodeIns.FixedRows := 1; // gets reset to 0 when removing all editable rows

	lvCodeInsSelectCell(nil,0,1,canselect);
end;

procedure TEditorOptForm.SaveCodeIns;
var
	I: integer;
	Item: PCodeIns;
begin
	dmMain.CodeInserts.Clear;
	for I := lvCodeIns.FixedRows to lvCodeIns.RowCount - 1 do begin
		if lvCodeIns.Cells[0,I] <> '' then begin
			Item := new(PCodeIns);

			// Get snippet from attached object
			Item.Caption := lvCodeIns.Cells[0,I];
			Item.Sep := StrToIntDef(lvCodeIns.Cells[1,I],0);
			Item.Desc := lvCodeIns.Cells[2,I];
			Item.Line := TStringList(lvCodeIns.Objects[0,I]).Text;

			dmMain.CodeInserts.AddItem(Item);
		end;
	end;
	dmMain.CodeInserts.SaveCode;
end;

procedure TEditorOptForm.ClearCodeIns;
var
	I : integer;
begin
	// Clear the code insertion string grid objects
	for I := 1 to lvCodeIns.RowCount - 1 do begin
		if Assigned(lvCodeIns.Objects[0,I]) then begin
			TStringList(lvCodeIns.Objects[0,I]).Free;
			lvCodeIns.Objects[0,I] := nil;
		end;
	end;
end;

{ ---------- Code completion ---------- }

procedure TEditorOptForm.tbCompletionDelayChange(Sender: TObject);
begin
	lblCompletionDelay.Caption := Lang[ID_EOPT_COMPLETIONDELAY] + ' ' + IntToStr(tbCompletionDelay.Position)+' ms';
end;

procedure TEditorOptForm.chkEnableCompletionClick(Sender: TObject);
begin
	with chkEnableCompletion do begin
		tbCompletionDelay.Enabled := Checked;
		cpCompletionBackground.Enabled := Checked;
		chkCCCache.Checked := chkCCCache.Checked and Checked;
		chkCCCache.Enabled := Checked;
		chkCCCacheClick(Self);
	end;
end;

procedure TEditorOptForm.btnSaveSyntaxClick(Sender: TObject);
var
  idx: integer;
  fINI: TIniFile;
  S: AnsiString;
  pt: TPoint;
begin
  s:='New syntax';
  if not InputQuery(Lang[ID_EOPT_SAVESYNTAX], Lang[ID_EOPT_SAVESYNTAXQUESTION], s) or (s='') then
    Exit;

  fINI:=TIniFile.Create(devDirs.Config+s+SYNTAX_EXT);
  try
    for idx:= 0 to pred(Cpp.AttrCount) do
      fINI.WriteString('Editor.Custom', Cpp.Attribute[idx].Name, AttrtoStr(Cpp.Attribute[idx]));

    for idx:= Cpp.AttrCount to pred(ElementList.Items.Count) do begin
      if CompareText(ElementList.Items[idx], cSel) = 0 then
        pt:= fSelColor
      else if CompareText(ElementList.Items[idx], cBP) = 0 then
        pt:= fBPColor
      else if CompareText(ElementList.Items[idx], cErr) = 0 then
        pt:= fErrColor
      else if CompareText(ElementList.Items[idx], cABP) = 0 then
        pt:= fABPColor
      else if CompareText(ElementList.Items[idx], cGut) = 0 then
        pt:= fGutColor
      else if CompareText(ElementList.Items[idx], cFld) = 0 then
        pt:= fFoldColor;

      fINI.WriteString('Editor.Custom', ElementList.Items[idx], PointtoStr(pt));
    end;
  finally
    fINI.Free;
  end;
  if cboQuickColor.Items.IndexOf(S)=-1 then
    cboQuickColor.Items.Add(S);
  cboQuickColor.ItemIndex:=cboQuickColor.Items.IndexOf(S);
end;

procedure TEditorOptForm.LoadSyntax(const Value: AnsiString);
var
  idx: integer;
  fINI: TIniFile;
  Attr: TSynHighlighterAttributes;
  pt: TPoint;
begin
	fINI:=TIniFile.Create(devDirs.Config+Value+SYNTAX_EXT);
	try
		for idx:= 0 to pred(Cpp.AttrCount) do begin
			Attr:=TSynHighlighterAttributes.Create(Cpp.Attribute[idx].Name);
			try
				StrToAttr(Attr, fINI.ReadString('Editor.Custom', Cpp.Attribute[idx].Name, devEditor.Syntax.Values[Cpp.Attribute[idx].Name]));
				Cpp.Attribute[idx].Assign(Attr);
			finally
				Attr.Free;
			end;
		end;

		for idx:= Cpp.AttrCount to pred(ElementList.Items.Count) do begin
			StrToPoint(pt, fINI.ReadString('Editor.Custom', ElementList.Items[idx], PointToStr(Point(clNone, clNone))));
			if CompareText(ElementList.Items[idx], cSel) = 0 then
				fSelColor:= pt
			else if CompareText(ElementList.Items[idx], cBP) = 0 then
				fBPColor:= pt
			else if CompareText(ElementList.Items[idx], cErr) = 0 then
				fErrColor:= pt
			else if CompareText(ElementList.Items[idx], cABP) = 0 then
				fABPColor:= pt
			else if CompareText(ElementList.Items[idx], cGut) = 0 then begin
				fGutColor:= pt;
				SetGutter;
			end else if CompareText(ElementList.Items[idx], cFld) = 0 then begin
				fFoldColor:= pt;
				SetGutter;
			end;
		end;
	finally
		fINI.Free;
	end;
	ElementListClick(nil);
end;

procedure TEditorOptForm.FillSyntaxSets;
var
  SR: TSearchRec;
begin
  if FindFirst(devDirs.Config+'*'+SYNTAX_EXT, faAnyFile, SR)=0 then
    repeat
      cboQuickColor.Items.Add(StringReplace(SR.Name, SYNTAX_EXT, '', [rfIgnoreCase]));
    until FindNext(SR)<>0;
end;

procedure TEditorOptForm.btnFileBrowseClick(Sender: TObject);
var
	I,J: integer;
	sl : TStringList;
	S : AnsiString;
begin
	with TOpenDialog.Create(Self) do try

		Filter := BuildFilter([FLT_HEADS]);
		Options := Options + [ofAllowMultiSelect];

		// Start in the include folder, if it's set
		FileName := '';
		if Assigned(devCompilerSets.CurrentSet) and (devCompilerSets.CurrentSet.CppDir.Count > 0) then
			InitialDir := devCompilerSets.CurrentSet.CppDir[0];

		if Execute then begin
			sl := TStringList.Create;
			try
				// Make them all relative
				for I := 0 to Files.Count - 1 do begin
					S := Files[i];
					if Assigned(devCompilerSets.CurrentSet) then begin
						for J := 0 to devCompilerSets.CurrentSet.CppDir.Count -1 do
							S := StringReplace(S,devCompilerSets.CurrentSet.CppDir[j] + pd,'',[rfReplaceAll]);
					end;
					sl.Add(s);
				end;

				// Append ; to old part
				if (Length(edIncludeFile.Text) > 0) and (edIncludeFile.Text[Length(edIncludeFile.Text)] <> ';') then
					edIncludeFile.Text := edIncludeFile.Text + ';';

				// Add all files, separated by ;
				for I := 0 to sl.Count - 1 do
					edIncludeFile.Text := edIncludeFile.Text + sl[i] + ';';
			finally
				sl.Free;
			end;
		end;
	finally
		Free;
	end;
end;

procedure TEditorOptForm.btnCCCaddClick(Sender: TObject);
var
	I: integer;
	sl : TStringList;
	S : AnsiString;
begin
	sl := TStringList.Create;
	try
		// This edit box contains files separated by ;
		ExtractStrings([';'],[],PAnsiChar(edIncludeFile.Text),sl);

		Screen.Cursor:=crHourglass;
		Application.ProcessMessages;

		// Undo the make relative step
		for I := 0 to sl.Count - 1 do begin
			S := MainForm.CppParser.GetFullFileName(sl[i]);
			CppParser.AddFileToScan(S);
		end;

		CppParser.ParseList;
		CppParser.Save(devDirs.Config + DEV_COMPLETION_CACHE,devDirs.Exec);

		lbCCC.Items.BeginUpdate;
		lbCCC.Clear;
		for I := 0 to CppParser.CacheContents.Count - 1 do
			lbCCC.Items.Add(ReplaceFirststr(CppParser.CacheContents[i],devDirs.Exec,''));
		lbCCC.Items.EndUpdate;

		Screen.Cursor := crDefault;
		edIncludeFile.Text := '';
		chkCCCache.Tag := 1; // mark modified
	finally
		sl.Free;
	end;
end;

procedure TEditorOptForm.btnCCCdeleteClick(Sender: TObject);
begin
	if lbCCC.Items.Count=0 then
		Exit;

	if MessageDlg(Lang[ID_EOPT_CLEARCACHE], mtConfirmation, [mbYes, mbNo], 0)=mrYes then begin

		// Delete cache from disk
		DeleteFile(devDirs.Config + DEV_COMPLETION_CACHE);

		// Delete everything from RAM
		CppParser.Reset(false);

		lbCCC.Clear;

		chkCCCache.Tag := 1; // mark modified
	end;
end;

procedure TEditorOptForm.btnCCCrefreshClick(Sender: TObject);
var
	sl : TStringList;
	I : integer;
begin
	if lbCCC.Items.Count=0 then
		Exit;

	// Don't ask for confirmation, not dangerous

	sl := TStringList.Create;
	try
		Screen.Cursor := crHourglass;
		Application.ProcessMessages;

		// Backup cache contents
		sl.Assign(CppParser.CacheContents);

		// Delete cache from disk
		DeleteFile(devDirs.Config + DEV_COMPLETION_CACHE);

		// Delete everything from RAM
		CppParser.Reset(false);

		// Parse list again
		for I := 0 to sl.Count - 1 do
			CppParser.AddFileToScan(sl[i]);
		CppParser.ParseList;
		CppParser.Save(devDirs.Config + DEV_COMPLETION_CACHE,devDirs.Exec);

		Screen.Cursor := crDefault;
		chkCCCache.Tag := 1; // mark modified
	finally
		sl.Free;
	end;
end;

procedure TEditorOptForm.FillCCC;
var
	I : integer;
begin
	Screen.Cursor := crHourglass;
	Application.ProcessMessages;

	CppParser.Load(devDirs.Config + DEV_COMPLETION_CACHE,devDirs.Exec);

	lbCCC.Items.BeginUpdate;
	lbCCC.Clear;
	for I := 0 to CppParser.CacheContents.Count - 1 do
		lbCCC.Items.Add(ReplaceFirststr(CppParser.CacheContents[i],devDirs.Exec,''));
	lbCCC.Items.EndUpdate;

	btnCCCadd.Enabled := false;

	Screen.Cursor:=crDefault;
end;

procedure TEditorOptForm.chkCCCacheClick(Sender: TObject);
begin
	chkCCCache.Tag := 1; // mark modified
	lbCCC.Enabled := chkCCCache.Checked;
	btnCCCadd.Enabled := chkCCCache.Checked and (edIncludeFile.Text <> '');
	edIncludeFile.Enabled := chkCCCache.Checked;
	btnFileBrowse.Enabled := chkCCCache.Checked;
end;

procedure TEditorOptForm.CppParser1StartParsing(Sender: TObject);
begin
  pbCCCache.Visible:=True;
end;

procedure TEditorOptForm.CppParser1EndParsing(Sender: TObject);
begin
  pbCCCache.Visible:=False;
end;

procedure TEditorOptForm.CppParser1TotalProgress(Sender: TObject;const FileName: string; Total, Current: Integer);
begin
	if not HasProgressStarted then begin
		pbCCCache.Max := Total;
		HasProgressStarted := true;
	end;
	pbCCCache.Position := pbCCCache.Position + Current;
	Application.ProcessMessages;
end;

procedure TEditorOptForm.OnGutterClick(Sender: TObject;Button: TMouseButton; X, Y, Line: Integer; Mark: TSynEditMark);
var
	idx: integer;
begin
	idx:= ElementList.Items.IndexOf(cGut);
	if idx <> -1 then begin
		ElementList.ItemIndex:= idx;
		ElementListClick(Self);
	end;
end;

procedure TEditorOptForm.cbHighCurrLineClick(Sender: TObject);
begin
	cpHighColor.Enabled := cbHighCurrLine.Checked;
end;

procedure TEditorOptForm.cbAutoSaveClick(Sender: TObject);
begin
	MinutesDelay.Enabled := cbAutoSave.Checked;
	SaveInterval.Enabled := cbAutoSave.Checked;
	FileOptions.Enabled := cbAutoSave.Checked;
	OptionsGroup.Enabled := cbAutoSave.Checked;
	NameOptions.Enabled := cbAutoSave.Checked;
	lblTimeStampExample.Enabled := cbAutoSave.Checked;
end;

procedure TEditorOptForm.MinutesDelayChange(Sender: TObject);
begin
	if MinutesDelay.Position = 1 then
		SaveInterval.Caption := Lang[ID_EOPT_AUTOSAVEINTERNAL] + ' ' + IntToStr(MinutesDelay.Position) + ' minute'
	else
		SaveInterval.Caption := Lang[ID_EOPT_AUTOSAVEINTERNAL] + ' ' + IntToStr(MinutesDelay.Position) + ' minutes';
end;

procedure TEditorOptForm.cbSymbolCompleteClick(Sender: TObject);
begin
	cbArray.Enabled := cbSymbolComplete.Checked;
	cbBraces.Enabled := cbSymbolComplete.Checked;
	cbComments.Enabled := cbSymbolComplete.Checked;
	cbInclude.Enabled := cbSymbolComplete.Checked;
	cbParenth.Enabled := cbSymbolComplete.Checked;
end;

procedure TEditorOptForm.FormClose(Sender: TObject;var Action: TCloseAction);
begin
	ClearCodeIns;
	// Our caller needs the form, so let it call Free instead
	action := caHide;
end;

procedure TEditorOptForm.PagesMainChange(Sender: TObject);
begin
	if (PagesMain.ActivePage = tabCBCompletion) and (CppParser.Statements.Count = 0) then
		FillCCC;
end;

procedure TEditorOptForm.NameOptionsClick(Sender: TObject);
begin
	case NameOptions.ItemIndex of
		0: begin
			lblTimeStampExample.Caption := Format(Lang[ID_EOPT_AUTOSAVEEXAMPLE],
				['main.cpp']);
		end;
		1: begin
			lblTimeStampExample.Caption := Format(Lang[ID_EOPT_AUTOSAVEEXAMPLE],
				[ChangeFileExt('main.cpp','.' + IntToStr(DateTimeToUnix(Now)) + ExtractFileExt('main.cpp'))]);
		end;
		2: begin
			lblTimeStampExample.Caption := Format(Lang[ID_EOPT_AUTOSAVEEXAMPLE],
				[ChangeFileExt('main.cpp','.' + FormatDateTime('yyyy mm dd hh mm ss',Now) + ExtractFileExt('main.cpp'))]);
		end;
	end;
end;

procedure TEditorOptForm.edIncludeFileKeyPress(Sender: TObject;
  var Key: Char);
begin
	if Key = Chr(VK_RETURN) then begin
		Key := #0; // mute beep
		btnCCCaddClick(nil);
	end;
end;

procedure TEditorOptForm.edIncludeFileChange(Sender: TObject);
begin
	btnCCCadd.Enabled := edIncludeFile.Text <> '';
end;

procedure TEditorOptForm.edIncludeFileClick(Sender: TObject);
begin
	if edIncludeFile.Font.Color <> clWindowText then begin // first click
		edIncludeFile.Text := '';
		edIncludeFile.Font.Color := clWindowText;
	end;
end;

end.
