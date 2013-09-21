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
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, Spin, ColorPickerButton,
  SynEdit, SynEditHighlighter, SynHighlighterCpp,
  Buttons, ClassBrowser, CppParser, CppTokenizer, StrUtils;
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
    cpForeground: TColorPickerButton;
    cpBackground: TColorPickerButton;
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
    cpMarginColor: TColorPickerButton;
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
    btnEdit: TButton;
    btnRemove: TButton;
    lvCodeins: TListView;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    cboQuickColor: TComboBox;
    lblSpeed: TLabel;
    CodeIns: TSynEdit;
    tabClassBrowsing: TTabSheet;
    chkEnableClassBrowser: TCheckBox;
    btnSaveSyntax: TSpeedButton;
    ClassCodePage: TPageControl;
    tabCBBrowser: TTabSheet;
    tabCBCompletion: TTabSheet;
    lblClassBrowserSample: TLabel;
    ClassBrowser: TClassBrowser;
    gbCBEngine: TGroupBox;
    chkCBParseGlobalH: TCheckBox;
    chkCBParseLocalH: TCheckBox;
    gbCBView: TGroupBox;
    lblCompletionDelay: TLabel;
    cpCompletionBackground: TColorPickerButton;
    lblCompletionColor: TLabel;
    tbCompletionDelay: TTrackBar;
    chkEnableCompletion: TCheckBox;
    chkCBUseColors: TCheckBox;
    chkCCCache: TCheckBox;
    btnCCCnew: TSpeedButton;
    btnCCCdelete: TSpeedButton;
    CppTokenizer: TCppTokenizer;
    CppParser: TCppParser;
    lbCCC: TListBox;
    pbCCCache: TProgressBar;
    chkCBShowInherited: TCheckBox;
    cbMatch: TCheckBox;
    grpEditorOpts: TGroupBox;
    edMarginWidth: TSpinEdit;
    edGutterWidth: TSpinEdit;
    cbHighCurrLine: TCheckBox;
    cpHighColor: TColorPickerButton;
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
    cbDoubleLine: TCheckBox;
    cbAutoIndent: TCheckBox;
    cbTrimTrailingSpaces: TCheckBox;
    ScrollHint: TLabel;
    tabAutosave: TTabSheet;
    cbAutoSave: TCheckBox;
    OptionsGroup: TGroupBox;
    SaveInterval: TLabel;
    MinutesDelay: TTrackBar;
    FileOptions: TRadioGroup;
    HighCurLineBox: TGroupBox;
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
    GroupBox1: TGroupBox;
    seTabSize: TSpinEdit;
    lblTabSize: TLabel;
    cbUseTabs: TCheckBox;
    cbSmartTabs: TCheckBox;
    cbHighlightColor: TLabel;
    cbDefaultCode: TCheckBox;
    seDefault: TSynEdit;
    NameOptions: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure SetGutter;
    procedure ElementListClick(Sender: TObject);
    procedure cpMarginColorHint(Sender: TObject; Cell: Integer;var Hint: string);
    procedure cpMarginColorDefaultSelect(Sender: TObject);
    procedure cppEditStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure DefaultSelect(Sender: TObject);
    procedure PickerHint(Sender: TObject; Cell: integer; var Hint: string);
    procedure StyleChange(Sender: TObject);
    procedure cbLineNumClick(Sender: TObject);
    procedure cbSyntaxHighlightClick(Sender: TObject);
    procedure cbGutterFntClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure lvCodeinsColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvCodeinsCompare(Sender: TObject; Item1, Item2: TListItem;Data: Integer; var Compare: Integer);
    procedure lvCodeinsSelectItem(Sender: TObject; Item: TListItem;Selected: Boolean);
    procedure CodeInsStatusChange(Sender: TObject;Changes: TSynStatusChanges);
    procedure cboQuickColorSelect(Sender: TObject);
    procedure CppEditSpecialLineColors(Sender: TObject; Line: Integer;var Special: Boolean; var FG, BG: TColor);
    procedure tbCompletionDelayChange(Sender: TObject);
    procedure chkEnableCompletionClick(Sender: TObject);
    procedure chkEnableClassBrowserClick(Sender: TObject);
    procedure btnSaveSyntaxClick(Sender: TObject);
    procedure chkCBUseColorsClick(Sender: TObject);
    procedure btnCCCnewClick(Sender: TObject);
    procedure btnCCCdeleteClick(Sender: TObject);
    procedure chkCCCacheClick(Sender: TObject);
    procedure CppParser1StartParsing(Sender: TObject);
    procedure CppParser1EndParsing(Sender: TObject);
    procedure CppParser1TotalProgress(Sender: TObject; const FileName: string;Total, Current: Integer);
    procedure ClassCodePageChange(Sender: TObject);
    procedure chkCBShowInheritedClick(Sender: TObject);
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
    procedure cpHighColorDefaultSelect(Sender: TObject);
    procedure cpHighColorHint(Sender: TObject; Cell: Integer;var Hint: String);
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
    procedure LoadSampleText;
    procedure GetOptions;
    procedure SaveCodeIns;
    procedure UpdateCIButtons;
    procedure LoadSyntax(const Value: AnsiString);
    procedure FillSyntaxSets;
    procedure FillCCC;
  end;

implementation

uses 
{$IFDEF WIN32}
  shlobj, MultiLangSupport, devcfg, version, utils, CodeInsFrm, datamod, IniFiles, editor,
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

	// Make editors look a similar to main ones
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

	// Color example
	LoadSampleText;

	// Load color themes
	FillSyntaxSets;

	// Read ini file
	GetOptions;

	// Update code insertion
	UpdateCIButtons;
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
  tabClassBrowsing.Caption:=     Lang[ID_EOPT_BROWSERTAB];
  tabCBBrowser.Caption:=         Lang[ID_EOPT_BROWSERTAB];
  tabCBCompletion.Caption:=      Lang[ID_EOPT_COMPLETIONTAB];
  tabAutosave.Caption:=          Lang[ID_EOPT_AUTOSAVETAB];

  // sub tabs
  tabCPInserts.Caption:=         Lang[ID_EOPT_CPINSERTS];
  tabCPDefault.Caption:=         Lang[ID_EOPT_CPDEFAULT];
  tabSymbols.Caption:=           Lang[ID_EOPT_CPSYMBOLS];

// General Tab
  grpEditorOpts.Caption:=        ' '+Lang[ID_EOPT_EDOPTIONS]+' ';
  cbAutoIndent.Caption:=         Lang[ID_EOPT_AUTOINDENT];
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
  cbDoubleLine.Caption:=         Lang[ID_EOPT_DBLCLKLINE];
  cbFindText.Caption:=           Lang[ID_EOPT_FINDTEXT];
  cbSmartScroll.Caption:=        Lang[ID_EOPT_SMARTSCROLL];
  cbHalfPage.Caption:=           Lang[ID_EOPT_HALFPAGE];
  cbScrollHint.Caption:=         Lang[ID_EOPT_SCROLLHINT];
  cbParserHints.Caption:=        Lang[ID_EOPT_PARSERHINTS];
  cbFunctionHint.Caption:=       Lang[ID_EOPT_CLOSEBRACE];

  cbSyntaxHighlight.Caption:=    Lang[ID_EOPT_USESYNTAX];
  lblTabSize.Caption:=           Lang[ID_EOPT_TABSIZE];

  grpMargin.Caption:=            ' '+Lang[ID_EOPT_MARGIN]+' ';
  cbMarginVis.Caption:=          Lang[ID_EOPT_GENERICENABLED];
  lblMarginWidth.Caption:=       Lang[ID_EOPT_WIDTH];
  lblMarginColor.Caption:=       Lang[ID_EOPT_COLOR];
  HighCurLineBox.Caption:=       Lang[ID_EOPT_HIGHCURLINE];
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
  lvCodeIns.Columns[0].Caption:= Lang[ID_EOPT_CIMENU];
  lvCodeIns.Columns[1].Caption:= Lang[ID_EOPT_CISECTION];
  lvCodeIns.Columns[2].Caption:= Lang[ID_EOPT_CIDESC];
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
  lblCompletionDelay.Caption:=   Lang[ID_EOPT_COMPLETIONDELAY];
  lblCompletionColor.Caption:=   Lang[ID_EOPT_COMPLETIONCOLOR];

// Class browsing Tab
  gbCBEngine.Caption:=           ' '+Lang[ID_EOPT_BROWSERENGINE]+' ';
  gbCBView.Caption:=             ' '+Lang[ID_EOPT_BROWSERVIEW]+' ';
  chkEnableClassBrowser.Caption:=Lang[ID_EOPT_BROWSERENABLE];
  lblClassBrowserSample.Caption:=Lang[ID_EOPT_BROWSERSAMPLE];
  chkCBParseLocalH.Caption:=     Lang[ID_EOPT_BROWSERLOCAL];
  chkCBParseGlobalH.Caption:=    Lang[ID_EOPT_BROWSERGLOBAL];
  chkCBUseColors.Caption:=       Lang[ID_POP_USECOLORS];
  chkCBShowInherited.Caption:=   Lang[ID_POP_SHOWINHERITED];
  chkCCCache.Caption:=           Lang[ID_EOPT_CCCACHECHECK];
  btnCCCnew.Caption:=            Lang[ID_BTN_ADD];
  btnCCCdelete.Caption:=         Lang[ID_BTN_CLEAR];

  btnAdd.Caption:=               Lang[ID_BTN_ADD];
  btnEdit.Caption:=              Lang[ID_BTN_EDIT];
  btnRemove.Caption:=            Lang[ID_BTN_REMOVE];

  ScrollHint.Caption:=           Lang[ID_EOPT_CTRLSCROLLHINT];

	if MinutesDelay.Position = 1 then
		SaveInterval.Caption := Lang[ID_EOPT_AUTOSAVEINTERNAL] + ' ' + IntToStr(MinutesDelay.Position) + ' minute'
	else
		SaveInterval.Caption := Lang[ID_EOPT_AUTOSAVEINTERNAL] + ' ' + IntToStr(MinutesDelay.Position) + ' minutes';

	lblCompletionDelay.Caption := Lang[ID_EOPT_COMPLETIONDELAY] + ' ' + IntToStr(tbCompletionDelay.Position)+' ms';

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
end;

procedure TEditorOptForm.LoadSampleText;
begin
	CppEdit.Lines.BeginUpdate;
	with cppEdit.Lines do begin
		Add('#include <iostream>');
		Add('#include <conio.h>');
		Add('');
		Add('int main(int argc, char **argv)');
		Add('{');
		Add('	int numbers[20];');
		Add('	float average, total; //breakpoint');
		Add('	for (int i = 0; i <= 19; i++)');
		Add('	{ // active breakpoint');
		Add('		numbers[i] = i;');
		Add('		Total += i; // error line');
		Add('	}');
		Add('	average = total / 20;');
		Add('	cout << numbers[0] << "\n" << numbers[19] << "\n";');
		Add('	cout << "total: " << total << "\nAverage: " << average;');
		Add('	getch();');
		Add('}');
	end;
	CppEdit.Lines.EndUpdate;
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
     cbInsertMode.Checked:=          InsertMode;
     cbUseTabs.Checked:=             UseTabs;
     cbSmartTabs.Checked:=           SmartTabs;
     cbGroupUndo.Checked:=           GroupUndo;
     cbEHomeKey.Checked:=            EHomeKey;
     cbPastEOF.Checked:=             PastEOF;
     cbPastEOL.Checked:=             PastEOL;
     cbDoubleLine.Checked:=          DblClkLine;
     cbFindText.Checked:=            FindText;
     cbSmartScroll.Checked:=         Scrollbars;
     cbHalfPage.Checked:=            HalfPageScroll;
     cbScrollHint.Checked:=          ScrollHint;
     cbSpecialChars.Checked:=        SpecialChars;
     cbFunctionHint.Checked:=        ShowFunctionTip;
     cbTrimTrailingSpaces.Checked:=  TrimTrailingSpaces;

     cbMarginVis.Checked:=           MarginVis;
     edMarginWidth.Value:=           MarginSize;
     cpMarginColor.SelectionColor:=  MarginColor;

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
     cpHighColor.SelectionColor :=   HighColor;
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

	if FileExists(devDirs.Config + DEV_DEFAULTCODE_FILE) then begin
		seDefault.Lines.LoadFromFile(devDirs.Config + DEV_DEFAULTCODE_FILE);
	end;

	// CODE_COMPLETION
	chkEnableCompletion.OnClick:=nil;
	chkEnableCompletion.Checked:=devCodeCompletion.Enabled;
	chkEnableCompletion.OnClick:=chkEnableCompletionClick;
	tbCompletionDelay.Position:=devCodeCompletion.Delay;
	cpCompletionBackground.SelectionColor:=devCodeCompletion.BackColor;
	tbCompletionDelay.Enabled:=chkEnableCompletion.Checked;
	cpCompletionBackground.Enabled:=chkEnableCompletion.Checked;
	chkCCCache.Checked:=devCodeCompletion.UseCacheFiles;
	chkCCCache.Tag:=0; // mark un-modified
	chkCCCache.Enabled:=chkEnableCompletion.Checked;
	lbCCC.Enabled:=chkCCCache.Checked and chkEnableCompletion.Checked;
	btnCCCnew.Enabled:=chkCCCache.Checked and chkEnableCompletion.Checked;
	btnCCCdelete.Enabled:=chkCCCache.Checked and chkEnableCompletion.Checked;

	// Class browsing
	chkEnableClassBrowser.Checked:=devClassBrowsing.Enabled;
	ClassBrowser.Enabled:=chkEnableClassBrowser.Checked;
	ClassBrowser.UseColors:=devClassBrowsing.UseColors;
	ClassBrowser.ShowInheritedMembers:=devClassBrowsing.ShowInheritedMembers;
	ClassBrowser.ShowSampleData; // Class browsing page preview
	chkCBParseLocalH.Checked:= devClassBrowsing.ParseLocalHeaders;
	chkCBParseGlobalH.Checked:= devClassBrowsing.ParseGlobalHeaders;
	chkCBParseLocalH.Enabled:= chkEnableClassBrowser.Checked;
	chkCBParseGlobalH.Enabled:= chkEnableClassBrowser.Checked;
	chkCBUseColors.Checked:= devClassBrowsing.UseColors;
	chkCBShowInherited.Checked:= devClassBrowsing.ShowInheritedMembers;
	chkCBUseColors.Enabled:= chkEnableClassBrowser.Checked;
	chkEnableCompletion.Enabled:=chkEnableClassBrowser.Checked;
	tbCompletionDelay.Enabled:=chkEnableClassBrowser.Checked;
	cpCompletionBackground.Enabled:=chkEnableClassBrowser.Checked;

	// Autosave
	MinutesDelay.Position := devEditor.Interval;
	FileOptions.ItemIndex := devEditor.AutoSaveFilter;
	NameOptions.ItemIndex := devEditor.AutoSaveMode;
	cbAutoSave.Checked := devEditor.EnableAutoSave;

	MinutesDelay.Enabled := cbAutoSave.Checked;
	SaveInterval.Enabled := cbAutoSave.Checked;
	FileOptions.Enabled := cbAutoSave.Checked;
	OptionsGroup.Enabled := cbAutoSave.Checked;
	NameOptions.Enabled := cbAutoSave.Checked;

	SetGutter;
end;

procedure TEditorOptForm.btnOkClick(Sender: TObject);
var
 s, aName: AnsiString;
 a, idx: integer;
 e : TEditor;
begin
	with devEditor do begin
		AutoIndent:=          cbAutoIndent.Checked;
		InsertMode:=          cbInsertMode.Checked;
		UseTabs:=             cbUseTabs.Checked;
		SmartTabs:=           cbSmartTabs.Checked;
		GroupUndo:=           cbGroupUndo.Checked;
		EHomeKey:=            cbEHomeKey.Checked;
		PastEOF:=             cbPastEOF.Checked;
		PastEOL:=             cbPastEOL.Checked;
		DblClkLine:=          cbDoubleLine.Checked;
		FindText:=            cbFindText.Checked;
		Scrollbars:=          cbSmartScroll.Checked;
		HalfPageScroll:=      cbHalfPage.Checked;
		ScrollHint:=          cbScrollHint.Checked;
		SpecialChars:=        cbSpecialChars.Checked;
		ShowFunctionTip:=     cbFunctionHint.Checked;
		TrimTrailingSpaces:=  cbTrimTrailingSpaces.Checked;

		MarginVis:=           cbMarginVis.Checked;
		MarginSize:=          edMarginWidth.Value;
		MarginColor:=         cpMarginColor.SelectionColor;
		InsertCaret:=         cboInsertCaret.ItemIndex;
		OverwriteCaret:=      cboOverwriteCaret.ItemIndex;
		Match:=               cbMatch.Checked;

		HighCurrLine:=        cbHighCurrLine.Checked;
		HighColor:=           cpHighColor.SelectionColor;

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

	devCodeCompletion.Enabled:=chkEnableCompletion.Checked;
	devCodeCompletion.Delay:=tbCompletionDelay.Position;
	devCodeCompletion.BackColor:=cpCompletionBackground.SelectionColor;
	devCodeCompletion.UseCacheFiles:=chkCCCache.Checked;

	devClassBrowsing.Enabled:=chkEnableClassBrowser.Checked;
	devClassBrowsing.ParseLocalHeaders:=chkCBParseLocalH.Checked;
	devClassBrowsing.ParseGlobalHeaders:=chkCBParseGlobalH.Checked;
	devClassBrowsing.UseColors:=chkCBUseColors.Checked;
	devClassBrowsing.ShowInheritedMembers:=chkCBShowInherited.Checked;

	// Autosave
	devEditor.EnableAutoSave := cbAutoSave.Checked;
	devEditor.Interval := MinutesDelay.Position;
	devEditor.AutoSaveFilter := FileOptions.ItemIndex;
	devEditor.AutoSaveMode := NameOptions.ItemIndex;

	// Properly configure the timer object
	if not devEditor.EnableAutoSave then begin

		// Delete the timer when we don't need it anymore
		if Assigned(MainForm.AutoSaveTimer) then
			FreeAndNil(MainForm.AutoSaveTimer);

	end else begin

		// Create the timer when we changed the enable option
		if not Assigned(MainForm.AutoSaveTimer) then
			MainForm.AutoSaveTimer := TTImer.Create(Self);

		// And set corresponding options
		MainForm.AutoSaveTimer.Interval := devEditor.Interval*60*1000; // miliseconds to minutes
		MainForm.AutoSaveTimer.Enabled := devEditor.EnableAutoSave;
		MainForm.AutoSaveTimer.OnTimer := MainForm.EditorSaveTimer;
	end;

	SaveOptions;
	dmMain.LoadDataMod;

	e := MainForm.GetEditor;
	if Assigned(e) then begin

		// Unpaint matching symbols
		if not devEditor.Match then
			e.PaintMatchingBrackets(ttBefore);

		// Repaint highlighted line
		if cbHighCurrLine.Checked then
			e.Text.ActiveLineColor := cpHighColor.SelectionColor
		else
			e.Text.ActiveLineColor := clNone;
	end;
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
	if ElementList.ItemIndex> pred(cpp.AttrCount) then begin
		fUpdate:= FALSE;

		cpBackground.Enabled := True;

		if CompareText(ElementList.Items[ElementList.ItemIndex], cSel) = 0 then
			pt:= fSelColor
		else if CompareText(ElementList.Items[ElementList.ItemIndex], cBP) = 0 then
			pt:= fBPColor
		else if CompareText(ElementList.Items[ElementList.ItemIndex], cErr) = 0 then
			pt:= fErrColor
		else if CompareText(ElementList.Items[ElementList.ItemIndex], cABP) = 0 then
			pt:= fABPColor
		else if CompareText(ElementList.Items[ElementList.ItemIndex], cGut) = 0 then
			pt:= fGutColor
		else if CompareText(ElementList.Items[ElementList.ItemIndex], cFld) = 0 then begin
			pt:= fFoldColor;
			cpBackground.Enabled := false;
		end;

		cpBackground.SelectionColor:= pt.x;
		cpForeground.SelectionColor:= pt.y;

		cbBold.Checked:= False;
		cbItalic.Checked:= False;
		cbUnderlined.Checked:= False;

		cbBold.Enabled := False;
		cbItalic.Enabled:= False;
		cbUnderlined.Enabled:= False;

		fUpdate:= TRUE;
	end else if ElementList.ItemIndex > -1 then
		with Cpp.Attribute[ElementList.ItemIndex] do begin
			fUpdate:= FALSE;
			if Foreground = clNone then
				cpForeground.SelectionColor:= clWindowText //clNone
			else
				cpForeground.SelectionColor:= Foreground;
			if Background = clNone then
				cpBackground.SelectionColor:= clWindow //clNone
			else
				cpBackground.SelectionColor:= Background;

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

procedure TEditorOptForm.DefaultSelect(Sender: TObject);
begin
	TColorPickerButton(Sender).SelectionColor:= clNone;
end;

procedure TEditorOptForm.PickerHint(Sender: TObject; Cell: integer; var Hint: AnsiString);
begin
	if Cell = DEFAULTCELL then
		Hint:= Lang[ID_EOPT_HINTWHITESPACE];
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
		pt.x:= cpBackground.SelectionColor;
		pt.y:= cpForeground.SelectionColor;

		// use local AnsiString just to ease readability
		s:= ElementList.Items[ElementList.ItemIndex];

		// if either value is clnone set to Whitespace color values
		if pt.x = clNone then pt.x:= fbgColor;
		if pt.y = clNone then pt.y:= ffgColor;
		if CompareText(s, cSel) = 0 then
			fSelColor:= pt
		else if CompareText(s, cBP) = 0 then
			fBPColor:= pt
		else if CompareText(s, cABP) = 0 then
			fABPColor:= pt
		else if CompareText(s, cerr) = 0 then
			fErrColor:= pt
		else if CompareText(s, cGut) = 0 then begin
			fGutColor:= pt;
			SetGutter;
		end else if CompareText(s, cFld) = 0 then begin
			fFoldColor:= pt;
			SetGutter;
		end;
	end else begin
     Attr:= TSynHighlighterAttributes.Create(ElementList.Items[ElementList.ItemIndex]);
     Attr.Assign(cpp.Attribute[ElementList.ItemIndex]);
     with Attr do
      try
       Foreground:= cpForeground.SelectionColor;
       if Sender = cpBackground then
        Background:= cpBackground.SelectionColor;
       if CompareText(Name, 'WhiteSpace') = 0 then
        begin
          ffgColor:= Foreground;
          fbgColor:= Background;
        end;
       Style:= [];
       if cbBold.checked then Style:= Style +[fsBold];
       if cbItalic.Checked then Style:= Style +[fsItalic];
       if cbUnderlined.Checked then Style:= Style +[fsUnderline];
       cpp.Attribute[ElementList.ItemIndex].Assign(Attr);
      finally
       Free;
      end;
   end;

	// invalidate special lines
	cppEdit.Repaint;
	cboQuickColor.ItemIndex:=-1;
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

procedure TEditorOptForm.cpMarginColorDefaultSelect(Sender: TObject);
begin
	cpMarginColor.SelectionColor:= cl3DLight;
end;

procedure TEditorOptForm.cpMarginColorHint(Sender: TObject; Cell: Integer;var Hint: AnsiString);
begin
	if Cell = DEFAULTCELL then
		Hint:= Lang[ID_EOPT_HINTHIGHLIGHT];
end;

procedure TEditorOptForm.cpHighColorDefaultSelect(Sender: TObject);
begin
	cpHighColor.SelectionColor:= $FFFFCC;
end;

procedure TEditorOptForm.cpHighColorHint(Sender: TObject; Cell: Integer;var Hint: String);
begin
	if Cell = DEFAULTCELL then
		Hint:= Lang[ID_EOPT_HINTWHITESPACE];
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
	if cboQuickColor.ItemIndex > 9 then begin
		// custom style; load from disk
		LoadSyntax(cboQuickColor.Items[cboQuickColor.ItemIndex]);
		Exit;
	end;

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

	cppEdit.InvalidateLine(cSelection);
	cppEdit.InvalidateLine(cBreakLine);
	cppEdit.InvalidateLine(cABreakLine);
	cppEdit.InvalidateLine(cErrorLine);

	SetGutter;
end;

{ ---------- Code insert's methods ---------- }
procedure TEditorOptForm.btnAddClick(Sender: TObject);
var
	NewItem: PCodeIns;
	Item: TListItem;
begin
	with TfrmCodeEdit.Create(Self) do
		try
			CodeIns.ClearAll;
			Edit:= FALSE;
			New(NewItem);
			NewItem^.Sep:= 0;
			Entry:= NewItem;
			if ShowModal = mrOk then begin
				Item:= lvCodeIns.Items.Add;
				Item.Caption:= edMenuText.Text;
				Item.SubItems.Add(inttostr(seSection.Value));
				Item.SubItems.Add(edDesc.Text);
				Item.SubItems.Add('');
				lvCodeIns.Selected:=Item;
			end else
				dispose(NewItem);
		finally
			Free;
			UpdateCIButtons;
		end;
end;

procedure TEditorOptForm.btnEditClick(Sender: TObject);
begin
  with TfrmCodeEdit.Create(Self) do
   try
    Edit:= TRUE;
    edMenuText.Text:= lvCodeIns.Selected.Caption;
    seSection.Value:= strtoint(lvCodeIns.Selected.SubItems[0]);
    edDesc.Text:= lvCodeIns.Selected.SubItems[1];
    if ShowModal = mrOk then
     begin
       lvCodeIns.Selected.Caption:= edMenuText.Text;
       lvCodeIns.Selected.SubItems[0]:= inttostr(seSection.Value);
       lvCodeIns.Selected.SubItems[1]:= edDesc.Text;
     end;
   finally
    Free;
    UpdateCIButtons;
   end;
end;

procedure TEditorOptForm.btnRemoveClick(Sender: TObject);
begin
  dmMain.CodeInserts.Delete(lvCodeIns.Selected.Index);
  lvCodeIns.Selected.Delete;
  CodeIns.ClearAll;
  UpdateCIButtons;
end;

procedure TEditorOptForm.UpdateCIButtons;
begin
  btnAdd.Enabled:= TRUE;
  btnEdit.Enabled:= assigned(lvCodeIns.Selected);
  btnRemove.Enabled:= assigned(lvCodeIns.Selected);
end;

procedure TEditorOptForm.lvCodeinsColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  lvCodeIns.AlphaSort;
end;

procedure TEditorOptForm.lvCodeinsCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
var
 i1, i2: integer;
begin
  i1:= strtoint(Item1.SubItems[0]);
  i2:= strtoint(Item2.SubItems[0]);
  if i1> i2 then
   Compare:= 1
  else
   if i1 = i2 then
    Compare:= 0
   else
    Compare:= -1;
end;

procedure TEditorOptForm.lvCodeinsSelectItem(Sender: TObject;Item: TListItem; Selected: Boolean);
begin
	Codeins.ClearAll;
	CodeIns.Text:= StrtoCodeIns(Item.SubItems[2]);
	UpdateCIButtons;
end;

procedure TEditorOptForm.CodeInsStatusChange(Sender: TObject;Changes: TSynStatusChanges);
begin
	if assigned(lvCodeIns.Selected) then
		if (scModified in Changes) then begin
			lvCodeIns.Selected.SubItems[2]:= CodeInstoStr(CodeIns.Text);
			CodeIns.Modified:= false;
		end;
end;

procedure TEditorOptForm.LoadCodeIns;
var
 idx: integer;
 Item: TListItem;
 Ins: PCodeIns;
begin
	for idx:= 0 to pred(dmMain.CodeInserts.Count) do begin
		Item:= lvCodeIns.Items.Add;
		ins:= dmMain.CodeInserts[idx];
		Item.Caption:= ins.Caption;
		Item.SubItems.Add(inttostr(ins.Sep));
		Item.SubItems.Add(ins.Desc);
		Item.SubItems.Add(ins.Line);
	end;
	if lvCodeIns.Items.Count> 0 then
		lvCodeIns.ItemIndex:= 0;
end;

procedure TEditorOptForm.SaveCodeIns;
var
 idx: integer;
 Item: PCodeIns;
begin
  lvCodeIns.AlphaSort;
  for idx:= 0 to dmMain.CodeInserts.Count - 1 do
    Dispose(dmMain.CodeInserts.Items[idx]);
  dmMain.CodeInserts.Clear;
  for idx:= 0 to pred(lvCodeIns.Items.Count) do
   begin
     new(Item);
     Item.Caption:= lvCodeIns.Items[idx].Caption;
     Item.Sep:= strtoint(lvCodeIns.Items[idx].SubItems[0]);
     Item.Desc:= lvcodeIns.Items[idx].SubItems[1];
     Item.Line:= lvCodeIns.Items[idx].SubItems[2];
     dmMain.CodeInserts.AddItem(Item);
   end;
  dmMain.Codeinserts.SaveCode;
end;

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

procedure TEditorOptForm.chkEnableClassBrowserClick(Sender: TObject);
begin
  // browser
  ClassBrowser.Enabled:=chkEnableClassBrowser.Checked;
  chkCBParseLocalH.Enabled:= chkEnableClassBrowser.Checked;
  chkCBParseGlobalH.Enabled:= chkEnableClassBrowser.Checked;
  chkCBUseColors.Enabled:= chkEnableClassBrowser.Checked;
  // completion
  chkEnableCompletion.Enabled:=chkEnableClassBrowser.Checked;
  tbCompletionDelay.Enabled:=chkEnableClassBrowser.Checked;
  cpCompletionBackground.Enabled:=chkEnableClassBrowser.Checked;
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

procedure TEditorOptForm.chkCBUseColorsClick(Sender: TObject);
begin
  ClassBrowser.UseColors:=chkCBUseColors.Checked;
  ClassBrowser.Refresh;
end;

procedure TEditorOptForm.btnCCCnewClick(Sender: TObject);
var
  I, I1: integer;
  Hits: integer;
  MaxHits, MaxIndex: integer;
  sl: TStrings;
  flt: AnsiString;
begin
  // the following piece of code is a quick'n'dirty way to find the base
  // compiler's include dir (if we 're lucky).
  // we search through devDirs.C and try to locate the base dir that is
  // most common between the others(!).
  // if no most-common dir is found, we select the first in list.
  // for a default installation, it should work.
  //
  // will be replaced by a dialog ( when it's ready ;) to let the user
  // select, so that he gets the blame if the thing does not work ;)))
  //
  // PS: is there a better way to do it???
  sl:=TStringList.Create;
  try
    sl.Delimiter:=';';
    sl.DelimitedText:=devCompiler.CppDir;
    if sl.Count>1 then begin
      MaxHits:=0;
      MaxIndex:=0;
      for I1:=0 to sl.Count-1 do begin
        Hits:=0;
        for I:=0 to sl.Count-1 do
          if StartsText(sl[I1], sl[I]) then
            Inc(Hits);
        if Hits>MaxHits then begin
          MaxHits:=Hits;
          MaxIndex:=I1;
        end;
      end;
      CppParser.ProjectDir:=IncludeTrailingPathDelimiter(sl[MaxIndex]);
    end
    else
      CppParser.ProjectDir:=IncludeTrailingPathDelimiter(devCompiler.CppDir);
  finally
    sl.Free;
  end;

	with dmMain do begin
		BuildFilter(flt, [FLT_HEADS]);
		OpenDialog.Filter:=flt;

		if OpenDialog.Execute then begin
			Screen.Cursor:=crHourglass;
			Application.ProcessMessages;

			for I:=0 to OpenDialog.Files.Count-1 do
				CppParser.AddFileToScan(OpenDialog.Files[I]);
			CppParser.ParseList;
			CppParser.Save(devDirs.Config + DEV_COMPLETION_CACHE,devDirs.Exec);

			lbCCC.Items.BeginUpdate;
			lbCCC.Clear;
			for I := 0 to CppParser.CacheContents.Count - 1 do
				lbCCC.Items.Add(ReplaceFirststr(CppParser.CacheContents[i],devDirs.Exec,'.\'));
			lbCCC.Items.EndUpdate;

			Screen.Cursor:=crDefault;
			chkCCCache.Tag:=1; // mark modified
		end;
	end;
end;

procedure TEditorOptForm.btnCCCdeleteClick(Sender: TObject);
var
	I : integer;
begin
	if lbCCC.Items.Count=0 then
		Exit;

	if MessageDlg('Are you sure you want to clear the cache?', mtConfirmation, [mbYes, mbNo], 0)=mrYes then begin
		DeleteFile(devDirs.Config+DEV_COMPLETION_CACHE);

		FreeAndNil(CppParser);
		CppParser:=TCppParser.Create(Self);
		CppParser.Tokenizer:=CppTokenizer;
		CppParser.ParseLocalHeaders:=True;
		CppParser.ParseGlobalHeaders:=True;
		CppParser.OnStartParsing:=CppParser1StartParsing;
		CppParser.OnEndParsing:=CppParser1EndParsing;
		CppParser.OnTotalProgress:=CppParser1TotalProgress;

		lbCCC.Items.BeginUpdate;
		lbCCC.Clear;
		for I := 0 to CppParser.CacheContents.Count - 1 do
			lbCCC.Items.Add(ReplaceFirststr(CppParser.CacheContents[i],devDirs.Exec,'.\'));
		lbCCC.Items.EndUpdate;

		chkCCCache.Tag:=1; // mark modified
	end;
end;

procedure TEditorOptForm.FillCCC;
var
	I : integer;
begin
	Screen.Cursor:=crHourglass;
	Application.ProcessMessages;

	CppParser.Load(devDirs.Config+DEV_COMPLETION_CACHE,devDirs.Exec);

	lbCCC.Items.BeginUpdate;
	lbCCC.Clear;
	for I := 0 to CppParser.CacheContents.Count - 1 do
		lbCCC.Items.Add(ReplaceFirststr(CppParser.CacheContents[i],devDirs.Exec,'.\'));
	lbCCC.Items.EndUpdate;

	Screen.Cursor:=crDefault;
end;

procedure TEditorOptForm.chkCCCacheClick(Sender: TObject);
begin
  chkCCCache.Tag:=1; // mark modified
  lbCCC.Enabled:=chkCCCache.Checked;
  btnCCCnew.Enabled:=chkCCCache.Checked;
  btnCCCdelete.Enabled:=chkCCCache.Checked;
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

procedure TEditorOptForm.ClassCodePageChange(Sender: TObject);
begin
	if (ClassCodePage.ActivePage=tabCBCompletion) and (CppParser.Statements.Count=0) then
		FillCCC;
end;

procedure TEditorOptForm.chkCBShowInheritedClick(Sender: TObject);
begin
  ClassBrowser.ShowInheritedMembers:=chkCBShowInherited.Checked;
  ClassBrowser.Refresh;
end;

procedure TEditorOptForm.OnGutterClick(Sender: TObject;
  Button: TMouseButton; X, Y, Line: Integer; Mark: TSynEditMark);
var
 idx: integer;
begin
  idx:= ElementList.Items.IndexOf(cGut);
  if idx <> -1 then
   begin
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

end.
