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

unit LangFrm;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, Menus, FileCtrl, SynEdit, ToolWin, ComCtrls;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QButtons, QExtCtrls, QMenus, QComCtrls;
{$ENDIF}

type
  TLangForm = class(TForm)
    OkBtn: TBitBtn;
    LangPanel: TPanel;
    lbLanguages: TListBox;
    grpLanguages: TGroupBox;
    lblLangInfo: TLabel;
    grpThemes: TGroupBox;
    cmbIcons: TComboBox;
    CachePanel: TPanel;
    CacheInfo1: TLabel;
    BuildPanel: TPanel;
    YesCache: TRadioButton;
    NoCache: TRadioButton;
    ProgressPanel: TPanel;
    pbCCCache: TProgressBar;
    ParseLabel: TLabel;
    FinishPanel: TPanel;
    Finish2: TLabel;
    Finish3: TLabel;
    AltCache: TRadioButton;
    AltFileList: TListBox;
    CacheInfo2: TLabel;
    ButtonAddFile: TButton;
    ButtonRemove: TButton;
    ButtonAddFolder: TButton;
    cmbColors: TComboBox;
    lblIcons: TLabel;
    lblColor: TLabel;
    Finish1: TLabel;
    synExample: TSynEdit;
    EditPanel: TPanel;
    lblEditInfo: TLabel;
    lblFont: TLabel;
    cmbFont: TComboBox;
    tbExample: TToolBar;
    NewFileBtn: TToolButton;
    OpenBtn: TToolButton;
    SaveUnitBtn: TToolButton;
    SaveAllBtn: TToolButton;
    CloseBtn: TToolButton;
    PrintBtn: TToolButton;
    UndoBtn: TToolButton;
    RedoBtn: TToolButton;
    FindBtn: TToolButton;
    ReplaceBtn: TToolButton;
    FindNextBtn: TToolButton;
    GotoLineBtn: TToolButton;
    CompileBtn: TToolButton;
    RunBtn: TToolButton;
    CompileAndRunBtn: TToolButton;
    RebuildAllBtn: TToolButton;
    DebugBtn: TToolButton;
    ProfileBtn: TToolButton;
    ProfilingInforBtn: TToolButton;
    procedure OkBtnClick(Sender: TObject);
    procedure ColorChange(Sender: TObject);
    procedure ButtonAddFileClick(Sender: TObject);
    procedure ButtonRemoveClick(Sender: TObject);
    procedure ButtonAddFolderClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FontChange(Sender: TObject);
    procedure cmbIconsChange(Sender: TObject);
    procedure cmbFontDrawItem(Control: TWinControl; Index: Integer;Rect: TRect; State: TOwnerDrawState);
    function GetSelected: integer;
    procedure CppParserTotalProgress(Sender: TObject; const FileName: string; Total, Current: Integer);
    procedure HandleLangPanel;
    procedure HandleEditPanel;
    procedure HandleCachePanel;
  public
    procedure LoadText; // call after selecting a language of course
    procedure UpdateList(List: TStrings);
    property Selected: integer read GetSelected;
  end;

implementation

uses
  MultiLangSupport, datamod, devcfg, utils, main, version, ImageTheme, SynEditTypes;

{$R *.dfm}

procedure TLangForm.LoadText;
begin
	grpThemes.Caption := Lang[ID_LANGFORM_SELECTTHEME];
	lblFont.Caption := Lang[ID_LANGFORM_FONT];
	lblColor.Caption := Lang[ID_LANGFORM_COLOR];
	lblIcons.Caption := Lang[ID_LANGFORM_ICONS];
	lblEditInfo.Caption := Lang[ID_LANGFORM_THEMCHANGEHINT];
	CacheInfo1.Caption := Lang[ID_LANGFORM_OPTCODECOMPL1];
	CacheInfo2.Caption := Lang[ID_LANGFORM_OPTCODECOMPL2];
	YesCache.Caption := Lang[ID_LANGFORM_CACHEALL];
	AltCache.Caption := Lang[ID_LANGFORM_CACHESEL];
	NoCache.Caption := Lang[ID_LANGFORM_CACHENONE];
	ButtonAddFile.Caption := Lang[ID_LANGFORM_ADDFILE];
	ButtonAddFolder.Caption := Lang[ID_LANGFORM_ADDFOLDER];
	ButtonRemove.Caption := Lang[ID_LANGFORM_REMOVE];
	ParseLabel.Caption := Lang[ID_LANGFORM_PARSING];
	Finish1.Caption := Lang[ID_LANGFORM_FINISH1];
	Finish2.Caption := Lang[ID_LANGFORM_FINISH2];
	Finish3.Caption := Lang[ID_LANGFORM_FINISH3];
	OkBtn.Caption := Lang[ID_LANGFORM_NEXT];
end;

procedure TLangForm.UpdateList(List: TStrings);
var
	I, sel: integer;
begin
	lbLanguages.Items.BeginUpdate;
	lbLanguages.Clear;
	for I := 0 to List.Count - 1 do begin
		sel := lbLanguages.Items.Add(List.ValueFromIndex[I]);
		if StartsText('english', lbLanguages.Items[sel]) then
			lbLanguages.Selected[sel] := True;
	end;
	lbLanguages.Items.EndUpdate;
end;

function TLangForm.GetSelected: integer;
begin
	result := lbLanguages.ItemIndex;
end;

procedure TLangForm.CppParserTotalProgress(Sender: TObject; const FileName: string; Total, Current: Integer);
begin
	if Total <> -1 then
		pbCCCache.Max := Total;
	if Current <> -1 then
		pbCCCache.Position := Current + 1; // Current is 0-based
	ParseLabel.Caption := Lang[ID_LANGFORM_PARSING] + #13#10 + WrapText(ReplaceFirstText(FileName,devDirs.Exec,''),sLineBreak,['\'],45);
	Application.ProcessMessages;
end;

procedure TLangForm.HandleLangPanel;
begin
	OkBtn.Tag := 1;
	LangPanel.Visible := false;

	// Update translation
	if Selected <> -1 then begin
		Lang.Open(Lang.Langs.Names[Selected]);
		devData.Language := Lang.FileFromDescription(Lang.Langs.Names[Selected]);
	end else begin
		Lang.Open('English.lng'); // never happens...
	end;
	LoadText;

	EditPanel.Visible := true;
end;

procedure TLangForm.HandleEditPanel;
begin
	OkBtn.Tag := 2;
	EditPanel.Visible := false;
	CachePanel.Visible := true;
	devData.ThemeChange := true;
	devData.Theme := cmbIcons.Items[cmbIcons.ItemIndex];
end;

procedure TLangForm.HandleCachePanel;
var
	sl,f : TStringList;
	I, J : integer;
	S : AnsiString;
begin
	if YesCache.Checked or AltCache.Checked then begin
		YesCache.Enabled := false;
		NoCache.Enabled := false;
		AltCache.Enabled := false;
		AltFileList.Enabled := false;
		OkBtn.Enabled := false;
		BuildPanel.Visible := False;
		ProgressPanel.Visible := True;
		OkBtn.Caption := Lang[ID_LANGFORM_WAIT];
		devCodeCompletion.Enabled := true;
		devCodeCompletion.UseCacheFiles := true;
		devCodeCompletion.Enabled := true;
		devCodeCompletion.ParseLocalHeaders := true;
		devCodeCompletion.ParseGlobalHeaders := true;
		SaveOptions;

		MainForm.CppParser.Tokenizer := MainForm.CppTokenizer;
		MainForm.CppParser.Preprocessor := MainForm.CppPreprocessor;
		MainForm.CppParser.ParseLocalHeaders := True;
		MainForm.CppParser.ParseGlobalHeaders := True;
		MainForm.CppParser.OnTotalProgress := CppParserTotalProgress;
		MainForm.CppParser.OnStartParsing := nil;
		MainForm.CppParser.OnEndParsing := nil;
		MainForm.CppParser.Enabled := true;

		MainForm.ClassBrowser.BeginUpdate;

		sl := TStringList.Create;
		if AltCache.Checked then begin
			for I := 0 to AltFileList.Count - 1 do
				sl.Add(AltFileList.Items[I]);
		end else if Assigned(devCompilerSets.CurrentSet) then // cache all include dirs if there are any
			sl.Assign(devCompilerSets.CurrentSet.CppDir);

		// Make it look busy
		Screen.Cursor:=crHourglass;

		// Add default include search directories if a compiler has been installed
		if Assigned(devCompilerSets.CurrentSet) then begin
			with devCompilerSets.CurrentSet do begin
				for I := 0 to CDir.Count - 1 do
					MainForm.CppParser.AddIncludePath(CDir[I]);
				for I := 0 to CppDir.Count - 1 do
					MainForm.CppParser.AddIncludePath(CppDir[I]);

				// Add default include dirs last, just like gcc does
				for I := 0 to DefInclude.Count - 1 do
					MainForm.CppParser.AddIncludePath(DefInclude[I]);
			end;
		end;

		f := TStringList.Create;
		if not AltCache.Checked then begin
			for i := 0 to sl.Count-1 do begin

				// Relative paths make the recursive/loop searcher go nuts
				sl[i] := ReplaceFirstStr(sl[i],'%path%\',devDirs.exec);
				if DirectoryExists(sl[i]) then begin
					FilesFromWildcard(sl[i], '*.*', f, false, false, false);
					for j := 0 to f.Count - 1 do
						MainForm.CppParser.AddFileToScan(f[j]);
				end;
			end;
		end else begin
			for i := 0 to sl.Count-1 do begin
				// only pass full filenames
				S := MainForm.CppParser.GetSystemHeaderFileName(sl[i]);
				MainForm.CppParser.AddFileToScan(S);
			end;
		end;
		sl.Free;
		f.Free;

		// Parse all given files.
		MainForm.CppParser.ParseList;

		// When done, save files
		ParseLabel.Caption :=  Lang[ID_LANGFORM_SAVING];
		Application.ProcessMessages;
		MainForm.CppParser.Save(devDirs.Config + DEV_COMPLETION_CACHE,devDirs.Exec);

		MainForm.CppParser.OnStartParsing := MainForm.CppParserStartParsing;
		MainForm.CppParser.OnEndParsing := MainForm.CppParserEndParsing;
		MainForm.CppParser.OnTotalProgress := MainForm.CppParserTotalProgress;

		// Erase ALL memory of the C++ parser
		MainForm.CppParser.Reset(false);
		MainForm.ClassBrowser.EndUpdate; // only mess with class browser once

		Screen.Cursor:=crDefault;
	end else begin
		devCodeCompletion.Enabled := true;
		devCodeCompletion.ParseLocalHeaders := true;
		devCodeCompletion.ParseGlobalHeaders := true; // can be slow without cache
		devClassBrowsing.ShowInheritedMembers := false;
	end;
	OkBtn.Tag := 3;
	OkBtn.Kind := bkOK;
	OkBtn.ModalResult := mrOK;
	OkBtn.Enabled := true;
	CachePanel.Visible := false;
	FinishPanel.Visible := true;
end;

procedure TLangForm.OkBtnClick(Sender: TObject);
begin
	case OkBtn.Tag of
		0: HandleLangPanel;
		1: HandleEditPanel;
		2: HandleCachePanel;
	end;
end;

procedure TLangForm.ButtonAddFileClick(Sender: TObject);
var
	I,J: integer;
	s: AnsiString;
begin
	with TOpenDialog.Create(self) do try
		Filter := BuildFilter([FLT_HEADS]);
		Title := Lang[ID_NV_OPENFILE];
		Options := Options + [ofAllowMultiSelect];

		// Start in the include folder
		FileName := '';
		if Assigned(devCompilerSets.CurrentSet) and (devCompilerSets.CurrentSet.CppDir.Count > 0) then
			InitialDir := devCompilerSets.CurrentSet.CppDir[0];

		if Execute then begin
			for i:= 0 to Files.Count-1 do begin
				s := Files[i];
				if Assigned(devCompilerSets.CurrentSet) then begin
					for J := 0 to devCompilerSets.CurrentSet.CppDir.Count -1 do
						s := StringReplace(s,devCompilerSets.CurrentSet.CppDir[j] + pd,'',[rfReplaceAll]);
				end;
				AltFileList.Items.Add(s);
			end;
		end;
	finally
		Free;
	end;
end;

procedure TLangForm.ButtonRemoveClick(Sender: TObject);
begin
	AltFileList.DeleteSelected;
end;

procedure TLangForm.ButtonAddFolderClick(Sender: TObject);
var
	Dir : AnsiString;
	f : TStringList;
	I,J : integer;
	s : AnsiString;
begin
	f := TStringList.Create;
	try
		if Assigned(devCompilerSets.CurrentSet) and (devCompilerSets.CurrentSet.CppDir.Count > 0) then
			Dir := devCompilerSets.CurrentSet.CppDir[0]
		else if devDirs.Default <> '' then
			Dir := devDirs.Default
		else
			Dir := devDirs.Exec;
		if NewSelectDirectory('Select Folder', '', Dir) then begin
			FilesFromWildcard(Dir, '*.*', f, false, false, false);
			for i := 0 to f.Count-1 do begin
				s := f[i];
				if Assigned(devCompilerSets.CurrentSet) then begin
					for J := 0 to devCompilerSets.CurrentSet.CppDir.Count -1 do
						s := StringReplace(s,devCompilerSets.CurrentSet.CppDir[j] + pd,'',[rfReplaceAll]);
				end;
				AltFileList.Items.Add(s);
			end;
		end;
	finally
		f.Free;
	end;
end;

procedure TLangForm.FormShow(Sender: TObject);
begin
	// Set interface font
	Font.Name := devData.InterfaceFont;
	Font.Size := devData.InterfaceFontSize;

	synExample.CaretXY := BufferCoord(11,5);

	// Interface themes
	devImageThemes.GetThemeTitles(cmbIcons.Items);
	cmbIcons.ItemIndex := 0; // new look

	// Editor colors
	cmbColors.ItemIndex := 1; // Classic Plus
	dmMain.InitHighlighterFirstTime(cmbColors.ItemIndex);
	devEditor.AssignEditor(synExample,'main.cpp');

	// Font options
	cmbFont.Items.Assign(Screen.Fonts);
	cmbFont.ItemIndex := cmbFont.Items.IndexOf('Consolas');
	if cmbFont.ItemIndex = -1 then
		cmbFont.ItemIndex := cmbFont.Items.IndexOf('Courier New');
	lbLanguages.SetFocus;
end;

procedure TLangForm.ColorChange(Sender: TObject);
begin
	dmMain.InitHighlighterFirstTime(cmbColors.ItemIndex);

	// Pick a proper current line color (choice is up for debate...)
	if cmbColors.Text = 'Obsidian' then
		devEditor.HighColor := clBlack
	else if cmbColors.Text = 'Twilight' then
		devEditor.HighColor := $202020
	else if cmbColors.Text = 'Borland' then
		devEditor.HighColor := $202020
	else if cmbColors.Text = 'Matrix' then
		devEditor.HighColor := $202020 // dark brown
	else if cmbColors.Text = 'GSS Hacker' then
		devEditor.HighColor := clBlack
	else if cmbColors.Text = 'Obvilion' then
		devEditor.HighColor := clBlack
	else if cmbColors.Text = 'PlasticCodeWrap' then
		devEditor.HighColor := clBlack
	else
		devEditor.HighColor := $FFFFCC; // Light Turquoise

	devEditor.AssignEditor(synExample,'main.cpp');
end;

procedure TLangForm.FontChange(Sender: TObject);
begin
	devEditor.Font.Name := cmbFont.Text;
	devEditor.Gutterfont.Name := cmbFont.Text;
	devEditor.AssignEditor(synExample,'main.cpp');
end;

procedure TLangForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	Action := caFree;
end;

procedure TLangForm.cmbIconsChange(Sender: TObject);
begin
	if cmbIcons.ItemIndex =  1 then
		tbExample.Images := dmMain.MenuImages_Gnome
	else if cmbIcons.ItemIndex = 2 then
		tbExample.Images := dmMain.MenuImages_Blue
	else
		tbExample.Images := dmMain.MenuImages_NewLook;
end;

procedure TLangForm.cmbFontDrawItem(Control: TWinControl; Index: Integer;Rect: TRect; State: TOwnerDrawState);
var
	alignleft : integer;
	aligntop : integer;
begin
	with TComboBox(Control) do begin
		Canvas.Font.Name := Items.Strings[Index];
		Canvas.Font.Size := devEditor.Font.Size;
		Canvas.FillRect(Rect);
		alignleft := (Rect.Right - Rect.Left) div 2 - Canvas.TextWidth(Canvas.Font.Name) div 2;
		aligntop := Rect.Top + (Rect.Bottom - Rect.Top) div 2 - Canvas.TextHeight(Canvas.Font.Name) div 2;
		Canvas.TextOut(alignleft, aligntop,Canvas.Font.Name);
	end;
end;

end.
