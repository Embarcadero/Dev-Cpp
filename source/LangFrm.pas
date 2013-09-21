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
  private
    HasProgressStarted : boolean;
    function GetSelected: integer;
    procedure CppParserTotalProgress(Sender: TObject; const FileName: string; Total, Current: Integer);
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
	if not HasProgressStarted then begin
		pbCCCache.Max := Total;
		HasProgressStarted := true;
	end;
	pbCCCache.Position := pbCCCache.Position + Current;
	ParseLabel.Caption := Lang[ID_LANGFORM_PARSING] + #13#10 + ReplaceFirstText(FileName,devDirs.Exec,'\');
	Application.ProcessMessages;
end;

procedure TLangForm.OkBtnClick(Sender: TObject);
var
	sl, f : TStringList;
	i, j : integer;
	fullpath : AnsiString;
begin
	if OkBtn.Tag = 0 then begin // goto edit page
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
	end else if OkBtn.Tag = 1 then begin // goto cache page
		OkBtn.Tag := 2;
		EditPanel.Visible := false;
		CachePanel.Visible := true;
		devData.ThemeChange := true;
		devData.Theme := cmbIcons.Items[cmbIcons.ItemIndex];
	end else if OkBtn.Tag = 2 then begin // done, goto finish page
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

			MainForm.CppParser.ParseLocalHeaders := True;
			MainForm.CppParser.ParseGlobalHeaders := True;
			MainForm.CppParser.OnTotalProgress := CppParserTotalProgress;
			MainForm.CppParser.OnStartParsing := nil;
			MainForm.CppParser.OnEndParsing := nil;
			MainForm.CppParser.Tokenizer:= MainForm.CppTokenizer;
			MainForm.CppParser.Enabled := true;

			MainForm.ClassBrowser.SetUpdateOff;

			sl := TStringList.Create;
			if AltCache.Checked then begin
				for I := 0 to AltFileList.Count - 1 do
					sl.Add(AltFileList.Items[I]);
			end else
				sl.Assign(devCompiler.CppDir);

			// Make it look busy
			Screen.Cursor:=crHourglass;

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
					//end else
					//	MessageDlg('Directory "' + sl[i] + '" does not exist', mtWarning, [mbOK], 0);
				end;
			end else begin
				for i := 0 to sl.Count-1 do begin

					// Assemble full path
					if (Length(sl[i]) > 0) and (sl[i][1] = ':') then
						fullpath := sl[i]
					else if devCompiler.CppDir.Count > 0 then
						fullpath := devCompiler.CppDir[0] + pd + sl[i]
					else
						fullpath := sl[i];

					// Then check for existance
					if FileExists(fullpath) then begin
						MainForm.CppParser.AddFileToScan(fullpath);
					end;
					//end else
					//	MessageDlg('File "' + fullpath + '" does not exist', mtWarning, [mbOK], 0);
				end;
			end;
			sl.Free;
			f.Free;

			MainForm.CppParser.ParseList;

			ParseLabel.Caption := Lang[ID_LANGFORM_SAVING];
			Application.ProcessMessages;

			MainForm.CppParser.Save(devDirs.Config + DEV_COMPLETION_CACHE,devDirs.Exec);

			MainForm.CppParser.OnStartParsing := MainForm.CppParserStartParsing;
			MainForm.CppParser.OnEndParsing := MainForm.CppParserEndParsing;
			MainForm.CppParser.OnTotalProgress := MainForm.CppParserTotalProgress;

			MainForm.ClassBrowser.SetUpdateOn;

			// Erase ALL memory of the C++ parser
			MainForm.CppParser.Reset(false);

			Screen.Cursor:=crDefault;
		end else begin
			devCodeCompletion.Enabled := true;
			devCodeCompletion.ParseLocalHeaders := true;
			devCodeCompletion.ParseGlobalHeaders := false; // can be slow without cache
			devClassBrowsing.ShowInheritedMembers := false;
		end;
		OkBtn.Tag := 3;
		OkBtn.Kind := bkOK;
		OkBtn.ModalResult := mrOK;
		OkBtn.Enabled := true;
		FinishPanel.Visible := true;
		CachePanel.Visible := false;
	end;
end;

procedure TLangForm.ButtonAddFileClick(Sender: TObject);
var
	I: integer;
	s: AnsiString;
begin
	with TOpenDialog.Create(self) do try
		Filter:= FLT_HEADS;
		Title:= Lang[ID_NV_OPENFILE];
		Options := Options + [ofAllowMultiSelect];

		// Start in the include folder
		if devCompiler.CppDir.Count > 0 then
			InitialDir := devCompiler.CppDir[0];

		if Execute then begin
			for i:= 0 to Files.Count-1 do begin
				if devCompiler.CppDir.Count > 0 then
					s := StringReplace(Files.Strings[i],devCompiler.CppDir[0] + pd,'',[rfReplaceAll])
				else
					s := Files.Strings[i];
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
	I : integer;
	s : AnsiString;
begin
	f := TStringList.Create;
	try
		if SelectDirectory('Select Folder', devDirs.Exec, Dir) then begin
			FilesFromWildcard(Dir, '*.*', f, false, false, false);
			for i := 0 to f.Count-1 do begin
				if devCompiler.CppDir.Count > 0 then
					s := StringReplace(f[i],devCompiler.CppDir[0] + pd,'',[rfReplaceAll])
				else
					s := f[i];
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

	HasProgressStarted := false;
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
	cmbFont.ItemIndex := cmbFont.Items.IndexOf('Courier New'); // suggest Consolas?
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
		devEditor.HighColor := clBlack // dark brown
	else if cmbColors.Text = 'Obvilion' then
		devEditor.HighColor := clBlack // dark brown
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
		aligntop  := Rect.Top + (Rect.Bottom - Rect.Top) div 2 - Canvas.TextHeight(Canvas.Font.Name) div 2;
		Canvas.TextOut(alignleft, aligntop,Canvas.Font.Name);
	end;
end;

end.
