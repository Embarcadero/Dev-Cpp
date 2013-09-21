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

{$WARN UNIT_PLATFORM OFF}
unit FilePropertiesFrm;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StrUtils, ExtCtrls, StdCtrls, SynEdit, utils,
  SynEditTypes, FileCtrl, Menus;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Variants, Classes, QGraphics, QControls, QForms,
  QDialogs, StrUtils, QExtCtrls, QStdCtrls, QSynEdit,
  QSynEditTypes;
{$ENDIF}

type
  TFilePropertiesForm = class(TForm)
    lblFileName: TLabel;
    lblProject: TLabel;
    lblTotalLines: TLabel;
    lblCodeLines: TLabel;
    lblCommentLines: TLabel;
    lblFileSize: TLabel;
    lblEmptyLines: TLabel;
    edProject: TEdit;
    edFileSize: TEdit;
    edTotalLines: TEdit;
    edCodeLines: TEdit;
    edCommentLines: TEdit;
    edEmptyLines: TEdit;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    Bevel7: TBevel;
    btnOK: TButton;
    Bevel8: TBevel;
    lblIncludes: TLabel;
    edIncludes: TEdit;
    cmbFiles: TComboBox;
    lblAbsolute: TLabel;
    lblRelative: TLabel;
    edAbsolute: TEdit;
    edRelative: TEdit;
    Bevel9: TBevel;
    lblTimeStamp: TLabel;
    edTimestamp: TEdit;
    PropertiesPop: TPopupMenu;
    PropertiesCopy: TMenuItem;
    N1: TMenuItem;
    PropertiesSelAll: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnOKClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cmbFilesClick(Sender: TObject);
    procedure PropertiesCopyClick(Sender: TObject);
    procedure PropertiesSelAllClick(Sender: TObject);
  private
    fFileDate : integer;
    fFileSize : integer;
    fTotalLines : integer;
    fCodeLines : integer;
    fEmptyLines : integer;
    fIncludeLines : integer;
    fCommentLines : integer;
    fFilename: AnsiString;
    fEdit: TSynEdit;
    procedure LoadText;
    procedure RightAlign(window : TEdit);
    procedure CalculateFile(const Filename: AnsiString);
    procedure ShowPropsFor(const Filename: AnsiString);
    procedure FillFiles;
  public
    procedure SetFile(const Filename: AnsiString);
  end;

implementation

uses 
{$IFDEF WIN32}
  SynEditHighlighter, main, MultiLangSupport, datamod, project, editor, devcfg;
{$ENDIF}
{$IFDEF LINUX}
  QSynEditHighlighter, main, MultiLangSupport, datamod, project, editor, devcfg;
{$ENDIF}

{$R *.dfm}

{ TFilePropertiesForm }

procedure TFilePropertiesForm.CalculateFile(const Filename: AnsiString);
var
	Attri: TSynHighlighterAttributes;
	Current, Token: AnsiString;
	I, J, hFile: integer;
begin
	// Calculate size
	hFile := FileOpen(Filename, fmOpenRead);
	if hFile > 0 then begin
		fFileDate := FileGetDate(hFile);
		Inc(fFileSize,FileSeek(hFile, 0, 2));
		FileClose(hFile);
	end;

	Inc(fTotalLines,fEdit.Lines.Count);

	// iterate through all lines of file
	for I := 0 to fEdit.Lines.Count - 1 do begin
		Current := fEdit.Lines[I];

		// locate first non-space char in line
		J := 1;
		while (J <= Length(Current)) and (Current[J] in [#9,#32]) do
			Inc(J);

		// take the token type of the first word of the line
		if fEdit.GetHighlighterAttriAtRowCol(BufferCoord(J, I + 1), Token, Attri) then begin

			// if it is preprocessor...
			if SameStr(Attri.Name,'Preprocessor') then begin

				// check for includes
				if StartsStr('#include', Token) or StartsStr('# include', Token) then
					Inc(fIncludeLines);

				// preprocessor directives are considered as code
				Inc(fCodeLines);
			end

			// if it is a comment
			else if SameStr(Attri.Name,'Comment') then
				Inc(fCommentLines)

			// else it is code
			else
				Inc(fCodeLines);
		end else // if we don't get a token type, this line is empty or contains only spaces
			Inc(fEmptyLines);
	end;
end;

procedure TFilePropertiesForm.FormCreate(Sender: TObject);
begin
	LoadText;
	fEdit := TSynEdit.Create(nil);
	fEdit.Highlighter := dmMain.Cpp;
	fFilename := '';
end;

procedure TFilePropertiesForm.FormClose(Sender: TObject;var Action: TCloseAction);
begin
	Action := caFree;
end;

procedure TFilePropertiesForm.btnOKClick(Sender: TObject);
begin
	Close;
end;

procedure TFilePropertiesForm.LoadText;
begin
	// Set interface font
	Font.Name := devData.InterfaceFont;
	Font.Size := devData.InterfaceFontSize;

	PropertiesCopy.Caption := Lang[ID_ITEM_COPY];
	PropertiesSelAll.Caption := Lang[ID_ITEM_SELECTALL];

	Caption := Lang[ID_ITEM_PROPERTIES];
	btnOK.Caption := Lang[ID_BTN_OK];

	lblFileName.Caption := Lang[ID_PROPS_FILENAME] + ':';
	lblProject.Caption := Lang[ID_PROPS_INPROJECT] + ':';
	lblAbsolute.Caption := Lang[ID_PROPS_ABSOLUTE] + ':';
	lblRelative.Caption := Lang[ID_PROPS_RELATIVE] + ':';

	lblTotalLines.Caption := Lang[ID_PROPS_TOTAL] + ':';
	lblCodeLines.Caption := Lang[ID_PROPS_CODE] + ':';
	lblCommentLines.Caption := Lang[ID_PROPS_COMMENTS] + ':';
	lblEmptyLines.Caption := Lang[ID_PROPS_EMPTY] + ':';
	lblFileSize.Caption := Lang[ID_PROPS_FILESIZE] + ':';
	lblIncludes.Caption := Lang[ID_PROPS_INCLUDES] + ':';

	lblTimeStamp.Caption := Lang[ID_PROPS_TIMESTAMP] + ':';

	RightAlign(edTotalLines);
	RightAlign(edCodeLines);
	RightAlign(edCommentLines);
	RightAlign(edEmptyLines);
	RightAlign(edFileSize);
	RightAlign(edIncludes);
	RightAlign(edTimestamp);
end;

procedure TFilePropertiesForm.RightAlign(window : TEdit);
var
	currentstyle: dWord;
begin
	currentstyle := GetWindowLong(window.Handle, GWL_STYLE);
	SetWindowLong(window.Handle, GWL_STYLE, currentstyle or ES_RIGHT);
end;

procedure TFilePropertiesForm.FormDestroy(Sender: TObject);
begin
	fEdit.Free;
end;

procedure TFilePropertiesForm.FormShow(Sender: TObject);
begin
	if fFilename = '' then
		fFilename := MainForm.GetEditor.FileName;
	FillFiles;
	ShowPropsFor(fFilename);
end;

procedure TFilePropertiesForm.ShowPropsFor(const Filename: AnsiString);
var
	I : integer;
	loopfilename : AnsiString;
begin

	fFileDate := 0;
	fFileSize := 0;
	fTotalLines := 0;
	fFileSize := 0;
	fFileDate := 0;
	fCodeLines := 0;
	fEmptyLines := 0;
	fIncludeLines := 0;
	fCommentLines := 0;

	// Selected the project? Loop through files
	if SameStr('.dev',ExtractFileExt(FileName)) then begin

		// Loop through files
		for I := 0 to cmbfiles.Items.Count - 1 do begin

			// Get file name
			if Assigned(cmbFiles.Items.Objects[I]) then
				loopfilename := TProjUnit(cmbFiles.Items.Objects[I]).FileName
			else
				loopfilename := cmbFiles.Items[I];

			if not SameStr('.dev',ExtractFileExt(loopfilename)) then begin
				try
					fEdit.Lines.LoadFromFile(loopfilename);
					CalculateFile(loopfilename);
				except end;
			end;
		end;

		edProject.Text := '-';
		edAbsolute.Text := MainForm.fProject.FileName;
		edRelative.Text := ExtractRelativePath(MainForm.fProject.Directory, Filename);

		if fFileDate = 0 then
			edTimestamp.Text := '(Project file only) -'
		else
			edTimestamp.Text := '(Project file only) ' + FormatDateTime(ShortDateFormat + ' hh:nn:ss', FileDateToDateTime(fFileDate));

	// Just show information about the selected file only
	end else begin

		try
			fEdit.Lines.LoadFromFile(Filename);
			CalculateFile(FileName);
		except end;

		if Assigned(MainForm.fProject) then begin
			edProject.Text := MainForm.fProject.Name;
			edRelative.Text := ExtractRelativePath(MainForm.fProject.Directory, Filename)
		end else begin
			edProject.Text := '-';
			edRelative.Text := '-';
		end;
		edAbsolute.Text := FileName;

		if fFileDate = 0 then
			edTimestamp.Text := '-'
		else
			edTimestamp.Text := FormatDateTime(ShortDateFormat + ' hh:nn:ss', FileDateToDateTime(fFileDate));
	end;

	edTotalLines.Text := IntToStr(fTotalLines);
	edFileSize.Text := FormatFloat('#,###,##0', fFileSize);
	edCodeLines.Text := IntToStr(fCodeLines);
	edEmptyLines.Text := IntToStr(fEmptyLines);
	edIncludes.Text := IntToStr(fIncludeLines);
	edCommentLines.Text := IntToStr(fCommentLines);
end;

procedure TFilePropertiesForm.FillFiles;
var
	I: integer;
	idx: integer;
	e: TEditor;
begin
	cmbFiles.Clear;
	cmbFiles.Items.BeginUpdate;

	// add all project files
	if Assigned(MainForm.fProject) then begin

		// Add project file itself
		cmbFiles.Items.Add(ExtractFileName(MainForm.fProject.FileName));

		for I := 0 to MainForm.fProject.Units.Count - 1 do
			cmbFiles.Items.AddObject(ExtractFileName(MainForm.fProject.Units[I].FileName), Pointer(MainForm.fProject.Units[I]));
	end;

	// add all open editor files not in project
	for I := 0 to MainForm.PageControl.PageCount - 1 do begin
		e := MainForm.GetEditor(I);
		if not e.InProject then
			cmbFiles.Items.Add(e.FileName);
	end;

	// Highlight current file 
	idx := cmbFiles.Items.IndexOf(ExtractFileName(fFilename));
	if idx = -1 then
		idx := cmbFiles.Items.IndexOf(fFilename);
	if idx <> -1 then // just to be on the safe side
		cmbFiles.ItemIndex := idx;

	cmbFiles.Items.EndUpdate;
end;

procedure TFilePropertiesForm.cmbFilesClick(Sender: TObject);
begin
	if Assigned(cmbFiles.Items.Objects[cmbFiles.ItemIndex]) then begin
		fFilename := TProjUnit(cmbFiles.Items.Objects[cmbFiles.ItemIndex]).FileName;
		ShowPropsFor(fFilename);
	end else
		ShowPropsFor(cmbFiles.Items[cmbFiles.ItemIndex]);
end;

procedure TFilePropertiesForm.SetFile(const Filename: AnsiString);
begin
	fFilename := Filename;
end;

procedure TFilePropertiesForm.PropertiesCopyClick(Sender: TObject);
begin
	if edProject.Focused then
		edProject.CopyToClipboard
	else if edAbsolute.Focused then
		edAbsolute.CopyToClipboard
	else if edRelative.Focused then
		edRelative.CopyToClipboard
	else if edTotalLines.Focused then
		edTotalLines.CopyToClipboard
	else if edCodeLines.Focused then
		edCodeLines.CopyToClipboard
	else if edCommentLines.Focused then
		edCommentLines.CopyToClipboard
	else if edEmptyLines.Focused then
		edEmptyLines.CopyToClipboard
	else if edFileSize.Focused then
		edFileSize.CopyToClipboard
	else if edIncludes.Focused then
		edIncludes.CopyToClipboard
	else if edTimestamp.Focused then
		edTimeStamp.CopyToClipboard;
end;

procedure TFilePropertiesForm.PropertiesSelAllClick(Sender: TObject);
begin
	if edProject.Focused then
		edProject.SelectAll
	else if edAbsolute.Focused then
		edAbsolute.SelectAll
	else if edRelative.Focused then
		edRelative.SelectAll
	else if edTotalLines.Focused then
		edTotalLines.SelectAll
	else if edCodeLines.Focused then
		edCodeLines.SelectAll
	else if edCommentLines.Focused then
		edCommentLines.SelectAll
	else if edEmptyLines.Focused then
		edEmptyLines.SelectAll
	else if edFileSize.Focused then
		edFileSize.SelectAll
	else if edIncludes.Focused then
		edIncludes.SelectAll
	else if edTimestamp.Focused then
		edTimeStamp.SelectAll;
end;

end.
