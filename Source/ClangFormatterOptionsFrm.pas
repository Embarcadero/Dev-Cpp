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

unit ClangFormatterOptionsFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, StdCtrls, Inifiles, ExtCtrls, ComCtrls, Spin, Math,
  CompOptionsFrame, CompOptionsList, SynEdit, Editor;

type
  TClangFormatterOptionsForm = class(TForm)
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    btnExecute: TBitBtn;
    lblBracketAlignmentStyle: TLabel;
    cmbBracketAlignmentStyle: TComboBox;
    synExample: TSynEdit;
    grpOptions: TGroupBox;
    lblPoweredBy: TLabel;
    spIndentWidth: TSpinEdit;
    lblCommand: TLabel;
    bvCustom: TBevel;
    lblPreview: TLabel;
    chkBracedListStyle: TCheckBox;
    memFullCommand: TMemo;
    spColumnLimit: TSpinEdit;
    chkColumnLimit: TCheckBox;
    lblBasedOnStyle: TLabel;
    coBasedOnStyle: TComboBox;
    spTabWidth: TSpinEdit;
    chkIndentWidth: TCheckBox;
    chkTabWidth: TCheckBox;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure btnExecuteClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OptionChange(Sender: TObject);
    procedure CommandChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
  private
    fCreating: Boolean;
    fValid: Boolean;
    procedure LoadText;
    procedure LoadSettings;
    procedure LoadSampleText;
    procedure CreateScratchFile;
    procedure SaveSettings;
    function GetFullCommand: String;
  end;

implementation

uses
  System.IOUtils, System.UITypes, ShellAPI, Main, FileCtrl, version, devcfg, utils, MultiLangSupport, DataFrm;

{$R *.dfm}

procedure TClangFormatterOptionsForm.btnCancelClick(Sender: TObject);
begin
  devFormatterClang.ExecFromOpt := false;
  Close;
end;

procedure TClangFormatterOptionsForm.btnExecuteClick(Sender: TObject);
begin
  devFormatterClang.ExecFromOpt := true;
  Close;
end;

procedure TClangFormatterOptionsForm.btnOkClick(Sender: TObject);
begin
  devFormatterClang.ExecFromOpt := false;
  SaveSettings;
end;

procedure TClangFormatterOptionsForm.btnHelpClick(Sender: TObject);
begin
  if fValid then
    ShellExecute(
      0,
      PChar('open'),
      PChar(devDirs.Exec + devFormatterClang.ClangDir + 'doc\clang.html'),
      nil,
      nil,
      SW_SHOWNORMAL);
end;

procedure TClangFormatterOptionsForm.LoadText;
begin
  // Set interface font
  Font.Name := devData.InterfaceFont;
  Font.Size := devData.InterfaceFontSize;

  Caption := Lang[ID_FORMATTER_WINDOW];
  grpOptions.Caption := Lang[ID_FORMATTER_OPTIONS];

  lblBasedOnStyle.Caption := Lang[ID_FORMATTER_BASEDONSTYLE];
  lblBracketAlignmentStyle.Caption := Lang[ID_FORMATTER_BRACKETALIGNMENTSTYLE];
  chkIndentWidth.Caption := Lang[ID_FORMATTER_INDENTWIDTH];
  chkBracedListStyle.Caption := Lang[ID_FORMATTER_BRACEDLISTSTYLE];
  chkTabWidth.Caption := Lang[ID_FORMATTER_TABWIDTH];
  chkColumnLimit.Caption := Lang[ID_FORMATTER_COLUMNLIMIT];

  lblCommand.Caption := Lang[ID_FORMATTER_COMMAND];
  lblPreview.Caption := Lang[ID_FORMATTER_PREVIEW];
  if fValid then
    lblPoweredBy.Caption := Format(Lang[ID_FORMATTER_POWEREDBY], [devFormatterClang.GetVersion])
  else
    lblPoweredBy.Caption := Lang[ID_FORMATTER_CLANG_POWEREDBYFAIL];
end;

procedure TClangFormatterOptionsForm.FormCreate(Sender: TObject);
begin
  fCreating := True; // prevents spamming of astyle commands when initializing UI
  try
    // If we cannot find AStyle, only issue a warning
    if not devFormatterClang.Validate then begin
      fValid := False;
      MessageDlg(Lang[ID_FORMATTER_CLANG_NOTVALID], mtWarning, [mbOK], 0);
    end else
      fValid := True;

    // Translate
    LoadText;

    // Load dummy text from current file or dummy file
    LoadSampleText;

    // Create scratch file
    CreateScratchFile;

    // Load settings
    LoadSettings;
  finally
    fCreating := False;
  end;
end;

procedure TClangFormatterOptionsForm.LoadSampleText;
var
  e: TEditor;
  FileName: String;
begin
  // Create a rough copy of the current file
  e := MainForm.EditorList.GetEditor;
  if Assigned(e) then begin
    FileName := e.FileName;
    synExample.Text := e.Text.Text;
  end else
    FileName := 'main.cpp';
  devEditor.AssignEditor(synExample, FileName);
  synExample.BorderStyle := bsSingle;
end;

procedure TClangFormatterOptionsForm.CreateScratchFile;
begin
  synExample.Lines.SaveToFile(TPath.Combine(TPath.GetTempPath,'DummyInput.txt')); // devDirs.Exec + devFormatterClang.ClangDir
end;

procedure TClangFormatterOptionsForm.LoadSettings;
begin
  with devFormatterClang do begin
    //Set Base style
    coBasedOnStyle.ItemIndex := BasedOnStyle;

    //Set Bracket Alignment Style
    cmbBracketAlignmentStyle.ItemIndex := BracketAlignmentStyle;

    //Set Indent Width
    chkIndentWidth.Checked := ModifyIndentWidth;
    spIndentWidth.Value := IndentWidth;

    //Set Braced List Style
    chkBracedListStyle.Checked := BracedListStyle;

    //Set Modify Tab Width
    chkTabWidth.Checked := ModifyTabWidth;
    spTabWidth.Value := TabWidth;


    //Set Column Limit
    chkColumnLimit.Checked := ModifyColumnLimit;
    spColumnLimit.Value := ColumnLimit;

    // Set full command
    memFullCommand.Text := FullCommand;
  end;
end;

procedure TClangFormatterOptionsForm.SaveSettings;
begin
  with devFormatterClang do begin
    //Set Base style
    BasedOnStyle := coBasedOnStyle.ItemIndex;

    //Set Bracket Alignment Style
    BracketAlignmentStyle := cmbBracketAlignmentStyle.ItemIndex;

    //Set Indent Width
    ModifyIndentWidth := chkIndentWidth.Checked;
    IndentWidth := spIndentWidth.Value;

    //Set Braced List Style
    BracedListStyle := chkBracedListStyle.Checked;

    //Set Modify Tab Width
    ModifyTabWidth := chkTabWidth.Checked;
    TabWidth := spTabWidth.Value;


    //Set Column Limit
    ModifyColumnLimit := chkColumnLimit.Checked;
    ColumnLimit := spColumnLimit.Value;

    // Set full command
    FullCommand := memFullCommand.Text;
  end;
end;

procedure TClangFormatterOptionsForm.OptionChange(Sender: TObject);
begin
  if fCreating then
    Exit;

  // Update UI
  memFullCommand.Text := GetFullCommand;
end;

procedure TClangFormatterOptionsForm.CommandChange(Sender: TObject);
var
  ClangOutput, DummyFileName: String;
begin
  if fCreating then
    Exit;
  // Apply to dummy file
  DummyFileName := TPath.Combine(TPath.GetTempPath,'DummyInput.txt'); //devDirs.Exec + devFormatterClang.AStyleDir
  ClangOutput := devFormatterClang.FormatFile(DummyFileName, memFullCommand.Text);

  // Check if formatting finished correctly
  if FileExists(DummyFileName) then begin
    synExample.Lines.Text := ClangOutput;
    synExample.Lines.SaveToFile(DummyFileName);
  end else
    synExample.Lines.Text := Format(Lang[ID_FORMATTER_CLANG_LOADERROR], [DummyFileName]);
end;

// copy of TdevFormatter.GetFullCommand

function TClangFormatterOptionsForm.GetFullCommand: String;
begin
  Result := '--style="{';

  // Add based style
  case coBasedOnStyle.ItemIndex of
    1: Result := Result + 'BasedOnStyle: LLVM, ';
    2: Result := Result + 'BasedOnStyle: Google, ';
    3: Result := Result + 'BasedOnStyle: Chromium, ';
    4: Result := Result + 'BasedOnStyle: Mozilla, ';
    5: Result := Result + 'BasedOnStyle: WebKit, ';
    6: Result := Result + 'BasedOnStyle: Microsoft, ';
    7: Result := Result + 'BasedOnStyle: GNU, ';
  end;

  // Add Bracket Alignment Style
  case cmbBracketAlignmentStyle.ItemIndex of
    1: Result := Result + 'AlignAfterOpenBracket: Align, ';
    2: Result := Result + 'AlignAfterOpenBracket: DontAlign, ';
    3: Result := Result + 'AlignAfterOpenBracket: AlwaysBreak, ';
  end;

  // Add IndentWidth
  if chkIndentWidth.Checked then
    Result := Result + 'IndentWidth: ' + IntToStr(spIndentWidth.Value) + ', ';

  //Add Braced List Style
  if chkBracedListStyle.Checked then
    Result := Result + 'Cpp11BracedListStyle: true, ';

  //Add Tab Width
  if chkTabWidth.Checked then
    Result := Result + 'TabWidth: ' + IntToStr(spTabWidth.Value) + ', ';

  //Add Column Limit
  if chkColumnLimit.Checked then
    Result := Result + 'ColumnLimit: ' + IntToStr(spColumnLimit.Value) + ', ';

  Result := Result + 'Language: Cpp}"';
end;

procedure TClangFormatterOptionsForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TClangFormatterOptionsForm.FormActivate(Sender: TObject);
begin
  ActiveControl := nil;
end;

end.

