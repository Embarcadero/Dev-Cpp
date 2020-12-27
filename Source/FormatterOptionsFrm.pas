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

unit FormatterOptionsFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, StdCtrls, Inifiles, ExtCtrls, ComCtrls, Spin, Math,
  CompOptionsList, SynEdit;

type
  TFormatterOptionsForm = class(TForm)
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    lblBracketStyle: TLabel;
    cmbBracketStyle: TComboBox;
    synExample: TSynEdit;
    cmbIndentStyle: TComboBox;
    lblIndentStyle: TLabel;
    grpOptions: TGroupBox;
    lblPoweredBy: TLabel;
    lblTabWidth: TLabel;
    spinTabWidth: TSpinEdit;
    lblCommand: TLabel;
    bvCustom: TBevel;
    lblPreview: TLabel;
    chkClasses: TCheckBox;
    chkSwitches: TCheckBox;
    chkNamespace: TCheckBox;
    chkCases: TCheckBox;
    chkLabels: TCheckBox;
    chkPreprocessor: TCheckBox;
    lblIndentParts: TLabel;
    memFullCommand: TMemo;
    spinMaxLineLength: TSpinEdit;
    chkMaxLineLength: TCheckBox;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
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
  System.IOUtils, System.UITypes, Editor, ShellAPI, Main, FileCtrl, devcfg, MultiLangSupport;

{$R *.dfm}

procedure TFormatterOptionsForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFormatterOptionsForm.btnOkClick(Sender: TObject);
begin
  SaveSettings;
end;

procedure TFormatterOptionsForm.btnHelpClick(Sender: TObject);
begin
  if fValid then
    ShellExecute(
      0,
      PChar('open'),
      PChar(devDirs.Exec + devFormatter.AStyleDir + 'doc\astyle.html'),
      nil,
      nil,
      SW_SHOWNORMAL);
end;

procedure TFormatterOptionsForm.LoadText;
begin
  // Set interface font
  Font.Name := devData.InterfaceFont;
  Font.Size := devData.InterfaceFontSize;

  Caption := Lang[ID_FORMATTER_WINDOW];
  grpOptions.Caption := Lang[ID_FORMATTER_OPTIONS];
  lblBracketStyle.Caption := Lang[ID_FORMATTER_BRACKET];
  lblIndentStyle.Caption := Lang[ID_FORMATTER_INDENT];
  lblTabWidth.Caption := Lang[ID_FORMATTER_TABWIDTH];
  chkMaxLineLength.Caption := Lang[ID_FORMATTER_MAXLINELENGTH];
  lblIndentParts.Caption := Lang[ID_FORMATTER_INDENTPARTS];
  chkClasses.Caption := Lang[ID_FORMATTER_CLASSES];
  chkSwitches.Caption := Lang[ID_FORMATTER_SWITCHES];
  chkCases.Caption := Lang[ID_FORMATTER_CASES];
  chkNamespace.Caption := Lang[ID_FORMATTER_NAMESPACE];
  chkLabels.Caption := Lang[ID_FORMATTER_LABELS];
  chkPreprocessor.Caption := Lang[ID_FORMATTER_PREPROC];
  lblCommand.Caption := Lang[ID_FORMATTER_COMMAND];
  lblPreview.Caption := Lang[ID_FORMATTER_PREVIEW];
  if fValid then
    lblPoweredBy.Caption := Format(Lang[ID_FORMATTER_POWEREDBY], [devFormatter.GetVersion])
  else
    lblPoweredBy.Caption := Lang[ID_FORMATTER_POWEREDBYFAIL];
end;

procedure TFormatterOptionsForm.FormCreate(Sender: TObject);
begin
  fCreating := True; // prevents spamming of astyle commands when initializing UI
  try
    // If we cannot find AStyle, only issue a warning
    if not devFormatter.Validate then begin
      fValid := False;
      MessageDlg(Lang[ID_FORMATTER_NOTVALID], mtWarning, [mbOK], 0);
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

procedure TFormatterOptionsForm.LoadSampleText;
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

procedure TFormatterOptionsForm.CreateScratchFile;
begin
  synExample.Lines.SaveToFile(TPath.Combine(TPath.GetTempPath,'DummyInput.txt')); // devDirs.Exec + devFormatter.AStyleDir
end;

procedure TFormatterOptionsForm.LoadSettings;
begin
  with devFormatter do begin
    // Set bracket style
    cmbBracketStyle.ItemIndex := BracketStyle;

    // Set indent style
    cmbIndentStyle.ItemIndex := IndentStyle;

    // Set tab width
    spinTabWidth.Value := TabWidth;

    // Set max line length
    chkMaxLineLength.Checked := ModifyMaxLineLength;
    spinMaxLineLength.Value := MaxLineLength;

    // Set indentation options
    chkClasses.Checked := IndentClasses;
    chkSwitches.Checked := IndentSwitches;
    chkNamespace.Checked := IndentNamespaces;
    chkLabels.Checked := IndentLabels;
    chkPreprocessor.Checked := IndentPreprocessor;

    // Set full command
    memFullCommand.Text := FullCommand;
  end;
end;

procedure TFormatterOptionsForm.SaveSettings;
begin
  with devFormatter do begin
    // Set bracket style
    BracketStyle := cmbBracketStyle.ItemIndex;

    // Set indent style
    IndentStyle := cmbIndentStyle.ItemIndex;

    // Set tab width
    TabWidth := spinTabWidth.Value;

    // Set max line length
    ModifyMaxLineLength := chkMaxLineLength.Checked;
    MaxLineLength := spinMaxLineLength.Value;

    // Set indentation options
    IndentClasses := chkClasses.Checked;
    IndentSwitches := chkSwitches.Checked;
    IndentNamespaces := chkNamespace.Checked;
    IndentLabels := chkLabels.Checked;
    IndentPreprocessor := chkPreprocessor.Checked;

    // Set full command
    FullCommand := memFullCommand.Text;
  end;
end;

procedure TFormatterOptionsForm.OptionChange(Sender: TObject);
begin
  if fCreating then
    Exit;

  // Update UI
  memFullCommand.Text := GetFullCommand;
end;

procedure TFormatterOptionsForm.CommandChange(Sender: TObject);
var
  AStyleOutput, DummyFileName: String;
begin
  if fCreating then
    Exit;
  // Apply to dummy file
  DummyFileName := TPath.Combine(TPath.GetTempPath,'DummyInput.txt'); //devDirs.Exec + devFormatter.AStyleDir
  AStyleOutput := devFormatter.FormatFile(DummyFileName, memFullCommand.Text);

  // Check if formatting finished correctly
  if FileExists(DummyFileName) then begin
    synExample.Lines.LoadFromFile(DummyFileName);
  end else
    synExample.Lines.Text := Format(Lang[ID_FORMATTER_LOADERROR], [DummyFileName]);
end;

// copy of TdevFormatter.GetFullCommand

function TFormatterOptionsForm.GetFullCommand: String;
begin
  Result := '';

  // Add bracket style
  if cmbBracketStyle.ItemIndex > 0 then
    Result := Result + ' -A' + IntToStr(cmbBracketStyle.ItemIndex);

  // Add indent style and tab width
  case cmbIndentStyle.ItemIndex of
    1: Result := Result + ' --indent=spaces=' + IntToStr(spinTabWidth.Value);
    2: Result := Result + ' --indent=tab=' + IntToStr(spinTabWidth.Value);
    3: Result := Result + ' --indent=force-tab=' + IntToStr(spinTabWidth.Value);
    4: Result := Result + ' --indent=force-tab-x=' + IntToStr(spinTabWidth.Value);
  end;

  // Add line length
  if chkMaxLineLength.Checked then begin
    if spinMaxLineLength.Text <> '' then
      Result := Result + ' --max-code-length=' + IntToStr(spinMaxLineLength.Value)
    else
      Result := Result + ' --max-code-length=' + IntToStr(spinMaxLineLength.MinValue);
  end;

  // Add indentation options
  if chkClasses.Checked then
    Result := Result + ' --indent-classes';
  if chkSwitches.Checked then
    Result := Result + ' --indent-switches';
  if chkCases.Checked then
    Result := Result + ' --indent-cases';
  if chkNamespace.Checked then
    Result := Result + ' --indent-namespaces';
  if chkLabels.Checked then
    Result := Result + ' --indent-labels';
  if chkPreprocessor.Checked then
    Result := Result + ' --indent-preprocessor';

  Result := TrimLeft(Result);
end;

procedure TFormatterOptionsForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TFormatterOptionsForm.FormActivate(Sender: TObject);
begin
  ActiveControl := nil;
end;

end.

