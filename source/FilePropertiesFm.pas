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
unit FilePropertiesFm;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StrUtils, ExtCtrls, StdCtrls, SynEdit,
  SynEditTypes, FileCtrl, XPMenu;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Variants, Classes, QGraphics, QControls, QForms,
  QDialogs, StrUtils, QExtCtrls, QStdCtrls, QSynEdit,
  QSynEditTypes;
{$ENDIF}

type
  TFilePropertiesForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    lblFilename: TLabel;
    lblProject: TLabel;
    lblSize: TLabel;
    lblTotal: TLabel;
    lblCode: TLabel;
    lblComments: TLabel;
    lblEmpty: TLabel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    Bevel7: TBevel;
    btnOK: TButton;
    Bevel8: TBevel;
    Label8: TLabel;
    lblIncludes: TLabel;
    cmbFiles: TComboBox;
    Label9: TLabel;
    Label10: TLabel;
    lblAbsolute: TLabel;
    lblRelative: TLabel;
    Bevel9: TBevel;
    Label11: TLabel;
    lblTimestamp: TLabel;
    XPMenu: TXPMenu;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnOKClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cmbFilesClick(Sender: TObject);
  private
    { Private declarations }
    fFilename: string;
    fEdit: TSynEdit;
    procedure LoadText;
    procedure CalculateFile(Filename: string);
    procedure CalculateSize(Filename: string);
    procedure ShowPropsFor(Filename: string);
    procedure FillFiles;
  public
    { Public declarations }
    procedure SetFile(Filename: string);
  end;

var
  FilePropertiesForm: TFilePropertiesForm;
  Size, Stamp, Total, Code, Comments, Includes, Empty: integer;

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

procedure TFilePropertiesForm.CalculateSize(Filename: string);
var
  hFile: integer;
begin
  hFile := FileOpen(Filename, fmOpenRead);
  if hFile > 0 then begin
    Stamp := FileGetDate(hFile);
    Size := FileSeek(hFile, 0, 2);
    FileClose(hFile);
  end
  else begin
    Stamp := 0;
    Size := 0;
  end;
end;

procedure TFilePropertiesForm.CalculateFile(Filename: string);
var
  Attri: TSynHighlighterAttributes;
  Current, Token: string;
  I, C: integer;
begin
  if not Assigned(fEdit) then
    Exit;

  CalculateSize(FileName);

  Total := fEdit.Lines.Count;
  Code := 0;
  Empty := 0;
  Includes := 0;
  Comments := 0;

  // iterate through all lines of file
  for I := 0 to fEdit.Lines.Count - 1 do begin
    Current := fEdit.Lines[I];

    // locate first non-space char in line
    C := 1;
    while (C <= Length(Current)) and (Current[C] in [#9, ' ']) do
      Inc(C);

    // take the token type of the first word of the line
    fEdit.GetHighlighterAttriAtRowCol(BufferCoord(C, I + 1), Token, Attri);

    // if we get a token type...
    if Assigned(Attri) then begin
      // if it is preprocessor...
      if Attri.Name = 'Preprocessor' then begin
        // check for includes
        if AnsiStartsStr('#include', Token) or AnsiStartsStr('# include', Token) then
          Inc(Includes);
        // preprocessor directives are considered as code
        Inc(Code);
      end

      // if it is a comment
      else if Attri.Name = 'Comment' then
        Inc(Comments)

      // else it is code
      else
        Inc(Code);
    end
    // if we don't get a token type, this line is empty or contains only spaces
    else
      Inc(Empty);
  end;
end;

procedure TFilePropertiesForm.FormCreate(Sender: TObject);
begin
  LoadText;
  fEdit := TSynEdit.Create(Application);
  fEdit.Parent := nil;
  fEdit.Highlighter := dmMain.Cpp;
  fFilename := '';
end;

procedure TFilePropertiesForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TFilePropertiesForm.btnOKClick(Sender: TObject);
begin
  Close;
end;

procedure TFilePropertiesForm.LoadText;
begin
  if devData.XPTheme then
    XPMenu.Active := true
  else
    XPMenu.Active := false;
  Caption := Lang[ID_ITEM_PROPERTIES];
  btnOK.Caption := Lang[ID_BTN_OK];
  Label1.Caption := Lang[ID_PROPS_FILENAME] + ':';
  Label2.Caption := Lang[ID_PROPS_INPROJECT] + ':';
  Label3.Caption := Lang[ID_PROPS_TOTAL] + ':';
  Label4.Caption := Lang[ID_PROPS_CODE] + ':';
  Label5.Caption := Lang[ID_PROPS_COMMENTS] + ':';
  Label6.Caption := Lang[ID_PROPS_FILESIZE] + ':';
  Label7.Caption := Lang[ID_PROPS_EMPTY] + ':';
  Label8.Caption := Lang[ID_PROPS_INCLUDES] + ':';
  Label9.Caption := Lang[ID_PROPS_ABSOLUTE] + ':';
  Label10.Caption := Lang[ID_PROPS_RELATIVE] + ':';
  Label11.Caption := Lang[ID_PROPS_TIMESTAMP] + ':';
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

procedure TFilePropertiesForm.ShowPropsFor(Filename: string);
begin
  try
    fEdit.Lines.LoadFromFile(Filename);
    CalculateFile(Filename);
  except
    // probably the file does not exist (isn't saved yet maybe?)
    Total := 0;
    Size := 0;
    Stamp := 0;
    Code := 0;
    Empty := 0;
    Includes := 0;
    Comments := 0;
  end;
  lblFilename.Caption := MinimizeName(FileName, lblFilename.Canvas, 285);
  if Assigned(MainForm.fProject) then begin
    lblProject.Caption := MainForm.fProject.Name;
    lblRelative.Caption := ExtractRelativePath(MainForm.fProject.Directory, Filename)
  end
  else begin
    lblProject.Caption := '-';
    lblRelative.Caption := '-';
  end;
  lblAbsolute.Caption := Filename;
  lblTotal.Caption := IntToStr(Total);
  lblSize.Caption := FormatFloat('#,###,##0', Size);
  lblCode.Caption := IntToStr(Code);
  lblEmpty.Caption := IntToStr(Empty);
  lblIncludes.Caption := IntToStr(Includes);
  lblComments.Caption := IntToStr(Comments);
  if Stamp = 0 then
    lblTimestamp.Caption := '-'
  else
    lblTimestamp.Caption := FormatDateTime(ShortDateFormat + ' hh:nn:ss', FileDateToDateTime(Stamp));
end;

procedure TFilePropertiesForm.FillFiles;
var
  I: integer;
  idx: integer;
  e: TEditor;
begin
  cmbFiles.Clear;
  // add all project files
  if Assigned(MainForm.fProject) then begin
    for I := 0 to MainForm.fProject.Units.Count - 1 do
      cmbFiles.Items.AddObject(ExtractFileName(MainForm.fProject.Units[I].FileName), Pointer(MainForm.fProject.Units[I]));
  end;

  // add all open editor files not in project
  for I := 0 to MainForm.PageControl.PageCount - 1 do begin
    e := MainForm.GetEditor(I);
    if not e.InProject then
      cmbFiles.Items.Add(e.FileName);
  end;

  idx := cmbFiles.Items.IndexOf(ExtractFileName(fFilename));
  if idx = -1 then
    idx := cmbFiles.Items.IndexOf(fFilename);
  if idx <> -1 then // just to be on the safe side
    cmbFiles.ItemIndex := idx;
end;

procedure TFilePropertiesForm.cmbFilesClick(Sender: TObject);
begin
  if Assigned(cmbFiles.Items.Objects[cmbFiles.ItemIndex]) then begin
    fFilename := TProjUnit(cmbFiles.Items.Objects[cmbFiles.ItemIndex]).FileName;
    ShowPropsFor(fFilename);
  end
  else
    ShowPropsFor(cmbFiles.Items[cmbFiles.ItemIndex]);
end;

procedure TFilePropertiesForm.SetFile(Filename: string);
begin
  fFilename := Filename;
end;

end.

