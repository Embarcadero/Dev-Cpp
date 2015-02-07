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

unit IncrementalFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ActnList, SynEdit, SynEditTypes, SynEditSearch, ExtCtrls, Menus;

type
  TIncrementalForm = class(TForm)
    Edit: TEdit;
    btnPrev: TButton;
    btnNext: TButton;
    IncrementalPop: TPopupMenu;
    IncrementalCut: TMenuItem;
    IncrementalCopy: TMenuItem;
    IncrementalPaste: TMenuItem;
    IncrementalSelAll: TMenuItem;
    IncrementalUndo: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    procedure EditChange(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure IncrementalCutClick(Sender: TObject);
    procedure IncrementalCopyClick(Sender: TObject);
    procedure IncrementalPasteClick(Sender: TObject);
    procedure IncrementalSelAllClick(Sender: TObject);
    procedure IncrementalUndoClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditKeyPress(Sender: TObject; var Key: Char);
  private
    fOptions: TSynSearchOptions;
    fSearchEngine: TSynEditSearch;
    fOriginalColor: TColor;
    fEditor: TSynEdit;
    procedure DoSearch;
    procedure SetEditor(value: TSynEdit);
  public
    property Editor: TSynEdit read fEditor write SetEditor;
  end;

var
  IncrementalForm: TIncrementalForm = nil;

implementation

{$R *.dfm}

uses
  main, MultilangSupport;

procedure TIncrementalForm.SetEditor(value: TSynEdit);
begin
  if value <> fEditor then begin

    // remove search engine of old editor
    if Assigned(fEditor) then
      fEditor.SearchEngine := nil;

    fEditor := value;
    fEditor.SearchEngine := fSearchEngine;
  end;
end;

procedure TIncrementalForm.DoSearch;
begin

  // Search forwards...
  if Editor.SearchReplace(Edit.Text, '', fOptions) = 0 then begin

    // Nothing found? Start at top
    Include(fOptions, ssoEntireScope);
    if Editor.SearchReplace(Edit.Text, '', fOptions) = 0 then begin
      Edit.Color := clRed;
      MessageBeep(MB_OK);
    end else
      Edit.Color := fOriginalColor;
  end else
    Edit.Color := fOriginalColor;
end;

procedure TIncrementalForm.EditChange(Sender: TObject);
begin
  if Length(Edit.Text) > 0 then begin

    // Stick with the same word when query changes
    if Editor.SelAvail then
      Editor.CaretX := Editor.BlockBegin.Char;

    // Search forward
    fOptions := [];
    DoSearch;
  end else begin

    // Remove previous find
    Editor.BlockBegin := Editor.BlockEnd;
  end;
end;

procedure TIncrementalForm.btnPrevClick(Sender: TObject);
begin
  if Length(Edit.Text) > 0 then begin

    // Step over current word
    if Editor.SelAvail then
      Editor.CaretX := Editor.BlockBegin.Char;

    fOptions := [ssoBackwards];
    DoSearch;
  end;
end;

procedure TIncrementalForm.btnNextClick(Sender: TObject);
begin
  if Length(Edit.Text) > 0 then begin

    // Step over current word
    if Editor.SelAvail then
      Editor.CaretX := Editor.BlockEnd.Char;

    fOptions := [];
    DoSearch;
  end;
end;

procedure TIncrementalForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  fSearchEngine.Free;
  Action := caFree;
  IncrementalForm := nil;
end;

procedure TIncrementalForm.FormCreate(Sender: TObject);
begin
  fSearchEngine := TSynEditSearch.Create(Self);

  IncrementalUndo.Caption := Lang[ID_ITEM_UNDO];
  IncrementalCut.Caption := Lang[ID_ITEM_CUT];
  IncrementalCopy.Caption := Lang[ID_ITEM_COPY];
  IncrementalPaste.Caption := Lang[ID_ITEM_PASTE];
  IncrementalSelAll.Caption := Lang[ID_ITEM_SELECTALL];
end;

procedure TIncrementalForm.FormShow(Sender: TObject);
begin
  ActiveControl := Edit;
  fOriginalColor := Edit.Color;
end;

procedure TIncrementalForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = Chr(VK_ESCAPE) then begin // Esc
    Key := #0; // Mute beep
    Close;
  end;
end;

procedure TIncrementalForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then begin
    Key := 0; // Stop processing by input window
    if Shift = [ssShift] then
      btnPrevClick(nil)
    else
      btnNextClick(nil);
  end;
end;

procedure TIncrementalForm.IncrementalUndoClick(Sender: TObject);
begin
  Edit.Undo;
end;

procedure TIncrementalForm.IncrementalCutClick(Sender: TObject);
begin
  Edit.CutToClipboard;
end;

procedure TIncrementalForm.IncrementalCopyClick(Sender: TObject);
begin
  Edit.CopyToClipboard;
end;

procedure TIncrementalForm.IncrementalPasteClick(Sender: TObject);
begin
  Edit.PasteFromClipboard;
end;

procedure TIncrementalForm.IncrementalSelAllClick(Sender: TObject);
begin
  Edit.SelectAll;
end;

procedure TIncrementalForm.EditKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = Chr(VK_RETURN) then
    Key := #0;
end;

end.

