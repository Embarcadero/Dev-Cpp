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
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ActnList, SynEdit, SynEditTypes, SynEditSearch, ExtCtrls, Menus;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QActnList, QSynEdit, QSynEditTypes;
{$ENDIF}

type
  TfrmIncremental = class(TForm)
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
  public
    Editor : TSynEdit;
    OrgPt : TBufferCoord;
  private
    fOptions : TSynSearchOptions;
    fSearchEngine : TSynEditSearch;
    fOriginalColor : TColor;
    procedure DoSearch;
  end;

var
	frmIncremental : TFrmIncremental = nil;

implementation

{$R *.dfm}

uses
{$IFDEF WIN32}
  main, MultilangSupport;
{$ENDIF}
{$IFDEF LINUX}
  Xlib, main;
{$ENDIF}

procedure TfrmIncremental.DoSearch;
begin
	// When the editor changes, search forwards
	if Editor.SearchReplace(Edit.Text,'',fOptions) = 0 then begin

		// nothing found? wrap around
		Include(fOptions, ssoEntireScope);
		if Editor.SearchReplace(Edit.Text,'',fOptions) = 0 then
			Edit.Color := clRed
		else
			Edit.Color := fOriginalColor;
	end else
		Edit.Color := fOriginalColor;
end;

procedure TfrmIncremental.EditChange(Sender: TObject);
begin
	if Length(Edit.Text) > 0 then begin
		fOptions := [];

		// Stick with the same word when query changes
		if Editor.SelAvail then
			Editor.CaretX := Editor.BlockBegin.Char;

		DoSearch;
	end else begin

		// Remove previous find
		Editor.BlockBegin := Editor.BlockEnd;
	end;
end;

procedure TfrmIncremental.btnPrevClick(Sender: TObject);
begin
	if Length(Edit.Text) > 0 then begin
		// Step over current word
		if Editor.SelAvail then
			Editor.CaretX := Editor.BlockBegin.Char;

		fOptions := [ssoBackWards];
		DoSearch;
	end;
end;

procedure TfrmIncremental.btnNextClick(Sender: TObject);
begin
	if Length(Edit.Text) > 0 then begin
		// Step over current word
		if Editor.SelAvail then
			Editor.CaretX := Editor.BlockEnd.Char;

		fOptions := [];
		DoSearch;
	end;
end;

procedure TfrmIncremental.FormClose(Sender: TObject;var Action: TCloseAction);
begin
	fSearchEngine.Free;
	Action := caFree;
	frmIncremental := nil;
end;

procedure TfrmIncremental.FormCreate(Sender: TObject);
begin
	fSearchEngine := TSynEditSearch.Create(Self);

	IncrementalUndo.Caption := Lang[ID_ITEM_UNDO];
	IncrementalCut.Caption := Lang[ID_ITEM_CUT];
	IncrementalCopy.Caption := Lang[ID_ITEM_COPY];
	IncrementalPaste.Caption := Lang[ID_ITEM_PASTE];
	IncrementalSelAll.Caption := Lang[ID_ITEM_SELECTALL];
end;

procedure TfrmIncremental.FormShow(Sender: TObject);
begin
	editor.SearchEngine := fSearchEngine;
	ActiveControl := Edit;
	fOriginalColor := Edit.Color;
end;

procedure TfrmIncremental.FormKeyPress(Sender: TObject; var Key: Char);
begin
	if Key = Chr(VK_ESCAPE) then begin // Esc
		Key := #0; // Mute beep
		Close;
	end;
end;

procedure TfrmIncremental.FormKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
begin
	if Key = VK_DOWN then begin
		btnNextClick(nil);
		Key := 0; // Stop processing by input window
	end else if Key = VK_UP then begin
		btnPrevClick(nil);
		Key := 0; // Stop processing by input window
	end;
end;

procedure TfrmIncremental.IncrementalUndoClick(Sender: TObject);
begin
	Edit.Undo;
end;

procedure TfrmIncremental.IncrementalCutClick(Sender: TObject);
begin
	Edit.CutToClipboard;
end;

procedure TfrmIncremental.IncrementalCopyClick(Sender: TObject);
begin
	Edit.CopyToClipboard;
end;

procedure TfrmIncremental.IncrementalPasteClick(Sender: TObject);
begin
	Edit.PasteFromClipboard;
end;

procedure TfrmIncremental.IncrementalSelAllClick(Sender: TObject);
begin
	Edit.SelectAll;
end;

end.
