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
  StdCtrls, ActnList, SynEdit, SynEditTypes, SynEditSearch, ExtCtrls;
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
    procedure EditChange(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  public
    Editor : TSynEdit;
    OrgPt : TBufferCoord;
    lastcommand : integer;
    OriginalColor : TColor;
  private
    rOptions : TSynSearchOptions;
    fSearchEngine : TSynEditSearch;
    procedure DoSearch;
  end;

var
	frmIncremental : TFrmIncremental = nil;

implementation

{$R *.dfm}

uses
{$IFDEF WIN32}
  main;
{$ENDIF}
{$IFDEF LINUX}
  Xlib, main;
{$ENDIF}

procedure TfrmIncremental.DoSearch;
begin
	// When the editor changes, search forwards
	if Editor.SearchReplace(Edit.Text,'',rOptions) = 0 then begin

		// nothing found? wrap around
		Include(rOptions, ssoEntireScope);
		if Editor.SearchReplace(Edit.Text,'',rOptions) = 0 then
			Edit.Color := clRed
		else
			Edit.Color := OriginalColor;
	end else
		Edit.Color := OriginalColor;
end;

procedure TfrmIncremental.EditChange(Sender: TObject);
begin
	rOptions := [];

	// Stick with the same word when query changes
	if Editor.SelAvail then
		Editor.CaretX := Editor.BlockBegin.Char;

	DoSearch;
end;

procedure TfrmIncremental.btnPrevClick(Sender: TObject);
begin
	rOptions := [ssoBackWards];
	DoSearch;
end;

procedure TfrmIncremental.btnNextClick(Sender: TObject);
begin
	rOptions := [];
	DoSearch;
end;

procedure TfrmIncremental.FormClose(Sender: TObject;var Action: TCloseAction);
begin
	fSearchEngine.Free;
	Action := caFree;
end;

procedure TfrmIncremental.FormCreate(Sender: TObject);
begin
	fSearchEngine := TSynEditSearch.Create(Self);
end;

procedure TfrmIncremental.FormShow(Sender: TObject);
begin
	editor.SearchEngine := fSearchEngine;
	ActiveControl := Edit;
	OriginalColor := Edit.Color;
end;

procedure TfrmIncremental.FormKeyPress(Sender: TObject; var Key: Char);
begin
	if Key = #27 then begin // Esc
		Key := #0; // Mute beep
		Close;
	end;
end;

end.
