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

(* derived from the free pascal editor project source *)
unit Incrementalfrm;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ActnList, SynEdit, SynEditTypes;
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
    procedure FormShow(Sender: TObject);
    procedure EditKeyPress(Sender: TObject; var Key: Char);
    procedure EditKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
    procedure btnPrevClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
  public
    SearchString : string;
    Editor       : TSynEdit;
    OrgPt        : TBufferCoord;
    lastcommand  : integer;
  private
    rOptions : TSynSearchOptions;
  end;

var
  frmIncremental: TfrmIncremental;

implementation

{$R *.dfm}

uses
{$IFDEF WIN32}
  main;
{$ENDIF}
{$IFDEF LINUX}
  Xlib, main;
{$ENDIF}

procedure TfrmIncremental.EditChange(Sender: TObject);
begin
	if Editor.SelAvail then
		Editor.CaretX := Editor.CaretX - Editor.SelLength;

	// Als we niks vinden...
	if Editor.SearchReplace(Edit.Text,'',rOptions) = 0 then begin

		// Zoek dan achterstevoren
		Include(rOptions, ssoBackwards);
		Editor.CaretX := Editor.CaretX + Editor.SelLength;

		// Nog steeds niks? Kleurtje geven
		if Editor.SearchReplace(Edit.Text,'',rOptions) = 0 then
			Edit.Font.Color:=clRed
		else
			Edit.Font.Color:=clBlack;
	end else
		Edit.Font.Color:=clBlack;

	rOptions := [];
	if Length(Edit.Text) = 0 then begin
		Editor.BlockBegin := OrgPt;
		Editor.BlockEnd   := OrgPt;
		Editor.CaretXY    := OrgPt;
	end;
end;

procedure TfrmIncremental.FormShow(Sender: TObject);
begin
	SearchString := Edit.Text;
	Edit.Text    := '';
	OrgPt        := Editor.CaretXY;
	lastcommand  := 2;
end;

procedure TfrmIncremental.EditKeyPress(Sender: TObject; var Key: Char);
begin
	if Key = #27 then Close; // Escape
end;

procedure TfrmIncremental.EditKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
begin
  case Key of
{$IFDEF WIN32}
    VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN : Close;
{$ENDIF}
{$IFDEF LINUX}
    XK_LEFT, XK_RIGHT, XK_UP, XK_DOWN : Close;
{$ENDIF}
  end;
end;

procedure TfrmIncremental.btnPrevClick(Sender: TObject);
begin
	Include(rOptions,ssoBackWards);
	if lastcommand = 1 then
		Editor.SearchReplace(Edit.Text,'',rOptions);
	if Editor.SearchReplace(Edit.Text,'',rOptions) = 0 then begin
		Include(rOptions,ssoEntireScope);
		if Editor.SearchReplace(Edit.Text,'',rOptions) = 0 then
			Edit.Font.Color:=clRed;
	end;
	Exclude(rOptions,ssoEntireScope);
	lastcommand:=0; // 0 == prev
end;

procedure TfrmIncremental.btnNextClick(Sender: TObject);
begin
	Exclude(rOptions,ssoBackWards);
	if lastcommand = 0 then
		Editor.SearchReplace(Edit.Text,'',rOptions);
	if Editor.SearchReplace(Edit.Text,'',rOptions) = 0 then begin
		Include(rOptions,ssoEntireScope);
		if Editor.SearchReplace(Edit.Text,'',rOptions) = 0 then
			Edit.Font.Color:=clRed;
	end;
	Exclude(rOptions,ssoEntireScope);
	lastcommand:=1; // 1 == next
end;

end.
