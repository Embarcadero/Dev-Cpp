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

unit FunctionSearchFrm;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, CppParser, ComCtrls;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Variants, Classes, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls, QExtCtrls, CppParser, QComCtrls;
{$ENDIF}

type
  TFunctionSearchForm = class(TForm)
    Label1: TLabel;
    txtSearch: TEdit;
    lvEntries: TListView;
    procedure txtSearchChange(Sender: TObject);
    procedure lvEntriesDblClick(Sender: TObject);
    procedure lvEntriesCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure txtSearchKeyPress(Sender: TObject; var Key: Char);
    procedure txtSearchKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    procedure LoadText;
  public
    { Public declarations }
    fParser: TCppParser;
    fFileName: TFileName;
  end;

implementation

uses 
{$IFDEF WIN32}
  MultiLangSupport, devcfg;
{$ENDIF}
{$IFDEF LINUX}
  Xlib, MultiLangSupport, devcfg;
{$ENDIF} 

{$R *.dfm}

procedure TFunctionSearchForm.txtSearchChange(Sender: TObject);
var
	I: integer;
begin
	if not Assigned(fParser) then
		Exit;

	lvEntries.Items.BeginUpdate;
	lvEntries.Items.Clear;

	for I := 0 to fParser.Statements.Count - 1 do
		if PStatement(fParser.Statements[I])^._Kind = skFunction then
			if (PStatement(fParser.Statements[I])^._IsDeclaration and SameText(PStatement(fParser.Statements[I])^._DeclImplFileName, fFilename)) or
			   (not PStatement(fParser.Statements[I])^._IsDeclaration and SameText(PStatement(fParser.Statements[I])^._FileName, fFilename)) then

				if (txtSearch.Text = '') or (Pos(LowerCase(txtSearch.Text), LowerCase(PStatement(fParser.Statements[I])^._ScopelessCmd)) > 0) then begin
					with lvEntries.Items.Add do begin
						ImageIndex := -1;
						case PStatement(fParser.Statements[I])^._ClassScope of
							scsPrivate: StateIndex := 5;
							scsProtected: StateIndex := 6;
							scsPublic: StateIndex := 7;
							scsPublished: StateIndex := 7;
						end;
						SubItems.Add(PStatement(fParser.Statements[I])^._Type);
						SubItems.Add(PStatement(fParser.Statements[I])^._ScopeCmd);
						if PStatement(fParser.Statements[I])^._IsDeclaration then
							SubItems.Add(IntToStr(PStatement(fParser.Statements[I])^._DeclImplLine))
						else
							SubItems.Add(IntToStr(PStatement(fParser.Statements[I])^._Line));
						Data := fParser.Statements[I];
					end;
				end;

	lvEntries.AlphaSort;
	if lvEntries.ItemIndex = -1 then
		if lvEntries.Items.Count > 0 then
				lvEntries.ItemIndex := 0;

	lvEntries.Items.EndUpdate;

	// without this, the user has to press the down arrow twice to
	// move down the listview entries (only the first time!)...
{$IFDEF WIN32}
	lvEntries.Perform(WM_KEYDOWN, VK_DOWN, 0);
{$ENDIF}
{$IFDEF LINUX}
	lvEntries.Perform(WM_KEYDOWN, XK_DOWN, 0);
{$ENDIF}
end;

procedure TFunctionSearchForm.txtSearchKeyPress(Sender: TObject;var Key: Char);
begin
	if lvEntries = nil then Exit;
	case Key of
		Chr(VK_ESCAPE): begin
			ModalResult := mrCancel;
			Key := #0;
		end;
		Chr(VK_RETURN): begin
			ModalResult := mrOK;
			Key := #0;
		end;
	end;
end;

procedure TFunctionSearchForm.txtSearchKeyDown(Sender: TObject;var Key: Word; Shift: TShiftState);
begin
	if lvEntries = nil then Exit;
	case Key of
		VK_DOWN, VK_UP: begin
			lvEntries.Perform(WM_KEYDOWN, Key, 0); // send the key to lventries
		end;
	end;
end;

procedure TFunctionSearchForm.lvEntriesDblClick(Sender: TObject);
begin
	if lvEntries.Selected <> nil then
		ModalResult := mrOK;
end;

procedure TFunctionSearchForm.lvEntriesCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
begin
	Compare := CompareText(Item1.SubItems[1], Item2.SubItems[1]);
end;

procedure TFunctionSearchForm.LoadText;
begin
	// Set interface font
	Font.Name := devData.InterfaceFont;
	Font.Size := devData.InterfaceFontSize;

	Caption := StringReplace(Lang[ID_ITEM_GOTOFUNCTION], '&', '', []);
	Label1.Caption := Lang[ID_GF_TEXT];
	lvEntries.Column[1].Caption := Lang[ID_GF_TYPE];
	lvEntries.Column[2].Caption := Lang[ID_GF_FUNCTION];
	lvEntries.Column[3].Caption := Lang[ID_GF_LINE];
end;

procedure TFunctionSearchForm.FormCreate(Sender: TObject);
begin
	LoadText;
end;

procedure TFunctionSearchForm.FormShow(Sender: TObject);
begin
	txtSearchChange(self);
end;

end.

