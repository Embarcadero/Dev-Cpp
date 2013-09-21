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

unit FunctionSearchFm;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, CppParser, ComCtrls, XPMenu;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Variants, Classes, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls, QExtCtrls, CppParser, QComCtrls;
{$ENDIF}

type
  TFunctionSearchForm = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    txtSearch: TEdit;
    lvEntries: TListView;
    XPMenu: TXPMenu;
    procedure FormShow(Sender: TObject);
    procedure txtSearchChange(Sender: TObject);
    procedure txtSearchExit(Sender: TObject);
    procedure txtSearchKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lvEntriesDblClick(Sender: TObject);
    procedure lvEntriesCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure LoadText;
  public
    { Public declarations }
    fParser: TCppParser;
    fFileName: TFileName;
  end;

var
  FunctionSearchForm: TFunctionSearchForm;

implementation

uses 
{$IFDEF WIN32}
  datamod, MultiLangSupport, devcfg;
{$ENDIF}
{$IFDEF LINUX}
  Xlib, datamod, MultiLangSupport, devcfg;
{$ENDIF} 

{$R *.dfm}

procedure TFunctionSearchForm.FormShow(Sender: TObject);
begin
  txtSearch.Text := '';
  txtSearchChange(nil);
end;

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
      if (PStatement(fParser.Statements[I])^._IsDeclaration and
        (AnsiCompareText(PStatement(fParser.Statements[I])^._DeclImplFileName, fFilename) = 0)) or
        (not PStatement(fParser.Statements[I])^._IsDeclaration and
        (AnsiCompareText(PStatement(fParser.Statements[I])^._FileName, fFilename) = 0)) then
        if (txtSearch.Text = '') or
          (AnsiPos(LowerCase(txtSearch.Text), LowerCase(PStatement(fParser.Statements[I])^._ScopelessCmd)) > 0) then begin
          with lvEntries.Items.Add do begin
            ImageIndex := -1;
            case PStatement(fParser.Statements[I])^._ClassScope of
              scsPrivate: StateIndex := 5;
              scsProtected: StateIndex := 6;
              scsPublic: StateIndex := 7;
              scsPublished: StateIndex := 7;
            end;
            SubItems.Add(PStatement(fParser.Statements[I])^._Type);
            SubItems.Add(PStatement(fParser.Statements[I])^._Command);
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

procedure TFunctionSearchForm.txtSearchExit(Sender: TObject);
begin
  txtSearch.SetFocus;
  txtSearch.SelStart := Length(txtSearch.Text);
end;

procedure TFunctionSearchForm.txtSearchKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if lvEntries = nil then Exit;

  case Key of
{$IFDEF WIN32}
    VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT: begin
{$ENDIF}
{$IFDEF LINUX}
    XK_UP, XK_DOWN, XK_PRIOR, XK_NEXT: begin
{$ENDIF}
        lvEntries.Perform(WM_KEYDOWN, Key, 0);
        Key := 0;
      end;
{$IFDEF WIN32}
    VK_ESCAPE: ModalResult := mrCancel;
    VK_RETURN: if lvEntries.Selected <> nil then ModalResult := mrOK;
{$ENDIF}
{$IFDEF LINUX}
    XK_ESCAPE: ModalResult := mrCancel;
    XK_RETURN: if lvEntries.Selected <> nil then ModalResult := mrOK;
{$ENDIF}
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
  Compare := AnsiCompareText(Item1.SubItems[1], Item2.SubItems[1]);
end;

procedure TFunctionSearchForm.LoadText;
begin
  if devData.XPTheme then
    XPMenu.Active := true
  else
    XPMenu.Active := false;
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

end.

