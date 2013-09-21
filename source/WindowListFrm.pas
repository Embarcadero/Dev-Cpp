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

unit WindowListFrm;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ComCtrls;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QButtons, QExtCtrls;
{$ENDIF}

type
  TWindowListForm = class(TForm)
    Panel: TPanel;
    OkBtn: TBitBtn;
    CancelBtn: TBitBtn;
    UnitList: TListView;
    procedure FormCreate(Sender: TObject);
    procedure UnitListDblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure UnitListKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure UnitListCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure UnitListColumnClick(Sender: TObject; Column: TListColumn);

  private
    Descending: Boolean;
    SortedColumn: Integer;
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

procedure TWindowListForm.FormCreate(Sender: TObject);
begin
	// Set interface font
	Font.Name := devData.InterfaceFont;
	Font.Size := devData.InterfaceFontSize;

	Caption:=           Lang[ID_WL];
	OkBtn.Caption :=    Lang[ID_BTN_OK];
	CancelBtn.Caption:= Lang[ID_BTN_CANCEL];
end;

procedure TWindowListForm.UnitListDblClick(Sender: TObject);
begin
	if UnitList.ItemIndex <> -1 then
		ModalResult := mrOk;
end;

procedure TWindowListForm.FormShow(Sender: TObject);
begin
	UnitList.SetFocus;
end;

procedure TWindowListForm.UnitListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
{$IFDEF WIN32}
  if Key=vk_Return then
{$ENDIF}
{$IFDEF LINUX}
  if Key = XK_RETURN then
{$ENDIF}
    UnitListDblClick(Sender);
end;

procedure TWindowListForm.FormClose(Sender: TObject;var Action: TCloseAction);
begin
	Action := caFree;
end;

procedure TWindowListForm.UnitListCompare(Sender: TObject; Item1,Item2: TListItem; Data: Integer; var Compare: Integer);
begin
	if SortedColumn = 0 then
		Compare := AnsiCompareText(Item1.Caption, Item2.Caption)
	else
		Compare := AnsiCompareText(Item1.SubItems[SortedColumn-1], Item2.SubItems[SortedColumn-1]);

	if Descending then
		Compare := -Compare;
end;

procedure TWindowListForm.UnitListColumnClick(Sender: TObject;Column: TListColumn);
begin
	TListView(Sender).SortType := stNone;

	if Column.Index <> SortedColumn then begin
		SortedColumn := Column.Index;
		Descending := False;
	end else
		Descending := not Descending;

	TListView(Sender).SortType := stText;
end;

end.
