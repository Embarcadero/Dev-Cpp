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

unit RemoveUnitFrm;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QButtons, QExtCtrls;
{$ENDIF}

type
  TRemoveUnitForm = class(TForm)
    UnitList: TListBox;
    DelBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure UnitListKeyPress(Sender: TObject; var Key: Char);
    procedure DelBtnClick(Sender: TObject);
  end;

implementation

uses 
  MultiLangSupport, devcfg, main;

{$R *.dfm}

procedure TRemoveUnitForm.FormCreate(Sender: TObject);
begin
	// Set interface font
	Font.Name := devData.InterfaceFont;
	Font.Size := devData.InterfaceFontSize;

	Caption := Lang[ID_RU];
	DelBtn.Caption := Lang[ID_ITEM_DELETE];
end;

procedure TRemoveUnitForm.FormClose(Sender: TObject;var Action: TCloseAction);
begin
	Action := caFree;
end;

procedure TRemoveUnitForm.UnitListKeyPress(Sender: TObject; var Key: Char);
begin
	if Key = Chr(VK_DELETE) then
		DelBtnClick(nil);
end;

procedure TRemoveUnitForm.DelBtnClick(Sender: TObject);
var
	I : integer;
begin
	// Delete selection from project
	for i := UnitList.Count - 1 downto 0 do
		if UnitList.Selected[i] then
			MainForm.fProject.Remove(i,true);

	// Delete selection from list
	for I := UnitList.Count - 1 downto 0 do
		if UnitList.Selected[I] then
			UnitList.Items.Delete(I);
end;

end.
