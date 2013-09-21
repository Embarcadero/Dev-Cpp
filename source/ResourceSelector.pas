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

unit ResourceSelector;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Variants, Classes, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls, QButtons;
{$ENDIF}

type
  TSelectResource = class(TForm)
    List: TListBox;
    OkBtn: TBitBtn;
    Cancel: TBitBtn;
    Edit1: TEdit;
    procedure ListClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure ListDblClick(Sender: TObject);
  private
    { Private declarations }
  public
    function Select(Resources: TStringList): String;
    function SelectIndex(Resources: TStringList): Integer;
  end;

var
  SelectResource: TSelectResource;

implementation

{$R *.dfm}

function TSelectResource.Select(Resources: TStringList): String;
var
  i: Integer;
begin
  Result := '';
  if Resources.Count = 0 then
      Exit;

  for i := 0 to Resources.Count - 1 do
      List.Items.Add(ExtractFileName(Resources.Strings[i]));
  List.ItemIndex := 0;

  if ShowModal = mrOK then
      Result := Resources.Strings[List.ItemIndex];
end;

function TSelectResource.SelectIndex(Resources: TStringList): Integer;
var
  i: Integer;
begin
  Result := -1;
  if Resources.Count = 0 then
      Exit;

  for i := 0 to Resources.Count - 1 do
      List.Items.Add(ExtractFileName(Resources.Strings[i]));
  List.ItemIndex := 0;
  Edit1.Text := List.Items[List.ItemIndex];

  if ShowModal = mrOK then
      Result := List.ItemIndex;
end;

procedure TSelectResource.ListClick(Sender: TObject);
begin
  if List.ItemIndex <> -1 then
      Edit1.Text := List.Items[List.ItemIndex];
end;

procedure TSelectResource.Edit1Change(Sender: TObject);
var
  i: Integer;
begin
  if Length(Edit1.Text) = 0 then
  begin
      List.ItemIndex := -1;
      Exit;
  end;

  for i := 0 to List.Items.Count - 1 do
      if CompareText(Copy(List.Items[i], 1, Length(Edit1.Text)),
        Edit1.Text) = 0 then
      begin
          List.ItemIndex := i;
          Exit;
      end;
  List.ItemIndex := -1;
end;

procedure TSelectResource.ListDblClick(Sender: TObject);
var a : TCloseAction;
begin
  if List.ItemIndex > -1 then begin
    ModalResult := mrOk;
    DoClose(a);
  end;
end;

end.
