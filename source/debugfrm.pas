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

unit debugfrm;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Variants, Classes, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls, QComCtrls;
{$ENDIF}

type
  TDebugForm = class(TForm)
    lvItems: TListView;
    btnClose: TButton;
    procedure FormShow(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure AddItem(const Text, Value: string);
  private

  public
    { Public declarations }
  end;

var
  DebugForm: TDebugForm;

implementation

uses 
  utils, devcfg, main;

{$R *.dfm}

procedure TDebugForm.AddItem(const Text, Value: string);
var
 Item: TListItem;
begin
  Item:= lvItems.Items.Add;
  Item.Caption:= Text;
  Item.SubItems.Add(Value);
end;

procedure TDebugForm.FormShow(Sender: TObject);
begin
  AddItem('Current Path', GetCurrentDir);
  with devDirs do
   begin
     AddItem('devDirs.Exec', Exec);
     AddItem('devDirs.Icons', Icons);
     AddItem('devDirs.Help', ExpandFileto(Help, Exec));
     AddItem('devDirs.Lang', ExpandFileto(Lang, Exec));
     AddItem('devDirs.Templates', ExpandFileto(Templates, Exec));
     AddItem('devDirs.Default', Default);
     AddItem('devDirs.Bin', Bins);
     AddItem('devDirs.C', C);
     AddItem('devDirs.Cpp', Cpp);
     AddItem('devDirs.Lib', Lib);
     AddItem('devDirs.OriginalPath', OriginalPath);
   end;
end;

procedure TDebugForm.btnCloseClick(Sender: TObject);
begin
  close;
end;

end.
