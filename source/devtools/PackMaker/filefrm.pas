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

unit filefrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, FileCtrl;

type
  TFileForm = class(TForm)
    Panel1: TPanel;
    lblSource: TLabel;
    edSource: TEdit;
    lblDest: TLabel;
    OkBtn: TBitBtn;
    CancelBtn: TBitBtn;
    edDest: TComboBox;
    LoadBtn: TSpeedButton;
    OpenDialog: TOpenDialog;
    procedure LoadBtnClick(Sender: TObject);
  private
    { Private declarations }
    Dir : boolean;
  public
    procedure SetMode(d : boolean);
    { Public declarations }
  end;

var
  FileForm: TFileForm;

implementation

uses main;

{$R *.dfm}

procedure TFileForm.SetMode(d : boolean);
begin
  Dir := d;
  if dir then begin
    Caption := 'Add Directory';
  end
  else begin
    Caption := 'Add File';
  end
end;

procedure TFileForm.LoadBtnClick(Sender: TObject);
var s : string;
begin
  if Dir then begin
    if SelectDirectory('Select Directory', ExtractFilePath(MainForm.FileName), s) then
      edSource.Text := ExtractRelativePath(MainForm.FileName, s);
  end
  else begin
    OpenDialog.InitialDir := ExtractFilePath(MainForm.FileName);
    if OpenDialog.Execute then
      edSource.Text := ExtractRelativePath(MainForm.FileName, OpenDialog.FileName);
  end;
end;

end.
