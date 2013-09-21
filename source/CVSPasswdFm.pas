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

unit CVSPasswdFm;

interface

uses
{$IFDEF WIN32}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, XPMenu;
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Variants, Classes, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls;
{$ENDIF}

type
  TCVSPasswdForm = class(TForm)
    Label1: TLabel;
    txtPass: TEdit;
    btnOK: TButton;
    Label2: TLabel;
    XPMenu: TXPMenu;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CVSPasswdForm: TCVSPasswdForm;

implementation

uses 
  devcfg;

{$R *.dfm}

procedure TCVSPasswdForm.FormShow(Sender: TObject);
begin
  if devData.XPTheme then
    XPMenu.Active := true
  else
    XPMenu.Active := false;
  txtPass.Text := '';
  txtPass.SetFocus;
end;

end.

