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

unit ParamsFrm;

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
  TParamsForm = class(TForm)
    grpParameters: TGroupBox;
    ParamEdit: TEdit;
    grpHost: TGroupBox;
    HostEdit: TEdit;
    LoadBtn: TSpeedButton;
    OpenDialog: TOpenDialog;
    OkBtn: TBitBtn;
    CancelBtn: TBitBtn;
    procedure LoadBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure LoadText;
    { Private declarations }
  public
    procedure DisableHost;
    { Public declarations }
  end;

var
  ParamsForm: TParamsForm;

implementation

uses
  MultiLangSupport;

{$R *.dfm}

procedure TParamsForm.LoadText;
begin
  Caption := Lang.Strings[ID_PARAM_CAPTION];
  grpParameters.Caption := Lang.Strings[ID_PARAM_PARAMS];
  grpHost.Caption := Lang.Strings[ID_PARAM_HOST];
  OkBtn.Caption := Lang.Strings[ID_BTN_OK];
  CancelBtn.Caption := Lang.Strings[ID_BTN_CANCEL];
end;

procedure TParamsForm.LoadBtnClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    HostEdit.Text := OpenDialog.FileName;
end;

procedure TParamsForm.DisableHost;
begin
  HostEdit.Enabled := false;
  LoadBtn.Enabled := false;
end;

procedure TParamsForm.FormCreate(Sender: TObject);
begin
  LoadText;
end;

end.
