//**************************************************************************************************
//
// Unit uVCLStylesInfo
// unit uVCLStylesInfo  for the VCL Styles Utils  project
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is uVCLStylesInfo.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2012-2014 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************
unit uVCLStylesInfo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Themes;

type
  TFrmVCLStyleInfoDialog = class(TForm)
    EditName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    EditAuthor: TEdit;
    Label3: TLabel;
    EditEMail: TEdit;
    Label4: TLabel;
    EditURL: TEdit;
    Label5: TLabel;
    EditVersion: TEdit;
    Button1: TButton;
    Button2: TButton;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FStatus   : Boolean;
    FStyleInfo: TStyleInfo;
    procedure SetStyleInfo(const Value: TStyleInfo);
    { Private declarations }
  public
    property  StyleInfo:  TStyleInfo read FStyleInfo write SetStyleInfo;
    function Execute : Boolean;
  end;

implementation

{$R *.dfm}

procedure TFrmVCLStyleInfoDialog.Button1Click(Sender: TObject);
begin
  FStyleInfo.Name       :=EditName.Text;
  FStyleInfo.Author     :=EditAuthor.Text;
  FStyleInfo.AuthorEMail:=EditEMail.Text;
  FStyleInfo.AuthorURL  :=EditURL.Text;
  FStyleInfo.Version    :=EditVersion.Text;
  FStatus:=True;
  Close;
end;

procedure TFrmVCLStyleInfoDialog.Button2Click(Sender: TObject);
begin
 FStatus:=False;
 Close;
end;

function TFrmVCLStyleInfoDialog.Execute: Boolean;
begin
  ShowModal;
  Result:=FStatus;
end;

procedure TFrmVCLStyleInfoDialog.FormCreate(Sender: TObject);
begin
  FStatus:=False;
end;

procedure TFrmVCLStyleInfoDialog.SetStyleInfo(const Value: TStyleInfo);
begin
  FStyleInfo := Value;
  EditName.Text:=FStyleInfo.Name;
  EditAuthor.Text:=FStyleInfo.Author;
  EditEMail.Text:=FStyleInfo.AuthorEMail;
  EditURL.Text:=FStyleInfo.AuthorURL;
  EditVersion.Text:=FStyleInfo.Version;
end;

end.
