(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Abbrevia
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * Roman Kassebaum
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* ABBREVIA: Vcl.AbPeVer.pas                             *}
{*********************************************************}
{* ABBREVIA: Property Editor - Version                   *}
{*********************************************************}

unit Vcl.AbPeVer;

{$I AbDefine.inc}

interface

uses
  Windows,
  ShellAPI,
  Graphics,
  Forms,
  Controls,
  StdCtrls,
  Buttons,
  ExtCtrls,
  Dialogs,
  DesignIntf,
  DesignEditors,
  SysUtils,
  Classes;

type
  TAbAboutBox = class(TForm)
    lblVersion: TLabel;
    Panel1: TPanel;
    Image1: TImage;
    btnOK: TButton;
    Panel2: TPanel;
    WebLbl: TLabel;
    NewsLbl: TLabel;
    Bevel1: TBevel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure WebLblClick(Sender: TObject);
    procedure WebLblMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure NewsLblClick(Sender: TObject);
    procedure NewsLblMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Panel2MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  end;


  TAbVersionProperty = class( TStringProperty )
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

implementation

{$R *.dfm}

uses
  AbArcTyp,
  AbConst,
  AbResString;

procedure TAbAboutBox.FormCreate(Sender: TObject);
begin
  Top := (Screen.Height - Height ) div 3;
  Left := (Screen.Width - Width ) div 2;
  lblVersion.Caption := Format(AbVersionFormatS, [AbVersionS] );
end;

function TAbVersionProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

procedure TAbVersionProperty.Edit;
begin
  with TAbAboutBox.Create( Application ) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TAbAboutBox.btnOKClick(Sender: TObject);
begin
  Close;
end;

procedure TAbAboutBox.WebLblClick(Sender: TObject);
begin
  if ShellExecute(0, 'open', 'https://github.com/TurboPack/Abbrevia', '', '',
    SW_SHOWNORMAL) <= 32 then
    ShowMessage('Unable to start web browser');
  WebLbl.Font.Color := clNavy;
end;

procedure TAbAboutBox.WebLblMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  WebLbl.Font.Color := clRed;
end;

procedure TAbAboutBox.NewsLblClick(Sender: TObject);
begin
  if ShellExecute(0, 'open', 'https://github.com/TurboPack/Abbrevia', '', '',
    SW_SHOWNORMAL) <= 32 then
    ShowMessage('Unable to start web browser');
  NewsLbl.Font.Color := clNavy;
end;

procedure TAbAboutBox.NewsLblMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  NewsLbl.Font.Color := clRed;
end;

procedure TAbAboutBox.Panel2MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  NewsLbl.Font.Color := clNavy;
end;

procedure TAbAboutBox.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  WebLbl.Font.Color := clNavy;
  NewsLbl.Font.Color := clNavy;
end;

end.

