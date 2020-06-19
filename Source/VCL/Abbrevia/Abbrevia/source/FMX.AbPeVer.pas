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
{* ABBREVIA: FMX.AbPeVer.pas                             *}
{*********************************************************}
{* ABBREVIA: Property Editor - Version                   *}
{*********************************************************}

unit FMX.AbPeVer;

{$I AbDefine.inc}

interface

uses
  System.SysUtils, System.Types, DesignIntf, DesignEditors, FMX.Forms, FMX.StdCtrls,
  FMX.Objects, System.Classes, FMX.Types, FMX.Controls, FMX.Controls.Presentation;

type
  TAbAboutBox = class(TForm)
    lblVersion: TLabel;
    Panel1: TPanel;
    Image1: TImage;
    btnOK: TButton;
    Panel2: TPanel;
    WebLbl: TLabel;
    NewsLbl: TLabel;
    Bevel1: TPanel;
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
    procedure NewsLblClick(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure NewsLblMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure Panel2MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure WebLblMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
  end;


  TAbVersionProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

implementation

{$R *.fmx}

uses
  System.UITypes, Winapi.Windows, Winapi.ShellApi, FMX.DialogService.Sync, AbArcTyp, AbConst,
  AbResString;

{ TAbAboutBox }

procedure TAbAboutBox.FormCreate(Sender: TObject);
begin
  Top := (Screen.Height - Height ) div 3;
  Left := (Screen.Width - Width ) div 2;
  lblVersion.Text := Format(AbVersionFormatS, [AbVersionS] );
end;

procedure TAbAboutBox.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  WebLbl.FontColor := TColorRec.Navy;
  NewsLbl.FontColor := TColorRec.Navy;
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
    TDialogServiceSync.ShowMessage('Unable to start web browser');
  WebLbl.FontColor := TColorRec.Navy;
end;

procedure TAbAboutBox.NewsLblClick(Sender: TObject);
begin
  if ShellExecute(0, 'open', 'https://github.com/TurboPack/Abbrevia', '', '',
    SW_SHOWNORMAL) <= 32 then
    TDialogServiceSync.ShowMessage('Unable to start web browser');
  NewsLbl.FontColor := TColorRec.Navy;
end;

procedure TAbAboutBox.NewsLblMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  NewsLbl.FontColor := TColorRec.Red;
end;

procedure TAbAboutBox.Panel2MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  NewsLbl.FontColor := TColorRec.Navy;
end;

procedure TAbAboutBox.WebLblMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  WebLbl.FontColor := TColorRec.Red;
end;

end.

