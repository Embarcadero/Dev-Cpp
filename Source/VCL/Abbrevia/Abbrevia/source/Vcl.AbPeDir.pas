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
{* ABBREVIA: Vcl.AbPeDir.pas                             *}
{*********************************************************}

unit Vcl.AbPeDir;

{$I AbDefine.inc}

interface

uses
  Windows,
  Graphics,
  Forms,
  Controls,
  StdCtrls,
  Buttons,
  ExtCtrls,
  DesignIntf,
  DesignEditors,
  SysUtils,
  Classes;

type
  TAbDirectoryProperty = class( TStringProperty )
  public
    function GetAttributes: TPropertyAttributes;
             override;
    procedure Edit;
              override;
  end;

implementation

uses
  AbDlgDir;


function TAbDirectoryProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TAbDirectoryProperty.Edit;
var
  D : TAbDirDlg;
begin
  D := TAbDirDlg.Create(Application);
  try
    D.Caption := 'Directory';
    D.AdditionalText := 'Select Directory';
    if D.Execute then
      Value := D.SelectedFolder;
  finally
    D.Free;
  end;
end;
end.
