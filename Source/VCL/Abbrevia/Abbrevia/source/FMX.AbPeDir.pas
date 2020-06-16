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
{* ABBREVIA: FMX.AbPeDir.pas                             *}
{*********************************************************}

unit FMX.AbPeDir;

{$I AbDefine.inc}

interface

uses
  Windows,
  FMX.Graphics,
  FMX.Forms,
  FMX.Controls,
  FMX.StdCtrls,
  FMX.ExtCtrls,
  DesignIntf,
  DesignEditors,
  SysUtils,
  Classes;

type
  TAbDirectoryProperty = class( TStringProperty )
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

implementation

uses
  FMX.Dialogs.Win;

{ TAbDirectoryProperty }

function TAbDirectoryProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TAbDirectoryProperty.Edit;
var
  pDialog: TFileOpenDialog;
begin
  pDialog := TFileOpenDialog.Create(nil);
  try
    pDialog.Title := 'Select Directory';
    pDialog.Options := [TFileDialogOption.PickFolders, TFileDialogOption.PathMustExist, TFileDialogOption.ForceFileSystem];
    pDialog.OkButtonLabel := 'Select';
    pDialog.DefaultFolder := Value;
    pDialog.FileName := Value;
    if pDialog.Execute then
       Value := pDialog.FileName;
  finally
    Free;
  end;
end;

end.
