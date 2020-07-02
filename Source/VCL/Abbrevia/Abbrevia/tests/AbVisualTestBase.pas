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
 * Craig Peterson
 *
 * Portions created by the Initial Developer are Copyright (C) 2010
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

unit AbVisualTestBase;

{$I AbDefine.inc}

interface

uses
  {$IFDEF UsingCLX }
  QForms,
  {$ELSE}
  Forms,
  {$ENDIF}
  AbTestFramework;

type
  TAbVisualTestCase = class(TAbCompTestCase)
  private
    FTestForm : TForm;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  public
    property TestForm: TForm read FTestForm;
  end;

implementation

{ TAbVisualTestCase }

procedure TAbVisualTestCase.SetUp;
begin
  inherited;
  FTestForm := TForm.CreateNew(nil, 0);
end;

procedure TAbVisualTestCase.TearDown;
begin
  FTestForm.Free;
  inherited;
end;

end.
