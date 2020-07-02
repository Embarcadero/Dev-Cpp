
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
 * Robert Love
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)
unit AbMeterTests;
{$I AbDefine.inc}
interface

uses
  AbMeter, AbVisualTestBase;

type

  TAbMeterTests = class(TAbVisualTestCase)
  private
    Component : TAbMeter;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDefaultStreaming;
  end;

implementation

uses
  TestFrameWork, AbTestFrameWork;

{ TAbMeterTests }

procedure TAbMeterTests.SetUp;
begin
  inherited;
  Component := TAbMeter.create(TestForm);
end;

procedure TAbMeterTests.TearDown;
begin
  inherited;
end;

procedure TAbMeterTests.TestDefaultStreaming;
begin
  inherited TestDefaultStreaming(Component);
end;


initialization

  TestFramework.RegisterTests('Abbrevia.Component Level Test Suite',
    [TAbMeterTests.Suite]);

end.

 