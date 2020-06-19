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

unit AbCabViewTests;

{$I AbDefine.inc}

interface

uses
  AbCView, AbVisualTestBase;

type

  TAbCabViewTests = class(TAbVisualTestCase)
  private
    Component : TAbCabView;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDefaultStreaming;
    procedure TestComponentLinks;
  end;

implementation

uses
  Menus, TestFrameWork, AbCBrows, AbTestFrameWork;

{ TAbCabViewTests }

procedure TAbCabViewTests.SetUp;
begin
  inherited;
  Component := TAbCabView.Create(TestForm);
end;

procedure TAbCabViewTests.TearDown;
begin
  Component.Free;
  inherited;
end;

procedure TAbCabViewTests.TestComponentLinks;
begin
  TestComponentLink(Component, 'PopupMenu', TPopupMenu);
  TestComponentLink(Component, 'CabComponent', TAbCabBrowser);
end;

procedure TAbCabViewTests.TestDefaultStreaming;
begin
  inherited TestDefaultStreaming(Component);
end;

initialization

  TestFramework.RegisterTest('Abbrevia.Component Level Test Suite',
    TAbCabViewTests.Suite);
 
end.

 