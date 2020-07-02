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
 * The Initial Developer of the Original Code is * Robert Love
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

program AbbreviaUnitTests;

{$I AbDefine.inc}

{$DEFINE UsingVCL}
{$IFNDEF UsingVCL}
  {$APPTYPE CONSOLE}
{$ENDIF}

uses
  TestFramework,
  TextTestRunner,
{$IFDEF UsingVCL}
  Forms,
  GUITestRunner,
{$ENDIF}
  AbTestFramework in 'AbTestFramework.pas',
{$IFDEF UsingVCL}
  AbFloppySpanTests in 'AbFloppySpanTests.pas',
  AbCabViewTests in 'AbCabViewTests.pas',
  AbCBrowsTests in 'AbCBrowsTests.pas',
{$ENDIF}
{$IFDEF MSWINDOWS}
  AbCabExtTests in 'AbCabExtTests.pas',
  AbCabMakTests in 'AbCabMakTests.pas',
  AbCabKitTests in 'AbCabKitTests.pas',
  AbCabTypTests in 'AbCabTypTests.pas',
//  AbWinzipTests in 'AbWinzipTests.pas',
{$ENDIF}
{$IFDEF UsingVCL}
  AbVisualTestBase in 'AbVisualTestBase.pas',
  AbZipViewTests in 'AbZipViewTests.pas',
  AbZipOutTests in 'AbZipOutTests.pas',
  AbMeterTests in 'AbMeterTests.pas',
{$ENDIF}
  AbUnzPrcTests in 'AbUnzPrcTests.pas',
  AbZipPrcTests in 'AbZipPrcTests.pas',
  AbTestConsts in 'AbTestConsts.pas',
  AbUnzperTests in 'AbUnzperTests.pas',
  AbZipperTests in 'AbZipperTests.pas',
  AbZipKitTests in 'AbZipKitTests.pas',
  AbZBrowsTests in 'AbZBrowsTests.pas',
  AbSelfExTests in 'AbSelfExTests.pas',
  AbArcTypTests in 'AbArcTypTests.pas',
  AbGzTypTests in 'AbGzTypTests.pas',
  AbArchiveSplitTests in 'AbArchiveSplitTests.pas',
  AbZipTypTests in 'AbZipTypTests.pas',
  AbBzip2Tests in 'AbBzip2Tests.pas',
  AbBzip2TypTests in 'AbBzip2TypTests.pas',
  AbBrowseTests in 'AbBrowseTests.pas',
  AbTarTypTests in 'AbTarTypTests.pas',
{$IF DEFINED(MSWINDOWS) AND NOT DEFINED(FPC)}
  AbLZMATests in 'AbLZMATests.pas',
{$IFEND}
  AbVMStrmTests in 'AbVMStrmTests.pas';

{$R *.res}

begin
  Application.Initialize;
  if System.IsConsole then
    TextTestRunner.RunRegisteredTests
  else
    GUITestRunner.RunRegisteredTests;
end.

