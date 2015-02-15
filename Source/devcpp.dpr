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

program devcpp;
{$R 'icons.res' 'icons.rc'}
{$R 'DefaultFiles.res' 'DefaultFiles.rc'}
{%File 'LangIDs.inc'}

uses
  FastMM4 in 'FastMM4.pas',
  Windows,
  Forms,
  sysUtils,
  SHFolder,
  Messages,
  main in 'main.pas' {MainForm},
  MultiLangSupport in 'MultiLangSupport.pas',
  SplashFrm in 'SplashFrm.pas' {SplashForm},
  Version in 'Version.pas',
  Utils in 'Utils.pas',
  Tests in 'Tests.pas',
  LangFrm in 'LangFrm.pas' {LangForm},
  Project in 'Project.pas',
  Templates in 'Templates.pas',
  NewProjectFrm in 'NewProjectFrm.pas' {NewProjectForm},
  RemoveUnitFrm in 'RemoveUnitFrm.pas' {RemoveUnitForm},
  GotoLineFrm in 'GotoLineFrm.pas' {GotoLineForm},
  PrintFrm in 'PrintFrm.pas' {PrintForm},
  AboutFrm in 'AboutFrm.pas' {AboutForm},
  Compiler in 'Compiler.pas',
  devrun in 'devrun.pas',
  ProjectOptionsFrm in 'ProjectOptionsFrm.pas' {ProjectOptionsForm},
  ToolFrm in 'ToolFrm.pas' {ToolFrom},
  ToolEditFrm in 'ToolEditFrm.pas' {ToolEditForm},
  IconFrm in 'IconFrm.pas' {IconForm},
  devcfg in 'devcfg.pas',
  DataFrm in 'DataFrm.pas' {dmMain: TDataModule},
  EditorOptFrm in 'EditorOptFrm.pas' {EditorOptForm},
  CodeInsList in 'CodeInsList.pas',
  IncrementalFrm in 'IncrementalFrm.pas' {IncrementalForm},
  FindFrm in 'FindFrm.pas' {FindForm},
  Editor in 'Editor.pas',
  EnviroFrm in 'EnviroFrm.pas' {EnviroForm},
  DebugReader in 'DebugReader.pas',
  Debugger in 'Debugger.pas',
  EditorList in 'EditorList.pas',
  CFGData in 'CFGData.pas',
  ProjectTypes in 'ProjectTypes.pas',
  Macros in 'Macros.pas',
  devExec in 'devExec.pas',
  NewTemplateFrm in 'NewTemplateFrm.pas' {NewTemplateForm},
  FunctionSearchFrm in 'FunctionSearchFrm.pas' {FunctionSearchForm},
  NewVarFrm in 'NewVarFrm.pas' {NewVarForm},
  NewFunctionFrm in 'NewFunctionFrm.pas' {NewFunctionForm},
  NewClassFrm in 'NewClassFrm.pas' {NewClassForm},
  ProfileAnalysisFrm in 'ProfileAnalysisFrm.pas' {ProfileAnalysisForm},
  FilePropertiesFrm in 'FilePropertiesFrm.pas' {FilePropertiesForm},
  AddToDoFrm in 'AddToDoFrm.pas' {AddToDoForm},
  ViewToDoFrm in 'ViewToDoFrm.pas' {ViewToDoForm},
  ImportMSVCFrm in 'ImportMSVCFrm.pas' {ImportMSVCForm},
  ImportCBFrm in 'ImportCBFrm.pas' {ImportCBForm},
  CPUFrm in 'CPUFrm.pas' {CPUForm},
  FileAssocs in 'FileAssocs.pas',
  TipOfTheDayFrm in 'TipOfTheDayFrm.pas' {TipOfTheDayForm},
  ExceptionFrm in 'ExceptionFrm.pas' {ExceptionFrm},
  WindowListFrm in 'WindowListFrm.pas' {WindowListForm},
  ParamsFrm in 'ParamsFrm.pas' {ParamsForm},
  CompOptionsFrame in 'CompOptionsFrame.pas' {CompOptionsFrame: TFrame},
  CompOptionsFrm in 'CompOptionsFrm.pas' {CompOptionsForm},
  FormatterOptionsFrm in 'FormatterOptionsFrm.pas' {FormatterOptionsForm},
  ProcessListFrm in 'ProcessListFrm.pas' {ProcessListForm},
  PackmanExitCodesU in 'Tools\Packman\PackmanExitCodesU.pas',
  ImageTheme in 'ImageTheme.pas',
  Instances in 'Instances.pas';

{$R *.res}

var
  AppData, INIFileName, ExeFolder: AnsiString;
  Buffer: array[0..MAX_PATH] of char;
  PrevInstance: THandle;
begin
  // Check for previous instances (only allow once instance)
  // If we are able to find a previous instance, activate that one instead
  PrevInstance := GetPreviousInstance;
  if PrevInstance <> 0 then begin
    SendToPreviousInstance(PrevInstance, AnsiString(GetCommandLineW));
    Exit;
  end;

  // Read INI filename
  INIFileName := ChangeFileExt(ExtractFileName(Application.ExeName), INI_EXT);
  ExeFolder := ExtractFilePath(Application.ExeName);

  // Create config files directory
  // Set devData.INIFileName, ConfigMode
  // Did someone pass the -c command to us?
  if (ParamCount >= 2) and SameStr(ParamStr(1), '-c') then begin
    if not DirectoryExists(ParamStr(2)) then
      CreateDir(ParamStr(2));

    // Store the INI file in the directory given to us
    if ParamStr(2)[2] <> ':' then // if a relative path is specified...
      devData.INIFileName := ExeFolder + IncludeTrailingBackslash(ParamStr(2)) + INIFileName
    else
      devData.INIFileName := IncludeTrailingBackslash(ParamStr(2)) + INIFileName;
  end else begin

    // default dir should be %APPDATA%\Dev-Cpp
    AppData := '';
    if SUCCEEDED(SHGetFolderPath(0, CSIDL_APPDATA, 0, 0, Buffer)) then
      AppData := IncludeTrailingBackslash(AnsiString(Buffer));

    // Store the INI file in %APPDATA% or if we are not allowed to do so, in the exe directory
    if (AppData <> '') and (DirectoryExists(AppData + 'Dev-Cpp') or CreateDir(AppData + 'Dev-Cpp')) then
      devData.INIFileName := AppData + 'Dev-Cpp\' + INIFileName
    else
      // store it in the default portable config folder anyways...
      devData.INIFileName := ExeFolder + 'config\' + INIFileName;
  end;

  // free ansistrings...
  SetLength(AppData, 0);
  SetLength(INIFileName, 0);
  SetLength(ExeFolder, 0);

  // Make the caption look nice
  Application.Initialize;
  Application.Title := 'Dev-C++';

  // Create and fill settings structures
  devData.ReadSelf;
  CreateOptions;

  // Display it as soon as possible, and only if its worth viewing...
  if (not devData.NoSplashScreen) or devData.First then
    SplashForm := TSplashForm.Create(nil);

  Application.CreateForm(TMainForm, MainForm);
  if Assigned(SplashForm) then
    SplashForm.Close;

  Application.Run;
end.

