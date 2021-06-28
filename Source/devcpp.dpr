{
    This file is part of Embarcadero Dev-C++
    Copyright (c) 2004 Bloodshed Software

    Embarcadero Dev-C++ is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Embarcadero Dev-C++ is distributed in the hope that it will be useful,
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

uses
  FastMM5 in 'FastMM5.pas',
  Windows,
  Forms,
  sysUtils,
  SHFolder,
  Messages,
  System.IOUtils,
  main in 'main.pas' {MainForm},
  MultiLangSupport in 'MultiLangSupport.pas',
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
  ProcessListFrm in 'ProcessListFrm.pas' {ProcessListForm},
  PackmanExitCodesU in 'Tools\Packman\PackmanExitCodesU.pas',
  ImageTheme in 'ImageTheme.pas',
  Instances in 'Instances.pas',
  CharUtils in 'CharUtils.pas',
  ConsoleAppHostFrm in 'ConsoleAppHostFrm.pas' {ConsoleAppHost},
  Vcl.Themes,
  Vcl.Styles,
  AStyleFormatterOptionsFrm in 'AStyleFormatterOptionsFrm.pas' {AStyleFormatterOptionsForm},
  ClangFormatterOptionsFrm in 'ClangFormatterOptionsFrm.pas' {ClangFormatterOptionsForm};

{$R *.res}

var
  AppData, INIFileName, ExeFolder: String;
//  PrevInstance: THandle;
begin
   if (ParamCount > 1) and ParamStr(1).Equals('INTERNAL_DEL') then
   begin
//     {$I-}
     AllocConsole;
     try
       WriteLn('Start deleting...');
       for var i := 2 to ParamCount do
         if TFile.Exists(ParamStr(i)) then
         begin
           Write('Deleting: ' + ParamStr(i));
           try
             TFile.Delete(ParamStr(i));
             WriteLn(' Ok');
           except
             on E: exception do
               WriteLn(' fail: ' + E.Message);
           end;
         end
         else
           WriteLn('File not found: ' + ParamStr(i));
     finally
       WriteLn('Deleting complete...');
       WriteLn('');
       FreeConsole;
     end;
//     {$I+}
     Exit;
   end;


  // Configure memory manager
  {$IFDEF FASTMM_DEBUG_MODE}
    FastMM_LogToFileEvents := FastMM_LogToFileEvents + [mmetUnexpectedMemoryLeakDetail, mmetUnexpectedMemoryLeakSummary];
    FastMM_MessageBoxEvents := FastMM_MessageBoxEvents + [mmetUnexpectedMemoryLeakSummary];
    if FastMM_LoadDebugSupportLibrary then
    begin
      FastMM_EnterDebugMode;
    end else
    begin
      MessageBox(0, FastMM_DebugSupportLibraryNotAvailableError, FastMM_DebugSupportLibraryNotAvailableError_Caption, MB_OK or MB_ICONERROR or MB_TASKMODAL or MB_DEFAULT_DESKTOP_ONLY);
      Halt(217);
    end;
  {$ENDIF}

  // Check for previous instances (only allow once instance)
  // If we are able to find a previous instance, activate that one instead
  {PrevInstance := GetPreviousInstance;
  if PrevInstance <> 0 then begin
    SendToPreviousInstance(PrevInstance, String(GetCommandLineW));
    Exit;
  end;}

  devData.IsPortable := TPath.GetFileNameWithoutExtension(ParamStr(0)).EndsWith('portable', True)
                        or FindCmdLineSwitch('portable', True)
                        or FindCmdLineSwitch('-portable', True);

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
    // default dir should be %APPDATA%\Embarcadero\Dev-Cpp
    AppData := IncludeTrailingBackslash(TPath.GetHomePath);

    const DevCppDir = 'Embarcadero\Dev-Cpp\';

    // Store the INI file in %APPDATA% or if we are not allowed to do so, in the exe directory
    if (not devData.IsPortable) and ((AppData <> '') and (DirectoryExists(AppData + DevCppDir) or ForceDirectories(AppData + DevCppDir))) then
    begin
      devData.INIFileName := TPath.Combine(TPath.Combine(AppData,DevCppDir),INIFileName);
    end
    else
    begin
      // store it in the default portable config folder anyways...
      devData.INIFileName := ExeFolder + 'config\' + INIFileName;
      TDirectory.CreateDirectory(TPath.GetDirectoryName(devData.INIFileName));
    end;
  end;

  // Load settings
  devData.ReadSelf;
  CreateOptions;

  // Create main window
  Application.Initialize;
  Application.Title := 'Embarcadero Dev C++';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

