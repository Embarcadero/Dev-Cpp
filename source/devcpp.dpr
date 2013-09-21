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
{$R 'LangFrm.res' 'LangFrm.rc'}
{%File 'LangIDs.inc'}

uses
  FastMM4 in 'FastMM4.pas',

{$IFDEF WIN32}
  Windows, Forms, sysUtils, SHFolder, Dialogs,
{$ENDIF}
{$IFDEF LINUX}
  QForms, sysUtils, QDialogs,
{$ENDIF}

  main in 'main.pas' {MainForm},
  MultiLangSupport in 'MultiLangSupport.pas',
  SplashFrm in 'SplashFrm.pas' {SplashForm},
  version in 'version.pas',
  utils in 'utils.pas',
  LangFrm in 'LangFrm.pas' {LangForm},
  project in 'project.pas',
  Templates in 'Templates.pas',
  NewProjectFrm in 'NewProjectFrm.pas' {NewProjectForm},
  RemoveUnitFrm in 'RemoveUnitFrm.pas' {RemoveUnitForm},
  GotoLineFrm in 'GotoLineFrm.pas' {GotoLineForm},
  PrintFrm in 'PrintFrm.pas' {PrintForm},
  AboutFrm in 'AboutFrm.pas' {AboutForm},
  compiler in 'compiler.pas',
  devrun in 'devrun.pas',
  ProjectOptionsFrm in 'ProjectOptionsFrm.pas' {ProjectOptionsForm},
  ToolFrm in 'ToolFrm.pas' {ToolFrom},
  ToolEditFrm in 'ToolEditFrm.pas' {ToolEditForm},
  IconFrm in 'IconFrm.pas' {IconForm},
  devcfg in 'devcfg.pas',
  datamod in 'datamod.pas' {dmMain: TDataModule},
  EditorOptFrm in 'EditorOptFrm.pas' {EditorOptForm},
  CodeInsFrm in 'CodeInsFrm.pas' {CodeInsForm},
  IncrementalFrm in 'IncrementalFrm.pas' {IncrementalForm},
  FindFrm in 'FindFrm.pas' {FindForm},
  editor in 'editor.pas',
  EnviroFrm in 'EnviroFrm.pas' {EnviroForm},
  debugreader in 'debugreader.pas',
  debugger in 'debugger.pas',
  CFGData in 'CFGData.pas',
  CFGINI in 'CFGINI.pas',
  CheckForUpdate in 'CheckForUpdate.pas',
  prjtypes in 'prjtypes.pas',
  ResourceSelectorFrm in 'ResourceSelectorFrm.pas' {ResourceSelectorForm},
  Macros in 'Macros.pas',
  devExec in 'devExec.pas',
  NewTemplateFrm in 'NewTemplateFrm.pas' {NewTemplateForm},
  FunctionSearchFrm in 'FunctionSearchFrm.pas' {FunctionSearchForm},
  NewVarFrm in 'NewVarFrm.pas' {NewVarForm},
  NewMemberFrm in 'NewMemberFrm.pas' {NewMemberForm},
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
  ExceptionsAnalyzerFrm in 'ExceptionsAnalyzerFrm.pas' {ExceptionsAnalyzerForm},
  CVSFrm in 'CVSFrm.pas' {CVSForm},
  WindowListFrm in 'WindowListFrm.pas' {WindowListForm},
  CVSThread in 'CVSThread.pas',
  CVSPasswdFrm in 'CVSPasswdFrm.pas' {CVSPasswdForm},
  DevThemes in 'DevThemes.pas',
  ParamsFrm in 'ParamsFrm.pas' {ParamsForm},
  CompOptionsFrame in 'CompOptionsFrame.pas' {CompOptionsFrame: TFrame},
  CompOptionsFrm in 'CompOptionsFrm.pas' {CompOptionsForm},
  CompileProgressFrm in 'CompileProgressFrm.pas' {CompileProgressForm},
  WebThread in 'Tools\webupdate\WebThread.pas',
  WebUpdate in 'Tools\webupdate\WebUpdate.pas' {WebUpdateForm},
  ProcessListFrm in 'ProcessListFrm.pas' {ProcessListForm},
  PackmanExitCodesU in 'Tools\Packman\PackmanExitCodesU.pas',
  ImageTheme in 'ImageTheme.pas';

{$R *.res}

var
	appdata, inifilename, exefolder: AnsiString;
	tempc: array [0..MAX_PATH] of char;
begin
	inifilename := ChangeFileExt(ExtractFileName(Application.ExeName), INI_EXT);
	exefolder := ExtractFilePath(Application.ExeName);

	// Did someone pass the -c command to us?
	if (ParamCount >= 2) and SameStr(ParamStr(1),'-c') then begin
		if not DirectoryExists(ParamStr(2)) then
			CreateDir(ParamStr(2));

		if ParamStr(2)[2] <> ':' then // if a relative path is specified...
			devData.INIFile := exefolder + IncludeTrailingBackslash(ParamStr(2)) + inifilename
		else
			devData.INIFile := IncludeTrailingBackslash(ParamStr(2)) + inifilename;

		ConfigMode := CFG_PARAM;
	end else begin

		// default dir should be %APPDATA%\Dev-Cpp
		appdata := '';
		if SUCCEEDED(SHGetFolderPath(0, CSIDL_APPDATA, 0, 0, tempc)) then
			appdata := IncludeTrailingBackslash(AnsiString(tempc));

		if (appdata <> '') and (DirectoryExists(appdata + 'Dev-Cpp') or CreateDir(appdata + 'Dev-Cpp')) then begin
			devData.INIFile := appdata + 'Dev-Cpp\' + inifilename;
			ConfigMode := CFG_APPDATA;
		end else begin

			// store it in the default portable config folder anyways...
			devData.INIFile := exefolder + 'config\' + inifilename;
			ConfigMode := CFG_EXEFOLDER;
		end;
	end;

	// free ansistrings...
	SetLength(appdata,0);
	SetLength(inifilename,0);
	SetLength(exefolder,0);

	// support for user-defined alternate ini file (permanent, but overriden by command-line -c)
	if ConfigMode <> CFG_PARAM then begin
		StandardConfigFile := devData.INIFile;
		CheckForAltConfigFile(devData.INIFile);
		if UseAltConfigFile and (AltConfigFile<>'') and FileExists(AltConfigFile) then
			devData.INIFile:=AltConfigFile;
	end;

	// Create and fill settings structures
	devData.ReadConfigData; // fill devData
	InitializeOptions;

	// Display it as soon as possible, and only if its worth viewing...
	if (not devData.NoSplashScreen and devCodeCompletion.UseCacheFiles) or devData.First then
		SplashForm := TSplashForm.Create(nil);

	Application.Initialize;
	Application.Title := 'Dev-C++';
	Application.CreateForm(TMainForm, MainForm);
  if Assigned(SplashForm) then
		SplashForm.Close;

	Application.Run;
end.
