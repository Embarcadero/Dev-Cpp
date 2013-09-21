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
{$R 'LangForm.res' 'LangFrm.rc'}

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
  CompOptionsFrm in 'CompOptionsFrm.pas' {CompOptionsForm},
  ToolFrm in 'ToolFrm.pas' {ToolFrom},
  ToolEditFrm in 'ToolEditFrm.pas' {ToolEditForm},
  IconFrm in 'IconFrm.pas' {IconForm},
  devcfg in 'devcfg.pas',
  datamod in 'datamod.pas' {dmMain: TDataModule},
  EditorOptFrm in 'EditorOptFrm.pas' {EditorOptForm},
  CodeInsFrm in 'CodeInsFrm.pas' {CodeInsForm},
  IncrementalFrm in 'IncrementalFrm.pas' {IncrementalForm},
  Search_Center in 'Search_Center.pas',
  ReplaceFrm in 'ReplaceFrm.pas' {ReplaceForm},
  FindFrm in 'FindFrm.pas' {FindForm},
  editor in 'editor.pas',
  EnviroFrm in 'EnviroFrm.pas' {EnviroForm},
  debugwait in 'debugwait.pas',
  debugreader in 'debugreader.pas',
  debugger in 'debugger.pas',
  CFGData in 'CFGData.pas',
  CFGINI in 'CFGINI.pas',
  CheckForUpdate in 'CheckForUpdate.pas',
  prjtypes in 'prjtypes.pas',
  DebugFrm in 'DebugFrm.pas' {DebugForm},
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
  CompilerOptionsFrame in 'CompilerOptionsFrame.pas' {CompOptionsFrame: TFrame},
  CompileProgressFrm in 'CompileProgressFrm.pas' {CompileProgressForm},
  WebThread in 'webupdate\WebThread.pas',
  WebUpdate in 'webupdate\WebUpdate.pas' {WebUpdateForm},
  ProcessListFrm in 'ProcessListFrm.pas' {ProcessListForm},
  ModifyVarFrm in 'ModifyVarFrm.pas' {ModifyVarForm},
  PackmanExitCodesU in 'packman\PackmanExitCodesU.pas',
  ImageTheme in 'ImageTheme.pas';

{$R *.res}

var
	// ConfigMode moved to devcfg, 'cause I need it in enviroform (for AltConfigFile)
	UserHome, strLocalAppData, strAppData, strIniFile, exefolder: AnsiString;
	tempc: array [0..MAX_PATH] of char;
begin

	strIniFile := ChangeFileExt(ExtractFileName(Application.ExeName), INI_EXT);
	exefolder := ReplaceFirstStr(Application.ExeName,ExtractFileName(Application.ExeName),'');

	if (ParamCount > 0) and (ParamStr(1) = CONFIG_PARAM) then begin
		if not DirectoryExists(ParamStr(2)) then
			CreateDir(ParamStr(2));

		if ParamStr(2)[2] <> ':' then// if a relative path is specified...
			devData.INIFile := exefolder + IncludeTrailingBackslash(ParamStr(2)) + strIniFile
		else
			devData.INIFile := IncludeTrailingBackslash(ParamStr(2)) + strIniFile;
		ConfigMode := CFG_PARAM;
	end else begin
		//default dir should be %APPDATA%\Dev-Cpp
		strLocalAppData := '';
		if SUCCEEDED(SHGetFolderPath(0, CSIDL_LOCAL_APPDATA, 0, 0, tempc)) then
			strLocalAppData := IncludeTrailingBackslash(AnsiString(tempc));

		strAppData := '';
		if SUCCEEDED(SHGetFolderPath(0, CSIDL_APPDATA, 0, 0, tempc)) then
			strAppData := IncludeTrailingBackslash(AnsiString(tempc));

		if (strLocalAppData <> '') and FileExists(strLocalAppData + strIniFile) then begin
			UserHome := strLocalAppData;
			devData.INIFile := UserHome + strIniFile;
			ConfigMode := CFG_USER;
		end else if (strAppData <> '') and FileExists(strAppData + strIniFile) then begin
			UserHome := strAppData;
			devData.INIFile := UserHome + strIniFile;
			ConfigMode := CFG_USER;
		end else if (strAppData <> '') and (DirectoryExists(strAppData + 'Dev-Cpp') or CreateDir(strAppData + 'Dev-Cpp')) then begin
			UserHome := strAppData + 'Dev-Cpp\';
			devData.INIFile := UserHome + strIniFile;
			ConfigMode := CFG_USER;
		end else
			devData.INIFile:= ChangeFileExt(Application.EXEName, INI_EXT);
	end;

	// support for user-defined alternate ini file (permanent, but overriden by command-line -c)
	if ConfigMode <> CFG_PARAM then begin
		StandardConfigFile:=devData.INIFile;
		CheckForAltConfigFile(devData.INIFile);
		if UseAltConfigFile and (AltConfigFile<>'') and FileExists(AltConfigFile) then
			devData.INIFile:=AltConfigFile;
	end;

	InitializeOptions;
	if ConfigMode = CFG_PARAM then begin
		if ParamStr(2)[2] <> ':' then // if a relative path is specified...
			devDirs.Config := exefolder + IncludeTrailingBackslash(ParamStr(2))
		else
			devDirs.Config := IncludeTrailingBackslash(ParamStr(2));
	end else if ConfigMode = CFG_USER then
		devDirs.Config := UserHome;
	devData.ReadConfigData;

	devTheme:= TdevTheme.Create;
	Application.Initialize;
	Application.Title := 'Dev-C++';
	Application.CreateForm(TMainForm, MainForm);

	// Display it a bit later
	if not devData.NoSplashScreen then
		SplashForm := TSplashForm.Create(Application);

	// do the creation stuff when the splashscreen is displayed because it takes quite a while ...
	MainForm.DoCreateEverything;

	if not devData.NoSplashScreen then
		SplashForm.Free;

	Application.Run;
end.
