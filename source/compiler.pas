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

unit Compiler;

interface

uses
{$IFDEF WIN32}
  Windows, SysUtils, Dialogs, StdCtrls, Controls, ComCtrls, Forms,
  devrun, version, project, utils, ProjectTypes, Classes, Graphics;
{$ENDIF}
{$IFDEF LINUX}
SysUtils, QDialogs, QStdCtrls, QComCtrls, QForms,
devrun, version, project, utils, prjtypes, Classes, QGraphics;
{$ENDIF}

type
  TLogEntryEvent = procedure(const Msg: AnsiString) of object;
  TOutputEvent = procedure(const _Line, _Col, _Unit, _Message: AnsiString) of object;
  TResOutputEvent = procedure(const _Line, _Col, _Unit, _Message: AnsiString) of object;
  TCompEndEvent = procedure of object;
  TCompSuccessEvent = procedure of object;
  TRunEndEvent = procedure of object;

  TTarget = (ctInvalid, ctNone, ctFile, ctProject);

  TCompiler = class
  private
    fOnLogEntry: TLogEntryEvent;
    fOnOutput: TOutputEvent;
    fOnResOutput: TResOutputEvent;
    fOnCompEnd: TCompEndEvent;
    fOnCompSuccess: TCompSuccessEvent;
    fOnRunEnd: TRunEndEvent;
    fProject: TProject;
    fSourceFile: AnsiString;
    fRunParams: AnsiString;
    fMakefile: AnsiString;
    fTarget: TTarget;
    fErrCount: integer;
    fCheckSyntax: Boolean;
    fWarnCount: integer;
    fStartTime: cardinal;
    fShowOutputInfo: boolean;
    procedure DoLogEntry(const msg: AnsiString);
    procedure DoOutput(const s1, s2, s3, s4: AnsiString);
    procedure DoResOutput(const s1, s2, s3, s4: AnsiString);
    function GetMakeFile: AnsiString;
    function GetCompiling: Boolean;
    procedure RunTerminate(Sender: TObject);
    procedure InitProgressForm;
    procedure ProcessProgressForm(const Line: AnsiString);
    procedure EndProgressForm;
  public
    procedure BuildMakeFile;
    procedure CheckSyntax;
    procedure Compile;
    procedure Run;
    procedure Clean;
    procedure RebuildAll;
    property Compiling: Boolean read GetCompiling;
    property Project: TProject read fProject write fProject;
    property OnLogEntry: TLogEntryEvent read fOnLogEntry write fOnLogEntry;
    property OnOutput: TOutputEvent read fOnOutput write fOnOutput;
    property OnResOutput: TResOutputEvent read fOnResOutput write fOnResOutput;
    property OnCompEnd: TCompEndEvent read fOnCompEnd write fOnCompEnd;
    property OnCompSuccess: TCompSuccessEvent read fOnCompSuccess write fOnCompSuccess;
    property OnRunEnd: TRunEndEvent read fOnRunEnd write fOnRunEnd;
    property SourceFile: AnsiString read fSourceFile write fSourceFile;
    property RunParams: AnsiString read fRunParams write fRunParams; // only for nonproject compilations
    property MakeFile: AnsiString read GetMakeFile;
    property Target: TTarget read fTarget write fTarget;
    property WarningCount: integer read fWarnCount;
    property ErrorCount: integer read fErrCount;
    procedure AbortThread;
  protected
    fCompileParams: AnsiString;
    fCppCompileParams: AnsiString;
    fLibrariesParams: AnsiString;
    fIncludesParams: AnsiString;
    fCppIncludesParams: AnsiString;
    fBinDirs: AnsiString;
    fDevRun: TDevRun;
    fAbortThread: boolean;
    procedure CreateStandardMakeFile; // executable creation
    procedure CreateStaticMakeFile; // static library creation
    procedure CreateDynamicMakeFile; // dynamic library creation
    procedure GetCompileParams;
    procedure GetLibrariesParams;
    procedure GetIncludesParams;
    procedure LaunchThread(const s, dir: AnsiString);
    procedure ThreadCheckAbort(var AbortThread: boolean);
    procedure OnCompilationTerminated(Sender: TObject);
    procedure OnLineOutput(Sender: TObject; const Line: AnsiString);
    procedure ProcessOutput(const line: AnsiString);
    procedure NewMakeFile(var F: TextFile); // create a fits-all makefile
    procedure WriteMakeHeader(var F: TextFile); // append commented header
    procedure WriteMakeDefines(var F: TextFile); // append definitions
    procedure WriteMakeTarget(var F: TextFile); // append target definitions (PHONY, all)
    procedure WriteMakeIncludes(var F: TextFile); // append list of includes
    procedure WriteMakeClean(var F: TextFile); // append cleaning rules
    procedure WriteMakeObjFilesRules(var F: TextFile); // append linkage rules
  end;

implementation

uses
  MultiLangSupport, devcfg, Macros, devExec, main, StrUtils;

procedure TCompiler.DoLogEntry(const msg: AnsiString);
begin
  if Assigned(fOnLogEntry) then
    fOnLogEntry(msg);
end;

procedure TCompiler.DoOutput(const s1, s2, s3, s4: AnsiString);
begin
  if Assigned(fOnOutput) then
    fOnOutput(s1, s2, s3, s4);
end;

procedure TCompiler.DoResOutput(const s1, s2, s3, s4: AnsiString);
begin
  if Assigned(fOnResOutput) then
    fOnResOutput(s1, s2, s3, s4);
end;

function TCompiler.GetMakeFile: AnsiString;
begin
  if not FileExists(fMakeFile) then
    BuildMakeFile;
  result := fMakefile;
end;

procedure TCompiler.BuildMakeFile;
begin
  if not Assigned(fProject) then begin
    fMakeFile := '';
    Exit;
  end else if fProject.Options.UseCustomMakefile then begin
    fMakefile := fProject.Options.CustomMakefile;
    Exit;
  end;

  case fProject.Options.typ of
    dptStat: CreateStaticMakeFile;
    dptDyn: CreateDynamicMakeFile;
  else
    CreateStandardMakeFile;
  end;

  if FileExists(fMakeFile) then
    FileSetDate(fMakefile, DateTimeToFileDate(Now)); // fix the "Clock skew detected" warning ;)
end;

procedure TCompiler.NewMakeFile(var F: TextFile);
begin
  // Create OBJ output directory
  SetCurrentDir(fProject.Directory); // .dev file location
  if fProject.Options.ObjectOutput <> '' then
    if not DirectoryExists(fProject.Options.ObjectOutput) then
      CreateDir(fProject.Options.ObjectOutput);

  fMakefile := fProject.Directory + 'Makefile.win';

  // Write more information to the log file than before
  DoLogEntry(Lang[ID_LOG_BUILDINGMAKEFILE]);
  DoLogEntry('--------');
  DoLogEntry(Format(Lang[ID_LOG_MAKEFILENAME], [fMakefile]));

  // Create the actual file
  Assignfile(F, fMakefile);
  Rewrite(F);

  // Write header
  WriteMakeHeader(F);

  // Writes definition list
  WriteMakeDefines(F);

  // Write PHONY and all targets
  WriteMakeTarget(F);

  // Write list of includes
  WriteMakeIncludes(F);

  // Write clean command
  WriteMakeClean(F);
end;

procedure TCompiler.WriteMakeHeader(var F: TextFile);
begin
  Writeln(F, '# Project: ' + fProject.Name);
  Writeln(F, '# Makefile created by Dev-C++ ' + DEVCPP_VERSION);
  Writeln(F);
  if fCheckSyntax then begin
    Writeln(F, '# This Makefile is written for syntax check!');
    Writeln(F, '# Regenerate it if you want to use this Makefile to build.');
    Writeln(F);
  end;
end;

procedure TCompiler.WriteMakeDefines(var F: TextFile);
var
  Objects, ObjResFile, LinkObjects, ObjFile, RelativeName, OutputFileDir, LibOutputFile: AnsiString;
  I: integer;
begin
  // Get list of object files
  Objects := '';
  LinkObjects := '';

  // Create a list of object files
  for i := 0 to Pred(fProject.Units.Count) do begin

    if not fProject.Units[i].Compile and not fProject.Units[i].Link then
      Continue;

    // Only process source files
    RelativeName := ExtractRelativePath(fProject.FileName, fProject.Units[i].FileName);
    if not (GetFileTyp(RelativeName) in [utcHead, utcppHead, utResSrc]) then begin
      if fProject.Options.ObjectOutput <> '' then begin

        // ofile = C:\MyProgram\obj\main.o
        ObjFile := IncludeTrailingPathDelimiter(fProject.Options.ObjectOutput) +
          ExtractFileName(fProject.Units[i].FileName);
        ObjFile := GenMakePath(ExtractRelativePath(fProject.FileName, ChangeFileExt(ObjFile, OBJ_EXT)), True, True);
        Objects := Objects + ' ' + ObjFile;

        if fProject.Units[i].Link then
          LinkObjects := LinkObjects + ' ' + ObjFile;
      end else begin
        Objects := Objects + ' ' + GenMakePath(ChangeFileExt(RelativeName, OBJ_EXT), True, True);
        if fProject.Units[i].Link then
          LinkObjects := LinkObjects + ' ' + GenMakePath1(ChangeFileExt(RelativeName, OBJ_EXT));
      end;
    end;
  end;

  Objects := Trim(Objects);
  LinkObjects := Trim(LinkObjects);

  // Get windres file
  if Length(fProject.Options.PrivateResource) = 0 then begin
    ObjResFile := '';
  end else begin
    if fProject.Options.ObjectOutput <> '' then begin
      ObjResFile := IncludeTrailingPathDelimiter(fProject.Options.ObjectOutput) +
        ChangeFileExt(fProject.Options.PrivateResource, RES_EXT);
    end else
      ObjResFile := ChangeFileExt(fProject.Options.PrivateResource, RES_EXT);
  end;

  // Mention progress in the logs
  if ObjResFile <> '' then
    DoLogEntry(Format(Lang[ID_LOG_RESFILENAME], [ExpandFileto(ObjResFile, fProject.Directory)]));
  DoLogEntry('');

  // Get list of applicable flags
  GetCompileParams;
  GetLibrariesParams;
  GetIncludesParams;

  if Pos(' -g3', fCompileParams) > 0 then begin
    Writeln(F, 'CPP      = ' + devCompilerSets.CurrentSet.gppName + ' -D__DEBUG__');
    Writeln(F, 'CC       = ' + devCompilerSets.CurrentSet.gccName + ' -D__DEBUG__');
  end else begin
    Writeln(F, 'CPP      = ' + devCompilerSets.CurrentSet.gppName);
    Writeln(F, 'CC       = ' + devCompilerSets.CurrentSet.gccName);
  end;
  Writeln(F, 'WINDRES  = ' + devCompilerSets.CurrentSet.windresName);
  if (ObjResFile <> '') then begin
    Writeln(F, 'RES      = ' + GenMakePath1(ObjResFile));
    Writeln(F, 'OBJ      = ' + Objects + ' $(RES)');
    Writeln(F, 'LINKOBJ  = ' + LinkObjects + ' $(RES)');
  end else begin
    Writeln(F, 'OBJ      = ' + Objects);
    Writeln(F, 'LINKOBJ  = ' + LinkObjects);
  end;
  Writeln(F, 'LIBS     = ' + StringReplace(fLibrariesParams, '\', '/', [rfReplaceAll]));
  Writeln(F, 'INCS     = ' + StringReplace(fIncludesParams, '\', '/', [rfReplaceAll]));
  Writeln(F, 'CXXINCS  = ' + StringReplace(fCppIncludesParams, '\', '/', [rfReplaceAll]));
  Writeln(F, 'BIN      = ' + GenMakePath1(ExtractRelativePath(Makefile, fProject.Executable)));
  Writeln(F, 'CXXFLAGS = $(CXXINCS) ' + fCppCompileParams);
  Writeln(F, 'CFLAGS   = $(INCS) ' + fCompileParams);
  Writeln(F, 'RM       = ' + CLEAN_PROGRAM + ' -f'); // TODO: use del or rm?

  // This needs to be put in before the clean command.
  if fProject.Options.typ = dptDyn then begin
    OutputFileDir := ExtractFilePath(Project.Executable);
    LibOutputFile := OutputFileDir + 'lib' + ExtractFileName(Project.Executable);
    if FileSamePath(LibOutputFile, Project.Directory) then
      LibOutputFile := ExtractFileName(LibOutputFile)
    else
      LibOutputFile := ExtractRelativePath(Makefile, LibOutputFile);

    Writeln(F, 'DEF      = ' + GenMakePath1(ChangeFileExt(LibOutputFile, DEF_EXT)));
    Writeln(F, 'STATIC   = ' + GenMakePath1(ChangeFileExt(LibOutputFile, LIB_EXT)));
  end;
  Writeln(F);
end;

procedure TCompiler.WriteMakeTarget(var F: TextFile);
begin
  if fCheckSyntax then
    Writeln(F, '.PHONY: all all-before all-after clean clean-custom $(OBJ) $(BIN)')
  else
    Writeln(F, '.PHONY: all all-before all-after clean clean-custom');
  Writeln(F);
  Writeln(F, 'all: all-before $(BIN) all-after');
  Writeln(F);
end;

procedure TCompiler.WriteMakeIncludes(var F: TextFile);
var
  I: integer;
begin
  for i := 0 to fProject.Options.MakeIncludes.Count - 1 do
    Writeln(F, 'include ' + GenMakePath1(fProject.Options.MakeIncludes.Strings[i]));
  if fProject.Options.MakeIncludes.Count > 0 then
    Writeln(F);
end;

procedure TCompiler.WriteMakeObjFilesRules(var F: TextFile);
var
  i: integer;
  FileName, ShortFileName, ObjFileName, BuildCmd, ResFiles, ResIncludes, ResFile, PrivResName, WindresArgs: AnsiString;
begin
  for i := 0 to pred(fProject.Units.Count) do begin
    if not fProject.Units[i].Compile then
      Continue;

    // skip resource files
    if GetFileTyp(fProject.Units[i].FileName) = utResSrc then
      Continue;

    // Get unit filename relative to project or relative to makefile
    FileName := fProject.Units[i].FileName;
    if FileSamePath(FileName, fProject.Directory) then
      ShortFileName := ExtractFileName(FileName)
    else
      ShortFileName := ExtractRelativePath(Makefile, FileName);

    // Only process source files
    if GetFileTyp(ShortFileName) in [utcSrc, utcppSrc] then begin
      Writeln(F);

      // Get obj filename (e.g. obj/main.o)
      if fProject.Options.ObjectOutput <> '' then begin
        ObjFileName := IncludeTrailingPathDelimiter(fProject.Options.ObjectOutput) +
          ExtractFileName(fProject.Units[i].FileName);
        ObjFileName := GenMakePath1(ExtractRelativePath(fProject.FileName, ChangeFileExt(ObjFileName, OBJ_EXT)));
      end else
        ObjFileName := GenMakePath1(ChangeFileExt(ShortFileName, OBJ_EXT));

      // objectfile: sourcefile
      Writeln(F, GenMakePath2(ObjFileName) + ': ' + GenMakePath2(ShortFileName));

      // Write custom build command
      if fProject.Units[i].OverrideBuildCmd and (fProject.Units[i].BuildCmd <> '') then begin
        BuildCmd := fProject.Units[i].BuildCmd;
        BuildCmd := StringReplace(BuildCmd, '<CRTAB>', #10#9, [rfReplaceAll]);
        Writeln(F, #9 + BuildCmd);

        // Or roll our own
      end else begin
        if fCheckSyntax then begin
          if fProject.Units[i].CompileCpp then
            Writeln(F, #9 + '$(CPP) -c ' + GenMakePath1(ShortFileName) + ' $(CXXFLAGS)')
          else
            Writeln(F, #9 + '$(CC) -c ' + GenMakePath1(ShortFileName) + ' $(CFLAGS)');
        end else begin
          if fProject.Units[i].CompileCpp then
            Writeln(F, #9 + '$(CPP) -c ' + GenMakePath1(ShortFileName) + ' -o ' + ObjFileName + ' $(CXXFLAGS)')
          else
            Writeln(F, #9 + '$(CC) -c ' + GenMakePath1(ShortFileName) + ' -o ' + ObjFileName + ' $(CFLAGS)');
        end;
      end;
    end;
  end;

  if (Length(fProject.Options.PrivateResource) > 0) then begin

    // Concatenate all resource include directories
    ResIncludes := ' ';
    for i := 0 to fProject.Options.ResourceIncludes.Count - 1 do begin
      ShortFileName := GetShortName(fProject.Options.ResourceIncludes[i]);
      if ShortFileName <> '' then
        ResIncludes := ResIncludes + ' --include-dir ' + GenMakePath1(ShortFileName);
    end;

    // Concatenate all resource filenames (not created when syntax checking)
    if not fCheckSyntax then begin
      ResFiles := '';
      for i := 0 to fProject.Units.Count - 1 do begin
        if GetFileTyp(fProject.Units[i].FileName) <> utResSrc then
          Continue;
        ResFile := ExtractRelativePath(fProject.Executable, fProject.Units[i].FileName);
        if FileExists(GetRealPath(ResFile, fProject.Directory)) then
          ResFiles := ResFiles + GenMakePath2(ResFile) + ' ';
      end;
      ResFiles := Trim(ResFiles);
    end;

    // Determine resource output file
    if fProject.Options.ObjectOutput <> '' then
      ObjFileName := IncludeTrailingPathDelimiter(fProject.Options.ObjectOutput) +
        ChangeFileExt(fProject.Options.PrivateResource, RES_EXT)
    else
      ObjFileName := ChangeFileExt(fProject.Options.PrivateResource, RES_EXT);
    ObjFileName := GenMakePath1(ExtractRelativePath(fProject.FileName, ObjFileName));
    PrivResName := GenMakePath1(ExtractRelativePath(fProject.FileName, fProject.Options.PrivateResource));

    // Build final cmd
    if ContainsStr(fCompileParams, '-m32') then
      WindresArgs := ' -F pe-i386'
    else
      WindresArgs := '';
    if fCheckSyntax then begin
      Writeln(F);
      Writeln(F, ObjFileName + ':');
      Writeln(F, #9 + '$(WINDRES) -i ' + PrivResName + WindresArgs + ' --input-format=rc -o nul -O coff' + ResIncludes)
    end else begin
      Writeln(F);
      Writeln(F, ObjFileName + ': ' + PrivResName + ' ' + ResFiles);
      Writeln(F, #9 + '$(WINDRES) -i ' + PrivResName + WindresArgs + ' --input-format=rc -o ' + ObjFileName + ' -O coff'
        + ResIncludes);
    end;
    Writeln(F);
  end;
end;

procedure TCompiler.WriteMakeClean(var F: TextFile);
begin
  Writeln(F, 'clean: clean-custom');
  case fProject.Options.typ of
    dptDyn:
      Writeln(F, #9 + '${RM} $(OBJ) $(BIN) $(DEF) $(STATIC)');
  else
    Writeln(F, #9 + '${RM} $(OBJ) $(BIN)');
  end;
  Writeln(F);
end;

procedure TCompiler.CreateStandardMakefile;
var
  F: TextFile;
begin
  try
    NewMakeFile(F);
    Writeln(F, '$(BIN): $(OBJ)');
    if not fCheckSyntax then
      if fProject.Options.useGPP then
        Writeln(F, #9 + '$(CPP) $(LINKOBJ) -o $(BIN) $(LIBS)')
      else
        Writeln(F, #9 + '$(CC) $(LINKOBJ) -o $(BIN) $(LIBS)');
    WriteMakeObjFilesRules(F);
  finally
    CloseFile(F);
  end;
end;

procedure TCompiler.CreateStaticMakeFile;
var
  F: TextFile;
begin
  try
    NewMakeFile(F);
    Writeln(F, '$(BIN): $(LINKOBJ)');
    if not fCheckSyntax then begin
      Writeln(F, #9 + 'ar r $(BIN) $(LINKOBJ)');
      Writeln(F, #9 + 'ranlib $(BIN)');
    end;
    WriteMakeObjFilesRules(F);
  finally
    CloseFile(F);
  end;
end;

procedure TCompiler.CreateDynamicMakeFile;
var
  F: TextFile;
begin
  try
    NewMakeFile(F);
    Writeln(F, '$(BIN): $(LINKOBJ)');
    if not fCheckSyntax then begin
      if fProject.Options.useGPP then
        Writeln(F, #9 +
          '$(CPP) -shared $(LINKOBJ) -o $(BIN) $(LIBS) -Wl,--output-def,$(DEF),--out-implib,$(STATIC),--add-stdcall-alias')
      else
        Writeln(F, #9 +
          '$(CC) -shared $(LINKOBJ) -o $(BIN) $(LIBS) -Wl,--output-def,$(DEFFILE),--out-implib,$(STATICLIB),--add-stdcall-alias')
    end;
    WriteMakeObjFilesRules(F);
  finally
    CloseFile(F);
  end;
end;

procedure TCompiler.GetCompileParams;
var
  I, val: integer;
  option: TCompilerOption;
begin
  with devCompilerSets.CurrentSet do begin

    // Force syntax checking when we have to
    if fCheckSyntax then begin
      fCompileParams := '-fsyntax-only';
      fCppCompileParams := '-fsyntax-only';
    end else begin
      fCompileParams := '';
      fCppCompileParams := '';
    end;

    for I := 0 to OptionList.Count - 1 do begin

      option := PCompilerOption(OptionList[I])^;

      // consider project specific options for the compiler, else global compiler options
      if (Assigned(fProject) and (I < Length(fProject.Options.CompilerOptions))) or (not Assigned(fProject) and
        (option.Value > 0)) then begin
        if option.IsC then begin
          if Assigned(option.Choices) then begin
            if Assigned(fProject) then
              val := CharToValue(fProject.Options.CompilerOptions[I + 1])
            else
              val := option.Value;
            if (val > 0) and (val < option.Choices.Count) then
              fCompileParams := fCompileParams + ' ' + option.Setting +
                option.Choices.Values[option.Choices.Names[val]];
          end else if (Assigned(fProject) and (StrToIntDef(fProject.Options.CompilerOptions[I + 1], 0) = 1)) or (not
            Assigned(fProject)) then begin
            fCompileParams := fCompileParams + ' ' + option.Setting;
          end;
        end;
        if option.IsCpp then begin
          if Assigned(option.Choices) then begin
            if Assigned(fProject) then
              val := CharToValue(fProject.Options.CompilerOptions[I + 1])
            else
              val := option.Value;
            if (val > 0) and (val < option.Choices.Count) then
              fCppCompileParams := fCppCompileParams + ' ' + option.Setting +
                option.Choices.Values[option.Choices.Names[val]];
          end else if (Assigned(fProject) and (StrToIntDef(fProject.Options.CompilerOptions[I + 1], 0) = 1)) or (not
            Assigned(fProject)) then begin
            fCppCompileParams := fCppCompileParams + ' ' + option.Setting;
          end;
        end;
      end;
    end;

    // Add custom commands inherited from Tools >> Compiler Options
    if (Length(CompOpts) > 0) and AddtoComp then begin
      fCompileParams := fCompileParams + ' ' + CompOpts;
      fCppCompileParams := fCppCompileParams + ' ' + CompOpts;
    end;

    // Add custom commands at the end so the advanced user can control everything
    if Assigned(fProject) and (fTarget = ctProject) then begin
      if Length(fProject.Options.CompilerCmd) > 0 then
        fCompileParams := fCompileParams + ' ' + Trim(StringReplace(fProject.Options.CompilerCmd, '_@@_', ' ',
          [rfReplaceAll]));
      if Length(fProject.Options.CppCompilerCmd) > 0 then
        fCppCompileParams := fCppCompileParams + ' ' + Trim(StringReplace(fProject.Options.CppCompilerCmd, '_@@_', ' ',
          [rfReplaceAll]));
    end;

    fCompileParams := Trim(ParseMacros(fCompileParams));
    fCppCompileParams := Trim(ParseMacros(fCppCompileParams));
  end;
end;

procedure TCompiler.CheckSyntax;
begin
  fCheckSyntax := True;
  Compile;
  fCheckSyntax := False;
end;

procedure TCompiler.Compile;
resourcestring
  // windres, input, output
  cResourceCmdLine = '%s --input-format=rc -i %s -o %s';
  // gcc, input, compileparams, includeparams, librariesparams
  cSyntaxCmdLine = '%s "%s" %s %s %s';
  // gcc, input, compileparams, includeparams, librariesparams
  cHeaderCmdLine = '%s "%s" %s %s %s';
  // gcc, input, output, compileparams, includeparams, librariesparams
  cSourceCmdLine = '%s "%s" -o "%s" %s %s %s';
  // make, makefile
  cMakeLine = '%s -f "%s" all';
var
  cmdline: AnsiString;
  s: AnsiString;
begin
  case Target of
    ctFile: begin
        InitProgressForm;

        DoLogEntry(Lang[ID_LOG_COMPILINGFILE]);
        DoLogEntry('--------');
        DoLogEntry(Format(Lang[ID_LOG_SOURCEFILE], [fSourceFile]));
        DoLogEntry(Format(Lang[ID_LOG_COMPILERNAME], [devCompilerSets.CurrentSet.Name]));
        DoLogEntry('');

        // Gather commands to pass to gcc/g++
        GetCompileParams;
        GetLibrariesParams;
        GetIncludesParams;

        // Determine command line to execute
        case GetFileTyp(fSourceFile) of
          utResSrc: begin
              s := devCompilerSets.CurrentSet.windresName;
              if fCheckSyntax then
                cmdline := Format(cResourceCmdLine, [s, fSourceFile, 'nul'])
              else
                cmdline := Format(cResourceCmdLine, [s, fSourceFile, ChangeFileExt(fSourceFile, OBJ_EXT)]);

              DoLogEntry(Lang[ID_LOG_PROCESSINGRES]);
              DoLogEntry('--------');
              DoLogEntry(Format(Lang[ID_LOG_WINDRESNAME],
                [IncludeTrailingPathDelimiter(devCompilerSets.CurrentSet.BinDir[0]) + s]));
              DoLogEntry(Format(Lang[ID_LOG_COMMAND], [cmdLine]));
            end;
          utcSrc: begin
              s := devCompilerSets.CurrentSet.gccName;
              if fCheckSyntax then
                cmdline := Format(cSyntaxCmdLine, [s, fSourceFile, fCompileParams, fIncludesParams, fLibrariesParams])
              else
                cmdline := Format(cSourceCmdLine, [s, fSourceFile, ChangeFileExt(fSourceFile, EXE_EXT), fCompileParams,
                  fIncludesParams, fLibrariesParams]);

              DoLogEntry(Lang[ID_LOG_PROCESSINGCSRC]);
              DoLogEntry('--------');
              DoLogEntry(Format(Lang[ID_LOG_GCCNAME],
                [IncludeTrailingPathDelimiter(devCompilerSets.CurrentSet.BinDir[0]) + s]));
              DoLogEntry(Format(Lang[ID_LOG_COMMAND], [cmdLine]));
            end;
          utCppSrc: begin
              s := devCompilerSets.CurrentSet.gppName;
              if fCheckSyntax then
                cmdline := Format(cSyntaxCmdLine, [s, fSourceFile, fCppCompileParams, fCppIncludesParams,
                  fLibrariesParams])
              else
                cmdline := Format(cSourceCmdLine, [s, fSourceFile, ChangeFileExt(fSourceFile, EXE_EXT),
                  fCppCompileParams, fCppIncludesParams, fLibrariesParams]);

              DoLogEntry(Lang[ID_LOG_PROCESSINGCPPSRC]);
              DoLogEntry('--------');
              DoLogEntry(Format(Lang[ID_LOG_GPPNAME],
                [IncludeTrailingPathDelimiter(devCompilerSets.CurrentSet.BinDir[0]) + s]));
              DoLogEntry(Format(Lang[ID_LOG_COMMAND], [cmdLine]));
            end;
        else begin // any header files
            s := devCompilerSets.CurrentSet.gppName;
            if fCheckSyntax then
              cmdline := Format(cSyntaxCmdLine, [s, fSourceFile, fCppCompileParams, fCppIncludesParams,
                fLibrariesParams])
            else
              cmdline := Format(cHeaderCmdLine, [s, fSourceFile, fCompileParams, fIncludesParams,
                fLibrariesParams]);

            DoLogEntry(Lang[ID_LOG_PROCESSINGHEADER]);
            DoLogEntry('--------');
            DoLogEntry(Format(Lang[ID_LOG_GCCNAME],
              [IncludeTrailingPathDelimiter(devCompilerSets.CurrentSet.BinDir[0]) + s]));
            DoLogEntry(Format(Lang[ID_LOG_COMMAND], [cmdLine]));
          end;
        end;

        // Execute it
        LaunchThread(cmdline, ExtractFilePath(fSourceFile));
      end;
    ctProject: begin
        InitProgressForm;

        DoLogEntry(Lang[ID_LOG_PROJECTCOMPILE]);
        DoLogEntry('--------');
        DoLogEntry(Format(Lang[ID_LOG_PROJECTFILE], [fProject.FileName]));
        DoLogEntry(Format(Lang[ID_LOG_COMPILERNAME], [devCompilerSets.CurrentSet.Name]));
        DoLogEntry('');

        BuildMakeFile;
        cmdline := Format(cMakeLine, [devCompilerSets.CurrentSet.makeName, fMakeFile]);

        DoLogEntry(Lang[ID_LOG_PROCESSINGMAKE]);
        DoLogEntry('--------');
        DoLogEntry(Format(Lang[ID_LOG_MAKEFILEPROC],
          [IncludeTrailingPathDelimiter(devCompilerSets.CurrentSet.BinDir[0]) + devCompilerSets.CurrentSet.MakeName]));
        DoLogEntry(Format(Lang[ID_LOG_COMMAND], [cmdLine]));
        DoLogEntry('');

        // Execute it
        LaunchThread(cmdline, ExtractFilePath(Project.FileName));
      end;
  end;
end;

procedure TCompiler.RunTerminate(Sender: TObject);
begin
  Application.Restore;

  OnRunEnd;

  MainForm.UpdateAppTitle;
end;

procedure TCompiler.Run;
var
  FileToRun: AnsiString;
  Parameters: AnsiString;
begin
  case fTarget of
    ctNone:
      Exit;
    ctFile: begin
        // Determine file to execute
        case GetFileTyp(fSourceFile) of
          utcSrc, utcppSrc: begin
              FileToRun := ChangeFileExt(fSourceFile, EXE_EXT);
            end;
        else begin
            Exit; // nothing to run...
          end;
        end;

        // Check if it exists
        if not FileExists(FileToRun) then begin
          if MainForm.actCompRun.Enabled then begin // suggest a compile
            if MessageDlg(Lang[ID_ERR_SRCNOTCOMPILEDSUGGEST], mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
              MainForm.actCompRunExecute(nil);
            end;
          end else
            MessageDlg(Lang[ID_ERR_SRCNOTCOMPILED], mtWarning, [mbOK], 0);
        end else begin

          // Pause programs if they contain a console
          if devData.ConsolePause and ProgramHasConsole(FileToRun) then begin
            Parameters := '"' + FileToRun + '" ' + fRunParams;
            FileToRun := devDirs.Exec + 'ConsolePauser.exe';
          end else begin
            Parameters := fRunParams;
            FileToRun := FileToRun;
          end;

          if devData.MinOnRun then
            Application.Minimize;
          devExecutor.ExecuteAndWatch(FileToRun, Parameters, ExtractFilePath(fSourceFile), True, INFINITE,
            RunTerminate);
          MainForm.UpdateAppTitle;
        end;
      end;
    ctProject: begin
        if fProject.Options.typ = dptStat then
          MessageDlg(Lang[ID_ERR_NOTEXECUTABLE], mtError, [mbOK], 0)
        else if not FileExists(fProject.Executable) then begin
          if MainForm.actCompRun.Enabled then begin // suggest a compile
            if MessageDlg(Lang[ID_ERR_PROJECTNOTCOMPILEDSUGGEST], mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
              MainForm.actCompRunExecute(nil); // move this to mainform?
            end;
          end else
            MessageDlg(Lang[ID_ERR_SRCNOTCOMPILED], mtWarning, [mbOK], 0);
        end else if fProject.Options.typ = dptDyn then begin
          if fProject.Options.HostApplication = '' then
            MessageDlg(Lang[ID_ERR_HOSTMISSING], mtWarning, [mbOK], 0)
          else if not FileExists(fProject.Options.HostApplication) then
            MessageDlg(Lang[ID_ERR_HOSTNOTEXIST], mtWarning, [mbOK], 0)
          else begin // execute DLL's host application
            if devData.MinOnRun then
              Application.Minimize;
            devExecutor.ExecuteAndWatch(fProject.Options.HostApplication, fRunParams,
              ExtractFileDir(fProject.Options.HostApplication), True, INFINITE, RunTerminate);
            MainForm.UpdateAppTitle;
          end;
        end else begin // execute normally
          if devData.ConsolePause and ProgramHasConsole(fProject.Executable) then begin
            Parameters := '"' + fProject.Executable + '" ' + fProject.Options.CmdLineArgs;
            FileToRun := devDirs.Exec + 'ConsolePauser.exe';
          end else begin
            Parameters := fRunParams;
            FileToRun := fProject.Executable;
          end;

          if devData.MinOnRun then
            Application.Minimize;
          devExecutor.ExecuteAndWatch(FileToRun, Parameters, ExtractFileDir(fProject.Executable), True, INFINITE,
            RunTerminate);
          MainForm.UpdateAppTitle;
        end;
      end;
  end;
end;

procedure TCompiler.Clean;
const
  cCleanLine = '%s clean -f "%s"';
  cmsg = 'make clean';
var
  cmdLine: AnsiString;
  FileName: AnsiString;
begin
  case fTarget of
    ctFile: begin
        InitProgressForm;

        DoLogEntry(Lang[ID_LOG_CLEANINGFILE]);
        DoLogEntry('--------');
        DoLogEntry(Format(Lang[ID_LOG_SOURCEFILE], [fSourceFile]));
        DoLogEntry(Format(Lang[ID_LOG_COMPILERNAME], [devCompilerSets.CurrentSet.Name]));
        DoLogEntry('');

        FileName := '';
        case GetFileTyp(fSourceFile) of
          utresSrc: begin
              FileName := ChangeFileExt(fSourceFile, OBJ_EXT);
            end;
          utcSrc: begin
              FileName := ChangeFileExt(fSourceFile, EXE_EXT);
            end;
          utcppSrc: begin
              FileName := ChangeFileExt(fSourceFile, EXE_EXT);
            end;
        else begin
            FileName := fSourceFile + GCH_EXT;
          end;
        end;
        if FileExists(FileName) then begin
          DoLogEntry(Format(Lang[ID_LOG_CLEANEDFILE], [FileName]));
          DeleteFile(FileName);
        end else
          DoLogEntry(Lang[ID_LOG_NOCLEANFILE]);
      end;
    ctProject: begin
        InitProgressForm;

        DoLogEntry(Lang[ID_LOG_CLEANINGPROJECT]);
        DoLogEntry('--------');
        DoLogEntry(Format(Lang[ID_LOG_PROJECTFILE], [fProject.FileName]));
        DoLogEntry(Format(Lang[ID_LOG_COMPILERNAME], [devCompilerSets.CurrentSet.Name]));
        DoLogEntry('');

        // Try to create a makefile that cleans for the whole project...
        BuildMakeFile;
        if not FileExists(fMakefile) then begin
          DoLogEntry(Lang[ID_ERR_NOMAKEFILE]);
          DoLogEntry(Lang[ID_ERR_CLEANFAILED]);
          MessageBox(MainForm.Handle, PAnsiChar(Lang[ID_ERR_NOMAKEFILE]), PAnsiChar(Lang[ID_ERROR]), MB_OK or
            MB_ICONERROR);
          Exit;
        end;

        cmdLine := Format(cCleanLine, [devCompilerSets.CurrentSet.makeName, fMakeFile]);

        DoLogEntry(Lang[ID_LOG_PROCESSINGMAKE]);
        DoLogEntry('--------');
        DoLogEntry(Format(Lang[ID_LOG_MAKEFILEPROC],
          [IncludeTrailingPathDelimiter(devCompilerSets.CurrentSet.BinDir[0]) + devCompilerSets.CurrentSet.MakeName]));
        DoLogEntry(Format(Lang[ID_LOG_COMMAND], [cmdLine]));
        DoLogEntry('');

        // Let make parse the makefile
        LaunchThread(cmdLine, fProject.Directory);
      end;
  end;
end;

procedure TCompiler.RebuildAll; // TODO: unite with TCompiler.Clean?
const
  cCleanLine = '%s -f "%s" clean all';
var
  cmdLine: AnsiString;
begin
  case Target of
    ctFile: begin
        Compile;
      end;
    ctProject: begin
        InitProgressForm;

        DoLogEntry(Lang[ID_LOG_REBUILDINGPROJECT]);
        DoLogEntry('--------');
        DoLogEntry(Format(Lang[ID_LOG_PROJECTFILE], [fProject.FileName]));
        DoLogEntry(Format(Lang[ID_LOG_COMPILERNAME], [devCompilerSets.CurrentSet.Name]));
        DoLogEntry('');

        // Try to create a makefile for the whole project...
        BuildMakeFile;
        if not FileExists(fMakefile) then begin
          DoLogEntry(Lang[ID_ERR_NOMAKEFILE]);
          DoLogEntry(Lang[ID_ERR_CLEANFAILED]);
          MessageBox(MainForm.Handle, PAnsiChar(Lang[ID_ERR_NOMAKEFILE]), PAnsiChar(Lang[ID_ERROR]), MB_OK or
            MB_ICONERROR);
          Exit;
        end;

        cmdLine := Format(cCleanLine, [devCompilerSets.CurrentSet.makeName, fMakeFile]);

        DoLogEntry(Lang[ID_LOG_PROCESSINGMAKE]);
        DoLogEntry('--------');
        DoLogEntry(Format(Lang[ID_LOG_MAKEFILEPROC],
          [IncludeTrailingPathDelimiter(devCompilerSets.CurrentSet.BinDir[0]) + devCompilerSets.CurrentSet.MakeName]));
        DoLogEntry(Format(Lang[ID_LOG_COMMAND], [cmdLine]));
        DoLogEntry('');

        // Let make parse the makefile
        LaunchThread(cmdLine, fProject.Directory);
      end;
  end;
end;

procedure TCompiler.LaunchThread(const s, dir: AnsiString);
begin
  if Assigned(fDevRun) then
    MessageDlg(Lang[ID_MSG_ALREADYCOMP], mtInformation, [mbOK], 0)
  else begin
    fAbortThread := False;
    fDevRun := TDevRun.Create(true);
    fDevRun.Command := s;
    fDevRun.Directory := dir;
    fDevRun.OnTerminate := OnCompilationTerminated;
    fDevRun.OnLineOutput := OnLineOutput;
    fDevRun.OnCheckAbort := ThreadCheckAbort;
    fDevRun.FreeOnTerminate := True;
    fDevRun.Resume;

    MainForm.UpdateAppTitle;
  end;
end;

procedure TCompiler.ThreadCheckAbort(var AbortThread: boolean);
begin
  AbortThread := fAbortThread;
end;

procedure TCompiler.AbortThread;
begin
  if not Assigned(fDevRun) then
    Exit;
  fAbortThread := True;
end;

procedure TCompiler.OnCompilationTerminated(Sender: TObject);
begin
  OnCompEnd;

  fDevRun := nil;

  MainForm.UpdateAppTitle;

  EndProgressForm;

  if (fErrCount = 0) and not fAbortThread then
    OnCompSuccess;
end;

procedure TCompiler.OnLineOutput(Sender: TObject; const Line: AnsiString);
var
  List: TStringList;
  I: integer;
begin
  DoLogEntry(Line);

  List := TStringList.Create;
  List.Text := Line;
  for I := 0 to List.Count - 1 do begin
    ProcessOutput(List.Strings[I]);
    ProcessProgressForm(List.Strings[I]);
  end;
  List.Free;
end;

procedure TCompiler.ProcessOutput(const line: AnsiString);
var
  OLine, OCol, OFile, OMsg, S: AnsiString;
  delim: integer;
  CurrentSet: TdevCompilerSet;

  procedure GetFileName; // obtain delimiter AFTER (full) filename
  begin
    OMsg := Trim(OMsg);
    if (Length(OMsg) > 2) and (OMsg[2] = ':') then begin // full file path at start, ignore this one
      delim := FPos(':', OMsg, 3);
    end else begin // find first
      delim := FPos(':', OMsg, 1);
    end;

    if delim > 0 then begin
      OFile := Copy(OMsg, 1, delim - 1);
      Delete(OMsg, 1, delim);
    end;
  end;

  procedure GetLineNumber;
  begin
    OMsg := Trim(OMsg);
    delim := FPos(':', OMsg, 1);
    if delim = 0 then
      delim := FPos(',', OMsg, 1);
    if delim > 0 then begin
      OLine := Copy(OMsg, 1, delim - 1);
      if StrToIntDef(OLine, -1) = -1 then // don't accept
        OLine := ''
      else
        Delete(OMsg, 1, delim);
    end;
  end;

  procedure GetColNumber;
  begin
    OMsg := Trim(OMsg);
    delim := FPos(':', OMsg, 1);
    if delim = 0 then
      delim := FPos(',', OMsg, 1);
    if delim > 0 then begin
      OCol := Copy(OMsg, 1, delim - 1);
      if StrToIntDef(OCol, -1) = -1 then // don't accept
        OCol := ''
      else
        Delete(OMsg, 1, delim);
    end;
  end;

  procedure GetMessageType;
  begin
    OMsg := Trim(OMsg);
    delim := FPos(':', OMsg, 1);
    if delim > 0 then begin
      S := Copy(OMsg, 1, delim - 1);
      if SameStr(S, 'error') or SameStr(S, 'fatal error') then begin
        Inc(fErrCount);
        Delete(OMsg, 1, delim + 1);
        OMsg := '[Error] ' + Trim(OMsg);
      end else if SameStr(S, 'warning') then begin
        Inc(fWarnCount);
        Delete(OMsg, 1, delim + 1);
        OMsg := '[Warning] ' + Trim(OMsg);
      end else if SameStr(S, 'info') then begin
        //Inc(fInfoCount);
        Delete(OMsg, 1, delim + 1);
        OMsg := '[Info] ' + Trim(OMsg);
      end else if SameStr(S, 'note') then begin
        //Inc(fInfoCount);
        Delete(OMsg, 1, delim + 1);
        OMsg := '[Note] ' + Trim(OMsg);
      end;
    end;
  end;

begin
  CurrentSet := devCompilerSets.CurrentSet;

  OLine := '';
  OCol := '';
  OFile := '';
  OMsg := Trim(Line);

  // Ignore generic 'we are starting program x' messages
  if (Pos(CurrentSet.gccName + ' ', Line) = 1) or
    (Pos(CurrentSet.gppName + ' ', Line) = 1) or
    (Pos(CurrentSet.makeName, Line) = 1) or // ignore all make errors for now
  (Pos(CurrentSet.windresName + ' ', Line) = 1) or
    (Pos(CLEAN_PROGRAM + ' ', Line) = 1) then
    Exit;

  // Direction strings
  if StartsStr('In file included from ', OMsg) then begin
    Delete(OMsg, 1, Length('In file included from '));

    GetFileName;
    GetLineNumber;
    GetColNumber;

    OMsg := 'In file included from ' + OFile;
    DoOutput(OLine, OCol, OFile, OMsg);
    Exit;
  end else if StartsStr('from ', OMsg) then begin
    Delete(OMsg, 1, Length('from '));

    GetFileName;
    GetLineNumber;
    GetColNumber;

    OMsg := '                 from ' + OFile;
    DoOutput(OLine, OCol, OFile, OMsg);
    Exit;
  end;

  // Ignore code snippets that GCC 4.8 produces
  if (Length(line) > 0) and (Line[1] = ' ') then // they always start with a space
    Exit;

  GetFileName; // assume regular main.cpp:line:col: message

  if SameStr(OFile, 'windres.exe') then begin // resource error
    GetFileName;
    GetLineNumber;
    Inc(fErrCount); // assume it's always an error

    DoResOutput(OLine, '', OFile, OMsg);
  end else begin
    GetLineNumber;
    GetColNumber;
    GetMessageType;

    DoOutput(OLine, OCol, OFile, OMsg);
  end;
end;

procedure TCompiler.GetLibrariesParams;
resourcestring
  cAppendStr = '%s -L"%s"';
var
  i, val: integer;
  option: TCompilerOption;
  CurrentSet: TdevCompilerSet;
begin
  CurrentSet := devCompilerSets.CurrentSet;

  // Add libraries
  fLibrariesParams := FormatList(CurrentSet.LibDir, cAppendStr);

  // Add global compiler linker extras
  if CurrentSet.AddtoLink and (Length(CurrentSet.LinkOpts) > 0) then
    fLibrariesParams := fLibrariesParams + ' ' + CurrentSet.LinkOpts;

  // Add libs added via project
  if (fTarget = ctProject) and assigned(fProject) then begin
    for i := 0 to pred(fProject.Options.Libs.Count) do
      fLibrariesParams := format(cAppendStr, [fLibrariesParams, fProject.Options.Libs[i]]);

    // got sick of "symbol 'blah blah' is deprecated"
    if fProject.Options.typ = dptGUI then
      fLibrariesParams := fLibrariesParams + ' -mwindows';

    // Add project compiler linker extras
    if Length(fProject.Options.LinkerCmd) > 0 then
      fLibrariesParams := fLibrariesParams + ' ' + StringReplace(fProject.Options.LinkerCmd, '_@@_', ' ', [rfReplaceAll])
  end;

  fLibrariesParams := Trim(fLibrariesParams);

  // Add project settings that need to be passed to the linker
  for I := 0 to CurrentSet.OptionList.Count - 1 do begin
    option := PCompilerOption(CurrentSet.OptionList[I])^;
    if (Assigned(fProject) and (I < Length(fProject.Options.CompilerOptions))) or (not Assigned(fProject) and
      (option.Value > 0)) then begin
      if option.IsLinker then
        if Assigned(option.Choices) then begin
          if Assigned(fProject) then
            val := CharToValue(fProject.Options.CompilerOptions[I + 1])
          else
            val := option.Value;
          if (val > 0) and (val < option.Choices.Count) then
            fLibrariesParams := fLibrariesParams + ' ' + option.Setting +
              option.Choices.Values[option.Choices.Names[val]];
        end else if (Assigned(fProject) and (StrToIntDef(fProject.Options.CompilerOptions[I + 1], 0) = 1)) or (not
          Assigned(fProject)) then
          fLibrariesParams := fLibrariesParams + ' ' + option.Setting;
    end;
  end;

  fLibrariesParams := Trim(fLibrariesParams);
end;

procedure TCompiler.GetIncludesParams;
resourcestring
  cAppendStr = '%s -I"%s"';
var
  i: integer;
  CurrentSet: TdevCompilerSet;
begin
  CurrentSet := devCompilerSets.CurrentSet;

  fIncludesParams := FormatList(CurrentSet.CDir, cAppendStr);
  fCppIncludesParams := FormatList(CurrentSet.CppDir, cAppendStr);

  if (fTarget = ctProject) and assigned(fProject) then
    for i := 0 to pred(fProject.Options.Includes.Count) do
      if DirectoryExists(fProject.Options.Includes[i]) then begin
        fIncludesParams := format(cAppendStr, [fIncludesParams, fProject.Options.Includes[i]]);
        fCppIncludesParams := format(cAppendStr, [fCppIncludesParams, fProject.Options.Includes[i]]);
      end;

  fIncludesParams := Trim(fIncludesParams);
  fCppIncludesParams := Trim(fCppIncludesParams);
end;

function TCompiler.GetCompiling: Boolean;
begin
  Result := fDevRun <> nil;
end;

procedure TCompiler.InitProgressForm;
var
  numsourcefiles, I: integer;
begin
  // Count file types
  if Assigned(fProject) then begin
    numsourcefiles := 0;
    for I := 0 to fProject.Units.Count - 1 do
      if GetFileTyp(fProject.Units[I].FileName) in [utcSrc, utcppSrc] then
        Inc(numsourcefiles);

    MainForm.pbCompilation.Min := 0;
    MainForm.pbCompilation.Max := numsourcefiles + 3; // cleaning + all project units + linking output + private resource
    MainForm.pbCompilation.Position := 0;
  end else
    MainForm.pbCompilation.Max := 1; // just fSourceFile

  // Initialize counters
  fStartTime := GetTickCount;
  fWarnCount := 0;
  fErrCount := 0;

  // Set some preferences
  fShowOutputInfo := not fCheckSyntax;
end;

procedure TCompiler.ProcessProgressForm(const Line: AnsiString);
var
  filename: AnsiString;
  I: integer;
  Done: boolean;
  CurrentSet: TdevCompilerSet;
begin
  CurrentSet := devCompilerSets.CurrentSet;
  Done := false;

  // The compiler started to compile a new file
  if StartsStr(CurrentSet.gppName + ' ', Line) or StartsStr(CurrentSet.gccName + ' ', Line) then begin
    filename := '';
    if Assigned(fProject) then begin
      for I := 0 to fProject.Units.Count - 1 do begin
        filename := ExtractFilename(fProject.Units[I].FileName);
        if Pos(filename, Line) > 0 then begin
          MainForm.pbCompilation.StepIt;
          Done := true;
          break;
        end;
      end;
    end else if Pos(fSourceFile, Line) > 0 then begin
      MainForm.pbCompilation.StepIt;
      Done := true;
    end;

    if not Done then begin // might be the linker
      if Assigned(fProject) then
        filename := ExtractFileName(fProject.Executable)
      else
        filename := fSourceFile;

      if ContainsStr(Line, filename) then begin
        MainForm.pbCompilation.StepIt;
      end;
    end;
  end else if StartsStr(CLEAN_PROGRAM + ' ', Line) then begin // Cleaning obj files
    MainForm.pbCompilation.StepIt;
  end else if StartsStr(CurrentSet.windresName + ' ', Line) and Assigned(fProject) then begin // Resource files
    filename := ExtractFileName(fProject.Options.PrivateResource);
    if ContainsStr(Line, filename) then begin
      MainForm.pbCompilation.StepIt;
    end;
  end;
end;

procedure TCompiler.EndProgressForm;
var
  CompileTime: Extended; // fp
  FileName: AnsiString;
begin
  MainForm.pbCompilation.Position := 0;

  CompileTime := (GetTickCount - fStartTime) / 1000;

  DoLogEntry('');
  DoLogEntry(Lang[ID_LOG_COMPILERESULTS]);
  DoLogEntry('--------');
  DoLogEntry(Format(Lang[ID_LOG_ERRORS], [fErrCount]));
  DoLogEntry(Format(Lang[ID_LOG_WARNINGS], [fWarnCount]));

  // Check if the output file has been created
  if Assigned(fProject) then
    FileName := ExpandFileto(fProject.Executable, fProject.Directory)
  else begin
    case GetFileTyp(fSourceFile) of
      utresSrc: begin
          FileName := ChangeFileExt(fSourceFile, OBJ_EXT);
        end;
      utcSrc: begin
          FileName := ChangeFileExt(fSourceFile, EXE_EXT);
        end;
      utcppSrc: begin
          FileName := ChangeFileExt(fSourceFile, EXE_EXT);
        end;
    else begin
        FileName := fSourceFile + GCH_EXT;
      end;
    end;
  end;

  // Only show information if we managed to create the file and if it was the result of the current compilation
  if fShowOutputInfo and FileExists(FileName) then begin
    DoLogEntry(Format(Lang[ID_LOG_OUTPUTFILE], [FileName]));
    DoLogEntry(Format(Lang[ID_LOG_OUTPUTSIZE], [FormatFileSize(GetFileSize(FileName))]));
  end;
  DoLogEntry(Format(Lang[ID_LOG_COMPILETIME], [CompileTime]));

  if Assigned(fProject) then
    fProject.SaveToLog;
end;

end.

