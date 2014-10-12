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

unit Macros;

interface

uses
  SysUtils, devCfg, Version;

function ParseMacros(Str: AnsiString): AnsiString;

implementation

uses
{$IFDEF WIN32}
  Main, editor, Dialogs, Utils, Classes;
{$ENDIF}
{$IFDEF LINUX}
Main, editor, QDialogs, Utils, Classes;
{$ENDIF}

procedure Replace(var Str: AnsiString; Old, New: AnsiString);
begin
  Str := StringReplace(Str, Old, New, [rfReplaceAll]);
end;

function ParseMacros(Str: AnsiString): AnsiString;
var
  e: TEditor;
begin
  Result := Str;
  e := MainForm.EditorList.GetEditor;

  Replace(Result, '<DEFAULT>', devDirs.Default);
  Replace(Result, '<DEVCPP>', ExtractFileDir(ParamStr(0)));
  Replace(Result, '<DEVCPPVERSION>', DEVCPP_VERSION);
  Replace(Result, '<EXECPATH>', devDirs.Exec);
  Replace(Result, '<DATE>', DateToStr(Now));
  Replace(Result, '<DATETIME>', DateTimeToStr(Now));

  // Only provide the first cpp dir
  if Assigned(devCompilerSets.CurrentSet) and (devCompilerSets.CurrentSet.CppDir.Count > 0) then
    Replace(Result, '<INCLUDE>', devCompilerSets.CurrentSet.CppDir[0])
  else
    Replace(Result, '<INCLUDE>', '');

  // Only provide the first lib dir
  if Assigned(devCompilerSets.CurrentSet) and (devCompilerSets.CurrentSet.LibDir.Count > 0) then
    Replace(Result, '<LIB>', devCompilerSets.CurrentSet.LibDir[0])
  else
    Replace(Result, '<LIB>', '');

  // Project-dependent macros
  if Assigned(MainForm.Project) then begin
    Replace(Result, '<EXENAME>', MainForm.Project.Executable);
    Replace(Result, '<PROJECTNAME>', MainForm.Project.Name);
    Replace(Result, '<PROJECTFILE>', MainForm.Project.FileName);
    Replace(Result, '<PROJECTPATH>', MainForm.Project.Directory);
    Replace(Result, '<SOURCESPCLIST>', MainForm.Project.ListUnitStr(' '));
  end else if Assigned(e) then begin // Non-project editor macros
    Replace(Result, '<EXENAME>', '"' + ChangeFileExt(e.FileName, EXE_EXT) + '"');
    Replace(Result, '<PROJECTNAME>', e.FileName);
    Replace(Result, '<PROJECTFILE>', e.FileName);
    Replace(Result, '<PROJECTPATH>', ExtractFilePath(e.FileName));
    Replace(Result, '<SOURCESPCLIST>', ''); // clear unchanged macros
  end else begin // clear unchanged macros
    Replace(Result, '<EXENAME>', '');
    Replace(Result, '<PROJECTNAME>', '');
    Replace(Result, '<PROJECTFILE>', '');
    Replace(Result, '<PROJECTPATH>', '');
    Replace(Result, '<SOURCESPCLIST>', '');
  end;

  // Editor macros
  if Assigned(e) then begin
    Replace(Result, '<SOURCENAME>', e.FileName);
    Replace(Result, '<SOURCENAME>', ExtractFilePath(e.FileName));
    Replace(Result, '<WORDXY>', e.Text.WordAtCursor);
  end else begin // clear unchanged macros
    Replace(Result, '<SOURCENAME>', '');
    Replace(Result, '<SOURCENAME>', '');
    Replace(Result, '<WORDXY>', '');
  end;
end;

end.

