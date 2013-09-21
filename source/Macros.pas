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

function ParseMacros(Str: String): String;

implementation

uses
{$IFDEF WIN32}
  Main, editor, Dialogs, Utils, Classes;
{$ENDIF}
{$IFDEF LINUX}
  Main, editor, QDialogs, Utils, Classes;
{$ENDIF}

procedure Replace(var Str: String; Old, New: String);
begin
  Str := StringReplace(Str, Old, New, [rfReplaceAll]);
end;

function ParseMacros(Str: String): String;
var
  e: TEditor;
  Dir: String;
  StrList: TStringList;
begin
  Result := Str;
  e := MainForm.GetEditor;

  Replace(Result, '<DEFAULT>', devDirs.Default);
  Replace(Result, '<DEVCPP>', ExtractFileDir(ParamStr(0)));
  Replace(Result, '<DEVCPPVERSION>', DEVCPP_VERSION);
  Replace(Result, '<EXECPATH>', devDirs.Exec);
  Replace(Result, '<DATE>', DateToStr(Now));
  Replace(Result, '<DATETIME>', DateTimeToStr(Now));

  Dir := ExtractFilePath(ParamStr(0)) + '\include';
  if (not DirectoryExists(Dir)) and (devDirs.C <> '') then
  begin
      StrList := TStringList.Create;
      StrToList(devDirs.C, StrList);
      Dir := StrList.Strings[0];
      StrList.Free;
  end;
  Replace(Result, '<INCLUDE>', Dir);

  Dir := ExtractFilePath(ParamStr(0)) + '\lib';
  if (not DirectoryExists(Dir)) and (devDirs.Lib <> '') then
  begin
      StrList := TStringList.Create;
      StrToList(devDirs.Lib, StrList);
      Dir := StrList.Strings[0];
      StrList.Free;
  end;
  Replace(Result, '<LIB>', Dir);


  { Project-dependent macros }
  if Assigned(MainForm.fProject) then
  begin
      Replace(Result, '<EXENAME>',       MainForm.fProject.Executable);
      Replace(Result, '<PROJECTNAME>',   MainForm.fProject.Name);
      Replace(Result, '<PROJECTFILE>',   MainForm.fProject.FileName);
      Replace(Result, '<PROJECTPATH>',   MainForm.fProject.Directory);
      Replace(Result, '<SOURCESPCLIST>', MainForm.fProject.ListUnitStr(' '));
  end
  { Non-project editor macros }
  else if Assigned(e) then
  begin
      Replace(Result, '<EXENAME>',       ChangeFileExt(e.FileName, EXE_EXT));
      Replace(Result, '<PROJECTNAME>',   e.FileName);
      Replace(Result, '<PROJECTFILE>',   e.FileName);
      Replace(Result, '<PROJECTPATH>',   ExtractFilePath(e.FileName));

      // clear unchanged macros
      Replace(Result, '<SOURCESPCLIST>', '');
  end else
  begin
      // clear unchanged macros
      Replace(Result, '<EXENAME>',       '');
      Replace(Result, '<PROJECTNAME>',   '');
      Replace(Result, '<PROJECTFILE>',   '');
      Replace(Result, '<PROJECTPATH>',   '');
      Replace(Result, '<SOURCESPCLIST>', '');
  end;

  { Editor macros }
  if Assigned(e) then
  begin
      Replace(Result, '<SOURCENAME>', e.FileName);

      if Length(e.FileName) = 0 then
          Replace(Result, '<SOURCENAME>', devDirs.Default)
      else
          Replace(Result, '<SOURCENAME>', ExtractFilePath(e.FileName));

      Replace(Result, '<WORDXY>', e.GetWordAtCursor);
  end else
  begin
      // clear unchanged macros
      Replace(Result, '<SOURCENAME>', '');
      Replace(Result, '<SOURCENAME>', '');
      Replace(Result, '<WORDXY>',     '');
  end;
end;

end.
