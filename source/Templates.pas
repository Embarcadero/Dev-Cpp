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

unit Templates;

interface

uses 
  Classes, Types, IniFiles, prjtypes;

type
 TTemplateUnit = record
  CName: string;
  CppName: string;
  CText: string;
  CppText: string;
 end;

 TTemplateRec = record
  CCaret: TPoint;
  CppCaret: TPoint;
  IsCPP: boolean;   // TRUE = C++ / FALSE = C
  CText: string;
  CppText: string;
 end;

 TTemplate = class
  private
   fFileName: string;
   fOptions: TProjOptions;
   fName: string;
   fDesc: string;
   fCatagory: string;
   fPrjName: string;
   fProjectIcon: String;
   fIconIndex: integer;

   fTemplate: TmeminiFile;
   fVersion: integer;

   function GetUnitCount: integer;
   function GetOldData: TTemplateRec;
   function GetVersion: integer;
   function GetUnit(index: integer): TTemplateUnit;
   procedure SetUnit(index: integer; value: TTemplateUnit);
   procedure SetOldData(value: TTemplateRec);
  public
   constructor Create;
   destructor Destroy; override;

   procedure ReadTemplateFile(const FileName: string);
   procedure LoadFromStream(const ms: TStream);
   function AddUnit: integer;
   procedure RemoveUnit(const index: integer);
   function Save: boolean;

   property Catagory: string read fCatagory write fCatagory;
   property Description: string read fDesc write fDesc;
   property FileName: string read fFileName write fFileName;
   property Name: string read fName write fName;
   property OptionsRec: TProjOptions read fOptions write fOptions;
   property ProjectName: string read fPrjName write fPrjName;
   property ProjectIcon: string read fProjectIcon write fProjectIcon;
   property UnitCount: integer read GetUnitCount;
   property Units[index: integer]: TTemplateUnit read GetUnit write SetUnit;
   property OldData: TTemplateRec read GetOldData write SetOldData;
   property Version: integer read GetVersion write fVersion;
   property IconIndex: integer read fIconIndex write fIconIndex;
  end;

implementation

uses
{$IFDEF WIN32}
  Windows, Forms, SysUtils, version, utils, Dialogs, MultiLangSupport;
{$ENDIF}
{$IFDEF LINUX}
  QForms, SysUtils, version, utils, QDialogs, MultiLangSupport;
{$ENDIF}

resourcestring
 cTemplate = 'Template';
 cEditor = 'Editor';
 cProject = 'Project';

{ TTemplate }

constructor TTemplate.Create;
begin
  initOptionsRec(fOptions);
end;

destructor TTemplate.Destroy;
begin
  fTemplate.Free;
  inherited;
end;

procedure TTemplate.ReadTemplateFile(const FileName: string);
begin
  if assigned(fTemplate) then fTemplate.Free;
  if FileExists(FileName) then
   begin
     fFileName:= FileName;
     fTemplate:= TmemINIFile.Create(fFileName);
   end
  else
   begin
     MessageBox(Application.mainform.handle,
       PChar(Format(Lang[ID_ERR_TEMPFNF], [fFileName])),
       PChar(Lang[ID_INFO]), MB_OK or MB_ICONINFORMATION);
     exit;
   end;

  with fTemplate do
   begin
     fVersion:= ReadInteger(cTemplate, 'Ver', 0);
     // read entries for both old and new
      // template info
     fName:= ReadString(cTemplate, 'Name', 'NoName');
     fDesc:= ReadString(cTemplate, 'Description', 'NoDesc');
     fCatagory:= ReadString(cTemplate, 'Catagory', '');
     fIconIndex:= ReadInteger(cTemplate, 'IconIndex', 0);

      // project info
     fPrjName:= ReadString(cProject, 'Name', '');
     if fPrjName = '' then
      fPrjName:= fName;

     if fName = '' then
      fName:= fPrjName;

     // read old style
     if (fVersion <= 0) then
      begin
        fOptions.icon:= ReadString(cTemplate, 'Icon', '');
        if ReadBool(cProject, 'Console', FALSE) then
         fOptions.typ:= dptCon
        else
         if ReadBool(cProject, 'DLL', FALSE) then
          fOptions.typ:= dptDyn
         else
          fOptions.Typ:= dptGUI;

        fOptions.Libs.Append(ReadString(cProject, 'Libs', ''));
        fOptions.Includes.Append(ReadString(cProject, 'Includes', ''));

        fOptions.useGPP:= ReadBool(cProject, 'Cpp', TRUE);
        fOptions.cmdLines.Compiler:= ReadString(cProject, 'CompilerOptions', '');
      end
     else // read new style
      begin
        fOptions.icon:= ReadString(cTemplate, 'Icon', '');
        ProjectIcon:= ReadString(cProject, 'ProjectIcon', '');
        fOptions.typ:= ReadInteger(cProject, 'Type', 0); // default = gui
        fOptions.Objfiles.DelimitedText := ReadString(cProject, 'ObjFiles', '');
        fOptions.Includes.DelimitedText:= ReadString(cProject, 'Includes', '');
        fOptions.Libs.DelimitedText:= ReadString(cProject, 'Libs', '');
        fOptions.ResourceIncludes.DelimitedText := ReadString(cProject, 'ResourceIncludes', '');
        fOptions.cmdLines.Compiler:= ReadString(cProject, 'Compiler', '');
        fOptions.cmdLines.CppCompiler:= ReadString(cProject, 'CppCompiler', '');
        fOptions.cmdLines.Linker:= ReadString(cProject, 'Linker', '');
        fOptions.useGPP:= ReadBool(cProject, 'IsCpp', FALSE);
        fOptions.IncludeVersionInfo:= ReadBool(cProject, 'IncludeVersionInfo', FALSE);
        fOptions.SupportXPThemes:= ReadBool(cProject, 'SupportXPThemes', FALSE);

        // RNC -- 07-23-04 Added the ability to set an output dir in a template
        fOptions.ExeOutput:= ReadString(cProject, 'ExeOutput', '');
        fOptions.ObjectOutput:= ReadString(cProject, 'ObjectOutput', '');
        fOptions.CompilerOptions:= ReadString(cProject, 'CompilerSettings','');
        // units are read on demand
      end;
   end;
end;

// need to actually save values. (basiclly TTemplate is readonly right now)
function TTemplate.Save: boolean;
var
 fName: string;
begin
  result:= FALSE;
  try
   if assigned(fTemplate) then
    begin
      if (fTemplate.FileName = '') then
       begin
         // ** prompt for name place in fname
         fTemplate.Rename(fName, FALSE);
       end;
      fTemplate.UpdateFile;
      result:= TRUE;
    end;
  except
   // swallow
  end;
end;

function TTemplate.AddUnit: integer;
var
 section: string;
begin
  if (fVersion> 0) and (assigned(fTemplate)) then
   begin
     result:= GetUnitCount +1;
     section:= 'Unit' +inttostr(result);
     fTemplate.WriteString(section, 'C', '');
     fTemplate.WriteString(section, 'Cpp', '');
     fTemplate.WriteInteger(cProject, 'UnitCount', result);
   end
  else
   result:= -1;
end;

procedure TTemplate.RemoveUnit(const index: integer);
var
 section: string;
 count: integer;
begin
  section:= 'Unit' +inttostr(index);
  if fTemplate.SectionExists(section) then
   fTemplate.EraseSection(section);
  Count:= fTemplate.ReadInteger(cProject, 'UnitCount', 0);
  dec(count);
  fTemplate.WriteInteger(cProject, 'UnitCount', Count);
end;

function TTemplate.GetOldData: TTemplateRec;
begin
  if not assigned(fTemplate) then
   begin
     FillChar(result, Sizeof(TTemplateRec), #0);
     exit;
   end;

  if (fVersion <= 0) then // read old style
   begin
     result.CText:= fTemplate.ReadString(cEditor, 'Text', '');
     result.CppText:= fTemplate.ReadString(cEditor, 'Text_Cpp', '');
     result.CCaret:= point(fTemplate.ReadInteger(cEditor, 'CursorX', 1),
                           fTemplate.ReadInteger(cEditor, 'CursorY', 1));
     result.CppCaret:= point(fTemplate.ReadInteger(cEditor, 'CursorX_Cpp', 1),
                             fTemplate.ReadInteger(cEditor, 'CursorY_Cpp', 1));
     result.IsCPP:= fTemplate.ReadBool(cTemplate, 'EnableC', FALSE);
     if not result.IsCPP then
      result.IsCPP:= fTemplate.ReadBool(cTemplate, 'EnableCpp', FALSE);
   end
  else // read new style
   begin
     if fTemplate.ReadInteger(cTemplate, 'UnitCount', 0)> 0 then
      begin
        result.CText:= fTemplate.ReadString('Unit0', 'C', '');
        result.CppText:= fTemplate.ReadString('Unit0', 'Cpp', '');
      end;
     result.CCaret:= point(1,1);
     result.CppCaret:= point(1,1);
     result.IsCpp:= fTemplate.ReadBool(cProject, 'IsCpp', TRUE);
   end;
end;

procedure TTemplate.SetOldData(value: TTemplateRec);
begin
  if not assigned(fTemplate) then
   begin
     MessageBox(Application.MainForm.Handle,
      PChar(Lang[ID_ERR_NOTEMPLATE]), PChar(Lang[ID_INFO]), MB_OK or MB_ICONWARNING);
     exit;
   end;

  if (fVersion <= 0) then // set old style
   with fTemplate do
    begin
      WriteInteger(cEditor, 'CursorX', value.CCaret.X);
      WriteInteger(cEditor, 'CursorY', value.CCaret.Y);
      WriteInteger(cEditor, 'Cursor_CppX', value.CppCaret.X);
      WriteInteger(cEditor, 'Cursor_CppY', value.CppCaret.Y);
      WriteBool(cTemplate, 'EnableC', not value.IsCPP);
      WriteBool(cTemplate, 'EnableCpp', value.IsCpp);
    end
  else // seems dumb but might happen
   fTemplate.WriteBool(cProject, 'Cpp', value.IsCpp);
end;

function TTemplate.GetUnit(index: integer): TTemplateUnit;
var
 section: string;
begin
  if not assigned(fTemplate) then
   begin
     FillChar(result, Sizeof(TTemplateUnit), #0);
     exit;
   end;
  if fVersion <= 0 then // return nothing no units in old style
   begin
     result.CText:= '';
     result.CppText:= '';
   end
  else // return unit info
   begin
     section:= 'Unit' +inttostr(index);
     result.CText:= fTemplate.ReadString(section, 'C', '');
     result.CppText:= fTemplate.ReadString(section, 'Cpp', '');
     if Length(result.CppText) = 0 then
         result.CppText := result.CText;

     result.CName:= fTemplate.ReadString(section, 'CName', '');
     result.CppName:= fTemplate.ReadString(section, 'CppName', '');
     if Length(result.CppName) = 0 then
         result.CppName := result.CName;
   end;
end;

procedure TTemplate.SetUnit(index: integer; value: TTemplateUnit);
var
 section: string;
begin
  if not assigned(fTemplate) or (fVersion <= 0) then exit;
  section:= 'Unit' +inttostr(index);
  if fTemplate.SectionExists(section) then
   begin
     fTemplate.WriteString(section, 'C', value.CText);
     fTemplate.WriteString(section, 'Cpp', value.CppText);
     fTemplate.WriteString(section, 'CName', value.CName);
     fTemplate.WriteString(section, 'CppName', value.CppName);
   end
  else
   // debugging (we need to add debugging mode defines)
   showmessage('Section doesn''t exists '+inttostr(index));
end;

function TTemplate.GetUnitCount: integer;
begin
  if not assigned(fTemplate) then
   result:= -1
  else
   if (fVersion <= 0) then
    result:= 0
   else
    result:= fTemplate.ReadInteger(cProject, 'UnitCount', 0);
end;

function TTemplate.GetVersion: integer;
begin
  result:= fVersion;
end;

procedure TTemplate.LoadFromStream(const ms: TStream);
begin
//
end;

end.

