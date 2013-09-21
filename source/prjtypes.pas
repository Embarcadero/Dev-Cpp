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

unit prjtypes;

interface

uses
{$IFDEF WIN32}
  Classes, editor, ComCtrls, datamod;
{$ENDIF}
{$IFDEF LINUX}
  Classes, editor, QComCtrls, datamod;
{$ENDIF}

const
 dptGUI  = 0;
 dptCon  = 1;
 dptStat = 2;
 dptDyn  = 3;

type
 TProjType = byte;
 TProjTypeSet = set of TProjType;

 TProjVersionInfo = record
  Major: integer;
  Minor: integer;
  Release: integer;
  Build: integer;
  LanguageID: integer;
  CharsetID: integer;
  CompanyName: string;
  FileVersion: string;
  FileDescription: string;
  InternalName: string;
  LegalCopyright: string;
  LegalTrademarks: string;
  OriginalFilename: string;
  ProductName: string;
  ProductVersion: string;
  AutoIncBuildNr: boolean;
 end;

 TProjOptions = record
   typ: TProjType;
   Ver: integer;
   Objfiles: TStrings;

   cmdLines: record
     Compiler: string;
     CppCompiler: string;
     Linker: string;
   end;

   Includes: TStrings;
   Libs: TStrings;
   PrivateResource: String; // Dev-C++ will overwrite this file
   ResourceIncludes: TStringList;
   MakeIncludes: TStringList;
   useGPP: boolean;
   Icon: string;

   ExeOutput: String;
   ObjectOutput: String;

   OverrideOutput: boolean;
   OverridenOutput: string;

   HostApplication : string;

   IncludeVersionInfo: boolean;
   SupportXPThemes: boolean;
   CompilerSet: integer;
   CompilerOptions: string;
   VersionInfo: TProjVersionInfo;
 end;

procedure InitOptionsRec(var Rec: TProjOptions);
procedure AssignOptionsRec(var R1, R2: TProjOptions);

implementation

uses 
  devcfg;

procedure InitOptionsRec(var Rec: TProjOptions);
begin
  with Rec do
   begin
     Includes:= TStringList.Create;
     Libs:= TStringList.Create;
     ResourceIncludes := TStringList.Create;
     MakeIncludes := TStringList.Create;
     ObjFiles:= TStringList.Create;
     Includes.Delimiter:= ';';
     Libs.Delimiter:= ';';
     PrivateResource := '';
     ResourceIncludes.Delimiter:= ';';
     MakeIncludes.Delimiter := ';';
     ObjFiles.Delimiter:= ';';
     Icon := '';
     ExeOutput := '';
     ObjectOutput := '';
     HostApplication := '';
     SupportXPThemes:=False;
     CompilerSet:=0;
     CompilerOptions:=  devCompiler.OptionStr;

     IncludeVersionInfo:=False;
     VersionInfo.Major:=0;
     VersionInfo.Minor:=1;
     VersionInfo.Release:=1;
     VersionInfo.Build:=1;
     VersionInfo.LanguageID:=$0409; // US English
     VersionInfo.CharsetID:=$04E4; // Windows multilingual
     VersionInfo.CompanyName:='';
     VersionInfo.FileVersion:='';
     VersionInfo.FileDescription:='Developed using the Dev-C++ IDE';
     VersionInfo.InternalName:='';
     VersionInfo.LegalCopyright:='';
     VersionInfo.LegalTrademarks:='';
     VersionInfo.OriginalFilename:='';
     VersionInfo.ProductName:='';
     VersionInfo.ProductVersion:='';
     VersionInfo.AutoIncBuildNr:=False;
   end;
end;

procedure AssignOptionsRec(var R1, R2: TProjOptions);
begin
  with R2 do
   begin
     typ:= R1.typ;
     Ver:= r1.ver;
     Objfiles.Text:= r1.ObjFiles.Text;
     Includes.Text:= R1.Includes.Text;
     Libs.Text:= R1.Libs.Text;
     PrivateResource := R1.PrivateResource;
     ResourceIncludes.Text := R1.ResourceIncludes.Text;
     MakeIncludes.Text := R1.MakeIncludes.Text;
     cmdLines.Compiler:= R1.cmdLines.Compiler;
     cmdLines.CppCompiler:= R1.cmdLines.CppCompiler;
     cmdLines.Linker:= R1.cmdLines.Linker;
     useGPP:= R1.useGPP;
     icon:= R1.icon;
     ExeOutput := R1.ExeOutput;
     ObjectOutput := R1.ObjectOutput;
     HostApplication := R1.HostApplication;
     IncludeVersionInfo:=R1.IncludeVersionInfo;
     SupportXPThemes:=R1.SupportXPThemes;
     CompilerSet:=R1.CompilerSet;
     CompilerOptions:=R1.CompilerOptions;
     VersionInfo:=R1.VersionInfo;
   end;
end;

end.
