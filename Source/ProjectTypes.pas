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

unit ProjectTypes;

interface

uses
  Classes, ComCtrls, Windows;

const
  dptGUI = 0;
  dptCon = 1;
  dptStat = 2;
  dptDyn = 3;

type
  TProjType = byte;
  TProjTypeSet = set of TProjType;

  TProjVersionInfo = class(TPersistent) // allow deep copies
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  public
    Major: integer;
    Minor: integer;
    Release: integer;
    Build: integer;
    LanguageID: integer;
    CharsetID: integer;
    CompanyName: String;
    FileVersion: String;
    FileDescription: String;
    InternalName: String;
    LegalCopyright: String;
    LegalTrademarks: String;
    OriginalFilename: String;
    ProductName: String;
    ProductVersion: String;
    AutoIncBuildNr: boolean;
    SyncProduct: boolean;
  end;

  TCompilerOptionsValues = TArray<Integer>;

  TProjOptions = class(TPersistent) // allow deep copies
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  public
    typ: TProjType;
    Ver: integer;
    Objfiles: TStrings;
    CompilerCmd: String;
    CppCompilerCmd: String;
    LinkerCmd: String;
    Includes: TStrings;
    Libs: TStrings;
    PrivateResource: String; // Dev-C++ will overwrite this file
    ResourceIncludes: TStrings;
    MakeIncludes: TStrings;
    useGPP: boolean;
    Icon: String;
    ExeOutput: String;
    ObjectOutput: String;
    LogOutput: String;
    LogOutputEnabled: boolean;
    UseCustomMakefile: boolean;
    CustomMakefile: String;
    OverrideOutput: boolean;
    OverridenOutput: String;
    HostApplication: String;
    IncludeVersionInfo: boolean;
    SupportXPThemes: boolean;
    CompilerSet: integer;
    CompilerOptions: TCompilerOptionsValues;
    VersionInfo: TProjVersionInfo;
    CmdLineArgs: String;
  end;


  TCompilerOptionsValuesHelper = record helper for TCompilerOptionsValues
  private
    function GetOptionValue(const Index: Integer): Integer;
  public
    function ToString: string;
    property OptionValue[const Index: Integer]: Integer read GetOptionValue;
    class function FromString(const AStr: string): TArray<Integer>;
  end;


implementation

uses
  devcfg, SysUtils;

constructor TProjVersionInfo.Create;
begin
  inherited Create;

  Major := 1;
  Minor := 0;
  Release := 0;
  Build := 0;
  LanguageID := $0409; // US English
  CharsetID := $04E4; // Windows multilingual
  CompanyName := '';
  FileVersion := '';
  FileDescription := 'Developed using the Dev-C++ IDE';
  InternalName := '';
  LegalCopyright := '';
  LegalTrademarks := '';
  OriginalFilename := '';
  ProductName := '';
  ProductVersion := '';
  AutoIncBuildNr := False;
  SyncProduct := True;
end;

procedure TProjVersionInfo.Assign(Source: TPersistent);
var
  input: TProjVersionInfo;
begin
  input := TProjVersionInfo(Source);

  Major := input.Major;
  Minor := input.Minor;
  Release := input.Release;
  Build := input.Build;
  LanguageID := input.LanguageID;
  CharsetID := input.CharsetID;
  CompanyName := input.CompanyName;
  FileVersion := input.FileVersion;
  FileDescription := input.FileDescription;
  InternalName := input.InternalName;
  LegalCopyright := input.LegalCopyright;
  LegalTrademarks := input.LegalTrademarks;
  OriginalFilename := input.OriginalFilename;
  ProductName := input.ProductName;
  ProductVersion := input.ProductVersion;
  AutoIncBuildNr := input.AutoIncBuildNr;
  SyncProduct := input.SyncProduct;
end;

constructor TProjOptions.Create;
begin
  inherited Create;

  Ver := 2; // 2 since 5.2.0.3
  Includes := TStringList.Create;
  Libs := TStringList.Create;
  ResourceIncludes := TStringList.Create;
  MakeIncludes := TStringList.Create;
  ObjFiles := TStringList.Create;
  Includes.Delimiter := ';';
  Libs.Delimiter := ';';
  PrivateResource := '';
  ResourceIncludes.Delimiter := ';';
  MakeIncludes.Delimiter := ';';
  ObjFiles.Delimiter := ';';
  Icon := '';
  ExeOutput := '';
  ObjectOutput := '';
  HostApplication := '';
  SupportXPThemes := False;
  CompilerSet := devCompilerSets.DefaultSetIndex;
  if (CompilerSet < devCompilerSets.Count) and (CompilerSet >= 0) then
    CompilerOptions := devCompilerSets[CompilerSet].INIOptions
  else
    CompilerOptions := [];
  VersionInfo := TProjVersionInfo.Create;
  IncludeVersionInfo := False;
end;

destructor TProjOptions.Destroy;
begin
  Includes.Free;
  Libs.Free;
  ResourceIncludes.Free;
  MakeIncludes.Free;
  ObjFiles.Free;
  VersionInfo.Free;

  inherited;
end;

procedure TProjOptions.Assign(Source: TPersistent);
var
  input: TProjOptions;
begin
  input := TProjOptions(Source);

  typ := input.typ;
  Ver := input.Ver;
  Objfiles.Assign(input.Objfiles);
  CompilerCmd := input.CompilerCmd;
  CppCompilerCmd := input.CppCompilerCmd;
  LinkerCmd := input.LinkerCmd;
  Includes.Assign(input.Includes);
  Libs.Assign(input.Libs);
  PrivateResource := input.PrivateResource;
  ResourceIncludes.Assign(input.ResourceIncludes);
  MakeIncludes.Assign(input.MakeIncludes);
  useGPP := input.useGPP;
  Icon := input.Icon;
  ExeOutput := input.ExeOutput;
  ObjectOutput := input.ObjectOutput;
  LogOutput := input.LogOutput;
  LogOutputEnabled := input.LogOutputEnabled;
  UseCustomMakefile := input.UseCustomMakefile;
  CustomMakefile := input.CustomMakefile;
  OverrideOutput := input.OverrideOutput;
  OverridenOutput := input.OverridenOutput;
  HostApplication := input.HostApplication;
  IncludeVersionInfo := input.IncludeVersionInfo;
  SupportXPThemes := input.SupportXPThemes;
  CompilerSet := input.CompilerSet;
  CompilerOptions := input.CompilerOptions;
  VersionInfo.Assign(input.VersionInfo);
  CmdLineArgs := input.CmdLineArgs;
end;

{ TCompilerOptionsHelper }

class function TCompilerOptionsValuesHelper.FromString(const AStr: string): TArray<Integer>;
begin
  Result := [];
  if AStr.IndexOf(';')>-1 then
  begin
    // read compiler options with a ; delimiter
    for var OV in AStr.Split([';']) do
      Result := Result + [StrToIntDef(OV.Trim, 0)];
  end
  else
  begin
    // read compiler options that have no delimiter
    for var OV in AStr do
      Result := Result + [StrToIntDef(OV, 0)];
  end;
end;

function TCompilerOptionsValuesHelper.GetOptionValue(const Index: Integer): Integer;
begin
  if (Index > High(Self)) or (Index < Low(Self)) then
    Result := 0
  else
    Result := Self[Index];
end;

function TCompilerOptionsValuesHelper.ToString: string;
begin
  Result := '';
  for var OV in Self do
    if Result = '' then
      Result := OV.ToString
    else
      Result := Result + ';' + OV.ToString;
end;

end.

