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

unit CFGData;

interface

uses
  Windows, Classes, sysUtils, TypInfo, IniFiles, Graphics;

type
  TConfigData = class(TPersistent)
  private
    fIni: TIniFile;
    function GetIniFileName: String;
    procedure SetIniFileName(const s: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SettoDefaults; virtual; abstract;

    procedure ReadSelf;
    procedure SaveSelf;

    procedure EraseSection(const name: String);

    function ReadI(const key: String; const entry: String): integer;
    function ReadB(const key: String; const entry: String): boolean;
    function ReadS(const key: String; const entry: String): String;

    function ReadDefaultI(const key: String; const entry: String; default: integer): integer;
    function ReadDefaultB(const key: String; const entry: String; default: boolean): boolean;
    function ReadDefaultS(const key: String; const entry: String; const default: String): String;

    procedure Write(const key: String; const entry: String; value: integer); overload;
    procedure Write(const key: String; const entry: String; value: boolean); overload;
    procedure Write(const key: String; const entry: String; const value: String); overload;

    procedure ReadObject(const Section: String; Obj: TPersistent);
    procedure WriteObject(const Section: String; Obj: TPersistent);

    // Used for reading/writing each item as a separate entry
    procedure ReadStrings(const key: String; value: TStrings);
    procedure WriteStrings(const key: String; value: TStrings);

    // Used for reading/writing string lists as a single string entry
    procedure ReadDelimitedString(const key: String; const entry: String; value: TStringList);
    procedure WriteDelimitedString(const key: String; const entry: String; value: TStringList);

    property INIFileName: String read GetIniFileName write SetIniFileName;
  end;

function GetPropName(Instance: TPersistent; Index: Integer): TSymbolName;
function GetPropCount(Instance: TPersistent): Integer;

implementation

//Returns the number of properties of a given object

function GetPropCount(Instance: TPersistent): Integer;
var
  Data: PTypeData;
begin
  Data := GetTypeData(Instance.Classinfo);
  Result := Data^.PropCount;
end;

//Returns the property name of an instance at a certain index

function GetPropName(Instance: TPersistent; Index: Integer): TSymbolName;
var
  PropList: PPropList;
  PropInfo: PPropInfo;
  Data: PTypeData;
begin
  Result := '';
  Data := GetTypeData(Instance.Classinfo);
  GetMem(PropList, Data^.PropCount * Sizeof(PPropInfo));
  try
    GetPropInfos(Instance.ClassInfo, PropList);
    PropInfo := PropList^[Index];
    Result := PropInfo^.Name;
  finally
    FreeMem(PropList, Data^.PropCount * Sizeof(PPropInfo));
  end;
end;

{ TConfigData }

constructor TConfigData.Create;
begin
  inherited Create;
end;

destructor TConfigData.Destroy;
begin
  if Assigned(fIni) then
    fIni.free;
  inherited Destroy;
end;

function TConfigData.GetIniFileName: String;
begin
  if Assigned(fIni) then
    result := fIni.FileName
  else
    result := '';
end;

procedure TConfigData.SetIniFileName(const s: String);
begin
  if Assigned(fIni) then
    fini.free;
  fIni := TIniFile.Create(s);
end;

procedure TConfigData.ReadObject(const Section: String; Obj: TPersistent);
var
  I: integer;
  PropName: string;
begin
  if not fini.SectionExists(Section) then
    Exit;

  for I := 0 to GetPropCount(Obj) - 1 do begin
    PropName := string(GetPropName(Obj, I));

    // Ignore properties which aren't listed in the INI (leave defaults)
    if (not fINI.ValueExists(Section, PropName)) and (not fINI.SectionExists(Section + '.' + PropName)) then
      Continue;

    case PropType(Obj, PropName) of
        tkString,
        tkLString,
        tkUString,
        tkWString: SetStrProp(Obj, PropName, fINI.ReadString(Section, PropName, ''));

      tkChar,
        tkEnumeration,
        tkInteger: SetOrdProp(Obj, PropName, fINI.ReadInteger(Section, PropName, 1));

      tkInt64: SetInt64Prop(Obj, PropName, StrtoInt(fINI.ReadString(Section, PropName, '0')));

      tkFloat: SetFloatProp(Obj, PropName, StrtoFloat(fINI.ReadString(Section, PropName, '0.0')));

      tkClass: begin
          if TPersistent(GetOrdProp(Obj, PropName)) is TStrings then
            ReadStrings(Section + '.' + PropName, TStrings(GetOrdProp(Obj, PropName)))
          else
            ReadObject(Section + '.' + PropName, TPersistent(GetOrdProp(Obj, PropName)));
        end;
    end;
  end;
end;

procedure TConfigData.WriteObject(const Section: String; Obj: TPersistent);
var
  I: integer;
  PropName: String;
begin
  EraseSection(Section);

  for I := 0 to GetPropCount(Obj) - 1 do begin
    PropName := string(GetPropName(Obj, I));
    case PropType(Obj, PropName) of

      // 11 Jul 2002: mandrav: added double quotes around strings.
      // fixes a bug with stringlists comma-text saved as String...
        tkUString,
        tkString,
        tkLString,
        tkWString: fINI.WriteString(Section, PropName, '"' + GetStrProp(Obj, PropName) + '"');

      tkChar,
        tkEnumeration,
        tkInteger: fINI.WriteInteger(Section, PropName, GetOrdProp(Obj, PropName));

      tkInt64: fINI.WriteString(Section, PropName, InttoStr(GetInt64Prop(Obj, PropName)));

      tkFloat: fINI.WriteString(Section, PropName, FloattoStr(GetFloatProp(Obj, PropName)));

      tkClass: begin
          if TPersistent(GetOrdProp(Obj, PropName)) is TStrings then
            WriteStrings(Section + '.' + PropName, TStrings(GetOrdProp(Obj, PropName)))
          else
            WriteObject(Section + '.' + PropName, TPersistent(GetOrdProp(Obj, PropName)));
        end;
    end;
  end;
end;

procedure TConfigData.ReadSelf;
begin
  if not Assigned(fIni) then
    Exit;

  try
    ReadObject('Options', self);
  except
  end;
end;

procedure TConfigData.SaveSelf;
begin
  if not Assigned(fIni) then
    Exit;

  try
    WriteObject('Options', self);
  except
  end;
end;

procedure TConfigData.ReadStrings(const key: String; value: TStrings);
begin
  Value.BeginUpdate;
  try
    Value.Clear;
    if fini.SectionExists(key) then
      fini.ReadSectionValues(key, value);
  finally
    Value.EndUpdate;
  end;
end;

procedure TConfigData.WriteStrings(const key: String; value: TStrings);
var
  I: integer;
begin
  EraseSection(key);
  for I := 0 to Pred(value.Count) do
    if Value.Names[I] = '' then // name/value stuff not found...
      fIni.WriteString(key, IntToStr(I), value[I])
    else
      fIni.WriteString(key, Value.Names[I], value.Values[Value.Names[I]]);
end;

// fix without using StringList.DelimitedText:
// http://stackoverflow.com/questions/1335027/delphi-stringlist-delimiter-is-always-a-space-character-even-if-delimiter-is-se

procedure TConfigData.ReadDelimitedString(const key: String; const entry: String; value: TStringList);
var
  S: String;
begin
  S := ReadS(key, entry);

  // Convert string to string list
  value.Clear;
  ExtractStrings([';'], [], PChar(S), value);
end;

procedure TConfigData.WriteDelimitedString(const key: String; const entry: String; value: TStringList);
var
  S: String;
begin
  // Convert stringlist to string
  value.Delimiter := ';';
  S := value.DelimitedText;
  Write(key, entry, S);
end;

function TConfigData.ReadI(const key, entry: String): integer;
begin
  result := ReadDefaultI(key, entry, 0);
end;

function TConfigData.ReadB(const key, entry: String): boolean;
begin
  result := ReadDefaultB(key, entry, false);
end;

function TConfigData.ReadS(const key: String; const entry: String): String;
begin
  result := ReadDefaultS(key, entry, '');
end;

function TConfigData.ReadDefaultI(const key: String; const entry: String; default: integer): integer;
begin
  result := fIni.ReadInteger(Key, Entry, default);
end;

function TConfigData.ReadDefaultB(const key: String; const entry: String; default: boolean): boolean;
begin
  result := fIni.ReadBool(Key, Entry, default);
end;

function TConfigData.ReadDefaultS(const key: String; const entry: String; const default: String):
  String;
begin
  result := fIni.ReadString(Key, Entry, default);
end;

procedure TConfigData.Write(const key: String; const entry: String; value: integer);
begin
  fIni.WriteInteger(Key, Entry, Value);
end;

procedure TConfigData.Write(const key: String; const entry: String; value: boolean);
begin
  fIni.WriteBool(Key, Entry, value);
end;

procedure TConfigData.Write(const key: String; const entry: String; const value: String);
begin
  fIni.WriteString(Key, Entry, Value);
end;

procedure TConfigData.EraseSection(const name: String);
begin
  if fIni.SectionExists(Name) then
    fIni.EraseSection(Name);
end;

end.

