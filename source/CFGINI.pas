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

unit CFGINI;

interface

uses
  Classes, IniFiles, cfgTypes, TypInfo;

type
 TCFGINI = class(TObject)
  private
   fOwner: TComponent;                 // assumes a TConfigData
   fini: Tinifile;
    procedure ReadFrominifile(Obj: TPersistent; const Section: string);
    procedure ReadObject(const Name: string; Obj: TPersistent);
    function ReadSet(const Name: string; TypeInfo: PTypeInfo): integer;
    procedure ReadStrings(const Name: string; value: TStrings);
    procedure WriteObject(const Name: string; Obj: TPersistent);
    procedure WriteSet(const Name: string; value: integer;
      TypeInfo: PTypeInfo);
    procedure WriteStrings(const Name: string; value: TStrings);
    procedure Writetoinifile(const Section: string; Obj: TPersistent);
    procedure ClearSection(const Name: string);
  public
   constructor Create(aOwner: TComponent);
   destructor Destroy; override;

   procedure SetIniFile(s : string);
   procedure ReadConfig;
   procedure SaveConfig;

   procedure LoadObject(var Obj: TCFGOptions);
   procedure SaveObject(var  Obj: TCFGOptions);

   function LoadSetting(const key: string; const Entry: string): string; overload;
   function LoadSetting(val : boolean; const key, Entry: string): string; overload;
   procedure SaveSetting(const key: string; const entry: string; const value: string);
 end;


implementation

uses
{$IFDEF WIN32}
  CFGData, SysUtils, Graphics;
{$ENDIF}
{$IFDEF LINUX}
  CFGData, SysUtils, QGraphics;
{$ENDIF}

{ TCFGINI }

constructor TCFGINI.Create(aOwner: TComponent);
begin
  fOwner:= aOwner;
  fIni := nil;
end;

destructor TCFGINI.Destroy;
begin
  if (assigned(fIni)) then
    fini.free;
  inherited;
end;

procedure TCFGIni.SetIniFile(s : string);
begin
  if (assigned(fIni)) then
    fini.free;
  fini:= TIniFile.Create(s);
end;

procedure TCFGINI.ReadConfig;
var
 section: string;
begin
  if not assigned(fIni) then exit;
  section:= TConfigData(fOwner).INISection;
  if section = '' then
   raise EConfigDataError.Create('(ConfigData(INIReadCFG): Section not set');
  try
   ReadFromINIFile(fOwner, Section);
  except end;
end;

procedure TCFGINI.SaveConfig;
var
 section: string;
begin
  if not assigned(fIni) then exit;
  section:= TConfigData(fOwner).INISection;
  if section = '' then
   raise EConfigDataError.Create('(ConfigData(INISaveCFG): Section not set');
  with fINI do
   try
    WritetoINIfile(Section, fOwner);
   except end;
end;


// Reading methods

function TCFGINI.ReadSet(const Name: string; TypeInfo: PTypeInfo): integer;
var
 idx: integer;
 value: integer;
begin
  result := 0;
  if not assigned(fIni) then exit;
  TypeInfo:= GetTypeData(TypeInfo).CompType^;
  value:= 0;
  if fINI.SectionExists(Name) then
   with GetTypeData(TypeInfo)^ do
    for idx:= MinValue to MaxValue do
     if ReadBoolString(fini.ReadString(Name, GetENumName(TypeInfo, idx), 'FALSE')) then
      include(TIntegerSet(value), idx);
  result:= value;
end;

procedure TCFGINI.ReadStrings(const Name: string; value: TStrings);
begin
  Value.BeginUpdate;
  try
   Value.Clear;
   if fini.SectionExists(Name) then
    fini.ReadSectionValues(Name, value);
  finally
   Value.EndUpdate;
  end;
end;

procedure TCFGINI.ReadObject(const Name: string; Obj: TPersistent);
begin
  if fini.SectionExists(Name) then
   ReadFromINIFile(Obj, Name);
end;

procedure TCFGINI.ReadFrominifile(Obj: TPersistent; const Section: string);
var
 idx, idx2: integer;
 PropName: string;
 Cd: TConfigData;
begin
  CD:= TConfigData(fOwner);
  for idx:= 0 to pred(GetPropCount(Obj)) do
   begin
     PropName:= GetPropName(Obj, idx);
     if Obj is TFont then
      begin
        idx2:= CD.IgnoreProperties.Indexof('Name');
        if idx2 <> -1 then
         CD.IgnoreProperties[idx2]:= 'Height';
      end
     else
      begin
        idx2:= CD.IgnoreProperties.Indexof('Height');
        if idx2 <> -1 then
         CD.IgnoreProperties[idx2]:= 'Name';
      end;
     if (CD.IgnoreProperties.Indexof(PropName)> -1) or
        ((not fINI.ValueExists(Section, PropName)) and
         (not fINI.SectionExists(Section +'.'+PropName))) then
      continue;

     case PropType(Obj, PropName) of
      tkString,
      tkLString,
      tkWString: SetStrProp(Obj, PropName, fINI.ReadString(Section, PropName, ''));

      tkChar,
      tkEnumeration,
      tkInteger: SetOrdProp(Obj, PropName, fINI.ReadInteger(Section, PropName, 1));

      tkInt64: SetInt64Prop(Obj, PropName, StrtoInt(fINI.ReadString(Section, PropName, '0')));

      tkFloat: SetFloatProp(Obj, PropName, StrtoFloat(fINI.ReadString(Section, PropName, '0.0')));

      tkSet: SetOrdProp(Obj, PropName, ReadSet(Section +'.' +PropName,
          GetPropInfo(Obj, PropName, [tkSet])^.PropType^));

      tkClass:
       begin
         if TPersistent(GetOrdProp(Obj, PropName)) is TStrings then
          ReadStrings(Section +'.' +PropName, TStrings(GetOrdProp(Obj, PropName)))
         else
          ReadObject(Section +'.' +PropName, TPersistent(GetOrdProp(Obj, PropName)));
       end;
     end;
   end;
end;

// Writing Methods
procedure TCFGINI.WriteSet(const Name: string; value: integer;
  TypeInfo: PTypeInfo);
var
 idx: integer;
 s: string;
begin
  TypeInfo:= GetTypeData(TypeInfo).CompType^;
  ClearSection(Name);
  with GetTypeData(TypeInfo)^ do
   for idx:= MinValue to MaxValue do
    begin
      s:= GetENumName(TypeInfo, idx);
      if fINI.ValueExists(Name, s) then
       fini.DeleteKey(Name, s);
      fINI.WriteString(Name, s, boolStr[idx in TIntegerSet(value)]);
    end;
end;

procedure TCFGINI.WriteStrings(const Name: string; value: TStrings);
var
 idx: integer;
begin
  ClearSection(Name);
  if value.count <= 0 then exit;
  for idx:= 0 to Pred(value.Count) do
   if AnsiPos('=', value[idx]) <> 0 then
    fini.WriteString(Name, Value.Names[idx], value.Values[Value.Names[idx]])
   else
    fini.WriteString(Name, Value[idx], '');
end;

procedure TCFGINI.WriteObject(const Name: string; Obj: TPersistent);

  function WritetheObject(Obj: TPersistent): boolean;
   var
    idx, c, Count: integer;
   begin
     result:= FALSE;
     Count:= GetPropCount(Obj);
     if Count <= 0 then exit;
     c:= Count;
     for idx:= 0 to pred(Count) do
      begin
        if TConfigData(fOwner).IgnoreProperties.Indexof(getPropName(Obj, idx)) <> -1 then
         dec(c);
      end;
     result:= c> 0;
   end;
begin
  if not WritetheObject(Obj) then exit;
  ClearSection(Name);
  WritetoINIFile(Name, Obj);
end;

procedure TCFGINI.Writetoinifile(const Section: string; Obj: TPersistent);
var
 idx,
 idx2: integer;
 PropName: string;
 CD: TConfigData;
begin
  CD:= TConfigData(fOwner);
  for idx:= 0 to pred(GetPropCount(Obj)) do
   begin
     PropName:= GetPropName(Obj, idx);
     if Obj is TFont then
      begin
        idx2:= CD.IgnoreProperties.Indexof('Name');
        if idx2> -1 then
         CD.IgnoreProperties[idx2]:= 'Height';
      end
     else
      begin
        idx2:= CD.IgnoreProperties.Indexof('Height');
        if idx2> -1 then
         CD.IgnoreProperties[idx2]:= 'Name';
      end;
     if CD.IgnoreProperties.Indexof(PropName)>= 0 then continue;
     case PropType(Obj, PropName) of
      tkString,
      tkLString,
      tkWString: fINI.WriteString(Section, PropName, '"'+GetStrProp(Obj, PropName)+'"');
      // 11 Jul 2002: mandrav: added double quotes around strings.
      // fixes a bug with stringlists comma-text saved as string...

      tkChar,
      tkEnumeration,
      tkInteger: fINI.WriteInteger(Section, PropName, GetOrdProp(Obj, PropName));

      tkInt64: fINI.WriteString(Section, PropName, InttoStr(GetInt64Prop(Obj, PropName)));

      tkFloat: fINI.WriteString(Section, PropName, FloattoStr(GetFloatProp(Obj, PropName)));

      tkSet: WriteSet(Section +'.' +PropName, GetOrdProp(Obj, PropName),
        GetPropInfo(Obj, PropName, [tkSet])^.PropType^);
      tkClass:
       begin
         if TPersistent(GetOrdProp(Obj, PropName)) is TStrings then
          WriteStrings(Section +'.'+PropName, TStrings(GetOrdProp(Obj, PropName)))
         else
          WritetoINIFile(Section +'.' +PropName, TPersistent(GetOrdProp(Obj, PropName)));
       end;
     end;
   end;
end;

procedure TCFGINI.LoadObject(var Obj: TCFGOptions);
begin
  if not assigned(Obj) then exit;
  try
   ReadObject(Obj.Name, Obj);
  except end;
end;

function TCFGINI.LoadSetting(const key, Entry: string): string;
begin
  result:= fini.ReadString(Key, Entry, '');
end;

function TCFGINI.LoadSetting(val : boolean; const key, Entry: string): string;
begin
  result := fini.ReadString(Key, Entry, '');
  if result = '' then begin
    if val then
      result := '1'
    else
      result := '0';
  end;
end;

procedure TCFGINI.SaveObject(var Obj: TCFGOptions);
begin
  if not assigned(Obj) then exit;
  WriteObject(Obj.Name, Obj);
end;

procedure TCFGINI.SaveSetting(const key, entry, value: string);
begin
  fini.WriteString(Key, Entry, Value);
end;

procedure TCFGINI.ClearSection(const Name: string);
//var
// tmp: TStrings;
// idx: integer;
begin
  // This way it's much easier...
  if fini.SectionExists(Name) then
    fini.EraseSection(Name);
//  tmp:= TStringList.Create;
//  try
//   fini.ReadSectionValues(Name, tmp);
//   if tmp.Count> 0 then
//    for idx:= 0 to pred(tmp.Count) do
//     fini.DeleteKey(Name, tmp[idx]);
//  finally
//   tmp.Free;
//  end;
end;

end.
