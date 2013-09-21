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
  Classes, IniFiles, TypInfo;

type
 TCFGINI = class(TObject)
  private
   fOwner: TComponent; // assumes a TConfigData
   fini: Tinifile;
    procedure ReadFrominifile(Obj: TPersistent; const Section: AnsiString);
    procedure ReadObject(const Name: AnsiString; Obj: TPersistent);
    procedure ReadStrings(const Name: AnsiString; value: TStrings);
    procedure WriteObject(const Name: AnsiString; Obj: TPersistent);
    procedure WriteStrings(const Name: AnsiString; value: TStrings);
    procedure Writetoinifile(const Section: AnsiString; Obj: TPersistent);
    procedure ClearSection(const Name: AnsiString);
  public
   constructor Create(aOwner: TComponent);
   destructor Destroy; override;

   procedure SetIniFile(s : AnsiString);
   procedure ReadConfig;
   procedure SaveConfig;

   procedure LoadObject(var Obj: TPersistent;const CustomName : AnsiString);
   procedure SaveObject(var  Obj: TPersistent;const CustomName : AnsiString);

   function LoadSetting(const key: AnsiString; const Entry: AnsiString): AnsiString;
   procedure SaveSettingS(const key: AnsiString; const entry: AnsiString; const value: AnsiString);
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

procedure TCFGIni.SetIniFile(s : AnsiString);
begin
  if (assigned(fIni)) then
    fini.free;
  fini:= TIniFile.Create(s);
end;

procedure TCFGINI.ReadConfig;
var
 section: AnsiString;
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
 section: AnsiString;
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

procedure TCFGINI.ReadStrings(const Name: AnsiString; value: TStrings);
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

procedure TCFGINI.ReadObject(const Name: AnsiString; Obj: TPersistent);
begin
  if fini.SectionExists(Name) then
   ReadFromINIFile(Obj, Name);
end;

procedure TCFGINI.ReadFrominifile(Obj: TPersistent; const Section: AnsiString);
var
 idx, idx2: integer;
 PropName: AnsiString;
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

procedure TCFGINI.WriteStrings(const Name: AnsiString; value: TStrings);
var
 idx: integer;
begin
  ClearSection(Name);
  if value.count <= 0 then exit;
  for idx:= 0 to Pred(value.Count) do
   if Pos('=', value[idx]) <> 0 then
    fini.WriteString(Name, Value.Names[idx], value.Values[Value.Names[idx]])
   else
    fini.WriteString(Name, Value[idx], '');
end;

procedure TCFGINI.WriteObject(const Name: AnsiString; Obj: TPersistent);

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

procedure TCFGINI.Writetoinifile(const Section: AnsiString; Obj: TPersistent);
var
 idx,
 idx2: integer;
 PropName: AnsiString;
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
      // fixes a bug with stringlists comma-text saved as AnsiString...

      tkChar,
      tkEnumeration,
      tkInteger: fINI.WriteInteger(Section, PropName, GetOrdProp(Obj, PropName));

      tkInt64: fINI.WriteString(Section, PropName, InttoStr(GetInt64Prop(Obj, PropName)));

      tkFloat: fINI.WriteString(Section, PropName, FloattoStr(GetFloatProp(Obj, PropName)));

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

procedure TCFGINI.LoadObject(var Obj: TPersistent;const CustomName : AnsiString);
begin
	if not assigned(Obj) then exit;
	if CustomName <> '' then
		ReadObject(CustomName, Obj)
	else
		ReadObject(Obj.ClassName, Obj);
end;

function TCFGINI.LoadSetting(const key, Entry: AnsiString): AnsiString;
begin
	result:= fini.ReadString(Key, Entry, '');
end;

procedure TCFGINI.SaveObject(var Obj: TPersistent;const CustomName : AnsiString);
begin
	if not assigned(Obj) then exit;
	if CustomName <> '' then
		WriteObject(CustomName, Obj)
	else
		WriteObject(Obj.ClassName,Obj);
end;

procedure TCFGINI.SaveSettingS(const key, entry, value: AnsiString);
begin
	fini.WriteString(Key, Entry, Value);
end;

procedure TCFGINI.ClearSection(const Name: AnsiString);
begin
	if fini.SectionExists(Name) then
		fini.EraseSection(Name);
end;

end.
