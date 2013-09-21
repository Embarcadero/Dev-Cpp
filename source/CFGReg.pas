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

unit CFGReg;

interface

uses
{$IFDEF WIN32}
  Classes, Registry, Types, TypInfo, cfgTypes;
{$ENDIF}
{$IFDEF LINUX}
  Classes, Types, TypInfo, cfgTypes;
{$ENDIF}

type
 TCFGReg = class(TObject)
  private
   fOwner: TComponent;                 // assumes a TConfigData
   fReg: TRegistry;
    procedure ReadFromRegistry(Obj: TPersistent);
    procedure ReadObject(const Name: string; Obj: TPersistent);
    function ReadSet(const Name: string; TypeInfo: PTypeInfo): integer;
    procedure ReadStrings(const Name: string; value: TStrings);
    procedure WriteObject(const Name: string; Obj: TPersistent);
    procedure WriteSet(const Name: string; value: integer;
      TypeInfo: PTypeInfo);
    procedure WriteStrings(const Name: string; value: TStrings);
    procedure WritetoRegistry(Obj: TPersistent);                    // Registry access object;
  public
   constructor Create(aOwner: TComponent);
   destructor Destroy; override;
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
  CFGData, Graphics, SysUtils;
{$ENDIF}
{$IFDEF LINUX}
  CFGData, QGraphics, SysUtils;
{$ENDIF}

{ TCFGReg }

constructor TCFGReg.Create(aOwner: TComponent);
begin
  fOwner:= aOwner;
  fReg:= TRegistry.Create;
end;

destructor TCFGReg.Destroy;
begin
  fReg.Free;
  inherited;
end;

procedure TCFGReg.ReadConfig;
var
 RegKey: string;
begin
  RegKey:= TConfigData(fOwner).BaseRegKey;
  if RegKey = '' then
   raise EConfigDataError.Create('ConfigData: Registry Key not set.');
  with fReg do
   try
    RootKey:= TConfigData(fOwner).Root;
    if OpenKey(RegKey, FALSE) then
     ReadFromRegistry(fOwner);
   finally
    CloseKey;
   end;
end;

procedure TCFGReg.SaveConfig;
var
 RegKey: string;
begin
  RegKey:= TConfigData(fOwner).BaseRegKey;
  if RegKey = '' then
   raise EConfigDataError.Create('ConfigData: Registry Key not set.');
  with fReg do
   try
    RootKey:= TConfigData(fOwner).Root;
    if OpenKey(RegKey, FALSE) then
     WritetoRegistry(fOwner);
   finally
    CloseKey;
   end;
end;

// Reading methods

procedure TCFGReg.ReadFromRegistry(Obj: TPersistent);
var
 idx,
 idx2,
 Count: integer;
 PropName: string;
 CD: TConfigData;
begin
  Count:= GetPropCount(Obj);
  CD:= TConfigData(fOwner);
  for idx:= 0 to pred(Count) do
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

     if (CD.IgnoreProperties.Indexof(PropName)> -1)
	 or (not fReg.ValueExists(PropName)) then
       continue;

     case PropType(Obj, PropName) of
      tkString,
      tkLString,
      tkWString: SetStrProp(Obj, PropName, fReg.ReadString(PropName));

      tkChar,
      tkEnumeration,
      tkInteger: SetOrdProp(Obj, PropName, fReg.ReadInteger(PropName));

      tkInt64: SetInt64Prop(Obj, PropName, StrtoInt(fReg.ReadString(PropName)));

      tkFloat: SetFloatProp(Obj, PropName, StrtoFloat(fReg.ReadString(PropName)));

      tkSet: SetOrdProp(Obj, PropName, ReadSet(PropName,
        GetPropInfo(Obj, PropName, [tkSet])^.PropType^));

      tkClass:
       begin
         if TPersistent(GetOrdProp(Obj, PropName)) is TStrings then
          ReadStrings(PropName, TStrings(GetOrdProp(Obj, PropName)))
         else
          ReadObject(PropName, TPersistent(GetOrdProp(Obj, PropName)));
       end;
     end;
   end;
end;

function TCFGReg.ReadSet(const Name: string; TypeInfo: PTypeInfo): integer;
var
 OldKey: string;
 idx: integer;
 value: integer;
begin
  TypeInfo:= GetTypeData(TypeInfo).CompType^;
  OldKey:= '\' +fReg.CurrentPath;
  value:= 0;
  try
   if not freg.OpenKey(Name, FALSE) then
    raise ERegistryException.Createfmt('ConfigData(RegReadSet): Cannot read subkey %s', [Name]);

   with GetTypeData(TypeInfo)^ do
    for idx:= MinValue to MaxValue do
     if ReadBoolString(fReg.ReadString(GetENumName(TypeInfo, idx))) then
      include(TIntegerSet(value), idx);
  finally
   result:= value;
   fReg.OpenKey(OldKey, FALSE);
  end;
end;

procedure TCFGREg.ReadStrings(const Name: string; value: TStrings);
var
 OldKey: string;
begin
  value.BeginUpdate;
  OldKey:= '\' +fReg.CurrentPath;
  try
   value.Clear;
   if not fReg.OpenKey(Name, FALSE) then
    raise ERegistryException.Createfmt('ConfigData(RegReadStrings): Cannot open subkey %s', [Name]);

   fReg.GetValueNames(value);
  finally
   value.EndUpdate;
   fReg.OpenKey(OldKey, FALSE);
  end;
end;

procedure TCFGReg.ReadObject(const Name: string; Obj: TPersistent);
var
 OldKey: string;
begin
  OldKey:= '\'  +fReg.CurrentPath;
  try
   if fReg.OpenKey(Name, FALSE) then
    ReadFromRegistry(Obj);
  finally
   fReg.OpenKey(Oldkey, FALSE);
  end;
end;


// Writing Methods

procedure TCFGReg.WritetoRegistry(Obj: TPersistent);
var
 idx,
 idx2,
 Count: integer;
 PropName: string;
 CD: TConfigData;
begin
  Count:= GetPropCount(Obj);
  CD:= TConfigData(fOwner);

  for idx:= 0 to pred(Count) do
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

     if (CD.IgnoreProperties.Indexof(PropName)> -1) then continue;
     case  PropType(Obj, PropName) of
      tkString,
      tkLString,
      tkWString: fReg.WriteString(PropName, GetStrProp(Obj, PropName));

      tkChar,
      tkEnumeration,
      tkInteger: fReg.WriteInteger(PropName, GetOrdProp(Obj, PropName));

      tkInt64: fReg.WriteString(PropName, InttoStr(GetInt64Prop(Obj, PropName)));

      tkFloat: fReg.WriteString(PropName, FloattoStr(GetFloatProp(Obj, PropName)));

      tkSet: WriteSet(PropName, GetOrdProp(Obj, PropName),
        GetPropInfo(Obj, PropName, [tkSet])^.PropType^);

      tkClass:
       begin
         if TPersistent(GetOrdProp(Obj, PropName)) is TStrings then
          WriteStrings(PropName, TStrings(GetOrdProp(Obj, PropName)))
         else
          WriteObject(PropName, TPersistent(GetOrdProp(Obj, PropName)));
       end;
     end;
   end;
end;

procedure TCFGReg.WriteSet(const Name: string; value: integer;
  TypeInfo: PTypeInfo);
var
 OldKey: string;
 idx: integer;
begin
  TypeInfo:= GetTypeData(TypeInfo)^.CompType^;
  OldKey:= '\' +fReg.CurrentPath;
  try
   if not fReg.OpenKey(Name, TRUE) then
    raise ERegistryException.Createfmt('ConfigData(RegSaveSet): Cannot create subkey %s', [name]);

   with GetTypeData(TypeInfo)^ do
    for idx:= MinValue to MaxValue do
     fReg.WriteString(GetENumName(TypeInfo, idx), boolStr[idx in TIntegerSet(value)]);
  finally
   fReg.OpenKey(OldKey, FALSE);
  end;
end;

procedure TCFGReg.WriteStrings(const Name: string; value: TStrings);
var
 OldKey: string;
 idx: integer;
begin
  if value.Count <= 0 then exit;
  OldKey:= '\' +fReg.CurrentPath;
  try
   if not fReg.OpenKey(Name, TRUE) then
    raise ERegistryException.Createfmt('ConfigData(RegSaveStrings): Cannot create %s', [name]);

   for idx:= 0 to pred(value.Count) do
    fReg.WriteString(value[idx], '');
  finally
   fReg.OpenKey(OldKey, FALSE);
  end;
end;

procedure TCFGReg.WriteObject(const Name: string; Obj: TPersistent);

 function WritetheObject(Obj: TPersistent): boolean;
  var
   idx, c, Count: integer;
  begin
    result:= FALSE;
    if not assigned(Obj) then exit;
    Count:= GetPropCount(Obj);
    if Count <= 0 then exit;
    c:= Count;
    for idx:= 0 to pred(Count) do
     if TConfigData(fOwner).IgnoreProperties.Indexof(GetPropName(Obj, idx))> -1 then
      dec(c);

    result:= c> 0;
  end;

var
 OldKey: string;
begin
  if not WritetheObject(Obj) then exit;
  OldKey:= '\' +fReg.CurrentPath;
  try
   if not fReg.OpenKey(Name, TRUE) then
    raise  ERegistryException.Createfmt('(ConfigData(RegSaveObj): Cannot create %s', [name]);

   WritetoRegistry(Obj);
  finally
   fReg.OpenKey(OldKey, FALSE);
  end;
end;


// Public access methods

function TCFGReg.LoadSetting(const key, Entry: string): string;
var
 OldKey: string;
begin
  OldKey:= '\' +fReg.CurrentPath;
  try
   if fReg.OpenKey(Key, FALSE) then
    result:= fReg.ReadString(Entry)
   else
    result:= '';
  finally
   fReg.OpenKey(Oldkey, FALSE);
  end;
end;

function TCFGReg.LoadSetting(val : boolean; const key, Entry: string): string;
var
 OldKey: string;
begin
  OldKey:= '\' +fReg.CurrentPath;
  try
   if fReg.OpenKey(Key, FALSE) then
    result:= fReg.ReadString(Entry)
   else begin
     if val then
       result:= '1'
     else
       result:= '';
   end
  finally
   fReg.OpenKey(Oldkey, FALSE);
  end;
end;

procedure TCFGReg.LoadObject(var Obj: TCFGOptions);
var
 OldKey: string;
begin
  Oldkey:= '\' +fReg.CurrentPath;
  try
   ReadObject(Obj.Name, Obj);
  finally
   fReg.OpenKey(Oldkey, FALSE);
  end;
end;

procedure TCFGReg.SaveObject(var Obj: TCFGOptions);
var
 OldKey: string;
begin
  if not assigned(Obj) then exit;
  Oldkey:= '\' +fReg.CurrentPath;
  try
   WriteObject(Obj.Name, Obj);
  finally
   fReg.OpenKey(Oldkey, FALSE);
  end;
end;

procedure TCFGReg.SaveSetting(const key, entry, value: string);
var
 Oldkey: string;
begin
  OldKey:= '\' +fReg.CurrentPath;
  try
   if fReg.OpenKey(Key, TRUE) then
    fReg.WriteString(Entry, Value);
  finally
   fReg.OpenKey(OldKey, FALSE);
  end;
end;

end.
