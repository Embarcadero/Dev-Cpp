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
{$IFDEF WIN32}
  Windows, Messages, Classes, sysUtils, TypInfo, cfgreg, CFGINI, cfgtypes;
{$ENDIF}
{$IFDEF LINUX}
  Classes, sysUtils, TypInfo, cfgreg, CFGINI, cfgtypes;
{$ENDIF}

type
 EConfigDataError = class(Exception);

 TConfigData = class(TComponent)
  private
   finiFileName: string;               // ini filename
   finiSection: string;                // default section of ini file
   fRegKey: string;                    // root key for registry
   fRegRoot: HKEY;                     // registry main key

   // options
   fUseRegistry: boolean;              // Save settings to registry
   fboolAsWords: boolean;              // Save boolean values as TRUE/FALSE
   fIgnores: TStrings;                 // Ignored Properties
   GetReg: TCFGReg;
   GetINI: TCFGINI;

   procedure SetIni(s : string);
  public
   constructor Create(aOwner: TComponent); override;
   destructor Destroy; override;
   procedure SettoDefaults; virtual; abstract;

   procedure SaveConfigData; virtual;
   procedure ReadConfigData; virtual;

   // load/save windowplacement structure
   procedure SaveWindowPlacement(const key: string; value: TWindowPlacement);

   function LoadWindowPlacement(const key: string;
      var Value: TWindowPlacement): boolean;

   // load/save individual setting
   procedure SaveSetting(const key: string;
       const Entry: string; const value: string);

   function LoadSetting(const key: string;
       const entry: string): string;

   // load/save boolean setting
   procedure SaveboolSetting(const key: string;
       const entry: string; const value: boolean);

   function LoadboolSetting(const key: string;
       const entry: string): boolean; overload;
   function LoadboolSetting(val : boolean; const key: string;
       const entry: string): boolean; overload;
   // load/save TCFGOptions
   procedure SaveObject(obj: TCFGOptions); virtual;
   procedure LoadObject(obj: TCFGOptions); virtual;

   property BaseRegKey: string read fRegKey write fRegKey;
   property BoolAsWords: boolean read fboolAsWords write fboolAsWords;
   property INIFile: string read fINIFileName write SetIni;
   property INISection: string read finiSection write finiSection;
   property Root: HKEY read fRegRoot write fRegRoot;
   property UseRegistry: boolean read fUseRegistry write fUseRegistry;
   property IgnoreProperties: TStrings read fIgnores write fIgnores;
 end;

function ReadBoolString(Value: string): boolean;
function GetPropName(Instance: TPersistent; Index: Integer): String;
function GetPropCount(Instance: TPersistent): Integer;

const
 boolStr: array[boolean] of string[5] = ('False', 'True');

implementation

// returns boolean value of passed string
function ReadBoolString(Value: string): boolean;
begin
  result:= uppercase(Value) = 'TRUE';
end;

// returns empty TWindowPlacement Record
function EmptyWinPlace: TWindowPlacement;
 begin
   FillChar(result, Sizeof(TWindowPlacement), #0);
   Result.Length:= Sizeof(TWindowPlacement);
 end;

//Returns the number of properties of a given object
function GetPropCount(Instance: TPersistent): Integer;
var Data: PTypeData;
Begin
  Data:=GetTypeData(Instance.Classinfo);
  Result:=Data^.PropCount;
End;

//Returns the property name of an instance at a certain index
function GetPropName(Instance: TPersistent; Index: Integer): String;
var PropList: PPropList;
PropInfo:PPropInfo;
Data: PTypeData;
Begin
  Result:='';
  Data:=GetTypeData(Instance.Classinfo);
  GetMem(PropList,Data^.PropCount*Sizeof(PPropInfo));
  Try
    GetPropInfos(Instance.ClassInfo,PropList);
    PropInfo:=PropList^[Index];
    Result:=PropInfo^.Name;
  finally
  FreeMem(PropList,Data^.PropCount*Sizeof(PPropInfo));
  End;
End;


{ TConfigData }

constructor TConfigData.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  GetINI := TCFGINI.Create(Self);
  GetREG := TCFGREG.Create(Self);
  fUseRegistry:= TRUE;                 // defaultly use the registry to save info
  fRegRoot:= HKEY_CURRENT_USER;        // default to current user registry
  fIgnores:= TStringList.Create;
  with fIgnores do
   begin
     Add('Name');
     Add('Tag');
     // local props (might not need)
   end;
end;

destructor TConfigData.Destroy;
begin
  GetINI.Free;
  GetREG.Free;
  fIgnores.Free;
  inherited Destroy;
end;

{function TConfigData.GetINI: TCFGINI;
begin
  try
   result:= TCFGINI.Create(Self);
  except
   result:= nil;
  end;
end;

function TConfigData.GetReg: TCFGReg;
begin
  try
   result:= TCFGReg.Create(Self);
  except
   result:= nil;
  end;
end;  }

procedure TConfigData.SetIni(s : string);
begin
  fIniFileName := s;                                            
  GetINI.SetIniFile(s);
end;

procedure TConfigData.ReadConfigData;
begin
  if fUseRegistry then
   try
    GetReg.ReadConfig;
   except end
  else
   try
    GetINI.ReadConfig;
   except end;
end;

procedure TConfigData.SaveConfigData;
begin
  if fUseRegistry then
   try
    GetReg.SaveConfig;
   except end
  else
   try
    GetINI.SaveConfig;
   except end;
end;

function TConfigData.LoadWindowPlacement(const key: string;
  var Value: TWindowPlacement): boolean;

  // convert string to TWindowPlacement record
  function StrtoWinPlace(s: string): TWindowPlacement;
   var
    tmp: TSTringList;
   begin
     if s = '' then result:= EmptyWinPlace
     else
      begin
        tmp:= TStringList.Create;
        try
         tmp.CommaText:= s;
         if tmp.Count = 10 then
          with Result do
           begin
             Length:= Sizeof(TWindowPlacement);
             Flags:= StrtoInt(tmp[0]);
             ShowCmd:= StrtoInt(tmp[1]);
             ptMinPosition:= point(StrtoInt(tmp[2]), StrtoInt(tmp[3]));
             ptMaxPosition:= point(StrtoInt(tmp[4]), StrtoInt(tmp[5]));
             rcNormalPosition:= rect(StrtoInt(tmp[6]), StrtoInt(tmp[7]),
                                     StrtoInt(tmp[8]), StrtoInt(tmp[9]));
           end
          else
           result:= emptyWinPlace;
        finally
         tmp.Free;
        end;
      end;
   end;

var
 s: string;
begin
  try
   if fUseRegistry then
    s:= GetReg.LoadSetting(key, 'WinPlace')
   else
    s:= GetINI.LoadSetting(key, 'WinPlace');
   value:= StrtoWinPlace(s);
   result:= TRUE;
  except
   result:= false;
  end;
end;

procedure TConfigData.SaveWindowPlacement(const key: string;
  value: TWindowPlacement);

  function WinPlacetoStr(WinPlace: TWindowPlacement): string;
   begin
     with WinPlace do
      Result:= format('%d, %d, %d, %d, %d, %d, %d, %d, %d, %d',
        [Flags, ShowCmd, ptMinPosition.X, ptMinPosition.Y,
         ptMaxPosition.X, ptMaxPosition.Y,
         rcNormalPosition.Left, rcNormalPosition.Top,
         rcNormalPosition.Right, rcNormalPosition.Bottom]);
   end;
var
 s: string;
begin
  try
   s:= WinPlacetoStr(Value);
   if fUseRegistry then
    GetReg.SaveSetting(key, 'WinPlace', s)
   else
    GetINI.SaveSetting(key, 'WinPlace', s);
  except end;
end;

procedure TConfigData.LoadObject(obj: TCFGOptions);
begin
  try
   if fUseRegistry then
    GetReg.LoadObject(Obj)
   else
    GetINI.LoadObject(Obj);
  except end;
end;

procedure TConfigData.SaveObject(obj: TCFGOptions);
begin
  try
   if fUseRegistry then
    GetReg.SaveObject(Obj)
   else
    GetINI.SaveObject(Obj);
  except end;
end;

function TConfigData.LoadboolSetting(const key, entry: string): boolean;
var
 s: string;
begin
  try
   if fUseRegistry then
    s:= GetReg.LoadSetting(Key, Entry)
   else
    s:= GetINI.LoadSetting(key, Entry);

   if s = '' then s:= '0';
   if fboolAsWords then
    result:= ReadboolString(s)
   else
    result:= strtoint(s) = 1;
  except
   result:= FALSE;
  end;
end;

function TConfigData.LoadboolSetting(val : boolean; const key, entry: string): boolean;
var
 s: string;
begin
  try
   if fUseRegistry then
    s:= GetReg.LoadSetting(val, Key, Entry)
   else
    s:= GetINI.LoadSetting(val, key, Entry);

   if s = '' then s:= '0';
   if fboolAsWords then
    result:= ReadboolString(s)
   else
    result:= strtoint(s) = 1;
  except
   result:= FALSE;
  end;
end;

function TConfigData.LoadSetting(const key, entry: string): string;
begin
  try
   if fUseRegistry then
    result:= GetReg.LoadSetting(key, Entry)
   else
    result:= GetINI.LoadSetting(key, Entry);
  except end;
end;

procedure TConfigData.SaveSetting(const key: string; const Entry: string;
  const value: string);
begin
  try
   if fUseRegistry then
    GetReg.SaveSetting(key, Entry, Value)
   else
    GetINI.SaveSetting(key, Entry, Value);
  except end;
end;

procedure TConfigData.SaveboolSetting(const key: string; const entry: string;
  const value: boolean);
var
 s: string;
begin
  try
   if fBoolAsWords then
    s:= boolStr[value]
   else
    s:= inttostr(ord(value));

   if fUseRegistry then
    GetReg.SaveSetting(key, Entry, s)
   else
    GetINI.SaveSetting(key, Entry, s);
  except end;
end;

end.
