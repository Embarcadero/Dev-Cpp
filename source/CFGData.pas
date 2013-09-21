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
  Windows, Classes, sysUtils, TypInfo, CFGINI, cfgtypes, version;
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
   fIgnores: TStrings;                 // Ignored properties of objects
   GetINI: TCFGINI;

   procedure SetIni(s : string);
  public
   constructor Create(aOwner: TComponent); override;
   destructor Destroy; override;
   procedure SettoDefaults; virtual; abstract;

   procedure SaveConfigData; virtual;
   procedure ReadConfigData; virtual;

   // load/save functions
   procedure SaveSettingB(const key: string; const entry: string; const value: boolean);
   procedure SaveSettingS(const key: string; const Entry: string; const value: string);

   function LoadSettingB(const key,entry: string): boolean; overload;
   function LoadSettingB(const key,entry,default: string): boolean; overload;
   function LoadSettingS(const key: string; const entry: string): string;

   // load/save TCFGOptions
   procedure SaveObject(obj: TCFGOptions); virtual;
   procedure LoadObject(obj: TCFGOptions); virtual;

   property INIFile: string read fINIFileName write SetIni;
   property INISection: string read finiSection write finiSection;
   property IgnoreProperties: TStrings read fIgnores write fIgnores;
 end;

function GetPropName(Instance: TPersistent; Index: Integer): String;
function GetPropCount(Instance: TPersistent): Integer;

implementation

//Returns the number of properties of a given object
function GetPropCount(Instance: TPersistent): Integer;
var
	Data: PTypeData;
begin
	Data:=GetTypeData(Instance.Classinfo);
	Result:=Data^.PropCount;
end;

//Returns the property name of an instance at a certain index
function GetPropName(Instance: TPersistent; Index: Integer): String;
var
	PropList: PPropList;
	PropInfo:PPropInfo;
	Data: PTypeData;
begin
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
end;

{ TConfigData }

constructor TConfigData.Create(aOwner: TComponent);
begin
	inherited Create(aOwner);
	GetINI := TCFGINI.Create(Self);
	INISection := OPT_OPTIONS;
	fIgnores:= TStringList.Create;
	with fIgnores do begin
		Add('Name');
		Add('Tag');
		// local props (might not need)
	end;
end;

destructor TConfigData.Destroy;
begin
	GetINI.Free;
	fIgnores.Free;
	inherited Destroy;
end;

procedure TConfigData.SetIni(s : string);
begin
	fIniFileName := s;
	GetINI.SetIniFile(s);
end;

procedure TConfigData.ReadConfigData;
begin
	GetINI.ReadConfig;
end;

procedure TConfigData.SaveConfigData;
begin
	GetINI.SaveConfig;
end;

procedure TConfigData.LoadObject(obj: TCFGOptions);
begin
	GetINI.LoadObject(Obj);
end;

procedure TConfigData.SaveObject(obj: TCFGOptions);
begin
	GetINI.SaveObject(Obj);
end;

function TConfigData.LoadSettingB(const key, entry: string): boolean;
var
	s: string;
begin
	try
		s:= GetINI.LoadSetting(key, Entry);

		if s = '' then
			s:= '0';
		result:= strtoint(s) = 1;
	except
		result:= FALSE;
	end;
end;

function TConfigData.LoadSettingB(const key, entry, default: string): boolean;
var
	s: string;
begin
	try
		s:= GetINI.LoadSetting(key, Entry);

		if s = '' then
			s:= default;
		result:= strtoint(s) = 1;
	except
		result:= FALSE;
	end;
end;

function TConfigData.LoadSettingS(const key, entry: string): string;
begin
	result:= GetINI.LoadSetting(key, Entry);
end;

procedure TConfigData.SaveSettingS(const key: string; const Entry: string;const value: string);
begin
	GetINI.SaveSettingS(key, Entry, Value);
end;

procedure TConfigData.SaveSettingB(const key: string; const entry: string; const value: boolean);
begin
	GetINI.SaveSettingS(key, Entry, inttostr(ord(value)));
end;

end.
