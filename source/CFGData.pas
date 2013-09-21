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
  Windows, Classes, sysUtils, TypInfo, CFGINI, version;
{$ENDIF}
{$IFDEF LINUX}
  Classes, sysUtils, TypInfo, cfgreg, CFGINI;
{$ENDIF}

type
 EConfigDataError = class(Exception);

  TConfigData = class(TComponent)
  private
   finiFileName: AnsiString;
    finiSection: AnsiString; // current section of ini file
    fIgnores: TStrings;      // Ignored properties of objects
    GetINI: TCFGINI;

   procedure SetIni(const s : AnsiString);
  public
   constructor Create(aOwner: TComponent); override;
   destructor Destroy; override;
   procedure SettoDefaults; virtual; abstract;

   procedure SaveConfigData; virtual;
   procedure ReadConfigData; virtual;

   // load/save functions
   procedure SaveSettingB(const key: AnsiString; const entry: AnsiString; const value: boolean);
   procedure SaveSettingS(const key: AnsiString; const entry: AnsiString; const value: AnsiString);

   function LoadSettingB(const key: AnsiString; const entry: AnsiString): boolean; overload;
   function LoadSettingB(const key: AnsiString; const entry: AnsiString; const default: AnsiString): boolean; overload;
   function LoadSettingS(const key: AnsiString; const entry: AnsiString): AnsiString;

   // load/save TPersistent object
   procedure SaveObject(obj: TPersistent;const CustomName : AnsiString);
   procedure LoadObject(obj: TPersistent;const CustomName : AnsiString);

   property INIFile: AnsiString read fINIFileName write SetIni;
   property INISection: AnsiString read finiSection write finiSection;
   property IgnoreProperties: TStrings read fIgnores write fIgnores;
 end;

function GetPropName(Instance: TPersistent; Index: Integer): AnsiString;
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
function GetPropName(Instance: TPersistent; Index: Integer): AnsiString;
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
	INISection := 'Options';
	fIgnores:= TStringList.Create;
	with fIgnores do begin
		Add('Name');
		Add('Tag');
	end;
end;

destructor TConfigData.Destroy;
begin
	GetINI.Free;
	fIgnores.Free;
	inherited Destroy;
end;

procedure TConfigData.SetIni(const s : AnsiString);
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

procedure TConfigData.LoadObject(obj: TPersistent;const CustomName : AnsiString);
begin
	GetINI.LoadObject(Obj,CustomName);
end;

procedure TConfigData.SaveObject(obj: TPersistent;const CustomName : AnsiString);
begin
	GetINI.SaveObject(Obj,CustomName);
end;

function TConfigData.LoadSettingB(const key, entry: AnsiString): boolean;
var
	s: AnsiString;
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

function TConfigData.LoadSettingB(const key, entry, default: AnsiString): boolean;
var
	s: AnsiString;
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

function TConfigData.LoadSettingS(const key: AnsiString; const entry: AnsiString): AnsiString;
begin
	result:= GetINI.LoadSetting(key, Entry);
end;

procedure TConfigData.SaveSettingS(const key: AnsiString; const entry: AnsiString; const value: AnsiString);
begin
	GetINI.SaveSettingS(key, Entry, Value);
end;

procedure TConfigData.SaveSettingB(const key: AnsiString; const entry: AnsiString; const value: boolean);
begin
	GetINI.SaveSettingS(key, Entry, inttostr(ord(value)));
end;

end.
