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

unit oysUtils;

interface

uses
 Classes;

type
  ToysStringList = class(TStringList)
   private
    function GetValues(index: integer): AnsiString;
    procedure SetValues(index: integer; Value: AnsiString);
    function GetName(index: integer): AnsiString;
    procedure SetName(index: integer; Value: AnsiString);
    function GetValue(Name: AnsiString): AnsiString;
    procedure SetValue(Name: AnsiString; Value: AnsiString);
   public
    procedure Appendfmt(s: AnsiString; const args: array of const);
    function IndexofValue(Value: AnsiString): integer;
    function Addfmt(s: AnsiString; const args: array of const): integer;
    property Values[index: integer]: AnsiString read GetValues write SetValues;
    property Value[Name: AnsiString]: AnsiString read GetValue write SetValue;
    property Names[index: integer]: AnsiString read GetName write SetName;
  end;

function ReadBoolStr(Value: AnsiString): boolean;

const
 BoolStr: array[boolean] of AnsiString = ('False', 'True');

implementation

uses
 SysUtils;

function ReadBoolStr(Value: AnsiString): boolean;
begin
  result:= uppercase(Value) = 'TRUE';
end;

 { ToysStringList }

//Indexof function for Values in TStringList
function ToysStringList.IndexofValue(Value: AnsiString): integer;
var
 s: AnsiString;
 p: integer;
begin
  for Result:= 0 to pred(Count) do
   begin
     s:= Get(Result);
     p:= Pos('=', s);
     if (p <> 0) and
        (CompareStrings(Copy(s, p +1, length(s)), Value) = 0) then exit;
   end;
  result:= -1;
end;

procedure ToysStringList.SetValues(index: integer; Value: AnsiString);
var
 s: AnsiString;
 p: integer;
begin
  if (index>= 0) and (index < Count) then
   begin
     s:= Get(index);
     p:= Pos('=', s);
     if (p <> 0) then
      s:= copy(s, 1, p +1) +Value
     else
      s:= s +'='+value;
     Put(index, s);
   end
  else
   raise EListError.Createfmt('Module List SetValue index out of range: %d', [index]);
end;

function ToysStringList.GetValues(index: integer): AnsiString;
var
 s: AnsiString;
 p: integer;
begin
  if (index>= 0) and (index < Count) then
   begin
     s:= Get(index);
     p:= Pos('=', s);
     if (p <> 0) then
      result:= Copy(s, p+1, length(s))
     else
      result:= '';
   end
  else
   raise EListError.CreateFmt('Module List GetValue index out of range: %d', [index]);
end;

function ToysStringList.GetValue(Name: AnsiString): AnsiString;
var
  I: Integer;
begin
  I:= IndexOfName(Name);
  if I >= 0 then
    Result:= Copy(Get(I), Length(Name) + 2, MaxInt) else
    Result:= '';
end;

procedure ToysStringList.SetValue(Name, Value: AnsiString);
var
  I: Integer;
begin
  I:= IndexOfName(Name);
  if Value <> '' then
  begin
    if I < 0 then I:= Add('');
    Put(I, Name + '=' + Value);
  end else
  begin
    if I >= 0 then Delete(I);
  end;
end;


function ToysStringList.GetName(index: integer): AnsiString;
begin
  result:= ExtractName(Get(index));
end;

procedure ToysStringList.SetName(index: integer; Value: AnsiString);
var
 s: AnsiString;
 p: integer;
begin
  s:= get(index);
  p:= Pos('=', s);
  if p> 0 then
   s:= copy(s, p+1, length(s));
  s:= Value+'='+s;
  put(index, s);

end;

function ToysStringList.Addfmt(s: AnsiString;
  const args: array of const): integer;
var
 astr: AnsiString;
begin
  fmtstr(astr, s, args);
  result:= Add(astr);
end;

procedure ToysStringList.Appendfmt(s: AnsiString; const args: array of const);
begin
  addfmt(s, args);
end;

end.
