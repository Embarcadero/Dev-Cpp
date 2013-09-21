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

// This is part of the utils library that I needed in some files  of this VCl package

unit stringutils;

interface

uses
{$IFDEF WIN32}
 Sysutils;
{$ENDIF}
{$IFDEF LINUX}
 Sysutils;
{$ENDIF}

// Fast replacements of Ansi functions
function EndsStr(const subtext, text: AnsiString): boolean;
function EndsText(const subtext, text: AnsiString): boolean;

function ContainsStr(const text, subtext: AnsiString): boolean;
function ContainsText(const text, subtext: AnsiString): boolean;

function SameStr(const s1,s2 : AnsiString) : boolean;
function SameText(const s1,s2 : AnsiString) : boolean;

function NotSameStr(const s1,s2 : AnsiString) : boolean;
function NotSameText(const s1,s2 : AnsiString) : boolean;

function StartsStr(const subtext,text : AnsiString) : boolean;
function StartsText(const subtext,text : AnsiString) : boolean;

function ReplaceFirstStr(const S, OldPattern, NewPattern : string) : string;
function ReplaceFirstText(const S, OldPattern, NewPattern : string) : string;

implementation

function EndsStr(const subtext, text: AnsiString): boolean;
var
	SubTextLocation: Integer;
begin
	SubTextLocation := Length(text) - Length(subtext) + 1;
	if (SubTextLocation > 0) and (subtext <> '') then
		Result := StrComp(Pointer(subtext), Pointer(@text[SubTextLocation])) = 0
	else
		Result := False;
end;

function EndsText(const subtext, text: AnsiString): boolean;
var
	SubTextLocation: Integer;
begin
	SubTextLocation := Length(text) - Length(subtext) + 1;
	if (SubTextLocation > 0) and (subtext <> '') then
		Result := StrIComp(Pointer(subtext), Pointer(@text[SubTextLocation])) = 0
	else
		Result := False;
end;

function ContainsStr(const text, subtext: AnsiString): boolean;
begin
	Result := Pos(subtext, text) > 0;
end;

function ContainsText(const text, subtext: AnsiString): boolean;
begin
	Result := Pos(UpperCase(subtext), UpperCase(text)) > 0;
end;

function SameStr(const s1,s2 : AnsiString) : boolean;
begin
	Result := (CompareStr(s1,s2) = 0);
end;

function SameText(const s1,s2 : AnsiString) : boolean;
begin
	Result := (CompareText(s1,s2) = 0);
end;

function NotSameStr(const s1,s2 : AnsiString) : boolean;
begin
	Result := (CompareStr(s1,s2) <> 0);
end;

function NotSameText(const s1,s2 : AnsiString) : boolean;
begin
	Result := (CompareText(s1,s2) <> 0);
end;

function StartsStr(const subtext,text : AnsiString) : boolean;
begin
	Result := SameStr(subtext, Copy(text, 1, Length(subtext)));
end;

function StartsText(const subtext,text : AnsiString) : boolean;
begin
	Result := SameText(subtext, Copy(text, 1, Length(subtext)));
end;

function ReplaceFirstStr(const S, OldPattern, NewPattern : string) : string;
var
	Offset: Integer;
begin

	Offset := Pos(OldPattern, S);
	if Offset = 0 then begin
		Result := S;
	end else begin

		// Copy the preceding stuff, append the new part, append old stuff after old pattern
		Result := Copy(S, 1, Offset - 1) + NewPattern + Copy(S, Offset + Length(OldPattern), MaxInt);
	end;
end;

function ReplaceFirstText(const S, OldPattern, NewPattern : string) : string;
var
	Offset: Integer;
	UpperS,UpperOldPattern : string;
begin
	UpperS := UpperCase(S);
	UpperOldPattern := UpperCase(OldPattern);

	Offset := Pos(UpperOldPattern, UpperS);
	if Offset = 0 then begin
		Result := S;
	end else begin

		// Copy the preceding stuff, append the new part, append old stuff after old pattern
		Result := Copy(S, 1, Offset - 1) + NewPattern + Copy(S, Offset + Length(UpperOldPattern), MaxInt);
	end;
end;

end.